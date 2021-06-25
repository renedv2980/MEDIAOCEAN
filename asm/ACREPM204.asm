*          DATA SET ACREPM204  AT LEVEL 008 AS OF 01/28/13                      
*PHASE ACM204A                                                                  
         TITLE 'REPORT CONTROL'                                                 
ACM204   CSECT                                                                  
         DC    AL4(ENTRYTAB-ACM204)                                             
         PRINT NOGEN                                                            
*                                                                               
         USING MAND,RA                                                          
         USING ACWORKD,RC                                                       
         USING BIGPRNTD,R6                                                      
REPS     NMOD1 0,**REPS**,R9,R8                                                 
         L     RC,BASERC                                                        
         L     R6,VBIGPRNT                                                      
         MVI   SPCNT,0             SET SPACING COUNT                            
         MVI   SUPSW,0             SUPPRESS OPTIONS                             
         MVI   SPCSW,0             SPACING/SKIP OPTIONS                         
         MVI   FORCEHED,YES                                                     
         MVC   PAGE,=H'1'                                                       
         OI    SRTSW,SRTFR         FIRST TIME SWITCH                            
         XC    SORTDUM,SORTDUM                                                  
         XC    LASTREP,LASTREP                                                  
         XC    LASTDUM,LASTDUM                                                  
         XC    LASTLEVS,LASTLEVS                                                
         XC    DWNRPT,DWNRPT       CLEAR DOWNLOAD REPORT NUMBER                 
         BAS   RE,CLRXP            PRINT LINES                                  
         BAS   RE,CLRAM            CLEAR AMOUNTS                                
*                                                                               
REPS3    BAS   RE,GETSORT          GET A RECORD FROM SORT                       
         TM    UPSI,TRCGETS        DUMP RECORDS FROM SORT/MERGE                 
         BNO   *+8                                                              
         BAS   RE,DMPGET                                                        
         TM    SRTSW,SRTLP         TEST LAST RECORD                             
         BO    REPS11              PROCESS LAST                                 
*                                                                               
         CLI   SORTTYPE,0          NON-ZERO RECORDS ARE TOTAL                   
         BE    REPS7               RECORDS FOR VERTICAL PERCENT                 
         SR    R1,R1                                                            
         IC    R1,SORTTYPE                                                      
         LA    R1,10(,R1)          ACCUMULATOR LINES 11-19 (TEMP)               
         MH    R1,WLINE            ACCUMULATOR LINES 01-09 (PERM)               
         A     R1,AACCUM           ARE RESERVED FOR THESE                       
         MVC   0(256,R1),SORTCOLS                                               
         B     REPS3                                                            
*&&US                                                                           
REPS7    TM    HPSW,HPDID          IS IT A DIVIDEND RECORD                      
         BNO   REPS9                                                            
         LA    R1,LNDID            R1 TO DIVIDEND ACCUM LINE                    
         MH    R1,WLINE                                                         
         A     R1,AACCUM                                                        
         MVC   0(256,R1),SORTCOLS  SAVE DIVIDEND ACCUMS                         
         B     REPS3                                                            
*                                                                               
REPS9    TM    HPSW,HPDIV          IS IT A DIVISOR RECORD                       
         BNO   REPS11                                                           
         BAS   RE,CALPCT           PERCENT IN THE SORTREC                       
*&&                                                                             
*&&UK                                                                           
REPS7    DS    0H                                                               
*&&                                                                             
REPS11   MVI   CBLEVEL,0                                                        
         CLI   LASTREP,0           FIRST TIME?                                  
         BNE   REPS15                                                           
*                                                                               
REPS13   MVC   REPCODE,SORTREP                                                  
         SR    R2,R2                                                            
         IC    R2,REPCODE          POSITION R2 TO APPROPRIATE                   
         BCTR  R2,0                STACK ITEM KEYED BY REPCODE                  
         MH    R2,WRSTACK+2                                                     
         A     R2,ARSTACK                                                       
         MVC   COPYNO,SORTCOPY                                                  
         CLC   LASTREP,SORTREP     IF FIRST FOR REPORT                          
         BE    *+8                                                              
         BAS   RE,NAMEINIT         INITIALIZE NAME STACK                        
         BAS   RE,DIGCODE          REFRESH CODES AND NAMES                      
         BAS   RE,MADD             THEN ADD INTO ACCUMULATORS                   
         BAS   RE,MFST             AND DEAL WITH 'FIRST' CONDITIONS             
         MVI   CBLEVEL,0           SET LAST BREAK                               
         MVC   LASTREP,SORTREP     SAVE THIS RECORD'S KEY                       
         TM    SRTSW,SRTLP         TEST LAST RECORD                             
         BO    REPS19              GET ANOTHER                                  
         MVC   LASTLEVS,SORTLEVS                                                
         B     REPS3                                                            
*                                                                               
REPS15   TM    SRTSW,SRTLP         TEST LAST RECORD                             
         BO    REPS19              PROCESS LAST                                 
         LA    R3,SORTDUM          SET CONTROL BREAK                            
         LA    R4,LASTDUM                                                       
         SR    R1,R1                                                            
         LA    R0,10                                                            
*                                                                               
REPS17   STC   R1,CBLEVEL          SET CBLEVEL TO LEVEL WHERE                   
         CLC   0(16,R3),0(R4)      CONTROL BREAK OCCURS                         
         BNE   REPS19                                                           
         LA    R3,16(,R3)                                                       
         LA    R4,16(,R4)                                                       
         LA    R1,1(,R1)                                                        
         BCT   R0,REPS17                                                        
*                                                                               
REPS19   MVC   REPCODE,LASTREP                                                  
         SR    R2,R2                                                            
         IC    R2,REPCODE          POSITION R2 TO APPROPRIATE                   
         BCTR  R2,0                STACK ITEM KEYED BY REPCODE                  
         MH    R2,WRSTACK+2                                                     
         A     R2,ARSTACK                                                       
         BAS   RE,MLST             AND DEAL WITH 'LAST' CONDITIONS              
         TM    SRTSW,SRTLP         TEST LAST RECORD                             
         BNO   REPS13                                                           
*                                                                               
         NI    SRTSW,ALL-SRTLP     TURNOFF LAST PROCESSED                       
         TM    DWNSTAT,DWNLOAD     DOWNLOADING                                  
         BNO   REPX                                                             
         GOTO1 ADWNL,DWNEOR        SET END OF REPORT                            
*                                                                               
REPX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GET RECORD FROM SORT ADD RECORDS WITH SAME KEYS                     *         
***********************************************************************         
         SPACE 1                                                                
GETSORT  NTR1  ,                                                                
GS00     TM    SRTSW,SRTEO         HAS EOF BEEN SET                             
         BNO   *+12                                                             
         OI    SRTSW,SRTLP         TELL REPORT IT'S ALL OVER                    
         B     REPX                                                             
*                                                                               
         TM    SRTSW,SRTFR         FIRST TIME                                   
         BO    GS01                                                             
         LA    R4,NXTMRGE                                                       
         LA    R5,THSMRGE                                                       
         BAS   RE,GS99             MOVE SAVED(NXTMRGE) TO THSMRGE               
*                                                                               
GS01     GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R4,15,DMCB+4                                                     
         BNZ   GS02                                                             
         OI    SRTSW,SRTEO         SET EOF                                      
         B     GS09                                                             
*                                                                               
GS02     LA    R5,NXTMRGE          SAVE IN NXTMRGE                              
         BAS   RE,GS99                                                          
         TM    SRTSW,SRTFR         FIRST TIME ?                                 
         BNO   GS03                                                             
         NI    SRTSW,ALL-SRTFR     TURN OFF FIRST TIME AND                      
         B     GS00                GET ANOTHER                                  
*                                                                               
GS03     SR    R1,R1                                                            
         LH    R1,SRTLNGTH         DOES IT HAVE SAME KEY                        
         AHI   R1,3                PLUS REPORT/COPY/VERT                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NXTMRGE(0),THSMRGE                                               
         BNE   GS09                KEY CHANGE                                   
         LA    R4,NXTMRGE                                                       
         AH    R4,DSPNME                                                        
         LA    R3,THSMRGE                                                       
         AH    R3,DSPNME                                                        
         L     R0,HIGHROW          NUMBER OF ROW NAMES                          
*                                                                               
GS05     OC    0(36,R3),0(R3)      MOVE IN SIGNIFICANT NAMES                    
         BNZ   *+10                                                             
         MVC   0(36,R3),0(R4)                                                   
         LA    R3,36(R3)                                                        
         LA    R4,36(R4)                                                        
         BCT   R0,GS05                                                          
*                                                                               
         LA    R4,NXTMRGE          R4=NEXT RECORD                               
         AH    R4,DSPCOL           PLUS DISPLACMENT TO COLUMNS                  
         LA    R3,THSMRGE          R3=CURRENT RECORD                            
         AH    R3,DSPCOL           PLUS DISPL.                                  
         L     R0,HIGHCOL          NUMBER OF COLS IN THIS RECORD                
*                                                                               
GS07     TM    7(R4),X'08'         TEST NON-ADDITIVE COLUMN                     
         BO    GS08                DON'T ADD IT                                 
         OI    7(R4),X'0C'         ENSURE PROPER SIGNS                          
         OI    7(R3),X'0C'                                                      
         CP    0(8,R3),=P'0'       OK TO ADD IF EITHER IS ZERO                  
         BNE   GS08A                                                            
*                                                                               
GS08     OI    7(R3),X'0C'                                                      
         AP    0(8,R3),0(8,R4)     ADD UP THE COLS                              
GS08A    LA    R3,8(,R3)                                                        
         LA    R4,8(,R4)                                                        
         BCT   R0,GS07                                                          
         B     GS01                AND GO AND GET ANOTHER RECORD                
*                                                                               
GS09     LA    R3,THSMRGE          R3=CURRENT RECORD                            
         AH    R3,DSPCOL           PLUS DISPL.                                  
         L     R0,HIGHCOL          NUMBER OF COLS IN THIS RECORD                
         OI    7(R3),X'08'         ENSURE PROPER SIGN                           
         LA    R3,8(,R3)                                                        
         BCT   R0,*-8                                                           
*                                                                               
         USING RSTACKD,R2                                                       
         LA    R4,THSMRGE                                                       
         AH    R4,DSPREP           DISPLACEMENT TO REPORT CODE                  
         SR    RF,RF               GET REPORT STACK                             
         IC    RF,0(,R4)           REPORT NUMBER                                
         BCTR  RF,0                                                             
         MH    RF,WRSTACK+2                                                     
         L     R2,ARSTACK                                                       
         AR    R2,RF               R2 POINTS TO THE REPORT STACK ITEM           
         LA    R4,THSMRGE                                                       
         ST    R4,HOOKAREC                                                      
         LA    RE,RSHKOUT                                                       
         CLI   0(RE),CMSROT        IS IT SORTOUT HOOK (29)                      
         BNE   GS20                NO                                           
         MVI   HOOKTYPE,CMSROT     SORTOUT (29)                                 
         MVC   HOOKNUM,2(RE)       PASS THE HOOK NUMBER FOR THIS REPORT         
         OI    HOOKSTAT,HOOKSPRC   SET HOOK IN PROCESS                          
         GOTO1 AHOOKCDE,DMCB,(RA)                                               
         BZ    GS20                PROCES                                       
         NI    HOOKSTAT,ALL-HOOKSPRC                                            
         B     GS00                DON'T WANT - GET ANOTHER                     
*                                                                               
GS20     NI    HOOKSTAT,ALL-HOOKSPRC                                            
         BAS   RE,MTS              CONVERT RECORD TO SORT FORMAT                
*&&US                                                                           
         L     R1,RSNROWS          GET LAST BYTE OF LAST ROW                    
         SLL   R1,4                                                             
         LA    R1,SORTLEVS(R1)                                                  
         BCTR  R1,0                                                             
         MVC   HPSW,0(R1)          OPERATION CODE                               
*&&                                                                             
         CLI   RSKEYCOL,0          ANY KEY COLUMNS TO CHECK                     
         BE    GS23                NO, CONTINUE                                 
         CLI   RSKEYCOL,99         ALL COLUMNS                                  
         BE    GS22                                                             
         LA    R5,RSKEYCOL         THE COLS TO KEY                              
         LA    R0,10               MAX OF 10 COLS                               
*                                                                               
GS21     CLI   0(R5),0             LAST ONE PROC W/O STUFF                      
         BE    GS00                DON'T PUT IT - GET ANOTHER                   
         SR    R1,R1                                                            
         IC    R1,0(,R5)           COLUMN NUMBER                                
         BCTR  R1,0                                                             
         SLL   R1,3                X 8                                          
         LA    RF,SORTCOLS                                                      
         AR    R1,RF                                                            
         OC    0(8,R1),0(R1)                                                    
         BZ    *+14                                                             
         CP    0(8,R1),=P'0'                                                    
         BNE   GS23                STUFF FOUND IN ONE COL, KEEP IT              
         LA    R5,1(,R5)                                                        
         BCT   R0,GS21                                                          
         B     GS00                                                             
*                                                                               
GS22     L     R0,RSNCOLS                                                       
         LA    R1,SORTCOLS                                                      
GS22A    OC    0(8,R1),0(R1)                                                    
         BZ    *+14                                                             
         CP    0(8,R1),=P'0'                                                    
         BNE   GS23                STUFF FOUND IN ONE COL, KEEP IT              
         LA    R1,8(,R1)                                                        
         BCT   R0,GS22A                                                         
         B     GS00                                                             
*                                                                               
GS23     TM    DWNSTAT,DWNLOAD     TEST DOWNLOADING                             
         BNO   REPX                OK TO PROCESS RECORD                         
         CLI   DWNRPT,0            DID WE ALREADY SAVE IT OFF?                  
         BNE   *+10                                                             
         MVC   DWNRPT,SORTREP      ONLY FIRST CAN BE DOWNLOADED                 
         CLC   DWNRPT,SORTREP      MATCH REPORT NUMBER                          
         BE    REPX                OK TO PROCESS RECORD                         
         CLI   RSDWNRPT,0                                                       
         BE    GS00                GET NEXT RECORD, DO NOT DOWN-LOAD            
         CLC   DWNRPT,RSDWNRPT     FORCE TO DOWNLOAD THIS REPORT TOO            
         BE    REPX                  IF MATCHES                                 
         B     GS00                GET NEXT IF NOT THE SAME                     
*                                                                               
GS99     LR    R0,RE               MOVE RECORD (R4) TO (R5)                     
         LR    R2,R5                                                            
         LH    R3,RECLNGTH                                                      
         LR    RE,R4                                                            
         LR    RF,R3                                                            
         MVCL  R2,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* MERGE TO SORT FORMAT                                              *           
* R2 = REPORT STACK ENTRY                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
MTS      NTR1  ,                   BUILD RECORD IN SORTREP AREA                 
         LA    RE,SORTREP          CLEAR RECORD                                 
         LHI   RF,SORTLNQ                                                       
         XCEFL                                                                  
         LA    R1,SORTCOLS         AND INIT ACCUMS                              
         LA    R0,L'SORTCOLS/8                                                  
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(,R1)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         LA    R4,THSMRGE                                                       
         LR    R5,R4               R4 = A(RECORD FROM SORTER)                   
         AH    R5,DSPREP           DISPLACEMENT TO REPORT CODE                  
         MVC   SORTREP(4),0(R5)    REPORT/COPY/TYPE/SPARE                       
*                                                                               
         LR    R5,R4                                                            
         AH    R5,DSPCOL           DISPLACEMENT TO COLUMNS                      
         L     RF,RSNCOLS          NUMBER OF COLUMNS                            
         SLL   RF,3                TIMES 8 = TOTAL LENGTH OF ALL COLS           
         BCTR  RF,0                REDUCE IT BY 1 FOR EX                        
*MN      EX    RF,*+4                                                           
         MVC   SORTCOLS(0),0(R5)   RESTORE THE COLUMNS                          
         EX    RF,*-6                                                           
         LR    R5,R4                                                            
         AH    R5,DSPNME           R5 = NAMES, R4 =LEVELS                       
         LA    RE,SORTLEVS                                                      
         LA    RF,SORTNAMS                                                      
         LA    R0,1                CURRENT ROW                                  
         LA    R6,RSNAMSQ          ROWS TO BE SORTED BY NAME                    
         LA    R3,SRTFLN           SORT FIELD LENGTHS                           
         SR    R1,R1                                                            
*                                                                               
MTS05    MVC   0(2,RE),0(R4)       REPORT/COPY                                  
         CLM   R0,1,0(R6)          DOES THIS ROW USE NAME SEQ                   
         BNE   MTS09                                                            
         MVC   0(36,RF),2(R4)      NAME                                         
         MVC   2(14,RE),0(R5)      CODE (LEVEL)                                 
         LA    R6,1(,R6)                                                        
         B     MTS11                                                            
*                                                                               
MTS09    MVC   2(14,RE),2(R4)      CODE (LEVEL)                                 
         MVC   0(36,RF),0(R5)      NAME                                         
*                                                                               
MTS11    IC    R1,0(,R3)           LENGTH OF SORT FIELD                         
         LA    R4,0(R1,R4)         LEVELS                                       
         LA    RE,16(,RE)                                                       
         LA    R5,36(,R5)          NAMES                                        
         LA    RF,36(,RF)                                                       
         LA    R3,1(,R3)           SORT FIELD LENGTHS                           
         AHI   R0,1                ROW NUMBER                                   
         C     R0,HIGHROW                                                       
         BNH   MTS05                                                            
         B     REPX                                                             
         EJECT                                                                  
***********************************************************************         
* DUMP THE RECORD CURRENTLY IN SORTREP                                *         
***********************************************************************         
         SPACE 1                                                                
DMPGET   CP    TRCGCNT,=P'1000'    ALREADY PRINTED  THE MAX                     
         BHR   RE                                                               
*                                                                               
         USING BOXD,R4                                                          
         NTR1  ,                                                                
         L     R4,ADBXAREA                                                      
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   RCSUBPRG,2          TURN OFF BOXES                               
         LA    R0,SORTLNQ                                                       
         LA    RF,=C'GETSORT'                                                   
         LA    R5,SORTREP                                                       
         GOTO1 PRNTBL,DMCB,(7,(RF)),(R5),                              X        
               C'DUMP',(R0),=C'2D',(C'P',PRINT)                                 
         AP    TRCGCNT,=P'1'                                                    
         L     RF,ADBXAREA                                                      
         MVC   BOXWIDTH,=F'198'                                                 
         MVI   RCSUBPRG,0                                                       
         B     REPX                                                             
         TITLE 'REPORT - HANDLE ADDING INTO ACCUMULATORS'                       
***********************************************************************         
* ADD TO ACCUMULATORS                                                 *         
*  R2=RSTACK                                                          *         
***********************************************************************         
         SPACE 1                                                                
MADD     STM   RE,R6,SVRE                                                       
         SR    R3,R3               ADD INTO LINES FROM RECAP LEVEL              
         IC    R3,RSRECAP+1        R3=RECAP LEVEL                               
         L     R5,RSNROWS          R5=TOTAL NUMBER OF ROWS                      
         LA    R5,1(,R5)                                                        
         SR    R5,R3               R5=NUMBER OF ROWS TO ADD                     
         BNP   MADDX                                                            
         LA    R3,20(,R3)          LEVEL +20                                    
         MH    R3,WLINE                                                         
         A     R3,AACCUM           R3=ACCUMULATORS                              
*                                                                               
MADD3    LA    RE,SORTCOLS         RE=INPUT VALUES                              
         LR    RF,R3               RF=ACCUMULATORS                              
         L     R0,RSNCOLS          R0=NUMBER OF COLUMNS                         
         LA    R7,RSCOPTN          COLUMN OPTIONS                               
*                                                                               
MADD5    CHI   R5,1                LAST LEVEL (LOWEST)                          
         BE    MADD7               NOROLL HAS NO EFFECT ON IT                   
         TM    RSCOPTN,RSCNRALL    DON'T ROLL ALL COLUMNS                       
         BO    MADD9                                                            
         TM    0(R7),RSCNROLL      DON'T ROLL THIS COL                          
         BO    MADD9                                                            
*                                                                               
MADD7    AP    0(8,RF),0(8,RE)     ADD IN THE COLUMNS                           
MADD9    LA    RE,8(,RE)           RE=SORTCOLS                                  
         LA    RF,8(,RF)           RF=ACCUMS                                    
         LA    R7,1(,R7)           R7=REPORT RSCNROLL OPTION                    
         BCT   R0,MADD5                                                         
*                                                                               
         AH    R3,WLINE            NEXT ACCUMULATOR ROW                         
         BCT   R5,MADD3                                                         
*                                                                               
MADDX    LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         TITLE 'REPORT - MULTIPLE FIRST '                                       
***********************************************************************         
* MULTIPLE FIRST                                                      *         
*  R2=RSTACK                                                          *         
***********************************************************************         
         SPACE 1                                                                
MFST     ST    RE,RTNRE            SAVE RETURN REGISTER                         
         MVI   SPCSW,0             SPACING CONTROL SWITCHES                     
         CLI   RSSPACE,99          SPACING  99  N                               
         BNE   *+8                                                              
         OI    SPCSW,SPAC99                                                     
         CLI   RSSKIP,99           SPACING  N   SKIP                            
         BNE   *+8                                                              
         OI    SPCSW,SKIP99                                                     
         L     R0,RSNROWS          NUMBER OF ROWS                               
         SR    R1,R1                                                            
         IC    R1,CBLEVEL          CURRENT LEVEL                                
         CR    R1,R0                                                            
         BL    MFST3                                                            
         LR    R1,R0               GET LOWER                                    
*                                                                               
MFST3    STC   R1,REPTLEV          LEVEL TO PROCESS                             
         NI    SPCSW,ALL-SKIPCUR                                                
         CLC   RSSKIP,REPTLEV      IS THIS LEVEL TO WHICH SPACE 99              
         BNE   *+16                APPLIES                                      
         CLI   REPTLEV,0           BUT NEVER LEVEL 0                            
         BE    *+8                                                              
         OI    SPCSW,SKIPCUR                                                    
         NI    SPCSW,ALL-ROWLAST                                                
         CR    R1,R0               IS THIS LAST (DETAIL) ROW                    
         BNE   *+8                                                              
         OI    SPCSW,ROWLAST                                                    
         BAS   RE,SFST             GO AND HANDLE FIRST TIME ROUTINE             
         TM    SPCSW,ROWLAST       LAST LEVEL                                   
         BO    MFSTX                                                            
         LA    R1,1(,R1)           PROCESS EACH LEVEL                           
         B     MFST3                                                            
*                                                                               
MFSTX    L     RE,RTNRE                                                         
         BR    RE                                                               
         TITLE 'REPORT - FIRST TIME FOR LEVEL '                                 
***********************************************************************         
* SINGLE FIRST                                                        *         
*  R2=RSTACK                                                          *         
*  REPTLEV = CURRENT LEVEL                                            *         
***********************************************************************         
         SPACE 1                                                                
SFST     STM   RE,R6,SVRE                                                       
*                                  MOVE ACCUMULATOR LINES 11-19                 
         SR    R3,R3               TO LINES 1-9 FOR VERTICAL PCT.               
         IC    R3,REPTLEV          R3=LEVEL                                     
         LA    R4,10(,R3)          R4=LEVEL+10                                  
         MH    R3,WLINE            PLUS WIDTH OF ACCUM ROW                      
         MH    R4,WLINE                                                         
         A     R3,AACCUM           PLUS START OF ACCUMS                         
         A     R4,AACCUM                                                        
         MVC   0(256,R3),0(R4)     MOVE ACCUMS                                  
*                                                                               
         USING NAMED,R5                                                         
         SR    R5,R5                                                            
         ICM   R5,1,REPTLEV        R5=LEVEL                                     
         BZ    SFSTX                                                            
         BCTR  R5,0                LESS ONE                                     
         LR    R1,R5                                                            
         SLL   R1,4                X 16                                         
         LA    R1,SORTLEVS(R1)     R1=LEVEL CODE                                
         CLC   2(14,R1),SPACES     IS ROW BLANK                                 
         BE    SFSTX               THEN WE DONT NEED A FIRST                    
         MHI   R5,NAMELNQ          R5=NAME ENTRY                                
         A     R5,ANAMES                                                        
         CLI   NAMELINE,NAMEHEAD   NO PRINTING IF ZERO                          
         BL    SFSTX               NO ROWNAME FOR THIS LEVEL                    
         BH    SFST3               BRANCH IF MIDLINE OR DETAIL LINE             
         MVI   FORCEHED,YES        HEADLINE SO PAGE                             
         B     SFSTX                                                            
*                                                                               
SFST3    TM    SPCSW,SPAC99+SKIPCUR SKIP THIS LEVEL                             
         BO    SFSTX                                                            
         OI    NAMENAME+6,X'40'                                                 
         CLC   NAMENAME(7),=C'FILLER '                                          
         BE    SFST47              IGNORE ACCOUNTS CALLED FILLER                
         CLI   NAMENAME,C'='       LOOK HOW NAMES WITH EQUAL SIGNS              
         BE    SFST47              ARE TREATED LIKE FILLERS HERE                
*                                                                               
SFST5    SR    R1,R1                                                            
         IC    R1,REPTLEV                                                       
         MHI   R1,RSOMX            X NUMBER OF COLUMN OPTION FIELDS             
         LA    R1,RSCELLO(R1)                                                   
         TM    SPCSW,ROWLAST       TEST LAST(DETAIL) ROW                        
         BO    SFST7                                                            
         TM    0(R1),RSCNTALL      ENTIRE LINE IS NOTOT                         
         BO    *+12                                                             
         TM    SUPSW,SUPMIN        WAS PREVIOUS A MINTOTS SUPPRESSION           
         BNO   *+8                                                              
         BAS   RE,RPT              TAKE A SPACE NOW                             
         B     SFST13                                                           
*                                                                               
SFST7    TM    0(R1),RSCNTALL      DETAIL LINE - IS ENTIRE LINE A NOTOT         
         BNO   SFST13                                                           
         OI    SUPSW,SUPDET        DETAIL SUPPRESSED                            
         TM    RSOPT1,RSRNDALL     IF ROUNDALL                                  
         BO    SFST13              DON'T GO YET                                 
         TM    SPCSW,SPAC99        OR IF RSSPACE = 99                           
         BNO   SLST35              ELSE, CLEAR LINE                             
*                                                                               
SFST13   NI    SUPSW,ALL-SUPMIN    TURN OFF SUPPRESS MINTOTS                    
         TM    DWNSTAT,DWNLOAD     ARE WE DOWNLOADING                           
         BO    SFST28              NO, SO SKIP                                  
         LA    R3,XP               DISPLACE                                     
         A     R3,RSDISP           BY REGULAR DISPLACEMENT                      
         SR    R4,R4                                                            
         IC    R4,NAMELDSP                                                      
         AR    R3,R4               PLUS SPECIAL INDENTATION                     
         L     R7,RSWLEFT          CHOP WIDTH - INDENTATION                     
         SR    R7,R4                          = AVAILABLE SPACE                 
         MVC   XPFOURTH(52),NAMESQ                                              
         CLI   NAMELINE,NAMEDETL   DETAIL LINE?                                 
         BE    SFST17              YES                                          
         CLI   LINE,51             DONT START A 'MID' TOO                       
         BL    *+8                 FAR DOWN THE PAGE                            
         MVI   FORCEHED,YES                                                     
         MVC   XPFOURTH(40),NAMETIT                                             
         MVC   XPFOURTH+40(52),NAMESQ                                           
*                                                                               
SFST17   GOTO1 ADSQUASH,DMCB,XPFOURTH,92                                        
         LA    RF,L'XP                                                          
         GOTO1 CHOPPER,DMCB,(92,XPFOURTH),((R7),(R3)),((RF),3)                  
         MVI   XPFOURTH,C' '                                                    
         MVC   XPFOURTH+1(L'XPFOURTH-1),XPFOURTH                                
         CLI   NAMELINE,NAMEDETL   DETAIL LINE?                                 
         BE    SFST29              YES                                          
         LA    R4,L'XP(,R3)        UNDERLINE 'MID2' ON P2 OR P3                 
         CLC   XPSECOND,XSPACES                                                 
         BE    *+8                                                              
         LA    R4,L'XP(,R4)                                                     
         CLI   RSNULM,X'02'        SUPPRESS MIDS UNDERLINES                     
         BNE   *+12                                                             
         MVI   1(R4),X'00'                                                      
         B     SFST21                                                           
         CLI   RSNULM,X'01'        TAKE A SPACE INSTEAD                         
         BE    SFST21                                                           
         GOTO1 UNDERLIN,DMCB,((R7),(R3)),(X'BF',(R4))                           
*                                                                               
SFST21   MVI   SPACING,2                                                        
         B     SFST47                                                           
*                                                                               
SFST28   TM    DWNSTAT,DWNMICRO    MICRO FORMAT DOWNLOADING                     
         BNO   SFST29                                                           
         CLI   NAMELINE,NAMEMID    IS IT A MIDLINE?                             
         BE    SFSTX               YES, SO IGNORE BY EXITING                    
         MVC   P,XSPACES                                                        
         MVC   P(L'NAMESQ),NAMESQ                                               
         GOTO1 ADWNL,DWNTEXT       PUT TEXT FROM P                              
*                                                                               
SFST29   TM    SPCSW,ROWLAST       IF WE'RE AT THE LOWEST LEVEL                 
         BNO   SFSTX                                                            
         TM    RSOPT1,RSMINTOT     OPTION TO SUPPRESS                           
         BNO   SFST31              UNNECESSARY TOTALS                           
         SR    R1,R1               SHOW THE COLUMNS AS WELL                     
         IC    R1,REPTLEV          LEVEL                                        
         BCTR  R1,0                LESS ONE                                     
         SLL   R1,1                X 2                                          
         LA    R1,TOTCOUNT(R1)     COUNT ITEMS                                  
         LH    RE,0(,R1)                                                        
         LA    RE,1(,RE)           ADD 1 TO THIS LEVEL COUNT                    
         STH   RE,0(,R1)                                                        
*                                                                               
SFST31   TM    DWNSTAT,DWNLOAD                                                  
         BNO   SFST33                                                           
         TM    DWNSTAT,DWNMICRO                                                 
         BO    SFST33                                                           
         CLC   NAMENAME(7),=C'NOSPACE'                                          
         BE    *+10                                                             
         CLC   NAMENAME(7),=C'NOPRINT'                                          
         BNE   *+8                                                              
         OI    DWNSTAT,DWNNOPRT    SET NOT TO PRINT                             
         GOTO1 ADWNL,DWNROWS       DOWNLOAD ROW CODES/NAMES                     
*                                                                               
SFST33   GOTO1 AEDIT                                                            
         TM    DWNSTAT,DWNLOAD                                                  
         BNO   SFST34                                                           
         GOTO1 ADWNL,DWNEOL            EOL IF DWNLOADING                        
         TM    DWNSTAT,DWNNOPRT                                                 
         BZ     *+8                                                             
         BAS   RE,CLRXP                                                         
         NI    DWNSTAT,ALL-DWNNOPRT    SET TO PRINT AGAIN                       
         B     SFSTX                                                            
*                                                                               
SFST34   DS    0H                                                               
*&&UK                                                                           
         NI    SUPSW,ALL-SUPDET                                                 
         CLC   NAMENAME(7),=C'NOSPACE'                                          
         BNE   *+16                                                             
         BAS   RE,CLRXP                                                         
         OI    SUPSW,SUPDET                                                     
         B     SFSTX                                                            
*&&                                                                             
         CLC   NAMENAME(7),=C'NOPRINT'  WE DON'T EVER PRINT ACCTS               
         BNE   SFST36                   WITH THE NAME "NOPRINT".                
         BAS   RE,CLRXP                                                         
         B     SFST45                                                           
*                                                                               
SFST36   SR    R1,R1                                                            
         IC    R1,REPTLEV                                                       
         MHI   R1,RSOMX            X NUMBER OF COLUMN OPTION FIELDS             
         LA    R1,RSCELLO(R1)                                                   
         TM    0(R1),RSCNTALL      IS IT A NOTOT ON ALL COLUMNS                 
         BNO   SFST37                                                           
         BAS   RE,CLRXP            CLEAR PRINT                                  
         B     SLST35              CLEAR LINE                                   
*                                                                               
SFST37   TM    SPCSW,SKIP99        RSSKIP=99                                    
         BNO   *+12                                                             
         MVI   SPACING,1                                                        
         B     SFST43                                                           
         TM    SPCSW,SPAC99        RSSPACE=99                                   
         BO    SFST43                                                           
         MVC   SPACING,RSSPACE                                                  
*                                                                               
SFST43   BAS   RE,RPT                                                           
         CLI   RSFOLD,0            IS THERE FOLD?                               
         BE    *+8                                                              
         BAS   RE,RPT              TAKE AN EXTRA SPACE                          
*                                                                               
SFST45   TM    SPCSW,SKIP99        RSSKIP=99                                    
         BNO   SFSTX                                                            
         SR    R1,R1                                                            
         IC    R1,SPCNT                                                         
         LA    R1,1(R1)            ADD 1 TO SPACING COUNT                       
         STC   R1,SPCNT                                                         
         CLC   SPCNT,RSSPACE       TIME FOR THE SPACE?                          
         BL    SFSTX               NOT YET                                      
         MVI   SPCNT,0             TAKE IT NOW                                  
*                                                                               
SFST47   BAS   RE,RPT                                                           
SFSTX    LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         TITLE 'REPORT - MULTIPLE LAST '                                        
***********************************************************************         
* MULTIPLE LAST                                                       *         
*  R2=RSTACK                                                          *         
***********************************************************************         
         SPACE 1                                                                
MLST     ST    RE,RTNRE                                                         
         MVI   SPCSW,0             SPACING CONTROL SWITCHES                     
         CLI   RSSPACE,99          SPACING  99  N                               
         BNE   *+8                                                              
         OI    SPCSW,SPAC99                                                     
         CLI   RSSKIP,99           SPACING  N   SKIP                            
         BNE   *+8                                                              
         OI    SPCSW,SKIP99                                                     
         L     R0,RSNROWS          FIGURE MAX ROWS                              
         BCTR  R0,0                -1                                           
         SR    R1,R1                                                            
         IC    R1,CBLEVEL                                                       
         CR    R1,R0                                                            
         BH    MLSTX                                                            
*                                                                               
MLST3    STC   R0,REPTLEV                                                       
         NI    SPCSW,ALL-SKIPCUR                                                
         CLC   RSSKIP,REPTLEV      IS THIS LEVEL TO WHICH SPACE 99              
         BNE   *+16                APPLIES                                      
         CLI   REPTLEV,0                                                        
         BE    *+8                 BUT NEVER LEVEL 0                            
         OI    SPCSW,SKIPCUR                                                    
         BAS   RE,SLST             HANDLE LAST TIME FOR LEVEL (REPTLEV)         
         CLI   REPTLEV,0           END OF REPORT                                
         BE    MLST5                                                            
         CR    R0,R1               END OF CONTROL BREAK?                        
         BE    MLSTX                                                            
         BCTR  R0,0                BACKUP ONE MORE BREAK                        
         B     MLST3                                                            
*                                                                               
MLST5    TM    SRTSW,SRTLP         HAVE WE READ LAST RECORD?                    
         BNO   MLST7               NO, STILL MORE RECORDS                       
         CLI   APGNNARL,0          ANY NARRATIVES?                              
         BE    *+8                                                              
         OI    RPSW,RPNRR          FORCE PRINT OF NARRATIVE                     
         BAS   RE,RPT              PRINT THE NARRATIVES/FOOTLINES               
         NI    RPSW,ALL-RPNRR                                                   
         CLI   RSFOOT1,0           ANY FOOTLINES                                
         BE    MLST7                                                            
         MVI   SKIPSPEC,C'F'       FORCE THE LAST FOOTLINES                     
         BAS   RE,RPT                                                           
*                                                                               
MLST7    MVI   FORCEHED,YES                                                     
*                                                                               
MLSTX    L     RE,RTNRE                                                         
         BR    RE                                                               
         TITLE 'REPORT  - HANDLE LAST TIME FOR LEVEL (REPTLEV)'                 
***********************************************************************         
* SINGLE LAST                                                         *         
*  R2=RSTACK                                                          *         
*  REPTLEV = CURRENT LEVEL                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING NAMED,R5                                                         
SLST     STM   RE,R6,SVRE                                                       
         L     R5,ANAMES                                                        
         SR    R1,R1                                                            
         ICM   R1,1,REPTLEV                                                     
         BZ    SLST5                                                            
         LR    RF,R1               RF=LEVEL                                     
         TM    RSOPT1,RSMINTOT     OPTION TO SUPPRESS                           
         BNO   SLST3               UNNECESSARY TOTALS                           
         BCTR  RF,0                LESS ONE                                     
         SLL   RF,1                X 2                                          
         LA    RF,TOTCOUNT(RF)                                                  
         LH    RE,0(,RF)           RE=NUMBER AT THIS LEVEL                      
         LA    RE,1(,RE)           ADD 1 TO THIS LEVEL COUNT                    
         STH   RE,0(,RF)                                                        
         LH    RE,2(,RF)           LOOK AT THE LEVEL BELOW                      
         XC    2(2,RF),2(RF)                                                    
         CHI   RE,1                AND IGNORE IF EXACTLY 1                      
         BNE   SLST3                                                            
         OI    SUPSW,SUPMIN        MINTOTS SUPPRESSED                           
         B     SLST35                                                           
*                                                                               
SLST3    LR    R5,R1               R5=LEVEL                                     
         BCTR  R5,0                LESS ONE                                     
         MHI   R5,NAMELNQ          X LENGTH OF ENTRY                            
         A     R5,ANAMES                                                        
         NI    SUPSW,ALL-SUPMIN                                                 
         CLI   NAMELINE,NAMENONE   NO NAMES                                     
         BE    SLST35                                                           
         CLC   NAMENAME(7),=C'FILLER '                                          
         BE    SLST33              NO TOTALS FOR FILLER                         
         LR    RF,R1               RF=LEVEL                                     
         BCTR  RF,0                LESS ONE                                     
         SLL   RF,4                X 16                                         
         LA    RF,LASTLEVS(RF)                                                  
         CLC   2(14,RF),SPACES     TEST KEY DATA                                
         BE    SLST35              NO KEYS                                      
*                                                                               
SLST5    LR    RF,R1               RF=LEVEL                                     
         MHI   RF,RSOMX            X NUMBER OF COLUMN OPTION FIELDS             
         LA    RF,RSCELLO(RF)                                                   
         TM    0(RF),RSCNTALL      IS IT A NOTOT ON ALL COLUMNS                 
         BO    SLST35              OK TO EXIT                                   
         TM    SUPSW,SUPDET        IF DETAIL WAS SUPPRESSED WE PROBABLY         
         BO    SLST11              DON'T WANT TO TAKE THIS SPACE.               
         TM    SPCSW,SPAC99+SKIPCUR                                             
         BO    *+8                                                              
         BAS   RE,RPT              SPACE                                        
*                                                                               
SLST11   NI    SUPSW,ALL-SUPDET                                                 
         MVI   SPCNT,0             RESET SPACING COUNT                          
         MVC   XPFOURTH(L'LTOTFOR),LTOTFOR  TOTALS FOR .....                    
         LA    RE,XPFOURTH+L'LTOTFOR-1                                          
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    RE,2(,RE)                                                        
         CLI   REPTLEV,0           IS IT TOTAL LINE                             
         BNE   SLST13                                                           
         MVC   0(L'LREPORT,RE),LREPORT                                          
         CLI   RSSRTOT,NO          IF NOT WANTED ON SAME PAGE                   
         BE    *+8                                                              
         MVI   FORCEHED,YES        REPORT TOTALS ON SEPARATE PAGE               
         CLI   LASTLEVS+16,0                                                    
         BE    SLST17                                                           
         CLC   LASTLEVS(1),LASTLEVS+16                                          
         BE    SLST17                                                           
         MVI   FORCEHED,YES                                                     
         B     SLST17                                                           
*                                                                               
SLST13   MVC   0(52,RE),NAMESQ                                                  
         CLC   NAMESQ,SPACES                                                    
         BNE   SLST17                                                           
         MVC   0(40,RE),NAMETIT                                                 
*                                                                               
SLST17   LA    R3,XP                                                            
         A     R3,RSDISP           DISPLACE INTO PRINT LINE                     
         L     R4,RSWLEFT          CHOP INTO AVAILABLE SPACE                    
         SR    RF,RF                                                            
         IC    RF,NAMELDSP         FURTHER SELECTED DISPLACEMENT                
         AR    R3,RF                                                            
         SR    R4,RF               CUTS DOWN ON AVAILABLE SPACE                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         TM    SPCSW,SKIPCUR       RSSKIP=REPTLEV                               
         BNO   SLST21                                                           
         TM    SPCSW,SPAC99                                                     
         BO    SLST23                                                           
*                                                                               
SLST21   CLI   0(RE),C'='          PRINT NAME WITHOUT 'TOTALS FOR'              
         BNE   *+14                                                             
         LA    RE,1(,RE)                                                        
*                                                                               
SLST23   MVC   XPFOURTH(119),0(RE)                                              
         TM    DWNSTAT,DWNLOAD     DOWNLOADING?                                 
         BO    SLST34                                                           
         GOTO1 CHOPPER,DMCB,(64,XPFOURTH),((R4),(R3)),(198,3)                   
         MVC   XPFOURTH,XSPACES                                                 
*&&UK                                                                           
         TM    RSOPT2,RSUNDRTO        UNDERLINE TOTAL                           
         BNO   SLST25                                                           
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         SR    RF,RF                                                            
         IC    RF,MAXLINES                                                      
         SR    RF,R0                                                            
         CHI   RF,3                                                             
         BL    SLST25                                                           
         L     RF,DMCB+8                                                        
         AR    RF,R0                                                            
         L     R4,ADBXAREA                                                      
         USING BOXD,R4                                                          
         MVI   BOXINIT,0                                                        
         LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'M'                                                       
*&&                                                                             
SLST25   CLI   REPTLEV,0           IS IT TOTAL LINE                             
         BE    *+8                 YES                                          
         OI    HPSW,HPOFF                                                       
         GOTO1 AEDIT                                                            
         NI    HPSW,X'FF'-HPOFF                                                 
         CLC   NAMENAME(7),=C'NOPRINT'  DON'T PRINT NOPRINTS                    
         BE    SLST34                                                           
         TM    SPCSW,SPAC99+SKIPCUR                                             
         BO    *+8                                                              
         MVI   SPACING,2                                                        
         BAS   RE,RPT                                                           
         B     SLSTX                                                            
*                                                                               
SLST33   BAS   RE,RPT                                                           
         B     SLST35                                                           
*                                                                               
SLST34   BAS   RE,CLRXP                                                         
*                                                                               
SLST35   SR    R4,R4                                                            
         IC    R4,REPTLEV          LINE NUMBER                                  
         LA    R4,20(R4)           PLUS 20                                      
         MH    R4,WLINE            X WIDTH                                      
         A     R4,AZRO             PLUS START                                   
         LA    R5,WLN              SET LENGTH                                   
         L     RE,AZRO             A(THE ZERO LINE)                             
         LR    RF,R5                                                            
         MVCL  R4,RE               MOVE ZEROS TO LINE                           
*                                                                               
SLSTX    LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         TITLE 'REPORT - INITIALIZE NAME STACK FOR NEW REPORT'                  
***********************************************************************         
* INIT NAME STACK                                                     *         
*  R2=RSTACK                                                          *         
***********************************************************************         
         SPACE 1                                                                
NAMEINIT STM   RE,R6,SVRE                                                       
         USING RSTACKD,R2                                                       
         TM    RSOPT1,RSNOPAGE        OPTION NOT TO RESET PAGE                  
         BO    *+10                                                             
         MVC   PAGE,=H'1'                                                       
         L     R5,ANAMES                                                        
         USING NAMED,R5                                                         
         LA    R3,RSARNAME                                                      
         LA    R4,RSROWS                                                        
         L     R0,RSNROWS                                                       
*                                                                               
NI2      MVI   NAMESQ,C' '                                                      
         MVC   NAMESQ+1(L'NAMENTRY-1),NAMESQ                                    
         XC    NAMETYPE(3),NAMETYPE                                             
         ICM   R1,15,0(R3)                                                      
         BZ    NI4                                                              
         MVC   NAMETYPE(3),2(R1)   1-3/LINE/DISP                                
         SR    RE,RE                                                            
         IC    RE,1(R1)                                                         
         SHI   RE,5                                                             
         STC   RE,NAMELTIT                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NAMETIT(0),5(R1)                                                 
*                                                                               
NI4      LA    R3,4(R3)                                                         
         LA    R4,3(R4)                                                         
         LA    R5,NAMELNQ(R5)                                                   
         BCT   R0,NI2                                                           
         LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         TITLE 'REPORT - DIG OUT CODES INTO NAME STACK'                         
***********************************************************************         
* DIG OUT CODES INTO NAME STACK                                       *         
***********************************************************************         
         SPACE 1                                                                
DIGCODE  STM   RE,R6,SVRE                                                       
         USING RSTACKD,R2                                                       
         L     R5,ANAMES                                                        
         USING NAMED,R5                                                         
         LA    R3,SORTLEVS                                                      
         L     R0,RSNROWS                                                       
         LA    R4,SORTNAMS                                                      
*                                                                               
DC2      MVC   NAMEEFF,2(R3)                                                    
         MVC   NAMENAME,0(R4)                                                   
         MVC   NAMESQ,SPACES                                                    
         TM    NAMETYPE,X'02'      DO WE WANT THE CODE                          
         BNO   *+10                                                             
         MVC   NAMESQ(14),NAMEEFF                                               
         TM    NAMETYPE,X'01'      DO WE NEED THE NAME                          
         BNO   DC20                                                             
         MVC   NAMESQ+14(36),NAMENAME                                           
         CLC   NAMESQ,SPACES       IF NAME IS NOT AVAILABLE                     
         BNE   *+10                                                             
         MVC   NAMESQ(14),NAMEEFF  USE NUMBER                                   
         GOTO1 ADSQUASH,DMCB,NAMESQ,52                                          
*                                                                               
DC20     LA    R3,16(R3)                                                        
         LA    R4,36(R4)                                                        
         LA    R5,NAMELNQ(R5)                                                   
         BCT   R0,DC2                                                           
         LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         TITLE 'REPORT - CONTROL LINE PRINTING'                                 
***********************************************************************         
* CONTROL LINE PRINTING                                               *         
*  R2=RSTACK                                                          *         
***********************************************************************         
         SPACE 1                                                                
RPT      NTR1  ,                                                                
         TM    DWNSTAT,DWNLOAD     DOWNLOADING?                                 
         BO    REPX                YES                                          
         SR    R1,R1                                                            
         IC    R1,MAXLINES                                                      
         CLI   RSFOOT1,0           ANY FOOTLINES                                
         BE    RPT03                                                            
         MVC   XFOOT1(50),RSFOOT1  SET UP THE FOOTLINES                         
         MVC   XFOOT2(50),RSFOOT2                                               
*                                                                               
RPT03    CLI   APGNNARL,0          ANY NARRATIVE LINES TO PRINT                 
         BE    RPT19               NO NARRATIVES, JUST REGULAR PRINT            
         TM    RPSW,RPNRR          FORCE PRINT OF NARRATIVE                     
         BO    RPT11               FOR LAST PAGE                                
         TM    RPSW,RPLUP          HAS FIRST LINE BEEN PRINTED                  
         BNO   RPT19               JUST PRINT THE FIRST SET                     
         CLI   FORCEHED,YES        REQUESTING NEW PAGE?                         
         BE    RPT11               FORCE PRINTING OF NARRATIVE                  
         LA    RF,1                COUNT NUMBER OF PRINT LINES                  
         LA    R3,3                CHECK SECOND, THIRD AND FOURTH               
         LA    RE,XPSECOND                                                      
*                                                                               
RPT05    CLC   0(L'XP,RE),XSPACES  ANYTHING TO PRINT                            
         BE    RPT09               END OF PRINT LINES                           
         LA    RF,1(RF)                                                         
         LA    RE,L'XP(RE)                                                      
         BCT   R3,RPT05                                                         
*                                                                               
RPT09    SR    R0,R0                                                            
         IC    R0,LINE             RF = LINES TO BE PRINTED                     
         AR    RF,R0               ADD CURRENT LINE                             
         IC    R0,SPACING                                                       
         AR    RF,R0               ADD SPACING                                  
         IC    R0,APGNNARL                                                      
         AR    RF,R0               ADD NARRATIVE LINES                          
         CR    RF,R1               IS TOTAL MORE THAN MAXLINES                  
         BNH   RPT19               DON'T PRINT NARRATIVE                        
*                                                                               
RPT11    MVI   FORCEHED,NO         CAN'T GO TO NEW PAGE JUST YET                
         MVC   APGSVSPC,SPACING    SAVE SPACING                                 
         MVI   SPACING,1                                                        
         L     RE,AXPLN            RE = A(SAVE PRINT LINES)                     
         LA    RF,XP               RF = PRINT LINES                             
         LA    R0,4                                                             
         MVC   0(L'XP,RE),0(RF)    SAVE THE 4 XP LINES                          
         MVC   0(L'XP,RF),XSPACES  AND CLEAR THEM                               
         LA    RE,L'XP(RE)                                                      
         LA    RF,L'XP(RF)                                                      
         BCT   R0,*-20                                                          
         L     R4,ADBXAREA                                                      
         USING BOXD,R4                                                          
         MVI   BOXREQ,C'C'         CLOSE THE BOX                                
         GOTO1 ACREPORT                                                         
         MVI   BOXOFF,YES          TURN OFF THE BOXES                           
         IC    R0,APGNNARL         NUMBER OF NARRATIVE LINES                    
         SR    R1,R0               R1 = FIRST LINE FOR NARRATIVES               
         CLM   R1,1,LINE           IS THIS THE START OF NARRATIVES              
         BNH   *+10                OK, TO BEGIN NARRATIVE PRINT                 
         BASR  RE,RF               SPACE DOWN 1 LINE AT A TIME                  
         B     *-10                                                             
         L     R5,ANARBK            A(NARRATIVE BLOCK)                          
         MVC   XP+50(NARBKLQ),0(R5) NARRATIVE TO PRINT                          
         BASR  RE,RF                                                            
         LA    R5,NARBKLQ(R5)      PRINT ALL THE LINES                          
         BCT   R0,*-12                                                          
         MVC   SPACING,APGSVSPC    RESTORE SPACING                              
         MVI   BOXOFF,C' '                                                      
         L     RE,AXPLN            RE = A(SAVE PRINT LINES)                     
         LA    RF,XP               RF = PRINT LINES                             
         LA    R0,4                                                             
         MVC   0(L'XP,RF),0(RE)    RESTORE 4 XP LINES                           
         MVC   0(L'XP,RE),XSPACES  AND CLEAR THEM                               
         LA    RE,L'XP(RE)                                                      
         LA    RF,L'XP(RF)                                                      
         BCT   R0,*-20                                                          
         TM    RPSW,RPNRR          FORCE NARR. ON LAST PAGE                     
         BO    RPT30               JUST EXIT                                    
         MVI   FORCEHED,YES        FORCE NEW PAGE, NEXT TIME                    
*                                                                               
RPT19    CLI   FORCEHED,YES        IF FORCE HEAD                                
         BE    RPT21                                                            
         CLI   FORCEMID,YES        FORCE MID                                    
         BE    RPT21                                                            
         CLI   SKIPSPEC,C'F'                                                    
         BE    RPT21                                                            
         TM    RPSW,RPBLK          WAS LAST LINE A BLANK                        
         BNO   RPT21               NO, OK TO PRINT THIS LINE                    
         CLC   XP,XSPACES          IS THIS LINE A BLANK                         
         BE    RPT30               DON'T PRINT 2 CONSECUTIVE BLANKS             
*                                                                               
RPT21    NI    RPSW,ALL-RPBLK      TURN OFF BLANK BIT                           
         CLC   XP,XSPACES          IS THIS A BLANK LINE                         
         BNE   *+12                                                             
         MVI   SPACING,1           THEN NO EXTRA SPACE                          
         OI    RPSW,RPBLK          TURN-ON BLANK LINE PRINTED                   
         CLI   SPACING,1           OR WILL A LINE BE SKIPPED                    
         BNH   *+12                                                             
         MVI   SPACING,2           BUT NEVER MORE THAN 1 BLANK                  
         OI    RPSW,RPBLK                                                       
         GOTO1 ACREPORT                                                         
         OI    RPSW,RPLUP          SET FIRST LINE INDICATOR                     
*                                                                               
RPT30    B     REPX                                                             
         TITLE 'REPORT - CLEAR PRINT / ACCUMULATOR LINES'                       
***********************************************************************         
* CLEAR PRINT LINES                                                   *         
***********************************************************************         
         SPACE 1                                                                
CLRXP    MVC   XP,XSPACES          CLEAR PRINT LINES                            
         MVC   XPSECOND,XSPACES                                                 
         MVC   XPTHIRD,XSPACES                                                  
         MVC   XPFOURTH,XSPACES                                                 
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* CLEAR ACCUMULATOR LINES                                             *         
***********************************************************************         
         SPACE 1                                                                
CLRAM    LA    R1,1                CLEAR SOME LINES                             
CLRAM3   L     R4,AZRO             R4=A(ZERO'S)                                 
         LA    R5,WLN              R5=WITH                                      
         LR    R2,R1               ACCUMULATOR ROW                              
         MHI   R2,WLN              X WIDTH                                      
         AR    R2,R4               PLUS START                                   
         LR    R3,R5                                                            
         MVCL  R2,R4               MOVE IN THE ZEROS                            
         AHI   R1,1                NEXT LINE                                    
         CHI   R1,50               CLEAR 50 LINES                               
         BNH   CLRAM3                                                           
         BR    RE                                                               
         TITLE 'REPORT - CALCULATE HORIZONTAL PERCENT'                          
***********************************************************************         
* CALCULATE HORIZONTAL PERCENT                                        *         
*  R2=RSTACK                                                          *         
***********************************************************************         
         SPACE 1                                                                
CALPCT   STM   RE,R6,SVRE                                                       
         LA    R4,LNDID            R4 = DIVIDEND ACCUMS                         
         MH    R4,WLINE                                                         
         A     R4,AACCUM                                                        
         LA    R3,SORTCOLS         R3 = DIVISOR                                 
         L     RF,RSNCOLS          NUMBER OF COLUMNS                            
*                                                                               
CALPCT3  ZAP   WORK(16),0(8,R4)    DIVIDE TO GET PERCENT                        
         CP    0(8,R3),=P'0'                                                    
         BE    CALPCT5                                                          
         TM    HPSW,HPPCT          IS IT PERCENT                                
         BNO   *+10                                                             
         SRP   WORK(16),6,0                                                     
         DP    WORK(16),0(8,R3)                                                 
         ZAP   0(8,R3),WORK(8)                                                  
*                                                                               
CALPCT5  LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   RF,CALPCT3                                                       
         LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         TITLE 'REPORT - CONSTANTS'                                             
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
RTNRE    DS    F                   LOCAL SAVE REGISTER RE                       
TRCGCNT  DC    PL2'0'                                                           
TOTCOUNT DC    12H'0'                                                           
LASTREP  DS    CL4                                                              
LASTDUM  DS    CL14                                                             
LASTLEVS DS    CL160                                                            
*                                                                               
SUPSW    DC    X'00'                                                            
SUPMIN   EQU   X'80'               SUPPRESS MINTOTS                             
SUPDET   EQU   X'40'               SUPPRESS DETAIL                              
*                                                                               
***********************************************************************         
* SPACING N          MEANS: SPACE N AFTER EVERY LINE                  *         
*                           RSSPACE=N, RSSKIP=0                       *         
*                                                                     *         
* SPACING N  SKIP    MEANS: SPACE AFTER EVERY NTH LINE                *         
*                           RSSPACE=N, RSSKIP=99                      *         
*                                                                     *         
* SPACING 99 N       MEANS: SUPPRESS THE SPACE AFTER THE TOTAL FOR    *         
*                           THE NTH ROW                               *         
*                           RSSPACE=99, RSSKIP=N                      *         
***********************************************************************         
SPCSW    DC    X'00'                                                            
SPAC99   EQU   X'80'               RSSPACE=99                                   
SKIP99   EQU   X'40'               RSSKIP=99                                    
SKIPCUR  EQU   X'08'               RSSKIP=REPTLEV                               
ROWLAST  EQU   X'04'               THIS IS THE LAST(DETAIL ROW)                 
*                                                                               
SPCNT    DS    XL1                 SPACING COUNT                                
*                                                                               
APGSVSPC DS    XL1                 SAVED SPACING                                
THSMRGE  DS    CL(SORTLNQ)         CURRENT SORTER RECORD                        
NXTMRGE  DS    CL(SORTLNQ)         NEXT SORTER RECORD                           
CBLEVEL  DS    XL1                                                              
DWNRPT   DS    XL1                 DOWNLOAD REPORT NUMBER                       
         EJECT                                                                  
         LTORG                                                                  
         DROP   RB,R9,R8                                                        
         TITLE 'EDIT - EDIT A ROW OF ACCUMULATORS'                              
***********************************************************************         
* EDIT A ROW OF ACCUMULATORS                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
EDIT     DS    0D                                                               
         NMOD1 0,**EDIT**,R9                                                    
         L     RC,BASERC                                                        
*                                                                               
         L     R5,VBIGPRNT                                                      
         LA    R5,XP-BIGPRNTD(R5)  R5=A(XP)                                     
         LA    R5,1(R5)            +1                                           
         A     R5,RSDISP           +DISP. INTO PRINT LINE                       
         A     R5,RSWLEFT          +WIDTH OF LEFT SIDE                          
         ST    R5,SVXP                                                          
         LA    RF,DIFFS                                                         
         ST    RF,ADIFFS                                                        
*                                                                               
         LA    RF,EDB              EDIT FOR BUDGET                              
         SLL   RF,2                                                             
         A     RF,ATYPEDIT         RF TO EDIT FORMAT FOR BUDGET                 
         MVI   2(RF),C'0'          DEFAULT NO DP                                
         TM    RSOPT1,RSBUDCNT     PRINT BUDGETS TO THE PENNY                   
         BNO   *+8                                                              
         MVI   2(RF),C'2'          2 DP FOR THIS REPORT                         
*                                                                               
         SR    R3,R3                                                            
         IC    R3,REPTLEV          CURRENT LEVEL                                
         LA    R3,20(R3)           +20                                          
         MH    R3,WLINE            X WIDTH                                      
         A     R3,AACCUM           R3=A(ACCUMULATOR LINE) LEVEL +20             
*                                                                               
         SR    R7,R7                                                            
         IC    R7,REPTLEV          LEVEL                                        
         MHI   R7,RSOMX            X NUMBER OF COLUMN OPTION FIELDS             
         LA    R7,RSCELLO(R7)      R7=CELL OPTIONS                              
         MVI   CURRCOL,1           START WITH COLUMN 1                          
         LA    R4,RSCOLS           R4=COLUMN INSTRUCTIONS                       
         L     R0,RSNCOLS          R0=NUMBER OF COLS                            
*                                                                               
*                                                                               
EDT3     SR    R6,R6                                                            
         ICM   R6,1,RSFOLD         R6=FOLD VALUE(I.E. COLUMN 14)                
         BNZ   *+6                                                              
         LR    R6,R0               R0=TOTAL NUMBER OF COLUMNS                   
         CR    R0,R6                                                            
         BH    EDT5                                                             
         LR    R6,R0                                                            
*                                                                               
EDT5     SR    RE,RE                                                            
         IC    RE,CURRCOL          COLUMN NUMBER                                
         BCTR  RE,0                                                             
         LA    RF,0(RE,R7)         CELL OPTIONS FOR THIS ROW                    
         MVC   CELLOPT,0(RF)                                                    
         LA    RF,RSCOPTN(RE)      COLUMN OPTIONS FOR THIS REPORT               
         MVC   CLMNOPT,0(RF)                                                    
         MVI   EDITSW,0            INIT EDIT CONTROL SWITCH                     
         MVI   CURRCOMP,0                                                       
         CLI   9(R4),0             ANY COLUMN COMPUTATION?                      
         BE    *+8                                                              
         BAS   RE,CCOMP            COLUMN COMPUTATIONS                          
         L     RF,ADIFFS                                                        
         ZAP   0(8,RF),0(8,R3)     SAVE ORIGINAL (BEFORE)                       
         ZAP   8(8,RF),0(8,R3)     AND (AFTER)                                  
         CP    0(8,R3),=P'0'       IS THE RESULT ZERO?                          
         BNE   EDT7                                                             
         TM    EDITSW,EDSZRO       IF DELIBERATE MULT BY ZERO                   
         BO    EDT7                OVERLAY WITH ZERO RESULT                     
         CLI   RSZERO,YES          OPTION TO SHOW ZERO                          
         BE    EDT7                YES, PRINT COLUMN                            
         TM    DWNSTAT,DWNLOAD     DOWNLOADING?                                 
         BNO   EDT17               NO, DON'T PRINT COLUMN                       
         MVI   RSZERO,YES          FORCE ZEROS TO PRINT                         
*                                                                               
EDT7     TM    0(R7),RSCNTALL      NOTOT ON ALL COLUMNS                         
         BO    EDT9                                                             
         TM    CELLOPT,RSCNTCOL    NOTOT ON SPECIFIC COLUMN                     
         BO    EDT9                                                             
         CLI   8(R4),1             IS COLUMN WIDTH =1?                          
         BNE   EDT15               NO, OK TO EDIT                               
         OC    RSOVLY,RSOVLY       ANY ENCROACH?                                
         BZ    EDT11               NO,                                          
         B     EDT15               YES, OK TO EDIT                              
*                                                                               
*                                  FOR NOTOT COLUMNS                            
EDT9     TM    RSOPT1,RSRNDALL     IS ROUNDALL ON?                              
         BO    EDT13               YES, EDIT AND HANDLE ROUNDING                
         CLI   8(R4),1             W=1 ?                                        
         BNE   EDT13               NO, EDIT AND HANDLE ROUNDING                 
         MVI   RNDSTOP,YES         TURN-OFF THE ROUNDING                        
         B     EDT13               EDIT                                         
*                                                                               
EDT11    TM    RSOPT1,RSRNDALL     ROUNDALL EFFECTIVE IF W=1                    
         BNO   EDT17               AND NO ENCROACH                              
*                                                                               
EDT13    BAS   RE,ED10             EDIT                                         
         BAS   RE,ED99             HANDLE ROUNDING PROBLEMS                     
         B     EDT17                                                            
*                                                                               
EDT15    BAS   RE,ED10             EDIT ROUTINE                                 
*                                                                               
EDT17    L     RF,ADIFFS                                                        
         LA    RF,16(RF)           SET A(NEXT DIFFERENCE ACCUMS)                
         ST    RF,ADIFFS                                                        
         SR    R1,R1                                                            
         ICM   R1,1,8(R4)            WIDTH OF THIS COLUMN                       
         BNZ   *+8                                                              
         LA    R1,8                (8 IF NOT SPECIFIED)                         
         CHI   R1,1                W=1?                                         
         BE    *+8                                                              
         LA    R5,1(R1,R5)         R5=PRINT LINE                                
         LA    R3,8(R3)            R3=NEXT ACCUMULATOR                          
         LA    R4,29(R4)           R4=INSTRUCTIONS FOR NEXT COLUMN              
         SR    RF,RF                                                            
         IC    RF,CURRCOL                                                       
         LA    RF,1(RF)                                                         
         STC   RF,CURRCOL          INCREMENT COLUMN NUMBER                      
*                                                                               
         BCTR  R0,0                DECREMENT COLUMN COUNT                       
         BCT   R6,EDT5             PROCESS NEXT COLUMN                          
         LTR   R0,R0               FINISHED A SET OF COLS (FOLD)                
         BZ    EDT23                                                            
         L     R5,SVXP             R5=XP                                        
*                                                                               
EDT21    LA    R5,L'XP(R5)         FIND A BLANK LINE FOR NEXT FOLD              
         ST    R5,SVXP                                                          
         CLC   0(70,R5),SPACES                                                  
         BE    EDT3                FOUND A BLANK LINE                           
         L     RE,VBIGPRNT                                                      
         LA    RE,XPFOURTH-BIGPRNTD(RE)                                         
         CR    R5,RE                                                            
         BL    EDT21                                                            
*                                                                               
EDT23    L     R4,AZRO             R4=A(ZEROS)                                  
         LA    R5,WLN              R5=WIDTH                                     
         SR    RE,RE                                                            
         IC    RE,REPTLEV          CURRENT LINE                                 
         LA    RE,20(RE)           +20                                          
         MH    RE,WLINE            X WIDTH                                      
         AR    RE,R4               + START                                      
         LR    R3,RE               R3=A(CURRENT LEVEL)                          
         LR    RF,R5               WIDTH(LENGTH)                                
         MVCL  RE,R4               CLEAR LINE TO ZEROS                          
*                                                                               
         SR    R1,R1               ANY LEVEL LOWER WILL ALSO BE CLEARED         
         IC    R1,REPTLEV                                                       
         L     R0,RSNROWS          MAX # OF LEVELS                              
EDT23A   CR    R1,R0                                                            
         BNL   EDT23X                                                           
         LA    RF,WLN                                                           
         LR    R5,RF                                                            
         L     R4,AZRO             R4 WAS CHANGED BY LAST MVCL                  
         MVCL  RE,R4               RE ALREADY POINTS TO NEXT ONE                
         LA    R1,1(R1)                                                         
         B     EDT23A                                                           
*                                                                               
EDT23X   CLI   REPTLEV,0           LEVEL 0 IS REPORT TOTAL                      
         BE    EDT56                                                            
         TM    RSCOPTN,RSCNRANY    ANY NOROLL?                                  
         BO    EDT25               YES, THERE IS A NO ROLL                      
         CLI   RSRNDXF,RSRNDXFQ    FORCE ROUNDED NUMBERS TO ADD?                
         BNE   EDTXIT              NO,                                          
*                                                                               
EDT25    SR    RF,RF                                                            
         IC    RF,REPTLEV          RF=NUMBER OF LEVELS TO ADJUST                
*                                                                               
EDT29    SH    R3,WLINE            GET TO NEXT (HIGHER) LEVEL                   
         ST    R3,SVR3                                                          
         LA    R4,DIFFS                                                         
         L     RE,RSNCOLS          NUMBER OF COLUMNS                            
         LA    R5,RSCOPTN          COLUMN OPTION FIELD                          
*                                                                               
EDT31    TM    RSCOPTN,RSCNRALL    NO ROLL ON ALL COLUMNS                       
         BO    *+12                                                             
         TM    0(R5),RSCNROLL      NOROLL ON THIS COL?                          
         BNO   EDT35               NO, OK TO ADJUST FROM LOWER LEVELS           
         CLC   REPTLEV,RSNROWS+3   IS THIS THE DETAIL LEVEL                     
         BNE   EDT39               NO, JUST SKIP THE "INS" AND "OUTS"           
         B     EDT37               JUST ADD TO ACCUMS                           
*                                                                               
EDT35    SP    0(8,R3),0(8,R4)     OUT WITH THE OLD                             
EDT37    AP    0(8,R3),8(8,R4)     IN WITH THE NEW                              
EDT39    LA    R3,8(R3)            NEXT ACCUMULATOR COLUMN                      
         LA    R4,16(R4)           NEXT SET FOR DIFFERENCES                     
         LA    R5,1(R5)            NOROLL FOR COLUMN                            
         BCT   RE,EDT31            COLUMN LOOP                                  
*                                                                               
         L     R3,SVR3                                                          
         BCT   RF,EDT29            ROW LOOP - ONCE FOR EACH LEVEL               
*                                                                               
EDT56    SR    R1,R1                                                            
EDT57    LA    R4,DIFFS            CLEAR DIFFERENCES                            
         LR    RF,R1                                                            
         MH    RF,WLINE                                                         
         AR    R4,RF                                                            
         LA    R5,WLN              WIDTH(LENGTH)                                
         L     RE,AZRO                                                          
         LR    RF,R5                                                            
         MVCL  R4,RE               CLEAR DIFFS TO ZERO                          
         AHI   R1,1                                                             
         CHI   R1,1                                                             
         BNH   EDT57                                                            
         B     EDTXIT                                                           
         TITLE 'EDIT - COLUMN COMPUTATIONS'                                     
***********************************************************************         
* COLUMN COMPUTATIONS - COLCOMP                                       *         
***********************************************************************         
         SPACE 1                                                                
CCOMP    NTR1  ,                                                                
         CLC   REPTLEV,RSNROWS+3   FOR ALL BUT LOWEST LEVEL                     
         BE    CCOMP3                                                           
         TM    RSCOPTN,RSCNRALL    DON'T COLCOMP A NOROLL COLUMN                
         BO    EDTXIT                                                           
         TM    CLMNOPT,RSCNROLL    NOROLL ON THIS COLUMN                        
         BO    EDTXIT                                                           
*                                                                               
CCOMP3   LA    R4,8(R4)            POSITION TO IT (-1)                          
         ST    R4,SVR4                                                          
         SR    R5,R5               (TELL ELOAD ITS FIRST)                       
         BAS   RE,ELOAD                                                         
         B     CCOMP7                                                           
*                                                                               
CCOMP5   CP    DUB,=P'0'           IF NEGATIVE THERE MAY BE                     
         BNL   EDTXIT              A SPECIAL NEGATIVE FORMULA                   
         CLI   11(R4),0            IF THERE IS GET TO IT.                       
         BE    CCOMP7                                                           
         LA    R4,10(R4)                                                        
         OI    EDITSW,EDSMNS       SET MINUS SWITCH                             
         NI    EDITSW,ALL-EDSPLS   CLEAR PLUS SWITCH                            
         SR    R5,R5                                                            
         BAS   RE,ELOAD                                                         
*                                                                               
CCOMP7   ZAP   WORK(16),DUB                                                     
         LA    R5,4                MAX OF 4 OPERATIONS                          
         TM    EDITSW,EDSPLS       SECOND TIME THRU W/O NEG FORM.               
         BO    EDTXIT                                                           
         OI    EDITSW,EDSPLS                                                    
*                                                                               
CCOMP9   LA    R4,2(R4)                                                         
         CLI   0(R4),0                                                          
         BE    CCOMP35                                                          
         BAS   RE,ELOAD                                                         
         CLI   0(R4),1             +                                            
         BE    CCOMP11                                                          
         CLI   0(R4),2             -                                            
         BE    CCOMP13                                                          
         CLI   0(R4),3             X                                            
         BE    CCOMP15                                                          
         CLI   0(R4),4             /                                            
         BE    CCOMP19                                                          
         CLI   0(R4),5             I                                            
         BE    CCOMP21                                                          
         CLI   0(R4),7             R                                            
         BE    CCOMP23                                                          
         B     CCOMP25             %                                            
*                                                                               
CCOMP11  AP    WORK(16),DUB        ADD                                          
         B     CCOMP33                                                          
*                                                                               
CCOMP13  SP    WORK(16),DUB        SUBTRACT                                     
         B     CCOMP33                                                          
*                                                                               
CCOMP15  CLI   1(R4),20            DELIBERATE MULT BY ZERO                      
         BNE   CCOMP17                                                          
         CP    WORK(16),=P'0'      ON A VALID AMOUNT                            
         BE    CCOMP17                                                          
         OI    EDITSW,EDSZRO       SETS SWITCH                                  
*                                                                               
CCOMP17  MP    WORK(16),DUB        MULTIPLY                                     
         B     CCOMP33                                                          
*                                                                               
CCOMP19  CP    DUB,=P'0'           DIVIDE                                       
         BE    CCOMP27                                                          
         MP    WORK(16),=P'2'                                                   
         DP    WORK(16),DUB                                                     
         MVC   WORK+8(8),WORK                                                   
         XC    WORK(8),WORK                                                     
         CP    WORK+8(8),=P'0'                                                  
         BL    *+10                                                             
         AP    WORK+8(8),=P'1'                                                  
         DP    WORK(16),=PL8'2'                                                 
         MVC   WORK+8(8),WORK                                                   
         XC    WORK(8),WORK                                                     
         B     CCOMP33                                                          
*                                                                               
CCOMP21  MVI   CURRCOMP,C'I'       INDEX                                        
         MP    WORK(16),=P'1000000'                                             
         B     CCOMP27                                                          
*                                                                               
CCOMP23  MVC   DMCB(8),WORK+8      REVERSE PERCENT                              
         ZAP   WORK(16),DUB                                                     
         MVC   DUB,DMCB                                                         
*                                                                               
CCOMP25  MVI   CURRCOMP,C'%'       PERCENT                                      
         MP    WORK(16),=P'1000000'                                             
         MVC   SVZN,WORK+15        SAVE ORIGINAL SIGN                           
*                                                                               
CCOMP27  CP    DUB,=P'0'                                                        
         BE    CCOMP29                                                          
         OC    WORK(8),WORK        IF DIVIDEND MORE THAN 8 BYTES                
         BZ    CCOMP31                                                          
         OC    DUB(6),DUB          AND DIVISOR IS 2 BYTES OR LESS               
         BNZ   CCOMP31                                                          
*                                                                               
CCOMP29  ZAP   WORK(16),=P'0'      FORCE RESULT TO ZERO                         
         B     CCOMP33                                                          
*                                                                               
CCOMP31  DP    WORK(16),DUB                                                     
         OC    WORK(4),WORK        IF QUOTIENT IS TOO BIG                       
         BNZ   CCOMP29             FUCK IT                                      
         MVC   WORK+8(8),WORK                                                   
         XC    WORK(8),WORK                                                     
*                                                                               
CCOMP33  BCT   R5,CCOMP9                                                        
*                                                                               
CCOMP35  MVC   0(8,R3),WORK+8      RETURN RESULT IN A(R3)                       
         TM    EDITSW,EDSMNS       TEST MINUS                                   
         BO    EDTXIT                                                           
         L     R4,SVR4                                                          
         MVC   DUB,WORK+8                                                       
         CLI   CURRCOMP,C'%'       IF % REPLACE ORIGINAL SIGN                   
         BNE   *+10                                                             
         MVN   DUB+7,SVZN          FOR COLCOMP- EXAM                            
         B     CCOMP5                                                           
         TITLE 'EDIT - LOCATE SELECTED COLUMN VALUE'                            
***********************************************************************         
* LOAD SELECTED COLUMN VALUE FROM COLCOMP INTO DUB                    *         
***********************************************************************         
         SPACE 1                                                                
ELOAD    NTR1  ,                                                                
         ZAP   DUB,=P'0'                                                        
         SR    R3,R3                                                            
         IC    R3,REPTLEV          ACCUMULATOR LINE IS LEVEL                    
         LA    R3,20(,R3)                              +20                      
         LTR   R5,R5               UNLESS VERTICAL PERCENTS ARE                 
         BZ    ELOAD2              IN OPERATIONS (CANT BE ON FIRST)             
         CLI   0(R4),10            WHEN OPERATOR EXCEEDS 10                     
         BL    ELOAD2                                                           
         SR    R3,R3                                                            
         IC    R3,0(,R4)                                                        
         SHI   R3,10                                                            
         SR    R0,R0                                                            
         IC    R0,REPTLEV          VERTICAL PERCENTS ARE NOT                    
         CR    R0,R3               MEANINGFULL IF LEVEL IS TOO 'HIGH'           
         BL    EDTXIT                                                           
*&&US                                                                           
         TM    HPSW,HPOFF          IS THIS TURNOFF FOR NOW ?                    
         BO    ELOAD2              YES                                          
         TM    HPSW,HPPCT          OR ON HORIZONTAL PERCENT LINE                
         BO    EDTXIT                                                           
*&&                                                                             
*                                                                               
ELOAD2   MH    R3,WLINE                                                         
         A     R3,AACCUM                                                        
         CLI   1(R4),30            COLUMNS 30 & OVER ARE INTEGERS               
         BNL   ELOAD4                                                           
         SR    R1,R1                                                            
         IC    R1,1(,R4)           OTHERWISE PICK UP COL NO.                    
         C     R1,RSNCOLS          INSURANCE AGAINTS BLOWUP ON HIGHER           
         BH    EDTXIT              THAN HIGHEST COLUMN.                         
         BCTR  R1,0                                                             
         SLL   R1,3                                                             
         AR    R3,R1                                                            
         MVC   DUB,0(R3)           PASS BACK CONTENTS OF COLUMN                 
         B     EDTXIT                                                           
*                                                                               
ELOAD4   SR    R1,R1                                                            
         IC    R1,1(,R4)           PASS BACK INTEGER VALUE                      
         SHI   R1,30                                                            
         CHI   R1,100                                                           
         BNH   ELOAD6                                                           
         SHI   R1,100              VALUES 101-200 ARE -1 - -100                 
         LCR   R1,R1                                                            
*                                                                               
ELOAD6   CVD   R1,DUB                                                           
         B     EDTXIT                                                           
         TITLE 'EDIT - LOCATE COLUMN EDIT INSTRUCTIONS'                         
***********************************************************************         
* LOCATE COLUMN EDIT INSTRUCTIONS                                     *         
***********************************************************************         
         SPACE 1                                                                
ED10     NTR1  ,                                                                
*&&US                                                                           
         TM    HPSW,HPPCT          HORIZONTAL PERCENT                           
         BNO   ED11                                                             
         TM    HPSW,HPOFF                                                       
         BO    ED11                OPTION OFF FOR NOW                           
         MVI   CURRTYPE,KWPCT                                                   
         B     ED22                                                             
*&&                                                                             
ED11     MVI   CURRTYPE,KWSAL                                                   
         LA    R1,RSALLCOL         LOOK FOR TYPE CODE IN ALL INST.              
         LA    R0,3                                                             
*                                                                               
ED12     CLI   0(R1),9                                                          
         BE    ED14                                                             
         LA    R1,2(,R1)                                                        
         BCT   R0,ED12                                                          
         B     ED16                                                             
*                                                                               
ED14     MVC   CURRTYPE,1(R1)                                                   
*                                                                               
ED16     LR    R1,R4               NOW CHECK IF SPECIFIC COLUMN                 
         LA    R0,4                                                             
*                                                                               
ED18     CLI   0(R1),9                                                          
         BE    ED20                                                             
         LA    R1,2(R1)                                                         
         BCT   R0,ED18                                                          
         B     ED22                                                             
*                                                                               
ED20     MVC   CURRTYPE,1(R1)                                                   
*                                                                               
ED22     LR    R0,R5               SAVE R5 A(PRINT AREA)                        
         SR    R5,R5                                                            
         IC    R5,CURRTYPE         USE TYPE CODE TO INDEX                       
         SLL   R5,3                INTO TYPE LIST                               
         A     R5,ATYPELST                                                      
         USING TYPLD,R5                                                         
         SR    R1,R1                                                            
         IC    R1,TYPLED           GET EDIT NUMBER                              
         SLL   R1,2                AND INDEX INTO EDIT TABLE                    
         A     R1,ATYPEDIT                                                      
         LR    R5,R0               RESTORE R5                                   
         MVC   CURRED,2(R1)                                                     
         CLI   CURRCOMP,C'%'       CHECK IF COMPUTES AFFECT EDIT                
         BNE   ED220                                                            
         MVC   CURRED,=C'1%'                                                    
         TM    RSOPT1,RSWHOLEP                                                  
         BNO   *+8                                                              
         MVI   CURRED,C'0'                                                      
*                                                                               
ED220    CLI   CURRCOMP,C'I'                                                    
         BNE   *+10                                                             
         MVC   CURRED,=C'0 '                                                    
         TM    0(R7),RSCNTALL      NOTOT ALL COLUMNS                            
         BO    EDTXIT                                                           
         TM    CELLOPT,RSCNTCOL    NOTOT COLUMN                                 
         BO    EDTXIT                                                           
         CLI   8(R4),1             W=1 COLUMNS                                  
         BNE   ED221                                                            
         OC    RSOVLY,RSOVLY       AND NO ENCROACH                              
         BZ    EDTXIT              EXIT HERE                                    
*                                                                               
ED221    BAS   RE,ED30                                                          
         B     EDTXIT                                                           
         TITLE 'EDIT - EDIT THE COLUMN'                                         
***********************************************************************         
* EDIT THE COLUMN                                                     *         
***********************************************************************         
         SPACE 1                                                                
ED30     NTR1                                                                   
         MVC   EDOUT,SPACES                                                     
         SR    R6,R6                                                            
         ICM   R6,1,8(R4)          GET COLUMN WIDTH                             
         BNZ   *+8                                                              
         LA    R6,8                DEFAULT IS 8                                 
         CLI   8(R4),1             W=1 CAN ENCROACH ON OTHER COLS               
         BNE   NOENC               DON'T WORRY ABOUT ENCROACH                   
         LH    R6,RSOVLY           ENCROACH SIZE                                
         SR    R5,R6               R5=PRINT AREA - OVLY (ENCROCH SIZE)          
         BCTR  R5,0                                                             
NOENC    LA    R7,EDOUT+4                                                       
         CLI   CURRED+1,C' '       TRAILER (USUALLY PERCENT)                    
         BE    *+12                                                             
         MVC   EDOUT+19(1),CURRED+1                                             
         BCTR  R7,0                                                             
         LA    R4,EDOUT+19         POSITION R4 TO FROM-1                        
         SR    R4,R6                                                            
         BCTR  R6,0                                                             
         ZAP   SVAMT,0(8,R3)     SAVE ORIG IN CASE OF MANIPULATION              
         TM    CELLOPT,RSCRVEDT  IS REVEDIT ON FOR THIS COL                     
         BNO   ED31                                                             
         MP    0(8,R3),=P'-1'                                                   
*                                                                               
ED31     CLI   CURRED,C'2'         HANDLE 2 DEC                                 
         BL    ED32                                                             
         BAS   RE,ED99                                                          
         CLI   RSZERO,YES                                                       
         BNE   ED31A                                                            
         CURED (P8,(R3)),(16,(R7)),2,FLOAT=-,ZERO=NOBLANK                       
         B     ED31B                                                            
ED31A    CURED (P8,(R3)),(16,(R7)),2,FLOAT=-                                    
ED31B    BAS   RE,FIT                                                           
         BE    SEEREV                                                           
*                                                                               
ED32     DS    0H             ************ 1 DEC                                
         CLI   CURRED,C'1'                                                      
         BL    ED34                                                             
*                                                                               
ED320    BAS   RE,ED99                                                          
         CLI   RSZERO,YES                                                       
         BNE   ED32A                                                            
         CURED (P8,(R3)),(16,EDOUT2),2,FLOAT=-,ZERO=NOBLANK                     
         B     ED32B                                                            
*                                                                               
ED32A    CURED (P8,(R3)),(16,EDOUT2),2,FLOAT=-                                  
ED32B    MVC   0(16,R7),EDOUT2-1                                                
         CLC   EDOUT+16(4),=C' .0%' COSMETICS                                   
         BE    SEEREV                                                           
         CLI   CURRED+1,C'%'                                                    
         BNE   ED33                                                             
         CLC   EDOUT+13(7),=C' 100.0%'                                          
         BL    ED33                                                             
         TM    RSOPT1,RSMX100P     OPTION TO LIMIT TO 100%                      
         BNO   ED32C                                                            
         MVC   EDOUT,SPACES                                                     
         MVC   EDOUT+13(7),=C' 100.0%'                                          
         CP    0(8,R3),=P'0'                                                    
         BNL   *+8                                                              
         MVI   EDOUT+13,C'-'       NEGATIVE PERCENT                             
*                                                                               
ED32C    TM    RSOPT1,RSNO100P     OPTION TO SUPPRESS 100%                      
         BO    SEEREV                                                           
*                                                                               
ED33     BAS   RE,FIT                                                           
         BE    SEEREV                                                           
*                                                                               
ED34     DS    0H              ************ 0 DEC                               
         CLI   CURRED+1,C'%'                                                    
         BNE   ED340                                                            
         AP    0(8,R3),=P'50'                                                   
         BP    *+10                                                             
         SP    0(8,R3),=P'100'                                                  
ED340    BAS   RE,ED99                                                          
         CURED (P8,(R3)),(16,EDOUT2),2,FLOAT=-                                  
         MVC   0(16,R7),EDOUT2-3                                                
         CLI   RSZERO,YES                                                       
         BNE   ED340A                                                           
         CLC   0(16,R7),SPACES                                                  
         BNE   ED340A                                                           
         MVI   15(R7),C'0'                                                      
ED340A   CLC   EDOUT+16(4),=C'   %'                                             
         BE    SEEREV                                                           
         CLI   CURRED+1,C'%'         ONLY FIX UP %S                             
         BNE   ED34A                                                            
         CLC   EDOUT+13(7),=C'   100%'                                          
         BL    ED34A                                                            
         TM    RSOPT1,RSMX100P     MAXIMUM PERCENT IS 100                       
         BNO   ED340B                                                           
         MVC   EDOUT,SPACES                                                     
         MVC   EDOUT+13(7),=C'   100%'                                          
         CP    0(8,R3),=P'0'                                                    
         BNL   *+8                                                              
         MVI   EDOUT+15,C'-'       NEGATIVE PERCENT                             
ED340B   TM    RSOPT1,RSNO100P     SUPPRESS 100% NUMBERS                        
         BO    SEEREV                                                           
ED34A    BAS   RE,FIT                                                           
         BE    SEEREV                                                           
         SHI   R5,3                                                             
         LA    R6,3(,R6)                                                        
         SHI   R4,3                                                             
         CLC   0(3,R5),SPACES                                                   
         BE    *+8                                                              
         LA    R5,L'XP(,R5)                                                     
         EX    R6,EDMOVE                                                        
         B     SEEREV                                                           
*                                                                               
         DC    F'0'                                                             
FIT      CLI   0(R4),C' '          DOES NUMBER FIT?                             
         BNER  RE                  NO SO TRY NEXT EDITING OPTION                
         TM    DWNSTAT,DWNLOAD     TEST DOWNLOADING                             
         BNO   FIT10               NO, SO CONTINUE AS USUAL                     
         ST    RE,FIT-4                                                         
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P        CLEAR P                                      
         LA    R5,P                                                             
         EX    R6,EDMOVE                                                        
         GOTO1 ADWNL,DWNNUMB                                                    
         SR    RF,RF               SET CONCODE TO EQUAL                         
         L     RE,FIT-4                                                         
         BR    RE                                                               
*                                                                               
FIT10    EX    R6,EDMOVE                                                        
         BR    RE                  YES, SO MOVE TO PRINT LINE                   
EDMOVE   MVC   0(0,R5),1(R4)                                                    
*                                                                               
SEEREV   ZAP   0(8,R3),SVAMT                                                    
         B     EDTXIT                                                           
         TITLE 'EDIT - HANDLE ROUNDING PROBLEMS'                                
***********************************************************************         
* HANDLE ROUNDING PROBLEMS                                            *         
***********************************************************************         
         SPACE 1                                                                
ED99     CLI   CURRED+1,C'%'      PERCENTS HAVE 2 EXTRA DECIMAL PLACES          
         BE    EDPCT                                                            
         CLI   CURRCOMP,C'I'                                                    
         BE    EDPCT                                                            
*                                                                               
         CLI   RNDSTOP,YES         SOMEONE DOESN'T WANT US TO ROUND             
         MVI   RNDSTOP,NO                                                       
         BER   RE                                                               
         CLI   RSROUND+1,C'F'      FORCE TOTALS TO CONFORM?                     
         BE    ED99A               YES                                          
         TM    RSCOPTN,RSCNRANY    ANY NOROLL?                                  
         BO    ED99A                                                            
         CLI   CURRED,C'0'         ALLOW IT TO ROUND TO NEAREST DOLLAR          
         BNER  RE                  ONLY, ON NON-FORCE OR NON-NOROLL             
*                                                                               
ED99A    CLI   CURRED,C'0'         FOR OTHER THAN NO DEC DONT ROUND             
         BNER  RE                                                               
         SR    R1,R1                                                            
         IC    R1,RSROUND          0=DOLLARS,1=TENS,2=HUNDS,3=THOUS             
         LA    RF,62               ROUND RIGHT BY TWO TO START                  
         SR    RF,R1               ADDITIONAL ROUND RIGHT FACTOR                
         SRP   0(8,R3),0(RF),5     SHIFT RIGHT AND ROUND                        
         SRP   0(8,R3),2,0         SHIFT BACK LEFT BY TWO                       
         L     RF,ADIFFS                                                        
         ZAP   8(8,RF),0(8,R3)                                                  
         SRP   8(8,RF),0(R1),0     SHIFT LEFT ADDITIONAL ROUND FACTOR           
         ZAP   SVAMT,8(8,RF)       SAVE FOR RESTORING WITH ZEROS                
         BR    RE                                                               
*                                                                               
EDPCT    L     RF,ADIFFS                                                        
         ZAP   0(8,RF),0(8,R3)  SAVE OLD NUMBER - WITH 4 DECIMAL PLACES         
         SRP   0(8,R3),64-3,5   SHIFT RIGHT 3 PLACES                            
         SRP   0(8,R3),1,0      SHIFT LEFT 1 PLACE                              
         ZAP   8(8,RF),0(8,R3)  NEW NUMBER                                      
         SRP   8(8,RF),2,0      SHIFT LEFT 2 PLACES(3 TRAILING ZERO'S)          
         BNOR  RE               BRANCH ON NO OVERFLOW                           
         DC    H'0'             SHIFT OVERFLOW                                  
*                                                                               
EDTXIT   XIT1                                                                   
         TITLE 'EDIT - CONSTANTS'                                               
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
CELLOPT  DS    X                   OPTIONS FROM RSCELLO                         
CLMNOPT  DS    X                   OPTIONS FROM RSCCOPT                         
EDITSW   DS    X                   LOCAL EDIT CONTROL SWITCH                    
EDSZRO   EQU   X'80'               FORCE ZERO FROM FORMULA                      
EDSMNS   EQU   X'40'               MINUS                                        
EDSPLS   EQU   X'20'               PLUS                                         
SVZN     DS    C                                                                
ZEROS3   DC    CL3'000'                                                         
EDOUT    DC    CL20' '                                                          
         DC    CL4' '                                                           
EDOUT2   DC    CL16' '                                                          
SVAMT    DC    PL8'0'                                                           
SAV1     DS    CL1                                                              
SAV2     DS    CL1                                                              
RNDSTOP  DC    C'N'                                                             
SVXP     DS    F                                                                
CURRCOMP DS    CL1                                                              
ADIFFS   DS    F                                                                
DIFFS    DC    (NAC*2)PL8'0'                                                    
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,R9                                                            
         TITLE 'HEAD - HEADLINE HOOK'                                           
***********************************************************************         
* HEADLINE HOOK ROUTINES                                              *         
***********************************************************************         
         SPACE 1                                                                
HEAD     DS    0D                                                               
         TM    DWNSTAT,DWNLOAD     DOWNLOADING?                                 
         BOR   RE                  IGNORE HEADHOOK                              
         NMOD1 0,**HEAD**,R9                                                    
         L     RC,BASERC                                                        
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         L     R4,ADBXAREA                                                      
         USING BOXD,R4                                                          
         CLI   STOPHOOK,YES                                                     
         BE    HEADXIT                                                          
         CLI   MODE,REQFRST        MUST BE RUNFRST FOR TRACE=YES                
         BL    HEADXIT                                                          
         CLC   SYSWIDTH,LSTWIDTH   HAS THE WIDTH CHANGED                        
         BE    HH13                ITS THE SAME                                 
         LA    R1,WIDTH132         R1 TO WIDTH TABLES                           
*                                                                               
HH09     MVC   WIDTHTAB,0(R1)      SAVE CONSTANTS FOR THIS WIDTH                
         CLC   SYSWIDTH,WIDTH      FIND MATCHING ENTRY                          
         BE    *+16                                                             
         LA    R1,WIDTHLEN(R1)     R1 TO NEXT ENTRY                             
         CLI   0(R1),X'FF'         END OF TABLE - KEEP THE LAST ONE             
         BNE   HH09                                                             
         MVC   LSTWIDTH,SYSWIDTH                                                
*                                                                               
HH11     ICM   R1,15,ACNTR         DISPLACEMENT TO CENTER                       
         AR    R1,R6                                                            
         STCM  R1,15,ACNTR         ADDRESS OF FIELD                             
         ICM   R1,15,ARGHT         DISPLACEMENT TO RIGHT                        
         AR    R1,R6                                                            
         STCM  R1,15,ARGHT         ADDRESS OF FIELD                             
         TITLE 'HEAD - SUMMARY AND TRACE ROUTINES'                              
***********************************************************************         
* HEADLINE ROUTINES FOR SUMMARY AND TRACE                             *         
***********************************************************************         
         SPACE 1                                                                
HH13     CLI   RCSUBPRG,2          TRACE OPTION                                 
         BNE   HH15                                                             
         MVI   BOXYORN,NO          NO BOXES                                     
         B     HEADXIT                                                          
*                                                                               
HH15     CLI   RCSUBPRG,1          SUPERLEDGER RULES                            
         BNE   HH19                                                             
         MVC   WORK(40),=CL40'(APG EXTRACT RULES)'                              
         GOTO1 CENTER,DMCB,WORK,40                                              
         ICM   R3,15,ACNTR         CENTER OF HEAD                               
         MVC   0(40,R3),WORK                                                    
         LA    RF,L'XP(R3)                                                      
         GOTO1 UNDERLIN,DMCB,(40,(R3)),(X'BF',0(RF))                            
         MVC   XHEAD7+1(102),SUPHEAD   HEADLINE FOR SUPERLEDGER RULES           
         MVC   BOXYORN,BOXOPT                                                   
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS(164),XSPACES                                             
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+32,C'C'                                                  
         MVI   BOXCOLS+64,C'C'                                                  
         MVI   BOXCOLS+74,C'C'                                                  
         MVI   BOXCOLS+83,C'C'                                                  
         MVI   BOXCOLS+103,C'R'                                                 
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
*&&US*&& MVI   BOXROWS+56,C'B'                                                  
*&&UK*&& MVI   BOXROWS+59,C'B'                                                  
         B     HEADXIT                                                          
         TITLE 'HEAD - HEADLINE TOP'                                            
***********************************************************************         
* HEADLINE TOP                                                        *         
***********************************************************************         
         SPACE 1                                                                
HH19     SR    R1,R1                                                            
         IC    R1,REPCODE          POSITION R2 TO STACK ITEM                    
         BCTR  R1,0                FOR THIS REPORT                              
         M     R0,WRSTACK                                                       
         L     R2,ARSTACK                                                       
         AR    R2,R1                                                            
         USING RSTACKD,R2                                                       
         ICM   R3,15,ACNTR         CENTRE TOP 3 LINES                           
         BZ    HEADXIT                                                          
*                                                                               
HH21     MVC   0(40,R3),SYSNAME                                                 
         LA    RF,L'XP(R3)                                                      
         GOTO1 UNDERLIN,DMCB,(40,(R3)),(X'BF',0(RF))                            
         MVC   L'XP*2(40,R3),RSNAME   XHEAD3                                    
         TITLE 'HEAD - LEFT HEAD ROW NAMES'                                     
***********************************************************************         
* LEFT HEAD ROW NAMES                                                 *         
***********************************************************************         
         SPACE 1                                                                
HH23     LA    R3,XHEAD4+1                                                      
         LA    RF,RSARNAME                                                      
         L     R5,ANAMES                                                        
         USING NAMED,R5                                                         
         LA    R0,10                                                            
*                                                                               
HH25     ICM   R7,15,0(RF)         LOOK AT ROWNAME SPEC                         
         BZ    HH35                NONE, SO TRY NEXT ROW                        
         CLI   3(R7),1             IS IT A HEADLINE                             
         BNE   HH35                                                             
         IC    R1,1(,R7)           YES - DIG OUT TITLE                          
         SHI   R1,6                                                             
         BM    HH29                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),5(R7)       MOVE IN PREFIX TITLE                         
*                                                                               
HH29     SR    R1,R1                                                            
         ICM   R1,1,RSBIGH         WIDEST TITLE                                 
         LA    R1,0(R1,R3)         GO DISTANCE OF LONGEST ROW PREFIX            
         BZ    *+8                                                              
         AHI   R1,1                                                             
         TM    2(R7),X'02'         ROW CODE?                                    
         BNO   *+10                NO                                           
         MVC   0(14,R1),NAMEEFF    YES, MOVE IN CODE                            
         SR    RE,RE                                                            
         ICM   RE,1,RSBIGC         WIDEST CODE                                  
         BZ    *+8                                                              
         AHI   RE,1                                                             
         AR    R1,RE               DISPLACE BY WIDEST CODE                      
         TM    2(R7),X'01'         ROWNAME USED?                                
         BNO   HH31                NO                                           
         CLI   NAMENAME,C'='       DON'T PRINT ACCOUNTS WITH "="                
         BE    HH31                                                             
         OI    NAMENAME+6,X'40'                                                 
         CLC   NAMENAME(7),=C'FILLER '  DON'T PRINT ACCTS CALLED FILLER         
         BE    HH31                                                             
         MVC   0(36,R1),NAMENAME   AND MOVE IN THE NAME                         
*                                                                               
HH31     LA    R3,L'XP(R3)         BUMP TO NEXT HEADLINE                        
*                                                                               
HH35     LA    RF,4(RF)                                                         
         LA    R5,NAMELNQ(R5)                                                   
         BCT   R0,HH25                                                          
*                                                                               
         TITLE 'HEAD - RIGHT AND BOTTOM ROUTINES'                               
***********************************************************************         
* HEADLINE RIGHT AND BOTTOM ROUTINES                                  *         
***********************************************************************         
         SPACE 1                                                                
         ICM   R3,15,ARGHT         XHEAD1 - RIGHT HAND LINES                    
         MVC   0(L'LHEAD1,R3),LHEAD1                                            
         MVC   10(2,R3),QPROG                                                   
         CLC   QSRTAREA(2),=C'P='                                               
         BNE   *+10                                                             
         MVC   10(2,R3),QSRTAREA+2                                              
         EDIT  (1,REPCODE),(2,13(R3)),ALIGN=LEFT                                
         CLI   RSCODE,C' '                                                      
         BNH   *+10                                                             
         MVC   13(2,R3),RSCODE                                                  
         EDIT  (1,COPYNO),(2,24(R3))                                            
         EDIT  (2,PAGE),(4,34(R3)),ALIGN=LEFT                                   
*                                                                               
         LA    R3,L'XP(R3)         XHEAD2 - MODULE ACXXXXX                      
         CLI   PRTMOD,YES                                                       
         BNE   *+16                                                             
         MVC   0(6,R3),=C'MODULE'                                               
         MVC   8(8,R3),MODULE                                                   
*                                                                               
         LA    R3,L'XP(R3)         XHEAD3 - ALLOCATION METHOD                   
         MVC   0(30,R3),MTHDNME                                                 
*                                                                               
         LA    R3,L'XP(R3)         XHEAD4 - PERIOD                              
         TM    RSOPT2,RSNOPERD     SUPPRESS?                                    
         BO    *+10                                                             
         MVC   0(L'PEREXP,R3),PEREXP                                            
*                                                                               
         LA    R3,L'XP(R3)         XHEAD5 - OVERHEAD RATE                       
         CP    FOHR,=P'0'                                                       
         BE    HH36                                                             
         MVC   0(9,R3),=C'OVERHEAD='                                            
         CURED (P3,FOHR),(7,9(R3)),2,ALIGN=LEFT,TRAIL=%                         
*                                                                               
HH36     LA    R3,L'XP(R3)         XHEAD6 - END DATE                            
         MVC   0(24,R3),RSRIGHT                                                 
         SHI   R3,(L'XP*2)                                                      
         TM    RSOPT1,RSENDDTE     OPT TO USE END DATE ONLY                     
         BNO   HH37                                                             
         CLI   16(R3),C' '         IF NO END DATE LEAVE IT ALONE                
         BE    HH39                                                             
         CLI   16(R3),C'-'                                                      
         BNE   *+14                                                             
         MVC   7(19,R3),18(R3)                                                  
         B     HH39                                                             
         MVC   7(15,R3),16(R3)     ELSE MOVE END TO START                       
         B     HH39                                                             
*                                                                               
HH37     DS    0H                                                               
*&&US                                                                           
         TM    RSOPT2,RSPEREND     PERIOD END OPTION                            
         BNO   HH39                                                             
         MVC   7(7,R3),=C'ENDING '                                              
         CLI   16(R3),C'-'                                                      
         BNE   *+14                                                             
         MVC   14(12,R3),18(R3)                                                 
         B     HH39                                                             
         CLI   16(R3),C' '                                                      
         BNE   *+14                                                             
         MVC   14(6,R3),7(R3)                                                   
         B     HH39                                                             
         MVC   14(10,R3),16(R3)                                                 
*&&                                                                             
         TITLE 'HEAD - SET UP COLUMN HEADINGS'                                  
***********************************************************************         
* SET UP COLUMN HEADINGS                                              *         
***********************************************************************         
         SPACE 1                                                                
HH39     LA    R1,L'RSHEAD1                                                     
         LA    R3,XHEAD10                                                       
*                                                                               
HH41     MVC   0(L'RSHEAD1,R3),RSHEAD1                                          
         MVC   L'XP(L'RSHEAD2,R3),RSHEAD2                                       
         MVC   2*L'XP(L'RSHEAD3,R3),RSHEAD3                                     
         A     R3,RSDISP                                                        
         LA    RF,RSARNAME                                                      
         LA    R0,10                                                            
         SR    R1,R1                                                            
*                                                                               
HH43     ICM   R7,15,0(RF)         LOOK FOR A ROWNAME SPEC                      
         BZ    HH45                                                             
         CLI   3(R7),3             THAT REFERS TO PRINT LINE                    
         BNE   HH45                                                             
         IC    R1,1(,R7)                                                        
         SHI   R1,6                FOUND ONE SO DIG OUT TITLE                   
         EX    R1,*+8                                                           
         B     HH47                                                             
         MVC   0(0,R3),5(R7)       AND DISPLAY IN LINE 10                       
*                                                                               
HH45     LA    RF,4(,RF)                                                        
         BCT   R0,HH43                                                          
         TITLE 'HEAD - INITIALIZE BOXES'                                        
***********************************************************************         
* INITIALIZE THE BOXES                                                *         
***********************************************************************         
         SPACE 1                                                                
HH47     MVC   BOXYORN,BOXOPT                                                   
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS(164),RSBOXES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+8,C'T'                                                   
         LA    R1,BOXROWS+12                                                    
         CLC   XHEAD12,XSPACES                                                  
         BNE   HH49                                                             
         LA    R1,BOXROWS+11                                                    
         CLC   XHEAD11,XSPACES                                                  
         BNE   HH49                                                             
         LA    R1,BOXROWS+10                                                    
*                                                                               
HH49     MVI   0(R1),C'M'                                                       
         SR    R1,R1                                                            
         IC    R1,MAXLINES         BOTTOM BOX LINE                              
*&&US*&& BCTR  R1,0                                                             
         LA    R1,BOXROWS(R1)      SET BOTTOM OF BOX                            
         MVI   0(R1),C'B'                                                       
*                                                                               
HEADXIT  XIT1                                                                   
         TITLE 'HEAD - CONSTANTS'                                               
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
LSTWIDTH DC    X'00'                                                            
*                                                                               
WIDTHTAB DS    0XL9                                                             
WIDTH    DS    XL1                 WIDTH OF THIS REPORT                         
ACNTR    DS    XL4                 CENTER FOR TITLE                             
ARGHT    DS    XL4                 RIGHT HAND SIDE                              
WIDTHLEN EQU   *-WIDTHTAB                                                       
*                                                                               
WIDTH132 DC    AL1(132)                                                         
         DC    AL4(XHEAD1+046-BIGPRNTD)                                         
         DC    AL4(XHEAD1+095-BIGPRNTD)                                         
*                                                                               
WIDTH164 DC    AL1(164)                                                         
         DC    AL4(XHEAD1+059-BIGPRNTD)                                         
         DC    AL4(XHEAD1+125-BIGPRNTD)                                         
         DC    X'FF'                                                            
*                                                                               
SUPHEAD  DC    C'  GIVING ACCOUNT DETAILS          GIVING CON'                  
         DC    C'TRA-ACCOUNT DETAILS   FILTERS   ACTION   REC'                  
         DC    C'EIVING ACCOUNT                              '                  
*                                                                               
ENTRYTAB DC    AL2(REPS-ACM204),AL2(AREPS-MAND)                                 
         DC    AL2(EDIT-ACM204),AL2(AEDIT-MAND)                                 
         DC    AL2(HEAD-ACM204),AL2(AHEAD-MAND)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACAPGGEND                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPM204 01/28/13'                                      
         END                                                                    
