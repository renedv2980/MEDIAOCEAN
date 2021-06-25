*          DATA SET REREP8C02  AT LEVEL 034 AS OF 05/11/05                      
*PHASE RE8C02A                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE CENTER                                                                 
*INCLUDE SCANNER                                                                
***********************************************************************         
*          REREP8C02 - SALESPERSON SUCCESS REPORT - RE8C              *         
*                                                                     *         
*---------------------------------------------------------------------*         
* MOD LOG:                                                            *         
* -------                                                             *         
*                                                                     *         
* ???????  (???) --- HISTORY LOST                                     *         
*                                                                     *         
* 28JAN91  (EFJ) --- UPDATED TO HANDLE BOP DEMOS VALIDATED BY DEMOVAL *         
*                     IN CONTRACT                                     *         
*                                                                     *         
* OCT23/92 (BU ) --- DATE FILTERING DONE IN FILCON, WHERE IT IS DONE  *         
*                    FOR ALL 80 SERIES REPORTS.                       *         
*                                                                     *         
* DEC11/95 (BG ) --- 2K CONTRACTS REGENALL REGENALL1                  *         
*                                                                     *         
* APR15/99 (BU ) --- 4K CONTRACTS REGENALL1 REGENALL1A                *         
*                                                                     *         
***********************************************************************         
         TITLE 'SALESPERSON SUCCESS REPORT - RE8C'                              
RE8C02   CSECT                                                                  
         ENTRY DEMTAB                                                           
         ENTRY MANTAB                                                           
         ENTRY SORTC                                                            
         PRINT NOGEN                                                            
         NMOD1 TEMPX-TEMPD,**RE8C                                               
         USING TEMPD,RC                                                         
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING RE8C02+4096,R8                                                   
         SPACE                                                                  
         CLI   MODE,PROCCONT                                                    
         BE    SAL30                                                            
         CLI   MODE,GRUPFRST                                                    
         BE    SAL20                                                            
         CLI   MODE,GRUPLAST                                                    
         BE    SAL70                                                            
         CLI   MODE,REQFRST                                                     
         BE    SAL10                                                            
         B     SALOUT                                                           
         EJECT                                                                  
* REQFRST INITIALIZATION PROCESSING                                             
*                                                                               
SAL10    RELOC FACTOR                                                           
         SPACE                                                                  
         L     RF,=V(CENTER)                                                    
         AR    RF,RE                                                            
         ST    RF,CENTER                                                        
         SPACE                                                                  
         L     RF,=V(BINSRCH)                                                   
         AR    RF,RE                                                            
         ST    RF,BINSRCH                                                       
         SPACE                                                                  
         L     RF,=V(SCANNER)                                                   
         AR    RF,RE                                                            
         ST    RF,SCANNER                                                       
         SPACE                                                                  
         L     R2,=V(DEMTAB)                                                    
         AR    R2,RE               RELOCATE A(DEMTAB)                           
         ST    R2,ADEMTAB                                                       
         SPACE 1                                                                
         L     R2,=V(MANTAB)                                                    
         AR    R2,RE                                                            
         ST    R2,AMANTAB                                                       
         SPACE                                                                  
         L     R2,=V(SORTC)                                                     
         AR    R2,RE                                                            
         ST    R2,ASORTC                                                        
         SPACE                                                                  
         LR    RE,RC               CLEAR WORKING STORAGE                        
         LA    RF,TEMPX-TEMPD                                                   
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SPACE                                                                  
SAL12    MVC   WORK(4),QSTART      BROADCAST YEAR/MONTH                         
         MVC   WORK+4(2),=C'01'    COMPLETE DAY                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,START)                                   
         GOTO1 GETBROAD,(R1),WORK,WORK2,GETDAY,ADDAY                            
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,(R1),(0,WORK2),(5,PERIOD)                                 
         MVI   PERIOD+8,DASH                                                    
         CLC   QEND,SPACES                                                      
         BNE   SAL14               2 MONTH ENTRIES                              
         MVC   END,START           ONLY 1 BROADCAST MONTH                       
         GOTO1 (RF),(R1),(0,WORK2+6),(5,PERIOD+9)                               
         B     SALOUT                                                           
         SPACE                                                                  
SAL14    MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,END)                                     
         GOTO1 GETBROAD,(R1),WORK,WORK2,GETDAY,ADDAY                            
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,(R1),(0,WORK2+6),(5,PERIOD+9)                             
         B     SALOUT                                                           
         SPACE 2                                                                
* GRUPFRST PROCESSING - SORTER INITIALIZATION                                   
*                                                                               
SAL20    DC    0H'0'                                                            
         CLI   SORTINIT,YES                                                     
         BE    SALOUT                                                           
         L     R2,ASORTC                                                        
         GOTO1 SORTER,DMCB,SORTFLD,RECTYPE,(40,(R2))                            
         MVI   SORTINIT,YES                                                     
         B     SALOUT                                                           
         EJECT                                                                  
* PROCCONT PROCESSING - DETERMINE IF CONTRACT QUALIFIES FOR                     
* REPORT AND BUILD A SORT RECORD IF IT DOES.                                    
*                                                                               
SAL30    TM    RCONCNTL,X'81'      NO DELETES OR COMPRESSED CONTRACTS           
         BNZ   SALOUT                                                           
         CLC   RCONDATE(3),=X'510701'                                           
         BL    SALOUT                                                           
         CLI   RCONTYPE,C'X'                                                    
         BE    SALOUT                                                           
         SPACE 2                                                                
* LOCATE EPL ELEMENT, IF ANY TEST THAT EPL INPUT DATE IS                        
* WITHIN REPORT PERIOD                                                          
*                                                                               
SAL32    LA    R4,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   SALOUT              ELEM NOT FOUND                               
         ST    R4,EPLADDR          SAVE A(ELEM)                                 
         SPACE 2                                                                
* FIND BOP ELEMENT IF ANY                                                       
*                                                                               
SAL34    LA    R4,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   SALOUT              ELEM NOT FOUND                               
         ST    R4,BOPADDR          FOUND-SAVE ADDR                              
         SPACE 2                                                                
* BUILD SORT RECORD                                                             
*                                                                               
SAL36    LA    RE,RECORD           CLEAR SORT REC AREA                          
         LR    R3,RE                                                            
         LA    RF,L'RECORD                                                      
         XCEF                                                                   
         USING SORTD,R3                                                         
         SPACE                                                                  
         MVC   SORTOFF,ROFFNAME    START WITH KEY FIELDS                        
         MVC   SORTMAN,RSALNAME                                                 
         MVC   SORTADV,RADVNAME                                                 
         MVC   SORTAGY,RAGYNAM1                                                 
         CLC   RCONPRD,SPACES      PRODUCT NAME INPUT OR CODE USED              
         BE    SAL38               INPUT                                        
         MVC   SORTPRD,RPRDNAME    CODE USED                                    
         B     SAL40                                                            
         SPACE                                                                  
SAL38    LA    R4,RCONREC          FIND PRODUCT NAME FROM CONTRACT              
         MVI   ELCODE,X'05'        EXPANSION ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   SAL40                                                            
         USING RCONEXEL,R4                                                      
         MVC   SORTPRD,RCONEXPR                                                 
         DROP  R4                                                               
         B     SAL40                                                            
         SPACE                                                                  
SAL40    MVC   SORTCON,RCONKCON    END OF SORT KEY                              
         MVC   SORTSTA,RCONKSTA                                                 
         MVC   SORTMKTN,RSTAMKT                                                 
         MVC   SORTCST,RCONDATE    CONTRACT START DATE                          
         MVC   SORTCTYP,RCONTYPE   CONTRACT TYPE                                
         SPACE                                                                  
SAL42    L     R4,BOPADDR          EXTRACT DATA FROM BOP ELEM                   
         USING RCONBPEL,R4                                                      
         MVC   SORTAWKS,RCONBAWK                                                
         MVC   SORTLEN,SPACES                                                   
         CLI   RCONBPCL+2,SPACE                                                 
         BE    SAL44               2 BYTE LENGTH EXPRESSION                     
         CLI   RCONBPCL+2,C'A'                                                  
         BL    SAL44               THIRD BYTE IS ASSUMNED DELIMITER             
         MVC   SORTLEN,RCONBPCL    MOVE ALL 3 BYTES                             
         B     SAL46                                                            
         SPACE                                                                  
SAL44    MVC   SORTLEN+1(2),RCONBPCL                                            
         B     SAL46                                                            
         SPACE                                                                  
SAL46    DS    0H                                                               
         CLI   RCONBPDM,X'FF'      VALIDATED BY DEMOVAL?                        
         BNE   SAL46A                                                           
*                                                                               
* OPEN CTFILE FOR DEMOCON                                                       
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X'                      
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'R'                                                    
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         DROP  RF                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'RCONBPDM-1),RCONBPDM+1                                    
*                                                                               
         CLI   WORK+1,C'T'                                                      
         BNE   *+8                                                              
         MVI   WORK+1,C'I'                                                      
*                                                                               
         GOTO1 ADEMOCON,DMCB,(1,WORK),(9,SORTDEMO),(0,DBLOCK)                   
         B     SAL46X                                                           
*                                                                               
SAL46A   XC    WORK,WORK           EXTRACT FIRST DEMO                           
         XC    WORK2,WORK2                                                      
         LA    RE,L'WORK2          BUILD PSEUDO FIELD HEADER FOR                
         STC   RE,WORK2            SCANNER.                                     
         LA    RF,RCONBPDM                                                      
         LA    R0,L'RCONBPDM       FIND LENGTH OF NON-SPACE DATA                
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         CLI   0(RF),SPACE                                                      
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,WORK2+5          STORE INPUT LENGTH                           
         MVC   WORK2+8(L'RCONBPDM),RCONBPDM                                     
         GOTO1 SCANNER,DMCB,WORK2,(2,WORK)                                      
         SPACE                                                                  
         CLI   DMCB+4,0                                                         
         BE    *+10                                                             
         MVC   SORTDEMO,WORK+12                                                 
SAL46X   OC    SORTDEMO,SPACES                                                  
         B     SAL48                                                            
         DROP  R4                                                               
         SPACE 2                                                                
* DETERMINE IF ITS A WIN OR LOSS AND FIND WIN DOLLARS                           
*                                                                               
SAL48    LA    R4,RCONREC                                                       
         MVI   ELCODE,X'03'        LOOK FOR A BUCKET ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   SAL50                                                            
         SR    R5,R5               CLEAR ACCUMULATOR                            
         USING RCONBKEL,R4                                                      
         MVC   FULL,RCONBKAM                                                    
         A     R5,FULL             UPDATE ACCUMULATOR                           
         BAS   RE,NEXTEL           TRY FOR ANOTHER ELEMENT                      
         BE    *-14                FOUND ONE                                    
         STCM  R5,15,SORTORD       ALL DONE-STORE RESULT                        
         DROP  R4                                                               
*                                                                               
SAL50    L     R4,EPLADDR                                                       
         USING RCONSPEL,R4                                                      
         OC    RCONSPAM,RCONSPAM   TEST THE STATION'S EPL AMOUNT                
         BZ    SAL51                                                            
         OI    SORTOPP,WIN         NOT ZERO - MARK A WIN                        
         ICM   RE,15,RCONSPAM                                                   
         MH    RE,=H'100'                                                       
         STCM  RE,15,SORTORD                                                    
         B     SAL52                                                            
*                                                                               
SAL51    OC    SORTORD,SORTORD     EPL ZERO - TEST ORDERED DOLLARS              
         BZ    *+12                                                             
         OI    SORTOPP,WIN                    NOT ZERO - WIN                    
         B     SAL52                                                            
         OI    SORTOPP,LOSS                   ZERO - LOSS                       
         DROP  R4                                                               
         SPACE                                                                  
SAL52    LA    R0,SORTEPL-SORTREC  COMPUTE REC LEN BEFORE EPL                   
         L     R4,EPLADDR                                                       
         ZIC   R1,1(R4)            R1 HAS EPL ELEM LENGTH                       
         AR    R0,R1               REC LEN W EPL                                
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE ELEM TO SORT RECORD                     
         B     SAL54                                                            
         MVC   SORTEPL(0),0(R4)                                                 
         SPACE                                                                  
SAL54    EQU   *                   MOVE ANY EPL COMMENT ELS TO RECD             
         STH   R0,SORTRLEN         SAVE REC LEN                                 
         LA    R4,RCONREC          LOOK FOR EPL COMMENTS                        
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   SAL56               NONE FOUND-WRITE TO SORTER                   
         SPACE                                                                  
SAL55    LA    RF,RECORD           FOUND-POINT RF TO END OF SORT RECORD         
         AH    RF,SORTRLEN                                                      
         ZIC   R1,1(R4)            EL LENGTH                                    
         LR    R0,R1               SAVE THE LENGTH                              
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE COMMENTS TO EOR                         
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
         AH    R0,SORTRLEN         UPDATE RECORD LENGTH                         
         STH   R0,SORTRLEN                                                      
         BAS   RE,NEXTEL           TRY FOR ANOTHER ELEMENT                      
         BE    SAL55               FOUND ONE                                    
         SPACE                                                                  
SAL56    MVI   SORTINIT,NO         RECORDS HAVE BEEN PUT TO SORTER              
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         B     SALOUT                                                           
         EJECT                                                                  
* GRUPLAST PROCESSING - READ FROM SORT FILE AND WRITE REPORT                    
*                                                                               
SAL70    EQU   *                                                                
         CLI   SORTINIT,YES                                                     
         BNE   SAL72                                                            
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     SALOUT                                                           
         SPACE                                                                  
SAL72    MVC   CONOFF,SPACES                                                    
         MVC   CONMAN,SPACES                                                    
         MVC   CONSTA,SPACES                                                    
         SPACE                                                                  
         XC    STABUCK,STABUCK                                                  
         XC    MANBUCK,MANBUCK                                                  
         XC    OFFBUCK,OFFBUCK                                                  
         SPACE                                                                  
         MVI   RCSUBPRG,0                                                       
         XC    DEMCOUNT,DEMCOUNT                                                
         SPACE                                                                  
GETSORT  GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BP    SAL75                                                            
         BAS   RE,STATOT                                                        
         BAS   RE,MANTOT                                                        
         MVI   RCSUBPRG,1                                                       
         BAS   RE,DEMSUM                                                        
         MVI   RCSUBPRG,2                                                       
         BAS   RE,OFFSUM                                                        
         MVI   RCSUBPRG,3                                                       
         BAS   RE,OFFSUM                                                        
         B     SALOUT                                                           
         SPACE                                                                  
* CONTROL OFFICE BREAK                                                          
*                                                                               
SAL75    CLC   CONOFF,SPACES                                                    
         BE    SAL76                                                            
         CLC   CONOFF,SORTOFF                                                   
         BE    SAL78                                                            
         SPACE                                                                  
         BAS   RE,STATOT                                                        
         BAS   RE,MANTOT                                                        
         BAS   RE,DEMSUM                                                        
         MVI   RCSUBPRG,2                                                       
         BAS   RE,OFFSUM                                                        
         MVI   RCSUBPRG,3                                                       
         BAS   RE,OFFSUM                                                        
         XC    OFFBUCK,OFFBUCK     RE-CLEAR BUCKETS FOR NEW OFFICE              
         SPACE 1                                                                
SAL76    MVI   MANCOUNT,0          CLEAR COUNTERS AND                           
         XC    DEMCOUNT,DEMCOUNT   SALESPERSON TABLE                            
         L     RE,AMANTAB                                                       
         LH    RF,=AL2(SORTC-MANTAB)                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SPACE 1                                                                
         MVC   CONOFF,SORTOFF                                                   
         MVC   WORK,SPACES         PREPARE OFC NAME FOR HEADLINE                
         MVC   WORK(L'CONOFF),CONOFF                                            
         MVC   WORK+30(6),=C'OFFICE'                                            
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   OFFNAM,WORK                                                      
         GOTO1 CENTER,(R1),OFFNAM,L'OFFNAM                                      
         MVI   FORCEHED,YES                                                     
         MVC   PAGE,=H'1'          RESET NUMBERING                              
         SPACE 2                                                                
* CONTROL SALESPERSON BREAK                                                     
*                                                                               
SAL78    CLC   CONMAN,SPACES                                                    
         BE    SAL80                                                            
         CLC   CONMAN,SORTMAN                                                   
         BE    SAL82                                                            
         SPACE                                                                  
         OC    STABUCK,STABUCK     AVOID EXTRA SUB-TOTALS LINES                 
         BZ    *+8                                                              
         BAS   RE,STATOT                                                        
         OC    MANBUCK,MANBUCK                                                  
         BZ    SAL80                                                            
         BAS   RE,MANTOT                                                        
         BAS   RE,DEMSUM                                                        
         SPACE                                                                  
SAL80    MVC   CONMAN,SORTMAN                                                   
         XC    DEMCOUNT,DEMCOUNT   RE-INITIALIZE DEMO COUNTER                   
         MVI   FORCEHED,YES                                                     
         SPACE                                                                  
* CONTROL STATION BREAK                                                         
*                                                                               
SAL82    CLC   CONSTA,SPACES                                                    
         BE    SAL84                                                            
         CLC   CONSTA,SORTSTA                                                   
         BE    SAL90                                                            
         SPACE                                                                  
         OC    STABUCK,STABUCK     AVOID DUPLICATION OF SUB-TOTAL LINES         
         BZ    SAL84                                                            
         BAS   RE,STATOT                                                        
         SPACE                                                                  
SAL84    MVC   CONSTA,SORTSTA      UPDATE STATION CONTROL                       
         MVC   STALN,SPACES                                                     
         MVC   STALN(5),SORTSTA                                                 
         CLI   SORTSTA+3,SPACE                                                  
         BE    SAL85                                                            
         MVC   STALN+5(1),SORTSTA+4                                             
         MVI   STALN+4,DASH                                                     
         MVI   STALN+6,C'M'                                                     
         B     SAL86                                                            
         SPACE                                                                  
SAL85    MVI   STALN+3,DASH                                                     
         MVI   STALN+5,C'M'                                                     
         SPACE                                                                  
SAL86    MVC   STALN+10(L'SORTMKTN),SORTMKTN                                    
         GOTO1 SQUASHER,DMCB,STALN,L'STALN                                      
         L     R1,DMCB+4           INPUT STRING LENGTH                          
         MVI   SPACING,1                                                        
         BAS   RE,SKIPLIN                                                       
         MVC   P+1(L'STALN),STALN                                               
         MVI   PSECOND+1,DASH                                                   
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PSECOND+2(0),PSECOND+1                                           
         MVI   BYTE,5              BLOCKING OF LINES                            
         BAS   RE,PRT                                                           
         BAS   RE,SKIPLIN                                                       
         B     SAL90                                                            
         SPACE 2                                                                
* FORMAT PRINT LINE AND UPDATE STATION BUCKETS                                  
*                                                                               
SAL90    EQU   *                                                                
         MVC   P+1(20),SORTADV                                                  
         MVC   PSECOND+1(20),SORTAGY                                            
         MVC   P+22(20),SORTPRD                                                 
         LA    R2,PSECOND+22                                                    
         GOTO1 DATCON,DMCB,(3,SORTCST),(5,(R2))                                 
         LA    R2,PSECOND+33                                                    
         EDIT  (B1,SORTAWKS),(2,(R2))                                           
         MVC   PSECOND+35(3),=C'WKS'                                            
         MVC   PSECOND+39(3),SORTLEN                                            
         MVC   P+44(9),SORTDEMO                                                 
         MVO   DUB(5),SORTCON      CONTRACT NUMBER                              
         OI    DUB+4,X'0F'                                                      
         EDIT  (P5,DUB),(7,P+54)                                                
         SPACE                                                                  
         CLI   SORTCTYP,C'N'       FLAG TYPE N CONTRACT                         
         BNE   *+10                                                             
         MVC   PSECOND+52(10),=C'(*NET OP*)'                                    
         SPACE 1                                                                
         TM    SORTOPP,WIN                                                      
         BO    SAL96               ITS A WIN                                    
         MVI   P+79,ASTER          ITS A LOSS                                   
         B     SAL98                                                            
         SPACE                                                                  
SAL96    MVI   P+65,ASTER                                                       
         ICM   R6,15,SORTORD       ROUND WIN DOLLARS TO                         
         SR    R7,R7               NEAREST DOLLAR                               
         SRDA  R6,31                                                            
         D     R6,=F'100'                                                       
         LTR   R7,R7                                                            
         BM    *+8                                                              
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         LR    R6,R7               SAVE RESULT IN R6                            
         CLI   QOPTION1,N                                                       
         BE    SAL98                                                            
         EDIT  (R7),(7,P+68)                                                    
         SPACE                                                                  
SAL98    EQU   *                                                                
         LA    R4,SORTEPL                                                       
         USING RCONSPEL,R4                                                      
         CLC   RCONSPAM(3),=C'CAN' TEST FOR CANCELLED CONTRACT                  
         BNE   SAL99               NO                                           
         MVI   P+79,SPACE                                                       
         MVC   P+67(9),=C'CANCELLED'                                            
         MVI   BYTE,2              OTHERWISE, PRINT LINES AND SKIP              
         BAS   RE,PRT              OVER ALL SUB-TOTALS                          
         B     SAL105                                                           
         DROP  R4                                                               
         SPACE                                                                  
SAL99    EQU   *                                                                
         LA    RE,STABUCK                                                       
         CLI   SORTCTYP,C'N'                                                    
         BNE   *+8                                                              
         LA    RE,12(RE)           BUMP AHEAD TO TYPE 'N' BUCKETS               
         TM    SORTOPP,WIN                                                      
         BO    SAL100                                                           
         L     R1,4(RE)                                                         
         LA    R1,1(R1)            INCREMENT LOSS COUNTER                       
         ST    R1,4(RE)                                                         
         B     SAL101                                                           
         SPACE                                                                  
SAL100   L     R1,0(RE)            INCREMENT WINS COUNTER                       
         LA    R1,1(R1)                                                         
         ST    R1,0(RE)                                                         
         A     R7,8(RE)            UPDATE DOLLARS                               
         ST    R7,8(RE)                                                         
         SPACE 2                                                                
* UPDATE SALESPERSON DEMO TABLE                                                 
*                                                                               
SAL101   CLI   SORTCTYP,C'N'                                                    
         BE    SAL105                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(L'SORTDEMO),SORTDEMO                                        
         L     R2,ADEMTAB                                                       
         L     R5,DEMCOUNT                                                      
         L     R7,MAXDEMEN                                                      
         GOTO1 BINSRCH,DMCB,(1,WORK),(R2),(R5),L'DEMENT,(0,L'DEMKEY),  X        
               (R7)                                                             
         L     R5,DMCB                                                          
         USING DEMD,R5                                                          
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DEMCOUNT,DMCB+8                                                  
         TM    SORTOPP,LOSS                                                     
         BO    SAL102                                                           
         L     RE,DEMMWIN                                                       
         LA    RE,1(RE)                                                         
         ST    RE,DEMMWIN                                                       
         A     R6,DEMMDOLL                                                      
         ST    R6,DEMMDOLL                                                      
         B     SAL105                                                           
         SPACE                                                                  
SAL102   L     RE,DEMMLOSS                                                      
         LA    RE,1(RE)                                                         
         ST    RE,DEMMLOSS                                                      
         B     SAL105                                                           
         DROP  R5                                                               
         SPACE 2                                                                
* FORMAT REMAINDER OF PRINT LINE - COMPETITION - EPL COMMENTS                   
*                                                                               
SAL105   CLI   QOPTION1,N          FOR THIS OPTION, SKIP FURTHER                
         BE    GETSORT             FORMATTING                                   
         LA    R4,SORTEPL          POINT TO EPL ELEMENT                         
         USING RCONSPEL,R4                                                      
         CLI   RCONSPNU,1          NO MINI ELEMENTS                             
         BH    SAL108                                                           
         CLC   RCONSPAM(3),=C'CAN' SKIP RIGHT TO COMMENTS FOR                   
         BE    SAL130              CANCELLED CONTRACTS                          
         TM    RCONSPES,X'80'      DID STATION WIN IT ALL                       
         BO    *+14                YES                                          
         MVC   P+83(13),=C'NOT AVAILABLE'  NO COMPETING STATIONS INPUT          
         B     *+10                                                             
         MVC   P+83(21),=C'STATION RECEIVED 100%'                               
         MVI   BYTE,2              BLOCK TWO PRINT LINES TOGETHER               
         BAS   RE,PRT                                                           
         B     SAL130              LOOK FOR EPL COMMENTS                        
         SPACE                                                                  
SAL108   ZIC   R0,RCONSPNU         NUMBER OF MINI ELS                           
         BCTR  R0,0                LESS 1 FOR CONTRACT'S STATION                
         LA    R5,COMP             POINTER TO AREA FOR OUTPUT                   
         LR    RF,R0               CALCULATE NUMBER OF PRINT LINES              
         SR    RE,RE               NEEDED FOR COMPETING STATIONS                
         LA    RF,2(RF)            AT 3 STATIONS PER LINE.                      
         D     RE,=F'3'            RF CONTAINS RESULT                           
         STH   RF,HALF             SAVE IT                                      
         LA    R2,RCONSPST+9       POINT R2 AT MINI ELEMENTS                    
         SPACE                                                                  
SAL110   CH    R0,=H'3'            TEST FOR 3 OR MORE MINI ELS                  
         BH    *+10                                                             
         LR    R1,R0               LESS SO USE EXACT NUMBER AS COUNTER          
         B     *+8                                                              
         LA    R1,3                3 IS MAXIMUM IN COUNTER                      
         SPACE                                                                  
         MVC   0(L'COMP,R5),SPACES CLEAR OUTPUT LINE                            
         SPACE                                                                  
SAL112   CLI   3(R2),SPACE         3 OR 4 CALL LETTERS                          
         BE    SAL114              3 LETTERS                                    
         CLC   0(5,R2),=C'OTHER'                                                
         BE    SAL113                                                           
         MVC   0(4,R5),0(R2)       MOVE 4 6ETTERS TO OUTPUT                     
         LA    R5,4(R5)            BUMP POINTER ALONG                           
         MVI   0(R5),DASH                                                       
         CLI   4(R2),C'A'          APPEND THE BAND DESIGNATION                  
         BE    *+14                                                             
         MVC   1(2,R5),=C'FM'                                                   
         B     *+10                                                             
         MVC   1(2,R5),=C'AM'                                                   
         B     SAL116                                                           
         SPACE                                                                  
SAL113   MVC   0(6,R5),=C'OTHERS'                                               
         LA    R5,4(R5)                                                         
         B     SAL116                                                           
         SPACE                                                                  
SAL114   MVC   0(3,R5),0(R2)       MOVE 3 LETTERS                               
         LA    R5,3(R5)                                                         
         MVI   0(R5),DASH          APPEND BAND                                  
         CLI   4(R2),C'A'                                                       
         BE    *+14                                                             
         MVC   1(2,R5),=C'FM'                                                   
         B     *+10                                                             
         MVC   1(2,R5),=C'AM'                                                   
         LA    R5,1(R5)            FOR 3 LETTERS-MOVE POINTER 1 MORE            
         SPACE                                                                  
SAL116   LA    R5,4(R5)            POSITION R5 FOR NEXT STATION                 
         LA    R2,9(R2)            POINT R2 AT NEXT MINI ELEMENT                
         BCTR  R0,0                DECREMENT COUNT OF MINI ELEMENTS             
         BCT   R1,SAL112           FORMAT NEXT STATION                          
         BCTR  R5,0                BACK UP POINTER ONE BYTE                     
         LTR   R0,R0               LOOP BACK TO LINE FORMAT IF                  
         BP    SAL110              ANY COMPETING STATIONS ARE LEFT.             
         LR    R1,RF               MOVE NUM LINES TO R1                         
         LA    R5,COMP             RESTORE POINTER TO START OF OUTPUT           
         SPACE 2                                                                
* CONTROL THE MOVEMENT OF LINES OF COMPETING STATIONS TO PRINT LINES            
*                                                                               
SAL118   LTR   R1,R1               TEST FOR END OF LINES                        
         BZ    SAL130              GO DO THE COMMENTS                           
         SPACE                                                                  
         CH    R1,=H'4'                                                         
         BL    *+16                LINES TO MOVE TO PRINT AREA.                 
         SH    R1,=H'4'                                                         
         LA    R0,4                IT MAY NOT BE G.T. 4                         
         B     SAL120                                                           
         LR    R0,R1                                                            
         SR    R1,R1               ZERO OUT LINES LEFT TO PRINT                 
         SPACE                                                                  
SAL120   CLC   P,SPACES            ARE THE 2 PRINT LINES W CONTRACT             
         BE    SAL122              THERE-NO                                     
         SPACE                                                                  
         MVI   BYTE,2              YES-AND THEY MUST BE BLOCKED                 
         CH    R0,=H'2'                                                         
         BNH   *+8                                                              
         STC   R0,BYTE             IF 3 OR 4 LINES OF COMPETING STAS,           
         LA    R2,P                USE THAT AS SIZE OF BLOCK.                   
         MVC   83(L'COMP,R2),0(R5)                                              
         LA    R5,L'COMP(R5)       POINT TO NEXT COMPETING STAS                 
         LA    R2,132(R2)          POINT TO NEXT PRT LINE                       
         BCT   R0,*-14                                                          
         BAS   RE,PRT              PRINT ALL TOGETHER                           
         B     SAL124                                                           
         SPACE                                                                  
SAL122   MVI   BYTE,1                                                           
         MVC   P+83(L'COMP),0(R5)                                               
         BAS   RE,PRT              PRINT 1 LINE AT A TIME                       
         LA    R5,L'COMP(R5)                                                    
         BCT   R0,*-14                                                          
         B     SAL124                                                           
         SPACE                                                                  
SAL124   B     SAL118                                                           
         SPACE 2                                                                
* HANDLE PRINTING OF EPL COMMENTS                                               
*                                                                               
SAL130   ZIC   R1,RCONSPLN         FIND LENGTH OF FIXED FIELDS AND              
         LA    R0,SORTEPL-SORTREC  EPL AND SEE IF THERE ARE ANY                 
         AR    R0,R1               COMMENTS.                                    
         CH    R0,SORTRLEN                                                      
         BE    SAL140              NONE                                         
         MVI   BYTE,1                                                           
         LA    R2,P                                                             
         AR    R4,R1               POINT R4 TO NEXT EL                          
         MVI   SPACING,1                                                        
         BAS   RE,SKIPLIN          SKIP A LINE BEFORE EPL COMMENTS              
         DROP  R4                                                               
         USING RCONSMEL,R4                                                      
         SPACE                                                                  
SAL132   ZIC   R1,RCONSMLN         EL LENGTH                                    
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R2),RCONSMNT   COMMENT TO PRT LINE                          
         BAS   RE,PRT                                                           
         LA    R1,3(R1)            RESTORE EL LENGTH                            
         AR    R0,R1                                                            
         CH    R0,SORTRLEN         EOR REACHED                                  
         BE    SAL140              YES                                          
         AR    R4,R1               NO-POINT R4 TO NEXT EL                       
         B     SAL132                                                           
         DROP  R4                                                               
         SPACE                                                                  
SAL140   MVI   SPACING,1                                                        
         BAS   RE,SKIPLIN                                                       
         B     GETSORT                                                          
         SPACE 2                                                                
SALOUT   XMOD1 1                                                                
         SPACE 2                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR STATION SUB-TOTALS                                            
*                                                                               
STATOT   NTR1                                                                   
         MVC   P+22(10),=C'TOTALS FOR'                                          
         CLI   CONSTA+3,SPACE                                                   
         BE    *+14                                                             
         MVC   P+33(7),STALN                                                    
         B     *+10                                                             
         MVC   P+33(6),STALN                                                    
         SPACE                                                                  
         GOTO1 SUBLIN,DMCB,STABUCK                                              
         MVI   BYTE,1                                                           
         BAS   RE,PRT                                                           
         MVI   SPACING,1                                                        
         BAS   RE,SKIPLIN                                                       
         SPACE                                                                  
         OC    STANWIN(8),STANWIN  ANY TYPE N RESOLUTIONS                       
         BZ    STATOT2             NO                                           
         MVC   P+22(10),=C'TOTALS FOR'                                          
         MVC   P+33(21),=C'NETWORK OPPORTUNITIES'                               
         GOTO1 SUBLIN,DMCB,STABUCK+12                                           
         MVI   BYTE,1                                                           
         BAS   RE,PRT                                                           
         SPACE                                                                  
STATOT2  DC    0H'0'                                                            
         LA    R0,6                BUCKET COUNTER                               
         LA    RE,STABUCK                                                       
         LA    R1,MANBUCK                                                       
         L     R2,0(RE)            UPDATE BUCKET                                
         A     R2,0(R1)                                                         
         ST    R2,0(R1)                                                         
         LA    RE,4(RE)            BUMP BUCKET POINTERS                         
         LA    R1,4(R1)                                                         
         BCT   R0,*-20                                                          
         XC    STABUCK,STABUCK     ZERO OUT BUCKETS                             
         B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE FOR SALESPERSON BUCKETS AND UPDATE OF OFFICE SALESPERSON          
* TABLE                                                                         
*                                                                               
MANTOT   NTR1                                                                   
         MVI   SPACING,2                                                        
         BAS   RE,SKIPLIN                                                       
         MVC   P+22(10),=C'TOTALS FOR'                                          
         MVC   P+33(L'CONMAN),CONMAN                                            
         GOTO1 SUBLIN,DMCB,MANBUCK                                              
         MVI   BYTE,1                                                           
         BAS   RE,PRT                                                           
         OC    MANNWIN(8),MANNWIN  ANY TYPE N RESOLUTIONS                       
         BZ    MANTOT1             NO-SKIP TOTALS LINE                          
         MVI   SPACING,1                                                        
         BAS   RE,SKIPLIN                                                       
         MVC   P+22(10),=C'TOTALS FOR'                                          
         MVC   P+33(21),=C'NETWORK OPPORTUNITIES'                               
         GOTO1 SUBLIN,DMCB,MANBUCK+12                                           
         MVI   BYTE,1                                                           
         BAS   RE,PRT                                                           
         SPACE                                                                  
MANTOT1  ZIC   R0,MANCOUNT         COUNT OF SALESPERSON ENTRIES                 
         LR    R2,R0                                                            
         LA    R1,L'MANTENT                                                     
         MR    R0,R0                                                            
         L     RE,AMANTAB          POINT RE AT NEXT AVAILABLE                   
         AR    RE,R1               ENTRY                                        
         USING MANTABD,RE                                                       
         MVC   MANTNAME,CONMAN                                                  
         MVC   MANTWIN(24),MANBUCK                                              
         LA    R2,1(R2)                                                         
         STC   R2,MANCOUNT         UPDATE ENTRY COUNT                           
         DROP  RE                                                               
         SPACE                                                                  
MANTOT2  DC    0H'0'                                                            
         LA    R0,6                                                             
         LA    RE,MANBUCK          ADD SALESPERSON BUCKETS INTO                 
         LA    R1,OFFBUCK          OFFICE TOTALS                                
         L     R2,0(RE)                                                         
         A     R2,0(R1)                                                         
         ST    R2,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,*-20                                                          
         B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO FORMAT A LINE OF SUB-TOTALS                                    
*              P1=A(BUCKETS)                                                    
*                                                                               
SUBLIN   ST    RE,FULL                                                          
         L     R6,0(R1)            R6 POINTS TO BUCKETS                         
         EDIT  (B4,(R6)),(4,P+62),ZERO=NOBLANK                                  
         L     R2,8(R6)            WIN DOLLARS                                  
         EDIT  (R2),(7,P+68)                                                    
         LM    RE,RF,0(R6)         WINS IN RE, LOSSES IN RF                     
         EDIT  (RF),(4,P+76),ZERO=NOBLANK                                       
         AR    RF,RE               TOTAL OPPORTUNITIES                          
         EDIT  (RF),(4,P+95),ZERO=NOBLANK                                       
         LR    R0,RF               SAVE RESOLVED IN R0                          
         MH    RE,=H'100'          WINS X 100 FOR PERCENTAGE                    
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         LTR   R0,R0               DEFEND AGAINST ZERO DIVISOR                  
         BP    *+12                                                             
         MVI   P+103,C'0'                                                       
         B     SUBLIN2                                                          
         SPACE                                                                  
         DR    RE,R0                                                            
         AH    RF,=H'1'            ROUND PCT                                    
         SRA   RF,1                                                             
         EDIT  (RF),(3,P+101),ZERO=NOBLANK                                      
         SPACE                                                                  
SUBLIN2  MVC   P+83(12),=C'(TOTAL OPS ='                                        
         MVC   P+104(6),=C'% WON)'                                              
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO PRODUCE SALESPERSON SUMMARIES                                      
*                                                                               
OFFSUM   NTR1                                                                   
         L     R5,AMANTAB                                                       
         ZIC   R2,MANCOUNT                                                      
         LTR   R2,R2                                                            
         BZ    OFFSUMX                                                          
         MVI   FORCEHED,YES                                                     
         USING MANTABD,R5                                                       
         SPACE                                                                  
OFFSUM1  DC    0H'0'                                                            
         CLI   RCSUBPRG,2                                                       
         BE    *+12                                                             
         LM    R6,R7,MANTNWIN                                                   
         B     *+8                                                              
         LM    R6,R7,MANTWIN                                                    
         STM   R6,R7,DUB                                                        
         OC    DUB,DUB             TEST FOR ZERO WINS AND LOSSES                
         BZ    OFFSUM3                                                          
         SPACE 1                                                                
         MVC   P+5(L'MANTNAME),MANTNAME                                         
         EDIT  (R6),(3,P+41),ZERO=NOBLANK                                       
         EDIT  (R7),(3,P+53),ZERO=NOBLANK                                       
         AR    R7,R6                                                            
         EDIT  (R7),(4,P+28)                                                    
         LR    R1,R7               RESOLVED IN R2                               
         SRDA  R6,32                                                            
         SLDA  R6,1                                                             
         M     R6,=F'100'          FOR PERCENTAGE WINS                          
         DR    R6,R1                                                            
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         EDIT  (R7),(3,P+66),ZERO=NOBLANK                                       
         MVI   P+69,PCT                                                         
         SPACE 1                                                                
         L     R7,MANTDOLL                                                      
         CLI   RCSUBPRG,2                                                       
         BE    *+8                                                              
         L     R7,MANTNDOL                                                      
         EDIT  (R7),(6,P+76)                                                    
         M     R6,=F'100'                                                       
         SLDA  R6,1                                                             
         L     RE,OFFDOLL                                                       
         CLI   RCSUBPRG,2                                                       
         BE    *+8                                                              
         L     RE,OFFNDOLL                                                      
         LTR   RE,RE               DEFEND AGAINST ZERO WIN DOLLARS              
         BZ    OFFSUM2                                                          
         DR    R6,RE                                                            
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         EDIT  (R7),(3,P+92)                                                    
         MVI   P+95,PCT                                                         
         SPACE                                                                  
OFFSUM2  DC    0H'0'                                                            
         MVI   BYTE,1                                                           
         BAS   RE,PRT                                                           
         MVI   SPACING,1                                                        
         BAS   RE,SKIPLIN                                                       
         SPACE 1                                                                
OFFSUM3  LA    R5,L'MANTENT(R5)                                                 
         BCT   R2,OFFSUM1                                                       
         SPACE 2                                                                
OFFSUM4  DC    0H'0'                                                            
         CLI   RCSUBPRG,3                                                       
         BE    OFFSUM5                                                          
         OC    OFFWIN(8),OFFWIN    SKIP PRINTING FOR NO RESOLUTIONS             
         BZ    OFFSUMX                                                          
         EDIT  (B4,OFFWIN),(4,P+40),ZERO=NOBLANK                                
         EDIT  (B4,OFFLOSS),(4,P+52),ZERO=NOBLANK                               
         LM    R2,R3,OFFWIN                                                     
         B     OFFSUM6                                                          
         SPACE 1                                                                
OFFSUM5  OC    OFFNWIN(8),OFFNWIN  FORGET PRINT FOR NO OUTCOMES                 
         BZ    OFFSUMX                                                          
         EDIT  (B4,OFFNWIN),(4,P+40),ZERO=NOBLANK                               
         EDIT  (B4,OFFNLOSS),(4,P+52),ZERO=NOBLANK                              
         LM    R2,R3,OFFNWIN                                                    
         SPACE 1                                                                
OFFSUM6  LA    R1,0(R2,R3)         PUT RESOLUTIONS IN R1                        
         EDIT  (R1),(4,P+28)                                                    
         XR    R3,R3                                                            
         SRDA  R2,31                                                            
         M     R2,=F'100'                                                       
         DR    R2,R1                                                            
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         EDIT  (R3),(4,P+66),TRAIL=C'%'                                         
         SPACE                                                                  
OFFSUM8  L     R2,OFFDOLL                                                       
         CLI   RCSUBPRG,2                                                       
         BE    *+8                                                              
         L     R2,OFFNDOLL                                                      
         EDIT  (R2),(7,P+75)                                                    
         MVC   P+5(13),=C'OFFICE TOTALS'                                        
         MVC   P+92(4),=C'100%'                                                 
         MVI   BYTE,1                                                           
         BAS   RE,PRT                                                           
         SPACE                                                                  
OFFSUMX  MVI   RCSUBPRG,0                                                       
         B     EXIT                                                             
         SPACE 2                                                                
* DEMO SUMMARY SUB-ROUTINE                                                      
*                                                                               
DEMSUM   NTR1                                                                   
         L     R5,ADEMTAB                                                       
         USING DEMD,R5                                                          
         L     R2,DEMCOUNT                                                      
         LTR   R2,R2                                                            
         BZ    DEMSUMX                                                          
         MVI   FORCEHED,YES                                                     
         MVI   RCSUBPRG,1                                                       
         SPACE                                                                  
DEMSUM1  MVC   P+14(L'DEMKEY),DEMKEY                                            
         LM    R6,R7,DEMMWIN                                                    
         EDIT  (R6),(3,P+41),ZERO=NOBLANK                                       
         EDIT  (R7),(3,P+54),ZERO=NOBLANK                                       
         LA    RE,0(R6,R7)                                                      
         EDIT  (RE),(4,P+27)                                                    
         SRDL  R6,32                                                            
         SLDA  R6,1                                                             
         M     R6,=F'100'                                                       
         DR    R6,RE                                                            
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         EDIT  (R7),(4,P+66),TRAIL=C'%'                                         
         SPACE                                                                  
         L     R6,DEMMDOLL                                                      
         EDIT  (R6),(6,P+76)                                                    
         SRDL  R6,32                                                            
         M     R6,=F'100'                                                       
         SLDA  R6,1                                                             
         OC    MANDOLL,MANDOLL     DEFEND AGAINST ZERO WIN DOLLARS              
         BZ    DEMSUM2                                                          
         D     R6,MANDOLL                                                       
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         EDIT  (R7),(4,P+92),TRAIL=C'%'                                         
         SPACE                                                                  
DEMSUM2  MVI   BYTE,1                                                           
         BAS   RE,PRT                                                           
         MVI   SPACING,1                                                        
         BAS   RE,SKIPLIN                                                       
         SPACE 1                                                                
DEMSUM3  LA    R5,L'DEMENT(R5)                                                  
         BCT   R2,DEMSUM1                                                       
         SPACE                                                                  
DEMSUM4  MVC   P+14(6),=C'TOTALS'                                               
         LM    R6,R7,MANWIN                                                     
         EDIT  (R6),(3,P+41),ZERO=NOBLANK                                       
         EDIT  (R7),(3,P+54),ZERO=NOBLANK                                       
         LA    RE,0(R6,R7)        RESOLVED                                      
         EDIT  (RE),(4,P+27)                                                    
         SRDL  R6,32                                                            
         M     R6,=F'100'                                                       
         SLDA  R6,1                                                             
         DR    R6,RE                                                            
         LA    R7,1(R7)                                                         
         SRA   R7,1                                                             
         EDIT  (R7),(4,P+66),TRAIL=C'%'                                         
         SPACE                                                                  
         EDIT  (B4,MANDOLL),(6,P+76)                                            
         MVC   P+92(4),=C'100%'                                                 
         BAS   RE,PRT                                                           
         SPACE                                                                  
DEMSUMX  MVI   RCSUBPRG,0                                                       
         XC    MANBUCK,MANBUCK                                                  
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* CONTROL LINE PRINTING                                                         
*                                                                               
PRT      NTR1                                                                   
         ZIC   R0,LINE                                                          
         ZIC   R1,BYTE             BLOCK OF LINE(S)                             
         AR    R0,R1               TEST FOR FIT ON PAGE                         
         CLM   R0,1,MAXLINES                                                    
         BNH   *+8                                                              
         MVI   FORCEHED,YES                                                     
         SPACE                                                                  
         CLI   FORCEHED,YES                                                     
         BNE   PRTLINE                                                          
         SPACE                                                                  
         MVC   HEAD3+42(28),OFFNAM                                              
         MVC   HEAD4+44(6),=C'PERIOD'                                           
         MVC   HEAD4+51(17),PERIOD                                              
         CLI   RCSUBPRG,1                                                       
         BH    *+16                                                             
         MVC   HEAD4+1(11),=C'SALESPERSON'                                      
         MVC   HEAD4+13(L'CONMAN),CONMAN                                        
         CLI   RCSUBPRG,1                                                       
         BNE   PRTLINE                                                          
         CLC   QSTATION,SPACES                                                  
         BNE   *+14                                                             
         MVC   HEAD4+88(12),=C'ALL STATIONS'                                    
         B     PRTLINE                                                          
         SPACE                                                                  
         MVC   HEAD4+88(7),=C'STATION'                                          
         MVC   HEAD4+96(4),QSTATION                                             
         CLI   QSTATION+3,SPACE                                                 
         BE    *+22                                                             
         MVI   HEAD4+100,DASH                                                   
         MVC   HEAD4+101(1),QSTATION+4                                          
         MVI   HEAD4+102,C'M'                                                   
         B     PRTLINE                                                          
         SPACE                                                                  
         MVI   HEAD4+99,DASH                                                    
         MVC   HEAD4+100(1),QSTATION+4                                          
         MVI   HEAD4+101,C'M'                                                   
         SPACE                                                                  
PRTLINE  MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
* ROUTINE TO CONTROL SKIPPING A LINE                                            
*                                                                               
SKIPLIN  NTR1                                                                   
         CLI   FORCEHED,YES                                                     
         BE    EXIT                                                             
         ZIC   R0,LINE                                                          
         ZIC   R1,SPACING                                                       
         AR    R0,R1                                                            
         CLM   R0,1,MAXLINES                                                    
         BH    EXIT                                                             
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
         DS    0D                                                               
PATCH    DC    XL30'0'                                                          
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* STORAGE FOR BETWEEN I/O DATA                                                  
*                                                                               
SORTFLD  DC    CL80'SORT FIELDS=(5,109,A),FORMAT=BI,WORK=1'                     
RECTYPE  DC    CL80'RECORD TYPE=V,LENGTH=1000'                                  
         SPACE                                                                  
ASORTC   DS    A                                                                
ADEMTAB  DS    A                                                                
AMANTAB  DS    A                                                                
BINSRCH  DS    A                                                                
CENTER   DS    A                                                                
SCANNER  DS    A                                                                
         SPACE                                                                  
ELCODE   DS    C                                                                
SORTINIT DC    C'N'                                                             
MANCOUNT DS    X                                                                
         SPACE                                                                  
START    DS    XL3                                                              
END      DS    XL3                                                              
PERIOD   DS    CL17                                                             
STALN    DS    CL30                                                             
MKT      DS    CL20                                                             
OFFNAM   DS    CL28                                                             
CONOFF   DS    CL20                                                             
CONMAN   DS    CL20                                                             
CONSTA   DS    CL5                                                              
         SPACE                                                                  
FACTOR   DS    A                                                                
DEMCOUNT DS    F                                                                
MAXDEMEN DC    F'100'                                                           
         SPACE                                                                  
         DS    0F                                                               
OFFBUCK  DS    0CL24                                                            
OFFWIN   DS    F                                                                
OFFLOSS  DS    F                                                                
OFFDOLL  DS    F                                                                
OFFNWIN  DS    F                                                                
OFFNLOSS DS    F                                                                
OFFNDOLL DS    F                                                                
         SPACE                                                                  
MANBUCK  DS    0CL24                                                            
MANWIN   DS    F                                                                
MANLOSS  DS    F                                                                
MANDOLL  DS    F                                                                
MANNWIN  DS    F                                                                
MANNLOSS DS    F                                                                
MANNDOLL DS    F                                                                
         SPACE                                                                  
STABUCK  DS    0CL24                                                            
STAWIN   DS    F                                                                
STALOSS  DS    F                                                                
STADOLL  DS    F                                                                
STANWIN  DS    F                                                                
STANLOSS DS    F                                                                
STANDOLL DS    F                                                                
         SPACE 2                                                                
*                                                                               
* DBLOCK                                                                        
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
DEMTAB   DS    0D                                                               
         DS    CL2400                                                           
MANTAB   DS    0D                                                               
         DS    CL4400                                                           
SORTC    DS    0D                                                               
         DC    41000X'00'                                                       
         SPACE 2                                                                
* DSECT TO COVER WORKING STORAGE                                                
*                                                                               
TEMPD    DSECT                                                                  
EPLADDR  DS    A                                                                
BOPADDR  DS    A                                                                
WORK2    DS    CL30                                                             
COMP     DS    24CL23                                                           
         DS    0D                                                               
RECORD   DS    CL1000                                                           
TEMPX    EQU   *                                                                
         SPACE 2                                                                
* DSECT TO COVER DEMO SUMMARY TABLE                                             
*                                                                               
DEMD     DSECT                                                                  
DEMENT   DS    0CL24                                                            
DEMKEY   DS    CL9                                                              
         DS    0F                                                               
DEMMTOT  DS    0CL12                                                            
DEMMWIN  DS    F                                                                
DEMMLOSS DS    F                                                                
DEMMDOLL DS    F                                                                
         SPACE 2                                                                
* DSECT TO COVER SALESPERSON TABLE ENTRIES                                      
*                                                                               
MANTABD  DSECT                                                                  
MANTENT  DS    0CL44                                                            
MANTNAME DS    CL20                SALESPERSON NAME                             
MANTWIN  DS    F                                                                
MANTLOSS DS    F                                                                
MANTDOLL DS    F                                                                
MANTNWIN DS    F                                                                
MANTNLOS DS    F                                                                
MANTNDOL DS    F                                                                
         SPACE 2                                                                
* DSECT TO COVER SORT RECORD                                                    
*                                                                               
SORTD    DSECT                                                                  
SORTREC  DS    0D                                                               
SORTRLEN DS    H                                                                
         DS    H                                                                
SORTOFF  DS    CL20                                                             
SORTMAN  DS    CL20                                                             
SORTSTA  DS    CL5                                                              
SORTADV  DS    CL20                                                             
SORTAGY  DS    CL20                                                             
SORTPRD  DS    CL20                                                             
SORTCON  DS    XL4                                                              
SORTFIXD DS    0C                  START OF FIXED DATA                          
SORTMKTN DS    CL20                STATION MARKET NAME                          
SORTOPP  DS    B                   PENDING/WIN/LOSS ETC.                        
SORTCST  DS    CL3                 CONTRACT START                               
SORTORD  DS    CL4                                                              
SORTCTYP DS    C                   CONTRACT TYPE                                
SORTAWKS DS    B                                                                
SORTLEN  DS    CL3                                                              
SORTDEMO DS    CL9                                                              
SORTEPL  DS    0C                  EPL AND COMMENT ELEMENTS START HERE          
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
SPACE    EQU   C' '                                                             
DASH     EQU   C'-'                                                             
ASTER    EQU   C'*'                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
WIN      EQU   X'04'                                                            
LOSS     EQU   X'08'                                                            
PCT      EQU   C'%'                                                             
N        EQU   C'N'                                                             
         SPACE 2                                                                
* WORKD AND FILE DSECTS                                                         
         PRINT OFF                                                              
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034REREP8C02 05/11/05'                                      
         END                                                                    
