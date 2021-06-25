*          DATA SET ACREPTJ02  AT LEVEL 031 AS OF 09/28/16                      
*PHASE ACTJ02A,*                                                                
*INCLUDE ACCDIV                                                                 
*INCLUDE GETLOGO                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PQPROF                                                                 
*INCLUDE ACRECTYP                                                               
ACTJ02   TITLE '- TIMESHEET TURNAROUND REPORT'                                  
ACTJ02   CSECT                                                                  
TABGN    DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**ACTJ**,R9                                                    
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACTJD,RC            RC=A(LOCAL W/S)                              
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         L     R7,ADBXAREA                                                      
         USING BOXD,R7                                                          
         MVC   BOXWIDTH,=F'198'                                                 
                                                                                
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         OC    MCREMPQK,MCREMPQK   IS THIS A SOON REQUEST                       
         BZ    *+10                                                             
         MVC   FCRDREQS,YES        READ REQUEST CARDS                           
         CLI   MODE,PROCRQST       IF PROCESS REQUEST MODE NEED TO              
         BE    RQSTRT              SET ACCOUNT FROM REQUEST CARD                
                                                                                
         CLI   MODE,PROCRCVR       PROCESS RECOVERY FILE RECORD                 
         BE    PROC                                                             
         CLI   MODE,RUNFRST        FIRST FOR RUN                                
         BE    FRST                                                             
         CLI   MODE,RUNLAST        LAST FOR RUN                                 
         BE    LAST                                                             
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* RUN FIRST ROUTINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
FRST     L     R2,=A(IO1)                                                       
         ST    R2,AIO1                                                          
         L     R2,=A(IO2)                                                       
         ST    R2,AIO2                                                          
         L     R2,=A(IO3)                                                       
         ST    R2,AIO3                                                          
                                                                                
         L     R0,ABXHOOK                                                       
         ST    R0,HEADHOOK                                                      
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)                                                         
                                                                                
         RELOC RELO                                                             
         LA    RE,ATYPES                                                        
FRST10   L     RF,0(RE)            RELOCATE A TYPES                             
         A     RF,RELO                                                          
         ST    RF,0(RE)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   FRST10                                                           
                                                                                
         XC    SRTSEQ,SRTSEQ       CLEAR                                        
         MVI   LOGOFRST,0                                                       
                                                                                
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   CTRY,MCCTRY                                                      
         DROP  RF                                                               
                                                                                
         USING CPYRECD,R2                                                       
         L     R2,AIO1                                                          
         USING CPYTABD,R3          R3=A(COMPANY TABLE)                          
         L     R3,ACPYTAB                                                       
         LA    R4,X'40'                                                         
                                                                                
         CLC   FCRDREQS,YES        FOR SOON REQUESTS FILTER ITEMS               
         BNE   FRST20              FOR THIS COMPANY ONLY                        
         ZIC   R4,RCCOMPFL                                                      
         MVC   CPYKEY,SPACES                                                    
         B     FRST30                                                           
                                                                                
FRST20   MVC   CPYKEY,SPACES                                                    
         LA    R4,1(R4)                                                         
FRST30   STC   R4,CPYKCPY                                                       
         CLI   CPYKCPY,X'FF'                                                    
         BE    FRST120                                                          
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,CPYRECD,CPYRECD                       
         BNE   FRST20                                                           
                                                                                
         LA    R1,CPYRECD+ACCORFST                                              
         SR    R0,R0                                                            
         USING CPYELD,R1                                                        
FRST40   CLI   CPYEL,0             TEST E-O-R                                   
         BE    FRST80                                                           
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BNE   FRST50                                                           
         MVC   CPYTCPY,CPYKCPY                                                  
         MVC   CPYTUID,CPYUID                                                   
         MVC   CPYTSTA1,CPYSTAT1                                                
         MVC   CPYTSTA2,CPYSTAT2                                                
         MVC   CPYTSTA3,CPYSTAT3                                                
         MVC   CPYTSTA4,CPYSTAT4                                                
         MVC   CPYTSTA8,CPYSTAT8                                                
         MVC   CPYTALPH,CPYALPHA                                                
         MVC   CPYTLOGO,CPYLOGO                                                 
         B     FRST70                                                           
                                                                                
         USING NAMELD,R1                                                        
FRST50   CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   FRST60                                                           
         SR    RE,RE                                                            
         ICM   RE,1,NAMLN                                                       
         SH    RE,=Y(NAMEREC+1-NAMELD)                                          
         MVC   CPYTNAME,SPACES                                                  
         MVC   CPYTNAME(0),NAMEREC                                              
         EX    RE,*-6                                                           
         B     FRST70                                                           
                                                                                
         USING ADRELD,R1                                                        
FRST60   CLI   ADREL,ADRELQ        TEST ADDRESS ELEMENT                         
         BNE   FRST70                                                           
         MVC   CPYTADDR,ADRADD1                                                 
         B     FRST70                                                           
                                                                                
FRST70   IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     FRST40                                                           
                                                                                
         USING PROFKD,R2                                                        
FRST80   DS    0H                                                               
         ZAP   PKMXHRS,=P'0'       SET AS DEFAULT                               
         LA    R2,WORK                                                          
         XC    PROFKEY,PROFKEY                                                  
         XC    PROFILES,PROFILES                                                
         MVI   PROFKSYS,C'A'                                                    
         MVC   PROFKPGM,=C'0TJ'                                                 
         MVC   PROFKAGY,ALPHAID                                                 
         GOTO1 GETPROF,DMCB,PROFKEY,PROFILES,DATAMGR                            
         OC    PROFILES,PROFILES   DID WE GET ANYTHING BACK?                    
         BZ    FRST90                                                           
         SR    R1,R1                                                            
         IC    R1,PRDHRS           CONVERT TO PACKED                            
         CVD   R1,DUB                                                           
         MP    DUB,=P'100'         BUMP IT TO THE ONES POSITION                 
         ZAP   PKMXHRS,DUB                                                      
*                                                                               
         USING CT5REC,R2                                                        
FRST90   L     R2,AIO1                                                          
         MVC   CPYTCTRY,CTRY       DEFAULT IS THE DDS COUNTRY CODE              
         XC    CT5KEY,CT5KEY       READ ACCESS RECORD TO ESTABLISH              
         MVI   CT5KTYP,CT5KTYPQ    AGENCY COUNTRY CODE                          
         MVC   CT5KALPH,CPYTALPH                                                
         GOTO1 DATAMGR,DMCB,DMREAD,CONFIL,CT5REC,CT5REC                         
         BNE   FRST110                                                          
         LA    R1,CT5DATA                                                       
         USING CTAGDD,R1                                                        
         SR    R0,R0                                                            
FRST100  CLI   CTAGDEL,0                                                        
         BE    FRST110                                                          
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    *+14                                                             
         IC    R0,CTAGDLEN                                                      
         AR    R1,R0                                                            
         B     FRST100                                                          
         CLI   CTAGDLEN,CTAGDCTY-CTAGDD                                         
         BL    FRST110                                                          
         MVC   CPYTCTRY,CTAGDCTY   SET AGENCY COUNTRY CODE                      
                                                                                
FRST110  LA    R3,CPYTABL(R3)      BUMP TABLE                                   
         CLC   FCRDREQS,YES        IF SOON REQUEST DON'T NEED TO BUILD          
         BE    FRST120             TABLE OF COMPANIES ON THIS ACCFILE           
         B     FRST20                                                           
                                                                                
FRST120  DS    0H                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
                                                                                
         GOTO1 ADDICTAT,DMCB,C'L   ',DCLIST,DSLIST                              
                                                                                
         LA    R5,XTABLEN+DETLNQ   GETMAIN AREA FOR SORTING ELEMENT             
         LHI   RE,SORTMAX+DETMAX   GROUPS WITHIN A PERSON CODE                  
         MR    R4,RE                                                            
         ST    R5,MAINLEN                                                       
         L     R0,MAINLEN                                                       
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,MAINBGN          START OF AREA FOR XSORT TABLE                
         MVI   0(R1),X'FF'         INITTIALIZE TABLE TO NO ENTRIES              
         A     R1,=A(XTABLEN*SORTMAX)                                           
         ST    R1,ADETTAB          START OF AREA FOR DETAIL TABLE               
*                                                                               
         USING BIND,RE                                                          
         L     RE,ADETTAB                                                       
         XC    BININ,BININ               CLEAR BIN TABLE                        
         LA    R1,DETLNQ                                                        
         STCM  R1,15,BINLEN              ENTRY LENGTH                           
         XC    BINDISP,BINDISP           DISPLACEMENT TO KEY                    
         LA    R1,DETKLNQ                                                       
         STC   R1,BINKEY+2               LENGTH OF KEY                          
         L     R1,=A(DETMAX)                                                    
         STCM  R1,15,BINMAX              MAXIMUM NUMBER OF ENTRIES              
         MVI   BINNUM,DETDBNUM           NUMBER OF BUCKETS                      
         MVI   BINFST,DETDBKT-DETD       DISP TO FIRST BUCKET                   
         DROP  RE                                                               
*                                                                               
*        L     RF,ADMASTC                                                       
*        USING MASTD,RF                                                         
*        ST    R1,MCUSRDMP         SAVE   NEW START                             
*        A     R1,MAINLEN          LENGTH OF PHASE + R1=(GETMAIN)               
*        ST    R1,MCUSRDMP+4                                                    
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
* PROCESS A RECOVERY FILE RECORD                                      *         
*---------------------------------------------------------------------*         
PROC     DS    0H                                                               
         L     R2,ADTRANS                                                       
         USING RCVRECD,R2             R2=A(RECOVERY HEADER)                     
         CLI   RCVPRGNO,RCVPCAPQ      TEST TIMESHEET PROGRAM                    
         BE    PROC1                                                            
         CLI   RCVPRGNO,RCVPBRAQ      TEST BRANDOCEAN PROGRAM                   
         BE    PROC1                                                            
         CLI   RCVPRGNO,RCVPCBLQ      OR CBILL PROGRAM                          
         BNE   EXIT                                                             
                                                                                
PROC1    CLI   RCVFILTY,RCVFAMST      TEST ACCMST RECORD                        
         BE    PROC2                                                            
         CLI   RCVFILTY,RCVFAACC      TEST ACCFIL RECORD                        
         BNE   EXIT                                                             
         GOTO1 VACCEMU,DMCB,EMUOLDN,,,RCVRECRD                                  
         ORG   *-2                                                              
         LR    R2,R1                  (EMU USES R2 FOR PLIST ADDRESS)           
         O     R2,=X'FF000000'                                                  
         BASR  RE,RF                                                            
         L     R2,ADTRANS                                                       
                                                                                
PROC2    DS    0H                                                               
         USING TIMRECD,R3             R3=A(TRANSACTION KEY)                     
         LA    R3,RCVRECRD            IF THERE ARE NO TIMELS (X'86')            
         LR    R4,R3                  DON'T NEED TO PROCESS THE RECORD          
         MVI   ELCODE,TIMELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
                                                                                
*MN                                                                             
         GOTO1 ACRECTYP,DMCB,(C'D',TIMRECD)                                     
         CLI   0(R1),ACRTTIM                                                    
         BNE   EXIT                                                             
*MN                                                                             
                                                                                
         TM    TIMRSTA,X'80'          IF RECORD IS MARKED DELETED               
         BO    EXIT                   DON'T PROCESS                             
         CLI   0(R3),TSWKTYPQ         SKIP PASSIVE POINTERS                     
         BE    EXIT                                                             
         CLI   0(R3),CAHKTYPQ         SKIP COST ALLOC HISTORY RECS              
         BE    EXIT                                                             
         CLI   0(R3),AUDKTYPQ         SKIP TIME AUDIT RECORDS AUDKTIME          
         BE    EXIT                                                             
                                                                                
         CLI   RCVRECTY,RCVRCPYQ      TEST COPY                                 
         BE    PROC5                                                            
         CLI   RCVRECTY,RCVRCHAQ      OR CHANGE                                 
         BE    PROC5                                                            
         CLI   RCVRECTY,RCVRADDQ      OR ADD                                    
         BNE   EXIT                                                             
         CLI   QOPT1,C'Y'          FORCE TO READ DETAILS                        
         BE    *+14                                                             
         CP    PKMXHRS,=P'0'                                                    
         BE    *+8                                                              
         BAS   RE,GETDET           GET THE DETAILS FOR THIS RECORD              
                                                                                
PROC5    DS    0H                                                               
         LA    RE,SREC                CLEAR SORTREC AREA                        
         LA    RF,SRLNQ                                                         
         XCEFL (RE),(RF)                                                        
         L     RE,AIO1                CLEAR IO AREA                             
         LH    RF,=Y(L'IO1)                                                     
         XCEFL (RE),(RF)                                                        
                                                                                
         CLC   FCRDREQS,YES           IF SOON REQUEST FILTER ON                 
         BNE   PROC15                 USER ID                                   
         USING MASTD,RF                                                         
         L     RF,ADMASTC                                                       
         CLC   MCORIGID,RCVUSRID                                                
         BNE   EXIT                                                             
                                                                                
PROC15   DS    0H                                                               
         L     R2,ADTRANS                                                       
         MVC   SKPRG,RCVPRGNO                                                   
                                                                                
         LA    R3,RCVRECRD            NEED TO READ '1R' LEDGER FOR              
         L     R1,AIO1                LEVEL LENGTHS TO BUILD SORT KEY           
         MVC   0(42,R1),SPACES                                                  
         MVC   0(3,R1),TIMKCPY                                                  
         MVC   SKTYPE,RCVRECTY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO1,AIO1                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1                GET HEIRARCHY ELEMENT                     
         AH    R4,DATADISP                                                      
         MVI   ELCODE,ACLELQ                                                    
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
PROC20   LA    R3,RCVRECRD                                                      
         LA    R5,L1R                 GO GET LEVEL BREAKOUT                     
         GOTO1 ACCDIV,DMCB,(R4),(R3),(R5)                                       
         LA    R3,RCVRECRD                                                      
         MVC   SKCMPY,TIMKCPY         SORTKEY COMPANY                           
         MVC   SKUSRID,RCVUSRID       SORTKEY USERID                            
                                                                                
         LA    R5,SKPRSPD             BUILD PERSON/PERIOD/OFFICE/               
         ZIC   R4,L1R4                DEPT/SUB DEPT PART OF SORTKEY             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),L1R4C          MOVE IN PERSON                            
         ZIC   R4,L1R4                BUMP BY LENGTH OF PERSON                  
         AR    R5,R4                                                            
         LA    R5,1(R5)                                                         
         MVC   0(L'TIMKPEDT,R5),TIMKPEDT    MOVE IN PERIOD                      
         LA    R5,L'TIMKPEDT(R5)      BUMP BY LENGTH OF PERIOD                  
         ZIC   R4,L1R1                LENGTH OF OFFICE                          
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),L1R1C          MOVE IN OFFICE                            
         ZIC   R4,L1R1                BUMP BY LENGTH OF OFFICE                  
         AR    R5,R4                                                            
         LA    R5,1(R5)                                                         
         ZIC   R4,L1R2                LENGTH OF DEPARTMENT                      
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),L1R2C          MOVE IN DEPARTMENT                        
         ZIC   R4,L1R2                BUMP BY LENGTH OF DEPARTMENT              
         AR    R5,R4                                                            
         AR    R5,R4                                                            
         ZIC   R4,L1R3                LENGTH OF SUB-DEPARTMENT                  
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),L1R3C          MOVE IN SUB-DEPARTMENT                    
                                                                                
         L     R5,SRTSEQ              INCREMENT SORT RECORD SEQUENCE            
         LA    R5,1(R5)               NUMBER AND MOVE INTO KEY                  
         STCM  R5,15,SKSEQ                                                      
         ST    R5,SRTSEQ                                                        
         LA    R0,SRCVREC             MOVE ACTUAL RECORD INTO SORT              
         SR    R1,R1                  RECORD SPACE                              
         ICM   R1,3,TIMRLEN                                                     
         LA    RE,TIMKEY                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
PROC98   DS    0H                                                               
         CLI   SORTSW,0                                                         
         BNE   PROC99                                                           
         GOTO1 ADSORTER,DMCB,SORTCARD,SORTRECD                                  
         MVI   SORTSW,1            SET SORT INITIALISED                         
                                                                                
PROC99   DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'PUT',SREC                                       
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* AT REQUEST FIRST (FOR SOON REQUESTS ONLY)                           *         
*---------------------------------------------------------------------*         
RQSTRT   DS    0H                                                               
         LA    R5,QACCOUNT         COUNT NUMBER OF POSITIONS FOR                
         LA    R4,0                ACCOUNT COMPARISON                           
         LA    R1,12                                                            
RQST7    CLI   0(R5),C' '                                                       
         BNH   RQST9                                                            
         LA    R5,1(R5)                                                         
         LA    R4,1(R4)                                                         
         BCT   R1,RQST7                                                         
RQST9    STC   R4,COMPBYTE                                                      
         MVC   COMPACCT,QACCOUNT                                                
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
* GET RECORDS AND PRINT REPORT                                        *         
*---------------------------------------------------------------------*         
LAST     CLI   SORTSW,0            TEST ANYTHING SORTED                         
         BE    EXIT                                                             
         XC    LKEY,LKEY           SET NO PREVIOUS KEY SAVED                    
         XC    CNTELS,CNTELS       INITIALIZE ELEMENT SORT COUNT AND            
         MVC   CURRXSRT,MAINBGN    CURRENT SPOT IN TABLE (BEGINNING)            
         LA    RE,SREC             CLEAR SORTREC AREA                           
         LA    RF,SRLNQ                                                         
         XCEFL (RE),(RF)                                                        
         LA    RE,SVREC            CLEAR SAVE SORTREC AREA                      
         LA    RF,SRLNQ                                                         
         XCEFL (RE),(RF)                                                        
                                                                                
LAST10   DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R1,15,4(R1)                                                      
         BZ    LAST60                                                           
                                                                                
         CLC   FCRDREQS,YES        IF SOON REQUEST FILTER ON ACCOUNT            
         BNE   LAST20              FROM REQUEST CARD                            
         OC    COMPBYTE,COMPBYTE   IF ZERO - NO ACCOUNT WAS INPUT               
         BZ    LAST20                                                           
         ZIC   R4,COMPBYTE         IF NOT ZERO - COMPARE FOR LENGTH             
         BCTR  R4,0                INPUT                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   COMPACCT(0),SRKEY-SREC+3(R1)                                     
         BNE   LAST10                                                           
                                                                                
LAST20   LA    R2,SVREC            MOVE SORTRECORD FROM SORT AREA INTO          
         LA    R3,SRLNQ            SAVE SORTAREA                                
         LA    R4,SREC                                                          
         LA    R5,SRLNQ                                                         
         MVCL  R2,R4                                                            
                                                                                
         LA    R2,SREC             MOVE SORTRECORD FROM SORTER INTO             
         LA    R3,SRLNQ            MY SORTAREA                                  
         LR    R4,R1                                                            
         LA    R5,SRLNQ                                                         
         MVCL  R2,R4                                                            
                                                                                
         OC    LKEY,LKEY                                                        
         BZ    LAST30                                                           
         ZIC   R5,L1R4             COMPARE FOR PERSON/DATE CHANGE               
         LA    R5,2(R5)            IF YES NEW PAGE                              
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   SKPRSPD(0),LKEY+L'SKCMPY+L'SKUSRID                               
         BE    LAST30                                                           
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRNTALL                                                       
         MVC   TOTLIT(6),AC@PERD                                                
         MVC   TOTLIT+7(5),AC@TOTAL                                             
         LA    R4,PPACCUMS                                                      
         BAS   RE,PRTTOTS                                                       
         CLI   QOPT1,C'Y'          PRINT ERROR REPORT?                          
         BE    *+14                                                             
         OC    ERRFLAG,ERRFLAG     DO WE HAVE ANY ERRORS?                       
         BZ    LAST25                                                           
         GOTO1 APRTERR,(RC)        PRINT ERROR REPORT                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
LAST25   SR    R5,R5                                                            
         IC    R5,L1R4             COMPARE FOR PERSON CHANGE                    
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   SKPRSPD(0),LKEY+L'SKCMPY+L'SKUSRID                               
         BE    LAST30                                                           
         MVC   TOTLIT(6),AC@PRSN                                                
         MVC   TOTLIT+7(5),AC@TOTAL                                             
         LA    R4,PRACCUMS                                                      
         BAS   RE,PRTTOTS                                                       
*                                                                               
LAST30   DS    0H                                                               
         GOTO1 ACHKDET,(RC)                                                     
         BAS   RE,PUTELS                                                        
                                                                                
         CLI   LOGOFRST,0          IF FIRST ID SKIP PRINTING END AND            
         BE    LAST50              START LOGOS                                  
         CLC   SKCMPY,SVKCMPY                                                   
         BNE   LAST40                                                           
         CLC   SKUSRID,SVKUSRID                                                 
         BE    LAST50                                                           
LAST40   MVI   FORCEHED,C'Y'                                                    
         MVC   TOTLIT(6),AC@RPT                                                 
         MVC   TOTLIT+7(5),AC@TOTAL                                             
         LA    R4,RTACCUMS                                                      
         BAS   RE,PRTTOTS                                                       
         BAS   RE,ENLOGOS                                                       
         MVC   LGOCMPY,SKCMPY                                                   
         MVC   LGOUSRID,SKUSRID                                                 
         BAS   RE,STLOGOS                                                       
         L     R7,ADBXAREA                                                      
                                                                                
LAST50   MVC   LKEY,SKKEY          SAVE KEY FOR COMPARISON ON NEXT PASS         
         MVC   SVCMPY,SKCMPY       SAVE COMPANY CODE TO SIGNAL CHANGE           
         B     LAST10                                                           
                                                                                
LAST60   DS    0H                                                               
         CLI   SORTSW,0            AT LAST RECORD END SORT AND EXIT             
         BE    EXIT                                                             
         LA    R2,SVREC            THIS WILL ENSURE THAT ALL LOGIC              
         LA    R3,SRLNQ            FLOWS CORRECTLY INCASE I HAD ONLY            
         LA    R4,SREC             ONE SORTRECORD                               
         LA    R5,SRLNQ                                                         
         MVCL  R2,R4                                                            
                                                                                
         LA    R2,SVREC              IF THERE'S NOTHING HERE A SOON             
         OC    0(L'SKKEY,R2),0(R2)   REQUEST PROCESSED SORT RECORDS             
         BZ    LAST80                BUT DID NOT FIND ANY FOR THIS              
*                                    SPECIFIC ACCOUNT                           
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRNTALL                                                       
         MVC   TOTLIT(6),AC@PERD                                                
         MVC   TOTLIT+7(5),AC@TOTAL                                             
         LA    R4,PPACCUMS                                                      
         BAS   RE,PRTTOTS                                                       
         CLI   QOPT1,C'Y'          PRINT ERROR REPORT?                          
         BE    *+14                                                             
         OC    ERRFLAG,ERRFLAG     DID WE GET ANY ERORS?                        
         BZ    LAST70                                                           
         GOTO1 APRTERR,(RC)        PRINT ERROR REPORT                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
LAST70   MVC   TOTLIT(6),AC@PRSN                                                
         MVC   TOTLIT+7(5),AC@TOTAL                                             
         LA    R4,PRACCUMS                                                      
         BAS   RE,PRTTOTS                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   TOTLIT(6),AC@RPT                                                 
         MVC   TOTLIT+7(5),AC@TOTAL                                             
         LA    R4,RTACCUMS                                                      
         BAS   RE,PRTTOTS                                                       
         BAS   RE,ENLOGOS                                                       
                                                                                
LAST80   GOTO1 ADSORTER,DMCB,=C'END'                                            
         CLI   RCPOSTNG,C'N'       TEST POSTING FILE REQUIRED                   
         BE    EXIT                                                             
         GOTO1 DATAMGR,DMCB,WRKCLO,WRKFIL,WRKID,AIO2,AWRKBUFF                   
         LM    R0,R1,MAINLEN       FREE UP STORAGE                              
         FREEMAIN R,LV=(0),A=(1)                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET THE TSI DETAIL RECORDS AND LEAVE IN AIO                         *         
*     IN PUTELS ROUTINE - CHECK TO MAKE SURE THAT DAY HOURS DO NOT    *         
*     EXCEED 24 HOURS FOR ANY GIVEN SINGLE DAY                        *         
***********************************************************************         
         SPACE 1                                                                
GETDET   NTR1                                                                   
         USING RCVRECD,RE             R2=A(RECOVERY HEADER)                     
         L     RE,ADTRANS                                                       
         USING TIMRECD,R4                                                       
         LA    R4,RCVRECRD                                                      
         DROP  RE                                                               
*                                                                               
         USING TSIRECD,R2                                                       
         LA    R2,SVKEY            READ FOR DETAIL RECORD                       
         MVI   TSIKTYP,TSIKTYPQ    X'3E' - TYPE                                 
         MVI   TSIKSUB,TSIKSUBQ    X'14' - SUB TYPE                             
         MVC   TSIKCULA(TIMKPEDT-TIMKEY),TIMKEY   COMPANY - CONTRA              
         SR    R1,R1                                                            
         ICM   R1,7,TIMKPEDT                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TSIKPEDT                                                    
         GOTO1 =A(DMHIGHDR),DMCB,(RC),AIO1                                      
         B     GETD20                                                           
*                                                                               
GETD10   DS    0H                                                               
         GOTO1 =A(DMSEQDR),DMCB,(RC),AIO1                                       
GETD20   L     R2,AIO1                                                          
         CLC   SVKEY(TSIKPEDT-TSIKEY),0(R2)    SAME KEY?                        
         BNE   GETDX                                                            
         LA    R1,SVKEY                                                         
         CLC   TSIKPEDT,TSIKPEDT-TSIKEY(R1)    SAME PERIOD END DATE?            
         BNE   GETDX                                                            
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC),AIO1     GET DETAIL RECORD                
*                                                                               
         USING DETD,R6                                                          
         LA    R6,DETWRK                                                        
         XC    DETWRK,DETWRK                                                    
         LA    R1,DETDBKT          CLEAR BUCKETS                                
         LA    R0,DETDBNUM                                                      
         ZAP   0(L'DETDBKT,R1),=P'0'                                            
         LA    R1,L'DETDBKT(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         USING XTMELD,R5                                                        
         LA    R5,TSIRFST          POINT TO FIRST ELEMENT                       
GETD30   CLI   0(R5),0             AT THE END OF THE RECORD?                    
         BE    GETD10                                                           
         CLI   0(R5),XTMELQ        WE ONLY WANT TEMPO LINE DETAIL               
         BNE   GETD40                                                           
         CLI   XTMTYP,XTMTLIN      PULL LINE INFO OFF RECORD                    
         BE    GETD45                                                           
         CLI   XTMTYP,XTMTALL      PULL ALLOC INFO AS WELL                      
         BE    GETD50                                                           
GETD40   SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     GETD30                                                           
*                                                                               
GETD45   MVC   SVLIN#,XTMLIN#      SAVE OFF THE TMS LINE NUMBER                 
         B     GETD40                                                           
*                                                                               
GETD50   SR    R0,R0                                                            
         ICM   R0,1,XTMMINI                                                     
         BZ    GETD40                                                           
         LA    R3,XTMDATA          POINT TO START OF MINIS                      
         USING XTMDATA,R3                                                       
GETD60   TM    XTMMSTAT,XTMMSWPQ   SKIP ALL SWIPES                              
         BNO   GETD70                                                           
GETD65   LA    R3,XTMLN4Q(R3)                                                   
         BCT   R0,GETD60                                                        
         B     GETD40                                                           
*                                                                               
GETD70   DS    0H                                                               
         MVC   DETWRK,SPACES                                                    
         MVC   DET1R,TIMKACT       PUT IN 1R ACCOUNT                            
         MVC   DETPEDT,TIMKPEDT    PUT IN PERIOD END DATE                       
         MVC   DETLN#,SVLIN#       FILL IN TMS LINE NUMBER                      
         MVC   DETTLN#,XTMTLIN#    TEMPO LINE NUMBER                            
         MVC   DETDAY,XTMDAY       DAY                                          
         ZAP   DETDHRS,XTMBLK      MOVE IN HOURS                                
         DROP  R3                                                               
*                                                                               
*        MVC   MSG,=CL10'DETWRK#1'                                              
*        GOTO1 ADUMP,DMCB,(RC),DETWRK,L'DETWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),DETWRK,ADETTAB   ADD TABLE ENTRY               
*                                                                               
         XC    DETLN#,DETLN#       CLEAR OUT TMS   LINE NUM FOR TOTALS          
         XC    DETTLN#,DETTLN#     CLEAR OUT TEMPO LINE NUM FOR TOTALS          
*                                                                               
*        MVC   MSG,=CL10'DETWRK#2'                                              
*        GOTO1 ADUMP,DMCB,(RC),DETWRK,L'DETWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),DETWRK,ADETTAB   ADD TABLE ENTRY               
         B     GETD65                                                           
*                                                                               
GETDX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ALL ELEMENTS WITHIN A PERSON CODE NEED TO BE SORTED TO              *         
*     BE ABLE TO REPORT ON THEM IN LINE NUMBER AND TO DETERMINE       *         
*     WHICH ONES HAVE BEEN ADDED, CHANGED, OR DELETED - OR ARE        *         
*     DUPLICATES AND CAN BE DROPPED FROM REPORTING                    *         
***********************************************************************         
PUTELS   NTR1                                                                   
         CLC   SVCMPY,SKCMPY       IF COMPANY HAS CHANGED READ LEDGER           
         BE    *+8                 RECORD INFORMATION INTO LEDGER               
         BAS   RE,LEDGRD           TABLE                                        
                                                                                
         LA    R4,SRKEY            ARE THERE ANY TIME ELEMENTS TO BE            
         MVI   ELCODE,TIMELQ       PROCESSED                                    
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
                                                                                
         USING TIMELD,R4                                                        
PELS050  CLI   TIMETYP,TIMEINP        TYPE OF TIME ELEMENT                      
         BE    PELS100                                                          
         CLI   TIMETYP,TIMETAX        TAX TYPE                                  
         BE    PELS100                                                          
         CLI   TIMETYP,TIMENAR        NARRATIVE                                 
         BNE   PELS200                                                          
                                                                                
         USING XSRTD,R2                                                         
PELS100  L     R2,CURRXSRT            CURRENT SPOT IN SORT TABLE                
                                                                                
PELS150  CLI   TIMETYP,TIMEINP        INPUT DETAIL IS THE ONLY ELEMENT          
         BE    PELS160                  TO CARRY THE LINE NUMBER BUT I          
         MVC   XTIMLIN,SVTIMLN#         NEED IT ON EVERY ELEMENT FOR            
         B     PELS170                  SORT                                    
PELS160  MVC   XTIMLIN,TIMLINE#       TIME LINE NUMBER                          
         MVC   SVTIMLN#,TIMLINE#      (SAVE FOR NARR AND TAX ELS)               
PELS170  MVC   XSEQNM,CNTELS          INTERNAL COUNTER                          
         MVC   XRCVTYP,SKTYPE         RCVREC TYPE (ADD,CPY,CHA)                 
         MVC   XTIMTYP,TIMETYP        TIME ELEMENT TYPE                         
         MVC   XRCVPRG,SKPRG                                                    
*                                                                               
         USING TIMRECD,R3                                                       
         LA    R3,SRKEY               MOVE CONTRA ACCOUNT INTO SORT             
         MVC   XCNTRA,TIMKULC           FOR ELEMENTS SO I HAVE IT TO            
         ZIC   R5,1(R4)                 PRINT                                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   XELEM(0),0(R4)         MOVE IN ACTUAL ELEMENT                    
         LA    R2,XTABLEN(R2)                                                   
         MVI   0(R2),X'FF'            MARK NEW END OF TABLE                     
         ST    R2,CURRXSRT            STORE CURRENT TABLE ADDRESS               
         L     R5,CNTELS              INCREMENT COUNT OF ELEMENTS               
         LA    R5,1(R5)                 I WILL BE SORTING                       
         ST    R5,CNTELS                                                        
         CHI   R5,SORTMAX             HAVE I REACHED GETMAIN                    
         BNH   *+6                      TABLE CAPACITY                          
         DC    H'0'                                                             
                                                                                
PELS200  MVI   ELCODE,TIMELQ                                                    
         BAS   RE,NEXTEL                                                        
         BE    PELS050                                                          
         B     EXIT                   GET NEXT SORTRECORD                       
         EJECT                                                                  
*---------------------------------------------------------------------*         
* PRINT ALL ELEMENTS THAT CONSTITUTE AN ADD, COPY, OR CHANGE FROM     *         
* ALL '8B' ELEMENTS FROM THIS GROUP OF PERSON/PERIOD RECORDS          *         
*---------------------------------------------------------------------*         
PRNTALL  NTR1                                                                   
         CLI   LOGOFRST,0                                                       
         BNE   PALL020                                                          
         MVC   LGOCMPY,SVKCMPY                                                  
         MVC   LGOUSRID,SVKUSRID                                                
         BAS   RE,STLOGOS                                                       
         L     R7,ADBXAREA                                                      
                                                                                
PALL020  L     R2,MAINBGN                                                       
         CLI   0(R2),X'FF'                                                      
         BE    PALLXIT                                                          
         L     R3,CNTELS                                                        
         GOTO1 XSORT,DMCB,(0,(R2)),(R3),XTABLEN,XSRKLEN,0                       
                                                                                
         BAS   RE,FILHEAD             GET ALL HEADLINE INFO                     
                                                                                
         USING TIMELD,R4                                                        
         USING XSRTD,R2                                                         
         L     R2,MAINBGN                                                       
                                                                                
PALL050  CLI   0(R2),X'FF'            END OF TABLE                              
         BE    PALLXIT                                                          
         XC    SVEL1O,SVEL1O          CLEAR OUT ALL ELEMENT WORK AREAS          
         XC    SVEL2O,SVEL2O                                                    
         XC    SVEL3O,SVEL3O                                                    
         XC    SVEL1N,SVEL1N                                                    
         XC    SVEL2N,SVEL2N                                                    
         XC    SVEL3N,SVEL3N                                                    
                                                                                
         MVC   SVXSRTK,XSRTKEY        SAVE SORT KEY FOR BREAK CHECK             
         CLI   XRCVTYP,RCVRCPYQ       IS RECORD TYPE COPY (ORIGINAL)            
         BNE   PALL300                                                          
         CLI   XTIMTYP,TIMEINP        IS ELEMENT FLAG INPUT DETAIL              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVEL1O,XSRTSTRT                                                  
                                                                                
         LA    R2,XTABLEN(R2)         BUMP TO NEXT ELEMENT                      
         CLI   0(R2),X'FF'            END OF TABLE?                             
         BE    PALL800                                                          
         CLC   XTIMLIN,SVXSRTK        HAS KEY CHANGED                           
         BH    PALL800                                                          
         CLI   XTIMTYP,TIMETAX        IS IT TAX ELEMENT                         
         BNE   PALL150                                                          
         MVC   SVEL2O,XSRTSTRT                                                  
                                                                                
         LA    R2,XTABLEN(R2)         NEXT ELEMENT IN TABLE                     
         CLI   0(R2),X'FF'            END OF TABLE?                             
         BE    PALL800                                                          
PALL150  CLC   XTIMLIN,SVXSRTK        HAS SORT KEY CHANGED                      
         BH    PALL800                                                          
         CLI   XTIMTYP,TIMENAR        COULD BE NEXT ITEM FOR SAME LINE          
         BNE   PALL300                NUMBER                                    
         MVC   SVEL3O,XSRTSTRT                                                  
         LA    R2,XTABLEN(R2)         NEXT ELEMENT IN TABLE                     
         CLI   0(R2),X'FF'            END OF TABLE?                             
         BE    PALL800                                                          
                                                                                
PALL300  CLI   XRCVTYP,RCVRCPYQ       IF NOT CHANGE OR ADD - END                
         BE    PALL800                                                          
         CLC   XTIMLIN,SVXSRTK        HAS SORT KEY CHANGED                      
         BNE   PALL800                                                          
         CLI   XTIMTYP,TIMEINP        IS ELEMENT TYPE INPUT DETAIL              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVEL1N,XSRTSTRT                                                  
                                                                                
         LA    R2,XTABLEN(R2)         NEXT ELEMENT IN TABLE                     
         CLI   0(R2),X'FF'            END OF TABLE?                             
         BE    PALL800                                                          
         CLC   XTIMLIN,SVXSRTK        HAS SORT KEY CHANGED                      
         BH    PALL800                                                          
         CLI   XTIMTYP,TIMETAX        IS ELEMENT TYPE TAX                       
         BNE   PALL350                                                          
         MVC   SVEL2N,XSRTSTRT                                                  
                                                                                
         LA    R2,XTABLEN(R2)         NEXT ELEMENT IN TABLE                     
         CLI   0(R2),X'FF'            END OF TABLE?                             
         BE    PALL800                                                          
PALL350  CLC   XTIMLIN,SVXSRTK                                                  
         BH    PALL800                                                          
         CLI   XTIMTYP,TIMENAR        MUST BE A SECOND CHANGE TO THE            
         BNE   PALL800                SAME LINE INFORMATION                     
         MVC   SVEL3N,XSRTSTRT                                                  
         LA    R2,XTABLEN(R2)         BUMP TABLE SO IT WILL BE POINTING         
*                                     TO NEXT ELEMENT TO BE PROCESSED           
                                                                                
PALL800  DS    0H                                                               
         USING XSRTD,R2               NEED THIS                                 
         ST    R2,SXSRTADD            STORE ADDRESS FOR NEXT PASS               
         BAS   RE,COMPELS             ASSIGN PROPER ACTION TO ELEMENTS          
         LA    R2,SVEL1O                                                        
         CLI   XELEM,0                NOTHING WAS IN THIS SPACE                 
         BE    PALL820                                                          
         CLI   XELEM,MARKDUP          IF IT WAS A DUPLICATE DON'T               
         BE    PALL840                INCLUDE IT IN THE REPORT                  
         ST    R2,SVELADDR            ELSE PRINT THIS GROUP                     
         LA    R4,XELEM               POINT TO ACTUAL '8B' ELEMENT              
         BAS   RE,PTYPE1              PRINT THIS GROUP                          
                                                                                
PALL820  DS    0H                                                               
         LA    R2,SVEL1N              POINT TO NEW GROUP                        
         CLI   XELEM,0                IS ANYTHING IN THIS SPACE                 
         BE    PALL840                                                          
         ST    R2,SVELADDR                                                      
         LA    R4,XELEM               POINT TO ACTUAL '8B' ELEMENT              
         BAS   RE,PTYPE1              PRINT THIS GROUP                          
PALL840  L     R2,SXSRTADD            RESTORE SORT TABLE ADDRESS                
         B     PALL050                MAKE NEXT PASS                            
                                                                                
PALLXIT  DS    0H                                                               
         BAS   RE,CLRXSRT             CLEAR SORT TABLE FOR NEXT PASS            
         B     EXIT                                                             
         DROP R2                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT TYPE ONE ELEMENTS                                  *         
***********************************************************************         
         USING TIMRECD,R3                                                       
PTYPE1   NTR1                                                                   
         SR    RF,RF                  IF NOT ENOUGH LINES LEFT TO PRINT         
         IC    RF,LINE                ENTIRE ENTRY, FORCE TO A NEW PAGE         
         CH    RF,=H'43'                                                        
         BNH   PTYP110                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     PTYP120                                                          
                                                                                
PTYP110  CH    RF,=H'11'              IF THIS IS THE FIRST ENTRY BOX            
         BNH   PTYP120                WILL AUTOMATICALLY BE OPENED              
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         GOTO1 ACREPORT                                                         
                                                                                
         USING PLINED,RE                                                        
PTYP120  LA    RE,XP                                                            
         MVC   XP,XSPACES             FILL PRINT BLOCK WITH CONSTANTS           
         MVC   PACT,AC@ACT                                                      
         MVC   PTYPE(4),AC@TYPE1                                                
         MVC   PHRS(5),AC@HOURS                                                 
         MVC   PCODE(4),AC@CODE                                                 
         MVC   PNAME(4),AC@NAME                                                 
         MVC   PTASK(4),AC@TASK                                                 
         MVC   PMOA(3),AC@MOA                                                   
         MVC   PRATE(4),AC@RATE                                                 
         MVC   PAMOUNT(6),AC@AMT                                                
         MVC   PINC(16),AC@ICR                                                  
         GOTO1 ACREPORT               PRINT                                     
         DROP  RE                                                               
*                                                                               
         SR    RF,RF                  AND INSERT A MIDLINE                      
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
*                                                                               
         GOTO1 ACREPORT               PRINT                                     
*                                                                               
         MVC   XP,XSPACES                   FILL IN REST OF CONSTANTS           
**       MVC   XP+133(8),AC@WC                                                  
         MVC   XPSECOND,XSPACES                                                 
**       MVC   XPSECOND+133(8),AC@LOCAL                                         
         MVC   XPTHIRD,XSPACES                                                  
         MVC   XPTHIRD+2(4),AC@PROG                                             
**       MVC   XPTHIRD+133(5),AC@BASIS                                          
         MVC   XPFOURTH,XSPACES                                                 
         MVC   XPFOURTH+19(4),AC@RSNAR                                          
**       MVC   XPFOURTH+133(6),AC@AMT                                           
                                                                                
         USING TIMELD,R4                                                        
         USING XSRTD,R5                                                         
         L     R5,SVELADDR                                                      
         USING PLINED,R2                                                        
         LA    R2,XPTHIRD                                                       
         MVC   PID+2(4),AC@TIME                                                 
         CLI   XRCVPRG,RCVPCAPQ                                                 
         BE    PTYP130                                                          
         MVC   PID+2(6),AC@BRAO                                                 
         CLI   XRCVPRG,RCVPBRAQ                                                 
         BE    PTYP130                                                          
         MVC   PID+2(4),AC@BLG                                                  
         CLI   XRCVPRG,RCVPCBLQ                                                 
         BE    PTYP130                                                          
         MVC   PID+2(4),=C'    '                                                
                                                                                
PTYP130  LA    R2,XP                     PRINT TIME TYPE INDICATORS             
*                                                                               
PTYP140  MVC   PTYPE,AC@RTIME                                                   
         CLI   TIMTTYP,TIMTCR                                                   
         BE    PTYP150                                                          
         MVC   PTYPE,AC@NTIME                                                   
         CLI   TIMTTYP,TIMTCN                                                   
         BE    PTYP150                                                          
         CLI   TIMTTYP,TIMTNC                                                   
         BE    PTYP150                                                          
                                                                                
         MVC   PTYPE,AC@BTIME                                                   
         MVC   PTYPE+1(1),AC@WIND                                               
         TM    TIMIND,TIMIWO                                                    
         BO    PTYP160                                                          
         MVC   PTYPE+1(1),AC@AIND                                               
         TM    TIMIND,TIMIADJ                                                   
         BO    PTYP160                                                          
         MVI   PTYPE+1,C' '                                                     
                                                                                
PTYP150  MVC   PTYPE+1(1),AC@TIND                                               
         TM    TIMSTAT,TIMTEMPO                                                 
         BO    PTYP160                                                          
         MVI   PTYPE+1,C' '                                                     
PTYP160  MVC   PACT,AC@DEL               PRINT ELEMENT ACTION                   
         CLI   0(R4),MARKDEL                                                    
         BE    PTYP170                                                          
         MVC   PACT,AC@ADD                                                      
         CLI   0(R4),MARKADD                                                    
         BE    PTYP170                                                          
         MVC   PACT,AC@ORG                                                      
         CLI   0(R4),MARKCPY                                                    
         BE    PTYP170                                                          
         MVC   PACT,AC@CHG1                                                     
         CLI   0(R4),MARKCHA                                                    
         BE    PTYP170                                                          
                                                                                
PTYP170  CLC   TIMTSK,SPACES                                                    
         BE    PTYP1100                                                         
         MVC   PTASK(L'TIMTSK),TIMTSK    TASK CODE TO PRINT LINE                
         USING WCORECD,R1                                                       
         L     R1,AIO1                                                          
         MVC   0(42,R1),SPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,SVKCMPY                                                  
         MVC   WCOKUNT(2),=C'SJ'                                                
         MVC   WCOKWRK,TIMTSK                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO2                             
         BNE   PTYP1100                                                         
         L     R5,AIO2                                                          
         AH    R5,DATADISP                                                      
PTYP180  CLI   0(R5),0                                                          
         BE    PTYP1100                                                         
         CLI   0(R5),WCOELQ              FIND WORK CODE ELEMENT ON              
         BE    PTYP190                   SJ RECORD                              
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     PTYP180                                                          
*                                                                               
         USING WCOELD,R5                                                        
PTYP190  LA    R2,XPSECOND                                                      
         MVC   PTASK,WCODESC             WORK CODE DESC TO PRINT LINE           
                                                                                
PTYP1100 LA    R2,XP                                                            
         MVC   WORK(2),TIMMOA            MONTH OF ACTIVITY                      
         MVC   WORK+2(1),=X'01'                                                 
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMOA)                                    
                                                                                
         CLI   TIMLN,TIMILNQ             CHECK TO SEE IF HOURS                  
         BNH   PTYP1170                  EXISTS ON ELEMENT                      
         CLI   0(R4),MARKDEL                                                    
         BNE   PTYP1110                                                         
         ZAP   FULL,TIMHRS                                                      
         MP    FULL,=PL1'-1'                                                    
         ZAP   TIMHRS,FULL+1(3)                                                 
PTYP1110 DS    0H                                                               
         EDIT  (P3,TIMHRS),(7,PHRS),2,MINUS=YES                                 
         AP    PPHRS,TIMHRS              ACCUM HOURS BY PERSON/PERIOD           
         AP    PRHRS,TIMHRS              PERSON                                 
         AP    RTHRS,TIMHRS              REPORT                                 
                                                                                
         CLI   TIMLN,TIMILN1Q            DOES RATE AND INCOME ACCT              
         BNH   PTYP1170                  EXIST ON ELEMENT                       
         EDIT  (P4,TIMRATE),(8,PRATE),2,MINUS=YES   EDIT RATE                   
                                                                                
         CLC   TIMINC,SPACES             INCOME ACCOUNT PRESENT                 
         BNH   PTYP1150                                                         
         MVC   PINC,TIMINC               MOVE IN INCOME ACCOUNT                 
         L     R1,AIO1                   READ INCOME ACCOUNT TO GET             
         MVC   0(42,R1),SPACES           12 ANALYSIS POINTER FROM               
         MVC   0(1,R1),SVKCMPY                                                  
         MVC   1(L'TIMINC,R1),TIMINC                                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO1,AIO2                             
         BE    PTYP1120                                                         
         LA    R2,XPTHIRD                                                       
         MVC   PINC(14),AC@BIA           BAD INCOME ACCOUNT                     
         B     PTYP1150                                                         
*                                                                               
PTYP1120 L     R5,AIO2                                                          
         AH    R5,DATADISP                                                      
PTYP1130 CLI   0(R5),0                                                          
         BE    PTYP1150                                                         
         CLI   0(R5),SPAELQ              FIND '2C' ELEMENT                      
         BE    PTYP1140                                                         
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     PTYP1130                                                         
*                                                                               
         USING SPAELD,R5                                                        
PTYP1140 LA    R2,XPTHIRD                                                       
         MVC   PINC(2),=C'12'                                                   
         MVC   PINC+2(12),SPAAANAL       MOVE 12 ACCOUNT TO PRINT LINE          
                                                                                
PTYP1150 CLI   TIMLN,TIMBLNQ             DOES AMOUNT EXIST ON ELEMENT           
         BNH   PTYP1170                                                         
         LA    R2,XP                     RESET PRINT ADDRESS                    
         CLI   0(R4),MARKDEL                                                    
         BNE   PTYP1160                                                         
         ZAP   DUB,TIMAMNT                                                      
         MP    DUB,=PL1'-1'                                                     
         ZAP   TIMAMNT,DUB+2(6)                                                 
*                                                                               
PTYP1160 DS    0H                                                               
         EDIT  (P6,TIMAMNT),(12,PAMOUNT),2,MINUS=YES                            
         AP    PPAMT,TIMAMNT                                                    
         AP    PRAMT,TIMAMNT                                                    
         AP    RTAMT,TIMAMNT                                                    
                                                                                
PTYP1170 CLC   TIMACC(2),=C'1N'          IS THIS NON CLIENT TIME                
         BNE   PTYP1180                                                         
         LA    R2,XP                     RESET PRINT ADDRESS                    
         MVC   PCODE,TIMACC+2            MOVE ACC TO PRINT LINE                 
         MVC   TEMPCUL(1),SVKCMPY        AND SET ACCOUNT TO BE READ             
         MVC   TEMPCUL+1(14),TIMACC      FOR NAME                               
         LA    R5,12                                                            
         LA    R6,PNAME                                                         
         BAS   RE,READACC                                                       
         B     PTYP1240                                                         
                                                                                
PTYP1180 LA    R2,XPSECOND                                                      
         CLI   TIMTTYP,TIMTCB            ONLY SHOW COST ACCOUNT FOR             
         BE    PTYP1190                  TYPE B TIME                            
         CLI   TIMTTYP,TIMTCR            OR TYPE R TIME                         
         BNE   PTYP1200                                                         
         USING XSRTD,R5                                                         
PTYP1190 L     R5,SVELADDR                                                      
         MVC   PINC,XCNTRA                                                      
                                                                                
PTYP1200 LA    R2,HEIRSJ                                                        
         LA    R3,TIMACC-1                                                      
         LA    R5,LSJ                                                           
         GOTO1 ACCDIV,DMCB,(R2),(R3),(R5)                                       
         LA    R6,4                                                             
         LA    R5,LSJ                    ACCDIV PASSES BACK LENGTHS             
PTYP1210 CLI   0(R5),X'FF'               MINUS ONE SO BUMP LENGTH BY 1          
         BNE   PTYP1220                                                         
         MVI   0(R5),0                                                          
         B     PTYP1230                                                         
PTYP1220 ZIC   R0,0(R5)                                                         
         LA    R0,1(R0)                                                         
         STC   R0,0(R5)                                                         
PTYP1230 LA    R5,LVENT(R5)                                                     
         BCT   R6,PTYP1210                                                      
                                                                                
         LA    R2,XP                                                            
         MVC   PCODE(L'PCODE),LSJ1C      CLIENT                                 
         MVC   TEMPCUL(1),SVKCMPY        SET COMPANY FOR NAME READ              
         MVC   TEMPCUL+1(2),=C'SJ'       SET U/L FOR NAME READ                  
         MVC   TEMPACC,TIMACC+2          SET ACCOUNT FOR NAME READ              
         ZIC   R5,CLSJ1                                                         
         LA    R6,PNAME                                                         
         BAS   RE,READACC                                                       
                                                                                
         CLC   LSJ2C,SPACES                                                     
         BNH   PTYP1240                                                         
         LA    R2,L'XP(R2)                                                      
         MVC   PCODE(L'PCODE),LSJ2C      PRODUCT                                
         ZIC   R5,CLSJ2                                                         
         LA    R6,PNAME                                                         
         BAS   RE,READACC                                                       
                                                                                
         CLC   LSJ3C,SPACES                                                     
         BNH   PTYP1240                                                         
         LA    R2,L'XP(R2)                                                      
         MVC   PCODE(L'PCODE),LSJ3C      JOB                                    
         ZIC   R5,CLSJ3                                                         
         LA    R6,PNAME                                                         
         BAS   RE,READACC                                                       
                                                                                
PTYP1240 DS    0H                                                               
         LA    R4,L'SVEL1O(R4)                                                  
         CLI   0(R4),0                                                          
         BE    *+8                                                              
         BAS   RE,PTYPE2                                                        
         LA    R4,L'SVEL2O(R4)                                                  
         CLI   0(R4),0                                                          
         BE    *+8                                                              
         BAS   RE,PTYPE3                                                        
                                                                                
         GOTO1 ACREPORT                  PRINT ALL DETAIL INFO                  
         SR    RF,RF                     CLOSE BOX                              
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         GOTO1 ACREPORT                                                         
                                                                                
PTXIT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ROUTINE TO PRINT TYPE TWO ELEMENTS                                  *         
*---------------------------------------------------------------------*         
PTYPE2   NTR1                                                                   
**T      TAX SOMEHOW ENDED UP ON FILE AND HASN'T BEEN TESTED YET                
         B     EXIT                                                             
**T                                                                             
*        USING PLINED,R2                                                        
*        LA    R2,XP                                                            
*        USING TIMELD,R4                                                        
*        LA    R6,PTAX                                                          
*        MVC   0(L'TIMTWC,R6),TIMTWC                                            
*        LA    R6,L'XP(R6)                                                      
*        MVC   0(L'TIMTLOC,R6),TIMTLOC                                          
*        LA    R6,L'XP(R6)                                                      
*        EDIT  (P6,TIMTBAS),(12,(R6)),2,MINUS=YES,ALIGN=LEFT                    
*        CLI   TIMLN,TIMTHDQ                                                    
*        BNH   EXIT                                                             
*        ZIC   R3,TIMTMINI                                                      
*        LA    R5,TIMTAMNT                                                      
*TWO10   AP    DUB,0(L'TIMTAMNT,R5)                                             
*        LA    R5,TIMTMINQ(R5)                                                  
*        BCT   R3,PTWO10                                                        
*        LA    R6,L'XP(R6)                                                      
*        EDIT  (P8,DUB),(12,(R6)),2,MINUS=YES,ALIGN=LEFT                        
*        B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ROUTINE TO PRINT TYPE THREE ELEMENTS                                *         
*---------------------------------------------------------------------*         
PTYPE3   NTR1                                                                   
         USING PLINED,R2                                                        
         LA    R2,XPFOURTH                                                      
         USING TIMELD,R4                                                        
         ZIC   R5,1(R4)                                                         
         SH    R5,=H'5'            ACCOUNT FOR ELEMENT CODE, LENGTH,            
         EX    R5,*+8              SEQ NUM, AND TYPE                            
         B     *+10                                                             
         MVC   PNARR(0),TIMNARR                                                 
         B     EXIT                                                             
                                                                                
         GETEL R4,MASTDISP,ELCODE                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ROUTINE TO PRINT PERSON/PERIOD TOTALS, OR PERSON TOTALS, OR         *         
* REPORT TOTALS                                                       *         
*---------------------------------------------------------------------*         
PRTTOTS  NTR1                                                                   
         USING PLINED,R2                                                        
         USING PDACCUMS,R4                                                      
         L     R7,ADBXAREA                                                      
         SR    RF,RF                  IF NOT ENOUGH LINES LEFT TO PRINT         
         IC    RF,LINE                ENTIRE ENTRY, FORCE TO A NEW PAGE         
         CH    RF,=H'43'                                                        
         BNH   PRT010                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     PRT020                                                           
PRT010   CH    RF,=H'11'              IF THIS IS THE FIRST ENTRY BOX            
         BNH   PRT020                 WILL AUTOMATICALLY BE OPENED              
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         GOTO1 ACREPORT                                                         
                                                                                
PRT020   LA    R2,XP                                                            
         MVC   XP,XSPACES                                                       
         MVC   PACT,AC@ACT                                                      
         MVC   PTYPE(4),AC@TYPE1                                                
         MVC   PHRS(5),AC@HOURS                                                 
         MVC   PCODE(4),AC@CODE                                                 
         MVC   PNAME(4),AC@NAME                                                 
         MVC   PTASK(4),AC@TASK                                                 
         MVC   PMOA(3),AC@MOA                                                   
         MVC   PRATE(4),AC@RATE                                                 
         MVC   PAMOUNT(6),AC@AMT                                                
         MVC   PINC(16),AC@ICR                                                  
         GOTO1 ACREPORT                                                         
                                                                                
         SR    RF,RF                  AND INSERT A MIDLINE                      
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         GOTO1 ACREPORT                                                         
         LA    R2,XP                                                            
         MVC   1(L'TOTLIT,R2),TOTLIT                                            
         GOTO1 ACREPORT                                                         
         EDIT  (P4,PDHRS),(9,PHRS-2),2,MINUS=YES                                
         EDIT  (P6,PDAMT),(12,PAMOUNT),2,MINUS=YES                              
         GOTO1 ACREPORT                                                         
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         GOTO1 ACREPORT                                                         
         ZAP   PDHRS,=P'0'                                                      
         ZAP   PDAMT,=P'0'                                                      
         MVC   TOTLIT,SPACES                                                    
         B     EXIT                                                             
         EJECT                                                                  
         DROP  R2                                                               
*---------------------------------------------------------------------*         
* ROUTINE TO COMPARE ELEMENT GROUPS                                   *         
*---------------------------------------------------------------------*         
         USING XSRTD,R2                                                         
COMPELS  NTR1                                                                   
         LA    R2,SVEL1O                                                        
         CLI   XELEM,0                                                          
         BNE   CMP020                                                           
         LA    R2,SVEL1N                                                        
         MVI   XELEM,MARKADD                                                    
         B     CMP100                                                           
                                                                                
CMP020   DS    0H                                                               
         LA    R2,SVEL1N                                                        
         CLI   XELEM,0                                                          
         BNE   CMP030                                                           
         LA    R2,SVEL1O                                                        
         MVI   XELEM,MARKDEL                                                    
         B     CMP100                                                           
                                                                                
CMP030   DS    0H                                                               
         LA    R4,SVEL1O+XELSTRT                                                
         LA    R6,SVEL1N+XELSTRT                                                
         ZIC   R5,1(R4)                                                         
         STC   R5,BYTE            NEED FOR NEXT COMPARISON                      
         SH    R5,=H'4'           BCTR,ELCODE,ELLEN,ELSEQ#                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   SVEL1O+XELSTRT+3(0),SVEL1N+XELSTRT+3                             
         BE    CMP045                                                           
                                                                                
         CLC   SVEL1O+XELSTRT+TIMHLNQ(TIMILNQ),SVEL1N+XELSTRT+TIMHLNQ           
         BNE   CMP050                                                           
         CLC   SVEL1O+XELSTRT+TIMHLNQ+TIMILNQ+1(TIMLKCBI-TIMLINE#),SVELX        
               1N+XELSTRT+TIMHLNQ+TIMILNQ+1                                     
         BNE   CMP050                                                           
         CLC   TIMAMNT-TIMELD(L'TIMAMNT,R4),TIMAMNT-TIMELD(R6)                  
         BNE   CMP050                                                           
         LA    R2,SVEL1O                                                        
         CLI   XRCVPRG,RCVPCBLQ                                                 
         BNE   CMP050                                                           
                                                                                
CMP045   MVI   SVEL1O+XELSTRT,MARKDUP                                           
         MVI   SVEL1N+XELSTRT,MARKDUP                                           
         B     CMP100                                                           
CMP050   CLC   TIMHLNQ(TIMILNQ-TIMHLNQ,R4),TIMHLNQ(R6)                          
         BNE   CMP070                                                           
         MVI   SVEL1O+XELSTRT,MARKCPY                                           
         MVI   SVEL1N+XELSTRT,MARKCHA                                           
         B     CMP100                                                           
CMP070   MVI   SVEL1O+XELSTRT,MARKDEL                                           
         MVI   SVEL1N+XELSTRT,MARKADD                                           
                                                                                
CMP100   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ROUTINE TO CLEAR ANY ENTRIES ON XSORT TABLE                         *         
*---------------------------------------------------------------------*         
CLRXSRT  NTR1                                                                   
         XC    CNTELS,CNTELS                                                    
         L     R2,MAINBGN                                                       
CLR050   CLI   0(R2),X'FF'                                                      
         BE    CLRXIT                                                           
         XC    0(XTABLEN,R2),0(R2)                                              
         LA    R2,XTABLEN(R2)                                                   
         B     CLR050                                                           
CLRXIT   MVI   0(R2),0                                                          
         L     R2,MAINBGN                                                       
         MVI   0(R2),X'FF'                                                      
         ST    R2,CURRXSRT                                                      
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        READ LEDGER RECORD FOR STRUCTURE                                       
*---------------------------------------------------------------------*         
LEDGRD   NTR1                                                                   
         USING LDGTABD,R2                                                       
         LA    R2,LDGTAB              LEDGER READ TABLE                         
                                                                                
LEDG10   L     RE,AIO1                CLEAR IO AREA                             
         LH    RF,=Y(L'IO1)                                                     
         XCEFL (RE),(RF)                                                        
                                                                                
         L     R1,AIO1                READ FIRST LEDGER                         
         MVC   0(42,R1),SPACES                                                  
         MVC   0(1,R1),SKCMPY                                                   
         MVC   1(2,R1),LDG                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO1,AIO1                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1                GET HEIRARCHY ELEMENT                     
         AH    R4,DATADISP                                                      
         MVI   ELCODE,ACLELQ                                                    
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING ACLELD,R4                                                        
LEDG15   SR    R5,R5                  SAVE OFF HEIRARCHY ELEM                   
         ICM   R5,3,LDGHEIR           ADDRESS TO SAVE ELEMENT                   
         AR    R5,RB                                                            
         XC    0(L'HEIR1R,R5),0(R5)   CLEAR IT OUT FIRST                        
         ZIC   R6,1(R4)               GET ELEMENT LENGTH AND MOVE               
         BCTR  R6,0                   IN ELEMENT                                
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)                                                    
                                                                                
         SR    R5,R5                  SAVE OFF CUMULATIVE LENGTHS               
         ICM   R5,3,LDGCUML           ADDRESS TO SAVE CUM LENGTHS               
         AR    R5,RB                                                            
         XC    0(L'CL1R,R5),0(R5)     CLEAR IT OUT FIRST                        
         LA    R6,ACLVALS                                                       
LEDG23   MVC   0(1,R5),0(R6)          MOVE IN LENGTHS                           
         CLI   0(R6),X'0C'                                                      
         BE    LEDG25                                                           
         LA    R5,1(R5)                                                         
         LA    R6,L'ACLVALS(R6)                                                 
         B     LEDG23                                                           
                                                                                
LEDG25   L     R3,AIO1                SAVE OFF INDIVIDUAL LENGTHS               
         SR    R5,R5                                                            
         ICM   R5,3,LDGLVAD           ADDRESS TO SAVE OFF INDIVIDUAL            
         AR    R5,RB                  LEVEL LENGTHS                             
         ST    R5,TEMPREG                                                       
         GOTO1 ACCDIV,DMCB,(R4),(R3),(R5)                                       
                                                                                
         LA    R6,4                                                             
         L     R5,TEMPREG             ACCDIV PASSES BACK LENGTHS MINUS          
LEDG26   CLI   0(R5),X'FF'            ONE SO BUMP IT UP TO GET TRUE             
         BNE   LEDG27                 LENGTH                                    
         MVI   0(R5),0                                                          
         B     LEDG28                                                           
LEDG27   ZIC   R4,0(R5)                                                         
         LA    R4,1(R4)                                                         
         STC   R4,0(R5)                                                         
LEDG28   LA    R5,LVENT(R5)                                                     
         BCT   R6,LEDG26                                                        
                                                                                
         LA    R2,LDGTLN(R2)          PROCESS NEXT LEDGER IN TABLE              
LEDG30   CLI   0(R2),X'FF'                                                      
         BNE   LEDG10                                                           
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ROUTINE TO FILL HEADLINES FOR USE IN HEADHOOK ROUTINE               *         
*---------------------------------------------------------------------*         
FILHEAD  NTR1                                                                   
         USING TIMRECD,R3                                                       
         LA    R3,SVRKEY                                                        
         MVC   TEMPCUL,TIMKCULA       COMP/U/L - USED FOR NAME READ             
         MVC   TEMPACC,TIMKACT        ACCOUNT - USED FOR NAME READ              
         LA    R4,HEIR1R              BREAK OUT 1R CODE INTO LEVELS             
         LA    R5,L1R                 TO COMPARE FOR LOGIC BREAKS               
         GOTO1 ACCDIV,DMCB,(R4),(R3),(R5)                                       
         LA    R5,L1R                                                           
         LA    R6,4                                                             
FILH005  ZIC   R4,0(R5)                                                         
         LA    R4,1(R4)                                                         
         STC   R4,0(R5)                                                         
         LA    R5,LVENT(R5)                                                     
         BCT   R6,FILH005                                                       
                                                                                
         NI    HDSTAT,X'FF'-NEWHILEV                                            
                                                                                
         CLI   HDSVCMPY,X'00'                                                   
         BE    FILH006                                                          
         CLC   HDSVCMPY,SVKCMPY                                                 
         BE    FILH007                                                          
         OI    HDSTAT,NEWHILEV                                                  
FILH006  MVC   HDSVCMPY,SVKCMPY                                                 
         B     FILH009                                                          
                                                                                
FILH007  EQU   *                                                                
         CLC   HDOFF,L1R1C            IF OFFICE HASN'T CHANGED DON'T            
         BE    FILH010                READ RECORD FOR NAME                      
FILH009  OI    HDSTAT,NEWHILEV                                                  
         MVC   HDOFF,L1R1C            SAVE OFF NEW CODE                         
         MVC   HDOFFN,SPACES          CLEAR NAME AREA                           
         ZIC   R5,CL1R1               LEVEL ONE LENGTH                          
         LA    R6,HDOFFN              NAME SAVE AREA                            
         BAS   RE,READACC             READ ACC                                  
                                                                                
FILH010  TM    HDSTAT,NEWHILEV                                                  
         BO    FILH011                                                          
         CLC   HDDEPT,L1R2C           HAS DEPARTMENT CHANGED                    
         BE    FILH020                                                          
         OI    HDSTAT,NEWHILEV                                                  
FILH011  MVC   HDDEPT,L1R2C           SAVE OFF NEW CODE                         
         MVC   HDDEPTN,SPACES         CLEAR NAME SAVE AREA                      
         ZIC   R5,CL1R2               LEVEL 2 LENGTH                            
         LA    R6,HDDEPTN             NAME SAVE AREA                            
         BAS   RE,READACC             READ ACC                                  
                                                                                
FILH020  TM    HDSTAT,NEWHILEV                                                  
         BO    FILH021                                                          
         CLC   HDSUBD,L1R3C           HAS SUB DEPT CHANGED                      
         BE    FILH030                                                          
         OI    HDSTAT,NEWHILEV                                                  
FILH021  MVC   HDSUBD,L1R3C           SAVE OFF NEW CODE                         
         MVC   HDSUBDN,SPACES         CLEAR NAME SAVE AREA                      
         ZIC   R5,CL1R3               LEVEL THREE LENGTH                        
         LA    R6,HDSUBDN             NAME SAVE AREA                            
         BAS   RE,READACC             READ ACC                                  
                                                                                
FILH030  TM    HDSTAT,NEWHILEV                                                  
         BO    FILH031                                                          
         CLC   HDPER,L1R4C            HAS PERSON CHANGED                        
         BE    FILH040                                                          
         OI    HDSTAT,NEWHILEV                                                  
FILH031  MVC   HDPER,L1R4C            SAVE OFF NEW CODE                         
         MVC   HDPERN,SPACES          CLEAR NAME SAVE AREA                      
         ZIC   R5,CL1R4               LEVEL FOUR LENGTH                         
         LA    R6,HDPERN              NAME SAVE AREA                            
         BAS   RE,READACC             READ ACC                                  
                                                                                
FILH040  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ ACCOUNT RECORDS FOR NAME                            *         
* R4 - FOR EXMVC OF KEY                                               *         
* R6 - SAVE AREA FOR NAME                                             *         
***********************************************************************         
         SPACE 1                                                                
READACC  NTR1                                                                   
         L     R1,AIO1                                                          
         MVC   0(56,R1),SPACES        CLEAR KEY TO SPACES                       
         MVC   0(3,R1),TEMPCUL        COMP/U/L                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),TEMPACC        MOVE IN ACCOUNT AND READ                  
         OC    1(41,R1),SPACES        DON'T CREAM COMPANY CODE                  
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCFIL,AIO1,AIO2                     
         TM    DMCB+8,X'10'                                                     
         BO    EXIT                                                             
         L     R4,AIO2                                                          
         AH    R4,DATADISP                                                      
         MVI   ELCODE,NAMELQ          GET NAME ELEMENT                          
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NAMELD,R4                                                        
         ZIC   R5,1(R4)                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),NAMEREC        SAVE OFF NAME                             
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        STARTING LOGOS                                               *         
*---------------------------------------------------------------------*         
STLOGOS  NTR1                                                                   
         MVI   LOGOFRST,1                                                       
                                                                                
         MVC   PAGE,=H'1'          RESET PAGE                                   
         L     R1,ACPYTAB                                                       
         USING CPYTABD,R1          R1=A(COMPANY TABLE)                          
STL010   CLI   CPYTABD,CPYTEOTQ    TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LGOCMPY,CPYTCPY     MATCH ON COMPANY CODE                        
         BE    *+12                                                             
         LA    R1,CPYTABL(R1)                                                   
         B     STL010                                                           
         MVC   ORIGINUM,CPYTUID    NEED CPY USERID FOR ALT SYSPRINT             
         ST    R1,ACPYTAB                                                       
         ST    R1,ACPYNTRY                                                      
                                                                                
         GOTO1 =A(OUTTRNTO),DMCB,AIO3,ORIGINUM                                  
         L     R7,ADBXAREA                                                      
                                                                                
         CLI   RCWRITE,C'N'        IF WRITE=NO WILL NOT PRINT LOGOS             
         BE    EXIT                NOR SEND REPORT TO AGENCIES QUE              
                                                                                
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   MCCTRY,CPYTCTRY                                                  
         DROP  RF                                                               
         DROP  R1                                                               
                                                                                
         L     R2,AIO1                                                          
         USING CTIREC,R2           R2=A(USER-ID RECORD)                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,LGOUSRID                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,CONFIL,CTIREC,CTIREC                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UIDLOG1,SPACES                                                   
         MVC   UIDLOG2,SPACES                                                   
         MVC   UIDNAME,SPACES                                                   
         MVC   UIDADDR,SPACES                                                   
                                                                                
         SR    R0,R0                                                            
         LA    R1,CTIDATA          LOCATE ORIGIN DETAILS ELEMENT                
         USING CTDSTD,R1                                                        
STL020   IC    R0,CTDSTLEN                                                      
         AR    R1,R0                                                            
         CLI   CTDSTEL,0           TEST E-O-R                                   
         BE    STL040                                                           
         CLI   CTDSTEL,CTDSTELQ    TEST DESTINATION DETAILS ELEMENT             
         BNE   STL020                                                           
         MVC   UIDLOG1,CTDSTLG1                                                 
         MVC   UIDLOG2,CTDSTLG2                                                 
         MVC   UIDNAME,CTDSTNAM                                                 
         MVC   UIDADDR,CTDSTADD                                                 
         CLI   CTDSTLEN,166        TEST LONG ELEMENT                            
         BL    STL030                                                           
         MVC   UIDADD2,CTDSTAD2    YES - GET REST OF ADDRESS/ATTN DETS          
         MVC   UIDADD3,CTDSTAD3                                                 
STL030   MVC   UIDJOBC,CTDSTPOW                                                 
         CLI   UIDJOBC+L'UIDJOBC-1,C' '                                         
         BNE   *+8                                                              
         MVI   UIDJOBC+L'UIDJOBC-1,C'X'                                         
         B     STL020                                                           
         DROP  R1,R2                                                            
                                                                                
STL040   CLI   RCPOSTNG,C'N'       TEST POSTING FILE REQUIRED                   
         BE    STL050                                                           
         XC    WRKID,WRKID         BUILD KEY OF WORKER FILE                     
         MVC   WRKIUID,SKUSRID                                                  
         MVI   WRKISYS,WRKISYSQ                                                 
         MVC   WRKIPRG,RCPROG                                                   
         MVC   WRKIDAY,TODAYP+2                                                 
         MVI   WRKITYP,WRKITYPQ                                                 
         L     R2,AIO2                                                          
         USING WRKRECD,R2          BUILD WORKER RECORD                          
         XC    WRKRECD(WRKRLENQ),WRKRECD                                        
         MVC   WRKRLEN,=Y(WRKRLENQ)                                             
         MVC   WRKRCPY,SKCMPY                                                   
         MVC   WRKRUID,SKUSRID                                                  
         MVC   WRKRLOGO,UIDLOG1                                                 
         L     R1,ACPYTAB                                                       
         CLC   UIDLOG1,SPACES                                                   
         BNE   *+10                                                             
         MVC   WRKRLOGO,CPYTLOGO-CPYTABD(R1)                                    
         GOTO1 DATAMGR,DMCB,WRKADD,WRKFIL,WRKID,WRKRECD,AWRKBUFF                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
STL050   NI    REPINDS,255-REPIREM SET REPORT NOT REMOTE                        
         L     R3,REMOTEC                                                       
         USING REMOTED,R3          R3=A(REMOTE CSECT)                           
         XC    REMOTKEY,REMOTKEY                                                
*                                                                               
         XC    SVREMOTE,SVREMOTE                                                
         LA    R4,SVREMOTE                                                      
SAVE     USING REMOTED,R4                                                       
         XC    WORK,WORK                                                        
         MVI   WORK,WRKISYSQ       SYSTEM CODE IS 'A' ACCOUNTING                
         MVC   WORK+1(2),RCPROG      PROGRAM                                    
         MVC   WORK+3(2),LGOUSRID                                               
         GOTO1 PQPROF,DMCB,(0,WORK),(0,SVREMOTE),ADCOMFAC                       
         MVC   REMOTTYP,SAVE.REMOTTYP                                           
         MVC   REMOTARC,SAVE.REMOTARC                                           
         MVC   REMOTTY1,SAVE.REMOTTY1                                           
         DROP  SAVE                                                             
*                                                                               
*&&UK                                                                           
         L     R2,AIO1                                                          
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,LGOUSRID                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,CONFIL,CTIREC,CTIREC                         
         BNE   STL090                                                           
         SR    R0,R0                                                            
         LA    R1,CTIDATA                                                       
         USING CTOCOD,R1                                                        
STL060   CLI   CTOCOEL,0                                                        
         BE    STL090                                                           
         CLI   CTOCOEL,CTOCOELQ                                                 
         BE    STL080                                                           
STL070   IC    R0,CTOCOLEN                                                      
         AR    R1,R0                                                            
         B     STL060                                                           
STL080   CLI   CTOCODE,C'#'                                                     
         BNE   STL070                                                           
         CLC   CTOCODE+1(L'CTOCODE-1),SPACES                                    
         BE    STL140                                                           
         LA    R0,L'CTOCODE-2                                                   
         LA    RE,CTOCODE+1                                                     
         CLI   0(RE),C'A'                                                       
         BE    STL140                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
         B     STL070                                                           
*&&                                                                             
*                                                                               
STL090   L     R2,AIO1                                                          
         USING CTPREC,R2           R2=A(PROFILE RECORD)                         
         XC    CTPKEY,CTPKEY                                                    
STL100   MVI   CTPKTYP,CTPKTYPQ                                                 
         MVI   CTPKSYS,WRKISYSQ                                                 
         MVC   CTPKPROG,RCPROG                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,CONFIL,CTPREC,CTPREC                         
         BNE   STL140                                                           
                                                                                
         SR    R0,R0                                                            
         LA    R1,CTPDATA          R1=A(FIRST ELEMENT)                          
         USING CTOCOD,R1                                                        
STL110   CLI   CTOCOEL,0           TEST E-O-R                                   
         BE    STL130                                                           
         CLI   CTOCOEL,CTOCOELQ    TEST OUTPUT CODE ELEMENT                     
         BNE   STL120                                                           
         MVC   OUTTYPE,CTOCODE     SAVE OFF OUTPUT TYPE                         
         B     STL130                                                           
STL120   IC    R0,CTOCOLEN         LOCATE OUTPUT TYPE CODE ELEMENT              
         AR    R1,R0                                                            
         B     STL110                                                           
*                                                                               
STL130   OC    CTPKORIG,CTPKORIG                                                
         BNZ   STL140                                                           
         XC    CTPKEY,CTPKEY                                                    
         MVC   CTPKORIG,LGOUSRID                                                
         B     STL100                                                           
*                                                                               
STL140   CLC   OUTTYPE(L'REMLIT),REMLIT                                         
         BNE   STL150                                                           
         OI    REPINDS,REPIREM     SET REPORT IS REMOTE                         
         MVI   REMOTFLG,X'FA'                                                   
         MVI   REMOTSYS,WRKISYSQ                                                
         MVC   REMOTPRG,RCPROG                                                  
         MVC   REMOTJID,REMOTSYS                                                
         MVC   REMOTDSC(8),AC@TSTAR                                             
         MVC   REMOTDST,LGOUSRID                                                
         MVI   REMOTCLS,C'Q'                                                    
                                                                                
         L     RE,ACPYNTRY         TEST REMOTE LOGO COMPANY                     
         TM    CPYTSTA8-CPYTABD(RE),CPYSRLOG                                    
         BZ    STL160                                                           
         DROP  R2,R3                                                            
                                                                                
STL150   L     R2,LOGOC            INITIALISE LOGOD & PRINT START LOGOS         
         USING LOGOD,R2                                                         
         MVC   LOGO1,UIDLOG1                                                    
         MVC   LOGO2,UIDLOG2                                                    
         MVC   LOGONAME,UIDNAME                                                 
         MVC   LOGOADD,UIDADDR                                                  
         MVC   LOGOADD2,UIDADD2                                                 
         MVC   LOGOADD3,UIDADD3                                                 
         MVC   LOGOIDNO,LGOUSRID                                                
         L     R1,ACPYNTRY                                                      
         USING CPYTABD,R1          SET DEFAULTS FROM COMPANY VALUES             
         CLC   LOGO1,SPACES                                                     
         BNE   *+10                                                             
         MVC   LOGO1,CPYTLOGO                                                   
         CLC   LOGONAME,SPACES                                                  
         BNE   *+10                                                             
         MVC   LOGONAME,CPYTNAME                                                
         CLC   LOGOADD,SPACES                                                   
         BNE   *+10                                                             
         MVC   LOGOADD,CPYTADDR                                                 
         MVC   LOGOJOB(L'UIDJOBC),UIDJOBC                                       
         MVC   LOGOJOB+L'UIDJOBC(3),AC@ATJ                                      
         DROP  R1                                                               
         MVI   LOGOTYPE,C'S'                                                    
         MVI   LOGOEND,C'X'                                                     
         MVC   BYTE,BOXYORN                                                     
         MVI   BOXYORN,C'N'                                                     
         GOTO1 LOGO,DMCB,LOGOD     PRINT START LOGOS                            
         MVC   BOXYORN,BYTE                                                     
                                                                                
STL160   DS    0H                                                               
         MVI   LINE,58                ENTIRE ENTRY, FORCE TO A NEW PAGE         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        ENDING LOGOS                                                 *         
*---------------------------------------------------------------------*         
ENLOGOS  NTR1                                                                   
         CLI   RCWRITE,C'N'        IF WRITE=NO WILL NOT PRINT LOGOS             
         BE    EXIT                NOR SEND REPORT TO AGENCIES QUE              
                                                                                
         L     RE,ACPYNTRY         TEST REMOTE LOGO COMPANY                     
         TM    CPYTSTA8-CPYTABD(RE),CPYSRLOG                                    
         BNZ   ENL10                                                            
                                                                                
         TM    REPINDS,REPIREM     TEST REMOTE REPORT                           
         BNZ   ENL022                                                           
                                                                                
ENL10    L     R2,LOGOC                                                         
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         MVC   BYTE,BOXYORN                                                     
         MVI   BOXYORN,C'N'                                                     
         GOTO1 LOGO,DMCB,LOGOD                                                  
         MVC   BOXYORN,BYTE                                                     
         B     ENL099                                                           
         DROP  R2                                                               
                                                                                
ENL022   GOTO1 PRINT,DMCB,SPACES,BC01                                           
         GOTO1 (RF),(R1),(L'PRTCLOSE,PRTCLOSE)                                  
                                                                                
ENL099   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        DICTIONARY ENTRIES                                           *         
*---------------------------------------------------------------------*         
DCLIST   DS    0C                                                               
         DCDDL AC#PERD,6,L                                                      
         DCDDL AC#TOTAL,5,L                                                     
         DCDDL AC#PRSN,6,L                                                      
         DCDDL AC#RPT,6,L                                                       
         DCDDL AC#TIME,4,L                                                      
         DCDDL AC#BLG,4,L                                                       
         DCDDL AC#RTIME,2,L                                                     
         DCDDL AC#NTIME,2,L                                                     
         DCDDL AC#BTIME,2,L                                                     
         DCDDL AC#WIND,2,L                                                      
         DCDDL AC#AIND,2,L                                                      
         DCDDL AC#TIND,2,L                                                      
         DCDDL AC#DEL,3,L                                                       
         DCDDL AC#ADD,3,L                                                       
         DCDDL AC#ORG,3,L                                                       
         DCDDL AC#CHG1,3,L                                                      
         DCDDL AC#BIA,13,L                                                      
         DCDDL AC#TSTAR,8,L                                                     
         DCDDL AC#ATJ,3,L                                                       
         DCDDL AC#ACT,3,L                                                       
         DCDDL AC#TYPE1,4,L                                                     
         DCDDL AC#HOURS,5,L                                                     
         DCDDL AC#CODE,4,L                                                      
         DCDDL AC#NAME,4,L                                                      
         DCDDL AC#TASK,4,L                                                      
         DCDDL AC#MOA,3,L                                                       
         DCDDL AC#RATE,4,L                                                      
         DCDDL AC#AMT,6,L                                                       
         DCDDL AC#ICR,16,L                                                      
         DCDDL AC#ERROR,5,L                                                     
         DCDDL AC#WC,8,L                                                        
         DCDDL AC#LOCAL,8,L                                                     
         DCDDL AC#PROG,4,L                                                      
         DCDDL AC#BASIS,5,L                                                     
         DCDDL AC#RSNAR,4,L                                                     
         DCDDL AC#BRAO,6,L                                                      
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
BOXRC    DC    A(0)                                                             
ACPYTAB  DC    A(CPYTAB)                                                        
ADUMP    DC    A(DUMP)             ADDRESS OF DUMP ROUTINE                      
ABINADD  DC    A(BINADD)           ADDRESS OF BINADD ROUTINE                    
ACHKDET  DC    A(CHKDET)           FILL DETAIL TABLE                            
APRTERR  DC    A(PRTERR)           PRINT OUT THE ERROR REPORT                   
ABXHOOK  DC    A(BXHOOK)           BOX ROUTINE                                  
ACPYNTRY DC    A(0)                                                             
AWRKBUFF DC    A(WRKBUFF)          A(WORKER BUFFER)                             
                                                                                
EMUOLDN  DC    C'OLDN'                                                          
ACCFIL   DC    C'ACCFIL '                                                       
PRTCLOSE DC    C'CLOSE'                                                         
WRKCLO   DC    C'CLO'                                                           
WRKADD   DC    C'ADD'                                                           
WRKFIL   DC    C'WKFILE '                                                       
BC01     DC    C'BC01'                                                          
CONFIL   DC    C'CTFILE '                                                       
REMLIT   DC    C'REMOTE'                                                        
YES      DC    C'Y'                                                             
                                                                                
HDSTAT   DC    X'00'                                                            
NEWHILEV EQU   X'80'                                                            
                                                                                
HDSVCMPY DC    X'00'                                                            
                                                                                
PPACCUMS DS    0C                                                               
PPHRS    DC    PL(L'PDHRS)'0'                                                   
PPAMT    DC    PL(L'PDAMT)'0'                                                   
                                                                                
PRACCUMS DS    0C                                                               
PRHRS    DC    PL(L'PDHRS)'0'                                                   
PRAMT    DC    PL(L'PDAMT)'0'                                                   
                                                                                
RTACCUMS DS    0C                                                               
RTHRS    DC    PL(L'PDHRS)'0'                                                   
RTAMT    DC    PL(L'PDAMT)'0'                                                   
                                                                                
ATYPES   DS    0A                                                               
HELLO    DC    V(HELLO)                                                         
ACCDIV   DC    V(ACCDIV)                                                        
PRNTBL   DC    V(PRNTBL)           PRINT DATA                                   
PQPROF   DC    V(PQPROF)           PRINT QUEUE PROFILE                          
*MN                                                                             
ACRECTYP DC    V(ACRECTYP)         GET ACC RECORD TYPE EQUATE                   
*MN                                                                             
         DC    X'FF'                                                            
                                                                                
LDGTAB   DS    0C                                                               
         DC    C'SJ',AL2(LSJ-TABGN),AL2(HEIRSJ-TABGN),AL2(CLSJ-TABGN)           
         DC    C'1N',AL2(L1N-TABGN),AL2(HEIR1N-TABGN),AL2(CL1N-TABGN)           
         DC    C'1R',AL2(L1R-TABGN),AL2(HEIR1R-TABGN),AL2(CL1R-TABGN)           
         DC    X'FF'                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,24,A),FORMAT=CH,WORK=1'                      
SORTRECD DC    CL80'RECORD TYPE=F,LENGTH=(2024,,,,)'                            
SORTSW   DC    AL1(0)              SORT ACTIVITY SWITCH                         
MASTDISP DC    H'56'                                                            
*                                                                               
LSJ      DS    0CL52                                                            
LSJ1     DS    CL1                                                              
LSJ1C    DS    CL12                                                             
LSJ2     DS    CL1                                                              
LSJ2C    DS    CL12                                                             
LSJ3     DS    CL1                                                              
LSJ3C    DS    CL12                                                             
LSJ4     DS    CL1                                                              
LSJ4C    DS    CL12                                                             
HEIRSJ   DS    CL66                                                             
CLSJ     DS    0CL4                                                             
CLSJ1    DS    CL1                                                              
CLSJ2    DS    CL1                                                              
CLSJ3    DS    CL1                                                              
CLSJ4    DS    CL1                                                              
                                                                                
L1N      DS    0CL52                                                            
L1N1     DS    CL1                                                              
L1N1C    DS    CL12                                                             
LVENT    EQU   *-L1N                                                            
L1N2     DS    CL1                                                              
L1N2C    DS    CL12                                                             
L1N3     DS    CL1                                                              
L1N3C    DS    CL12                                                             
L1N4     DS    CL1                                                              
L1N4C    DS    CL12                                                             
HEIR1N   DS    CL66                                                             
CL1N     DS    0CL4                                                             
CL1N1    DS    CL1                                                              
CL1N2    DS    CL1                                                              
CL1N3    DS    CL1                                                              
CL1N4    DS    CL1                                                              
                                                                                
L1R      DS    0CL52                                                            
L1R1     DS    CL1                                                              
L1R1C    DS    CL12                                                             
L1R2     DS    CL1                                                              
L1R2C    DS    CL12                                                             
L1R3     DS    CL1                                                              
L1R3C    DS    CL12                                                             
L1R4     DS    CL1                                                              
L1R4C    DS    CL12                                                             
HEIR1R   DS    CL66                                                             
CL1R     DS    0CL4                                                             
CL1R1    DS    CL1                                                              
CL1R2    DS    CL1                                                              
CL1R3    DS    CL1                                                              
CL1R4    DS    CL1                                                              
                                                                                
CPYTAB   DC    ((X'FE'-X'40')*CPYTABL)X'00'                                     
         EJECT                                                                  
**********************************************************************          
* BUFFERS                                                            *          
**********************************************************************          
         SPACE 1                                                                
         DS    0F                                                               
         DC    CL4'AIO1'                                                        
IO1      DS    XL2100              I/O AREA                                     
                                                                                
         DS    0F                                                               
         DC    CL4'AIO2'                                                        
IO2      DS    XL2100              SAVED COPY RECORD                            
                                                                                
         DS    0F                                                               
         DC    CL4'AIO3'                                                        
IO3      DS    XL2100              SAVED COPY RECORD                            
                                                                                
WRKBUFF  DS    4500C               BUFFER FOR WORKER                            
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     DS    0H                                                               
         NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CHECK FOR ANY ERRORS ON THIS LINE                                             
**********************************************************************          
         SPACE 1                                                                
CHKDET   DS    0H                                                               
         NMOD1 0,**CKD**                                                        
         LR    RC,R1                                                            
*                                                                               
         XC    ERRFLAG,ERRFLAG     CLEAR STATUS FLAG                            
*                                                                               
         USING BIND,R1                                                          
         L     R1,ADETTAB          R1=A(TIME DETAIL TABLE)                      
         ICM   R0,15,BININ                                                      
         BZ    CHKDX                                                            
         USING DETD,R6                                                          
         LA    R6,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         LA    RE,SRKEY            1R RECORD KEY                                
*                                                                               
CHKD10   CLC   DET1R,TIMKACT-TIMKEY(RE)           MATCH ON ACCOUNT              
         BNE   CHKD20                                                           
         CLC   DETPEDT,TIMKPEDT-TIMKEY(RE)        MATCH ON PERIOD               
         BNE   CHKD20                                                           
         OC    DETLN#(L'DETLN#+L'DETTLN#),DETLN#  ONLY TOTAL RECORDS?           
         BNZ   CHKD20                                                           
         CP    DETDHRS,PKMXHRS                                                  
         BNH   CHKD20              CHECK NEXT DAY                               
         OI    ERRFLAG,FLGHRS                                                   
         B     CHKDX                                                            
*                                                                               
CHKD20   LA    R6,DETLNQ(R6)                                                    
         BCT   R0,CHKD10                                                        
*                                                                               
CHKDX    DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ERROR REPORT FROM DETAIL TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PLINED,R2                                                        
PRTERR   DS    0H                                                               
         NMOD1 0,**PTE**                                                        
         LR    RC,R1                                                            
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    R2,XP                                                            
         MVI   RCSUBPRG,1             SET FOR ERROR REPORT                      
*                                                                               
         SR    RF,RF                  IF NOT ENOUGH LINES LEFT TO PRINT         
         IC    RF,LINE                ENTIRE ENTRY, FORCE TO A NEW PAGE         
         CH    RF,=H'43'                                                        
         BNH   PRTE10                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     PRTE20                                                           
*                                                                               
PRTE10   CH    RF,=H'11'              IF THIS IS THE FIRST ENTRY BOX            
         BNH   PRTE20                 WILL AUTOMATICALLY BE OPENED              
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
PRTE20   MVC   XP,XSPACES                                                       
         MVC   PDAY(3),=C'DAY'          DAY                                     
         MVC   PLIN#(9),=C'TMS LINE#'   TMS LINE NUMBER                         
         MVC   PTLIN#(8),=C'TMP LINE#'  TEMPO LINE NUMBER                       
         MVC   PDYHRS+4(5),AC@HOURS     HOURS                                   
         GOTO1 ACREPORT                                                         
*                                                                               
         SR    RF,RF                  AND INSERT A MIDLINE                      
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
         USING BIND,R1                                                          
         L     R1,ADETTAB          R1=A(TIME DETAIL TABLE)                      
         ICM   R3,15,BININ                                                      
         BZ    PRTEX                                                            
         USING DETD,R4                                                          
         LA    R4,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         XC    LSTDAY,LSTDAY       INITIALIZE LAST DAY FIELD                    
PRTE30   DS    0H                                                               
         MVC   XP,XSPACES                                                       
*        MVC   MSG,=CL10'DETWRK#3'                                              
*        GOTO1 ADUMP,DMCB,(RC),(R4),L'DETWRK                                    
*                                                                               
         LA    RE,SVRKEY                          1R RECORD KEY                 
*                                                                               
         CLC   DET1R,TIMKACT-TIMKEY(RE)           MATCH ON ACCOUNT              
         BNE   PRTE80                                                           
         CLC   DETPEDT,TIMKPEDT-TIMKEY(RE)        MATCH ON PERIOD               
         BNE   PRTE80                                                           
*                                                                               
         OC    LSTDAY,LSTDAY       1ST TIME THROUGH?                            
         BZ    PRTE40                                                           
         CLC   LSTDAY,DETDAY       SAME DAY AS BEFORE?                          
         BE    PRTE40                                                           
         GOTO1 ACREPORT            SKIP LINE                                    
PRTE40   MVC   LSTDAY,DETDAY       UPDATE LAST DAY FIELD                        
         GOTO1 HEXOUT,DMCB,DETDAY,PDAY,L'DETDAY                                 
         OC    DETLN#,DETLN#       IS THER A LINE NUMBER?                       
         BZ    PRTE50                                                           
         EDIT  DETLN#,PLIN#,ZERO=NOBLANK                                        
PRTE50   OC    DETTLN#,DETTLN#     IS THERE A TEMPO LINE NUMBER?                
         BZ    PRTE60                                                           
         EDIT  DETTLN#,PTLIN#,ZERO=NOBLANK                                      
PRTE60   EDIT  DETDHRS,PDYHRS,2,ZERO=NOBLANK,MINUS=YES                          
         OC    DETLN#,DETLN#       IS THER A LINE NUMBER?                       
         BNZ   PRTE70                                                           
         CP    DETDHRS,PKMXHRS                                                  
         BNH   *+8                                                              
         MVI   PDAY-1,C'*'            MARK DAY AS OVER THE LIMIT                
*                                                                               
PRTE70   SR    RF,RF                  IF NOT ENOUGH LINES LEFT TO PRINT         
         IC    RF,LINE                ENTIRE ENTRY, FORCE TO A NEW PAGE         
         CH    RF,=H'42'                                                        
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
PRTE80   LA    R4,DETLNQ(R4)                                                    
         BCT   R3,PRTE30                                                        
*                                                                               
PRTEX    DS    0H                                                               
         MVI   RCSUBPRG,0                SET FOR ERROR REPORT                   
         SR    RF,RF                     CLOSE BOX                              
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD HEADLINES                                          *         
***********************************************************************         
         SPACE 1                                                                
BXHOOK   DS    0H                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
*                                                                               
         CLC   TOTLIT(6),AC@RPT                                                 
         BE    BXHK50                                                           
         CLC   TOTLIT(6),AC@PRSN                                                
         BE    BXHK40                                                           
         CLI   RCSUBPRG,1                                                       
         BNE   BXHK10                                                           
         MVC   XHEAD3+93(L'ERRRPTL1),ERRRPTL1                                   
         MVC   XHEAD4+93(L'ERRRPTL2),ERRRPTL2                                   
         EDIT  PKMXHRS,(10,XHEAD4+93+L'ERRRPTL2),2                              
         B     BXHK30                                                           
*                                                                               
BXHK10   OC    ERRFLAG,ERRFLAG     DID WE GET ANY ERRORS?                       
         BZ    BXHK30                                                           
*                                                                               
         LA    RE,ERRTAB           PRINT OUT ERROR MESSAGE                      
BXHK20   CLI   0(RE),X'FF'                                                      
         BE    BXHK30                                                           
         MVC   MASK,0(RE)                                                       
         NC    MASK,ERRFLAG        IS THIS BIT ON?                              
         BNZ   *+12                                                             
         LA    RE,L'ERRTAB(RE)                                                  
         B     BXHK20                                                           
         MVC   XHEAD3+100(45),1(RE)                                             
*                                                                               
         USING TIMRECD,R3                                                       
BXHK30   LA    R3,SVRKEY                                                        
         GOTO1 DATCON,DMCB,(1,TIMKPEDT),(5,XHEAD2+10)  PERIOD                   
         DROP  R3                                                               
*                                                                               
BXHK40   MVC   XHEAD3+10(L'HDPER),HDPER             PERSON CODE                 
         MVC   XHEAD3+24(L'HDPERN),HDPERN           PERSON NAME                 
         MVC   XHEAD4+10(L'HDOFF),HDOFF             OFFICE CODE                 
         MVC   XHEAD4+24(L'HDOFFN),HDOFFN           OFFICD NAME                 
         MVC   XHEAD5+10(L'HDDEPT),HDDEPT           DEPARTMENT CODE             
         MVC   XHEAD5+24(L'HDDEPTN),HDDEPTN         DEPARTMENT NAME             
         MVC   XHEAD6+10(L'HDSUBD),HDSUBD           SUB DEPT CODE               
         MVC   XHEAD6+24(L'HDSUBDN),HDSUBDN         SUB DEPT NAME               
*                                                                               
BXHK50   L     RC,BOXRC                 SET UP BOX PARAMETERS                   
         L     R7,ADBXAREA                                                      
         USING BOXD,R7                                                          
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVC   BOXCOLS,XSPACES                                                  
         MVC   BOXCOLSR,XSPACES                                                 
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BNE   BXHK60              REGULAR REPORT                               
         MVI   BOXCOLS+(PTYPE-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PHRS-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PCODE-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PNAME-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PTASK-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PMOA-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PRATE-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PAMOUNT-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PINC-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
         B     BXHKX                                                            
*                                                                               
BXHK60   CLI   RCSUBPRG,1                                                       
         BNE   BXHKX               ERROR REPORT                                 
         MVI   BOXCOLS+(PLIN#-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PTLIN#-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PDYHRS-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+PLIN1LNQ,C'R'                                            
*                                                                               
BXHKX    MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+54,C'B'                                                  
         MVI   BOXCOLS+1,C'L'                                                   
         XIT1                                                                   
                                                                                
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* TABLE/CONSTANTS                                                    *          
**********************************************************************          
         SPACE 1                                                                
ERRTAB   DS    0C                                                               
         DC    X'80',CL45'*ERROR-T/S HAS DAY HRS THAT EXCEED MAX HRS*'          
         DC    X'FF'                                                            
*                                                                               
ERRRPTL1 DC    CL16'**ERROR REPORT**'                                           
ERRRPTL2 DC    CL45'FOR T/S WITH DAY HRS THAT EXCEED MAX DAY HRS-'              
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         L     R2,4(R1)            IO AREA                                      
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,(R2),0             
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         L     R2,4(R1)            IO AREA                                      
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,(R2),0             
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         L     R2,4(R1)            IO AREA                                      
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,(R2),0             
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R2                                                       
         L     R2,4(R1)                                                         
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,(R2),DMWORK                
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         L     R2,4(R1)            IO AREA                                      
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',ACCKDA,(R2),DMWORK          
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
*                                                                               
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         SR    R6,R6                                                            
         IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
BINA10   AP    0(L'DETDBKT,R4),0(L'DETDBKT,R3) ADD TO BUCKET                    
         LA    R3,L'DETDBKT(R3)    BUMP TO NEXT ENTRY IN NEW ITEM               
         LA    R4,L'DETDBKT(R4)    BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,BINA10                                                        
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACOTRNTO                                                       
**********************************************************************          
* DSECT FOR WORKING STORAGE                                          *          
**********************************************************************          
         SPACE 1                                                                
ACTJD    DSECT                     ** LOCAL WORKING STORAGE **                  
REPINDS  DS    XL1                 REPORT INDICATOR                             
REPIREM  EQU   X'80'               REMOTE FOR THIS REPORT                       
REPICNS  EQU   X'40'               CONSOLIDATED REPORT                          
*                                                                               
TODAYP   DS    PL3                 TODAYS DATE                                  
LOGOFRST DS    CL1                 IS THIS FIRST LOGO PRINT                     
LKEY     DS    XL(SKCOMPLN)        LAST SORTED RECORD KEY                       
SVCMPY   DS    XL(L'SKCMPY)        COMPANY CODE ON LAST SORT RECORD             
LGOCMPY  DS    CL1                                                              
LGOUSRID DS    CL2                                                              
CTRY     DS    XL1                 SAVED DDS COUNTRY  CODE                      
TEMPCUL  DS    CL3                 PASS U/L TO LEDGER READ ROUTINE              
TEMPACC  DS    CL12                PASS ACCOUNT TO READ ROUTINE                 
TOTLIT   DS    CL12                TOTAL LITERAL FOR PRINT                      
SVREMOTE DS    CL(REMOTEDL)        SAVED REMOTE BLOCK                           
OUTTYPE  DS    CL(L'CTOCODE)                                                    
*                                                                               
MASK     DS    XL1                 MASK FIELD FOR ERROR TABLE COMPARE           
ERRFLAG  DS    XL1                 FLAG TO SHOW IF HOURS EXCEED 24              
FLGHRS   EQU   X'80'               HOURS HAS EXCEEDED 24 HOURS                  
*                                                                               
PKMXHRS  DS    PL6                 MAXIMUM AMOUNT OF HOURS                      
*                                                                               
LSTDAY   DS    XL1                 LAST DAY FIELD-USED IN PRTERR RTE            
*                                                                               
COMPBYTE DS    CL1                 IF SOON REQUEST - LENGTH OF ACCOUNT          
COMPACCT DS    CL(L'QACCOUNT)      SOON ACCOUNT REQUESTED                       
*                                                                               
SVKEY    DS    CL49                SAVED AREA FOR IO                            
MSG      DS    CL10                MESSAGE FOR DUMP ROUTINE                     
*                                                                               
PROFILES DS    0CL16               PROFILES                                     
PRDHRS   DS    XL1                 PROFILE 1                                    
PROF2    DS    CL1                 PROFILE 2                                    
PROF3    DS    CL1                 PROFILE 3                                    
PROF4    DS    CL1                 PROFILE 4                                    
PROF5    DS    CL1                 PROFILE 5                                    
PROF6    DS    CL1                 PROFILE 6                                    
PROF7    DS    CL1                 PROFILE 7                                    
PROF8    DS    CL1                 PROFILE 8                                    
PROF9    DS    CL1                 PROFILE 9                                    
PROF10   DS    CL1                 PROFILE 10                                   
PROF11   DS    CL1                 PROFILE 11                                   
PROF12   DS    CL1                 PROFILE 12                                   
PROF13   DS    CL1                 PROFILE 13                                   
PROF14   DS    CL1                 PROFILE 14                                   
PROF15   DS    CL1                 PROFILE 15                                   
PROF16   DS    CL1                 PROFILE 16                                   
*                                                                               
COMMAND  DS    CL8                 FOR DATAMGR READS                            
ELCODE   DS    CL1                                                              
SVTIMLN# DS    CL(L'TIMLINE#)                                                   
RELO     DS    F                                                                
SRTSEQ   DS    F                                                                
TEMPREG  DS    F                                                                
SXSRTADD DS    F                                                                
CNTELS   DS    F                                                                
CURRXSRT DS    F                                                                
SVELADDR DS    F                                                                
*                                                                               
MARKDUP  EQU   X'FF'               MARK ELEMENT CLUSTERS AS DUPLICATES          
MARKCPY  EQU   X'FE'               COPIES                                       
MARKCHA  EQU   X'FD'               CHANGES                                      
MARKDEL  EQU   X'FC'               DELETES                                      
MARKADD  EQU   X'FB'               OR ADDS                                      
*                                                                               
SORTMAX  EQU   20000                                                            
DETMAX   EQU   10000                                                            
*                                                                               
DETWRK   DS    CL(DETLNQ)          DETAIL TABLE WORK AREA                       
SVLIN#   DS    XL2                 SAVED AREA FOR LINE NUMBER                   
*                                                                               
UIDNAME  DS    CL(L'CTORGNAM)      USER-ID NAME                                 
UIDADDR  DS    CL(L'CTORGADD)      USER-ID ADDRESS                              
UIDADD2  DS    CL(L'CTORGADD)      USER-ID ADDRESS LINE 2                       
UIDADD3  DS    CL(L'CTORGADD)      USER-ID ADDRESS LINE 3                       
UIDLOG1  DS    CL(L'CTDSTLG1)      USER-ID LOGO 1                               
UIDLOG2  DS    CL(L'CTDSTLG2)      USER-ID LOGO 2                               
UIDJOBC  DS    CL(L'CTDSTPOW)      JES JOB CODE                                 
*                                                                               
HDPER    DS    CL12                SAVE CODES AND NAMES TO USE TO               
HDPERN   DS    CL36                FILL IN HEADLINES                            
HDOFF    DS    CL12                                                             
HDOFFN   DS    CL36                                                             
HDDEPT   DS    CL12                                                             
HDDEPTN  DS    CL36                                                             
HDSUBD   DS    CL12                                                             
HDSUBDN  DS    CL36                                                             
HDREF    DS    CL6                                                              
HDLEN    EQU   *-HDPER                                                          
*                                                                               
WRKID    DS    0XL16               ** WORKER KEY **                             
WRKIUID  DS    XL2                 USER-ID NUMBER                               
WRKISYS  DS    CL1                 SYSTEM                                       
WRKISYSQ EQU   C'A'                                                             
WRKIPRG  DS    CL2                 PROGRAM CODE                                 
         DS    XL1                 N/D                                          
WRKIDAY  DS    PL1                 DAY (PWOS)                                   
WRKITYP  DS    CL1                 WORKER FILE TYPE                             
WRKITYPQ EQU   C'O'                                                             
         DS    XL8                 REST OF WORKER INDEX ENTRY                   
*                                                                               
AIO1     DS    A                   IO AREAS FOR DATAMGR READS                   
AIO2     DS    A                                                                
AIO3     DS    A                                                                
*                                                                               
MAINLEN  DS    F                   LENGTH OF AREA                               
MAINBGN  DS    A                   A(START) OF GETMAIN                          
ADETTAB  DS    A                   DETAIL RECORD (TSIREC) TABLE                 
*                                                                               
SVXSRTK  DS    CL(XSRKLEN)         SAVE FILE SORT KEY FOR BREAK CHECK           
*                                                                               
SVEL1O   DS    CL(XTABLEN)         SAVE ELEMENT TIME ELEMENTS IN                
SVEL2O   DS    CL(XTABLEN)         CLUSTERS FOR COMPARISON - THIS WOULD         
SVEL3O   DS    CL(XTABLEN)         BE COPY GROUP                                
*                                                                               
SVEL1N   DS    CL(XTABLEN)         THIS WOULD BE CHANGE GROUP OR                
SVEL2N   DS    CL(XTABLEN)         ADD GROUP                                    
SVEL3N   DS    CL(XTABLEN)                                                      
*                                                                               
DSLIST   DS    0C                  PRINT LIST OF DICTIONARY ENTRIES             
         DSDDL PRINT=YES                                                        
*                                                                               
         DS    0F                                                               
SREC     DS    0C                  ** SORT RECORD **                            
SKKEY    DS    0CL23               KEY I'M SORTING ON                           
SKCMPY   DS    XL1                 COMPANY CODE                                 
SKUSRID  DS    XL2                 USER ID                                      
SKPRSPD  DS    CL15                PERSON CODE                                  
SKCOMPLN EQU   *-SREC                                                           
SKSEQ    DS    XL4                 INTERNAL SORT SEQUENCING                     
SKTYPE   DS    CL1                 RECOVERY RECORD TYPE (ADD,CPY,CHA)           
SKPRG    DS    CL1                 INPUT PROGRAM                                
SKLNQ    EQU   *-SREC                                                           
SRCVREC  DS    0CL2000             ACTUAL TIME RECORD                           
SRKEY    DS    CL42                KEY OF TIME RECORD                           
SRELEMS  DS    CL1958              BODY OF TIME RECORD                          
SRLNQ    EQU   *-SREC              LENGTH OF SORT RECORD                        
*                                                                               
         DS    0F                                                               
SVREC    DS    0X                  ** SAVE SORT RECORD **                       
SVKCMPY  DS    XL1                 COMPANY                                      
SVKUSRID DS    XL2                 USER ID                                      
SVKPRSPD DS    CL15                PERSON KEY                                   
SVKSEQ   DS    XL4                 INTERNAL SEQUENCE NUMBER                     
SVKTYPE  DS    CL1                 RECOVERY REC TYPE (ADD,CPY,CHA)              
SVKPRG   DS    CL1                 INPUT PROGRAM                                
SVRCVREC DS    0CL2000             ACTUAL TIME RECORD                           
SVRKEY   DS    CL42                KEY OF TIME RECORD                           
SVRELEMS DS    CL1958              BODY OF TIME RECORD                          
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER GETPROFS KEY                                        *          
**********************************************************************          
         SPACE 1                                                                
PROFKD   DSECT                                                                  
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                 NEW OFFICE                                   
         EJECT                                                                  
**********************************************************************          
* DSECT FOR LEDGER TABLE                                             *          
**********************************************************************          
         SPACE 1                                                                
LDGTABD  DSECT                                                                  
LDG      DS    CL2                 LEDGER TO BE READ                            
LDGLVAD  DS    CL2                 ADDRESS TO STORE LEVELS AT                   
LDGHEIR  DS    CL2                 ADDRESS TO STORE HEIRARCHY ELEMENT           
LDGCUML  DS    CL2                 ADDRESS TO STORE CUMULATIVE LENGTHS          
LDGTLN   EQU   *-LDGTABD           TABLE ENTRY LENGTH                           
         EJECT                                                                  
**********************************************************************          
* DSECT FOR COMPANY TABLE                                            *          
**********************************************************************          
         SPACE 1                                                                
CPYTABD  DSECT                     ** COMPANY TABLE **                          
CPYTEOTQ EQU   0                   END OF TABLE INDICATOR                       
CPYTCPY  DS    XL(L'CPYKCPY)       COMPANY CODE                                 
CPYTUID  DS    XL(L'CPYUID)        PRINCIPAL USER-ID NUMBER                     
CPYTSTA1 DS    XL1                 STATUS BYTE 1                                
CPYTSTA2 DS    XL1                 STATUS BYTE 2                                
CPYTSTA3 DS    XL1                 STATUS BYTE 3                                
CPYTSTA4 DS    XL1                 STATUS BYTE 4                                
CPYTSTA8 DS    XL1                 STATUS BYTE 8                                
CPYTALPH DS    XL(L'CPYALPHA)      ALPHA-ID                                     
CPYTINDS DS    XL1                 COMPANY INDICATORS                           
CPYTIMUQ EQU   X'01'               MIXED USER-IDS IN RECOVERY FILE              
CPYTLOGO DS    XL(L'CPYLOGO)       COMPANY LOGO                                 
CPYTNAME DS    XL(L'NAMEREC)       COMPANY NAME                                 
CPYTADDR DS    XL(L'ADRADD1)       COMPANY ADDRESS LINE 1                       
CPYPRDAL DS    XL1                 PRODUCTION LEDGER L'CLIENT                   
CPYPRDBL DS    XL1                                   L'PRODUCT                  
CPYPRDCL DS    XL1                                   L'JOB                      
CPYTCTRY DS    XL1                 COMPANY COUNTRY CODE                         
CPYTABL  EQU   *-CPYTABD                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT FOR ACCUMULATOR TABLE                                        *          
**********************************************************************          
         SPACE 1                                                                
PDACCUMS DSECT                                                                  
PDHRS    DS    PL4                                                              
PDAMT    DS    PL6                                                              
         EJECT                                                                  
***********************************************************************         
* DSECT FOR DETAIL RECORD TABLE                                       *         
***********************************************************************         
         SPACE 1                                                                
DETD     DSECT                                                                  
DET1R    DS    CL12                   1R ACCOUNT                                
DETPEDT  DS    CL3                    PERIOD END DATE                           
DETDAY   DS    XL1                    DAY                                       
DETLN#   DS    XL2                    TMS LINE NUMBER                           
DETTLN#  DS    XL2                    TEMPO LINE NUMBER                         
DETKLNQ  EQU   *-DETD                                                           
DETDBKT  DS    0PL6                   START OF DAY BUCKETS                      
DETDHRS  DS    PL6                    DAY HOURS                                 
DETDBNUM EQU   (*-DETDBKT)/L'DETDBKT  NUMBER OF BUCKETS                         
DETLNQ   EQU   *-DETD                 RECORD LENGTH                             
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
**********************************************************************          
* DSECT FOR PRINT LINE                                               *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL2                                                              
PACT     DS    CL3                                                              
         DS    CL1                                                              
PTYPE    DS    CL1                                                              
         DS    CL4                                                              
PHRS     DS    CL7                                                              
         DS    CL1                                                              
PCODE    DS    CL12                                                             
         DS    CL1                                                              
PNAME    DS    CL36                                                             
         DS    CL1                                                              
PTASK    DS    CL15                                                             
         DS    CL1                                                              
PMOA     DS    CL6                                                              
         DS    CL1                                                              
PRATE    DS    CL8                                                              
         DS    CL1                                                              
PAMOUNT  DS    CL12                                                             
         DS    CL1                                                              
PINC     DS    CL14                                                             
         DS    CL5                                                              
PLINELNQ EQU   *-PRTLINE                                                        
         ORG   PACT                                                             
         DS    CL1                 MARK AS ERROR                                
PDAY     DS    CL2                 DETAIL DAY                                   
         DS    CL5                                                              
PLIN#    DS    CL5                 TMS LINE NUMBER                              
         DS    CL5                                                              
PTLIN#   DS    CL5                 TEMPO LINE NUMBER                            
         DS    CL5                                                              
PDYHRS   DS    CL10                DAY HOURS                                    
         DS    CL5                                                              
PLIN1LNQ EQU   *-PRTLINE                                                        
         ORG   PTYPE                                                            
PID      DS    CL8                                                              
         ORG   PCODE+6                                                          
PNARR    DS    CL100                                                            
         EJECT                                                                  
**********************************************************************          
* XSORT DSECT                                                        *          
**********************************************************************          
         SPACE 1                                                                
XSRTD    DSECT                                                                  
XSRTSTRT DS    0C                                                               
XSRTKEY  DS    0CL8              SORTED PORTION OF KEY                          
XTIMLIN  DS    CL2                 TIME LINE NUMBER                             
XSEQNM   DS    CL4                 MY OWN SEQUENCE NUMBER                       
XRCVTYP  DS    CL1                 RECORD TYPE                                  
XTIMTYP  DS    CL1                 TIME INPUT TYPE                              
XSRKLEN  EQU   *-XSRTD           OTHER INFORMATION NOT IN ELEMENT               
XRCVPRG  DS    CL1                 PROGRAM OF ORIGIN                            
XCNTRA   DS    CL14                CONTRA ACCOUNT                               
XSTAT    DS    CL1                 STATUS BYTE                                  
XDHRS    EQU   X'80'                 DAY HOURS EXCEED 24 HOURS                  
XELSTRT  EQU   *-XSRTD           DISPLACEMENT OF ELEMENT INTO SORTREC           
XELEM    DS    CL100               ACTUAL ELEMENT                               
XTABLEN  EQU   *-XSRTD                                                          
         EJECT                                                                  
**********************************************************************          
* WORKER FILE DSECT                                                  *          
**********************************************************************          
         SPACE 1                                                                
WRKRECD  DSECT                     ** WORKER RECORD **                          
WRKRLEN  DS    XL2                 RECORD LENGTH                                
         DS    XL2                 N/D                                          
WRKRCPY  DS    XL1                 COMPANY CODE                                 
WRKRUID  DS    XL2                 USER-ID NUMBER                               
WRKRLOGO DS    CL(L'UIDLOG1)       LOGO VALUE                                   
WRKRLENQ EQU   *-WRKRECD                                                        
         EJECT                                                                  
**********************************************************************          
* INCLUDE                                                            *          
**********************************************************************          
         SPACE 1                                                                
* ACRCVRECD                                                                     
* ACGENBOTH                                                                     
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* ACRECEQUS                                                                     
* DDLOGOD                                                                       
* DDREMOTED                                                                     
* DDEBLOCK                                                                      
* ACDDEQUS                                                                      
* ACREPWORKD                                                                    
* CTGENFILE                                                                     
* DDBIGBOX                                                                      
* ACBIGPRNTD                                                                    
* DDMASTD                                                                       
* DDACTIVD                                                                      
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACRECEQUS                                                      
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDEBLOCK                                                       
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREPTJ02 09/28/16'                                      
         END                                                                    
