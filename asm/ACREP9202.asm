*          DATA SET ACREP9202  AT LEVEL 016 AS OF 06/03/15                      
*PHASE AC9202A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE SORTER                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'MONTHLY DIRECT TIME REPORT'                                     
AC9202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC9202,R8,R9,RR=R5                                           
         L     RC,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RC          RC=A(GLOBAL W/S)                             
         USING WORKD,RA            RA=A(TEMP W/S)                               
*                                                                               
         LA    RA,SPACEND                                                       
         ST    R5,RELO                                                          
*                                  HANDLE MODES                                 
         CLI   MODE,PROCHIST       *                                            
         BE    TR2                                                              
         CLI   MODE,SBACFRST       *                                            
         BE    SB2                                                              
         CLI   MODE,SBACLAST       *                                            
         BE    SB4                                                              
         CLI   MODE,PROCACC        *                                            
         BE    AC2                                                              
         CLI   MODE,ACCLAST        *                                            
         BE    SB4                                                              
         CLI   MODE,LEVCFRST       *                                            
         BE    AC8                                                              
         CLI   MODE,LEVBFRST       *                                            
         BE    AC4                                                              
         CLI   MODE,LEVAFRST       *                                            
         BE    AC6                                                              
         CLI   MODE,LEDGFRST       *                                            
         BE    LE2                                                              
         CLI   MODE,REQFRST        *                                            
         BE    RE2                                                              
         CLI   MODE,REQLAST        *                                            
         BE    RE8                                                              
         CLI   MODE,RUNFRST        *                                            
         BE    RU2                                                              
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT ,                                                                
*              RUNFRST - INITIALIZE                                             
*                                                                               
RU2      LA    R1,ADCONS           RELOCATE ADCONS                              
         LA    RE,VADCONS                                                       
         RELOC (RF)                                                             
*                                                                               
RU4      CLI   0(R1),X'FF'                                                      
         BE    RU6                                                              
         L     R0,0(,R1)                                                        
         AR    R0,RF                                                            
         ST    R0,0(,RE)                                                        
         LA    R1,4(,R1)                                                        
         LA    RE,4(,RE)                                                        
         B     RU4                                                              
*                                                                               
RU6      GOTO1 BUFFALO,DMCB,=C'SET',VBUFF                                       
         L     RF,VBUFF                                                         
*                                                                               
         USING BUFFALOD,RF                                                      
*                                                                               
         OI    BUFFXOPT,BUFFXBIG   DON'T COMPRESS LARGE BINARY NUMBERS          
         B     EXIT                                                             
*                                                                               
         DROP  RF                                                               
*                                                                               
ADCONS   DC    V(SORTER)                                                        
         DC    V(BUFFALOC)                                                      
         DC    A(ACCBUFF)                                                       
         DC    V(SQUASHER)                                                      
         DC    V(UNDERLIN)                                                      
         DC    A(MEMOPOOL)                                                      
         DC    V(GETBROAD)                                                      
         DC    V(PRNTBL)                                                        
         DC    A(KEYSPLIT)                                                      
         DC    A(HNTDATE)                                                       
         DC    A(IOAREA)                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*              REQFRST - INITIALIZE SORT/BUILD DATE TABLE                       
*                                                                               
RE2      GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
         MVI   SORTSW,C'N'                                                      
*                                                                               
         CLI   QOPT10,C'N'         ARE WE ALLOWED TO SEE DOLLARS?               
         BNE   *+8                                                              
         MVI   QOPT3,C'Y'          FORCE SUPPRESSION OF $                       
*                                                                               
         MVI   METHOD,C'1'         *** SET METHOD TO DEFAULT ****               
         CLI   QMTHD,X'40'                                                      
         BNH   RE3                                                              
         MVC   METHOD,QMTHD        METHOD FROM REQUEST                          
*                                                                               
RE3      XC    DATAB,DATAB                                                      
         LA    R2,DATAB                                                         
*                                  **** START     OF   COMPANY                  
*                                  **** FINANCIAL YEAR                          
         MVC   WORK(4),QEND        GET  END  DATE                               
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(0,EDATEC)    CHAR END  DATE YYMM01         
         GOTO1 DATCON,DMCB,(0,EDATEC),(1,WORK+6)  PACKED    END  DATE           
         MVC   0(1,R2),WORK+6      GET  YEAR                                    
         MVI   1(R2),1             DEFAULT   MON  TO   JANUARY                  
         CLC   QSTART,SPACES       ANY  START     DATE ?                        
         BE    RE4                 NO,  SKIP                                    
         MVC   WORK(4),QSTART      GET  START     DATE                          
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)    PACKED    START               
         MVC   0(2,R2),WORK+6      SAVE START     DATE                          
*                                                                               
*                                  **** 12   MONTHS                             
*                                  **** ENDING    WITH END  DATE                
RE4      MVC   WORK(6),EDATEC                          END  DATE                
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'-11'    START     MONTH          
         LA    R3,12                                   SET  LOOP COUNT          
*                                                                               
RE5      LA    R2,2(,R2)                                                        
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK+12)      PACKED                   
         MVC   0(2,R2),WORK+12                         SAVE YYMM                
         MVC   WORK(6),WORK+6                          CHAR YYMM01              
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'1'      NEXT MONTH               
         BCT   R3,RE5                                  LOOP                     
*                                                                               
*                                  **** GET  BROADCAST WEEKS                    
*                                  **** FOR  EACH MONTH                         
         LA    R2,DATAB+2                                                       
         LA    R3,BRDTAB                                                        
         LA    R0,12                                                            
*                                                                               
RE6      MVC   WORK(2),0(R2)       GET  YYMM PACKED                             
         MVI   WORK+2,X'15'        DAY  15                                      
         GOTO1 DATCON,DMCB,(1,WORK),(0,DUB)            CHAR YYMM15              
         GOTO1 VGETBRD,DMCB,(X'FF',DUB),WORK,GETDAY,ADDAY                       
         CLI   0(R1),X'FF'         VALID     DATE ?                             
         BNE   RE7                 YES, CONTINUE                                
         DC    H'0'                NO,  DUMP                                    
*                                                                               
RE7      MVC   0(1,R3),0(R1)       DATE LIES IN   4/5  WEEK BROAD MONTH         
         LA    R2,2(,R2)           GET  NEXT MONTH                              
         LA    R3,1(,R3)           GET  NEXT NUM  OF   WEEKS                    
         BCT   R0,RE6              LOOP                                         
*                                                                               
         MVC   PAGE,=H'1'                                                       
         B     EXIT                                                             
*                                                                               
SORTCARD DC    C'SORT FIELDS=(1,46,A),FORMAT=BI,WORK=1 '                        
RECCARD  DC    C'RECORD TYPE=F,LENGTH=(144,,144) '                              
         EJECT ,                                                                
*              LEDGFRST - EXTRACT HEIRARCY INFO                                 
*                                                                               
LE2      L     R2,ADLDGHIR         PERSONEL LEDGER                              
         LA    R3,PERHEIR                                                       
         LA    R4,PERLEV                                                        
         BAS   RE,SETHEIR                                                       
         CLI   PROGPROF,C'Y'       FUDGE HEIRARCHY TO IGNORE SUB-DEPTS          
         BNE   LE3                                                              
         MVC   PERHEIR+8(4),PERHEIR+12                                          
         XC    PERHEIR+12(4),PERHEIR+12                                         
         MVC   PERHEIR+20(4),PERHEIR+24                                         
         XC    PERHEIR+24(4),PERHEIR+24                                         
*                                                                               
LE3      CLI   PROGPROF+5,C'Y'       FUDGE HEIRARCHY TO IGNORE DEPTS            
         BNE   LE4                                                              
         MVC   PERHEIR+4(8),PERHEIR+8                                           
         XC    PERHEIR+12(4),PERHEIR+12                                         
         MVC   PERHEIR+16(8),PERHEIR+20                                         
         XC    PERHEIR+24(4),PERHEIR+24                                         
         MVC   PERHEIR+32(4),PERHEIR+36                                         
         XC    PERHEIR+36(4),PERHEIR+36                                         
*                                                                               
LE4      MVC   READKEY,SPACES      CLIENT LEDGER                                
         MVC   READKEY(1),RCCOMPFL                                              
         MVC   READKEY+1(2),=C'1C'                                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',READKEY,AIOAREA              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA                                                       
         AH    R2,DATADISP                                                      
         SR    R1,R1                                                            
*                                                                               
LE5      CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R2),X'16'                                                      
         BE    LE6                                                              
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     LE5                                                              
*                                                                               
LE6      LA    R3,CLIHEIR                                                       
         LA    R4,CLILEV                                                        
         BAS   RE,SETHEIR                                                       
         MVC   READKEY,SPACES      RE-POSITION FILE                             
         MVC   READKEY(L'KEY),KEY                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',READKEY,AIOAREA              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT ,                                                                
*              LEVAFRST - EXTRACT NAME                                          
*                                                                               
AC4      L     R1,ADHEIRA                                                       
         BAS   RE,GETNAME                                                       
         B     EXIT                                                             
*                                                                               
*              LEVBFRST - EXTRACT NAME                                          
*                                                                               
AC6      L     R1,ADHEIRB                                                       
         BAS   RE,GETNAME                                                       
         B     EXIT                                                             
*                                                                               
*              LEVCFRST - EXTRACT NAME                                          
*                                                                               
AC8      L     R1,ADHEIRC                                                       
         BAS   RE,GETNAME                                                       
         B     EXIT                                                             
*                                                                               
*              PROCACC - EXTRACT NAME                                           
*                                                                               
AC2      MVI   THISACC,C'N'                                                     
         XC    PERSACCS,PERSACCS                                                
         XC    TOTACCS,TOTACCS                                                  
         MVI   FIRST,C'Y'                                                       
         L     RE,VMEMO            CLEAR MEMO POOL                              
         XC    0(4,RE),0(RE)                                                    
*&&UK                                                                           
         LA    R1,PERHEIR+12       IGNORE EXPENSE ACCOUNTS                      
         CLI   1(R1),0                                                          
         BNE   *+12                                                             
         SH    R1,=H'4'                                                         
         B     *-12                                                             
         ZIC   RE,0(,R1)                                                        
         L     R1,ADACC                                                         
         LA    R1,4(RE,R1)                                                      
         CLI   0(R1),C' '                                                       
         BE    EXIT                                                             
*&&                                                                             
         MVI   THISACC,C'Y'                                                     
         L     R1,ADACC                                                         
         BAS   RE,GETNAME                                                       
         MVC   PERACCT,0(R1)                                                    
         B     EXIT                                                             
         EJECT ,                                                                
*              SBACFRST - EXTRACT NAME                                          
*                                                                               
         USING TRSUBHD,R2                                                       
*                                                                               
SB2      CLI   THISACC,C'Y'                                                     
         BNE   EXIT                                                             
         L     R2,ADSUBAC                                                       
*&&US                                                                           
         CLI   QCOMPANY,C'8'       SPECIAL FIX FOR SCALI                        
         BNE   *+8                                                              
         BAS   RE,FIXSCA                                                        
*&&                                                                             
*                                                                               
         MVC   CLIACCT,TRSBACNT                                                 
         XC    BUFFREC,BUFFREC                                                  
         MVC   BUFFKEY,TRSBACNT                                                 
         MVC   BUFFNAME,SPACES                                                  
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         BM    SB2A                                                             
         EXMVC R1,BUFFNAME,TRSBNAME                                             
*                                                                               
SB2A     BAS   RE,ADDBUFF                                                       
         XC    SORTREC,SORTREC     READY FOR TRANSACTIONS                       
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
*              TEMP FIX FOR SCALI                                               
*CAN'T BE REMOVED UNTIL SCALI HAS COMPLETED ALL 1985 ALLOCATIONS                
*&&US                                                                           
*                                                                               
         USING TRSUBHD,R2                                                       
*                                                                               
FIXSCA   NTR1                                                                   
         LA    R1,SCALST                                                        
*                                                                               
FIXS1    CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         CLC   TRSBACNT+3(5),0(R1)     CHANGE OFFICE INDIRECT                   
         BE    FIXS3               TO OFFICE OVERHEAD                           
         LA    R1,5(,R1)                                                        
         B     FIXS1                                                            
*                                                                               
FIXS3    MVI   TRSBACNT+3,C'T'                                                  
         B     EXIT                                                             
*                                                                               
SCALST   DC    C'O 100'                                                         
         DC    C'O 101'                                                         
         DC    C'O 200'                                                         
         DC    C'O 201'                                                         
         DC    C'O 202'                                                         
         DC    C'O 203'                                                         
         DC    C'O 204'                                                         
         DC    C'O 205'                                                         
         DC    C'FF'                                                            
*                                                                               
         DROP  R2                                                               
*&&                                                                             
         EJECT ,                                                                
*              PROCHIST - ADD IN HOURS/COST INTO BUCKETS                        
*                                                                               
TR2      CLI   THISACC,C'Y'                                                     
         BNE   EXIT                                                             
*&&UK                                                                           
*                                                                               
         USING TRSUBHD,R2                                                       
*                                                                               
         L     R2,ADSUBAC          IGNORE 1J CONTRAS IN UK                      
         CLC   TRSBACNT+1(2),=C'1J'                                             
         BE    EXIT                                                             
*                                                                               
         DROP  R2                                                               
*&&                                                                             
*                                                                               
         USING TRHISTD,R2                                                       
*                                                                               
         L     R2,ADTRANS                                                       
         CLI   TRHSEL,X'45'        HISTORY ELEMENT                              
         BNE   EXIT                                                             
*                                                                               
         L     RF,ADSUBAC                                                       
         SH    RF,DATADISP                                                      
         CLI   ACKEYREF+1-ACKEYD(RF),C' '                                       
         BNH   TR2A                       NEW COST BUCKET                       
         CLC   METHOD,ACKEYREF-ACKEYD(RF) MATCH REQUESTED METHOD                
         BNE   EXIT                                                             
         B     TR2B                                                             
*                                                                               
TR2A     CLI   BUCKTYPE,C' '                                                    
         BE    TR2B                                                             
         CLI   BUCKTYPE,C'H'                                                    
         BNE   EXIT                                                             
*                                                                               
TR2B     LA    R1,DATAB+2                                                       
         LA    RE,SORTCOST                                                      
         CLI   BUCKTYPE,C'H'                                                    
         BNE   *+8                                                              
         LA    RE,SORTHOUR                                                      
         LA    R0,12                                                            
*                                                                               
TR4      CLC   TRHSYEAR(2),0(R1)                                                
         BE    TR6                                                              
         LA    R1,2(,R1)                                                        
         LA    RE,8(,RE)                                                        
         BCT   R0,TR4                                                           
         B     EXIT                                                             
*                                                                               
TR6      ZAP   DUB,TRHSDR          ADD INTO MONTH BUCKET                        
         CLI   BUCKTYPE,C'H'                                                    
         BNE   TR8                                                              
*&&US*&& ZAP   DUB,TRHSCR                                                       
*&&UK*&& ZAP   DUB,TRHSDR                                                       
*&&UK*&& AP    DUB,TRHSCR                                                       
*                                                                               
TR8      CP    DUB,=P'2100000000'  CANT CONVERT NUMBERS THIS BIG OR             
         BH    EXIT                                                             
         CP    DUB,=P'-2100000000' OR THIS SMALL - JUST IGNORE THEM.            
         BL    EXIT                                                             
         CVB   R0,DUB                                                           
         A     R0,0(,RE)                                                        
         ST    R0,0(,RE)                                                        
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*              SBACLAST/ACCLAST - PUT RECORD TO SORT                            
*                                                                               
SB4      CLI   THISACC,C'Y'                                                     
         BNE   SBXIT                                                            
         XC    SORTKEY,SORTKEY                                                  
         MVC   SORTKEY3,PERACCT                                                 
         CLI   MODE,SBACLAST                                                    
         BE    SB6                                                              
         MVC   CLIACCT,FIRSTCLI                                                 
         XC    SORTKEY3,SORTKEY3   SORT TO FRONT                                
         MVC   SORTACCS,PERSACCS                                                
*                                                                               
SB6      OC    SORTACCS,SORTACCS                                                
         BZ    SBXIT                                                            
         CLI   MODE,SBACLAST                                                    
         BNE   SB10                                                             
         BAS   RE,CLIFLT1          FILTER CLIENT                                
         BNE   SB9                                                              
         LA    R1,SORTACCS         ROLL SORTACCS INTO PERSACCS                  
         LA    RE,PERSACCS                                                      
         LA    R0,24                                                            
*                                                                               
SB8      L     RF,0(,R1)                                                        
         A     RF,0(,RE)                                                        
         ST    RF,0(,RE)                                                        
         LA    R1,4(,R1)                                                        
         LA    RE,4(,RE)                                                        
         BCT   R0,SB8                                                           
*                                                                               
SB9      LA    R1,SORTACCS         ROLL SORTACCS INTO TOTACCS                   
         LA    RE,TOTACCS                                                       
         LA    R0,24                                                            
*                                                                               
SB9A     L     RF,0(,R1)                                                        
         A     RF,0(,RE)                                                        
         ST    RF,0(,RE)                                                        
         LA    R1,4(,R1)                                                        
         LA    RE,4(,RE)                                                        
         BCT   R0,SB9A                                                          
*                                                                               
         BAS   RE,CLIFLT2          FILTER CLIENT                                
         BNE   SBXIT                                                            
         L     RE,VMEMO            ADD CLIENT TO MEMO POOL                      
         L     R1,0(,RE)                                                        
         LA    RF,1(,R1)                                                        
         ST    RF,0(,RE)                                                        
         MH    R1,=H'15'                                                        
         LA    R1,4(R1,RE)                                                      
         MVC   0(15,R1),CLIACCT                                                 
         CLI   FIRST,C'Y'                                                       
         BNE   SB10                                                             
         MVI   FIRST,C'N'                                                       
         MVC   FIRSTCLI,CLIACCT                                                 
*                                                                               
SB10     LA    R1,OPTAB                                                         
         SR    RE,RE                                                            
*                                                                               
SB12     CLI   0(R1),X'FF'                                                      
         BE    SB14                                                             
         CLC   0(1,R1),QOPT1                                                    
         BE    SB14                                                             
         IC    RE,1(,R1)                                                        
         AR    R1,RE                                                            
         B     SB12                                                             
*                                                                               
SB14     LA    R2,2(,R1)                                                        
*                                                                               
SB16     CLI   0(R2),X'FF'         ONE RECORD/REPORT                            
         BE    SBXIT                                                            
         ZIC   RE,0(,R2)                                                        
         LA    RF,L'REPTAB                                                      
         MR    RE,RE                                                            
         LA    RE,REPTAB-L'REPTAB(RF)                                           
         MVC   SORTYPE,0(R2)       REPORT NUMBER                                
         CLI   0(RE),C'P'                                                       
         BNE   SB18                                                             
         MVC   SORTKEY2,CLIACCT                                                 
         LA    R5,PERHEIR                                                       
         LA    R3,PERACCT                                                       
         LA    R4,1                                                             
         B     SB20                                                             
*                                                                               
SB18     MVC   SORTKEY2,PERACCT                                                 
         LA    R5,CLIHEIR                                                       
         LA    R3,CLIACCT                                                       
         LA    R4,1                                                             
         CLI   MODE,SBACLAST                                                    
         BE    SB20                                                             
         L     R3,VMEMO                                                         
         ICM   R4,15,0(R3)                                                      
         BZ    SBXIT               EXIT IF NO CLIENTS IN MEMO POOL              
         LA    R3,4(,R3)                                                        
*                                                                               
SB20     ZIC   R1,1(,RE)           R1=KEY FLAVOUR                               
         MH    R1,=H'16'                                                        
         AR    R1,R5               R1=A(FLAVOUR TABLE ENTRY)                    
         SH    R1,=H'4'            FIND LONGEST KEY LENGTH                      
         CLI   1(R1),0                                                          
         BE    *-8                                                              
         ZIC   R5,2(,R1)                                                        
         LA    R5,2(,R5)           R5=OUTPUT KEY LENGTH (FOR EXECUTE)           
*                                                                               
SB21     CLI   2(R3),C'N'          TEST IF NON-CLIENT TIME                      
         BNE   *+8                                                              
         LA    R5,14               YES - SET LENGTH TO MAXIMUM                  
         MVC   SORTKEY1,SPACES                                                  
         EXMVC R5,SORTKEY1,0(R3)   MOVE KEY TO SORT KEY                         
*                                                                               
SB21A    GOTO1 VSORTER,DMCB,=C'PUT',SORTREC                                     
         CLI   QOPT7,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,DMPPUT        SPECIAL DILLON OPTION  TO TRACE BUGS            
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   SB21B                                                            
         OC    SORTKEY3,SORTKEY3                                                
         BNZ   SB21C                                                            
         MVI   SORTKEY3,1                                                       
         MVC   SORTACCS,TOTACCS                                                 
         B     SB21A                                                            
*                                                                               
SB21B    LA    R3,15(,R3)                                                       
         BCT   R4,SB21                                                          
         B     SB22                                                             
*                                                                               
SB21C    XC    SORTKEY3,SORTKEY3   FOR BBDO PUT PERSONAL                        
         MVC   SORTACCS,PERSACCS   AND TOTAL FOR EACH CLIENT                    
         LA    R3,15(,R3)                                                       
         BCT   R4,SB21                                                          
*                                                                               
SB22     MVI   SORTSW,C'Y'                                                      
         LA    R2,1(,R2)                                                        
         B     SB16                                                             
*                                                                               
SBXIT    CLI   MODE,ACCLAST                                                     
         BNE   EXIT                                                             
         XC    PERSACCS,PERSACCS                                                
         B     EXIT                                                             
         EJECT ,                                                                
*              FILTER CLIENT BASED ON PROFILE OPTION                            
*                                                                               
CLIFLT1  CLI   PROGPROF+1,C'P'     TOTAL TIME EXCLUDING PERSONAL TIME           
         BNE   CLIFLT12                                                         
         CLC   CLIACCT+2(3),=C'NP '                                             
         BE    CLIFLTN                                                          
         B     CLIFLTY                                                          
*                                                                               
CLIFLT12 CLI   PROGPROF+1,C'T'     TOTAL TIME                                   
         BE    CLIFLTY                                                          
         CLI   CLIACCT+2,C'C'      DIRECT TIME ONLY                             
         BNE   CLIFLTN                                                          
         B     CLIFLTY                                                          
         EJECT ,                                                                
*              FILTER CLIENT BASED ON REQUEST OPTION                            
*                                                                               
CLIFLT2  CLI   QOPT2,C' '          INCLUDE ALL TIME                             
         BE    CLIFLTY                                                          
         CLI   QOPT2,C'D'          DIRECT TIME ONLY                             
         BNE   *+16                                                             
         CLI   CLIACCT+2,C'C'                                                   
         BE    CLIFLTY                                                          
         B     CLIFLTN                                                          
         CLI   QOPT2,C'I'          INDIRECT TIME ONLY                           
         BNE   CLIFLTY                                                          
         CLI   CLIACCT+2,C'C'                                                   
         BNE   CLIFLTY                                                          
         B     CLIFLTN                                                          
*                                                                               
CLIFLTY  CR    RE,RE               SET CC=EQ TO INCLUDE                         
         BR    RE                                                               
*                                                                               
CLIFLTN  LTR   RE,RE               SET CC=NEQ TO EXCLUDE                        
         BR    RE                                                               
         EJECT ,                                                                
*              REQLAST - PRINT REPORT                                           
*                                                                               
RE8      CLI   SORTSW,C'Y'                                                      
         BNE   RE28                                                             
         XC    LASTKEYS,LASTKEYS                                                
         XC    LASTFRMS,LASTFRMS                                                
         MVI   LASTTYPE,0                                                       
         MVI   SORTFRST,C'Y'                                                    
         B     RE9A                                                             
*                                                                               
RE9      CLI   SORTYPE,X'FF'       E-O-F, GET OUT                               
         BE    RE28                                                             
*                                                                               
RE9A     GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R1,4(,R1)                                                        
         LTR   R1,R1                                                            
         BNZ   RE10                                                             
         CLI   SORTFRST,C'Y'                                                    
         BE    RE28                NOTHING TO PRINT                             
         MVI   SORTYPE,X'FF'       SET E-O-F ON SORT                            
         MVC   CLIACCT,FIRSTCLI                                                 
         B     RE11                                                             
*                                                                               
RE10     MVI   SORTFRST,C'N'                                                    
         MVC   SORTREC,0(R1)       SAVE SORT RECORD                             
         CLI   QOPT7,C'Y'                                                       
         BNE   REQ10A                                                           
         BAS   RE,DMPGET           SPECIAL DILLON OPTION TO TRACE BUGS          
*                                                                               
REQ10A   BAS   RE,CLIFILT          DO WE WANT THIS CLIENT ?                     
         CLI   WANT,C'Y'                                                        
         BNE   RE9                                                              
*                                                                               
RE11     CLC   SORTYPE,LASTTYPE    REPORT CONTROL BREAK                         
         BE    RE14                                                             
         CLI   LASTTYPE,0                                                       
         BE    RE12                                                             
         ZIC   R1,LEVELS                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,TOTLEV                                                        
         BAS   RE,TOTALS                                                        
         CLI   SORTYPE,X'FF'       E-O-F - EXIT                                 
         BE    RE28                                                             
*                                                                               
RE12     XC    LASTKEYS,LASTKEYS                                                
         XC    LASTFRMS,LASTFRMS                                                
         MVI   LASTTYPE,0                                                       
         L     RE,VACCUMS          CLEAR ACCUMS                                 
         L     RF,ACCLEN                                                        
         XCEF                                                                   
         XC    TOTP1,TOTP1                                                      
         XC    TOTP2,TOTP2                                                      
         XC    TOTP3,TOTP3                                                      
         MVI   HEDFORM,C'N'                                                     
         MVI   MIDFORM,C'N'                                                     
         MVI   FIRST,C'Y'                                                       
*                                                                               
RE14     GOTO1 AKEYSPIT,DMCB,(RC)                                               
         CLC   THISKEY1(L'THISKEYS),LASTKEY1                                    
         BNE   RE15                                                             
         OC    SORTKEY3,SORTKEY3                                                
         BZ    RE9                                                              
         OC    SORTKEY3+1(L'SORTKEY3-1),SORTKEY3+1                              
         BZ    RE19                                                             
         BAS   RE,GETVALS                                                       
         L     RF,VACCUMS                                                       
         ZIC   R0,LEVELS                                                        
         AH    R0,=H'2'                                                         
         B     RE22                                                             
*                                                                               
RE15     LA    R1,HEADSPEC                                                      
         LA    R2,THISKEY1                                                      
         LA    R3,LASTKEY1                                                      
         ZIC   R4,LEVELS                                                        
         LA    R5,120                                                           
*                                                                               
RE16     CLC   0(15,R2),0(R3)      KEY CONTROL BREAKS                           
         BE    RE18                                                             
         OC    0(15,R3),0(R3)                                                   
         BZ    RE18                                                             
         STC   R4,TOTLEV                                                        
         BAS   RE,TOTALS                                                        
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
*                                                                               
         TM    0(R1),X'80'                                                      
         BZ    RE17                                                             
         MVI   HEDFORM,C'N'                                                     
         MVI   MIDFORM,C'N'                                                     
*                                                                               
RE17     TM    0(R1),X'40'                                                      
         BZ    RE18                                                             
         MVI   MIDFORM,C'N'                                                     
*                                                                               
RE18     LA    R2,15(,R2)                                                       
         LA    R3,15(,R3)                                                       
         SH    R5,=H'15'                                                        
         LA    R1,1(,R1)                                                        
         BCT   R4,RE16                                                          
*                                                                               
RE19     BAS   RE,GETVALS                                                       
         OC    SORTKEY3,SORTKEY3   TOTAL RECORD                                 
         BNZ   RE19A                                                            
         MVC   SAVEACCS,VALUES                                                  
         B     RE26                                                             
*                                                                               
RE19A    OC    SORTKEY3+1(L'SORTKEY3-1),SORTKEY3+1                              
         BNZ   RE20                                                             
         MVC   SAVETOTS,VALUES                                                  
         B     RE9                                                              
*                                                                               
RE20     L     RE,VACCUMS          ROLL INTO ACCUMS                             
         MVC   0(L'VALUES,RE),VALUES                                            
         ZIC   R0,LEVELS                                                        
         AH    R0,=H'1'                                                         
         LA    RF,L'VALUES(,RE)                                                 
*                                                                               
RE22     LA    R1,L'VALUES                                                      
         SRL   R1,3                                                             
         LA    RE,VALUES                                                        
*                                                                               
RE24     LM    R2,R3,0(RF)                                                      
         A     R2,0(,RE)                                                        
         A     R3,4(,RE)                                                        
         STM   R2,R3,0(RF)                                                      
         LA    RE,8(,RE)                                                        
         LA    RF,8(,RF)                                                        
         BCT   R1,RE24                                                          
         BCT   R0,RE22                                                          
*                                                                               
RE26     MVC   LASTKEYS,THISKEYS                                                
         MVC   LASTFRMS,THISFRMS                                                
         MVC   LASTTYPE,THISTYPE                                                
         B     RE9                                                              
*                                                                               
RE28     GOTO1 VSORTER,DMCB,=C'END'                                             
         B     EXIT                                                             
         EJECT ,                                                                
*              EXTRACT VALUES FROM HEIRARCHY ELEMENT                            
*              R2=A(HEIRARCHY ELEMENT)                                          
*              R3=A(OUTPUT TABLE)                                               
*              R4=A(NAME TABLE)                                                 
*                                                                               
         USING ACHEIRD,R2                                                       
*                                                                               
SETHEIR  NTR1                                                                   
         MVC   00(15,R4),ACHRDESA                                               
         MVC   15(15,R4),ACHRDESB                                               
         MVC   30(15,R4),ACHRDESC                                               
         MVC   45(15,R4),ACHRDESD                                               
         XC    0(L'PERHEIR,R3),0(R3)                                            
         LA    R1,ACHRLEVD         GET NUMBER OF LEVELS                         
         LA    RE,3                                                             
*                                                                               
SETH1    CLI   0(R1),0                                                          
         BNE   SETH2                                                            
         SH    R1,=H'16'                                                        
         BCT   RE,SETH1                                                         
*                                                                               
SETH2    LA    RF,L'LEVTAB                                                      
         MR    RE,RE                                                            
         LA    R1,LEVTAB(RF)                                                    
         SR    R5,R5                                                            
         SR    R6,R6                                                            
*                                                                               
SETH3    CLI   0(R1),X'FF'                                                      
         BE    SETHX                                                            
         CLI   0(R1),0                                                          
         BNE   SETH3A                                                           
         SR    R5,R5                                                            
         SR    R6,R6                                                            
         B     SETH4                                                            
*                                                                               
SETH3A   AR    R5,R6                                                            
         STC   R5,0(,R3)                                                        
         ZIC   RE,0(,R1)                                                        
         BCTR  RE,0                                                             
         MH    RE,=H'16'                                                        
         LA    RE,ACHRLEVA(RE)                                                  
         IC    R6,0(,RE)                                                        
         SR    R6,R5                                                            
         STC   R6,1(,R3)                                                        
         MVC   2(1,R3),0(RE)                                                    
         MVC   3(1,R3),0(R1)                                                    
*                                                                               
SETH4    CLI   0(R1),4                                                          
         BNE   *+8                                                              
         SR    R5,R5                                                            
         SR    R6,R6                                                            
         LA    R1,1(,R1)                                                        
         LA    R3,4(,R3)                                                        
         B     SETH3                                                            
*                                                                               
SETHX    XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*              PRINT TOTALS FOR ANY LEVEL                                       
*                                                                               
TOTALS   NTR1                                                                   
         MVC   INDEX1,LEVELS       KEY INDEX                                    
         MVI   INDEX2,0            ACCUM INDEX                                  
         CLI   FIRST,C'Y'                                                       
         BNE   TOTAL2                                                           
*        CLI   SORTYPE,X'FF'                                                    
*        BE    EXIT                E-O-F                                        
*        ZIC   RE,SORTYPE          SET REPORT SPECS ON FIRST TIME               
         ZIC   RE,LASTTYPE         SET REPORT SPECS ON FIRST TIME               
         LA    RF,L'REPTAB                                                      
         MR    RE,RE                                                            
         LA    RE,REPTAB-L'REPTAB(RF)                                           
         XC    AROUT,AROUT                                                      
         MVC   SORTORD,0(RE)                                                    
         MVC   AHEAD+1(3),4(RE)                                                 
         MVC   AFORM+1(3),7(RE)                                                 
         MVC   FORMSPEC,10(RE)                                                  
         LM    RE,RF,AROUT                                                      
         A     RE,RELO                                                          
         A     RF,RELO                                                          
         STM   RE,RF,AROUT                                                      
         ZIC   RE,LEVELS                                                        
         LA    RF,L'SPECTAB                                                     
         MR    RE,RE                                                            
         LA    RE,SPECTAB-L'SPECTAB(RF)                                         
         MVC   HEADSPEC,0(RE)                                                   
         GOTO1 AHEAD,DMCB,(RC)                                                  
         MVI   FIRST,C'N'                                                       
*                                                                               
TOTAL2   ZIC   RE,INDEX2           EXTRACT ACCUMS/KEYS                          
         LA    RF,L'VALUES                                                      
         MR    RE,RE                                                            
         A     RF,VACCUMS                                                       
         MVC   VALUES,0(RF)                                                     
         XC    0(L'VALUES,RF),0(RF)                                             
         MVC   ACCKEY,SPACES                                                    
         MVC   ACCFRM,SPACES                                                    
         MVC   ACCLEV,SPACES                                                    
         MVC   ACCNAM,SPACES                                                    
         ZIC   R1,INDEX1                                                        
         SH    R1,=H'1'                                                         
         BNM   TOTAL3                                                           
         MVC   ACCLEV,=CL15'REQUEST'                                            
         B     TOTAL4                                                           
*                                                                               
TOTAL3   LR    RE,R1                                                            
         MH    R1,=H'15'                                                        
         LA    R1,LASTKEY1(R1)                                                  
         MVC   ACCKEY,0(R1)                                                     
         LR    R1,RE                                                            
         MH    R1,=H'12'                                                        
         LA    R1,LASTFRM1(R1)                                                  
         MVC   ACCFRM,0(R1)                                                     
         LR    R1,RE                                                            
         MH    R1,=H'15'                                                        
         LA    R1,LASTLEV1(R1)                                                  
         MVC   ACCLEV,0(R1)                                                     
         MVC   BUFFKEY,ACCKEY                                                   
         CLI   INDEX2,0                                                         
         BNE   TOTAL4                                                           
         BAS   RE,GETBUFF                                                       
         MVC   ACCNAM,BUFFNAME                                                  
         MVC   TEMPHIRE,BUFFHIRE                                                
         MVC   TEMPTERM,BUFFTERM                                                
*                                                                               
TOTAL4   BAS   RE,PRINTIT          FORMAT/PRINT TOTAL LINE                      
         ZIC   R1,INDEX1                                                        
         BCTR  R1,0                                                             
         STC   R1,INDEX1                                                        
         ZIC   R1,INDEX2                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,INDEX2                                                        
         CLC   INDEX2,TOTLEV                                                    
         BL    TOTAL2                                                           
         B     EXIT                                                             
         EJECT ,                                                                
*              FORMAT HEADLINES & PRINT A LINE                                  
*                                                                               
         USING HEADLND,R5                                                       
*                                                                               
PRINTIT  NTR1                                                                   
         CLI   HEDFORM,C'Y'                                                     
         BE    PRINT4                                                           
         MVI   HEDFORM,C'Y'                                                     
         L     R5,=A(HEADLNS)                                                   
         MVC   HEADA,SPACES                                                     
         MVC   HEADB,SPACES                                                     
         MVC   HEADC,SPACES                                                     
         MVC   HEADD,SPACES                                                     
         MVC   HEADE,SPACES                                                     
         MVC   HEADF,SPACES                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   HEADA+85(16),=C'FOR THE MONTH OF'                                
         GOTO1 DATCON,DMCB,(0,EDATEC),(11,WORK)   MMMDD/YY                      
         MVC   HEADA+102(3),WORK                  MMM                           
         MVC   HEADA+105(3),WORK+5                /YY                           
         MVC   HEADB+85(11),=C'REPORT TYPE'                                     
         ZIC   R1,LASTTYPE                                                      
         EDIT  (R1),(3,HEADB+97),ALIGN=LEFT                                     
         ZIC   RE,LASTTYPE                                                      
         LA    RF,L'REPTAB                                                      
         MR    RE,RE                                                            
         LA    RE,REPTAB-L'REPTAB(RF)                                           
         LA    R1,HEADC+85                                                      
         MVC   0(20,R1),=CL20'STAFF ORDER'                                      
         CLI   0(RE),C'P'                                                       
         BE    *+10                                                             
         MVC   0(20,R1),=CL20'CLIENT ORDER'                                     
         CLI   11(RE),C' '                                                      
         BE    PRINT1                                                           
         LA    R1,19(,R1)                                                       
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   1(5,R1),=C'-CASH'                                                
         CLI   11(RE),C'C'                                                      
         BE    PRINT1                                                           
         MVC   1(6,R1),=C'-HOURS'                                               
*                                                                               
PRINT1   LA    R1,HEADSPEC                                                      
         LA    R2,LASTKEY1                                                      
         LA    R3,LASTFRM1                                                      
         LA    R4,LASTLEV1                                                      
         L     R5,=A(HEADLNS)                                                   
*                                                                               
PRINT2   TM    0(R1),X'80'                                                      
         BZ    PRINT4                                                           
         MVC   BUFFKEY,0(R2)                                                    
         BAS   RE,GETBUFF                                                       
         MVC   1(15,R5),0(R4)           MOVE INFO ON RECORD TO HEADLINE         
         MVC   17(10,R5),0(R3)                                                  
         MVC   28(36,R5),BUFFNAME                                               
         BAS   RE,SQUASHIT                                                      
*                                                                               
         ST    R1,REG1                      SAVE REGISTER 1                     
         LA    R6,3                         GET 2ND PARM FROM SQUASHER          
         A     R6,DMCB+4                                                        
         GOTO1 AHNTDATE,DMCB,(RC),(R5),(R6)    PRT HIRE AND TERM DATES          
         L     R1,REG1                                                          
*                                                                               
         LA    R1,1(,R1)                                                        
         LA    R2,L'LASTKEY1(,R2)       BUMP TO NEXT KEY==>LENGTH 15            
         LA    R3,L'LASTFRM1(,R3)       BUMP TO NEXT ? ==>LEN. 12               
         LA    R4,L'LASTLEV1(,R4)       BUMP TO NEXT LEVEL==>LEN. 15            
         LA    R5,L'HEADA(,R5)          BUMP NEXT HEAD LINE==>LEN.132           
         B     PRINT2                                                           
*                                                                               
PRINT4   CLI   MIDFORM,C'Y'                                                     
         BE    PRINT10                                                          
         MVI   MIDFORM,C'Y'                                                     
         LA    R1,HEADSPEC                                                      
         LA    R2,LASTKEY1                                                      
         LA    R3,LASTFRM1                                                      
         LA    R4,LASTLEV1                                                      
         LA    R0,8                                                             
*                                                                               
PRINT6   TM    0(R1),X'40'                                                      
         BO    PRINT8                                                           
         LA    R1,1(,R1)                                                        
         LA    R2,L'LASTKEY1(,R2)                                               
         LA    R3,L'LASTFRM1(,R3)                                               
         LA    R4,L'LASTLEV1(,R4)                                               
         BCT   R0,PRINT6                                                        
         B     PRINT10                                                          
*                                                                               
PRINT8   MVC   BUFFKEY,0(R2)                                                    
         BAS   RE,GETBUFF                                                       
         MVC   MIDA,SPACES                                                      
         MVC   MIDB,SPACES                                                      
         MVC   MIDA+1(12),0(R3)                                                 
         MVC   MIDA+14(36),BUFFNAME                                             
         GOTO1 VSQUASH,DMCB,MIDA+1,60                                           
         L     R6,DMCB+4                    GET 2ND PARM FROM SQUASHER          
         GOTO1 AHNTDATE,DMCB,(RC),MIDA+2,(R6)  PRT HIRE AND TERM DATES          
         GOTO1 VUNDER,DMCB,(70,MIDA+1),MIDB+1                                   
         MVI   MIDPRINT,C'N'                                                    
*                                                                               
PRINT10  MVC   PA,SPACES                                                        
         MVC   PB,SPACES                                                        
         MVC   PC,SPACES                                                        
         MVC   PD,SPACES                                                        
         MVI   PERCSW,C'N'                                                      
         CLI   INDEX2,0                                                         
         BNE   *+8                                                              
         MVI   PERCSW,C'Y'                                                      
         GOTO1 AFORM                                                            
         CLC   PA,SPACES                                                        
         BE    EXIT                                                             
         CLI   INDEX2,0                                                         
         BNE   PRINT12                                                          
         CLI   MIDPRINT,C'Y'                                                    
         BE    PRINT11                                                          
         MVI   MIDPRINT,C'Y'                                                    
         LA    R1,P                                                             
         CLI   FORCEHED,C'Y'                                                    
         BE    *+12                                                             
         MVI   0(R1),0                                                          
         LA    R1,132(R1)                                                       
         MVC   0(132,R1),MIDA                                                   
         MVC   132(132,R1),MIDB                                                 
         MVI   264(R1),0                                                        
         L     R5,=A(HEADLNS)                                                   
*                                                                               
         MVC   HEAD5,HEADA                                                      
         MVC   HEAD6,HEADB                                                      
         MVC   HEAD7,HEADC                                                      
         MVC   HEAD8,HEADD                                                      
         MVC   HEAD9,HEADE                                                      
         MVC   HEAD10,HEADG                                                     
         MVC   HEAD11,HEADH                                                     
         MVC   HEAD12,HEADI                                                     
         GOTO1 ACREPORT                                                         
*                                                                               
PRINT11  MVC   WORKAREA,SPACES                                                  
         MVC   WORKAREA(12),ACCFRM                                              
         MVC   WORKAREA+13(36),ACCNAM                                           
         MVC   BUFFHIRE,TEMPHIRE                                                
         MVC   BUFFTERM,TEMPTERM                                                
         GOTO1 AHNTDATE,DMCB,(RC),WORKAREA,50                                   
         XC    BUFFHIRE,BUFFHIRE                                                
         XC    BUFFTERM,BUFFTERM                                                
*                                                                               
         LA    R6,L'ACCFRM                                                      
         LA    R2,ACCFRM+L'ACCFRM-1                                             
*                                                                               
STRLEN   CLI   0(R2),C' '               CALCULATE THE LENGTH OF                 
         BNE   STRLEN2                                                          
         BCTR  R2,R0                    A STRING IN A FIELD                     
         BCT   R6,STRLEN                                                        
*                                                                               
STRLEN2  EXMVC R6,PA+1,ACCFRM                                                   
*                                                                               
         ZIC   R2,CHOPSPEC              SET UP DESTINATION PARAM FOR            
         SR    R2,R6                    THE NEXT CHOPPER CALL                   
         STC   R2,DESTIN                WIDTH OF DESTIN BLOCK                   
         LA    R2,PA+2                                                          
         AR    R2,R6                                                            
         STCM  R2,7,DESTIN+1            ADDRESS OF BLOCK                        
*                                                                               
         GOTO1 VSQUASH,DMCB,WORKAREA,132                                        
         LR    R2,R6                                                            
         S     R2,DMCB+4                    GET 2ND PARM FROM SQUASHER          
         LPR   R2,R2                        SET UP SOURCE PARAM FOR             
         LA    R2,1(,R2)                    THE NEXT CHOPPER CALL               
         STC   R2,SOURCE                    LENGTH OF SOURCE                    
         LA    R2,WORKAREA(R6)                                                  
         STCM  R2,7,SOURCE+1                ADDRESS OF SOURCE                   
         GOTO1 CHOPPER,DMCB,SOURCE,DESTIN,(C'P',4)                              
         B     PRINT14                                                          
*                                                                               
PRINT12  MVC   WORK,SPACES                                                      
         MVC   WORK+1(9),=C'TOTAL FOR'                                          
         MVC   WORK+11(15),ACCLEV                                               
         MVC   WORK+27(12),ACCFRM                                               
         GOTO1 VSQUASH,DMCB,WORK+1,40                                           
         MVC   PC,PB                                                            
         MVC   PB,PA                                                            
         MVC   PA,SPACES                                                        
         MVI   PA,0                                                             
         ZIC   R2,CHOPSPEC                                                      
         GOTO1 CHOPPER,DMCB,(60,WORK+1),((R2),PB+2),(C'P',2)                    
*                                                                               
PRINT14  BAS   RE,MYREPORT                                                      
         B     EXIT                                                             
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*                                  PRINT A LINE                                 
         USING HEADLND,R5                                                       
*                                                                               
MYREPORT L     R5,=A(HEADLNS)                                                   
         MVC   HEAD5,HEADA                                                      
         MVC   HEAD6,HEADB                                                      
         MVC   HEAD7,HEADC                                                      
         MVC   HEAD8,HEADD                                                      
         MVC   HEAD9,HEADE                                                      
         MVC   HEAD10,HEADG                                                     
         MVC   HEAD11,HEADH                                                     
         MVC   HEAD12,HEADI                                                     
         MVC   P,PA                                                             
         MVC   PSECOND,PB                                                       
         MVC   PTHIRD,PC                                                        
         MVC   PFOURTH,PD                                                       
         ST    RE,REGE                                                          
         GOTO1 ACREPORT                                                         
         L     RE,REGE                                                          
         BR    RE                                                               
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
         EJECT ,                                                                
*                                  CALCULATE PERCENT TO IDP                     
PERCENT  XC    DUB(4),DUB                                                       
         LTR   R1,R1                                                            
         BM    PERCX                                                            
         LTR   R2,R2                                                            
         BNP   PERCX                                                            
         CVD   R1,DUB1                                                          
         ZAP   PWK1,DUB1                                                        
         CVD   R2,DUB1                                                          
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   *+14                                                             
         MP    PWK1,=P'100000'                                                  
         B     *+10                                                             
         MP    PWK1,=P'10000'                                                   
         DP    PWK1,DUB1                                                        
         SRP   PWK1(8),64-1,5                                                   
         ZAP   DUB1,PWK1(8)                                                     
         CP    DUB1,=P'2100000000' NO PERCENT OVER 21 MILLION                   
         BNH   *+10                                                             
         ZAP   DUB1,=P'0'                                                       
         CVB   R1,DUB1                                                          
         ST    R1,DUB                                                           
*                                                                               
PERCX    L     R1,DUB                                                           
         BR    RE                                                               
         EJECT ,                                                                
*                                  SQUASH HEADLINE DATA                         
SQUASHIT NTR1                                                                   
         GOTO1 VSQUASH,DMCB,1(R5),64                                            
         B     EXIT                                                             
         EJECT ,                                                                
*              DATA FORMAT ROUTINES                                             
*                                                                               
DATAF1   NTR1                                                                   
         L     R1,REQMONTH                                                      
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   *+8                                                              
         L     R1,BRDMONTH                                                      
         EDIT  (R1),(11,PA+46),2,ZERO=BLANK,MINUS=YES                           
         L     R1,YTD                                                           
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   *+8                                                              
         L     R1,BRDYTD                                                        
         EDIT  (R1),(11,PA+78),2,ZERO=BLANK,MINUS=YES                           
         CLI   QOPT3,C'Y'          SUPPRESS CASH OPTION                         
         BE    DATAF12                                                          
         L     R1,REQMONTH+4                                                    
         LA    R3,PA+57                                                         
         CLI   PROGPROF+2,C'2'                                                  
         BNE   *+8                                                              
         LA    R3,PA+66                                                         
         EDIT  (R1),(12,0(R3)),2,ZERO=BLANK,MINUS=YES                           
         L     R1,YTD+4                                                         
         LA    R3,PA+89                                                         
         CLI   PROGPROF+2,C'2'                                                  
         BNE   *+8                                                              
         LA    R3,PA+99                                                         
         EDIT  (R1),(12,0(R3)),2,ZERO=BLANK,MINUS=YES                           
*                                                                               
DATAF12  CLI   PROGPROF+3,C'Y'                                                  
         BE    DATAF17                                                          
         CLI   PROGPROF+4,C'Y'                                                  
         BNE   DATAF121            DON'T WANT TOTALS FOR PERCENT                
         CLI   INDEX2,0                                                         
         BE    DATAF121            LOWEST LEVEL CALCULATE PERCENT               
         ZIC   R3,INDEX2                                                        
         BCTR  R3,0                                                             
         MH    R3,=H'8'                                                         
         LA    R3,TOTP3(R3)                                                     
         L     R1,0(,R3)           ELSE GET PERCENT FOR TOTAL LINE              
         MVC   TOTP2,4(R3)         SAVE YTD PERCENT                             
         XC    0(8,R3),0(R3)                                                    
         B     DATAF13                                                          
*                                                                               
DATAF121 BAS   RE,CLIFLT1                                                       
         BNE   EXIT                                                             
         CLI   PERCSW,C'Y'                                                      
         BNE   EXIT                                                             
         MVC   DOUBLE(4),REQMONTH                                               
         MVC   DOUBLE+4(4),YTD                                                  
         MVC   VALUES,SAVEACCS                                                  
         L     R1,DOUBLE                                                        
         L     R2,REQMONTH                                                      
*                                                                               
DATAF12A BAS   RE,PERCENT                                                       
         ST    R1,TOTP1        SAVE MONTHLY PERCENT                             
*                                                                               
DATAF13  LA    R3,PA+70                                                         
         CLI   PROGPROF+2,C'2'                                                  
         BNE   *+8                                                              
         LA    R3,PA+58                                                         
         EDIT  (R1),(7,0(R3)),1,ZERO=BLANK                                      
         SPACE 1                                                                
         CLI   INDEX2,0                                                         
         BE    DATAF13A                                                         
         CLI   PROGPROF+4,C'Y'                                                  
         BNE   EXIT                DON'T WANT TOTALS FOR PERCENT                
         L     R1,TOTP2            YTD TOTAL PERCENT                            
         B     DATAF14                                                          
*                                                                               
DATAF13A L     R1,DOUBLE+4                                                      
         L     R2,YTD                                                           
         BAS   RE,PERCENT                                                       
         ST    R1,TOTP2                                                         
*                                                                               
DATAF14  LA    R3,PA+102                                                        
         CLI   PROGPROF+2,C'2'                                                  
         BNE   *+8                                                              
         LA    R3,PA+90                                                         
         EDIT  (R1),(7,0(R3)),1,ZERO=BLANK                                      
*                                                                               
DATAF16  CLI   PROGPROF+4,C'Y'                                                  
         BNE   EXIT                DON'T WANT TOTALS FOR PERCENT                
         CLI   INDEX2,0                                                         
         BNE   EXIT        ADD TO HIGHER LEVELS IF THIS IS LOW LEVEL            
         ZIC   R0,LEVELS                                                        
         LA    R3,TOTP3                                                         
*                                                                               
DATAF16B L     RE,0(,R3)                                                        
         A     RE,TOTP1                                                         
         ST    RE,0(,R3)                                                        
         L     RE,4(,R3)                                                        
         A     RE,TOTP2                                                         
         ST    RE,4(,R3)                                                        
         LA    R3,8(,R3)                                                        
         BCT   R0,DATAF16B                                                      
         B     EXIT                                                             
*                                                                               
DATAF17  CLI   INDEX2,0                                                         
         BNE   DATAF17A            MUST BE SOME TOTAL LINE                      
         BAS   RE,CLIFLT1                                                       
         BNE   EXIT                                                             
*                                                                               
DATAF17A L     R1,MONTHGP          PERCENT TOTALS FOR BBDO                      
         L     R2,YTDGP                                                         
         C     R1,=F'99999'                                                     
         BH    DATAF17B                                                         
         C     R2,=F'99999'                                                     
         BH    DATAF17B                                                         
         LA    R3,PA+58                                                         
         EDIT  (R1),(7,0(R3)),2,ZERO=BLANK,MINUS=YES                            
         LA    R3,PA+90                                                         
         EDIT  (R2),(7,0(R3)),2,ZERO=BLANK,MINUS=YES                            
         B     EXIT                                                             
*                                                                               
DATAF17B LA    R3,PB+54                                                         
         EDIT  (R1),(11,0(R3)),2,ZERO=BLANK,MINUS=YES                           
         LA    R3,PB+86                                                         
         EDIT  (R2),(11,0(R3)),2,ZERO=BLANK,MINUS=YES                           
         B     EXIT                                                             
         EJECT ,                                                                
*              MONTHLY REPORT FORMAT                                            
*                                                                               
DATAF2   NTR1                                                                   
         LA    R1,EXTAB1                                                        
         CLI   FORMSPEC,C'Y'                                                    
         BNE   *+8                                                              
         LA    R1,EXTAB2                                                        
         LA    R2,ACCLINE1                                                      
         BAS   RE,EXTRACT          EXTRACT PERSON/CLIENT ACCUMS                 
         CLI   PERCSW,C'Y'                                                      
         BNE   DATAF22                                                          
         LA    R2,ACCLINE2                                                      
         MVC   VALUES,SAVEACCS                                                  
         BAS   RE,EXTRACT          EXTRACT TOTAL PERSON ACCUMS                  
*                                                                               
DATAF22  LA    R3,ACCLINE1                                                      
         LA    R4,ACCLINE2                                                      
         LA    R5,PA+29                                                         
         LA    R6,9                                                             
*                                                                               
DATAF24  L     R0,0(,R3)                                                        
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    DATAF24A                                                         
         LA    R1,1(,R1)                                                        
*                                                                               
DATAF24A SRA   R1,1                                                             
         EDIT  (R1),(8,0(R5)),MINUS=YES                                         
         CLI   PERCSW,C'Y'                                                      
         BNE   DATAF26                                                          
         BAS   RE,CLIFLT1                                                       
         BNE   DATAF26                                                          
         L     R1,0(,R3)                                                        
         L     R2,0(,R4)                                                        
         BAS   RE,PERCENT                                                       
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   DATAF25                                                          
         EDIT  (R1),(6,133(R5)),2,ZERO=BLANK                                    
         B     DATAF26                                                          
*                                                                               
DATAF25  EDIT  (R1),(5,134(R5)),1,ZERO=BLANK                                    
*                                                                               
DATAF26  LA    R3,4(,R3)                                                        
         LA    R4,4(,R4)                                                        
         LA    R5,9(,R5)                                                        
         BCT   R6,DATAF24                                                       
         B     EXIT                                                             
         EJECT ,                                                                
*                                                                               
DATAF3   NTR1                                                                   
         LA    R1,EXTAB3                                                        
         LA    R2,ACCLINE1                                                      
         BAS   RE,EXTRACT                                                       
         CLI   PERCSW,C'Y'                                                      
         BNE   DATAF32                                                          
         LA    R2,ACCLINE2                                                      
         MVC   VALUES,SAVEACCS                                                  
         BAS   RE,EXTRACT                                                       
*                                                                               
DATAF32  B     DATAF22                                                          
*                                                                               
DATAF4   NTR1                                                                   
         LA    R1,EXTAB4                                                        
         LA    R2,ACCLINE1                                                      
         BAS   RE,EXTRACT                                                       
         CLI   PERCSW,C'Y'                                                      
         BNE   DATAF42                                                          
         LA    R2,ACCLINE2                                                      
         MVC   VALUES,SAVEACCS                                                  
         BAS   RE,EXTRACT                                                       
*                                                                               
DATAF42  LA    R3,ACCLINE1                                                      
         LA    R4,ACCLINE2                                                      
         LA    R5,PA+41                                                         
         LA    R6,7                                                             
*                                                                               
DATAF44  L     R0,0(,R3)                                                        
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    DATAF44A                                                         
         LA    R1,1(,R1)                                                        
*                                                                               
DATAF44A SRA   R1,1                                                             
         EDIT  (R1),(8,0(R5)),MINUS=YES                                         
         CLI   PERCSW,C'Y'                                                      
         BNE   DATAF46                                                          
         BAS   RE,CLIFLT1                                                       
         BNE   DATAF46                                                          
         L     R1,0(,R3)                                                        
         L     R2,0(,R4)                                                        
         BAS   RE,PERCENT                                                       
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   DATAF45                                                          
         EDIT  (R1),(6,133(R5)),2,ZERO=BLANK                                    
         B     DATAF46                                                          
*                                                                               
DATAF45  EDIT  (R1),(5,134(R5)),1,ZERO=BLANK                                    
*                                                                               
DATAF46  LA    R3,4(,R3)                                                        
         LA    R4,4(,R4)                                                        
         LA    R5,10(,R5)                                                       
         BCT   R6,DATAF44                                                       
         B     EXIT                                                             
         EJECT ,                                                                
*              GET ACCOUNT NAME FROM BUFFALO IF NOT FOUND READ FILE             
*              AND ADD RECORD                                                   
*                                                                               
GETBUFF  NTR1                                                                   
         GOTO1 BUFFALO,DMCB,=C'GET',VBUFF,BUFFKEY                               
         TM    8(R1),X'10'                                                      
         BZ    EXIT                                                             
         MVC   READKEY,SPACES                                                   
         MVC   READKEY(15),BUFFKEY                                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',READKEY,AIOAREA              
         MVC   BUFFNAME,SPACES                                                  
         MVC   BUFFNAME(17),=C'** NOT ON FILE **'                               
         CLI   8(R1),0                                                          
         BE    GETBUFF5                                                         
         BAS   RE,ADDBUFF                                                       
         B     EXIT                                                             
*                                                                               
GETBUFF5 L     R1,AIOAREA                                                       
         BAS   RE,GETNAME                                                       
         B     EXIT                                                             
         EJECT ,                                                                
*              EXTRACT NAME FROM RECORD AND ADD TO BUFFER                       
*                                                                               
         USING ACEMPD,R2                                                        
*                                                                               
GETNAME  NTR1                                                                   
         MVC   BUFFKEY,0(R1)                                                    
         AH    R1,DATADISP                                                      
         LR    R2,R1                                                            
         SR    RE,RE                                                            
*                                                                               
GETN2    CLI   0(R1),0                                                          
         BE    GETN4                                                            
         CLI   0(R1),X'20'                                                      
         BE    GETN3                                                            
         IC    RE,1(,R1)                                                        
         AR    R1,RE                                                            
         B     GETN2                                                            
*                                                                               
GETN3    MVC   BUFFNAME,SPACES                                                  
         IC    RE,1(,R1)                                                        
         SH    RE,=H'3'                                                         
         EXMVC RE,BUFFNAME,2(R1)                                                
         XC    BUFFHIRE,BUFFHIRE                                                
         XC    BUFFTERM,BUFFTERM                                                
*                                                                               
GETHNT2  CLI   0(R2),0                                                          
         BE    GETN4                                                            
         CLI   0(R2),X'56'                                                      
         BE    GETHNT4                                                          
         ZIC   R3,1(,R2)                                                        
         AR    R2,R3                                                            
         B     GETHNT2                                                          
*                                                                               
GETHNT4  MVC   BUFFHIRE,ACEMPHIR                                                
         MVC   BUFFTERM,ACEMPTRM                                                
         B     GETN4                                                            
*                                                                               
ADDBUFF  NTR1                                                                   
*                                                                               
GETN4    DS    0H                                                               
*        CLC   BUFFKEY,=CL15'91RD010000100'                                     
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         GOTO1 BUFFALO,DMCB,=C'PUT',VBUFF,BUFFKEY                               
         B     EXIT                                                             
*                                                                               
         DROP  R2                  KEEP IT CLEAN                                
         EJECT ,                                                                
*              SET PRINT VALUES FROM SORT ACCUMS                                
*                                                                               
GETVALS  NTR1                                                                   
         XC    VALUES,VALUES                                                    
         MVC   REQMONTH,SORTACCS+88                                             
         XC    BRDMONTH,BRDMONTH                                                
         XC    BRDYTD,BRDYTD                                                    
         MVC   BRDMONTH+3(1),BRDTAB+11  WEEKS IN LAST MONTH                     
         LA    R1,SORTACCS         ->   SORT HOURS/COST TABLE CR'S              
*                                       IN UK CR'S + DR'S                       
         LA    R0,12                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R2,MONTH            ->   HOURS/COST TABLE                        
         LA    R3,DATAB+2          ->   12 MONTH TO END DATE                    
         LA    R6,BRDTAB           ->   WEEKS IN MONTH TABLE                    
*                                                                               
GETV2    CLC   0(2,R3),DATAB       BEFORE FISCAL YEAR START ?                   
         BL    GETV4               YES, SKIP                                    
         MVC   0(8,R2),0(R1)       SAVE HOURS AND COST                          
         LA    R2,8(,R2)           NEXT HOURS AND MONTH TABLE ENTRY             
         A     RE,0(,R1)           ADD  HOURS                                   
         A     RF,4(,R1)           ADD  COST                                    
         ZIC   R4,0(,R6)           ADD  WEEKS IN MONTH                          
         A     R4,BRDYTD                TO                                      
         ST    R4,BRDYTD                   BROADCAST YEAR TO DATE               
*                                                                               
GETV4    LM    R4,R5,TWELVE        UPDATE 12 MONTHS                             
         A     R4,0(,R1)                HOURS                                   
         A     R5,4(,R1)                AND                                     
         STM   R4,R5,TWELVE             COST                                    
         LA    R1,8(,R1)           NEXT SORT TABLE ENTRY                        
         LA    R3,2(,R3)           NEXT MONTH                                   
         LA    R6,1(,R6)           NEXT MONTH'S NUM OF BROADCAST WEEKS          
         BCT   R0,GETV2            NEXT MONTH                                   
*                                                                               
         STM   RE,RF,YTD           SAVE TOTAL HOURS AND COST                    
*                                                                               
         LA    R1,MONTH            ->   HOURS/COST TABLE                        
         LA    R0,4                                                             
         LA    R2,QTR              ->   QUARTERLY SUM                           
*                                                                               
GETV6    LA    R3,3                ->   3 MONTHS PER QUARTER                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
GETV8    A     RE,0(,R1)           UPDATE QUARTERLY HOURS/COST SUM              
         A     RF,4(,R1)                                                        
         LA    R1,8(,R1)                                                        
         BCT   R3,GETV8                                                         
*                                                                               
         STM   RE,RF,0(R2)         SAVE QUARTERLY SUM                           
         LA    R2,8(,R2)           NEXT QUARTER                                 
         BCT   R0,GETV6                                                         
*                                                                               
         LM    RE,RF,QTR           GET  1ST  HALF YEAR SUM                      
         A     RE,QTR+8                                                         
         A     RF,QTR+12                                                        
         STM   RE,RF,HALFYR        SAVE 1ST  HALF YEAR SUM                      
         LM    RE,RF,QTR+16        GET  2ND  HALF YEAR SUM                      
         A     RE,QTR+24                                                        
         A     RF,QTR+28                                                        
         STM   RE,RF,HALFYR+8      SAVE 2ND  HALF YEAR SUM                      
*                                                                               
         LA    R1,SORTACCS+72      ->   LAST 3    MONTHS                        
         LA    R0,3                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
GETV10   A     RE,0(,R1)           HOUR                                         
         A     RF,4(,R1)           COST                                         
         LA    R1,8(,R1)                                                        
         BCT   R0,GETV10                                                        
*                                                                               
         STM   RE,RF,THREEM        SAVE LAST 3 MONTHS                           
*                                                                               
         L     RF,BRDMONTH         WEEKS IN LAST MONTH                          
         M     RE,=F'100'          TIMES 100                                    
         ST    RF,TOTMONTH         SAVE  IN TOTAL                               
         L     RF,BRDYTD           WEEKS IN 12 MONTHS                           
         M     RE,=F'100'          TIMES 100                                    
         ST    RF,TOTYTD           SAVE  IN TOTAL                               
         L     RF,REQMONTH         LAST  MONTH HOURS                            
         M     RE,=F'1'                                                         
         D     RE,BRDMONTH         DIVIDED BY WEEKS IN LAST MONTH               
         ST    RF,BRDMONTH         SAVE  HOURS/WEEK IN LAST MONTH               
         L     RF,YTD              TOTAL HOURS                                  
         M     RE,=F'1'                                                         
         D     RE,BRDYTD           DIVIDED BY WEEKS IN YEAR                     
         ST    RF,BRDYTD           SAVE AVERAGE IN WEEKS IN YEAR                
*                                                                               
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   GVXIT                                                            
         XC    REQMONTH+4(4),REQMONTH+4  CLEAR MONTH COST                       
         MVC   WORK,CLIACCT                                                     
         MVC   CLIACCT,FIRSTCLI                                                 
         BAS   RE,CLIFLT1                                                       
         MVC   CLIACCT,WORK                                                     
         BNE   GVXIT                                                            
         LA    R5,SAVEACCS         TOTAL MINUS PERS                             
         LA    R6,SAVETOTS         TOTAL INCLUDES PERS                          
         L     R1,REQMONTH                                                      
         M     R0,=F'1'                                                         
         M     R0,REQMONTH-VALUES(,R6)  CLI X (TOTAL+PERS)                      
         L     R3,REQMONTH-VALUES(,R5)                                          
         M     R2,=F'1'                                                         
         BAS   RE,DIV                                                           
         M     R0,=F'100'                                                       
         L     R3,TOTMONTH                                                      
         BAS   RE,DIV                                                           
*                                                                               
GETVAL11 ST    R1,MONTHGP          MONTH GROSS PERCENT                          
*                                                                               
         L     R1,YTD                                                           
         M     R0,=F'1'                                                         
         M     R0,YTD-VALUES(,R6)  CLI X (TOTAL+PERS)                           
         L     R3,YTD-VALUES(,R5)                                               
         M     R2,=F'1'                                                         
         BAS   RE,DIV                                                           
         M     R0,=F'100'                                                       
         L     R3,TOTYTD                                                        
         BAS   RE,DIV                                                           
*                                                                               
GETVAL12 ST    R1,YTDGP            MONTH GROSS PERCENT                          
*                                                                               
GVXIT    XMOD1 1                                                                
         EJECT ,                                                                
*              EXTRACT A LINE OF ACCUMS FROM VALUES                             
*                                                                               
EXTRACT  NTR1                                                                   
         XC    0(L'ACCLINE1,R2),0(R2)                                           
*                                                                               
EXTRAC2  CLI   0(R1),X'FF'         DELIMITER                                    
         BE    EXIT                                                             
         ZIC   R3,0(,R1)                                                        
         BCTR  R3,0                                                             
         SLL   R3,3                                                             
         LA    R3,VALUES(R3)                                                    
         CLI   FORMSPEC+1,C'H'     HOURS OR COST                                
         BE    *+8                                                              
         LA    R3,4(,R3)                                                        
         MVC   0(4,R2),0(R3)                                                    
         LA    R2,4(,R2)                                                        
         LA    R1,1(,R1)                                                        
         B     EXTRAC2                                                          
         EJECT ,                                                                
*                                                                               
*                                                                               
DIV      DS    0H                                                               
         LTR   R3,R3                                                            
         BNZ   DIV2                                                             
         SR    R1,R1                                                            
         BR    RE                                                               
*                                                                               
DIV2     DS    0H                                                               
*        SLDA  R0,1                                                             
         DR    R0,R3                                                            
*        LTR   R1,R1                                                            
*        BNP   *+8                                                              
*        A     R1,=F'1'                                                         
*        SRA   R1,1                                                             
         BR    RE                                                               
         EJECT ,                                                                
*              DETERMINE IF WE WANT THIS SUB-ACCOUNT                            
*                                                                               
CLIFILT  NTR1                                                                   
         MVI   WANT,C'Y'                                                        
         CLC   QSELECT,SPACES      SELECT OPTION                                
         BE    EXIT                                                             
         OC    SORTKEY3,SORTKEY3   TOTAL RECORD                                 
         BZ    EXIT                                                             
         ZIC   RE,SORTYPE                                                       
         LA    RF,L'REPTAB                                                      
         MR    RE,RE                                                            
         LA    RE,REPTAB-L'REPTAB(RF)                                           
         CLI   0(RE),C'C'                                                       
         BNE   CLIFL0                                                           
         MVC   WORK(L'SORTKEY1),SORTKEY1                                        
         B     CLIFL1                                                           
*                                                                               
CLIFL0   MVC   WORK(L'SORTKEY2),SORTKEY2                                        
*                                                                               
CLIFL1   LA    RE,QSELECT+5                                                     
         LA    RF,6                                                             
*                                                                               
CLIFL2   CLI   0(RE),X'40'         FIND LENGTH                                  
         BNE   CLIFL4                                                           
         BCTR  RE,0                MINUS ONE                                    
         BCT   RF,CLIFL2                                                        
*                                                                               
CLIFL4   BCTR  RF,0                                                             
         EXCLC RF,QSELECT,WORK+3                                                
         BE    EXIT                OK TO INCLUDE                                
         TM    QSELECT,X'40'       EXCLUDE SELECT                               
         BO    CLIFL6                                                           
         MVC   DUB(6),QSELECT                                                   
         OI    DUB,X'40'                                                        
         EXCLC RF,DUB,WORK+3                                                    
         BNE   EXIT                OK TO INCLUDE                                
*                                                                               
CLIFL6   MVI   WANT,C'N'                                                        
         B     EXIT                                                             
         EJECT ,                                                                
*                                                                               
DMPPUT   NTR1                                                                   
         LA    R6,=C'PUT'                                                       
         GOTO1 VPRNTBL,DMCB,(3,(R6)),SORTREC,C'DUMP',144,=C'2D'                 
         B     EXIT                                                             
*                                                                               
DMPGET   NTR1                                                                   
         LA    R6,=C'GET'                                                       
         GOTO1 VPRNTBL,DMCB,(3,(R6)),SORTREC,C'DUMP',144,=C'2D'                 
         B     EXIT                                                             
         EJECT ,                                                                
         DROP  R8,R9               KEEP IT CLEAN                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
OPTAB    DS    0C                                                               
         DC    C'1',AL1(4),AL1(01,255)                                          
         DC    C'2',AL1(4),AL1(02,255)                                          
         DC    C'3',AL1(4),AL1(03,255)                                          
         DC    C'4',AL1(4),AL1(04,255)                                          
         DC    C'5',AL1(4),AL1(05,255)                                          
         DC    C'6',AL1(4),AL1(06,255)                                          
         DC    C'7',AL1(4),AL1(07,255)                                          
         DC    C'8',AL1(4),AL1(08,255)                                          
         DC    C'9',AL1(4),AL1(09,255)                                          
         DC    C'A',AL1(4),AL1(10,255)                                          
         DC    C'B',AL1(4),AL1(11,255)                                          
         DC    C'C',AL1(4),AL1(12,255)                                          
         DC    C'D',AL1(4),AL1(13,255)                                          
         DC    C'E',AL1(4),AL1(14,255)                                          
         DC    C'F',AL1(4),AL1(15,255)                                          
         DC    C'G',AL1(4),AL1(16,255)                                          
         DC    C'H',AL1(4),AL1(17,255)                                          
         DC    C'I',AL1(4),AL1(18,255)                                          
         DC    C'J',AL1(4),AL1(19,255)                                          
         DC    C'K',AL1(4),AL1(20,255)                                          
         DC    C'L',AL1(4),AL1(21,255)                                          
         DC    C'M',AL1(4),AL1(22,255)                                          
         DC    C'N',AL1(4),AL1(23,255)                                          
         DC    X'FF',AL1(04),AL1(01,255)                                        
*                                                                               
         EJECT ,                                                                
*                                                                               
LEVTAB   DS    0CL21                                                            
         DC    AL1(1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,255)                 
         DC    AL1(1,2,0,0,1,2,0,0,1,2,0,0,2,0,0,0,1,0,0,0,255)                 
         DC    AL1(1,2,3,0,1,3,0,0,2,3,0,0,3,0,0,0,2,0,0,0,255)                 
         DC    AL1(1,2,3,4,2,3,4,0,2,4,0,0,4,0,0,0,3,0,0,0,255)                 
*                                                                               
SPECTAB  DS    0CL8                                                             
         DC    X'21',7X'00'                                                     
         DC    X'8121',6X'00'                                                   
         DC    X'814121',5X'00'                                                 
         DC    X'81824121',4X'00'                                               
         DC    X'8182834121',3X'00'                                             
         DC    X'818283844121',2X'00'                                           
         DC    X'81828384854121',X'00'                                          
         DC    X'8182838485864121'                                              
*                                                                               
EXTAB1   DC    AL1(2,3,4,14,5,6,7,15,18,255)                                    
EXTAB2   DC    AL1(8,9,10,16,11,12,13,17,20,255)                                
EXTAB3   DC    AL1(14,15,8,9,10,11,12,13,20,255)                                
EXTAB4   DC    AL1(14,15,18,16,17,19,20,255)                                    
*                                                                               
HEADL1   DC    C'-----------MONTH----------    '                                
         DC    C'  -----------Y.T.D----------  '                                
HEADL2   DC    C'HOURS        COST      PCT    '                                
         DC    C'  HOURS        COST      PCT  '                                
HEADL2A  DC    C'NET      GROSS        COST    '                                
         DC    C'  NET      GROSS        COST  '                                
HEADL2B  DC    C'PCT        PCT                '                                
         DC    C'  PCT        PCT              '                                
HEADL3   DC    C' 1ST       2ND       1ST       3'                              
         DC    C'RD       4TH       2ND     Y.T.D'                              
HEADL4   DC    C' QTR       QTR      HALF       Q'                              
         DC    C'TR       QTR      HALF     -----'                              
         EJECT ,                                                                
*                                                                               
ACCLEN   DC    F'2500'                                                          
REPTAB   DS    0CL18                                                            
REPTAB1  DC    C'P',AL1(1),C'C',AL1(1),AL3(HEADF1,DATAF1),CL8'        '         
REPTAB2  DC    C'C',AL1(1),C'P',AL1(1),AL3(HEADF1,DATAF1),CL8'        '         
REPTAB3  DC    C'C',AL1(1),C'P',AL1(1),AL3(HEADF2,DATAF2),CL8' H      '         
REPTAB4  DC    C'C',AL1(1),C'P',AL1(1),AL3(HEADF2,DATAF2),CL8'YH      '         
REPTAB5  DC    C'P',AL1(1),C'C',AL1(1),AL3(HEADF2,DATAF2),CL8' H      '         
REPTAB6  DC    C'P',AL1(1),C'C',AL1(1),AL3(HEADF2,DATAF2),CL8'YH      '         
REPTAB7  DC    C'C',AL1(1),C'P',AL1(1),AL3(HEADF2,DATAF2),CL8' C      '         
REPTAB8  DC    C'C',AL1(1),C'P',AL1(1),AL3(HEADF2,DATAF2),CL8'YC      '         
REPTAB9  DC    C'P',AL1(1),C'C',AL1(1),AL3(HEADF2,DATAF2),CL8' C      '         
REPTABA  DC    C'P',AL1(1),C'C',AL1(1),AL3(HEADF2,DATAF2),CL8'YC      '         
REPTABB  DC    C'C',AL1(1),C'P',AL1(1),AL3(HEADF3,DATAF3),CL8' H      '         
REPTABC  DC    C'P',AL1(1),C'C',AL1(1),AL3(HEADF3,DATAF3),CL8' H      '         
REPTABD  DC    C'C',AL1(1),C'P',AL1(1),AL3(HEADF3,DATAF3),CL8' C      '         
REPTABE  DC    C'P',AL1(1),C'C',AL1(1),AL3(HEADF3,DATAF3),CL8' C      '         
REPTABF  DC    C'C',AL1(1),C'P',AL1(1),AL3(HEADF4,DATAF4),CL8' H      '         
REPTABG  DC    C'P',AL1(1),C'C',AL1(1),AL3(HEADF4,DATAF4),CL8' H      '         
REPTABH  DC    C'C',AL1(1),C'P',AL1(1),AL3(HEADF4,DATAF4),CL8' C      '         
REPTADI  DC    C'P',AL1(1),C'C',AL1(1),AL3(HEADF4,DATAF4),CL8' C      '         
REPTABJ  DC    C'C',AL1(4),C'P',AL1(1),AL3(HEADF1,DATAF1),CL8'        '         
REPTABK  DC    C'P',AL1(1),C'C',AL1(4),AL3(HEADF1,DATAF1),CL8'        '         
REPTABL  DC    C'C',AL1(5),C'P',AL1(2),AL3(HEADF1,DATAF1),CL8'        '         
REPTABM  DC    C'P',AL1(2),C'C',AL1(5),AL3(HEADF1,DATAF1),CL8'        '         
REPTABN  DC    C'C',AL1(3),C'P',AL1(2),AL3(HEADF1,DATAF1),CL8'        '         
         EJECT ,                                                                
*              SPLIT SORT KEY INTO 8 15BYTE KEYS                                
*                                                                               
KEYSPLIT DS    0D                                                               
         NMOD1 0,***KYSP**                                                      
         L     RC,0(,R1)                                                        
         XC    THISKEYS,THISKEYS                                                
         XC    THISFRMS,THISFRMS                                                
         MVC   THISTYPE,SORTYPE                                                 
         ZAP   COUNTLV,=P'0'                                                    
         ZIC   RE,SORTYPE                                                       
         LA    RF,L'REPTAB         LENGTH OF REPTAB                             
         MR    RE,RE               SQUARED                                      
         L     RE,=A(REPTAB)       ->   REPTAB                                  
         A     RE,RELO                                                          
         AR    RE,RF               ->   REPTAB(RF)                              
         S     RE,=A(L'REPTAB)     ->   REPTAB-L'REPTAB(RF)                     
         LA    R1,THISKEY1                                                      
         LA    R2,THISFRM1                                                      
         LA    R3,THISLEV1                                                      
         LA    RF,SORTKEY1                                                      
         LA    R0,2                                                             
*                                                                               
KEYS2    ZIC   R4,1(,RE)                                                        
         BCTR  R4,0                                                             
         MH    R4,=H'16'                                                        
         CH    R0,=H'2'                                                         
         BNE   KEYS2B                                                           
         CLI   0(RE),C'C'                                                       
         BNE   KEYS2A                                                           
         MVC   CLIACCT,FIRSTCLI                                                 
         MVC   FIRSTCLI,SORTKEY1                                                
         B     KEYS2B                                                           
*                                                                               
KEYS2A   MVC   CLIACCT,FIRSTCLI                                                 
         MVC   FIRSTCLI,SORTKEY2                                                
*                                                                               
KEYS2B   CLI   0(RE),C'C'                                                       
         BE    KEYS2C                                                           
         LA    R4,PERHEIR(R4)                                                   
         B     KEYS2D                                                           
*                                                                               
KEYS2C   LA    R4,CLIHEIR(R4)                                                   
*                                                                               
KEYS2D   LA    R5,4                                                             
*                                                                               
KEYS4    CLI   1(R4),0                                                          
         BE    KEYS6                                                            
         CLI   2(RF),C'N'                                                       
         BNE   KEYS4A                                                           
         MVC   0(L'THISKEY1,R1),0(RF)                                           
         MVC   0(L'THISFRM1,R2),3(RF)                                           
         B     KEYS4B                                                           
*                                                                               
KEYS4A   ZIC   R6,2(,R4)                                                        
         LA    R6,2(,R6)                                                        
         MVC   0(L'THISKEY1,R1),SPACES                                          
         EXMVC R6,0(R1),0(RF)                                                   
         ZIC   R6,0(,R4)                                                        
         LA    R6,3(RF,R6)                                                      
         ZIC   R7,1(,R4)                                                        
         BCTR  R7,0                                                             
         MVC   0(L'THISFRM1,R2),SPACES                                          
         EXMVC R7,0(R2),0(R6)                                                   
*                                                                               
KEYS4B   ZIC   R6,3(,R4)                                                        
         BCTR  R6,0                                                             
         MH    R6,=H'15'                                                        
         LA    R7,CLILEV(R6)                                                    
         CLI   0(RE),C'C'                                                       
         BE    *+8                                                              
         LA    R7,PERLEV(R6)                                                    
         MVC   0(15,R3),0(R7)                                                   
         AP    COUNTLV,=P'1'                                                    
         LA    R1,L'THISKEY1(,R1)                                               
         LA    R2,L'THISFRM1(,R2)                                               
         LA    R3,L'THISLEV1(,R3)                                               
*                                                                               
KEYS6    LA    R4,4(,R4)                                                        
         BCT   R5,KEYS4                                                         
         LA    RE,2(,RE)                                                        
         LA    RF,L'SORTKEY1(,RF)                                               
         BCT   R0,KEYS2                                                         
         CVB   R0,COUNTLV                                                       
         STC   R0,LEVELS                                                        
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*              HEADLINE FORMAT ROUTINES                                         
*                                                                               
         USING HEADLND,R5                                                       
*                                                                               
         DS    0D                                                               
HEADF1   NMOD1 0,*HEADF1*                                                       
         L     RC,0(,R1)                                                        
         L     R5,=A(HEADLNS)                                                   
         MVC   HEADG,SPACES                                                     
         MVC   HEADH,SPACES                                                     
         MVC   HEADI,SPACES                                                     
         MVI   CHOPSPEC,45                                                      
         L     R2,=A(HEADL1)                                                    
         A     R2,RELO                                                          
         MVC   HEADG+51(60),0(R2)  HEADL1                                       
         L     R2,=A(HEADL2)                                                    
         A     R2,RELO                                                          
         MVC   HEADH+51(60),0(R2)  HEADL2                                       
*                                                                               
         CLI   PROGPROF+2,C'2'     OPTION FOR BBDO                              
         BNE   HEADF1X                                                          
         L     R2,=A(HEADL2A)                                                   
         A     R2,RELO                                                          
         MVC   HEADH+51(60),0(R2)  HEADL2A                                      
         L     R2,=A(HEADL2B)                                                   
         A     R2,RELO                                                          
         MVC   HEADI+51(60),0(R2)  HEADL2B                                      
*                                                                               
HEADF1X  XMOD1 1                                                                
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
         USING HEADLND,R5                                                       
*                                                                               
         DS    0D                                                               
HEADF2   NMOD1 0,*HEADF2*                                                       
         L     RC,0(,R1)                                                        
         L     R5,=A(HEADLNS)                                                   
         MVC   HEADG,SPACES                                                     
         MVC   HEADH,SPACES                                                     
         MVC   HEADI,SPACES                                                     
         GOTO1 GETMONTH,DMCB,(RC)                                               
         LA    R1,HEADG+31                                                      
         LA    R2,MNTHLIST                                                      
         CLI   FORMSPEC,C'Y'                                                    
         BNE   *+8                                                              
         LA    R2,30(,R2)                                                       
         LA    R3,2                                                             
*                                                                               
HEADF22  LA    R4,3                                                             
*                                                                               
HEADF24  MVC   0(5,R1),0(R2)                                                    
         MVC   132(5,R1),=C'-----'                                              
         LA    R1,9(R1)                                                         
         LA    R2,5(R2)                                                         
         BCT   R4,HEADF24                                                       
         MVC   2(3,R1),=C'QTR'                                                  
         MVC   134(3,R1),=C'---'                                                
         LA    R1,9(R1)                                                         
         BCT   R3,HEADF22                                                       
         MVC   0(5,R1),=C' HALF'                                                
         MVC   132(5,R1),=C' YEAR'                                              
         CLI   FORMSPEC,C'Y'                                                    
         BNE   HEADF25                                                          
         MVC   0(5,R1),=C'Y.T.D'                                                
         MVC   132(5,R1),=C'-----'                                              
*                                                                               
HEADF25  MVI   CHOPSPEC,27                                                      
         XMOD1 1                                                                
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
         USING HEADLND,R5                                                       
*                                                                               
         DS    0D                                                               
HEADF3   NMOD1 0,*HEADF3*                                                       
         L     RC,0(,R1)                                                        
         L     R5,=A(HEADLNS)                                                   
         MVC   HEADG,SPACES                                                     
         MVC   HEADH,SPACES                                                     
         MVC   HEADI,SPACES                                                     
         GOTO1 GETMONTH,DMCB,(RC)                                               
         MVC   HEADG+31(5),MNTHLIST                                             
         MVC   HEADH+31(5),MNTHLIST+10                                          
         MVI   HEADG+36,C'-'                                                    
         MVC   HEADG+40(5),MNTHLIST+15                                          
         MVC   HEADH+40(5),MNTHLIST+25                                          
         MVI   HEADG+45,C'-'                                                    
         LA    R1,HEADG+49                                                      
         LA    R2,MNTHLIST+30                                                   
         LA    R3,6                                                             
*                                                                               
HEADF32  MVC   0(5,R1),0(R2)                                                    
         MVC   132(5,R1),=C'-----'                                              
         LA    R1,9(,R1)                                                        
         LA    R2,5(,R2)                                                        
         BCT   R3,HEADF32                                                       
         MVC   0(5,R1),=C'Y.T.D'                                                
         MVC   132(5,R1),=C'-----'                                              
         MVI   CHOPSPEC,27                                                      
         XMOD1 1                                                                
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
         USING HEADLND,R5                                                       
*                                                                               
         DS    0D                                                               
HEADF4   NMOD1 0,*HEADF4*                                                       
         L     RC,0(,R1)                                                        
         L     R5,=A(HEADLNS)                                                   
         MVC   HEADG,SPACES                                                     
         MVC   HEADH,SPACES                                                     
         MVI   CHOPSPEC,36                                                      
         L     R2,=A(HEADL3)                                                    
         A     R2,RELO                                                          
         MVC   HEADG+44(64),0(R2)  HEADL3                                       
         L     R2,=A(HEADL4)                                                    
         A     R2,RELO                                                          
         MVC   HEADH+44(64),0(R2)  HEADL4                                       
         XMOD1 1                                                                
*                                                                               
         DROP  R5                  KEEP IT CLEAN                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*              BUILD LIST OF ALPHA MONTHS FOR FISCAL YEAR                       
*                                                                               
         DS    0D                                                               
GETMONTH NMOD1 0,*GTMTH*                                                        
         L     RC,0(,R1)                                                        
         MVC   WORK(4),QSTART      START DATE YYMM                              
         MVC   WORK+4(2),=C'01'    DAY   01                                     
         LA    R3,MNTHLIST                                                      
         LA    R0,12                                                            
*                                                                               
GETM2    GOTO1 DATCON,DMCB,(0,WORK),(11,WORK+6)    MMM01/YY                     
         MVC   0(3,R3),WORK+6                      MMM                          
         MVC   3(2,R3),WORK+12                     YY                           
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,F'1'    NEXT MONTH                   
         LA    R3,5(,R3)                           NEXT ENTRY                   
         BCT   R0,GETM2                            LOOP                         
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              PRINT HIRE AND/OR TERMINATION DATE ON REPORT                     
*                                                                               
         DS    0D                                                               
HNTDATE  NMOD1 0,***HNT**                                                       
         L     RC,0(,R1)           PARM1-GLOBAL WORK STORAGE                    
         L     R5,4(,R1)           PARM2-HEADLINE TO PRINT IN                   
         L     R6,8(,R1)           PARM3-LENGTH OF PREV SQUASH                  
         AR    R5,R6                                                            
         LA    R5,2(,R5)           ADD  ONE FOR A SPACE                         
         LR    R6,R5                                                            
*                                                                               
         CLI   QOPT4,C'B'                                                       
         BE    *+8                                                              
         CLI   QOPT4,C'T'                                                       
         BE    *+8                                                              
         CLI   QOPT4,C'H'                                                       
         BNE   HNT9                                                             
*                                                                               
         OC    BUFFHIRE,BUFFHIRE   SEE  IF THERE IS A HIRE DATE                 
         BZ    HNT1                                                             
         CLI   QOPT4,C'T'                                                       
         BE    HNT1                                                             
         MVI   0(R5),C'*'                                                       
         LA    R5,1(,R5)                                                        
         MVC   0(2,R5),=C'H='                                                   
         LA    R5,2(,R5)                                                        
         GOTO1 DATCON,DMCB,(1,BUFFHIRE),(5,(R5))                                
         LA    R5,8(,R5)                                                        
         CLI   QOPT4,C'H'                                                       
         BE    HNT8                                                             
*                                                                               
HNT1     DS    0H                                                               
         OC    BUFFTERM,BUFFTERM                                                
         BZ    HNT8                                                             
         CLI   QOPT4,C'T'                                                       
         BE    HNT4                                                             
*                                                                               
         OC    BUFFHIRE,BUFFHIRE                                                
         BZ    HNT3                                                             
         MVC   0(2,R5),=X'6B42'                                                 
         LA    R5,2(,R5)                                                        
         B     HNT4                                                             
*                                                                               
HNT3     DS    0H                                                               
         MVI   0(R5),C'*'                                                       
         LA    R5,1(,R5)                                                        
*                                                                               
HNT4     DS    0H                                                               
         MVC   0(2,R5),=C'T='                                                   
         LA    R5,2(,R5)                                                        
         GOTO1 DATCON,DMCB,(1,BUFFTERM),(5,(R5))                                
         LA    R5,8(,R5)                                                        
*                                                                               
HNT8     DS    0H                                                               
         CLI   0(R6),C'*'                                                       
         BNE   HNT9                                                             
         MVI   0(R5),C'*'                                                       
*                                                                               
HNT9     DS    0H                                                               
         XMOD1 1                                                                
         LTORG                                                                  
*                                                                               
         EJECT ,                                                                
*              BUFFER AREAS                                                     
*                                                                               
         BUFF  LINES=200,FLAVOR=DATA,COMMENT=42,KEYLIST=(15,A)                  
*                                                                               
ACCBUFF  DS    0D                                                               
         DS    2500C                                                            
*                                                                               
MEMOPOOL DS    0D                                                               
         DS    40000C                                                           
*                                                                               
HEADLNS  DS    0D                                                               
         DS    1320C                                                            
*                                                                               
IOAREA   DS    2048C                                                            
         EJECT ,                                                                
*              DSECT TO COVER SPACEND                                           
*                                                                               
WORKD    DSECT                                                                  
DUB1     DS    D                                                                
COUNTLV  DS    D                                                                
PWK1     DS    PL16                                                             
*                                                                               
RELO     DS    F                                                                
*                                                                               
VADCONS  DS    0F                                                               
VSORTER  DS    V                                                                
VBUFF    DS    V                                                                
VACCUMS  DS    V                                                                
VSQUASH  DS    V                                                                
VUNDER   DS    V                                                                
VMEMO    DS    V                                                                
VGETBRD  DS    V                                                                
VPRNTBL  DS    V                                                                
AKEYSPIT DS    A                                                                
AHNTDATE DS    A                                                                
AIOAREA  DS    A                                                                
*                                                                               
REG1     DS    F                        SAVE REGISTER R1                        
REGE     DS    F                        SAVE REGISTER RE                        
SOURCE   DS    A                                                                
DESTIN   DS    A                                                                
*                                                                               
         DS    0F                                                               
AROUT    DS    0CL8                                                             
AHEAD    DS    A                                                                
AFORM    DS    A                                                                
*                                                                               
WORKAREA DS    CL132                                                            
*                                  **** BEGIN KEEP TOGETHER ****                
VALUES   DS    0CL200                                                           
REQMONTH DS    CL8                 FISCAL    REQUESTED MONTH                    
MONTH    DS    12CL8                         MONTHS                             
QTR      DS    4CL8                          QUARTERS                           
HALFYR   DS    2CL8                          HALF-YEARS                         
YTD      DS    CL8                           YEAR-TO-DATE                       
THREEM   DS    CL8                 CALENDAR  3 MONTH                            
TWELVE   DS    CL8                           12 MONTH                           
BRDMONTH DS    CL4                                                              
BRDYTD   DS    CL4                                                              
TOTMONTH DS    CL4                                                              
TOTYTD   DS    CL4                                                              
MONTHGP  DS    CL4                 MONTH GROSS PERCENT                          
YTDGP    DS    CL4                 YTD GROSS PERCENT                            
         ORG   VALUES                                                           
PERSACCS DS    CL96                                                             
TOTACCS  DS    CL96                                                             
         ORG                                                                    
*                                  **** END   KEEP TOGETHER ****                
SAVEACCS DS    CL200                                                            
SAVETOTS DS    CL200                                                            
ACCLINE1 DS    CL52                                                             
ACCLINE2 DS    CL52                                                             
*                                                                               
*              SORT RECORD                                                      
*                                                                               
         DS    0F                                                               
SORTREC  DS    0CL144                                                           
SORTKEY  DS    0CL48                                                            
SORTYPE  DS    CL1                                                              
SORTKEY1 DS    CL15                                                             
SORTKEY2 DS    CL15                                                             
SORTKEY3 DS    CL15                                                             
         DS    CL2                                                              
SORTACCS DS    0CL96                                                            
SORTHOUR DS    F                                                                
SORTCOST DS    F                                                                
         DS    CL88                                                             
*                                                                               
*              BUFFALO RECORD                                                   
*                                                                               
BUFFREC  DS    0CL57                                                            
BUFFKEY  DS    CL15                                                             
BUFFNAME DS    CL36                                                             
BUFFHIRE DS    CL3                                                              
BUFFTERM DS    CL3                                                              
         EJECT ,                                                                
*                                                                               
THISTYPE DS    CL1                                                              
THISKEYS DS    0CL120                                                           
THISKEY1 DS    CL15                                                             
THISKEY2 DS    CL15                                                             
THISKEY3 DS    CL15                                                             
THISKEY4 DS    CL15                                                             
THISKEY5 DS    CL15                                                             
THISKEY6 DS    CL15                                                             
THISKEY7 DS    CL15                                                             
THISKEY8 DS    CL15                                                             
THISFRMS DS    0CL216                                                           
THISFRM1 DS    CL12                                                             
THISFRM2 DS    CL12                                                             
THISFRM3 DS    CL12                                                             
THISFRM4 DS    CL12                                                             
THISFRM5 DS    CL12                                                             
THISFRM6 DS    CL12                                                             
THISFRM7 DS    CL12                                                             
THISFRM8 DS    CL12                                                             
THISLEV1 DS    CL15                                                             
THISLEV2 DS    CL15                                                             
THISLEV3 DS    CL15                                                             
THISLEV4 DS    CL15                                                             
THISLEV5 DS    CL15                                                             
THISLEV6 DS    CL15                                                             
THISLEV7 DS    CL15                                                             
THISLEV8 DS    CL15                                                             
         EJECT ,                                                                
*                                                                               
LASTTYPE DS    CL1                                                              
LASTKEYS DS    0CL120                                                           
LASTKEY1 DS    CL15                                                             
LASTKEY2 DS    CL15                                                             
LASTKEY3 DS    CL15                                                             
LASTKEY4 DS    CL15                                                             
LASTKEY5 DS    CL15                                                             
LASTKEY6 DS    CL15                                                             
LASTKEY7 DS    CL15                                                             
LASTKEY8 DS    CL15                                                             
LASTFRMS DS    0CL216                                                           
LASTFRM1 DS    CL12                                                             
LASTFRM2 DS    CL12                                                             
LASTFRM3 DS    CL12                                                             
LASTFRM4 DS    CL12                                                             
LASTFRM5 DS    CL12                                                             
LASTFRM6 DS    CL12                                                             
LASTFRM7 DS    CL12                                                             
LASTFRM8 DS    CL12                                                             
LASTLEV1 DS    CL15                                                             
LASTLEV2 DS    CL15                                                             
LASTLEV3 DS    CL15                                                             
LASTLEV4 DS    CL15                                                             
LASTLEV5 DS    CL15                                                             
LASTLEV6 DS    CL15                                                             
LASTLEV7 DS    CL15                                                             
LASTLEV8 DS    CL15                                                             
         EJECT ,                                                                
*                                                                               
FIRSTCLI DS    CL15                                                             
CLIACCT  DS    CL15                                                             
PERACCT  DS    CL15                                                             
TEMPKEY  DS    CL15                                                             
CLIHEIR  DS    CL80                                                             
PERHEIR  DS    CL80                                                             
CLILEV   DS    4CL15                                                            
PERLEV   DS    4CL15                                                            
WANT     DS    CL1                                                              
SORTFRST DS    CL1                                                              
THISACC  DS    C                                                                
SORTSW   DS    C                                                                
PERCSW   DS    C                                                                
SORTORD  DS    C                                                                
DATAB    DS    CL28                                                             
BRDTAB   DS    XL12                                                             
MNTHLIST DS    CL60                                                             
ACCKEY   DS    CL15                                                             
ACCFRM   DS    CL12                                                             
ACCLEV   DS    CL15                                                             
ACCNAM   DS    CL36                                                             
HEADSPEC DS    CL8                                                              
FORMSPEC DS    CL8                                                              
CHOPSPEC DS    X                                                                
HEDFORM  DS    C                                                                
MIDFORM  DS    C                                                                
MIDPRINT DS    C                                                                
FIRST    DS    C                                                                
TOTLEV   DS    X                                                                
LEVELS   DS    X                                                                
INDEX1   DS    X                                                                
INDEX2   DS    X                                                                
MIDA     DS    CL132                                                            
MIDB     DS    CL132                                                            
PA       DS    CL132                                                            
PB       DS    CL132                                                            
PC       DS    CL132                                                            
PD       DS    CL132                                                            
TEMPHIRE DS    CL3                                                              
TEMPTERM DS    CL3                                                              
METHOD   DS    CL1                 NEW COST METHOD OF ALLOCATION                
TOTP1    DS    F                                                                
TOTP2    DS    F                                                                
TOTP3    DS    CL120               KEEP TOTAL PERCENT LINES                     
READKEY  DS    CL49                                                             
EDATEC   DS    CL6                 END  DATE CHARACTER FORMAT                   
         EJECT ,                                                                
*              DSECT FOR HEADLINES                                              
HEADLND  DSECT                                                                  
HEADA    DS    CL132                                                            
HEADB    DS    CL132                                                            
HEADC    DS    CL132                                                            
HEADD    DS    CL132                                                            
HEADE    DS    CL132                                                            
HEADF    DS    CL132                                                            
HEADG    DS    CL132                                                            
HEADH    DS    CL132                                                            
HEADI    DS    CL132                                                            
         EJECT ,                                                                
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*DDBUFFALOD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREP9202 06/03/15'                                      
         END                                                                    
