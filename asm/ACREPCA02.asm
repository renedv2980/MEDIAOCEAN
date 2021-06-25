*          DATA SET ACREPCA02  AT LEVEL 023 AS OF 03/21/12                      
*PHASE ACCA02B,+0                                                               
*INCLUDE ACSLRY                                                                 
*INCLUDE ACSALHST                                                               
*INCLUDE BINSR31                                                                
*INCLUDE GETCAP                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ACTRAVL                                                                
*INCLUDE BUFFERIN                                                               
         TITLE 'ACCA02 -  TIME/COST BUCKET ALLOCATION '                         
*                                                                               
ACCA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCA**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         LA    RC,SPACEND                                                       
         USING ACCAD,RC            RC=A(LOCAL W/S)                              
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,LEVAFRST                                                    
         BE    LEVAF                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    LEVBF                                                            
         CLI   MODE,LEVCFRST                                                    
         BE    LEVCF                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,SBACFRST                                                    
         BE    PSUB                                                             
         CLI   MODE,PROCHIST                                                    
         BE    PHIS                                                             
         CLI   MODE,SBACLAST                                                    
         BE    SUBL                                                             
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,LEVBLAST                                                    
         BE    LEVBL                                                            
         CLI   MODE,LEVALAST                                                    
         BE    LEVAL                                                            
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* FIRST FOR RUN                                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING MASTD,R3                                                         
RUNF     DS    0H                                                               
         L     R3,ADMASTC                                                       
         LA    R2,VADDTRN          LOADABLE PHASES                              
         MVC   MCDUB,=CL8'T00AXX'                                               
RUNF10   GOTO1 HEXOUT,DMCB,0(R2),MCDUB+4,1,0,0                                  
         GOTO1 MCVLOADM,DMCB,0                                                  
         BE    *+6                 TEST LOAD WAS OK                             
         DC    H'0'                                                             
         MVC   0(4,R2),4(R1)                                                    
         LA    R2,4(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   RUNF10                                                           
         DROP  R3                                                               
*                                                                               
         L     RE,VADDTRN                                                       
         ST    RE,ADDTRN           MOVE ADDRESS TO GLOBAL STORAGE               
*                                                                               
         LA    RE,RELOTAB                                                       
         LA    R1,ATYPES                                                        
RUNF20   L     RF,0(RE)            ADDRESSES TO GLOBAL STORAGE                  
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),FOX                                                        
         BNE   RUNF20                                                           
*                                  SET UP COMMON ENTRIES TO WORK NMODS          
         L     R2,COMTAB                                                        
         USING COMD,R2                                                          
RUNF30   CLI   COMRELO,EOT         END OF TABLE?                                
         BE    RUNF40                                                           
         SR    R0,R0                                                            
         IC    R0,COMNUMB          NUMBER OF ROUTINES                           
         SR    RE,RE                                                            
         ICM   RE,3,COMRELO        RE IS RELOCATED ADDR OF NMOD                 
         AR    RE,RC                                                            
         L     RE,0(RE)                                                         
         SR    RF,RF                                                            
         ICM   RF,3,COMENT                                                      
         AR    RF,RC                                                            
         LA    RF,0(RF)            RF ADDR OF 1ST ENTERABLE ROUT                
         SR    R1,R1                                                            
         ST    RE,0(RF)            ADDRESS OF COMMON                            
         STC   R1,0(RF)            SET ROUTINE NUMBER                           
         LA    R1,1(R1)                                                         
         LA    RF,4(RF)            NEXT ENTERABLE ROUTINE                       
         BCT   R0,*-16                                                          
         LA    R2,COMLEN(R2)       NEXT TABLE ENTRY                             
         B     RUNF30                                                           
*                                  LITERALS TO GLOBAL STORAGE                   
RUNF40   DS    0H                                                               
         L     R2,VEXTRAS          SET UP BOX ROUTINE                           
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,BXHOOK                                                        
         ST    R2,HEADHOOK                                                      
         DROP  R2                                                               
*                                                                               
         MVC   LITVALS(LITVALSL),GLOBALS                                        
*                                                                               
         MVI   SAVEMET,SPACE       CLEAR PREV REQUESTED METHOD CODE             
         MVC   SVLOC(SVLOCLN),SPACES  CLEAR SAVED LOCATION FIELDS               
         XC    RUNSTAT,RUNSTAT       RESET RUN STATUS                           
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         MVC   UPSI,MCUPSI           TEST OPTIONS                               
         MVC   CTRY,RCCTRY           COUNTRY CODE                               
         OC    MCREMPQK,MCREMPQK     SOON RUN?                                  
         BZ    *+8                                                              
         OI    RUNSTAT,RUNSOON       MAKE NOTE OF IT                            
         DROP  RE                                                               
         GOTO1 MAIN                  ACQUIRE STORAGE                            
         L     RE,ACOBLOCK           CLEAR THE COBLOCK                          
         L     RF,=A(COBLOCKX-COBLOCK)                                          
         XCEFL                                                                  
         L     RF,ASALAREA           CLEAR SALARY AREA                          
         XC    SALBLOCK-SALARYD(SALLNQ,RF),SALBLOCK-SALARYD(RF)                 
         L     RF,NEWBIZBF           CLEAR 1C NEW BIZ TABLE                     
         XC    BININ-BIND(L'BININ,RF),BININ-BIND(RF)                            
*                                    GLOBAL DICTIONARY ENTRIES                  
         GOTO1 ADDICTAT,DMCB,C'LU  ',DDIN,DDOUT                                 
*                                                                               
         L     R2,DICTTAB            SEED TABS WITH DICTIONARY ENTRIES          
         USING DICD,R2                                                          
RUNF50   CLI   DICTAB,EOT                                                       
         BE    RUNF80                                                           
         SR    RE,RE                                                            
         ICM   RE,3,DICTAB                                                      
         AR    RE,RC                                                            
         L     R3,0(RE)              R3 IS CURRENT TABLE TO UPDATE              
RUNF60   CLI   0(R3),EOT             CURRENT TABLE COMPLETE?                    
         BE    RUNF70                YES BUMP TO NEW TABLE                      
         LR    R4,R3                                                            
         AH    R4,DICENTRY           R4 IS THE ENTRY TO UPDATE                  
         GOTO1 ADDICTAT,DMCB,C'SU  ',(R4),0                                     
         AH    R3,DICBUMP            BUMP TO NEXT TABLE ENTRY                   
         B     RUNF60                                                           
RUNF70   LA    R2,DICLEN(R2)                                                    
         B     RUNF50                                                           
*                                                                               
RUNF80   DS    0H                                                               
         BAS   RE,READCO           READ COMPANY REC                             
         GOTO1 BLDTAB                BUILD 1N TABLE                             
         GOTO1 BOFFL                 BUILD OFFICE/GROUP TABLE                   
         ZAP   FILCOUNT,=P'0'                                                   
         GOTO1 ACTRAVL,DMCB,(1,ADCOMFAC),LOGIO                                  
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* FIRST FOR REQUEST                                                  *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   FCRDACC,C'Y'                                                     
         MVI   FCRDHIST,C'Y'                                                    
*                                                                               
* TEST FOR ANY DUMPS IN THIS RUN                                                
*                                                                               
         L     RE,ADMASTC                                                       
         CP    MCNDUMPS-MASTD(L'MCNDUMPS,RE),ZEROS                              
         BE    REQF10              NO  - CONTINUE                               
         MVI   FCRDACC,C'N'        YES - DON'T PROCESS ANYMORE REQUESTS         
         MVI   FCRDTRNS,C'N'             SINCE YTD WILL BE SCREWED UP           
         MVI   FCRDHIST,C'N'                                                    
         B     REQFX                                                            
*                                                                               
REQF10   DS    0H                                                               
         TM    RUNSTAT,RUNSOON     SOON RUN?                                    
         BO    REQF20                                                           
         OPEN  (COSTRCV,(OUTPUT))  OPEN RECOVERY FILE                           
         LTR   RF,RF               SUCCESSFUL OPEN?                             
         BZ    *+6                 YES - CONTINUE                               
         DC    H'0'                NO  - DEATH                                  
*                                                                               
REQF20   MVI   FORCEHED,C'Y'       FORCE FIRST HEADING                          
         MVC   PAGE,=H'1'          AND PAGE NUMBER                              
*                                                                               
* INIT FOR SORT **                                                              
*                                                                               
         XC    ALSORT,ALSORT       CLEAR A(LAST SORT)                           
         LA    R1,SRTKLEN          SORT KEY LENGTH                              
         CVD   R1,DUB              CONVERT KEY LEN TO CHARS                     
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTLEN           SORT RECORD LENGTH                           
         CVD   R1,DUB              CONVERT REC LEN TO CHARS                     
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
* SET FIXED OPTIONS                                                             
*                                                                               
         XC    OPTIONS(OPTLNTH),OPTIONS                                         
         L     R3,FIXOPTS          FIXED OPTIONS TABLE                          
         USING FIXOD,R3                                                         
REQF30   CLI   0(R3),EOT           END OF TABLE                                 
         BE    REQF40                                                           
         LA    RE,ACCAD                                                         
         MVC   HALF,FIXSOPT                                                     
         AH    RE,HALF             POINT TO OPTION IN WORKING STORAGE           
         MVC   0(L'FIXDFLT,RE),FIXDFLT                                          
         LA    R3,FIXOLN(R3)                                                    
         B     REQF30                                                           
*                                                                               
REQF40   MVI   METHOD,C'1'         *** SET METHOD TO DEFAULT ****               
         CLI   QMTHD,SPACE                                                      
         BNH   *+10                                                             
         MVC   METHOD,QMTHD        METHOD FROM REQUEST                          
         XC    BYTE,BYTE                                                        
         BAS   RE,METVAL           VALIDATE METHOD                              
         BE    REQF50                                                           
         MVI   FCRDACC,C'N'                                                     
         MVI   FCRDHIST,C'N'                                                    
         B     REQFX                                                            
*                                                                               
REQF50   BAS   RE,INCCEK           CHECK FOR INCOME OR HOURS PROFILES           
         GOTO1 BCLIL               CHECK FOR CLIENT LISTS ON 1N                 
         L     R4,ACOBLOCK         INITIALIZE COBLOCK FOR METHOD                
         USING COBLOCKD,R4                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(8),COBLOCK+8   ADDR AND LENGTH OF ACQUIRED BUFFER           
         LA    RE,COBLOCK          CLEAR THE BLOCK                              
         L     RF,=A(COBLOCKX-COBLOCK)                                          
         XCEFL                                                                  
         MVC   COBLOCK+8(8),WORK   RESTORE ADDR/LEN OF BUFFER                   
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,RCCOMPFL     COMPANY                                      
         MVC   COKMTHD,METHOD      COSTING METHOD                               
         MVC   COACOVL,COVAIL      ADDR OF COVAIL                               
         MVC   COABINSR,BINSRCH    ADDR OF BINSRCH                              
         MVC   COADM,DATAMGR       ADDR OF DATAMGR                              
         GOTO1 PROFIT,DMCB,0,0,0,0  LOOKUP METHOD LEVEL PROFILES                
*                                                                               
         CLI   OPTOVHY,C'Y'        OVERHEAD ON YEAR TO DATE                     
         BNE   REQF60                                                           
         CLI   OPTYTD,C'Y'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T HAVE OPTOVHY=Y IF OPTYTD=Y             
*                                                                               
REQF60   L     R7,AAGYCUM                                                       
         USING AGYD,R7                                                          
         LA    R2,AGBK             CLEAR AGENCY CUM TABLE                       
         LA    R0,AGBKCNT                                                       
         ZAP   0(AGBUKLN,R2),ZEROS                                              
         LA    R2,AGBUKLN(R2)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         LA    R0,WRKNUM           NUMBER OF REPORT WORK  ACCUMS                
         LA    R1,WRKACM           LOCATION OF FIRST BUCKET                     
         ZAP   0(WRKABKLN,R1),ZEROS                                             
         LA    R1,WRKABKLN(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         USING BIND,R5             CLEAR BINARY TABLES                          
         LA    R0,CCLRNUM          NUMBER OF TABLES TO CLEAR                    
         L     R1,CLRTAB           ADDR OF CLEAR TABLE                          
REQF70   SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         AR    RE,RC                                                            
         L     R5,0(RE)                                                         
         XC    BININ,BININ         CLEAR NUMBER IN TABLE                        
         LA    R1,2(R1)                                                         
         BCT   R0,REQF70                                                        
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         LA    R0,BBUFFAINI         INITIALIZE BUFFERIN                         
         L     RF,=A(YTDBUF)                                                    
         GOTO1 BUFFERIN,DMCB,((R0),(RF)),(0,0),ADCOMFAC                         
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LIVEOPT,SPACES                                                   
         MVC   LIVEOPT(2),=C'**'                                                
         MVC   LIVEOPT+3(L'AC@DRAFT),AC@DRAFT     ** DRAFT **                   
         LA    RF,LIVEOPT+3+L'AC@DRAFT                                          
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(2,RF),=C'**'                                                   
         CLI   QOPT1,C'L'                                                       
         BNE   REQF80                                                           
         TM    RUNSTAT,RUNSOON     SOON RUN CAN ONLY BE DRAFT                   
         BNO   *+12                                                             
         MVI   QOPT1,C' '                                                       
         B     REQF80                                                           
         OI    RUNSTAT,RUNLIVE                                                  
         MVC   LIVEOPT,SPACES                                                   
         MVC   LIVEOPT(2),=C'**'                                                
         MVC   LIVEOPT+3(L'AC@LIVE),AC@LIVE       ** LIVE **                    
         LA    RF,LIVEOPT+3+L'AC@LIVE                                           
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(2,RF),=C'**'                                                   
*                                                                               
REQF80   CLI   QOPT1,C'E'          ERRORS ONLY?                                 
         BNE   *+8                                                              
         MVI   QOPT3,C'S'          YES -FORCE SUPPRESS OVH/IND PAGES            
         CLI   OPTYTD,C'Y'         ALLOCATION RUN YTD?                          
         BE    REQF110                                                          
         CLI   OPTYTD,C'Q'         ALLOCATION IS RUN QTRLY?                     
         BE    REQF90                                                           
*                                  ALLOCATION MUST BE MONTHLY                   
         CLC   QSTART(4),QEND      START AND END MUST BE EQUAL                  
         BE    REQF110                                                          
         MVC   QSTART,QEND         IF NOT EQUAL THEN COPY END TO START          
         B     REQF100                                                          
*                                                                               
REQF90   MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         L     R3,=F'65'                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         CLC   QEND(4),WORK+6                                                   
         BE    REQF110              QTR. ALLOC MUST HAVE QTRLY DATES            
*                                                                               
         USING ERRORD,R2                                                        
REQF100  MVC   WORK,SPACES                                                      
         LA    R2,WORK                                                          
         MVI   ERRNUM,ERRDATES                                                  
         MVI   ERRTYPE,ERRWARN   SET TO WARNING                                 
         GOTO1 BINADD,DMCB,(R2),ERRORBUF ADD TO TABLE                           
*                                                                               
REQF110  DS    0H                                                               
         MVC   HEADMON,SPACES                                                   
         MVC   QSTART+4(2),=C'01'                                               
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,(C'Y',QEND),(X'80',WORK+20),0 LAST DAY OF MO          
         MVC   QEND+4(2),WORK+24                     END OF THE MONTH           
*                                      START AND END ARE YMD(PACKED)            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         GOTO1 (RF),(R1),,(9,HEADMON)                                           
         MVI   HEADMON+7,C'-'                                                   
         GOTO1 (RF),(R1),(0,QEND),(1,END)                                       
         GOTO1 (RF),(R1),,(9,HEADMON+9)                                         
         GOTO1 (RF),(R1),(4,RCDATE),(2,TODAYC)  TODAY COMPRESSED                
         GOTO1 (RF),(R1),(4,RCDATE),(1,TODAYP)  TODAY PACKED YMD                
*                                                                               
         MVC   MOS,END             BUCKET MONTH FOR POSTINGS                    
         MVC   STEND(2),START      STEND IS FOR ACSLRY                          
         MVC   STEND+2(2),END                                                   
         MVC   MOST(1),QEND+1      TRNMOS FOR ADDTRN                            
         MVC   MOST+1(1),QEND+3                                                 
         CLI   QEND+2,C'1'                                                      
         BNE   REQF120                                                          
         MVI   MOST+1,C'A'                                                      
         CLI   QEND+3,C'0'                                                      
         BE    REQF120                                                          
         MVI   MOST+1,C'B'                                                      
         CLI   QEND+3,C'1'                                                      
         BE    REQF120                                                          
         MVI   MOST+1,C'C'                                                      
*                                                                               
* WORK OUT PREVIOUS MONTH/QTR DATE                                              
*      FOR YTD-1 TESTS                                                          
*                                                                               
REQF120  L     R3,=F'-34'                                                       
         CLI   OPTPERD,C'M'        RUN PERIOD IS MTHLY OR QTRLY                 
         BE    *+8                                                              
         L     R3,=F'-100'                                                      
         GOTO1 ADDAY,DMCB,QEND,WORK,(R3)                                        
         GOTO1 DATCON,DMCB,(0,WORK),(1,LAST)                                    
*                                                                               
         XC    FISCAL,FISCAL       GET START OF FINANCIAL YEAR                  
         MVC   FISCAL,END                                                       
         MVI   FISCAL+1,1          FORCE JAN AS MONTH                           
         L     R2,ADCMPEL                                                       
         USING CPYELD,R2                                                        
         CLI   CPYSFST,0           WAS A STARTING MTH SPECIFIED/                
         BE    REQF130             NO, LEAVE AS '01' & CHECK START              
         MVC   WORK(1),CPYSFST     YES - ISOLATE IT                             
         NI    WORK,X'0F'                                                       
         SR    R1,R1                                                            
         IC    R1,WORK                                                          
         TM    CPYSFST,X'F0'       WAS IT A NUMBER?                             
         BO    *+8                                                              
         LA    R1,15(0,R1)         NO, ADD CONVERSION FACTOR                    
         STC   R1,FISCAL+1         UPDATE FIELD WITH MTH                        
*                                                                               
REQF130  CLC   FISCAL,END          HOW DOES IT COMPARE WITH END?                
         BNH   REQF140             EQUAL OR LOW IS OK                           
         XC    WORK,WORK                                                        
         MVC   WORK(1),FISCAL                                                   
         MVC   WORK+1(2),=X'0101'  SET JAN/01 AND GET PREVIOUS YEAR             
         GOTO1 DATCON,DMCB,(X'31',WORK),(1,WORK),(5,0)                          
         MVC   FISCAL(1),WORK      YY PACKED                                    
*                                                                               
REQF140  DS    0H                                                               
         CLI   OPTOVHY,C'Y'        OVERHEAD ON YEAR TO DATE                     
         BNE   *+10                                                             
         MVC   STEND(2),FISCAL     STEND IS FOR ACSLRY                          
         L     R3,ASALAREA                                                      
         USING SALARYD,R3                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(8),SALBLOCK+8   ADDR AND LENGTH OF ACQUIRED BUFFER          
         XC    SALBLOCK(SALLNQ),SALBLOCK                                        
         MVC   SALBLOCK+8(8),WORK   RESTORE ADDR/LEN OF BUFFER                  
         MVC   SALCMPY,RCCOMPFL                                                 
         MVC   SALMETHD,METHOD                                                  
         MVC   SALSTART,START                                                   
         MVC   SALEND,END                                                       
         MVC   SALACOVL,COVAIL                                                  
         CLI   OPTHRS,C'Y'                STANDARD HR REQUEST?                  
         BNE   REQF150                                                          
         BAS   RE,CALVAL                  VALIDAT CAL AND SET STD DATE          
         MVC   SALSTART,STANDST                                                 
         OI    SALSTAT1,SALSTAND          SET BLOCK TO INDICATE                 
         NI    SALSTAT1,ALL-SALDISTR      TURN OFF REDISTRIB FOR NOW??          
         BAS   RE,STDVAL                  VALIDATE THAT A RECORD EXISTS         
         BE    REQF150                    END THE REQUEST                       
         MVI   FCRDACC,C'N'                                                     
         MVI   FCRDHIST,C'N'                                                    
         B     REQFX                                                            
         DROP  R3                                                               
*                                                                               
REQF150  L     R3,AMTHRTAB                MONTH TABLE                           
         LA    R4,MTHMAX                  MAX # OF MONTHS IN REQ                
         USING MTHD,R3                                                          
         XC    MTHD(MTHRLEN),MTHD         CLEAR REQUEST MONTHS TABLE            
         MVC   WORK(L'SALSTART),START     1ST ENTRY WILL BE START DATE          
         B     REQF170                                                          
*                                                                               
REQF160  GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+4)                                  
         L     R2,=F'34'                                                        
         GOTO1 ADDAY,DMCB,WORK+4,WORK+10,(R2)                                   
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,WORK)                                 
*                                                                               
REQF170  MVI   WORK+2,X'01'        SET DAY TO 01                                
         MVC   MTHCODE,WORK                                                     
         ZAP   MTHHRS,ZEROS                                                     
         CLC   MTHCODE,END         REACH THE END DATE AND YOUR DONE             
         BE    REQF180                                                          
         LA    R3,MTHLN(R3)                                                     
         BCT   R4,REQF160                                                       
         DC    H'0'                CANT BE MORE THAN MAX ENTRIES                
*                                                                               
REQF180  DS    0H                                                               
         L     RE,AUPDBLOK                                                      
         LA    RF,TRNBLKL          CLEAR ADDTRN BLOCK TO BINARY ZEROS           
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         L     RF,AUPDBLOK                                                      
         USING TRNBLKD,RF                                                       
         L     R2,ADCMPEL                                                       
         USING CPYELD,R2                                                        
         MVC   TRNCOMF,ADCOMFAC                                                 
         MVC   TRNCPYS1,CPYSTAT1   COMPANY STATUS BYTES                         
         MVC   TRNCPYS2,CPYSTAT2                                                
         MVC   TRNCPYS3,CPYSTAT3                                                
         MVC   TRNCPYS4,CPYSTAT4                                                
         MVC   TRNCPYS5,CPYSTAT5                                                
         MVC   TRNCPYS6,CPYSTAT6                                                
         MVC   TRNCPYS7,CPYSTAT7                                                
         MVC   TRNCPYS8,CPYSTAT8                                                
         CLI   CPYLN,CPYLN3Q       LONG ENOUGH FOR THESE FIELDS?                
         BL    *+16                NO                                           
         MVC   TRNCPYS9,CPYSTAT9                                                
         MVC   TRNCPYSA,CPYSTATA                                                
*                                                                               
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
*                                                                               
         MVC   TRNBMOS,MOS         MOS FOR POSTINGS                             
         MVC   TRNHOOK,RECOVER     A(RECOVERY ROUTINE) PASSED TO ADDTRN         
         OI    TRNMODE,TRNMOFLN+TRNMEMUY     OFFLINE AND NEW FILE               
         OI    TRNINDS2,TRNIUBKO+TRNIRBKA+TRNIHKAL                              
*                                  UPDATE BUCKETS ONLY                          
*                                  REPLACE (DON'T ADD TO ) BUCKET AMNTS         
*                                  HOOK ALL IO CALLS BACK FOR RECOVER           
         OI    TRNINDS,TRNICONV    PASSING ADDTRN NEW FILE FORMAT               
         CLI   RCWRITE,C'N'              *** WRITE=NO ***                       
         BNE   *+8                                                              
         OI    TRNINDS,TRNIWRNO    SET ADDTRN INDICATOR TO REFLECT              
         DROP  RF                                                               
*                                                                               
* BUILDING TABLE OF YTD P AND L RECORDS                                         
*                                                                               
         ZAP   PDUMP,ZEROS                                                      
*                                                                               
         USING PLDRECD,R6                                                       
         L     R6,DIO                                                           
         XC    PLDKEY,PLDKEY       CLEAR KEY                                    
         MVI   PLDKTYP,PLDKTYPQ    X'18' DIRECT TIME POINTER REC                
         MVI   PLDKSUB,PLDKSUBQ    X'CD'                                        
         MVC   PLDKCPY,RCCOMPFL    COMPANY                                      
         MVC   PLDKMTHD,METHOD     READ FOR METHOD                              
         MVC   SAVEKEY,PLDKEY                                                   
         NI    DMINBTS,ALL-PASSDEL                                              
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,DIO,DIO                        
         B     REQF200                                                          
*                                                                               
REQF190  GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),DIR,DIO,DIO                        
REQF200  CLC   PLDKEY(PLDKSPR1-PLDKEY),SAVEKEY                                  
         BNE   REQF210                                                          
*                                                                               
         CLC   PLDKYYMM,START      START DATE                                   
         BL    REQF190                                                          
         CLC   PLDKYYMM,LAST       END DATE                                     
         BNH   *+14                                                             
         CLC   PLDKYYMM,END        SAME START AND END (MOA ONLY)?               
         BNE   REQF190                                                          
*                                                                               
         CLC   QACCOUNT,SPACES     ANY ACCOUNT SPACIFIED?                       
         BE    *+14                                                             
         CLC   PLDKRACT,QACCOUNT   SAME ACCOUNT                                 
         BNE   REQF190                                                          
*                                                                               
         USING YTDPLD,R2                                                        
         L     R2,AYTDPWRK                                                      
         XC    0(YTDPLNQ,R2),0(R2) CLEAR WORK AREA                              
         MVC   YTDPCPY,PLDKCPY     COMPANY                                      
         MVC   YTDP1C,PLDKCACT     1C ACCOUNT                                   
         MVC   YTDP1R,PLDKRACT     1R ACCOUNT                                   
         MVC   YTDPMTHD,PLDKMTHD   METHOD                                       
         MVC   YTDPANAL,PLDKANAL   ANALYSIS CODE                                
         MVC   YTDPTYP,PLDKPTYP    PAYROLL TYPE                                 
         ZAP   YTDPAMNT,PLDKAMT    AMOUNT                                       
*                                                                               
         MVC   MSG,=CL10'YTD - FILE'                                            
         GOTO1 ADUMP,DMCB,(RC),YTDPREC,YTDPLNQ                                  
*                                                                               
*        LA    RF,*+10             HIGH CORE TABLE                              
*        O     RF,=X'80000000'                                                  
*        BSM   0,RF                31 BIT MODE                                  
         GOTO1 BIN31,DMCB,(R2),YTDPLTB  ADD TO TABLE                            
*        LA    RF,*+6                                                           
*        BSM   0,RF                24 BIT MODE                                  
         B     REQF190                                                          
*                                                                               
REQF210  TM    UPSI,POSTRATE       POST RATE RECORDS ONLY?                      
         BO    REQFX               YES - DON'T BOTHER BUILDING TBL              
         GOTO1 TABBLD              BUILD YTD-1 TABLE                            
*                                                                               
REQFX    B     EXIT                                                             
         DROP  R2,R3,R4,R5,R7                                                   
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEDGER                                                   *          
**********************************************************************          
         SPACE 1                                                                
LDGF     DS    0H                                                               
         L     RF,ADLDGHIR                                                      
         USING ACLELD,RF                                                        
         MVC   LLEVA(LLEVELLN),ACLVALS     LEVEL LENGTHS/NAMES                  
         LA    R3,LENLEVLS                 INDIVIDUAL LENGTHS OF LEVELS         
         SR    R4,R4                                                            
         LA    R1,LLEVA                    COMBINED LEVEL LENGTHS               
         LA    R0,LLEVLNUM                 MAXIMUM NUMBER OF LEVELS             
LDG02    DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,0(R1)                    PREVIOUS COMBINED LENGTH             
         SR    R5,R4                       MINUS NEW COMBINED LENGTH            
         BP    *+6                         EQUALS INDIVIDUAL LEVEL LEN          
         DC    H'0'                                                             
         STC   R5,0(R3)                    SAVE INDIVD LENGTH OF LEVEL          
         CLI   0(R1),MAXLEN                LAST LEV HAS MAXLEN FOR ACCT         
         BE    LDG04                                                            
         SR    R4,R4                                                            
         IC    R4,0(R1)                    COMBINED LENGTH IN R4                
         LA    R1,LLEVALN(R1)              BUMP TO NEXT COMBINED LENGTH         
         LA    R3,L'LENLEVA(R3)            NEXT INDIVDUAL LEN SAVE AREA         
         BCT   R0,LDG02                                                         
         DC    H'0'                                                             
                                                                                
LDG04    L     R3,ASALAREA                                                      
         USING SALARYD,R3                                                       
         MVC   SALLEVS,LENLEVLS            SET 1R LEVELS FOR SALHST             
*                                                                               
         MVC   CSTGRPLN,LLEVB                                                   
         CLI   OPTHYB,C'2'                 WHAT LEVEL(S) OF 1R TO APEND         
         BE    *+10                        DEFAULT IS LEVEL B                   
         MVC   CSTGRPLN,LLEVC                                                   
*                                                                               
         USING SUMD,R6                                                          
         L     R6,AAGYTOT                                                       
         ST    R6,AACCUM                                                        
         BAS   RE,CLSUM                    CLEAR AGENCY TOTAL BUCKETS           
         MVC   SOFFDEP,FOXES               SET AGENCY KEY                       
*                                                                               
         L     R6,AAGY99                                                        
         ST    R6,AACCUM                                                        
         BAS   RE,CLSUM                    CLEAR AGENCY 99 TOTAL                
         SR    R1,R1                                                            
         IC    R1,LENLEVA                  LENGTH OF LEVEL A                    
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SOFFDEP(0),NINES            NINES INTO LEVEL A POSITIONS         
*                                                                               
         LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB                  LENGTH OF LEVEL B                    
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FOXES               FOXES INTO LEVEL B POSITIONS         
*                                                                               
         L     R6,AOFF99                                                        
         ST    R6,AACCUM                                                        
         BAS   RE,CLSUM                    CLEAR OFFICE 9/DEPT 99               
         MVC   SOFFDEP,NINES                                                    
         B     EXIT                                                             
         DROP  R3,R6,RF                                                         
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEVEL A                                                  *          
**********************************************************************          
         SPACE 1                                                                
LEVAF    DS    0H                                                               
         LH    R2,=Y(COKOFC-COBLOCKD)  OFFICE LEVEL PROFILE LOOKUP              
         GOTO1 PROFIT,DMCB,ADHEIRA,0,LENLEVA,(R2)                               
*                                                                               
         USING SUMD,R6                                                          
         L     R6,AOFFTOT                                                       
         ST    R6,AACCUM                                                        
         BAS   RE,CLSUM            CLEAR OFFICE TOTAL BUCKETS                   
         MVC   OFFIC,SPACES                                                     
         MVC   OFFICN,SPACES                                                    
         USING NAMELD,R2                                                        
         L     R2,ADLVANAM         ADDR OF NAME EL                              
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFICN(0),NAMEREC                                                
         L     R2,ADHEIRA                                                       
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF OFFICE                             
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFIC(0),3(R2)      SAVE OFFICE CODE FOR SALHST                  
         MVC   SOFFDEP,3(R2)       OFFICE                                       
         LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FOXES      FILL DEPT POSITIONS WITH FOXES                
         L     R6,ADEPT99                                                       
         ST    R6,AACCUM                                                        
         BAS   RE,CLSUM            CLEAR DEPT99 BUCKETS                         
         L     R2,ADHEIRA                                                       
         MVC   SOFFDEP,3(R2)                                                    
         LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NINES      FILL DEPT POSITIONS WITH NINES                
*                                                                               
         L     R3,AOFFCUM                                                       
         USING OFFD,R3                                                          
         LA    R1,OFBK             CLEAR OFFICE CUM TABLE                       
         LA    R0,OFBKCNT                                                       
         ZAP   0(OFBUKLN,R1),ZEROS                                              
         LA    R1,OFBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         LA    R0,NUMBUK           NUMBER OF WORK  BUCKETS                      
         LA    R1,WRKLOC           LOCATION OF FIRST BUCKET                     
         ZAP   0(WRKBKLN,R1),ZEROS                                              
         LA    R1,WRKBKLN(R1)                                                   
         BCT   R0,*-10                                                          
         B     EXIT                                                             
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEVEL B (DEPARTMENT)                                     *          
**********************************************************************          
         SPACE 1                                                                
LEVBF    DS    0H                                                               
         LH    R2,=Y(COKDPT-COBLOCKD)  INITIALIZE COBLOCK FOR DEPT              
         GOTO1 PROFIT,DMCB,ADHEIRB,LLEVA,LENLEVB,(R2)                           
*                                                                               
         USING SUMD,R6                                                          
LVB02    L     R6,ADEPTOT           INITIALIZE OFFICE /DEPT RECORD              
         ST    R6,AACCUM                                                        
         BAS   RE,CLSUM                                                         
         MVC   DEPART,SPACES                                                    
         MVC   DEPARTN,SPACES                                                   
         USING NAMELD,R2                                                        
         L     R2,ADLVBNAM          ADDR OF NAME EL                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEPARTN(0),NAMEREC                                               
*                                                                               
         L     R2,ADHEIRB                                                       
         LA    R2,3(R2)                                                         
         MVC   SOFFDEP,0(R2)       OFFICE / DEPT                                
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0               R2 NOW POINTS TO DEPT                        
         SR    R1,R1                                                            
         IC    R1,LENLEVB                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEPART(0),0(R2)     SAVE DEPT CODE FOR SALHST                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R3,ADEPCUM                                                       
         USING DEPD,R3                                                          
         LA    R1,DEBK             CLEAR DEPT CUM TABLE                         
         LA    R0,DEBKCNT                                                       
         ZAP   0(DEBUKLN,R1),ZEROS                                              
         LA    R1,DEBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         LA    R0,NUMBUK           NUMBER OF WORK  BUCKETS                      
         LA    R1,WRKLOC           LOCATION OF FIRST BUCKET                     
         ZAP   0(WRKBKLN,R1),ZEROS                                              
         LA    R1,WRKBKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         XC    SCHMOPT,SCHMOPT                                                  
         MVC   SCHEMKEY,SPACES                                                  
         MVC   SCHEMKEY(L'SOFFDEP),SOFFDEP                                      
         OI    SCHMOPT,LOOKUP      LOOKUP ONLY OPTION                           
         XC    POOLTYPE,POOLTYPE                                                
         GOTO1 ASCHEME             GET ADDRESS OF VALID SCHEME                  
*                                  ADDR RETURNED IN ASCHMREC                    
         B     EXIT                                                             
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEVEL C (SUB-DEPARTMENT)                                 *          
**********************************************************************          
         SPACE 1                                                                
LEVCF    DS    0H                                                               
         LH    R2,=Y(COKSDT-COBLOCKD)  INITIALIZE COBLOCK FOR SUBDEPT           
         GOTO1 PROFIT,DMCB,ADHEIRC,LLEVB,LENLEVC,(R2)                           
*                                                                               
LVC02    MVC   SDEPART,SPACES                                                   
         L     R2,ADHEIRC                                                       
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVB                                                         
         AR    R2,R0               R2 NOW POINTS TO SUBDEPT                     
         SR    R1,R1                                                            
         IC    R1,LENLEVC                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SDEPART(0),0(R2)    SAVE SUBDEPT CODE FOR SALHST                 
*                                                                               
         USING OVERD,R3                                                         
         L     R3,AOVHWRK2                                                      
         BAS   RE,CLEROVH          CLEAR OVH WORK AREA FOR IND PAY TYPE         
*                                  SETUP KEY OF IND PAY TYPE POOL               
*                                  EXAMPLE: 1R0185000999IND                     
         L     R2,ADHEIRC                            ~~~~~~                     
         LA    R2,3(R2)                                                         
         SR    R1,R1                                                            
         IC    R1,LLEVC                                                         
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OVACCT(0),0(R2)                                                  
         LA    R2,OVACCT                                                        
         SR    R0,R0                                                            
         IC    R0,LLEVC                                                         
         AR    R2,R0               R2 NOW POINTS AT LEVEL 4                     
         MVC   0(3,R2),NINES       3 NINES INDICATE OVH                         
         MVC   3(2,R2),=C'IN'      IND MAKES IT IND PAY TYPE OVERHEAD           
         MVC   OVFRMACT,OVACCT                                                  
         MVI   OVHORIG,DEPT                                                     
         MVI   OVHTYPE,OVHINDP     SET TYPE TO IND PAY TYPE OVERHEAD            
         L     R1,ASCHMREC                   ADDR OF SCHEME                     
         AH    R1,=Y(SCHINDT-SCHMD)          DISP TO ENTRY                      
         MVC   OVHLEVL(L'SCHINDT),0(R1)      REPLACE LEVEL TO DO ALLOC          
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS AN ACCOUNT                                                 *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         MVI   POSTSW,SPACE                                                     
         MVI   FCRDHIST,C'N'                                                    
         XC    ACCSTUS,ACCSTUS     CLEAR THIS ACCOUNTS STATUS BYTE              
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         CLI   QOPT4,C'S'          SUPPRESS LOCKED ACCOUNTS                     
         BNE   ACF02                                                            
         TM    RSTSTAT,RSTSACIL                                                 
         BO    EXIT                                                             
*                                                                               
ACF02    DS    0H                                                               
         LH    R2,=Y(COKPER-COBLOCKD)   INITIALIZE COBLOCK FOR PERSON           
         GOTO1 PROFIT,DMCB,ADACC,LLEVC,LENLEVD,(R2)                             
*                                                                               
         L     R3,APERWRK                                                       
         USING PTOTD,R3                                                         
         XC    PTOTD(PTOTLEN),PTOTD                                             
         LA    R1,PBK              CLEAR THE PERSON / OVERHEAD RECORD           
         LA    R0,PBKCNT                                                        
         ZAP   0(PBUKLN,R1),ZEROS                                               
         LA    R1,PBUKLN(R1)                                                    
         BCT   R0,*-10                                                          
*                                                                               
* CHECK IF OVERHEAD OR IND PAY ACCOUNT                                          
*                                                                               
         USING ACTRECD,RF                                                       
         L     RF,ADACC                                                         
         MVI   LEVEL,COMP                                                       
         SR    R1,R1                                                            
         IC    R1,LLEVA            LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACTKACT(0),NINES    CORPORATE LEVEL                              
         BE    ACF03                                                            
         MVI   LEVEL,OFFICE        OFFICE LEVEL                                 
         LA    R2,ACTKACT                                                       
         SR    R0,R0                                                            
         IC    R0,LLEVA                                                         
         AR    R2,R0               R2 POINTS AT DEPT                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF LEVEL B (DEPT)                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),NINES                                                    
         BE    ACF03                                                            
         MVI   LEVEL,DEPT          DEPARTMENT LEVEL                             
         LA    R2,ACTKACT                                                       
         SR    R0,R0                                                            
         IC    R0,LLEVC            LENGTH OF LEVEL A+B+C                        
         AR    R2,R0                                                            
         CLC   0(3,R2),NINES       AT LEAST 3 NINES AT LOWER LEVEL              
         BNE   ACF03A              NO - IT'S AN EMPLOYEE                        
         CLC   =C'IN',3(R2)        IS IT A IND PAY TYPE ACCOUNT                 
         BNE   ACF03               NO - IT'S AN OVERHEAD ACCOUNT                
         OI    ACCSTUS,INDTACC     YES -INDICATE THAT IT'S IND PAY ACCT         
         B     ACF03A                                                           
ACF03    OI    ACCSTUS,OHDACC      MARK AS OVERHEAD                             
         L     R3,AOVHWRK          CLEAR OVERHEAD AREAS                         
         BAS   RE,CLEROVH                                                       
         DROP  RF                                                               
ACF03A   BAS   RE,SPECIAL          CHECK FOR ANY SPECIAL CONSIDERATION          
         BNE   EXIT                NOT EQUAL CONDITION MEANS SUPPRESS           
*                                                                               
         L     R2,ADHEIRB          OFFICE/DEPT                                  
         L     R3,ADLVBNAM                                                      
         CLI   OPTHYB,C'2'         WHAT LEVEL(S) OF 1R TO APEND                 
         BE    *+12                                                             
         L     R2,ADHEIRC          OFFICE/DEPT/SUBDPT                           
         L     R3,ADLVCNAM                                                      
         BAS   RE,HYBRD            BUILD HYBRID CONTRA TABLE                    
*                                                                               
         L     RF,ASALAREA                                                      
         USING SALARYD,RF                                                       
         LA    R0,SALACTM#         NUMBER OF MONTHLY HRLY ACCUMS                
         LA    R1,SALACTM1                                                      
         ZAP   0(L'SALACTM1,R1),ZEROS                                           
         LA    R1,L'SALACTM1(R1)                                                
         BCT   R0,*-10                                                          
         USING MTHD,RF                                                          
         L     RF,AMTHRTAB         CLEAR MONTHLY TABLE                          
         LA    R0,MTHMAX                                                        
         ZAP   MTHHRS,ZEROS                                                     
         LA    RF,MTHLN(RF)                                                     
         BCT   R0,*-10                                                          
         DROP  RF                                                               
*                                                                               
ACF04    MVI   FCRDHIST,C'Y'                                                    
         CLI   PROGPROF,C'Y'       PAGE BREAK BY PERSON?                        
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'       THEN FORCE NEW PAGE FOR EA ACCOUNT           
         L     R5,CLIBUF2          CLEAR CLIENT TABLE                           
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR NUMBER IN CLIENT RECORD                
         L     RF,ADACC                                                         
         USING ACTRECD,RF                                                       
         MVC   ACCOUNT,ACTKACT     SAVE CURRENT ACCOUNT                         
*                                                                               
         TM    ACCSTUS,OHDACC+INDTACC  IS THIS AN OVERHEAD ACCOUNT?             
         BNZ   ACF10               YES - SKIP SAVING THE PERSON                 
*                                  NO  - IT MUST BE A STAFF ACCOUNT             
ACF06    MVC   PERSON,SPACES                                                    
         MVC   PERSONN,SPACES                                                   
         USING NAMELD,R2                                                        
         L     R2,ADACCNAM          ADDR OF NAME EL                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERSONN(0),NAMEREC                                               
         L     R2,ADACC                                                         
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVC                                                         
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVD                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERSON(0),0(R2)     SAVE PERSON CODE FOR SALHST                  
*                                                                               
ACF10    L     R2,ADACCSTA         FIND GROUP FOR DIRECT ACCOUNTS               
         USING RSTELD,R2                                                        
         CLI   RSTCOSTG,SPACE                                                   
         BH    ACF12                                                            
         L     R2,ADLVCSTA         SUB - DEPT                                   
         CLI   RSTCOSTG,SPACE                                                   
         BH    ACF12                                                            
         L     R2,ADLVBSTA         DEPT                                         
         CLI   RSTCOSTG,SPACE                                                   
         BNH   *+14                                                             
ACF12    MVC   GROUP,RSTCOSTG                                                   
         B     ACF14                                                            
         CLI   OPTDGRP,SPACE                                                    
         MVC   GROUP,OPTDGRP       THE DEFAULT DIR GROUP FROM PROFILE           
         BH    *+6                                                              
         DC    H'0'                                                             
ACF14    MVC   DIRGROUP,GROUP                                                   
         B     EXIT                                                             
         DROP  R2,R3,R5,RF                                                      
         EJECT                                                                  
**********************************************************************          
* PROCESS A SUB-ACCOUNT                                              *          
**********************************************************************          
         SPACE 1                                                                
PSUB     DS    0H                                                               
         GOTO1 CLERWRK                                                          
         L     R7,ACLIWRK                INITIALIZE CLIENT RECORD               
         USING CLID,R7                                                          
         L     R5,ADSUBAC                                                       
         SH    R5,DATADISP               ADDR OF CONTRA RECORD                  
         USING CACRECD,R5                                                       
         CLC   =C'1J',CACKCUNT           TEST FOR C'1J' CONTRA                  
         BNE   *+12                      NO GO FURTHER                          
         NI    ACCSTUS,ALL-WANTIT        SET TO "DON'T WANT THIS ACCT"          
         B     EXIT                                                             
         MVC   CLILEDAC,CACKCLDG         LEDGER AND ACCOUNT                     
         L     R2,ADSUBAC                                                       
         USING CACELD,R2                 X'43'                                  
         CLI   CACEL,CACELQ                                                     
         BNE   CAF02                                                            
         SR    R1,R1                                                            
         IC    R1,CACLN                                                         
         SH    R1,=Y(CACLN1Q+1)                                                 
         BM    CAF02                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLICLNME(0),CACNAME                                              
*                                                                               
CAF02    OI    ACCSTUS,WANTIT            SET TO "WE WANT THIS ACCOUNT"          
         CLC   NONCLI,CACKCUNT           IS CONTRA U/L 1N?                      
         BE    CAF02A                                                           
         ST    R7,ACLREC                                                        
         BAS   RE,NBTEST                 CHECK IF NEW BUSINESS CLIENT           
         TM    CLISTAT,CLINB+CLIPB+CLIHS NEWBIZ,PRO BONO OR HOUSE               
         BZ    CAF02B                                                           
CAF02A   BAS   RE,ALOOKUP                LOOKUP ANALYSIS= CODE                  
         CLC   NONCLI,CACKCUNT           IS CONTRA U/L 1N?                      
         BE    CAF04                                                            
         USING BCLID,R4                                                         
CAF02B   LA    R4,NMEWRK                                                        
         MVC   BCLICDE,CLICDE           ADD CLT CODE + NAME TO TBL              
         MVC   BCLINME,CLICLNME                                                 
         GOTO1 BINADD,DMCB,(R4),NMEBUFF                                         
*                                                                               
CAF04    TM    ACCSTUS,OHDACC           IS IT AN OVERHEAD ACCOUNT?              
         BZ    CAF06                    NO - CONTINUE                           
         CLI   OPTSHOWO,C'Y'            SHOW 14 DIRECT GROUP FOR OVH?           
         BNE   EXIT                                                             
         MVC   CLGRPING(L'DIRGROUP),DIRGROUP                                    
         MVC   CLGRPING+L'DIRGROUP(L'CLGRPING-L'DIRGROUP),CSTGRP                
         B     EXIT                                                             
*                                                                               
CAF06    CLC   CLILEG,CACKCUNT          IS U/L 1C                               
         BE    EXIT                     YES - YOUR THRU                         
         CLI   CLIANALS,LOA             ANALYSIS=L                              
         BE    CAF10                    SKIP LEAVE OF ABSENCE                   
         CLI   OPTNO1N,C'Y'             SKIP ALL 1N                             
         BNE   EXIT                                                             
CAF10    NI    ACCSTUS,ALL-WANTIT        SET TO "DON'T WANT THIS ACCT"          
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4,R5,R7                                                      
         EJECT                                                                  
**********************************************************************          
* PROCESS A HISTORY RECORD (BUCKETS)                                 *          
**********************************************************************          
         SPACE 1                                                                
PHIS     DS    0H                                                               
         TM    ACCSTUS,WANTIT                                                   
         BZ    EXIT                                                             
         L     R5,ADSUBAC                CONTRA RECORD                          
         SH    R5,DATADISP                                                      
         USING CACRECD,R5                                                       
         CLI   CACKBTYP,HOURS            WE WANT HOUR BUCKETS                   
         BE    PHIS02                                                           
         CLC   CACKBTYP(L'METHOD),METHOD AND DOLLARS FOR METHOD BUCKETS         
         BNE   EXIT                                                             
         USING BUKELD,R4                                                        
PHIS02   L     R4,ADTRANS                                                       
         CLI   0(R4),BUKELQ        MAKE SURE IT'S A HISTORY BUCKET              
         BNE   EXIT                                                             
*                                                                               
         USING CLID,R7                                                          
         L     R7,ACLIWRK          R7 CLIENT SUMMARY RECORD (CLID)              
         CLI   BUCKTYPE,HOURS      HOURS                                        
         BNE   PHIS04                                                           
*                                  HOUR BUCKETS                                 
         CLC   BUKMOS,START                                                     
         BL    EXIT                BUCKET MUST BE WITHIN REQUEST RANGE          
         CLC   BUKMOS,END                                                       
         BH    EXIT                                                             
         CLC   BUKMOS,END                                                       
         BNE   *+8                                                              
         OI    ACCSTUS,ACTIVE      INDICATE CURRENT MONTH ACTIVITY              
*&&US*&& ZAP   TOTIME,BUKCR        US  HOURS ARE CR SIDE BUCKETS                
*&&UK*&& ZAP   TOTIME,BUKDR        UK  HOURS ARE DR SIDE                        
*                                                                               
         USING PTOTD,R6                                                         
         L     R6,APERWRK          R6 EMPLOYEE SUMMARY RECORD (PTOTD)           
         TM    PERSTAT,PERDIRT     DIRECT TIME ONLY FOR EMPL?                   
         BZ    *+14                                                             
         CLC   CLILEG,CACKCUNT     IS U/L 1C                                    
         BNE   PHIS03              NO- SKIP UPDATE OF MTHLY HRS TABLE           
         USING MTHD,RF                                                          
         L     RF,AMTHRTAB         MONTHLY TABLE                                
         LA    R0,MTHMAX                                                        
         CLC   BUKMOS,MTHCODE      MONTH EQUAL ADD IN HRS                       
         BE    *+14                                                             
         LA    RF,MTHLN(RF)                                                     
         BCT   R0,*-14                                                          
         DC    H'0'                MUST FIND THE MONTH                          
         AP    MTHHRS,TOTIME                                                    
*                                                                               
         USING PTOTD,R6                                                         
PHIS03   L     R6,APERWRK          R6 EMPLOYEE SUMMARY RECORD (PTOTD)           
         AP    CYTDHRS,TOTIME      YTD HOURS INTO CLIENT SUMMARY                
         AP    PYTDHRS,TOTIME      YTD HOURS INTO EMPLOYEE SUMMARY              
         CLC   BUKMOS,END                                                       
         BNE   EXIT                                                             
         AP    CPERHRS,TOTIME      THIS MONTHS HOURS                            
         AP    PPERHRS,TOTIME                                                   
         LA    R1,PPERHRP          POSITIVE PERIOD HOURS                        
         CP    TOTIME,ZEROS                                                     
         BH    *+8                                                              
         LA    R1,PPERHRN          NEGATIVE PERIOD HOURS                        
         AP    0(PBUKLN,R1),TOTIME                                              
         B     EXIT                                                             
*                                  DOLLAR BUCKETS                               
*                                                                               
PHIS04   CLC   BUKMOS,END          PUT OUT DUMMY SORT BUCKET                    
         BNE   PHIS04A             FOR ALL 1R-1C/1N COMBOS                      
         CP    BUKDR,ZEROS                                                      
         BE    *+8                                                              
         BAS   RE,DUMSRT           WITH A $ BUCKET IN CURRENT MTH               
PHIS04A  CLC   BUKMOS,LAST         NOTHING ELSE HIGHER THAN YTD-1               
         BH    EXIT                                                             
         LA    R1,START            USE REQUEST START DTE IF YTD                 
         CLI   OPTYTD,C'Y'         ALLOCATION AS A WHOLE  YTD?                  
         BE    PHIS06                                                           
         CLI   OPTOVHY,C'Y'        OVERHEAD ON YTD BASIS ONLY                   
         BNE   EXIT                                                             
         LA    R1,FISCAL           THEN USE FISCAL START                        
*                                                                               
PHIS06   CLC   BUKMOS,0(R1)        NOT BEFORE START(FISCAL/REQUEST)             
         BL    EXIT                                                             
         TM    ACCSTUS,OHDACC+INDTACC  IS THIS AN OVERHEAD ACCOUNT?             
         BZ    PHIS08              NO  - IT MUST BE A STAFF ACCOUNT             
*                                                                               
*                                                                               
*                                  OVERHEAD ACCOUNTS                            
         L     R3,AOVHWRK                                                       
         TM    ACCSTUS,INDTACC     IS THIS A IND PAY TYPE ACCOUNT?              
         BZ    *+8                 NO - OK JUST USE THE NORMAL OVHWRK           
         L     R3,AOVHWRK2         YES- USE OVHWRK2 INSTEAD                     
         USING OVERD,R3                                                         
         AP    OVALST,BUKDR        OVERHEAD YTD-1 (OH WORK AREA)                
         AP    CCSTLST,BUKDR       ALSO IN CLID FOR THIS CONTRA                 
         BAS   RE,OVHIT            YTD-1 OVH POSTINGS TAB                       
         B     EXIT                                                             
*                                  STAFF ACCOUNTS                               
*                                                                               
PHIS08   CLI   OPTYTD,C'Y'         YTD ALLOCATION?                              
         BNE   PHIS10                                                           
         AP    CCSTLST,BUKDR       YTD-1 CASH IN CLID FOR THIS CONTRA           
         BAS   RE,SALTPST          YTD-1 BY SALARY TYPE                         
         B     EXIT                                                             
*                                                                               
*                                  IF ALLOC IS MTHLY BUT OH IS YTD              
PHIS10   CLI   CLILEDG,C'C'                                                     
         BNE   EXIT                DON'T POST NON-CLIENT TIME                   
         ZAP   DUB,BUKDR           POST THE YTD-1 COST FOR THIS EMPL            
         MVI   POSTSW,C'M'         AS A MEMO TO BUFFALO FOR USE IN              
         ST    R7,ACLREC                                                        
         GOTO1 ABUFPOST            YTD OH ALLOCATION                            
         B     EXIT                                                             
         DROP  R3,R4,R5,R6,R7,RF                                                
         EJECT                                                                  
**********************************************************************          
* END OF SUB ACCOUNT (CONTRA)                                        *          
**********************************************************************          
         SPACE 1                                                                
SUBL     DS    0H                                                               
         TM    ACCSTUS,WANTIT      DO WE WANT THIS ACCOUNT?                     
         BZ    EXIT                                                             
         TM    ACCSTUS,OHDACC      IS THIS AN OVERHEAD ACCOUNT?                 
         BO    EXIT                                                             
         L     R6,APERWRK          PERSON TOTALS                                
         USING PTOTD,R6                                                         
         L     R7,ACLIWRK          CLIENT TOTALS                                
         USING CLID,R7                                                          
*                                                                               
         CLI   CLILEDG,C'N'        NON-CLT TIME?                                
         BE    SBL02               YES - CHECK IF IT'S PERSONAL TIME            
         TM    CLISTAT,CLINB+CLIPB+CLIHS  NEWBIZ, PROBONO OR HOUSE?             
         BZ    *+12                                                             
         CLI   CLIANALS,PERANAL                                                 
         BE    SBL02               YES, THEN ADD YTD PERSONAL HRS               
         AP    PDIRLST,CCSTLST     NO - IT'S YTD-1 DIRECT COST                  
         B     SBL10               GO ADD TO CLIBUF2                            
SBL02    CLI   CLIANALS,PERANAL    PERSONAL TIME/                               
         BE    *+14                                                             
         AP    PINDLST,CCSTLST     YTD-1 INDIRECT COST                          
         B     SBL10                                                            
         AP    PRSYTDH,CYTDHRS     YTD PERSONAL HOURS                           
*                                                                               
SBL10    LA    R2,CLIBK                                                         
         LA    R0,CBKCNT2                                                       
         CP    0(CBUKLEN,R2),ZEROS                                              
         BNE   SBL14                                                            
         LA    R2,CBUKLEN(R2)                                                   
         BCT   R0,*-14                                                          
         B     EXIT                DON'T ADD IF ALL BUCKETS ZERO                
*                                                                               
SBL14    DS    0H                                                               
         GOTO1 BINADD,DMCB,(R7),CLIBUF2      ADD CLIENT TAB (CLIBUF2)           
         B     EXIT                                                             
         DROP  R6,R7                                                            
         EJECT                                                                  
**********************************************************************          
* END OF AN ACCOUNT (PERSON)                                         *          
**********************************************************************          
         SPACE 1                                                                
ACCL     DS    0H                                                               
         CLI   FCRDHIST,C'N'       NO ACTIVITY FOR THIS EMPLOYEE                
         BE    EXIT                                                             
         XC    WARNNUM,WARNNUM     CLEAR WARNING NUMBER                         
         TM    ACCSTUS,OHDACC+INDTACC THIS AN OVERHEAD/IND PAY ACCOUNT?         
         BZ    ACL02               NO  - IT'S AN EMPLOYEE CONTINUE              
         GOTO1 DISOVH              YES - GO DISTRIBUTE OVERHEAD                 
         B     EXIT                                                             
*                                                                               
*                                  ACCLAST FOR EMPLOYEE                         
         USING PTOTD,R6                                                         
ACL02    L     R6,APERWRK                                                       
         ZAP   PNONPER,PYTDHRS     TOTAL YTD HOURS                              
         SP    PNONPER,PRSYTDH     LESS PERSONAL  GIVES NON-PERS                
         BAS   RE,RATES            SAL LOOKUP/FIGURE HOURLY RATES               
         CP    PCOST,ZEROS         DID YOU COME BACK WITH A SALARY?             
         BNE   ACL08               YES - CONTINUE                               
*                                  NO  - NO SALARY PRESENT,WHAT TO DO           
*                                                                               
         CLI   OPTYTD,C'Y'         AGENCY IS YTD?                               
         BNE   ACL06               NO  - IT MUST BE PERIOD                      
         ZAP   DUB,PINDLST                                                      
         AP    DUB,PDIRLST         IS DIRECT + IND COST YTD-1 = 0?              
         CP    DUB,ZEROS                                                        
         BNZ   ACLXX               NO  - YTD-1 MUST BE REVERSED                 
*                                                                               
ACL06    CP    PYTDHRS,ZEROS       ANY HOURS AT ALL?                            
         BNE   ACL07               NO  - GET OUT                                
         TM    ACCSTUS,ACTIVE                                                   
         BZ    EXIT                                                             
         USING BIND,RF                                                          
         L     RF,CLIBUF2                                                       
         L     R1,BININ                                                         
         LTR   R1,R1                                                            
         BZ    EXIT                NO CLIENT IN EMPLOYEES TABLE                 
         DROP  RF                                                               
         MVI   WARNNUM,ERRHRNET    SET TO HOURS NET TO ZERO                     
         OI    PERSTAT,PERENOSL    SET EMPL STATUS TO REFLECT NO SALARY         
         B     ACLXX               YES - GO ALLOC COSTS (HRLY RATE 0)           
ACL07    MVI   WARNNUM,ERRNOSAL    SET TO HOURS BUT NO SALARY                   
         OI    PERSTAT,PERENOSL    SET EMPL STATUS TO REFLECT NO SALARY         
         B     ACLXX               YES - GO ALLOC COSTS (HRLY RATE 0)           
*                                                                               
*                                  GETTING HERE MEANS YOU HAVE A SALARY         
*                                                                               
ACL08    CP    PYTDHRS,ZEROS       ANY HOURS AT ALL?                            
         BE    ACL10               NO - PUT ADJ 1N ENTRY TO CLIBUF2             
         CP    PYTDHRS,PRSYTDH     ARE ALL HOURS PERSONAL TIME?                 
         BNE   ACLXX               NO - GO ALLOC COSTS                          
ACL10    BAS   RE,NOHRS            PUT ADJUSTING 1N ENTRY TO CLIBUF2            
*                                                                               
ACLXX    DS    0H                  GO ALLOCATE THIS EMPLOYEE'S COST             
         EJECT                                                                  
*---------------------------------------------------------                      
*              ALLOCATE COST TO EACH CLIENT/NON-CLIENT                          
*---------------------------------------------------------                      
*                                                                               
ALOC00   DS    0H                                                               
         USING BIND,RF                                                          
         MVI   POSTSW,SPACE                                                     
         L     RF,CLIBUF2                                                       
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    ALOCXX               NO CLIENT IN EMPLOYEES TABLE                
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    R7,BINTABLE                                                      
         MVC   TOTLINE(80),=10PL8'0'                                            
*                                                                               
         ZAP   PDUMP,ZEROS                                                      
*                                                                               
         USING CLID,R7                                                          
ALOC04   ST    R7,ACLREC            SAVE ADDR OF CURRENT RECORD                 
         CLI   CLIANALS,PERANAL     PERSONAL TIME?                              
         BNE   ALOC06               NO CONTINUE                                 
         CP    CCSTLST,ZEROS        MAY NOT HAVE BEEN PERSONAL BEFORE           
         BNE   ALOC08               ADJUST OUT ANY POSTINGS                     
         B     ALOC10               IF NOT ADJUSTMENT THEN SKIP                 
*                                                                               
ALOC06   ZAP   CADJHRS,CYTDHRS      ADJUST HOURS FOR PERSONAL TIME              
         TM    PERSTAT,PERDIRT      DIRECT TIME ONLY FOR EMPL?                  
         BNO   *+12                 NO  - CONTINUE                              
         CLI   CLILEDG,C'N'         YES - POST ONLY DIRECT PORTION              
         BE    ALOC10                                                           
         BAS   RE,YTDPST            FIGURE YTD FOR CLIENT BY SAL TYPE           
ALOC08   ZAP   CCSTPST,CCSTYTD                                                  
         SP    CCSTPST,CCSTLST      THIS IS TOTAL POSTING TO THIS CLT           
         GOTO1 AEMPLOY              MAKE EMPLOYEE POSTINGS TO SORT              
         CLI   CLILEDG,C'C'                                                     
         BNE   ALOC09                                                           
         GOTO1 ABUFPOST            POST DIRECT TIME BUFFALO RECORDS             
ALOC09   GOTO1 SUMPOST             POST TO SUMMARY RECORDS                      
ALOC10   GOTO1 REPORT,DMCB,(0,ACLREC),0           PERSONS CLIENT LINE           
         LA    R7,CLEN2(R7)                       NEXT CLIENT                   
         BCT   R3,ALOC04                                                        
*                                                                               
         GOTO1 REPORT,DMCB,(0,ACLREC),(C'T',0)    TOTAL LINE                    
ALOCXX   B     EXIT                                                             
         DROP  R6,R7,RF                                                         
         EJECT                                                                  
**********************************************************************          
* END OF LEVEL B (DEPARTMENT)                                        *          
**********************************************************************          
         SPACE 1                                                                
LEVBL    DS    0H                                                               
         MVI   LEVEL,DEPT                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 INDPOST                    POST THE INDIRECT                     
*                                                                               
         USING OVERD,R4                                                         
         L     R4,AOVHWRK2                WORKAREA FOR IND PAY OVERHEAD         
         TM    OVHSTAT2,OVHTOTBL          ENTRY PUT TO TABLE?                   
         BO    LVBL02                     YES - NO PROBLEM                      
         LA    R1,OVBK                                                          
         LA    R0,OVBUKCNT                                                      
         CP    0(OVBKLN,R1),ZEROS                                               
         BNZ   LVBL01                                                           
         LA    R1,OVBKLN(R1)                                                    
         BCT   R0,*-14                                                          
         B     LVBL02                     DON'T WORRY ZERO POOL ENTRIES         
*                                                                               
LVBL01   CLI   QOPT1,C'L'                                                       
         BNE   *+8                                                              
         OI    RUNSTAT,POSTERR            AN ERROR HERE MEANS THAT WE           
         USING ERRORD,R3                  HAVE A POOL OF IND PAY TYPES          
         MVC   WORK,SPACES                FOR THIS DEPT BUT THE                 
         LA    R3,WORK                    1ROODDDSSS999IND ACCOUNT WAS          
         MVI   ERRTYPE,ERRERROR           NOT SET UP SO WE CAN'T MAKE           
         MVI   ERRNUM,ERR1RMIS            THE 1R POSTING                        
         MVC   ERRACCT(L'EMPLEDG),EMPLEDG                                       
         MVC   ERRACCT+L'EMPLEDG(L'OVACCT),OVACCT                               
         MVC   ERRAMNT,OVAYTD                                                   
         GOTO1 BINADD,DMCB,(R3),ERRORBUF ADD TO TABLE                           
*                                                                               
         USING SUMD,R6                                                          
LVBL02   L     R6,ADEPTOT                                                       
         ZAP   SCINDPST,SCINDYTD   GET NEW POSTING AMOUNT                       
         SP    SCINDPST,SCINDLST                                                
         GOTO1 SUMUP,DMCB,ADEPTOT,AOFFTOT   ADD DEPT TOTAL TO OFFICE            
*                                                                               
         USING OFFD,R7                                                          
         L     R7,AOFFCUM                                                       
         LA    R3,OFDACCUM                                                      
         USING DEPD,R7                                                          
         L     R7,ADEPCUM                                                       
         LA    R1,DEBK                                                          
         LA    R0,DEBKCNT                                                       
LVBL04   AP    0(DEBUKLN,R3),0(DEBUKLN,R1)  ADD DEPT ACCUMS TO OFFICE           
         LA    R3,DEBUKLN(R3)                                                   
         LA    R1,DEBUKLN(R1)                                                   
         BCT   R0,LVBL04                                                        
*                                                                               
         L     R2,ADHEIRA                                                       
         LA    R2,L'CUL(R2)                                                     
         SR    R1,R1                                                            
         IC    R1,LLEVA                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),NINES       OFFICE 9 IS NOT AN OFFICE                    
         BE    LVBL06              DON'T PRINT SUMMARY AT DUMMY DEPT            
         GOTO1 REPORT,DMCB,(2,0),(C'S',0)  DEPT SUMMARY                         
LVBL06   DS    0H                    ADD 1 TO 2-3 THEN CLEAR 1                  
         GOTO1 BUFFALO,DMCB,(L'ADDC,ADDC),ADBUFC,1,2,(X'80',3)                  
         GOTO1 BUFFALO,DMCB,(L'CLEARC,CLEARC),ADBUFC,(X'80',1)                  
*                                                                               
         L     R6,ADEPTOT                                                       
         ST    R6,AACCUM                                                        
         BAS   RE,CLSUM                                                         
*                                                                               
         B     EXIT                                                             
         DROP  R3,R4,R6,R7                                                      
         EJECT                                                                  
**********************************************************************          
* END OF LEVEL A (OFFICE)                                            *          
**********************************************************************          
         SPACE 1                                                                
LEVAL    DS    0H                                                               
         MVI   LEVEL,OFFICE                                                     
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 INDPOST                    POST THE INDIRECT                     
         GOTO1 DISOVH                     POST VALID UNALLOCATED POOLS          
*                                                                               
         GOTO1 SUMUP,DMCB,ADEPT99,AOFFTOT ADD DEPT 99 TO OFFTOT                 
         GOTO1 SUMUP,DMCB,AOFFTOT,AAGYTOT ADD OFFTOT TO AGYTOT                  
         L     RE,ADEPTOT                                                       
         L     RF,AOFFTOT                                                       
         MVC   0(SLEN,RE),0(RF)                                                 
*                                                                               
         USING AGYD,R7                                                          
         L     R7,AAGYCUM                                                       
         LA    R3,AGOACCUM                                                      
         USING OFFD,R7                                                          
         L     R7,AOFFCUM                                                       
         LA    R1,OFBK                                                          
         LA    R0,OFBKCNT                                                       
LVAL04   AP    0(OFBUKLN,R3),0(OFBUKLN,R1) ADD OFFICE ACCUMS TO AGENCY          
         LA    R3,OFBUKLN(R3)                                                   
         LA    R1,OFBUKLN(R1)                                                   
         BCT   R0,LVAL04                                                        
*                                                                               
         L     R2,ADHEIRA                                                       
         LA    R2,L'CUL(R2)                                                     
         SR    R1,R1                                                            
         IC    R1,LLEVA                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),NINES       OFFICE 9 IS NOT AN OFFICE                    
         BE    LVAL08              DON'T PRINT SUMMARY AT DUMMY DEPT            
         GOTO1 REPORT,DMCB,(2,0),(C'S',0)  OFFICE SUMMARY                       
LVAL08   DS    0H                 CLEAR 1-2                                     
         GOTO1 BUFFALO,DMCB,(L'CLEARC,CLEARC),ADBUFC,1,(X'80',2)                
*                                                                               
         L     R6,AOFFTOT                                                       
         ST    R6,AACCUM                                                        
         BAS   RE,CLSUM            CLEAR OFFICE TOTAL BUCKETS                   
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
* END OF REQUEST                                                     *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         TM    RUNSTAT,ERROUT             FATAL ERROR                           
         BO    REQL08                     STRAIGHT TO ERROR REPORT              
         MVI   LEVEL,COMP                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 INDPOST                    POST THE INDIRECT                     
         GOTO1 DISOVH                     POST VALID UNALLOCATED POOLS          
         GOTO1 SUMUP,DMCB,AOFF99,AAGY99   ADD OFFICE 9/DEPT99 AGENCY            
         GOTO1 SUMUP,DMCB,AAGY99,AAGYTOT  OFFICE9 DEPT 99 TO TABLE              
         GOTO1 SUMUP,DMCB,AAGYTOT,0       AGENCY TOTAL TO TABLE                 
         L     RE,ADEPTOT                                                       
         L     RF,AAGYTOT                                                       
         MVC   0(SLEN,RE),0(RF)                                                 
         GOTO1 REPORT,DMCB,(2,0),(C'S',0)  CORP SUMMARY                         
         BAS   RE,SUMMARY                 PRINT DEPT/OFFICE SUMMARY             
*                                                                               
         GOTO1 YTDADJ              PUT YTD-1 ADJUSTMENTS TO SORT IF ANY         
         GOTO1 CONCK               MAKE SURE ALL CONTRAS EXIST                  
*                                                                               
         ZAP   DUB,PDEBITS                                                      
         SP    DUB,PCREDITS                                                     
         CP    DUB,=P'5000'                                                     
         BH    REQL06              ROUNDING MORE THAN 50 DOLLARS                
         CP    DUB,=P'-5000'       BUMPED UP FROM 30 TO 50 ON 7/20/11           
         BNL   REQL08                                                           
*                                  ROUNDING MORE THAN -50 DOLLARS               
         USING ERRORD,R3                                                        
REQL06   MVC   WORK,SPACES                                                      
         LA    R3,WORK                                                          
         MVI   ERRTYPE,ERRERROR  SET TO ERROR                                   
         MVI   ERRNUM,ERRDRCR                                                   
         MVC   ERRAMNT,DUB                                                      
         GOTO1 BINADD,DMCB,(R3),ERRORBUF ADD TO ERROR TABLE                     
         CLI   QOPT1,C'L'                                                       
         BNE   *+8                                                              
         OI    RUNSTAT,POSTERR                                                  
*                                                                               
REQL08   DS    0H                                                               
         GOTO1 ERRREPT             PRINT ERRORS IF ANY                          
         CLI   QOPT1,C'L'          "LIVE" REQUEST?                              
         BE    REQL12                                                           
         TM    UPSI,SORTREP        SORT BUCKET REPORT?(ONLY DRAFT)              
         BNO   REQL20                                                           
         GOTO1 BUKREPT                                                          
         B     REQL20                                                           
*                                                                               
REQL12   DS    0H                                                               
         L     R1,PLCSAV           CLEAR PLCRECD SAVE AREA                      
         XC    0(L'CPLCSAV,R1),0(R1)                                            
         MVC   0(L'PLCKEY,R1),SPACES                                            
         GOTO1 POSTIT              THEN GO TO POST                              
*                                                                               
REQL20   DS    0H                                                               
         TM    RUNSTAT,RUNSOON     SOON RUN?                                    
         BO    REQL22                                                           
         CLOSE (COSTRCV)           CLOSE THE RECOVERY FILE                      
         LTR   RF,RF               SUCCESSFUL CLOSE?                            
         BZ    *+6                 YES - CONTINUE                               
         DC    H'0'                NO  - DEATH                                  
*                                                                               
         OPEN  (COSTRCV,(OUTPUT))  FILE UPDATING COMPLETE PASS                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLOSE (COSTRCV)           STEP2 NULL FILE IF DUMP AFTER THIS           
         LTR   RF,RF               SUCCESSFUL CLOSE?                            
         BZ    *+6                 YES - CONTINUE                               
         DC    H'0'                NO  - DEATH                                  
*                                                                               
REQL22   DS    0H                  CLEAR 3 AND CLOSE SORT                       
         GOTO1 BUFFALO,DMCB,(L'CLEARC,CLEARC),ADBUFC,(X'80',3)                  
         GOTO1 ADSORTER,DMCB,(L'ENDC,ENDC)                                      
         MVC   SAVEMET,METHOD      SAVE METHOD CODE                             
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* END OF RUN                                                         *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'       NEW PAGE FOR SUMMARY                         
*                                                                               
         GOTO1 ACTRAVL,DMCB,(3,ADCOMFAC),LOGIO                                  
*                                                                               
         GOTO1 WRAP                RETURN ACCQUIRED CORE                        
         TM    RUNSTAT,POSTERR     ANY ERRORS IN RUN?                           
         BNO   RUNL04                                                           
*                                                                               
*                            ALLOCATION INCOMPLETE-CHECK FOR ERRORS             
         MVC   P+1(L'AC@ALICE),AC@ALICE                                         
         GOTO1 ACREPORT                                                         
*                                                                               
RUNL04   TM    RUNSTAT,RUNLIVE     ANY LIVE REQUESTS?                           
         BNO   EXIT                                                             
         GOTO1 RECREPT             RECORDS ADDED REPORT(DATA CONTROL)           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* HANDLE PEOPLE WITHOUT HOURS                                         *         
*        MAKE POSTING TO 1N FOR EACH SALARY TYPE                      *         
***********************************************************************         
         SPACE 1                                                                
         USING PTOTD,R6                                                         
NOHRS    NTR1                                                                   
         L     R6,APERWRK                                                       
         TM    PERSTAT2,PERHRRTA   MTHLY EMPL WITH (+) AND (-) HRS              
         BO    EXIT                                                             
         OI    PERSTAT,PBAKOUT     SET EMPL STATUS TO REFLECT BACKOUT           
         BAS   RE,NCONTRA          GO GET THE 1N CONTRA TO POST TO              
         USING CLID,R7                                                          
         L     R7,ACLIWRK                                                       
         USING CMPUTD,R4                                                        
         L     R4,COMPRAT          TBL OF DISP TO SAL TYP ACCUMS                
*                                                                               
NOHR02   OC    CMSLRD,CMSLRD       IF Y(0) SKIP THIS SAL TYPE                   
         BZ    NOHR06                                                           
         L     R1,ACLIWRK                                                       
         AH    R1,CMPCLIDY         DISP TO YTD ACCUM FOR SALARY TYPE            
         L     R2,APERWRK                                                       
         AH    R2,CMPTOTD          DISP TO EMPLOYEE'S SALARY BREAKDOWN          
         ZAP   0(CBUKLEN,R1),0(PBUKLN,R2) YTD SALARY AS INDIRECT COST           
         AP    CCSTYTD,0(PBUKLN,R2)                ADD TO TOTAL ACCUM           
NOHR06   LA    R4,CMPULN(R4)       NEXT SAL TYPE IN TABLE                       
         CLI   0(R4),EOT           END OF TABLE                                 
         BNE   NOHR02                                                           
*                                                                               
         GOTO1 BINADD,DMCB,(R7),CLIBUF2                                         
*                                                                               
         MVI   WARNNUM,ERRPERHR    SET TO PERSONAL HOURS ONLY                   
         CP    PYTDHRS,ZEROS                                                    
         BNE   *+12                                                             
         MVI   WARNNUM,ERRNOHRS    SET ERROR TYPE TO NO HOURS                   
         OI    PERSTAT,PERENOHR    SET EMPL STATUS TO REFLECT NO HOURS          
         B     EXIT                                                             
         DROP  R4,R6,R7                                                         
         EJECT                                                                  
***********************************************************************         
* POST EMPLOYEE YTD SALARY TYPE BUCKETS                               *         
*      TO CLIENT RECORD BASED ON THE HRLY RATE                        *         
*      OF EACH SALARY TYPE                                            *         
***********************************************************************         
         SPACE 1                                                                
YTDPST   NTR1                                                                   
         L     R7,ACLREC           ADDR OF CURRENT CLIENT RECORD                
         USING CLID,R7                                                          
         L     R6,APERWRK          ADDR OF EMPLOYEE RECORD                      
         USING PTOTD,R6                                                         
*                                                                               
         USING CMPUTD,R5                                                        
         L     R5,COMPRAT          TBL OF DISP TO SAL TYP ACCUMS                
YTDP02   OC    CMSLRD,CMSLRD       IF Y(0) SKIP THIS SAL TYPE                   
         BZ    YTDP10                                                           
         L     R2,ARATWRK          EMPLOYEES HRLY RATE RECORD                   
         AH    R2,CMRATD           DISP TO HOURLY RATE                          
         CP    0(RBUKLN,R2),ZEROS  DO WE HAVE A HRLY RATE                       
         BE    YTDP10              NO - NEXT TABLE ENTRY                        
*                                                                               
YTDP04   ZAP   DIVWRK,CADJHRS      ADJUSTED HOURS                               
         MP    DIVWRK,0(RBUKLN,R2) TIMES RATE                                   
         SRP   DIVWRK,64-4,5       SHIFT 4 FOR RATE                             
         ZAP   DUB,DIVWRK+(L'DIVWRK-RBUKLN)(RBUKLN)                             
         L     R3,ACLREC           EMPLOYEE'S CLIENT RECORD                     
         AH    R3,CMPCLIDY         DISP TO SAL TYPE YTD ACCUM                   
         TM    PERSTAT,PBAKOUT     ALL TIME BACKOUT FOR EMPLOYEE?               
         BNO   YTDP08              NO- CONTINUE                                 
         CLI   CLILEDG,C'N'                                                     
         BNE   YTDP08              ADDITIONAL CHECKS FOR 1N                     
         CP    DUB,ZEROS                                                        
         BE    *+6                                                              
         DC    H'0'                IF IT'S NOT ZERO SOMETHING IS WRONG          
         CP    0(CBUKLEN,R3),ZEROS ALREADY HAVE A YTD ADJUST FIGURE?            
         BNE   YTDP10              YES - THEN USE THAT                          
*                                                                               
YTDP08   ZAP   0(CBUKLEN,R3),DUB   ADD SAL TYPE YTD TO PROPER ACCUM             
         AP    CCSTYTD,DUB         TOTAL YTD CASH                               
YTDP10   LA    R5,CMPULN(R5)       NEXT SAL TYPE IN TABLE                       
         CLI   0(R5),EOT           END OF TABLE                                 
         BNE   YTDP02                                                           
         B     EXIT                                                             
         DROP  R5,R6,R7                                                         
         EJECT                                                                  
**********************************************************************          
* -------POST EMPLOYEE YTD-1 SALARY TYPE BUCKETS------               *          
*               TO THIS CLIENTS RECORD                               *          
**********************************************************************          
         SPACE 1                                                                
SALTPST  NTR1                                                                   
         L     RF,ADSUBAC          CONTRA RECORD                                
         SH    RF,DATADISP                                                      
         USING CACRECD,RF                                                       
         L     R4,ADTRANS          ADDR OF X'45'                                
         USING BUKELD,R4                                                        
         L     R5,COMPRAT          TBL OF DISP TO SAL TYP ACCUMS                
         USING CMPUTD,R5                                                        
SALT02   CLI   0(R5),EOT           END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                SALARY TYPE UNKNOWN                          
         CLC   CMBUKTYP,CACKBTYP+1 COMPARE SAL TYPE                             
         BE    *+12                                                             
         LA    R5,CMPULN(R5)       NEXT SAL TYPE IN TABLE                       
         B     SALT02                                                           
         L     R1,ACLIWRK          EMPLOYEE'S CLIENT RECORD                     
         AH    R1,CMPCLIDL         DISP TO SAL TYPE YTD-1 ACCUM                 
         AP    0(CBUKLEN,R1),BUKDR ADD SAL TYPE YTD-1                           
         B     EXIT                                                             
         DROP  R4,R5,RF                                                         
         EJECT                                                                  
**********************************************************************          
* ------POST OVERHEAD YTD-1 POSTINGS TO TABLE------                  *          
*       NOTE: THIS INCLUDES NORMAL "OVERHEAD" PLUS                   *          
*             IND PAY TYPE "OVERHEAD"                                *          
**********************************************************************          
         SPACE 1                                                                
OVHIT    NTR1                                                                   
         L     R7,ACLIWRK          CLIENT TOTALS                                
         USING CLID,R7                                                          
         L     R4,ADTRANS          ADDR OF X'45'                                
         USING BUKELD,R4                                                        
         L     R3,AYTDWRK                                                       
         USING YTD1D,R3                                                         
         MVC   YTD1D(YTDLEN),SPACES                                             
         MVC   YTDACC,ACCOUNT      1R ACCOUNT                                   
         MVC   YTDCNTRA,CLILEDAC   CONTRA LEDGER/ACCOUNT                        
         L     RF,ADSUBAC                                                       
         SH    RF,DATADISP                                                      
         USING CACRECD,RF                                                       
         MVC   YTDBTYP,CACKSTYP    BUCKET TYPE                                  
         ZAP   YTD1AMT,BUKDR       SAVE YTD-1 FOR 1R/1C OVH POSTING             
         CP    YTD1AMT,ZEROS       $00.00                                       
         BE    EXIT                DON'T BOTHER WITH ZERO BUCKETS               
         GOTO1 BINADD,DMCB,(R3),YTD2BUF                                         
         B     EXIT                                                             
         DROP  R3,R4,R7,RF                                                      
         EJECT                                                                  
*------------------------------------------------------                         
*        -------EMPLOYEE INFORMATION-------                                     
*        LOOKUP SALARY COMPUTE HOURLY RATES                                     
*------------------------------------------------------                         
*                                                                               
RATES    NTR1                                                                   
*                                                                               
         USING RATD,R7                                                          
         USING PTOTD,R6                                                         
         L     R6,APERWRK          EMPLOYEE TOTALS RECORD                       
         L     R7,ARATWRK          EMPL'S HOURLY RATE RECORD                    
         ZAP   WRKHRS,PNONPER      YTD HRS (WITHOUT PERSONAL)                   
         CP    WRKHRS,ZEROS                                                     
         BNE   RAT02                                                            
         ZAP   WRKHRS,PYTDHRS                                                   
*                                                                               
*                                                                               
         CLI   OPTYTD,C'M'         ALLOCATION RUN MONTHLY                       
         BNE   RAT02                                                            
         CLI   OPTHRS,C'Y'         STANDARD HRS REQUEST?                        
         BE    RAT02                                                            
         TM    PERSTAT2,PERHRRTO   ONLY HOURLY RATES?                           
         BNO   RAT02                                                            
         CP    PPERHRS,ZEROS       PERIOD HRS ZERO?                             
         BNE   RAT02                                                            
         CP    PPERHRP,ZEROS       POSITIVE PERIOD HRS ZERO?                    
         BE    RAT02                                                            
         USING MTHD,RF                                                          
         L     RF,AMTHRTAB         MONTHLY TABLE                                
         LA    R0,MTHMAX                                                        
         CLC   MTHCODE,END         USE POSITIVE HRS IN CURRENT MTH              
         BE    *+14                                                             
         LA    RF,MTHLN(RF)                                                     
         BCT   R0,*-14                                                          
         DC    H'0'                MUST FIND THE MONTH                          
         AP    MTHHRS,PPERHRP                                                   
         ZAP   WRKHRS,PPERHRP                                                   
         OI    PERSTAT2,PERHRRTA   MARK AS ADJ HR RATE                          
*                                                                               
RAT02    MVC   RATACC,ACCOUNT      EMPLOYEE'S 1R CODE                           
         LA    R0,RBKCNT           CLEAR HRLY RATES TO ZERO                     
         LA    R1,RBUKS                                                         
         ZAP   0(RBUKLN,R1),ZEROS                                               
         LA    R1,RBUKLN(R1)                                                    
         BCT   R0,*-10                                                          
*                                                                               
         L     R3,ASALAREA                                                      
         USING SALARYD,R3                                                       
         MVC   SALOFFC(L'SALOFFC+L'SALDEPT+L'SALSDPT+L'SALPRSN),OFFIC           
         CLI   OPTHRS,C'Y'         STANDARD HRS REQUEST?                        
         BNE   RAT03                                                            
         TM    PERSTAT,PERUSEAC    USE ACTUAL HOURS FOR THIS PERSON?            
         BNO   *+8                                                              
         NI    SALSTAT1,ALL-SALSTAND TURN OFF STANDARD BIT                      
RAT03    L     RF,AMTHRTAB                                                      
         LA    R0,MTHMAX           MAX NUMBER OF MONTHS                         
         LA    R1,SALACTM1         ACTUAL MONTHLY HOURS                         
         USING MTHD,RF                                                          
         ZAP   0(L'SALACTM1,R1),MTHHRS   INTO MONTHLY ACCUMS OF BLOCK           
         LA    RF,MTHLN(RF)                                                     
         LA    R1,L'SALACTM1(R1)                                                
         BCT   R0,*-14                                                          
         DROP  RF                                                               
RAT04    DS    0H                                                               
         NI    SALSTAT1,ALL-SALHRRAT-SALHRRTO TURN OFF HOURLY RATES             
         TM    PERSTAT2,PERHRRAT     INCLUDE HOURLY RATES?                      
         BNO   *+12                                                             
         OI    SALSTAT1,SALHRRAT     TURN ON INCLUDE HOURLY RATES               
         B     RAT06                                                            
         TM    PERSTAT2,PERHRRTO     ONLY HOURLY RATES?                         
         BNO   *+8                                                              
         OI    SALSTAT1,SALHRRTO     TURN ON HOURLY RATES ONLY                  
RAT06    DS    0H                                                               
         GOTO1 ACSALHST,DMCB,ACWORKD,(R3),RECORD2                               
         OC    SALSTAT2,SALSTAT2   ANY ERRORS?                                  
         BZ    RAT06B              NO                                           
         TM    SALSTAT2,ALL-SALINVPR ANY ERROR BUT PERSON MISSING               
         BZ    *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
RAT06B   CLI   OPTHRS,C'Y'         STANDARD HRS REQUEST?                        
         BNE   *+8                                                              
         OI    SALSTAT1,SALSTAND   RESET STANDARD BIT                           
         L     RF,ACOBLOCK         INITIALIZE COBLOCK FOR METHOD                
         USING COBLOCKD,RF                                                      
         CLI   COINDX,C'N'         INCLUDE IND PAY TYPES?                       
         BE    RAT08                                                            
         L     RE,AOVHWRK2         IND PAY TYPE OVERHEAD POOL                   
         USING OVERD,RE                                                         
         AP    OVAYTD,SALINDIR     ACCUMULATE IND PAY TYPE                      
         DROP  RE,RF                                                            
*                                                                               
         USING CMPUTD,R5                                                        
RAT08    L     R5,COMPRAT          TABL OF DISPS ACCUMS/HRLY RATES              
RAT08A   CLI   0(R5),EOT           END OF DISPLACEMENT TABLE                    
         BE    RAT22                                                            
         L     R4,APERWRK          EMPL'S TOTALS RECORD                         
         L     R2,ASALAREA         SALARY BLOCK FROM ACSALHST                   
         AH    R4,CMPTOTD          DISP TO PTOD ACCUM                           
         AH    R2,CMSLRD           DISP TO SALARYD ACCUM                        
         OC    CMSLRD,CMSLRD       IF Y(0) GET NEXT HRLY RATE                   
         BZ    RAT20                                                            
         CP    0(L'SALINFO,R2),ZEROS  $ FIGURE FOR THIS SAL TYPE?               
         BZ    RAT20               NO - LOOKUP NEXT                             
         ZAP   0(PBUKLN,R4),0(L'SALINFO,R2) YES -UPDATE EMPL SAL ACCUM          
*                                                                               
RAT10    TM    PERSTAT,PADJPCT     IS THERE AN ADJUSTMENT PCT?                  
         BNO   RAT14               NO - JUST ADD OR SUBTRACT                    
         ZAP   DIVWRK,PERADJ       GET ADJ AMOUNT (HAS 3 DEC PLACES)            
         MP    DIVWRK,0(PBUKLN,R4)                                              
         SRP   DIVWRK,64-6,5       SHIFT 6 (4 FOR PCT + 2 FOR DOLLARS)          
         ZAP   0(PBUKLN,R4),DIVWRK+(L'DIVWRK-PBUKLN)(PBUKLN)                    
*                                                                               
RAT14    AP    PCOST,0(PBUKLN,R4)  UPDATE TOTAL                                 
         TM    PERSTAT2,PERHRRTA   MTHLY EMPL WITH (+) AND (-) HRS              
         BO    *+14                SKIP ZERO HRS CHECK                          
         CP    PYTDHRS,ZEROS       EMPLOYEE'S YTD TOTAL HOURS ZERO?             
         BE    RAT20               YES - HRLY RATES ARE ZERO                    
         L     R2,ARATWRK          POINT R2 TO HRLY RATE REC                    
         AH    R2,CMRATD           DISP TO HOURLY RATE ACCUM                    
         ZAP   DIVWRK,0(PBUKLN,R4) SALARY TYPE COST FIGURE                      
         SRP   DIVWRK,4,0          SHIFT 4 POSITIONS TO LEFT                    
         DP    DIVWRK,WRKHRS       COST / HOURS = HOURLY RATE (4DP)             
         ZAP   0(RBUKLN,R2),DIVWRK(L'DIVWRK-L'WRKHRS)                           
         CP    WRKHRS,ZEROS        HRS POSITIVE?                                
         BNL   RAT18                                                            
         OI    ACCSTUS,NEGHOUR     SET STATUS TO INDICATE WARNING               
RAT18    AP    RRAT,0(RBUKLN,R2)   ADD TO OVERALL HOURLY RATE                   
RAT20    LA    R5,CMPULN(R5)       GO FIGURE NEXT HRLY RATE                     
         B     RAT08A                                                           
*                                                                               
RAT22    LA    R1,PBK              ANY EMPLOYEE ACCUMS?                         
         LA    R0,PBKCNT                                                        
RAT24    CP    0(PBUKLN,R1),ZEROS                                               
         BNZ   RAT26                                                            
         LA    R1,PBUKLN(R1)                                                    
         BCT   R0,RAT24                                                         
         B     EXIT                                                             
RAT26    DS    0H                                                               
         GOTO1 BINADD,DMCB,ARATWRK,RATBUFF   ADD EMPL HRLY RAT TO TABLE         
         TM    ACCSTUS,NEGHOUR                                                  
         BZ    EXIT                                                             
         MVI   WARNNUM,ERRNEGHR                                                 
         NI    ACCSTUS,ALL-NEGHOUR MARK NOT A NEGATIVE EMPLOYEE                 
         B     EXIT                                                             
         DROP  R3,R5,R6,R7                                                      
         EJECT                                                                  
*---------------------------------------------------------                      
*              LOOKING UP THE 1N ANALYSIS CODE                                  
*---------------------------------------------------------                      
*                                                                               
ALOOKUP  NTR1                                                                   
         USING CLID,R7                                                          
         MVI   INCOME,C'N'                                                      
         L     R7,ACLIWRK          CURRENT CLID RECORD                          
         MVI   CLIANALS,DPTANAL    DEFAULT IS DEPARTMENT                        
         TM    CLISTAT,CLINB+CLIPB+CLIHS NEW BIZ /PRO BONO/HOUSE CLIENT         
         BZ    ALOOK01                                                          
         TM    CLISTAT,CLINB                                                    
         BZ    ALOOK00                                                          
         CLI   OPTNBA,C'I'         ALLOC BY INCOME?                             
         BNE   *+8                                                              
         MVI   INCOME,C'Y'                                                      
         CLI   OPTNBT,SPACE        IS N.B. LEVEL OPTION SET?                    
         BNH   ALOOK01                                                          
         MVC   CLIANALS,OPTNBT                                                  
         B     ALOOK01                                                          
ALOOK00  TM    CLISTAT,CLIPB                                                    
         BZ    ALOOK00A                                                         
         CLI   OPTPBA,C'I'         ALLOC BY INCOME?                             
         BNE   *+8                                                              
         MVI   INCOME,C'Y'                                                      
         CLI   OPTPBT,SPACE        IS P.B. LEVEL OPTION SET?                    
         BNH   ALOOK01                                                          
         MVC   CLIANALS,OPTPBT                                                  
         B     ALOOK01                                                          
ALOOK00A CLI   OPTHSA,C'I'         ALLOC BY INCOME?                             
         BNE   *+8                                                              
         MVI   INCOME,C'Y'                                                      
         CLI   OPTHST,SPACE        IS HOUSE LEVEL OPTION SET?                   
         BNH   ALOOK01                                                          
         MVC   CLIANALS,OPTHST                                                  
*                                                                               
         USING PTOTD,R6                                                         
ALOOK01  L     R6,APERWRK                                                       
         TM    PERSTAT,PEREXEC     IS THIS EMPLOYEE AN EXECUTIVE?               
         BZ    ALOOK02                                                          
         MVI   CLIANALS,OFFANAL    DEFAULT IS TREAT ALL 1N AS OFFICE            
         CLI   OPTEXEC,SPACE                                                    
         BNH   ALOOK02                                                          
         MVC   CLIANALS,OPTEXEC                                                 
         B     ALOOK12             CHECK FOR SCHEME OVERRIDES                   
*                                                                               
ALOOK02  TM    CLISTAT,CLINB+CLIPB+CLIHS  IF NEWBIZ PRO BONO/HOUSE CLT          
         BNZ   ALOOK12             CHECK FOR SCHEME OVERRIDES                   
         CLI   CLICDE+1,SPACE      OLD STYLE 1N POSTING(1ND VAC)                
         BNE   ALOOK04             NO - CONTINUE                                
         CLI   CLICDE+2,SPACE      IS IT A 1 CHAR CODE?                         
         BE    ALOOK04                                                          
         CLI   OPTINOLD,C'Y'       WANT TO RECOGNIZE IT?                        
         BE    ALOOK02C            NO - LOOK UP CURRENT 1N AN= SETTING          
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'CLICDE-2),CLICDE+2                                        
         MVC   CLICDE,WORK                                                      
         B     ALOOK04                                                          
ALOOK02C MVC   CLIANALS(1),CLICDE  YES- USE FIRST DIGIT AS ANAL CODE            
         B     ALOOK12                                                          
*                                                                               
         USING BIND,R5             LOOK UP CURRENT AN=                          
ALOOK04  L     R5,NONCLTBF         ADDR OF 1N NON-CLIENT TABLE                  
         ICM   R0,15,BININ         NUMBER OF TABLE ENTRIES                      
         BZ    ALOOK07                                                          
         LA    RF,BINTABLE         1ST TABLE ENTRY                              
         USING NOND,RF                                                          
ALOOK06  CLC   CLICDE,NONCODE                                                   
         BNE   ALOOK06A                                                         
         MVC   CLIANALS,NONANAL                                                 
         MVC   CLICLIST,NONLISC    CLIENT LIST                                  
         B     ALOOK12             CHECK FOR EXEC                               
ALOOK06A LA    RF,NONLEN(RF)                                                    
         BCT   R0,ALOOK06                                                       
         DROP  RF                                                               
*                                                                               
         USING ACNVD,R4                     CHECK FOR NOHOURS CONTRA            
ALOOK07  L     R4,ANALCV                                                        
ALOOK08  CLI   ACNVCODE,EOT                 END OF TABLE                        
         BE    ALOOK12                      USE DEFAULT                         
         CLC   CLILEDAC(L'ACNVNOHR),ACNVNOHR  1N CODE                           
         BNE   *+14                                                             
         MVC   CLIANALS,ACNVCODE            ANALYSIS=                           
         B     ALOOK12                                                          
         LA    R4,ACNVLN(R4)                BUMP TO NEXT TABL ENTRY             
         B     ALOOK08                                                          
*                                                                               
ALOOK12  CLI   CLIANALS,PERANAL             PERSONAL TIME?                      
         BNE   *+14                                                             
         L     RF,ACOBLOCK                                                      
         USING COBLOCKD,RF                                                      
         MVC   CLIANALS,COIPER              PERSONAL TIME OVERRIDE              
         BAS   RE,CHECKAN                   CHECK AN= TO THE SCHEME             
         B     EXIT                                                             
         DROP  R4,R5,R6,R7,RF                                                   
         EJECT                                                                  
*---------------------------------------------------------                      
*              FIGURING THE 1N CONTRA ACCOUNT FOR NO HOURS                      
*---------------------------------------------------------                      
*                                                                               
NCONTRA  NTR1                                                                   
         GOTO1 CLERWRK                                                          
         USING CLID,R7                                                          
         L     R7,ACLIWRK                                                       
         ST    R7,ACLREC            SAVE ADDR OF CURRENT RECORD                 
         USING ACNVD,R4                     CHECK FOR NOHOURS CONTRA            
         L     R4,ANALCV                                                        
NCON02   CLI   ACNVCODE,EOT                 END OF TABLE                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OPTNOTS,ACNVCODE             NO HOURS TYPE LEVEL                 
         BE    *+12                                                             
         LA    R4,ACNVLN(R4)                BUMP TO NEXT TABL ENTRY             
         B     NCON02                                                           
         MVC   CLILEDAC(L'ACNVNOHR),ACNVNOHR  1N CODE                           
         MVC   CLIANALS,ACNVCODE            ANALYSIS=                           
         CLC   CLIANALS,SPACES                                                  
         BH    *+8                                                              
         MVI   CLIANALS,DPTANAL                                                 
         MVC   CLIANALR,CLIANALS            SAVE ORIGINAL AN= CODE              
         MVC   CLICLNME(L'ACNVNOHN),ACNVNOHN 1N ACCOUNT NAME                    
         L     R1,ASCHMREC                  ADDR OF SCHEME                      
         MVC   HALF,ACNVDIS                                                     
         AH    R1,HALF                      DISP TO CONVERSION ENTRY            
         MVC   CLIANALS,0(R1)               REPLACE ANALYSIS CODE               
         B     EXIT                                                             
         DROP  R4,R7                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*              CHECK AN= TO THE SCHEME                                          
*---------------------------------------------------------                      
*                                                                               
*                                                                               
CHECKAN  NTR1                                                                   
         USING CLID,R7                                                          
         L     R7,ACLIWRK                   CURRENT CLIENT RECORD               
         MVC   CLIANALR,CLIANALS            SAVE ORIGINAL AN= CODE              
*                                                                               
         USING ACNVD,R4                                                         
         L     R4,ANALCV                    ANALYSIS CONVERSION TABL            
CHECK02  CLI   ACNVCODE,EOT                 END OF TABLE                        
         BE    CHECK04                                                          
         CLC   CLIANALS,ACNVCODE            COMPARE TO TABLE ENTRY              
         BE    *+12                                                             
         LA    R4,ACNVLN(R4)                BUMP TO NEXT TABL ENTRY             
         B     CHECK02                                                          
         L     R1,ASCHMREC                  ADDR OF SCHEME                      
         MVC   HALF,ACNVDIS                                                     
         AH    R1,HALF                      DISP TO CONVERSION ENTRY            
         MVC   CLIANALS,0(R1)               REPLACE ANALYSIS CODE               
*                                                                               
CHECK04  TM    CLISTAT,CLINB+CLIPB+CLIHS  IF NEWBIZ PRO BONO/HOUSE CLT          
         BZ    EXIT                                                             
         CLI   INCOME,C'Y'                AND ALLOC BY INCOME                   
         BNE   EXIT                                                             
         MVI   CLIANALS,CORPANAL          DEALT WITH AT CORP LEVEL              
         CLI   CLIANALR,OFFANAL           BY OFFICE OR CORP CLTS                
         BE    *+8                                                              
         MVI   CLIANALR,CORPANAL                                                
         MVI   INCOME,C'N'                                                      
         B     EXIT                                                             
         DROP  R4,R7                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*              ROUTINE TO CHECK SPECIALS TABLE                                  
*---------------------------------------------------------                      
*                                                                               
         USING BIND,R5                                                          
SPECIAL  NTR1                                                                   
         L     R6,APERWRK                                                       
         USING PTOTD,R6                                                         
         L     R7,ACOBLOCK          A(COBLOCK) PROFILE BLOCK                    
         USING COBLOCKD,R7                                                      
         L     RE,ADACCSTA                                                      
         USING RSTELD,RE                                                        
         TM    RSTSTAT,RSTSPIAE     IS SHE AN EXECUTIVE?                        
         BZ    SP06                                                             
         OI    PERSTAT,PEREXEC      YES SET STATUS TO REFLECT                   
         CLI   COEXEC,C'N'          INCLUDE EXECS IN ALLOCATION                 
         BE    SP99NO               SET CC TO NO                                
         DROP  RE                                                               
*                                                                               
SP06     CLI   COINC,C'N'           INCLUDE IN ALLOCATION                       
         BE    SP99NO               SET CC TO NO                                
         CLI   CODONLY,C'N'         INCLUDE ONLY DIRECT COST?                   
         BE    *+8                                                              
         OI    PERSTAT,PERDIRT      SET EMPLOYEE STATUS TO REFLECT              
         CP    COPCT,=P'1000000'    PCT OF SALARY TO INCLUDE                    
         BE    *+14                                                             
         ZAP   PERADJ,COPCT                                                     
         OI    PERSTAT,PADJPCT      YES SET EMPLOYEE STATUS TO REFLECT          
         CLI   COHRRAT,C'Y'         INCLUDE HOURLY RATES?                       
         BNE   *+8                                                              
         OI    PERSTAT2,PERHRRAT    SET EMPLOYEE STATUS 2 TO REFLECT            
         CLI   COHRRAT,C'O'         ONLY HRLY RATES?                            
         BNE   *+8                                                              
         OI    PERSTAT2,PERHRRTO    SET EMPLOYEE STATUS 2 TO REFLECT            
*                                                                               
         CLI   OPTHRS,C'Y'          STANDARD HOURS REQUEST?                     
         BNE   SP99YES              NO  - YOUR DONE                             
         L     R2,ADACC                                                         
         AH    R2,DATADISP          1ST EL                                      
SP08     CLI   0(R2),0                                                          
         BE    SP99YES                                                          
         CLI   0(R2),EMPELQ                                                     
         BE    SP10                                                             
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    SP08                                                             
         DC    H'0'                                                             
         USING EMPELD,R2                                                        
SP10     TM    EMPSTAT,EMPSACT      USE ACTUAL AS STANDARD?                     
         BNO   SP99YES                                                          
         OI    PERSTAT,PERUSEAC     YES SET EMPLOYEE STATUS TO REFLECT          
*                                                                               
SP99YES  CR    RB,RB                EQUAL CONDITION ON EXIT                     
         B     EXIT                                                             
SP99NO   LTR   RB,RB                NOT EQUAL CONDITION ON EXIT                 
         B     EXIT                                                             
         DROP  R5,R6,R7                                                         
         EJECT                                                                  
*-------------------------------------------------------------------            
*        BUILD HYBRID CONTRA ENTRY                                              
*        THIS IS THE PART OF THE 1R ACCOUNT TO APPEND AS 1C CONTRA              
*        R2    IS ADDR OF RECORD                                                
*        R3    ADDR OF NAMEL                                                    
*-------------------------------------------------------------------            
*                                                                               
HYBRD    NTR1                                                                   
         MVC   CSTGRP,SPACES                                                    
         MVC   CSTGRPNM,SPACES                                                  
*                                        PORTION OF 1R FOR HYBRID CNTRA         
         MVC   CSTGRP,CACKACT-CACRECD(R2)                                       
*                                                                               
         USING NAMELD,R3                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSTGRPNM(0),NAMEREC       ACCOUNT NAME                           
*                                                                               
         USING HYBND,R2                                                         
         LA    R2,NMEWRK                                                        
         MVC   HYCODE,CSTGRP                                                    
         MVC   HYNAME,CSTGRPNM                                                  
         GOTO1 BINADD,DMCB,(R2),HYNMBUF  ADD TO TABLE                           
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*------------------------------------------------------------                   
*              ADD UP SUMMARY RECORDS - ADD TO HIGHER LEVELS                    
*------------------------------------------------------------                   
*                                                                               
*                                                                               
         USING SUMD,R6                                                          
SUMUP    NTR1                                                                   
         L     R6,0(R1)            ACCUMS TO ADD TO SUMBUFF                     
         L     R3,4(R1)            HIGHER LEVEL ACCUMS TO ADD TO                
*                                                                               
         LA    R1,SBK                                                           
         LA    R0,SBKCNT                                                        
         CP    0(SBKLN,R1),ZEROS                                                
         BNE   SUMUP3                                                           
         LA    R1,SBKLN(R1)                                                     
         BCT   R0,*-14                                                          
         B     EXIT                DON'T ADD IF ALL BUCKETS ZERO                
*                                                                               
SUMUP3   GOTO1 BINADD,DMCB,(R6),SUMBUFF       ADD IT TO TABLE                   
         LTR   R3,R3                                                            
         BZ    EXIT                NO HIGHER LEVEL                              
         LA    R1,SBK              ADD TO HIGHER LEVEL                          
         LA    R2,SBK-SUMD(R3)                                                  
         LA    R0,SBKCNT                                                        
SUMUP4   AP    0(SBKLN,R2),0(SBKLN,R1)                                          
         LA    R1,SBKLN(R1)                                                     
         LA    R2,SBKLN(R2)                                                     
         BCT   R0,SUMUP4                                                        
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------                   
*              CLEAR AN OVERHEAD POOL WORKAREA                                  
*              NOTE : R3 HAS ADDR OF WORKAREA COMING IN                         
*------------------------------------------------------------                   
*                                                                               
*                                                                               
         USING OVERD,R3                                                         
CLEROVH  NTR1                                                                   
         MVC   OVERD(OVLEN),SPACES                                              
         MVI   OVHSTATS,0          CLEAR STATUS                                 
         MVI   OVHSTAT2,0                                                       
         LA    R1,OVBK             1ST BUCKET                                   
         LA    R0,OVBUKCNT         NUMBER OF BUCKETS                            
         ZAP   0(OVBKLN,R1),ZEROS                                               
         LA    R1,OVBKLN(R1)                                                    
         BCT   R0,*-10                                                          
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*------------------------------------------------------------                   
*              CLEAR A SUMMARY BUFFER                                           
*------------------------------------------------------------                   
*                                                                               
*                                                                               
CLSUM    NTR1                                                                   
         USING SUMD,R6                                                          
         L     R6,AACCUM                                                        
         MVC   SUMD(SKLEN),SPACES                                               
         LA    R1,SBK                                                           
         LA    R0,SBKCNT                                                        
         ZAP   0(SBKLN,R1),ZEROS       CLEAR THE BUCKETS                        
         LA    R1,SBKLN(R1)                                                     
         BCT   R0,*-10                                                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*------------------------------------------------------------                   
*              PRINT DEPT./OFFICE SUMMARIES                                     
*------------------------------------------------------------                   
*                                                                               
*                                                                               
         USING BIND,R5                                                          
SUMMARY  NTR1                                                                   
         MVI   FORCEHED,C'Y'              **SUMMARY FOR PERIOD**                
         L     R5,SUMBUFF                                                       
         L     R3,BININ                   NUMBER OF SUMMARY RECORDS             
         LTR   R3,R3                                                            
         BZ    EXIT                                                             
         USING SUMD,R6                                                          
         LA    R6,BINTABLE                                                      
SUMM2    BAS   RE,SUMM10                  GET LABOR AND OVERALL TOTALS          
         ST    R6,ASUMREC                                                       
         GOTO1 REPORT,DMCB,(3,0),0        PRINT A SUMMARY RECORD                
         LA    R6,SLEN(R6)                                                      
         BCT   R3,SUMM2                                                         
*                                                                               
*                                                                               
         MVI   FORCEHED,C'Y'              **SUMMARY FOR YTD**                   
         L     R3,BININ                   NUMBER OF SUMMARY RECORDS             
         LA    R6,BINTABLE                                                      
SUMM4    AP    SDIRPST,SDIRLST            ADD YTD-1 TO POSTING                  
         AP    SDINDPST,SDINDLST          TO GET YTD                            
         AP    SOINDPST,SOINDLST                                                
         AP    SCINDPST,SCINDLST                                                
         AP    SOVHPST,SOVHLST                                                  
         AP    SOVOPST,SOVOLST                                                  
         BAS   RE,SUMM10                  GET LABOR AND OVERALL TOTALS          
         ST    R6,ASUMREC                                                       
         GOTO1 REPORT,DMCB,(3,0),(1,0)    PRINT A SUMMARY RECORD                
         LA    R6,SLEN(R6)                                                      
         BCT   R3,SUMM4                                                         
         B     EXIT                                                             
*                                                                               
*                                                                               
SUMM10   ZAP   STTIME,SDIRPST             GET LABOR TOTALS                      
         AP    STTIME,SDINDPST                                                  
         AP    STTIME,SOINDPST                                                  
         AP    STTIME,SCINDPST                                                  
         ZAP   STCOST,STTIME              GET OVERALL TOTAL                     
         AP    STCOST,SOVHPST                                                   
         AP    STCOST,SOVOPST                                                   
         BR    RE                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* DETERMINE IF THIS 1C ACCOUNT IS NEW BUSINESS CLIENT                *          
**********************************************************************          
         SPACE 1                                                                
NBTEST   NTR1                                                                   
         USING CLID,R7                                                          
         L     R7,ACLREC            SAVED ADDR OF CURRENT RECORD                
         CLI   OPTNBT,C' '                                                      
         BH    NBTEST01             NEW BUSINESS OPTION?                        
         CLI   OPTHST,C' '                                                      
         BH    NBTEST01             HOUSE CLIENT OPTION?                        
         CLI   OPTPBT,C' '                                                      
         BNH   EXIT                 PRO BONO OPTION?                            
*                                                                               
         USING BIND,R5                                                          
NBTEST01 L     R5,NEWBIZBF         NEW BUSINESS PRO BONO TABLE                  
         L     R0,BININ                                                         
         LTR   R0,R0                                                            
         BP    *+10                                                             
         BZ    EXIT                                                             
         DC    H'0'                                                             
         USING NEWBD,RF                                                         
         LA    RF,BINTABLE         NEW BUSINESS TABLE                           
*                                                                               
NBTEST04 SR    R1,R1                                                            
         IC    R1,NEWBCLN          LNGTH OF N.B. TABLE ENTRY MINUS 1            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NEWBCDE(0),CLICDE    IS IT A MATCH                               
         BE    NBTEST90             YES - SET SWITCH AND LEAVE                  
         LA    RF,NEWBLEN(RF)       NO  - BUMP TABLE                            
         BCT   R0,NBTEST04          TRY AGAIN FOR A MATCH                       
         B     NBTEST99             NOT FOUND , IT'S NOT N.B.                   
*                                                                               
NBTEST90 DS    0H                                                               
         TM    NEWBSTAT,NEWBNB      IS IT NB                                    
         BZ    *+12                                                             
         OI    CLISTAT,CLINB                                                    
         B     NBTEST99                                                         
         TM    NEWBSTAT,NEWBPB      IS IT PB                                    
         BZ    *+12                                                             
         OI    CLISTAT,CLIPB                                                    
         B     NBTEST99                                                         
         OI    CLISTAT,CLIHS        OR HOUSE                                    
NBTEST99 B     EXIT                                                             
         DROP  R5,R7,RF                                                         
         EJECT                                                                  
**********************************************************************          
* PUT OUT A DUMMY 1R - 1C OR 1N REPRESENTATIVE BUCKET                *          
**********************************************************************          
         SPACE 1                                                                
DUMSRT   NTR1                                                                   
         L     R5,ASRTWRK                                                       
         USING SRTD,R5                                                          
         GOTO1 CLERSRT,DMCB,(R5)   **DUMMY 1R - 1C OR 1N BUCKET **              
         L     R7,ACLIWRK                                                       
         USING CLID,R7                                                          
         L     RF,ADSUBAC                                                       
         SH    RF,DATADISP                                                      
         USING CACRECD,RF                                                       
         MVC   SRTKACC,CACKCULA    1R ACCOUNT CODE                              
         MVC   SRTBTYP,CACKBTYP    BUCKET TYPE  (TWO BYTES)                     
         MVC   SRTCLGAC,CLILEDAC   CONTRA LEDGER AND ACCOUNT CODE               
         MVC   SRTCNAME,CLICLNME   CONTRA NAME                                  
         DROP  RF                                                               
*                                                                               
         MVC   MSG,=CL10'DUM SRT'                                               
         GOTO1 ADUMP,DMCB,(RC),(R5),SRTLEN                                      
*                                                                               
         GOTO1 ADSORTER,DMCB,(L'PUTC,PUTC),(R5)                                 
*                                                                               
* POST HERE EITHER THE MONTHLY BUCKET OR COMPLETE YTD AND MARK OTHERS           
*      AS WITH THE SAME 1C/1R/ANAL AS NOT CHANGED                               
*                                                                               
         USING CACRECD,RF                                                       
         L     RF,ADSUBAC                                                       
         SH    RF,DATADISP                                                      
*                                                                               
         CLC   CACKCUNT(2),=C'1C'  ONLY ADD IN 1C ACCOUNT                       
         BNE   DUMSRTX                                                          
         TM    ACCSTUS,OHDACC+INDTACC  IS THIS AN OVERHEAD ACCOUNT?             
         BNZ   DUMSRTX                                                          
*                                                                               
         USING PLBUFD,R2                                                        
         L     R2,APALWRK                                                       
         XC    0(PLLNQ,R2),0(R2)   CLEAR WORK AREA                              
         MVC   PLCPY,SRTCOMP       COMPANY                                      
         MVC   PL1CACC,CACKCACT    1C ACCOUNT                                   
         MVC   PL1RACC,CACKACT     1R ACCOUNT                                   
         MVC   PLMTHD,SRTMETHD     METHOD                                       
         MVC   PLYYMM,SRTMOS       YEAR/MONTH                                   
         MVC   PLANAL,GROUP        ANALYSIS CODE                                
         MVC   PLPTYP,SRTBTYPE     PAYROLL TYPE                                 
         ZAP   PLAMNT,ZEROS        AMOUNT                                       
*                                                                               
         MVC   MSG,=CL10'PAL-ADD D'                                             
         GOTO1 ADUMP,DMCB,(RC),(R2),PLLNQ                                       
*                                                                               
*        LA    RF,*+10             HIGH CORE TABLE                              
*        O     RF,=X'80000000'                                                  
*        BSM   0,RF                31 BIT MODE                                  
         GOTO1 BIN31,DMCB,(R2),PALBUFF  ADD TO TABLE                            
*        LA    RF,*+6                                                           
*        BSM   0,RF                24 BIT MODE                                  
*                                                                               
         GOTO1 AMRKYTD             MARK YTD TABLE THAT THIS WAS POSTED          
*                                                                               
DUMSRTX  B     EXIT                                                             
         DROP  R5,RF                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE THE METHOD OF ALLOCATION RECORD                           *          
**********************************************************************          
         SPACE 1                                                                
METVAL   NTR1                                                                   
         MVC   ARECORD,RECORD      USE "RECORD"                                 
         USING CAHRECD,R6                                                       
         L     R6,DIO                                                           
         XC    CAHKEY(L'CDIO),CAHKEY                                            
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E' METHOD (HISTORY) RECORD                
         MVI   CAHKSUB,CAHKSUBQ    X'01'                                        
         MVC   CAHKCPY,RCCOMPFL    COMPANY                                      
         XC    CAHKOFC,CAHKOFC     CLEAR OFFICE TO 000 FOR NOW*******           
         MVC   CAHKMTHD,METHOD     METHOD OF ALLOCATION (NUMBER)                
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,DIO,DIO                        
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BO    MTNO_1                                                           
*                                  INVALID METHOD REQUESTED                     
         MVC   DA,CAHKDA           DISK ADDRESS OF RECORD                       
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,ARECORD,DMWORK              
         NI    DMINBTS,ALL-PASSDEL                                              
         CLI   8(R1),0             DID I FIND THE RECORD?                       
         BNE   MTNO_1                                                           
*                                                                               
         L     R6,ARECORD                                                       
         GOTO1 HELLO,ELIST,(C'G',MST),('METELQ',CAHRECD),0                      
         CLI   ELERR,0                                                          
         BNE   MTNO_1              YOU MUST HAVE A METHOD ELEMENT               
*                                                                               
         USING METELD,R4                                                        
         L     R4,ELADDR                                                        
         MVC   METHCODE,METCODE    SAVE METHOD CODE FOR HEADLINES               
*                                                                               
         GOTO1 HELLO,ELIST,(C'G',MST),('PATELQ',CAHRECD),0                      
         CLI   ELERR,0                                                          
         BNE   MTNO_2              YOU MUST HAVE PAY ELEMENTS                   
*                                                                               
*                                                                               
         GOTO1 HELLO,ELIST,(C'G',MST),('NAMELQ',CAHRECD),0                      
         CLI   ELERR,0                                                          
         BNE   MTNO_1              YOU MUST HAVE A NAME ELEMENT                 
*                                                                               
         MVC   METHNAME,SPACES     CLEAR METHOD NAME SAVE AREA                  
         L     R4,ELADDR                                                        
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    MT99YES                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   METHNAME(0),NAMEREC       METHOD DESCRIPTION                     
         B     MT99YES                                                          
*                                                                               
MTNO_1   MVI   BYTE,ERRMETH        INVALID METHOD                               
         B     *+8                                                              
MTNO_2   MVI   BYTE,ERRPTYP        INVALD PAY TYPES                             
*                                                                               
         USING ERRORD,R2                                                        
         MVC   WORK,SPACES                                                      
         LA    R2,WORK                                                          
         MVC   ERRNUM,BYTE                                                      
         MVI   ERRTYPE,ERRERROR  SET TO ERROR                                   
         GOTO1 BINADD,DMCB,(R2),ERRORBUF ADD TO TABLE                           
         OI    RUNSTAT,ERROUT      FATAL ERROR                                  
         CLI   QOPT1,C'L'                                                       
         BNE   *+8                                                              
         OI    RUNSTAT,POSTERR     POSTING ERROR                                
*                                                                               
MT99NO   LTR   RB,RB                NOT EQUAL CONDITION ON EXIT                 
         B     EXIT                                                             
MT99YES  CR    RB,RB                EQUAL CONDITION ON EXIT                     
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE CALENDAR RECORD                                           *          
**********************************************************************          
         SPACE 1                                                                
CALVAL   NTR1                                                                   
         MVC   STANDST,START       SET STD HRS START TO REQUEST START           
         MVC   ARECORD,RECORD      USE "RECORD"                                 
         USING CASRECD,R6                                                       
         L     R6,DIO                                                           
         XC    CASKEY(L'CDIO),CASKEY                                            
         MVC   CASKEY,SPACES                                                    
         MVI   CASKTYP,CASKTYPQ    X'3E' CALENDAR RECORD                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,RCCOMPFL    COMPANY                                      
         MVC   CASKEMOA,END        PERIOD                                       
         XC    CASKSMOA,CASKSMOA                                                
         MVC   CASKOFC,SPACES      OFFICE                                       
         MVC   SAVEKEY,CASKEY                                                   
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,DIO,DIO                        
         CLC   CASKEY(CASKEMOA-CASRECD),SAVEKEY                                 
         BE    *+6                                                              
         DC    H'0'                COMPANY  CALENDAR NOT FOUND                  
         CLC   CASKEMOA,END                                                     
         BNL   *+6                                                              
         DC    H'0'                DEFAULT NOT FOUND                            
         CLC   CASKSMOA,START                                                   
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   DA,CASKDA           SAVE DEFAULT DISK ADDRESS                    
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,ARECORD,DMWORK              
         NI    DMINBTS,ALL-PASSDEL                                              
         CLI   8(R1),0             DID I FIND THE RECORD?                       
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
*                                                                               
         CLC   START+1(1),=X'01'   IF JAN MAY HAVE TO ADJUST                    
         BNE   EXIT                                                             
*                                                                               
         L     R6,ARECORD                                                       
         LA    R4,CASRFST          BUMP TO FIRST ELEMENT                        
         USING TMPELD,R4                                                        
CALV10   CLI   TMPEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF YOU GET TO END                        
         CLI   TMPEL,TMPELQ                                                     
         BE    CALV30                                                           
         SR    R1,R1                                                            
         IC    R1,TMPLN                                                         
         AR    R4,R1                                                            
         B     CALV10                                                           
CALV30   CLC   TMPMTH,START        START MONTH JAN                              
         BNE   EXIT                NOT CROSSING YEARS SO DONT WORRY             
         CLI   TMPNUMB,X'01'       PERIOD #1?                                   
         BE    *+6                                                              
         DC    H'0'                PERIOD #1 SHOULD BE FIRST ELEMENT            
         CLC   TMPSTART+1(1),=X'12'  IF DEC NO NEED TO ADJUST                   
         BE    EXIT                                                             
         MVC   STANDST,TMPSTART    SAVE 1ST PERIOD OF START MONTH               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*------------------------------------------------------------------             
*              VALIDATE THE STD HRS RECORD                                      
*------------------------------------------------------------------             
*                                                                               
*                                                                               
STDVAL   NTR1                                                                   
         L     R4,ASALAREA                                                      
         USING SALARYD,R4                                                       
         MVC   ARECORD,RECORD      USE "RECORD"                                 
         USING STDRECD,R6                                                       
         L     R6,DIO                                                           
         XC    STDKEY(L'CDIO),STDKEY                                            
         MVC   STDKEY((STDKPER-STDRECD)+L'STDKPER),SPACES                       
         MVI   STDKTYP,STDKTYPQ    X'3E' STANDARD HOURS RECORD                  
         MVI   STDKSUB,STDKSUBQ    X'0D'                                        
         MVC   STDKCPY,RCCOMPFL    COMPANY                                      
         MVI   STDKYR,0                                                         
         MVI   STDKSEQ,0                                                        
         MVC   SAVEKEY,STDKEY                                                   
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,DIO,DIO                        
         B     STDV04                                                           
STDV02   GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),DIR,DIO,DIO                        
STDV04   NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),0             DID I FIND THE RECORD?                       
         BNZ   STDNO                                                            
         CLC   STDKEY((STDKPER-STDRECD)+L'STDKPER),SAVEKEY   TO PERS            
         BNE   STDNO                                                            
         CLC   STDKSTDT,SALEND     FILTER DIR WITHIN START/END                  
         BH    STDV02                                                           
         CLC   STDKENDT,SALSTART                                                
         BL    STDV02                                                           
*                                                                               
STDYES   CR    RB,RB                EQUAL CONDITION ON EXIT                     
         B     EXIT                                                             
STDNO    DS    0H                                                               
         USING ERRORD,R2                                                        
         MVC   WORK,SPACES                                                      
         LA    R2,WORK                                                          
         MVI   ERRNUM,ERRSTDHR                                                  
         MVI   ERRTYPE,ERRERROR  SET TO ERROR                                   
         GOTO1 BINADD,DMCB,(R2),ERRORBUF ADD TO TABLE                           
         OI    RUNSTAT,ERROUT       FATAL ERROR                                 
         CLI   QOPT1,C'L'                                                       
         BNE   *+8                                                              
         OI    RUNSTAT,POSTERR      POSTING ERROR                               
         LTR   RB,RB                NOT EQUAL CONDITION ON EXIT                 
         B     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*------------------------------------------------------------------             
*              CHECK FOR INCOME AND/OR HOURS PROFILES                           
*------------------------------------------------------------------             
*                                                                               
*                                                                               
INCCEK   NTR1                                                                   
         MVI   INCOME,C'N'                                                      
         MVI   BUFFIT,0                                                         
         USING CAPRECD,R6                                                       
         L     R6,RECORD           RECORD READING AREA                          
         XC    CAPKEY(L'CDIO),CAPKEY                                            
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    X'3E'                                        
         MVI   CAPKSUB,CAPKSUBQ    X'09'                                        
         MVC   CAPKCPY,RCCOMPFL    COMPANY                                      
         MVC   CAPKMTHD,METHOD     METHOD OF ALLOCATION (NUMBER)                
         MVC   SAVEKEY,CAPKEY                                                   
         OI    DMINBTS,PASSDEL                                                  
INC02    GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,RECORD,RECORD                  
         B     INC02B                                                           
INC02A   GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),DIR,RECORD,RECORD                  
INC02B   CLC   SAVEKEY(4),CAPKEY                                                
         BNE   EXIT                STILL METHOD X PROFILES                      
*                                                                               
         MVC   DA,CAPKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,RECORD,DMWORK               
         TM    8(R1),ALL-PASSDEL                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CAPRECD+(CAPRFST-CAPRECD)                                     
         USING OPDELD,R2                                                        
INC04    CLI   0(R2),0                                                          
         BE    INC02A                                                           
         CLI   0(R2),OPDELQ        OPTION(PROFILE) DATA?                        
         BE    INC08                                                            
INC06    SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     INC04               NEXT ELEMENT                                 
*                                                                               
INC08    LA    RF,INCTABLE         PROFILE NUMBERS FOR INCOME PROFILES          
INC08A   CLI   0(RF),X'FF'                                                      
         BE    INC06                                                            
         CLC   OPDNUM,0(RF)                                                     
         BNE   INC10                                                            
         CLI   OPDDATA,C'I'                                                     
         BE    INC08B                                                           
         CLI   OPDDATA,C'H'                                                     
         BE    INC08C                                                           
         CLI   OPDDATA,C'P'                                                     
         BNE   INC10                                                            
         OI    BUFFIT,INDIRIT      SET TO POST DPT IND TO BUFFALO               
         B     INC10                                                            
INC08B   MVI   INCOME,C'Y'         SET TO READ INCOME                           
         B     INC10                                                            
INC08C   OI    BUFFIT,HOURIT       SET TO POST HOURS TO BUFFALO                 
INC10    LA    RF,1(RF)                                                         
         B     INC08A                                                           
*                                                                               
*                                                                               
*                                                                               
INCTABLE DC    AL1(CONBAB#)                                                     
         DC    AL1(COPBAB#)                                                     
         DC    AL1(COHOAB#)                                                     
         DC    AL1(COVAB#)                                                      
         DC    AL1(COOCIND#)                                                    
         DC    AL1(COACIND#)                                                    
         DC    AL1(EOT)                                                         
                                                                                
         DROP  R2,R6                                                            
         EJECT                                                                  
*------------------------------------------------------------------             
*              READ COMPANY REC TO CHECK IF NEW OFFICE                          
*------------------------------------------------------------------             
*                                                                               
READCO   NTR1                                                                   
         USING CPYRECD,R6                                                       
         L     R6,RECORD           RECORD READING AREA                          
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,RCCOMPFL                                                 
         MVC   SAVEKEY,CPYKEY                                                   
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,RECORD,RECORD                  
         CLC   SAVEKEY(L'CPYKEY),CPYKEY    CO REC MUST EXIST                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,CPYKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,CPYRECD,DMWORK              
*                                                                               
         LA    R2,CPYRECD+(CPYRFST-CPYRECD) POINT TO 1ST ELEM                   
READC10  CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                X'10' MUST BE THERE                          
         CLI   0(R2),CPYELQ        X'10' COMPANY ELEMENT                        
         BE    READC20                                                          
         ICM   R1,1,1(R2)                                                       
         AR    R2,R1                                                            
         B     READC10                                                          
*                                                                               
         USING CPYELD,R2                                                        
READC20  MVI   OFF1RLN,ONEBYTE             DEFAULT TO 1 BYTE OFFICES            
         TM    CPYSTAT4,CPYSOFF2           NEW OFFICES?                         
         BZ    *+8                                                              
         MVI   OFF1RLN,TWOBYTE                                                  
         B     EXIT                                                             
         DROP  R6,R2                                                            
         EJECT                                                                  
*********************************************************************           
* CONSTANTS                                                         *           
*********************************************************************           
         SPACE 1                                                                
         DS    0D                                                               
VADDTRN  DC    AL1(QADDTRN),AL3(0)  LOADABLE PHASES                             
         DC    AL1(0)                                                           
*                                                                               
RELOTAB  DS    0A                                                               
         DC    V(ACSLRY)                                                        
         DC    V(ACSALHST)                                                      
         DC    V(GETCAP)                                                        
         DC    V(COVAIL)                                                        
         DC    V(DATVAL)                                                        
         DC    V(HELLO)                                                         
         DC    V(PRNTBL)                                                        
         DC    V(ACTRAVL)                                                       
         DC    V(BUFFERIN)                                                      
         DC    V(BINSRCH)                                                       
         DC    5F'0'               SPARE                                        
         DC    A(DEPTAB)                                                        
         DC    A(DEPOTAB)                                                       
         DC    A(OFFTAB)                                                        
         DC    A(OFFOTAB)                                                       
         DC    A(CORTAB)                                                        
         DC    A(COROTAB)                                                       
         DC    A(VCOMMON)          COMMON ROUTINES NMOD # 1                     
         DC    A(VCOMMON2)         COMMON ROUTINES NMOD # 2                     
         DC    A(VCOMMON3)         COMMON ROUTINES NMOD # 3                     
         DC    A(VCOMMON4)         COMMON ROUTINES NMOD # 4                     
         DC    A(CRECORD)                                                       
         DC    A(CRECORD2)                                                      
         DC    A(CDIO)                                                          
         DC    A(CPLCSAV)                                                       
         DC    A(CRCVIO)                                                        
         DC    A(CCOBLOCK)                                                      
         DC    A(CSALAREA)                                                      
         DC    A(CUPDBLOK)                                                      
         DC    A(CTSRBLK1)                                                      
         DC    A(CDDIN)                                                         
         DC    A(CCOMTAB)                                                       
         DC    A(CDICTTAB)                                                      
         DC    A(CCLRTAB)                                                       
         DC    A(CCOMPRAT)                                                      
         DC    A(CSCHMDEF)                                                      
         DC    A(CSCHDFLT)                                                      
         DC    A(CSUMPST)                                                       
         DC    A(CSTDOPTT)                                                      
         DC    A(CFIXOPTT)                                                      
         DC    A(CSCHMTBL)                                                      
         DC    A(CSCHMCON)                                                      
         DC    A(CANALCV)                                                       
         DC    A(COHDEFN)                                                       
         DC    A(CPOLDEFN)                                                      
         DC    A(CERRDEF)                                                       
         DC    A(AMAINTAB)                                                      
         DC    A(AWORKTAB)                                                      
         DC    A(ACONTTAB)                                                      
         DC    A(DUMP)                                                          
         DC    2F'0'                                                            
         DC    X'FF'                                                            
*                                                                               
GLOBALS  DS    0X                                                               
         DC    P'0'         ZEROS                                               
         DC    PL4'100'     MAX DUMP - 100                                      
         DC    C'1N'        NONCLI                                              
         DC    C'1C'        CLILEG                                              
         DC    C'1R'        EMPLEDG                                             
         DC    CL6'ACCDIR'                                                      
         DC    CL6'ACCMST'                                                      
         DC    CL8'GETREC  '                                                    
         DC    CL8'ADDREC  '                                                    
         DC    CL8'PUTREC  '                                                    
         DC    12X'FF'      FOXES                                               
         DC    12C'9'       NINES                                               
         DC    C'SET  '     SETC                                                
         DC    C'PUT  '     PUTC                                                
         DC    C'ADD  '     ADDC                                                
         DC    C'GET  '     GETC                                                
         DC    C'SEQ  '     SEQC                                                
         DC    C'END  '     ENDC                                                
         DC    C'HIGH '     HIGHC                                               
         DC    C'CLEAR'     CLEARC                                              
GLOBALSL EQU   *-GLOBALS                                                        
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,)'                              
*                                                                               
*                                                                               
YTDBUF   BUFFD TYPE=P,                                                 *        
               KEYLEN=YTDKLEN,                                         *        
               COLUMNS=1,                                              *        
               COMLEN=1,                                               *        
               REPCOM=NO,                                              *        
               BUFFERS=250,                                            *        
               FILE=SOUT                                                        
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* REMAINING ADDRESSABILITY                                           *          
**********************************************************************          
         SPACE 1                                                                
ABILITY  EQU   (4095*3)-(*-ACCA02)    REMAINING ADDRESSIBILITY                  
*                                     FREE UP ALL USINGS                        
         DROP                                                                   
         EJECT                                                                  
**********************************************************************          
* ROUTINES ENTERABLE FROM BASE OR WORK NMODS                         *          
* ************WORK NMOD # 1 ****************                         *          
**********************************************************************          
         SPACE 1                                                                
         DS    0D                                                               
VCOMMON  NMOD1 0,**COMM1**,R9,R8                                                
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         LA    RC,SPACEND                                                       
         USING ACCAD,RC            RC=A(LOCAL W/S)                              
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     VINDPOST            1 -ALLOCATE INDIRECT                         
         B     VDISOVH             2 -DISTRIBUTE OVERHEAD                       
         B     VMINUS1             3 -GET YTD-1                                 
         B     VPROFIT             4 -PROFILE LOOKUP                            
         B     VASCHEME            5 -ALLOCATION SCHEME LOOKUP                  
         B     VBUFPOST            6 -PUT CLIENT DIRECT/DPT IND TO BUF          
         B     VEMPLOY             7 -MAKE EMPLOYEE POSTINGS TO SORTER          
         B     VMRKYTD             8 -MARK YTDPLTB TABLE EACH POSTING           
         DC    (COMSPARE*L'COM1)X'00'                                           
*                                                                               
COM1XIT  XIT1  ,                                                                
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO ALLOCATE INDIRECT AT END OF DEPT/OFFICE/REQ             *          
**********************************************************************          
         SPACE 1                                                                
VINDPOST DS    0H                                                               
         TM    UPSI,POSTRATE       POST RATE RECORDS ONLY?                      
         BO    COM1XIT             YES - DON'T BOTHER WITH IND POOLS            
         L     R5,INDBUFF          INDIRECT POOL TABLE                          
         MVI   TYPE,ZERO           CLEAR POOL TYPE STATUS                       
         USING BIND,R5                                                          
         L     R1,BININ                                                         
         LTR   R1,R1                                                            
         BZ    COM1XIT             NO INDIRECT POOLS                            
*                                                                               
         OI    TYPE,INDITYPE       FOR INDIRECT ALLOCATION                      
         CLI   OPTSHOWD,C'Y'       SHOW 14 DIRECT GROUP?                        
         BNE   *+8                                                              
         OI    TYPE,DIRGRUP                                                     
         BAS   RE,TOTDIR           GET TOTAL DIRECT TIME AT THIS LEVEL          
         ZAP   TOTYTD,DIRYTD       SAVE INITIAL TOTAL DIRECT TIME               
*                                                                               
INDP01   L     R5,INDBUFF          INDIR POOL TBL (RESET FOR REREAD)            
         L     R3,BININ            NUMBER OF ENTRIES IN IND POOL TABLE          
         LA    R4,BINTABLE                                                      
         USING INDID,R4                                                         
*                                                                               
INDP02   XC    APOLREC,APOLREC     CLEAR ADDR OF CURRENT POOL RECORD            
         TM    INDSTAT2,INDABSOR   HAS THIS IND POOL BEEN ABSORBED              
         BO    INDP20              YES - SKIP IT                                
         CLI   LEVEL,COMP          IF CORP POST ALL UNABSORBED INDIRECT         
         BE    INDP10                                                           
         CLI   LEVEL,DEPT          IF LEVEL IS DEPT THEN ONLY CONSIDER          
         BNE   INDP04                                                           
         CLI   INDTYPE,INDDEPT     "DEPT TYPES" OF INDIRECT POOLS               
         BNE   INDP20                                                           
         SR    R1,R1                                                            
         IC    R1,LLEVB                                                         
         B     INDP06              OFFICE DEPT MUST MATCH                       
INDP04   CLI   LEVEL,OFFICE                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   INDTYPE,INDCORP     SKIP CORP POOLS AT OFFICE LEVEL              
         BE    INDP20                                                           
         CLI   INDTYPE,INDGRUP     SKIP OFFC GROUP POOLS AT OFF LEVEL           
         BE    INDP20                                                           
         SR    R1,R1                                                            
         IC    R1,LLEVA            OFFICE MUST MATCH                            
INDP06   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   INDOFDEP(0),ACCOUNT                                              
         BL    INDP20              IF POOL LOWER,GET NEXT POOL ENTRY            
         BH    INDP99              IF POOL HIGHER,YOUR DONE FOR NOW             
         CP    TOTYTD,ZEROS        TOTAL DIRECT YTD                             
         BNE   INDP10                                                           
         L     R1,ASRTWRK                                                       
         MVC   0(SRTLEN,R1),SPACES                                              
         MVI   FORCEHED,C'Y'       FORCE FIRST HEADING                          
         GOTO1 REPORT,DMCB,(2,0),(1,0)    NO DIRECT TIME MESSAGE                
         B     INDP99                                                           
*                                                                               
* WE HAVE A VALID IND POOL TO ALLOCATE                                          
*    GO CHECK ALLOC SCHEME APPROPIATE FOR                                       
*    THIS POOL.COME BACK FRM ASCHEME WITH                                       
*    CLIBUFF CONTAINING THE CLIENTS                                             
*    ELIGIBLE TO BE HIT WITH THIS POOL.                                         
*    DIRLST AND DIRYTD WILL CONTAIN TOTAL                                       
*                                                                               
INDP10   CLI   LEVEL,DEPT          DEPT LEVEL CAN'T USE INDPLUS                 
         BNE   *+8                                                              
         NI    INDSTATS,ALL-INDPLUS  DIR +DPT IND  ALLOCATED POOL?              
         ST    R4,APOLREC          ADDR OF CURRENT TABLE ENTRY                  
         MVC   SCHEMKEY,SPACES                                                  
         MVC   SCHEMKEY(L'INDOFDEP),INDOFDEP                                    
         XC    SCHMOPT,SCHMOPT                                                  
         XC    POOLTYPE,POOLTYPE                                                
         OI    POOLTYPE,SCMINDI    MARK AS INDIRECT POOL                        
         USING ACNVD,RF                                                         
         L     RF,ANALCV           ANALYSIS CONVERSION TABLE                    
*                                                                               
INDP10A  CLI   ACNVCODE,EOT                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   INDORIG,ACNVCODE    MARK THE POOL AS DEPT ,OFFC,CORP             
         BE    *+12                                                             
         LA    RF,ACNVLN(RF)                                                    
         B     INDP10A                                                          
         OC    POOLTYPE(1),ACNVBIT                                              
         DROP  RF                                                               
*                                                                               
         MVI   BFSTAT,0                                                         
         TM    INDSTATS,INDINCOM   INCOME ALLOCATED POOL?                       
         BZ    *+8                                                              
         MVI   INCOME,C'Y'                                                      
         TM    INDSTATS,INDHOURS   HOURS ALLOCATED POOL?                        
         BZ    *+8                                                              
         OI    BFSTAT,HRSSTAT                                                   
         TM    INDSTATS,INDPLUS    DIR +DPT IND  ALLOCATED POOL?                
         BZ    *+8                                                              
         OI    BFSTAT,INSSTAT                                                   
         GOTO1 ASCHEME                                                          
         MVI   INCOME,C'N'                                                      
         MVI   BFSTAT,0                                                         
         CP    DIRYTD,ZEROS                                                     
         BNE   INDP18                                                           
*                                  *-------------------------------*            
*                                  * NO DIRECT TIME (YTD)          *            
*                                  * BASED ON CURRENT SCHEME       *            
*                                  *-------------------------------*            
         CLI   LEVEL,COMP          IF CORP LEVEL SKIP THE ERROR MSG             
         BE    INDP20                                                           
         L     R1,ASRTWRK                                                       
         MVC   0(SRTLEN,R1),SPACES                                              
         MVI   FORCEHED,C'Y'       FORCE FIRST HEADING                          
         GOTO1 REPORT,DMCB,(2,0),(1,0)    NO DIRECT TIME MESSAGE                
         B     INDP20                                                           
*                                  NO DIRECT TIME FOR THIS SCHEME               
*                                  WE'LL ALLOCATE THIS ONE NEXT LEV UP          
*                                                                               
*                                                                               
*                                                                               
INDP18   BAS   RE,INDALLOC         THEN GO ALLOCATE POOL ENTRY                  
INDP20   LA    R4,INLEN(R4)        GET NEXT INDIRECT POOL ENTRY                 
         BCT   R3,INDP02                                                        
INDP99   MVI   TYPE,ZERO           CLEAR POOL TYPE STATUS                       
         B     COM1XIT                                                          
         EJECT                                                                  
*------------------------------------------------------------                   
*SPREAD BACK OF INDIRECT                                                        
*                                                                               
*DIRLST AND DIRYTD HAVE TOTAL DIRECT FOR THIS LEVEL/SCHEME                      
*INDDYTD HAVE THE AMOUNT TO BE SPREAD BACK AT THIS LEVEL/SCHEME.                
*READ CLIBUFF FOR INDIVIDUAL CLIENT TIME AT THIS LEVEL/SCHEME                   
*SPREAD BACK IN PROPORTION OF CLIENTTIME TO TOTAL TIME                          
*-------------------------------------------------------------                  
INDALLOC NTR1                                                                   
         USING INDID,R4                                                         
         L     R4,APOLREC          ADDR OF INDIRECT POOL TABLE ENTRY            
         MVC   BUCKIT(L'METHOD),METHOD                SET THE METHOD            
         MVC   BUCKIT+L'METHOD(L'INDSALTP),INDSALTP   AND SALARY TYPE           
         USING BIND,R5                                                          
         L     R5,CLIBUFF                                                       
         USING CLID,R7                                                          
         LA    R7,BINTABLE                                                      
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    COM1XIT                                                          
         XC    SVADDR,SVADDR                                                    
         XC    HALF,HALF                                                        
*                                                                               
         MVC   TOTLINE(80),=10PL8'0'                                            
         MVI   FORCEHED,C'Y'       NEW PAGE FOR EACH POOL                       
*                                                                               
INDAL04  CP    CDIRLST,ZEROS                                                    
         BNE   INDAL06                                                          
         CP    CDIRYTD,ZEROS                                                    
         BE    INDAL70             SKIP THE ZEROS                               
*                                                                               
INDAL06  ST    R7,ACLREC                                                        
         L     R6,ASRTWRK                                                       
         USING SRTD,R6                                                          
         GOTO1 CLERSRT,DMCB,(R6)   **1C - 14,15,16 BUCKET **                    
         MVC   SRTLGACC,CLILEDAC   1C LEDGER/ACCOUNT CODE                       
         MVC   SRTBTYP,BUCKIT      BUCKET TYPE (METHOD AND SALARY TYPE)         
         MVC   SRTCLEDG,OPTINDD                                                 
         CLI   LEVEL,COMP                                                       
         BNE   *+10                                                             
         MVC   SRTCLEDG,OPTINDC                                                 
         CLI   LEVEL,OFFICE                                                     
         BNE   *+10                                                             
         MVC   SRTCLEDG,OPTINDO                                                 
         CLI   INDACC,SPACE             INDIRECT ACCOUNT                        
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R1,INDACC                                                        
         TM    INDSTATS,INDNEWBI        IS THIS NEW BUSINESS INDIRECT?          
         BZ    INDAL08                                                          
         MVC   SRTCLEDG,OPTNBLDG        LEDGER FOR NEW BUSINESS SPREAD          
         LA    R1,OPTNBACC              ACCOUNT FOR N.B. SPREAD BACK            
         B     INDAL10                                                          
INDAL08  TM    INDSTATS,INDPROBO        IS THIS PRO BONO INDIRECT?              
         BZ    INDAL08A                                                         
         MVC   SRTCLEDG,OPTPBLDG        LEDGER FOR PRO BONO SPREAD              
         LA    R1,OPTPBACC              ACCOUNT FOR P.B. SPREAD BACK            
         B     INDAL10                                                          
INDAL08A TM    INDSTATS,INDHOUSE        IS THIS HOUSE INDIRECT?                 
         BZ    INDAL10                                                          
         MVC   SRTCLEDG,OPTHSLDG        LEDGER FOR HOUSE SPREAD                 
         LA    R1,OPTHSACC              ACCOUNT FOR HOUSE SPREAD BACK           
INDAL10  MVC   SRTCON(L'INDACC),0(R1)   ACCOUNT FOR INDIRECT TIME               
         SR    RE,RE                    KEEP TRACK OF CONTRA LENGTH             
         AH    RE,=Y(L'INDACC)                                                  
         SR    R1,R1                                                            
         IC    R1,CSTGRPLN              LENGTH OF HYBRID 1R CONTRA              
         AR    RE,R1                    KEEP TRACK OF CONTRA LENGTH             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTCON+L'INDACC(0),INOFDPFM                                      
         LA    R1,SRTCON+L'INDACC                                               
         SR    R0,R0                                                            
         IC    R0,CSTGRPLN                                                      
         AR    R1,R0                                                            
         MVC   0(L'INDORIG,R1),INDORIG                                          
         AH    RE,=Y(L'INDORIG)         KEEP TRACK OF CONTRA LENGTH             
         LA    R1,L'INDORIG(R1)                                                 
         MVC   0(L'INDCLIST,R1),INDCLIST      APPEND CLIST CODE                 
         AH    RE,=Y(L'INDCLIST)                                                
         CH    RE,=Y(L'SRTCON)                MUST FIT IN SRTCON                
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   NMEWRK,SPACES                                                    
         USING HYBND,R2                                                         
         LA    R2,NMEWRK                      GET HYBIRD CNTRA NME              
         MVC   HYCODE(L'INOFDPFM),INOFDPFM                                      
         GOTO1 BINSRC,DMCB,NMEWRK,HYNMBUF,NMEWRK                                
         MVC   SRTCNAME,HYNAME                CONTRA NAME                       
*                                                                               
         USING CMPUTD,R2                                                        
         L     R2,COMPRAT                                                       
         OC    INDSALTP,INDSALTP              NO SALARY TYPE                    
         BZ    INDAL20                                                          
INDAL15  CLC   CMBUKTYP,INDSALTP              COMPARE SAL TYPE                  
         BE    INDAL25                                                          
         LA    R2,CMPULN(R2)       BUMP TABLE                                   
         CLI   0(R2),EOT                                                        
         BNE   INDAL15                                                          
         DC    H'0'                                                             
*                                                                               
INDAL20  LH    R1,HALF          KEEP TRACK OF # OF ACCUMS PROCESSED             
         AHI   R1,1                                                             
         STH   R1,HALF                                                          
         OC    SVADDR,SVADDR                                                    
         BNZ   INDAL22                                                          
         ST    R2,SVADDR                                                        
         B     INDAL25                                                          
INDAL22  L     R2,SVADDR           POINT TO PREVIOUS ACCUM                      
         LA    R2,CMPULN(R2)       AND BUMP TO NEXT ONE                         
         ST    R2,SVADDR                                                        
*                                                                               
INDAL25  L     R1,APOLREC          ADDR OF INDIRECT POOL TABLE ENTRY            
         AH    R1,CMPTYIND                                                      
                                                                                
         ZAP   ALLOC,0(INBKLN,R1)                                               
         MVC   SRTBTYP+1,CMBUKTYP  FILL IN SAL TYPE(METH ALREADY THERE)         
*                                                                               
         GOTO1 MINUS1,DMCB,YTD1BUF            GO GET YTD-1                      
         MVI   BYTE,C'N'                      CLEAR ACTIVE SWITCH               
         BNE   *+8                            NEQ-NEVER POSTED BEFORE           
         MVI   BYTE,C'Y'                      SET ACTIVITY TO YES               
         ZAP   INDLST,DUB                     YTD-1 RETURNED IN DUB             
*                                                                               
         ZAP   CLITIME,CDIRYTD                                                  
         ZAP   TOTIME,DIRYTD                  AGYTIME                           
         GOTO1 DODIVD                         GET AMNT OF IND FOR CLT           
         ZAP   INDYTD,DUB                                                       
         ZAP   INDPST,INDYTD                                                    
         SP    INDPST,INDLST                  POSTING AMOUNT                    
         AP    INDTTLST,INDLST                TOTAL FOR YTD-1                   
         AP    INDTTYTD,INDYTD                TOTAL FOR YTD                     
         AP    INDTTPST,INDPST                TOTAL FOR POSTING                 
         ZAP   SRTCR,INDPST                   DOLLAR AMOUNT                     
         AP    PCREDITS,SRTCR                 ADD TO POSTING TOTAL              
         CP    SRTCR,ZEROS                    POSTING $00.00                    
         BNE   INDAL50                        NO - POST IT                      
         CLI   BYTE,C'Y'                      ANY ACTIVITY AT ALL?              
         BNE   INDAL60                        NO - SKIP PUT TO SORT             
INDAL50  L     R2,ASRTWRK                     YES - PUT REC TO SORT             
         GOTO1 ADSORTER,DMCB,(L'PUTC,PUTC),(R2)                                 
         TM    BUFFIT,INDIRIT                                                   
         BZ    INDAL60                                                          
         CLI   LEVEL,DEPT                     DEPT ONLY                         
         BNE   INDAL60                                                          
         TM    INDSTATS,INDNEWBI+INDPROBO+INDHOUSE+INDHOURS                     
         BNZ   INDAL60                                                          
         L     R1,ACLIWRK                     CLIENT WORK AREA                  
         MVC   0(CLEN,R1),0(R7)               COPY CLIENT REC TO WORK           
         ZAP   CCSTLST-CLID(L'CCSTLST,R1),INDLST                                
         ZAP   CCSTYTD-CLID(L'CCSTYTD,R1),INDYTD                                
         MVI   CLISTAT-CLID(R1),CLIIND        SET DPT IND INDICATOR             
         MVC   GROUP,INDACC                   DPT IND GROUP CODE                
         MVC   CSTGRP,INOFDPFM                DPT IND FROM ACCOUNT              
         ST    R1,ACLREC                                                        
         GOTO1 ABUFPOST                                                         
         ST    R7,ACLREC                      RESET TO TABLE ENTRY              
*                                                                               
INDAL60  BAS   RE,ABSORB                      MOVE INDIRECT TO ABSORBED         
INDAL61  LA    R2,2                           RCSUBPRG FOR COST                 
         TM    INDSTATS,INDINCOM              INCOME ALLOCATED POOL?            
         BZ    *+8                                                              
         LA    R2,14                          RCSUBPRG FOR INCOME               
         TM    INDSTATS,INDHOURS              HOURS ALLOCATED POOL?             
         BZ    *+8                                                              
         LA    R2,17                          RCSUBPRG FOR INCOME               
         TM    INDSTATS,INDPLUS               DIR+DPT IND POOL?                 
         BZ    *+8                                                              
         LA    R2,19                                                            
         CLI   QOPT7,C'B'          BREAKDOWN INDIR POOLS BY PTYPE?              
         BE    INDAL65                                                          
         LH    R1,HALF                                                          
         CHI   R1,3                HAVE WE PROCESSED ALL 3 PAYTYPES?            
         BL    INDAL04             NO CONTINUE BEFORE PRINTING                  
INDAL65  GOTO1 REPORT,DMCB,((R2),ACLREC),0    PRINT A CLIENT LINE               
         ZAP   INDTTLST,ZEROS                                                   
         ZAP   INDTTYTD,ZEROS                                                   
         ZAP   INDTTPST,ZEROS                                                   
         XC    SVADDR,SVADDR                                                    
         XC    HALF,HALF                                                        
*                                                                               
INDAL70  LA    R7,CLEN(R7)                                                      
         BCT   R3,INDAL04                     NEXT CLIENT RECORD                
*                                                                               
         OI    INDSTAT2,INDABSOR              MARK IND POOL AS ABSORBED         
         CLC   TOTLINE(80),=10PL8'0'                                            
         BE    COM1XIT                        NO ACTIVITY                       
         LA    R2,2                           RCSUBPRG FOR COST                 
         TM    INDSTATS,INDINCOM              INCOME ALLOCATED POOL?            
         BZ    *+8                                                              
         LA    R2,14                          RCSUBPRG FOR INCOME               
         TM    INDSTATS,INDHOURS              HOURS ALLOCATED POOL?             
         BZ    *+8                                                              
         LA    R2,17                          RCSUBPRG FOR HOURS                
         TM    INDSTATS,INDPLUS               DIR+DPT IND POOL?                 
         BZ    *+8                                                              
         LA    R2,19                                                            
         GOTO1 REPORT,DMCB,((R2),ACLREC),(C'T',0)                               
         B     COM1XIT                                                          
         DROP  R2,R4,R5,R6,R7                                                   
         EJECT                                                                  
*-----------------------------------------------------------                    
*              ROUTINE TO MOVE INDIRECT TO ABSORBED COLUMN                      
*-----------------------------------------------------------                    
*                                                                               
         USING SUMD,R6                                                          
         USING INDID,R4                                                         
ABSORB   NTR1                                                                   
         L     R4,APOLREC          ADDRESS OF CURRENT POOL                      
         CLI   MODE,LEVBLAST                                                    
         BNE   ABSORB3             END OF DEPT                                  
         USING DEPD,R3                                                          
         L     R3,ADEPCUM                                                       
         L     R6,ADEPTOT                                                       
         AP    SDINDLST,INDLST     KEEP TRACK OF ABSORBED                       
         AP    SDINDYTD,INDYTD                                                  
         AP    SDINDPST,INDPST                                                  
         SP    SDANDLST,INDLST     KEEP TRACK OF UNABSORBED REMAINING           
         SP    SDANDYTD,INDYTD                                                  
         SP    SDANDPST,INDPST                                                  
         AP    DDINDAL,INDLST                                                   
         AP    DDINDAY,INDYTD                                                   
         AP    DDINDAP,INDPST                                                   
         B     COM1XIT                                                          
*                                                                               
ABSORB3  CLI   MODE,LEVALAST                                                    
         BNE   ABSORB4                                                          
         L     R6,ADEPTOT                                                       
         ST    R6,AACCUM                                                        
         BAS   RE,ABCLEAR                                                       
         MVC   SOFFDEP,INOFDPFM   THE OFFICE /DEPT WHERE IT CAME FROM           
         AP    SOINDLST,INDLST     ABSORBED OFFICE INDIRECT                     
         AP    SOINDYTD,INDYTD                                                  
         AP    SOINDPST,INDPST                                                  
         GOTO1 BINADD,DMCB,(R6),SUMBUFF       ADD IT TO TABLE                   
         L     R6,AOFFTOT                                                       
         AP    SOINDLST,INDLST     ABSORBED OFFFICE INDIRECT                    
         AP    SOINDYTD,INDYTD                                                  
         AP    SOINDPST,INDPST                                                  
         L     R6,ADEPT99                                                       
         AP    SDINDLST,INDLST     PUT TO ABSORB AT DEP 99                      
         AP    SDINDYTD,INDYTD                                                  
         AP    SDINDPST,INDPST                                                  
         SP    SOINDLST,INDLST     OUT OF OFFICE AT DEP99                       
         SP    SOINDYTD,INDYTD                                                  
         SP    SOINDPST,INDPST                                                  
         BAS   RE,UNABSOR          UNABSORBED REMAINING                         
         USING OFFD,R3                                                          
         L     R3,AOFFCUM                                                       
         CLI   INDORIG,INDDEPT     ORIGINALLY DEPT POOL                         
         BE    ABSORB3C                                                         
         AP    OFINDAL,INDLST      KEEP TRACK OF ABSORBED                       
         AP    OFINDAY,INDYTD                                                   
         AP    OFINDAP,INDPST                                                   
         B     COM1XIT                                                          
ABSORB3C AP    OFINDADL,INDLST     DEPT ABSORBED AT OFFICE LEVEL                
         AP    OFINDADY,INDYTD                                                  
         AP    OFINDADP,INDPST                                                  
         B     COM1XIT                                                          
*                                                                               
ABSORB4  CLI   MODE,REQLAST                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ADEPTOT                                                       
         ST    R6,AACCUM                                                        
         BAS   RE,ABCLEAR                                                       
         MVC   SOFFDEP,INOFDPFM   THE OFFICE /DEPT WHERE IT CAME FROM           
         AP    SCINDLST,INDLST     ABSORBED CORP INDIRECT                       
         AP    SCINDYTD,INDYTD                                                  
         AP    SCINDPST,INDPST                                                  
         GOTO1 BINADD,DMCB,(R6),SUMBUFF       ADD DEPT INFO TO TABLE            
         LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB                     LENGTH OF LEVEL B                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FOXES                  DEPT CODE                         
         GOTO1 BINADD,DMCB,(R6),SUMBUFF       ADD OFFICE TO TABLE               
         MVC   SOFFDEP,FOXES                                                    
         GOTO1 BINADD,DMCB,(R6),SUMBUFF       ADD AGENCY TO TABLE               
         L     R6,AOFF99                                                        
         AP    SDINDLST,INDLST     PUT TO ABSORB AT OFF 99                      
         AP    SDINDYTD,INDYTD                                                  
         AP    SDINDPST,INDPST                                                  
         SP    SCINDLST,INDLST     OUT OF CORP AT 9999                          
         SP    SCINDYTD,INDYTD                                                  
         SP    SCINDPST,INDPST                                                  
         BAS   RE,UNABSOR          UNABSORBED REMAINING                         
         USING AGYD,R3                                                          
         L     R3,AAGYCUM                                                       
         AP    AGINDAL,INDLST      KEEP TRACK OF ABSORBED                       
         AP    AGINDAY,INDYTD                                                   
         AP    AGINDAP,INDPST                                                   
         B     COM1XIT                                                          
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
*--------------------------------------                                         
*        ADJUST UNABSORBED REMAINING                                            
*--------------------------------------                                         
*                                                                               
         USING INDID,R4                                                         
UNABSOR  L     R4,APOLREC                   ADDR OF CURRENT POOL                
         MVC   WORK(3),=C'N  '                                                  
         MVC   WORK+1(L'INDTYPE),INDTYPE    INDIRECT LEVEL                      
         USING SUMPD,RF                                                         
         L     RF,SUMPST                    TABLE OF SUMMARY ACCUMS             
UNAB02   CLI   SUMPKEY,X'FF'                END OF TABLE                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,SUMPKLN                   R1 LENGTH OF KEY COMPARE            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SUMPKEY              COMPARE TO TABLE ENTRY              
         BE    *+12                                                             
         LA    RF,SUMPLN(RF)                BUMP TO NEXT TABL ENTRY             
         B     UNAB02                                                           
         LR    R1,R6                                                            
         AH    R1,SUMPLST                   R1 POINTS TO YTD-1 ACCUM            
         SP    0(SBKLN,R1),INDLST                                               
         LR    R1,R6                                                            
         AH    R1,SUMPYTD                   R1 POINTS TO YTD ACCUM              
         SP    0(SBKLN,R1),INDYTD                                               
         LR    R1,R6                                                            
         AH    R1,SUMPPST                   R1 POINTS TO POST ACCUM             
         SP    0(SBKLN,R1),INDPST                                               
         BR    RE                                                               
         DROP  R4,RF                                                            
*                                                                               
*--------------------------------------                                         
*        CLEAR ACCUMS                                                           
*--------------------------------------                                         
*                                                                               
         USING SUMD,R6                                                          
ABCLEAR  L     R6,AACCUM            ADDR OF PERTINENT ACCUMS                    
         LA    R1,SBK                                                           
         LA    R0,SBKCNT                                                        
         ZAP   0(SBKLN,R1),ZEROS                                                
         LA    R1,SBKLN(R1)                                                     
         BCT   R0,*-10                                                          
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*------------------------------------------------------------                   
*              ROUTINE TO DISTRIBUTE OVERHEAD                                   
*                                                                               
*        NOTE: THERE ARE 2 TYPES OF OH. "ACCOUNT"                               
*              IS THE INPUT AMOUNT FOR THIS ACCOUNT.                            
*              UNABSORBED IS AMOUNT BUMPED FROM OTHERS.                         
*------------------------------------------------------------                   
*                                                                               
*                                                                               
VDISOVH  DS    0H                                                               
         TM    UPSI,POSTRATE       POST RATE RECORDS ONLY?                      
         BO    COM1XIT             YES - DON'T BOTHER WITH OVH POOLS            
         L     RE,OHDEFN           SET BUCKET TYPE                              
         USING OHDFD,RE                                                         
DISOH02  CLI   OHDFKEY,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LEVEL,OHDFKEY                                                    
         BE    *+12                                                             
         LA    RE,OHDFLN(RE)                                                    
         B     DISOH02                                                          
         MVC   BUCKIT(L'METHOD),METHOD                SET THE METHOD            
         MVC   BUCKIT+L'METHOD(L'OHBTYP),OHBTYP       OVERHEAD LEVEL            
         DROP  RE                                                               
         MVI   TYPE,ZERO           CLEAR POOL TYPE STATUS                       
         XC    APOLREC,APOLREC     CLEAR ADDR OF CURRENT POOL RECORD            
         OI    TYPE,OHTYPE                                                      
         CLI   OPTSHOWO,C'Y'       SHOW 14 DIRECT GROUP?                        
         BNE   *+8                                                              
         OI    TYPE,DIRGRUP                                                     
         BAS   RE,TOTDIR           GET TOTAL DIRECT TIME AT THIS LEVEL          
         CLI   MODE,ACCLAST        A SPECIFIC OVERHEAD ACCOUNT                  
         BNE   DISOH11             NO - GO RUN THRU POOLS                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   TOTLINE(80),=10PL8'0'                                            
         BAS   RE,GETOVH           GET THIS ACCOUNTS INPUT OH AMOUNT            
         TM    TYPE,OHTYPE                                                      
         BZ    DISOH99             NO OVERHEAD POOL FOR THIS ACCOUNT            
*                                                                               
*                                  RECALL THIS ACCOUNTS OH POOL.                
         L     R2,ALTAB            ALTAB -SET IN GETOVH                         
         GOTO1 BINSRC,DMCB,(R2),OVHBUFF,(R2)                                    
         L     R4,DMCB             THIS ACCOUNTS OVERHEAD ENTRY                 
         ST    R4,APOLREC          ADDR CURRENT OVERHEAD TABLE ENTRY            
         USING OVERD,R4                                                         
         CLC   OVACCT,ACCOUNT      MAKE SURE ITS SAME ACCOUNT                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,5                MSG NUMBER FOR HIGHER LEVEL SCHEME           
         CLC   OVHLEVL,LEVEL       OH  ALLOCATED AT CURRENT LEVEL?              
         BNE   DISOH94             NO - YOUR DONE FOR NOW                       
         TM    OVHSTATS,OVHINCOM   ALLOCATE BY INCOME                           
         BO    DISOH18             SKIP DIRECT TIME TEST                        
         CP    DIRYTD,ZEROS        TOTAL DIRECT YTD   º                         
         BNE   DISOH18                                                          
         LA    R2,2                NO DIRECT TIME MSG NUMBER                    
         MVI   OVHTYPE,OVHUNABS    SET POOL ENTRY TO UNABSORBED                 
         B     DISOH94             YOUR DONE FOR NOW                            
*                                                                               
*                                  RUN THRU THE OH POOL                         
*                                                                               
DISOH11  DS    0H                                                               
         CP    DIRYTD,ZEROS        TOTAL DIRECT YTD   º                         
         BE    DISOH99                                                          
         L     R5,OVHBUFF          OVERHEAD TABLE                               
         USING BIND,R5                                                          
         L     R6,BININ                                                         
         LTR   R6,R6                                                            
         BZ    DISOH99             NO OVERHEAD WHATSOEVER SO GET OUT            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    R4,BINTABLE                                                      
         USING OVERD,R4                                                         
*                                                                               
DISOH14  TM    OVHSTAT2,OVHABSOR   HAS THIS OVERHEAD BEEN ABSORBED?             
         BO    DISOH90             YES- SKIP IT                                 
         CLI   LEVEL,COMP          IF CORP POST ALL UNABSORBED OVERHEAD         
         BE    DISOH18                                                          
         CLI   LEVEL,DEPT          IF DEPT LEVEL                                
         BNE   DISOH16                                                          
         CLI   OVHLEVL,DEPT        MUST BE A DEPT POOL                          
         BNE   DISOH90                                                          
         SR    R1,R1                                                            
         IC    R1,LLEVB            POOL OF/DEP MUST = CURRENT OF/DEP            
         B     DISOH17                                                          
DISOH16  CLI   LEVEL,OFFICE        IF OFFICE LEVEL                              
         BNE   DISOH99                                                          
         CLI   OVHLEVL,OFFICE      MUST BE A OFFICE POOL OR LOWER               
         BH    DISOH90                                                          
         SR    R1,R1                                                            
         IC    R1,LLEVA            POOL OFFC MUST = CURRENT OFFC                
DISOH17  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OVACCT(0),ACCOUNT                                                
         BL    DISOH90             IF POOL LOWER GET,NEXT POOL ENTRY            
         BH    DISOH99             IF HIGHER, YOUR DONE FOR NOW                 
*                                                                               
DISOH18  ST    R4,APOLREC          ADDR CURRENT OVERHEAD TABLE ENTRY            
         MVC   SCHEMKEY,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,LLEVB            LENGTH OF LEVEL A+B                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCHEMKEY(0),OVACCT                                               
         XC    SCHMOPT,SCHMOPT                                                  
         XC    POOLTYPE,POOLTYPE                                                
         CLI   OVHTYPE,OVHINDP     IS THIS A IND PAY TYPE POOL                  
         BNE   *+12                NO -                                         
         OI    POOLTYPE,SCMINDS    YES - SET POOL TYPE TO INDICATE              
         B     *+8                                                              
         OI    POOLTYPE,SCMOVHE    MARK POOL AS NORMAL OVERHEAD                 
         L     RE,OHDEFN           THEN MARK IT AS DPT OFFC OR CORP             
         USING OHDFD,RE                                                         
DISOH18A CLI   OHDFKEY,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OVHORIG,OHDFKEY                                                  
         BE    *+12                                                             
         LA    RE,OHDFLN(RE)                                                    
         B     DISOH18A                                                         
         OC    POOLTYPE(1),OHBBIT                                               
         MVI   BFSTAT,0                                                         
         TM    OVHSTATS,OVHINCOM   INCOME ALLOCATED POOL?                       
         BZ    *+8                                                              
         MVI   INCOME,C'Y'                                                      
         TM    OVHSTATS,OVHHOURS   HOURS ALLOCATED POOL?                        
         BZ    *+8                                                              
         OI    BFSTAT,HRSSTAT                                                   
         TM    OVHSTATS,OVHPLUS    DIRECT + DPT IND ALLOCATED POOL?             
         BZ    *+8                                                              
         OI    BFSTAT,INSSTAT                                                   
         GOTO1 ASCHEME             BUILD CLIENT LIST THAT FITS SCHEME           
         MVI   INCOME,C'N'                                                      
         MVI   BFSTAT,0                                                         
*                                                                               
         CP    DIRYTD,ZEROS                                                     
         BNE   DISOH20             YES- THEIR IS DIRECT YTD                     
         MVI   OVHTYPE,OVHUNABS    SET POOL ENTRY TO UNABSORBED                 
*                                                                               
         CLI   MODE,ACCLAST        SPECIFIC OVH ACCOUNT?                        
         BNE   DISOH90             NO- BUMP TO NEXT OVH POOL ENTRY              
         LA    R2,2                YES- NO DIRECT TIME MGS NUMBER               
         B     DISOH94                                                          
*                                                                               
         USING BIND,R5                                                          
DISOH20  L     R5,CLIBUFF                                                       
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    DISOH90             NO CLIENTS IN TABLE FOR SCHEME               
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CLID,R7                                                          
         MVI   FORCEHED,C'Y'       NEW PAGE FOR EACH POOL                       
         MVC   TOTLINE(80),=10PL8'0'                                            
         LA    R7,BINTABLE                                                      
*                                  ALLOCATE UNABSORBED OVERHEAD                 
DISOH22  ST    R7,ACLREC                                                        
         ZAP   OVHPST,ZEROS                                                     
         ZAP   OVHYTD,ZEROS                                                     
         ZAP   CLITIME,CDIRYTD     CLIENT TIME YTD                              
         ZAP   ALLOC,OVAYTD        ACCOUNT OVERHEAD YTD                         
         ZAP   TOTIME,DIRYTD       AGYTIME YTD                                  
         GOTO1 DODIVD              GET AMOUNT OF O/H FOR THIS CLIENT            
         CLI   QOPT5,C'F'          FREEZE AS YTD-1 OVERHEAD                     
         BE    DISOH24                                                          
         ZAP   OVHYTD,DUB          CLIENT'S SHARE YTD                           
         BAS   RE,OHPOST           DO THE 1R 1C AND 1C 15 POSTINGS              
DISOH24  DS    0H                                                               
         LA    R2,1                RCSUBPRG FOR OVERHEAD BY COST                
         TM    OVHSTATS,OVHINCOM   INCOME ALLOCATED POOL?                       
         BZ    *+8                                                              
         LA    R2,15               RCSUBPRG FOR INCOME                          
         TM    OVHSTATS,OVHHOURS   HOURS ALLOCATED POOL?                        
         BZ    *+8                                                              
         LA    R2,18               RCSUBPRG FOR HOURS                           
         TM    OVHSTATS,OVHPLUS    DIR+DPT IND ALLOCATED POOL?                  
         BZ    *+8                                                              
         LA    R2,20               RCSUBPRG FOR DIR PLUS                        
         GOTO1 REPORT,DMCB,((R2),ACLREC),0   PRINT ALLOCATION TO CLI            
         BAS   RE,ABOVH                                                         
         LA    R7,CLEN(R7)                                                      
         BCT   R3,DISOH22           LOOPING THRU CLIENTS                        
*                                                                               
         OI    OVHSTAT2,OVHABSOR    MARK OVERHEAD ENTRY AS ABSORBED             
         LA    R2,1                RCSUBPRG FOR OVERHEAD BY COST                
         TM    OVHSTATS,OVHINCOM   INCOME ALLOCATED POOL?                       
         BZ    *+8                                                              
         LA    R2,15               RCSUBPRG FOR INCOME                          
         TM    OVHSTATS,OVHHOURS   HOURS ALLOCATED POOL?                        
         BZ    *+8                                                              
         LA    R2,18               RCSUBPRG FOR HOURS                           
         TM    OVHSTATS,OVHPLUS    DIR+DPT IND ALLOCATED POOL?                  
         BZ    *+8                                                              
         LA    R2,20               RCSUBPRG FOR DIR PLUS                        
         GOTO1 REPORT,DMCB,((R2),0),(C'T',0)    PRINT OUT TOTAL                 
DISOH90  CLI   MODE,ACCLAST        SPECIFIC OVH ACCOUNT?                        
         BE    DISOH99             YES - DONE FOR NOW                           
         LA    R4,OVLEN(R4)        NO  - NEXT OVERHEAD ENTRY                    
         BCT   R6,DISOH14                                                       
         B     DISOH99             POOL PROCESSING COMPLETE                     
*                                                                               
*                                  ERROR MSG'S FOR ACCLAST                      
*                                  R2 = 2 FOR NO DIRECT TIME                    
*                                  R2 = 5 FOR HIGHER LEVEL SCHEME               
DISOH94  L     R1,ASRTWRK          YES- PUT OUT NO DIRECT TIME MSG              
         MVC   0(SRTLEN,R1),SPACES                                              
         GOTO1 REPORT,DMCB,(1,0),((R2),0)                                       
         OI    TYPE,UNABSORB       SET POOL STATUS TO UNABSORBED                
         BAS   RE,ABOVH            PUT POOL AMOUNT IN UNABSORBED COLUMN         
*                                                                               
DISOH99  MVI   TYPE,ZERO           CLEAR POOL TYPE STATUS                       
         XC    APOLREC,APOLREC     CLEAR ADDR OF CURRENT POOL RECORD            
         B     COM1XIT                                                          
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------                     
*              ROUTINE TO GET OVERHEAD                                          
*              NOTE: YTD-1 BUCKET IS FILLED IN FROM PROCHIST                    
*----------------------------------------------------------                     
*                                                                               
         USING OVERD,R4                                                         
GETOVH   NTR1                                                                   
         TM    ACCSTUS,INDTACC     IND PAY TYPE OVERHEAD?                       
         BZ    *+16                                                             
         L     R4,AOVHWRK2         IF YES SET BIT AND SKIP THE LOOKUP           
         OI    OVHSTAT2,OVHTOTBL   INDICATE THAT ENTRY PUT TO TABLE             
         B     GETOVH08            YOU ALREADY BUILT THE RECORD                 
         L     RF,ACOBLOCK          A(COBLOCK) PROFILE BLOCK                    
         USING COBLOCKD,RF                                                      
         CLI   COVINC,C'N'         INCLUDE THIS OH IN ALLOCATION?               
         BE    GETOVH09                                                         
         DROP  RF                                                               
         L     R4,AOVHWRK                                                       
         L     R2,ADACC                                                         
         USING CACRECD,R2                                                       
         MVC   OVACCT,CACKACT      SAVE THE OFFICE DEPT ACCT                    
         MVC   OVFRMACT,CACKACT    OFF/DEPT BEING BUMPED FROM                   
         L     R3,AOVHAREA         ACSLRY WORK AREA                             
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),STEND,(R3),ADCOMFAC                     
*                                                                               
         USING SLRD,R3                                                          
         ZAP   OVAYTD,SLRSAL                                                    
         TM    SLRSALST,SLR2DP+SLR5DP  2 DEC PLACES OR 5DP MEANS A %            
         BZ    GETOVH01                                                         
         ZAP   DIVWRK,SLRSAL       THIS MONTHS OVERHEAD PCT                     
         MP    DIVWRK,DIRYTD       TIMES YTD DIRECT                             
         TM    SLRSALST,SLR2DP     2DP                                          
         BZ    GETOVHA                                                          
         DP    DIVWRK,=P'10000'                                                 
         ZAP   OVAYTD,DIVWRK(13)   GIVES YTD O'HEAD                             
         B     GETOVH01                                                         
GETOVHA  DP    DIVWRK,=P'10000000'                                              
         ZAP   OVAYTD,DIVWRK(11)   GIVES YTD O'HEAD                             
*                                                                               
GETOVH01 DS    0H                                                               
         MVC   OVHLEVL,LEVEL       SET CURRENT LEVEL AS DEFAULT                 
         MVI   OVHTYPE,OVHACCT     "ACCOUNT" TYPE OF OVERHEAD                   
         CLI   OPTOVA,C'P'         ALLOCATE BY COST PLUS DPT IND                
         BNE   *+12                                                             
         OI    OVHSTATS,OVHPLUS                                                 
         B     GETOVH02                                                         
         CLI   OPTOVA,C'H'         ALLOCATE BY HOURS                            
         BNE   *+12                                                             
         OI    OVHSTATS,OVHHOURS                                                
         B     GETOVH02                                                         
         CLI   OPTOVA,C'I'         ALLOCATE BY INCOME                           
         BNE   GETOVH02                                                         
         OI    OVHSTATS,OVHINCOM                                                
         CLI   OVHLEVL,DEPT                                                     
         BNE   *+8                                                              
         MVI   OVHLEVL,OFFICE      ALLOCATING BY INC OFFICE OR HIGHER           
GETOVH02 MVC   OVHORIG,OVHLEVL                                                  
         XC    SCHMOPT,SCHMOPT                                                  
         MVC   SCHEMKEY,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,LLEVB            LENGTH OF LEVEL A+B                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCHEMKEY(0),OVACCT                                               
         OI    SCHMOPT,NOCLEAR+LOOKUP       LOOKUP ONLY OPTION                  
         XC    POOLTYPE,POOLTYPE                                                
         GOTO1 ASCHEME                      GET ADDRESS OF VALID SCHEME         
*                                           ADDR RETURNED IN ASCHMREC           
         USING OHDFD,RE                                                         
         L     RE,OHDEFN                    OVERHEAD DEF TABLE                  
GETOVH06 CLI   OHDFKEY,EOT                  END OF TABLE                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OHDFKEY,OVHORIG              COMPARE TO TABLE ENTRY              
         BE    *+12                                                             
         LA    RE,OHDFLN(RE)                BUMP TO NEXT TABL ENTRY             
         B     GETOVH06                                                         
         L     R1,ASCHMREC                  ADDR OF SCHEME                      
         MVC   HALF,OHDFDIS                                                     
         AH    R1,HALF                      DISP TO ENTRY                       
         MVC   OVHLEVL(L'SCHOHD),0(R1)      REPLACE LEVEL TO DO ALLOC           
         DROP  RE                                                               
*                                                                               
GETOVH08 NI    OVHSTAT2,ALL-OVHABSOR  MAKE SURE ABSORBED FLAG IS OFF            
         LA    R1,OVBK             DON'T PUT ZERO POOL ENTRIES                  
         LA    R0,OVBUKCNT                                                      
         CP    0(OVBKLN,R1),ZEROS                                               
         BNZ   GETOVH10                                                         
         LA    R1,OVBKLN(R1)                                                    
         BCT   R0,*-14                                                          
GETOVH09 MVI   TYPE,ZERO                                                        
         B     COM1XIT                                                          
GETOVH10 DS    0H                                                               
         ST    R4,ALTAB           ADDR OF WORK AREA (AOVHWRK/AOVHWRK2)          
         GOTO1 BINADD,DMCB,(R4),OVHBUFF                                         
         B     COM1XIT                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* DO OVERHEAD POSTINGS                                               *          
**********************************************************************          
         SPACE 1                                                                
OHPOST   NTR1                                                                   
         L     R4,APOLREC          ADDR CURRENT OVERHEAD TABLE ENTRY            
         USING OVERD,R4                                                         
         L     R7,ACLREC           ADDR OF CLI SUMMARY RECORD                   
         USING CLID,R7                                                          
         L     R5,ASRTWRK          ADDR OF SORT REC                             
         USING SRTD,R5                                                          
*                                                                               
         GOTO1 CLERSRT,DMCB,(R5)                                                
         MVI   SRTLEDG,EMPLDGR     1R LEDGER                                    
         MVC   SRTACC,OVFRMACT     FROM OVERHEAD ACCOUNT                        
         MVC   SRTCLGAC,CLILEDAC   CONTRA LEDGER AND ACCOUNT CODE               
         MVC   SRTCNAME,CLICLNME   CONTRA NAME                                  
         MVC   SRTBTYP,BUCKIT                                                   
         GOTO1 MINUS1,DMCB,YTD2BUF GO GET YTD-1 (RETURNED IN DUB)               
         ZAP   OVHLST,DUB          YTD MINUS YTD-1 IS POSTING AMT               
         ZAP   OVHPST,OVHYTD                                                    
         SP    OVHPST,OVHLST       GET POSTING AMOUNT                           
         ZAP   SRTDR,OVHPST        DOLLAR AMOUNT                                
         AP    PDEBITS,SRTDR       ADD TO POSTING TOTAL                         
         CP    SRTDR,ZEROS         POSTING $00.00                               
         BE    OHP04               YES- DON'T PUT TO SORT                       
         GOTO1 ADSORTER,DMCB,(L'PUTC,PUTC),(R5)                                 
*                                                                               
OHP04    L     R5,ASRTWRK                                                       
         GOTO1 CLERSRT,DMCB,(R5)   **1C - 14,15,16 BUCKET **                    
         MVC   SRTLGACC,CLILEDAC   1C LEDGER/ACCOUNT CODE                       
         MVC   SRTBTYP,BUCKIT                                                   
         MVC   SRTCLEDG,OPT54                                                   
         CLI   LEVEL,COMP                                                       
         BNE   *+10                                                             
         MVC   SRTCLEDG,OPTC54                                                  
         CLI   LEVEL,OFFICE                                                     
         BNE   *+10                                                             
         MVC   SRTCLEDG,OPTOFH                                                  
         MVC   SRTCON,OVFRMACT                                                  
         USING HYBND,R6                                                         
         MVC   NMEWRK,SPACES                                                    
         LA    R6,NMEWRK                 GET HYBRID CONTRA NAME                 
         SR    R1,R1                                                            
         IC    R1,CSTGRPLN               HYBRID CONTRA LENGTH                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HYCODE(0),OVFRMACT        PORTION OF 1R FOR HYBRID CNTRA         
         GOTO1 BINSRC,DMCB,NMEWRK,HYNMBUF,NMEWRK                                
         MVC   SRTCNAME,HYNAME           CONTRA NAME                            
         GOTO1 MINUS1,DMCB,YTD1BUF       GO GET YTD-1 (RETURNED IN DUB)         
         MVI   BYTE,C'N'                 CLEAR ACTIVE SWITCH                    
         BNE   *+8                       NEQ-NEVER POSTED BEFORE                
         MVI   BYTE,C'Y'                 SET ACTIVITY TO YES                    
         ZAP   SRTCR,OVHYTD              YTD MINUS YTD-1 IS POSTING AMT         
         SP    SRTCR,DUB                 YTD-1 RETURNED IN DUB                  
         AP    PCREDITS,SRTCR            ADD TO POSTING TOTAL                   
         CP    SRTCR,ZEROS               IS POSTING AMNT $00.00?                
         BNE   *+12                      NO - PUT IT TO SORT                    
         CLI   BYTE,C'Y'                 ANY ACTIVITY AT ALL?                   
         BNE   COM1XIT                   NO -SKIP SORT                          
         GOTO1 ADSORTER,DMCB,(L'PUTC,PUTC),(R5)                                 
         B     COM1XIT                                                          
         DROP  R4,R5,R6,R7                                                      
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO PUT OVERHEAD TO ABSORBED OR UNABSORB COLUMN             *          
**********************************************************************          
         SPACE 1                                                                
         USING SUMD,R6                                                          
ABOVH    NTR1                                                                   
         USING OVERD,R4                                                         
         L     R4,APOLREC          OH POOL ENTRY                                
         CLI   MODE,ACCLAST        REGULAR OVERHEAD ACCOUNT?                    
         BNE   ABOVH10             NO - IT'S UNALLOCATED POOL                   
         L     R6,ADEPTOT                                                       
         TM    TYPE,UNABSORB        UNABSORBED?                                 
         BZ    ABOVH02              NO -ADD THIS CLTS AMT TO ABSORBED           
         AP    SOVOLST,OVALST       YES- ADD ENTIRE POOL AMT IN                 
         AP    SOVOYTD,OVAYTD                                                   
         ZAP   DUB,OVAYTD                                                       
         SP    DUB,OVALST                                                       
         AP    SOVOPST,DUB          UNABSORBED COLUMN                           
         CLI   LEVEL,COMP                                                       
         BE    ABOVH95              CAN'T BE UNABSORBED AT CORP LEVEL           
         B     COM1XIT                                                          
*                                                                               
ABOVH02  AP    SOVHLST,OVHLST      THIS CLIENTS SHARE                           
         AP    SOVHYTD,OVHYTD                                                   
         AP    SOVHPST,OVHPST      ADD TO ABSORBED                              
*                                                                               
         CLI   LEVEL,DEPT          DEPT OVERHEAD ACCOUNT                        
         BNE   ABOVH3                                                           
         USING DEPD,R3                                                          
         L     R3,ADEPCUM                                                       
         AP    DOVHAL,OVHLST       KEEP TRACK OF ABSORBED DEPT OH               
         AP    DOVHAY,OVHYTD                                                    
         AP    DOVHAP,OVHPST                                                    
         B     COM1XIT                                                          
*                                                                               
ABOVH3   CLI   LEVEL,OFFICE        OFFICE OVERHEAD ACCOUNT                      
         BE    ABOVH12                                                          
*                                                                               
ABOVH4   CLI   LEVEL,COMP          CORP OVERHEAD ACCOUNT                        
         BE    ABOVH20                                                          
         B     ABOVH95                                                          
*                                                                               
ABOVH10  CLI   MODE,LEVALAST                                                    
         BNE   ABOVH16             END OF AN OFFICE (UNALLOCATED POOLS)         
         L     R6,ADEPT99                                                       
         BAS   RE,ABOVH90                                                       
*                                                                               
ABOVH12  L     R3,AOFFCUM                                                       
         USING OFFD,R3                                                          
         AP    OFOVHAL,OVHLST      KEEP TRACK OF ABSORBED AT THIS LEVEL         
         AP    OFOVHAY,OVHYTD                                                   
         AP    OFOVHAP,OVHPST                                                   
         B     COM1XIT                                                          
*                                                                               
ABOVH16  CLI   MODE,REQLAST                                                     
         BNE   ABOVH95             END OF REQUEST (UNALLOCATED POOLS)           
         L     R6,AOFF99                                                        
         BAS   RE,ABOVH90          ADD ABSORBED /BACK OUT UNABSORBED            
*                                                                               
ABOVH20  L     R3,AAGYCUM                                                       
         USING AGYD,R3                                                          
         AP    AGOVHAL,OVHLST                                                   
         AP    AGOVHAY,OVHYTD                                                   
         AP    AGOVHAP,OVHPST                                                   
         B     COM1XIT                                                          
*                                                                               
*                                                                               
ABOVH90  AP    SOVHLST,OVHLST      OVERHEAD ABSORBED                            
         AP    SOVHYTD,OVHYTD                                                   
         AP    SOVHPST,OVHPST                                                   
         SP    SOVOLST,OVHLST      TAKE UNABSORBED OUT OF UNABSOR COL           
         SP    SOVOYTD,OVHYTD                                                   
         SP    SOVOPST,OVHPST                                                   
         BR    RE                                                               
*                                                                               
ABOVH95  CLI   QOPT1,C'L'                                                       
         BNE   *+8                                                              
         OI    RUNSTAT,POSTERR                                                  
         GOTO1 REPORT,DMCB,(1,0),(3,0)                                          
         USING ERRORD,R3                                                        
         MVC   WORK,SPACES                                                      
         L     R4,APOLREC          OH POOL ENTRY                                
         LA    R3,WORK                                                          
         MVI   ERRTYPE,ERRERROR    SET TO ERROR                                 
         MVI   ERRNUM,ERRUNABS                                                  
         MVC   ERRACCT(L'EMPLEDG),EMPLEDG                                       
         MVC   ERRACCT+L'EMPLEDG(L'OVACCT),OVACCT                               
*        MVC   ERRAMNT,OVAPST                                                   
         MVC   ERRAMNT,OVAYTD                                                   
         GOTO1 BINADD,DMCB,(R3),ERRORBUF ADD TO TABLE                           
         B     COM1XIT                                                          
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
**********************************************************************          
* GO GET YTD-1                                                       *          
* A(TABLE) PASSED IS EITHER                                          *          
*          YTD2BUF - 1R OVERHEAD POSTINGS                            *          
*          YTD1BUF - 1C POSTINGS (IN TSAR BUFFER)                    *          
**********************************************************************          
         SPACE 1                                                                
VMINUS1  DS    0H                                                               
         L     R2,0(R1)            A(TABLE)                                     
         L     R5,ASRTWRK          ADDR OF SORT RECORD                          
         USING SRTD,R5                                                          
         L     R4,AYTDWRK                                                       
         USING YTD1D,R4                                                         
         MVC   YTD1D(YTDLEN),SPACES                                             
         MVC   YTDACC,SRTACC       1C CLIENT OR 1R OVH ACCOUNT CODE             
         MVC   YTDCNTRA,SRTCLGAC   CONTRA LEDGER/ACCOUNT                        
         MVC   YTDBTYP,SRTBTYPE    BUCKET TYPE                                  
         ZAP   DUB,ZEROS                                                        
         LR    R5,R2                                                            
         C     R5,YTD1BUF          YTD1BUF IS IN HIGH STORAGE                   
         BE    MIN04                                                            
*                                                                               
         USING BIND,R5                                                          
         MVC   DMCB+8(BINKLEN),BININ                                            
         LA    R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(X'00',(R4)),(R3)                                   
         TM    0(R1),NOTFOUND                                                   
         BO    MIN9NO              NO YTD-1 FOR THIS COMBO ACT/CONTRA           
         L     R4,0(R1)                                                         
         ZAP   DUB,YTD1AMT         YTD-1 AMOUNT                                 
         OI    YTDSTAT,YTD1USED    MARK IT USED                                 
         B     MIN9YES                                                          
*                                                                               
MIN04    DS    0H                                                               
*        L     R1,TSARBLK1                                                      
*        USING TSARD,R1                                                         
*        ST    R4,TSAREC           KEY OF THE RECORD FOR LOOKUP                 
*        MVI   TSOFFACT,TSARDH     READHIGH                                     
*        GOTO1 ATSAROFF                                                         
*        BNE   MIN9NO              NO YTD-1 FOR THIS COMBO ACT/CONTRA           
*                                                                               
         LA    R0,BBUFFAGET                                                     
         L     RF,=A(YTDBUF)                                                    
         GOTO1 BUFFERIN,DMCB,((R0),(RF)),(0,(R4)),ADCOMFAC                      
         CLI   4(R1),0                                                          
         BNE   MIN9NO                                                           
*                                                                               
         ZAP   DUB,YTD1AMT         YTD-1 AMOUNT                                 
         OI    YTDSTAT,YTD1USED    MARK IT USED                                 
*                                                                               
*        USING TSARD,R1                                                         
*        L     R1,TSARBLK1                                                      
*        ST    R4,TSAREC           KEY OF THE RECORD FOR WRITE BACK             
*        MVI   TSOFFACT,TSAWRT     WRITE BACK MARKED RECORD                     
*        GOTO1 ATSAROFF                                                         
*        BE    MIN9YES             OK TO EXIT                                   
*        DC    H'0'                PROBLEM WRITING BACK TABL ENTRY              
*                                                                               
         LA    R0,BBUFFAPUT                                                     
         L     RF,=A(YTDBUF)                                                    
         GOTO1 BUFFERIN,DMCB,((R0),(RF)),(0,(R4)),ADCOMFAC                      
         CLI   4(R1),0                                                          
         BE    MIN9YES                                                          
         DC    H'0'                                                             
*                                                                               
MIN9YES  CR    RB,RB                EQUAL CONDITION ON EXIT                     
         B     COM1XIT                                                          
MIN9NO   LTR   RB,RB                NOT EQUAL CONDITION ON EXIT                 
         B     COM1XIT                                                          
         DROP  R4,R5                                                            
         EJECT                                                                  
**********************************************************************          
* LOOK UP PROFILES FOR A LEVEL OF ACCOUNT AND UPDATE OPTIONS         *          
* PARM1 A(RECORD)                                  --R2              *          
* PARM2 DISPLACEMENT INTO ACCOUNT FOR DESIRED LEVEL--R3              *          
* PARM3 LENGTH OF DESIRED LEVEL                    --R4              *          
* PARM4 Y(DISP INTO ACOBLOCK KEY)                  --R5              *          
* NOTE : ASSUMPTION HERE IS THAT YOU CAME TO PROFIT AT EACH          *          
*        REQFRST,LEVAFRST,LEVBFRST,LEVCFRST,PROCACC.                 *          
*        THIS MEANS THE HIGHER LEVELS OF THE COBLOCK KEY ARE         *          
*        ALREADY FILLED IN.                                          *          
**********************************************************************          
         SPACE 1                                                                
VPROFIT  DS    0H                                                               
         LM    R2,R5,0(R1)                                                      
         L     R6,ACOBLOCK                                                      
         USING COBLOCKD,R6                                                      
*                                  FILL IN COBLOCK KEY                          
         LTR   R2,R2               IF NO A(RECORD) PASSED IT IS A               
         BZ    PROFIT02            METHOD INITIALIZATION READ                   
*                                                                               
         L     RF,ACOBLOCK         ADDR COBLOCK                                 
         AR    RF,R5               DISP INTO COBLOCK TO DESIRED KEY LEV         
         LA    RE,3(R2)            RE POINTS PAST CUL IN RECORD PASSED          
         SR    R0,R0                                                            
         LTR   R3,R3               WAS A DISPLACEMENT PASSED?                   
         BZ    *+8                                                              
         IC    R0,0(R3)            LENGTH OF PREVIOUS LEVELS                    
         AR    RE,R0               RE NOW POINTS CURRENT LEVEL IN REC           
         SR    R1,R1                                                            
         IC    R1,0(R4)            LENGTH OF CURRENT LEVEL                      
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)                                                    
*                                                                               
PROFIT02 DS    0H                                                               
         GOTO1 GETCAP,DMCB,ACOBLOCK                                             
         OC    COSTATUS,COSTATUS                                                
         BZ    *+6                                                              
         DC    H'0'                DIE FOR NOW WITH ERRORS                      
*                                                                               
*        CLI   MODE,PROCACC        THESE ARE HIGH LEVEL OPTIONS                 
*        BE    COM1XIT             SO DONT BOTHER WITH PROCACC                  
         L     R3,STDOPTS          STANDARD OPTIONS TABLE                       
         USING STDOD,R3                                                         
PROFIT04 CLI   0(R3),EOT           END OF TABLE                                 
         BE    PROFIT08                                                         
         LA    RE,ACCAD                                                         
         AH    RE,STDSOPT          POINT TO OPTION IN WORKING STORAGE           
         L     RF,ACOBLOCK                                                      
         AH    RF,STDBLOCK         COBLOCK EQUIVALENT                           
         MVC   0(1,RE),0(RF)                                                    
PROFIT06 LA    R3,STDOLN(R3)                                                    
         B     PROFIT04                                                         
*                                                                               
PROFIT08 CLI   MODE,LEVBFRST       SET UP SCHEME RECORD FOR OFF/DEPT            
         BE    PROFIT10                                                         
         CLI   MODE,LEVAFRST                                                    
         BNE   COM1XIT                     SET DEFAULT SCHEME LENGTHS           
         MVC   ASCHDF1R,LLEVB              LENGTH OF OFFICE DEPT                
         MVC   ASCHDF1C,LENLEVA            1C COMPARE IS OFFICE LENGTH          
PROFIT10 BAS   RE,SCHEMER                                                       
         B     COM1XIT                                                          
         DROP  R3,R6                                                            
         EJECT                                                                  
**********************************************************************          
* LOOK UP ALLOCATION SCHEME /BUILD TABLE OF CLIENTS                  *          
**********************************************************************          
         SPACE 1                                                                
VASCHEME DS    0H                                                               
         USING BIND,R5                                                          
         XC    ASCHMREC,ASCHMREC   CLEAR ADDR OF SCHEME FOUND                   
         TM    SCHMOPT,NOCLEAR     DON'T CLEAR CLIENT TABLE?                    
         BO    *+14                                                             
         L     R5,CLIBUFF          CLEAR CLIENT TABLE                           
         XC    BININ,BININ                                                      
         L     R5,SCHEMBUF         ADDR OF SCHEME TABLE                         
         L     R0,BININ            NUMBER OF SCHEMES IN TABLE                   
         LTR   R0,R0                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         BZ    COM1XIT                                                          
         TM    SCHMOPT,LOOKUP      SCHEME LOOKUP ONLY?                          
         BO    ASCH01              YES - DON'T CLEAR                            
         ZAP   DIRLST,ZEROS        CLEAR DIRECT TOTAL ACCUMS                    
         ZAP   DIRYTD,ZEROS                                                     
*                                                                               
ASCH01   LA    R7,BINTABLE                                                      
         USING SCHMD,R7                                                         
ASCH02   SR    R1,R1                                                            
         IC    R1,SCHMALN          LENGTH OF COMPARE                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    R2,SCHMACCT         KEY OF SCHEME                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SCHEMKEY    KEY OF POOL MATCH KEY OF SCHEME              
         BE    ASCH04              IT MATCHES                                   
         LA    R7,SCHMLEN(R7)                                                   
         BCT   R0,ASCH02                                                        
*        DC    H'0'                SHOULD HAVE FOUND DEFAULT SCHEME             
         LA    R7,ASCHDFLT         DEFAULT SCHEME                               
*                                                                               
ASCH04   ST    R7,ASCHMREC         SAVE ADDR OF SCHEME ENTRY                    
         TM    SCHMOPT,LOOKUP      SCHEME LOOKUP ONLY?                          
         BO    ASCH99              YES - LEAVE WITH ASCHMREC ADDR SET           
*                                                                               
*                                                                               
*                                  TEST SCHEME STATUS / SET MASK FOR BC         
*                                                                               
         OC    POOLTYPE,POOLTYPE   HAS A POOL TYPE BEEN INDICATED?              
         BNZ   *+6                                                              
         DC    H'0'                IF NOT YOU CAN'T GO FARTHER                  
*                                                                               
         L     R3,SCHMTBL          TABLE OF SCHEME STATUS LOCATIONS             
         USING SCMD,R3                                                          
ASCH05   CLI   0(R3),EOT           YOU MUST FIND A MATCH                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   POOLTYPE,SCMPTYPE   MATCH POOL TYPE TO TABLE                     
         BE    *+12                                                             
         LA    R3,SCMLEN(R3)       NEXT TABLE ENTRY                             
         B     ASCH05                                                           
         L     RE,ASCHMREC         SAVED ADDR OF SCHEME ENTRY                   
         AH    RE,SCMST1                                                        
         MVC   STAT1,0(RE)         STATUS 1 FOR THIS POOL TYPE                  
         L     RE,ASCHMREC                                                      
         AH    RE,SCMST2                                                        
         MVC   STAT2,0(RE)         STATUS 2 FOR THIS POOL TYPE                  
         MVC   SCHEDESC,SCMDEF     DEFAULT SCHEME DEFINITION NUMBER             
         L     RE,ASCHMREC         TRY TO GET A CLOSER DEFINITION               
         AH    RE,SCMANAL          ANALYSIS CODE                                
         USING SCHDFD,RF                                                        
         L     RF,SCHDFLT                                                       
ASCH05A  CLI   SCHDFCOD,EOT                                                     
         BE    ASCH05C                                                          
         CLC   SCHDFCOD,0(RE)                                                   
         BE    *+12                                                             
         LA    RF,SCHDFLN(RF)                                                   
         B     ASCH05A                                                          
         MVC   SCHEDESC,SCHDFLIT   DEFAULT SCHEME DEFINITION NUMBER             
         DROP  RF                                                               
ASCH05C  OC    STAT2,STAT2                                                      
         BZ    ASCH06                                                           
         MVI   SCHEDESC,SCHDOCLT   THIS OFFICE'S CLIENTS                        
         TM    STAT1,SCHMOFLT                                                   
         BZ    ASCH06                                                           
         MVI   SCHEDESC,SCHDOGP    SET TO OFF GROUP IF YOU HAVE A LIST          
         TM    STAT1,SCHMINCL+SCHMRECK                                          
         BNO   ASCH05D                                                          
         MVI   SCHEDESC,SCHDOGP2   SET TO WORKED ON BY GRUP/BELONGS TO          
         B     ASCH06                                                           
ASCH05D  TM    STAT1,SCHMRECK                                                   
         BNZ   ASCH06                                                           
         MVI   SCHEDESC,SCHDOVER   OFFICE /OFF LIST OVERRIDE                    
*                                                                               
*                                  *** INCOME ALLOCATION ***                    
ASCH06   CLI   INCOME,C'Y'         IF ALLOC BY INCOME MATCH OFFICE              
         BNE   ASCH14                                                           
*                                                                               
         NI    TYPE,ALL-UNABSORB   TURN OFF UNABSORBED SWITCH                   
         TM    POOLTYPE,SCMINDI    INDIRECT POOL                                
         BZ    ASCH10                                                           
         MVI   STAT1,SCHMINCL      INCLUDE AT MATCHES                           
         MVI   STAT2,0             WITH NO RECHECK                              
         MVI   SCHEDESC,SCHDINCO   BY OFFICE INCOME                             
         TM    POOLTYPE,SCMOFC                                                  
         BO    ASCH14                                                           
ASCH08   MVI   STAT1,SCHMALL       ALL FOR CORP LEVEL INCOME POOLS              
         MVI   STAT2,0             WITH NO RECHECK                              
         MVI   SCHEDESC,SCHDINCC   BY CORP INCOME                               
         B     ASCH14                                                           
*                                                                               
ASCH10   TM    POOLTYPE,SCMOVHE    OVERHEAD POOL                                
         BO    *+6                                                              
         DC    H'0'                                                             
         L     R4,APOLREC                                                       
         USING OVERD,R4                                                         
         CLI   OVHLEVL,OFFICE                                                   
         BNE   ASCH14                                                           
         NI    STAT1,ALL-SCHMALL   TURN OFF ALL CLIENTS                         
         OI    STAT1,SCHMINCL      INCLUDE OFFICE MATCHES                       
*                                                                               
*                                                                               
ASCH14   MVI   MASK,EQUAL          DEFAULT IS BRANCH EQUAL (MASK)               
         TM    STAT1,SCHMEXCL                                                   
         BZ    *+12                                                             
         MVI   MASK,NOTEQUAL       SET TO NOT EQUAL                             
         B     ASCH15                                                           
         TM    STAT1,SCHMALL       ALL CLIENTS?                                 
         BZ    ASCH15                                                           
         MVI   MASK,BRANCH         SET TO UNCONDITIONAL BRANCH                  
*                                                                               
ASCH15   MVI   MASK2,EQUAL         DEFAULT IS BRANCH EQUAL (MASK2)              
         TM    STAT2,SCHMEXCL                                                   
         BZ    *+12                                                             
         MVI   MASK2,NOTEQUAL       SET TO NOT EQUAL                            
         B     ASCH16                                                           
         TM    STAT2,SCHMALL        ALL CLIENTS?                                
         BZ    ASCH16                                                           
         MVI   MASK2,BRANCH         SET TO UNCONDITIONAL BRANCH                 
*                                                                               
ASCH16   L     R6,ACLIWRK          CLIENT WORK AREA                             
         USING CLID,R6                                                          
         L     R5,ABUFWRK                                                       
         USING BUFD,R5                                                          
         XC    BUFKEY(BUFKLEN),BUFKEY  SET KEY FOR READ HIGH                    
         MVC   BUFCLI,SPACES                                                    
         ZAP   BDIRLST,ZEROS                                                    
         ZAP   BDIRYTD,ZEROS                                                    
         LA    RF,HIGHC                                                         
         B     *+8                                                              
ASCH18   LA    RF,SEQC                                                          
         SR    R0,R0                                                            
         CLI   INCOME,C'Y'         IF ALLOC BY INCOME ALWAYS LEVEL 3            
         BNE   ASCH20                                                           
         LA    R0,COMP                                                          
         LA    R2,BUFINCM                                                       
         B     ASCH24A                                                          
ASCH20   IC    R0,LEVEL                                                         
         LA    R2,BUFNOGP          DEFAULT IS NOT TO SHOW DIRECT GROUP          
         TM    BFSTAT,HRSSTAT      HOURS?                                       
         BNO   *+8                                                              
         LA    R2,BUFNOGPH                                                      
         TM    BFSTAT,INSSTAT      DIRECT + DPT INDIRECT?                       
         BNO   *+8                                                              
         LA    R2,BUFNOGPI                                                      
         TM    TYPE,DIRGRUP        PRINT DIRECT GROUP?                          
         BO    ASCH24              YES -                                        
         TM    STAT1,SCHMRECK      RECHECK SCHEME TO DIRECT TIME GROUP?         
         BZ    ASCH24A             NO - NO NEED FOR DIRECT GROUP                
ASCH24   LA    R2,BUFBYGP                                                       
         TM    BFSTAT,HRSSTAT      HOURS?                                       
         BNO   *+8                                                              
         LA    R2,BUFBYGPH                                                      
         TM    BFSTAT,INSSTAT      DIRECT + DPT INDIRECT?                       
         BNO   *+8                                                              
         LA    R2,BUFBYGPI                                                      
ASCH24A  STC   R2,BUFTYPE                                                       
         GOTO1 BUFFALO,DMCB,(L'SEQC,(RF)),((R2),ADBUFC),(R5),(R0)               
         TM    8(R1),EOF                                                        
         BO    ASCH90              EOF                                          
         BAS   RE,SCHMCK           GO COMPARE BUFFALO CLIENT TO SCHEME          
         BNE   ASCH18              NOT EQUAL CC MEANS CLIENT DID NOT            
*                                  PASS THE TEST FOR THE SCHEME                 
*                                                                               
         GOTO1 CLERWRK             IF CLIENT FITS SCHEME ADD TO CLIBUFF         
         MVC   CLILEDAC,BUFCLI                                                  
         TM    TYPE,DIRGRUP        DO YOU WANT TO PRINT DIRECT GROUP?           
         BNO   *+10                                                             
         MVC   CLGRPING,BUFGRPCD   CLIENTS COST GROUP CODE                      
         ZAP   CDIRLST,BDIRLST     DIRECT TIME TO CLIENT TABLE                  
         ZAP   CDIRYTD,BDIRYTD                                                  
         TM    POOLTYPE,SCMOVHE    OVERHEAD POOL                                
         BZ    ASCH30                                                           
         CLI   OPTOVHY,C'Y'         IS OVERHEAD YTD                             
         BNE   ASCH30                                                           
         AP    CDIRLST,BDIRLSTM    IF IT IS ADD MEMO DIRECT YTD-1               
         AP    CDIRYTD,BDIRLSTM                                                 
ASCH30   CP    CDIRLST,ZEROS                                                    
         BNE   ASCH32                                                           
         CP    CDIRYTD,ZEROS                                                    
         BE    ASCH18                                                           
ASCH32   AP    DIRLST,CDIRLST                                                   
         AP    DIRYTD,CDIRYTD                                                   
         USING BCLID,R2                                                         
         LA    R2,NMEWRK                                                        
         MVC   NMEWRK,SPACES                                                    
         MVC   BCLICDE,CLICDE      LOOKUP CLIENT NAME                           
         GOTO1 BINSRC,DMCB,NMEWRK,NMEBUFF,NMEWRK                                
         MVC   CLICLNME,BCLINME                                                 
         ST    R6,ACLREC           SAVE ADDR OF RECORD                          
         GOTO1 BINADD,DMCB,ACLREC,CLIBUFF                                       
         B     ASCH18                                                           
ASCH90   CLI   LEVEL,COMP          IF CORP LEVEL AND NO TIME                    
         BNE   ASCH99              FOR SCHEME, PUT OUT ERROR                    
         CP    DIRLST,ZEROS                                                     
         BNE   ASCH99                                                           
         CP    DIRYTD,ZEROS                                                     
         BNE   ASCH99                                                           
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE HEADING                                
         L     R1,ASRTWRK                                                       
         MVC   0(SRTLEN,R1),SPACES                                              
         GOTO1 REPORT,DMCB,(2,0),(4,0)                                          
         CLI   INCOME,C'Y'         INCOME POOL?                                 
         BNE   ASCH91              NO -PUT OUT ERROR NOW                        
         TM    TYPE,UNABSORB       HAVE YOU TRIED TO SPREAD TO CORP             
         BO    *+12                YES- PUT OUT ERROR                           
         OI    TYPE,UNABSORB       SET POOL STATUS TO UNABSORBED                
         B     ASCH08              TRY TO BUMP TO CORP INCOME                   
         NI    TYPE,ALL-UNABSORB                                                
ASCH91   L     R4,APOLREC                                                       
         USING ERRORD,R3                                                        
         MVC   WORK,SPACES                                                      
         LA    R3,WORK                                                          
         MVI   ERRTYPE,ERRERROR  SET TO ERROR                                   
         MVI   ERRNUM,ERRNODIR                                                  
         MVC   ERRACCT(L'EMPLEDG),EMPLEDG                                       
         TM    TYPE,OHTYPE                                                      
         BZ    ASCH92                                                           
         CP    OVAYTD,ZEROS        DONT BOTHER IF ZERO POOL                     
         BE    ASCH99                                                           
         USING OVERD,R4                                                         
         MVC   ERRACCT+L'EMPLEDG(L'OVACCT),OVACCT                               
         MVC   ERRAMNT,OVAYTD                                                   
         B     ASCH94                                                           
         USING INDID,R4                                                         
ASCH92   MVC   ERRACCT+L'EMPLEDG(L'INOFDPFM),INOFDPFM                           
         CLI   QOPT7,C'B'          PRINTING TOTALS FOR ALL PAYTYPES?            
         BE    ASCH92A                                                          
         MVC   ERRAMNT,INDTOTAC    TOTAL FOR ALL 3 PAYTYPES                     
         B     ASCH94                                                           
ASCH92A  LA    R1,INDSALAC         SALARY AMOUNT                                
         LA    R0,3                3 PAYTYPES                                   
         OC    0(INBKLN,R1),0(R1)  ANY AMOUNT?                                  
         BNZ   ASCH93                                                           
         LA    R1,INBKLN(R1)                                                    
         BCT   R0,*-14                                                          
         B     ASCH94                                                           
ASCH93   MVC   ERRAMNT,0(R1)                                                    
*        MVC   ERRAMNT,INDDYTD                                                  
ASCH94   DS    0H                                                               
         GOTO1 BINADD,DMCB,(R3),ERRORBUF ADD TO TABLE                           
         CLI   QOPT1,C'L'                                                       
         BNE   *+8                                                              
         OI    RUNSTAT,POSTERR                                                  
ASCH99   B     COM1XIT                                                          
*                                                                               
ASCHDFLT DC    CL12' '             DEFAULT SCHEME                               
ASCHDF1R DC    AL1(0)              LENGTH FOR POOL KEY COMPARE                  
ASCHDF1C DC    AL1(0)              LENGTH FOR 1C COMPARE                        
         DC    CL1'D'              AN=D                                         
         DC    AL1(SCHMALL)        INCLUDE ALL MATCHES                          
         DC    XL1'00'             SCHEME STATUS 2 (RECHECK STATUS)             
         DC    CL1'O'              AN=O                                         
         DC    AL1(SCHMALL)        INCLUDE ALL MATCHES                          
         DC    XL1'00'             SCHEME STATUS 2 (RECHECK STATUS)             
         DC    CL1'C'              AN=C                                         
         DC    AL1(SCHMALL)        INCLUDE ALL MATCHES                          
         DC    XL1'00'             SCHEME STATUS 2 (RECHECK STATUS)             
*                                  **   O V E R H E A D **                      
         DC    AL1(1)              WHERE ALLOCATE DEPT LEVEL OH                 
         DC    AL1(2)              WHERE ALLOCATE OFFICE LEVEL OH               
         DC    AL1(3)              WHERE ALLOCATE CORP LEVEL OH                 
         DC    AL1(SCHMALL)        INCLUDE ALL MATCHES                          
         DC    XL1'00'             SCHEME STATUS 2 (RECHECK STATUS)             
         DC    AL1(SCHMALL)        INCLUDE ALL MATCHES                          
         DC    XL1'00'             SCHEME STATUS 2 (RECHECK STATUS)             
         DC    AL1(SCHMALL)        INCLUDE ALL MATCHES                          
         DC    XL1'00'             SCHEME STATUS 2 (RECHECK STATUS)             
         DC    AL1(2)              WHERE ALLOCATE IND PAY TYPE                  
         DC    AL1(SCHMALL)        INCLUDE ALL MATCHES                          
         DC    XL1'00'             SCHEME STATUS 2 (RECHECK STATUS)             
         DC    XL4'00'             ADDR OF OFFLIST FOR OH/IND TIME              
         DC    XL4'00'             ADDR OF OFFLIST FOR IND PAY TYPE             
ASCHDLN  EQU   *-ASCHDFLT                                                       
         DROP  R2,R3,R4,R5,R6,R7                                                
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO POST DIRECT TIME TO BUFFALO RECORDS                     *          
**********************************************************************          
         SPACE 1                                                                
         USING BUFD,R5                                                          
         USING CLID,R7                                                          
         USING PTOTD,R6                                                         
VBUFPOST DS    0H                                                               
         L     R7,ACLREC                                                        
         CLI   CLILEDG,C'C'                                                     
         BNE   COM1XIT                  DON'T POST NON-CLIENT TIME              
         L     R6,APERWRK                                                       
         TM    PERSTAT,PEREXEC          EMPLOYEE AN EXECUTIVE?                  
         BO    COM1XIT                  YES - DON'T PUT HIS TIME                
BUFP2    L     R5,ABUFWRK                                                       
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         LA    R1,BUFBK                 CLEAR BUFFALO BUCKETS                   
         LA    R0,BBKCNT                                                        
         ZAP   0(BUFBKLN,R1),ZEROS                                              
         LA    R1,BUFBKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         TM    CLISTAT,CLIIND           IS THIS A DPT INDIRECT ENTRY?           
         BNZ   BUFP6                                                            
*                                                                               
         MVC   BUFCLI,CLILEDG           CLIENT CODE                             
         MVC   BUFGLDG,OPTDIRLG         DIRECT LEDGER                           
         TM    CLISTAT,CLIIND           IS THIS A DPT INDIRECT ENTRY?           
         BZ    *+10                                                             
         MVC   BUFGLDG,OPTINDD          INDIRECT LEDGER                         
         MVC   BUFGRP,GROUP             1 DIGIT ACCOUNT                         
         MVC   BUFGCOD,CSTGRP           GROUP CODE                              
         MVI   BUFTYPE,BUFBYGP          CLIENT BY GROUP                         
         TM    CLISTAT,CLINB+CLIPB+CLIHS                                        
         BZ    *+8                                                              
         MVI   BUFTYPE,BUFNEWB          NEW BUSINESS CLIENT TYPE 3              
         CLI   POSTSW,C'M'                                                      
         MVI   POSTSW,SPACE                                                     
         BNE   BUFP3                                                            
         ZAP   BDIRLSTM,DUB             YTD-1 AS MEMO                           
         B     BUFP4                                                            
BUFP3    ZAP   BDIRLST,CCSTLST          YTD-1                                   
         ZAP   BDIRYTD,CCSTYTD          YTD                                     
*                                                                               
BUFP4    DS    0H                       CLIENT BY GROUP                         
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFC,(R5)                           
         XC    BUFCLI,BUFCLI            TOTAL RECORD BY GROUP                   
         XC    BUFGRPCD,BUFGRPCD                                                
         BASR  RE,RF                                                            
*                                                                               
         TM    CLISTAT,CLINB+CLIPB+CLIHS  IF NB OR PB OR HOUSE DONE             
         BNZ   COM1XIT                                                          
         MVC   BUFCLI,CLILEDG                                                   
         XC    BUFGRPCD,BUFGRPCD                                                
         MVI   BUFTYPE,BUFNOGP         CLIENT NO GROUP                          
         BASR  RE,RF                                                            
         XC    BUFCLI,BUFCLI           TOTAL FOR NO GROUP                       
         BASR  RE,RF                                                            
*                                                                               
         TM    BUFFIT,HOURIT                                                    
         BZ    BUFP6                                                            
         LA    R3,BUFBK                 CLEAR BUFFALO BUCKETS                   
         LA    R0,BBKCNT                                                        
         ZAP   0(BUFBKLN,R3),ZEROS                                              
         LA    R3,BUFBKLN(R3)                                                   
         BCT   R0,*-10                                                          
         MVC   BUFCLI,CLILEDG           CLIENT CODE                             
         MVC   BUFGRP,GROUP             1 DIGIT ACCOUNT                         
         MVC   BUFGCOD,CSTGRP           GROUP CODE                              
         MVI   BUFTYPE,BUFBYGPH         CLIENT BY GROUP HOURS                   
         ZAP   BHRSLST,CYTDHRS          YTD-1 HOURS                             
         SP    BHRSLST,CPERHRS                                                  
         ZAP   BHRSYTD,CYTDHRS          YTD   HOURS                             
         BASR  RE,RF                                                            
         XC    BUFCLI,BUFCLI            TOTAL RECORD BY GROUP HOURS             
         XC    BUFGRPCD,BUFGRPCD                                                
         BASR  RE,RF                                                            
         MVC   BUFCLI,CLILEDG                                                   
         XC    BUFGRPCD,BUFGRPCD                                                
         MVI   BUFTYPE,BUFNOGPH        CLIENT NO GROUP                          
         BASR  RE,RF                                                            
         XC    BUFCLI,BUFCLI           TOTAL FOR NO GROUP                       
         BASR  RE,RF                                                            
*                                                                               
BUFP6    TM    BUFFIT,INDIRIT           ADD DEPT IND TO DIRECT?                 
         BZ    COM1XIT                                                          
*                                                                               
         MVC   BUFCLI,CLILEDG           CLIENT CODE                             
         MVC   BUFGLDG,OPTDIRLG         DIRECT LEDGER                           
         TM    CLISTAT,CLIIND           IS THIS A DPT INDIRECT ENTRY?           
         BZ    *+10                                                             
         MVC   BUFGLDG,OPTINDD          INDIRECT LEDGER                         
         MVC   BUFGRP,GROUP             1 DIGIT ACCOUNT                         
         MVC   BUFGCOD,CSTGRP           GROUP CODE                              
         MVI   BUFTYPE,BUFBYGPI         CLIENT BY GROUP(DIRECT + IND)           
         TM    CLISTAT,CLIIND           IS THIS A DPT INDIRECT ENTRY?           
         BZ    BUFP8                                                            
         ZAP   BDIRLSTM,ZEROS           CLEAR                                   
         CLI   POSTSW,C'M'                                                      
         MVI   POSTSW,SPACE                                                     
         BNE   BUFP8                                                            
         ZAP   BDIRLSTM,DUB             YTD-1 AS MEMO                           
         B     BUFP10                                                           
BUFP8    ZAP   BDIRLST,CCSTLST          YTD-1                                   
         ZAP   BDIRYTD,CCSTYTD          YTD                                     
*                                                                               
BUFP10   DS    0H                       CLIENT BY GROUP                         
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFC,(R5)                           
         XC    BUFCLI,BUFCLI            TOTAL RECORD BY GROUP                   
         XC    BUFGRPCD,BUFGRPCD                                                
         BASR  RE,RF                                                            
*                                                                               
         MVC   BUFCLI,CLILEDG                                                   
         XC    BUFGRPCD,BUFGRPCD                                                
         MVI   BUFTYPE,BUFNOGPI        CLIENT NO GROUP(DIRECT +IND)             
         BASR  RE,RF                                                            
         XC    BUFCLI,BUFCLI           TOTAL FOR NO GROUP                       
         BASR  RE,RF                                                            
*                                                                               
         B     COM1XIT                                                          
         DROP  R5,R6,R7                                                         
         EJECT                                                                  
***********************************************************************         
* MAKE EMPLOYEE POSTINGS TO SORTER                                    *         
*      R7 - POINTS TO CLIREC                                          *         
***********************************************************************         
         SPACE 1                                                                
VEMPLOY  DS   0H                                                                
         TM    UPSI,POSTRATE       POST RATE RECORDS ONLY?                      
         BO    EMPLX               YES - SKIP EMPLOYEE TO SORT                  
         L     R7,ACLREC           ADDR OF CURRENT CLIENT RECORD                
         USING CLID,R7                                                          
         USING CMPUTD,R4                                                        
         L     R4,COMPRAT          TBL OF DISP TO SAL TYP ACCUMS                
*                                                                               
EMPL10   OC    CMSLRD,CMSLRD       IF Y(0) SKIP THIS SAL TYPE                   
         BZ    EMPL20                                                           
         L     RF,ACLREC           ADDR OF CLIENT RECORD                        
         AH    RF,CMPCLIDL         DISP TO YTD-1 ACCUM FOR SALARY TYPE          
         L     R1,ACLREC                                                        
         AH    R1,CMPCLIDY         DISP TO YTD ACCUM FOR SALARY TYPE            
         ZAP   ALLOCYTD,0(CBUKLEN,R1)  YTD AMOUNT                               
         ZAP   ALLOC,ALLOCYTD      YTD MINUS (YTD-1) = POSTING AMOUNT           
         SP    ALLOC,0(CBUKLEN,RF)                                              
         CP    ALLOC,ZEROS         IF THERE IS NO POSTING AMT                   
         BNE   EMPL30                                                           
         CP    ALLOCYTD,ZEROS      AND THERE NEVER WAS                          
         BNE   EMPL30              SKIP IT                                      
EMPL20   LA    R4,CMPULN(R4)       NEXT SAL TYPE IN TABLE                       
         CLI   0(R4),EOT           END OF TABLE                                 
         BNE   EMPL10                                                           
         B     EMPLX               DONE WITH THIS CLIENT/NON CLIENT             
*                                                                               
EMPL30   MVC   BUCKIT(L'METHOD),METHOD                SET THE METHOD            
         MVC   BUCKIT+L'METHOD(L'CMBUKTYP),CMBUKTYP   AND SALARY TYPE           
         L     R5,ASRTWRK                                                       
         USING SRTD,R5                                                          
         GOTO1 CLERSRT,DMCB,(R5)   **1R - 1C OR 1N BUCKET **                    
         L     RF,ADACC                                                         
         USING ACTRECD,RF                                                       
         MVC   SRTKACC,ACTKCULA    1R ACCOUNT CODE                              
         MVC   SRTCLGAC,CLILEDAC   CONTRA LEDGER AND ACCOUNT CODE               
         MVC   SRTCNAME,CLICLNME   CONTRA NAME                                  
         MVC   SRTBTYP,BUCKIT      BUCKET TYPE (METHOD AND SALARY TYPE)         
         ZAP   SRTDR,ALLOC         DOLLAR AMOUNT                                
         MVC   MSG,=CL10'SRT REC 1'                                             
         GOTO1 ADUMP,DMCB,(RC),(R5),SRTLEN                                      
         CP    SRTDR,ZEROS         IS IT $00.00                                 
         BE    EMPL40              DON'T PUT TO SORT                            
         AP    PDEBITS,SRTDR       ADD TO POSTING TOTAL                         
         GOTO1 ADSORTER,DMCB,(L'PUTC,PUTC),(R5)                                 
         DROP  RF                                                               
*                                                                               
EMPL40   CLI   CLILEDG,C'C'                                                     
         BNE   EMPL20              IF 1N DO NEXT SALARY TYPE                    
*                                                                               
         GOTO1 CLERSRT,DMCB,(R5)   **1C  DIRECT BUCKET **                       
         MVC   SRTLGACC,CLILEDAC   LEDGER (C) AND ACCOUNT CODE                  
         MVC   SRTCLEDG,OPTDIRLG   CONTRA LEDGER FOR DIRECT TIME                
         MVC   SRTCON(L'GROUP),GROUP                                            
         SR    R1,R1                                                            
         IC    R1,CSTGRPLN                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+8              CONTRA HYBRID CODE                           
         B     *+10                                                             
         MVC   SRTCON+L'GROUP(0),CSTGRP                                         
         MVC   SRTCNAME,CSTGRPNM   CONTRA NAME                                  
         MVC   SRTBTYP,BUCKIT      BUCKET TYPE (METHOD AND SALARY TYPE)         
         ZAP   SRTCR,ALLOC         1R/ 1C OR 1N POSTED AMT                      
*                                  *DEAL WITH CHANGING 14 POINTER*              
         GOTO1 MINUS1,DMCB,YTD1BUF GO MARK YTD-1 USED (RETURNED IN DUB)         
         CP    DUB,ZEROS           DID WE EVER POST TO THIS 1C 14 COMBO         
         BNZ   *+10                YES - THEN ALL IS WELL                       
         ZAP   SRTCR,ALLOCYTD      NO - THEN POST ENTIRE YTD AMNT               
         AP    PCREDITS,SRTCR      ADD TO POSTING TOTAL                         
         GOTO1 ADSORTER,DMCB,(L'PUTC,PUTC),(R5)                                 
*                                                                               
         CP    SRTCR,ALLOCYTD                                                   
         BNE   EMPL45                                                           
         MVC   MSG,=CL10'SRT REC 2'                                             
         GOTO1 ADUMP,DMCB,(RC),(R5),SRTLEN                                      
EMPL45   DS    0H                                                               
*                                                                               
*                                                                               
* POST HERE EITHER THE MONTHLY BUCKET OR COMPLETE YTD AND MARK OTHERS           
*      AS WITH THE SAME 1C/1R/ANAL AS NOT CHANGED                               
*                                                                               
         USING ACTRECD,RF                                                       
         L     RF,ADACC                                                         
         USING PLBUFD,R2                                                        
         L     R2,APALWRK                                                       
         XC    0(PLLNQ,R2),0(R2)   CLEAR WORK AREA                              
         MVC   PLCPY,SRTCOMP       COMPANY                                      
         MVC   PL1CACC,SRTACC      1C ACCOUNT                                   
         MVC   PL1RACC,ACTKACT     1R ACCOUNT                                   
         MVC   PLMTHD,SRTMETHD     METHOD                                       
         MVC   PLYYMM,SRTMOS       YEAR/MONTH                                   
         MVC   PLANAL,GROUP        ANALYSIS CODE                                
         MVC   PLPTYP,SRTBTYPE     PAYROLL TYPE                                 
         ZAP   PLAMNT,SRTCR        AMOUNT                                       
*                                                                               
         MVC   MSG,=CL10'PAL-ADD R'                                             
         GOTO1 ADUMP,DMCB,(RC),(R2),PLLNQ                                       
*                                                                               
*        LA    RF,*+10             HIGH CORE TABLE                              
*        O     RF,=X'80000000'                                                  
*        BSM   0,RF                31 BIT MODE                                  
         GOTO1 BIN31,DMCB,(R2),PALBUFF  ADD TO TABLE                            
*        LA    RF,*+6                                                           
*        BSM   0,RF                24 BIT MODE                                  
*                                                                               
         GOTO1 AMRKYTD             MARK YTD TABLE THAT THIS WAS POSTED          
         DROP  R2,RF                                                            
*                                                                               
         TM    CLISTAT,CLINB+CLIPB+CLIHS  NEW BIZ/PRO BONO/HOUSE                
         BZ    EMPL20              NO  - CONTINUE                               
*                                                                               
* **1C NEW BIZ DIRECT TIME BACKOUT**                                            
*   MUCH OF SRTREC REMAINS THE SAME                                             
*                                                                               
         TM    CLISTAT,CLINB       IS THIS A NEW BIZ CLIENT                     
         BZ    EMPL50              NO  - MUST BE PRO BONO                       
         MVC   SRTCLEDG,OPTNBBLG   CONTRA LEDGER FOR NEW BIZ BACKOUT            
         MVC   SRTCON(L'OPTNBBAC),OPTNBBAC                                      
         B     EMPL70                                                           
*                                                                               
EMPL50   TM    CLISTAT,CLIPB       IS THIS A PRO BONO CLIENT                    
         BZ    EMPL60              NO  - MUST BE HOUSE                          
         MVC   SRTCLEDG,OPTPBBLG   CONTRA LEDGER FOR PRO BONO BACKOUT           
         MVC   SRTCON(L'OPTPBBAC),OPTPBBAC                                      
         B     EMPL70                                                           
*                                                                               
EMPL60   MVC   SRTCLEDG,OPTHSBLG   CONTRA LEDGER FOR HOUSE BACKOUT              
         MVC   SRTCON(L'OPTHSBAC),OPTHSBAC                                      
*                                                                               
EMPL70   SR    R1,R1                                                            
         IC    R1,CSTGRPLN                                                      
         SH    R1,=H'1'                                                         
         EX    R1,*+8              CONTRA HYBRID CODE                           
         B     *+10                                                             
         MVC   SRTCON+L'OPTPBBAC(0),CSTGRP                                      
         MVC   SRTCNAME,CSTGRPNM   CONTRA NAME                                  
         MVC   SRTBTYP,BUCKIT     BUCKET TYPE (METHOD AND SALARY TYPE)          
         ZAP   SRTCR,ALLOC         1R/ 1C OR 1N POSTED AMT                      
         GOTO1 MINUS1,DMCB,YTD1BUF GO MARK YTD-1 USED (RETURNED IN DUB)         
         CP    DUB,ZEROS           DID WE EVER POST TO THIS 1C 14 COMBO         
         BNZ   *+10                YES - THEN ALL IS WELL                       
         ZAP   SRTCR,ALLOCYTD      NO - THEN POST ENTIRE YTD AMNT               
         MP    SRTCR,=P'-1'        AS A MINUS                                   
         AP    PCREDITS,SRTCR      ADD TO POSTING TOTAL                         
         GOTO1 ADSORTER,DMCB,(L'PUTC,PUTC),(R5)                                 
         B     EMPL20                                                           
*                                                                               
EMPLX    B     COM1XIT                                                          
         DROP  R4,R5,R7                                                         
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO MARK YTDPLTB TABLE EACH TIME A BUCKET IS POSTED         *          
*         TO THE PALBUF                                              *          
*                                                                    *          
* ON ENTRY : R2 - A(PALBUFF)                                         *          
**********************************************************************          
         SPACE 1                                                                
         USING PLBUFD,R2                                                        
VMRKYTD  DS    0H                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         USING BIND,R1                                                          
         L     R1,YTDPLTB          R1=A(PAL RECORD TABLE)                       
         ICM   R3,15,BININ                                                      
         BZ    MRKYX                                                            
         USING YTDPLD,R7                                                        
         LA    R7,BINTABLE                                                      
         DROP  R1                                                               
*                                                                               
MRKY10   CLC   YTDPREC(YTDKLNQ),PLREC   SAME REC?                               
         BNE   *+12                                                             
         OI    YTDPSTAT,YTDPPST    MARK THAT IT WAS POSTED                      
         B     MRKYX                                                            
*                                                                               
         LA    R7,YTDPLNQ(R7)      BUMP TO NEXT ENTRY                           
         BCT   R3,MRKY10                                                        
*                                                                               
MRKYX    DS    0H                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
         B     COM1XIT                                                          
         DROP  R2,R7                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO COMPARE TO SCHEME                                       *          
*      "SCHEME CHECK"                                                *          
*                                                                    *          
* NOTE: THE CLIENT CODE AND DIRECT GROUP CODE(THE OFF/DEPT           *          
*       THAT WORKED ON THE CLIENT) ARE COMPARED TO THE SCHEME        *          
*       ENTRY TO SEE IF THIS CLIENT QUALIFIES.                       *          
**********************************************************************          
         SPACE 1                                                                
         USING SCHMD,R7                                                         
         USING BUFD,R5                                                          
SCHMCK   NTR1                                                                   
         L     R5,ABUFWRK                                                       
         MVC   WORK,SPACES                                                      
         LA    R2,BUFCLT            CLIENT CODE                                 
         SR    R0,R0                                                            
         IC    R0,OFFDISP           DISPLACEMENT OF OFFICE                      
         AR    R2,R0                                                            
         LH    R1,=Y(L'BUFCLT)      MOVE OFFICE/CLIENT DIVISION                 
         SR    R1,R0                                                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
         L     R7,ASCHMREC         ADDR OF SCHEME                               
         MVC   OFFG,SPACES                                                      
         TM    POOLTYPE,SCMINDI    THIS A INDIRECT POOL?                        
         BZ    SCHMCK01            NO - THEN USE REGULAR OFFLIST                
*                                                                               
         L     R4,APOLREC          LOOK FOR CLIENT LIST FOR IND POOLS           
         USING INDID,R4                                                         
         CLC   INDCLIST,SPACES                                                  
         BE    SCHMCK01                                                         
         USING LISTD,RE                                                         
         L     RE,ALISTAB                                                       
         LA    R0,LISTMAX                                                       
SCHMCK00 CLC   INDCLIST,LISTCDE    LOOKUP LIST INDEX                            
         BE    SCHMCK0A                                                         
         LA    RE,LISTLEN(RE)                                                   
         BCT   R0,SCHMCK00                                                      
         DC    H'0'                                                             
SCHMCK0A LA    R2,SAVEKEY                                                       
         USING CLTLD,R2                                                         
         MVC   CLTLD(CLTLKLEN),SPACES                                           
         MVC   CLTLIDX,LISTIDX      CLIENT LIST INDEX                           
         LA    R0,CLEVNUM                                                       
         LA    R3,CLEVELS                                                       
SCHMCK0B MVC   CLTLCODE,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,0(R3)             LENGTH OF LEVEL                             
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLTLCODE(0),BUFCLT                                               
         L     RE,CLTLBUFF                                                      
         USING BIND,RE                                                          
         MVC   DMCB+8(BINKLEN),BININ                                            
         LA    R4,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(X'00',(R2)),(R4)                                   
         TM    0(R1),NOTFOUND                                                   
         BZ    SCHMCK01            FOUND IT                                     
         LA    R3,CLVALN(R3)                                                    
         BCT   R0,SCHMCK0B                                                      
         B     SCHMNO              CLIENT IS NOT IN THE CLIENT LIST             
         DROP  RE                                                               
*                                                                               
SCHMCK01 TM    STAT1,SCHMOFLT      DO WE CHECK AGAINST AN OFFICE LIST?          
         BZ    SCHMCK04            NO -USE SCHEME ACCOUNT FOR COMPARE           
         TM    POOLTYPE,SCMINDS    IS THIS A IND PAY TYPE POOL?                 
         BZ    SCHMCK02            NO - THEN USE REGULAR OFFLIST                
*                                                                               
         OC    SCHMTAB2,SCHMTAB2   USE IND PAY TYPE OFFLIST                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R2,15,SCHMTAB2      R2 ADDR OF OFFLIST TABLE ENTRY               
         B     SCHMCK03                                                         
SCHMCK02 OC    SCHMTABI,SCHMTABI   DO WE HAVE AN OFFLIST COMPARE TABLE          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R2,15,SCHMTABI      R2 ADDR OF OFFLIST TABLE ENTRY               
         USING OFFLD,R2                                                         
SCHMCK03 LA    R4,OFFLOFF          SET R4 TO FIRST TABLE ENTRY                  
         MVC   OFFG,OFFLCDE        SAVE CODE FOR HEADL                          
         MVC   GRPDESC,OFFLNME     SAVE GROUPINGS DESCRIPTION FOR HEADL         
         SR    R0,R0                                                            
         IC    R0,OFFLNUM          NUMBER OF ENTRIES IN GROUP TABLE             
         LTR   R0,R0                                                            
         BP    SCHMCK06            IF POSITIVE,USE THE LIST                     
         BZ    *+6                 IF ZERO, IT'S AN OFFICE OVERRIDE             
         DC    H'0'                                                             
         LA    R4,OFFLCDE          SET R4 TO OFFICE OVERRIDE                    
         MVI   SCHEDESC,SCHDOVER   SET TO OFFICE OVERRID DESCRIPTION            
         B     *+8                                                              
SCHMCK04 LA    R4,SCHEMKEY         SET R4 TO SCHEME KEY                         
         MVC   GRPDESC,SPACES                                                   
         LA    R0,1                SET R1 FOR BCT DEFAULT IS 1                  
         USING OFFLOFF,R4                                                       
SCHMCK06 DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,SCHMCOM          LENGTH OF 1C COMPARE                         
         LTR   R1,R1                                                            
         BP    *+8                 MAKE SURE ZERO OR HIGHER                     
         LA    R1,1                                                             
         BCTR  R1,0                                                             
         MVC   SCHMBC+1(1),MASK    * MASK SETS CONDITION FOR BC                 
SCHMCK08 EXCLC R1,OFFLOFF,WORK     DOES THIS BUF CLT/GRP FIT THE SCHEME         
SCHMBC   BC    0,SCHMCK12          * EITHER "B"  "BE"  BNE" SET BY MASK         
         LA    R4,L'OFFLOFF(R4)                                                 
         BCT   R0,SCHMCK08                                                      
         B     SCHMNO              LEAVE WITH NOT EQUAL CONDITION               
*                                                                               
SCHMCK12 TM    STAT1,SCHMRECK      RECHECK SCHEME TO DIRECT TIME GROUP?         
         BZ    SCHMYES             NO NEED - LEAVE WITH EQUAL CC                
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFGCOD),BUFGCOD      DIRECT TIME GROUP                   
         TM    STAT1,SCHMOFLT      DO WE CHECK AGAINST AN OFFICE LIST?          
         BZ    SCHMCK16            NO -USE SCHEME ACCOUNT FOR COMPARE           
         OC    SCHMTABI,SCHMTABI   DO WE HAVE A OFFLIST COMPARE TABLE?          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R2,15,SCHMTABI      R2 ADDR OF OFFLIST TABLE ENTRY               
*                                                                               
         USING OFFLD,R2                                                         
         DROP  R4                                                               
         LA    R4,OFFLOFF          SET R4 TO FIRST TABLE ENTRY                  
         SR    R0,R0                                                            
         IC    R0,OFFLNUM          NUMBER OF ENTRIES IN GROUP TABLE             
         LTR   R0,R0                                                            
         BP    SCHMCK18            IF POSITIVE,USE THE LIST                     
         BZ    *+6                 IF ZERO, IT'S AN OFFICE OVERRIDE             
         DC    H'0'                                                             
         LA    R4,OFFLCDE          SET R4 TO OFFICE OVERRIDE                    
         B     *+8                                                              
SCHMCK16 LA    R4,SCHEMKEY         SET R4 TO SCHEME KEY                         
         LA    R0,1                SET R1 FOR BCT DEFAULT IS 1                  
         USING OFFLOFF,R4                                                       
SCHMCK18 DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,SCHMCOM          LENGTH OF COMPARE                            
         LTR   R1,R1                                                            
         BP    *+8                 MAKE SURE ZERO OR HIGHER                     
         LA    R1,1                                                             
         BCTR  R1,0                                                             
         MVC   SCHMBC2+1(1),MASK2  * MASK SETS CONDITION FOR BC                 
SCHMCK22 EXCLC R1,OFFLOFF,WORK     DOES THIS BUF CLT/GRP FIT THE SCHEME         
SCHMBC2  BC    0,SCHMYES           * EITHER "B"  "BE"  BNE" SET BY MASK         
         LA    R4,L'OFFLOFF(R4)                                                 
         BCT   R0,SCHMCK22                                                      
*                                                                               
SCHMNO   LTR   RB,RB               NOT EQUAL CONDITION ON EXIT                  
         B     COM1XIT                                                          
SCHMYES  CR    RB,RB               EQUAL CONDITION ON EXIT                      
         B     COM1XIT                                                          
*                                                                               
         DROP  R2,R4,R7                                                         
         EJECT                                                                  
*-----------------------------------------------------------                    
*              ROUTINE TO BUILD A SCHEME RECORD FROM COBLOCK                    
*                                                                               
*                                                                               
*-----------------------------------------------------------                    
*                                                                               
         USING SCHMD,R5                                                         
         USING COBLOCKD,R6                                                      
SCHEMER  NTR1                                                                   
         L     R5,ASCHEMWK          SCHEME WORK AREA                            
         L     R6,ACOBLOCK          A(COBLOCK) PROFILE BLOCK                    
         XC    SCHMD(SCHMLEN),SCHMD                                             
         MVC   SCHMACCT,SPACES                                                  
*                                                                               
         SR    R1,R1                                                            
         IC    R1,LENLEVA                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCHMACCT(0),COKOFC   MOVE IN OFFICE                              
         CLI   MODE,LEVAFRST                                                    
         BE    SCH01                                                            
         LA    RE,SCHMACCT                                                      
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    RE,R0                BUMP PAST LEVEL A                           
         SR    R1,R1                                                            
         IC    R1,LENLEVB                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),COKDPT       MOVE IN DEPT                                
SCH01    MVC   SCHMALN,ASCHDF1R     LENGTH OF OFFICE DEPT                       
         MVC   SCHMCOM,ASCHDF1C     1C COMPARE IS OFFICE LENGTH                 
*                                                                               
         OC    COIOFFL,COIOFFL      IS THERE A INDIRECT OFFICE LIST?            
         BZ    SCH02                                                            
         MVC   OFFG,COIOFFL         GO LOOK IT UP                               
         GOTO1 GOFFL                                                            
         BE    *+6                                                              
         DC    H'0'                 INVALID OFFICE                              
         MVC   SCHMTABI,AOFFL       ADDR OF OFFICE LIST INTO SCHEME             
*                                                                               
SCH02    OC    COISOFL,COISOFL      IS THERE A IND PAY TYPE OFF LIST?           
         BZ    SCH04                                                            
         MVC   OFFG,COISOFL         GO LOOK IT UP                               
         GOTO1 GOFFL                                                            
         BE    *+6                                                              
         DC    H'0'                 INVALID OFFICE                              
         MVC   SCHMTAB2,AOFFL       ADDR OF OFFICE LIST INTO SCHEME             
*                                                                               
         USING SCMD,R3                                                          
         USING SCCOD,R2                                                         
SCH04    L     R3,SCHMTBL           TAB OF COBLOCK/SCHEME EQUIVALENTS           
SCH04A   CLI   0(R3),EOT            END OF TABLE                                
         BE    SCH10                YES - ADD SCHEME RECORD TO TABLE            
         L     R1,ACOBLOCK                                                      
         AH    R1,SCMCOBL           DISP TO COBLOCK IND/OVH CODE                
         L     R2,SCHMCON           SCHEME STATUS SETTING TABLE                 
SCH06    CLI   0(R2),EOT            END OF TABLE?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SCCOSET,0(R1)        COMPARE TO COBLOCK CODE                     
         BE    *+12                                                             
         LA    R2,SCCOLEN(R2)                                                   
         B     SCH06                                                            
         L     R1,ASCHEMWK          A(SCHEME) RECORD                            
         AH    R1,SCMANAL           DISP TO IND/OVH CODE                        
         LA    RE,SCCOANAL              ANALYSIS CODE FOR INDIRECT              
         TM    SCMPTYPE,SCMOVHE+SCMINDS ALLOC LEVEL FOR OVERHEAD                
         BZ    *+8                                                              
         LA    RE,SCCOOHLV                                                      
         MVC   0(L'SCHAND,R1),0(RE)          MOVE CODE INTO SCHEME              
         L     R1,ASCHEMWK                   A(SCHEME) RECORD                   
         AH    R1,SCMST1                     DISP TO STATUS 1 SETTINGS          
         MVC   0(L'SCHMST_D,R1),SCCOSTA1     MOVE ST INTO SCHEME                
         L     R1,ASCHEMWK                   A(SCHEME) RECORD                   
         AH    R1,SCMST2                     DISP TO STATUS 2 SETTINGS          
         MVC   0(L'SCHMST2D,R1),SCCOSTA2     MOVE ST INTO SCHEME                
         LA    R3,SCMLEN(R3)                                                    
         B     SCH04A                                                           
*                                                                               
SCH10    GOTO1 BINADD,DMCB,ASCHEMWK,SCHEMBUF                                    
         B     COM1XIT                                                          
         DROP  R2,R3,R5,R6                                                      
         EJECT                                                                  
*-------------------------------------------------                              
*              ROUTINE TO READ TOTAL DIRECT TIME                                
*              BUFTYPE PASSED IN R3                                             
*-------------------------------------------------                              
*                                                                               
         USING BUFD,R5                                                          
TOTDIR   NTR1                                                                   
         L     R5,ABUFWRK                                                       
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         LA    R3,BUFNOGP          O/H NOT ALLOCATED BY GROUP                   
         TM    TYPE,OHTYPE         IS THIS FOR OVERHEAD ROUTINE                 
         BO    *+8                                                              
         LA    R3,BUFBYGP          IND ALLOCATED BY GROUP                       
         STC   R3,BUFTYPE          BY GROUP OR NEW BUSINESS                     
         SR    R0,R0                                                            
         IC    R0,LEVEL            SHOULD BE DEPT. LEVEL ONLY                   
         MVI   BUFCLI,BUFTOT       TOTAL RECORD                                 
         ZAP   DIRLST,ZEROS        CLEAR DIRECT TIME WORK FIELDS                
         ZAP   DIRYTD,ZEROS                                                     
         GOTO1 BUFFALO,DMCB,(L'GETC,GETC),((R3),ADBUFC),(R5),(R0)               
         CLI   DMCB+8,0                                                         
         BNE   TOTDIR02            NO DIRECT TIME                               
         ZAP   DIRLST,BDIRLST                                                   
         ZAP   DIRYTD,BDIRYTD                                                   
TOTDIR02 TM    TYPE,OHTYPE         IS THIS FOR OVERHEAD ROUTINE                 
         BZ    COM1XIT                                                          
*                                  **FOR OVERHEAD ONLY **                       
         CLI   OPTOVHY,C'Y'        IS OVERHEAD YTD                              
         BNE   COM1XIT                                                          
         AP    DIRLST,BDIRLSTM     IF IT IS ADD MEMO DIRECT YTD-1               
         AP    DIRYTD,BDIRLSTM                                                  
         B     COM1XIT                                                          
         EJECT                                                                  
         LTORG                                                                  
ABILITY1 EQU   (4095*3)-(*-VCOMMON)  REMAINING ADDRESSIBILITY                   
*                                    FREE ALL USINGS                            
         DROP                                                                   
         EJECT                                                                  
**********************************************************************          
* ROUTINES ENTERABLE FROM BASE OR WORK NMODS                         *          
* ************WORK NMOD # 2 ****************                         *          
**********************************************************************          
         SPACE 1                                                                
         DS    0D                                                               
VCOMMON2 NMOD1 0,**COMM2**,R9,R8                                                
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         LA    RC,SPACEND                                                       
         USING ACCAD,RC            RC=A(LOCAL W/S)                              
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     VREPORT             1 -PRINT REPORT                              
         B     VHEADUP             2 -HEADLINES                                 
         B     VBLDTAB             3 -BUILD SUPPORTING LEDGERS TABLE            
         B     VTABBLD             4 -BUILD YTD-1 AND NEW BIZ TABLES            
         B     VBINSRCH            5 -SEARCH BINARY TABLES                      
         B     VBINADD             6 -ADD TO BINARY TABLES (24 BIT)             
         B     VBIN31              7 -ADD TO BINARY TABLES (31 BIT)             
         B     VCLERWRK            8 -CLEAR CLIENT WORK AREA                    
         B     VDODIVD             9 -AMOUNT OF COST FOR THIS CLIENT            
         B     VCLERSRT            10-CLEAR SORT RECORD AREA                    
         B     VERRREPT            11-PRINT ERROR/WARNING REPORT                
         B     VRECREPT            12-PRINT RECORDS ADDED REPORT                
         B     VBXHOOK             13-INITIALIZE BOX PRINTING                   
         DC    (COMSPAR2*L'COM2)X'00'                                           
*                                                                               
*                                                                               
COM2XIT  XIT1  ,                                                                
         EJECT                                                                  
*----------------------------------------------------------------------         
*              BUILD HEADLINES AND PRINT DATA                                   
*                                                                               
*              PARAM1              BYTE 0 = RCSUBPRG                            
*                                  BYTE 1-3 = ADDRESS OF CLIBUF RECORD          
*              PARAM2              BYTE 0 = SPECIAL MODE ,C'T' = TOTAL          
*                                  BYTE 1-3 NOTUSED                             
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
*                                                                               
VREPORT  DS    0H                                                               
         TM    UPSI,SUPPRESS       SUPPRESS PRINTING OF THE REPORT?             
         BO    REPORTX             YES                                          
         USING CLID,R7                                                          
         L     R7,0(R1)                                                         
         MVC   RCSUBPRG,0(R1)                                                   
         MVC   MYMODE,4(R1)                                                     
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BE    REPER               PERSON PAGE                                  
         CLI   RCSUBPRG,1                                                       
         BE    REOVH               OVERHEAD PAGE                                
         CLI   RCSUBPRG,15                                                      
         BE    REOVH               OVERHEAD PAGE(INCOME)                        
         CLI   RCSUBPRG,18                                                      
         BE    REOVH               OVERHEAD PAGE(HOURS)                         
         CLI   RCSUBPRG,20                                                      
         BE    REOVH               OVERHEAD PAGE(DIR+DPT IND)                   
         CLI   RCSUBPRG,2                                                       
         BE    REIND               INDIRECT PAGE                                
         CLI   RCSUBPRG,14                                                      
         BE    REIND               INDIRECT PAGE(INCOME)                        
         CLI   RCSUBPRG,17                                                      
         BE    REIND               INDIRECT PAGE(HOURS)                         
         CLI   RCSUBPRG,19                                                      
         BE    REIND               INDIRECT PAGE(DIR+DPT IND)                   
         CLI   RCSUBPRG,3                                                       
         BE    RESUM               OFFICE DEPT SUMMARY                          
         DC    H'0'                                                             
         EJECT                                                                  
********************************************************************            
*        REPORT--------------------PERSON PAGE                                  
********************************************************************            
*                                                                               
*                                                                               
REPER    DS    0H                                                               
         CLI   QOPT2,C'S'          SUPPRESS PERSON PAGES?                       
         BNE   REPER02                                                          
         OC    WARNNUM,WARNNUM     WARNING FOR THIS EMPLOYEE?                   
         BZ    *+8                                                              
         BAS   RE,WARNEM           ADD WARNING TO ERROR TABLE                   
         B     REPORTX             NO PERSON PAGES                              
*                                                                               
REPER02  CLI   QOPT1,C'E'          ERRORS ONLY?                                 
         BNE   REPER04                                                          
         OC    WARNNUM,WARNNUM     WARNING FOR THIS EMPLOYEE?                   
         BZ    REPORTX             IF NOT AN ERROR DON'T PRINT                  
*                                                                               
REPER04  DS    0H                                                               
         GOTO1 HEADUP                                                           
         NI    BIT,X'FF'-(OPENBOX+CLOSEBOX)                                     
         CLC   PERSON,SVPERSON     SAME PERSON                                  
         BNE   REPER10                                                          
         CLC   OFFIC,SVOFFIC       SAME LOCATION                                
         BNE   REPER10                                                          
         CLC   DEPART,SVDEPART                                                  
         BNE   REPER10                                                          
         CLC   SDEPART,SVSDPT                                                   
         BNE   REPER10                                                          
         CLC   PAGE,SVPAGE         ARE WE ON A DIFFERENT PAGE                   
         BNE   REPER30                                                          
         CLC   LINE,MAXLINES       DID WE REACH THE END OF A PAGE               
         BL    *+12                                                             
         OI    BIT,NOPAYHD         SUPPRESS PRINTING OF PAYTYPE TOTALS          
         B     REPER20             FORCE A NEW PAGE                             
         BAS   RE,BOXMAINT         OPEN/CLOSE/PRINT BOX                         
         B     REPER40                                                          
*                                                                               
REPER10  GOTO1 HEADUP                                                           
         GOTO1 ACREPORT                                                         
         MVC   SVOFFIC,OFFIC                                                    
         MVC   SVDEPART,DEPART                                                  
         MVC   SVSDPT,SDEPART                                                   
         MVC   SVPERSON,PERSON                                                  
         SR    RF,RF                                                            
         IC    RF,LINE             CURRENT LINE NUMBER                          
         AH    RF,=Y(MXPERHED)     ADD # OF HEADINGS FOR PERSON                 
         CLM   RF,1,MAXLINES       IF CANT FIT ANY DETAIL LINES SKIP            
         BL    REPER30             TO NEXT PAGE                                 
REPER20  MVI   FORCEHED,C'Y'                                                    
         GOTO1 HEADUP              PRINT GENERAL HEADLINES                      
         GOTO1 ACREPORT                                                         
REPER30  OI    BIT,OPENBOX         SET FLAG TO OPEN A NEW BOX                   
         BAS   RE,BOXMAINT         OPEN/CLOSE/PRINT BOX                         
         BAS   RE,PERINFO          PRINT PERSON INFORMATION                     
         BAS   RE,PERHEAD          PRINT PERSON HEADINGS                        
         NI    BIT,X'FF'-NOPAYHD                                                
*                                                                               
REPER40  NI    BIT,X'FF'-(OPENBOX+CLOSEBOX)                                     
REPER42  LA    R4,P                                                             
         USING PRNTPD,R4                                                        
         CLI   MYMODE,0            CLIENT LINE FOR PERSON                       
         BE    REPER50                                                          
         CLI   MYMODE,C'T'         TOTAL LINE FOR A PERSON                      
         BE    REPER60                                                          
         DC    H'0'                MYMODE UNKNOW                                
*                                                                               
*                                  ****CLIENT LINE FOR A PERSON****             
REPER50  MVC   SVPAGE,PAGE         SAVE THE CURRENT PAGE NUMBER                 
         LA    R0,CBKCNT           NUMBER OF BUCKETS                            
         LA    R1,CLIBK            DISP TO 1ST BUCKET                           
         LA    RF,TOTLINE          ACCUM LINE                                   
         BAS   RE,ADDTOT           ADD CLIENT TOTALS TO TOTLINE                 
*                                                                               
         MVC   PTCLI,CLICDE        CLIENT CODE                                  
         MVC   PTCLIAN,CLIANALS    ANALYSIS CODE                                
         MVC   PTCLINM,CLICLNME    NAME                                         
*                                                                               
         LA    R2,CPERHRS          HOUR ACCUMS                                  
         LA    R0,3                NUMBER OF ACCUMS                             
         LA    R3,PTHR             HOUR AREA OF PRINT LINE                      
         BAS   RE,FRMTH                                                         
*                                  FORMAT DOLLARS                               
         LA    R2,CCSTLST          DOLLAR ACCUMS                                
         LA    R3,PTCOS1           PRINT AREA FOR $'S                           
         LA    R0,3                NUMBER OF ACCUMS                             
         BAS   RE,FRMTD                                                         
         B     REPORTP                                                          
*                                                                               
REPER60  GOTO1 HEADUP               SET-UP HEADLINES                            
         GOTO1 ACREPORT            SKIP LINE BEFORE TOTAL                       
         CLC   LINE,MAXLINES       DID WE REACH THE END OF A PAGE               
         BL    REPER65                                                          
         OI    BIT,NOPAYHD         SUPPRESS PRINTING OF PAYTYPE TOTALS          
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 HEADUP              PRINT GENERAL HEADLINES                      
         GOTO1 ACREPORT                                                         
         OI    BIT,OPENBOX         SET FLAG TO OPEN A NEW BOX                   
         BAS   RE,BOXMAINT         OPEN/CLOSE/PRINT BOX                         
         BAS   RE,PERINFO          PRINT PERSON INFORMATION                     
         BAS   RE,PERHEAD          PRINT PERSON HEADINGS                        
         NI    BIT,X'FF'-(NOPAYHD+OPENBOX)                                      
REPER65  MVI   PTCLI,C'*'                                                       
         MVC   PTCLI+1(L'AC@TOTAL),AC@TOTAL       *TOTAL*                       
         LA    RF,PTCLI+1+L'AC@TOTAL                                            
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'*'                                                       
         LA    R2,TOTLINE          HOUR ACCUMS                                  
         LA    R0,3                NUMBER OF ACCUMS                             
         LA    R3,PTHR             HOUR AREA OF PRINT LINE                      
         BAS   RE,FRMTH                                                         
         LA    R2,TOTLINE+24       DOLLAR ACCUMS                                
         LA    R3,PTCOS1           PRINT AREA FOR $'S                           
         LA    R0,3                NUMBER OF ACCUMS                             
         BAS   RE,FRMTD                                                         
*                                                                               
         GOTO1 HEADUP              SETUP HEADLINES                              
         GOTO1 ACREPORT                                                         
         OI    BIT,CLOSEBOX        PRINTING TOTALS SO CLOSE BOX                 
         BAS   RE,BOXMAINT         OPEN/CLOSE BOX                               
         B     REPORTX                                                          
         EJECT                                                                  
********************************************************************            
*        PRINT PERSON REPORT HEADINGS                                           
********************************************************************            
*                                                                               
         USING PRNTPHD,R4                                                       
PERHEAD  NTR1                                                                   
         LA    R4,P                                                             
         MVC   PHCLI,AC@CLI                                                     
         MVI   PHDIV,C'/'                                                       
         MVC   PHNCLI,AC@NCLI                                                   
         MVC   PHPERD,AC@PERD                                                   
         MVC   PHYTD,AC@YTD                                                     
         MVC   PHYTD2,AC@YTD                                                    
         MVC   PHYTD3,AC@YTD                                                    
         MVC   PHYTDN1,=CL2'-1'                                                 
         MVC   PHYTD4,AC@YTD                                                    
         MVC   PHPOST,AC@PSTG                                                   
         MVC   PHHRS,AC@RSHRS                                                   
         MVC   PHHRS2,AC@RSHRS                                                  
         MVC   PHADJHR,AC@APG72                                                 
         MVC   PHCOST,AC@COST                                                   
         MVC   PHCOST2,AC@COST                                                  
         MVC   PHAMT,=C'AMT'                                                    
         GOTO1 HEADUP                                                           
         GOTO1 ACREPORT                                                         
*                                                                               
         B     REPORTX                                                          
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
*        PRINT PERSON INFORMATION                                               
********************************************************************            
*                                                                               
PERINFO  NTR1                                                                   
         LA    R4,P                                                             
         MVC   P+1(11),LLEVDNAM                                                 
         MVC   P+12(L'SDEPART),SDEPART                                          
         MVC   P+12+L'SDEPART+1(L'PERSON),PERSON                                
         MVC   P+24(L'PERSONN),PERSONN                                          
         MVC   P+62(10),=C'(      = -'                                          
         MVC   P+63(6),AC@GROUP                                                 
         MVC   P+70(1),OPTDIRLG                                                 
         MVC   P+72(1),GROUP                                                    
         MVC   P+73(L'CSTGRP),CSTGRP                                            
         MVI   P+73+L'CSTGRP,C')'                                               
         GOTO1 ADSQUASH,DMCB,P+12,83                                            
         TM    BIT,NOPAYHD         SUPPRESS PRINTING OF PAYTYPES                
         BZ    PINF05                                                           
         GOTO1 HEADUP                                                           
         MVC   P+101(L'AC@CONTD),AC@CONTD    INDICATE PERSON INFO               
         GOTO1 ACREPORT                     CONTINUED ON NEXT PAGE              
         GOTO1 ACREPORT                                                         
         B     PINFXX                                                           
*                                                                               
PINF05   L     R6,APERWRK                                                       
         LA    R4,P+101                                                         
         USING PTOTD,R6                                                         
         USING SALBLKD,R4                                                       
         LA    R3,4                CAN SHOW 3 SAL TYPES PLUS TOTAL              
         USING CMPUTD,R5                                                        
         L     R5,COMPRAT          TBL OF DISP TO SAL TYP ACCUMS                
PINF10   CLI   0(R5),EOT           END OF TABLE                                 
         BE    PINF25                                                           
         CLI   CMBUKTYP,PHRTTOT    IS THIS THE "TOTAL" HRLY RATE?               
         BNE   *+8                                                              
         LH    R3,=H'1'            MAKE SURE IT'S THE LAST TIME THRU            
         L     R2,ARATWRK          EMPLOYEES HRLY RATE RECORD                   
         AH    R2,CMRATD           DISP TO HOURLY RATE                          
         CP    0(RBUKLN,R2),ZEROS  DO WE HAVE A HRLY RATE                       
         BNZ   PINF15              YES - CONTINUE                               
         L     RF,APERWRK          EMPLOYEES SALARY RECORD                      
         AH    RF,CMPTOTD          DISP TO SALARY DOLLARS                       
         CP    0(PBUKLN,RF),ZEROS                                               
         BNZ   PINF15              YES - CONTINUE                               
         LA    R5,CMPULN(R5)       NEXT SAL TYPE IN TABLE                       
         B     PINF10                                                           
PINF15   MVC   SALBLKD(SALBLKLN),SPACES                                         
         MVC   SALCOD,CMDESC       DESCRIPTION CODE                             
         MVI   SALEQUAL,C'='                                                    
         ZAP   DUB,0(RBUKLN,R2)    HOURLY RATE TO 2 DP(ORIGINALLY 4DP)          
         SRP   DUB,64-2,5                                                       
         CURED DUB,(L'SALPERH,SALPERH),2,MINUS=YES                              
         L     R2,APERWRK          EMPLOYEES SALARY RECORD                      
         AH    R2,CMPTOTD          DISP TO SALARY DOLLARS                       
         ZAP   DUB,0(PBUKLN,R2)                                                 
         CURED DUB,(L'SALFIG,SALFIG),2,TRAIL=/                                  
*                                                                               
PINF20   LA    R4,L'P(R4)         NEXT LINE IN REPORT                           
         LA    R5,CMPULN(R5)       NEXT SAL TYPE IN TABLE                       
         BCT   R3,PINF10                                                        
*                                                                               
PINF25   CLI   OPTHRS,C'Y'         STANDARD HRS REQ?                            
         BNE   PINF35                                                           
         TM    PERSTAT,PERUSEAC    USE ACTUAL HOURS FOR THIS PERSON?            
         BO    PINF35                                                           
         L     R3,ASALAREA                                                      
         USING SALARYD,R3                                                       
         MVC   PSECOND+65(L'AC@STDHS),AC@STDHS    STD HRS                       
         ZAP   DUB,SALSTDHR                                                     
         CURED DUB,(9,PSECOND+74),2,MINUS=YES                                   
         CP    SALSTDPC,ZEROS                                                   
         BE    PINF30                                                           
         MVC   PTHIRD+65(L'AC@PCTS),AC@PCTS      % TO STD                       
         ZAP   DUB,SALSTDPC                                                     
         CURED DUB,(9,PTHIRD+74),4,MINUS=YES                                    
PINF30   CP    SALDPTPC,ZEROS                                                   
         BE    PINF35                                                           
         MVC   PFOURTH+65(L'AC@PCTD),AC@PCTD      % TO DPT                      
         ZAP   DUB,SALDPTPC                                                     
         CURED DUB,(9,PFOURTH+74),4,MINUS=YES                                   
PINF35   TM    PERSTAT,PERDIRT     POST DIRECT ONLY                             
         BZ    PINF40                                                           
         MVC   PSECOND+1(L'AC@SCHM9),AC@SCHM9  SCHEME #9 "DIRECT ONLY"          
*                                                                               
PINF40   TM    PERSTAT,PADJPCT     ADJUSTING SALARY PCT?                        
         BZ    PINF45                                                           
         MVC   PFOURTH+61(L'AC@SLRAT),AC@SLRAT    SALARY AT                     
         CURED PERADJ,(9,PFOURTH+64+L'AC@SLRAT),4,TRAIL=%                       
*                                                                               
PINF45   GOTO1 HEADUP                                                           
         GOTO1 ACREPORT                                                         
         TM    PERSTAT,PEREXEC     EXECUTIVE?                                   
         BZ    PINF50                                                           
         MVC   P+101(2),=C'**'     ** FLAGGED AS EXEC **                        
         MVC   P+104(L'AC@EXEF),AC@EXEF                                         
         LA    RF,P+104+L'AC@EXEF                                               
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   3(2,RF),=C'**'                                                   
PINF50   TM    PERSTAT,PERENOHR    EMPLOYEE HAS NO HOURS?                       
         BZ    PINF55                                                           
         MVC   PSECOND+101(2),=C'**'     ** NO HOURS PRESENT **                 
         MVC   PSECOND+104(L'AC@NOHRS),AC@NOHRS                                 
         LA    RF,PSECOND+104+L'AC@NOHRS                                        
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   3(2,RF),=C'**'                                                   
PINF55   TM    PERSTAT,PERENOSL    EMPLOYEE HAS NO SALARY?                      
         BZ    PINFX                                                            
         MVC   PSECOND+101(2),=C'**'     ** NO SALARY PRESENT **                
         MVC   PSECOND+104(L'AC@NOSLR),AC@NOSLR                                 
         LA    RF,PSECOND+104+L'AC@NOSLR                                        
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   3(2,RF),=C'**'                                                   
*                                                                               
PINFX    GOTO1 HEADUP                                                           
         GOTO1 ACREPORT                                                         
         OC    WARNNUM,WARNNUM     WARNING FOR THIS EMPLOYEE?                   
         BZ    *+8                                                              
         BAS   RE,WARNEM           ADD WARNING TO ERROR TABLE                   
PINFXX   B     REPORTX                                                          
         EJECT                                                                  
*---------------------------------------------------------                      
*              ADD A WARNING TO ERRROR TABLE                                    
*---------------------------------------------------------                      
*                                                                               
WARNEM   NTR1                                                                   
         USING ERRORD,R3                                                        
         MVC   WORK,SPACES                                                      
         LA    R3,WORK                                                          
         MVC   ERRNUM,WARNNUM                                                   
         MVC   ERRACCT(L'EMPLEDG),EMPLEDG                                       
         MVC   ERRACCT+L'EMPLEDG(L'ACCOUNT),ACCOUNT                             
         SR    R1,R1                                                            
         ICM   R1,3,PAGE                                                        
         CVD   R1,DUB                                                           
         CP    DUB,=P'1'                 SUB 1 FROM PAGE UNLESS PAGE1           
         BNH   *+10                                                             
         SP    DUB,=P'1'                                                        
         ZAP   ERRAMNT,DUB               PAGE NUMBER OF EMPLOYEE                
         SRP   ERRAMNT,2,0               SHIFT 2 POSITIONS TO LEFT              
         MVI   ERRTYPE,ERRWARN           SET TO A WARNING                       
         GOTO1 BINADD,DMCB,(R3),ERRORBUF ADD TO TABLE                           
         L     RF,APERWRK                MARK EMPL AS POSSIBLE ERROR            
         OI    PERSTAT-PTOTD(RF),PERERROR                                       
         B     COM2XIT                                                          
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
*        MAINTAIN BOX PRINTING                                                  
********************************************************************            
*                                                                               
         USING BOXD,R3                                                          
BOXMAINT NTR1                                                                   
         L     R3,ADBOX                                                         
*                                                                               
         TM    BIT,OPENBOX         OPENING A NEW BOX?                           
         BZ    BOXM10                                                           
         SR    RF,RF                                                            
         IC    RF,LINE             POINT TO CURRENT LINE                        
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'T'                                                       
         MVI   BOXCOLS,C'L'       SET LEFT SIDE COLUMN                          
         MVI   BOXCOLS+129,C'R'    SET RIGHT SIDE COLUMN                        
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         B     BOXM30                                                           
*                                                                               
BOXM10   TM    BIT,CLOSEBOX        CLOSING THE CURRENT BOX?                     
         BZ    BOXM20                                                           
         ZIC   RF,LINE             POINT TO CURRENT LINE                        
         LA    RF,BOXROWS-1(RF)    POINT TO END OF BOX                          
         MVI   0(RF),C'B'          CLOSE CURRENT BOX                            
         GOTO1 HEADUP                                                           
         B     BOXX                                                             
*                                                                               
BOXM20   MVI   BOXCOLS,C'L'       SET LEFT SIDE COLUMN                          
         MVI   BOXCOLS+129,C'R'    SET RIGHT SIDE COLUMN                        
         GOTO1 HEADUP                                                           
         B     BOXX                                                             
*                                                                               
BOXM30   GOTO1 HEADUP                                                           
         GOTO1 ACREPORT                                                         
BOXX     B     REPORTX                                                          
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
*        REPORT--------------------OVERHEAD PAGE                                
********************************************************************            
*                                                                               
*                                                                               
REOVH    DS    0H                                                               
         CLI   QOPT3,C'S'                                                       
         BE    REPORTX             NO OVERHEAD PAGES                            
*                                                                               
         LA    R4,P                                                             
         USING PRNTPD,R4                                                        
         CLI   MYMODE,0                                                         
         BE    REOVH02                                                          
         CLI   MYMODE,C'T'                                                      
         BE    REOVH04                                                          
         CLI   MYMODE,2                                                         
         BE    REOVH06                                                          
         CLI   MYMODE,3                                                         
         BE    REOVH10                                                          
         CLI   MYMODE,5                                                         
         BE    REOVH14                                                          
         DC    H'0'                MYMODE UNKNOW                                
*                                                                               
REOVH02  LA    R0,2                NUMBER OF BUCKETS                            
         LA    R1,CDIRLST          DISP TO 1ST BUCKET                           
         LA    RF,TOTLINE          ACCUM LINE                                   
         BAS   RE,ADDTOT           ADD DIRECT TO TOTAL                          
*                                                                               
         LA    R0,3                                                             
         LA    R1,OVHLST                                                        
         LA    RF,TOTLINE+16       ACCUM LINE                                   
         BAS   RE,ADDTOT           ADD OVERHEAD YTD-1,YTD,POST TO TOTAL         
*                                                                               
         MVC   PTOHCLI,CLICDE                                                   
         MVC   PTOHCLNM,CLICLNME                                                
         CLI   OPTSHOWO,C'Y'       SHOW DIRECT GRP FOR OVERHEAD PAGES?          
         BNE   *+16                                                             
         MVC   PTOHGRPL,OPTDIRLG   LEDGER                                       
         MVC   PTOHGRPC,CLGRPING   GROUP CODE FOR DIRECT                        
*                                                                               
         LA    R2,CDIRLST          DOLLAR ACCUMS                                
         LA    R3,PTOHCOS          PRINT AREA FOR $'S                           
         LA    R0,2                NUMBER OF ACCUMS                             
         BAS   RE,FRMTD                                                         
*                                                                               
         LA    R2,OVHLST           OVERHEAD DOLLAR ACCUMS                       
         LA    R3,PTOH             PRINT AREA FOR $'S                           
         LA    R0,3                NUMBER OF ACCUMS                             
         BAS   RE,FRMTD                                                         
         B     REPORTP                                                          
*                                                                               
REOVH04  DS    0H                                                               
         GOTO1 HEADUP              SETUP HEADLINES AGAIN                        
         GOTO1 ACREPORT            SKIP LINE BEFORE TOTAL                       
         MVI   PTOHCLI,C'*'                                                     
         MVC   PTOHCLI+1(L'AC@TOTAL),AC@TOTAL      *TOTAL*                      
         LA    RF,PTOHCLI+1+L'AC@TOTAL                                          
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'*'                                                       
         LA    R2,TOTLINE          TOTAL ACCUMS                                 
         LA    R3,PTOHCOS          PRINT AREA FOR $'S                           
         LA    R0,5                NUMBER OF ACCUMS                             
         BAS   RE,FRMTD            FORMAT DIRECT DOLLARS                        
         B     REPORTP                                                          
*                                                                               
REOVH06  LA    R3,PTOH             PRINT AREA                                   
         LA    R0,2                NUMBER OF ACCUMS                             
         LA    R2,OVHLST           OVERHEAD ACCUMS                              
         BAS   RE,FRMTD                                                         
*                                NO OFFICE DIRECT TIME - COST TO CORP           
         MVC   PTOHCLI(L'AC@NODTC),AC@NODTC                                     
         CLI   LEVEL,OFFICE                                                     
         BE    REPORTP                                                          
*                               'NO DIRECT TIME - COST TO OFFICE'               
         MVC   PTOHCLI(L'AC@NDDTO),AC@NDDTO                                     
         B     REPORTP                                                          
*                                ** ERROR ** NO DIRECT TIME COSTS'              
REOVH10  MVC   PTOHCLI(L'AC@ENDTC),AC@ENDTC                                     
         B     REPORTP                                                          
*                                                                               
REOVH14  L     RF,APOLREC                                                       
         USING OVERD,RF                                                         
         CLI   OVHLEVL,OFFICE                                                   
         BE    REOVH16                                                          
         MVC   PTOHCLI(2),=C'**'  ** ALLOCATE AT CORP LEVEL PER SCHEM           
         MVC   PTOHCLI+3(L'AC@ACFLV),AC@ACFLV                                   
         LA    RF,PTOHCLI+3+L'AC@ACFLV                                          
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   3(2,RF),=C'**'                                                   
         B     REPORTP                                                          
REOVH16  MVC   PTOHCLI(2),=C'**'  ** ALLOCATE AT OFFICE LEVEL PER SCHEM         
         MVC   PTOHCLI+3(L'AC@AOFLV),AC@AOFLV                                   
         LA    RF,PTOHCLI+3+L'AC@AOFLV                                          
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   3(2,RF),=C'**'                                                   
         B     REPORTP                                                          
         DROP  RF                                                               
         EJECT                                                                  
********************************************************************            
*        REPORT--------------------INDIRECT PAGE                                
********************************************************************            
*                                                                               
*                                                                               
REIND    DS    0H                                                               
         CLI   QOPT3,C'S'                                                       
         BE    REPORTX             NO OVERHEAD/INDIRECT PAGES                   
*                                                                               
         LA    R4,P                                                             
         USING PRNTPD,R4                                                        
         CLI   MYMODE,0                                                         
         BE    REIND2                                                           
         CLI   MYMODE,C'T'                                                      
         BE    REIND4                                                           
         CLI   MYMODE,1                                                         
         BE    REIND6                                                           
         CLI   MYMODE,4                                                         
         BE    REIND10                                                          
         CLI   MYMODE,C'S'                                                      
         BE    REIND12                                                          
         DC    H'0'                MYMODE UNKNOWN                               
*                                                                               
REIND2   ZAP   CDIRPST,CDIRYTD                                                  
         SP    CDIRPST,CDIRLST                                                  
*                                                                               
         LA    R0,3                NUMBER OF BUCKETS                            
         LA    R1,CDIRLST          DISP TO 1ST BUCKET                           
         LA    RF,TOTLINE          ACCUM LINE                                   
         BAS   RE,ADDTOT           ADD DIRECT TO TOTLINE                        
*                                                                               
         LA    R0,3                                                             
         LA    R1,INDLST                                                        
         CLI   QOPT7,C'B'          BREAKDOWN INDIR POOLS BY PTYPE?              
         BE    *+8                                                              
         LA    R1,INDTTLST                                                      
         BAS   RE,ADDTOT           ADD IND YTD-1,YTD,POST TO TOTAL              
*                                                                               
         MVC   PTINCLI,CLICDE                                                   
         MVC   PTINCLNM,CLICLNME                                                
         CLI   OPTSHOWD,C'Y'       SHOW DIRECT GRP FOR INDIRECT PAGES?          
         BNE   *+10                                                             
         MVC   PTINGRPC,CLGRPING   GROUP FOR DIRECT                             
*                                                                               
         LA    R3,PTINCOS          PRINT AREA                                   
         LA    R0,3                NUMBER OF ACCUMS                             
         LA    R2,CDIRLST          DIRECT(OR INCOME) ACCUMS                     
         BAS   RE,FRMTD            FORMAT DIRECT DOLLARS                        
*                                                                               
         LA    R3,PTIN             PRINT AREA FOR INDIRECT DOLLARS              
         LA    R0,3                NUMBER OF ACCUMS                             
         LA    R2,INDLST           INDIRECT ACCUMS                              
         CLI   QOPT7,C'B'          BREAKDOWN INDIR TOTALS BY PTYPE              
         BE    *+8                                                              
         LA    R2,INDTTLST                                                      
         BAS   RE,FRMTD            FORMAT INDIRECT DOLLARS                      
         B     REPORTP                                                          
*                                                                               
REIND4   DS    0H                                                               
         GOTO1 HEADUP              SETUP HEADLINES AGAIN                        
         GOTO1 ACREPORT            SKIP LINE BEFORE TOTAL                       
         MVI   PTINCLI,C'*'                                                     
         MVC   PTINCLI+1(L'AC@SUBT),AC@SUBT   *SUB TOTAL*                       
         LA    RF,PTINCLI+1+L'AC@SUBT                                           
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'*'                                                       
*                                                                               
         LA    R3,PTINCOS          PRINT AREA                                   
         LA    R0,3                NUMBER OF ACCUMS                             
         LA    R2,TOTLINE          DIRECT(OR INCOME) TOTAL ACCUMS               
         BAS   RE,FRMTD            FORMAT DIRECT DOLLARS                        
*                                                                               
         LA    R3,PTIN             PRINT AREA FOR INDIRECT DOLLARS              
         LA    R0,3                NUMBER OF ACCUMS                             
         LA    R2,TOTLINE+24       INDIRECT TOTAL ACCUMS                        
         BAS   RE,FRMTD            FORMAT INDIRECT DOLLARS                      
         B     REPORTP                                                          
*                                                                               
REIND6   LA    R3,PTIN             PRINT AREA FOR INDIRECT DOLLARS              
         LA    R0,3                NUMBER OF ACCUMS                             
         LA    R2,INDLST           INDIRECT ACCUMS                              
         BAS   RE,FRMTD            FORMAT INDIRECT DOLLARS                      
*                                                                               
*                               'NO DIRECT TIME - COST TO OFFICE'               
         MVC   PTOHCLI(L'AC@NDDTO),AC@NDDTO                                     
         CLI   MODE,LEVBLAST                                                    
         BE    REPORTP                                                          
*                                NO OFFICE DIRECT TIME - COST TO CORP           
         MVC   PTOHCLI(L'AC@NODTC),AC@NODTC                                     
         B     REPORTP                                                          
*                                ** ERROR ** NO DIRECT TIME FOR SCHEME          
*                                ** ERROR ** POOL CANNOT BE ALLOCATED           
REIND10  MVC   PTINCLI(L'AC@NODIR),AC@NODIR                                     
         MVC   PSECOND+1(L'AC@POOLA),AC@POOLA                                   
         B     REPORTP                                                          
*                                                                               
REIND12  DS    0H                    DEPT. SUMMARIES                            
         L     R7,ADEPTAB          DEPT. LABOR SUMMARY                          
         CLI   MODE,LEVBLAST                                                    
         BE    REIND14                                                          
         L     R7,AOFFTAB                                                       
         CLI   MODE,LEVALAST                                                    
         BE    REIND14                                                          
         L     R7,ACORTAB                                                       
REIND14  BAS   RE,DEPLOOK          ANYTHING IN SUMMARY                          
         CLC   TOTLINE(24),=3PL8'0'                                             
         BE    REIND16             NOTHING IN TABLE FOR LABOR                   
         GOTO1 HEADUP                                                           
         GOTO1 ACREPORT                                                         
         BAS   RE,DEPSUM           PRINT THE SUMMARY                            
         MVI   PTINCLI,C'*'                                                     
         MVC   PTINCLI+1(L'AC@TTIME),AC@TTIME        *TOTAL TIME*               
         LA    RF,PTINCLI+1+L'AC@TTIME                                          
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'*'                                                       
         LA    R3,PTINCOS          PRINT AREA                                   
         LA    R0,3                NUMBER OF ACCUMS                             
         LA    R2,TOTLINE          DIRECT(OR INCOME) TOTAL ACCUMS               
         BAS   RE,FRMTD            NOW PRINT THE TOTAL                          
         GOTO1 HEADUP                                                           
         GOTO1 ACREPORT                                                         
REIND16  L     R7,ADEPOTAB                                                      
         CLI   MODE,LEVBLAST                                                    
         BE    REIND18                                                          
         L     R7,AOFFOTAB                                                      
         CLI   MODE,LEVALAST                                                    
         BE    REIND18                                                          
         L     R7,ACOROTAB                                                      
REIND18  BAS   RE,DEPLOOK          ANYTHING FOR OVERHEAD                        
         CLC   TOTLINE(24),=3PL8'0'                                             
         BE    REPORTX             NOTHING SO GET OUT                           
         GOTO1 HEADUP                                                           
         GOTO1 ACREPORT            SKIP A LINE                                  
         BAS   RE,DEPSUM           PRINT THE OVERHEAD SUMMARY                   
         MVI   PTINCLI,C'*'                                                     
         MVC   PTINCLI+1(L'AC@TOVER),AC@TOVER *TOTAL OVERHEAD*                  
         LA    RF,PTINCLI+1+L'AC@TOVER                                          
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'*'                                                       
         LA    R3,PTINCOS          PRINT AREA                                   
         LA    R0,3                NUMBER OF ACCUMS                             
         LA    R2,TOTLINE          DIRECT(OR INCOME) TOTAL ACCUMS               
         BAS   RE,FRMTD            NOW PRINT THE TOTAL                          
         GOTO1 HEADUP                                                           
         GOTO1 ACREPORT                                                         
         B     REPORTX                                                          
         EJECT                                                                  
********************************************************************            
*              SEE IF ANYTHING TO PRINT                                         
*                                                                               
********************************************************************            
         USING TABD,R7                                                          
DEPLOOK  NTR1                                                                   
         MVC   TOTLINE(80),=10PL8'0'                                            
DEPLOOKA LA    R2,TABBKS           FIRST BUCKET DISP.                           
         LA    R3,TOTLINE                                                       
         LA    R0,3                                                             
DEPLOOKB CLI   0(R2),0                                                          
         BE    DEPLOOKC            NO BUCKET                                    
         LH    R1,TABADD           DISP TO TABLE                                
         AR    R1,RC               IN MY WORK AREA                              
         L     R1,0(R1)                                                         
         SR    R5,R5                                                            
         IC    R5,0(R2)            DISP. TO BUCKET IN SUMMARY RECORD            
         LA    R4,0(R5,R1)                                                      
         AP    0(SBKLN,R3),0(SBKLN,R4)         ADD TO TOTLINE                   
DEPLOOKC LA    R2,1(R2)            NEXT BUCKET DISP.                            
         LA    R3,SBKLN(R3)        NEXT TOTLINE                                 
         BCT   R0,DEPLOOKB                                                      
         LA    R7,TABLEN(R7)       NEXT CATEGORY                                
         CLI   0(R7),X'FF'         END OF TABLE                                 
         BE    REPORTX                                                          
         B     DEPLOOKA                                                         
         EJECT                                                                  
********************************************************************            
*              PRINT THE DEPT. SUMMARY                                          
********************************************************************            
*                                                                               
DEPSUM   NTR1                                                                   
         USING PRNTPD,R4                                                        
         LA    R4,P                                                             
DEPSUMA  LA    R5,TABBKS           FIRST BUCKET DISP                            
         LA    R6,3                                                             
         LA    R3,PTINCOS                                                       
DEPSUMB  CLI   0(R5),0                                                          
         BE    DEPSUMC                                                          
         LH    RE,TABADD                                                        
         AR    RE,RC                                                            
         L     RE,0(RE)                                                         
         SR    R1,R1                                                            
         IC    R1,0(R5)                                                         
         LA    R2,0(R1,RE)         R2 TO BUCKET                                 
         CP    0(SBKLN,R2),ZEROS                                                
         BZ    DEPSUMC             SKIP ZERO                                    
         CP    0(SBKLN,R2),=P'100'                                              
         BNL   DEPSUMBB            ONE DOLLAR OR HIGHER                         
         CP    0(SBKLN,R2),=P'-100'                                             
         BNL   DEPSUMC             LOWER THAN MINUS DOLLAR                      
DEPSUMBB MVC   PTINCLI(L'TABNME),TABNME      PRINT CATEGORY NAME                
         LA    R0,1                R0 # OF BUCKETS R2 ACCUMS R3 P LINE          
         BAS   RE,FRMTD            PRINT THE AMOUNT                             
         SH    R3,=Y(L'PTINCOS)                                                 
DEPSUMC  LA    R5,1(R5)            NEXT BUCKET DISP.                            
         LA    R3,L'PTINCOS(R3)    NEXT PRINT AREA                              
         BCT   R6,DEPSUMB                                                       
         CLC   P,SPACES                                                         
         BE    DEPSUMD             NOTHING TO PRINT                             
         GOTO1 HEADUP                                                           
         GOTO1 ACREPORT            PRINT A LINE OF SUMMARY                      
DEPSUMD  LA    R7,TABLEN(R7)                                                    
         CLI   0(R7),X'FF'         END OF TABLE                                 
         BNE   DEPSUMA                                                          
         B     REPORTX             GO AWAY                                      
         EJECT                                                                  
********************************************************************            
*        REPORT--------------------OFFICE-DEPT SUMMARY PAGE                     
********************************************************************            
*                                                                               
         USING SUMD,R6                                                          
         USING PRNTPD,R4                                                        
RESUM    LA    R4,P                                                             
         L     R6,ASUMREC          CURRENT SUMMARY RECORD                       
         LA    RE,SDIRPST          1ST BUCKET                                   
         LA    R0,SBKSCNT          NUMBER OF BUCKETS                            
         CP    0(SBKLN,RE),ZEROS                                                
         BNE   RESUM2                                                           
         LA    RE,SBKLN(RE)                                                     
         BCT   R0,*-14                                                          
         B     REPORTX             NOHING GOOD TO PRINT                         
*                                                                               
RESUM2   SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SOFFDEP(0),FOXES    FOXES IN OFFICE MEANS AGY TOTALS             
         BNE   RESUM3                                                           
         GOTO1 HEADUP                                                           
         GOTO1 ACREPORT            SKIP LINE BEFORE TOTAL                       
         MVI   PTSMDESP,C'*'                                                    
         MVC   PTSMDESP+1(L'AC@AGYTS),AC@AGYTS   *AGENCY TOTALS*                
         LA    RF,PTSMDESP+1+L'AC@AGYTS                                         
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'*'                                                       
         B     RESUM7                                                           
*                                                                               
RESUM3   LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),FOXES      OFFICE TOTAL                                  
         BNE   RESUM4                                                           
         MVI   SPACING,2                                                        
         MVI   PTSMDESP,C'*'                                                    
         MVC   PTSMDESP+1(L'AC@OFFTS),AC@OFFTS   *OFFICE TOTALS*                
         LA    RF,PTSMDESP+1+L'AC@OFFTS                                         
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'*'                                                       
         B     RESUM7                                                           
*                                                                               
RESUM4   SR    R1,R1               LENGTH OF LEVEL A                            
         IC    R1,LENLEVA                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PTSMOFF(0),SOFFDEP  OFFICE CODE                                  
         LA    R2,SOFFDEP                                                       
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PTSMDPT(0),0(R2)    DEPT CODE                                    
*                                                                               
RESUM7   LA    R0,SBKSCNT          NUMBER OF ACCUMS                             
         LA    R2,SDIRPST          1ST ACCUM                                    
         LA    R3,PTSM             PRINT LINE                                   
         BAS   RE,FRMTD2           FORMAT SUMMARY DOLLARS                       
         B     REPORTP                                                          
         EJECT                                                                  
REPORTP  GOTO1 HEADUP              SET UP HEADLINES                             
         GOTO1 ACREPORT                                                         
REPORTX  B     COM2XIT                                                          
*                                                                               
ADDTOT   AP    0(8,RF),0(8,R1)     ADD ACCUMULATORS TO TOTLINE                  
         LA    RF,8(RF)                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,*-14                                                          
         BR    RE                                                               
*                                                                               
*                                                                               
*                                     FORMAT HOURS                              
FRMTH    ST    RE,SAVERE                                                        
FRMTH01  ZAP   DOUBLE,0(8,R2)                                                   
         CURED DOUBLE,(11,0(R3)),2,MINUS=YES,ALIGN=RIGHT                        
         LA    R2,8(R2)                                                         
         LA    R3,L'PTHR(R3)                                                    
         BCT   R0,FRMTH01                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
*                                                                               
*                                     FORMAT DOLLARS                            
FRMTD    ST    RE,SAVERE                                                        
FRMTD01  ZAP   DOUBLE,0(8,R2)                                                   
         SRP   DOUBLE,64-2,5                                                    
         CURED DOUBLE,(L'PTCOS,(R3)),0,ZERO=BLANK,MINUS=YES,ALIGN=RIGHT         
         LA    R2,8(R2)                                                         
         LA    R3,L'PTCOS(R3)                                                   
         BCT   R0,FRMTD01                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
FRMTD2   ST    RE,SAVERE                                                        
FRMTD2A  ZAP   DOUBLE,0(8,R2)                                                   
         SRP   DOUBLE,64-2,5                                                    
         CURED DOUBLE,(L'PTSM,(R3)),0,ZERO=BLANK,MINUS=YES,ALIGN=RIGHT          
         LA    R2,8(R2)                                                         
         LA    R3,L'PTSM(R3)                                                    
         BCT   R0,FRMTD2A                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*-----------------------------------------------------                          
*              ROUTINES TO HEADUP PAGE                                          
*                                                                               
*-----------------------------------------------------                          
*                                                                               
*                                                                               
VHEADUP  DS    0H                                                               
         MVC   HEAD3+1(L'AC@METH),AC@METH      METHOD                           
         MVC   HEAD3+1+L'AC@METH+1(1),QMTHD                                     
         MVC   HEAD3+12(32),METHNAME                                            
         MVC   HEAD3+46(L'HEADMON),HEADMON                                      
         MVC   HEAD4+48(L'LIVEOPT),LIVEOPT                                      
         CLI   QOPT5,C'F'                      O/H F R O Z E N **               
         BNE   *+10                                                             
         MVC   HEAD4+48+L'LIVEOPT-2(L'AC@OHFRO),AC@OHFRO                        
*                                                                               
*                                  HEADUP A PERSON PAGE                         
         CLI   RCSUBPRG,0                                                       
         BNE   HEADOVH                                                          
         MVC   HEAD4+1(11),LLEVANAM                                             
         MVC   HEAD4+12(L'OFFIC),OFFIC                                          
         MVC   HEAD4+16(L'OFFICN-7),OFFICN                                      
         MVC   HEAD5+1(11),LLEVBNAM                                             
         MVC   HEAD5+12(L'DEPART),DEPART                                        
         MVC   HEAD5+16(L'DEPARTN),DEPARTN                                      
*                                                                               
         B     HEADX                                                            
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                  HEADUP AN OVERHEAD PAGE                      
HEADOVH  CLI   RCSUBPRG,1                                                       
         BE    HEADOV0                                                          
         CLI   RCSUBPRG,15                                                      
         BE    HEADOV0                                                          
         CLI   RCSUBPRG,18                                                      
         BE    HEADOV0                                                          
         CLI   RCSUBPRG,20                                                      
         BNE   HEADIND                                                          
HEADOV0  CLI   OPTSHOWO,C'Y'       SHOW DIRECT GRP FOR OVERHEAD PAGES?          
         BNE   *+16                                                             
         MVC   HEAD11+46(6),AC@GROUP                                            
         MVC   HEAD12+46(6),=C'------'                                          
         BAS   RE,POOLDEF          GET POOL DEFINITION                          
         CLI   LEVEL,COMP                                                       
         BE    HEADOV2             CORPORATE OVERHEAD                           
         BAS   RE,HEADOFF                                                       
         CLI   LEVEL,OFFICE                                                     
         BE    HEADOV2             OFFICE OVERHEAD                              
         BAS   RE,HEADDEP                                                       
HEADOV2  LA    R7,HEAD8                                                         
         MVC   HEAD8+1(L'AC@COVER),AC@COVER    OVERHEAD                         
         CLI   MODE,ACCLAST                                                     
         BE    HEADOV4                                                          
         MVC   HEAD8+12(2),=C'**'     ** UNALLOCATED POOL **                    
         MVC   HEAD8+15(L'AC@UNALL),AC@UNALL                                    
         LA    RF,HEAD8+15+L'AC@UNALL                                           
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   3(2,RF),=C'**'                                                   
         B     HEADOV6                                                          
HEADOV4  L     R2,ADACC                                                         
         LA    R2,L'CUL(R2)        BUMP PAST CUL                                
         SR    R0,R0                                                            
         IC    R0,LLEVB                                                         
         AR    R2,R0               BUMP PAST OFFICE AND DEPT                    
         SR    R1,R1                                                            
         IC    R1,LENLEVC          LENGTH OF SUB DEPT LEVEL                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD8+12(0),0(R2)   SUB DEP                                      
         SR    R1,R1                                                            
         IC    R1,LENLEVC                                                       
         LA    R1,1(R1)                                                         
         LA    R3,HEAD8+12                                                      
         AR    R3,R1               LEAVE A SPACE BETWEEN SUB AND EMPLY          
         L     R2,ADACC                                                         
         LA    R2,L'CUL(R2)        BUMP PAST CUL                                
         SR    R0,R0                                                            
         IC    R0,LLEVC            LENGTH OF OFF DPT SUB                        
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVD          LENGTH OF PERSON LEVEL                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       ACCOUNT                                      
         L     R2,ADACCNAM                                                      
         LA    R3,HEAD8+23                                                      
         BAS   RE,NAMOUT           NAME                                         
         GOTO1 ADSQUASH,DMCB,(0,HEAD8+12),60                                    
HEADOV6  B     HEADX                                                            
         EJECT                                                                  
HEADIND  CLI   RCSUBPRG,2                                                       
         BE    HEADIND0                                                         
         CLI   RCSUBPRG,14                                                      
         BE    HEADIND0                                                         
         CLI   RCSUBPRG,17                                                      
         BE    HEADIND0                                                         
         CLI   RCSUBPRG,19                                                      
         BNE   HEADSUM                                                          
HEADIND0 CLI   OPTSHOWD,C'Y'       SHOW DIRECT GRP FOR INDIRECT PAGES?          
         BNE   HEADIND2                                                         
         MVC   HEAD11+34(6),AC@GROUP                                            
         MVC   HEAD12+34(6),=C'------'                                          
HEADIND2 CLI   LEVEL,COMP                                                       
         BE    HEADIND4            CORPORATE INDIRECT PAGE                      
         BAS   RE,HEADOFF                                                       
         CLI   LEVEL,OFFICE                                                     
         BE    HEADIND4            OFFICE INDIRECT PAGE                         
         BAS   RE,HEADDEP                                                       
*                                                                               
HEADIND4 MVC   HEAD8+1(2),=C'**'    ** DIRECT/INDIRECT TIME TOTALS **           
         MVC   HEAD8+4(L'AC@DIRIT),AC@DIRIT                                     
         LA    RF,HEAD8+4+L'AC@DIRIT                                            
         CLI   0(RF),SPACE                                                      
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVC   3(2,RF),=C'**'                                                   
         BAS   RE,POOLDEF                                                       
         B     HEADX                                                            
*                                                                               
HEADSUM  CLI   RCSUBPRG,3                                                       
         BNE   HEADX               HEADUP AN OFFICE-DEPT SUMMARY PAGE           
         CLI   MYMODE,1                                                         
         BNE   HEADX                                                            
         MVC   HEAD5+21(5),=C'(   )'                                            
         MVC   HEAD5+22(3),AC@YTD                                               
HEADX    B     COM2XIT                                                          
         EJECT                                                                  
HEADOFF  NTR1                                                                   
         L     R2,ADHEIRA                                                       
         LA    R2,L'CUL(R2)                                                     
         SR    R1,R1                                                            
         IC    R1,LLEVA                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),NINES        OFFICE 9 IS NOT AN OFFICE                   
         BE    COM2XIT                                                          
         MVC   HEAD4+1(10),LLEVANAM                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD4+12(0),0(R2)    ***NOTE R1 AND R2 SET ABOVE***              
         L     R2,ADLVANAM                                                      
         LA    R3,HEAD4+16                                                      
         BAS   RE,NAMOUT            AND NAME                                    
         MVC   HEAD4+48(L'LIVEOPT),LIVEOPT                                      
         CLI   QOPT5,C'F'                      O/H F R O Z E N **               
         BNE   *+10                                                             
         MVC   HEAD4+48+L'LIVEOPT-2(L'AC@OHFRO),AC@OHFRO                        
         B     COM2XIT                                                          
*                                                                               
HEADDEP  NTR1                                                                   
         L     R2,ADHEIRB                                                       
         LA    R2,L'CUL(R2)                                                     
         SR    R0,R0                                                            
         IC    R0,LLEVA                                                         
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),NINES        DEPT 99 IS NOT A DEPT                       
         BE    COM2XIT                                                          
         MVC   HEAD5+1(10),LLEVBNAM                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+12(0),0(R2)   DEPARTMENT CODE (R1 N R2 SET ABOVE)          
         L     R2,ADLVBNAM                                                      
         LA    R3,HEAD5+16                                                      
         BAS   RE,NAMOUT           AND NAME                                     
         B     COM2XIT                                                          
         EJECT                                                                  
*-----------------------------------------------------                          
*              ROUTINE TO DEFINE POOL FOR HEADLINES                             
*-----------------------------------------------------                          
*                                                                               
*                                                                               
POOLDEF  NTR1                                                                   
         L     R5,APOLREC          ADDR OF IND OR OVH POOL ENTRY                
         LTR   R5,R5                                                            
         BZ    COM2XIT                                                          
         ZAP   DUB,ZEROS                                                        
         CLI   TYPE,ZERO                                                        
         BH    PLDF02                                                           
         LA    R0,7                HEAD3 TO HEAD9                               
         LA    R1,HEAD3+95         CLEAR POOL AREA OF HEADLINES                 
         MVC   0(25,R1),SPACES                                                  
         LA    R1,L'HEAD3(R1)                                                   
         BCT   R0,*-10                                                          
         B     COM2XIT                                                          
*                                                                               
PLDF02   MVC   POOLBYTE,TYPE                                                    
         NI    POOLBYTE,OHTYPE+INDITYPE                                         
         TM    POOLBYTE,OHTYPE               TYPE OF POOL                       
         BO    *+12                                                             
         CLI   QOPT7,C'B'          PRINTING TOTALS FOR ALL PTYPES ONLY          
         BNE   *+16                                                             
         MVC   HEAD7+85(L'AC@AMT),AC@AMT                                        
         MVC   HEAD7+85+L'AC@AMT+2(1),=C'=='                                    
         MVC   HEAD4+95(L'EMPLEDG),EMPLEDG   ORIGIN LEDGER                      
         TM    POOLBYTE,OHTYPE               TYPE OF POOL                       
         BO    PLDF06                                                           
         USING INDID,R5                                                         
*                                                                               
         L     RE,POLDEFN          POOL DEFINITION TABLE                        
         USING POOLD,RE                                                         
PLDF03   CLI   POOTYPE,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   POOLBYTE,POOTYPE                                                 
         BNE   PLDF04A                                                          
         CLI   POOLCAT,C'*'                                                     
         BNE   PLDF04                                                           
         MVC   BYTE,INDSTATS                                                    
         NC    BYTE,POOLSUB        CHECK STATUS BYTE OF IND POOL                
         BM    PLDF04B                                                          
PLDF04   CLC   INDTYPE,POOLCAT                                                  
         BE    PLDF04B                                                          
PLDF04A  LA    RE,POOLLN(RE)                                                    
         B     PLDF03                                                           
PLDF04B  MVC   HEAD3+95(L'POOLDES),POOLDES                                      
         MVC   HEAD4+95+L'EMPLEDG(L'INOFDPFM),INOFDPFM                          
         MVC   HEAD5+95(L'INDOFDEP),INDOFDEP            KEY                     
         CLI   QOPT7,C'B'          SHOW TOTALS FOR ALL PAYTYPES ONLY            
         BNE   PLDF10                                                           
         L     RE,COMPRAT          TBL OF DISP TO SAL TYP ACCUMS                
         USING CMPUTD,RE                                                        
PLDF04C  CLI   0(RE),EOT           END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                SALARY TYPE UNKNOWN                          
         CLC   CMBUKTYP,INDSALTP   COMPARE SAL TYPE                             
         BE    *+12                                                             
         LA    RE,CMPULN(RE)       NEXT SAL TYPE IN TABLE                       
         B     PLDF04C                                                          
         MVC   HEAD3+103(6),=C' (   )'                                          
         MVC   HEAD3+105(L'CMDESC),CMDESC                                       
         L     R1,APOLREC                                                       
         AH    R1,CMPTYIND                                                      
         ZAP   DUB,0(INBKLN,R1)                                                 
         B     PLDF10                                                           
         USING OVERD,R5                                                         
PLDF06   MVC   HEAD4+95+L'EMPLEDG(L'OVFRMACT),OVFRMACT                          
         SR    R1,R1                                                            
         IC    R1,LLEVB                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+95(0),OVACCT                       KEY                     
         ZAP   DUB,OVAYTD                                                       
         L     RE,POLDEFN          POOL DEFINITION TABLE                        
         USING POOLD,RE                                                         
PLDF07   CLI   POOTYPE,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   POOLBYTE,POOTYPE                                                 
         BNE   *+14                                                             
         CLC   OVHTYPE,POOLCAT                                                  
         BE    *+12                                                             
         LA    RE,POOLLN(RE)                                                    
         B     PLDF07                                                           
         MVC   HEAD3+95(L'POOLDES),POOLDES                                      
*                                                       AMOUNT                  
PLDF10   DS    0H                                                               
         TM    POOLBYTE,OHTYPE     OVHD PAGES DON'T PRINT NEW WAY               
         BO    PLDF10A                                                          
         CLI   QOPT7,C'B'          BREAKDOWN INDIR POOLS BY TYPE?               
         BNE   PLDF11                                                           
PLDF10A  CURED DUB,(11,HEAD7+95),2,MINUS=YES,ALIGN=LEFT                         
         B     PLDF11D                                                          
*                                                                               
         USING CMPUTD,R3                                                        
         USING PLPAYD,R4                                                        
PLDF11   L     R3,COMPRAT          TBL OF DISP TO SAL TYP ACCUMS                
         LA    R4,HEAD3+112                                                     
         LA    R2,4                3 PAYTYPES + TOTAL                           
PLDF11A  L     R1,APOLREC          INDIRECT POOL TABLE                          
         AH    R1,CMPTYIND                                                      
         ZAP   DUB,0(INBKLN,R1)                                                 
         CURED DUB,(11,PLPAYAMT),2,MINUS=YES,ALIGN=LEFT                         
         MVC   PLPAYDSC,CMDESC                                                  
         MVI   PLPAYEQ,C'='                                                     
         LA    R3,CMPULN(R3)                                                    
         LA    R4,L'HEAD4(R4)                                                   
         BCT   R2,PLDF11A                                                       
         DROP  R3,R4                                                            
*                                                                               
         USING SRTD,R3                                                          
PLDF11D  L     R3,ASRTWRK                                                       
         MVC   HEAD6+95(L'SRTCULAC),SRTCULAC            CONTRA                  
*                                                                               
         USING SCHDEFD,R3                                                       
         L     R3,SCHMDEF          SCHEME DEFINITION TABLE                      
PLDF12   CLI   SCHDEFN,EOT         END OF TABLE?                                
         BE    PLDF20                                                           
         CLC   SCHDEFN,SCHEDESC    MATCH SCHEME NUMBER                          
         BE    PLDF14                                                           
         LA    R3,SCHDEFLN(R3)                                                  
         B     PLDF12                                                           
*                                                                               
PLDF14   MVC   HEAD8+95(L'SCHDEF),SCHDEF         SCHEME LITERAL                 
         CURED SCHEDESC,(2,HEAD8+93),0,ALIGN=LEFT                               
         CLI   OFF1RLN,ONEBYTE                   ONE BYTE OFFICES?              
         BNE   PLDF16                                                           
         MVC   HEAD9+92(L'OFFG-1),OFFG           ONLY PRINT FIRST CHAR          
         CLC   OFFG(1),GRPDESC                                                  
         BNE   PLDF19                                                           
         B     PLDF20                                                           
PLDF16   MVC   HEAD9+92(L'OFFG),OFFG             OFFICE CODE                    
         CLC   OFFG,GRPDESC        THE SAME?                                    
         BNE   *+14                                                             
         CLC   GRPDESC+2(L'GRPDESC-2),SPACES                                    
         BE    PLDF20                                                           
PLDF19   MVC   HEAD9+95(L'GRPDESC),GRPDESC       GROUPINGS DESCRIPTION          
PLDF20   TM    POOLBYTE,INDITYPE       INDIRECT POOL CHECK FOR CLI LIST         
         BZ    PLDF24                                                           
         USING INDID,R5                                                         
         CLC   INDCLIST,SPACES                                                  
         BE    PLDF24                                                           
         LA    R4,HEAD10                                                        
         CLC   HEAD9+85(20),SPACES                                              
         BNE   *+8                                                              
         LA    R4,HEAD9                                                         
         MVC   85(L'AC@LIST,R4),AC@LIST                                         
         MVC   91(3,R4),INDCLIST                                                
         USING LISTD,RE                                                         
         L     RE,ALISTAB                                                       
         LA    R0,LISTMAX                                                       
PLDF22   CLC   INDCLIST,LISTCDE    LOOKUP LIST                                  
         BE    PLDF23                                                           
         LA    RE,LISTLEN(RE)                                                   
         BCT   R0,PLDF22                                                        
         DC    H'0'                                                             
PLDF23   MVC   95(15,R4),LISTNME   LIST NAME                                    
*                                                                               
PLDF24   B     COM2XIT                                                          
         DROP  R3,R5,RE                                                         
*                                                                               
         EJECT                                                                  
         USING NAMELD,R2                                                        
NAMOUT   LR    R0,RE                                                            
         LTR   R2,R2                                                            
         BZ    NAMOUTX                                                          
         MVC   0(36,R3),SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),NAMEREC                                                  
*                                                                               
NAMOUTX  LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
*------------------------------------------------                               
*              BUILD SUPPORTING LEDGERS TABLES                                  
*              TABLES INCLUDE :                                                 
*------------------------------------------------                               
*                                                                               
*                                                                               
VBLDTAB  DS    0H                                                               
*                                  *** BUILD 1N NON-CLT TABL ***                
*                                                                               
         L     R5,NONCLTBF         ADDR OF 1N NON-CLIENT TABLE                  
         USING BIND,R5                                                          
         XC    BININ,BININ                                                      
         SR    R3,R3                                                            
         LA    R4,BINTABLE         1ST TABLE ENTRY                              
         USING NOND,R4                                                          
         L     R6,RECORD           RECORD READING AREA                          
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                CLEAR THE KEY                       
         MVC   ACTKCPY,RCCOMPFL             COMPANY                             
         MVC   ACTKUNT(L'NONCLI),NONCLI     UNIT/LEDGER TO READ                 
         MVI   ACTKACT,X'41'                FORCE PAST LEDGER RECORD            
         MVC   CUL,ACTKCULA                                                     
         B     *+8                                                              
BLDT02   MVI   ACTKCULA+L'ACTKCULA,X'FF'                                        
         GOTO1 DATAMGR,DMCB,DMRDHI,DIR,RECORD,RECORD                            
         CLC   CUL,ACTKEY                                                       
         BNE   BLDT14                                                           
*                                  ALL AFTER ACCOUNT MUST BE SPACES             
*                                                                               
         CLC   ACTKCULA+L'ACTKCULA(L'ACTKEY-L'ACTKCULA),SPACES                  
         BNE   BLDT02              FORCE NEXT ACCOUNT                           
         MVC   DA,ACTKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,RECORD,DMWORK               
         TM    8(R1),ALL-PASSDEL                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   NOND(NONLEN),SPACES                                              
         MVC   NONCODE,ACTKACT     U/L ACCOUNT                                  
         MVI   NONSTAT,0                                                        
         MVI   NONLSTAT,0                                                       
         LA    R2,ACTRECD+(ACTRFST-ACTRECD)                                     
BLDT04   CLI   0(R2),0                                                          
         BE    BLDT12                                                           
         CLI   0(R2),NAMELQ                                                     
         BE    BLDT06                                                           
         CLI   0(R2),RSTELQ                                                     
         BE    BLDT08                                                           
         CLI   0(R2),FFTELQ                CLIENT LIST                          
         BE    BLDT10                                                           
BLDT05   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    BLDT04                                                           
         DC    H'0'                                                             
         USING NAMELD,R2                                                        
BLDT06   SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    BLDT05                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NONNAME(0),NAMEREC                                               
         B     BLDT05                                                           
*                                                                               
BLDT08   DS    0H                                                               
         USING RSTELD,R2                                                        
         MVC   NONANAL,RSTCOSTG    ANALYSIS= THE 1R LEVEL(O,D,C,P,L)            
         B     BLDT05                                                           
*                                                                               
BLDT10   DS    0H                                                               
         USING FFTELD,R2                                                        
         CLI   FFTTYPE,FFTTINDL    INDIRECT CLIENT LIST?                        
         BNE   BLDT05                                                           
         OI    NONSTAT,NONSLST     SET CLIENT LISTS PRESENT                     
         B     BLDT05                                                           
*                                                                               
BLDT12   CLI   NONANAL,SPACE                                                    
         BH    *+8                                                              
         MVI   NONANAL,C'D'        DEFAULT IS DEPARTMENTALLY                    
         CLC   NONNAME,SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE FOUND A NAME                       
         LA    R4,NONLEN(R4)                                                    
         A     R3,=F'1'                                                         
         C     R3,BINMAX                                                        
         BL    BLDT02                                                           
         DC    H'0'                TABLE IS FULL                                
BLDT14   ST    R3,BININ            UPDATE NUMBER IN TABLE                       
         EJECT                                                                  
*                                  *** READ 1C LEDGER RECORD ***                
*                                                                               
LDGREAD  L     R6,RECORD           RECORD READING AREA                          
         USING LDGRECD,R6                                                       
         MVI   OFFCLEN,ONEBYTE      SET DEFAULTS                                
         MVI   OFFDISP,00                                                       
         MVI   DPTLEN,00                                                        
         MVI   DEPDISP,00                                                       
*                                                                               
         MVC   LDGKEY,SPACES                CLEAR THE KEY                       
         MVC   LDGKCPY,RCCOMPFL             COMPANY                             
         MVC   LDGKUNT(L'CLILEG),CLILEG     UNIT/LEDGER TO READ                 
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,RECORD,RECORD                  
         TM    8(R1),0                   DID I FIND THE RECORD?                 
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,LDGKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,RECORD,DMWORK               
         TM    8(R1),0                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,LDGRECD+(LDGRFST-LDGRECD)                                     
*                                                                               
LDGR02   CLI   0(R2),0                                                          
         BE    COM2XIT                                                          
*                                                                               
         USING LDGELD,R2                                                        
         CLI   0(R2),LDGELQ        LEDGER ELEMENT?                              
         BE    LDGR06                                                           
         CLI   0(R2),ACLELQ        ACCOUNT LENGTHS                              
         BE    LDGR08                                                           
LDGR04   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    LDGR02              NEXT ELEMENT                                 
         DC    H'0'                DIE --- BAD ELEMENT LENGTH                   
*                                                                               
LDGR06   OC    LDGOPOS,LDGOPOS     OFFICE POSITION SPECIFIED?                   
         BZ    LDGR04                                                           
         TM    LDGOPOS,LDGONKHI    CHECK FOR OFFICE POSTION                     
         BNH   *+6                                                              
         DC    H'0'                CAN'T BE ANYWHERE BUT THE KEY                
         TM    LDGOPOS,LDGOKEY2    CHECK FOR NEW OFFICES                        
         BZ    *+8                 NOT ON NEW OFFICES                           
         MVI   OFFCLEN,TWOBYTE                                                  
         MVC   OFFDISP,LDGOPOS     ISOLATE DISP INTO KEY                        
         NI    OFFDISP,B'00001111'                                              
         SR    R1,R1                                                            
         IC    R1,OFFDISP                                                       
         SH    R1,=H'1'                                                         
         STC   R1,OFFDISP                                                       
*                                                                               
         OC    LDGDPOS,LDGDPOS     DEPT POSITION                                
         BZ    LDGR04                                                           
         MVC   DPTLEN,LDGDLEN      DEPT LENGTH                                  
         MVC   DEPDISP,LDGDPOS     DEPT POSITION                                
         NI    DEPDISP,B'00001111'                                              
         SR    R1,R1                                                            
         IC    R1,DEPDISP                                                       
         SH    R1,=H'1'                                                         
         STC   R1,DEPDISP                                                       
         B     LDGR04                                                           
*                                                                               
         USING ACLELD,R2                                                        
LDGR08   LA    R1,ACLVLEN                  1ST COMBINED LEVEL LENGTH S          
         LA    R0,CLEVNUM                  MAXIMUM NUMBER OF LEVELS             
         LA    RE,CLEVELS                                                       
         XC    LASTLEVD,LASTLEVD           DISP TO LAST LEVEL SET TO 0          
         XC    CLEVELS(CLEVLLN),CLEVELS                                         
LDGR10   DS    0H                                                               
         MVC   0(1,RE),0(R1)               COMBINED LEN IN CLEVELS              
         CLI   0(R1),MAXLEN                LAST LEV HAS MAXLEN FOR ACCT         
         BE    LDGR04                                                           
         MVC   LASTLEVD,0(R1)              DISP TO LAST LEVEL                   
         LA    R1,L'ACLVALS(R1)            BUMP TO NEXT COMBINED LENGTH         
         LA    RE,CLVALN(RE)                                                    
         BCT   R0,LDGR10                                                        
         DC    H'0'                                                             
         DROP  R2,R4,R5,R6,R7                                                   
         EJECT                                                                  
**********************************************************************          
*      READING 1C  - 1) BUILDING YTD-1 TABLE                         *          
*                    2) NEW BUSINESS TABLE                           *          
*      NOTE: THE YTD-1 TABLE IS BUILT FOR ALL REQUEST WHERE QSTART   *          
*            AND QEND ARE NOTEQUAL. THE NEW BUSINESS TABLE NEED ONLY *          
*            BE BUILT FOR THE FIRST REQUEST. ACCORDINGLY THE NEW     *          
*            BUSINESS BAS IS BRANCHED OVER FOR ALL OTHER REQUESTS.   *          
**********************************************************************          
         SPACE 1                                                                
VTABBLD  DS    0H                                                               
         L     R5,NEWBIZBF         ADDR OF 1C NEW BIZ TABLE                     
         USING BIND,R5                                                          
         LA    R1,BINTABLE                                                      
         ST    R1,ALTAB            ADDRESS OF NEXT TABLE ENTRY                  
         L     R1,BININ            IS TABLE ALREADY COMPLETE?                   
         LTR   R1,R1                                                            
         BZ    TABB01              NO  - BUILD IT THIS TIME THRU                
         MVI   TABBBC+1,BRANCH     YES - MAKE THE "BC" A "B" TO SKIP            
*                                        THE BAS TO BUILD IT.                   
*                                                                               
TABB01   MVC   STARTLED,=C'12'     START WITH INCOME                            
         CLI   INCOME,C'Y'         NEED TO READ INCOME?                         
         BE    *+10                                                             
         MVC   STARTLED,=C'14'     (SKIP 11 12 13)                              
         BAS   RE,CLERBUF          CLEAR BUFFALO RECORD                         
         MVC   NMEWRK,SPACES                                                    
*                                                                               
         L     R4,AYTDWRK          ADDR OF 1C YTD-1 WORK AREA                   
         USING YTD1D,R4                                                         
         MVC   YTD1D(YTDLEN),SPACES                                             
         L     R6,RECORD           RECORD READING AREA                          
         USING CACRECD,R6                                                       
*                                                                               
         MVC   CACKEY,SPACES                CLEAR THE KEY                       
         MVC   CACKCPY,RCCOMPFL             COMPANY                             
         MVC   CACKUNT(L'CLILEG),CLILEG     UNIT/LEDGER TO READ                 
         MVI   CACKACT,X'41'                FORCE PAST LEDGER RECORD            
         MVC   CUL,CACKEY                   SAVE COMP/UNIT/LEDGER               
TABB02   GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,RECORD,RECORD                  
         B     TABB02B                                                          
TABB02A  GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),DIR,RECORD,RECORD                  
TABB02B  CLC   CUL,CACKEY                                                       
         BE    TABB02C             STILL 1C THEN CONTINUE                       
         CLI   INCOME,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,INCPUT           PUT OUT LAST INCOME RECORD                   
         B     COM2XIT             WE'RE DONE                                   
*                                                                               
*                                  EVERYTHING AFTER ACCOUNT SPACES?             
TABB02C  CLC   CACKEY+L'CACKCULA(L'CACKEY-L'CACKCULA),SPACES                    
         BNE   TABB04              NO - CONTINUE                                
         MVC   DA,CACKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,RECORD,DMWORK               
         TM    8(R1),ALL-PASSDEL                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  *****SELF-MODIFIED INSTRUX********           
TABBBC   BC    0,TABB03            NEW BUSINESS TABL ONLY BUILT ONCE            
*                                  OTHERWISE THIS IS A BRANCH                   
         BAS   RE,NEWTST           NO - CHECK IF NEW BUSINESS CLIENT            
TABB03   LA    R2,CACKACT          ACCOUNT CODE                                 
         SR    R0,R0                                                            
         IC    R0,LASTLEVD         DISP TO LAST LEVEL                           
         AR    R2,R0                                                            
         CLI   0(R2),SPACE         IS THIS THE LAST LEVEL OF 1C ACCOUNT         
         BE    TABB02A             NO - SKIP TO NEXT ACCOUNT RECORD             
         CLI   INCOME,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,INCSET           SET UP INCOME BUFFALO RECORD                 
         MVC   CACKCCPY,RCCOMPFL   YES - START WITH 12 OR 14 CONTRA             
         MVC   CACKCUNT(2),STARTLED                                             
         B     TABB02              READ HIGH                                    
*                                                                               
*                                                                               
TABB04   DS    0H                  FILTER DIRECTORY KEY                         
*&&US                                                                           
         CLC   CACKOFF,SPACES      IF WE REACH OFFICE RECORDS WE'RE             
         BNE   TABB04A             DONE WITH THE HISTORY FOR THIS ACCT          
*&&                                                                             
         OC    CHDKNULL-CHDRECD(L'CHDKNULL,R6),CHDKNULL-CHDRECD(R6)             
         BZ    TABB02A             SKIP CONTRA HEADERS                          
         CLC   =C'13',CACKCUNT     (SKIP 13)                                    
         BNE   TABB04A                                                          
         MVC   CACKEY+L'CACKCULA(L'CACKEY-L'CACKCULA),SPACES                    
         MVC   CACKCCPY,RCCOMPFL   START WITH 14 CONTRA                         
         MVC   CACKCUNT(2),=C'14'                                               
         B     TABB02              READ HIGH                                    
*                                  ***  NEW OFFICES ***                         
TABB04A  CLC   CACKOFF,SPACES      IF WE REACH OFFICE RECORDS WE'RE             
         BE    TABB05              DONE WITH THE HISTORY FOR THIS ACCT          
         XC    CACKEY+L'CACKCULA(L'CACKEY-L'CACKCULA),CACKEY+L'CACKCULA         
         SR    R1,R1                                                            
         IC    R1,CACKACT+L'CACKACT-1                                           
         AH    R1,=H'1'                                                         
         STC   R1,CACKACT+L'CACKACT-1                                           
         B     TABB02              READ HIGH RECORD                             
*                                  ***  OLD OFFICES ***                         
TABB05   CLC   CACKSPAC,SPACES     IF WE REACH TRANSDATE WE'RE                  
         BE    TABB06              DONE WITH THE HIST FOR THIS CONTRA           
         XC    CACKSPAC(CACKEND-(CACKSPAC-CACRECD)),CACKSPAC                    
         SR    R1,R1                                                            
         IC    R1,CACKCACT+L'CACKCACT-1                                         
         AH    R1,=H'1'                                                         
         STC   R1,CACKCACT+L'CACKCACT-1                                         
         B     TABB02              READ HIGH RECORD                             
*                                                                               
TABB06   CLC   =C'12',CACKCUNT               NO METHOD ON INCOME                
         BE    *+14                                                             
         CLC   CACKBTYP(L'METHOD),METHOD     MATCH ON BT TYPE METHOD            
         BNE   TABB02A                       NO - READ SEQ NEXT RECORD          
         MVC   DA,CACKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,RECORD,DMWORK               
         TM    8(R1),ALL-PASSDEL                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'12',CACKCUNT               INCOME                             
         BE    TABB30                                                           
*                                                                               
         MVC   YTDACC,CACKACT      1C ACCOUNT                                   
         MVC   YTDCNTRA,CACKCLDG   CONTRA LEDGER/ACCOUNT                        
         MVC   YTDBTYP,CACKSTYP    BUCKET TYPE                                  
         XC    YTDSTAT,YTDSTAT     CLEAR STATUS                                 
         ZAP   YTD1AMT,ZEROS                                                    
*                                                                               
         LA    R2,CACRECD+(CACRFST-CACRECD)                                     
         USING BUKELD,R2                                                        
TABB12   CLI   0(R2),0                                                          
         BE    TABB22                                                           
         CLI   0(R2),BUKELQ        HISTORY BUCKET?                              
         BE    TABB16                                                           
TABB14   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BNZ   TABB12              NEXT ELEMENT                                 
         DC    H'0'                DIE --- BAD ELEMENT LENGTH                   
*                                                                               
TABB16   LA    R1,START            USE REQUEST START                            
         CLC   =C'16',CACKCUNT     OVERHEAD?                                    
         BNE   *+8                                                              
         LA    R1,STOVH            USE OVERHEAD START(FISCAL OR START)          
         CLC   BUKMOS,0(R1)        BETWEEN START AND (END-1)                    
         BL    TABB14                                                           
         CLC   BUKMOS,END          SET BIT IF CURRENT MTH POSTING               
         BNE   TABB20                                                           
         CP    BUKCR,ZEROS         EXCEPT IF IT'S ZERO                          
         BE    TABB14                                                           
         OI    YTDSTAT,YTD1MTH                                                  
         B     TABB14              NEXT EL                                      
TABB20   CLC   BUKMOS,LAST                                                      
         BH    TABB14                                                           
         AP    YTD1AMT,BUKCR       ADD UP CREDITS                               
         B     TABB14              GET NEXT ELEMENT                             
*                                                                               
TABB22   CP    YTD1AMT,ZEROS       DON'T PUT ZERO RECORDS TO TABLE              
         BNE   TABB24              UNLESS IT HAD CURRENT MTH POSTINGS           
         TM    YTDSTAT,YTD1MTH                                                  
         BO    TABB24                                                           
         MVC   YTD1D(YTDKLEN),SPACES                                            
         B     TABB02A             SEQ READ                                     
*                                                                               
*ABB24   L     R1,TSARBLK1                                                      
*        USING TSARD,R1                                                         
*        ST    R4,TSAREC           RECORD TO BE WRITTEN                         
*        MVI   TSOFFACT,TSAADD     ACTION ADD                                   
*        GOTO1 ATSAROFF                                                         
*        BE    TABB02A             SEQ READ                                     
*        DC    H'0'                TABLE FULL                                   
*                                                                               
TABB24   LA    R0,BBUFFAPUT         PUT TABLE ENTRY TO BUFFERIN                 
         L     RF,=A(YTDBUF)                                                    
         GOTO1 BUFFERIN,DMCB,((R0),(RF)),(0,(R4)),ADCOMFAC                      
         CLI   4(R1),0                                                          
         BE    TABB02A                                                          
         DC    H'0'                                                             
*                                  *** INCOME BUCKETS ***                       
         USING BUFD,R5                                                          
TABB30   L     R5,ABUFWRK                                                       
         LA    R2,CACRECD+(CACRFST-CACRECD)                                     
         USING BUKELD,R2                                                        
TABB36   CLI   0(R2),0                                                          
         BE    TABB02A             SEQ READ                                     
         CLI   0(R2),BUKELQ        HISTORY BUCKET?                              
         BE    TABB40                                                           
TABB38   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BNZ   TABB36              NEXT ELEMENT                                 
         DC    H'0'                DIE --- BAD ELEMENT LENGTH                   
*                                                                               
TABB40   CLC   BUKMOS,START        BETWEEN START AND (END)                      
         BL    TABB42                                                           
         CLC   BUKMOS,END                                                       
         BH    TABB38                                                           
         AP    BDIRYTD,BUKDR       YTD DR'S                                     
         CLC   BUKMOS,LAST                                                      
         BH    TABB38                                                           
         AP    BDIRLST,BUKDR       YTD-1 DR'S                                   
*                                                                               
*                                  (IN CASE ALLOC DIR MTHLY AND OH YTD)         
TABB42   CLI   OPTOVHY,C'Y'        BETWEEN FISCAL AND (END-1)                   
         BNE   TABB38                                                           
         CLC   BUKMOS,FISCAL       BETWEEN FISCAL AND (END-1)                   
         BL    TABB38              (IN CASE ALLOC DIR MTHLY AND OH YTD)         
         CLC   BUKMOS,LAST                                                      
         BH    TABB38                                                           
         AP    BDIRLSTM,BUKDR      YTD-1 DR'S                                   
         B     TABB38              GET NEXT ELEMENT                             
         DROP  R2,R4,R5,R6                                                      
         EJECT                                                                  
*******************************************************************             
*        SET UP INCOME BUFFALO RECORD                                           
*******************************************************************             
*                                                                               
         USING BUFD,R5                                                          
         USING CACRECD,R6                                                       
INCSET   NTR1                                                                   
         L     R5,ABUFWRK                                                       
         L     R6,RECORD                                                        
         BAS   RE,INCPUT                PUT OUT PREVIOUS INCOME REC             
*                                                                               
         USING BIND,R3                                                          
         L     R3,NEWBIZBF              NB PB OR HOUSE TABLE                    
         L     R0,BININ                                                         
         LTR   R0,R0                                                            
         BZ    INCS04                                                           
         BP    *+6                                                              
         DC    H'0'                                                             
         USING NEWBD,RF                                                         
         LA    RF,BINTABLE              TABLE                                   
*                                                                               
INCS02   SR    R1,R1                                                            
         IC    R1,NEWBCLN               LNGTH OF TABLE ENTRY MINUS 1            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NEWBCDE(0),CACKACT       IS IT A MATCH                           
         BE    COM2XIT                  YES - NO NEED TO LOOKUP INCOME          
         LA    RF,NEWBLEN(RF)           NO  - BUMP TABLE                        
         BCT   R0,INCS02                TRY AGAIN FOR A MATCH                   
*                                                                               
INCS04   MVI   BUFTYPE,BUFINCM          CLIENT INCOME                           
         MVC   BUFCLI,CACKLDG           CLIENT CODE                             
         MVC   BUFGCOD,SPACES                                                   
         LA    RE,BUFCLT                                                        
         SR    R0,R0                                                            
         IC    R0,OFFDISP               DISPLACEMENT OF OFFICE                  
         AR    RE,R0                                                            
         SR    R1,R1                                                            
         IC    R1,OFFCLEN               LENGTH OF OFFICE LEVEL                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUFGCOD(0),0(RE)         OFFICE OF CLIENT                        
*                                                                               
         LA    R2,CACRECD+(CACRFST-CACRECD)                                     
         USING NAMELD,R2                                                        
INCS06   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),NAMELQ        NAME?                                        
         BE    INCS08                                                           
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     INCS06              NEXT ELEMENT                                 
*                                                                               
INCS08   LA    RF,NMEWRK                                                        
         USING BCLID,RF                                                         
         MVC   BCLICDE,BUFCLT      SAVE CLT CODE AND NAME                       
         LA    R3,BCLINME                                                       
         BAS   RE,NAMOUT                                                        
         B     COM2XIT                                                          
         DROP  R2,R3,R5,R6,RF                                                   
         EJECT                                                                  
*******************************************************************             
*        PUT INCOME BUFFALO RECORD                                              
*******************************************************************             
*                                                                               
         USING BUFD,R5                                                          
INCPUT   NTR1                                                                   
         L     R5,ABUFWRK                                                       
         LA    R1,BUFBK            DON'T PUT ZERO RECS TO BUFF                  
         LA    R0,BBKCNT                                                        
         CP    0(BUFBKLN,R1),ZEROS                                              
         BNZ   INCP02                                                           
         LA    R1,BUFBKLN(R1)                                                   
         BCT   R0,*-14                                                          
         B     INCPXX                                                           
INCP02   DS    0H                                                               
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFC,(R5)                           
         XC    BUFCLI,BUFCLI       TOTAL RECORD BY CLIENT OFFICE                
         BASR  RE,RF                                                            
         XC    BUFGRPCD,BUFGRPCD   TOTAL AGENCY INCOME                          
         BASR  RE,RF                                                            
         LA    R4,NMEWRK                                                        
         GOTO1 BINADD,DMCB,(R4),NMEBUFF                                         
INCPXX   MVC   NMEWRK,SPACES                                                    
         BAS   RE,CLERBUF                                                       
         B     COM2XIT                                                          
         EJECT                                                                  
*******************************************************************             
*        CLEAR INCOME BUFFALO RECORD                                            
*******************************************************************             
*                                                                               
         USING BUFD,R5                                                          
CLERBUF  NTR1                                                                   
         L     R5,ABUFWRK                                                       
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         LA    R1,BUFBK                 CLEAR BUFFALO BUCKETS                   
         LA    R0,BBKCNT                                                        
         ZAP   0(BUFBKLN,R1),ZEROS                                              
         LA    R1,BUFBKLN(R1)                                                   
         BCT   R0,*-10                                                          
         B     COM2XIT                                                          
         EJECT                                                                  
*******************************************************************             
*        BUILD 1C NEW BUS TABL                                                  
*******************************************************************             
*                                                                               
*                                                                               
NEWTST   NTR1                                                                   
         L     R2,RECORD                                                        
         AH    R2,=Y(ACTRFST-ACTRECD) FIRST ELEMENT                             
NEWT02   CLI   0(R2),0                                                          
         BE    COM2XIT                                                          
         USING RSTELD,R2                                                        
         CLI   0(R2),RSTELQ        RECORD STATUS ELEMENT?                       
         BE    NEWT04                                                           
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    NEWT02              NEXT ELEMENT                                 
         DC    H'0'                DIE --- BAD ELEMENT LENGTH                   
*                                  IS IT NEWBIZ PROBONO OR HOUSE?               
NEWT04   DS    0H                                                               
         MVI   BYTE,0                                                           
*&&US                                                                           
         CLI   RSTLN,RSTLN2Q                                                    
         BNH   COM2XIT                                                          
         TM    RSTSTAT5,RSTSNBIZ+RSTSBONO+RSTSHOUS                              
         BZ    COM2XIT             NO -                                         
         MVC   BYTE,RSTSTAT5                                                    
         NI    BYTE,RSTSNBIZ+RSTSBONO+RSTSHOUS                                  
*&&                                                                             
*&&UK                                                                           
         CLI   RSTLN,RSTLN2Q                                                    
         BL    COM2XIT                                                          
         TM    RSTSTAT4,RSTSCSTH+RSTSCSTN                                       
         BNM   COM2XIT             BOTH ON OR BOTH OFF MEANS CLT                
         TM    RSTSTAT4,RSTSCSTH                                                
         BZ    *+12                                                             
         OI    BYTE,NEWBHS         SET TO HOUSE                                 
         B     *+8                                                              
         OI    BYTE,NEWBNB                                                      
*&&                                                                             
*                                                                               
         L     R5,NEWBIZBF         ADDR OF 1C NEW BIZ TABLE                     
         USING BIND,R5                                                          
         L     R4,ALTAB            ADDRESS OF NEXT TABLE ENTRY                  
         USING NEWBD,R4                                                         
         L     R3,BININ            USE R3 AS COUNTER                            
         L     R6,RECORD           ADDRESS OF THE RECORD JUST READ              
         USING ACTRECD,R6                                                       
         XC    NEWBD(NEWBLEN),NEWBD                                             
         MVC   NEWBCDE,ACTKACT     ACCOUNT                                      
         MVC   NEWBSTAT,BYTE                                                    
*                                                                               
         LA    R1,NEWLEN           MAX LNGTH OF N.B. TABLE ENTRY                
         LA    RF,NEWBCLST         ADDR OF LAST BYTE OF NEW BIZ CODE            
         CLI   0(RF),SPACE         IS IT A SPACE                                
         BNE   *+12                NO  - R1 CONTAINS LENGTH                     
         BCTR  RF,0                YES - THEN REDUCE R2 BY 1                    
         BCT   R1,*-10             TRY AGAIN                                    
         DC    H'0'                ERROR TABLE HAS A BLANK ENTRY                
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         STC   R1,NEWBCLN          SAVE LENGTH -1 FOR LATER COMPARE             
*                                                                               
         LA    R4,NEWBLEN(R4)      SET FOR NEXT ENTRY IN TABLE                  
         ST    R4,ALTAB            SAVE THE ADDR OF THE NEXT ENTRY              
         A     R3,=F'1'            UPDATE COUNTER                               
         C     R3,BINMAX           COMPARE TO MAX ENTRIES                       
         BL    *+6                 GO READ NEXT RECORD                          
         DC    H'0'                TABLE IS FULL                                
         ST    R3,BININ            UPDATE NUMBER IN TABLE (BININ)               
         B     COM2XIT                                                          
         DROP  R2,R4,R5,R6                                                      
         EJECT                                                                  
*----------------------------------------------------                           
*        SEARCH FOR RECORDS FROM BIN TABLES                                     
*               PARM 1   ADDR OF KEY FOR LOOKUP                                 
*               PARM 2   ADDR OF BIND PARMS                                     
*               PARM 3   ADDR OF SAVE AREA FOR RECS                             
*----------------------------------------------------                           
*                                                                               
VBINSRCH DS    0H                                                               
         USING BIND,R4                                                          
*                                                                               
         LM    R3,R5,0(R1)                                                      
*                                   R3 - A(ITEM)                                
*                                   R4 - ADDR OF BIND PARMS                     
*                                   R5 - ADDR OF WHERE TO PUT THE REC           
*                                   NUMBER,LENGTH,KEY MAX (3RD PARM)            
         MVC   DMCB+8(BINKLEN),BININ                                            
         LA    R2,BINTABLE          A(TABLE)                                    
         GOTO1 BINSRCH,DMCB,(X'00',(R3)),(R2)                                   
         CLI   DMCB,0                                                           
         BE    *+6                  NOT FOUND                                   
         DC    H'0'                                                             
         LTR   R5,R5                                                            
         BZ    COM2XIT              JUST PASS BACK ADDR OF ENTRY                
         L     R2,DMCB              ADDR OF RECORD FOUND                        
         L     R1,BINLEN            LENGTH OF THE RECORD                        
         BCTR  R1,0                 REDUCED FOR THE EX                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R2)                                                    
         B     COM2XIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO ADD TO A BINSRCH TABLE                                  *          
*         PARAM1              A(RECORD TO BE ADDED)                  *          
*         PARAM2              A(BINSRCH PARAMS)                      *          
**********************************************************************          
         SPACE 1                                                                
VBINADD  DS    0H                                                               
         USING BIND,R5                                                          
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   BIN001                                                           
         C     R5,ERRORBUF         DIE UNLESS ERROR TABLE FULL                  
         BE    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         OI    RUNSTAT,POSTERR     SET SO NOTHING POSTS                         
BIN001   MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R6,R6                                                            
         IC    R6,BINFRST          DISP. TO FIRST BUCKET                        
         LTR   R6,R6                                                            
         BZ    BINXIT              NO BUCKETS FOR THIS TABLE                    
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         SR    R0,R0                                                            
         IC    R0,BINNUMB          NUMBER OF BUCKETS                            
         LTR   R0,R0                                                            
         BZ    BINXIT              NO BUCKETS FOR THIS TABLE                    
         TM    BINSTAT,BINSIXB     6 BYTE ACCUMS                                
         BO    BIN002                                                           
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,*-14                                                          
         B     BINXIT                                                           
BIN002   AP    0(6,R4),0(6,R3)                                                  
         LA    R4,6(R4)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,*-14                                                          
*                                                                               
BINXIT   B     COM2XIT                                                          
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO ADD TO A BINSRCH TABLE                                  *          
*         PARAM1              A(RECORD TO BE ADDED)                  *          
*         PARAM2              A(BINSRCH PARAMS)                      *          
**********************************************************************          
         SPACE 1                                                                
VBIN31   DS    0H                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         USING BIND,R5                                                          
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         MVI   DMCB+12,X'01'                                                    
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRC31,DMCB,(R3),(R2)                                          
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         TM    DMCB,X'80'          WAS RECORD FOUND                             
         BO    BIN31X              YES                                          
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R6,R6                                                            
         IC    R6,BINFRST          DISP. TO FIRST BUCKET                        
         LTR   R6,R6                                                            
         BZ    BIN31X              NO BUCKETS FOR THIS TABLE                    
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         SR    R0,R0                                                            
         IC    R0,BINNUMB          NUMBER OF BUCKETS                            
         LTR   R0,R0                                                            
         BZ    BIN31X              NO BUCKETS FOR THIS TABLE                    
         TM    BINSTAT,BINSIXB     6 BYTE ACCUMS                                
         BO    BIN3110                                                          
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,*-14                                                          
         B     BIN31X                                                           
BIN3110  AP    0(6,R4),0(6,R3)                                                  
         LA    R4,6(R4)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,*-14                                                          
*                                                                               
BIN31X   DS    0H                                                               
*        LA    RF,*+6                                                           
*        BSM   0,RF                24 BIT MODE                                  
         B     COM2XIT                                                          
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* CLEAR CLIENT WORK AREA                                             *          
**********************************************************************          
         SPACE 1                                                                
VCLERWRK DS    0H                                                               
         L     R2,ACLIWRK                INITIALIZE CLIENT RECORD               
         USING CLID,R2                                                          
         MVC   CLID(CLIDLEN),SPACES                                             
         MVI   CLISTAT,X'00'                                                    
         LA    R1,CLIBK                                                         
         LA    R0,CBKCNT2                                                       
         ZAP   0(CBUKLEN,R1),ZEROS       CLEAR CLIENT BUCKETS                   
         LA    R1,CBUKLEN(R1)                                                   
         BCT   R0,*-10                                                          
         B     COM2XIT                                                          
         DROP  R2                                                               
         EJECT                                                                  
*-------------------------------------------------------------------            
*               GET AMOUNT OF COST (DIR,IND OR OVH) FOR THIS CLIENT             
*                                                                               
*               CLITIME = THIS CLIENTS DIRECT TIME                              
*               ALLOC   = TOTAL AMOUNT TO BE ALLOCATED                          
*               TOTIME  = TOTAL DIRECT TIME FOR THIS LEVEL OR SCHEME            
*-------------------------------------------------------------------            
*                                                                               
VDODIVD  DS    0H                                                               
         ZAP   DIVWRK,CLITIME      THIS CLIENT TIME                             
         MP    DIVWRK,ALLOC+2(6)   AMOUNT TO BE ALLOCATED                       
         MP    DIVWRK,=P'10'                                                    
         CP    TOTIME,ZEROS                                                     
         BNE   *+14                                                             
         ZAP   DUB,ZEROS                                                        
         B     COM2XIT                                                          
         DP    DIVWRK,TOTIME+2(6)  TOTAL CLIENT TIME                            
         SRP   DIVWRK(10),64-1,5                                                
         ZAP   DUB,DIVWRK+2(8)     ALLOCATED AMOUNT RETURNED IN DUB             
         B     COM2XIT                                                          
         EJECT                                                                  
*------------------------------------------------------                         
*        CLEAR SORT RECORD AREA                                                 
*------------------------------------------------------                         
*                                                                               
VCLERSRT DS    0H                                                               
         USING SRTD,R2                                                          
         L     R2,0(R1)                  A(RECORD)                              
         MVC   SRTD(SRTLEN),SPACES                                              
         MVC   SRTBTYP,SPACES            BUCKET TYPE (SPACE FOR NOW)            
         MVC   SRTMETHD,METHOD                                                  
         MVI   SRTSTAT,0                                                        
         MVC   SRTMOS,MOS                BUCKET YYMM                            
         MVC   SRTCOMP,RCCOMPFL          COMPANY                                
         MVI   SRTUNIT,ONE               UNIT                                   
         MVC   SRTCCMPU,SRTCOMP          SAME FOR CONTRA                        
         LA    R1,SRTBUKS                START OF COL BUCKETS                   
         LA    R0,SRTBKNUM               NUMBER OF BUCKETS INTO R0              
*                                                                               
         ZAP   0(L'SRTBUKS,R1),ZEROS     CLEAR TO PACKED ZEROS                  
         LA    R1,L'SRTBUKS(R1)          BUMP TO NEXT BUCKET                    
         BCT   R0,*-10                                                          
         B     COM2XIT                                                          
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------------                     
*        PRINT ERROR AND WARNING REPORTS                                        
*----------------------------------------------------------                     
*                                                                               
VERRREPT DS    0H                                                               
         MVI   FORCEHED,C'Y'       FORCE HEADING                                
         USING BIND,R5                                                          
         NI    BIT,X'FF'-(COLTWO+COLTHREE)                                      
         L     R5,ERRORBUF                                                      
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    COM2XIT                                                          
*                                                                               
         USING ERDEFD,R1                                                        
         L     R1,ERRDEF                                                        
ERRR00   CLI   ERDFNM,EOT          END OF TABLE NUMBER                          
         BE    ERRR01                                                           
         NI    ERDSTAT,ALL-ERDFCHED      RESET NEW PAGE SWITCH                  
         LA    R1,ERDFLN(R1)                                                    
         B     ERRR00                                                           
*                                                                               
         USING ERRORD,R7                                                        
ERRR01   LA    R7,BINTABLE                                                      
ERRR02   L     R1,ERRDEF                                                        
ERRR04   CLI   ERDFNM,EOT          END OF TABLE NUMBER                          
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN ERROR NUMBER                         
         CLC   ERRNUM,ERDFNM       MATCH ERROR NUMBER                           
         BE    ERRR08                                                           
         LA    R1,ERDFLN(R1)                                                    
         B     ERRR04                                                           
*                                                                               
ERRR08   MVC   RCSUBPRG,ERDRCPRG   RCSUBPRG FOR TITLE IN 01 PHASE               
         TM    ERDSTAT,ERDHEDUP    FORCE NEW REPORT HEADING?                    
         BNO   ERRR10                                                           
         TM    ERDSTAT,ERDFCHED    DID I ALREADY START NEW REPORT?              
         BO    ERRR10                                                           
         OI    ERDSTAT,ERDFCHED    MARK IT STARTED                              
         TM    BIT,COLTWO+COLTHREE IF THIS IS ON I SUPPRESSED PRINTING          
         BZ    ERRR09              UNTIL I PROCESSED THREE ACCOUNTS             
         GOTO1 ACREPORT            FORCE TO PRINT BEFORE I START                
ERRR09   MVI   FORCEHED,C'Y'       A NEW SECTION                                
         GOTO1 ACREPORT                                                         
         NI    BIT,X'FF'-(COLTWO+COLTHREE)                                      
*                                                                               
         USING ERLIND,R2                                                        
ERRR10   LA    R2,P+1                                                           
         TM    BIT,COLTWO          PRINTING THE 2ND COLUMN                      
         BZ    *+8                                                              
         LA    R2,P+40                                                          
         TM    BIT,COLTHREE        PRINTING THE 3RD COLUMN                      
         BZ    *+8                                                              
         LA    R2,P+80                                                          
         CLI   ERRTYPE,ERRWARN                                                  
         BE    ERRR12                                                           
         MVC   ERLNDF,ERDESCRP                                                  
         MVC   ERLNAC,ERRACCT                                                   
         CLC   ERRAMNT,SPACES                                                   
         BE    ERRR18                                                           
         ZAP   DOUBLE,ERRAMNT                                                   
         SRP   DOUBLE,64-2,5                                                    
         CURED DOUBLE,(L'ERLNAMT,ERLNAMT),0,MINUS=YES                           
         B     ERRR18                                                           
*                                                                               
ERRR12   MVC   ERRACNT,ERRACCT                                                  
         CLI   QOPT2,C'S'          IF SUPPRESS EMPL, NO PAGE NUMBERS            
         BE    ERRR18                                                           
         MVC   ERRPAGE,=C'PAGE'                                                 
ERRR14   CLC   ERRAMNT,SPACES                                                   
         BE    ERRR18                                                           
         ZAP   DOUBLE,ERRAMNT                                                   
         SRP   DOUBLE,64-2,5                                                    
         CURED DOUBLE,(L'ERRAMT,ERRAMT),0,MINUS=YES                             
ERRR18   DS    0H                                                               
         CLC   PAGE,SVPAGE         STILL ON THE SAME PAGE?                      
         BE    *+8                                                              
         NI    BIT,X'FF'-(COLTWO+COLTHREE)  SET TO START IN 1ST COLUMN          
         CLI   ERRNUM,ERRDATES     IS THIS THE WARNING ABOUT REQ DATES?         
         BE    ERRR19              TREAT LIKE AN ERROR INSTEAD                  
         CLI   ERRTYPE,ERRWARN     PRINTING A WARNING?                          
         BE    ERRR20              NO-DON'T PRINT IN TWO COLUMNS                
ERRR19   GOTO1 ACREPORT                                                         
         B     ERRR50                                                           
ERRR20   TM    BIT,COLTWO          HAVE WE ALREADY PRINTED THE 2ND COL?         
         BO    *+12                                                             
         TM    BIT,COLTHREE                                                     
         BZ    ERRR40                                                           
         NI    BIT,X'FF'-COLTWO                                                 
         TM    BIT,COLTHREE        HAVE WE ALREADY PRINTED THE 3RD COL?         
         BZ    ERRR45                                                           
         NI    BIT,X'FF'-COLTHREE                                               
         GOTO1 ACREPORT                                                         
         B     ERRR50                                                           
ERRR40   OI    BIT,COLTWO          INDICATES PRINTING THE 2ND COLUMN            
         B     ERRR50                                                           
ERRR45   OI    BIT,COLTHREE                                                     
ERRR50   MVC   SVPAGE,PAGE         SAVE CURRENT PAGE NUMBER                     
         LA    R7,ERRLEN(R7)                                                    
         BCT   R3,ERRR02                                                        
*                                                                               
         GOTO1 ACREPORT            FORCE ANY LEFTOVER ERROR MESSAGES            
         B     COM2XIT                                                          
         DROP  R1,R2,R5,R7                                                      
         EJECT                                                                  
*----------------------------------------------------------                     
*        PRINT RECORDS ADDED REPORT FOR DDS DATA CONTROL                        
*----------------------------------------------------------                     
*                                                                               
VRECREPT DS    0H                                                               
*&&UK                                                                           
         CLI   RCWRITE,C'N'          WRITE=NO WILL SKIP REPORT INUK             
         BE    COM2XIT                                                          
*&&                                                                             
         MVI   RCREQSUM,C'N'                                                    
         USING LOGOD,R3                                                         
         L     R3,LOGOC                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 LOGO,DMCB,LOGOD                                                  
*                                                                               
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     R4,MCVREMOT                                                      
         USING REMOTED,R4                                                       
*&&UK*&& OC    REMOTKEY,REMOTKEY                                                
*&&UK*&& BZ    RECREP05                                                         
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         XC    REMOTKEY,REMOTKEY     TOTAL PAGE GOES TO DATA CONTROL            
*&&US                                                                           
         MVI   REMOTJID,C'A'                                                    
         MVC   REMOTJID+1(2),QPROG                                              
         MVI   REMOTCLS,C'T'                                                    
         MVI   REMOTCPY,X'01'                                                   
         MVC   REMOTDST,=AL2(0043)   TCH1\ID                                    
         CLI   RCWRITE,C'N'          WRITE=NO GOES TO TCH1 ID                   
         BE    RECREP04                                                         
         TM    UPSI,COUNTREP         SO DOES UPSI SETTING                       
         BO    RECREP04                                                         
         MVC   REMOTDST,=AL2(1106)   DATA CONTROL'S ZIP ID                      
RECREP04 MVC   REMOTKEY(8),MCJOB                                                
         MVC   REMOTFNO,REMOTKEY                                                
         MVI   REMOTPRT,0                                                       
*&&                                                                             
*                                                                               
RECREP05 DS    0H                                                               
*&&UK                                                                           
         MVI   LOGOEND,C'X'                                                     
         MVI   LOGOTYPE,C'S'                                                    
         MVC   LOGONAME,=CL33'******** INTERNAL CONTROL *******'                
         MVC   LOGOADD,=CL33'******** DO NOT SEND OUT ******'                   
         MVC   LOGO1,=C'CONTROL'                                                
         GOTO1 LOGO,DMCB,LOGOD                                                  
*&&                                                                             
         MVI   FORCEHED,C'Y'         FORCE HEADING                              
         MVI   RCSUBPRG,9                                                       
         GOTO1 HEADUP                SET-UP HEADLINES                           
         USING CONTD,R5                                                         
         L     R5,CONTTAB                                                       
         LA    R6,ACNTNUM                                                       
         LTR   R6,R6                                                            
         BZ    COM2XIT                                                          
*                                                                               
         LA    R2,P+1                                                           
         USING CONTPD,R2                                                        
RECREP06 DS    0H                                                               
         CURED CONTSIZ,(L'CONTSIZP,CONTSIZP),0                                  
         CURED CONTER,(L'CONTERP,CONTERP),0                                     
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT            SKIP LINE                                    
         LA    R5,CONTLNQ(R5)                                                   
         BCT   R6,RECREP06                                                      
*&&UK                                                                           
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 LOGO,DMCB,LOGOD                                                  
*&&                                                                             
         B     COM2XIT                                                          
         DROP  R2,R3,R4,R5,RF                                                   
         EJECT                                                                  
*----------------------------------------------------------                     
*        BOX HOOK ROUTINE                                                       
*----------------------------------------------------------                     
*                                                                               
VBXHOOK  DS    0H                                                               
*                                                                               
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX                                                         
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         CLI   RCSUBPRG,0          PERSON PAGE                                  
         BE    BXX                 HANDLE DURING REPORT PRINTING                
*                                                                               
BX10     CLI   RCSUBPRG,2          DIRECT/INDIRECT PAGES                        
         BE    BX20                                                             
         CLI   RCSUBPRG,14                                                      
         BE    BX20                                                             
         CLI   RCSUBPRG,17                                                      
         BE    BX20                                                             
         CLI   RCSUBPRG,19                                                      
         BE    BX20                                                             
*                                                                               
         CLI   RCSUBPRG,3          OFFICE/DEPT SUMMARY                          
         BE    BX30                                                             
*                                                                               
         CLI   RCSUBPRG,1          OVERHEAD PAGES                               
         BE    BX40                                                             
         CLI   RCSUBPRG,5                                                       
         BE    BX40                                                             
         CLI   RCSUBPRG,15                                                      
         BE    BX40                                                             
         CLI   RCSUBPRG,18                                                      
         BE    BX40                                                             
         CLI   RCSUBPRG,20                                                      
         BE    BX40                                                             
         B     BXX                                                              
*                                                                               
BX15     MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+129,C'R'                                                 
         B     BXX                                                              
*                                                                               
BX20     MVI   BOXROWS+9,C'T'                                                   
         MVI   BOXROWS+11,C'M'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+130,C'R'                                                 
         MVI   BOXCOLS+45,C'C'                                                  
         MVI   BOXCOLS+60,C'C'                                                  
         MVI   BOXCOLS+74,C'C'                                                  
         MVI   BOXCOLS+87,C'C'                                                  
         MVI   BOXCOLS+103,C'C'                                                 
         MVI   BOXCOLS+117,C'C'                                                 
         B     BXX                                                              
*                                                                               
BX30     MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+11,C'M'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+110,C'R'                                                 
         B     BXX                                                              
*                                                                               
BX40     TM    UPSI,POSTREP        POSTING SUMMARY                              
         BO    BXX                 DON'T PRINT BOXES                            
         MVI   BOXROWS+9,C'T'                                                   
         MVI   BOXROWS+11,C'M'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+128,C'R'                                                 
         MVI   BOXCOLS+57,C'C'                                                  
         MVI   BOXCOLS+72,C'C'                                                  
         MVI   BOXCOLS+86,C'C'                                                  
         MVI   BOXCOLS+100,C'C'                                                 
         MVI   BOXCOLS+114,C'C'                                                 
         B     BXX                                                              
*                                                                               
BXX      B     COM2XIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
*-------------------------------------------                                    
*        LITERAL POOL                                                           
*-------------------------------------------                                    
*                                                                               
         LTORG                                                                  
ABILITY2 EQU   (4095*3)-(*-VCOMMON2)  REMAINING ADDRESSIBILITY                  
*                                     FREE UP ALL USINGS                        
         DROP                                                                   
         EJECT                                                                  
**********************************************************************          
* ROUTINES ENTERABLE FROM BASE OR WORK NMODS                         *          
* ************WORK NMOD # 3 ****************                         *          
**********************************************************************          
         SPACE 1                                                                
         DS    0D                                                               
VCOMMON3 NMOD1 0,**COMM3**,R9,R8                                                
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         LA    RC,SPACEND                                                       
         USING ACCAD,RC            RC=A(LOCAL W/S)                              
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     VPOSTIT             01 - DO POSTINGS                             
         B     VRECOVER            02 - PUT ADDTRN RECORDS TO RECOVERY          
         B     VYTDADJ             03 - ADJUST FOR YTD-1                        
         B     VBUKREPT            04 - PRINT SORTER BUCKET REPORT              
         B     VMAIN               05 - ACQUIRE CORE                            
         B     VWRAP               06 - RETURN ACQUIRED CORE                    
         B     VBOFFL              07 - BUILD OFFICE\GROUP TABLE                
         B     VGOFFL              08 - GET AN OFFICE\GROUP ENTRY               
         B     VBCLIL              09 - BUILD 1N CLIENT LIST TABLE              
         DC    (COMSPAR3*L'COM3)X'00'                                           
*                                                                               
COM3XIT  XIT1  ,                                                                
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO DO POSTINGS                                            *           
*    NOTES:                                                         *           
*    - ALL 1R POSSIBLE BUCKETS ARE REPRESENTED IN SORT              *           
*      SINCE MONACC READS 1R FOR ACCA.                              *           
*    - ANY 1C BUCKETS IN THE YTD-1 TABLE THAT WERE NOT USED         *           
*      IN THIS RUN ARE ZEROED OUT.                                  *           
*********************************************************************           
         SPACE 1                                                                
VPOSTIT  DS    0H                                                               
         CLI   QOPT1,C'L'          LIVE REQUEST                                 
         BNE   COM3XIT                                                          
         TM    RUNSTAT,POSTERR                                                  
         BO    COM3XIT             DOUBLE CHECK FOR ERRORS                      
         TM    UPSI,POSTRATE       POST RATE RECORDS ONLY?                      
         BO    VPST40              YES - DON'T BOTHER WITH SORT RECORDS         
         CP    PDEBITS,ZEROS       DONT BOTHER IF NO DOLLARS                    
         BE    COM3XIT                                                          
         TM    UPSI,POSTREP        POSTING REPORT?                              
         BNO   *+8                                                              
         MVI   RCSUBPRG,5                                                       
         XC    ALSORT,ALSORT       CLEAR ADDRESS OF LAST SORT RECORD            
         XC    POSTSW,POSTSW       CLEAR POST SWITCHES                          
*                                                                               
         XC    EL44SAVE,EL44SAVE   BUILD THE COMPOSITE 44 ELEMENT               
         LA    R4,EL44SAVE                                                      
         USING TRNELD,R4                                                        
         MVI   TRNEL,TRNELQ        X'44'                                        
         LH    RE,=Y(TRNLN1Q+1)                                                 
         STC   RE,TRNLN                                                         
         MVC   TRNDATE,TODAYP                                                   
         MVC   TRNREF,SPACES                                                    
         MVC   TRNREF(L'METHOD),METHOD                                          
         XC    TRNSUB,TRNSUB                                                    
         MVC   TRNMOS,MOST                                                      
         MVC   TRNANAL,SPACES                                                   
         MVI   TRNSTAT,0                                                        
*                                                                               
         XC    EL50SAVE,EL50SAVE   BUILD THE COMPOSITE 50 ELEMENT               
         LA    R3,EL50SAVE                                                      
         USING SCIELD,R3                                                        
         MVI   SCIEL,SCIELQ        X'50'                                        
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITNULL    SET TO SPECIAL TYPE FOR 2BTYE BUCKET         
         ZAP   SCIAMNT,ZEROS                                                    
         ZAP   SCIADMN,ZEROS                                                    
         MVC   SCISUBTY,SPACES     BUCKET SUB-TYPE                              
         MVC   ARECORD,RECORD      USE "RECORD"                                 
         BAS   RE,GETSORT          GET 1ST SORT REC (ALSTWRK)                   
         OC    ALSORT,ALSORT       ANY SORT RECS AT ALL?                        
         BZ    COM3XIT             NO JUST LEAVE                                
         B     VPST20                                                           
*                                                                               
VPST10   MVC   ARECORD,RECORD      USE "RECORD"                                 
         TM    POSTSW,SORTDONE     IS SORT COMPLETE?                            
         BO    VPST30              YES - YOUR DONE                              
         BAS   RE,GETSORT          GET A SORT REC (ALSTWRK)                     
VPST20   OC    ALSORT,ALSORT                                                    
         BNZ   *+8                                                              
         OI    POSTSW,SORTDONE     SORT COMPLETE/PROCESS LAST RECORD            
         USING SRTD,R5                                                          
         L     R5,ALSTWRK                                                       
*                                  CLEAR RECORD AREA                            
         L     RE,ARECORD          RECEIVING FIELD                              
         LH    RF,=S(L'CRECORD)    RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,ARECORD                                                       
         USING TRNRECD,R6          CONTRA-ACCOUNT RECORD                        
         MVC   TRNKEY,SPACES       CLEAR IT                                     
         MVC   TRNKCULA,SRTKACC    ACCOUNT                                      
         MVC   TRNKCULC,SRTKCON    CONTRA-ACCOUNT                               
         MVC   TRNKDATE,TODAYP                                                  
         MVC   TRNKREF(L'METHOD),METHOD                                         
         XC    TRNKSBR,TRNKSBR                                                  
*                                                                               
         LH    RE,=Y(TRNRFST-TRNRECD+1)                                         
         STCM  RE,3,TRNRLEN        RECORD LENGTH                                
         ZAP   TRNAMNT,ZEROS                                                    
         MVC   SCISUBTY,SPACES                                                  
         MVC   SCISUBTY(L'SRTBTYP),SRTBTYP       BUCKET TYPE                    
*                                                                               
         ZAP   SCIADMN,SRTCR       DEBIT AND CREDIT TO 50 EL                    
         ZAP   SCIAMNT,SRTDR                                                    
         TM    UPSI,ZEROPOST       ZERO OUT THE MONTHS POSTINGS?                
         BZ    *+16                                                             
         ZAP   SCIAMNT,ZEROS                                                    
         ZAP   SCIADMN,ZEROS                                                    
*                                                                               
         GOTO1 HELLO,ELIST,(C'P',MST),TRNRECD,EL44SAVE,0                        
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         GOTO1 HELLO,ELIST,(C'P',MST),TRNRECD,EL50SAVE,0                        
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
*                                                                               
         L     R7,AUPDBLOK                                                      
         USING TRNBLKD,R7                                                       
         MVC   TRNREC,ARECORD                                                   
         MVC   TRNCACNM,SRTCNAME                                                
         GOTO1 ADDTRN,TRNBLKD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VPST10              READ NEXT SORT RECORD                        
         EJECT                                                                  
*                                                                               
VPST30   TM    UPSI,POSTREP        POSTING REPORT?                              
         BNO   VPST40                                                           
         USING BKLIND,R3                                                        
         LA    R3,P                PRINT TOTALS FOR POSTING REPORT              
         CURED POSTDR,(L'BKDEB,BKDEB),2,MINUS=YES                               
         CURED POSTCR,(L'BKCRD,BKCRD),2,MINUS=YES                               
         GOTO1 ACREPORT                                                         
*                                                                               
         USING BIND,R5                                                          
VPST40   ZAP   DUMPCNT,ZEROS       RESET DUMP COUNTS FOR RATE REC               
         ZAP   PDUMP,ZEROS                                                      
         L     R5,RATBUFF          EMPLOYEE HOURLY RATE TABLE                   
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         USING RATD,R7                                                          
         LA    R7,BINTABLE         FIRST TABLE ENTRY                            
VPST50   MVC   ARECORD,RECORD      USE "RECORD"                                 
         L     R6,DIO                                                           
         USING CPRRECD,R6                                                       
         ST    R7,ALTAB            STORE ADDR OF TABLE ENTRY                    
         XC    CPRKEY(L'CDIO),CPRKEY                                            
         MVC   CPRKEY,SPACES                                                    
         MVI   CPRKTYP,CPRKTYPQ    X'3E'                                        
         MVI   CPRKSUB,CPRKSUBQ    SUB RECORD X'07'                             
         MVC   CPRKCPY,RCCOMPFL    COMPANY                                      
         MVC   CPRKUNT(L'EMPLEDG),EMPLEDG                                       
         MVC   CPRKACT,RATACC      EMPLOYEE CODE                                
*&&US*&& MVC   CPRKMTHD,METHOD     COST METHOD                                  
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,DIO,DIO                        
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BO    VPST60              NO BUILD A VIRGIN RECORD                     
         MVC   DA,CPRKDA           DISK ADDRESS OF RECORD                       
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,ARECORD,DMWORK              
         NI    DMINBTS,ALL-PASSDEL                                              
         CLI   8(R1),0             DID I FIND THE RECORD?                       
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    UPSI,DUMPRECS       DUMP THE RECORD?                             
         BNO   *+8                                                              
         BAS   RE,DMPGET                                                        
         B     *+8                                                              
VPST60   BAS   RE,BLDRATR          BUILD A VIRGIN RECORD                        
         BAS   RE,ADDRATEL         ADD OR UPDATE HOURLY RATE ELEMENT            
         BAS   RE,WRITE            WRITE BACK THE RECORD                        
         LA    R7,RATLEN(R7)       BUMP TABLE                                   
         BCT   R3,VPST50           NEXT EMPLOYEE                                
         DROP  R5                                                               
*                                                                               
* **** PROFIT AND LOSS RECORDS ****                                             
*                                                                               
         L     R2,ADCMPEL                                                       
         USING CPYELD,R2                                                        
         CLI   CPYEL,CPYELQ                                                     
         BNE   VPST142                                                          
         TM    CPYSTATA,CPYSACCT   IS COMPANY USING ACCENT                      
         BZ    VPST142                                                          
*                                                                               
         NI    FLAG,X'FF'-FLGYTD   TURN OFF YTD FLAG                            
         ZAP   DUMPCNT,ZEROS       RESET DUMP COUNTS FOR PAL REC                
         ZAP   PDUMP,ZEROS                                                      
*                                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         USING BIND,R1                                                          
         L     R1,PALBUFF          R1=A(PAL RECORD TABLE)                       
         ICM   R3,15,BININ                                                      
         BZ    VPST100                                                          
         USING PLBUFD,R7                                                        
         LA    R7,BINTABLE                                                      
         DROP  R1                                                               
*                                                                               
VPST70   MVC   ARECORD,RECORD      USE "RECORD"                                 
         L     R6,DIO                                                           
         USING PLDRECD,R6                                                       
         ST    R7,ALTAB            STORE ADDR OF TABLE ENTRY                    
         XC    PLDKEY(L'CDIO),PLDKEY                                            
         MVI   PLDKTYP,PLDKTYPQ    X'18' - DIRECT TIME POINTER                  
         MVI   PLDKSUB,PLDKSUBQ    X'CD' - SUB POINTER                          
         MVC   PLDKCPY,PLCPY       COMPANY                                      
         MVC   PLDKMTHD,PLMTHD     METHOD                                       
         MVC   PLDKCACT,PL1CACC    1C ACCOUNT                                   
         MVC   PLDKRACT,PL1RACC    1R ACCOUNT                                   
         MVC   PLDKANAL,PLANAL     ANALYSIS ACCOUNT                             
         MVC   PLDKYYMM,PLYYMM     YEAR/MONTH                                   
         MVC   PLDKPTYP,PLPTYP     PAYROLL TYPE                                 
         OI    DMINBTS,PASSDEL                                                  
         MVI   RECSW,0             CLEAR OUT RECOVERY SWITCH                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,DIO,DIO                        
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BO    VPST80              NO BUILD A VIRGIN RECORD                     
         LR    R0,R1               SET R0=A(DMCB)                               
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
         GOTO1 RECOVER                                                          
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         MVI   RECSW,0             RESET RECOVERY SWITCH                        
         ZAP   PLDKAMT,PLAMNT      SET AMOUNT ON RECORD                         
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         XC    PLDKDA,PLDKDA       CLEAR ANY BAD DISK ADD FIELDS                
         TM    PLDKSTA,DELETED     IS IT MARKED DELETED?                        
         BNO   *+8                                                              
         NI    PLDKSTA,ALL-DELETED UNDELETE THE DIRECTORY RECORD                
*                                                                               
         MVC   MSG,=CL10'OLD PLD' DUMP THE RECORD?                              
         GOTO1 ADUMP,DMCB,(RC),(R6),L'PLDKEY+L'PLDKSTA+L'PLDKAMT                
*                                                                               
         CLI   RCWRITE,C'N'        *** WRITE=NO ***                             
         BE    VPST90                                                           
         GOTO1 DATAMGR,DMCB,DMWRT,DIR,DIO,DIO                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIED WRITING A RECORD                        
         B     VPST90                                                           
*                                                                               
VPST80   BAS   RE,BLDPLDR          BUILD A VIRGIN RECORD                        
*                                                                               
         CP    PLDKAMT,ZEROS       NO NEED FOR ZERO NEW RECORDS                 
         BE    VPST90                                                           
         MVC   MSG,=CL10'NEW PLD' DUMP THE RECORD?                              
         GOTO1 ADUMP,DMCB,(RC),(R6),L'PLDKEY+L'PLDKSTA+L'PLDKAMT                
*                                                                               
         CLI   RCWRITE,C'N'        *** WRITE=NO ***                             
         BE    VPST90                                                           
         GOTO1 DATAMGR,DMCB,DMADD,DIR,DIO,DIO                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIED WRITING A RECORD                        
         LR    R0,R1               SET R0=A(DMCB)                               
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         GOTO1 RECOVER                                                          
         B     VPST90                                                           
*                                                                               
VPST90   DS    0H                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         LA    R7,PLLNQ(R7)        BUMP TABLE                                   
         BCT   R3,VPST70           NEXT EMPLOYEE                                
*                                                                               
* **** PROFIT AND LOSS RECORDS YTD BACKOUTS (IF ANY) ****                       
*                                                                               
VPST100  DS    0H                                                               
         OI    FLAG,FLGYTD         SHOW THAT WE ARE DOING YTD BACKOUTS          
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         ZAP   DUMPCNT,ZEROS       RESET DUMP COUNTS FOR PAL REC                
         ZAP   PDUMP,ZEROS                                                      
*                                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         USING BIND,R1                                                          
         L     R1,YTDPLTB          R1=A(YTD PAL RECORD TABLE)                   
         ICM   R3,15,BININ                                                      
         BZ    VPST140                                                          
         USING YTDPLD,R7                                                        
         LA    R7,BINTABLE                                                      
         DROP  R1                                                               
*                                                                               
VPST110  MVC   ARECORD,RECORD      USE "RECORD"                                 
         L     R6,DIO                                                           
         USING PLDRECD,R6                                                       
*                                                                               
         TM    YTDPSTAT,YTDPPST    WAS THIS COMBO POSTED TO?                    
         BO    VPST130             YES-DONT BACKOUT                             
*                                                                               
         USING YTD1D,RE                                                         
         LA    RE,WORK             MAKE CALL TO VALCON TO CHECK O/D             
         ST    RE,ALTAB            SET ADDRESS FOR VALCON                       
         XC    WORK,WORK                                                        
         MVC   YTDCON+L'GROUP(L'YTDCON),YTDP1R       SEND 1R ACCOUNT            
         BAS   RE,VALCON           MAKE SURE OFF/DPT WAS USED B4                
         BNE   VPST130             NOT VALID-SKIP                               
         DROP  RE                                                               
*                                                                               
         ST    R7,ALTAB            STORE ADDR OF TABLE ENTRY                    
         XC    PLDKEY(L'CDIO),PLDKEY                                            
         MVI   PLDKTYP,PLDKTYPQ    X'18' - DIRECT TIME POINTER                  
         MVI   PLDKSUB,PLDKSUBQ    X'CD' - SUB POINTER                          
         MVC   PLDKCPY,YTDPCPY     COMPANY                                      
         MVC   PLDKMTHD,YTDPMTHD   METHOD                                       
         MVC   PLDKCACT,YTDP1C     1C ACCOUNT                                   
         MVC   PLDKRACT,YTDP1R     1R ACCOUNT                                   
         MVC   PLDKANAL,YTDPANAL   ANALYSIS ACCOUNT                             
         MVC   PLDKYYMM,MOS        YEAR/MONTH                                   
         MVC   PLDKPTYP,YTDPTYP    PAYROLL TYPE                                 
         OI    DMINBTS,PASSDEL                                                  
         MVI   RECSW,0             INIT RECOVERY SWITCH                         
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,DIO,DIO                        
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BO    VPST120             NO BUILD A VIRGIN RECORD                     
         LR    R0,R1               SET R0=A(DMCB)                               
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
         GOTO1 RECOVER                                                          
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         MVI   RECSW,0             RESET RECOVERY SWITCH                        
         MP    YTDPAMNT,=P'-1'     BACKOUT THE WHOLE AMOUNT                     
         AP    PLDKAMT,YTDPAMNT    SUBTRACT AMOUNT ON RECORD                    
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         XC    PLDKDA,PLDKDA       CLEAR ANY BAD DISK ADD FIELDS                
         TM    PLDKSTA,DELETED     IS IT MARKED DELETED?                        
         BNO   *+8                                                              
         NI    PLDKSTA,ALL-DELETED UNDELETE THE DIRECTORY RECORD                
*                                                                               
         MVC   MSG,=CL10'OLD YTD' DUMP THE RECORD?                              
         GOTO1 ADUMP,DMCB,(RC),(R6),L'PLDKEY+L'PLDKSTA+L'PLDKAMT                
*                                                                               
         CLI   RCWRITE,C'N'        *** WRITE=NO ***                             
         BE    VPST130                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,DIR,DIO,DIO                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIED WRITING A RECORD                        
         LR    R0,R1               SET R0=A(DMCB)                               
         B     VPST130                                                          
*                                                                               
VPST120  BAS   RE,BLDPLDR          BUILD A VIRGIN RECORD                        
*                                                                               
         CP    PLDKAMT,ZEROS       NEW BACKOUT SHOULD NEVER BE ZEROS            
         BE    VPST130                                                          
         MVC   MSG,=CL10'NEW YTD' DUMP THE RECORD?                              
         GOTO1 ADUMP,DMCB,(RC),(R6),L'PLDKEY+L'PLDKSTA+L'PLDKAMT                
*                                                                               
         CLI   RCWRITE,C'N'        *** WRITE=NO ***                             
         BE    VPST130                                                          
         GOTO1 DATAMGR,DMCB,DMADD,DIR,DIO,DIO                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIED WRITING A RECORD                        
         LR    R0,R1               SET R0=A(DMCB)                               
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         GOTO1 RECOVER                                                          
*                                                                               
VPST130  DS    0H                                                               
*                                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         LA    R7,YTDPLNQ(R7)      BUMP TABLE                                   
         BCT   R3,VPST110          NEXT EMPLOYEE                                
*                                                                               
VPST140  DS    0H                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
VPST142  TM    UPSI,POSTRATE       POST RATE RECORDS ONLY?                      
         BO    COM3XIT             YES - DON'T BOTHER WITH HIST RECORD          
         ZAP   DUMPCNT,ZEROS       RESET DUMP FOR COST HISTORY RECORD           
         ZAP   PDUMP,ZEROS                                                      
*                                                                               
* **** COST HISTORY RECORD ****                                                 
*                                                                               
         MVC   ARECORD,RECORD      USE "RECORD"                                 
         L     R6,DIO                                                           
         USING CMTRECD,R6                                                       
         XC    CMTKEY(L'CDIO),CMTKEY                                            
         MVC   CMTKEY,SPACES                                                    
         MVI   CMTKTYP,CMTKTYPQ    X'3E'                                        
         MVI   CMTKSUB,CMTKSUBQ    SUB RECORD X'02'                             
         MVC   CMTKCPY,RCCOMPFL    COMPANY                                      
         MVC   CMTKMTHD,METHCODE   METHOD OF ALLOCATION                         
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,DIO,DIO                        
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BO    VPST150             NO BUILD A VIRGIN RECORD                     
         MVC   DA,CMTKDA           DISK ADDRESS OF RECORD                       
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,ARECORD,DMWORK              
         NI    DMINBTS,ALL-PASSDEL                                              
         CLI   8(R1),0             DID I FIND THE RECORD?                       
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    UPSI,DUMPRECS       DUMP THE RECORD?                             
         BNO   *+8                                                              
         BAS   RE,DMPGET                                                        
         B     *+8                                                              
VPST150  BAS   RE,BLDCMTR          BUILD A VIRGIN RECORD                        
         BAS   RE,ADDDOAEL         ADD OR UPDATE DATE OF ALLOCATION EL          
         BAS   RE,WRITE            WRITE BACK THE RECORD                        
         CLI   OPTLOCK,C'Y'        LOCK MOS FOR TIMESHEETS?(PROFILE)            
         BE    VPST160                                                          
         CLI   QOPT6,C'L'          LOCK MOS FOR TIMESHEETS?(REQUEST)            
         BNE   COM3XIT                                                          
*                                                                               
* **** UPDATE MOS LOCK EL  ****                                                 
* **** ON THE COMPANY RECD ****                                                 
*                                                                               
VPST160  ZAP   DUMPCNT,ZEROS       RESET DUMP                                   
         ZAP   PDUMP,ZEROS                                                      
         MVC   ARECORD,RECORD      USE "RECORD"                                 
         L     R6,DIO                                                           
         USING CPYRECD,R6                                                       
         XC    CPYKEY(L'CDIO),CPYKEY                                            
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,RCCOMPFL    COMPANY                                      
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,DIO,DIO                        
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BNO   *+6                 NO - YOU BETTER FIND IT                      
         DC    H'0'                                                             
         MVC   DA,CPYKDA           DISK ADDRESS OF RECORD                       
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,ARECORD,DMWORK              
         NI    DMINBTS,ALL-PASSDEL                                              
         CLI   8(R1),0             DID I FIND THE RECORD?                       
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    UPSI,DUMPRECS       DUMP THE RECORD?                             
         BNO   *+8                                                              
         BAS   RE,DMPGET                                                        
         BAS   RE,ADDMSLEL         ADD OR UPDATE MOS LOCK ELEMENT               
         BAS   RE,WRITE            WRITE BACK THE RECORD                        
         B     COM3XIT                                                          
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO GET A COMBINED SORT RECORD                          *          
*     ON EXIT, ALSTWRK HAS ADDR OF RECORD                            *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
GETSORT  NTR1                                                                   
         L     R5,ASRTWRK                                                       
         L     R4,ALSTWRK                                                       
         OC    ALSORT,ALSORT        1ST TIME THRU?                              
         BNZ   GETS10               NO - SORT REC BECOMES LAST SORT REC         
         GOTO1 CLERSRT,DMCB,(R5)    YES - CLEAR BOTH WORK AREAS                 
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
GETS10   MVC   0(SRTLEN,R4),SRTD                                                
GETS20   GOTO1 ADSORTER,DMCB,(L'GETC,GETC)                                      
         L     R2,4(R1)                                                         
         ST    R2,ALSORT           ADDRESS OF LAST SORT REC                     
         LTR   R2,R2                                                            
         BZ    COM3XIT             SORT IS DONE                                 
*                                                                               
         MVC   SRTD(SRTLEN),0(R2)                 SAVE CURRENT SRT REC          
         CLC   SRTLGACC-SRTD(L'SRTLGACC,R4),SPACES DO I HAVE ONE SAVED?         
         BE    GETS10                             NO - GO SAVE                  
*                                                                               
         CLC   SRTKEY(SRTKLEN),SRTKEY-SRTD(R4)    COMPARE SRT TO LST            
         BNE   COM3XIT                            NOT EQUAL YOUR DONE           
*                                                                               
         LA    R0,SRTBKNUM                        NUMBER OF BUCKETS             
         LA    R1,SRTBUKS                         CURRENT SORT REC              
         LA    R2,SRTBUKS-SRTD(R4)                SAVED SORT REC                
         AP    0(L'SRTBUKS,R2),0(L'SRTBUKS,R1)    ADD 'EM                       
         LA    R1,L'SRTBUKS(R1)                   BUMP 'EM                      
         LA    R2,L'SRTBUKS(R2)                                                 
         BCT   R0,*-14                                                          
         B     GETS20                             AND GET THE NEXT ONE          
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE THE POSSIBLITY OF THE 1C CONTRA BEING AFFECTED  *         
* BY THIS REQUEST                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALCON   NTR1                                                                   
         L     R4,ALTAB                                                         
         USING YTD1D,R4                                                         
         MVC   MSG,=CL10'VAL CON'                                               
         GOTO1 ADUMP,DMCB,(RC),(R4),YTDLEN                                      
         MVC   NMEWRK,SPACES                                                    
         USING HYBND,R2                                                         
         LA    R2,NMEWRK                                                        
         LA    R3,YTDCON+L'GROUP   GROUP CODE PRECEDING HYBID                   
         CLI   YTDCNTLD,C'6'       HARDCODE LEDGER 6 TO 1ST POSITION            
         BNE   *+8                                                              
         LA    R3,YTDCON                                                        
         SR    R1,R1                                                            
         IC    R1,CSTGRPLN         LENGTH FOR MVC                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HYCODE(0),0(R3)                                                  
         L     R5,HYNMBUF                                                       
         USING BIND,R5                                                          
         MVC   DMCB+8(BINKLEN),BININ                                            
         LA    R3,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(X'00',(R2)),(R3)                                   
         TM    0(R1),NOTFOUND                                                   
         BNO   VALC10              FOUND IT                                     
*                                                                               
* THIS CONTRA NOT VALID FOR REQ CHECK FOR CHANGE IN CONTRA STRUCTURE            
*                                                                               
         CLC   CSTGRPLN,LLEVC      COST GROUP LEVEL C?                          
         BNE   VALCNO              ASSUME NOT VALID                             
         MVC   DMCB+8(BINKLEN),BININ                                            
         XC    DMCB+16(4),DMCB+16                                               
         MVC   DMCB+19(1),LLEVB    SEARCH FOR OCCURANCE OF LEVEL B              
         GOTO1 BINSRCH,DMCB,(X'00',(R2)),(R3)                                   
         TM    0(R1),NOTFOUND                                                   
         BO    VALCNO              THIS CONTRA NOT VALID FOR REQUEST            
VALC10   L     R2,DMCB             ADDR OF RECORD FOUND                         
         MVC   NMEWRK(HYBNLEN),0(R2)                                            
*                                                                               
VALCYES  CR    RB,RB               EQUAL CONDITION ON EXIT                      
         B     COM3XIT                                                          
VALCNO   LTR   RB,RB               NOT EQUAL CONDITION ON EXIT                  
         B     COM3XIT                                                          
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* WRITE BACK A RECORD (NEW FILE)                                     *          
**********************************************************************          
         SPACE 1                                                                
         USING CACRECD,R6                                                       
WRITE    NTR1                                                                   
         L     R6,ARECORD          ADDR OF RECORD TO ADD OR WRITE BACK          
*                                                                               
         USING CHDRECD,R4                                                       
         L     R4,DIO              ADDR OF DIRECTORY IO AREA                    
         XC    CHDKEY(L'CDIO),CHDKEY                                            
         MVC   CHDKEY,CACKEY                                                    
         MVC   COMMAND,ADDREC            SET SUBSEQUENT COMMAND TO ADD          
         OI    DMINBTS,PASSDEL           PASS BACK DELETES AS WELL              
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,DIO,DIO                        
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'               DID I FIND THE RECORD?                 
         BO    WRT10                     NO-THEN ADD IT                         
         MVC   DA,CHDKDA                 DISK ADDRESS                           
         OI    DMINBTS,PASSDEL           PASS BACK DELETES AS WELL              
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,RECORD2,DMWORK              
         NI    DMINBTS,ALL-PASSDEL                                              
         CLI   8(R1),0                   DID I FIND THE RECORD?                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                     SET R0=A(DMCB)                         
         GOTO1 RECOVER                                                          
         MVC   ARECORD,RECORD            RESET TO USE "RECORD"                  
         MVC   COMMAND,PUTREC            YES - WRITE IT BACK                    
*                                                                               
WRT10    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,MST,DA,ARECORD,DMWORK                       
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'              *** WRITE=NO ***                       
         BE    WRT20                                                            
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                      DIED WRITING A RECORD                  
WRT20    LR    R0,R1                     SET R0=A(DMCB)                         
         GOTO1 RECOVER                                                          
         MVC   ARECORD,RECORD            RESET TO USE "RECORD"                  
*                                                                               
         CLC   COMMAND,PUTREC            WERE WE PUTTING A RECORD               
         BNE   WRTX                      NO- SKIP CHECK FOR DEL DIR             
*                                                                               
         USING CHDRECD,R4                                                       
WRT30    L     R4,DIO                    ADDR OF DIRECTORY IO AREA              
         TM    CHDKSTA,DELETED           IS IT MARKED DELETED?                  
         BNO   WRTX                                                             
         NI    CHDKSTA,ALL-DELETED       UNDELETE THE DIRECTORY RECORD          
         CLI   RCWRITE,C'N'              *** WRITE=NO ***                       
         BE    WRTX                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,DIR,DIO,DIO                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                      DIED WRITING A RECORD                  
*                                                                               
WRTX     DS    0H                                                               
         B     COM3XIT                                                          
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A VIRGIN COSTING PERSONAL RATE RECORD              *         
***********************************************************************         
         SPACE 1                                                                
BLDRATR  NTR1                                                                   
         L     R7,ALTAB            ADDR OF CURRENT TABLE ENTRY                  
         USING RATD,R7                                                          
*                                  CLEAR RECORD AREA TO BINARY ZEROS            
         L     RE,ARECORD          RECEIVING FIELD                              
         LH    RF,=S(L'CRECORD)    RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,ARECORD                                                       
         USING CPRRECD,R6          COSTING PERSONAL RATES RECORD                
         MVC   CPRKEY,SPACES                                                    
         MVI   CPRKTYP,CPRKTYPQ    X'3E'                                        
         MVI   CPRKSUB,CPRKSUBQ    SUB RECORD X'07'                             
         MVC   CPRKCPY,RCCOMPFL    COMPANY                                      
         MVC   CPRKUNT(L'EMPLEDG),EMPLEDG                                       
         MVC   CPRKACT,RATACC      EMPLOYEE CODE                                
*&&US*&& MVC   CPRKMTHD,METHOD     COST METHOD                                  
*                                                                               
         LH    RE,=Y(CPRRFST-CPRRECD+1)                                         
         STCM  RE,3,CPRRLEN        RECORD LENGTH                                
         B     COM3XIT                                                          
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A VIRGIN DIRECT TIME POINTER RECORD (PLDREC)       *         
*         THIS WILL EITHER BE CALLED FROM THE PALBUFF OR YTDPLTB      *         
*         THEY BOTH HAVE THE SAME OFFSETS IN THEIR DSECT EXCEPT       *         
*         FOR THE MONTH FIELD.  YTDPLTB DOES NOT HAVE A YYMM FIELD    *         
*         IT USES THE MOS STORAGE FIELD INSTEAD.                      *         
***********************************************************************         
         SPACE 1                                                                
BLDPLDR  NTR1                                                                   
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         L     R7,ALTAB            ADDR OF CURRENT TABLE ENTRY                  
         USING PLBUFD,R7                                                        
*                                  CLEAR RECORD AREA TO BINARY ZEROS            
         L     RE,ARECORD          RECEIVING FIELD                              
         LH    RF,=S(L'CRECORD)    RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING PLDRECD,R6          DIRECT TIME POINTER RECORD                   
         L     R6,DIO                                                           
         XC    PLDKEY(L'CDIO),PLDKEY                                            
         MVI   PLDKTYP,PLDKTYPQ    X'18'                                        
         MVI   PLDKSUB,PLDKSUBQ    SUB RECORD X'CD'                             
         MVC   PLDKCPY,PLCPY       COMPANY                                      
         MVC   PLDKMTHD,PLMTHD     METHOD                                       
         MVC   PLDKCACT,PL1CACC    1C ACCOUNT                                   
         MVC   PLDKRACT,PL1RACC    1R ACCOUNT                                   
         MVC   PLDKANAL,PLANAL     ANALYSIS ACCOUNT                             
         MVC   PLDKPTYP,PLPTYP     PAYROLL TYPE                                 
         MVC   PLDKYYMM,PLYYMM     YEAR/MONTH                                   
         ZAP   PLDKAMT,PLAMNT      AMOUNT                                       
         TM    FLAG,FLGYTD         ARE WE POSTING A BACKOUT?                    
         BNO   *+16                                                             
         MVC   PLDKYYMM,MOS        YEAR/MONTH                                   
         MP    PLDKAMT,=P'-1'      POST NEGATIVE AMOUNT                         
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         B     COM3XIT                                                          
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A VIRGIN COSTING ALLOCATION HISTORY RECORD         *         
***********************************************************************         
         SPACE 1                                                                
BLDCMTR  NTR1                                                                   
*                                  CLEAR RECORD AREA TO BINARY ZEROS            
         L     RE,ARECORD          RECEIVING FIELD                              
         LH    RF,=S(L'CRECORD)    RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,ARECORD                                                       
         USING CMTRECD,R6                                                       
         MVC   CMTKEY,SPACES                                                    
         MVI   CMTKTYP,CMTKTYPQ    X'3E'                                        
         MVI   CMTKSUB,CMTKSUBQ    SUB RECORD X'02'                             
         MVC   CMTKCPY,RCCOMPFL    COMPANY                                      
         MVC   CMTKMTHD,METHCODE   METHOD OF ALLOCATION                         
*                                                                               
         LH    RE,=Y(CMTRFST-CMTRECD+1)                                         
         STCM  RE,3,CMTRLEN        RECORD LENGTH                                
         B     COM3XIT                                                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A PERSONAL HOURLY RATE ELEMENT                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPRRECD,R6                                                       
ADDRATEL NTR1                                                                   
         L     R6,ARECORD                                                       
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING PHRELD,R4                                                        
         MVI   PHREL,PHRELQ        SET UP HOURLY RATE ELEMENT                   
         MVI   PHRLN,PHRLN1Q       LENGTH OF ELEMENT KEY INFO                   
         MVC   PHRPER,MOS          MONTH OF SERVICE                             
         MVC   PHRMTH,METHOD       METHOD OF ALLOCATION                         
         XC    PHRNUM,PHRNUM       NUMBER OF MINI ELEMENTS                      
*                                                                               
         GOTO1 HELLO,ELIST,(C'G',MST),('PHRELQ',CPRRECD),              X        
               (L'PHRPER+L'PHRMTH,PHRPER)                                       
         CLI   ELERR,0                                                          
         BNE   ADDRA10             NO MATCHING HRLY RATE ELEMENT                
         L     R4,ELADDR                                                        
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   PHREL,DELELQ        MARK IT DELETED                              
         GOTO1 HELLO,ELIST,(C'D',MST),('DELELQ',CPRRECD),0                      
*                                                                               
ADDRA10  LA    R4,ELEMENT          SET R4 TO "NEW" ELEMENT                      
         L     R7,ALTAB            ADDR OF CURRENT TABLE ENTRY                  
         USING RATD,R7                                                          
         LA    R3,RBKCNT           NUMBER OF HRLY RATE BUCKETS                  
         LA    R2,RBUKS            1ST BUCKET                                   
ADDRA20  SR    RE,RE                                                            
         IC    RE,PHRLN                                                         
         LA    RE,PHRELD(RE)       POINT RE TO SUB ELEMENT                      
         USING PHRNTRY,RE                                                       
         USING CMPUTD,R5                                                        
         L     R5,COMPRAT          TBL OF DISP TO SAL TYPES                     
ADDRA30  L     R1,ALTAB            EMPLOYEE'S HRLY RATE TABL ENTRY              
         AH    R1,CMRATD           DISP TO AN HRLY RATE BUCKET                  
         CR    R2,R1               R2 POINTS TO CURRENT BUCKET                  
         BE    ADDRA40             EQUAL MEANS YOU FOUND CORRECT TYPE           
         LA    R5,CMPULN(R5)       NEXT SAL TYPE DISP ENTRY                     
         CLI   0(R5),EOT           END OF TABL?                                 
         BNE   ADDRA30                                                          
         DC    H'0'                SALARY TYPE UNKNOWN                          
*                                                                               
ADDRA40  CP    0(RBUKLN,R2),ZEROS  HRLY RATE FOR THIS SAL TYPE ZERO?            
         BNZ   *+12                NO - CONTINUE                                
         CLI   CMBUKTYP,PHRTTOT    IS THIS THE "TOTAL" HRLY RATE?               
         BNE   ADDRA50             YES - PUT OUT EVEN WITH 0 HRLY RATE          
         MVC   PHRTYPE,CMBUKTYP    SALARY TYPE                                  
         ZAP   PHRRATE,0(RBUKLN,R2) HOURLY RATE                                 
         DROP  RE                                                               
         SR    RE,RE               INCREMENT ELEMENT LENGTH                     
         IC    RE,PHRLN                                                         
         AH    RE,=Y(PHRLN2Q)                                                   
         STC   RE,PHRLN                                                         
         SR    RE,RE               INCREMENT NUMBER OF SUB ELEMENTS             
         IC    RE,PHRNUM                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PHRNUM                                                        
ADDRA50  LA    R2,RBUKLN(R2)                                                    
         BCT   R3,ADDRA20                                                       
*                                                                               
ADDRA60  LA    R1,CPRRFST-CPRRECD(R6)            INT TO 1ST EL                  
         SR    RE,RE               RE=A(OLDEST BUCKET ELEMENT)                  
ADDRA70  CLI   0(R1),0             TEST E-O-R                                   
         BE    ADDRA90                                                          
         CLI   0(R1),PHRELQ        TEST HOURLY RATE ELEMENT                     
         BE    ADDRA80                                                          
         SR    R0,R0                                                            
         IC    R0,PHRLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     ADDRA70                                                          
*                                                                               
ADDRA80  LR    RE,R1               SAVE ADDR OF 1ST (OLDEST ONE)                
*                                                                               
ADDRA90  SR    R1,R1                                                            
         ICM   R1,3,CPRRLEN        CURRENT LENGTH OF RECORD                     
         SR    RF,RF                                                            
         IC    RF,PHRLN            NEW ELEMENT TO ADD                           
         AR    RF,R1               ADD TOGETHER                                 
         C     RF,=A(RECLNMAX)                                                  
         BNH   ADDRA100            RECORD TOO BIG DELETE OLDEST ELEMENT         
         LTR   R4,RE               POINT TO OLDEST (FIRST) BUCKET               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   PHREL,DELELQ                                                     
         GOTO1 HELLO,ELIST,(C'D',MST),('DELELQ',CPRRECD),0                      
         LA    R4,ELEMENT          RESET R4 TO "NEW" ELEMENT                    
         B     ADDRA60             TRY AGAIN TO FIT IT                          
*                                                                               
* ADD THE NEW ELEMENT                                                           
*                                                                               
ADDRA100 DS    0H                                                               
         GOTO1 HELLO,ELIST,(C'P',MST),CPRRECD,ELEMENT,0                         
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         B     COM3XIT                                                          
         DROP  R4,R5,R6,R7                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A DATE OF ALLOCATION ELEMENT                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CMTRECD,R6                                                       
ADDDOAEL NTR1                                                                   
         L     R6,ARECORD                                                       
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING DOAELD,R4                                                        
         MVI   DOAEL,DOAELQ        SET UP DOA ELEMENT                           
         MVI   DOALN,DOALNQ        LENGTH OF ELEMENT KEY INFO                   
         MVC   DOAPER,MOS          MONTH OF SERVICE                             
         MVC   DOADTE,TODAYC       TODAY'S DATE COMPRESSED                      
*                                                                               
         GOTO1 HELLO,ELIST,(C'G',MST),('DOAELQ',CMTRECD),              X        
               (L'DOAPER,DOAPER)                                                
         CLI   ELERR,0                                                          
         BNE   ADDDA10             NO MATCHING DOA ELEMENT                      
         L     R4,ELADDR                                                        
         MVC   DOADTE,TODAYC       TODAY'S DATE COMPRESSED                      
         B     COM3XIT                                                          
*                                                                               
ADDDA10  LA    R4,ELEMENT          SET R4 TO "NEW" ELEMENT                      
         GOTO1 HELLO,ELIST,(C'P',MST),CMTRECD,ELEMENT,0                         
         CLI   ELERR,0                                                          
         BE    COM3XIT                                                          
         CLI   ELERR,X'05'         HAS REC BECOME TOO BIG?                      
         BE    *+6                 YES THAN REMOVE OLDEST ELEMENT               
         DC    H'0'                ELSE DIE                                     
*                                                                               
         L     R6,ARECORD          GET THE FIRST ELEMENT                        
         GOTO1 HELLO,ELIST,(C'G',MST),('DOAELQ',CMTRECD),0,0                    
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,ELADDR                                                        
         MVI   0(R4),X'FF'         MARK ELEMENT FOR DELETION                    
         GOTO1 HELLO,ELIST,(C'D',MST),(X'FF',CMTRECD),0,0                       
*                                  NOW TRY ADDING ELEMENT AGAIN                 
         LA    R4,ELEMENT          SET R4 TO "NEW" ELEMENT                      
         L     R6,ARECORD                                                       
         GOTO1 HELLO,ELIST,(C'P',MST),CMTRECD,ELEMENT,0                         
         CLI   ELERR,0                                                          
         BE    COM3XIT                                                          
         DC    H'0'               IF STILL CAN'T ADD SOMETHING'S WRONG          
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A MOS LOCK ELEMENT                                   *         
***********************************************************************         
         SPACE 1                                                                
ADDMSLEL NTR1                                                                   
         MVC   LMONTH,MOS+1        PACKED END MONTH (MM)                        
         TR    LMONTH,ADDMLTAB     CONVERT PACKED TO CHAR FORMAT                
*                                                                               
         USING ADTYPD,R5                                                        
         LA    R5,ADDTYPTB         TS TYPE TABLE                                
ADDML10  CLI   ADTYCRTY,EOT                                                     
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN COUNTRY CODE                         
         CLC   ADTYCRTY,CTRY                                                    
         BE    ADDML20                                                          
         SR    R1,R1                                                            
         IC    R1,ADTYLEN                                                       
         AR    R5,R1                                                            
         B     ADDML10                                                          
*                                                                               
ADDML20  L     R6,ARECORD                                                       
         USING CPYRECD,R6                                                       
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING MSLELD,R4                                                        
         MVI   MSLEL,MSLELQ        SET UP MOS LOCK ELEMENT                      
         MVC   MSLNUM,ADTYNUM      NUMBER OF SUB EL                             
         SR    R0,R0                                                            
         IC    R0,ADTYNUM                                                       
         LR    R1,R0                                                            
*&&US*&& MH    R1,=Y(L'MSLVALS)                                                 
*&&UK*&& MH    R1,=Y(L'MSLVALS2)                                                
         AH    R1,=Y(MSLLN1Q)                                                   
         STC   R1,MSLLN                                                         
         LA    RF,ADTYENT          1ST INPUT TYPE                               
         LA    R2,MSLVALS          1ST MINI                                     
         USING MSLVALS,R2                                                       
ADDML30  MVC   MSLVTYP,0(RF)       BATCH TYPE                                   
         MVC   MSLVMON,LMONTH      LATEST LOCKED MTH FOR END DATE               
         MVI   MSLVSEC,MSLVSDEF    NO SECURITY OVERRIDE                         
*&&UK                                                                           
         MVC   MSLVYEAR,MOS        YEAR FOR EXTENDED ENTRY                      
         OI    MSLVSEC,MSLVSXTN    INDICATE EXTENDED ENTRY                      
         LA    R2,L'MSLVALS2(R2)   NEXT MINI EXTENDED                           
*&&                                                                             
*&&US*&& LA    R2,L'MSLVALS(R2)    NEXT MINI                                    
         LA    RF,1(RF)                                                         
         BCT   R0,ADDML30                                                       
*                                                                               
         GOTO1 HELLO,ELIST,(C'G',MST),('MSLELQ',CPYRECD),0                      
         CLI   ELERR,0                                                          
         BNE   ADDML110            NO MATCHING MOS ELEMENT                      
**                                                                              
         MVI   BYTE,C'N'           SET ACTIVITY BYTE TO NO                      
         L     R4,ELADDR                                                        
         XC    ELEMENT,ELEMENT                                                  
         SR    R1,R1                                                            
         IC    R1,MSLLN                                                         
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),MSLEL    COPY "OLD" EL TO "NEW" ELEMENT               
         MVI   MSLLONG,C'N'                                                     
         CLI   MSLVTYP-MSLELD(R4),0  NEW EXTENDED FORMAT?                       
         BNE   *+16                                                             
         TM    MSLVSEC-MSLELD(R4),MSLVSXTN                                      
         BZ    *+8                                                              
         MVI   MSLLONG,C'Y'                                                     
**                                                                              
         SR    R3,R3                                                            
         IC    R3,ADTYNUM                                                       
         LA    R7,ADTYENT          1ST INPUT TYPE                               
ADDML40  LA    R4,ELEMENT          "NEW" ELEMENT                                
         SR    R0,R0                                                            
         ICM   R0,1,MSLNUM         NUMBER OF MINI ENTRIES                       
         LA    R2,MSLVALS-MSLELD(0,R4)    1ST MINI                              
         USING MSLVALS,R2                                                       
ADDML50  CLC   MSLVTYP,0(R7)                                                    
         BNE   ADDML80                                                          
         MVC   HALF,TODAYP         EL DOSN'T HAVE YEAR ,TRY WITH TODAY          
         TM    MSLVSEC,MSLVSXTN    DOES EXTENDED ENTRY CARRY YEAR?              
         BZ    *+10                                                             
         MVC   HALF(1),MSLVYEAR    TAKE YEAR FROM MINI EL                       
         LA    RF,ADDMLTAX-1       CONVERT TO PACKED MONTH                      
         CLC   0(1,RF),MSLVMON                                                  
         BE    *+12                                                             
         BCT   RF,*-10                                                          
         B     ADDML100                                                         
         LA    RE,ADDMLTAB         BEGINNING OF TABLE                           
         SR    RF,RE               DISP INTO TABLE IS "PACKED" MONTH            
         STC   RF,HALF+1                                                        
*                                                                               
         LA    R1,2                TRY THIS TWICE                               
ADDML60  CLC   HALF,MOS            HALF IS CURRENT LOCKED MOS IS NEW            
         BNH   ADDML70             EQUAL OF LOWER REPLACE                       
         CLC   HALF,TODAYP         IS CURRENT HIGHER THAN TODAY?                
         BNH   ADDML100            NO - PROBABLY REDOING AN OLD MTH             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),HALF                                                     
         MVC   WORK+1(2),=X'0101'     SET 01/JAN AND GET PREVIOUS YEAR          
         GOTO1 DATCON,DMCB,(X'31',WORK),(1,WORK),(5,0)                          
         MVC   HALF(1),WORK        YY PACKED                                    
         BCT   R1,ADDML60                                                       
         B     ADDML100            IF HASN'T WORKED BY NOW JUST LEAVE           
*                                                                               
ADDML70  MVC   MSLVMON,LMONTH      UPDATE LOCKED MONTH                          
         TM    MSLVSEC,MSLVSXTN    DOES EXTENDED ENTRY CARRY YEAR?              
         BNO   *+10                                                             
         MVC   MSLVYEAR,MOS                                                     
         MVI   BYTE,C'Y'           SET ACTIVITY SWITCH TO YES                   
         B     ADDML100                                                         
ADDML80  TM    MSLVSEC,MSLVSXTN    EXTENDED MINI?                               
         LA    R2,L'MSLVALS(R2)    BUMP TO NEXT MINI(STANDARD)                  
         BZ    *+8                 NO - ITS NOT EXTENDED                        
         LA    R2,L'MSLVALS2-L'MSLVALS(R2) YES - BUMP FOR EXTENSION             
         BCT   R0,ADDML50                                                       
*                                                                               
         LA    R4,ELEMENT                                                       
         LR    R2,R4                                                            
         SR    R1,R1                                                            
         IC    R1,MSLLN                                                         
         AR    R2,R1               POINT R2 TO BYTE AFTER LAST SUB EL           
         USING MSLVALS,R2                                                       
         MVC   MSLVTYP,0(R7)       ADD A SUB EL FOR INPUTTYPE                   
         MVI   MSLVSEC,MSLVSDEF    NO SECURTIY OVERRIDE                         
         MVC   MSLVMON,LMONTH                                                   
         LH    RE,=Y(L'MSLVALS)    LENGTH ON MINI EL IN RE                      
         CLI   MSLLONG,C'Y'                                                     
         BNE   ADDML90                                                          
         LH    RE,=Y(L'MSLVALS2)   LENGTH OF EXTENDED MINI EL                   
         MVC   MSLVYEAR,MOS        YEAR FOR EXTENDED ENTRY                      
         OI    MSLVSEC,MSLVSXTN    INDICATE EXTENDED ENTRY                      
ADDML90  SR    R1,R1                                                            
         IC    R1,MSLLN            INCREMENT ELEMENT LENGTH                     
         AR    R1,RE                                                            
         STC   R1,MSLLN                                                         
         SR    R1,R1                                                            
         IC    R1,MSLNUM           INCREMENT NUMBER OF SUB ELEMENTS             
         LA    R1,1(R1)                                                         
         STC   R1,MSLNUM                                                        
         MVI   BYTE,C'Y'           SET ACTIVITY SWITCH TO YES                   
*                                                                               
ADDML100 LA    R7,1(R7)                                                         
         BCT   R3,ADDML40                                                       
*                                                                               
* DEL OLD ADD NEW                                                               
*                                                                               
         CLI   BYTE,C'Y'           ANY ACTIVITY                                 
         BNE   COM3XIT                                                          
         L     R4,ELADDR           SET R4 TO OLD ELEMENT                        
         MVI   MSLEL,DELELQ        MARK IT DELETED                              
         GOTO1 HELLO,ELIST,(C'D',MST),('DELELQ',CPYRECD),0                      
ADDML110 LA    R4,ELEMENT          SET R4 TO "NEW" ELEMENT                      
         GOTO1 HELLO,ELIST,(C'P',MST),CPYRECD,ELEMENT,0                         
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD ELEMENT                            
         B     COM3XIT                                                          
*                                                                               
ADDMLTAB DS    0X                                                               
         DC    C'.123456789......ABC'      X'10' CONVERTS TO X'C1'              
ADDMLTAX DS    0X                                                               
*                                                                               
ADDTYPTB DS    0X                          TS INPUT TYPES TO LOCK               
         DC    AL1(CTRYUSA),AL1(6),AL1(3)                                       
***      DC    AL1(27)                     27   REMOVED 9/9/98                  
         DC    AL1(41)                     41                                   
         DC    AL1(49)                     49                                   
         DC    AL1(97)                     97 (SALRY HISTORY INPUT)             
*                                                                               
         DC    AL1(CTRYCAN),AL1(6),AL1(3)                                       
***      DC    AL1(27)                     27                                   
         DC    AL1(41)                     41                                   
         DC    AL1(49)                     49                                   
         DC    AL1(97)                     97 (SALRY HISTORY INPUT)             
*                                                                               
         DC    AL1(CTRYGBR),AL1(6),AL1(3)                                       
         DC    AL1(17)                     17                                   
         DC    AL1(49)                     49                                   
         DC    AL1(97)                     97 (SALRY HISTORY INPUT)             
*                                                                               
         DC    AL1(CTRYGER),AL1(6),AL1(3)                                       
         DC    AL1(27)                     27                                   
         DC    AL1(49)                     49                                   
         DC    AL1(97)                     97 (SALRY HISTORY INPUT)             
         DC    X'FF'                                                            
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
**********************************************************************          
* DUMP A RECORD (OLD FASHIONED WAY)                                  *          
**********************************************************************          
         SPACE 1                                                                
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),ZEROS                                                   
         BNE   COM3XIT                                                          
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    COM3XIT                                                          
         LA    R3,GETC                                                          
         L     R6,ARECORD                                                       
         B     DMPREC                                                           
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    COM3XIT                                                          
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),ZEROS                                                   
         BNE   COM3XIT                                                          
         LA    R3,PUTC                                                          
         L     R6,ARECORD                                                       
*                                                                               
         USING CACRECD,R6                                                       
DMPREC   SR    R4,R4                                                            
         ICM   R4,3,CACRLEN                                                     
         GOTO1 PRNTBL,DMCB,(L'PUTC,(R3)),(R6),C'DUMP',(R4),=C'2D'               
         B     COM3XIT                                                          
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT ADDTRN RECORDS TO RECOVERY                           *         
***********************************************************************         
         SPACE 1                                                                
VRECOVER DS    0H                                                               
         LR    R2,R0               R2=A(ADDTRN DMCB)                            
         LM    RE,RF,0(R2)         ACCOUNT COMMANDS                             
         MVC   COMMANDS,0(RE)      SET COMMANDS                                 
         MVC   ARECORD,12(R2)                                                   
         CLC   0(L'MST,RF),MST     TEST ACCMST D/A FILE                         
         BNE   VRECO60                                                          
         L     R6,ARECORD          ADDR OF I/O AREA                             
         USING CACRECD,R6                                                       
*                                                                               
         CLC   COMMANDS,ADDREC     ADDREC COMMAND?                              
         BE    VRECO10                                                          
         CLC   COMMANDS,PUTREC     PUTREC COMMAND?                              
         BE    VRECO10                                                          
         CLC   COMMANDS,GETREC     GETREC COMMAND?                              
         BNE   VRECOX                                                           
*                                                                               
         CLC   CACKCPY,RCCOMPFL    IS IT A SPECIAL RECORD?                      
         BNE   VRECO05             YES- CONTINUE                                
         CLC   CACKCULC,SPACES     NO- ONLY WANT CTRA HIST GETS                 
         BNH   VRECOX                                                           
VRECO05  TM    UPSI,DUMPRECS       DUMP OF RECORD                               
         BNO   *+8                                                              
         BAS   RE,DMPGET                                                        
         B     VRECO50                                                          
*                                                                               
VRECO10  DS    0H                  CHECK FILE SPACE AVAILABLE                   
         CP    FILCOUNT,=P'10000'  EVERY X NUMBER OF RECORDS                    
         BL    VRECO20                                                          
         GOTO1 ACTRAVL,DMCB,(2,ADCOMFAC),LOGIO                                  
         ZAP   FILCOUNT,=P'0'                                                   
         B     *+10                                                             
VRECO20  AP    FILCOUNT,=P'1'                                                   
         TM    UPSI,POSTREP        POSTING REPORT                               
         BNO   VRECO30             NO - CONTINUE                                
*                                                                               
         CLC   CACKCPY,RCCOMPFL    IS IT A SPECIAL RECORD?                      
         BNE   VRECO30                                                          
         XC    BUKELEM,BUKELEM                                                  
         LA    R2,BUKELEM                                                       
         USING BUKELD,R2                                                        
         MVI   BUKEL,BUKELQ        SET UP HISTORY BUCKET ELEMENT                
         MVI   BUKLN,BUKLNQ        LENGTH                                       
         MVC   BUKMOS,MOS          MONTH OF SERVICE                             
*                                                                               
         GOTO1 HELLO,ELIST,(C'G',MST),('BUKELQ',CACRECD),              X        
               (L'BUKMOS,BUKMOS)                                                
         CLI   ELERR,0                                                          
         BNE   VRECO30             NO MATCHING BUCKET ELEMENT                   
*                                                                               
         USING BKLIND,R3                                                        
         LA    R3,P                               PRINT LINE                    
         L     R2,ELADDR                          ADDR OF ELEMENT               
         MVC   BKACCT,CACKUNT                     ACCOUNT                       
         MVC   BKCNTRA,CACKCUNT                   CONTRA                        
         MVC   BKBKTP,CACKBTYP                    BUCKET TYPE                   
         CURED BUKDR,(L'BKDEB,BKDEB),2,MINUS=YES                                
         CURED BUKCR,(L'BKCRD,BKCRD),2,MINUS=YES                                
         GOTO1 ACREPORT                                                         
         AP    POSTDR,BUKDR                                                     
         AP    POSTCR,BUKCR                                                     
*                                                                               
VRECO30  TM    UPSI,DUMPRECS       DUMP OF RECORD                               
         BNO   *+8                                                              
         BAS   RE,DMPPUT                                                        
         CLC   COMMANDS,PUTREC     PUTREC COMMAND?                              
         BE    VRECOX              YES -  YOUR DONE                             
*                                                                               
* ADDREC COMMAND                                                                
*                                                                               
         L     RF,ARECORD                                                       
         MVC   HALF,CACRLEN-CACRECD(RF)                                         
         LA    R0,ACNTNUM                                                       
         L     RF,CONTTAB          COUNT # OF RECORDS ADDED TO FILE             
         USING CONTD,RF                                                         
         CLC   HALF,CONTSIZ                                                     
         BNH   VRECO40                                                          
         LA    RF,CONTLNQ(RF)                                                   
         BCT   R0,*-14                                                          
         SH    RF,=Y(CONTLNQ)            DROP IT IN THE LAST ONE                
VRECO40  AP    CONTER,=P'1'                                                     
         DROP  RF                                                               
*                                                                               
VRECO50  CLC   CACKCPY,RCCOMPFL    IS IT A SPECIAL RECORD?                      
         BNE   VRECO52             YES- CONTINUE                                
         OC    CHDKNULL-CHDRECD(L'CHDKNULL,R6),CHDKNULL-CHDRECD(R6)             
         BZ    VRECOX              C HEADERS DONT NEED TO GO TO RCV             
*                                                                               
VRECO52  L     R5,RCVIO            ADDR OF RECOVERY I/O AREA                    
         USING CSTRCVD,R5                                                       
         XC    CSTRCVD(CSTHDLN),CSTRCVD         CLEAR HEADER                    
         MVC   CSTBMOS,MOS         BUCKET MONTH FOR POSTING                     
         MVI   CSTRECTY,CSTRCPYQ   MARK AS A COPY                               
         CLC   COMMANDS,ADDREC     ADDREC COMMAND?                              
         BNE   *+8                 NO                                           
         MVI   CSTRECTY,CSTRADDQ   MARK AS AN ADD                               
         LH    RF,CACRLEN                                                       
         LA    R1,CSTHDLN(RF)      ADD RECOVERY HEADER TO REC LENGTH            
         STH   R1,CSTRLEN          STORE LENGTH OF RECOVERY RECORD              
         LA    RF,CSTRECRD         RECEIVING FIELD (THE RECOVERY REC)           
         LH    R1,CACRLEN          LENGTH                                       
         LA    RE,CACKEY           SENDING FIELD                                
         MOVE  ((RF),(R1)),(RE)                                                 
         PUT   COSTRCV,CSTRCVD     PUT RECORD TO RECOVERY                       
         B     VRECOX                                                           
*                                                                               
VRECO60  CLC   0(L'DIR,RF),DIR     TEST ACCMST D/A FILE                         
         BNE   VRECOX                                                           
*                                                                               
         USING PLDRECD,R6                                                       
         L     R6,ARECORD                                                       
         CLI   PLDKTYP,PLDKTYPQ    X'18' - DIRECTORY P&L RECORDS                
         BNE   VRECOX                                                           
         CLI   PLDKSUB,PLDKSUBQ    X'CD' - DIRECT TIME SUB                      
         BE    VRECO62                                                          
         CLI   PLDKSUB,PLCKSUBQ    X'CC' - CLIENT P&L SUB                       
         BNE   VRECOX                                                           
         USING PLCRECD,RF          18CC                                         
         L     RF,PLCSAV           ONLY PUT PLCRECD TO RECOVERY ONCE            
         CLC   PLCKEY,0(R6)                                                     
         BE    VRECOX                                                           
         MVC   PLCKEY,0(R6)        SAVE                                         
         DROP  RF                                                               
*                                                                               
VRECO62  CLC   COMMANDS,DMADD      ADD COMMAND?                                 
         BE    VRECO64                                                          
         CLC   COMMANDS,DMWRT      WRITE COMMAND?                               
         BE    VRECO64                                                          
         CLC   COMMANDS,DMREAD     READ COMMAND?                                
         BNE   VRECOX                                                           
         TM    UPSI,DUMPRECS       DUMP OF RECORD                               
         BNO   *+8                                                              
         BAS   RE,DMPGET                                                        
         B     VRECO66                                                          
*                                                                               
VRECO64  TM    UPSI,DUMPRECS       DUMP OF RECORD                               
         BNO   *+8                                                              
         BAS   RE,DMPPUT                                                        
         CLC   COMMANDS,DMWRT      DMWRT COMMAND?                               
         BE    VRECOX              YES - YOUR DONE                              
*                                                                               
VRECO66  L     R5,RCVIO            ADDR OF RECOVERY I/O AREA                    
         USING CSTRCVD,R5                                                       
         XC    CSTRCVD(CSTHDLN),CSTRCVD         CLEAR HEADER                    
         MVC   CSTBMOS,MOS         BUCKET MONTH FOR POSTING                     
         MVI   CSTRECTY,CSTRCPYQ   MARK AS A COPY                               
         CLC   COMMANDS,DMADD      DMADD DIR COMMAND?                           
         BNE   *+8                 NO                                           
         MVI   CSTRECTY,CSTRADDQ   MARK AS AN ADD                               
         LA    RF,L'PLDKEY+L'PLDKSTA+L'PLDKAMT+L'PLDKDA                         
         STH   RF,HALF                                                          
         LA    R1,CSTHDLN(RF)      ADD RECOVERY HEADER TO REC LENGTH            
         STH   R1,CSTRLEN          STORE LENGTH OF RECOVERY RECORD              
         LA    RF,CSTRECRD         RECEIVING FIELD (THE RECOVERY REC)           
         LH    R1,HALF             LENGTH                                       
         LA    RE,PLDKEY           SENDING FIELD                                
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVC   MSG,=CL10'RECOVERY'                                              
         SR    R2,R2                                                            
         ICM   R2,3,CSTRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),CSTRCVD,(R2)                                     
*                                                                               
         PUT   COSTRCV,CSTRCVD     PUT RECORD TO RECOVERY                       
*                                                                               
VRECOX   DS    0H                  ADDTRN REQURES AN EQUAL CONDITION            
         CR    RB,RB               ON EXITING ANY HOOK ROUTINE                  
         B     COM3XIT                                                          
         DROP  R2,R3,R5,R6                                                      
         EJECT                                                                  
**********************************************************************          
*  ADJUST YTD-1 FOR ANY ACCOUNT/CONTRA COMBO THAT IS NOT MARKED AS   *          
*  USED BY THIS ALLOCATION BY ZEROING OUT THE YTD-1 WITH A NEGATIVE  *          
*  BUCKET ENTRY FOR THIS MONTH.  LOOP THRU ROUTINE 1ST WITH YTD2BUF  *          
*  (THE 1R OVH YTD-1'S) THEN YTD1BUF (THE 1C YTD-1'S) THRU TSAR      *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R5                                                          
VYTDADJ  DS    0H                                                               
         TM    UPSI,POSTRATE       POST RATE RECORDS ONLY?                      
         BO    VYADJX              YES - SKIP ADJUSTING ENTRIES TO SORT         
         L     R5,YTD2BUF          ADDR OF 1R OVH YTD-1 TABLE                   
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         BZ    VYADJ20             NOTHING IN TABLE                             
         MVI   LEDG,EMPLDGR        THIS IS 1R OVH ACCOUNT                       
         LA    R4,BINTABLE         1ST TABLE ENTRY                              
         USING YTD1D,R4                                                         
VYADJ10  TM    YTDSTAT,YTD1USED    HAS THIS YTD-1 BEEN USED                     
         BO    *+12                YES- GET NEXT TABL ENTRY                     
         ST    R4,ALTAB            PASS ADDR OF RECORD                          
         BAS   RE,YADJOUT          NO - GO BACK IT OUT                          
         LA    R4,YTDLEN(R4)                                                    
         BCT   R3,VYADJ10                                                       
*                                                                               
VYADJ20  MVI   LEDG,CLILDGR        1C LEDGER                                    
         L     R4,AYTDWRK                                                       
         USING YTD1D,R4                                                         
         MVC   YTD1D(YTDLEN),SPACES                                             
*        L     R1,TSARBLK1         HIGH STORAGE 1C YTD-1 TSARBLK                
*        USING TSARD,R1                                                         
*        OC    TSPRECN(4),TSPRECN  ANY RECORDS IN TABLE?                        
*        BZ    VYADJX              NO - THEN JUST LEAVE                         
*        MVI   TSOFFACT,TSARDH     READHIGH                                     
*        ST    R4,TSAREC           KEY OF THE RECORD FOR LOOKUP                 
*        GOTO1 ATSAROFF                                                         
*        TM    TSERRS,ALL-TSERNF   ANY ERRORS EXCEPT REC NOT FOUND?             
*        BZ    VYADJ40             NO - CONTINUE                                
*        DC    H'0'                                                             
*                                                                               
         LA    R0,BBUFFARDH                                                     
         L     RF,=A(YTDBUF)                                                    
         GOTO1 BUFFERIN,DMCB,((R0),(RF)),(0,(R4)),ADCOMFAC                      
         CLI   4(R1),0                                                          
         BNE   VYADJX                                                           
         B     VYADJ40                                                          
                                                                                
VYADJ30  DS    0H                                                               
*        L     R1,TSARBLK1                                                      
*        MVI   TSOFFACT,TSANXT     READ NEXT                                    
*        GOTO1 ATSAROFF                                                         
*        BE    VYADJ40                                                          
*        TM    TSERRS,TSEEOF       END OF BUFFER?                               
*        BO    VYADJX              YES - WE'RE DONE                             
*        DC    H'0'                NO - CHECK CONDITION CODE                    
*                                                                               
         LA    R0,BBUFFASEQ         READ SEQUENTIAL                             
         L     RF,=A(YTDBUF)                                                    
         GOTO1 BUFFERIN,DMCB,((R0),(RF)),(0,(R4)),ADCOMFAC                      
         CLI   4(R1),0                                                          
         BE    VYADJ40                                                          
         TM    4(R1),X'80'         END OF FILE?                                 
         BO    VYADJX              YES - WE'RE DONE                             
*                                                                               
VYADJ40  TM    YTDSTAT,YTD1USED    HAS THIS YTD-1 BEEN USED                     
         BO    VYADJ30             YES- GET NEXT TABL ENTRY                     
***      CLC   YTDCNTLD,OPTDIRLG   1C CONTRA 14?                                
***      BE    VYADJ30             DON'T ADJUST THESE                           
         ST    R4,ALTAB            PASS ADDR OF RECORD                          
         BAS   RE,YADJOUT          NO - GO BACK IT OUT                          
         B     VYADJ30                                                          
*                                                                               
VYADJX   B     COM3XIT             WE'RE DONE                                   
         DROP  R4,R5                                                            
         EJECT                                                                  
**********************************************************************          
* MAKE YTD-1 POSTING TO SORTER                                       *          
*                                                                    *          
* 1- FOR 1R OVH WE ARE MAKING MINUS POSTING FOR                      *          
*    YTD-1 TO CURRENT MONTH.                                         *          
* 2- FOR 1C WE ARE EITHER MAKING THE YTD-1 MINUS                     *          
*    POSTING OR ZEROING OUT THE CURRENT MONTH IF                     *          
*    IF THERE IS NO YTD-1.                                           *          
**********************************************************************          
         SPACE 1                                                                
         USING YTD1D,R4                                                         
YADJOUT  NTR1                                                                   
         L     R4,ALTAB                                                         
         L     R2,ASRTWRK                                                       
         USING SRTD,R2                                                          
         GOTO1 CLERSRT,DMCB,(R2)                                                
         CLI   LEDG,EMPLDGR                 IS THIS 1R OVH ACCOUNT?             
         BE    VYAOUT10                                                         
         BAS   RE,VALCON                                                        
         BNE   VYAOUTX                      CONTRA NOT VALID FOR REQ            
         USING HYBND,R3                                                         
         LA    R3,NMEWRK                    NMEWRK FILLED IN BY VALCON          
         MVC   SRTCNAME,HYNAME              CONTRA NAME                         
         MVI   SRTLEDG,CLILDGR              1C LEDGER                           
         LA    R1,SRTCR                     POINT R1 TO SRTCR FOR 1C            
         B     VYAOUT20                                                         
*                                                                               
VYAOUT10 MVI   SRTLEDG,EMPLDGR              THIS IS 1R OVH ACCOUNT              
         MVC   NMEWRK,SPACES                                                    
         USING BCLID,R3                                                         
         LA    R3,NMEWRK                                                        
         MVC   BCLICDE,YTDCON               LOOKUP CLIENT NAME                  
         GOTO1 BINSRC,DMCB,NMEWRK,NMEBUFF,NMEWRK                                
         MVC   SRTCNAME,BCLINME                                                 
         LA    R1,SRTDR                     POINT R1 TO SRTDR FOR 1R            
*                                                                               
VYAOUT20 MVC   SRTACC,YTDACC                ACCOUNT                             
         MVC   SRTCLGAC,YTDCNTRA            CONTRA                              
         MVC   SRTMETHD,METHOD              METHOD                              
         MVC   SRTBTYPE,YTDBTYP             BUCKET TYPE                         
         ZAP   0(L'SRTBUKS,R1),YTD1AMT      YTD-1 TO BE ADJUSTED OUT            
         MP    0(L'SRTBUKS,R1),=P'-1'       REVERSE SIGN                        
         AP    PDEBITS,SRTDR                ADD TO POSTING TOTALS               
         AP    PCREDITS,SRTCR                                                   
         GOTO1 ADSORTER,DMCB,(L'PUTC,PUTC),(R2)                                 
*                                                                               
         TM    UPSI,PRINTADJ                PRINT ADJ RECORDS                   
         BNO   VYAOUTX                                                          
         USING BKLIND,R3                                                        
         LA    R3,P                               PRINT LINE                    
         MVC   BKACCT,SRTULAC                     ACCOUNT                       
         MVC   BKCNTRA,SRTCULAC                   CONTRA                        
         MVC   BKCNME,SRTCNAME                    CONTRA NAME                   
         MVC   BKBKTP,SRTBTYP                     BUCKET TYPE                   
         CURED SRTDR,(L'BKDEB,BKDEB),2,MINUS=YES                                
         CURED SRTCR,(L'BKCRD,BKCRD),2,MINUS=YES                                
         GOTO1 ACREPORT                                                         
*                                                                               
VYAOUTX  B     COM3XIT                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT SORTER BUCKET REPORT                                          *         
***********************************************************************         
         SPACE 1                                                                
VBUKREPT DS    0H                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST HEADING                          
         MVC   PAGE,=H'1'          AND PAGE NUMBER                              
         MVI   RCSUBPRG,8                                                       
         XC    ALSORT,ALSORT                                                    
         BAS   RE,GETSORT          GET 1ST SORT REC(ALSTWRK)                    
         OC    ALSORT,ALSORT       ANY AT ALL?                                  
         BZ    VBUKR20             NO                                           
         B     *+8                                                              
VBUKR10  BAS   RE,GETSORT          GET A SORT REC(ALSTWRK)                      
         USING BKLIND,R3                                                        
         USING SRTD,R5                                                          
         L     R5,ALSTWRK                         ADDR OF SAVED SRT REC         
         LA    R3,P                               PRINT LINE                    
         MVC   BKACCT,SRTULAC                     ACCOUNT                       
         MVC   BKCNTRA,SRTCULAC                   CONTRA                        
         MVC   BKCNME,SRTCNAME                    CONTRA NAME                   
         MVC   BKBKTP,SRTBTYP                     BUCKET TYPE                   
         CURED SRTDR,(L'BKDEB,BKDEB),2,MINUS=YES                                
         CURED SRTCR,(L'BKCRD,BKCRD),2,MINUS=YES                                
         GOTO1 ACREPORT                                                         
         OC    ALSORT,ALSORT                      WAS THAT LAST ONE?            
         BNZ   VBUKR10                            NO- GET THE NEXT ONE          
*                                                                               
VBUKR20  DS    0H                                                               
         CURED PDEBITS,(L'BKDEB,BKDEB),2,MINUS=YES                              
         CURED PCREDITS,(L'BKCRD,BKCRD),2,MINUS=YES                             
         GOTO1 ACREPORT                                                         
*                                                                               
VBUKRX   DS    0H                                                               
         BAS   RE,PALREP           PRINT OUT THE PROFIT/LOSS RECORDS            
         B     COM3XIT                                                          
         EJECT                                                                  
***********************************************************************         
* PRINT PROFIT AND LOSS RECORDS FROM THE P AND L TABLE                *         
***********************************************************************         
         SPACE 1                                                                
PALREP   NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE FIRST HEADING                          
         MVC   PAGE,=H'1'          AND PAGE NUMBER                              
         MVI   RCSUBPRG,22                                                      
*                                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         USING BIND,R1                                                          
         L     R1,PALBUFF          R1=A(PAL RECORD TABLE)                       
         ICM   R3,15,BININ                                                      
         BZ    PALR90                                                           
         LA    R2,BINTABLE                                                      
         DROP  R1                                                               
*                                                                               
         L     R5,APALWRK                                                       
         MVC   0(PLLNQ,R5),0(R2)   MOVE HIGH CORE ENTRY TO REG STORAGE          
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,P                                                             
*                                                                               
         XC    LSTPAL,LSTPAL       CLEAR SAVED FIELD                            
         LA    RE,PALTNUM          NUMBER OF BUCKETS                            
         LA    RF,PALTOTS          START OF BUCKETS                             
         ZAP   0(L'PALTOTS,RF),ZEROS                                            
         LA    RF,L'PALTOTS(RF)                                                 
         BCT   RE,*-10                                                          
*                                                                               
         ZAP   PDUMP,ZEROS                                                      
         LA    R1,PALTOTS          SET UP INITIAL LOOP                          
         LA    R0,4                                                             
         B     PALR20                                                           
*                                                                               
         USING PLBUFD,R5                                                        
PALR10   DS    0H                                                               
         L     R5,APALWRK                                                       
         MVC   0(PLLNQ,R5),0(R2)   MOVE HIGH CORE ENTRY TO REG STORAGE          
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         SR    R0,R0               INITIALIZE BUMPER                            
         CLC   LSTPMTH,PLMTHD      SAME METHOD?                                 
         BNE   PALR30                                                           
         AHI   R0,1                BUMP UP BUMPER                               
         CLC   LSTP1CA,PL1CACC     SAME 1C ACCOUNT                              
         BNE   PALR20                                                           
         AHI   R0,1                BUMP UP BUMPER                               
         CLC   LSTP1RA,PL1RACC     SAME 1R ACCOUNT                              
         BNE   PALR20                                                           
         AHI   R0,1                BUMP UP BUMPER                               
         CLC   LSTPANL,PLANAL      SAME ANALYSIS ACCOUNT                        
         BNE   PALR20                                                           
         AHI   R0,1                BUMP UP BUMPER                               
*                                                                               
PALR20   DS    0H                                                               
         STC   R0,BYTE             SAVE LOOP COUNTER                            
         LTR   R0,R0                                                            
         BZ    PALR30                                                           
         CHI   R0,4                                                             
         BE    PALR80                                                           
         CHI   R0,3                                                             
         BE    PALR60                                                           
         LA    RF,PALR50                                                        
         CHI   R0,1                                                             
         BH    *+8                                                              
         LA    RF,PALR40                                                        
         BR    RF                                                               
*                                                                               
PALR30   DS    0H                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1RA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1RATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1RATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1CA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1CATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1CATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   PMTHD(5),=C'TOTAL'                                               
         ZAP   DUB,PKMTHTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKMTHTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         B     PALR70                                                           
*                                                                               
PALR40   DS    0H                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1RA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1RATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1RATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1CA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1CATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1CATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         B     PALR70                                                           
*                                                                               
PALR50   DS    0H                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1RA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1RATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1RATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         B     PALR70                                                           
*                                                                               
PALR60   DS    0H                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
*                                                                               
PALR70   LTR   R0,R0                                                            
         BNZ   PALR80                                                           
         GOTO1 ACREPORT                                                         
*                                                                               
PALR80   DS    0H                                                               
         MVC   MSG,=CL10'PALREP'                                                
         GOTO1 ADUMP,DMCB,(RC),(R5),PLLNQ                                       
*                                                                               
         MVC   PRTLNE(PLNQ),SPACES                                              
         MVC   PMTHD,PLMTHD        METHOD                                       
         MVC   P1RA,PL1RACC        1R ACCOUNT                                   
         MVC   P1CA,PL1CACC        1C ACCOUNT                                   
         MVC   PANAL,PLANAL        ANALYSIS CODE                                
         ZAP   DUB,PLAMNT                                                       
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R0,PALTNUM          NUMBER OF BUCKETS                            
         LA    R1,PALTOTS          SET UP INITIAL LOOP                          
         AP    0(L'PALTOTS,R1),PLAMNT     ADD AMOUNT TO TOTALS                  
         LA    R1,L'PALTOTS(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         MVC   LSTPAL,PLMTHD       UPDATE LAST REC                              
*                                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         LA    R2,PLLNQ(R2)                                                     
         BCT   R3,PALR10                                                        
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1RA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1RATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1RATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1CA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1CATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1CATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   PMTHD(5),=C'TOTAL'                                               
         ZAP   DUB,PKMTHTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKMTHTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         DROP  R5                                                               
*                                                                               
PALR90   MVI   FORCEHED,C'Y'       FORCE FIRST HEADING                          
         MVC   PAGE,=H'1'          AND PAGE NUMBER                              
         MVI   RCSUBPRG,23                                                      
*                                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         USING BIND,R1                                                          
         L     R1,YTDPLTB          R1=A(YTD PAL RECORD TABLE)                   
         ICM   R3,15,BININ                                                      
         BZ    PALRX                                                            
         LA    R2,BINTABLE                                                      
         DROP  R1                                                               
*                                                                               
         L     R5,AYTDPWRK                                                      
         MVC   0(YTDPLNQ,R5),0(R2) MOVE HIGH CORE ENTRY TO REG STORAGE          
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,P                                                             
*                                                                               
         XC    LSTPAL,LSTPAL       CLEAR SAVED FIELD                            
         LA    RE,PALTNUM          NUMBER OF BUCKETS                            
         LA    RF,PALTOTS          START OF BUCKETS                             
         ZAP   0(L'PALTOTS,RF),ZEROS                                            
         LA    RF,L'PALTOTS(RF)                                                 
         BCT   RE,*-10                                                          
*                                                                               
         ZAP   PDUMP,ZEROS                                                      
         LA    R1,PALTOTS          SET UP INITIAL LOOP                          
         LA    R0,4                                                             
         B     PALR110                                                          
*                                                                               
         USING YTDPLD,R5                                                        
PALR100  DS    0H                                                               
         L     R5,AYTDPWRK                                                      
         MVC   0(YTDPLNQ,R5),0(R2) MOVE HIGH CORE ENTRY TO REG STORAGE          
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         TM    YTDPSTAT,YTDPPST    WAS THIS COMBO POSTED TO?                    
         BO    PALR180             YES-DONT PRINT                               
*                                                                               
         DS    0H                                                               
         SR    R0,R0               INITIALIZE BUMPER                            
         CLC   LSTPMTH,YTDPMTHD    SAME METHOD?                                 
         BNE   PALR30                                                           
         AHI   R0,1                BUMP UP BUMPER                               
         CLC   LSTP1CA,YTDP1C      SAME 1C ACCOUNT                              
         BNE   PALR110                                                          
         AHI   R0,1                BUMP UP BUMPER                               
         CLC   LSTP1RA,YTDP1R      SAME 1R ACCOUNT                              
         BNE   PALR110                                                          
         AHI   R0,1                BUMP UP BUMPER                               
         CLC   LSTPANL,YTDPANAL    SAME ANALYSIS ACCOUNT                        
         BNE   PALR110                                                          
         AHI   R0,1                BUMP UP BUMPER                               
*                                                                               
PALR110  DS    0H                                                               
         STC   R0,BYTE             SAVE LOOP COUNTER                            
         LTR   R0,R0                                                            
         BZ    PALR120                                                          
         CHI   R0,4                                                             
         BE    PALR170                                                          
         CHI   R0,3                                                             
         BE    PALR150                                                          
         LA    RF,PALR140                                                       
         CHI   R0,1                                                             
         BH    *+8                                                              
         LA    RF,PALR130                                                       
         BR    RF                                                               
*                                                                               
PALR120  DS    0H                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1RA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1RATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1RATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1CA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1CATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1CATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   PMTHD(5),=C'TOTAL'                                               
         ZAP   DUB,PKMTHTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKMTHTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         B     PALR160                                                          
*                                                                               
PALR130  DS    0H                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1RA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1RATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1RATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1CA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1CATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1CATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         B     PALR160                                                          
*                                                                               
PALR140  DS    0H                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1RA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1RATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1RATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         B     PALR160                                                          
*                                                                               
PALR150  DS    0H                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
*                                                                               
PALR160  LTR   R0,R0                                                            
         BNZ   PALR170                                                          
         GOTO1 ACREPORT                                                         
*                                                                               
PALR170  DS    0H                                                               
         MVC   MSG,=CL10'PALREP'                                                
         GOTO1 ADUMP,DMCB,(RC),(R5),PLLNQ                                       
*                                                                               
         MVC   PRTLNE(PLNQ),SPACES                                              
         MVC   PMTHD,YTDPMTHD      METHOD                                       
         MVC   P1RA,YTDP1R         1R ACCOUNT                                   
         MVC   P1CA,YTDP1C         1C ACCOUNT                                   
         MVC   PANAL,YTDPANAL      ANALYSIS CODE                                
         ZAP   DUB,YTDPAMNT                                                     
         MP    DUB,=P'-1'                                                       
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R0,PALTNUM          NUMBER OF BUCKETS                            
         LA    R1,PALTOTS          SET UP INITIAL LOOP                          
         AP    0(L'PALTOTS,R1),DUB        ADD AMOUNT TO TOTALS                  
         LA    R1,L'PALTOTS(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
PALR180  MVC   LSTPAL,YTDPMTHD     UPDATE LAST REC                              
*                                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         LA    R2,PLLNQ(R2)                                                     
         BCT   R3,PALR100                                                       
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         MVC   PANAL(5),=C'TOTAL'                                               
         ZAP   DUB,PKANLTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKANLTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1RA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1RATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1RATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   P1CA(5),=C'TOTAL'                                                
         ZAP   DUB,PK1CATOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PK1CATOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
         MVC   PMTHD(5),=C'TOTAL'                                               
         ZAP   DUB,PKMTHTOT                                                     
         BAS   RE,EDAMNT           EDIT AMOUNT INTO PRINT LINE                  
         ZAP   PKMTHTOT,ZEROS                                                   
         GOTO1 ACREPORT                                                         
*                                                                               
PALRX    DS    0H                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
         B     COM3XIT                                                          
*                                                                               
EDAMNT   ST    RE,FULL                                                          
         CURED DUB,PAMNT,2,MINUS=YES                                            
         L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R4,R5                                                            
         EJECT                                                                  
**********************************************************************          
* ACQUIRE CORE FOR BINARY TABLES / BUFFALO                           *          
**********************************************************************          
         SPACE 1                                                                
VMAIN    DS    0H                                                               
         L     R0,=A(LENBUFF)            ACQUIRE AN ADDITIONAL BUFFER           
         GETMAIN R,LV=(0)                                                       
         ST    R1,ABUFF                  SAVE A(BUFFER)                         
         LR    R2,R1                     R2=BUFFER POINTER                      
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         STCM  R2,15,MCUSRDMP            PRINT THE BUFFER IN A DUMP             
         LR    RF,R2                                                            
         A     RF,=A(LENBUFF)                                                   
         STCM  RF,15,MCUSRDMP+4          END OF DUMP AREA                       
         DROP  RE                                                               
*                                        INIT BINARY TABLES                     
         USING MAIND,R3                                                         
         USING BIND,R2                                                          
         LA    R0,MAINNUM                NUMBER OF BUFFERS NEEDED               
         L     R3,MAINTAB                TABLE OF BINARY PARMS                  
VMAIN10  MVC   0(L'MAINEYE,R2),MAINEYE   SEED TABLE EYE CATCHER                 
         LA    R2,L'MAINEYE(R2)                                                 
         SR    RE,RE                                                            
         ICM   RE,3,MAINAST              DISP TO TAB ADDR IN WORK STOR          
         AR    RE,RC                     ADD ADDR OF WORKING STORAGE            
         ST    R2,0(RE)                  SAVE ADDR OF TABLE                     
         XC    BIND(BINLENQ),BIND        BUILD BIN TABLE PARMS                  
         MVC   BINLEN+2(2),MAINLEN                                              
         MVC   BINDISPK+2(2),MAINDISK                                           
         MVC   BINMAX+2(2),MAINMAX                                              
         MVC   BINNUMB,MAINNUMB                                                 
         MVC   BINFRST,MAINFRST                                                 
         MVC   BINSTAT,MAINSTAT                                                 
         ICM   RE,15,MAINSIZE                                                   
         AR    R2,RE                     BUMP BY LENGTH OF TABLE                
         LA    R3,MAINLNQ(R3)                                                   
         BCT   R0,VMAIN10                                                       
*                                        INIT BINARY TABLES                     
         USING WORKMD,R3                                                        
         LA    R0,MWRKNUM                NUMBER OF WORK AREAS NEEDED            
         L     R3,WORKTAB                TABLE OF WORK AREAS                    
VMAIN20  SR    RE,RE                                                            
         ICM   RE,3,WORKAST              DISP TO TAB ADDR IN WORK STOR          
         AR    RE,RC                     ADD ADDR OF WORKING STORAGE            
         ST    R2,0(RE)                  SAVE ADDR OF TABLE                     
         SR    RE,RE                                                            
         ICM   RE,3,WORKSIZE                                                    
         AR    R2,RE                     BUMP BY LEN OF WORK AREA               
         LA    R3,WORKLNQ(R3)                                                   
         BCT   R0,VMAIN20                                                       
*                                        SET BUFFALOC                           
         ST    R2,ADBUFC                                                        
         USING BUFFALOD,R4                                                      
         L     R4,ADBUFC                                                        
         XC    BUFFCNTL,BUFFCNTL         CLEAR BUFFALO CONTROL AREA             
         MVI   BUFFCNTL,BUFKLEN          1ST BYTE IS THE KEY LENGTH             
         MVI   BUFFCNTL+1,ASCEND         2ND BYTE IS ASCENDING                  
         MVI   BUFFCNTL+2,FOX            3RD BYTE IS X'FF'                      
         MVC   BUFFLKEY,=A(BUFKLEN)      LENGTH OF THE KEY                      
         MVC   BUFFLDTA,=A(BDATLN)       LENGTH OF THE DATA                     
         MVC   BUFFLALL,=A(BRECLN)       LENGTH OF THE RECORD                   
         MVC   BUFFROWS,=A(BUFROW)       # OF ACCUM ROWS                        
         MVC   BUFFCOLS,=A(BBKCNT)       # OF ACCUM COLS                        
         MVI   BUFFFLVR,PACKED           FLAVOR                                 
         MVC   BUFFCRMX,=A(BUFMAX)       MAX NUMBER OF LINES IN CORE            
         MVI   BUFFFLIP,FILINDC          FILE INDICATOR                         
*                                                                               
         GOTO1 BUFFALO,DMCB,(L'SETC,SETC),ADBUFC                                
         DROP  R2                                                               
*                                                                               
         L     R0,=A(LENBUFF2)     ACQUIRE ABOVE THE LINE BUFFER                
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF               TEST IF STORAGE ACQUIRED                     
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
         ST    R1,PALBUFF                SAVE BEGINNING A(BUFFERS)              
         L     RE,=A(PLSIZE)             BUMP TO NEXT BUFFER                    
         AR    R1,RE                                                            
         ST    R1,YTDPLTB                                                       
*                                                                               
         LA    RF,*+10             HIGH CORE TABLE                              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                31 BIT MODE                                  
*                                                                               
         USING BIND,RE                                                          
         L     RE,PALBUFF                                                       
         XC    BININ,BININ               CLEAR BIN TABLE                        
         LA    R1,PLLNQ                                                         
         STCM  R1,15,BINLEN              RECORD LENGTH                          
         XC    BINDISPK,BINDISPK         DISPLACEMENT TO KEY                    
         LA    R1,PLKLNQ                                                        
         STC   R1,BINDISPK+3             LENGTH OF KEY                          
         L     R1,=A(PLMAX)                                                     
         STCM  R1,15,BINMAX              MAXIMUM NUMBER OF ENTRIES              
         MVI   BINNUMB,PLBUKCNT          NUMBER OF BUCKETS                      
         MVI   BINFRST,PLAMNT-PLREC      DISP TO FIRST BUCKET                   
*                                                                               
         L     RE,YTDPLTB                                                       
         XC    BININ,BININ               CLEAR BIN TABLE                        
         LA    R1,YTDPLNQ                                                       
         STCM  R1,15,BINLEN              RECORD LENGTH                          
         XC    BINDISPK,BINDISPK         DISPLACEMENT TO KEY                    
         LA    R1,YTDKLNQ                                                       
         STC   R1,BINDISPK+3             LENGTH OF KEY                          
         L     R1,=A(YTDPMAX)                                                   
         STCM  R1,15,BINMAX              MAXIMUM NUMBER OF ENTRIES              
         MVI   BINNUMB,YTDPBKCT          NUMBER OF BUCKETS                      
         MVI   BINFRST,YTDPAMNT-YTDPREC  DISP TO FIRST BUCKET                   
         DROP  RE                                                               
*                                                                               
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
VMAINX   B     COM3XIT                                                          
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO WRAP UP REPORT                                      *          
*     FREE UP ACQUIRED CORE                                          *          
**********************************************************************          
         SPACE 1                                                                
VWRAP    DS    0H                                                               
         L     R1,ABUFF                                                         
         L     R0,=A(LENBUFF)                                                   
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     RE,VEXTRAS                                                       
         USING RUNXTRAD,RE                                                      
         L     RE,ADMASTD                                                       
         USING MASTD,RE                                                         
         XC    MCUSRDMP(8),MCUSRDMP CLEAR OUT EXTRA DUMP AREA                   
         B     COM3XIT                                                          
         DROP  RE                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD OFFICE\GROUP TABLE                            *          
**********************************************************************          
         SPACE 1                                                                
VBOFFL   DS    0H                                                               
         L     R7,AOFFLTAB         R7=A(OFFICE TABLE)                           
         USING OFFLD,R7                                                         
         MVI   0(R7),X'FF'                                                      
         XC    TABCOUNT,TABCOUNT                                                
*                                                                               
         CLI   OFF1RLN,ONEBYTE     AGY ON ONE BYTE OFFICES?                     
         BE    VBOFF70                                                          
*                                                                               
         L     R6,RECORD                                                        
         USING OFFRECD,R6          BUILD KEY OF OFFICE RECORD                   
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,RCCOMPFL                                                 
         MVC   SAVEKEY,OFFKEY      SAVE START KEY                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,OFFRECD,OFFRECD                
         B     VBOFF20                                                          
*                                                                               
VBOFF10  GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),DIR,OFFRECD,OFFRECD                
VBOFF20  CLC   SAVEKEY(2),OFFKEY   SAME TYPE AND COMPANY                        
         BNE   VBOFFX                                                           
         MVC   DA,OFFKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,OFFRECD,DMWORK              
         XC    OFFLD(OFFLOFF-OFFLD),OFFLD                                       
         LA    R1,OFFLOFF-OFFLD    SET MINIMUM LENGTH                           
         STCM  R1,3,OFFLLEN                                                     
         MVC   OFFLCDE,OFFKOFF     OFFICE CODE                                  
         LA    R2,OFFRECD+(OFFRFST-OFFRECD)                                     
         SR    R1,R1                                                            
*                                                                               
VBOFF30  CLI   0(R2),0             TEST END-OF-RECORD                           
         BE    VBOFF60                                                          
         CLI   0(R2),X'20'         R2=NAME ELEMENT                              
         BNE   VBOFF40                                                          
         MVC   OFFLNME,SPACES                                                   
         USING NAMELD,R2                                                        
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFLNME(0),NAMEREC  NAME TO TABLE                                
         B     VBOFF50                                                          
*                                                                               
VBOFF40  CLI   0(R2),X'D2'         TEST OFFICE LIST ELEMENT                     
         BNE   VBOFF50                                                          
         USING OFLELD,R2           R2=OFFICE LIST ELEMENT                       
         SR    RE,RE                                                            
         IC    RE,OFFLNUM          NUMBER IN LIST (SO FAR)                      
         SLL   RE,1                X 2                                          
         LA    RE,OFFLOFF(RE)      RE=A(END OF LIST)                            
         SR    RF,RF                                                            
         IC    RF,OFLLN            ELEMENT LENGTH                               
         SH    RF,=Y(OFLLN1Q)      LESS THE FIXED FIELDS                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),OFLNTRY     MOVE LIST TO W/S                             
         SRL   RF,1                RF=NUMBER OF ENTRIES JUST ADDED              
         SR    RE,RE                                                            
         IC    RE,OFFLNUM          RE=NUMBER OF ENTRIES SO FAR                  
         AR    RE,RF                                                            
         STC   RE,OFFLNUM                                                       
         SLL   RE,1                                                             
         AH    RE,=Y(OFFLOFF-OFFLD) PLUS FIXED FIELDS                           
         STCM  RE,3,OFFLLEN        SET NEW LENGTH                               
*                                                                               
VBOFF50  ICM   R1,1,1(R2)          BUMP TO NEXT ELEMENT                         
         BZ    VBOFF60                                                          
         AR    R2,R1                                                            
         B     VBOFF30                                                          
*                                                                               
VBOFF60  SR    RF,RF                                                            
         ICM   RF,3,OFFLLEN        LENGTH OF LAST ENTRY                         
         AR    R7,RF                                                            
         MVI   0(R7),X'FF'         MARK END OF TABLE                            
         L     RF,AOFFLTAB         RF=A(OFFICE TABLE)                           
         AH    RF,=Y(OFFTLEN)      PLUS TABLE MAXIMUM                           
         CR    R7,RF               TEST PASSED END OF TABLE                     
         BNH   VBOFF10                                                          
         DC    H'0'                OFFICE TABLE IS FULL                         
*                                                                               
* BUILD FOR ONE BYTE OFFICES                                                    
*                                                                               
VBOFF70  DS    0H                                                               
         L     R6,RECORD                                                        
         USING CTUREC,R6           USER PROFILE RECORDS                         
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,C'U'                                                     
         MVI   CTUKSYS,C'A'                                                     
         MVC   SAVEKEY,CTUKEY                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),=C'CTFILE ',RECORD,       X        
               RECORD                                                           
         B     VBOFF90                                                          
*                                                                               
VBOFF80  NI    BIT,X'FF'-TWOPGLST                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),=C'CTFILE ',RECORD,       X        
               RECORD                                                           
VBOFF90  CLC   SAVEKEY(CTUKPROG-CTUKEY),CTUKEY        CHANGE OF SYSTEM?         
         BNE   VBOFFX                                                           
*                                                                               
         CLI   CTUKPROG+1,C'$'      INDICATES OFFICE LIST (1ST PAGE)            
         BE    *+12                                                             
         CLI   CTUKPROG,C'$'        INDICATES OFFICE LIST (2ND PAGE)            
         BNE   VBOFF80                                                          
*                                                                               
         CLC   CTUKAGY,ALPHAID     RIGHT AGENCY?                                
         BNE   VBOFF80                                                          
         ZIC   R5,TABCOUNT                                                      
         LA    R5,1(R5)                                                         
         STC   R5,TABCOUNT                                                      
*                                                                               
* MUST CHECK IF THIS IS THE 2ND PAGE OF THE OFFICE LIST.  IT IS                 
* IDENTIFIED BY AN 'A' IN THE NAME EG $2A.  ADD THIS TO THE $2 ENTRY            
*                                                                               
         L     R7,AOFFLTAB                                                      
VBOFF100 CLI   0(R7),X'FF'                                                      
         BE    VBOFF130                                                         
         CLI   CTUKPROG,0          FIRST BYTE IS ZERO WHEN ON 1ST PAGE          
         BNE   VBOFF110                                                         
         CLC   OFFLCDE(1),CTUKPROG+2    IS THIS THE 2ND PAGE                    
         BE    VBOFF120                                                         
         B     *+14                                                             
VBOFF110 CLC   OFFLCDE(1),CTUKPROG+1                                            
         BE    VBOFF120                                                         
         LH    R1,OFFLLEN                                                       
         AR    R7,R1                                                            
         B     VBOFF100                                                         
*                                                                               
VBOFF120 OI    BIT,TWOPGLST        INDICATE ON 2ND PAGE                         
         LA    RE,OFFLOFF          MUST FIND LAST ENTRY SO FAR                  
         LH    R1,=Y(MAXOLIST)     MAX IN LIST (32)                             
         OC    0(2,RE),0(RE)       IS THERE AN OFFICE                           
         BZ    VBOFF140            NO, SO ADD TO THIS SPOT                      
         LA    RE,2(RE)                                                         
         BCT   R1,*-14                                                          
         DC    H'0'                                                             
*                                                                               
VBOFF130 XC    OFFLD(OFFLOFF-OFFLD),OFFLD                                       
         MVC   OFFLCDE(1),CTUKPROG+2      1ST BYTE=0 2ND=$ IF ON 1ST PG         
         MVC   OFFLNME(2),OFFLCDE           OFFICE LIST HAS NO NAME             
         OC    OFFLNME,SPACES                                                   
         LA    R1,OFFLOFF-OFFLD          SET MAXIMUM LENGTH                     
         AH    R1,=Y(MAXOLIST*2)         32 MAX                                 
         STCM  R1,3,OFFLLEN                                                     
*                                                                               
VBOFF140 LA    R2,CTUDATA                                                       
         USING CTPVD,R2                                                         
VBOFF150 CLI   CTPVEL,0                                                         
         BE    VBOFF190                                                         
         CLI   CTPVEL,X'72'              PROFILE VALUE ELEMENT                  
         BNE   VBOFF180                                                         
         ZIC   R1,CTPVLEN                ELEMENT LENGHT                         
         SH    R1,=Y(CTPVALUE-CTPVEL)    LESS THE FIXED FIELDS                  
         BNM   *+6                                                              
         DC    H'0'                                                             
         TM    BIT,TWOPGLST            IF ON 2ND PAGE ALREADY POINTS            
         BO    *+8                     TO RIGHT AREA                            
         LA    RE,OFFLOFF                                                       
         LA    RF,CTPVALUE                                                      
VBOFF160 CLI   0(RF),C'0'                NOT A VALID OFFICE                     
         BE    VBOFF170                                                         
         MVC   0(1,RE),0(RF)             MOVE OFFICE TO W/S                     
         ZIC   R5,OFFLNUM                                                       
         LA    R5,1(R5)                                                         
         STC   R5,OFFLNUM                                                       
         LA    RE,2(RE)                  AND KEEP AS TWO BYTES EACH             
VBOFF170 LA    RF,1(RF)                  BUMP TO NEXT OFFICE IN ELEM            
         BCT   R1,VBOFF160                                                      
*                                                                               
VBOFF180 ICM   R1,1,1(R2)                BUMP TO NEXT ELEMENT                   
         BZ    VBOFF190                                                         
         AR    R2,R1                                                            
         B     VBOFF150                                                         
*                                                                               
VBOFF190 DS    0H                                                               
         TM    BIT,TWOPGLST        IS THIS THE 2ND PAGE                         
         BO    VBOFF80             READ SEQ                                     
         LA    R1,OFFLOFF-OFFLD          SET MAXIMUM LENGTH                     
         AH    R1,=Y(MAXOLIST*2)         32 MAX                                 
         AR    R7,R1                                                            
         MVI   0(R7),X'FF'         MARK END OF TABLE                            
         L     RF,AOFFLTAB         RF=A(OFFICE TABLE)                           
         AH    RF,=Y(OFFTLEN)      PLUS TABLE MAXIMUM                           
         CR    R7,RF               TEST PASSED END OF TABLE                     
         BNH   VBOFF80             READ SEQ                                     
         DC    H'0'                OFFICE TABLE IS FULL                         
*                                                                               
VBOFFX   B     COM3XIT                                                          
         DROP  R2,R6,R7                                                         
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO GET AN OFFICE\OFFICE GROUP ENTRY                               
**********************************************************************          
         SPACE 1                                                                
         USING OFFLD,R7                                                         
VGOFFL   DS    0H                                                               
         L     R7,AOFFLTAB         R7=A(OFFICE TABLE)                           
         SR    R1,R1                                                            
*                                                                               
VGOFF10  CLI   0(R7),X'FF'                                                      
         BNE   *+10                OFFICE NOT FOUND                             
         LTR   RB,RB                                                            
         B     COM3XIT                                                          
         CLC   OFFG,OFFLCDE        MATCH CODE                                   
         BNE   *+12                                                             
         ST    R7,AOFFL            RETURN ADDRESS OF ENTRY                      
         B     COM3XIT                                                          
         ICM   R1,3,OFFLLEN        LENGTH OF ENTRY                              
         AR    R7,R1               BUMP R7 TO NEXT                              
         B     VGOFF10                                                          
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD 1N CLIENT LIST TABLE                          *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R5             DOES ANY 1N HAVE LIST ATTACHED?              
VBCLIL   DS    0H                                                               
         L     R5,NONCLTBF         ADDR OF 1N NON-CLIENT TABLE                  
         ICM   R3,15,BININ         NUMBER OF TABLE ENTRIES                      
         BZ    COM3XIT                                                          
         CLC   SAVEMET,METHOD      SAME METHOD AS LAST REQUEST?                 
         BE    COM3XIT             THEN NO NEED TO BUILD AGAIN                  
*                                  CLEAR LIST TABLE TO BINARY ZEROS             
         L     RE,ALISTAB          RECEIVING FIELD                              
         LA    RF,LSTLEN           RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R5,BINTABLE         1ST TABLE ENTRY                              
         USING NOND,R5                                                          
         MVI   BYTE,C'Y'           SET FIRST TIME THRU                          
VBCL10   TM    NONSTAT,NONSLST     LISTS PRESENT?                               
         BZ    VBCL90                                                           
         MVC   NONLMET,SPACES                                                   
         MVC   NONLISC,SPACES                                                   
         L     R6,RECORD           RECORD READING AREA                          
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                CLEAR THE KEY                       
         MVC   ACTKCPY,RCCOMPFL             COMPANY                             
         MVC   ACTKUNT(L'NONCLI),NONCLI     UNIT/LEDGER TO READ                 
         MVC   ACTKACT,NONCODE                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,RECORD,RECORD                  
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,ACTKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,RECORD,DMWORK               
         TM    8(R1),ALL-PASSDEL                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,ACTRECD+(ACTRFST-ACTRECD)                                     
         MVC   WORK,SPACES                                                      
VBCL20   CLI   0(R2),0                                                          
         BE    VBCL70                                                           
         CLI   0(R2),FFTELQ                CLIENT LIST                          
         BE    VBCL40                                                           
VBCL30   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    VBCL20                                                           
         DC    H'0'                                                             
*                                                                               
VBCL40   DS    0H                                                               
         USING FFTELD,R2                                                        
         CLI   FFTTYPE,FFTTINDL    INDIRECT CLIENT LIST?                        
         BNE   VBCL30                                                           
         SR    R0,R0                                                            
         ICM   R0,1,FFTDLEN        LENGTH OF DATA                               
         SRDA  R0,32                                                            
         D     R0,=A(NONLLEN)                                                   
         LTR   R0,R0               SHOULD BE NO REMAINDER                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         LTR   R1,R1               MAKE SURE YOU HAVE MINI ELS                  
         BP    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1               USE R0 AS MINI EL COUNTER                    
         LA    R1,FFTDATA                                                       
VBCL50   CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   WORK(L'NONLISC),1(R1)     SAVE DEFAULT LIST CODE                 
         B     VBCL60                    BUT CONTINUE TO LOOK                   
         CLC   0(1,R1),METHOD                                                   
         BNE   VBCL60                                                           
         MVC   NONLMET,0(R1)                                                    
         MVC   NONLISC,1(R1)                                                    
         B     VBCL80                                                           
VBCL60   LA    R1,NONLLEN(R1)                                                   
         BCT   R0,VBCL50                                                        
         B     VBCL30                    LOOK FOR ANOTHER FFTEL                 
*                                                                               
VBCL70   CLC   WORK,SPACES                                                      
         BE    VBCL90                                                           
         MVC   NONLMET,METHOD      SET WITH DEFAULT LIST                        
         MVC   NONLISC,WORK                                                     
*                                                                               
VBCL80   BAS   RE,LISTIT           TABLE OF LISTS FOR THIS METHOD               
VBCL90   LA    R5,NONLEN(R5)                                                    
         BCT   R3,VBCL10                                                        
*                                                                               
VBCLX    B     COM3XIT                                                          
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF LISTS FOR THIS METHOD                                *         
*       THEN LOOKUP THE LIST AND SAVE THE CLIENTS                     *         
*       R5 IS SET TO THE 1N TABLE ENTRY                               *         
***********************************************************************         
         SPACE 1                                                                
LISTIT   NTR1                                                                   
         USING NOND,R5                                                          
         USING LISTD,R7                                                         
         L     R7,ALISTAB                                                       
         LA    R0,LISTMAX                                                       
         LH    R3,=H'1'                                                         
LSTIT10  CLC   NONLISC,LISTCDE     ALREADY IN TABLE                             
         BE    LISTITX                                                          
         OC    LISTCDE,LISTCDE     BLANK ENTRY                                  
         BZ    LSTIT20                                                          
         AH    R3,=H'1'            BUMP INDEX                                   
         LA    R7,LISTLEN(R7)                                                   
         BCT   R0,LSTIT10                                                       
         DC    H'0'                LIST TABLE FULL                              
*                                                                               
LSTIT20  MVC   LISTCDE,NONLISC                                                  
         STCM  R3,3,LISTIDX        SET LIST INDEX NUMBER                        
*                                                                               
         L     R6,RECORD                                                        
         USING LSTRECD,R6          BUILD KEY OF LIST RECORD                     
         MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ                                                 
         MVC   LSTKCPY,RCCOMPFL                                                 
         MVC   LSTKLST(L'LISTCDE),LISTCDE     LIST CODE FOR LOOKUP              
         MVC   SAVEKEY,LSTKEY      SAVE START KEY                               
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,LSTRECD,LSTRECD                
         NI    DMINBTS,ALL-PASSDEL                                              
         TM    8(R1),X'10'         DID I FIND THE RECORD?                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,LSTKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,RECORD,DMWORK               
         TM    8(R1),ALL-PASSDEL                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,LSTRECD+(LSTRFST-LSTRECD)                                     
LSTIT30  CLI   0(R2),0                                                          
         BE    LISTITX                                                          
         CLI   0(R2),NAMELQ        NAME OF LIST                                 
         BE    LSTIT50                                                          
         CLI   0(R2),LIDELQ        CLIENTS IN LIST                              
         BE    LSTIT60                                                          
LSTIT40  SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     LSTIT30                                                          
*                                                                               
         USING NAMELD,R2                                                        
LSTIT50  SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTNME(0),NAMEREC                                               
         B     LSTIT40                                                          
*                                                                               
LSTIT60  DS    0H                                                               
         USING LIDELD,R2                                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LIDLN            ELEMENT LENGTH                               
         SH    R1,=H'10'                                                        
         SR    RE,RE                                                            
         IC    RE,LIDITLN          LENGTH OF ITEM                               
         DR    R0,RE               R0=NUMBER OF ITEMS                           
         LR    R0,R1                                                            
         LA    R3,LIDDACCS         LIST ITEMS                                   
         LA    R4,WORK                                                          
         USING CLTLD,R4                                                         
LSTIT70  MVC   WORK,SPACES                                                      
         MVC   CLTLIDX,LISTIDX                                                  
         XC    CLTLSTAT,CLTLSTAT                                                
         SR    R1,R1                                                            
         IC    R1,LIDITLN          LENGTH OF DATA                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLTLCODE(0),0(R3) LIST ITEM                                      
         GOTO1 BINADD,DMCB,(R4),CLTLBUFF ADD TO TABLE                           
         SR    R1,R1                                                            
         IC    R1,LIDITLN          LENGTH OF DATA                               
         LA    R3,0(R1,R3)                                                      
         BCT   R0,LSTIT70                                                       
         B     LSTIT40             GET NEXT ELEMENT                             
*                                                                               
LISTITX  B     COM3XIT                                                          
         DROP  R2,R4,R5,R6,R7                                                   
         EJECT                                                                  
*-------------------------------------------                                    
*        LITERAL POOL                                                           
*-------------------------------------------                                    
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
COSTRCV  DCB   DDNAME=COSTRCV,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               LRECL=RCVSIZE,BLKSIZE=RCVSIZE+4                                  
*                                                                               
ABILITY3 EQU   (4095*3)-(*-VCOMMON3)  REMAINING ADDRESSIBILITY                  
         EJECT                                                                  
**********************************************************************          
* ROUTINES ENTERABLE FROM BASE OR WORK NMODS                         *          
* ************WORK NMOD # 4 ****************                         *          
**********************************************************************          
         SPACE 1                                                                
         DS    0D                                                               
VCOMMON4 NMOD1 0,**COMM4**,R9,R8                                                
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         LA    RC,SPACEND                                                       
         USING ACCAD,RC            RC=A(LOCAL W/S)                              
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     VSUMPOST            1 -DO SUMMARY RECORDS                        
         B     VCONCK              2 -CHECK FOR VALID CONTRAS                   
         DC    (COMSPAR4*L'COM4)X'00'                                           
*                                                                               
*                                                                               
COM4XIT  XIT1  ,                                                                
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO ADD CLIENT TOTALS TO OFF DPT TOTALS                    *           
*         BUILD INDIRECT POOLS                                      *           
*********************************************************************           
         SPACE 1                                                                
VSUMPOST DS    0H                                                               
         USING CLID,R7                                                          
         L     R7,ACLREC                    CLIENT CONTRA ACCUMS                
*                                                                               
         MVC   WORK(L'CLILEDAC),CLILEDAC    MOVE ACCOUNT INTO WORK              
         CLI   CLILEDG,C'N'                 IS THIS 1N                          
         BE    SUMP10                                                           
         TM    CLISTAT,CLINB+CLIPB+CLIHS    IF IT'S NEW BIZ CLIENT              
         BZ    SUMP20                       REPLACE KEY                         
SUMP10   MVC   WORK(3),=C'N  '                                                  
         MVC   WORK+1(L'CLIANALS),CLIANALS                                      
         CLI   CLIANALS,PERANAL             SHOULD NOT BE HERE IF P             
         BE    COM4XIT                                                          
*                                                                               
         USING SUMPD,R4                                                         
SUMP20   L     R4,SUMPST                    TABLE OF SUMMARY ACCUMS             
SUMP30   CLI   SUMPKEY,EOT                  END OF TABLE                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,SUMPKLN                   R1 LENGTH OF KEY COMPARE            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SUMPKEY              COMPARE TO TABLE ENTRY              
         BE    SUMP40                                                           
         LA    R4,SUMPLN(R4)                BUMP TO NEXT TABL ENTRY             
         B     SUMP30                                                           
*                                                                               
SUMP40   L     RE,ADEPTOT                   DEPARTMENT ACCUMS                   
         LR    R1,RE                                                            
         AH    R1,SUMPLST                   R1 POINTS TO YTD-1 ACCUM            
         AP    0(SBKLN,R1),CCSTLST                                              
         LR    R1,RE                                                            
         AH    R1,SUMPYTD                   R1 POINTS TO YTD ACCUM              
         AP    0(SBKLN,R1),CCSTYTD                                              
         LR    R1,RE                                                            
         AH    R1,SUMPPST                   R1 POINTS TO POST ACCUM             
         AP    0(SBKLN,R1),CCSTPST                                              
*                                                                               
         CLI   CLILEDG,C'C'                 IS THIS A CLIENT                    
         BNE   *+12                                                             
         TM    CLISTAT,CLINB+CLIPB+CLIHS    IF IT'S NEW BIZ CLIENT              
         BZ    COM4XIT                      NO - DONE                           
*                                                                               
         USING INDID,R6                                                         
         L     R6,AINDWRK                   INDIRECT WORK AREA                  
         XC    INDID(INLEN),INDID                                               
         MVC   INDID(INKLEN),SPACES                                             
         LA    R1,INBK                      CLEAR IT                            
         LA    R0,INBUKCNT                                                      
         ZAP   0(INBKLN,R1),ZEROS                                               
         LA    R1,INBKLN(R1)                                                    
         BCT   R0,*-10                                                          
*                                                                               
         MVI   INDSALTP,0          SET SALARY TYPE TO ZERO                      
         SR    R1,R1                                                            
         IC    R1,CSTGRPLN                  LENGTH OF HYBRID 1R CONTRA          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INDOFDEP(0),ACCOUNT          OFFICE/DEPT(SUB?)                   
         MVC   INOFDPFM,INDOFDEP            OFFICE/DEPT FROM                    
         XC    INDSTATS,INDSTATS            MARK NOT ABSORBED                   
         MVI   INDTYPE,INDDEPT              DEPT IS DEFAULT                     
         MVI   INDORIG,INDDEPT              DEPT IS DEFAULT                     
         MVC   INDCLIST,CLICLIST            CLIENT LIST CODE                    
         TM    CLISTAT,CLINB+CLIPB+CLIHS    NEWBIZ PROBONO OR HOUSE?            
         BZ    SUMP170                      NO - SKIP THIS                      
         TM    CLISTAT,CLINB                NEWBIZ ?                            
         BZ    SUMP50                       YES                                 
         OI    INDSTATS,INDNEWBI            MARK AS NEW BUSINESS                
         CLI   OPTNBA,C'I'                                                      
         BE    SUMP70                       INCOME ALLOC NEEDS 1C OFFC          
         CLI   OPTNBA,C'H'                                                      
         BNE   *+8                                                              
         OI    INDSTATS,INDHOURS            HOURS ALLOCATED POOL                
         CLI   OPTNBA,C'P'                                                      
         BNE   *+8                                                              
         OI    INDSTATS,INDPLUS             DIR+DPT IND ALLOC POOL              
         CLI   OPTNBOFF,C'C'                USE 1C OFFICE FOR N.B.?             
         BE    SUMP80                                                           
         B     SUMP170                                                          
*                                                                               
SUMP50   TM    CLISTAT,CLIPB                NEWBIZ ?                            
         BZ    SUMP60                       YES                                 
         OI    INDSTATS,INDPROBO            MARK AS PRO BONO                    
         CLI   OPTPBA,C'I'                  INCOME ALLOC NEEDS 1C OFFC          
         BE    SUMP70                                                           
         CLI   OPTPBA,C'H'                  MARK AS HOUR POOL                   
         BNE   *+8                                                              
         OI    INDSTATS,INDHOURS            HOURS ALLOCATED POOL                
         CLI   OPTPBA,C'P'                                                      
         BNE   *+8                                                              
         OI    INDSTATS,INDPLUS             DIRECT + DPT IND ALLOC POOL         
         CLI   OPTPBOFF,C'C'                USE 1C OFFICE FOR P.B.?             
         BE    SUMP80                                                           
         B     SUMP170                                                          
*                                                                               
SUMP60   OI    INDSTATS,INDHOUSE            MARK AS HOUSE                       
         CLI   OPTHSA,C'I'                  INCOME ALLOC NEEDS 1C OFFC          
         BE    SUMP70                                                           
         CLI   OPTHSA,C'H'                  MARK AS HOURS POOL                  
         BNE   *+8                                                              
         OI    INDSTATS,INDHOURS            HOURS ALLOCATED POOL                
         CLI   OPTHSA,C'P'                                                      
         BNE   *+8                                                              
         OI    INDSTATS,INDPLUS             DIRECT + DPT IND ALLOC POOL         
         CLI   OPTHSOFF,C'C'                USE 1C OFFICE FOR HOUSE?            
         BE    SUMP80                                                           
         B     SUMP170                                                          
*                                                                               
SUMP70   OI    INDSTATS,INDINCOM            INCOME ALLOCATED POOL               
         B     SUMP90                                                           
*                                                                               
SUMP80   DS    0H                                                               
         LA    R1,CLIANALS                                                      
         CLC   CLIANALS,SPACES              LEVEL OF INDIRECT                   
         BH    *+8                                                              
         LA    R1,INDTYPE                                                       
         CLI   0(R1),INDCORP                AGY(CORP)INDIRECT?                  
         BE    SUMP170                                                          
SUMP90   MVC   INDOFDEP,SPACES              OFFICE/DEPT OF CLIENT               
         LA    R2,CLICDE                    CLIENT CODE                         
         SR    R0,R0                                                            
         IC    R0,OFFDISP                   DISPLACEMENT OF OFFICE              
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,OFFCLEN                   LENGTH OF OFFICE LEVEL              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INDOFDEP(0),0(R2)            OFFICE/DEPT OF CLIENT               
         OC    DEPDISP,DEPDISP              IS DEPT CODE IN 1C                  
         BZ    SUMP100                                                          
         LA    R2,CLICDE                    CLIENT CODE                         
         SR    R0,R0                                                            
         IC    R0,DEPDISP                                                       
         AR    R2,R0                                                            
         LA    R3,INDOFDEP                                                      
         SR    R0,R0                                                            
         IC    R0,OFFCLEN                                                       
         AR    R3,R0                                                            
         SR    R1,R1                                                            
         IC    R1,DPTLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                OFFICE/DEPT OF CLIENT               
*                                                                               
SUMP100  TM    INDSTATS,INDNEWBI    NEW BUSINESS?                               
         BZ    SUMP110                                                          
         CLI   OPTNBOFF,C'C'        USE 1C OFFICE FOR N.B.?                     
         BNE   SUMP170                                                          
         MVI   BYTE,CONBAL#                                                     
         B     SUMP130                                                          
SUMP110  TM    INDSTATS,INDPROBO    PRO BONO?                                   
         BZ    SUMP120                                                          
         CLI   OPTPBOFF,C'C'        USE 1C OFFICE FOR P.B.?                     
         BNE   SUMP170                                                          
         MVI   BYTE,COPBAL#                                                     
         B     SUMP130                                                          
SUMP120  TM    INDSTATS,INDHOUSE    HOUSE?                                      
         BZ    SUMP170                                                          
         CLI   OPTHSOFF,C'C'        USE 1C OFFICE FOR HOUSE?                    
         BNE   SUMP170                                                          
         MVI   BYTE,COPBAL#                                                     
*                                                                               
         USING CAPRECD,R5                                                       
SUMP130  L     R5,RECORD           RECORD READING AREA                          
         XC    CAPKEY(L'CDIO),CAPKEY                                            
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    X'3E'                                        
         MVI   CAPKSUB,CAPKSUBQ    X'09'                                        
         MVC   CAPKCPY,RCCOMPFL    COMPANY                                      
         MVC   CAPKMTHD,METHOD     METHOD OF ALLOCATION (NUMBER)                
         SR    R1,R1                                                            
         IC    R1,OFFCLEN                   LENGTH OF OFFICE LEVEL              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CAPKOFC(0),INDOFDEP         OFFICE OF CLIENT                     
         MVC   SAVEKEY,CAPKEY                                                   
         OI    DMINBTS,PASSDEL                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,RECORD,RECORD                  
         CLC   SAVEKEY(6),CAPKEY                                                
         BNE   SUMP170                     NO 1C OFFICE PROFILE                 
*                                                                               
         MVC   DA,CAPKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,RECORD,DMWORK               
         TM    8(R1),ALL-PASSDEL                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CAPRECD+(CAPRFST-CAPRECD)                                     
         USING OPDELD,R2                                                        
SUMP140  CLI   0(R2),0                                                          
         BE    SUMP170                                                          
         CLI   0(R2),OPDELQ        OPTION(PROFILE) DATA?                        
         BE    SUMP160                                                          
SUMP150  SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SUMP140             NEXT ELEMENT                                 
*                                                                               
SUMP160  DS    0H                                                               
         CLC   OPDNUM,BYTE                                                      
         BNE   SUMP150                                                          
         CLI   OPDDATA,CORPANAL    IF CLIENT OFF SAYS AGY LEVEL USE IT          
         BNE   SUMP170                                                          
         MVI   CLIANALS,CORPANAL                                                
         MVI   CLIANALR,CORPANAL                                                
*                                                                               
SUMP170  CLC   CLIANALS,SPACES              LEVEL OF INDIRECT                   
         BNH   *+10                                                             
         MVC   INDTYPE,CLIANALS                                                 
         CLC   CLIANALR,SPACES              LEVEL OF INDIRECT(ORIGINAL)         
         BNH   *+10                                                             
         MVC   INDORIG,CLIANALR                                                 
         CLI   INDTYPE,INDCORP             AGY(CORP)INDIRECT?                   
         BNE   *+12                                                             
         CLI   OPTINAA,C'P'                ALLOC AGY IND ON DIR+IND?            
         BE    SUMP190                     MARK IT                              
         CLI   INDTYPE,INDOFFIC            OFFICE INDIRECT?                     
         BE    SUMP180                                                          
         CLI   INDTYPE,INDGRUP             OFFICE GROUP INDIRECT?               
         BE    SUMP180                                                          
         CLI   INDTYPE,INDDEPT             DEPT INDIRECT?                       
         BNE   SUMP200                     MARK DPT TOO IN CASE IT GETS         
*                                          BUMPED UP                            
SUMP180  CLI   OPTINOA,C'P'                ALLOC OFFC IND ON DIR+IND?           
         BNE   SUMP200                                                          
SUMP190  OI    INDSTATS,INDPLUS             DIRECT + DPT IND ALLOC POOL         
SUMP200  L     R4,COMPRAT                   TBL DISP TO SAL TYP ACCUMS          
         USING CMPUTD,R4                                                        
SUMP210  OC    CMSLRD,CMSLRD                IF Y(0) SKIP THIS SAL TYPE          
         BZ    SUMP230                                                          
         L     R1,AINDWRK          INDIRECT WORK AREA                           
         L     R3,ACLREC                                                        
         AH    R3,CMPCLIDY                  DISP TO YTD ACCUM                   
         CLI   QOPT7,C'B'          BREAKDOWN INDIR POOLS BY PAYTYPE?            
         BNE   *+10                NO-THEN DON'T FILL IN SALARY TYPE            
         MVC   INDSALTP,CMBUKTYP   FILL IN SALARY TYPE                          
         AH    R1,CMPTYIND                                                      
         ZAP   0(INBKLN,R1),0(CBUKLEN,R3)                                       
         ST    R1,SVADDR           SAVE ADDRESS OF ACCUM                        
*                                                                               
         AP    INDTOTAC,0(CBUKLEN,R3)       KEEP TOTAL FOR ALL PAYTYPES         
         CLI   QOPT7,C'B'                                                       
         BNE   SUMP230                                                          
         L     R1,SVADDR                    POINT TO PAYTYPE AMOUNT             
         CP    0(INBKLN,R1),ZEROS           DON'T PUT ZERO POOL ENTRIES         
         BNZ   SUMP220                                                          
         L     R3,ACLREC                                                        
         AH    R3,CMPCLIDL                  DISP TO YTD-1 ACCUM                 
         ZAP   DUB,0(CBUKLEN,R3)            IF YOU HAD SOMETHING LAST           
         CP    DUB,ZEROS                    MONTH PUT OUT POOL TO ADJ           
         BNE   SUMP220                                                          
         B     SUMP230                                                          
*                                                                               
SUMP220  DS    0H                                                               
         CLI   QOPT7,C'B'                                                       
         BNE   SUMP230                                                          
         GOTO1 BINADD,DMCB,(R6),INDBUFF     ADD TO INDIRECT TABLE               
         ZAP   INDSALAC,ZEROS                                                   
         ZAP   INDBENAC,ZEROS                                                   
         ZAP   INDPENAC,ZEROS                                                   
*                                                                               
SUMP230  LA    R4,CMPULN(R4)                NEXT SALARY TYPE                    
         CLI   0(R4),EOT                    END OF TABLE                        
         BNE   SUMP210                                                          
         CLI   QOPT7,C'B'          BREAKDOWN INDIR POOLS BY TYPE                
         BE    SUMPX                                                            
         USING CMPUTD,R4                                                        
         L     R4,COMPRAT                   TBL DISP TO SAL TYP ACCUMS          
         LA    R1,INBK                      DON'T PUT ZERO POOL ENTRIES         
         LA    R0,INBUKCNT                                                      
SUMP240  OC    CMSLRD,CMSLRD       IF Y(0) SKIP THIS SAL TYPE                   
         BZ    SUMP250                                                          
         CP    0(INBKLN,R1),ZEROS                                               
         BNZ   SUMP270                                                          
         B     SUMP260                                                          
         LA    R1,INBKLN(R1)                                                    
SUMP250  LA    R4,CMPULN(R4)                                                    
         BCT   R0,SUMP240                                                       
         B     SUMPX                                                            
*                                                                               
SUMP260  L     R3,ACLREC                                                        
         AH    R3,CMPCLIDL                  DISP TO YTD-1 ACCUM                 
         ZAP   DUB,0(CBUKLEN,R3)            IF YOU HAD SOMETHING LAST           
         CP    DUB,ZEROS                    MONTH PUT OUT POOL TO ADJ           
         BNE   SUMP270                                                          
         LA    R1,INBKLN(R1)                                                    
         LA    R4,CMPULN(R4)                                                    
         BCT   R0,SUMP240                                                       
         B     SUMPX                                                            
*                                                                               
SUMP270  GOTO1 BINADD,DMCB,(R6),INDBUFF     ADD TO INDIRECT TABLE               
*                                                                               
SUMPX    B     COM4XIT                                                          
         DROP  R4,R6,R7                                                         
         EJECT                                                                  
*------------------------------------------------------------                   
*              ROUTINE TO CHECK IF 1C ACCOUNTS ARE VALID                        
*------------------------------------------------------------                   
*                                                                               
VCONCK   DS    0H                                                               
         L     R5,ABUFWRK                                                       
         USING BUFD,R5                                                          
         XC    BUFKEY(BUFKLEN),BUFKEY  SET KEY FOR READ HIGH                    
         MVC   BUFCLI,SPACES                                                    
         LA    RF,HIGHC                                                         
         B     *+8                                                              
CONCK02  LA    RF,SEQC                                                          
         SR    R0,R0                                                            
         LA    R0,COMP                                                          
         LA    R2,BUFNOGP          NO DIRECT GROUP                              
         STC   R2,BUFTYPE                                                       
         GOTO1 BUFFALO,DMCB,(L'SEQC,(RF)),((R2),ADBUFC),(R5),(R0)               
         TM    8(R1),EOF                                                        
         BO    CONCKX              EOF                                          
*                                                                               
*        MVC   P+1(L'BUFCLT),BUFCLT                                             
*        GOTO1 ACREPORT                                                         
         L     R6,DIO                                                           
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVC   ACTKUNT(L'CLILEG),CLILEG                                         
         MVC   ACTKACT,BUFCLT                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMREAD),DIR,DIO,DIO                        
         CLI   8(R1),0             DID I FIND THE RECORD?                       
         BE    CONCK02             YES - GET NEXT RECORD                        
         USING ERRORD,R7                                                        
         MVC   WORK,SPACES         NO - PUT OUT ERROR                           
         LA    R7,WORK                                                          
         MVI   ERRTYPE,ERRERROR  SET TO ERROR                                   
         MVI   ERRNUM,ERRCONTR                                                  
         MVC   ERRACCT(L'CLILEG),CLILEG                                         
         MVC   ERRACCT+L'CLILEG(L'BUFCLT),BUFCLT                                
         MVC   ERRAMNT,SPACES                                                   
         GOTO1 BINADD,DMCB,(R7),ERRORBUF ADD TO ERROR TABLE                     
         CLI   QOPT1,C'L'                                                       
         BNE   *+8                                                              
         OI    RUNSTAT,POSTERR                                                  
         B     CONCK02                                                          
*                                                                               
CONCKX   B     COM4XIT                                                          
         DROP  R5,R6,R7                                                         
         EJECT                                                                  
*-------------------------------------------                                    
*        LITERAL POOL                                                           
*-------------------------------------------                                    
*                                                                               
         LTORG                                                                  
ABILITY4 EQU   (4095*3)-(*-VCOMMON4)  REMAINING ADDRESSIBILITY                  
*                                     FREE UP ALL USINGS                        
         DROP                                                                   
         EJECT                                                                  
*                                                                               
*-------------------------------------------                                    
*        TABLES AND CONSTANTS                                                   
*-------------------------------------------                                    
*                                                                               
DEPTAB   DS    0F                                                               
*                                 'DIRECT TIME FOR DEPARTMENT '                 
         DCDD  AC#DTD,36                                                        
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SDIRLST-SUMD)                                                
         DC    AL1(SDIRYTD-SUMD)                                                
         DC    AL1(SDIRPST-SUMD)                                                
*                                 'D-DEPT. INDIRECT (ABSORBED)'                 
         DCDD  AC#DIA,36                                                        
         DC    AL2(ADEPCUM-ACCAD)                                               
         DC    AL1(DDINDAL-DEPD)                                                
         DC    AL1(DDINDAY-DEPD)                                                
         DC    AL1(DDINDAP-DEPD)                                                
*                                 'D-DEPT. INDIRECT (UNABSORBED)'               
         DCDD  AC#DIUA,36                                                       
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SDANDLST-SUMD)                                               
         DC    AL1(SDANDYTD-SUMD)                                               
         DC    AL1(0)                                                           
*                                 'O-OFFICE INDIRECT'                           
         DCDD  AC#OI,36                                                         
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SOANDLST-SUMD)                                               
         DC    AL1(SOANDYTD-SUMD)                                               
         DC    AL1(0)                                                           
*                                 'C-CORPORATE INDIRECT'                        
         DCDD  AC#CI,36                                                         
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SCANDLST-SUMD)                                               
         DC    AL1(SCANDYTD-SUMD)                                               
         DC    AL1(0)                                                           
         DC    X'FF'                                                            
*                                                                               
DEPOTAB  DS    0F                                                               
*                                 'OVERHEAD (ABSORBED)'                         
         DCDD  AC#OVHA,36                                                       
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SOVHLST-SUMD)                                                
         DC    AL1(SOVHYTD-SUMD)                                                
         DC    AL1(SOVHPST-SUMD)                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
OFFTAB   DS    0F                                                               
*                                 'DIRECT TIME FOR OFFICE     '                 
         DCDD  AC#DTO,36                                                        
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SDIRLST-SUMD)                                                
         DC    AL1(SDIRYTD-SUMD)                                                
         DC    AL1(SDIRPST-SUMD)                                                
*                                 'D-DEPT. INDIRECT (ABSORBED)'                 
         DCDD  AC#DIA,36                                                        
         DC    AL2(AOFFCUM-ACCAD)                                               
         DC    AL1(DDINDAL-DEPD)                                                
         DC    AL1(DDINDAY-DEPD)                                                
         DC    AL1(DDINDAP-DEPD)                                                
*                                 'D-DEPT. INDIRECT (UNABSORBED)'               
         DCDD  AC#DIUA,36                                                       
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SDANDLST-SUMD)                                               
         DC    AL1(SDANDYTD-SUMD)                                               
         DC    AL1(0)                                                           
*                                 'D-DEPT. AS OFFICE IND (ABSORBED)'            
         DCDD  AC#ODAB,36                                                       
         DC    AL2(AOFFCUM-ACCAD)                                               
         DC    AL1(OFINDADL-OFFD)                                               
         DC    AL1(OFINDADY-OFFD)                                               
         DC    AL1(OFINDADP-OFFD)                                               
*                                 'O-OFFICE INDIRECT (ABSORBED)'                
         DCDD  AC#OIAB,36                                                       
         DC    AL2(AOFFCUM-ACCAD)                                               
         DC    AL1(OFINDAL-OFFD)                                                
         DC    AL1(OFINDAY-OFFD)                                                
         DC    AL1(OFINDAP-OFFD)                                                
*                                 'O-OFFICE INDIRECT (UNABSORBED)'              
         DCDD  AC#OIUA,36                                                       
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SOANDLST-SUMD)                                               
         DC    AL1(SOANDYTD-SUMD)                                               
         DC    AL1(0)                                                           
*                                 'C-CORPORATE INDIRECT'                        
         DCDD  AC#CI,36                                                         
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SCANDLST-SUMD)                                               
         DC    AL1(SCANDYTD-SUMD)                                               
         DC    AL1(0)                                                           
         DC    X'FF'                                                            
*                                                                               
OFFOTAB  DS    0F                                                               
*                                 'DEPT. OVERHEAD (ABSORBED)'                   
         DCDD  AC#DOA,36                                                        
         DC    AL2(AOFFCUM-ACCAD)                                               
         DC    AL1(DOVHAL-DEPD)                                                 
         DC    AL1(DOVHAY-DEPD)                                                 
         DC    AL1(DOVHAP-DEPD)                                                 
*                                 'OFFICE OVERHEAD (ABSORBED)'                  
         DCDD  AC#OOA,36                                                        
         DC    AL2(AOFFCUM-ACCAD)                                               
         DC    AL1(OFOVHAL-OFFD)                                                
         DC    AL1(OFOVHAY-OFFD)                                                
         DC    AL1(OFOVHAP-OFFD)                                                
         DC    X'FF'                                                            
         EJECT                                                                  
CORTAB   DS    0F                                                               
*                                 'DIRECT TIME FOR AGENCY     '                 
         DCDD  AC#DTA,36                                                        
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SDIRLST-SUMD)                                                
         DC    AL1(SDIRYTD-SUMD)                                                
         DC    AL1(SDIRPST-SUMD)                                                
*                                 'D-DEPT. INDIRECT (ABSORBED)'                 
         DCDD  AC#DIA,36                                                        
         DC    AL2(AAGYCUM-ACCAD)                                               
         DC    AL1(DDINDAL-DEPD)                                                
         DC    AL1(DDINDAY-DEPD)                                                
         DC    AL1(DDINDAP-DEPD)                                                
*                                 'D-DEPT. INDIRECT (UNABSORBED)'               
         DCDD  AC#DIUA,36                                                       
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SDANDLST-SUMD)                                               
         DC    AL1(SDANDYTD-SUMD)                                               
         DC    AL1(SDANDPST-SUMD)                                               
*                                 'D-DEPT. AS OFFICE IND (ABSORBED)'            
         DCDD  AC#ODAB,36                                                       
         DC    AL2(AAGYCUM-ACCAD)                                               
         DC    AL1(OFINDADL-OFFD)                                               
         DC    AL1(OFINDADY-OFFD)                                               
         DC    AL1(OFINDADP-OFFD)                                               
*                                 'O-OFFICE INDIRECT (ABSORBED)'                
         DCDD  AC#OIAB,36                                                       
         DC    AL2(AAGYCUM-ACCAD)                                               
         DC    AL1(OFINDAL-OFFD)                                                
         DC    AL1(OFINDAY-OFFD)                                                
         DC    AL1(OFINDAP-OFFD)                                                
*                                 'O-OFFICE INDIRECT (UNABSORBED)'              
         DCDD  AC#OIUA,36                                                       
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SOANDLST-SUMD)                                               
         DC    AL1(SOANDYTD-SUMD)                                               
         DC    AL1(SOANDPST-SUMD)                                               
*                                 'C-CORPORATE INDIRECT (ABSORBED)'             
         DCDD  AC#CIA,36                                                        
         DC    AL2(AAGYCUM-ACCAD)                                               
         DC    AL1(AGINDAL-AGYD)                                                
         DC    AL1(AGINDAY-AGYD)                                                
         DC    AL1(AGINDAP-AGYD)                                                
*                                 'C-CORPORATE INDIRECT (UNABSORBED)'           
         DCDD  AC#CIUA,36                                                       
         DC    AL2(ADEPTOT-ACCAD)                                               
         DC    AL1(SCANDLST-SUMD)                                               
         DC    AL1(SCANDYTD-SUMD)                                               
         DC    AL1(SCANDPST-SUMD)                                               
*                                                                               
         DC    X'FF'                                                            
*                                                                               
COROTAB  DS    0F                                                               
*                                 'DEPT. OVERHEAD (ABSORBED)'                   
         DCDD  AC#DOA,36                                                        
         DC    AL2(AAGYCUM-ACCAD)                                               
         DC    AL1(DOVHAL-DEPD)                                                 
         DC    AL1(DOVHAY-DEPD)                                                 
         DC    AL1(DOVHAP-DEPD)                                                 
*                                 'OFFICE OVERHEAD (ABSORBED)'                  
         DCDD  AC#OOA,36                                                        
         DC    AL2(AAGYCUM-ACCAD)                                               
         DC    AL1(OFOVHAL-OFFD)                                                
         DC    AL1(OFOVHAY-OFFD)                                                
         DC    AL1(OFOVHAP-OFFD)                                                
*                                 'CORP. OVERHEAD (ABSORBED)'                   
         DCDD  AC#COA,36                                                        
         DC    AL2(AAGYCUM-ACCAD)                                               
         DC    AL1(AGOVHAL-AGYD)                                                
         DC    AL1(AGOVHAY-AGYD)                                                
         DC    AL1(AGOVHAP-AGYD)                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         DS    0D                  ALIGNMENT                                    
CRECORD  DS    CL(IOSIZE)          RECORD READ OR BUILD AREA                    
         DS    0D                  ALIGNMENT                                    
CRECORD2 DS    CL(IOSIZE)          2ND RECORD READ OR BUILD AREA                
         DS    0D                  ALIGNMENT                                    
CDIO     DS    CL100               DIRECTORY IO AREA                            
         DS    0D                  ALIGNMENT                                    
CPLCSAV  DS    CL100               PLCRECD DIRECTORY IO SAVE AREA               
         DS    0D                  ALIGNMENT                                    
CRCVIO   DS    CL(RCVSIZE)         RECOVERY I/O AREA                            
         DS    0D                  ALIGNMENT                                    
*COBLOCK DS    CL(L'COBLOCK+100)   COBLOCK FOR GETCAP                           
CCOBLOCK DS    CL(COBLOCKX-COBLOCK)    COBLOCK FOR GETCAP                       
         DS    0D                  ALIGNMENT                                    
CSALAREA DS    CL(SALLNQ)          BLOCK FOR SALHST                             
         DS    0D                  ALIGNMENT                                    
CUPDBLOK DS    CL(TRNBLKL)         BLOCK FOR ADDTRN                             
         DS    0D                  ALIGNMENT                                    
CTSRBLK1 DS    CL(TSARDL)          TSARBLK 1                                    
*                                                                               
CDDIN    DS    0D                  DATA DICTIONARY IN                           
         DCDDL AC#ERDI,36          **ERROR** REQUEST DATES INVALID'             
         DCDDL AC#ALICE,40         ALLOCATION INCOMPLETE CHECK FOR ERR          
         DCDDL AC#ENDTC,32         ** ERROR ** NO DIRECT TIME COSTS             
         DCDDL AC#NODTC,36         NO OFFICE DIRECT TIME- COST TO CORP          
         DCDDL AC#NDDTO,36         NO DIRECT TIME- COST TO OFFICE               
         DCDDL AC#DIRIT,27         DIRECT/INDIRECT TIME TOTALS                  
         DCDDL AC#AOFLV,36         ALLOCATE AT OFFICE LEVEL PER SCHEME          
         DCDDL AC#ACFLV,36         ALLOCATE AT CORP LEVEL PER SCHEME            
         DCDDL AC#NODIR,36         ** ERROR** NO DIRECT TIME PER SCHEME         
         DCDDL AC#POOLA,36         ** ERROR** POOL CANNOT BE ALLOCATED          
         DCDDL AC#STDHS,10         STD  HRS                                     
         DCDDL AC#PCTS,10          % TO STD                                     
         DCDDL AC#PCTD,10          % IN DPT                                     
         DCDDL AC#UNALL,16         UNALLOCATED POOL                             
         DCDDL AC#DRAFT,5          DRAFT                                        
         DCDDL AC#LIVE,5           LIVE                                         
         DCDDL AC#COVER,8          OVERHEAD                                     
         DCDDL AC#SUBT,9           SUB TOTAL                                    
         DCDDL AC#TTIME,10         TOTAL TIME                                   
         DCDDL AC#TOVER,15         TOTAL OVERHEAD                               
         DCDDL AC#METH,6           METHOD                                       
         DCDDL AC#TOTAL,5          TOTAL                                        
         DCDDL AC#AGYTS,13         AGENCY TOTALS                                
         DCDDL AC#OFFTS,13         OFFICE TOTALS                                
         DCDDL AC#GROUP,6          GROUP                                        
         DCDDL AC#YTD,3            YTD                                          
         DCDDL AC#OHFRO,23         -- O/H F R O Z E N **                        
         DCDDL AC#EXEF,15          ** FLAGGED AS EXEC **                        
         DCDDL AC#NOHRS,16         ** NO HOURS PRESENT **                       
         DCDDL AC#NOSLR,17         ** NO SALARY PRESENT **                      
         DCDDL AC#SLRAT,9          SALARY AT                                    
         DCDDL AC#SCHM9,23         SCHEME #9 DIRECT TIME ONLY                   
         DCDDL AC#LIST,5           LIST                                         
         DCDDL AC#CLI,6            CLIENT                                       
         DCDDL AC#NCLI,10          NON-CLIENT                                   
         DCDDL AC#PERD,6           PERIOD                                       
         DCDDL AC#RSHRS,3          HRS                                          
         DCDDL AC#APG72,7          ADJ HRS                                      
         DCDDL AC#COST,4           COST                                         
         DCDDL AC#PSTG,7           POSTING                                      
         DCDDL AC#AMT,6            AMOUNT                                       
         DCDDL AC#CONTD,15         CONTINUED                                    
         DC    X'00'                                                            
*                                                                               
*                                                                               
CCOMTAB  DS    0D                  TBLE FOR COMMON WORK NMOD ENTRIES            
         DC    AL2(COMMON-ACCAD),AL2(COM1-ACCAD),AL1(COMNUM1)                   
         DC    AL2(COMMON2-ACCAD),AL2(COM2-ACCAD),AL1(COMNUM2)                  
         DC    AL2(COMMON3-ACCAD),AL2(COM3-ACCAD),AL1(COMNUM3)                  
         DC    AL2(COMMON4-ACCAD),AL2(COM4-ACCAD),AL1(COMNUM4)                  
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
CDICTTAB DS    0F                  TBLE FOR DICTIONARY UPDATES                  
         DC    AL2(ADEPTAB-ACCAD),AL2(TABNME-TABD),AL2(TABLEN)                  
         DC    AL2(ADEPOTAB-ACCAD),AL2(TABNME-TABD),AL2(TABLEN)                 
         DC    AL2(AOFFTAB-ACCAD),AL2(TABNME-TABD),AL2(TABLEN)                  
         DC    AL2(AOFFOTAB-ACCAD),AL2(TABNME-TABD),AL2(TABLEN)                 
         DC    AL2(ACORTAB-ACCAD),AL2(TABNME-TABD),AL2(TABLEN)                  
         DC    AL2(ACOROTAB-ACCAD),AL2(TABNME-TABD),AL2(TABLEN)                 
         DC    AL2(SCHMDEF-ACCAD),AL2(SCHDEF-SCHDEFD),AL2(SCHDEFLN)             
         DC    AL2(POLDEFN-ACCAD),AL2(POOLDES-POOLD),AL2(POOLLN)                
         DC    AL2(ERRDEF-ACCAD),AL2(ERDESCRP-ERDEFD),AL2(ERDFLN)               
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
CCLRTAB  DS    0D                  TBLE FOR CLEARING BINARY TABLES              
         DC    AL2(SUMBUFF-ACCAD)  SUMMARY TABLE                                
         DC    AL2(OVHBUFF-ACCAD)  OVERHEAD TABLE                               
         DC    AL2(RATBUFF-ACCAD)  HRLY RATE TABLE                              
         DC    AL2(INDBUFF-ACCAD)  INDIRECT TABLE                               
         DC    AL2(HYNMBUF-ACCAD)  HYBRID TABLE                                 
         DC    AL2(ERRORBUF-ACCAD) ERROR TABLE                                  
         DC    AL2(YTD2BUF-ACCAD)  1R OVERHEAD YTD-1 TABLE                      
         DC    AL2(SCHEMBUF-ACCAD) ALLOCATION SCHEME TABLE                      
         DC    AL2(PALBUFF-ACCAD)  P AND L SPEC RECS (PLDREC) BUFFER            
         DC    AL2(YTDPLTB-ACCAD)  YTD P AND L RECS TABLE                       
CCLRNUM  EQU   (*-CCLRTAB)/2                                                    
*                                                                               
CCOMPRAT DS    0D                  TABLE TO COMPUTATE HRLY RATES                
         DC    AL1(PHRTSAL),CL3'SAL'                                            
         DC    Y(RSAL-RATD),Y(SALSALRY-SALARYD)                                 
         DC    Y(PSAL-PTOTD),Y(CCSTLST1-CLID),Y(CCSTYTD1-CLID)                  
         DC    Y(INDSALAC-INDID)                                                
*                                                                               
         DC    AL1(PHRTBEN),CL3'BEN'                                            
         DC    Y(RBEN-RATD),Y(SALBENFT-SALARYD)                                 
         DC    Y(PBEN-PTOTD),Y(CCSTLST2-CLID),Y(CCSTYTD2-CLID)                  
         DC    Y(INDBENAC-INDID)                                                
*                                                                               
         DC    AL1(PHRTPEN),CL3'PEN'                                            
         DC    Y(RPEN-RATD),Y(SALPENSN-SALARYD)                                 
         DC    Y(PPEN-PTOTD),Y(CCSTLST3-CLID),Y(CCSTYTD3-CLID)                  
         DC    Y(INDPENAC-INDID)                                                
*                                                                               
         DC    AL1(PHRTTOT),CL3'TOT'                                            
         DC    Y(RRAT-RATD),Y(0)                                                
         DC    Y(PCOST-PTOTD),Y(0),Y(0),Y(INDTOTAC-INDID)                       
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
CSCHMDEF DS    0D                  TABLE FOR SCHEME LITERAL DEFINITIONS         
*                               'DEFINED AS DEPT  '                             
         DC    AL1(SCHDDEP)                                                     
         DCDD  AC#SDAD,17                                                       
*                               'DEFINED AS OFFICE'                             
         DC    AL1(SCHDOFF)                                                     
         DCDD  AC#SDAO,17                                                       
*                               'DEFINED AS CORP  '                             
         DC    AL1(SCHDCORP)                                                    
         DCDD  AC#SDAC,17                                                       
*                               'BY OFFICE GROUP  '                             
         DC    AL1(SCHDOGP)                                                     
         DCDD  AC#SBOG,17                                                       
*                               'BY OFFICE GROUPII'                             
         DC    AL1(SCHDOGP2)                                                    
         DCDD  AC#SBOG2,17                                                      
*                               'OFFICE CLTS ONLY '                             
         DC    AL1(SCHDOCLT)                                                    
         DCDD  AC#OCLIO,17                                                      
*                               'EXCL FROM ALLOC  '                             
         DC    AL1(SCHDEXCL)                                                    
         DCDD  AC#EXALL,17                                                      
*                               'POST SAL AT 50%  '                             
         DC    AL1(SCHDPCT)                                                     
         DCDD  AC#PSAL,17                                                       
*                               'DIRECT TIME ONLY '                             
         DC    AL1(SCHDDIR)                                                     
         DCDD  AC#DIRO,17                                                       
*                               'OFFICE OVERRIDE  '                             
         DC    AL1(SCHDOVER)                                                    
         DCDD  AC#OOVR,17                                                       
*                               'BY OFFICE INCOME '                             
         DC    AL1(SCHDINCO)                                                    
         DCDD  AC#OINC,17                                                       
*                               'BY CORP INCOME   '                             
         DC    AL1(SCHDINCC)                                                    
         DCDD  AC#CINC,17                                                       
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
CSCHDFLT DS    0D                  TABLE FOR SCHEME LITERAL DEFAULTS            
         DC    AL1(DPTANAL),AL1(SCHDDEP)                                        
         DC    AL1(DEPT),AL1(SCHDDEP)                                           
         DC    AL1(OFFANAL),AL1(SCHDOFF)                                        
         DC    AL1(OFFICE),AL1(SCHDOFF)                                         
         DC    AL1(CORPANAL),AL1(SCHDCORP)                                      
         DC    AL1(COMP),AL1(SCHDCORP)                                          
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                                                               
*                                  TABLE FOR POSTING SUMMARY RECS(SUMD)         
CSUMPST  DS    0D                                                               
         DC    C'C  ',AL1(1),Y(SDIRLST-SUMD),Y(SDIRYTD-SUMD)                    
         DC    Y(SDIRPST-SUMD)                                                  
         DC    C'NO ',AL1(3),Y(SOANDLST-SUMD),Y(SOANDYTD-SUMD)                  
         DC    Y(SOANDPST-SUMD)                                                 
         DC    C'NC ',AL1(3),Y(SCANDLST-SUMD),Y(SCANDYTD-SUMD)                  
         DC    Y(SCANDPST-SUMD)                                                 
         DC    C'NG ',AL1(3),Y(SCANDLST-SUMD),Y(SCANDYTD-SUMD)                  
         DC    Y(SCANDPST-SUMD)                                                 
         DC    C'N  ',AL1(1),Y(SDANDLST-SUMD),Y(SDANDYTD-SUMD)                  
         DC    Y(SDANDPST-SUMD)                                                 
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                  TABLE FOR UPDATING OPTIONS                   
CSTDOPTT DS    0D                                                               
         DC    Y(OPTPERD-ACCAD),Y(CORUN-COBLOCKD)                               
         DC    Y(OPTYTD-ACCAD),Y(CODBASE-COBLOCKD)                              
         DC    Y(OPTOVHY-ACCAD),Y(COVBASE-COBLOCKD)                             
         DC    Y(OPTLOCK-ACCAD),Y(COLOK-COBLOCKD)                               
         DC    Y(OPTHRS-ACCAD),Y(COSTD-COBLOCKD)                                
         DC    Y(OPTDGRP-ACCAD),Y(CODIR-COBLOCKD)                               
         DC    Y(OPTNBT-ACCAD),Y(CONBAL-COBLOCKD)                               
         DC    Y(OPTNBA-ACCAD),Y(CONBAB-COBLOCKD)                               
         DC    Y(OPTNBOFF-ACCAD),Y(CONBOFF-COBLOCKD)                            
         DC    Y(OPTNBBAC-ACCAD),Y(CONBOUT-COBLOCKD)                            
         DC    Y(OPTNBACC-ACCAD),Y(CONBIN-COBLOCKD)                             
         DC    Y(OPTPBT-ACCAD),Y(COPBAL-COBLOCKD)                               
         DC    Y(OPTPBA-ACCAD),Y(COPBAB-COBLOCKD)                               
         DC    Y(OPTPBOFF-ACCAD),Y(COPBOFF-COBLOCKD)                            
         DC    Y(OPTPBBAC-ACCAD),Y(COPBOUT-COBLOCKD)                            
         DC    Y(OPTPBACC-ACCAD),Y(COPBIN-COBLOCKD)                             
         DC    Y(OPTHST-ACCAD),Y(COHOAL-COBLOCKD)                               
         DC    Y(OPTHSA-ACCAD),Y(COHOAB-COBLOCKD)                               
         DC    Y(OPTHSOFF-ACCAD),Y(COHOOFF-COBLOCKD)                            
         DC    Y(OPTHSBAC-ACCAD),Y(COHOOUT-COBLOCKD)                            
         DC    Y(OPTHSACC-ACCAD),Y(COHOIN-COBLOCKD)                             
         DC    Y(OPTNO1N-ACCAD),Y(CODONLY-COBLOCKD)                             
         DC    Y(INDACC-ACCAD),Y(COIND-COBLOCKD)                                
         DC    Y(OPTNOTS-ACCAD),Y(COINOH-COBLOCKD)                              
         DC    Y(OPTEXEC-ACCAD),Y(COIEXC-COBLOCKD)                              
         DC    Y(OPTOVA-ACCAD),Y(COVAB-COBLOCKD)                                
         DC    Y(OPTHYB-ACCAD),Y(COCON-COBLOCKD)                                
         DC    Y(OPTINOA-ACCAD),Y(COOCIND-COBLOCKD)                             
         DC    Y(OPTINAA-ACCAD),Y(COACIND-COBLOCKD)                             
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                  TABLE FOR FIXED OPTIONS                      
CFIXOPTT DS    0D                                                               
         DC    AL2(OPTSHOWD-ACCAD),C'N'                                         
         DC    AL2(OPTSHOWO-ACCAD),C'N'                                         
         DC    AL2(OPTDIRLG-ACCAD),C'4'                                         
         DC    AL2(OPT54-ACCAD),C'6'                                            
         DC    AL2(OPTOFH-ACCAD),C'6'                                           
         DC    AL2(OPTC54-ACCAD),C'6'                                           
         DC    AL2(OPTNBBLG-ACCAD),C'5'                                         
         DC    AL2(OPTNBLDG-ACCAD),C'5'                                         
         DC    AL2(OPTPBBLG-ACCAD),C'5'                                         
         DC    AL2(OPTPBLDG-ACCAD),C'5'                                         
         DC    AL2(OPTHSBLG-ACCAD),C'5'                                         
         DC    AL2(OPTHSLDG-ACCAD),C'5'                                         
         DC    AL2(OPTINDD-ACCAD),C'5'                                          
         DC    AL2(OPTINDO-ACCAD),C'5'                                          
         DC    AL2(OPTINDC-ACCAD),C'5'                                          
         DC    AL2(OPTINOLD-ACCAD),C'Y'                                         
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                  TABLE OF SCHEME ANALYSIS= AND STATUS         
CSCHMTBL DS    0D                                                               
         DC    Y(COIALLD-COBLOCKD),Y(SCHAND-SCHMD)                              
         DC    Y(SCHMST_D-SCHMD),Y(SCHMST2D-SCHMD)                              
         DC    AL1(SCMINDI+SCMDPT),AL1(SCHDDEP)                                 
*                                                                               
         DC    Y(COIALLO-COBLOCKD),Y(SCHANO-SCHMD)                              
         DC    Y(SCHMST_O-SCHMD),Y(SCHMST2O-SCHMD)                              
         DC    AL1(SCMINDI+SCMOFC),AL1(SCHDOFF)                                 
*                                                                               
         DC    Y(COIALLC-COBLOCKD),Y(SCHANC-SCHMD)                              
         DC    Y(SCHMST_C-SCHMD),Y(SCHMST2C-SCHMD)                              
         DC    AL1(SCMINDI+SCMCOR),AL1(SCHDCORP)                                
*                                                                               
         DC    Y(COVALLD-COBLOCKD),Y(SCHOHD-SCHMD)                              
         DC    Y(SCHSTO_D-SCHMD),Y(SCHSTO2D-SCHMD)                              
         DC    AL1(SCMOVHE+SCMDPT),AL1(SCHDDEP)                                 
*                                                                               
         DC    Y(COVALLO-COBLOCKD),Y(SCHOHO-SCHMD)                              
         DC    Y(SCHSTO_O-SCHMD),Y(SCHSTO2O-SCHMD)                              
         DC    AL1(SCMOVHE+SCMOFC),AL1(SCHDOFF)                                 
*                                                                               
         DC    Y(COVALLC-COBLOCKD),Y(SCHOHC-SCHMD)                              
         DC    Y(SCHSTO_C-SCHMD),Y(SCHSTO2C-SCHMD)                              
         DC    AL1(SCMOVHE+SCMCOR),AL1(SCHDCORP)                                
*                                                                               
         DC    Y(COISALL-COBLOCKD),Y(SCHINDT-SCHMD)                             
         DC    Y(SCHSTI_C-SCHMD),Y(SCHSTI2C-SCHMD)                              
         DC    AL1(SCMINDS+SCMDPT),AL1(SCHDDEP)                                 
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                  TABLE FOR UPDATING SCHEME BUFFER             
CSCHMCON DS    0D                                                               
*        SETTING OF C'C' ALL CLTS WORKED ON BY COMPANY                          
*        ***************** COMP **********************                          
         DC    AL1(COSET_C),AL1(CORPANAL),AL1(COMP)                             
         DC    AL1(SCHMALL),AL1(0)                                              
*                                                                               
*        SETTING OF C'O' ALL CLTS WORKED ON BY OFFICE                           
*        ***************** OFFICE ********************                          
*        *** NEW NAME **** PEROFF *******************                           
         DC    AL1(COSET_O),AL1(OFFANAL),AL1(OFFICE)                            
         DC    AL1(SCHMALL),AL1(0)                                              
*                                                                               
*        SETTING OF C'D' ALL CLTS WORKED ON BY DEPT                             
*        ***************** DEPT  *********************                          
         DC    AL1(COSET_D),AL1(DPTANAL),AL1(DEPT)                              
         DC    AL1(SCHMALL),AL1(0)                                              
*                                                                               
*        SETTING OF C'L' ALL CLTS WORKED ON BY OFFICE LIST                      
*        ***************** OLIST *********************                          
*        *** NEW NAME **** PEROLIST *****************                           
         DC    AL1(COSET_L),AL1(GRPANAL),AL1(OGROUP)                            
         DC    AL1(SCHMALL+SCHMRECK+SCHMOFLT),AL1(SCHMINCL)                     
*                                                                               
*        C'M' ALL CLTS WORKED ON BY OFFICE THAT BELONG TO OFFICE                
*        ***************** COFFICE *******************                          
*        *** NEW NAME **** PCOFF ********************                           
         DC    AL1(COSET_M),AL1(OFFANAL),AL1(OFFICE)                            
         DC    AL1(SCHMINCL+SCHMRECK),AL1(SCHMINCL)                             
*                                                                               
*        C'N' ALL CLTS WORKED ON BY OFFLST THAT BELONG TO OFFLST                
*        ***************** COLIST  *******************                          
*        *** NEW NAME **** PCOLIST ******************                           
         DC    AL1(COSET_N),AL1(GRPANAL),AL1(OGROUP)                            
         DC    AL1(SCHMINCL+SCHMRECK+SCHMOFLT),AL1(SCHMINCL)                    
*                                                                               
*        C'K' ALL CLTS THAT BELONG TO THAT OFFICE LIST                          
*        ***************** CLIST *********************                          
*        *** NEW NAME **** CLIOLIST *****************                           
         DC    AL1(COSET_K),AL1(GRPANAL),AL1(OGROUP)                            
         DC    AL1(SCHMINCL+SCHMOFLT),AL1(SCHMINCL)                             
*                                                                               
*        C'Q' ALL CLTS THAT BELONG TO THAT OFFICE                               
*        ***************** CLIOFF ********************                          
         DC    AL1(COSET_Q),AL1(GRPANAL),AL1(OGROUP)                            
         DC    AL1(SCHMINCL),AL1(SCHMINCL)                                      
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                  TABLE FOR AN= CONVERSIONS (ACNVD)            
CANALCV  DS    0D                                                               
         DC    C'O',AL1(SCMOFC),AL2(SCHANO-SCHMD)                               
         DC    CL13'NOFFICENOHRS ',CL22'OFFICE INDIRECT NO HRS'                 
         DC    C'C',AL1(SCMCOR),AL2(SCHANC-SCHMD)                               
         DC    CL13'NCORPNOHOURS ',CL22'CORP INDIRECT NO HRS  '                 
         DC    C'D',AL1(SCMDPT),AL2(SCHAND-SCHMD)                               
         DC    CL13'NDEPTNOHOURS ',CL22'DEPT INDIRECT NO HRS  '                 
         DC    C' ',AL1(SCMDPT),AL2(SCHAND-SCHMD)                               
         DC    CL13'NDEPTNOHOURS ',CL22'DEPT INDIRECT NO HRS  '                 
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                  TABLE FOR OVERHEAD DEFINITION(OHDFD)         
COHDEFN  DS    0D                                                               
         DC    AL1(DEPT),AL2(SCHOHD-SCHMD),C'D',AL1(SCMDPT)                     
         DC    AL1(OFFICE),AL2(SCHOHO-SCHMD),C'O',AL1(SCMOFC)                   
         DC    AL1(COMP),AL2(SCHOHC-SCHMD),C'C',AL1(SCMCOR)                     
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                  TABLE FOR POOL HEADLINE DESCRIPTIONS         
CPOLDEFN DS    0D                                                               
*                                  'NEW BIZ '                                   
         DC    AL1(INDITYPE),CL1'*',AL1(INDNEWBI)                               
         DCDD  AC#NBIZ,14                                                       
*                                  'PRO BONO'                                   
         DC    AL1(INDITYPE),CL1'*',AL1(INDPROBO)                               
         DCDD  AC#PBONO,14                                                      
*                                  'HOUSE   '                                   
         DC    AL1(INDITYPE),CL1'*',AL1(INDHOUSE)                               
         DCDD  AC#HOUSE,14                                                      
*                                  'DEPT IND'                                   
         DC    AL1(INDITYPE),AL1(INDDEPT),AL1(0)                                
         DCDD  AC#DIND,14                                                       
*                                  'OFFC IND'                                   
         DC    AL1(INDITYPE),AL1(INDOFFIC),AL1(0)                               
         DCDD  AC#OIND,14                                                       
*                                  'CORP IND'                                   
         DC    AL1(INDITYPE),AL1(INDCORP),AL1(0)                                
         DCDD  AC#CIND,14                                                       
*                                  'GRUP IND'                                   
         DC    AL1(INDITYPE),AL1(INDGRUP),AL1(0)                                
         DCDD  AC#GIND,14                                                       
*                                  'ACCOUNT OVH'                                
         DC    AL1(OHTYPE),AL1(OVHACCT),AL1(0)                                  
         DCDD  AC#AOVH,14                                                       
*                                  'UNABSORBED OVH'                             
         DC    AL1(OHTYPE),AL1(OVHUNABS),AL1(0)                                 
         DCDD  AC#UOVH,14                                                       
*                                  '*IND* PAY TYPE'                             
         DC    AL1(OHTYPE),AL1(OVHINDP),AL1(0)                                  
         DCDD  AC#IPAY,14                                                       
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
CERRDEF  DS    0D                  TABLE FOR ERROR NUMBER DEFINITIONS           
*                                  INVALID REQUEST DATES                        
         DC    AL1(ERRDATES),AL1(21),AL1(ERRWARN),AL1(ERDHEDUP)                 
         DCDD  AC#ERQDT,36                                                      
*                                  INVALID METHOD RECORD                        
         DC    AL1(ERRMETH),AL1(7),AL1(ERRERROR),AL1(0)                         
         DCDD  AC#ERMT,36                                                       
*                                  INVALID PAY TYPES FOR THIS METHOD            
         DC    AL1(ERRPTYP),AL1(7),AL1(ERRERROR),AL1(0)                         
         DCDD  AC#ERPT,36                                                       
*                                  INVALID STD HOURS RECORD                     
         DC    AL1(ERRSTDHR),AL1(7),AL1(ERRERROR),AL1(0)                        
         DCDD  AC#ERSTD,36                                                      
*                                  INVALID OR MISSING 1C ACCOUNT                
         DC    AL1(ERRCONTR),AL1(7),AL1(ERRERROR),AL1(0)                        
         DCDD  AC#IVLA,36                                                       
*                                  TOTAL DEBITS DO NOT EQUAL CREDITS            
         DC    AL1(ERRDRCR),AL1(7),AL1(ERRERROR),AL1(0)                         
         DCDD  AC#DNOTC,36                                                      
*                                  UNABSORBED OH/IND POOL AT CORP LEVEL         
         DC    AL1(ERRUNABS),AL1(7),AL1(ERRERROR),AL1(0)                        
         DCDD  AC#UCORP,36                                                      
*                                  NO DIRECT TIME FOR SCHEME FOR POOL           
         DC    AL1(ERRNODIR),AL1(7),AL1(ERRERROR),AL1(0)                        
         DCDD  AC#NODIP,36                                                      
*                                  IND PAY TYPE 1R OVH ACCOUNT MISSING          
         DC    AL1(ERR1RMIS),AL1(7),AL1(ERRERROR),AL1(0)                        
         DCDD  AC#INDP,36                                                       
*                                  EMPLOYEE HAS SALARY BUT NO HOURS             
         DC    AL1(ERRNOHRS),AL1(10),AL1(ERRWARN),AL1(ERDHEDUP)                 
         DCDD  AC#ESNH,36                                                       
*                                  EMPLOYEE HAS NEGATIVE HOURS ONLY             
         DC    AL1(ERRNEGHR),AL1(11),AL1(ERRWARN),AL1(ERDHEDUP)                 
         DCDD  AC#NEGH,36                                                       
*                                  EMPLOYEE HAS PERSONAL HOURS ONLY             
         DC    AL1(ERRPERHR),AL1(12),AL1(ERRWARN),AL1(ERDHEDUP)                 
         DCDD  AC#PERH,36                                                       
*                                  EMPLOYEE HAS HOURS BUT NO SALARY             
         DC    AL1(ERRNOSAL),AL1(13),AL1(ERRWARN),AL1(ERDHEDUP)                 
         DCDD  AC#NOSAL,36                                                      
*                                  EMPLOYEE HRS NET TO ZERO                     
         DC    AL1(ERRHRNET),AL1(16),AL1(ERRWARN),AL1(ERDHEDUP)                 
         DCDD  AC#HRNET,36                                                      
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                                                               
AMAINTAB DS    0F                                                               
         DC    CL8'**CLID** '                                                   
         DC    AL2(CLIBUFF-ACCAD)  STORED ADDR OF THE TABLE (CLID)              
         DC    AL2(CLEN)           RECORD LENGTH                                
         DC    AL2(CLIKLEN)        DISP IN REC/KEY LENGTH                       
         DC    AL2(CLIMAX)         MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(CBKCNT)         NUMBER OF BUCKETS                            
         DC    AL1(CLIBK-CLID)     DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(CLISIZE)        SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**CLID2* '                                                   
         DC    AL2(CLIBUF2-ACCAD)  STORED ADDR OF THE TABLE (CLID)              
         DC    AL2(CLEN2)          RECORD LENGTH                                
         DC    AL2(CLIKLEN)        DISP IN REC/KEY LENGTH                       
         DC    AL2(CLIMAX2)        MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(CBKCNT2)        NUMBER OF BUCKETS                            
         DC    AL1(CLIBK-CLID)     DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(CLISIZ2)        SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**SUMD** '                                                   
         DC    AL2(SUMBUFF-ACCAD)  STORED ADDR OF THE TABLE (SUMD)              
         DC    AL2(SLEN)           RECORD LENGTH                                
         DC    AL2(SKLEN)          DISP IN REC/KEY LENGTH                       
         DC    AL2(SUMMAX)         MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(SBKCNT)         NUMBER OF BUCKETS                            
         DC    AL1(SBK-SUMD)       DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(SUMSIZE)        SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**INDID**'                                                   
         DC    AL2(INDBUFF-ACCAD)  STORED ADDR OF THE TABLE (INDID)             
         DC    AL2(INLEN)          RECORD LENGTH                                
         DC    AL2(INKLEN)         DISP IN REC/KEY LENGTH                       
         DC    AL2(INMAX)          MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(INBUKCNT)       NUMBER OF BUCKETS                            
         DC    AL1(INBK-INDID)     DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(INSIZE)         SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**OVERD**'                                                   
         DC    AL2(OVHBUFF-ACCAD)  STORED ADDR OF THE TABLE (OVERD)             
         DC    AL2(OVLEN)          RECORD LENGTH                                
         DC    AL2(OVKLEN)         DISP IN REC/KEY LENGTH                       
         DC    AL2(OVMAX)          MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(OVBUKCNT)       NUMBER OF BUCKETS                            
         DC    AL1(OVBK-OVERD)     DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(OVSIZE)         SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**RATD** '                                                   
         DC    AL2(RATBUFF-ACCAD)  STORED ADDR OF THE TABLE (RATD)              
         DC    AL2(RATLEN)         RECORD LENGTH                                
         DC    AL2(RTKLEN)         DISP IN REC/KEY LENGTH                       
         DC    AL2(RTMAX)          MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(RBKCNT)         NUMBER OF BUCKETS                            
         DC    AL1(RBUKS-RATD)     DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(RTSIZE)         SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**BCLID**'                                                   
         DC    AL2(NMEBUFF-ACCAD)  STORED ADDR OF THE TABLE (BCLID)             
         DC    AL2(BCLEN)          RECORD LENGTH                                
         DC    AL2(BCKLEN)         DISP IN REC/KEY LENGTH                       
         DC    AL2(BCMAX)          MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(BCSIZE)         SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**HYBND**'                                                   
         DC    AL2(HYNMBUF-ACCAD)  STORED ADDR OF THE TABLE (HYBND)             
         DC    AL2(HYBNLEN)        RECORD LENGTH                                
         DC    AL2(HYKLEN)         DISP IN REC/KEY LENGTH                       
         DC    AL2(HYMAX)          MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(HYSIZE)         SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**NEWBD**'                                                   
         DC    AL2(NEWBIZBF-ACCAD) STORED ADDR OF THE TABLE (NEWBD)             
         DC    AL2(NEWBLEN)        RECORD LENGTH                                
         DC    AL2(NEWLEN)         DISP IN REC/KEY LENGTH                       
         DC    AL2(NEWBMAX)        MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(NEWBSIZE)       SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**CLTLD**'                                                   
         DC    AL2(CLTLBUFF-ACCAD) STORED ADDR OF THE TABLE (NEWBD)             
         DC    AL2(CLTLLEN)        RECORD LENGTH                                
         DC    AL2(CLTLKLEN)       DISP IN REC/KEY LENGTH                       
         DC    AL2(CLILMAX)        MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(CLILSIZE)       SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**NOND** '                                                   
         DC    AL2(NONCLTBF-ACCAD) STORED ADDR OF THE TABLE (NOND)              
         DC    AL2(NONLEN)         RECORD LENGTH                                
         DC    AL2(NONKLEN)        DISP IN REC/KEY LENGTH                       
         DC    AL2(NONMAX)         MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(NONSIZE)        SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**ERRORD*'                                                   
         DC    AL2(ERRORBUF-ACCAD) STORED ADDR OF THE TABLE (ERRORD)            
         DC    AL2(ERRLEN)         RECORD LENGTH                                
         DC    AL2(ERRLEN)         DISP IN REC/KEY LENGTH                       
         DC    AL2(ERRMAX)         MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE                            
         DC    AL4(ERRSIZE)        SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**YTD2D*'                                                    
         DC    AL2(YTD2BUF-ACCAD)  STORED ADDR OF THE TABLE (YTD1D)             
         DC    AL2(YTDLEN)         RECORD LENGTH                                
         DC    AL2(YTDKLEN)        DISP IN REC/KEY LENGTH                       
         DC    AL2(YTD2MAX)        MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(YTDBKCNT)       NUMBER OF BUCKETS                            
         DC    AL1(YTDBK-YTD1D)    DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTE(REMOVED BINSIXB)           
         DC    AL4(YTD2SIZE)       SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**SCHEM*'                                                    
         DC    AL2(SCHEMBUF-ACCAD) STORED ADDR OF THE TABLE (SCHMD)             
         DC    AL2(SCHMLEN)        RECORD LENGTH                                
         DC    AL2(SCHKLEN)        DISP IN REC/KEY LENGTH                       
         DC    AL2(SCHMMAX)        MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL1(0)              TABLE STATUS BYTES                           
         DC    AL4(SCHMSIZE)       SIZE OF THE TABLE                            
*                                                                               
*        DC    CL8'*PALBUFF*'                                                   
*        DC    AL2(PALBUFF-ACCAD)  STORED ADDR OF THE TABLE (SCHMD)             
*        DC    AL2(PLLNQ)          RECORD LENGTH                                
*        DC    AL2(PLKLNQ)         DISP IN REC/KEY LENGTH                       
*        DC    AL2(PLMAX)          MAX NUMBER OF TAB ENTRIES                    
*        DC    AL1(1)              NUMBER OF BUCKETS                            
*        DC    AL1(PLAMNT-PLREC)   DISP TO BUCKETS                              
*        DC    AL1(0)              TABLE STATUS BYTES                           
*        DC    AL4(PLSIZE)         SIZE OF THE TABLE                            
*                                                                               
*        DC    CL8'*YTDPLTB*'                                                   
*        DC    AL2(YTDPLTB-ACCAD)    STORED ADDR OF THE TABLE (SCHMD)           
*        DC    AL2(YTDPLNQ)          RECORD LENGTH                              
*        DC    AL2(YTDKLNQ)          DISP IN REC/KEY LENGTH                     
*        DC    AL2(YTDPMAX)          MAX NUMBER OF TAB ENTRIES                  
*        DC    AL1(1)                NUMBER OF BUCKETS                          
*        DC    AL1(YTDPAMNT-YTDPREC) DISP TO BUCKETS                            
*        DC    AL1(0)                TABLE STATUS BYTES                         
*        DC    AL4(YTDPSIZE)         SIZE OF THE TABLE                          
*                                                                               
MAINNUM  EQU   (*-AMAINTAB)/MAINLNQ NUMBER OF BINARY TABLES                     
*                                                                               
*                                     **ACQUIRED WORK AREAS    **               
*                                     **NOTE: UPDATE LENSIZE!!!**               
AWORKTAB DS    0F                                                               
         DC    AL2(ABUFWRK-ACCAD),AL2(BLEN)                                     
         DC    AL2(APALWRK-ACCAD),AL2(PLLNQ)                                    
         DC    AL2(AYTDPWRK-ACCAD),AL2(YTDPLNQ)                                 
         DC    AL2(ACLIWRK-ACCAD),AL2(CLEN2)                                    
         DC    AL2(AINDWRK-ACCAD),AL2(INLEN)                                    
         DC    AL2(AOVHWRK-ACCAD),AL2(OVLEN)                                    
         DC    AL2(AOVHWRK2-ACCAD),AL2(OVLEN)                                   
         DC    AL2(APERWRK-ACCAD),AL2(PTOTLEN)                                  
         DC    AL2(ARATWRK-ACCAD),AL2(RATLEN)                                   
         DC    AL2(ASRTWRK-ACCAD),AL2(SRTLEN)                                   
         DC    AL2(ALSTWRK-ACCAD),AL2(SRTLEN)                                   
         DC    AL2(AOVHAREA-ACCAD),AL2(SLRLEN)                                  
         DC    AL2(AYTDWRK-ACCAD),AL2(YTDLEN)                                   
         DC    AL2(ASCHEMWK-ACCAD),AL2(SCHMLEN)                                 
         DC    AL2(AOFFLTAB-ACCAD),AL2(OFFTLEN)                                 
         DC    AL2(ALISTAB-ACCAD),AL2(LSTLEN)                                   
         DC    AL2(AMTHRTAB-ACCAD),AL2(MTHRLEN)                                 
         DC    AL2(ADEPT99-ACCAD),AL2(SLEN)                                     
         DC    AL2(ADEPTOT-ACCAD),AL2(SLEN)                                     
         DC    AL2(AOFF99-ACCAD),AL2(SLEN)                                      
         DC    AL2(AOFFTOT-ACCAD),AL2(SLEN)                                     
         DC    AL2(AAGY99-ACCAD),AL2(SLEN)                                      
         DC    AL2(AAGYTOT-ACCAD),AL2(SLEN)                                     
         DC    AL2(ADEPCUM-ACCAD),AL2(DEPLEN)                                   
         DC    AL2(AOFFCUM-ACCAD),AL2(OFFLEN)                                   
         DC    AL2(AAGYCUM-ACCAD),AL2(AGYLEN)                                   
MWRKNUM  EQU   (*-AWORKTAB)/WORKLNQ NUMBER OF WORK AREAS                        
*                                                                               
*                                                                               
*                                      RECORDS ADDED COUNTER TABLE              
*                                                                               
ACONTTAB DS    0F                                                               
         DC    H'100',PL4'0'                                                    
         DC    H'150',PL4'0'                                                    
         DC    H'200',PL4'0'                                                    
         DC    H'250',PL4'0'                                                    
         DC    H'300',PL4'0'                                                    
         DC    H'350',PL4'0'                                                    
         DC    H'400',PL4'0'                                                    
         DC    H'450',PL4'0'                                                    
         DC    H'500',PL4'0'                                                    
         DC    H'550',PL4'0'                                                    
         DC    H'600',PL4'0'                                                    
         DC    H'650',PL4'0'                                                    
         DC    H'700',PL4'0'                                                    
         DC    H'750',PL4'0'                                                    
         DC    H'1000',PL4'0'                                                   
ACNTNUM  EQU   (*-ACONTTAB)/CONTLNQ NUMBER OF RECORD COUNTERS                   
*                                                                               
*                                     BINARY TABLE EQUATES                      
*                                     **NOTE: UPDATE LENSIZE!!!**               
*                                                                               
CLIMAX   EQU   3700                                                             
CLISIZE  EQU   L'MAINEYE+BINLENQ+(CLIMAX*CLEN)                                  
CLIMAX2  EQU   400                                                              
CLISIZ2  EQU   L'MAINEYE+BINLENQ+(CLIMAX2*CLEN2)                                
SUMMAX   EQU   1100                                                             
SUMSIZE  EQU   L'MAINEYE+BINLENQ+(SUMMAX*SLEN)                                  
INMAX    EQU   1500                                                             
INSIZE   EQU   L'MAINEYE+BINLENQ+(INMAX*INLEN)                                  
OVMAX    EQU   300                                                              
OVSIZE   EQU   L'MAINEYE+BINLENQ+(OVMAX*OVLEN)                                  
RTMAX    EQU   5500                                                             
RTSIZE   EQU   L'MAINEYE+BINLENQ+(RTMAX*RATLEN)                                 
BCMAX    EQU   15000                                                            
BCSIZE   EQU   L'MAINEYE+BINLENQ+(BCMAX*BCLEN)                                  
HYMAX    EQU   1100                                                             
HYSIZE   EQU   L'MAINEYE+BINLENQ+(HYMAX*HYBNLEN)                                
*&&US                                                                           
NEWBMAX  EQU   600                                                              
*&&                                                                             
*&&UK                                                                           
NEWBMAX  EQU   3500                                                             
*&&                                                                             
NEWBSIZE EQU   L'MAINEYE+BINLENQ+(NEWBMAX*NEWBLEN)                              
CLILMAX  EQU   600                                                              
CLILSIZE EQU   L'MAINEYE+BINLENQ+(CLILMAX*CLTLLEN)                              
NONMAX   EQU   500                                                              
NONSIZE  EQU   L'MAINEYE+BINLENQ+(NONMAX*NONLEN)                                
ERRMAX   EQU   3000                                                             
ERRSIZE  EQU   L'MAINEYE+BINLENQ+(ERRMAX*ERRLEN)                                
YTD2MAX  EQU   22000                                                            
YTD2SIZE EQU   L'MAINEYE+BINLENQ+(YTD2MAX*YTDLEN)                               
SCHMMAX  EQU   5000                                                             
SCHMSIZE EQU   L'MAINEYE+BINLENQ+(SCHMMAX*SCHMLEN)                              
OFFTLEN  EQU   20000                                                            
MTHMAX   EQU   12                                                               
MTHRLEN  EQU   MTHLN*MTHMAX                                                     
LISTMAX  EQU   25                                                               
LSTLEN   EQU   LISTLEN*LISTMAX                                                  
MAXOLIST EQU   32                                                               
MXPERHED EQU   10                                                               
MAXERRPG EQU   24                  MAX ERRORS ON A PAGE                         
*                                                                               
LENBUFF2 EQU   PLSIZE+YTDPSIZE+100                                              
*                                                                               
*                                                                               
*              ***** ABOVE THE LINE STORAGE T S A R  *****                      
*                                                                               
PLMAX    EQU   500000                                                           
PLSIZE   EQU   BINLENQ+(PLMAX*PLLNQ)                                            
YTDPMAX  EQU   500000                                                           
YTDPSIZE EQU   BINLENQ+(PLMAX*PLLNQ)                                            
YTD1MAX  EQU   600000                                                           
YTD1SIZE EQU   YTD1MAX*YTDLEN                                                   
         EJECT                                                                  
*                                                                               
*                                           BUFFALO EQUATES                     
*                                                                               
BUFROW   EQU   3                            NUMBER OF BUFFALO ROWS              
BDATLN   EQU   BUFROW*BBKCNT*BUFBKLN        BUFFALO DATA LENGTH                 
BRECLN   EQU   BDATLN+BUFKLEN               BUFFALO RECORD LENGTH               
BUFMAX   EQU   500                          MAX NUMBER IN CORE                  
BUFCORE  EQU   BUFMAX*BRECLN                BUFFALO CORE                        
BUFSPACE EQU   L'BUFFCNTL+BRECLN+BUFCORE    TOTAL BUFFALO CORE TAKEN            
ASCEND   EQU   C'A'                         KEY IN ASENDING ORDER               
FILINDC  EQU   C'A'                         BUFFALO FILE INDICATOR              
PACKED   EQU   C'P'                         BUFF ACCUMS PACKED FORMAT           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         USING ACCAD,RC            RC=A(LOCAL W/S)                              
DUMP     NMOD1 0,**DMP**                                                        
         LA    RF,*+6                                                           
         BSM   0,RF                24 BIT MODE                                  
*                                                                               
         L     RC,0(R1)                                                         
         CLI   QOPT8,C'Y'                                                       
         BNE   DUMPX                                                            
*                                                                               
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    DUMPX                                                            
*                                                                               
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
         DROP                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GENERAL STORAGE DSECT                                              *          
**********************************************************************          
         SPACE 1                                                                
ACCAD    DSECT                                                                  
ATYPES   DS    0A                                                               
ACSLRY   DS    V                                                                
ACSALHST DS    V                                                                
GETCAP   DS    V                                                                
COVAIL   DS    V                                                                
DATVAL   DS    V                                                                
HELLO    DS    V                                                                
PRNTBL   DS    V                                                                
ACTRAVL  DS    V                                                                
BUFFERIN DS    V                                                                
BINSRC31 DS    V                   BINSRCH (31 BIT MODE)                        
         DS    5F                  SPARE                                        
*                                                                               
ADEPTAB  DS    A                                                                
ADEPOTAB DS    A                                                                
AOFFTAB  DS    A                                                                
AOFFOTAB DS    A                                                                
ACORTAB  DS    A                                                                
ACOROTAB DS    A                                                                
COMMON   DS    A                   COMMON ROUTINES NMOD # 1                     
COMMON2  DS    A                   COMMON ROUTINES NMOD # 2                     
COMMON3  DS    A                   COMMON ROUTINES NMOD # 3                     
COMMON4  DS    A                   COMMON ROUTINES NMOD # 4                     
RECORD   DS    A                                                                
RECORD2  DS    A                                                                
DIO      DS    A                                                                
PLCSAV   DS    A                                                                
RCVIO    DS    A                                                                
ACOBLOCK DS    A                                                                
ASALAREA DS    A                                                                
AUPDBLOK DS    A                                                                
TSARBLK1 DS    A                                                                
DDIN     DS    A                                                                
COMTAB   DS    A                                                                
DICTTAB  DS    A                                                                
CLRTAB   DS    A                                                                
COMPRAT  DS    A                                                                
SCHMDEF  DS    A                                                                
SCHDFLT  DS    A                                                                
SUMPST   DS    A                                                                
STDOPTS  DS    A                                                                
FIXOPTS  DS    A                                                                
SCHMTBL  DS    A                                                                
SCHMCON  DS    A                                                                
ANALCV   DS    A                                                                
OHDEFN   DS    A                                                                
POLDEFN  DS    A                                                                
ERRDEF   DS    A                                                                
MAINTAB  DS    A                                                                
WORKTAB  DS    A                                                                
CONTTAB  DS    A                                                                
ADUMP    DS    A                   DUMP ROUTINE                                 
         DS    2F                  SPARE                                        
         DS    CL1                 END OF TABLE                                 
*                                                                               
ADDTRN   DS    A                   ADDRESS OF ADDTRNS                           
*                                                                               
*                                  BINARY TABLES(GETMAIN)                       
ABUFF    DS    A                                                                
ATBUFF   DS    A                                                                
CLIBUFF  DS    A                                                                
CLIBUF2  DS    A                                                                
SUMBUFF  DS    A                                                                
INDBUFF  DS    A                                                                
OVHBUFF  DS    A                                                                
RATBUFF  DS    A                                                                
NMEBUFF  DS    A                                                                
HYNMBUF  DS    A                                                                
NEWBIZBF DS    A                                                                
CLTLBUFF DS    A                                                                
NONCLTBF DS    A                                                                
ERRORBUF DS    A                                                                
YTD1BUF  DS    A                                                                
YTD2BUF  DS    A                                                                
SCHEMBUF DS    A                                                                
*                                                                               
ABUFWRK  DS    A                   ADDRESS OF BUFFALO REC WORK AREA             
APALWRK  DS    A                   P AND L SPECIAL RECORDS (PLDREC)             
AYTDPWRK DS    A                   YTD P AND L RECORD WORK AREA                 
ACLIWRK  DS    A                               CLIENT                           
AINDWRK  DS    A                               INDIRECT                         
AOVHWRK  DS    A                               OVERHEAD                         
AOVHWRK2 DS    A                               OVERHEAD(IND PAY TYPE)           
APERWRK  DS    A                               EMPLOYEE                         
ARATWRK  DS    A                               EMPLOYEE HOURLY RATE REC         
ASRTWRK  DS    A                               SORTER                           
ALSTWRK  DS    A                               LAST SORT                        
AOVHAREA DS    A                               OVERHEAD WK FO ACSALARY          
AYTDWRK  DS    A                               YTD-1 WORK AREA                  
ASCHEMWK DS    A                               ALLOC SCHEME WORK AREA           
AOFFLTAB DS    A                               OFFICE/GROUP TABLE               
ALISTAB  DS    A                               LIST TABLE                       
AMTHRTAB DS    A                               MONTHS OF REQUEST TABLE          
ADEPT99  DS    A                               TOTAL ACCUMS                     
ADEPTOT  DS    A                                                                
AOFF99   DS    A                                                                
AOFFTOT  DS    A                                                                
AAGY99   DS    A                                                                
AAGYTOT  DS    A                                                                
ADEPCUM  DS    A                                                                
AOFFCUM  DS    A                                                                
AAGYCUM  DS    A                                                                
*                                                                               
ADBUFC   DS    A                   ADDR OF BUFFALOC AREA                        
*                                                                               
SAVERE   DS    A                   SAVE ADDR OF RE                              
AACCUM   DS    A                   SAVED ADDR OF CURRENT ACCUM                  
ACLREC   DS    A                   SAVED ADDR OF CURRENT CLIBUFF RECORD         
ASUMREC  DS    A                   SAVED ADDR OF CURRENT SUMMARY RECORD         
APOLREC  DS    A                   SAVED ADDR OF CURRENT POOL ENTRY             
ASCHMREC DS    A                   SAVED ADDR OF CURRENT SCHEME ENTRY           
ALSORT   DS    A                   SAVE ADDR OF LAST SORT  RECORD               
ALTAB    DS    A                   SAVE ADDR OF GENERAL TABLE ENTRY             
ARECORD  DS    A                   ADDR OF RECORD WE'RE WORKING WITH            
ATSAROFF DS    A                   ADDR OF OFFINE TSAR (TSAROFF)                
SVADDR   DS    A                   SAVED ADDRESS FIELD                          
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
PALBUFF  DS    A                   P AND L SPEC RECS (PLDREC) BUFFER            
YTDPLTB  DS    A                   YTD P AND L RECS TABLE                       
*                                                                               
COM1     DS    0A                  *** COMMON NMOD # 1 ***                      
INDPOST  DS    V                   ALLOCATE INDIRECT COST                       
DISOVH   DS    V                   DISTRIBUTE OVERHEAD                          
MINUS1   DS    V                   GET REAL YTD-1 ON THE 1C SIDE                
PROFIT   DS    V                   GET PROFILES AT EACH LEVEL                   
ASCHEME  DS    V                   SCHEME LOOKUP/BILD TAB OF CLIENTS            
ABUFPOST DS    V                   PUT DIR+DPT INDIRECT TO BUFFALO              
AEMPLOY  DS    V                   MAKE EMPLOYEE POSTINGS TO SORTER             
AMRKYTD  DS    V                   ROUTINE TO MARK YTDPLTB                      
RESERVE  DS    4V                                                               
COMSPARE EQU (*-RESERVE)/L'COM1    NUMBER OF SPARE ADDRESSES                    
COMNUM1  EQU   (*-COM1)/L'COM1     NUMBER OF ROUTINES                           
*                                                                               
*                                                                               
COM2     DS    0A                  *** COMMON NMOD # 2 ***                      
REPORT   DS    V                   HANDLE REPORT PRINTING                       
HEADUP   DS    V                   HEADLINE ROUTINES                            
BLDTAB   DS    V                   BUILD TABS 1N,14-17 AND LEDG HEIR            
TABBLD   DS    V                   BUILD TAB OF 1C YTD-1 NUMBERS                
BINSRC   DS    V                   GET RECORD FROM BINSRCH TABLE                
BINADD   DS    V                   ADD ITEM TO BINSRCH TABLE (24 MODE)          
BIN31    DS    V                   ADD ITEM TO BINSRCH TABLE (31 MODE)          
CLERWRK  DS    V                   CLEAR BINARY TABLE ENTRY                     
DODIVD   DS    V                   DO DIVISION TO GET CLIENT SHARE              
CLERSRT  DS    V                   CLEAR AND INIT SORTER WORK ENTRY             
ERRREPT  DS    V                   PRINT ERROR REPORT                           
RECREPT  DS    V                   PRINT RECORDS ADDED                          
BXHOOK   DS    V                   BOX HOOK ROUTINE                             
RESERVE2 DS    4V                                                               
COMSPAR2 EQU (*-RESERVE2)/L'COM2   NUMBER OF SPARE ADDRESSES                    
COMNUM2  EQU   (*-COM2)/L'COM2     NUMBER OF ROUTINES                           
*                                                                               
COM3     DS    0A                  *** COMMON NMOD # 3 ***                      
POSTIT   DS    V                   PUT WORKER RECORDS TO ACPOST                 
RECOVER  DS    V                   HANDLE RECOVERY (ADDTRN/MY OWN)              
YTDADJ   DS    V                   PUT YTD-1 ADJUSTMENTS TO SORT                
BUKREPT  DS    V                   PRINT SORTER BUCKET REPORT                   
MAIN     DS    V                   ACQUIRE STORAGE FOR BINARY TABLES            
WRAP     DS    V                   RELEASE ACQUIRED STORAGE                     
BOFFL    DS    V                   BUILD THE OFFICE/GROUP LIST                  
GOFFL    DS    V                   GET THE OFFICE/GROUP ENTRY                   
BCLIL    DS    V                   CLIENT LIST FOR THIS METHOD                  
RESERVE3 DS    5V                                                               
COMSPAR3 EQU (*-RESERVE3)/L'COM3   NUMBER OF SPARE ADDRESSES                    
COMNUM3  EQU   (*-COM3)/L'COM3     NUMBER OF ROUTINES                           
*                                                                               
*                                                                               
COM4     DS    0A                  *** COMMON NMOD # 4 ***                      
SUMPOST  DS    V                   POST SUMMARY RECORDS                         
CONCK    DS    V                   CHECK FOR CONTRAS                            
RESERVE4 DS    9V                                                               
COMSPAR4 EQU (*-RESERVE4)/L'COM4   NUMBER OF SPARE ADDRESSES                    
COMNUM4  EQU   (*-COM4)/L'COM4     NUMBER OF ROUTINES                           
*                                                                               
*                                                                               
LITVALS  DS    0X                  LITERAL VALUES                               
ZEROS    DS    PL1                 P'0'                                         
MAXDUMP  DS    PL4                 P'100'                                       
NONCLI   DS    CL2                 C'1N'                                        
CLILEG   DS    CL2                 C'1C'                                        
EMPLEDG  DS    CL2                 C'1R'                                        
DIR      DS    CL6                 C'ACCDIR'                                    
MST      DS    CL6                 C'ACCMST'                                    
GETREC   DS    CL8                 C'GETREC  '                                  
ADDREC   DS    CL8                 C'ADDREC  '                                  
PUTREC   DS    CL8                 C'PUTREC  '                                  
FOXES    DS    XL12                12X'FF'                                      
NINES    DS    CL12                C'99999999999'                               
SETC     DS    CL5                 C'SET  '                                     
PUTC     DS    CL5                 C'PUT  '                                     
ADDC     DS    CL5                 C'ADD  '                                     
GETC     DS    CL5                 C'GET  '                                     
SEQC     DS    CL5                 C'SEQ  '                                     
ENDC     DS    CL5                 C'END  '                                     
HIGHC    DS    CL5                 C'HIGH '                                     
CLEARC   DS    CL5                 C'CLEAR'                                     
LITVALSL EQU   *-LITVALS                                                        
*                                                                               
LLEVELS  EQU   *                                                                
LLEVA    DS    CL(L'ACLVLEN)         1R LEV A LENGTH                            
LLEVANAM DS    CL(L'ACLVDESC)        1R LEV A NAME                              
LLEVALN  EQU   *-LLEVELS                                                        
LLEVB    DS    CL(L'ACLVLEN)         1R LEV B LENGTH (A+B)                      
LLEVBNAM DS    CL(L'ACLVDESC)        1R LEV B NAME                              
LLEVC    DS    CL(L'ACLVLEN)         1R LEV C LENGTH (A+B+C)                    
LLEVCNAM DS    CL(L'ACLVDESC)        1R LEV C NAME                              
LLEVD    DS    CL(L'ACLVLEN)         1R LEV D LENGTH (A+B+C+D)                  
LLEVDNAM DS    CL(L'ACLVDESC)        1R LEV D NAME                              
LLEVELLN EQU   *-LLEVELS                                                        
LLEVLNUM EQU   LLEVELLN/LLEVALN                                                 
*                                                                               
LENLEVLS EQU   *                                                                
LENLEVA  DS    XL1        REAL LENGTH OF LEVEL A                                
LENLEVB  DS    XL1        REAL LENGTH OF LEVEL B                                
LENLEVC  DS    XL1        REAL LENGTH OF LEVEL C                                
LENLEVD  DS    XL1        REAL LENGTH OF LEVEL D                                
LENLEVLN EQU   *-LENLEVLS                                                       
LENLVNUM EQU   LENLEVLN/L'LENLEVA                                               
OFFDISP  DS    XL1        DISPLACEMENT OF OFFICE CODE INTO 1C ACCOUNT           
OFFCLEN  DS    XL1        LENGTH OF 1C OFFICE CODE                              
DEPDISP  DS    XL1        DISPLACEMENT OF DPT INTO 1C ACCOUNT IF ANY            
DPTLEN   DS    XL1        LENGTH OF 1C DEPT CODE IF ANY                         
LASTLEVD DS    XL1        DISPLACEMENT OF LAST LEVEL INTO 1C ACCOUNT            
*                                                                               
CLEVELS  EQU   *                                                                
CLEVA    DS    CL(L'ACLVLEN)        1C LEV A LENGTH                             
CLVALN   EQU   *-CLEVELS                                                        
CLEVB    DS    CL(L'ACLVLEN)        1C LEV B LENGTH (A+B)                       
CLEVC    DS    CL(L'ACLVLEN)        1C LEV C LENGTH (A+B+C)                     
CLEVD    DS    CL(L'ACLVLEN)        1C LEV D LENGTH (A+B+C+D)                   
CLEVLLN  EQU   *-CLEVELS                                                        
CLEVNUM  EQU   CLEVLLN/CLVALN                                                   
*                                                                               
LSTPAL   DS    0CL(PLPTYP-PLMTHD)                                               
LSTPMTH  DS    CL(L'PLMTHD)        METHOD                                       
LSTPLV1  EQU   *-LSTPAL                                                         
LSTP1CA  DS    CL(L'PL1CACC)       1C ACCOUNT                                   
LSTPLV2  EQU   *-LSTPAL                                                         
LSTP1RA  DS    CL(L'PL1RACC)       1R ACCOUNT                                   
LSTPLV3  EQU   *-LSTPAL                                                         
LSTPANL  DS    CL(L'PLANAL)        ANALYSIS ACCOUNT                             
*                                                                               
PALTOTS  DS    0PL8                                                             
PKMTHTOT DS    PL8                                                              
PK1CATOT DS    PL8                                                              
PK1RATOT DS    PL8                                                              
PKANLTOT DS    PL8                                                              
PALTNUM  EQU   (*-PALTOTS)/L'PALTOTS                                            
*                                                                               
OPTIONS  DS    0C         PROFILE OPTIONS                                       
OPTPERD  DS    CL1        RUN PERIOD-MONTHLY (OR QTR)      M,Q                  
OPTYTD   DS    CL1        ALLOCATION IS YTD, MTHLY, QTR    Y,M,Q                
OPTOVHY  DS    CL1        ALLOCATE ONLY OVERHEAD YTD       Y,N                  
OPTLOCK  DS    CL1        LOCK TIMESHEET INPUT             Y,N                  
OPTHRS   DS    CL1        STANDARD HOURS                   Y,N                  
OPTHYB   DS    CL1        LEVEL OF 1R TO BILD HYBRID CONTRA                     
OPTSHOWD DS    CL1        SHOW DIRECT GROUP ON INDIRECT PAGES                   
OPTSHOWO DS    CL1        SHOW DIRECT GROUP ON OVERHEAD PAGES                   
OPTDIRLG DS    CL1        LEDGER FOR DIRECT TIME                                
OPTDGRP  DS    CL1        DEFAULT DIRECT GROUP ACCOUNT                          
OPT54    DS    CL1        DEPT. O'HEAD CONTRA LEDGER       5,4                  
OPTOFH   DS    CL1        OFFICE O'HEAD CONTRA LEDGER      4,5,7,D              
OPTC54   DS    CL1        CORP. OVERHEAD CONTRA LEDGER     4,5                  
OPTNBT   DS    CL1        INDIRECT LEVEL FOR  N.B. TIME    D,O,C                
OPTNBA   DS    CL1        ALLOC BASIS  N.B. C=CST I=INC H=HRS P=D+IND           
OPTNBOFF DS    CL1        OFFICE USED TO SPREAD N.B. TIME  R=1R,C=1C            
OPTNBBLG DS    CL1        LEDGER FOR NEW BUSINES BACKOUT                        
OPTNBBAC DS    CL1        ACCOUNT FOR NEW BUSINES BACKOUT                       
OPTNBLDG DS    CL1        LEDGER FOR NEW BUSINES SPREAD BACK                    
OPTNBACC DS    CL1        ACCOUNT FOR NEW BUSINES SPREAD BACK                   
OPTPBT   DS    CL1        INDIRECT LEVEL FOR  P.B. TIME    D,O,C                
OPTPBA   DS    CL1        ALLOC BASIS  P.B. C=CST I=INC H=HRS P=D+IND           
OPTPBOFF DS    CL1        OFFICE USED TO SPREAD P.B. TIME  R=1R,C=1C            
OPTPBBLG DS    CL1        LEDGER FOR PRO BONO BACKOUT                           
OPTPBBAC DS    CL1        ACCOUNT FOR PRO BONO BACKOUT                          
OPTPBLDG DS    CL1        LEDGER FOR PRO BONO SPREAD BACK                       
OPTPBACC DS    CL1        ACCOUNT FOR PRO BONO SPREAD BACK                      
OPTHST   DS    CL1        INDIRECT LEVEL FOR  HOUSE TIME    D,O,C               
OPTHSA   DS    CL1        ALLOC BASIS  HOUSE C=CST I=INC H=HRS P=D+IND          
OPTHSOFF DS    CL1        OFFICE USED TO SPREAD HOUSE TIME  R=1R,C=1C           
OPTHSBLG DS    CL1        LEDGER FOR HOUSE BACKOUT                              
OPTHSBAC DS    CL1        ACCOUNT FOR HOUSE BACKOUT                             
OPTHSLDG DS    CL1        LEDGER FOR HOUSE SPREAD BACK                          
OPTHSACC DS    CL1        ACCOUNT FOR HOUSE SPREAD BACK                         
OPTNO1N  DS    CL1        IGNORE ALL 1N TIME                                    
INDACC   DS    CL1        ACCOUNT FOR INDIRECT TIME        0-9,A-Z              
OPTNOTS  DS    CL1        INDIRECT LEVEL IF NO HOURS       D,O,C                
OPTEXEC  DS    CL1        INDIRECT LEVEL FOR EXECUTIVES    D,O,C                
OPTINDD  DS    CL1        LEDGER FOR DEPT INDIRECT                              
OPTINDO  DS    CL1        LEDGER FOR OFFICE INDIRECT                            
OPTINDC  DS    CL1        LEDGER FOR CORP INDIRECT                              
OPTINOLD DS    CL1        OLD STYLE POINTERS RECOGNIZED(1ND VACATION)           
OPTOVA   DS    CL1        ALLOC BASIS OVH C=CST I=INC H=HRS P=D+IND             
OPTINOA  DS    CL1        ALLOC BASIS OFFICE IND  C=COST,P=DIR+IND              
OPTINAA  DS    CL1        ALLOC BASIS AGENCY IND  C=COST,P=DIR+IND              
OPTSPARE DS    CL2        SPARE                                                 
OPTLNTH  EQU   *-OPTIONS                                                        
*                                                                               
TOTLINE  DS    CL80                                                             
*                                                                               
WARNNUM  DS    CL1                 WARNING NUMBER                               
*                                                                               
LEVEL    DS    CL1                 OVERHEAD LEVEL                               
DEPT     EQU   1                                                                
OFFICE   EQU   2                                                                
COMP     EQU   3                                                                
OGROUP   EQU   4                   OFFICE GROUP                                 
*                                                                               
WRKLOC   DS    0C                  WORK AREA BUCKETS                            
WRKBUK   DS    PL8                                                              
WRKBKLN  EQU   *-WRKBUK                                                         
         ORG   WRKBUK                                                           
OVHLST   DS    PL(WRKBKLN)         OVERHEAD WORK BUCKETS                        
OVHYTD   DS    PL(WRKBKLN)                                                      
OVHPST   DS    PL(WRKBKLN)                                                      
INDLST   DS    PL(WRKBKLN)         WORK AREAS FOR INDIRECT                      
INDYTD   DS    PL(WRKBKLN)                                                      
INDPST   DS    PL(WRKBKLN)                                                      
DIRLST   DS    PL(WRKBKLN)         WORK AREAS FOR DIRECT TIME                   
DIRYTD   DS    PL(WRKBKLN)                                                      
TOTYTD   DS    PL(WRKBKLN)                                                      
INDTTLST DS    PL(WRKBKLN)         WORK AREAS FOR INDIRECT TOTALS               
INDTTYTD DS    PL(WRKBKLN)                                                      
INDTTPST DS    PL(WRKBKLN)                                                      
NUMBUK   EQU   (*-WRKLOC)/WRKBKLN   NUMBER OF WORK BUCKETS                      
*                                                                               
NMEWRK   DS    CL(BCLEN)           WORK AREA FOR CLIENT CODE/NAME TAB           
*                                                                               
PDUMP    DS    PL4                 DUMP ROUTINE                                 
PKAMNT   DS    PL8                 SAVED AREA FOR DOLLARS                       
DIVWRK   DS    PL16                                                             
CLITIME  DS    PL8                                                              
ALLOC    DS    PL8                                                              
ALLOCYTD DS    PL8                 YTD AMOUNT                                   
TOTIME   DS    PL8                                                              
POSTSW   DS    CL1                 M=AS MEMO ,A=ALL OTHERS(NOT MEMO)            
RECSW    DS    XL1                 RECOVERY SWITCH                              
SORTDONE EQU   X'08'               TOTAL SORT COMPLETE                          
HISTBUK  EQU   X'02'               HISTORY BUCKET RECORD                        
UPSI     DS    CL1                 UPSI SWITCH                                  
SORTREP  EQU   X'80'               SORT RECORD REPORT                           
POSTREP  EQU   X'40'               POSTING RECORD REPORT                        
SUPPRESS EQU   X'20'               REPORT PRINTING                              
DUMPRECS EQU   X'10'               DUMP GET /PUT RECORDS                        
COUNTREP EQU   X'08'               RECORDS ADDED COUNTER REPORT                 
PRINTADJ EQU   X'04'               PRINT YTDADJ RECORDS                         
ZEROPOST EQU   X'02'               ZERO OUT MONTHLY POSTINGS                    
POSTRATE EQU   X'01'               POST RATE RECORDS ONLY                       
*                                                                               
INCOME   DS    CL1                 NEED TO READ INCOME C'Y'                     
BUFFIT   DS    XL1                 PUT ADDITIONAL RECORDS TO BUFFALO            
HOURIT   EQU   X'80'               NEED TO PUT HOURS TO BUFFALO C'Y'            
INDIRIT  EQU   X'40'               NEED TO PUT DPT IND TO BUFFALO C'Y'          
BFSTAT   DS    XL1                 FROM BUFFALO STAT                            
HRSSTAT  EQU   X'80'               HOURS FROM BUFFALO                           
INSSTAT  EQU   X'40'               DIRECT + DPT IND FROM BUFFALO                
STARTLED DS    CL2                 STARTING LEDGER TO READ                      
CTRY     DS    CL1                 COUNTRY CODE                                 
BIT      DS    XL1                                                              
TWOPGLST EQU   X'80'               THERE IS A 2 PAGE OFFICE LIST                
CLOSEBOX EQU   X'40'               FLAG TO CLOSE CURRENT BOX                    
OPENBOX  EQU   X'20'               FLAG TO OPEN CURRENT BOX                     
NOPAYHD  EQU   X'10'               DON'T PRINT PAYTYPE HEADINGS                 
COLTWO   EQU   X'08'               PRINTING 2ND ERROR MSG ON THE LINE           
COLTHREE EQU   X'04'               PRINTING 3RD ERROR MSG ON THE LINE           
*                                                                               
TABCOUNT DS    XL1                                                              
OFF1RLN  DS    XL1                 LENGTH OF 1R OFFICE CODE                     
STANDST  DS    CL3                 STANDARD HRS CALL START PERIOD               
START    DS    CL3                 START DATE PACKED                            
END      DS    0CL3                END DATE PACKED                              
MOS      DS    CL2                 YYMM PACKED                                  
DAY      DS    CL1                 DD PACKED                                    
MOST     DS    CL2                 TRANSACTION MOTH OF SERVICE                  
TODAYC   DS    CL2                 TODAY'S DATE COMPRESSED                      
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
FISCAL   DS    CL2                                                              
LAST     DS    CL3                                                              
LMONTH   DS    CL1                 SAVED MONTH FOR MOS FOR MOS LOCK EL          
NUMONTHS DS    PL2                                                              
HEADMON  DS    CL15                                                             
LIVEOPT  DS    CL15               LIVE /DRAFT                                   
GRPDESC  DS    CL(L'OFFLNME)      SAVE GROUPINGS DESCRIPTION                    
SCHEMKEY DS    CL(L'SCHMACCT)     SAVE SCHEME KEY                               
STEND    DS    0CL4               FOR ACSALARY(OVERHEAD)                        
STOVH    DS    CL2                OVERHEAD START                                
ENDOVH   DS    CL2                OVERHEAD END                                  
ACCOUNT  DS    CL12               CURRENT 1R ACCOUNT                            
*                                 SAVE 1R KEY IN SALARYD FORMAT(SALHST)         
OFFIC    DS    CL(L'SALOFFC)      OFFICE                                        
DEPART   DS    CL(L'SALDEPT)      DEPARTMENT                                    
SDEPART  DS    CL(L'SALSDPT)      SUBDEPARTMENT                                 
PERSON   DS    CL(L'SALPRSN)      PERSON                                        
SVLOC    DS    0C                                                               
SVOFFIC  DS    CL(L'SALOFFC)       SAVED OFFICE                                 
SVDEPART DS    CL(L'SALDEPT)       SAVED DEPARTMENT                             
SVSDPT   DS    CL(L'SALSDPT)       SAVED SUBDEPARTMENT                          
SVPERSON DS    CL(L'SALPRSN)       SAVED PERSON                                 
SVLOCLN  EQU   *-SVLOC                                                          
OFFICN   DS    CL36               OFFICE NAME                                   
DEPARTN  DS    CL36               DEPT NAME                                     
PERSONN  DS    CL36               PERSON NAME                                   
SVPAGE   DS    CL2                SAVED PAGE NUMBER                             
*                                                                               
MSG      DS    CL10               MESSAGE FOR DUMP ROUTINE                      
*                                                                               
SAVEMET  DS    CL1                METHOD CODE FROM LAST REQUEST                 
METHOD   DS    CL1                ALLOCTION METHOD NUMBER (DEFAULT 1)           
METHCODE DS    CL3                METHOD CODE                                   
METHNAME DS    CL36               METHOD DESCRIPTION (FOR HEADLINES)            
BUCKIT   DS    CL2                BUCKET TYPE                                   
DA       DS    XL4                DISK ADDRESS                                  
*                                                                               
WRKACM   DS    PL6                                                              
WRKABKLN EQU   *-WRKACM                                                         
         ORG   WRKACM                                                           
PDEBITS  DS    PL6                                                              
PCREDITS DS    PL6                                                              
POSTDR   DS    PL6                                                              
POSTCR   DS    PL6                                                              
WRKNUM   EQU   (*-WRKACM)/WRKABKLN                                              
*                                                                               
FLAG     DS    XL1                 STATUS FLAG                                  
FLGYTD   EQU   X'80'               DOING YTD BACKOUTS FOR P/L RECORDS           
*                                                                               
ACCSTUS  DS    XL1                 CURRENT ACCOUNT'S STATUS                     
WANTIT   EQU   X'80'               WE WANT THIS ACCOUNT                         
OHDACC   EQU   X'40'               THIS IS AN OVERHEAD ACCOUNT                  
NEGHOUR  EQU   X'20'               EMPLOYEE WITH NEGATIVE HOURS                 
NOHOUR   EQU   X'10'               EMPLOYEE WITH NO HOURS                       
INDTACC  EQU   X'08'               THIS IS A IND PAY TYPE ACCOUNT               
ACTIVE   EQU   X'04'               CURRENT PERIOD ACTIVITY SWITCH               
*                                  BRANCH ON CONDITION MASKS (BC)               
MASK     DS    XL1                                                              
MASK2    DS    XL1                                                              
BRANCH   EQU   X'F0'               UNCONDITIONAL BRANCH                         
EQUAL    EQU   X'80'               BRANCH EQUAL                                 
NOTEQUAL EQU   X'70'               BRANCH NOT EQUAL                             
TYPE     DS    XL1                 TYPE OF DATA FOR TOTDIR ROUTINE              
INDITYPE EQU   X'80'               CONSIDERED INDIRECT POOL                     
OHTYPE   EQU   X'40'               CONSIDERED OVERHEAD POOL                     
UNABSORB EQU   X'20'               HAS NOT BEEN ABSORBED                        
DIRGRUP  EQU   X'10'               BUFFALO BY DIRECT 14 GROUP                   
SCHMOPT  DS    XL1                 OPTION BYTE FOR ASCHEME ROUTINE              
NOCLEAR  EQU   X'80'               DON'T CLEAR CLIENT TABLE COMING IN           
LOOKUP   EQU   X'40'               LOOKUP THE SCHEME/PASS BACK ADDRESS          
*                                                                               
POOLBYTE DS    XL1                 TYPE OF POOL FOR POOLDEF(SEE TYPE)           
SCHEDESC DS    XL1                 SCHEME DESCRIP # (SEE SCHMDEF TABL)          
POOLTYPE DS    XL1                 OPTION BYTE TO DESCRIBE POOL                 
*                                  EQUATES FOR POOLTYPE IN SCMD DSECT           
STAT1    DS    XL1                 STATUS BYTE TO FOR SCHEME STATUS 1           
STAT2    DS    XL1                 STATUS BYTE TO FOR SCHEME STATUS 2           
*                                                                               
GROUP    DS    CL1                 GENERAL GROUP CODE                           
DIRGROUP DS    CL1                 DIRECT COST GROUP CODE                       
CSTGRP   DS    CL12                SAVE COSTING GROUP                           
CSTGRPLN DS    XL1                 ACTUAL LENGTH OF CSTGRP                      
CSTGRPNM DS    CL36                SAVE COSTING GROUP NAME                      
MYMODE   DS    CL1                                                              
RUNSTAT  DS    XL1                                                              
POSTERR  EQU   X'80'               RUN HAS ERRORS                               
RUNLIVE  EQU   X'40'               RUN IS LIVE                                  
RUNSOON  EQU   X'20'               RUN IS SOON (DRAFT ONLY)                     
ERROUT   EQU   X'10'               RUN HAS FATAL ERRORS                         
COMMAND  DS    CL8                 COMMAND AREA FOR DATAMGR ADD/WRT             
COMMANDS DS    CL8                 COMMAND AREA FOR RECOVERY                    
MSLLONG  DS    CL1                 EXTENDED MOS ELEMENTS Y OR N                 
         DS    CL9                 SPARE                                        
*                                                                               
SAVEKEY  DS    CL42                                                             
         DS    CL42                                                             
CUL      DS    0CL3                COMPANY/UNIT/LEDGER                          
COMPY    DS    XL1                                                              
UNIT     DS    CL1                                                              
LEDG     DS    CL1                                                              
ELEMENT  DS    CL256               ELEMENT WORK AREA                            
BUKELEM  DS    XL(BUKLNQ)          BUCKET ELEMENT WORK AREA                     
EL44SAVE DS    XL(TRNLN1Q+1)       BUILD AND SAVE COMPOSITE 44 EL               
EL50SAVE DS    XL(SCILN3Q)         BUILD AND SAVE COMPOSITE 50 EL               
*                                                                               
ELIST    DS    3A                  HELLO PARM LIST                              
ELERR    DS    0XL1                HELLO ERROR RETURN BYTE                      
ELADDR   DS    A                   HELLO ELEMENT ADDRESS (GET)                  
ELADDR2  DS    A                   HELLO ELEMENT ADDRESS (ADD)                  
*                                                                               
RATE     DS    PL6                 EMPLOYEE HOURLY RATE                         
FILCOUNT DS    PL4                 FILE RECORD COUNT                            
WRKHRS   DS    PL6                 WORK HOURS FOR RATES ROUTINE                 
         DS    CL24                SPARE                                        
*                                                                               
OFFG     DS    CL2                 OFFICE/OFFICE GROUP CODE                     
AOFFL    DS    A                   ADDRESS OF THE OFFICE ENTRY                  
*                                                                               
PALAREA  DS    XL20                P&L BUCKET AREA                              
***                                                                             
DDOUT    DS    0D                  DATA DICTIONARY OUT                          
         DSDDL PRINT=YES                                                        
STORUSED EQU   *-ACCAD             AMOUNT OF SPACEND USED                       
         EJECT                                                                  
**********************************************************************          
* TABLE DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
* DSECT FOR COMMON WORK NMOD  ENTRIES                                           
*                                                                               
COMD     DSECT                                                                  
COMRELO  DS    AL2                 RELOCATED ADDR OF COMMON NMOD                
COMENT   DS    AL2                 ADDR OF FIRST ENTERABLE ROUTINE              
COMNUMB  DS    AL1                 NUMBER OF ENTERALBE ROUTINES IN NMOD         
COMLEN   EQU   *-COMD                                                           
*                                                                               
* DSECT FOR DATA DICTIONARY UPDATES  (DICTTAB)                                  
*                                                                               
DICD     DSECT                                                                  
DICTAB   DS    AL2                 DISP TO TABLE TO UPDATE                      
DICENTRY DS    AL2                 DISP TO ENTRY WITHIN TAB TO UPDATE           
DICBUMP  DS    AL2                 LENGTH OF TABLE ENTRY TO BUMP                
DICLEN   EQU   *-DICD                                                           
*                                                                               
* DSECT FOR ALLOCATION SCHEME LITERAL DEFINITION                                
*                                                                               
SCHDEFD  DSECT                                                                  
SCHDEFN  DS    AL1                 SCHEME NUMNBER                               
SCHDDEP  EQU   1                   DEFINED AS DEPT                              
SCHDOFF  EQU   2                   DEFINED AS OFFICE                            
SCHDCORP EQU   3                   DEFINED AS CORP                              
SCHDOGP  EQU   4                   BY OFFICE GROUP                              
SCHDOGP2 EQU   5                   BY OFFICE GROUP II                           
SCHDOCLT EQU   6                   OFFICE CLIENTS ONLY                          
SCHDEXCL EQU   7                   EXCLUDE FROM ALLOC                           
SCHDPCT  EQU   8                   POST SAL AT N %                              
SCHDDIR  EQU   9                   DIRECT TIME ONLY                             
SCHDOVER EQU   10                  OFFICE OVERRIDE                              
SCHDINCO EQU   11                  BY OFFICE INCOME                             
SCHDINCC EQU   12                  BY CORP INCOME                               
SCHDEF   DS    CL17                SCHEME LITERAL DEFINITION FOR HEADL          
SCHDEFLN EQU   *-SCHDEFD                                                        
*                                                                               
* DSECT FOR SCHEME LITERAL DEFINITION DEFAULTS                                  
*                                                                               
SCHDFD   DSECT                                                                  
SCHDFCOD DS    AL1                 ANALYSIS CODE                                
SCHDFLIT DS    AL1                 SCHEME LITERAL NUMBER                        
SCHDFLN  EQU   *-SCHDFD                                                         
*                                                                               
* DSECT FOR ALLOCATION SCHEME BUFFER                                            
*                                                                               
SCHMD    DSECT                                                                  
SCHMACCT DS    CL12                OFFICE DEPT OF 1R FOR MATCH                  
SCHMALN  DS    AL1                 LENGTH FOR POOL KEY COMPARE                  
SCHMCOM  DS    AL1                 LENGTH FOR 1C COMPARE                        
*                                                                               
*                                  **   I N D I R E C T **                      
SCHAND   DS    CL1                 AN=D CONVERSION FOR INDIRECT                 
SCHMST_D DS    XL1                 SCHEME STATUS -DEPT IND                      
SCHMEXCL EQU   X'80'               EXCLUDE ALL MATCHES                          
SCHMALL  EQU   X'40'               INCLUDE ALL ITEMS                            
SCHMINCL EQU   X'20'               INCLUDE ALL MATCHES                          
SCHMRECK EQU   X'08'               RECHECK SCHEME VS. DIRECT GROUP TOO          
SCHMOFLT EQU   X'04'               CHECK AGAINST OFFICE LIST                    
SCHMST2D DS    XL1                 SCHEME STATUS 2 (RECHECK STATUS)             
*                                  (CHECK CLIENT VS. DIRECT TIME GROUP)         
*              X'80'               EXCLUDE ALL MATCHES                          
*              X'40'               INCLUDE ALL ITEMS                            
*              X'20'               INCLUDE ALL MATCHES                          
*                                                                               
SCHANO   DS    CL1                 AN=O CONVERSION FOR INDIRECT                 
SCHMST_O DS    XL1                 SCHEME STATUS -OFFC IND                      
SCHMST2O DS    XL1                 (RECHECK STATUS) OFFC IND                    
*                                                                               
SCHANC   DS    CL1                 AN=C CONVERSION  FOR INDIRECT                
SCHMST_C DS    XL1                 SCHEME STATUS -COMP IND                      
SCHMST2C DS    XL1                 (RECHECK STATUS) COMP IND                    
*                                                                               
*                                                                               
*                                  **   O V E R H E A D **                      
SCHOHD   DS    AL1                 WHERE ALLOCATE DEPT LEVEL OH                 
SCHOHO   DS    AL1                 WHERE ALLOCATE OFFICE LEVEL OH               
SCHOHC   DS    AL1                 WHERE ALLOCATE CORP LEVEL OH                 
SCHSTO_D DS    XL1                 SCHEME STATUS -DEPT OVH                      
SCHSTO2D DS    XL1                 (RECHECK STATUS)                             
SCHSTO_O DS    XL1                 SCHEME STATUS -OFFICE OVH                    
SCHSTO2O DS    XL1                 (RECHECK STATUS)                             
SCHSTO_C DS    XL1                 SCHEME STATUS -COMP OVH                      
SCHSTO2C DS    XL1                 (RECHECK STATUS)                             
SCHINDT  DS    AL1                 WHERE ALLOCATE IND PAY TYPE                  
SCHSTI_C DS    XL1                 SCHEME STATUS -IND PAY TYPE                  
SCHSTI2C DS    XL1                 (RECHECK STATUS)                             
SCHMTABI DS    XL4                 ADDR OF OFFLIST FOR OH/IND TIME              
SCHMTAB2 DS    XL4                 ADDR OF OFFLIST FOR IND PAY TYPE             
SCHKLEN  EQU   *-SCHMD                                                          
SCHMLEN  EQU   *-SCHMD                                                          
*                                                                               
* DSECT FOR SCHEME ANALYSIS= AND STATUS TABLE                                   
*       (TABLE SCHMTBL)                                                         
*                                                                               
SCMD     DSECT                                                                  
SCMCOBL  DS    Y                   DISP TO COBLOCK EQUIVALENT                   
SCMANAL  DS    Y                   DISP TO ANALYSIS= IN SCHEME ENTRY            
SCMST1   DS    Y                   DISP TO STATUS 1                             
SCMST2   DS    Y                   DISP TO STATUS 2                             
SCMPTYPE DS    AL1                 SCHEME POOLTYPE                              
SCMINDI  EQU   X'80'               INDIRECT TIME SCHEME                         
SCMOVHE  EQU   X'40'               OVEHEAD SCHEME                               
SCMINDS  EQU   X'20'               IND SALARY TYPE SCHEME                       
*                                                                               
SCMDPT   EQU   X'08'               DEPT TYPE POOL                               
SCMOFC   EQU   X'04'               OFFICE TYPE POOL                             
SCMCOR   EQU   X'02'               COMP TYPE POOL                               
SCMDEF   DS    AL1                 DEFAULT SCHEME DESCRIPTION NUMBER            
SCMLEN   EQU   *-SCMD                                                           
*                                                                               
* DSECT FOR COBLOCK TO SCHEME COVERSION ON AN= AND STATUS                       
*                                                                               
*                                                                               
SCCOD    DSECT                                                                  
SCCOSET  DS    AL1                 COBLOCK SETTING (C,O,D,L,M,N)                
SCCOANAL DS    AL1                 SCHEME EQUIVALENT (C,O,D,G)                  
SCCOOHLV DS    AL1                 SCHEME OH LEVEL(01=DPT,02=OFF,03=CO)         
SCCOSTA1 DS    AL1                 STATUS 1                                     
SCCOSTA2 DS    AL1                 STATUS 2                                     
SCCOLEN  EQU   *-SCCOD                                                          
*                                                                               
* DSECT FOR CCOMPRAT COMPUTING HOURLY RATES TABLE                               
*                                                                               
CMPUTD   DSECT                                                                  
CMBUKTYP DS    CL1                 HOURLY RATE BUCKET TYPE                      
CMDESC   DS    CL3                 HOURLY RATE DESCRIPTION CODE                 
CMRATD   DS    Y                   DISP TO RATD ACCUM FOR HOURLY RATE           
CMSLRD   DS    Y                   DISP TO SALARYD ACCUM FOR SALARY TYP         
CMPTOTD  DS    Y                   DISP TO PTOTD ACCUM FOR SALARY TYPE          
CMPCLIDL DS    Y                   DISP TO CLID ACCUM  SAL TYPE YTD-1           
CMPCLIDY DS    Y                   DISP TO CLID ACCUM  SAL TYPE YTD             
CMPTYIND DS    Y                   DISP TO PAYTYPE ACCUM FOR INDIRECT           
CMPULN   EQU   *-CMPUTD                                                         
*                                                                               
* DSECT FOR CSUMPST POSTING SUMMARY UPDATE TABLE                                
*                                                                               
SUMPD    DSECT                                                                  
SUMPKEY  DS    CL3                 KEY FOR COMPARISON                           
SUMPKLN  DS    AL1                 LENGTH FOR KEY COMPARE                       
SUMPLST  DS    Y                   DISP TO YTD-1 ACCUM                          
SUMPYTD  DS    Y                   DISP TO YTD ACCUM                            
SUMPPST  DS    Y                   DISP TO POSTING ACCUM                        
SUMPLN   EQU   *-SUMPD                                                          
*                                                                               
* DSECT FOR CSTDOPTT STANDARD OPTION UPDATE TABLE                               
*                                                                               
STDOD    DSECT                                                                  
STDSOPT  DS    Y                   DISP TO STANDARD OPTION FOR PROGRAM          
STDBLOCK DS    Y                   DISP TO COBLOCK EQUIVALENT                   
STDOLN   EQU   *-STDOD                                                          
*                                                                               
* DSECT FOR CFIXOPTT FIXED OPTION TABLE                                         
*                                                                               
FIXOD    DSECT                                                                  
FIXSOPT  DS    AL2                 DISP TO STANDARD OPTION FOR PROGRAM          
FIXDFLT  DS    CL1                 BASIC DEFAULT                                
FIXOLN   EQU   *-FIXOD                                                          
*                                                                               
* DSECT FOR CANALCV ANALYSIS CONVERSION TABLE                                   
*                                                                               
ACNVD    DSECT                                                                  
ACNVCODE DS    CL1                 KEY FOR COMPARISON                           
ACNVBIT  DS    X                   BIT SETTING FOR POOLTYPE                     
*                                  (EQUATES FROM SCMD DSECT)                    
*                                  X'08'=DPT,X'04'=OFC,X'02=CORP                
ACNVDIS  DS    AL2                 DISP TO ANALYSIS CONVERSION CODE             
ACNVNOHR DS    CL13                1N CODE TO POST FOR NO HOURS                 
ACNVNOHN DS    CL22                1N CODE NAME FOR NO HOURS                    
ACNVLN   EQU   *-ACNVD                                                          
*                                                                               
* DSECT FOR COHDEFN OVERHEAD DEFINITION TABLE                                   
*                                                                               
OHDFD    DSECT                                                                  
OHDFKEY  DS    AL1                 KEY FOR COMPARISON                           
OHDFDIS  DS    AL2                 DISP TO OVERHEAD DEFINITION CODE             
OHBTYP   DS    CL1                 OVERHEAD BUCKET TYPE                         
OHBBIT   DS    XL1                 BIT SETTING TO MARK POOL                     
OHDFLN   EQU   *-OHDFD                                                          
*                                                                               
* DSECT FOR P AND L RECORD                                                      
*                                                                               
PLBUFD   DSECT                                                                  
PLREC    DS    0C                                                               
PLCPY    DS    XL(L'PLDKCPY)       COMPANY                                      
PLMTHD   DS    CL(L'PLDKMTHD)      METHOD                                       
PL1CACC  DS    CL(L'PLDKCACT)      1C ACCOUNT                                   
PL1RACC  DS    CL(L'PLDKRACT)      1R ACCOUNT                                   
PLANAL   DS    CL(L'PLDKANAL)      ANALYSIS CODE                                
PLPTYP   DS    CL(L'PLDKPTYP)      PAYCODE TYPE(1-SAL/2-PEN/3-BEN)              
PLYYMM   DS    PL(L'PLDKYYMM)      MONTH                                        
PLKLNQ   EQU   *-PLREC                                                          
PLAMNT   DS    PL8                 AMOUNT                                       
PLBUKCNT EQU   (*-PLAMNT)/L'PLAMNT                                              
PLLNQ    EQU   *-PLREC             RECORD LENGTH                                
*                                                                               
* DSECT FOR YTD P AND L RECORD                                                  
*                                                                               
YTDPLD   DSECT                                                                  
YTDPREC  DS    0C                                                               
YTDPCPY  DS    XL(L'PLDKCPY)       COMPANY                                      
YTDPMTHD DS    CL(L'PLDKMTHD)      METHOD                                       
YTDP1C   DS    CL(L'PLDKCACT)      1C ACCOUNT                                   
YTDP1R   DS    CL(L'PLDKRACT)      1R ACCOUNT                                   
YTDPANAL DS    CL(L'PLDKANAL)      ANALYSIS CODE                                
YTDPTYP  DS    CL(L'PLDKPTYP)      PAYCODE TYPE(1-SAL/2-PEN/3-BEN)              
YTDKLNQ  EQU   *-YTDPREC                                                        
         DS    XL1                 SPARE                                        
YTDPSTAT DS    XL(L'PLDKSTA)       STATUS                                       
YTDPPST  EQU   X'80'               TRANSACTION WAS ALREADY POSTED               
YTDPAMNT DS    PL8                 AMOUNT                                       
YTDPBKCT EQU   (*-YTDPAMNT)/L'YTDPAMNT                                          
YTDPLNQ  EQU   *-YTDPREC           RECORD LENGTH                                
*                                                                               
* DSECT FOR CERRDEF ERROR DEFINITION TABLE                                      
*                                                                               
ERDEFD   DSECT                                                                  
ERDFNM   DS    AL1                 ERROR NUMBER (SEE ERRNUM IN ERRORD)          
ERDRCPRG DS    AL1                 RCSUBPRG FOR TITLE IN 01 PHASE               
ERDTYPE  DS    CL1                 WARNING OR ERROR(SEE ERRTYPE ERRORD)         
ERDSTAT  DS    XL1                 STATUS BYTE                                  
ERDHEDUP EQU   X'80'               HEADUP NEW PAGE FOR THIS ERROR               
ERDFCHED EQU   X'40'               SET IF FORCEHED ALREDY SET                   
ERDESCRP DS    CL36                DESCRIPTION                                  
ERDFLN   EQU   *-ERDEFD                                                         
*                                                                               
* DSECT FOR CERROR ERROR TABLE                                                  
*                                                                               
ERRORD   DSECT                                                                  
ERRTYPE  DS    CL1                 ERROR DEFINITION NUMBER                      
ERRERROR EQU   C'E'                ERROR- REQ AND SUBSEQUENT WONT POST          
ERRWARN  EQU   C'W'                WARNING (WON'T STOP YOU POSTING)             
ERRNUM   DS    AL1                 ERROR DEFINITION NUMBER                      
ERRDATES EQU   1                   BAD REQUEST DATES (E)                        
ERRMETH  EQU   2                   INVALID METHOD (E)                           
ERRDRCR  EQU   3                   DEBITS DON'T EQUAL CREDITS(E)                
ERRPTYP  EQU   4                   INVALID PAY TYPE FOR METHOD (E)              
ERRUNABS EQU   5                   UNABSORBED OH/IND POOL(E)                    
ERRNODIR EQU   6                   NO DIRECT TIME FOR SCHEME (E)                
ERRNOHRS EQU   7                   SALARY FOR EMPL BUT NO HOURS (W)             
ERRNEGHR EQU   8                   EMPLOYEE HAS NEGATIVE HOURS (W)              
ERRPERHR EQU   9                   EMPLOYEE HAS PERSONAL HOURS ONLY (W)         
ERRNOSAL EQU   10                  EMPLOYEE HAS HOURS BUT NO SALARY (W)         
ERR1RMIS EQU   11                  IND ACCNT 1ROODDDSS999IND MISSING(E)         
ERRHRNET EQU   12                  EMPL HRS NET TO ZERO FOR MONTH(W)            
ERRSTDHR EQU   13                  INVALID STD HOURS RECORD(E)                  
ERRCONTR EQU   14                  MISSING ACCOUNT RECORD(E)                    
ERRACCT  DS    CL14                ACCOUNT (ORIGIN OF POOL)                     
ERRAMNT  DS    PL8                 AMOUNT                                       
ERRLEN   EQU   *-ERRORD                                                         
         SPACE 3                                                                
*              DSECT FOR MONTHS OF REQUEST TABLE                                
*                                                                               
*                                                                               
MTHD     DSECT                                                                  
MTHCODE  DS    PL2                 YYMM                                         
MTHHRS   DS    PL4                 HOURS                                        
MTHLN    EQU   *-MTHD                                                           
*                                                                               
*              DSECT FOR YTD-1 POSTING TABLE                                    
*                                                                               
YTD1D    DSECT                                                                  
YTDACC   DS    CL12                1C ACCOUNT CODE OR 1R OVH ACCOUNT            
YTDCNTRA DS    0CL13               LEDGER/CONTRA ACCOUNT                        
YTDCNTLD DS    CL1                 LEDGER                                       
YTDCON   DS    CL12                CONTRA ACCOUNT                               
YTDBTYP  DS    0CL1                BUCKET TYPE                                  
YTDBTYPE DS    CL1                 SAL TYPE OR C,O,D,G                          
YTDKLEN  EQU   *-YTD1D                                                          
YTDSTAT  DS    CL1                 YTD-1 STATUS                                 
YTD1USED EQU   X'80'               YTD-1 USED IN THIS ALLOCATION                
YTD1MTH  EQU   X'40'               COMBO HAS POSTING FOR CURRENT MTH            
YTDBK    DS    PL8                 CHANGED FROM PL6 FOR BUFFERIN                
YTDBKLN  EQU   *-YTDBK                                                          
         ORG   YTDBK                                                            
YTD1AMT  DS    PL(YTDBKLN)         AMOUNT                                       
YTDBKCNT EQU   (*-YTDBK)/YTDBKLN                                                
YTDLEN   EQU   *-YTD1D                                                          
*                                                                               
* DSECT FOR ERROR PRINT LINE                                                    
*                                                                               
ERLIND   DSECT                                                                  
ERLNDF   DS    CL(L'ERDESCRP)      ERROR DEFINITION                             
         DS    CL1                                                              
ERLNAC   DS    CL(L'ERRACCT)       ACCOUNT                                      
         DS    CL1                                                              
ERPAGE   DS    CL4                                                              
ERLNAMT  DS    CL11                AMOUNT                                       
         ORG   ERLNDF                                                           
ERRACNT  DS    CL(L'ERRACCT)       ERROR 1 (3 ERRORS PER LINE)                  
         DS    CL1                                                              
ERRPAGE  DS    CL4                                                              
ERRAMT   DS    CL11                                                             
         DS    CL10                                                             
ERRACT2  DS    CL(L'ERRACCT)       ERROR 2                                      
         DS    CL1                                                              
ERRPAGE2 DS    CL4                                                              
ERRAMT2  DS    CL11                                                             
         DS    CL10                                                             
ERRACT3  DS    CL(L'ERRACCT)       ERROR 3                                      
         DS    CL1                                                              
ERRPAGE3 DS    CL4                                                              
ERRAMT3  DS    CL11                                                             
ERLNLN   EQU   *-ERLIND                                                         
         SPACE 3                                                                
*              DSECT FOR SORTER BUCKET REPORT LINE DSECT                        
*                                                                               
*                                                                               
BKLIND   DSECT                                                                  
BKACCT   DS    CL14                ACCOUNT                                      
         DS    CL3                                                              
BKCNTRA  DS    CL14                CONTRA ACCOUNT                               
         DS    CL3                                                              
BKCNME   DS    CL36                CONTRA ACCOUNT NAME                          
         DS    CL1                                                              
BKBKTP   DS    CL2                 BUCKET TYPE                                  
         DS    CL1                                                              
BKDEB    DS    CL11                DEBIT AMOUNT                                 
         DS    CL3                                                              
BKCRD    DS    CL11                CREDIT AMOUNT                                
BKLNLN   EQU   *-BKLIND                                                         
         SPACE 3                                                                
*                                                                               
*              DSECT FOR SALARY PRINT WORK AREA                                 
*                                                                               
*                                                                               
SALBLKD  DSECT                                                                  
SALCOD   DS    CL3                SALARY CODE                                   
SALEQUAL DS    CL1                                                              
SALFIG   DS    CL11               DOLLAR FIGURE                                 
         DS    CL1                                                              
SALPERH  DS    CL10               PER HOUR                                      
SALBLKLN EQU   *-SALBLKD          SALARY BLOCK LENGTH                           
         SPACE 3                                                                
*              DSECT FOR A CLIENT DETAILS - WITHIN AN ACCOUNT                   
*                                                                               
CLID     DSECT                                                                  
CLILEDAC DS    0CL13               LEDGER/ACCOUNT                               
CLILEDG  DS    CL1                 LEDGER                                       
*                                  C=CLIENT TIME                                
*                                  N=NON CLIENT TIME                            
CLICDE   DS    CL12                CLIENT CODE                                  
CLGRPING DS    CL12                CLIENT'S GROUPING                            
CLIANALS DS    CL1                 NON-CLIENT'S ANALYSIS CODE(CONVERT)          
CLIANALR DS    CL1                 NON-CLIENT'S ANALYSIS CODE(ORIG)             
CLISTAT  DS    XL1                                                              
CLINB    EQU   X'80'               NEW BIZ                                      
CLIPB    EQU   X'40'               PROBONO                                      
CLIHS    EQU   X'20'               HOUSE                                        
CLIIND   EQU   X'10'               DEPT INDIRECT AMOUNT                         
CLIKLEN  EQU   *-CLID              KEY LENGTH                                   
CLICLNME DS    CL36                CLIENT NAME                                  
CLICLIST DS    CL(L'LISTCDE)       CLIENT LIST CODE (FOR 1N)                    
CLIDLEN  EQU   *-CLID              DATA LENGTH                                  
CLIBK    DS    PL8                                                              
CBUKLEN  EQU   *-CLIBK             SIZE OF A BUCKET  /---OH AND IND             
         ORG   CLIBK                                                            
CDIRLST  DS    PL(CBUKLEN)         DIRECT TIME YTD-1 /                          
CDIRYTD  DS    PL(CBUKLEN)         DIRECT TIME YTD   /                          
CDIRPST  DS    PL(CBUKLEN)                           /                          
         ORG   CDIRLST                                                          
CPERHRS  DS    PL(CBUKLEN)         PERIOD HOURS      /                          
CYTDHRS  DS    PL(CBUKLEN)         YTD HOURS         /---EMPLOYEE               
CADJHRS  DS    PL(CBUKLEN)         ADJUSTED HOURS    /                          
*                                                                               
CCSTLST  DS    PL(CBUKLEN)         COST YTD-1                                   
CCSTYTD  DS    PL(CBUKLEN)         COST YTD                                     
CCSTPST  DS    PL(CBUKLEN)         COST POSTING                                 
CBKCNT   EQU   (*-CLIBK)/CBUKLEN   NUMBER OF BUCKETS                            
CLEN     EQU   *-CLID              RECORD LENGTH OF OH/IND CLI TOTALS           
*                                                                               
CCSTLST1 DS    PL(CBUKLEN)         SAL TYPE 1 COST YTD-1                        
CCSTYTD1 DS    PL(CBUKLEN)         SALTYPE 1 COST YTD                           
CCSTLST2 DS    PL(CBUKLEN)         SAL TYPE 2 COST YTD-1                        
CCSTYTD2 DS    PL(CBUKLEN)         SALTYPE 2 COST YTD                           
CCSTLST3 DS    PL(CBUKLEN)         SAL TYPE 3 COST YTD-1                        
CCSTYTD3 DS    PL(CBUKLEN)         SALTYPE 3 COST YTD                           
CBKCNT2  EQU   (*-CLIBK)/CBUKLEN   NUMBER OF BUCKETS                            
CLEN2    EQU   *-CLID              RECORD LENGTH OF EMPL CLI TOTALS             
         SPACE 3                                                                
*                                                                               
* DSECT FOR POOL PAYTYPE PRINTING                                               
*                                                                               
PLPAYD   DSECT                                                                  
PLPAYDSC DS    CL3                 PAYTYPE DESCRIPTION                          
         DS    CL1                                                              
PLPAYEQ  DS    CL1                                                              
         DS    CL1                                                              
PLPAYAMT DS    CL11                PAYTYPE AMOUNT                               
*                                                                               
* DSECT FOR PRINT LINE                                                          
*                                                                               
PLINED   DSECT                                                                  
PRTLNE   DS    0H                                                               
         DS    CL5                                                              
PMTHD    DS    CL1                 METHOD                                       
         DS    CL5                                                              
P1CA     DS    CL12                1C ACCOUNT                                   
         DS    CL5                                                              
P1RA     DS    CL12                1R ACCOUNT                                   
         DS    CL5                                                              
PANAL    DS    CL1                 ANALYSIS ACCOUNT                             
         DS    CL5                                                              
PAMNT    DS    CL10                DOLLARS                                      
         DS    CL5                                                              
PLNQ     EQU   *-PRTLNE                                                         
*                                                                               
* DSECT FOR PERSON COLUMN HEADINGS                                              
*                                                                               
PRNTPHD  DSECT                                                                  
         DS    CL1                                                              
PHCLI    DS    CL6                 CLIENT                                       
PHDIV    DS    CL1                 /                                            
PHNCLI   DS    CL10                NON-CLIENT                                   
         DS    CL36                                                             
PHPERD   DS    CL6                 PERIOD                                       
         DS    CL1                                                              
PHHRS    DS    CL3                 HOURS                                        
         DS    CL4                                                              
PHYTD    DS    CL3                 YTD                                          
         DS    CL1                                                              
PHHRS2   DS    CL3                 HOURS                                        
         DS    CL3                                                              
PHYTD2   DS    CL3                 YTD                                          
         DS    CL1                                                              
PHADJHR  DS    CL7                                                              
         DS    CL3                                                              
PHYTD3   DS    CL3                 YTD                                          
PHYTDN1  DS    CL2                 -1                                           
         DS    CL1                                                              
PHCOST   DS    CL4                 COST                                         
         DS    CL4                                                              
PHYTD4   DS    CL3                 YTD                                          
         DS    CL1                                                              
PHCOST2  DS    CL4                 COST                                         
         DS    CL3                                                              
PHPOST   DS    CL7                 POSTING                                      
         DS    CL1                                                              
PHAMT    DS    CL3                 AMT                                          
         SPACE 3                                                                
*                                                                               
*              DSECT FOR A PERSON REPORT PRINT LINE                             
PRNTPD   DSECT                                                                  
         DS    CL1                                                              
PTCLI    DS    CL12                CLIENT/NON CLIENT                            
         DS    CL1                                                              
PTCLIAN  DS    CL1                 ANALYSIS CODE                                
         DS    CL1                                                              
PTCLINM  DS    CL36                NAME                                         
         DS    CL2                                                              
PTHR     DS    0CL11                                                            
PTHRP    DS    CL11                HOURS FOR PERIOD                             
PTHRY    DS    CL11                HOURS YTD                                    
PTHRA    DS    CL11                HOURS YTD ADJUSTED                           
PTCOS    DS    0CL14                                                            
PTCOS1   DS    CL14                COST YTD-1                                   
PTCOSY   DS    CL14                COST YTD                                     
PTCOSP   DS    CL14                COST POSTED                                  
PTLN1    EQU   *-PRNTPD                                                         
         ORG   PRNTPD                                                           
*                                  OVERHEAD PAGE                                
         DS    CL1                                                              
PTOHCLI  DS    CL12                CLIENT                                       
         DS    CL1                                                              
PTOHCLNM DS    CL36                NAME (OVERHEAD PAGE)                         
         DS    CL5                                                              
         ORG   PTOHCLNM+30                                                      
PTOHGRP  DS    0CL11               DIRECT GROUP CODE                            
PTOHGRPL DS    CL1                 DIRECT LEDGER                                
PTOHGRPC DS    CL10                CODE                                         
         DS    CL3                                                              
PTOHCOS  DS    0CL14                                                            
PTOHCOS1 DS    CL14                COST OR INCOME YTD-1                         
PTOHCOSY DS    CL14                COST OR INCOME YTD                           
PTOH     DS    0CL14                                                            
PTOH1    DS    CL14                OVERHEAD YTD-1                               
PTOHY    DS    CL14                OVERHEAD YTD                                 
PTOHP    DS    CL14                OVERHEAD POSTED                              
PTLN2    EQU   *-PRNTPD                                                         
         ORG   PRNTPD                                                           
*                                  INDIRECT PAGE                                
         DS    CL1                                                              
PTINCLI  DS    CL12                CLIENT                                       
         DS    CL1                                                              
PTINCLNM DS    CL30                NAME (INDIRECT PAGE)                         
         ORG   PTINCLNM+20                                                      
PTINGRPC DS    CL10                CODE (DIRECT CODE FOR TESTING ONLY)          
PTINCOS  DS    0CL14                                                            
PTINCOS1 DS    CL14                COST OR INCOME YTD-1                         
PTINCOSY DS    CL14                COST OR INCOME YTD                           
PTINCOSP DS    CL14                COST POSTED OR MTHY INCOME                   
PTIN     DS    0CL14                                                            
PTIN1    DS    CL14                INDIRECT YTD-1                               
PTINY    DS    CL14                INDIRECT YTD                                 
PTINP    DS    CL14                INDIRECT POSTED                              
PTLN3    EQU   *-PRNTPD                                                         
         ORG   PRNTPD                                                           
*                                  OFFICE/DEPT SUMMARY PAGE                     
         DS    CL1                                                              
PTSMDESP DS    CL15                SUMMARY LINE DESCRIPTION                     
         ORG   PTSMDESP                                                         
         DS    CL4                                                              
PTSMOFF  DS    CL2                 SUMMARY OFFICE                               
         DS    CL5                                                              
PTSMDPT  DS    CL3                 SUMMARY DEPARTMENT                           
         DS    CL1                                                              
*                                                                               
         DS    CL6                                                              
PTSM     DS    0CL11                                                            
PTSMDIR  DS    CL11                SUMMARY OF DIRECT POSTING                    
PTSMDIND DS    CL11                SUMMARY OF DEPT INDIRECT POSTINGS            
PTSMOIND DS    CL11                SUMMARY OF OFFC INDIRECT POSTINGS            
PTSMCIND DS    CL11                SUMMARY OF CORP INDIRECT POSTINGS            
PTSMTIME DS    CL11                SUMMARY OF TOTAL TIME POSTINGS               
PTSMOVH  DS    CL11                SUMMARY OF OVERHEAD POSTINGS                 
PTSMOVHO DS    CL11                SUMMARY OF OVERHEAD OTHER POSTINGS           
PTSTOT   DS    CL11                TOTAL COST                                   
PTSMCNT  EQU   (*-PTSM)/L'PTSM     NUMBER OF COLS TO PRINT                      
PTLN4    EQU   *-PRNTPD                                                         
         EJECT                                                                  
*              DSECT FOR OFFICE/DEPT SUMMARY RECORDS                            
*                                                                               
SUMD     DSECT                                                                  
SOFFDEP  DS    CL12                                                             
*                                  OFFICE CODE X'FF' = AGENCY TOTALS            
*                                  DEPT. CODE X'FFFF' = OFFICE TOTALS           
*                                                                               
SKLEN    EQU   *-SUMD                                                           
SBK      DS    PL8                                                              
SBKLN    EQU   *-SBK                                                            
         ORG   SBK                                                              
SDIRPST  DS    PL(SBKLN)           DIRECT POSTING                               
SDINDPST DS    PL(SBKLN)           DEPT. INDIRECT POSTING                       
SOINDPST DS    PL(SBKLN)           OFFICE INDIRECT POSTING                      
SCINDPST DS    PL(SBKLN)           CORP. INDIRECT POSTING                       
STTIME   DS    PL(SBKLN)           TOTAL TIME                                   
SOVHPST  DS    PL(SBKLN)           OVERHEAD POSTING                             
SOVOPST  DS    PL(SBKLN)           OTHER OVERHEAD                               
STCOST   DS    PL(SBKLN)           TOTAL COST                                   
SBKSCNT  EQU   (*-SDIRPST)/SBKLN   NUMBER OF SUMMARY PAGE BUCKETS               
SDIRLST  DS    PL(SBKLN)           DIRECT YTD-1                                 
SDIRYTD  DS    PL(SBKLN)           DIRECT YTD                                   
SDINDLST DS    PL(SBKLN)           DEPT. INDIRECT YTD-1                         
SDINDYTD DS    PL(SBKLN)           DEPT. INDIRECT YTD                           
SOINDLST DS    PL(SBKLN)           OFFICE INDIRECT YTD-1                        
SOINDYTD DS    PL(SBKLN)           OFFICE INDIRECT YTD                          
SCINDLST DS    PL(SBKLN)           CORP. INDIRECT YTD-1                         
SCINDYTD DS    PL(SBKLN)           CORP. INDIRECT YTD                           
SOVHLST  DS    PL(SBKLN)           OVERHEAD YTD-1                               
SOVHYTD  DS    PL(SBKLN)           OVERHEAD YTD                                 
SOVOLST  DS    PL(SBKLN)           OTHER OVERHEAD YTD-1                         
SOVOYTD  DS    PL(SBKLN)           OTHER OVERHEAD YTD                           
SDANDLST DS    PL(SBKLN)           DEPT. TOTAL ANALYSIS=D YTD-1                 
SDANDYTD DS    PL(SBKLN)           DEPT. TOTAL ANALYSIS=D YTD                   
SDANDPST DS    PL(SBKLN)           DEPT. TOTAL ANALYSIS=D TO POST               
SOANDLST DS    PL(SBKLN)           OFFC. TOTAL ANALYSIS=O YTD-1                 
SOANDYTD DS    PL(SBKLN)           OFFC. TOTAL ANALYSIS=O YTD                   
SOANDPST DS    PL(SBKLN)           OFFC. TOTAL ANALYSIS=O TO POST               
SCANDLST DS    PL(SBKLN)           CORP. TOTAL ANALYSIS=C YTD-1                 
SCANDYTD DS    PL(SBKLN)           CORP. TOTAL ANALYSIS=C YTD                   
SCANDPST DS    PL(SBKLN)           CORP. TOTAL ANALYSIS=C TO POST               
SBKCNT   EQU   (*-SBK)/SBKLN       NUMBER OF BUCKETS                            
SLEN     EQU   *-SUMD              LENGTH                                       
         SPACE 3                                                                
*              DSECT FOR INDIRECT TIME RECORDS                                  
*                                                                               
INDID    DSECT                                                                  
INDOFDEP DS    CL12                OFFICE/DEPT                                  
INOFDPFM DS    CL12                OFFICE/DEPT                                  
INCLEN   EQU   *-INDID                                                          
INDTYPE  DS    CL1                 TYPE OF INDIRECT                             
INDDEPT  EQU   C'D'                DEPT                                         
INDOFFIC EQU   C'O'                OFFICE                                       
INDCORP  EQU   C'C'                CORP                                         
INDGRUP  EQU   C'G'                OFFICE GROUP                                 
INDSALTP DS    CL(L'CMBUKTYP)      SALARY TYPE                                  
INDORIG  DS    CL1                 INDIRECT ORIGINATED FROM                     
*        EQU   C'D'                DEPT                                         
*        EQU   C'O'                OFFICE                                       
*        EQU   C'C'                CORP                                         
INDCLIST DS    CL(L'LISTCDE)       INDIRECT CLIENT LIST                         
INDSTATS DS    XL1                 STATUS OF INDIRECT                           
INDINCOM EQU   X'80'               ALLOCATE BY INCOME                           
INDNEWBI EQU   X'40'               NEW BUSINESS (DIRECT)                        
INDNEWEX EQU   X'20'               NEW BUSINESS (EXPENSE)                       
INDPROBO EQU   X'10'               PRO BONO (DIRECT)                            
INDPROEX EQU   X'08'               PRO BONO (EXPENSE)                           
INDHOUSE EQU   X'04'               HOUSE    (DIRECT)                            
INDPLUS  EQU   X'02'               ALLOCATE BY DIRECT + DPT INDIRECT            
INDHOURS EQU   X'01'               ALLOCATE BY HOURS                            
INKLEN   EQU   *-INDID                                                          
INDSTAT2 DS    XL1                 STATUS OF INDIRECT                           
INDABSOR EQU   X'80'               IND HAS BEEN ABSORBED                        
INDSECUR EQU   X'40'               CLIENT SECURITY CHECK                        
INBK     DS    PL8                                                              
INBKLN   EQU   *-INBK                                                           
         ORG   INBK                                                             
INDSALAC DS    PL(INBKLN)          INDIRECT SALARY ACCUMULATOR                  
INDBENAC DS    PL(INBKLN)          INDIRECT BENEFIT ACCUMULATOR                 
INDPENAC DS    PL(INBKLN)          INDIRECT PENSION ACCUMULATOR                 
INDTOTAC DS    PL(INBKLN)          TOTAL FOR PAYTYPES                           
*NDDYTD  DS    PL(INBKLN)                        YTD                            
INBUKCNT EQU   (*-INBK)/INBKLN                                                  
INLEN    EQU   *-INDID                                                          
         SPACE 3                                                                
*              DSECT FOR OFFICE/DEPT OVERHEAD RECORDS                           
*                                                                               
OVERD    DSECT                                                                  
OVACCT   DS    CL12                OVERHEAD ACCOUNT                             
OVFRMACT DS    CL12                FROM OVERHEAD ACCOUNT (ORIGIN)               
OVHLEVL  DS    CL1                 LEVEL OF OVERHEAD(SCHEME OVERRIDE)           
OVHORIG  DS    CL1                 LEVEL OF OVERHEAD (EQU FROM"LEVEL")          
OVHTYPE  DS    CL1                 TYPE OF OVERHEAD                             
OVHACCT  EQU   C'A'                ACCOUNT (NORMAL OVERHEAD)                    
OVHINDP  EQU   C'I'                IND PAY TYPE OVERHEAD                        
OVHUNABS EQU   C'U'                UNABSORBED FROM ANOTHER LEVEL                
OVHSTATS DS    XL1                 STATUS OF OH                                 
OVHINCOM EQU   X'80'               ALLOCATE BY INCOME                           
OVHHOURS EQU   X'40'               ALLOCATE BY HOURS                            
OVHPLUS  EQU   X'20'               ALLOCATE BY DIRECT + DPT INDIRECT            
OVKLEN   EQU   *-OVERD                                                          
OVHSTAT2 DS    XL1                 STATUS OF OH                                 
OVHABSOR EQU   X'80'               OH HAS BEEN ABSORBED                         
OVHTOTBL EQU   X'40'               MARK ENTRY TO TABLE(IND PAY TYPE)            
OVBK     DS    PL8                                                              
OVBKLN   EQU   *-OVBK                                                           
         ORG   OVBK                                                             
OVALST   DS    PL(OVBKLN)          ACCOUNT OVERHEAD YTD -1                      
OVAYTD   DS    PL(OVBKLN)                           YTD                         
OVBUKCNT EQU   (*-OVBK)/OVBKLN                                                  
OVLEN    EQU   *-OVERD                                                          
         SPACE 3                                                                
*              DSECT FOR POOL DESCRIPTIONS FOR HEADLINES                        
*                                                                               
POOLD    DSECT                                                                  
POOTYPE  DS    AL1                 OHTYPE OR INDITYPE                           
POOLCAT  DS    AL1                 DEPT, OFFICE, CORP UNABSORBED ETC            
POOLSUB  DS    AL1                 SUB TYPE GRUP IND NEWBIZ PROBONO HSE         
POOLDES  DS    CL14                DESCRIPTION FOR HEADLINE                     
POOLLN   EQU   *-POOLD                                                          
         SPACE 3                                                                
*              DSECT FOR CLIENT CODE/NAME LIST                                  
*                                                                               
BCLID    DSECT                                                                  
BCLICDE  DS    CL12                CLIENT CODE                                  
BCKLEN   EQU   *-BCLID                                                          
BCLINME  DS    CL36                CLIENT NAME                                  
BCLEN    EQU   *-BCLID                                                          
         SPACE 3                                                                
*              DSECT FOR HYBRID CODE/NAME LIST                                  
*                                                                               
HYBND    DSECT                                                                  
HYCODE   DS    CL12                HYBRID CODE                                  
HYKLEN   EQU   *-HYBND                                                          
HYNAME   DS    CL36                HYBRID NAME                                  
HYBNLEN  EQU   *-HYBND                                                          
         SPACE 3                                                                
*              DSECT FOR NEW BUSINESS /PRO BONO CLIENTS FROM 1C                 
*              (MAY CONTAIN HIGHER LEVEL 1C'S)                                  
*                                                                               
NEWBD    DSECT                                                                  
NEWBCDE  DS    0CL12               CLIENT CODE                                  
         DS    CL11                                                             
NEWBCLST DS    CL1                 LAST BYTE OF CLIENT CODE                     
NEWLEN   EQU   *-NEWBD                                                          
NEWBCLN  DS    XL1                 ACTUAL CODE ENTRY LENGTH                     
NEWBSTAT DS    XL1                 STATUS                                       
NEWBNB   EQU   X'80'               NEW BUSINESS CLIENT                          
NEWBPB   EQU   X'40'               PRO BONO CLIENT                              
NEWBHS   EQU   X'08'               HOUSE CLIENT                                 
NEWBLEN  EQU   *-NEWBD                                                          
         SPACE 3                                                                
*              DSECT FOR 1N NON CLIENT CODES AND POINTERS                       
*                                                                               
*                                                                               
NOND     DSECT                                                                  
NONCODE  DS    CL12                NON CLIENT CODE                              
NONKLEN  EQU   *-NOND                                                           
NONANAL  DS    CL1                 1R LEV SPREAD BACK/ANALYSIS CODE             
NONNAME  DS    CL36                NON CLIENT NAME                              
*                                                                               
NONLMET  DS    CL1                 LIST CODES METHOD                            
NONLISC  DS    CL(L'LISTCDE)       LIST CODE                                    
         DS    CL(L'LSTKLST-L'NONLISC) ALIGN WITH FFTELQ (FFTTINDL)             
NONLSTAT DS    XL1                 LIST STATUS                                  
NONLLEN  EQU   *-NONLMET                                                        
NONSTAT  DS    XL1                 STATUS                                       
NONSLST  EQU   X'80'               CLIENT LIST PRESENT                          
NONLEN   EQU   *-NOND                                                           
         SPACE 3                                                                
*              DSECT FOR PERSON/ACCOUNT HOURLY RATES                            
RATD     DSECT                                                                  
RATACC   DS    CL12                EMPLOYEE CODES                               
RTKLEN   EQU   *-RATD              KEY LENGTH                                   
RBUKS    DS    PL6                                                              
RBUKLN   EQU   *-RBUKS                                                          
         ORG   RBUKS                                                            
RRAT     DS    PL(RBUKLN)          OVERALL HRLY RATE (4 DECIMALS)               
RSAL     DS    PL(RBUKLN)          HRLY SALARY RATE                             
RPEN     DS    PL(RBUKLN)          HRLY PENSION RATE                            
RBEN     DS    PL(RBUKLN)          HRLY BENEFIT RATE                            
RBKCNT   EQU   (*-RBUKS)/RBUKLN    NUMBER OF BUCKETS                            
RATLEN   EQU   *-RATD              RECORD LENGTH                                
         SPACE 3                                                                
*              DSECT FOR PERSON/ACCOUNT  HRS,COMPENSATION,COST TOTALS           
PTOTD    DSECT                                                                  
PBK      DS    PL6                                                              
PBUKLN   EQU   *-PBK                                                            
         ORG   PBK                                                              
PPERHRS  DS    PL6                 PERIOD HOURS                                 
PPERHRP  DS    PL6                 PERIOD HOURS (+)                             
PPERHRN  DS    PL6                 PERIOD HOURS (-)                             
PYTDHRS  DS    PL6                 YTD HOURS                                    
PRSYTDH  DS    PL6                 YTD PERSONAL HOURS                           
PNONPER  DS    PL6                 YTD NON-PERS                                 
PDIRLST  DS    PL6                 YTD-1 DIRECT COST                            
PINDLST  DS    PL6                 YTD-1 INDIRECT COST                          
PCOMPBK  EQU   *                   COMPENSATION BUCKETS                         
PCOST    DS    PL6                 TOTAL COST  (SALARY + PERCENT)               
PSAL     DS    PL6                 PURE SALARY COST                             
PPEN     DS    PL6                 PENSION ALLOCATION                           
PBEN     DS    PL6                 BENEFIT AMOUNT                               
PERADJ   DS    PL6                 ADJUSTING AMOUNT                             
PBKCNTC  EQU   (*-PCOMPBK)/PBUKLN  NUMBER OF COMPENSATION BUCKETS               
PBKCNT   EQU   (*-PBK)/PBUKLN      TOTAL NUMBER OF BUCKETS                      
PERSTAT  DS    XL1                 PERSON STATUS BYTE                           
PERDIRT  EQU   X'80'               POST DIRECT TIME ONLY FOR THIS EMPL          
PADJPCT  EQU   X'40'               ADJUSTMENT AMOUNT IS A PCT                   
PBAKOUT  EQU   X'20'               TOTAL TIME BACKOUT ADJ REQUIRED              
PEREXEC  EQU   X'10'               EMPLOYEE IS EXECUTIVE                        
PERERROR EQU   X'08'               EMPLOYEE ERROR                               
PERENOHR EQU   X'04'               EMPLOYEE HAS NO HOURS **WARNING**            
PERENOSL EQU   X'02'               EMPLOYEE HAS NO SALARY**WARNING**            
PERUSEAC EQU   X'01'               USE ACTUAL HOURS FOR STANDARD                
PERSTAT2 DS    XL1                 PERSON STATUS BYTE 2                         
PERHRRAT EQU   X'80'               INCLUDE HOURLY RATES                         
PERHRRTO EQU   X'40'               ONLY HOURLY RATES                            
PERHRRTA EQU   X'20'               HRLY RATES ADJ FOR NO HRS IN PERIOD          
PTOTLEN  EQU   *-PTOTD                                                          
         EJECT                                                                  
*              DSECT FOR DEPARTMENT ACCUMULATORS                                
DEPD     DSECT                                                                  
DEBK     DS    PL8                 LOCATION OF BUCKETS                          
DEBUKLN  EQU   *-DEBK                                                           
         ORG   DEBK                                                             
DDINDAL  DS    PL(DEBUKLN)         DEPT. INDIRECT ABSORBED  YTD-1               
DDINDAY  DS    PL(DEBUKLN)                                  YTD                 
DDINDAP  DS    PL(DEBUKLN)                                  POST                
DOVHAL   DS    PL(DEBUKLN)         DEPT. OVERHEAD ABSORBED  YTD-1               
DOVHAY   DS    PL(DEBUKLN)                                  YTD                 
DOVHAP   DS    PL(DEBUKLN)                                  POST                
DEBKCNT  EQU   (*-DEBK)/DEBUKLN                                                 
DEPLEN   EQU   *-DEPD                                                           
*                                                                               
*                                                                               
*              DSECT FOR OFFICE ACCUMULATORS                                    
OFFD     DSECT                                                                  
OFBK     EQU   *                                                                
OFDACCUM DS    (DEBKCNT)PL(DEBUKLN)  DEPT TOTALS (DEPD)                         
OFBUKLN  EQU   DEBUKLN                                                          
OFINDADL DS    PL(OFBUKLN)         DPT AS OFF INDIRECT (ABSORBED) YTD-1         
OFINDADY DS    PL(OFBUKLN)                                        YTD           
OFINDADP DS    PL(OFBUKLN)                                        POST          
OFINDAL  DS    PL(OFBUKLN)         OFFICE INDIRECT (ABSORBED) YTD-1             
OFINDAY  DS    PL(OFBUKLN)                                    YTD               
OFINDAP  DS    PL(OFBUKLN)                                    POST              
OFOVHAL  DS    PL(OFBUKLN)         OFFICE OVERHEAD (ABSORBED) YTD-1             
OFOVHAY  DS    PL(OFBUKLN)                                    YTD               
OFOVHAP  DS    PL(OFBUKLN)                                    POST              
OFBKCNT  EQU   (*-OFBK)/OFBUKLN                                                 
OFFLEN   EQU   *-OFFD                                                           
*                                                                               
*                                                                               
*              DSECT FOR AGENCY ACCUMULATORS                                    
AGYD     DSECT                                                                  
AGBK     EQU   *                                                                
AGOACCUM DS    (OFBKCNT)PL(OFBUKLN) OFFICE TOTALS (OFFD)                        
AGBUKLN  EQU   DEBUKLN                                                          
AGINDAL  DS    PL(AGBUKLN)         CORP. INDIRECT ABSORBED YTD-1                
AGINDAY  DS    PL(AGBUKLN)                                 YTD                  
AGINDAP  DS    PL(AGBUKLN)                                 POST                 
AGOVHAL  DS    PL(AGBUKLN)         CORP OVERHEAD ABSORBED  YTD-1                
AGOVHAY  DS    PL(AGBUKLN)                                 YTD                  
AGOVHAP  DS    PL(AGBUKLN)                                 POST                 
AGBKCNT  EQU   (*-AGBK)/AGBUKLN                                                 
AGYLEN   EQU   *-AGYD                                                           
         EJECT                                                                  
*              DSECT FOR BUFFALO RECORDS                                        
*                                  LEVEL 1=DEPT, 2=OFFICE, 3=COMPANY            
BUFD     DSECT                                                                  
BUFKEY   DS    0C                                                               
BUFTYPE  DS    CL1                                                              
BUFBYGP  EQU   1                   CLIENTS BY GROUP                             
BUFNOGP  EQU   2                   CLIENTS NO GROUP                             
BUFNEWB  EQU   3                   NEW BUSINESS CLIENTS                         
BUFINCM  EQU   4                   CLIENT INCOME                                
BUFBYGPH EQU   5                   CLIENTS BY GROUP (HOURS)                     
BUFNOGPH EQU   6                   CLIENTS NO GROUP (HOURS)                     
BUFBYGPI EQU   7                   CLIENTS BY GRP DIRECT + DPT INDIRECT         
BUFNOGPI EQU   8                   CLIENTS NO GRP DIRECT + DPT INDIRECT         
BUFCLI   DS    0CL13               LEDGER/CLIENT                                
BUFLEDG  DS    CL1                 LEDGER                                       
BUFCLT   DS    CL12                CLIENT                                       
BUFTOT   EQU   0                   TOTAL RECORD =X'00'                          
BUFGRPCD DS    0CL13               GROUP CODE                                   
BUFGLDG  DS    CL1                 LEDGER                                       
BUFGRP   DS    CL1                 GROUP FOR DIRECT  DEPT/CORP =X'00'           
BUFGCOD  DS    CL11                REMAINING BYTES OF GROUP CODE                
BUFKLEN  EQU   *-BUFKEY            KEY LENGTH                                   
BUFBK    DS    PL8                                                              
BUFBKLN  EQU   *-BUFBK             SIZE OF A BUCKET                             
         ORG   BUFBK                                                            
BDIRLST  DS    PL(BUFBKLN)         DIRECT COST YTD-1                            
BDIRYTD  DS    PL(BUFBKLN)         DIRECT COST YTD                              
BDIRLSTM DS    PL(BUFBKLN)         DIRECT COST YTD-1 MEMO                       
         ORG   BDIRLST                                                          
BINCLST  DS    PL(BUFBKLN)         INCOME YTD-1                                 
BINCYTD  DS    PL(BUFBKLN)         INCOME YTD                                   
         DS    PL(BUFBKLN)                                                      
         ORG   BDIRLST                                                          
BHRSLST  DS    PL(BUFBKLN)         HOURS YTD-1                                  
BHRSYTD  DS    PL(BUFBKLN)         HOURS YTD                                    
         DS    PL(BUFBKLN)                                                      
BBKCNT   EQU   (*-BUFBK)/BUFBKLN   NUMBER OF BUCKETS                            
BLEN     EQU   *-BUFD                                                           
         EJECT                                                                  
*              DSECT FOR THE BINSRCH LIST                                       
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISPK DS    F                   KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLEN  EQU   *-BIND                                                           
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
BINSIXB  EQU   X'40'               6 BYTE ACCUMS                                
BINLENQ  EQU   *-BIND                                                           
BINTABLE DS    0CL1                                                             
*                                                                               
*                                                                               
*              DSECT FOR MAINTAB FOR ACQUIRED STORAGE (GETMAIN)                 
MAIND    DSECT                                                                  
MAINEYE  DS    CL8                 TABLE EYE CATCHER IN DUMP                    
MAINAST  DS    AL2                 ADDR TO STORE A(TABLE)                       
MAINLEN  DS    AL2                 RECORD LENGTH                                
MAINDISK DS    AL2                 DISP/KEY LENGTH                              
MAINMAX  DS    AL2                 MAXIMUM NUMBER IN TABLE                      
MAINNUMB DS    AL1                 NUMBER OF BUCKETS                            
MAINFRST DS    AL1                 DISP TO FIRST BUCKET                         
MAINSTAT DS    AL1                 TABLE STATUS                                 
MAINSIZE DS    AL4                 TABLE SIZE                                   
MAINLNQ  EQU   *-MAIND                                                          
*                                                                               
*                                                                               
*              DSECT FOR ADDITIONAL WORK FROM ACQUIRED STORAGE(GETMAIN)         
WORKMD   DSECT                                                                  
WORKAST  DS    AL2                 ADDR TO STORE A(WORK AREA)                   
WORKSIZE DS    AL2                 WORK AREA SIZE                               
WORKLNQ  EQU   *-WORKMD                                                         
*                                                                               
*                                                                               
*              DSECT FOR ADD RECORD COUNTER                                     
CONTD    DSECT                                                                  
CONTSIZ  DS    H                   RECORD SIZE FOR COMPARISON GROUPING          
CONTER   DS    PL4                 ACTUAL COUNT OF RECORDS                      
CONTLNQ  EQU   *-CONTD                                                          
*                                                                               
*                                                                               
*              DSECT FOR ADD RECORD COUNTER PRINT LINE                          
CONTPD   DSECT                                                                  
CONTSIZP DS    CL11                RECORD SIZE FOR COMPARISON GROUPING          
CONTERP  DS    CL11                ACTUAL COUNT OF RECORDS                      
CONTPLNQ EQU   *-CONTPD                                                         
*                                                                               
*                                                                               
*              DSECT FOR SORTED POSTING FILE                                    
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTKACC  DS    0CL15                                                            
SRTCUL   DS    0CL3                COMPANY/UNIT/LEDGER                          
SRTCMPU  DS    0CL2                COMPANY/UNIT                                 
SRTCOMP  DS    CL1                 COMPANY                                      
SRTULAC  DS    0CL14               UNIT LEDGER/ACCOUNT                          
SRTUNIT  DS    CL1                 UNIT LEDGER                                  
SRTLGACC DS    0CL13               LEDGER ACCOUNT                               
SRTLEDG  DS    CL1                 LEDGER                                       
SRTACC   DS    CL12                ACCOUNT                                      
*                                                                               
SRTKCON  DS    0CL15               CONTRA                                       
SRTCCMPU DS    0CL2                COMPANY/UNIT                                 
SRTCCMP  DS    CL1                 COMPANY                                      
SRTCULAC DS    0CL14               UNIT LEDGER/ACCOUNT                          
SRTCUNT  DS    CL1                 UNIT                                         
SRTCLGAC DS    0CL13               LEDGER/ACCOUNT                               
SRTCLEDG DS    CL1                 UNIT LEDGER                                  
SRTCON   DS    CL12                CONTRA ACCOUNT                               
SRTBTYP  DS    0CL2                BUCKET TYPE (METHOD/SALARY TYPE)             
SRTMETHD DS    CL1                 METHOD OF ALLOCATION                         
SRTBTYPE DS    CL1                 TYPE OF DOLLARS (SALARY TYPE)                
*                                                                               
SRTKLEN  EQU   *-SRTD              LENGTH OF SORT RECORD KEY                    
SRTMOS   DS    0PL2                MOS  YYMM PACKED                             
SRTYEAR  DS    PL1                                                              
SRTMNTH  DS    PL1                                                              
SRTCNAME DS    CL36                CONTRA ACCOUNT NAME                          
SRTSTAT  DS    XL1                 STATUS BYTE                                  
SRTBUKS  DS    0PL8                                                             
SRTDR    DS    PL8                 DEBIT                                        
SRTCR    DS    PL8                 CREDIT                                       
SRTBKNUM EQU   (*-SRTBUKS)/L'SRTBUKS                                            
SRTLEN   EQU   *-SRTD              LENGTH OF SORT RECORD                        
*                                                                               
*                                                                               
*              DSECT FOR SUMMARY TABLES                                         
TABD     DSECT                                                                  
TABNME   DS    CL36                CATEGORY NAME                                
TABADD   DS    AL2                 DISP TO TABLE ADDRESS                        
TABBKS   DS    CL3                                                              
TABLEN   EQU   *-TABNME                                                         
*                                                                               
*                                                                               
*              DSECT FOR AN OFFICE/GROUP ENTRY                                  
OFFLD    DSECT                                                                  
OFFLCDE  DS    CL2                 OFFICE/GROUP CODE                            
OFFLLEN  DS    XL2                 LENGTH OF THIS ENTRY                         
OFFLNME  DS    CL36                OFFICE/GROUP NAME                            
OFFLNUM  DS    XL1                 NUMBER IN LIST (ZERO IF NOT GROUP)           
OFFLOFF  DS    0CL2                OFFICE CODE(S)                               
*                                                                               
*                                                                               
*              DSECT FOR  LIST INDEX TABLE                                      
LISTD    DSECT                                                                  
LISTCDE  DS    CL3                 LIST CODE                                    
LISTNME  DS    CL36                LIST NAME                                    
LISTIDX  DS    XL2                 LIST INDEX NUMBER                            
LISTLEN  EQU   *-LISTD                                                          
*                                                                               
*                                                                               
*                                                                               
*              DSECT FOR CLIENT LIST TABLE                                      
CLTLD    DSECT                                                                  
CLTLIDX  DS    XL2                 LIST INDEX NUMBER                            
CLTLCODE DS    CL12                1C ACCOUNT CODE                              
CLTLKLEN EQU   *-CLTLD                                                          
CLTLSTAT DS    XL1                 STATUS                                       
CLTLLEN  EQU   *-CLTLD                                                          
*                                                                               
*                                                                               
*              DSECT FOR ADDTYPTB TS INPUT TYPES TO LOCK                        
ADTYPD   DSECT                                                                  
ADTYCRTY DS    AL1                 COUNTRY                                      
ADTYLEN  DS    AL1                 LENGTH OF THIS ENTRY                         
ADTYNUM  DS    AL1                 NUMBER IN LIST                               
ADTYENT  DS    0XL1                INPUT TYPE(S)                                
         EJECT                                                                  
********* EQUATES ********************************                              
*                                                                               
*                                                                               
FOX      EQU   X'FF'               X'FF'                                        
DELELQ   EQU   X'FF'               X'FF' FOR ELEMENT DELETION                   
ALL      EQU   X'FF'               X'FF' USED IN "NI" TURN SWITCH OFF           
PASSDEL  EQU   X'08'               PASS BACK DELETED RECORDS (DATAMGR)          
SPACE    EQU   X'40'               SPACE                                        
HOURS    EQU   C'H'                H                                            
EMPLDGR  EQU   C'R'                R LEDGER EMPLOYEES                           
CLILDGR  EQU   C'C'                C LEDGER CLIENTS                             
NINE     EQU   C'9'                9                                            
ONE      EQU   C'1'                UNIT 1                                       
PLUS     EQU   C'+'                PLUS SIGN                                    
MINUS    EQU   C'-'                MINUS SIGN                                   
EOT      EQU   X'FF'               END OF TABLE                                 
EOF      EQU   X'80'               END OF FILE                                  
DELETED  EQU   X'80'               DELETE                                       
NOTFOUND EQU   X'01'                                                            
ONEBYTE  EQU   X'01'                                                            
TWOBYTE  EQU   X'02'                                                            
LOA      EQU   C'L'                LEAVE OF ABSENCE                             
CORPANAL EQU   C'C'                ANALYSIS=C ON 1N                             
OFFANAL  EQU   C'O'                ANALYSIS=O ON 1N                             
DPTANAL  EQU   C'D'                ANALYSIS=D ON 1N                             
PERANAL  EQU   C'P'                ANALYSIS=P ON 1N                             
GRPANAL  EQU   C'G'                ANALYSIS=G ON 1N                             
MAXLEN   EQU   12                  MAXIMUM LENGTH OF AN ACCOUNT                 
ZERO     EQU   0                   BINARY ZERO TO CLEAR A BYTE                  
RECLNMAX EQU   2000                MAX LENGTH OF A RECORD TODAY                 
LENBUFF  EQU   CLISIZE+SUMSIZE+INSIZE+OVSIZE+BCSIZE+HYSIZE+NEWBSIZE+CLIX        
               LSIZE+NONSIZE+ERRSIZE+BLEN+CLEN2+(2*OVLEN)+INLEN+PTOTLENX        
               +RATLEN+(2*SRTLEN)+SLRLEN+YTDLEN+BUFSPACE+RTSIZE+CLISIZ2X        
               +YTD2SIZE+SCHMSIZE+OFFTLEN+MTHRLEN+(6*SLEN)+DEPLEN+OFFLEX        
               N+AGYLEN+LSTLEN+100                                              
*                                                                               
         EJECT                                                                  
*        ACCSTRCVD                                                              
*        ACGENFILE                                                              
*        CTGENFILE                                                              
*        ACGENBOTH                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        DDBUFFALOD                                                             
*        DDREPXTRAD                                                             
*        DDLOGOD                                                                
*        DDREMOTED                                                              
*        DDMASTD                                                                
*        DDSLRD                                                                 
*        ACSALHSTD                                                              
*        DDTSARD                                                                
*        ACDDEQUS                                                               
*        DDCTRYEQUS                                                             
*        DDLANGEQUS                                                             
COBLOCKD DSECT                                                                  
       ++INCLUDE ACCAPBLOCK                                                     
*                                                                               
TRNBLKD  DSECT                                                                  
       ++INCLUDE ACADDTRND                                                      
*                                                                               
       ++INCLUDE ACCSTRCVD                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDSLRD                                                         
       ++INCLUDE ACSALHSTD                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE DDLANGEQUS                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
*PREFIX=B  =DDBUFFD               *DDBUFFD CONTAINS THE SAME VARIABLES          
       ++INCLUDE DDBUFFD          *AS DDBUFFALOD SO NEED THIS PREFIX            
*PREFIX=                                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACREPCA02 03/21/12'                                      
         END                                                                    
