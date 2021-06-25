*          DATA SET ACREPNB02  AT LEVEL 052 AS OF 10/21/15                      
*PHASE ACNB02C                                                                  
*INCLUDE ACDITTO                                                                
*INCLUDE ACGETTXT                                                               
*INCLUDE ACJAX                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE ACTRACK                                                                
*INCLUDE BUFFERIN                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
ACNB02   TITLE ' - Billing controller'                                          
         PRINT NOGEN                                                            
***********************************************************************         
* RUN=TEST(MCTSTRUN=X'FF'):                                           *         
*               DON'T CALL UPDATE                                     *         
*                                                                     *         
* PARM= T        TEST RUN - KEEP WORKER FILE                          *         
*   +1   P       PRINT WORKER FILE                                    *         
*   +1   W       PRINT & PURGE THE WORKER FILE                        *         
*   +2    Y      CHECK FOR PATCH CARDS                                *         
*                                                                     *         
* PARM=T - TEST RUN - KEEP POSTING & FACWK FILES.                     *         
***********************************************************************         
                                                                                
ACNB02   CSECT ,                                                                
         NMOD1 0,**NB02                                                         
         DROP  RB                                                               
         L     RC,0(R1)                                                         
         USING ACWORKD,RC                                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LARL  R8,NBILC                                                         
         USING NBILC,R8            R8=A(BILLING CSECT)                          
         LARL  R9,JXBLKC                                                        
         USING JXBLK,R9            R9=A(JXBLK)                                  
         ST    R9,AJXBLK                                                        
                                                                                
         MVI   FCRESET,YES                                                      
                                                                                
         CLI   MODE,RUNFRST                                                     
         JE    RNF00                                                            
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JNO   NB03                YES, PRINT IO COUNT AT ENTER                 
         GOTOR AIOCNTR,DMCB,MODE                                                
                                                                                
NB03     CLI   MODE,LEDGFRST                                                    
         JE    LDG00                                                            
         CLI   MODE,REQFRST                                                     
         JE    RQF00                                                            
         CLI   MODE,LEVAFRST                                                    
         JE    LVAF00                                                           
         CLI   MODE,LEVBFRST                                                    
         JE    LVBF00                                                           
         CLI   MODE,PROCACC                                                     
         JE    PA00                                                             
         CLI   MODE,PROCTRNS                                                    
         JE    PTN00                                                            
         CLI   MODE,ACCLAST                                                     
         JNE   NB05                                                             
         GOTOR AL00,DMCB,ACWORKD                                                
         J     XIT                                                              
                                                                                
NB05     CLI   MODE,LEVALAST                                                    
         JE    LVAL00                                                           
         CLI   MODE,LEVBLAST                                                    
         JE    LVBL00                                                           
         CLI   MODE,LEDGLAST                                                    
         JE    LDGL00                                                           
         CLI   MODE,REQLAST                                                     
         JE    RQL00                                                            
         CLI   MODE,RUNLAST                                                     
         JNE   XIT                                                              
         GOTOR RNL                                                              
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
                                                                                
RNF00    ST    RC,BASERC                                                        
         MVI   MAXLINES,54                                                      
         MVI   RCREQREP,NO         DEFAULT NO REQUEST DETAILS                   
         MVI   RUNOPT,0                                                         
         MVI   WKFOPT,0                                                         
         MVI   SOONS,0                                                          
         XC    LUID,LUID                                                        
                                                                                
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   VATICAN,ACMAVTCN    VATICAN                                      
         L     RF,ADCOMFAC                                                      
         USING COMFACSD,RF                                                      
         MVC   HELLO,CHELLO                                                     
         MVC   HELEN,CHELEN                                                     
         MVC   HEXIN,CHEXIN                                                     
         DROP  RF                                                               
                                                                                
         GOTOR LOAD                FIRST TIME - LOAD PHASES                     
                                                                                
         CLI   RCFFPARM,C' '                                                    
         JNH   RNF01                                                            
         CLI   RCFFPARM,C'T'       TEST RUN                                     
         JE    *+6                                                              
         DC    H'0'                UNKNOWN PARM                                 
         OI    WKFOPT,WKFKEEP      KEEP POSTING FILE                            
                                                                                
RNF01    CLI   RCFFPARM+1,C'P'                                                  
         JNE   *+8                                                              
         OI    WKFOPT,WKFPRTW      PRINT WORKER FILE                            
         CLI   RCFFPARM+1,C'W'                                                  
         JNE   *+8                                                              
         OI    WKFOPT,WKFPRTW+WKFPURW   PRINT & PURGE WORKER FILE               
                                                                                
         CLI   RCFFPARM+2,YES      TEST FOR PATCH CARDS                         
         JNE   RNF02                                                            
         GOTOR PATCH                                                            
                                                                                
RNF02    LARL  R5,TABTAB           A(TABLE DEFINITIONS)                         
         SR    R1,R1               LOW CORE REQUIREMENTS                        
         SR    R2,R2               HIGH CORE REQUIREMENTS                       
                                                                                
RNF03    TM    12(R5),X'80'        TEST HIGH CORE                               
         JO    *+12                                                             
         A     R1,8(R5)            ADD TO LOW CORE                              
         J     *+8                                                              
         A     R2,8(R5)            ADD TO HIGH CORE                             
         LA    R5,L'TABTAB(R5)                                                  
         CLI   0(R5),EOT           END OF TABLE DEFINITIONS                     
         JNE   RNF03                                                            
         ST    R1,LOWLEN           SAVE CORE REQUIREMENTS                       
         ST    R2,HILEN                                                         
                                                                                
         BASR  RB,0                                                             
         USING *,RB                ADDRESSABILITY FOR IBM MACROS                
         L     R0,LOWLEN                                                        
         GETMAIN R,LV=(0)          GET SOME STORAGE                             
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                NOT ENOUGH STORAGE AVAILABLE                 
         ST    R1,LOWBGN           SAVE START OF TABLE                          
         MVI   BYTE,0              ALLOCATE LOW                                 
         GOTOR TABINI              INITIALIZE 24 BIT STORAGE AREAS              
                                                                                
         MVI   BYTE,X'80'          ALLOCATE HIGH                                
         L     R0,HILEN                                                         
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF               TEST IF STORAGE ACQUIRED                     
         JZ    *+6                 YES                                          
         DC    H'0'                                                             
         ST    R1,HIBGN            SAVE START OF TABLE                          
         GOTOR TABINI              INITIALIZE 31 BIT STORAGE AREAS              
         DROP  RB                                                               
                                                                                
         GOTOR DATCON,DMCB,(4,RCDATE),(X'20',TODAY0)                            
         GOTOR (RF),(R1),,(1,TODAY1)                                            
         GOTOR (RF),(R1),,(2,TODAY2)                                            
         GOTOR (RF),(R1),,(3,TODAY3)                                            
         GOTOR (RF),(R1),,(8,TODAY8)                                            
                                                                                
         USING MASTD,R5                                                         
         L     R5,ADMASTC                                                       
         CLI   MCTSTRUN,X'FF'                                                   
         JNE   *+8                                                              
         OI    RUNOPT,RUNTST                                                    
         MVC   UPSI,MCUPSI            TEST/TRACE OPTIONS                        
         MVC   MCUSRDMP(4),PHSEBGN    DUMP PHASES                               
         MVC   MCUSRDMP+4(4),PHSEEND                                            
         MVC   USERID,MCORIGID        SAVE USER-ID                              
         ICM   R0,15,LOWBGN           DUMP THE LOW CORE TABLES                  
         STCM  R0,15,MCUSRDMP+8                                                 
         A     R0,LOWLEN                                                        
         STCM  R0,15,MCUSRDMP+12                                                
         MVC   CTRYCOD,MCAGCTRY    COUNTRY CODE                                 
         CLI   MCRECOVR,C'W'       TEST SOON RUN - TSO                          
         JNE   *+8                 NO,                                          
         OI    SOONS,SOONRUN                                                    
         OC    MCREMPQK,MCREMPQK   TEST REAL SOON                               
         JZ    *+8                 NO,                                          
         OI    SOONS,SOONRUN                                                    
         DROP  R5                                                               
                                                                                
         XC    CPYC,CPYC                                                        
         L     R5,ADCMPEL          SAVE COMPANY ELEMENT                         
         USING CPYELD,R5                                                        
         LLC   R1,CPYLN                                                         
         BCTR  R1,0                                                             
         EXRL  R1,RNFMVCCC                                                      
                                                                                
         L     R4,LOGOC                                                         
         USING LOGOD,R4                                                         
         MVI   LOGOEND,C'X'                                                     
         CLC   LOGO1,SPACES                                                     
         JNE   *+10                                                             
         MVC   LOGO1,CPYLOGO                                                    
         GOTOR LOGO,DMCB,LOGOD                                                  
         DROP  R4                                                               
                                                                                
         CLI   CTRYCOD,0           IF NO COUNTRY SPECIFIED                      
         JNE   *+8                                                              
         MVI   CTRYCOD,CTRYUSA     MAKE IT USA                                  
         MVC   CURCOD,CPYCURR      SET CURRENCY CODE                            
         OC    CURCOD,CURCOD                                                    
         JNZ   RNF032                                                           
         GOTOR GETCUR                                                           
         DROP  R5                                                               
                                                                                
RNF032   LARL  R5,PGMTAB                                                        
RNF04    CLC   0(L'PGMCDE,R5),RCPROG                                            
         JE    RNF05                                                            
         LA    R5,PGMLNQ(R5)                                                    
         CLI   0(R5),EOT                                                        
         JNE   RNF04                                                            
         DC    H'0'                                                             
RNF05    MVC   PGMCDE(PGMLNQ),0(R5)                                             
         CLI   RCRERUN,YES                                                      
         JNE   *+8                                                              
         OI    PGMSTA,PGMRRB       SET RE-RUN SWITCH                            
         TM    PGMSTA,PGMDFT       TEST DRAFT BILLING                           
         JNO   *+12                NO,                                          
         MVI   RCPOSTNG,NO         YES, NO POSTINGS                             
         MVI   RCWRITE,NO                                                       
                                                                                
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         LARL  R0,DICINP                                                        
         L     R1,ADICO                                                         
         LA    R2,ACMAUCDI         UPPER CASE FOR 21                            
         TM    PGMAQ,PGMALL21                                                   
         JNZ   *+12                                                             
         LA    R2,ACMALCDI         LOWER FOR 27                                 
         MVI   RCFLAG1,RCFREPLC    SET SPECS FOR LOWER CASE                     
         STM   R0,R1,0(R2)                                                      
         DROP  RF                                                               
                                                                                
         GOTOR APOST               OPEN POSTING FILE                            
                                                                                
         GOTOR ASORT,SORTINIQ      INITIALIZE SORT                              
         MVI   RCREQREP,NO         NO REQUEST DETAILS                           
         GOTOR AADDREP,MEDSUM      ADD MEDIA SUMMARY RECORDS                    
                                                                                
         USING MASTD,RF                                                         
         L     RF,ADMASTC                                                       
                                                                                
         USING UTLD,RE                                                          
         ICM   RE,15,MCUTL         A(UTL)                                       
         MVC   SE#,TSYS                                                         
         DROP  RE,RF                                                            
                                                                                
         USING CTWREC,R2                                                        
         MVC   SENAME,=C'ACC??'    SYSTEM NAME                                  
         LA    R2,CKEY                                                          
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ    SYSTEM LIST RECORD                           
         MVI   CTWKREC,CTWKRSYS    C'S'                                         
         MVI   CTWKSYSN,CTWKACC                                                 
         MVC   AIO,AIO1                                                         
         GOTOR ADMGR,CONHIQ                                                     
         L     R2,AIO                                                           
         CLC   CTWKEY,CKEY                                                      
         JNE   RNF08               THAT NOT TO GOOD                             
         LA    R2,CTWDATA                                                       
                                                                                
         USING SYSELD,R2                                                        
RNF06    CLI   0(R2),0             END OF RECORD?                               
         JE    RNF08                                                            
         CLI   0(R2),SYSELQ                                                     
         JNE   RNF07                                                            
         CLC   SYSSEN,SE#          MATCH ON SE NUMBER                           
         JNE   RNF07                                                            
         MVC   SENAME,SYSNAME      SYSTEM NAME                                  
         J     RNF08                                                            
                                                                                
RNF07    LLC   RE,1(,R2)                                                        
         AR    R2,RE                                                            
         J     RNF06                                                            
                                                                                
         USING CPYELD,R5                                                        
RNFMVCCC MVC   CPYC(0),CPYEL                                                    
         DROP  R5                                                               
                                                                                
         USING CTPREC,R2                                                        
RNF08    LA    R2,CKEY                                                          
         XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,CTPKTYPQ    PROFILE RECORD "P"                           
         MVI   CTPKSYS,C'A'        SYSTEM=ACC                                   
         MVC   CTPKPROG,QPROG                                                   
         MVC   CTPKORIG,ORIGINUM                                                
         MVC   AIO,AIO1                                                         
         GOTOR ADMGR,CONHIQ                                                     
         L     R2,AIO                                                           
         CLC   CTPKEY,CKEY                                                      
         JNE   RNF15                                                            
         LA    R2,CTPDATA                                                       
         USING CTARTD,R2                                                        
                                                                                
RNF09    CLI   0(R2),0                                                          
         JE    RNF15                                                            
         CLI   0(R2),CTARTELQ                                                   
         JE    RNF11                                                            
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         J     RNF09               NEXT ELEMENT                                 
                                                                                
RNF11    CLI   CTARTCOD,C'A'                                                    
         JNE   RNF15                                                            
         OI    RUNOPT,RUNARCH      SET ARCHIVE IN USE                           
                                                                                
RNF15    XC    MEDSTA,MEDSTA                                                    
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
         USING MASTD,R5                                                         
RQF00    L     R5,ADMASTC                                                       
         MVC   USERPID,MCRHPSWD       SAVE PID                                  
         L     RF,ADICO            TEST DICTIONARY RESOLVED                     
         OC    0(4,RF),0(RF)       WILL NOT BE IF TRACE=YES                     
         JNZ   RQF01               YES,                                         
         LARL  R0,DICINP                                                        
         GOTOR ADDICTAT,DMCB,C'LU  ',(R0),ADICO                                 
                                                                                
RQF01    MVC   DRAFT,SPACES        SET *DRAFT*                                  
         MVI   DRAFT,C'*'                                                       
         L     RF,ADICO                                                         
         USING DICD,RF                                                          
         MVC   DRAFT+1(5),DD@DRAFT                                              
         MVI   DRAFT+6,C'*'                                                     
         DROP  RF                                                               
                                                                                
         MVI   RCSUBPRG,0                                                       
         MVC   CPY,RCCOMPFL                                                     
         MVC   UL,QUNIT                                                         
         MVC   CPJ,QACCOUNT                                                     
         XC    SVRCDATE,SVRCDATE                                                
         CLC   QUESTOR(2),=C'R='    RUN DATE OVERRIDE                           
         JNE   RQF02                                                            
         GOTOR VDATVAL,DMCB,(0,QUESTOR+2),WORK                                  
         OC    0(4,R1),0(R1)                                                    
         JZ    RQF02               INVALID DATE, IGNORE                         
         MVC   SVRCDATE,RCDATE                                                  
         GOTOR DATCON,DMCB,(0,WORK),(10,RCDATE)                                 
         GOTOR (RF),(R1),(4,RCDATE),(X'20',TODAY0)                              
         GOTOR (RF),(R1),,(1,TODAY1)                                            
         GOTOR (RF),(R1),,(2,TODAY2)                                            
         GOTOR (RF),(R1),,(3,TODAY3)                                            
         GOTOR (RF),(R1),,(8,TODAY8)                                            
                                                                                
RQF02    MVI   REQOPT,0                                                         
         MVI   RQLV,0                                                           
         MVI   PGLV,0                                                           
         GOTOR GETPRF              SET REQUEST OPTIONS                          
         CLI   OPTGBF,C' '         TEST GROUP BILL FORMAT                       
         JNH   *+8                                                              
         OI    REQOPT,REQGRP       SET GROUP BILL REQUEST                       
                                                                                
         TM    RUNOPT,RUNREQD      TEST ALREADY DID THIS                        
         JO    RQF04                                                            
         TM    PGMSTA,PGMLIV       TEST LIVE                                    
         JO    RQF04                                                            
         TM    OPT2,OPT2PRQD       TEST WANT REQUEST DETAILS                    
         JNO   RQF04                                                            
         OI    RUNOPT,RUNREQD      SET ALREADY DID THIS                         
         XR    R0,R0                                                            
         L     R1,ACSPECS          R1=A(SPEC POOL)                              
         IC    R0,1(R1)            FIND END OF SPECS                            
         AR    R1,R0                                                            
         CLI   0(R1),0             TEST END                                     
         JNE   *-10                                                             
                                                                                
         XR    RF,RF                                                            
         LA    R2,1(R1)            SHIFT END OF SPECS UP 1                      
RQF03    IC    RF,1(R2)                                                         
         BCTR  RF,0                                                             
         EXRL  RF,RQFMVC12                                                      
         LA    R1,1(RF,R1)                                                      
         LA    R2,1(RF,R2)                                                      
         CLI   0(R2),0             TEST "NEW" END                               
         JNE   RQF03               YES, ALL DONE                                
         MVI   0(R1),0                                                          
                                                                                
RQF04    CLC   QUESTOR(7),=CL7'XIX'                                             
         JNE   *+8                                                              
         OI    REQOPT,REQNBJ       NON-BILLABLE JOBS                            
         MVI   FCJOBBER,0          KILL JOBBER CALL IN MONACC                   
         ZAP   DUB,RCRQTOT                                                      
         CVB   RF,DUB                                                           
         STCM  RF,3,REQNUM         SAVE REQUEST NUMBER                          
                                                                                
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   JXADDMST,ADMASTC    MASTD                                        
         MVC   JXAACMST,AMONACC    ACMD                                         
         MVC   JXAPRNTB,PRNTBL     PRINTABER                                    
         MVC   JXACOMF,ADCOMFAC    COMFACS                                      
         MVC   JXAGETOP,GETOPT     GETOPT                                       
         MVC   JXAJOBR,ACMAJOBR    JOBBER                                       
         MVC   JXAJOBC,ACMAJOBL    JOB COL                                      
                                                                                
         MVC   JXAGOBK,ADGOBLOC    GOBLOCK                                      
         MVC   JXACOLS,ACMACOLL    COLUMN LIST AREA                             
         MVC   JXACOLTB,ACMACOL    COLUMN OUTPUT TABLE                          
         MVC   JXLCOLTB,ACMLCOL    LENGTH OF COLUMN OUTPUT TABLE                
         MVC   JXAOPVTB,ACMAOPV    OPERAND VALUE TABLE                          
         MVC   JXLOPVTB,ACMLOPV    LENGTH OF OPERAND VALUE TABLE                
         MVC   JXTODAY0,TODAY0                                                  
         MVC   JXMOA,QAPPL+6       CYCLE MOA                                    
         DROP  RF                                                               
                                                                                
         MVC   JXAEWC,AESWC        ESTIMATE WORKCODES                           
         LHI   RF,ESWCLNQ                                                       
         STCM  RF,15,JXLEWC                                                     
         MVC   JXARETLB,ARTLRB     RETAIL BUFFER                                
         LHI   R1,RTLBLNQ                                                       
         ST    R1,JXLRETLB         LENGTH OF BUFFER                             
         MVC   JXAUSRF,AUSFLD      A(USER FIELD TABLE)                          
         LA    R1,USRBLNQ                                                       
         ST    R1,JXLUSRF          LENGTH OF TABLE                              
                                                                                
         MVI   JXXRSTA,JXXRSEX     ONLY FLAG BILLING PROBLEMS                   
         MVC   JXWCFLT,QTRNSFLT    WORKCODE FILTER                              
                                                                                
         NI    JXOPTS2,X'FF'-OPTADTBL+OPTARJOB                                  
         TM    OPTA,OPTADTBL       TEST DEMAND TOTAL BILL(OPT3=Y)               
         JZ    *+8                                                              
         OI    JXOPTS2,JXODTBL                                                  
         TM    OPTA,OPTARJOB       TEST DEMAND TOTAL BILL RETAIL 'R'            
         JZ    *+8                                                              
         OI    JXOPTS2,JXORJOB                                                  
                                                                                
         MVC   BILDT0,TODAY0       BILL DATE IS TODAY (YYMMDD)                  
         CLC   QSTART,SPACES                                                    
         JE    *+10                                                             
         MVC   BILDT0,QSTART       UNLESS THEY SPECIFY                          
         GOTOR DATCON,DMCB,(0,BILDT0),(1,BILDT1)                                
         GOTOR (RF),(R1),,(2,BILDT2)                                            
         MVC   BILMOSP,BILDT1                                                   
         MVC   BILMOSC(1),BILDT0+1 MOS - YEAR                                   
         LLC   R0,BILDT0+3         - MONTH                                      
         CLI   BILDT0+2,C'0'       0 THRU 9 ARE OK                              
         JE    *+8                                                              
         SHI   R0,47               10=A, 11=B, 12=C                             
         STC   R0,BILMOSC+1                                                     
                                                                                
         MVC   TRNSFLT,QTRNSFLT    SAVE FILTER                                  
         MVI   ENDATE1,X'FF'                                                    
         MVI   ENDATE2,X'FF'                                                    
         MVI   ORGBDTE,X'FF'                                                    
         CLC   QEND,SPACES                                                      
         JE    RQF07                                                            
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JO    RQF05                                                            
         GOTOR DATCON,DMCB,(0,QEND),(1,ENDATE1)                                 
         GOTOR (RF),(R1),,(2,ENDATE2)                                           
         J     RQF07                                                            
                                                                                
RQFMVC12 MVC   0(0,R1),0(R2)                                                    
                                                                                
RQF05    GOTOR DATCON,DMCB,(0,QEND),(1,ORGBDTE)                                 
                                                                                
RQF07    TM    PGMAQ,PGMALL27      TEST 27 BILLING                              
         JZ    RQFX                NO,                                          
         L     R3,ADQSTACK                                                      
         USING ACQD,R3                                                          
         LA    R4,ACQTYP1                                                       
         USING ACQTYP1,R4                                                       
         MVC   TTLSDAY2,BILDT2     DEFAULT IS BILL DATE                         
         MVC   TTLEDAY2,BILDT2                                                  
         XC    TTLEDAY8,TTLEDAY8                                                
         CLC   ACQDTSTR,SPACES     NO START, USE TODAY                          
         JE    RQF09                                                            
         GOTOR DATCON,DMCB,(0,ACQDTSTR),(2,TTLSDAY2)                            
                                                                                
RQF09    GOTOR DATCON,DMCB,(2,TTLSDAY2),(8,TTLSDAY8)                            
         CLC   ACQDTEND,SPACES                                                  
         JE    RQFX                                                             
         GOTOR DATCON,DMCB,(0,ACQDTEND),(2,TTLEDAY2)                            
         GOTOR DATCON,DMCB,(2,TTLEDAY2),(8,TTLEDAY8)                            
                                                                                
RQFX     TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
                                                                                
LDG00    GOTOR AWRKM,WRKMBDQ       BUILD WORKCODE TABLE                         
         TM    PGMAQ,PGMALL27      TEST A27                                     
         JZ    LDG03                                                            
         GOTOR TSTLOCK,DMCB,('LOCKLDG',ADLEDGER) TEST LEDGER LOCKED             
         JE    LDG03                                                            
         DC    H'0'                LOCK PROBLEM                                 
                                                                                
LDG03    XC    ALDGPMD,ALDGPMD     GET LEDGER LEVEL MEDIA ELEMENT               
         MVI   ELCODE,PMDELQ                                                    
         L     R2,ADLEDGER                                                      
         GOTOR GETEL                                                            
         JNE   *+8                                                              
         ST    R2,ALDGPMD                                                       
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LEVEL A FIRST                                                       *         
***********************************************************************         
                                                                                
LVAF00   GOTOR ASETEL,SETLINI      Initialize element addresses                 
         MVC   AIO,ADHEIRA                                                      
         GOTOR ASETEL,SETLCLQ      Set client element address                   
         TM    PGMAQ,PGMALL27      Test A27                                     
         JZ    LVAF01                                                           
         GOTOR TSTLOCK,DMCB,('LOCKCLI',ADHEIRA) Test client locked              
         JE    LVAF01                                                           
         DC    H'0'                Lock problem                                 
                                                                                
LVAF01   LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         GOTOR CLRBUK,LVABK        Clear level a buckets                        
         DROP  R7                                                               
                                                                                
         XC    NBTRN,NBTRN         Clear billable item count                    
         XC    NPRVB,NPRVB                                                      
         XC    FPRVB,FPRVB         Previous bills                               
                                                                                
         MVI   AGLV,0              Group activity switch                        
         MVI   GBRC,0              Group bill record control                    
         MVC   PRENAME,SPACES                                                   
         MVI   GRPANLN,0           Init. # of group bill analysis acct          
         MVI   FORMCDE,C'0'        Defaults for 21                              
         MVI   GRPLCDE,C'P'                                                     
                                                                                
         GOTOR GETPRF              Set profile options                          
                                                                                
         TM    PGMAQ,PGMA21+PGMA22+PGMA23+PGMA24                                
         JZ    LVAF02                                                           
         CLI   QAPPL+4,C' '        Test request format code override            
         JNH   LVAF7                                                            
         MVC   FORMCDE,QAPPL+4     Set format code from request                 
         J     LVAF7                                                            
                                                                                
LVAF02   TM    PGMAQ,PGMALL27      Test 27 billing                              
         JZ    LVAF7               No,                                          
         MVC   FORMCDE,OPTFRMT     Get format number from profile               
         CLI   QAPPL+4,C' '                                                     
         JNH   *+10                                                             
         MVC   FORMCDE,QAPPL+4     Unless request override                      
                                                                                
         MVC   GRPLCDE,OPTGLVL     Get group level option                       
         CLI   QAPPL+5,C' '                                                     
         JNH   *+10                                                             
         MVC   GRPLCDE,QAPPL+5     Save level code                              
         MVI   RQLV,LVC            Assume job level                             
         CLI   GRPLCDE,C'P'                                                     
         JNE   *+8                                                              
         MVI   RQLV,LVB            Product level                                
         CLI   GRPLCDE,C'C'                                                     
         JNE   *+8                                                              
         MVI   RQLV,LVA            Client level                                 
         TM    RQLV,LVA+LVB        Test A27 "group" bill                        
         JZ    *+8                                                              
         OI    REQOPT,REQGRP       Set group flag                               
                                                                                
         LA    RE,FORMCTAB                                                      
LVAF03   CLC   FORMCDE,0(RE)       Match format code                            
         JE    LVAF05                                                           
         LA    RE,L'FORMCTAB(RE)                                                
         CLI   0(RE),EOT                                                        
         JNE   LVAF03                                                           
         J     ERMBGR              Must be group request                        
                                                                                
LVAF05   MVC   FORMLVL,1(RE)                                                    
         MVC   FORMTRN,2(RE)                                                    
         LLC   RF,RQLV             Test valid request level                     
         EXRL  RF,LVAFTMF0                                                      
         JNO   ERMBGR                                                           
                                                                                
         TM    FORMTRN,LVR         Test process transactions a req lvl          
         JNO   *+10                                                             
         MVC   FORMTRN,RQLV        Set level to process transactions            
                                                                                
LVAF7    CLI   CTRYCOD,CTRYCAN     Test Canada                                  
         JNE   LVAF9                                                            
         GOTOR CANTX,CANINIQ       Initialize Canadian tax                      
                                                                                
LVAF9    TM    UPSI,UPDMG          Trace DM calls                               
         JO    IOXIT               Yes, print io count at exit                  
         J     XIT                                                              
                                                                                
LVAFTMF0 TM    FORMLVL,0                                                        
         EJECT                                                                  
***********************************************************************         
* LEVEL B FIRST                                                       *         
***********************************************************************         
                                                                                
LVBF00   TM    REQOPT,REQERR       TEST ERROR                                   
         JO    XIT                                                              
                                                                                
         MVC   AIO,ADHEIRB                                                      
         GOTOR ASETEL,SETLPRQ      SET PRODUCT ELEMENT ADDRESSES                
                                                                                
         LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         GOTOR CLRBUK,LVBBK        CLEAR LEVEL B BUCKETS                        
         DROP  R7                                                               
                                                                                
         NI    AGLV,ALL-(LVB+LVC)                                               
         NI    GBRC,ALL-(GBRCB+GBRCC)                                           
         MVC   PRENAME,SPACES                                                   
         TM    FORMTRN,LVA         PROCESS TRANSACTIONS AT LEVEL A              
         JO    LVBF3               YES,                                         
         MVI   GTIND,0                                                          
         XC    NBTRN,NBTRN         CLEAR BILLABLE ITEM COUNT                    
         XC    NPRVB,NPRVB                                                      
         XC    FPRVB,FPRVB         PREVIOUS BILLS                               
                                                                                
LVBF3    TM    PGMAQ,PGMALL21      TEST 21 BILLING                              
         JNZ   LVBF5               YES, INIT CANADIAON TAX                      
         TM    RQLV,LVA            TEST REQUEST LEVEL (A27)                     
         JO    LVBF7               YES, DON'T INITIALIZE NOW                    
                                                                                
LVBF5    MVI   GRPANLN,0           INIT. # OF GROUP BILL ANALYSIS ACCT          
         CLI   CTRYCOD,CTRYCAN     TEST CANADA                                  
         JNE   LVBF7               YES, DON'T INITIALIZE NOW                    
         GOTOR CANTX,CANINIQ       INITIALIZE CANADIAN TAX                      
                                                                                
LVBF7    TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS JOB ACCOUNT                                                 *         
***********************************************************************         
                                                                                
PA00     L     RF,ADACC                                                         
         MVC   JOBCDE,0(RF)        SAVE CLIENT PRODUCT JOB                      
                                                                                
         TM    REQOPT,REQERR       TEST ERROR                                   
         JO    XIT                                                              
         TM    PGMAQ,PGMALL21      TEST A21                                     
         JZ    PA01                                                             
         GOTOR TSTLOCK,DMCB,('LOCKACC',ADACC) TEST ACCOUNT LOCKED               
         JE    PA01                                                             
         DC    H'0'                LOCK PROBLEM                                 
                                                                                
PA01     GOTOR GETPRF              SET PROFILE OPTIONS                          
                                                                                
         LA    R1,BK$              CLEAR ACCUMS                                 
         LA    R0,NALLBK                                                        
         ZAP   0(L'BK$,R1),PZERO                                                
         LA    R1,L'BK$(R1)                                                     
         JCT   R0,*-10                                                          
                                                                                
         LA    R1,AMTS                                                          
         LA    R1,JOCUR-AMTD(R1)                                                
         LA    R0,NACC$                                                         
         ZAP   0(L'BK$,R1),PZERO                                                
         LA    R1,L'BK$(R1)                                                     
         JCT   R0,*-10                                                          
                                                                                
         NI    AGLV,ALL-(LVC)                                                   
         NI    GBRC,ALL-(GBRCC)                                                 
         NI    BLNSTA,ALL-(BLNSNXT)                                             
         MVC   PRENAME,SPACES                                                   
                                                                                
         ZAP   SK2SICOM,PZERO      SK TO SI ACCUMS - COMMISSION                 
         ZAP   SK2SIGRS,PZERO      AND GROSS                                    
         ZAP   POBAMT,PZERO        PERCENT OF BILL AMOUNT                       
         ZAP   POBPRV,PZERO                                                     
         ZAP   POBCUR,PZERO                                                     
         ZAP   OOP,PZERO                                                        
         ZAP   PSF,PZERO                                                        
                                                                                
         XC    INTBLK,INTBLK       CLEAR INTERNAL INCOME BLOCK                  
         MVI   PTYPE,0                   POSTING TYPE                           
         XC    WRKCWC,WRKCWC             WORKCODE                               
         XC    CRTLR,CRTLR               CURRENT RETAILER                       
                                                                                
         L     R2,ADPROFIL                                                      
         MVC   OFC,PPRGAOFF-PPRELD(R2) SET OFFICE                               
         MVC   BOFC,OFC                                                         
         MVC   RECVAC,PPRRECV-PPRELD(R2) SET RECEIVABLE                         
         TM    RQLV,LVA            TEST 27 LEVEL A                              
         JNO   PA02                                                             
         L     R2,ADLVASUP                                                      
         MVC   BOFC,PPRGAOFF-PPRELD(R2)  SAVE BILLING OFFICE                    
         MVC   RECVAC,PPRRECV-PPRELD(R2) SET RECEIVABLE                         
                                                                                
PA02     CLI   CTRYCOD,CTRYCAN     TEST CANADA                                  
         JNE   PA03                                                             
         TM    REQOPT,REQGRP       TEST GROUP IN PROGRESS                       
         JO    *+8                 YES, DON'T INITIALIZE SWITCH                 
         MVI   CTAXOPT,0                                                        
         OI    CTAXOPT,CTAXOGST+CTAXOPST                                        
         GOTOR CANTX,CANINIQ       INITIALIZE CANADIAN TAX                      
                                                                                
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JO    PA03                YES, DEPENDS ON ORIGINAL BILL                
         TM    OPTA,OPTASGST       SUPPRESS BOTH TAXES?                         
         JNO   *+8                 NO,                                          
         NI    CTAXOPT,ALL-(+CTAXOGST+CTAXOPST)                                 
         TM    OPTA,OPTASPST       SUPPRESS PST ONLY                            
         JNO   PA03                NO,                                          
         NI    CTAXOPT,ALL-(CTAXOPST)                                           
                                                                                
PA03     L     RF,ADACC                                                         
         ST    RF,AIO                                                           
         GOTOR ASETEL,SETLJBQ      SET ELEMENT ADDRESS FOR JOB                  
         GOTOR PARCPJ              PARSE CLIENT, PRODUCT, JOB                   
                                                                                
         MVC   BILOTHR,SPACES      SET 'OTHER NUMBER'                           
         ICM   RF,15,AOTHEL                                                     
         JZ    *+10                                                             
         MVC   BILOTHR,OTHNUM-OTHELD(RF)                                        
                                                                                
         MVI   ELCODE,PMDELQ       GET MEDIA ELEMENT                            
         L     R2,ADCOMP                                                        
         XR    R0,R0                                                            
         GOTOR GETEL                                                            
         J     PA0502                                                           
                                                                                
PA05     GOTOR NEXTEL                                                           
PA0502   JE    *+6                                                              
         DC    H'0'                NO, MEDIA ELEMENT                            
         USING PMDELD,R2                                                        
         CLC   PMDCODE,MEDIA                                                    
         JE    PA07                                                             
         AHI   R0,1                GET RELATIVE ELEMENT NUMBER                  
         CHI   R0,MXMED                                                         
         JNH   PA05                                                             
         DC    H'0'                MEDSTA IS TOO SMALL                          
                                                                                
PA07     ST    R2,APMDEL           R2=A(MEDIA ELEMENT)                          
         STC   R0,APMDEL           R0=RELATIVE NUMBER                           
         MVC   MEDN,PMDSPACE       SAVE MEDIA NAME                              
                                                                                
         XC    ACLINUM,ACLINUM     GET NUMBER ELEMENT FOR THIS MEDIA            
         MVI   ELCODE,NUMELQ                                                    
         L     R2,ADHEIRA                                                       
         GOTOR GETEL                                                            
         J     PA0902                                                           
                                                                                
PA09     GOTOR NEXTEL                                                           
PA0902   JNE   PA13                                                             
         USING NUMELD,R2                                                        
         CLI   NUMLN,NUMLN2Q                                                    
         JL    PA09                                                             
         CLC   NUMTYPE,MEDIA                                                    
         JNE   PA09                                                             
         ST    R2,ACLINUM                                                       
         DROP  R2                                                               
                                                                                
PA13     MVI   ERRNUM,0                                                         
         MVI   JOBTYP,0                                                         
         MVI   JFLG,0                                                           
         XC    AGYLSTUD,AGYLSTUD                                                
                                                                                
         TM    PGMSTA,PGMALL21     TEST 21                                      
         JNZ   *+12                YES.                                         
         TM    FORMTRN,LVC         TEST PROCESS TRANS. AT LEVEL C               
         JZ    PA14                NO, KEEP TRANSACTION TABLE                   
         MVI   GTIND,0                                                          
         XC    NBTRN,NBTRN         CLEAR BILLABLE ITEM COUNT                    
         XC    NPRVB,NPRVB                                                      
         XC    FPRVB,FPRVB         PREVIOUS BILLS                               
                                                                                
PA14     L     R2,ABESEL           BILLING ESTIMATE ELEMENTS                    
         MVI   0(R2),EOT                                                        
         L     R2,AINCEL           INTERNAL INCOME ELEMENTS                     
         MVI   0(R2),EOT                                                        
         L     R2,AINTLB           INTERNAL INCOME TABLE                        
         MVI   0(R2),EOT                                                        
         L     R2,AICMPWC          INTERCOMPANY WORKCODE TABLE                  
         MVI   0(R2),EOT                                                        
                                                                                
         L     R0,AESWC            ESTIMATE WORKCODES                           
         LHI   R1,ESWCLNQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   FCRDTRNS,NO                                                      
         MVI   FCRDEST,NO                                                       
         ZAP   JXBIGBAL,PZERO                                                   
         MVC   JXACLI,ADHEIRA                                                   
         MVC   JXAPRD,ADHEIRB                                                   
         MVC   JXAJOB,ADACC                                                     
         MVC   JXACQ,ADQSTACK                                                   
         MVI   JXACTN,JXAPACF      PROCESS ACCOUNT FIRST                        
         TM    UPSI,UPDMG          TRACE DATAMGR CALLS?                         
         JNO   *+8                                                              
         OI    JXOPTS,JXOPTIO      SET TRACE IO'S IN AJAX                       
         MVC   JXFORMC,FORMCDE     PASS 27 FORM CODE                            
                                                                                
         MVI   JXMODE,JXMOVRN      ASSUME OVERNIGHT                             
         TM    SOONS,SOONRUN       TEST SOON                                    
         JNO   *+8                                                              
         MVI   JXMODE,JXMSOON      SET SOON IN JOB BLOCK                        
                                                                                
         TM    OPTA,OPTARJOB       TEST RETAIL ADJUSTMENT                       
         JNO   *+8                                                              
         OI    JXOPTS2,JXORJOB     SET RETAIL ADJ. FOR AJAX                     
                                                                                
         L     R7,ADGOBLOC         GET GOBLOCK                                  
         USING GOBLOCKD,R7                                                      
         CLI   GOSUP99S,0          CHECK FOR SUPPRESS 99'S OVERRIDE             
         JE    PA15                                                             
         NI    OPT2,X'FF'-OPT2SPB                                               
         CLI   GOSUP99S,C'Y'                                                    
         JNE   PA15                                                             
         OI    OPT2,OPT2SPB                                                     
                                                                                
PA15     L     R7,GOAEXT           GET EXTENSION BLOCK                          
         USING GOXBLKD,R7                                                       
         CLI   GOUSD,C'Y'          ARE WE PRINTING USD?                         
         JNE   *+8                 NO                                           
         OI    OPTC,OPTCUSD        YES, SET OPTION                              
*                                                                               
         XC    JXRR,JXRR           CLEAR THE RE-RUN BLOCK                       
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JNO   PA16                                                             
         JAS   RE,UNBIL                                                         
         JNE   XIT                                                              
         J     PA17                                                             
                                                                                
PA16     TM    PGMSTA,PGMRRB       TEST RERUN A BILL                            
         JNO   PA17                                                             
         GOTOR RERUN                                                            
         JNE   XIT                                                              
                                                                                
PA17     TM    CTAXOPT,CTAXOGST    TEST NEED GST                                
         JNO   *+8                                                              
         OI    JXOPTS2,JXOPGST                                                  
         TM    CTAXOPT,CTAXOPST    TEST NEED PST                                
         JNO   *+8                                                              
         OI    JXOPTS2,JXOPPST                                                  
         GOTOR AJAX                                                             
                                                                                
PA19     LLC   RF,JXBLTYP          TEST BILL TYPE FOR THIS PROGRAM              
         EXRL  RF,PJOBTMT0         SKIP TYPES NOT VALID FOR THIS PGM            
         JNO   XIT                                                              
         MVC   BILTDATA,JXBLCODE   SAVE BILL TYPE DATA                          
                                                                                
PA21     OC    JXERRS,JXERRS       ANY BILLING PROBLEMS                         
         JZ    PA22                                                             
         GOTOR AADDREP,NONBIL      ADD 'NON-BILLABLE' JOB                       
         J     XIT                                                              
                                                                                
PJOBTMT0 TM    PGMBTYP,0                                                        
                                                                                
PA22     CLI   ERRNUM,0            TEST 'OTHER' ERRORS                          
         JNE   XIT                                                              
         TM    BILTTYP,ESTM        TEST % OF ESTIMATE                           
         JNO   PA27                                                             
         GOTOR PEST,PESTFQ         % OF ESTIMATE - FIRST                        
         J     PA29                                                             
                                                                                
PA27     TM    BILTTYP,SPCL        TEST SPECIAL AMOUNT                          
         JNO   PA29                                                             
         GOTOR SPEC,SPECFQ                                                      
                                                                                
PA29     TM    JXBLSTAT,JXBLSTRN   TEST READ TRANSACTIONS                       
         JNO   *+8                                                              
         MVI   FCRDTRNS,YES                                                     
         TM    JXBLSTAT,JXBLSEST   TEST READ ESTIMATES                          
         JNO   *+8                                                              
         MVI   FCRDEST,YES                                                      
                                                                                
         OC    ALNKEL,ALNKEL       TEST AGENCY/STUDIO LINK                      
         JZ    PA31                                                             
         GOTOR STUDIO              SET AGENCY STUDIO DATA                       
                                                                                
*                                  GET PRINT ON BILLS + AFTER                   
PA31     GOTOR GETCMT,DMCB,('SCMTPRBI+SCMTPRAD',ADACC),ADHEIRB,ADHEIRA          
                                                                                
         LARL  R3,WPPTAB           GET SPECIAL WPP COMMENTS                     
         LHI   R0,WPPTABN                                                       
C        USING CPYELD,CPYC                                                      
PA32     CLC   C.CPYALPHA,0(R3)    TEST IF A WPP AGENCY                         
         JNE   PA33                NO                                           
         GOTOR GETCMT,DMCB,(C'W',2(R3)) GET WPP COMMENTS & EXIT                 
         J     PA35                                                             
                                                                                
PA33     LA    R3,L'WPPTAB(R3)     BUMP TO NEXT - TRY AGAIN                     
         JCT   R0,PA32                                                          
                                                                                
         GOTOR GETJLD              Get extra job comment (if any)               
                                                                                
PA35     GOTOR APOST               CALL POSTING ROUTINE                         
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT I/O COUNT AT EXIT                 
         J     XIT                                                              
                                                                                
         DS    0H                                                               
WPPTAB   DS    0XL8                                                             
         DC    C'YP',C'WPPCOM'     YSHNY                                        
         DC    C'YN',C'WPPCOM'     YNRO                                         
         DC    C'JG',C'WPPCOM'     CWSFS                                        
         DC    C'WW',C'WPPCOM'     WWSFS                                        
         DC    C'M2',C'WPPCOM'     MENYA                                        
         DC    C'H7',C'WPPCOM'     MSNY                                         
         DC    C'JW',C'WPPCOM'     JWNY                                         
WPPTABN  EQU   (*-WPPTAB)/L'WPPTAB                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS - DEBITS                                       *         
***********************************************************************         
                                                                                
PTN00    TM    REQOPT,REQERR       TEST ERROR                                   
         JO    XIT                                                              
         TM    JFLG,JFNONB          TEST ALREADY FOUND TO BE UNBILLABLE         
         JO    XIT                                                              
         MVI   TFLG,0              INITIALIZE TRANSACTION FLAG                  
         MVI   JXTNS,0                                                          
         CLI   ERRNUM,0            TEST PREVIOUS ERROR                          
         JNE   XIT                 YES, SKIP IT                                 
         L     R3,ADTRANS                                                       
         USING TRNELD,R3                                                        
         LR    R2,R3                                                            
         SH    R2,DATADISP                                                      
         ST    R2,ATRNREC                                                       
         STCM  R2,15,JXATRN                                                     
         USING TRNRECD,R2                                                       
         TM    TRNRECD+ACCOSTAT,TRNSDRFT                                        
         JO    XIT                 SKIP, DRAFT                                  
         CLC   TRNKWORK,WCORDRQ    SKIP ORDERS                                  
         JE    XIT                                                              
         CLC   TRNKWORK,WCBILLQ    TEST PREVIOUS BILLING                        
         JE    PTC                 YES, PROCESS CREDITS                         
                                                                                
         TM    TRNSTAT,TRNSREV     TEST REVERSAL                                
         JO    XIT                 YES, SKIP IT                                 
                                                                                
         TM    JXPBSTA,JXPBSPCT    TEST PERCENT OF BILL                         
         JZ    PTD01                                                            
         TM    JXSTAT1,JXS1DIST    TEST RETAIL                                  
         JO    PTD01               YES, NO POB ON RETAIL                        
         TM    BILTTYP,TOTL+ONEL   TEST TOTAL BILL                              
         JZ    PTD01               NO, DON'T ADD PREVIOUS                       
         CLC   TRNKWORK,JXPBWRK    SKIP % OF BILL(POB) WORKCODES                
         JNE   PTD01                                                            
         AP    POBPRV,TRNAMNT      ADD TO PREVIOUS POB                          
                                                                                
PTD01    TM    BILTTYP,ESTM+SPCL   TEST % OF ESTIMATE OR SPECIAL                
         JNZ   XIT                 YES, SKIP REAL CHARGES                       
                                                                                
         MVC   WRKCCUR,TRNKWORK                                                 
         CLC   WRKCWC,WRKCCUR      TEST SAME WORKCODE                           
         JE    PTD03                                                            
         GOTOR AWRKM,WRKMUPQ       UPDATE WORKCODE ENTRY                        
                                                                                
PTD03    TM    PGMSTA,PGMUNB       IF UNBILLING, SKIP                           
         JO    PTD08                                                            
                                                                                
         L     R7,ADGOBLOC         SET THE GOBLOCK                              
         USING GOBLOCKD,R7                                                      
         TM    TRNSTAT,TRNSHOLD    TEST HELD ITEM                               
         JNO   PTD05               NO,                                          
         CLI   GOAUTOUN,YES        TEST AUTO UNHOLD                             
         JNE   PTD04                                                            
         NI    TRNSTAT,X'FF'-(TRNSHOLD)                                         
         MVI   MODE,WRITRANS       SET TO UNHOLD FOR NEXT TIME                  
                                                                                
PTD04    TM    OPTB,OPTBAPP        TEST UNHELD ONLY                             
         JO    XIT                 YES, DON'T BILL                              
         NI    TRNSTAT,X'FF'-(TRNSHOLD)  TURNOFF FOR BILLING                    
         OI    JXTNS,JXTNSUNH      TREAT AS UNHELD                              
                                                                                
PTD05    CLC   TRNDATE,ENDATE1     TEST PASSED END DATE                         
         JH    XIT                 YES, SKIP IT                                 
         CLI   TRNTYPE,TRNTTALE    TEST TALENT TRANSFER                         
         JNE   PTD07               NO,                                          
         TM    OPT2,OPT2XTED       EXCLUDE TALENT FOR 1 EXTRA DAY               
         JNO   PTD07                                                            
         CLC   TRNDATE,ENDATE1                                                  
         JE    XIT                 EXCLUDE LAST DAY                             
                                                                                
PTD07    TM    OPTB,OPTBMED        TEST MEDIA ONLY                              
         JNO   *+12                NO,                                          
         TM    WRKCSTA,WRKCSMT     YES, TEST MEDIA CHARGE                       
         JNO   XIT                 NO, SKIP IT                                  
         TM    OPTB,OPTBPRD        TEST PRODUCTION ONLY                         
         JNO   *+12                NO,                                          
         TM    WRKCSTA,WRKCSMT     YES, TEST MEDIA CHARGE                       
         JO    XIT                 YES, SKIP IT                                 
                                                                                
PTD08    GOTOR PTDC                TEST AJAX - ADD TO TABLE                     
         JNE   XIT                 SKIP IT                                      
         NI    JXSTATA,ALL-(JXSANACT)  TURNOFF 'NO-ACTIVITY'                    
                                                                                
         L     R4,CBTRN                                                         
         USING BTRND,R4                                                         
         LA    R6,BTRNBK                                                        
         USING JOBD,R6                                                          
                                                                                
         CLC   TRNKCUNT(2),=C'SK'  SK CONTRA                                    
         JNE   *+8                                                              
         OI    BTRNSTA,BTRNSSK     SET SK REVERSAL REQUIRED                     
                                                                                
         TM    TRNSTAT,TRNSNOCM    IS IT NON-COMM?                              
         JNO   *+8                                                              
         OI    BTRNSTA,BTRNSNC                                                  
                                                                                
         TM    JXTSTAT2,JXTS2NEW   TEST NEW ITEM                                
         JNO   *+8                                                              
         OI    BTRNSTA,BTRNSNEW    SET 'NOT PREVIOUSLY BILLED'                  
                                                                                
         TM    JXTSTAT3,JXTS3MKU   TEST MARK AS USED                            
         JNO   *+8                                                              
         OI    BTRNSTA2,BTRNSMKU   SET 'NOT PREVIOUSLY BILLED'                  
                                                                                
         TM    WRKCSTA,WRKCSMT     TEST MEDIA CHARGE                            
         JNO   *+12                                                             
         MVI   BTRNPORM,BTRNMEDA   MAKE IT MEDIA                                
         OI    JFLG,JFMED          SET MEDIA ITEMS ON JOB                       
                                                                                
         MVI   ELCODE,SPDELQ                                                    
         L     R2,ADTRANS                                                       
         GOTOR NEXTEL                                                           
         JNE   PTD11                                                            
         USING SPDELD,R2                                                        
         CLC   SPDACCS(2),=C'SK'   SK IN ELEMENT (CONTRA 1R)                    
         JNE   PTD11                                                            
         OI    BTRNSTA,BTRNSSK     SET SK REVERSAL REQUIRED                     
                                                                                
PTD11    LA    R5,JXTTOT                                                        
         TM    PGMAQ,PGMALL27      TEST 27 BILLING                              
         JZ    *+8                                                              
         LA    R5,JXTALLO                                                       
         USING JXBKD,R5                                                         
         ZAP   BTRNCOMR,WRKCRTE    COMM RATE                                    
         ZAP   JOBHRS,JXBHRS       HOURS                                        
         ZAP   JOBNET,JXBNET       NET                                          
         ZAP   JOBCOM,JXBCOM       COMMISSION                                   
         ZAP   JOBCD,JXBCSD        CASH DISCOUNT                                
         AP    JOBNET,JOBCD        ADD CD TO NET                                
         ZAP   JOBGRS,JOBNET       NET + COMMISSION=GROSS                       
         AP    JOBGRS,JOBCOM                                                    
                                                                                
         TM    PGMAQ,PGMALL27      TEST 27 BILLING                              
         JZ    PTD17                                                            
         TM    JXTSTAT3,JXTS3ASR   TEST ASP REVERSAL                            
         JZ    *+14                                                             
         SP    ASPAMT,JXBNET                                                    
         OI    BTRNSTA,BTRNSASR                                                 
                                                                                
         ZAP   DUB,JXBNET                                                       
         AP    DUB,JXBCSD                                                       
         TM    OPT6,OPT6COWC       TEST ADD COMMISSION TO WC                    
         JNO   *+10                NO,                                          
         AP    DUB,JXBCOM                                                       
                                                                                
         CLI   WRKCTYPE,WRKCTOOP    TEST OOP                                    
         JNE   *+10                                                             
         AP    OOP,DUB                                                          
                                                                                
         MVC   BTRNWCTY,WRKCTYPE    SAVE WC TYPE                                
         CLI   WRKCTYPE,WRKCTTIM    TEST TIME                                   
         JE    *+12                                                             
         CLI   WRKCTYPE,WRKCTRET    RETAINER IS ADDED TO PSF                    
         JNE   *+10                                                             
         AP    PSF,DUB              ADD TO PROFESSIONAL SERVICE FEE             
                                                                                
         OI    AGLV,LVC+LVB+LVA    SET A27 GROUP ACTIVITY FLAGS                 
                                                                                
PTD17    MVI   ELCODE,PRTELQ                                                    
         L     R2,ADTRANS                                                       
         GOTOR NEXTEL                                                           
         JNE   PTD18               GET RATE ELEMENT                             
         USING PRTELD,R2                                                        
         ZAP   BTRNBILR,PRTRATE                                                 
                                                                                
PTD18    MVI   ELCODE,UNPELQ       LOOK FOR UNIT PRICING                        
         L     R2,ADTRANS                                                       
         GOTOR NEXTEL                                                           
         JNE   PTD19                                                            
         USING UNPELD,R2                                                        
         ZAP   BTRNUNTS,UNPUNIT                                                 
         ZAP   BTRNUPRC,UNPRICE                                                 
         ZAP   BTRNBILR,UNPRICE                                                 
         TM    UNPSTAT,UNPSQTRH    TEST UNITS ARE QUARTER HOURS                 
         JZ    PTD19                                                            
         OI    BTRNSTA,BTRNSUQH                                                 
                                                                                
PTD19    MVI   ELCODE,FFTELQ       LOOK FOR LONG INVOICE #                      
         L     R2,ADTRANS                                                       
         USING FFTELD,R2                                                        
PTD21    GOTOR NEXTEL                                                           
         JNE   PTD23                                                            
         CLI   FFTTYPE,FFTTINVN                                                 
         JNE   PTD21                                                            
         OI    BTRNSTA,BTRNSLIN    SET LONG INVOICE AVAILABLE                   
                                                                                
PTD23    GOTOR PTDCX               LAST ROUTINE FOR DR/CR                       
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS - CREDITS                                      *         
***********************************************************************         
                                                                                
PTC      GOTOR GETSK                CHECK FOR SK/SI FLIP                        
         TM    PGMSTA,PGMRRB+PGMUNB TEST RERUN OR UNBILL                        
         JZ    PTC07                                                            
                                                                                
         L     R2,ADTRANS                                                       
         CLC   TRNNARR(6),=C'MANUAL'                                            
         JE    PTC05                                                            
                                                                                
         MVI   ELCODE,PTAELQ                                                    
PTC00    GOTOR NEXTEL                                                           
         JNE   PTCX                                                             
         USING PTAELD,R2                                                        
         CLI   PTATYPE,0                                                        
         JE    PTC00                                                            
         CLC   PTADATE,JXRRADT                                                  
         JH    PTCX                SKIP, BILLS AFTER THIS DATE                  
         JL    PTC07               TAKE BILLS BEFORE TODAY                      
         TM    JXSTAT1,JXS1DIST    TEST RETAIL BILL                             
         JO    PTC01                                                            
         CLC   JXRRBILL,PTARBLNO   TEST ELEMENT FOR UNBILL                      
         JNE   PTC07                                                            
         L     R3,ADTRANS                                                       
         J     PTC07A                                                           
         DROP  R2                                                               
                                                                                
PTC01    L     R2,ATRNREC          ** RETAIL UNBILL **                          
         USING TRNRECD,R2                                                       
         L     RF,JXARETLB                                                      
         USING RTLD,RF                                                          
PTC02    CLI   0(RF),EOT                                                        
         JE    PTCX                                                             
         CLC   TRNKCULC,RTLDIS     MATCH CONTRA TO DISTRIBUTOR TABLE            
         JE    PTC03                                                            
         LA    RF,RTLLNQ(RF)                                                    
         J     PTC02                                                            
                                                                                
         USING TRNELD,R3                                                        
PTC03    L     R3,ADTRANS                                                       
         CLC   TRNREF,RTLBIL       MATCH BILL NUMBER                            
         JH    PTCX                SKIP HIGHER BILL                             
         TM    BILTTYP,SPCL        NEED SPECIAL TO GET TOTAL                    
         JO    PTC04                                                            
         CLC   TRNREF,RTLBIL       MATCH BILL NUMBER                            
         JL    PTC07                                                            
         GOTOR OBA,DMCB,TRNELD,RTLOBA                                           
         J     PTCX                                                             
                                                                                
PTC04    CLC   TRNREF,RTLBIL        MATCH BILL NUMBER                           
         JNE   PTCX                                                             
         GOTOR OBA,DMCB,TRNELD,RTLOBA                                           
         J     PTC11                                                            
         DROP  R2,RF                                                            
                                                                                
PTC05    MVI   ELCODE,X'60'        INCLUDE ALL MANUAL BILLING                   
         GOTOR NEXTEL              THAT WAS ON FILE WHEN THE TOTAL              
         JNE   PTC05A              BILL WAS RUN                                 
         USING TRSELD,R2                                                        
         CLC   TRSDATE,JXRRADT                                                  
         JH    PTCX                                                             
         J     PTC07                                                            
         DROP  R2                                                               
                                                                                
         USING TRNELD,R3                                                        
PTC05A   L     R3,ADTRANS                                                       
         GOTOR DATCON,DMCB,(1,TRNDATE),(2,WORK)                                 
         CLC   WORK(2),JXRRADT                                                  
         JH    PTCX                                                             
         DROP  R3                                                               
                                                                                
         USING TRNELD,R3                                                        
PTC07    L     R3,ADTRANS                                                       
         CLC   TRNREF,JXRRNUM      TEST NUMBER                                  
         JNE   PTC08                                                            
         TM    PGMSTA,PGMRRB       TEST RERUN                                   
         JO    PTCX                YES, SKIP THIS BILL                          
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JZ    PTC08                                                            
PTC07A   GOTOR OBA,DMCB,TRNELD,JXAOBA                                           
         J     PTCX                SKIP THE ORIGINAL INVOICE                    
                                                                                
PTC08    TM    BILTTYP,TOTL+ONEL+ESTM                                           
         JNZ   PTC09                                                            
         TM    PGMSTA,PGMUNB+PGMRRB IS IT UNBILLING OR RERUN?                   
         JNZ   PTC09                                                            
         TM    TRNSTAT,TRNSREV     SKIP REVERSALS                               
         JO    PTCX                                                             
                                                                                
PTC09    CLC   JXWCFLT,SPACES      TEST W/C FILTER                              
         JNE   PTCX                YES, SKIP 99'S                               
                                                                                
PTC11    GOTOR PTDC                TEST AJAX - ADD TO TABLE                     
         JNE   PTCX                SKIP IT                                      
         L     R4,CBTRN                                                         
         USING BTRND,R4                                                         
         LA    R6,BTRNBK                                                        
         USING JOBD,R6                                                          
                                                                                
         MVC   BTRNBTYP,TRNBLTYP                                                
         OI    JFLG,JFPBIL         SET JOB HAS PREVIOUS BILLS                   
                                                                                
         TM    TFLG,TFSK2SI                                                     
         JNO   *+8                                                              
         OI    BTRNSTA,BTRNSSK2    SK TO SI INCOME                              
                                                                                
         LA    R5,JXTTOT           GET BILLED AMOUNTS                           
         ZAP   JOBGRS,JXBGRS                                                    
         ZAP   JOBNET,JXBNET                                                    
         ZAP   JOBCD,JXBCSD                                                     
         ZAP   JOBCOM,JXBCOM                                                    
         SP    JOBGRS,JOBCD                                                     
         SP    JOBNET,JOBCD                                                     
                                                                                
         ICM   RF,15,NPRVB         NUMBER OF PREVIOUS BILLS                     
         JNZ   *+8                                                              
         ST    R4,FPRVB            A(OF FIRST BILL IN TABLE)                    
         AHI   RF,1                                                             
         ST    RF,NPRVB                                                         
                                                                                
         GOTOR PTDCX               LAST COMMON ROUTINES                         
                                                                                
PTCX     TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS - DEBITS & CREDITS                             *         
***********************************************************************         
                                                                                
PTDC     NTR1  LABEL=*                                                          
         MVI   JXACTN,JXAPTRN      PROCESS TRANSACTION                          
         GOTOR AJAX                PASS TO AJAX                                 
                                                                                
         TM    JXBLTYP,ALLO        TEST ALLOCATED BILL                          
         JZ    PTDC01              NO,                                          
         TM    JXTSTAT2,JXTS2ALC   YES, TEST ALLOCATED CHARGE                   
         JNO   XITN                NO, SKIP IT                                  
         CLI   WRKCTYPE,WRKCTPRE   TEST PREBILL                                 
         JNE   *+10                                                             
         MVC   PRENAME,WRKCNME     SAVE PREBILL NAME                            
         J     PTDC02              YES, TAKE IT                                 
                                                                                
*              SKIP IF: NOT IN RERUN, PENDING, OR WRITEOFF                      
PTDC01   TM    JXTSTAT2,JXTS2NRR+JXTS2PND+JXTS2WO                               
         JNZ   XITN                                                             
                                                                                
PTDC02   L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
                                                                                
         L     R4,ABTRN            R4=TABLE OF BILLABLE ITEMS                   
         ICM   R1,15,NBTRN                                                      
         MHI   R1,BTRNLQ           R1=LENGTH OF TABLE                           
         AR    R4,R1                                                            
         ST    R4,CBTRN            SAVE ADDRESS OF CURRENT ENTRY                
         USING BTRND,R4            ADD ITEM TO BILLABLE TABLE                   
                                                                                
         LA    R6,BTRNBK                                                        
         USING JOBD,R6                                                          
         XC    BTRND(BTRNLQ),BTRND                                              
                                                                                
         MVC   BTRNWC,TRNKWORK     WORKCODE                                     
         MVC   BTRNCA,TRNKCULC     CONTRA                                       
         MVC   BTRNDTE,TRNKDATE    DATE                                         
         MVC   BTRNREF,TRNKREF     REF                                          
         MVC   BTRNSBR,TRNKSBR     SUBREF                                       
         MVC   BTRNTYP,TRNTYPE     TYPE                                         
         MVI   BTRNPORM,BTRNPROD   DEFAULT IS PRODUCTION                        
         MVC   BTRNUSD,TRNRECD+ACCOUSED                                         
                                                                                
         TM    PGMSTA,PGMRRB       TEST RERUN                                   
         JZ    PTDC05              NO,                                          
         CLC   BTRNUSD,TODAY2      WAS THIS BILLED IN ORIGINAL RUN              
         JNE   PTDC09              NO,                                          
         J     PTDC07                                                           
                                                                                
PTDC05   TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JZ    PTDC09              NO,                                          
         TM    PGMAQ,PGMALL27      TEST A27 BILLING                             
         JNZ   *+14                YES, SKIP MARKED DATE                        
         CLC   BTRNUSD,JXRRADT     WAS THIS BILLED IN ORIGINAL RUN              
         JNE   PTDC09              NO,                                          
         MVI   ELCODE,PTAELQ                                                    
         L     R2,ADTRANS                                                       
                                                                                
PTDC06   GOTOR NEXTEL                                                           
         JNE   PTDC09                                                           
         USING PTAELD,R2                                                        
         CLI   PTATYPE,0                                                        
         JE    PTDC06                                                           
         CLC   JXRRBILL,PTARBLNO   TEST ELEMENT FOR UNBILL                      
         JNE   PTDC06                                                           
                                                                                
PTDC07   XC    BTRNUSD,BTRNUSD     YES, CONSIDER AS NOT BILLED                  
         OI    BTRNSTA,BTRNSNEW    SET 'NOT PREVIOUSLY BILLED'                  
         OI    BTRNSTA,BTRNSUNB    SET TO UNBILL THIS ITEM                      
         TM    JXPBSTA,JXPBSPCT    TEST PERCENT OF BILL                         
         JZ    PTDC09                                                           
         CLC   BTRNWC,JXPBWRK      POB WORKCODE                                 
         JNE   PTDC09                                                           
         L     R3,ADTRANS                                                       
         USING TRNELD,R3                                                        
         ZAP   POBCUR,TRNAMNT                                                   
         XI    POBCUR+L'POBCUR-1,X'01'                                          
                                                                                
PTDC09   L     RF,AMONACC                                                       
         MVC   BTRNDA,ACMTRNDA-ACMD(RF) SAVE DISK ADDRESS                       
         CLI   WRKCFTYP,WRKCFTIM                                                
         JNE   PTDC10                                                           
         CLI   WRKCGRP,C' '        TEST ANY GROUP                               
         JH    PTDC10              YES,                                         
         OI    GTIND,GTIPFS        PFS WILL BE DEFAULT                          
                                                                                
PTDC10   L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         ZAP   BTRNBILR,PZERO      CLEAR TRANSACTION ACCUMS                     
         ZAP   BTRNCOMR,PZERO                                                   
         ZAP   BTRNUNTS,PZERO                                                   
         ZAP   BTRNUPRC,PZERO                                                   
                                                                                
         LA    RF,BKL                                                           
         LA    R0,BTBKN                                                         
         ZAP   0(BKLQ,RF),PZERO                                                 
         LA    RF,BKLQ(RF)                                                      
         JCT   R0,*-10                                                          
                                                                                
         MVI   MODE,PROCTRNS       ADDING TO TABLE - CAN'T BE WRITRANS          
         ICM   R1,15,NBTRN         ADD TO ITEM IN TABLE                         
         CHI   R1,MXBTRN           TEST MAX                                     
         JNL   ER2MANY                                                          
                                                                                
PTDC11   AHI   R1,1                                                             
         STCM  R1,15,NBTRN                                                      
         J     XITY                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS - DEBITS & CREDITS - LAST                      *         
***********************************************************************         
                                                                                
PTDCX    NTR1  LABEL=*                                                          
         L     R4,CBTRN            PASS TO AJAX                                 
         USING BTRND,R4                                                         
         CLI   CTAXOPT,0           TEST CANADIAN TAX                            
         JE    PTDCX4              NO, SKIP TAX ROUTINE                         
         CLC   BTRNWC,WCBILLQ      TEST WORKCODE 99                             
         JNE   PTDCX2              YES, PROCESS OLD TAXES                       
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JO    XIT                 YES, ALREADY HAVE TAXES                      
         GOTOR CANTX,CANTRNQ                                                    
         J     PTDCX4                                                           
                                                                                
PTDCX2   TM    BILTTYP,PROG        TEST PROGRESSIVE                             
         JZ    PTDCX3                                                           
         OC    BTRNUSD,BTRNUSD     TEST NEW ITEM                                
         JNZ   PTDCX4              NO, DON'T ADD TO TAX SUMMARY                 
                                                                                
PTDCX3   LA    R5,JXTTOT                                                        
TRN      USING JXBKD,R5                                                         
                                                                                
         LA    R6,JXATOT                                                        
         TM    PGMAQ,PGMALL27      TEST A27 BILLING                             
         JZ    *+8                 NO,                                          
         LA    R6,JXAALLO          YES, ADD TO ALLOCATED                        
ACC      USING JXBKD,R6                                                         
                                                                                
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JO    PTDCX4              YES, ALREADY HAVE TAXES                      
         GOTOR CANTX,CANTRNQ       GET  TAX FOR THIS ITEM                       
                                                                                
         LA    RF,BGSTBK                                                        
         USING CTAXD,RF                                                         
         AP    TRN.JXBGST,CTAXTAX  ADD GST TO TRANSACTION ACCUMS                
         AP    ACC.JXBGST,CTAXTAX  AND ACCOUNT ACCUMS                           
                                                                                
         LA    RF,BPSTBK                                                        
         AP    TRN.JXBPST,CTAXTAX  SAME FOR PST                                 
         AP    ACC.JXBPST,CTAXTAX                                               
         DROP  R5,R6,RF,TRN,ACC                                                 
                                                                                
PTDCX4   GOTOR AINTRNL,INTLTRNQ    GET INTERNAL INCOME                          
         TM    BTRNSTA,BTRNSNEW    TEST 'NEW' ITEM                              
         JNO   PTDCX5                                                           
         TM    AGYSTAT,AGYSICMP    TEST INTERCOMPANY POSTINGS?                  
         JNO   PTDCX5                                                           
         GOTOR AICMPY,ICMPTRNQ     ADD TRANSACTION TO INTERCOMPANY              
                                                                                
PTDCX5   TM    PGMSTA,PGMUNB       IS IT UNBILLING?                             
         JNO   PTDCX7              NO,                                          
         LA    R0,BTBKN            YES, REVERSE BUCKETS                         
         LA    RE,BTRNBK                                                        
         XI    BKLQ-1(RE),X'01'    REVERSE SIGN                                 
         LA    RE,BKLQ(RE)                                                      
         JCT   R0,*-8                                                           
                                                                                
PTDCX7   TM    UPSI,UPTRN          TRACE TRANSACTIONS                           
         JNO   XIT                                                              
         GOTOR ATRCE,TRCTRNQ                                                    
         J     XIT                                                              
         DROP  R2,R3,R4,R7                                                      
         EJECT                                                                  
***********************************************************************         
* GET AMOUNT OF SK TO SI FLIP                                         *         
***********************************************************************         
                                                                                
         USING TRNELD,R3                                                        
GETSK    ST    RE,SAVRE            GET AMOUNT OF SK INCOME FLIP                 
         CLI   TRNBTYPE,TRNBTPER   TEST %EST BILL                               
         JNE   GETSKX              NO,                                          
         CP    TRNAM2SK,PZERO      ANY SK INCOME FROM A %EST?                   
         JE    GETSKX              NO,                                          
         TM    BILTTYP,TOTL+ONEL   TEST THIS A TOTAL OR ONELINE                 
         JZ    GETSKX              NO, DON'T DO THE FLIP                        
*                                  ** RERUN **                                  
         TM    PGMSTA,PGMRRB       TEST RERUN?                                  
         JNO   GETSK03             NO,                                          
         CLC   TRNSK2SI,TODAY2     TEST MOVED ON RERUN DATE                     
         JNE   GETSKX              NO,                                          
         J     GETSK09             YES, OK TO PROCESS                           
                                                                                
*                                  ** UNBILL **                                 
GETSK03  TM    PGMSTA,PGMUNB       TEST UNBILL                                  
         JNO   GETSK07             NO,                                          
         CLI   TRNSK2SI,0          TEST STILL IN SI                             
         JE    GETSKX              NO, DON'T FLIP                               
         MVI   BYTE,0                                                           
         LR    R2,R3               GET ORGINAL BILL NUMBER                      
         MVI   ELCODE,GDAELQ                                                    
                                                                                
         USING GDAELD,R2                                                        
GETSK04  GOTOR NEXTEL                                                           
         JNE   GETSK05             NO ELEMENT - DO THE FLIP                     
         CLI   GDATYPE,GDATBILL                                                 
         JNE   GETSK04                                                          
         CLI   GDALN,GDALN4Q       OLD ELEMENT - DO THE FLIP                    
         JL    GETSK04                                                          
         MVI   BYTE,C'Y'           SET ELEMENT FOUND STATUS                     
         CLC   JXRRNUM,GDABILNO    TEST THIS IS BILL BEING REVERSED             
         JNE   GETSK04             NO, DON'T FLIP                               
         J     GETSK09             YES, OK TO PROCESS                           
                                                                                
GETSK05  CLI   BYTE,C'Y'           TEST FOUND NEW ELEMENTS                      
         JE    GETSKX              YES, BUT DID NOT MATCH - DON'T FLIP          
         J     GETSK09             NO, BETTER DO THE FLIP                       
                                                                                
*                                  ** BILL **                                   
GETSK07  CLI   TRNSK2SI,0          TEST AREADY MOVED TO SI                      
         JNE   GETSKX              YES, DON'T FLIP                              
                                                                                
GETSK09  AP    SK2SICOM,TRNAM2SK   MOVE FROM SK TO SI                           
         AP    SK2SIGRS,TRNBLPAY   AND GROSS                                    
         OI    TFLG,TFSK2SI        SET SK TO SI INCOME                          
                                                                                
GETSKX   L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FOR UNBILL - GET ORIGINAL BILL AMOUNTS                              *         
***********************************************************************         
                                                                                
OBA      NTR1  LABEL=*                                                          
         LM    R2,R3,0(R1)                                                      
         USING TRNELD,R2                                                        
         USING JXBKD,R3                                                         
                                                                                
         LR    RF,R3                                                            
         LA    R0,JXBKRN           INITIALIZE BUCKETS                           
         ZAP   0(L'JXBK,RF),PZERO                                               
         LA    RF,L'JXBK(RF)                                                    
         JCT   R0,*-10                                                          
                                                                                
         ZAP   JXBNET,TRNAMNT      NET                                          
         ZAP   JXBCOM,TRNBLCOM     COMMISSION                                   
         ZAP   JXBCSD,TRNBLCD      CASH DISCOUNT                                
         AP    JXBGRS,JXBNET                                                    
         AP    JXBGRS,JXBCOM                                                    
         ZAP   JXBSKI,TRNBINTI     INTERNAL INCOME                              
                                                                                
OBA3     LLC   R0,TRNLN                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JE    OBA7                                                             
                                                                                
         USING VBIELD,R2                                                        
         CLI   0(R2),VBIELQ        GST                                          
         JNE   OBA5                                                             
         OI    CTAXOPT,CTAXOGST    TEST NEED GST                                
         AP    JXBGST,VBIVAT                                                    
         J     OBA3                                                             
                                                                                
         USING PBIELD,R2                                                        
OBA5     CLI   0(R2),PBIELQ        PST                                          
         JNE   OBA3                                                             
         OI    CTAXOPT,CTAXOPST    TEST NEED PST                                
         AP    JXBPST,PBIPST                                                    
         J     OBA3                                                             
                                                                                
OBA7     CLI   CTAXOPT,0                                                        
         JE    OBAX                                                             
         XC    CBTRN,CBTRN                                                      
         GOTOR CANTX,CANTRNQ       ADD CANADIAN TAX TO TABLE                    
                                                                                
OBAX     GOTOR SETNEG                                                           
         J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LEVEL A LAST                                                        *         
***********************************************************************         
                                                                                
LVAL00   TM    REQOPT,REQERR       TEST ERROR                                   
         JO    XIT                                                              
                                                                                
         TM    LOCKS,LOCKCLI       TEST CLIENT LOCKED(A27)                      
         JNO   LVAL01                                                           
         GOTOR UNLOCK,DMCB,('LOCKCLI',ADHEIRA)                                  
         OI    WRIT,WRITLVA        SET WRITE LEVEL A                            
                                                                                
LVAL01   TM    REQOPT,REQGRP       TEST GROUP BILL                              
         JNO   LVALX               NO,                                          
         TM    PGMAQ,PGMALL21      TEST A21                                     
         JNZ   LVALX               YES, OK TO SKIP                              
                                                                                
         LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         MVC   LVGBK,LVABK         SET GROUP BUCKETS                            
         ZAP   CSD$,LVGCSD                                                      
         DROP  R7                                                               
                                                                                
         TM    AGLV,LVA            TEST ACTIVITY AT THIS LEVEL                  
         JZ    LVAL03                                                           
         OI    CTAXOPT,CTAXOGRP                                                 
         GOTOR CANTX,CANTOTQ       GET TOTAL GST/PST                            
         MVI   PGLV,LVA            PROCESS LEVEL A RECORDS                      
         GOTOR ABFMT                                                            
                                                                                
         TM    RQLV,LVA            TEST END OF REQUEST LEVEL A                  
         JNO   LVAL03              NO,                                          
         GOTOR APOST               POST THE LEVEL 'A' GROUP BILL                
         NI    REQOPT,ALL-(REQGRPBN) TURNOFF 'HAVE GROUP BILL NUMBER'           
                                                                                
LVAL03   NI    CTAXOPT,ALL-(CTAXOGRP)                                           
                                                                                
LVALX    TM    WRIT,WRITLVA        TEST WRITE LEVEL A                           
         JNO   *+8                                                              
         MVI   MODE,WRITLEVA                                                    
         NI    WRIT,ALL-(WRITLVA)                                               
         NI    BLNSTA,ALL-(BLNSCLI)                                             
                                                                                
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LAST FOR LEVEL B                                                    *         
***********************************************************************         
                                                                                
LVBL00   TM    REQOPT,REQERR       TEST ERROR                                   
         JO    XIT                                                              
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         JNO   LVBLX               NO,                                          
                                                                                
         OI    CTAXOPT,CTAXOGRP    SET FOR GROUP BILL                           
         GOTOR CANTX,CANTOTQ       GET TOTAL GST/PST                            
                                                                                
         TM    PGMAQ,PGMALL21      TEST A21 GROUP                               
         JNZ   LVBL07              YES, POST BILL                               
                                                                                
         LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         MVC   LVGBK,LVBBK         SET GROUP BUCKETS                            
         ZAP   CSD$,LVGCSD                                                      
         TM    AGLV,LVB            TEST ACTIVITY AT THIS LEVEL                  
         JZ    LVBL05                                                           
         GOTOR ADDBUK,DMCB,LVBBK,LVABK                                          
                                                                                
         MVI   PGLV,LVB            PROCESS LEVEL B RECORDS                      
         GOTOR ABFMT                                                            
                                                                                
LVBL05   TM    RQLV,LVB            TEST END OF REQUEST LEVEL B                  
         JNO   LVBLX               NO,                                          
                                                                                
LVBL07   GOTOR APOST               POST THE LEVEL 'B' GROUP BILL                
         NI    REQOPT,X'FF'-(REQGRPBN) TURNOFF 'HAVE GROUP BILL NUMBER'         
                                                                                
LVBLX    NI    CTAXOPT,ALL-(CTAXOGRP)                                           
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR LEDGER                                                     *         
***********************************************************************         
                                                                                
LDGL00   TM    LOCKS,LOCKLDG       TEST LEDGER LOCKED(A27)                      
         JNO   LDGL03                                                           
         GOTOR UNLOCK,DMCB,('LOCKLDG',ADLEDGER)                                 
         OI    WRIT,WRITLDG                                                     
                                                                                
LDGL03   TM    SOONS,SOONRUN       TEST SOON RUN                                
         JO    LDGL05              YES, DON'T WRITE LEDGER YET                  
         TM    WRIT,WRITLDG        TEST NEED TO WRITE LEDGER                    
         JNO   LDGL05              NO,                                          
         MVI   MODE,WRITLEDG       FOR OVERNIGHT WRITE LEDGER                   
                                                                                
LDGL05   TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LAST FOR REQUEST                                                    *         
***********************************************************************         
                                                                                
RQL00    TM    REQOPT,REQGRP       TEST GROUP BILL                              
         JNO   RQL03                                                            
         GOTOR APOST               POST GROUP TOTAL                             
                                                                                
RQL03    L     R5,ADMASTC          WRITE REQUEST DETAILS TO SORT                
         USING MASTD,R5                                                         
         L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRQN,REQNUM        REQUEST NUMBER                               
         MVI   SKRQRT,SKRQRDQ      REQUEST DETAILS                              
         LA    R0,SRDATA                                                        
         L     RE,ADQSTACK                                                      
         LLC   R1,MCRQNUM          NUMBER OF CARDS                              
         MHI   R1,L'MCREQREC       * LENGTH OF EACH                             
         LR    RF,R1                                                            
         STH   RF,HALF                                                          
         MVCL  R0,RE               MOVE REQUEST STACK TO SORT                   
         LH    RF,HALF                                                          
         LA    RF,SRLNQ(RF)                                                     
         STCM  RF,3,SRLEN                                                       
         TM    REQOPT,REQPRT       TEST BILLS PRINTED                           
         JNO   *+8                                                              
         OI    SRSTAT,SRPRNT                                                    
         GOTOR ASORT,SORTPUTQ                                                   
         DROP  R3,R5                                                            
                                                                                
RQL04    OC    SVRCDATE,SVRCDATE                                                
         JZ    RQL05                                                            
         MVC   RCDATE,SVRCDATE                                                  
         GOTOR DATCON,DMCB,(4,RCDATE),(X'20',TODAY0)                            
         GOTOR (RF),(R1),,(1,TODAY1)                                            
         GOTOR (RF),(R1),,(2,TODAY2)                                            
         GOTOR (RF),(R1),,(3,TODAY3)                                            
         GOTOR (RF),(R1),,(8,TODAY8)                                            
                                                                                
RQL05    TM    SOONS,SOONRUN                                                    
         JZ    RQL07                                                            
         TM    WRIT,WRITMED        IF SOON RUN WRITE MEDIA RECORD NOW           
         JNO   *+8                                                              
         MVI   MODE,WRITMEDS       BECAUSE LEDGER MUST BE LAST                  
         NI    WRIT,ALL-(WRITMED)                                               
                                                                                
RQL07    TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VERIFY AND SET UNBILLING INFO                                       *         
***********************************************************************         
                                                                                
UNBIL    NTR1  LABEL=*                                                          
         MVC   AIO,AIO6            USE AIO6                                     
         LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,JOBCDE     C/U/L/JOB                                    
         MVC   TRNKWORK,WCBILLQ                                                 
         GOTOR ADMGR,ACCHIQ                                                     
         J     UNBIL5                                                           
                                                                                
UNBIL3   GOTOR ADMGR,ACCSEQQ                                                    
UNBIL5   LA    R2,DIR                                                           
         CLC   TRNKEY(TRNKCULC-TRNRECD),DKEY  TEST BILLING TRANSACTION          
         JNE   XITN                                                             
         CLI   ORGBDTE,X'FF'       TEST SPECIFIC INVOICE DATE                   
         JE    *+14                                                             
         CLC   TRNKDATE,ORGBDTE    MUST MATCH DATE                              
         JNE   UNBIL3                                                           
         CLC   QSELECT,SPACES      TEST SPECIFIC INVOICE                        
         JE    *+14                                                             
         CLC   TRNKREF,QSELECT     THAN MUST MATCH INVOICE                      
         JNE   UNBIL3                                                           
                                                                                
         GOTOR ADMGR,ACCGETQ       GET THE RECORD                               
         L     R2,AIO6                                                          
         CLI   TRNRFST,TRNELQ      TRANSACTION ELEMENT                          
         JNE   UNBIL3                                                           
                                                                                
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         TM    PGMSTA,PGMRRB+PGMUNB  TEST RE-RUN+UNBILLING                      
         JO    UNBIL13             YES                                          
         OC    TRNUNBIL,TRNUNBIL   TEST UNBILLED DATE                           
         JNZ   UNBIL3              SKIP IF ALREADY UNBILLED                     
         LR    R3,R2                                                            
         MVI   ELCODE,PTAELQ                                                    
UNBIL7   GOTOR NEXTEL                                                           
         JNE   UNBIL3                                                           
         USING PTAELD,R2                                                        
         TM    PTASTAT1,PTASREVS   TEST UNBILLING                               
         JO    UNBIL3              CAN'T UNBILL AN UNBILL                       
         TM    PTASTAT2,PTASRETB   TEST SECONDAEY RETAILER POSTING              
         JO    UNBIL3              CAN'T UNBILL                                 
                                                                                
         USING TRNELD,R2                                                        
UNBIL9   LR    R2,R3               RESTORE R2 TO TRNELD                         
         OI    JXRROPT,JXRROUNB    SET UNBILL OPTION IN AJAX                    
         MVC   JXRRADT,TRN2DAY     SET RUN DATE OF ORIGINAL                     
         GOTOR DATCON,DMCB,(2,JXRRADT),(1,JXRRADT1)                             
         J     UNBIL15                                                          
*                                  ** RERUN - UNBILLING **                      
UNBIL13  CLC   TRNUNBIL,TODAY2     WAS IT UNBILLED TODAY?                       
         JNE   UNBIL3              NO,                                          
         OI    JXRROPT,JXRRORRB    SET RERUN OPTION IN AJAX                     
         MVC   JXRRADT,TRN2DAY     SET RUN DATE OF ORIGINAL                     
         GOTOR DATCON,DMCB,(2,JXRRADT),(1,JXRRADT1)                             
                                                                                
UNBIL15  MVC   JXRRNUM,TRNREF               NUMBER                              
         MVC   JXRRDT1,TRNDATE              DATE                                
         MVC   JXRRBDA,DA                   DISK ADDRESS                        
         GOTOR DATCON,DMCB,(1,JXRRDT1),(2,JXRRDT2)                              
                                                                                
         LR    R2,R3               RESTORE R2 TO TRNELD                         
         MVI   ELCODE,VBIELQ       LOOK FOR TAX ELEMENTS                        
         GOTOR NEXTEL                                                           
         JNE   *+8                 YES,                                         
         OI    CTAXOPT,CTAXOGST    SET GST FOR UNBILL                           
                                                                                
         LR    R2,R3               RESTORE R2 TO TRNELD                         
         MVI   ELCODE,PBIELQ       LOOK FOR TAX ELEMENTS                        
         GOTOR NEXTEL                                                           
         JNE   *+8                                                              
         OI    CTAXOPT,CTAXOPST    SET PST FOR UNBILL                           
                                                                                
UNBILX   J     XITY                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VERIFY AND SET RERUN INFO                                           *         
***********************************************************************         
                                                                                
RERUN    NTR1  LABEL=*                                                          
         OI    JXRROPT,JXRRORRB    RERUN OPTION IN AJAX                         
         MVC   AIO,AIO6            USE AIO6                                     
         LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,JOBCDE     C/U/L/JOB                                    
         GOTOR ADMGR,ACCHIQ                                                     
         LA    R2,DIR                                                           
         CLC   TRNKEY(L'TRNKCULA),DKEY  TEST FOUND JOB                          
         JNE   XITN                BAD JOB CODE                                 
         TM    BILTTYP,ESTM        TEST % OF ESTIMATE                           
         JO    RERUN11                                                          
                                                                                
RERUN3   GOTOR ADMGR,ACCSEQQ                                                    
RERUN5   LA    R2,DIR                                                           
         CLC   TRNKEY(L'TRNKCULA),DKEY  TEST SAME JOB                           
         JNE   XITY                                                             
         CLI   TRNKDATE,C' '       TEST TRANSACTION                             
         JNH   RERUN3              NO,                                          
         GOTOR ADMGR,ACCGETQ       GET THE RECORD                               
         L     R2,AIO6                                                          
         CLI   TRNRFST,TRNELQ                                                   
         JNE   RERUN3                                                           
         MVI   ELCODE,PTAELQ       FIND PTAEL ELEMENT                           
         GOTOR GETELN                                                           
RERUN7   JNE   RERUN3                                                           
         USING PTAELD,R2                                                        
         CLI   PTATYPE,PTATRAL     TEST BILLING                                 
         JNE   RERUN3              NO,                                          
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         JO    RERUN3              YES,                                         
         CLC   PTARDATE,TODAY2     TEST BILLING DATE                            
         JE    RERUN9                                                           
         GOTOR NEXTEL                                                           
         J     RERUN7                                                           
                                                                                
RERUN9   MVC   JXRRADT,TODAY2      RERUN DATE                                   
         MVC   JXRRNUM,PTARBLNO    BILL NUMBER                                  
         MVC   JXRRDT2,TODAY2      DATE                                         
         GOTOR DATCON,DMCB,(2,JXRRDT2),(1,JXRRDT1)                              
                                                                                
RERUN11  LA    R2,DKEY             *CHECK TO SEE IF 99 POSTING ON FILE*         
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,JOBCDE     C/U/L/JOB                                    
         MVC   TRNKWORK,WCBILLQ                                                 
         GOTOR ADMGR,ACCHIQ                                                     
         J     RERUN15                                                          
                                                                                
RERUN13  GOTOR ADMGR,ACCSEQQ                                                    
RERUN15  LA    R2,DIR                                                           
         CLC   TRNKEY(TRNKCULC-TRNRECD),DKEY  TEST BILLING TRANSACTION          
         JNE   XITY                                                             
                                                                                
         GOTOR ADMGR,ACCGETQ       GET THE RECORD                               
         L     R2,AIO6                                                          
         CLI   TRNRFST,TRNELQ      TRANSACTION ELEMENT                          
         JNE   RERUN13                                                          
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         CLC   TRN2DAY,TODAY2      TEST ORIGINAL DATE                           
         JNE   RERUN13                                                          
         MVC   JXRRBDA,DA                                                       
         MVC   JXRRDT1,TRNDATE                                                  
         GOTOR DATCON,DMCB,(1,JXRRDT1),(2,JXRRDT2)                              
         J     XITY                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOAD OTHER PHASES                                        *         
***********************************************************************         
                                                                                
LOAD     NTR1  LABEL=*                                                          
                                                                                
         LARL  RF,RELOTAB          SET ADDRESSES FOR THIS OVERLAY               
         LHI   R0,RELOTABN                                                      
LOAD1    LLH   R1,4(RF)            R1=DISPLACEMENT IN CSECT                     
         LA    R1,NBILC(R1)                                                     
         MVC   0(4,R1),0(RF)       MOVE ADDRESS TO CSECT                        
         LA    RF,L'RELOTAB(RF)                                                 
         JCT   R0,LOAD1                                                         
                                                                                
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         LARL  R2,PHASE03                                                       
         USING PHSED,R2                                                         
                                                                                
LOAD3    MVC   MCDUB,PHSNME        LOAD IN PHASE                                
         GOTOR LOADM                                                            
                                                                                
LOAD5    MVC   PHSADR,DMCB+4       SAVE LOAD ADDRESS                            
         LA    R2,PHSLNQ(R2)       LOAD NEXT PHASE                              
         CLI   0(R2),EOT           TEST END OF TABLE                            
         JNE   LOAD3                                                            
                                                                                
         LARL  R2,PHASE02          RESOLVE ADDRESSES                            
         LARL  RF,ACNB02                                                        
         ST    RF,PHSADR           SET ADDRESS OF MYSELF                        
                                                                                
LOAD7    ICM   RE,15,PHSROU        LIST OF ROUTINES                             
         JZ    LOAD11              NONE                                         
         XR    R0,R0                                                            
                                                                                
LOAD9    AHI   R0,1                SET ADDRESSES FOR ROUTINES                   
         LLH   R1,0(RE)                                                         
         LA    R1,NBILC(R1)                                                     
         MVC   0(4,R1),PHSADR                                                   
         STC   R0,0(R1)                                                         
         LA    RE,2(RE)                                                         
         CLI   0(RE),EOT                                                        
         JNE   LOAD9                                                            
                                                                                
LOAD11   LA    R2,PHSLNQ(R2)       LOAD NEXT PHASE                              
         CLI   0(R2),EOT           TEST END OF TABLE                            
         JNE   LOAD7                                                            
         L     R5,ADMASTC                                                       
         LARL  R2,PHASE03                                                       
         MVC   MCAPHAS3,8(R2)                                                   
         LARL  R2,PHASE04                                                       
         MVC   MCAPHAS4,8(R2)                                                   
                                                                                
         LA    R2,CENTER           LOADABLE PHASES                              
         MVC   MCDUB,=CL8'T00AXX'                                               
LOAD13   GOTOR HEXOUT,DMCB,0(R2),MCDUB+4,1,0,0                                  
         GOTOR LOADM                                                            
         MVC   0(4,R2),DMCB+4                                                   
         LA    R2,4(R2)                                                         
         CLI   0(R2),0                                                          
         JNE   LOAD13                                                           
         J     XIT                                                              
                                                                                
LOADM    NTR1  LABEL=*                                                          
         GOTOR MCVLOADM,DMCB,0                                                  
         JE    *+6                                                              
         DC    H'0'                                                             
         LM    R0,R1,DMCB          R0=LENGTH,R1=START                           
         AR    R0,R1               R0=END                                       
         OC    PHSEBGN,PHSEBGN     FIRST PHASE?                                 
         JZ    *+12                                                             
         CLM   R1,15,PHSEBGN       TEST LOW PHASE                               
         JNL   *+8                                                              
         ST    R1,PHSEBGN          SAVE START OF FIRST PHASE                    
         CLM   R0,15,PHSEEND                                                    
         JNH   *+8                                                              
         ST    R0,PHSEEND          SAVE LAST PHASE END                          
         J     XITY                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR ACCOUNT                                                    *         
***********************************************************************         
                                                                                
AL00     NTR1  LABEL=*                                                          
         L     RC,0(R1)                                                         
         XC    SVBILNUM,SVBILNUM                                                
         LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         TM    REQOPT,REQERR        TEST ERROR                                  
         JO    XIT                                                              
         MVI   RETLSQN,0            RETAILER SEQUENCE                           
                                                                                
AL01     TM    JFLG,JFNONB          TEST ALREADY FOUND TO BE UNBILLABLE         
         JO    XIT                                                              
         CLC   NBTRN,NPRVB          ANY TRANSACTIONS(OTHER THAN BILLS)?         
         JNE   AL02                 YES, GO DO IT                               
         OC    NBTRN,NBTRN          TEST ANY ACTIVITY                           
         JNZ   AL01A                YES,                                        
         LHI   R0,AE$NAFRP          SET ERROR MESSAGE                           
         STCM  R0,3,JXERRS                                                      
         J     AL07                                                             
                                                                                
AL01A    TM    BILTTYP,TOTL         TOTAL BILL?                                 
         JO    AL02                 YES, BILL IT                                
         TM    BILTTYP,PROG         PROGRESSIVE?                                
         JNO   XIT                  SKIP IT.                                    
         CP    JXBALN,PZERO         TEST 'BAD JOB'                              
         JZ    XIT                  NO, OK TO SKIP                              
                                                                                
AL02     CLI   CTAXOPT,0           TEST CANADIAN                                
         JE    AL03                NO,                                          
         GOTOR CANTX,CANTOTQ       GET TOTAL GST/PST                            
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JNO   AL03                YES, GOT TAX FROM 99 POSTING                 
         LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         LA    R1,JXAALLO                                                       
CUR      USING JXBKD,R1                                                         
         LA    R2,JXATOT                                                        
TOT      USING JXBKD,R2                                                         
         LA    R6,CTOTGST$                                                      
         USING CTAXD,R6                                                         
         ZAP   DUB,CTAXTAX         SET GST                                      
         XI    DUB+L'DUB-1,X'01'                                                
         ZAP   CUR.JXBGST,DUB                                                   
         ZAP   TOT.JXBGST,DUB                                                   
                                                                                
         LA    R6,CTOTPST$                                                      
         ZAP   DUB,CTAXTAX         SET PST                                      
         XI    DUB+L'DUB-1,X'01'                                                
         ZAP   CUR.JXBPST,DUB                                                   
         ZAP   TOT.JXBPST,DUB                                                   
         DROP  CUR,TOT,R6                                                       
                                                                                
AL03     TM    BILTTYP,ESTM        TEST % ESTIMATE                              
         JNO   AL04                NO,                                          
         GOTOR PEST,PESTLQ         % OF ESTIMATE - LAST                         
         J     AL05                                                             
                                                                                
AL04     TM    BILTTYP,SPCL        TEST SPECIAL AMOUNT                          
         JNO   AL05                                                             
         GOTOR SPEC,SPECLQ         % OF ESTIMATE - LAST                         
                                                                                
AL05     MVI   JXACTN,JXAPACL      PROCESS ACCOUNT LAST                         
         GOTOR AJAX                                                             
         OC    JXERRS,JXERRS       TEST ANY ERRORS                              
         JNZ   AL07                YES,                                         
         CLI   ERRNUM,0                                                         
         JNE   XIT                 YES, SKIP IT                                 
         J     AL08                                                             
                                                                                
AL07     GOTOR AADDREP,NONBIL      ADD TO JOB NON-BILLABLE                      
         J     XIT                                                              
                                                                                
AL08     MVI   ASPIND,0            CLEAR ASP FLAG                               
         TM    JXASPIND,JXASPRTE   TEST JOB HAS ASP RATE                        
         JNO   AL09                                                             
         GOTOR ASPADD              GET ASP AMOUNT AND ADD TO TABLE              
                                                                                
AL09     TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JNO   AL11                NO,                                          
                                                                                
         LA    R3,JXAALLO          REVERSE AMOUNTS                              
         GOTOR SETNEG                                                           
         LA    R3,JXATOT                                                        
         GOTOR SETNEG                                                           
                                                                                
         XI    SK2SICOM+L'SK2SICOM-1,X'01'                                      
         XI    SK2SIGRS+L'SK2SIGRS-1,X'01'                                      
         XI    OOP+L'OOP-1,X'01'                                                
         XI    PSF+L'PSF-1,X'01'                                                
                                                                                
AL11     TM    JXSTAT1,JXS1DIST     TEST RETAIL                                 
         JO    ALR00                YES, SPECIAL RETAIL ROUTINES                
         TM    PGMAQ,PGMALL27       TEST A27 GROUP                              
         JNZ   AL13                 YES, SPECIAL ROUTINE                        
         LA    R4,JOCUR             SET 'CURRENT'                               
         LA    R3,JXAALLO                                                       
         TM    BILTTYP,TOTL+ONEL                                                
         JZ    *+8                                                              
         LA    R3,JXATOT                                                        
         GOTOR SETAMT               SET FIELDS(R4) FROM (R3)                    
         J     AL14                                                             
                                                                                
*                                  ** A27 ROUTINE **                            
AL13     LA    R4,JOCUR             SET 'CURRENT'                               
         LA    R3,JXAALLO                                                       
         GOTOR SETAMT               SET FIELDS(R4) FROM (R3)                    
                                                                                
         MVC   LVCBK,JOCUR          SET LEVEL C TOTALS FOR A27                  
         AP    LVCOOP,OOP           ADD SPECIAL A27 AMOUNTS                     
         AP    LVCPSF,PSF                                                       
         ZAP   LVCTOT,LVCNET        TOT=NET+COMM                                
         TM    OPT6,OPT6COWC        TEST COMMISSION IN WC                       
         JO    *+10                 YES                                         
         AP    LVCTOT,LVCCOM        NO, ADD IT NOW                              
         AP    LVCNET,LVCCSD        ADD CD TO NET                               
                                                                                
         GOTOR ADDBUK,DMCB,LVCBK,LVBBK                                          
                                                                                
         MVC   LVGBK,LVCBK         SET GROUP BUCKETS                            
         ZAP   CSD$,LVGCSD                                                      
         TM    AGLV,LVC            TEST ANY ACTIVITY ON THIS ACCOUNT            
         JZ    XIT                 NO, SKIP IT                                  
         MVI   PGLV,LVC            SET PROCESS LEVEL C FLAG                     
                                                                                
                                                                                
AL14     LA    R4,BTXT             SET 'BEFORE TAX TOTAL'                       
         LA    R3,JXATOT                                                        
         GOTOR SETAMT              SET FIELDS(R4) FROM (R3)                     
                                                                                
         LA    R4,BTXC             SET 'BEFORE TAX CURRENT'                     
         LA    R3,JXAALLO                                                       
         GOTOR SETAMT                                                           
                                                                                
         LA    R4,JOTOT            SET 'JOB TOTALS'                             
         LA    R3,JXATOT                                                        
         GOTOR SETAMT                                                           
                                                                                
         LA    R4,PBTOT            SET 'PREV. BILLS'                            
         LA    R3,JXABILL                                                       
         GOTOR SETAMT                                                           
                                                                                
         LA    R4,DUE              SET 'DUE'                                    
         LA    R3,JXAALLO                                                       
         GOTOR SETAMT                                                           
                                                                                
         TM    JXPBSTA,JXPBSPCT    TEST PERCENT OF BILL(POB)                    
         JNO   *+8                                                              
         GOTOR GETPOB                                                           
                                                                                
         TM    BILTTYP,PROG        TEST PROGRESSIVE                             
         JZ    AL15                NO,                                          
         CLC   NBTRN,NPRVB         TEST ONLY BILLS                              
         JE    XIT                 YES, SKIP IT                                 
                                                                                
         LA    R3,PBTOT            ADD PREV. BILLS                              
         LA    R4,BTXT             TO 'BEFORE TAX TOTAL'                        
         MVI   BYTE,C'+'                                                        
         GOTOR SUBPRV                                                           
         J     AL19                                                             
                                                                                
AL15     TM    BILTTYP,ESTM        TEST %EST                                    
         JO    AL17                                                             
         TM    PGMSTA,PGMUNB       TEST UNBILL                                  
         JO    AL19                YES, SIGN NOT CHANGED                        
         LA    R3,PBTOT            FIX SIGN ON PREV. BILL TOTAL                 
         GOTOR REVSIGN                                                          
         J     AL19                                                             
*                                  ** % ESTIMATE BILLS **                       
AL17     LA    R4,DUE              SUBTRACT FROM DUE                            
         LA    R3,PBTOT            PREVIOUS BILLS                               
         MVI   BYTE,C'-'                                                        
         GOTOR SUBPRV                                                           
                                                                                
         LA    R4,BTXC             SUBTRACT FROM BEFORE TAX                     
         LA    R3,PBTOT            PREVIOUS BILLS                               
         MVI   BYTE,C'-'                                                        
         GOTOR SUBPRV                                                           
                                                                                
         LA    R4,BTXT             SET 'BEFORE TAX TOTAL'                       
         LA    R3,JXAALLO                                                       
         GOTOR SETAMT                                                           
                                                                                
         TM    PGMSTA,PGMUNB       TEST UNBILL                                  
         JO    AL18                YES, SIGN NOT CHANGED                        
         LA    R3,PBTOT            PREVIOUS BILLS                               
         GOTOR REVSIGN             REVERSE SIGN                                 
                                                                                
AL18     CLI   CTAXOPT,0           TEST CANADIAN TAX                            
         JE    AL19                                                             
         AP    JOTOTNET,JOTOTTAX   ADD GST & PST                                
         AP    JOTOTGRS,JOTOTTAX                                                
         AP    JOCURNET,JOCURTAX                                                
         AP    JOCURGRS,JOCURTAX                                                
                                                                                
AL19     DS    0H                                                               
AL21     GOTOR PPOST               PRINT & POST                                 
         J     ALXIT                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR ACCOUNT - RETAIL                                           *         
***********************************************************************         
                                                                                
ALR00    LA    R4,JOCUR             SET 'CURRENT'                               
         LA    R3,JXATOT                                                        
         TM    BILTTYP,ESTM        TEST %EST                                    
         JNO   *+8                                                              
         LA    R3,JXAALLO                                                       
         GOTOR SETAMT               SET FIELDS(R4) FROM (R3)                    
                                                                                
         L     R5,JXARETLB                                                      
         USING RTLD,R5                                                          
ALR01    ST    R5,CRTLR            SET ADDRESS OF CURRENT RETAILER              
         CLI   0(R5),EOT           TEST EOT                                     
         JE    ALXIT               YES,                                         
                                                                                
         TM    RTLSTAT,RTLCURR     TEST CURRENT ALLOCATION                      
         JNO   ALR23               NO, SKIP ENTRY                               
         ZAP   SHARPCT,RTLPCT                                                   
                                                                                
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JNO   *+10                NO,                                          
         MVC   RTLNET,RTLOBA       YES, USE ORIG. BILL AMOUNTS                  
                                                                                
         CLI   CTAXOPT,0           TEST CANADIAN                                
         JE    ALR08               NO,                                          
         USING JXBKD,R3                                                         
         LA    R3,RTLNET           ADJUST NET LINE FOR TAXES                    
         ZAP   JXBGST,PZERO                                                     
         ZAP   JXBPST,PZERO                                                     
         L     R6,AGSTBUF          GET SHARE OF GST BY TYPE                     
         USING CTDLD,R6                                                         
         MVI   BYTE,C'G'           SET GST FLAG                                 
ALR03    CLI   0(R6),0             TEST EOT                                     
         JE    ALR04               YES,                                         
         LA    R4,CTDLTXB                                                       
         USING CTAXD,R4                                                         
         GOTOR TAXSHR              GET SHARE FOR EACH TYPE                      
         LA    RF,JXBGST                                                        
         CLI   BYTE,C'G'                                                        
         JE    *+8                                                              
         LA    RF,JXBPST                                                        
         AP    0(L'JXBGST,RF),CTAXRTX                                           
         LA    R6,CTDLLNQ(R6)                                                   
         J     ALR03                                                            
                                                                                
ALR04    CLI   BYTE,C'P'           TEST ALREADY DID PCT                         
         JE    ALR05               YES                                          
         MVI   BYTE,C'P'                                                        
         L     R6,APSTBUF          GET SHARE OF PST BY TYPE                     
         J     ALR03                                                            
                                                                                
ALR05    GOTOR CANTX,CANTOTQ       RESET PROVINCE TOTALS                        
         DROP  R3,R4,R6                                                         
                                                                                
ALR08    LA    R3,RTLCHG                                                        
         TM    BILTTYP,PROG                                                     
         JZ    *+8                                                              
         LA    R3,RTLNET           YES, SHARE OF NET TOTALS                     
         LA    R4,SHAR                                                          
         GOTOR SETAMT              SET AMOUNTS FOR RETAILER                     
                                                                                
         LA    R3,RTLPRV           SET RETAILER PREV. BILLS                     
         LA    R4,PBTOT                                                         
         GOTOR SETAMT                                                           
                                                                                
         LA    R3,RTLNET           SET RETAILER DUE AMOUNTS                     
         LA    R4,DUE                                                           
         GOTOR SETAMT                                                           
                                                                                
         LA    R3,RTLNET                                                        
         LA    R4,BTXC             SET 'BEFORE TAX CURRENT'                     
         GOTOR SETAMT                                                           
                                                                                
         TM    BILTTYP,ESTM        TEST %EST                                    
         JZ    ALR09                                                            
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JO    ALR09                                                            
         LA    R4,DUE              SUBTRACT FROM DUE                            
         LA    R3,PBTOT            PREVIOUS BILLS                               
         MVI   BYTE,C'-'                                                        
         GOTOR SUBPRV                                                           
*                                  RESTORE NET TAX $                            
         ZAP   DUEGST,RTLNET+(JXBGST-JXBKD)(L'JXBGST)                           
         ZAP   DUEPST,RTLNET+(JXBPST-JXBKD)(L'JXBPST)                           
                                                                                
         LA    R4,DUE                                                           
         GOTOR SETRCV              RECALCULATE DUE                              
                                                                                
         LA    R4,BTXC             SUBTRACT FROM BEFORE TAX                     
         LA    R3,PBTOT            PREVIOUS BILLS                               
         MVI   BYTE,C'-'                                                        
         GOTOR SUBPRV                                                           
                                                                                
ALR09    TM    BILTTYP,PROG        TEST PROGRESSIVE                             
         JO    ALR11               YES                                          
         LA    R3,PBTOT                                                         
         GOTOR REVSIGN             NO, REVERSE SIGN ON PREV BILL TOTAL          
                                                                                
ALR11    LA    R3,RTLCHG           JOB TOTAL BEFORE TAX                         
         LA    R4,BTXT                                                          
         GOTOR SETAMT                                                           
                                                                                
         LA    R3,RTLCHG           JOB TOTAL - AFTER TAX                        
         LA    R4,JOTOT                                                         
         GOTOR SETAMT                                                           
                                                                                
         LA    R3,PBTOT            ADD PREV. BILLS                              
         LA    R4,BTXT             TO JOB TOTAL                                 
         MVI   BYTE,C'+'                                                        
         GOTOR SUBPRV                                                           
                                                                                
         AP    JOTOTNET,JOTOTTAX   ADD GST & PST                                
         AP    JOTOTGRS,JOTOTTAX                                                
                                                                                
         TM    JXBLTYP,TOTL+ONEL+SPCL                                           
         JZ    ALR16                                                            
         TM    OPTA,OPTARJOB       TEST DEMAND TOTAL BILL RETAIL 'R'            
         JO    ALR21               YES, SKIP THE TEST                           
                                                                                
ALR16    CP    DUENET,PZERO                                                     
         JNE   ALR21                                                            
         CP    DUECOM,PZERO                                                     
         JNE   ALR21                                                            
         CP    DUEGRS,PZERO                                                     
         JNE   ALR21                                                            
         CP    DUEGST,PZERO                                                     
         JNE   ALR21                                                            
         CP    DUEPST,PZERO                                                     
         JNE   ALR21                                                            
         J     ALR23               ZERO BILL - SKIP IT                          
                                                                                
ALR21    GOTOR PPOST               PRINT & POST BILLS                           
                                                                                
         LLC   R0,RETLSQN          INCREMENT SEQUENCE NUMBER                    
         AHI   R0,1                                                             
         STC   R0,RETLSQN                                                       
                                                                                
         CLI   RETLSQN,1           TEST FIRST RETAIL BILL                       
         JNE   *+10                NO,                                          
         MVC   SVBILNUM,BILNUM     YES,  SAVE FIRST BILL NUMBER                 
                                                                                
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JNO   ALR23               NO,                                          
         PACK  DUB,JXRRNUM+2(4)    INCREMENT OLD NUMBER                         
         AP    DUB,PONE                                                         
         OI    DUB+7,X'0F'                                                      
         UNPK  JXRRNUM+2(4),DUB                                                 
                                                                                
ALR23    LA    R5,RTLLNQ(R5)       NEXT RETAIL ENTRY                            
         J     ALR01                                                            
*                                  * EXIT FOR ACCLAST *                         
ALXIT    TM    PGMSTA,PGMDFT       TEST DRAFT                                   
         JO    ALXIT3                                                           
         GOTOR UPDFL,DMCB,ACWORKD  UPDATE THE FILE                              
                                                                                
ALXIT3   TM    UPSI,UPDMG          TRACE DM CALLS                               
         JO    IOXIT               YES, PRINT IO COUNT AT EXIT                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET RETAILER SHARE OF EACH TAX LINE                                 *         
***********************************************************************         
                                                                                
         USING CTAXD,R4                                                         
TAXSHR   LA    R0,CTAXRTN          CLEAR RETAILER BUXKETS                       
         LA    RF,CTAXRTL                                                       
         ZAP   0(BKLQ,RF),PZERO                                                 
         LA    RF,BKLQ(RF)                                                      
         JCT   R0,*-10                                                          
                                                                                
         ZAP   PL16,CTAXTAX        TAX                                          
         MP    PL16,RTLPCT         X PERCENT                                    
         SRP   PL16,64-6,5         ROUNDED                                      
         ZAP   CTAXRTX,PL16        RETAILER TAX                                 
                                                                                
         ZAP   PL16,CTAXCOM        COM                                          
         MP    PL16,RTLPCT         X PERCENT                                    
         SRP   PL16,64-6,5         ROUNDED                                      
         ZAP   CTAXRTC,PL16        RETAILER COM                                 
                                                                                
         ZAP   PL16,CTAXGRS        TOTAL GROSS                                  
         SP    PL16,CTAXTAX         LESS TOTAL TAX                              
         MP    PL16,RTLPCT         X PERCENT                                    
         SRP   PL16,64-6,5         ROUNDED=RETAIL BASIS                         
         ZAP   CTAXRTG,PL16        RETAILER BASIS                               
         AP    CTAXRTG,CTAXRTX      PLUS RETAIL TAX=RETAIL GROSS                
                                                                                
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT BILLS & REPORTS - CALL POST ROUTINE                           *         
***********************************************************************         
                                                                                
PPOST    NTR1  LABEL=*                                                          
         TM    REQOPT,REQGRPBN     SET 'HAVE GROUP BILL NUMBER'                 
         JO    PPOST02                                                          
         LA    R3,10               DUE IN 10 DAYS IS DEFAULT                    
         L     R6,ADGOBLOC                                                      
         USING GOBLOCKD,R6                                                      
         CLI   GODUEDAY,0          ANY DAYS IN GOBLOCK?                         
         JE    *+8                                                              
         IC    R3,GODUEDAY                                                      
                                                                                
         TM    PGMAQ,PGMALL27      TEST A27                                     
         JNZ   PPOST01             YES, SKIP DAYS IN REQUEST                    
         CLI   QBDAY,0             ANY DAYS IN REQUEST?                         
         JE    PPOST01                                                          
         CLI   QBDAY,X'FF'                                                      
         JE    PPOST01                                                          
         IC    R3,QBDAY                                                         
                                                                                
PPOST01  GOTOR ADDAY,DMCB,BILDT0,DUEDT0,(R3)                                    
         GOTOR DATCON,DMCB,(0,DUEDT0),(2,DUEDT2)                                
         GOTOR DATCON,DMCB,(0,DUEDT0),(1,DUEDT1)                                
                                                                                
PPOST02  XC    SHPDT0,SHPDT0                                                    
         XC    SHPDT2,SHPDT2                                                    
         CLI   OPTSHIPD,0          TEST ANY SHIP DATE                           
         JE    PPOST03                                                          
         LLC   RF,OPTSHIPD         NUMBER OF DAYS BEFORE DUE DATE               
         LNR   RF,RF                                                            
         GOTOR ADDAY,DMCB,DUEDT0,SHPDT0,(RF)                                    
         GOTOR DATCON,DMCB,(0,SHPDT0),(2,SHPDT2)                                
         DROP  R6                                                               
                                                                                
PPOST03  MVC   LNGBILNO(1),MEDIA   EVEN FOR GROUP BILL CHANGE MEDIA             
         TM    REQOPT,REQGRP+REQGRPBN TEST ALREADY HAVE GROUP BILL              
         JO    PPOST04                                                          
         GOTOR GTBNUM              GET A BILL NUMBER                            
                                                                                
PPOST04  TM    REQOPT,REQGRP                                                    
         JNO   *+8                                                              
         OI    REQOPT,REQGRPBN     SET 'HAVE GROUP BILL NUMBER'                 
         OI    REQOPT,REQPRT       SET 'BILL(S) HAVE BEEN PRINTED'              
                                                                                
         GOTOR AADDREP,CLISUM      ADD RECORD FOR CLIENT SUMMARY                
         GOTOR AADDREP,PRDSUM                     PRODUCT SUMMARY               
         GOTOR AADDREP,BILREG                     BILLING REGISTER              
         GOTOR AADDREP,MEDSUM                     MEDIA SUMMARY                 
         GOTOR AADDREP,TARGET                     TARGET REGISTER               
         GOTOR AADDREP,INTINC                     INTERNAL INCOME REG.          
         TM    AGYSTAT,AGYSICMP    WANT INTERCOMPANY POSTINGS?                  
         JNO   PPOST04A                                                         
         GOTOR AADDREP,INPEXC      IC. EXCEPT.REPORT                            
                                                                                
PPOST04A TM    JXASPIND,JXASPRTE   TEST JOB HAS ASP RATE                        
         JNO   PPOST05                                                          
         GOTOR AADDREP,ASPREG      ADD ASP SUMMARY RECORDS                      
                                                                                
PPOST05  TM    JXPBSTA,JXPBSPCT    TEST PERCENT OF BILL                         
         JNO   PPOST06                                                          
         GOTOR AADDREP,POBREG      % OF BILL REGISTER                           
                                                                                
PPOST06  TM    PGMAQ,PGMALL21      TEST 21                                      
         JZ    PPOST07             NO, MUST BE 27                               
         TM    REQOPT,REQGRP       TEST GROUP BILL                              
         JNO   PPOST09                                                          
         GOTOR AADDREP,GRPSUM      ADD GROUP SUMMARIES                          
         J     PPOST09                                                          
                                                                                
PPOST07  TM    PGMSTA,PGMLIV       TEST LIVE 27                                 
         JZ    PPOST09             NO, SKIP TRACE                               
         GOTOR AADDREP,GBTRAS      ADD GROUP BILL TRACE SUMMARY                 
                                                                                
PPOST09  GOTOR APOST               GET TOTALS & POST                            
         MVC   CSD$,JOTOTCSD       SET CD FLAG                                  
         TM    RUNOPT,RUNTST       RUN=TEST?                                    
         JNO   PPOST11             NO, LIVE RUN DON'T DIE                       
         CLI   RCPOSTNG,YES        TEST POSTINGS=YES                            
         JNE   PPOST11             NO, SKIP TEST                                
         CP    POSTDR,POSTCR                                                    
         JE    PPOST11                                                          
         DC    H'0'                                                             
                                                                                
PPOST11  GOTOR ABFMT               FORMAT THE BILL                              
         J     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* SET AMOUNT FIELDS (R4) FROM JOB TOTALS (R3)                         *         
***********************************************************************         
                                                                                
         USING JXBKD,R3                                                         
         USING BK$,R4                                                           
SETAMT   ZAP   NET$,JXBNET                                                      
         ZAP   COM$,JXBCOM                                                      
         ZAP   GRS$,JXBGRS                                                      
         ZAP   CSD$,JXBCSD                                                      
         ZAP   MED$,JXBMED                                                      
         ZAP   GST$,JXBGST                                                      
         ZAP   PST$,JXBPST                                                      
                                                                                
SETRCV   ZAP   PAY$,NET$           PAY=NET + COM                                
         TM    JXSTAT3,JXS3PNET    TEST PAY=NET                                 
         JO    *+10                YES, DON'T ADD COMMISION                     
         AP    PAY$,COM$                                                        
         ZAP   TAX$,GST$           TAX=GST + PST                                
         AP    TAX$,PST$                                                        
         ZAP   NTX$,NET$           NTX=NET + TAX                                
         AP    NTX$,TAX$                                                        
         ZAP   RCV$,GRS$           RCV=GROSS + TAX                              
         AP    RCV$,TAX$                                                        
         BR    RE                                                               
         DROP  R3,R4                                                            
                                                                                
SUBPRV   STM   RE,R7,12(RD)        SUB/ADD  R3 FROM R4                          
         LA    R0,BK$N                                                          
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JZ    SUBPRV1             NO,                                          
         CLI   BYTE,C'-'           YES, SWITCH THE SIGN                         
         JE    *+12                                                             
         MVI   BYTE,C'-'                                                        
         J     SUBPRV1                                                          
         MVI   BYTE,C'+'                                                        
SUBPRV1  CLI   BYTE,C'+'                                                        
         JNE   SUBPRV5                                                          
         AP    0(L'BK$,R4),0(L'BK$,R3)   YES, ADD PREVIOUS                      
         J     SUBPRV7                                                          
SUBPRV5  SP    0(L'BK$,R4),0(L'BK$,R3)   NO, SUBTRACT PREVIOUS                  
SUBPRV7  LA    R3,L'BK$(R3)                                                     
         LA    R4,L'BK$(R4)                                                     
         JCT   R0,SUBPRV1                                                       
         LM    RE,R7,12(RD)                                                     
         BR    RE                                                               
                                                                                
REVSIGN  STM   RE,R7,12(RD)        REVERSE SIGN OF PBTOT                        
         LA    R0,BK$N             SUBTRACT R3 FROM R4                          
REVSIGN2 XI    L'BK$-1(R3),X'01'   REVERSE SIGN                                 
         LA    R3,L'BK$(R3)                                                     
         JCT   R0,REVSIGN2                                                      
         LM    RE,R7,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET 'PERCENT OF BILL' AMOUNT                                        *         
***********************************************************************         
                                                                                
GETPOB   NTR1  LABEL=*                                                          
         MVC   POBACC,JXPBACC      POB VENDOR ACCOUNT                           
         MVC   ACCCDE,POBACC       GET ACCOUNT NAME                             
         GOTOR AGETNAM                                                          
         JNE   ERACNF              SET 'ACCOUNT NOT FOUND' ERROR                
         TM    ACCSTA,ACCSBL       TEST ACCOUNT HAS BALANCE ELEMENT             
         JNO   ERACNB              NO,                                          
         MVC   POBACN,ACCNAM                                                    
         MVC   POBWRK,JXPBWRK      SAVE WORKCODE                                
         MVC   WRKCCUR,POBWRK                                                   
         GOTOR AWRKM,WRKMUPQ       GET WORKCODE DETAILS                         
         MVC   POBNAM(L'WRKCDSC),WRKCDSC    W/C NAME                            
         TM    OPT3,OPT3PWCL                                                    
         JNO   *+10                                                             
         MVC   POBNAM,WRKCNME      W/C LONG NAME                                
                                                                                
         TM    PGMSTA,PGMUNB       IS IT UNBILLING?                             
         JO    XIT                 YES, AMOUNT ALREADY IN WC LIST               
                                                                                
         LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         ZAP   PL13,JXPBPCT        % OF BILL                                    
         ZAP   DUB,JOCURNET                                                     
         SP    DUB,POBPRV          LESS: PREVIOUS POB                           
         MP    PL13,DUB                                                         
         SRP   PL13,64-8,5                                                      
         ZAP   POBAMT,PL13         TOTAL POB                                    
         ZAP   POBCUR,POBAMT                                                    
         SP    POBCUR,POBPRV                                                    
         AP    DUENET,POBCUR                                                    
         AP    DUEGRS,POBCUR                                                    
         AP    DUERCV,POBCUR                                                    
                                                                                
         TM    BILTTYP,PROG        TEST PROGRESSIVE                             
         JZ    XIT                 NO,                                          
         AP    JOTOTNET,POBCUR     ADD POB TO TOTALS                            
         AP    JOTOTGRS,POBCUR                                                  
         J     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* GET ASP AMOUNT                                                      *         
***********************************************************************         
                                                                                
ASPADD   NTR1  LABEL=*                                                          
         TM    JXASPIND,JXASPBIL                                                
         JNO   *+8                                                              
         OI    ASPIND,ASPIBIL      SET BILL THIS TIME                           
         TM    ASPIND,ASPIBIL                                                   
         JO    *+8                                                              
         OI    ASPIND,ASPINOT      SET NOT BILLED THIS TIME                     
                                                                                
         MVC   ASPWRK,JXASPWRK                                                  
         MVC   ASPACC,JXASPACC                                                  
                                                                                
         MVC   ACCCDE,ASPACC       GET ACCOUNT NAME                             
         GOTOR AGETNAM                                                          
         JNE   ERACNF              SET 'ACCOUNT NOT FOUND' ERROR                
         TM    ACCSTA,ACCSBL       TEST ACCOUNT HAS BALANCE ELEMENT             
         JNO   ERACNB              NO,                                          
         MVC   ASPACN,ACCNAM                                                    
                                                                                
         MVC   ANLINA,ASPACC       SET SI ACCOUNT                               
         MVC   ANLINA+1(2),=C'SI'  MAKE SURE IT'S SI NOT SK                     
         GOTOR AANAL               GET ANALYSIS ACCOUNT FOR THIS SI             
         MVC   ASPRVA,ANLRVA       REVENUE ANALYSIS(UL=12)                      
         MVC   ASPRVN,ANLRVN       REVENUE ANALYSIS NAME                        
                                                                                
         MVC   WRKCCUR,ASPWRK                                                   
         GOTOR AWRKM,WRKMUPQ       GET WORKCODE DETAILS                         
         MVC   ASPNAM(L'WRKCDSC),WRKCDSC    W/C NAME                            
         TM    OPT3,OPT3PWCL                                                    
         JNO   *+10                                                             
         MVC   ASPNAM,WRKCNME      W/C LONG NAME                                
                                                                                
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JO    XIT                 YES, ALREADY HAVE ASP AMOUNT                 
                                                                                
         ZAP   PL13,PSF            PSF=TIME + RETAINER                          
         MP    PL13,JXASPPCT       X %                                          
         SRP   PL13,64-4,5                                                      
         ZAP   ASPAMT,PL13         TOTAL ASP                                    
                                                                                
ASPADD3  TM    ASPIND,ASPIBIL      TEST BILL THIS TIME                          
         JNO   XIT                 NO, DON'T ADD TO TABLE                       
                                                                                
         L     R4,ABTRN            ADD TO TRANSACTION TABLE                     
         ICM   R1,15,NBTRN                                                      
         MHI   R1,BTRNLQ           R1=LENGTH OF TABLE                           
         AR    R4,R1                                                            
         ST    R4,CBTRN            SAVE ADDRESS OF CURRENT ENTRY                
         USING BTRND,R4            ADD ITEM TO BILLABLE TABLE                   
                                                                                
         XC    BTRND(BTRNLQ),BTRND                                              
                                                                                
         MVC   BTRNWC,JXASPWRK     WORKCODE                                     
         MVC   BTRNCA,JXASPACC     CONTRA                                       
         MVC   BTRNDTE,TODAY1      DATE                                         
         MVI   BTRNPORM,BTRNPROD   DEFAULT IS PRODUCTION                        
         MVI   BTRNWCTY,WRKCTRET                                                
         ZAP   BTRNBILR,PZERO      CLEAR TRANSACTION ACCUMS                     
         ZAP   BTRNCOMR,PZERO                                                   
         ZAP   BTRNUNTS,PZERO                                                   
         ZAP   BTRNUPRC,PZERO                                                   
                                                                                
         LA    R6,BTRNBK                                                        
         USING JOBD,R6                                                          
         LA    R0,BTBKN            CLEAR BUCKETS                                
         LA    RE,BTRNBK                                                        
         ZAP   0(BKLQ,RE),PZERO                                                 
         LA    RE,BKLQ(RE)                                                      
         JCT   R0,*-10                                                          
                                                                                
         ZAP   JOBNET,ASPAMT                                                    
         ZAP   JOBGRS,ASPAMT                                                    
         ICM   R1,15,NBTRN         ADD TO ITEM IN TABLE                         
         CHI   R1,MXBTRN           TEST MAX                                     
         JNL   ER2MANY                                                          
         AHI   R1,1                                                             
         STCM  R1,15,NBTRN                                                      
                                                                                
         LA    R3,JXAALLO                                                       
         USING JXBKD,R3                                                         
         AP    JXBNET,ASPAMT       ADD ASP AMOUNT TO NET                        
         AP    JXBGRS,ASPAMT       AND GROSS                                    
         AP    PSF,ASPAMT          ADD BACK TO PSF                              
         J     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* LAST FOR RUN                                                        *         
***********************************************************************         
                                                                                
RNL      NTR1  LABEL=*                                                          
         GOTOR APOST               CLOSE POSTING FILE                           
                                                                                
         TM    OPT2,OPT2SXRD       SUPPRESS EXTRA REQUEST DETAIL?               
         JO    RNL01                                                            
         L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         XC    SRLEN(SRLNQ),SRLEN  REQUEST PAGE FOR END OF RUN                  
         LLH   RF,REQNUM                                                        
         AHI   RF,1                                                             
         STCM  RF,3,SKRQN                                                       
         MVI   SKRQRT,SKRQRDQ      REQUEST DETAILS                              
         MVI   SRDATA,C' '                                                      
         MVC   SRDATA+1((L'QRECORD*3)-1),SRDATA                                 
         MVC   SRDATA+1+((L'QRECORD*3)-1)((L'QRECORD)-1),SRDATA                 
         MVC   SRDATA(L'QPROG),QPROG                                            
         MVC   SRDATA+(QUESTOR-QRECORD)(11),=C'INVOICE REG'                     
         MVI   SRDATA+(QCOMPANY-QRECORD),C' '                                   
         LA    RF,SRLNQ+L'QRECORD*3(RF)                                         
         STCM  RF,3,SRLEN                                                       
         GOTOR ASORT,SORTPUTQ                                                   
                                                                                
RNL01    MVI   RCREQREP,YES        SET TO PRINT REQUEST DETAILS                 
         L     R3,ASRTWK                                                        
         XC    SRLEN(SRLNQ),SRLEN                                               
         MVC   SKRUN,=AL2(SKRUNQ)  RUN REPORT                                   
         MVI   SKRNRT,SKRNENQ      END OF RUN RECORD                            
         MVC   SRDATA(L'ENDREMSG),ENDREMSG                                      
         LA    R5,SRLNQ+L'ENDREMSG                                              
         STCM  R5,3,SRLEN                                                       
         GOTOR ASORT,SORTPUTQ                                                   
                                                                                
RNL03    GOTOR ASORT,SORTGETQ                                                   
         JNE   RNL11                                                            
RNL04    CLC   SKRUN,=AL2(SKRUNQ)  TEST END OF RUN RECORDS                      
         JE    RNL07                                                            
                                                                                
RNL05    CLI   SKRQRT,SKRQRDQ      TEST REQUEST DETAILS                         
         JNE   RNL06                                                            
         GOTOR REQDT               PRINT THE REQUEST DETAILS                    
         J     RNL03                                                            
                                                                                
RNL06    CLI   SKRQRT,SKRQGSQ      TEST 27 GROUP "SUMMARIES"                    
         JE    *+12                SKIP IT                                      
         CLI   SKRQRT,SKRQBLQ      TEST REPORT "BILLS" OR "SUMMARIES"           
         JNE   RNL03               SKIP IT                                      
         CLI   CPSKSUM,CPSKSUMQ    TEST CLIENT/ PRODUCT SUMMARY                 
         JE    RNL07               YES, PRINT SUMMARY                           
         GOTOR ABPRT               NO, PRINT BILLS                              
         JE    RNL03               PROCESSED RECORD(GET NEXT)                   
         J     RNL04               CHECK RECORD TYPE                            
                                                                                
RNL07    GOTOR ARUNREP             END OF RUN REPORTS                           
         CLI   SKRNRT,SKRNENQ      TEST END OF RUN RECORD                       
         JNE   RNL05                                                            
                                                                                
RNL11    GOTOR ASORT,SORTENDQ      END SORT                                     
         L     R4,LOGOC                                                         
         USING LOGOD,R4                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTOR LOGO,DMCB,LOGOD                                                  
                                                                                
         L     R5,ADMASTC                                                       
         USING MASTD,R5                                                         
         MVC   USID,MCUSERID       SAVE USER ID                                 
         MVC   PQID,MCREMPQK       AND PQ ID                                    
                                                                                
         TM    WKFOPT,WKFPRTW      PRINT WORKER FILE?                           
         JNO   RNL15                                                            
         MVC   MCDUB,=CL8'ACWK02'                                               
         GOTOR MCVLOADM,DMCB,0     GET PRINT MODULE                             
         JE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,4(R1)        SAVE ADDRESS OF OVERLAY                       
         SR    R0,R0                                                            
         TM    WKFOPT,WKFPURW     PURGE FILE                                    
         JNO   *+8                                                              
         LA    R0,1                                                             
         GOTOR (RF),DMCB,(X'80',ACWORKD),((R0),WKID)                            
                                                                                
RNL15    GOTOR PRINT,DMCB,=C'CLOSE'                                             
         TM    WKFOPT,WKFPURW      PURGE FILE                                   
         JO    RNL25                                                            
         TM    PGMSTA,PGMDFT       TEST DRAFT                                   
         JO    RNL25               YES,                                         
                                                                                
         GOTOR ACTRACK,DMCB,ACWORKD,POSTDR,MARKED,POSTRECS,WKID                 
         ORG   *-2                                                              
         TM    RUNOPT,RUNTST       RUN=TEST?                                    
         JNO   *+8                                                              
         OI    0(R1),X'40'         YES, TELL ACTRACK                            
         TM    SOONS,SOONRUN       SOON RUN?                                    
         JZ    *+8                                                              
         OI    0(R1),X'20'         YES, TELL ACTRACK                            
         BASR  RE,RF                                                            
                                                                                
         XC    HEADHOOK,HEADHOOK                                                
         MVC   SVPARM,RCFFPARM                                                  
         TM    SOONS,SOONRUN       TEST SOON RUN                                
         JO    RNL17               YES                                          
                                                                                
*                                  ** OVERNIGHT **                              
         TM    WKFOPT,WKFNONE      TEST ANY WORKER FILE                         
         JO    RNL25               NONE                                         
         XC    RCFFPARM,RCFFPARM                                                
         GOTOR APOSTWRK,DMCB,ACWORKD,(C'R',WKID)                                
         MVC   RCFFPARM,SVPARM                                                  
         TM    WRIT,WRITMED                                                     
         JNO   *+8                                                              
         MVI   MODE,WRITMEDS                                                    
         NI    WRIT,ALL-(WRITMED)                                               
         J     RNL25                                                            
                                                                                
*                                  ** SOON **                                   
RNL17    CLI   RCPOSTNG,NO         TEST POSTING=NO                              
         JE    RNL21               YES, SKIP UPDATE                             
         TM    WKFOPT,WKFNONE      TEST ANY WORKER FILE(NO DATA)                
         JNO   RNL21               YES, THERE IS A POSTING FILE                 
         TM    LOCKS,LOCKACC       TEST ACCOUNT STILL LOCKED                    
         JNO   RNL21               NO,                                          
         LA    R2,DKEY             UNLOCK IF ZERO RUN                           
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,JOBCDE     C/U/L/JOB                                    
         GOTOR ADMGR,ACCRDQ                                                     
         MVC   AIO,AIO1                                                         
         GOTOR DMGR,ACCGETQ                                                     
         L     R2,AIO                                                           
         MVI   ELCODE,LGLELQ       YES, GET LOCK ELEMENT                        
         GOTOR GETELN                                                           
         JE    *+6                                                              
         DC    H'0'                NO LOCK ELEMENT                              
                                                                                
         USING LGLELD,R2                                                        
         NI    LGLSTAT,ALL-LGLSLOCK                                             
         NI    LOCKS,ALL-LOCKACC                                                
         GOTOR DMGR,ACCPUTQ        AND PUT ACCMST                               
         J     RNL21                                                            
         DROP  R2                                                               
                                                                                
RNL21    TM    WRIT,WRITLDG        TEST NEED TO WRITE LEDGER                    
         JNO   RNL23                                                            
         L     R4,ADLEDGER                                                      
         MVC   DKEY,0(R4)                                                       
         GOTOR DMGR,ACCTRDQ        READ THE LEDGER RECORD                       
         L     R4,AIO                                                           
         MVC   DUB,LDGRLEN-LDGRECD(R4)                                          
         MVC   AIO,ADLEDGER                                                     
         L     R4,AIO                                                           
         AH    R4,DUB                                                           
         BCTR  R4,0                                                             
         MVI   0(R4),0                                                          
         GOTOR DMGR,ACCTWRTQ       WRITE THE LEDGER RECORD                      
                                                                                
RNL23    XC    WKID,WKID           DON'T PASS WORKER ID TO SRUD00               
         GOTOR FACUSER             ADD USER= INFO                               
         GOTOR FACLAST             ADD LAST=                                    
         GOTOR FACSMF              ADD SMF=                                     
                                                                                
         L     RF,AMONACC                                                       
         TM    WKFOPT,WKFKEEP      KEEP FACWK                                   
         JNO   RNL25               NO,                                          
         OI    ACMINDS7-ACMD(RF),ACMIKPFW   'KEEP' FACWRK FILE                  
         TM    WKFOPT,WKFKEPT      TEST WORKER FILE ALREADY ON KEEP             
         JO    RNL25                                                            
         GOTOR AWRKF,WRKKEPQ       "KEEP" WORKER FILE                           
                                                                                
RNL25    LM    R0,R1,LOWLEN                                                     
         FREEMAIN R,LV=(0),A=(1)                                                
         J     XIT                 ALL DONE                                     
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT REQUEST DETAILS PAGE                                          *         
***********************************************************************         
                                                                                
REQDT    NTR1  LABEL=*                                                          
         MVI   RCSUBPRG,0                                                       
         NI    UPSI,ALL-(UPDMG)                                                 
         L     R5,ADMASTC          WRITE REQUEST DETAILS TO STACK               
         USING MASTD,R5                                                         
                                                                                
         L     R0,ADQSTACK         CLEAR REQUEST STACK TO SPACES                
         LA    R1,MCRQCOLS*L'MCREQREC                                           
         SR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R3,ASRTWK                                                        
         USING SORTD,R3                                                         
         LLH   R1,SRLEN                                                         
         SHI   R1,SRLNQ                                                         
         LR    RF,R1                                                            
         STH   RF,HALF                                                          
         L     R0,ADQSTACK                                                      
         LA    RE,SRDATA                                                        
         MVCL  R0,RE               MOVE REQUEST CARDS TO STACK                  
                                                                                
         MVC   QRECORD,SPACES                                                   
         MVC   QRECORD2,SPACES                                                  
         L     RF,ADQSTACK                                                      
         MVC   QRECORD,0(RF)       RESTORE FIRST                                
         LH    R1,HALF                                                          
         SHI   R1,L'MCREQREC       REDUCE LENGTH FOR FIRST                      
         JNP   *+10                AND EXIT IF THERE'S ONLY ONE                 
         MVC   QRECORD2,L'MCREQREC(RF) RESTORE SECOND                           
                                                                                
******* TEMP TO MATCH OLD 21 *******                                            
         CLC   QUESTOR(11),=C'INVOICE REG'                                      
         JE    *+10                                                             
***********************************                                             
                                                                                
         MVC   QCOMPANY,RCCOMPFL                                                
         MVI   FORCEHED,YES                                                     
         MVI   SKIPSPEC,C'R'                                                    
         GOTOR ACREPORT            PRINT THE DETAILS                            
         MVI   FORCEHED,YES                                                     
         MVC   UPSI,MCUPSI         RESTORE UPSI SWITCHES                        
                                                                                
         TM    RUNOPT,RUNARCH      TEST USING ARCHIVE                           
         JNO   XIT                                                              
         TM    SOONS,SOONRUN       TEST 'SOON RUN'                              
         JO    XIT                                                              
         TM    SRSTAT,SRPRNT       TEST ANY BILLS PRINTED                       
         JO    XIT                                                              
         L     RF,ADICO                                                         
         USING DICD,RF                                                          
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   P(L'DD@REQDE),DD@REQDE                                           
         DROP  RF                                                               
         GOTOR HEXOUT,DMCB,QCOMPANY,PSECOND,1,=C'TOG',2                         
         MVC   PSECOND+3(L'QRECORD),QRECORD                                     
         MVI   PSECOND+3+(QCOMPANY-QRECORD),C' '                                
         MVC   PTHIRD(L'QRECORD2),QRECORD2                                      
         GOTOR ACREPORT                                                         
         MVC   P(22),=C'**NO BILLING GENERATED'                                 
         GOTOR ACREPORT                                                         
         J     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE FACWK RECORDS FOR SOON UPDATES                                *         
***********************************************************************         
                                                                                
         USING FWRECD,R3                                                        
FACUSER  NTR1  LABEL=*                                                          
         XC    FWREC,FWREC                                                      
         LA    R3,FWREC                                                         
         MVC   FWRLEN,=Y(FWRLNQ)   USER INFO FOR SRUPD00                        
         MVC   FWRUSER,=C'USER='                                                
         MVC   FWRUSID,USID                                                     
         MVC   FWRLUID,LUID                                                     
         MVC   FWRPQID,PQID                                                     
         MVC   FWRWKID,WKID                                                     
         TM    RUNOPT,RUNTST       RUN=TEST?                                    
         JO    FACUSR2                                                          
         OC    PQID,PQID                                                        
         JNZ   FACUSR2                                                          
         DC    H'0'                                                             
FACUSR2  GOTOR FACOUT                                                           
         J     XIT                                                              
                                                                                
FACLAST  NTR1  LABEL=*                                                          
         XC    FWREC,FWREC                                                      
         LA    R3,FWREC                                                         
         MVC   FWRLEN,=Y(FWRLNQ)   LAST PQID                                    
         MVC   FWRUSER,=C'LAST='                                                
         MVC   FWRPQID,PQID                                                     
         GOTOR FACOUT                                                           
         J     XIT                                                              
                                                                                
***********************************************************************         
* CREATE SMF RECORD TO PUT TO FACWK                                   *         
***********************************************************************         
                                                                                
         USING SMFBRECD,SMFREC                                                  
FACSMF   NTR1  LABEL=*                                                          
         XC    SMFREC,SMFREC       CLEAR SMF RECORD                             
         MVC   SMFBLEN,=AL2(SMFBRLNQ)                                           
         MVI   SMFBTYPE,C'M'       MONEY                                        
         MVC   SMFBTXT1,SPACES                                                  
         MVC   SMFBTXT2,SPACES                                                  
         MVC   SMFBSRCE,SPACES                                                  
         MVC   SMFBDRS(6*L'SMFBMNY),=6PL8'0'                                    
         MVC   SMFBINFO,=C'BAL2'                                                
         MVC   SMFBAGY,QCOMPANY    AGENCY HEX                                   
         MVC   SMFBAGYA,ALPHAID    AGENCY ALPHA                                 
         MVC   SMFBUID,ORIGINUM    DEFAULT JCL INFO                             
         MVI   SMFBWPRG,C'A'                                                    
         MVC   SMFBWPRG+1(2),QPROG                                              
         MVC   SMFBTXT2(8),RCJOB   JOB                                          
         GOTOR DATCON,DMCB,(4,RCDATE),(15,SMFBDATE)                             
                                                                                
         USING MASTD,RF                                                         
         L     RF,ADMASTC                                                       
         MVC   SMFBTXT(8),MCUSERID COMPANY ID                                   
                                                                                
         USING SSOOFF,RE           SET DSPACE FROM SS                           
         ICM   RE,15,MCSSB                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SMFBDSPC,SSODSPAC                                                
         DROP  RE                                                               
                                                                                
         MVC   SMFBSENO,SE#                                                     
         MVC   SMFBSENM,SENAME     ACC??                                        
                                                                                
         ZAP   SMFBDRS,POSTDR                                                   
         ZAP   SMFBCRS,POSTCR                                                   
         ZAP   SMFBCNT3,MARKED                                                  
         ZAP   SMFBCNT4,POSTRECS                                                
                                                                                
         LA    R3,SMFRECH          HEADER FOR FWREC                             
         LLH   RF,SMFBLEN          GET LENGTH OF RECORD                         
         AHI   RF,FWRSMFD-FWRECD                                                
         STH   RF,FWRLEN           LENGTH OF FWREC                              
         MVI   FWRSMFTY,12         TYPE - 12 SEE DDSMFOUT                       
         MVC   FWRHDR,=C'SMF='                                                  
         GOTOR FACOUT                                                           
         J     XIT                                                              
                                                                                
FACOUT   NTR1  LABEL=*                                                          
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         L     RF,MCSSB                                                         
         USING SSOOFF,RF                                                        
         L     R2,SSOFWNDX         INDEX                                        
         L     R0,SSOFWBUF         BUFFER                                       
                                                                                
         GOTOR DATAMGR,DMCB,(0,FACADD),(0,FACWRK),(R2),(R3),(R0)                
         CLI   8(R1),0                                                          
         JE    XIT                                                              
         DC    H'0'                                                             
         DROP  RF,R3                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE THE FILE                                                     *         
***********************************************************************         
                                                                                
UPDFL    NMOD1 0,**UPD                                                          
         DROP  RB                                                               
         L     RC,0(R1)                                                         
         TM    JFLG,JFNONB         TEST ALREADY FOUND TO BE UNBILLABLE          
         JO    XIT                                                              
         TM    PGMSTA,PGMUNB       TEST UNBILL                                  
         JO    UNBACC              YES, UPDATE FOR UNBILLING                    
                                                                                
*                                  ** UPDATE ACCOUNT RECORD **                  
UPDACC   OI    RUNOPT,RUNUPDT      SET FILE UPDATES                             
         MVI   MODE,WRITACC        ON EXIT, WRITE ACCOUNT RECORD                
         LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         MVC   RSTPBILL,TODAY2     SET BILLED TODAY                             
         TM    SOONS,SOONRUN       TEST SOON RUN                                
         JNO   UPDACC1                                                          
         CLI   RSTLN,RSTLN3Q                                                    
         JL    UPDACC1                                                          
         MVC   RSTLSBD,TODAY2      LAST SOON BILLED DATE                        
                                                                                
         USING JOBELD,R3                                                        
UPDACC1  ICM   R3,15,AJOBEL        GET THE JOB ELEMENT                          
         TM    BILTTYP,ESTM        IS IT  %EST?                                 
         JNO   UPDACC2                                                          
         CLI   JOBLN,JOBLN3Q                                                    
         JL    UPDACC2                                                          
                                                                                
         TM    JOBBIST,JOBB3BIL    ALREADY DONE 3                               
         JO    UPDACC2                                                          
         TM    JOBBIST,JOBB1BIL    TEST FIRST BILLING DONE                      
         JO    *+12                YES,                                         
         OI    JOBBIST,JOBB1BIL    NO, SET FIRST                                
         J     UPDACC2                                                          
         TM    JOBBIST,JOBB2BIL    TEST SECOND BILLING DONE                     
         JO    *+12                YES,                                         
         OI    JOBBIST,JOBB2BIL    NO, SET SECOND                               
         J     UPDACC2                                                          
         OI    JOBBIST,JOBB3BIL    SET THIRD                                    
                                                                                
UPDACC2  L     R6,ADGOBLOC                                                      
         USING GOBLOCKD,R6                                                      
                                                                                
         CLI   GONEWBT,C'T'        TEST NEW BILL TYPE 'TOTAL'                   
         JNE   UPDACC5                                                          
         LTR   R3,R3                                                            
         JZ    UPDACC3                                                          
         CLI   JOBLN,JOBLN3Q                                                    
         JL    UPDACC3                                                          
         MVI   JOBBTBLB,C'E'       SET PREVIOUS WAS %ESTIMATE                   
                                                                                
UPDACC3  ICM   R4,15,APPREL                                                     
         USING PPRELD,R4                                                        
         JZ    UPDACC5                                                          
         MVI   PPRBTYPE,C'T'       CHANGE TO 'TOTAL' BILL                       
                                                                                
UPDACC5  TM    JFLG,JFREP          TEST REVERSED ESTIMATED PRODUCTION           
         JNO   UPDACC7                                                          
         CLI   GOFILT2,0           TEST REPLACE FILTER 2                        
         JE    UPDACC7                                                          
         LTR   R3,R3                                                            
         JZ    UPDACC6                                                          
         CLI   JOBLN,JOBLN3Q                                                    
         JL    UPDACC6                                                          
         MVC   JOBF2BLB,RSTFILT2   SAVE FILTER 2                                
                                                                                
UPDACC6  MVC   RSTFILT2,GOFILT2    SET NEW FILTER 2                             
                                                                                
UPDACC7  TM    BILTTYP,SPCL        TEST SPECIAL BILL                            
         JO    UPDACC10            YES, DON'T ADD SCI ELEMENT                   
         ZAP   DUB,DUENET                                                       
         ICM   R3,15,ASCI99        ADD NET DOLLARS                              
         JZ    UPDACC9                                                          
         USING SCIELD,R3                                                        
         AP    SCIAMNT,DUB                                                      
         J     UPDACC10                                                         
                                                                                
UPDACC9  LA    R3,WORK             NO ELEMENT, ADD ONE                          
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITT99S                                                 
         ZAP   SCIAMNT,DUB                                                      
         GOTOR HELLO,DMCB,(C'P',ACCOUNT),ADACC,SCIELD,0                         
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,ADACC                                                        
         GOTOR ASETEL,SETLJBQ      RESET ELEMENT ADDRESS FOR JOB                
                                                                                
UPDACC10 TM    PGMAQ,PGMALL27      TEST 27                                      
         JZ    UPDACC12            NO,                                          
         ICM   R3,15,ASCIBA        GET SCIEL ALLOCATED ELEMENT                  
         JZ    UPDACC12            NONE, SKIP IT                                
         ZAP   DUB,DUENET          NET DUE                                      
         SP    DUB,ASPAMT          LESS ASP(TO GET ALLOCATED)                   
         USING SCIELD,R3                                                        
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JO    UPDACC11            YES,                                         
         SP    SCIAMNT,DUB         REDUCE NET                                   
         CLI   SCILN,SCILN2Q                                                    
         JL    *+10                                                             
         SP    SCIADMN,DUECOM      AND COMMISSION                               
         J     UPDACC12                                                         
                                                                                
UPDACC11 TM    OPTA,OPTARWAS       TEST REVERSE WITH ALLOCATED STATUS           
         JNO   UPDACC12            NO,                                          
         ZAP   SCIAMNT,DUB         RESET NET                                    
         CLI   SCILN,SCILN2Q                                                    
         JL    UPDACC12                                                         
         ZAP   SCIADMN,DUECOM      RESET COMMISSION                             
                                                                                
UPDACC12 TM    LOCKS,LOCKACC       TEST ACCOUNT LOCKED(A21)                     
         JNO   UPDACCX                                                          
         GOTOR UNLOCK,DMCB,('LOCKACC',ADACC)                                    
                                                                                
UPDACCX  DS    0H                                                               
         DROP  R2,R3,R4,R6,R7                                                   
         EJECT                                                                  
*                                  ** UPDATE CYCLE RECORD **                    
UPDCYC   TM    JXSTAT5,JXS5AUTO    TEST 'AUTO' BILL                             
         JNO   UPDCYCX                                                          
         OC    JXCYCDA,JXCYCDA                                                  
         JZ    UPDCYCX                                                          
         MVC   AIO,AIO1                                                         
         MVC   DA,JXCYCDA          GET CYCLE RECORD                             
         GOTOR DMGR,ACCGETQ                                                     
         L     R2,AIO                                                           
         MVI   ELCODE,BCYELQ       GET CYCLE ELEMENT                            
         USING BCYELD,R2                                                        
         GOTOR GETELN                                                           
         J     UPDCYC4                                                          
UPDCYC3  GOTOR NEXTEL                                                           
UPDCYC4  JE    *+6                                                              
         DC    H'0'                MISSING CYCLE ELEMENT                        
         CLC   BCYCLE,JXCYCLE      TEST CURRENT CYCLE ELEMENT                   
         JNE   UPDCYC3                                                          
         ZAP   BCYESTA,JXCUNEST    CURRENT NET ESTIMATE                         
         LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         ZAP   BCYBILA,DUENET                                                   
         MVC   BCYREF,BILNUM                                                    
         MVC   BCYBILD,TODAY2                                                   
         GOTOR DMGR,ACCPUTQ                                                     
                                                                                
UPDCYCX  DS    0H                                                               
         DROP  R2,R7                                                            
         EJECT                                                                  
*                                  ** UPDATE TRANSACTIONS **                    
UPDTRN   L     R0,NBTRN            R0=NUMBER IN TABLE                           
         L     R3,ABTRN            R3=A(TRANSACTION TABLE)                      
                                                                                
         USING BTRND,R3                                                         
UPDTRN2  ST    R3,CBTRN                                                         
         MVI   TRNWRIT,NO                                                       
         OC    BTRNDA,BTRNDA       TEST REAL RECORD                             
         JZ    UPDTRN23            NO,                                          
                                                                                
UPDTRN3  MVC   AIO,AIO1                                                         
         MVC   DA,BTRNDA           GET TRANSACTION                              
         GOTOR DMGR,ACCGETQ                                                     
         L     R4,AIO                                                           
         USING TRNRECD,R4                                                       
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         CLC   BTRNWC,WCBILLQ      TEST WORKCODE 99                             
         JE    UPDTRN13                                                         
         NI    TRNRSTA2,ALL-(TRNSBILP)                                          
         NI    TRNSTAT,(ALL-TRNSHOLD) TURN OFF HELD                             
         OC    BTRNUSD,BTRNUSD     TEST FULLY BILLED                            
         JNZ   UPDTRN23            YES,                                         
         MVC   TRNRSANL,TRNANAL    SET ANALYSIS                                 
         TM    PGMAQ,PGMALL27      TEST 27                                      
         JZ    UPDTRN4             NO,                                          
         TM    BTRNSTA2,BTRNSMKU   TEST OK TO MARK AS USED                      
         JNO   *+8                 NO,                                          
UPDTRN4  OI    TRNRSTA2,TRNSUSED   SET TRANSACTION USED STATUS                  
                                                                                
UPDTRN5  L     R2,AIO              GET STATUS ELEMENT                           
         USING TRSELD,R2                                                        
         MVI   ELCODE,TRSELQ                                                    
         GOTOR GETELN                                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    PGMAQ,PGMALL27      TEST 27                                      
         JZ    UPDTRN6             NO,                                          
         TM    BTRNSTA2,BTRNSMKU   TEST OK TO MARK AS USED                      
         JNO   *+10                NO,                                          
UPDTRN6  MVC   TRSUDAT,TODAY2      SET USED DATE                                
                                                                                
         LA    RF,BTRNBK                                                        
         USING JOBD,RF                                                          
         L     R2,AELMNT           CREATE PTAEL                                 
         USING PTAELD,R2                                                        
         XC    PTAEL(255),PTAEL                                                 
         MVI   PTAEL,PTAELQ                                                     
         MVI   PTALN,PTARLN1Q                                                   
         MVC   PTADATE,TODAY2      ACTIVITY DATE                                
         MVI   PTATYPE,PTATRAL     TYPE - ALLOCATE TO BILL                      
         ZAP   PTANET,JOBNET       NET                                          
         SP    PTANET,JOBCD                                                     
         ZAP   PTANETF,PZERO                                                    
         MVC   PTACUR,CURCOD       CURRENCY CODE                                
         MVC   PTAMOA,BILMOSP      MOA                                          
         ZAP   PTACDSC,JOBCD       DISCOUNT                                     
         ZAP   DUB,JOBHRS          HOURS                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,PTAHOURS                                                    
         ZAP   PTARCORT,BTRNCOMR   COMMISSION RATE                              
         TM    BTRNSTA,BTRNSNC     TEST COMMISSIONABLE                          
         JNO   *+10                                                             
         ZAP   PTARCORT,PZERO      NO, CLEAR RATE                               
         ZAP   PTARCOM,JOBCOM      COMMISSION                                   
         MVC   PTARDATE,TODAY2     RUN DATE                                     
         MVC   PTARBLNO,BILNUM     BILL NUMBER                                  
         OC    SVBILNUM,SVBILNUM   FOR RETAIL USE FIRST NUMBER                  
         JZ    *+10                                                             
         MVC   PTARBLNO,SVBILNUM                                                
         MVC   PTARBLDT,BILDT2     BILL DATE                                    
         MVC   PTARCODE,BILTNUM    TRNBTYPE                                     
         TM    CTAXOPT,CTAXOGST    TEST GST PRESENT                             
         JNO   *+10                                                             
         MVC   PTARGSTC,BGSTTYPE   GST TYPE                                     
         TM    CTAXOPT,CTAXOPST    TEST PST PRESENT                             
         JNO   *+16                                                             
         MVC   PTARPSTC,BPSTTYPE   PST TYPE                                     
         MVC   PTARPRVC,BPSTPROV   PST PROVINCE                                 
                                                                                
         L     R2,AIO              LOOK FOR A SPARE PTAEL                       
         MVI   ELCODE,PTAELQ                                                    
         GOTOR GETELN                                                           
         J     UPDTRN72                                                         
UPDTRN7  GOTOR NEXTEL                                                           
UPDTRN72 JNE   UPDTRN9             NONE FOUND - ADD NEW ONE                     
         CLI   PTALN,PTARLN1Q                                                   
         JNE   UPDTRN7                                                          
         TM    PGMAQ,PGMALL21      TEST 21                                      
         JNZ   UPDTRN8             YES,                                         
         CLI   PTATYPE,PTATRAL     TEST ALLOCATED BILLING                       
         JNE   UPDTRN7             NO, GET NEXT ONE                             
         TM    PTASTAT1,PTASPEND   TEST PENDING                                 
         JNO   UPDTRN7             NO, GET NEXT ONE                             
         NI    PTASTAT1,ALL-(PTASPEND)                                          
         MVC   PTAMOA,BILMOSP      MOA                                          
         MVC   PTARDATE,TODAY2     RUN DATE                                     
         MVC   PTARCODE,BILTNUM    TRNBTYPE                                     
         MVC   PTARBLNO,BILNUM     BILL NUMBER                                  
         OC    SVBILNUM,SVBILNUM   FOR RETAIL USE FIRST NUMBER                  
         JZ    *+10                                                             
         MVC   PTARBLNO,SVBILNUM                                                
         MVC   PTARBLDT,BILDT2     BILL DATE                                    
         MVC   PTARFORM,FORMCDE                                                 
         MVC   PTARFORM+1(1),GRPLCDE                                            
         MVC   PTARTYPE,BTRNWCTY                                                
         J     UPDTRN11                                                         
                                                                                
UPDTRN8  CLI   PTATYPE,0           TEST SPARE                                   
         JNE   UPDTRN7                                                          
         L     RF,AELMNT                                                        
         MVC   PTAEL(PTARLN1Q),0(RF)  UPDATE OLD ELEMENT                        
         J     UPDTRN11                                                         
                                                                                
UPDTRN9  GOTOR HELLO,DMCB,(C'P',ACCMST),AIO,AELMNT,0                            
         CLI   12(R1),0                                                         
         JE    UPDTRN11                                                         
         DC    H'0'                                                             
                                                                                
UPDTRN11 TM    PGMAQ,PGMALL27      TEST 27                                      
         JZ    UPDTRN12            NO,                                          
         L     R2,AIO              GET TRXELD                                   
         MVI   ELCODE,TRXELQ                                                    
         GOTOR GETELN                                                           
         JNE   UPDTRN12                                                         
         USING TRXELD,R2                                                        
         NI    TRXSTA2,X'FF'-TRXSBILP                                           
                                                                                
UPDTRN12 MVI   TRNWRIT,YES                                                      
         J     UPDTRN21                                                         
                                                                                
         USING TRNELD,R2                                                        
UPDTRN13 TM    BILTTYP,TOTL+ONEL   ** 99 **                                     
         JZ    UPDTRN14                                                         
         GOTOR BDATL               ADD/CHANGE BILL DATE ELEMENT                 
UPDTRN14 TM    BTRNSTA,BTRNSSK2    TEST SK INCOME TO SI                         
         JNO   UPDTRN21                                                         
         MVC   TRNSK2SI,TODAY2                                                  
         MVI   TRNWRIT,YES                                                      
                                                                                
UPDTRN21 CLI   TRNWRIT,YES                                                      
         JNE   UPDTRN23                                                         
         GOTOR DMGR,ACCPUTQ        WRITE THE UPDATED RECORD                     
         MVC   DKEY,TRNKEY                                                      
         GOTOR DMGR,ACCRDQ         READ THE DIRECTORY                           
         LA    R5,DIR                                                           
D        USING TRNRECD,R5                                                       
         CLC   D.TRNKSTA,TRNRSTA   TEST STATUS CHANGED                          
         JE    UPDTRN23            NO, DON'T WRITE DIRECTORY                    
         MVC   D.TRNKSTA,TRNRSTA   UPDATE DIRECTORY                             
         GOTOR DMGR,ACCWRTQ        AND WRITE IT BACK                            
                                                                                
UPDTRN23 L     R3,CBTRN                                                         
         LA    R3,BTRNLQ(R3)                                                    
         JCT   R0,UPDTRN2                                                       
         J     XIT                                                              
         DROP  R2,R3,R4,RF,D                                                    
         EJECT                                                                  
***********************************************************************         
* UPDATE THE FILE FOR UNBILLING                                       *         
***********************************************************************         
                                                                                
UNBACC   MVC   AIO,AIO1            ** UNBILLING **                              
         MVC   DA,JXRRBDA          GET UNBILLED 99 TRANSACTION                  
         GOTOR DMGR,ACCGETQ                                                     
         L     R4,AIO                                                           
         USING TRNRECD,R4                                                       
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         MVC   TRNUNBIL,TODAY2     SET UNBILLED DATE                            
         GOTOR DMGR,ACCPUTQ        AND PUT ACCMST                               
                                                                                
*                                  ** UPDATE ACCOUNT RECORD **                  
         MVI   MODE,WRITACC        ON EXIT, WRITE ACCOUNT RECORD                
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         TM    SOONS,SOONRUN       TEST SOON RUN                                
         JNO   UNBACC1                                                          
         CLI   RSTLN,RSTLN3Q                                                    
         JL    UNBACC2                                                          
         MVC   RSTLSBD,TODAY2      LAST SOON BILLED DATE                        
         J     UNBACC2                                                          
                                                                                
UNBACC1  MVC   RSTPBILL,TODAY2    SET BILLED TODAY                              
                                                                                
         USING JOBELD,R3                                                        
UNBACC2  ICM    R3,15,AJOBEL        GET THE JOB ELEMENT                         
         TM    BILTTYP,ESTM        IS IT  %EST?                                 
         JNO   UNBACC3                                                          
         CLI   JOBLN,JOBLN3Q                                                    
         JL    UNBACC3                                                          
         TM    JOBBIST,JOBB3BIL    TEST THIRD                                   
         JNO   *+12                                                             
         NI    JOBBIST,X'FF'-JOBB3BIL                                           
         J     UNBACC3                                                          
         TM    JOBBIST,JOBB2BIL    TEST SECOND                                  
         JNO   *+12                                                             
         NI    JOBBIST,X'FF'-JOBB2BIL                                           
         J     UNBACC3                                                          
         NI    JOBBIST,X'FF'-JOBB1BIL                                           
                                                                                
UNBACC3  L     R6,ADGOBLOC                                                      
         USING GOBLOCKD,R6                                                      
UNBACC5  TM    JFLG,JFREP          TEST REVERSED ESTIMATED PRODUCTION           
         JNO   UNBACC7                                                          
         CLI   GOFILT2,0           TEST REPLACE FILTER 2                        
         JE    UNBACC7                                                          
         LTR   R3,R3                                                            
         JZ    UNBACC7                                                          
         CLI   JOBLN,JOBLN3Q                                                    
         JL    UNBACC7                                                          
                                                                                
UNBACC6  MVC   RSTFILT2,JOBF2BLB   RESTORE FILTER 2                             
         MVI   JOBF2BLB,0                                                       
                                                                                
UNBACC7  TM    BILTTYP,SPCL        TEST SPECIAL BILL                            
         JO    UNBACC12            YES, DON'T ADD SCI ELEMENT                   
         LA    R7,AMTS                                                          
         USING AMTD,R7                                                          
         ZAP   DUB,DUENET                                                       
         ICM   R3,15,ASCI99        ADD NET DOLLARS                              
         JZ    UNBACC9                                                          
         USING SCIELD,R3                                                        
         AP    SCIAMNT,DUB                                                      
         J     UNBACC12                                                         
                                                                                
UNBACC9  LA    R3,WORK             NO ELEMENT, ADD ONE                          
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITT99S                                                 
         ZAP   SCIAMNT,DUB                                                      
         GOTOR HELLO,DMCB,(C'P',ACCOUNT),ADACC,SCIELD,0                         
         CLI   12(R1),0                                                         
         JE    UNBACC12                                                         
         DC    H'0'                                                             
                                                                                
UNBACC12 TM    LOCKS,LOCKACC       TEST ACCOUNT LOCKED(A23)                     
         JNO   UNBACCX                                                          
         GOTOR UNLOCK,DMCB,('LOCKACC',ADACC)                                    
                                                                                
UNBACCX  DS    0H                                                               
         DROP  R2,R3,R4,R6,R7                                                   
         EJECT                                                                  
*                                  ** UPDATE TRANSACTIONS FOR UNBILL **         
UNBTRN   L     R0,NBTRN            R0=NUMBER IN TABLE                           
         L     R3,ABTRN            R3=A(TRANSACTION TABLE)                      
                                                                                
         USING BTRND,R3                                                         
UNBTRN2  ST    R3,CBTRN                                                         
         MVI   TRNWRIT,NO                                                       
         OC    BTRNDA,BTRNDA       TEST REAL RECORD                             
         JZ    UPDTRN23            NO,                                          
                                                                                
UNBTRN4  MVC   AIO,AIO1                                                         
         MVC   DA,BTRNDA           GET TRANSACTION                              
         GOTOR DMGR,ACCGETQ                                                     
         L     R4,AIO                                                           
         USING TRNRECD,R4                                                       
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         CLC   BTRNWC,WCBILLQ      TEST WORKCODE 99                             
         JE    UNBTRN13                                                         
         TM    BTRNSTA,BTRNSUNB    TEST UNBILL THIS ITEM                        
         JNO   UNBTRN23            NO,                                          
         NI    TRNRSTA2,ALL-(TRNSUSED) REMOVE USED BIT                          
         NI    TRNRSTA2,ALL-(TRNSBILP)                                          
                                                                                
UNBTRN5  L     R2,AIO              GET STATUS ELEMENT                           
         USING TRSELD,R2                                                        
         MVI   ELCODE,TRSELQ                                                    
         GOTOR GETELN                                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         XC    TRSUDAT,TRSUDAT     REMOVED USED DATE                            
                                                                                
         L     R2,AIO              LOOK FOR ORIGINAL PTAEL                      
         MVI   ELCODE,PTAELQ                                                    
         USING PTAELD,R2                                                        
         GOTOR GETELN                                                           
         J     UNBTRN62                                                         
UNBTRN6  GOTOR NEXTEL                                                           
UNBTRN62 JE    *+6                                                              
         DC    H'0'                CAN'T FIND PTAEL FOR THIS BILL               
         CLC   PTARBLDT,JXRRDT2                                                 
         JNE   UNBTRN6                                                          
         OI    PTASTAT1,PTASREVU   INDICATE REVERSED                            
         MVC   PTARUNBD,TODAY2     SAVE DATE UNBILLED                           
         MVC   PTARUNBB,BILNUM     SAVE UNBILLED BILL NUMBER                    
                                                                                
         L     RF,AELMNT                                                        
         XC    0(255,RF),0(RF)                                                  
         LLC   R1,PTALN                                                         
         BCTR  R1,0                                                             
         EXRL  R1,UTRNMVFP         COPY PTAEL ELEMENT                           
         LR    R3,R2               SAVE ADDRESS OF ORIGINAL PTAEL               
         LR    R2,RF               SET R2 TO ADDRESS OF NEW ELEMENT             
         MVI   PTASTAT1,PTASREVS                                                
         MVC   PTARORGB,PTARBLNO   SAVE ORIGINAL BILL NUMBER                    
         MVC   PTARORGD,PTARBLDT   SAVE ORIGINAL BILL DATE                      
                                                                                
         MVC   PTARDATE,TODAY2     REVERSE DATE                                 
         MVC   PTADATE,TODAY2      ACTIVITY DATE                                
         MVC   PTARBLDT,BILDT2     BILL DATE                                    
         MVC   PTAMOA,BILMOSP      MOA                                          
                                                                                
         MVC   PTARBLNO,BILNUM     BILL NUMBER                                  
         MVC   PTARCODE,BILTNUM    TRNBTYPE                                     
                                                                                
         XI    PTANET+L'PTANET-1,X'01'                                          
         XI    PTANETF+L'PTANETF-1,X'01'                                        
         XI    PTACDSC+L'PTACDSC-1,X'01'                                        
         XI    PTARCOM+L'PTARCOM-1,X'01'                                        
         LH    RF,PTAHOURS                                                      
         LCR   RF,RF                                                            
         STH   RF,PTAHOURS                                                      
                                                                                
UNBTRN9  GOTOR HELLO,DMCB,(C'P',ACCMST),AIO,AELMNT,0                            
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    OPTA,OPTARWAS       TEST OPTION TO RE-ALLOCATE                   
         JNO   UNBTRN11                                                         
         LR    R2,R3               RESTORE ADDRESS OF ORIGINAL                  
         L     RF,AELMNT                                                        
         XC    0(255,RF),0(RF)                                                  
         LLC   R1,PTALN                                                         
         BCTR  R1,0                                                             
         EXRL  R1,UTRNMVFP                                                      
         LR    R2,RF                                                            
         MVC   PTADATE,TODAY2      ACTIVITY DATE                                
         XC    PTARDATE,PTARDATE                                                
         XC    PTARBLNO,PTARBLNO                                                
         XC    PTARBLDT,PTARBLDT                                                
         XC    PTARORGB,PTARORGB                                                
         XC    PTARORGD,PTARORGD                                                
         XC    PTARCODE,PTARCODE                                                
         XC    PTARFORM,PTARFORM                                                
         XC    PTARTYPE,PTARTYPE                                                
         MVI   PTASTAT1,PTASPEND                                                
         OI    PTASTAT1,PTASCASH                                                
*                                  ADD RE-ALLOCATED ELEMENT                     
         GOTOR HELLO,DMCB,(C'P',ACCMST),AIO,AELMNT,0                            
         CLI   12(R1),0                                                         
         JE    UNBTRN11                                                         
         DC    H'0'                                                             
                                                                                
UTRNMVFP MVC   0(0,RF),PTAEL                                                    
                                                                                
UNBTRN11 TM    PGMAQ,PGMALL27      TEST 27                                      
         JZ    UNBTRN12            NO,                                          
         TM    OPTA,OPTARWAS       TEST OPTION TO REALLOCATE                    
         JNO   UNBTRN12            NO,                                          
         L     R4,AIO              GET TRXELD                                   
         USING TRNRECD,R4                                                       
         OI    TRNRSTA2,TRNSBILP   SET PENDING BIT                              
                                                                                
         L     R2,AIO              GET TRXELD                                   
         MVI   ELCODE,TRXELQ                                                    
         GOTOR GETELN                                                           
         JNE   UNBTRN12                                                         
         USING TRXELD,R2                                                        
         OI    TRXSTA2,TRXSBILP                                                 
UNBTRN12 MVI   TRNWRIT,YES                                                      
         J     UNBTRN21                                                         
                                                                                
         USING TRNELD,R2                                                        
UNBTRN13 TM    BILTTYP,TOTL+ONEL+ESTM                                           
         JZ    UNBTRN14                                                         
         GOTOR BDATL               ADD/CHANGE BILL DATE ELEMENT                 
UNBTRN14 TM    BTRNSTA,BTRNSSK2    TEST SK TO SI FLIP(ON %EST)                  
         JNO   UNBTRN21                                                         
         XC    TRNSK2SI,TRNSK2SI                                                
         MVI   TRNWRIT,YES                                                      
                                                                                
UNBTRN21 CLI   TRNWRIT,YES                                                      
         JNE   UNBTRN23                                                         
         GOTOR DMGR,ACCPUTQ        WRITE THE UPDATED RECORD                     
         MVC   DKEY,TRNKEY                                                      
         GOTOR DMGR,ACCRDQ         READ THE DIRECTORY                           
         LA    R5,DIR                                                           
D        USING TRNRECD,R5                                                       
         CLC   D.TRNKSTA,TRNRSTA   TEST STATUS CHANGED                          
         JE    UNBTRN23            NO, DON'T WRITE DIRECTORY                    
         MVC   D.TRNKSTA,TRNRSTA   UPDATE DIRECTORY                             
         GOTOR DMGR,ACCWRTQ        AND WRITE IT BACK                            
                                                                                
UNBTRN23 L     R3,CBTRN                                                         
         LA    R3,BTRNLQ(R3)                                                    
         JCT   R0,UNBTRN2                                                       
         J     XIT                                                              
         DROP  R2,R3,R4,D                                                       
         EJECT                                                                  
***********************************************************************         
* ADD/CHANGE DATE ELEMENT 0N W/C 99 RECORD                            *         
***********************************************************************         
                                                                                
BDATL    NTR1  LABEL=*                                                          
         L     R2,AIO                                                           
         USING TRNRECD,R2                                                       
         LA    R2,TRNRFST                                                       
         USING TRNELD,R2                                                        
         LARL  R1,BTYPTAB          ONLY FOR CERTAIN BILL TYPES                  
BDATL3   CLC   TRNBTYPE,0(R1)                                                   
         JE    BDATL5                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),EOT                                                        
         JNE   BDATL3                                                           
         J     XIT                                                              
                                                                                
BDATL5   MVI   ELCODE,GDAELQ       GET DATE ELEMENT                             
         USING GDAELD,R2                                                        
BDATL7   GOTOR NEXTEL                                                           
         JNE   BDATL13             ELEMENT NOT FOUND                            
         CLI   GDATYPE,GDATBILL                                                 
         JNE   BDATL7                                                           
*                                  ** ELEMENT FOUND **                          
         TM    PGMSTA,PGMUNB       TEST RUNNING UNBILL                          
         JO    BDATL9              YES                                          
         OC    GDADATE2,GDADATE2   TEST UNBILLED THIS INVOICE                   
         JNZ   BDATL7              YES, LOOK FOR ANOTHER                        
         J     XIT                 NO, LEAVE AS BILLED                          
                                                                                
BDATL9   CLC   GDADATE,JXRRADT1    TEST BILL DATE TO THIS UNBILL                
         JNE   BDATL7              NO, LOOK AGAIN                               
         OC    GDADATE2,GDADATE2   TEST ALREADY UNBILLED                        
         JNZ   BDATL7                                                           
         CLI   GDALN,GDALN4Q                                                    
         JL    BDATL7                                                           
         CLC   GDABILNO,JXRRNUM    TEST SAME NUMBER                             
         JNE   BDATL7                                                           
         MVC   GDADATE2,TODAY1     MARKED IT UNBILLED                           
         MVI   TRNWRIT,YES                                                      
         J     XIT                                                              
*                                  ** ELEMENT NOT FOUND **                      
BDATL13  TM    PGMSTA,PGMUNB       TEST RUNNING UNBILLING                       
         JO    XIT                                                              
         L     R2,AELMNT           ADD DATE BILLED ELEMENT                      
         XC    GDAEL(GDALN4Q),GDAEL                                             
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN4Q                                                    
         MVI   GDATYPE,GDATBILL                                                 
         MVC   GDADATE,TODAY1                                                   
         MVC   GDABILNO,BILNUM                                                  
         GOTOR HELLO,DMCB,(C'P',ACCMST),AIO,GDAEL,0                             
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNWRIT,YES                                                      
         J     XIT                                                              
                                                                                
BTYPTAB  DC    AL1(TRNBTMAN)       MANUAL                                       
         DC    AL1(TRNBTPER)       %EST                                         
         DC    AL1(TRNBTSPE)       SPECIAL                                      
         DC    AL1(EOT)                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TEST LOCK STATUS                                                    *         
***********************************************************************         
                                                                                
TSTLOCK  NTR1  LABEL=*                                                          
         L     R2,0(R1)            R2=A(RECORD TO TEST)                         
         LLC   R3,0(R1)            R3=LOCK BIT                                  
         TM    PGMSTA,PGMDFT       TEST DRAFT                                   
         JO    XITY                YES, DON'T TEST LOCK                         
         TM    SOONS,SOONRUN       TEST SOON RUN                                
         JZ    XITY                NO,                                          
         CLI   RCWRITE,NO          TEST WRITE=NO                                
         JE    XITY                YES,                                         
                                                                                
         MVI   ELCODE,LGLELQ       GET LOCK ELEMENT                             
         GOTOR GETEL                                                            
         JE    *+6                                                              
         DC    H'0'                NO, LOCK ELEMENT                             
         USING LGLELD,R2                                                        
         TM    LGLSTAT,LGLSLOCK    MAKE SURE RECORD IS LOCKED                   
         JNO   XITN                                                             
         CLC   LGLDATE,TODAY3      AND THAT IT WAS LOCKED TODAY                 
         JNE   XITN                                                             
         OC    LUID,LUID                                                        
         JNZ   *+10                                                             
         MVC   LUID,LGLLUID        SAVE LUID                                    
         EXRL  R3,TLOKOIL0         SET LOCK STATUS                              
         J     XITY                                                             
                                                                                
TLOKOIL0 OI    LOCKS,0                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UNLOCK RECORDS                                                      *         
***********************************************************************         
                                                                                
UNLOCK   NTR1  LABEL=*                                                          
         L     R2,0(R1)            R2=A(RECORD TO TEST)                         
         LLC   R3,0(R1)            R3=LOCK BIT                                  
         MVI   ELCODE,LGLELQ       YES, GET LOCK ELEMENT                        
         GOTOR GETEL                                                            
         JE    UNLOCK3                                                          
         DC    H'0'                NO LOCK ELEMENT                              
                                                                                
         USING LGLELD,R2                                                        
UNLOCK3  NI    LGLSTAT,ALL-LGLSLOCK                                             
         LA    R1,ALL                                                           
         SR    R1,R3                                                            
         EXRL  R1,ULOKNIL0                                                      
         J     XIT                                                              
                                                                                
ULOKNIL0 NI    LOCKS,0                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET PROGRAM PROFILE OPTIONS                                         *         
***********************************************************************         
                                                                                
GETPRF   NTR1  LABEL=*                                                          
         XC    OPTS,OPTS                                                        
         MVI   BYTE,C'P'           FIRST GET PROFILE OPTIONS                    
         L     R2,PGMPROF          R2=A(PROGRAM PROFILE LINK TABLE)             
         L     R3,APROFILE         R3=A(PROGRAM PROFILE)                        
         J     GETPRF1                                                          
                                                                                
GETPRF0  L     R2,PGMQOPT          R2=A(PROGRAM REQUEST OPTIONS)                
         LA    R3,ACWORKD          R3=A(MONACC WORKAREA)                        
                                                                                
GETPRF1  LA    R5,OPTS             R4=A(INTERNAL OPTIONS)                       
         LLC   RF,PGMAQ            RF=PROGRAM EQUATE                            
                                                                                
         USING PRFD,R2                                                          
GETPRF2  EXRL  RF,GPRFTMP0         TEST PROFILE USED IN THIS PROGRAM            
         JNO   GETPRF11                                                         
         LLC   R0,PRFSTA                                                        
         SLL   R0,30               GET THE # OF LEVELS                          
         SRL   R0,30                                                            
         LA    R1,L'ACPPFCPY       LENGTH OF A LEVEL                            
         TM    PRFSTA,PRFSLVBK     BACKUP N LEVELS?                             
         JNO   *+6                                                              
         LNR   R1,R1               MAKE THE LENGTH NEGATIVE                     
                                                                                
         LLH   R4,PRFFLD           DISPLACEMENT TO PROFILE FIELD                
         AR    R4,R3               R4=A(PROFILE FIELD)                          
         LLC   R6,PRFOPT           DISPLACEMENT TO OPTION FIELD                 
         AR    R6,R5               R6=A(INTERNAL OPTION FIELD)                  
         XR    RE,RE                                                            
         ICM   RE,1,PRFNOT         TURNOFF ITEMS SET IN PROFILE                 
         JZ    GETPRF3             THAT REQUESTS OVERRIDE                       
         CLI   0(R4),C' '          TEST ANY REQUEST DATA                        
         JNH   GETPRF3                                                          
         EXRL  RE,GPRFNI60                                                      
                                                                                
GETPRF3  XR    RE,RE                                                            
         CLI   PRFINPT,C' '        IS FIRST ENTRY A DEFAULT?                    
         JNE   GETPRF5                                                          
         IC    RE,PRFOBIT          RE=DEFAULT VALUE                             
                                                                                
GETPRF5  CLI   0(R4),0             ANY PROFILE VALUE?                           
         JNE   GETPRF7                                                          
         LTR   R0,R0               CAN GO TO ANOTHER LEVEL?                     
         JZ    GETPRF7                                                          
         SHI   R0,1                                                             
         AR    R4,R1               GO BACK/ FORWARD ONE LEVEL                   
         J     GETPRF5                                                          
                                                                                
GETPRF7  TM    PRFSTA,PRFSINP      USE INPUT VALUE?                             
         JNO   *+14                                                             
         MVC   0(1,R6),0(R4)                                                    
         J     GETPRF11                                                         
                                                                                
         IC    R0,PRFNUM           NUMBER OF INPUT OPTIONS                      
         LA    R7,PRFDATA                                                       
GETPRF9  CLC   0(1,R7),0(R4)       MATCH TABLE TO PROFILE INPUT VALUE           
         JNE   *+12                                                             
         IC    RE,1(R7)            RE=OPTION BIT                                
         J     *+12                                                             
         LA    R7,L'PRFDATA(R7)                                                 
         JCT   R0,GETPRF9                                                       
         EXRL  RE,GPRFOI60         SET OPTION BIT                               
                                                                                
GETPRF11 LLC   RE,PRFLEN                                                        
         AR    R2,RE                                                            
         CLI   0(R2),EOT                                                        
         JNE   GETPRF2                                                          
         CLI   BYTE,C'Q'           TEST ALREADY HAVE REQUEST OPTIONS            
         JE    XIT                 YES,                                         
         MVI   BYTE,C'Q'           SET TO GET REQUEST OPTIONS                   
         J     GETPRF0                                                          
                                                                                
GPRFTMP0 TM    PRFPGM,0                                                         
GPRFNI60 NI    0(R6),0                                                          
GPRFOI60 OI    0(R6),0                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
DMGR     NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,DMGRACT                                                       
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
DMGRACT  J     ACCRD               READ ACCDIR                                  
         J     ACCHI               READ HIGH ACCDIR                             
         J     ACCHID              READ HIGH ACCDIR(PASS DELETES)               
         J     ACCSEQ              READ SEQ ACCDIR                              
         J     ACCWRT              WRITE ACCDIR                                 
         J     ACCGET              GETREC ACCMST                                
         J     ACCPUT              PUTREC ACCMST                                
         J     ACCADD              ADDREC ACCMST                                
         J     ACCTRD              READ ACCOUNT RECORD - OLD STYLE              
         J     ACCTHI              READ HI ACCOUBT RECORD - OLD FILE            
         J     ACCTWRT             WRITE ACCOUNT RECORD - OLD STYLE             
         J     ACCTADD             ADD ACCOUNT RECORD - OLD STYLE               
         J     CONRD               READ CONTROL FILE                            
         J     CONHI               READ HIGH CONTROL FILE                       
         J     CONSEQ              READ SEQUENTIAL CONTROL FILE                 
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES - ACCOUNT FILES                               *         
***********************************************************************         
                                                                                
ACCRD    GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR     READ ACC DIR             
         CLC   DKEY,DIR                                                         
         JE    *+6                                                              
         DC    H'0'                RECORD NOT FOUND                             
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         J     DMERR                                                            
                                                                                
ACCHI    GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR    READ HI ACC DIR           
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         J     DMERR                                                            
                                                                                
ACCHID   GOTOR DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,DKEY,DIR PASS DELETED         
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         NI    8(R1),X'FF'-(X'02')                                              
         J     DMERR                                                            
                                                                                
ACCSEQ   GOTOR DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR     READ SEQ ACC DIR         
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         J     DMERR                                                            
                                                                                
ACCWRT   GOTOR DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR        WRITE ACC DIR           
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         JNE   *+6                                                              
         BASR  RE,RF                                                            
         J     DMERR                                                            
                                                                                
ACCGET   GOTOR DATAMGR,DMCB,GETREC,ACCMST,DA,AIO,DMWORK GETREC ACCMST           
         J     DMERR                                                            
                                                                                
ACCPUT   GOTOR DATAMGR,DMCB,PUTREC,ACCMST,DA,AIO,DMWORK PUTREC ACCMST           
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         JNE   *+6                                                              
         BASR  RE,RF                                                            
         J     DMERR                                                            
                                                                                
ACCADD   GOTOR DATAMGR,DMCB,ADDREC,ACCMST,DA,AIO,DMWORK ADDREC ACCMST           
         ORG   *-2                                                              
         CLI   RCWRITE,C'Y'                                                     
         JNE   *+6                                                              
         BASR  RE,RF                                                            
         J     DMERR                                                            
                                                                                
ACCTRD   GOTOR DATAMGR,DMCB,DMREAD,ACCOUNT,DKEY,AIO     READ ACCOUNT            
         J     DMERR                                                            
                                                                                
ACCTHI   GOTOR DATAMGR,DMCB,DMRDHI,ACCOUNT,DKEY,AIO     READ HI                 
         J     DMERR                                                            
                                                                                
ACCTWRT  GOTOR DATAMGR,DMCB,DMWRT,ACCOUNT,DKEY,AIO      WRITE ACCOUNT           
         J     DMERR                                                            
                                                                                
ACCTADD  GOTOR DATAMGR,DMCB,DMADD,ACCOUNT,AIO,AIO       ADD ACCOUNT             
         J     DMERR                                                            
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES - CONTROL FILE                                *         
***********************************************************************         
                                                                                
CONRD    GOTOR DATAMGR,DMCB,DMREAD,CTFILE,CKEY,AIO                              
         L     RE,AIO                                                           
         CLC   CKEY,0(RE)                                                       
         JE    DMERR                                                            
         DC    H'0'                RECORD NOT FOUND                             
                                                                                
CONHI    GOTOR DATAMGR,DMCB,DMRDHI,CTFILE,CKEY,AIO                              
         J     DMERR                                                            
                                                                                
CONSEQ   GOTOR DATAMGR,DMCB,DMRSEQ,CTFILE,CKEY,AIO                              
         J     DMERR                                                            
                                                                                
DMERR    MVC   DMBYTE,8(R1)                                                     
         TM    UPSI,UPDMG          TRACE DATAMGR CALLS?                         
         JNO   DMERR2                                                           
         GOTOR DMTRCE                                                           
                                                                                
DMERR2   CLI   DMBYTE,X'10'        TEST RECORD NOT FOUND                        
         JE    XITN                RETURN NOT EQUAL                             
         CLI   DMBYTE,0                                                         
         JE    XITY                                                             
         DC    H'0'                                                             
         J     XITN                                                             
         EJECT                                                                  
DMTRCE   NTR1  LABEL=*                                                          
         MVC   DMBLK,SPACES                                                     
         ICM   RE,15,4(RD)                                                      
         ICM   RE,15,4(RE)                                                      
         ICM   RE,15,4(RE)                                                      
         MVC   DMBLKA,0(RE)        GET NAME OF CALLER                           
         L     R2,0(R1)                                                         
         MVC   DMBLKB,0(R2)        COMMAND                                      
         L     R2,4(R1)                                                         
         MVC   DMBLKC,0(R2)        FILE                                         
         MVC   DMBLKD,DMCOUNTQ     DMCOUNT=00                                   
         L     RF,AMONACC                                                       
         L     RF,ACMDMCNT-ACMD(RF)                                             
         ICM   R0,15,0(RF)                                                      
         EDITR (R0),DMBLKE,ALIGN=LEFT                                           
         L     R2,12(R1)           R2=A(IO)                                     
         MVC   DMBLKF,DMSTIOQ      START IO=                                    
         L     RF,ADMASTC                                                       
         ICM   R0,15,MCACTIOS-MASTD(RF)                                         
         EDITR (R0),DMBLKG,ALIGN=LEFT                                           
         MVC   DMCB+20(4),MCVPRINT-MASTD(RF)                                    
         MVI   DMCB+20,C'P'                                                     
         GOTOR PRNTBL,DMCB,(L'DMBLK,DMBLK),(R2),C'DUMP',42,=C'2D'               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SORTER ROUTINES                                                     *         
***********************************************************************         
                                                                                
SORT     NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         L     R3,ASRTWK                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,SORTACT                                                       
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
SORTACT  J     SORTINI             SORT INITIALIZE                              
         J     SORTPUT             SORT PUT                                     
         J     SORTGET             SORT GET                                     
         J     SORTEND             SORT END                                     
                                                                                
SORTINI  LA    R2,SKLNQ             LENGTH OF SORT KEY                          
         EDIT  (R2),(3,SORTCARD+15),FILL=0                                      
         LHI   R2,SRDLNQ            LENGTH OF SORT RECORD                       
         EDIT  (R2),(4,RECCARD+22),FILL=0                                       
         GOTOR ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
         J     XITY                                                             
                                                                                
SORTPUT  GOTOR ADSORTER,DMCB,=C'PUT',ASRTWK                                     
         TM    UPSI,UPSRT          TRACE SORT RECORDS?                          
         JZ    XITY                                                             
         GOTOR ATRCE,TRCTSRQ                                                    
         J     XITY                                                             
                                                                                
SORTGET  GOTOR ADSORTER,DMCB,=C'GET'                                            
         ICM   RE,15,4(R1)                                                      
         JZ    XITN                                                             
         LLH   RF,0(RE)            MOVE RECORD TO SORT WORK AREA                
         L     R0,ASRTWK                                                        
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         TM    UPSI,UPSRF          TRACE RECORDS FROM SORT                      
         JZ    XITY                                                             
         GOTOR ATRCE,TRCFSRQ                                                    
         J     XITY                                                             
                                                                                
SORTEND  GOTOR ADSORTER,DMCB,=C'GET'                                            
         GOTOR ADSORTER,DMCB,=C'END',0                                          
         J     XITY                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT PRINT LINES TO UPPER CASE ONLY                   *         
*                                                                     *         
* NTRY: P1 BYTE 0=NUMBER OF PRINT LINES                               *         
*             1-3=A(1ST PRINT LINE)                                   *         
***********************************************************************         
                                                                                
UPPER    STM   RE,RC,12(RD)                                                     
         LLC   R0,0(R1)                                                         
         XR    RE,RE                                                            
         ICM   RE,7,1(R1)                                                       
         LARL  RF,UPPTAB                                                        
UPPER02  TR    0(PLNQ,RE),0(RF)                                                 
         LA    RE,PLNQ(RE)                                                      
         JCT   R0,UPPER02                                                       
         LM    RE,RC,12(RD)                                                     
         J     XITY                                                             
                                                                                
UPPTAB   DS    0X                  LOWER TO UPPER CASE TRANSLATE TABLE          
         DC    X'000102030405060708090A0B0C0D0E0F'                              
         DC    X'101112131415161718191A1B1C1D1E1F'                              
         DC    X'202122232425262728292A2B2C2D2E2F'                              
         DC    X'303132333435363738393A3B3C3D3E3F'                              
         DC    X'404142434445464748494A4B4C4D4E4F'                              
         DC    X'505152535455565758595A5B5C5D5E5F'                              
         DC    X'606162636465666768696A6B6C6D6E6F'                              
         DC    X'707172737475767778797A7B7C7D7E7F'                              
         DC    X'80C1C2C3C4C5C6C7C8C98A8B8C8D8E8F'                              
         DC    X'90D1D2D3D4D5D6D7D8D99A9B9C9D9E9F'                              
         DC    X'A0A1E2E3E4E5E6E7E8E9AAABACADAEAF'                              
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'                              
         DC    X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'                              
         DC    X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'                              
         DC    X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'                              
         DC    X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'                              
UPPTABX  DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* GET CURRENCY CODE                                                   *         
***********************************************************************         
                                                                                
GETCUR   LARL  RF,CURTAB                                                        
GETCUR3  CLI   0(RF),0             END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CTRYCOD,0(RF)       MATCH ON COUNTRY CODE                        
         JE    *+12                                                             
         LA    RF,L'CURTAB(RF)                                                  
         J     GETCUR3                                                          
         MVC   CURCOD,1(RF)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PARSE CLIENT / PRODUCT / JOB                                        *         
***********************************************************************         
                                                                                
PARCPJ   NTR1  LABEL=*                                                          
         MVC   CLI,SPACES          GET CLIENT, PRODUCT, JOB CODES               
         MVC   PRD,SPACES                                                       
         MVC   JOB,SPACES                                                       
                                                                                
         L     R2,AIO                                                           
         USING ACTRECD,R2                                                       
         LA    RF,ACTKACT                                                       
         L     R5,ADLDGHIR                                                      
         USING ACLELD,R5                                                        
         LLC   R1,ACLVLEN                                                       
         BCTR  R1,0                                                             
         EXRL  R1,PCPJMVCF                                                      
         LA    RF,1(R1,RF)                                                      
         LA    R3,1(R1)                                                         
         IC    R1,ACLVLEN+L'ACLVALS                                             
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EXRL  R1,PCPJMVPF                                                      
         LA    RF,1(R1,RF)                                                      
         LA    R3,1(R1,R3)                                                      
         IC    R1,ACLVLEN+(L'ACLVALS*2)                                         
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EXRL  R1,PCPJMVJF                                                      
                                                                                
         LARL  R6,NAMTAB           GET CLIEN/PRD/JOB KEYS AND NAME              
PARCPJ3  MVC   SOURC,0(R6)                                                      
         SR    RF,RF                                                            
         EXRL  RF,LARF                                                          
         L     RF,0(RF)                                                         
         MVC   DEST,2(R6)                                                       
         SR    RE,RE                                                            
         EXRL  RE,LARE                                                          
         MVC   0(L'CLIK,RE),0(RF)  SAVE RECORD KEY                              
                                                                                
         MVC   SOURC,4(R6)                                                      
         SR    RF,RF                                                            
         EXRL  RF,LARF                                                          
         L     RF,0(RF)                                                         
         MVC   DEST,6(R6)                                                       
         SR    RE,RE                                                            
         EXRL  RE,LARE                                                          
         MVC   0(L'JOBNAM,RE),SPACES                                            
         USING NAMELD,RF                                                        
         LLC   R1,NAMLN                                                         
         SHI   R1,NAMEREC+1-NAMELD                                              
         EXRL  R1,PCPJMVEN                                                      
         LA    R6,L'NAMTAB(R6)                                                  
         CLI   0(R6),EOT                                                        
         JNE   PARCPJ3                                                          
         J     XITY                                                             
                                                                                
PCPJMVCF MVC   CLI(0),0(RF)        SAVE CLIENT                                  
PCPJMVPF MVC   PRD(0),0(RF)        SAVE PRODUCT                                 
PCPJMVJF MVC   JOB(0),0(RF)        SAVE JOB                                     
PCPJMVEN MVC   0(0,RE),NAMEREC     MOVE NAME TO DESTINATION                     
         DROP  R2,R5,RF                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET ADDRESSES OF RECORD ELEMENTS                         *         
***********************************************************************         
                                                                                
SETEL    NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         LTR   R1,R1               TEST INITIALIZATION                          
         JNZ   SETEL1                                                           
         LARL  R6,SETTAB           CLEAR ADDRESS FIELDS                         
                                                                                
SETLIN   L     R5,0(R6)            CLEAR ALL ADDRESS FIELDS                     
         LA    R5,ELMDATA-ELMTD(R5)                                             
         GOTOR SETCLR              CLEAR  ADDRESS LIST                          
         LA    R6,L'SETTAB(R6)     NEXT SETTAB ENTRY                            
         CLI   0(R6),EOT                                                        
         JNE   SETLIN                                                           
         J     XIT                                                              
                                                                                
SETEL1   BCTR  R1,0                                                             
         SLL   R1,2                LIST NUMBER X 4                              
         LARL  R2,SETTAB                                                        
         AR    R1,R2               R1=A(ELEMENT LIST FOR THIS RECORD)           
         ICM   R6,15,0(R1)                                                      
         USING ELMTD,R6                                                         
         LR    R5,R6               CLEAR ADDRESS FIELDS                         
         LA    R5,ELMDATA-ELMTD(R5)                                             
         GOTOR SETCLR                                                           
         L     R2,AIO                                                           
         LA    RE,ACCORFST                                                      
         TM    ELMRST,ELMROLD      TEST OLD STYLE                               
         JO    *+8                                                              
         LA    RE,ACCRFST-ACCRECD                                               
         AR    R2,RE                                                            
         SR    R0,R0                                                            
                                                                                
SETEL3   LA    R3,ELMDATA                                                       
         USING ELMDATA,R3                                                       
SETEL5   CLI   ELMCODE,EOT         TEST EOT                                     
         JE    SETEL9                                                           
         CLC   ELMCODE,0(R2)       MATCH TABLE TO RECORD ELEMENT                
         JNE   SETEL6              NO, GET NEXT TABLE ITEM                      
         CLI   ELMCODE,SCIELQ      TEST SCIEL                                   
         JNE   SETEL7              NO, IT'S A MATCH                             
         USING SCIELD,R2                                                        
         CLC   SCITYPE,ELMSCIT     MATCH TYPE                                   
         JE    SETEL7              YES                                          
SETEL6   LA    R3,L'ELMDATA(R3)                                                 
         J     SETEL5                                                           
         DROP  R2                                                               
                                                                                
SETEL7   LLH   RE,ELMBDSP                                                       
         SRDL  RE,12               RE=BASE REGISTER                             
         SRL   RF,32-12                                                         
         EXRL  RE,SETELLRE         GET BASE REGISTER VALUE                      
         AR    RF,RE               ADD BASE TO DISP                             
         STCM  R2,15,0(RF)         SAVE ADDRESS OF ELEMENT                      
         J     SETEL9                                                           
                                                                                
SETEL9   IC    R0,1(R2)            GET NEXT ELEMENT                             
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         JNE   SETEL3                                                           
         J     XITY                                                             
                                                                                
SETCLR   LR    R0,RE                                                            
SETCLR3  LLH   RE,ELMBDSP-ELMDATA(R5)                                           
         SRDL  RE,12               RE=BASE REGISTER                             
         SRL   RF,32-12                                                         
         EXRL  RE,SETELLRE         GET BASE REGISTER VALUE                      
         AR    RF,RE               ADD BASE TO DISP                             
         XC    0(4,RF),0(RF)       CLEAR ADDRESS                                
         LA    R5,L'ELMDATA(R5)                                                 
         CLI   0(R5),EOT                                                        
         JNE   SETCLR3                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
SETELLRE LR    RE,0                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PERCENT OF ESTIMATES ROUTINES                                       *         
***********************************************************************         
                                                                                
PEST     NTR1  LABEL=*                                                          
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,PESTACT                                                       
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
PESTACT  J     PESTF               ACCOUNT FIRST                                
         J     PESTL               ACCOUNT LAST                                 
                                                                                
PESTF    L     RF,ADCMPEL          ** ACCOUNT FIRST **                          
         TM    CPYSTAT4-CPYELD(RF),CPYSPESK                                     
         JNO   *+8                                                              
         OI    OPTC,OPTCPESK       POST INCOME TO SK                            
                                                                                
         SR    R0,R0                                                            
         L     R4,JXAEWC           GET BILLABLE ESTIMATES                       
         USING JXEWD,R4                                                         
         CLI   0(R4),EOT           TEST ANY ESTIMATES                           
         JE    XIT                 NO, SKIP IT                                  
         L     R3,ABTRN            R3=TABLE OF BILLABLE ITEMS                   
         USING BTRND,R3            ADD ITEM TO BILLABLE TABLE                   
                                                                                
PESTF3   XC    BTRND(BTRNLQ),BTRND                                              
         MVC   BTRNWC,JXEWWC       WORKCODE                                     
         MVI   BTRNPORM,BTRNPROD   PRODUCTION                                   
         LA    R5,BTRNBK                                                        
         USING JOBD,R5                                                          
         ZAP   BTRNCOMR,JXEWCOMR   COMMISSION RATE                              
         ZAP   JOBNET,JXEWCNET      NET                                         
         ZAP   JOBGRS,JXEWCGRS      GROSS                                       
         ZAP   JOBCOM,JXEWCGRS      COMM                                        
         SP    JOBCOM,JXEWCNET                                                  
         ZAP   JOBCD,PZERO                                                      
         ZAP   JOBHRS,PZERO                                                     
                                                                                
PESTF5   AHI   R0,1                                                             
         ST    R0,NBTRN                                                         
         STC   R0,BTRNSBR          KEEP IN ESTIMATE SEQUENCE                    
         ST    R3,CBTRN                                                         
                                                                                
         CLI   CTAXOPT,0                                                        
         JE    PESTF7                                                           
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JO    PESTF7              YES, WILL GET TAX FROM 99 POSTING            
         MVC   WRKCCUR,JXEWWC                                                   
         GOTOR GETGO,GETGWCQ       UPDATE WORKCODE OPTIONS                      
                                                                                
         GOTOR CANTX,CANTRNQ       ADD CANADIAN TAX                             
         LA    R1,JXAALLO                                                       
CUR      USING JXBKD,R1                                                         
         LA    R2,JXATOT                                                        
TOT      USING JXBKD,R2                                                         
         LA    R6,BGSTBK           R5=A(GST BUCKETS)                            
                                                                                
         USING CTAXD,R6                                                         
         ZAP   DUB,CTAXTAX                                                      
         AP    CUR.JXBGST,DUB      ADD GST TO ACCUMS                            
         AP    TOT.JXBGST,DUB                                                   
                                                                                
         LA    R6,BPSTBK           R6=A(PST BUCKETS)                            
         ZAP   DUB,CTAXTAX                                                      
         AP    CUR.JXBPST,DUB      ADD PST TO ACCUMS                            
         AP    TOT.JXBPST,DUB                                                   
                                                                                
PESTF7   TM    PGMSTA,PGMUNB       IS IT UNBILLING?                             
         JNO   PESTF9                                                           
         XI    JOBNET+L'JOBNET-1,X'01'                                          
         XI    JOBCOM+L'JOBCOM-1,X'01'                                          
         XI    JOBGRS+L'JOBGRS-1,X'01'                                          
PESTF9   LA    R3,BTRNLQ(R3)                                                    
         LA    R4,JXEWLNQ(R4)                                                   
         CLI   0(R4),EOT                                                        
         JNE   PESTF3                                                           
         J     XIT                                                              
         DROP  R4,R5,R6,CUR,TOT                                                 
         EJECT                                                                  
*                                  ** ACCOUNT LAST ****                         
PESTL    L     R3,ABTRN            ESTIMATE BY WC                               
         L     R2,ABESEL           BILLING ESTIMATE ELEMENTS                    
         USING BESELD,R2                                                        
         ICM   R0,15,NBTRN                                                      
                                                                                
PESTL3   CLC   BTRNWC,WCBILLQ                                                   
         JE    PESTL5                                                           
         MVC   WRKCCUR,BTRNWC                                                   
         GOTOR GETGO,GETGWCQ       UPDATE WORKCODE OPTIONS                      
                                                                                
         XC    BESEL(BESLN2Q),BESEL                                             
         MVI   BESEL,BESELQ        BUILD ELEMENT                                
         MVI   BESLN,BESLN2Q                                                    
         MVC   BESWORK,BTRNWC                                                   
         LA    RF,BTRNBK                                                        
         USING JOBD,RF                                                          
         ZAP   BESPEBC,JOBNET                                                   
         ZAP   BESPEBCG,JOBGRS                                                  
         TM    CTAXOPT,CTAXOGST                                                 
         JNO   *+10                                                             
         MVC   BESVAT,GOTAXCOD                                                  
         TM    CTAXOPT,CTAXOPST                                                 
         JNO   *+16                                                             
         MVC   BESPST,GOPSTCOD                                                  
         MVC   BESPRV,GOPSTPRV                                                  
                                                                                
         TM    PGMSTA,PGMUNB       TEST UNBILLING                               
         JNO   PESTL4              NO,                                          
         XI    BESPEBC+L'BESPEBC-1,X'01'                                        
         XI    BESPEBCG+L'BESPEBCG-1,X'01'                                      
                                                                                
PESTL4   ST    R2,DMCB                                                          
         GOTOR AINTRNL,INTLESTQ    TEST/ADD INTERNAL INCOME                     
                                                                                
         LA    R2,BESLN2Q(R2)                                                   
         MVI   0(R2),EOT                                                        
                                                                                
PESTL5   LA    R3,BTRNLQ(R3)                                                    
         JCT   R0,PESTL3                                                        
         J     XIT                                                              
         DROP  R2,R3,R7,RF                                                      
         EJECT                                                                  
***********************************************************************         
* SPECIAL AMOUNT BILL                                                 *         
***********************************************************************         
                                                                                
SPEC     NTR1  LABEL=*                                                          
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,SPECACT                                                       
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
SPECACT  J     SPECF               ACCOUNT FIRST                                
         J     SPECL               ACCOUNT LAST                                 
                                                                                
SPECF    TM    PGMSTA,PGMUNB       TEST UNBILL A SPECIAL                        
         JZ    SPECF3              NO,                                          
         OI    JXBLSTAT,JXBLSTRN   YES, NEED READ TRANSACTIONS                  
                                                                                
SPECF3   LA    R4,JXAALLO                                                       
         USING JXBKD,R4                                                         
                                                                                
         L     R3,ABTRN            R3=TABLE OF BILLABLE ITEMS                   
         USING BTRND,R3            ADD ITEM TO BILLABLE TABLE                   
         XC    BTRND(BTRNLQ),BTRND                                              
         MVC   BTRNWC,SPACES       WORKCODE (NO WORKCODE)                       
                                                                                
         LA    R5,BTRNBK                                                        
         USING JOBD,R5                                                          
         LA    R0,BTBKN            YES, REVERSE BUCKETS                         
         LA    RE,BTRNBK                                                        
         ZAP   0(BKLQ,RE),PZERO                                                 
         LA    RE,BKLQ(RE)                                                      
         JCT   R0,*-10                                                          
                                                                                
         ZAP   BTRNCOMR,PZERO      COMMISSION RATE                              
         ZAP   JOBNET,JXBNET       NET                                          
         ZAP   JOBGRS,JXBGRS       GROSS                                        
         ZAP   JOBCOM,JXBCOM       COMM                                         
         ZAP   JOBCD,PZERO                                                      
         ZAP   JOBHRS,PZERO                                                     
                                                                                
         LA    R0,1                                                             
         ST    R0,NBTRN            ADD 1 ITEM TO TABLE                          
         ST    R3,CBTRN                                                         
                                                                                
         TM    PGMSTA,PGMUNB       TEST UNBILL A SPECIAL                        
         JO    SPECF5              YES,  GET TAX FROM OBA                       
                                                                                
         CLI   CTAXOPT,0                                                        
         JE    SPECF5                                                           
         GOTOR CANTX,CANTRNQ       ADD CANADIAN TAX                             
         LA    R5,BGSTBK           R5=A(GST BUCKETS)                            
         USING CTAXD,R5                                                         
         AP    JXBGST,CTAXTAX      ADD GST TO ACCUMS                            
         LA    R5,BPSTBK           R6=A(PST BUCKETS)                            
         AP    JXBPST,CTAXTAX      ADD PST TO ACCUMS                            
                                                                                
SPECF5   MVC   JXATOT,JXAALLO                                                   
         J     XIT                                                              
                                                                                
SPECL    J     XIT                                                              
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* WORKCODE ROUTINES                                                   *         
***********************************************************************         
                                                                                
WRKM     NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         SAM31 ,                   SWITCH TO 31 BIT MODE                        
         STC   R1,WRKACTN          SAVE CURRENT ACTION                          
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,WRKMACT                                                       
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
WRKMACT  J     WRKMBD              BUILD THE WORKCODE TABLE                     
         J     WRKMGT              UPDATE MUST FIRST GET CURRENT ENTRY          
         J     WRKMGT              GET CURRENT WORKCODE VALUES                  
                                                                                
WRKMBD   LA    R1,WRKPARM                                                       
         USING TABD,R1                                                          
         OC    TABNUM,TABNUM       TEST TABLE ALREADY BUILT                     
         JNZ   WRKMX               YES, GET OUT                                 
         DROP  R1                                                               
         L     R5,AWRKTAB          R5=WORKCODE NAME TABLE                       
         XC    0(WRKCLNQ,R5),0(R5)                                              
         XR    R3,R3                                                            
         LA    R0,MXWRK                                                         
         L     R2,ADLEDGER                                                      
         MVI   ELCODE,WCOELQ       WORKCODE ELEMENTS                            
         GOTOR GETEL                                                            
         STCM  R2,15,JXAWCODE      SET ADDRESS OF FIRST CODE                    
         J     WRKMBD32                                                         
                                                                                
         USING WCOELD,R2                                                        
WRKMBD3  GOTOR NEXTEL                                                           
WRKMBD32 JNE   WRKMBD11                                                         
         JCT   R0,*+6                                                           
         DC    H'0'                TABLE IS FULL                                
         OI    WCOCODE+1,C' '                        SOME HAVE BZ               
         MVC   WRKCWC-WRKCD(L'WRKCWC,R5),WCOCODE     CODE                       
         MVC   WRKCEL-WRKCD(L'WRKCEL,R5),WCOEL       WORKCODE ELEMENT           
         MVC   WRKCDSC-WRKCD(L'WRKCDSC,R5),WCODESC   DESCRIPTION                
         MVC   WRKCTYPE-WRKCD(L'WRKCTYPE,R5),WCOTYPE TYPE                       
         CLI   WRKCTYPE-WRKCD(R5),C' '                                          
         JH    *+8                                                              
         MVI   WRKCTYPE-WRKCD(R5),WRKCTOOP           MAKE IT OOP                
         MVI   WRKCFTYP-WRKCD(R5),WRKCFCST           DEFAULT IS COST            
         CLI   WCOTYPE,C'T'                          TEST TIME WORKCODE         
         JNE   *+8                                                              
         MVI   WRKCFTYP-WRKCD(R5),WRKCFTIM                                      
         MVC   WRKCGRP-WRKCD(L'WRKCGRP,R5),WCOGRP    GROUP                      
         ZAP   WRKCRTE-WRKCD(L'WRKCRTE,R5),=P'0'     RATE                       
         CLI   WRKCGRP-WRKCD(R5),C' '           TEST WORKCODE GROUP             
         JNH   WRKMBD4                NO,                                       
         GOTOR WCGRPN                 GET WORKCODE GROUP NAME                   
         MVI   ELCODE,WCOELQ          RESTORE ELCODE                            
                                                                                
         USING WCNTABD,R4                                                       
WRKMBD4  L     R4,AMONACC                                                       
         L     R4,ACMWCNMA-ACMD(R4) LOOK FOR NAME                               
WRKMBD5  CLI   0(R4),0             END OF NAME LIST                             
         JE    WRKMBD9                                                          
         CLC   WCNCODE,WCOCODE     MATCH WORK CODE                              
         JE    WRKMBD7                                                          
         LA    R4,WCNTABL(R4)                                                   
         J     WRKMBD5                                                          
                                                                                
WRKMBD7  MVC   WRKCNME-WRKCD(L'WRKCNME,R5),WCNNAME   WORK CODE NAME             
WRKMBD9  LA    R5,WRKCLNQ(R5)                                                   
         XC    0(WRKCLNQ,R5),0(R5)                                              
         AHI   R3,1                                                             
         J     WRKMBD3                                                          
                                                                                
WRKMBD11 LA    R1,WRKPARM                                                       
         USING TABD,R1                                                          
         MVC   TABADR,AWRKTAB      SET ADDRESS OF TABLE                         
         ST    R3,TABNUM           NUMBER IN TABLE                              
         LA    R0,MXWRK                                                         
         ST    R0,TABMAX                                                        
         LA    R0,WRKCLNQ                                                       
         ST    R0,TABLEN           RECORD LENGTH                                
         LA    R0,L'WRKCWC                                                      
         STCM  R0,7,TABKYL         KEY LENGTH                                   
         LA    R0,WRKCWC-WRKCD                                                  
         STC   R0,TABKYD           DISPLACEMENT TO KEY                          
         OI    TABSTA,TABSHI       TABLE IS IN HIGH CORE                        
         SAM24 ,                                                                
         GOTOR TQSORT              QSORT                                        
         GOTOR TSET                AND SET SEARCH PARAMETERS                    
         J     WRKMX                                                            
         DROP  R1,R2,R4                                                         
         EJECT                                                                  
WRKMGT   CLC   WRKCWC,WRKCCUR      IS THIS THE CURRENT?                         
         JE    WRKMUP                                                           
         LA    R1,WRKPARM                                                       
         USING TABD,R1                                                          
         LA    RF,WRKCCUR                                                       
         ST    RF,TABARG           SET A(SEARCH ARGUMENT)                       
         GOTOR TSRCH,WRKPARM,XA=OFF                                             
         JE    *+6                                                              
         DC    H'0'                MISSING WORKCODE                             
                                                                                
         L     R5,TABRTN                                                        
         MVC   WRKCD(WRKCLNQ),0(R5) MOVE DATA TO LOCAL STORAGE                  
         DROP  R1                                                               
                                                                                
WRKMUP   CLI   WRKACTN,WRKMUPQ     IS THE ACTION UPDATE?                        
         JNE   WRKMX                                                            
         LA    R2,WRKCEL                                                        
         USING WCOELD,R2                                                        
         L     RF,ADGOBLOC         UPDATE COMMISSION RATE                       
         USING GOBLOCKD,RF                                                      
         ZAP   WRKCRTE,GOAGYCOM                                                 
         MVI   WRKCSTA,0                                                        
         TM    WCOSTAT2,WCOSINC    IS THIS AN INCOME WORKCODE?                  
         JNO   *+8                                                              
         OI    WRKCSTA,WRKCSINC                                                 
         TM    WCOSTAT,WCOSMEDT    IS MEDIA TRANSFER BIT ON?                    
         JNO   WRKMX               NO                                           
         OI    WRKCSTA,WRKCSMT                                                  
                                                                                
WRKMX    SAM24 ,                   SET 24 BIT MODE                              
         J     XITY                                                             
         DROP  R2,RF                                                            
         EJECT                                                                  
***********************************************************************         
* GET WORKCODE GROUP NAME                                             *         
***********************************************************************         
                                                                                
WCGRPN   NTR1  LABEL=*                                                          
         LA    R2,DKEY                                                          
         USING WGRRECD,R2                                                       
         XC    DKEY,DKEY                                                        
         MVI   WGRKTYP,WGRKTYPQ                                                 
         MVI   WGRKSUB,WGRKSUBQ                                                 
         MVC   WGRKCPY,CPY                                                      
         MVC   WGRKUNT(2),PRODLGR                                               
         MVC   WGRKCODE,WRKCGRP-WRKCD(R5)                                       
         GOTOR ADMGR,ACCRDQ                                                     
         JNE   XIT                                                              
         MVC   AIO,AIO1                                                         
         GOTOR ADMGR,ACCGETQ                                                    
         L     R2,AIO                                                           
         MVI   ELCODE,NAMELQ                                                    
         GOTOR GETELN                                                           
         JNE   XIT                                                              
                                                                                
         USING NAMELD,R2           GET NAME FROM RECORD                         
         LLC   R1,NAMLN                                                         
         SHI   R1,NAMEREC+1-NAMELD                                              
         EXRL  R1,WGRPMVNN                                                      
         J     XIT                                                              
                                                                                
WGRPMVNN MVC   WRKCGRPN-WRKCD(0,R5),NAMEREC                                     
         DROP  R2                                                               
         EJECT                                                                  
XTRT     NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,XTRTACT                                                       
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
XTRTACT  J     XTWCNME             WORKCODE NAME                                
         J     XTWCDSC             WORKCODE DESCRIPTION                         
         J     XTDATE              DATE MMM/DD/YY                               
         J     XTCANME             CONTRA NAME                                  
         J     XTNARR              NARRATIVE                                    
         J     XTTRNF              TRANSFER INFO                                
         J     XTLINV              LONG INVOICE NUMBER                          
                                                                                
XTWCNME  OC    WRKCNME,WRKCNME     ANY WORKCODE NAME?                           
         JZ    XTWCDSC             NO, USE DESCRIPTION                          
         LA    R1,WRKCNME                                                       
         ST    R1,DMCB             SET A(OF WORKCODE NAME)                      
         LA    R1,WRKCNME+L'WRKCNME-1                                           
         LA    R0,L'WRKCNME        GET THE LENGTH                               
         CLI   0(R1),X'40'                                                      
         JNH   *+10                                                             
         BCTR  R1,0                                                             
         JCT   R0,*-10                                                          
         STC   R0,DMCB                                                          
         J     XTX                                                              
                                                                                
XTWCDSC  OC    WRKCDSC,WRKCDSC     ANY DESCRIPTION?                             
         JZ    XTXNO                                                            
         LA    R1,WRKCDSC                                                       
         ST    R1,DMCB             SET A(OF WORKCODE NAME)                      
         LA    R1,WRKCDSC+L'WRKCDSC-1                                           
         LA    R0,L'WRKCDSC        GET THE LENGTH                               
         CLI   0(R1),C' '                                                       
         JNH   *+10                                                             
         BCTR  R1,0                                                             
         JCT   R0,*-10                                                          
         STC   R0,DMCB                                                          
         J     XTX                                                              
                                                                                
         USING BTRND,R3                                                         
XTDATE   L     R3,CBTRN            TRANSACTION DATE MMM/DD/YY                   
         GOTOR DATCON,DMCB,(1,BTRNDTE),(8,DUB)                                  
         MVC   DMCB(4),DMCB+4                                                   
         J     XTX                                                              
                                                                                
XTCANME  L     R3,CBTRN            GET CONTRA NAME                              
         CLC   CPY,BTRNCA           TEST VALID ACCOUNT                          
         JNE   XTXNO                                                            
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(L'ACTKCULA),BTRNCA                                          
         GOTOR ADMGR,ACCHIQ                                                     
         CLC   DKEY,DIR            CONTRA ACCOUNT FOUND                         
         JNE   XTXNO                                                            
         ICM   R0,15,AIO           SAVE CALLERS IO ADDRESS                      
         MVC   AIO,AIO6                                                         
         GOTOR ADMGR,ACCGETQ       GET THE CONTRA ACCOUNT                       
         STCM  R0,15,AIO           RESTORE CALLERS IO ADDRESS                   
         L     R2,AIO6                                                          
         MVI   ELCODE,NAMELQ       FIND THE NAME                                
         GOTOR GETELN                                                           
         JNE   XTXNO                                                            
                                                                                
         USING NAMELD,R2                                                        
         LA    R1,NAMEREC                                                       
         ST    R1,DMCB             RETURN A(OF NAME)                            
         LLC   R0,NAMLN                                                         
         SHI   R0,2                AND LENGTH                                   
         STC   R0,DMCB                                                          
         J     XTX                                                              
                                                                                
         USING BTRND,R3                                                         
XTNARR   L     R3,CBTRN            GET NARRATIVE                                
         OC    BTRNDA,BTRNDA       TEST 'REAL TRANSACTION'                      
         JZ    XTXNO                                                            
         MVC   DA,BTRNDA                                                        
         ICM   R0,15,AIO           SAVE CALLERS IO ADDRESS                      
         MVC   AIO,AIO6            USE IO6 FOR THIS READ                        
         GOTOR ADMGR,ACCGETQ       GET THE TRANSACTION                          
         STCM  R0,15,AIO           RESTORE CALLERS IO ADDRESS                   
         L     R2,AIO6                                                          
         MVI   ELCODE,TRNELQ       FIND THE TRANSACTION ELEMENT                 
         GOTOR GETELN                                                           
         JNE   XTXNO                                                            
                                                                                
         USING TRNELD,R2                                                        
         LA    R1,TRNNARR                                                       
         ST    R1,DMCB             RETURN A(OF NARRATIVE)                       
         LLC   R0,TRNLN                                                         
         SHI   R0,TRNLN1Q                                                       
         JNP   XTXNO                                                            
         STC   R0,DMCB                                                          
         J     XTX                                                              
                                                                                
XTTRNF   MVC   WORK,SPACES                                                      
         MVI   WORK,0              GET TRANSFER INFO                            
         L     R3,CBTRN                                                         
         OC    BTRNDA,BTRNDA       TEST 'REAL TRANSACTION'                      
         JZ    XTXNO                                                            
         MVC   DA,BTRNDA                                                        
         ICM   R0,15,AIO           SAVE CALLERS IO ADDRESS                      
         MVC   AIO,AIO6            USE IO6 FOR THIS READ                        
         GOTOR ADMGR,ACCGETQ       GET THE TRANSACTION                          
         STCM  R0,15,AIO           RESTORE CALLERS IO ADDRESS                   
         L     R2,AIO6                                                          
         MVI   ELCODE,PXDELQ       FIND THE TRANSACTION ELEMENT                 
         GOTOR GETELN                                                           
         JNE   XTXNO                                                            
                                                                                
         USING PXDELD,R2                                                        
         L     RE,ADICO                                                         
         USING DICD,RE                                                          
         LA    R4,WORK+1                                                        
         LA    RF,DD@XFRTO         TRANSFER TO                                  
         LA    R1,L'DD@XFRTO                                                    
         CLI   PXDTYPE,C'T'                                                     
         JE    *+12                                                             
         LA    RF,DD@XFRFR         TRANSFER FROM                                
         LA    R1,L'DD@XFRFR                                                    
         BCTR  R1,0                                                             
         DROP  RE                                                               
         EXRL  R1,XINFMV4F                                                      
         LA    R4,3(R1,R4)                                                      
         MVC   0(L'PXDFRTO-3,R4),PXDFRTO+3                                      
         LA    R4,L'PXDFRTO-2(R4)                                               
         MVC   0(2,R4),=C'ON'                                                   
         LA    R4,4(R4)                                                         
         GOTOR DATCON,DMCB,(1,PXDDATE),(8,0(R4))                                
         GOTOR ADSQUASH,DMCB,WORK,L'WORK                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB                                                          
         MVC   DMCB(1),7(R1)                                                    
         J     XTX                                                              
                                                                                
XINFMV4F MVC   1(0,R4),0(RF)                                                    
                                                                                
         USING BTRND,R3                                                         
XTLINV   L     R3,CBTRN            GET LONG INVOICE                             
         LA    R1,BTRNREF          USE REFERENCE AS DEFAULT                     
         ST    R1,DMCB                                                          
         MVI   DMCB,L'BTRNREF                                                   
         TM    BTRNSTA,BTRNSLIN                                                 
         JNO   XTX                                                              
         OC    BTRNDA,BTRNDA                                                    
         JZ    XTX                                                              
         MVC   DA,BTRNDA                                                        
         ICM   R0,15,AIO           SAVE CALLERS IO ADDRESS                      
         MVC   AIO,AIO6            USE IO6 FOR THIS READ                        
         GOTOR ADMGR,ACCGETQ       GET THE TRANSACTION                          
         STCM  R0,15,AIO           RESTORE CALLERS IO ADDRESS                   
         L     R2,AIO6                                                          
         USING FFTELD,R2                                                        
         MVI   ELCODE,FFTELQ       LOOK FOR LONG INVOICE #                      
         GOTOR GETELN                                                           
         J     XTLIN32                                                          
XTLIN3   GOTOR NEXTEL                                                           
XTLIN32  JNE   XTX                                                              
         CLI   FFTTYPE,FFTTINVN                                                 
         JNE   XTLIN3                                                           
         MVC   WORK,SPACES                                                      
         LA    RF,WORK                                                          
         ST    RF,DMCB                                                          
         LLC   R1,FFTDLEN                                                       
         STC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EXRL  R1,XLINMVWD                                                      
         J     XTX                                                              
                                                                                
XTXNO    XC    DMCB(4),DMCB                                                     
XTX      J     XITY                                                             
                                                                                
XLINMVWD MVC   WORK(0),FFTDATA                                                  
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET ANALYSIS ACCOUNTS                                               *         
***********************************************************************         
                                                                                
ANAL     NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         LA    R1,ANLPARM                                                       
         USING TABD,R1                                                          
         OC    TABNUM,TABNUM       TEST FIRST TIME                              
         JNZ   ANAL3                                                            
         MVC   TABADR,AANLS        START OF TABLE                               
         LA    RF,ANLBLNQ          LENGTH OF RECORD                             
         ST    RF,TABLEN                                                        
         LA    RF,L'ANLINA         LENGTH OF KEY                                
         STCM  RF,7,TABKYL                                                      
         LA    RF,MXANAL           SET MAX NUMBER                               
         ST    RF,TABMAX                                                        
         J     ANAL5               GET AND ADD FIRST ITEM                       
                                                                                
ANAL3    LA    RF,ANLINA           SET ADDRESS OF SEARCH ARGUMENT               
         ST    RF,TABARG                                                        
         GOTOR TSRCH               SEARCH TABLE                                 
         JNE   ANAL5               NOT IN TABLE, ADD NEW                        
         L     R2,TABRTN                                                        
         MVC   ANLBLK(ANLBLNQ),0(R2) RETURN ANALYSIS DATA                       
         J     XITY                                                             
                                                                                
ANAL5    MVC   DKEY,SPACES                                                      
         LA    R3,DKEY                                                          
         USING ACTRECD,R3                                                       
         MVC   ACTKCULA,ANLINA     READ INCOME ACCOUNT                          
         GOTOR ADMGR,ACCHIQ                                                     
         CLC   DKEY,DIR                                                         
         JNE   ERACNF              ACCOUNT NOT FOUND                            
                                                                                
ANAL6    ICM   R0,15,AIO           SAVE CALLERS IO ADDRESS                      
         MVC   AIO,AIO6            USE IO6 FOR THIS READ                        
         GOTOR ADMGR,ACCGETQ       GET THE INCOME ACCOUNT                       
         STCM  R0,15,AIO           RESTORE CALLERS IO ADDRESS                   
         MVC   ANLGBA,SPACES                                                    
         LA    R3,ANLGBA           BUILD BILLING ANALYSIS ACCOUNT               
         MVC   ACTKCPY,CPY         COMPANY                                      
         MVI   ACTKUNT,ANALUQ      ANALYSIS UNIT                                
         MVI   ACTKLDG,GBILLQ      GROSS BILLING LEDGER                         
                                                                                
         L     R2,AIO6                                                          
         USING RSTELD,R2                                                        
         MVI   ELCODE,RSTELQ                                                    
         GOTOR GETELN                                                           
         JNE   *+10                                                             
         MVC   ACTKACT(L'RSTCOSTG),RSTCOSTG  ANALYSIS CODE                      
                                                                                
         MVI   ELCODE,SPAELQ                                                    
         L     R2,AIO6                                                          
         GOTOR GETELN                                                           
ANAL7    JNE   ANAL9                                                            
         USING SPAELD,R2                                                        
         CLI   SPATYPE,SPATANAL                                                 
         JNE   ANAL7                                                            
         MVC   ACTKACT,SPAAANAL                                                 
                                                                                
ANAL9    CLI   ACTKACT,C' '        IS IT A VALID ACCOUNT?                       
         JH    ANAL11                                                           
         CLI   PTYPE,INTLPQ        TEST INTERNAL INCOME POSTINGS                
         JE    ANAL11              YES, USE LEDGER RECORD(LIKE OLD 21)          
         MVC   ANLRVA,SPACES                                                    
         MVC   ANLRVA(1),CPY                                                    
         MVC   ANLRVA+1(L'REVNDUL+L'REVNDAC),REVNDUL  USE DEFAULT               
         MVC   ANLRVN,SPACES                                                    
         MVC   ANLRVN(10),=CL10'PRODUCTION'                                     
         CLC   REVNAC,SPACES                                                    
         JE    *+10                                                             
         MVC   ANLRVA,REVNAC                                                    
         J     ANAL13                                                           
                                                                                
ANAL11   MVC   ANLRVA,ANLGBA       NOW DO REVENUE ACCOUNT                       
         LA    R3,ANLRVA                                                        
         MVI   ACTKLDG,REVNLQ      REVENUE LEDGER                               
                                                                                
         MVC   ACCCDE,ANLGBA       GET ACCOUNT NAMES                            
         GOTOR AGETNAM                                                          
         JNE   ERACNF              SET 'ACCOUNT NOT FOUND' ERROR                
         TM    ACCSTA,ACCSBL       TEST ACCOUNT HAS BALANCE ELEMENT             
         JNO   ERACNB              NO,                                          
         MVC   ANLGBN,ACCNAM                                                    
                                                                                
         MVC   ACCCDE,ANLRVA                                                    
         GOTOR AGETNAM                                                          
         JNE   ERACNF              SET 'ACCOUNT NOT FOUND' ERROR                
         TM    ACCSTA,ACCSBL       TEST ACCOUNT HAS BALANCE ELEMENT             
         JNO   ERACNB              NO,                                          
         MVC   ANLRVN,ACCNAM                                                    
                                                                                
ANAL13   LA    R1,ANLPARM                                                       
         USING TABD,R1                                                          
         LA    RF,ANLBLK                                                        
         ST    RF,TABREC           SET ADDRESS OF NEW RECORD ADDRESS            
         GOTOR TBADD               BINARY ADD TO TABLE                          
         GOTOR TSET                SET SEARCH ARGUMENTS                         
         J     XITY                                                             
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* GET ACCOUNT NAMES                                                   *         
***********************************************************************         
                                                                                
GETNAM   NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         XC    ACCD(ACCDLNQ),ACCD                                               
         MVC   ACCNAM,SPACES                                                    
         LA    R1,NAMPARM                                                       
         USING TABD,R1                                                          
         OC    TABNUM,TABNUM       TEST FIRST TIME                              
         JNZ   GETNAM3                                                          
         MVC   TABADR,ANAMS        START OF TABLE                               
         LA    RF,ACCLNQ           LENGTH OF RECORD                             
         ST    RF,TABLEN                                                        
         LA    RF,L'ACCCDE         LENGTH OF KEY                                
         STCM  RF,7,TABKYL                                                      
         LA    RF,MXNAMS           SET MAX NUMBER                               
         ST    RF,TABMAX                                                        
         J     GETNAM5             GET AND ADD FIRST ITEM                       
                                                                                
GETNAM3  LA    RF,ACCCDE           SET ADDRESS OF SEARCH ARGUMENT               
         ST    RF,TABARG                                                        
         GOTOR TSRCH               SEARCH TABLE                                 
         JNE   GETNAM5             NOT IN TABLE, ADD NEW                        
         L     R2,TABRTN                                                        
         MVC   ACCD(ACCDLNQ),ACCD-ACCCDE(R2)  RETURN NAME, ETC.                 
         J     XITY                                                             
                                                                                
GETNAM5  MVC   DKEY,SPACES         GET FROM RECORD                              
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKCULA,ACCCDE     ACCOUNT                                      
         GOTOR ADMGR,ACCHIQ                                                     
         CLC   DKEY,DIR                                                         
         JNE   XITN                NOT FOUND                                    
         ICM   R0,15,AIO           SAVE CALLERS IO ADDRESS                      
         MVC   AIO,AIO6                                                         
         GOTOR ADMGR,ACCGETQ                                                    
         STCM  R0,15,AIO           RESTORE CALLERS IO ADDRESS                   
         L     R2,AIO6                                                          
         MVI   ELCODE,NAMELQ       FIND THE NAME                                
         GOTOR GETELN                                                           
         JNE   XITN                                                             
                                                                                
         USING NAMELD,R2           GET NAME FROM RECORD                         
         LLC   R1,NAMLN                                                         
         SHI   R1,NAMEREC+1-NAMELD                                              
         EXRL  R1,GNAMMVNN                                                      
                                                                                
         L     R2,AIO6                                                          
         MVI   ELCODE,ABLELQ       FIND BALANCE ELEMENT                         
         GOTOR GETELN                                                           
         JNE   *+8                                                              
         OI    ACCSTA,ACCSBL       SET 'HAS BALANCE ELEMENT'                    
                                                                                
         LA    R1,NAMPARM                                                       
         USING TABD,R1                                                          
         LA    RF,ACCCDE                                                        
         ST    RF,TABREC           SET ADDRESS OF NEW RECORD ADD                
         GOTOR TBADD               BINARY ADD TO TABLE                          
         GOTOR TSET                SET SEARCH ARGUMENTS                         
         J     XITY                                                             
                                                                                
GNAMMVNN MVC   ACCNAM(0),NAMEREC                                                
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* Get comments                                                        *         
***********************************************************************         
                                                                                
GETCMT   NTR1  LABEL=*                                                          
         MVC   CMTPRMS,0(R1)                                                    
         MVC   SAVELC,ELCODE       SAVE CALLERS ELEMENT CODE                    
         MVI   ELCODE,SCMELQ                                                    
         CLI   CMTSTA,C'W'         TEST 'SPECIAL' WPP COMMENTS                  
         JNE   GETCMT2                                                          
         LA    R4,CMTPRMS                                                       
         LA    R2,WORK                                                          
         USING SCMELD,R2                                                        
         XR    RF,RF                                                            
         ICM   RF,7,1(R4)          RF=A(WPP CODE)                               
         MVC   SCMNARR(6),0(RF)                                                 
         L     R3,ACMNT                                                         
         SR    R0,R0                                                            
GETCMT1  CLI   0(R3),0                                                          
         JE    GETCMT13                                                         
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         LA    R3,1(R3)                                                         
         J     GETCMT1                                                          
                                                                                
GETCMT2  L     R3,ACMNT                                                         
         MVI   0(R3),0             CLEAR FIRST COMMENT LENGTH                   
         LLC   R7,CMTSTA                                                        
         LA    R4,CMTPRMS                                                       
         LA    R0,3                MAX OF THREE RECORDS                         
                                                                                
GETCMT3  XR    R2,R2                                                            
         ICM   R2,7,1(R4)          R2=A(CLI, PROD OR JOB RECORD)                
         JZ    GETCMT6             RETURN TO CALLER                             
         GOTOR GETEL                                                            
         J     GETCMT52                                                         
GETCMT5  GOTOR NEXTEL                                                           
GETCMT52 JE    GETCMT7                                                          
         LA    R4,4(R4)                                                         
         JCT   R0,GETCMT3                                                       
GETCMT6  MVC   ELCODE,SAVELC       RESTORE CALLERS ELEMENT CODE                 
         J     XIT                                                              
                                                                                
         USING SCMELD,R2                                                        
GETCMT7  CLI   SCMTYPE,C'M'        IGNORE OLD TYPE                              
         JE    GETCMT5                                                          
         EXRL  R7,GCMTTMT0         TEST DESIRED TYPE                            
         JNO   GETCMT5                                                          
         TM    OPT1,OPT1MJMC       TEST MUST MATCH MEDIA/COMMENT                
         JNO   GETCMT13                                                         
         LA    R1,SCMNARR                                                       
GETCMT9  CLC   MEDIA,0(R1)                                                      
         JE    GETCMT13                                                         
         CLI   0(R1),C' '          MATCH ON FIRST NON-BLANK                     
         JH    GETCMT5                                                          
         LA    R1,1(R1)                                                         
         J     GETCMT9                                                          
                                                                                
GCMTTMT0 TM    SCMTYPE,0           TEST DESIRED TYPE                            
                                                                                
GETCMT13 XC    DKEY,DKEY           READ THE COMMENT RECORD                      
         LA    R6,DKEY                                                          
         USING SCMRECD,R6                                                       
         MVI   SCMKTYP,SCMKTYPQ    RECORD TYPE                                  
         MVC   SCMKCPY,CPY         COMPANY                                      
         MVC   SCMKCODE,SCMNARR    NARRATIVE CODE                               
         GOTOR ADMGR,ACCHIQ        GET DIRECTORY RECORD                         
         CLC   DKEY,DIR                                                         
         JE    GETCMT14            CAN'T GET IT -  SKIP IT                      
         CLI   CMTSTA,C'W'         TEST 'SPECIAL' WPP COMMENTS                  
         JNE   GETCMT5                                                          
         J     GETCMT6                                                          
                                                                                
GETCMT14 ICM   R6,15,AIO           SAVE CURRENT AIO                             
         MVC   AIO,AIO6            READ INTO IO6                                
         GOTOR ADMGR,ACCGETQ                                                    
         STCM  R6,15,AIO                                                        
         ST    R2,SAVER2                                                        
         L     R2,AIO6                                                          
         GOTOR GETELN              GET COMMENTS ELEMENTS FROM COMM REC.         
         J     GETCMT16                                                         
GETCMT15 GOTOR NEXTEL                                                           
GETCMT16 JE    GETCMT17                                                         
         CLI   CMTSTA,C'W'         TEST 'SPECIAL' WPP COMMENTS                  
         JE    GETCMT6             YES, EXIT                                    
         L     R2,SAVER2           RESTORE R2 TO ORIGINAL RECORD                
         J     GETCMT5                                                          
                                                                                
GETCMT17 LLC   R1,SCMLN                                                         
         SHI   R1,SCMLN1Q                                                       
         JNP   GETCMT15                                                         
         STC   R1,0(R3)            SAVE LENGTH OF COMMENT                       
         BCTR  R1,0                                                             
         EXRL  R1,GCMTMV3N         SAVE COMMENT IN BUFFER                       
         LA    R3,2(R1,R3)                                                      
         MVI   0(R3),0                                                          
         J     GETCMT15                                                         
                                                                                
GCMTMV3N MVC   1(0,R3),SCMNARR                                                  
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* Get extra job comments into extra job comment buffer                *         
***********************************************************************         
                                                                                
GETJLD   NTR1  LABEL=*                                                          
         L     RF,AXTRA                                                         
         MVI   0(RF),0             Clear output buffer                          
         TM    PGMAQ,PGMALL21      Test 21 style billing                        
         JZ    PA35                                                             
         CLI   FORMCDE,C'1'        Yes - test format 1                          
         JNE   PA35                                                             
K        USING AEXRECD,DKEY        Read job description record                  
         XC    K.AEXKEY,K.AEXKEY                                                
         MVI   K.AEXKTYP,AEXKTYPQ                                               
         MVI   K.AEXKSUB,AEXKSUBQ                                               
         L     RF,ADACC                                                         
         MVC   K.AEXKCPY,ACTKCPY-ACTRECD(RF)                                    
         MVC   K.AEXKULA,ACTKULA-ACTRECD(RF)                                    
                                                                                
         MVC   SAVAIO,AIO          Save current i/o pointer                     
         MVC   AIO,AIO6            Use IO6 for reading                          
                                                                                
         GOTOR ADMGR,ACCHIQ        Read high for directory record               
         CLC   DKEY,DIR                                                         
         JNE   GETJLDX                                                          
         LARL  R2,TEXTWORK         Point to text work area                      
         J     GETJLD04                                                         
                                                                                
GETJLD02 GOTOR ADMGR,ACCSEQQ       Get next job extension record                
         CLC   K.AEXKEY(AEXKSEQ-AEXRECD),DIR                                    
         JNE   GETJLD10                                                         
                                                                                
GETJLD04 GOTOR ADMGR,ACCGETQ       Read job extension record                    
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* Concatenate the text strings stored in individual elements in       *         
* TEXTWORK                                                            *         
***********************************************************************         
                                                                                
         L     R3,AIO                                                           
         AHI   R3,AEXRFST-AEXRECD                                               
         USING JLDELD,R3           R3=A(first element on extension)             
         MVI   BYTE,0                                                           
                                                                                
GETJLD06 CLI   JLDEL,0             test end of record                           
         JE    GETJLD02            Yes - get next job extension record          
         CLI   JLDEL,JLDELQ        Test job long description element            
         JNE   GETJLD08                                                         
         LR    R0,R2               R0=A(output area)                            
         LLC   R1,JLDLN                                                         
         SHI   R1,JLDDESC-JLDELD   R1=L'text in this element                    
         LA    RE,JLDDESC          RE=A(input area)                             
         LR    RF,R1                                                            
         MVCL  R0,RE               Move text to output area                     
         LR    R2,R0               Point to next output area                    
GETJLD08 LLC   R1,JLDLN            Bump to next element                         
         AR    R3,R1                                                            
         J     GETJLD06            Back to start of loop                        
         DROP  R3                                                               
                                                                                
GETJLD10 LARL  R3,TEXTWORK         Point to start of text work area             
         CR    R2,R3               Test have any comments at all                
         JE    GETJLDX             No                                           
         MVI   0(R2),FF            Set end of text work buffer                  
                                                                                
***********************************************************************         
* Format of text string is 'blahblah<nl>blahblahblah<nl>etc'          *         
* Convert to 'blahblah' 'blahblahblah' elements in comment buffer     *         
* If no <nl> commands are present chop the text into printable lines  *         
***********************************************************************         
                                                                                
JLDWIDTH EQU   79                  Width of text line                           
                                                                                
         LARL  R2,TEXTWORK         Point to start of text buffer                
         LR    R4,R2               R4=A(start of first line)                    
         L     R3,AXTRA            Point to output buffer                       
         LHI   R0,JLDWIDTH         Width of destination print line              
                                                                                
GETJLD22 CLI   0(R2),FF            Test end of text buffer                      
         JE    GETJLD24                                                         
         CLC   =C'<nl>',0(R2)      Test new line command                        
         JNE   GETJLD30                                                         
         CR    R2,R4               Test at start of text buffer                 
         JE    GETJLD26                                                         
                                                                                
GETJLD24 LR    R1,R2                                                            
         SR    R1,R4               R1=length of text string                     
         CHI   R1,JLDWIDTH         Test it fits into print line                 
         JNH   *+8                                                              
         LHI   R1,JLDWIDTH         No - set maximum width                       
         STC   R1,0(R3)            Set length of text string                    
         LA    R0,1(R3)            Point to output text area                    
         LR    RE,R4               Point to start of line                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LR    R3,R0               Point to next output area                    
         J     GETJLD28                                                         
                                                                                
GETJLD26 MVI   0(R3),1             Insert a blank line                          
         MVI   1(R3),C' '                                                       
         LA    R3,2(R3)            and bump over it                             
                                                                                
GETJLD28 CLI   0(R2),FF            Test end of buffer                           
         JE    GETJLD50                                                         
         LA    R4,4(R2)            Point to new start of line                   
         LA    R2,4(R2)                                                         
         LHI   R0,JLDWIDTH                                                      
         J     GETJLD22                                                         
                                                                                
GETJLD30 LA    R2,1(R2)            Bump to next input character                 
         JCT   R0,GETJLD22                                                      
                                                                                
         LR    R1,R2               Try to locate split point                    
         LHI   R0,50               within 50 characters of here                 
GETJLD32 CLI   0(R1),C' '          Test space                                   
         JE    GETJLD34            Yes - split text here                        
         BCTR  R1,0                                                             
         JCT   R0,GETJLD32                                                      
         LR    R1,R2               No split - just split the text               
         J     GETJLD36                                                         
                                                                                
GETJLD34 LA    R2,1(R1)            Point beyond split                           
                                                                                
GETJLD36 SR    R1,R4                                                            
         STC   R1,0(R3)            Set length of text string                    
         LA    R0,1(R3)            Point to output text area                    
         LR    RE,R4               Point to start of line                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LR    R3,R0               Point to next output area                    
         LR    R4,R2               Point to start of new line                   
         LHI   R0,JLDWIDTH                                                      
         J     GETJLD22                                                         
                                                                                
GETJLD50 MVI   0(R3),0             Set end of extra job comment buffer          
                                                                                
GETJLDX  MVC   AIO,SAVAIO                                                       
         J     XITN                                                             
         EJECT                                                                  
***********************************************************************         
* GET BILL NUMBER                                                     *         
***********************************************************************         
                                                                                
GTBNUM   NTR1  LABEL=*                                                          
         MVC   SHTBILNO,SPACES                                                  
         MVC   LNGBILNO,SPACES                                                  
         TM    PGMSTA,PGMDFT       TEST DRAFT                                   
         JO    GTBNUM32                                                         
         TM    BLNSTA,BLNSNXT      TEST GET NEXT NUMBER                         
         JNO   GTBNUM01                                                         
         PACK  FULL(4),BILNUM+2(4)                                              
         AP    FULL,PONE                                                        
         UNPK  BILNUM+2(4),FULL                                                 
         OI    BILNUM+5,X'F0'                                                   
         J     GTBNUM30                                                         
                                                                                
GTBNUM01 MVC   MNTH,RCDATE         DEFAULT MONTH IS RUN DATE                    
         SR    R1,R1                                                            
         ICM   R1,1,OPTMONTH       COMPANY PROFILE OPTION                       
         JZ    GTBNUM05                                                         
         CHI   R1,12                                                            
         JH    GTBNUM05                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB+6(2)    GET CHARARCTER MONTH NUMBER                  
         CLC   WORK+1(2),BILDTMM   IF PROFILE MONTH NOT HIGHER                  
         JL    GTBNUM05            THAN RUN DATE USE RUN DATE                   
         CLC   WORK+1(2),=C'12'    IF PROFILE IS FOR DECEMBER                   
         JNE   *+14                                                             
         CLC   BILDTMM,=C'01'      AND DATE IS JANUARY                          
         JE    GTBNUM05            USE RUN MONTH                                
         MVC   MNTH,WORK+1         MONTH FROM PROFILE                           
                                                                                
GTBNUM05 TM    JXRROPT,JXRRORRB    TEST RERUN BILL?                             
         JNO   GTBNUM07                                                         
         MVC   BILNUM,JXRRNUM      USE ORIGINAL NUMBER                          
         J     GTBNUM30                                                         
                                                                                
GTBNUM07 TM    PGMSTA,PGMUNB       TEST UNBILL                                  
         JNO   GTBNUM08            NO,                                          
         TM    OPTA,OPTAORGB       TEST USE ORIGINAL NUMBER                     
         JZ    GTBNUM08                                                         
         MVC   BILNUM,JXRRNUM      USE ORIGINAL NUMBER                          
         J     GTBNUM30                                                         
                                                                                
GTBNUM08 CLI   QSRTAREA,C' '       TEST "SOON" BILL NUMBER                      
         JNH   GTBNUM10                                                         
         MVC   BILNUM,QSRTAREA     BILL NUMBER IF "SOON"                        
         TM    JXSTAT1,JXS1DIST    TEST RETAIL                                  
         JNO   *+8                 NO,                                          
         OI    BLNSTA,BLNSNXT      SET, NEXT TIME GET NEXT #                    
         J     GTBNUM30                                                         
                                                                                
GTBNUM10 TM    PGMAQ,PGMALL21      TEST 21 BILLING                              
         JNZ   GTBNUM11            YES,                                         
         GOTOR GTBNUM14            CLIENT                                       
         GOTOR GTBNUM12            AGENCY NUMBER                                
         J     GTBNUM22                                                         
                                                                                
GTBNUM11 TM    SOONS,SOONRUN       TEST SOON 21                                 
         JNO   *+6                 NO,                                          
         DC    H'0'                YES, MUST ALREADY HAVE NUMBER                
         GOTOR GTBNUM12            AGENCY NUMBER                                
         GOTOR GTBNUM14            CLIENT                                       
         GOTOR GTBNUM16            CLIENT(OLD STYLE)                            
         J     GTBNUM20                                                         
                                                                                
GTBNUM12 ICM   R2,15,ALDGPMD       TEST AGENCY LEVEL ELEMENT                    
         BZR   RE                                                               
         OI    WRIT,WRITLDG        SET WRITE LEDGER RECORD                      
         LA    R1,BLNSAGY          SET AGENCY NUMBER                            
         LA    RF,BLNSTA                                                        
         J     GTBNUM24                                                         
                                                                                
GTBNUM14 ICM   R2,15,ACLIPMD       TEST CLIENT                                  
         BZR   RE                                                               
         J     GTBNUM18                                                         
GTBNUM16 ICM   R2,15,ACLINUM       TEST CLIENT NUMBER ELEMENT                   
         BZR   RE                                                               
GTBNUM18 OI    WRIT,WRITLVA        SET WRITE LEVEL A RECORD                     
         LA    R1,BLNSCLI          SET CLIENT NUMBER                            
         LA    RF,BLNSTA                                                        
         J     GTBNUM24                                                         
                                                                                
GTBNUM20 ICM   R2,15,APMDEL        USE MEDIA RECORD                             
         JZ    GTBNUM22                                                         
         LA    R1,BLNSMED          SET MEDIA NUMBER                             
         LLC   RF,APMDEL           RELATIVE NUMBER FOR MEDIA STATUS             
         LA    RF,MEDSTA(RF)                                                    
         OI    WRIT,WRITMED        SET WRITE MEDIA RECORD                       
         J     GTBNUM24                                                         
                                                                                
GTBNUM22 AP    RCSPECNO+1(3),PONE                                               
         OI    RCSPECNO+L'RCSPECNO-1,X'0F'                                      
         UNPK  BILNUM,RCSPECNO                                                  
         J     GTBNUM30                                                         
                                                                                
         USING PMDELD,R2                                                        
GTBNUM24 CLI   PMDEL,PMDELQ                                                     
         JNE   GTBNUM28                                                         
         EXRL  R1,GBNMTMF0         TEST NUMBER ALREADY SET                      
         JO    GTBNUM26            YES,                                         
         EXRL  R1,GBNMOIF0         SET FOR NEXT TIME                            
         CLC   MNTH,PMDLBILL       SAME MONTH                                   
         JE    *+16                YES,                                         
         MVC   PMDLBILL(2),MNTH        SET NEW MONTH                            
         MVC   PMDLBILL+2(4),PMDRBILL  AND RESET VALUE                          
         MVC   PMDFBILL,PMDLBILL       SAVE START NUMBER                        
                                                                                
GTBNUM26 MVC   BILNUM,PMDLBILL                                                  
         PACK  FULL,PMDLBILL+2(4)                                               
         AP    FULL,PONE                                                        
         UNPK  PMDLBILL+2(4),FULL                                               
         OI    PMDLBILL+5,X'F0'                                                 
         J     GTBNUM30                                                         
                                                                                
GBNMTMF0 TM    0(RF),0                                                          
GBNMOIF0 OI    0(RF),0                                                          
                                                                                
GTBNUM28 LR    R3,R2                                                            
         USING NUMELD,R3                                                        
         L     R2,APMDEL                                                        
         CLC   MNTH,PMDLBILL       SAME MONTH                                   
         JE    *+16                YES,                                         
         MVC   NUMAFT(2),MNTH      RESET MONTH                                  
         MVC   NUMAFT+2(4),PMDRBILL                                             
         MVC   BILNUM,NUMAFT                                                    
         PACK  FULL,NUMAFT                                                      
         AP    FULL,PONE                                                        
         UNPK  NUMAFT,FULL                                                      
         OI    NUMAFT+5,X'F0'                                                   
                                                                                
GTBNUM30 LLC   RF,RCDATE+7             RCDATE MM/DD/YY - GET YEAR               
         LLC   RE,RCDATE+1             GET MONTH                                
         CLI   RCDATE,C'0'             0-9 ARE OK                               
         JE    *+8                                                              
         SHI   RE,47                   10=A, 11=B, 12=C                         
         MVC   SHTBILNO(2),BILNUM      MONTH                                    
         CLI   OPTBNPFX,1              TEST OPTION FOR MONTH/YEAR               
         JNE   *+12                                                             
         STC   RE,SHTBILNO                                                      
         STC   RF,SHTBILNO+1                                                    
         CLI   OPTBNPFX,2              TEST OPTION FOR YEAR/MONTH               
         JNE   *+12                                                             
         STC   RF,SHTBILNO                                                      
         STC   RE,SHTBILNO+1                                                    
         MVI   SHTBILNO+2,C'-'         MM(MY)(YM)-NNNN                          
         MVC   SHTBILNO+3(4),BILNUM+2                                           
         MVC   BILNUM(2),SHTBILNO                                               
                                                                                
         MVC   LNGBILNO+2(L'SHTBILNO),SHTBILNO                                  
         MVC   LNGBILNO(1),MEDIA                                                
         MVI   LNGBILNO+1,C'-'         MEDIA-SHORT BILL NUMBER                  
         J     XITY                                                             
                                                                                
GTBNUM32 MVC   SHTBILNO(L'DRAFT),DRAFT     DRAFT BILL NUMBER                    
         MVC   LNGBILNO(L'SHTBILNO),SHTBILNO                                    
         J     XITY                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GETOPT CALLS                                                        *         
***********************************************************************         
                                                                                
GETGO    NTR1  LABEL=*                                                          
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         L     RF,AMONACC                                                       
         MVC   GOAKEY,ACMALTN-ACMD(RF)                                          
         MVC   GOSELCUL,CPY        SET JOB DETAIL                               
         MVC   GOSELCLI,CLI                                                     
         MVC   GOSELPRO,PRD                                                     
         MVC   GOSELJOB,JOB                                                     
         XC    GOSELWC,GOSELWC                                                  
         MVC   GOACLI,ADHEIRA                                                   
         MVC   GOAPRO,ADHEIRB                                                   
         MVC   GOAJOB,ADHEIRC                                                   
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,GETGOACT                                                      
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
GETGOACT J     GETGAS              AGENCY/STUDIO                                
         J     GETGWC              WORKCODE LEVEL                               
         J     GETGJO              JOB LEVEL                                    
                                                                                
GETGAS   MVC   GOSELCUL,AGYLCUL    GET AGENCY/STUDIO OPTIONS                    
         MVC   GOSELCLI(L'AGYLCLI),AGYLCLI                                      
         MVC   GOSELPRO(L'AGYLPRO),AGYLPRO                                      
         MVC   GOSELJOB(L'AGYLJOB),AGYLJOB                                      
         XC    GOACLI,GOACLI                                                    
         XC    GOAPRO,GOAPRO                                                    
         XC    GOAJOB,GOAJOB                                                    
         GOTOR GETOP                                                            
         J     XIT                                                              
                                                                                
GETGWC   MVC   GOSELWC,WRKCCUR     GET WORKCODE LEVEL OPTIONS                   
         GOTOR GETOP                                                            
         XC    WRKCWC,WRKCWC       FORCE UPDATE                                 
         GOTOR AWRKM,WRKMUPQ       UPDATE WORKCODE TABLE                        
         J     XIT                                                              
                                                                                
GETGJO   GOTOR GETOP                                                            
         J     XIT                                                              
                                                                                
GETOP    LR    R0,RE                                                            
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JNO   GETOP3              YES, PRINT IO COUNT AT ENTER                 
         GOTOR AIOCNTR,DMCB,GETOPMO                                             
GETOP3   GOTOR GETOPT,DMCB,ADGOBLOC                                             
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JNO   GETOP5              YES, PRINT IO COUNT AT ENTER                 
         GOTOR AIOCNTR,DMCB,(C'X',GETOPMO)                                      
GETOP5   LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* INTERNAL INCOME ROUTINES                                            *         
***********************************************************************         
                                                                                
INTRNL   NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,INTRNACT                                                      
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
INTRNACT J     INTTRN              PROCESS TRANSACTION                          
         J     INTEST              PROCESS ESTIMATE                             
         J     INTELM              BUILD INCOME ELEMENTS                        
         J     INTFST              SET DATA FOR FIRST POSTING                   
         J     INTNXT              SET DATA FOR NEXT                            
                                                                                
INTTRN   L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLC   TRNANAL,WCBILLQ     IS THIS A PREVIOUS BILL?                     
         JE    INTPRV                                                           
                                                                                
                                                                                
         TM    BILTTYP,TOTL+ONEL   TEST TOTAL BILL                              
         JZ    XIT                                                              
         L     R4,CBTRN            GET AMOUNT FOR THIS TRANSACTION              
         USING BTRND,R4                                                         
         CLC   BTRNCUL,=C'SK'                                                   
         JE    INTTRN3                                                          
         CLC   BTRNCUL,=C'SI'                                                   
         JNE   XIT                                                              
                                                                                
INTTRN3  ZAP   CURAMNT,BTRNBK+(JOBNET-JOBD)(L'JOBNET)                           
         J     INTEST3                                                          
                                                                                
INTEST   L     R4,DMCB                                                          
         USING BESELD,R4           R4 POINTS TO BILLING ESTIMATE                
         ZAP   CURAMNT,BESPEBC                                                  
         TM    PGMSTA,PGMUNB       LEAVE AMOUNTS AS NEGATIVE FOR B7             
         JNO   INTEST3             NO,                                          
         ZAP   CURAMNT,BESPEBC                                                  
         XI    CURAMNT+L'CURAMNT-1,X'01'                                        
                                                                                
INTEST3  TM    WRKCSTA,WRKCSINC    IS IT AN INCOME WORKCODE?                    
         JZ    XIT                                                              
         GOTOR INTCUR                                                           
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
INTCUR   NTR1  LABEL=*             ADD TO "CURRENT TOTAL" COLUMN                
         L     R3,AINTLB                                                        
                                                                                
         USING INTLD,R3                                                         
INTCUR3  CLI   INTLD,EOT           END OF TABLE?                                
         JE    INTCUR7             NO, UPDATE CURRENT TABLE ONLY                
         CLC   INTLWKC,WRKCWC      YES, FIND THIS WORKCODE                      
         JE    INTCUR5                                                          
         LA    R3,INTLLNQ(R3)      GET NEXT TABLE ENTRY                         
         J     INTCUR3             KEEP LOOKING                                 
                                                                                
INTCUR5  AP    INTLCUR,CURAMNT     ADD TO CURRENT TOTAL                         
         GOTOR INTADJ              GET CURRENT POSTINGS                         
         J     XIT                                                              
                                                                                
INTCUR7  XC    INTLD(INTLLNQ),INTLD                                             
         MVC   INTLWKC,WRKCWC      ADD NEW WORKCODE                             
         ZAP   INTLCUR,CURAMNT     CURRENT VALUE                                
         ZAP   INTLPRV,PZERO       PREVIOUS POSTINGS                            
         ZAP   INTLACL,PZERO       CUMULATIVE ACCRUAL                           
         ZAP   INTLAMNT,PZERO      CURRENT POSTING                              
         ZAP   INTLINAM,PZERO      CURRENT ACCRUAL                              
                                                                                
         L     R7,ADGOBLOC         ADD THE REST OF THE NEW ENTRY                
         USING GOBLOCKD,R7                                                      
         L     R7,GOAEXT                                                        
         USING GOXBLKD,R7                                                       
         MVC   INTLDRAC,GOINCDR+1  DEFAULT DEBIT/CREDIT ACCOUNTS                
         MVC   INTLCRAC,GOINCCR+1                                               
         OI    INTLSTAT,INTSCUR    SET CURRENT ITEM FLAG                        
         MVI   INTLD+INTLLNQ,EOT                                                
         GOTOR INTADJ                                                           
         J     XIT                                                              
         DROP  R3,R7                                                            
         EJECT                                                                  
INTPRV   LR    R2,R4               PREVIOUS BILLING                             
         OI    JFLG,JFPWOI         SET PREV. BILLS WITHOUT INCOME               
         MVI   ELCODE,INCELQ       LOOK FOR INCOME ELEMENTS                     
                                                                                
INTPRV3  GOTOR NEXTEL                                                           
         JNE   XIT                                                              
         NI    JFLG,ALL-(JFPWOI)   TURNOFF 'NO INCOME ELEMENTS'                 
         USING INCELD,R2                                                        
         L     R3,AINTLB           R3=A(INTERNAL INCOME TABLE)                  
                                                                                
         USING INTLD,R3                                                         
INTPRV5  CLI   INTLD,EOT                                                        
         JE    INTPRV9                                                          
         CLC   INCWRK2,INTLWKC                                                  
         JE    INTPRV7             MATCH WORKCODE                               
         LA    R3,INTLLNQ(R3)                                                   
         J     INTPRV5                                                          
                                                                                
INTPRV7  CLI   INCCACC,0           CREDIT ACCOUNT                               
         JE    *+10                                                             
         MVC   INTLCRAC,INCCACC    CREDIT ACCOUNT TO TABLE                      
         CLI   INCLN,INCLN2Q                                                    
         JL    INTPRV8                                                          
         OC    INCDACC,INCDACC                                                  
         JZ    *+10                                                             
         MVC   INTLDRAC,INCDACC    PREVIOUS DEBIT ACCOUNT                       
                                                                                
INTPRV8  AP    INTLPRV,INCAMNT     ADD TO PREVIOUS POSTINGS                     
         LA    RF,INCAMNT                                                       
         CLI   INCLN,INCLN2Q                                                    
         JL    *+8                                                              
         LA    RF,INCINAM                                                       
         AP    INTLACL,0(L'INCAMNT,RF) ADD CUMULATIVE ACCRUAL                   
         GOTOR INTADJ                                                           
         J     INTPRV3                                                          
                                                                                
INTPRV9  XC    INTLD(INTLLNQ),INTLD                                             
         MVC   INTLWKC,INCWRK2     ADD NEW WORKCODE                             
         ZAP   INTLCUR,PZERO       CURRENT VALUE                                
         ZAP   INTLPRV,PZERO       PREVIOUS POSTINGS                            
         ZAP   INTLACL,PZERO       CUMULATIVE ACCRUAL                           
         ZAP   INTLAMNT,PZERO      CURRENT POSTING                              
         ZAP   INTLINAM,PZERO      CURRENT ACCRUAL                              
         MVC   INTLCRAC,INCCACC    CREDIT ACCOUNT TO TABLE                      
         CLI   INCLN,INCLN2Q                                                    
         JL    *+10                                                             
         MVC   INTLDRAC,INCDACC    DEBIT ACCOUNT                                
         L     R7,ADGOBLOC         ADD THE REST OF THE NEW ENTRY                
         USING GOBLOCKD,R7                                                      
         L     R7,GOAEXT                                                        
         USING GOXBLKD,R7                                                       
         CLI   INTLDRAC,0                                                       
         JH    *+10                                                             
         MVC   INTLDRAC,GOINCDR+1  DEFAULT DEBIT/CREDIT ACCOUNTS                
         CLI   INTLCRAC,0                                                       
         JH    *+10                                                             
         MVC   INTLCRAC,GOINCCR+1                                               
         MVI   INTLD+INTLLNQ,EOT                                                
         J     INTPRV8                                                          
         DROP  R2,R7                                                            
                                                                                
*              CALCULATE CURRENT ADJUSTMENTS                                    
                                                                                
INTADJ   ZAP   INTLAMNT,INTLCUR    CURRENT TOTAL                                
         SP    INTLAMNT,INTLPRV    LESS PREVIOUS=CURRENT FOR PERIOD             
         ZAP   INTLINAM,INTLAMNT   INCOME ACCRUAL AMOUNT                        
         TM    BILTTYP,ESTM        IS IT  %EST?                                 
         BOR   RE                                                               
         ZAP   INTLINAM,PZERO      FOR TOTAL AND ONELINE                        
         SP    INTLINAM,INTLACL    REVERSE ACCRUAL                              
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
*              BUILD INCOME ELEMENTS                                            
                                                                                
INTELM   L     R2,AINCEL           R2=ELEMENT BUFFER                            
         USING INCELD,R2                                                        
         MVI   0(R2),0                                                          
         L     R3,AINTLB                                                        
         TM    BILTTYP,ESTM        %EST                                         
         JO    INTELM3                                                          
         TM    JFLG,JFPWOI         PREVIOUS BILLS W/O INCOME ELEMENTS           
         JO    XIT                 CAN'T DO ADJUSTMENTS                         
                                                                                
         USING INTLD,R3                                                         
INTELM3  XC    0(INCLN2Q,R2),0(R2)                                              
         CLI   0(R3),EOT                                                        
         JE    XIT                                                              
         MVI   INCEL,INCELQ                                                     
         MVI   INCLN,INCLN2Q                                                    
         MVC   INCWRK2,INTLWKC     WORKCODE                                     
         MVC   INCCACC,INTLCRAC    CREDIT ACCOUNT                               
         MVC   INCDACC,INTLDRAC    DEBIT ACCOUNT                                
                                                                                
         ZAP   INCAMNT,INTLAMNT    CURRENT AMOUNT                               
         ZAP   INCINAM,INTLINAM    CURRENT ACCRUAL                              
         LA    R2,INCLN2Q(R2)                                                   
         LA    R3,INTLLNQ(R3)                                                   
         J     INTELM3                                                          
         DROP  R2,R3                                                            
         EJECT                                                                  
*              SET DATA FOR POSTINGS                                            
                                                                                
INTNXT   L     R3,LASTINT                                                       
         USING INTLD,R3                                                         
         LA    R3,INTLLNQ(R3)                                                   
         J     INTFST3                                                          
                                                                                
INTFST   XC    LASTINT,LASTINT                                                  
         TM    JFLG,JFPWOI         PREVIOUS BILLS W/O INCOME ELEMENTS           
         JO    XITN                                                             
         L     R3,AINTLB                                                        
INTFST3  CLI   0(R3),EOT                                                        
         JE    XITN                                                             
         ST    R3,LASTINT                                                       
         MVC   INTWKC,INTLWKC                                                   
         MVC   INTDRACC(1),CPY                                                  
         MVC   INTDRACC+1(14),INTLDRAC                                          
         MVC   INTCRACC(1),CPY                                                  
         MVC   INTCRACC+1(14),INTLCRAC                                          
         ZAP   INTAMNT,INTLAMNT    CURRENT TOTAL                                
         ZAP   INTINAM,INTLINAM                                                 
         MVC   INTSTAT,INTLSTAT                                                 
                                                                                
         MVC   ANLINA,INTCRACC                                                  
         OC    INTLCRAC,INTLCRAC                                                
         JZ    XITY                                                             
         GOTOR AANAL                                                            
         J     XITY                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* AGENCY/STUDIO LINKS                                                 *         
***********************************************************************         
                                                                                
STUDIO   NTR1  LABEL=*                                                          
         OI    JOBTYP,JOBTSTUD     SET STUDIO JOB                               
         L     R2,ALNKEL                                                        
         USING LNKELD,R2                                                        
         OC    LNKSTUD,LNKSTUD                                                  
         JZ    ERSTUD                                                           
         OC    LNKAGJB,LNKAGJB                                                  
         JZ    ERSTUD                                                           
                                                                                
         MVC   AGYLCUL,JOBCDE                                                   
         MVC   AGYLCPJ,LNKAGJB                                                  
         MVC   AGYSTUD,LNKSTUD                                                  
         MVC   AGYLNAM,SPACES                                                   
         MVC   ACCCDE,AGYLNK                                                    
         GOTOR AGETNAM                                                          
         JNE   ERACNF              SET 'ACCOUNT NOT FOUND' ERROR                
         TM    ACCSTA,ACCSBL       TEST ACCOUNT HAS BALANCE ELEMENT             
         JNO   ERACNB              NO,                                          
         MVC   AGYLNAM,ACCNAM      NAME OF AGENCY LINK                          
         DROP  R2                                                               
                                                                                
         MVC   DKEY,SPACES                                                      
         LA    R2,DKEY             GET AGENCY JOB                               
         USING ACTRECD,R2                                                       
         MVC   ACTKCPY(3),AGYLCUL                                               
         MVC   ACTKACT,AGYLCPJ                                                  
         GOTOR ADMGR,ACCHIQ                                                     
         CLC   DKEY,DIR                                                         
         JNE   ERSTUD                                                           
         ICM   R0,15,AIO           SAVE CALLERS IO ADDRESS                      
         MVC   AIO,AIO6            SET IO                                       
         GOTOR ADMGR,ACCGETQ                                                    
         STCM  R0,15,AIO           RESTORE CALLERS IO ADDRESS                   
         L     R2,AIO6                                                          
         MVI   ELCODE,JOBELQ       GET JOB ELEMENT                              
         GOTOR GETELN                                                           
         JNE   STUDIO3                                                          
         USING JOBELD,R2                                                        
         TM    JOBSTA1,JOBSXJOB    IS IT AN X-JOB?                              
         JNO   STUDIO3                                                          
         OI    AGYSTAT,AGYSXJOB    SET AGENCY JOB IS XJOB                       
                                                                                
STUDIO3  MVC   AGYLCLK,SPACES      SET CLIENT KEY                               
         MVC   AGYLCLK(6),AGYLNK                                                
         MVC   ACCCDE,AGYLCLK      GET CLIENT NAME                              
         GOTOR AGETNAM                                                          
         MVC   AGYLCLN,ACCNAM      NAME OF CLIENT                               
                                                                                
         LA    R2,DKEY                                                          
         USING STURECD,R2                                                       
         XC    STUKEY,STUKEY       GET STUDIO RECORD                            
         MVI   STUKTYP,STUKTYPQ                                                 
         MVI   STUKSUB,STUKSUBQ                                                 
         MVC   STUKCPY,CPY                                                      
         MVC   STUKCODE,AGYSTUD                                                 
         GOTOR ADMGR,ACCHIQ                                                     
         CLC   DKEY,DIR                                                         
         JNE   ERSTUD                                                           
         ICM   R0,15,AIO           SAVE CALLERS IO ADDRESS                      
         MVC   AIO,AIO6            SET IO                                       
         GOTOR ADMGR,ACCGETQ                                                    
         STCM  R0,15,AIO           RESTORE CALLERS IO ADDRESS                   
         L     R2,AIO6                                                          
         MVI   ELCODE,STUELQ       GET STUDIO ELEMENT                           
         GOTOR GETELN                                                           
         JNE   ERSTUD                                                           
         USING STUELD,R2                                                        
         MVC   AGYSTVND,STUVEND    VENDOR CODE                                  
         MVC   ACCCDE,AGYSTVND                                                  
         GOTOR AGETNAM                                                          
         MVC   AGYSTVNM,ACCNAM     VENDOR NAME                                  
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         L     R7,GOAEXT                                                        
         USING GOXBLKD,R7                                                       
         MVC   AGYICCR,GOICR       CREDIT ACCOUNT                               
         MVC   AGYBLDR,GOBDB       BILLING DEBIT ACCOUNT                        
                                                                                
         GOTOR GETGO,GETGASQ       GET AGENCY/STUDIO OPTIONS                    
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         MVC   AGYOFC,GOEFFOFC                                                  
         L     R7,GOAEXT                                                        
         USING GOXBLKD,R7                                                       
         CLI   GOINTPST,YES        WANT INTERCOMPANY POSTINGS?                  
         JNE   *+8                                                              
         OI    AGYSTAT,AGYSICMP                                                 
         CLI   GOHOLD,YES          HOLD INTERCOMPANY DEBITS?                    
         JNE   *+8                                                              
         OI    AGYSTAT,AGYSIHLD                                                 
                                                                                
         GOTOR GETGO,GETGJOQ       RESTORE JOB OPTIONS                          
         J     XIT                                                              
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* INTERCOMPANY ROUTINES                                               *         
***********************************************************************         
                                                                                
ICMPY    NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,ICMPYACT                                                      
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
ICMPYACT J     ICMPTRN             PROCESS TRANSACTION                          
         J     ICMPFST             RETURN FIRST WORKCODE AND AMOUNT             
         J     ICMPNXT             RETURN NEXT                                  
                                                                                
ICMPTRN  L     R4,AICMPWC                                                       
         USING ICMPD,R4                                                         
         LA    RF,WRKCCUR          RF=A(WORKCODE)                               
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         L     R7,GOAEXT                                                        
         USING GOXBLKD,R7                                                       
         OC    GOAGWC,GOAGWC                                                    
         JZ    *+8                                                              
         LA    RF,GOAGWC           GET WC POSTING OVERRIDE                      
         LA    R0,MXICMP                                                        
         DROP  R7                                                               
                                                                                
ICMPTRN3 CLI   0(R4),EOT           END OF TABLE - ADD NEW ENTRY                 
         JE    ICMPTRN5                                                         
         CLC   ICMPWC,0(RF)        MATCH WORKCODE                               
         JE    ICMPTRN7                                                         
         LA    R4,ICMPLNQ(R4)                                                   
         JCT   R0,ICMPTRN3                                                      
         DC    H'0'                TABLE IS FULL                                
                                                                                
ICMPTRN5 MVC   ICMPWC,0(RF)        ADD NEW ITEM TO TABLE                        
         ZAP   ICMPAMT,PZERO                                                    
         MVI   ICMPD+ICMPLNQ,EOT                                                
                                                                                
ICMPTRN7 L     R5,CBTRN            GET AMOUNT FOR THIS TRANSACTION              
         USING BTRND,R5                                                         
         LA    RF,BTRNBK+(JOBGRS-JOBD)                                          
         TM    JXSTAT3,JXS3PNET    PAY=NET?                                     
         JNO   *+8                                                              
         LA    RF,BTRNBK+(JOBNET-JOBD)                                          
         AP    ICMPAMT,0(L'JOBGRS,RF)                                           
         SP    ICMPAMT,BTRNBK+(JOBCD-JOBD)(L'JOBCD)                             
         J     XIT                                                              
         DROP  R5                                                               
                                                                                
ICMPFST  XC    LASTICM,LASTICM    RETURN FIRST WORKCODE AND AMOUNT              
         L     R4,AICMPWC                                                       
ICMPFST3 CLI   0(R4),EOT                                                        
         JE    XITN                                                             
         ST    R4,LASTICM                                                       
         MVC   AGYWC,ICMPWC                                                     
         ZAP   AGYAMNT,ICMPAMT                                                  
         TM    PGMSTA,PGMUNB       UNBILLING?                                   
         JNO   XITY                NO                                           
         XI    AGYAMNT+L'AGYAMNT-1,X'01'                                        
         J     XITY                                                             
                                                                                
ICMPNXT  L     R4,LASTICM          RETURN NEXT WORKCODE AND AMOUNT              
         LA    R4,ICMPLNQ(R4)                                                   
         J     ICMPFST3                                                         
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BINARY ADD AN ITEM TO A TABLE                                       *         
***********************************************************************         
                                                                                
TBADD    NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         LR    R5,R1                                                            
         USING TABD,R5                                                          
         MVC   BINREC(BINPLNQ),TABREC                                           
         OI    BINREC,X'01'        ADD IF NOT FOUND                             
         GOTOR BINSRCH,BINREC                                                   
         OC    BINREC,BINREC                                                    
         JNZ   *+6                 TABLE IS FULL                                
         DC    H'0'                                                             
         MVC   TABNUM,BINNUM       UPDATE NUMBER                                
         MVC   TABRTN,BINREC                                                    
         CLI   TABFND,TABFNDQ      RECORD FOUND                                 
         JE    XITY                                                             
         J     XITN                RECORD NOT FOUND(ADDED)                      
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* QSORT INTERFACE                                                     *         
***********************************************************************         
                                                                                
TQSORT   NTR1  LABEL=*                                                          
         LR    R5,R1                                                            
         USING TABD,R5                                                          
         MVC   QSPTAB,TABADR       ADDRESS OF TABLE                             
         MVC   QSPNUM,TABNUM       NUMBER IN TABLE                              
         MVC   QSPLEN,TABLEN       LENGTH                                       
         SR    R0,R0                                                            
         ICM   R0,7,TABKYL                                                      
         ST    R0,QSPKEY           KEY LENGTH                                   
         SR    R0,R0                                                            
         ICM   R0,1,TABKYD                                                      
         ST    R0,QSPDSP           DISPLACEMENT TO KEY                          
         LA    R1,QSPARM                                                        
         L     RF,AQSORT                                                        
         TM    TABSTA,TABSHI       IS TABLE IN HIGH CORE                        
         JNO   *+10                                                             
         O     RF,BIT31            FLAG FOR QSORT                               
         SAM31 ,                   SWITCH TO 31 BIT                             
         BASR  RE,RF               GO TO QSORT                                  
         SAM24 ,                                                                
         J     XITY                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SET TABLE PARAMETERS FOR SEARCH                                     *         
***********************************************************************         
                                                                                
TSET     NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         LR    R5,R1                                                            
         USING TABD,R5                                                          
         L     R1,TABNUM           # RECORDS                                    
         L     R3,TABLEN           R3=L'RECORD                                  
         MR    R0,R3                                                            
         L     R2,TABADR           R2=A(START OF TABLE)                         
         AR    R1,R2                                                            
         BCTR  R1,0                R1=END OF TABLE                              
         LR    R0,R3               R0=L'RECORD                                  
         SR    R2,R0               R2=A(START LESS LENGTH OF ENTRY)             
         SR    R1,R2               R1=LENGTH OF TABLE PLUS AN ENTRY             
         AR    R0,R0               R0=LOWEST POWER OF 2 GE R1                   
         CR    R0,R1                                                            
         JNH   *-4                                                              
         AR    R1,R2               R1=END OF TABLE                              
         SR    R4,R4                                                            
         ICM   R4,7,TABKYL         R4=L'KEY                                     
         STM   R0,R4,TABSRC        SAVE SEARCH PARAMETERS                       
         J     XITY                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL SEARCH ROUTINE                                              *         
***********************************************************************         
                                                                                
TSRCH    NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         LR    R5,R1                                                            
         USING TABD,R5                                                          
         TM    TABSTA,TABSHI                                                    
         JNO   *+6                                                              
         SAM31 ,                                                                
         L     RF,TABARG                                                        
         XC    TABRTN,TABRTN                                                    
         LM    R0,R4,TABSRC        LOAD SEARCH REGISTERS                        
         BCTR  R4,0                                                             
                                                                                
TSRCH3   SRL   R0,1                1/2 REMAINING TABLE LENGTH                   
         CR    R0,R3               TEST IF LESS THAN AN ENTRY LENGTH            
         JNL   TSRCH4                                                           
         SAM24 ,                                                                
         J     XITN                NOT FOUND - EOT                              
                                                                                
TSRCH4   JXH   R2,R0,TSRCH5        COMPUTE NEW TABLE START ADDRESS              
         EXRL  R4,TSRCCLF2                                                      
         JE    TSRCH7              GOT A MATCH                                  
         JH    TSRCH3                                                           
TSRCH5   SR    R2,R0                                                            
         J     TSRCH3                                                           
                                                                                
TSRCH7   ST    R2,TABRTN                                                        
         SAM24 ,                                                                
         J     XITY                                                             
                                                                                
TSRCCLF2 CLC   0(0,RF),0(R2)                                                    
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* RETURN NEXT ITEM FROM TABLE                                         *         
***********************************************************************         
                                                                                
TNXT     NTR1  LABEL=*                                                          
         LARL  RA,GLOBALS                                                       
         LR    R5,R1                                                            
         USING TABD,R5                                                          
         OC    TABNUM,TABNUM                                                    
         JZ    XITN                TABLE IS EMPTY                               
         TM    TABSTA,TABSHI                                                    
         JNO   *+6                                                              
         SAM31 ,                                                                
         SR    R2,R2                                                            
         L     R3,TABNUM           NUMBER                                       
         M     R2,TABLEN           X LENGTH                                     
         A     R3,TABADR                                                        
         BCTR  R3,0                R3=END OF TABLE                              
                                                                                
         L     R0,TABREC           RETURN IN WORK AREA                          
         L     R1,TABLEN           LENGTH                                       
         LR    RF,R1                                                            
         ICM   RE,15,TABLST        SET ADDRESS OF LAST                          
         JZ    *+12                                                             
         A     RE,TABLEN           BUMP TO NEXT                                 
         J     *+8                                                              
         ICM   RE,15,TABADR        FIRST TIME PASS FIRST                        
         CR    RE,R3               TEST PAST END                                
         JH    TNXTN                                                            
         STCM  RE,15,TABLST                                                     
         MVCL  R0,RE               MOVE THE RECORD                              
         SAM24 ,                                                                
         J     XITY                                                             
                                                                                
TNXTN    SAM24 ,                                                                
         J     XITN                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CANADIAN TAX - INITIALIZATION                                       *         
***********************************************************************         
                                                                                
CANTX    NTR1  LABEL=*                                                          
         LA    R4,VATBUFF          SET VATICAN BUFFER                           
         USING VTCD,R4                                                          
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LARL  RF,CANTXACT                                                      
         AR    RF,R1                                                            
         BR    RF                                                               
                                                                                
CANTXACT J     CANINI              INITIALIZE                                   
         J     CANRTE              GET DEFAULT RATES                            
         J     CANTRN              GET GST/PST POST TO BUFFER                   
         J     CANTOT              RETURN TOTAL PST/GST                         
                                                                                
CANINI   CLI   MODE,PROCACC        TEST ACCOUNT                                 
         JE    CANINI2             YES, DON'T CLEAR GROUP                       
         L     RF,AGSTGRP          CLEAR GROUP BUFFERS                          
         XC    0(CTDLLNQ,RF),0(RF)                                              
         L     RF,APSTGRP                                                       
         XC    0(CTDLLNQ,RF),0(RF)                                              
         CLI   MODE,LEVBFRST       TEST FIRST FOR PRODUCT                       
         JE    XIT                 YES, ALL DONE                                
                                                                                
CANINI2  L     RF,AGSTBUF                                                       
         XC    0(CTDLLNQ,RF),0(RF)                                              
         L     RF,APSTBUF                                                       
         XC    0(CTDLLNQ,RF),0(RF)                                              
                                                                                
         L     RF,AGSTRATE         CLEAR RATE BUFFERS                           
         MVI   0(RF),0                                                          
         L     RF,APSTRATE                                                      
         MVI   0(RF),0                                                          
                                                                                
         L     R7,ADGOBLOC                                                      
         USING GOBLOCK,R7                                                       
                                                                                
CANINI3  XC    VTCACTN(VTCLNQ),VTCACTN                                          
         MVI   VTCACTN,VTCALOOK    LOOK FOR RATES                               
         MVC   VTCCOMF,ADCOMFAC    A(COMFACS)                                   
         MVC   VTCCPY,CPY          COMPANY CODE                                 
         MVC   VTCOFFC,OFC         OFFICE                                       
         MVC   VTCINVD,BILDT1      INVOICE/UNBILL DATE                          
         MVC   VTCTYPE,GOTAXCOD    GST CODE                                     
                                                                                
         CLI   VTCTYPE,C' '                                                     
         JH    *+8                                                              
         MVI   VTCTYPE,C'S'                                                     
                                                                                
         GOTOR VATICAN,VTCD                                                     
         TM    VTCINDS,VTCINA      IS TAX APPLICABLE?                           
         JZ    *+8                 YES                                          
         NI    CTAXOPT,ALL-(CTAXOGST)  NO, TURN OFF OPTION                      
         J     XITY                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* GET CANADIAN TAX RATE                                               *         
***********************************************************************         
                                                                                
CANRTE   L     R7,ADGOBLOC         LOOK UP DEFAULT RATES                        
         USING GOBLOCK,R7                                                       
         MVC   VTCINVD,BILDT1      BILL DATE                                    
         MVC   VTCOFFC,OFC         OFFICE                                       
         MVC   VTCTYPE,GOTAXCOD    GST TYPE                                     
         MVC   PSTTYP,GOPSTCOD     PST TYPE                                     
         MVC   PROV,GOPSTPRV       PROVINCE                                     
         TM    CTAXOPT,CTAXOBIL    TEST ITEM ALREADY BILLED                     
         JNO   CANRTE1             NO,                                          
         NI    CTAXOPT,ALL-(CTAXOBIL)                                           
         ICM   R3,15,JXAPTAEL      GET LAST BILLED ELEMENT                      
         JZ    CANRTE1                                                          
         USING PTAEL,R3                                                         
         GOTOR DATCON,DMCB,(2,PTARBLDT),(1,VTCINVD)                             
         MVC   VTCTYPE,PTARGSTC                                                 
         MVC   PSTTYP,PTARPSTC                                                  
         MVC   PROV,PTARPRVC                                                    
                                                                                
CANRTE1  GOTOR CANRATE             GET DEFAULT RATES ETC.                       
         J     XITY                                                             
         DROP  R3,R7                                                            
                                                                                
CANRATE  NTR1  LABEL=*                                                          
         XC    VTCPRV,VTCPRV                                                    
         XC    CTAXGST,CTAXGST     CLEAR RETURN DATA                            
         XC    CTAXPST,CTAXPST                                                  
         TM    CTAXOPT,CTAXOGST    TEST USING GST                               
         JNO   CANRATE3                                                         
         GOTOR CANLOK              GET A(CURRENT RATE ENTRY)                    
         JNE   CANRATE3                                                         
         MVC   CTAXGST,0(R5)                                                    
                                                                                
CANRATE3 CLI   PSTTYP,0             ANY PST TYPE?                               
         JE    XITY                                                             
         MVC   VTCTYPE,PSTTYP       TYPE                                        
         OC    PROV,PROV            ANY PROVINCE?                               
         JZ    XITY                                                             
         MVC   VTCPRV,PROV                                                      
         GOTOR CANLOK              GET A(CURRENT RATE ENTRY)                    
         JNE   *+10                                                             
         MVC   CTAXPST,0(R5)                                                    
         J     XITY                                                             
         EJECT                                                                  
***********************************************************************         
* LOOK UP RATE IN BUFFER - IF NOT FOUND CALL VATICAN                  *         
***********************************************************************         
                                                                                
CANLOK   L     R5,AGSTRATE          TEST GST OR PST                             
         USING CTXD,R5                                                          
         OC    VTCPRV,VTCPRV                                                    
         JZ    *+8                                                              
         L     R5,APSTRATE                                                      
         LA    R0,MXRATE                                                        
CANLOK3  CLI   0(R5),0             TEST END OF TABLE                            
         JE    CANLOK7                                                          
         CLC   VTCINVD,CTXDATE     MATCH DATE                                   
         JNE   CANLOK5                                                          
         CLC   VTCTYPE,CTXTYPE     MATCH TYPE                                   
         JNE   CANLOK5                                                          
         OC    VTCPRV,VTCPRV       ANY PROVINCE CODE(IE. GST)?                  
         BZR   RE                  IT'S A MATCH FOR PST                         
         CLC   VTCPRV,CTXPROV                                                   
         BER   RE                  IT'S A MATCH FOR GST                         
CANLOK5  LA    R5,CTXLNQ(R5)                                                    
         JCT   R0,CANLOK3                                                       
         DC    H'0'                TABLE IS FULL                                
                                                                                
CANLOK7  XC    VTCERR,VTCERR                                                    
         XC    VTCINDS,VTCINDS                                                  
         MVI   VTCACTN,VTCALOOK    LOOK UP RATES                                
         LR    R0,RE                                                            
         GOTOR VATICAN,VTCD                                                     
         LR    RE,R0                                                            
         TM    VTCINDS,VTCINA                                                   
         BOR   RE                                                               
         OC    VTCERR,VTCERR                                                    
         BNZR  RE                                                               
         XC    0(CTXLNQ+1,R5),0(R5)                                             
         MVC   CTXDATE,VTCINVD     ADD NEW ITEM TO TABLE                        
         MVC   CTXTYPE,VTCTYPE                                                  
         MVC   CTXPROV,VTCPRV                                                   
         MVC   CTXRATE,VTCRATE                                                  
         MVC   CTXACCT,VTCACT                                                   
         MVC   CTXEFFD,VTCEFFD                                                  
         MVC   CTXREG,VTCREG                                                    
         OC    CTXREG,SPACES                                                    
         OC    VTCPRV,VTCPRV       TEST PROVINCE CODE                           
         BZR   RE                                                               
                                                                                
         MVI   VTCACTN,VTCANAME                                                 
         GOTOR VATICAN,VTCD                                                     
         MVC   CTXPRVD,VTCPRVD     PROVINCE DESCRIPTION                         
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET GST/PST AMOUNTS FOR CURRENT TRANSACTION - POST TO BUFFER        *         
***********************************************************************         
                                                                                
CANTRN   TM    WRKCSTA,WRKCSMT     TEST MEDIA TRANSFER                          
         JO    XITY                DON'T TO GST/PST TABLES                      
         ICM   R3,15,CBTRN         R4=A(CURRENT TRANSACTION DATA)               
         JZ    CANPRV              99 POSTING                                   
         USING BTRND,R3                                                         
         CLC   BTRNWC,WCBILLQ                                                   
         JE    CANPRV                                                           
         LA    R4,BTRNBK           R4=A(ACCUMS FOR THIS TRANSACTION)            
         USING JOBD,R4                                                          
         LA    R5,BGSTBK           R5=A(GST BUCKETS)                            
         USING CTAXD,R5                                                         
         LA    R6,BPSTBK           R6=A(PST BUCKETS)                            
                                                                                
PST      USING CTAXD,R6                                                         
         OC    BTRNUSD,BTRNUSD     TEST ALREADY BILLED                          
         JZ    CANTRN1             NO,                                          
         OI    CTAXOPT,CTAXOBIL    SET OPTION TO USE ORIGINAL INFO              
                                                                                
CANTRN1  GOTOR CANTX,CANRTEQ       GET DEFAULT RATES                            
         LA    R7,CTAXGST          R7=A(GST TAX DATA)                           
         USING CTXD,R7                                                          
         MVC   BGSTTYPE,CTXTYPE    GST TYPE                                     
         LLH   RF,CTXRATE                                                       
         CVD   RF,DUB              SAVE GST TAX RATE                            
         ZAP   CTAXGRS,JOBNET      TAX GROSS=NET                                
         SP    CTAXGRS,JOBCD       LESS: CD                                     
         ZAP   CTAXCOM,PZERO                                                    
         TM    JXSTAT3,JXS3PNET    TEST PAY=NET                                 
         JO    *+16                                                             
         AP    CTAXGRS,JOBCOM       + COMMISSION                                
         ZAP   CTAXCOM,JOBCOM      COMMISSION BILLED                            
                                                                                
         ZAP   PL16,CTAXGRS                                                     
         MP    PL16,DUB            * RATE                                       
         SRP   PL16,64-4,5         ROUNDED                                      
         ZAP   CTAXTAX,PL16        SAVE GST                                     
         AP    CTAXGRS,CTAXTAX     ADD TO TAX GROSS                             
         ZAP   CTAXPB,PZERO                                                     
                                                                                
         XC    CTDWRK,CTDWRK       BUILD TAX RECORD                             
         LA    RF,CTDWRK                                                        
         USING CTDLD,RF                                                         
         MVC   CTDLTXI,CTAXGST     GST ACCOUNT INFO                             
         MVC   CTDLTXB,CTAXD       TAX BUCKETS                                  
         GOTOR CANBUFR             POST GST TO BUFFER                           
                                                                                
CANTRN3  TM    CTAXOPT,CTAXOPST    NEED PST?                                    
         JNO   XITY                                                             
         LA    R7,CTAXPST                                                       
         USING CTXD,R7                                                          
         MVC   BPSTTYPE,CTXTYPE    PST TYPE                                     
         MVC   BPSTPROV,CTXPROV    PST PROV                                     
         LLH   RF,CTXRATE                                                       
         CVD   RF,DUB                 SAVE PST TAX RATE                         
         ZAP   PST.CTAXCOM,CTAXCOM    COMMISSION BILLED                         
         ZAP   PST.CTAXGRS,CTAXGRS     GROSS + GST                              
         ZAP   PL16,PST.CTAXGRS                                                 
         MP    PL16,DUB                 * RATE                                  
         SRP   PL16,64-4,5              ROUNDED                                 
         ZAP   PST.CTAXTAX,PL16         SAVE PST                                
         AP    PST.CTAXGRS,PST.CTAXTAX  ADD TO TAX GROSS                        
         ZAP   PST.CTAXPB,PZERO                                                 
                                                                                
         XC    CTDWRK,CTDWRK                                                    
         LA    RF,CTDWRK                                                        
         USING CTDLD,RF                                                         
         CLI   CTAXPST,C' '                                                     
         JNH   XITY                                                             
         MVC   CTDLTXI,CTAXPST        PST ACCOUNT INFO                          
         MVC   CTDLTXB,PST.CTAXD      TAX BUCKETS                               
         GOTOR CANBUFR                POST PST TO BUFFER                        
         J     XITY                                                             
         DROP  R3,R4,R5,PST,R7,RF                                               
         EJECT                                                                  
***********************************************************************         
* POST GST/PST DATA FROM PREVIOUS BILL TO BUFFER                      *         
***********************************************************************         
                                                                                
CANPRV   TM    PGMSTA,PGMUNB        TEST UNBILLING                              
         JO    *+12                                                             
         TM    BILTTYP,PROG+SPCL    TEST PROGRESSIVE OR SPECIAL                 
         JNZ   XITY                 YES, DON'T ADD TO SUMMARY                   
         TM    CTAXOPT,CTAXOGST     TEST NEED GST ON THIS BILL                  
         JNO   CANPRV3              NO, IGNORE OLD                              
         L     R2,ADTRANS                                                       
         USING TRNELD,R2                                                        
CANPRV1  MVI   ELCODE,VBIELQ                                                    
         GOTOR NEXTEL                                                           
         JNE   CANPRV3                                                          
         USING VBIELD,R2                                                        
         XC    CTDWRK,CTDWRK                                                    
         LA    R3,CTDWRK                                                        
         USING CTDLD,R3                                                         
         LA    R4,CTDLTXI                                                       
         USING CTXD,R4                                                          
         MVC   CTXTYPE,VBITYPE     POST GST DETAILS FOR PREVIOUS                
         MVC   CTXRATE,VBIRATE                                                  
         MVC   CTXDATE,VBIDATE                                                  
         MVC   CTXEFFD,VBIDATE                                                  
         MVC   CTXACCT,VBIACCT                                                  
         MVC   CTXREG,SPACES                                                    
                                                                                
         LA    R7,VATBUFF          LOOK FOR OLD REGISTRATION NUMBER             
         USING VTCD,R7                                                          
         MVC   BYTE,VTCTYPE        SAVE DEFAULT TYPE                            
         MVI   VTCACTN,VTCALOOK                                                 
         XC    VTCPRV,VTCPRV                                                    
         MVC   VTCINVD,VBIDATE                                                  
         MVC   VTCTYPE,VBITYPE     GST CODE                                     
         GOTOR VATICAN,VTCD                                                     
         MVC   CTXREG,VTCREG       SAVE OLD REGISTRATION CODE                   
         OC    CTXREG,SPACES                                                    
         MVC   VTCINVD,BILDT1      RESTORE ORIGINAL DATE                        
         MVC   VTCTYPE,BYTE        AND TYPE                                     
         DROP  R7                                                               
                                                                                
         LA    R4,CTDLTXB                                                       
         USING CTAXD,R4                                                         
         ZAP   CTAXTAX,VBIVAT                                                   
         XI    CTAXTAX+L'CTAXTAX-1,X'01'                                        
         ZAP   CTAXGRS,VBIGROSS                                                 
         XI    CTAXGRS+L'CTAXGRS-1,X'01'                                        
         ZAP   CTAXCOM,VBICOMM                                                  
         XI    CTAXCOM+L'CTAXCOM-1,X'01'                                        
         ZAP   CTAXPB,VBIVAT                                                    
         TM    JXSTAT3,JXS3PNET    TEST PAY=NET                                 
         JZ    *+10                                                             
         ZAP   CTAXCOM,PZERO                                                    
         GOTOR CANBUFR                                                          
                                                                                
         ICM   RF,15,CBTRN     ADD PREV. GST TO TRANSACTION BUFFER              
         JZ    CANPRV1                                                          
         USING BTRND,RF                                                         
         LA    RF,BGSTBK                                                        
PRV      USING CTAXD,RF                                                         
         AP    PRV.CTAXTAX,VBIVAT                                               
         J     CANPRV1                                                          
                                                                                
CANPRV3  TM    CTAXOPT,CTAXOPST     TEST NEED PST ON THIS BILL                  
         JNO   XITY                 NO, IGNORE OLD                              
         L     R2,ADTRANS                                                       
CANPRV4  MVI   ELCODE,PBIELQ                                                    
         GOTOR NEXTEL                                                           
         JNE   XITY                                                             
         USING PBIELD,R2                                                        
         XC    CTDWRK,CTDWRK                                                    
         LA    R3,CTDWRK                                                        
         USING CTDLD,R3                                                         
         LA    R4,CTDLTXI                                                       
         USING CTXD,R4                                                          
         MVC   CTXTYPE,PBITYPE     POST PST DETAILS FOR PREVIOUS                
         MVC   CTXRATE,PBIRATE                                                  
         MVC   CTXDATE,PBIDATE                                                  
         MVC   CTXEFFD,PBIDATE                                                  
         MVC   CTXACCT,PBIACCT                                                  
         MVC   CTXPROV,PBIPRV                                                   
         MVC   CTXREG,SPACES                                                    
                                                                                
         LA    R7,VATBUFF          SET VATICAN BUFFER                           
         USING VTCD,R7                                                          
         MVI   VTCACTN,VTCANAME                                                 
         MVC   VTCPRV,CTXPROV                                                   
         GOTOR VATICAN,VTCD                                                     
         MVC   CTXPRVD,VTCPRVD     PROVINCE DESCRIPTION                         
         MVC   BYTE,VTCTYPE        SAVE DEFAULT TYPE                            
         MVI   VTCACTN,VTCALOOK    GET OLD REGISTRATION NUMBER                  
         MVC   VTCINVD,PBIDATE                                                  
         MVC   VTCTYPE,PBITYPE                                                  
         GOTOR VATICAN,VTCD                                                     
         MVC   CTXREG,VTCREG       SAVE OLD REGISTRATION CODE                   
         OC    CTXREG,SPACES                                                    
         MVC   VTCINVD,BILDT1      RESTORE ORIGINAL DATE                        
         MVC   VTCTYPE,BYTE        AND TYPE                                     
         DROP  R7                                                               
                                                                                
         LA    R4,CTDLTXB                                                       
PST      USING CTAXD,R4                                                         
         ZAP   PST.CTAXTAX,PBIPST                                               
         XI    PST.CTAXTAX+L'CTAXTAX-1,X'01'                                    
         ZAP   PST.CTAXGRS,PBIGROSS                                             
         XI    PST.CTAXGRS+L'CTAXGRS-1,X'01'                                    
         ZAP   PST.CTAXCOM,PBICOMM                                              
         XI    PST.CTAXCOM+L'CTAXCOM-1,X'01'                                    
         ZAP   PST.CTAXPB,PBIPST                                                
                                                                                
         GOTOR CANBUFR                                                          
         ICM   RF,15,CBTRN      ADD PREV. GST TO TRANSACTION BUFFER             
         JZ    CANPRV4                                                          
         USING BTRND,RF                                                         
         LA    RF,BPSTBK                                                        
PRV      USING CTAXD,RF                                                         
         AP    PRV.CTAXTAX,PBIPST                                               
         J     CANPRV4                                                          
         DROP  R2,R3,R4,PST,PRV,RF                                              
         EJECT                                                                  
***********************************************************************         
* POST DETAIL TO TAX DETAIL BUFFER                                    *         
***********************************************************************         
                                                                                
CANBUFR  NTR1  LABEL=*                                                          
         LA    R6,CTDWRK                                                        
         USING CTDLD,R6                                                         
         LA    RF,CTDLTXB                                                       
         USING CTAXD,RF                                                         
         ZAP   CTAXRTX,PZERO       INITIALIZE RETAIL BUCKETS                    
         ZAP   CTAXRTG,PZERO                                                    
         ZAP   CTAXRTC,PZERO                                                    
         DROP  RF                                                               
                                                                                
         LA    R6,CTDLTXI                                                       
WRK      USING CTXD,R6                                                          
         CLI   WRK.CTXTYPE,C' '                                                 
         JNH   XITY                                                             
         NI    CTAXOPT,ALL-(CTAXOGRP) TURN OFF GROUP                            
                                                                                
CANBUFR1 L     R5,AGSTBUF                                                       
         TM    CTAXOPT,CTAXOGRP    TEST USE GROUP BUFFER                        
         JNO   *+8                 NO,                                          
         L     R5,AGSTGRP          YES, ADD TO GROUP TOTALS                     
         MVI   BYTE,CTAXOBHG                                                    
TAB      USING CTXD,R5                                                          
         OC    WRK.CTXPROV,WRK.CTXPROV                                          
         JZ    CANBUFR2                                                         
         L     R5,APSTBUF                                                       
         TM    CTAXOPT,CTAXOGRP    TEST USE GROUP BUFFER                        
         JNO   *+8                 NO,                                          
         L     R5,APSTGRP          YES, ADD TO GROUP TOTALS                     
         MVI   BYTE,CTAXOBHP                                                    
                                                                                
CANBUFR2 OC    CTAXOPT,BYTE                                                     
         XR    R3,R3               R3=# IN BUFFER                               
         LR    R4,R5               R4=A(START OF BUFFER)                        
         LA    R0,MXDTL                                                         
                                                                                
CANBUFR3 CLI   TAB.CTXD,0                                                       
         JE    CANBUFR7                                                         
         AHI   R3,1                COUNT ITEMS IN BUFFER                        
         CLC   WRK.CTXTYPE,TAB.CTXTYPE                                          
         JNE   CANBUFR4                                                         
         CLC   WRK.CTXRATE,TAB.CTXRATE                                          
         JNE   CANBUFR4                                                         
         CLC   WRK.CTXACCT,TAB.CTXACCT                                          
         JNE   CANBUFR4                                                         
         CLC   WRK.CTXPROV,TAB.CTXPROV                                          
         JE    CANBUFR5                                                         
                                                                                
CANBUFR4 LA    R5,CTDLLNQ(R5)                                                   
         JCT   R0,CANBUFR3                                                      
         DC    H'0'                DETAIL TABLE IS FULL                         
                                                                                
CANBUFR5 LA    RF,TAB.CTXD+(CTDLTXB-CTDLD) ADD TO TABLE                         
         LA    RE,WRK.CTXD+(CTDLTXB-CTDLD)                                      
         LA    R0,CTAXBN                                                        
         AP    0(BKLQ,RF),0(BKLQ,RE)                                            
         LA    RE,BKLQ(RE)                                                      
         LA    RF,BKLQ(RF)                                                      
         JCT   R0,*-14                                                          
         J     CANBUFR8                                                         
                                                                                
CANBUFR7 MVC   0(CTDLLNQ,R5),CTDWRK   SAVE NEW RECORD                           
         LA    R5,CTDLLNQ(R5)                                                   
         XC    0(CTDLLNQ,R5),0(R5)                                              
         LTR   R3,R3               TEST- ONLY ONE IN BUFFER                     
         JZ    CANBUFR8                                                         
         AHI   R3,1                INCREMENT NUMBER IN BUFFER                   
         LA    RF,CTDLLNQ                                                       
         LA    R0,CTXSORTL                                                      
         GOTOR AQSORT,DMCB,(R4),(R3),(RF),(R0),0                                
                                                                                
CANBUFR8 TM    CTAXOPT,CTAXOGRP    TEST ALREADY DID GROUP                       
         JO    CANBUFR9            YES,                                         
         TM    REQOPT,REQGRP       TEST GROUP BILL IN PROGRESS                  
         JNO   CANBUFR9            NO, DON'T NEED GROUP                         
         OI    CTAXOPT,CTAXOGRP    SET TO ADD GROUP TO BUFFER                   
         J     CANBUFR1                                                         
                                                                                
CANBUFR9 NI    CTAXOPT,ALL-(CTAXOGRP)                                           
         J     XITY                                                             
         DROP  TAB,WRK                                                          
         EJECT                                                                  
***********************************************************************         
* RETURN TOTALS FOR GST & PST                                         *         
***********************************************************************         
                                                                                
CANTOT   LA    R2,CTOTGST$         CLEAR THE TOTAL LINE                         
         LA    R3,CTAXBN                                                        
         ZAP   0(L'CTAXTAX,R2),PZERO                                            
         LA    R2,L'CTAXTAX(R2)                                                 
         JCT   R3,*-10                                                          
                                                                                
         LA    R2,CTOTGST$                                                      
         L     R6,AGSTBUF          GET GST TOTALS                               
         TM    CTAXOPT,CTAXOGRP                                                 
         JNO   *+8                                                              
         L     R6,AGSTGRP          GET GROUP BILL TOTALS                        
         GOTOR CANTOTL                                                          
                                                                                
         LA    R2,CTOTPST$                                                      
         LA    R3,CTAXBN                                                        
         ZAP   0(L'CTAXTAX,R2),PZERO                                            
         LA    R2,L'CTAXTAX(R2)                                                 
         JCT   R3,*-10                                                          
                                                                                
         LA    R2,CTOTPST$                                                      
         L     R6,APSTBUF          GET PST TOTAL LINE                           
         TM    CTAXOPT,CTAXOGRP                                                 
         JNO   *+8                                                              
         L     R6,APSTGRP          GET GROUP BILL TOTALS                        
         GOTOR CANTOTL                                                          
                                                                                
         L     R2,APROTYP          GET PROVINVCE TOTALS BY TYPE                 
         USING CPROD,R2                                                         
         MVI   0(R2),0             ASSUME NONE                                  
         L     R6,APSTBUF                                                       
         USING CTDLD,R6                                                         
                                                                                
CANTOT3  CLI   0(R6),0             TEST EOT PROVINCE TABLE                      
         JE    XITY                YES, ALL DONE                                
         L     R2,APROTYP                                                       
         SR    R0,R0                                                            
         LA    R4,CTDLTXI                                                       
         USING CTXD,R4                                                          
                                                                                
CANTOT5  CLI   CPRODES,0           TEST EOT                                     
         JE    CANTOT9             YES, ADD NEW ENTRY                           
         AHI   R0,1                                                             
         CLC   CPRODES,CTXPRVD     MATCH DESCRIPTION                            
         JNE   CANTOT7                                                          
         LA    R3,CTAXBN                                                        
         LA    RE,CPROTXB                                                       
         LA    RF,CTDLTXB                                                       
CANTOT6  AP    0(L'CTAXTAX,RE),0(L'CTAXTAX,RF)                                  
         LA    RE,L'CTAXTAX(RE)                                                 
         LA    RF,L'CTAXTAX(RF)                                                 
         JCT   R3,CANTOT6                                                       
         J     CANTOT11                                                         
                                                                                
CANTOT7  LA    R2,CPROLNQ(R2)                                                   
         J     CANTOT5                                                          
                                                                                
CANTOT9  MVC   CPRODES,CTXPRVD     ADD TYPE                                     
         MVC   CPROTXB,CTDLTXB     AND BUCKETS                                  
         MVI   CPROD+CPROLNQ,0     CLEAR NEXT ENTRY                             
         AHI   R0,1                                                             
         LA    R1,MXPRV                                                         
         CR    R0,R1                                                            
         JNH   CANTOT11                                                         
         DC    H'0'                TABLE IS FULL                                
                                                                                
CANTOT11 LA    R6,CTDLLNQ(R6)                                                   
         J     CANTOT3                                                          
         DROP  R2,R4,R6                                                         
                                                                                
CANTOTL  LR    R0,R2               SAVE A(OUTPUT BUCKETS)                       
CANTOTL1 CLI   0(R6),0             TEST EOT                                     
         BER   RE                                                               
                                                                                
         CLI   CTXREG-CTXD(R6),X'FF'                                            
         JNE   *+8                                                              
         MVI   CTXREG-CTXD(R6),C' ' REMOVE HIGH SORT FIELD                      
                                                                                
         LA    R4,CTAXGST$-CTAXG(R6)                                            
         LA    R3,CTAXBN                                                        
                                                                                
         AP    0(L'CTAXTAX,R2),0(L'CTAXTAX,R4)                                  
         LA    R2,L'CTAXTAX(R2)                                                 
         LA    R4,L'CTAXTAX(R4)                                                 
         JCT   R3,*-14                                                          
                                                                                
         LR    R2,R0               RESTORE A(OUTPUT BUCKETS)                    
         LA    R6,CTDLLNQ(R6)                                                   
         J     CANTOTL1            GET NEXT                                     
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO AJAX                                                   *         
***********************************************************************         
                                                                                
AJAX     NTR1  LABEL=*                                                          
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JNO   AJAX3               YES, PRINT IO COUNT AT ENTER                 
         GOTOR AIOCNTR,DMCB,AJAXMO                                              
                                                                                
AJAX3    GOTOR ACJAX,JXBLK                                                      
         TM    UPSI,UPDMG          TRACE DM CALLS                               
         JNO   AJAX5               YES, PRINT IO COUNT AT ENTER                 
         GOTOR AIOCNTR,DMCB,(C'X',AJAXMO)                                       
                                                                                
AJAX5    J     XIT                                                              
         EJECT                                                                  
SETNEG   LA    R0,JXBKRN           REVERSE BUCKETS                              
SETNEG2  XI    L'JXBK-1(R3),X'01'  REVERSE SIGN                                 
         LA    R3,L'JXBK(R3)                                                    
         JCT   R0,SETNEG2                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS PATCH CARDS                                                 *         
***********************************************************************         
                                                                                
PATCH    NTR1  LABEL=*             PATCH 00 000000 HHHHHH                       
PATCH1   LA    R6,CARDIO                                                        
         GOTOR CARDS,DMCB,(R6),=C'RE00'                                         
         CLC   0(2,R6),=C'/*'                                                   
         JE    XIT                                                              
         CLC   0(5,R6),=C'PATCH'                                                
         JNE   XIT                                                              
         LARL  R2,PHASES                                                        
                                                                                
PATCH3   CLI   0(R2),EOT                                                        
         JE    PATCH1                                                           
         CLC   4(2,R2),6(R6)       MATCH PHASE                                  
         JE    PATCH5                                                           
         LA    R2,L'PHASES(R2)                                                  
         J     PATCH3                                                           
                                                                                
PATCH5   LA    R6,8(R6)                                                         
         SR    R3,R3                                                            
PATCH7   GOTOR HEXIN,DMCB,1(R6),FULL,6,0  CONVERT LOCATION                      
         OC    12(4,R1),12(R1)                                                  
         JNZ   *+6                                                              
         DC    H'0'                BAD LOCATION                                 
         SR    RF,RF                                                            
         ICM   RF,7,FULL                                                        
         AR    R3,RF                                                            
         LA    R6,7(R6)                                                         
         CLI   0(R6),C'+'                                                       
         JE    PATCH7                                                           
                                                                                
         A     R3,12(R2)           R3=LOCATION TO BE PATCHED                    
         LA    RF,1(R6)            GET LENGTH OF PATCH                          
         SR    R0,R0                                                            
         CLI   0(RF),C' '                                                       
         JE    *+16                                                             
         AHI   R0,1                                                             
         LA    RF,1(RF)                                                         
         J     *-16                                                             
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                BAD PATCH                                    
         GOTOR HEXIN,DMCB,1(R6),(R3),(R0),0                                     
         OC    12(4,R1),12(R1)                                                  
         JNZ   PATCH1                                                           
         DC    H'0'                BAD PATCH                                    
         EJECT                                                                  
***********************************************************************         
* INITIALIZE TABLES                                                   *         
***********************************************************************         
                                                                                
TABINI   LR    R0,RE                                                            
         LARL  R5,TABTAB                                                        
         SAM31 ,                                                                
TABINI3  CLC   BYTE,12(R5)         MATCH TABLE TYPE (24/31)                     
         JNE   TABINI7                                                          
         LR    R2,R1               SAVE A(START OF BUFFER)                      
         LLH   RE,13(R5)           DISPLACEMENT INTO NBILC                      
         LA    RE,NBILC(RE)                                                     
                                                                                
         MVC   0(8,R2),0(R5)       TABLE NAME TO START OF AREA                  
         LA    R2,8(R2)            R2=A(THE DATA AREA)                          
         ST    R2,0(RE)            A(THIS AREA INTO NBILD)                      
         L     R3,8(R5)            LENGTH OF THIS AREA                          
         SHI   R3,8                ADJUST FOR NAME                              
         SR    RF,RF                                                            
         MVCL  R2,RE               INITIALIZE THE AREA                          
         L     R3,8(R5)            R3=LENGTH OF THIS AREA                       
         AR    R1,R3               R1 TO NEXT AREA                              
                                                                                
TABINI7  LA    R5,L'TABTAB(R5)                                                  
         CLI   0(R5),EOT           END OF LIST                                  
         JNE   TABINI3                                                          
         SAM24 ,                                                                
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* MANAGER BUCKETS                                                     *         
***********************************************************************         
                                                                                
CLRBUK   STM   RE,R7,12(RD)        CLEAR BUCKETS                                
         LA    R0,BK$N                                                          
         ZAP   0(L'BK$,R1),PZERO                                                
         LA    R1,L'BK$(R1)                                                     
         JCT   R0,*-10                                                          
         LM    RE,R7,12(RD)                                                     
         BR    RE                                                               
                                                                                
ADDBUK   STM   RE,R7,12(RD)        ADD LEVEL (R2) TO (R3)                       
         LM    R2,R3,0(R1)                                                      
         LA    R0,BK$N                                                          
         AP    0(L'BK$,R3),0(L'BK$,R2)                                          
         LA    R2,L'BK$(R2)                                                     
         LA    R3,L'BK$(R3)                                                     
         JCT   R0,*-14                                                          
         LM    RE,R7,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINE                                                       *         
***********************************************************************         
                                                                                
ERACNF   MVI   ERRNUM,ERRACNF      ACCOUNT NOT FOUND                            
         J     ERJOB                                                            
ERACNB   MVI   ERRNUM,ERRACNB      NO BALANCE ELEMENT                           
         J     ERJOB                                                            
ER2MANY  MVI   ERRNUM,ERR2MANY     TOO MANY ITEMS                               
         J     ERJOB                                                            
ERSTUD   MVI   ERRNUM,ERRSTUD      SET STUDIO ERROR                             
                                                                                
ERJOB    TM    JFLG,JFNONB         TEST ALREADY SET                             
         JO    XIT                                                              
         OI    JFLG,JFNONB         SET JOB TO NOT BILLABLE                      
         J     ERALL                                                            
                                                                                
ERMBGR   MVI   ERRNUM,ERRMBGR      MUST BE A GROUP REQUEST                      
         OI    REQOPT,REQERR                                                    
         J     ERALL                                                            
ERALL    GOTOR AADDREP,NONBIL      ADD TO JOB NON-BILLABLE REPORT               
         J     XIT                                                              
         EJECT                                                                  
                                                                                
GETELN   AH    R2,NEWDISP                                                       
         J     FIRSTEL                                                          
         GETEL R2,DATADISP,ELCODE                                               
                                                                                
IOXIT    GOTOR AIOCNTR,DMCB,(C'X',MODE)                                         
         J     XIT                                                              
                                                                                
XITN     DS    0H                                                               
XITL     LA    RF,0                                                             
         J     XITCC                                                            
XITH     LA    RF,2                                                             
         J     XITCC                                                            
XITY     LA    RF,1                                                             
XITCC    CHI   RF,1                                                             
XIT      XIT1  ,                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
LARE     LA    RE,0                                                             
         ORG   LARE+2                                                           
DEST     DS    XL2                 DESTINATION                                  
                                                                                
LARF     LA    RF,0                                                             
         ORG   LARF+2                                                           
SOURC    DS    XL2                 SOURCE DATA                                  
                                                                                
WRKACTN  DS    X                                                                
WRKPARM  DC    (TABLNQ/4)F'0'                                                   
                                                                                
CARDIO   DS    CL80                                                             
                                                                                
AJAXMO   DC    AL1(AJAXQ)                                                       
                                                                                
         DS    0D                                                               
VATBUFF  DS    XL(VTCLNQ)                                                       
                                                                                
CTDWRK   DS    XL(CTDLLNQ)                                                      
                                                                                
PSTTYP   DS    CL(L'VTCTYPE)                                                    
PROV     DS    CL(L'VTCPRV)                                                     
                                                                                
QSPARM   DS    0F                  QSORT PARMS                                  
QSPTAB   DS    XL4                 A(TABLE)                                     
QSPNUM   DS    XL4                 NUMBER                                       
QSPLEN   DS    XL4                 LENGTH                                       
QSPKEY   DS    XL4                 KEY LENGTH                                   
QSPDSP   DS    XL4                 DISP. TO KEY                                 
                                                                                
*                                  BINSRCH PARAMETERS                           
BINREC   DS    F                   A(RECORD TO BE ADDED)                        
BINTAB   DS    F                   A(TABLE)                                     
BINNUM   DS    F                   NUMBER IN THE TABLE                          
BINRLN   DS    F                   RECORD LENGTH                                
BINDSP   DS    XL1                 DISPLACEMENT TO KEY                          
BINKLN   DS    AL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAX IN TABLE                                 
BINPLNQ  EQU   *-BINREC                                                         
                                                                                
LASTICM  DS    F                                                                
CURAMNT  DS    PL6                                                              
LASTINT  DS    F                                                                
                                                                                
GETOPMO  DC    AL1(GETOPTQ)                                                     
MNTH     DS    CL2                                                              
                                                                                
SAVELC   DS    XL1                                                              
SAVER2   DS    F                                                                
                                                                                
CMTPRMS  DS    XL12                                                             
         ORG   CMTPRMS                                                          
CMTSTA   DS    XL1                 STATUS OF COMMENTS NEEDED                    
CMTREC1  DS    AL3                 A(FIRST RECORD)                              
         ORG   CMTPRMS+L'CMTPRMS                                                
                                                                                
NAMPARM  DC    (TABLNQ/4)F'0'                                                   
                                                                                
ANALUQ   EQU   C'1'                ANALYSIS UNIT                                
INCOLDQ  EQU   C'I'                INCOME LEDGER                                
SUSPLDQ  EQU   C'K'                SUSPENSE LEDGER                              
GBILLQ   EQU   C'1'                GROSS BILLING LEDGER                         
REVNLQ   EQU   C'2'                REVENUE LEDGER                               
ANLPARM  DC    (TABLNQ/4)F'0'                                                   
                                                                                
SORTCARD DC    C'SORT FIELDS=(5,NNN,A),FORMAT=BI,WORK=1 '                       
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(NNNN,,,,) '                              
                                                                                
DMBLK    DS    0CL(DMBLKX-DMBLKA)                                               
DMBLKA   DS    CL4                  CALLER                                      
         DS    CL1                                                              
DMBLKB   DS    CL8                  COMMAND                                     
         DS    CL1                                                              
DMBLKC   DS    CL8                  FILE                                        
         DS    CL1                                                              
DMBLKD   DS    CL(L'DMCOUNTQ)       DMCOUNT=                                    
DMBLKE   DS    CL7                  000                                         
         DS    CL1                                                              
DMBLKF   DS    CL(L'DMSTIOQ)        START IO=                                   
DMBLKG   DS    CL7                  000                                         
DMBLKX   EQU   *                                                                
                                                                                
DMCOUNTQ DC    C'DMCOUNT='                                                      
DMSTIOQ  DC    C'START IO='                                                     
                                                                                
COMMAND  DC    C'        '                                                      
GETREC   DC    C'GETREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
ADDREC   DC    C'ADDREC  '                                                      
                                                                                
TRNWRIT  DC    C' '                                                             
                                                                                
FACADD   DC    CL8'ADD     '                                                    
FACWRK   DC    CL8'FACWRK  '                                                    
                                                                                
ENDREMSG DC    C'END OF RUN'                                                    
                                                                                
SVPARM   DS    CL(L'RCFFPARM)                                                   
                                                                                
FWREC    DS    CL(FWRLNQ)                                                       
                                                                                
SMFRECH  DS    CL(FWRSMFR-FWRECD)                                               
SMFREC   DS    CL(SMFBRLNQ)                                                     
                                                                                
A21BQ    EQU   (PROG+TOTL+ONEL+ESTM+SPCL)                                       
A27BQ    EQU   (ALLO)                                                           
                                                                                
FORMCTAB DS    0XL3              A27 FORMAT TABLE                               
         DC    C'0',AL1(LVA+LVB+LVC,LVR)                                        
         DC    C'1',AL1(LVA+LVB+LVC,LVR)                                        
         DC    C'2',AL1(LVA+LVB+LVC,LVR)                                        
         DC    C'3',AL1(LVA+LVB+LVC,LVR)                                        
         DC    C'4',AL1(LVA+LVB,LVC)                                            
         DC    C'5',AL1(LVA+LVB,LVC)                                            
         DC    C'6',AL1(LVA+LVB+LVC,LVC)                                        
         DC    C'7',AL1(LVA+LVB+LVC,LVR)                                        
         DC    C'8',AL1(LVA+LVB+LVC,LVC)                                        
         DC    AL1(EOT)                                                         
                                                                                
ATRNREC  DC    A(0)                                                             
SVRCDATE DC    CL(L'RCDATE)' '                                                  
                                                                                
SAVRE    DC    F'0'                                                             
SAVAIO   DC    A(0)                                                             
                                                                                
SVREG    DS    0F        SAVE RE THRU R7                                        
SVRE     DS    F                                                                
SVRF     DS    F                                                                
SVR0     DS    F                                                                
SVR1     DS    F                                                                
SVR2     DS    F                                                                
SVR3     DS    F                                                                
SVR4     DS    F                                                                
SVR5     DS    F                                                                
SVR6     DS    F                                                                
SVR7     DS    F                                                                
                                                                                
TEXTWORK DS    XL2048              TEXT BUILDER WORK AREA                       
                                                                                
         DS    0H                                                               
PGMTAB   DC    C'21',AL1(PGMA21,PGMLIV,A21BQ),AL3(0)                            
         DC    A(A21PRF,A21QOP,0,0)                                             
         DC    C'22',AL1(PGMA22,PGMDFT,A21BQ),AL3(0)                            
         DC    A(A21PRF,A21QOP,0,0)                                             
         DC    C'23',AL1(PGMA23,PGMLIV+PGMUNB,A21BQ),AL3(0)                     
         DC    A(A21PRF,A23QOP,0,0)                                             
         DC    C'24',AL1(PGMA24,PGMDFT+PGMUNB,A21BQ),AL3(0)                     
         DC    A(A21PRF,A23QOP,0,0)                                             
                                                                                
         DC    C'27',AL1(PGMA27,PGMLIV,A27BQ),AL3(0)                            
         DC    A(A27PRF,A27QOP,0,0)                                             
         DC    C'28',AL1(PGMA28,PGMDFT,A27BQ),AL3(0)                            
         DC    A(A27PRF,A27QOP,0,0)                                             
         DC    C'29',AL1(PGMA29,PGMLIV+PGMUNB,A27BQ),AL3(0)                     
         DC    A(A27PRF,A29QOP,0,0)                                             
         DC    C'30',AL1(PGMA30,PGMDFT+PGMUNB,A27BQ),AL3(0)                     
         DC    A(A27PRF,A29QOP,0,0)                                             
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* PROGRAM PROFILES  - SEE PRFD                                        *         
***********************************************************************         
                                                                                
A21PRF   DS    0X                                                               
A21P01   DC    AL1(A21P01X-A21P01,PGMALL21)                                     
         DC    AL1(OPT1-OPTS,0,0)                                               
         DC    AL2(ACPPFA01-ACPROFSD)                                           
         DC    AL1((A21P01X-*-1)/2)                                             
         DC    C' ',AL1(OPT1ESTN)  EST. VALUE IS NET                            
         DC    C'N',AL1(OPT1ESTN)                                               
         DC    C'G',AL1(OPT1ESTG)                GROSS                          
         DC    C'S',AL1(OPT1ESTS)                SUPPRESSED                     
         DC    C'Y',AL1(OPT1ESTS)                                               
A21P01X  EQU   *                                                                
                                                                                
A21P02   DC    AL1(A21P02X-A21P02,PGMALL21)                                     
         DC    AL1(OPT1-OPTS,0,0)                                               
         DC    AL2(ACPPFA02-ACPROFSD)                                           
         DC    AL1((A21P02X-*-1)/2)                                             
         DC    C'Y',AL1(OPT1PVIN)  PRINT VENDOR INVOICE NUMBER                  
*        DC    C'L',AL1(OPT1PLIN)  PRINT LONG INVOICE NUMBER                    
A21P02X  EQU   *                                                                
                                                                                
A21P03   DC    AL1(A21P03X-A21P03,PGMALL21)                                     
         DC    AL1(OPT1-OPTS,0,0)                                               
         DC    AL2(ACPPFA03-ACPROFSD)                                           
         DC    AL1((A21P03X-*-1)/2)                                             
         DC    C'Y',AL1(OPT1SVN)   SUPPRESS VENDOR NAME                         
A21P03X  EQU   *                                                                
                                                                                
A21P04   DC    AL1(A21P04X-A21P04,PGMALL21)                                     
         DC    AL1(OPT1-OPTS,0,0)                                               
         DC    AL2(ACPPFA04-ACPROFSD)                                           
         DC    AL1((A21P04X-*-1)/2)                                             
         DC    C'Y',AL1(OPT1SNC)   SUPPRESS NET AND COMMISSION                  
A21P04X  EQU   *                                                                
                                                                                
A21P05   DC    AL1(A21P05X-A21P05,PGMALL21)                                     
         DC    AL1(OPT1-OPTS,0,0)                                               
         DC    AL2(ACPPFA05-ACPROFSD)                                           
         DC    AL1((A21P05X-*-1)/2)                                             
         DC    C'Y',AL1(OPT1MJMC)  MATCH JOB MEDIA/COMMENT                      
A21P05X  EQU   *                                                                
                                                                                
A21P06   DC    AL1(A21P06X-A21P06,PGMALL21)                                     
         DC    AL1(OPT4-OPTS,0,0)                                               
         DC    AL2(ACPPFA06-ACPROFSD)                                           
         DC    AL1((A21P06X-*-1)/2)                                             
         DC    C'Y',AL1(OPT4PTAR)  PRINT TARGET ACCOUNT REGISTER                
A21P06X  EQU   *                                                                
                                                                                
A21P07   DC    AL1(A21P07X-A21P07,PGMA21)                                       
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFA07-ACPROFSD)                                           
         DC    AL1((A21P07X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2XTED)  EXCLUDE TALENT FOR 1 EXTRA DAY               
A21P07X  EQU   *                                                                
                                                                                
A21P07A  DC    AL1(A21P07AX-A21P07A,PGMA22)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFC07-ACPROFSD)                                           
         DC    AL1((A21P07AX-*-1)/2)                                            
         DC    C'Y',AL1(OPT2PRQD)  PRINT REQUEST DETAILS                        
A21P07AX EQU   *                                                                
                                                                                
A21P08   DC    AL1(A21P08X-A21P08,PGMALL21)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFA08-ACPROFSD)                                           
         DC    AL1((A21P08X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2SPB)   SUPPRESS PREVIOUS BILLS                      
A21P08X  EQU   *                                                                
                                                                                
A21P09   DC    AL1(A21P09X-A21P09,PGMALL21)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFA09-ACPROFSD)                                           
         DC    AL1((A21P09X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2CDWC)  CASH DISCOUNT BY WORKCODE                    
A21P09X  EQU   *                                                                
                                                                                
A21P10   DC    AL1(A21P10X-A21P10,PGMALL21)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFA10-ACPROFSD)                                           
         DC    AL1((A21P10X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2PTI)   PRINT TRANSFER INFORMATION                   
A21P10X  EQU   *                                                                
                                                                                
A21P11   DC    AL1(A21P11X-A21P11,PGMALL21)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFC11-ACPROFSD)                                           
         DC    AL1((A21P11X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2PPCR)  PAGE PER CLIENT ON REGISTER                  
A21P11X  EQU   *                                                                
                                                                                
A21P12   DC    AL1(A21P12X-A21P12,PGMALL21)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFC12-ACPROFSD)                                           
         DC    AL1((A21P12X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2DSR)   DOUBLE SPACE REGISTER                        
A21P12X  EQU   *                                                                
                                                                                
A21P13   DC    AL1(A21P13X-A21P13,PGMALL21)                                     
         DC    AL1(OPTB-OPTS,0,0)                                               
         DC    AL2(ACPPFA13-ACPROFSD)                                           
         DC    AL1((A21P13X-*-1)/2)                                             
         DC    C'Y',AL1(OPTBAPP)   BILL APPROVED(UNHELD) ITEMS ONLY             
A21P13X  EQU   *                                                                
                                                                                
A21P14   DC    AL1(A21P14X-A21P14,PGMALL21)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFA14-ACPROFSD)                                           
         DC    AL1((A21P14X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3SDD)   SUPPRESS DUE DATE                            
A21P14X  EQU   *                                                                
                                                                                
A21P15   DC    AL1(A21P15X-A21P15,PGMA21+PGMA23+PGMA24)                         
         DC    AL1(OPTMONTH-OPTS,PRFSINP,0)                                     
         DC    AL2(ACPPFC15-ACPROFSD)                                           
         DC    AL1((A21P15X-*-1)/2)                                             
         DC    C' ',AL1(0)         MONTH FOR INVOICE                            
A21P15X  EQU   *                                                                
                                                                                
A21P15A  DC    AL1(A21P15AX-A21P15A,PGMA22)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFA15-ACPROFSD)                                           
         DC    AL1((A21P15AX-*-1)/2)                                            
         DC    C'Y',AL1(OPT2XTED)  EXCLUDE TALENT FOR 1 EXTRA DAY               
A21P15AX EQU   *                                                                
                                                                                
A21P16   DC    AL1(A21P16X-A21P16,PGMALL21)                                     
         DC    AL1(OPT3-OPTS,PRFSLVBK+PRFS2LVL,0) BACK TO LEDG / COMP           
         DC    AL2(ACPPFO16-ACPROFSD)                                           
         DC    AL1((A21P16X-*-1)/2)                                             
         DC    C' ',AL1(0)         PRINT NAME/ADDRESS IN HEADLINE               
         DC    C'Y',AL1(OPT3PNH+OPT3PAH) BOTH                                   
         DC    C'N',AL1(0)               NEITHER                                
         DC    C'B',AL1(OPT3PNH)         NAME ONLY                              
         DC    C'A',AL1(OPT3PAH)         ADDRESS ONLY                           
A21P16X  EQU   *                                                                
                                                                                
A21P17   DC    AL1(A21P17X-A21P17,PGMALL21)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFA17-ACPROFSD)                                           
         DC    AL1((A21P17X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3STFR)  SUPPRESS TOTAL FOR 1L RETAIL                 
A21P17X  EQU   *                                                                
                                                                                
A21P18   DC    AL1(A21P18X-A21P18,PGMALL21)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFA18-ACPROFSD)                                           
         DC    AL1((A21P18X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3PROB)  PRINT REGISTRATION ON BILL                   
A21P18X  EQU   *                                                                
                                                                                
A21P19   DC    AL1(A21P19X-A21P19,PGMALL21)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFC19-ACPROFSD)                                           
         DC    AL1((A21P19X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3PTAX)  PRINT TAX INSTEAD OF CD ON REG               
A21P19X  EQU   *                                                                
                                                                                
A21P20   DC    AL1(A21P20X-A21P20,PGMALL21)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFC20-ACPROFSD)                                           
         DC    AL1((A21P20X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3PT2L)  PRINT TAX AS 2ND LINE ON REG                 
A21P20X  EQU   *                                                                
                                                                                
A21P21   DC    AL1(A21P21X-A21P21,PGMALL21)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFA21-ACPROFSD)                                           
         DC    AL1((A21P21X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3PWCL)  PRINT WORKCODE LONG NAME                     
A21P21X  EQU   *                                                                
                                                                                
A21P22   DC    AL1(A21P22X-A21P22,PGMALL21)                                     
         DC    AL1(OPT4-OPTS,0,0)                                               
         DC    AL2(ACPPFA22-ACPROFSD)                                           
         DC    AL1((A21P22X-*-1)/2)                                             
         DC    C'Y',AL1(OPT4XTFB)  EXCLUDE TAX FROM BILLING AMT                 
A21P22X  EQU   *                                                                
                                                                                
A21P23   DC    AL1(A21P23X-A21P23,PGMALL21)                                     
         DC    AL1(OPTSHIPD-OPTS,PRFSINP,0)                                     
         DC    AL2(ACPPFA23-ACPROFSD)                                           
         DC    AL1((A21P23X-*-1)/2)                                             
         DC    C' ',AL1(0)         SHIP DATE                                    
A21P23X  EQU   *                                                                
                                                                                
A21P24   DC    AL1(A21P24X-A21P24,PGMA21+PGMA23)                                
         DC    AL1(OPTBNPFX-OPTS,PRFSINP,0)                                     
         DC    AL2(ACPPFA24-ACPROFSD)                                           
         DC    AL1((A21P24X-*-1)/2)                                             
         DC    C' ',AL1(0)         BILL NUMBER PREFIX                           
A21P24X  EQU   *                                                                
                                                                                
A21P24A  DC    AL1(A21P24AX-A21P24A,PGMA22+PGMA24)                              
         DC    AL1(OPT4-OPTS,0,0)                                               
         DC    AL2(ACPPFA24-ACPROFSD)                                           
         DC    AL1((A21P24AX-*-1)/2)                                            
         DC    C'Y',AL1(OPT4PNON)  PRINT NON-BILLABLE DETAILS                   
A21P24AX EQU   *                                                                
                                                                                
A21P25   DC    AL1(A21P25X-A21P25,PGMALL21)                                     
         DC    AL1(OPT8-OPTS,0,0)                                               
         DC    AL2(ACPPFA25-ACPROFSD)                                           
         DC    AL1((A21P25X-*-1)/2)                                             
         DC    C'Y',AL1(OPT8PAWC)  PRINT AMOUNTS WITH COMMAS                    
A21P25X  EQU   *                                                                
                                                                                
A21P26   DC    AL1(A21P26X-A21P26,PGMALL21)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFC26-ACPROFSD)                                           
         DC    AL1((A21P26X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2SXRD)  SUPPRESS EXTRA REQUEST DETAILS               
A21P26X  EQU   *                                                                
                                                                                
A21P27   DC    AL1(A21P27X-A21P27,PGMALL21)                                     
         DC    AL1(OPT6-OPTS,0,0)                                               
         DC    AL2(ACPPFA27-ACPROFSD)                                           
         DC    AL1((A21P27X-*-1)/2)                                             
         DC    C'Y',AL1(OPT6PLIV)  PRINT LONG INVOICE NUMBER                    
A21P27X  EQU   *                                                                
                                                                                
A21P28   DC    AL1(A21P28X-A21P28,PGMALL21)                                     
         DC    AL1(OPT6-OPTS,0,0)                                               
         DC    AL2(ACPPFA28-ACPROFSD)                                           
         DC    AL1((A21P28X-*-1)/2)                                             
         DC    C'Y',AL1(OPT6PINV)  PRINT INVOICE INSTEAD OF BILL                
A21P28X  EQU   *                                                                
                                                                                
A21P29   DC    AL1(A21P29X-A21P29,PGMALL21)                                     
         DC    AL1(OPT7-OPTS,0,0)                                               
         DC    AL2(ACPPFA29-ACPROFSD)                                           
         DC    AL1((A21P29X-*-1)/2)                                             
         DC    C'Y',AL1(OPT7PDOL)  PRINT DOLLAR SIGN ON TOTALS                  
A21P29X  EQU   *                                                                
                                                                                
                                                                                
A21P30   DC    AL1(A21P30X-A21P30,PGMALL21)                                     
         DC    AL1(OPT4-OPTS,0,0)                                               
         DC    AL2(ACPPFC30-ACPROFSD)                                           
         DC    AL1((A21P30X-*-1)/2)                                             
         DC    C'Y',AL1(OPT4SINT)  SUPPRESS INTERNAL INCOME SUMMARY             
A21P30X  EQU   *                                                                
                                                                                
                                                                                
A21P31   DC    AL1(A21P31X-A21P31,PGMALL21)                                     
         DC    AL1(OPT4-OPTS,0,0)                                               
         DC    AL2(ACPPFC31-ACPROFSD)                                           
         DC    AL1((A21P31X-*-1)/2)                                             
         DC    C'Y',AL1(OPT4SDAT)  USER MMMDD/YY FORMAT FOR BILLDATE            
A21P31X  EQU   *                                                                
                                                                                
                                                                                
A21P32   DC    AL1(A21P32X-A21P32,PGMALL21)                                     
         DC    AL1(OPT4-OPTS,0,0)                                               
         DC    AL2(ACPPFC32-ACPROFSD)                                           
         DC    AL1((A21P32X-*-1)/2)                                             
         DC    C'Y',AL1(OPT4SMED)  SUPPRESS MEDIA SUMMARY REPORT                
A21P32X  EQU   *                                                                
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
A27PRF   DS    0X                                                               
A27P01   DC    AL1(A27P01X-A27P01,PGMALL27)                                     
         DC    AL1(OPT1-OPTS,0,0)                                               
         DC    AL2(ACPPFA01-ACPROFSD)                                           
         DC    AL1((A27P01X-*-1)/2)                                             
         DC    C'Y',AL1(OPT1PVIN)  PRINT VENDOR INVOICE NUMBER                  
A27P01X  EQU   *                                                                
                                                                                
A27P02   DC    AL1(A27P02X-A27P02,PGMALL27)                                     
         DC    AL1(OPT5-OPTS,0,0)                                               
         DC    AL2(ACPPFA02-ACPROFSD)                                           
         DC    AL1((A27P02X-*-1)/2)                                             
         DC    C' ',AL1(0)         PRINT VENDOR CODE/DESCRIPTION                
         DC    C'N',AL1(0)                      NEITHER                         
         DC    C'B',AL1(OPT5PVCO+OPT5PVDE)      BOTH                            
         DC    C'C',AL1(OPT5PVCO)               CODE                            
         DC    C'D',AL1(OPT5PVDE)               DESCRIPTION                     
A27P02X  EQU   *                                                                
                                                                                
A27P03   DC    AL1(A27P03X-A27P03,PGMALL27)                                     
         DC    AL1(OPT5-OPTS,0,0)                                               
         DC    AL2(ACPPFA03-ACPROFSD)                                           
         DC    AL1((A27P03X-*-1)/2)                                             
         DC    C' ',AL1(OPT5WCDE)  PRINT WORKCODE/DESCRIPTION                   
         DC    C'N',AL1(0)                   NEITHER                            
         DC    C'B',AL1(OPT5WCCO+OPT5WCDE)   BOTH                               
         DC    C'C',AL1(OPT5WCCO)            CODE                               
         DC    C'D',AL1(OPT5WCDE)            DESCRIPTION                        
A27P03X  EQU   *                                                                
                                                                                
A27P04   DC    AL1(A27P04X-A27P04,PGMALL27)                                     
         DC    AL1(OPT5-OPTS,0,0)                                               
         DC    AL2(ACPPFA04-ACPROFSD)                                           
         DC    AL1((A27P04X-*-1)/2)                                             
         DC    C' ',AL1(OPT5PDUE)  PRINT DUE DATE/RECEIPT                       
         DC    C'N',AL1(0)                   NEITHER                            
         DC    C'D',AL1(OPT5PDUE)            DUE DATE                           
         DC    C'R',AL1(OPT5RCPT)            RECEIPT                            
A27P04X  EQU   *                                                                
                                                                                
A27P05   DC    AL1(A27P05X-A27P05,PGMALL27)                                     
         DC    AL1(OPT5-OPTS,0,0)                                               
         DC    AL2(ACPPFA05-ACPROFSD)                                           
         DC    AL1((A27P05X-*-1)/2)                                             
         DC    C'Y',AL1(OPT5NCGC)  SHOW NET/COM/GROSS COLUMNS                   
A27P05X  EQU   *                                                                
                                                                                
A27P06   DC    AL1(A27P06X-A27P06,PGMALL27)                                     
         DC    AL1(OPT4-OPTS,0,0)                                               
         DC    AL2(ACPPFA06-ACPROFSD)                                           
         DC    AL1((A27P06X-*-1)/2)                                             
         DC    C'Y',AL1(OPT4PTAR)  PRINT TARGET ACCOUNT REGISTER                
A27P06X  EQU   *                                                                
                                                                                
A27P07   DC    AL1(A27P07X-A27P07,PGMALL27)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFA07-ACPROFSD)                                           
         DC    AL1((A27P07X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2SPB)   SUPPRESS PREVIOUS BILLS                      
A27P07X  EQU   *                                                                
                                                                                
A27P08   DC    AL1(A27P08X-A27P08,PGMALL27)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFA08-ACPROFSD)                                           
         DC    AL1((A27P08X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2CDWC)  CASH DISCOUNT BY WORKCODE                    
A27P08X  EQU   *                                                                
                                                                                
A27P09   DC    AL1(A27P09X-A27P09,PGMALL27)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFC09-ACPROFSD)                                           
         DC    AL1((A27P09X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2PPCR)  PAGE PER CLIENT ON REGISTER                  
A27P09X  EQU   *                                                                
                                                                                
A27P10   DC    AL1(A27P10X-A27P10,PGMALL27)                                     
         DC    AL1(OPT5-OPTS,0,0)                                               
         DC    AL2(ACPPFC10-ACPROFSD)                                           
         DC    AL1((A27P10X-*-1)/2)                                             
         DC    C'Y',AL1(OPT5SRBO)  SORT REGISTER BY OFFICE                      
A27P10X  EQU   *                                                                
                                                                                
A27P11   DC    AL1(A27P11X-A27P11,PGMALL27)                                     
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFC11-ACPROFSD)                                           
         DC    AL1((A27P11X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2DSR)   DOUBLE SPACE REGISTER                        
A27P11X  EQU   *                                                                
                                                                                
A27P12   DC    AL1(A27P12X-A27P12,PGMALL27)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFO12-ACPROFSD)                                           
         DC    AL1((A27P12X-*-1)/2)                                             
         DC    C' ',AL1(0)         PRINT NAME/ADDRESS IN HEADLINE               
         DC    C'B',AL1(OPT3PNH+OPT3PAH) BOTH                                   
         DC    C'N',AL1(OPT3PNH)         NAME                                   
         DC    C'A',AL1(OPT3PAH)         ADDRESS                                
A27P12X  EQU   *                                                                
                                                                                
A27P13   DC    AL1(A27P13X-A27P13,PGMALL27)                                     
         DC    AL1(OPT6-OPTS,0,0)                                               
         DC    AL2(ACPPFO13-ACPROFSD)                                           
         DC    AL1((A27P13X-*-1)/2)                                             
         DC    C' ',AL1(OPT6LNAC)  LOCATION OF NAME/ADDRESS                     
         DC    C'C',AL1(OPT6LNAC)        CENTER                                 
         DC    C'L',AL1(OPT6LNAL)        LEFT                                   
         DC    C'R',AL1(OPT6LNAR)        RIGHT                                  
A27P13X  EQU   *                                                                
                                                                                
A27P14   DC    AL1(A27P14X-A27P14,PGMALL27)                                     
         DC    AL1(OPT6-OPTS,0,0)                                               
         DC    AL2(ACPPFA14-ACPROFSD)                                           
         DC    AL1((A27P14X-*-1)/2)                                             
         DC    C' ',AL1(OPT6PNAD)  PRINT NARRATIVE DETAIL                       
         DC    C'Y',AL1(OPT6PNAD)                                               
         DC    C'N',AL1(0)                                                      
A27P14X  EQU   *                                                                
                                                                                
A27P15   DC    AL1(A27P15X-A27P15,PGMALL27)                                     
         DC    AL1(OPT6-OPTS,0,0)                                               
         DC    AL2(ACPPFA15-ACPROFSD)                                           
         DC    AL1((A27P15X-*-1)/2)                                             
         DC    C' ',AL1(OPT6COWC)  PRINT COMM IN W/C OR TOTAL                   
         DC    C'W',AL1(OPT6COWC)                W/C                            
         DC    C'T',AL1(OPT6COTO)                TOTAL                          
A27P15X  EQU   *                                                                
                                                                                
A27P16   DC    AL1(A27P16X-A27P16,PGMALL27)                                     
         DC    AL1(OPT7-OPTS,0,0)                                               
         DC    AL2(ACPPFA16-ACPROFSD)                                           
         DC    AL1((A27P16X-*-1)/2)                                             
         DC    C' ',AL1(OPT7PHAN)  PRINT HANDLING/FEE/COMMISSION                
         DC    C'H',AL1(OPT7PHAN)        HANDLING                               
         DC    C'F',AL1(OPT7PFEE)        FEE                                    
         DC    C'C',AL1(OPT7PCOM)        COMMISSION                             
A27P16X  EQU   *                                                                
                                                                                
A27P17   DC    AL1(A27P17X-A27P17,PGMALL27)                                     
         DC    AL1(OPTFRMT-OPTS,PRFSINP,0)                                      
         DC    AL2(ACPPFA17-ACPROFSD)                                           
         DC    AL1((A27P17X-*-1)/2)                                             
         DC    C' ',AL1(0)         BILL FORMAT                                  
A27P17X  EQU   *                                                                
                                                                                
A27P18   DC    AL1(A27P18X-A27P18,PGMALL27)                                     
         DC    AL1(OPTGLVL-OPTS,PRFSINP,0)                                      
         DC    AL2(ACPPFA18-ACPROFSD)                                           
         DC    AL1((A27P18X-*-1)/2)                                             
         DC    C' ',AL1(0)         GROUP LEVEL                                  
A27P18X  EQU   *                                                                
                                                                                
A27P19   DC    AL1(A27P19X-A27P19,PGMALL27)                                     
         DC    AL1(OPT8-OPTS,0,0)                                               
         DC    AL2(ACPPFA19-ACPROFSD)                                           
         DC    AL1((A27P19X-*-1)/2)                                             
         DC    C' ',AL1(OPT8SSTD)  SUPPRESS STANDARD COMMENTS                   
         DC    C'Y',AL1(OPT8SSTD)                                               
         DC    C'N',AL1(0)                                                      
A27P19X  EQU   *                                                                
                                                                                
A27P20   DC    AL1(A27P20X-A27P20,PGMA28)                                       
         DC    AL1(OPT2-OPTS,0,0)                                               
         DC    AL2(ACPPFC20-ACPROFSD)                                           
         DC    AL1((A27P20X-*-1)/2)                                             
         DC    C'Y',AL1(OPT2PRQD)  PRINT REQUEST DETAILS                        
A27P20X  EQU   *                                                                
                                                                                
A27P21   DC    AL1(A27P21X-A27P21,PGMA28)                                       
         DC    AL1(OPT8-OPTS,0,0)                                               
         DC    AL2(ACPPFA21-ACPROFSD)                                           
         DC    AL1((A27P21X-*-1)/2)                                             
         DC    C'Y',AL1(OPT8PNON)  PRINT NON-BILLABLE INVOICES                  
A27P21X  EQU   *                                                                
                                                                                
A27P22   DC    AL1(A27P22X-A27P22,PGMALL27)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFC22-ACPROFSD)                                           
         DC    AL1((A27P22X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3PROB)  PRINT REGISTRATION NUMBER ON BILL            
A27P22X  EQU   *                                                                
                                                                                
A27P23   DC    AL1(A27P23X-A27P23,PGMALL27)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFC23-ACPROFSD)                                           
         DC    AL1((A27P23X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3PTAX)  PRINT GST INSTEAD OF CD ON REG               
A27P23X  EQU   *                                                                
                                                                                
A27P24   DC    AL1(A27P24X-A27P24,PGMALL27)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFC24-ACPROFSD)                                           
         DC    AL1((A27P24X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3PT2L)  PRINT GST AS 2ND LINE ON REG                 
A27P24X  EQU   *                                                                
                                                                                
A27P25   DC    AL1(A27P25X-A27P25,PGMALL27)                                     
         DC    AL1(OPT4-OPTS,0,0)                                               
         DC    AL2(ACPPFA25-ACPROFSD)                                           
         DC    AL1((A27P25X-*-1)/2)                                             
         DC    C'Y',AL1(OPT4XTFB)  EXCLUDE TAX FROM BILLING AMOUNT              
A27P25X  EQU   *                                                                
                                                                                
A27P26   DC    AL1(A27P26X-A27P26,PGMALL27)                                     
         DC    AL1(OPT8-OPTS,0,0)                                               
         DC    AL2(ACPPFA26-ACPROFSD)                                           
         DC    AL1((A27P26X-*-1)/2)                                             
         DC    C'Y',AL1(OPT8PTMD)  PRINT TIME DETAILS                           
A27P26X  EQU   *                                                                
                                                                                
A27P27   DC    AL1(A27P27X-A27P27,PGMALL27)                                     
         DC    AL1(OPT3-OPTS,0,0)                                               
         DC    AL2(ACPPFA27-ACPROFSD)                                           
         DC    AL1((A27P27X-*-1)/2)                                             
         DC    C'Y',AL1(OPT3PWCL)  PRINT WORKCODE LONG NAME                     
A27P27X  EQU   *                                                                
                                                                                
A27P28   DC    AL1(A27P28X-A27P28,PGMALL27)                                     
         DC    AL1(OPT8-OPTS,0,0)                                               
         DC    AL2(ACPPFA28-ACPROFSD)                                           
         DC    AL1((A27P28X-*-1)/2)                                             
         DC    C'Y',AL1(OPT8PIRC)  PRINT INVOICE RECAP                          
A27P28X  EQU   *                                                                
                                                                                
A27P29   DC    AL1(A27P29X-A27P29,PGMALL27)                                     
         DC    AL1(OPT8-OPTS,0,0)                                               
         DC    AL2(ACPPFA29-ACPROFSD)                                           
         DC    AL1((A27P29X-*-1)/2)                                             
         DC    C'Y',AL1(OPT8OSTF)  OMIT 'STAFF' FROM DESCRIPTION                
A27P29X  EQU   *                                                                
                                                                                
A27P30   DC    AL1(A27P30X-A27P30,PGMALL27)                                     
         DC    AL1(OPT8-OPTS,0,0)                                               
         DC    AL2(ACPPFA30-ACPROFSD)                                           
         DC    AL1((A27P30X-*-1)/2)                                             
         DC    C'Y',AL1(OPT8PTBW)  PRINT TIME BY WORKCODE                       
A27P30X  EQU   *                                                                
                                                                                
A27P31   DC    AL1(A27P31X-A27P31,PGMALL27)                                     
         DC    AL1(OPT8-OPTS,0,0)                                               
         DC    AL2(ACPPFA31-ACPROFSD)                                           
         DC    AL1((A27P31X-*-1)/2)                                             
         DC    C'Y',AL1(OPT8PTBP)  PRINT TIME BY PERSON                         
A27P31X  EQU   *                                                                
                                                                                
A27P32   DC    AL1(A27P32X-A27P32,PGMALL27)                                     
         DC    AL1(OPT8-OPTS,0,0)                                               
         DC    AL2(ACPPFA32-ACPROFSD)                                           
         DC    AL1((A27P32X-*-1)/2)                                             
         DC    C'Y',AL1(OPT8PAWC)  PRINT AMMOUNTS WITH COMMAS                   
A27P32X  EQU   *                                                                
                                                                                
A27P33   DC    AL1(A27P33X-A27P33,PGMALL27)                                     
         DC    AL1(OPT9-OPTS,0,0)                                               
         DC    AL2(ACPPFA33-ACPROFSD)                                           
         DC    AL1((A27P33X-*-1)/2)                                             
         DC    C'Y',AL1(OPT9UWDP)  USE W/C DESCRIPTION ON PREBILL               
A27P33X  EQU   *                                                                
                                                                                
A27P34   DC    AL1(A27P34X-A27P34,PGMALL27)                                     
         DC    AL1(OPT9-OPTS,0,0)                                               
         DC    AL2(ACPPFA34-ACPROFSD)                                           
         DC    AL1((A27P34X-*-1)/2)                                             
         DC    C'Y',AL1(OPT9IT50)  INCLUDE TALENT (TYPE 50)                     
A27P34X  EQU   *                                                                
                                                                                
A27P35   DC    AL1(A27P35X-A27P35,PGMALL27)                                     
         DC    AL1(OPT9-OPTS,0,0)                                               
         DC    AL2(ACPPFA35-ACPROFSD)                                           
         DC    AL1((A27P35X-*-1)/2)                                             
         DC    C'Y',AL1(OPT9SPSD)  SUPPRESS PROF'L SERVICE DATE                 
A27P35X  EQU   *                                                                
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* REQUEST OPTIONS  - SEE PRFD                                         *         
***********************************************************************         
                                                                                
A21QOP   DS    0X                                                               
A21Q01   DC    AL1(A21Q01X-A21Q01,PGMA21+PGMA22)                                
         DC    AL1(OPT1-OPTS,PRFSQOPT)                                          
         DC    AL1(ALL-(OPT1ESTN+OPT1ESTG+OPT1ESTS))                            
         DC    AL2(QOPT1-ACWORKD)                                               
         DC    AL1((A21Q01X-*-1)/2)                                             
         DC    C'S',AL1(OPT1ESTS)  EST. VALUE IS NET - SUPPRESSED               
A21Q01X  EQU   *                                                                
                                                                                
A21Q02   DC    AL1(A21Q02X-A21Q02,PGMA21+PGMA22)                                
         DC    AL1(OPT1-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT2-ACWORKD)                                               
         DC    AL1((A21Q02X-*-1)/2)                                             
         DC    C'Y',AL1(OPT1PVIN)  PRINT VENDOR INVOICE NUMBER                  
A21Q02X  EQU   *                                                                
                                                                                
A21Q03   DC    AL1(A21Q03X-A21Q03,PGMA21+PGMA22)                                
         DC    AL1(OPTA-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT3-ACWORKD)                                               
         DC    AL1((A21Q03X-*-1)/2)                                             
         DC    C'Y',AL1(OPTADTBL)  DEMAND TOTAL BILL                            
         DC    C'R',AL1(OPTARJOB)  RETAIL JOB                                   
A21Q03X  EQU   *                                                                
                                                                                
A21Q04   DC    AL1(A21Q04X-A21Q04,PGMA21+PGMA22)                                
         DC    AL1(OPT1-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT4-ACWORKD)                                               
         DC    AL1((A21Q04X-*-1)/2)                                             
         DC    C'Y',AL1(OPT1SVN)   SUPPRESS VENDOR NAME                         
A21Q04X  EQU   *                                                                
                                                                                
A21Q05   DC    AL1(A21Q05X-A21Q05,PGMA21+PGMA22)                                
         DC    AL1(OPT1-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT5-ACWORKD)                                               
         DC    AL1((A21Q05X-*-1)/2)                                             
         DC    C'Y',AL1(OPT1SNC)   SUPPRESS NET AND COMMISSION                  
A21Q05X  EQU   *                                                                
                                                                                
A21Q06   DC    AL1(A21Q06X-A21Q06,PGMA21+PGMA22)                                
         DC    AL1(OPTB-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT6-ACWORKD)                                               
         DC    AL1((A21Q06X-*-1)/2)                                             
         DC    C'M',AL1(OPTBMED)   MEDIA ONLY                                   
         DC    C'P',AL1(OPTBPRD)   PRODUCTION ONLY                              
A21Q06X  EQU   *                                                                
                                                                                
A21Q07   DC    AL1(A21Q07X-A21Q07,PGMA21+PGMA22)                                
         DC    AL1(OPTGBF-OPTS,PRFSQOPT+PRFSINP,0)                              
         DC    AL2(QOPT7-ACWORKD)                                               
         DC    AL1((A21Q07X-*-1)/2)                                             
         DC    AL2(0)              GROUP BILL FORMAT                            
A21Q07X  EQU   *                                                                
                                                                                
A21Q08   DC    AL1(A21Q08X-A21Q08,PGMA21+PGMA22)                                
         DC    AL1(OPTA-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT8-ACWORKD)                                               
         DC    AL1((A21Q08X-*-1)/2)                                             
         DC    C'Y',AL1(OPTASGST)  SUPPRESS GST/PST                             
         DC    C'P',AL1(OPTASPST)  SUPPRESS PST ONLY                            
A21Q08X  EQU   *                                                                
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
A23QOP   DS    0X                                                               
A23Q01   DC    AL1(A23Q01X-A23Q01,PGMA23+PGMA24)                                
         DC    AL1(OPTA-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT1-ACWORKD)                                               
         DC    AL1((A23Q01X-*-1)/2)                                             
         DC    C'Y',AL1(OPTAORGB)  USE ORIGINAL NUMBER                          
A23Q01X  EQU   *                                                                
                                                                                
                                                                                
A23Q07   DC    AL1(A23Q07X-A23Q07,PGMA23+PGMA24)                                
         DC    AL1(OPTGBF-OPTS,PRFSQOPT+PRFSINP,0)                              
         DC    AL2(QOPT7-ACWORKD)                                               
         DC    AL1((A23Q07X-*-1)/2)                                             
         DC    AL2(0)              GROUP BILL FORMAT                            
A23Q07X  EQU   *                                                                
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
A27QOP   DS    0X                                                               
A27Q01   DC    AL1(A27Q01X-A27Q01,PGMA27+PGMA28)                                
         DC    AL1(OPTB-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT1-ACWORKD)                                               
         DC    AL1((A27Q01X-*-1)/2)                                             
         DC    C'M',AL1(OPTBMED)   MEDIA                                        
         DC    C'P',AL1(OPTBPRD)   PRODUCTION                                   
A27Q01X  EQU   *                                                                
                                                                                
A27Q02   DC    AL1(A27Q02X-A27Q02,PGMA27+PGMA28)                                
         DC    AL1(OPTA-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT8-ACWORKD)                                               
         DC    AL1((A27Q02X-*-1)/2)                                             
         DC    C'Y',AL1(OPTASGST)  SUPPRESS GST/PST                             
         DC    C'P',AL1(OPTASPST)  SUPPRESS PST ONLY                            
A27Q02X  EQU   *                                                                
         DC    AL1(EOT)                                                         
                                                                                
A29QOP   DS    0X                                                               
A29Q01   DC    AL1(A29Q01X-A29Q01,PGMA29+PGMA30)                                
         DC    AL1(OPTA-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT1-ACWORKD)                                               
         DC    AL1((A29Q01X-*-1)/2)                                             
         DC    C'Y',AL1(OPTAORGB)  USE ORIGINAL NUMBER                          
A29Q01X  EQU   *                                                                
                                                                                
A29Q03   DC    AL1(A29Q03X-A29Q03,PGMA29)                                       
         DC    AL1(OPTA-OPTS,PRFSQOPT,0)                                        
         DC    AL2(QOPT3-ACWORKD)                                               
         DC    AL1((A29Q03X-*-1)/2)                                             
         DC    C'Y',AL1(OPTARWAS)  REVERSE WITH ALLOCATED STATUS                
A29Q03X  EQU   *                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DICTIONARY ITEMS                                                    *         
***********************************************************************         
                                                                                
DICINP   DS    0H                                                               
         DCDDL AC#ACC,L'DD@ACC              ACCOUNT CODE                        
         DCDDL AC#ACC#,L'DD@ACC#            ACCOUNT #                           
         DCDDL AC#ACL,L'DD@ACL              ACTUAL                              
         DCDDL AC#AGY,L'DD@AGY              AGENCY                              
         DCDDL AC#RSJNA,L'DD@AGJOB          AGENCY JOB                          
         DCDDL AC#AGJBL,L'DD@AGJBL          AGENCY JOB LINK                     
         DCDDL AC#ALPHA,L'DD@ALPHA          ALPHA                               
         DCDDL AC#AMT,L'DD@AMT              AMOUNT                              
         DCDDL AC#ABTAX,L'DD@ABTAX          AMOUNT BEFORE TAX                   
         DCDDL AC#APREV,L'DD@APREV          AMOUNT PREVIOUSLY BILLED            
         DCDDL AC#BASIS,L'DD@BASIS          BASIS                               
         DCDDL AC#BIL,L'DD@BIL              BILL                                
         DCDDL AC#BILAM,L'DD@BILAM          BILL AMOUNT                         
         DCDDL AC#BILDT,L'DD@BILDT          BILL DATE                           
         DCDDL AC#BILC,L'DD@BLN7            BILL NO                             
         DCDDL AC#BILC,L'DD@BLN11           BILL NUMBER                         
         DCDDL AC#BLDIO,L'DD@BLDIO          BILLED ITEMS ONLY                   
         DCDDL AC#BPER,L'DD@BPER            BILLING PERIOD                      
         DCDDL AC#STBIL,L'DD@STBIL          BILL SUBTOTAL                       
         DCDDL AC#BOTH,L'DD@BOTH            BOTH                                
         DCDDL AC#CASH,L'DD@CASH            CASH                                
         DCDDL AC#CSHDS,L'DD@CSHDS          CASH DISCOUNT                       
         DCDDL AC#CLINT,L'DD@CLINT          CLIENT                              
         DCDDL AC#CLI,L'DD@CLI              CLIENT CODE                         
         DCDDL AC#CMN,L'DD@CMN              COMMISSION                          
         DCDDL AC#CODE,L'DD@CODE            CODE                                
         DCDDL AC#COST,L'DD@COST            COST                                
         DCDDL AC#CSG,L'DD@CSG              COSTING                             
         DCDDL AC#STCST,L'DD@STCST          COST SUBTOTAL                       
         DCDDL AC#CRA,L'DD@CRA              CREDIT ACCOUNT                      
         DCDDL AC#CRAM,L'DD@CRAM            CREDIT AMOUNT                       
         DCDDL AC#DATE,L'DD@DATE            DATE                                
         DCDDL AC#DRA,L'DD@DRA              DEBIT ACCOUNT                       
         DCDDL AC#DESC,L'DD@DESC            DESCRIPTION                         
         DCDDL AC#DISS,L'DD@DISS            DISCOUNT                            
         DCDDL AC#DOCC,L'DD@DOCC            DOCUMENT NUMBER                     
         DCDDL AC#DRAFT,L'DD@DRAFT          DRAFT                               
         DCDDL AC#DUEDT,L'DD@DUEDT          DUE DATE                            
         DCDDL AC#DUER,L'DD@DUER            DUE UPON RECEIPT                    
         DCDDL AC#EST,L'DD@EST              ESTIMATE                            
         DCDDL AC#EXT,L'DD@EXT              EXTERNAL                            
         DCDDL AC#FEE,L'DD@FEE              FEE                                 
         DCDDL AC#FOR,L'DD@FOR              FOR                                 
         DCDDL AC#FRN,L'DD@FRN              FOREIGN                             
         DCDDL AC#GROSS,L'DD@GROSS          GROSS                               
         DCDDL AC#VAT1,L'DD@VAT1            GST                                 
         DCDDL AC#GSTAM,L'DD@GSTAM          GST AMOUNT                          
         DCDDL AC#HAND,L'DD@HAND            HANDLING CHARGE                     
         DCDDL AC#HOURS,L'DD@HOURS          HOURS                               
         DCDDL AC#IFDED,L'DD@IFDED          IF PAID BY DUEDATE, DEDUCT          
         DCDDL AC#INCAC,L'DD@INCAC          INCOME ACCOUNT                      
         DCDDL AC#INCAM,L'DD@INCAM          INCOME AMOUNT                       
         DCDDL AC#INV,L'DD@INV              INVOICE                             
         DCDDL AC#INVC,L'DD@INVC            INVOICE NUMBER                      
         DCDDL AC#INVDE,L'DD@INVDE          INVOICE DETAIL                      
         DCDDL AC#INVSU,L'DD@INVSU          INVOICE SUMMARY                     
         DCDDL AC#INT,L'DD@INT              INTERNAL                            
         DCDDL AC#INVTO,L'DD@INVTO          INVOICE TOTAL                       
         DCDDL AC#INVT2,L'DD@INVT2          INVOICE TOTAL                       
         DCDDL AC#JOB,L'DD@JOB              JOB                                 
         DCDDL AC#JOBC,L'DD@JOBC            JOB CODE                            
         DCDDL AC#JOBN,L'DD@JOBN            JOB NAME                            
         DCDDL AC#JOBNU,L'DD@JOBNU          JOB NUMBER                          
         DCDDL AC#MED,L'DD@MED              MEDIA                               
         DCDDL AC#MMPS,L'DD@MMPS            MONTHLY MINIMUM PROF. SER.          
         DCDDL AC#NAME,L'DD@NAME            NAME                                
         DCDDL AC#NET,L'DD@NET              NET                                 
         DCDDL AC#NETAM,L'DD@NETAM          NET AMOUNTS                         
         DCDDL AC#NETCT,L'DD@NETCT          NET COSTS                           
         DCDDL AC#NIL,L'DD@NIL              NIL                                 
         DCDDL AC#NO,L'DD@NO                NO                                  
         DCDDL AC#NONE,L'DD@NONE            NONE                                
         DCDDL AC#OOP,L'DD@OOP              OUT-OF-POCKET EXPENSES              
         DCDDL AC#OTHRS,L'DD@OTHRS          OTHERS                              
         DCDDL AC#PAGE,L'DD@PAGE            PAGE                                
         DCDDL AC#PRGRP,L'DD@PRGRP          PARAGRAPH                           
         DCDDL AC#PAA,L'DD@PAA              PAY ABOVE AMOUNT                    
         DCDDL AC#PLCD,L'DD@PLCD            PLUS CASH DISCOUNT                  
         DCDDL AC#PPTA,L'DD@PPTA            PLEASE PAY THIS AMOUNT              
         DCDDL AC#PRVBL,L'DD@PRVBL          PREVIOUS BILLING                    
         DCDDL AC#PRVBS,L'DD@PRVBS          PREVIOUS BILLS                      
         DCDDL AC#PRVDE,L'DD@PRVDE          PREVIOUSLY DEDUCTED                 
         DCDDL AC#PRO,L'DD@PRO              PRODUCT                             
         DCDDL AC#PROC,L'DD@PROC            PRODUCT CODE                        
         DCDDL AC#PROFS,L'DD@PROFS          PROFESSIONAL                        
         DCDDL AC#PSSF,L'DD@PSSF            PROFESSIONAL STAFF SERV.FOR         
         DCDDL AC#PRD,L'DD@PRD              PRODUCTION                          
         DCDDL AC#PRDBL,L'DD@PRDBL          PRODUCTION BILL                     
         DCDDL AC#PRBIR,L'DD@PRBIR          PRD'N BILLING INVOICE REG'R         
         DCDDL AC#PST,L'DD@PST              PST                                 
         DCDDL AC#PSTAM,L'DD@PSTAM          PST AMOUNT                          
         DCDDL AC#RATE,L'DD@RATE            RATE                                
         DCDDL AC#RSN,L'DD@RSN              REASON                              
         DCDDL AC#RECAP,L'DD@RECAP          RECAPITULATION                      
         DCDDL AC#RCV,L'DD@RCV              RECEIVABLE                          
         DCDDL AC#REF,L'DD@REF              REFERENCE                           
         DCDDL AC#REQDE,L'DD@REQDE          REQUEST DETAILS                     
         DCDDL AC#RTAG,L'DD@RTAG            REGISTER OF TARGET ACCOUNTS         
         DCDDL AC#SALES,L'DD@SALES          SALES                               
         DCDDL AC#SERVE,L'DD@SERVE          SERVICES                            
         DCDDL AC#SHIP,L'DD@SHIP            SHIP DATE                           
         DCDDL AC#STAFF,L'DD@STAFF          STAFF                               
         DCDDL AC#INPST,L'DD@INPST          SI/SB POSTING                       
         DCDDL AC#STDIO,L'DD@STDIO          STUDIO                              
         DCDDL AC#SMYBL,L'DD@SMYBL          SUMMARY BILL                        
         DCDDL AC#SRCRG,L'DD@SRCRG          SURCHARGE                           
         DCDDL AC#SUBT,L'DD@SUBT            SUB TOTAL                           
         DCDDL AC#TAX,L'DD@TAX              TAX                                 
         DCDDL AC#TAXAN,L'DD@TAXAN          TAX ANALYSIS                        
         DCDDL AC#TIME,L'DD@TIME            TIME                                
         DCDDL AC#STTIM,L'DD@STTIM          TIME SUBTOTAL                       
         DCDDL AC#TO,L'DD@TO                TO                                  
         DCDDL AC#TBLNG,L'DD@TBLNG          TOTAL BILLING                       
         DCDDL AC#TOREV,L'DD@TOREV          TO REVERSE                          
         DCDDL AC#TOTAL,L'DD@TOTAL          TOTAL                               
         DCDDL AC#TOTAU,L'DD@TOTAU          TOTAL USD                           
         DCDDL AC#TAMTD,L'DD@TAMTD          TOTAL AMOUNT DUE                    
         DCDDL AC#TAMTU,L'DD@TAMTU          TOTAL AMOUNT DUE USD                
         DCDDL AC#TCRGS,L'DD@TCRGS          TOTAL CHARGES                       
         DCDDL AC#TFOR,L'DD@TFOR            TOTAL FOR                           
         DCDDL AC#TCLI,L'DD@TCLI            TOTAL FOR CLIENT                    
         DCDDL AC#TJOB,L'DD@TJOB            TOTAL FOR JOB                       
         DCDDL AC#TPRO,L'DD@TPRO            TOTAL FOR PRODUCT                   
         DCDDL AC#TWC,L'DD@TWC              TOTAL FOR WORK CODE                 
         DCDDL AC#TMED,L'DD@TMED            TOTAL FOR MEDIA CODE                
         DCDDL AC#TMEDC,L'DD@TMEDC          TOTAL MEDIA CHARGES                 
         DCDDL AC#TPRVB,L'DD@TPRVB          TOTAL PREVIOUS BILLS                
         DCDDL AC#TPRCG,L'DD@TPRCG          TOTAL PRODUCTION CHARGES            
         DCDDL AC#TPSTS,L'DD@TPSTS          TOTAL POSTINGS                      
         DCDDL AC#TOTCL,L'DD@TOTCL          TOTALS FOR CLIENT                   
         DCDDL AC#TOTPR,L'DD@TOTPR          TOTALS FOR PRODUCT                  
         DCDDL AC#TREP,L'DD@TREP            TOTAL FOR REPORT                    
         DCDDL AC#TOTRN,L'DD@TOTRN          TOTALS FOR RUN                      
         DCDDL AC#TTAX,L'DD@TTAX            TOTAL TAXES                         
         DCDDL AC#XFRFR,L'DD@XFRFR          TRANSFERRED FROM                    
         DCDDL AC#XFRTO,L'DD@XFRTO          TRANSFERRED TO                      
         DCDDL AC#TYPE,L'DD@TYPE            TYPE                                
         DCDDL AC#UNITS,L'DD@UNITS          UNITS                               
         DCDDL AC#USD,L'DD@USD              USD                                 
         DCDDL AC#VNDR,L'DD@VNDR            VENDOR                              
         DCDDL AC#RSVNN,L'DD@VNN            VENDOR NAME                         
         DCDDL AC#UNBIO,L'DD@UNBIO          UNBILLED ITEMS ONLY                 
         DCDDL AC#WC2,L'DD@WC2              W/C                                 
         DCDDL AC#WC,L'DD@WC9               WORK CODE                           
         DCDDL AC#YES,L'DD@YES              YES                                 
         DCDDL AC#YSHRE,L'DD@YSHRE          YOUR SHARE                          
         DC    AL1(0)                                                           
                                                                                
         DS    0H                                                               
CURTAB   DS    0XL4                                                             
       ++INCLUDE ACCURTAB                                                       
CURTABX  DC    AL1(0)                                                           
                                                                                
         DS    0H                                                               
NAMTAB   DS    0XL8                                                             
         DC    S(ADHEIRA,CLIK,ADLVANAM,CLINAM)                                  
         DC    S(ADHEIRB,PRDK,ADLVBNAM,PRDNAM)                                  
         DC    S(ADACC,JOBK,ADACCNAM,JOBNAM)                                    
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                                                               
RELOTAB  DS    0XL6                RELOCATION TABLE 02 PHASE                    
         DC    AL4(DMGR),AL2(ADMGR-NBILC)                                       
         DC    AL4(SORT),AL2(ASORT-NBILC)                                       
         DC    AL4(SETEL),AL2(ASETEL-NBILC)                                     
         DC    AL4(WRKM),AL2(AWRKM-NBILC)                                       
         DC    AL4(XTRT),AL2(AXTRT-NBILC)                                       
         DC    AL4(ANAL),AL2(AANAL-NBILC)                                       
         DC    AL4(GETNAM),AL2(AGETNAM-NBILC)                                   
         DC    AL4(INTRNL),AL2(AINTRNL-NBILC)                                   
         DC    AL4(ICMPY),AL2(AICMPY-NBILC)                                     
         DC    AL4(TBADD),AL2(ATBADD-NBILC)                                     
         DC    AL4(TSET),AL2(ATSET-NBILC)                                       
         DC    AL4(TSRCH),AL2(ATSRCH-NBILC)                                     
         DC    AL4(TNXT),AL2(ATNXT-NBILC)                                       
         DC    AL4(PARCPJ),AL2(APARCPJ-NBILC)                                   
RELOTABN EQU   (*-RELOTAB)/L'RELOTAB                                            
                                                                                
         DS    0H                  PHASE LIST (SEE PHSED)                       
PHASES   DS    0XL(PHSLNQ)                                                      
PHASE02  DC    CL8'ACNB02',AL4(0,0)                                             
PHASE03  DC    CL8'ACNB03',AL4(0,RELOTAB3)                                      
PHASE04  DC    CL8'ACNB04',AL4(0,RELOTAB4)                                      
PHASE05  DC    CL8'ACNB05',AL4(0,RELOTAB5)                                      
         DC    AL1(EOT)                                                         
                                                                                
RELOTAB3 DS    0X                  RELO TABLE FOR 03 PHASE                      
         DC    AL2(ABFMT-NBILC)                                                 
         DC    AL2(ABPRT-NBILC)                                                 
         DC    AL2(AFMTTXT-NBILC)                                               
         DC    AL2(AGENB-NBILC)                                                 
         DC    AL1(EOT)                                                         
                                                                                
RELOTAB4 DS    0X                  RELO TABLE FOR 04 PHASE                      
         DC    AL2(AADDREP-NBILC)                                               
         DC    AL2(ARUNREP-NBILC)                                               
         DC    AL2(ATRCE-NBILC)                                                 
         DC    AL2(AIOCNTR-NBILC)                                               
         DC    AL1(EOT)                                                         
                                                                                
RELOTAB5 DS    0X                  RELO TABLE FOR 05 PHASE                      
         DC    AL2(APOST-NBILC)                                                 
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                                                               
SETTAB   DC    AL4(SETTCL)         CLIENT                                       
         DC    AL4(SETTPR)         PRODUCT                                      
         DC    AL4(SETTJB)         JOB                                          
         DC    AL4(SETTBF)         BILL FORMAT                                  
         DC    AL4(SETTBH)         BILL HEADER                                  
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                  FORCE HALFWORD ALIGNMENT                     
SETTCL   DS    0XL2                CLIENT RECORD                                
         DC    AL1(ELMROLD),AL1(0)                                              
         DC    AL1(NAMELQ),AL1(0),S(ADLVANAM)                                   
         DC    AL1(ADRELQ),AL1(0),S(ADLVAADD)                                   
         DC    AL1(PPRELQ),AL1(0),S(ADLVASUP)                                   
         DC    AL1(RSTELQ),AL1(0),S(ADLVASTA)                                   
         DC    AL1(PMDELQ),AL1(0),S(ACLIPMD)                                    
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                  FORCE HALFWORD ALIGNMENT                     
SETTPR   DS    0XL2                PRODUCT RECORD                               
         DC    AL1(ELMROLD),AL1(0)                                              
         DC    AL1(NAMELQ),AL1(0),S(ADLVBNAM)                                   
         DC    AL1(ADRELQ),AL1(0),S(ADLVBADD)                                   
         DC    AL1(PPRELQ),AL1(0),S(ADLVBSUP)                                   
         DC    AL1(RSTELQ),AL1(0),S(ADLVBSTA)                                   
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                  FORCE HALFWORD ALIGNMENT                     
SETTJB   DS    0XL2                JOB RECORD                                   
         DC    AL1(ELMROLD),AL1(0)                                              
         DC    AL1(NAMELQ),AL1(0),S(ADACCNAM)                                   
         DC    AL1(ADRELQ),AL1(0),S(ADACCADD)                                   
         DC    AL1(PPRELQ),AL1(0),S(ADACCJOB)                                   
         DC    AL1(RSTELQ),AL1(0),S(ADACCSTA)                                   
         DC    AL1(ABLELQ),AL1(0),S(ADACCBAL)                                   
         DC    AL1(OTHELQ),AL1(0),S(AOTHEL)                                     
         DC    AL1(LNKELQ),AL1(0),S(ALNKEL)                                     
         DC    AL1(JFNELQ),AL1(0),S(AJFNEL)                                     
         DC    AL1(BCYELQ),AL1(0),S(ABCYEL)                                     
         DC    AL1(PPRELQ),AL1(0),S(APPREL)                                     
         DC    AL1(JOBELQ),AL1(0),S(AJOBEL)                                     
         DC    AL1(SCIELQ),AL1(SCITT99S),S(ASCI99)                              
         DC    AL1(SCIELQ),AL1(SCITCBAP),S(ASCIBA)                              
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                  FORCE HALFWORD ALIGNMENT                     
SETTBF   DS    0XL2                BILL FORMAT                                  
         DC    AL1(ELMRNEW),AL1(0)                                              
         DC    AL1(BOFELQ),AL1(0),S(ABOFEL)                                     
         DC    AL1(EOT)                                                         
                                                                                
         DS    0H                  FORCE HALFWORD ALIGNMENT                     
SETTBH   DS    0XL2                BILL HEADER                                  
         DC    AL1(ELMRNEW),AL1(0)                                              
         DC    AL1(BLHELQ),AL1(0),S(ABLHEL)                                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* EQUATES FOR BUFFERS                                                 *         
***********************************************************************         
                                                                                
IOABQ    EQU   ((((IOLNQ)+15)/8)*8)    IOAREAS                                  
DICBQ    EQU   ((((DICLNQ)+15)/8)*8)   DICTIONARY                               
BFMRQ    EQU   ((((BFMRLNQ)+15)/8)*8)  BFM RECORD                               
POSTBQ   EQU   ((((4500)+15)/8)*8)     POST BUFFER                              
SRTWBQ   EQU   ((((SRDLNQ)+15)/8)*8)   SORT RECORDS                             
BTRNBQ   EQU   ((((BTRNTLNQ)+15)/8)*8) BILLABLE TRANSACTION BUFFER              
SFBQ     EQU   ((((SFTLNQ)+15)/8)*8)   SECTION FILTER BUFFER                    
SSBQ     EQU   ((((SSTLNQ)+15)/8)*8)   SECTION SORT BUFFER                      
PDBQ     EQU   ((((PDTLNQ)+15)/8)*8)   PARAGRAPH DETAIL                         
WRKTBQ   EQU   ((((WRKTLNQ)+15)/8)*8)  WORKCODE RECORD TABLE                    
ESWCBQ   EQU   ((((ESWCLNQ)+15)/8)*8)  ESTIMATED WORKCODES                      
NAMSBQ   EQU   ((((NAMSLNQ)+15)/8)*8)  NAMES BUFFER                             
BESBBQ   EQU   ((((BESBLNQ)+15)/8)*8)  BILLED % ESTIMATE BUFFER                 
INCTBQ   EQU   ((((INTBLNQ)+15)/8)*8)  INTERNAL INCOME TABLE                    
INCLBQ   EQU   ((((INCBLNQ)+15)/8)*8)  INTERNAL INCOME ELEMENT BUFFER           
ANLSBQ   EQU   ((((ANLSLNQ)+15)/8)*8)  ANALYSIS ACCOUNTS BUFFER                 
CMNTBQ   EQU   ((((20*255)+15)/8)*8)   COMMENTS BUFFER                          
XTRABQ   EQU   ((((20*255)+15)/8)*8)   EXTRA JOB COMMENTS BUFFER                
BFMBQ    EQU   ((((BFMBLNQ)+15)/8)*8)  BUFFER FOR BILL FORMAT RECORDS           
RTLBQ    EQU   ((((RTLBLNQ)+15)/8)*8)  BUFFER FOR RETAILERS                     
ICWCBQ   EQU   ((((ICWCLNQ)+15)/8)*8)  TABLE OF INTERCOMPANY WORKCODES          
USFRBQ   EQU   ((((USRBLNQ)+15)/8)*8)  TABLE OF USER FIELDS                     
GSTRBQ   EQU   ((((GSTRLNQ)+15)/8)*8)  GST RATE TABLE                           
PSTRBQ   EQU   ((((PSTRLNQ)+15)/8)*8)  PST RATE TABLE                           
GSTDBQ   EQU   ((((GSTDLNQ)+15)/8)*8)  GST DETAIL TABLE                         
PSTDBQ   EQU   ((((PSTDLNQ)+15)/8)*8)  PST DETAIL TABLE                         
PROTBQ   EQU   ((((PROTLNQ)+15)/8)*8)  PROVINCE TAX BY TYPE                     
LMNTBQ   EQU   ((((LMNTLNQ)+15)/8)*8)  ELEMENT                                  
GRPABQ   EQU   ((((GRPTLNQ)+15)/8)*8)  GROUP ANALYSIS POINTERS                  
         EJECT                                                                  
***********************************************************************         
* DYNAMIC STORAGE ALLOCATION                                          *         
* STORAGE TABLE DEFINITIONS                                           *         
* 8 BYTE NAME                                                         *         
* 4 BYTE LENGTH                                                       *         
* 1 BYTE STATUS (X'00'=24 BIT, X'80'=31 BIT)                          *         
* 2 BYTE DISPLACEMENT TO A(XX) INTO NBILC                             *         
***********************************************************************         
                                                                                
         DS    0H                                                               
TABTAB   DS    0XL15               BELOW THE LINE STORAGE                       
         DC    CL8'**IO1***',AL4(IOABQ),X'00',AL2(AIO1-NBILC)                   
         DC    CL8'**IO2***',AL4(IOABQ),X'00',AL2(AIO2-NBILC)                   
         DC    CL8'**IO3***',AL4(IOABQ),X'00',AL2(AIO3-NBILC)                   
         DC    CL8'**IO4***',AL4(IOABQ),X'00',AL2(AIO4-NBILC)                   
         DC    CL8'**IO5***',AL4(IOABQ),X'00',AL2(AIO5-NBILC)                   
         DC    CL8'**IO6***',AL4(IOABQ),X'00',AL2(AIO6-NBILC)                   
         DC    CL8'**BLH***',AL4(IOABQ),X'00',AL2(ABLH-NBILC)                   
         DC    CL8'**BFM***',AL4(BFMRQ),X'00',AL2(ABFM-NBILC)                   
         DC    CL8'**DIC***',AL4(DICBQ),X'00',AL2(ADICO-NBILC)                  
         DC    CL8'**POSTB*',AL4(POSTBQ),X'00',AL2(APOSTB-NBILC)                
         DC    CL8'**SRTWK*',AL4(SRTWBQ),X'00',AL2(ASRTWK-NBILC)                
         DC    CL8'**BTRN**',AL4(BTRNBQ),X'00',AL2(ABTRN-NBILC)                 
         DC    CL8'**ESWC**',AL4(ESWCBQ),X'00',AL2(AESWC-NBILC)                 
         DC    CL8'**NAMS**',AL4(NAMSBQ),X'00',AL2(ANAMS-NBILC)                 
         DC    CL8'**BESB**',AL4(BESBBQ),X'00',AL2(ABESEL-NBILC)                
         DC    CL8'**INCT**',AL4(INCTBQ),X'00',AL2(AINTLB-NBILC)                
         DC    CL8'**INCL**',AL4(INCLBQ),X'00',AL2(AINCEL-NBILC)                
         DC    CL8'**ANLS**',AL4(ANLSBQ),X'00',AL2(AANLS-NBILC)                 
         DC    CL8'**CMNT**',AL4(CMNTBQ),X'00',AL2(ACMNT-NBILC)                 
         DC    CL8'**XTRA**',AL4(XTRABQ),X'00',AL2(AXTRA-NBILC)                 
         DC    CL8'**SECF**',AL4(SFBQ),X'00',AL2(ASFTAB-NBILC)                  
         DC    CL8'**SECS**',AL4(SSBQ),X'00',AL2(ASSTAB-NBILC)                  
         DC    CL8'**PDL***',AL4(PDBQ),X'00',AL2(APDTAB-NBILC)                  
         DC    CL8'**RTL***',AL4(RTLBQ),X'00',AL2(ARTLRB-NBILC)                 
         DC    CL8'**ICWC**',AL4(ICWCBQ),X'00',AL2(AICMPWC-NBILC)               
         DC    CL8'**USRF**',AL4(USFRBQ),X'00',AL2(AUSFLD-NBILC)                
         DC    CL8'**GSTR**',AL4(GSTRBQ),X'00',AL2(AGSTRATE-NBILC)              
         DC    CL8'**PSTR**',AL4(PSTRBQ),X'00',AL2(APSTRATE-NBILC)              
         DC    CL8'**GSTD**',AL4(GSTDBQ),X'00',AL2(AGSTBUF-NBILC)               
         DC    CL8'**PSTD**',AL4(PSTDBQ),X'00',AL2(APSTBUF-NBILC)               
         DC    CL8'**GSTG**',AL4(GSTDBQ),X'00',AL2(AGSTGRP-NBILC)               
         DC    CL8'**PSTG**',AL4(PSTDBQ),X'00',AL2(APSTGRP-NBILC)               
         DC    CL8'**PROT**',AL4(PROTBQ),X'00',AL2(APROTYP-NBILC)               
         DC    CL8'**ELMT**',AL4(LMNTBQ),X'00',AL2(AELMNT-NBILC)                
         DC    CL8'**GRPA**',AL4(GRPABQ),X'00',AL2(AGRPANL-NBILC)               
TABHI    DS    0X                  ABOVE THE LINE STORAGE                       
         DC    CL8'**WRKT**',AL4(WRKTBQ),X'80',AL2(AWRKTAB-NBILC)               
         DC    CL8'**BFMB**',AL4(BFMBQ),X'80',AL2(ABFMB-NBILC)                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'**BILC**'                                                    
NBILC    DS    0H                                                               
       ++INCLUDE ACREPNBC                                                       
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'**JXBL**'                                                    
JXBLKC   DS    0H                                                               
       ++INCLUDE ACJAXD                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER PHASE TABLE                                          *         
***********************************************************************         
                                                                                
PHSED    DSECT ,                                                                
PHSNME   DS    CL8                 NAME                                         
PHSADR   DS    XL4                 LOAD ADDRESS                                 
PHSROU   DS    XL4                 ADDRESS OF ROUTINE LIST                      
PHSLNQ   EQU   *-PHSED                                                          
                                                                                
***********************************************************************         
* DSECT TO COVER ELEMENT TABLE                                        *         
***********************************************************************         
                                                                                
ELMTD    DSECT ,                                                                
ELMRST   DS    XL1                 RECORD STATUS                                
ELMROLD  EQU   X'80'               RECORD IS OLD STYLE                          
ELMRNEW  EQU   X'40'               RECORD IS NEW STYLE                          
         DS    XL1                 N/D                                          
ELMDATA  DS    0XL4                                                             
ELMCODE  DS    XL1                 ELEMENT CODE                                 
ELMSCIT  DS    XL1                 SCIEL TYPE                                   
ELMBDSP  DS    S                   BASE AND DISPLACEMENT                        
         EJECT                                                                  
       ++INCLUDE ACREPNBD                                                       
       ++INCLUDE FAUTL                                                          
       ++INCLUDE DDSYSELD                                                       
                                                                                
       ++INCLUDE DDSMFFBAL                                                      
         ORG   SMFBTXT1                                                         
SMFBSENM DS    CL5                                                              
         ORG   SMFBTXT2+8                                                       
SMFBINFO DS    CL4                                                              
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052ACREPNB02 10/21/15'                                      
         END                                                                    
