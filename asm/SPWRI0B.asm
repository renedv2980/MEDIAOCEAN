*          DATA SET SPWRI0B    AT LEVEL 050 AS OF 06/17/19                      
*PHASE T2040BA,*                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'T2040B - SPOTPAK WRITER OFFLINE GENERAL ROUTINES'               
*                                                                               
***********************************************************************         
*                                                                     *         
*          SPWRI0B (T2040B) - SPOT WRITER OFFLINE GENERAL ROUTINES    *         
*                                                                     *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-27405  06/17/19 ALLOW PGROUP REQUEST FOR MULI-CLIENT REQ  *         
* AKAT SPEC-12212  02/16/18 COMSCORE SUPPORT FOR GOALS                *         
* AKAT SPEC-11692  02/16/18 NEW TARGET3 & TARGET4 KEYWORD SUPPORT     *         
* AKAT SPEC-13699  06/12/17 RELINK TO PICK UP NEW L'SBUFMAX           *         
* AKAT SPEC-6939   02/28/17 SUPPORT COMSCORE DEMO LOOKUPS FOR 17.1.5  *         
* AKAT MOXSYS-150  10/07/16 SUPPORT COMSCORE DEMOS                    *         
* AKAT SPSUG-95    06/15/16 SUPPORT NEW AD-IDCD2 KEYWORD              *         
***********************************************************************         
* 06OCT14 44 AKT -- ADD MEDIA TO PAYING REP KEY FOR ALL MEDIA REQ   *           
* 05JUN12 43 AKT -- CAUGHT AGAIN BY SBMED/SBQMED!                   *           
* 26JAN12 42 AKT -- EXPAND COMMERCIAL NAMES 2 AND 3                 *           
* 05NOV10 40 AKT -- SET WORK AREA FOR GETBFRW                       *           
* 18MAY10 39 AKT -- DON'T USE ELEM IN VGETCML - INV ROUTINES USE IT *           
*                -- CAN'T JUST RE-INIT TSAR - MUST FREE & REALLOCATE*           
* 06MAY10 38 AKT -- DO NOT INIT CML TSAR BUFFER ONLY FOR ROWS       *           
* 12APR10 37 AKT -- TSAR OFF THE COMMERCIAL RECORDS                 *           
* 20OCT09 36 AKT -- FIX COMMERCIAL TABLE BUG                        *           
* 05MAR09 35 AKT -- CAUGHT AGAIN BY SBMED/SBQMED!                   *           
* 23JUL08 34 AKT -- SUPPORT HIGHDEF AND CENTERCUT IN GETCML         *           
* 19MAY08 33 AKT -- GETNINV TO USE NINREC INSTEAD OF AIO2           *           
* 31MAR08 32 AKT -- PROCESS NEW ACOM RECORDS                        *           
* 11FEB08 31 AKT -- PUT/GET EST BOOK TYPE IN TSAR BUFFER            *           
* 11FEB08 30 AKT -- DATE CONTROL IS IN BINARY - DON'T CHANGE!       *           
* 19DEC07 29 AKT -- GM BILLING RESTRUCTURE SUPPORT                  *           
* 01DEC07 28 AKT -- MERGE BUY AND OM DATA SUPPORT                   *           
* 13JUL07 27 AKT -- SBASTANT GOT BIGGER & ADD VGTSTABF ROUTINE      *           
* 29JUN06 26 AKT -- ADD GETSLP AND PUTSLP ROUTINES TO TSAR          *           
* 10NOV06 25 AKT -- PUT SBASTANT ENTRY IN GLOBALLN                  *           
* 05OCT06 24 AKT -- PUT ESTIMATE RATE IN TSAR BUFFER                *           
* 21SEP06 23 AKT -- IF LOOKING FOR UCOMMS UNDER MED=N & REQ MED=C,  *           
*                -- LOOK UNDER MED C INSTEAD                        *           
* 24AUG06 22 AKT -- PUT ESTIMATE UCOMMS IN TSAR BUFFER              *           
* 24MAY06 21 AKT -- FIX GETGEN TSAR LENGTH BUG                      *           
* 02MAY06 20 AKT -- GIVE TSAR IT'S OWN AIO BUFFER FOR TSARHIGH CALLS*           
* 15NOV05 19 AKT -- ADD PUTNINV AND GETNINV ROUTINES TO TSAR        *           
* 20AUG05 18 AKT -- ADD MEDIA TO VGETCLTN / VPUTCLTN                *           
* 08OCT04 17 AKT -- DO NOT CLOBBER ELEM OR AIO3 IN GETCML (NOW ONLY *           
*                -- CALLED AT INPUT). PROCINV USES THEM IN SPWRI01  *           
* 23AUG04 16 AKT -- CHANGE GETCML TO SUPPORT AD-ID KEYWORDS         *           
* 08JUL04 15 AKT -- SAVE SBAFLTAB IN GLOBAL                         *           
* 08JUN04 14 EFJ -- SUPPORT PER=FF (FISCAL FOLLIES) OPTION          *           
* 16APR04 13 AKT -- CHANGE VGET/PUTESTN TO INCLUDE ETYPE            *           
* 04NOV03 12 EFJ -- SAVE SBALKXTD IN GLOBAL                         *           
* 32OCT03 10 AKT -- FIX FIX MGROUP AND CGROUP X'40' BUGS            *           
* 31OCT03 09 AKT -- FIX COUNT REC (FORGOT TO CALC ELEM CODE IN LEN) *           
* 09OCT03 08 EFJ -- SUPPORT ECREATE KWD                             *           
* 20AUG03 07 EFJ -- BUILD QTR TABLE PROPERLY!                       *           
* 26FEB03 05 EFJ -- USE PRD POL IN GETESTN FOR UNAL (SBBPRD=X'FE')  *           
* 03FEB03 04 EFJ -- FIX CMML CODE                                   *           
* 17OCT02 03 AKT -- USE TSAR BUFFER FOR ALL TSARADD & TSARHIGH CALLS*           
* 06SEP02 02 EFJ -- CHANGE NMOD TO NTR - DOESN'T KILL RC            *           
* 21AUG02 01 EFJ -- CHANGE FROM SPWRIOFF TO SPWRI0B                 *           
*-------------------------------------------------------------------*           
* 25OCT01 18 EFJ -- INCREASE GLOBALLN FOR LARGER NAME POOL          *           
* 09OCT01 17 EFJ -- CHANGE *+4 TO *+8 FOR IDF                       *           
* 09MAY01 16 EFJ -- SUPPORT PURPOSE CODE KEYWORD                    *           
* 23APR01 15 EFJ -- EXTRACT CML BASIC CODE                          *           
* 02APR01 14 EFJ -- PUT MEDIA IN MKT NAMPOOL ELEMS (FOR MEDIA *)    *           
* 21FEB01 13 EFJ -- FILL IN SBBRSM(S) AT PUTMKTN                    *           
* 22JAN01 12 EFJ -- INCREASE GLOBALLN/SBAOFFBF                      *           
* 21NOV00 11 EFJ -- COPY CK CODE FOR JM                             *           
* 03OCT00 10 EFJ -- SUPPORT USER COMMENT RECORDS                    *           
* 20APR99 09 EFJ -- FIX COKE CROSS CLT CMML BUG                     *           
* 10SEP98 08 EFJ -- INCREASE SBACHKTB                               *           
* 14JUL98 07 EFJ -- ADD MKTEST TAB AT END OF GLOBAL                 *           
* 09JUL98 06 EFJ -- IF BIG MONTH TAB, ACCEPT ALL MONTHS OF SERVICE  *           
* 30JUN98 05 EFJ -- SUPPORT LARGER MONTH TABLE FOR WRI25            *           
* 24JUN98 04 EFJ -- INCREASE SBAOFFBF                               *           
* 05MAY98 03 EFJ -- Y2K                                             *           
* 14APR98 02 EFJ -- ADD NETSIZE TABLE AT END OF GLOBAL              *           
* 14APR98 01 EFJ -- LEVEL RESET                                     *           
*-------------------------------------------------------------------*           
* 03DEC97 19 EFJ -- TEST FOR BILL PERIOD TABLE IS WRONG             *           
* 09JUL97 18 EFJ -- GET CMML NAME 2 & 3                             *           
* 16JUN97 17 EFJ -- NEED MEDIA IN PUT/GETMGRN FOR MEDIA *           *           
* 13JUN97 16 EFJ -- IF NO CMLCLASS FOR CLT, TRY NO CLT              *           
* 19DEC96 15 EFJ -- FIX BUG SWITCHING DPT'S                         *           
* 17OCT96 14 EFJ -- UPGRADE CABLE AGG                               *           
* 11APR96 13 EFJ -- USE CLT CC FOR AGY CK CLIENT ALL REQUESTS       *           
*                -- ALWAYS READ CLT CC CMML RECS FOR AGY CK         *           
* 05MAR96 12 EFJ -- FIX BUG IN BILLING DATE LOGIC                   *           
* 03JAN96 11 EFJ -- NEED MED IN PUTESTN/GETESTN FOR MEDIA *         *           
* 12DEC95 10 EFJ -- FIX CBL AGG FOR POL & EST=NO                    *           
* 28NOV95 09 EFJ -- CABLE AGGREGATE ACROSS CLT/PRD/EST              *           
* 16OCT95 08 EFJ -- ROUTINES FOR COLUMN ACCUMS                      *           
* 21SEP95 07 EFJ -- MORE CABLE AGGREGATE                            *           
* 06SEP95 06 EFJ -- SUPPORT FOR CABLAGG                             *           
*                -- USE EQU FOR AGLOBAL ALLOCATION                  *           
* 30AUG95 05 EFJ -- INCREASE NAMEPOOL                               *           
* 16JUN95 04 EFJ -- HAVE GETCML BREAK OUT CLASS SPLITS FOR HOME DEP *           
* 13DEC94 03 EFJ -- SUPPORT FOR READING PW RECS IN SPOTIO           *           
* 12DEC94 02 EFJ -- SUPPORT TSAROFF IN SPOTIO (NOT IN USE)          *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
         EJECT                                                                  
T2040B   CSECT                                                                  
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
W0B      NTR1  BASE=*,LABEL=*                                                   
         LR    RA,RB                                                            
         AHI   RA,4096                                                          
         LR    R7,RA                                                            
         AHI   R7,4096                                                          
         USING W0B+4096,RA,R7                                                   
         USING GEND,RC                                                          
*                                                                               
         L     R9,ASYSD                                                         
         LR    R8,R9                                                            
         AHI   R8,4096                                                          
         USING SYSD,R9,R8                                                       
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VPUTCGRN                                                         
         B     VPUTREPN                                                         
         B     VGETREPN                                                         
         B     VPUTCNT                                                          
         B     VGETCML                                                          
         B     VPUTTOT                                                          
         B     VPUTSGRN                                                         
         B     VGETSGRN                                                         
         B     VPUTCSFL                                                         
         B     VGETCSFL                                                         
         B     VSETDPT                                                          
         B     VPUTGEN                                                          
         B     VGETGEN                                                          
         B     VGETXSPL                                                         
         B     VGETTOT                                                          
         B     VINTDRIV                                                         
         B     VGETCGRN                                                         
         B     VGENHEAD                                                         
         B     VSETDATE                                                         
         B     VPUTCLTN                                                         
         B     VPUTESTN                                                         
         B     VPUTMKTN                                                         
         B     VPUTMGRN                                                         
         B     VPUTPGRN                                                         
         B     VGETCLTN                                                         
         B     VGETESTN                                                         
         B     VGETMKTN                                                         
         B     VGETMGRN                                                         
         B     VGETPGRN                                                         
         B     VPUTCBLC                                                         
         B     VGETCBLC                                                         
         B     VGETMKTW                                                         
         B     VPUTOMBY                                                         
         B     VGETOMBY                                                         
         B     VPUTNINV                                                         
         B     VGETNINV                                                         
         B     VGETSLP                                                          
         B     VPUTSLP                                                          
         B     VGTSTABF                                                         
         B     VGETACOM                                                         
*                                                                               
NEXIT    LTR   RB,RB                                                            
         J     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
*                                                                               
XIT      XIT1  ,                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* GENERAL HEADLINE HOOK ROUTINES                                                
*                                                                               
VGENHEAD L     R2,AH1              DEAL WITH MAIN TITLE                         
         LA    R2,32(R2)                                                        
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
         MVC   0(63,R2),TITLE      (TITLES ARE ALREADY CENTERED)                
         A     R2,PWIDTH                                                        
         GOTO1 UNDERLIN,DMCB,(63,TITLE),(X'BF',(R2))                            
         A     R2,PWIDTH                                                        
         LA    R2,16(R2)                                                        
         MVC   0(32,R2),SUBTITLE   AND THE SUBTITLE                             
         TM    OPTIND2,OPTITITL    TEST SUBTITLE PERIOD MUST CHANGE             
         BZ    VGENHD2                                                          
         LA    RF,16               YES-SCAN FOR DATES                           
         CLC   0(11,R2),=C'PERIOD FROM'                                         
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   RF,*-14                                                          
         B     VGENHD2                                                          
         LA    R2,12(R2)           FOUND-FORMAT THE PERIOD                      
         GOTO1 DATCON,DMCB,(2,SBBQSTP),(5,(R2))                                 
         MVC   8(4,R2),=C' TO '                                                 
         GOTO1 (RF),(R1),(2,SBBQENDP),(5,12(R2))                                
*                                                                               
VGENHD2  L     R2,AH4              LINE UP THE LEFT SIDE                        
         LA    R2,1(R2)                                                         
         LA    R3,15                                                            
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
         LLC   R4,GLFHEADL                                                      
         DROP  R1                                                               
         SHI   R4,5                                                             
         TM    OUTIND,OUTIHEAD     OPTION TO SHUFFLE EXTRA HEADLINE             
         BO    *+6                                                              
         BCTR  R4,0                                                             
         BRAS  RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,1(R2)                                                         
         CLC   FULL,=F'15'                                                      
         BNL   *+8                                                              
         LA    R2,1(R2)                                                         
         BRAS  RE,SHUFFLE                                                       
         LA    R3,12                                                            
         BRAS  RE,GETLONG                                                       
         LA    R3,13(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BRAS  RE,SHUFFLE                                                       
*                                                                               
         CLC   SUBTIT2,BLANKS      MORE SUBTITLES                               
         BNH   XIT                                                              
         L     R2,AH4                                                           
         LA    R2,48(R2)                                                        
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
         MVC   0(32,R2),SUBTIT2                                                 
         CLC   SUBTIT3,BLANKS                                                   
         BNH   XIT                                                              
         A     R2,PWIDTH                                                        
         MVC   0(32,R2),SUBTIT3                                                 
         B     XIT                                                              
         EJECT                                                                  
* SUBSIDIARY ROUTINES FOR GENHEAD                                               
*                                                                               
         SPACE 1                                                                
* SET UP DATE BUFFERS                                                           
*                                                                               
VSETDATE MVC   DATEFORM,SBSPPROF+2  PICK UP DATE FORM FROM SPOT PROFILE         
         CLI   SBESTOWD,0          TEST ESTIMATE OUT OF WEEK ROTATION           
         BE    *+10                                                             
         MVC   SBSPPROF+8(1),SBESTOWD  YES                                      
         OC    SBQBCLT,SBQBCLT     TEST MULTI CLIENT REQUEST                    
         BNZ   SD10                                                             
         CLI   PEROPT,PEROCAL      YES,UNLESS CALENDAR MONS ASKED FOR,          
         BE    SD10                                                             
         MVI   DATEFORM,0          FORCE TO BROADCAST WEEKS                     
         MVC   SBSPPROF+6(3),=X'010101'                                         
*                                                                               
SD10     L     RE,ADAYS                                                         
         LA    RF,NDAYS                                                         
         SLL   RF,2                                                             
         XCEFL ,                                                                
         L     R1,AWEEKS                                                        
         XC    0(NWEEKS*4,R1),0(R1)                                             
         L     R1,AMONTHS                                                       
         XC    0(NMONTHS*4,R1),0(R1)                                            
         L     R1,AQTRS                                                         
         XC    0(NQTRS*4,R1),0(R1)                                              
         L     R1,AYEARS                                                        
         XC    0(NYEARS*4,R1),0(R1)                                             
         L     R1,AHYEARS                                                       
         XC    0(NHYEARS*4,R1),0(R1)                                            
         L     R1,ABILMNTH                                                      
         XC    0(NBILMNTH*6,R1),0(R1)                                           
*                                                                               
         CLI   QPERTYPE,2          TEST PERIOD REQUESTED IN MONTHS              
         BNE   SD12                                                             
         MVC   WORK+32(6),SBQSTART YES-SAVE ORIGINAL DATES IN WORK+32           
         MVC   WORK+38(6),SBQEND                                                
*                                                                               
SD12     CLC   SBQSTART+4(2),=C'00' TEST START DAY SET                          
         BNE   *+8                                                              
         MVI   SBQSTART+5,C'1'      NO-MAKE IT THE 1ST                          
         CLC   SBQEND+4(2),=C'00'   TEST END DAY SET                            
         BNE   SD14                                                             
         MVI   SBQEND+5,C'1'        NO-MAKE IT THE BROADCAST MONTH END          
         GOTO1 GETBROAD,DMCB,(1,SBQEND),WORK,GETDAY,ADDAY                       
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SBQEND,WORK+6                                                    
*                                                                               
SD14     CLI   DATEFORM,2                                                       
         BE    SD20                                                             
*                                  FORCE START AND END TO BROADCAST             
         GOTO1 GETDAY,DMCB,SBQSTART,DUB                   WEEKS                 
         SR    R4,R4                                                            
         IC    R4,0(R1)            (DAY NUMBER)                                 
         LA    R5,1                ASSUME MONDAY                                
         CLI   SBSPPROF+8,0        TEST SPECIAL DAY OF WEEK                     
         BE    SD16                                                             
         LLC   R5,SBSPPROF+8       FOUND ONE - USE THAT                         
*                                                                               
SD16     CR    R4,R5                                                            
         BE    SD18                                                             
         BH    *+8                                                              
         LA    R4,7(R4)                                                         
         SR    R5,R4                                                            
         MVC   DUB,SBQSTART                                                     
         GOTO1 ADDAY,DMCB,DUB,SBQSTART,(R5)                                     
*                                                                               
SD18     GOTO1 GETDAY,DMCB,SBQEND,DUB                                           
         IC    R4,0(R1)                                                         
         LA    R5,7                ASSUME ENDING ON SUNDAY                      
         CLI   SBSPPROF+8,0                                                     
         BE    SD19                                                             
         LLC   R5,SBSPPROF+8       START DAY FOUND IN PROFILE                   
         BCT   R5,SD19             END IS 1 LESS THAN THIS                      
         LA    R5,7                                                             
*                                                                               
SD19     CR    R4,R5                                                            
         BE    SD20                                                             
         BL    *+8                                                              
         LA    R5,7(R5)                                                         
         SR    R5,R4                                                            
         MVC   DUB,SBQEND                                                       
         GOTO1 ADDAY,DMCB,DUB,SBQEND,(R5)                                       
*                                                                               
SD20     GOTO1 DATCON,DMCB,(0,SBQSTART),(2,SBBQSTP)                             
         GOTO1 (RF),(R1),(0,SBQSTART),(3,SBBQST)                                
         GOTO1 (RF),(R1),(0,SBQEND),(2,SBBQENDP)                                
         GOTO1 (RF),(R1),(0,SBQEND),(3,SBBQEND)                                 
*                                                                               
         MVC   SUBTIT2,BLANKS      CLEAR 2ND & 3RD SUBTITLES                    
         MVC   SUBTIT3,BLANKS                                                   
*                                                                               
         CLI   DATEOPT,DOPAID      TEST DATE=PD                                 
         BNE   SD20A                                                            
         MVC   SBQPAYST,SBBQSTP    YES-REQUEST DATES BECOME PAID DATES          
         MVC   SBQPAYEN,SBBQENDP                                                
         MVC   QPPERTYP,QPERTYPE                                                
         LA    R2,SUBTIT2                                                       
         CLI   QPERTYPE,2                                                       
         BNE   SD26                                                             
         CLC   WORK+32(4),WORK+38                                               
         BNE   SD27                                                             
         OI    QPPERTYP,X'80'                                                   
         B     SD27                                                             
*                                                                               
SD20A    CLI   DATEOPT,DOBILL      TEST DATE=BD                                 
         BNE   SD20B                                                            
         MVC   SBQBILST,SBBQSTP    YES-REQUEST DATES BECOME BILL DATES          
         MVC   SBQBILEN,SBBQENDP                                                
         MVC   QBPERTYP,QPERTYPE                                                
         CLI   QPERTYPE,2                                                       
         BNE   SD23A                                                            
         CLC   WORK+32(4),WORK+38                                               
         BNE   SD23B                                                            
         OI    QBPERTYP,X'80'                                                   
         B     SD23B                                                            
*                                  SET SUBTITLE NOW                             
SD20B    MVC   SUBTITLE,BLANKS                                                  
         CLI   QPERTYPE,2          TEST PERIOD REQUESTED IN MONTHS              
         BE    SD21                                                             
         MVC   WORK+32(6),SBQSTART NO-DATES WERE NOT SAVED EARLIER              
         MVC   WORK+38(6),SBQEND                                                
         B     SD21A                                                            
*                                                                               
SD21     CLC   WORK+32(4),WORK+38  TEST SINGLE MONTH REQUEST                    
         BNE   SD21A                                                            
         MVC   SUBTITLE(17),=C'FOR THE PERIOD OF'                               
         LA    R4,SUBTITLE+18                                                   
         GOTO1 DATCON,DMCB,WORK+32,(6,0(R4))                                    
         B     SD22                                                             
*                                                                               
SD21A    MVC   SUBTITLE(11),=C'PERIOD FROM'                                     
         LA    R4,SUBTITLE+12                                                   
         LA    R3,QPERTYPE                                                      
         BAS   RE,FMTDATES                                                      
*                                                                               
SD22     GOTO1 CENTER,DMCB,SUBTITLE,32                                          
*                                                                               
         CLC   SUBTIT2,BLANKS      UNLESS MORE SUBTITLES                        
         BH    SD30                ALREADY DONE,                                
         LA    R2,SUBTIT2                                                       
         LA    R3,SBQBHIST         TEST ANY BILL HEADER DATE FILTERS            
         OC    SBQBHIST,SBQBHIST                                                
         BZ    *+18                                                             
         MVC   0(7,R2),=C'INVOICE'                                              
         LA    R2,8(R2)                                                         
         B     SD22A                                                            
         LA    R3,SBQBHRST                                                      
         OC    SBQBHRST,SBQBHRST                                                
         BZ    *+18                                                             
         MVC   0(3,R2),=C'RUN'                                                  
         LA    R2,4(R2)                                                         
         B     SD22A                                                            
         LA    R3,SBQBHDST                                                      
         OC    SBQBHDST,SBQBHDST                                                
         BZ    *+18                                                             
         MVC   0(3,R2),=C'DUE'                                                  
         LA    R2,4(R2)                                                         
         B     SD22A                                                            
         LA    R3,SBQBHPST                                                      
         OC    SBQBHPST,SBQBHPST                                                
         BZ    SD23                                                             
         MVC   0(4,R2),=C'POST'                                                 
         LA    R2,5(R2)                                                         
*                                                                               
SD22A    MVC   0(5,R2),=C'DATES'         YES-FORMAT BILL HEADER FILTER          
         GOTO1 DATCON,DMCB,(2,(R3)),(5,6(R2)) DATES TO 2ND SUBTITLE             
         MVI   14(R2),C'-'                                                      
         GOTO1 (RF),(R1),(2,2(R3)),(5,15(R2))                                   
         GOTO1 CENTER,DMCB,SUBTIT2,32                                           
         B     SD30                                                             
*                                                                               
SD23     OC    SBQBILST,SBQBILST   TEST BILLING PERIOD                          
         BZ    SD26                                                             
         LA    R2,SUBTIT3                                                       
*                                                                               
SD23A    GOTO1 DATCON,DMCB,(2,SBQBILST),WORK+32                                 
         GOTO1 (RF),(R1),(2,SBQBILEN),WORK+38                                   
*                                                                               
SD23B    CLI   QBPERTYP,0          TEST PERIOD REQUESTED IN MONTHS              
         BE    SD24                                                             
         TM    QBPERTYP,X'80'      YES-TEST SINGLE MONTH REQUEST                
         BZ    SD23C                                                            
         MVC   SUBTIT2(17),=C'BILLING PERIOD OF'                                
         GOTO1 DATCON,DMCB,WORK+38,(6,SUBTIT2+18)                               
         B     SD25                                                             
*                                                                               
SD23C    GOTO1 GETBROAD,DMCB,(1,WORK+32),WORK+44,GETDAY,ADDAY                   
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+32(4),WORK+50  MAKE SURE START MONTH IS CORRECT             
*                                                                               
SD24     MVC   SUBTIT2(11),=C'BILL PERIOD'                                      
         LA    R4,SUBTIT2+12                                                    
         LA    R3,QBPERTYP                                                      
         BAS   RE,FMTDATES                                                      
*                                                                               
SD25     GOTO1 CENTER,DMCB,SUBTIT2,32                                           
         CLI   DATEOPT,DOBILL                                                   
         BE    SD29A                                                            
*                                                                               
SD26     OC    SBQPAYST,SBQPAYST   TEST PAY PERIOD                              
         BZ    SD30                                                             
         GOTO1 DATCON,DMCB,(2,SBQPAYST),WORK+32                                 
         GOTO1 (RF),(R1),(2,SBQPAYEN),WORK+38                                   
*                                                                               
SD27     CLI   QPPERTYP,0          TEST PERIOD REQUESTED IN MONTHS              
         BE    SD28                                                             
         TM    QPPERTYP,X'80'      YES-TEST SINGLE MONTH REQUEST                
         BZ    SD27A                                                            
         MVC   0(16,R2),=C'PAYING PERIOD OF'                                    
         GOTO1 DATCON,DMCB,WORK+38,(6,17(R2))                                   
         B     SD29                                                             
*                                                                               
SD27A    GOTO1 GETBROAD,DMCB,(1,WORK+32),WORK+44,GETDAY,ADDAY                   
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+32(4),WORK+50  MAKE SURE START MONTH IS CORRECT             
*                                                                               
SD28     MVC   0(10,R2),=C'PAY PERIOD'                                          
         LA    R4,11(R2)                                                        
         LA    R3,QPPERTYP                                                      
         BAS   RE,FMTDATES                                                      
*                                                                               
SD29     GOTO1 CENTER,DMCB,(R2),32                                              
*                                                                               
SD29A    CLI   DATEOPT,0           TEST DATE OPTION SET                         
         BE    SD30                                                             
         MVC   SUBTITLE,SUBTIT2    YES-MAIN SUBTITLE BECOMES                    
         XC    SUBTIT2,SUBTIT2         BILL/PAY PERIOD                          
*                                                                               
*                                  BUILD DATE TABLES                            
SD30     XC    WORK,WORK                                                        
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         TM    SBQPER,SBQPWK       TEST NEED WEEKS                              
         BZ    SD32                                                             
         L     R4,AWEEKS           BUILD WEEKS                                  
         GOTO1 MOBILE,DMCB,(55,SBQSTART),(4,(R4)),WORK,SBSPPROF                 
         CLI   0(R4),X'FF'                                                      
         BE    *+12                                                             
         LA    R4,4(R4)                                                         
         B     *-12                                                             
         MVI   0(R4),0                                                          
*                                                                               
SD32     TM    SBQPER,SBQPMN+SBQPQT+SBQPHY+SBQPYR  TEST NEED MONTHS             
         BZ    SD48                                                             
*                                                                               
         LA    RE,SBQSTART         BUILD P1                                     
         ST    RE,DMCB                                                          
         MVI   DMCB,NMONTHS-1                                                   
         TM    SBQPER,SBQPBIG      EXTENDED MONTH TAB?  (WRI25)                 
         BZ    *+8                                                              
         MVI   DMCB,48                                                          
*                                                                               
         L     R4,AMONTHS          BUILD P2                                     
         ST    R4,DMCB+4                                                        
         MVC   DMCB+4(1),DATEFORM                                               
         CLI   PEROPT,PEROFF       USE FISCAL FOLLIES?                          
         BNE   *+8                  NO                                          
         MVI   DMCB+4,C'P'                                                      
*                                                                               
         GOTO1 MOBILE,DMCB,,,WORK,SBSPPROF                                      
         CLI   0(R4),X'FF'                                                      
         BE    *+12                                                             
         LA    R4,4(R4)                                                         
         B     *-12                                                             
         MVI   0(R4),0                                                          
*                                                                               
         TM    SBQPER,SBQPQT       TEST NEED QUARTERS                           
         BZ    SD42                                                             
         CLI   DATEFORM,10         YES-TEST 4-WEEK MONTHS                       
         BNE   SD34                                                             
         L     R4,AQTRS            YES-CALL MOBILE TO GET QUARTERS              
         LA    R5,17                                                            
         SR    R1,R1                                                            
         NI    SBWRPROF+1,X'0F'    WR PROFILE HAS QTR PATTERN                   
         ICM   R1,1,SBWRPROF+1                                                  
         BZ    *+8                                                              
         LA    R5,13(R1)                                                        
         GOTO1 MOBILE,DMCB,('NQTRS',SBQSTART),((R5),(R4)),WORK,SBSPPROF         
         CLI   0(R4),X'FF'                                                      
         BE    *+12                                                             
         LA    R4,4(R4)                                                         
         B     *-12                                                             
         MVI   0(R4),0                                                          
         B     SD42                                                             
*                                                                               
SD34     L     R4,AMONTHS                                                       
         L     R5,AQTRS                                                         
         LA    R3,NQTRS                                                         
         CLI   SBQSPQRT,C'Y'                                                    
         BE    SD39                                                             
*&&DO                                                                           
*                                                                               
* THIS CODE TO BUILD THE QUARTER PERIODS IS WRONG, UNLESS THE START             
* OF REQUEST PERIOD HAPPENS TO FALL ON THE START OF A QUARTER!                  
* IT WAS REPLACED WITH CODE BELOW 21AUG03                                       
*                                                                               
SD36     LR    R2,R4                                                            
         MVC   0(4,R5),0(R2)                                                    
         LA    R2,4(R2)                                                         
         OC    2(2,R2),2(R2)                                                    
         BZ    SD38                                                             
         MVC   2(2,R5),2(R2)                                                    
         LA    R2,4(R2)                                                         
         OC    2(2,R2),2(R2)                                                    
         BZ    SD38                                                             
         MVC   2(2,R5),2(R2)                                                    
*                                                                               
SD38     LA    R4,12(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R3,SD36                                                          
         B     SD42                                                             
*&&                                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R4)),(X'23',FULL)  GET REQ START MON            
         XR    RE,RE                                                            
         ICM   RE,1,SBSPPROF+6     GET FY START MON                             
         BNZ   *+8                                                              
         LHI   RE,1                IF NONE, DEFAULT TO 1                        
*                                                                               
         LLC   RF,FULL+1           START MON OF REQ                             
         SR    RF,RE               FIND REMAINDER OF QTR (0,1,2)                
         BNM   *+8                                                              
         AHI   RF,12                                                            
         XR    RE,RE                                                            
         D     RE,=F'3'                                                         
         LHI   RF,2                                                             
         SR    RF,RE               RF=#MONTHS TO END OF QTR (-1)                
         MHI   RF,4                                                             
         LA    R2,2(R4,RF)         R2=A(QTR END MONTH)                          
*                                                                               
SD36     MVC   0(2,R5),0(R4)       SET START DATE OF Q                          
         OC    0(2,R2),0(R2)       PAST END OF REQ PERIOD?                      
         BNZ   *+12                 NO                                          
         AHI   R2,-4                ELSE BACK UP TO LAST END DATE               
         B     *-14                                                             
         MVC   2(2,R5),0(R2)       SET END DATE OF Q                            
*                                                                               
         LA    R5,4(R5)            NEXT QTR PERIOD                              
         LA    R4,2(R2)            START MONTH OF NEXT QTR                      
         OC    0(4,R4),0(R4)       PAST END OF REQ PERIOD?                      
         BZ    SD42                 YES - NO MORE DATES                         
         LA    R2,12(R2)           NEXT END MONTH                               
         BCT   R3,SD36                                                          
         B     SD42                                                             
*                                                                               
SD39     LA    R2,SPQRTTAB                                                      
*                                                                               
SD40     CLC   0(2,R2),SBQAGY                                                   
         BE    SD41                                                             
         LA    R2,L'SPQRTTAB(R2)                                                
         CLI   0(R2),X'FF'                                                      
         BNE   SD40                                                             
         B     SD36                NO SPECIAL TABLE FOUND                       
*                                                                               
SD41     GOTO1 DATCON,DMCB,(2,2(R4)),(3,DUB)                                    
         LLC   RE,DUB+1            GET END DATE MONTH                           
         BCTR  RE,0                                                             
         IC    RE,2(RE,R2)         GET QUARTER NUMBER FOR MONTH                 
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,0(R5,RE)         POINT TO QUARTER ENTRY                       
*                                                                               
         OC    0(2,RE),0(RE)       TEST QTR START DATE PRESENT                  
         BZ    *+14                NO                                           
         CLC   0(2,RE),0(R4)       CURRENT QTR START TO MONTH START             
         BL    *+10                LOW - KEEP IT                                
         MVC   0(2,RE),0(R4)                                                    
*                                                                               
         CLC   2(2,RE),2(R4)       CURRENT QTR END TO MONTH END                 
         BH    *+10                LATER - KEEP IT                              
         MVC   2(2,RE),2(R4)                                                    
*                                                                               
         LA    R4,4(R4)            NEXT MONTH ENTRY                             
         OC    0(2,R4),0(R4)                                                    
         BNZ   SD41                                                             
*                                                                               
SD42     TM    SBQPER,SBQPHY       TEST NEED HALF YEARS                         
         BZ    SD45                                                             
         L     R4,AMONTHS                                                       
         L     R5,AHYEARS                                                       
         LA    R3,NHYEARS                                                       
         MVI   HALF,1              SET HALF=START MONTH OF 1ST HALF YR          
         MVI   HALF+1,7                HALF+1=START MONTH OF 2ND HALF           
         MVC   0(2,R5),0(R4)                                                    
         CLI   SBSPPROF+6,1                                                     
         BNH   SD43                                                             
         LLC   RE,SBSPPROF+6                                                    
         STC   RE,HALF                                                          
         LA    RE,6(RE)                                                         
         CHI   RE,12                                                            
         BNH   *+8                                                              
         SHI   RE,12                                                            
         STC   RE,HALF+1                                                        
*                                                                               
SD43     MVC   2(2,R5),2(R4)                                                    
         OC    4(2,R4),4(R4)                                                    
         BZ    SD48                                                             
         GOTO1 DATCON,DMCB,(2,6(R4)),(3,FULL)                                   
         CLC   HALF(1),FULL+1                                                   
         BE    *+14                                                             
         CLC   HALF+1(1),FULL+1                                                 
         BNE   SD44                                                             
         BCT   R3,*+8                                                           
         B     SD48                                                             
         LA    R5,4(R5)                                                         
         MVC   0(2,R5),4(R4)                                                    
*                                                                               
SD44     LA    R4,4(R4)                                                         
         B     SD43                                                             
*                                                                               
SD45     TM    SBQPER,SBQPYR       TEST NEED YEARS                              
         BZ    SD48                                                             
         L     R4,AMONTHS                                                       
         L     R5,AYEARS                                                        
         LA    R3,NYEARS                                                        
         MVI   BYTE,1              SET BYTE=START MONTH OF YEAR                 
         CLI   SBSPPROF+6,1                                                     
         BNH   *+10                                                             
         MVC   BYTE,SBSPPROF+6                                                  
         MVC   0(2,R5),0(R4)                                                    
*                                                                               
SD46     MVC   2(2,R5),2(R4)                                                    
         OC    4(2,R4),4(R4)                                                    
         BZ    SD48                                                             
         GOTO1 DATCON,DMCB,(2,6(R4)),(3,FULL)                                   
         CLC   BYTE,FULL+1                                                      
         BNE   SD47                                                             
         BCT   R3,*+8                                                           
         B     SD48                                                             
         LA    R5,4(R5)                                                         
         MVC   0(2,R5),4(R4)                                                    
*                                                                               
SD47     LA    R4,4(R4)                                                         
         B     SD46                                                             
*                                                                               
SD48     TM    SBQPER,SBQPDY       TEST NEED DAYS                               
         BZ    SD50                                                             
         L     R4,ADAYS                                                         
         MVC   0(2,R4),SBBQSTP                                                  
         MVC   2(2,R4),0(R4)                                                    
         LA    R0,NDAYS-2                                                       
         LA    R4,4(R4)                                                         
         MVC   WORK+16(6),SBQSTART                                              
*                                                                               
SD49     GOTO1 ADDAY,DMCB,WORK+16,WORK+22,1                                     
         GOTO1 DATCON,DMCB,WORK+22,(2,HALF)                                     
         CLC   HALF,SBBQENDP                                                    
         BH    SD50                                                             
         MVC   0(2,R4),HALF                                                     
         MVC   2(2,R4),0(R4)                                                    
         MVC   WORK+16(6),WORK+22                                               
         LA    R4,4(R4)                                                         
         BCT   R0,SD49                                                          
*                                                                               
SD50     TM    SBQSKIP,SBQSKBIL    TEST READING STATION BILL RECORDS            
         BZ    *+12                                                             
         TM    SBQREAD,SBQRDBH     OR BILL HEADER RECORDS                       
         BZ    SD70                                                             
         XC    BILLST,BILLST                                                    
         MVC   BILLEND,XFF                                                      
         TM    SBQPER,SBQPBIG      EXTENDED MONTH TAB?  (WRI25)                 
         BNZ   SD68                 YES - TAKE ALL MONTHS OF SERVICE            
         CLI   DATEOPT,DOBILL      TEST DATE=BD                                 
         BE    SD68                YES-ACCEPT ALL MONTHS OF SERVICE             
         OC    SBBQST,SBBQST       CHECK THAT REQUEST DATES ARE SET             
         BZ    SD68                                                             
         LA    R4,WORK+16                                                       
         MVC   0(6,R4),SBQSTART    SET BILLING PERIODS                          
         MVC   6(6,R4),SBQEND                                                   
*                                                                               
* GO BACK 26 MONTHS                                                             
         MVC   DMCB+8(4),=F'-26'                                                
         GOTO1 ADDAY,DMCB,(C'M',SBQSTART),0(R4)                                 
*                                                                               
*&&DO                                                                           
         PACK  DUB,SBQSTART+2(2)   GO BACK 26 MONTHS                            
         LA    RF,=P'2'                                                         
         SP    DUB,=P'2'                                                        
         BP    *+14                                                             
         LA    RF,=P'3'                                                         
         AP    DUB,=P'12'                                                       
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R4),DUB                                                      
*                                                                               
         PACK  DUB,SBQSTART(2)                                                  
* I THINK THE NEXT COMMENT SHOULD BE 2 OR 3 YEARS                               
         SP    DUB,0(1,RF)         GO BACK 1 OR 2 YEARS                         
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R4),DUB                                                      
*&&                                                                             
*                                                                               
* GO FORWARD 2 MONTHS                                                           
         MVC   DMCB+8(4),=F'2'                                                  
         GOTO1 ADDAY,DMCB,(C'M',SBQEND),6(R4)                                   
*                                                                               
*&&DO                                                                           
         PACK  DUB,8(2,R4)         AND FORWARD 2 MONTHS                         
         AP    DUB,=P'2'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R4),DUB                                                      
         CP    DUB,=P'12'                                                       
         BNH   SD52                                                             
         SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R4),DUB                                                      
*                                                                               
         PACK  DUB,6(2,R4)                                                      
         AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R4),DUB                                                      
*&&                                                                             
*                                                                               
SD52     IC    R0,SBSPPROF+2       DATE CONTROL                                 
*                                                                               
         GOTO1 MOBILE,DMCB,(52,(R4)),((R0),AIO3),WORK,SBSPPROF                  
*                                                                               
* NOW WORK OUT PERIOD NUMBERS - FIND PLACES WHERE YEAR CHANGES                  
*                                                                               
         L     R5,AIO2                                                          
         XCEF  (R5),316                                                         
         L     R4,AIO3                                                          
*                                                                               
SD54     BAS   RE,CKNEWYR          TEST START OF NEW YEAR                       
         BE    SD56                YES                                          
         LA    R4,4(R4)                                                         
         B     SD54                                                             
*                                                                               
SD56     LLC   R3,2(R4)            SET INITIAL YEAR                             
         SRL   R3,1                                                             
*                                                                               
SD58     LA    R1,1                FOR PERIOD NUMBERS WITHIN YEAR               
*                                                                               
SD59     STC   R3,0(R5)            YEAR                                         
         STC   R1,1(R5)            MONTH                                        
         MVC   2(4,R5),0(R4)       START-END OF PERIOD                          
         LA    R5,6(R5)                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'         TEST E-O-L                                   
         BE    SD60                                                             
         LA    R1,1(R1)                                                         
         CHI   R1,2                MAKE SURE THERE'S NOT 2 YEAR CHANGES         
         BE    SD59                TOGETHER                                     
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BNE   SD59                NO - CONTINUE                                
         LA    R3,1(R3)            BUMP YEAR                                    
         B     SD58                                                             
*                                                                               
SD60     L     R4,AIO2             BUILD SAVED LIST OF BILL MONTHS              
         L     R5,ABILMNTH                                                      
         LA    R0,NBILMNTH                                                      
*                                                                               
SD62     CLC   SBBQSTP,4(R4)                                                    
         BNH   *+12                                                             
         LA    R4,6(R4)                                                         
         B     SD62                                                             
         CLC   SBBQSTP,2(R4)       FIRST MONTH MUST NOT START BEFORE            
         BNH   SD63                REQUEST START                                
*****         BE    SD63                REQUEST START                           
         LA    R4,6(R4)                                                         
         CLC   SBBQENDP,4(R4)                                                   
         BL    SD68                                                             
*                                                                               
SD63     MVC   BILLST,0(R4)        FIRST REQUESTED BILL MONTH                   
*                                                                               
SD64     OC    0(2,R4),0(R4)                                                    
         BZ    SD66                                                             
*****         CLC   SBBQENDP,4(R4)                                              
         CLC   SBBQENDP,2(R4)      USE START OF PERIOD, NOT END                 
         BL    SD66                                                             
         MVC   BILLEND,0(R4)       LAST REQUESTED BILL MONTH                    
         MVC   0(6,R5),0(R4)                                                    
         LA    R4,6(R4)                                                         
         LA    R5,6(R5)                                                         
         BCT   R0,SD64                                                          
         DC    H'0'                MORE THAN MAX N'BILL MONTHS                  
*                                                                               
SD66     OC    BILLEND,BILLEND     TEST AT LEAST ONE COMPLETE MONTH             
         BZ    *+14                                                             
         CLC   BILLEND,XFF                                                      
         BNE   SD68                                                             
         XC    BILLST,BILLST       NO                                           
         XC    BILLEND,BILLEND                                                  
*                                                                               
SD68     MVC   SBBQBLST,BILLST     SET BILLING START/END MONTH NUMBERS          
         MVC   SBBQBLEN,BILLEND    IN SPOTBLOCK                                 
*                                                                               
SD70     B     SDX                                                              
*                                                                               
SDX      B     XIT                                                              
         SPACE 2                                                                
SPQRTTAB DS    0XL14     J F M A M J J A S O N D                                
         DC    C'JW',AL1(1,1,2,2,2,3,3,3,4,4,4,4)                               
         DC    X'FF'                                                            
         SPACE 2                                                                
*                J F M A M J J A S O N D                                        
MONENDTB DC    C'312831303130313130313031'                                      
         SPACE 2                                                                
FMTDATES LR    R0,RE                                                            
         ST    R4,DMCB+4                                                        
         MVI   DMCB+4,5                                                         
         CLI   0(R3),0                                                          
         BE    *+8                                                              
         MVI   DMCB+4,6                                                         
         GOTO1 DATCON,DMCB,WORK+32                                              
         LA    R4,7(R4)                                                         
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         LA    R4,2(R4)                                                         
         MVC   0(2,R4),=C'TO'                                                   
         LA    R4,3(R4)                                                         
         STCM  R4,7,DMCB+5                                                      
         GOTO1 (RF),(R1),WORK+38                                                
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO FIND START OF NEW YEAR                                             
*        1) A PERIOD THAT SPANS YEAR CHANGE                                     
*           AND BEGINS NO FURTHER AWAY                                          
*           FROM 12/31 THAN IT ENDS                                             
*   OR   2) A PERIOD THAT STARTS BEFORE 1/14                                    
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R4)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         LLC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
         EJECT                                                                  
* ROUTINES TO MAINTAIN NAME BUFFER                                              
*                                                                               
VPUTCGRN DS    0H                  PUT CLIENT  GROUP                            
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,24       LENGTH                                       
         MVI   0(R4),NPCGRCDQ      REC TYPE (7)                                 
         MVC   1(2,R4),SBBCGR      CLIENT GROUP                                 
*                                                                               
         LA    R4,MYDATA                                                        
         MVC   0(24,R4),0(R1)      R1=A(CLIENT GROUP NAME)                      
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
VPUTCLTN DS    0H                  PUT CLIENT                                   
*                                                                               
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,20       LENGTH                                       
         MVI   0(R4),NPCLTCDQ      REC TYPE (1)                                 
         MVC   1(1,R4),SBMED       MEDIA                                        
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BE    *+10                YES                                          
         MVC   1(1,R4),SBQMED      NO - USE SBQMED!                             
         MVC   2(3,R4),SBCLT       CLIENT                                       
         LA    R4,MYDATA                                                        
         MVC   0(20,R4),SBCLTNM    CLIENT NAME                                  
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
VPUTESTN DS    0H                  PUT ESTIMATE                                 
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,14       LENGTH                                       
         MVI   0(R4),NPESTCDQ      REC TYPE (2)                                 
         MVC   1(L'SBMED,R4),SBMED MEDIA                                        
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BE    *+10                YES                                          
         MVC   1(L'SBQMED,R4),SBQMED                                            
         MVC   2(3,R4),SBCLT       CLIENT                                       
*                                                                               
VPUTEST2 MVC   5(1,R4),SBBPRD      PRODUCT                                      
         MVC   6(1,R4),SBBEST      ESTIMATE                                     
*                                                                               
         LA    R4,MYDATA           DATA OF RECORD                               
         MVC   0(6,R4),SBESTDEM                                                 
         MVC   6(2,R4),SBESTSTP                                                 
         MVC   8(2,R4),SBESTNDP                                                 
         L     RF,SBAIO1                                                        
         MVC   10(2,R4),ECRDATE-ESTHDRD(RF)                                     
         MVC   12(1,R4),ETYPE-ESTHDRD(RF)                                       
         MVC   13(1,R4),ERATE-ESTHDRD(RF)                                       
         MVC   14(1,R4),EBKTYPE-ESTHDRD(RF)                                     
*                                                                               
         LA    R1,15(R4)           ELEMENTS START HERE                          
         LLC   R3,THISRECL+1       CURRENT RECORD LENGTH                        
*                                                                               
         TM    DATAIND3,DIESTNM    TEST EST NAME NEEDED                         
         BZ    VPUTEST3            NOPE                                         
         OC    SBESTNM,SBESTNM     HAVE ESTIMATE DESCRIPTION?                   
         BZ    VPUTEST3            NOPE                                         
         MVI   0(R1),X'10'         EST NAME IS X'10' ELEMENT                    
         LA    R5,SBESTNM+19       LAST BYTE OF ESTIMATE DESCRIPTION            
         LA    R6,20               LENGTH OF ESTIMATE DESCRIPTION               
         MVI   BYTE,X'40'          EST DESCRIPTION IS SPACE PADDED              
         MVC   WORK(L'SBESTNM),SBESTNM                                          
         BAS   RE,ESTELEM          BUILD THE ELEMENT                            
*                                                                               
VPUTEST3 TM    SBEUDEF,SBEUEST1        USER FIELD 1 REQUIRED?                   
         BZ    VPUTEST4                NO                                       
         LA    R2,EUSER1-ESTHDRD(RF)   ESTIMATE UCOMM 1                         
         L     RE,SBAIO2                                                        
         TM    SBEFLAG5,SBE5UPOL   UDEF=POL?                                    
         BZ    *+18                NOPE                                         
         CLC   =C'POL',4(RF)       PROCESSING POL ESTIMATE?                     
         BE    *+8                 YES                                          
         LA    R2,EUSER1-ESTHDRD(RE)   ESTIMATE UCOMM 1                         
*                                                                               
         OC    0(L'EUSER1,R2),0(R2)    HAVE ESTIMATE USER FIELD 1?              
         BZ    VPUTEST4                NO                                       
         MVI   0(R1),X'20'             YES - THIS IS ELEMENT X'20'              
         LA    R5,L'EUSER1-1(R2)       LAST BYTE OF ESTIMATE USER FLD1          
         LA    R6,L'EUSER1             LENGTH OF EST USER FIELD 1               
         MVI   BYTE,0                  END OF EST USER FIELD 1 IS X'00'         
         MVC   WORK(L'EUSER1),0(R2)    MOVE EST USER FIELD 1 TO WORK            
         BAS   RE,ESTELEM              BUILD THE ELEMENT                        
*                                                                               
VPUTEST4 TM    SBEUDEF,SBEUEST2        USER FIELD 2 REQUIRED                    
         BZ    VPUTEST5                NO                                       
         LA    R2,EUSER2-ESTHDRD(RF)   ESTIMATE USER FIELD 2?                   
         L     RE,SBAIO2                                                        
         TM    SBEFLAG5,SBE5UPOL   UDEF=POL?                                    
         BZ    *+18                NOPE                                         
         CLC   =C'POL',4(RF)       PROCESSING POL ESTIMATE?                     
         BE    *+8                 YES                                          
         LA    R2,EUSER2-ESTHDRD(RE)   ESTIMATE UCOMM 1                         
*                                                                               
         OC    0(L'EUSER2,R2),0(R2)    HAVE ESTIMATE USER FIELD 2?              
         BZ    VPUTEST5                NO                                       
         MVI   0(R1),X'30'             YES - THIS IS ELEMENT X'30'              
         LA    R5,L'EUSER2-1(R2)       LAST BYTE OF ESTIMATE USER FLD2          
         LA    R6,L'EUSER2             LENGTH OF EST USER FIELD 2               
         MVI   BYTE,0                  END OF EST USER FIELD 2 IS X'00'         
         MVC   WORK(L'EUSER2),0(R2)    MOVE EST USER FIELD 2 TO WORK            
         BAS   RE,ESTELEM              BUILD THE ELEMENT                        
*                                                                               
VPUTEST5 OC    SBESTDM2,SBESTDM2       HAVE TARGET3/4 DEMOS?                    
         BZ    VPUTEST6                NO                                       
         MVI   0(R1),X'40'             TARGET3/4 DEMOS = X'40' ELEMENT          
         LA    R6,12                   LENGTH OF 2 TARGET DEMOS                 
         MVC   WORK(12),SBESTDM2       2 COMSCORE DEMOS                         
         BAS   RE,ESTELEM2             BUILD THE ELEMENT                        
*                                                                               
VPUTEST6 AHI   R3,1                    MAKE SURE END OF REC HAS 0!              
         STC   R3,THISRECL+1           RE-ADJUST RECORD LENGTH                  
*                                                                               
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
ESTELEM  DS    0H                                                               
*                                                                               
         CLC   0(1,R5),BYTE        LAST CHARACTER?                              
         BH    *+10                YES                                          
         BCTR  R5,0                NO - LOOK AT PREVIOUS CHARACTER              
         BCT   R6,*-12                                                          
*                                                                               
ESTELEM2 AHI   R6,2                LENGTH = X'10' + LEN + EST DESC LEN          
         STC   R6,1(R1)                                                         
         SHI   R6,3                MINUS X'10', LEN AND 1 FOR EX                
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),WORK        ** EXECUTED **                               
         LA    R1,3(R1,R6)         NEXT ELEMENT GOES HERE                       
         LA    R3,3(R3,R6)         NEW RECORD LENGTH                            
         BR    RE                                                               
*                                                                               
VPUTMKTN DS    0H                  PUT MARKET                                   
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,32       LENGTH                                       
         MVI   0(R4),NPMKTCDQ      REC TYPE (3)                                 
         MVC   1(1,R4),SBQMED      MEDIA                                        
         CLI   SBQMED,C'*'                                                      
         BNE   *+10                                                             
         MVC   1(1,R4),SBMED       MEDIA                                        
         MVC   2(2,R4),SBBMKT      MARKET                                       
*                                                                               
         LA    R4,MYDATA                                                        
         MVC   0(24,R4),SBMKTNM    MARKET NAME                                  
         LA    RE,SBMKTREC                                                      
         USING MKTRECD,RE                                                       
*                                                                               
         CLI   MKTRS1,C'0'                                                      
         BNE   *+10                                                             
         MVC   24(2,R4),MKTRSM1    NSI MARKET NUMBER                            
         CLI   MKTRS1,C'1'                                                      
         BNE   *+10                                                             
         MVC   26(2,R4),MKTRSM1    ARB MARKET NUMBER                            
         CLI   MKTRS2,C'0'                                                      
         BNE   *+10                                                             
         MVC   24(2,R4),MKTRSM2                                                 
         CLI   MKTRS2,C'1'                                                      
         BNE   *+10                                                             
         MVC   26(2,R4),MKTRSM2                                                 
         MVC   SBBRSM0,24(R4)      NSI MARKET NUM                               
         MVC   SBBRSM1,26(R4)      ARB MARKET NUM                               
*                                                                               
         TM    SBQDEMOP,SBQDOMWZ                                                
         BO    *+10                                                             
         MVC   28(4,R4),=F'1'                                                   
         CLC   MKTWT,BLANKS        MARKET WEIGHT                                
         BH    *+12                                                             
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
         PACK  DUB,MKTWT                                                        
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         STCM  R0,15,28(R4)                                                     
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
         DROP  RE                                                               
*                                                                               
VPUTMGRN DS    0H                  PUT MARKET GROUP                             
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,26       LENGTH                                       
         MVI   0(R4),NPMGRCDQ      REC TYPE (5)                                 
         MVC   1(1,R4),SBMED       MEDIA                                        
*                                                                               
         LA    R2,SBAGYREC                                                      
         USING AGYHDRD,R2                                                       
         CLI   AGYPROF+7,C'C'      CANADIAN                                     
         BNE   VPMG01              NOPE                                         
         DROP  R2                                                               
         CLI   SBQMED,C'C'         MEDIA C?                                     
         BNE   VPMG01              NOPE                                         
         MVC   1(1,R4),SBQMED      MEDIA                                        
*                                                                               
VPMG01   MVC   2(2,R4),SBBMGR      MARKET GROUP                                 
         OC    SBPGRPEX(2),SBPGRPEX   TEST PRODUCT GROUP EXCEPTION              
         BZ    VPMG20                                                           
*                                                                               
         MVC   HALF,SBBPGR         PRODUCT GROUP                                
         OC    HALF,SBPG1MSK       PRODUCT GROUP LEVEL 1 MASK                   
         LA    R0,SBEXMAX          MAX # MGRP TABLES = 7                        
         LA    RE,SBPGRPEX         PRODUCT GROUP EXCEPTION TABLE                
*                                                                               
VPMG05   OC    0(2,RE),0(RE)       END OF TABLE ENTRIES?                        
         BZ    VPMG20              YES                                          
         MVC   FULL(2),0(RE)       MOVE PGROUP EXCEPTION TO FULL                
         OC    FULL(2),SBPG1MSK    PRODUCT GROUP LEVEL 1 MASK                   
         CLC   HALF,FULL           MATCH THIS PRODUCT GROUP?                    
         BE    VPMG10              YES                                          
         LA    RE,2(RE)            BUMP                                         
         BCT   R0,VPMG05                                                        
*                                                                               
VPMG10   MVC   4(2,R4),HALF                                                     
*                                                                               
VPMG20   LA    R4,MYDATA                                                        
         MVC   0(24,R4),0(R1)      R1 = A(MKTGRP NAME)                          
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
VPUTPGRN DS    0H                  PUT PRODUCT GROUP                            
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
***      MVI   THISRECL+1,24       LENGTH                                       
         MVI   THISRECL+1,36       LENGTH                                       
         MVI   0(R4),NPPGRCDQ      REC TYPE (5)                                 
         MVC   1(1,R4),SBMED       MEDIA                                        
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BE    *+10                YES                                          
         MVC   1(1,R4),SBQMED      NO - USE SBQMED!                             
         MVC   2(3,R4),SBCLT       CLIENT                                       
         MVC   5(2,R4),SBBPGR      PRODUCT GROUP                                
*                                                                               
         LA    R4,MYDATA                                                        
         MVC   0(24,R4),0(R1)      R1 = A(PRDGRP NAME)                          
         LA    RE,SBPGR1NM         RE = SBPGR1NM                                
         LA    RF,SBPGR1BK         RF = PGRDEF BREAK 1 NAME                     
         CR    R1,RE               HAVE BREAK 1 NAME?                           
         BE    *+8                 YES                                          
         LA    RF,SBPGR2BK         RF = PGRDEF BREAK 2 NAME                     
         MVC   24(12,R4),0(RF)     BUFFER SBPGR1BK/SBPGR2BK                     
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
VPUTSGRN DS    0H                  PUT STATION GROUP                            
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,24       LENGTH                                       
         MVI   0(R4),NPSGRCDQ      REC TYPE (8)                                 
         MVC   1(2,R4),SBBSGR      STATION GROUP                                
*                                                                               
         LA    R4,MYDATA                                                        
         MVC   0(24,R4),0(R1)      R1=A(STATION GROUP NAME)                     
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
VPUTREPN DS    0H                  PUT REP NAME                                 
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,22       LENGTH                                       
         MVI   0(R4),NPREPCDQ      REC TYPE (6)                                 
         MVC   1(L'SBMED,R4),SBMED                                              
         CLI   SBQMED,C'*'         FOR MEDIA * USE SBMED                        
         BE    *+10                                                             
         MVC   1(L'SBQMED,R4),SBQMED                                            
         MVC   2(3,R4),SBREP       REP NUMBER                                   
*                                                                               
         LA    R4,MYDATA                                                        
         MVC   0(22,R4),SBREPNM    REP NAME                                     
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
VPUTCSFL DS    0H                  PUT CHILD SPOT FLIGHT DATES                  
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPFLTCDQ      REC TYPE (9)                                 
         MVC   1(3,R4),SBPRD       PRODUCT                                      
*                                                                               
         LA    R4,MYDATA                                                        
         L     RE,SBADATE          A(DATE LIST)                                 
         L     RF,SBNDATES         NUM DATE ENTRIES (WHY FULLWORD?)             
         STC   RF,0(R4)            NUMBER OF DATE ENTRIES                       
         SLL   RF,2                MULTIPLY BY 4                                
         MVI   THISRECL,0                                                       
         STC   RF,THISRECL+1       LENGTH                                       
         BCTR  RF,0                -1 FOR EX                                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),0(RE)       DATE LIST                                    
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
***                                                                             
* NOTE: KEY IS 33(+2 FOR LEN) BYTES LONG, NO DATA IS PRESENT                    
***                                                                             
VPUTCNT  DS    0H                  PUT A COUNT ELEMENT                          
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
*                                                                               
         MVI   0(R4),NPCNTCDQ      10                                           
*                                                                               
         L     RE,AGLOBAL                                                       
         LA    RE,GLAIFLD-GLOBALD(RE)                                           
         L     R1,0(RE)            R1=A(ROW VALUE)                              
         LLC   R3,COUNTLEN                                                      
         SLL   R3,25               GET RID OF X'80' BIT                         
         SRL   R3,25                                                            
*                                                                               
         CHI   R3,30               CHECK ROW IS NO LONGER THAN 30               
         BNH   *+6                 (KEY IS 31 BYTES LONG)                       
         DC    H'0'                                                             
*                                                                               
         BCTR  R3,0                SUBTRACT ONE FOR EX                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),0(R1)       KEY IS ONLY AS LONG AS OUTPUT LENGTH         
         TM    COUNTLEN,X'80'      TOTALING PRD OR EST?                         
         BZ    VPUTCNT2            NO                                           
*                                                                               
         MVC   1(2,R4),SBBCLT      WE NEED THE CLIENT IN THE KEY                
         SHI   R3,2                AND WHATEVER ELSE WILL FIT                   
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R4),0(R1)                                                    
         AHI   R3,2                RESTORE KEY LENGTH                           
*                                                                               
VPUTCNT2 MVI   THISRECL,0                                                       
         MVI   THISRECL+1,2        LENGTH                                       
*                                                                               
         BRAS  RE,TSARHIGH         HAVE THIS COUNT ELEMENT?                     
         BNE   VPUTCNT3            NO, ADD ONE                                  
*                                                                               
         LA    R4,MYDATA           REPLACE THE DATA                             
         MVC   0(2,R4),TOTLEVS                                                  
         BRAS  RE,TSARWRT          WRITE RECORD BACK                            
         B     XIT                                                              
*                                                                               
VPUTCNT3 XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         AHI   R3,1                ADD 1 FOR NPCNTCDQ                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   MYKEY(0),WORK       SAVED KEY (FROM TSARHIGH)                    
         MVC   MYDATA(2),TOTLEVS   LEVEL                                        
         BRAS  RE,TSARADD          ADD THE RECORD                               
         B     XIT                                                              
*                                                                               
VPUTCBLC DS    0H                  PUT/ADD TO A CABLE SYSTEM COUNT ELEM         
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPCBLCDQ      REC TYPE (13)                                
         CLI   SBBUYCH,SBBCPRD     IS PRD ONLY CHANGE?                          
         BE    *+10                 YES                                         
         XC    PRDTAB,PRDTAB        NO - RESET PRD/STA TAB                      
*&&DO                                                                           
         B     VPUTCBLT            NOP PRNTBL  VPUTCBLT                         
*        CLI   SBBEST,X'3C'                                                     
*        BNE   VPUTCBLT                                                         
*        CLC   SBBSTA(2),=X'4309'                                               
*        BNE   VPUTCBLT                                                         
*        CLI   SBBSTA+2,X'08'      NETWORK CTV                                  
*        BNE   VPUTCBLT                                                         
*        CLI   SBBPRD,X'01'                                                     
*        BNE   VPUTCBLT                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'SBBUYCH',SBBUYCH,C'DUMP',1,=C'1D',   X        
               (C'P',SBPRINT)                                                   
         GOTO1 =V(PRNTBL),DMCB,=C'SBBPRD',SBBPRD,C'DUMP',1,=C'1D',     X        
               (C'P',SBPRINT)                                                   
         GOTO1 =V(PRNTBL),DMCB,=C'SBBEST',SBBEST,C'DUMP',1,=C'1D',     X        
               (C'P',SBPRINT)                                                   
         GOTO1 =V(PRNTBL),DMCB,=C'PRDTAB',PRDTAB,C'DUMP',256,=C'1D',   X        
               (C'P',SBPRINT)                                                   
         GOTO1 =V(PRNTBL),DMCB,=C'BUYREC',SBAIO1,C'DUMP',13,=C'1D',    X        
               (C'P',SBPRINT)                                                   
VPUTCBLT DS    0H                                                               
*&&                                                                             
*                                                                               
         TM    ROWIND2,ROWICLT                                                  
         BZ    *+10                                                             
         MVC   1(3,R4),SBCLT       CLIENT                                       
*                                                                               
         TM    ROWIND2,ROWIPRD                                                  
         BZ    VPUTCBL1                                                         
         LLC   R1,SBBPRD                                                        
*        CLC   SBQPRD,=C'POL'                                                   
*        BE    VPUTCBL0            IF REQ IS POL, USE POL PRD                   
*        CLI   SBBPRD,X'FE'        IF POL OR UNAL, USE MASPRD, SINCE            
*        BE    *+12                 NOT BROKEN OUT BY SPOTBUY YET               
*        CLI   SBBPRD,X'FF'                                                     
*        BNE   *+8                                                              
*        IC    R1,SBBMPRD                                                       
VPUTCBL0 STC   R1,4(R4)            PRODUCT                                      
*                                                                               
VPUTCBL1 TM    ROWIND2,ROWIEST                                                  
         BZ    VPUTCBLA                                                         
         CLC   SBQEST,SBQESTND     SINGLE EST REQ                               
         BE    *+12                 NO                                          
         CLI   SBQSEPES,C'Y'       ESTIMATES SEPERATE?                          
         BNE   VPUTCBLA             NO                                          
         MVC   5(1,R4),SBBEST      ESTIMATE                                     
*                                                                               
VPUTCBLA LA    R1,SBAGYREC                                                      
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'      TEST CANADIAN                                
         BNE   VPUTCBLB                                                         
         MVC   8(L'SBNETWK,R4),SBNETWK                                          
         B     VPUTCBLD                                                         
VPUTCBLB MVC   6(2,R4),SBBMKT                                                   
         MVC   8(L'SBCBLNET,R4),SBCBLNET                                        
         DROP  R1                                                               
*                                                                               
VPUTCBLD MVC   WORK(L'MYKEY),MYREC SAVE REC BEFORE WE READ IT                   
         BRAS  RE,TSARHIGH                                                      
         BNE   VPUTCBL2                                                         
         LLC   RF,SBBPRD           SEE IF WE'VE SEEN THIS PRD...                
         LA    RF,PRDTAB(RF)       ...FOR THIS STATION                          
         CLI   0(RF),0                                                          
         BNE   XIT                  YES                                         
         MVC   0(1,RF),SBBPRD       NO - SET YES FOR NEXT TIME                  
*                                                                               
         LA    R4,MYDATA                                                        
         LLC   R1,0(R4)            INCREMENT SYSTEM COUNTER                     
         LA    R1,1(R1)                                                         
         STC   R1,0(R4)                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,1        LENGTH                                       
         BRAS  RE,TSARWRT          WRITE RECORD BACK                            
         B     XIT                                                              
*                                                                               
VPUTCBL2 DS    0H                  ELEM DOESN'T EXIST - ADD ONE                 
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         MVC   MYKEY,WORK          SAVED KEY                                    
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,1        LENGTH                                       
         MVI   MYDATA,1            SYSTEM COUNTER                               
*                                                                               
         LLC   RF,SBBPRD           SET WE'VE SEEN THIS PRD FOR THIS STA         
         LA    RF,PRDTAB(RF)                                                    
         MVC   0(1,RF),SBBPRD                                                   
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
* P1 = COLUMN                                                                   
* P2 = DETAIL VALUE                                                             
VPUTTOT  DS    0H                  PUT/ADD TO A TOTAL COUNTER                   
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPTOTCDQ      TYPE (14)                                    
         MVC   1(1,R4),0(R1)       RIGHT COLUMN  0(R1)=COLUMN LABEL             
         BRAS  RE,TSARHIGH                                                      
         BNE   VPUTTOT4                                                         
*                                                                               
         LA    R0,16               INC ALL ACCUMS                               
         LA    R4,MYDATA                                                        
         L     R2,4(R1)            R1=A(DETAIL BEING PUT OUT)                   
VPUTTOT2 L     RE,0(R4)                                                         
         AR    RE,R2                                                            
         ST    RE,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,VPUTTOT2                                                      
*                                                                               
         MVI   THISRECL,0          DON'T SCREW UP LENGTH                        
         MVI   THISRECL+1,64       LENGTH                                       
         BRAS  RE,TSARWRT                                                       
         B     XIT                                                              
*                                                                               
VPUTTOT4 XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R0,16                                                            
         LA    R4,MYDATA                                                        
         L     R2,4(R1)            4(R1)=DETAIL VALUE                           
*                                                                               
         ST    R2,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,*-8                                                           
*                                                                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPTOTCDQ                                                   
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,64       LENGTH                                       
         MVC   1(1,R4),0(R1)       0(R1)=COLUMN LABEL                           
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
VPUTGEN  DS    0H                  PUT A GENERALIZED ELEMENT                    
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPGENCDQ      TYPE (11)                                    
         MVC   1(3,R4),0(R1)                                                    
         LLC   RE,3(R1)                                                         
         MVI   THISRECL,0                                                       
         STC   RE,THISRECL+1       R1 POINTS TO AREA CONTAINING:                
         BCTR  RE,0                +0 TYPE (1)                                  
         EX    RE,*+8              +1 SEQ NUM (2)                               
         B     *+10                +3 LENGTH OF DATA (1)                        
         MVC   MYDATA(0),4(R1)     +4 THE DATA                                  
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
*                                                                               
VGETCGRN DS    0H                  GET CLIENT GROUP                             
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPCGRCDQ                                                   
         MVC   1(L'SBBCGR,R4),SBBCGR                                            
         BRAS  RE,TSARHIGH                                                      
         BNE   VGETCGR2                                                         
         LTR   R1,R1               R1 = A(NAME FIELD)                           
         BZ    EQXIT                                                            
         MVC   0(24,R1),MYDATA                                                  
         B     EQXIT                                                            
VGETCGR2 LTR   R1,R1                                                            
         BZ    NEXIT                                                            
         MVC   0(24,R1),=CL24'** UNKNOWN **'                                    
         B     NEXIT                                                            
*                                                                               
VGETCLTN DS    0H                  GET CLIENT                                   
         MVC   SBCLTNM,BLANKS      CLEAR CLIENT NAME FIELD                      
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPCLTCDQ                                                   
         MVC   1(1,R4),SBMED       MEDIA                                        
         MVC   2(L'SBCLT,R4),SBCLT                                              
         MVC   WORK,MYKEY                                                       
         BRAS  RE,TSARHIGH         READ HI...HIT END OF FILE?                   
         BNE   XIT                 YES                                          
         CLC   MYKEY,WORK          DID WE FIND AN EXACT RECORD MATCH?           
         BNE   XIT                 NO, NO CLIENT                                
         MVC   SBCLTNM,MYDATA                                                   
         B     XIT                                                              
*                                                                               
VGETESTN MVC   SBESTNM,BLANKS      GET ESTIMATE                                 
         XC    SBUE1FLD,SBUE1FLD                                                
         XC    SBUE2FLD,SBUE2FLD                                                
         CLI   SBBEST,0                                                         
         BNE   GE01                                                             
         MVC   SBESTNM(7),=C'*EST 0*'                                           
         XC    SBESTDEM,SBESTDEM                                                
         XC    SBESTDM2,SBESTDM2                                                
         XC    SBESTSTP,SBESTSTP                                                
         XC    SBESTNDP,SBESTNDP                                                
         XC    ECREATE,ECREATE                                                  
         MVI   ESTETYPE,0                                                       
         MVI   ESTRATE,0                                                        
         MVI   ESTBKTYP,0                                                       
         CR    RE,RE                                                            
         B     XIT                                                              
*                                                                               
LGETESTN NTR1  RETURN TO CALLER                                                 
*                                                                               
GE01     XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPESTCDQ                                                   
         MVC   1(L'SBMED,R4),SBMED                                              
         CLI   SBQMED,C'*'                                                      
         BE    GE03                                                             
         MVC   1(L'SBQMED,R4),SBQMED                                            
*                                                                               
GE03     MVC   2(L'SBCLT,R4),SBCLT                                              
         MVC   5(L'SBBPRD,R4),SBBPRD                                            
         CLI   5(R4),X'FE'         UNALLOCATED?                                 
         BNE   *+8                                                              
         MVI   5(R4),X'FF'          YES - USE POL                               
         MVC   6(L'SBBEST,R4),SBBEST                                            
         BRAS  RE,TSARHIGH                                                      
         BNE   XIT                 REC NOT THERE, CC SET TO NOT EQU             
*                                                                               
         LA    R4,MYDATA                                                        
         MVC   SBESTDEM,0(R4)      SECOND DEMO                                  
         MVC   SBESTSTP,6(R4)      START DATE                                   
         MVC   SBESTNDP,8(R4)      END DATE                                     
         MVC   ECREATE,10(R4)      EST CREATION DATE                            
         MVC   ESTETYPE,12(R4)     EST ETYPE                                    
         MVC   ESTRATE,13(R4)      EST RATE                                     
         MVC   ESTBKTYP,14(R4)                                                  
GE04     LA    R1,15(R4)           ELEMENTS START HERE                          
*                                                                               
GE05     CLI   0(R1),0             END OF RECORD?                               
         BE    GEXIT               YES                                          
         LA    R3,SBESTNM          ESTIMATE DESCRIPTION                         
         CLI   0(R1),X'10'         HAVE ESTIMATE DESCRIPTION?                   
         BE    GE10                YES                                          
         LA    R3,SBUE1FLD         ESTIMATE USER FIELD 1                        
         CLI   0(R1),X'20'         HAVE ESTIMATE USER FIELD 1?                  
         BE    GE10                YES                                          
         LA    R3,SBUE2FLD         ESTIMATE USER FIELD 2                        
         CLI   0(R1),X'30'         HAVE ESTIMATE USER FIELD 2?                  
         BE    GE10                YES                                          
         LA    R3,SBESTDM2         TARGET3/4 FIELDS                             
         CLI   0(R1),X'40'         HAVE TARGET3/4 ELEMENT?                      
         BNE   GEXIT               NO - JUST EXIT                               
*                                                                               
GE10     LLC   R2,1(R1)            ELEMENT LENGTH                               
         SHI   R2,3                2 BYTES OVERHEAD + 1 FOR EX                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R1)       ** EXECUTED **                               
         LA    R1,3(R1,R2)         BUMP TO NEXT ELEMENT                         
         B     GE05                LOOK FOR MORE ELEMENTS                       
*                                                                               
GEXIT    CR    RE,RE                                                            
         B     XIT                 RETURN CC EQU                                
*                                                                               
VGETMKTN DS    0H                  GET MARKET                                   
         MVC   SBMKTNM,BLANKS                                                   
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPMKTCDQ                                                   
         MVC   1(L'SBMED,R4),SBMED                                              
         CLI   SBQMED,C'*'         FOR MEDIA * USE SBMED                        
         BE    *+10                                                             
         MVC   1(L'SBQMED,R4),SBQMED                                            
         MVC   2(L'SBBMKT,R4),SBBMKT                                            
         BRAS  RE,TSARHIGH                                                      
         BNE   XIT                                                              
*                                                                               
         LA    R4,MYDATA                                                        
         MVC   SBMKTNM,0(R4)       NAME                                         
         MVC   SBBRSM0,24(R4)      NSI MARKET NUM                               
         MVC   SBBRSM1,26(R4)      ARB MARKET NUM                               
         MVC   SBMKTWGT,28(R4)     MARKET WEIGHT                                
         B     XIT                                                              
*                                                                               
VGETMGRN DS    0H                  GET MARKET GROUP                             
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPMGRCDQ                                                   
         MVC   1(L'SBMED,R4),SBMED                                              
*                                                                               
         LA    R2,SBAGYREC                                                      
         USING AGYHDRD,R2                                                       
         CLI   AGYPROF+7,C'C'      CANADIAN                                     
         BNE   VGMG01              NOPE                                         
         DROP  R2                                                               
         CLI   SBQMED,C'C'         MEDIA C?                                     
         BNE   VGMG01              NOPE                                         
         MVC   1(1,R4),SBQMED      MEDIA                                        
*                                                                               
VGMG01   MVC   2(L'SBBMGR,R4),SBBMGR                                            
         OC    SBPGRPEX(2),SBPGRPEX   TEST PRODUCT GROUP EXCEPTION              
         BZ    VGMG20                                                           
*                                                                               
         MVC   HALF,SBBPGR         PRODUCT GROUP                                
         OC    HALF,SBPG1MSK       PRODUCT GROUP LEVEL 1 MASK                   
         LA    R0,SBEXMAX          MAX # MGRP TABLES = 7                        
         LA    RE,SBPGRPEX         PRODUCT GROUP EXCEPTION TABLE                
*                                                                               
VGMG05   OC    0(2,RE),0(RE)       END OF TABLE ENTRIES?                        
         BZ    VGMG20              YES                                          
         MVC   FULL(2),0(RE)       MOVE PGROUP EXCEPTION TO FULL                
         OC    FULL(2),SBPG1MSK    PRODUCT GROUP LEVEL 1 MASK                   
         CLC   HALF,FULL           MATCH THIS PRODUCT GROUP?                    
         BE    VGMG10              YES                                          
         LA    RE,2(RE)            BUMP                                         
         BCT   R0,VGMG05                                                        
*                                                                               
VGMG10   MVC   4(2,R4),HALF                                                     
*                                                                               
VGMG20   BRAS  RE,TSARHIGH         DID WE FIND THE RECORD?                      
         BNE   VGMG25              NO                                           
*                                                                               
         LA    R4,MYDATA                                                        
         LTR   R1,R1               HAVE A(NAME FIELD)?                          
         BZ    EQXIT               NO, JUST EXIT WITH CC EQU                    
         MVC   0(24,R1),0(R4)                                                   
         B     EQXIT                                                            
*                                                                               
VGMG25   LTR   R1,R1               HAVE A(NAME FIELD)?                          
         BZ    EQXIT               NO, JUST EXIT WITH CC EQU                    
         MVC   0(24,R1),=CL24'** UNKNOWN **'                                    
         B     EQXIT                                                            
*                                                                               
VGETPGRN DS    0H                  GET PRODUCT GROUP                            
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPPGRCDQ                                                   
         MVC   1(L'SBMED,R4),SBMED                                              
         CLI   SBQMED,C'*'                                                      
         BE    *+10                                                             
         MVC   1(L'SBQMED,R4),SBQMED                                            
         MVC   2(L'SBCLT,R4),SBCLT                                              
         MVC   5(L'SBBPGR,R4),SBBPGR                                            
         BRAS  RE,TSARHIGH                                                      
         BNE   VGETPGR2                                                         
*                                                                               
         LTR   R1,R1               HAVE A(NAME FIELD)?                          
         BZ    EQXIT               NO, EXIT CC EQU                              
         MVC   0(24,R1),MYDATA                                                  
         LA    RE,SBPGR1NM         RE = SBPGR1NM                                
         LA    RF,SBPGR1BK         RF = SBPGR1BK                                
         CR    RE,R1               GETTING PGR1?                                
         BE    *+8                 YES                                          
         LA    RF,SBPGR2BK         NO, SET RF TO SBPGR1BK                       
         MVC   0(12,RF),MYDATA+24  SET SBPGR1BK/SBPGR2BK                        
         B     EQXIT                                                            
VGETPGR2 LTR   R1,R1               HAVE A(NAME FIELD)?                          
         BZ    NEXIT               NO, EXIT CC NOT EQU                          
         MVC   0(24,R1),=CL24'** UNKNOWN **'                                    
         B     NEXIT                                                            
*                                                                               
VGETSGRN DS    0H                  GET STATION GROUP                            
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPSGRCDQ                                                   
         MVC   1(L'SBBSGR,R4),SBBSGR                                            
         BRAS  RE,TSARHIGH                                                      
         BNE   VGETSGR2                                                         
*                                                                               
         LTR   R1,R1               HAVE A(NAME FIELD)?                          
         BZ    EQXIT               NO, EXIT WITH CC EQU                         
         MVC   0(24,R1),MYDATA                                                  
         B     EQXIT                                                            
VGETSGR2 LTR   R1,R1               HAVE A(NAME FIELD)?                          
         BZ    NEXIT               NO, EXIT WITH CC NOT EQU                     
         MVC   0(24,R1),=CL24'** UNKNOWN **'                                    
         B     NEXIT                                                            
*                                                                               
VGETREPN DS    0H                  GET REP NAME                                 
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPREPCDQ                                                   
         MVC   1(L'SBMED,R4),SBMED                                              
         CLI   SBQMED,C'*'         FOR MEDIA * USE SBMED                        
         BE    *+10                                                             
         MVC   1(L'SBQMED,R4),SBQMED                                            
         MVC   2(L'SBREP,R4),SBREP                                              
         BRAS  RE,TSARHIGH                                                      
         BNE   VGETREP2                                                         
*                                                                               
         MVC   SBREPNM,MYDATA                                                   
         B     EQXIT                                                            
VGETREP2 MVC   SBREPNM,=CL24'** UNKNOWN **'                                     
         B     NEXIT                                                            
*                                                                               
VGETCSFL DS    0H                  GET CHILD SPOT FLIGHT INFO                   
         XC    SBADATE,SBADATE                                                  
         XC    SBNDATES,SBNDATES                                                
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPFLTCDQ                                                   
         MVC   1(L'SBPRD,R4),SBPRD                                              
         BRAS  RE,TSARHIGH                                                      
         BNE   NEXIT                                                            
*                                                                               
         LA    R4,MYDATA                                                        
         MVC   SBNDATES+3(1),0(R4) NUMBER OF DATES                              
         LA    R4,1(R4)            A(FIRST FLIGHT DATE)                         
         ST    R4,SBADATE          RETURN A(FLIGHT DATES)                       
         B     EQXIT                                                            
*                                                                               
VGETCBLC DS    0H                  GET CABLE SYSTEM COUNT                       
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPCBLCDQ                                                   
         TM    ROWIND2,ROWICLT     CLT IS ROW?                                  
         BZ    *+10                NO                                           
         MVC   1(3,R4),SBCLT                                                    
         TM    ROWIND2,ROWIPRD     PRD IS ROW?                                  
         BZ    *+10                NO                                           
         MVC   4(1,R4),SBBPRD                                                   
*                                                                               
         TM    ROWIND2,ROWIEST     EST IS ROW?                                  
         BZ    VGETCBL2            NO                                           
         CLC   SBQEST,SBQESTND     SINGLE EST REQ?                              
         BE    *+12                YES                                          
         CLI   SBQSEPES,C'Y'       ESTIMATES SEPERATE?                          
         BNE   VGETCBL2            NO                                           
         MVC   5(1,R4),SBBEST                                                   
*                                                                               
VGETCBL2 LA    R1,SBAGYREC                                                      
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'      TEST CANADIAN                                
         BNE   VGETCBL3                                                         
         MVC   8(L'SBNETWK,R4),SBNETWK                                          
         B     VGETCBL4                                                         
VGETCBL3 MVC   6(2,R4),SBBMKT                                                   
         MVC   8(L'SBCBLNET,R4),SBCBLNET                                        
         DROP  R1                                                               
*                                                                               
VGETCBL4 BRAS  RE,TSARHIGH                                                      
         BNE   BADCBCT                                                          
         MVC   BYTE,MYDATA         GET SYSTEM COUNT                             
         B     EQXIT                                                            
BADCBCT  B     NEXIT                                                            
         SPACE 2                                                                
*                                                                               
* P1 = COLUMN                                                                   
* P2 = LEVEL                                                                    
* RETURNS P1 = TOTAL                                                            
VGETTOT  DS    0H                                                               
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPTOTCDQ                                                   
         MVC   1(1,R4),0(R1)       COLUMN LABEL                                 
         BRAS  RE,TSARHIGH                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   RE,4(R1)            7(R1)=LEVEL                                  
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    R4,MYDATA                                                        
         LA    RE,3(RE,R4)                                                      
         MVC   0(4,R1),0(RE)                                                    
         XC    0(4,RE),0(RE)       CLEAR ACCUM FOR THIS LEVEL                   
         B     EQXIT                                                            
*                                                                               
VGETGEN  DS    0H                  GET A GENERALIZED ELEMENT                    
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPGENCDQ                                                   
         MVC   1(3,R4),0(R1)       TYPE AND SEQUENCE NUM                        
         BRAS  RE,TSARHIGH                                                      
         BNE   NEXIT                                                            
*                                                                               
         LLC   RE,MYKEY-1          LENGTH                                       
         SHI   RE,(L'MYKEY+2)      KEY LENGTH + 2 BYTE LENGTH                   
         STC   RE,3(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R1),MYDATA                                                   
         B     EQXIT                                                            
*                                                                               
* SET THE DAYPARTS FOR DRIVER                                                   
* INPUT  : R1 = A(DAYPART CODE)                                                 
* OUTPUT : SBDPTGRP AND SBDPT SET                                               
*                                                                               
VSETDPT  LR    R3,R1                                                            
         LA    R4,SBDPTTAB                                                      
         LA    RE,2                                                             
*                                                                               
DPT01    LA    R0,36               FIND DAYPART CODE IN TABLE                   
         LR    R5,R4                                                            
         SR    R1,R1                                                            
*                                                                               
DPT02    CLC   0(1,R3),0(R5)                                                    
         BE    DPT04                                                            
         LA    R1,1(R1)                                                         
         LA    R5,5(R5)                                                         
         BCT   R0,DPT02                                                         
         BCT   RE,*+6                                                           
         DC    H'0'                                                             
         LA    R3,=C'Z'            DAYPART NOT FOUND - TRY DPT=Z                
         B     DPT01                                                            
*                                                                               
DPT04    MVC   SBDPT+1(3),2(R5)    FOUND                                        
         STC   R1,SBDPT                                                         
         MVC   BYTE,1(R5)                                                       
         XC    SBDPTGRP,SBDPTGRP                                                
         LLC   RE,BYTE                                                          
         SRL   RE,4                                                             
         LTR   RE,RE               TEST ANY SUB-DAYPARTS                        
         BNZ   DPT08               YES                                          
         LR    R5,R4               NO-GET FIRST DAYPART WITH SAME               
         SR    R1,R1                  CODE                                      
         LA    R0,36                                                            
*                                                                               
DPT06    CLC   BYTE,1(R5)                                                       
         BE    DPT07                                                            
         LA    R5,5(R5)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,DPT06                                                         
         DC    H'0'                                                             
*                                                                               
DPT07    STC   R1,SBDPT                                                         
         MVC   SBDPT+1(3),2(R5)                                                 
         B     DPTX                                                             
*                                                                               
DPT08    LA    R0,36               FIND DAYPART GROUP, IF ANY                   
         LR    R5,R4                                                            
*                                                                               
DPT10    DS    0H                                                               
         LLC   RF,1(R5)                                                         
         SRL   RF,4                                                             
         CR    RE,RF                                                            
         BNE   DPT20                                                            
         OC    SBDPTGRP,SBDPTGRP                                                
         BNZ   DPT30                                                            
         MVC   SBDPTGRP+1(3),2(R5)                                              
         MVC   SBDPTGRP(1),1(R5)                                                
         NI    SBDPTGRP,X'F0'                                                   
*                                                                               
DPT20    LA    R5,5(R5)                                                         
         BCT   R0,DPT10                                                         
         XC    SBDPTGRP+1(3),SBDPTGRP+1                                         
         B     DPT40                                                            
*                                                                               
DPT30    CLC   SBDPT+1(3),SBDPTGRP+1                                            
         BNE   DPT40                                                            
         MVI   SBDPT,17                                                         
*                                                                               
DPT40    LA    R0,36                                                            
         SR    R1,R1                                                            
         SR    R3,R3                                                            
         SR    R5,R5                                                            
         SR    RF,RF                                                            
*                                                                               
DPT50    CLC   BYTE,1(R4)                                                       
         BE    DPT52                                                            
         LTR   R3,R3                                                            
         BNZ   DPT60                                                            
         IC    RF,1(R4)                                                         
         SRL   RF,4                                                             
         CR    RE,RF                                                            
         BNE   DPT60                                                            
         LA    R3,1                                                             
         B     DPT60                                                            
*                                                                               
DPT52    CLC   SBDPT+1(3),2(R4)                                                 
         BNE   DPT54                                                            
         CLM   R1,1,SBDPT                                                       
         BNE   *+12                                                             
         LA    R5,2                                                             
         B     DPT60                                                            
DPT54    LTR   R5,R5                                                            
         BNZ   DPT60                                                            
         STC   R1,SBDPT                                                         
         MVC   SBDPT+1(3),2(R4)                                                 
         LA    R5,1                                                             
*                                                                               
DPT60    LA    R1,1(R1)                                                         
         LA    R4,5(R4)                                                         
         BCT   R0,DPT50                                                         
*        LTR   R5,R5                                                            
*        BZ    DPTX                                                             
         CHI   R5,1                                                             
         BNE   DPTX                                                             
         LTR   R3,R3                                                            
         BNZ   DPTX                                                             
         XC    SBDPTGRP+1(3),SBDPTGRP+1                                         
*                                                                               
DPTX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                  ROUTINE TO GET COMMERCIAL DETAILS                  *         
*                                                                     *         
* INPUT  : R1=0 GET COMMERCIAL CODE, NAME, CLASS AND CLT NUM          *         
*          R1=3 GET COMMERCIAL CODE, NAME2, CLASS AND CLT NUM         *         
*          R1=4 GET COMMERCIAL CODE, NAME3, CLASS AND CLT NUM         *         
*          R1=1 GET COMMERCIAL CLASS NAME IN ADDITION TO ABOVE        *         
*          R1=2 GET CML CLASS NAMES FOR SPLIT CLASSES (HOME DEPOT)    *         
*          R1=5 GET COMMERCIAL CODE FROM AD-ID ONLY                   *         
*          R1=X'FF' CLEAR THE CML TSAR BUFFER                         *         
*          SBCMLSQ  = COMMERCIAL SEQUENCE NUMBER                      *         
*                                                                     *         
* OUTPUT : CMLCODE  = COMMERCIAL CODE                                 *         
*          CMLNAME  = COMMERCIAL NAME                                 *         
*          CMLNAME2 = COMMERCIAL NAME 2                               *         
*          CMLNAME3 = COMMERCIAL NAME 3                               *         
*          CMLCLAS  = COMMERCIAL CLASS                                *         
*          CMLNUM   = COMMERCIAL CLT NUMBER                           *         
*          CMLCLSNM = COMMERCIAL CLASS NAME                           *         
*          CMLCNUM  = NUMBER OF COMMERCIAL CLASSES                    *         
*          CMLBCODE = COMMERCIAL BASIC CODE (SAG/AFTRA)               *         
***********************************************************************         
VGETCML  OC    ATSARBF2,ATSARBF2   BUFFER PRESENT?                              
         BNZ   *+8                 YES                                          
         BRAS  RE,INITSAR          NO - INITIALIZE THE TSAR BUFFER              
         STC   R1,BYTE             COMMERCIAL SWITCH                            
         CLI   BYTE,X'FF'          CLEAR THE COMMERCIAL TSAR BUFFER?            
         BNE   *+12                NO                                           
         BRAS  RE,DELCML           YES - RE-INIT THE CML TSAR BUFFER            
         B     XIT                 AND EXIT                                     
*                                                                               
         XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK(1),SBBAGYMD    A/M                                          
         MVC   WORK+1(2),SBBCLT    CLIENT                                       
         OC    SBBCMCLT,SBBCMCLT   TEST MASTER TRAFFIC CLIENT CODE              
         BZ    *+10                NO                                           
         MVC   WORK+1(2),SBBCMCLT  YES-USE THAT                                 
         MVC   WORK+3(2),SBCMLSQ   SEQ NUMBER                                   
         TM    DTAIND11,DIADID2    USE SBCMLSQ2 FOR AD-IDCD2 KEYWORD?           
         BZ    *+10                NO                                           
         MVC   WORK+3(2),SBCMLSQ2  SEQ NUMBER2                                  
         TM    ROWIND2,ROWICLAS    CML CLASS NAMES NEEDED?                      
         BZ    *+10                NO                                           
         MVC   WORK+5(3),SBPRD     YES - CLASS IS PRD SPECIFIC                  
         CLC   CMLOPT,WORK         SAME LOOKUP AS LAST TIME?                    
         BE    XIT                 YES - EXIT CML DATA ALREADY SET              
*                                                                               
         MVC   CMLNUM,BLANKS       SPACE PAD                                    
         MVC   CMLNUM(7),UNKNOWN   CML NUM UNKNOWN                              
         MVC   CMLCODE,CMLNUM      CML CODE UNKNOWN                             
         MVC   CMLBCODE,CMLNUM     CML BASIC CODE UNKNOWN                       
         MVC   CMLNAME,CMLNUM      CML NAME UNKNOWN                             
         MVC   CMLNAME2,CMLNUM     CML NAME 2 UNKNOWN                           
         MVC   CMLNAME3,CMLNUM     CML NAME 3 UNKNOWN                           
         MVC   CMLCLAS,=C'ZZZZ'    CML CLASS = ZZZZ                             
         MVC   CMLCLSNM,BLANKS     SPACE PAD                                    
         MVC   CMLCLSNM(10),UNASGN CML CLASS NAME UNASSIGNED                    
         MVI   CMLCNUM,1           DEFAULT TO 1 CLASS NAME                      
         LA    RE,SBCMLSQ          CML SEQ NUMBER                               
         TM    DTAIND11,DIADID2    USE SBCMLSQ2 FOR AD-IDCD2 KEYWORD?           
         BZ    *+8                 NO                                           
         LA    RE,SBCMLSQ2         CML SEQ NUMBER2                              
         OC    0(2,RE),0(RE)       HAVE A CML SEQ NUMBER?                       
         BNZ   GETCML0A            YES                                          
         MVC   CMLOPT(5),WORK      NO - SAVE OPTIMIZATION                       
         TM    ROWIND2,ROWICLAS    CML CLASS NAMES NEEDED?                      
         BZ    XIT                 NO - EXIT                                    
         MVC   CMLOPTPR,SBPRD      YES - SET PRD OPTIMIZATION                   
         B     XIT                 AND EXIT                                     
*                                                                               
GETCML0A MVC   CMLNAME,BLANKS      INITIALIZE TO NOT SET UP IN TRAFFIC          
         MVC   CMLNAME(4),=C'*NT*' CML NAME = *NT*                              
         MVC   CMLCODE,CMLNAME     CML CODE = *NT*                              
*                                                                               
GETCML0B XC    ELEM2,ELEM2         CLEAR KEY+DATA                               
         MVI   ELEM2+2,NPCMLCDQ    COMMERCIAL REC                               
         MVC   ELEM2+3(1),SBBAGYMD A/M                                          
         MVC   ELEM2+4(2),SBBCLT   CLIENT                                       
         OC    SBBCMCLT,SBBCMCLT   TEST MASTER TRAFFIC CLIENT CODE              
         BZ    *+10                NO                                           
         MVC   ELEM2+4(2),SBBCMCLT YES-USE THAT                                 
         MVC   ELEM2+6(2),SBCMLSQ  SEQ NUMBER                                   
         TM    DTAIND11,DIADID2    USE SBCMLSQ2 FOR AD-IDCD2 KEYWORD?           
         BZ    *+10                NO                                           
         MVC   ELEM2+6(2),SBCMLSQ2 SEQ NUMBER 2                                 
         OI    TSARFLAG,TSARCMML   INDICATE READING CML RECORDS                 
         BRAS  RE,TSARHIGH         HAVE A MATCH?                                
         BE    GETCML0C            YES                                          
*                                                                               
         BRAS  RE,READCML          READ AND BUFFERED THE CML RECORD?            
         BE    GETCML0B            YES                                          
         MVC   CMLOPT(5),WORK+1    NO - REC NOT FOUND SAVE OPTIMIZATION         
         TM    ROWIND2,ROWICLAS    CML CLASS NAMES NEEDED?                      
         BZ    XIT                 NO - EXIT                                    
         MVC   CMLOPTPR,SBPRD      YES - SET PRD OPTIMIZATION                   
         B     XIT                 EXIT                                         
*                                                                               
GETCML0C MVC   CMLOPT(5),ELEM2+3   CML OPTIMIZATION                             
         TM    ROWIND2,ROWICLAS    CML CLASS NAMES NEEDED?                      
         BZ    *+10                NO                                           
         MVC   CMLOPTPR,SBPRD      YES - SET PRD OPTIMIZATION                   
         MVC   CMLNAME2,BLANKS     CML NAME 2 NOW BLANKS - NOT UNKNOWN          
         MVC   CMLNAME3,BLANKS     CML NAME 3 NOW BLANKS - NOT UNKNOWN          
         MVC   CMLNUM,BLANKS       CML NUMBER NOW BLANKS - NOT UNKNOWN          
         MVC   CMLBCODE,BLANKS     BASIC CODE NOW BLANKS - NOT UNKNOWN          
         LA    RE,ELEM2+TSA2KEYL+2 RECORD WAS BUFFERED HERE                     
         XC    MYSVR1,MYSVR1       A(ELEMENT X'02')                             
*                                                                               
GETCML00 CLI   0(RE),0             END OF RECORD?                               
         BE    GETCML60            YES                                          
*                                                                               
         CLI   0(RE),X'01'         COMMERCIAL CODE ELEMENT?                     
         BE    GETCML10            YES                                          
*                                                                               
         CLI   0(RE),X'02'         COMMERCIAL CLASS ELEMENT?                    
         BNE   *+8                 NO                                           
         ST    RE,MYSVR1           YES - SAVE A(ELEMENT)                        
*                                                                               
         CLI   0(RE),X'03'         COMMERCIAL NAME 1 ELEMENT?                   
         BE    GETCML20            YES                                          
*                                                                               
         CLI   0(RE),X'04'         CLIENT COMMERCIAL NUMBER ELEMENT?            
         BE    GETCML30            YES                                          
*                                                                               
         CLI   0(RE),X'05'         COMMERCIAL CLASS CAMPAIGN CATEGORY           
         BE    GETCML40            YES                                          
*                                                                               
         CLI   0(RE),X'06'         COMMERCIAL NAME 2?                           
         BE    GETCML21            YES                                          
*                                                                               
         CLI   0(RE),X'07'         COMMERCIAL NAME 3?                           
         BE    GETCML22            YES                                          
*                                                                               
         CLI   0(RE),X'08'         BASIC CODE (SAG/AFTRA)?                      
         BE    GETCML50            YES                                          
*                                                                               
GETCML05 LLC   R0,1(RE)            ELEMENT LENGTH                               
         AR    RE,R0               BUMP TO NEXT ELEMENT                         
         B     GETCML00                                                         
*                                                                               
GETCML10 LLC   RF,1(RE)            ELEMENT LENGTH                               
         SHI   RF,3                MINUS OVERHEAD +1 FOR EX                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CMLCODE(0),2(RE)    COMMERCIAL CODE                              
         B     GETCML05                                                         
*                                                                               
GETCML20 MVC   CMLNAME,2(RE)       COMMERCIAL NAME                              
         B     GETCML05                                                         
*                                                                               
GETCML21 MVC   CMLNAME2,2(RE)      COMMERCIAL NAME 2                            
         B     GETCML05                                                         
*                                                                               
GETCML22 MVC   CMLNAME3,2(RE)      COMMERCIAL NAME 3                            
         B     GETCML05                                                         
*                                                                               
GETCML30 MVC   CMLNUM,2(RE)        CLIENT COMMERCIAL NUMBER                     
         B     GETCML05                                                         
*                                                                               
GETCML40 MVC   CMLCLAS,2(RE)       COMMERCIAL CLASS                             
         B     GETCML05                                                         
*                                                                               
GETCML50 MVC   CMLBCODE,2(RE)      BASIC CODE (SAG/AFTRA)                       
         B     GETCML05                                                         
*                                                                               
GETCML60 TM    ROWIND2,ROWICLAS    CML CLASS NAMES NEEDED?                      
         BZ    XIT                 NO - EXIT                                    
*                                                                               
         LA    R6,CMLCLSNM         BUILD CLASS ENTRY HERE                       
         ICM   RE,15,MYSVR1        HAVE A(X'02') ELEMENT?                       
         BZ    GETCML65            NO - TRY AND GET IT FROM CMLCLAS             
*                                                                               
         LLC   R1,1(RE)            ELEMENT LENGTH                               
         SHI   R1,3                ELEM OVERHEAD -1 FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CLSLIST,2(RE)       COMMERCIAL CLASS LIST                        
         AHI   R1,1                ADD 1 BACK TO GET CML CLASSES * 4            
         SRL   R1,2                DIVIDE BY 4 TO GET NUM OF CLASSES            
         STC   R1,CMLCNUM          NUMBER OF CLASSES                            
*                                                                               
         LR    R0,R1               24/NUMBER OF CLASSES                         
         LA    R3,24                                                            
         SR    R2,R2                                                            
         DR    R2,R0               LENGTH OF EACH NAME = R3                     
         BCTR  R3,0                -1 FOR EX                                    
         LA    R5,CLSLIST          LIST OF CLASS SPLITS                         
         B     GETCML70                                                         
*                                                                               
GETCML65 LA    R5,CMLCLAS          COMMERCIAL CLASS                             
         LA    R1,1                ONE TIME FOR BCT LOOP                        
         LA    R3,23               LENGTH 24                                    
         CLC   CMLCLAS,=C'ZZZZ'    UNKNOWN CLASS?                               
         BE    XIT                 YES                                          
*                                                                               
GETCML70 XC    ELEM2,ELEM2         SEARCH TSAR BUFF FOR CML CLASS REC           
         LA    R4,ELEM2+2          BUILD CML KEY HERE                           
         MVI   0(R4),NPCCLCDQ      CML RECORD TYPE                              
         MVC   5(2,R4),SBBCLT      CLIENT                                       
         MVC   7(3,R4),SBPRD       PRODUCT                                      
*                                                                               
         MVC   1(4,R4),0(R5)       CLASS                                        
         OI    TSARFLAG,TSARCMML   INDICATE READING CML RECORDS                 
         BRAS  RE,TSARHIGH         ALREADY BUFFERED CML CLASS RECORD?           
         BE    GETCML75            YES                                          
         MVC   0(TSA2KEYL,R4),WORK NO - RESTORE KEY                             
         BRAS  RE,ADDCLASS         ADDED COMMERCIAL CLASS ENTRY?                
         BNE   GETCML80            NO                                           
*                                                                               
GETCML75 EX    R3,*+8              ** EXTRACT CLASS NAME **                     
         B     *+10                                                             
         MVC   0(0,R6),TSA2KEYL(R4)                                             
*                                                                               
GETCML80 LA    R6,1(R3,R6)         BUMP TO NEXT SLOT                            
         AHI   R5,4                BUMP TO NEXT CLASS                           
         BCT   R1,GETCML70         LOOP BACK AND PROCESS SPLIT CLASS            
         B     XIT                 EXIT                                         
*                                                                               
* ROUTINE TO GET THE MARKET WEIGHT                                              
*                                                                               
VGETMKTW CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    GMX                                                              
         MVC   MWGTDEM,PRMYDEMO    PRIMARY DEMO                                 
         CLI   MWGTDEM+2,0         COMSCORE DEMO?                               
         BNE   *+8                 NO                                           
         MVI   MWGTDEM+2,1         YES - FORCE HOMES                            
         LA    R0,MLNMKTS          YES - FIND MARKET IN MARKET LIST             
         L     R3,AMKTLST                                                       
         USING MKTLSTD,R3                                                       
*                                                                               
GM10     OC    MLMKT,MLMKT                                                      
         BNZ   *+14                                                             
         MVC   MLMKT,SBBMKT        NEW ENTRY                                    
         B     GM12                                                             
         CLC   MLMKT,SBBMKT                                                     
         BE    GM12                                                             
         LA    R3,MKTLSTL(R3)                                                   
         BCT   R0,GM10                                                          
         DC    H'0'                NEED TO EXPAND MARKET LIST                   
*                                                                               
GM12     CLI   SBQMKTWT,C'D'       TEST PRIMARY DEMO MARKET WEIGHTING           
         BE    *+14                YES                                          
         MVC   MLWGT,SBMKTWGT      NO-MAKE SURE MARKET WEIGHT SET IN            
         B     GM20                   MARKET LIST                               
         XC    SBMKTWGT,SBMKTWGT                                                
         OC    MLWGT,MLWGT         TEST DEMO MARKET WEIGHT SET YET              
         BZ    GM14                                                             
         CLC   MLRTGSVC,SBCPROF+3  TEST RATING SETVICE AND PRIMARY DEMO         
         BNE   GM14                ARE THE ONES WE NEED                         
         CLC   MLDEMO,MWGTDEM                                                   
         BE    GM18                                                             
*                                                                               
GM14     MVC   MLRTGSVC,SBCPROF+3  SET RTG SVC AND DEMO                         
         MVC   MLDEMO,MWGTDEM                                                   
******** MVC   MLWGT,=F'1'                                                      
         USING MWTABD,R5                                                        
         L     R5,SBAMWTAB         FIND CORRECT MARKET WEIGHT FROM              
         LA    R0,MWTENTS          MARKET WEIGHT TABLE                          
*                                                                               
GM16     OC    MWTDEM,MWTDEM                                                    
         BZ    GM20                                                             
         CLC   MWTSVC,SBCPROF+3    MATCH RATING SERVICE                         
         BNE   *+14                                                             
         CLC   MWTDEM,MWGTDEM      MATCH PRIMARY DEMO                           
         BE    *+16                                                             
         LA    R5,MWTABL(R5)                                                    
         BCT   R0,GM16                                                          
         B     GM20                                                             
         LA    R1,SBBRSM0          NSI MARKET NUM                               
         CLI   SBCPROF+3,C'0'                                                   
         BE    *+8                                                              
         LA    R1,SBBRSM1          ARB MARKET NUM                               
         LA    R0,MWTNMKT                                                       
         CLC   MWTMKT,0(R1)        FIND THE RATING SERVICE MARKET               
         BE    *+16                                                             
         LA    R5,L'MWTMKENT(R5)                                                
         BCT   R0,*-14                                                          
         B     GM20                                                             
         TM    MWTMKUNV,MWTMKWGT   CHECK WEIGHT HAS BEEN CALCULATED             
         BZ    GM20                                                             
         MVC   MLWGT,MWTMKUNV      SET THE WEIGHT IN MARKET LIST                
         NI    MLWGT,X'FF'-MWTMKWGT                                             
*                                                                               
GM18     MVC   SBMKTWGT,MLWGT      SET THE MARKET WEIGHT                        
*                                                                               
GM20     OC    SBMKTWGT,SBMKTWGT   TEST MARKET HAS ANY WEIGHT                   
         BNZ   GM22                                                             
         TM    SBQDEMOP,SBQDOMWZ   NO-SET DEFAULT WEIGHT TO 1                   
         BO    GM22                                                             
         MVC   SBMKTWGT,=F'1'                                                   
*                                                                               
GM22     LLC   R1,MKTLEV           SET MARKET ENCOUNTERED AT ALL                
         BCTR  R1,0                LEVELS ABOVE MARKET                          
         LTR   R1,R1                                                            
         BNP   GMX                                                              
         SR    RE,RE                                                            
         ICM   RE,3,XFF                                                         
         SR    RF,RF                                                            
         SRDL  RE,1                                                             
         BCT   R1,*-4                                                           
         STCM  RF,12,MLLEVS                                                     
*                                                                               
GMX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* GET CANADIAN SPILL STATIONS                                                   
*                                                                               
         SPACE 1                                                                
VGETXSPL DS    0H                                                               
         LA    R2,XSNSTA           GET STORAGE FOR TABLE                        
         MHI   R2,XSPILLL                                                       
         ICM   R3,15,SBAXSPIL                                                   
         BNZ   XSPL1                                                            
         ST    R2,DMCB+4                                                        
         ST    R2,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   R3,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R3,SBAXSPIL         SET A(CANADIAN SPILL TABLE)                  
*                                                                               
         USING XSPILLD,R3                                                       
XSPL1    LR    RE,R3               CLEAR THE TABLE                              
         LR    RF,R2                                                            
         XCEFL ,                                                                
         LA    R5,XSNSTA+1                                                      
         XC    KEY,KEY             READ CANADIAN SPILL RECORDS                  
         LA    R2,KEY                                                           
         USING XSDFRECD,R2                                                      
         MVC   XSDFKTYP,=X'0D23'                                                
         MVC   XSDFKAGY,AGENCY                                                  
         MVC   XSDFKRSV,SBCPROF+3                                               
         SR    R4,R4                                                            
*                                                                               
XSPL2    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     XSPL6                                                            
*                                                                               
XSPL4    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
XSPL6    CLC   KEY(XSDFKSTA-XSDFKEY),KEYSAVE  TEST END OF FILE                  
         BE    *+14                                                             
         LTR   R4,R4               YES-TEST LOOKING FOR REC WITH CLT            
         BNZ   XSPL12              YES-GET CLIENT 0 RECORD                      
         B     XSPL16              NO-DONE                                      
         LTR   R4,R4               TEST LOOKING FOR RECORD WITH CLIENT          
         BZ    XSPL8                                                            
         CLC   KEY(XSDFKCLT+2-XSDFKEY),KEYSAVE  YES-TEST FOUND IT               
         BE    XSPL14                   YES-OK                                  
         B     XSPL12                   NO-GET CLIENT 0 RECORD                  
*                                                                               
XSPL8    OC    XSDFKCLT,XSDFKCLT   TEST CLIENT IN KEY                           
         BNZ   *+18                                                             
         LA    R4,1                NO-LOOK FOR RECORD WITH CLIENT               
         MVC   SBSTA,XSDFKSTA                                                   
         B     XSPL10                                                           
         CLC   XSDFKCLT,SBBCLT     YES - COMPARE TO THIS CLIENT                 
         BE    XSPL14              EQUAL - OK                                   
         BL    XSPL10                                                           
         MVC   XSDFKCLT(3),XFF     HIGH - SKIP TO NEXT STATION                  
         B     XSPL2                                                            
*                                                                               
XSPL10   MVC   XSDFKCLT,SBBCLT     LOW - SKIP TO THIS CLIENT                    
         MVI   XSDFKCLT+2,0                                                     
         B     XSPL2                                                            
*                                                                               
XSPL12   MVC   KEY,KEYSAVE         GET CLIENT 0 RECORD                          
         MVC   XSDFKSTA,SBSTA                                                   
         XC    XSDFKCLT(3),XSDFKCLT                                             
         GOTO1 HIGH                                                             
         CLC   KEY(XSDFKCLT+2-XSDFKEY),KEYSAVE                                  
         BE    XSPL14                                                           
         DC    H'0'                                                             
*                                                                               
XSPL14   L     R2,SBAIO2           GET THE RECORD                               
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         BAS   RE,XSPL20           ADD SPILL MARKETS TO TABLE                   
         LA    R2,KEY                                                           
         MVC   XSDFKCLT(3),XFF     SKIP TO NEXT STATION                         
         SR    R4,R4                                                            
         B     XSPL2                                                            
*                                                                               
XSPL16   SR    R2,R2               COUNT N'SPILL STATIONS                       
         L     R3,SBAXSPIL                                                      
         LA    R5,XSNSTA                                                        
         OC    0(XSPILLL,R3),0(R3)                                              
         BZ    *+16                                                             
         LA    R2,1(R2)                                                         
         LA    R3,XSPILLL(R3)                                                   
         BCT   R5,*-18                                                          
         LTR   R2,R2               TEST ANY STATIONS                            
         BZ    XSPLX                                                            
         BAS   RE,XSPL30           YES-GET STATIONS' MARKETS                    
         BRAS  RE,XSPL40           GET SPILL MARKETS' NAMES                     
         LA    R1,XSPILLL                                                       
         ST    R1,DMCB+8                                                        
         LA    R1,L'XSBMSTA        SORT TABLE BY MARKET/STATION                 
         ST    R1,DMCB+12                                                       
         GOTO1 XSORT,DMCB,SBAXSPIL,(R2),,,0                                     
         B     XSPLX                                                            
*                                                                               
*                                                                               
XSPL20   DS    0H                  *** ADD SPILL MARKETS TO TABLE ***           
         MVC   XSSTA,XSDFKSTA      STATION                                      
         CLI   XSSTA+4,C' '                                                     
         BH    *+8                                                              
         MVI   XSSTA+4,C'T'                                                     
         LA    R4,XSMKTS                                                        
         LA    RF,XSNMKTS+1                                                     
         LA    R1,XSDFEL           LOOK FOR SPILL MARKET ELEMENTS               
         SR    R0,R0                                                            
*                                                                               
XSPL22   CLI   0(R1),0                                                          
         BE    XSPL26                                                           
         CLI   0(R1),5                                                          
         BNE   XSPL24                                                           
         USING XSDFEL05,R1                                                      
         MVC   0(2,R4),XSDFAMKT    SAVE AGENCY MARKET                           
         MVC   2(2,R4),XSDFRMKT    SAVE RATING SERVICE MARKET                   
         LA    R4,L'XSMKTS(R4)                                                  
         BCT   RF,XSPL24                                                        
         DC    H'0'                INCREASE XSNMKTS                             
*                                                                               
XSPL24   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     XSPL22                                                           
*                                                                               
XSPL26   OC    XSMKTS,XSMKTS       CHECK AT LEAST ONE SPILL MARKET              
         BNZ   *+12                                                             
         XC    0(XSPILLL,R3),0(R3)                                              
         BR    RE                                                               
         LA    R3,XSPILLL(R3)                                                   
         BCTR  R5,RE                                                            
         DC    H'0'                INCREASE XSNSTA                              
*                                                                               
*                                                                               
XSPL30   LR    R0,RE               *** GET STATIONS' MARKETS ***                
         L     R3,SBAXSPIL                                                      
         LR    R5,R2               R5 = N'STATIONS                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(STAKEYLN-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,SBQMED                                                   
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,SBCLT                                                    
         MVC   KEYSAVE,KEY                                                      
*                                                                               
XSPL32   MVC   STAKCALL,XSSTA                                                   
         L     R4,SBAIO2                                                        
         ST    R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MSPACK,DMCB,SMKT,XSSTA,XSBMSTA  SAVE PACKED MKT/STA              
         MVC   KEY,KEYSAVE                                                      
         LA    R4,KEY                                                           
         LA    R3,XSPILLL(R3)      NEXT STATION                                 
         BCT   R5,XSPL32                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
XSPLX    B     XIT                                                              
*                                                                               
* INITIALIZE TO RUN DRIVER - LOADS PHASES                                       
*                            SETS GLOBAL ADDRESSES                              
*                                                                               
VINTDRIV CLI   OFFLINE,C'Y'                                                     
         BNE   DRIX                                                             
*                                                                               
         XC    ATSARBF2,ATSARBF2   CLEAR CML TSAR BUFFER                        
*                                                                               
         LA    R1,PWAREA                                                        
         STCM  R1,7,SBAWIPW                                                     
         GOTO1 CALLOV,DMCB,X'B2000000',0,0  LOAD T204B1 (BUFFERS)               
         L     R2,DMCB                                                          
         LA    R2,8(R2)                                                         
         ST    R2,SBAPRDBF         A(PRODUCT BUFFER)                            
         LHI   R1,PRDBUFFL         PRODUCT BUFFER ENTRY LENGTH                  
         SLL   R1,8                X 256                                        
         LA    R2,8(R1,R2)                                                      
         ST    R2,SBAESTTB         A(ESTIMATE TABLE)                            
         LA    R1,1                256 X 256                                    
         SLL   R1,16                                                            
         LA    R2,8(R1,R2)                                                      
         ST    R2,SBAUSNTB         A(USER DEMO NAME TABLE)                      
         L     R1,4(R2)            GET MAXIMUM NUMBER OF USER NAMES             
         MHI   R1,7                                                             
         LA    R2,16(R1,R2)                                                     
         ST    R2,SBADPTTB         A(DAYPART TABLES BUFFER)                     
         CLI   SBQSEPES,C'Y'       TEST NOT SEPARATE ESTIMATES                  
         BE    DRI1                                                             
         CLC   SBQPRD,=C'POL'      AND PRD=POL                                  
         BNE   DRI1                                                             
         CLI   SBQMED,C'*'         AND SINGLE MEDIA REQUEST                     
         BE    DRI1                                                             
         XC    SBADPTTB,SBADPTTB   YES - WE DON'T NEED IT                       
*                                                                               
DRI1     LA    R1,180              L'ENTRY                                      
         MHI   R1,36               N'ENTRIES                                    
         LA    R2,8(R1,R2)                                                      
         ST    R2,SBAMWTAB         A(MARKET WEIGHT TABLE)                       
         SR    R0,R0                                                            
         LA    R1,MWTABL           L'ENTRY                                      
         LA    RE,MWTENTS          N'ENTRIES                                    
         MR    R0,RE                                                            
         LA    R2,8(R1,R2)                                                      
         ST    R2,AMKTLST          A(MARKET LIST)                               
         SR    R0,R0                                                            
         LA    R1,MKTLSTL          L'ENTRY                                      
         LA    RE,MLNMKTS          N'ENTRIES                                    
         MR    R0,RE                                                            
         LA    R2,8(R1,R2)                                                      
         ST    R2,SBAESTBF                                                      
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',A(GLOBALLN),A(GLOBALLN)                       
         ICM   R4,15,4(R1)         GET FREE STORAGE                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,0(R4)                                                         
         ST    R4,AGLOBAL          DRIVER'S GLOBAL AREA                         
         MVC   0(8,R4),=C'*GLOBAL*'                                             
         XC    8(8,R4),8(R4)                                                    
         MVC   16(4,R4),=F'200000' THIS MAPS TO GLSIZE                          
*                                                                               
         A     R1,=F'200016'                                                    
         ST    R1,SBAOFFBF         A(OFFLINE BUFFER)                            
         MVC   0(4,R1),=F'240000'  L'BUFFER                                     
*                                                                               
         A     R1,=F'240000'                                                    
         ST    R1,ANETSIZE         L'TABLE IS 256 BYTES                         
*                                                                               
         AHI   R1,256              MKT-EST TABLE                                
         ST    R1,SBAMKEST         L'TABLE IS 256 BYTES                         
*                                                                               
         AHI   R1,256              USER COMMENT DATA AREA                       
         ST    R1,AUCOMTAB                                                      
*                                                                               
         AHI   R1,UCOML            UCOML IS L'UCOMTAB                           
         ST    R1,APURPTAB                                                      
*                                                                               
         AHI   R1,PURPTABL*PURPTABQ                                             
         STCM  R1,15,SBALKXTD                                                   
*                                                                               
         AHI   R1,32               BUMP PAST SPXTAREA                           
         STCM  R1,15,SBAFLTAB      START OF FLIGHT TABLE                        
*                                                                               
         AHI   R1,4080             BUMP PAST FLIGHT TABLE                       
         STCM  R1,15,SBATSATB      START OF TSAR AIO AREA                       
*                                                                               
         AHI   R1,4000             BUMP PAST TSAR AIO AREA                      
         AHI   R1,2                FOR TSAR RECORD LENGTH                       
         AHI   R1,TSARKEYL         FOR TSAR MAX KEY LENGTH                      
*                                                                               
         STCM  R1,7,SBASTANT       CURRENT STATION BUFFER ENTRY                 
         AHI   R1,SBUFMAX          BUMP PAST STATION BUFFER ENTRY               
*                                                                               
         STCM  R1,15,AGETBFRW      A(GETBFRW) WORK AREA + OPTKEY                
         AHI   R1,SPGBFRDL+(SPGBACOM-SPGBAM)                                    
*                                                                               
         STCM  R1,15,ACOMLSTG      COMSCORE DEMO LIST FOR GOALS                 
         AHI   R1,160              BUMP PAST COMSCORE DEMO LIST                 
         GOTO1 COVAIL,DMCB,C'GET',20000,20000    GET MARKET GROUP TABLE         
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,SBAMGTAB         A(MARKET GROUP TABLE)                        
*                                                                               
         L     RE,ATWA                                                          
         USING CONHEADH-64,RE                                                   
         CLC   =C'HD',CONREC       HD REPORT?                                   
         BNE   DRI2                NO - DON'T ALLOCATE CMML TABLE               
         DROP  RE                                                               
         LA    R2,2000             YES-SET MAX N'CML TABLE ENTRIES              
         LR    R1,R2                                                            
         MHI   R1,CMLTABL                                                       
         LA    R1,8(R1)                                                         
         ST    R1,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'   GET STORAGE FOR COMMERCIAL TABLE            
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,SBACMLTB         SET A(COMMERCIAL TABLE)                      
         ST    R2,4(RE)                                                         
*                                                                               
DRI2     LA    RE,CHKTABL                                                       
         TM    SBQREAD,SBQRDCLS    TEST READ CLEARANCE STATUS RECORDS           
         BNZ   DRI2A                                                            
         LA    RE,BPCTTABL                                                      
         TM    SBQREAD2,SBQRD2BP   BPCT ALSO USES CMML TABLE SINCE              
         BZ    DRI4                THEY ARE MUTUALLY EXCLUSIVE                  
*                                                                               
DRI2A    LHI   R5,2000             YES-GET STORAGE FOR CHECK TABLE              
         LR    R3,R5                                                            
         MR    R2,RE               MAX N'RECORDS * L'ENTRY                      
         LA    R3,12(R3)                                                        
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,SBACHKTB                                                      
         ST    R3,0(RE)            +0 = L'TABLE                                 
         XC    4(4,RE),4(RE)       +4 = N'RECORDS IN TABLE SO FAR               
         ST    R5,8(RE)            +8 = MAX N'RECORDS IN TABLE                  
*                                                                               
DRI4     DS    0H                                                               
*&&DO                                                                           
         L     R0,=A(TSARBUFL)     L'BUFFER                                     
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ATSARBF                                                       
*&&                                                                             
*                                                                               
         USING GLOBALD,R4                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,DRIVER                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A46'   LOAD T00A46 (SPOTPAK DRIVER)         
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
         MVC   GLAPROG,ADPGPROG                                                 
         MVI   GLTWORKD,GLTSPOOL                                                
         MVC   GLFHEADL,MYFIRSTH                                                
******** MVI   GLPUTMAX,20         INCREASE DRIVER'S INPUT STACK TO 20          
         LLC   RE,GLFHEADL                                                      
         LA    RE,3(RE)                                                         
         LA    RF,14                                                            
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         STC   RE,GLLHEADL                                                      
         MVC   GLSPACE,SPACOPT      PASS THRU SPACING OPT                       
         MVC   GLBOXOPT,BOXOPT                BOX OPTION                        
         MVC   GLLFTOPT,LEFTOPT           AND LEFT OPTION                       
         MVC   GLDOWNLD,DOWNOPT     DOWNLOAD OPTIONS                            
         MVC   GLDWNLD2,DOWNOPT2                                                
         MVC   GLDLCHAR,DOWNCHAR                                                
         CLI   TRACEOPT,C'Y'        OPTION TO TRACE                             
         BNE   *+8                                                              
         MVI   GLTRACE,C'Y'                                                     
         MVC   GLMAXREC,MAXREC      SET MAX INPUT RECORD COUNT                  
         MVI   GLDETHED,C'Y'        O/P HEADLINES AT DETAIL TIME                
         MVC   GLINDS,DRINDS        PASS THROUGH INDICATORS                     
         OI    GLINDS,GLISDONT      SUPPRESS SPACING FOR REJECT LINES           
         MVC   GLINDS2,DRINDS2                                                  
         OI    GLINDS3,GLNOOVER     SUPPRESS LARGE NEGATIVE NUMBERS             
         MVC   GLRNKMAX+2(2),RANKMAX    RANK MAXIMUM                            
******** MVI   GLNORBOX,X'40'       TURN OFF BOXES FOR TOTALS                   
*                                                                               
         XC    GLOPTS,GLOPTS       SET GLOBAL USER OPTIONS                      
         CLI   SBQPGRD,C' '                                                     
         BNH   DRI6                                                             
         MVI   GLOPTS,1            OPT1=NUMBER OF PRODUCT GROUP LEVELS          
         CLC   SBPGR1LN,SBPGR2LN                                                
         BE    DRI6                                                             
         MVI   GLOPTS,2                                                         
*                                                                               
DRI6     CLI   SBQMGRD,0                                                        
         BE    DRI8                                                             
         MVI   GLOPTS+1,1          OPT2=NUMBER OF MARKET GROUP LEVELS           
         CLC   SBMGR1LN,SBMGR2LN                                                
         BE    DRI8                                                             
         MVI   GLOPTS+1,2                                                       
         CLC   SBMGR2LN,SBMGR3LN                                                
         BE    DRI8                                                             
         MVI   GLOPTS+1,3                                                       
*                                                                               
DRI8     B     DRI10                                                            
         EJECT                                                                  
* INITIALIZATION OF PRINT RELATED FIELDS                                        
*                                                                               
DRI10    L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         CLI   WIDTHOPT,C'W'                                                    
         BE    DRIWIDE                                                          
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
         LA    R1,H1               PRINT ADDRESSES FOR STANDARD                 
         ST    R1,AH1                                                           
         LA    R1,H4                                                            
         ST    R1,AH4                                                           
         LA    R1,P                                                             
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'132'                                                   
         LA    R1,REGSPECS                                                      
         ST    R1,SPECS                                                         
         CLI   WIDTHOPT,C'N'                                                    
         BNE   DRIX                                                             
         LA    R1,NARSPECS                                                      
         ST    R1,SPECS                                                         
         B     DRIX                                                             
*                                                                               
DRIWIDE  MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
         LA    R1,XHEAD4                                                        
         ST    R1,AH4                                                           
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'198'                                                   
         LA    R1,WIDSPECS                                                      
         ST    R1,SPECS                                                         
         B     DRIX                                                             
*                                                                               
DRIX     B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
VPUTOMBY DS    0H                                                               
         USING DOIDELD,R3                                                       
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,26       BUYER NAME(26) + CITY(2)                     
         MVI   0(R4),NPOMBCDQ      REC TYPE (15)                                
         MVC   1(3,R4),DOIDBYR     BUYER                                        
         BRAS  RE,TSARHIGH         HAVE THIS BUYER?                             
         BNE   POMBYR05            NO, LOOK THIS ADDS BUYER RECORD              
*                                                                               
         OC    OMOFFICE,OMOFFICE   FILTERING ON OFFICE?                         
         BZ    POMBYREQ            NOPE, SET CC EQU                             
         CLC   OMOFFICE,MYDATA     MATCH?                                       
         B     POMBYRX             CC IS SET                                    
*                                                                               
POMBYR05 XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BYRKEY,R4                                                        
         MVI   BYRKTYP,BYRKTYPQ    X'0D'                                        
         MVI   BYRKSUB,BYRKSUBQ    X'31'                                        
         MVC   BYRKAM,SBBAGYMD     A/M                                          
         MVC   BYRKBYR,DOIDBYR     3-BYTE BUYER CODE                            
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(6),KEYSAVE      HAVE THIS BUYER RECORD?                      
         BNE   POMBYRNE            NO, RESTORE KEY & I/O AREA & EXIT            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ      X'10' DESCRIPTION ELEMENT                    
         BRAS  RE,GETEL            HAVE ONE?                                    
         BNE   POMBYRNE            NO                                           
         OC    OMOFFICE,OMOFFICE   FILTERING ON OFFICE?                         
         BZ    POMBYR10            NOPE                                         
         USING BYRDSCD,R6                                                       
         CLC   OMOFFICE,BYROFFID   MATCH ON OFFICE CODE?                        
         BNE   POMBYRNE            NO                                           
*                                                                               
POMBYR10 XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,26       BUYER NAME(26) + CITY(2)                     
         MVI   0(R4),NPOMBCDQ      REC TYPE (15)                                
         MVC   1(3,R4),DOIDBYR     BUYER                                        
         MVC   MYDATA(2),BYROFFID  OFFICE ID                                    
         MVC   MYDATA+2(L'BYRFNAME),BYRFNAME                                    
         BRAS  RE,TSARADD                                                       
         B     POMBYREQ                                                         
*                                                                               
POMBYRNE LTR   RB,RB                                                            
         B     POMBYRX                                                          
*                                                                               
POMBYREQ CR    RB,RB                                                            
*                                                                               
POMBYRX  B     XIT                                                              
         DROP  R3,R6                                                            
*                                                                               
VGETOMBY DS    0H                                                               
         ICM   R6,15,OMREC                                                      
         BZ    GOMBYRX                                                          
         MVI   ELCODE,DOIDELQ        X'01' PRIMARY ID ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   GOMBYRX                                                          
*                                                                               
         USING DOIDELD,R6                                                       
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,26       BUYER NAME(26) + CITY(2)                     
         MVI   0(R4),NPOMBCDQ      REC TYPE (15)                                
         MVC   1(3,R4),DOIDBYR     BUYER                                        
         BRAS  RE,TSARHIGH         HAVE THIS BUYER?                             
         BNE   GOMBYRX             NO, DONE                                     
*                                                                               
         MVC   0(2,R2),MYDATA                                                   
         L     RE,AGLOBAL                                                       
         CLI   GLARGS+2-GLOBALD(RE),C'N'                                        
         BNE   GOMBYRX                                                          
         MVC   0(24,R2),MYDATA+2                                                
*                                                                               
GOMBYRX  B     XIT                                                              
*                                                                               
VPUTNINV DS    0H                                                               
         L     R6,SBAIO1                                                        
         USING SNVKEYD,R6                                                       
*                                                                               
         BAS   RE,CLEARAIO         CLEAR NINVREC                                
*                                                                               
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         XR    R2,R2                                                            
         ICM   R2,3,SNVRLEN        INVOICE RECORD LENGTH                        
         LA    R1,SNVELS-SNVKEY    SAVING EVERYTHING FROM X'10' ON              
         SR    R2,R1               LENGTH OF ELEMENTS                           
         STCM  R2,3,THISRECL                                                    
         MVI   0(R4),NPNINCDQ      REC TYPE (16)                                
         MVC   1(8,R4),SNVKAM      MOVE A/M-CLT-STA-DATE                        
         MVC   9(10,R4),SNVKINV    INVOICE NUMBER                               
         OI    TSARFLAG,TSARNINV   INDICATE THAT WE ARE DOING NINV              
         L     R1,NINVREC          BUILD RECORD HERE                            
         MVC   2(20,R1),MYKEY                                                   
         LA    R3,0                INDICATE FIRST OF MINIO SET                  
*                                                                               
PNINV05  STC   R3,19(R4)           RECORD NUMBER IN MYKEY                       
         STC   R3,21(R1)           RECORD NUMBER IN INV REC                     
*                                                                               
         BRAS  RE,TSARHIGH         HAVE THIS INVOICE RECORD?                    
         BNE   PNINV10             NO, PROCESS THIS INVOICE                     
         BAS   RE,CLEARAIO         CLEAR NINVREC                                
         MVC   2(20,R1),MYKEY      SAVED KEY                                    
         AHI   R3,1                BUMP MINIO INVOICE NUMBER                    
         B     PNINV05                                                          
*                                                                               
PNINV10  BAS   RE,CLEARAIO         CLEAR NINVREC                                
         MVC   2(20,R1),MYKEY      SAVED KEY                                    
         L     R0,NINVREC          A(BUFFERED INV REC)                          
         AHI   R0,31               BUILD ELEMENTS HERE                          
         LR    R1,R2               LENGTH OF ELEMENTS                           
         LA    RE,SNVELS           COPY THESE ELEMENTS FROM AIO1                
         LR    RF,R1               LENGTH OF ELEMENTS                           
         MVCL  R0,RE               COPY THE ELEMENTS TO NINVREC                 
*                                                                               
         BRAS  RE,TSARADD          BUFFER THIS INVOICE RECORD                   
*                                                                               
PNINVX   J     XIT                                                              
         DROP  R6                                                               
*                                                                               
CLEARAIO NTR1                                                                   
         L     R0,NINVREC          CLEAR TO BUILD TSAR BUFFER                   
         LAY   R1,6000                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     XIT                                                              
*                                                                               
VGETNINV DS    0H                                                               
*                                                                               
         L     R6,SBAIO1                                                        
         USING SNVKEYD,R6                                                       
*                                                                               
         BAS   RE,CLEARAIO                                                      
*                                                                               
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         XR    R2,R2                                                            
         ICM   R2,3,SNVRLEN        INVOICE RECORD LENGTH                        
         LA    R1,SNVELS-SNVKEY    SAVING EVERYTHING FROM X'10' ON              
         SR    R2,R1               LENGTH OF ELEMENTS                           
         STCM  R2,3,THISRECL                                                    
         MVI   0(R4),NPNINCDQ      REC TYPE (16)                                
         MVC   1(8,R4),SNVKAM      MOVE A/M-CLT-STA-DATE                        
         MVC   9(10,R4),SNVKINV    INVOICE NUMBER                               
         MVC   19(1,R4),BYTE                                                    
         OI    TSARFLAG,TSARNINV   INDICATE THAT WE ARE ADDING NINV             
         L     R1,NINVREC          BUILD RECORD HERE                            
         MVC   2(20,R1),MYKEY                                                   
         BRAS  RE,TSARHIGH         HAVE THIS INVOICE RECORD?                    
         BE    GNINVEQU            YES, SET CC EQU                              
*                                                                               
         BRAS  RE,TSARDEL          DELETE THIS BUFFERED INVOICE                 
*                                                                               
GNINVNEQ NI    TSARFLAG,X'FF'-TSARNINV                                          
         LTR   RB,RB                                                            
         B     GNINVXIT                                                         
*                                                                               
GNINVEQU NI    TSARFLAG,X'FF'-TSARNINV                                          
         CR    RB,RB                                                            
*                                                                               
GNINVXIT B     XIT                                                              
         DROP  R6                                                               
***********************************************************************         
* ROUTINE TO BUFFER REDI SALESPERSON/POINTPERSON                      *         
* INPUT  : PARM 1 = 5 CHAR SALESPERSON CODE + C'S' OR C'P'            *         
*          PARM 2 = 40 CHAR SALESPERSON NAME                          *         
***********************************************************************         
VPUTSLP  DS    0H                  PUT A SALESPERSON ELEMENT                    
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPSLPCDQ      TYPE (18)                                    
         L     RE,0(R1)                                                         
         MVC   1(6,R4),0(RE)       SALESPERSON CODE + C'S' OR C'P'              
         L     RE,4(R1)            SALESPERSON NAME                             
         LA    R2,40               MAX NAME LENGTH                              
         LA    R3,39(RE)           LAST CHAR OF NAME                            
*                                                                               
PSLP10   CLI   0(R3),X'40'         LAST CHAR?                                   
         BH    PSLP20              YES                                          
         BCTR  R3,0                MOVE BACK 1 SPACE                            
         BCT   R2,PSLP10           DECREMENT NAME LENGTH COUNTER                
*                                                                               
PSLP20   MVI   THISRECL,0                                                       
         STC   R2,THISRECL+1       R1 POINTS TO AREA CONTAINING:                
         BCTR  R2,0                +0 TYPE (1)                                  
         EX    R2,*+8              +1 SEQ NUM (2)                               
         B     *+10                +3 LENGTH OF DATA (1)                        
         MVC   MYDATA(0),0(RE)     +4 THE DATA                                  
         BRAS  RE,TSARADD                                                       
         B     XIT                                                              
***********************************************************************         
* ROUTINE TO GET BUFFERED REDI SALESPERSON/POINTPERSON                *         
* INPUT  : PARM 1 = 5 CHAR SALESPERSON CODE + C'S' OR C'P'            *         
*          PARM 2 = 40 CHAR AREA TO PUT SALESPERSON NAME              *         
***********************************************************************         
VGETSLP  DS    0H                  GET A SALESPERSON ELEMENT                    
         XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         LA    R4,MYKEY                                                         
         MVI   0(R4),NPSLPCDQ      TYPE (18)                                    
         L     RE,0(R1)                                                         
         MVC   1(6,R4),0(RE)       SALESPERSON CODE + C'S' OR C'P'              
         BRAS  RE,TSARHIGH                                                      
         BNE   NEXIT                                                            
*                                                                               
         L     R3,4(R1)            A(CODEAREA)                                  
         LLC   RE,MYKEY-1          LENGTH                                       
         SHI   RE,(L'MYKEY+2)      KEY LENGTH + 2 BYTE LENGTH                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),MYDATA                                                   
         B     EQXIT                                                            
*                                                                               
VGTSTABF XC    MYREC(MYRECL),MYREC CLEAR KEY+DATA                               
         MVI   MYKEY,NPSTACDQ      TYPE (17)                                    
         MVC   MYKEY+1(3),0(R1)    BINARY STATION                               
         LA    R1,SBAGYREC                                                      
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'      CANADIAN                                     
         BE    GETSTA10            YES, NFC(ABLE) FOR CANADA                    
         DROP  R1                                                               
         CLI   MYKEY+1,X'E8'       CABLE?                                       
         BL    GETSTA10            NO                                           
         NI    MYKEY+3,X'80'       STRIP NETWORK BITS                           
*                                                                               
GETSTA10 ICM   R0,7,SBASTANT       HAVE A(STATION BUFFER ENTRY)?                
         BZ    XIT                 NO                                           
         LA    R1,SBUFMAX          YES - CLEAR IN CASE ENTRY NOT FOUND          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,TSARHIGH         FOUND STATION BUFFER ENTRY?                  
         BNE   XIT                 NO                                           
*                                                                               
         ICM   R0,7,SBASTANT       A(STATION BUFFER ENTRY)                      
         L     RE,SBATSATB         RECORD WAS BUFFERED HERE                     
         AHI   RE,TSARKEYL+2       BUMP PAST LENGTH AND KEY                     
         XR    R1,R1                                                            
         ICM   R1,3,MYREC-2                                                     
         SHI   R1,TSARKEYL+2       LENGTH                                       
         LR    RF,R1               LENGTH                                       
         MVCL  R0,RE                                                            
         B     XIT                                                              
*                                                                               
VGETACOM DS    0H                  GET ACOM RECORD                              
         BRAS  RE,VGTACOM                                                       
         BNE   NEXIT                                                            
         B     EQXIT                                                            
*                                                                               
* CONSTANTS TABLES ETC                                                          
*                                                                               
BLANKS   DC    CL132' '                                                         
*                                                                               
XFF      DC    XL4'FFFFFFFF'                                                    
*                                                                               
SUBTIT2  DC    CL32' '                                                          
SUBTIT3  DC    CL32' '                                                          
UNKNOWN  DC    CL07'UNKNOWN'                                                    
UNASGN   DC    CL10'UNASSIGNED'                                                 
*                                                                               
CLSLIST  DS    CL16                LIST OF CLASS SPLITS                         
CLSNUM   DS    X                   NUMBER OF CLASSES                            
*                                                                               
MYSVR1   DS    F                   SAVE R1                                      
SAVEKEY  DS    XL13                SAVED KEY                                    
*                                                                               
PRDTAB   DS    XL256               TABLE OF PRODUCTS FOR THIS STA               
***                                                                             
* BUILD TSAR RECORDS HERE FOR I/O                                               
***                                                                             
         DS    CL2                 VARIABLE RECORD LENGTH                       
MYREC    DS    0D                                                               
MYKEY    DS    CL(TSARKEYL)        MAX KEY LENGTH                               
MYDATA   DS    CL(TSARRECL)        MAX DATA LENGTH                              
MYRECL   EQU   *-MYREC                                                          
*                                                                               
THISRECL DS    CL2                 CURRENT RECORD LENGTH                        
*                                                                               
TSARFLAG DS    CL1                 TSAR FLAG                                    
TSARNINV EQU   X'80'               DOING NEW INVOICES                           
TSARCMML EQU   X'40'               DOING COMMERCIAL RECORD                      
*                                                                               
CMLFLAG  DS    CL1                 CML ADDED FLAG                               
CMLOPT   DS    0XL8                CML OPTIMIZATION (A/M,CLT,SEQ NUM)           
CMLOPTAM DS    XL1                 CML OPTIMIZATION A/M                         
CMLOPTCL DS    XL2                 CML OPTIMIZATION CLIENT                      
CMLOPTSQ DS    XL2                 CML OPTIMIZATION SEQ NUM                     
CMLOPTPR DS    CL3                 CML OPTIMIZATION PRD (FOR CML CLASS)         
*                                                                               
ELEM2    DS    CL256               ELEMENT FOR TSARED CML REC                   
*                                                                               
MWGTDEM  DS    XL3                 USE INSTEAD OF PRMYDEMO FOR MKT WGT          
         EJECT                                                                  
REGSPECS DS    0C                                                               
         SSPEC H1,2,RUN            SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
NARSPECS DS    0C                                                               
         SSPEC H1,60,RUN           SPECS FOR NARROW PRINTING                    
         SSPEC H2,60,REQUESTOR                                                  
         SSPEC H4,60,REPORT                                                     
         SSPEC H4,73,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
WIDSPECS DS    0C                                                               
         WSPEC H1,2,RUN            SPECS FOR WIDE PRINTING                      
         WSPEC H2,2,REQUESTOR                                                   
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,129,REPORT                                                    
         WSPEC H4,142,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
***********************************************************************         
* CLEAR THE COMMERCIAL TSAR BUFFER                                    *         
***********************************************************************         
DELCML   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    CMLOPT,CMLOPT       CLEAR CML OPTIMIZATION                       
         ICM   R1,15,ATSARBF2      HAVE A(TSAR BUFFER2)?                        
         BNZ   *+6                 YES                                          
         DC    H'0'                NO                                           
         ICM   R0,15,=A(TSA2BUFL)  1 MEG BUFFER                                 
         FREEMAIN RC,A=(1),LV=(0)  FREE STORAGE                                 
         LTR   RF,RF               ANY ERRORS?                                  
         BZ    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
         BRAS  RE,INITSAR          INITIALIZE THE TSAR BUFFER                   
         J     XIT                 EXIT                                         
         DROP  R2                                                               
***********************************************************************         
* ALLOCATE STORAGE AND INIT THE CML TSAR BUFFER                       *         
***********************************************************************         
INITSAR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   R0,15,=A(1000000)   1 MEG BUFFER FOR CML ENTRIES                 
         GETMAIN  RU,LV=(0),LOC=(ANY,ANY)    31-BIT STORAGE                     
         LTR   RF,RF               ANY ERRORS?                                  
         BZ    *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
         ST    R1,ATSARBF2         A(TSAR BUFFER) FOR CML ENTRIES               
*                                                                               
         OC    ATSARBF2,ATSARBF2   BUFFER PRESENT?                              
         BNZ   *+6                 YES                                          
         DC    H'0'                WHERE ARE WE SUPPOSED TO KEEP RECS?          
*                                                                               
         XC    TSAREA2,TSAREA2                                                  
         LA    R2,TSAREA2                                                       
         USING TSARD,R2                                                         
*                                                                               
         MVI   TSOFFACT,TSAINI     ACTION = INIT                                
         MVC   TSABUF,ATSARBF2     SET A(BUFFER)                                
         MVC   TSAREC,=A(TSA2BUFL) SET BUFFLEN HERE ON INIT CALL                
         MVI   TSKEYL,TSA2KEYL     KEY LENGTH                                   
         OI    TSRECI,TSRVAR       SET VARIABLE LENGTH RECS                     
         MVC   TSRECL,=Y(TSA2RECL) MAX REC LEN                                  
         OI    TSIND2,TSI2MANY     N'RECS IS FULLWORD                           
         GOTO1 ATSAROF,(R2)        INIT TSAR                                    
         CLI   TSERRS,0            ANY ERRORS?                                  
         BE    *+6                 NO - EXIT                                    
         DC    H'0'                YES - DEATH                                  
         J     XIT                 EXIT                                         
         DROP  R2                                                               
***********************************************************************         
* READ THE COMMERCIAL RECORDS AND BUFFER WHAT WE NEED                 *         
* ** NOTE *** DON'T TOUCH R3,R4 AND R5                                *         
***********************************************************************         
READCML  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SYSDIR,=C'TRFDIR  ' SET DIRECTORY TO TRAFFIC                     
         MVC   SYSFIL,=C'TRFFILE ' SET FILE TO TRAFFIC                          
*                                                                               
         MVC   AIO,AIO2            READ THE RECORD INTO AIO2                    
         MVI   CMLFLAG,C'N'        NO COMMERCIAL ADDED YET                      
         XC    MYSVR1,MYSVR1       MYSVR1= A(CMLCLASS)                          
*                                                                               
         LA    R5,KEY              READ COMMERCIAL RECORD                       
         USING CMLRECD,R5                                                       
         XC    CMLKEY,CMLKEY       CLEAR THE KEY                                
         MVC   CMLPID(2),=X'0AA1'  PASSIVE KEY MY CMML SEQ                      
         MVC   CMLPAM,SBBAGYMD     A/M                                          
         MVC   CMLPCLT,SBBCLT      CLIENT                                       
         OC    SBBCMCLT,SBBCMCLT   HAVE MASTER TRAFFIC CLIENT CODE?             
         BZ    *+10                NO                                           
         MVC   CMLPCLT,SBBCMCLT    YES-USE THAT                                 
         MVC   CMLPSEQ+1(2),SBCMLSQ SEQ NUMBER FROM BUY RECORD                  
         TM    DTAIND11,DIADID2    USE SBCMLSQ2 FOR AD-IDCD2 KEYWORD?           
         BZ    *+10                NO                                           
         MVC   CMLPSEQ+1(2),SBCMLSQ2 SEQ NUMBER 2 FROM BUY RECORD               
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   CMLKEY,KEYSAVE      TEST RECORD FOUND                            
         BNE   RDCMLXIT            NO - DONE                                    
*                                                                               
         XC    ELEM2,ELEM2         CLEAR AIO AREA FOR CMML RECORD               
*                                                                               
         LA    R4,ELEM2            ADD COMMERCIAL RECORD HERE                   
         MVI   1(R4),TSA2KEYL+2    RECORD LENGTH THUS FAR (ONLY KEY)            
         MVI   2(R4),NPCMLCDQ      COMMERCIAL REC                               
         MVC   3(1,R4),SBBAGYMD    A/M                                          
         MVC   4(2,R4),CMLPCLT     CLIENT                                       
         MVC   6(2,R4),CMLPSEQ+1   SEQ NUMBER                                   
         AHI   R4,TSA2KEYL+2       RECORD STARTS HERE                           
         DROP  R5                                                               
*                                                                               
         GOTO1 GETREC              GET THE COMMERCIAL RECORD                    
         MVC   CMLCODE,BLANKS      NO COMMERCIAL CODE YET                       
         L     R6,AIO              A(CML RECORD)                                
*                                                                               
         MVI   HALF,X'24'          HIGHDEF/CENTERCUT ELEMENT = X'24'            
         MVI   HALF+1,CMLXHDEF-CMLXDTEL HIGHDEF DISPLACEMENT INTO ELEM          
         TM    SBEFLAG5,SBE5HDEF   WANT HIGHDEF?                                
         BNZ   RDCML20             YES                                          
         MVI   HALF+1,CMLXCNTR-CMLXDTEL CNTRCUT DISPLACEMENT INTO ELEM          
         TM    SBEFLAG5,SBE5CNTR   WANT CENTERCUT?                              
         BNZ   RDCML20             YES                                          
         MVI   HALF,X'A0'          AD-ID ELEMENT = X'A0'                        
         MVI   HALF+1,CMLADID-CMLADIEL  AD-ID DISPLACEMENT INTO ELEM            
         TM    SBEFLAG4,SBEADID    WANT AD-ID?                                  
         BNZ   RDCML20             YES                                          
         XC    HALF,HALF           NO - CLEAR HALF AND ADD ISCII ELEM           
         USING CMLRECD,R6                                                       
         GOTO1 ADDEL,DMCB,0(R4),X'01',10,CMLKCML                                
         DROP  R6                                                               
*                                                                               
RDCML20  AHI   R6,24               A(FIRST ELEMENT)                             
         SR    R3,R3               COMMERCIAL CLASS COUNTER                     
         LA    R5,CLSLIST          R5 = COMMERCIAL CLASS LIST                   
         XC    CLSLIST,CLSLIST     CLEAR THE COMMERCIAL CLASS LIST              
*                                                                               
RDCML25  CLI   0(R6),0             END OF RECORD?                               
         BE    RDCML90             YES - ADD TO TSAR BUFFER                     
         CLC   0(1,R6),HALF        HDEF/CNTRCT/AD-ID ELEMENT?                   
         BE    RDCML40             YES                                          
         CLI   0(R6),X'21'         COMMERCIAL CLASS ELEMENT?                    
         BE    RDCML50             YES                                          
         CLI   0(R6),X'10'         COMMERCIAL DATA ELEMENT?                     
         BE    RDCML60             YES                                          
         CLI   0(R6),X'30'         COMMERCIAL NAME ELEMENT?                     
         BE    RDCML70             YES                                          
         CLI   0(R6),X'90'         BROADCAST BUSINESS ELEMENT?                  
         BE    RDCML80             YES                                          
*                                                                               
RDCML30  LLC   R0,1(R6)            LENGTH OF ELEMENT                            
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         B     RDCML25                                                          
*                                                                               
RDCML40  LLC   R2,HALF+1           DISPLACEMENT INTO ELEMENT                    
         AR    R2,R6                                                            
         GOTO1 ADDEL,DMCB,0(R4),X'01',14,0(R2)                                  
         XC    HALF,HALF           CLEAR ELEM/LENGTH FLAG                       
         B     RDCML30             BUMP TO NEXT ELEMENT                         
*                                                                               
         USING CMLCLSEL,R6                                                      
RDCML50  MVC   0(L'CMLCLS,R5),CMLCLS                                            
         LA    R5,L'CMLCLS(R5)     BUMP TO NEXT COMMERCIAL CLASS                
         BCT   R3,RDCML30          COUNT OF SPLITS (NEGATIVE)                   
         DROP  R6                                                               
*                                                                               
         USING CMLDTAEL,R6                                                      
RDCML60  GOTO1 ADDEL,DMCB,0(R4),X'03',17,CMLTITLE                               
         GOTO1 ADDEL,DMCB,0(R4),X'04',22,CMLCLTNO                               
         CLC   CMLCLASS,BLANKS     CLASS DEFINED?                               
         BNH   RDCML30             NO                                           
         GOTO1 ADDEL,DMCB,0(R4),X'05',6,CMLCLASS                                
         LA    R1,CMLCLASS         A(CMLCLASS)                                  
         ST    R1,MYSVR1           SAVE A(CMLCLASS) IN MYSVR1                   
         B     RDCML30             BUMP TO NEXT ELEMENT                         
         DROP  R6                                                               
*                                  GET NAME 2 AND 3                             
         USING CMLDSCEL,R6                                                      
RDCML70  MVI   DMCB+4,X'06'        ELEMENT X'06' FOR NAME 2                     
         CLI   CMLDSCSQ,1          IS THIS NAME 2?                              
         BE    RDCML75             YES                                          
         MVI   DMCB+4,X'07'        ELEMENT X'07' FOR NAME 3                     
         CLI   CMLDSCSQ,2          IS THIS NAME 3?                              
         BNE   RDCML30             NO                                           
RDCML75  GOTO1 ADDEL,DMCB,0(R4),,22,CMLDSC                                      
         B     RDCML30             BUMP TO NEXT ELEMENT                         
         DROP  R6                                                               
*                                                                               
         USING CMLBBEL,R6                                                       
RDCML80  GOTO1 ADDEL,DMCB,0(R4),X'08',18,CMLBBBCP                               
         B     RDCML30             BUMP TO NEXT ELEMENT                         
         DROP  R6                                                               
*                                                                               
RDCML90  TM    SBEFLAG4,SBEADID    DID WE WANT THE AD-ID?                       
         BZ    RDCML95             NO                                           
         CLI   HALF,0              YES - DID WE FIND AN AD-ID ELEMENT?          
         BE    RDCML95             YES                                          
         L     R6,AIO              NO - ADD THE ISCII INSTEAD                   
         USING CMLRECD,R6                                                       
         GOTO1 ADDEL,DMCB,0(R4),X'01',10,CMLKCML                                
         DROP  R6                                                               
*                                                                               
RDCML95  LPR   R3,R3               ANY CLASS SPLIT ELEMS?                       
         BZ    RDCML100            NO                                           
         MHI   R3,L'CMLCLS         EACH CLASS SPLIT IS 4 BYTES                  
         AHI   R3,2                ADD 2 BYTES FOR ELEMENT OVERHEAD             
         OC    CLSLIST,BLANKS      SPACE PAD                                    
         GOTO1 ADDEL,DMCB,0(R4),X'02',(R3),CLSLIST                              
*                                                                               
RDCML100 OI    TSARFLAG,TSARCMML   ADDING A COMMERCIAL RECORD                   
         BRAS  RE,TSARADD          ADD THE COMMERCIAL RECORD                    
         MVI   CMLFLAG,C'Y'        COMMERCIAL ADDED                             
*                                                                               
RDCMLXIT MVC   SYSDIR,=C'SPTDIR  ' SET DIRECTORY TO SPOT                        
         MVC   SYSFIL,=C'SPTFILE ' SET FILE TO SPOT                             
         CLI   CMLFLAG,C'Y'        COMMERCIAL ADDED = CC EQU                    
         J     XIT                 YES                                          
*                                                                               
ADDCLASS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SYSDIR,=C'TRFDIR  ' SET DIRECTORY TO TRAFFIC                     
         MVC   SYSFIL,=C'TRFFILE ' SET FILE TO TRAFFIC                          
*                                                                               
         MVC   AIO,AIO2            READ THE RECORD INTO AIO2                    
         MVI   CMLFLAG,C'N'        COMMERCIAL CLASS ADDED = CC NEQ              
*                                                                               
         LA    R4,KEY              READ COMMERCIAL CLASS RECORD                 
         USING CLSRECD,R4                                                       
         XC    CLSKEY,CLSKEY                                                    
         MVC   CLSKID(2),=X'0A44'  0A44 RECORD                                  
         MVC   CLSKAM,SBBAGYMD     A/M                                          
         MVC   CLSKCLAS,0(R5)      4 BYTES OF CLASS                             
         OC    CLSKCLAS,BLANKS     SPACE PAD IT                                 
         MVC   CLSKCLT,SBBCLT      CLIENT                                       
         MVC   CLSKPROD,SBPRD      PRODUCT                                      
         OC    CLSKPROD,BLANKS     SPACE PAD IT                                 
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         CLC   CLSKEY,KEYSAVE      PRD SPECIFIC FOUND?                          
         BE    ACLASS05            YES                                          
*                                                                               
         MVC   KEY,KEYSAVE         NO-TRY WITHOUT PRODUCT                       
         XC    CLSKPROD,CLSKPROD   CLEAR THE PRODUCT                            
         GOTO1 HIGH                READ HIGH                                    
         CLC   CLSKEY,KEYSAVE      CLT SPECIFIC FOUND?                          
         BE    ACLASS05            YES                                          
*                                                                               
         MVC   KEY,KEYSAVE         NO-TRY WITHOUT CLT                           
         XC    CLSKCLT,CLSKCLT     CLEAR THE CLIENT                             
         GOTO1 HIGH                READ HIGH                                    
         CLC   CLSKEY,KEYSAVE      CLASS SPECIFIC FOUND?                        
         BNE   ACLASSX             NO - DONE CC=NEQ                             
*                                                                               
ACLASS05 GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R6,AIO              A(CLASS RECORD)                              
         MVI   ELCODE,X'10'        LOOK FOR DESCRIPTION ELEMENT                 
         BRAS  RE,GETEL            HAVE DESCRIPTION ELEMENT?                    
         JNE   ACLASSX             NO                                           
*                                                                               
         USING CLSDSCEL,R6                                                      
         MVI   ELEM2+1,TSA2KEYL+26 ELEMENT LENGTH (KEY+2+DATA)                  
         LA    R1,ELEM2+TSA2KEYL+2 A(DATA)                                      
         MVC   0(24,R1),CLSDESC    CLASS NAME                                   
         OI    TSARFLAG,TSARCMML   ADDING A COMMERCIAL RECORD                   
         BRAS  RE,TSARADD          TSAR OFF THE CLASS                           
         MVI   CMLFLAG,C'Y'        COMMERCIAL ADDED = CC EQU                    
         DROP  R4,R6                                                            
*                                                                               
ACLASSX  MVC   SYSDIR,=C'SPTDIR  ' SET DIRECTORY TO SPOT                        
         MVC   SYSFIL,=C'SPTFILE ' SET FILE TO SPOT                             
         CLI   CMLFLAG,C'Y'        COMMERCIAL ADDED = CC EQU                    
         J     XIT                 YES                                          
*                                                                               
ADDEL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,12(R1)           ELEMENT DATA TO ADD                          
         LLC   R6,11(R1)           LENGTH OF ELEMENT                            
         SHI   R6,3                2 BYTES OF OVERHEAD -1 FOR EX                
         EX    R6,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),0(R5)       ** DATA IS BINARY ZEROES? **                 
         JZ    XIT                 YES - EXIT                                   
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),BLANKS      ** DATA IS SPACES? **                        
         JE    XIT                 YES - EXIT                                   
*                                                                               
         L     R4,0(R1)            ADD ELEMENT HERE                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),0(R5)       ** MOVE DATA **                              
*                                                                               
         AHI   R6,3                ADD BACK FULL ELEM LENGTH                    
         MVC   0(1,R4),4(R1)       ELEMENT CODE                                 
         STC   R6,1(R4)            ELEMENT LENGTH                               
         XR    R3,R3               CLEAR R3                                     
         ICM   R3,3,ELEM2          CURRENT RECORD LENGTH                        
         AR    R3,R6               ADD ELEM LENGTH TO RECORD LENGTH             
         STCM  R3,3,ELEM2          NEW RECORD LENGTH                            
         AR    R4,R6               NEXT ELEMENT OR EOR GOES HERE                
*                                                                               
         XIT1  REGS=(R4)                                                        
***********************************************************************         
*              DELETE INVOICE RECORDS FROM THE TSAR BUFFER            *         
***********************************************************************         
TSARDEL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
TDEL05   CLI   BYTE,0              DID WE JUST LOOK UP THE LAST INV?            
         BE    TDELXIT             YES, NOTHING TO DELETE                       
*                                                                               
         LLC   R1,BYTE             LOOK FOR THE NEXT BUFFERED INV               
         BCTR  R1,0                                                             
         STC   R1,BYTE                                                          
*                                                                               
         MVC   MYKEY+19(1),BYTE                                                 
*                                                                               
         L     R0,NINVREC          CLEAR TO BUILD TSAR BUFFER                   
         LAY   R1,6000                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,NINVREC          BUILD RECORD HERE                            
         MVC   2(20,R1),MYKEY                                                   
         MVC   WORK(L'MYKEY),MYKEY SAVE OFF THE KEY                             
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         ST    R1,TSAREC                                                        
         GOTO1 ATSAROF,(R2)                                                     
         TM    TSERRS,TSEEOF       DID WE HIT EOF?                              
         BO    TDEL05              YES, SET CC NOT EQU                          
*                                                                               
         CLC   MYKEY,WORK          DID THE KEY WE FIND MATCH?                   
         BNE   TDEL05              NO                                           
*                                                                               
         MVI   TSOFFACT,TSADEL     DELETE THIS RECORD                           
         GOTO1 ATSAROF,(R2)        DELETE THIS INVOICE RECORD                   
         B     TDEL05                                                           
*                                                                               
TDELXIT  J     XIT                                                              
*        DROP  R2                                                               
         EJECT                                                                  
*                                                                               
TSARADD  NTR1  BASE=*,LABEL=*                                                   
         TM    TSARFLAG,TSARCMML   ADDING COMMERCIAL RECORD?                    
         BZ    TADD00              NO                                           
         LA    R2,TSAREA2                                                       
         USING TSARD,R2                                                         
         LA    R4,ELEM2            A(REC) MUST BEGIN AT LENGTH                  
         B     TADD20                                                           
*                                                                               
TADD00   LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         XR    R1,R1                                                            
         ICM   R1,3,THISRECL                                                    
         AHI   R1,TSARKEYL+2                                                    
         STCM  R1,3,THISRECL                                                    
         TM    TSARFLAG,TSARNINV   ADDING NINV ENTRY?                           
         BZ    TADD10              NO                                           
         L     R4,NINVREC          BUILD THE KEY + DATA HERE                    
         MVC   0(2,R4),THISRECL                                                 
         B     TADD20                                                           
*                                                                               
TADD10   MVC   MYREC-2(2),THISRECL 2-BYTE VARIABLE LENGTH                       
         LA    R4,MYREC-2          A(REC) MUST BEGIN AT LENGTH                  
TADD20   ST    R4,TSAREC           A(VARIABLE LENGTH RECORD)                    
*                                                                               
         MVI   TSOFFACT,TSAADD     SET TO ADD A RECORD                          
         GOTO1 ATSAROF,(R2)        ADD THE RECORD                               
         MVI   TSARFLAG,0                                                       
         CLI   TSERRS,0            ANY ERRORS?                                  
         JE    XIT                 NO                                           
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         JE    XIT                 YES, IGNORE IT                               
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
TSARWRT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSAWRT     SET TO WRITE BACK A RECORD                   
         XR    R1,R1                                                            
         ICM   R1,3,THISRECL                                                    
         AHI   R1,TSARKEYL+2                                                    
         STCM  R1,3,THISRECL                                                    
         MVC   MYREC-2(2),THISRECL 2-BYTE VARIABLE LENGTH                       
         LA    R4,MYREC-2          A(REC) MUST BEGIN AT LENGTH                  
         ST    R4,TSAREC           A(VARIABLE LENGTH RECORD)                    
         GOTO1 ATSAROF,(R2)        WRITE BACK THE RECORD                        
         CLI   TSERRS,0            ANY ERRORS?                                  
         JE    XIT                 NO                                           
         DC    H'0'                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
TSARHIGH NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    TSARFLAG,TSARCMML   READING COMMERCIAL RECORDS?                  
         BZ    TSARH05             NO                                           
         MVC   WORK(TSA2KEYL),ELEM2+2  SAVE OFF THE KEY                         
         LA    R2,TSAREA2          2ND TSAR AREA FOR CML RECS ONLY!             
         LA    R1,ELEM2            A(CML KEY/RECORD)                            
         B     TSARH10                                                          
*                                                                               
TSARH05  MVC   WORK(L'MYKEY),MYKEY SAVE OFF THE KEY                             
         LA    R2,TSAREA                                                        
         L     R1,NINVREC                                                       
         TM    TSARFLAG,TSARNINV   PROCESSING NINV RECORD?                      
         BNZ   TSARH10             YES                                          
         L     R1,SBATSATB                                                      
         MVC   0(MYRECL+2,R1),MYKEY-2                                           
*                                                                               
         USING TSARD,R2                                                         
TSARH10  ST    R1,TSAREC                                                        
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         GOTO1 ATSAROF,(R2)                                                     
         TM    TSERRS,TSEEOF       DID WE HIT EOF?                              
         JO    TSARHNEQ            YES, SET CC NOT EQU                          
*                                                                               
         TM    TSARFLAG,TSARNINV   PROCESSING NINV RECORD?                      
         BZ    TSARH15             NO                                           
         L     R1,NINVREC          A(INVOICE RECORD)                            
         CLC   2(20,R1),WORK       MATCH ON KEY?                                
         B     TSARH21                                                          
*                                                                               
TSARH15  TM    TSARFLAG,TSARCMML   READING COMMERCIAL RECORDS?                  
         BZ    TSARH16             NO                                           
         NI    TSARFLAG,X'FF'-TSARCMML TURN THE CML FLAG OFF                    
         CLC   ELEM2+2(TSA2KEYL),WORK  MATCH ON KEY?                            
         B     TSARH21                                                          
*                                                                               
TSARH16  L     R1,SBATSATB                                                      
         MVC   MYKEY-2(MYRECL+2),0(R1)                                          
*                                                                               
TSARH20  CLC   MYKEY,WORK          DID THE KEY WE FIND MATCH?                   
TSARH21  JNE   NEXIT               NO                                           
         J     EQXIT               YES                                          
*                                                                               
TSARHNEQ NI    TSARFLAG,X'FF'-TSARCMML TURN THE CML FLAG OFF                    
         J     NEXIT                                                            
         DROP  R2                                                               
*                                                                               
XSPL40   NTR1  BASE=*,LABEL=*      *** GET SPILL MARKETS' NAMES ***             
         USING XSPILLD,R3                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKTRECD,R4                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(MKTKEYLN-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,SBQMED                                                   
         MVC   MKTKAGY,AGENCY                                                   
         MVC   KEYSAVE,KEY                                                      
         L     R3,SBAXSPIL                                                      
*                                                                               
XSPL42   LA    R5,XSMKTS           LOOP THROUGH SPILL MARKETS                   
         LA    R0,XSNMKTS                                                       
*                                                                               
XSPL44   OC    0(4,R5),0(R5)                                                    
         BZ    XSPL46                                                           
         SR    RE,RE                                                            
         ICM   RE,3,0(R5)          AGENCY MARKET                                
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTKMKT,DUB                                                      
         L     R4,SBAIO2                                                        
         ST    R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   4(24,R5),MKTNAME    SAVE MARKET NAME                             
         MVC   KEY,KEYSAVE                                                      
         LA    R4,KEY                                                           
         LA    R5,L'XSMKTS(R5)     NEXT SPILL MARKET                            
         BCT   R0,XSPL44                                                        
*                                                                               
XSPL46   LA    R3,XSPILLL(R3)      NEXT STATION                                 
         BCT   R2,XSPL42                                                        
         J     XIT                                                              
         DROP  R3                                                               
*                                                                               
GETLONG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* INPUT  : R2=A(FIELD ON FIRST LINE)                                            
*          R3=MAX WIDTH                                                         
*          R4=NUMBER OF LINES                                                   
* OUTPUT : FULL=WIDEST FOUND                                                    
*                                                                               
GETLONG2 ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         JZ    XIT                                                              
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
*                                                                               
GETLONG4 CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         JH    XIT                                                              
         A     R1,PWIDTH           ON EACH OF THE LINES                         
         BCT   R0,GETLONG4                                                      
         BCTR  R3,0                                                             
         B     GETLONG2                                                         
*                                                                               
SHUFFLE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* INPUT  : R2=A(START DATA ON FIRST LINE)                                       
*          R3=A(FROM DATA)                                                      
*          R4=NUMBER OF LINES                                                   
*                                                                               
         LR    RE,R3                                                            
         SR    RE,R2                                                            
         JNP   XIT                                                              
         BCTR  RE,0                                                             
SHUFFLE2 MVC   0(50,R2),0(R3)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   50(0,R2),BLANKS                                                  
         A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
         BCT   R4,SHUFFLE2                                                      
         J     XIT                                                              
*                                                                               
VGTACOM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING COMHDRD,R4                                                       
         MVC   COMKTYPE,=X'0D0C'   X'0D0C'                                      
         MVC   COMKAGY,SBBAGYMD    A/M                                          
         MVI   COMCTYPE,C'1'       ACOM = C'1'                                  
         MVC   COMKCLT,SBBCLT      CLIENT                                       
*                                                                               
GETAC10  GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     FOUND ACOM RECORD?                           
         BE    GETAC20             YES                                          
         OC    KEYSAVE+4(2),KEYSAVE+4  LOOKED UP ALL CLIENT ACOM?               
         JZ    NEXIT                   YES - DONE                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEYSAVE                                                  
         XC    COMKCLT,COMKCLT     LOOK UP ALL CLIENT ACOM                      
         B     GETAC10                                                          
*                                                                               
GETAC20  MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO1                                                          
*                                                                               
         ICM   R0,15,ACOMREC       MOVE COMMENTS TO ACOMREC                     
         JZ    NEXIT                                                            
*                                                                               
         LAY   R1,6000             SET ACOMREC LENGTH                           
         XR    RE,RE               SET DUMMY FROM ADDRESS                       
         LA    RF,X'40'            SET PADDING CHARACTER                        
         SLL   RF,24               SHIFT TO PADDING POSITION                    
         MVCL  R0,RE               BLANK OUT THE AREA                           
*                                                                               
         ICM   R1,15,ACOMREC       MOVE COMMENTS TO ACOMREC                     
*                                                                               
         LLC   RF,LASTHEAD         NUMBER OF HEADLINES                          
         STC   RF,4(R1)                                                         
         CLI   WIDTHOPT,C'W'       WIDE OPTION?                                 
         BNE   GETAC21             NO                                           
         LA    R4,198              YES - 192 CHAR PRINT LINES                   
         MHI   RF,198              ACOM RECORD STARTS HERE                      
         B     GETAC22                                                          
*                                                                               
GETAC21  LA    R4,132              132 CHAR PRINT LINES                         
         MHI   RF,132              ACOM RECORD STARTS HERE                      
*                                                                               
GETAC22  LA    R1,6(RF,R1)         START COMMENTS HERE                          
                                                                                
         XR    R3,R3               R3 = NUMBER OF ACOM LINES                    
*                                                                               
         MVI   ELCODE,X'05'        LOOK FOR X'05' COMMENT ELEMS                 
         BRAS  RE,GETEL                                                         
         B     *+8                 HAVE A X'05' ELEMENT?                        
*                                                                               
GETAC25  BRAS  RE,NEXTEL           HAVE ANOTHER X'05' ELEMENT?                  
         BNE   GETAC30             NO - DONE                                    
*                                                                               
         LLC   R2,1(R6)            ELEMENT LENGTH                               
         SHI   R2,3                -2 FOR ELEM OVERHEAD AND -1 FOR EX           
         BM    GETAC25             MUST HAVE AT LEAST ONE BYTE OF DATA          
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    2(0,R6),2(R6)       COMMENT IS ZEROES?                           
         BZ    GETAC25             YES - IGNORE                                 
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),2(R6)       MOVE COMMENT TO ACOMREC                      
*                                                                               
         AR    R1,R4               CONTINUE ACOM HERE                           
         AHI   R3,1                ADD 1 COMMENT LINE                           
         B     GETAC25                                                          
*                                                                               
GETAC30  LTR   R3,R3               ACOM LINES=0?                                
         JZ    NEXIT               YES - RETURN CC=NEQ                          
*                                                                               
         L     R1,ACOMREC                                                       
         AHI   R3,14               ADD 14 HEADLINES                             
         STC   R3,5(R1)            STORE THE TOTAL NUMBER OF LINES              
         J     EQXIT               RETURN CC=EQU                                
                                                                                
         EJECT                                                                  
       ++INCLUDE SPWRIWORKD                                                     
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*        INCLUDE DDSPOOLD                                                       
*        INCLUDE DDSPLWORKD                                                     
*        INCLUDE SPWRIFFD                                                       
*        INCLUDE SPWRIF1D                                                       
*        INCLUDE DDGENTWA                                                       
*        INCLUDE SPGENAGY                                                       
*        INCLUDE SPGENPRG                                                       
*        INCLUDE SPGENEST                                                       
*        INCLUDE SPGENMKG                                                       
*        INCLUDE SPGENMKT                                                       
*        INCLUDE SPGENSTA                                                       
*        INCLUDE SPGENGRP                                                       
*        INCLUDE SPGENXSDF                                                      
*        INCLUDE SPTRCMML                                                       
*        INCLUDE SPTRCMLCLS                                                     
*        INCLUDE SPSTABLK                                                       
*        INCLUDE CTGENFILE                                                      
*        INCLUDE FAFACTS                                                        
*        INCLUDE FATIOB                                                         
*        INCLUDE DDCOMFACS                                                      
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE DDBIGBOX                                                       
*        INCLUDE DDWIDED                                                        
*        INCLUDE DDOFFICED                                                      
*        INCLUDE DRONEBLKHD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPWRIFFD                                                       
         ORG CONTAGH                                                            
       ++INCLUDE SPWRIF1D                                                       
       ++INCLUDE DDGENTWA                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENPRG                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENMKG                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE SPGENXSDF                                                      
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPTRCMLCLS                                                     
*PREFIX=S                                                                       
       ++INCLUDE SPSTABLK                                                       
*PREFIX=                                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DRONEBLKHD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPADBUYER                                                      
       ++INCLUDE SPGENSNV                                                       
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
SPGBFRDD DSECT                                                                  
       ++INCLUDE SPGETBFRD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050SPWRI0B   06/17/19'                                      
         END                                                                    
