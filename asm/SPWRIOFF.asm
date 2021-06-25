*          DATA SET SPWRIOFF   AT LEVEL 020 AS OF 08/27/02                      
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*                                                                   *           
*  THIS MODULE IS DEAD.  IT HAS BEEN REPLACED BY SPWRI0B            *           
*        IT IS HERE FOR REFERENCE ONLY!                             *           
*                                                                   *           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*********************************************************************           
*PHASE T00B52A,*                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'T00B52 - SPOTPAK WRITER OFFLINE GENERAL ROUTINES'               
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRIOFF (T00B52) - SPOT WRITER OFFLINE GENERAL ROUTINES *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
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
         PRINT NOGEN                                                            
T00B52   CSECT                                                                  
         REQUS                                                                  
         USING *,RF                                                             
OFF      NTR1  ,                                                                
         DROP  RF                                                               
         LR    RB,RF                                                            
         USING T00B52,RB,RA,R7                                                  
         B     *+12                                                             
         DC    CL8'**GENB**'                                                    
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R7,2048(RA)                                                      
         LA    R7,2048(R7)                                                      
*                                                                               
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         LA    R8,4095(R9)                                                      
         LA    R8,1(R8)                                                         
         USING SYSD,R9,R8                                                       
*                                                                               
         LR    RF,RB                                                            
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
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
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
         ZIC   R4,GLFHEADL                                                      
         DROP  R1                                                               
         SH    R4,=H'5'                                                         
         TM    OUTIND,OUTIHEAD     OPTION TO SHUFFLE EXTRA HEADLINE             
         BO    *+6                                                              
         BCTR  R4,0                                                             
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,1(R2)                                                         
         CLC   FULL,=F'15'                                                      
         BNL   *+8                                                              
         LA    R2,1(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         LA    R3,12                                                            
         BAS   RE,GETLONG                                                       
         LA    R3,13(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
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
GETLONG  NTR1                                                                   
*                                                                               
* INPUT  : R2=A(FIELD ON FIRST LINE)                                            
*          R3=MAX WIDTH                                                         
*          R4=NUMBER OF LINES                                                   
* OUTPUT : FULL=WIDEST FOUND                                                    
*                                                                               
GETLONG2 ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
*                                                                               
GETLONG4 CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         BH    XIT                                                              
         A     R1,PWIDTH           ON EACH OF THE LINES                         
         BCT   R0,GETLONG4                                                      
         BCTR  R3,0                                                             
         B     GETLONG2                                                         
         SPACE 2                                                                
SHUFFLE  NTR1                                                                   
*                                                                               
* INPUT  : R2=A(START DATA ON FIRST LINE)                                       
*          R3=A(FROM DATA)                                                      
*          R4=NUMBER OF LINES                                                   
*                                                                               
         LR    RE,R3                                                            
         SR    RE,R2                                                            
         BNP   XIT                                                              
         BCTR  RE,0                                                             
SHUFFLE2 MVC   0(50,R2),0(R3)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   50(0,R2),BLANKS                                                  
         A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
         BCT   R4,SHUFFLE2                                                      
         B     XIT                                                              
         EJECT                                                                  
* SET UP DATE BUFFERS                                                           
*                                                                               
VSETDATE MVC   DATEFORM,SBSPPROF+2  PICK UP DATE FORM FROM SPOT PROFILE         
         NI    DATEFORM,X'0F'                                                   
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
         ZIC   R5,SBSPPROF+8       FOUND ONE - USE THAT                         
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
         ZIC   R5,SBSPPROF+8       START DAY FOUND IN PROFILE                   
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
         L     R4,AMONTHS          BUILD MONTHS                                 
         LA    RE,SBQSTART                                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,NMONTHS-1                                                   
         TM    SBQPER,SBQPBIG      EXTENDED MONTH TAB?  (WRI25)                 
         BZ    *+8                                                              
         MVI   DMCB,48                                                          
         GOTO1 MOBILE,DMCB,,(DATEFORM,(R4)),WORK,SBSPPROF                       
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
         CLI   SBQSPQRT,C'Y'                                                    
         BE    SD39                                                             
         LA    R3,NQTRS                                                         
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
         ZIC   RE,DUB+1            GET END DATE MONTH                           
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
         ZIC   RE,SBSPPROF+6                                                    
         STC   RE,HALF                                                          
         LA    RE,6(RE)                                                         
         CH    RE,=H'12'                                                        
         BNH   *+8                                                              
         SH    RE,=H'12'                                                        
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
SD56     ZIC   R3,2(R4)            SET INITIAL YEAR                             
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
         CH    R1,=H'2'            MAKE SURE THERE'S NOT 2 YEAR CHANGES         
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
         ZIC   RF,DUB+1                                                         
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
         LA    R4,WORK             R1=A(CLIENT GROUP NAME)                      
         MVI   0(R4),NPCGRCDQ                                                   
         MVI   1(R4),NPCGRLNQ                                                   
         MVC   2(2,R4),SBBCGR                                                   
         MVC   4(24,R4),0(R1)                                                   
         B     PUTBFEL                                                          
*                                                                               
VPUTCLTN LA    R4,WORK             PUT CLIENT                                   
         MVI   0(R4),NPCLTCDQ                                                   
         MVI   1(R4),NPCLTLNQ                                                   
         MVC   2(3,R4),SBCLT                                                    
         MVC   5(20,R4),SBCLTNM                                                 
         B     PUTBFEL                                                          
*                                                                               
VPUTESTN LA    R4,WORK             PUT ESTIMATE                                 
         MVI   0(R4),NPESTCDQ                                                   
         MVI   1(R4),NPESTLNQ                                                   
         MVC   2(1,R4),SBMED                                                    
         MVC   3(3,R4),SBCLT                                                    
*                                                                               
         CLC   AGENCY,=C'JM'       COKE?                                        
         BE    *+14                 YES                                         
         CLC   AGENCY,=C'CK'       COKE?                                        
         BNE   VPUTEST2             NO                                          
         OC    SBQBCLT,SBQBCLT     TEST MULTI CLIENT REQUEST                    
         BNZ   VPUTEST2             NO                                          
         BAS   RE,LGETESTN         SEE IF ALREADY THERE                         
         BE    XIT                  YES                                         
         MVC   3(3,R4),=C'CC '                                                  
*                                                                               
VPUTEST2 MVC   6(1,R4),SBBPRD                                                   
         MVC   7(1,R4),SBBEST                                                   
         MVC   8(6,R4),SBESTDEM                                                 
         MVC   14(2,R4),SBESTSTP                                                
         MVC   16(2,R4),SBESTNDP                                                
         TM    DATAIND3,DIESTNM    TEST EST NAME NEEDED                         
         BZ    *+14                                                             
         MVC   18(20,R4),SBESTNM   YES                                          
         B     PUTBFEL                                                          
         LA    RE,NPESTLNQ         NO-THE ESTIMATE DATES ARE ENOUGH             
         SH    RE,=H'20'                                                        
         STC   RE,1(R4)                                                         
         B     PUTBFEL                                                          
*                                                                               
VPUTMKTN LA    R4,WORK             PUT MARKET                                   
         XC    WORK,WORK                                                        
         MVI   0(R4),NPMKTCDQ                                                   
         MVI   1(R4),NPMKTLNQ                                                   
         MVC   2(1,R4),SBQMED      MEDIA                                        
         CLI   SBQMED,C'*'                                                      
         BNE   *+10                                                             
         MVC   2(1,R4),SBMED                                                    
         MVC   3(2,R4),SBBMKT      MARKET                                       
         MVC   5(24,R4),SBMKTNM    NAME                                         
         LA    RE,SBMKTREC                                                      
         USING MKTRECD,RE                                                       
*                                                                               
         CLI   MKTRS1,C'0'                                                      
         BNE   *+10                                                             
         MVC   29(2,R4),MKTRSM1    NSI MARKET NUMBER                            
         CLI   MKTRS1,C'1'                                                      
         BNE   *+10                                                             
         MVC   31(2,R4),MKTRSM1    ARB MARKET NUMBER                            
         CLI   MKTRS2,C'0'                                                      
         BNE   *+10                                                             
         MVC   29(2,R4),MKTRSM2                                                 
         CLI   MKTRS2,C'1'                                                      
         BNE   *+10                                                             
         MVC   31(2,R4),MKTRSM2                                                 
         MVC   SBBRSM0,29(R4)      NSI MARKET NUM                               
         MVC   SBBRSM1,31(R4)      ARB MARKET NUM                               
*                                                                               
         TM    SBQDEMOP,SBQDOMWZ                                                
         BO    *+10                                                             
         MVC   33(4,R4),=F'1'                                                   
         CLC   MKTWT,BLANKS        MARKET WEIGHT                                
         BNH   PUTBFEL                                                          
         PACK  DUB,MKTWT                                                        
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         STCM  R0,15,33(R4)                                                     
         B     PUTBFEL                                                          
         DROP  RE                                                               
*                                                                               
VPUTMGRN DS    0H                  PUT MARKET GROUP                             
         LA    R4,WORK             R1 = A(MKTGRP NAME)                          
         MVI   0(R4),NPMGRCDQ                                                   
         MVI   1(R4),NPMGRLNQ                                                   
         MVC   2(1,R4),SBMED                                                    
         MVC   3(2,R4),SBBMGR                                                   
         XC    5(2,R4),5(R4)                                                    
         OC    SBPGRPEX(2),SBPGRPEX   TEST PRODUCT GROUP EXCEPTION              
         BZ    *+10                                                             
         MVC   5(2,R4),SBBPGR                                                   
         MVC   7(24,R4),0(R1)                                                   
         B     PUTBFEL                                                          
*                                                                               
VPUTPGRN DS    0H                  PUT PRODUCT GROUP                            
         LA    R4,WORK             R1 = A(PRDGRP NAME)                          
         MVI   0(R4),NPPGRCDQ                                                   
         MVI   1(R4),NPPGRLNQ                                                   
         MVC   2(2,R4),SBBPGR                                                   
         MVC   4(24,R4),0(R1)                                                   
         B     PUTBFEL                                                          
*                                                                               
VPUTSGRN DS    0H                  PUT STATION GROUP                            
         LA    R4,WORK             R1=A(STATION GROUP NAME)                     
         MVI   0(R4),NPSGRCDQ                                                   
         MVI   1(R4),NPSGRLNQ                                                   
         MVC   2(2,R4),SBBSGR                                                   
         MVC   4(24,R4),0(R1)                                                   
         B     PUTBFEL                                                          
*                                                                               
VPUTREPN DS    0H                  PUT REP NAME                                 
         LA    R4,WORK                                                          
         MVI   0(R4),NPREPCDQ                                                   
         MVI   1(R4),NPREPLNQ                                                   
         MVC   2(3,R4),SBREP                                                    
         MVC   5(22,R4),SBREPNM                                                 
         B     PUTBFEL                                                          
*                                                                               
VPUTCSFL DS    0H                  PUT CHILD SPOT FLIGHT DATES                  
         LA    R4,WORK                                                          
         MVI   0(R4),NPFLTCDQ                                                   
         MVC   2(3,R4),SBPRD                                                    
         L     RE,SBADATE                                                       
         L     RF,SBNDATES                                                      
         STC   RF,5(R4)                                                         
         SLL   RF,2                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R4),0(RE)                                                    
         LA    RF,7(RF)                                                         
         STC   RF,1(R4)                                                         
         B     PUTBFEL                                                          
*                                                                               
VPUTCNT  L     RE,AGLOBAL          PUT A COUNT ELEMENT                          
         LA    RE,GLAIFLD-GLOBALD(RE)                                           
         L     R1,0(RE)            R1=A(ROW VALUE)                              
         MVC   WORK+34(30),0(R1)                                                
         TM    COUNTLEN,X'80'                                                   
         BZ    *+16                                                             
         MVC   WORK+34(2),SBBCLT                                                
         MVC   WORK+36(28),0(R1)                                                
         ZIC   RF,COUNTLEN                                                      
         SLL   RF,25               GET RID OF X'80' BIT                         
         SRL   RF,25                                                            
         CH    RF,=H'28'           CHECK ROW IS NO LONGER THAN 28               
         BNH   *+6                 (WORK IS ONLY 64 BYTES)                      
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         MVI   ELCODE,NPCNTCDQ                                                  
         BAS   RE,GETELBF                                                       
         BNE   VPUTCNT4                                                         
VPUTCNT2 EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   4(0,R4),0(R1)                                                    
         BNE   *+14                                                             
         MVC   2(2,R4),TOTLEVS                                                  
         B     XIT                                                              
         BAS   RE,NXTELBF                                                       
         BE    VPUTCNT2                                                         
VPUTCNT4 LA    R4,WORK                                                          
         MVI   0(R4),NPCNTCDQ                                                   
         LA    RE,5(RF)                                                         
         STC   RE,1(R4)                                                         
         MVC   2(2,R4),TOTLEVS                                                  
         EX    RF,*+8                                                           
         B     PUTBFEL                                                          
         MVC   4(0,R4),WORK+34                                                  
*                                                                               
VPUTCBLC DS    0H                  PUT/ADD TO A CABLE SYSTEM COUNT ELEM         
         XC    WORK,WORK                                                        
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
         MVC   WORK+2(3),SBCLT                                                  
*                                                                               
         TM    ROWIND2,ROWIPRD                                                  
         BZ    VPUTCBL1                                                         
         ZIC   R1,SBBPRD                                                        
*        CLC   SBQPRD,=C'POL'                                                   
*        BE    VPUTCBL0            IF REQ IS POL, USE POL PRD                   
*        CLI   SBBPRD,X'FE'        IF POL OR UNAL, USE MASPRD, SINCE            
*        BE    *+12                 NOT BROKEN OUT BY SPOTBUY YET               
*        CLI   SBBPRD,X'FF'                                                     
*        BNE   *+8                                                              
*        IC    R1,SBBMPRD                                                       
VPUTCBL0 STC   R1,WORK+5                                                        
*                                                                               
VPUTCBL1 TM    ROWIND2,ROWIEST                                                  
         BZ    VPUTCBLA                                                         
         CLC   SBQEST,SBQESTND     SINGLE EST REQ                               
         BE    *+12                 NO                                          
         CLI   SBQSEPES,C'Y'       ESTIMATES SEPERATE?                          
         BNE   VPUTCBLA             NO                                          
         MVC   WORK+6(1),SBBEST                                                 
*                                                                               
VPUTCBLA LA    R1,SBAGYREC                                                      
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'      TEST CANADIAN                                
         BNE   VPUTCBLB                                                         
         MVC   WORK+9(L'SBNETWK),SBNETWK                                        
         B     VPUTCBLD                                                         
VPUTCBLB MVC   WORK+7(2),SBBMKT                                                 
         MVC   WORK+9(L'SBCBLNET),SBCBLNET                                      
         DROP  R1                                                               
*                                                                               
VPUTCBLD MVI   ELCODE,NPCBLCDQ                                                  
         BAS   RE,GETELBF                                                       
         B     *+8                                                              
         BAS   RE,NXTELBF                                                       
         BNE   VPUTCBL2                                                         
         CLC   WORK+2(11),2(R4)     SAME CLT/PRD/EST/MKT/NETWORK?               
         BNE   *-14                                                             
         SPACE                                                                  
*                                                                               
         ZIC   RF,SBBPRD           SEE IF WE'VE SEEN THIS PRD...                
         LA    RF,PRDTAB(RF)       ...FOR THIS STATION                          
         CLI   0(RF),0                                                          
         BNE   XIT                  YES                                         
         MVC   0(1,RF),SBBPRD       NO - SET YES FOR NEXT TIME                  
*                                                                               
         ZIC   R1,13(R4)           INCREMENT SYSTEM COUNTER                     
         LA    R1,1(R1)                                                         
         STC   R1,13(R4)                                                        
         B     XIT                                                              
         SPACE                                                                  
VPUTCBL2 LA    R4,WORK             ELEM DOESN'T EXIST - ADD ONE                 
         MVI   0(R4),NPCBLCDQ                                                   
         MVI   1(R4),NPCBLLNQ                                                   
         MVI   13(R4),1                                                         
*                                                                               
         ZIC   RF,SBBPRD           SET WE'VE SEEN THIS PRD FOR THIS STA         
         LA    RF,PRDTAB(RF)                                                    
         MVC   0(1,RF),SBBPRD                                                   
         B     PUTBFEL                                                          
*                                                                               
* P1 = COLUMN                                                                   
* P2 = DETAIL VALUE                                                             
VPUTTOT  DS    0H                  PUT/ADD TO A TOTAL COUNTER                   
         MVI   ELCODE,NPTOTCDQ                                                  
         BAS   RE,GETELBF                                                       
         B     *+8                                                              
         BAS   RE,NXTELBF                                                       
         BNE   VPUTTOT4                                                         
         CLC   0(1,R1),2(R4)       RIGHT COLUMN?  0(R1)=COLUMN LABEL            
         BNE   *-14                                                             
*                                                                               
         LA    R0,16               INC ALL ACCUMS                               
         LA    R4,3(R4)                                                         
         L     R2,4(R1)            R1=A(DETAIL BEING PUT OUT)                   
VPUTTOT2 L     RE,0(R4)                                                         
         AR    RE,R2                                                            
         ST    RE,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,VPUTTOT2                                                      
         B     XIT                                                              
*                                                                               
VPUTTOT4 LA    R0,16                                                            
         LA    R4,WORK+3                                                        
         L     R2,4(R1)            4(R1)=DETAIL VALUE                           
*                                                                               
         ST    R2,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,*-8                                                           
*                                                                               
         MVI   WORK,NPTOTCDQ                                                    
         MVI   WORK+1,NPTOTLNQ                                                  
         MVC   WORK+2(1),0(R1)     0(R1)=COLUMN LABEL                           
         LA    R4,WORK                                                          
         B     PUTBFEL                                                          
*                                                                               
VPUTGEN  LA    R4,WORK             PUT A GENERALIZED ELEMENT                    
         MVI   0(R4),NPGENCDQ      R1 POINTS TO AREA CONTAINING:                
         MVC   2(3,R4),0(R1)       +0 TYPE (1)                                  
         ZIC   RE,3(R1)            +1 SEQ NUM (2)                               
         BCTR  RE,0                +3 LENGTH OF DATA (1)                        
         EX    RE,*+8              +4 THE DATA                                  
         B     *+10                                                             
         MVC   5(0,R4),4(R1)                                                    
         LA    RE,6(RE)                                                         
         STC   RE,1(R4)                                                         
         B     PUTBFEL                                                          
*                                                                               
PUTBFEL  ZIC   RE,1(R4)                                                         
         ICM   R1,15,ANAMPOOL                                                   
         BZ    XIT                                                              
         L     R5,0(R1)                                                         
         L     RF,4(R1)                                                         
         SR    RF,RE                                                            
         BP    *+6                                                              
         DC    H'0'                NAME POOL OVERFLOW                           
         ST    RF,4(R1)                                                         
         L     RF,0(R1)                                                         
         LA    RF,0(RE,RF)                                                      
         ST    RF,0(R1)                                                         
         MVI   0(RF),0                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R5),0(R4)                                                    
*                                                                               
VGETCGRN DS    0H                  GET CLIENT GROUP                             
         MVI   ELCODE,NPCGRCDQ     R1 = A(NAME FIELD)                           
         BAS   RE,GETELBF                                                       
         BNE   VGETCGR2                                                         
         B     *+12                                                             
         BAS   RE,NXTELBF                                                       
         BNE   VGETCGR2                                                         
         CLC   SBBCGR,2(R4)                                                     
         BNE   *-14                                                             
         LTR   R1,R1                                                            
         BZ    EQXIT                                                            
         MVC   0(24,R1),4(R4)                                                   
         B     EQXIT                                                            
VGETCGR2 LTR   R1,R1                                                            
         BZ    NEXIT                                                            
         MVC   0(24,R1),=CL24'** UNKNOWN **'                                    
         B     NEXIT                                                            
*                                                                               
VGETCLTN MVC   SBCLTNM,BLANKS      GET CLIENT                                   
         MVI   ELCODE,NPCLTCDQ                                                  
         BAS   RE,GETELBF                                                       
         BNE   XIT                                                              
         B     *+12                                                             
         BAS   RE,NXTELBF                                                       
         BNE   XIT                                                              
         CLC   SBCLT,2(R4)                                                      
         BNE   *-14                                                             
         MVC   SBCLTNM,5(R4)                                                    
         B     XIT                                                              
*                                                                               
LGETESTN NTR1                                                                   
         B     GE01                                                             
VGETESTN MVC   SBESTNM,BLANKS      GET ESTIMATE                                 
         CLI   SBBEST,0                                                         
         BNE   GE01                                                             
         MVC   SBESTNM(7),=C'*EST 0*'                                           
         XC    SBESTDEM,SBESTDEM                                                
         XC    SBESTSTP,SBESTSTP                                                
         XC    SBESTNDP,SBESTNDP                                                
         CR    RE,RE                                                            
         B     XIT                                                              
GE01     MVI   ELCODE,NPESTCDQ                                                  
         BAS   RE,GETELBF                                                       
         BNE   XIT                                                              
         B     GE02+8                                                           
GE02     BAS   RE,NXTELBF                                                       
         BNE   XIT                                                              
         CLC   SBMED,2(R4)                                                      
         BNE   GE02                                                             
*                                                                               
         CLC   AGENCY,=C'JM'       COKE?                                        
         BE    *+14                 YES                                         
         CLC   AGENCY,=C'CK'       COKE?                                        
         BNE   GE03                 NO                                          
         OC    SBQBCLT,SBQBCLT     TEST MULTI CLIENT REQUEST                    
         BNZ   GE03                 NO                                          
         CLC   =C'CC ',3(R4)                                                    
         B     *+10                                                             
*                                                                               
GE03     CLC   SBCLT,3(R4)                                                      
         BNE   GE02                                                             
         CLC   SBBPRD,6(R4)                                                     
         BNE   GE02                                                             
         CLC   SBBEST,7(R4)                                                     
         BNE   GE02                                                             
         MVC   SBESTDEM,8(R4)                                                   
         MVC   SBESTSTP,14(R4)                                                  
         MVC   SBESTNDP,16(R4)                                                  
         CLI   1(R4),NPESTLNQ                                                   
         BL    *+10                                                             
         MVC   SBESTNM,18(R4)                                                   
         CR    RE,RE                                                            
         B     XIT                                                              
*                                                                               
VGETMKTN MVC   SBMKTNM,BLANKS      GET MARKET                                   
         MVI   ELCODE,NPMKTCDQ                                                  
         BAS   RE,GETELBF                                                       
         BNE   XIT                                                              
         B     *+12                                                             
*                                                                               
VGMKTN10 BAS   RE,NXTELBF                                                       
         BNE   XIT                                                              
         CLI   SBQMED,C'*'         FOR MEDIA * USE SBMED                        
         BNE   VGMKTN20                                                         
         CLC   SBMED,2(R4)                                                      
         BNE   VGMKTN10                                                         
         B     VGMKTN30                                                         
*                                                                               
VGMKTN20 CLC   SBQMED,2(R4)        ELSE USE SBQMED                              
         BNE   VGMKTN10                                                         
*                                                                               
VGMKTN30 CLC   SBBMKT,3(R4)                                                     
         BNE   VGMKTN10                                                         
*                                                                               
         MVC   SBMKTNM,5(R4)       NAME                                         
         MVC   SBBRSM0,29(R4)      NSI MARKET NUM                               
         MVC   SBBRSM1,31(R4)      ARB MARKET NUM                               
         MVC   SBMKTWGT,33(R4)     MARKET WEIGHT                                
         B     XIT                                                              
*                                                                               
VGETMGRN DS    0H                  GET MARKET GROUP                             
         MVI   ELCODE,NPMGRCDQ     R1 = A(NAME FIELD)                           
         BAS   RE,GETELBF                                                       
         BNE   VGETMGR6                                                         
         B     *+12                                                             
VGETMGR1 BAS   RE,NXTELBF                                                       
         BNE   VGETMGR6                                                         
         CLC   SBMED,2(R4)                                                      
         BNE   VGETMGR1                                                         
         CLC   SBBMGR,3(R4)                                                     
         BNE   VGETMGR1                                                         
         OC    SBPGRPEX(2),SBPGRPEX                                             
         BZ    VGETMGR5                                                         
         MVC   HALF,SBBPGR                                                      
         OC    HALF,SBPG1MSK                                                    
         LA    R0,SBEXMAX                                                       
         LA    RE,SBPGRPEX                                                      
VGETMGR2 OC    0(2,RE),0(RE)                                                    
         BZ    VGETMGR3                                                         
         MVC   FULL(2),0(RE)                                                    
         OC    FULL(2),SBPG1MSK                                                 
         CLC   HALF,FULL                                                        
         BE    VGETMGR4                                                         
         LA    RE,2(RE)                                                         
         BCT   R0,VGETMGR2                                                      
VGETMGR3 OC    5(2,R4),5(R4)                                                    
         BZ    VGETMGR5                                                         
         B     VGETMGR1                                                         
VGETMGR4 CLC   5(2,R4),0(RE)                                                    
         BNE   VGETMGR1                                                         
VGETMGR5 LTR   R1,R1                                                            
         BZ    EQXIT                                                            
         MVC   0(24,R1),7(R4)                                                   
         B     EQXIT                                                            
VGETMGR6 LTR   R1,R1                                                            
         BZ    NEXIT                                                            
         MVC   0(24,R1),=CL24'** UNKNOWN **'                                    
         B     NEXIT                                                            
*                                                                               
VGETPGRN DS    0H                  GET PRODUCT GROUP                            
         MVI   ELCODE,NPPGRCDQ     R1 = A(NAME FIELD)                           
         BAS   RE,GETELBF                                                       
         BNE   VGETPGR2                                                         
         B     *+12                                                             
         BAS   RE,NXTELBF                                                       
         BNE   VGETPGR2                                                         
         CLC   SBBPGR,2(R4)                                                     
         BNE   *-14                                                             
         LTR   R1,R1                                                            
         BZ    EQXIT                                                            
         MVC   0(24,R1),4(R4)                                                   
         B     EQXIT                                                            
VGETPGR2 LTR   R1,R1                                                            
         BZ    NEXIT                                                            
         MVC   0(24,R1),=CL24'** UNKNOWN **'                                    
         B     NEXIT                                                            
*                                                                               
VGETSGRN DS    0H                  GET STATION GROUP                            
         MVI   ELCODE,NPSGRCDQ     R1 = A(NAME FIELD)                           
         BAS   RE,GETELBF                                                       
         BNE   VGETSGR2                                                         
         B     *+12                                                             
         BAS   RE,NXTELBF                                                       
         BNE   VGETSGR2                                                         
         CLC   SBBSGR,2(R4)                                                     
         BNE   *-14                                                             
         LTR   R1,R1                                                            
         BZ    EQXIT                                                            
         MVC   0(24,R1),4(R4)                                                   
         B     EQXIT                                                            
VGETSGR2 LTR   R1,R1                                                            
         BZ    NEXIT                                                            
         MVC   0(24,R1),=CL24'** UNKNOWN **'                                    
         B     NEXIT                                                            
*                                                                               
VGETREPN DS    0H                                                               
         MVI   ELCODE,NPREPCDQ                                                  
         BAS   RE,GETELBF                                                       
         BNE   VGETREP2                                                         
         B     *+12                                                             
         BAS   RE,NXTELBF                                                       
         BNE   VGETREP2                                                         
         CLC   SBREP,2(R4)                                                      
         BNE   *-14                                                             
         MVC   SBREPNM,5(R4)                                                    
         B     EQXIT                                                            
VGETREP2 MVC   SBREPNM,=CL24'** UNKNOWN **'                                     
         B     NEXIT                                                            
*                                                                               
VGETCSFL MVI   ELCODE,NPFLTCDQ     GET CHILD SPOT FLIGHT INFO                   
         XC    SBADATE,SBADATE                                                  
         XC    SBNDATES,SBNDATES                                                
         BAS   RE,GETELBF                                                       
         BNE   NEXIT                                                            
         B     *+12                                                             
         BAS   RE,NXTELBF                                                       
         BNE   NEXIT                                                            
         CLC   SBPRD,2(R4)         MATCH THE PRODUCT                            
         BNE   *-14                                                             
         MVC   SBNDATES+3(1),5(R4)                                              
         LA    R4,6(R4)                                                         
         ST    R4,SBADATE          RETURN A(FLIGHT DATES)                       
         B     EQXIT                                                            
*                                                                               
VGETCBLC XC    WORK,WORK                                                        
         TM    ROWIND2,ROWICLT                                                  
         BZ    *+10                                                             
         MVC   WORK(3),SBCLT                                                    
         TM    ROWIND2,ROWIPRD                                                  
         BZ    *+10                                                             
         MVC   WORK+3(1),SBBPRD                                                 
*                                                                               
         TM    ROWIND2,ROWIEST                                                  
         BZ    VGETCBL2                                                         
         CLC   SBQEST,SBQESTND     SINGLE EST REQ                               
         BE    *+12                                                             
         CLI   SBQSEPES,C'Y'       ESTIMATES SEPERATE?                          
         BNE   VGETCBL2             NO                                          
         MVC   WORK+4(1),SBBEST                                                 
*                                                                               
VGETCBL2 LA    R1,SBAGYREC                                                      
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'      TEST CANADIAN                                
         BNE   VGETCBL3                                                         
         MVC   WORK+7(L'SBNETWK),SBNETWK                                        
         B     VGETCBL4                                                         
VGETCBL3 MVC   WORK+5(2),SBBMKT                                                 
         MVC   WORK+7(L'SBCBLNET),SBCBLNET                                      
         DROP  R1                                                               
*                                                                               
VGETCBL4 MVI   ELCODE,NPCBLCDQ                                                  
         BAS   RE,GETELBF                                                       
         B     *+8                                                              
         BAS   RE,NXTELBF                                                       
         BNE   BADCBCT                                                          
         CLC   WORK(11),2(R4)                                                   
         BNE   *-14                                                             
         SPACE                                                                  
         MVC   BYTE,13(R4)         GET SYSTEM COUNT                             
         B     EQXIT                                                            
BADCBCT  B     NEXIT                                                            
         SPACE 2                                                                
*                                                                               
* P1 = COLUMN                                                                   
* P2 = LEVEL                                                                    
* RETURNS P1 = TOTAL                                                            
VGETTOT  DS    0H                                                               
         MVI   ELCODE,NPTOTCDQ                                                  
         BAS   RE,GETELBF                                                       
         B     *+8                                                              
         BAS   RE,NXTELBF                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),2(R4)       RIGHT COLUMN? 3(R1)=COLUMN LABEL             
         BNE   *-16                                                             
*                                                                               
         ZIC   RE,4(R1)            7(R1)=LEVEL                                  
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,3(RE,R4)                                                      
         MVC   0(4,R1),0(RE)                                                    
         XC    0(4,RE),0(RE)       CLEAR ACCUM FOR THIS LEVEL                   
         B     EQXIT                                                            
*                                                                               
VGETGEN  MVI   ELCODE,NPGENCDQ     GET A GENERALIZED ELEMENT                    
         BAS   RE,GETELBF                                                       
         BNE   NEXIT                                                            
         B     *+12                                                             
         BAS   RE,NXTELBF                                                       
         BNE   NEXIT                                                            
         CLC   0(3,R1),2(R4)       MATCH TYPE AND SEQUENCE NUM                  
         BNE   *-14                                                             
         ZIC   RE,1(R4)                                                         
         SH    RE,=H'5'                                                         
         STC   RE,3(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R1),5(R4)                                                    
         B     EQXIT                                                            
*                                                                               
*                                                                               
GETELBF  SR    R0,R0                                                            
         L     R4,ANAMPOOL                                                      
         LA    R4,8(R4)                                                         
GETELBF2 CLI   0(R4),0                                                          
         BNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         CLC   ELCODE,0(R4)                                                     
         BER   RE                                                               
NXTELBF  ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     GETELBF2                                                         
         EJECT                                                                  
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
         ZIC   RE,BYTE                                                          
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
         ZIC   RF,1(R5)                                                         
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
         CH    R5,=H'1'                                                         
         BNE   DPTX                                                             
         LTR   R3,R3                                                            
         BNZ   DPTX                                                             
         XC    SBDPTGRP+1(3),SBDPTGRP+1                                         
*                                                                               
DPTX     B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO GET COMMERCIAL DETAILS                                             
* INPUT  : SBCMLSQ = COMMERCIAL SEQUENCE NUMBER                                 
*          R1=0 GET COMMERCIAL CODE, NAME, CLASS AND CLT NUM                    
*          R1=3 GET COMMERCIAL CODE, NAME2, CLASS AND CLT NUM                   
*          R1=4 GET COMMERCIAL CODE, NAME3, CLASS AND CLT NUM                   
*          R1=1 GET COMMERCIAL CLASS NAME IN ADDITION TO ABOVE                  
*          R1=2 GET CML CLASS NAMES FOR SPLIT CLASSES (HOME DEPOT)              
*             AND RETURN IN WORK.                                               
* OUTPUT : SBCMLCD                                                              
*          SBCMLNM                                                              
*          SBCMLCLS                                                             
*          COMMERCIAL CLASS NAME IS RETURNED IN WORK                            
*          COMMERCIAL CLT NO IS RETURNED IN BLOCK                               
*          COMMERCIAL BASIC CODE (SAG/AFTRA) RETURNED IN BLOCK+20(16)           
*                                                                               
VGETCML  STC   R1,BYTE                                                          
         BAS   RE,SETTRF           SET SYSTEM TO TRAFFIC                        
         MVC   SBCMLCD,=CL8'UNKNOWN'                                            
         MVC   BLOCK(36),BLANKS                                                 
         MVC   BLOCK(8),=CL8'UNKNOWN'                                           
         MVC   BLOCK+20(8),=CL8'UNKNOWN'                                        
         MVC   SBCMLNM,BLANKS                                                   
         MVC   SBCMLNM(8),SBCMLCD                                               
         MVC   SBCMLCLS,=C'ZZZZ'                                                
         MVC   WORK(24),BLANKS                                                  
         MVC   WORK(10),=C'UNASSIGNED'                                          
         MVI   WORK+24,1           DEFAULT TO 1 CLASS NAME                      
         ICM   R4,15,SBACMLTB      'UNKNOWN' IF NO COMMERCIAL TABLE             
         BZ    GETCMLX                                                          
         OC    SBCMLSQ,SBCMLSQ     OR CML=0                                     
         BZ    GETCMLX                                                          
         MVC   SBCMLCD,BLANKS      INITIALIZE TO NOT SET UP IN TRAFFIC          
         MVC   SBCMLCD(4),=C'*NT*'                                              
         MVC   SBCMLNM,BLANKS                                                   
         MVC   SBCMLNM(4),SBCMLCD                                               
         LM    R2,R3,0(R4)         N'RECORDS SO FAR & MAX RECORDS               
         LA    R4,8(R4)                                                         
         ST    R4,DMCB+4                                                        
         LA    R4,ELEM                                                          
         USING CMLTABD,R4                                                       
         MVC   CTNAME2,BLANKS                                                   
         MVC   CTNAME3,BLANKS                                                   
         MVC   CTCLT,SBBCLT                                                     
*                                                                               
         CLC   AGENCY,=C'JM'       COKE?                                        
         BE    *+14                 YES                                         
         CLC   AGENCY,=C'CK'       COKE?                                        
         BNE   *+10                 NO                                          
         MVC   CTCLT,=X'885F'       YES - USE CLT CC                            
*                                                                               
         MVC   CTSEQ,SBCMLSQ                                                    
         LA    R5,CMLTABL                                                       
         GOTO1 BINSRCH,DMCB,(0,ELEM),,(R2),(R5),(0,4),(R3)                      
         L     R4,0(R1)                                                         
         CLI   0(R1),0             TEST RECORD FOUND                            
         BE    GETCML6             YES                                          
         LA    R4,ELEM             NO-BUILD TABLE ENTRY                         
         LA    R5,KEY              READ COMMERCIAL RECORD                       
         USING CMLRECD,R5                                                       
         XC    CMLKEY,CMLKEY                                                    
         MVC   CMLPID(2),=X'0AA1'                                               
         MVC   CMLPAM,SBBAGYMD                                                  
         MVC   CMLPCLT,SBBCLT                                                   
*                                                                               
         OC    SBBCMCLT,SBBCMCLT   TEST MASTER TRAFFIC CLIENT CODE              
         BZ    *+10                                                             
         MVC   CMLPCLT,SBBCMCLT    YES-USE THAT                                 
*                                                                               
         CLC   AGENCY,=C'JM'       COKE?                                        
         BE    *+14                 YES                                         
         CLC   AGENCY,=C'CK'       COKE?                                        
         BNE   *+10                 NO                                          
         MVC   CMLPCLT,=X'885F'     YES - USE CLT CC                            
*                                                                               
         MVC   CMLPSEQ+1(2),SBCMLSQ                                             
         GOTO1 HIGH                                                             
         CLC   CMLKEY,KEYSAVE      TEST RECORD FOUND                            
         BNE   GETCML4                                                          
         L     R5,AIO3             YES-GET THE RECORD                           
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   SBCMLCD,CMLKCML     COMMERCIAL CODE                              
*                                                                               
         LA    R5,24(R5)                                                        
         SR    R0,R0                                                            
*                                                                               
         XC    CLSLIST,CLSLIST                                                  
         MVI   CLSNUM,0                                                         
         CLI   BYTE,2              HOME DEPOT CLASS SPLIT?                      
         BNE   GETCML3                                                          
         SR    R1,R1                                                            
         LA    RF,CLSLIST                                                       
*                                                                               
GETCML2  CLI   0(R5),0             LOOK FOR CMML CLASS ELEM                     
         BE    GETCML2B                                                         
         CLI   0(R5),X'21'                                                      
         BE    *+14                                                             
GETCML2A IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETCML2                                                          
*                                                                               
         USING CMLCLSEL,R5                                                      
         MVC   0(L'CMLCLS,RF),CMLCLS                                            
         LA    RF,L'CMLCLS(RF)                                                  
         BCT   R1,GETCML2A         COUNT OF SPLITS (NEGATIVE)                   
         DROP  R5                                                               
*                                                                               
GETCML2B LPR   R1,R1                                                            
         BZ    GETCML3             NO CLASS SPLIT ELEMS                         
         STC   R1,CLSNUM           STORE # ELEMS                                
*                                                                               
GETCML3  L     R5,AIO                                                           
         LA    R5,24(R5)                                                        
         SR    R0,R0                                                            
*                                                                               
GETCML3A CLI   0(R5),0             LOOK FOR COMMERCIAL NAME                     
         BE    GETCML4                                                          
         CLI   0(R5),X'10'                                                      
         BE    *+14                                                             
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETCML3A                                                         
         USING CMLDTAEL,R5                                                      
         MVC   SBCMLNM,CMLTITLE                                                 
         MVC   CTNUM,CMLCLTNO                                                   
         CLC   CMLCLASS,BLANKS     TEST CLASS DEFINED                           
         BNH   *+10                NO-CLASS=ZZZZ                                
         MVC   SBCMLCLS,CMLCLASS                                                
*                                  GET NAME 2 AND 3                             
GETCML3B IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0             LOOK FOR COMMERCIAL NAME 2 & 3               
         BE    GETCML4                                                          
         CLI   0(R5),X'30'                                                      
         BNE   GETCML3B                                                         
         USING CMLDSCEL,R5                                                      
         CLI   CMLDSCSQ,1          IS THIS NAME 2?                              
         BNE   *+14                 NO                                          
         MVC   CTNAME2,CMLDSC                                                   
         B     GETCML3B             NO                                          
*                                                                               
         CLI   CMLDSCSQ,2          IS THIS NAME 3?                              
         BNE   GETCML3B             NO                                          
         MVC   CTNAME3,CMLDSC                                                   
         DROP  R5                                                               
*                                                                               
GETCML4  L     R5,AIO                                                           
         AHI   R5,24                                                            
         SR    R0,R0                                                            
*                                                                               
GETCML4A CLI   0(R5),0             LOOK FOR COMMERCIAL NAME                     
         BE    GETCML5                                                          
         CLI   0(R5),X'90'                                                      
         BE    *+14                                                             
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GETCML4A                                                         
         MVC   CTBASCD,CMLBBBCP-CMLBBEL(R5)                                     
*                                                                               
GETCML5  MVC   CTCODE,SBCMLCD                                                   
         MVC   CTNAME,SBCMLNM                                                   
         MVC   CTCLASS,SBCMLCLS                                                 
         MVC   CTCLSPLT,CLSLIST                                                 
         MVC   CTCLNUM,CLSNUM                                                   
         L     R4,SBACMLTB         ADD TO TABLE                                 
         LA    R4,8(R4)                                                         
         LA    R5,CMLTABL                                                       
         GOTO1 BINSRCH,DMCB,(1,ELEM),(R4),(R2),(R5),(0,4),(R3)                  
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         L     R1,8(R1)                                                         
         L     RE,SBACMLTB                                                      
         ST    R1,0(RE)            UPDATE N' RECORDS SO FAR                     
         LA    R4,ELEM                                                          
*                                                                               
GETCML6  MVC   SBCMLCD,CTCODE      COMMERCIAL CODE                              
         MVC   SBCMLNM,CTNAME                 NAME                              
         CLI   BYTE,3                                                           
         BNE   *+10                                                             
         MVC   SBCMLNM,CTNAME2                                                  
         CLI   BYTE,4                                                           
         BNE   *+10                                                             
         MVC   SBCMLNM,CTNAME3                                                  
*                                                                               
         MVC   SBCMLCLS,CTCLASS               CLASS                             
         MVC   BLOCK(20),CTNUM                CLT CML NO                        
         MVC   BLOCK+20(20),CTBASCD                                             
         MVC   CLSLIST,CTCLSPLT                                                 
         MVC   CLSNUM,CTCLNUM                                                   
         DROP  R4                                                               
*                                                                               
         CLI   BYTE,2              TEST COMMERCIAL CLASS SPLIT NAMES?           
         BE    *+12                                                             
         CLI   BYTE,1              TEST COMMERCIAL CLASS NAME NEEDED            
         BNE   GETCMLX                                                          
         MVC   WORK(24),BLANKS     YES-                                         
         MVC   WORK(10),=C'UNASSIGNED'                                          
         MVI   WORK+24,1           DEFAULT TO 1 CLASS NAME                      
         SR    R1,R1                                                            
         ICM   R1,1,CLSNUM                                                      
         BZ    GETCML6A                                                         
         LR    R0,R1                                                            
         LA    RF,24                                                            
         SR    RE,RE                                                            
         DR    RE,R0                                                            
         BCTR  RF,0                                                             
         STC   RF,CLSLEN           SAVE LENGTH OF NAME                          
         LA    R5,CLSLIST                                                       
         B     GETCML6B                                                         
*                                                                               
GETCML6A DS    0H                                                               
         LA    R5,SBCMLCLS                                                      
         LA    R1,1                                                             
         MVI   CLSNUM,1                                                         
         MVI   CLSLEN,23                                                        
         CLC   SBCMLCLS,=C'ZZZZ'   EXIT IF UNKNOWN CLASS                        
         BE    GETCMLX                                                          
*                                                                               
GETCML6B LA    R6,WORK                                                          
GETCML6C MVI   ELCODE,NPCCLCDQ     SEARCH NAME POOL FOR CML CLASS ELEM          
         BAS   RE,GETELBF                                                       
         BNE   GETCML12                                                         
*                                                                               
GETCML8  CLC   0(4,R5),2(R4)       MATCH CLASS,                                 
         BNE   GETCML10                                                         
*                                                                               
         CLC   AGENCY,=C'JM'       COKE?                                        
         BE    *+14                 YES                                         
         CLC   AGENCY,=C'CK'       COKE?                                        
         BNE   *+14                 NO                                          
         CLC   =X'885F',6(R4)       YES - USE CLT CC                            
         B     *+10                                                             
*                                                                               
         CLC   SBBCLT,6(R4)        CLIENT,                                      
         BNE   GETCML10                                                         
         CLC   SBPRD,8(R4)         AND PRODUCT                                  
         BNE   GETCML10                                                         
         ZIC   RE,CLSLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),11(R4)      EXTRACT CLASS NAME                           
         LA    R6,1(RE,R6)         NEXT SLOT                                    
         LA    R5,4(R5)                                                         
         BCT   R1,GETCML10                                                      
         MVC   WORK+24(1),CLSNUM                                                
         B     GETCMLX                                                          
*                                                                               
GETCML10 BAS   RE,NXTELBF                                                       
         BE    GETCML8                                                          
*                                                                               
GETCML12 ST    R1,MYSVR1                                                        
         LA    R4,KEY              READ COMMERCIAL CLASS RECORD                 
         USING CLSRECD,R4                                                       
         XC    CLSKEY,CLSKEY                                                    
         MVC   CLSKID(2),=X'0A44'                                               
         MVC   CLSKAM,SBBAGYMD                                                  
         MVC   CLSKCLAS,0(R5)                                                   
         OC    CLSKCLAS,BLANKS                                                  
         MVC   CLSKCLT,SBBCLT                                                   
*                                                                               
         CLC   AGENCY,=C'JM'       COKE?                                        
         BE    *+14                 YES                                         
         CLC   AGENCY,=C'CK'       COKE?                                        
         BNE   *+10                 NO                                          
         MVC   CLSKCLT,=X'885F'     YES - USE CLT CC                            
*                                                                               
         MVC   CLSKPROD,SBPRD                                                   
         OC    CLSKPROD,BLANKS                                                  
         GOTO1 HIGH                                                             
         CLC   CLSKEY,KEYSAVE      TEST RECORD FOUND                            
         BE    GETCML14                                                         
         MVC   KEY,KEYSAVE         NO-TRY WITHOUT PRODUCT                       
         XC    CLSKPROD,CLSKPROD                                                
         GOTO1 HIGH                                                             
         CLC   CLSKEY,KEYSAVE                                                   
         BE    GETCML14                                                         
         MVC   KEY,KEYSAVE         NO-TRY WITHOUT CLT                           
         XC    CLSKCLT,CLSKCLT                                                  
         GOTO1 HIGH                                                             
         CLC   CLSKEY,KEYSAVE                                                   
         BE    GETCML14                                                         
         MVC   ELEM(24),BLANKS                                                  
         MVC   ELEM(10),=C'UNASSIGNED'                                          
         B     GETCML18                                                         
*                                                                               
GETCML14 L     R4,AIO3             YES-GET THE RECORD                           
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R4,24(R4)                                                        
         SR    R0,R0                                                            
*                                                                               
GETCML16 CLI   0(R4),0             LOOK FOR DESCRIPTION ELEMENT                 
         BE    GETCML18                                                         
         CLI   0(R4),X'10'                                                      
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETCML16                                                         
         USING CLSDSCEL,R4                                                      
         MVC   ELEM(24),CLSDESC    EXTRACT CLASS NAME                           
         DROP  R4                                                               
*                                                                               
GETCML18 LA    R4,ELEM+24          ADD ELEMENT TO NAME POOL                     
         MVI   0(R4),NPCCLCDQ                                                   
         MVI   1(R4),NPCCLLNQ                                                   
         MVC   2(4,R4),0(R5)                                                    
         MVC   6(2,R4),SBBCLT                                                   
*                                                                               
         CLC   AGENCY,=C'JM'       COKE?                                        
         BE    *+14                 YES                                         
         CLC   AGENCY,=C'CK'       COKE?                                        
         BNE   *+10                 NO                                          
         MVC   6(2,R4),=X'885F'     YES - USE CLT CC                            
*                                                                               
         MVC   8(3,R4),SBPRD                                                    
         MVC   11(24,R4),ELEM                                                   
* LOCAL COPY OF PUTBFEL TO PREVENT EXIT                                         
*                                                                               
         ZIC   RE,1(R4)                                                         
         ICM   R1,15,ANAMPOOL                                                   
         BZ    GETCML20                                                         
         L     R0,0(R1)                                                         
         L     RF,4(R1)                                                         
         SR    RF,RE                                                            
         BP    *+6                                                              
         DC    H'0'                NAME POOL OVERFLOW                           
         ST    RF,4(R1)                                                         
         L     RF,0(R1)                                                         
         LA    RF,0(RE,RF)                                                      
         ST    RF,0(R1)                                                         
         MVI   0(RF),0                                                          
         LR    RF,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
GETCML20 L     R1,MYSVR1                                                        
         B     GETCML6C                                                         
*                                                                               
***         B     PUTBFEL                                                       
*                                                                               
GETCMLX  BAS   RE,SETSPT           SET SYSTEM BACK TO SPOT                      
         B     XIT                                                              
         EJECT                                                                  
SETTRF   MVC   SYSDIR,=C'TRFDIR  '                                              
         MVC   SYSFIL,=C'TRFFILE '                                              
         BR    RE                                                               
*                                                                               
SETSPT   MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFILE '                                              
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO GET THE MARKET WEIGHT                                              
*                                                                               
VGETMKTW CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    GMX                                                              
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
         CLC   MLDEMO,PRMYDEMO                                                  
         BE    GM18                                                             
*                                                                               
GM14     MVC   MLRTGSVC,SBCPROF+3  SET RTG SVC AND DEMO                         
         MVC   MLDEMO,PRMYDEMO                                                  
******** MVC   MLWGT,=F'1'                                                      
         USING MWTABD,R5                                                        
         L     R5,SBAMWTAB         FIND CORRECT MARKET WEIGHT FROM              
         LA    R0,MWTENTS          MARKET WEIGHT TABLE                          
*                                                                               
GM16     OC    MWTDEM,MWTDEM                                                    
         BZ    GM20                                                             
         CLC   MWTSVC,SBCPROF+3    MATCH RATING SERVICE                         
         BNE   *+14                                                             
         CLC   MWTDEM,PRMYDEMO     MATCH PRIMARY DEMO                           
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
GM22     ZIC   R1,MKTLEV           SET MARKET ENCOUNTERED AT ALL                
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
         MH    R2,=Y(XSPILLL)                                                   
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
         BAS   RE,XSPL40           GET SPILL MARKETS' NAMES                     
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
*                                                                               
XSPL40   NTR1  ,                   *** GET SPILL MARKETS' NAMES ***             
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
         B     XIT                                                              
*                                                                               
*                                                                               
XSPLX    B     XIT                                                              
         EJECT                                                                  
* INITIALIZE TO RUN DRIVER - LOADS PHASES                                       
*                            SETS GLOBAL ADDRESSES                              
*                                                                               
VINTDRIV CLI   OFFLINE,C'Y'                                                     
         BNE   DRIX                                                             
         LA    R1,PWAREA                                                        
         STCM  R1,7,SBAWIPW                                                     
         GOTO1 CALLOV,DMCB,X'B2000000',0,0  LOAD T204B1 (BUFFERS)               
         L     R2,DMCB                                                          
         LA    R2,8(R2)                                                         
         ST    R2,SBAPRDBF         A(PRODUCT BUFFER)                            
         LH    R1,=Y(PRDBUFFL)     PRODUCT BUFFER ENTRY LENGTH                  
         SLL   R1,8                X 256                                        
         LA    R2,8(R1,R2)                                                      
         ST    R2,SBAESTTB         A(ESTIMATE TABLE)                            
         LA    R1,1                256 X 256                                    
         SLL   R1,16                                                            
         LA    R2,8(R1,R2)                                                      
         ST    R2,SBAUSNTB         A(USER DEMO NAME TABLE)                      
         L     R1,4(R2)            GET MAXIMUM NUMBER OF USER NAMES             
         MH    R1,=H'7'                                                         
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
         MH    R1,=H'36'           N'ENTRIES                                    
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
         ST    R1,ANAMPOOL         A(NAME POOL)                                 
         ICM   RE,15,LNAMPOOL      L'NAME POOL                                  
         BNZ   *+8                                                              
         L     RE,=F'2000000'                                                   
         ST    RE,LNAMPOOL                                                      
         LA    RF,8(R1)                                                         
         ST    RF,0(R1)            A(FIRST AVAILABLE ENTRY)                     
         ST    RE,4(R1)            LENGTH OF POOL                               
*                                                                               
         AR    R1,RE               NETWORK SIZE TABLE                           
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
*        AHI   R1,PURPTABL*PURPTABQ                                             
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',20000,20000    GET MARKET GROUP TABLE         
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,SBAMGTAB         A(MARKET GROUP TABLE)                        
*                                                                               
         TM    ROWIND,ROWICML      TEST NEED COMMERCIAL TABLE                   
         BZ    DRI2                                                             
         LA    R2,2000             YES-SET MAX N'CML TABLE ENTRIES              
         LR    R1,R2                                                            
         MH    R1,=Y(CMLTABL)                                                   
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
DRI2     TM    SBQREAD,SBQRDCLS    TEST READ CLEARANCE STATUS RECORDS           
         BZ    DRI4                                                             
         LH    R5,=H'2000'         YES-GET STORAGE FOR CHECK TABLE              
         LR    R3,R5                                                            
         LA    RE,CHKTABL                                                       
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
         L     R0,=A(SBTSARBL)     L'BUFFER                                     
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,SBTSARBF                                                      
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
         ZIC   RE,GLFHEADL                                                      
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
DRI6     CLI   SBQMGRD,C' '                                                     
         BNH   DRI8                                                             
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
* CONSTANTS TABLES ETC                                                          
*                                                                               
BLANKS   DC    CL132' '                                                         
*                                                                               
XFF      DC    XL4'FFFFFFFF'                                                    
*                                                                               
SUBTIT2  DC    CL32' '                                                          
SUBTIT3  DC    CL32' '                                                          
*                                                                               
CLSLIST  DS    CL16                LIST OF CLASS SPLITS                         
CLSNUM   DS    X                   NUMBER OF CLASSES                            
CLSLEN   DS    X                   LENGTH OF EACH CLASS TO KEEP                 
*                                                                               
MYSVR1   DS    F                   SAVE R1                                      
*                                                                               
PRDTAB   DS    XL256               TABLE OF PRODUCTS FOR THIS STA               
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
         LTORG                                                                  
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
*        INCLUDE DEDBLOCK                                                       
*        INCLUDE SPGENAGY                                                       
*        INCLUDE SPGENCLT                                                       
*        INCLUDE SPGENPRD                                                       
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
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPWRIOFF  08/27/02'                                      
         END                                                                    
