*          DATA SET PPREP1202  AT LEVEL 132 AS OF 07/08/15                      
*PHASE PP1202A                                                                  
*INCLUDE PPRTLOOK                                                               
*INCLUDE CHOPPER                                                                
*INCLUDE DATVAL                                                                 
*INCLUDE PPGETADR                                                               
*INCLUDE PPGETCG                                                                
         TITLE 'PP1202 - PRINT CONTRACTS CHANGE LOG'                            
*                                                                               
* BPLA 03/14    LOOK FOR INTERNET SITE FOR ANY MEDIA                            
*              -WAS JUST CHECKING I - MEDIA L MAY HAVE IT                       
*                                                                               
* SMYE 02/13/09 RELINK FOR UPDATED PPGETCG (ALSO CHANGED LABELS                 
*                G1 THRU G9 TO G1XX THRU G9XX TO PREVENT DUPLICATION            
*                OF THE GENERAL REGISTER LABELS G1-G9 IN NMOD)                  
*                                                                               
*                                                                               
* SMYE 05/04/06 PRDTAB INCREASED TO HANDLE 1500 PRODUCTS (FROM 1000)            
*                                                                               
* SMYE 02/06    DEFAULT AGENCY COMMISSION IS 15% FOR ALL MEDIA                  
*                 (INCLUDING OUTDOOR)  SEE *SMY*                                
*                                                                               
* SMYE 03/05    CONDITIONALLY ELIMINATE 1ST CHARACTER OF COMMENTS               
*                 CONTAINED IN PBYOCOMS                                         
*               AND ACTIVATED (WAS COMMENTED OUT) TEST OF PROGPROA+6            
*                 AT CON3C TO UNLOCK ONLY IF FILE MARKER REQUESTED              
*                                                                               
* SMYE 11/04    FIX CODE FOR ESTIMATE FILTERING (QESTEND IN REQ CARD)           
*                                                                               
* SMYE 06/04    CHANGES BY YKAP INCLUDING ISSUE NAME - 04/03-->                 
*                                                                               
* SMYE 04/07/04 BIGTABLE INCREASED TO 7000C TO HANDLE 200 LINES                 
*                 FROM 3500C TO HANDLE 100 LINES                                
*                                                                               
* SMYE 01/13/03 PRDTAB INCREASED TO HANDLE 1000 PRODUCTS (FROM 600)             
*                                                                               
* BPLA 06/01    CHANGES FOR UPDATIVE SOON SPACE RESERVATIONS                    
*                                                                               
* BPLA 04/01    WHEN PROCESSING CONCOM RECS                                     
*               CHECK FOR "NESTED" STANDARD COMMENTS                            
*               COM=XXXX                                                        
*                                                                               
* BPLA 03/01    CHANGE TO DISPLAY ADDITIONAL CHARGES                            
*                                                                               
* BPLA 03/01    CHANGES FOR MARKING INSERTIONS                                  
*               (IF NOT RUNNING SOON)                                           
*               CONTROLLED BY NEW P12 PROFILE OPTION                            
*                                                                               
* SMYE 02/01    ADD CONTRACT COMMENTS (X'44' RECORDS)                           
*                                                                               
* KWAN 01/01    FOR AGY GZ SKIP MASTER CLT SO 000 CONTRACT PRINTED              
*                                                                               
* KWAN 01/01    REVERSE AGENCY COMMISSION TO OLD % FOR NOW                      
*                                                                               
* KWAN 01/01    DEFAULT AGENCY COMMISSION IS 15% FOR ALL                        
*                                                                               
* KWAN 10/00    BINARY ACCUMULATORS CHANGED INTO PACKED                         
*                                                                               
* BPLA 8/16/00  SORTTAB INCREASED TO HANDLE 2500 BUYS (FROM 1500)               
*                                                                               
* SMYE 07/14/00 FIX PPNEWFILE BUG AT CON22                                      
*                                                                               
* SMYE 04/24/00 CHANGES FOR LARGER PPNEWFILE                                    
*                                                                               
* BPLA 01/27/00 SORTTAB INCREASED TO HANDLE 1500 BUYS (FROM 1000)               
*                                                                               
* KWAN 12/99    TEL AND FAX OVER WRITES (REVISED)                               
*                                                                               
* BPLA 12/10/99 FIX Y2K BUG IN GRID FORMAT - SEE **Y2K                          
*               FIX GOOD THRU 2155                                              
*                                                                               
* SMYE 10/15/99 USE "INTERACTIVE" FOR MEDIA NAME IN PLACE OF OTHER              
*               SPELLINGS (AT LABELS REGPRT.. AND CH1D..)                       
*                                                                               
* KWAN 08/99    TEL AND FAX OVER WRITES                                         
*                                                                               
* KWAN 07/99    USE PPGETADK INSTEAD OF PPGETADR                                
*                                                                               
* KWAN 03/30/99 ADD EST COLUMN TO CONTRACT REGISTER PAGE                        
*                                                                               
* SMYE 03/30/99 CHECK FOR PGADFAX GREATER THAN SPACES                           
*               AS WELL AS LENGTH OF PGADELEM BEFORE MOVING                     
*               PGADLIN3 AND PGADFAX INTO REP AREAS (AT CON34..)                
*                                                                               
* KWAN 03/16/99 ADD CODES FOR REQUESTING CONTRACTS BY ESTIMATE                  
*                                                                               
* SMYE 10/30/98 NO-OP MEDIA N EXCEPTION TO RATES-BY-PRODUCT DISPLAY             
*               IN PROCS CON51 AND CON58Z                                       
*                                                                               
* SMYE 10/97    SKIP "HELD" CASH IN ADVANCE BUYS (X'08' IN PBDSTAT)             
*                                                                               
* SMYE 9/97     DO NOT DIE IF PRODUCT RECORD NOT FOUND IN PROC CON08            
*                                                                               
* BPLA  7/97    SUPPORT CONTROL FILE FAX RECORDS (FX=XXXXX)                     
*                                                                               
* SMYE 4/97     USE CALL TO PPGETADR TO GET ADDRESSES (IN CON32..)              
*                                                                               
* SMYE 7/96     SLAVE CLIENT NOW BEING READ INTO SLAVEIO IN                     
*               SPACEND INSTEAD OF INTO PDIVREC                                 
*                                                                               
* BPLA 5/96     USE MEDIA NAME OVRRIDE FROM CLTHDR (X'41' ELEMENT)              
*                                                                               
* BPLA 3/96     CHECK LENGTH OF PUBAOVEL BEFORE MOVING                          
*               PUBAOLN3 AND PUBAOFAX INTO REP AREAS                            
*                                                                               
* BPLA 1/96     IF PRBLIND IS "N" (FOR NET $) PRECEED                           
*               LEVEL WITH "$" (LIKE PRBLIND = "$")                             
*                                                                               
* SMYE 12/06/95 CHANGED DTCNV TO DATCON WITH "NEW" PARAM'S                      
*                                                                               
* BPLA 7/95     IF QOPT2 = X - SUPPRESS PREMIUM AS WELL                         
*               + FIX PREM DISPLAY IF NOT COLOR                                 
*               + NEW 12A, P13A AND P15A PROFILE OPTION                         
*                 DELETED BUY COMMENT OPTION (PROGPROA+3)                       
*                   R=NO REGULAR COMMENTS                                       
*                   N=DON'T SUPPRESS COMMENTS                                   
*               ANOTHER NEW 12A, 13A, AND 15A PROFILE OPTION                    
*               SUPPRESS REGISTER OPTION (PROGPROA+4)                           
*                   Y= SUPPRESS                                                 
*                   N= DON'T                                                    
*               ** NOTE ** THIS OPTION JUST SUPPRESSES ADDING                   
*                          CONTRACTS TO THE REGISTER  - NOT                     
*                          THE PRINTING OF THE REGISTER                         
*                                                                               
* BPLA 6/95     NEW VALUE FOR QOPT2 X - MEANS SUPPRESS COSTS                    
*               ON SCHEDULE                                                     
*                                                                               
* BPLA 3/95     DISPLAY /I FOR INCH RATES FOR NON-INCH LEVEL INDS               
*                                                                               
* BPLA 11/22/94 OLD AOR (DUPONT) DISABLED                                       
*                                                                               
* BPLA SEP/94   WHEN FAXING AGENCY NAME AND ADDRESS MUST PRINT                  
*               ALSO REMOVE *'S  OVER AREA FOR AAAA MEMBER MESSAGE              
*               (DONE IN CONPRT)                                                
* BPLA AUG/94   AT RUNLAST CHECK FTIME  - IF 'N'                                
*               AND SEND A FINAL                                                
*               '*** END OF DDS MESSAGE ***'                                    
*               SINCE SOMETHING WAS FAXED                                       
*                                                                               
* BPLA AUG/94   USE 'MVI   FORCEHED,C'P''  (NOT C'Y')                           
*                                                                               
* BPLA AUG/94   CHANGES TO ++DDS CARD                                           
*                                                                               
* BPLA JUL/94   ADD REGISTER FOR P13'S - RCSUBPRG = 12                          
*                                                                               
* BPLA MAR/94   IF FAXING - SPECIAL CODE IN CONEOP                              
*                                                                               
* BPLA FEB/94   CHANGES FOR FAXING                                              
*                                                                               
* BPLA 9/17/93  ADD SLAVE CLT TO REGISTER                                       
*               ALSO FIX REGISTER REGISTER PROBLEM FOR 13'S                     
*                                                                               
* BPLA 12/18/91 SORTAB INCREASED FROM 600 BUYS TO 1000                          
*                                                                               
* ROSA 3/19/91  MILLIONS OF RATE TRUNCATED                                      
*                                                                               
* ROSA 10/23/90 CONTRACT ADDRESS OVERIDE NOT WORKING WHEN SLAVE BUG             
*               CLIENT IS REQUESTED WITH MASTER.  PREPNAME HAS  BUG02           
*               A VALUE IN IT CAUSING INCORRECT LOGIC PATH      BUG02           
* ROSA 10/12/90 OPTION TO VARY SEQUENCE OF CONTRACT STANDARD AND  L18           
*               AND SPECIAL COMMENTS BASED ON VALUE OF PROFILE+14 L18           
*                                                                               
* BPLA 8/30/90  PROBLEMS WHEN PRINTING SCHEDULE AND COMMENTS/RATES              
*               GO OVER 1 PAGE                                                  
*               1) SKIPPING 2 LINES BEFORE **SCHEDULE** CAUSES                  
*                  HEADING TO PRINT TWICE + NO (CONTINUED) MSG.                 
*                 (IF SKIP CAUSED NEW PAGE)                                     
*               FIX - SEE IF THE SKIP WILL CAUSE A PAGE BREAK                   
*                     IF SO DON'T SKIP.                                         
*                                                                               
*               2) **SCHEDULE** HEADING DOESN'T PRINT IF COMMENTS               
*                  GO OVER TO PAGE 2.                                           
*               FIX - NO-OPED CKECKS FOR LINE UNDER 20                          
*                                                                               
* ROSA 8/7/90   LAURENCE CHARLES REQUESTED NO PRODUCT TOTALS ON   L16           
*               GRIDS WHEN PRODUCT INFO SUPPRESSED                L16           
*                                                                               
* BPLA 7/12/90  CONTROL DATE MOVED FROM QPUBCLAS+1(3)                           
*               TO QCNTLDT (QPUBCLAS) AND STANDARD COMMENT MOVED                
*               FROM QPAY TO QCOMM (COL 21 CARD 2)                              
*                                                                               
* BPLA 3/15/90  ALWAYS DO SPECIAL HEADINGS FOR AGENCY LC           L15          
*               (ONLY USED TO DO FOR 12 AND 13)                    L15          
*                                                                               
* SWON 1/19/90  PRINT FAX,OR TELEPHONE OR BOTH                     L14          
*               PROFOPTS+13 CAN HAVE T,F,B OPTIONS:                L14          
*               T=TELEPHONE NUMBER ONLY                            L14          
*               F=FAX NUMBER ONLY                                  L14          
*               B=BOTH TELEPHONE AND FAX                           L14          
*                                                                               
*                                                                               
* BPLA 1/26/89  HAVE PP13'S USE PP12 PROFILE                       L14          
*               IF NO PP13 PROFILE IS FOUND                        L14          
*                                                                               
* BPLA 12/5/89  GROUP REQUESTS                                     L13          
*                                                                               
* BPLA 10/2/89  PRINT ATTENTION OF INFO BEFORE ADDRESS LINES       L12          
*               FOR PUB OR CON REP OR ADDR,CON                     L12          
*                                                                               
* BPLA 9/7/89  AT CONSCH0E AND SORTSH0E  MOVING REQPUB TO          L11          
*              PCONKPUB WAS ONLY MOVING 4 CHARS - THIS CAUSED      L11          
*              A PROBLEM FOR REQUESTS FOR CON=000 (NO CONTRACT)    L11          
*                                                                               
* ROSA 6/22/89 1- FOR LC (LAURENCE CHARLES) OUTDOOR, TO PRINT      L10          
*      'DURING THE PERIOD' INSTEAD OF 'FOR THE PERIOD'             L10          
*     2- OPTION IN P12 & P15 TO SUPPRESS PRINTING OF THE           L10          
*        'RATE EFF DATE' COLUMN..  PRINT THE ACTUAL DATES IF       L10          
*        RATES ARE SUPERCEEDED.                                    L10          
*     3- OUTDOOR GRIDS -- DO NOT FILL IN THE 'RESERVED' LINE       L10          
*                                                                  L10          
* ROSA 12/27/88 1-ALLOW GRIDS TO PRINT WHEN SUPPRESSING PRODUCT FOR L09         
*               OUTDOOR                                             L09         
*               2-OUTDOOR ONLY// PRODUCT NOT TO APPEAR ANYWHERE IF  L09         
*                 NO PRODUCT OPTION ON                              L09         
*               3-P12- NEW PROFILE OPTION..+11 PRINT "POSTERS" IF   L09         
*                      NO EDITION IS PRESENT AND IS OUTDOOR         L09         
*                 P12- NEW PROFILE OPTION +12 // PRINT OPTION       L09         
*                      C=CONTRACT ONLY S=SCHEDULE ONLY BLANK =BOTH  L09         
*                                                                   L09         
*ROSA 9/1/88 QOPT4 WAS BEING OVERRIDEN BY TESTING FOR PROFOPTS+8  BUG01         
*            IF Y THEN G WAS PUT INTO QOPT4. ELIMINATE THAT CODE..BUG01         
*                                                                   L08         
* ROSA 7/19/88 PP15 TO REFLECT SAME AS LEVEL06 CHANGE (PRODUCT AND  L08         
*          PRODUCT CODE SUPPRESSIONS)                               L08         
*                                                                   L08         
* ROSA 7/15/88 PROFOPTS+10 IF 'B' DO NOT PRINT LEVEL AND PERCENT    L07         
*             DESCRIPTIONS AND DATA. LAURENCE CHARLES BENEFACTOR..  L07         
*             IF 'L' DO NOT PRINT LEVEL  IF 'P' DO NOT PRINT %      L07         
*                                                                   L06         
* ROSA 7/13/88 (LAURENCE CHARLES) FOR 12- PROFOPTS+6 CAN HAVE Y,N,B L06         
*           OPTIONS.  N=NO SUPPRESSION OF PRODUCT                   L06         
*                     Y= SUPPRESS PRODUCT DESCRIPTION               L06         
*            NEW--->  B= NO PRODUCT CODE OR DESCRIPTION             L06         
*        ALSO CHANGE HEADING TO PRINT MASTER BILLING CONTRACT FOR   L06         
*                  LAURENCE CHARLES                                 L06         
*====NOTE-- IF PROFOPTS+6 = B .. FORCE TO SORT BY INSERT DATE       L06         
*            MOVE D INTO SSORTOPT                                   L06         
* ROSA  6/2/88 PRINT C RATE                                         L05         
*                                                                   L04         
* ROSA 5/26/88 PRINT S RATE                                         L04         
*                                                                   L03         
* ROSA 5/25/88 PRINT POSITION INSTRUCTIONS ON CONTRACTS             L03         
* ALSO ADD PROFILE OPTION                                           L03         
*    +9    PRINT POSITITON INSTRUCTIONS  -DEFAULT =N                L03         
*                                                                   L02         
* ROSA 5/2/88  CHANGES FOR OUTDOOR                                  L02         
*              THESE INCLUDE NEW PROFILES                           L02         
*          PROFOPTS  (FROM PROFILE)                                 L02         
*    +3    SUPPRESS PRINTING OF CASH DISCOUNT                       L02         
*    +4    SUPPRESS    "      "   AGENCY COMMISSION                 L02         
*    +5      "         "      "   EFFECTIVE DATE                    L02         
*    +6      "         "      "   PRODUCT NAMES                     L02         
*    +7    PRINT GRP LEVEL INSTEAD OF SIZE/SHOW                     L02         
*    +8    PRINT MEDIA ALLOC INSTEAD OF DISPLAYS                    L02         
*    +9    PRINT POSITITON INSTRUCTIONS  -DEFAULT =N                L03         
*                                                                   L01         
*   ROSA 4/29/88 ** L01 **  PRINT A 'N' PRIOR TO RATE IF NET        L01         
*                                                                   L01         
         TITLE 'PP1202 - PRINT CONTRACTS'                                       
PP1202   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
* REQUEST RULES                                                                 
*                                                                               
*  PUB   DATE  CON NUM                                                          
*  ---   ----  -------                                                          
*                                                                               
*  ALL    YES   NO                                                              
*                                                                               
*  YES    YES   NO                                                              
*                                                                               
*  YES    NO    YES                                                             
*                                                                               
*                                                                               
*  IF CON NUM = 'NO' DATES MUST BE GIVEN                                        
*  AND PUB CAN BE ALL OR YES - NO CONTRACT READ                                 
*                                                                               
*                                                                               
*  QDIV MAY CONTAIN SLAVE CLIENT, IF SO PRODUCT BELONGS TO SLAVE                
*                                                                               
*  QCOMM(6) MAY HAVE COMMENT CODE                                               
*                                                                               
*  QCNTLDT(3) MAY HAVE CHG CONTROL DATE                                         
*                                                                               
*                                                                               
*  QPAY     'Y' - MEANS FAX CONTRACTS (IF I FIND A FAX NUMBER)                  
*           'N' - DO NOT FAX THE CONTRACTS                                      
*                 THIS OPTION IS MOVED TO FAXOPT AND OVERRIDES                  
*                 THE PROFILE (12A,13A,15A)                                     
*                                                                               
*              *** NOTE THAT THE PROGRAM ALSO CHECKS THE                        
*              *** WP PROFILE TO BE SURE THAT THE AGENCY IS                     
*              *** ALLOWED TO FAX VIA ATT EASYLINK                              
*              *** FAXOPT WILL BE SET TO "N" IF FAXING IS NOT ALLOWED           
*                                                                               
*                                                                               
*  QOPT1 = 'S' PRINT SCHEDULE ONLY                                              
*          'C' PRINT CONTRACT ONLY                                              
*  QOPT2 = 'N'  PRINT NET INSTEAD OF GROSS                                      
*  QOPT2 = 'X'  SUPPRESS COSTS ON SCHEDULE                                      
*  QOPT3 = 'R' SPACES RESERVATION INSTEAD OF CONTRACT                           
*  QOPT4 = 'G' GRID SCHEDULE FORMAT                                             
*  QOPT5 = 'F' FLAG CHANGES SINCE REVISION DATE                                 
*          'C' = SHOW ONLY CHANGES                                              
*  QOPT6 = 'A' AD NO.                                                           
*          'B' AD NO. + CAPTION                                                 
*          '1' COPY NO.                                                         
*          '2' COPY NO. + CAPTION                                               
*                                                                               
*  QOPT7 = 'I' TOTALS IN INCHES                                                 
*          'L' TOTALS IN LINES                                                  
*  OLD USE FOR QOPT7 - NO-OPED IN REQ                                           
***QOPT7 = 'D' SORT SCHEDULE IN DATE ORDER                                      
*          'P' SCHEDULE IN PRODUCT ORDER                                        
*          DEFAULTS TO PROFILE                                                  
*          IF NO PROFILE THEN 'P' - PRODUCT ORDER                               
*          PUT IN SSORTOPT                                                      
*                                                                               
*        NOTE THAT THIS REPORT READS SLAVE CLIENT INTO PDIVREC                  
*        NOTE THAT THIS REPORT READS SLAVE CLIENT INTO SLAVEIO (7/96)           
*                                                                               
*                                                                               
         EJECT                                                                  
         NMOD1 0,PP1202,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         LA    R3,PP1202+4095                                                   
         LA    R3,1(R3)                                                         
         USING PP1202+4096,R3       ** NOTE USE OF SECOND BASE REG **           
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         AHI   R9,4096                                                          
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP12WRKD,R8                                                      
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         MVC   ACONIO1,ACONIO      GET AND STORE (A)PCONREC IN ACONIO1          
         DROP  RF                                                               
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   CON2                                                             
*                                                                               
         MVI   SOONSW,C'N'                                                      
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         L     R1,VMASTC           ADDRESS OF MASTER CONTROLS                   
         USING MASTD,R1                                                         
         OC    MCREMPQK,MCREMPQK    SEE IF SOON REQ                             
         BZ    *+8                                                              
         MVI   SOONSW,C'Y'                                                      
         DROP  R1                                                               
         DROP  RF                                                               
*                                                                               
         MVI   FTIME,C'Y'          NEEDED WHEN FAXING                           
*                                  WILL GET SET TO 'N' WHEN ANYTING             
*                                  IS FAXED                                     
*                                                                               
         MVI   LASTCKEY,0          FORCE NO MATCH                               
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVI   FORCEHED,C'P'                                                    
         MVI   DASHES1,C'-'                                                     
         MVC   DASHES1+1(L'DASHES1-1),DASHES1                                   
* SET RELOCATABLE ADDRESSES                                                     
         LA    R1,ACONS                                                         
         LA    R0,30                                                            
         LA    R2,ARTLOOK                                                       
CON1     DS    0H                                                               
         L     RF,0(R1)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,CON1                                                          
*                                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         L     R1,ACONTAB                                                       
         ST    R1,ANEXTCON                                                      
         MVI   0(R1),0                                                          
*                                                                               
         MVI   FULLSW,0             0 REGISTER FULL SWITCH                      
         MVI   LASTMED,0       CLEAR LAST MEDIA                                 
         XC    LASTMEDN,LASTMEDN  CLEAR MEDIA NAME OF LAST MEDIA                
         XC    SVLKCLT,SVLKCLT    CLEAR                                         
         XC    SVLKPUB,SVLKPUB                                                  
*                                                                               
         XC    SVESTFLT,SVESTFLT   INIT EST FILTER                              
         CLC   QESTEND,=C'000'                                                  
         BNH   CON1ESTX            NO FILTER                                    
         PACK  DUB,QESTEND                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,SVESTFLT       BINARY EST FILTER                            
CON1ESTX DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CON2     CLI   MODE,FBUYREQ                                                     
         BNE   CON3                                                             
*                                                                               
         MVC   MYFAX,SPACES          JUST IN CASE                               
         MVC   TOFAX,SPACES          JUST IN CASE                               
*                                                                               
         XCEFL SLAVEIO,600           CLEAR AREA USED FOR SLAVE CLIENT           
*                                                                               
         XC    SVESTFLT,SVESTFLT   INIT EST FILTER                              
         CLC   QESTEND,=C'000'                                                  
         BNH   CON2ESTX            NO FILTER                                    
         PACK  DUB,QESTEND                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,SVESTFLT       BINARY EST FILTER                            
CON2ESTX DS    0H                                                               
*                                                                               
         CLI   QPUB,C'0'              IF ONE PUB                                
         BNL   CON2B                                                            
         CLI   QSORT,C' '          OR NO SORT                                   
         BE    CON2B               PROC REQ NOW                                 
*                                                                               
         CLI   SOONSW,C'Y'         SEE IF SOON PROCESSING                       
         BNE   EXIT                NO NEED TO SAVE                              
         MVC   SVLKCLT,QCLIENT     SAVE VALUES FROM FIRST                       
*                                                                               
*        ALSO IN CH22                                                           
*                                                                               
         MVC   SVQPROG,QPROG         SAVE PROG/AGY/MED                          
         MVC   SVQAGY,QAGENCY                                                   
         MVC   SVQMED,QMEDIA                                                    
*                                                                               
         MVC   SVQOPT3,QOPT3       NEEDED FOR UPDATIVE                          
*                                  SOON SPACE RESERVATIONS                      
*                                  NEEDED FOR LOCK KEY                          
*                                  SORTED REQ - NO PUB                          
         B     EXIT                ELSE WAIT FOR SORTED REQS                    
*                                                                               
CON2B    DS    0H                                                               
         CLI   SOONSW,C'Y'                                                      
         BNE   CON2B3                                                           
         OC    SVLKCLT,SVLKCLT     AM I PROCESSING SORTED REQUESTS              
         BNZ   CON2B3              IF SO SVLKCLT WILL BE SET                    
         MVC   SVLKCLT,QCLIENT                                                  
         MVC   WORK(11),QPUB       SET SVLKPUB IF ONE PUB REQ                   
         CLI   QEDITION,C'Z'                                                    
         BNE   *+8                                                              
         MVI   WORK+10,C' '                                                     
         CLI   QZONE,C'Z'                                                       
         BNE   *+10                                                             
         MVC   WORK+8(2),SPACES                                                 
         GOTO1 PUBVAL,DMCB,WORK,SVLKPUB                                         
*                                                                               
*        NEEDED IN CASE NO CONTRACTS REPORTED                                   
*                                                                               
         MVC   SVQPROG,QPROG         SAVE PROG/AGY/MED                          
         MVC   SVQAGY,QAGENCY                                                   
         MVC   SVQMED,QMEDIA                                                    
*                                                                               
         MVC   SVQOPT3,QOPT3       NEEDED FOR UPDATIVE                          
*                                  SOON SPACE RESERVATIONS                      
*                                  NEEDED FOR LOCK KEY                          
*                                  SORTED REQ - NO PUB                          
*                                                                               
CON2B3   CLI   PAGYPROF+1,C'L'     UNLESS AGYPROF SAYS LINE NO.                 
         BNE   *+8                                                              
         MVI   DATEOPT,C'L'                                                     
CON2B5   MVC   AFSTCON,ANEXTCON          SAVE ADDRESS OF FIRST CON              
*                                        IN REGISTER FOR THIS REQUEST           
         CLC   QMEDIA,LASTMED                                                   
         BE    CONCLT        SAME MEDIA - NO REGISTER TO PRINT                  
         CLI   LASTMED,0                                                        
         BE    CONCLT          FIRST TIME                                       
         GOTO1 APRTREG          GO PRINT REGISTER                               
         B     CONCLT                                                           
*                                                                               
CON3     CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
*                            FTIME WILL BE SET TO 'N' IF ANYTHING               
*                            GOT FAXED                                          
         CLI   FTIME,C'Y'                                                       
         BE    CON3C                                                            
         MVI   FORCEHED,C'N'   JUST IN CASE                                     
         MVC   P,SPACES                                                         
         MVC   P(26),=C'*** END OF DDS MESSAGE ***'                             
         MVI   LINE,1         SO THIS WILL NEVER CAUSE A NEW PAGE               
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
CON3C    DS    0H                                                               
         GOTO1 APRTREG           GO PRINT LAST REGISTER                         
*                                                                               
         CLI   SOONSW,C'Y'                                                      
         BNE   CON3CX                                                           
         CLI   SVQOPT3,C'R'        MUST BE SPACE RESERVATION                    
         BNE   CON3CX                                                           
*                                                                               
         CLI   PROGPROA+6,C'Y'  IF NOT MARKING BUYS - DON'T UNLOCK              
         BNE   CON3CX                                                           
*                                                                               
*  NOW I MUST ISSUE UNLOCKS FOR BUY/CLT AND POSSIBLY BUY/CLT/PUB                
*        CALL LOCKET                                                            
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         L     RF,VCOMFACS                                                      
         L     RF,(CCALLOV-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
*                                                                               
L        USING LKKEYD,LKUPKEY                                                   
         XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKRTY,=C'BP'     BUY/CLT/PUB UNLOCK                          
         L     R1,UTL                                                           
         MVC   L.LOCKSE,4(R1)                                                   
         MVC   L.LOCKAGY,SVQAGY                                                 
         MVC   L.LOCKMED,SVQMED                                                 
         MVC   L.LOCKCLT,SVLKCLT                                                
         OC    SVLKPUB,SVLKPUB      CHECK ONE PUB REQ                           
         BZ    TSTLOCK3                                                         
*                             NOTE THAT THE BASE PUB IS                         
*                             LOCKED  - NOT BY ZONE/EDITION                     
*                                                                               
         MVC   L.LOCKPUB(4),SVLKPUB                                             
         XC    L.LOCKPUB(4),=4X'FF'    COMPLIMENT                               
*                                      SO LOCKET CAN FIND                       
*                                                                               
TSTLOCK2 GOTO1 (RF),DMCB,('LKUNLKQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2      TABLE BUSY                                          
         BE    TSTLOCK2                                                         
         B     CON3CX                                                           
*                                                                               
TSTLOCK3 DS    0H                                                               
         MVC   L.LOCKRTY,=C'BC'     BUY/CLT LOCK                                
TSTLOCK4 GOTO1 (RF),DMCB,('LKUNLKQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),2      TABLE BUSY                                          
         BE    TSTLOCK4                                                         
*                                                                               
         DROP  L                                                                
*                                                                               
CON3CX   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
EXIT     XC    PCLTKEY,PCLTKEY     CLEAR IND FOR NEXT TIME                      
         XMOD1 1                                                                
         EJECT                                                                  
CONCLT   XC    KEY,KEY                                                          
         XC    SVPRDNAM,SVPRDNAM                                                
         XC    CONPUB,CONPUB                                                    
         XC    REQPUB,REQPUB                                                    
*                                                                               
         MVC   LASTMED,QMEDIA                                                   
         MVC   LASTMEDN,PAGYMED                                                 
*                                                                               
         CLI   PCLTKEY,0           TEST FIRST TIME                              
         BZ    CONCLT2             YES                                          
         CLC   =C'ALL',QCLIENT                                                  
         BE    CONCLT1                                                          
         CLI   QCLIENT,C'&&'       GROUP REQUEST            L13                 
         BE    CONCLT1                                      L13                 
         CLI   QCLIENT,C'*'                                                     
         BNE   EXIT                                                             
CONCLT1  DS    0H                                                               
         MVC   KEY,PCLTKEY                                                      
         BAS   RE,HIGH1                                                         
         BAS   RE,SEQ1             READ NEXT CLIENT                             
         B     CONCLT4                                                          
CONCLT2  MVC   KEY(3),PAGYREC      A/M                                          
         MVI   KEY+3,2                                                          
         CLC   =C'ALL',QCLIENT                                                  
         BE    CONCLT3                                                          
         CLI   QCLIENT,C'&&'         GROUP REQUEST           L13                
         BE    CONCLT3                                       L13                
         CLI   QCLIENT,C'*'                                                     
         BNE   CONCLT6                                                          
CONCLT3  DS    0H                                                               
* ALL CLIENTS                                                                   
         BAS   RE,HIGH1                                                         
CONCLT4  CLC   KEY(4),KEYSAVE      A/M/X                                        
         BE    CONCLTX                                                          
         B     EXIT                                                             
* ONE CLIENT                                                                    
CONCLT6  CLI   QDIV,C' '           FIRST SEE IF SLAVE REQUEST                   
         BE    CONCLT6D                                                         
         MVC   KEY+4(3),QDIV                                                    
         BAS   RE,HIGH1                                                         
         CLC   KEYSAVE(7),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*****    LA    R0,PDIVREC         READ SLAVE CLIENT INTO PDIVREC                
         LA    R0,SLAVEIO         READ SLAVE CLIENT INTO SLAVEIO                
         ST    R0,IOAREA                                                        
         BAS   RE,GET              GET CLIENT HEADER                            
*                                                                               
CONCLT6D MVC   KEY+4(3),QCLIENT                                                 
         BAS   RE,HIGH1                                                         
         CLC   KEYSAVE(7),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CONCLTX  LA    R0,PCLTREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET              GET CLIENT HEADER                            
         CLI   QCLIENT,C'*'                                                     
         BNE   CONCLTX1                                           L13           
         CLC   QCLIENT+1(1),PCLTOFF     TEST RIGHT OFFICE                       
         BNE   CONCLT1                                                          
         B     CONCLTX2                                           L13           
CONCLTX1 DS    0H                                                 L13           
         CLI   QCLIENT,C'&&'            GROUP     REQUEST         L13           
         BNE   CONCLTX2                                           L13           
         CLC   QCLIENT+1(1),PCLTBLGP    TEST RIGHT OFFICE         L13           
         BNE   CONCLT1                                            L13           
*                                                                               
CONCLTX2 DS    0H                                                               
*                                  NEED TO READ PROFILE HERE                    
         MVC   PROFKEY,=CL12'P000'                                              
         MVC   PROFKEY+4(3),QAGENCY                                             
         MVC   PROFKEY+7(3),PCLTKCLT                                            
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   PROFKEY+10,C'*'                                                  
         MVC   PROFKEY+11(1),PCLTOFF                                            
         GOTO1 GETPROF,DMCB,PROFKEY,SYSPROF,DATAMGR                             
*                                                                               
         MVC   PROFKEY+2(2),=C'PW'       GET PW PROFILE                         
         GOTO1 (RF),(R1),,PWPROF                                                
*                                                                               
         MVC   PROFKEY+2(2),QPROG                                               
         GOTO1 (RF),(R1),,PROFOPTS                                              
         OC    PROFOPTS,PROFOPTS   SEE IF PROFILE FOUND            L14          
         BNZ   CONCLTX4                                            L14          
         CLC   QPROG,=C'13'       SEE IF PP13                      L14          
         BNE   CONCLTX4                                            L14          
         MVC   PROFKEY+2(2),=C'12'     TRY FOR PP12 PROFILE        L14          
         GOTO1 (RF),(R1),,PROFOPTS                                 L14          
CONCLTX4 DS    0H                                                  L14          
*                                                                               
*        NOW TRY TO READ P12A, P13A OR P15A PROFILE                             
*        AND READ INTO PROGPROA                                                 
*                                                                               
         MVC   PROFKEY+1(2),QPROG                                               
         MVI   PROFKEY+3,C'A'                                                   
         NI    PROFKEY,X'BF'        MAKE SYSTEM LOWER CASE                      
         GOTO1 (RF),(R1),,PROGPROA                                              
         OC    PROGPROA,PROGPROA   SEE IF PROFILE FOUND            L14          
         BNZ   CONCLTX7                                            L14          
         CLC   QPROG,=C'13'       SEE IF PP13                      L14          
         BNE   CONCLTX7                                            L14          
         MVC   PROFKEY+1(2),=C'12'     TRY FOR PP12 PROFILE        L14          
         GOTO1 (RF),(R1),,PROGPROA                                 L14          
*                                                                               
CONCLTX7 DS    0H                                                               
*                                                                               
         MVC   FAXOPT,PROGPROA+2                                                
         CLI   QPAY,C' '           SEE IF OVERRIDING PROFILE                    
         BNH   *+10                                                             
         MVC   FAXOPT,QPAY                                                      
*                                                                               
         CLI   PWPROF+1,C'Y'       SEE IF P12,P13,P15 FAXING ALLOWED            
         BE    *+8                                                              
         MVI   FAXOPT,C'N'                                                      
*                                                                               
         CLI   FAXOPT,C' '                                                      
         BH    *+8                                                              
         MVI   FAXOPT,C'N'                                                      
*                                                                               
         MVC   NAMOPT,PROGPROA                                                  
         MVC   MEMBOPT,PROGPROA+1                                               
         CLI   NAMOPT,C' '                                                      
         BH    CONCLTX8                                                         
         MVI   NAMOPT,C'N'                                                      
         TM    PAGYPROF+20,X'02'     7,5,4,3 (NO NAME)                          
         BZ    *+8                                                              
         MVI   NAMOPT,C'Y'           SUPPRESS AGENCY NAME                       
*                                                                               
CONCLTX8 CLI   MEMBOPT,C' '                                                     
         BH    CONCLTX9                                                         
         MVI   MEMBOPT,C'N'                                                     
         TM    PAGYPROF+20,X'01'     7,5,3,1 (NO MEMBER MESSAGE)                
         BZ    *+8                                                              
         MVI   MEMBOPT,C'Y'        SUPPRESS MEMBER MESSAGE                      
*****                                                                           
*                                  SET OPTIONS FROM PROFILE                     
CONCLTX9 CLC   QPROG,=C'15'                                                     
         BNE   MAYB12                                               L02         
*                                                                   L10         
         CLI   PROFOPTS+12,0                                        L10         
         BNE   *+8                                                  L10         
         MVI   PROFOPTS+12,C'N'    DO NOT SUPPRESS                  L10         
*                                                                   L10         
         CLI   PROFOPTS+11,C'B'                                     L08         
         BNE   YES15                                                L08         
         MVI   SSORTOPT,C'D'   PROCESS ON INSERT DATE               L08         
         B     YES15                                                L08         
*                                                                   L08         
MAYB12   CLC   QPROG,=C'13'     CONTRACT TURNAROUND                L14          
         BE    MAYBE12C         SAME AS CONTRACT                   L14          
         CLC   QPROG,=C'12'                                         L06         
         BNE   LAFPR                                                L06         
*                                                                   L09         
*                                                                   L09         
MAYBE12C CLI   QOPT1,C'B'       OVERRIDE PROFILE                    L09         
         BNE   *+12                                                 L09         
         MVI   QOPT1,C' '                                           L09         
         B     NOOPTSC                                              L09         
         CLI   QOPT1,C' '       IF ANYTHING WAS ENTERED IN REQUEST  L09         
         BH    NOOPTSC              REQUEST OVERRIDES               L09         
         CLI   PROFOPTS+12,C'C'   ANYTHING ENTERED IN REQUEST       L09         
         BL    NOOPTSC                                              L09         
         MVC   QOPT1,PROFOPTS+12                                    L09         
NOOPTSC  DS    0H                                                   L09         
         CLI   QOPT4,C'G'        WERE GRIDS REQUESTED               L09         
         BNE   *+12                                                 L09         
         MVI   SSORTOPT,C'P'     ASSUME IN PRODUCT ORDER                        
         B     LAFPR                                                            
         CLI   PROFOPTS+6,C'B'   IF PRODUCT AND PRODUCT CODE        L06         
         BNE   LAFPR             SUPPRESSION / THEN FORCE SORT TO   L06         
*                                PROCESS ON INSERT DATE..                       
FORCED   MVI   SSORTOPT,C'D'                                        L06         
LAFPR    LA    RF,PROFOPTS+3                                        L02         
         LA    RE,5                                                 L02         
DOVER    CLI   0(RF),X'40'                                          L02         
         BH    *+8                                                  L02         
         MVI   0(RF),C'N'       FORCE TO NO                         L02         
         LA    RF,1(RF)                                             L02         
         BCT   RE,DOVER                                             L02         
         B     CON08                                                L02         
*                                                                   L02         
YES15    DS    0H                                                   L02         
*                                                                               
         MVC   QOPT3,PROFOPTS+3                                                 
*                                                                               
         MVI   QOPT1,C' '                                                       
         CLI   PROFOPTS+4,C'Y'                                                  
         BNE   *+8                                                              
         MVI   QOPT1,C'S'                                                       
*                                                                               
         MVI   QOPT2,C' '                                                       
         CLI   PROFOPTS+5,C'N'   IF N OR X MOVE TO QOPT2                        
         BNE   *+8                                                              
         MVI   QOPT2,C'N'                                                       
         CLI   PROFOPTS+5,C'X'                                                  
         BNE   *+8                                                              
         MVI   QOPT2,C'X'                                                       
*                                                                               
         MVI   QOPT5,C'F'                                                       
         CLI   PROFOPTS+6,C'Y'                                                  
         BNE   *+8                                                              
         MVI   QOPT5,C'C'                                                       
*                                                                               
         MVC   QOPT6(1),PROFOPTS+7                                              
         CLI   QOPT6,C'N'                                                       
         BNE   *+8                                                              
         MVI   QOPT6,C' '                                                       
*                                                                               
* BUG01  MVI   QOPT4,C' '                                                       
* BUG01  CLI   PROFOPTS+8,C'Y'                                                  
* BUG01  BNE   *+8                                                              
* BUG01  MVI   QOPT4,C'G'                                                       
*****                                                                           
*                                                                               
CON08    CLC   =C'ALL',QPRODUCT                                                 
         BE    CON10                                                            
         CLC   =C'   ',QPRODUCT                                                 
         BE    CON10                                                            
         MVI   KEY+3,6                                                          
*                                  SEE IF DOING ONE SLAVE CLT                   
         CLI   QDIV,C' '                                                        
         BE    *+10                                                             
         MVC   KEY+4(3),QDIV       SLAVE CLIENT                                 
         MVC   KEY+7(3),QPRODUCT                                                
         BAS   RE,HIGH1                                                         
         CLC   KEYSAVE(10),KEY                                                  
*****    BE    *+6                                                              
*****    DC    H'0'      PRODUCT NOT FOUND                                      
         BNE   CON10     PRODUCT NOT FOUND - LEAVE SVPRDNAM NULLS               
         LA    R0,PPRDREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
         MVC   SVPRDNAM,PPRDNAME        SAVE PRD NAME                           
*                                                                               
CON10    XC    BSTART(6),BSTART                                                 
         MVC   BEND,=3X'FF'                                                     
         CLC   QSTART,SPACES       TEST DATE PARAM SPECIFIED                    
         BE    CON10A              NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
         GOTO1 DATCON,(R1),(0,QEND),(3,BEND)                                    
*                                                                               
CON10A   CLC   QPUB,SPACES                                                      
         BNE   *+10                                                             
         MVC   QPUB(3),=C'ALL'                                                  
*                                                                               
         CLC   QPUB(3),=C'ALL'                                                  
         BNE   CON10B                                                           
         CLC   =C'000',QEST                                                     
         BE    CON10A1                                                          
         CLC   QEST,SPACES         MUST NOT SPECIFY CONTRACT                    
         BNE   CONERR                                                           
CON10A1  DS    0H                                                               
         CLC   QSTART,SPACES       AND MUST NAME DATE                           
         BE    CONERR                                                           
         B     CON12                                                            
CON10B   CLC   QSTART,SPACES       MUST NAME DATE OR CONTRACT                   
         BNE   CON12                                                            
         CLC   QEST,SPACES                                                      
         BNE   CON12                                                            
CONERR   MVC   P(L'ERRMSG),ERRMSG                                               
         MVC   PSECOND(80),QRECORD                                              
         MVI   RCWHATPR,X'01'                                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
ERRMSG   DC    C'INVALID PUB/CONTRACT/DATE SPECIFICATION'                       
         SPACE 2                                                                
CON12    XC    KEY,KEY             BUILD CONTRACT KEY                           
         MVC   KEY(7),PCLTKEY      A/M/X/CLT                                    
         MVC   CONAGY,PAGYKAGY                                                  
*                                                                               
* OLD AOR (DUPONT) DISABLED                                                     
*                                                                               
******** CLI   PCLTAGYR,0          IF SLAVE AGY                                 
******** BE    *+16                                                             
******** MVC   KEY(2),PCLTAGYR     USE AGY OF REC                               
******** MVC   CONAGY,PCLTAGYR                                                  
*                                                                               
         MVI   KEY+3,X'10'                                                      
         CLC   =C'ALL',QPUB                                                     
         BNE   CON12B                                                           
         CLC   =C'000',QEST                                                     
         BNE   CON16                                                            
*                                                                               
         CLC   QAGENCY,=C'GZ'      GM PLANWORKS ONLY                            
         BNE   CON12A                                                           
         CLI   PCLTPROF+5,C'1'     MASTER CLIENT?                               
         BNE   CON12A                                                           
         XC    KEY+4(3),KEY+4      CLR MAST CLT SO BUYS CAN BE FOUND            
*                                                                               
CON12A   CLI   QDIV,C' '           SLAVE REQUEST?                               
         BE    *+10                                                             
         MVC   KEY+4(3),QDIV       YES, USE SLAVE CLIENT                        
*                                                                               
         MVI   KEY+3,X'21'         SET 21 POINTER                               
         B     CON16                                                            
         EJECT                                                                  
* ONE PUB                                                                       
CON12B   DS    0H                                                               
         MVC   WORK(11),QPUB                                                    
         CLI   QEDITION,C'Z'                                                    
         BNE   *+8                                                              
         MVI   WORK+10,C' '                                                     
         CLI   QZONE,C'Z'                                                       
         BNE   *+10                                                             
         MVC   WORK+8(2),SPACES                                                 
         GOTO1 PUBVAL,DMCB,WORK,REQPUB                                          
         MVC   CONPUB,REQPUB                                                    
*                                  IF 'SLAVE' AGY - DO PUB CONVERSION           
*                                  IF NECESSARY                                 
****                                                                            
****     OLD AOR (DUPONT) DISABLED                                              
****                                                                            
****     CLI   PCLTAGYR,0                                                       
****     BE    CON12B20                                                         
****     CLC   PCLTAGYR,PAGYKAGY                                                
****     BE    CON12B20                                                         
****     TM    PCLTACTL,X'80'                                                   
****     BZ    CON12B20                                                         
****                                                                            
****     MVC   PCONKEY,KEY         SAVE KEY                                     
****     MVC   KEY(1),PAGYKMED                                                  
****     MVC   KEY+1(6),CONPUB                                                  
****     MVC   KEY+7(2),PAGYKAGY                                                
****     MVI   KEY+9,X'81'                                                      
****                                                                            
****     BAS   RE,HIGHPUB1                                                      
****     CLC   KEY(5),KEYSAVE                                                   
****     BNE   CONERR                                                           
****     CLC   KEY+7(3),KEYSAVE+7  NOTE - NO ZZ ALLOWED FOR SLAVE               
****     BNE   CONERR                                                           
****     CLI   QZONE,C'Z'                                                       
****     BE    *+14                                                             
****     CLC   KEY+5(1),KEYSAVE+5                                               
****     BNE   CONERR                                                           
****     CLI   QEDITION,C'Z'                                                    
****     BE    *+14                                                             
****     CLC   KEY+6(1),KEYSAVE+6                                               
****     BNE   CONERR                                                           
****     LA    R0,PUBREC                                                        
****     ST    R0,IOAREA                                                        
****     BAS   RE,GETPUB                                                        
****                                                                            
****     MVC   KEY,PCONKEY         RESTORE KEY                                  
****     LA    R2,PUBREC+33                                                     
****     MVI   ELCODE1,X'14'                                                    
****     MVC   DUB(6),PUBKPUB                                                   
****2B6  DS    0H                                                               
****     BAS   RE,NEXTEL                                                        
****     BNE   CON12B8                                                          
****     USING PUBREPEL,R2                                                      
****     CLC   PUBRPOFF,PCLTKCLT                                                
****     BNE   CON12B6                                                          
****     MVC   DUB(6),PUBCVEN                                                   
****                                                                            
****2B8  DS    0H                                                               
****     CLI   QZONE,C'Z'                                                       
****     BNE   *+8                                                              
****     MVI   DUB+4,0                                                          
****     CLI   QEDITION,C'Z'                                                    
****     BNE   *+8                                                              
****     MVI   DUB+5,0                                                          
****                                                                            
****     MVC   CONPUB,DUB                                                       
****                                                                            
****     END OD OLD AOR (DUPONT) LOGIC                                          
****                                                                            
CON12B20 DS    0H                                                               
         MVC   KEY+7(6),CONPUB                                                  
CON12C   CLC   QZONE,=C'ZZ'          SEE IF DOING ALL ZONES                     
         BNE   CON12D                                                           
         MVI   KEY+11,0                                                         
*                                                                               
CON12D   CLC   QEST,SPACES         TEST CON SPECIFIED                           
         BNE   CON14               NO                                           
* ONE PUB/ALL CONTRACTS                                                         
         BAS   RE,HIGH1                                                         
*                                                                               
CON13    CLI   QEDITION,C'Z'        SEE IF DOING ALL EDITIONS                   
         BNE   CON13C                                                           
         LA    R7,CON13A            RETURN ADDRESS FOR CON20 ROUTINE            
CON13A   CLC   KEY(11),KEYSAVE          SAME A/M/X/CLT/8 DIGITS                 
         BNE   CONCLT                                                           
         CLC   QZONE,=C'ZZ'          SEE IF DOING ALL ZONES                     
         BE    CON20                                                            
         CLC   KEY(12),KEYSAVE                                                  
         BNE   CONCLT                                                           
         B     CON20                                                            
*                                                                               
CON13C    CLC   KEY(13),KEYSAVE     SAME A/M/X/CLT/PUB                          
         BNE   CONCLT                                                           
         B     CON20               GO FILTER ON DATES                           
*                                                                               
* ONE PUB/ONE CONTRACT                                                          
*                                                                               
CON14    DS    0H                                                               
         CLC   =C'000',QEST                                                     
         BE    CON22                                                            
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   KEY+13(2),HALF                                                   
         BAS   RE,HIGH1                                                         
         CLI   QEDITION,C'Z'        SEE IF DOING ALL EDITIONS                   
         BNE   CON14B                                                           
         LA    R7,CON14A           RETURN ADDRESS FOR CON20 ROUTINE             
CON14A   CLC   KEY(11),KEYSAVE        SAME A/M/CLT/8 DIGITS                     
         BNE   CONCLT                                                           
         CLC   QZONE,=C'ZZ'          SEE IF DOING ALL ZONES                     
         BE    CON14A1                                                          
         CLC   KEY(12),KEYSAVE                                                  
         BNE   CONCLT                                                           
CON14A1  CLC   KEY+13(2),KEYSAVE+13                                             
         BE    CON20                                                            
         BAS   RE,SEQ1                                                          
         B     CON14A                                                           
*                                                                               
CON14B   CLC   KEY(15),KEYSAVE     SAMEA/M/X/CLT/PUB/CON                        
         BNE   CONCLT                                                           
         B     CON20                                                            
*                                                                               
*ALL PUBS/ALL CONTRACTS                                                         
*                                                                               
CON16    BAS   RE,HIGH1                                                         
         B     *+8                                                              
*                                                                               
CON17    DS    0H                                                               
         BAS   RE,SEQ1                                                          
*                                                                               
CON17A   DS    0H                                                               
         MVC   SAVBKEY,KEY                                                      
*                                                                               
         CLC   QAGENCY,=C'GZ'      GM PLANWORKS ONLY                            
         BNE   CON17D                                                           
         CLI   PCLTPROF+5,C'1'     MASTER CLIENT?                               
         BNE   CON17D                                                           
         CLC   KEY(4),KEYSAVE      COMPARE UP TO MASTER CLIENT                  
         BNE   CONCLT                                                           
         B     CON17F                                                           
*                                                                               
CON17D   CLC   KEY(7),KEYSAVE      TEST SAME A/M/X/CLT                          
         BNE   CONCLT                                                           
CON17F   CLC   =C'000',QEST                                                     
         BNE   CON20                                                            
         CLC   KEY+16(3),BSTART                                                 
         BL    CON17                                                            
         CLC   KEY+16(3),BEND                                                   
         BH    CON17                                                            
*                                                                               
* USED TO READ ESTHDR HERE TO SEE IF IT WAS TEST                                
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
         CLI   PBDBFD,C'T'                                                      
         BE    CON17               SKIP TEST BUYS                               
*                                                                               
         TM    PBDSTAT,X'08'                                                    
         BO    CON17               AND "HELD" CASH IN ADVANCE BUYS              
*                                                                               
         OC    SVESTFLT,SVESTFLT   FILTERING ON ESTIMATE ?                      
         BZ    CONESTX             NO                                           
         CLC   SVESTFLT,KEY+19                                                  
         BNE   CON17               EST DOESN'T MATCH, SKIP                      
CONESTX  DS    0H                                                               
*                                                                               
*                                                                               
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    CON22                                                            
         CLI   QPRODUCT,C' '                                                    
         BE    CON22                                                            
         CLC   QPRODUCT,KEY+13     PRODUCT REQ                                  
         BH    CON17                                                            
         BL    CON92A              NEXT PUB                                     
         B     CON22                                                            
         EJECT                                                                  
*                                                                               
* GET CONTRACT REC AND FILTER ON DATES                                          
*                                                                               
CON20    L     R4,ACONIO1          A(PCONREC)                                   
         ST    R4,IOAREA                                                        
         BAS   RE,GET                                                           
*                                                                               
         USING PCONREC,R4          NOTE TEMPORARY USE OF R4                     
*                                                                               
         CLI   QEDITION,C'Z'                                                    
         BNE   CON21E                                                           
*                                                                               
*                       IF DOING ALL EDITIONS CHECK ALL ENTRIES                 
*                       ADDED TO CONTRACT REGISTER FOR THIS                     
*                       REQUEST FOR THE SAME                                    
*                       CLT/PUB/NUMBER/DATES                                    
*                       SO I WON'T REPRINT A CONTRACT                           
*                                                                               
         L     R5,AFSTCON                                                       
CON21    CLI   0(R5),0       END OF REGISTER                                    
         BE    CON21B                                                           
         CLC   1(3,R5),PCONKCLT                                                 
         BNE   CON21A                                                           
         CLC   4(4,R5),PCONKPUB         8 DIGITS                                
         BNE   CON21A                                                           
         CLC   10(2,R5),PCONNUM          CONTRACT NUMBER                        
         BNE   CON21A                                                           
         CLC   12(6,R5),PCONSDT         START AND END DATES                     
         BNE   CON21A                                                           
         CLI   18(R5),C'A'         IS THIS A PRD CON                            
         BL    CON21A0             NO                                           
         CLC   18(3,R5),PCONPRD    YES - CHECK IF I'VE DONE THIS PRD            
         BNE   CON21A              NO                                           
         B     CON21A1             YES                                          
*                                                                               
CON21A0  CLI   PCONPRD,C'A'                                                     
         BNL   CON21A              HAVEN'T DONE TIS CON                         
*                                  HAVE SO DON'T REPRINT IT                     
CON21A1  DS    0H                                                               
         BAS   RE,SEQ1        READ NEXT CONTRACT                                
         BR    R7        RETURN TO PROPER ROUTINE                               
*                                                                               
CON21A   LA    R5,24(R5)        NEXT ENTRY IN REGISTER                          
         B     CON21                                                            
*                                                                               
CON21B   CLC   PCONEDT,BSTART                                                   
         BL    CON21C                                                           
         CLC   BEND,PCONSDT                                                     
         BL    CON21C                                                           
*                                  SEE IF DOING ONE PRD                         
*                                  IF SO DON'T PROCESS OTHER PRD CONS           
         JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',CON23,JUMP=N          
         CLI   PCONPRD,C'A'                                                     
         BL    CON23                                                            
         CLC   PCONPRD,QPRODUCT                                                 
         BE    CON23                                                            
         B     CON21C                                                           
*                                                                               
CON21C   BAS   RE,SEQ1                                                          
         BR    R7        RETURN TO PROPER ROUTINE                               
CON21E   CLC   PCONEDT,BSTART      CON END BEFORE REQ START                     
         BL    CON92A                                                           
         CLC   BEND,PCONSDT        REQ END BEFORE CON START                     
         BL    CON92A                                                           
*                                                                               
*                                  SEE IF DOING ONE PRD                         
*                                  IF SO DON'T PROCESS OTHER PRD CONS           
         JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',CON23,JUMP=N          
         CLI   PCONPRD,C'A'                                                     
         BL    CON23                                                            
         CLC   PCONPRD,QPRODUCT                                                 
         BE    CON23                                                            
         B     CON92A                                                           
*                                                                               
*                                  SET DUMM Y CONTRACT                          
CON22    DS    0H                                                               
         L     R4,ACONIO1          A(PCONREC)                                   
         XC    PCONREC(256),PCONREC                                             
         MVC   PCONKEY(7),PCLTKEY                                               
         MVI   PCONKEY+3,X'10'                                                  
         MVC   PCONKPUB(6),KEY+7   PUB                                          
         XC    PCONNUM,PCONNUM                                                  
         MVC   PCONSDT(6),BSTART                                                
*                                                                               
*                                                                               
CON23    DS    0H                                                               
         MVC   CSTART(6),PCONSDT                                                
         CLI   QEST,C' '                                                        
         BE    CON30                                                            
         CLI   QSTART,C' '                                                      
         BE    CON30                                                            
         MVC   CSTART(6),BSTART                                                 
         EJECT                                                                  
* GET PUB RECORDS                                                               
CON30    XC    KEY,KEY                                                          
         MVC   KEY(1),PCONKEY+2    MEDIA                                        
         MVC   SAVCKPUB,PCONKPUB                                                
         MVC   SAVCKAGY,PCONKAGY                                                
         OC    REQPUB,REQPUB                                                    
         BZ    *+16                                                             
         MVC   SAVCKPUB,REQPUB     IF SLAVE AGY USE REQUESTED PUB               
         MVC   SAVCKAGY,PAGYKAGY   AND AGENCY                                   
         MVC   KEY+1(6),SAVCKPUB                                                
         MVC   KEY+7(2),SAVCKAGY                                                
         CLI   QEDITION,C'Z'                                                    
         BNE   *+10                                                             
         XC    KEY+5(2),KEY+5      IF ALL ED'S TRY FOR 'BASE' PUB               
         MVI   KEY+9,X'81'                                                      
CON301   DS    0H                                                               
         BAS   RE,HIGHPUB1                                                      
         CLC   KEY(10),KEYSAVE     CHECK FOUND X'81' REC                        
         BE    CON32               YES                                          
*                                                                               
         CLI   QEDITION,C'Z'          SEE IF DOING ALL EDITIONS                 
         BNE   CON30E       NO - GO FIND ZZ REC                                 
         CLC   KEY(5),KEYSAVE            CHECK 8 DIGITS                         
         BNE   CONERR        INVALID PUB                                        
         CLC   QZONE,=C'ZZ'         SEE IF DOING ALL ZONES                      
         BE    CON30A                                                           
         CLC   KEY(6),KEYSAVE        CHECK ZONE                                 
         BE    CON30A                                                           
         CLC   QEST,=C'000'         SEE IF DOING DUMMY CONTRACT                 
         BNE   CON30E                                                           
         B     CON31                                                            
*                                                                               
CON30A   CLI   KEY+9,X'81'           NO 81 REC                                  
         BNE   CON30B                                                           
         CLC   KEY+7(2),SAVCKAGY          CHECK RIGHT AGY                       
         BE    CON32         YES - OK TO READ                                   
         BL    CON30A2                                                          
         CLC   KEY+7(2),=C'ZZ'                                                  
         BE    CON32                                                            
         MVC   KEY(5),KEYSAVE                                                   
         CLC   QZONE,=C'ZZ'                                                     
         BE    *+10                                                             
         MVC   KEY+5(1),KEYSAVE+5          PRESERVE ZONE                        
         ZIC   RE,KEY+6                                                         
         LA    RE,1(RE)            INCREMENT EDITION                            
         STC   RE,KEY+6            AND TRY AGAIN                                
CON30A2  MVC   KEY+7(2),SAVCKAGY                                                
         B     CON301                                                           
*                                                                               
*          GO FIND ZZ REC                                                       
CON30B   MVC   KEYSAVE+7(2),=C'ZZ'                                              
         MVC   KEY,KEYSAVE                                                      
         BAS   RE,HIGHPUB1                                                      
         CLC   KEY(5),KEYSAVE        CHECK 8 DIGITS                             
         BNE   CONERR        INVALID PUB                                        
         CLC   QZONE,=C'ZZ'         SEE IF DOING ALL ZONES                      
         BE    CON32                                                            
         CLC   KEY(6),KEYSAVE        CHECK ZONE                                 
         BE    CON32                                                            
         B     CON31      INVALID PUB                                           
*                                                                               
*                                                                               
CON30E   CLC   KEY+7(2),=C'ZZ'     CHECK FOUND ZZ X'81' REC                     
         BE    CON32               YES                                          
*                                                                               
         MVC   KEYSAVE+7(2),=C'ZZ' TRY FOR DEFAULT                              
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BAS   RE,HIGHPUB1                                                      
         CLC   KEY(10),KEYSAVE     MUST FIND DEFAULT                            
         BE    CON32                                                            
*                                                                               
CON31    DS    0H                                                               
         CLI   QEDITION,C'Z'                                                    
         BNE   CONERR                                                           
         CLC   KEYSAVE+5(2),SAVCKPUB+4                                          
         BE    CONERR                                                           
         MVC   KEYSAVE+5(2),SAVCKPUB+4   'BASE' PUB NOT FOUND                   
         MVC   KEY,KEYSAVE                 TRY FOR CONTRACT PUB                 
         B     CON301                                                           
*                                                                               
CON32    DS    0H                                                               
         CLI   PAGYPROF+16,C'1'                                                 
         BE    CON32A                                                           
         CLC   KEY+7(2),=C'ZZ'     SRDR NOT ALLOWED                             
         BE    CONERR                                                           
CON32A   DS    0H                                                               
         LA    R0,PUBREC                                                        
         ST    R0,IOAREA                                                        
         BAS   RE,GETPUB                                                        
         MVC   PUBKED,KEY+6          RESET EDITION CODE                         
*                                  TRY FOR CON ADDR FIRST                       
*                                 IF SLAVE REQUEST FIRST TRY FOR                
*                                 ADDRESS OVERRIDE FOR SLAVE                    
*                                 CLIENT OR OFFICE                              
*                                                                               
         XC    PREPNAME,PREPNAME                                  BUG02         
         MVC   ADRCLT,PCLTKCLT                                                  
         MVC   ADROFF,PCLTOFF                                                   
         CLI   QDIV,C' '                                                        
         BE    CON32A5                                                          
*****    MVC   ADRCLT,PDIVKCLT    SINCE SLAVE CLT IS IN PDIVREC                 
*****    MVC   ADROFF,PCLTOFF-PCLTREC+PDIVREC                                   
*                                 SINCE SLAVE CLT IS IN SLAVEIO                 
         MVC   ADRCLT,PCLTKCLT-PCLTREC+SLAVEIO                                  
         LA    R2,SLAVEIO     "SLAVE" OFFICE NOT ADDRESSABLE OTHERWISE          
         MVC   ADROFF,PCLTOFF-PCLTREC(R2)                                       
*                                                                               
CON32A5  XC    PREPKEY,PREPKEY                                                  
         XC    DUB,DUB                                                          
*                                                                               
         MVI   ADRTYP,C'C'         CONTRACT                                     
*                                    FILL IN 7 BYTES OF CLTDATA                 
         MVC   CLTAGY,PUBKAGY        AGY                                        
         MVC   CLTMED,PUBKMED        MED                                        
         MVC   CLTCODE,ADRCLT        CLIENT CODE                                
         MVC   CLTOFF,ADROFF         CLIENT OFFICE                              
*                                                                               
         GOTO1 =V(PPGETADR),DMCB,(ADRTYP,CLTDATA),PUBREC,DATAMGR,0              
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),0             ADDRESS RECORD FOUND ?                       
         BE    CON32H              NO - LOOK FOR REP                            
*                                                                               
*                                  ADDRESS REC FOUND                            
CON32D   DS    0H                                                               
         MVC   DUB(3),1(R1)        ADDRESS LEVEL                                
         L     R2,4(R1)            A(ADDRESS INFO FROM CALL)                    
         ST    R2,FULL             SAVE A(ADDRESS)                              
         CLI   DUB,X'FF'           TEST HAVE CLIENT ADDR                        
         BL    CON34               YES - DONT LOOK FOR REP                      
*                                                                               
CON32H   DS    0H                                                               
         MVI   ELCODE1,X'14'       FIND REP ELEM                                
         LA    R2,PUBREC+33                                                     
         CLC   ELCODE1,0(R2)                                                    
         BE    *+8                                                              
CON33    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   CON34                                                            
         USING PPDUMD14,R2                                                      
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    CON33B                                                           
         CLC   PUBRPOFF,ADRCLT     RPOFF IS REALLY CLIENT CODE                  
         BE    CON33B                                                           
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   CON33                                                            
         CLC   PUBRPOFF+1(1),ADROFF                                             
         BNE   CON33                                                            
CON33B   DS    0H                                                               
         OC    PUBPAREP(12),PUBPAREP    TEST ANY OVERIDES                       
         BZ    CON33               NO - USE DEFAULT                             
         OC    PUBCNREP,PUBCNREP                                                
         BZ    CON34                                                            
         MVC   DUB+3(3),PUBRPOFF                                                
CON33D   DS    0H                                                               
         OC    DUB(3),DUB                                                       
         BZ    *+14                                                             
         CLC   DUB(3),DUB+3        TEST 'LEVEL'                                 
         BNH   CON34               ADDR MORE SPECIFIC                           
         EJECT                                                                  
* GET REP RECORD                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKAGY                                                   
         MVC   KEY+2(1),PCONKMED                                                
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),PUBCNREP                                                
         BAS   RE,HIGH1                                                         
         CLC   KEY(11),KEYSAVE                                                  
         BNE   CON40                                                            
         LA    R0,PREPREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
         B     CON40                                                            
         SPACE 2                                                                
CON34    DS    0H                                                               
         OC    DUB(3),DUB                                                       
         BZ    CON40               NO ADDR ELEM                                 
         L     R2,FULL                                                          
         USING PGETADRD,R2                                                      
*                                                                               
         XC    PREPELEM(166),PREPELEM        PREPELEM IS 166 BYTES              
*                                                                               
         MVC   PREPELEM(2),PGADELEM                                             
         MVC   PREPNAME(122),PGADNAME        MOVE UP TO TELEPHONE #             
*                                                                               
         OC    PGADFAX,PGADFAX               FAX?                               
         BZ    CON34D                        NO                                 
         MVC   PREPFAX(12),PGADFAX                                              
         MVC   PREPLIN3(26),PGADLIN3                                            
*                                                                               
CON34D   MVC   PREPKEY(3),PCONKEY                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
CON40    DS    0H                                                               
*                                                                               
*                               SEE IF I FOUND ADDRESS                          
         OC    PREPNAME(20),PREPNAME                                            
         BNZ   CON40C                                                           
         CLI   QDIV,C' '           SEE IF DOING SLAVE REQUEST                   
         BE    CON40C                                                           
*****    CLC   ADRCLT,PDIVKCLT      SEE IF I WAS LOOKING FOR SLAVE              
         CLC   ADRCLT,PCLTKCLT-PCLTREC+SLAVEIO                                  
         BNE   CON40C               NO THE I'M DONE                             
         MVC   ADRCLT,PCLTKCLT      YES - NOW TRY FOR MASTER CLIENT             
         MVC   ADROFF,PCLTOFF       ADDRESS/REP                                 
         B     CON32A5                                                          
*                                                                               
CON40C   MVI   RATESW,0                                                         
         XC    PAGE,PAGE                                                        
         MVI   PAGE+1,1                                                         
         GOTO1 ACONHEAD                                                         
*                                       PRINT 'SUBS'                            
         L     R2,AMHSUBS                                                       
         LA    R5,MHSUBN                                                        
CON41    DS    0H                                                               
         OC    0(80,R2),0(R2)                                                   
         BZ    CON41A                                                           
         MVC   P(80),0(R2)                                                      
         BAS   RE,CONPRT                                                        
         LA    R2,080(R2)                                                       
         BCT   R5,CON41                                                         
*                                                                               
CON41A   DS    0H                                                               
         MVC   SCONVDTE,=3X'FF'                                                 
         XC    SCONVFAC,SCONVFAC                                                
         MVI   SCONVIND,0                                                       
         CLC   =C'000',QEST                                                     
         BE    CON81                                                            
*                                                                               
*                                  SEE IF ANY RATE ELEMS PRESENT                
         MVC   SCONPUB,PCONKPUB        SAVE FIRST PUB                           
         MVC   SCONSDT,PCONSDT         SAVE START AND END DATES                 
         MVC   SCONEDT,PCONEDT         CONS FOR OTHER EDITIONS MUST             
*                                      HAVE SAME NUMBER AND DATES TO            
*                                      APPEAR ON THIS CONTRACT                  
         MVC   SCONPRD,PCONPRD     SAVE PRD CODE                                
CON41A1  LA    R2,PCONREC+33                                                    
         MVI   ELCODE1,X'20'                                                    
         BAS   RE,NEXTEL                                                        
         BNE   CON60               NO                                           
*                                                                               
         CLC   SVQPROG,=C'15'                                                   
         BNE   CON41A4                                                          
         CLI   QOPT1,C'S'                                                       
         BE    CON60A                                                           
*                                                                               
CON41A4  CLI   RATESW,C'R'                                                      
         BE    CON41A8                                                          
         MVI   RATESW,C'R'                                                      
         MVI   SPACING,2                                                        
         MVC   P+07(19),=C'CONTRACT RATE BASIS'                                 
         BAS   RE,CONPRT                                                        
*                                                                               
         LA    RF,PROFOPTS+5       PRINT EFFECTIVE DATE                         
         BAS   RE,OPTIONCH                                                      
         B     *+14                SUPPRESS TITLE AND DATE                      
         B     *+10                SUPPRESS TITLE ONLY                          
         MVC   P+58(4),=C'RATE'                                                 
         BAS   RE,CONPRT                                                        
*                                                                               
         CLI   PROFOPTS+10,C'L'    LEVEL SUPPRESSION                            
         BE    *+8                                                              
         CLI   PROFOPTS+10,C'B'    LEVEL AND PERCENT SUPPRESSION                
         BE    *+10                                                             
         MVC   P+07(05),=C'LEVEL'                                               
         CLI   PROFOPTS+10,C'P'    PERCNT SUPPRESSION                           
         BE    *+8                                                              
         CLI   PROFOPTS+10,C'B'    LEVEL AND PERCENT SUPPRESSION                
         BE    *+10                                                             
         MVC   P+18(07),=C'PERCENT'                                             
         MVC   P+31(04),=C'RATE'                                                
         MVC   P+40(11),=C'DESCRIPTION'                                         
         LA    R1,PSECOND                                                       
         LA    RF,PROFOPTS+5       PRINT EFFECTIVE DATE                         
         BAS   RE,OPTIONCH                                                      
         B     LEAPO               NO MOVE                                      
         B     LEAPO               NO TITLE                                     
         MVC   58(09,R1),DASHES1                                                
         MVC   P+58(09),=C'EFF. DATE'                                           
LEAPO    DS    0H                                                               
*                                                                               
         CLI   PROFOPTS+10,C'L'    LEVELT SUPPRESSION                           
         BE    *+8                                                              
         CLI   PROFOPTS+10,C'B'    LEVEL AND PERCENT SUPPRESSION                
         BE    *+10                                                             
*                                                                               
         MVC   06(08,R1),DASHES1   LEVEL UNERLINE                               
         CLI   PROFOPTS+10,C'P'    PERCNT SUPPRESSION                           
         BE    *+8                                                              
         CLI   PROFOPTS+10,C'B'    LEVEL AND PERCENT SUPPRESSION                
         BE    *+10                                                             
*                                                                               
         MVC   18(07,R1),DASHES1                                                
         MVC   29(08,R1),DASHES1                                                
         MVC   40(11,R1),DASHES1                                                
         BAS   RE,CONPRT                                                        
*                                                                               
CON41A8  DS    0H                                                               
*                                                                               
* CHECK FOR RATELOOK REQUIRED                                                   
*                                                                               
         CLI   QEDITION,C'Z'       SEE IF DOING ALL EDITIONS                    
         BNE   CON41E                                                           
*                                                                               
CON41B   DS    0H                  GET PUB AND PRINT ZONE AND EDITION           
         XC    KEY,KEY                                                          
         MVC   KEY(1),PCONKEY+2    MEDIA                                        
         MVC   KEY+1(6),PCONKPUB                                                
         MVC   KEY+7(2),PCONKAGY                                                
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB1                                                      
         CLC   KEY(10),KEYSAVE                                                  
         BE    CON41C                                                           
         CLC   KEY+7(2),=C'ZZ'                                                  
         BE    CON41C                                                           
         MVC   KEYSAVE+7(2),=C'ZZ'                                              
         MVC   KEY,KEYSAVE                                                      
         BAS   RE,HIGHPUB1                                                      
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CON41C   LA    R0,PUBREC                                                        
         ST    R0,IOAREA                                                        
         BAS   RE,GETPUB                                                        
         MVC   PUBKED,KEY+6        SET EDITION CODE                             
         LA    R2,P+2                                                           
         CLI   PUBKZON,0                                                        
         BE    CON41D              NO ZONE                                      
         UNPK  DUB(3),PUBKZON(2)                                                
         MVC   P+2(2),DUB                                                       
         MVI   P+4,C'-'                                                         
         MVC   P+6(20),PUBZNAME                                                 
         LA    R2,PSECOND+6                                                     
CON41D   DS    0H                                                               
         CLC   QPROG,=C'13'        CONTRACT TURAROUND                           
         BE    CON41D5                                                          
*                                                                               
         CLC   QPROG,=C'12'        OR CONTRACTS                                 
         BNE   *+8                 CASCADE THRU                                 
CON41D5  CLI   QMEDIA,C'O'         AND OUTDOOR                                  
         BNE   *+8                 CASCADE THRU                                 
         CLI   PUBKED,0            AND NO EDITON                                
         BNE   *+8                 CASCADE THRU                                 
         CLI   PROFOPTS+11,C'Y'                                                 
         BNE   CON41DX                                                          
         MVC   0(7,R2),=C'POSTERS'                                              
         B     CON41DZ                                                          
CON41DX  GOTO1 PUBEDIT,DMCB,PUBKPUB,(C'E',0(R2))                                
CON41DZ  BAS   RE,CONPRT                                                        
*                                                                               
CON41E   DS    0H                                                               
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE1,X'20'                                                    
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   QMEDIA,C'N'                                                      
         BNE   CONRTX                                                           
         LA    R7,PRBIND-PRBELEM(R2)                                            
         TM    0(R7),X'80'                                                      
         BO    CONRTX                                                           
* AWAY WE GO                                                                    
         GOTO1 ARTLOOK,DMCB,0,PUBREC,PCONREC,ARTLKELS                           
*                                                                               
         CLI   0(R1),0             CHECK FOR ERRORS                             
         BNE   CONRTX                                                           
*                                                                               
         L     R2,ARTLKELS         POINT TO NEW ELEMENTS                        
*                                                                               
         DROP  R4                                                               
*                                                                               
CONRTX   B     *+12                                                             
CON51    BAS   RE,NEXTEL                                                        
         BNE   CON60                                                            
*NOP*    CLI   QMEDIA,C'N'         FOR NON-NEWSPAPERS ONLY                      
*NOP*    BE    CON51A              ONLY PRINT RATES FOR REQUESTED PRD           
         CLI   SVPRDNAM,0                                                       
         BE    CON51A            NOT DOING ONE PRODUCT                          
         LA    R4,PRBOPEN-PRBELEM(R2)                                           
         CLC   0(3,R4),QPRODUCT                                                 
         BE    CON51A                                                           
         CLI   0(R4),C'A'       CHK FOR PRODUCT CODE                            
         BL    CON51A           NO - APPLIES TO ALL PRODUCTS                    
         B     CON51            SKIP RATES FOR OTHER PRODUCTS                   
*                                                                               
CON51A   MVC   W,SPACES                                                         
*                                                                               
         LA    R4,PRBLEVEL-PRBELEM(R2)                                          
         CP    0(L'PRBLEVEL,R4),=P'0'        SEE IF LEVEL THERE                 
         BNZ   CON51A1                       YES                                
         SR    R0,R0                                                            
         CLI   PRBLIND-PRBELEM(R2),C'S'                                         
         BE    CON51BA                  JUST OUTPUT 'SPECIAL'                   
         MVC   W+06(4),=C'OPEN'                                                 
         LA    R4,PRBIND-PRBELEM(R2)                                            
         TM    0(R4),X'01'                                                      
         BZ    CON54                                                            
         MVC   W+06(4),=C'FLAT'                                                 
         B     CON54                                                            
*                                                                               
CON51A1   DS    0H                                                              
         LA    R7,PRBLEVEL-PRBELEM(R2)                                          
         CLI   PRBLIND-PRBELEM(R2),C'N'     NET $                               
         BE    CON51A2                                                          
         CLI   PRBLIND-PRBELEM(R2),C'$'                                         
         BNE   CON51B                                                           
CON51A2  DS    0H                                                               
         EDIT  (P5,(R7)),(11,W+06),ALIGN=LEFT,COMMAS=YES,FLOAT=$                
*                                                                               
         B     CON54                                                            
*                                                                               
CON51B   DS    0H                                                               
         EDIT  (P5,(R7)),(9,W+06),ALIGN=LEFT,COMMAS=YES                         
*                                                                               
CON51BA  LA    R6,W+06                                                          
         AR    R6,R0                                                            
         LA    R7,PRBLIND-PRBELEM(R2)                                           
CON52    CLI   0(R7),C'X'                                                       
         BNE   *+10                                                             
         MVC   1(05,R6),=C'TIMES'                                               
         CLI   0(R7),C'P'                                                       
         BNE   *+10                                                             
         MVC   1(05,R6),=C'PAGES'                                               
         CLI   0(R7),C'L'                                                       
         BNE   *+10                                                             
         MVC   1(05,R6),=C'LINES'                                               
         CLI   0(R7),C'I'                                                       
         BNE   *+10                                                             
         MVC   1(06,R6),=C'INCHES'                                              
         CLI   0(R7),C'U'                                                       
         BNE   *+10                                                             
         MVC   1(06,R6),=C'ISSUES'                                              
         CLI   0(R7),C'S'                                                       
         BNE   *+10                                                             
         MVC   1(07,R6),=C'SPECIAL'                                             
*                                                                               
CON54    LA    R4,PRBPCT-PRBELEM(R2)                                            
         CP    0(L'PRBPCT,R4),=P'0'                                             
         BZ    CON55                                                            
         EDIT  (P3,(R4)),(5,W+20),2                                             
         CLC   W+22(3),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   W+22(3),SPACES                                                   
CON55    DS    0H                                                               
*                                                                               
         CLI   PROFOPTS+10,C'L'  LEVELT SUPPRESSION                             
         BE    *+12                                                             
         CLI   PROFOPTS+10,C'B'    LEVEL AND PERCENT SUPPRESSION                
         BNE   *+10                                                             
*                                                                               
         MVC   W+6(11),SPACES    CLEAR LEVEL                                    
         CLI   PROFOPTS+10,C'P'  PERCNT SUPPRESSION                             
         BE    *+12                                                             
         CLI   PROFOPTS+10,C'B'    LEVEL AND PERCENT SUPPRESSION                
         BNE   *+10                                                             
*                                                                               
         MVC   W+20(5),SPACES       NO PERCENTAGE                               
*                                                                               
CON555   LA    R6,PRBRATE-PRBELEM(R2)                                           
         CP    0(5,R6),=P'0'                                                    
         BE    CON58                                                            
         LA    R7,PRBLIND-PRBELEM(R2)                                           
         TM    PRBIND-PRBELEM(R2),X'40'     TOTAL RATE                          
         BNZ   CON55B                                                           
         TM    PRBIND-PRBELEM(R2),X'20'      UNIT RATE                          
         BNZ   CON56                                                            
         CLI   QMEDIA,C'N'                                                      
         BE    CON56                                                            
         CLI   PRBLIND-PRBELEM(R2),C'L'                                         
         BE    CON56                                                            
         CLI   PRBLIND-PRBELEM(R2),C'I'                                         
         BE    CON56                                                            
CON55B   DS    0H                                                               
         TM    PRBIND-PRBELEM(R2),X'10'     NET L RATE                          
         BNO   *+8                                                              
         MVI   W+26,C'N'                                                        
         TM    PRBIND-PRBELEM(R2),X'02'         S RATE                          
         BNO   *+8                                                              
         MVI   W+26,C'S'                                                        
         TM    PRBIND-PRBELEM(R2),X'04'         C RATE                          
         BNO   *+8                                                              
         MVI   W+26,C'C'                                                        
         CP    0(5,R6),=P'100000000'  IF AMT IS 1 MILLION OR OVER               
         BL    CONLT1M                                                          
         EDIT  (P5,(R6)),(12,W+27),2,COMMAS=YES                                 
         B     CON58                                                            
CONLT1M  EDIT  (P5,(R6)),(10,W+27),2,COMMAS=YES                                 
         B     CON58                                                            
CON56    TM    PRBIND-PRBELEM(R2),X'10'     NET L RATE                          
         BNO   *+8                                                              
         MVI   W+26,C'N'                                                        
         TM    PRBIND-PRBELEM(R2),X'02'         S RATE                          
         BNO   *+8                                                              
         MVI   W+26,C'S'                                                        
         TM    PRBIND-PRBELEM(R2),X'04'         C RATE                          
         BNO   *+8                                                              
         MVI   W+26,C'C'                                                        
*CON56   DS    0H                                                               
         EDIT  (P5,(R6)),(10,WORK),5                                            
*                                                                               
         LA    R1,W+27                                                          
         CLC   WORK+7(3),=C'000'                                                
         BNE   *+10                                                             
         MVC   WORK+7(3),SPACES                                                 
         MVC   0(10,R1),WORK                                                    
*                                                                               
         LA    RF,=C'/I'                                                        
         CLI   PRBLIND-PRBELEM(R2),C'I'                                         
         BE    CON57                                                            
         TM    PRBIND-PRBELEM(R2),X'08' INCH RATE+NON-INCH LEVEL IND            
         BO    CON57                                                            
         LA    RF,=C'/L'                                                        
*****                      NOW ALWAYS SHOW /I OR /L                             
*******  CLI   PRBLIND-PRBELEM(R2),C'L'                                         
*******  BNE   CON58                                                            
*                                                                               
CON57    DS    0H                                                               
         LA    R1,10(R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   1(2,R1),0(RF)                                                    
*                                                                               
CON58    CLI   W+26,C'N'           NET INDICATOR                                
         BE    BING00                                                           
         CLI   W+26,C'S'           S RATE                                       
         BE    BING00              NOT S OR N                                   
         CLI   W+26,C'C'           C RATE                                       
         BNE   CON58A              NOT S,N,OR C                                 
BING00   LA    R7,W+26                                                          
         LA    R5,7                                                             
CON58AA  CLI   1(R7),C' '                                                       
         BH    MOVEIT                                                           
         LA    R7,1(R7)                                                         
         BCT   R5,CON58AA                                                       
MOVEIT   MVC   0(1,R7),W+26        MOVE EITHER S OR N                           
         CHI   R5,7                NO FLOAT                                     
         BE    *+8                                                              
         MVI   W+26,C' '                                                        
*                                                                               
CON58A   LA    R7,PRBDESC-PRBELEM(R2)                                           
*                                                                               
         LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
*                                                                               
         ST    R7,PBYOINPT                                                      
         MVC   PBYOINPT(1),QMEDIA                                               
         MVI   PBYOCTL,X'80'       ONLY SPACE INPUT                             
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*                                                                               
         CLC   PBYOSPC(2),=C'R='                                                
         BNE   CON58C                                                           
         MVC   W+40(5),PBYOSPC                                                  
         MVI   W+45,C','                                                        
         MVC   W+46(12),PBYOSPC+5                                               
         B     *+10                                                             
*                                                                               
CON58C   MVC   W+40(17),PBYOSPC                                                 
         CLI   PBYOSPC2,C' '                                                    
         BNH   CON5804                                                          
*                                                                               
         CLC   PBYOSPC(2),=C'R='                                                
         BNE   CON58D                                                           
         MVC   PSECOND+40(5),PBYOSPC2                                           
         MVI   PSECOND+45,C','                                                  
         MVC   PSECOND+46(12),PBYOSPC2+5                                        
         B     CON5804                                                          
*                                                                               
CON58D   MVC   PSECOND+40(17),PBYOSPC2                                          
         DROP  R5                                                               
CON5804  DS    0H                                                               
*                                                                               
*                                                                               
         LA    RF,PROFOPTS+5        PRINT EFFECTIVE DATE                        
         BAS   RE,OPTIONCH                                                      
         B     CON58Z               NO MOVE                                     
         B     *+4                  NO TITLE ONLY SUPPRESSION                   
         LA    R7,PRBDATE-PRBELEM(R2)                                           
         OC    0(3,R7),0(R7)                                                    
         BZ    CON58Z                                                           
         GOTO1 DATCON,DMCB,(3,(R7)),(5,W+58)                                    
*                                                                               
*NOP*CON58Z   CLI   QMEDIA,C'N'                                                 
*NOP*    BE    CON59                                                            
CON58Z   CLI   SVPRDNAM,0          SEE IF DOING ONE PRD                         
         BNE   CON59               YES - DON'T DISPLAY  -PRD                    
         LA    R7,PRBOPEN-PRBELEM(R2)      FOR RATES BY PRODUCT                 
         CLI   0(R7),C'A'                                                       
         BL    CON59                                                            
         MVI   W+66,C'-'                                                        
         MVC   W+67(3),0(R7)       MMMDD/YY-PRD                                 
*                                                                               
CON59    MVC   P,W                 ** MOVE WORK PRINT LINE  **                  
         BAS   RE,CONPRT                                                        
*                                                                               
         B     CON51                                                            
         EJECT                                                                  
* COMMENTS                                                                      
*                                                                               
CON60    DS    0H                                                               
         L     R4,ACONIO1          A(PCONREC)                                   
*                                                                               
         USING PCONREC,R4          NOTE TEMPORARY USE OF R4                     
*                                                                               
         CLI   QEDITION,C'Z'       SEE IF DOING ALL EDITIONS                    
         BNE   CON603                                                           
         IC    R1,PCONKPUB+5                                                    
         LA    R1,1(R1)                                                         
         STC   R1,PCONKPUB+5                                                    
         MVC   KEY,PCONKEY                                                      
         BAS   RE,HIGH1                                                         
CON600   CLC   KEYSAVE(11),KEY     CHECK SAME 8 DIGITS                          
         BNE   CON602                                                           
         CLC   QZONE,=C'ZZ'        SEE IF DOING ALL ZONES                       
         BE    CON600A                                                          
         CLC   KEYSAVE(12),KEY                                                  
         BNE   CON602                                                           
CON600A  CLC   KEY+13(2),KEYSAVE+13        CHECK SAME CONTRACT NUMBER           
         BE    CON601                                                           
CON600B  BAS   RE,SEQ1                                                          
         B     CON600                                                           
*                                                                               
CON601   LA    R0,PCONREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
*                                                                               
* CONTRACTS FOR THE OTHER EDITIONS MUST                                         
* HAVE THE SAME NUMBER AND DATES                                                
* OR THEY ARE BYPASSED                                                          
*                                                                               
         CLC   PCONSDT,SCONSDT                                                  
         BNE   CON600B                                                          
         CLC   PCONEDT,SCONEDT                                                  
         BNE   CON600B                                                          
         CLI   SCONPRD,C'A'        MUST BE SAME PRD ALSO                        
         BL    CON601A                                                          
         CLC   PCONPRD,SCONPRD                                                  
         BNE   CON600B                                                          
         B     CON41A1                                                          
*                                                                               
CON601A  CLI   PCONPRD,C'A'                                                     
         BNL   CON600B                                                          
         B     CON41A1                                                          
         B     CON41A1                                                          
*                                                                               
CON602   MVC   KEY,KEYSAVE                                                      
         MVC   KEY+7(6),SCONPUB                                                 
         BAS   RE,HIGH1                                                         
         LA    R0,PCONREC          REREAD FIRST CONTRACT                        
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
*                                                                               
CON603   DS    0H                                                               
         GOTO1 =A(GETFAC),DMCB,RR=Y                                             
*                                                                               
CON603N  CLI   PAGYPROF+15,C'0'    TEST 'BATES' OPTION                          
         BE    CON60A                                                           
         CLI   QOPT1,C'C'          CONTRACT ONLY - NO SCHED                     
         BE    CON60A                                                           
         L     RF,ACONSCHD                                                      
         CLI   SSORTOPT,C'D'       SEE IF SORTING BY DATE                       
         BNE   *+8                 NO                                           
         L     RF,ASORTSCH                                                      
         GOTO1 (RF)                                                             
         SPACE 1                                                                
CON60A   DS    0H                                                               
         CLI   QOPT1,C'S'                                                       
         BNE   CON60A00                                                         
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         CLC   QCOMM(6),SPACES                                                  
         BE    CON80                                                            
         DROP  RF                                                               
*              FOR SCHEDULES STILL PRINT REQUESTED COMMENT                      
CON60A00 CLI   PAGYPROF+15,C'0'    TEST 'BATES' OPTION                          
         BE    CON60B              NO                                           
         CLI   QOPT1,C'C'                                                       
         BE    CON60A0                                                          
         CLI   PAGYPROF+15,C'2'    OPT NO TO SKIP TO NEW PAGE                   
         BE    *+8                                                              
         MVI   FORCEHED,C'P'       YES - NEW PGAE                               
         B     CON60A1                                                          
CON60A0  DS    0H                                                               
         MVI   SPACING,3                                                        
         BAS   RE,CONPRT                                                        
         MVI   P,0                                                              
         MVC   PSECOND+24(25),=C'***SCHEDULE AS ORDERED***'                     
         MVI   SPACING,3                                                        
         BAS   RE,CONPRT                                                        
CON60A1  EQU   *                                                                
         BAS   R7,CON65            PRINT REQUESTED STD COM                      
         CLI   QOPT1,C'S'          SCHD ONLY - ONLY REQUEST COMMENT             
         BE    CON80                                                            
         CLI   PROFOPTS+14,C'1'    STANDARD THEN SPECIAL                        
         BNH   STTHNSP                                                          
         MVI   P,0                                                              
         BAS   RE,CONPRT           SKIP A LINE                                  
         B     SPTHNST             SPECIAL THEN STANDARD                        
STTHNSP  DS    0H                  STANDARD                                     
         BAS   R7,CON78            CONTRACT (X'44') COMMENT                     
         BAS   R7,CON70            STANDARD                                     
******   MVI   P,0                                                              
******   BAS   RE,CONPRT           SKIP A LINE                                  
******                                                                          
******   ABOVE CODE N-OPED SINCE CON61 WILL NOW SKIP A LINE                     
******   BEFORE DOING ITS FIRST LINE                                            
******                                                                          
         BAS   R7,CON61            SPECIAL                                      
         BAS   R7,CON67            CLT STND COMMENT                             
         BAS   R7,CON69            CLT (SLAVE) STND COMMENT                     
         B     CON80                                                            
CON60B   DS    0H                                                               
         MVI   SPACING,2                                                        
         BAS   RE,CONPRT           SKIP 2 LINES                                 
         BAS   R7,CON65            PRINT REQUESTED STD COM                      
         CLI   QOPT1,C'S'          SCHD ONLY - REQUEST COMMENT ONLY             
         BE    CON80                                                            
**NEW 7/12/90                                                                   
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         CLC   QCOMM(6),SPACES                                                  
         BE    CON60C                                                           
         DROP  RF                                                               
******   MVI   P,0                                                              
******   BAS   RE,CONPRT           SKIP A LINE                                  
******                                                                          
******   ABOVE CODE N-OPED SINCE CON61 WILL NOW SKIP A LINE                     
******   BEFORE DOING ITS FIRST LINE                                            
******                                                                          
CON60C   DS    0H                                                               
         CLI   PROFOPTS+14,C'1'    STANDARD THEN SPECIAL                        
         BE    STTHNSP                                                          
* ASSUME SPECIAL THEN STANDARD                                                  
SPTHNST  DS    0H                  SPECIAL                                      
         BAS   R7,CON78            CONTRACT (X'44') COMMENT                     
         BAS   R7,CON61            SPECIAL                                      
         BAS   R7,CON70            STANDARD                                     
         BAS   R7,CON67            CLT STND COMMENT                             
         BAS   R7,CON69            CLT (SLAVE) STND COMMENT                     
         B     CON80                                                            
         SPACE 2                                                                
CON61    DS    0H                                                               
*                                                                               
         MVI   SKIPSW,0                                                         
         MVI   ELCODE1,X'40'       FIND SPECIAL COMMENT ELEMENTS                
         LA    R2,PCONREC+33                                                    
CON62    BAS   RE,NEXTEL                                                        
         BNER  R7                                                               
         CLI   SKIPSW,0            SEE IF ALREADY SKIPPED A LINE                
         BNE   CON62C              YES                                          
         MVI   P,0                                                              
         BAS   RE,CONPRT                                                        
         MVI   SKIPSW,1            SO I WON'T SKIP AGAIN                        
*                                                                               
CON62C   DS    0H                                                               
         BAS   RE,PRTCOM                                                        
         B     CON62                                                            
*                                                                               
**NEW 7/12/90      WAS ONLY CLC QPAY(6),SPACES                                  
CON65    L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         CLC   QCOMM(6),SPACES                                                  
         BER   R7                  NONE SO RETURN                               
*                                                                               
* FETCH COMMENT REC                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),CONAGY                                                    
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),QCOMM                                                   
         DROP  RF                                                               
         BAS   RE,HIGH1                                                         
         CLC   KEYSAVE(10),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
         MVI   P,0                 NON-BLANK                                    
         BAS   RE,CONPRT           SKIP A LINE                                  
         MVI   ELCODE1,X'40'                                                    
         LA    R2,PBUYREC+33                                                    
         CLC   ELCODE1,0(R2)                                                    
         BE    *+8                                                              
*                                                                               
CON66    BAS   RE,NEXTEL                                                        
         BNER  R7                  RETURN                                       
         BAS   RE,PRTCOM                                                        
         B     CON66                                                            
*                                                                               
CON67    DS    0H                  CHECK FOR STANDARD COMMENT ON                
*                                  CLIENT HEADER                                
         LA    R2,PCLTREC+33                                                    
CON67B   MVI   ELCODE1,X'10'       LOOK FOR SPECIAL STND COMM ELEM              
         BAS   RE,NEXTEL                                                        
         BNER  R7                                                               
* FETCH COMMENT REC                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),CONAGY                                                    
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),2(R2)                                                   
         BAS   RE,HIGH1                                                         
         CLC   KEYSAVE(10),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
         MVI   P,0                 NON-BLANK                                    
         BAS   RE,CONPRT           SKIP A LINE                                  
         MVI   ELCODE1,X'40'                                                    
         LA    R2,PBUYREC+33                                                    
         CLC   ELCODE1,0(R2)                                                    
         BE    CON67F                                                           
*                                                                               
CON67E   BAS   RE,NEXTEL                                                        
         BNER  R7                  RETURN                                       
CON67F   BAS   RE,PRTCOM                                                        
         B     CON67E                                                           
*                                                                               
         SPACE 2                                                                
CON69    DS    0H                  IF SLAVE REQ - READ SLAVE CLTHDR             
*                                  AND CHECK FOR STANDARD COMMENT               
         CLC   QDIV,=3C' '                                                      
         BER   R7                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),CONAGY                                                    
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),QDIV                                                    
         BAS   RE,HIGH1                                                         
         CLC   KEYSAVE(10),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                MUST FIND SLAVE CLTHDR                       
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
         LA    R2,PBUYREC+33       SLAVE CLTHDR NOW IN PBUYREC                  
         B     CON67B              REST SAME AS CON67                           
*                                                                               
         SPACE 2                                                                
CON70    DS    0H                                                               
*                                                                               
         LA    R2,PCONREC+33                                                    
CON72    MVI   ELCODE1,X'30'                                                    
         BAS   RE,NEXTEL                                                        
         BNER  R7                                                               
* FETCH COMMENT REC                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),CONAGY                                                    
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),2(R2)                                                   
         BAS   RE,HIGH1                                                         
         CLC   KEYSAVE(10),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
         ST    R2,IOAREA           SAVE CON REC POINTER                         
         MVI   P,0                 NON-BLANK                                    
         BAS   RE,CONPRT           SKIP A LINE                                  
         MVI   ELCODE1,X'40'                                                    
         LA    R2,PBUYREC+33                                                    
         CLC   ELCODE1,0(R2)                                                    
         BE    *+8                                                              
CON74    BAS   RE,NEXTEL                                                        
         BNE   CON76                                                            
         BAS   RE,PRTCOM                                                        
         B     CON74                                                            
CON76    L     R2,IOAREA           RESTORE CON REC POINTER                      
         B     CON72               FIND MORE COMMENTS                           
*                                                                               
         SPACE 2                                                                
CON78    DS    0H                           LOOK FOR CONTRACT COMMENT           
         XC    KEY,KEY                                                          
         MVC   KEY(2),CONAGY                                                    
         MVC   KEY+2(1),QMEDIA              FOR SPECIFIC CLIENT                 
         MVI   KEY+3,X'44'                  IF DOESN'T EXIST                    
         MVC   KEY+4(3),PCLTKCLT                                                
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,HIGH1                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BE    CON78B              GET RECORD AND PROCESS                       
*                                                                               
         MVC   KEYSAVE+4(3),=X'FF4040'      THEN READ FOR OFFICE                
         MVC   KEYSAVE+5(1),PCLTOFF         IF DOESN'T EXIST                    
         MVC   KEY,KEYSAVE                                                      
         BAS   RE,HIGH1                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BE    CON78B              GET RECORD AND PROCESS                       
*                                                                               
         MVC   KEYSAVE+4(3),=X'FFFFFF'      THEN READ FOR MEDIA                 
         MVC   KEY,KEYSAVE                  (ALL CLIENTS)                       
         BAS   RE,HIGH1                     IF DOESN'T EXIST                    
         CLC   KEY(7),KEYSAVE                                                   
         BNER  R7                           RETURN EMPTY-HANDED                 
*                                                                               
CON78B   DS    0H                                                               
         LA    R0,PBUYREC          READ CONTRACT COMMENT INTO BUY               
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
         MVI   P,0                 NON-BLANK                                    
         BAS   RE,CONPRT           SKIP A LINE                                  
         MVI   ELCODE1,X'40'                                                    
         LA    R2,PBUYREC+33                                                    
         CLC   ELCODE1,0(R2)                                                    
         BE    CON78F                                                           
*                                                                               
CON78E   BAS   RE,NEXTEL                                                        
         BNER  R7                  RETURN                                       
*                                                                               
CON78F   CLC   2(4,R2),=C'COM='  CHECK FOR IMBEDDED STANDARD COMMENT            
         BNE   CON78G                                                           
         BAS   RE,STDCOM                                                        
         B     CON78E                                                           
*                                                                               
CON78G   BAS   RE,PRTCOM                                                        
         B     CON78E                                                           
*********************  INSERT DATA BEFORE  ****************************         
         EJECT                                                                  
CON80    CLI   QOPT1,C'C'          TEST SUPPRESS SCHED                          
         BE    CON90                                                            
         CLI   PAGYPROF+15,C'0'    TEST 'BATES' OPTION                          
         BNE   CON90                                                            
         B     CON82                                                            
         SPACE 1                                                                
CON81    DS    0H                                                               
         CLC   QEST,=C'000'        DUMMY CONTRACT                               
         BNE   CON82               NO                                           
**NEW 7/12/90      WAS ONLY CLC QPAY(6),SPACES                                  
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         CLC   QCOMM(6),SPACES     STND COM REQUESTED                           
         BE    CON82               NO                                           
         DROP  RF                                                               
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,CONPRT                                                        
         BAS   R7,CON65            PRINT REQUESTED STND COM                     
*                                                                               
CON82    DS    0H                                                               
         L     RF,ACONSCHD                                                      
         CLI   SSORTOPT,C'D'       SEE IF SORTING BY DATE                       
         BNE   *+8                 NO                                           
         L     RF,ASORTSCH                                                      
         GOTO1 (RF)                                                             
         SPACE 2                                                                
CON90    DS    0H                                                               
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         GOTO1 ACONEOP                                                          
*                                                                               
CON92    DS    0H                                                               
         CLI   FULLSW,0                                                         
         BNE   CON92A              REGISTER FULL                                
         GOTO1 ACONREG             ADD TO CONTRACT REGISTER                     
CON92A   CLC   =C'000',QEST                                                     
         BNE   CON93                                                            
         CLC   =C'ALL',QPUB                                                     
         BNE   CONCLT                                                           
         IC    RF,SAVBKEY+12       LAST BYTE OF PUB                             
         LA    RF,1(RF)                                                         
         STC   RF,SAVBKEY+12                                                    
         MVC   KEY,SAVBKEY                                                      
         MVI   KEY+13,0                                                         
         B     CON16                                                            
*                                                                               
CON93    DS    0H                                                               
         CLC   QEST,SPACES         IF ONE CON SPECIFIED, DONE                   
         BNE   CONCLT                                                           
* ELSE READ NEXT CON KEY                                                        
         MVC   KEY,PCONKEY                                                      
         CLI   QEDITION,C'Z'       IF DOING ALL EDITIONS RESET KEY              
         BNE   *+10                TO FIRST PUB READ AND GET NEXT CON           
         MVC   KEY+7(6),SCONPUB                                                 
         BAS   RE,HIGH1                                                         
         BAS   RE,SEQ1                                                          
*                                                                               
         CLC   =C'ALL',QPUB                                                     
         BE    CON17A                                                           
         B     CON13                                                            
*                                                                               
         DROP  R4                  DROP PCONREC USING                           
         EJECT                                                                  
PRTCOM   NTR1                                                                   
         LA    R4,2(R2)                                                         
         SR    R5,R5                                                            
*                                                                               
         CLC   0(2,R4),=C'E-'      E- COMMENTS DONT PRINT ON CONTRACTS          
         BE    PRTCOMX                                                          
         CLC   0(3,R4),=C'RC='                                                  
         BE    PRTCOMX                                                          
*                                                                               
PRTCOM2  IC    R5,1(R2)                                                         
         AHI   R5,-2                                                            
         CLI   0(R4),C'+'                                                       
         BNE   PRTCOM3                                                          
         MVC   SPACING,1(R4)                                                    
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         LA    R4,2(R4)                                                         
         AHI   R5,-2                                                            
         BAS   RE,CONPRT           DO SPACING                                   
         B     PRTCOM6                                                          
*                                                                               
PRTCOM3  CLC   0(2,R4),=C'E+'                                                   
         BNE   PRTCOM6                                                          
         LA    R4,2(R4)            DONT PRINT E+                                
         AHI   R5,-2                                                            
*                                                                               
PRTCOM6  LTR   R5,R5                                                            
         BNP   PRTCOMX                                                          
         BCTR  R5,0                                                             
         EX    R5,MVCOM                                                         
         BAS   RE,CONPRT                                                        
PRTCOMX  XIT1                                                                   
*                                                                               
MVCOM    MVC   P+06(0),0(R4)                                                    
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELX             RETURN WITH CC =                             
         CLC   ELCODE1,0(R2)                                                    
         BCR   8,RE                                                             
         B     NEXTEL+2                                                         
NEXTELX  LTR   R2,R2               RETURN WITH CC NOT =                         
         BR    RE                                                               
         SPACE 2                                                                
         SPACE 2                                                                
CONFLOAT OI    0(R7),C' '                                                       
         CLI   0(R7),C' '                                                       
         BNE   *+8                                                              
         BCT   R7,*-12                                                          
         BR    RE                                                               
         EJECT                                                                  
CONPRT   NTR1                                                                   
         CLI   FORCEHED,C'P'                                                    
         BE    CONPRT2                                                          
         CLC   LINE,MAXLINES                                                    
         BH    CONPRT2                                                          
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    CONPRT4                                                          
*                                                                               
CONPRT2  EQU   *                                                                
         CLC   PAGE,=H'1'                                                       
         BE    CONPRT2A                                                         
         GOTO1 ACONEOP                                                          
         SPACE 1                                                                
CONPRT2A EQU   *                                                                
         EDIT  (B2,PAGE),(2,MH5+74)                                             
*                                                                               
         LA    R0,14                                                            
         LA    R1,HEAD1                                                         
         LA    R2,MH1                                                           
         MVC   0(080,R1),0(R2)                                                  
         LA    R1,132(R1)                                                       
         LA    R2,080(R2)                                                       
         BCT   R0,*-14                                                          
*                                                                               
         CLI   FAXOPT,C'Y'            SEE IF FAXING                             
         BNE   CONPRT4                                                          
         CLC   MYFAX,SPACES           BE SURE I HAVE A FAX NUMBER               
         BE    CONPRT4                (IF NOT I WON'T BE FAXING)                
         MVC   HEAD1+45(18),SPACES    BLANK OUT *'S OVER AAAA MESSAGE           
         MVC   HEAD2+45(33),PAGYNAME  MUST PRINT AGENCY NAME                    
         MVC   HEAD3+45(33),PAGYADDR  AND ADDRESS                               
*                                                                               
*                                                                               
CONPRT4  MVI   RCSUBPRG,0                                                       
         GOTO1 AFREPORT                                                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*        MODULE FOR PROCESS STANDARD COMMENTS                                   
*        EMBEDDED IN A CONCOM                                                   
STDCOM   NTR1                                                                   
*                      R2 POINTS TO CONCOM ELEMENT                              
         LR    R5,R2                                                            
         LA    R5,1(R5)                                                         
*                                                                               
STDC2    DS    0H                                                               
*                                                                               
         CLC   1(4,R5),=C'COM='                                                 
         BNE   STDCOMX                                                          
         LA    R5,5(R5)                                                         
         LR    R4,R5                                                            
STDC4    DS    0H                                                               
         CLI   0(R4),C','                                                       
         BE    STDC6                                                            
         CLI   0(R4),0             EOR                                          
         BE    STDC6                                                            
         CLI   0(R4),X'40'         NEXT ELEM                                    
         BE    STDC6                                                            
         LA    R4,1(R4)                                                         
         B     STDC4                                                            
STDC6    DS    0H                                                               
         SR    R4,R5                                                            
         BNP   STDCERX                                                          
         CH    R4,=H'6'                                                         
         BH    STDCERX                                                          
         MVC   WORK,SPACES                                                      
         LA    R7,WORK+6                                                        
         SR    R7,R4                                                            
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R5)       COMM NO RIGHT ALIGN IN WORK                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),CONAGY                                                    
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         BAS   RE,HIGH1                                                         
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 COMMENT NOT FOUND                           
         LA    R0,PBUYREC+2000                                                  
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
*                                                                               
         BAS   RE,CONPRT           SKIP A LINE                                  
*                                  R2 POINTS TO REC ON RETURN                   
*                                  FROM FNDREC                                  
         LA    R2,PBUYREC+2000                                                  
         LA    R2,33(R2)                                                        
         CLI   0(R2),X'40'                                                      
         BE    STDC8                                                            
*                                                                               
STDC7    DS    0H                                                               
         MVI   SCELCOD,X'40'                                                    
         BAS   RE,SCNXTEL                                                       
         BNE   STDC9               EOR                                          
STDC8    DS    0H                                                               
         CLC   2(5,R2),=C'SHIP='   NO SHIP= COMMENTS                            
         BE    STDC9                                                            
         CLC   2(6,R2),=C'LABEL='                                               
         BE    STDC9                                                            
         BAS   RE,PRTCOM                                                        
         B     STDC7                                                            
*                                                                               
STDC9    DS    0H                                                               
         LA    R5,1(R5,R4)         POINT TO NEXTCOM=                            
         CLI   0(R5),0                                                          
         BE    STDCOMX                                                          
         B     STDC2                                                            
*                                                                               
STDCOMX  DS    0H                                                               
         BAS   RE,CONPRT           SPACE 1                                      
*                                                                               
STDCOMX2 DS    0H                                                               
         XIT1                                                                   
*                                                                               
STDCERX  DC    H'0'                BAD CONCOM REC                               
         EJECT                                                                  
*                                                                               
         SPACE 3                                                                
SCNXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SCNXTELX                                                         
         CLC   SCELCOD,0(R2)                                                    
         BER   RE                                                               
         B     SCNXTEL+2                                                        
SCNXTELX LTR   R2,R2                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
HIGH1    MVC   KEYSAVE,KEY                                                      
         LA    R0,DMRDHI                                                        
         B     DMDIR                                                            
*                                                                               
SEQ1     LA    R0,DMRSEQ                                                        
*                                                                               
DMDIR    ST    R0,DMCB                                                          
*                                                                               
         LA    R0,PRTDIR                                                        
         ST    R0,DMCB+4                                                        
         B     DMALL                                                            
*                                                                               
HIGHPUB1 MVC   KEYSAVE,KEY                                                      
         LA    R0,DMRDHI                                                        
         ST    R0,DMCB                                                          
         LA    R0,PUBDIR                                                        
         ST    R0,DMCB+4                                                        
*                                                                               
DMALL    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,,,KEY,KEY                                           
*                                                                               
         B     DMX                                                              
         EJECT                                                                  
GET      NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
         B     DMX                                                              
*                                                                               
GETPUB   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
DMX      TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
* RF POINTS TO OPTION                                                           
* IF PROGRAM IS 12 AND OPTION IS YES WILL RETURN 0(RE)                          
* OTHERWISE 4(RE)                                                               
*                                                                               
OPTIONCH CLC   QPROG,=C'12'                                                     
         BE    OPTIONC1                                                         
         CLC   QPROG,=C'13'          OR TURNAROUNDS                             
         BE    OPTIONC1                                                         
         CLC   QPROG,=C'15'                                                     
         BNE   8(RE)                                                            
         LA    RF,PROFOPTS+12                                                   
OPTIONC1 CLI   0(RF),C'N'            DO NOT SUPPRESS TITLES                     
         BE    8(RE)                                                            
         CLI   0(RF),C'T'            SKIP JUST TITLE                            
         BE    4(RE)                                                            
         BR    RE                    M/B/YES                                    
*                                                                               
         LTORG                                                                  
*                                                                               
LKUPKEY  DS    CL20                                                             
*                                                                               
SCELCOD  DS    CL1        ELEMENT CODE                                          
CSAVKEY  DS    CL32                                                             
*                                                                               
ACONS    DS    0F                                                               
         DC    V(RATELOOK)                                                      
         DC    A(PRDTAB)                                                        
         DC    A(PRDTABX)                                                       
         DC    A(CONEOP)                                                        
         DC    A(CONTAB)                                                        
         DC    A(CONTABX)                                                       
         DC    A(CONREG)                                                        
         DC    A(PRTREG)                                                        
         DC    V(CHOPPER)                                                       
         DC    A(GRTAB)                                                         
         DC    A(GRID)                                                          
         DC    A(MHSUBS)                                                        
         DC    A(CSPUB)                                                         
         DC    A(FNDEDTS)                                                       
         DC    A(CONSCHED)                                                      
         DC    A(CONSPRT)                                                       
         DC    A(CONHEAD)                                                       
         DC    A(SORTSCH)                                                       
         DC    A(SORTTAB)                                                       
         DC    A(SORTPRT)                                                       
         DC    A(EDTLIST)                                                       
         DC    V(DATVAL)                                                        
         DC    A(FREPORT)                                                       
         DC    V(PPGETCG)                                                       
         DC    A(STDCOM)                                                        
         DC    A(RTLKELS)         26 SO FAR                                     
         DC    4F'0'              4 SPARE ADDRESS                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
* SEARCH FOR SPECIAL COMMENT WITH COLUMN CONVERSION                             
* FIND AND SAVE CONVERSION FACTOR AND EFFECTIVE DATE                            
*                                                                               
* FORMAT IS COL.CONV=N-N,MMMDD/YY                                               
*                                                                               
GETFAC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,ACONIO1                                                       
         USING PCONREC,R4                                                       
         MVI   ELCODE1,X'40'                                                    
         LA    R2,PCONREC+33                                                    
GETFC3C  BAS   RE,NEXTEL                                                        
         BNE   GETFC3N                                                          
         CLC   2(9,R2),=C'COL.CONV='    LOOK FOR KEY WORD                       
         BNE   GETFC3C                                                          
         ZIC   R7,1(R2)            ELEM LENGHT                                  
         AHI   R7,-11              ELEM CODE + KEY WORD                         
         BNP   GETFC3C                                                          
         LA    RE,11(R2)                                                        
         LR    RF,RE                                                            
         LA    R6,WORK                                                          
         SR    R5,R5                                                            
GETFC3E  CLI   0(RE),C'-'          SCAN FOR -                                   
         BE    GETFC3G                                                          
         CLI   0(RE),C'*'          OR *                                         
         BE    GETFC3G                                                          
         CLI   0(RE),C'0'                                                       
         BL    GETFC3C                                                          
         CLI   0(RE),C'9'                                                       
         BH    GETFC3C                                                          
         MVC   0(1,R6),0(RE)                                                    
         LA    R6,1(R6)                                                         
         LA    RE,1(RE)                                                         
         LA    R5,1(R5)                                                         
         BCT   R7,GETFC3E                                                       
         B     GETFC3C             INVALID SKIP THIS ELEM                       
*                                                                               
GETFC3G  MVC   SCONVIND,0(RE)                                                   
         LTR   R5,R5                                                            
         BZ    GETFC3C             INVALID SKIP THIS ELEM                       
         CHI   R5,2                                                             
         BH    GETFC3C             MAX 2 CHARS                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)         FROM COLUMNS                                 
*                                                                               
         LA    RE,1(RE)            BUMP PAST -                                  
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BNP   GETFC3C             INVALID SKIP THIS ELEM                       
         LR    RF,RE                                                            
         LA    R6,WORK                                                          
         SR    R5,R5                                                            
GETFC3H  CLI   0(RE),C','          SCAN FOR ,                                   
         BE    GETFC3J                                                          
         CLI   0(RE),C'0'                                                       
         BL    GETFC3C                                                          
         CLI   0(RE),C'9'                                                       
         BH    GETFC3C                                                          
         MVC   0(1,R6),0(RE)                                                    
         LA    R6,1(R6)                                                         
         LA    RE,1(RE)                                                         
         LA    R5,1(R5)                                                         
         BCT   R7,GETFC3H                                                       
         B     GETFC3C             INVALID SKIP THIS ELEM                       
*                                                                               
GETFC3J  LTR   R5,R5                                                            
         BZ    GETFC3C             INVALID SKIP THIS ELEM                       
         CHI   R5,2                                                             
         BH    GETFC3C             MAX 2 CHARS                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DOUBLE,0(0,RF)      TO COLUMNS                                   
*                                                                               
         LA    RE,1(RE)            BUMP PAST                                    
         BCTR  R7,0                DECREMENT R7                                 
         LTR   R7,R7                                                            
         BNP   GETFC3C             MUST HAVE DATE NEXT                          
         LR    R5,RE                                                            
         GOTO1 ADATVAL,DMCB,(0,0(R5)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    GETFC3C             INVALID DATE SKIP THIS ELEM                  
         GOTO1 DATCON,DMCB,(0,WORK),(3,SCONVDTE)                                
*                                                                               
         CLI   SCONVIND,C'-'       SEE IF DOING NEW TO OLD                      
         BE    GETFC3K                                                          
         CVB   R0,DUB              NEW TO OLD  - SWAP DUB + DOUBLE              
         CVB   R1,DOUBLE                                                        
         CVD   R0,DOUBLE                                                        
         CVD   R1,DUB                                                           
*                                                                               
GETFC3K  MP    DUB,=P'1000'                                                     
         DP    DUB,DOUBLE+6(2)                                                  
         CP    DUB+6(2),=P'50'     CHK REMAINDER                                
         BL    *+10                                                             
         AP    DUB(6),=P'1'                                                     
         ZAP   DUB,DUB(6)                                                       
         CVB   R0,DUB                                                           
         ST    R0,SCONVFAC         SAVE CONVERSION FACTOR                       
*              3 DECIMALS IMPLIED , EXAMPLE 9-6 IS 1.500                        
*                                  END OF PAGE ROUTINE                          
GETFC3N  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
CONEOP   CSECT                                                                  
         NMOD1 0,CONEOP                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP12WRKD,R8                                                      
*                                                                               
         L     R6,ACONIO1          A(PCONREC)                                   
         USING PCONREC,R6          NOTE TEMPORARY USE OF R6                     
*                                                                               
         SPACE 1                                                                
         MVI   FORCEHED,C'N'                                                    
         MVC   BYTE,MAXLINES                                                    
         MVI   MAXLINES,99                                                      
         MVC   SAVEP,P                                                          
         MVC   SAVEP2,PSECOND                                                   
         MVC   SAVESPAC,SPACING                                                 
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         CLC   SAVEP2,SPACES                                                    
         BNE   *+14                                                             
         CLC   SAVEP,SPACES                                                     
         BE    CONEOP2                                                          
         MVI   P,0                                                              
         MVC   PSECOND+34(11),=C'(CONTINUED)'                                   
         GOTO1 AFREPORT                                                         
         SPACE 1                                                                
CONEOP2  EQU   *                                                                
         CLC   QEST,=C'000'        SEE IF DOING DUMMY CONTRACT                  
         BNE   CONEOP3             NO                                           
         CLI   QUESTOR,C'*'        SEE IF REQUESTOR STARTS WITH *               
         BNE   CONEOP8             NO - NO SIGNATURE                            
         MVC   PCONREQ(11),QUESTOR+1       USE AS SIGNATURE                     
         MVI   PCONREQ+11,C' '                                                  
*                                                                               
CONEOP3  DS    0H                                                               
         OC    PCONREQ,SPACES                                                   
         CLC   PCONREQ,SPACES                                                   
         BE    CONEOP8                                                          
         CLC   CONAGY,PAGYKAGY     DONT USE SIGNATURE FROM                      
         BNE   CONEOP8             NON-REQUESTING AGENCY                        
*                                                                               
         CLI   FAXOPT,C'Y'         SEE IF FAXING                                
         BNE   CONEOP3C                                                         
         CLC   MYFAX,SPACES        BE SURE I HAVE A FAX NUMBER                  
         BE    CONEOP3C                                                         
         MVI   SPACING,3           SKIP 3 LINE THEN PRINT SIGNATURE             
         GOTO1 AFREPORT                                                         
         B     CONEOP7                                                          
*                                                                               
CONEOP3C SR    R0,R0                                                            
         IC    R0,LINE                                                          
         LA    R4,83                                                            
*                                                                               
         SR    R4,R0                                                            
CONEOP4  EQU   *                                                                
         CHI   R4,3                                                             
         BL    CONEOP6                                                          
         MVI   SPACING,3                                                        
         GOTO1 AFREPORT                                                         
         SPACE 1                                                                
         AHI   R4,-3                                                            
         B     CONEOP4                                                          
CONEOP6  EQU   *                                                                
         LTR   R4,R4                                                            
         BNP   CONEOP7                                                          
         STC   R4,SPACING                                                       
         GOTO1 AFREPORT                                                         
         SPACE 1                                                                
CONEOP7  EQU   *                                                                
         MVC   P+64(12),PCONREQ                                                 
         GOTO1 AFREPORT                                                         
         SPACE 1                                                                
CONEOP8  EQU   *                                                                
         MVC   P,SAVEP                                                          
         MVC   PSECOND,SAVEP2                                                   
         MVC   SPACING,SAVESPAC                                                 
         MVC   MAXLINES,BYTE                                                    
         MVI   FORCEHED,C'P'                                                    
         XMOD1 1                                                                
         SPACE 1                                                                
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                  BUILD HEADLINES                              
         SPACE 2                                                                
CONHEAD  CSECT                                                                  
         NMOD1 0,CONHEAD                                                        
*                                                                               
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP12WRKD,R8                                                      
         SPACE 2                                                                
         L     R4,ACONIO1          A(PCONREC)                                   
         USING PCONREC,R4          NOTE TEMPORARY USE OF R4                     
*                                                                               
         MVC   MYFAX,SPACES        INITIALIZE MYFAX TO SPACES                   
         MVC   TOFAX,SPACES        INITIALIZE TOFAX TO SPACES                   
*                                                                               
         LA    R1,MH1                                                           
         LA    R0,14                                                            
         MVC   0(MHL,R1),SPACES                                                 
         LA    R1,MHL(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,MH1+1                                                         
         MVC   0(10,R1),PAGYMED                                                 
*                                                                               
         MVI   ELCODE1,X'41'       LOOK FOR MEDIA NAME OVERRIDE ELEM            
         LA    R2,PCLTREC+33                                                    
         CLC   ELCODE1,0(R2)                                                    
         BE    *+8                                                              
CH1C     DS    0H                                                               
         BAS   RE,CHNXTEL                                                       
         BNE   CH1D                                                             
         MVC   0(10,R1),2(R2)                                                   
*                                                                               
CH1D     DS    0H                                                               
         LA    R7,10(R1)                                                        
*                                                                               
* CHECK FOR VARIOUS SPELLINGS FOR INTERACTIVE                                   
* AND CONVERT THEM TO "INTERACTIVE" FOR THE TITLE                               
*                                                                               
         CLC   0(10,R1),=C'INTERACTIV'                                          
         BE    CH1E                                                             
         CLC   0(9,R1),=C'INTERACTV' COVERS INTERACTV AND INTERACTVE            
         BE    CH1E                                                             
         CLC   0(6,R1),=C'INTRAC' COVERS INTRACTIVE (AND OTHERS)                
         BE    CH1E                                                             
         B     CH1F                                                             
*                                                                               
CH1E     MVC   0(11,R1),=C'INTERACTIVE'                                         
         LA    R7,11(R1)                                                        
*                                                                               
CH1F     BAS   RE,CHFLOAT                                                       
         LR    RF,R7                                                            
*                                                                               
         CLC   QAGENCY,=C'LC'      SEE IF LAURANCE CHARLES ALWAYS               
         BNE   CH1Z                DO SPECIAL TITLE FOR 12,13,15                
*                                                                               
         CLI   QOPT3,C'R'          SEE IF SPACE RESERVATION                     
         BE    CH1Z5                                                            
         MVC   2(23,R7),=C'MASTER BILLING CONTRACT'                             
         SR    R7,R1                                                            
         LA    R7,24(R7)                                                        
         B     CH2                                                              
*                                                                               
CH1Z     MVC   2(8,R7),=C'CONTRACT'                                             
         SR    R7,R1                                                            
         LA    R7,9(R7)                                                         
QOPT3CK  CLI   QOPT3,C'R'                                                       
         BNE   CH2                                                              
CH1Z5    LR    R7,RF                                                            
         MVC   2(17,R7),=C'SPACE RESERVATION'                                   
         SR    R7,R1                                                            
         LA    R7,18(R7)                                                        
CH2      DS    0H                                                               
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   MHL(0,R1),DASHES1                                                
*                                                                               
         CLI   QOPT3,C'R'                                                       
         BNE   CH2A                                                             
         CLI   PROFOPTS+2,C'Y'         SEE IF SHOWING CON NUMBER                
         BNE   CH3                                                              
         OC    PCONNUM,PCONNUM         SEE IF I HAVE CONTRACT NUMBER            
         BZ    CH3                                                              
*                                                                               
CH2A     LA    R1,MH3+1                                                         
         CLI   PCONTYP,C'R'                                                     
         BNE   *+10                                                             
         MVC   0(11,R1),=C'RESERVATION'                                         
         CLI   PCONTYP,C'M'                                                     
         BNE   *+10                                                             
         MVC   0(6,R1),=C'MASTER'                                               
*                                                                               
CH2A5    LA    R1,MH5+1                                                         
         MVC   0(12,R1),=C'CONTRACT NO.'                                        
*                                                                               
         MVC   HALF,PCONNUM                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  13(3,R1),DUB                                                     
         CLI   SVPRDNAM,0          SEE IF DOING ONE PRODUCT                     
         BE    CH2B                                                             
         BAS   RE,CKPROD           CHECK PRODUCT OPTIONS                        
         B     *+8                 PRINT JUST PROD CODE                         
         B     CH3                 NOTHING TO PRINT                             
*                                                                               
         MVI   16(R1),C'-'                                                      
         MVC   17(3,R1),QPRODUCT                                                
         B     CH3                                                              
CH2B     DS    0H                                                               
         CLI   PCONPRD,C'A'                                                     
         BL    CH3                                                              
         BAS   RE,CKPROD           CHECK PRODUCT OPTIONS                        
         B     *+8                 PRINT JUST PROD CODE                         
         B     CH3                 NOTHING TO PRINT                             
*                                                                               
         MVI   16(R1),C'-'                                                      
         MVC   17(3,R1),PCONPRD                                                 
*                                                                               
CH3      DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,CSTART),(5,W)                                     
*                                                                               
         GOTO1 DATCON,(R1),(3,CEND),(5,W+9)                                     
*                                                                               
         MVI   W+8,C'-'                                                         
         LA    R1,MH6+1                                                         
         CLI   QOPT3,C'R'                                                       
         BNE   *+8                                                              
         LA    R1,MH4+1                                                         
*                                                                               
         CLC   QAGENCY,=C'LC'      LAURENCE CHARLES                             
         BNE   CHPERIOD                                                         
         CLI   QMEDIA,C'O'         OUTDOOR                                      
         BNE   CHPERIOD                                                         
         MVC   0(17,R1),=C'DURING THE PERIOD'                                   
         MVC   18(17,R1),W                                                      
         B     CHPERIOT                                                         
*                                                                               
CHPERIOD MVC   0(14,R1),=C'FOR THE PERIOD'                                      
         MVC   15(17,R1),W                                                      
*                                                                               
CHPERIOT CLI   MEMBOPT,C'Y'        VALUES 7,5,3,1 - NO MEMBER MSG               
         BNE   CH3B                                                             
         LA    R1,MH1+45                                                        
         MVI   0(R1),C'*'                                                       
         MVC   1(17,R1),0(R1)                                                   
*                                                                               
CH3B     DS    0H                                                               
         CLI   NAMOPT,C'Y'         VALUES 7,6,4,3 - OMIT AGYNAME                
         BE    CH3D                                                             
*                                                                               
CH3C     DS    0H                                                               
         LA    R1,MH2+45                                                        
         MVC   0(33,R1),PAGYNAME                                                
         MVC   MHL(33,R1),PAGYADDR                                              
*                                                                               
CH3D     LA    R1,MH5+69                                                        
         MVC   0(4,R1),=C'PAGE'                                                 
*                                                                               
         PACK  DUB,RCDATE(2)                                                    
         CVB   R2,DUB                                                           
         MHI   R2,3                                                             
         LA    R2,MONTHS-3(R2)                                                  
         LA    R1,MH8+1                                                         
         MVC   8(3,R1),0(R2)                                                    
         MVC   11(2,R1),RCDATE+3                                                
         MVC   14(2,R1),RCDATE+6                                                
         MVI   13(R1),C'/'                                                      
         MVC   0(4,R1),=C'DATE'                                                 
*                                                                               
         LA    R1,MH11+1                                                        
         MVC   0(6,R1),=C'CLIENT'                                               
         MVC   8(3,R1),PCLTKCLT                                                 
         MVC   12(20,R1),PCLTNAME                                               
*                                                                               
         MVI   MH12,0                                                           
         OC    SVPRDNAM,SVPRDNAM                                                
         BZ    CH4                                                              
         LA    R1,MH12+1                                                        
         BAS   RE,CKPROD           CHECK PRODUCT OPTIONS                        
         B     CH3K                PRINT JUST PROD CODE                         
         B     CH4B                NOTHING TO PRINT                             
*                                                                               
         MVC   12(20,R1),SVPRDNAM                                               
CH3K     MVC   0(7,R1),=C'PRODUCT'                                              
         MVC   8(3,R1),QPRODUCT                                                 
         B     CH4B                                                             
*                                                                               
CH4      CLI   PCONPRD,C'A'        SEE IF THIS IS A PRD CONTRACT                
         BL    CH4B                NO                                           
         XC    KEY,KEY                                                          
         MVC   KEY(7),PCLTKEY                                                   
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+7(3),PCONPRD    READ PRD                                     
         CLC   KEY(10),PPRDKEY                                                  
         BE    CH4A                ALL READY HAVE PRD                           
         BAS   RE,CHHIGH                                                        
         CLC   KEY(10),KEYSAVE                                                  
         BNE   CH4B                DON'T BLOW UP                                
         LA    R0,PPRDREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,CHGET                                                         
*                                                                               
CH4A     LA    R1,MH12+1                                                        
         BAS   RE,CKPROD           CHECK PRODUCT OPTIONS                        
         B     JSTCODE             PRINT JUST PROD CODE                         
         B     CH4B                NOTHING TO PRINT                             
*                                                                               
         MVC   12(20,R1),PPRDNAME                                               
JSTCODE  MVC   0(7,R1),=C'PRODUCT'                                              
         MVC   8(3,R1),PCONPRD                                                  
*                                                                               
*                                                                               
CH4B     LA    R1,MH7+45                                                        
         MVC   0(16,R1),=C'TO PUBLISHER OF-'                                    
         CLI   QMEDIA,C'O'         OUTDOOR                                      
         BNE   *+10                                                             
         MVC   0(24,R1),=C'OUTDOOR ADVERTISING CO.-'                            
*                                            PUB NO.                            
         MVC   WORK,SPACES                                                      
         MVC   HALF(2),PUBKPUB+4   SAVE ZONE AND EDITION                        
         CLI   QEDITION,C'Z'       SEE IF DOING ALL EDITIONS                    
         BNE    *+8                                                             
         MVI   PUBKPUB+5,0                                                      
         CLC   QZONE,=C'ZZ'        SEE IF DOING ALL ZONES                       
         BNE    *+8                                                             
         MVI   PUBKPUB+4,0                                                      
         IC    R7,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R7),PUBKPUB),WORK                                 
*                                                                               
         MVC   PUBKPUB+4(2),HALF   RESTORE ZONE AND EDITION                     
*                                                                               
         LA    R7,WORK+20                                                       
         BAS   RE,CHFLOAT                                                       
         LA    R0,WORK-1                                                        
         SR    R7,R0               R7 = LENGTH                                  
         LA    R1,MH8+78                                                        
         SR    R1,R7               R1 = TO                                      
         MVC   0(17,R1),WORK                                                    
*                                                                               
         LA    R1,MH9+45                                                        
         MVC   0(20,R1),PUBNAME                                                 
         CLI   QMEDIA,C'N'                                                      
         BE    CH5                                                              
         CLI   PUBKZON,0                                                        
         BE    CH4C                                                             
         CLC   QZONE,=C'ZZ'        SEE IF DOING ALL ZONES                       
         BE    CH6                                                              
         CLI   QMEDIA,C'O'         DON'T PRINT MARKET NOW                       
         BE    CH6                 FOR OUTDOOR                                  
CH4C     DS    0H                                                               
         MVC   MHL(20,R1),PUBZNAME                                              
         B     CH6                                                              
CH5      MVC   WORK,SPACES                                                      
         MVC   WORK(16),PUBCITY                                                 
         LA    R7,WORK+16                                                       
         BAS   RE,CHFLOAT                                                       
         MVI   1(R7),C','                                                       
         MVC   3(2,R7),PUBSTATE                                                 
         MVC   MHL(25,R1),WORK                                                  
         CLC   QZONE,=C'ZZ'         SEE IF DOING ALL ZONES                      
         BNE   CH5B                                                             
         CLI   PUBKZON,0                                                        
         BNE   CH6                                                              
CH5B     DS    0H                                                               
         MVC   MHL*2(20,R1),PUBZNAME                                            
*                                                                               
CH6      DS    0H                                                               
         CLI   QEDITION,C'Z'                                                    
         BE    CH7                 DOING ALL EDITS                              
         LR    R7,R1                                                            
         MVC   WORK,SPACES                                                      
         CLC   QPROG,=C'13'       CONTRACT TURAROUNDS                           
         BE    CH6D                                                             
         CLC   QPROG,=C'12'       OR CONTRACTS                                  
         BNE   *+8                    CASCADE THRU                              
CH6D     CLI   QMEDIA,C'O'           AND OUTDOOR                                
         BNE   *+8                    CASCADE THRU                              
         CLI   PUBKED,0              AND NO EDITON                              
         BNE   *+8                    CASCADE THRU                              
         CLI   PROFOPTS+11,C'Y'                                                 
         BNE   CH6DX                                                            
         MVC   WORK(7),=C'POSTERS'                                              
         B     CH6DZ                                                            
CH6DX    GOTO1 PUBEDIT,DMCB,PUBKPUB,(C'E',WORK)                                 
*                                                                               
CH6DZ    CLI   0(R7),C' '                                                       
         BNH   *+12                                                             
         LA    R7,MHL(R7)                                                       
         B     *-12                                                             
         MVC   MHL(11,R7),WORK                                                  
CH7      DS    0H                                                               
         EJECT                                                                  
*                                                                               
*                                  LINES BUILT IN MHSUBS WILL PRINT             
*                                  AFTER HEADLINES ON FIRST PAGE                
*                                                                               
*                                                                               
         L     R1,AMHSUBS                                                       
         LA    R0,MHSUBN                                                        
         XC    0(MHL,R1),0(R1)                                                  
         LA    R1,MHL(R1)                                                       
         BCT   R0,*-10                                                          
*                                  FORCE AT LEAST 11 LINES                      
         L     R1,AMHSUBS                                                       
         LA    R0,7                                                             
         MVI   0(R1),C' '                                                       
         LA    R1,MHL(R1)                                                       
         BCT   R0,*-8                                                           
*                                                                               
* TELEPHONE NUMBER                                                              
*                                                                               
         CLI   PROFOPTS+13,C'A'    DEFAULT VALUE?                               
         BH    *+8                                                              
         MVI   PROFOPTS+13,C'T'    DEFAULT TO TELEPHONE ONLY                    
*                                  TELEPHONE NUMBER                             
         CLI   PROFOPTS+13,C'F'    FAX OPTION ONLY?                             
         BE    CH13                                                             
         XC    WORK,WORK                                                        
         CLI   PREPKEY,0                                                        
         BE    *+14                                                             
         MVC   WORK(12),PREPTEL                                                 
         B     CH12                                                             
         LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'11'                                                      
         BE    CH10                                                             
         MVI   ELCODE1,X'11'                                                    
         BAS   RE,CHNXTEL                                                       
         BNE   CH12                                                             
CH10     DS    0H                                                               
         USING PUBSADEL,R2                                                      
         MVC   WORK(12),PUBTEL                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
CH12     DS    0H                                                               
         LA    R2,PCONREC+33                                                    
         CLI   0(R2),X'55'         CONTRACT TEL AND FAX ELEM                    
         BE    CH12M                                                            
         MVI   ELCODE1,X'55'                                                    
         BAS   RE,CHNXTEL                                                       
         BNE   CH12Z                                                            
CH12M    DS    0H                                                               
         USING PCTFELEM,R2                                                      
         CLC   =C'DEFAULT',PCONTELE                                             
         BE    CH12Z                                                            
         CLC   =C'NONE',PCONTELE                                                
         BNE   CH12P                                                            
         XC    WORK(12),WORK       NO TELEPHONE NUMBER                          
         B     CH12Z                                                            
CH12P    MVC   WORK(12),PCONTELE                                                
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
CH12Z    DS    0H                                                               
         OC    WORK(12),SPACES                                                  
         CLC   WORK(12),SPACES                                                  
         BE    CH13A                                                            
         L     R1,AMHSUBS                                                       
         LA    R1,45+0*MHL(R1)     LINE 1 POS 46                                
         MVC   0(3,R1),=C'TEL'                                                  
         MVC   4(12,R1),WORK                                                    
*                                                                               
CH13A    DS    0H                                                               
         CLI   FAXOPT,C'Y'         SEE IF FAXING                                
         BE    CH13                                                             
*                                                                               
         CLI   PROFOPTS+13,C'T'    TELEPHONE NUMBER ONLY                        
         BE    CH14                                                             
*                                                                               
* FAX NUMBER                                                                    
*                                                                               
CH13     XC    WORK,WORK                                                        
         CLI   PREPKEY,0                                                        
         BE    RREPKOK                                                          
         MVC   WORK(12),PREPFAX                                                 
         B     CH13AA                                                           
RREPKOK  LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'11'                                                      
         BE    CH13B                                                            
         MVI   ELCODE1,X'11'                                                    
         BAS   RE,CHNXTEL                                                       
         BNE   CH13AA              NO X'11' ELEMENT                             
CH13B    DS    0H                                                               
         USING PUBSADEL,R2                                                      
         MVC   WORK(12),PUBSFAXN                                                
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
CH13AA   DS    0H                                                               
         LA    R2,PCONREC+33                                                    
         CLI   0(R2),X'55'         CONTRACT TEL AND FAX ELEM                    
         BE    CH13AAM                                                          
         MVI   ELCODE1,X'55'                                                    
         BAS   RE,CHNXTEL                                                       
         BNE   CH13AAZ                                                          
CH13AAM  DS    0H                                                               
         USING PCTFELEM,R2                                                      
         CLC   =C'DEFAULT',PCONFAX                                              
         BE    CH13AAZ                                                          
         CLC   =C'NONE',PCONFAX                                                 
         BNE   CH13AAP                                                          
         XC    WORK(12),WORK       NO TELEPHONE NUMBER                          
         B     CH13AAZ                                                          
CH13AAP  MVC   WORK(12),PCONFAX                                                 
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
CH13AAZ  DS    0H                                                               
         OC    WORK(12),SPACES                                                  
*                                                                               
         MVC   MYFAX,WORK          NEEDED FOR FAXING                            
*                                                                               
         CLC   MYFAX(3),=C'FX='    SEE IF CONTROL FAX CODE                      
         BNE   CH13AD                                                           
         GOTO1 =A(GETFAX),DMCB,RR=Y          PUT REAL FAX IN TOFAX              
CH13AD   DS    0H                                                               
*                                                                               
         CLC   WORK(12),SPACES     ANY FAX NUMBER                               
         BE    CH14                                                             
*                                                                               
         CLC   WORK(3),=C'FX='                                                  
         BNE   CH13AD5                                                          
         CLC   TOFAX,SPACES        CONTROL FILE FAX RECORD NOT FOUND            
         BE    CH14                TREAT LIKE NO FAX                            
*                                                                               
CH13AD5  DS    0H                                                               
         CLI   PROFOPTS+13,C'T'    SEE IF ONLY DOING TELEPHONE                  
         BE    CH14                                                             
*                                                                               
         L     R1,AMHSUBS                                                       
         LA    R1,45+0*MHL(R1)     LINE 1 POS 46                                
*                                                                               
         CLI   PROFOPTS+13,C'F'                                                 
         BE    NOTEL                                                            
         CLC   0(2,R1),SPACES      SEE IF TELEPHONE NUMBER DISPLAYED            
         BNH   NOTEL               NO - CAN USE POS 46                          
*                                                                               
         CLC   WORK(3),=C'FX='                                                  
         BNE   CH13AF              IF NOT CONTROL FAX REC                       
*                                  WILL ALWAYS FIT ON ONE LINE                  
*                                                                               
         CLC   TOFAX+12(4),=C'    '                                             
         BH    CH13AE       SEE IF CAN FIT ON SAME LINE                         
         MVC   18(3,R1),=C'FAX'                                                 
         MVC   22(12,R1),TOFAX    JUST MOVE 12 CHARS                            
         B     CH14                                                             
*                                                                               
* TOFAX TOO BIG MUST PUT ON LINE 2                                              
*                                                                               
CH13AE   L     R1,AMHSUBS                                                       
         LA    R1,45+1*MHL(R1)     LINE 2 POS 46                                
         MVC   0(3,R1),=C'FAX'                                                  
         MVC   4(16,R1),TOFAX                                                   
         B     CH14                                                             
*                                                                               
CH13AF   MVC   18(3,R1),=C'FAX'                                                 
         MVC   22(12,R1),WORK                                                   
         B     CH14                                                             
*                                                                               
NOTEL    MVC   0(3,R1),=C'FAX'     DISPLAYING ONLY FAX                          
         MVC   4(12,R1),WORK                                                    
*                                                                               
         CLC   WORK(3),=C'FX='     CHECK FOR CONTROL FILE FAX CODE              
         BNE   CH14                                                             
         MVC   4(16,R1),TOFAX      DISPLAY REAL FAX NUMBER                      
*                                                                               
* MAILING ADDRESS                                                               
*                                                                               
CH14     DS    0H                                                               
         L     R1,AMHSUBS                                                       
         LA    R1,45+4*MHL(R1)     LINE 5 POS 46                                
         CLI   PREPKEY,0                                                        
         BNE   CH16                REP - GO DO REP FIELDS FIRST                 
*                                  FROM PUB                                     
CH14B    MVC   0(20,R1),PUBNAME                                                 
         CLI   PUBZNAME,C' '                                                    
         BNH   CH14E                                                            
         CLI   QMEDIA,C'O'         BYPASS ZONE NAME                             
         BE    CH14E               FOR OUTDOOR                                  
         CLC   QZONE,=C'ZZ'        SEE IF DOING ALL ZONES                       
         BE    CH14E                                                            
         MVC   MHL(20,R1),PUBZNAME                                              
         LA    R1,MHL(R1)                                                       
CH14E    MVC   MHL*1(30,R1),PUBLINE1                                            
         MVC   MHL*2(30,R1),PUBLINE2                                            
*****                                                                           
         LA    R2,PCONREC+33                                                    
         CLI   0(R2),X'50'                                                      
         BE    CH14F                                                            
         MVI   ELCODE1,X'50'                                                    
         BAS   RE,CHNXTEL                                                       
         BNE   CH14J                                                            
         USING PCATELEM,R2                                                      
CH14F    OC    PCATNAM,SPACES                                                   
         CLC   PCATNAM,SPACES                                                   
         BE    CH14J                                                            
         XC    MHL*1(30,R1),MHL*1(R1)      CLEAR PUBLINES 1+2                   
         XC    MHL*2(30,R1),MHL*2(R1)      PRINT AFTER ATTN                     
         MVC   MHL*1(5,R1),=C'ATTN-'                                            
         MVC   MHL*1+6(24,R1),PCATNAM                                           
         MVC   MHL*2(30,R1),PUBLINE1                                            
         MVC   MHL*3(30,R1),PUBLINE2                                            
         B     CH18                                                             
         DROP  R2                                                               
*                                       FROM REP                                
*****                                                                           
CH14J    DS    0H                                                               
         LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'11'                                                      
         BE    CH15                                                             
         MVI   ELCODE1,X'11'                                                    
         BAS   RE,CHNXTEL                                                       
         BNE   CH18                                                             
CH15     DS    0H                                                               
         USING PUBSADEL,R2                                                      
         OC    PUBATTN,SPACES                                                   
         CLC   PUBATTN,SPACES                                                   
         BE    CH18                                                             
         XC    MHL*1(30,R1),MHL*1(R1)      CLEAR PUBLINES 1+2                   
         XC    MHL*2(30,R1),MHL*2(R1)      PRINT AFTER ATTN                     
         MVC   MHL*1(5,R1),=C'ATTN-'                                            
         MVC   MHL*1+6(24,R1),PUBATTN                                           
         MVC   MHL*2(30,R1),PUBLINE1                                            
         MVC   MHL*3(30,R1),PUBLINE2                                            
         B     CH18                                                             
*                                       FROM REP OR CON,ADDR                    
CH16     DS    0H                                                               
         MVC   0(30,R1),PREPNAME                                                
         MVC   MHL(30,R1),PREPLIN1                                              
         MVC   MHL*2(30,R1),PREPLIN2                                            
***REP***                                                                       
         CLI   PREPKEY+3,X'11'         SEE IF REALLY A REP                      
         BE    CH16J                   YES - NO OVERRIDE OF ATTN                
***REP***                                                                       
*****                                                                           
         LA    R2,PCONREC+33                                                    
         CLI   0(R2),X'50'                                                      
         BE    CH16F                                                            
         MVI   ELCODE1,X'50'                                                    
         BAS   RE,CHNXTEL                                                       
         BNE   CH16J                                                            
         USING PCATELEM,R2                                                      
CH16F    OC    PCATNAM,SPACES                                                   
         CLC   PCATNAM,SPACES                                                   
         BE    CH16J                                                            
         XC    MHL*1(30,R1),MHL*1(R1)      CLEAR PREPLINES 1+2                  
         XC    MHL*2(30,R1),MHL*2(R1)      PRINT AFTER ATTN                     
         MVC   MHL*1(5,R1),=C'ATTN-'                                            
         MVC   MHL*1+6(24,R1),PCATNAM                                           
         MVC   MHL*2(30,R1),PREPLIN1                                            
         MVC   MHL*3(30,R1),PREPLIN2                                            
         B     CH17                                                             
         DROP  R2                                                               
*                                  FROM REP                                     
CH16J    OC    PREPATTN,SPACES                                                  
         CLC   PREPATTN,SPACES                                                  
         BE    CH17                                                             
         XC    MHL*1(30,R1),MHL*1(R1)      CLEAR PREPLINES 1+2                  
         XC    MHL*2(30,R1),MHL*2(R1)      PRINT AFTER ATTN                     
         MVC   MHL*1(5,R1),=C'ATTN-'                                            
         MVC   MHL*1+6(20,R1),PREPATTN                                          
         MVC   MHL*2(30,R1),PREPLIN1                                            
         MVC   MHL*3(30,R1),PREPLIN2                                            
*                                                                               
CH17     DS    0H                  NOW DO PUB NAME+ADDR                         
         CLI   ADDROPT,C'Y'        SEE IF DOING PUB NAME TOO                    
         BNE   CH18                                                             
         OI    MHL*3(R1),C' '      SO LINES 8,9,10 WILL PRINT                   
         OI    MHL*4(R1),C' '                                                   
         OI    MHL*5(R1),C' '      THREE LINES TO GET ME OUT                    
         L     R1,AMHSUBS          ME OUT OF THE ENVELOPE WINDOW                
         LA    R1,45+10*MHL(R1)    TO LINE 11 COL 46                            
         B     CH14B                                                            
*                                                                               
CH18     DS    0H                                                               
*                                  MASTER CLIENT COMMENT                        
         L     R6,AMHSUBS                                                       
         LA    R6,1(R6)                                                         
         CLI   PCLTPROF+5,C'1'     TEST MASTER CLIENT                           
         BNE   CH22                NO                                           
*                                  FIND COMMENT                                 
         XC    KEY,KEY                                                          
         MVC   KEY(3),PCLTKEY      A/M                                          
         MVI   KEY+3,X'40'                                                      
         MVI   KEY+4,C' '                                                       
         LA    R2,KEY+4                                                         
         CLI   PCLTKCLT+2,C' '                                                  
         BH    *+8                                                              
         LA    R2,KEY+5                                                         
         MVC   0(3,R2),=C'MAS'                                                  
         MVC   3(3,R2),PCLTKCLT                                                 
         MVI   KEY+10,0                                                         
*                                                                               
         BAS   RE,CHHIGH                                                        
         CLC   KEY(10),KEYSAVE                                                  
         BNE   CH22                NO COMMENT                                   
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,CHGET                                                         
         L     R6,AMHSUBS                                                       
         LA    R6,1(R6)                                                         
         MVI   ELCODE1,X'40'                                                    
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'40'                                                      
         BE    CH19                                                             
CH18B    BAS   RE,CHNXTEL                                                       
         BNE   CH22                                                             
*                                                                               
CH19     DS    0H                                                               
         DROP  R4                  DROP R4 AS A POINTER TO PCONREC              
*                                  SINCE CHMOVE (BELOW) ALTERS R4               
         BAS   RE,CHMOVE                                                        
         MVC   0(30,R6),W                                                       
         LA    R6,MHL(R6)                                                       
         B     CH18B                                                            
*                                  SUPERCEDES                                   
CH22     DS    0H                                                               
         L     R4,ACONIO1          RESET A(PCONREC)                             
         USING PCONREC,R4          NOTE TEMPORARY USE OF R4                     
*                                                                               
         XC    SCONREV,SCONREV                                                  
*                                                                               
         MVC   SVQPROG,QPROG         SAVE PROG/AGY/MED                          
         MVC   SVQAGY,QAGENCY                                                   
         MVC   SVQMED,QMEDIA                                                    
*                                                                               
         MVC   SVQOPT3,QOPT3       NEEDED FOR UPDATIVE                          
*                                  SOON SPACE RESERVATIONS                      
*                                                                               
         CLC   QPROG,=C'15'                                                     
         BNE   CH22F                                                            
*                                                                               
         LA    R2,PCONREC+33                                                    
         CLI   0(R2),X'85'                                                      
         BE    CH22C                                                            
         MVI   ELCODE1,X'85'                                                    
CH22B    BAS   RE,CHNXTEL                                                       
         BNE   CH22H                                                            
         USING PASRELEM,R2                                                      
CH22C    CLC   QDIV,SPACES                                                      
         BH    CH22D                                                            
         CLC   PASRCLT,SPACES                                                   
         BH    CH22B                                                            
         B     CH22E                                                            
CH22D    CLC   QDIV,PASRCLT                                                     
         BNE   CH22B                                                            
CH22E    OC    PASRLDAT,PASRLDAT                                                
         BZ    CH22H                                                            
         MVC   SCONREV,PASRLDAT                                                 
         B     CH22H                                                            
*****                                                                           
CH22F    CLC   QCNTLDT(3),SPACES                                                
         BE    *+10                                                             
         MVC   SCONREV,QCNTLDT     TO OVERRIDE REV DATE                         
CH22H    CLC   CONAGY,PAGYKAGY     IF SLAVE AGY                                 
         BNE   CH26                BYPASS REV DATE AND REV COMMENTS             
         CLI   SCONREV,0           OVERRIDING REV DATE                          
         BNE   CH22K               YES                                          
*****                                                                           
         CLC   QPROG,=C'15'                                                     
         BNE   CH22J                                                            
         OC    SCONREV,SCONREV                                                  
         BZ    CH22M                                                            
         B     CH22K                                                            
*****                                                                           
CH22J    OC    PCONREV,PCONREV                                                  
         BZ    CH22M                                                            
         MVC   SCONREV,PCONREV                                                  
CH22K    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,SCONREV),(5,W)                                    
*                                                                               
         LA    R6,MHL(R6)                                                       
         MVC   0(25,R6),=C'SUPERSEDES CONTRACT DATED'                           
         MVC   26(8,R6),W                                                       
         LA    R6,MHL(R6)                                                       
*                                                                               
*                                  TRY TO FIND REVISION COMMENTS                
CH22M    DS    0H                                                               
         MVI   BYTE,0                                                           
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE1,X'40'                                                    
CH23     DS    0H                                                               
         BAS   RE,CHNXTEL                                                       
         BNE   CH26                                                             
         CLC   2(3,R2),=C'RC='                                                  
         BNE   CH23                                                             
         CLI   BYTE,0              TEST FIRST COMMENT                           
         BNE   CH23B                                                            
         MVI   BYTE,1                                                           
         LA    R6,MHL*1(R6)                                                     
         MVC   0(23,R6),=C'**REASON FOR REVISION**'                             
         LA    R6,MHL(R6)                                                       
*                                                                               
CH23B    DS    0H                                                               
         DROP  R4                  DROP R4 AS A POINTER TO PCONREC              
*                                  SINCE CHMOVE (BELOW) ALTERS R4               
         BAS   RE,CHMOVE                                                        
         MVC   2(30,R6),W                                                       
         LA    R6,MHL(R6)                                                       
         MVI   0(R6),C' '                                                       
         B     CH23                                                             
*                                  TERMS                                        
CH26     DS    0H                                                               
         L     R4,ACONIO1          RESET A(PCONREC)                             
         USING PCONREC,R4          NOTE TEMPORARY USE OF R4                     
*                                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE1,X'20'                                                    
         BAS   RE,CHNXTEL                                                       
         BE    CH27                                                             
         USING PUBGENEL,R2                                                      
         ZAP   PUBCD,=P'0'                                                      
         ZAP   PUBCDDAS,=P'0'                                                   
*                                                                               
         ZAP   PUBAC,=P'15000'     DEFAULT IS 15% FOR ALL                       
*                                                                               
*SMY*    CLI   QMEDIA,C'O'         OUTDOOR?                                     
*SMY*    BNE   CH27                                                             
*SMY*    CLI   PAGYNAT,C'C'        CANADIAN?                                    
*SMY*    BE    CH27                                                             
*SMY*    ZAP   PUBAC,=P'16667'     OUTDOOR AND NOT CANADIAN                     
*                                                                               
CH27     DS    0H                                                               
         LA    RF,PROFOPTS+4       SUPPRESS AGENCY COMISSION                    
         BAS   RE,OPTIONCA                                                      
         B     CH28                NO MOVE                                      
         LA    R6,MHL(R6)                                                       
         MVC   0(6,R6),=C'TERMS-'                                               
         LA    R6,MHL(R6)                                                       
         MVC   0(17,R6),=C'LESS AGENCY COMM.'                                   
         CP    PUBAC,=P'0'                                                      
         BNE   *+14                                                             
         MVC   18(4,R6),=C'NONE'                                                
         B     CH28                                                             
         MVC   W,SPACES                                                         
         EDIT  (P3,PUBAC),(6,W),3                                               
*                                                                               
         LA    R5,W+5                                                           
         CLI   0(R5),C'0'                                                       
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
*                                                                               
         CLI   0(R5),C'.'                                                       
         BNE   *+6                                                              
         BCTR  R5,R0                                                            
         MVC   1(4,R5),=C' PCT'                                                 
*                                                                               
         MVC   18(10,R6),W                                                      
*                                                                               
CH28     DS    0H                                                               
         LA    RF,PROFOPTS+3       SUPPRESS CHASH DISCOUNT                      
         BAS   RE,OPTIONCA                                                      
         B     CH30                NO MOVE                                      
*                                                                               
         LA    R5,PCONREC+33                                                    
         MVI   ELCODE1,X'50'                                                    
*                                                                               
         SR    R0,R0                                                            
CH28C    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    CH28H                                                            
         CLC   0(1,R5),ELCODE1                                                  
         BNE   CH28C                                                            
*                                                                               
         USING PCATELEM,R5                                                      
         CLC   PCATPCT,=2X'00'             IF HEX ZEROS, THEY ENTERED           
         BE    CH28H                       NOTHING ON CONTRACT SCREEN.          
         LA    R6,MHL(R6)                                                       
         MVC   5(10,R6),=C'CASH DISC.'                                          
         CLC   PCATPCT,=2X'FF'             IF HEX F'S, THEY ENTERED AN          
         BNE   CH28E                       AMOUNT OF ZERO.                      
         MVC   W,=C'0.0'                                                        
         B     CH28P                                                            
*                                                                               
CH28E    DS    0H                                                               
         MVC   W,SPACES                                                         
         EDIT  PCATPCT,(4,W),1                                                  
         B     CH28P                                                            
         DROP  R5                                                               
*****                                                                           
CH28H    LA    R6,MHL(R6)                                                       
         MVC   5(10,R6),=C'CASH DISC.'                                          
         CP    PUBCD,=P'0'                                                      
         BNE   *+14                                                             
CH28M    MVC   18(4,R6),=C'NONE'                                                
         B     CH30                                                             
*                                                                               
         MVC   W,SPACES                                                         
         EDIT  (P2,PUBCD),(3,W),1                                               
*                                                                               
CH28P    MVC   W+5(3),=C'PCT'                                                   
         CP    PUBCDDAS,=P'0'                                                   
         BE    CH29                                                             
         MVI   W+8,C'/'                                                         
         EDIT  (P2,PUBCDDAS),(2,W+9)                                            
*                                                                               
         MVC   W+12(4),=C'DAYS'                                                 
*                                                                               
CH29     DS    0H                                                               
         MVC   18(16,R6),W                                                      
*                                                                               
CH30     DS    0H                                                               
         CLI   QMEDIA,C'O'                                                      
         BNE   CH32                                                             
         CLI   PUBKZON,0                                                        
         BE    CH32                                                             
         CLC   QZONE,=C'ZZ'                                                     
         BE    CH32                                                             
         CLI   PUBZNAME,C' '                                                    
         BNH   CH32                                                             
         LA    R6,MHL*1(R6)     OUTDOOR OVERWRITTEN WHEN SUPERSEDED L02         
         MVC   0(6,R6),=C'MARKET'                                               
*                                  OUTDOOR - ST,MKTNAME                         
         MVC   8(2,R6),PUBSTACD    USE STACD IF THERE                           
         CLI   8(R6),C' '                                                       
         BNH   CH31                                                             
         CLI   8(R6),C'0'                                                       
         BL    *+10                                                             
CH31     DS    0H                                                               
         MVC   8(2,R6),PUBSTATE    ELSE USE STATE                               
         MVI   10(R6),C','                                                      
         MVC   12(20,R6),PUBZNAME                                               
*                                                                               
CH32     DS    0H                                                               
         LA    R6,MHL(R6)                                                       
         MVI   0(R6),C' '                                                       
CHXIT    XIT1                                                                   
*                                                                               
* RF POINTS TO OPTION                                                           
* IF PROGRAM IS 12  OR 13 AND OPTION IS YES WILL RETURN 0(RE)                   
* OTHERWISE 4(RE)                                                               
*                                                                               
OPTIONCA CLC   QPROG,=C'13'                                                     
         BE    OPTNCA5                                                          
         CLC   QPROG,=C'12'                                                     
         BNE   4(RE)                                                            
OPTNCA5  CLI   0(RF),C'N'                                                       
         BE    4(RE)                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
CKPROD   CLC   QPROG,=C'12'                                                     
         LA    RF,PROFOPTS+6       12 PRODUCT OPTION                            
         BE    CKPROC                                                           
         CLC   QPROG,=C'13'        OR CONTRACT TURNAROUND                       
         BE    CKPROC                                                           
*                                                                               
         CLC   QPROG,=C'15'                                                     
         LA    RF,PROFOPTS+11      15 PRODUCT OPTION                            
         BNE   8(RE)               NORMAL PROCESSING                            
CKPROC   CLI   0(RF),C'N'          NORMAL PROCESSING                            
         BE    8(RE)                                                            
         CLI   0(RF),C'Y'          PRINT PRODUCT CODE ONLY                      
         BER   RE                                                               
         CLI   0(RF),C'B'          DO NOT PRINT CODE OR DESCRIPTION             
         BE    4(RE)                                                            
         B     8(RE)                                                            
*                                                                               
         DROP  R2,R4                                                            
*                                                                               
         EJECT                                                                  
CHHIGH   MVC   KEYSAVE,KEY                                                      
         LA    R0,DMRDHI                                                        
*                                                                               
CHDIR    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(R0),PRTDIR,KEY,KEY                                 
*                                                                               
         B     CHDMX                                                            
*                                                                               
CHGET    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
CHDMX    TM    8(R1),X'FF'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
CHNXTEL  SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    CHNXTELX                                                         
         CLC   0(1,R2),ELCODE1                                                  
         BER   RE                                                               
         B     CHNXTEL+2                                                        
CHNXTELX LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 3                                                                
CHFLOAT  CLI   0(R7),C' '                                                       
         BHR   RE                                                               
         BCT   R7,*-6                                                           
         BR    RE                                                               
         SPACE 3                                                                
CHMOVE   DS    0H                                                               
         SR    R3,R3                                                            
         IC    R3,1(R2)                                                         
         LA    R4,2                                                             
         CLC   2(3,R2),=C'RC='                                                  
         BNE   *+8                                                              
         LA    R4,5                                                             
         SR    R3,R4               R3 = LENGTH                                  
         AR    R4,R2               R4 = START                                   
         CHI   R3,30                                                            
         BNH   *+8                                                              
         LHI   R3,30                                                            
         BCTR  R3,R0                                                            
         XC    W,W                                                              
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   W(0),0(R4)                                                       
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
* GET FAX NUMBER FROM CONTROL FILE FAX RECORD                                   
*                                                                               
GETFAX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   TOFAX,SPACES                                                     
         MVC   SIOKEY,KEY       SAVE KEY                                        
GETFAXB  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,QAGENCY                                                  
         MVC   CTFXCODE,MYFAX+3                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,IO                     
         CLC   IO(18),KEYSAVE      COMPARE 7-BYTE FAX CODE                      
         BNE   GETFAXX                                                          
         LA    R4,IO                                                            
         LA    R6,CTFXEL1                                                       
         B     GETFX4                                                           
         SPACE 1                                                                
GETFX2   ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         SPACE 1                                                                
GETFX4   CLI   0(R6),0                                                          
         BE    GETFAXX                                                          
         CLI   0(R6),CTFX1ELQ                                                   
         BE    GETFXNO                                                          
         B     GETFX2                                                           
*                                                                               
         USING CTFX1EL,R6                                                       
GETFXNO  ZIC   R1,CTFX1LEN         FAX NUMBER                                   
         AHI   R1,-3                                                            
         CHI   R1,24                                                            
         BL    *+8                                                              
         LA    R1,24                                                            
         EX    R1,*+8                                                           
         B     GETFX2                                                           
         MVC   TOFAX(0),CTFX1NUM                                                
*                                                                               
GETFAXX  MVC   KEY,SIOKEY      RESTORE MY KEY                                   
         B     GETFAXXX                                                         
*                                                                               
SIOKEY   DS    CL32                                                             
IO       DS    CL500                                                            
*                                                                               
GETFAXXX XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PP1202 - PRINT CONTRACT SCHEDULES'                              
CONSCHED CSECT                                                                  
         NMOD1 0,CONSCHED                                                       
         USING PPWORKD,RA          RA=V(PPWORKC)                                
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9       R9=RC+4096                                   
         USING PP12WRKD,R8         R8=A(PP12WRKD)                               
         LA    R7,CONSCHED+4095                                                 
         LA    R7,1(R7)                                                         
         USING CONSCHED+4096,R7     ** NOTE USE OF SECOND BASE **               
*                                                                               
         MVI   DMINBTS,0                                                        
         MVI   DMOUTBTS,0                                                       
         MVI   LNEED,0                                                          
         CLI   QOPT5,C' '                                                       
         BE    CONSCH0                                                          
         CLI   SCONREV,0                                                        
         BE    CONSCH0                                                          
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
CONSCH0  DS    0H                                                               
         L     R6,ACONIO1          A(PCONREC)                                   
         USING PCONREC,R6                                                       
*                                                                               
         MVI   ACTSW,0             RESET ACTIVITY SW                            
         MVI   PUBSW,0                                                          
         MVI   PRDSW,0                                                          
         MVI   CLTSW,0                                                          
*                                                                               
         MVI   ININD,0             LINES VS. INCH                               
         CLI   QOPT7,C'I'          INCHES                                       
         BE    CONSCH0D                                                         
         CLI   QOPT7,C'L'          LINES                                        
         BE    CONSCH0E                                                         
*                                                                               
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE1,X'20'                                                    
         BAS   RE,CSNEXTEL                                                      
         BNE   CONSCH0E                                                         
*                                  INCHES/LINES FROM FRST RATE ELEM             
         CLI   PRBLIND-PRBELEM(R2),C'I'                                         
         BNE   CONSCH0E                                                         
CONSCH0D MVI   ININD,1                                                          
CONSCH0E OC    REQPUB,REQPUB                                                    
         BZ    *+10                                                             
         MVC   PCONKPUB(6),REQPUB                                 L11           
*                                                                               
         LA    RE,EDTTOTS          POINT TO EDT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,CONTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         ZAP   TOTEDS,=P'0'                                                     
         CLI   QEDITION,C'Z'       SEE IF DOING ALL EDTS                        
         BNE   CONSCH0X                                                         
***  NOW USES EDITION TABLE FOR ALL 'ALL EDITION' REQS                          
***                                                                             
***  USED TO TO ONLY FOR MASTER CLT REQUEST                                     
*        CHANGED TO BUILD TABLE OF EDITIONS FOR ALL ALL EDTION REQS             
*                                                                               
         GOTO1 AFNDEDTS                                                         
*                                  SET EDITION TO FIRST ONE IN TABLE            
         L     R4,ANXTEDT                                                       
         MVC   PCONKPUB+4(2),0(R4)                                              
*                                                                               
CONSCH0X XC    MLIST,MLIST                                                      
         L     RF,AGRTAB                                                        
         MVI   0(RF),0                                                          
         BAS   RE,CKPRODC  CHECK PRODUCT OPTIONS                    L06         
         B     CONSC11     PRINT JUST PROD CODE                     L06         
         B     *+8         NOTHING TO PRINT                         L06         
         B     CONSC11     PRINT JUST PROD CODE                     L06         
         XC    CSHN(15),CSHN                                        L06         
         XC    CSHM(15),CSHM                                        L06         
         XC    CSHO2(15),CSHO2                                      L06         
         XC    CSHNUND(15),CSHNUND                                  L06         
         MVC   CSHMUND(15),CSHNUND                                  L06         
         MVC   CSHOUND(15),CSHNUND                                  L06         
         B     CONSC11A                                             L06         
*                                  FIRST TIME HEADLINE                          
*                                  SET HEADLINES FOR NO EDIT TOTALS             
CONSC11  MVC   CSHN(15),=CL15'PRODUCT'                                          
         MVC   CSHM(15),=CL15'PRODUCT'                                          
         MVC   CSHO2(15),=CL15'PRODUCT'                                         
         MVC   CSHNUND(15),=CL15'-------'                                       
         MVC   CSHMUND(15),=CL15'-------'                                       
         MVC   CSHOUND(15),=CL15'-------'                                       
CONSC11A MVC   CSHO+47(17),=C'SIZE  -DISPLAYS- '                    L02         
         MVC   CSHO2+47(5),=C'SHOW '                                L02         
         CLI   QEDITION,C'Z'                                                    
         BNE   CONSCH1                                                          
*                                  FIX HEADLINES FOR EDIT TOTALS                
         BAS   RE,CKPRODC  CHECK PRODUCT OPTIONS                    L06         
         B     CONSC1X     PRINT JUST PROD CODE                     L06         
         B     *+8         NOTHING TO PRINT                         L06         
         B     CONSC1X     PRINT JUST PROD CODE                     L06         
         MVC   CSHN(7),=C'EDITION'                                 L06          
         MVC   CSHM(7),CSHN                                        L06          
         MVC   CSHO2(6),=C'MARKET'                                 L06          
         MVC   CSHNUND(7),=CL15'-------'                            L06         
         MVC   CSHMUND(7),=CL15'-------'                            L06         
         MVC   CSHOUND(7),=CL15'-------'                            L06         
         B     CONSCH1                                              L06         
CONSC1X  MVC   CSHN(15),=C'EDITION/PRODUCT'                                     
         MVC   CSHM(15),=C'EDITION/PRODUCT'                                     
         MVC   CSHO2(14),=C'MARKET/PRODUCT'                                     
         MVC   CSHNUND(15),DASHES1                                              
         MVC   CSHMUND(15),DASHES1                                              
         MVC   CSHOUND(14),DASHES1                                              
*                                                                               
CONSCH1  DS    0H                                                               
         CLI   QOPT4,C'G'          TEST GRID                                    
         BE    CONSCH2                                                          
*                            SEE IF SKIPPING WILL CAUSE PAGE BREAK              
*                            IF YES - DON'T SKIP                                
         ZIC   RE,LINE                                                          
         AHI   RE,2                                                             
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BNL   CONSCH1A                                                         
         MVI   SPACING,2                                                        
         BAS   RE,CSPRT                                                         
*                                                                               
CONSCH1A DS    0H                                                               
***      CLI   LINE,20                                                          
***      BL    CONSCH2             JUST PRINTED ALL HEADLINES                   
         MVI   LNEED,10                                                         
         MVI   SPACING,2                                                        
         MVC   P+33(14),=C'** SCHEDULE **'                                      
         BAS   RE,CSPRT                                                         
         LA    R5,SPACES                                                        
         CLI   QOPT2,C'X'           SEE IF SUPPRESSING COSTS                    
         BE    CONSCH1B                                                         
         LA    R5,GRSWDS                                                        
         CLI   QOPT2,C'N'                                                       
         BNE   *+8                                                              
         LA    R5,NETWDS                                                        
*                                                                               
CONSCH1B DS    0H                                                               
         LA    RF,PROFOPTS+7       OVERRIDE WITH GRP                L02         
         BAS   RE,OPTIONCB                                          L02         
         B     *+8                 OVERRIDE                         L02         
         B     *+16          NO    OVERRIDE                         L02         
         MVC   CSHO+47(5),=C'GRP  '                                 L02         
         MVC   CSHO2+47(6),=C'LEVEL '                               L02         
*                                                                   L02         
         LA    RF,PROFOPTS+8       USE MEDIA ALLOCATION             L02         
         BAS   RE,OPTIONCB                                          L02         
         B     *+8                 OVERRIDE                         L02         
         B     *+10          NO    OVERRIDE                         L02         
         MVC   CSHO+53(11),=C'MEDIA ALLOC'                          L02         
*                                                                   L02         
         LA    R2,CSHO                                                          
         LA    R3,3                                                             
         CLI   QMEDIA,C'O'                                                      
         BE    CONSCH1D                                                         
         LA    R5,12(R5)                                                        
         LA    R2,CSHN                                                          
         LA    R3,2                                                             
         CLI   QMEDIA,C'N'                                                      
         BE    CONSCH1D                                                         
         LA    R2,CSHM                                                          
         LA    R3,2                                                             
CONSCH1D DS    0H                                                               
         MVC   P+66(12),0(R5)                                                   
         MVC   P+1(L'CSHN),0(R2)                                                
*                                                                               
         JIF   QOPT2,NE,C'X',OR,QMEDIA,NE,C'N',CONSCH1F,JUMP=N                  
*                            IF NEWSPAPERS AND SUPPRESSING COST                 
*                            BLANK-OUT 'RATE' AND ITS UNDERLINING               
*                                                                               
         MVC   P+58(8),SPACES                                                   
CONSCH1F DS    0H                                                               
         BAS   RE,CSPRT                                                         
         LA    R2,L'CSHN(R2)                                                    
         LA    R5,12(R5)                                                        
         BCT   R3,CONSCH1D                                                      
         BAS   RE,CSPRT                                                         
CONSCH2  DS    0H                                                               
*                                                                               
         LA    RE,PRDTOTS          POINT TO PRD TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,CLTTOTS          POINT TO CLT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,MASTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         MVC   WORKCLT,PCLTKEY                                                  
         CLI   PCLTPROF+5,C'1'     TEST THIS A MASTER CLIENT                    
         BNE   CS1                                                              
* READ THRU CLTHDRS FOR SLAVE CLIENTS                                           
         XC    WORKCLT+4(4),WORKCLT+4        FOR FIRST TIME                     
*                                                                               
CSCLT2   XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT                                                   
         MVI   KEY+7,X'FF'                                                      
         BAS   RE,CSHIGH                                                        
         B     *+8                                                              
CSCLT4   BAS   RE,CSSEQ                                                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CSENDMAS                                                         
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
*                                                                               
         BAS   RE,CSGET                                                         
*                                                                               
         CLC   PCLTKCLT,PCLTPROF+6-PCLTKEY+PBUYREC                              
         BNE   CSCLT4                                                           
*                                  SEE IF DOING ONE SLAVE CLIENT                
         CLI   QDIV,C' '                                                        
         BE    CS0                                                              
         CLC   PBUYREC+4(3),QDIV                                                
         BNE   CSCLT4              SKIP OTHER CLIENTS                           
*                                                                               
CS0      MVC   WORKCLT,PBUYREC     SAVE THIS CLIENT KEY                         
         MVC   WORKCNM,PCLTNAME-PCLTKEY+PBUYREC  AND NAME                       
*                                                                               
* READ PRDHDRS FOR NAMES AND BUILD TABLE                                        
*                                                                               
CS1      DS    0H                                                               
         CLC   LASTCKEY,WORKCLT    TEST SAME CLIENT                             
         BE    CS4                                                              
         MVC   LASTCKEY,WORKCLT                                                 
*                                                                               
         LA    R0,PPRDKEY                                                       
         ST    R0,IOAREA                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT      A/M/X/CLT                                    
         MVI   KEY+3,6                                                          
         L     R3,APRDTAB                                                       
         LA    R4,24                                                            
         L     R5,APRDTABX                                                      
         BAS   RE,CSHIGH                                                        
         B     *+8                                                              
CS2      BAS   RE,CSSEQ                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CS4                                                              
*              SKIP OAR DATA - PRD START WITH *                                 
         CLI   KEY+7,C'*'                                                       
         BE    CS2                                                              
         BAS   RE,CSGET                                                         
         MVC   0(3,R3),PPRDKPRD                                                 
         MVI   3(R3),C' '                                                       
*                                                                   L02         
         MVC   4(20,R3),PPRDNAME                                                
         BXLE  R3,R4,CS2                                                        
         DC    H'0'                TOO MANY PRDS                                
*                                                                               
CS4      XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT      A/M/X/CLT                                    
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),PCONKPUB                                                
*                                  NOTE - IF DOING ALL ZONES,EDTS               
*                                  THEN PCONPRD IS FROM FIRST CON READ          
**NEW 2/18/88                                                                   
         MVI   ONEPRD,C'N'                                                      
         CLI   PCONPRD,C'A'        SEE IF DOING A PRD-CONTRACT                  
         BL    *+10                                                             
         MVC   KEY+13(3),PCONPRD                                                
         JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',CS4B,JUMP=N           
         MVC   KEY+13(3),QPRODUCT                                               
*                                                                               
CS4B     DS    0H                                                               
**NEW 2/18/88                                                                   
         OC    KEY+13(3),KEY+13      SEE IF DOING ONE PRODUCT                   
         BZ    CS4C                                                             
         MVC   KEY+16(3),CSTART      CAN SET DATE                               
         MVI   ONEPRD,C'Y'           SET DOING ONE PRODUCT                      
**NEW 2/18/88                                                                   
CS4C     DS    0H                                                               
         BAS   RE,CSHIGH                                                        
         B     CS6A                                                             
CS6      MVC   KEYSAVE,KEY                                                      
         BAS   RE,CSSEQ                                                         
CS6A     DS    0H                                                               
         CLC   KEY(16),KEYSAVE     SAME A/M/X/C/PUB/PRD                         
         BNE   CSENDPRD                                                         
*                                                                               
CS7      CLI   KEY+13,C'*'         SKIP OTHER AGY DATA                          
         BE    CS6                                                              
         CLC   KEY+16(3),CSTART   TEST BUY WITHIN CONTRACT PERIOD               
**NEW 2/18/88     WAS BL CS6                                                    
         BL    CSLOW                                                            
**NEW 2/18/88                                                                   
         CLC   KEY+16(3),CEND                                                   
**NEW 2/18/88     WAS BH CS6                                                    
         BH    CSNEXTP                                                          
**NEW 2/18/88                                                                   
**                      CODE DELETED TO CHK PRD SINCE ONE PRODUCT               
**                      REQUESTS SET PRD IN KEY AT CS4A                         
**                      CHG OF PRODUCT CHECKED AT CS6A                          
**                      ALSO CSENDPRD WILL STOP READING                         
**                                                                              
*                                                                               
         B     CS7A0                                                            
**NEW 2/18/88                                                                   
CSLOW    MVC   KEY+16(3),CSTART       TO SKIP TO CONTRACT START                 
         XC    KEY+19(6),KEY+19       MUST CLEAR EST                            
         B     CS4C                   GO TO READ HIGH                           
*                                                                               
CSNEXTP  CLI   ONEPRD,C'Y'            IF DOING ONE PRD - DONE                   
         BE    CSENDPRD                                                         
         MVC   KEY+16(3),=3X'FF'      TO SKIP TO THE NEXT PRODUCT               
         XC    KEY+19(6),KEY+19       MUST CLEAR EST                            
         B     CS4C                   GO TO READ HIGH                           
**NEW 2/18/88                                                                   
**                                                                              
CS7A0    DS    0H                                                               
*                                                                               
CS7A0D   DS    0H                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,CSGET            READ BUYREC                                  
*****                                                                           
         CLI   PBDBFD,C'T'         SKIP TEST BUYS                               
         BE    CS6                                                              
*****                                                                           
         TM    PBDSTAT,X'08'                                                    
         BO    CS6                 AND "HELD" CASH IN ADVANCE BUYS              
*****                                                                           
*                                                                               
         OC    SVESTFLT,SVESTFLT   FILTERING ON ESTIMATE ?                      
         BZ    CSESTX              NO                                           
         CLC   SVESTFLT,KEY+19                                                  
         BNE   CS6                 EST DOESN'T MATCH, SKIP                      
*                                                                               
CSESTX   DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
CS7A0E   GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
*                                                                               
*                                  DETERMINE CHANGE STATUS                      
         MVI   CHGSW,0                                                          
         CLI   QOPT5,C' '                                                       
         BE    CS7A01                                                           
         CLI   SCONREV,0                                                        
         BNE   CS7A01B                                                          
CS7A01   DS    0H                                                               
         TM    PBUYCNTL,X'80'                                                   
         BNZ   CS6                                                              
         B     CS7A08                                                           
CS7A01B  DS    0H                                                               
         CLC   PBDBUYDT,SCONREV                                                 
         BNH   CS7A02                                                           
         MVI   CHGSW,C'N'          NEW                                          
         B     CS7A04                                                           
*                                                                               
CS7A02   DS    0H                                                               
         CLC   PBDCHGDT,SCONREV                                                 
         BNH   *+8                                                              
         MVI   CHGSW,C'C'          CHANGE                                       
CS7A04   DS    0H                                                               
         TM    PBUYCNTL,X'80'                                                   
         BZ    CS7A06                                                           
         CLC   PBDDATE,SCONREV     DELETED BEFORE LAST REV                      
         BNH   CS6                 YES - BYPASS                                 
         CLI   CHGSW,C'N'          ADDED AND DEL'D SING LAST REV                
         BE    CS6                 YES - BYPASS                                 
         MVI   CHGSW,C'D'          DELETED                                      
         XC    GROSS(20),GROSS                                                  
CS7A06   DS    0H                                                               
         CLI   CHGSW,0             TEST ANY GHANGE                              
         BNE   CS7A08              YES                                          
         CLI   QOPT5,C'C'          NO- SHOW ONLY CHANGES                        
         BE    CS6                 YES - BYPASS                                 
*                                                                               
CS7A08   DS    0H                                                               
*                                                                               
*                                                                               
         CLI   QOPT3,C'R'          SEE IF SPACE RESERVATION                     
         BNE   CS7A08X          FOR P15'S - SET FROM PROFILE                    
*                                                                               
         CLI   PROGPROA+6,C'Y'  AM I MARKING INSERTIONS?                        
         BNE   CS7A08X                                                          
******   CLI   SOONSW,C'Y'         NO-OP FOR UPDATE SOONS                       
******   BE    CS7A08X                                                          
         TM    PBDSTAT,X'02'       SEE IF ALREADY SET                           
         BNZ   CS7A08X             CAN SKIP PUTREC                              
         OI    PBDSTAT,X'02'       HAS BEEN ON SPACE RES                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   CS7A08X                                                          
         BAS   RE,CSPUT            WRITE RECORD                                 
*                                                                               
CS7A08X  DS    0H                                                               
         CLI   QEDITION,C'Z'                                                    
         BNE   CS7A10                                                           
         CLI   PUBSW,0             TEST NEW PUB                                 
         BNE   CS7A10              NO                                           
         MVI   PUBSW,1                                                          
         GOTO1 ACSPUB                                                           
         CLC   W,SPACES                                                         
         BE    CS7A10                                                           
         MVI   LNEED,10                                                         
         MVC   P+1(30),W                                                        
         MVC   PSECOND+1(30),W+30                                               
         BAS   RE,CSPRT                                                         
         MVI   SPACING,2                                                        
         MVC   P+1(30),W+60                                                     
         BAS   RE,CSPRT                                                         
*                                                                               
CS7A10   DS    0H                                                               
         MVI   ACTSW,1                                                          
         CLI   QOPT2,C'N'                                                       
         BE    CS7A10B                                                          
         CLI   PBDCTYP,C'N'        SHOW NET IF INPUT AS NET                     
         BNE   CS7A1                                                            
*                                                                               
CS7A10B  DS    0H                                                               
         L     R0,GROSS                                                         
         S     R0,AGYCOM                                                        
         ST    R0,GROSS            USE NET NOT GROSS                            
*                                                                               
         DROP  R6                                                               
*                                                                               
CS7A1    DS    0H                                                               
         LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
*                                                                               
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         MVC   PBYODTCN,DATCON                                                  
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
         MVI   PBYOCTL,X'48'       SPECIAL OUTDOOR + REG COMMS                  
*                                                                               
*                                  CHECK DELETED BUY COMMENT OPTION             
*                                                                               
         CLI   PROGPROA+3,C'R'     SEE IF SUPPRESSING REGULAR                   
         BNE   CS7A5               COMMENTS                                     
*                                                                               
         TM    PBUYCNTL,X'80'      SEE IF DELETED BUY                           
         BZ    CS7A5                                                            
         MVI   PBYOCTL,X'40'       NO COMMENTS FOR DELETED BUYS                 
*                                                                               
CS7A5    DS    0H                                                               
         OI    PBYOCTL,X'01'       NET RATES AS NET                             
         CLI   DATEOPT,C'L'                                                     
         BE    *+8                                                              
         OI    PBYOCTL,X'02'       SUPPRESS LINE NO.                            
*                                                                               
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
         CLI   PBDSPACE,C'*'  IF NOT "REAL" INSERTION CLEAR 2ND DATE            
         BNE   *+10                                                             
         MVC   PBYOMDY2(L'PBYOMDY2),SPACES   CLEAR SECOND DATE                  
*                                                                               
* SET UP PRINT LINE                                                             
         CLI   PRDSW,0             TEST NEW PRD                                 
         BNE   CS8                 NO                                           
         MVI   PRDSW,1                                                          
         CLI   PCLTPROF+5,C'1'     TEST MASTER CLT                              
         BNE   CS7B                                                             
* PRINT SLAVE CLIENT NAME                                                       
         SR    RE,RE               NEED 4 LINES                                 
         IC    RE,LINE                                                          
         LA    RE,4(RE)                                                         
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    *+8                                                              
         MVI   LNEED,4                                                          
         CLI   CLTSW,0             TEST NEW CLT                                 
         BNE   CS7B                NO                                           
         MVI   CLTSW,1                                                          
         MVI   P+1,C'*'                                                         
         MVC   P+3(20),WORKCNM                                                  
         LA    R2,P+25                                                          
         BAS   RE,CSFLOAT                                                       
         MVI   2(R2),C'*'                                                       
         MVI   SPACING,2                                                        
*                                                                               
         BAS   RE,CSPRT                                                         
*                                                                               
CS7B     DS    0H                                                               
         CLI   QOPT4,C'G'         TEST GRID                                     
         BE    CS8                                                              
         L     R3,APRDTAB                                                       
         CLC   PBUYKPRD,0(R3)                                                   
         BE    *+12                                                             
         LA    R3,24(R3)                                                        
         B     *-14                                                             
         BAS   RE,CKPRODC  CHECK PRODUCT OPTIONS                    L06         
         B     CS7AA       PRINT JUST PROD CODE                     L06         
         B     CS7AB       NOTHING TO PRINT                         L06         
         MVI   P+4,C'-'                                                         
         MVC   P+5(20),4(R3)                                                    
CS7AA    MVC   P+1(3),PBUYKPRD                                                  
CS7AB    MVC   SAVPRD,PBUYKPRD      SAVE CODE AND NAME                          
         MVC   SAVPRDNM,4(R3)       FOR NEW PAGE                                
*                                                                               
CS8      DS    0H                                                               
         CLI   QOPT4,C'G'         TEST GRID                                     
         BNE   CS8A                                                             
         GOTO1 AGRID,DMCB,=C'B'                                                 
*                                                                               
         B     CS15                                                             
CS8A     DS    0H                                                               
         CLI   CHGSW,0                                                          
         BE    CS8B                                                             
         MVI   P+26,C'*'                                                        
         MVI   P+30,C'*'                                                        
         MVC   P+27(3),=C'NEW'                                                  
         CLI   CHGSW,C'N'                                                       
         BE    CS8B                                                             
         MVC   P+27(3),=C'CHA'                                                  
         CLI   CHGSW,C'C'                                                       
         BE    CS8B                                                             
         MVC   P+27(3),=C'DEL'                                                  
CS8B     CLI   PBDSPACE,C'*'      NOT 'REAL' INSERTION                          
         BE    CS9                                                              
         CLI   PBYOSPC,C'*'       NOT 'REAL' INSERTION                          
         BE    CS9                                                              
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+32(3),DUB                                                      
*                                                                               
         MVC   P+36(11),PBYOMDY     SHOW YEAR AND MAYBE LINE NO.                
*                                                                               
CS9      DS    0H                                                               
         L     RF,PBYOLNS          FROM PPBYOUT                                 
         LA    R5,PBYOCOMS                                                      
         LA    R4,5                FOR BCT                                      
CS9B     CLC   34(5,R5),SPACES     SEE IF COMMENT WILL HAVE TO BE               
         BE    *+8                 CHOPPED                                      
         LA    RF,1(RF)                                                         
         LA    R5,47(R5)                                                        
         BCT   R4,CS9B                                                          
*                                  COUNT LINES NEEDED                           
*                                                                               
CS9D     DS    0H                                                               
         CLI   QOPT6,C' '                                                       
         BE    CS9F                                                             
         LA    RF,1(RF)            JOB NO.                                      
         TM    QOPT6,X'02'                                                      
         BZ    *+8                                                              
         LA    RF,3(RF)            +CAPTION                                     
*                                                                               
CS9F     DS    0H                                                               
         STC   RF,LNEED                                                         
         MVC   P+48(20),PBYOSPC                                                 
         CLI   QMEDIA,C'N'                                                      
         BNE   CS10                                                             
* NEWSPAPER FORMAT                                                              
         CLI   PBYOSPC,C' '        SEE IF I HAVE SPACE                          
         BH    CS9H                YES DON'T SHOW UNITS HERE                    
         MVC   P+48(7),PBYOUNTS                                                 
CS9H     CLI   PBDCOSTY,C'U'        SEE IF UNIT COST                            
         BNE   CS10                                                             
         CLI   QOPT2,C'X'         SEE IF SUPPRESSING COST                       
         BE    CS10               THEN NO RATES AS WELL                         
*                                                                               
         MVC   P+58(8),PBYOUR                                                   
CS10     DS    0H                                                               
*                                                                               
*                                                                               
CS12     DS    0H                                                               
         CLI   QOPT2,C'X'        SEE IF SUPPRSSING COSTS                        
         BE    CS12C                                                            
*                                                                               
         MVC   P+66(13),PBYOGRS+1                                               
         JIF   QOPT2,NE,C' ',OR,PBDCTYP,NE,C'N',CS12C,JUMP=N                    
*                            FLOAT AN N IN FRONT OF NET COSTS                   
*                            ON CONTRACTS SHOWING BOTH GROSS AND NET            
         LA    R5,12                                                            
         OC    P+66(12),SPACES                                                  
         LA    R4,P+65                                                          
CS12A    CLI   1(R4),C' '                                                       
         BE    CS12B                                                            
         MVI   0(R4),C'N'                                                       
         B     CS12C                                                            
*                                                                               
CS12B    LA    R4,1(R4)                                                         
         BCT   R5,CS12A                                                         
*                                                                               
CS12C    DS    0H                                                               
         BAS   RE,CSPRT                                                         
         LA    R2,P+36                                                          
         LA    RF,PBYOMDY2         SECOND DATE                                  
         CLI   0(RF),C' '                                                       
         BNH   CS12C2                                                           
         LA    R2,PSECOND+36  MUST USE PSECOND FOR POSSIBLE DAY OF WK           
         MVI   P+36,C'-'                                                        
         MVC   P+37(8),0(RF)                                                    
         CLI   QMEDIA,C'N'         ONLY FOR NON-NEWS                            
         BNE   CS12C3                                                           
         MVI   P+35,C'+'                                                        
         MVC   P+36(8),0(RF)                                                    
         MVI   P+44,C' '           MAY BE SOMETHING FROM MAG LOGIC              
         B     CS12C3                                                           
*                                                                               
CS12C2   DS    0H                                                               
         LA    RF,PBYOISNM                                                      
         CLC   0(L'PBYOISNM,RF),SPACES                                          
         BE    CS12C3                                                           
         LA    R2,PSECOND+36                                                    
*        MVI   P+36,C' '                                                        
         MVC   P+36(11),0(RF)                                                   
*                                                                               
CS12C3   DS    0H                                                               
         CLI   PROGPROA+7,C'Y'     SEE IF SHOWING DAY OF WEEK                   
         BNE   CS12C5                                                           
         CLI   PBDSPACE,C'*'       DATE WON'T SHOW IF NOT "REAL"                
         BE    CS12C5              INSERTION,SO DON'T SHOW DAY                  
         CLI   PBDFREQ,C'M'        SEE IF DAY ENTERED IN $BUY                   
         BE    CS12C5              MONTHLY                                      
         CLC   PBYOBFD,SPACES      NO DAY FOR BEST FOOD DAY OR                  
         BNE   CS12C5              WEEK OF INSERTIONS                           
         CLI   QMEDIA,C'N'         ONLY FOR MEDIA N AND I                       
         BE    CS12C4                                                           
         CLI   QMEDIA,C'I'                                                      
         BE    CS12C4                                                           
         CLI   PBUYKDAT+2,X'01'     SEE IF FIRST OF MONTH                       
         BE    CS12C5               YES - DON'T SHOW FOR OTHER MEDIA            
CS12C4   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(0,WORK)                                
*      NOTE -  R2 WILL EITHER POINT TO P+36 OR PSECOND+36                       
         GOTO1 GETDAY,DMCB,WORK,0(R2)                                           
*                                                                               
*        IF SECOND DATE PRESENT - SHOW IT'S DAY TOO                             
*                                                                               
         CLI   PBYOMDY2,C' '                                                    
         BNH   CS12C5                                                           
         GOTO1 DATCON,DMCB,(3,PBDIDAT2),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,4(R2)                                           
         MVI   3(R2),C'/'                                                       
*                                                                               
CS12C5   CLC   PBYOSPC2,SPACES                                                  
         BE    *+10                                                             
         MVC   P+48(17),PBYOSPC2                                                
*                                                                               
         CLI   PBYOPRM,C' '                                                     
         BNH   CS12E                                                            
         LA    RF,P+48                                                          
         CLI   0(RF),C' '                                                       
         BNH   CS12D                                                            
         LA    RF,15(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,3(RF)                                                         
*                                                                               
CS12D    DS    0H                                                               
         CLI   PBYOPRM+1,C'C'    SEE IF COLOR PREMIUM                           
         BNE   *+10                                                             
         MVC   0(2,RF),PBYOPRM                                                  
*                         IF COLOR PREMIUM ALWAYS DISPLAY COLORS                
*                                                                               
         CLI   QOPT2,C'X'        SEE IF SUPPRESSING COST AND PREMIUMS           
         BE    CS12E                                                            
*                                                                               
         MVC   0(11,RF),PBYOPRM                                                 
         CLI   PBYOPRM+1,C'C'      SEE IF COLOR PREMIUM                         
         BE    CS12E                                                            
         MVC   0(5,RF),=C'PREM='                                                
         MVC   5(11,RF),PBYOPRM                                                 
*                                                                               
CS12E    DS    0H                                                               
         CLC   P+48(17),SPACES     SEE IF I NEED TO PRINT P NOW                 
         BE    *+8                 NO                                           
         BAS   RE,CSPRT                                                         
*                                                                               
CS13A    CLI   PBYOBFD,C' '                                                     
         BE    CS13B                                                            
         MVC   P+48(27),PBYOBFD                                                 
         BAS   RE,CSPRT                                                         
*                                                                               
CS13B    DS    0H                                                               
* CHECK FOR COMMENTS                                                            
         LA    R2,PBYOCOMS                                                      
         LA    R3,5                                                             
CS13D    DS    0H                                                               
         CLI   0(R2),C' '                                                       
         BNH   CS14                                                             
*                                                                               
         CLI   PROGPROA+9,0        REMOVE CHARACTER FROM COMMENT ?              
         BNH   CS13J               NO                                           
         CLI   PROGPROA+9,C'N'     REMOVE CHARACTER FROM COMMENT ?              
         BE    CS13J               NO                                           
         CLC   0(1,R2),PROGPROA+9  COMMENT STARTING WITH CHARACTER ?            
         BNE   CS13J               NO - "NORMAL" PRINT                          
         GOTO1 ACHOP,DMCB,(46,1(R2)),(34,P+48),(C'P',2)                         
         B     CS13P                                                            
CS13J    DS    0H                                                               
         GOTO1 ACHOP,DMCB,(47,0(R2)),(34,P+48),(C'P',2)                         
*                                                                               
CS13P    DS    0H                                                               
         BAS   RE,CSPRT                                                         
         LA    R2,47(R2)                                                        
         BCT   R3,CS13D                                                         
*                                                                               
*                                                                               
CS14     DS    0H                                                               
         CLI   QOPT6,C' '                                                       
         BE    CS15                                                             
*                                  JOB NO.                                      
         OC    PBDJOB,PBDJOB                                                    
         BZ    CS15                                                             
         LA    R4,P+48                                                          
*                                                                               
         CLI   QOPT6,C'A'                                                       
         BE    CS14B                                                            
         MVC   WORK,KEY                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVC   KEY+10(6),PBDJOB                                                 
         MVI   KEY+3,X'15'                                                      
         BAS   RE,CSHIGH                                                        
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,PJOBREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,CSGET                                                         
*                                  RESTORE SEQ READ                             
         MVC   KEY(64),WORK                                                     
         BAS   RE,CSHIGH                                                        
CS14B    DS    0H                                                               
         TM    QOPT6,X'30'                                                      
         BNZ   CS14D                                                            
         MVC   0(8,R4),=C'AD NO. ='                                             
         MVC   9(6,R4),PBDJOB                                                   
         B     CS14F                                                            
CS14D    DS    0H                                                               
         MVC   0(6,R4),=C'COPY ='                                               
         MVC   7(17,R4),PJOBCPY                                                 
CS14F    DS    0H                                                               
         BAS   RE,CSPRT                                                         
         TM    QOPT6,X'02'                                                      
         BZ    CS14H                                                            
         MVC   0(25,R4),PJOBCAP1                                                
         MVC   132(25,R4),PJOBCAP2                                              
         MVI   SPACING,2                                                        
         BAS   RE,CSPRT                                                         
*                                                                               
CS14H    DS    0H                                                               
CS15     DS    0H                DISPLAY INTERNET SITE                          
*                                WAS CHECKING FOR JUST MEDIA I                  
*                                MEDIA L MAY ALSO HAVE                          
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'98'                                                    
         BAS   RE,CSNEXTEL                                                      
         BNE   CS15X                                                            
         USING PISITEEL,R2                                                      
         MVC   P+48(5),=C'SITE='                                                
         MVC   P+53(L'PISITE),PISITE                                            
         DROP  R2                                                               
CS15X    DS    0H                                                               
* ADD TO ACCUMULATORS                                                           
*                                                                               
CS16     DS    0H                                                               
         MVC   ESAVKEY(64),KEY     SAVE CURRENT KEY                             
*                                  GET ADDITIONAL CHARGE DATA                   
         GOTO1 APPGETCG,DMCB,(C'T',PBUYREC),DATAMGR,GETINS,PBUYKPRD             
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),X'00'         ANY ADD'L. CHARGES FOUND ?                   
         BE    CCLUPX              NO                                           
*                                                                               
         ZIC   R3,0(R1)         FOR LUP COUNTER - NO OF LINES IN BLOCK          
*                                                                               
         ICM   RE,15,0(R1)         RE POINTING TO DATA BLOCK                    
         BNZ   *+6                                                              
         DC    H'0'                ADDRESS IS NULLS                             
*                                                                               
         LA    RF,CADCGWRK        ADDRESS OF AREA FOR ADD'L. CHGS.              
         LA    R1,ADCDLNTH         LENGTH OF BLOCK                              
         MOVE  ((RF),(R1)),(RE)    MOVE BLOCK TO ADCGWRK AREA IN W/S            
*                                                                               
         LA    R4,CADCGWRK        WORK AREA FOR ADD'L. CHGS.                    
         USING ADDCHGD,R4                                                       
*                                                                               
         MVC   P+38(24),=C'** ADDITIONAL CHARGES **'                            
         GOTO1 CSPRT                                                            
*                                                                               
CCLUP    MVC   P+38(ADCLNLEN),ADCCOD1                                           
         GOTO1 CSPRT                                                            
         LA    R4,ADCLNLEN(R4)     BUMP TO NEXT BLOCK "LINE"                    
         BCT   R3,CCLUP                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVC   KEY(64),ESAVKEY                                                  
         BAS   RE,CSHIGH                                                        
*                                                                               
CCLUPX   DS    0H                                                               
         CLC   P,SPACES             SEE IF ANYTHING LEFT TO PRINT               
         BE    *+8                 NO                                           
         BAS   RE,CSPRT                                                         
         TM    PBUYCNTL,X'80'                                                   
         BNZ   CS6                                                              
         LA    R3,PRDTOTS+0*ACLEQ  USE BEFORE CONV ACCUMULATORS                 
         CLC   PBUYKDAT,SCONVDTE   SEE IF BEFORE COL CONV DTE                   
         BL    CS16A                                                            
         LA    R3,PRDTOTS+3*ACLEQ  USE AFTER CONV ACCUMULATORS                  
CS16A    CLI   PBDSPACE,C'*'                                                    
         BE    CS17                                                             
         CLI   PBYOSPC,C'*'       NOT 'REAL' INSERTION                          
         BE    CS17                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R0,UNITS                                                         
         CLI   ININD,1             SEE IF DOING INCHES                          
         BE    CS16F                                                            
         CLI   PBDUIND,C'I'                                                     
         BE    CS16C                                                            
         CLI   PBDUIND,X'89'       LOWER CASE I - UNITS 2 DECIMALS              
         BNE   CS16X                                                            
CS16C    MHI   R0,14               14 LINES/INCH                                
         CLI   PBDUIND,X'89'       LOWER CASE I - UNITS TO 2 DECIMALS           
         BNE   CS16X                                                            
         CVD   R0,DUB                                                           
         AP    DUB,=P'50'          MUST ROUND TO NEAREST LINE                   
         DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R0,DUB                                                           
         B     CS16X                                                            
*                                                                               
CS16F    CLI   PBDUIND,X'89'       LOWER CASE I - ALREADY 2 DECIMALS            
         BE    CS16X                                                            
         CLI   PBDUIND,C'I'                                                     
         BNE   CS16G                                                            
         MHI   R0,100                                                           
         B     CS16X                                                            
*                                                                               
CS16G    DS    0H                                                               
         LR    R1,R0                                                            
         SR    R0,R0                                                            
         MHI   R1,100                                                           
         D     R0,=F'14'           14 LINES/INCF                                
         LR    R0,R1                                                            
CS16X    DS    0H                                                               
         CVD   R0,DOUBLE                                                        
         AP    0*ACLEQ(ACLEQ,R3),DOUBLE      CONV UNITS                         
         AP    2*ACLEQ(ACLEQ,R3),=P'1'       INSERTION COUNTER                  
*                                                                               
CS17     DS    0H                                                               
         L     R6,ACONIO1          POINT R6 "BACK" TO A(PCONREC)                
         USING PCONREC,R6                                                       
*                                                                               
         L     RE,GROSS                                                         
         CVD   RE,DOUBLE                                                        
         AP    1*ACLEQ(ACLEQ,R3),DOUBLE      DOLLARS                            
*                                                                               
CS6AA    GOTO1 =V(PP12468),DMCB,PBUYREC   SEE IF ANY POSITION RECS  L03         
         L     R3,0(R1)     RETURN HAS ADD OF TABLE                 L03         
         CLI   0(R3),255                                            L03         
         BE    CS6                                                  L03         
PPLOOP   MVC   P+33(35),0(R3)                                       L03         
         BAS   RE,CSPRT                                             L03         
         LA    R3,35(R3)                                            L03         
         CLI   0(R3),255                                            L03         
         BNE   PPLOOP                                               L03         
         BAS   RE,CSPRT                                             L03         
*                                                                   L03         
         B     CS6                                                              
*                                                                   L02         
*    RF POINTS TO OPTION                                            L02         
*           IF PROGRAM IS 12 AND OPTION IS YES WILL RETURN 0(RE)    L02         
*           OTHERWISE 4(RE)                                         L02         
*                                                                   L02         
OPTIONCB CLC   QPROG,=C'13'       CONTRACT TURNAROUND              L14          
         BE    OPTNCB5                                             L14          
         CLC   QPROG,=C'12'                                         L02         
         BNE   4(RE)                                                L02         
OPTNCB5  CLI   0(RF),C'N'                                           L02         
         BE    4(RE)                                                L02         
         BR    RE                                                   L02         
*                                                                   L02         
*                                                                   L06         
*                                                                   L06         
CKPRODC  CLC   QPROG,=C'12'                                         L06         
         LA    RF,PROFOPTS+6     12 PRODUCT OPTION                  L06         
         BE    CKPROCC                                              L06         
         CLC   QPROG,=C'13'      OR TURNAROUND                      L14         
         BE    CKPROCC                                              L14         
         CLC   QPROG,=C'15'                                         L06         
         LA    RF,PROFOPTS+11    15 PRODUCT OPTION                  L06         
         BNE   8(RE)         NORMAL PROCESSING                      L06         
CKPROCC  CLI   0(RF),C'N'  NORMAL PROCESSING                        L06         
         BE    8(RE)                                                L06         
         CLI   0(RF),C'Y'  PRINT PRODUCT CODE ONLY                  L06         
         BER   RE                                                   L06         
         CLI   0(RF),C'B'  DO NOT PRINT CODE OR DESCRIPTION         L06         
         BE    4(RE)                                                L06         
         B     8(RE)                                                L06         
*                                                                   L06         
*                                                                   L06         
*                                                                   L06         
         EJECT                                                                  
CSENDPRD DS    0H                                                               
         LA    RE,PRDTOTS          POINT TO PRD TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTAL             
         CP    0(ACLEQ,RE),=P'0'                                                
         BNE   *+16                                                             
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-14                                                          
         B     CSPRDX              ALL ACCUMULATORS ARE ZERO                    
*                                                                               
* ROLL TO CLIENT TOTALS                                                         
*                                                                               
         AP    CLTTOTS+0*ACLEQ(ACLEQ),PRDTOTS+0*ACLEQ(ACLEQ)                    
         AP    CLTTOTS+1*ACLEQ(ACLEQ),PRDTOTS+1*ACLEQ(ACLEQ)                    
         AP    CLTTOTS+2*ACLEQ(ACLEQ),PRDTOTS+2*ACLEQ(ACLEQ)                    
*                                                                               
         AP    CLTTOTS+3*ACLEQ(ACLEQ),PRDTOTS+3*ACLEQ(ACLEQ)                    
         AP    CLTTOTS+4*ACLEQ(ACLEQ),PRDTOTS+4*ACLEQ(ACLEQ)                    
         AP    CLTTOTS+5*ACLEQ(ACLEQ),PRDTOTS+5*ACLEQ(ACLEQ)                    
*                                                                               
         CLI   QOPT4,C'G'          TEST GRID                                    
         BNE   CSPRD2                                                           
         GOTO1 AGRID,DMCB,=C'T'                                                 
*                                                                               
         L     R3,AGRTAB                                                        
         MVI   0(R3),0             CLEAR GRID TABLE                             
         BAS   RE,CKPRODC  CHECK PRODUCT OPTIONS                    L16         
         B     CSPRDX      PRINT JUST PROD CODE                     L16         
         B     CSPRDX      NOTHING TO PRINT                         L16         
CSPRD2   DS    0H                                                               
         BAS   RE,CSPRT            SKIP A LINE                                  
         MVC   P+5(18),=C'* PRODUCT TOTALS *'                                   
         LA    R0,PRDTOTS                                                       
         ST    R0,TOTADDR                                                       
         BAS   RE,CSTOT                                                         
*                                                                               
CSPRDX   DS    0H                                                               
         LA    RE,PRDTOTS          POINT TO PRD TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         MVI   PRDSW,0                                                          
*          DATA SET PPREP1402  AT LEVEL 011 AS OF 02/17/88                      
**NEW 2/18/88                                                                   
         CLI   ONEPRD,C'Y'         SEE IF DOING ONE PRD                         
         BE    CSENDCLT            YES - THEN DONE                              
**NEW 2/18/88                                                                   
CSENDP8  CLC   KEY(13),KEYSAVE     TEST SAME A/M/X/CLT/PUB                      
         BE    CS7                 YES - PROCESS THIS KEY                       
         SPACE 2                                                                
*                                                                               
* CLIENT TOTALS                                                                 
*                                                                               
CSENDCLT LA    RE,CLTTOTS          POINT TO CLT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTAL             
         CP    0(ACLEQ,RE),=P'0'                                                
         BNE   *+16                                                             
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-14                                                          
         B     CSENDC2             ALL ACCUMULATORS ARE ZERO                    
*                                                                               
         MVC   P+5(17),=C'* CLIENT TOTALS *'                                    
         LA    R0,CLTTOTS                                                       
         ST    R0,TOTADDR                                                       
         BAS   RE,CSTOT                                                         
*                                                                               
CSENDC2  CLI   QEDITION,C'Z'       SEE IF DOING ALL EDITIONS                    
         BNE   CSENDC3                                                          
         AP    EDTTOTS+0*ACLEQ(ACLEQ),CLTTOTS+0*ACLEQ(ACLEQ)                    
         AP    EDTTOTS+1*ACLEQ(ACLEQ),CLTTOTS+1*ACLEQ(ACLEQ)                    
         AP    EDTTOTS+2*ACLEQ(ACLEQ),CLTTOTS+2*ACLEQ(ACLEQ)                    
*                                                                               
         AP    EDTTOTS+3*ACLEQ(ACLEQ),CLTTOTS+3*ACLEQ(ACLEQ)                    
         AP    EDTTOTS+4*ACLEQ(ACLEQ),CLTTOTS+4*ACLEQ(ACLEQ)                    
         AP    EDTTOTS+5*ACLEQ(ACLEQ),CLTTOTS+5*ACLEQ(ACLEQ)                    
*                                                                               
CSENDC3  CLI   PCLTPROF+5,C'1'     TEST MASTER CLIENT                           
         BNE   CSENDCX                                                          
*                                                                               
* ROLL TO CONTRACT TOTALS                                                       
*                                                                               
         AP    MASTOTS+0*ACLEQ(ACLEQ),CLTTOTS+0*ACLEQ(ACLEQ)                    
         AP    MASTOTS+1*ACLEQ(ACLEQ),CLTTOTS+1*ACLEQ(ACLEQ)                    
         AP    MASTOTS+2*ACLEQ(ACLEQ),CLTTOTS+2*ACLEQ(ACLEQ)                    
*                                                                               
         AP    MASTOTS+3*ACLEQ(ACLEQ),CLTTOTS+3*ACLEQ(ACLEQ)                    
         AP    MASTOTS+4*ACLEQ(ACLEQ),CLTTOTS+4*ACLEQ(ACLEQ)                    
         AP    MASTOTS+5*ACLEQ(ACLEQ),CLTTOTS+5*ACLEQ(ACLEQ)                    
*                                                                               
         LA    RE,CLTTOTS          POINT TO CLT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         MVI   CLTSW,0                                                          
         B     CSCLT2              READ NEXT CLIENT                             
CSENDCX  CLI   QEDITION,C'Z'       SEE IF DOING ALL EDITIONS                    
         BNE   CSENDCZ                                                          
*                                                                               
         LA    RE,EDTTOTS          POINT TO EDT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTAL             
         CP    0(ACLEQ,RE),=P'0'                                                
         BNE   *+16                                                             
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-14                                                          
         B     CSENDCX1            ALL ACCUMULATORS ARE ZERO                    
*                                                                               
         GOTO1 ACSPUB                                                           
         CLC   W,SPACES                                                         
         BE    CSENDCX0                                                         
         MVI   LNEED,3                                                          
         MVC   P+5(30),W                                                        
         MVC   PSECOND+5(30),W+30                                               
         BAS   RE,CSPRT                                                         
CSENDCX0 DS    0H                                                               
         MVC   P+5(18),=C'* EDITION TOTALS *'                                   
         CLI   QMEDIA,C'O'                                                      
         BNE   *+10                                                             
         MVC   P+5(9),=C' * MARKET'                                             
         LA    R0,EDTTOTS                                                       
         ST    R0,TOTADDR                                                       
         BAS   RE,CSTOT                                                         
*                                                                               
         AP    CONTOTS+0*ACLEQ(ACLEQ),EDTTOTS+0*ACLEQ(ACLEQ)                    
         AP    CONTOTS+1*ACLEQ(ACLEQ),EDTTOTS+1*ACLEQ(ACLEQ)                    
         AP    CONTOTS+2*ACLEQ(ACLEQ),EDTTOTS+2*ACLEQ(ACLEQ)                    
*                                                                               
         AP    CONTOTS+3*ACLEQ(ACLEQ),EDTTOTS+3*ACLEQ(ACLEQ)                    
         AP    CONTOTS+4*ACLEQ(ACLEQ),EDTTOTS+4*ACLEQ(ACLEQ)                    
         AP    CONTOTS+5*ACLEQ(ACLEQ),EDTTOTS+5*ACLEQ(ACLEQ)                    
*                                                                               
         AP    TOTEDS,=P'1'                                                     
CSENDCX1 DS    0H                                                               
*                       NOW ALL ALL EDTION REQS USE EDTION TABLE                
**                      USED TO USE ONLY FOR MASTER CLIENT REQUESTS             
CSENDCX2 L     R4,ANXTEDT                                                       
         CLC   0(2,R4),=X'FFFF'    TEST END OF TABLE                            
         BE    CSENDCON                                                         
         CLC   PCONKPUB+4(2),0(R4)  SEE IF I HAVE JUST DONE THIS EDT            
         BNE   CSENDCX3                                                         
         LA    R4,2(R4)                                                         
         ST    R4,ANXTEDT                                                       
         B     CSENDCX2                                                         
*                                                                               
CSENDCX3 MVC   PCONKPUB+4(2),0(R4)                                              
         LA    R4,2(R4)                                                         
         ST    R4,ANXTEDT                                                       
         B     CSENDCW                                                          
CSENDCX4 CLC   KEY(11),KEYSAVE       CHECK SAME 8 DIGITS                        
         BNE   CSENDCON         NO - DONE                                       
         CLC   QZONE,=C'ZZ'         SEE IF DOING ALL ZONES                      
         BE    CSENDCY                                                          
         CLC   KEY(12),KEYSAVE                                                  
         BNE   CSENDCON                                                         
CSENDCY  MVC   PCONKPUB(6),KEY+7                                                
*                                                                               
CSENDCW  LA    RE,EDTTOTS          POINT TO EDT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         MVI   PUBSW,0                                                          
         B     CONSCH2                                                          
*                                                                               
CSENDCON CP    TOTEDS,=P'1'         ONLY ONE EDITION                            
         BNH   CSENDCZ                                                          
         MVC   P+5(19),=C'* CONTRACT TOTALS *'                                  
         LA    R0,CONTOTS                                                       
         ST    R0,TOTADDR                                                       
         BAS   RE,CSTOT                                                         
         B     CSENDCZ                                                          
*                                                                               
CSENDCZ  DS    0H                                                               
         LA    RE,CLTTOTS          POINT TO CLT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,EDTTOTS          POINT TO EDT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,CONTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         ZAP   TOTEDS,=P'0'                                                     
         CLI   QOPT5,C'C'                                                       
         BNE   CSENDCZZ                                                         
         CLI   SCONREV,0                                                        
         BE    CSENDCZZ                                                         
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         CLI   ACTSW,0                                                          
         BE    CSENDCZ2                                                         
         MVC   P+05(43),=C'**NOTE - ONLY SCHEDULE REVISIONS SINCE LAST'         
         MVC   PSECOND+14(29),=C'ISSUING OF CONTRACT ARE SHOWN'                 
         B     CSENDCZ4                                                         
CSENDCZ2 DS    0H                                                               
         MVC   P+05(56),=C'**NO SCHEDULE REVISIONS SINCE LAST ISSUING O'        
               F CONTRACT**'                                                    
CSENDCZ4 DS    0H                                                               
         BAS   RE,CSPRT                                                         
CSENDCZZ DS    0H                                                               
         XMOD1 1                                                                
         SPACE 2                                                                
CSENDMAS CLI   QEDITION,C'Z'      SEE IF DOING ALL EDITIONS                     
         BE    CSENDMX                                                          
*                                                                               
         LA    RE,MASTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTAL             
         CP    0(ACLEQ,RE),=P'0'                                                
         BNE   *+16                                                             
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-14                                                          
         B     CSENDMX             ALL ACCUMULATORS ARE ZERO                    
*                                                                               
         MVC   P+5(19),=C'* CONTRACT TOTALS *'                                  
         LA    R0,MASTOTS                                                       
         ST    R0,TOTADDR                                                       
         BAS   RE,CSTOT                                                         
*                                                                               
CSENDMX  LA    RE,MASTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         B     CSENDCX                                                          
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
CSFLOAT  OI    0(R2),C' '                                                       
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,CSFLOAT                                                       
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SUBROUTINE TO FORMAT PRD/CLT TOTALS (TOTADDR POINTS TO ACCUMS)                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CSTOT    NTR1                                                                   
         L     R5,TOTADDR                                                       
         LA    R3,P+30                                                          
         CLI   QOPT4,C'G'                    TEST GRID                          
         BE    CSTOT3A                                                          
         CLI   QMEDIA,C'N'                                                      
         BNE   CSTOT2                                                           
         CLI   SCONVDTE,X'FF'                CHK FOR COL CONV                   
         BE    CSTOTC                        NO                                 
         MVI   LNEED,3                       NEED 3 LINES                       
         CLI   SCONVIND,C'-'                 SEE IF DOING NEW TO OLD            
         BE    CSTOTB                                                           
         ZAP   DUB,3*ACLEQ(ACLEQ,R5)         MUST BE OLD TO NEW                 
         ZAP   DOUBLE,0*ACLEQ(ACLEQ,R5)                                         
         B     CSTOTB5                                                          
*                                                                               
CSTOTB   ZAP   DUB,0*ACLEQ(ACLEQ,R5)                                            
         ZAP   DOUBLE,3*ACLEQ(ACLEQ,R5)                                         
CSTOTB5  SR    R2,R2                                                            
         M     R2,SCONVFAC                                                      
         D     R2,=F'1000'                                                      
         C     R2,=F'500'                                                       
         BL    *+10                                                             
         AP    DOUBLE,=P'1'                  ROUND                              
         AP    DUB,DOUBLE                                                       
         B     CSTOTD                                                           
*                                                                               
CSTOTC   ZAP   DUB,0*ACLEQ(ACLEQ,R5)         UNITS FOR NEWSPAPERS ONLY          
CSTOTD   MVC   P+51(5),=C'LINES'                                                
*                                                                               
         CLI   ININD,0                       SEE IF DOING LINES                 
         BE    CSTOT1                                                           
         MVC   P+51(6),=C'INCHES'                                               
         EDIT  (P8,DUB),(14,P+36),2,COMMAS=YES                                  
         B     CSTOT2                                                           
CSTOT1   DS    0H                                                               
         EDIT  (P8,DUB),(11,P+39),COMMAS=YES                                    
*                                                                               
CSTOT2   DS    0H                                                               
         CLI   QMEDIA,C'O'                   OUTDOOR - NO INSERT COUNT          
         BE    CSTOT3                                                           
         ZAP   DOUBLE,2*ACLEQ(ACLEQ,R5)      INSERTIONS                         
         AP    DOUBLE,5*ACLEQ(ACLEQ,R5)      ADD POST CONV INS                  
         EDIT  (P8,DOUBLE),(4,P+25)                                             
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    DOUBLE,=P'1'                                                     
         BH    *+8                                                              
         MVI   P+39,C' '                     SINGULAR, NO "S"                   
*                                                                               
CSTOT3   DS    0H                                                               
         LA    R3,P+66-3                                                        
CSTOT3A  DS    0H                                                               
         CLI   QOPT2,C'X'                    SEE IF SUPPRESSING COSTS           
         BE    CSTOT3B                                                          
*                                                                               
         ZAP   DUB,1*ACLEQ(ACLEQ,R5)                                            
         AP    DUB,4*ACLEQ(ACLEQ,R5)         ADD POST CONV $                    
         EDIT  (P8,DUB),(15,0(R3)),2,COMMAS=YES,FLOAT=-                         
         MVI   15(R3),C'*'                                                      
*                                                                               
CSTOT3B  DS    0H                                                               
         CLI   SCONVDTE,X'FF'                CHK FOR COL CONV                   
         BE    CSTOTX                                                           
         MVC   P+51(6),SPACES                                                   
         MVC   P+51(3),=C'OLD'                                                  
         MVC   P+55(5),=C'LINES'                                                
         CLI   ININD,0                                                          
         BE    *+10                                                             
         MVC   P+55(6),=C'INCHES'                                               
         CLI   SCONVIND,C'-'                 SEE IF CONV NEW TO OLD             
         BE    CSTOT3X                                                          
         MVC   P+51(3),=C'NEW'               MUST BE OLD TO NEW                 
CSTOT3X  BAS   RE,CSPRT                                                         
         MVC   P+12(12),=C'BEFORE CONV.'                                        
         CLI   QMEDIA,C'N'                                                      
         BNE   CSTOT4                                                           
         ZAP   DUB,0*ACLEQ(ACLEQ,R5)                                            
         CLI   ININD,0                                                          
         BE    CSTOT3X5                                                         
         EDIT  (P8,DUB),(14,P+36),2,COMMAS=YES                                  
         B     CSTOTX8                                                          
CSTOT3X5 EDIT  (P8,DUB),(11,P+39),COMMAS=YES                                    
CSTOTX8  MVC   P+51(3),=C'OLD'                                                  
         MVC   P+55(5),=C'LINES'                                                
         CLI   ININD,0                                                          
         BE    *+10                                                             
         MVC   P+55(6),=C'INCHES'                                               
CSTOT4   CLI   QMEDIA,C'O'                   OUTDOOR - NO INSERT COUNT          
         BE    CSTOT5                                                           
         ZAP   DUB,2*ACLEQ(ACLEQ,R5)         INSERTIONS                         
         EDIT  (P8,DUB),(4,P+25)                                                
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    DUB,=P'1'                                                        
         BH    *+8                                                              
         MVI   P+39,C' '                     SINGULAR, NO "S"                   
*                                                                               
CSTOT5   DS    0H                                                               
         CLI   QOPT2,C'X'                    SEE IF SUPPRESSING COSTS           
         BE    CSTOT5C                                                          
         LA    R3,P+66-3                                                        
         ZAP   DUB,1*ACLEQ(ACLEQ,R5)                                            
         EDIT  (P8,DUB),(15,0(R3)),2,COMMAS=YES,FLOAT=-                         
         MVI   15(R3),C'*'                                                      
*                                                                               
CSTOT5C  DS    0H                                                               
         BAS   RE,CSPRT                                                         
         MVC   P+13(11),=C'AFTER CONV.'                                         
         GOTO1 DATCON,DMCB,(3,SCONVDTE),(5,P+4)                                 
         CLI   QMEDIA,C'N'                                                      
         BNE   CSTOT6                                                           
         ZAP   DUB,3*ACLEQ(ACLEQ,R5)                                            
         CLI   ININD,0                                                          
         BE    CSTOT5X                                                          
         EDIT  (P8,DUB),(14,P+36),2,COMMAS=YES                                  
         B     CSTOT5XX                                                         
CSTOT5X  EDIT  (P8,DUB),(11,P+39),COMMAS=YES                                    
CSTOT5XX MVC   P+51(3),=C'NEW'                                                  
         MVC   P+55(5),=C'LINES'                                                
         CLI   ININD,0                                                          
         BE    *+10                                                             
         MVC   P+55(6),=C'INCHES'                                               
CSTOT6   CLI   QMEDIA,C'O'                   OUTDOOR - NO INSERT COUNT          
         BE    CSTOT7                                                           
         ZAP   DUB,5*ACLEQ(ACLEQ,R5)         INSERTIONS                         
         EDIT  (P8,DUB),(4,P+25)                                                
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    DUB,=P'1'                                                        
         BH    *+8                                                              
         MVI   P+39,C' '                     SINGULAR, NO "S"                   
*                                                                               
CSTOT7   DS    0H                                                               
         CLI   QOPT2,C'X'                    SEE IF SUPPRESSING COSTS           
         BE    CSTOTX                                                           
         LA    R3,P+66-3                                                        
         ZAP   DUB,4*ACLEQ(ACLEQ,R5)                                            
         EDIT  (P8,DUB),(15,0(R3)),2,COMMAS=YES,FLOAT=-                         
         MVI   15(R3),C'*'                                                      
*                                                                               
CSTOTX   MVI   SPACING,2                     SKIP LINE AFTER                    
*                                                                               
         BAS   RE,CSPRT                                                         
*                                                                               
         XIT1                                                                   
*                                                                               
*****                        DS    F                                            
*****               CSPRT    ST    RE,*-4                                       
*****                        GOTO1 ACONSPRT                                     
*****                        L     RE,CSPRT-4                                   
*****                        BR    RE                                           
*                                                                               
CSPRT    NTR1                       CHANGE IS TO NOT USE                        
         GOTO1 ACONSPRT             STORAGE IN A MIDDLE OF PROGRAM              
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
CSHIGH   MVC   KEYSAVE,KEY                                                      
         LA    R0,DMRDHI                                                        
         B     *+8                                                              
CSSEQ    LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         NTR1                                                                   
         SPACE 2                                                                
CSSEQ2   DS    0H                                                               
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
         CLI   KEY+25,X'FF'                                                     
         BNE   CSDMX                                                            
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         B     CSSEQ2                                                           
         B     CSDMX                                                            
CSPUT    NTR1                                                                   
         SPACE 2                                                                
         GOTO1 DATAMGR,DMCB,(DMINBTS,PUTREC),PRTFILE,KEY+27,IOAREA,    X        
               DMWORK                                                           
         B     CSDMX                                                            
*                                                                               
CSGET    NTR1                                                                   
         SPACE 2                                                                
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PRTFILE,KEY+27,IOAREA,    X        
               DMWORK                                                           
*                                                                               
CSDMX    DS    0H                                                               
         MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,DMCB+8                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
CSNEXTEL SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    CSNXTELX                                                         
         CLC   ELCODE1,0(R2)                                                    
         BCR   8,RE                                                             
         B     CSNEXTEL                                                         
CSNXTELX LTR   R2,R2                                                            
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
ESAVKEY  DS    CL64                                                             
*                                                                               
CSHN     DC    C'PRODUCT                        EST INS DATE    SPACE  X        
                    RATE  '                                                     
CSHNUND  DC    C'-------                        --- --------    -----  X        
                  --------'                                                     
CSHM     DC    C'PRODUCT                        EST INS DATE    SPACE DX        
               ESCRIPTION '                                                     
CSHMUND  DC    C'-------                        --- --------    -------X        
               ---------- '                                                     
CSHO     DC    C'                                    POSTING    SIZE  -X        
               DISPLAYS-  '                                                     
CSHO2    DC    C'PRODUCT                        EST   DATE      SHOW  RX        
               EG  ILLUM  '                                                     
CSHOUND  DC    C'-------                        --- --------    ----  -X        
               --  -----  '                                                     
*                                                                               
GRSWDS   DC    C'            '                                                  
         DC    C'  GROSS COST'                                                  
         DC    C'  ----------'                                                  
NETWDS   DC    C'            '                                                  
         DC    C'    NET COST'                                                  
         DC    C'    --------'                                                  
*                                                                               
CADCGWRK DS    CL(ADCDLNTH)                                                     
*                                                                               
         EJECT                                                                  
CONSPRT  CSECT                                                                  
         NMOD1 0,CONSPRT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP12WRKD,R8                                                      
         CLI   FORCEHED,C'P'                                                    
         BE    CSPRT2                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,LNEED                                                         
         LA    RE,1(RF,RE)                                                      
         STC   RE,BYTE                                                          
         MVI   LNEED,0                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    CSPRT4                                                           
CSPRT2   EQU   *                                                                
         GOTO1 ACONEOP                                                          
*                                                                               
         EDIT  (B2,PAGE),(2,MH5+74)                                             
*                                                                               
*                                  SO I WON'T REDO HEADLINES                    
         LA    R0,14                                                            
         LA    R1,HEAD1                                                         
         LA    R2,MH1                                                           
         MVC   0(080,R1),0(R2)                                                  
         LA    R1,132(R1)                                                       
         LA    R2,080(R2)                                                       
         BCT   R0,*-14                                                          
         CLC   P+33(14),=C'** SCHEDULE **'                                      
         BE    CSPRT4              IF SCHEDULE STARTS ON NEW PAGE               
*                                                                               
         LA    R1,HEAD14                                                        
         MVC   33(14,R1),=C'** SCHEDULE **'                                     
         MVI   RCSUBPRG,0                                                       
         MVC   SAVEP,P                                                          
         MVC   SAVEP2,PSECOND                                                   
         MVC   SAVESPAC,SPACING                                                 
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVI   SPACING,1                                                        
*                                  FIRST TIME HEADLINE                          
*                                  SET HEADLINES FOR NO EDIT TOTALS             
         MVC   CSHNP(15),=CL15'PRODUCT'                                         
         MVC   CSHMP(15),=CL15'PRODUCT'                                         
         MVC   CSHO2P(15),=CL15'PRODUCT'                                        
         MVC   CSHNUNDP(15),=CL15'-------'                                      
         MVC   CSHMUNDP(15),=CL15'-------'                                      
         MVC   CSHOP+47(17),=C'SIZE  -DISPLAYS- '                 L02           
         MVC   CSHO2P+47(5),=C'SHOW '                             L02           
         MVC   CSHOUNDP(15),=CL15'-------'                                      
         CLI   QEDITION,C'Z'                                                    
         BNE   CSPRT2C                                                          
*                                  FIX HEADLINES FOR EDIT TOTALS                
         MVC   CSHNP(15),=C'EDITION/PRODUCT'                                    
         MVC   CSHMP(15),=C'EDITION/PRODUCT'                                    
         MVC   CSHO2P(14),=C'MARKET/PRODUCT'                                    
         MVC   CSHNUNDP(15),DASHES1                                             
         MVC   CSHMUNDP(15),DASHES1                                             
         MVC   CSHOUNDP(14),DASHES1                                             
*                                                                               
CSPRT2C  DS    0H                                                               
         LA    R5,SPACES                                                        
         CLI   QOPT2,C'X'        SEE IF SUPPRESSING COSTS                       
         BE    CSPRT2D                                                          
         LA    R5,GRSWDSP                                                       
         CLI   QOPT2,C'N'                                                       
         BNE   *+8                                                              
         LA    R5,NETWDSP                                                       
*                                                                               
CSPRT2D  DS    0H                                                               
         LA    RF,PROFOPTS+7       OVERRIDE WITH GRP                L02         
         BAS   RE,OPTIONCZ                                          L02         
         B     *+8                 OVERRIDE                         L02         
         B     *+16          NO    OVERRIDE                         L02         
         MVC   CSHOP+47(5),=C'GRP  '                             L02            
         MVC   CSHO2P+47(6),=C'LEVEL '                            L02           
*                                                                   L02         
         LA    RF,PROFOPTS+8       USE MEDIA ALLOCATION             L02         
         BAS   RE,OPTIONCZ                                          L02         
         B     *+8                 OVERRIDE                         L02         
         B     *+10          NO    OVERRIDE                         L02         
         MVC   CSHOP+53(11),=C'MEDIA ALLOC'                      L02            
*                                                                   L02         
*                                                                               
         LA    R2,CSHOP                                                         
         LA    R3,3                                                             
         CLI   QMEDIA,C'O'                                                      
         BE    CSPRT3                                                           
         LA    R5,12(R5)                                                        
         LA    R2,CSHNP                                                         
         LA    R3,2                                                             
         CLI   QMEDIA,C'N'                                                      
         BE    CSPRT3                                                           
         LA    R2,CSHMP                                                         
         LA    R3,2                                                             
CSPRT3   DS    0H                                                               
         MVC   P+66(12),0(R5)                                                   
         MVC   P+1(L'CSHNP),0(R2)                                               
*                                                                               
         JIF   QOPT2,NE,C'X',OR,QMEDIA,NE,C'N',CSPRT3C,JUMP=N                   
*                            IF NEWSPAPERS AND SUPPRESSING COST                 
*                            BLANK-OUT 'RATE' AND ITS UNDERLINING               
*                                                                               
         MVC   P+58(8),SPACES                                                   
CSPRT3C  DS    0H                                                               
         GOTO1 AFREPORT                                                         
*                                                                               
         LA    R2,L'CSHNP(R2)                                                   
         LA    R5,12(R5)                                                        
         BCT   R3,CSPRT3                                                        
         GOTO1 AFREPORT                                                         
*                                                                               
         MVC   P,SAVEP                                                          
         MVC   PSECOND,SAVEP2                                                   
         CLC   P(25),SPACES                                                     
         BNE   CSPRT3E                                                          
         MVC   P+1(3),SAVPRD       REPRINT PRODUCT                              
         MVC   P+5(11),=C'(CONTINUED)'                                          
CSPRT3E  MVC   SPACING,SAVESPAC                                                 
CSPRT4   DS    0H                                                               
         GOTO1 AFREPORT                                                         
*                                                                               
         XMOD1 1                                                                
*                                                                               
CSHNP    DC    C'PRODUCT                        EST INS DATE    SPACE  X        
                    RATE  '                                                     
CSHNUNDP DC    C'-------                        --- --------    -----  X        
                  --------'                                                     
CSHMP    DC    C'PRODUCT                        EST INS DATE    SPACE DX        
               ESCRIPTION '                                                     
CSHMUNDP DC    C'-------                        --- --------    -------X        
               ---------- '                                                     
CSHOP    DC    C'                                    POSTING    SIZE  -X        
               DISPLAYS-  '                                                     
CSHO2P   DC    C'PRODUCT                        EST   DATE      SHOW  RX        
               EG  ILLUM  '                                                     
CSHOUNDP DC    C'-------                        --- --------    ----  -X        
               --  -----  '                                                     
*                                                                               
GRSWDSP  DC    C'            '                                                  
         DC    C'  GROSS COST'                                                  
         DC    C'  ----------'                                                  
NETWDSP  DC    C'            '                                                  
         DC    C'    NET COST'                                                  
         DC    C'    --------'                                                  
*                                                                               
*                                                                   L02         
*    RF POINTS TO OPTION                                            L02         
*    IF PROGRAM IS 12 (OR13) AND OPTION IS YES WILL RETURN 0(RE)    L02         
*           OTHERWISE 4(RE)                                         L02         
*                                                                   L02         
OPTIONCZ CLC   QPROG,=C'13'                CONTRACT TURNAROUND      L14         
         BE    OPTNCZ5                                              L14         
         CLC   QPROG,=C'12'                                         L02         
         BNE   4(RE)                                                L02         
OPTNCZ5  CLI   0(RF),C'N'                                           L02         
         BE    4(RE)                                                L02         
         BR    RE                                                   L02         
*                                                                   L02         
         LTORG                                                                  
         TITLE 'PP1202 - PRINT SORTED CONTRACT SCHEDULES'                       
SORTSCH  CSECT                                                                  
         NMOD1 0,SORTSCH                                                        
         USING PPWORKD,RA          RA=V(PPWORKC)                                
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9       R9=RC+4096                                   
         USING PP12WRKD,R8         R8=A(PP12WRKD)                               
         LA    R7,SORTSCH+4095                                                  
         LA    R7,1(R7)                                                         
         USING SORTSCH+4096,R7     ** NOTE USE OF SECOND BASE **                
*                                                                               
         L     R6,ACONIO1          A(PCONREC)                                   
         USING PCONREC,R6                                                       
*                                                                               
         MVI   DMINBTS,0                                                        
         MVI   DMOUTBTS,0                                                       
         MVI   LNEED,0                                                          
         CLI   QOPT5,C' '                                                       
         BE    SORTSH0                                                          
         CLI   SCONREV,0                                                        
         BE    SORTSH0                                                          
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
SORTSH0  DS    0H                                                               
         MVI   ACTSW,0             RESET ACTIVITY SW                            
         MVI   PUBSW,0                                                          
         MVI   PRDSW,0                                                          
         MVI   CLTSW,0                                                          
         MVI   ININD,0             LINES VS. INCH                               
         CLI   QOPT7,C'I'          INCHES                                       
         BE    SORTSH0D                                                         
         CLI   QOPT7,C'L'          LINES                                        
         BE    SORTSH0E                                                         
*                                                                               
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE1,X'20'                                                    
         BAS   RE,SRNEXTEL                                                      
         BNE   SORTSH0E                                                         
*                                  INCHES/LINES FROM FRST RATE ELEM             
         CLI   PRBLIND-PRBELEM(R2),C'I'                                         
         BNE   SORTSH0E                                                         
SORTSH0D MVI   ININD,1                                                          
SORTSH0E OC    REQPUB,REQPUB                                                    
         BZ    *+10                                                             
         MVC   PCONKPUB(6),REQPUB                                               
*                                                                               
         LA    RE,EDTTOTS          POINT TO EDT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,CONTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         ZAP   TOTEDS,=P'0'                                                     
         CLI   QEDITION,C'Z'       SEE IF DOING ALL EDTS                        
         BNE   SORTSH0X                                                         
****                                                                            
***      NOW ALL 'ALL EDITION' REQUEST USE EDTION TABLE                         
***      USED TO USE ONLY FOR MASTER CLIENT REQUESTS                            
*                                                                               
****                                                                            
*        NEED TO BUILD TABLE OF EDITIONS IF ALL EDTS AND MASTER CLT             
         GOTO1 AFNDEDTS                                                         
*                                  SET EDITION TO FIRST ONE IN TABLE            
         L     R4,ANXTEDT                                                       
         MVC   PCONKPUB+4(2),0(R4)                                              
*                                                                               
SORTSH0X DS    0H                                                               
*                                  FIRST TIME HEADLINE                          
*                                  SET HEADLINES FOR NO EDIT TOTALS             
*                                                                   L06         
         MVC   SRHN+18(7),=C'PRODUCT'                               L06         
         MVC   SRHM+18(7),=C'PRODUCT'                               L06         
         MVC   SRHO2+18(7),=C'PRODUCT'                              L06         
         MVC   SRHNUND+18(7),=7C'-'                                 L06         
         MVC   SRHMUND+18(7),=7C'-'                                 L06         
         MVC   SRHOUND+18(7),=7C'-'                                 L06         
*                                                                   L06         
         BAS   RE,CKPRODB  CHECK PRODUCT OPTIONS                    L06         
         B     JSPPC       PRINT JUST PROD CODE                     L06         
         B     *+8         NOTHING TO PRINT                         L06         
         B     JSPPC       NORMAL PROCESSING                        L06         
*                                                                   L06         
         MVC   SRHN+18(7),=C'       '                               L06         
         MVC   SRHM+18(7),=C'       '                               L06         
         MVC   SRHO2+18(7),=C'       '                              L06         
         MVC   SRHNUND+18(7),=7C' '                                 L06         
         MVC   SRHMUND+18(7),=7C' '                                 L06         
         MVC   SRHOUND+18(7),=7C' '                                 L06         
JSPPC    MVC   SRHN(6),SPACES                                                   
         MVC   SRHM(6),SPACES                                                   
         MVC   SRHO2(6),SPACES                                                  
         MVC   SRHO+47(17),=C'SIZE  -DISPLAYS- '                 L02            
         MVC   SRHO2+47(5),=C'SHOW '                             L02            
         MVC   SRHNUND(6),SPACES                                                
         MVC   SRHMUND(6),SPACES                                                
         MVC   SRHOUND(6),SPACES                                                
         CLI   QEDITION,C'Z'                                                    
         BNE   SORTSH1                                                          
*                                  FIX HEADLINES FOR EDIT TOTALS                
         MVC   SRHN(6),=C'EDITN/'                                               
         MVC   SRHM(6),=C'EDITN/'                                               
         MVC   SRHO2(6),=C'MARKT/'                                              
         MVC   SRHNUND(14),DASHES1                                              
         MVC   SRHMUND(14),DASHES1                                              
         MVC   SRHOUND(14),DASHES1                                              
*                                                                               
SORTSH1  DS    0H                                                               
         LA    RF,PROFOPTS+7       OVERRIDE WITH GRP                L02         
         BAS   RE,OPTIONCC                                          L02         
         B     *+8                 OVERRIDE                         L02         
         B     *+16          NO    OVERRIDE                         L02         
         MVC   SRHO+47(5),=C'GRP  '                             L02             
         MVC   SRHO2+47(6),=C'LEVEL '                            L02            
*                                                                   L02         
         LA    RF,PROFOPTS+8       USE MEDIA ALLOCATION             L02         
         BAS   RE,OPTIONCC                                          L02         
         B     *+8                 OVERRIDE                         L02         
         B     *+10          NO    OVERRIDE                         L02         
         MVC   SRHO+53(11),=C'MEDIA ALLOC'                      L02             
*                                                                   L02         
*                         SEE IF SKIPPING WILL CAUSE PAGE BREAK                 
*                         IF YES - DON'T SKIP                                   
         ZIC   RE,LINE                                                          
         AHI   RE,2                                                             
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BNL   SORTSH1A                                                         
         MVI   SPACING,2                                                        
         BAS   RE,SRPRT                                                         
*                                                                               
SORTSH1A DS    0H                                                               
***      CLI   LINE,20                                                          
***      BL    SORTSH2             JUST PRINTED ALL HEADLINES                   
         MVI   LNEED,10                                                         
         MVI   SPACING,2                                                        
         MVC   P+33(14),=C'** SCHEDULE **'                                      
         BAS   RE,SRPRT                                                         
*                                                                               
         LA    R5,SPACES                                                        
         CLI   QOPT2,C'X'        SEE IF SUPPRESSING COSTS                       
         BE    SORTSH1B                                                         
         LA    R5,SGRSWDS                                                       
         CLI   QOPT2,C'N'                                                       
         BNE   *+8                                                              
         LA    R5,SNETWDS                                                       
*                                                                               
SORTSH1B DS    0H                                                               
         LA    R2,SRHO                                                          
         LA    R3,3                                                             
         CLI   QMEDIA,C'O'                                                      
         BE    SORTSH1D                                                         
         LA    R5,12(R5)                                                        
         LA    R2,SRHN                                                          
         LA    R3,2                                                             
         CLI   QMEDIA,C'N'                                                      
         BE    SORTSH1D                                                         
         LA    R2,SRHM                                                          
         LA    R3,2                                                             
SORTSH1D DS    0H                                                               
         MVC   P+66(12),0(R5)                                                   
         MVC   P+1(L'SRHN),0(R2)                                                
*                                                                               
         JIF   QOPT2,NE,C'X',OR,QMEDIA,NE,C'N',SORTSH1F,JUMP=N                  
*                            IF NEWSPAPERS AND SUPPRESSING COST                 
*                            BLANK-OUT 'RATE' AND ITS UNDERLINING               
*                                                                               
         MVC   P+58(8),SPACES                                                   
SORTSH1F DS    0H                                                               
         BAS   RE,SRPRT                                                         
         LA    R2,L'SRHN(R2)                                                    
         LA    R5,12(R5)                                                        
         BCT   R3,SORTSH1D                                                      
         BAS   RE,SRPRT                                                         
SORTSH2  DS    0H                                                               
         LA    RE,PRDTOTS          POINT TO PRD TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,CLTTOTS          POINT TO CLT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,MASTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         MVC   WORKCLT,PCLTKEY                                                  
         CLI   PCLTPROF+5,C'1'     TEST THIS A MASTER CLIENT                    
         BNE   SR1                                                              
* READ THRU CLTHDRS FOR SLAVE CLIENTS                                           
         XC    WORKCLT+4(4),WORKCLT+4        FOR FIRST TIME                     
*                                                                               
SRCLT2   XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT                                                   
         MVI   KEY+7,X'FF'                                                      
         BAS   RE,SRHIGH                                                        
         B     *+8                                                              
SRCLT4   BAS   RE,SRSEQ                                                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SRENDMAS                                                         
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
*                                                                               
         BAS   RE,SRGET                                                         
*                                                                               
         CLC   PCLTKCLT,PCLTPROF+6-PCLTKEY+PBUYREC                              
         BNE   SRCLT4                                                           
*                                  SEE IF DOING ONE SLAVE CLIENT                
         CLI   QDIV,C' '                                                        
         BE    SR0                                                              
         CLC   PBUYREC+4(3),QDIV                                                
         BNE   SRCLT4              SKIP OTHER CLIENTS                           
*                                                                               
*                                                                               
SR0      MVC   WORKCLT,PBUYREC     SAVE THIS CLIENT KEY                         
         MVC   WORKCNM,PCLTNAME-PCLTKEY+PBUYREC  AND NAME                       
*                                                                               
* READ PRDHDRS FOR NAMES AND BUILD TABLE                                        
*                                                                               
SR1      DS    0H                                                               
         CLC   LASTCKEY,WORKCLT    TEST SAME CLIENT                             
         BE    SR4                                                              
         MVC   LASTCKEY,WORKCLT                                                 
*                                                                               
         LA    R0,PPRDKEY                                                       
         ST    R0,IOAREA                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT      A/M/X/CLT                                    
         MVI   KEY+3,6                                                          
         L     R3,APRDTAB                                                       
         LA    R4,24                                                            
         L     R5,APRDTABX                                                      
         BAS   RE,SRHIGH                                                        
         B     *+8                                                              
SR2      BAS   RE,SRSEQ                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BNE   SR4                                                              
         CLI   KEY+7,C'*'          SKIP OTHER AGY DATA                          
         BE    SR2                                                              
         BAS   RE,SRGET                                                         
         MVC   4(20,R3),PPRDNAME                                                
         MVC   0(3,R3),PPRDKPRD                                                 
         MVI   3(R3),C' '                                                       
BXLESR   BXLE  R3,R4,SR2                                                        
         DC    H'0'                TOO MANY PRDS                                
*                                                                               
SR4      DS    0H                  INITIALIZE SORTTAB                           
         L     R1,ASORTTAB                                                      
         ST    R1,ANEXTSRT                                                      
         XC    SORTCNT,SORTCNT     CLEAR COUNTER - NUMBER OF BUYS               
         GOTO1 BUFFALO,DMCB,=C'RESET',BUFFBUFF                                  
         XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT      A/M/X/CLT                                    
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),PCONKPUB                                                
*                                  NOTE - IF DOING ALL ZONES,EDTS               
*                                  THEN PCONPRD IS FROM FIRST CON READ          
         MVI   ONEPRD,C'N'         SET ONE PRODUCT SWITCH                       
         CLI   PCONPRD,C'A'        SEE IF DOING A PRD-CONTRACT                  
         BL    *+10                                                             
         MVC   KEY+13(3),PCONPRD                                                
         JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',SR4B,JUMP=N           
         MVC   KEY+13(3),QPRODUCT                                               
*                                                                               
SR4B     DS    0H                                                               
**NEW 2/18/88                                                                   
         OC    KEY+13(3),KEY+13     SEE IF DOING ONE PRD                        
         BZ    SR4C                                                             
         MVI   ONEPRD,C'Y'                                                      
         MVC   KEY+16(3),CSTART     CAN PUT START DATE IN KEY                   
*                                                                               
SR4C     DS    0H                                                               
         BAS   RE,SRHIGH                                                        
         B     SR6A                                                             
SR6      MVC   KEYSAVE,KEY                                                      
         BAS   RE,SRSEQ                                                         
SR6A     DS    0H                                                               
         CLC   KEY(13),KEYSAVE     SAME A/M/X/C/PUB                             
         BNE   SRENDPUB                                                         
SR7      CLI   KEY+13,C'*'         SKIP OTHER AGY DATA                          
         BE    SR6                                                              
         CLC   KEY+16(3),CSTART   TEST BUY WITHIN CONTRACT PERIOD               
**NEW 2/18/88         WAS BL  SR6                                               
         BL    SRLOW                                                            
**NEW 2/18/88                                                                   
         CLC   KEY+16(3),CEND                                                   
**NEW 2/18/88         WAS BH  SR6                                               
         BH    SRNEXTP          HIGH GO TRY NEXT PRD                            
**NEW 2/18/88                                                                   
*                                                                               
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    SR7A                                                             
         CLC   QPRODUCT,=C'   '                                                 
         BE    SR7A                                                             
         CLC   QPRODUCT,KEY+13         DOING ONE PRODUCT                        
         BE    SR7A                                                             
**NEW 2/18/88     WAS B   SR6                                                   
         B     SRENDPUB           CAN STOP LOOKING SINCE PRODUCT                
*                                 WAS SET IN KEY                                
**NEW 2/18/88                                                                   
*                                                                               
SR7A     CLI   PCONPRD,C'A'        SEE IF DOING A PRD CONTRACT                  
         BL    SR7A0                                                            
         CLC   PCONPRD,KEY+13                                                   
         BE    SR7A0                                                            
**NEW 2/18/88     WAS B   SR6                                                   
         B     SRENDPUB           CAN STOP LOOKING SINCE PRODUCT                
*                                 WAS SET IN KEY                                
         DROP  R6                                                               
*                                                                               
SRLOW    MVC   KEY+16(3),CSTART    SKIP TO START DATE                           
         XC    KEY+19(6),KEY+19     MUST CLEAR ESTIMATE                         
         B     SR4C                 GO TO READ HIGH                             
*                                                                               
SRNEXTP  CLI   ONEPRD,C'Y'          SEE IF DOING ONE PRODUCT                    
         BE    SRENDPUB             YES-DONE IF DATE WAS HIGH                   
         MVC   KEY+16(3),=3X'FF'    SKIP TO NEXT PRODUCT                        
         XC    KEY+19(6),KEY+19                                                 
         B     SR4C                                                             
*                                                                               
**NEW 2/18/88                                                                   
SR7A0    DS    0H                USED TO READ ESTHDR HERE TO SEE IF             
*                                IT WAS TEST                                    
*                                                                               
SR7A0D   DS    0H                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,SRGET            READ BUYREC                                  
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    SR6                                                              
*                                                                               
         TM    PBDSTAT,X'08'                                                    
         BO    SR6                 AND "HELD" CASH IN ADVANCE BUYS              
*                                                                               
         OC    SVESTFLT,SVESTFLT   FILTERING ON ESTIMATE ?                      
         BZ    SRESTX              NO                                           
         CLC   SVESTFLT,KEY+19                                                  
         BNE   SR6                 EST DOESN'T MATCH, SKIP                      
SRESTX   DS    0H                                                               
*                                                                               
         L     R1,ANEXTSRT         ENTER THIS BUY IN SORTTAB                    
         MVC   0(3,R1),KEY+16      DATE HIGH                                    
         MVC   3(3,R1),KEY+13      THEN PRODUCT                                 
         MVC   6(2,R1),KEY+19      THEN EST                                     
         MVC   8(4,R1),KEY+27      DISK ADDR                                    
         LA    R1,12(R1)                                                        
         MVI   0(R1),X'FF'         SET END OF TABLE                             
         ST    R1,ANEXTSRT                                                      
         L     R1,SORTCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,SORTCNT                                                       
         C     R1,=F'2500'         CHK FOR MAX BUYS                             
         BNH   *+6                                                              
         DC    H'0'                MUST DIE                                     
         B     SR6                 GO DO NEXT BUY                               
*                                                                               
SRENDPUB DS    0H                  END OF EDITION OR PUB                        
*                                  XSORT BUYS IN SORTTAB                        
         L     R2,SORTCNT                                                       
         LTR   R2,R2               SEE IF ANY TO SORT                           
         BZ    SRENDPBX                                                         
         L     R3,ASORTTAB                                                      
         ST    R3,ANEXTSRT                                                      
         XC    SAVPRD,SAVPRD                                                    
         GOTO1 XSORT,DMCB,(0,(R3)),(R2),12,8,0                                  
SRENDPB2 CLI   0(R3),X'FF'         END OF BUYS                                  
         BE    SRENDPBX                                                         
         MVC   KEY+27(4),8(R3)                                                  
         LA    R3,12(R3)           SET R3 TO NEXT BUY IN TABLE                  
         ST    R3,ANEXTSRT                                                      
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,SRGET            READ BUYREC                                  
         CLC   SAVPRD,PBUYKPRD     CHK FOR CHG OF PRD                           
         BE    SRENDPB4                                                         
         MVC   SAVPRD,PBUYKPRD                                                  
         MVI   PRDSW,0             SET FOR NEW PRD                              
SRENDPB4 GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
*                                                                               
*                                  DETERMINE CHANGE STATUS                      
         MVI   CHGSW,0                                                          
         CLI   QOPT5,C' '                                                       
         BE    SR7A01                                                           
         CLI   SCONREV,0                                                        
         BNE   SR7A01B                                                          
SR7A01   DS    0H                                                               
         TM    PBUYCNTL,X'80'                                                   
         BNZ   SRENDPNX                                                         
         B     SR7A08                                                           
SR7A01B  DS    0H                                                               
         CLC   PBDBUYDT,SCONREV                                                 
         BNH   SR7A02                                                           
         MVI   CHGSW,C'N'          NEW                                          
         B     SR7A04                                                           
*                                                                               
SR7A02   DS    0H                                                               
         CLC   PBDCHGDT,SCONREV                                                 
         BNH   *+8                                                              
         MVI   CHGSW,C'C'          CHANGE                                       
SR7A04   DS    0H                                                               
         TM    PBUYCNTL,X'80'                                                   
         BZ    SR7A06                                                           
         CLC   PBDDATE,SCONREV     DELETED BEFORE LAST REV                      
         BNH   SRENDPNX            YES - BYPASS                                 
         CLI   CHGSW,C'N'          ADDED AND DEL'D SING LAST REV                
         BE    SRENDPNX            YES - BYPASS                                 
         MVI   CHGSW,C'D'          DELETED                                      
         XC    GROSS(20),GROSS                                                  
SR7A06   DS    0H                                                               
         CLI   CHGSW,0             TEST ANY GHANGE                              
         BNE   SR7A08              YES                                          
         CLI   QOPT5,C'C'          NO- SHOW ONLY CHANGES                        
         BE    SRENDPNX            YES - BYPASS                                 
*                                                                               
SR7A08   DS    0H                                                               
*                                                                               
         CLI   QOPT3,C'R'          SEE IF SPACE RESERVATION                     
         BNE   SR7A08X         FOR 15'S - SET FORM PROFILE                      
*                                                                               
         CLI   PROGPROA+6,C'Y'  AM I MARKING INSERTIONS?                        
         BNE   SR7A08X                                                          
******   CLI   SOONSW,C'Y'         NO-OP FOR UPDATIVE SOONS                     
******   BE    SR7A08X                                                          
         TM    PBDSTAT,X'02'      SEE IF ALREADY SET                            
         BNZ   SR7A08X             CAN SKIP PUTREC                              
         OI    PBDSTAT,X'02'       HAS BEEN ON SPACE RES                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   SR7A08X                                                          
         BAS   RE,SRPUT            WRITE RECORD                                 
*                                                                               
SR7A08X  DS    0H                                                               
         CLI   QEDITION,C'Z'                                                    
         BNE   SR7A10                                                           
         CLI   PUBSW,0             TEST NEW PUB                                 
         BNE   SR7A10              NO                                           
         MVI   PUBSW,1                                                          
         GOTO1 ACSPUB                                                           
         CLC   W,SPACES                                                         
         BE    SR7A10                                                           
         MVI   LNEED,10                                                         
         MVC   P+1(30),W                                                        
         MVC   PSECOND+1(30),W+30                                               
         BAS   RE,SRPRT                                                         
         MVI   SPACING,2                                                        
         MVC   P+1(30),W+60                                                     
         BAS   RE,SRPRT                                                         
*                                                                               
SR7A10   DS    0H                                                               
         MVI   ACTSW,1                                                          
         CLI   QOPT2,C'N'                                                       
         BE    SR7A10B                                                          
         CLI   PBDCTYP,C'N'        SHOW NET IF INPUT AS NET                     
         BNE   SR7A1                                                            
*                                                                               
SR7A10B  DS    0H                                                               
         L     R0,GROSS                                                         
         S     R0,AGYCOM                                                        
         ST    R0,GROSS            USE NET NOT GROSS                            
*                                                                               
SR7A1     DS    0H                                                              
         LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
*                                                                               
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         MVC   PBYODTCN,DATCON                                                  
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
         MVI   PBYOCTL,X'48'       SPECIAL OUTDOOR + REG COMMS                  
*                                  CHECK DELETED BUY COMMENT OPTION             
*                                                                               
         CLI   PROGPROA+3,C'R'     SEE IF SUPPRESSING REGULAR                   
         BNE   SR7A3               COMMENTS                                     
*                                                                               
         TM    PBUYCNTL,X'80'      SEE IF DELETED BUY                           
         BZ    SR7A3                                                            
         MVI   PBYOCTL,X'40'       NO COMMENTS FOR DELETED BUYS                 
*                                                                               
SR7A3    DS    0H                                                               
         OI    PBYOCTL,X'01'       NET RATES AS NET                             
         CLI   PAGYPROF+1,C'L'                                                  
         BE    *+8                                                              
         OI    PBYOCTL,X'02'       SUPPRESS LINE NO.                            
*                                                                               
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
         CLI   PBDSPACE,C'*'  IF NOT "REAL" INSERTION CLEAR 2ND DATE            
         BNE   *+10                                                             
         MVC   PBYOMDY2(L'PBYOMDY2),SPACES   CLEAR SECOND DATE                  
*                                                                               
* SET UP PRINT LINE                                                             
         CLI   PRDSW,0             TEST NEW PRD                                 
         BNE   SR8                 NO                                           
         MVI   PRDSW,1                                                          
         CLI   PCLTPROF+5,C'1'     TEST MASTER CLT                              
         BNE   SR7B                                                             
* PRINT SLAVE CLIENT NAME                                                       
         SR    RE,RE               NEED 4 LINES                                 
         IC    RE,LINE                                                          
         LA    RE,4(RE)                                                         
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    *+8                                                              
         MVI   LNEED,4                                                          
         CLI   CLTSW,0             TEST NEW CLT                                 
         BNE   SR7B                NO                                           
         MVI   CLTSW,1                                                          
         MVI   P+1,C'*'                                                         
         MVC   P+3(20),WORKCNM                                                  
         LA    R2,P+25                                                          
         BAS   RE,SRFLOAT                                                       
         MVI   2(R2),C'*'                                                       
         MVI   SPACING,2                                                        
*                                                                               
         BAS   RE,SRPRT                                                         
*                                                                               
SR7B     DS    0H                                                               
         L     R3,APRDTAB                                                       
         CLC   PBUYKPRD,0(R3)                                                   
         BE    *+12                                                             
         LA    R3,24(R3)                                                        
         B     *-14                                                             
         XC    P+22(21),P+22                                        L02         
         XC    SAVPRDNM,SAVPRDNM                                    L02         
         BAS   RE,CKPRODB  CHECK PRODUCT OPTIONS                    L06         
         B     SR7BA       PRINT JUST PROD CODE                     L06         
         B     SR7BB       NOTHING TO PRINT                         L06         
*                                                                   L06         
         MVC   P+23(20),4(R3)                                                   
         MVI   P+22,C'-'                                                        
SR7BA    MVC   P+19(3),PBUYKPRD                                                 
SR7BB    MVC   SAVPRDNM,4(R3)       SAVE PRD NAME FOR NEW PAGE                  
*                                  CODE WAS SAVED EARLIER                       
         B     SR8A                                                             
SR8      DS    0H                                                               
         BAS   RE,CKPRODB  CHECK PRODUCT OPTIONS                    L06         
         B     *+8         PRINT JUST PROD CODE                     L06         
         B     SR8A        NOTHING TO PRINT                         L06         
         MVC   P+22(2),=C''''''    DITTOS                                       
SR8A     DS    0H                                                               
         CLI   CHGSW,0                                                          
         BE    SR8B                                                             
         MVI   P+1,C'*'                                                         
         MVI   P+5,C'*'                                                         
         MVC   P+2(3),=C'NEW'                                                   
         CLI   CHGSW,C'N'                                                       
         BE    SR8B                                                             
         MVC   P+2(3),=C'CHA'                                                   
         CLI   CHGSW,C'C'                                                       
         BE    SR8B                                                             
         MVC   P+2(3),=C'DEL'                                                   
SR8B     CLI   PBDSPACE,C'*'             MEANS NOT 'REAL' INSERTION             
         BE    SR9                                                              
         CLI   PBYOSPC,C'*'              MEANS NOT 'REAL' INSERTION             
         BE    SR9                                                              
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+43(3),DUB                                                      
*                                                                               
         MVC   P+7(11),PBYOMDY                                                  
*                                                                               
SR9      DS    0H                                                               
*                                  COUNT LINES NEEDED                           
         L     RF,PBYOLNS                                                       
*                                                                               
SR9D     DS    0H                                                               
         CLI   QOPT6,C' '                                                       
         BE    SR9F                                                             
         LA    RF,1(RF)            JOB NO.                                      
         TM    QOPT6,X'02'                                                      
         BZ    *+8                                                              
         LA    RF,3(RF)            +CAPTION                                     
*                                                                               
SR9F     DS    0H                                                               
         STC   RF,LNEED                                                         
         MVC   P+48(20),PBYOSPC                                                 
         CLI   QMEDIA,C'N'                                                      
         BNE   SR10                                                             
* NEWSPAPER FORMAT                                                              
         CLI   PBYOSPC,C' '        IF I HAVE SPACE DON'T SHOW UNITS             
         BH    SR9H                                                             
         MVC   P+48(7),PBYOUNTS                                                 
SR9H     CLI   PBDCOSTY,C'U'       SEE IF I HAVE UNIT COST                      
         BNE   SR10                                                             
         CLI   QOPT2,C'X'          SEE IF SUPPRESSING COST                      
         BE    SR10                THEN NO RATES AS WELL                        
*                                                                               
         MVC   P+58(8),PBYOUR                                                   
SR10     DS    0H                                                               
*                                                                               
*                                                                               
SR12     DS    0H                                                               
         CLI   QOPT2,C'X'          SEE IF SUPPRESSING COSTS                     
         BE    SR12C                                                            
*                                                                               
         MVC   P+66(13),PBYOGRS+1                                               
         JIF   QOPT2,NE,C' ',OR,PBDCTYP,NE,C'N',SR12C,JUMP=N                    
*                            FLOAT AN N IN FRONT OF NET COSTS                   
*                            ON CONTRACTS SHOWING BOTH GROSS AND NET            
         LA    R5,12                                                            
         OC    P+66(12),SPACES                                                  
         LA    R4,P+65                                                          
SR12A    CLI   1(R4),C' '                                                       
         BE    SR12B                                                            
         MVI   0(R4),C'N'                                                       
         B     SR12C                                                            
*                                                                               
SR12B    LA    R4,1(R4)                                                         
         BCT   R5,SR12A                                                         
*                                                                               
SR12C    DS    0H                                                               
         BAS   RE,SRPRT                                                         
*                                  BUILD LINE 2 IF NEEDED                       
         LA    R2,P+7       FOR POSSIBLE DAY OF WEEK                            
         LA    RF,PBYOMDY2         DEFAULT IS TO SHOW YEAR                      
         CLI   0(RF),C' '                                                       
         BNH   SR12C2                                                           
         LA    R2,PSECOND+7       FOR POSSIBLE DAY OF WEEK                      
         MVI   P+7,C'-'            SPECIAL FORMAT FOR NON-NEWS                  
         MVC   P+08(8),0(RF)                                                    
         CLI   QMEDIA,C'N'                                                      
         BNE   SR12C3                                                           
         MVI   P+6,C'+'                                                         
         MVC   P+7(8),0(RF)                                                     
         MVI   P+15,C' '           JUST IN CASE                                 
         B     SR12C3                                                           
*                                                                               
SR12C2   DS    0H                                                               
         LA    RF,PBYOISNM                                                      
         CLC   0(L'PBYOISNM,RF),SPACES                                          
         BE    SR12C3                                                           
         LA    R2,PSECOND+07                                                    
*        MVI   P+07,C' '                                                        
         MVC   P+07(11),0(RF)                                                   
*                                                                               
SR12C3   DS    0H                                                               
         CLI   PROGPROA+7,C'Y'     SEE IF SHOWING DAY OF WEEK                   
         BNE   SR12C5                                                           
         CLI   PBDSPACE,C'*'       DATE WON'T SHOW IF NOT "REAL"                
         BE    SR12C5              INSERTION,SO DON'T SHOW DAY                  
         CLI   PBDFREQ,C'M'        SEE IF DAY ENTER IN $BUY                     
         BE    SR12C5              MONTHLY                                      
         CLC   PBYOBFD,SPACES      NO DAY FOR BEST FOOD DAY OR                  
         BNE   SR12C5              WEEK OF INSERTIONS                           
         CLI   QMEDIA,C'N'         SEE IF MEDIA N OR I                          
         BE    SR12C4                                                           
         CLI   QMEDIA,C'I'                                                      
         BE    SR12C4                                                           
         CLI   PBUYKDAT+2,X'01'    SEE IF FIRST OF MONTH                        
         BE    SR12C5              FOR OTHER MEDIA                              
*                                  IF SO SKIP DAY OF WEEK                       
SR12C4   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(0,WORK)                                
*      NOTE -  R2 WILL EITHER POINT TO P+7 OR PSECOND+7                         
         GOTO1 GETDAY,DMCB,WORK,0(R2)                                           
*                                                                               
*        IF SECOND DATE PRESENT - SHOW IT'S DAY TOO                             
*                                                                               
         CLI   PBYOMDY2,C' '                                                    
         BNH   SR12C5                                                           
         GOTO1 DATCON,DMCB,(3,PBDIDAT2),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,4(R2)                                           
         MVI   3(R2),C'/'                                                       
*                                                                               
*                                                                               
SR12C5   CLC   PBYOSPC2,SPACES                                                  
         BE    *+10                                                             
         MVC   P+48(17),PBYOSPC2                                                
*                                                                               
         CLI   PBYOPRM,C' '                                                     
         BNH   SR12E                                                            
         LA    RF,P+48                                                          
         CLI   0(RF),C' '                                                       
         BNH   SR12D                                                            
         LA    RF,15(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,3(RF)                                                         
*                                                                               
SR12D    DS    0H                                                               
         CLI   PBYOPRM+1,C'C'    SEE IF COLOR PREMIUM                           
         BNE   *+10                                                             
         MVC   0(2,RF),PBYOPRM                                                  
*                         IF COLOR PREMIUM ALWAYS DISPLAY COLORS                
         CLI   QOPT2,C'X'        SEE IF SUPPRESSING COST AND PREMIUMS           
         BE    SR12E                                                            
*                                                                               
         MVC   0(11,RF),PBYOPRM                                                 
         CLI   PBYOPRM+1,C'C'      SEE IF COLOR PREMIUM                         
         BE    SR12E                                                            
         MVC   0(5,RF),=C'PREM='                                                
         MVC   5(11,RF),PBYOPRM                                                 
*                                                                               
SR12E    DS    0H                                                               
         CLC   P+48(17),SPACES     SEE IF I NEED TO PRINT P NOW                 
         BE    *+8                 NO                                           
         BAS   RE,SRPRT                                                         
*                                                                               
SR13A    CLI   PBYOBFD,C' '                                                     
         BE    SR13B                                                            
         MVC   P+48(27),PBYOBFD                                                 
         BAS   RE,SRPRT                                                         
*                                                                               
SR13B    DS    0H                                                               
* CHECK FOR COMMENTS                                                            
         LA    R2,PBYOCOMS                                                      
         LA    R3,5                                                             
SR13D    DS    0H                                                               
         CLI   0(R2),C' '                                                       
         BNH   SR14                                                             
*                                                                               
         CLI   PROGPROA+9,0        REMOVE CHARACTER FROM COMMENT ?              
         BNH   SR13J               NO                                           
         CLI   PROGPROA+9,C'N'     REMOVE CHARACTER FROM COMMENT ?              
         BE    SR13J               NO                                           
         CLC   0(1,R2),PROGPROA+9  COMMENT STARTING WITH CHARACTER ?            
         BNE   SR13J               NO - "NORMAL" PRINT                          
         GOTO1 ACHOP,DMCB,(46,1(R2)),(34,P+48),(C'P',2)                         
         B     SR13P                                                            
SR13J    DS    0H                                                               
         GOTO1 ACHOP,DMCB,(47,0(R2)),(34,P+48),(C'P',2)                         
*                                                                               
SR13P    DS    0H                                                               
         BAS   RE,SRPRT                                                         
         LA    R2,47(R2)                                                        
         BCT   R3,SR13D                                                         
*                                                                               
SR14     DS    0H                                                               
         CLI   QOPT6,C' '                                                       
         BE    SR15                                                             
*                                  JOB NO.                                      
         OC    PBDJOB,PBDJOB                                                    
         BZ    SR15                                                             
         LA    R4,P+48                                                          
*                                                                               
         CLI   QOPT6,C'A'                                                       
         BE    SR14B                                                            
         MVC   WORK,KEY                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVC   KEY+10(6),PBDJOB                                                 
         MVI   KEY+3,X'15'                                                      
         BAS   RE,SRHIGH                                                        
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,PJOBREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,SRGET                                                         
*                                  RESTORE KEY AND KEYSAVE                      
         MVC   KEY(64),WORK                                                     
SR14B    DS    0H                                                               
         TM    QOPT6,X'30'                                                      
         BNZ   SR14D                                                            
         MVC   0(8,R4),=C'AD NO. ='                                             
         MVC   9(6,R4),PBDJOB                                                   
         B     SR14F                                                            
SR14D    DS    0H                                                               
         MVC   0(6,R4),=C'COPY ='                                               
         MVC   7(17,R4),PJOBCPY                                                 
SR14F    DS    0H                                                               
         BAS   RE,SRPRT                                                         
         TM    QOPT6,X'02'                                                      
         BZ    SR14H                                                            
         MVC   0(25,R4),PJOBCAP1                                                
         MVC   132(25,R4),PJOBCAP2                                              
         MVI   SPACING,2                                                        
         BAS   RE,SRPRT                                                         
*                                                                               
SR14H    DS    0H                                                               
SR15     DS    0H                DISPLAY INTERNET SITE                          
*                                WAS CHECKING FOR JUST MEDIA I                  
*                                MEDIA L MAY ALSO HAVE                          
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'98'                                                    
         BAS   RE,SRNEXTEL                                                      
         BNE   SR15X                                                            
         USING PISITEEL,R2                                                      
         MVC   P+48(5),=C'SITE='                                                
         MVC   P+53(L'PISITE),PISITE                                            
         DROP  R2                                                               
SR15X    DS    0H                                                               
* ADD TO ACCUMULATORS                                                           
*                                                                               
SR16     DS    0H                                                               
         MVC   SSAVKEY(64),KEY     SAVE CURRENT KEY                             
*                                  GET ADDITIONAL CHARGE DATA                   
         GOTO1 APPGETCG,DMCB,(C'T',PBUYREC),DATAMGR,GETINS,PBUYKPRD             
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),X'00'         ANY ADD'L. CHARGES FOUND ?                   
         BE    SCLUPX              NO                                           
*                                                                               
         ZIC   R3,0(R1)         FOR LUP COUNTER - NO OF LINES IN BLOCK          
*                                                                               
         ICM   RE,15,0(R1)         RE POINTING TO DATA BLOCK                    
         BNZ   *+6                                                              
         DC    H'0'                ADDRESS IS NULLS                             
*                                                                               
         LA    RF,SADCGWRK        ADDRESS OF AREA FOR ADD'L. CHGS.              
         LA    R1,ADCDLNTH         LENGTH OF BLOCK                              
         MOVE  ((RF),(R1)),(RE)    MOVE BLOCK TO ADCGWRK AREA IN W/S            
*                                                                               
         LA    R4,SADCGWRK        WORK AREA FOR ADD'L. CHGS.                    
         USING ADDCHGD,R4                                                       
*                                                                               
         MVC   P+38(24),=C'** ADDITIONAL CHARGES **'                            
         GOTO1 SRPRT                                                            
*                                                                               
SCLUP    MVC   P+38(ADCLNLEN),ADCCOD1                                           
         GOTO1 SRPRT                                                            
         LA    R4,ADCLNLEN(R4)     BUMP TO NEXT BLOCK "LINE"                    
         BCT   R3,SCLUP                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVC   KEY(64),SSAVKEY      RESTORE KEY AND KEYSAVE                     
*                                   NO NEED TO GO TO SRHIGH                     
*                                                                               
SCLUPX   DS    0H                                                               
         CLC   P,SPACES            SEE IF ANYTHING LEFT TO PRINT                
         BE    *+8                                                              
         BAS   RE,SRPRT                                                         
         TM    PBUYCNTL,X'80'                                                   
         BNZ   SRENDPNX                                                         
*                                  BUFFALO TO PRD TOTALS                        
         XC    BUFREC,BUFREC                                                    
         LA    RE,BUFUNITS         POINT TO BUFUNITS TOTAL                      
         LA    RF,ACNEQ            NUMBER OF BUF TOTALS (6)                     
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)            NEXT TOTAL IN BUFREC                         
         BCT   RF,*-10             DO ALL TOTALS IN BUFREC                      
*                                                                               
         MVI   BUFTYP,X'01'                                                     
         MVC   BUFPRD,PBUYKPRD                                                  
         CLI   PBDSPACE,C'*'                                                    
         BE    SR16D                                                            
         CLI   PBYOSPC,C'*'        NOT 'REAL' INSERTION                         
         BE    SR16D                                                            
         L     R0,UNITS                                                         
         CLI   ININD,1             SEE IF DOING INCHES                          
         BE    SR16B                                                            
*                                  CARRY IN LINES                               
         CLI   PBDUIND,C'I'                                                     
         BE    SR16A                                                            
         CLI   PBDUIND,X'89'       LOWER CASE I - UNITS TO 2 DECIMALS           
         BNE   SR16CX                                                           
SR16A    MHI   R0,14               14 LINES/INCH                                
         CLI   PBDUIND,X'89'       LOWER CASE I - UNITS TO 2 DECIMALS           
         BNE   SR16CX                                                           
         CVD   R0,DUB                                                           
         AP    DUB,=P'50'          MUST ROUND TO NEAREST LINE                   
         DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R0,DUB                                                           
         B     SR16CX                                                           
*                                                                               
SR16B    DS    0H                  TOTALS IN INCHES                             
*                                                                               
         CLI   PBDUIND,X'89'       LOWER CASE I - ALREADY 2 DECIMALS            
         BE    SR16CX                                                           
         CLI   PBDUIND,C'I'                                                     
         BNE   SR16B5                                                           
         MHI   R0,100                                                           
         B     SR16CX                                                           
*                                                                               
SR16B5   DS    0H                                                               
         LR    R1,R0                                                            
         SR    R0,R0                                                            
         MHI   R1,100                                                           
         D     R0,=F'14'           14 LINES/INCF                                
         LR    R0,R1                                                            
*                                                                               
SR16CX   LA    R1,BUFUNITS                                                      
         ST    R0,FULL             SAVE LINES OR INCHES                         
         CLC   PBUYKDAT,SCONVDTE   COMPARE INS DATE VS. COL CONV DATE           
         BL    *+8                                                              
         LA    R1,BUFCUNTS                                                      
         CVD   R0,DOUBLE                                                        
         ZAP   0(8,R1),DOUBLE                                                   
         ZAP   DOUBLE,=P'1'                                                     
         LA    R1,BUFINS                                                        
         CLC   PBUYKDAT,SCONVDTE   COMPARE INS DATE VS. COL CONV DATE           
         BL    *+8                                                              
         LA    R1,BUFCINS                                                       
         ZAP   0(8,R1),DOUBLE      INSERTION COUNTER                            
SR16D    L     R0,GROSS                                                         
         CVD   R0,DOUBLE                                                        
         LA    R1,BUFGRS                                                        
         CLC   PBUYKDAT,SCONVDTE   COMPARE INS DATE VS. COL CONV DATE           
         BL    *+8                                                              
         LA    R1,BUFCGRS                                                       
         ZAP   0(8,R1),DOUBLE                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         LA    R3,CLTTOTS+0*ACLEQ  USE BEFORE CONV ACCUMULATORS                 
         CLC   PBUYKDAT,SCONVDTE   COMPARE INSERTION DATE                       
         BL    SR16D5                                                           
         LA    R3,CLTTOTS+3*ACLEQ  USE AFTER CONV ACCUMULATORS                  
*                                  TO COLUMN CONVERSION DATE                    
SR16D5   CLI   PBDSPACE,C'*'                                                    
         BE    SR17                                                             
         CLI   PBYOSPC,C'*'        NOT 'REAL' INSERION                          
         BE    SR17                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
* UNITS WERE SAVE IN FULL AT SR16CX                                             
*                                                                               
         L     R0,FULL                                                          
         CVD   R0,DOUBLE                                                        
         AP    0*ACLEQ(ACLEQ,R3),DOUBLE                                         
         AP    2*ACLEQ(ACLEQ,R3),=P'1'       INSERTION COUNTER                  
*                                                                               
SR17     DS    0H                                                               
         L     R6,ACONIO1          "RESET" A(PCONREC)                           
         USING PCONREC,R6                                                       
*                                                                               
         L     RE,GROSS                                                         
         CVD   RE,DOUBLE                                                        
         AP    1*ACLEQ(ACLEQ,R3),DOUBLE      DOLLARS                            
*                                                                               
         GOTO1 =V(PP12468),DMCB,PBUYREC   SEE IF ANY POSITION RECS  L03         
         L     R3,0(R1)     RETURN HAS ADD OF TABLE                 L03         
         CLI   0(R3),255                                            L03         
         BE    SRENDPNX                                             L03         
PPLOOP1  MVC   P+33(35),0(R3)                                       L03         
         BAS   RE,SRPRT                                             L03         
         LA    R3,35(R3)                                            L03         
         CLI   0(R3),255                                            L03         
         BNE   PPLOOP1                                              L03         
         BAS   RE,SRPRT                                             L03         
*                                                                   L03         
*                                                                               
SRENDPNX L     R3,ANEXTSRT         BUMP TO NEXT BUY IN SORTTAB                  
         B     SRENDPB2                                                         
*                                                                               
SRENDPBX DS    0H                  END OF BUYS FOR A EDT OR PUB                 
*                                                                               
* CLIENT TOTALS                                                                 
*                                                                               
         LA    RE,CLTTOTS          POINT TO CLT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTAL             
         CP    0(ACLEQ,RE),=P'0'                                                
         BNE   *+16                                                             
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-14                                                          
         B     SRENDC2             ALL ACCUMULATORS ARE ZERO                    
*                                                                               
         BAS   RE,SRPRT            SKIP A LINE                                  
         XC    BUFREC,BUFREC                                                    
         LA    RE,BUFUNITS         POINT TO BUFUNITS TOTAL                      
         LA    RF,ACNEQ            NUMBER OF BUF TOTALS (6)                     
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)            NEXT TOTAL IN BUFREC                         
         BCT   RF,*-10             DO ALL TOTALS IN BUFREC                      
*                                                                               
         MVI   BUFTYP,X'01'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     SRENDC1B                                                         
SRENDC1  GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFTYP,BUFFBUFF),BUFREC,0                  
*                                                                               
SRENDC1B CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    SRENDC1X                                                         
*                                                                               
         MVC   P+1(22),=C'* PRODUCT     TOTALS *'                               
         MVC   P+11(3),BUFPRD                                                   
*                                                                   L06         
         BAS   RE,CKPRODB  CHECK PRODUCT OPTIONS                    L06         
         B     *+8         PRINT JUST PROD CODE                     L06         
         B     *+8         NOTHING TO PRINT                         L06         
         B     *+10        NORMAL PROCESSING                        L06         
         MVC   P+1(12),=C'DO NOT PRINT'                             L06         
*                                                                               
         LA    R0,BUFUNITS                                                      
         ST    R0,TOTADDR                                                       
         BAS   RE,SRTOT                                                         
         B     SRENDC1                                                          
SRENDC1X MVC   P+5(17),=C'* CLIENT TOTALS *'                                    
         LA    R0,CLTTOTS                                                       
         ST    R0,TOTADDR                                                       
         BAS   RE,SRTOT                                                         
*                                                                               
SRENDC2  CLI   QEDITION,C'Z'         SEE IF DOING ALL EDITIONS                  
         BNE   SRENDC3                                                          
         AP    EDTTOTS+0*ACLEQ(ACLEQ),CLTTOTS+0*ACLEQ(ACLEQ)                    
         AP    EDTTOTS+1*ACLEQ(ACLEQ),CLTTOTS+1*ACLEQ(ACLEQ)                    
         AP    EDTTOTS+2*ACLEQ(ACLEQ),CLTTOTS+2*ACLEQ(ACLEQ)                    
*                                                                               
         AP    EDTTOTS+3*ACLEQ(ACLEQ),CLTTOTS+3*ACLEQ(ACLEQ)                    
         AP    EDTTOTS+4*ACLEQ(ACLEQ),CLTTOTS+4*ACLEQ(ACLEQ)                    
         AP    EDTTOTS+5*ACLEQ(ACLEQ),CLTTOTS+5*ACLEQ(ACLEQ)                    
*                                                                               
SRENDC3  CLI   PCLTPROF+5,C'1'     TEST MASTER CLIENT                           
         BNE   SRENDCX                                                          
*                                                                               
* ROLL TO CONTRACT TOTALS                                                       
*                                                                               
         AP    MASTOTS+0*ACLEQ(ACLEQ),CLTTOTS+0*ACLEQ(ACLEQ)                    
         AP    MASTOTS+1*ACLEQ(ACLEQ),CLTTOTS+1*ACLEQ(ACLEQ)                    
         AP    MASTOTS+2*ACLEQ(ACLEQ),CLTTOTS+2*ACLEQ(ACLEQ)                    
*                                                                               
         AP    MASTOTS+3*ACLEQ(ACLEQ),CLTTOTS+3*ACLEQ(ACLEQ)                    
         AP    MASTOTS+4*ACLEQ(ACLEQ),CLTTOTS+4*ACLEQ(ACLEQ)                    
         AP    MASTOTS+5*ACLEQ(ACLEQ),CLTTOTS+5*ACLEQ(ACLEQ)                    
*                                                                               
         LA    RE,CLTTOTS          POINT TO CLT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         MVI   CLTSW,0                                                          
         B     SRCLT2              READ NEXT CLIENT                             
SRENDCX  CLI   QEDITION,C'Z'          SEE IF DOING ALL EDITIONS                 
         BNE   SRENDCZ                                                          
*                                                                               
         LA    RE,EDTTOTS          POINT TO EDT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTAL             
         CP    0(ACLEQ,RE),=P'0'                                                
         BNE   *+16                                                             
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-14                                                          
         B     SRENDCX1            ALL ACCUMULATORS ARE ZERO                    
*                                                                               
         GOTO1 ACSPUB                                                           
         CLC   W,SPACES                                                         
         BE    SRENDCX0                                                         
         MVI   LNEED,3                                                          
         MVC   P+5(30),W                                                        
         MVC   PSECOND+5(30),W+30                                               
         BAS   RE,SRPRT                                                         
SRENDCX0 DS    0H                                                               
         MVC   P+5(18),=C'* EDITION TOTALS *'                                   
         CLI   QMEDIA,C'O'                                                      
         BNE   *+10                                                             
         MVC   P+5(9),=C' * MARKET'                                             
         LA    R0,EDTTOTS                                                       
         ST    R0,TOTADDR                                                       
         BAS   RE,SRTOT                                                         
*                                                                               
         AP    CONTOTS+0*ACLEQ(ACLEQ),EDTTOTS+0*ACLEQ(ACLEQ)                    
         AP    CONTOTS+1*ACLEQ(ACLEQ),EDTTOTS+1*ACLEQ(ACLEQ)                    
         AP    CONTOTS+2*ACLEQ(ACLEQ),EDTTOTS+2*ACLEQ(ACLEQ)                    
*                                                                               
         AP    CONTOTS+3*ACLEQ(ACLEQ),EDTTOTS+3*ACLEQ(ACLEQ)                    
         AP    CONTOTS+4*ACLEQ(ACLEQ),EDTTOTS+4*ACLEQ(ACLEQ)                    
         AP    CONTOTS+5*ACLEQ(ACLEQ),EDTTOTS+5*ACLEQ(ACLEQ)                    
*                                                                               
         AP    TOTEDS,=P'1'                                                     
*                                                                               
SRENDCX1 DS    0H                                                               
*                                                                               
* NOW USES EDITION TABLE FOR ALL 'ALL EDITION' REQS                             
* USED TO TO ONLY FOR MASTER CLT REQUEST                                        
*                                                                               
SRENDCX2 L     R4,ANXTEDT                                                       
         CLC   0(2,R4),=X'FFFF'    TEST END OF TABLE                            
         BE    SRENDCON                                                         
         CLC   PCONKPUB+4(2),0(R4) SEE IF I HAVE JUST DONE THIS EDT             
         BNE   SRENDCX3                                                         
         LA    R4,2(R4)                                                         
         ST    R4,ANXTEDT                                                       
         B     SRENDCX2                                                         
*                                                                               
SRENDCX3 MVC   PCONKPUB+4(2),0(R4)                                              
         LA    R4,2(R4)                                                         
         ST    R4,ANXTEDT                                                       
         B     SRENDCW                                                          
SRENDCX4 CLC   KEY(11),KEYSAVE       CHECK SAME 8 DIGITS                        
         BNE   SRENDCON         NO - DONE                                       
         CLC   QZONE,=C'ZZ'         SEE IF DOING ALL ZONES                      
         BE    SRENDCY                                                          
         CLC   KEY(12),KEYSAVE                                                  
         BNE   SRENDCON                                                         
SRENDCY  MVC   PCONKPUB(6),KEY+7                                                
*                                                                               
SRENDCW  LA    RE,EDTTOTS          POINT TO EDT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         MVI   PUBSW,0                                                          
         B     SORTSH2                                                          
*                                                                               
SRENDCON CP    TOTEDS,=P'1'         ONLY ONE EDITION                            
         BNH   SRENDCZ                                                          
         MVC   P+5(19),=C'* CONTRACT TOTALS *'                                  
         LA    R0,CONTOTS                                                       
         ST    R0,TOTADDR                                                       
         BAS   RE,SRTOT                                                         
         B     SRENDCZ                                                          
*                                                                               
SRENDCZ  DS    0H                                                               
         LA    RE,CLTTOTS          POINT TO CLT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,EDTTOTS          POINT TO EDT TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         LA    RE,CONTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         ZAP   TOTEDS,=P'0'                                                     
         CLI   QOPT5,C'C'                                                       
         BNE   SRENDCZZ                                                         
         CLI   SCONREV,0                                                        
         BE    SRENDCZZ                                                         
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         CLI   ACTSW,0                                                          
         BE    SRENDCZ2                                                         
         MVC   P+05(43),=C'**NOTE - ONLY SCHEDULE REVISIONS SINCE LAST'         
         MVC   PSECOND+14(29),=C'ISSUING OF CONTRACT ARE SHOWN'                 
         B     SRENDCZ4                                                         
SRENDCZ2 DS    0H                                                               
         MVC   P+05(56),=C'**NO SCHEDULE REVISIONS SINCE LAST ISSUING O'        
               F CONTRACT**'                                                    
SRENDCZ4 DS    0H                                                               
         BAS   RE,SRPRT                                                         
SRENDCZZ DS    0H                                                               
         XMOD1 1                                                                
         SPACE 2                                                                
SRENDMAS CLI   QEDITION,C'Z'      SEE IF DOING ALL EDITIONS                     
         BE    SRENDMX                                                          
*                                                                               
         LA    RE,MASTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTAL             
         CP    0(ACLEQ,RE),=P'0'                                                
         BNE   *+16                                                             
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-14                                                          
         B     SRENDMX             ALL ACCUMULATORS ARE ZERO                    
*                                                                               
         MVC   P+5(19),=C'* CONTRACT TOTALS *'                                  
         LA    R0,MASTOTS                                                       
         ST    R0,TOTADDR                                                       
         BAS   RE,SRTOT                                                         
*                                                                               
SRENDMX  LA    RE,MASTOTS          POINT TO CON TOTAL ACCUMULATORS              
         LA    RF,ACNEQ            NUMBER OF ACCUMULATORS PER TOTALS            
         ZAP   0(ACLEQ,RE),=P'0'                                                
         LA    RE,ACLEQ(RE)        NEXT ACCUMULATOR                             
         BCT   RF,*-10             DO ALL ACCUMULATORS                          
*                                                                               
         B     SRENDCX                                                          
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
SRFLOAT  OI    0(R2),C' '                                                       
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,SRFLOAT                                                       
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SUBROUTINE TO FORMAT PRD/CLT TOTALS (TOTADDR POINTS TO ACCUMS)                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SRTOT    NTR1                                                                   
         L     R5,TOTADDR                                                       
         LA    R3,P+30                                                          
         CLI   QMEDIA,C'N'                                                      
         BNE   SRTOT2                                                           
         CLI   SCONVDTE,X'FF'                CHK FOR COL CONV                   
         BE    SRTOTC                        NO                                 
         MVI   LNEED,3                       NEED 3 LINES                       
         CLI   SCONVIND,C'-'                 SEE IF DOING NEW TO OLD            
         BE    SRTOTB                                                           
         ZAP   DUB,3*ACLEQ(ACLEQ,R5)         MUST BE OLD TO NEW                 
         ZAP   DOUBLE,0*ACLEQ(ACLEQ,R5)                                         
         B     SRTOTB5                                                          
*                                                                               
SRTOTB   ZAP   DUB,0*ACLEQ(ACLEQ,R5)                                            
         ZAP   DOUBLE,3*ACLEQ(ACLEQ,R5)                                         
SRTOTB5  SR    R2,R2                                                            
         M     R2,SCONVFAC                                                      
         D     R2,=F'1000'                                                      
         CHI   R2,500                                                           
         BL    *+10                                                             
         AP    DOUBLE,=P'1'                  ROUND                              
         AP    DUB,DOUBLE                                                       
         B     SRTOTD                                                           
*                                                                               
SRTOTC   ZAP   DUB,0*ACLEQ(ACLEQ,R5)         UNITS FOR NEWSPAPERS ONLY          
SRTOTD   MVC   P+51(5),=C'LINES'                                                
*                                                                               
         CLI   ININD,0                                                          
         BE    SRTOT1              LINES                                        
         MVC   P+51(6),=C'INCHES'                                               
         EDIT  (P8,DUB),(14,P+36),2,COMMAS=YES                                  
         B     SRTOT2                                                           
*                                                                               
SRTOT1   DS    0H                                                               
         EDIT  (P8,DUB),(11,P+39),COMMAS=YES                                    
*                                                                               
SRTOT2   DS    0H                                                               
         CLI   QMEDIA,C'O'                   OUTDOOR - NO INSERT COUNT          
         BE    SRTOT3                                                           
         ZAP   DOUBLE,2*ACLEQ(ACLEQ,R5)      INSERTIONS                         
         AP    DOUBLE,5*ACLEQ(ACLEQ,R5)      ADD POST CONV INS                  
         EDIT  (P8,DOUBLE),(4,P+25)                                             
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    DOUBLE,=P'1'                                                     
         BH    *+8                                                              
         MVI   P+39,C' '                     SINGULAR, NO "S"                   
*                                                                               
SRTOT3   DS    0H                                                               
         LA    R3,P+66-3                                                        
SRTOT3A  DS    0H                                                               
         CLI   QOPT2,C'X'                    SEE IF SUPPRESSING COSTS           
         BE    SRTOT3B                                                          
*                                                                               
         ZAP   DUB,1*ACLEQ(ACLEQ,R5)                                            
         AP    DUB,4*ACLEQ(ACLEQ,R5)         ADD POST CONV $                    
         EDIT  (P8,DUB),(15,0(R3)),2,COMMAS=YES,FLOAT=-                         
         MVI   15(R3),C'*'                                                      
*                                                                               
SRTOT3B  DS    0H                                                               
         CLI   SCONVDTE,X'FF'                CHK FOR COL CONV                   
         BE    SRTOTX                                                           
         MVC   P+51(6),SPACES                                                   
         MVC   P+51(3),=C'OLD'                                                  
         MVC   P+55(5),=C'LINES'                                                
         CLI   ININD,0                                                          
         BE    *+10                                                             
         MVC   P+55(6),=C'INCHES'                                               
         CLI   SCONVIND,C'-'                 SEE IF CONV NEW TO OLD             
         BE    SRTOT3X                                                          
         MVC   P+51(3),=C'NEW'               MUST BE OLD TO NEW                 
*                                                                               
* THIS IS A PRODUCT TOTAL DO NOT PRINT BUT GO THRU LOGIC                        
*                                                                               
SRTOT3X  CLC   P+1(12),=C'DO NOT PRINT'                                         
         BE    SRTOTXX                                                          
*                                                                               
         BAS   RE,SRPRT                                                         
SRTOTXX  MVC   P+12(12),=C'BEFORE CONV.'                                        
         CLI   QMEDIA,C'N'                                                      
         BNE   SRTOT4                                                           
         ZAP   DUB,0*ACLEQ(ACLEQ,R5)                                            
         CLI   ININD,0                                                          
         BE    SRTOT3X5                                                         
         EDIT  (P8,DUB),(14,P+36),2,COMMAS=YES                                  
         B     SRTOT3X8                                                         
SRTOT3X5 EDIT  (P8,DUB),(11,P+39),COMMAS=YES                                    
SRTOT3X8 MVC   P+51(3),=C'OLD'                                                  
         MVC   P+55(5),=C'LINES'                                                
         CLI   ININD,0                                                          
         BE    *+10                                                             
         MVC   P+55(6),=C'INCHES'                                               
SRTOT4   CLI   QMEDIA,C'O'                   OUTDOOR - NO INSERT COUNT          
         BE    SRTOT5                                                           
         ZAP   DUB,2*ACLEQ(ACLEQ,R5)         INSERTIONS                         
         EDIT  (P8,DUB),(4,P+25)                                                
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    DUB,=P'1'                                                        
         BH    *+8                                                              
         MVI   P+39,C' '                     SINGULAR, NO "S"                   
*                                                                               
SRTOT5   DS    0H                                                               
         CLI   QOPT2,C'X'                    SEE IF SUPPRESSING COSTS           
         BE    SRTOT5C                                                          
         LA    R3,P+66-3                                                        
         ZAP   DUB,1*ACLEQ(ACLEQ,R5)                                            
         EDIT  (P8,DUB),(15,0(R3)),2,COMMAS=YES,FLOAT=-                         
         MVI   15(R3),C'*'                                                      
*                                                                               
* THIS IS A PRODUCT TOTAL DO NOT PRINT BUT GO THRU LOGIC                        
*                                                                               
SRTOT5C  DS    0H                                                               
         CLC   P+1(12),=C'DO NOT PRINT'                                         
         BE    *+8                                                              
*                                                                               
         BAS   RE,SRPRT                                                         
         MVC   P+13(11),=C'AFTER CONV.'                                         
         GOTO1 DATCON,DMCB,(3,SCONVDTE),(5,P+4)                                 
         CLI   QMEDIA,C'N'                                                      
         BNE   SRTOT6                                                           
         ZAP   DUB,3*ACLEQ(ACLEQ,R5)                                            
         CLI   ININD,0                                                          
         BE    SRTOT5X                                                          
         EDIT  (P8,DUB),(14,P+36),2,COMMAS=YES                                  
         B     SRTOT5X5                                                         
SRTOT5X  EDIT  (P8,DUB),(11,P+39),COMMAS=YES                                    
SRTOT5X5 MVC   P+51(3),=C'NEW'                                                  
         MVC   P+55(5),=C'LINES'                                                
         CLI   ININD,0                                                          
         BE    *+10                                                             
         MVC   P+55(6),=C'INCHES'                                               
SRTOT6   CLI   QMEDIA,C'O'                   OUTDOOR - NO INSERT COUNT          
         BE    SRTOT7                                                           
         ZAP   DUB,5*ACLEQ(ACLEQ,R5)         INSERTIONS                         
         EDIT  (P8,DUB),(4,P+25)                                                
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    DUB,=P'1'                                                        
         BH    *+8                                                              
         MVI   P+39,C' '                     SINGULAR, NO "S"                   
*                                                                               
SRTOT7   DS    0H                                                               
         CLI   QOPT2,C'X'                    SEE IF SUPPRESSING COSTS           
         BE    SRTOTX                                                           
         LA    R3,P+66-3                                                        
         ZAP   DUB,4*ACLEQ(ACLEQ,R5)                                            
         EDIT  (P8,DUB),(15,0(R3)),2,COMMAS=YES,FLOAT=-                         
         MVI   15(R3),C'*'                                                      
*                                                                               
* THIS IS A PRODUCT TOTAL DO NOT PRINT BUT GO THRU LOGIC                        
*                                                                               
SRTOTX   CLC   P+1(12),=C'DO NOT PRINT'                                         
         BE    XXIT                                                             
*                                                                               
         MVI   SPACING,2                     SKIP LINE AFTER                    
*                                                                               
         BAS   RE,SRPRT                                                         
*                                                                               
XXXIT    XIT1                                                                   
*                                                                               
XXIT     XC    P,P                                                              
         B     XXXIT                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*****                        DS    F                                            
*****               SRPRT    ST    RE,*-4                                       
*****                        GOTO1 ASORTPRT                                     
*****                        L     RE,SRPRT-4                                   
*****                        BR    RE                                           
*                                                                               
SRPRT    NTR1                CHANGE IS TO NOT USE                               
         GOTO1 ASORTPRT      STORAGE IN A MIDDLE OF PROGRAM                     
         XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
SRHIGH   MVC   KEYSAVE,KEY                                                      
         LA    R0,DMRDHI                                                        
         B     *+8                                                              
SRSEQ    LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         NTR1                                                                   
         SPACE 2                                                                
SRSEQ2   DS    0H                                                               
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
         CLI   KEY+25,X'FF'                                                     
         BNE   SRDMX                                                            
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         B     SRSEQ2                                                           
         B     SRDMX                                                            
SRPUT    NTR1                                                                   
         SPACE 2                                                                
         GOTO1 DATAMGR,DMCB,(DMINBTS,PUTREC),PRTFILE,KEY+27,IOAREA,    X        
               DMWORK                                                           
         B     SRDMX                                                            
*                                                                               
SRGET    NTR1                                                                   
         SPACE 2                                                                
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),PRTFILE,KEY+27,IOAREA,    X        
               DMWORK                                                           
*                                                                               
SRDMX    DS    0H                                                               
         MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,DMCB+8                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
SRNEXTEL SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SRNXTELX                                                         
         CLC   ELCODE1,0(R2)                                                    
         BCR   8,RE                                                             
         B     SRNEXTEL                                                         
SRNXTELX LTR   R2,R2                                                            
         BR    RE                                                               
*                                                                   L02         
*    RF POINTS TO OPTION                                            L02         
*    IF PROGRAM IS 12 (OR13) AND OPTION IS YES WILL RETURN 0(RE)    L02         
*           OTHERWISE 4(RE)                                         L02         
*                                                                   L02         
OPTIONCC CLC   QPROG,=C'13'       CONTRACT TURNAROUND               L14         
         BE    OPTNCC5                                              L14         
         CLC   QPROG,=C'12'                                         L02         
         BNE   4(RE)                                                L02         
OPTNCC5  CLI   0(RF),C'N'                                           L02         
         BE    4(RE)                                                L02         
         BR    RE                                                   L02         
*                                                                   L06         
*                                                                   L06         
CKPRODB  CLC   QPROG,=C'12'                                         L06         
         LA    RF,PROFOPTS+6     12 PRODUCT OPTION                  L06         
         BE    CKPROCB                                              L06         
         CLC   QPROG,=C'13'        OR TURNAROUND                    L14         
         BE    CKPROCB                                              L14         
         CLC   QPROG,=C'15'                                         L06         
         LA    RF,PROFOPTS+11    15 PRODUCT OPTION                  L06         
         BNE   8(RE)         NORMAL PROCESSING                      L06         
CKPROCB  CLI   0(RF),C'N'  NORMAL PROCESSING                        L06         
         BE    8(RE)                                                L06         
         CLI   0(RF),C'Y'  PRINT PRODUCT CODE ONLY                  L06         
         BER   RE                                                   L06         
         CLI   0(RF),C'B'  DO NOT PRINT CODE OR DESCRIPTION         L06         
         BE    4(RE)                                                L06         
         B     8(RE)                                                L06         
*                                                                   L06         
         LTORG                                                                  
*                                                                               
**NEW 2/18/88                                                                   
SRONEP   DS    CL1              SET TO 'Y' IF DOING ONE PRODUCT                 
**NEW 2/18/88                                                                   
*                                                                               
SSAVKEY  DS    CL64                                                             
*                                                                               
SRHN     DC    C'      INS DATE    PRODUCT                 EST  SPACE  X        
                    RATE  '                                                     
SRHNUND  DC    C'      --------    -------                 ---  -----  X        
                  --------'                                                     
SRHM     DC    C'      INS DATE    PRODUCT                 EST  SPACE DX        
               ESCRIPTION '                                                     
SRHMUND  DC    C'      --------    -------                 ---  -------X        
               ---------- '                                                     
SRHO     DC    C'       POSTING                                 SIZE  -X        
               DISPLAYS-  '                                                     
SRHO2    DC    C'        DATE      PRODUCT                 EST  SHOW  RX        
               EG  ILLUM  '                                                     
SRHOUND  DC    C'      --------    -------                 ---  ----  -X        
               --  -----  '                                                     
*                                                                               
SGRSWDS  DC    C'            '                                                  
         DC    C'  GROSS COST'                                                  
         DC    C'  ----------'                                                  
SNETWDS  DC    C'            '                                                  
         DC    C'    NET COST'                                                  
         DC    C'    --------'                                                  
*                                                                               
SADCGWRK DS    CL(ADCDLNTH)                                                     
         EJECT                                                                  
SORTPRT  CSECT                                                                  
         NMOD1 0,SORTPRT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP12WRKD,R8                                                      
         CLI   FORCEHED,C'P'                                                    
         BE    SRPRT2                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,LNEED                                                         
         LA    RE,1(RF,RE)                                                      
         STC   RE,BYTE                                                          
         MVI   LNEED,0                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    SRPRT4                                                           
SRPRT2   EQU   *                                                                
         GOTO1 ACONEOP                                                          
*                                                                               
         EDIT  (B2,PAGE),(2,MH5+74)                                             
*                                                                               
SRPRT2C  DS    0H                                                               
*                                                                               
*                                  FIRST TIME HEADLINE                          
*                                  SET HEADLINES FOR NO EDIT TOTALS             
         MVC   SRHNP+18(7),=C'PRODUCT'                              L06         
         MVC   SRHMP+18(7),=C'PRODUCT'                              L06         
         MVC   SRHOP+18(7),=C'PRODUCT'                             L06          
         MVC   SRHNUNDP+18(7),=7C'-'                                L06         
         MVC   SRHMUNDP+18(7),=7C'-'                                L06         
         MVC   SRHOUNDP+18(7),=7C'-'                                L06         
*                                                                  L06          
         BAS   RE,CKPRODD  CHECK PRODUCT OPTIONS                   L06          
         B     JSPPCC      PRINT JUST PROD CODE                    L06          
         B     *+8         NOTHING TO PRINT                        L06          
         B     JSPPCC      NORMAL PROCESSING                       L06          
*                                                                  L06          
         MVC   SRHNP+18(7),=C'       '                              L06         
         MVC   SRHMP+18(7),=C'       '                              L06         
         MVC   SRHOP+18(7),=C'       '                             L06          
         MVC   SRHNUNDP+18(7),=7C' '                                L06         
         MVC   SRHMUNDP+18(7),=7C' '                                L06         
         MVC   SRHOUNDP+18(7),=7C' '                                L06         
JSPPCC   MVC   SRHNP(6),SPACES                                                  
         MVC   SRHMP(6),SPACES                                                  
         MVC   SRHOP+47(17),=C'SIZE  -DISPLAYS- '                L02            
         MVC   SRHO2P+47(5),=C'SHOW '                            L02            
         MVC   SRHO2P(6),SPACES                                                 
         MVC   SRHNUNDP(6),SPACES                                               
         MVC   SRHMUNDP(6),SPACES                                               
         MVC   SRHOUNDP(6),SPACES                                               
         CLI   QEDITION,C'Z'                                                    
         BNE   SRPRT2E                                                          
*                                  FIX HEADLINES FOR EDIT TOTALS                
         MVC   SRHNP(6),=C'EDITN/'                                              
         MVC   SRHMP(6),=C'EDITN/'                                              
         MVC   SRHO2P(6),=C'MARKT/'                                             
         MVC   SRHNUNDP(14),DASHES1                                             
         MVC   SRHMUNDP(14),DASHES1                                             
         MVC   SRHOUNDP(14),DASHES1                                             
*                                                                               
SRPRT2E  DS    0H                                                               
         LA    RF,PROFOPTS+7       OVERRIDE WITH GRP                L02         
         BAS   RE,OPTIONCD                                          L02         
         B     *+8                 OVERRIDE                         L02         
         B     *+16          NO    OVERRIDE                         L02         
         MVC   SRHOP+47(5),=C'GRP  '                            L02             
         MVC   SRHO2P+47(6),=C'LEVEL '                           L02            
*                                                                   L02         
         LA    RF,PROFOPTS+8       USE MEDIA ALLOCATION             L02         
         BAS   RE,OPTIONCD                                          L02         
         B     *+8                 OVERRIDE                         L02         
         B     *+10          NO    OVERRIDE                         L02         
         MVC   SRHOP+53(11),=C'MEDIA ALLOC'                     L02             
*                                                                   L02         
         LA    R0,14                                                            
         LA    R1,HEAD1                                                         
         LA    R2,MH1                                                           
         MVC   0(080,R1),0(R2)                                                  
         LA    R1,132(R1)                                                       
         LA    R2,080(R2)                                                       
         BCT   R0,*-14                                                          
         CLC   P+33(14),=C'** SCHEDULE **'                                      
         BE    SRPRT4              SEE IF SCHEDULE ON NEW PAGE                  
*                                  SO I WON'T REDO HEADLINES                    
         LA    R1,HEAD14                                                        
         MVC   33(14,R1),=C'** SCHEDULE **'                                     
         MVI   RCSUBPRG,0                                                       
         MVC   SAVEP,P                                                          
         MVC   SAVEP2,PSECOND                                                   
         MVC   SAVESPAC,SPACING                                                 
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVI   SPACING,1                                                        
         LA    R5,SPACES                                                        
         CLI   QOPT2,C'X'            SEE IF SUPPRESSING COSTS                   
         BE    SRPRT2F                                                          
         LA    R5,SGRSWDSP                                                      
         CLI   QOPT2,C'N'                                                       
         BNE   *+8                                                              
         LA    R5,SNETWDSP                                                      
*                                                                               
SRPRT2F  DS    0H                                                               
         LA    R2,SRHOP                                                         
         LA    R3,3                                                             
         CLI   QMEDIA,C'O'                                                      
         BE    SRPRT3                                                           
         LA    R5,12(R5)                                                        
         LA    R2,SRHNP                                                         
         LA    R3,2                                                             
         CLI   QMEDIA,C'N'                                                      
         BE    SRPRT3                                                           
         LA    R2,SRHMP                                                         
         LA    R3,2                                                             
SRPRT3   DS    0H                                                               
         MVC   P+66(12),0(R5)                                                   
         MVC   P+1(L'SRHNP),0(R2)                                               
*                                                                               
         JIF   QOPT2,NE,C'X',OR,QMEDIA,NE,C'N',SRPRT3C,JUMP=N                   
*                            IF NEWSPAPERS AND SUPPRESSING COST                 
*                            BLANK-OUT 'RATE' AND ITS UNDERLINING               
*                                                                               
         MVC   P+58(8),SPACES                                                   
SRPRT3C  DS    0H                                                               
         GOTO1 AFREPORT                                                         
*                                                                               
         LA    R2,L'SRHNP(R2)                                                   
         LA    R5,12(R5)                                                        
         BCT   R3,SRPRT3                                                        
         GOTO1 AFREPORT                                                         
*                                                                               
         MVC   P,SAVEP                                                          
         MVC   PSECOND,SAVEP2                                                   
         CLC   P+22(2),=C''''''    CHK FOR DITTOS                               
         BNE   SRPRT3E                                                          
         XC    P+22(20),P+22      NO PRODUCT DESCRIPTION            L02         
*                                                                   L02         
         BAS   RE,CKPRODD  CHECK PRODUCT OPTIONS                    L06         
         B     SRPTT3D     PRINT JUST PROD CODE                     L06         
         B     SRPRT3E     NOTHING TO PRINT                         L06         
*                                                                   L06         
         MVI   P+22,C'-'           WHEN GOING TO NEW PAGE                       
         MVC   P+23(20),SAVPRDNM                                                
SRPTT3D  MVC   P+19(3),SAVPRD       REPRINT PRODUCT                             
SRPRT3E  MVC   SPACING,SAVESPAC                                                 
SRPRT4   DS    0H                                                               
         GOTO1 AFREPORT                                                         
*                                                                               
         XMOD1 1                                                                
*                                                                   L02         
*    RF POINTS TO OPTION                                            L02         
*   IF PROGRAM IS 12 (OR 13) AND OPTION IS YES WILL RETURN 0(RE)    L02         
*           OTHERWISE 4(RE)                                         L02         
*                                                                   L02         
OPTIONCD CLC   QPROG,=C'13'       CONTRACT TURNAROUND               L14         
         BE    OPTNCD5                                              L14         
         CLC   QPROG,=C'12'                                         L02         
         BNE   4(RE)                                                L02         
OPTNCD5  CLI   0(RF),C'N'                                           L02         
         BE    4(RE)                                                L02         
         BR    RE                                                   L02         
*                                                                   L06         
*                                                                   L06         
CKPRODD  CLC   QPROG,=C'12'                                         L06         
         LA    RF,PROFOPTS+6     12 PRODUCT OPTION                  L06         
         BE    CKPROCD                                              L06         
         CLC   QPROG,=C'13'       OR TURNAROUND                     L14         
         BE    CKPROCD                                              L14         
         CLC   QPROG,=C'15'                                         L06         
         LA    RF,PROFOPTS+11    15 PRODUCT OPTION                  L06         
         BNE   8(RE)         NORMAL PROCESSING                      L06         
CKPROCD  CLI   0(RF),C'N'  NORMAL PROCESSING                        L06         
         BE    8(RE)                                                L06         
         CLI   0(RF),C'Y'  PRINT PRODUCT CODE ONLY                  L06         
         BER   RE                                                   L06         
         CLI   0(RF),C'B'  DO NOT PRINT CODE OR DESCRIPTION         L06         
         BE    4(RE)                                                L06         
         B     8(RE)                                                L06         
*                                                                               
SRHNP    DC    C'      INS DATE    PRODUCT                 EST  SPACE  X        
                    RATE  '                                                     
SRHNUNDP DC    C'      --------    -------                 ---  -----  X        
                  --------'                                                     
SRHMP    DC    C'      INS DATE    PRODUCT                 EST  SPACE DX        
               ESCRIPTION '                                                     
SRHMUNDP DC    C'      --------    -------                 ---  -------X        
               ---------- '                                                     
SRHOP    DC    C'       POSTING                                 SIZE  -X        
               DISPLAYS-  '                                                     
SRHO2P   DC    C'        DATE      PRODUCT                 EST  SHOW  RX        
               EG  ILLUM  '                                                     
SRHOUNDP DC    C'      --------    -------                 ---  ----  -X        
               --  -----  '                                                     
*                                                                               
SGRSWDSP DC    C'            '                                                  
         DC    C'  GROSS COST'                                                  
         DC    C'  ----------'                                                  
SNETWDSP DC    C'            '                                                  
         DC    C'    NET COST'                                                  
         DC    C'    --------'                                                  
*                                                                               
         LTORG                                                                  
FNDEDTS  CSECT                                                                  
         NMOD1 0,FNDEDTS                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP12WRKD,R8                                                      
         L     RF,AEDTLIST                                                      
         ST    RF,ANXTEDT                                                       
         MVC   0(2,RF),=X'FFFF'                                                 
         L     R4,AEDTLIST                                                      
*                                                                               
         L     R6,ACONIO1          A(PCONREC)                                   
         USING PCONREC,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QMEDIA                                                    
         MVC   KEY+1(6),PCONKPUB                                                
         MVC   KEY+7(2),PCONKAGY                                                
         XC    KEY+5(2),KEY+5      TO TRY FOR BASE PUB                          
         MVI   KEY+9,X'81'                                                      
         BAS   RE,FHIPUB                                                        
         B     *+8                                                              
FED2     BAS   RE,FSEQPUB                                                       
         CLC   KEY(5),KEYSAVE      CHECK 8 DIGITS                               
         BNE   FEDX                NO - DONE                                    
         CLC   QZONE,=C'ZZ'           SEE IF DOING ALL ZONES                    
         BE    FED3                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   FED2                                                             
*                                                                               
FED3     CLC   KEY+7(2),PCONKAGY                                                
         BNE   FED4                                                             
FED3A    CLC   0(2,R4),KEY+5       SEE IF I JUST DID THIS EDTION                
*                                  WILL BE THERE IF THIS IS LTLREC              
         BE    FED2                YES - SKIP                                   
         CLC   0(2,R4),=X'FFFF'    CHK FOR FIRST TIME                           
         BE    FED3C               YES - OVERRIDE FFFF                          
         LA    R4,2(R4)                                                         
FED3C    MVC   0(2,R4),KEY+5       STORE ZONE AND EDITION IN TABLE              
         B     FED2                                                             
*                                                                               
FED4     CLI   PAGYPROF+16,C'1'    SEE IF USING SRDS                            
         BNE   FED2                                                             
         CLC   KEY+7(2),=C'ZZ'                                                  
         BE    FED3A                                                            
         B     FED2                                                             
*                                                                               
FEDX     LA    R4,2(R4)                                                         
         MVC   0(2,R4),=X'FFFF'    SET END OF TABLE                             
         XIT1                                                                   
         SPACE 2                                                                
FHIPUB   LA    R0,DMRDHI                                                        
         B     FREAD                                                            
*                                                                               
FSEQPUB  LA    R0,DMRSEQ                                                        
*                                                                               
FREAD    ST    R0,DMCB                                                          
         MVC   KEYSAVE,KEY                                                      
         NTR1                                                                   
         GOTO1 DATAMGR,DMCB,,PUBDIR,KEY,KEY                                     
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                 CONTRACT REGISTER                             
CONREG   CSECT                                                                  
         NMOD1 0,CONREG                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP12WRKD,R8                                                      
*****    CLC   QPROG,=C'13'        NO REGISTER FOR TA                           
*****    BE    CONREGX             NO-OPED 7/94                                 
*                                                                               
         CLI   PROGPROA+4,C'Y'     SEE IF NOT ADDING TO REGISTER                
         BE    CONREGX                                                          
*                                                                               
         L     R6,ACONIO1          A(PCONREC)                                   
         USING PCONREC,R6                                                       
*                                                                               
         L     R7,ANEXTCON        POINT R7 TO NEXT CONTRACT                     
         XC    0(CONTLEN,R7),0(R7)                                              
         MVC   0(1,R7),PCONKMED                                                 
         MVC   1(3,R7),PCONKCLT                                                 
         MVC   4(6,R7),PCONKPUB                                                 
*                                                                               
         MVC   25(3,R7),QESTEND                                                 
*                                                                               
         CLI   QEDITION,C'Z'         SEE IF DOING ALL EDITIONS                  
         BNE   *+8                                                              
         MVI   9(R7),0                                                          
         CLC   QZONE,=C'ZZ'         SEE IF DOING ALL ZONES                      
         BNE   *+8                                                              
         MVI   8(R7),0                                                          
         MVC   10(2,R7),PCONNUM                                                 
         MVC   12(6,R7),CSTART     START-END                                    
         CLI   PCONPRD,C'A'                                                     
         BL    *+10                                                             
         MVC   18(3,R7),PCONPRD                                                 
*                                                                               
         CLI   QDIV,C' '            SEE IF DOING A SLAVE CLT REQ                
         BNH   *+10                                                             
         MVC   21(3,R7),QDIV          PUT SLAVE IN REGISTER                     
         CLI   FAXOPT,C'Y'          SEE IF FAXING                               
         BNE   CONREG5                                                          
         CLC   MYFAX,SPACES         SEE IF I HAVE A FAX NUMBER                  
         BE    CONREG5                                                          
         MVI   24(R7),C'F'          FAXED CONTRACT                              
*                                                                               
CONREG5  LA    R7,CONTLEN(R7)                                                   
         ST    R7,ANEXTCON          STORE ADRESS OF NEXT CONTRACT               
         MVI   0(R7),0                                                          
         C     R7,ACONTABX         COMPARE TO END OF TABLE                      
         BL    CONREGX                                                          
         MVI   FULLSW,1       SET REGISTER FULL SWITCH                          
CONREGX  XMOD1 1                                                                
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
PRTREG   CSECT                                                                  
         NMOD1 0,PRTREG                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP12WRKD,R8                                                      
*                                                                               
         MVC   SVRCPG,RCPROG       SAVE "REAL" RCPROG                           
         MVC   RCPROG,SVQPROG   SO REGISTER WILL PRINT PROPER PROGRAM           
*                                                                               
******   CLC   SVQPROG,=C'13'      NO REGISTER FOR TA                           
******   BE    PRTREGX             NO-OPED 7/94                                 
         L     R7,ACONTAB                                                       
         CLI   0(R7),0                                                          
         BNE   PRTR0                                                            
         CLI   PROGPROA+4,C'Y'                                                  
         BNE   PRTREGS         NO CONTRACTS                                     
         B     PRTREGX                                                          
*                                                                               
PRTR0    DS    0H                                                               
         MVI   FORCEHED,C'P'      FORCE PAGE AND FORCE HEADING                  
         XC    PAGE,PAGE                                                        
         MVI   PAGE+1,1                                                         
PRTR1    MVC   P+1(3),1(R7)       CLIENT                                        
*                                                                               
         MVC   P+16(3),25(R7)     ESTIMATE                                      
*                                                                               
         CLI   21(R7),C' '        SEE IF SLAVE                                  
         BNH   PRTR3                                                            
         CLI   3(R7),C' '         CLT IS 2 CHARACTERS?                          
         BH    PRTR1C                                                           
         MVI   P+3,C'-'                                                         
         MVC   P+4(3),21(R7)                                                    
         B     PRTR3                                                            
*                                                                               
PRTR1C   MVI   P+4,C'-'                                                         
         MVC   P+5(3),21(R7)                                                    
*                                                                               
PRTR3    DS    0H                                                               
         CLI   18(R7),C' '         SEE IF PRODUCT CONTRACT                      
         BNH   PRTR5                                                            
         MVC   P+10(3),18(R7)       PRODUCT                                     
PRTR5    DS    0H                                                               
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),4(R7)),P+22                                   
*                                                                               
         MVC   HALF,10(R7)           CONTRACT                                   
         EDIT  HALF,(3,P+45),ALIGN=LEFT                                         
         GOTO1 DATCON,DMCB,(3,12(R7)),(5,P+54)                                  
         GOTO1 DATCON,DMCB,(3,15(R7)),(5,P+67)                                  
         CLI   24(R7),C'F'       SEE IF FAXED                                   
         BNE   *+10                                                             
         MVC   P+77(5),=C'FAXED'                                                
*                                                                               
         BAS   RE,REGPRT                                                        
         LA    R7,CONTLEN(R7)                                                   
         CLI   0(R7),0                                                          
         BNE   PRTR1                                                            
         CLI   FULLSW,1                                                         
         BNE   PRTREGX                                                          
         BAS   RE,REGPRT                                                        
         MVC   P+10(33),=C'*** REGISTER BUFFER EXCEEDED ***'                    
         BAS   RE,REGPRT                                                        
         B     PRTREGX                                                          
*                                                                               
PRTREGS  DS    0H                                                               
         MVI   FORCEHED,C'P'                                                    
         XC    PAGE,PAGE                                                        
         MVI   PAGE+1,1                                                         
         BAS   RE,REGPRT                                                        
         MVC   P+10(28),=C'*** NO CONTRACTS PRINTED ***'                        
         BAS   RE,REGPRT                                                        
         B     PRTREGX                                                          
*                                                                               
PRTREGX  L     R7,ACONTAB                                                       
         ST    R7,ANEXTCON                                                      
         MVI   0(R7),0        INITIALIZE ADDRESS                                
         MVI   FORCEHED,C'P'                                                    
         MVC   RCPROG,SVRCPG      RESTORE "REAL" RCPROG                         
         XMOD1 1                                                                
         EJECT                                                                  
REGPRT   NTR1                                                                   
*                                                                               
         MVC   HEAD1+1(10),LASTMEDN                                             
         LA    RE,HEAD1+11                                                      
         LA    RF,9                                                             
REGP1    CLI   0(RE),C' '                                                       
         BH    REGP2                                                            
         AHI   RE,-1                                                            
         BCT   RF,REGP1           TO PREVENT LOOPS                              
         B     REGP3                                                            
*                                                                               
REGP2    CLI   0(RE),C'S'         REMOVE 'S' FROM MEDIA NAME                    
         BNE   REGP3                                                            
         MVI   0(RE),C' '                                                       
*                                                                               
REGP3    DS    0H                                                               
*                                                                               
*        CHECK FOR VARIOUS SPELLING FOR INTERACTIVE                             
*        AND CONVERT THEM TO "INTERACTIVE" FOR THE TITLE                        
*                                                                               
         CLC   HEAD1+1(10),=C'INTERACTIV'                                       
         BE    REGP4                                                            
         CLC   HEAD1+1(9),=C'INTERACTV' COVERS INTERACTV AND INTERACTVE         
         BE    REGP4                                                            
         CLC   HEAD1+1(6),=C'INTRAC' COVERS INTRACTIVE (AND OTHERS)             
         BE    REGP4                                                            
         B     REGP5                                                            
*                                                                               
REGP4    MVC   HEAD1+1(11),=C'INTERACTIVE'                                      
*                                                                               
REGP5    DS    0H                                                               
         MVI   RCSUBPRG,12        CONTRACT T/A REGISTER                         
         CLC   SVQPROG,=C'13'                                                   
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
*****                                                                           
         CLC   SVQPROG,=C'15'     AUTO SPACE RESERVATIONS                       
         BNE   REGPRT4                                                          
         MVI   RCSUBPRG,3                                                       
         CLI   PROFOPTS+3,C'C'                                                  
         BNE   REGPRT4                                                          
         MVI   RCSUBPRG,4                                                       
*****                                                                           
*                                                                               
REGPRT4  DS    0H                                                               
         MVI   RCWHATPR,X'01'     REGISTER MUST GO TO SYSPRINT1                 
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
SVRCPG   DS    CL2                 USED TO SAVE "REAL" RCPROG                   
         EJECT                                                                  
*                                  GRID SCHEDULE FORMAT                         
         SPACE 2                                                                
GRID     CSECT                                                                  
         NMOD1 0,GRID                                                           
         SPACE 2                                                                
         USING GRTABD,R3                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP12WRKD,R8                                                      
         SPACE 2                                                                
         CLI   MLIST,0                                                          
         BNE   GR3XX                                                            
*                                       SET MLIST                               
         MVC   MLIST(2),CSTART                                                  
         LA    R4,11                                                            
         LA    R2,MLIST                                                         
         SR    R1,R1                                                            
GR2XX    DS    0H                                                               
         CLC   0(2,R2),CEND                                                     
         BE    GR3XX                                                            
         CLI   1(R2),12                                                         
         BE    GR2B                                                             
         IC    R1,1(R2)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,3(R2)                                                         
         MVC   2(1,R2),0(R2)                                                    
         B     GR2D                                                             
GR2B     DS    0H                                                               
         IC    R1,0(R2)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,2(R2)                                                         
         MVI   3(R2),1                                                          
GR2D     DS    0H                                                               
         LA    R2,2(R2)                                                         
         BCT   R4,GR2XX                                                         
GR3XX    DS    0H                                                               
         L     R1,DMCB                                                          
         CLI   0(R1),C'B'                                                       
         BE    GR2XX               DO A BUY                                     
*                                                                               
*                                  DO TOTALS                                    
         MVI   SPACING,2                                                        
         BAS   RE,GRPRNT                                                        
         CLI   SVPRDNAM,0              SEE IF DOING ONE PRODUCT                 
         BNE   GR3A                                                             
         MVI   LNEED,16                                                         
         MVC   P+6(7),=C'PRODUCT'                                               
         MVC   P+15(3),PBUYKPRD                                                 
         L     RF,APRDTAB                                                       
         CLC   PBUYKPRD,0(RF)                                                   
         BE    *+12                                                             
         LA    RF,24(RF)                                                        
         B     *-14                                                             
         MVC   P+19(20),4(RF)                                                   
         BAS   RE,CKPRODG                                          L09          
         B     *+18        PRINT JUST PROD CODE                    L09          
         B     *+8         PRINT NOTHING                           L09          
         B     *+16        PRINT ALL                               L09          
         XC    P+1(40),P+1                                                      
         XC    P+19(20),P+19                                                    
         MVI   SPACING,2                                                        
         BAS   RE,GRPRNT                                                        
GR3A     DS    0H                                                               
         L     R3,AGRTAB                                                        
GR3B     DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    GREXT                                                            
         LA    R0,10                                                            
         LA    R1,GRLIN1                                                        
         MVC   0(L'GRLIN1,R1),SPACES                                            
         LA    R1,L'GRLIN1(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         LA    R4,9                DETERMINE LENGTH TO MOVE                     
*                                                                               
*                                                                               
         LA    R2,MLIST                                                         
GR3C     DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    GR3D                                                             
         LA    R2,2(R2)                                                         
         LA    R4,5(R4)                                                         
         B     GR3C                                                             
GR3D     DS    0H                                                               
         LA    R0,9                                                             
         LA    R1,GRLIN2                                                        
         LA    R2,GRLDC                                                         
GR4XX    DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)                                                    
         LA    R1,70(R1)                                                        
         LA    R2,70(R2)                                                        
         BCT   R0,GR4XX                                                         
*                                                                               
         CLI   QMEDIA,C'O'                                                      
         BNE   *+10                                                             
         MVC   GRLIN8(8),=C'RESERVED'                                           
         LA    R4,GRLIN1+11                                                     
         LA    R2,MLIST                                                         
GR4B     DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         MHI   R1,3                                                             
         LA    R1,MONTHS-3(R1)                                                  
         MVC   0(3,R4),0(R1)                                                    
         LA    R4,5(R4)                                                         
         LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   GR4B                                                             
*                                                                               
         CLI   GRSUBSQ,C'Y'        IF HAVE SUBSEQUENT                           
         BNE   GR4C                                                             
         AHI   R4,-6                                                            
         MVC   0(3,R4),1(R4)       MOVE MONTH BACK 1                            
         MVI   3(R4),C'+'       AND STATE HAVE SUBSEQUENT                       
         MVC   L'GRLIN1(5,R4),=C'LATER'                                         
*                                                                               
GR4C     DS    0H                                                               
         LA    R4,GRLIN1                                                        
         MVC   0(2,R4),=C'19'                                                   
**Y2K                                                                           
         CLI   MLIST,100     SEE IF YEAR HIGHER THAT 100 (2000)                 
         BL    *+10          LEAVE AS 19..                                      
         MVC   0(2,R4),=C'20'                                                   
         CLI   MLIST,200  SEE IF YEAR BETWEEN 100 AND 200 (2000-2099)           
         BL    *+10          LEAVE AS 20..                                      
         MVC   0(2,R4),=C'21' IF HIGHER THAN 200 MUST BE 2100'S                 
**Y2K                                                                           
         SR    R1,R1                                                            
         IC    R1,MLIST                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R4),DUB                                                      
         AHI   R2,-2                                                            
         CLC   0(1,R2),MLIST                                                    
         BE    GR4D                                                             
         MVI   4(R4),C'/'                                                       
         IC    R1,0(R2)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  5(2,R4),DUB                                                      
GR4D     DS    0H                                                               
*                             FORMAT IS $N,NNN.00/MONTH X N = $N,NNN.NN         
         MVC   W,SPACES                                                         
         LA    R4,W                                                             
         EDIT  (B4,GRAMT),(14,W),2,COMMAS=YES,FLOAT=$,ALIGN=LEFT,      X        
               MINUS=YES                                                        
*                                                                               
         AR    R4,R0                                                            
         BCTR  R4,R0                                                            
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(R4)                                                         
         MVC   0(8,R4),=C'/MONTH X'                                             
         LA    R0,12                                                            
         LA    R5,GRDATES                                                       
         SR    R7,R7                                                            
GR5XX    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    GR5B                                                             
         LA    R1,1                                                             
         TM    1(R5),X'01'                                                      
         BZ    *+8                                                              
         IC    R1,0(R5)                                                         
         AR    R7,R1                                                            
GR5B     DS    0H                                                               
         LA    R5,2(R5)                                                         
         BCT   R0,GR5XX                                                         
GR6XX    DS    0H                                                               
         LTR   R7,R7                                                            
         BZ    GR11                NO BUYS                                      
         LA    R4,9(R4)                                                         
         EDIT  (R7),(3,0(R4)),ALIGN=LEFT                                        
*                                                                               
         AR    R4,R0                                                            
         MVI   1(R4),C'='                                                       
         MVC   FULL,GRAMT                                                       
         M     R6,FULL                                                          
         LA    R4,3(R4)                                                         
         EDIT  (R7),(14,0(R4)),2,COMMAS=YES,FLOAT=$,ALIGN=LEFT,        X        
               MINUS=YES                                                        
*                                                                               
         AR    R4,R0                                                            
         LA    R0,W+1                                                           
         SR    R4,R0               R4 = LENGTH OF $ LINE - 1                    
         LA    R1,P+69                                                          
         SR    R1,R4                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),W                                                        
*                                                                               
         MVC   P+6(20),GRSPACE                                                  
         MVC   PSECOND+6(20),GRSPACE+20                                         
         MVI   SPACING,2                                                        
         MVI   LNEED,14                                                         
         BAS   RE,GRPRNT                                                        
         MVC   P+6(70),GRLIN1                                                   
         MVC   PSECOND+6(70),GRLIN2                                             
         BAS   RE,GRPRNT                                                        
         MVC   P+6(70),GRLIN3                                                   
         BAS   RE,GRPRNT                                                        
*                                                                               
         LA    R4,GRDATES                                                       
         LA    R5,GRLIN4+11                                                     
         LA    R6,12                                                            
GR6B     DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    GR9XX               NOTHING THIS MONTH                           
         TM    1(R4),X'01'         TEST MULTIPLE DATES                          
         BNZ   GR7XX               YES                                          
         SR    R0,R0               ONE DATE                                     
         IC    R0,0(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R5),DUB                                                      
         TM    1(R4),X'02'         TEST 'PRIOR'                                 
         BZ    GR6F                NO                                           
*                                  PREVIOUS MONTH                               
         LR    RF,R4                                                            
         LA    R0,GRDATES                                                       
         SR    RF,R0                                                            
         LA    RF,MLIST(RF)                                                     
         SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         BCTR  R0,R0                                                            
         LTR   R0,R0                                                            
         BP    *+8                                                              
         LA    R0,12                                                            
         LR    RF,R0                                                            
         MHI   RF,3                                                             
         LA    RF,MONTHS-3(RF)                                                  
         MVC   70(3,R5),0(RF)                                                   
*                                                                               
GR6F     DS    0H                                                               
*        TM    1(R4),X'04'         TEST 'CONFIRMED'                 L10         
*        BZ    GR9XX               NO                               L10         
*        CLI   QMEDIA,C'O'                                          L10         
*        BNE   GR9XX                                                L10         
*        MVC   4*70(2,R5),0(R5)    4 LINES DOWN                     L10         
*        MVC   70+4*70(3,R5),70(R5)                                 L10         
         B     GR9XX                                                            
*                                                                               
GR7XX    DS    0H                  MULT INSERTS                                 
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R5),DUB                                                      
         LR    RF,R5                                                            
         CLI   0(R5),C'0'                                                       
         BE    *+6                                                              
         BCTR  RF,R0                                                            
         MVI   0(RF),C'('                                                       
         MVI   2(R5),C')'                                                       
GR9XX    DS    0H                                                               
         LA    R4,2(R4)            NEXT MONTH                                   
         LA    R5,5(R5)                                                         
         BCT   R6,GR6B                                                          
*                                                                               
         MVC   P+6(70),GRLIN4                                                   
         MVC   PSECOND+6(70),GRLIN5                                             
*                                                                               
         BAS   RE,GRPRNT                                                        
         LA    R0,5                                                             
         LA    R4,GRLIN6                                                        
GR10     DS    0H                                                               
         MVC   P+6(70),0(R4)                                                    
         BAS   RE,GRPRNT                                                        
         LA    R4,70(R4)                                                        
         BCT   R0,GR10                                                          
*                                                                               
         BAS   RE,GRPRNT                                                        
GR11     DS    0H                                                               
         LA    R3,GRTABL(R3)       NEXT SPACE DESC                              
         B     GR3B                                                             
         SPACE 2                                                                
GR20     DS    0H                                                               
         TM    PBUYCNTL,X'80'                                                   
         BNZ   GREXT               BYPASS DELETES                               
         LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         MVC   PBYODTCN,DATCON                                                  
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
         MVI   PBYOCTL,0                                                        
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*                                                                               
         MVC   W(20),PBYOSPC1                                                   
         MVC   W+20(20),PBYOSPC2                                                
         DROP  R6                                                               
*                                                                               
*                                                                               
         L     R3,AGRTAB                                                        
GR21     DS    0H                                                               
         CLC   GRSPACE,W                                                        
         BNE   GR26                                                             
         CLC   GRAMT,GROSS                                                      
         BNE   GR26                                                             
GR22     DS    0H                  HAVE SPACE                                   
         LA    R2,MLIST                                                         
GR23     DS    0H                                                               
         CLC   PBUYKDAT(2),0(R2)                                                
         BL    GREXT                                                            
         BE    GR24                                                             
         LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   GR23                                                             
         AHI   R2,-2               ALL SUBSEQUENT IN LAST MONTH                 
         MVI   GRSUBSQ,C'Y'                                                     
*                                                                               
GR24     DS    0H                                                               
         CLI   PBDBFD,C'P'         TEST 'PRIOR'                                 
         BNE   GR25                                                             
         CLI   2(R2),0                                                          
         BE    *+8                                                              
         LA    R2,2(R2)                                                         
GR25     LA    R0,MLIST                                                         
         SR    R2,R0                                                            
         LA    R2,GRDATES(R2)                                                   
         CLI   0(R2),0                                                          
         BNE   GR25B                                                            
         MVC   0(1,R2),PBUYKDAT+2  SET DATE                                     
         CLI   PBDBFD,C'P'                                                      
         BNE   *+8                                                              
         OI    1(R2),X'02'         SET 'PRIOR'                                  
         CLI   PBDBFD,C'T'                                                      
         BE    *+8                                                              
         OI    1(R2),X'04'         SET 'CONFIRMED'                              
         B     GREXT                                                            
*                                                                               
GR25B    DS    0H                                                               
         LA    R1,1                                                             
         TM    1(R2),X'01'         TEST ALREADY MULTIPLE                        
         BZ    *+8                                                              
         IC    R1,0(R2)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,0(R2)                 NO OF BUYS IN MONTH                     
         OI    1(R2),X'01'         SET MULTIPLE                                 
         B     GREXT                                                            
GR26     DS    0H                                                               
         CLI   0(R3),0                                                          
         BNE   GR27                                                             
         MVC   GRSPACE,W                                                        
         MVC   GRAMT,GROSS                                                      
         XC    GRDATES(25),GRDATES CLEAR DATES + NEXT BYTE                      
         B     GR22                                                             
GR27     DS    0H                                                               
         LA    R3,GRTABL(R3)                                                    
         B     GR21                                                             
         SPACE 3                                                                
GRPRNT   NTR1                                                                   
         CLI   FORCEHED,C'P'                                                    
         BE    GRP2                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,LNEED                                                         
         LA    RE,1(RE,RF)                                                      
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    GRP4                                                             
*                                  NEW PAGE                                     
GRP2     DS    0H                                                               
         GOTO1 ACONEOP                                                          
*                                                                               
         EDIT  (B2,PAGE),(2,MH5+74)                                             
*                                                                               
         LA    R0,14                                                            
         LA    R1,HEAD1                                                         
         LA    R2,MH1                                                           
         MVC   0(80,R1),0(R2)                                                   
         LA    R1,132(R1)                                                       
         LA    R2,80(R2)                                                        
         BCT   R0,*-14                                                          
*                                                                               
GRP4     DS    0H                                                               
         GOTO1 AFREPORT                                                         
*                                                                               
GREXT    DS    0H                                                               
         MVI   LNEED,0                                                          
         XIT1                                                                   
         SPACE 3                                                                
*                                                                   L02         
*                                                                   L02         
CKPRODG  CLC   QPROG,=C'12'                                         L06         
         LA    RF,PROFOPTS+6     12 PRODUCT OPTION                  L06         
         BE    CKPROCG                                              L06         
         CLC   QPROG,=C'13'      OR CONTRACT TURNAROUND             L14         
         BE    CKPROCG                                              L14         
         CLC   QPROG,=C'15'                                         L06         
         LA    RF,PROFOPTS+11    15 PRODUCT OPTION                  L06         
         BNE   8(RE)         NORMAL PROCESSING                      L06         
CKPROCG  CLI   0(RF),C'N'  NORMAL PROCESSING                        L06         
         BE    8(RE)                                                L06         
         CLI   0(RF),C'Y'  PRINT PRODUCT CODE ONLY                  L06         
         BER   RE                                                   L06         
         CLI   0(RF),C'B'  DO NOT PRINT CODE OR DESCRIPTION         L06         
         BE    4(RE)                                                L06         
         B     8(RE)                                                L06         
*                                                                   L06         
         LTORG                                                                  
         SPACE 2                                                                
GRLDC    DC    C'         +++++++++++++++++++++++++++++++++++++++++++++X        
               ++++++++++++++++'                                                
         DC    C'         +    +    +    +    +    +    +    +    +    X        
               +    +    +    +'                                                
         DC    C'ORDERED  +    +    +    +    +    +    +    +    +    X        
               +    +    +    +'                                                
         DC    C'         +    +    +    +    +    +    +    +    +    X        
               +    +    +    +'                                                
         DC    C'         +++++++++++++++++++++++++++++++++++++++++++++X        
               ++++++++++++++++'                                                
         DC    C'         +    +    +    +    +    +    +    +    +    X        
               +    +    +    +'                                                
         DC    C'         +    +    +    +    +    +    +    +    +    X        
               +    +    +    +'                                                
         DC    C'         +    +    +    +    +    +    +    +    +    X        
               +    +    +    +'                                                
         DC    C'         +++++++++++++++++++++++++++++++++++++++++++++X        
               ++++++++++++++++'                                                
         EJECT                                                                  
*                                  GET PUB AND FORMAT EDITION INFO              
         SPACE 2                                                                
CSPUB    CSECT                                                                  
         NMOD1 0,CSPUB                                                          
         SPACE 2                                                                
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC,R9                                                    
         USING PP12WRKD,R8                                                      
*                                                                               
         L     R6,ACONIO1          A(PCONREC)                                   
         USING PCONREC,R6                                                       
         SPACE 2                                                                
         CLC   PCONKPUB(6),PUBKPUB                                              
         BE    CSP20                                                            
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(1),PCONKEY+2                                                 
         MVC   KEY+1(6),PCONKPUB                                                
         MVC   KEY+7(2),PCONKAGY                                                
         MVI   KEY+9,X'81'                                                      
         BAS   RE,CSHIPUB                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    CSP4                                                             
         CLC   KEY+7(2),=C'ZZ'                                                  
         BE    CSP4                                                             
         MVC   KEYSAVE+7(2),=C'ZZ'                                              
         MVC   KEY,KEYSAVE                                                      
         BAS   RE,CSHIPUB                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    CSP4                                                             
         DC    H'0'                                                             
CSP4     DS    0H                                                               
         BAS   RE,CSGTPUB                                                       
         MVC   PUBKED,KEY+6        SET EDITION CODE                             
         MVC   KEY(64),WORK                                                     
CSP20     DS    0H                                                              
         MVC   W,SPACES                                                         
         LA    R2,W                                                             
         CLI   PUBKZON,0                                                        
         BE    CSP22                                                            
         UNPK  DUB(3),PUBKZON(2)                                                
         MVC   0(2,R2),DUB                                                      
         MVI   2(R2),C'-'                                                       
         MVC   4(20,R2),PUBZNAME                                                
         LA    R2,W+30+4                                                        
*                                                                               
         CLC   QPROG,=C'13'        CONTRACT TURNAROUND            L14           
         BE    CSP20C                                             L14           
         CLC   QPROG,=C'12'       ONLY FOR CONTRACTS              L09           
         BNE   *+8                    CASCADE THRU                  L09         
CSP20C   CLI   QMEDIA,C'O'           AND OUTDOOR                    L09         
         BNE   *+8                    CASCADE THRU                  L09         
         CLI   PUBKED,0              AND NO EDITON                  L09         
         BNE   *+8                    CASCADE THRU                  L09         
         CLI   PROFOPTS+11,C'Y'                                     L09         
         BNE   CSP22                                                L09         
         MVC   0(7,R2),=C'POSTERS'                                  L09         
         B     CSP22DZ                                              L09         
CSP22     DS    0H                                                              
         GOTO1 PUBEDIT,DMCB,PUBKPUB,(C'E',0(R2))                                
*                                                                               
CSP22DZ  CLC   W(60),SPACES                                                     
         BE    CSPX                                                             
         LA    R4,W+29                                                          
CSP23    DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BH    CSP24                                                            
         CLI   30(R4),C' '                                                      
         BH    CSP24                                                            
         BCT   R4,CSP23                                                         
*                                                                               
CSP24    DS    0H                                                               
         LA    R0,W                                                             
         SR    R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   W+60(0),DASHES1                                                  
*                                                                               
CSPX     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
CSHIPUB  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,PUBDIR,KEY,KEY                               
*                                                                               
         B     CSPDMCK                                                          
*                                                                               
CSGTPUB  DS    0H                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,PUBREC,DMWORK                 
*                                                                               
CSPDMCK  DS    0H                                                               
         LR    RE,R0                                                            
         TM    DMCB+8,X'FF'                                                     
         BZR   RE                                                               
         DC    H'0'                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'COLLECT 68 ELEMENTS               '                             
*                                                                   L03         
*                                                                   L03         
PP12468  CSECT                                                      L03         
         PRINT NOGEN                                                L03         
         NMOD1 0,PP12468                                            L03         
         SPACE 2                                                    L03         
*******                                                                         
** NOTE THAT DELETED BUYS NEVER GET HERE                                        
*******                                                                         
******                                                              L03         
** ON RETURN         WORD 1   BYTE 0                                L03         
*                                1-3  ADDRESS OF TABLE OF COMMENT   L03         
******                                                              L03         
         USING PPWORKD,RA                                           L03         
         USING PP12WRKD,R8                                          L03         
         USING PPFILED,RC,R9                                        L03         
         LR    RC,R9           INITIALIZE RC                        L03         
         S     RC,=F'4096'                                          L03         
         SPACE 2                                                    L03         
         ST    R1,SAVER1        SAVE POINTER                        L03         
         LA    R4,BIGTABLE                                          L03         
         XC    0(35,R4),0(R4)                                       L03         
         ST    R4,ANXTCOM       ADDRESS OF NEXT COMMENT             L03         
         CLI   PROFOPTS+9,C'Y'  Y M/B PRESENT TO PRINT POSITION INSTL03         
         BNE   BCX                                                  L03         
*                                                                               
BC1      DS    0H                                                               
         LA    R2,PBUYREC+33                                        L03         
         MVI   ELCODE1,X'68'                                        L03         
*                                                                   L03         
BC4      BAS   RE,NXTEL                                             L03         
         BNE   BCX       NO 68 ELEMENTS                             L03         
         CLI   1(R2),2                                              L03         
         BE    BC4                                                  L03         
*                                                                   L03         
POS1X    BC    0,POS1XX   PRINT HEADING ONCE                        L03         
         L     RF,ANXTCOM                                           L03         
         XC    0(35,RF),0(RF)                                       L03         
         MVC   0(29,RF),=C'*** POSITION INSTRUCTIONS ***'           L03         
         LA    RF,35(RF)                                            L03         
         ST    RF,ANXTCOM                                           L03         
         OI    POS1X+1,X'F0'                                        L03         
POS1XX   DS    0H                                                   L03         
*                                                                   L03         
BC5      CLC   2(4,R2),=C'COM='                                     L03         
         BE    BC8                 STANDARD COMMENT                 L03         
         L     R4,ANXTCOM                                           L03         
         XC    0(35,R4),0(R4)                                       L03         
         MVC   SAVEP,SPACES                                                     
         ZIC   R5,1(R2)                                             L03         
         AHI   R5,-3                                                L03         
         EX    R5,*+8                                               L03         
         B     *+10                                                 L03         
*                                                                   L03         
         MVC   SAVEP(0),2(R2)                                       L03         
         L     R3,ANXTCOM                                                       
         MVC   0(35,R3),SAVEP                                                   
         LA    R3,35(R3)                                                        
         CLC   SAVEP+35(35),SPACES                                              
         BE    BC5H                                                             
         AHI   R3,-35              BACK UP R3                                   
         LA    R5,1(R5)            ADJUST R5 FOR CHOPPER                        
         L     RF,ACHOP                                                         
         GOTO1 (RF),DMCB,((R5),SAVEP),(35,0(R3)),2                              
         OC    DMCB+8(4),DMCB+8                                                 
         BNE   *+6                                                              
         DC    H'0'                CHOPPER ERROR                                
         LA    R3,35(R3)                                                        
         CLC   0(35,R3),SPACES     SEE IF I USED 2 LINES                        
         BE    BC5H                NO                                           
         LA    R3,35(R3)           YES                                          
*                                                                               
BC5H     ST    R3,ANXTCOM                                                       
         B     BC4                                                              
*                                                                   L03         
BC8      DS    0H                                                   L03         
         MVC   SVELCOD,ELCODE1     SAVE ELCODE1                     L03         
         ST    R2,SAVER2           SAVE BUYREC'S R2                 L03         
         MVC   SVBUYKEY,KEY                                         L03         
         XC    KEY,KEY                                              L03         
         MVC   KEY(3),QAGENCY                                       L03         
         MVI   KEY+3,X'40'                                          L03         
         ZIC   R5,1(R2)                                             L03         
         AHI   R5,-6                                                L03         
         LTR   R5,R5                                                L03         
         BZ    BC8X                NO NUMBER                        L03         
         MVC   WORK(10),SPACES                                      L03         
         LA    R7,WORK+6                                            L03         
         SR    R7,R5                                                L03         
         BCTR  R5,0                                                 L03         
         EX    R5,*+8                                               L03         
         B     *+10                                                 L03         
*                                                                   L03         
         MVC   0(0,R7),6(R2)                                        L03         
*                                                                   L03         
         MVC   KEY+4(6),WORK                                        L03         
         GOTO1 DATAMGR,DMCB,DMREAD,PRTDIR,KEY,KEY                   L03         
         CLI   DMCB+8,0                                             L03         
         BE    *+6                                                  L03         
         DC    H'0'                STND COMMENT NOT FOUND           L03         
         GOTO1 (RF),(R1),GETREC,PRTFILE,KEY+27,ACOMREC,DMWORK       L03         
         L     R2,ACOMREC                                           L03         
         LA    R2,33(R2)                                            L03         
         MVI   ELCODE1,X'40'                                        L03         
         CLI   0(R2),X'40'                                          L03         
         BE    BC8D                                                 L03         
*                                                                   L03         
BC8C     BAS   RE,NXTEL                                             L03         
         BNE   BC8X                                                 L03         
*                                                                   L03         
BC8D     DS    0H                                                   L03         
         CLC   2(5,R2),=C'SHIP='                                    L03         
         BE    BC8C                                                 L03         
         CLC   2(6,R2),=C'LABEL='                                   L03         
         BE    BC8C                                                 L03         
         CLC   2(4,R2),=C'MAT='                                     L03         
         BE    BC8C                                                 L03         
         CLC   2(3,R2),=C'RC='                                      L03         
         BE    BC8C                                                 L03         
         LA    R4,2(R2)                                             L03         
         LR    R6,R4                                                L03         
         ZIC   R5,1(R2)                                             L03         
         AHI   R5,-3                                                L03         
         BM    BC8C                                                 L03         
*                                                                   L03         
         CLI   0(R4),C'+'                                           L03         
         BNE   *+8                                                  L03         
         LA    R6,2(R6)                                             L03         
         SR    RF,RF                                                L03         
         CLC   0(3,R6),=C'DC='     ONLY DC= COMMNTS                 L03         
         BE   BC8C                                                  L03         
*                                                                   L03         
         CLI   0(R4),C'+'                                           L03         
         BNE   BC8F                                                 L03         
         CLI   1(R4),C'1'          ONLY PACK IF NUMERIC             L03         
         BL    BC8F                ELSE TREAT AS REGULAR LINE       L03         
         PACK  DUB,1(1,R4)                                          L03         
         CVB   R7,DUB                                               L03         
         L     R3,ANXTCOM                                           L03         
BC8E     XC    0(35,R3),0(R3)      CAN'T USE SPACES                 L03         
         LA    R3,35(R3)                                            L03         
         BCT   R7,BC8E                                              L03         
         ST    R3,ANXTCOM                                           L03         
         LA    R4,2(R4)                                             L03         
         AHI   R5,-2                                                L03         
*                                                                   L03         
BC8F     DS    0H                                                   L03         
         LTR   R5,R5                                                L03         
         BM    BC8C                                                 L03         
         MVC   SAVEP,SPACES                                         L03         
         EX    R5,*+8                                               L03         
         B     *+10                                                 L03         
         MVC   SAVEP(0),0(R4)                                       L03         
         L     R3,ANXTCOM                                           L03         
         MVC   0(35,R3),SAVEP                                       L03         
         LA    R3,35(R3)                                            L03         
         CLC   SAVEP+35(35),SPACES                                  L03         
         BE    BC8H                                                 L03         
         AHI   R3,-35              BACK UP R3                       L03         
         LA    R5,1(R5)            ADJUST R5 FOR CHOPPER            L03         
         L     RF,ACHOP                                             L03         
         GOTO1 (RF),DMCB,((R5),SAVEP),(35,0(R3)),3                  L03         
         OC    DMCB+8(4),DMCB+8                                     L03         
         BNE   *+6                                                  L03         
         DC    H'0'                CHOPPER ERROR                    L03         
         LA    R3,35(R3)                                            L03         
         CLC   0(35,R3),SPACES     SEE IF I USED 2 LINES            L03         
         BE    BC8H                NO                               L03         
         LA    R3,35(R3)           YES                              L03         
         CLC   0(35,R3),SPACES     SEE IF I USED 3 LINES                        
         BE    BC8H                NO                                           
         LA    R3,35(R3)                                                        
*                                                                               
BC8H     ST    R3,ANXTCOM                                           L03         
         MVC   SAVEP,SPACES                                         L03         
         B     BC8C                                                 L03         
*                                                                   L03         
BC8X     MVC   ELCODE1,SVELCOD     RESTORE ELCODE1                  L03         
         MVC   KEY,SVBUYKEY                                         L03         
         GOTO1 DATAMGR,DMCB,DMRDHI,PRTDIR,KEY,KEY                   L03         
         CLI   DMCB+8,0                                             L03         
         BE    *+6                                                  L03         
         DC    H'0'                                                 L03         
         L     R2,SAVER2           RESTORE BUYREC'S R2              L03         
         B     BC4                                                  L03         
*                                                                   L03         
*                                                                   L03         
BCX      L     R3,ANXTCOM                                           L03         
         MVI   0(R3),X'FF'         SET END OF TABLE                 L03         
         MVI   POS1X+1,00                                           L03         
         LA    R3,BIGTABLE                                          L03         
         L     RF,SAVER1                                            L03         
         ST    R3,0(RF)            INITIALIZE ANXTCOM TO FIRST      L03         
         XIT1                                                       L03         
*                                                                   L03         
NXTEL    DS    0H                                                   L03         
         SR    R0,R0                                                L03         
         IC    R0,1(R2)                                             L03         
         AR    R2,R0                                                L03         
         CLI   0(R2),0                                              L03         
         BE    NXTELX                                               L03         
         CLC   ELCODE1,0(R2)                                        L03         
         BER   RE                                                   L03         
         B     NXTEL+2                                              L03         
NXTELX   LTR   R2,R2                                                L03         
         BR    RE                                                   L03         
         SPACE 3                                                    L03         
SVBUYKEY DS    CL32                                                 L03         
SAVER2   DS    F                                                    L03         
SAVER1   DS    F                                                    L03         
ANXTCOM  DS    A                                                    L03         
SVELCOD  DS    CL1                                                  L03         
*                                                                   L03         
         SPACE 3                                                    L03         
         EJECT                                                      L03         
         LTORG                                                      L03         
*SMYE*BIGTABLE DS    3500C    ROOM FOR 100 LINES                                
BIGTABLE DS    7000C    ROOM FOR 200 LINES      (04/07/04)                      
         DS    CL1                                                              
         SPACE 3                                                                
*                                                                               
         EJECT                                                                  
         TITLE 'INTERFACE TO REPORT FOR FAXING    '                             
*                          SPECIAL REPORT FOR FAXING                            
FREPORT  CSECT                                                                  
         NMOD1 0,FREPORT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         USING PP12WRKD,R8                                                      
         LA    R7,FREPWORK                                                      
         USING PPREPD,R7                                                        
*                                                                               
         L     R6,ACONIO1          A(PCONREC                                    
         USING PCONREC,R6                                                       
*                                                                               
         CLI   FAXOPT,C'Y'            SEE IF FAXING CONTRACTS                   
         BE    PC6                                                              
PC4      MVI   RCWHATPR,X'01'                                                   
         GOTO1 REPORT       NO-JUST GOTO NORMAL REPORT AND EXIT                 
         B     PCX                                                              
*                   CONTROL OF HEADLINE PRINTING                                
         SPACE 3                                                                
PC6      DS    0H                                                               
         CLC   MYFAX,SPACES       SEE IF I HAVE A FAX NUMBER                    
         BE    PC4                NO FAX NUMBER - SEND OF AGY QUE               
*                                                                               
         LA    R4,HEAD1                                                         
         LA    R5,MYHEAD1                                                       
         LA    R3,14                                                            
PC6C     MVC   0(132,R5),0(R4)      MOVE TO MYHEADS                             
         MVC   0(132,R4),SPACES     CLEAR HEADLINES                             
         LA    R4,132(R4)                                                       
         LA    R5,132(R5)                                                       
         BCT   R3,PC6C                                                          
*                                                                               
PC7      CLI   FORCEHED,C'N'                                                    
         BE    PC8                                                              
         MVI   FORCEHED,C'N'                                                    
         B     PC10                                                             
         SPACE 2                                                                
PC8      CLC   LINE,MAXLINES                                                    
         BL    PC12                                                             
         SPACE 2                                                                
PC10     MVI   FORCEMID,C'Y'                                                    
*                                                                               
PC10D    DS    0H                                                               
         MVI   FORCEHED,C'N'     TRICK "REAL" REPORT                            
*                                SO IT WON'T PRINT HEADLINES                    
         CLI   FTIME,C'Y'                                                       
         BE    PC10D5                                                           
         B     PC10P                                                            
*                                                                               
PC10D5   DS    0H                                                               
*                            NOTE - NO SKIP TO CHANNEL 1 NEEDED                 
*                            IF FAXING  (/PAGE WILL CAUSE NEW PAGE)             
         MVI   FTIME,C'N'                                                       
         MVC   FRSVFAX,MYFAX  SAVE THIS FAX NUMBER SO I CAN CHECK FOR           
*                             A CHANGE OF FAX NUMBER                            
         MVC   MYPLINE,SPACES                                                   
*******  MVC   WUORIG,AGYORIG        NO-OPED                                    
*                                                                               
         MVC   WU9FB(5),=C'*HDR*'                                               
         MVC   WUDESTID(4),=C'FAXL'                                             
         MVC   WUDESTID+5(12),MYFAX                                             
*                                                                               
         CLC   MYFAX(5),=C'MB=62'       CHECK FOR EASY-LINK MAIL BOX            
         BNE   PC10F                    (THEY ALL MUST BEGIN WITH 62)           
         MVC   WUDESTID(17),SPACES                                              
         MVC   WUDESTID(8),MYFAX+3      MOVE MAIL BOX NUMBER                    
*                                                                               
         B     PC10F5                                                           
*                                                                               
PC10F    CLC   MYFAX(3),=C'FX='    CHECK FOR CONTROL FILE FAX CODE              
         BNE   PC10F5                                                           
         MVC   WUDESTID(17),SPACES                                              
         MVC   WUDESTID(6),=C'FXKEY='                                           
         MVC   WUDESTID+6(9),MYFAX+3                                            
         B     PC10F5                                                           
*                                                                               
*****    BNE   PC10F5                                                           
******** GOTO1 =A(GETFAX),DMCB,RR=Y          NOW READ IN CONHEAD                
*****                                                                           
*****    CLC   TOFAX,SPACES        FAX RECORD NOT FOUND                         
*****    BE    PC10F5              LEAVE FX=XXXXX IN WUDESTID                   
*****    MVC   WUDESTID+5(16),TOFAX                                             
*****    B     PC10F5                                                           
*                                                                               
PC10F5   MVC   WUBILL(1),QMEDIA                                                 
         MVC   WUBILL+1(3),QCLIENT                                              
         MVC   WUBILL+4(3),QPRODUCT                                             
*                                                                               
         MVC   WUDEST(12),MYFAX                                                 
*                                                                               
         OC    WUBILL(7),SPACES                                                 
         MVI   WUEND,C'.'                                                       
         MVC   WUEND+1(7),WUEND                                                 
         MVC   SVP,P                                                            
         MVC   SVP2,PSECOND                                                     
         MVC   SVSPACE,SPACING                                                  
         MVC   PSECOND,SPACES      JUST IN CASE                                 
         MVI   SPACING,1                                                        
         MVC   P,MYPLINE                                                        
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
         MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(5),=C'++DDS'                                             
         MVC   MYPLINE+6(8),=C'PPP12TRN'   CONTRACTS                            
         CLC   QPROG,=C'12'                                                     
         BE    PC10K                                                            
         MVC   MYPLINE+6(8),=C'PPP15TRN'   AUTO SPACE RESERVATION               
         CLC   QPROG,=C'15'                                                     
         BE    PC10K                                                            
         MVC   MYPLINE+6(8),=C'PPP13TRN'   MUST BE CONTRACT T/A                 
*                                                                               
PC10K    MVC   PPEDMED,QMEDIA                                                   
         MVC   PPEDCLT,QCLIENT                                                  
         MVC   PPEDPRD,QPRODUCT                                                 
         MVC   PPEDCON,QEST             REALLY CONTRACT NUMBER                  
         MVC   PPEDRPT,QPROG            SET REPORT NUMBER                       
*                                                                               
         GOTO1 PUBEDIT,DMCB,PCONKPUB,PPEDPUB                                    
*                                                                               
         MVC   PPEDREQ,QUESTOR     (3 BYTES)                                    
*                                                                               
         MVC   P,MYPLINE                                                        
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
PC10L    DS    0H                                                               
*                            THIS SHOULD SUPPRESS EASYLINK HEADER               
*                            MESSAGE                                            
         MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(10),=C'/FORM/PAGE'                                       
*                                                                               
         MVC   P,MYPLINE                                                        
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
         B     PC10X              PC10X RESTORES P LINES                        
*                                 AND PRINTS THEM                               
PC10P    DS    0H                                                               
         MVC   SVP,P                SAVE P AND PSECOND                          
         MVC   SVP2,PSECOND                                                     
         MVC   SVSPACE,SPACING                                                  
         MVC   PSECOND,SPACES      JUST IN CASE                                 
         MVI   SPACING,1                                                        
*                                                                               
         CLC   FRSVFAX,MYFAX        CHECK FOR NEW FAX NUMBER                    
         BE    PC10P8                                                           
         MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(26),=C'*** END OF DDS MESSAGE ***'                       
         MVC   P,MYPLINE                                                        
         MVI   LINE,1         SO THIS WILL NEVER CAUSE A NEW PAGE               
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
         MVC   FRSVFAX,MYFAX             SAVE FAX NUMBER                        
*                                                                               
         MVC   MYPLINE,SPACES                                                   
*******  MVC   WUORIG,AGYORIG            NO-OPED                                
*                                                                               
         MVC   WU9FB(5),=C'*HDR*'                                               
*                                                                               
         MVC   WUDESTID(4),=C'FAXL'                                             
         MVC   WUDESTID+5(12),MYFAX                                             
*                                                                               
         CLC   MYFAX(5),=C'MB=62'       CHECK FOR EASY LINK MAIL BOX            
         BNE   PCP02                    (THEY ALL MIST BEGIN WITH 62)           
         MVC   WUDESTID(17),SPACES                                              
         MVC   WUDESTID(8),MYFAX+3                                              
*                                                                               
         B     PCP05                                                            
*                                                                               
PCP02    CLC   MYFAX(3),=C'FX='    CHECK FOR CONTROL FILE FAX CODE              
         BNE   PCP05                                                            
         MVC   WUDESTID(17),SPACES                                              
         MVC   WUDESTID(6),=C'FXKYL='                                           
         MVC   WUDESTID+6(9),MYFAX+3                                            
         B     PCP05                                                            
*****                                                                           
*****    BNE   PCP05                                                            
******** GOTO1 =A(GETFAX),DMCB,RR=Y          NOW READ IN CONHEAD                
*****    CLC   TOFAX,SPACES        FAX RECORD NOT FOUND                         
*****    BE    PCP05               LEAVE FX=XXXXX IN WUDESTID                   
*****    MVC   WUDESTID+5(16),TOFAX                                             
*****    B     PCP05                                                            
*                                                                               
PCP05    MVC   WUBILL(1),QMEDIA                                                 
         MVC   WUBILL+1(3),QCLIENT                                              
         MVC   WUBILL+4(3),QPRODUCT                                             
*                                                                               
         MVC   WUDEST(12),MYFAX                                                 
*                                                                               
         OC    WUBILL(7),SPACES                                                 
         MVI   WUEND,C'.'                                                       
         MVC   WUEND+1(7),WUEND                                                 
*                                                                               
         MVC   P,MYPLINE                                                        
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
         MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(5),=C'++DDS'                                             
         MVC   MYPLINE+6(8),=C'PPP12TRN'   CONTRACTS                            
         CLC   QPROG,=C'12'                                                     
         BE    PCP08                                                            
         MVC   MYPLINE+6(8),=C'PPP15TRN'   AUTO SPACE RESERVATION               
         CLC   QPROG,=C'15'                                                     
         BE    PCP08                                                            
         MVC   MYPLINE+6(8),=C'PPP13TRN'   MUST BE CONTRACT T/A                 
*                                                                               
PCP08    DS    0H                                                               
         MVC   PPEDMED,QMEDIA                                                   
         MVC   PPEDCLT,QCLIENT                                                  
         MVC   PPEDPRD,QPRODUCT                                                 
         MVC   PPEDEST,QEST               REALLY CONTRACT NUMBER                
*                                                                               
         GOTO1 PUBEDIT,DMCB,PCONKPUB,PPEDPUB                                    
*                                                                               
         MVC   PPEDREQ,QUESTOR     (3 BYTES)                                    
*                                                                               
         MVC   P,MYPLINE                                                        
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
         MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(10),=C'/FORM/PAGE'                                       
         MVC   P,MYPLINE                                                        
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
         B     PC10X                                                            
*                                                                               
*                                                                               
PC10P8   MVC   MYPLINE,SPACES                                                   
         MVC   MYPLINE(5),=C'/PAGE'                                             
         MVC   P,MYPLINE                                                        
         MVI   LINE,1   SO THIS WON'T CAUSE REPORT TO GO TO NEW PAGE            
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT             GO TO "REAL" REPORT                           
*                                                                               
PC10X    DS    0H                 I MUST PRINT MY OWN HEADLINES                 
         MVI   LINE,3      NOT REALLY SURE WHY THIS WORKS                       
*                          BUT IT IS NEEDED FOR THE FAXED CONTRACT              
*                          TO LOOK LIKE THE NON FAXED CONTRACT                  
         LA    R4,MYHEAD1                                                       
         LA    R3,14                                                            
PC11     MVC   P,0(R4)                                                          
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT                                                           
         LA    R4,132(R4)                                                       
         BCT   R3,PC11                                                          
*                                                                               
         MVC   P,SVP              RESTORE "REAL' P AND PSECOND                  
         MVC   PSECOND,SVP2                                                     
         MVC   SPACING,SVSPACE    RESTORE "REAL' SPACING                        
*                              SINCE ALL THE ABOVE ARE NOT "REAL"               
*                              LINES                                            
*                                                                               
PC12     DS    0H                                                               
         MVI   RCWHATPR,X'02'     SEND TO FAX QUE                               
         GOTO1 REPORT          FINALLY GO THE "REAL" REPORT                     
*                              WITH "REAL" PRINT LINES                          
PCX      DS    0H                                                               
         XMOD1 1                                                                
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*        FREPORT LTORG                                                          
         LTORG                                                                  
*                                                                               
FRSVFAX  DS    CL12                                                             
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
FREPWORK DS    2500C                                                            
*                   DSECT FOR THIS MODULE                                       
         SPACE 3                                                                
PPREPD   DSECT                                                                  
REPDUB   DS    D                                                                
REPWORK  DS    CL40                                                             
CONTROL  DS    CL4                                                              
PROGSW   DS    CL1                                                              
**FAX                                                                           
MYPLINE  DS    CL132                                                            
*                                                                               
         ORG   MYPLINE                                                          
WUORIG   DS    CL4             WAS AGYORIG                                      
WU9FB    DS    CL5             OLD - WAS USED BY GRAFNET (/9FB)                 
WUDESTID DS    CL24            C'FAXL AAA-NNN-NNNN'  (FAX NUMBER)               
         DS    CL5             SPARE                                            
WUDEST   DS    CL16            FORMATTED DESTINATION  (FAX NUMBER)              
*                                                                               
WUBILL   DS    CL20            QMED/QCLIENT/QPRD                                
WUEND    DS    CL8                                                              
*                                                                               
         ORG   MYPLINE+15                                                       
       ++INCLUDE PPEDICT                                                        
         ORG                                                                    
MYHEAD1  DS    CL132                                                            
MYHEAD2  DS    CL132                                                            
MYHEAD3  DS    CL132                                                            
MYHEAD4  DS    CL132                                                            
MYHEAD5  DS    CL132                                                            
MYHEAD6  DS    CL132                                                            
MYHEAD7  DS    CL132                                                            
MYHEAD8  DS    CL132                                                            
MYHEAD9  DS    CL132                                                            
MYHEAD10 DS    CL132                                                            
MYHEAD11 DS    CL132                                                            
MYHEAD12 DS    CL132                                                            
MYHEAD13 DS    CL132                                                            
MYHEAD14 DS    CL132                                                            
*                                                                               
SVP      DS    CL132             USED TO SVAVE "REAL" PRINT LINES               
SVP2     DS    CL132                                                            
SVSPACE  DS    XL1               SAVE "REAL" SPACING                            
         SPACE 2                                                                
*                                                                               
*                                                                               
*                                                                               
         TITLE 'DSECTS'                                                         
PP12WRKD DSECT                                                                  
IOAREA   DS    A                                                                
APPUNBUY DS    A                                                                
ARTLOOK  DS    A                                                                
APRDTAB  DS    A                                                                
APRDTABX DS    A                                                                
ACONEOP  DS    A                                                                
ACONTAB  DS    A                                                                
ACONTABX DS    A                                                                
ACONREG  DS    A                                                                
APRTREG  DS    A                                                                
ACHOP    DS    A                                                                
AGRTAB   DS    A                                                                
AGRID    DS    A                                                                
AMHSUBS  DS    A                                                                
ACSPUB   DS    A                                                                
AFNDEDTS DS    A                                                                
ACONSCHD DS    A                                                                
ACONSPRT DS    A                                                                
ACONHEAD DS    A                                                                
ASORTSCH DS    A                                                                
ASORTTAB DS    A                                                                
ASORTPRT DS    A                                                                
AEDTLIST DS    A                                                                
ADATVAL  DS    A                                                                
AFREPORT DS    A                                                                
APPGETCG DS    A                                                                
ASTDCOM  DS    A                                                                
ARTLKELS DS    A         26 SO FAR                                              
         DS    4A        SPARE ACONS                                            
*              END OF ACONS                                                     
*                                                                               
ANEXTSRT DS    A                                                                
AFSTCON  DS    A                                                                
ANEXTCON DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
*                                                                               
ACONIO1  DS    A                   ADDRESS OF PCONREC                           
*                                                                               
TOTADDR  DS    A                                                                
ANXTEDT  DS    A                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ACCUMULATOR FORMAT                                                            
* 1. BEFORE COL CONV UNITS (PL8)   2. DOLLARS (PL8)  3. INSERTS (PL8)           
* 4. AFTER COL CONV UNITS  (PL8)   5. DOLLARS (PL8)  6. INSERTS (PL8)           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRDTOTS  DS    6PL8                                                             
CLTTOTS  DS    6PL8                                                             
MASTOTS  DS    6PL8                CONTRACT TOTALS                              
EDTTOTS  DS    6PL8                                                             
CONTOTS  DS    6PL8                                                             
*                                                                               
ACNEQ    EQU   6                   NUMBER OF ACCUMULATORS PER TOTAL             
ACLEQ    EQU   8                   LENGTH OF EACH ACCUMULATOR (PACKED)          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SORTCNT  DS    F                   NUMBER OF BUYS TO SORT                       
TOTEDS   DS    PL2                 EDITION COUNTER                              
*                                                                               
SOONSW   DS    CL1                                                              
         DS    CL99         SPARE                                               
LASTCKEY DS    CL8                                                              
WORKCLT  DS    CL8                                                              
WORKCNM  DS    CL20                                                             
ELCODE1  DS    C                                                                
*                                                                               
ADRTYP   DS    CL1          TYPE OF ADDRESS REC - PAY, TRAFFIC, ETC.            
CLTDATA  DS    0CL7         USED TO PASS KEY INFO TO PPGETADR MODULE            
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
ADRCLT   DS    CL3                                                              
ADROFF   DS    CL1                                                              
ININD    DS    CL1                                                              
SKIPSW   DS    CL1                                                              
*                                NEXT TO FIELDS EXTRACTED FROM COMMENT          
SCONVDTE DS    CL3                 COLUMN CONVERSION DATE                       
SCONVFAC DS    F                   COLUMN CONVERSION FACTOR                     
SCONVIND DS    CL1                 C'-' MEANS NEW TO OLD                        
*                                  C'*' MEANS OLD TO NEW                        
FULLSW   DS    CL1         REGISTER FULL SWITCH                                 
ONEPRD   DS    CL1                 SET TO 'Y' IF DOING ONE PRD                  
*                                                                               
MYFAX    DS    CL12                                                             
TOFAX    DS    CL16                FROM CONTROL FILE FAX RECORD                 
FTIME    DS    CL1                                                              
*                                  IN SCHEDULES                                 
LASTMED  DS    CL1                 MEDIA OF LAST REQUEST                        
LASTMEDN DS    CL10                MEDIA NAME OF LAST REQUEST                   
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
CSTART   DS    XL3                                                              
CEND     DS    XL3                                                              
DASHES1  DS    CL35                                                             
SAVBKEY  DS    CL32                                                             
SCONPUB  DS    CL6                                                              
SCONSDT  DS    CL3                                                              
SCONEDT  DS    CL3                                                              
SCONPRD  DS    CL3                                                              
SCONREV  DS    XL3                                                              
SAVCKPUB DS    XL6                                                              
SAVCKAGY DS    CL2                                                              
CONPUB   DS    XL6                                                              
REQPUB   DS    XL6                                                              
CONAGY   DS    CL2                                                              
DATEOPT  DS    CL1                 L=SHOW LINE NUMBER                           
CHGSW    DS    XL1                                                              
ACTSW    DS    XL1                                                              
PUBSW    DS    X                                                                
PRDSW    DS    X                                                                
CLTSW    DS    X                                                                
*                                                                               
SVESTFLT DS    XL2                 BINARY ESTIMATE FOR FILTERING                
*                                                                               
SVPRDNAM DS    CL20                                                             
SAVPRD   DS    CL3                 USED FOR SCHEDULE                            
SAVPRDNM DS    CL20                USED FOR SCHEDULE                            
LNEED    DS    XL1                                                              
PROFKEY  DS    CL12                                                             
PROFOPTS DS    0CL16               OPTIONS FROM PROFILE                         
ADDROPT  DS    CL1                 Y- SHOW REP + PUB ADDRS                      
SSORTOPT DS    CL1                 SCHEDULE SORT OPT D=DATE,P=PRD               
*                                  Y=YEAR AND NO LINE NO.                       
         DS    CL14                THIS IS NOT SPARE                            
*                                                                               
PROGPROA DS    CL16                P12A, P13A OR P15A PROFILE                   
PWPROF   DS    CL16                PW PROFILE                                   
*                                                                               
FAXOPT   DS    C                   'Y' IF FAXING                                
*                                                                               
NAMOPT   DS    C                   'Y' = SUPPRESS AGY NAME/ADDR                 
MEMBOPT  DS    C                   'Y' = REMOVE MEMBER AAAA MESSAGE             
         DS    0D                                                               
BUFREC   DS    0CL56               (8+6*8)                                      
BUFKEY   DS    0CL8                                                             
BUFTYP   DS    CL1                                                              
BUFPRD   DS    CL3                                                              
         DS    CL4                 SPARE KEY                                    
*                                                                               
BUFUNITS DS    PL8                 BUFFALO PACKED FLAVORED (8 BYTES)            
BUFGRS   DS    PL8                                                              
BUFINS   DS    PL8                                                              
BUFCUNTS DS    PL8                 NEW UNITS I.E. AFTER                         
BUFCGRS  DS    PL8                 NEW GROSS I.E. AFTER                         
BUFCINS  DS    PL8                 NEW INS   I.E. AFTER                         
*                                  COLUMN CONVERSION DATE                       
*                                                                               
SVQPROG  DS    CL2                                                              
SVQAGY   DS    CL2                                                              
SVQMED   DS    CL1                                                              
*                                                                               
SVQOPT3  DS    CL1                                                              
*                                                                               
SVLKCLT  DS    CL3                                                              
SVLKPUB  DS    CL6               FROM REQPUB                                    
*                                                                               
*                                                                               
PPBYOWRK DS    600C                                                             
*                                                                               
MH1      DS    CL80                                                             
MH2      DS    CL80                                                             
MH3      DS    CL80                                                             
MH4      DS    CL80                                                             
MH5      DS    CL80                                                             
MH6      DS    CL80                                                             
MH7      DS    CL80                                                             
MH8      DS    CL80                                                             
MH9      DS    CL80                                                             
MH10     DS    CL80                                                             
MH11     DS    CL80                                                             
MH12     DS    CL80                                                             
MH13     DS    CL80                                                             
MH14     DS    CL80                                                             
*                                                                               
GRLINS   DS    0CL700              10 X 70                                      
GRLIN1   DS    CL70                                                             
GRLIN2   DS    CL70                                                             
GRLIN3   DS    CL70                                                             
GRLIN4   DS    CL70                                                             
GRLIN5   DS    CL70                                                             
GRLIN6   DS    CL70                                                             
GRLIN7   DS    CL70                                                             
GRLIN8   DS    CL70                                                             
GRLIN9   DS    CL70                                                             
GRLIN10  DS    CL70                                                             
*                                                                               
W        DS    CL132               WORK SPACE TO BUILD PRINT LINE               
*                                                                               
SAVEP    DS    CL132                                                            
SAVEP2   DS    CL132                                                            
SAVESPAC DS    X                                                                
RATESW   DS    X                                                                
*                                                                               
MLIST    DS    XL26                                                             
SLAVEIO  DS    CL600               I/O AREA FOR SLAVE CLIENT                    
*                                                                               
         SPACE 2                                                                
GRTABD   DSECT                                                                  
GRSPACE  DS    CL40                                                             
GRAMT    DS    CL4                                                              
GRSUBSQ  DS    X                                                                
GRDATES  DS    CL24                                                             
*                                                                               
SORTTAB  CSECT                TABLE OF SORTED BUYS                              
         DS    30012X              ROOM FOR 2500 INSERTIONS + FF BUY            
*                                  12 BYTES PER BUY                             
*                                  INS DATE -CL3                                
*                                  PRD -CL3                                     
*                                  EST -CL2                                     
*                                  DISK ADDR -CL4                               
SORTTABX EQU   *-1                                                              
*                                                                               
EDTLIST  CSECT                                                                  
         DS    CL1000                                                           
*        ROOM FOR 500 EDITIONS                                                  
EDTLISTX EQU   *-1                                                              
*                                                                               
PRDTAB   CSECT                                                                  
*NOP*    DS    24000C          ROOM FOR 1000 PRODUCTS (24 BYTES PER)            
         DS    36000C          ROOM FOR 1500 PRODUCTS (24 BYTES PER)            
PRDTABX  EQU   *-1                                                              
*                                                                               
CONTAB   CSECT                                                                  
         DS    42000C          ROOM FOR 1500 CONTRACTS (28 BYTES PER)           
CONTABX  DC    X'00'                                                            
*                                                                               
CONTLEN  EQU   28                 LENGTH OF CONTAB ENTRIES                      
         SPACE 2                                                                
GRTAB    CSECT                                                                  
         DS    30CL69              30 X GRTABL (SEE GRTABD)                     
GRTABL   EQU   69                                                               
*                                                                               
MHSUBS   CSECT                                                                  
         DS    20CL80                                                           
MHSUBN   EQU   20                                                               
MHL      EQU   80                                                               
*                                                                               
RTLKELS  CSECT                                                                  
         DS    4000C                                                            
*                                                                               
         BUFF  LINES=500,ROWS=1,COLUMNS=6,FLAVOR=PACKED,KEYLIST=(8,A)           
       ++INCLUDE DDBUFFALOD                                                     
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE PPMODEQU                                                       
         EJECT                                                                  
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
         PRINT ON                                                               
*       IN SECOND REQUEST CARD COL 21                                           
         ORG   Q2USER                                                           
QCOMM    DS    CL6            STANDARD COMMENT                                  
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE PASRELEM                                                       
       ++INCLUDE PCATELEM                                                       
       ++INCLUDE PCTFELEM          CONTRACT TEL AND FAX ELEM                    
       ++INCLUDE PISITEEL          INTERNET SITE                                
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE PPNEWFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE CTGENFAX                                                       
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
       ++INCLUDE PPGETCGD                                                       
*                                                                               
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG     LOCKKEY                                                        
LOCKMED  DS      XL1                                                            
LOCKCLT  DS      XL3                                                            
LOCKPUB  DS      XL6                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'132PPREP1202 07/08/15'                                      
         END                                                                    
