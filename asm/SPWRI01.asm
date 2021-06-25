*          DATA SET SPWRI01    AT LEVEL 163 AS OF 02/22/21                      
*PHASE T20401A,*                                                                
*INCLUDE SPOTMKWT                                                               
*INCLUDE SPAORLK                                                                
*INCLUDE SPGETBFR                                                               
*INCLUDE BRDMON                                                                 
*INCLUDE GETDAY                                                                 
         TITLE 'T20401 - SPOTPAK WRITER APPLICATION'                            
*                                                                               
***********************************************************************         
*                                                                     *         
*          SPWRI01 (T20401) - SPOT WRITER APPLICATION                 *         
*                                                                     *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-42845  02/22/21 NEW MARKET LOCK OPTIONS FOR SL            *         
* AKAT SPEC-43482  10/20/20 SUPPORT NEW UNIVERSE KEYWORDS             *         
* AKAT SPEC-50579  10/08/20 SPECIAL FILE NAMES FOR ACTIVE             *         
* AKAT SPEC-32039  11/08/19 SUPPORT FOR 2 DECIMAL IMPRESSIONS         *         
* AKAT SPEC-33500  11/07/19 2 DECIMAL IMP SUPPORT FOR SL REPORT       *         
* AKAT SPEC-32039  11/07/19 2 DECIMAL IMP SUPPORT FOR BGL REPORT      *         
* AKAT SPEC-31519  11/07/19 NEW NET2CPP KEYWORD SUPPORT               *         
* AKAT SPEC-37275  07/10/19 FIX BUG IN SPEC-23936                     *         
* AKAT SPEC-23936  06/17/19 NEW DARE MAKEGOODS KEYWORDS SUPPORT       *         
* AKAT SPEC-24843  02/20/19 FIX CLRST RECORDS ON THE FLY              *         
* AKAT SPEC-32148  02/13/19 FIX ESTIMATE UCOMM BUGS FOR INVOICES      *         
* AKAT SPEC-25698  07/10/18 SET COMSCORE DEMOS FOR STATION LOCKIN RECS*         
* AKAT SPEC-16250  05/30/18 NEW COST2 KEYWORDS SUPPORT                *         
* AKAT SPEC-22452  05/09/18 SET COMSCORE DEMOS WHEN PROCESSING GOALS  *         
* AKAT SPEC-22452  04/25/18 FLAG COMSCORE REPORT EVEN IF NO DEMS USED *         
* AKAT SPEC-19808  02/16/18 COMSCORE SUPPORT FOR STATION LOCKIN REPORT*         
* AKAT SPEC-12212  02/16/18 SET COMSCORE DEMO LIST FOR SPOTGOALS      *         
* AKAT SPEC-11692  02/16/18 NEW TARGET3 & TARGET4 KEYWORD SUPPORT     *         
* AKAT SPEC-12852  02/16/18 NEW MSMINFEM, MSFCCMIN & MSFCCFEM KEYWORDS*         
* AKAT SPEC-13400  02/16/18 NEW OMRESEND KEYWORD SUPPORT              *         
* AKAT SPEC-12018  06/01/17 COMSCORE OVERNIGHT SUPPORT                *         
* AKAT SPEC-13149  05/24/17 SET PARENT+ STATION FOR COMSCORE          *         
* AKAT SPEC-6939   02/24/17 SUPPORT COMSCORE DEMO LOOKUPS FOR 17.1.5  *         
* AKAT SPEC-9011   02/10/17 UPDATE TO CANADIAN NATIONAL DEMOS         *         
* AKAT SPEC-15     02/10/17 SUPPORT NEW SQAD CPM                      *         
* AKAT CUSTENH3341 10/28/16 SUPPORT NEW CANADIAN NATIONAL DEMOS       *         
* AKAT SPSUG-95    06/14/16 SUPPORT NEW AD-IDCD2 KEYWORD              *         
* AKAT DSSTK-270   04/18/16 SUPPORT EST UCOMM 5-8                     *         
* AKAT MOSTK-77    01/25/16 SUPPORT NEW MGDTL KEYWORD                 *         
*                                                                   *           
* NOTE: HISTORY PRIOR TO 16MAY02 IS AT BOTTOM OF SOURCE CODE        *           
*28JAN09 141 AKT -- CLEAR MGR2SAVE ON MEDIA CHANGE                  *           
*09JUN15 140 AKT -- RETURN CC NEQ IF OM FILTER DOES NOT PASS        *           
*22MAY15 139 AKT -- READ WR2 PROFILE FOR CASHFLOW REPORT            *           
*09APR15 138 AKT -- GET EST DETAILS BEFORE CALLING DRIVER FOR AUTH$ *           
*28JAN15 137 AKT -- SUPPORT NEW BILINVMY KEYWORD                    *           
*19NOV14 136 AKT -- GET PRD DETAILS BEFORE CALLING DRIVER FOR AUTH$ *           
*13MAY14 135 AKT -- SUPPORT NEW INVCMAT KEYWORD                     *           
*27AUG13 134 AKT -- SUPPORT NEW NTYPE KEYWORD                       *           
*01MAY13 133 AKT -- SUPPORT NEW INVAGYID KEYWORD                    *           
*07MAR12 131 AKT -- FIX ANOTHER EST UCOM BUG FOR INVOICES           *           
*19OCT12 130 AKT -- FIX EST UCOM BUG FOR INVOICES                   *           
*24SEP12 129 AKT -- SUPPORT NEW INVREP KEYWORD AND FILTER           *           
*22AUG12 128 AKT -- SUPPORT NEW SQAD QUARTERS 3 AND 4               *           
*07JUL12 126 HWN -- FIX DARE ORDER STATUS DISPLAY BUG               *           
*04JUN12 125 AKT -- FIX UCOM BUG WHEN PROCESSING GOALS              *           
*21MAR12 124 AKT -- OPTIMIZATION FOR SBEBYDT                        *           
*08FEB12 123 AKT -- SUPPORT FOR COST2CPP                            *           
*26JAN12 122 AKT -- SUPPORT NEW BUYBOOK AND OMEMADD KEYWORDS        *           
*07DEC11 121 AKT -- SUPPORT FOR NEW SC JOHNSON FILE                 *           
*05DEC11 120 AKT -- SUPPORT BILL COST2                              *           
*21NOV11 119 AKT -- SUPPORT NEW RATETYPE KEYWORD                    *           
*28OCT11 118 AKT -- SUPPORT NEW CLRST FORMAT                        *           
*03AUG11 117 AKT -- FIX GOALS COST2 BUG                             *           
*05MAY11 116 AKT -- GET PROGRAM EXCHANGE BILLED NET SEPARATELY      *           
*06APR11 115 AKT -- NEW MFID# KEYWORD SUPPORT                       *           
*16MAR11 114 AKT -- SUPPORT CALCULATED NET FOR BILLED EXCHANGE      *           
*20JAN11 113 AKT -- FIX AFFID TABLE BUG                             *           
*08DEC10 112 AKT -- DELETE OM RECS FROM TSAR BUFFER AT SBPROCDO     *           
*05NOV10 111 AKT -- FIX BFORM BUG                                   *           
*05JUL10 110 AKT -- FIX 2-DEC DEMO BUG BY GETTING EST DETAILS BEFORE*           
*                -- CALLING SPOTGOALS AS WE NEED SBESTDEM!          *           
*07APR10 109 AKT -- REMOVE ALL CLT CML RESTRICTION EXCEPT FOR HD RPT*           
*29JAN10 108 AKT -- REMOVE CODE FOR DEFUNCT AGENCIES                *           
*24DEC09 107 AKT -- FIX CONFIRM/PARTIAL CONFIRM STATUS FOR OM KYWD  *           
*12JUN09 105 AKT -- NEW OMFRTSNT KEYWORD SUPPORT                    *           
*31MAY09 104 AKT -- NEW WRITER DEMO OPTION SUPPORT                  *           
*06MAY09 103 AKT -- NEW LNBKTYPE KEYWORD SUPPORT                    *           
*28APR09 102 AKT -- NEW BHHST KEYWORD SUPPORT                       *           
*15APR09 101 AKT -- SUPPORT MTRADE=Y/N OPTION FOR STATION BUCKETS   *           
*12MAR09 100 AKT -- DON'T DIE IF WE DON'T FIND EST ENTRY IF WE REQ  *           
*                -- PRDGRP. WE PROCESS PRDGRPS THAT ARE NOT IN THE  *           
*                -- PRD/EST BUFFER BECAUSE IT ISN'T A REAL FILTER!  *           
*OCT2708 100 EFJ -- REORG FOR ADDRESSABILITY                        *           
*                -- PUT NODATA MESSAGE ON MQ IF NO REPORT GENERATED *           
*-------------------------------------------------------------------*           
* 28JAN09 99 AKT -- CLEAR MGR1SAVE ON MEDIA CHANGE                  *           
* 14JAN09 98 AKT -- READ BFORM RECORD IN SFM                        *           
* OCT0208 97 AKT -- NEW HDEF AND CNTR KEYWORD SUPPORT               *           
* 08SEP08 96 AKT -- NEW BK5 AND BK6 OPTIONS                         *           
* 15FEB08 95 EFJ -- ADD SPT BACK INTO FILE NAME                     *           
*                -- MAKE SFTP ROUTING SOFT                          *           
* 21JUL08 94 AKT -- FIX BUG FOR GOAL PIGGIES WHEN REQUESTING BY     *           
*                   PGROUP AND STACKING                             *           
* 21MAY09 93 AKT -- STRIP NETWORK BITS PROPERLY FOR ORDERS          *           
* 19MAY08 92 AKT -- NINV ROUTINE USING ALL 3 AIO BUFFERS!           *           
* 14MAY08 91 AKT -- MAKE SURE WE DON'T MISS ANY SYSCODES FOR ORDERS!*           
* 21APR08 90 AKT -- GET STORAGE FOR ACOM BUFFER                     *           
* 16APR08 89 AKT -- SUPPORT NEW BHINVM KEYWORD AS A ROW             *           
* 14APR08 88 AKT -- ALWAYS READ ORDERS BY SYSCODE NOT NETWORK!      *           
* 10MAR08 87 AKT -- FIX HEADLINE SUPRESSION BUG ON DOWNLOAD         *           
* 27FEB08 86 AKT -- SUPPORT NEW UCOM=POL FILTER                     *           
* 21FEB08 85 AKT -- INIT OMSTATUS TO UNSENT                         *           
* 11FEB08 84 AKT -- SUPPORT NEW ESTBKTYP KEYWORD AS A ROW           *           
* 23JAN08 83 EFJ -- NEW FILE NAME FOR FILE OPTION                   *           
* 18JAN08 82 EFJ -- UNDO LEVEL 81 CHANGE AND CHANGE QUALIFIER!      *           
* 15JAN08 81 EFJ -- CHANGE HUB ID FOR MQ REPORT                     *           
* 19DEC07 80 AKT -- NEW GM BILLING RESTRUCTURE SUPPORT              *           
* 30NOV07 79 AKT -- MERGE OM AND BUY DATA                           *           
* 23AUG07 78 EFJ -- SUPPORT OUTPUT TO FILE                          *           
* 01AUG07 77 AKT -- SUPPORT NEW MSTREET KEYWORDS AS A ROW           *           
* 31JUL07 76 AKT -- SKIP CLT ERR MESSAGE WHEN IN DOWNLOAD FORMAT    *           
* 05JUL07 75 AKT -- NEW REPORT2M OPTION TO VALIDATE SECOND REPORT   *           
*                -- STARTING AT MEDIA FIELD, NOT FILTERS FIELD      *           
* 06MAR07 74 AKT -- SET SBBAGYMD IN FMED FOR MEDIA N                *           
* 27FEB07 73 AKT -- BILLGST NOW FULLWORD, NOT XL3                   *           
* 01FEB07 72 AKT -- SUPPORT OMLSTSNT AS ROW                         *           
* 28DEC06 71 BEN -- INDICATE TO SPOTBUY CALLING APP IS WRITER       *           
* 14DEC06 70 AKT -- SBASTABF NOW DEFUNCT                            *           
* 13DEC06 69 AKT -- SUPPORT INVFLAGS AS ROW                         *           
* 18OCT06 68 EFJ -- RE-LINK TO GET NEW SPOTMKWT                     *           
* OCT1606 67 AKT -- SUPPORT NEW GOAL LOCKIN DATE KEYWORDS           *           
* 05OCT06 66 AKT -- SUPPORT NEW ERATE KEYWORD                       *           
* 19SEP06 65 AKT -- BUFFER EST USER FIELDS IF USER ASKS FOR THEM    *           
* 24AUG06 64 AKT -- MOVE EST USER FILED DATA OUT OF EST BUFFER      *           
* 06JUL06 63 AKT -- SUPPORT EQLEN KEYWORD AS ROW                    *           
*                -- DON'T ABEND IF WE DONT FIND A COUNT ELEMENT     *           
* 14JUL06 62 EFJ -- SET GLD2128 IF COLIBIGD                         *           
* 26JUN06 60 AKT -- GET BUYER/BILLER NAMES FOR OM KEYWORDS AND      *           
*                -- DONT GET STUCK WITH MATCHED EST IN PROCINVO     *           
* 31MAY06 59 EFJ -- SUPPORT TRADEG KEYWORD                          *           
* 18MAY06 58 AKT -- SUPPORT ORBIT KEYWORD AS ROW                    *           
* 13JAN06 57 EFJ -- GET EST FROM INV DETAIL MATCH INFO              *           
* 18JAN06 56 AKT -- ALLOW AGY GZ TO REQ OVERNIGHT WRITERS WITH CMML *           
*                -- KEYWORDS AND CLIENT GROUPS OVERNIGHT            *           
* 16JAN06 55 AKT -- SUPPORT BYDEMSRC KEYWORD AS A ROW & UCOM BUG FIX*           
* 06DEC05 54 AKT -- PURGE INV FROM TSAR BUFFER ON MODE INVSKIP      *           
* 15NOV05 53 AKT -- EXTRACT MORE INV INFO / 72-73 BUY ELEMS DEFUNCT *           
* 31OCT05 52 AKT -- DON'T OVERWRITE DEMO NAMES IF SUPPRESSING TGT   *           
* 13SEP05 51 AKT -- FIX SREP BUG FOR INVOICES                       *           
* 20JUL05 50 EFJ -- 2 CHARACTER OFFICE CODE SUPPORT                 *           
* 09AUG05 49 AKT -- HONOR CREATION DATE FILTER FOR BILL & INV RECS  *           
* 03AUG05 48 AKT -- FILL IN SBEPRD WHEN PROCESSING BUYS FOR AN ALL  *           
*                -- MEDIA SINGLE PRODUCT REQUEST                    *           
* 09MAY05 47 AKT -- FIX RCPACK BUG (R1 USED IN PARAM LIST)          *           
* 03JAN05 46 AKT -- ADD FAX RESENT STAT FOR OM DATA AND DO NOT DUMP *           
*                   IF STAT IS NOT FOUND                            *           
* 30NOV04 45 AKT -- MAKE SURE R1 POINTS TO ESTBUFF AFTER ESTTEST    *           
* 02NOV04 44 AKT -- GO THROUGH ALL X'12' ELEMS FOR OMREP FILTER     *           
* 27SEP04 43 AKT -- CLEAR OMFLAGS AT EVERY OM HOOK                  *           
* 17SEP04 42 AKT -- CLEAR COMMERCIAL TABLE IN BETWEEN CLIENTS AND   *           
*                   SUPPORT AD-ID KEYWORD AS ROW                    *           
* 31AUG04 41 AKT -- SBREP SHOULD BE C'000' IF NOT ON INV OR EST!    *           
* 17AUG04 40 AKT -- SBREP SHOULD BE SET FROM INV REC AND USE RCPACK *           
* 12JUL04 39 AKT -- SUPPORT 2ND SET OF COL FILTERS FOR CONTINUE OPT *           
* 08JUL04 38 AKT -- EXTRACT OM DATA                                 *           
* 16JUN04 37 AKT -- FIX PQIX BUG SET 4 LEVELS BACK                  *           
* 09JUN04 36 AKT -- SET REQSML FOR SAATCHI X3 JOBS                  *           
* 08JUN04 35 PWE -- EXTRA NETSIZE FILTERS (NOW THEY TELL ME!)       *           
* 03JUN04 34 PWE -- NETSIZE FILTER                                  *           
* 02JUN04 33 AKT -- ALWAYS PRINT HEADINGS IN PRINT WHEN DOWNLOADING *           
* 16APR04 32 AKT -- ETYPE KEYWORD AS A ROW                          *           
* 14APR04 31 AKT -- SET ADEMLST IN PROCSL FOR MKT WEIGHING          *           
* 17MAR04 30 AKT -- FILTER OUT INVOICES THAT DONT MATCH REQ PERIOD  *           
* 29JAN04 29 PWE -- SHOW K/W (SHOWCODE - CANADA NETWORK)            *           
* 22JAN04 28 AKT -- CALL FSTA FOR CANADIAN SOFT DEMOS               *           
* 05JAN04 27 AKT -- SAVE SCREEN TO TEMPSTR INSTEAD OF TIA           *           
*                   PASS FILTER FIELD'S HELP NUM TO GENPROG         *           
* 13JAN04 26 EFJ -- SET REQSML FOR 'ALL' CLIENT OR ALL MKT REQ      *           
*                -- SET ZENITH MIRAMAX WRITERS TO A HIGHER PRIORITY *           
* 16DEC03 25 PWE -- PST REFINEMENTS                                 *           
* 19NOV03 24 AKT -- 2 MORE MSTREET KEYWORDS TO SUPPORT AS ROWS      *           
* 23OCT03 23 EFJ -- RE-LINKED FOR BIGGER SPDEMLK                    *           
* 06NOV03 22 PWE -- OPTIONAL USE OF NETWORK DOLLARS (CANADA)        *           
* 31OCT03 21 AKT -- FIX MGROUP AND CGROUP X'40' BUGS                *           
* 31OCT03 20 AKT -- FIX SMALL COUNT RECORD BUG                      *           
* 23OCT03 19 EFJ -- FORCE DOWNFIX FOR INITIATIVE                    *           
* 21OCT03 18 AKT -- DO NOT OVERWRITE SBSPPROF+3 IF ACTBOOK OPT SET  *           
* 08OCT03 17 AKT -- CALL FSTA IN WRI02 IF MSTREET FLAG SET          *           
*                   SUPPORT MSTREET KEYWORDS AS ROWS                *           
* 22SEP03 16 EFJ -- CHAIN RPTS IN SAME PQ ENTRY                     *           
* 24SEP03 15 PWE -- PST                                             *           
* 10JUL03 14 AKT -- INVSRCE                                         *           
* 14APR03 13 EFJ -- BETTER ERROR HANDLING ON CONTINUED REPORT       *           
* 24MAR03 12 EFJ -- FIX UPGRADE REPORTING WITH SQAD                 *           
* 14MAR03 11 AKAT-- START USING PACKED VERSIONS OF FOLLOWING FIELDS *           
*                   SBBILGRS = SBILGRSP                             *           
*                   SBBILNET = SBILNETP                             *           
*                   BILLCOST = BILCOSTP                             *           
* 17JAN03 10 EFJ -- SUPPORT REPORT2 AND CONTINUED OPTIONS           *           
*                -- RENUMBER VREC LABELS                            *           
* 17OCT02 09 AKAT-- MOVE NAMPOOL TO TSAR BUFFER                     *           
* 02DEC02 08 EFJ -- FIRSTAB ENTRY FOR BLANK                         *           
* 13SEP02 07 EFJ -- SET SBSKIP IF INVOICE HDR ELEM FAILS FILTERS    *           
* 24JUL02 06 EFJ -- COMMERCIAL KYWDS ACROSS CLT FOR MC              *           
* 15JUL02 05 EFJ -- FIRSTAB ENTRY FOR INVSTAT                       *           
*                -- NEW PGEST KYWD                                  *           
* 30MAY02 04 EFJ -- EXPAND PGDATA KYWD                              *           
* 29MAY02 03 EFJ -- BONUS KEYWORD                                   *           
* 16MAY02 02 EFJ -- LEVEL RESET                                     *           
*                -- SUPPORT CONTRACT NUMBER IN BYID KYWD            *           
*                                                                   *           
*********************************************************************           
T20401   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20401,RA,RR=R2,CLEAR=YES                                  
         LR    R6,RC                                                            
         USING WORKD,R6                                                         
         ST    R2,RELO                                                          
         BASR  R8,0                                                             
         AHI   R8,GLOBALS-*                                                     
         USING GLOBALS,R8          RA=A(GLOBAL LITERAL POOL)                    
         ST    RB,PRGBASE1                                                      
         ST    RA,PRGBASE2                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         ST    RC,AGEND                                                         
         USING GETTXTD,GETTXTCB                                                 
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         MVC   SBCOMFAC,ACOMFACS   SET UP FOR SPOTIO                            
         LA    R1,DRIVGEN                                                       
         ST    R1,ADRIVER          A(DRIVER CALLING ROUTINE)                    
         ST    RD,SAVERD01         SAVE A(REGISTER SAVE AREA)                   
*                                                                               
         LA    R0,AEXTRAN          SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(EXTRA)                                                     
         A     R1,RELO                                                          
         ST    R1,AEXTRA(RE)                                                    
         STC   RF,AEXTRA(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE,                                  
         BNE   WRI4                                                             
         ICM   R1,15,ASPWRI02      LOAD SPWRI02 IF NOT LOADED YET               
         BNZ   WRI2                                                             
         GOTO1 CALLOV,DMCB,X'02000000',0,0                                      
         L     R1,0(R1)                                                         
         ST    R1,ASPWRI02                                                      
*                                                                               
WRI2     LA    R0,AXTRAN           SET ADDRESSES OF SPWRI02 EXTENSION           
         SR    RE,RE               ROUTINES                                     
         SR    RF,RF                                                            
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
WRI4     L     R1,=A(FINALVAL)     SET A(FINAL VALIDATION ROUTINE)              
         A     R1,RELO                                                          
         ST    R1,AFINVAL                                                       
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP OFFLINE                                 
         BE    WRI6                 YES                                         
         CLI   DDS,C'Y'            DDS?                                         
         BE    WRI6                 YES                                         
         CLI   WLPROF,C'Y'         ON SECURITY?                                 
         BNE   WRI6                 NO                                          
*                                                                               
WRI4B    TM    AUTH,X'40'          AUTHORIZED?                                  
         BNZ   WRI6                 YES                                         
         CLI   ACTNUM,ACTDEL       TEST ACTION=DELETE                           
         BE    WRI4C                YES                                         
         CLI   ACTNUM,ACTADD       TEST ACTION=ADD                              
         BE    WRI4C                YES                                         
         CLI   ACTNUM,ACTCHA       TEST ACTION=CHANGE                           
         BNE   WRI6                 NO                                          
WRI4C    MVI   ERROR,SECLOCK                                                    
         LA    R2,CONACTH                                                       
         J     TRAPERR                                                          
*                                                                               
WRI6     CLI   MODE,RUNFRST        RUNFIRST                                     
         BE    FRST                                                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DISP                                                          
         J     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         VALIDATE REQUEST                             
         BNE   *+12                                                             
         BRAS  RE,VREC                                                          
         J     XIT                                                              
*                                                                               
         CLI   MODE,ERRHOOK        HANDLE ERROR                                 
         BNE   *+12                                                             
         BRAS  RE,MQRPTERR                                                      
         J     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PRINT THE REPORT                             
         BNE   *+12                                                             
         BAS   RE,PREP                                                          
         J     XIT                                                              
*                                                                               
         CLI   MODE,RUNLAST        RUNLAST                                      
         BE    LST                                                              
*                                                                               
         J     XITNE                                                            
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
* ***NOTE*** ***NOTE*** ***NOTE***                                              
* IF YOU THINK THAT RUNFRST COMES BEFORE VALREC, YOU'RE WRONG!                  
*                                                                               
FRST     CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   *+8                                                              
         MVI   TWAFIRST,2          YES-GET RUNLAST MODE ALSO                    
         GOTOR RPT,RPRNFRST        PASS RUNFRST TO USER APPLICATION             
         BNE   FRSTX               OPPORTUNITY TO NOT CLEAR TSPFUSER!           
*                                                                               
         L     R1,TWADCONS         CLEAR FIRST PART OF INTER-REQUEST            
         L     R1,TSPFUSER-TWADCOND(R1)  USER SAVE AREA                         
         XC    0(256,R1),0(R1)                                                  
FRSTX    J     XIT                                                              
         SPACE 2                                                                
* RUNLAST                                                                       
*                                                                               
LST      GOTOR RPT,RPRUNLST        PASS RUNLAST TO USER APPLICATION             
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VKEY     CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    VKEY10                                                           
         CLI   ACTNUM,ACTDIS                                                    
         BNE   VKEYX                                                            
*                                                                               
VKEY10   XC    WORK,WORK           YES-READ AGENCY'S WR PROFILE                 
         MVC   WORK(4),=C'S0WR'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         CLI   WORK+16,C'Y'        TEST FILTER REQUIRED FOR LIST                
         BNE   VKEYX                                                            
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BNE   *+12                                                             
         CLI   WORK+16+9,C'Y'      TEST FILTER NOT REQUIRED FOR LIST            
         BE    VKEYX                NO                                          
*                                                                               
         L     R2,EFHTAG           YES-LOCATE FILTER FIELD                      
         SR    R0,R0                                                            
         LA    RE,3                                                             
         CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BE    *+8                  YES                                         
         LA    RE,6                 NO - FILTER FIELD DIFFERENT!                
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
VKEY1    CLI   DDS,C'Y'            DDS?                                         
         BE    VKEY3                YES - DON'T VALIDATE, JUST ACCEPT           
         TM    AUTH,X'10'          OVERRIDE SECURITY?                           
         BNZ   VKEY3                YES                                         
         CLI   5(R2),4             TEST AT LEAST 4 FILTER CHARACTERS            
         JL    EFILT                                                            
         LLC   RE,5(R2)                                                         
         LA    R1,8(R2)            CHARACTERS * AND - NOT ALLOWED               
*                                                                               
VKEY2    CLI   0(R1),C'*'                                                       
         JE    EFILT                                                            
         CLI   0(R1),C'-'                                                       
         JE    EFILT                                                            
         LA    R1,1(R1)                                                         
         BCT   RE,VKEY2                                                         
VKEY3    MVC   FILTER,8(R2)        SAVE VALID FILTER                            
*                                                                               
VKEYX    J     XIT                                                              
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PREP     NTR1                                                                   
*                                                                               
         L     RF,TWAMASTC         A(MASTD)                                     
         L     RF,MCAEXTRA-MASTD(RF) A(EXTRA DATA AREA)                         
         CLI   MCCSPASS-MCEXTRA(RF),MCCSP002 COMSCORE PASS 2?                   
         BNE   *+8                 NO                                           
         OI    SBEFLAG8,SBE8CP2    YES - FLAG COMSCORE PASS 2                   
         CLI   MCCSPASS-MCEXTRA(RF),MCCSP001 COMSCORE PASS 1?                   
         BNE   PR10                NO                                           
         OI    SBEFLAG8,SBE8CP1    YES - FLAG COMSCORE PASS 1                   
         OI    SBQSKIP,SBQSKGL     SKIP READING GOALS                           
         OI    SBQSKIP,SBQSKBIL    SKIP READING STATION BILLS                   
         MVI   SBQREAD,0           ONLY READ BUYS                               
         LA    RE,SBLOCK           GET ADDRESSABILITY TO END OF SBLOCK          
         MVI   SBQREAD2-SBLOCK(RE),0 ONLY READ BUYS                             
*                                                                               
PR10     GOTO1 INITDRIV            INITIALIZE DRIVER                            
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         L     RE,AGLOBAL          GET OFFLINE BUFFER IN DUMPS                  
         ST    RE,MCUSRDMP                                                      
         L     RF,=A(GLOBALLN)                                                  
         LA    RE,0(RE,RF)                                                      
         ST    RE,MCUSRDMP+4                                                    
         DROP  R1                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         LA    R1,DRHOOK           APPLICATION HOOK                             
         ST    R1,GLAHOOK                                                       
         MVI   GLHOOK,GLINIT                                                    
         GOTOR RPT,RPDRHOOK                                                     
*                                                                               
         MVI   GLMODE,GLINIT                                                    
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
         TM    DTAIND11,DIOMKGD    READ ORDER MAKEGOOD RECORDS?                 
         BNZ   *+12                YES - PRINT ALL DETAILS                      
         TM    SBQREAD,SBQRDINF    READING INFOMERCIALS?                        
         BZ    PR50                 NO                                          
         OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
*                                                                               
         LH    R5,=H'2000'         YES-GET STORAGE FOR INFO UPSELL TAB          
         LR    R3,R5                                                            
         LA    RE,INFTABL                                                       
         MR    R2,RE               MAX N'RECORDS * L'ENTRY                      
         LA    R3,12(R3)                                                        
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,SBAINFTB                                                      
         ST    R3,0(RE)            +0 = L'TABLE                                 
         XC    4(4,RE),4(RE)       +4 = N'RECORDS IN TABLE SO FAR               
         ST    R5,8(RE)            +8 = MAX N'RECORDS IN TABLE                  
*                                                                               
PR50     TM    COLIND2,COLISTAK    TEST ANY COLS ARE STACKS                     
         BZ    PR60                                                             
         GOTO1 ASETSTAK            YES-SET DATA INDICATORS FOR STACKS           
*                                                                               
PR60     TM    DATAIND9,DIWIPW     WESTERN PW?                                  
         BZ    PR62                 NO                                          
         OI    SBEFLAG,SBEWIPW      YES - SET EXTRACT FLAG FOR SPOTBUY          
         OI    SBQPER,SBQPWK                                                    
         NI    SBQRDOPT,X'FF'-SBQROSTA                                          
*                                                                               
* ERROR CAN'T BE TRAPPED AT REQUEST TIME BECAUSE                                
* WE DON'T KNOW IT'S A PW KEYWORD UNTIL NOW                                     
*                                                                               
         TM    OPTIND4,OPTXEST                                                  
         BNZ   PR62                SKIP TEST                                    
         CLC   SBQEST,SBQESTND     SINGLE EST?                                  
         BNE   PR330                NO - FREE CORE & ERROR EXIT                 
*                                                                               
PR62     TM    COLIND3,COLICBAG    REPORTING CABLE AGGREGATE?                   
         BZ    *+8                  NO                                          
         OI    GLINDS,GLPALTOT      YES - PRINT ALL TOTALS                      
*                                                                               
         TM    DATAIND9,DICPS      COST KEYWORD?                                
         BZ    PR64                 NO                                          
         LR    R1,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   R1,4096                                                          
         USING SYSD+4096,R1                                                     
         OI    SBEFLAG2,SBE1SPT    SET FLAG FOR SPOTBUY                         
         DROP  R1                                                               
*                                                                               
PR64     GOTO1 DATCON,DMCB,(3,BTODAY),SBQTODAY                                  
         LA    R1,IOHOOK                                                        
         ST    R1,SBIOHOOK                                                      
         LA    R1,ESTUFHK          HOOK FOR ESTIMATE USER FIELDS                
         LA    RF,SBLOCK                                                        
         ST    R1,SBEHOOK-SBLOCK(RF)                                            
         MVC   SBAIO1(12),AIO1     PASS 3 IO AREAS                              
*                                                                               
         L     R1,SBAOFFBF         YES-USE OFF-LINE BUFFER AREA                 
         L     RF,0(R1)            RF=L'AVAILABLE SPACE                         
*                                                                               
         LR    RE,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   RE,4096                                                          
         USING SYSD+4096,RE                                                     
         LA    R0,0(R1,RF)         A(END OF BUFFER)                             
         ST    R0,SBACHNKX         END OF BUFF IS END OF CHNKS                  
         DROP  RE                                                               
*                                                                               
         LA    R1,4(R1)                                                         
*                                                                               
PR70     TM    SBQSKIP,SBQSKBUY                                                 
         BO    PR90                                                             
         ST    R1,SBASPTTB                                                      
         LA    RE,2000             ONLINE, L'SPOT TABLE = 2K                    
         CLI   OFFLINE,C'Y'                                                     
         BNE   PR80                                                             
         SR    RE,RE               OFFLINE, L'SPTTAB=2/3 OF AVAILABLE           
         SLDA  RE,2                                                             
         D     RE,=F'3'                                                         
         LA    RE,1(RF)                                                         
         SRL   RE,1                                                             
*                                                                               
PR80     ST    RE,SBLSPTTB                                                      
         AR    R1,RE                                                            
*                                                                               
PR90     ST    R1,SBACHUNK                                                      
         XC    SBADATE,SBADATE                                                  
         MVC   SBENDEM,NDEMOS      NUMBER OF DEMOS                              
         MVI   SBEDEM,C'Y'                                                      
         CLI   NDEMOS,0            TEST FOR DEMOS                               
         BNE   *+8                                                              
         MVI   SBEDEM,C'N'         NO-NO DEMOS IN EST BUFF ENTRIES              
         XC    SBEDEMOS,SBEDEMOS   CLEAR OVERRIDE DEMO LIST                     
         MVI   SBEBILL,C'N'        BILLED DATA                                  
         MVI   SBEBEF,C'N'         SPOTS BEFORE REQUEST                         
         MVI   SBEAFTER,C'N'       SPOTS AFTER REQUEST                          
         MVI   SBEBYCLS,C'N'       BY COMMERCIAL CLASS (NOT USED)               
         XC    SBECML,SBECML       COMMERCIAL FILTER                            
         MVI   SBESPILL,0          SPILL OPTION                                 
         MVI   SBETAX,C'N'         SUBTRACT TAX                                 
         TM    OPTIND5,OPTNOTAX                                                 
         BZ    *+8                                                              
         MVI   SBETAX,C'Y'                                                      
         TM    SBQREAD,SBQRDCLS    TEST READ CLEARANCE STATUS RECORDS           
         BZ    *+8                                                              
         MVI   SBEBYCHK,C'Y'       AND BREAK OUT CHUNKS BY CHECK                
*                                                                               
         MVI   SBEPAYDT,C'N'                                                    
         CLI   DATEOPT,0           TEST DATE OPTION SET                         
         BE    PR100                                                            
         OI    SBQRDOPT,SBQROEST   YES-LET SPOTIO ACCEPT ALL ESTIMATES          
         CLI   DATEOPT,DOPAID      TEST DATE=PD                                 
         BNE   *+8                                                              
         MVI   SBEPAYDT,C'Y'       YES-USE PAY DATE INSTEAD OF BUY DATE         
*                                                                               
PR100    CLI   SBEUNALL,0          TEST UNALL OPTION SET                        
         BNE   PR110                                                            
         MVI   SBEUNALL,C'N'       NO-EXCLUDE UNALLOCATED                       
         CLC   SBQPRD,=C'ALL'                                                   
         BE    *+14                                                             
         CLC   SBQPRD,=C'POL'                                                   
         BNE   PR110                                                            
         MVI   SBEUNALL,C'Y'       INCLUDE THEM FOR ALL OR POL                  
*                                                                               
PR110    MVC   SBEPRD,SBQBPRD      PRODUCT FILTER                               
         CLI   SBQBPRD,FF                                                       
         BNE   *+8                                                              
         MVI   SBEPRD,0                                                         
         TM    SBQPIND,SBQPUNA     UNALLOCATED ONLY                             
         BZ    *+8                                                              
         MVI   SBEPRD,X'FE'                                                     
*                                                                               
         MVI   SBEPRD2,0           BRAND 2 NOT SUPPORTED                        
         CLI   SBQBPRD,X'FF'       TEST POL BREAK OUT                           
         BE    *+12                                                             
         MVI   SBEBYPRD,C'Y'       YES-BREAK OUT POL PRODUCTS                   
         B     PR120                                                            
         TM    SBQPIND,SBQPPB      NO-TEST SPLIT PIGGYBACKS                     
         BO    PR120                                                            
         MVI   SBEBYSLN,C'Y'       YES-THEN BREAK OUT SPOT LENGTHS              
         B     PR130                                                            
*                                                                               
PR120    MVI   SBEBYSLN,C'N'                                                    
         TM    DATAIND,DILEN       TEST SPOT LENGTH REQUESTED                   
         BO    *+12                                                             
         TM    DATAIND4,DISLN                                                   
         BZ    PR130                                                            
         MVI   SBEBYSLN,C'Y'       YES - SET BY SPOT LENGTH                     
*                                                                               
PR130    MVI   SBEBYDT,C'N'                                                     
         TM    COLIND2,COLIMAT     MATCHED/UNMATCHED COLUMN FILTERS?            
         BZ    *+8                 NO                                           
         MVI   SBEBYDT,C'C'        YES - INDICATE COLUMN FILTERS                
         TM    DATAIND6,DIDATE     BREAK OUT BY AFFIDAVIT DATE?                 
         BZ    *+8                 NO                                           
         MVI   SBEBYDT,C'Y'        YES - THIS OVERRIDES COLUMN FILTERS          
*                                                                               
         MVI   SBEBYTM,C'N'                                                     
         OC    ATIMRNG,ATIMRNG     ANY AFFID TIME FILTER?                       
         BNZ   *+12                YES - MUST BREAK OUT BY AFFID TIME           
         TM    DATAIND6,DITIME     BREAK OUT BY AFFIDAVIT TIME                  
         BZ    *+8                                                              
         MVI   SBEBYTM,C'Y'                                                     
*                                                                               
         MVI   SBECS,C'N'                                                       
         MVI   SBEBYDPT,C'N'                                                    
         TM    DATAIND6,DICHILD    CHILD SPOT DATA                              
         BZ    *+12                                                             
         MVI   SBECS,C'Y'                                                       
         B     PR140                                                            
         TM    DATAIND6,DIDR       DIRECT RESPONSE DATA                         
         BZ    PR140                                                            
         MVI   SBECS,C'R'                                                       
         TM    DATAIND,DIDPT       TEST FOR DAYPARTS                            
         BZ    PR140                                                            
         MVI   SBEBYDPT,C'Y'       YES-ALSO BREAK OUT BY RESPONSE DPT           
*                                                                               
PR140    TM    DATAIND6,DIAPROG    AFFID PROGRAM                                
         BZ    PR150                                                            
         GOTO1 AAFFIDHK            YES - SET AFFID HOOK                         
         MVI   SBEBYDT,C'Y'        MAKE SURE CHUNKS ARE BROKEN OUT              
         MVI   SBEBYTM,C'Y'        BY AFFID DATE/TIME                           
*                                                                               
PR150    MVI   SBESPLIT,C'Y'       SPLIT PIGGYBACKS                             
         TM    SBQPIND,SBQPPB      TEST NOT TO                                  
         BZ    *+8                                                              
         MVI   SBESPLIT,C'N'                                                    
*                                                                               
         MVI   SBEPAID,C'N'        PAID DATA                                    
         TM    DATAIND2,DIBYPAID                                                
         BZ    *+8                                                              
         MVI   SBEPAID,C'Y'                                                     
*                                                                               
         TM    DATAIND7,DIGSTI     INPUT GST                                    
         BZ    *+8                                                              
         OI    SBEGST,SBEGSTI                                                   
         TM    DATAIND7,DIGSTO     OUTPUT GST                                   
         BZ    *+8                                                              
         OI    SBEGST,SBEGSTO                                                   
         TM    DATAIND7,DIGSTB     BILLED GST                                   
         BZ    *+8                                                              
         OI    SBEGST,SBEGSTB                                                   
*                                                                               
         TM    DTAIND10,DIPSTI     INPUT PST                                    
         BZ    *+8                                                              
         OI    SBEGST,SBEPSTI                                                   
         TM    DTAIND10,DIPSTO     OUTPUT PST                                   
         BZ    *+8                                                              
         OI    SBEGST,SBEPSTO                                                   
         TM    DTAIND10,DIPSTB     BILLED PST                                   
         BZ    *+8                                                              
         OI    SBEGST,SBEPSTB                                                   
*                                                                               
         MVI   SBEEFCST,C'N'       EFFECTIVE COST                               
         TM    DATAIND2,DIEFFCST   BUY COST                                     
         BO    PR160                                                            
         TM    DATAIND5,DIBILLC    BILL COST                                    
         BO    PR160                                                            
         TM    SBEGST,SBEGSTO+SBEGSTB+SBEPSTO+SBEPSTB  OR OUTPUT OR             
         BZ    *+8                                     BILLED GST/PST           
PR160    MVI   SBEEFCST,C'Y'       THIS ALSO TELLS SPOTIO TO GET                
*                                  BILL FORMULAE                                
         TM    SBEFLAG,SBEPST      IF ADDING BILLHDR PST SET SBEPSTB            
         BZ    *+8                 SO SPOTIO EXTRACTS PRODUCT PST FIELD         
         OI    SBEGST,SBEPSTB                                                   
*                                                                               
         MVI   SBEFILT,C'N'        ESTIMATE FILTERS                             
         TM    DATAIND4,DIESTFLT                                                
         BZ    *+8                                                              
         MVI   SBEFILT,C'Y'                                                     
*                                                                               
         MVI   SBERTLSC,C'N'       RETAIL SCHEME CODE                           
         TM    DATAIND7,DIRTLSCH                                                
         BZ    *+8                                                              
         MVI   SBERTLSC,C'Y'                                                    
*                                                                               
         MVI   SBESREP,C'N'        SPECIAL REP                                  
         TM    DATAIND4,DISREP                                                  
         BZ    *+8                                                              
         MVI   SBESREP,C'Y'                                                     
*                                                                               
         MVI   SBEBYCML,C'N'       COMMERCIALS                                  
         TM    DATAIND5,DICML                                                   
         BZ    *+8                                                              
         MVI   SBEBYCML,C'Y'                                                    
*                                                                               
         MVI   SBESVI,0            AUTO SVI ADJUSTMENTS                         
         CLI   SBQSVI,0            TEST SVI OPTION SET                          
         BE    PR170                                                            
         TM    SBQSVI,SBQSVIAU     YES - THEN SET SVI NOW                       
         BO    PR170                                                            
         MVI   SBESVI,FF                                                        
         TM    SBQSVI,SBQSVINO     NO ADJUST                                    
         BO    PR170                                                            
         LLC   RF,SBQSVI                                                        
         SLL   RF,4                                                             
         STC   RF,SBESVI           SVI MONTH                                    
*                                                                               
PR170    TM    DATAIND,DIDEMNDX    TEST ANY DEMO INDEX COLS                     
         BO    *+12                                                             
         TM    DATAIND3,DIDOLNDX   OR DOLLAR INDEX COLS                         
         BZ    PR220                                                            
         CLI   SBQDATA,0           YES - TEST DATA OPTION SET                   
         BE    PR190                                                            
         CLI   SBQDATA,SBQDORD     YES - IF SINGLE DATA TYPE                    
         BNE   *+12                      THEN SET THE DEFAULT OTHER             
         OI    SBQDATA,SBQDPUR           DATA TYPE                              
         B     PR200                                                            
         CLI   SBQDATA,SBQDPUR                                                  
         BE    PR180                                                            
         CLI   SBQDATA,SBQDRERT                                                 
         BE    PR180                                                            
         CLI   SBQDATA,SBQDAFFD                                                 
         BNE   PR200                                                            
*                                                                               
PR180    OI    SBQDATA,SBQDGOAL                                                 
         B     PR200                                                            
*                                                                               
PR190    MVI   SBQDATA,SBQDGOAL+SBQDPUR  DATA OPTION NOT SET - SET IT           
         TM    DATAIND,DIDEMNDX                                                 
         BZ    PR200                                                            
         LR    RF,R9                                                            
         AHI   RF,SBQBOOK-SYSD                                                  
         OC    0(4,RF),0(RF)                                                    
         BZ    PR200                                                            
         MVI   SBQDATA,SBQDGOAL+SBQDRERT                                        
*                                                                               
PR200    TM    DATAIND,DIDEMNDX    TEST DEMO INDEX                              
         BZ    PR210                                                            
         TM    SBQDATA,SBQDPUR     YES-SET PURCH/ACHVD/AFFID DEPENDING          
         BZ    *+8                 ON DATA OPTION                               
         OI    DATAIND,DIDEMP                                                   
         TM    SBQDATA,SBQDRERT                                                 
         BZ    *+8                                                              
         OI    DATAIND,DIDEMR                                                   
         TM    SBQDATA,SBQDAFFD                                                 
         BZ    *+8                                                              
         OI    DATAIND,DIDEMA                                                   
*                                                                               
PR210    NI    SBQSKIP,FF-SBQSKBUY MAKE SURE BUY RECORDS ARE READ               
         TM    SBQDATA,SBQDGOAL+SBQDORD    TEST WHETHER NEED GOALS              
         BZ    PR220                                                            
         NI    SBQSKIP,FF-SBQSKGL  YES - MAKE SURE WE READ THEM                 
         OI    DATAIND2,DIGLDEM                                                 
         TM    SBQDATA,SBQDORD     TEST NEED LOCKIN DATA                        
         BZ    PR220                                                            
         OI    DATAIND3,DILOCKIN   YES - MAKE SURE WE GET LOCKIN DATA           
*                                                                               
PR220    LR    RF,R9                                                            
         AHI   RF,SBQBOOK-SYSD                                                  
         OC    0(4,RF),0(RF)       TEST BOOK SET YET                            
         BNZ   *+10                                                             
         MVC   0(3,RF),=C'ACT'     NO - DEFAULT IS ACTUAL BOOK                  
*                                                                               
         LA    R2,SBLOCK           ESTABLISH SBLOCK                             
         USING SBLOCK,R2                                                        
*                                                                               
         XC    SBSQDBFA,SBSQDBFA   INIT BUFFER ADDRESS                          
*                                                                               
         TM    DTAIND12,DISQD2     SQAD CPM NEEDED?                             
         BNZ   *+12                YES                                          
         TM    DTAIND10,DISQD      IF SQUAD CPP'S NEEDED                        
         BZ    PR225                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   PR225                                                            
*                                                                               
         L     R3,=AL4(SQDBUFFL)   SQUAD BUFFER LENGTH                          
         AHI   R3,4                LEAVE ROOM FOR LENGTH WORD                   
         SRL   R3,3                NEXT DOUBLE WORD IN LENGTH                   
         AHI   R3,1                                                             
         SLL   R3,3                                                             
*                                                                               
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET'  GET STORAGE                                  
*                                                                               
         ICM   RE,15,4(R1)         A(STORAGE)                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R3,0(RE)            +0 = L'TABLE                                 
         LA    RE,4(RE)            A(BUFFER START)                              
*                                                                               
         STCM  RE,15,SBSQDBFA      SAVE BUFFER ADDRESS                          
*                                                                               
         GOTO1 ASQADINI            INIT SQAD DPT TRANSLATION TABLE              
         DROP  R2                                                               
*                                                                               
PR225    DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   PR226                                                            
*                                                                               
         LAY   R3,6000             NINV BUFFER LENGTH                           
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET'  GET STORAGE                                  
*                                                                               
         ICM   RE,15,4(R1)         A(STORAGE)                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  RE,15,NINVREC       SAVE A(NINV BUFFER)                          
*                                                                               
         TM    DTAIND11,DIACOM     USER REQUESTED ACOM?                         
         BZ    PR226               NO                                           
*                                                                               
         LAY   R3,6000             ACOM BUFFER LENGTH                           
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET'  GET STORAGE                                  
*                                                                               
         ICM   RE,15,4(R1)         A(STORAGE)                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  RE,15,ACOMREC       SAVE A(ACOM BUFFER)                          
*                                                                               
PR226    CLI   SBQMED,C'C'         TEST MEDIA=C                                 
         BNE   PR230                                                            
         TM    SBQSKIP,SBQSKGL     AND READING GOALS                            
         BO    PR230                                                            
         TM    DATAIND5,DISUBMED   AND SUBMED IS A ROW                          
         BZ    PR230                                                            
         OI    SBQRDOPT,SBQROCGL   YES-READ MEDIA C/T/N GOALS                   
*                                                                               
PR230    MVI   ESTSTART,0                                                       
         ZAP   CLTCNT,=P'0'                                                     
         MVI   SVDPTMEN,0                                                       
         MVI   SVMED,0                                                          
         MVI   SVMED2,0                                                         
         MVI   BUYRECSW,C'N'                                                    
         XC    MGR1SAVE,MGR1SAVE                                                
         XC    MGR2SAVE,MGR2SAVE                                                
         XC    PGRSAVE,PGRSAVE                                                  
         LA    R1,COMDLIST         COMSCORE DEMO OVERRIDE LIST                  
         ST    R1,SBCOMDEM         A(COMSCORE DEMO OVERRIDE LIST)               
*                                                                               
         L     R1,ASPOOLD                                                       
         MVC   SBPRINT,VPRINT-SPOOLD(R1)                                        
*                                                                               
         TM    OPTIND4,OPTXFILE    DDB CROSS FILE REPORT?                       
         BZ    PR235                NO                                          
         OC    SBQMGRD,SBQMGRD     MKT GROUP?                                   
         BNZ   PR235                                                            
         OC    SBQMKT,SBQMKT       ALL MARKETS?                                 
         BZ    PR235               YES                                          
         CLC   =C'ALL',SBQMKT                                                   
         BE    PR235                                                            
         PACK  DUB,SBQMKT                                                       
         CVB   R1,DUB                                                           
         AR    R1,R1               X 2                                          
         L     RE,SBAMGTAB                                                      
         AR    RE,R1                                                            
         STCM  R1,3,0(RE)          (TRUE MGRPS SAVE MGRP #, NOT MKT)            
*                                                                               
PR235    CLI   SBQSKIP,X'FF'       SKIPPING ALL RECORD TYPES?                   
         BNE   PR236                NO                                          
         CLI   SBQREAD,0           READING ANY RECORD TYPES?                    
         BNE   PR236                NO                                          
         LR    RF,R9                                                            
         AHI   RF,(SBQREAD2-SYSD)                                               
         CLI   0(RF),0             READING ANY RECORD TYPES?                    
         BNE   PR236                NO                                          
         GOTOR RPT,RPINPUT                                                      
         B     PR238                                                            
*                                                                               
PR236    DS    0H                                                               
         TM    OPTIND6,OPT6NM0B    FORCING MKT0 BUYS                            
         BZ    PR236A              NOPE!                                        
         LR    R1,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   R1,4096                                                          
         USING SYSD+4096,R1                                                     
         OI    SBEFLAG3,SBE3M0DL   EXTRACT DOLLARS FOR MARKET ZERO BUYS         
         B     PR236B                                                           
         DROP  R1                                                               
*                                                                               
PR236A   CLI   SBQMED,C'N'         IF MEDIA IS NETWORK                          
         BNE   PR237                                                            
         LA    R1,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDRD(R1),C'C'      AND CANADIAN                     
         BNE   PR237                                                            
         CLI   SBQNETWK,C'N'       AND PROCESSING NETWORK ONLY BUYS             
         BNE   PR237                                                            
PR236B   OI    SBQCAN,SBQCBYM0        READ MARKET ZERO BUYS                     
*                                                                               
PR237    DS    0H                                                               
         GOTO1 SPOTIO,DMCB,SBLOCK    *** CALL SPOTIO ***                        
*                                                                               
PR238    CLI   OFFLINE,C'Y'        ++TEST                                       
         BNE   PRX                                                              
*                                                                               
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   PR330               YES - JUST FREE STORAGE                      
         TM    SBQREAD,SBQRDINF    PROCESSING INFOMERCIALS?                     
         BZ    PR270                NO                                          
         TM    OPTIND3,OPTINFB     SKIP INFO'S W/OUT BUYS?                      
         BNZ   PR270                YES                                         
         MVI   SBMODE,SBPROCIN                                                  
* PROCESS INFO DATA THAT WASN'T PROCESSED DURING SBPROCSP                       
         ICM   R3,15,SBAINFTB      INFOMERCIAL TABLE                            
         BZ    PRX                                                              
         ICM   R0,15,4(R3)         # ENTRIES                                    
         BZ    PRX                                                              
         LA    R3,12(R3)           A(FIRST ENTRY)                               
         USING INFTABD,R3                                                       
         OC    INFSTA,INFSTA       'ALL' STATION ENTRY?                         
         BNZ   PR240                NO                                          
         CLI   INFFLAG,X'FF'       WAS ENTRY USED?                              
         BE    PR260                YES - SKIP IT                               
         ST    R3,SBACURCH          NO - SHOW DATA                              
         XC    SBMKT,SBMKT                                                      
         MVC   SBSTA,=C'AAAAA'                                                  
         MVI   MKTIND,0                                                         
         XC    SBCMLSQ,SBCMLSQ                                                  
         XC    SBCMLSQ2,SBCMLSQ2                                                
         B     PR250                                                            
*                                                                               
PR240    CLI   INFFLAG,X'FF'       WAS ENTRY USED?                              
         BE    PR260                YES - SKIP IT                               
         ST    R3,SBACURCH          NO - SHOW DATA                              
         GOTO1 MSUNPK,DMCB,(X'80',INFMKT),SBMKT,WORK                            
         MVC   SBSTA,WORK                                                       
         CLI   WORK,C'0'                                                        
         BL    *+10                                                             
         MVC   SBCBLNET,WORK+5                                                  
         MVC   SBBMKT,INFMKT                                                    
         MVC   SBBSTA,INFSTA                                                    
         MVI   MKTIND,0                                                         
         XC    SBCMLSQ,SBCMLSQ                                                  
         XC    SBCMLSQ2,SBCMLSQ2                                                
         GOTO1 GETMKTNM                                                         
PR250    GOTO1 ADRIVIN                                                          
PR260    LA    R3,INFTABL(R3)                                                   
         BCT   R0,PR240                                                         
         DROP  R3                                                               
*                                                                               
* IF DDB NEEDHAM, MAY HAVE TO READ ACROSS FILES                                 
PR270    TM    OPTIND4,OPTXFILE                                                 
         BZ    PR275                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   PR275                                                            
         GOTO1 ASWAPFLS                                                         
         BE    PR235                                                            
*                                                                               
PR275    OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU OR DEMO OPTION SET            
         BNZ   PR280                                                            
         TM    DATAIND5,DICLT      OR NONE OF CLT,PRD,EST ARE IN THE            
         BO    PR290               ROWS (IE. NO CLT/PRD/EST FIRSTS)             
         TM    DATAIND2,DIPRD+DIEST                                             
         BNZ   PR290                                                            
*                                                                               
PR280    GOTO1 ADEMOS              YES-GET DEMO NAMES EARLY IF NEEDED           
*                                                                               
PR290    TM    DATAIND9,DIPURP                                                  
         BZ    *+8                                                              
         BRAS  RE,PURPTAB                                                       
*                                                                               
         MVI   PRTSW,C'Y'          INITIALIZE PRINT SWITCH                      
         OI    COLIND2,COLIBLB0    ALL BILLABLE COLS START OFF ZERO             
         XC    SVMKT,SVMKT                                                      
*                                                                               
         MVI   GLHOOK,GLOUTPUT                                                  
         GOTOR RPT,RPDRHOOK                                                     
         BNE   PR330               OPPORTUNITY TO SKIP OUTPUT STAGE             
*                                                                               
         TM    OPTIND6,OPT6FILE    SEND OUTPUT DATA TO FILE?                    
         BZ    PR292                NO                                          
*                                                                               
** MQFILENM IS ORG'D OVER MAAORLK (ONLY USED DURING INPUT)                      
         MVC   MQFILENM,BLANKS                                                  
         LA    R2,MQFILENM                                                      
         USING FILNAMD,R2                                                       
         MVC   FNPFIX,=C'SFTPDISK.PROD.'                                        
         BRAS  RE,TESTRUN                                                       
         BNE   *+10                                                             
         MVC   FNPFIX+9(4),=C'TEST'                                             
*                                                                               
         L     RF,TWAMASTC         RF = A(MASTC)                                
         USING MASTD,RF            MASTD DSECT                                  
         L     RE,MCAEXTRA         RE = A(EXTRA DATA AREA)                      
         USING MCEXTRA,RE          MCEXTRA DSECT                                
         CLC   MCAGYCOD,=C'AINY'   AGENCY LABEL (CTAGCCOD) = AINY?              
         BNE   PR291               NO                                           
         DROP  RE,RF               DROP USINGS                                  
*                                                                               
         LA    R3,FNPRG            FILE NAME WILL GO HERE                       
         MVC   0(8,R3),WRITIT+32   1 BYTE PAST EDIHUBFT********BILLING          
         LA    R3,7(R3)            POSSIBLY LAST CHAR OF FILE NAME              
         CLI   0(R3),X'40'         LAST CHARACTER OF FILENAME?                  
         BH    *+8                 YES                                          
         BCT   R3,*-8              NO - BACK UP 1 CHARACTER                     
*                                                                               
         LA    RE,FNPRG            RE = A(START OF FILE NAME)                   
         LR    RF,R3               RF = LAST CHAR OF FILE NAME                  
*                                                                               
PR290A   CR    RF,RE               CHECKED EVERY CHAR OF FILE NAME?             
         BL    PR290B              YES                                          
         CLI   0(RF),X'40'         CHARACTER DATA?                              
         BH    *+6                 YES                                          
         DC    H'0'                NO - CANNOT HAVE IMBEDDED SPACES!            
         BCT   RF,PR290A           CHECK PREVIOUS CHARACTER                     
*                                                                               
PR290B   MVC   1(2,R3),=C'.D'      MOVE .D INTO FILE NAME                       
         LA    R3,3(R3)            MOVE DATE STAMP HERE                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(X'20',0(R3))                             
*                                                                               
         MVC   6(2,R3),=C'.T'      MOVE .T INTO FILE NAME                       
         LA    R3,8(R3)            MOVE TIME STAMP HERE                         
*                                                                               
         TIME  DEC                 R0=HHMMSSHH                                  
         ST    R0,FULL             FULL = HHMMSSHH                              
         GOTO1 HEXOUT,DMCB,FULL,0(R3),4                                         
         MVI   7(R3),C' '          GET RID OF HUNDREDTHS                        
*                                                                               
         B     PR291A              DONE WITH FILE NAME                          
*                                                                               
PR291    MVC   FNPRG(FILNAMX-FNPRG),=CL30'BUY.SP.SJSJ.DYYMMDD.THHMMSST'         
         L     RF,TWAMASTC                                                      
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   FNAGYLBL,MCAGYCOD-MCEXTRA(RF)                                    
         TM    OPTIND6,OPT6SCJE    SC JOHNSON ESTIMATE FILE?                    
         BZ    *+10                NO                                           
         MVC   FNPRG(3),=C'EST'    YES - CREATE ESTIMATE FILE                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(X'20',FNYYMMDD)                          
         TIME  DEC                 R0=HHMMSSHH                                  
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,FNTIME,4                                        
         MVI   FNTIME+7,C' '       GET RID OF HUNDREDTHS                        
*                                                                               
PR291A   LA    RE,MQFILENM                                                      
         ST    RE,GLAOUTP          TELL DRIVER FILE NAME                        
*                                                                               
PR292    MVI   GLMODE,GLOUTPUT     CALL DRIVER FOR OUTPUT                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
         TM    OPTIND6,OPT6FILE    DID WE SEND OUTPUT TO FILE?                  
         BZ    PR296                NO                                          
*                                                                               
* SEND MQ MESSAGE WITH FILE NAME                                                
MQ       USING MQMSGD,ELEM                                                      
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
         MVC   MQ.MQHID,=CL6'DANOT1'                                            
         MVC   MQ.MQSYS,=CL3'SPT'                                               
         L     RF,TWAMASTC                                                      
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   MQ.MQAGYID,MCAGYCOD-MCEXTRA(RF)                                  
*                                                                               
         TM    OPTIND6,OPT6SCJE    SC JOHNSON ESTIMATE FILE?                    
         BZ    *+14                NO                                           
         MVC   MQ.MQQUAL(11),SCJTIT+16 YES - USE TITLE FROM SCJE SCREEN         
         B     *+10                                                             
         MVC   MQ.MQQUAL,WRITIT+16                                              
         OC    MQ.MQQUAL,BLANKS                                                 
*                                                                               
         CLC   MQ.MQAGYID,=C'AINY' AGENCY ACTIVE?                               
         BNE   MQ10                NO                                           
         LA    RE,WRITIT+40        FOLDER NAME                                  
         CLC   =C'BILLING',0(RE)   BILLING FOLDER?                              
         BNE   MQ05                NO                                           
         MVC   MQ.MQAGYID,=C'AIN1' YES - SEND AIN1 AS 4-CHARACTER CODE          
         B     MQ06                GO SET DATE AND TIME                         
*                                                                               
MQ05     CLC   =C'PNL',0(RE)       PNL FOLDER?                                  
         BNE   MQ10                NO - LEAVE AINY AS IS                        
         MVC   MQ.MQAGYID,=C'AIN2' YES - SEND AIN2 AS 4-CHARACTER CODE          
*                                                                               
MQ06     LA    RE,FNPRG            FILE NAME                                    
*                                                                               
MQ06A    CLC   =C'.D',0(RE)        DATE STAMP?                                  
         BE    MQ07                YES                                          
         LA    RE,1(RE)            BUMP FILE NAME POINTER                       
         B     MQ06A               KEEP CHECKING FOR DATE STAMP                 
*                                                                               
MQ07     MVC   MQ.MQDATE,2(RE)     DATE STAMP YYMMDD                            
         MVC   MQ.MQTIME,10(RE)    TIME STAMP HHMMSS                            
         B     MQ20                DATE & TIME SET                              
*                                                                               
MQ10     MVC   MQ.MQDATE,FNYYMMDD                                               
         MVC   MQ.MQTIME,FNTIME                                                 
*                                                                               
MQ20     MVC   MQ.MQDATA1(L'WRIDESC),WRIDESC                                    
         OC    MQ.MQDATA1,BLANKS                                                
*                                                                               
* TEST FILE GENERATED - IF NOT, GIVE ERROR, ELSE GIVE FILENAME                  
         MVC   MQ.MQDATA2(6),=C'NODATA'                                         
         ICM   RF,15,TWADCONS                                                   
         BZ    PR294                                                            
         ICM   RF,15,TISPRACT-TWADCOND(RF)    IF A(PR ACT SW) RESOLVED          
         BZ    PR294                                                            
         CLI   0(RF),C'N'          NOTHING PRINTED                              
         BE    PR294                                                            
         MVC   MQ.MQDATA2,BLANKS                                                
         MVC   MQ.MQFILE(FILNAMX-FNPRG),FNPRG                                   
*                                                                               
PR294    GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         BE    PR296                                                            
         DCHO                                                                   
         DROP  MQ                                                               
         DROP  R2                                                               
*                                                                               
FILNAMD  DSECT                                                                  
FNPFIX   DS    CL14                PREFIX (SFTPDISK.PROD.)                      
FNPRG    DS    CL3                 BUY                                          
         DS    CL1                 .                                            
FNSYS    DS    CL2                 SP                                           
         DS    CL1                 .                                            
FNAGYLBL DS    CL4                 AGY LABEL (CTAGCCOD)                         
         DS    CL2                 .D                                           
FNYYMMDD DS    CL6                 YYMMDD                                       
         DS    CL2                 .T                                           
FNTIME   DS    CL7                 HHMMSST                                      
FILNAMDQ EQU   *-FILNAMD                                                        
FILNAMX  EQU   *                                                                
*                                                                               
* NOTE: THE 'ROUTE' IS PASSED IN THE MQOPEN AS THE HEADER                       
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM=SPT                                   
MQAGYID  DS    CL4                 AGENCY ID                                    
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 DATE                                         
MQTIME   DS    CL6                 TIME HHMMSS                                  
MQDATA1  DS    CL32                                                             
MQDATA2  DS    CL32                                                             
MQFILE   DS    CL64                DSN                                          
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
T20401   CSECT                                                                  
*                                                                               
PR296    GOTOR RPT,RPAFTOUT                                                     
*                                                                               
* CLOSE BOX & PRINT OPTIONAL DISCLAIMER                                         
         ICM   R3,15,TWADCONS                                                   
         BZ    PR330                                                            
         ICM   R3,15,TISPRACT-TWADCOND(R3)    IF A(PR ACT SW) RESOLVED          
         BZ    PR330                                                            
         CLI   0(R3),C'N'          NOTHING PRINTED                              
         BE    PR330                                                            
*                                                                               
         L     R3,ASPOOLD                                                       
         L     R1,GENHEAD          RESET HOOK ADDRESS IN SPOOL BLOCK            
         ST    R1,HEADHOOK-SPOOLD(R3)                                           
         TM    DOWNOPT,GLDLACTV    SKIP BOX CLOSE IF DOWNLOADING                
         BNZ   PR300                                                            
*                                                                               
         L     R3,ABOX-SPOOLD(R3)                                               
         USING BOXD,R3                                                          
         CLI   BOXYORN,C'Y'        SKIP BOX CLOSE IF NO BOXES USED              
         BNE   PR300                                                            
         MVI   BOXREQ,C'C'         CLOSE LAST BOX                               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         DROP  R3                                                               
*                                                                               
PR300    TM    OPTIND5,OPTNOWRN    OPTION TO SUPPRESS WARNINGS?                 
         BNZ   PR330                                                            
*                                                                               
         TM    OPTIND4,OPTDISCL    PRINT DISCLAIMER?                            
         BZ    PR310                NO                                          
         MVC   DUB(2),=Y(DISCL1L)                                               
         BAS   RE,PRCENT                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         MVC   0(DISCL1L,R3),DISCL1                                             
         GOTO1 (RF),(R1),ASPOOLD                                                
         MVC   0(DISCL2L,R3),DISCL2                                             
         GOTO1 (RF),(R1),ASPOOLD                                                
*                                                                               
PR310    TM    SBEFLAG,SBECLSK     CLIENTS IN CLTGRP SKIPPED?                   
         BZ    PR320                NO                                          
         CLI   DOWNOPT,0           DOWNLOAD?                                    
         BNE   PR320               YES                                          
         MVC   DUB(2),=Y(CLTSKIPL)                                              
         BAS   RE,PRCENT                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         MVC   0(CLTSKIPL,R3),CLTSKIP                                           
         GOTO1 (RF),(R1),ASPOOLD                                                
*                                                                               
         DROP  R4                                                               
PR320    L     R3,ASPOOLD                                                       
         L     R3,ABOX-SPOOLD(R3)                                               
         USING BOXD,R3                                                          
         MVC   BOXYORN,BOXOPT      RESET BOX STATE (FOR NEXT REQ)               
         DROP  R3                                                               
*                                                                               
PR330    L     R3,AGLOBAL          FREE UP GLOBAL STORAGE                       
         L     R5,=A(GLOBALLN)                                                  
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
*                                                                               
         L     R3,SBAMGTAB         FREE UP MARKET GROUP TABLE                   
         L     R5,=F'20000'                                                     
         GOTO1 COVAIL,(R1),C'FREE',(R3),(R5)                                    
*                                                                               
         ICM   R3,15,SBACMLTB      TEST COMMERCIAL TABLE                        
         BZ    PR340                                                            
         L     R5,4(R3)            YES-FREE IT UP                               
         MHI   R5,CMLTABL                                                       
         LA    R5,8(R5)                                                         
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
*                                                                               
PR340    OC    SBPGRPEX(2),SBPGRPEX  TEST PRDGRP EXCEPTIONS(S)                  
         BZ    PR370                                                            
         LA    R3,SBAMGTB2           YES-FREE THE EXTRA MARKET GROUP            
         LA    R5,SBEXMAX                TABLES                                 
*                                                                               
PR360    ICM   R4,15,0(R3)                                                      
         BZ    PR370                                                            
         L     R2,=F'20000'                                                     
         GOTO1 (RF),(R1),C'FREE',(R4),(R2)                                      
         LA    R3,4(R3)                                                         
         BCT   R5,PR360                                                         
*                                                                               
PR370    ICM   R3,15,SBACHKTB      TEST CHECK TABLE                             
         BZ    PR380                                                            
         L     R5,0(R3)            YES-FREE IT                                  
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
*                                                                               
PR380    ICM   R3,15,SBAINFTB      TEST INFOMERCIAL UPSELL TABLE                
         BZ    PR390                                                            
         L     R5,0(R3)            YES-FREE IT                                  
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
*                                                                               
PR390    ZICM  R3,AXTCOLS,3        TEST EXTENDED COLUMN TABLE                   
         BZ    PR392                                                            
         SHI   R3,4                POINT TO BEGINNING OF TABLE                  
         L     R5,0(R3)            YES-FREE IT (LENGTH OF TABLE)                
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
*                                                                               
PR392    ICM   R3,15,APQINDEX      TEST PQINDEX TABLE                           
         BZ    PR394                                                            
         AHI   R3,-4               POINT TO BEGINNING OF TABLE                  
         L     R5,0(R3)            YES-FREE IT (LENGTH OF TABLE)                
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
*                                                                               
PR394    LA    RE,SBLOCK           GET ADDRESSABILITY TO END OF SBLOCK          
         ICM   R3,15,SBSQDBFA-SBLOCK(RE)   IF SQAD BUFFER ACTIVE                
         BZ    PR395                                                            
         AHI   R3,-4               POINT TO BEGINNING OF TABLE                  
         L     R5,0(R3)            YES-FREE IT (LENGTH OF TABLE)                
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
*                                                                               
PR395    ICM   R3,15,ACOMREC                                                    
         BZ    PR396                                                            
         LAY   R5,6000                                                          
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
*                                                                               
PR396    ICM   R3,15,NINVREC                                                    
         BZ    PR400                                                            
         LAY   R5,6000                                                          
         GOTO1 (RF),(R1),C'FREE',(R3),(R5)                                      
*                                                                               
PR400    DS    0H                                                               
         TM    OPTIND4,OPTXEST     SKIP TEST?                                   
         BNZ   PR410                                                            
*                                                                               
         TM    DATAIND9,DIWIPW     WESTERN PW?                                  
         BZ    PR410                NO                                          
         CLC   SBQEST,SBQESTND     SINGLE EST?                                  
         BE    PR410                YES                                         
         MVI   ERROR,INVEST        MUST BE ONE EST FOR PW                       
         J     TRAPERR                                                          
*                                                                               
PR410    GOTOR RPT,RPFINAL         FINAL CALL TO USER APPLICATION               
*                                                                               
         OC    RPT2ID,RPT2ID       SKIP IF NO SECOND REPORT TO BE DONE          
         BZ    PRX                                                              
         TM    OPTIND5,OPT5CONT    NO CONTINUATION REPORTS HERE                 
         BNZ   PRX                                                              
         XC    GTMSGNO,GTMSGNO     MAKE SURE CLEAR ON CALL                      
         BRAS  RE,RP2GET           READ IN 2ND REPORT                           
*                                                                               
PRX      J     XIT                                                              
*                                                                               
* COMMENT LENGTH IN DUB                                                         
* PRINT POSN RETURNED IN R3                                                     
PRCENT   DS    0H                  CENTER END LINES OR PRINT ON LEFT            
         L     R3,ASPOOLD                                                       
         L     R3,ABOX-SPOOLD(R3)                                               
         USING BOXD,R3                                                          
         MVI   BOXYORN,C'N'        TURN OFF BOXES                               
         DROP  R3                                                               
*                                                                               
* FIND STARTING PRINT POSITION FOR COMMENTS                                     
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         L     R3,GLAP1            A(FIRST DETAIL LINE)                         
         L     R1,GLAINTD          DRIVER INTERNAL RECORD                       
         USING GLINTD,R1                                                        
*                                                                               
         LH    RF,GLPWIDTH         REPORT WIDTH                                 
         CH    RF,DUB              REPORT WIDER THAN COMMENT LENGTH?            
         BL    *+12                 NO                                          
         AH    R3,GLPDISP          POINT TO REPORT LEFT MARGIN                  
         B     PRCENT10                                                         
         DROP  R1                                                               
*                                                                               
         L     RF,PWIDTH           SET TO CENTER ON PAGE                        
         CLI   LEFTOPT,C'Y'        IF LEFT OPTION ON                            
         BNE   *+8                                                              
         LH    RF,DUB              FORCE LEFT SIDE START                        
*                                                                               
PRCENT10 DS    0H                                                               
         SH    RF,DUB              CENTER COMMENTS UNDER REPORT                 
         SRL   RF,1                                                             
         AR    R3,RF               STARTING PRINT POSITION                      
         BR    RE                                                               
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
* I/O HOOK                                                                      
*                                                                               
IOHOOK   NTR1                                                                   
         GOTOR RPT,RPINPUT         CALL USER REPORT OVERLAY FOR INPUT           
         CLI   RPMODE,RPSKIOHK                                                  
         BE    HKX                                                              
*                                                                               
         TM    SBEUDEF,SBEUPCOM+SBEUECOM+SBEUMCOM  EXTRACT UCOMS?               
         BZ    HK10                                 NO                          
         TM    DATAIND9,DIUCOM     NEED USER COMMENT DATA?                      
         BZ    HK10                 NO                                          
         OC    SBBCLT,SBBCLT       IF WE DON'T HAVE CLT YET, GET OUT            
         BZ    HK10                                                             
         BRAS  RE,GETUCOM                                                       
*                                                                               
HK10     L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCAG     AGENCY FIRST                                 
         BE    AGNCY                                                            
         CLI   SBMODE,SBPROCCG     CLIENT GROUP FIRST                           
         BE    CLTGRP                                                           
         CLI   SBMODE,SBPROCCL     CLIENT FIRST                                 
         BE    CLIENT                                                           
         CLI   SBMODE,SBPROCPG     PRODUCT GROUP FIRST                          
         BE    PRDGRP                                                           
         CLI   SBMODE,SBPROCES     ESTIMATE FIRST                               
         BE    ESTIMATE                                                         
         CLI   SBMODE,SBPROCMG     MARKET GROUP FIRST                           
         BE    MKTGRP                                                           
         CLI   SBMODE,SBPROCMK     MARKET FIRST                                 
         BE    MARKET                                                           
         CLI   SBMODE,SBPROCSG     STATION GROUP FIRST                          
         BE    STAGRP                                                           
         CLI   SBMODE,SBPROCST     STATION FIRST                                
         BE    STATION                                                          
         CLI   SBMODE,SBPROCSP     BUY RECORD                                   
         BE    PROCBUY                                                          
         CLI   SBMODE,SBPROCGL     GOAL RECORD                                  
         BE    PROCGOAL                                                         
         CLI   SBMODE,SBPROCBL     STATION BILL RECORD                          
         BE    PROCBILL                                                         
         CLI   SBMODE,SBPROCNV     INVOICE RECORD                               
         BE    PROCINV                                                          
         CLI   SBMODE,SBPROCBH     BILL HEADER RECORD                           
         BE    PROCBH                                                           
*                                                                               
         CLI   SBMODE,SBPROCB2     BILL HEADER RECORD (TABLE PASS)              
         BNE   *+12                                                             
         BRAS  RE,PROCB2                                                        
         B     HKX                                                              
*                                                                               
         CLI   SBMODE,SBPROCSL     STATION LOCKIN RECORD                        
         BE    PROCSLK                                                          
         CLI   SBMODE,SBPROCCS     CLEARANCE STATUS RECORD                      
         BE    PROCCS                                                           
         CLI   SBMODE,SBPROCIN     INFOMERCIAL RECORD                           
         BE    PROCIN                                                           
         CLI   SBMODE,SBPROCPW     WESTERN PW RECORD (STA)                      
         BE    PROCWIPW                                                         
         CLI   SBMODE,SBPROCP2     WESTERN PW RECORD (MKT)                      
         BE    PROCWIP2                                                         
         CLI   SBMODE,SBPROCOM     OM RECORD                                    
         BE    PROCOM                                                           
         CLI   SBMODE,SBPROCOB     TSAR OFF OM RECS FROM BUY KEY                
         BE    TSAROM                                                           
         CLI   SBMODE,SBPROCDO     DELETE OM RECS FROM TSAR BUFFER              
         BE    DELOM                                                            
*                                                                               
HKX      J     XIT                                                              
         EJECT                                                                  
*                                                                               
ESTUFHK  NTR1                                                                   
         GOTO1 GETESTNM                                                         
         J     XIT                                                              
*                                                                               
AGNCY    DS    0H                  ** AGENCY FIRST **                           
         LA    RE,MKTTAB           CLEAR MARKET TABLE                           
         LA    RF,MKTTABL                                                       
         XCEFL ,                                                                
*                                                                               
         LA    R1,SBAGYREC                                                      
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'      TEST CANADIAN                                
         BNE   *+16                                                             
         CLI   SBQCURR,0           YES-TEST CURRENCY ALREADY SET                
         BNE   *+8                                                              
         MVI   SBQCURR,C'C'        NO-SET CURRENCY TO CANADIAN DOLLARS          
*                                                                               
         MVI   SBQGETNM,C'N'                                                    
         TM    DATAIND4,DINAMES    TEST BUYER/BILLER NAMES                      
         BZ    AGY4                NO                                           
         MVI   SBBYRNM,C'*'        YES-INIT TO STARS                            
         MVC   SBBYRNM+1(L'SBBYRNM-1),SBBYRNM                                   
         MVC   SBBLRNM,SBBYRNM                                                  
         CLI   AGYPROF+14,C'Y'     FEATURE MUST BE IN AGYHDR                    
         BNE   AGY4                                                             
         DROP  R1                                                               
         L     R1,ASPOOLD                                                       
         LA    R1,SPOOLID-SPOOLD(R1)                                            
         CLI   0(R1),C'='          TEST SPOOL ID BEGINS WITH '='                
         BNE   AGY2                NO                                           
         MVC   SBBYRNM,BLANKS      YES-OVERRIDE BUYER/BILLER NAMES              
         MVC   SBBYRNM(3),0(R1)                                                 
         MVC   SBBLRNM,SBBYRNM                                                  
         B     AGY4                                                             
*                                                                               
AGY2     MVI   SBQGETNM,C'Y'       GET BUYER/BILLER NAMES                       
*                                                                               
AGY4     SR    RE,RE                                                            
         ICM   RE,8,BYDEMSTK       TEST BUY DEMO STACK IN COLUMNS               
         BZ    AGYX                                                             
         LA    R0,8                YES-ADD DEMO CATEGORIES TO DEMO              
         LA    R1,SBPDEMOS             LIST IF NECESSARY                        
         LLC   R2,NDEMOS                                                        
*                                                                               
AGY6     LTR   RE,RE                                                            
         BNM   AGY10                                                            
         OC    0(3,R1),0(R1)                                                    
         BZ    AGY12                                                            
         MVI   HALF,C'R'                                                        
         TM    OPTIND2,OPTISTR                                                  
         BZ    *+12                                                             
         CLI   1(R1),C'I'                                                       
         BE    AGY8                                                             
         MVI   HALF,C'I'                                                        
         TM    OPTIND2,OPTISTI                                                  
         BZ    AGY10                                                            
         CLI   1(R1),C'I'                                                       
         BE    AGY10                                                            
*                                                                               
AGY8     MVC   HALF+1(1),2(R1)                                                  
         LA    R3,SBPDEMOS                                                      
         LR    RF,R2                                                            
         CLC   HALF,1(R3)                                                       
         BE    AGY10                                                            
         LA    R3,3(R3)                                                         
         BCT   RF,*-14                                                          
         MVI   0(R3),0                                                          
         MVC   1(2,R3),HALF                                                     
         LA    R2,1(R2)                                                         
*                                                                               
AGY10    SLL   RE,1                                                             
         LA    R1,3(R1)                                                         
         BCT   R0,AGY6                                                          
*                                                                               
AGY12    STC   R2,SBENDEM                                                       
*                                                                               
AGYX     B     HKX                                                              
*                                                                               
         EJECT                                                                  
CLTGRP   DS    0H                  ** CLIENT GROUP FIRST **                     
         MVC   HALF,SBBCGR                                                      
         OC    SBBCGR,SBCG1MSK                                                  
         GOTO1 PUTCGRNM,SBCGR1NM                                                
         MVC   SBBCGR,HALF                                                      
         CLC   SBCGR1LN,SBCGR2LN                                                
         BE    CGX                                                              
         GOTO1 PUTCGRNM,SBCGR2NM                                                
CGX      B     HKX                                                              
         EJECT                                                                  
CLIENT   DS    0H                  ** CLIENT FIRST **                           
         GOTO1 PUTCLTNM                                                         
         CLC   SBMED,SVMED2                                                     
         BE    CL01                                                             
         ZAP   CLTCNT,=P'0'                                                     
         MVC   SVMED2,SBMED                                                     
         XC    MGR1SAVE,MGR1SAVE   CLEAR ON MEDIA CHANGE!                       
         XC    MGR2SAVE,MGR2SAVE   CLEAR ON MEDIA CHANGE!                       
*                                                                               
CL01     AP    CLTCNT,=P'1'                                                     
         MVI   GLMINTLV,0                                                       
         MVI   SVPRD,0                                                          
         CLI   XCLT,C'*'           TEST CLIENT OFFICE FILTER                    
         BNE   CL08                                                             
         CLC   SBCOFF,XCLT+1       YES - TEST CLIENT OFFICE IS IT               
         BE    CL08                                                             
         OI    GLINDS,GLINODET     NO - TURN ON DETAIL SUPPRESSION              
         LA    R0,L'LEVELS                                                      
         LA    R1,LEVELS                                                        
         LA    RE,0                                                             
*                                                                               
CL02     CLI   0(R1),QCLTOFF       FIND CLIENT OFFICE LEVEL                     
         BNE   *+12                                                             
         STC   RE,GLMINTLV         DON'T GENERATE TOTALS FOR CLIENT             
         B     CL08                OFFICE LEVEL AND ABOVE                       
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CL02                                                          
*                                                                               
CL08     OC    SBQDEMOS,SBQDEMOS   TEST DEMO MENU OVERRIDE                      
         BNZ   *+18                                                             
         OC    SBPDEMOS,SBPDEMOS   OR DEMO OPTION IS SET                        
         BNZ   *+8                                                              
         MVI   ESTSTART,FF         NO - INITIALIZE CONTROL ESTIMATE             
         MVC   SBQMKTWT,WGTOPT                                                  
         CLI   WGTOPT,0            TEST MKT WGT OPTION SET                      
         BE    CL10                                                             
         CLI   WGTOPT,C'Y'         YES                                          
         BE    CL20                                                             
         B     CL30                                                             
*                                                                               
CL10     MVI   SBQMKTWT,C'N'                                                    
         TM    DATAIND4,DIWEIGHT   TEST ANY WEIGHTED COLUMNS                    
         BZ    CL30                                                             
         CLI   SBSPPROF+1,C'N'     TEST CLIENT WANTS MARKET WEIGHTING           
         BE    CL30                                                             
         MVI   SBQMKTWT,C'Y'                                                    
*                                                                               
CL20     CLI   SBSPPROF+14,C'Y'    TEST CLIENT WANTS PRIMARY DEMO               
         BNE   CL30                MARKET WEIGHTING                             
         MVI   SBQMKTWT,C'D'                                                    
*                                                                               
CL30     TM    COLIND,COLIDEM      TEST DEMOS IN COLS                           
         BNZ   *+8                                                              
         MVI   SBQMKTWT,C'N'       NO-WE DON'T NEED MARKET WEIGHTS              
*                                                                               
         CLI   SVCOPT,0            TEST RATING SERVICE OVERRIDE                 
         BE    CL31                                                             
         MVC   SBCPROF+3(1),SVCOPT YES-PUT IT IN PROFILE                        
         OI    SBCPROF+3,X'F0'                                                  
         OI    SBINDS,SBIRSOVR     INDICATE THERE'S AN OVERRIDE                 
         TM    SVCOPT,X'80'        TEST OPTION WAS FORCE                        
         BO    CL31                                                             
         OI    SBINDS,SBIAGYMK     YES-PASS AGENCY MKT NUM TO LOOKUPS           
*                                                                               
CL31     TM    OPTIND,OPTINODF     TEST 'NO DEMO FACTORING' OPTION SET          
         BZ    *+8                                                              
         MVI   SBCEXTRA+12,C'N'    YES-REFLECT IN PROFILE                       
*                                                                               
         OC    ACTBOOK,ACTBOOK        ACTBOOK OPTION SET?                       
         BZ    *+10                   NO                                        
         MVC   SBSPPROF+3(1),ACTBOOK  OPTION BEATS PROFILE                      
*                                                                               
         XC    WORK,WORK           PREPARE FOR PROFILE READ                     
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),SBAGY                                                  
         MVC   WORK+6(1),SBQMED                                                 
*                                                                               
         TM    DATAIND,DIDEMA+DIDEMR    TEST DEMO LOOK-UP                       
         BZ    CL40                                                             
         LR    RF,R9                                                            
         AHI   RF,SBQBOOK-SYSD                                                  
         CLC   0(3,RF),=C'ACT'          AND ACTUAL BOKK                         
         BNE   CL40                                                             
         OC    SBQBCLT,SBQBCLT          AND MULTI CLIENT REQUEST                
         BNZ   CL40                                                             
         SR    R0,R0                    AND CLIENT NOT IN THE HEADLINES         
         ICM   R0,1,LSTHEDLV                                                    
         BZ    CL32                                                             
         LA    R1,LEVELS                                                        
CL31A    CLI   0(R1),QCLT                                                       
         BE    CL40                                                             
         CLI   0(R1),QCLTNM                                                     
         BE    CL40                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,CL31A                                                         
*                                                                               
CL32     CLI   SBQCLT,C'*'         YES - READ NON CLIENT SPECIFIC               
         BNE   *+14                      SPOT CONTROL PROFILE                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         OC    ACTBOOK,ACTBOOK           ACTBOOK OPTION SET?                    
         BNZ   *+10                      YES, IT WINS!                          
         MVC   SBSPPROF+3(1),WORK+19     OVERRIDE THE ACTUAL BOOK TABLE         
*                                                                               
CL40     MVC   WORK+7(3),SBCLT     FILL IN REST OF PROFILE KEY                  
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         MVC   WORK+2(2),=C'B3'                                                 
         CLI   PEROPT,PEROB3       YES-USE B3 OR AB PROFILE                     
         BE    CL41                                                             
         MVC   WORK+2(2),=C'AB'                                                 
         CLI   PEROPT,PEROAB                                                    
         BNE   CL42                                                             
*                                                                               
CL41     GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
*                                                                               
         MVC   SBSPPROF+2(1),WORK+16   OVERRIDE DATE CONTROL                    
         MVC   SBSPPROF+6(3),WORK+17   FISCAL BASE MON/DATE/DAY                 
         B     CL43                                                             
*                                                                               
CL42     CLI   PEROPT,PEROCAL      TEST FORCE CALENDAR MONTHS                   
         BNE   *+8                                                              
         MVI   SBSPPROF+2,2                                                     
         CLI   PEROPT,PEROBRD      OR BROADCAST MONTHS                          
         BNE   CL43                                                             
         MVI   SBSPPROF+2,0                                                     
         XC    SBSPPROF+6(2),SBSPPROF+6                                         
         MVI   SBSPPROF+8,1                                                     
*                                                                               
CL43     OC    SBADATE,SBADATE     TEST A(DATES) SET YET                        
         BNZ   CL44                                                             
         GOTO1 SETDATE             NO - SET THE DATES                           
         GOTO1 ADATES              SET A(DATE TABLE) AND N'DATES                
*                                                                               
CL44     TM    DATAIND7,DIFLIGHT   TEST FOR CHILD SPOT FLIGHTS                  
         BZ    CL46                                                             
         GOTO1 AGETFLTS            YES-GET THE FLIGHTS                          
*                                                                               
CL46     TM    OPTIND3,OPTIXSPL    TEST CANADIAN SPILL LOOKUP                   
         BZ    CL47                                                             
         GOTO1 GETXSPIL            YES-GET CANADIAN SPILL STATIONS              
         MVI   SBD0PROF,C'N'       MAKE SURE SPILL DOES NOT REQUIRE             
*                                  PURCHASED DEMOS                              
CL47     TM    ROWIND,ROWICML      NEED TO READ COMMERCIAL RECORDS?             
         BZ    CLX                 NO                                           
         GOTO1 GETCML,X'FF'        CALL TO CLEAR CML TSAR BUFFER                
*                                                                               
CLX      B     HKX                                                              
         EJECT                                                                  
PRDGRP   DS    0H                  ** PRODUCT GROUP FIRST **                    
         MVC   HALF,SBBPGR                                                      
         OC    SBBPGR,SBPG1MSK                                                  
         GOTO1 PUTPGRNM,SBPGR1NM                                                
         MVC   SBBPGR,HALF                                                      
         CLC   SBPGR1LN,SBPGR2LN                                                
         BE    PGX                                                              
         GOTO1 PUTPGRNM,SBPGR2NM                                                
PGX      B     HKX                                                              
         EJECT                                                                  
ESTIMATE DS    0H                  ** ESTIMATE FIRST **                         
         TM    DATAIND3,DIESTNM    TEST ESTIMATE DETAILS WILL BE NEEDED         
         BO    ES01                                                             
         TM    DATAIND4,DIESTDT                                                 
         BO    ES01                                                             
         TM    SBEUDEF,SBEUEST1+SBEUEST2  OR ESTIMATE USER FIELDS               
         BNZ   ES01                                                             
         TM    DTAIND10,DIEBKTYP   ESTIMATE BOOK TYPE NEEDED?                   
         BNZ   ES01                YES, PUT IN EST BUFFER                       
         TM    DATAIND5,DITARGET                                                
         BZ    ES02                                                             
*                                                                               
ES01     GOTO1 PUTESTNM            YES - PUT ESTIM DETAILS IN NAME POOL         
*                                                                               
ES02     CLI   SBQSEPES,C'Y'       TEST SEPERATE ESTIMATES                      
         BE    ES06                                                             
         CLI   SBQBPRD,0           NO - TEST SINGLE PRODUCT                     
         BE    ES04                                                             
         CLI   SBQBPRD,FF                                                       
         BE    ES04                                                             
         CLI   SBBPRD,FF                YES - TEST PRD=POL                      
         BE    ES06                           YES - ESTNUM NOT RELEVENT         
*                                                                               
ES04     CLC   SBBEST,ESTSTART     KEEP TRACK OF LOWEST ESTIMATE                
         BNL   ES06                                                             
         MVC   ESTSTART,SBBEST                                                  
*                                                                               
ES06     CLI   SBESTOWD,0          TEST EST HAS OUT OF WEEK ROTATION            
         BE    ES08                NO                                           
         TM    ININD,INIOWSDY      YES-TEST DATES ALREADY SET FOR               
         BO    ES10                    OUT OF WEEK ROTATION                     
         MVC   SBQSTART,QSTART     NO-SET THE DATES NOW                         
         MVC   SBQEND,QEND                                                      
         GOTO1 SETDATE                                                          
         GOTO1 ADATES              SET A(DATE TABLE) AND N'DATES                
*                                                                               
ES08     OI    ININD,INIOWSDY      ALWAYS SET INDICATOR, SO THAT LATER          
*                                  OUT-OF-WEEK ROTATOR ESTIMATES ARE            
*                                  NOT MIXED IN WITH NON-OOW ROTATORS           
*                                                                               
ES10     TM    SBQREAD,SBQRDPG     TEST READ PG ESTIMATE RECORDS                
         BZ    ES12                                                             
         L     R1,=A(PGDATA)                                                    
         GOTO1 APGEST              YES-CALL DRIVER FOR PG EST                   
*                                                                               
ES12     TM    DATAIND7,DIAUTH     TEST AUTHORIZATION DOLLARS NEEDED            
         BZ    ES20                                                             
         BRAS  RE,EXTPRD           EXTRACT PRODUCT DETAILS                      
         ICM   R1,15,SBAESTTB      PRD/EST BUFFER PASSED?                       
         BZ    ES15                NO                                           
         LLC   RE,SBBPRD           PRODUCT                                      
         CLI   SBQBPRD,FF          'POL' REQUEST?                               
         BNE   *+8                 NO                                           
         LA    RE,255              YES - LOOK UNDER 'POL' ESTIMATES             
         BCTR  RE,0                -1 FOR INDEX                                 
         SLL   RE,8                X 256                                        
         LLC   R0,SBBEST           ESTIMATE                                     
         BCTR  R0,0                -1 FOR INDEX                                 
         AR    RE,R0               ADD EST TO PRD INDEX                         
         AR    RE,R1               INDEX INTO EST BUFFER                        
         CLI   0(RE),0             ACTIVE ESTIMATE?                             
         BE    ES15                NO                                           
         ICM   R5,15,SBAESTBF      HAVE EST DETAILS BUFFER?                     
         BZ    ES15                NO                                           
         LLC   RF,0(RE)            INDEX INTO EST DETAILS BUFFER                
         BCTR  RF,0                -1 FOR INDEX                                 
         MHI   RF,ESTBUFFL         * EST DETAIL ENTRY LENGTH                    
         AR    R5,RF               INDEX TO EST DETAILS ENTRY                   
         USING ESTBUFFD,R5         EST DETAILS DSECT                            
         MVC   SBESTFLT,EBFILT     EST FILTERS                                  
         MVC   SBRTLSCH,EBRTLSCH   RETAIL SCHEME CODE                           
         DROP  R5                  DROP R5                                      
*                                                                               
ES15     GOTO1 AAUTH               YES-CALL DRIVER FOR AUTH DOLLARS             
*                                                                               
ES20     DS    0H                                                               
*                                                                               
ESX      B     HKX                                                              
         EJECT                                                                  
MKTGRP   DS    0H                  ** MARKET GROUP FIRST **                     
         CP    CLTCNT,=P'1'        ONLY EXECUTE FOR FIRST CLIENT                
         BH    MGX                                                              
         MVC   HALF,SBBMGR                                                      
         OC    SBBMGR,SBMG1MSK                                                  
         OC    SBPGRPEX(2),SBPGRPEX   TEST PRDGRP EXCEPTION(S)                  
         BZ    MG6                                                              
         OC    SBAMGTB2,SBAMGTB2                                                
         BNZ   MG4                                                              
         LA    R0,SBEXMAX                                                       
         LA    R2,SBPGRPEX                                                      
         LA    R3,SBAMGTB2                                                      
*                                                                               
MG2      OC    0(2,R2),0(R2)                                                    
         BZ    MG4                                                              
         CLI   OFFLINE,C'Y'        PROTECT AGAINST ONLINE GETMAIN               
         BNE   MG4                                                              
         GOTO1 COVAIL,DMCB,C'GET',20000,20000                                   
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,0(R3)                                                         
         LA    R2,2(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,MG2                                                           
*                                                                               
MG4      CLC   SBBPGR,PGRSAVE         TEST PRODUCT GROUP BREAK                  
         BE    MG6                                                              
         MVC   PGRSAVE,SBBPGR         YES-CLEAR SAVED MARKET GROUPS             
         XC    MGR1SAVE,MGR1SAVE                                                
         XC    MGR2SAVE,MGR2SAVE                                                
*                                                                               
MG6      CLC   SBBMGR,MGR1SAVE                                                  
         BE    MG10                                                             
         MVC   MGR1SAVE,SBBMGR                                                  
         GOTO1 PUTMGRNM,SBMGR1NM                                                
*                                                                               
MG10     MVC   SBBMGR,HALF                                                      
         CLC   SBMGR1LN,SBMGR2LN                                                
         BE    MGX                                                              
         OC    SBBMGR,SBMG2MSK                                                  
         CLC   SBBMGR,MGR2SAVE                                                  
         BE    MG20                                                             
         MVC   MGR2SAVE,SBBMGR                                                  
         GOTO1 PUTMGRNM,SBMGR2NM                                                
*                                                                               
MG20     MVC   SBBMGR,HALF                                                      
         CLC   SBMGR2LN,SBMGR3LN                                                
         BE    MGX                                                              
         GOTO1 PUTMGRNM,SBMGR3NM                                                
*                                                                               
MGX      B     HKX                                                              
         EJECT                                                                  
MARKET   DS    0H                  ** MARKET FIRST **                           
         GOTO1 AMKTNM                                                           
         B     HKX                                                              
         EJECT                                                                  
STAGRP   DS    0H                  ** STATION GROUP FIRST **                    
         MVC   HALF,SBBSGR                                                      
         OC    SBBSGR,SBSG1MSK                                                  
         GOTO1 PUTSGRNM,SBSGR1NM                                                
         MVC   SBBSGR,HALF                                                      
         CLC   SBSGR1LN,SBSGR2LN                                                
         BE    SGX                                                              
         GOTO1 PUTSGRNM,SBSGR2NM                                                
SGX      B     HKX                                                              
         EJECT                                                                  
STATION  DS    0H                  ** STATION FIRST **                          
         TM    DATAIND2,DIAFFIL    TEST AFFILIATE,                              
         BO    STATION2                                                         
         TM    DATAIND3,DICHAN+DIREP  CHANNEL OR REP IS A ROW                   
         BNZ   STATION2                                                         
         CLI   SBQSTATY,C' '       OR, STATION TYPE IS A FILTER                 
         BH    STATION2                                                         
         CLI   SBQAFFIL,C' '       OR, AFFILIATE FILTER                         
         BH    STATION2                                                         
         CLI   SBQNSIZE,C' '       OR, NETSIZE FILTER                           
         BH    STATION2                                                         
         TM    DATAIND5,DIPREP+DITREP   OR, PAYING/TIME REP IS A ROW            
         BNZ   STATION2                                                         
         TM    DATAIND7,DIGSTB          OR, BILLED GST (FOR HST CHECK)          
         BNZ   STATION2                                                         
         TM    DTAIND10,DIPSTB          OR, BILLED PST                          
         BNZ   STATION2                                                         
         TM    SBEGST,SBEGSTB+SBEPSTB   (DITTO)                                 
         BNZ   STATION2                                                         
         TM    DATAIND8,DISTANM+DISIZE+DIFORMAT+DIFAX   OR OTHER                
         BNZ   STATION2                                                         
         TM    DATAIND9,DICABLE     OR CABLE GROUP/ORDER DEADLINE               
         BNZ   STATION2                                                         
         LA    R1,SBLOCK                                                        
         TM    SBEFLAG3-SBLOCK(R1),SBE3MS                                       
         BNZ   STATION2                                                         
         TM    SBEFLAG8-SBLOCK(R1),SBE8CP1+SBE8CP2 COMSCORE?                    
         BNZ   STATION2             YES - NEED FSTA TO GET PARENT+              
***                                                                             
* CANADIAN SOFT DEMOS = CANADA + DATAIND=DIBYDEM + SPOT PROF+15=Y               
***                                                                             
         LA    R1,SBAGYREC                                                      
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'      CANADIAN?                                    
         BNE   STATION4            NO                                           
         DROP  R1                                                               
         TM    DATAIND,DIBYDEM     BUY DEMO KEYWORD?                            
         BZ    STATION4            NO                                           
         CLI   SBSPPROF+15,C'Y'    SOFT DEMO LOOKUPS?                           
         BNE   STATION4            NO                                           
*                                                                               
STATION2 GOTO1 AFSTA               YES-CALL STATION FIRST ROUTINE               
*                                                                               
STATION4 TM    DATAIND8,DICBLNM    TEST CABLE SYSTEM NAMES                      
         BZ    STATION6                                                         
         GOTO1 ACBLNM              YES-SET MSO/INTERCONNECT NAMES               
*                                                                               
STATION6 DS    0H                                                               
*                                                                               
STATIONX B     HKX                                                              
         EJECT                                                                  
PROCBUY  DS    0H                  ** PROCESS BUY RECORD **                     
         GOTO1 APROCBY                                                          
         B     HKX                                                              
PROCGOAL DS    0H                  ** PROCESS GOAL RECORD **                    
         GOTO1 APROCGL                                                          
         B     HKX                                                              
PROCBILL DS    0H                  ** PROCESS STATION BILL RECORD **            
         GOTO1 =A(PROCBL)                                                       
         B     HKX                                                              
PROCINV  DS    0H                  ** PROCESS INVOICE RECORD **                 
         GOTO1 =A(PROCINVO)                                                     
         B     HKX                                                              
PROCBH   DS    0H                  ** PROCESS BILL HEADER **                    
         GOTO1 =A(PROCBHD)                                                      
         B     HKX                                                              
PROCSLK  DS    0H                  ** PROCESS STATION LOCKIN **                 
         GOTO1 APROCSL                                                          
         B     HKX                                                              
PROCCS   DS    0H                  ** PROCESS CLEARANCE STATUS **               
         GOTO1 ACLRST                                                           
         B     HKX                                                              
PROCIN   DS    0H                  ** PROCESS INFOMERCIAL **                    
         GOTO1 =A(PROCINFO)                                                     
         B     HKX                                                              
PROCWIPW DS    0H                  ** PROCESS WESTERN PW (STA) **               
         GOTO1 APROCPW                                                          
         B     HKX                                                              
PROCWIP2 DS    0H                  ** PROCESS WESTERN PW (MKT) **               
         GOTO1 APROCPW2                                                         
         B     HKX                                                              
PROCOM   DS    0H                  ** PROCESS OM RECORD **                      
         GOTO1 =A(PROCOMG)                                                      
         B     HKX                                                              
TSAROM   DS    0H                  ** TSAR OFF OM RECORD **                     
         GOTO1 =A(TSAROMG)                                                      
         B     HKX                                                              
DELOM    DS    0H                  ** DELETE OM REC FROM TSAR BUFF **           
         XC    LASTSYS,LASTSYS                                                  
         GOTO1 =A(TSAROMG)                                                      
         B     HKX                                                              
         EJECT                                                                  
* GENERAL DRIVER CALLING ROUTINE                                                
*                                                                               
DRIVGEN  LR    R0,RE                                                            
         GOTO1 DRIVER,DMCB,AGLOBAL                                              
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
*                                                                               
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BNE   *+12                                                             
         BRAS  RE,PRINT                                                         
         B     DRHOOKX                                                          
*                                                                               
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
*                                                                               
         CLI   GLHOOK,GLLAST       LASTS                                        
         BNE   *+12                                                             
         BRAS  RE,LASTS                                                         
         B     DRHOOKX                                                          
*                                                                               
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BNE   DRHOOKX                                                          
         GOTO1 AHEADHK                                                          
*                                                                               
DRHOOKX  GOTOR RPT,RPDRHOOK        TRY USER REPORT OVERLAY                      
         J     XIT                                                              
         EJECT                                                                  
* RESOLVE ROUTINE ADDRESSES                                                     
*                                                                               
RESOLVE  LA    R1,ROUTLIST         TEST ROUTINE IN THIS OVERLAY                 
RESOLVE2 CLI   0(R1),FF                                                         
         BE    RESOLVEX                                                         
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         J     XIT                                                              
*                                                                               
RESOLVEX B     DRHOOKX                                                          
         SPACE 2                                                                
ROUTLIST DS    0F                  ROUTINE ADDRESS LIST                         
         DC    CL8'IPGDATA ',AL4(IPGDATA)                                       
         DC    CL8'OPGDATA ',AL4(OPGDATA)                                       
         DC    CL8'HPGDATA ',AL4(HPGDATA)                                       
         DC    CL8'IPGEST  ',AL4(IPGEST)                                        
         DC    X'FF'                                                            
         EJECT                                                                  
* EXECUTING ROUTINES                                                            
*                                                                               
EXEC     LA    R1,ROUTLIST         TEST ROUTINE IS IN THIS OVERLAY              
EXEC2    CLI   0(R1),FF                                                         
         BE    EXECX                                                            
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     EXEC2                                                            
         L     R2,GLAIFLD          YES-R2=A(INPUT)                              
         L     R3,GLAOFLD              R3=A(OUTPUT)                             
         L     RF,GLAROUT              BRANCH TO ROUTINE                        
         BASR  RE,RF                                                            
         J     XIT                                                              
*                                                                               
EXECX    B     DRHOOKX             NO-ROUTINE IS IN USER REPORT OVERLAY         
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
         SPACE 1                                                                
         USING PGDATAD,R1                                                       
IPGEST   CLI   SBMODE,SBPROCES         PG ESTIMATE ROUTINES                     
         JNE   XIT                                                              
         L     R1,=A(PGDATA)                                                    
         MVC   0(L'PGESTIM,R2),PGESTIM                                          
         MVI   INDATA,1                                                         
         J     XIT                                                              
         DROP  R1                                                               
*                                                                               
IPGDATA  CLI   SBMODE,SBPROCES         PG ESTIMATE ROUTINES                     
         JNE   XIT                                                              
         L     R1,=A(PGDATA)                                                    
         MVC   0(PGDATAL,R2),0(R1)                                              
         MVI   INDATA,1                                                         
         J     XIT                                                              
*                                                                               
         USING PGDATAD,R2                                                       
OPGDATA  CLC   0(PGDATAL,R2),XFF                                                
         BNE   *+14                                                             
         MVC   0(17,R3),=C'*** NOT FOUND ***'                                   
         J     XIT                                                              
         MVC   4(3,R3),PGCHPER                                                  
         MVC   09(6,R3),PGACCNT                                                 
         MVC   17(4,R3),PGBRAND                                                 
         MVC   26(4,R3),PGESTIM                                                 
         MVC   35(6,R3),PGEVENT                                                 
         MVC   52(1,R3),PGMULTI                                                 
         MVC   60(1,R3),PGNOBRD                                                 
         MVC   74(3,R3),PGOREST                                                 
         MVC   83(2,R3),PGESUF                                                  
         CLC   PGSRSTRT,BLANKS                                                  
         JNH   XIT                                                              
         GOTO1 DATCON,DMCB,PGSRSTRT,(5,62(R3))                                  
         J     XIT                                                              
         DROP  R2                                                               
*                                                                               
HPGDATA  L     R1,=A(PGHEAD)                                                    
         MVC   0(L'PGHEAD,R3),0(R1)                                             
         J     XIT                                                              
         EJECT                                                                  
* FIRST TIME CONTROLS                                                           
*                                                                               
FIRSTS   CLI   GLARGS,0            TEST LEVEL 0 BREAK                           
         BNE   FS02                                                             
         MVI   FSTFIRST,C'Y'                                                    
         BAS   RE,CLRDPCNT         MAKE SURE DPT COUNTERS ARE CLEARED           
*                                                                               
         CLI   GLRECNO,1           FIRST REPORT?                                
         BE    FSX                  YES - TITLE IS SET                          
         TM    OPTIND5,OPT5CONT    IS THIS A CONTINUATION                       
         BNZ   FSX                  YES - GETS SAME TITLE                       
         MVC   TITLE,RPT2TIT       MUST BE 2ND REPORT                           
         L     R1,ASPOOLD          RESET PAGE NUMBER TO 1                       
         MVC   PAGE-SPOOLD(L'PAGE,R1),=H'1'                                     
         B     FSX                                                              
*                                                                               
FS02     LLC   R1,GLARGS           FIND ROW AT THIS LEVEL                       
         CLI   GLARGS,L'LEVELS                                                  
         BH    FSX                 CAN'T HAVE TOO MANY LEVELS...                
         LA    R1,LEVELS-1(R1)                                                  
         SR    RF,RF                                                            
         ICM   RF,1,0(R1)                                                       
         BZ    FSX                                                              
         STC   RF,BYTE             BYTE = LEVEL CODE                            
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     R1,=A(FIRSTAB)                                                   
         LA    RF,0(RF,R1)         RF = BRANCH ADDRESS                          
         CLI   0(RF),0                                                          
         BE    FSX                                                              
         XC    AFIELD,AFIELD       FIND ADDRESS OF FIELD                        
         ICM   R1,15,GLADTENT                                                   
         BZ    FS04                                                             
         L     R1,DROIADD-DROD(R1)                                              
         LTR   R1,R1                                                            
         BZ    FS04                                                             
         LH    R1,DRINDISP-DRIND(R1)                                            
         A     R1,GLATHREC                                                      
         ST    R1,AFIELD                                                        
*                                                                               
FS04     CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    FS10                                                             
         CLC   GLARGS(1),MKTLEV    YES - TEST LEVEL ABOVE MARKET                
         BNL   FS10                                                             
         CLI   DTTOTLEV,0          YES - TEST XFF ENCOUNTERED YET               
         BNE   FS06                                                             
         LLC   RE,0(RF)            NO - TEST FIELD FOR XFF                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),XFF                                                      
         BNE   FS06                                                             
         MVC   DTTOTLEV,GLARGS     YES - SET THE LEVEL                          
*                                                                               
FS06     CLI   FSTFIRST,C'Y'       FIRST FIRST ??                               
         BNE   FS10                                                             
         MVI   FSTFIRST,C'N'       YES -                                        
         SR    R0,R0               TURN OFF MARKET ENCOUNTERED SWITCHES         
         ICM   R0,3,=X'FFFF'       AT THIS AND LOWER LEVELS                     
         SR    R1,R1                                                            
         LLC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BNP   *+12                                                             
         SRDA  R0,1                                                             
         BCT   RE,*-4                                                           
         STCM  R1,12,HALF                                                       
         L     R3,AMKTLST                                                       
         USING MKTLSTD,R3                                                       
         LA    R0,MLNMKTS                                                       
*                                                                               
FS08     OC    MLMKT,MLMKT                                                      
         BZ    FS10                                                             
         NC    MLLEVS,HALF                                                      
         LA    R3,MKTLSTL(R3)                                                   
         BCT   R0,FS08                                                          
         DROP  R3                                                               
*                                                                               
FS10     CLC   GLARGS(1),INDPTLEV  TEST LEVEL ONE ABOVE DPT                     
         BNE   *+8                                                              
         BAS   RE,CLRDPCNT         YES-CLEAR DAYPART COUNTERS                   
*                                                                               
         L     RF,0(RF)            EXECUTE ROW ROUTINE (IF ANY)                 
         BR    RF                                                               
*                                                                               
* FOR PQIX=Y, FOR ANY HEAD/MID/ROW THAT HAS CHANGED, PRINT INDEX                
* DATA FOR IT AND ALL FOLLOWING HEADS/MIDS/ROWS                                 
*                                                                               
FSX      DS    0H                                                               
         ICM   RF,15,APQINDEX      GET PQINDEX TABLE                            
         BZ    FSXX                                                             
         USING PQINDEX,RF                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,1,GLARGS                                                      
         BZ    FSXX                SKIP LEVEL 0 BREAKS                          
         AHI   R1,-1                                                            
         MHI   R1,PQINDXEQ                                                      
         AR    RF,R1                                                            
*                                                                               
         OI    PQSTATUS,PQCHG      SET CHG FOR THIS ROW                         
         L     RE,GLADTENT         AND SAVE IT'S OUTPUT ADDRESS                 
         MVC   PQAOUT,DROAPOS-DROD(RE)                                          
         DROP  RF                                                               
*                                                                               
FSXX     B     DRHOOKX                                                          
         SPACE 2                                                                
CLRDPCNT XC    DPTGRPSV,DPTGRPSV   CLEAR DPT GROUP SAVE                         
         MVI   LENSV,0             CLEAR SAVED SPOT LENGTH                      
         XC    SLCNTRS,SLCNTRS     CLEAR SPOT LENGTH COUNTERS                   
         ZAP   DLCNTR,=P'0'        INIT DPTLEN DETAIL COUNTER                   
         ZAP   DPTCNTR,=P'0'       INIT DAYPART COUNTER                         
         ZAP   LENCNTR,=P'0'       INIT LENGTH COUNTER                          
         BR    RE                                                               
         EJECT                                                                  
* FIRST ROUTINE FOR MEDIA                                                       
*                                                                               
FMED     XC    PRMYDEMO,PRMYDEMO   CLEAR PRIMARY DEMO                           
         XC    DEMNAMES,DEMNAMES   CLEAR DEMO NAMES                             
         MVC   SBBCLT,SBQBCLT      SET CLIENT                                   
         GOTO1 ADEMOS              GET DEMOS IF NEEDED                          
         ICM   R2,15,AFIELD                                                     
         BZ    FSX                                                              
         MVC   SBMED,0(R2)                                                      
*                                                                               
         CLI   SBQMED,C'*'                                                      
         BNE   FSX                                                              
         NI    SBBAGYMD,X'F0'                                                   
         CLI   SBMED,C'T'                                                       
         BNE   *+12                                                             
         OI    SBBAGYMD,X'01'                                                   
         B     FSX                                                              
         CLI   SBMED,C'R'                                                       
         BNE   *+12                                                             
         OI    SBBAGYMD,X'02'                                                   
         B     FSX                                                              
         CLI   SBMED,C'C'                                                       
         BE    *+12                                                             
         CLI   SBMED,C'N'                                                       
         BNE   *+12                                                             
         OI    SBBAGYMD,X'03'                                                   
         B     FSX                                                              
         CLI   SBMED,C'X'                                                       
         BNE   FSX                                                              
         OI    SBBAGYMD,X'04'                                                   
         B     FSX                                                              
         EJECT                                                                  
* FIRST ROUTINE FOR CLIENT                                                      
*                                                                               
FCLT     DS    0H                                                               
         GOTO1 AFCLT                                                            
         B     FSX                                                              
         EJECT                                                                  
* FIRST ROUTINES FOR PRODUCT GROUPS                                             
*                                                                               
FPGR1    CLC   SBPGR1LN,SBPGR2LN      TEST ONLY ONE LEVEL PRD GRPS              
         BNE   FSX                    NO                                        
*                                                                               
FPGR2    CLI   DEMGRP,C'P'         TEST DEMOS GROUPED BY PRDGRP                 
         BNE   FSX                                                              
         ICM   R2,15,AFIELD        YES-R2=A(PRDGRP)                             
         BZ    FSX                                                              
         CLI   SBQMKTWT,C'D'       TEST WHETHER DEMOS NEEDED                    
         BNE   *+14                                                             
         OC    PRMYDEMO,PRMYDEMO                                                
         BZ    FPGR22                                                           
         TM    DATAIND4,DIDEMHED                                                
         BZ    FSX                                                              
*                                                                               
FPGR22   CLC   SVPGRP,0(R2)        YES-TEST GROUP CHANGED                       
         BE    FSX                                                              
         MVC   SVPGRP,0(R2)        YES                                          
         CLC   SVPGRP,=X'9999'     TEST PRDGRP UNKNOWN                          
         BNE   *+18                                                             
         MVI   DEMNAMES,C'X'       YES-MAKE ALL DEMO NAMES = XXXXXXX            
         MVC   DEMNAMES+1(L'DEMNAMES-1),DEMNAMES                                
         B     FSX                                                              
         XC    KEY,KEY                                                          
         LA    R2,KEY              READ FIRST PRODUCT GROUP PASSIVE             
         USING PRGKEY,R2                                                        
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,SBBAGYMD                                                
         MVC   PRGPCLT,SBBCLT                                                   
         CLI   SBQCLT,C'='         ** THIS CODE FOR SPECIAL                     
         BNE   FPGR25              ** CLIENT GROUPS                             
         LA    R1,CLTGRPTB                                                      
*                                                                               
FPGR24   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   AGENCY,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     FPGR24                                                           
         MVC   PRGPCLT,2(R1)                                                    
         MVC   SVCLT,2(R1)                                                      
         B     FPGR25                                                           
*                                                                               
CLTGRPTB DC    CL2'BS',XL2'B167'   BSNY/MILLER                                  
         DC    CL2'OM',XL2'98BF'   OMNY/GF                                      
         DC    X'00'                                                            
*                                                                               
FPGR25   MVC   PRGPID,SBQPGRD                                                   
         MVC   PRGPGRP,SVPGRP                                                   
         GOTO1 HIGH                                                             
*                                                                               
FPGR26   CLC   KEY(PRGPPRD-PRGKEY),KEYSAVE                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FULL(3),PRGPPRD                                                  
         MVC   SVKEY,KEY           SAVE PRDGRP PASSIVE KEY                      
         DROP  R2                                                               
         XC    KEY,KEY             READ ESTIMATE HEADER                         
         USING ESTHDRD,R2                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         CLI   SBQCLT,C'='         ** SPECIAL FOR BSNY/MILLER                   
         BNE   *+10                **         AND OMNY/GF                       
         MVC   EKEYCLT,SVCLT       **                                           
         MVC   EKEYPRD,FULL                                                     
         MVC   EKEYEST,SBQEST      ESTIMATE START                               
         CLI   SBQSEPES,C'Y'                                                    
         BE    *+10                                                             
         MVC   EKEYEST,ESTSTART                                                 
*                                                                               
FPGR27   GOTO1 HIGH                                                             
         CLC   EKEY(EKEYEST-EKEY),KEYSAVE                                       
         BNE   FPGR28                                                           
         OC    EKEY+8(5),EKEY+8    TEST ITS AN ESTIMATE RECORD                  
         BZ    *+14                                                             
         MVC   EKEY+8(5),XFF                                                    
         B     FPGR27                                                           
         CLC   EKEYEST,SBQESTND                                                 
         BNH   FPGR29                                                           
*                                                                               
FPGR28   MVC   KEY,SVKEY           NO ESTIMATES FOR THIS PRODUCT                
         GOTO1 HIGH                SO, READ NEXT PRDGRP PASSIVE                 
         GOTO1 SEQ                                                              
         B     FPGR26                                                           
*                                                                               
FPGR29   L     R2,AIO1                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R3,EDEMLST                                                       
         LA    R4,EUSRNMS                                                       
         XC    DEMNAMES,DEMNAMES                                                
         GOTO1 AGETDEMS            GET THE DEMOS                                
         B     FSX                                                              
         DROP  R2                                                               
         EJECT                                                                  
* FIRST ROUTINE FOR PRODUCT                                                     
*                                                                               
FPRD     MVC   SBPRD,=C'POL'                                                    
         CLC   SBQPRD,=C'POL'      TEST POOL REQUEST                            
         BE    FP10                                                             
         XC    PRMYDEMO,PRMYDEMO   NO - CLEAR PRIMARY DEMO                      
         CLI   DEMGRP,C'P'         TEST DEMOS GROUPED BY PRDGRP                 
         BE    FP02                                                             
         OC    SBPDEMOS,SBPDEMOS   NO-TEST DEMO MENU OR DEMO OPTION SET         
         BNZ   FP02                                                             
         XC    DEMNAMES,DEMNAMES   NO-CLEAR DEMO NAMES                          
*                                                                               
FP02     ICM   R2,15,AFIELD                                                     
         BZ    FPX                                                              
         MVC   SBPRD,0(R2)                                                      
         CLI   BYTE,QPRDNM                                                      
         BNE   *+10                                                             
         MVC   SBPRD,8(R2)                                                      
         CLC   SBPRD,=X'FDFDFD'    TEST POL                                     
         BNE   *+10                                                             
         MVC   SBPRD,=C'POL'                                                    
         CLC   0(3,R2),XFF                                                      
         BNE   FP10                                                             
         CLC   SBQPRD,=C'ALL'                                                   
         BE    FPX                                                              
         MVC   SBPRD,SBQPRD                                                     
*                                                                               
FP10     L     R1,SBAPRDBF                                                      
         USING PRDBUFFD,R1                                                      
         LH    RE,=Y(PRDBUFFL)                                                  
         LA    R0,256                                                           
         CLC   PBALPH,SBPRD                                                     
         BE    *+16                                                             
         LA    R1,0(R1,RE)                                                      
         BCT   R0,*-14                                                          
         B     FP20                                                             
         MVC   SBBPRD,PBBCODE                                                   
*                                                                               
FP20     TM    DATAIND2,DIEST      TEST ESTIMATE IS A ROW                       
         BZ    FP24                NO-GET DEMO INFO NOW IF NEEDED               
         LA    R0,L'LEVELS         YES-TEST PRODUCT ABOVE ESTIMATE              
         LA    R1,LEVELS                                                        
         MVI   BYTE,0                                                           
*                                                                               
FP22     CLI   0(R1),QEST                                                       
         BE    *+12                                                             
         CLI   0(R1),QESTNM                                                     
         BNE   FP23                                                             
         CLI   BYTE,0                                                           
         BE    FP24                NO-GET DEMOS NOW                             
         CHI   R0,1                YES-GET DEMOS ON ESTIMATE FIRST              
         BE    FP24                    IF ESTIMATE NOT LOWEST LEVEL             
         CLI   1(R1),0                                                          
         BE    FP24                                                             
         B     FPX                                                              
*                                                                               
FP23     CLI   0(R1),QPRD                                                       
         BE    *+12                                                             
         CLI   0(R1),QPRDNM                                                     
         BNE   *+8                                                              
         MVI   BYTE,1                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,FP22                                                          
*                                                                               
FP24     GOTO1 ADEMOS              GET DEMO INFO IF REQUIRED                    
*                                                                               
FPX      B     FSX                                                              
         DROP  R1                                                               
         EJECT                                                                  
* FIRST ROUTINE FOR ESTIMATE                                                    
*                                                                               
FEST     DS    0H                                                               
         GOTO1 AFEST                                                            
         B     FSX                                                              
         EJECT                                                                  
* FIRST ROUTINES FOR TARGET AND TARGET2                                         
*                                                                               
FTGT     SR    R1,R1                                                            
         B     FTGTX                                                            
*                                                                               
FTGT2    LA    R1,7                                                             
         B     FTGTX                                                            
*                                                                               
FTGT3    LA    R1,14               TARGET3                                      
         B     FTGTX                                                            
*                                                                               
FTGT4    LA    R1,21               TARGET4                                      
*                                                                               
FTGTX    OC    SBPDEMOS,SBPDEMOS   UNLESS THERE IS A DEMO MENU OR THE           
         BNZ   FSX                 DEMO OPTION IS SET,                          
         LA    R1,DEMNAMES(R1)     RIP OFF THE DEMO NAME FROM THE               
*        L     RE,AFIELD           RECORD FOR DEMO HEADINGS                     
         ICM   RE,15,AFIELD        RECORD FOR DEMO HEADINGS                     
         BZ    FSX                                                              
         MVC   0(7,R1),0(RE)                                                    
         B     FSX                                                              
         EJECT                                                                  
* FIRST ROUTINE FOR MARKET                                                      
*                                                                               
FMKT     NI    OUTIND,255-OUTISUPM  DON'T SUPPRESS THE MARKET                   
         ICM   R2,15,AFIELD                                                     
         BZ    FMX                                                              
         CLC   0(2,R2),XFF         TEST ALL MARKETS                             
         BNE   FM10                                                             
         OI    OUTIND,OUTICRMK     YES - INDICATE ACROSS MARKETS                
         SR    RE,RE                                                            
         ICM   RE,1,DTTOTLEV       FIND FIRST 'ALL' LEVEL                       
         BNZ   *+12                                                             
         ICM   RE,1,MKTLEV                                                      
         BZ    FMX                                                              
         BCTR  RE,0                LEVEL-1 = DETAIL TOTAL LEVEL                 
         STC   RE,DTTOTLEV                                                      
         GOTO1 AGETOTWT,DTTOTLEV   GET TOTAL WEIGHT AND MARKETS                 
         BRAS  RE,MKTMID                                                        
         B     FMX                                                              
*                                                                               
FM10     NI    OUTIND,FF-OUTICRMK  NOT ACROSS MARKETS                           
         MVI   DTTOTLEV,0                                                       
         CLI   BYTE,QMKTR          TEST MARKET RANKED                           
         BE    *+12                                                             
         CLI   BYTE,QMKTRNM        OR MARKET NAME RANKED                        
         BNE   *+8                                                              
         LA    R2,2(R2)            YES-SKIP PAST THE RANK NUMBER                
         MVC   SBBMKT,0(R2)                                                     
         CLI   BYTE,QMKTNM                                                      
         BE    *+12                                                             
         CLI   BYTE,QMKTRNM                                                     
         BNE   *+14                                                             
         LA    R2,8(R2)                                                         
         MVC   SBBMKT,0(R2)                                                     
         CLC   SBBMKT,=X'FFFE'     TEST NETWORKS                                
         BNE   FM20                                                             
         MVC   SBMKT,=C'0000'      YES                                          
         MVC   SBMKTNM,BLANKS                                                   
         MVC   SBMKTNM(8),=C'NETWORKS'                                          
         B     FM24                                                             
*                                                                               
FM20     EDIT  SBBMKT,(4,SBMKT),FILL=0                                          
         GOTO1 GETMKTNM                                                         
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    FM24                                                             
         CLI   SBQMKTWT,C'D'       TEST PRIMARY DEMO WEIGHTING                  
         BNE   FM22                                                             
         OC    PRMYDEMO,PRMYDEMO   AND PRIMARY DEMO NOT SET                     
         BNZ   FM22                                                             
         L     R1,ADEMLST          YES-SET IT NOW                               
         MVC   PRMYDEMO,0(R1)                                                   
         MVI   PRMYDEMO+1,C'U'                                                  
*                                                                               
FM22     DS    0H                                                               
         GOTO1 GETMKTWT            YES - GET THE MKT WGT                        
*                                                                               
FM24     CLI   SBQSPILL,C'S'       TEST SEPERATE SPILL REQUEST                  
         BNE   FMX                                                              
         CLC   SBBMKT,SVMKT        YES-TEST MARKET CHANGE                       
         BE    *+14                                                             
         MVC   SVMKT,SBBMKT        YES-                                         
         MVI   SVMKTFLG,0          CLEAR MARKET FLAG                            
         CLI   2(R2),C'O'          TEST ORIGINATING                             
         BNE   *+12                                                             
         OI    SVMKTFLG,SVORIG     YES                                          
         B     FMX                                                              
         CLI   2(R2),C'S'          TEST SPILL                                   
         BNE   *+12                                                             
         OI    SVMKTFLG,SVSPILL    YES                                          
         B     FMX                                                              
         TM    SVMKTFLG,SVORIG+SVSPILL   MKT TOTAL - TEST ONLY ONE OF           
         BNM   FMX                       ORIG OR SPILL PREVIOUSLY               
         OI    OUTIND,OUTISUPM     YES-SUPPRESS THIS MARKET TOTAL               
*                                                                               
FMX      B     FSX                                                              
         DROP  RA,RB                                                            
         EJECT                                                                  
******************************************************                          
******************************************************                          
*                                                    *                          
* CODE THAT FOLLOWS IS BASELESS!                     *                          
*                                                    *                          
******************************************************                          
******************************************************                          
         SPACE 2                                                                
* ROUTINE TO CALL USER REPORT OVERLAY                                           
*                                                                               
RPT      LR    R0,RE                                                            
         STC   R1,RPMODE                                                        
         ICM   RF,15,ARPTNTRY                                                   
         JZ    RPTX                                                             
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   RPMODE,RPSTOP       TEST STOP RETURNED                           
         JNE   RPTX                                                             
         LTR   RE,R0               YES-CC NE                                    
         BR    RE                                                               
RPTX     LR    RE,R0               NORMAL RETURN - CC EQ                        
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
XITNE    LTR   RB,RB                                                            
         J     XIT                                                              
*                                                                               
XITLO    SR    R1,R1                                                            
         CR    R1,RB                                                            
         J     XIT                                                              
*                                                                               
XITHI    SR    R1,R1                                                            
         CR    RB,R1                                                            
         J     XIT                                                              
*                                                                               
XITEQ    CR    RB,RB                                                            
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
MYCURSER MVI   ERROR,X'FE'         OWN ERROR MESSAGE                            
         GOTO1 CURSERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL R3,24,ELCODE                                                     
         EJECT                                                                  
* PRINT A LINE                                                                  
*                                                                               
PRINT    NTR1  BASE=*,LABEL=*                                                   
         TM    OPTIND5,OPTRCAP                                                  
         BZ    PRINT3                                                           
         L     R1,GLATHID                                                       
         USING GLINTD,R1                                                        
         CLC   GLLEVEL,GLDETLEV    TEST TOTALING                                
         BH    PRINT3A              NO                                          
*                                                                               
         LA    RE,GLTOTLEV                                                      
         LHI   R0,GLNUMLEV                                                      
*                                                                               
         CLI   0(RE),X'10'         FIND IF ANY DETAILED TOTALS                  
         BNL   *+16                                                             
         AHI   RE,1                                                             
         BCT   R0,*-8                                                           
         B     PRINT3A             NONE - DON'T PRINT ANYTHING                  
*                                                                               
         LA    R3,GLLCBS+GLNUMLEV  GET L' DETAIL & PRIOR CONTROL BREAK          
         SR    R3,R0                                                            
*                                                                               
         IC    RE,0(RE)            GET THE LEVEL OF DETAIL                      
         SLL   RE,28               STRIP THE HIGH NIBBLE                        
         SRL   RE,28                                                            
*                                                                               
         LLC   RF,GLDETLEV         TEST IF LEVS BELOW THIS 1 ARE X'FF'S         
         SR    RF,RE                                                            
         LA    RF,GLLCBS-1(RF)                                                  
         IC    RE,0(RF)            L' THIS  & PRIOR C/B'S                       
         LLC   RF,0(R3)            L' DETAIL LEV & PRIOR C/B'S                  
         SR    RE,RF               DIFFERENCE                                   
         BCTR  RE,0                                                             
         L     R3,GLATHREC                                                      
         LA    RF,2(RF,R3)                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),XFF                                                      
         BNE   PRINT3A                                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
PRINT3   TM    COLIND2,COLIBLBL+COLIBLB0  TEST ALL COLUMNS ARE BILLABLE         
         BNO   *+8                        AND THEY'RE ALL ZERO                  
         MVI   PRTSW,C'N'          YES-DON'T PRINT                              
         TM    GLDWNLD2,GLDLHDLS   DOWNLOADING HEADLINES?                       
         BNZ   *+8                 YES, WE ALWAYS WANT THEM                     
         OI    COLIND2,COLIBLB0                                                 
         TM    OUTIND,OUTISUPM     TEST SUPPRESS MARKET DETAIL                  
         BZ    *+8                                                              
         MVI   PRTSW,C'N'          YES                                          
*                                                                               
         CLI   PRTSW,C'N'          TEST LINE WILL PRINT                         
         BNE   PRINT4                                                           
PRINT3A  TM    GLDWNLD2,GLDLHDLS   DOWNLOADING HEADLINES?                       
         BNZ   *+8                 YES, WE ALWAYS WANT THEM                     
         MVI   GLHOOK,GLDONT       NO-TELL DRIVER TO SUPPRESS                   
         MVI   PRTSW,C'Y'                                                       
         B     PRINTX2                                                          
*                                                                               
PRINT4   TM    REQIND,REQITRAN     YES-TEST RECORD=TRANSMIT                     
         BZ    PRINT10                                                          
         CLI   FIRSTLIN,C'Y'       YES-TEST FIRST LINE                          
         BNE   PRINTX                                                           
         GOTO1 AGENEDIC            GENERATE EDICT CONTROL CARDS                 
         B     PRINTX                                                           
*                                                                               
PRINT10  DS    0H                                                               
         CLI   FIRSTLIN,C'Y'       PRINT INDEX HEADERS FOR FIRSTLIN             
         BNE   PRINT19                                                          
*                                                                               
* IF PQIX=Y, PRINT INDEX HEADER                                                 
         ICM   R3,15,APQINDEX                                                   
         BZ    PRINTX              NO                                           
         USING PQINDEX,R3                                                       
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R0,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS                              
*                                                                               
         MVC   P(06),=C'<DECL>'    START OF HEADING DECLARATIONS                
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES                                                         
*                                                                               
         LA    R2,P                                                             
         MVC   0(09,R2),=C'<REQNAME '                                           
         AHI   R2,9                                                             
         LLC   RE,WRINAMH+5                                                     
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WRINAM                                                   
         LA    R2,1(RE,R2)                                                      
         MVI   0(R2),C'>'                                                       
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IH '                                                
         LA    R2,4(R2)                                                         
*                                                                               
PRINT11  OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    PRINT12              NO                                          
         CLI   PQPOSO,C'H'         IS THIS A HEADLINE?                          
         BNE   PRINT12              NO - CLOSE & LOOK FOR MIDS & ROWS           
         BAS   RE,IXFMT                                                         
         AHI   R3,PQINDXEQ                                                      
         BCT   R0,PRINT11                                                       
*                                                                               
PRINT12  DS    0H                                                               
         MVI   0(R2),C'>'                                                       
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IM '                                                
         LA    R2,4(R2)                                                         
*                                                                               
PRINT13  CLI   PQPOSO,C'C'         COLUMN DATA?                                 
         BE    PRINT14              YES - CLOSE MIDS & GET COLS                 
         OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    PRINT14              NO                                          
         BAS   RE,IXFMT                                                         
         AHI   R3,PQINDXEQ                                                      
         BCT   R0,PRINT13                                                       
*                                                                               
PRINT14  DS    0H                                                               
         MVI   0(R2),C'>'                                                       
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IC '                                                
         LA    R2,4(R2)                                                         
*                                                                               
PRINT15  OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    PRINT16              NO                                          
         BAS   RE,IXFMT                                                         
         AHI   R3,PQINDXEQ                                                      
         BCT   R0,PRINT15                                                       
*                                                                               
PRINT16  MVI   0(R2),C'>'                                                       
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         CLI   DOWNOPT,0           DOWNLOAD?                                    
         BE    PRINT17              NO                                          
         MVC   P(10),=C'<FMT DATA>'                                             
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
         B     PRINT18                                                          
*                                                                               
PRINT17  MVC   P(04),=C'<HL '      NUMBER OF HEADLINES                          
         EDIT  LASTHEAD,(2,P+4),FILL=0                                          
         MVI   P+6,C'>'                                                         
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
PRINT18  MVC   P(07),=C'</DECL>'   END OF HEADING DECLARATIONS                  
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         DROP  R3,R5                                                            
*                                                                               
PRINT19  CLI   DOWNOPT,0           IF WE ARE DOWNLOADING, NO DATA LINES         
         BNE   PRINTX                                                           
         ICM   R3,15,APQINDEX                                                   
         BZ    PRINTX                                                           
         USING PQINDEX,R3                                                       
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R2,P                                                             
*                                                                               
         MVI   HALF,0                                                           
         LA    R1,1                                                             
         LA    R4,MAXHEADS         INDEX LINES ONLY FOR HEAD CHANGES            
         LA    R2,P                                                             
*                                                                               
PRINT20  CLI   PQPOSO,C'H'         INDEX LINES ONLY FOR HEAD CHANGES            
         BNE   PRINT30                                                          
         TM    PQSTATUS,PQCHG      HAS THIS KEY CHANGED?                        
         BNZ   PRINT40              YES - PRINT IT                              
*                                                                               
PRINT30  AHI   R1,1                                                             
         AHI   R3,PQINDXEQ                                                      
         BCT   R4,PRINT20                                                       
         B     PRINT60                                                          
*                                                                               
PRINT40  NI    PQSTATUS,X'FF'-PQCHG       RESET CHANGE FLAG                     
         CLI   HALF,0              ANY ENTRIES YET?                             
         BNE   PRINT50              YES                                         
         MVI   HALF,1              SET HAVE ONE NOW                             
         MVC   0(06,R2),=C'<DATA '                                              
         AHI   R2,6                                                             
         EDIT  (R1),(2,0(R2)),FILL=0                                            
         MVI   2(R2),C'='                                                       
         AHI   R2,3                                                             
*                                                                               
PRINT50  L     RE,PQAOUT           GET A(OUTPUT SORT AREA)                      
         AHI   RE,L'LABLAREA+1     RE=CODE PORTION                              
         LA    RF,L'CODEAREA                                                    
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)       MOVE DATA TO PRINT LINE                      
*                                                                               
         LA    R2,1(RF,R2)         GET LAST USED PRINT POSN                     
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),X'5E'         SEMI-COLON                                   
         AHI   R2,2                                                             
         B     PRINT30                                                          
*                                                                               
         DROP  R3                                                               
*                                                                               
PRINT60  CLI   HALF,1                                                           
         BNE   PRINTX                                                           
         MVI   0(R2),C'>'                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES                                                         
*                                                                               
PRINTX   MVI   FIRSTLIN,C'N'                                                    
*                                                                               
PRINTX2  J     XIT                                                              
         SPACE 2                                                                
DOWNCNT  DC    PL2'0'                                                           
FIRSTLIN DC    C'Y'                                                             
         EJECT                                                                  
*                                                                               
* FORMAT INDEX DATA                                                             
*   INPUT    R2 = A(FORMAT AREA)                                                
*            R3 = A(PQINDEX ENTRY)                                              
*   RETURN   FORMATTED DATA, FOLLOWED BY A SEMI-COLON                           
*            R2 = A(NEXT BLANK SPACE)                                           
*                                                                               
IXFMT    NTR1                                                                   
         USING PQINDEX,R3                                                       
         MVC   0(8,R2),PQKEYWRD                                                 
         AHI   R2,8                                                             
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         CLC   PQHEAD1(96),BLANKS                                               
         BNH   IX40                                                             
*                                                                               
         CLC   =C'<IC ',P                                                       
         BE    IX40                                                             
*                                                                               
         LA    R1,PQHEAD1                                                       
         LA    RE,96                                                            
         MVC   1(2,R2),=C'="'                                                   
         AHI   R2,3                                                             
         LA    R0,4                MAX HEADERS                                  
*                                                                               
IX10     MVC   0(24,R2),0(R1)      MOVE OUT HEADER                              
         AHI   R2,24               END OF HEADER                                
         CLI   0(R2),C' '          BACK UP TO LAST CHAR                         
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C'"'          CLOSE QUOTE                                  
         AHI   R2,1                                                             
         BCT   R0,*+8                                                           
         B     IX40                                                             
*                                                                               
         AHI   R1,24               NEXT HEADER                                  
         AHI   RE,-24              L'REMAINING HEADERS                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),BLANKS      ANY LEFT?                                    
         BNH   IX40                                                             
         MVC   1(2,R2),=C',"'                                                   
         AHI   R2,3                                                             
         B     IX10                                                             
*                                                                               
IX40     MVI   1(R2),X'5E'         SEMICOLON                                    
         AHI   R2,2                                                             
IXX      XIT1  REGS=(R2)                                                        
         DROP  R3,R5                                                            
         EJECT                                                                  
* LAST TIME CONTROLS                                                            
*                                                                               
LASTS    NTR1  BASE=*,LABEL=*                                                   
         XC    COUNT,COUNT         TEST COUNT REQUIRED                          
         CLI   COUNTLEV,0                                                       
         BE    LS4                                                              
         CLC   COUNTLEV,GLARGS     YES-TEST LEVEL LOWER THAN COUNTED            
         BNH   LS4                     ROW                                      
*                                                                               
         LA    R5,1                                                             
         SLL   R5,31                                                            
         LLC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BM    LS4                                                              
         BZ    *+12                                                             
         SRL   R5,1                                                             
         BCT   RE,*-4                                                           
         BCTR  RE,0                                                             
         XR    RE,R5                                                            
         STCM  RE,12,HALF                                                       
         SR    R2,R2                COUNTER                                     
         SR    RE,RE                SO WE CAN IC INSTEAD OF ZIC IN LOOP         
*                                                                               
         XC    MYREC(MYRECL),MYREC  CLEAR KEY+DATA                              
         MVI   MYKEY,NPCNTCDQ       COUNT RECORD                                
*                                                                               
         BAS   RE,TSARHIGH          ON EOF, MYKEY DOES NOT CHANGE               
         BNE   LS3                                                              
         CLI   MYKEY,NPCNTCDQ       HAVE A COUNT RECORD                         
         BNE   LS3                  NO, DONE                                    
*                                                                               
LS1      CLI   MYKEY,NPCNTCDQ       HAVE A COUNT RECORD                         
         BNE   LS3                                                              
         MVC   WORK,MYKEY                                                       
*                                                                               
         ICM   RE,12,MYDATA         TOTAL LEVELS                                
         NR    RE,R5                AND THEM                                    
         BZ    LS2                  NEXT RECORD IF ZEROS                        
         LA    R2,1(R2)             INCRIMENT R2                                
         NC    MYDATA,HALF          CHANGE THE RECORD                           
         MVI   THISRECL,0                                                       
         MVI   THISRECL+1,2         DON'T SCREW UP LENGTH                       
         BAS   RE,TSARWRT           AND WRITE IT BACK                           
*                                                                               
LS2      MVC   MYKEY,WORK           LAST KEY READ                               
         BAS   RE,TSARHIGH                                                      
         BAS   RE,TSARSEQ           FOUND A RECORD?                             
         BE    LS1                  YES                                         
*                                                                               
LS3      STCM  R2,3,COUNT                                                       
*                                                                               
LS4      CLC   GLARGS(1),MKTLEV    TEST LEVEL ABOVE MARKET                      
         BNL   LS10                                                             
         OI    OUTIND,OUTICRMK     INDICATE ACROSS MARKETS                      
         MVI   FSTFIRST,C'Y'                                                    
         L     R1,GLADTENT                                                      
         CLI   DROTLFOL-DROD(R1),0  TEST FOR TOTAL                              
         BE    LS10                                                             
         GOTO1 AGETOTWT,GLARGS                                                  
         BRAS  RE,MKTMID                                                        
*                                                                               
LS10     NI    OUTIND,255-OUTIMRTO                                              
         CLC   MKRKLEV,GLARGS      TEST MARKET RANK LAST                        
         BNE   *+8                                                              
         OI    OUTIND,OUTIMRTO     YES-MARKET RANK TOTAL NEXT                   
*                                                                               
LSX      J     XIT                                                              
         EJECT                                                                  
*                                                                               
TSARWRT  NTR1                                                                   
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSAWRT     SET TO WRITE BACK A RECORD                   
         XR    R1,R1                                                            
         ICM   R1,3,THISRECL                                                    
         AHI   R1,TSARKEYL+2                                                    
         STCM  R1,3,THISRECL                                                    
         MVC   MYREC-2(2),THISRECL 2-BYTE VARIABLE LENGTH                       
         LA    R3,MYREC-2          A(REC) MUST BEGIN AT LENGTH                  
         ST    R3,TSAREC           A(VARIABLE LENGTH RECORD)                    
         GOTO1 ATSAROF,(R2)        WRITE BACK THE RECORD                        
         CLI   TSERRS,0            ANY ERRORS?                                  
         JE    XIT                 NO                                           
         DC    H'0'                                                             
         EJECT                                                                  
         DROP  R2                                                               
*                                                                               
TSARHIGH NTR1                                                                   
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         LA    R0,MYREC-2          A(WHERE TO PUT RECORD)                       
         ST    R0,TSAREC                                                        
         GOTO1 ATSAROF,(R2)                                                     
         TM    TSERRS,TSEEOF       DID WE FIND THE KEY?                         
         BNO   *+6                 YES, SET CC EQU                              
         LTR   RE,RE                                                            
         J     XIT                                                              
         EJECT                                                                  
         DROP  R2                                                               
*                                                                               
TSARSEQ  NTR1                                                                   
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSANXT     SET GET NEXT RECORD                          
         LA    R0,MYREC-2          A(WHERE TO PUT RECORD)                       
         ST    R0,TSAREC                                                        
         GOTO1 ATSAROF,(R2)                                                     
         TM    TSERRS,TSEEOF       DID WE FIND THE KEY?                         
         BNO   *+6                 YES, SET CC EQU                              
         LTR   RE,RE                                                            
         J     XIT                                                              
         EJECT                                                                  
         DROP  R2                                                               
* DISPLAY RECORD                                                                
*                                                                               
DISP     NTR1  BASE=*,LABEL=*                                                   
         CLI   DDS,C'Y'            DDS?                                         
         BE    DISP10               YES - SKIP SECURITY CHECK                   
         TM    AUTH,X'10'          OVERRIDE SECURITY?                           
         BNZ   DISP10               YES                                         
*                                                                               
         XC    WORK,WORK           READ AGENCY'S WR PROFILE                     
         MVC   WORK(4),=C'S0WR'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         CLI   WORK+16,C'Y'        TEST FILTER REQUIRED                         
         BE    *+12                 YES                                         
DISP10   NI    WRIRFLH+1,X'FF'-X'0C' MAKE SURE DISPLAY ON (FIRST SEL)           
         B     DISPX                                                            
*                                                                               
         LA    R2,WRIRFLH          YES-CHECK THAT IT'S THERE                    
         LA    R0,L'WRIRFL                                                      
         LA    R1,WRIRFL                                                        
         CLI   0(R1),C' '                                                       
         BNH   DISP30                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BE    DISPX               ALREADY SECURE IF HERE FROM LIST             
*                                                                               
DISP20   CLC   FILTER,WRIRFL       IS IT THE SAME?                              
         BE    DISPX                YES                                         
         MVC   WRIRFL,FILTER        NO - CHANGE IT BACK                         
         MVC   GTMSGNO,=Y(SECLOCK) SET SECURITY LOCKOUT                         
         B     *+10                                                             
*                                                                               
DISP30   MVC   GTMSGNO,=Y(ENOFILMS) SET MISSING FILTER ERROR                    
         CLI   DDS,C'Y'            DDS?                                         
         BNE   *+14                 NO                                          
         XC    GTMSGNO,GTMSGNO                                                  
         B     DISPX                                                            
*                                                                               
         LA    R1,WRIMEDH          NO-CLEAR ALL UPROTECTED FIELDS               
         SR    R0,R0                                                            
         B     DISP50                                                           
*                                                                               
DISP40   ICM   R0,1,0(R1)                                                       
         JZ    MYCURSER                                                         
         AR    R1,R0                                                            
*                                                                               
DISP50   TM    1(R1),X'20'                                                      
         BO    DISP40                                                           
         LLC   RE,0(R1)                                                         
         SHI   RE,9                                                             
         BM    DISP40                                                           
         TM    1(R1),X'02'                                                      
         BZ    *+12                                                             
         SHI   RE,8                                                             
         BM    DISP40                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         OI    6(R1),X'80'                                                      
         B     DISP40                                                           
*                                                                               
DISPX    J     XIT                                                              
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VREC     NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'        IF OFFLINE,                                  
         BNE   VREC70                                                           
         OI    GENSTAT7,GES7ERHK   SET FOR ERROR HOOK                           
         LA    R3,MAXCOLS*2        GET STORAGE FOR MORE EXT. COL FILTS          
         LA    RE,EXTCOLSL                                                      
         MR    R2,RE               MAX N'RECORDS * L'ENTRY                      
         LA    R3,4(R3)                                                         
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R3,0(RE)            +0 = L'TABLE                                 
         S     R3,=F'4'                                                         
         LA    RE,4(RE)                                                         
         STCM  RE,7,AXTCOLS                                                     
         XCEF  (RE),(R3)           CLEAR TABLE                                  
*                                                                               
VREC70   CLI   RPTOVLY,0           TEST USER REPORT OVERLAY                     
         BE    VREC80                                                           
         GOTO1 CALLOV,DMCB,(RPTOVLY,0),0,0  YES-LOAD IT                         
         CLI   4(R1),X'FF'                                                      
         BE    EROPT                                                            
         MVC   ARPTNTRY,0(R1)                                                   
         GOTOR RPT,RPFIRST                                                      
*                                                                               
VREC80   LA    R2,WRIMEDH          FIRST VALIDATE MEDIA                         
         GOTO1 VALMED                                                           
*                                                                               
         LA    R2,WRIOPTH          VALIDATE OPTIONS EARLY                       
         GOTO1 VALOPTS                                                          
*                                                                               
         LA    R2,WRIOTHH          SECOND OPTIONS LINE                          
         GOTO1 VALOTHER                                                         
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE,                                  
         BNE   VREC100                                                          
*                                                                               
         TM    OPTIND5,OPTPQIX     TEST TESTING?                                
         BNZ   VREC90               YES - ALWAYS GIVE INDEX                     
*                                                                               
         L     RF,TWAMASTC         ALLOCATE PQINDEX TABLE                       
         USING MASTD,RF                                                         
         TM    MCOPT1,MCQ1PQIX     PQIX?                                        
         BZ    VREC100                                                          
         OC    MCREMOTE,MCREMOTE   TEST DIRECT                                  
         BNZ   *+14                                                             
         OC    MCREMPQK,MCREMPQK   TEST SOON                                    
         BZ    VREC100                                                          
         DROP  RF                                                               
*                                                                               
VREC90   LA    R3,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS                              
         LA    RE,PQINDXEQ                                                      
         MR    R2,RE               MAX RECS * L'ENTRY                           
         AHI   R3,4                EXTRA 4 BYTES FOR TABLE LENGTH               
         ST    R3,DMCB+4                                                        
         ST    R3,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R3,0(RE)            +0 = L'TABLE                                 
         AHI   R3,-4                                                            
         AHI   RE,4                                                             
         ST    RE,APQINDEX                                                      
         XCEF  (RE),(R3)                                                        
*                                                                               
VREC100  TM    REQIND,REQITRAN     TEST RECORD=TRANSMIT                         
         BZ    *+12                                                             
         OI    DOWNOPT,GLDLACTV+GLDLNOTR+GLDLALPH+GLDLNOHD+GLDLCUT              
         OI    OPTIND3,OPTINORQ                                                 
*                                                                               
         TM    DOWNOPT,GLDLACTV    DOWNLOADING?                                 
         BZ    VREC108              NO                                          
         TM    OPTIND5,OPTPQIX     AND PQIX?                                    
         BZ    *+8                  NO                                          
         OI    DOWNOPT,GLDLHEAD     YES - FORCE DOWNHEAD                        
*                                                                               
         CLC   SBQAGY,=C'WI'       IF INITIATIVE                                
         BE    VREC106                                                          
         CLC   SBQAGY,=C'MD'                                                    
         BE    VREC106                                                          
         CLC   SBQAGY,=C'WR'                                                    
         BNE   VREC108                                                          
VREC106  OI    DOWNOPT2,GLD2FIX                                                 
*                                                                               
VREC108  TM    OPTIND3,OPTINORQ    OPTION TO SUPPRESS REQUEST DETAILS           
         BZ    *+8                                                              
         OI    GENSTAT2,NOREQDET                                                
*                                                                               
         MVI   FRSTLAST,C'N'       DEFAULT TO NO RUNFRST/RUNLAST                
         BAS   RE,DOWNCHK          CHECK FOR DOWNLOAD                           
*                                                                               
VREC110  LA    R2,WRICLTH          CLIENT                                       
         GOTO1 VALCLT                                                           
*                                                                               
         LA    R2,WRIPRDH          PRODUCT                                      
         GOTO1 VALPRD                                                           
*                                                                               
         LA    R2,WRIESTH          ESTIMATE                                     
         GOTO1 VALEST                                                           
*                                                                               
         LA    R2,WRIMKTH          MARKET                                       
         CLI   5(R2),0                                                          
         BE    VREC120                                                          
         GOTO1 VALMKT                                                           
*                                                                               
VREC120  LA    R2,WRISTAH          STATION                                      
         CLI   5(R2),0                                                          
         BE    VREC130                                                          
         GOTO1 VALSTAT                                                          
*                                                                               
VREC130  LA    R2,WRIFLTH          OPTIONAL FILTERS                             
         GOTO1 VALFILT                                                          
*                                                                               
         LA    R2,WRIPERH          PERIOD                                       
         OC    ARFPBLK,ARFPBLK     RFP MODE?                                    
         BZ    VREC140              NO                                          
         BAS   RE,VALRFP           CHECK IF VALID RFP SYMBOL                    
         BNE   VREC140              NO - CHECK IF VALID DATE                    
         CLC   9(2,R2),=Y(SP#PERD) ONLY PERIOD IS VALID HERE                    
         BE    VREC220                                                          
         MVI   ERROR,INVDATE                                                    
         J     TRAPERR                                                          
*                                                                               
VREC140  TM    OPTIND2,OPTINOP     OPTION NOT TO VALIDATE PERIOD                
         BO    VREC220                                                          
         GOTO1 VALPER                                                           
         CLI   WLPROF+1,X'FE'      USING SECOND WEEK LIMIT?                     
         BNE   VREC220                                                          
         LA    R1,WRICOLSH         TEST ANY DEMOS IN COLUMNS                    
         LA    RF,14                                                            
*                                                                               
VREC150  CLI   5(R1),0                                                          
         BE    VREC170                                                          
         LLC   RE,5(R1)                                                         
         SHI   RE,2                                                             
         BNP   VREC170                                                          
         LA    R3,8(R1)                                                         
*                                                                               
VREC160  CLC   0(3,R3),=C'DEM'                                                  
         BE    VREC210                                                          
         CLC   0(3,R3),=C'CPP'                                                  
         BE    VREC210                                                          
         CLC   0(3,R3),=C'PAR'                                                  
         BE    VREC210                                                          
         CLC   0(3,R3),=C'AAR'                                                  
         BE    VREC210                                                          
         CLC   0(3,R3),=C'RAR'                                                  
         BE    VREC210                                                          
         CLC   0(3,R3),=C'PUT'                                                  
         BE    VREC210                                                          
         CLC   0(3,R3),=C'SHR'                                                  
         BE    VREC210                                                          
         LA    R3,1(R3)                                                         
         BCT   RE,VREC160                                                       
*                                                                               
VREC170  LLC   RE,0(R1)                                                         
         AR    R1,RE                                                            
         IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         BCT   RF,VREC150                                                       
*                                                                               
* IF THERE IS A STACK, MAKE SURE NO STACK DEMO KEYWORDS                         
         CLI   STACKDEF,0                                                       
         BE    VREC220                                                          
         LA    RE,STACKDEF                                                      
*                                                                               
VREC180  CLI   0(RE),0                                                          
         BE    VREC220                                                          
         LA    RF,STNODEM                                                       
*                                                                               
VREC190  CLC   0(1,RE),0(RF)                                                    
         BE    VREC200                                                          
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BE    VREC210             ENTRY NOT IN VALID TABLE                     
         B     VREC190                                                          
*                                                                               
VREC200  LA    RE,2(RE)            NEXT STACKDEF                                
         B     VREC180                                                          
*                                                                               
VREC210  MVC   GTMSGNO,=Y(ENODEMS)                                              
         J     MYCURSER                                                         
*                                                                               
STNODEM  DS    0H                  TABLE OF NON-DEMO STACK KEYWORDS             
         DC    AL1(STSPACE,STDIFF,STNDX,STGOAL)                                 
         DC    AL1(STGROSS,STNET,STSPOTS,STGDOL)                                
         DC    AL1(STGNDOL,STBUY,STPAID,STUNPD)                                 
         DC    AL1(STBIL,STBILBL)                                               
         DC    X'FF'                                                            
         DS    0H                                                               
*                                                                               
VREC220  CLI   DPGFILE,0           TEST DPG FILE NUMBER PROVIDED                
         BNE   VREC240             YES-SKIP RPT DEFINITION VALIDATION           
*                                                                               
         LA    R2,WRIHEADH         HEADERS                                      
         MVI   MAX,MAXHEADS                                                     
         GOTO1 VALHEAD                                                          
*                                                                               
         LA    R2,WRIMIDH          MIDLINE                                      
         MVI   MAX,MAXMIDS                                                      
         GOTO1 VALMID                                                           
*                                                                               
         LA    R2,WRIROWSH         ROWS                                         
         MVI   MAX,MAXROWS                                                      
         GOTO1 VALROWS                                                          
*                                                                               
         LA    R1,SBLOCK           ESTABLISH SBLOCK                             
         USING SBLOCK,R1                                                        
         OC    SBSQDQT1(2*SBSQDOPL),SBSQDQT1 IF DOING SQAD LOOKUP               
         BZ    *+18                                                             
         SR    RF,RF                                                            
         IC    RF,MYFIRSTH                                                      
         AHI   RF,1                                                             
         STC   RF,MYFIRSTH            ALLOW FOR 1 EXTRA HEADLINE                
         DROP  R1                                                               
*                                  COLUMNS                                      
         MVI   SBQSKIP,SBQSKBUY+SBQSKGL+SBQSKBIL                                
         LA    R2,WRICOLSH                                                      
         MVI   MAX,MAXCOLS                                                      
         GOTO1 VALCOLS                                                          
*                                                                               
VREC240  BAS   RE,DOWNCHK          CHECK FOR DOWNLOAD AGAIN                     
         CLI   RPTSCRN,0           TEST USER SCREEN                             
         BNE   VREC250                                                          
         LA    R2,WRITITH          NO- VALIDATE TITLE                           
         GOTO1 VALTITS                                                          
*                                                                               
VREC250  XC    WORK,WORK           READ WR PROFILE                              
         MVC   WORK(4),=C'S0WR'                                                 
         MVC   WORK+4(2),SBQAGY                                                 
         MVC   WORK+6(1),SBQMED                                                 
         OC    SBQBCLT,SBQBCLT                                                  
         BZ    VREC260                                                          
         MVC   WORK+7(3),SBQCLT                                                 
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         B     VREC270                                                          
*                                                                               
VREC260  CLI   SBQCLT,C'*'                                                      
         BNE   VREC270                                                          
         MVC   WORK+10(2),SBQCLT                                                
*                                                                               
VREC270  GOTO1 GETPROF,DMCB,WORK,SBWRPROF,DATAMGR                               
*                                                                               
         MVC   WORK(4),=C'SWR2'    GET WR2 PROFILE                              
         NI    WORK,X'FF'-X'40'    MAKE THE S LOWERCASE                         
         GOTO1 GETPROF,DMCB,WORK,SBWR2PRF,DATAMGR                               
*                                                                               
         CLI   DPGFILE,0           FOR REGULAR WRITER,                          
         BNE   VREC280                                                          
         CLI   SBWRPROF,C'Y'       TEST FILTER REQUIRED                         
         BNE   VREC280                                                          
         LA    R2,WRIRFLH          YES-                                         
         GOTO1 ANY                                                              
         CLI   5(R2),L'WRIRFL                                                   
         JNE   EFILT                                                            
*                                                                               
VREC280  GOTO1 AFINVAL             FINAL SCREEN VALIDATION                      
*                                                                               
         OC    RPT2ID,RPT2ID       SKIP IF NO SECOND REPORT TO BE DONE          
         BZ    VREC300                                                          
         TM    OPTIND5,OPT5CONT    ONLY CONTINUATION REPORTS HERE               
         BZ    VREC300                                                          
         LR    R1,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   R1,4096                                                          
         USING SYSD+4096,R1                                                     
         MVC   DRSTBUF,DRCURBUF    RESET DRONE BUFFER START                     
         DROP  R1                                                               
*                                                                               
         XC    GTMSGNO,GTMSGNO     MAKE SURE CLEAR ON CALL                      
         BRAS  RE,RP2GET           READ IN 2ND REPORT                           
*                                                                               
VREC300  GOTO1 WRAPDRON            WRAP UP DRONE                                
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    VREC400             YES - DONE                                   
         CLI   ACTNUM,ACTREP       ACTION = REPORT?                             
         BNE   VREC400             NO - DONE                                    
***      TM    WHEN,X'20'          SOON REQUEST?                                
***      BZ    VREC400             NO - DONE                                    
         CLC   =C'BGL',CONREC      BGL REPORT?                                  
         BE    VREC305             YES                                          
         CLC   =C'SL',CONREC       STATION LOCKIN REPORT?                       
         BE    VREC305             YES                                          
         CLC   =C'WRITER',CONREC   WRITER REPORT?                               
         BNE   VREC400             NO - DONE                                    
***      TM    COLIND3,COLIDEML    HAVE DEMO LOOKUPS?                           
***      BNZ   *+12                YES                                          
***      MVI   REQSML,0            IN CASE THEY USED THE "DEMOS=" OPT           
***      B     VREC400             YES                                          
         CLI   REQSML,C'C'         ALREADY SET TO COMSCORE REQUEST?             
         BE    VREC324             YES - DONE                                   
         OC    COMDLIST,COMDLIST   HAVE COMSCORE DEMO FROM DEMO MENU?           
         BZ    *+12                NO                                           
         MVI   REQSML,C'C'         SET TO COMSCORE REQUEST                      
         B     VREC324             DONE                                         
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BE    *+12                YES - READ THE MEDIA T ESTIMATES             
         CLI   SBQMED,C'T'         MEDIA T REQUEST?                             
         BNE   VREC400             NO - DONE                                    
*                                                                               
VREC305  LA    R5,KEY              R5 = ESTIMATE KEY                            
         USING ESTHDRD,R5          ESTIMATE RECORD DSECT                        
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   EKEYAM,SBBAGYMD     A/M                                          
         MVC   EKEYCLT,SBBCLT      NULLS IF NOT A SINGLE CLT REQUEST            
         MVC   EKEYPRD,=C'POL'     POL EST HAVE THE SUPERSET OF DEMOS           
         MVC   EKEYEST,SBQEST      ESTIMATE START                               
*                                                                               
VREC310  GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   EKEYAM,SBBAGYMD     SAME A/M?                                    
         BNE   VREC330             NO - DONE                                    
*                                                                               
         OC    SBBCLT,SBBCLT       SINGLE CLIENT REQUEST?                       
         BZ    *+14                NO - TREAT AS AN "ALL" CLT REQUEST           
         CLC   EKEYCLT,SBBCLT      SAME CLIENT CODE?                            
         BNE   VREC330             NO - DONE                                    
*                                                                               
         CLC   EKEYPRD,=C'POL'     POL PRODUCT?                                 
         BE    VREC315             YES                                          
         BH    VREC320             IF HIGH - FORCE NEXT CLIENT                  
         MVC   EKEYPRD,=C'POL'     NO - READ FOR POL PRODUCT                    
         XC    EKEY+8(5),EKEY+8    CLEAR KEY AFTER ESTIMATE                     
         B     VREC310             READ HIGH FOR POL PRODUCT                    
*                                                                               
VREC315  CLC   EKEYEST,SBQEST      ESTIMATE >= ESTIMATE START?                  
         BNL   *+20                YES                                          
         MVC   EKEYEST,SBQEST      BUMP TO ESTIMATE START                       
         XC    EKEY+8(5),EKEY+8    CLEAR KEY AFTER ESTIMATE                     
         B     VREC310             READ HIGH FOR NEXT ESTIMATE                  
*                                                                               
         CLC   EKEYEST,SBQESTND    ESTIMATE <= ESTIMATE END?                    
         BNH   *+14                YES                                          
VREC320  MVC   EKEY+4(9),XFF       NO - BUMP TO NEXT CLIENT                     
         B     VREC310             READ HIGH FOR NEXT CLIENT                    
*                                                                               
         OC    EKEY+8(5),EKEY+8    IS THIS AN ESTIMATE RECORD?                  
         BZ    *+14                YES                                          
         MVC   EKEY+8(5),XFF       NO - FORCE NEXT ESTIMATE                     
         B     VREC310             READ HIGH FOR NEXT ESTIMATE                  
*                                                                               
         GOTO1 GETREC              READ ESTIMATE RECORD                         
*                                                                               
         L     R5,AIO              A(ESTIMATE RECORD)                           
*                                                                               
         CLC   ELEN,=AL2(ESTHDR2Q) NEW EST REC LEN?                             
         BL    VREC325             NO                                           
         OC    ENONTDMS(160),ENONTDMS  HAVE ANY COMSCORE DEMOS?                 
         BZ    VREC325             NO                                           
         MVI   REQSML,C'C'         COMSCORE REQUEST                             
*                                                                               
VREC324  TM    WHEN,X'20'          SOON REQUEST?                                
         BNZ   VREC330             YES - DONE                                   
         OI    GENSTAT7,GES7COMS   FLAG OVERNIGHT COMSCORE                      
         B     VREC330             YES - DONE                                   
*                                                                               
VREC325  LA    R5,KEY              R5 = ESTIMATE KEY                            
         MVC   EKEY+8(5),XFF       NO - FORCE NEXT ESTIMATE                     
         B     VREC310             READ HIGH FOR NEXT ESTIMATE                  
         DROP  R5                  DROP ESTIMATE RECORD USING                   
*                                                                               
VREC330  CLC   =C'SL',CONREC       STATION LOCKIN REPORT?                       
         BNE   VREC400             NO                                           
         TM    FLAGS,FLMLOCK       ADD ML REQUEST?                              
         BZ    VREC400             NO                                           
*                                                                               
         BRAS  RE,ADDMLREQ         ADD ML REQUEST                               
*                                                                               
VREC400  J     XIT                 EXIT                                         
*                                                                               
DOWNCHK  CLI   DOWNOPT,0           IF WE ARE DOWNLOADING                        
         BER   RE                                                               
*                                                                               
         OC    ARFPBLK,ARFPBLK     RFP MODE?                                    
         BZ    *+10                                                             
         MVC   TWAOUT,=CL8'DOWN'   MAKE SURE TWAOUT SET                         
*                                                                               
         OI    REQRTYP,REQTDOWN    PRINT FILE WILL HAVE TYPE DOWN               
***      CLI   CONOUT,C' '         TEST OUTPUT TYPE NOT REQUESTED               
***      BHR   RE                                                               
         OC    ARFPBLK,ARFPBLK     RFP MODE?                                    
         BNZR  RE                  YES - DON'T OVERWRITE FILENAME               
         MVC   CONOUT(8),=CL8'DOWN'   YES-DEFAULT TO OUTPUT OF 'DOWN'           
         MVI   CONOUTH+5,4                                                      
         OI    CONOUTH+6,X'80'                                                  
         MVC   TWAOUT,CONOUT                                                    
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
VALRFP   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    VRFPNEQ                                                          
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING QRFPD,R3                                                         
         MVI   QRFPMODE,QRFPSYMB   SYMBOLIC NAME VALIDATION                     
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),8(R2)   PASS SYMBOLIC NAME                           
         OC    QRFPWORK,BLANKS                                                  
         GOTO1 RFP,DMCB,(R3)                                                    
         OC    QRFPWORK,QRFPWORK   ERROR                                        
         BZ    VRFPNEQ                                                          
         MVC   8(L'QRFPESC,R2),QRFPWORK                                         
         MVI   5(R2),17            SET LENGTH OF EXPLODED DATA                  
         MVI   11(R2),17           PASS LENGTH OF EXPLODED DATA                 
         OI    4(R2),X'20'         SET VALIDATED BIT                            
         OI    6(R2),X'80'         AND TRANSMIT                                 
VRFPEQ   CR    RB,RB                                                            
         J     XIT                                                              
VRFPNEQ  LTR   RB,RB                                                            
         J     XIT                                                              
         EJECT                                                                  
* FORMAT EXTRA DATA FOR MARKET MIDLINE                                          
*                                                                               
MKTMID   NTR1  BASE=*,LABEL=*                                                   
         MVC   GLMIDXTR,BLANKS                                                  
         CLI   SBQMKTWT,C'N'                                                    
         JE    XIT                                                              
         CLC   MIDLEV,MKTLEV                                                    
         JNE   XIT                                                              
         LA    RF,GLMIDXTR                                                      
         MVC   0(5,RF),=C'MKTS='                                                
         ICM   R4,15,TOTMKT                                                     
         ICM   R5,15,TOTWGT                                                     
         EDIT  (R4),(3,5(RF)),ALIGN=LEFT                                        
         MVI   8(RF),C','                                                       
         MVC   9(4,RF),=C'CVG='                                                 
         EDIT  (R5),(6,13(RF)),2,ALIGN=LEFT                                     
         J     XIT                                                              
         EJECT                                                                  
* ERROR EXITS                                                                   
*                                                                               
EFILT    DS    0H                                                               
         MVC   GTMSGNO,=Y(EFILTMS)                                              
         J     MYCURSER                                                         
EROPT    DS    0H                                                               
         MVC   GTMSGNO,=Y(OPTERR)                                               
         LA    R2,WRIOPTH                                                       
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*====================================================================*          
* PROCB2: PROCESS BILL HEADER RECORDS (NEW PASS)                     *          
*         ONLY BUILD A TABLE OF BILL HEADER INVOICE DATES            *          
*====================================================================*          
         SPACE 2                                                                
PROCB2   NTR1  BASE=*,LABEL=*                                                   
PROCB2X  J     XIT                                                              
         EJECT                                                                  
* FINAL SCREEN VALIDATION                                                       
*                                                                               
FINALVAL NMOD1 0,**401F**                                                       
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
         USING GETTXTD,GETTXTCB                                                 
         CLI   DPGFILE,0           TEST DPG FILE NUMBER                         
         BE    FV04                                                             
         CLI   OFFLINE,C'Y'        AND OFFLINE                                  
         BNE   FV03                                                             
         GOTO1 CALLOV,DMCB,(DPGFILE,0),0,0  YES-GET ITS ADDRESS                 
         CLI   4(R1),X'FF'                                                      
         BE    FV99                                                             
         MVC   ADPGPROG,0(R1)                                                   
*                                                                               
FV03     CLI   RPTOVLY,0           CHECK FOR REPORT OVERLAY                     
         BE    FV99                                                             
         GOTOR RPT,RPINIT          CALL IT FOR INITIALIZATION                   
*                                                                               
FV04     GOTOR RPT,RPVAL           CALL FOR FURTHER VALIDATION                  
         CLI   OFFLINE,C'Y'        IF ONLINE, SET SIZE/PRIORITY                 
         BE    FV05                                                             
*                                                                               
* PER STEVE PEEPLES, ZENITH MIRAMAX WRITERS ARE CRITICAL TO RUN FAST            
         CLC   AGENCY,=C'TH'       ZENITH?                                      
         BNE   FV04A                                                            
         CLC   SBQCLT,=C'MIR'      CLT MIRAMAX?                                 
         BNE   FV04A                                                            
         CLC   SBQEST,SBQESTND     ONE EST                                      
         BNE   FV04A                                                            
         CLC   =C'GHMIRUPL',WRINAM  ONLY FORMAT GHMIRUPL                        
         BE    *+14                                                             
         CLC   =C'GHMIRRUP',WRINAM  OR GHMIRRUP                                 
         BNE   FV04A                                                            
         MVI   REQPRI2,C'7'        SET PRIORITY 7                               
         B     FV05                                                             
*                                                                               
FV04A    CLC   AGENCY,=C'DF'       SAATCHI?                                     
         BNE   FV04B               NO                                           
         CLC   =C'X3',WRINAM       WRITER NAME BEGINS WITH X3?                  
         BE    FV04C               YES                                          
*                                                                               
FV04B    OC    SBQBCLT,SBQBCLT     IS THERE A CLIENT?                           
         BNZ   FV05                 YES                                         
*                                                                               
FV04C    CLI   REQSML,C'C'         ALREADY SET TO COMSCORE?                     
         BE    FV05                YES - DO NOT OVERWRITE REQSML                
         MVI   REQSML,C'L'         MAKE IT A L(ARGE) REQUEST                    
*                                                                               
FV05     OC    SBQDEMOS,SBQDEMOS   TEST DEMO MENU                               
         BZ    FV06                NO                                           
         LA    R4,KEY              YES-VALIDATE IT                              
         USING DMNRECD,R4                                                       
         XC    DMNKEY,DMNKEY                                                    
         MVC   DMNKTYP,=X'0D26'                                                 
         MVC   DMNKAGMD,SBBAGYMD                                                
         MVC   DMNKCODE,SBQDEMOS                                                
         GOTO1 HIGH                                                             
         CLC   DMNKEY,KEYSAVE                                                   
         BNE   FV96                                                             
         DROP  R4                                                               
*                                                                               
FV06     CLI   SBQDPTMN,C' '       TEST DAYPART MENU SET                        
         BNH   FV08                                                             
         XC    DMCB,DMCB           YES-VALIDATE IT                              
         MVC   DMCB(2),SBQAGY                                                   
         MVC   DMCB+2(1),SBQMED                                                 
         MVC   DMCB+3(1),SBQDPTMN                                               
         GOTO1 DPTRD,DMCB,,AIO3,DATAMGR                                         
         CLI   DMCB+8,X'FF'                                                     
         BE    FV95                                                             
*                                                                               
FV08     TM    COLIND,COLIDEM      TEST ANY DEMO NAMES WILL APPEAR              
         BZ    FV10                IN THE COLUMN HEADLINES                      
         OC    SBQDEMOS,SBQDEMOS   YES-TEST DEMOS MIGHT VARY                    
         BNZ   *+14                                                             
         OC    SBPDEMOS,SBPDEMOS                                                
         BZ    FV09                                                             
         TM    ROWIND,ROWITGT      NO-TEST FOR TARGET ROW                       
         BO    FV90                YES-INVALID COMBO                            
         TM    ROWIND2,ROWITGT2    TARGET3/4 ROW?                               
         BO    FV90                YES-INVALID COMBO                            
         B     FV10                                                             
*                                                                               
FV09     GOTO1 HEDCHK,1            CHECK THE HEADLINES                          
*                                                                               
FV10     TM    ROWIND,ROWIDPT+ROWIDPLN   TEST DPT OR DPTLEN IS A ROW            
         BZ    FV12                                                             
         CLI   SBQDPTMN,C' '       YES - TEST DAYPART MENU SET                  
         BH    FV12                                                             
         GOTO1 HEDCHK,2            NO - CHECK THE HEADLINES                     
*                                                                               
FV12     CLI   BYDEMSTK,0          TEST FOR BUY DEMO STACK                      
         BE    FV20                                                             
         OC    SBQDEMOS,SBQDEMOS   YES-MENU OR DEMOS OPTION MUST BE SET         
         BNZ   FV20                                                             
         OC    SBPDEMOS,SBPDEMOS                                                
         BZ    EDEMS                                                            
*                                                                               
FV20     TM    COLIND,COLIGDEM     TEST DEMO MENU REQUIRED FOR                  
         BZ    FV21                GOAL DEMOS                                   
         OC    SBQDEMOS,SBQDEMOS   YES-TEST MENU SET                            
         BNZ   FV21                                                             
         OC    SBPDEMOS,SBPDEMOS       OR DEMO OPTION SET                       
         BNZ   FV21                                                             
         LA    R2,WRIOPTH              NO-ERROR                                 
         B     EMENU                                                            
*                                                                               
FV21     CLI   SBQMGRD,0           TEST MARKET GROUPS REQUESTED                 
         BH    FV23                                                             
         LA    R0,L'LEVELS         NO-TEST MKT GRP IN ROWS                      
         LA    R1,LEVELS                                                        
*                                                                               
FV22     CLI   0(R1),QMKTGR1                                                    
         BE    FV98                YES-ERROR                                    
         CLI   0(R1),QMKTGR2                                                    
         BE    FV98                                                             
         CLI   0(R1),QMKTGR3                                                    
         BE    FV98                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,FV22                                                          
*                                                                               
FV23     CLI   SBQCGRD,0           TEST CLIENT GROUPS REQUESTED                 
         BH    FV25                                                             
         LA    R0,L'LEVELS         NO-TEST CLT GRP IN ROWS                      
         LA    R1,LEVELS                                                        
*                                                                               
FV24     CLI   0(R1),QCLTGR1                                                    
         BE    FV81                YES-ERROR                                    
         CLI   0(R1),QCLTGR2                                                    
         BE    FV81                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,FV24                                                          
*                                                                               
FV25     CLI   SBQSGRD,C' '        TEST STATION GROUPS REQUESTED                
         BH    FV27                                                             
         LA    R0,L'LEVELS         NO-TEST STA GRP IN ROWS                      
         LA    R1,LEVELS                                                        
*                                                                               
FV26     CLI   0(R1),QSTAGR1                                                    
         BE    FV82                YES-ERROR                                    
         CLI   0(R1),QSTAGR2                                                    
         BE    FV82                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,FV26                                                          
*                                                                               
FV27     CLI   SBQPGRD,C' '        TEST PRODUCT GROUPS REQUESTED                
         BNH   *+16                                                             
         TM    SBQPIND,SBQPPB      YES-TEST PBSPLIT=NO                          
         BO    FV94                YES-CANNOT HAVE PRDGRPS IN THAT CASE         
         B     FV30                                                             
         CLI   DEMGRP,C'P'         NO PRDGRPS - TEST DEMOPGRP OPTION            
         BE    FV97                YES-ERROR                                    
         LA    R0,L'LEVELS         TEST PRD GRP IN ROWS                         
         LA    R1,LEVELS                                                        
*                                                                               
FV28     CLI   0(R1),QPRDGR1                                                    
         BE    FV97                YES-ERROR                                    
         CLI   0(R1),QPRDGR2                                                    
         BE    FV97                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,FV28                                                          
*                                                                               
FV30     CLI   DEMGRP,C'P'         TEST DEMOPGRP AND MENU OPTIONS SET           
         BNE   *+14                                                             
         OC    SBQDEMOS,SBQDEMOS                                                
         BNZ   FV89                YES-IMCOMPATIBLE                             
*                                                                               
         TM    ROWIND,ROWIDPT+ROWIDPLN TEST DPT AND DPTLEN ARE ROWS             
         BNO   FV32                                                             
         LA    R2,WRIROWSH         YES-ERROR                                    
         B     EDPTLEN                                                          
*                                                                               
FV32     TM    COLIND3,COLIBLST    BILL STATUS KEYWORD?                         
         BNZ   FV33                 ALLOW PERIOD KEYWORDS                       
         TM    SBQREAD,SBQRDBH     TEST READING BILL HEADERS                    
         BZ    FV33                                                             
         TM    ROWIND,ROWIPER      YES-IS THERE A PERIOD ROW                    
         BO    FV93                YES-ERROR                                    
         TM    OPTIND3,OPTIBD      TEST FOR BD= OPTION                          
         BO    FV80                YES-ERROR (THEY WANT BHRDATE=)               
*                                                                               
FV33     CLC   =C'HD',CONREC       HD BILLING REPORT?                           
         BNE   FV34                                                             
         TM    ROWIND,ROWICML      TEST COMMERCIAL IN ROWS                      
         BZ    FV34                                                             
         OC    SBQBCLT,SBQBCLT     AND NOT SINGLE CLIENT REQUEST                
         BNZ   FV34                                                             
         CLC   SBQAGY,=C'SC'       AND NOT SCALI ....                           
         BE    FV34                                                             
         CLC   SBQAGY,=C'BJ'       AND NOT BOZELL....                           
         BE    FV33A                                                            
         CLC   SBQAGY,=C'GZ'       AND NOT GENERAL MOTORS...                    
         BE    *+10                                                             
         CLC   SBQAGY,=C'MC'       AND NOT MCCAN.....                           
         BNE   FV33B                                                            
         CLI   SBQCGRD,0           MCCAN/GENERAL MOTORS ONLY CLT GROUPS         
         BE    FV33B                                                            
*                                                                               
FV33A    CLI   WHEN,X'20'          BJ/MC NOT SOON                               
         BNE   FV34                                                             
*                                                                               
FV33B    LA    R2,WRICLTH          YES-ERROR                                    
         MVI   ERROR,INVCLT                                                     
         J     TRAPERR                                                          
*                                                                               
FV34     OC    SBQDEMOS,SBQDEMOS   IF THERE'S NO DEMO MENU,                     
         BNZ   FV40                                                             
         OC    SBPDEMOS,SBPDEMOS   OR DEMO OPTION                               
         BNZ   FV40                                                             
         XC    FULL,FULL           MAKE SURE THAT IF MKTRNK IS A ROW,           
         CLC   SBQEST,SBQESTND     THEN THE DEMOS WON'T CHANGE BELOW            
         BE    *+12                                                             
         CLI   SBQSEPES,C'Y'                                                    
         BE    *+18                                                             
         CLC   SBQPRD,=C'ALL'                                                   
         BNE   FV40                                                             
         B     *+8                                                              
         MVI   FULL,1                                                           
         LA    R0,L'LEVELS                                                      
         LA    R1,LEVELS                                                        
*                                                                               
FV35     CLI   0(R1),QMKTRNK                                                    
         BE    FV92                                                             
         CLC   SBQPRD,=C'ALL'                                                   
         BNE   FV36                                                             
         CLI   0(R1),QPRD                                                       
         BE    *+12                                                             
         CLI   0(R1),QPRDNM                                                     
         BNE   FV36                                                             
         CLI   FULL,0                                                           
         BE    FV40                                                             
         CLI   FULL+2,1                                                         
         BE    FV40                                                             
         MVI   FULL+1,1                                                         
         B     FV38                                                             
*                                                                               
FV36     CLI   FULL,0                                                           
         BE    FV38                                                             
         CLI   0(R1),QEST                                                       
         BE    *+12                                                             
         CLI   0(R1),QESTNM                                                     
         BNE   FV38                                                             
         CLC   SBQPRD,=C'ALL'                                                   
         BNE   FV40                                                             
         CLI   FULL+1,1                                                         
         BE    FV40                                                             
         MVI   FULL+2,1                                                         
*                                                                               
FV38     LA    R1,1(R1)                                                         
         BCT   R0,FV35                                                          
*                                                                               
FV40     CLI   DATEOPT,DOBILL      TEST DATE=BD                                 
         BNE   FV42                                                             
         OC    SBQBILST,SBQBILST   YES- BD= AND PD= ARE INVALID                 
         BNZ   FV91                                                             
         OC    SBQPAYST,SBQPAYST                                                
         BNZ   FV91                                                             
         OI    SBQSKIP,SBQSKBUY+SBQSKGL  SKIP BUYS AND GOALS                    
         MVI   SBQREAD,0                                                        
         B     FV44                                                             
*                                                                               
FV42     CLI   DATEOPT,DOPAID      TEST DATE=PD                                 
         BNE   FV44                                                             
         OC    SBQPAYST,SBQPAYST   YES- PD= AND BD= ARE INVALID                 
         BNZ   FV91                                                             
         OC    SBQBILST,SBQBILST                                                
         BNZ   FV91                                                             
         OI    SBQSKIP,SBQSKGL+SBQSKBIL  SKIP GOALS AND BILLS                   
         MVI   SBQREAD,0                                                        
*                                                                               
FV44     TM    DATAIND6,DINETSTA   TEST NETWORK/STATION ROW                     
         BZ    FV46                                                             
         CLI   SBQMED,C'N'         YES-MEDIA MUST BE N OR C                     
         BE    *+12                                                             
         CLI   SBQMED,C'C'                                                      
         BNE   FV88                                                             
         CLI   MKTLEV,0            IMCOMPATIBLE WITH MARKET OR STATION          
         BNE   FV87                                                             
         TM    DATAIND2,DISTA                                                   
         BO    FV86                                                             
*                                                                               
FV46     TM    SBQPER,SBQPFL       TEST CHILD SPOT FLIGHTS                      
         BZ    FV48                                                             
         TM    SBQPER,255-SBQPFL   YES-CANNOT MIX IN ANY OTHER PERIODS          
         BNZ   FV85                                                             
         CLI   SBQBCLT,0           MUST BE SINGLE CLIENT                        
         BE    ECLT                                                             
         CLC   SBQPRD,=C'POL'      MUST HAVE POL=SEP IF PRD=POL                 
         BNE   FV48                                                             
         TM    SBQPIND,SBQPOLSP                                                 
         BZ    FV84                                                             
*                                                                               
FV48     LA    R1,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDRD(R1),C'C'  TEST CANADIAN AGENCY                 
         BNE   FV50                                                             
         TM    COLIND2,COLIRERT    YES-TEST RERATE DEMOS IN COLS                
         BZ    FV49                                                             
         LR    RF,R9                                                            
         AHI   RF,SBQBOOK-SYSD                                                  
         OC    0(4,RF),0(RF)       YES-BOOK OPTION IS REQUIRED                  
         BZ    FV83                                                             
*                                                                               
FV49     TM    OPTIND6,OPT6NM0B    USING ONLY NWK BUYS                          
         BZ    *+12                                                             
         TM    COLIND,COLIDEM      CANNOT HAVE DEMOS                            
         BNZ   FV104                                                            
         OC    ATIMRNG,ATIMRNG     ANY AFFID TIME FILTER?                       
         BZ    FV50                                                             
         CLI   SBQMED,C'N'         FOR MEDIA N                                  
         BNE   FV50                                                             
         TM    OPTIND6,OPT6NM0B    NEEDS ONLY NETWORK BUYS                      
         BZ    FV105                                                            
*                                                                               
FV50     TM    SBQREAD,SBQRDINF    READING INFOMERCIALS?                        
         BZ    FV52                                                             
         OC    SBQBCLT,SBQBCLT     YES - MUST BE FOR 1 CLT/PRD/EST              
         BZ    FV100                                                            
         CLI   SBQBPRD,0                                                        
         BE    FV100                                                            
         TM    WRIESTH+4,X'08'     ESTIMATE IS NUMERIC?                         
         BZ    FV100                                                            
*                                                                               
FV52     TM    COLIND3,COLICBAG    REPORTING CABLE AGGREGATE?                   
         BZ    FV60                 NO                                          
         LA    RF,SBAGYREC                                                      
         LA    R0,L'LEVELS         TEST MKT IN ROWS                             
         LA    R1,LEVELS                                                        
*                                                                               
         CLI   0(R1),QMKT                                                       
         BE    FV54                                                             
         CLI   0(R1),QMKTNM                                                     
         BE    FV54                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-20                                                          
*                                                                               
* MARKET MUST NOT BE IN ROWS FOR CANADA, MUST BE FOR US                         
         CLI   AGYPROF+7-AGYHDRD(RF),C'C'  TEST CANADIAN AGENCY                 
         BE    FV56                                                             
         B     FV101                                                            
*                                                                               
FV54     CLI   AGYPROF+7-AGYHDRD(RF),C'C'  TEST CANADIAN AGENCY                 
         BE    FV102                                                            
*                                                                               
* CNET MUST BE IN ROWS FOR BOTH                                                 
FV56     LA    R0,L'LEVELS         TEST CNET IN ROWS                            
         LA    R1,LEVELS                                                        
*                                                                               
         CLI   0(R1),QCNET                                                      
         BE    FV60                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     FV103                                                            
*                                                                               
FV60     TM    DATAIND6,DIDEMU     REPORTING UPGRADES?                          
         BZ    FV62                 NO                                          
         OC    SBQUPGRD,SBQUPGRD   MAKE SURE THERE'S A FORMULA                  
         BZ    *+14                                                             
         OC    SBQUPFBK,SBQUPFBK   AND A FROM BOOK                              
         BNZ   FV62                                                             
*                                                                               
         LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(MISUPGRD)                                             
         J     MYCURSER                                                         
*                                                                               
FV62     TM    DOWNOPT,GLDLACTV    DOWNLOADING?                                 
         BZ    FV64                 NO                                          
         TM    COLIND3,COLIBIGD    POSSIBLY BIG DATA FIELD?                     
         BZ    FV64                                                             
         OI    DOWNOPT2,GLD2128     YES - TELL DRIVDOWN                         
*                                                                               
FV64     B     FVX                                                              
*                                                                               
FV80     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(EBDATE)                                               
         J     MYCURSER                                                         
*                                                                               
FV81     LA    R2,WRICLTH                                                       
         MVC   GTMSGNO,=Y(ECGRPM)                                               
         J     MYCURSER                                                         
*                                                                               
FV82     LA    R2,WRISTAH                                                       
         MVC   GTMSGNO,=Y(ESGRPM)                                               
         J     MYCURSER                                                         
*                                                                               
FV83     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(ENOBOOK)                                              
         J     MYCURSER                                                         
*                                                                               
FV84     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(EPOLSEPM)                                             
         J     MYCURSER                                                         
*                                                                               
FV85     LA    R2,WRIHEADH                                                      
         MVC   GTMSGNO,=Y(EFLTM)                                                
         J     MYCURSER                                                         
*                                                                               
FV86     LA    R2,WRIHEADH                                                      
         MVC   GTMSGNO,=Y(ENSSTAM)                                              
         J     MYCURSER                                                         
*                                                                               
FV87     LA    R2,WRIHEADH                                                      
         MVC   GTMSGNO,=Y(ENSMKTM)                                              
         J     MYCURSER                                                         
*                                                                               
FV88     LA    R2,WRIMEDH                                                       
         MVI   ERROR,INVMED                                                     
         J     TRAPERR                                                          
*                                                                               
FV89     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(EDPGM)                                                
         J     MYCURSER                                                         
*                                                                               
FV90     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(ETGTM)                                                
         J     MYCURSER                                                         
*                                                                               
FV91     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(EDATM)                                                
         J     MYCURSER                                                         
*                                                                               
FV92     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(MENERR)                                               
         J     MYCURSER                                                         
*                                                                               
FV93     LA    R2,WRIHEADH                                                      
         MVC   GTMSGNO,=Y(EPERM)                                                
         J     MYCURSER                                                         
*                                                                               
FV94     LA    R2,WRIPRDH                                                       
         MVC   GTMSGNO,=Y(EPGPBM)                                               
         J     MYCURSER                                                         
*                                                                               
FV95     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(EDPTMM)                                               
         J     MYCURSER                                                         
*                                                                               
FV96     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(EDMENM)                                               
         J     MYCURSER                                                         
*                                                                               
FV97     LA    R2,WRIPRDH                                                       
         MVC   GTMSGNO,=Y(EPGRPM)                                               
         J     MYCURSER                                                         
*                                                                               
FV98     LA    R2,WRIMKTH                                                       
         MVC   GTMSGNO,=Y(EMGRPM)                                               
         J     MYCURSER                                                         
*                                                                               
FV99     LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(OPTERR)                                               
         J     MYCURSER                                                         
*                                                                               
FV100    LA    R2,WRICLTH                                                       
         MVC   GTMSGNO,=Y(ECLPRDES)                                             
         J     MYCURSER                                                         
*                                                                               
FV101    LA    R2,WRIHEADH                                                      
         MVC   GTMSGNO,=Y(EMKTREQ)                                              
         J     MYCURSER                                                         
*                                                                               
FV102    LA    R2,WRIHEADH                                                      
         MVC   GTMSGNO,=Y(EMKTNA)                                               
         J     MYCURSER                                                         
*                                                                               
FV103    LA    R2,WRIHEADH                                                      
         MVC   GTMSGNO,=Y(ECNETREQ)                                             
         J     MYCURSER                                                         
*                                                                               
FV104    LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(NWKBYOPX)                                             
         J     MYCURSER                                                         
*                                                                               
FV105    LA    R2,WRIOPTH                                                       
         MVC   GTMSGNO,=Y(NWKBYOP)                                              
         J     MYCURSER                                                         
*                                                                               
FVX      J     XIT                                                              
*                                                                               
         EJECT                                                                  
* CHECK THAT THE HEADLINES ARE SET CORRECTLY                                    
* R1 = 1 CHECKING DEMOS                                                         
*      2 CHECKING DPTS                                                          
*                                                                               
HEDCHK   NTR1  ,                                                                
         XC    HALF,HALF                                                        
         STC   R1,HALF             HALF(1)=1 OR 2                               
         CLI   HALF,1              TEST CHECKING DEMOS                          
         BNE   HC2                                                              
         TM    COLIND,COLIDEM1     YES-IF DEMO 1 IN COLS,                       
         BZ    *+16                                                             
         MVI   BYTE,QTARGET        TEST TARGET IN HEADLINES                     
         BAS   RE,TESTHEAD                                                      
         BNE   HC2                 NO                                           
         TM    COLIND,COLIDEM2     TEST DEMOS 2 IN COLS                         
         BZ    HC1                 NO-NO FURTHER CHECKING NEEDED                
         MVI   BYTE,QTARGET2       YES-TEST TARGET2 IN HEADLINES                
         BAS   RE,TESTHEAD                                                      
         BNE   HC2                 NO                                           
*                                                                               
HC1      CLC   SBQEST,SBQESTND     TARGET1/2 IN HEADLINES-                      
         BE    HCX                 TEST RANGE OF ESTIMATES                      
         MVI   SBQSEPES,C'Y'       YES-TREAT THEM AS SEPARATE                   
         B     HCX                                                              
*                                                                               
HC2      CLC   SBQPRD,=C'ALL'      TEST PRODUCT = ALL                           
         BNE   HC4                                                              
         MVI   BYTE,QPRD           YES - PRODUCT MUST BE IN                     
         BAS   RE,TESTHEAD               HEADLINES                              
         BE    HC4                                                              
         MVI   BYTE,QPRDNM                                                      
         BAS   RE,TESTHEAD                                                      
         BE    HC4                                                              
         CLI   HALF,1              TEST CHECKING DEMOS                          
         BNE   HC9                                                              
         CLI   DEMGRP,C'P'         YES-TEST GROUPING BY PRODUCT GROUP           
         BNE   HC9                                                              
         MVI   BYTE,QPRDGR1        YES-TEST PRODUCT GROUP IN HEADLINES          
         BAS   RE,TESTHEAD                                                      
         BE    HC4                                                              
         MVI   BYTE,QPRDGR2                                                     
         BAS   RE,TESTHEAD                                                      
         BNE   HC9                                                              
*                                                                               
HC4      MVI   HALF+1,1            INDICATE CHECKING ESTIMATE NOW               
         CLC   SBQEST,SBQESTND     TEST SEPARATE ESTIMATES                      
         BE    HCX                                                              
         CLI   SBQSEPES,C'Y'                                                    
         BNE   HCX                                                              
         MVI   BYTE,QEST           YES-ESTIMATE MUST BE IN HEADLINES            
         BAS   RE,TESTHEAD                                                      
         BE    HCX                                                              
         MVI   BYTE,QESTNM                                                      
         BAS   RE,TESTHEAD                                                      
         BE    HCX                                                              
*                                                                               
HC9      LA    R2,WRIOPTH                                                       
         CLI   HALF,1              TEST CHECKING DEMOS                          
         BNE   EDPT                NO-DPT OPTION REQUIRED                       
*                                                                               
         TM    SBQDEMOP,SBQDDEMO   NEW DEMO OPTION?                             
         BNZ   HCX                 YES - TARGET NOT NEEDED!                     
*                                                                               
         TM    ROWIND2,ROWITGT2    TARGET3/4 IN THE ROWS?                       
         BNZ   *+12                YES                                          
         TM    ROWIND,ROWITGT      YES-TEST TARGET IN THE ROWS                  
         BZ    EMENU               NO-DEMO MENU REQUIRED                        
         CLI   HALF+1,0            YES-EITHER PRODUCT OR ESTIMATE               
         BE    EPRD                    IS INVALID                               
         B     EEST                                                             
*                                                                               
HCX      J     XIT                                                              
         SPACE 2                                                                
TESTHEAD SR    R0,R0                                                            
         ICM   R0,1,LSTHEDLV                                                    
         BZ    TH2                                                              
         LA    RF,LEVELS                                                        
         CLC   BYTE,0(RF)                                                       
         BER   RE                                                               
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
TH2      LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* ERROR EXITS                                                                   
*                                                                               
EPRD     LA    R2,WRIPRDH                                                       
         MVI   ERROR,INVPROD                                                    
         J     TRAPERR                                                          
*                                                                               
EEST     LA    R2,WRIESTH                                                       
         MVI   ERROR,INVEST                                                     
         J     TRAPERR                                                          
*                                                                               
EMENU    MVC   GTMSGNO,=Y(MENERR)                                               
         J     MYCURSER                                                         
*                                                                               
EDEMS    MVC   GTMSGNO,=Y(EDEMSM)                                               
         LA    R2,WRIOPTH                                                       
         J     MYCURSER                                                         
*                                                                               
EDPTLEN  MVC   GTMSGNO,=Y(EDPTLENM)                                             
         J     MYCURSER                                                         
*                                                                               
ECLT     LA    R2,WRICLTH                                                       
         MVI   ERROR,INVCLT                                                     
         J     TRAPERR                                                          
*                                                                               
EDPT     DS    0H                                                               
         MVC   GTMSGNO,=Y(EDPTM)                                                
         J     MYCURSER                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
* EXTENSION ROUTINES                                                            
         DS    0D                                                               
EXTRA    NMOD1 0,**401X**,RA                                                    
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
         USING GETTXTD,GETTXTCB                                                 
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     PROCBY              PROCBUY                                      
         B     PROCGL              PROCGOAL                                     
         B     PROCSL              PROCSLK                                      
         B     MKTNM                                                            
         B     AFFIDHK             SET SBAFHOOK                                 
         B     PROCPW              PROCPW (STA)                                 
         B     PROCPW2             PROCPW (MKT)                                 
*                                                                               
         EJECT                                                                  
PROCBY   CLI   OFFLINE,C'Y'        ++TEST                                       
         BNE   BYX                                                              
         LA    R1,SBLOCK           SET BFORM MONTH                              
         AHI   R1,MONTAB-SBLOCK                                                 
         MVC   0(4,R1),AMONTHS                                                  
         OI    SBINDS,SBIWRI       INDICATE THAT WRITER IS CALLING APP          
         L     R3,SBAIO1           A(BUY RECORD)                                
         USING BUYREC,R3                                                        
*                                                                               
         CLC   =C'BUYL=',WRINAM    BUY LINE FILTER?                             
         BNE   BY001               NO                                           
         CLC   =C'MO TEST',WRIDESC WRITER DESCRIPTION "MO TEST"?                
         BNE   BY001               NO                                           
         PACK  DUB,WRINAM+5(3)     YES - BUYLINE NUM IN WRINAM+5                
         CVB   RE,DUB              CONVERT TO BINARY                            
         STCM  RE,3,HALF           BUYLINE NUMBER IN HALF                       
         TM    BUYRCNTL,BUYRLN2    2-BYTE BUYLINE?                              
         BZ    BY000               NO                                           
         CLC   HALF,10(R3)         MATCH ON BUYLINE?                            
         B     *+10                GO TEST CC                                   
BY000    CLC   10(1,R3),HALF+1     MATCH ON BUYLINE?                            
         BNE   BYX                 NO - EXIT                                    
*                                                                               
BY001    TM    DATAIND9,DINSIZE    EXTRACT NETWORK SIZE?                        
         BZ    BY088                                                            
         GOTO1 AGETNSIZ                                                         
         CLI   SBQNSIZE,C' '       NETSIZE FILTER(S)?                           
         BNH   BY088                                                            
         MVC   BYTE,SBQNSIZE                                                    
         OI    BYTE,X'40'          LOWERCASE MEANS EXCLUDING                    
         CLC   BYTE,SBSIZE                                                      
         BE    BY087M                                                           
         LA    RE,SBQNSIZX         RE=A(EXTRA NETSIZE FILTERS)                  
         LA    R0,L'SBQNSIZX                                                    
BY087    CLI   0(RE),0             TEST END OF LIST                             
         BE    BY087N                                                           
         CLC   SBSIZE,0(RE)                                                     
         BE    BY087M                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,BY087                                                         
BY087N   TM    SBQNSIZE,X'40'      FILTER NOT MATCH                             
         BZ    BY088               OK WHEN EXCLUDING                            
         B     BYX                                                              
BY087M   TM    SBQNSIZE,X'40'      FILTER MATCH                                 
         BNZ   BY088               OK WHEN INCLUDING                            
         B     BYX                                                              
*                                                                               
* TEMP CODE FOR IDR FILTER                                                      
BY088    DS    0H                                                               
         TM    OPTIND4,OPTIDR                                                   
         BZ    BY090                                                            
         LA    RE,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'70'                                                      
         BE    *+16                                                             
         CLI   0(RE),0                                                          
         BNE   *-18                                                             
         B     BY090                                                            
*                                                                               
         CLC   9(6,RE),BLANKS                                                   
         BH    BYX                                                              
*                                                                               
BY090    TM    DATAIND8,DIRSPDT    INFO RESPONSE AUDIT?                         
         BZ    *+12                 NO                                          
         BAS   RE,RSPDT                                                         
         B     BYX                                                              
*&&DO                                                                           
         CLI   SBEIBYCK,0          ANY INFO BUYER CHECKING FILTER?              
         BE    BY100                NO                                          
         LA    RE,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'72'                                                      
         BE    *+16                                                             
         CLI   0(RE),0                                                          
         BNE   *-18                                                             
         B     BYX                                                              
*                                                                               
         USING INFOELEM,RE                                                      
         CLC   SBEIBYCK,INFOCHEK                                                
         BNE   BYX                                                              
         DROP  RE                                                               
*&&                                                                             
BY100    LA    R1,SPOTHOOK         SET A(SPOTBUY SPOT HOOK)                     
         ST    R1,SBSPHOOK                                                      
         XC    AXSPILL,AXSPILL                                                  
         XC    AXSELEM,AXSELEM                                                  
         MVI   MKTIND,FF                                                        
         MVC   SBEMKT,SBBMKT       EXTRACT MARKET IN DIRECTORY POINTER          
         TM    OPTIND4,OPTXFILE    DDB CROSS FILE REPORT?                       
         BNZ   BY120                YES - SKIP SPILL CHECKS                     
         CLI   SBQSPILL,C'N'       TEST SPILL REQUESTED                         
         BNE   BY110                                                            
         CLC   SBBMKT,BUYMSTA      NO - REJECT SPILL MARKET                     
         BNE   BY700                                                            
         B     BY120                                                            
*                                  YES -                                        
BY110    MVC   SBBMKT,BUYMSTA      SET MARKET FOR SPOTBUY                       
         CLI   SBQSPILL,C'C'       TEST COMBINED ORIG + SPILL                   
         BE    BY120                                                            
         CLI   SBQSPILL,C'S'       TEST SEPERATE ORIG + SPILL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MKTIND,C'O'         YES -                                        
         CLC   SBEMKT,SBBMKT       TEST SPILL OR ORIG MARKET                    
         BE    BY120                                                            
         MVI   MKTIND,C'S'                                                      
*                                                                               
BY120    ICM   R1,15,SBAXSPIL      TEST CANADIAN SPILL REQUEST                  
         BZ    BY150                                                            
         CLC   SBBMKT,SBEMKT       YES-REJECT US SPILL                          
         BNE   BY700                                                            
         USING XSPILLD,R1                                                       
         LA    RF,XSNSTA           FIND STATION IN SPILL TABLE                  
*                                                                               
BY130    OC    0(XSPILLL,R1),0(R1)                                              
         BZ    BY700                                                            
         CLC   BUYMSTA,XSBMSTA                                                  
         BE    *+16                                                             
         LA    R1,XSPILLL(R1)                                                   
         BCT   RF,BY130                                                         
         B     BY700                                                            
         LA    RE,XSMKTS           LOOP THROUGH SPILL MARKETS                   
         LA    RF,XSNMKTS                                                       
*                                                                               
BY140    STM   RE,RF,AXSPILL                                                    
         OC    0(2,RE),0(RE)                                                    
         BZ    BY700                                                            
         MVC   SBEMKT,0(RE)        SET SPILL MARKET                             
         MVC   SBMKTNM,4(RE)                                                    
         DROP  R1                                                               
         LA    R1,2(RE)            ADD SPILL ELEMENT TO BUY                     
         GOTO1 ASPLELEM                                                         
*                                                                               
BY150    BAS   RE,GETDPTTB         GET DAYPART TABLE                            
         MVI   SVDEMFAC,0                                                       
         MVC   SBDPTCD,BDDAYPT     SET DAYPART CODE                             
         TM    DATAIND3,DIPDEMN    TEST FOR NON-ADJUSTED PURCH DEMOS            
         BZ    BY160                                                            
         GOTO1 AGETDF              YES-GET DEMO ADJUSTMENT FACTOR               
*                                                                               
BY160    TM    DATAIND,DIDPT       TEST DAYPART                                 
         BZ    BY170                                                            
         GOTO1 SETDPT,BDDAYPT      SET DAYPART DETAILS                          
*                                                                               
BY170    CLC   SBQPGRF,BLANKS      TEST PRDGRP FILTERING                        
         BNH   *+10                                                             
         MVC   SBEPRD,SBBPRD       YES-ONLY EXTRACT KEYED PRODUCT               
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BNE   BY175               NO                                           
         CLI   SBQBPRD,0           SINGLE PRODUCT REQUEST?                      
         BE    BY175               NO                                           
         CLI   SBQBPRD,X'FF'       POL PRODUCT REQUEST?                         
         BE    BY175               YES                                          
         MVC   SBEPRD,SBQBPRD                                                   
*                                                                               
BY175    TM    DATAIND6,DIDEMU     UPGRADES NOW INCLUDED                        
         BNZ   *+12                                                             
         TM    DATAIND,DIDEMOS     TEST ANY BUY DEMO LOOK-UP                    
         BZ    BY190               NO                                           
         CLI   DEMGRP,C'P'         TEST PRODUCT GROUP DEMO GROUPING             
         BNE   BY180                                                            
         GOTO1 ASETDEMO            YES-SET THE DEMO LIST NOW                    
*                                                                               
BY180    TM    DATAIND,DIDEMR+DIDEMA   TEST BUY DEMO RERATE OR AFFID            
         BZ    BY190                                                            
         CLI   SBQSVI,0            YES-TEST SVI ADJUST SET YET                  
         BNE   BY190                                                            
         MVI   SBESVI,0                                                         
         LLC   R1,BUYKPRD                                                       
         BAS   RE,CHKEST           NO-PICK UP FROM EST BUFFER                   
         BNE   BY190                                                            
         USING ESTBUFFD,R1                                                      
         LLC   RE,EBSVI                                                         
         SRL   RE,4                                                             
         STC   RE,SBESVI                                                        
         DROP  R1                                                               
*                                                                               
BY190    TM    DTAIND11,DIDEMV     WANT UNIVERSE DATA?                          
         BZ    BY191               NO                                           
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   BY191               YES                                          
         OI    SBEFLAG9,SBE9GTUV   FLAG TO GET UNIVERSE FOR DEMO LIST           
         GOTO1 SPOTBUY,PARAS,SBLOCK                                             
         NI    SBEFLAG9,X'FF'-SBE9GTUV                                          
*                                                                               
BY191    XC    PRDLST,PRDLST                                                    
         MVI   FLIGHTSW,C'N'                                                    
         TM    DATAIND7,DIFLIGHT   TEST FOR CHILD SPOT FLIGHTS                  
         BZ    BY220                                                            
         CLI   SBQBPRD,0           TEST SINGLE PRODUCT REQUEST                  
         BE    *+18                                                             
         OC    SBNDATES,SBNDATES   YES-IF NO DATES, EXIT                        
         BZ    BY700                                                            
         B     BY220               DATES ALREADY SET                            
         MVC   PRDLST(1),SBEPRD    BUILD LIST OF PRODUCTS                       
         CLI   SBEPRD,0            TEST SINGLE PRODUCT FILTER                   
         BNE   BY200               YES                                          
         CLI   BUYKPRD,X'FF'       NO-TEST PRODUCT POL BUY                      
         BE    *+14                                                             
         MVC   PRDLST(1),BUYKPRD   NO                                           
         B     BY200                                                            
         MVI   FLIGHTSW,C'Y'       YES-CALL SPOTBUY TO GET LIST OF              
         LA    R1,SBBQSTP              PRODUCTS                                 
         ST    R1,SBADATE                                                       
         MVC   SBNDATES,=F'1'                                                   
         GOTO1 SPOTBUY,PARAS,SBLOCK                                             
*                                                                               
         LR    RE,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   RE,4096                                                          
         USING SYSD+4096,RE                                                     
         CLC   SBACHNKX,=X'FFFFFFFF'                                            
         BNE   *+6                                                              
         DCHO                      CHUNK OVERFLOW                               
         DROP  RE                                                               
*                                                                               
         MVI   FLIGHTSW,C'N'                                                    
*                                                                               
BY200    LA    R1,PRDLST           CALL SPOTBUY FOR EACH PRODUCT                
*                                                                               
BY210    ST    R1,APRD                                                          
         CLI   0(R1),0             TEST ANY MORE PRODUCTS                       
         BE    BY700                                                            
         LLC   RE,0(R1)            YES-                                         
         STC   RE,SBEPRD           SET PRODUCT FILTER                           
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL         GET ALPHA PRODUCT                            
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH                                                     
         GOTO1 GETCSFLT            GET FLIGHT DATES                             
         ICM   R1,15,SBADATE                                                    
         BZ    BY680                                                            
         L     RE,AWEEKS                                                        
         ICM   RF,15,SBNDATES                                                   
         BZ    BY680                                                            
         SLL   RF,2                                                             
         BCTR  RF,0                MOVE DATES TO WEEKS AREA                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)                                                    
         ST    RE,SBADATE                                                       
         LA    RE,1(RF,RE)                                                      
         XC    0(4,RE),0(RE)       MARK END OF DATE LIST                        
*                                                                               
BY220    TM    SBEFLAG8,SBE8CP1+SBE8CP2 COMSCORE PASS 1/2?                      
         BZ    BY220A              NO                                           
         OI    SBEFLAG8,SBE8CPX    SET GETEST TO ONLY GET ECSSDTE               
         MVC   SVEST,BUYKEST       SET THE ESTIMATE CODE                        
         GOTO1 AGETEST,=C'POL'     GET ECSSDTE FROM ESTIMATE REC                
         NI    SBEFLAG8,X'FF'-SBE8CPX  TURN OFF SBE8CPX FLAG                    
*                                                                               
         OC    SBPDEMOS,SBPDEMOS   DEMO MENU OR DEMO OPTION SET?                
         BNZ   BY220A              YES - DO NOT OVERRIDE WITH ENONTDMS          
         OI    SBEFLAG8,SBE8NTD    SET GETEST TO ONLY GET ENONTDMS              
         MVC   SVEST,BUYKEST       SET THE ESTIMATE CODE                        
         GOTO1 AGETEST,SBPRD       GET ECSSDTE FROM ESTIMATE REC                
         NI    SBEFLAG8,X'FF'-SBE8NTD  TURN OFF SBE8NTD FLAG                    
*                                                                               
BY220A   XC    DEMTYPES,DEMTYPES   DETERMINE DEMO TYPES NEEDED                  
         LA    R2,DEMTYPES                                                      
         TM    DATAIND,DIDEMA                                                   
         BZ    *+12                                                             
         MVI   0(R2),C'A'                                                       
         LA    R2,1(R2)                                                         
         TM    DATAIND,DIDEMR                                                   
         BZ    *+12                                                             
         MVI   0(R2),C'R'                                                       
         LA    R2,1(R2)                                                         
         TM    DATAIND,DIDEMP                                                   
         BZ    *+12                                                             
         MVI   0(R2),C'P'                                                       
         LA    R2,1(R2)                                                         
         TM    DATAIND6,DIDEMU                                                  
         BZ    *+12                                                             
         MVI   0(R2),C'U'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         TM    DTAIND10,DISQDA     SQAD AVERAGE CPP'S                           
         BNO   *+12                                                             
         MVI   0(R2),C'1'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         TM    DTAIND10,DISQDH     SQAD HIGH    CPP'S                           
         BNO   *+12                                                             
         MVI   0(R2),C'2'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         TM    DTAIND10,DISQDL     SQAD LOW     CPP'S                           
         BNO   *+12                                                             
         MVI   0(R2),C'3'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         TM    DTAIND12,DISQDN     SQAD CPM TSA LOW?                            
         BNO   *+12                NO                                           
         MVI   0(R2),C'4'          YES - SET TO 4 (WILL MAP TO "N")             
         LA    R2,1(R2)            BUMP R2                                      
*                                                                               
         TM    DTAIND12,DISQDC     SQAD CPM TSA AVERAGE?                        
         BNO   *+12                NO                                           
         MVI   0(R2),C'5'          YES - SET TO 5 (WILL MAP TO "C")             
         LA    R2,1(R2)            BUMP R2                                      
*                                                                               
         TM    DTAIND12,DISQDJ     SQAD CPM TSA HIGH?                           
         BNO   *+12                NO                                           
         MVI   0(R2),C'6'          YES - SET TO 6 (WILL MAP TO "J")             
         LA    R2,1(R2)            BUMP R2                                      
*                                                                               
         TM    DTAIND12,DISQDM     SQAD CPM DMA LOW?                            
         BNO   *+12                NO                                           
         MVI   0(R2),C'7'          YES - SET TO 7 (WILL MAP TO "M")             
         LA    R2,1(R2)            BUMP R2                                      
*                                                                               
         TM    DTAIND12,DISQDB     SQAD CPM DMA AVERAGE?                        
         BNO   *+12                NO                                           
         MVI   0(R2),C'8'          YES - SET TO 8 (WILL MAP TO "B")             
         LA    R2,1(R2)            BUMP R2                                      
*                                                                               
         TM    DTAIND12,DISQDI     SQAD CPM DMA HIGH?                           
         BNO   *+12                NO                                           
         MVI   0(R2),C'9'          YES - SET TO 9 (WILL MAP TO "I")             
         LA    R2,1(R2)            BUMP R2                                      
*                                                                               
         LA    R2,DEMTYPES                                                      
         MVI   FIRST,C'Y'                                                       
         NI    ININD,255-INIBON    NOT PROCESSING BONUS DEMOS                   
         B     BY240                                                            
*                                                                               
BY230    CLI   0(R2),0             TEST END OF DEMO TYPES                       
         BE    BY670                                                            
         MVI   FIRST,C'N'                                                       
*                                                                               
BY240    MVC   SBEDEMTY,0(R2)      SET DEMO TYPE                                
         MVI   CURBOOK,0           SET BOOK IND TO FIRST BOOK                   
         XC    BOOKS,BOOKS                                                      
         CLI   SBEDEMTY,C'R'       TEST RERATE OR AFFID                         
         BE    BY245                                                            
         CLI   SBEDEMTY,C'A'                                                    
         BE    BY245                                                            
*                                                                               
         B     BY260                                                            
*                                                                               
BY245    DS    0H                                                               
*                                                                               
         LR    R1,R9                                                            
         AHI   R1,SBQBOOK-SYSD                                                  
         MVC   BOOKS,0(R1)         YES-SET LIST OF RERATE BOOKS                 
         LA    R1,BOOKS            SET A(CURRENT RERATE BOOK)                   
         ST    R1,ABOOK                                                         
*                                                                               
BY250    L     R1,ABOOK            TEST END OF RERATE BOOKS                     
         OC    0(4,R1),0(R1)                                                    
         BZ    BY660               YES-GOTO NEXT DEMO TYPE                      
         LR    RF,R9                                                            
         AHI   RF,SBQBOOK-SYSD                                                  
         MVC   0(4,RF),0(R1)       NO-SET THE BOOK                              
*                                                                               
BY260    MVI   SBBPRD,0            INITIALIZE PRODUCTS                          
         MVI   SBBPRD2,0                                                        
         XC    SBPRD,SBPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVI   SVPRD,0             INIT SAVED PRODUCT                           
         MVI   SVEST,0             INIT SAVED ESTIMATE                          
         XC    SVNAME,SVNAME                                                    
*                                                                               
         MVC   SBBMKT,BUYMSTA      SET MARKET FOR SPOTBUY                       
*                                                                               
         LA    R1,SBLOCK           SET SPLIT BUYS FLAG                          
         AHI   R1,SBEFLAG2-SBLOCK                                               
         OI    0(R1),SBESPLBY      SPLIT BUYS                                   
*                                                                               
BY262    TM    DATAIND8,DI2NDCOS   EXTRACT SECOND COST?                         
         BZ    BY290                NO                                          
         OI    SBEFLAG,SBE2COS     SET SPOTBUY CONTROL                          
         TM    DTAIND11,DIPGSTC2   HAVE COST2 GST/PST                           
         BZ    *+8                 NO                                           
         OI   SBEFLAG9,SBE9GPC2    YES - FLAG HAVE COST2 GST/PST                
*                                                                               
         LA    RF,SBLOCK                                                        
         AHI   RF,SBACONT-SBLOCK   SAVE OFF ANY CONTINUATION                    
         MVC   FULL,0(RF)                                                       
         GOTO1 SPOTBUY,PARAS,SBLOCK  CALL SPOTBUY FOR SECOND COST DATA          
         LA    RF,SBLOCK                                                        
         AHI   RF,SBACONT-SBLOCK   AND RESTORE IT AFTER COS2 CALL               
         MVC   0(4,RF),FULL                                                     
*                                                                               
         LR    RE,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   RE,4096                                                          
         USING SYSD+4096,RE                                                     
         CLC   SBACHNKX,=X'FFFFFFFF'                                            
         BNE   *+6                                                              
         DCHO                      CHUNK OVERFLOW                               
         DROP  RE                                                               
*                                                                               
         L     R5,SBACHUNK                                                      
         USING SCHUNKD,R5                                                       
BY270    ICM   RE,15,SCNEXT                                                     
         BZ    BY280               END OF CHUNKS                                
*                                                                               
* IF WE'RE ASKING FOR COS2 DOLLARS AND USING CHILD SPOT KEYWORDS,               
* WE REALLY MEAN FOR THE TRADEG KEYWORD TO REPORT THE TRADE                     
* DOLLARS AT GROSS VALUE                                                        
*                                                                               
         TM    DATAIND6,DICHILD    CHILD SPOT DATA                              
         BZ    *+14                                                             
         MVC   SCGSTI,SCCSNTP      SAVE NTP (TRADE) DATA IN GST AREA            
         B     BY275                                                            
         MVC   SCGROSS2,SCGROSS    SAVE DATA                                    
         MVC   SCNET2,SCNET                                                     
         MVC   SCGSTOC2,SCGSTO     SAVE COST2 GST                               
         MVC   SCPSTOC2,SCPSTO     SAVE COST2 PST                               
*                                                                               
BY275    LR    R5,RE                                                            
         B     BY270                                                            
         DROP  R5                                                               
BY280    NI    SBEFLAG,X'FF'-SBE2COS  RESET SECOND COST FLAG                    
         OI    SBEFLAG,SBEA2COS                                                 
*                                                                               
BY290    TM    SBEFLAG6,SBE6PXBY   GET PROGRAM EXCHANGE NET VALUE?              
         BZ    BY295               NO                                           
         CLI   SBBYTYPE,C'T'       TRADE BUY?                                   
         BNE   BY295               NO                                           
         TM    BDCIND2,BDCCOMOQ    C-RATE BUY?                                  
         BZ    BY295               NO                                           
         NI    BDCIND2,X'FF'-BDCCOMOQ TURN OFF C-RATE                           
         MVC   HALF,BDNTAX         SAVE OFF TAX                                 
         XC    BDNTAX,BDNTAX       TRADE BUYS SHOULD NOT TAX                    
*                                                                               
         LA    R4,SBLOCK           SPOTBLOCK                                    
         AHI   R4,SBACONT-SBLOCK   A(CONTINUATION)                              
         MVC   FULL,0(R4)          SAVE OFF ANY CONTINUATION                    
         GOTO1 SPOTBUY,PARAS,SBLOCK CALL SPOTBUY FOR NET PGM XCHANGE            
         MVC   0(4,R4),FULL        RESTORE CONTINUATION                         
*                                                                               
         LR    RE,R9               SYSD                                         
         AHI   RE,4096             SYSD+4096                                    
         USING SYSD+4096,RE        GET ADDRESSABILITY TO END OF SBLOCK          
         CLC   SBACHNKX,=X'FFFFFFFF' CHUNK OVERFLOW?                            
         BNE   *+6                 NO                                           
         DC    H'0'                YES                                          
         DROP  RE                  DROP USING                                   
*                                                                               
         L     R5,SBACHUNK         A(CHUNK)                                     
         USING SCHUNKD,R5          CHUNK USING                                  
BY291    MVC   SCNETPXG,SCNET      NET PROGRAM EXCHANGE                         
         ICM   R5,15,SCNEXT        HAVE ANOTHER CHUNK ENTRY?                    
         BNZ   BY291               YES                                          
         DROP  R5                  DROP CHUNK USING                             
*                                                                               
         OI    BDCIND2,BDCCOMOQ    RESTORE C-RATE BUY                           
         MVC   BDNTAX,HALF         RESTORE TAX                                  
         OI    SBEFLAG7,SBE7APXG   INDICATOR TO SAVE SCNETPXG                   
***                                                                             
* YES, I KNOW SPOTBUY IS GOING TO CLEAR SCNEXT, BUT IT SHOULD                   
* BUILD THE EXACT SAME TABLE - JUST MAKE SURE IT DOESN'T TRASH                  
* SCGROSS2, SCNET2 AND POSSIBLY SCNETPXG                                        
***                                                                             
BY295    LA    RF,SBLOCK           ESTABLISH SPOT BLOCK                         
         USING SBLOCK,RF                                                        
         MVC   SBSQDOPT,CURBOOK    PASS CURRENT SQAD OPTION                     
         DROP  RF                                                               
*                                                                               
         GOTO1 SPOTBUY,PARAS,SBLOCK   ** CALL SPOTBUY **                        
         NI    SBEFLAG,X'FF'-SBEA2COS  RESET AFTER SECOND COST FLAG             
         NI    SBEFLAG7,X'FF'-SBE7APXG RESET AFTER PROGRAM XCHG FLAG            
*                                                                               
         LR    RE,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   RE,4096                                                          
         USING SYSD+4096,RE                                                     
         CLC   SBACHNKX,=X'FFFFFFFF'                                            
         BNE   *+6                                                              
         DCHO                      CHUNK OVERFLOW                               
         DROP  RE                                                               
*                                                                               
         L     R5,SBACHUNK                                                      
         USING SCHUNKD,R5                                                       
*                                                                               
         MVI   SPILL,C'N'                                                       
         NI    ININD,X'FF'-INISPILL                                             
         TM    OPTIND4,OPTXFILE    DDB CROSS FILE REPORT?                       
         BNZ   BY300                YES - SKIP SPILL CHECKS                     
         CLC   SBEMKT,SBBMKT       TEST SPILL MARKET                            
         BE    BY300                                                            
         MVI   SPILL,C'Y'          YES-                                         
         OI    ININD,INISPILL                                                   
         MVC   SBBMKT,SBEMKT       RESTORE MARKET FOR DRIVER                    
         OC    SCNEXT,SCNEXT       TEST ALL SPOTS EXCLUDED                      
         BZ    BY670               YES-GET OUT NOW                              
         OC    SBAXSPIL,SBAXSPIL   TEST CANADIAN SPILL                          
         BZ    BY300                                                            
         GOTO1 AMKTNM                                                           
*                                                                               
BY300    MVI   CHUNKFLG,0          CLEAR CHUNK FLAG                             
         OC    SCNEXT,SCNEXT       TEST END OF CHUNKS                           
         BZ    BY640                                                            
*                                                                               
         LA    R1,1(R9)            GET ADDRESSABILITY TO END OF SBLOCK          
         LA    R1,4095(R1)                                                      
         USING SYSD+4096,R1                                                     
         TM    SBEFLAG2,SBESRC     SRC FILTER?                                  
         BNZ   BY302                YES - ZERO CHUNK OK                         
         DROP  R1                                                               
*                                                                               
         L     RE,SCNEXT           TEST DATA PART OF CHUNK IS ZERO              
         SR    RE,R5                                                            
         SHI   RE,(SCDATA-SCHUNKD)                                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    SCDATA(0),SCDATA                                                 
         BZ    BY630               YES-SKIP TO NEXT CHUNK                       
*                                                                               
* IF FILTERING IN AFFID TIMES, SKIP CHUNK IF OUTSIDE RANGE                      
BY302    DS    0H                                                               
         OC    ATIMRNG,ATIMRNG     ANY AFFID TIME FILTER?                       
         BZ    BY310                NO                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
         CLC   SCATIME,=X'FFFF'    TIME UNKNOWN?                                
         BE    BY630                YES - OUTSIDE RANGE - NEXT CHUNK            
*                                                                               
         ICM   RF,3,SCATIME                                                     
         CHI   RF,0600             L.T. 6AM?                                    
         BNL   *+8                                                              
         AHI   RF,2400                                                          
*                                                                               
         ICM   RE,3,ATIMRNG        FILT RANGE START TIME                        
         CHI   RE,0600             L.T. 6AM?                                    
         BNL   *+8                                                              
         AHI   RE,2400                                                          
         CR    RF,RE                                                            
         BL    BY630               OUTSIDE OF RANGE - NEXT CHUNK                
         ICM   RE,3,ATIMRNG+2      FILT RANGE END TIME                          
         CHI   RE,0600             L.T. 6AM?                                    
         BNL   *+8                                                              
         AHI   RE,2400                                                          
         CR    RF,RE                                                            
         BH    BY630               OUTSIDE OF RANGE - NEXT CHUNK                
*                                                                               
BY310    DS    0H                                                               
         CLI   SBQBPRD,0           TEST PRD=ALL                                 
         BNE   BY320                                                            
         TM    SBQPIND,SBQPOLAL    YES - TEST INCLUDE POL                       
         BO    BY340                                                            
         CLI   SCPRD1,X'FF'              NO - REJECT PRD=POL                    
         BE    BY630                                                            
         B     BY340                                                            
*                                                                               
BY320    CLI   SCPRD1,254          TEST UNALLOCATED                             
         BNE   BY330                                                            
         TM    SBQPIND,SBQPUNA     YES-IF UNAL ISN'T ONLY PRD REQUESTED         
         BO    BY340                                                            
         CLI   SBQBPRD,X'FF'       AND PRD=POL,                                 
         BNE   BY330                                                            
         MVI   SCPRD1,X'FF'        INCLUDE IT IN POL                            
*                                                                               
BY330    CLC   SCPRD1,SBQBPRD      CHECK THE PRODUCT                            
         BE    BY340                                                            
         CLI   SCPRD2,0            TEST SECOND PRODUCT                          
         BE    BY630                                                            
         CLC   SCPRD2,SBQBPRD      YES-THEN PRODUCT CAN MATCH EITHER            
         BNE   BY630                   ONE                                      
*                                                                               
BY340    CLI   SCPRD2,0            TEST SECOND PRODUCT                          
         BE    BY390                                                            
         CLI   SBEPRD,0            YES-TEST SINGLE PRODUCT REQUESTED            
         BE    BY350                   THIS TIME                                
         CLI   SBEPRD,X'FF'                                                     
         BE    BY350                                                            
         CLC   SCPRD1,SBEPRD       YES-REQUESTED PRD COMES FIRST                
         BNE   BY380                                                            
         B     BY390                                                            
*                                                                               
BY350    LA    R0,2                GET THE PRODUCTS' ALPHA CODES                
         LA    R1,WORK                                                          
         LA    RF,SCPRD1                                                        
*                                                                               
BY360    MVC   0(3,R1),=X'FEFEFE'                                               
         CLI   0(RF),254                                                        
         BE    BY370                                                            
         LLC   RE,0(RF)                                                         
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     RF,SBAPRDBF                                                      
         AR    RF,RE                                                            
         MVC   0(3,R1),PBALPH-PRDBUFFD(RF)                                      
*                                                                               
BY370    LA    R1,3(R1)                                                         
         LA    RF,SCPRD2                                                        
         BCT   R0,BY360                                                         
*                                                                               
         TM    OPTIND4,OPTPRDBY    REPORT PIGS AS ENTERED?                      
         BNZ   BY390                YES                                         
         CLC   WORK(3),WORK+3      TEST 1ST PRD GREATER THAN 2ND PRD            
         BNH   BY390               NO                                           
*                                                                               
BY380    MVC   WORK(L'SCKEY),SCKEY   SWITCH THE PRODUCTS                        
         MVC   SCPRD1,SCPRD2                                                    
         MVC   SCSLN1,SCSLN2                                                    
         MVC   SCPRD2,WORK+SCPRD1-SCKEY                                         
         MVC   SCSLN2,WORK+SCSLN1-SCKEY                                         
         CLI   SBEBYCML,C'Y'                                                    
         BNE   BY390                                                            
         MVC   SCML1,SCML2                                                      
         MVC   SCML2,WORK+SCML1-SCKEY                                           
*                                                                               
BY390    CLC   SBBPRD,SCPRD1       TEST CHANGE OF PRODUCT                       
         BE    BY410                                                            
         MVC   SBBPRD,SCPRD1       YES - SET PRODUCT CODE FOR DRIVER            
         CLI   SCPRD1,254          TEST UNALLOCATED                             
         BNE   BY400                                                            
         MVC   SBPRD,=X'FEFEFE'                                                 
         MVC   SBBPGR,=X'9999'                                                  
         MVC   SBPRDNM,BLANKS                                                   
         MVC   SBPRDNM(11),=C'UNALLOCATED'                                      
         LR    R1,R9                                                            
         AHI   R1,(SBUP1FLD-SYSD)                                               
         MVC   0(L'SBUP1FLD+L'SBUP2FLD,R1),BLANKS                               
         OI    SBEUDEF,SBEUPCOM    EXTRACT UCOM DATA                            
         BRAS  RE,GETUCOM                                                       
         B     BY410                                                            
*                                                                               
BY400    BRAS  RE,EXTPRD           EXTRACT PRODUCT DETAILS                      
         BNE   BY630                                                            
         CLI   SBQMGR,0            TEST MARKET GROUPS                           
         BE    BY410                                                            
         CLC   SBQPGRF,BLANKS      AND NOT MARKET GROUP FILTERING               
         BH    BY410                                                            
         CLI   BUYKPRD,X'FF'       AND PRD=POL                                  
         BNE   BY410                                                            
         OC    SBPGRPEX(2),SBPGRPEX  AND THERE ARE PRDGRP EXCEPTIONS            
         BZ    BY410                                                            
         GOTO1 AGETMGR             YES-GET THE MARKET GROUP                     
*                                                                               
BY410    CLC   SBBPRD2,SCPRD2      TEST CHANGE OF SECOND PRD                    
         BE    BY420                                                            
         MVC   SBBPRD2,SCPRD2                                                   
         XC    SBPRD2,SBPRD2                                                    
         CLI   SBBPRD2,0                                                        
         BE    BY420                                                            
         LLC   RE,SBBPRD2                                                       
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD2,PBALPH                                                    
         DROP  R1                                                               
*                                                                               
BY420    CLC   SBBPRD,SVPRD        TEST CHANGE OF PRODUCT/ESTIMATE              
         BNE   BY422                                                            
*        BNE   *+18                                                             
         CLC   BUYKEST,SVEST                                                    
         BE    BY500                                                            
         CLC   SBQEST,SBQESTND     EST HAS CHANGED AND NOT 1 EST REQ            
         BE    BY425                 (IT IS)                                    
         CLI   SBQSEPES,C'Y'       AND ESTS ARE BEING LUMPED TOGETHER           
         BNE   BY425               THEN DON'T SET CHANGED EST FLAG              
         LA    R1,1(R9)            GET ADDRESSABILITY TO END OF SBLOCK          
         LA    R1,4095(R1)                                                      
         USING SYSD+4096,R1                                                     
         OI    SBBUYCH,SBBCEST     SET EST CHANGED                              
         DROP  R1                                                               
         B     BY425                                                            
*                                                                               
BY422    LA    R1,1(R9)            GET ADDRESSABILITY TO END OF SBLOCK          
         LA    R1,4095(R1)                                                      
         USING SYSD+4096,R1                                                     
         OI    SBBUYCH,SBBCPRD     SET PRODUCT CHANGED                          
         DROP  R1                                                               
*                                                                               
BY425    MVC   SVPRD,SBBPRD        YES                                          
         MVC   SVEST,BUYKEST                                                    
         OI    SBEUDEF,SBEUECOM                                                 
         BRAS  RE,GETUCOM                                                       
*                                                                               
         CLI   SBQMKTWT,C'D'       TEST PRIMARY DEMO WEIGHTING                  
         BNE   BY430                                                            
         MVC   SBBEST,BUYKEST                                                   
         BAS   RE,MKTWGT           GET MARKET WEIGHT                            
*                                                                               
BY430    TM    DATAIND5,DIMKTRNK   TEST MARKET RANKING                          
         BZ    BY440                                                            
         OC    ABOOK,ABOOK         YES-IF MORE THAN ONE BOOK,                   
         BZ    *+16                                                             
         LA    R1,BOOKS            ONLY NEED TO GET MKT RANK FOR                
         C     R1,ABOOK            FIRST BOOK                                   
         BL    BY440                                                            
         GOTO1 SPOTMKRK,DMCB,SBLOCK   ** GET MARKET RANK                        
*                                                                               
BY440    TM    DATAIND,DIBYDEM     TEST BUY DEMOS                               
         BNZ   BY450                                                            
         TM    DATAIND6,DIDEMU     DIDEMU IS ALSO BUY DEMO                      
         BNZ   BY450                                                            
         CLI   SBEFILT,C'Y'        OR EST FILTER REQUIRED                       
         BE    BY450                                                            
         CLI   SBERTLSC,C'Y'       OR RETAIL SCHEME CODE REQUIRED               
         BE    BY450                                                            
         TM    SBEUDEF,SBEUEST1+SBEUEST2  OR ESTIMATE USER FIELDS               
         BZ    BY500                                                            
*                                                                               
BY450    LA    R1,SBPRD                                                         
         MVC   PROD,SBBPRD                                                      
         LA    R4,1                                                             
         CLI   SBBPRD2,0           TEST 2ND PRODUCT                             
         BE    BY460                                                            
         LA    R4,2                YES-2 TRIES AT FINDING DEMOS                 
*                                                                               
BY460    CLI   DEMOPT,0            TEST TGT/SEC DEMOS                           
         BE    BY470                                                            
         GOTO1 AGETEST             YES-GET ESTIMATE'S DEMOS                     
*                                                                               
BY470    LA    RE,SBEDEMOS                                                      
         OC    SBEDEMOS,SBEDEMOS   TEST DEMO LIST OVERRIDE                      
         BNZ   BY490                                                            
         LLC   R1,PROD             FIND ENTRY IN PRD/EST TABLE                  
         CLI   PROD,X'FE'                                                       
         BNE   *+8                                                              
         LA    R1,255              PRD=POL FOR UNALLOCATED                      
         BAS   RE,CHKEST                                                        
         BE    BY480                                                            
         BCT   R4,BY475            ESTIMATE INVALID-TRY 2ND PRODUCT?            
*                                                                               
         CLI   SBQPGRD,C' '        PRODUCT GROUPS REQUESTED?                    
         BH    *+6                 YES - DON'T DIE!                             
         DC    H'0'                                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      SUBMITTED FOR YOUR APPROVAL                              
*                      ---------------------------                              
* THE RULES FOR PRODUCT GROUP REQUESTS IN AN OVERLAY WHERE NOTHING              
* IS WHAT IT SEEMS...                                                           
*                                                                               
* WHEN WE REQUEST A WRITER WITH A PRODUCT GROUP DEFINITION ONLY                 
* (PGR=V AS OPPOSED TO PRODUCT GROUP FILTER PGR=V101) WE RUN                    
* INTO PROBLEMS IF WE LOOK UP AND ESTIMATE ENTRY FOR A PRD THAT                 
* IS NOT IN ANY OF THE PRODUCT GROUPS. WE ONLY BUFFER THE ESTIMATE              
* RECORDS FOR PRODUCTS THAT ARE IN PRODUCT GROUP V.  IF THE BUY                 
* RECORD HAS A PRODUCT THAT IS NOT IN THAT PRODUCT GROUP AND WE                 
* GET TO THE CODE BELOW - WE DIE.  I NOW LOOK UP THE ESTIMATE RECORD            
* AND BUFFER IT.  THIS HAPPENS BECAUSE WE TREAT A PRODUCT GROUP                 
* DEFINITION REQ AS A FILTER IN SOME CASES (WE FILTER OUT ESTIMATES             
* WHO'S PRODUCT IS NOT IN ANY OF THOSE PRODUCT GROUPS) BUT NOT FOR              
* REPORTING PURPOSES (PRODUCTS THAT ARE NOT IN ANY PRODUCT GROUP ARE            
* REPORTED WITH A "9" AT THE END - FOR INSTANCE "V9" AND ALSO REPORT            
* **UNKNOWN** AS THE PRODUCT GROUP NAME)                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         L     R0,SBAIO3           COPY BUY RECORD TO SBAIO3                    
         LAY   R1,6000             L'I/O AREA                                   
         L     RE,SBAIO1           BUY RECORD IS IN SBAIO1                      
         LR    RF,R1               L'I/O AREA                                   
         MVCL  R0,RE               COPY FROM AIO1 TO AIO3                       
*                                                                               
         LA    R1,SBPRD            POINT TO FIRST PRD FOR AGETEST!              
         OI    SBEFLAG5,SBE5GEST   GET ESTIMATE TO PUT IN TSAR BUFFER           
         GOTO1 AGETEST             READ EST RECS INTO AIO1/AIO2                 
         NI    SBEFLAG5,X'FF'-SBE5GEST  TURN OFF SPECIAL GETEST FLAG            
*                                                                               
         GOTO1 PUTESTNM            TSAR OFF THE EST RECORD IN AIO1/2            
*                                                                               
         L     RF,SBAIO1           EST RECORD                                   
         USING ESTHDRD,RF                                                       
         XC    SBESTFLT,SBESTFLT   CLEAR ESTIMATE FILTERS                       
         CLI   SBEFILT,C'Y'        ESTIMATE FILTERS REQUIRED?                   
         BNE   *+10                NO                                           
         MVC   SBESTFLT,EPROF      YES - EXTRACT ESTIMATE FILTERS               
         XC    SBRTLSCH,SBRTLSCH   CLEAR RETAIL SCHEME CODE                     
         CLI   SBERTLSC,C'Y'       RETAIL SCHEME CODE REQUIRED?                 
         BNE   *+10                NO                                           
         MVC   SBRTLSCH,ERTLSCHM   YES - EXTRACT RETAIL SCHEME CODE             
*                                                                               
         CLI   SBEDEM,C'N'         DEMOS REQUIRED?                              
         BE    BY473               NO - DON'T BOTHER                            
*                                                                               
         ICM   R1,15,SBAESTBF      A(ESTIMATE BUFFER)                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ESTBUFFD,R1                                                      
         LHI   R0,256              UP TO 256 EST ENTRIES                        
*                                                                               
BY471    OC    0(ESTBUFFL,R1),0(R1)    EMPTY SLOT?                              
         BNZ   BY471A                  NO                                       
         MVC   EBDEMOS,SBPDEMOS        DEMO MENU OVERRIDE                       
         OC    EBDEMOS,EBDEMOS         HAVE ANY OVERRIDE?                       
         BNZ   BY472                   YES - USE THOSE                          
         MVC   EBDEMOS,EDEMLST         OTHERWISE DEMOS FROM EST REC             
         B     BY472                                                            
*                                                                               
BY471A   LA    R4,SBPDEMOS             POINT TO DEMO OVERRIDE LIST              
         OC    SBPDEMOS,SBPDEMOS       HAVE ANY DEMO OVERRIDE?                  
         BNZ   *+8                     YES - USE IT                             
         LA    R4,EDEMLST              NO - POINT TO EST DEMO LIST              
         CLC   EBDEMOS,0(R4)           ALREADY HAVE THESE DEMOS?                
         BE    BY472                   YES - USE THOSE!                         
         LA    R1,ESTBUFFL(R1)         NEXT ENTRY                               
         BCT   R0,BY471                                                         
         DC    H'0'                    EST BUFFER NEEDS EXPANSION               
         DROP  R1,RF                                                            
*                                                                               
BY472    LR    R4,R1               SAVE A(EST BUFFER ENTRY)                     
*                                                                               
BY473    L     R0,SBAIO1           COPY BUY RECORD BACK TO SBAIO1               
         LAY   R1,6000             L'I/O AREA                                   
         L     RE,SBAIO3           BUY RECORD IS IN SBAIO3                      
         LR    RF,R1               L'I/O AREA                                   
         MVCL  R0,RE               COPY FROM AIO3 BACK TO AIO1                  
         LR    R1,R4               R1 = A(EST BUFFER ENTRY)                     
         B     BY480A              IT'S AS IF WE ALWAYS HAD THIS EST!           
*                                                                               
BY475    LA    R1,SBPRD2                                                        
         MVC   PROD,SBBPRD2                                                     
         B     BY460                                                            
*                                                                               
         USING ESTBUFFD,R1                                                      
BY480    MVC   SBESTFLT,EBFILT     EXTRACT ESTIMATE FILTERS                     
         MVC   SBRTLSCH,EBRTLSCH   EXTRACT RETAIL SCHEME CODE                   
*                                                                               
BY480A   TM    SBEUDEF,SBEUEST1+SBEUEST2   EST USER FIELDS REQUIRED?            
         BZ    BY481                       NO                                   
         GOTO1 GETESTNM                                                         
*                                                                               
BY481    TM    DATAIND,DIBYDEM     TEST NEED DEMOS                              
         BNZ   BY482                                                            
         TM    DATAIND6,DIDEMU                                                  
         BNZ   BY482                                                            
         TM    DTAIND12,DISQD2     SQAD CPM NEEDED?                             
         BNZ   *+12                YES                                          
         TM    DTAIND10,DISQD      OR SQAD                                      
         BZ    BY500                                                            
BY482    LA    RE,EBDEMOS          YES-EXTRACT A(DEMOS)                         
         DROP  R1                                                               
*                                                                               
BY490    ST    RE,ADEMLST                                                       
         MVC   DEMFAC,SVDEMFAC                                                  
         CLI   DEMFAC,0            TEST DEMO ADJUSTMENT FACTOR=0                
         BE    BY500                                                            
         BAS   RE,GETPDEM          NO-DETERMINE WHETHER PRIMARY PURCH           
*                                     DEMO IS OVERIDDEN                         
BY500    L     R4,AGLOBAL          RESTORE R4                                   
         LLC   RE,SCSLN1           SPOT LENGTH                                  
         LLC   RF,SCSLN2                                                        
         AR    RE,RF                                                            
         STC   RE,SBLEN                                                         
         CLI   SBEBYCML,C'Y'                                                    
         BNE   *+16                                                             
         MVC   SBCMLSQ,SCML1       COMMERCIAL SEQUENCE NUMBER                   
         MVC   SBCMLSQ2,SCML2      COMMERCIAL SEQUENCE NUMBER2                  
*                                                                               
         TM    DATAIND,DICPP       TEST CPP                                     
         BNZ   BY505                                                            
         TM    DTAIND12,DISQD2     SQAD CPM NEEDED?                             
         BNZ   BY505               YES                                          
         TM    DTAIND10,DISQD      OR SQAD                                      
         BZ    BY510                                                            
BY505    NI    ININD,FF-INIEQUIV   YES-TURN OFF EQUIVALENCING                   
         CLI   SBSPPROF+4,C'Y'     UNLESS, EQUIV DETAIL LINES                   
         BE    *+12                                                             
         TM    DATAIND,DIDPTLEN            OR DPT/LEN NOT IN ROWS               
         BNZ   BY510                                                            
         OI    ININD,INIEQUIV                                                   
*                                                                               
BY510    CLI   SBQGETNM,C'Y'       TEST GET BUYER/BILLER NAMES                  
         BNE   BY520               NO                                           
         MVC   DUB(2),SBBCLT       YES-                                         
         MVC   DUB+2(1),SBBPRD                                                  
         MVC   DUB+3(1),BUYKEST                                                 
         MVC   DUB+4(2),SBBMKT                                                  
         CLC   SVNAME,DUB          TEST CLT/PRD/EST/MKT CHANGE                  
         BE    BY520               NO                                           
         MVC   SVNAME,DUB          YES-GET BUYER/BILLER NAMES                   
         GOTO1 AGETNAME                                                         
*                                                                               
BY520    TM    DATAIND6,DIDR       TEST DIRECT RESPONSE                         
         BZ    BY530                                                            
         CLC   SBDPTCD,SCRSVPDP    YES-TEST DAYPART CHANGE                      
         BE    BY530                                                            
         MVC   SBDPTCD,SCRSVPDP    YES-                                         
         TM    DATAIND,DIDPT       TEST DAYPART DETAILS NEEDED                  
         BZ    BY530                                                            
         GOTO1 SETDPT,SBDPTCD      YES-GET THEM                                 
*                                                                               
BY530    TM    DATAIND6,DIAPROG    TEST AFFID PROGRAM NEEDED                    
         BZ    BY560                                                            
         MVC   SBLKPROG,BLANKS                                                  
         OC    SCADATE,SCADATE                                                  
         BZ    BY560                                                            
         LA    R1,AFDAREA          EXTRACT IT FROM AFFID SAVE AREA              
         LA    R0,MAXAFFID                                                      
*                                                                               
BY540    OC    0(L'AFDAREA,R1),0(R1)                                            
         BZ    BY560                                                            
         CLC   SCADATE,0(R1)                                                    
         BNE   BY550                                                            
         CLC   SCATIME,2(R1)                                                    
         BNE   BY550                                                            
         MVC   SBLKPROG,4(R1)                                                   
         OC    SBLKPROG,BLANKS                                                  
         B     BY560                                                            
*                                                                               
BY550    LA    R1,L'AFDAREA(R1)                                                 
         BCT   R0,BY540                                                         
*                                                                               
BY560    CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    BY580                                                            
         OC    SBMKTWGT,SBMKTWGT   AND MARKET WEIGHT = 0                        
         BNZ   BY580                                                            
         TM    SBEFLAG8,SBE8CNWK   NETWORK=NWK OPTION?                          
         BZ    *+14                NO                                           
         OC    BUYKMKT,BUYKMKT     MARKET ZERO?                                 
         BZ    BY580               YES - DON'T CLEAR RATINGS                    
         SR    R0,R0                                                            
         ICM   R0,1,SBENDEM        AND REPORTING DEMOS                          
         BZ    BY580                                                            
         L     R1,ADEMLST          YES-CLEAR OUT ALL RATINGS                    
         LA    RE,SCDEMOS                                                       
*                                                                               
BY570    CLI   2(R1),0             COMSCORE DEMO?                               
         BNE   BY570A              NO                                           
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,1,1(R1)          HAVE COMSCORE INDEX?                         
         BZ    BY570C              NO - CLEAR DEMO                              
         BCTR  RF,0                -1                                           
         MHI   RF,8                INDEX INTO COMDLIST                          
         LA    RF,COMDLIST(RF)     RF = INDEX INTO COMDLIST                     
         CLI   0(RF),C'R'          COMSCORE RATING?                             
         B     BY570B              GO TEST CC                                   
*                                                                               
BY570A   CLI   1(R1),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R1),C'E'                                                       
BY570B   BNE   *+10                                                             
BY570C   XC    0(8,RE),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,8(RE)                                                         
         BCT   R0,BY570                                                         
*                                                                               
BY580    XC    BYGROSS,BYGROSS                                                  
         XC    BYCOST2,BYCOST2                                                  
         XC    BYNCOST2,BYNCOST2                                                
         XC    BYEGROSS,BYEGROSS                                                
         XC    CSDDOL,CSDDOL                                                    
         XC    PWGROSS,PWGROSS                                                  
         XC    PWNET,PWNET                                                      
         MVC   BYSPOTS,SCSPOTS+3   SAVE N'SPOTS                                 
         CLI   SPILL,C'Y'          TEST SPILL                                   
         BE    BY590                                                            
         MVC   BYGROSS,SCGROSS     NO- SAVE DOLLARS FOR CPP INPUT FIELD         
         MVC   BYCOST2,SCGROSS2    SAVE COST2 FOR CPP INPUT FIELD               
         MVC   BYNCOST2,SCNET2     SAVE NET COST2 FOR CPP INPUT FIELD           
         MVC   BYEGROSS,SCEGROSS                                                
         MVC   CSDDOL,SCCSTPT      CHILD SPOT DELIVERED DOLLARS                 
         MVC   PWGROSS,SCPWGRS                                                  
         MVC   PWNET,SCPWNET                                                    
*                                                                               
BY590    MVC   FULL,SCSPOTS                                                     
         CLI   FIRST,C'Y'          TEST FIRST CALL TO SPOTBUY                   
         BE    *+14                                                             
         XC    SCDATA(SCDEMOS-SCDATA),SCDATA NO- CLEAR ALL EXCEPT DEMOS         
         B     BY600                                                            
         CLI   SPILL,C'Y'          YES - TEST SPILL MARKET                      
         BNE   BY600                                                            
         MVC   FULL,SCSPOTS        YES - CLEAR ALL EXCEPT DEMOS                 
         XC    SCDATA(SCDEMOS-SCDATA),SCDATA   AND SPOTS                        
         MVC   SCSPOTS,FULL                                                     
*                                                                               
BY600    ST    R5,SBACURCH         A(CURRENT EXTRACT CHUNK)                     
         MVI   BUYRECSW,C'Y'                                                    
*                                                                               
         XC    ACLRST01,ACLRST01   CLEAR PTR TO X'01' ELEMENT                   
         XC    ACLRST03,ACLRST03   CLEAR PTR TO X'03'/X'05' PAIR                
         OC    SCCLDISK,SCCLDISK   HAVE CLRST D/A?                              
         BZ    BY605               NOPE                                         
         OC    SCCHKNUM,SCCHKNUM   HAVE  A SEQ/CHECK NUMBER                     
         BZ    BY605               NOPE                                         
*                                                                               
         MVC   KEY+14(4),SCCLDISK  CLRST D/A                                    
         MVC   AIO,AIO2            READ CLRST RECORD INTO AIO2                  
         GOTO1 GETREC              GET THE RECORD                               
***                                                                             
* TEST IF WE HAVE SEQ NUMBER OF CHECK NUMBER IN SCCHKNUM                        
***                                                                             
         XC    FULL,FULL                                                        
         CLC   SCCHKNUM+3(3),=X'FFFFFF'  HAVE DATE/SEQ NUMBER?                  
         BNE   BY600A              NOPE                                         
***                                                                             
* POINT TO THE RIGHT X'01' ELEMENT BASED ON THE CLR DATE AND SEQ NUMBER         
***                                                                             
         MVC   FULL(3),SCCHKNUM    DATE/SEQ NUMBER                              
*                                                                               
BY600A   L     RF,AIO2             CLEARANCE STATUS RECORD                      
         LA    RF,24(RF)           POINT TO FIRST ELEMENT                       
*                                                                               
BY601    CLI   0(RF),0             END OF RECORD?                               
         BE    BY605               YES                                          
         CLI   0(RF),X'01'         HAVE X'01' ELEMENT?                          
         BE    BY603               YES - CHECK DATE/SEQ NUMBER                  
*                                                                               
BY602    LLC   R0,1(RF)            BUMP TO NEXT ELEMENT                         
         AR    RF,R0                                                            
         B     BY601                                                            
*                                                                               
         USING CLSTEL01,RF                                                      
BY603    CLI   CLSTPRD,X'FF'       POL?                                         
         BE    *+14                YES - MATCHES ALL BRANDS                     
         CLC   CLSTPRD(2),SCPRD1   MATCH ON PRODUCT?                            
         BNE   BY602               NO                                           
         OC    FULL,FULL           HAVE X'03'/X'05' PAIR?                       
         BNZ   *+14                YES                                          
         CLC   CLSTCHK,SCCHKNUM    NO - MATCH ON CHECK NUMBER                   
         B     *+10                                                             
         CLC   FULL(3),CLSTCLRD    DATE/SEQ NUMBER MATCH?                       
         BNE   BY602               NO, LOOK FOR NEXT X'01' ELEMENT              
*                                                                               
         TM    CLSTSTAT,X'02'      IF NO X'03' ELEMENTS                         
         BO    *+14                                                             
         OC    CLSTCHK,CLSTCHK        SKIP IF NO CHECK NUMBER YET               
         BZ    BY605                                                            
*                                                                               
         STCM  RF,15,ACLRST01      SAVE PTR TO X'01' ELEMENT                    
*                                                                               
*****    MVC   SCPAYTL,SCPAY       SAVE ORIGINAL PAID GROSS                     
         MVC   SCPAYTLN,SCPAYN     SAVE ORIGINAL PAID NET                       
*                                                                               
         OC    FULL,FULL           HAVE X'03'/X'05' PAIR?                       
         BZ    BY605               NO, OLD RECORD WITH NO INV NUMBER            
*                                                                               
         LLC   R1,1(RF)            YES - BUMP TO 1ST X'03'/X'05' PAIR           
         LA    R1,0(R1,RF)                                                      
         DROP  RF                                                               
***********************************************************************         
* GO THROUGH THE X'03'/X'05' ELEMENTS AND IF WE ENCOUNTER A SCENARIO  *         
* WHERE THERE IS A X'03'/X'05' PAIR WITH NO CHECK NUMBER AND THERE    *         
* IS ANOTHER X'03'/X'05' AFTER IT THAT HAS A CHECK NUMBER REORDER     *         
* THE ELEMENTS INTERNALLY FROM THIS...                                *         
*                                                                     *         
* CLRDATE  SEQ PRD/EST --INVOICE-  --AMOUNT-- CHKNUM  CHKDATE         *         
*                                                                     *         
* MAR23/17 001 POL     VI17021510         .00       *                 *         
*                      VI17021544         .00                         *         
*                      VI17021711      601.66 C50396  JUN12/17        *         
*                      VI17021714      940.36 C50396  JUN12/17        *         
*                                                                     *         
* TO THIS...                                                          *         
*                                                                     *         
* CLRDATE  SEQ PRD/EST --INVOICE-  --AMOUNT-- CHKNUM  CHKDATE         *         
*                                                                     *         
* MAR23/17 001 POL     VI17021711      601.66 C50396  JUN12/17        *         
*                      VI17021714      940.36 C50396  JUN12/17        *         
*                      VI17021510         .00       *                 *         
*                      VI17021544         .00                         *         
*                                                                     *         
***********************************************************************         
         BRAS  RE,FIXCLRST         FIX CLRST ELEMENT IF NEEDED                  
*                                                                               
         MVC   CHKSEQ,SCCHKNUM     SAVE OFF SEQUENCE NUMBER                     
         L     RE,SCNEXT           SAVE OFF CHUNK DATA                          
         SR    RE,R5                                                            
         SHI   RE,(SCDATA-SCHUNKD)                                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVCHUNK(0),SCDATA   ** CHUNK DATA SAVED **                       
         STC   RE,CHUNKLEN         CHUNK DATA LENGTH                            
*                                                                               
         BAS   RE,GETCLRST         GET A(X'03') ELEMENT                         
         BNE   BY602                 DROP ELEMENT                               
*                                                                               
* GET ADDRESSABILITY TO END OF SPOT BLOCK TEMPORARILY                           
*                                                                               
BY605    LA    R1,1(R9)                                                         
         LA    R1,4095(R1)                                                      
         USING SYSD+4096,R1                                                     
*                                                                               
         TM    DATAIND9,DICBLAGG   THIS A CABLE AGG REQ?                        
         BZ    BY620                NO                                          
         CLI   SBBUYCH,0           ANY CHANGES?                                 
         BE    BY620                NO                                          
*                                                                               
         TM    ROWIND2,ROWICLT     CLT A ROW?                                   
         BZ    *+12                 NO                                          
         TM    SBBUYCH,SBBCCLT     DID IT CHANGE?                               
         BNZ   BY610                YES                                         
*                                                                               
         TM    ROWIND2,ROWIPRD     PRD A ROW?                                   
         BZ    *+12                 NO                                          
         TM    SBBUYCH,SBBCPRD     DID IT CHANGE?                               
         BNZ   BY610                YES                                         
*                                                                               
         CLI   SBQSEPES,C'N'       EST=NO                                       
         BE    *+20                                                             
         TM    ROWIND2,ROWIEST     EST A ROW?                                   
         BZ    *+12                 NO                                          
         TM    SBBUYCH,SBBCEST     DID IT CHANGE?                               
         BNZ   BY610                YES                                         
*                                                                               
         TM    SBBUYCH,SBBCSTA     DID STA CHANGE?                              
         BZ    BY620                NO                                          
         DROP  R1                                                               
*                                                                               
BY610    GOTO1 PUTCBLCT                                                         
         LA    R1,1(R9)                                                         
         LA    R1,4095(R1)                                                      
         USING SYSD+4096,R1                                                     
         MVI   SBBUYCH,0                                                        
         DROP  R1                                                               
*                                                                               
BY620    LA    RE,SBLOCK           GET ADDRESSABILITY TO END OF SBLOCK          
         TM    SBQREAD2-SBLOCK(RE),SBQRD2OM   READ OM KEYWORDS?                 
         BZ    BY621               NO                                           
         BRAS  RE,GETOMREC         HAVE TSARED OM REC?                          
         BNE   BY621               NO                                           
         MVC   OMREC,SBAIO3        OM REC WE BUFFERED IS IN AIO3                
         GOTO1 =A(PROCOMG)         OM RECORD PASSES?                            
         BE    BY625               YES                                          
*                                                                               
BY621    XC    OMREC,OMREC         OM RECORD DOES NOT PASS                      
*                                                                               
BY625    BRAS  RE,GETBILL          GET BILLING PCT                              
         BRAS  RE,RDBFORM                                                       
*                                                                               
BY625A   TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   BY625B              YES - SKIP DRIVER INPUT CALL                 
         GOTO1 ADRIVIN             CALL DRIVER FOR INPUT                        
*                                                                               
BY625B   OC    OMREC,OMREC         HAVE OM REC?                                 
         BZ    *+8                 NOPE                                         
         BRAS  RE,PUTOMREC         WRITE REC BACK W/ZEROED SPOTS/DOLS           
*                                                                               
         TM    DTAIND11,DIOMKGD    READ ORDER MAKEGOOD RECORDS?                 
         BZ    BY625C              NO                                           
         OC    DMGKEY,DMGKEY       JUST PROCESSED A MAKEGOOD?                   
         BZ    BY625C              NO                                           
*                                                                               
         TM    CHUNKFLG,X'80'      ALREADY SAVED & CLEARED THIS CHUNK?          
         BNZ   BY625B1             YES                                          
         BRAS  RE,GETOMREC         OMREC NOW HAS ZERO SPOT/DOLLARS              
         L     RE,SCNEXT           SAVE OFF CHUNK DATA                          
         SR    RE,R5                                                            
         SHI   RE,(SCDATA-SCHUNKD)                                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVCHUNK(0),SCDATA   ** CHUNK DATA SAVED **                       
         STC   RE,CHUNKLEN         CHUNK DATA LENGTH                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    SCDATA(0),SCDATA    CLEAR CHUNK DATA                             
*                                                                               
BY625B1  GOTO1 =A(NEXTDRMG)        HAVE ANOTHER MG TO PROCESS?                  
         BE    BY625A              YES                                          
*                                                                               
         LLC   RE,CHUNKLEN         CHUNK DATA LENGTH                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SCDATA(0),SVCHUNK   RESTORE CHUNK DATA                           
*                                                                               
BY625C   OC    ACLRST03,ACLRST03   HAVE A(X'03'/X'05' PAIR)?                    
         BZ    BY626               NOPE                                         
         MVC   FULL,SCPDREP                                                     
         MVC   HALF,SCBNKDT                                                     
         LLC   RE,CHUNKLEN                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    SCDATA(0),SCDATA    CLEAR CHUNK DATA                             
         MVC   SCPDREP,FULL                                                     
         MVC   SCBNKDT,HALF                                                     
*                                                                               
         ICM   R1,15,ACLRST03      SEARCH FOR NEXT X'03'/X'05' PAIR             
         LLC   R0,1(R1)            ELEMENT LENGTH                               
         AR    R1,R0               BUMP TO X'05' ELEMENT                        
         BAS   RE,GETCLRST         HAVE ANOTHER X'03' / X'05' PAIR?             
         BE    BY625A              YES - THROW CHUNK TO DRIVER                  
*                                                                               
         LLC   RE,CHUNKLEN         CHUNK DATA LENGTH                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SCDATA(0),SVCHUNK   RESTORE CHUNK DATA                           
         XC    SCCHKNUM,SCCHKNUM                                                
         MVC   SCCHKNUM,CHKSEQ     PUT CLRST SEQ NUM BACK IN CHUNK KEY          
*                                                                               
BY626    TM    DATAIND,DIDPT       TEST FOR DAYPART                             
         BZ    BY630                                                            
         GOTO1 AGENDPTO            YES - GENERATE DAYPART TOTALS                
*                                                                               
BY630    L     R5,SCNEXT           NEXT CHUNK                                   
         B     BY300                                                            
*                                                                               
BY640    LA    R1,SBLOCK                                                        
         AHI   R1,SBACONT-SBLOCK   ANY BUY CONTINUATION?                        
         OC    0(4,R1),0(R1)                                                    
         BNZ   BY262                YES - GO GET THE REST                       
*                                                                               
         CLI   SBEDEMTY,C'R'       END OF CHUNKS-TEST RERATE/AFFID              
         BE    BY645                                                            
         CLI   SBEDEMTY,C'A'                                                    
         BE    BY645                                                            
         CLI   SBEDEMTY,C'0'       IF SQAD LOOK UP                              
         BL    BY660                                                            
*                                                                               
         LA    R1,SBLOCK           ESTABLISH SBLOCK                             
         USING SBLOCK,R1                                                        
*                                                                               
         OC    SBSQDQT2(SBSQDOPL),SBSQDQT2  SKIP IF NO SECOND SQAD OPT          
         BZ    BY660                                                            
         CLI   CURBOOK,2           PROCESSED SQAD2 YET?                         
         BL    BY641               NO - GO PROCESS IT NOW                       
*                                                                               
         OC    SBSQDQT3(SBSQDOPL),SBSQDQT3  SKIP IF NO THIRD SQAD OPT           
         BZ    BY660                                                            
         CLI   CURBOOK,3           PROCESSED SQAD3 YET?                         
         BL    BY641               NO - GO PROCESS IT NOW                       
*                                                                               
         OC    SBSQDQT4(SBSQDOPL),SBSQDQT4  SKIP IF NO FOURTH SQAD OPT          
         BZ    BY660                                                            
         CLI   CURBOOK,4           PROCESSED SQAD4 YET?                         
         BNL   BY660               YES - DONE                                   
*                                                                               
BY641    SR    RE,RE                                                            
         ICM   RE,1,CURBOOK        BUMP CURRENT SQAD POINTER                    
         BNZ   *+8                                                              
         LA    RE,1                                                             
*                                                                               
         LA    RE,1(RE)                                                         
         STC   RE,CURBOOK                                                       
*                                                                               
         B     BY260                                                            
*                                                                               
         DROP  R1                                                               
*                                                                               
BY645    DS    0H                                                               
         CLI   SBEDEMTY,C'R'       END OF CHUNKS-TEST RERATE/AFFID              
         BE    *+12                                                             
         CLI   SBEDEMTY,C'A'                                                    
         BNE   BY660                                                            
         TM    DATAIND3,DIBOOK     YES-GOTO NEXT RERATE BOOK                    
         BO    BY650                                                            
         LLC   RE,CURBOOK          IF BOOK IS NOT A ROW THEN UPDATE             
         CLI   CURBOOK,0           CURRENT BOOK POINTER                         
         BNE   *+8                                                              
         LA    RE,1                                                             
         LA    RE,1(RE)                                                         
         STC   RE,CURBOOK                                                       
*                                                                               
BY650    OC    BOOKS(4),BOOKS      TEST BOOKS LIST                              
         BZ    BY660                                                            
         L     R1,ABOOK            YES-NEXT BOOK                                
         LA    R1,4(R1)                                                         
         ST    R1,ABOOK                                                         
         MVI   FIRST,C'N'                                                       
         LA    RE,BOOKS+24                                                      
         CR    R1,RE               TEST PROCESSED ALL 6 BOOKS                   
         BL    BY250                                                            
*                                                                               
BY660    LA    R2,1(R2)            NEXT DEMO TYPE                               
         OC    BOOKS(4),BOOKS      TEST BOOKS LIST SET UP                       
         BZ    BY230                                                            
         LR    RF,R9                                                            
         AHI   RF,SBQBOOK-SYSD                                                  
         MVC   0(4,RF),BOOKS       YES-RESTORE FIRST REQUESTED BOOK             
         B     BY230                                                            
*                                                                               
BY670    XC    AFDAREA,AFDAREA                                                  
         TM    DATAIND3,DIPBON     TEST PURCHASED BONUS DEMOS NEEDED            
         BZ    BY680                                                            
         TM    ININD,INIBON        YES-TEST ALREADY PROCESSED                   
         BO    BY680                                                            
         GOTO1 ABYBONUS            ALTER BUY RECORD FOR BONUS                   
         BNE   BY680                                                            
         OI    ININD,INIBON                                                     
         XC    DEMTYPES,DEMTYPES                                                
         LA    R2,DEMTYPES                                                      
         MVI   0(R2),C'P'                                                       
         B     BY230                                                            
*                                                                               
BY680    OC    PRDLST,PRDLST       TEST PRODUCT LIST FOR CHILD SPOT             
         BZ    BY690               FLIGHTS                                      
         L     R1,APRD                                                          
         LA    R1,1(R1)            YES-PROCESS NEXT PRODUCT                     
         B     BY210                                                            
*                                                                               
BY690    LM    RE,RF,AXSPILL                                                    
         LTR   RE,RE               TEST CANADIAN SPILL REQUEST                  
         BZ    BY700                                                            
         LA    RE,L'XSMKTS(RE)     YES-NEXT SPILL MARKET                        
         MVC   SBBMKT,BUYMSTA      RESET HOME MARKET                            
         BCT   RF,BY140                                                         
*                                                                               
BY700    XC    SBEMKT,SBEMKT                                                    
         OC    PRDLST,PRDLST       TEST CHILD SPOT PRODUCT LIST                 
         BZ    BYX                                                              
         CLI   SBQBPRD,0           YES-SET PRODUCT FILTER BACK TO 0             
         BNE   BYX                     IF ALL PRODUCTS                          
         MVI   SBEPRD,0                                                         
*                                                                               
BYX      J     XIT                                                              
         DROP  R3                                                               
*                                                                               
GETCLRST NTR1                                                                   
*                                                                               
GC00     CLI   0(R1),0             END OF RECORD?                               
         JE    XITNE               YES                                          
         CLI   0(R1),X'01'         POINTING TO NEXT X'01' ELEMENT?              
         JE    XITNE               YES, DONE WITH X'03'/X'05' PAIRS             
         CLI   0(R1),X'03'         INVOICE NUMBER ELEMENT?                      
         BE    GC10                YES                                          
GC05     LLC   R0,1(R1)            ELEMENT LENGTH                               
         AR    R1,R0               BUMP TO NEXT ELEMENT                         
         B     GC00                LOOP BACK AND CHECK NEXT ELEM                
*                                                                               
GC10     STCM  R1,15,ACLRST03      A(X'03'/X'05' PAIR)                          
*                                                                               
GC15     ICM   RE,15,ACLRST01      A(X'01' ELEM)                                
         USING CLSTEL01,RE                                                      
         MVI   BYTE,C'G'           GROSS                                        
         TM    CLSTSTAT,X'20'      NET?                                         
         BZ    *+8                 NO                                           
         MVI   BYTE,C'N'           YES - SET NET                                
*                                                                               
         LLC   RF,1(R1)            ELEMENT LENGTH                               
         LA    RF,0(RF,R1)         BUMP TO NEXT ELEMENT                         
         USING CLSTEL05,RF                                                      
*                                                                               
         ICM   R1,15,CLS5GRS       GROSS                                        
         ICM   R3,15,CLSTGRS       GROSS FROM X'01' ELEMENT                     
         ICM   R4,15,SVCHUNK+(SCPAY-SCDATA)                                     
         CLI   BYTE,C'G'           DOING GROSS?                                 
         BE    GC16                YES                                          
         ICM   R1,15,CLS5NET       NET                                          
         ICM   R3,15,CLSTNET       NET FROM X'01' ELEMENT                       
         ICM   R4,15,SVCHUNK+(SCPAYN-SCDATA)                                    
*                                                                               
GC16     ZAP   SCPAYP,=P'0'        INIT GROSS AND NET PAID TO TENTHS            
         ZAP   SCPAYNP,=P'0'                                                    
*                                                                               
         CVD   R1,WPKQUOT          CVD - INVOICE AMOUNT                         
         SRP   WPKQUOT,1,0         ROUND UP TO TENTHS OF A CENT                 
*                                                                               
         LTR   R3,R3               NET/GROSS FROM X'01'=0?                      
         BZ    GC18                YES - DON'T DIVIDE!                          
*                                                                               
         ZAP   WPKDIVD,WPKQUOT     MOVE TO LARGER WORKAREA                      
         SRP   WPKDIVD,5,0         SCALE UP FOR ROUNDING                        
*                                                                               
         CVD   R3,DUB              CVD - TOTAL CLEARANCE                        
         DP    WPKDIVD,DUB         RATIO INVOICE TO TOTAL CLEARANCE             
         ZAP   WPKDIVD,WPKQUOT     LARGER WORKAREA                              
         CVD   R4,DUB              CVD CURRENT BUY AMOUNT                       
         MP    WPKDIVD,DUB         CALCULATE ALLOCATION FOR INVOICE             
         SRP   WPKDIVD,64-5,5      ROUND TO TENTHS OF A PENNY                   
         ZAP   WPKQUOT,WPKDIVD     SMALLER WORKAREA                             
*                                                                               
GC18     CLI   BYTE,C'G'           DOING GROSS?                                 
         BNE   *+14                NO                                           
         ZAP   SCPAYP,WPKQUOT      GROSS                                        
         B     *+10                                                             
         ZAP   SCPAYNP,WPKQUOT     NET                                          
*                                                                               
         SRP   WPKQUOT,64-1,0      ROUND TO PENNIES                             
         CVB   R1,WPKQUOT          CVB - INV ALLOCATION FOR BUY                 
*                                                                               
         CLI   BYTE,C'G'           DOING GROSS?                                 
         BNE   *+12                NO                                           
         ST    R1,SCPAY            GROSS                                        
         B     *+8                                                              
         ST    R1,SCPAYN           NET                                          
*                                                                               
GC20     MVC   SCCHKNUM,CLS5CHK    CHECK NUMBER                                 
*                                                                               
         TM    CLS5STAT,CLS5STAT_CK IF NOT CK PAYMENT                           
         BO    GC21                                                             
*                                                                               
         OC    CLS5CHK,CLS5CHK     AND NO CHECK NUMBER                          
         BZ    GC21A                  SKIP ELEMENT                              
         J     XITEQ               ELSE KEEP                                    
*                                                                               
GC21     DS    0H                                                               
*                                                                               
         CLI   SBWRPROF+13,C'Y'    IF NOT REPORTING CHECK CLEARANCES            
         JE    GC22                                                             
*                                                                               
GC21A    DS    0H                                                               
*                                                                               
         XC    ACLRST03,ACLRST03      SKIP ELEMENT                              
         XC    SCCHKNUM,SCCHKNUM      CLEAR CHECK NUMBER                        
         XC    SCCHKDT,SCCHKDT        CHECK DATE                                
         XC    SCBNKDT,SCCHKDT        BANK DATE                                 
         XC    SCPAYN,SCPAYN          NET                                       
         XC    SCPAY,SCPAY            GROSS                                     
         ZAP   SCPAYNP,=P'0'          NET                                       
         ZAP   SCPAYP,=P'0'           GROSS                                     
*                                                                               
         J     XITNE               DROP DATA                                    
*                                                                               
GC22     DS    0H                                                               
*                                                                               
         MVC   SCCHKNUM,=CL8'CHECK' SET SPECIAL CHECK NUMBER                    
         MVC   SCCHKDT,CLSTCLRD    CHECK DATE = CLEARED DATE                    
         MVC   SCBNKDT,CLSTCLRD    BANK DATE = CLEATED DATE                     
*                                                                               
         MVC   CLS5CHK,SCCHKNUM    CHECK NUMBER                                 
         MVC   CLS5CHDT,SCCHKDT    CHECK DATE                                   
         MVC   CLS5BKDT,SCBNKDT    BANK DATE                                    
         J     XITEQ               EXIT CC EQU                                  
         DROP  RE,RF               DROP USINGS                                  
*                                                                               
PROCGL   TM    DATAIND9,DIWIPW     WESTERN PW?                                  
         BZ    *+12                 NO                                          
         OI    SBEFLAG,SBEWIPW      YES - SET EXTRACT FLAG FOR SPOTBUY          
         OI    SBQPER,SBQPWK                                                    
*                                                                               
         L     R3,SBAIO1                                                        
         USING GOALRECD,R3                                                      
         MVC   SBEPRD,SBQBPRD                                                   
         CLI   SBQBPRD,0                                                        
         BE    *+12                                                             
         CLI   SBQBPRD,X'FF'                                                    
         BNE   GL01                                                             
         MVI   SBEPRD,0            ALL PRODUCTS -                               
         CLC   SBBPRD,GKEYPRD      TEST PASSIVE POINTER                         
         BNE   GLX                 YES - IGNORE THIS RECORD                     
*                                                                               
GL01     CLI   OFFLINE,C'Y'        ++TEST                                       
         BNE   GLX                                                              
         BAS   RE,GETDPTTB         GET DAYPART TABLE                            
         MVC   SBDPTCD,GKEYDPT     SET DAYPART CODE                             
         TM    DATAIND,DIDPT       TEST FOR DAYPART                             
         BZ    GL02                                                             
         GOTO1 SETDPT,GKEYDPT      YES - SET DAYPART DETAILS                    
*                                                                               
GL02     MVI   SBBPRD,0            INITIALIZE PRODUCTS                          
         MVI   SBBPRD2,0                                                        
         XC    SBPRD,SBPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVI   SVPRD,0                                                          
         MVI   SVEST,0                                                          
         MVI   SBEDEMTY,0                                                       
         XC    SBSTA,SBSTA                                                      
         MVC   SBSTA,=C'AAAAA'     GOALS ARE FOR ALL STATIONS                   
         MVC   SBAFFIL,=C'ALL'                                                  
         XC    SBCBLNET,SBCBLNET                                                
         XC    SBCHAN,SBCHAN                                                    
         MVI   SBCHAN,X'FF'                                                     
*                                                                               
         TM    DATAIND2,DIGLDEM    TEST GOAL DEMOS NEEDED                       
         BZ    *+8                                                              
         MVI   SBEDEMTY,C'G'       YES - SET DEMO TYPE TO GOAL                  
         TM    DATAIND3,DILOCKIN   TEST LOCKIN DATA REQUIRED                    
         BZ    GL04                                                             
         MVI   SBELOCK,C'Y'        YES-                                         
         MVI   SBESPILL,0          SET SPILL IND TO ORIG+SPILL                  
         CLI   SBQSPILL,C'C'       TEST SPILL=COMBINED                          
         BE    GL04                YES                                          
         MVI   SBESPILL,C'O'       NO-ORIGINATING MARKET ONLY                   
*                                                                               
GL04     XC    PARAS(24),PARAS                                                  
         TM    DATAIND8,DI2NDCOS   EXTRACT SECOND COST?                         
         BZ    GL05                 NO                                          
         LLC   R1,GKEYPRD                                                       
         MVC   SBBEST,GKEYEST                                                   
         BAS   RE,CHKEST                                                        
         BNE   GL05                                                             
*                                                                               
* BE CAREFUL - SBE2COS GETS RESET FOR SPOTBUY CALLS                             
*                                                                               
         OI    SBEFLAG,SBE2COS     SET SPOTBUY CONTROL                          
         USING ESTBUFFD,R1                                                      
         MVC   PARAS+4(4),ECOS2                                                 
         MVI   PARAS+4,X'FD'       PASSING COS2 FACTOR IN P2                    
         DROP  R1                                                               
*                                                                               
GL05     DS    0H                                                               
***      TM    DATAIND2,DIGLDEM    REPORTING GOAL DEMOS?                        
***      BZ    GL05A               NO                                           
***      OC    SBPDEMOS,SBPDEMOS   DEMO MENU OR DEMO OPTION?                    
***      BNZ   *+12                YES - GET EST DEMOS                          
***      CLI   SBQMKTWT,C'N'       NO  - MARKET WEIGHTING?                      
***      BE    GL05A               NO  - NO ESTIMATE DEMOS NEEDED               
         MVC   BYTE,SVEST          SAVE SVEST                                   
         MVC   SVEST,GKEYEST       ESTIMATE                                     
         LLC   RE,GKEYPRD          GOAL PRODUCT                                 
         BCTR  RE,0                -1 FOR INDEXING                              
         MHI   RE,PRDBUFFL         MULTIPLY BY PRD BUFFER LENGTH                
         L     R2,SBAPRDBF         A(PRD BUFFER)                                
         AR    R2,RE               ADD INDEX                                    
         USING PRDBUFFD,R2         PRODUCT BUFFER DSECT                         
***                                                                             
* EVEN IF SBPDEMOS IS SET, OVERRIDE SBPDEMOS WITH ENONTDMS BECAUSE              
* GOALS ARE ALWAYS BASED OFF THE TARGET DEMO ON THE ESTIMATE                    
***                                                                             
         TM    SBEFLAG8,SBE8CP1+SBE8CP2 COMSCORE PASS 1/2?                      
         BZ    GL05AA              NO                                           
         OI    SBEFLAG8,SBE8NTD    SET GETEST TO ONLY GET ENONTDMS              
         OC    SBPDEMOS,SBPDEMOS   DEMO MENU OR DEMO OPTION SET?                
         BZ    *+8                 NO                                           
         OI    SBEFLAG8,SBE8NTDG   YES - SAVE ENONTDMS TO ACOMLSTG              
         GOTO1 AGETEST,PBALPH      SET COMDLIST FROM ENONTDMS                   
         NI    SBEFLAG8,X'FF'-SBE8NTD  TURN OFF SBE8NTD FLAG                    
         NI    SBEFLAG8,X'FF'-SBE8NTDG TURN OFF SBE8NTDG FLAG                   
*                                                                               
GL05AA   GOTO1 AGETEST,PBALPH      GET ESTIMATE DEMOS                           
         MVC   SVEST,BYTE          RESTORE SVEST                                
         DROP  R2                  DROP R1                                      
*                                                                               
GL05A    GOTO1 SPOTGOAL,PARAS,SBLOCK     ** SPOTGOAL **                         
         MVI   MKTIND,FF                                                        
         CLI   SBQSPILL,C'S'       TEST SPILL=YES                               
         BNE   *+8                                                              
         MVI   MKTIND,C'O'         YES-SET MARKET TO ORIGINATING                
         CLI   SBELOCK,C'Y'        TEST LOCKIN                                  
         BNE   GL06                                                             
         CLI   SBQSPILL,C'S'       YES-TEST SEPERATE SPILL REQUESTED            
         BNE   GL06                                                             
         MVC   MKTIND,SBESPILL     YES-SET ORIGINATING/SPILL MKT                
*                                                                               
GL06     L     R5,SBACHUNK                                                      
         USING SGLCHNKD,R5                                                      
*                                                                               
GL10     OC    SGNEXT,SGNEXT       TEST END OF CHUNKS                           
         BZ    GL50                                                             
         CLI   SBESPILL,C'S'       TEST WANT SPILL LOCKIN ONLY                  
         BNE   *+16                                                             
         XC    SGDATA,SGDATA       YES-CLEAR GOAL DATA FIELDS                   
         XC    SGKDATA,SGKDATA                                                  
         CLI   SBQBPRD,0           TEST PRD=ALL                                 
         BNE   GL12                                                             
         TM    SBQPIND,SBQPOLAL    YES - TEST INCLUDE POL                       
         BO    GL14                                                             
         CLI   SGPRD1,FF                 NO - REJECT PRD=POL                    
         BE    GL40                                                             
         B     GL14                                                             
*                                                                               
GL12     CLC   SGPRD1,SBQBPRD      NO - CHECK PRODUCT                           
         BNE   GL40                                                             
*                                                                               
GL14     CLC   SBBPRD,SGPRD1       TEST CHANGE OF PRODUCT                       
         BE    GL18                                                             
         MVC   SBBPRD,SGPRD1       YES - SET PRODUCT CODE FOR DRIVER            
         BRAS  RE,EXTPRD           EXTRACT PRODUCT DETAILS                      
         BE    *+12                                                             
         MVI   SBBPRD,0                                                         
         B     GL40                                                             
         CLI   SBQMKTWT,C'D'       TEST PRIMARY DEMO WEIGHTING                  
         BNE   GL16                                                             
         MVC   SBBEST,GKEYEST                                                   
         BAS   RE,MKTWGT           GET MARKET WEIGHT                            
*                                                                               
GL16     TM    DATAIND5,DIMKTRNK   TEST MARKET RANKING                          
         BZ    GL18                                                             
         MVC   SBBEST,GKEYEST                                                   
         GOTO1 SPOTMKRK,DMCB,SBLOCK GET MARKET RANK                             
*                                                                               
GL18     CLC   SVPRD,SBBPRD        TEST CHANGE OF PRODUCT/ESTIMATE              
         BNE   *+14                                                             
         CLC   SVEST,GKEYEST                                                    
         BE    GL20                                                             
         MVC   SVPRD,SBBPRD        YES                                          
         MVC   SVEST,GKEYEST                                                    
         OI    SBEUDEF,SBEUECOM                                                 
         BRAS  RE,GETUCOM                                                       
*                                                                               
         TM    SBEUDEF,SBEUEST1+SBEUEST2  TEST ESTIMATE USER FIELDS             
         BZ    GL19                       NEEDED                                
         GOTO1 GETESTNM                                                         
*                                                                               
GL19     TM    DATAIND2,DIGLDEM    TEST GOAL DEMOS                              
         BZ    GL20                                                             
         OC    SBPDEMOS,SBPDEMOS   YES-TEST DEMO MENU OR DEMO OPTION            
         BNZ   *+12                                                             
         CLI   SBQMKTWT,C'N'           OR MARKET WEIGHTING                      
         BE    GL20                                                             
         GOTO1 AGETEST,SBPRD       YES-GET ESTIMATE DEMOS                       
*                                                                               
GL20     CLC   SBBPRD2,SGPRD2      TEST CHANGE OF SECOND PRD                    
         BE    GL30                                                             
         MVC   SBBPRD2,SGPRD2                                                   
         XC    SBPRD2,SBPRD2                                                    
         CLI   SBBPRD2,0                                                        
         BE    GL30                                                             
         LLC   RE,SBBPRD2                                                       
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    RE,R1                                                            
         MVC   SBPRD2,PBALPH-PRDBUFFD(RE)                                       
*                                                                               
GL30     TM    DATAIND,DICPP       TEST CPP                                     
         BZ    GL32                                                             
         NI    ININD,FF-INIEQUIV   YES-TURN OFF EQUIVALENCING                   
         CLI   SBSPPROF+4,C'Y'     UNLESS, EQUIV DETAIL LINES                   
         BE    *+12                                                             
         TM    DATAIND,DIDPTLEN            OR DPT/LEN NOT IN ROWS               
         BNZ   GL32                                                             
         OI    ININD,INIEQUIV                                                   
*                                                                               
GL32     MVI   SBBYTYPE,C'C'       SET BUY TYPE TO CASH                         
         CLI   SBQGETNM,C'Y'       TEST GET BUYER/BILLER NAMES                  
         BNE   GL34                NO                                           
         MVC   DUB(2),SBBCLT       YES-                                         
         MVC   DUB+2(1),SBBPRD                                                  
         MVC   DUB+3(1),GKEYEST                                                 
         MVC   DUB+4(2),GKEYMKT                                                 
         CLC   SVNAME,DUB          TEST CLT/PRD/EST/MKT CHANGE                  
         BE    GL34                NO                                           
         MVC   SVNAME,DUB          YES-GET BUYER/BILLER NAMES                   
         GOTO1 AGETNAME                                                         
*                                                                               
GL34     LA    R2,2                                                             
*                                                                               
GL36     MVC   SBLEN,SGSLNT                                                     
         ST    R5,SBACURCH         A(CURRENT EXTRACT CHUNK)                     
*                                                                               
         TM    DTAIND11,DIDEMV     WANT UNIVERSE DATA?                          
         BZ    GL36A               NO                                           
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   GL36A               YES                                          
*                                                                               
         ICM   RF,15,SBUNVDEM      A(UNIVERSE FOR FIRST 8 DEMOS)                
         BZ    GL36A               IF NOT SET, DON'T LOOK UP                    
         XC    0(32,RF),0(RF)      CLEAR UNIVERSE DEMOS                         
*                                                                               
         BRAS  RE,SETSTA           SET STATION FOR MARKET                       
         BNE   GL36A               IF NO STATIONS EXIST, NO UNIVERSE            
*                                                                               
         OI    SBEFLAG9,SBE9GTUV   FLAG TO GET UNIVERSE FOR DEMO LIST           
         GOTO1 SPOTBUY,PARAS,SBLOCK                                             
         NI    SBEFLAG9,X'FF'-SBE9GTUV                                          
         MVC   SBSTA,=C'AAAAA'     GOALS ARE FOR ALL STATIONS                   
*                                                                               
GL36A    GOTO1 ADRIVIN                                                          
*                                                                               
         TM    DATAIND,DIDPT       TEST FOR DAYPART                             
         BZ    GL38                                                             
         GOTO1 AGENDPTO            YES - GENERATE DAYPART TOTALS                
*                                                                               
GL38     TM    DATAIND4,DIBYTYPE   TEST BUY TYPE IS A ROW                       
         BZ    GL40                                                             
         CLI   BUYRECSW,C'Y'       YES-TEST ANY BUY RECORDS                     
         BNE   GL40                                                             
         MVI   SBBYTYPE,C'T'       YES-SET BUY TYPE TO TRADE                    
         BCT   R2,GL36                 AND RE-GENERATE DRIVER RECORDS           
*                                                                               
GL40     L     R5,SGNEXT           NEXT CHUNK                                   
         B     GL10                                                             
*                                                                               
GL50     CLI   SBELOCK,C'Y'        TEST LOCKIN REQUEST                          
         BNE   GLX                                                              
         CLI   SBQSPILL,C'S'       YES-TEST SEPERATE SPILL REQUESTED            
         BNE   GLX                                                              
         CLI   SBESPILL,C'S'       YES-TEST SPILL ASKED FOR YET                 
         BE    GLX                                                              
         MVI   SBESPILL,C'S'       NO-JUST GET SPILL LOCKIN NOW                 
         B     GL04                                                             
*                                                                               
GLX      J     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
PROCSL   L     R3,SBAIO1                                                        
         USING SLKRECD,R3                                                       
         TM    SLKKIND,SLKKIPSV    TEST PASSIVE POINTER                         
         BO    SLKX                YES - IGNORE THIS RECORD                     
         BAS   RE,GETDPTTB         GET DAYPART TABLE                            
         TM    DATAIND,DIDPT       TEST FOR DAYPART                             
         BZ    SLK2                                                             
         GOTO1 SETDPT,SLKKDPT      YES - SET DAYPART DETAILS                    
***                                                                             
* NEED TO SET ENONTDMS BEFORE CALLING SPOTSTLK                                  
* SBBEST AND SBPRD ALREADY SET WHEN SPOTIO PROCESSED SL RECORD                  
***                                                                             
SLK2     TM    SBEFLAG8,SBE8CP2    COMSCORE PASS 2?                             
         BZ    SLK3                NO                                           
         OC    SBPDEMOS,SBPDEMOS   DEMO MENU OR DEMO OPTION SET?                
         BNZ   SLK3                YES - DO NOT OVERRIDE WITH ENONTDMS          
         OI    SBEFLAG8,SBE8NTD    SET GETEST TO ONLY GET ENONTDMS              
         MVC   SVEST,SBBEST        SET ESTIMATE                                 
*                                                                               
         GOTO1 AGETEST,SBPRD       GET ENONTDM FROM ESTIMATE REC                
*                                                                               
         NI    SBEFLAG8,X'FF'-SBE8NTD  TURN OFF SBE8NTD FLAG                    
*                                                                               
SLK3     MVI   SBBPRD,0            INITIALIZE PRODUCTS                          
         MVI   SBBPRD2,0                                                        
         XC    SBPRD,SBPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVI   SVPRD,0                                                          
         MVI   SVEST,0                                                          
         MVI   SBEPRD,0                                                         
         CLI   SBQBPRD,0                                                        
         BE    SLK4                                                             
         CLI   SBQBPRD,X'FF'                                                    
         BE    SLK4                                                             
         MVC   SBEPRD,SBQBPRD                                                   
*                                                                               
SLK4     DS    0H                  CALL SPOTSLK                                 
         GOTO1 SPOTSTLK,PARAS,SBLOCK                                            
*                                                                               
         MVI   MKTIND,FF                                                        
         L     R5,SBACHUNK                                                      
         USING SSLCHNKD,R5                                                      
*                                                                               
SLK6     OC    SLNEXT,SLNEXT       TEST END OF CHUNKS                           
         BZ    SLK32                                                            
         CLI   SBQBPRD,0           TEST PRD=ALL                                 
         BNE   SLK8                                                             
         TM    SBQPIND,SBQPOLAL    YES-TEST INCLUDE POL                         
         BO    SLK10               YES                                          
         CLI   SLPRD1,FF           NO-REJECT PRD=POL                            
         BE    SLK30                                                            
         B     SLK10                                                            
*                                                                               
SLK8     CLC   SLPRD1,SBQBPRD      CHECK THE PRODUCT                            
         BNE   SLK30                                                            
*                                                                               
SLK10    CLC   SBBPRD,SLPRD1       TEST CHANGE OF PRODUCT                       
         BE    SLK16                                                            
         MVC   SBBPRD,SLPRD1       YES-SET PRODUCT DETAILS                      
         BRAS  RE,EXTPRD           EXTRACT PRODUCT DETAILS                      
         BNE   SLK30                                                            
         CLI   SBQMKTWT,C'D'       TEST PRIMARY DEMO WEIGHTING                  
         BNE   SLK14                                                            
         BAS   RE,MKTWGT           GET MARKET WEIGHT                            
*                                                                               
SLK14    TM    DATAIND5,DIMKTRNK   TEST MARKET RANKING                          
         BZ    SLK16                                                            
         GOTO1 SPOTMKRK,DMCB,SBLOCK GET MARKET RANK                             
*                                                                               
SLK16    CLC   SVPRD,SBBPRD        TEST CHANGE OF PRODUCT/ESTIMATE              
         BNE   *+14                                                             
         CLC   SVEST,SLKKEST                                                    
         BE    SLK18                                                            
         MVC   SVPRD,SBBPRD        YES                                          
         MVC   SVEST,SLKKEST                                                    
*                                                                               
SLK17    TM    DATAIND7,DISLDEM    TEST DEMOS NEEDED                            
         BZ    SLK18                                                            
         OC    SBPDEMOS,SBPDEMOS   YES-TEST DEMO MENU OR DEMO OPTION            
         BNZ   *+12                                                             
         CLI   SBQMKTWT,C'N'           OR MARKET WEIGHTING                      
         BE    SLK18                                                            
         GOTO1 AGETEST,SBPRD       YES-GET ESTIMATE DEMOS                       
         OC    ADEMLST,ADEMLST     DEMO LIST SET FOR MKT WEIGHING?              
         BNZ   SLK18               YES                                          
*                                                                               
         LA    RE,255              YES - FIND ENTRY FOR THIS PRD/EST            
         CLC   SBQPRD,=C'POL'                                                   
         BE    *+8                                                              
         IC    RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         SR    RF,RF                                                            
         ICM   RF,1,SBBEST                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         LA    R1,0(R1,RE)                                                      
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)          POINTER TO ESTIMATE BUFFER                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         MHI   RE,ESTBUFFL                                                      
         ICM   R1,15,SBAESTBF                                                   
         BZ    SLK18                                                            
         LA    RE,0(R1,RE)                                                      
         LA    R1,EBDEMOS-ESTBUFFD(RE)                                          
         ST    R1,ADEMLST                                                       
*                                                                               
SLK18    CLC   SBBPRD2,SLPRD2      TEST CHANGE OF SECOND PRD                    
         BE    SLK20                                                            
         MVC   SBBPRD2,SLPRD2      YES                                          
         XC    SBPRD2,SBPRD2                                                    
         CLI   SBBPRD2,0                                                        
         BE    SLK20                                                            
         LLC   RE,SBBPRD2                                                       
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    RE,R1                                                            
         MVC   SBPRD2,PBALPH-PRDBUFFD(RE)                                       
*                                                                               
SLK20    TM    DATAIND,DICPP       TEST CPP                                     
         BZ    SLK22                                                            
         NI    ININD,FF-INIEQUIV   YES-TURN OFF EQUIVALENCING                   
         CLI   SBSPPROF+4,C'Y'     UNLESS, EQUIV DETAIL LINES                   
         BE    *+12                                                             
         TM    DATAIND,DIDPTLEN            OR DPT/LEN NOT IN ROWS               
         BNZ   SLK22                                                            
         OI    ININD,INIEQUIV                                                   
*                                                                               
SLK22    MVI   SBBYTYPE,C'C'       SET BUY TYPE TO CASH                         
         CLI   SBQGETNM,C'Y'       TEST GET BUYER/BILLER NAMES                  
         BNE   SLK24               NO                                           
         MVC   DUB(2),SBBCLT       YES-                                         
         MVC   DUB+2(1),SBBPRD                                                  
         MVC   DUB+3(1),SLKKEST                                                 
         MVC   DUB+4(2),SBBMKT                                                  
         CLC   SVNAME,DUB          TEST CLT/PRD/EST/MKT CHANGE                  
         BE    SLK24               NO                                           
         MVC   SVNAME,DUB          YES-GET BUYER/BILLER NAMES                   
         GOTO1 AGETNAME                                                         
*                                                                               
SLK24    LA    R2,2                                                             
*                                                                               
SLK26    MVC   SBLEN,SLSLNT                                                     
         ST    R5,SBACURCH         A(CURRENT EXTRACT CHUNK)                     
         GOTO1 ADRIVIN                                                          
*                                                                               
         TM    DATAIND,DIDPT       TEST FOR DAYPART                             
         BZ    SLK28                                                            
         GOTO1 AGENDPTO            YES - GENERATE DAYPART TOTALS                
*                                                                               
SLK28    TM    DATAIND4,DIBYTYPE   TEST BUY TYPE IS A ROW                       
         BZ    SLK30                                                            
         CLI   BUYRECSW,C'Y'       YES-TEST ANY BUY RECORDS                     
         BNE   SLK30                                                            
         MVI   SBBYTYPE,C'T'       YES-SET BUY TYPE TO TRADE                    
         BCT   R2,SLK26                AND RE-GENERATE DRIVER RECORDS           
*                                                                               
SLK30    L     R5,SLNEXT           NEXT CHUNK                                   
         B     SLK6                                                             
*                                                                               
SLK32    B     SLKX                                                             
*                                                                               
SLKX     J     XIT                                                              
         EJECT                                                                  
MKTNM    DS    0H                                                               
         SR    R2,R2               TEST MARKET IN NAME POOL YET                 
         SR    R3,R3                                                            
         ICM   R3,3,SBBMKT                                                      
         D     R2,=F'8'                                                         
         LA    RF,MKTTAB(R3)                                                    
         LA    RE,X'80'                                                         
         LTR   R2,R2                                                            
         BZ    *+12                                                             
         SRL   RE,1                                                             
         BCT   R2,*-4                                                           
         EX    RE,TM                                                            
         BO    MKTNMX              YES                                          
         EX    RE,OI               NO -                                         
         GOTO1 PUTMKTNM            PUT MARKET DETAILS IN NAME POOL              
MKTNMX   J     XIT                                                              
*                                                                               
TM       TM    0(RF),0                                                          
OI       OI    0(RF),0                                                          
         EJECT                                                                  
AFFIDHK  DS    0H                                                               
         LA    R1,AFDHOOK          SET AFFID HOOK                               
         ST    R1,SBAFHOOK                                                      
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
* PROCESS PW OVERRIDE $$                                                        
* ON ENTRY, SBACURCH = A(PW MKT REC)                                            
PROCPW   DS    0H                                                               
         XC    PWLOCKDT,PWLOCKDT                                                
         MVI   PWLOCKST,0                                                       
         MVI   MKTIND,X'FF'                                                     
         SR    R0,R0                                                            
         L     R3,SBACURCH                                                      
         LR    R2,R3                                                            
         LA    R3,PWEL-PWRECD(R3)                                               
*                                                                               
         CLI   0(R3),X'01'         MISC PW DATA ELEM?                           
         BNE   PW02                 NO (BUT IT SHOULD BE HERE...)               
         MVC   PWLOCKST,PWGNFLG-PWGNEL(R3)  SAVE LOCK STATUS                    
*                                                                               
PW02     CLI   0(R3),0                                                          
         BE    PW05                                                             
         CLI   0(R3),X'20'         LOCK ACTIVITY ELEM?                          
         BE    PW03                                                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PW02                                                             
*                                                                               
         USING PWDTIEL,R3                                                       
PW03     MVC   PWLKCLDT,PWDTICLD   CLT LCK DATE                                 
         MVC   PWLKSTDT,PWDTISLD   STA LCK DATE                                 
         MVC   PWLKBYDT,PWDTIBLD   BUY LCK DATE                                 
         MVC   PWLKPWDT,PWDTIPLD   PW LCK DATE                                  
         DROP  R3                                                               
*                                                                               
PW05     DS    0H                                                               
         TM    SBACURCH,X'80'      CALLED FROM READGOLS?                        
         BNZ   PWX                  YES - SKIP MODE CALLS                       
*                                                                               
         LR    R3,R2                                                            
         LA    R3,PWEL-PWRECD(R3)                                               
*                                                                               
PW10     CLI   0(R3),0                                                          
         BE    PW40                                                             
         CLI   0(R3),X'15'         WEEKLY CLTBUY $OVRD ELEM?                    
         BE    PW30                                                             
         CLI   0(R3),X'16'         WEEKLY CLLOCK $OVRD ELEM?                    
         BE    PW30                                                             
PW20     IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PW10                                                             
*                                                                               
PW30     DS    0H                                                               
         BAS   RE,CKPER            MAKE SURE WEEK IN REQ PER                    
         BNE   PW20                ELEM NOT IN PER                              
         ST    R3,SBACURCH                                                      
         GOTO1 ADRIVIN                                                          
         B     PW20                                                             
*                                                                               
PW40     LR    R3,R2                                                            
         LA    R3,PWEL-PWRECD(R3)                                               
*                                                                               
PW50     CLI   0(R3),0                                                          
         BE    PWX                                                              
         CLI   0(R3),X'06'         WEEKLY $$$ ELEM?                             
         BE    PW70                                                             
PW60     IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PW50                                                             
*                                                                               
PW70     DS    0H                                                               
         BAS   RE,CKPER            MAKE SURE WEEK IN REQ PER                    
         BNE   PW60                ELEM NOT IN PER                              
         USING PWDOLEL,R3                                                       
         OC    PWDOLBIL,PWDOLBIL                                                
         BZ    PW60                                                             
         CLC   PWDOLBIL,=X'80000000'                                            
         BE    PW60                                                             
         ST    R3,SBACURCH                                                      
         GOTO1 ADRIVIN                                                          
         B     PW60                                                             
         DROP  R3                                                               
*                                                                               
PWX      J     XIT                                                              
         EJECT                                                                  
*                                                                               
* PROCESS PW LOCK $$                                                            
* ON ENTRY, SBACURCH = A(PW STA REC)                                            
PROCPW2  DS    0H                                                               
         MVI   MKTIND,X'FF'                                                     
         SR    R0,R0                                                            
         L     R3,SBACURCH                                                      
*                                                                               
         LA    R3,PWEL-PWRECD(R3)                                               
PW210    CLI   0(R3),0                                                          
         BE    PW2X                                                             
         CLI   0(R3),X'06'         ANOTHER OVERRIDE ELEM?                       
         BE    PW230                                                            
PW220    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PW210                                                            
*                                                                               
PW230    DS    0H                                                               
         BAS   RE,CKPER            MAKE SURE WEEK IN REQ PER                    
         BNE   PW220               ELEM NOT IN PER                              
         ST    R3,SBACURCH                                                      
         GOTO1 ADRIVIN                                                          
         B     PW220                                                            
*                                                                               
PW2X     J     XIT                                                              
         SPACE 2                                                                
*                                                                               
* CKPER: TEST IF ELEM WITHIN REQUEST PERIOD                                     
*        R3 = A(ELEM)                                                           
*                                                                               
CKPER    NTR1                                                                   
         CLC   SBBQSTP,2(R3)                                                    
         BH    CPBAD                                                            
         CLC   SBBQENDP,2(R3)                                                   
         BL    CPBAD                                                            
*                                                                               
CPGOOD   J     XITEQ                                                            
CPBAD    J     XITNE                                                            
         EJECT                                                                  
* RSPDT                                                                         
* READS EACH RESPONSE DATA ELEM AND PASSES TO DRIVER                            
RSPDT    NTR1                                                                   
         USING BUYREC,R3                                                        
         LA    R3,BDELEM                                                        
         DROP  R3                                                               
         SR    R0,R0                                                            
*                                                                               
RD10     CLI   0(R3),X'73'         INFO DAILY COUNT ELEM?                       
         BE    RD20                                                             
RD12     IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         JE    XIT                                                              
         B     RD10                                                             
*                                                                               
RD20     ST    R3,SBACURCH                                                      
*&&DO                                                                           
* CHECK FOR FILTER ON DATES                                                     
         USING INFDCNTS,R3                                                      
         OC    SBERSPDT,SBERSPDT   ANY RESPONSE DATE FILTER?                    
         BZ    RD30                 NO                                          
         CLC   INFDDATE,SBERSPDT   RSP DATE < PERIOD START?                     
         BL    RD12                 YES - SKIP ELEM                             
         CLC   INFDDATE,SBERSPDT+2 RSP DATE > PERIOD END?                       
         BH    RD12                 YES - SKIP ELEM                             
*&&                                                                             
RD30     GOTO1 ADRIVIN                                                          
         B     RD12                                                             
         EJECT                                                                  
*                                                                               
* SPOT HOOK                                                                     
* SETS SBYORN TO C'N' TO REJECT A SPOT                                          
*                                                                               
SPOTHOOK NTR1                                                                   
         L     R4,SBASPOT                                                       
         USING REGELEM,R4                                                       
         CLI   RCODE,11            TEST POL BUY                                 
         BL    SPHK1                                                            
         TM    SBQPIND2,SBQPIGNO   YES-TEST NO PIGGYBACKS                       
         BZ    SPHK0                                                            
         CLI   RLEN,14                                                          
         BH    SPHKNO                                                           
         B     SPHK1                                                            
*                                                                               
SPHK0    TM    SBQPIND,SBQPIGS     TEST PIGGYBACKS ONLY                         
         BO    *+12                                                             
         CLI   SBQBPRD2,0                                                       
         BE    SPHK1               NO-OK                                        
         CLI   RLEN,18             YES-TEST 2 PRODUCTS                          
         BL    SPHKNO                  NO-REJECT                                
         CLI   SBQBPRD2,0          TEST 2ND PRODUCT FILTER                      
         BE    SPHK1                                                            
         CLC   SBQBPRD2,RPPRD      YES-CHECK THAT ONE OF THEM IS THE            
         BE    SPHK1                   2ND REQUESTED PRODUCT                    
         CLC   SBQBPRD2,RPPRD+4                                                 
         BNE   SPHKNO                                                           
*                                                                               
SPHK1    CLI   SBQBPRD,X'FF'       TEST PRODUCT=POL REQUEST                     
         BE    SPHK2                                                            
         CLI   RCODE,11            NO-TEST POL BUY                              
         BL    SPHK2                                                            
         CLI   RLEN,14             AND SPOT'S ALLOCATED                         
         BL    SPHK2                                                            
         LLC   R1,RPPRD            YES-VALIDATE THE ESTIMATES FOR               
         BAS   RE,CHKEST               ALLOCATED PRODUCT(S)                     
         BE    SPHK2                                                            
         CLI   RLEN,18                                                          
         BL    SPHKNO                                                           
         LLC   R1,RPPRD+4                                                       
         BAS   RE,CHKEST                                                        
         BNE   SPHKNO                                                           
*                                                                               
SPHK2    TM    DATAIND6,DIDR       TEST DIRECT RESPONSE ONLY                    
         BZ    SPHK4                                                            
         CLI   SBESPOTS,SBESALL    AND SPT=ALL FILTER NOT SPECIFIED             
         BE    SPHK4                                                            
         SR    R0,R0               YES-LOOK FOR RSVP ELEMENT                    
*                                                                               
SPHK3    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    SPHKNO                                                           
         CLI   0(R4),X'17'                                                      
         BE    SPHK4               FOUND                                        
         CLI   0(R4),6                                                          
         BL    SPHK3                                                            
         CLI   0(R4),8                                                          
         BNH   SPHKNO                                                           
         CLI   0(R4),X'0B'                                                      
         BL    SPHK3                                                            
         CLI   0(R4),X'0D'                                                      
         BNH   SPHKNO                                                           
         B     SPHK3                                                            
*                                                                               
SPHK4    CLI   FLIGHTSW,C'Y'       TEST SCANNING BUY FOR PRODUCTS               
         BNE   SPHK8               (FOR CHILD SPOT FLIGHTS)                     
         LA    R1,PRDLST           YES-ADD PRODUCT TO LIST                      
         LA    R0,L'PRDLST                                                      
*                                                                               
SPHK6    CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),RPPRD                                                    
         B     SPHKNO                                                           
         CLC   RPPRD,0(R1)                                                      
         BE    SPHKNO                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,SPHK6                                                         
         DC    H'0'                                                             
*                                                                               
SPHK8    DS    0H                                                               
*                                                                               
SPHKYES  GOTO1 RPT2,RPSPHOOK       TRY REPORT APPLICATION OVERLAY               
         B     SPHKX                                                            
*                                                                               
SPHKNO   MVI   SBYORN,C'N'                                                      
*                                                                               
SPHKX    J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* CHECK FOR VALID ESTIMATE                                                      
* INPUT  : R1 = PRODUCT CODE                                                    
*          SBBEST = ESTIMATE NUMBER                                             
* OUTPUT : CC EQ - R1=A(ESTIMATE BUFFER ENTRY)                                  
*          CC NE - ESTIMATE IS INVALID                                          
*                                                                               
CHKEST   LR    R0,RE                                                            
         LR    RE,R1                                                            
         LR    R1,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   R1,4096                                                          
         USING SYSD+4096,R1                                                     
         TM    SBEFLAG3,SBE3TRD    TEST MUSHING TRADE/CASH                      
         BZ    *+8                                                              
         N     RE,=X'0000007F'     ELSE DROP X'80' FROM PRDCODE                 
         DROP  R1                                                               
*                                                                               
CE10     BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,SBBEST                                                        
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)                                                       
         BZ    CHKESTN                                                          
         BCTR  R1,0                                                             
         MHI   R1,ESTBUFFL                                                      
         A     R1,SBAESTBF                                                      
*                                                                               
CHKESTY  LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
CHKESTN  LTR   RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* DETERMINE WHETHER PRIMARY PURCH DEMO IS OVERRIDDEN                            
* IF IT ISN'T, THE DEMO ADJUSTMENT FACTOR, DEMFAC, IS TURNED OFF                
*                                                                               
         USING BUYREC,R3                                                        
GETPDEM  LR    R0,RE                                                            
         SR    RE,RE                                                            
         LA    RF,BDELEM                                                        
*                                                                               
GETPDEM2 CLI   0(RF),0                                                          
         BE    GETPDEM4                                                         
         CLI   0(RF),2                                                          
         BE    *+14                                                             
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     GETPDEM2                                                         
         USING NDELEM,RF                                                        
         IC    RE,NDLEN                                                         
         SHI   RE,(NDEMNO-NDELEM)                                               
         BNP   GETPDEM4                                                         
         SRL   RE,3                                                             
         LA    RF,NDEMNO                                                        
         L     R1,ADEMLST                                                       
         CLC   1(2,R1),1(RF)                                                    
         BE    *+16                                                             
         LA    RF,8(RF)                                                         
         BCT   RE,*-14                                                          
         B     GETPDEM4                                                         
         TM    4(RF),X'80'                                                      
         BO    GETPDEMX                                                         
*                                                                               
GETPDEM4 MVI   DEMFAC,0                                                         
*                                                                               
GETPDEMX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
         EJECT                                                                  
* GET MARKET WEIGHT                                                             
*                                                                               
MKTWGT   LR    R0,RE                                                            
         GOTO1 =V(SPOTMKWT),DMCB,SBLOCK                                         
         OC    SBMKTWGT,SBMKTWGT                                                
         BNZ   MKTWGTX                                                          
         TM    SBQDEMOP,SBQDOMWZ                                                
         BO    MKTWGTX                                                          
         MVC   SBMKTWGT,=F'1'                                                   
*                                                                               
MKTWGTX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* GET DAYPART TABLE                                                             
* INPUT  : SBDPTMEN = DAYPART MENU                                              
* OUTPUT : SBDPTTAB SET                                                         
*                                                                               
GETDPTTB LR    R0,RE                                                            
         OC    SBADPTTB,SBADPTTB   TEST DAYPART TABLES BUFFER                   
         BZ    GDX                                                              
         CLC   SBMED,SVMED         YES-TEST MEDIA OR DPT MENU CHANGE            
         BNE   *+14                                                             
         CLC   SBDPTMEN,SVDPTMEN                                                
         BE    GDX                                                              
         MVC   SVDPTMEN,SBDPTMEN   YES-GET DAYPART TABLE                        
         MVC   SVMED,SBMED                                                      
         LA    R1,ALPHATAB                                                      
         SR    RE,RE                                                            
*                                                                               
GD10     CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    RE,25                                                            
         B     GD20                                                             
         CLC   SBDPTMEN,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,GD10                                                          
         LPR   RE,RE                                                            
*                                                                               
GD20     MHI   RE,180                                                           
         L     R1,SBADPTTB                                                      
         LA    R1,0(RE,R1)                                                      
         XC    SBDPTTAB,SBDPTTAB                                                
         MVC   SBDPTTAB(180),0(R1)                                              
*                                                                               
GDX      LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO CALL USER REPORT OVERLAY                                           
*                                                                               
RPT2     LR    R0,RE                                                            
         STC   R1,RPMODE                                                        
         ICM   RF,15,ARPTNTRY                                                   
         BZ    RPT2X                                                            
         GOTO1 (RF),DMCB,(RC)                                                   
         CLI   RPMODE,RPSTOP       TEST STOP RETURNED                           
         BNE   RPT2X                                                            
         LTR   RE,R0               YES-CC NE                                    
         BR    RE                                                               
RPT2X    LR    RE,R0               NORMAL RETURN - CC EQ                        
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* AFFID HOOK                                                                    
* R1=A(SPDEMLK BLOCK)                                                           
*                                                                               
AFDHOOK  NTR1  ,                                                                
         USING SPDEMLKD,R1                                                      
         L     RF,SPLKAAFD         A(SPOT TABLE ENTRY)                          
         USING SPTTABD,RF                                                       
         LA    RE,AFDAREA                                                       
         LA    R0,MAXAFFID         LOOK FOR AFFID DATE/TIME IN AFFID            
*                                                              AREA             
AFDHK2   OC    0(L'AFDAREA,RE),0(RE)                                            
         BZ    AFDHK4                                                           
         CLC   SPTADATE,0(RE)                                                   
         BNE   *+14                                                             
         CLC   SPTATIME,2(RE)                                                   
         BE    AFDHKX                                                           
         LA    RE,L'AFDAREA(RE)                                                 
         BCT   R0,AFDHK2                                                        
         DC    H'0'                                                             
*                                  NOT FOUND-                                   
AFDHK4   MVC   0(2,RE),SPTADATE    ADD TO TABLE                                 
         MVC   2(2,RE),SPTATIME                                                 
         MVC   4(16,RE),SPLKPRG                                                 
         LA    RE,L'AFDAREA(RE)                                                 
         BCT   R0,*+8                                                           
         B     AFDHKX                                                           
         XC    0(L'AFDAREA,RE),0(RE)                                            
*                                                                               
AFDHKX   J     XIT                                                              
         DROP  R1,R3,R5,RF                                                      
         EJECT                                                                  
*                                                                               
PROCBL   NMOD1 0,**401B**                                                       
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
*                                                                               
         MVI   SBDPTCD,0                                                        
         MVI   MKTIND,FF                                                        
         CLI   SBQSPILL,C'S'                                                    
         BNE   *+8                                                              
         MVI   MKTIND,C'O'                                                      
         L     R3,SBAIO1                                                        
         USING STABUCKD,R3                                                      
*                                                                               
         CLI   SBQGETNM,C'Y'       TEST GET BUYER/BILLER NAMES                  
         BNE   BL2                 NO                                           
         MVC   DUB(2),STABKCLT     YES-                                         
         MVC   DUB+2(1),STABKPRD                                                
         MVC   DUB+3(1),STABKEST                                                
         MVC   DUB+4(2),STABKMKT                                                
         CLC   SVNAME,DUB          TEST CLT/PRD/EST/MKT CHANGE                  
         BE    BL2                 NO                                           
         MVC   SVNAME,DUB          YES-GET BUYER/BILLER NAMES                   
         GOTO1 AGETNAME                                                         
*                                                                               
BL2      LA    R5,STABELEM         SEARCH FOR ELEMNTS IN REQUEST PERIOD         
         USING STABELEM,R5                                                      
*                                                                               
BL10     CLI   0(R5),0                                                          
         BE    BLX                                                              
         CLI   0(R5),X'0E'                                                      
         BE    BL20                                                             
*                                                                               
BL15     LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     BL10                                                             
*                                                                               
BL20     CLC   STABPER,BILLST      CHECK PERIOD IS IN REQUEST PERIOD            
         BL    BL15                                                             
         CLC   STABPER,BILLEND                                                  
         BH    BL15                                                             
         OC    SBQBILST(4),SBQBILST  TEST FOR DATE OF BILLING DATES             
         BZ    BL22                                                             
         CLC   STABBDT,SBQBILST    YES-CHECK THEM                               
         BL    BL15                                                             
         CLC   STABBDT,SBQBILEN                                                 
         BH    BL15                                                             
*                                                                               
BL22     LA    R2,SBLOCK           GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   R2,SBECDAT-SBLOCK                                                
         OC    0(L'SBECDAT,R2),0(R2) CREATION DATE FILTER?                      
         BZ    BL23                NO                                           
         GOTO1 DATCON,DMCB,(3,0(R2)),(2,WORK)                                   
         CLC   STABBDT,WORK        CREATED BEFORE FILTERING DATE?               
         BL    BL15                YES                                          
*                                                                               
BL23     TM    SBEFLAG6,SBE6TGMY   MTRADE=Y?                                    
         BZ    *+12                NO                                           
         TM    STABSTAT,STABSTRD   TRADE BILLING FOR GROUP M?                   
         BZ    BL15                NO, REJECT THIS ELEMENT                      
         TM    SBEFLAG6,SBE6TGMN   MTRADE=N?                                    
         BZ    *+12                NO                                           
         TM    STABSTAT,STABSTRD   TRADE BILLING FOR GROUP M?                   
         BNZ   BL15                YES, REJECT THIS ELEMENT                     
                                                                                
         BRAS  RE,RDBFORM          READ BFORM REC NOW!                          
*                                                                               
         TM    SBEFLAG6,SBE6PXBL   REPORTING TRADE BILLING?                     
         BZ    BL35                NO - DON'T BOTHER                            
*                                                                               
         LR    R1,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   R1,4096                                                          
         USING SYSD+4096,R1                                                     
         XC    SBPXBILN,SBPXBILN   CLEAR PROGRAM EXCHANGE BILLED NET            
         XC    XBILLCST,XBILLCST   CLEAR PROGRAM EXCHANGE BILLED COST           
         DROP  R1                  DROP SBLOCK USING DSECT                      
*                                                                               
         TM    STABSTAT,STABSTRD   TRADE BILLING?                               
         BZ    BL35                NO                                           
         MVC   SVSTABNT,STABNET    SAVE STABNET                                 
         XC    STABNET,STABNET     CLEAR STABNET                                
         ICM   R1,15,STABGRS       HAVE GROSS AMOUNT?                           
         BZ    BL24                NO - CAN STILL HAVE CALCULATED NET           
         M     R0,=F'85'           MULTIPLY BY 85%                              
         LHI   RF,100              RF  = 100                                    
         DRNDR (R0),(RF)           GET 85% OF GROSS                             
         ST    R1,STABNET          BILLED EXCHANGE = 85% OF GROSS               
*                                                                               
BL24     LR    R2,R5               A(X'0E' ELEMENT)                             
         XR    R0,R0               CLEAR R0                                     
*                                                                               
BL25     CLI   0(R2),0             END OF RECORD?                               
         BE    BL31                YES                                          
         CLI   0(R2),X'E0'         TRADE ELEMENT?                               
         BE    BL30                YES                                          
BL26     IC    R0,1(R2)            ELEMENT LENGTH                               
         AR    R2,R0               BUMP TO NEXT ELEMENT                         
         B     BL25                KEEP CHECKING RECORD                         
*                                                                               
BL30     CLC   2(6,R5),2(R2)       SAME PERIOD/DATE/INVOICE NUM?                
         BNE   BL26                NO - KEEP CHECKING                           
         USING STABTRDE,R2         TRADE ELEMENT DSECT                          
         MVC   STABNET,STABTNET    CALCULATED NET                               
BL31     GOTO1 ABILLVAL,STABELEM   GET BILL VALUES                              
         LR    R1,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   R1,4096                                                          
         USING SYSD+4096,R1                                                     
         MVC   SBPXBILN,SBBILNET   PROGRAM EXCHANGE BILLED NET                  
         MVC   XBILLCST,BILLCOST   PROGRAM EXCHANGE BILLED COST                 
         DROP  R1,R2               DROP SBLOCK & TRADE USING DSECTS             
*                                                                               
BL34     MVC   STABNET,SVSTABNT    RESTORE STABNET                              
*                                                                               
BL35     GOTO1 ABILLVAL,STABELEM   GET BILL VALUES                              
*                                                                               
BL36     ST    R5,SBACURCH         USE CHUNK ADDRESS FOR ELEMENT ADDR           
*                                                                               
         TM    SBEFLAG7,SBE7COS2   IS THIS COST2?                               
         BNZ   *+16                YES - DON'T CHANGE MODES!                    
         CLI   STABKCUR,X'01'      THESE RECDS HAVE THE ADJUSTMENTS             
         BNE   *+8                                                              
         MVI   SBMODE,SBPROCB1     CHANGE THE MODE                              
         MVI   GMBILTYP,0                                                       
         TM    STABSTAT,STABSREG   REGIONAL BILLED?                             
         BZ    *+8                 NO                                           
         MVI   GMBILTYP,C'R'       YES - INDICATE FOR COLUMN FILTERS            
         TM    STABSTAT,STABSLMG   LMG (LOCAL MKT GRP) BILLED?                  
         BZ    *+8                 NO                                           
         MVI   GMBILTYP,C'L'       YES - INDICATE FOR COLUMN FILTERS            
*                                                                               
BLDRVLP  DS    0H                                                               
         GOTO1 ADRIVIN             PASS TO DRIVER INPUT                         
*                                                                               
         MVI   SBMODE,SBPROCCA     SET UP TO HANDLE CASH APPLIED ELMS           
*                                                                               
         TM    OPTIND4,OPTRPTDR    IF USER WANTS DRIVER CALLED AGAIN            
         BO    BLDRVLP                LOOP                                      
*                                                                               
         MVI   SBMODE,SBPROCBL     RETURN TO BILL PROCESSING                    
*                                                                               
         B     BL15                NEXT ELEMENT                                 
*                                                                               
BLX      J     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
PROCINVO NMOD1 0,**401I**                                                       
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
*                                                                               
         MVI   SBDPTCD,0                                                        
         MVI   SBBPRD,0                                                         
         MVI   SBBPRD2,0                                                        
         XC    SBPRD,SBPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVI   SBBEST,0                                                         
         MVC   SBEST,=C'000'                                                    
         MVI   MKTIND,FF                                                        
*        MVC   SBSREP,=C'000'      THIS ONLY GETS SET FOR THE HEADER!           
         CLI   SBQSPILL,C'S'                                                    
         BNE   *+8                                                              
         MVI   MKTIND,C'O'                                                      
         L     R3,SBAIO1                                                        
*                                                                               
* DON'T DELETE THIS CODE IN CASE WE EVER RESTORE OLD INVOICES 16MAY02           
*                                                                               
*&&DO                                                                           
         CLI   0(R3),X'0B'        OLD STYLE INVOICE RECORD?                     
         BNE   NV50                                                             
         B     NV50                NOP OLD INVOICE CODE                         
*                                                                               
         USING INVRECD,R3          YES                                          
         TM    INVCNTRL,X'C0'      RECORD MARKED AS DELETED?                    
         BO    NVX                 YES, IGNORE THIS RECORD                      
*                                                                               
         LA    R5,24(R3)                                                        
         MVI   ELEM,0                                                           
*                                                                               
NV2      CLI   0(R5),0             SEARCH RECORD FOR INVOICE ELEMENTS           
         BE    NVX                                                              
         CLI   0(R5),X'05'         HEADER ELEM?                                 
         BE    NV5                                                              
         CLI   0(R5),X'B1'                                                      
         BE    NV6                                                              
*                                                                               
NV4      LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     NV2                                                              
*                                                                               
         USING IHDELEM,R5                                                       
NV5      MVI   ELEM,X'0B'          FLAG OLD INVOICE                             
         MVC   ELEM+1(10),IHDID                                                 
         MVC   ELEM+11(3),IHDIDT                                                
         B     NV4                                                              
*                                                                               
         USING INVELEM,R5                                                       
NV6      CLI   SBQBPRD,0           TEST PRODUCT FILTER                          
         BE    NV8                                                              
         CLI   SBQBPRD,FF                                                       
         BE    NV8                                                              
         CLI   INVPRD,FF                                                        
         BE    NV8                                                              
         CLC   INVPRD,SBQBPRD                                                   
         BNE   NV4                                                              
         CLI   SBQBPRD2,0          TEST 2ND PRODUCT FILTER                      
         BE    NV8                                                              
         CLC   INVPRD2,SBQBPRD2                                                 
         BNE   NV4                                                              
*                                                                               
NV8      TM    INVSTAT,X'40'       TEST ESTIMATE IN THE ELEMENT                 
         BO    NV10                                                             
         CLI   SBQEST,1            NO-REJECT IF ESTIMATE FILTERING              
         BNE   NV4                                                              
         CLI   SBQESTND,255                                                     
         BNE   NV4                                                              
         CLC   SBQESFLT,BLANKS                                                  
         BH    NV4                                                              
         MVI   SVEST,0                                                          
         B     NV16                                                             
*                                                                               
NV10     L     R1,SBAESTTB                                                      
         LLC   RE,INVPRD           PRODUCT                                      
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         LLC   RF,INVPRD2          ESTIMATE                                     
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         AR    RE,R1                                                            
         CLI   0(RE),0             TEST ACTIVE                                  
         BE    NV4                                                              
         MVC   SVEST,INVPRD2       SET THE ESTIMATE                             
*                                                                               
NV16     CLC   SVEST,SBBEST        TEST ESTIMATE CHANGE                         
         BE    NV18                                                             
         MVC   SBBEST,SVEST                                                     
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
*                                                                               
NV18     MVI   SVPRD,FF            SET THE PRODUCT                              
         CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BE    NV20                                                             
         MVC   SVPRD,INVPRD                                                     
         CLI   SBQBPRD,0           TEST ALL PRODUCT REQUEST                     
         BE    NV20                                                             
         CLI   INVPRD,FF           NO-TEST INVOICE PRD=POL                      
         BNE   NV20                                                             
         MVC   SVPRD,SBQBPRD       YES-SET TO REQUESTED PRODUCT                 
*                                                                               
NV20     CLC   SVPRD,SBBPRD        TEST CHANGE OF PRODUCT                       
         BE    NV22                                                             
         MVC   SBBPRD,SVPRD        YES-SET PRODUCT DETAILS                      
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH                                                     
         MVC   SBPRDNM,PBNAME                                                   
         MVC   SBPRDINT,PBINT                                                   
         MVC   SBBPGR,PBGROUP      SET PRODUCT GROUP                            
         OC    SBBPGR,SBBPGR                                                    
         BNZ   NV22                                                             
         CLC   SBQPGRF,BLANKS      PRODUCT GROUP UNKNOWN                        
         BH    NV4                 IF PRDGRP FILTER, THEN IGNORE PRD            
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
NV22     MVI   SVPRD2,0            SET 2ND PRODUCT                              
         TM    INVSTAT,X'40'+X'08'                                              
         BNZ   *+10                                                             
         MVC   SVPRD2,INVPRD2                                                   
         CLC   SVPRD2,SBBPRD2      TEST CHANGE OF 2ND PRODUCT                   
         BE    NV24                                                             
         MVC   SBBPRD2,SVPRD2      YES-SET 2ND PRD DETAILS                      
         XC    SBPRD2,SBPRD2                                                    
         CLI   SBBPRD2,0                                                        
         BE    NV24                                                             
         LLC   RE,SBBPRD2                                                       
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD2,PBALPH                                                    
         DROP  R1                                                               
*                                                                               
NV24     ST    R5,SBACURCH         CALL DRIVER                                  
         GOTO1 ADRIVIN                                                          
*                                                                               
         CLC   SBQPRD,=C'POL'      TEST POL REQUEST                             
         BNE   NV4                                                              
         TM    SBQPIND,SBQPOLAL    AND SEPARATE POL AND INDIVIDUAL PRDS         
         BZ    NV4                                                              
         CLI   SBBPRD,FF           YES-THEN GENERATE POL RECORD                 
         BE    NV4                                                              
         MVI   SBBPRD,FF                                                        
         MVI   SBBPRD2,0                                                        
         MVC   SBPRD,=C'POL'                                                    
         LA    RE,254                                                           
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRDNM,PBNAME                                                   
         DROP  R1                                                               
         GOTO1 ADRIVIN                                                          
         B     NV4                                                              
*&&                                                                             
*                                                                               
NV50     DS    0H                                                               
         CLC   0(2,R3),=X'0E03'     NEW STYLE INVOICE RECORD?                   
         BE    *+6                                                              
         DC    H'0'                                                             
***                                                                             
* WE NEED TO EXTRACT THE INFO FROM THE LAST MINIO ELEMENT FOR THAT              
* INVOICE NUMBER, SO WE WILL BUFFER THE INVOICES THAT ARE BROKEN                
* UP, AND PROCESS THEM FIRST WHEN WE DO GET THE LAST INVOICE                    
***                                                                             
         USING SNVKEYD,R3                                                       
         CLC   SNVKMINK,=X'FFFFFFFFFFFF'  LAST INVOICE IN MINIO SET?            
         BE    NV50A                YES - START GETTING THEM                    
         GOTO1 PUTNINV              BUFFER INVOICE RECORD                       
         B     NVX                  AND EXIT                                    
*                                                                               
NV50A    LA    R5,SNVELS                                                        
         MVI   ELEM,X'0E'           SET NEW INV FLAG                            
         MVC   ELEM+1(10),SNVKINV                                               
         XC    ELEM+45(17),ELEM+45  CLEAR IN CASE WE DON'T HAVE I2 INFO         
*                                                                               
NV51     CLI   0(R5),0              SEARCH RECORD FOR INVOICE ELEMENTS          
         BE    NV53                                                             
         CLI   0(R5),X'E9'          I2 REQUEST DETAILS?                         
         BNE   NV51A                NO                                          
         USING SNVMTELD,R5                                                      
         MVC   ELEM+61(1),SNVMTACM  ASSIGNED COMMERCIAL MATCH                   
         DROP  R5                                                               
NV51A    CLI   0(R5),X'EA'          I2 MATCHING ELEM?                           
         BNE   NV52                 NO                                          
         USING SNVMRELD,R5                                                      
         MVC   ELEM+45(3),SNVMRFDT  DATE OF FIRST MATCH ATTEMPT                 
         MVC   ELEM+48(1),SNVMRFPC  FIRST MATCH - % MATCHED (1-100)             
         MVC   ELEM+49(3),SNVMRLDT  DATE OF LAST MATCH ATTEMPT                  
         MVC   ELEM+52(1),SNVMRLPC  LAST MATCH - % MATCHED- 100%=DONE           
         B     NV53                                                             
         DROP  R5                                                               
*                                                                               
NV52     LLC   R0,1(R5)             KEEP LOOKING FOR X'EA' ELEMENT              
         AR    R5,R0                                                            
         B     NV51                                                             
*                                                                               
NV53     MVI   BYTE,0               START WITH THE FIRST INVOICE                
         B     NV53B                                                            
*                                                                               
NV53A    LLC   R1,21(R3)            MINIO SET NUMBER                            
         AHI   R1,1                 BUMP TO SEE IF WE HAVE ANOTHER INV          
         STC   R1,BYTE                                                          
*                                                                               
NV53B    GOTO1 GETNINV              ANY BUFFERED INVOICE RECORDS?               
         BNE   NV54                 NO                                          
*                                                                               
         L     R3,NINVREC           PREVIOUS INV REC BUILT BY GETNINV           
         LA    R5,31(R3)            ELEMENTS START HERE                         
         LA    R3,1(R3)             FAKE INV KEY STARTS HERE                    
         CLI   SBMODE,SBSKIP        SKIP ENTIRE INVOICE SET?                    
         BE    NV200                YES, KEEP BUMPING MINIO SET NUMBER          
         B     NV60                                                             
*                                                                               
NV54     CLI   SBMODE,SBSKIP        SKIP ENTIRE INVOICE SET?                    
         BE    NVX                  YES, JUST DELETED THESE FROM TSAR           
         L     R3,SBAIO1                                                        
         LA    R5,SNVELS                                                        
*                                                                               
NV60     CLI   0(R5),0             SEARCH RECORD FOR INVOICE ELEMENTS           
         BE    NV200                                                            
         CLI   0(R5),X'10'                                                      
         BE    NV70                                                             
         CLI   0(R5),X'13'                                                      
         BE    NV75                                                             
         CLI   0(R5),X'30'                                                      
         BE    NV80                                                             
         CLI   0(R5),X'40'                                                      
         BE    NV100                                                            
*                                                                               
NV62     LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     NV60                                                             
*                                                                               
         USING SNVHDELD,R5                                                      
NV70     DS    0H                                                               
         LA    R2,SBLOCK           GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   R2,SBECDAT-SBLOCK                                                
         OC    0(L'SBECDAT,R2),0(R2) CREATION DATE FILTER?                      
         BZ    NV71                NO                                           
         GOTO1 DATCON,DMCB,(3,0(R2)),(2,WORK)                                   
         CLC   SNVHDCDT,WORK       CREATED BEFORE FILTERING DATE?               
         BL    NVSKIP              YES, SKIP ENTIRE INVOICE SET                 
*                                                                               
NV71     OC    INVQSREP,INVQSREP   SPECIAL REP FILTER?                          
         BZ    NV71B               NO                                           
         TM    OPTIND6,OPT6NREP    SPECIAL REP FILTER NEGATIVE?                 
         BZ    NV71A               NO                                           
         CLC   SNVHDREP,INVQSREP   REP MATCHES?                                 
         BE    NVSKIP              YES - SKIP                                   
         B     NV71B               NO - PROCESS THIS BUY                        
*                                                                               
NV71A    CLC   SNVHDREP,INVQSREP   REP MATCHES?                                 
         BNE   NVSKIP              NO - SKIP                                    
*                                                                               
NV71B    L     R0,AIO3             CLEAR CMML CODE TABLE                        
         LA    R1,15*255           ROOM FOR 255 12 CHAR CMMLS                   
         SR    RE,RE               AND 2 BYTE SEQ NUMBERS                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   SVEST,0                                                          
         MVI   SVESTHD,0                                                        
         MVI   SVPRD,0                                                          
         MVI   SVPRD2,0                                                         
*                                                                               
         MVC   ELEM+11(2),SNVHDIDT                                              
         MVC   ELEM+13(2),SNVHDDDT                                              
         MVC   ELEM+15(2),SNVHDTSP TOTAL SPOTS                                  
         ZAP   ELEM+17(8),SNVHDTCS TOTAL COST                                   
         MVC   ELEM+25(2),SNVHDSDT                                              
         MVC   ELEM+27(2),SNVHDCDT ACTIVITY DATE                                
         MVC   ELEM+29(12),SNVHDCON CONTRACT NUMBER                             
         MVC   ELEM+41(4),SNVHDEZS EASI SOURCE                                  
         MVC   SBSREP,SNVHDREP     SPECIAL REP OFF THE INVOICE                  
         MVC   INVSREP,SNVHDREP    SPECIAL REP OFF THE INVOICE                  
         OC    INVSREP,INVSREP     SPECIAL REP SET FROM INV RECORD?             
         BNZ   *+10                YES                                          
         MVC   INVSREP,=C'000'     DO NOT CALL RCPACK WITH NULLS!               
*                                                                               
         CLI   SNVHDPRD,0          TEST PRODUCT IN THE ELEMENT                  
         BE    NV72                                                             
         MVC   DUB(1),SNVHDPRD                                                  
         MVC   DUB+1(1),SNVHDPR2                                                
         BAS   RE,PRDFLT                                                        
         BNE   NVSKIP                                                           
         MVC   SVPRD,SNVHDPRD                                                   
         MVC   SVPRD2,SNVHDPR2                                                  
*                                                                               
NV72     CLI   SNVHDEST,0          TEST ESTIMATE IN THE ELEMENT                 
         BE    NV62                 NO                                          
         MVC   DUB+2(1),SNVHDEST                                                
         BAS   RE,ESTTEST                                                       
         BNE   NVSKIP                                                           
*                                                                               
         MVC   SVEST,SNVHDEST                                                   
         MVC   SVESTHD,SNVHDEST                                                 
         B     NV62                                                             
*                                                                               
NVSKIP   MVI   SBMODE,SBSKIP       SKIP ENTIRE INVOICE SET                      
*        B     NVX                                                              
         B     NV200               DELETE ANY BUFFERED INV RECORDS!             
*                                                                               
         USING SNVEOD,R5           EASI INVOICE ORIGIN ELEMENT                  
NV75     MVC   ELEM+53(8),SNVEOAID INVOICE AGENCY ID                            
         B     NV62                GET NEXT ELEMENT                             
*                                                                               
         USING SNVCMELD,R5                                                      
NV80     DS    0H                                                               
         LLC   RE,SNVCMICD                                                      
         BCTR  RE,0                                                             
         MHI   RE,15                                                            
         L     RF,AIO3                                                          
         AR    RE,RF                                                            
         MVC   0(12,RE),SNVCMCD                                                 
         MVC   12(2,RE),SNVCMSEQ                                                
         MVC   14(1,RE),SNVCMFLG      HDEF FLAG                                 
         B     NV62                                                             
*                                                                               
         USING SNVIDELD,R5                                                      
NV100    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,ELEM+25),(0,WORK)   CONVERT TO YYMMDD             
         LLC   R2,SNVIDDAY                        ADD THIS MANY DAYS            
         GOTO1 ADDAY,(R1),WORK,WORK,(R2)                                        
         CLC   WORK(6),SBQSTART    IS THIS BEFORE THE REQ START?                
         BL    NV62                YES                                          
         CLC   WORK(6),SBQEND      IS THIS AFTER THE REQ END?                   
         BH    NV62                YES                                          
         OC    ATIMRNG,ATIMRNG     ANY AFFID TIME FILTER?                       
         BZ    NV102                NO                                          
*                                                                               
         ICM   R0,3,SNVIDTIM                                                    
         SRDL  R0,32                                                            
         D     R0,=F'60'           CONVERT MINUTES FROM 6A TO HHMM              
         MHI   R1,100              HOURS X 100                                  
         AR    R0,R1               + MINUTES                                    
         AHI   R0,600              MILITARY TIME BASE IS 0, NOT 600             
*                                                                               
*        NOTE R0 IN TIME RANGE 0600 TO 3000 HOURS                               
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,ATIMRNG        FILT RANGE START TIME                        
         CHI   RE,0600             L.T. 6AM?                                    
         BNL   *+8                                                              
         AHI   RE,2400                                                          
         CR    R0,RE                                                            
         BL    NV62                OUTSIDE OF RANGE - NEXT CHUNK                
*                                                                               
         ICM   RE,3,ATIMRNG+2      FILT RANGE END TIME                          
         CHI   RE,0600             L.T. 6AM?                                    
         BNL   *+8                                                              
         AHI   RE,2400                                                          
         CR    R0,RE                                                            
         BH    NV62                OUTSIDE OF RANGE - NEXT CHUNK                
*                                                                               
NV102    MVC   DUB(1),SNVIDPRD     TEST PRODUCT FILTER                          
         MVC   DUB+1(1),SNVIDPR2                                                
         BAS   RE,PRDFLT                                                        
         BNE   NV62                                                             
*                                                                               
         ICM   RE,1,SNVIDEST       IF THERE'S AN ESTIMATE, USE IT               
         BNZ   *+8                                                              
         ICM   RE,1,SNVIDBES       ELSE TRY FOR THE MATCHED ESTIMATE            
         STC   RE,DUB+2                                                         
*                                                                               
         CLI   DUB+2,0             DID WE FIND A DETAIL/MATCHED EST?            
         BNE   *+10                YES                                          
         MVC   SVEST,SVESTHD       NO, WE MAY HAVE OVERWRITTEN SVEST            
*                                  WITH MATCHED EST & NEED TO RESTORE           
*                                                                               
         SR    R1,R1               RESET R1                                     
         BAS   RE,ESTTEST                                                       
         BNE   NV62                                                             
         LTR   R1,R1               R1 POINTING TO ESTIMATE BUFFER?              
         BZ    NV109               NO                                           
*                                                                               
         LR    R2,R1                                                            
         USING ESTBUFFD,R2                                                      
         LA    RF,SBLOCK                                                        
         USING SBLOCK,RF                                                        
         OC    SBSREP,SBSREP       SPECIAL REP SET FROM INV RECORD?             
         BNZ   NV109               YES                                          
         OC    EBSREP,EBSREP       IS THERE A SPECIAL REP ON THE EST?           
         BNZ   NV108               YES                                          
         MVC   SBSREP,=C'000'      DO NOT CALL RCPACK WITH NULLS!               
         B     NV109                                                            
*                                                                               
NV108    GOTO1 RCPACK,DMCB,(C'U',EBSREP),SBSREP                                 
         DROP  R2,RF                                                            
*                                                                               
NV109    ICM   RE,1,SNVIDEST       IF THERE'S AN ESTIMATE, USE IT               
         BNZ   *+12                                                             
         ICM   RE,1,SNVIDBES       ELSE TRY FOR THE MATCHED ESTIMATE            
         BZ    *+8                 DON'T SET 0!                                 
         STC   RE,SVEST            SET THE ESTIMATE                             
*                                                                               
NV110    CLC   SVEST,SBBEST        TEST ESTIMATE CHANGE                         
         BE    NV120                                                            
         MVC   SBBEST,SVEST                                                     
         LLC   RE,SBBEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBEST,DUB                                                        
         OI    SBEUDEF,SBEUECOM    NO PRD YET SO JUST SET FLAG!                 
*                                                                               
NV120    CLI   SBBEST,0            DO WE HAVE AN ESTIMATE?                      
         BNE   NV125               YES - OK TO GET UCOMMS                       
         NI    SBEUDEF,X'FF'-SBEUECOM  NO - DO NOT GET EST UCOMMS               
         L     RE,AUCOMTAB         A(UCOM TABLE)                                
         USING UCOMD,RE            UCOM DSECT                                   
         XC    UCDE1FLD(128),UCDE1FLD  IN CASE THIS WAS PREVIOUSLY SET          
         DROP  RE                  DROP UCOM DSECT                              
*                                                                               
NV125    DS    0H                  SET THE PRODUCT                              
         CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BNE   *+12                                                             
         MVI   SVPRD,FF                                                         
         B     NV130                                                            
*                                                                               
         OC    SNVIDPRD,SNVIDPRD                                                
         BZ    *+14                                                             
         MVC   SVPRD,SNVIDPRD                                                   
         MVI   SVPRD2,0                                                         
         CLI   SBQBPRD,0           TEST ALL PRODUCT REQUEST                     
         BE    NV130                                                            
         CLI   SNVIDPRD,FF         NO-TEST INVOICE PRD=POL                      
         BNE   NV130                                                            
         MVC   SVPRD,SBQBPRD       YES-SET TO REQUESTED PRODUCT                 
*                                                                               
NV130    CLC   SVPRD,SBBPRD        TEST CHANGE OF PRODUCT                       
         BE    NV140                                                            
         MVC   SBBPRD,SVPRD        YES-SET PRODUCT DETAILS                      
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH                                                     
         MVC   SBPRDNM,PBNAME                                                   
         MVC   SBPRDINT,PBINT                                                   
         MVC   SBBPGR,PBGROUP      SET PRODUCT GROUP                            
         LA    RE,SBLOCK                                                        
         USING SBLOCK,RE                                                        
         MVC   SBUP1FLD,PBUFLD1                                                 
         MVC   SBUP2FLD,PBUFLD2                                                 
         DROP  R1,RE                                                            
         OI    SBEUDEF,SBEUPCOM                                                 
*                                                                               
         OC    SBBPGR,SBBPGR                                                    
         BNZ   NV140                                                            
         CLC   SBQPGRF,BLANKS      PRODUCT GROUP UNKNOWN                        
         BH    NV62                IF PRDGRP FILTER, THEN IGNORE PRD            
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
NV140    TM    SBEUDEF,SBEUECOM+SBEUPCOM                                        
         BZ    *+8                                                              
         BRAS  RE,GETUCOM                                                       
*                                                                               
         TM    SBEUDEF,SBEUEST1+SBEUEST2  TEST ESTIMATE USER FIELDS             
         BZ    NV140A                     NEEDED                                
         GOTO1 GETESTNM                                                         
*                                                                               
NV140A   CLI   SBQBPRD,FF          TEST PRD=POL REQUEST                         
         BNE   *+12                                                             
         MVI   SVPRD2,FF                                                        
         B     NV150                                                            
*                                                                               
         OC    SNVIDPR2,SNVIDPR2                                                
         BZ    *+10                                                             
         MVC   SVPRD2,SNVIDPR2     SET 2ND PRODUCT                              
         CLC   SVPRD2,SBBPRD2      TEST CHANGE OF 2ND PRODUCT                   
         BE    NV150                                                            
         MVC   SBBPRD2,SVPRD2      YES-SET 2ND PRD DETAILS                      
         XC    SBPRD2,SBPRD2                                                    
         CLI   SBBPRD2,0                                                        
         BE    NV150                                                            
         LLC   RE,SBBPRD2                                                       
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD2,PBALPH                                                    
         DROP  R1                                                               
*                                                                               
NV150    DS    0H                                                               
         CLI   SNVIDNWK,0                                                       
         BE    NV160                                                            
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),SNVKSTA                                                
         OC    WORK+4(1),SNVIDNWK                                               
         GOTO1 MSUNPK,DMCB,(X'80',WORK),WORK+5,WORK+9                           
         MVC   SBCBLNET,WORK+14                                                 
NV160    ST    R5,SBACURCH         CALL DRIVER                                  
         GOTO1 ADRIVIN                                                          
*                                                                               
         CLC   SBQPRD,=C'POL'      TEST POL REQUEST                             
         BNE   NV62                                                             
         TM    SBQPIND,SBQPOLAL    AND SEPARATE POL AND INDIVIDUAL PRDS         
         BZ    NV62                                                             
         CLI   SBBPRD,FF           YES-THEN GENERATE POL RECORD                 
         BE    NV62                                                             
         MVI   SBBPRD,FF                                                        
         MVI   SBBPRD2,0                                                        
         MVC   SBPRD,=C'POL'                                                    
         LA    RE,254                                                           
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRDNM,PBNAME                                                   
         DROP  R1                                                               
         GOTO1 ADRIVIN                                                          
         B     NV62                                                             
*                                                                               
NV200    BCTR  R3,0                                                             
         C     R3,NINVREC          PROCESSING THE CURRENT INVOICE?              
         BE    NV53A               NO, GO AND PROCESS CURRENT INVOICE           
*                                                                               
NVX      J     XIT                                                              
         EJECT                                                                  
*                                                                               
* PRDFLT: INPUT DUB(1)=PRD, DUB+1(1)=PRD2                                       
PRDFLT   LR    R0,RE                                                            
         CLI   DUB,0                                                            
         BNE   *+10                                                             
         MVC   DUB(1),SVPRD                                                     
         CLI   DUB+1,0                                                          
         BNE   *+10                                                             
         MVC   DUB+1(1),SVPRD2                                                  
*                                                                               
         CLI   SBQPRD,0                                                         
         BE    PRDXEQ                                                           
         CLI   SBQBPRD,FF                                                       
         BE    PRDXEQ                                                           
         CLC   SBQPRD,=C'ALL'                                                   
         BE    PRDXEQ                                                           
         CLI   DUB,FF                                                           
         BE    PRDXEQ                                                           
         CLI   DUB,0               IS THERE A PRD                               
         BE    PRDXNEQ              NO - SKIP REC, SINCE SINGLE PRD REQ         
         CLC   SBQBPRD,DUB                                                      
         BNE   PRDXNEQ                                                          
PRD10    CLI   SBQBPRD2,0          TEST 2ND PRODUCT FILTER                      
         BE    PRDXEQ                                                           
         OC    DUB+1(1),DUB+1                                                   
         BZ    PRDXEQ                                                           
         CLC   SBQBPRD2,DUB+1                                                   
         BNE   PRDXNEQ                                                          
         B     PRDXEQ                                                           
*                                                                               
PRDXNEQ  LTR   RE,R0               CC NE                                        
         BR    RE                                                               
PRDXEQ   LR    RE,R0               NORMAL RETURN - CC EQ                        
         CR    RE,RE                                                            
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
* ESTTEST: INPUT DUB(1)=PRD DUB+2(1)=EST                                        
* ** BEWARE ** IF YOU EXPECT R1 TO POINT TO AN ESTIMATE BUFFER BE SURE          
* ** BEWARE ** YOU TEST FOR IT!     AKAT 11/30/04                               
ESTTEST  LR    R0,RE                                                            
         CLI   DUB+2,0             TEST ESTIMATE IN THE ELEMENT                 
         BNE   EST10                                                            
         CLI   SVEST,0             OR HEADER                                    
         BE    *+14                                                             
         MVC   DUB+2(1),SVEST                                                   
         B     EST10                                                            
         CLI   SBQEST,1            NO-REJECT IF ESTIMATE FILTERING              
         BNE   ESTXNEQ                                                          
         CLI   SBQESTND,255                                                     
         BNE   ESTXNEQ                                                          
         CLC   SBQESFLT,BLANKS                                                  
         BH    ESTXNEQ                                                          
         B     ESTXEQ                                                           
*                                                                               
EST10    LLC   RE,DUB              PRODUCT                                      
         CLI   SBQBPRD,X'FF'                                                    
         BNE   *+8                                                              
         LHI   RE,255                                                           
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         LLC   RF,DUB+2            ESTIMATE                                     
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)                                                       
         BZ    ESTXNEQ             EST INACTIVE                                 
         BCTR  R1,0                                                             
         MHI   R1,ESTBUFFL                                                      
         A     R1,SBAESTBF                                                      
         B     ESTXEQ                                                           
*                                                                               
ESTXNEQ  LTR   RE,R0               CC NE                                        
         BR    RE                                                               
ESTXEQ   LR    RE,R0               NORMAL RETURN - CC EQ                        
         CR    RE,RE                                                            
         BR    RE                                                               
         SPACE 2                                                                
         DROP  R3,R5                                                            
         EJECT                                                                  
PROCINFO NMOD1 0,**401O**                                                       
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R3,SBAIO1                                                        
         USING INFORECD,R3                                                      
         L     R1,SBAINFTB                                                      
         USING INFTABD,R1                                                       
         L     RE,4(R1)                                                         
         CLC   4(4,R1),8(R1)                                                    
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RF,1(RE)            NEW # OF ENTRIES                             
         ST    RF,4(R1)                                                         
         MHI   RE,INFTABL                                                       
         LA    R1,12(R1,RE)        R1 = A(NEW ENTRY)                            
         MVC   INFMKT,INFKMKT                                                   
         MVC   INFSTA,INFKSTA                                                   
         MVI   INFFLAG,0                                                        
*                                                                               
         SR    R0,R0                                                            
         LA    R3,INFEL                                                         
IN01     CLI   0(R3),X'05'                                                      
         BE    IN10                                                             
         CLI   0(R3),X'06'                                                      
         BE    IN20                                                             
         CLI   0(R3),X'07'                                                      
         BE    IN30                                                             
         CLI   0(R3),X'08'                                                      
         BE    IN40                                                             
         CLI   0(R3),X'09'                                                      
         BE    IN50                                                             
IN05     IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   IN01                                                             
         B     INX                                                              
*                                                                               
         USING INFCPOEL,R3                                                      
IN10     MVC   INFCPOS(24),INFCPO                                               
         B     IN05                                                             
*                                                                               
         USING INFRATEL,R3                                                      
IN20     MVC   INFRATE(24),INFRAT                                               
         B     IN05                                                             
*                                                                               
         USING INFCOSEL,R3                                                      
IN30     MVC   INFCOST(24),INFCOS                                               
         B     IN05                                                             
*                                                                               
         USING INFMSCEL,R3                                                      
IN40     MVC   INFMISC(8),INFMSCOR    INFMSCOR & INFMSCBU                       
         B     IN05                                                             
*                                                                               
         USING INFPRICE,R3                                                      
IN50     MVC   INFPRCE(24),INFPR                                                
         B     IN05                                                             
*                                                                               
INX      J     XIT                                                              
*                                                                               
         DROP  R1,R3                                                            
         EJECT                                                                  
PROCBHD  NMOD1 0,**401H**                                                       
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
*                                                                               
         XC    SBSTA,SBSTA                                                      
         MVC   SBSTA,=C'AAAAA'                                                  
         MVC   SBAFFIL,=C'ALL'                                                  
         XC    SBCHAN,SBCHAN                                                    
         MVI   SBCHAN,X'FF'                                                     
         OC    SBBMKT,SBBMKT                                                    
         BNZ   *+16                                                             
         XC    SBMKTNM,SBMKTNM                                                  
         MVC   SBBMKT,=X'FFFD'                                                  
         MVI   MKTIND,FF                                                        
         CLI   SBQSPILL,C'S'                                                    
         BNE   *+8                                                              
         MVI   MKTIND,C'O'                                                      
*                                 GET AOR DETAILS                               
         LA    R2,MAAORLK                                                       
         USING MAAORLKD,R2                                                      
         STCM  R2,15,AAORLK       SAVE ADDRESS                                  
*                                                                               
         MVC   MAAORABL,SBAIO1    ADDRESS OF BILL HEADER RECORD                 
         MVC   MAAORAIO,SBAIO3    IO AREA FOR MODULE                            
         MVC   MAAORACF,SBCOMFAC  ADDRESS OF COMFACS                            
*                                                                               
         GOTO1 =V(SPAORLK),DMCB,(R2)                                            
         DROP  R2                                                               
*                                                                               
         XC    SPBVALD(SPBVALDL),SPBVALD   GET BILL VALUES                      
         L     R2,SBAIO1                                                        
         GOTO1 SPBVAL,DMCB,(C'B',(R2)),SPBVALD                                  
         MVC   SBBILGRS,SPBVEGRS   EFFECTIVE GROSS                              
         MVC   SBBILNET,SPBVENET   EFFECTIVE NET                                
         MVC   SBBILTAX,SPBVETAX   EFFECTIVE TAX                                
         MVC   BILLCOST,SPBVACT    ACTUAL                                       
***      MVC   BILLGST,SPBVGST+1   GST                                          
         MVC   BILLGST,SPBVGST     GST                                          
         MVC   BILLPST,SPBVPST     PST                                          
         MVC   BILLHST,SPBVHST     HST                                          
         LA    R1,SBLOCK                                                        
         USING SBLOCK,R1                                                        
         ZAP   SBILGRSP,SPBVGRSP   EFFECTIVE GROSS PACKED                       
         ZAP   SBILNETP,SPBVNETP   EFFECTIVE NET PACKED                         
         DROP  R1                                                               
         ZAP   BILCOSTP,SPBVACTP   ACTUAL PACKED                                
*                                                                               
         GOTO1 ADRIVIN                                                          
BHX      J     XIT                                                              
         EJECT                                                                  
* ROUTINE TO EXTRACT PRODUCT DETAILS FROM PRODUCT BUFFER                        
* INPUT  : SBBPRD = PRODUCT CODE                                                
* OUTPUT : CC EQ - OK                                                           
*          CC NE - THE PRODUCT GROUP IS UNKNOWN AND THERE IS PRODUCT            
*                  GROUP FILTERING                                              
*                                                                               
EXTPRD   NTR1  BASE=*,LABEL=*                                                   
         USING GEND,RC                                                          
         USING SYSD,R9                                                          
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH        ALPHA PRODUCT                                
         MVC   SBPRDNM,PBNAME      PRODUCT NAME                                 
         MVC   SBPRDINT,PBINT      PRODUCT INTERFACE CODE                       
         LA    RE,SBLOCK                                                        
         USING SBLOCK,RE                                                        
         MVC   SBUP1FLD,PBUFLD1                                                 
         MVC   SBUP2FLD,PBUFLD2                                                 
         DROP  RE                                                               
         OI    SBEUDEF,SBEUPCOM    EXTRACT UCOM DATA                            
         BRAS  RE,GETUCOM                                                       
         MVC   SBBPGR,PBGROUP      SET PRODUCT GROUP                            
         OC    SBBPGR,SBBPGR                                                    
         BNZ   EXTPRDY                                                          
         CLC   SBQPGRF,BLANKS      PRODUCT GROUP UNKNOWN                        
         BH    EXTPRDN             IF PRDGRP FILTER, THEN RETURN ERROR          
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
EXTPRDY  J     XITEQ                                                            
*                                                                               
EXTPRDN  J     XITNE                                                            
*=================================================================*             
* GET USER COMMENT DATA                                           *             
*   SET SBEUDEF TO SBEUPCOM, SBEUECOM, OR SBEUMCOM                *             
*   NOTE: BECAUSE OF HIERARCHY, GET MCOM ANY TIME PCOM OR ECOM    *             
*         CHANGES. IOW, ALWAYS GET MCOM!                          *             
*=================================================================*             
         SPACE 1                                                                
         USING GEND,RC                                                          
         USING SYSD,R9                                                          
GETUCOM  NTR1  BASE=*,LABEL=*                                                   
         TM    DATAIND9,DIUCOM     NEED USER COMMENT DATA?                      
         BZ    GTUCX                NO                                          
         OC    SBBCLT,SBBCLT       IF WE DON'T HAVE CLT YET, GET OUT            
         BZ    GTUCX                                                            
*                                                                               
         XC    UCWORK,UCWORK                                                    
         LA    RF,UCWORK                                                        
         USING DDUCOMD,RF                                                       
         MVC   UCACOMF,ACOMFACS                                                 
         MVI   UCSYS,C'S'                                                       
         MVC   UCSAM,SBBAGYMD                                                   
         MVC   UCSCLT,SBBCLT                                                    
         MVC   UCPRD,=C'POL'                                                    
         CLC   SBPRD,=X'FEFEFE'                                                 
         BE    GTUC05                                                           
         TM    SBEFLAG5,SBE5CPOL   UCOM=POL?                                    
         BNZ   GTUC05              YES - UCPRD=POL                              
         MVC   UCPRD,SBPRD                                                      
GTUC05   MVC   UCSEST,SBBEST                                                    
         MVC   UCMKT,SBBMKT                                                     
*                                                                               
         TM    SBEUDEF,SBEUPCOM                                                 
         BZ    *+8                                                              
         OI    UCOPT,UCOPRD        EXTRACT PRD UCOMS                            
*                                                                               
         CLI   SBBEST,0            HAVE AN ESTIMATE?                            
         BE    GTUC06              NO! DON'T TRY TO EXTRACT UCOMM DATA!         
         TM    SBEUDEF,SBEUECOM                                                 
         BZ    *+8                                                              
         OI    UCOPT,UCOEST        EXTRACT EST UCOMS                            
         TM    DTAIND11,DIUCOM8    EXTRACT ESTIMATE UCOMM 5-8                   
         BZ    *+8                 NO                                           
         OI    UCOPT,UCO8EST       YES - EST UCOMM 5-8 RETURNED IN PRD          
*                                                                               
GTUC06   OC    SBBMKT,SBBMKT       NOT IF WE DON'T HAVE A MKT!                  
         BZ    *+8                                                              
         OI    UCOPT,UCOMKT        ALWAYS EXTRACT MKT UCOMS                     
*                                                                               
         GOTO1 VDDUCOM,UCWORK                                                   
         LA    RF,UCWORK                                                        
         CLI   UCERROR,0                                                        
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         L     RE,AUCOMTAB                                                      
         USING UCOMD,RE                                                         
*                                                                               
         TM    UCOPT,UCO8EST       WANTED EST UCOMMS 5-8?                       
         BNZ   GTUC07              YES - EXTRACT TO UCDP1FLD                    
         TM    SBEUDEF,SBEUPCOM    TEST EXTRACT PRD UCOM                        
         BZ    GTUC10               NO                                          
         XC    UCDP1FLD(128),UCDP1FLD                                           
         TM    UCDATA,UCDNOPRD     NO PRD LEVEL UCOM REC                        
         BNZ   GTUC10                                                           
GTUC07   L     R1,UCPDATA                                                       
         MVC   UCDP1FLD(128),0(R1) EXTRACT PRD LEVEL UCOMS                      
*                                                                               
GTUC10   TM    SBEUDEF,SBEUECOM    TEST EXTRACT EST UCOM                        
         BZ    GTUC20               NO                                          
         XC    UCDE1FLD(128),UCDE1FLD                                           
         TM    UCDATA,UCDNOEST     NO EST LEVEL UCOM REC                        
         BNZ   GTUC20                                                           
         L     R1,UCEDATA                                                       
         MVC   UCDE1FLD(128),0(R1) EXTRACT EST LEVEL UCOMS                      
*                                                                               
GTUC20   XC    UCDM1FLD(128),UCDM1FLD                                           
         TM    UCDATA,UCDNOMKT     NO MKT LEVEL UCOM REC                        
         BNZ   GTUC30                                                           
         ICM   R1,15,UCMDATA                                                    
         BZ    GTUC30                                                           
         MVC   UCDM1FLD(128),0(R1) EXTRACT MKT LEVEL UCOMS                      
*                                                                               
GTUC30   NI    SBEUDEF,X'FF'-SBEUPCOM-SBEUECOM-SBEUMCOM   RESET FLAG            
*                                                                               
GTUCX    J     XIT                                                              
         DROP  R9,RE                                                            
UCWORK   DS    CL(UCOMDLNQ)                                                     
         EJECT                                                                  
***********************************************************************         
* GET CURRENT BILLING PCT                                                       
***********************************************************************         
         USING GEND,RC                                                          
         USING SYSD,R9                                                          
         USING SCHUNKD,R5                                                       
GETBILL  NTR1  BASE=*,LABEL=*                                                   
         XC    BILLPCT,BILLPCT                                                  
         LR    RF,R9                                                            
         AHI   RF,(SBQREAD2-SYSD)                                               
         TM    0(RF),SBQRD2BP      READ BPCT RECORDS?                           
         BZ    GBXIT               NOPE                                         
         ICM   R2,15,SBACHKTB      HAVE A(CHECK TABLE)                          
         BZ    GBXIT               NOPE                                         
         XC    BILLPCT,BILLPCT                                                  
         GOTO1 DATCON,DMCB,(2,SCDATE),(0,WORK)                                  
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,FULL)                                 
*                                                                               
         L     RE,4(R2)                                                         
         ST    RE,DMCB+8           N'ENTRIES IN TABLE                           
         LA    RE,BPCTTABL                                                      
         ST    RE,DMCB+12          L'ENTRY                                      
         L     RE,8(R2)                                                         
         ST    RE,DMCB+20          MAX NUMBER OF ENTRIES                        
         LA    R2,12(R2)           A(TABLE)                                     
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING BPCTTABD,RE                                                      
         MVC   BPCTAM,SBBAGYMD     A/M                                          
         MVC   BPCTCLT,SBBCLT      CLIENT                                       
         MVC   BPCTPRD,SBBPRD      PRODUCT                                      
         MVC   BPCTEST,SBBEST      ESTIMATE                                     
         MVC   BPCTMON,FULL        YY/MM                                        
*                                                                               
         GOTO1 SBABINSR,DMCB,WORK,(R2),,,(0,7)                                  
         CLI   0(R1),1                                                          
         BE    GBXIT                                                            
         ICM   RE,15,0(R1)                                                      
         BZ    GBXIT                                                            
         MVC   BILLPCT,BPCTPCT                                                  
         DROP  RE                                                               
GBXIT    J     XIT                                                              
***********************************************************************         
* GET CURRENT BILLING PCT                                                       
***********************************************************************         
         USING GEND,RC                                                          
         USING CONHEADH-64,R7                                                   
RDBFORM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R2,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   R2,4096                                                          
         USING SYSD+4096,R2                                                     
         XC    SBBFORM,SBBFORM     CLEAR BILL FORMULA IN CASE OF EXIT           
         TM    SBEFLAG5,SBE5BFRM   GET BILL FORM RECORD?                        
         BZ    RDBX                NO                                           
         CLI   SBB1XPRF+11,C'N'    GET BILL FORMULA?                            
         BE    RDBX                NO                                           
         ICM   R4,15,AGETBFRW      HAVE A(GETBFRW)?                             
         BZ    RDBX                NO - EXIT                                    
         USING SPGBFRDD,R4                                                      
         XC    SPGBAM(SPGBRTRN-SPGBAM),SPGBAM                                   
*                                                                               
         MVC   SPGBAM,SBBAGYMD     A/M                                          
         MVC   SPGBCLT,SBBCLT      CLIENT                                       
         MVC   SPGBPRD,SBPRD       3 CHAR PRD CODE                              
         MVC   SPGBEST,SBBEST      EST                                          
         MVC   SPGBMKT,SBBMKT      MKT (NOTE- MGRS NOT DONE YET)                
*                                                                               
         CLI   SBB1XPRF+12,C'Y'    SFM BILL FORMS WITH EFF DATE?                
         BNE   RDBF6               NO                                           
*                                                                               
         USING SCHUNKD,R5                                                       
         LA    R6,SCDATE           BUY CHUNK DATE                               
         CLI   SBMODE,SBPROCSP     PROCESSING A BUY RECORD?                     
         BE    RDBF3               YES                                          
         DROP  R5                                                               
*                                                                               
         USING STABELEM,R5                                                      
RDBF1    LA    R6,STABBDT          DATE OF BILLING                              
         CLI   DATEOPT,DOBILL      DATE=BD?                                     
         BE    RDBF3               YES-USE BILLING DATE                         
*                                                                               
         L     R1,ABILMNTH         NO-EXTRACT BILLING PERIOD START DATE         
         LA    R0,NBILMNTH            FROM BILLING MONTHS TABLE                 
*                                                                               
RDBF2    OC    0(6,R1),0(R1)       HAVE BILLING PERIOD?                         
         BZ    RDBX                NO - NO BFORM                                
         CLC   STABPER,0(R1)       MATCH ON BILLING PERIOD?                     
         BNE   *+12                NO                                           
         LA    R6,2(R1)            YES - POINT TO BILLING PER STRT DATE         
         B     RDBF3               AND LOOK IT UP IN MOS TABLE                  
         LA    R1,6(R1)            BUMP TABLE ENTRY                             
         BCT   R0,RDBF2            LOOK AT NEXT MONTH'S DATES                   
         B     RDBX                NO BFORM                                     
         DROP  R5                                                               
*                                                                               
RDBF3    L     R1,AMONTHS          MONTH TABLE                                  
*                                                                               
RDBF4    OC    0(4,R1),0(R1)       HAVE A MONTH ENTRY                           
         BZ    RDBX                NO - NO BFORM                                
         CLC   0(2,R6),0(R1)       BEFORE REQUEST PERIOD?                       
         BL    RDBX                YES - NO BFORM                               
         CLC   0(2,R6),2(R1)       LESS THAN OR EQUAL TO MOS?                   
         BNH   RDBF5               YES - USE THIS MOS                           
         LA    R1,4(R1)            BUMP MONTH TABLE ENTRY                       
         B     RDBF4               CHECK NEXT MONTH                             
*                                                                               
RDBF5    MVC   SPGBMOS,0(R1)       REQUEST MONTH                                
         CLI   PEROPT,PEROCAL      USING CALENDAR MONTHS?                       
         BE    RDBF5A              YES - USE THIS DATE                          
*                                                                               
         GOTO1 =V(BRDMON),DMCB,(X'FF',SPGBMOS),SPGBMOS                          
*                                                                               
RDBF5A   GOTO1 DATCON,DMCB,(2,SPGBMOS),(3,DUB)                                  
         MVC   SPGBMOS,DUB                                                      
*                                                                               
RDBF6    OC    SVBFKEY,SVBFKEY     FIRST TIME THROUGH?                          
         BZ    *+14                YES                                          
         CLC   SVBFKEY,SPGBAM      SAME KEY AS LAST TIME?                       
         BE    RDBF8               YES - BFORMULA STILL IN SPGBFORM             
         MVC   SVBFKEY,SPGBAM      SAVE OFF THE KEY                             
*                                                                               
         MVC   SPGBACOM,ACOMFACS   A(COMFACS)                                   
*                                                                               
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         MVC   SPGBLODR,MCVLOADR   A(LOADER)                                    
         DROP  R1                                                               
*                                                                               
         GOTO1 =V(SPGETBFR),DMCB,SPGBFRD                                        
*                                                                               
RDBF8    MVC   SBBFORM,SPGBFORM    BILL FORMULA                                 
         DROP  R2,R4,R7,RC                                                      
*                                                                               
RDBX     J     XIT                                                              
*                                                                               
         USING GEND,RC                                                          
         USING CONHEADH-64,R7                                                   
         USING SYSD,R9                                                          
*                                                                               
ADDMLREQ NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,ELEM             R3 = ELEM                                    
         XC    ELEM(26),ELEM       CLEAR ELEM FOR 26 BYTES                      
         USING REQHDRD,R3          26 BYTE REQ HEADER                           
         MVC   REQORIG,TWAORIG     ORIGIN                                       
         MVC   REQDEST,TWADEST     DESTINATION                                  
         MVI   REQNUMB,128         THIS IS FROM RT128 IN SPREQ00                
         CLI   REQSML,C'C'         COMSCORE REQUEST?                            
         BNE   *+8                 NO                                           
         MVI   REQCTRL,1           YES - SET ML REQUEST TO COMSCORE             
         DROP  R3                  DROP REQHDRD USING                           
                                                                                
         LA    R3,ELEM+26          BUILD REQUEST CARD HERE                      
         USING FQRECORD,R3         SPONSOR STYLE REQUEST CARD DSECT             
         MVC   0(80,R3),BLANKS     INIT TO SPACES                               
         MVC   FQCODE,=C'ML'       ML REQUEST                                   
         MVC   FQAGY,SBQAGY        AGENCY                                       
         MVC   FQMED,SBQMED        MEDIA                                        
         MVC   FQCLT,SBQCLT        CLIENT                                       
         MVC   FQPRD,SBQPRD        PRODUCT                                      
         MVC   FQMKT,SBQMKT        MARKET                                       
         OC    FQMKT,FQMKT         MARKET SET?                                  
         BNZ   *+10                YES                                          
         MVC   FQMKT,=C'ALL '      NO - ALL MARKET REQUEST                      
         OC    FQMKT,BLANKS        SPACE PAD                                    
*                                                                               
         EDIT  (B1,SBQEST),(3,FQEST),FILL=0                                     
*                                                                               
         MVC   FQSTART,SBQREQST    START DATE                                   
         MVC   FQEND,SBQREQND      END DATE                                     
         TM    FLAGS,FLGLOCK       ALSO LOCK GOALS IN ML?                       
         BZ    *+8                 NO                                           
         MVI   FQOPT1,C'Y'         YES - THIS IS GLOCK=Y IN ML                  
         TM    FLAGS,FLGONLY       ONLY LOCK GOALS IN ML?                       
         BZ    *+8                 NO                                           
         MVI   FQOPT1,C'G'         YES - THIS IS GLOCK=ONLY IN ML               
         MVC   FQUESTOR(11),=C'AUTO ML REQ'                                     
         DROP  R3                  DROP FQRECORD USING                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',ELEM,ELEM                     
*                                                                               
         CLI   8(R1),0             ANY ERROR?                                   
         BE    *+6                 NO                                           
         DC    H'0'                YES - NO ERRORS TOLERATED                    
*                                                                               
         J     XIT                                                              
*                                                                               
         USING GEND,RC                                                          
         USING SYSD,R9                                                          
*                                                                               
SETSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,SBMKTREC         MARKET RECORD                                
         USING MKTRECD,RF          MARKET RECORD DSECT                          
         MVI   BYTE,C'1'           SET THE RATING SERVICE TO ARBITRON           
         CLI   SBCPROF+3,C'1'      CLIENT PROFILE INDICATES ARBITRON?           
         BE    *+8                 YES                                          
         MVI   BYTE,C'0'           NO - SET TO NIELSEN                          
         LA    R4,MKTRSM1          DETERMINE RATING SERVICE MKT NUM             
         CLC   MKTRS1,BYTE         RATING SERVICE MATCHES?                      
         BE    SSTA00              YES                                          
         LA    R4,MKTRSM2          DETERMINE RATING SERVICE MKT NUM             
         CLC   MKTRS2,BYTE         RATING SERVICE MATCHES?                      
         BNE   SSTANEQ             NO                                           
         DROP  RF                  DROP MARKET RECORD USING                     
*                                                                               
SSTA00   XR    R0,R0               CLEAR R0                                     
         ICM   R0,3,0(R4)          GET MKT NUM                                  
         CVD   R0,DUB              CONVERT TO DECIMAL                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  SBSTA(4),DUB        UNPACK                                       
         MVC   SBSTA+4(1),SBMED    SET MEDIA SUFFIX USING SBMED                 
         CLI   SBQMED,C'*'         MEDIA '*' REQUEST?                           
         BE    *+10                YES - USE SBMED                              
         MVC   SBSTA+4(1),SBQMED   SET MEDIA SUFFIX USING SBQMED                
*                                                                               
*&&DO                                                                           
         L     R4,AIO2             I/O AREA                                     
         USING STARECD,R4          STATION RECORD DSECT                         
         XC    STAKEY,STAKEY       CLEAR KEY                                    
         MVI   STAKTYPE,STKKTYPQ   K-PASSIVE                                    
         MVC   STKKAGY,SBQAGY      AGENCY                                       
         LA    R5,SBMED            MEDIA = SBMED                                
         CLI   SBQMED,C'*'         MEDIA '*' REQUEST?                           
         BE    *+8                 YES - USE SBMED                              
         LA    R5,SBQMED           NO - USE SBQMED                              
         MVC   STKKMED,0(R5)       MEDIA                                        
         MVC   STKKMKT,SBBMKT      MARKET                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',(R4),(R4)                    
         B     SSTA10                                                           
*                                                                               
SSTA05   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'STATION',(R4),(R4)                    
*                                                                               
SSTA10   CLI   STAKTYPE,STKKTYPQ   K-PASSIVE?                                   
         BNE   SSTANEQ             NO, DONE                                     
         CLC   STKKAGY,SBQAGY      SAME AGENCY?                                 
         BNE   SSTANEQ             NO, DONE                                     
         CLC   STKKMED,0(R5)       SAME MEDIA?                                  
         BNE   SSTANEQ             NO, DONE                                     
         CLC   STKKMKT,SBBMKT      SAME MARKET?                                 
         BNE   SSTANEQ             NO, DONE                                     
         OC    STKKCLT,STKKCLT     CLIENT SPECIFIC RECORD?                      
         BNZ   SSTA05              YES - READ NEXT RECORD                       
*                                                                               
         MVC   SBSTA,STKKSTA       SET STATION FOR UNIVERSE                     
         DROP  R4                  DROP STATION RECORD USING                    
*&&                                                                             
SSTAEQU  CR    RB,RB               SET CC EQU                                   
         J     XIT                 EXIT                                         
SSTANEQ  LTR   RB,RB               SET CC NEQ                                   
         J     XIT                 EXIT                                         
*                                                                               
         USING GEND,RC                                                          
         USING SYSD,R9                                                          
PURPTAB  NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,PURPTABQ         MAX NUMBER OF PURPTAB ENTRIES                
         L     R3,APURPTAB                                                      
         XC    KEY,KEY             READ PURP CODE RECORDS                       
         LA    R2,KEY                                                           
         USING PRPRECD,R2                                                       
         MVI   PRPKTYP,PRPKTYPQ                                                 
         MVI   PRPKSUB,PRPKSUBQ                                                 
         MVC   PRPKAGY,SBQAGY                                                   
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
         GOTO1 HIGH                                                             
*                                                                               
PT10     CLC   KEY(4),KEYSAVE      0D19 & AGYQ                                  
         BNE   PTX                                                              
         GOTO1 GETREC                                                           
         MVC   0(1,R3),PRPKMED                                                  
         MVC   1(6,R3),PRPCODE                                                  
         MVC   7(30,R3),PRPELTXT                                                
         AHI   R3,PURPTABL                                                      
         BCT   R0,PT20                                                          
         DCHO                                                                   
*                                                                               
PT20     GOTO1 SEQ                                                              
         B     PT10                                                             
*                                                                               
PTX      J     XIT                                                              
         DROP  R2,R9                                                            
         EJECT                                                                  
*=================================================================*             
* RP2GET - READ IN REPORT2/CONTINUE REPORT                        *             
*=================================================================*             
         SPACE 1                                                                
         USING WORKD,R6                                                         
         USING CONHEADH-64,R7                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
         USING GETTXTD,GETTXTCB                                                 
*                                                                               
RP2GET   NTR1  BASE=*,LABEL=*                                                   
         OC    GTMSGNO,GTMSGNO     ARE WE HERE FOR AN ERROR?                    
         BNZ   RP2ERR                                                           
*                                                                               
         USING CT01RECD,R4                                                      
         XC    KEY,KEY             ESTABLISH PROGRAM RECORD KEY                 
         LA    R4,KEY                                                           
         MVI   CT01TYPE,CT01TYPQ   SET RECORD TYPE                              
         MVC   CT01AGID,AGYALPHA   SET AGENCY ID                                
         MVI   CT01SYS,2           SET FOR SPOT SYSTEM                          
         MVI   CT01PRG,4           SET FOR SPOT WRITER                          
         MVI   CT01PHAS,1          SET FOR PHASE 1                              
         MVC   CT01NAME,RPT2ID     SET SECOND REPORT ID                         
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO3                                                         
         MVC   FILENAME,=CL8'CTFILE'  SET FILE NAME                             
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         GOTO1 READ                READ IN PROGRAM RECORD                       
         CLC   KEY(L'CT01KEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                SHOULD ALREADY HAVE BEEN VALIDATED           
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         STC   R0,USEIO                                                         
*                                                                               
         TM    OPTIND5,OPT5CONT    IF NOT A CONTINUATION, DON'T SAVE            
         BZ    RP2G05                                                           
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    RP2G01              YES - TEMPSTR CANNOT BE USED OFFLINE         
***                                                                             
* USE TEMPSTR FOR CONTINUE OPTION AND RLP IF ONLINE SINCE TIA IS USED           
***                                                                             
         MVI   DMCB+8,2            PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',,(R7),0                      
*                                                                               
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    RP2G05                                                           
         DC    H'0'                                                             
*                                                                               
RP2G01   L     R0,ATIA             SAVE OFF SCREEN                              
         LHI   R1,WRIWORK-T204FFD                                               
         LR    RF,R1                                                            
         LR    RE,R7                                                            
         MVCL  R0,RE                                                            
*                                                                               
RP2G05   MVC   WRINAM,RPT2ID                                                    
         MVC   SVDISP,DATADISP                                                  
         MVC   DATADISP,=H'28'                                                  
         IC    R0,MODE                                                          
         MVI   MODE,DISPREC                                                     
         XC    DMCB+4(4),DMCB+4                                                 
         TM    OPTIND6,OPT6RP2M    REPORT2M OPTION?                             
         BNZ   RP2G10              YES - LET GEGENPRG START FROM MEDIA          
         TM    OPTIND5,OPT5CONT    SKIP IF CONTINUATION OF A REPORT             
         BNZ   *+12                                                             
         MVI   DMCB+4,C'F'         TELL GENPRG TO FILL FROM FIELD #             
         MVI   DMCB+7,30           FIELD # TO FILL FROM                         
*                                                                               
RP2G10   GOTO1 GENPROG,DMCB,(RC)                                                
         STC   R0,MODE                                                          
         MVC   DATADISP,SVDISP                                                  
*                                                                               
         MVC   SVERREX,ERREX       FORCE ERROR EXIT TO THIS ROUTINE             
         LA    R1,RP2GET                                                        
         ST    R1,ERREX                                                         
*                                                                               
         TM    OPTIND5,OPT5CONT    SKIP IF CONTINUATION OF A REPORT             
         BZ    RP2X                                                             
         LA    R2,WRICOLSH         ADD COLS TO REPORT                           
         MVI   MAX,MAXCOLS                                                      
         GOTO1 VALCOLS                                                          
*                                                                               
RP2X     MVC   ERREX,SVERREX       RESTORE VALUABLE ADDRESS                     
         MVC   AIO,AIO1                                                         
         TM    OPTIND5,OPT5CONT    IF NOT A CONTINUATION, JUST EXIT             
         BZ    RP2XX                (LEAVING NEW TWA)                           
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    RP2X1               YES - TEMPSTR CANNOT BE USED OFFLINE         
*                                                                               
         MVI   DMCB+8,2            PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R7),0                      
*                                                                               
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    RP2XX                                                            
         DC    H'0'                                                             
*                                                                               
RP2X1    L     RE,ATIA             AND SCREEN                                   
         LHI   R1,WRIWORK-T204FFD                                               
         LR    RF,R1                                                            
         LR    R0,R7                                                            
         MVCL  R0,RE                                                            
*                                                                               
RP2XX    J     XIT                                                              
*                                                                               
RP2ERR   OI    GENSTAT2,USGETTXT                                                
         MVC   GTMSGNO,=Y(BADCONT)                                              
         MVC   ERREX,SVERREX       RESTORE VALUABLE ADDRESS                     
         GOTO1 ERREX                                                            
*                                                                               
SVERREX  DS    A                                                                
*                                                                               
***                                                                             
* PROCESS OM RECORD                                                             
***                                                                             
PROCOMG  NMOD1 0,**401M**                                                       
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
         CLI   SBMODE,SBPROCOM     PROCESSING AN OM RECORD?                     
         BNE   *+10                NO, OMREC ALREADY SET                        
         MVC   OMREC,SBAIO1        YES - OM RECORD IN SBAIO1                    
*                                                                               
         MVI   OMFLAGS,0           CLEAR OM FLAG                                
         MVI   OMTYPE,C'C'         CASH ORDER                                   
         L     R3,OMREC                                                         
         MVI   ELCODE,DOSPELQ      SUPPLEMENTARY ID ELEMENT                     
         BRAS  RE,GETEL            HAVE ONE?                                    
         BNE   POM05               NO                                           
         USING DOSPELD,R3                                                       
         TM    DOSPFLG1,DOSPTRDE   TRADE ORDER?                                 
         BZ    *+8                 NO                                           
         MVI   OMTYPE,C'T'         YES - INDICATE TRADE ORDER                   
         CLI   DOSPREVN,0          GOT A REVISION NUMBER?                       
         BE    *+8                 NO                                           
         OI    OMFLAGS,MF1REVOR    YES, REVISED ORDER                           
         TM    DOSPFLG1,DOSPCFCM   CONFIRM WITH COMMENTS?                       
         BZ    *+8                                                              
         OI    OMFLAGS,MF1CNFCM    YES, ORDER IS CONFIRMED W/COMMENTS           
         CLI   OMDEST,0            HAVE A DESTINATION FILTER                    
         BE    *+14                NO                                           
         CLC   OMDEST,DOSPDEST     MATCH ON DESTINATION?                        
         JNE   POMX                NO, DONE                                     
         CLI   OMPERSON,0          HAVE A PERSON FILTER                         
         BE    POM05                                                            
         MVI   BYTE,C'S'                                                        
         TM    DOSPFLG1,DOSPPPER   HAVE A POINTPERSON?                          
         BZ    *+8                 NO                                           
         MVI   BYTE,C'P'                                                        
         CLC   OMPERSON,BYTE       FILTER MATCH?                                
         BNE   POMX                                                             
         DROP  R3                                                               
*                                                                               
POM05    L     R3,OMREC                                                         
         MVI   ELCODE,DOIDELQ      X'01' PRIMARY ID ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   POMX                                                             
         USING DOIDELD,R3                                                       
         OC    OMBUYER,OMBUYER     HAVE A BUYER FILTER?                         
         BZ    POM10               NO                                           
*                                                                               
         CLC   OMBUYER,DOIDBYR     MATCH ON BUYER?                              
         BNE   POMX                NO, DONE                                     
*                                                                               
POM10    TM    DTAIND10,DIOMBYR    READ BUYER REC FOR BUYER?                    
         BNZ   POM12               YES                                          
         OC    OMOFFICE,OMOFFICE   HAVE AN OFFICE FILTER?                       
         BZ    POM15               NO                                           
*                                                                               
POM12    GOTO1 PUTOMBYR            PUT THE OM BUYER AND CITY                    
         BNE   POMX                                                             
*                                                                               
POM15    OC    OMREP,OMREP         HAVE A REP FILTER?                           
         BZ    POM20               NO                                           
*                                                                               
         L     R3,OMREC                                                         
         MVI   ELCODE,DOSTELQ                                                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
POM15A   BRAS  RE,NEXTEL                                                        
         BNE   POMX                                                             
*                                                                               
         USING DOSTELD,R3                                                       
         CLI   DOSTLEN,DOSTLNQ     STANDARD LENGTH?                             
         BE    POM15A              YES, NO ID NUMBER AVAILIBLE                  
         CLI   DOSTSTAT,DDLVRD     DELIVERED?                                   
         BE    POM15B                                                           
         CLI   DOSTSTAT,DFXDLVD    FAX DELIVERED?                               
         BE    POM15B                                                           
         CLI   DOSTSTAT,DEMDLVD    E-MAIL DELIVERED?                            
         BNE   POM15A                                                           
*                                                                               
POM15B   LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   CTIKNUM,DOSTIDNM                                                 
         DROP  R3,R4                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),(0,=C'CTFILE'),KEY,SBAIO2            
         L     R3,SBAIO2                                                        
         USING CTIKEY,R3                                                        
         LA    R3,CTIDATA                                                       
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   POMX                                                             
         USING CTDSCD,R3           COPY USER ID TO SCREEN                       
*                                                                               
         XC    DMCB(24),DMCB       GET A(DDDAREREPS)                            
         MVC   DMCB+4(4),=X'D9000A1F'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             A(REPID TABLE)                               
**NOP    LA    R1,REPIDS           REPID TABLE                                  
*                                                                               
POM16    CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    POMX                YES                                          
         CLC   OMREP,0(R1)         MATCH ON REP?                                
         BE    POM18               YES                                          
         LA    R1,25(R1)           NO - BUMP TABLE                              
         B     POM16                                                            
*                                                                               
POM18    LLC   R2,14(R1)           L'ENTRY LENGTH                               
         BCTR  R2,0                - 1 FOR EX                                   
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   15(0,R1),CTDSC      MATCH ON REP?                                
         BNE   POMX                NO                                           
*                                                                               
POM20    L     R3,OMREC                                                         
         MVI   ELCODE,DOXMTELQ     TRANSMISSION ELEMENT                         
         BRAS  RE,GETEL            HAVE ONE?                                    
         BNE   POM30               NO                                           
         USING DOXMTELD,R3                                                      
         OC    OMNC,OMNC           EMPTY STATUS FILTER?                         
         BZ    POM25               NO                                           
         CLI   OMNC,C'Y'           WANT EMPTY STATUS ONLY?                      
         BNE   POM20A              NO                                           
         CLI   DOXMTSTA,QEMPTY     YES - IS THIS AN EMPTY ORDER?                
         BNE   POMX                NO - ONLY WANT EMPTY ORDERS                  
         B     POM25                                                            
*                                                                               
POM20A   CLI   DOXMTSTA,QEMPTY     IS THIS AN EMPTY ORDER?                      
         BE    POMX                YES - DONT WANT EMPTY ORDERS                 
*                                                                               
POM25    CLI   OMUND,0             ANY UNDARE/NOTDARE FILTER?                   
         BE    POM30               NO                                           
         MVI   BYTE,C'Y'                                                        
         CLI   DOXMTSTA,QNODARE    NODARE ORDER?                                
         BE    POM25A              YES                                          
         CLI   DOXMTSTA,QUNDARE    UNDARED?                                     
         BE    POM25A                                                           
         MVI   BYTE,C'N'                                                        
*                                                                               
POM25A   CLC   OMUND,BYTE                                                       
         BNE   POMX                                                             
***                                                                             
* CHECK STATUS FILTER AT THE END SO WE DONT DUP STATUS CODE IN SYSDRV4          
***                                                                             
POM30    L     R3,OMREC                                                         
         MVI   ELCODE,DOI2ELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   POM30A                                                           
         USING DOI2ELD,R3                                                       
         TM    DOI2FLG1,DOI2FVAR                                                
         BZ    *+8                                                              
         OI    OMFLAGS,MF1VAROR                                                 
         DROP  R3                                                               
*                                                                               
POM30A   MVC   THISSTAT(6),=CL6'UNSENT'                                         
         L     R3,OMREC                                                         
         MVI   ELCODE,DOSTELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   POM40                                                            
         USING DOSTELD,R3                                                       
POM30B   CLI   DOSTSTAT,DSENT      SENT?                                        
         BNE   POM31                                                            
         MVC   THISSTAT(6),=CL6'*SENT'                                          
         TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'*VRSNT'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'*RVSNT'                                         
         B     POM40                                                            
*                                                                               
POM31    CLI   DOSTSTAT,DFXSENT    FAX SENT?                                    
         BE    *+12                                                             
         CLI   DOSTSTAT,DFXRSNT    FAX RESENT?                                  
         BNE   POM31A                                                           
         MVC   THISSTAT(6),=CL6'*FXSNT'                                         
*                                                                               
         TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'*VFSNT'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'*RFSNT'                                         
         B     POM40                                                            
*                                                                               
POM31A   CLI   DOSTSTAT,DEMSENT    EMAIL SENT?                                  
         BNE   POM31B                                                           
         MVC   THISSTAT(6),=CL6'*EMSNT'                                         
*                                                                               
         TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'*VESNT'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'*RESNT'                                         
         B     POM40                                                            
*                                                                               
POM31B   CLI   DOSTSTAT,DDLVRD     DELIVERED?                                   
         BNE   POM31C                                                           
         BRAS  RE,NEXTEL                                                        
         CLI   DOSTSTAT,DSENT      WAS IT PREVIOUSLY SENT?                      
         BNE   POM30B              NO, DELNOT NOT IN THE RIGHT SPOT             
*                                      GO START OVER W/NEW STATUS               
         MVC   THISSTAT(6),=CL6'SENT'                                           
*                                                                               
         TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VARSNT'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'REVSNT'                                         
         B     POM40                                                            
*                                                                               
POM31C   CLI   DOSTSTAT,DFXDLVD    FAX DELIVERED?                               
         BNE   POM31D                                                           
         MVC   THISSTAT(6),=CL6'FXSENT'                                         
*                                                                               
         TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VFXSNT'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'RFXSNT'                                         
         B     POM40                                                            
*                                                                               
POM31D   CLI   DOSTSTAT,DEMDLVD    EMAIL DELIVERED?                             
         BNE   POM31E                                                           
         MVC   THISSTAT(6),=CL6'EMSENT'                                         
*                                                                               
         TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VEMSNT'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'REMSNT'                                         
         B     POM40                                                            
*                                                                               
POM31E   CLI   DOSTSTAT,QRJCT      REJECTED?                                    
         BNE   POM31F                                                           
         MVC   THISSTAT(6),=CL6'RJCTED'                                         
         TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VARREJ'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'REVREJ'                                         
         B     POM40                                                            
*                                                                               
POM31F   CLI   DOSTSTAT,QAPP       APPROVED?                                    
         BNE   POM31G                                                           
         MVC   THISSTAT(6),=CL6'OPENED'                                         
         TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VAROPN'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'REVOPN'                                         
         B     POM40                                                            
*                                                                               
POM31G   CLI   DOSTSTAT,QCFMDPND   CONFIRM PENDING?                             
         BNE   POM31H                                                           
         MVC   THISSTAT(6),=CL6'CFMPND'   WAITING FOR OTHER PRD ORDERS          
         B     POM40                     THAT WERE SPAWNED TO GET CFRMD         
*                                                                               
POM31H   CLI   DOSTSTAT,QCFMD      CONFIRMED?                                   
         BNE   POM32                                                            
         MVC   THISSTAT(6),=CL6'CNFRMD'                                         
         CLI   DOSTLEN,DOSTLNQ3    HAVE DOSTTYPE?                               
         BL    POM31H1             NO                                           
         MVC   THISSTAT(6),=CL6'CANCFM'                                         
         TM    DOSTTYPE,DCNFMCAN   CANCELLED CONFIRM?                           
         BO    POM40               YES - THIS STAUS WINS!                       
         MVC   THISSTAT(6),=CL6'CNFRMD'                                         
         TM    DOSTTYPE,DCNFMFUL   FULL CONFIRM?                                
         BO    POM31H2             YES - REVCNF CAN STILL TRUMP THIS!           
         TM    DOSTTYPE,DCNFMCOM   PARTIAL CONFIRM?                             
         BO    POM31I              YES - **VPCF CAN STILL TRUMP THIS!           
*                                                                               
POM31H1  TM    OMFLAGS,MF1CNFCM    CONFIRM WITH COMMENTS?                       
         BO    POM31I               - YUP                                       
*                                                                               
POM31H2  TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'VARCNF'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'REVCNF'                                         
         B     POM40                                                            
                                                                                
POM31I   MVC   THISSTAT(6),=CL6'**PCFM'                                         
         TM    OMFLAGS,MF1VAROR                                                 
         BZ    *+14                                                             
         MVC   THISSTAT(6),=CL6'**VPCF'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'**RPCF'                                         
         B     POM40                                                            
*                                                                               
POM32    CLI   DOSTSTAT,QFAXDLVD   FAX DELIVERED?                               
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'DELFAX'                                         
         B     POM40                                                            
*                                                                               
         CLI   DOSTSTAT,QFAXCNCL   FAX CANCELLED?                               
         BNE   POM32A                                                           
         MVC   THISSTAT(6),=CL6'CANFAX'                                         
         B     POM40                                                            
*                                                                               
POM32A   CLI   DOSTSTAT,QERRORED   ORDER IN ERROR?                              
         BNE   POM32B                                                           
         MVC   THISSTAT(6),=CL6'ERROR'                                          
         B     POM40                                                            
*                                                                               
POM32B   CLI   DOSTSTAT,QBYRCNFM   BUYER CONFIRMED?                             
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'BYRCFM'                                         
         B     POM40                                                            
*                                                                               
         CLI   DOSTSTAT,QEMPTY     EMPTY STATUS - NO BUYS?                      
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'EMPTY'                                          
         B     POM40                                                            
*                                                                               
         CLI   DOSTSTAT,QUNDARE    UNDARED?                                     
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'UNDARD' UNDARED                                 
         B     POM40                                                            
*                                                                               
         CLI   DOSTSTAT,QNODARE    NOT DARED?                                   
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'NTDARE' NOT DARE                                
         B     POM40                                                            
*                                                                               
         CLI   DOSTSTAT,QRECALL    RECALLED?                                    
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RECALL' RECALLED                                
         B     POM33                                                            
*                                                                               
         CLI   DOSTSTAT,QRCLAPPR   RECALLED, REP STATUS APPROVED?               
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLAPP' YES                                     
         B     POM33                                                            
*                                                                               
         CLI   DOSTSTAT,QRCLCONF   NOT RECALLED, REP STATUS CONFIRMED?          
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLCFM' YES                                     
         B     POM33                                                            
*                                                                               
         CLI   DOSTSTAT,QRCLDELN   RECALLED, REP STATUS DELIVERED?              
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLDNT' YES                                     
         B     POM33                                                            
*                                                                               
         CLI   DOSTSTAT,QRCLREJD   NOT RECALLED, REP STATUS REJECTED?           
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLREJ' YES                                     
         B     POM33                                                            
*                                                                               
         CLI   DOSTSTAT,QRCLTRNS   NOT RECALLED, REP STATUS TRANSMITTED         
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLTRN' YES, REP TRANSMITTED TO STATION         
         B     POM33                                                            
*                                                                               
         CLI   DOSTSTAT,QRCLWIP    RECALLED, WORK IN PROGRESS                   
         BNE   POM34                                                            
         MVC   THISSTAT(6),=CL6'RCLWIP' YES                                     
*                                                                               
POM33    TM    OMFLAGS,MF1VAROR    THIS CHECK IS FOR ALL RECALLS                
         BZ    *+14                ...BESIDES RECALL UNKNOWN                    
         MVC   THISSTAT(6),=CL6'VARRCL'                                         
         B     POM40                                                            
         TM    OMFLAGS,MF1REVOR                                                 
         BZ    POM40                                                            
         MVC   THISSTAT(6),=CL6'REVRCL'                                         
         B     POM40                                                            
*                                                                               
POM34    CLI   DOSTSTAT,QRCLUNKN   RECALLED, REP STATUS UNKNOWN                 
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RCLUNK' NOT RECALLED, REP STAT UNKNOWN          
         B     POM40                                                            
*                                                                               
         CLI   DOSTSTAT,QSNTPNDG   SENT PENDING AFTER A RECALL                  
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'*PNDNG'                                         
         B     POM40                                                            
*                                                                               
         CLI   DOSTSTAT,QSNTXCNF   SENT CANCELLED, RECALL CONFIRMED             
         BE    POM31I               - SAME TREATMENT AS CONFIRMED W/COM         
*                                                                               
         CLI   DOSTSTAT,QSNTXREJ   SENT CANCELLED, RECALL REJECTED              
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'RJCTED'                                         
         B     POM40                                                            
*                                                                               
         CLI   DOSTSTAT,QTOBESNT   TO BE SENT VIA SCRIPT                        
         BNE   *+14                                                             
         MVC   THISSTAT(6),=CL6'*SENT'                                          
         B     POM40                                                            
*                                                                               
         MVC   THISSTAT(6),=CL6'??????'                                         
*                                                                               
POM40    OC    OMSTAT,OMSTAT         HAVE A STATUS FILTER?                      
         BZ    POM50                 NO                                         
         CLC   THISSTAT,OMSTAT       YES - DOES IT MATCH?                       
         JNE   XITNE                 NO                                         
*                                                                               
POM50    CLI   SBQGETNM,C'Y'       TEST GET BUYER/BILLER NAMES                  
         BNE   POM60               NO                                           
         MVC   DUB(2),SBBCLT       YES-                                         
         MVC   DUB+2(1),SBBPRD                                                  
         MVC   DUB+3(1),SBBEST                                                  
         MVC   DUB+4(2),SBBMKT                                                  
         CLC   SVNAME,DUB          TEST CLT/PRD/EST/MKT CHANGE                  
         BE    POM60               NO                                           
         MVC   SVNAME,DUB          YES-GET BUYER/BILLER NAMES                   
         GOTO1 AGETNAME                                                         
*                                                                               
POM60    TM    DTAIND11,DIOMKGD    READ ORDER MAKEGOOD RECORDS?                 
         BZ    POM70               NO                                           
         GOTO1 =A(PROCDRMG)        PROCESS DARE MAKEGOOD RECORDS                
*                                                                               
POM70    CLI   SBMODE,SBPROCOM     PROCESSING AN OM RECORD?                     
         JNE   XITEQ               NO, RETURN CC EQU                            
POM75    L     R4,AGLOBAL          RESTORE R4                                   
         GOTO1 ADRIVIN                                                          
*                                                                               
         TM    DTAIND11,DIOMKGD    READ ORDER MAKEGOOD RECORDS?                 
         BZ    POMX                NO - EXIT                                    
         OC    DMGKEY,DMGKEY       JUST PROCESSED A MAKEGOOD?                   
         BZ    POMX                NO                                           
*                                                                               
         L     R3,OMREC            A(ORDER RECORD)                              
         MVI   ELCODE,DOSPELQ      X'03' SUPPLEMENTARY ID ELEMENT               
         BRAS  RE,GETEL            HAVE ONE?                                    
         JNE   POM80               NO                                           
*                                                                               
         USING DOSPELD,R3          SUPPLEMENTARY ID ELEMENT DSECT               
         XC    DOSPTOTL,DOSPTOTL   CLEAR TOTAL DOLLARS                          
         XC    DOSPSPTS,DOSPSPTS   CLEAR TOTAL SPOTS                            
         DROP  R3                  DROP R3                                      
*                                                                               
POM80    GOTO1 =A(NEXTDRMG)        HAVE ANOTHER MG TO PROCESS?                  
         BE    POM75               YES                                          
*                                                                               
POMX     CLI   SBMODE,SBPROCOM     PROCESSING AN OM RECORD?                     
         JNE   XITNE               NO, RETURN CC NEQ                            
         J     XIT                                                              
***                                                                             
* TSAR OFF OM RECORD BASED ON BUY KEY                                           
***                                                                             
TSAROMG  NMOD1 0,**401T**                                                       
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R3,SBAIO1           A(BUY RECORD)                                
         USING BUYREC,R3                                                        
         OC    LASTSYS,LASTSYS     JUST PROCESSED A SYSCODE?                    
         BZ    TOM00               NO - DELETE OM RECS FROM TSAR BUFF!          
         MVC   FULL(3),BUYKSTA     STATION                                      
         NI    FULL+2,X'80'        STRIP NETWORK BITS                           
         CLC   LASTSYS,FULL        PROCESSING THIS SYSCODE?                     
         BE    TOM05               YES - DON'T DELETE - WE MAY REVISIT!         
TOM00    BRAS  RE,TSARDEL          DELETE OM RECS FROM TSAR BUFFER              
         CLI   SBMODE,SBPROCDO     DELETE OM RECS FROM TSAR BUFFER?             
         JE    TOMXIT              YES - THAT'S ALL WE ARE HERE FOR             
         XC    LASTSYS,LASTSYS                                                  
*                                                                               
         CLI   BUYKSTA,X'E8'       CABLE?                                       
         BL    TOM05               NO                                           
         MVC   LASTSYS,BUYKSTA     YES - SAVE THE SYSCODE!                      
         NI    LASTSYS+2,X'80'     STRIP NETWORK BITS                           
*                                                                               
TOM05    MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING DOKEY,R2                                                         
         MVI   DCKTYPE,DCKTYPQ     X'0D'                                        
         MVI   DCKSUBTY,DCKSTYPQ   X'B5'                                        
         MVC   DCKAGMD,BUYKAM      A/M                                          
         MVC   DCKCLT,BUYKCLT      CLIENT                                       
         GOTO1 HIGH                                                             
         B     TOM15                                                            
*                                                                               
TOM10    GOTO1 SEQ                                                              
*                                                                               
TOM15    CLC   DOKEY(5),KEYSAVE    SAME TYPE / A/M / CLT?                       
         BNE   TOMXIT              NO, DONE                                     
         CLC   DCKEST,BUYKEST      SAME ESTIMATE?                               
         BNE   TOM10               NO, READ SEQ                                 
*                                                                               
         MVC   FULL(3),BUYKSTA     SAVE THE STATION                             
         CLI   BUYKSTA,X'E8'       CABLE STATION?                               
         BL    TOM20               NO                                           
         NI    FULL+2,X'80'        YES - SEARCH BY SYSCODE ONLY!                
*                                                                               
TOM20    CLC   DCKSTA,FULL         SAME STA?                                    
         BNE   TOM10               NO, READ SEQ                                 
         CLI   DCKFLTNM,0          HAVE A FLIGHT ON THIS ORDER?                 
         BNE   TOM10               YES - WE DON'T SUPPORT IT                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,TSARADD          TSAR THIS RECORD OFF                         
         B     TOM10                                                            
*                                                                               
TOMXIT   MVC   AIO,AIO1                                                         
         J     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
TSARDEL  NTR1                                                                   
*                                                                               
TDEL00   L     R0,AIO2             CLEAR AIO2 TO BUILD TSAR BUFFER              
         LA    R1,4000                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,AIO2                                                          
         MVI   2(R3),NPOMRECQ      OM RECORD                                    
*                                                                               
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         ST    R3,TSAREC                                                        
         GOTO1 ATSAROF,(R2)                                                     
         TM    TSERRS,TSEEOF       DID WE FIND THE KEY?                         
         BO    TDELXIT             NO, DONE                                     
         CLIY  MYKEY,NPOMRECQ      HAVE AN OM RECORD?                           
         BNE   TDELXIT             NO, DONE                                     
         MVI   TSOFFACT,TSADEL     DELETE THIS RECORD                           
         GOTO1 ATSAROF,(R2)        DELETE THIS OM RECORD                        
         B     TDEL00                                                           
*                                                                               
TDELXIT  J     XIT                                                              
         DROP  R2                                                               
*                                                                               
TSARADD  NTR1                                                                   
*                                                                               
         L     R0,AIO3             CLEAR AIO3 TO BUILD TSAR BUFFER              
         LA    R1,4000                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,AIO3             BUILD OM REC FOR TSAR HERE                   
         L     RE,AIO2             OM REC                                       
         USING DOKEY,RE                                                         
         ICM   R3,3,DORLEN         RECORD LENGTH                                
         AHI   R3,TSARKEYL+2       DON'T NEED TO SAVE THE KEY TWICE             
         STCM  R3,3,0(R1)          TSAR RECORD LENGTH                           
         MVI   2(R1),NPOMRECQ      OM REC                                       
         MVC   3(13,R1),KEY        SAVE THE PASSIVE KEY                         
*                                                                               
         L     R0,SBAIO3           A(BUFFERED OM REC)                           
         AHI   R0,TSARKEYL+2       MOVE ENTIRE RECORD HERE                      
         XR    R1,R1                                                            
         ICM   R1,3,DORLEN         RECORD LENGTH                                
         LR    RF,R1               RECORD LENGTH                                
         MVCL  R0,RE               COPY RECORD TO AIO3                          
         DROP  RE                                                               
*                                                                               
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSAADD     SET TO ADD A RECORD                          
         L     R0,SBAIO3           A(WHERE TO PUT RECORD)                       
         ST    R0,TSAREC                                                        
         GOTO1 ATSAROF,(R2)        ADD THE RECORD                               
         CLI   TSERRS,0            ANY ERRORS?                                  
         JE    TSARAX              NO                                           
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         JE    TSARAX              YES, IGNORE IT                               
         DC    H'0'                                                             
*                                                                               
TSARAX   J     XIT                                                              
         DROP  R2                                                               
***                                                                             
* GET OM RECORD FROM TSAR BUFFER                                                
***                                                                             
GETOMREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,SBAIO2           CLEAR AIO2 TO BUILD TSAR BUFFER              
         LA    R1,4000                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LAY   R2,MYKEY                                                         
         XC    0(L'MYKEY,R2),0(R2)                                              
         MVI   0(R2),NPOMRECQ      OM REC                                       
         AHI   R2,1                BUILT OM KEY HERE                            
         USING DOKEY,R2                                                         
         USING BUYREC,R3                                                        
         USING SCHUNKD,R5                                                       
         MVI   DCKTYPE,DCKTYPQ     X'0D'                                        
         MVI   DCKSUBTY,DCKSTYPQ   X'B5'                                        
         MVC   DCKAGMD,BUYKAM      A/M                                          
         MVC   DCKCLT,BUYKCLT      CLIENT                                       
         MVC   DCKPRD,SCPRD1       PRODUCT                                      
         MVC   DCKEST,BUYKEST      ESTIMATE                                     
         MVC   DCKSTA,BUYKSTA      STA                                          
*                                                                               
         CLI   BUYKSTA,X'E8'       CABLE STATION?                               
         BL    GETOM10             NO                                           
         NI    DCKSTA+2,X'80'      YES - SEARCH BY SYSCODE ONLY!                
*                                                                               
GETOM10  MVC   DCKPRD2,SCPRD2      PRODUCT2                                     
         CLI   SBBYTYPE,C'T'       TRADE?                                       
         BNE   *+8                 NO                                           
         OI    DCKFLAG,DCKFTRDE    YES - SET TRADE BIT                          
         DROP  R2,R3,R5                                                         
*                                                                               
GETOM20  L     R4,SBAIO2           BUFFER RECORD HERE                           
         BCTR  R2,0                                                             
         MVC   2(L'MYKEY,R4),0(R2)                                              
*                                                                               
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         ST    R4,TSAREC                                                        
         GOTO1 ATSAROF,(R2)                                                     
         TM    TSERRS,TSEEOF       DID WE HIT EOF?                              
         JO    XITNE               YES, SET CC NOT EQU                          
         DROP  R2                                                               
*                                                                               
         LAY   R2,MYKEY                                                         
         CLC   0(13,R2),2(R4)      FOUND OM RECORD?                             
         JNE   XITNE               NO, SET CC NOT EQU                           
*                                                                               
         L     R0,SBAIO3           COPY OM RECORD TO SBAIO3                     
         LA    R1,4000-TSARKEYL-2                                               
         L     RE,SBAIO2                                                        
         AHI   RE,TSARKEYL+2                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         J     XITEQ                                                            
*                                                                               
PUTOMREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,GETOMREC         GET OM RECORD FROM TSAR IN AIO2              
         JNE   XIT                                                              
*                                                                               
         L     R3,SBAIO2                                                        
         AHI   R3,TSARKEYL+2                                                    
         MVI   ELCODE,DOSPELQ      X'03' SUPPLEMENTARY ID ELEMENT               
         BRAS  RE,GETEL            HAVE ONE?                                    
         JNE   XIT                 NO                                           
         USING DOSPELD,R3                                                       
         XC    DOSPTOTL,DOSPTOTL   CLEAR TOTAL DOLLARS                          
         XC    DOSPSPTS,DOSPSPTS                                                
         DROP  R3                                                               
*                                                                               
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         MVI   TSOFFACT,TSAWRT     SET TO WRITE BACK A RECORD                   
         L     R3,SBAIO2           A(REC) MUST BEGIN AT LENGTH                  
         ST    R3,TSAREC           A(VARIABLE LENGTH RECORD)                    
         GOTO1 ATSAROF,(R2)        WRITE BACK THE RECORD                        
         CLI   TSERRS,0            ANY ERRORS?                                  
         JE    XIT                 NO                                           
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
PROCDRMG NTR1  BASE=*,LABEL=*                                                   
         USING WORKD,R6                                                         
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
*                                                                               
         L     R5,OMREC            A(ORDER RECORD)                              
         USING DOKEY,R5            DARE ORDER RECORD DSECT                      
*                                                                               
         MVC   AIO,AIO2            USE AIO2                                     
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R3,KEY              R2 = KEY                                     
         USING MNKEY,R3            DARE MAKEGOOD NOTICE DSECT                   
         MVC   SVDISP,DATADISP     SAVE OFF DATADISP                            
         MVC   DATADISP,=H'24'     SET DATADISP TO 24 BYTES                     
*                                                                               
         CLI   DOKSTA,X'E8'        CABLE?                                       
         BL    DAREMG00            NO                                           
*                                                                               
         MVC   DATADISP,=H'42'     SET DATADISP TO 42 BYTES                     
         MVC   SYSDIR,=C'XSPDIR  ' SET TO XSPDIR                                
         MVC   SYSFIL,=C'XSPFILE ' SET TO XSPFILE                               
         MVC   LKEY,=H'32'         KEY LENGTH OF 32                             
         MVC   LSTATUS,=H'4'       STATUS BYTE LENGTH                           
*                                                                               
         MVI   MNXKTYPE,MNXKTYPQ   X'0D'                                        
         MVI   MNXKSBTY,MNXKSBTQ   X'36'                                        
         MVC   MNXKAGMD,DOKAGMD    AGENCY/MEDIA                                 
         MVC   MNXKORDR,DOKORDER   ORDER                                        
         B     DAREMG05            GO READ RECORDS                              
*                                                                               
DAREMG00 MVI   MNKTYPE,MNKTYPQ     X'0D'                                        
         MVI   MNKSUBTY,MNKSTYPQ   X'36'                                        
         MVC   MNKAGMD,DOKAGMD     AGENCY/MEDIA                                 
         MVC   MNKORDER,DOKORDER   ORDER                                        
         DROP  R5                  DROP DOKEY USING                             
*                                                                               
         MVI   ELCODE,DOIDELQ      X'01' PRIMARY ID ELEMENT                     
         BRAS  RE,GETEL1           HAVE THE PRIMARY ID ELEMENT?                 
         BNE   DAREMGX             NO - DONE                                    
         USING DOIDELD,R5          PRIMARY ID ELEMENT DSECT                     
         MVC   MNKBYR,DOIDBYR      BUYER                                        
         DROP  R5                  DROP DOIDELD USING                           
*                                                                               
DAREMG05 OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 HIGH                READ HIGH                                    
         B     DAREMG20            GO TEST KEY                                  
*                                                                               
DAREMG10 GOTO1 SEQ                 READ SEQ                                     
*                                                                               
DAREMG20 L     R5,OMREC            RESET A(ORDER RECORD)                        
         USING DOKEY,R5            DARE ORDER RECORD DSECT                      
         CLI   DOKSTA,X'E8'        CABLE?                                       
         BL    DAREMG25            NO                                           
         CLC   MNXKEY(25),KEYSAVE  SAME ORDER NUMBER?                           
         BNE   DAREMG80            NO, DONE                                     
         OC    MNXKSTTN,MNXKSTTN   NULL STATION/NETWORK?                        
         BNZ   DAREMG10            NO - READ SEQ                                
         CLI   MNXKSEQ,0           NULL SEQUENCE NUMBER?                        
         BNE   DAREMG10            NO - READ SEQ                                
         B     DAREMG30            PROCESS THIS RECORD                          
*                                                                               
DAREMG25 CLC   MNKEY(10),KEYSAVE   SAME BUYER/ORDER NUMBER?                     
         BNE   DAREMG80            NO, DONE                                     
*                                                                               
DAREMG30 GOTO1 GETREC              GET THE DARE MAKEGOOD NOTICE RECORD          
*                                                                               
         LAY   R4,MYREC-2          R4 = TSAR RECORD                             
         XC    2(MYRECL,R4),2(R4)  CLEAR KEY+DATA                               
         LA    RF,DMKGDLEN         RECORD LENGTH                                
         AHI   RF,TSARKEYL+2       TSAR KEY LEN + 2 BYTES FOR REC LEN           
         STCM  RF,3,0(R4)          TSAR RECORD LENGTH                           
         MVI   2(R4),NPDMRECQ      TYPE 20 - DARE MAKEGOOD NOTICE REC           
         MVC   3(3,R4),MNKGROUP    MAKEGOOD GROUP CODE                          
         CLI   DOKSTA,X'E8'        CABLE?                                       
         BL    *+10                NO                                           
         MVC   3(3,R4),MNXKGRP     MAKEGOOD GROUP CODE                          
         DROP  R5                  DROP DOKEY USING                             
*                                                                               
         AHI   R4,TSARKEYL+2       RECORD GOES HERE                             
         USING DAREMKGD,R4         DARE MAKEGOOD NOTICE DSECT FOR TSAR          
*                                                                               
         L     R5,AIO2             DARE MAKEGOOD NOTICE RECORD                  
*                                                                               
         MVI   ELCODE,MNSTELQ      X'05' MAKEGOOD GROUP STATUS ELEMENTS         
         BAS   RE,GETEL1           HAVE MAKEGOOD GROUP STATUS ELEM?             
         B     *+8                 GO TEST CC                                   
*                                                                               
DAREMG50 BAS   RE,NEXTEL1          ANOTHER MAKEGOOD GROUP STATUS ELEM?          
         BNE   DAREMG70            NO - DONE                                    
         USING MNSTELD,R5          MAKEGOOD GROUP STATUS ELEMENT DSECT          
*                                                                               
         CLI   DMKGDLST,0          LAST STATUS ALREADY SET?                     
         BNZ   DAREMG60            YES                                          
         CLI   MNSTSTAT,MNSTDELV   DELIVERED?                                   
         BE    DAREMG50            YES - PROCESS NEXT ELEMENT                   
         L     RF,OMREC            A(ORDER RECORD)                              
         USING DOKEY,RF            DARE ORDER RECORD DSECT                      
         CLI   DOKSTA,X'E8'        CABLE?                                       
         BL    *+12                NO                                           
         TM    MNXKSTAT,X'80'      IS KEY DELETED?                              
         B     *+8                 GO TEST CC                                   
         TM    MNKSTAT,X'80'       IS KEY DELETED?                              
         BZ    *+12                NO                                           
         CLI   MNSTSTAT,MNSTCAN    CANCELLED?                                   
         BNE   DAREMG10            NO - DON'T PROCESS THIS                      
         MVC   DMKGDLDT,MNSTDATE   LAST STATUS RECEIVED DATE/TIME               
         MVC   DMKGDLST,MNSTSTAT   LAST STATUS                                  
         DROP  RF                  DROP DOKEY USING                             
*                                                                               
DAREMG60 CLI   MNSTSTAT,MNSTNEW    STATUS NEW?                                  
         BNE   *+14                NO                                           
         MVC   DMKGDRDT,MNSTDATE   RECEIVED DATE/TIME                           
         B     DAREMG50            PROCESS NEXT ELEMENT                         
         CLI   MNSTSTAT,MNSTAMND   AMENDED?                                     
         BNE   DAREMG50            NO - PROCESS NEXT ELEMENT                    
         LLC   RF,DMKGDRVS         TOTAL MAKEGOOD AMENDMENTS                    
         AHI   RF,1                BUMP REVISION COUNT BY 1                     
         STC   RF,DMKGDRVS         TOTAL MAKEGOOD AMENDMENTS                    
         B     DAREMG50            PROCESS NEXT ELEMENT                         
         DROP  R5                  DROP MNSTELD USING                           
*                                                                               
DAREMG70 LA    R2,TSAREA           R2 = TSAR AREA                               
         USING TSARD,R2            TSAR DSECT                                   
         MVI   TSOFFACT,TSAADD     SET TO ADD A RECORD                          
         LAY   R4,MYREC-2          R4 = TSAR RECORD                             
         ST    R4,TSAREC           A(TSAR RECORD TO ADD)                        
         GOTO1 ATSAROF,(R2)        ADD THE RECORD                               
         CLI   TSERRS,0            ANY ERRORS?                                  
         JE    DAREMG10            NO - READ SEQ                                
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         JE    DAREMG10            YES, IGNORE IT                               
         DC    H'0'                NO OTHER ERRORS TOLERATED                    
         DROP  R2                  DROP TSARD USING                             
*                                                                               
DAREMG80 XC    DMGKEY,DMGKEY       CLEAR DARE MAKEGOOD KEY                      
         XC    DMGENTRY,DMGENTRY   CLEAR DARE MAKEGOOD RECORD ENTRY             
*                                                                               
         LAY   R2,MYKEY            R2 = MYKEY                                   
         XC    0(L'MYKEY,R2),0(R2) CLEAR THE TSAR KEY                           
         MVI   0(R2),NPDMRECQ      DARE MAKEGOOD NOTICE RECORD                  
*                                                                               
         LA    R2,TSAREA           R2 = TSAR AREA                               
         USING TSARD,R2            TSAR DSECT                                   
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         LAY   R4,MYREC-2          R4 = TSAR RECORD                             
         ST    R4,TSAREC           A(TSAR RECORD)                               
         GOTO1 ATSAROF,(R2)        READ HIGH                                    
         TM    TSERRS,TSEEOF       DID WE HIT EOF?                              
         JO    DAREMGX             YES, SET CC NOT EQU                          
         DROP  R2                  DROP TSARD USING                             
*                                                                               
         LAY   R2,MYKEY            TSAR KEY                                     
         CLI   0(R2),NPDMRECQ      FOUND A DARE MAKEGOOD RECORD?                
         JNE   DAREMGX             NO                                           
*                                                                               
         MVC   DMGKEY,1(R2)        SAVE MKGD GROUP                              
         LAY   R2,MYDATA           TSAR RECORD                                  
         MVC   DMGENTRY,0(R2)      SAVE MAKEGOOD RECORD ENTRY                   
*                                                                               
DAREMGX  MVC   DATADISP,SVDISP     RESTORE DATADISP                             
         L     R5,OMREC            RESET A(ORDER RECORD)                        
         USING DOKEY,R5            DARE ORDER RECORD DSECT                      
         CLI   DOKSTA,X'E8'        CABLE?                                       
         BL    DAREMGXX            NO                                           
*                                                                               
         MVC   SYSDIR,=C'SPTDIR  ' RESTORE TO SPTDIR                            
         MVC   SYSFIL,=C'SPTFILE ' RESTORE TO SPTFILE                           
         MVC   LKEY,=H'13'         RESTORE KEY LENGTH                           
         MVC   LSTATUS,=H'1'       RESTORE STATUS BYTE LENGTH                   
*                                                                               
DAREMGXX NI    DMINBTS,X'F7'       DON'T READ FOR DELETED                       
         J     XIT                 EXIT                                         
*                                                                               
         GETELN R5,DATADISP,ELCODE,1                                            
*                                                                               
NEXTDRMG NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R2,MYKEY            R2 = MYKEY                                   
         XC    0(L'MYKEY,R2),0(R2) CLEAR THE TSAR KEY                           
         MVI   0(R2),NPDMRECQ      DARE MAKEGOOD NOTICE RECORD                  
         MVC   1(3,R2),DMGKEY      LAST MAKEGOOD GROUP CODE PROCESSED           
*                                                                               
         XC    DMGKEY,DMGKEY       CLEAR DARE MAKEGOOD KEY                      
         XC    DMGENTRY,DMGENTRY   CLEAR DARE MAKEGOOD RECORD ENTRY             
*                                                                               
         LA    R2,TSAREA           R2 = TSAR AREA                               
         USING TSARD,R2            TSAR DSECT                                   
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         LAY   R4,MYREC-2          R4 = TSAR RECORD                             
         ST    R4,TSAREC           A(TSAR RECORD)                               
*                                                                               
         GOTO1 ATSAROF,(R2)        READ HIGH                                    
*                                                                               
         TM    TSERRS,TSEEOF       DID WE HIT EOF?                              
         JZ    *+6                 NO                                           
         DC    H'0'                JUST PROCESSED THIS - MUST FIND KEY!         
*                                                                               
         MVI   TSOFFACT,TSANXT     SET GET NEXT RECORD                          
*                                                                               
         GOTO1 ATSAROF,(R2)        READ SEQ                                     
*                                                                               
         TM    TSERRS,TSEEOF       DID WE HIT EOF?                              
         JNZ   NDMGDEL             YES - PURGE THESE RECORDS                    
*                                                                               
         CLIY  MYKEY,NPDMRECQ      FOUND A DARE MAKEGOOD RECORD?                
         JNE   NDMGDEL             NO - PURGE THESE RECORDS                     
*                                                                               
         LAY   R3,MYKEY            TSAR KEY                                     
         MVC   DMGKEY,1(R3)        SAVE MKGD GROUP                              
         LAY   R3,MYDATA           TSAR RECORD                                  
         MVC   DMGENTRY,0(R3)      SAVE MAKEGOOD RECORD ENTRY                   
         J     NDMGEQU             SET CC EQU                                   
*                                                                               
NDMGDEL  LAY   R3,MYKEY            TSAR KEY                                     
         XC    0(L'MYKEY,R3),0(R3) CLEAR THE TSAR KEY                           
         MVI   0(R3),NPDMRECQ      DARE MAKEGOOD NOTICE RECORD                  
*                                                                               
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
*                                                                               
         GOTO1 ATSAROF,(R2)        READ HIGH                                    
*                                                                               
         TM    TSERRS,TSEEOF       DID WE HIT EOF?                              
         JNZ   NDMGNEQ             YES - SET CC NEQ                             
*                                                                               
         CLIY  MYKEY,NPDMRECQ      FOUND A DARE MAKEGOOD RECORD?                
         JNE   NDMGNEQ             NO - SET CC NEQ                              
*                                                                               
         MVI   TSOFFACT,TSADEL     DELETE THIS RECORD                           
*                                                                               
         GOTO1 ATSAROF,(R2)        DELETE MAKEGOOD RECORD                       
         DROP  R2                  DROP TSARD USING                             
*                                                                               
         J     NDMGDEL             CONTINUE PURGING                             
*                                                                               
NDMGNEQ  LTR   RB,RB               SET CC NEQ                                   
         J     XIT                 EXIT                                         
NDMGEQU  CR    RB,RB               SET CC EQU                                   
         J     XIT                 EXIT                                         
*=================================================================*             
* MQRPTERR - ERROR HANDLER WHEN WRITING TO FILE                   *             
*                                                                 *             
* THIS CODE WILL INTERCEPT ERROR EXITS AND TEST WHETHER OUTPUT IS *             
* BEING WRITTEN TO A FILE.  IF NOT, IT WILL GO TO ERREX.  ELSE IT *             
* WILL SEND INFORMATION TO MQ ABOUT WHERE THE REPORT CAN BE FOUND *             
* AND THEN CONTINUE ON TO ERREX.                                  *             
*                                                                 *             
*=================================================================*             
         SPACE 1                                                                
         USING WORKD,R6                                                         
         USING CONHEADH-64,R7                                                   
         USING SYSD,R9                                                          
         USING GEND,RC                                                          
MQE      USING MQERRD,ELEM                                                      
*                                                                               
MQRPTERR NTR1  BASE=*,LABEL=*                                                   
         L     R7,ATWA                                                          
         TM    OPTIND6,OPT6FILE    SEND OUTPUT DATA TO FILE?                    
         BZ    MQRPX                NO - JUST GO ON TO ERREX                    
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQEERRLQ-1),ELEM                                          
         MVC   MQE.MQEHID,=CL6'ERNOT1'                                          
         MVC   MQE.MQESYS,=CL3'SPT'                                             
         L     RF,TWAMASTC                                                      
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   MQE.MQEAGYID,MCAGYCOD-MCEXTRA(RF)                                
         MVC   MQE.MQEQUAL,WRITIT+16                                            
         OC    MQE.MQEQUAL,=CL32' '                                             
         GOTO1 DATCON,DMCB,(3,BTODAY),(X'20',MQE.MQEDATE)                       
         THMS                                                                   
         ST    R1,FULL                                                          
         EDIT  (P4,FULL),(6,MQE.MQETIME),FILL=0                                 
         MVC   MQE.MQEDATA1(L'WRIDESC),WRIDESC  MEDIACOM REQ ID                 
         OC    MQE.MQEDATA1,=CL32' '                                            
*                                                                               
         L     RF,TWAMASTC         GET PRT QUE ID                               
         USING MASTD,RF                                                         
         MVC   MQE.MQECOD(3),MCREMPQK+2                                         
         MVI   MQE.MQECOD+3,C','                                                
         XR    RE,RE                                                            
         ICM   RE,3,MCREMPQK+5                                                  
         EDIT  (RE),(4,MQE.MQECOD+4),ALIGN=LEFT                                 
         DROP  RF                                                               
         MVC   MQE.MQEMSG(L'CONHEAD),CONHEAD                                    
         DROP  MQE                                                              
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQEERRLQ,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
*                                                                               
MQRPX    J     XIT                                                              
*                                                                               
* NOTE: THE 'ROUTE' IS PASSED IN THE MQOPEN AS THE HEADER                       
MQERRD   DSECT                                                                  
MQEHID   DS    CL6                 HUB RECORD ID                                
MQESYS   DS    CL3                 SYSTEM=SPT                                   
MQEAGYID DS    CL4                 AGENCY ID                                    
MQEQUAL  DS    CL16                QUALIFIER                                    
MQEDATE  DS    CL6                 FILE DATE                                    
MQETIME  DS    CL6                 FILE TIME HHMMSS                             
MQEDATA1 DS    CL32                                                             
MQEDATA2 DS    CL32                                                             
MQECOD   DS    CL32                ERROR CODE (OPT)                             
MQEMSG   DS    CL80                ERROR MESSAGE (OPT)                          
MQEERRLQ EQU   *-MQERRD                                                         
T20401   CSECT                                                                  
T20401   CSECT                                                                  
*                                                                               
MQOPEN   NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,TWADCONS                                                   
         BNZ   *+6                                                              
         DCHO                                                                   
         ICM   RF,15,TMQRPT-TWADCOND(RF)                                        
         BNZ   *+6                                                              
         DCHO                                                                   
         ST    RF,AMQRPT                                                        
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
*                                                                               
* IF WE'RE RUNNING A TEST, SEND TO TEST MQ BROKER                               
         BRAS  RE,TESTRUN          IS THIS A TEST RUN?                          
         BNE   *+8                  NO                                          
         OI    DMCB+8,X'01'         YES -PUT TO TEST MQ BROKER                  
*                                                                               
*NOTE: THE MQ 'HEADER' IS USED AS THE ROUTE                                     
         TM    OPTIND6,OPT6SCJE    SC JOHNSON ESTIMATE FILE?                    
         BZ    MQOPN10             NO                                           
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,SCJTIT),,0                           
         B     MQOPN20                                                          
*                                                                               
MQOPN10  GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,WRITIT),,0                           
MQOPN20  CLI   DMCB+8,0                                                         
         JE    XIT                                                              
         DCHO                                                                   
         EJECT                                                                  
*                                                                               
         USING CONHEADH-64,R7                                                   
TESTRUN  DS    0H                                                               
         ICM   RF,15,TWAMASTC                                                   
         JNZ   *+6                                                              
         DCHO                                                                   
         ICM   RF,15,MCSSB-MASTD(RF)                                            
         JNZ   *+6                                                              
         DCHO                                                                   
         CLI   SSOXTND-SSOOFF(RF),X'FF'                                         
         JNE   NOTEST                                                           
         CLI   SSODSPAC-SSOOFF(RF),C'A'  IS THIS A TEST REQUEST?                
         JE    NOTEST                     NO                                    
         CLI   SSODSPAC-SSOOFF(RF),C' '  IS THIS A TEST REQUEST?                
         JNH   NOTEST                     NO                                    
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
NOTEST   CR    RE,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
FIXCLRST NTR1  BASE=*,LABEL=*      FIX CLRST ELEMENT IF NEEDED                  
*                                                                               
         USING GEND,RC             GENERAL SPOOLING SYSTEM DSECT                
         USING SYSD,R9             SYSD DSECT                                   
***                                                                             
* I'M DOING THIS PART FIRST TO MAKE SURE I NEED TO RE-ORDER THE ELEMS           
* WHILE IT'S NOT NECESSARY, I FEEL IT'S SAFER JUST IN CASE THERE ARE            
* ANY BAD RECORDS OUT THERE MEANING THAT I COME ACROSS ELEMENTS I DON'T         
* EXPECT. SINCE THIS ISSUE IS RARE, DOING THIS WILL REALLY LIMIT                
* EXPOSURE TO IN CASE I ENCOUNTER ANYTHING UNEXPECTED                           
***                                                                             
         MVI   BYTE,0              USED TO FLAG NULL CHECK NUMBER               
         LR    RF,R1               A(FIRST X'03' ELEMENT)                       
*                                                                               
TESTCL10 CLI   0(RF),0             END OF RECORD?                               
         BE    FIXCLX              YES - NO ISSUE                               
         CLI   0(RF),X'01'         HIT THE NEXT X'01' ELEMENT?                  
         BE    FIXCLX              YES - NO ISSUE                               
         CLI   0(RF),X'03'         X'03' ELEMENT?                               
         BE    TESTCL20            YES - SKIP TO NEXT ELEMENT                   
         CLI   0(RF),X'05'         X'05' INVOICE ELEMENT?                       
         BNE   FIXCLX              NO - DONE - BAD CLRST RECORD                 
*                                                                               
         USING CLSTEL05,RF         INVOICE ELEMENT DSECT                        
         OC    CLS5CHK,CLS5CHK     HAVE A CHECK NUMBER?                         
         BZ    TESTCL15            NO                                           
         CLI   BYTE,1              DID WE ENCOUNTER A NULL CHECK NUM?           
         BE    FIXCL00             YES - THIS RECORD HAS AN ISSUE               
         B     TESTCL20            NO - BUMP TO NEXT ELEMENT                    
         DROP  RF                  DROP INVOICE ELEMENT USING                   
*                                                                               
TESTCL15 MVI   BYTE,1              FLAG NULL CHECK NUMBER                       
*                                                                               
TESTCL20 LLC   RE,1(RF)            ELEMENT LENGTH                               
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     TESTCL10            PROCESS NEXT ELEMENT                         
*                                                                               
FIXCL00  L     R2,SBAIO3           A(AIO3)                                      
         MVI   BYTE,1              PASS 1 LOOKS FOR CHECK NUMS                  
*                                                                               
FIXCL05  LR    RF,R1               A(FIRST X'03' ELEMENT)                       
*                                                                               
FIXCL10  CLI   0(RF),0             END OF RECORD?                               
         BE    FIXCL25             YES                                          
         CLI   0(RF),X'01'         HIT THE NEXT X'01' ELEMENT?                  
         BE    FIXCL25             YES                                          
         CLI   0(RF),X'03'         X'03' ELEMENT?                               
         BE    FIXCL20             YES - SKIP TO NEXT ELEMENT                   
         CLI   0(RF),X'05'         X'05' INVOICE ELEMENT?                       
         BNE   FIXCLX              NO - DONE - BAD CLRST RECORD                 
*                                                                               
         USING CLSTEL05,RF         INVOICE ELEMENT DSECT                        
         CLI   BYTE,1              PASS 1?                                      
         BNE   *+14                NO                                           
         OC    CLS5CHK,CLS5CHK     HAVE A CHECK NUMBER?                         
         BNZ   FIXCL15             YES - WRITE THIS ELEM PAIR TO BUFF           
         CLI   BYTE,2              PASS 2?                                      
         BNE   FIXCL20             NO                                           
         OC    CLS5CHK,CLS5CHK     HAVE A CHECK NUMBER?                         
         BNZ   FIXCL20             YES - IGNORE                                 
         DROP  RF                  DROP INVOICE ELEMENT USING                   
*                                                                               
FIXCL15  LLC   RE,1(R3)            X'03' ELEMENT LENGTH                         
         LLC   R0,1(RF)            X'05' ELEMENT LENGTH                         
         AR    RE,R0               MOVE BOTH ELEMENTS                           
         BCTR  RE,0                -1 FOR EX                                    
         EX    RE,*+8              EXECUTE THE MVC                              
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   0(0,R2),0(R3)       MOVE X'03'/X'05' ELEMENTS TO SBAIO3          
         LA    R2,1(R2,RE)         BUMP R2 PAST X'03'/X'05' ELEM PAIR           
*                                                                               
FIXCL20  LR    R3,RF               SAVE ELEMENT ADDRESS                         
         LLC   RE,1(RF)            ELEMENT LENGTH                               
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     FIXCL10             PROCESS NEXT ELEMENT                         
*                                                                               
FIXCL25  CLI   BYTE,2              FINISHED PASS 2?                             
         BE    FIXCL30             YES                                          
         MVI   BYTE,2              PASS 2 LOOKS FOR MISSING CHECK NUMS          
         B     FIXCL05             PROCESS PASS 2                               
*                                                                               
FIXCL30  L     RE,SBAIO3           A(AIO3)                                      
         SR    R2,RE               LENGTH TO MOVE                               
*                                                                               
         LR    R0,R1               COPY ELEMENTS IN CORE STARTING HERE          
         LR    R1,R2               COPY FOR THIS MANY BYTES                     
         L     RE,SBAIO3           BUY RECORD IS IN SBAIO1                      
         LR    RF,R1               COPY FOR THIS MANY BYTES                     
         MVCL  R0,RE               COPY RE-ORDERED X'03'/X'05' ELEMS            
*                                                                               
FIXCLX   J     XIT                 DONE                                         
*                                                                               
GLOBALS  DS    0D                                                               
*                                                                               
SVKEY    DS    CL(L'KEY)                                                        
SVMKT    DS    XL2                                                              
SVMKTFLG DS    XL1                                                              
SVORIG   EQU   X'80'                                                            
SVSPILL  EQU   X'40'                                                            
*                                                                               
SVPGRP   DC    XL2'FFFF'                                                        
*                                                                               
APRD     DS    A                                                                
ABOOK    DS    A                                                                
AXSPILL  DS    A                                                                
BOOKS    DS    CL24                                                             
SVSTABNT DS    F                                                                
*                                                                               
ALPHATAB DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
FF       EQU   X'FF'                                                            
XFF      DC    XL24'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'           
         DC    XL24'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'           
BLANKS   DC    96C' '                                                           
*                                                                               
PGDATA   DS    CL(PGDATAL)         PG ESTIMATE DATA                             
PGHEAD   DC    CL87'CHRGPER ACCOUNT BRAND ESTIMATE EVENT-CODE MULTI-BRAX        
               ND NOBRAND SR-START ORIGEST BRD-SUFF'                            
         EJECT                                                                  
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
         DS    0D                  ALIGNMENT                                    
WPKDIVD  DS    0PL16               DIVIDEND  - PACKED                           
WPKQUOT  DS    PL8                 QUOTIENT  - PACKED                           
WPKREMD  DS    PL8                 REMAINDER - PACKED                           
*                                                                               
CHUNKLEN DS    XL1                 CHUNK LENGTH                                 
CHKSEQ   DS    XL6                 CHUNK SEQ NUMBER                             
SVCHUNK  DS    XL250               SAVED CHUNK ENTRY                            
CHUNKFLG DS    XL1                 CHUNK FLAG                                   
*                                                                               
DISCL1   DC    C'DOLLAR FIGURES AS PRESENTED MAY VARY FROM ACTUAL '             
         DC    C'DOLLARS INVOICED AND'                                          
DISCL1L  EQU   *-DISCL1                                                         
DISCL2   DC    C'FROM INDIVIDUAL SCHEDULES DUE TO ROUNDING.'                    
DISCL2L  EQU   *-DISCL2                                                         
*                                                                               
CLTSKIP  DC    C'REPORT DOES NOT INCLUDE UNAUTHORIZED CLIENTS.'                 
CLTSKIPL EQU   *-CLTSKIP                                                        
*                                                                               
FIRSTAB  DC    AL1(1),AL3(FMED)    MEDIA                                        
         DC    AL1(3),AL3(FCLT)    CLIENT                                       
         DC    AL1(2),AL3(FPGR1)   PRDGRP1                                      
         DC    AL1(2),AL3(FPGR2)   PRDGRP2                                      
         DC    AL1(3),AL3(FPRD)    PRODUCT                                      
         DC    AL1(3),AL3(FEST)    ESTIMATE                                     
         DC    AL1(2),AL3(FSX)     MKTGRP1                                      
         DC    AL1(2),AL3(FSX)     MKTGRP2                                      
         DC    AL1(2),AL3(FSX)     MKTGRP3                                      
         DC    AL1(2),AL3(FMKT)    MARKET                                       
         DC    AL1(5),AL3(FSX)     STATION                                      
         DC    AL1(1),AL3(FSX)     AFFILIATE                                    
         DC    AL1(3),AL3(FSX)     BOOK                                         
         DC    AL1(4),AL3(FSX)     CHANNEL                                      
         DC    AL1(7),AL3(FSX)     DPT                                          
         DC    AL1(4),AL3(FSX)     PERIOD                                       
         DC    AL1(8),AL3(FSX)     CML                                          
         DC    AL1(1),AL3(FSX)     MARKET RANK                                  
         DC    AL1(1),AL3(FSX)     SUBMEDIA                                     
         DC    AL1(11),AL3(FCLT)   CLTNAME                                      
         DC    AL1(14),AL3(FPRD)   PRDNAME                                      
         DC    AL1(10),AL3(FEST)   ESTNAME                                      
         DC    AL1(10),AL3(FMKT)   MKTNAME                                      
         DC    AL1(01),AL3(FSX)    CLTOFF                                       
         DC    AL1(39),AL3(FSX)    BUYLINE                                      
         DC    AL1(01),AL3(FSX)    REPORT SEQUENCE                              
         DC    AL1(12),AL3(FSX)    BUY ID                                       
         DC    AL1(03),AL3(FSX)    SPECIAL REP                                  
         DC    AL1(12),AL3(FSX)    BUYER/BILLER NAMES                           
         DC    AL1(04),AL3(FSX)    RANK                                         
         DC    AL1(01),AL3(FSX)    BUY TYPE                                     
         DC    AL1(04),AL3(FSX)    PRODUCT INTERFACE CODE                       
         DC    AL1(08),AL3(FSX)    DPTLEN                                       
         DC    AL1(01),AL3(FSX)    LEN                                          
         DC    AL1(03),AL3(FSX)    ESTFILT                                      
         DC    AL1(07),AL3(FTGT)   TARGET                                       
         DC    AL1(07),AL3(FTGT2)  TARGET2                                      
         DC    AL1(03),AL3(FSX)    SREP                                         
         DC    AL1(03),AL3(FSX)    PREP                                         
*                                  FOLLOWING BILL HEADER ENTRIES                
         DC    AL1(1),AL3(FSX)     AOR BILL TYPE                                
         DC    AL1(1),AL3(FSX)     AOR COMM TYPE                                
         DC    AL1(6),AL3(FSX)     INVOICE NUMBER                               
         DC    AL1(12),AL3(FSX)    RETAIL ACCOUNT NUMBER                        
         DC    AL1(2),AL3(FSX)     BILL TYPE                                    
         DC    AL1(2),AL3(FSX)     CLIENT ACCOUNTING OFFICE CODE                
         DC    AL1(8),AL3(FSX)     CLIENT INTERFACE CODE                        
         DC    AL1(4),AL3(FSX)     PRODUCT INTERFACE CODE                       
         DC    AL1(7),AL3(FSX)     STATION TYPE                                 
         DC    AL1(24),AL3(FSX)    AOR AGENCY NAME                              
         DC    AL1(14),AL3(FSX)    AOR INCOME ACCOUNT                           
         DC    AL1(4),AL3(FSX)     AOR PERCENTAGE                               
         DC    AL1(14),AL3(FSX)    AOR RECEIVABLE/PAYABLE ACCOUNT               
         DC    AL1(9),AL3(FSX)     AOR EFFECTIVE DATE                           
         DC    AL1(6),AL3(FSX)     INVOICE DATE (MONTH/YEAR)                    
         DC    AL1(6),AL3(FSX)     DUE DATE (MONTH/YEAR)                        
         DC    AL1(7),AL3(FSX)     POST DATE (MONTH/YEAR)                       
         DC    AL1(6),AL3(FSX)     RUN DATE (MONTH/YEAR)                        
         DC    AL1(7),AL3(FSX)     MONTH OF SERVICE                             
         DC    AL1(2),AL3(FSX)     SINGLE -MAKES EACH LINE UNIQUE               
         DC    AL1(6),AL3(FSX)     RUN DATE                                     
         DC    AL1(2),AL3(FSX)     POST DATE                                    
         DC    AL1(6),AL3(FSX)     INVOICE DATE                                 
         DC    AL1(3),AL3(FSX)     DUE DATE                                     
*                                                                               
         DC    6AL4(0)                                                          
*                                                                               
         DC    AL1(04),AL3(FMKT)   MARKET RANKED                                
         DC    AL1(12),AL3(FMKT)   MARKET NAME RANKED                           
         DC    AL1(01),AL3(FSX)    ADJACENCY CODE                               
         DC    AL1(02),AL3(FSX)    AFFIDAVIT DATE                               
         DC    AL1(02),AL3(FSX)    AFFIDAVIT TIME                               
         DC    AL1(03),AL3(FSX)    TREP                                         
         DC    AL1(02),AL3(FSX)    CLIENT GROUP 1                               
         DC    AL1(02),AL3(FSX)    CLIENT GROUP 2                               
         DC    AL1(02),AL3(FSX)    STATION GROUP 1                              
         DC    AL1(02),AL3(FSX)    STATION GROUP 2                              
         DC    AL1(08),AL3(FSX)    NETSTA                                       
         DC    AL1(04),AL3(FSX)    CMLCLASS                                     
         DC    AL1(17),AL3(FSX)    PROGRAM                                      
         DC    AL1(04),AL3(FSX)    AFFIDAVIT DAY                                
         DC    AL1(16),AL3(FSX)    AFFIDAVIT PROGRAM                            
         DC    AL1(04),AL3(FSX)    TIMES                                        
         DC    AL1(02),AL3(FSX)    ROTATION                                     
         DC    AL1(01),AL3(FSX)    TEXT                                         
         DC    AL1(00),AL3(FSX)    DOWNLOAD                                     
         DC    AL1(02),AL3(FSX)    RETAIL SCHEME CODE                           
         DC    AL1(01),AL3(FSX)    CLEARANCE STATUS                             
         DC    AL1(24),AL3(FSX)    STATION NAME                                 
         DC    AL1(24),AL3(FSX)    STATION CITY                                 
         DC    AL1(03),AL3(FSX)    STATION STATE                                
         DC    AL1(47),AL3(FSX)    STATION ADDRESS                              
         DC    AL1(02),AL3(FSX)    BUYLINE COMMENT                              
         DC    AL1(13),AL3(FSX)    BUYLINE NUMBER                               
         DC    AL1(15),AL3(FSX)    CABLE SYSTEM MSO NAME                        
         DC    AL1(20),AL3(FSX)    CABLE SYSTEM INTERCONNECT NAME               
         DC    AL1(01),AL3(FSX)    STATION SIZE                                 
         DC    AL1(01),AL3(FSX)    USER DEFINITION                              
         DC    AL1(03),AL3(FSX)    CABLE NETWORK                                
         DC    AL1(24),AL3(FSX)    COMMERCIAL CLASS NAME                        
         DC    AL1(04),AL3(FSX)    STATION FORMAT                               
         DC    AL1(12),AL3(FSX)    STATION FAX                                  
         DC    AL1(01),AL3(FSX)    INFOMERCIAL STATUS                           
         DC    AL1(01),AL3(FSX)    INFOMERCIAL TYPE                             
         DC    AL1(03),AL3(FSX)    INFOMERCIAL DATE                             
         DC    AL1(02),AL3(FSX)    INFOMERCIAL TIME                             
         DC    AL1(02),AL3(FSX)    EST-LIN                                      
         DC    AL1(06),AL3(FSX)    BUY DATES                                    
         DC    AL1(01),AL3(FSX)    # BUY WEEKS                                  
         DC    AL1(04),AL3(FSX)    ESTIMATE DATES                               
         DC    AL1(02),AL3(FSX)    INFOMERCIAL RESPONSE DATE                    
         DC    AL1(02),AL3(FSX)    INFOMERCIAL RESPONSE DATE WEEKLY             
         DC    AL1(02),AL3(FSX)    INFOMERCIAL RESPONSE DATE MONTHLY            
         DC    AL1(20),AL3(FSX)    CLIENT CML NUM                               
         DC    AL1(01),AL3(FSX)    INFOMERCIAL BUY CHECK                        
         DC    AL1(05),AL3(FSX)    AT&T MEDIA CODE                              
         DC    AL1(02),AL3(FSX)    AFFID DATE REVERSED                          
         DC    AL1(02),AL3(FSX)    AFFID TIME MILITARY                          
         DC    AL1(04),AL3(FSX)    MARKET WEIGHT                                
         DC    AL1(03),AL3(FSX)    BUY CREATION DATE                            
         DC    AL1(02),AL3(FSX)    GOAL CREATION DATE                           
         DC    AL1(02),AL3(FSX)    AFFID DATE FOR DOWNLOAD (MM/DD/YY)           
         DC    AL1(12),AL3(FSX)    STA/AFFIL/CHAN                               
         DC    AL1(02),AL3(FSX)    STATION BILLING INVOICE NUMBER               
         DC    AL1(04),AL3(FSX)    BUY DOLLARS (ROW)                            
         DC    AL1(24),AL3(FSX)    STATION ADDRESS                              
         DC    AL1(03),AL3(FSX)    ESTIMATE PCT                                 
         DC    AL1(25),AL3(FSX)    HOME DEPOT CMML                              
         DC    AL1(03),AL3(FSX)    CLT PCT                                      
         DC    AL1(06),AL3(FSX)    ID REFERENCE                                 
         DC    AL1(04),AL3(FSX)    CANADIAN CABLE NET                           
         DC    AL1(10),AL3(FSX)    INVOICE NUMBER                               
         DC    AL1(02),AL3(FSX)    INVOICE DATE (DUE DATE)                      
         DC    AL1(02),AL3(FSX)    AGENCY CODE                                  
         DC    AL1(02),AL3(FSX)    NWS CAMPAIGN NUMBER                          
         DC    AL1(03),AL3(FSX)    NWS BUYER                                    
         DC    AL1(03),AL3(FSX)    NWS DATE                                     
         DC    AL1(04),AL3(FSX)    COST                                         
         DC    AL1(01),AL3(FSX)    NETSIZE                                      
         DC    AL1(10),AL3(FSX)    STAZIP                                       
         DC    AL1(3),AL3(FSX)     MKTA (ALPHA MARKET CODE)                     
         DC    AL1(1),AL3(FSX)     NAME                                         
         DC    AL1(2),AL3(FSX)     BILL HEADER EDI DATE (BHEDATE)               
         DC    AL1(4),AL3(FSX)     PAID REP (FROM CLRST REC)                    
         DC    AL1(2),AL3(FSX)     SQDDPT                                       
         DC    AL1(2),AL3(FSX)     BILINVDT                                     
         DC    AL1(1),AL3(FSX)     USER COMMENT                                 
         DC    AL1(2),AL3(FSX)     RATING SERVICE MARKET                        
         DC    AL1(4),AL3(FSX)     BUY COPY DATE/TIME                           
         DC    AL1(16),AL3(FSX)    CMLBCODE                                     
         DC    AL1(2),AL3(FSX)     RUNDATE                                      
         DC    AL1(3),AL3(FSX)     BONUS                                        
         DC    AL1(1),AL3(FSX)     INVSTAT                                      
         DC    AL1(1),AL3(FSX)     BLANK                                        
         DC    AL1(4),AL3(FSX)     INVSRCE                                      
         DC    AL1(50),AL3(FSX)    MSFORMAT                                     
         DC    AL1(5),AL3(FSX)     MSFREQ                                       
         DC    AL1(36),AL3(FSX)    MSOWNER                                      
         DC    AL1(5),AL3(FSX)     MSPARENT                                     
         DC    AL1(2),AL3(FSX)     MSSTATE                                      
         DC    AL1(5),AL3(FSX)     MSCHAN                                       
         DC    AL1(50),AL3(FSX)    MSNET                                        
         DC    AL1(4),AL3(FSX)     SHOW CODE                                    
         DC    AL1(3),AL3(FSX)     ETYPE                                        
         DC    AL1(1),AL3(FSX)     OMSTATUS                                     
         DC    AL1(4),AL3(FSX)     OMSTDT                                       
         DC    AL1(4),AL3(FSX)     OMORDNUM                                     
         DC    AL1(3),AL3(FSX)     OMBUYER                                      
         DC    AL1(1),AL3(FSX)     OMFLIGHT                                     
         DC    AL1(25),AL3(FSX)    OMSLP                                        
         DC    AL1(25),AL3(FSX)    OMPOINT                                      
         DC    AL1(2),AL3(FSX)     OMCITY                                       
         DC    AL1(24),AL3(FSX)    OMNAME                                       
         DC    AL1(20),AL3(FSX)    OMROUTE                                      
         DC    AL1(1),AL3(FSX)     OMDEST                                       
         DC    AL1(8),AL3(FSX)     OMREPCON                                     
         DC    AL1(10),AL3(FSX)    OMAGYID                                      
         DC    AL1(10),AL3(FSX)    OMREPID                                      
         DC    AL1(12),AL3(FSX)    AD-ID                                        
         DC    AL1(2),AL3(FSX)     INVDAYS                                      
         DC    AL1(2),AL3(FSX)     INVFDATE                                     
         DC    AL1(1),AL3(FSX)     INVFPER                                      
         DC    AL1(2),AL3(FSX)     INVLDATE                                     
         DC    AL1(1),AL3(FSX)     INVLPER                                      
         DC    AL1(1),AL3(FSX)     BYDEMSRC                                     
         DC    AL1(4),AL3(FSX)     ORBIT                                        
         DC    AL1(1),AL3(FSX)     EQLEN                                        
         DC    AL1(1),AL3(FSX)     ERATE                                        
         DC    AL1(2),AL3(FSX)     GLKBDATE                                     
         DC    AL1(2),AL3(FSX)     GLKGDATE                                     
         DC    AL1(1),AL3(FSX)     INVFLAGS                                     
         DC    AL1(10),AL3(FSX)    OMLSTSNT                                     
         DC    AL1(3),AL3(FSX)     MSMINOWN                                     
         DC    AL1(3),AL3(FSX)     MSMALCRY                                     
         DC    AL1(3),AL3(FSX)     MSMALCTY                                     
         DC    AL1(3),AL3(FSX)     MSMALSTA                                     
         DC    AL1(3),AL3(FSX)     MSMALST1                                     
         DC    AL1(3),AL3(FSX)     MSMALST2                                     
         DC    AL1(3),AL3(FSX)     MSMALZIP                                     
         DC    AL1(3),AL3(FSX)     MSTRACRY                                     
         DC    AL1(3),AL3(FSX)     MSTRACTY                                     
         DC    AL1(3),AL3(FSX)     MSTRASTA                                     
         DC    AL1(3),AL3(FSX)     MSTRAST1                                     
         DC    AL1(3),AL3(FSX)     MSTRAST2                                     
         DC    AL1(3),AL3(FSX)     MSTRAZIP                                     
         DC    AL1(1),AL3(FSX)     ESTBKTYP                                     
         DC    AL1(10),AL3(FSX)    BHINVM                                       
         DC    AL1(8),AL3(FSX)     BHINVMND                                     
         DC    AL1(12),AL3(FSX)    HDEF                                         
         DC    AL1(12),AL3(FSX)    CNTR                                         
         DC    AL1(4),AL3(FSX)     FILMTYPE                                     
         DC    AL1(1),AL3(FSX)     LNBKTYPE                                     
         DC    AL1(3),AL3(FSX)     OMFRTSNT                                     
         DC    AL1(6),AL3(FSX)     MFID#                                        
         DC    AL1(12),AL3(FSX)    CLINV                                        
         DC    AL1(3),AL3(FSX)     CLPRD                                        
         DC    AL1(3),AL3(FSX)     CLEST                                        
         DC    AL1(1),AL3(FSX)     RATETYPE                                     
         DC    AL1(20),AL3(FSX)    CMLNAME2/3                                   
         DC    AL1(20),AL3(FSX)    BUYBOOK                                      
         DC    AL1(4),AL3(FSX)     OMEMADD                                      
         DC    AL1(3),AL3(FSX)     INVREP                                       
         DC    AL1(8),AL3(FSX)     INVAGYID                                     
         DC    AL1(1),AL3(FSX)     NTYPE                                        
         DC    AL1(1),AL3(FSX)     INVCMAT                                      
         DC    AL1(4),AL3(FSX)     BILINVMY                                     
         DC    AL1(4),AL3(FSX)     MGDTL                                        
         DC    AL1(1),AL3(FSX)     OMRESEND                                     
         DC    AL1(1),AL3(FSX)     MSMINFEM                                     
         DC    AL1(1),AL3(FSX)     MSFCCMIN                                     
         DC    AL1(1),AL3(FSX)     MSFCCFEM                                     
         DC    AL1(07),AL3(FTGT3)  TARGET3                                      
         DC    AL1(07),AL3(FTGT4)  TARGET4                                      
         DC    AL1(2),AL3(FSX)     OMMGCODE                                     
         DC    AL1(5),AL3(FSX)     OMDTRECD                                     
         DC    AL1(5),AL3(FSX)     OMDTLST                                      
         DC    AL1(1),AL3(FSX)     OMTRVSN                                      
         DC    AL1(1),AL3(FSX)     OMLSTST                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPWRI01D                                                       
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDMASTD                                                                        
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*CTGENFILE                                                                      
*SPGENAGY                                                                       
*SPGENCLT                                                                       
*SPGENPRD                                                                       
*SPGENEST                                                                       
*SPGENSTA                                                                       
*SPGENADD                                                                       
*SPGENREP                                                                       
*SPGENCBL                                                                       
*SPGENBUY                                                                       
*SPGENGOAL                                                                      
*SPGENSTAB                                                                      
*SPGENPRG                                                                       
*SPGENDMN                                                                       
*SPGENSTAT                                                                      
*SPGENFLT                                                                       
*SPGENXLK                                                                       
*SPGENPGEST                                                                     
*SPGENCLRST                                                                     
*SPGENINFO                                                                      
*SPGENWIPW                                                                      
*SPGENPURP                                                                      
*SPDEMLK                                                                        
*SPOTTABD                                                                       
*SPEDICT                                                                        
*SPWRIFFD                                                                       
*MAAORLKD                                                                       
*SPSQDBUFFD                                                                     
*DDBSRPRMD                                                                      
*GEGENRFPD                                                                      
*SPDDEQUS                                                                       
*DDUCOMD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FASSBOFF                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
ADDRRECD DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
CBLRECD  DSECT                                                                  
       ++INCLUDE SPGENCBL                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
INVRECD  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENDMN                                                       
       ++INCLUDE SPGENSTAT                                                      
       ++INCLUDE SPGENFLT                                                       
       ++INCLUDE SPGENXLK                                                       
       ++INCLUDE SPGENPGEST                                                     
       ++INCLUDE SPGENCLRST                                                     
       ++INCLUDE SPGENINFO                                                      
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPGENPURP                                                      
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENDRMKN                                                     
       ++INCLUDE SPADBUYER                                                      
*                                                                               
REQHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
FQRECORD DSECT                     SPONSOR STYLE REQUEST CARD                   
FQAREA   DS    0CL80   COLUMN                                                   
FQPROG   DS    0CL2    ------                                                   
FQCODE   DS    CL2        1        PROGRAM CODE                                 
FQAGY    DS    CL2        3        AGENCY CODE                                  
FQMED    DS    CL1        5        MEDIA CODE (R/T)                             
FQCLT    DS    CL3        6        CLIENT CODE                                  
FQPGR    DS    CL1        9        PROCESS BY DIVISION                          
FQMGR    DS    CL1       10        PROCESS BY DISTRICT                          
FQCLOFFC DS    CL1       11        CLIENT OFFICE FILTER                         
FQBYID   EQU   FQCLOFFC            C'Y' IF BUYS PROCESSED BY ID                 
FQPRD    DS    CL3       12        PRODUCT MNEMONIC                             
FQMKT    DS    CL4       15        MARKET NUMBER                                
FQSTA    DS    CL5       19        STATION CALL LETTERS                         
FQEST    DS    CL3       24        ESTIMATE NUMBER                              
FQESTEND DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
FQDEMOVRD DS   CL1       30        Y=DEMO OVERRIDE ACTIVE                       
FQCONTREQ DS   CL1       31        C'*' ==> DATA IN QAREA2                      
FQSTAUTO DS    CL3       32        AUTO REQUEST START DATE                      
FQENDAUTO DS   CL3       35        AUTO REQUEST END DATE                        
         ORG   FQSTAUTO+2                                                       
FQDEMNOS DS    CL4                 DEMO OVERRIDE NUMBERS                        
FQSTART  DS    CL6       38        REQUEST START DATE                           
FQEND    DS    0CL6      44        REQUEST END DATE                             
FQTODAY  DS    CL6       44                                                     
FQBOOK1  DS    CL4       50        RATING BOOK (YYMM) GOAL/ESTD DATA            
FQHUT1   DS    CL2       54        HUT ADJUSTMENT MONTH                         
FQRERATE DS    CL1       56        RERATE TYPE  I=INVOICE                       
*                                               P=PURCHASED                     
*                                               A=ADJUST ONLY                   
*                                               U=UPGRADE (+Q2BOOK2)            
FQCOMPARE DS   CL1       57        DATA COMPARE OPTION                          
*                                  A=GOAL V PURCHASED                           
*                                  B=GOAL V AFFIDAVIT                           
*                                  C=PURCHASED V PURCHASED (RERATED)            
*                                  D=PURCHASED V AFFIDAVIT                      
*                                  E=LOCKIN V PURCHASED                         
*                                  F=LOCKIN V AFFIDAVIT                         
*                                  L=GOAL V PURCHASED, LOCKIN PURCHASED         
FQAFFIL  DS    CL1       58        AFFILIATION FILTER                           
FQPRGTYPE DS   CL1       59        PROGRAM TYPE FILTER                          
FQDPTDET DS    CL1       60        DAYPART DETAIL CONTROL                       
*                                  A=SHOW FULL DETAIL (DEFAULT)                 
*                                  B=SUPPRESS SPOT-LENGTH                       
*                                  C=SUPPRESS SPOT-LENGTH & DAY-PART            
FQDPTMENU DS   CL1       61        DAYPART MENU OVERRIDE                        
FQOPT1   DS    CL1       62        OPTION 1                                     
FQOPT2   DS    CL1       63        OPTION 2                                     
FQOPT3   DS    CL1       64        OPTION 3                                     
FQOPT4   DS    CL1       65        OPTION 4                                     
FQOPT5   DS    CL1       66        OPTION 5                                     
FQGRP    DS    CL2       67        GROUP                                        
FQFILTER EQU   FQGRP               FILTER TYPE/VALUE                            
FQUESTOR DS    CL12      69        REQUESTOR NAME                               
*                                                                               
SPDEMLKD DSECT                                                                  
       ++INCLUDE SPDEMLK                                                        
       ++INCLUDE SPOTTABD                                                       
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIF3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIF5D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
         EJECT                                                                  
       ++INCLUDE SPWRIWORKD                                                     
         EJECT                                                                  
       ++INCLUDE MAAORLKD                                                       
       ++INCLUDE SPSQDBUFFD                                                     
       ++INCLUDE DDBSRPRMD                                                      
         EJECT                                                                  
* RFP INCLUDES                                                                  
       ++INCLUDE GEGENRFPD                                                      
       ++INCLUDE SPDDEQUS                                                       
*                                                                               
       ++INCLUDE DDUCOMD                                                        
       ++INCLUDE DDTSARD                                                        
SPGBFRDD DSECT                                                                  
       ++INCLUDE SPGETBFRD                                                      
SVBFKEY  DS    XL(SPGBACOM-SPGBAM)                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'163SPWRI01   02/22/21'                                      
         END                                                                    
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 04MAR02 99 EFJ -- FIX SBACONT CODE TO WORK WITH COS2              *           
* 21FEB02 98 EFJ -- SET TRANSMIT BIT ON PERIOD FLD FOR ESC SEQ      *           
* 23OCT01 97 EFJ -- NEW FIRSTAB ENTRY FOR RUNDATE KEYWORD           *           
* 22OCT01 96 EFJ -- FIX ESTTEST FOR POL REQ'S                       *           
* 09OCT01 95 EFJ -- CHANGE *+4 TO *+8 FOR IDF                       *           
* 07SEP01 94 EFJ -- SKIP CALL TO UCOM IF NO MKT                     *           
* 27AUG01 93 EFJ -- SUPPORT MARKET LEVEL USER COMMENTS (UCOM)       *           
* 23AUG01 92 EFJ -- NEW RPRNFRST CALL TO APPLICATIONS               *           
* 02JUN01 91 EFJ -- NEW RPAFTOUT CALL TO APPLICATIONS               *           
* 24APR01 89 EFJ -- NEW FIRSTAB ENTRY FOR CMLBCODE KEYWORD          *           
*                -- BUILD TABLE OF PURPOSE CODES & DESCRIPTIONS     *           
* 02APR01 88 EFJ -- CLEAR MKTTAB AT SBPROCAG                        *           
* 12APR01 87 EFJ -- NEW FIRSTAB ENTRY FOR BUY COPY DATE/TIME KWDS   *           
* 19MAR01 86 EFJ -- SUPPORT ATIME FILTER FOR INVOICE DETAILS        *           
* 21FEB01 85 EFJ -- NEW FIRSTAB ENTRY FOR RSM KEYWORDS              *           
* 20FEB01 84 EFJ -- DONT ALWAYS WANT TO RESET FIRSTLIN AT PRINTX    *           
* 23JAN01 83 EFJ -- SET END ADDRESS OF SBACHUNK IN SBACHNKX AND     *           
*                   CHECK FOR ERRORS AFTER SPOTBUY CALLS            *           
* 02JAN01 82 EFJ -- SUPPORT COST2                                   *           
* 14DEC00 81 EFJ -- FIX FILTER REQUIRED SEC CODE (BROKEN 12/95???)  *           
* 21NOV00 80 EFJ -- COPY CK CODE FOR JM                             *           
* 03OCT00 79 EFJ -- SUPPORT USER COMMENT RECORDS                    *           
* 14SEP00 78 EFJ -- STATION LOCKIN RECORDS NOW ON XSPFIL            *           
* 20JUL00 77 EFJ -- NEW PROFILE - FILTER NOT REQUIRED FOR LIST      *           
* 10MAY00 76 BOB -- READ MARKET 0 BUYS FOR CANADIAN NETWORK REPORTS *           
* 10MAY00 75 BOB -- MAKE BILEDIDT A ROW                             *           
* 08MAR00 74 EFJ -- CHECK DIDEMU WHEN CHECKING DIBYDEM              *           
* 11FEB00 73 EFJ -- MAKE SURE UPT & UPTBK USED TOGETHER             *           
* 02FEB00 72 EFJ -- FORCE DOWNHEAD IF PQIX & DOWNLOADING            *           
* 04JAN99 71 EFJ -- FIRSTAB ENTRY FOR SQDDPT                        *           
*                -- MAKE SPLIT BUY CALLS LIVE                       *           
* 03JAN99 70 EFJ -- SUPPORT NOTAX OPTION (USE OLD SBSPPROF+12 CODE) *           
* 30DEC99 69 EFJ -- MOVE DROP FOR R2                                *           
*                -- MOVE FREE CALL FOR SQAD                         *           
* 21DEC99 68 BOB -- SUPPORT SQAD CPP'S                              *           
* 21DEC99 67 EFJ -- ALLOW SPOTBUY TO SPLIT BUYS (*** NOP'ED ***)    *           
* 08DEC99 65 EFJ -- OPTION TO SUPPRESS POST REPORT WARNINGS         *           
* 17NOV99 64 EFJ -- FIRSTAB ENTRY FOR PDREP KEYWORD                 *           
* 10NOV99 63 EFJ -- FIRSTAB ENTRY FOR BHEDATE KEYWORD               *           
* 23SEP99 62 EFJ -- CHANGE WESTERN TRADE HANDLING                   *           
* 10SEP99 61 EFJ -- DON'T PUT COLUMN LABELS OUT FOR COLUMNS         *           
* 25AUG99 60 EFJ -- SET BACK TO PHASE 'A'                           *           
* 13AUG99 58 EFJ -- OPTION TO PRINT ONLY RECAP TOTALS               *           
* 07JUL99 57 EFJ -- SUPPORT NEW SBPROCB2 MODE TO BUILD TABLE        *           
* 30JUN99 56 EFJ -- SUPPORT TRADE OPTION FOR WESTERN                *           
* 19MAY99 55 EFJ -- CLEAN UP PQIX CODE                              *           
* 18MAY99 54 EFJ -- MORE PQIX WORK                                  *           
* 13MAY99 53 EFJ -- FIX PQIX BUG                                    *           
* 12MAY99 52 EFJ -- ADD <REQNAME ...> TO PQIX (GSDM) OUTPUT         *           
* 06MAY99 51 EFJ -- RE-ENABLE OLD INVOICE CODE BY PATCH             *           
* 19MAR99 50 EFJ -- DON'T FILTER INVOICE HDR ON PRD IF NOT THERE    *           
* 19MAR99 49 EFJ -- ADD <DECL> AND </DECL>                          *           
* 15MAR99 48 EFJ -- BR TO THE RIGHT FUCKING LABEL                   *           
* 10MAR99 47 EFJ -- ALWAYS PRINT INDEX IF OPTPQIX SET               *           
*                -- PRINT HEADERS WITH INDEX LINES                  *           
* 24FEB99 46 EFJ -- FIX PROFILE READING ACROSS CLTS                 *           
* 19FEB99 45 EFJ -- ONLY ALLOCTATE PQINDEX IF SOON OR DIRECT        *           
*                   OR OPTIND5 HAS PQIX SET (FOR TESTING)           *           
*                -- PQIX REVISIONS                                  *           
* 18FEB99 44 EFJ -- DON'T ALLOCATE PQINDEX TABLE IF NOT NEEDED      *           
* 17FEB99 43 EFJ -- SUPPORT INVACTDT                                *           
* 09FEB99 42 EFJ -- RE-VAMP PQIX                                    *           
* 06JAN99 41 EFJ -- SKIP PRE-VAL CHECK FOR RFP                      *           
* 06JAN99 40 EFJ -- SUPPORT 4 BOOKS                                 *           
* 01DEC98 39 EFJ -- PRINT INDEX CARD IF PQIX=Y IS PRESENT           *           
* 13NOV98 38 EFJ -- FIX BUG NOT SHOWING FILTER FOR FIRST SELECT     *           
* 09NOV98 37 EFJ -- CODE TO SUPPORT WRITERS W/OUT SPOTIO            *           
* 29OCT98 36 EFJ -- SUPPORT FOR NAME KEYWORD                        *           
* 06JUL98 35 EFJ -- SAVE PWGROSS & NET FOR CPP CALCULATION          *           
* 24JUN98 34 NRK -- FIRSTAB ENTRY FOR MKTA (ALPHA MKT CODE) KEYWORD *           
* 01JUN98 33 EFJ -- FIRSTAB ENTRY FOR STAZIP KEYWORD                *           
* 26MAY98 32 EFJ -- CROSS EST REPORTING FOR PW                      *           
* 20MAY98 31 EFJ -- FIX SPECIAL REP CODE FOR INVOICES               *           
* 11MAY98 30 EFJ -- UPDATE FOR NEW STYLE CABLE HEADENDS             *           
* 14APR98 29 EFJ -- SUPPORT FOR NSIZE KEYWORD                       *           
* 26FEB98 28 EFJ -- COST KEYWORD                                    *           
* 18FEB98 27 EFJ -- MAKE SURE TO FREE CORE IF TAKING PW ERROR EXIT  *           
* 02FEB98 26 EFJ -- DON'T CLEAR AFFID AREA BETWEEN DEMO TYPES S.T.  *           
*                   AFFID DATA STAYS AROUND BETWEEN SPOTBUY CALLS   *           
* 27JAN98 25 EFJ -- EXIT IF PW & ACROSS ESTS (DON'T DIE)            *           
* 12NOV97 24 EFJ -- SET NEW MODE SBPROCB1 FOR CUR=X'01' IN PROCBL   *           
* 11NOV97 23 EFJ -- RESET SBQROSTA FOR PW (SET IN WRIGEN)           *           
* 05NOV97 22 EFJ -- PROCESS NO-DATA CHUNKS IF SRC FILTER            *           
* 06OCT97 21 EFJ -- ALLOW LIST W/OUT FILTER IF AUTH=X'10'           *           
* 10SEP97 20 EFJ -- FIX NINV PRD FILTERING                          *           
* 19AUG97 19 EFJ -- FIX TWAOUT FOR RFP DOWNLOADS                    *           
* 06JUN97 18 EFJ -- GET MKTGRPS IF MEDIA CHANGES                    *           
* 10APR97 17 EFJ -- NEED BIGGER WORK AREA FOR NINV CMMLS            *           
* 06FEB97 16 EFJ -- ALLOW CROSS CLT W/CMML KEYWORDS FOR BJ FOR OV   *           
* 21JAN97 15 WHO -- IGNORE INVOICES THAT ARE MARKED PURGED          *           
* 13JAN97 14 BOB -- IMPLEMENT CASHFLOW                              *           
* 08JAN97 13 EFJ -- NOP NEW TARGET FIRST CODE (NEVER WENT LIVE)     *           
* 26DEC96 12 EFJ -- PULL DEMO AT TARGET FIRST                       *           
*                -- REMOVE TEMP FIX FOR NINV (LEV 10)               *           
* 18DEC96 11 EFJ -- SET REQ'D MKT IN SBAMGTAB FOR XFIL              *           
*                -- SKIP SPILL CHECKS FOR XFIL                      *           
* 16DEC96 10 EFJ -- TEMP FIX FOR NINV RECS (HDR EST <> DET EST)     *           
* 12DEC96 09 EFJ -- FIX NOT SWITCHING PRDS...                       *           
* 05NOV96 08 EFJ -- MORE CABLE AGG FIXES                            *           
* 30OCT96 07 EFJ -- OPTION TO REPORT PIGS AS ENTERED (NOT BY ALPHA) *           
* 30OCT96 06 EFJ -- RE-NUMBER PROCBY LABELS                         *           
* 28OCT96 05 EFJ -- NEW FIRSTAB ENTRIES                             *           
* 22OCT96 04 EFJ -- CALL PUTCBLC ON SBPROCSP BASED ON SBBUYCH       *           
* 22JUL96 03 EFJ -- SUPPORT MULTIPLE FILE READING                   *           
*                -- FIRSTAB ENTRY FOR AGYCODE                       *           
* 22JUL96 02 EFJ -- RE-NUMBER PREP LABELS                           *           
* 22JUL96 01 EFJ -- LEVEL RESET                                     *           
*-------------------------------------------------------------------*           
*                                                                   *           
* 08JUL96 89 EFJ -- DROP SUPPORT OF SBSPPROF+12 (SUB TAX)           *           
* 17JUN96 88 EFJ -- **TEMP** SUPPORT NOIDR FILTER                   *           
* 13JUN96 87 EFJ -- GET PRD & EST UDEF INFO WHEN PROCESSING INV'S   *           
* 08MAY96 86 EFJ -- SUPPORT NO DEM/CCP WEEK LIMIT PROFILE           *           
* 03MAY96 85 EFJ -- SUPPORT FILM CODES FROM INVOICE RECS            *           
* 11APR96 84 EFJ -- ALLOW CMML ROWS & ALL CLT REQ FOR COKE          *           
* 26MAR96 83 EFJ -- SET REQ FLT FLD INTENSITY FOR DDS               *           
* 028FEB6 82 EFJ -- REMOVE NOP SECURITY (REPLACED BY WL PROF)       *           
* 03JAN96 81 EFJ -- USE GLOBALLN FOR BUFFER SIZE                    *           
* 28DEC95 80 EFJ -- SUPPORT NEW WL PROFILE                          *           
* 21DEC95 79 EFJ -- SUPPORT NETWORKS FOR NEW INVOICES               *           
* 11DEC95 78 EFJ -- SUPPORT INVOICE HEADER COST & SPOTS             *           
* 28NOV95 77 EFJ -- RFP SUPPORT                                     *           
* 27NOV95 76 EFJ -- FIRSTAB ENTRY                                   *           
* 27NOV95 75 EFJ -- SUPPORT INVOICE NUMBER & DATES                  *           
* 08NOV95 74 EFJ -- MORE NEW INVOICE                                *           
* 02NOV95 73 EFJ -- MORE NEW INVOICE                                *           
* 27OCT95 72 EFJ -- SUPPORT FOR NEW INVOICES IN WRITER              *           
* 20OCT95 71 EFJ -- MORE CABLE AGG - PRINT ALL TOTALS               *           
* 18OCT95 70 EFJ -- FIX SF COVAIL CALL                              *           
* 13OCT95 69 EFJ -- PRINT MESSAGE FOR SKIPPED CLTS IN CLTGRP        *           
* 10OCT95 68 EFJ -- MAKE MKTNAME VALID ROW FOR CNET                 *           
* 20SEP95 67 EFJ -- NEW FIRSTAB ENTRY                               *           
*                -- MORE CABLE AGG - RESTRICT CSPOTS & MKT...       *           
* 07SEP95 66 EFJ -- SUPPORT CABLE AGGREGATE                         *           
*                -- USE EQUATE FOR AGLOBAL SIZE                     *           
* 31AUG95 65 EFJ -- INCREASED NAMPOOL                               *           
* 22AUG95 64 EFJ -- ADD ACTADD TO SECURE ACTIONS                    *           
* 14AUG95 63 EFJ -- NEW FIRSTAB ENTRY                               *           
*                -- MORE SECURITY FIXES (LEV 60 INCOMPLETE)         *           
*                -- DON'T PASS PW ELEMS OUTSIDE REQUEST PER         *           
* 09AUG95 62 EFJ -- DON'T USE R8 TO RESOLVE EXTRA STUPID            *           
* 18JUL95 61 EFJ -- CLEAR COL FILTER AREA                           *           
* 12JUL95 60 EFJ -- ADD WI & WT TO SECURITY LIST                    *           
* 05JUL95 59 EFJ -- SUPPORT EXTENDED COL FILTERS - CALL COVAIL TO   *           
*                   ALLOCATE OPTION TABLE                           *           
*                -- USE EQUATES FOR MAX                             *           
*                -- SET ININD WHEN PROCESSING SPILL                 *           
* 28JUN95 58 EFJ -- BREAK OUT CHUNKS BY AFFID TIME IF AFFID TIME    *           
*                   FILTER IS USED                                  *           
* 27JUN95 57 EFJ -- SUPPORT CLTPCT                                  *           
* 15JUN95 56 EFJ -- HDCML KEYWORD - CML NAME FOR HOME DEPOT         *           
* 13JUN95 55 EFJ -- MOVE AFFID TIME FILTER HERE (FROM SPOTIO)       *           
* 12JUN95 54 EFJ -- PROCESS X'16' ELEM FOR PW                       *           
* 01JUN95 53 EFJ -- ALWAYS BUILD WEEK TABLE FOR PW                  *           
*                -- DON'T PROCESS PW OVRD ELEMS WHEN READING GOALS  *           
* 25MAY95 52 EFJ -- FIX SELECT BUGS WITH SECURITY                   *           
* 17MAY95 51 EFJ -- RELAX CHECK FOR NO PERIOD W/BH KEYWORDS FOR     *           
*                   BILLST KEYWORD                                  *           
* 10MAY95 50 EFJ -- FIX BROKEN BOXES                                *           
* 08MAY95 49 EFJ -- SUPPORT FOR PW & BUY LOCK STATUS                *           
* 05MAY95 48 EFJ -- FIX DISCLAIMER HEADHOOK PROBLEMS                *           
* 04MAY95 47 EFJ -- DON'T ALLOW DISPLAY W/OUT FILTER IF REQD (WI)   *           
* 03MAY95 46 EFJ -- SUPPORT DISCLAIMER OPTION                       *           
* 28APR95 45 EFJ -- ONLY PROCESS X'01' INVOICES FOR PW              *           
*                -- SUPPORT ESTPCT                                  *           
* 20APR95 44 EFJ -- SUPPORT STAADDR (STASTRT)                       *           
* 07APR95 43 EFJ -- SUPPORT BYDOLR ROW (FOR MCBUYC)                 *           
* 06APR95 42 EFJ -- SET SBCOMFAC EARLIER (FOR WRIGEN)               *           
* 04APR95 41 EFJ -- YASFPW BUG                                      *           
* 03APR95 40 EFJ -- WESTERN SECURITY                                *           
* 27MAR95 39 EFJ -- SPOTIO NOW PASSING A(PW REC), NOT A(ELEM)       *           
* 21MAR95 38 EFJ -- MORE PFW SHIT                                   *           
* 16MAR95 37 EFJ -- SUPPORT PW OVERRIDE $$$                         *           
*                -- SET DATAIND9 FOR PW BEFORE PROCBUY              *           
* 14MAR95 36 EFJ -- CREATE NEW NMOD FOR 'EXTRA' (PROC RTNS)         *           
* 13MAR95 35 EFJ -- FIX PW BUG                                      *           
* 07MAR95 34 EFJ -- SUPPORT NEW PW FEATURES                         *           
* 27FEB95 33 EFJ -- MAKE XFF BIGGER                                 *           
* 27FEB95 32 EFJ -- SUPPORT LARGER FIELDS FOR PGEST (PGDATA)        *           
* 21FEB95 31 GEP -- SUPPORT FOR NINV RECS (IN PROCINVO)             *           
* 08FEB95 30 EFJ -- TEMP SUPPORT FOR PST                            *           
* 21OCT94 29 EFJ -- SUPPORT NEW CABLE COLS                          *           
* 01SEP94 28 EFJ -- FIX PW BUG                                      *           
* 14JUL94 27 EFJ -- SUPPORT WESTERN PW                              *           
* 12JUL94 26 EFJ -- BREAK OUT MEDIA 'X' FOR MEDIA '*' REQ'S         *           
* 07JUL94 25 EFJ -- SUPPORT FOR STABINV KEYWORD                     *           
* 24JUN94 24 EFJ -- FIX ERROR HANDLING                              *           
* 06JUN94 23 TCS -- REWORK SYSD                                     *           
* 06JUN94 22 EFJ -- REMOVE HARD CODED ERROR MESSAGES                *           
*                -- NOP GENSUM ROUTINE - NEVER USED                 *           
* 01JUN94 21 EFJ -- FIX MEDIA '*' FIX IN FSTA                       *           
* 23MAY94 20 EFJ -- NEW ROW KEYWORD - STAFFCH                       *           
* 23MAY94 19 EFJ -- NEW ROW KEYWORD - ADATED                        *           
* 05MAY94 18 EFJ -- NEW ROW KEYWORDS - BYCDATE & GLCDATE            *           
* 26APR94 17 EFJ -- NEW ROW KEYWORD - MKTWT                         *           
* 15APR94 16 TCS -- GET FORCE=ARB TO WORK FOR BUYS AFTER 1993       *           
* 13APR94 15 EFJ -- INFO BUYER CHECKING FILTER                      *           
* 11APR94 14 EFJ -- SUPPORT FOR ADATER & ATIMEM AS ROWS             *           
* 07APR94 13 EFJ -- FIX MEDIA '*' BUGS                              *           
* 30MAR94 12 EFJ -- SUPPORT FOR ATTCODE AS ROW                      *           
* 22MAR94 11 EFJ -- SUPPORT FOR SECOND COST KEYWORDS                *           
*                -- CHANGED LABEL FROM SBRSPDTS TO SBERSPDT         *           
* 10MAR94 10 EFJ -- SUPPORT FOR IFBYCHK AS ROW                      *           
*                -- PRINT ALL DETAILS ON INFOMERCIAL                *           
*                -- ONLY ALLOW INFO DATA FOR 1 CLT/PRD/EST          *           
*                -- MOVE PROCBH TO OWN NMOD                         *           
* 25FEB94 09 EFJ -- OPTION TO SKIP INFO REPORTING W/OUT BUYS        *           
* 15FEB94 08 EFJ -- INFOMERCIAL RE-WORK (YET AGAIN)                 *           
* 14FEB94 07 EFJ -- SUPPORT FOR NEW ROW - CMLNUM                    *           
* 10FEB94 06 EFJ -- FILTER INFOMERCIAL RESPONSE DATE                *           
* 03FEB94 05 EFJ -- SUPPORT FOR NEW ROWS - IFRSPDT/DTW/DTM          *           
* 20JAN94 04 TCS -- GET NEW DAYPART TABLE IF MEDIA CHANGES          *           
* 18JAN94 03 TCS -- INITILAIZE STATION TABLE ENTRY TO ZEROS         *           
* 06JAN94 02 EFJ -- REWORK OF INFOMERCIALS - SUPPORT ALL STA REC    *           
* 05JAN94 00 EFJ -- HISTORY LOST.  LEVEL RESET                      *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
