*          DATA SET SPOTBUY    AT LEVEL 066 AS OF 02/24/21                      
*PHASE T00A48A,*                                                                
*INCLUDE SPGETBFR                                                               
*INCLUDE LOADER                                                                 
*INCLUDE BRDMON                                                                 
*INCLUDE GETDAY                                                                 
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
* SPOTBUY - T00A48   MOD LOG                                          *         
*                                                                     *         
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT SPEC-54358   02/24/21 PASS ALPHA MARKET FOR CANADIAN AFFIDS    *         
* AKAT SPEC-48035   02/10/21 SUPPORT 2 DECIMAL IMPS FOR UPGRADES      *         
* AKAT SPEC-43482   10/20/20 SUPPORT NEW UNIVERSE KEYWORDS            *         
* AKAT SPEC-30530   11/08/19 SUPPORT FOR 2 DECIMAL IMPRESSIONS        *         
* AKAT SPEC-38080   11/07/19 FIX SBQMED BUG FOR ALL MEDIA SQAD        *         
* AKAT SPEC-28127   06/17/19 SUPPORT NEW LKUP=ALL OPTION              *         
* AKAT SPEC-16250   05/30/18 NEW COST2 KEYWORDS SUPPORT               *         
* AKAT SPEC-22369   05/09/18 CALL SPEGTDEMF FOR RERATE ON AFFID FAIL  *         
* AKAT SPEC-22369   04/23/18 SET P1/DMCB TO SPDEMLK AFTER AFFIDHK     *         
* AKAT SPEC-22251   04/11/18 FIX COMSCORE AFFID BUG                   *         
* AKAT SPEC-20025   02/27/18 ALWAYS PASS ACTUAL BOOK FOR COMSCORE     *         
* AKAT SPEC-20674   02/27/18 ALWAYS PASS ACTUAL BOOK FOR COMSCORE     *         
* AKAT SPEC-20534   02/09/18 OPTIMIZE COMSCORE PASS 1                 *         
* AKAT SPEC-19667   02/01/18 ONLY RERATE AFFIDS FOR COMSCORE DEMOS    *         
* AKAT SPEC-13149   05/24/17 SET PARENT+ STATION FOR COMSCORE         *         
* AKAT SPEC-6939    02/24/17 SUPPORT COMSCORE DEMO LOOKUPS FOR 17.1.5 *         
* AKAT SPEC-15      02/10/17 SUPPORT NEW SQAD CPM                     *         
* AKAT CUSTENH-3341 10/28/16 SUPPORT NEW CANADIAN NATIONAL DEMOS      *         
* AKAT SPSUG-95     06/15/16 SUPPORT NEW AD-IDCD2 KEYWORD             *         
* AKAT CUSTENH-3347 05/19/16 HST INCREASING TO 15% FOR NB 07/01/16    *         
* AKAT SPSUG-20     05/19/16 HST INCREASING TO 15% FOR NF 07/01/16    *         
* AKAT SPSUG-42     05/19/16 HST INCREASING TO 15% FOR PE 10/01/16    *         
* 14MAR13 42  AKAT-- ELIMINATE HST FOR BC AND NEW 14% HST FOR PE      *         
*                 -- STARTING APR01/2013                              *         
* 14NOV12 41  AKAT-- QST RATE INCREASE FOR QUEBEC STARTING JAN01/2013 *         
* 10SEP12 41  AKAT-- SUPPORT SQAD3 AND SQAD4 OPTIONS                  *         
* 05SEP12 40  OPUP-- OVERRIDE WEEKLY BOOK FOR WTP OR LPM WEEKLY       *         
* 21MAR12 39  AKAT-- OPTIMIZATION FOR SBEBYDT                         *         
* 13FEB12 38  AKAT-- FIX SBQMED BUG FOR ALL MEDIA SPILL RADIO AFFIDS  *         
* 15DEC11 37  AKAT-- QST RATE INCREASE FOR QUEBEC STARTING JAN01/2012 *         
* 28NOV11 36  AKAT-- SUPPORT NEW SQAD BOOKTYPE OF Q                   *         
* 28OCT11 35  AKAT-- PASS NEW CLEARANCE STAT DATA IN CHUNK ENTRY      *         
* 25JUL11 34  JNEW-- ADDRESS OPTIMIZATION                             *         
* 05MAY11 33  AKAT-- GET NET FOR PROGRAM EXCHANGE AS WELL AS REG NET  *         
* 16MAR11 32  AKAT-- PROGRAM EXCHANGE SUPPORT                         *         
* 01DEC10 31  AKAT-- QST RATE INCREASE FOR QUEBEC STARTING JAN01/2011 *         
* 05NOV10 30  AKAT-- FIX BFORM BUG                                    *         
* 05NOV10 29  AKAT-- TEST IF MONTAB IS RESOLVED                       *         
* 19OCT10 28  AKAT-- USE CLT/PRD MAIN PST AFTER JUL01/10              *         
* 01DEC09 25  AKAT-- RELINK TO PICK UP NEW VERSION OF SPCNCBLTAB      *         
* 23JUL09 23  AKAT-- SUPPORT NTP=3                                    *         
* 14JAN09 22  AKAT-- SUPPORT NEW BFORM OPTION                         *         
* 08SEP08 20  AKAT-- SBQBOOK NOW 24 BYTES (6 BOOKS - 4 BYTES EACH)    *         
* 21AUG08 19  AKAT-- UPGRADES - PASS BOOKTYPE OVERRIDE (X'24' ELEM)   *         
*                 -- AND DEFAULT TO PURCHASED IF UPGRADES=0           *         
* 18MAR08 18  AKAT-- DON'T PROTECT AGAINST BAD POST BUY DEMOS!        *         
* 12MAR08 17  AKAT-- FIX PBD 2 DECIMAL BUG FROM PREVIOUS VERSION      *         
* 05MAR08 16  AKAT-- PROTECT AGAINST 2 DECIMAL POST BUY DEMOS         *         
* 25FEB08 15  AKAT-- PROTECT AGAINST 2 DECIMAL MANUAL OVERRIDES       *         
* 07JAN08 14  BPOO-- SUPPORT OVN=M OVENRIGHT POSTING OPTION           *         
* 04JAN08 13  EFJ -- SET NEW HST RATE OF 13 PCT FOR CANADA            *         
* 10DEC07 12  EFJ -- SET NEW GST RATE OF 5 PCT FOR CANADA             *         
* 11SEP07 10  AKT -- SPLKDPT & CHKDF ROUTINE DEFUNCT                  *         
* 10JAN07 06  AKT -- PROTECT AGAINST BAD ORBIT ELEMENT LENGTH         *         
* 31OCT06 04  AKT -- PROTECT AGAINST BAD X'02' BUY ELEMENT LENGTH     *         
* 25SEP06 03  AKT -- ALLOW BOOK TYPES 1-4 FOR NEW ZERO CELL DATA      *         
* 13JUN06 02  AKT -- SUPPORT FOR NEW SPOT LENGTHS                     *         
* 24JUL97 01  EFJ -- LEVEL RESET                                      *         
*                 -- CHANGE FROM GVT OPTION TO TRADEG KWD FOR PGM EXCH*         
*---------------------------------------------------------------------*         
* 31MAY06 111 EFJ -- CHANGE FROM GVT OPTION TO TRADEG KWD FOR PGM EXCH*         
*                 -- PUT COS2 CODE IN GOGETRAT                        *         
* 11MAY06 110 EFJ -- REPORT TRADE AT GROSS VALUE FOR PROGRAM EXCH     *         
* 12MAY06 109 AKT -- PASS SPLKDBK TO SPGETDEMF FOR AFFIDS             *         
* 23JAN05 108 AKT -- CHECK L'DLUELEM BEFORE HONORING SOFT DEMO FIELDS *         
* 22DEC05 107 AKT -- SET DEMO SOURCE FOR NEW KEYWORD                  *         
* 08NOV05 106 AKT -- CANADIAN SOFT DEMOS SUPPORT FROM BUY RECORD      *         
* 21OCT05 105 AKT -- ONLY GET SQAD DEMO FOR BUY'S DPT                 *         
* 07SEP05 104 AKT -- PASS CABLE OPTION FLAGS TO SPGETDEMF             *         
* 26MAY05 103 AKT -- LPM OVERNIGHT SUPPORT                            *         
* 09JUN05 102 AKT -- FIX LOOP BUG FOR UPGRADES                        *         
* 25MAY05 101 AKT -- DO NOT EXCLUDE CABLE FOR DEMOS (FUSION SUPPORT)  *         
*                 -- 2 DECIMAL BUG FIX FOR AFFIDS                     *         
* 02MAY05 100 AKT -- SKIP UPGRADES FOR -S BUY IF WEEKLY OPTION=N      *         
* 04APR05 99  AKT -- 2 DECIMAL DEMOS SUPPORT FOR SPDEMUP              *         
* 16MAR05 98  EFJ -- DON'T LOOK UP DEMOS FOR SAME WEEK IN RERATE      *         
* 12FEB05 97  AKT -- DO NOT UPDATE SQAD SCDEMOS IF CHUNK SCDEMOS IS   *         
*                 -- NOT SET                                          *         
* 12JAN05 96  AKT -- DO NOT FILL IN SQAD CPP DATA FOR MISSED/MAKEGOOD *         
* 08DEC04 95  AKT -- SBABKLST ENTRIES ARE NOW 9 BYTES (LPM POSTING)   *         
* 06APR04 95  EFJ -- SUPPORT 2 DEC PRECISION                          *         
* 21SEP04 94  AKT -- SUPRESS LPM DATE FROM MKT IF LPMOPT=N            *         
* 26JUL04 93  AKT -- PASS LPM START DATE TO SPDEMUP                   *         
* 10MAY04 92  EFJ -- FIX NEW TEST FOR CABLE FOR CANADA!               *         
* 02APR04 91  AKT -- MAKE CANADIAN NETWORK WORK IN GETCHK             *         
* 29MAR04 90  EFJ -- NEW TEST FOR CABLE                               *         
* 27FEB04 89  EFJ -- DON'T SET ALPHAMKT FROM DLUELEM IF 0.            *         
* 16DEC03 88  PWE -- CHECK CLT/PRD PST SETTINGS FOR OUTPUT PST(SCPSTO)*         
* 25NOV03 87  PWE -- SCGSTO/SCPSTO HST/GST FIX - BILLED PST IS FUNNY! *         
* 07NOV03 85  EFJ -- PASS NEW SPLKXTND                                *         
* 06NOV03 84  PWE -- OPTIONAL EXTRACT OF MKT0 $S + FIX HST (CANADA)   *         
* 23OCT03 83  AKT -- DBSELSRC MUST BE SET TO R FOR SQAD RADIO         *         
* 24SEP03 82  PWE -- CANADIAN PST                                     *         
* 09SEP03 81  EFJ -- IGNORE OLYNPIC EXCLUSION FOR RADIO!              *         
* 12AUG03 80  AKT -- ? CABLE FIX OF SOME SORT IN GETCHK               *         
* 11AUG03 79  EFJ -- CLEAR ALPHAMKT IN GETAMKT                        *         
* 25JUL03 77  AKT -- READCLS MUST CHECK BUY FOR CALL LETTER CHANGE    *         
* 23JUL03 76  EFJ -- SPLKSPL NOT THE BEST TEST FOR SPILL              *         
* 29MAY03 75  AKT -- SET NTI CODE                                     *         
* 27MAY03 75  EFJ -- PASS UID TO DEMAND FOR SQAD(H)                   *         
* 09APR03 74  EFJ -- HISPANIC BOOKS FOR SQAD                          *         
* 11MAR03 73  EFJ -- SET ALPHAMKT FOR CANADA CABLE                    *         
* 05MAR03 72  EFJ -- USE SPSLNTAB                                     *         
* 12FEB03 71  EFJ -- SQAD FOR RADIO                                   *         
*                 -- SAVE RPAY AND RPAYSEQ IN SPTTAB AND GET CLRST    *         
*                    DATA WHEN BUILDING THE CHUNKS, NOT SPOTTAB       *         
* 13JAN03 70  EFJ -- FIX GETCHK CALL FROM BRAND                       *         
* 06JAN03 69  EFJ -- MORE NTI CODES FOR CHILD SPOT                    *         
* 09AUG02 68  EFJ -- NO IMPS FOR CANADIAN SPILL                       *         
* 10JUL02 67  EFJ -- DON'T PASS BKTYPE 'P' FOR SC (NOT IJ!)           *         
* 09JUL02 66  EFJ -- DON'T PASS BKTYPE 'P' (PEOPLEMETER) FOR IJ       *         
* 26JUN02 65  EFJ -- DON'T PASS BKTYPE 'P' (PEOPLEMETER) FOR YN       *         
* 07JUN02 64  EFJ -- UNDO L63 AND REMOVE JULY EXCLUSION (PER ZEN)     *         
* 03JUN02 63  EFJ -- MORE OLYMPIC EXCLUSION                           *         
*                 -- RENUMBER AFFID & AFFINIT                         *         
* 01APR02 61  EFJ -- FUDGE LOOKUP CALL FOR PBDEM OVRDS W/NO BOOK      *         
* 16JAN02 60  EFJ -- PASS BANK CLEARED DATE                           *         
* 09OCT01 59  EFJ -- CHANGE *+4 TO *+8 FOR IDF                        *         
* 25JUL01 58  EFJ -- CHANGE SLNTAB                                    *         
* 31MAY01 57  EFJ -- SUPPORT FOR NTP=2 FACTORS                        *         
* 18MAY01 56  EFJ -- FIX TALENT FACTOR SAME AS GETRATE                *         
* 14MAR01 55  EFJ -- SUPPORT NTI CODES FOR CHILD SPOT                 *         
* 16JAN01 54  EFJ -- SPLIT BUYS ON 0C ELEMS IF NOT MINUS SPOTS        *         
* 02NOV00 53  EFJ -- FIX 4 BK AVG FOR AFFIDS                          *         
*                 -- PASS USERID SPLKUID FOR NON AFFIDS TOO!          *         
* 12SEP00 52  EFJ -- GET ADDRESSABILITY                               *         
*                 -- NEW OPTION TO IGNORE AFFIDS ON BRAND BUYS (FIS)  *         
* 11SEP00 51  EFJ -- FIX MAX SPOT ERROR CODE                          *         
* 29AUG00 50  EFJ -- PASS USERID TO SPGETDEM IN SPLKUID               *         
* 15AUG00 49  EFJ -- SUPPORT BOOK AVERAGING                           *         
* ??????? 48  ??? -- ???                                              *         
* 27JUL00 47  EFJ -- FIX LOOKUP CALLS FROM ORBLOOK                    *         
* 13MAY00 46  BOB -- FIX SQAD READING WRONG CPP                       *         
* 28FEB00 45  EFJ -- MISSED ONE !@#$%^& INSTRUCTION                   *         
*                 -- INCREASE IOAREA TO 2000 BYTES FOR DEMOS          *         
* 03JAN00 44  EFJ -- SUPPORT NOTAX OPTION                             *         
*                 -- GET SOME ADDRESSABILITY                          *         
* 21DEC99 43  BOB -- SQAD DATA                                        *         
* 08DEC99 42  EFJ -- CODE TO SPLIT BUYS                               *         
* 17NOV99 41  EFJ -- GET PAID REP IN CHUNK                            *         
* 24AUG99 29  M?H -- CHANGE WIM TRADE TEST FOR DIY TRADE CLIENT       *         
*                 -- FIX PRD FILTERS FOR TRADE                        *         
* 20AUG99 28  EFJ -- SKIP SPILL TEST IF AFFID HOOK FOR RADIO          *         
* 29JUL99 27  EFJ -- SET RADIO AFFID MKT FROM MKNUM IF NO ALPHA MKT   *         
* 29JUN99 26  EFJ -- SET OPT TO SUPPRESS CONDENSED MKT HOURLY DEMOS   *         
* 28JUN99 25  EFJ -- SUPPORT TRADE OPTION (WESTERN)                   *         
* 25JUN99 24  EFJ -- CLEAR STATUS BITS FROM SF AFFID TIME BEFORE DEMO *         
*                    LOOKUP                                           *         
* 29APR99 23  EFJ -- PASS A(SPOTTAB) FOR WTP FOR AFFIDS               *         
* 29MAR99 22  EFJ -- DON'T PASS SBMKNUM IN SPLKSPL FOR RADIO          *         
* 17FEB99 21  EFJ -- SET WTP FLAG FOR AFFIDS                          *         
* 27JAN99 19  EFJ -- GET RIGHT PRODUCT FOR WESTERN TRADE BUYS         *         
* 21DEC98 18  EFJ -- PASS WEEK DATES TO SPGETDEMF AND SET SPLKOWTP    *         
* 04DEC98 17  EFJ -- SUPPORT FOR BOOK COLUMN (NOT VALID WITH GST, PW, *         
*                    CHILD SPOT, SECOND COST)                         *         
* 22OCT98 16  EFJ -- TRY TO FIX RADIO AFFID REPORTING                 *         
* 22OCT98 15  EFJ -- GET MORE FUCKING ADDRESSABILITY                  *         
* 14OCT98 14  RPZ -- FIX OLYMPIC EXCLUSION                            *         
* 27AUG98 13  RPZ -- KILL BOOK TYPE "O' RERATE FROM BUY RECORD        *         
* 11AUG98 11  EFJ -- SKIP MISSED/MG IF SBE1SPT SET                    *         
* 11JUN98 10  MH/EFJ SUPPORT OLYMPIC EXCLUSION                        *         
* 08JUN98 09  MH/EFJ FIX PW TAX                                       *         
* 06APR98 08  EFJ -- SKIP SPILL TEST IN DEMO LOOKUP FOR RADIO         *         
* 02MAR98 07  EFJ -- SKIP CHUNK KEY TEST IF SBE1SPT SET               *         
* 09JAN98 06  EFJ -- JUST A COMMENT                                   *         
* 25NOV97 05  EFJ -- FIX GETTAL W/MULT. GROUP CODES                   *         
* 07OCT97 04  EFJ -- OPTIONAL END OF CHUNK AREA                       *         
* 24JUL97 03  EFJ -- DATE CHECK IN GETTAL IS WRONG                    *         
* 24JUL97 02  EFJ -- FIX ALLOCATED/BDMASPRD=0 IN GETPW                *         
* 24JUL97 01  EFJ -- LEVEL RESET                                      *         
*---------------------------------------------------------------------*         
* 11JUN97 42  EFJ -- NEW FLAG - SBENOMIN - IF 0B/0C ARE =, SKIP FROM  *         
*                    TABLE AND DON'T DIE IF MAXNSPT EXCEEDED (PW)     *         
* 26MAR97 41  EFJ -- FIX BUG IN RE-RATE FIX                           *         
* 11MAR97 40  EFJ -- SF NO-RERATE PROBLEM FOR NO BOOKS FOR CAN RADIO  *         
* 24JAN97 39  EFJ -- FIX RADIO/SPILL LOOKUP BUG                       *         
* 09JAN97 38  EFJ -- SUPPORT NORMALIZED HPT OPTION                    *         
* 11JUL96 34  MFH -- BDCANAD NOW X'80' NOT C'C'                       *         
* 09JUL96 33  EFJ -- USE DMA OVERRIDE FOR SPDEMUP                     *         
* 26APR96 32  MHER - USE CORE-RES GETRATE                             *         
* 24APR96 31  EFJ -- GREG HAD A BETTER IDEA FOR L30                   *         
* 12APR96 30  EFJ -- DONT CALC PW$ FOR OVERRIDE WEEKS                 *         
* 15MAR96 26-9MHER - IF YOU ONLY KNEW !                               *         
* 13MAR96 25  MHER - NEW TALENT FACTOR CODE FOR CHILD SPOT            *         
* 05MAR96 24  EFJ -- FIX SPECIAL PROBLEM                              *         
* 02OCT95 23  EFJ -- REMOVE PW EFFECTIVE COST CALCULATIONS            *         
* 13JUN95 22  EFJ -- NET DOWN TAX FOR PW BEFORE APPLYING BILL FORM    *         
* 07JUN95 21  MHER - NEVER ANY CABLE BOOKS                            *         
* 22MAY95 20  EFJ -- FIX BUG IN PW EFF TAX                            *         
* 08MAY95 19  EFJ -- REMOVE SCPWTAX CODE                              *         
* 02MAY95 17  EFJ -- COMPUTE PW EFF CST WITH TAX INCLUDED...          *         
* 27APR95 16  EFJ -- SUPPORT EFFECTIVE COST FOR PW                    *         
* 25APR95 15  EFJ -- FIX BUG WITH E ESTIMATES & PW                    *         
* 05APR95 14  GLEE - OPTION TO CALC GOAL W/ OR W/O TAX                *         
* 07MAR95 13  GL  -- FIX BUG EXTRACTING PW PCT                        *         
* 22FEB95 12  ??? -- BDCINDS STUFF                                    *         
* 01FEB95 11  EFJ -- SUPPORT FOR PST                                  *         
* 13DEC94 07  EFJ -- PW REC NOW PASSED IN FROM SPOTBUY                *         
* 07DEC94 06  EFJ -- DON'T DIE ON SPTTAB FULL IF P1 BYTE 0 = X'80'    *         
* 02NOV94 05  EFJ -- SET MEDIA ON GETDM CALLS FOR RADIO               *         
*                 -- REMOVE OFFENSIVE INSTRUCTIONS                    *         
* 01NOV94 04  EFJ -- YASF PW BUG                                      *         
* 20OCT94 03  EFJ -- Y.A. PW BUG                                      *         
* 08SEP94 02  EFJ -- HISTORY LOST, LEVEL RESET                        *         
*                 -- FIX BUG EXTRACTING PW PWCT                       *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
************************************************************                    
*                                                          *                    
* SPOTBUY - T00A48                                         *                    
*                                                          *                    
* BUILD A TABLE OF CHUNKS POINTED TO BY SBACHUNK AND       *                    
* COVERED BY SCHUNKD.  EXTRACT VALUES SECTION OF SPOTBLOCK *                    
* (SBEVALS) CONTROLS BUY EXTRACT.  CALLING PROGRAM CAN USE *                    
* CHUNKS TO BUILD SORT RECORDS FOR REPORTING.              *                    
*                                                          *                    
* ON ENTRY, P1 BYTE 0 = X'80' RETURN ERROR ON SPTTAB FULL  *                    
*                       X'20' NO AFFIDS IN SPTTAB          *                    
*              BYTE 1-3 = A(SPOT BLOCK)                    *                    
*                                                          *                    
* ON EXIT,  P1 BYTE 0 = 0 IF OK                            *                    
*                    NE 0 IF ERROR                         *                    
*                     = X'80' ON SPTTAB FULL               *                    
*                                                          *                    
************************************************************                    
         TITLE 'T00A48 - SPOTPAK BUY EXTRACT MODULE'                            
         EJECT                                                                  
SPOTBUY  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**SPBUY*,RA,RR=RE,CLEAR=YES                          
         LR    R9,RC                                                            
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPOTBUY+8192,RC                                                  
         ST    RE,RELO                                                          
         MVC   USERRD,4(RD)        SAVE LINK BACK TO USER                       
         ST    R1,APARM            SAVE A(PARM LIST)                            
*                                                                               
         TM    0(R1),X'80'                                                      
         BZ    *+8                                                              
         OI    FLAGS,DONTDIE                                                    
         TM    0(R1),X'20'                                                      
         BZ    *+8                                                              
         OI    FLAGS,SKPAFFDS                                                   
         MVI   0(R1),0             SET NORMAL COMPLETION                        
*                                                                               
         L     R8,0(R1)                                                         
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         L     R4,SBAIO1           R4 = A(BUY RECORD)                           
         USING BUYREC,R4                                                        
         L     R7,SBACHUNK         R7 = A(CHUNK)                                
         USING SCHUNKD,R7                                                       
*                                                                               
         CLI   FIRST,0             TEST FIRST TIME CALL                         
         BNE   INIT0030                                                         
*                                                                               
         L     R2,SBCOMFAC         SET COMFACS ADDRESSES                        
         USING COMFACSD,R2                                                      
         CLC   CMASTC,=F'0'        TEST MASTC RESOLVED                          
         BNE   INIT0010                                                         
         GOTOR CPROTOFF                                                         
*                                                                               
INIT0010 MVC   LCALLOV,CCALLOV                                                  
         MVC   LDATAMGR,CDATAMGR                                                
         MVC   LDATCON,CDATCON                                                  
         MVC   LADDAY,CADDAY                                                    
         MVC   LGETDAY,CGETDAY                                                  
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LARL  R1,EXTRA                                                         
INIT0020 ST    R1,LXTRA(RE)                                                     
         STC   RF,LXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,INIT0020                                                      
*                                                                               
         GOTO1 LCALLOV,DMCB,0,X'D9000A21'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LSPGETDM,0(R1)                                                   
*                                                                               
         MVI   DMCB+7,X'49'        NOW GET SPOTBOOK                             
         GOTO1 (RF),(R1),0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LSPBOOK,0(R1)                                                    
*                                                                               
         MVI   DMCB+7,X'22'        AND SPDEMUP                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LSPDEMUP,0(R1)                                                   
*                                                                               
         MVI   DMCB+7,X'79'        AND SPPWCALC                                 
         GOTO1 (RF),(R1),0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LPWCALC,0(R1)                                                    
*                                                                               
         MVI   DMCB+7,X'5F'        AND GETRATE                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LGETRATE,0(R1)                                                   
*                                                                               
         MVI   DMCB+7,X'7A'        STAPACK (T00A7A)                             
         GOTO1 (RF),(R1),0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LSTAPACK,0(R1)                                                   
*                                                                               
         MVI   FIRST,1             RESET FIRST TIME SWITCH                      
         CLC   CMASTC,=F'0'                                                     
         BNE   INIT0030                                                         
         GOTOR CPROTON                                                          
         DROP  R2                                                               
*                                                                               
INIT0030 MVC   ADDRS(ADDRSL),LADDRS                                             
*                                                                               
         TM    SBEFLAG9,SBE9GTUV   GET UNIVERSE?                                
         BZ    INIT0040            NO                                           
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   EXIT                YES - DON'T BOTHER                           
         BRAS  RE,GETUNIV          GET THE UNIVERSE                             
         B     EXIT                DONE                                         
*                                                                               
INIT0040 XC    SCNEXT,SCNEXT       CLEAR START OF CHUNK                         
         XC    SBBSPL,SBBSPL       CLEAR SPILL MKT RTG SRV NUMBER               
         MVC   BKTYPE,SBQBKTYP     SET SPECIAL BOOK TYPE                        
*                                                                               
BUY1     MVI   XFF,X'FF'                                                        
         MVC   XFF+1(L'XFF-1),XFF                                               
*                                                                               
BUY2     MVI   CANADA,C'N'                                                      
         MVI   GRIND,0             INITIALIZE GETRATE INDICATOR                 
         MVI   CURRENCY,0                                                       
         TM    SBINDS,SBAGYUS      SPECIAL US AGENCY IND (NO AGYHDR)            
         BO    BUY4                                                             
         TM    SBINDS,SBAGYCAN     SPECIAL CAN AGENCY IND (NO AGYHDR)           
         BO    BUY2A                                                            
* FOLLOWING IS NORMAL TEST FOR CANADIAN AGENCY !                                
         LA    RE,SBAGYREC                                                      
         USING AGYHDR,RE                                                        
         CLI   AGYPROF+7,C'C'      TEST CANADIAN AGENCY                         
         BNE   BUY4                                                             
BUY2A    MVI   CANADA,C'Y'         YES                                          
         DROP  RE                                                               
         CLI   SBQCURR,C'C'        TEST DOLLARS REQUESTED IN CANADIAN           
         BE    *+12                OR USA                                       
         CLI   SBQCURR,C'U'                                                     
         BNE   BUY3                                                             
         MVC   CURRENCY,SBQCURR    YES                                          
         MVI   GRIND,C'X'          SET GETRATE INDICATOR TO EXCHANGE            
***      TM    SBEFLAG,SBEPST      EXTRACT PST?                                 
***      BZ    *+8                                                              
***      MVI   GRIND,C'Z'          SET GETRATE INDICATOR TO EXCHANGE            
*                                                                               
BUY3     CLI   SBEGST,0            TEST INCLUDE GST/PST                         
         BE    BUY4                                                             
***      MVI   GRIND,C'X'          YES-SET GETRATE INDICATOR                    
***      TM    SBEFLAG,SBEPST      EXTRACT PST?                                 
***      BZ    *+8                                                              
         MVI   GRIND,C'Z'          YES-SET GETRATE INDICATOR                    
*                                                                               
         CLI   CURRENCY,0          AND SET DOLLARS TO CANADIAN IF               
         BNE   BUY4                NOT ALREADY SET                              
         MVI   CURRENCY,C'C'                                                    
*                                                                               
BUY4     LM    R1,R2,SBADATE       GET A(DATE TABLE),N'ENTRIES                  
         MVC   FSTDATE,0(R1)                                                    
         BCTR  R2,0                                                             
         SLL   R2,2                                                             
         LA    R1,0(R2,R1)         FIND LAST ENTRY                              
         MVC   LSTDATE,2(R1)                                                    
         MVC   ASPTTAB,SBASPTTB    A(SPOT TABLE)                                
*                                                                               
         CLI   SBEDEMTY,0          TEST FOR ACCOUNTING ONLY  MH                 
         BE    *+8                 YES                       MH                 
         CLI   SBEDEMTY,C'0'       TEST FOR SQAD                                
         BNL   *+8                 YES                                          
         BAS   RE,EQTAB            FIND EQUIVALENCE TABLE                       
*                                                                               
         CLI   SBECS,YES           TEST EXTRACTING CHILD SPOT                   
         BNE   BUY4A                                                            
         GOTO1 AGETTAL             YES-GET THE TALENT FACTORS                   
*                                                                               
BUY4A    MVC   BUYPRD,BUYKPRD      SAVE PRODUCT IN KEY                          
         CLI   SBELKUPA,C'Y'       LKUP=ALL?                                    
         BE    BUY4AA              YES - DON'T HONOR SPECIAL                    
         CLI   BDPROGT-1,0         TEST FOR SPECIAL                             
         BNE   *+8                                                              
         MVI   SPECIAL,YES                                                      
*                                                                               
BUY4AA   MVC   SVEDEMTY,SBEDEMTY   SAVE THE DEMO TYPE                           
         CLI   SBEDEMTY,0          TEST FOR ACCOUNTING ONLY                     
         BE    BUY6                YES                                          
*                                                                               
         BAS   RE,DEML             SET DEMO LIST                                
*                                                                               
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BZ    BUY4C               NO                                           
*                                                                               
         LA    RE,DEMOS            A(DEMO LIST)                                 
*                                                                               
BUY4B    CLI   0(RE),X'FF'         END OF DEMO LIST?                            
         BE    BUY22               YES - NO COMSCORE DEMOS FOUND - EXIT         
         CLI   2(RE),0             COMSCORE DEMO?                               
         BE    BUY4C               YES                                          
         AHI   RE,3                BUMP TO NEXT DEMO                            
         B     BUY4B               TEST NEXT DEMO                               
*                                                                               
BUY4C    MVC   NDEMOS,SBENDEM                                                   
         CLI   NDEMOS,0                                                         
         BNE   *+8                                                              
         MVI   NDEMOS,4            SET DEFAULT DEMO COUNT                       
         CLI   BUYKPRD,X'FF'       TEST POL BUY,                                
         BNE   BUY6                                                             
         CLC   SBQPRD,=C'ALL'      AND PRODUCT ALL REQUEST                      
         BNE   BUY6                                                             
         MVC   SVNDEMOS,NDEMOS     YES-SAVE N'DEMOS                             
         SR    R1,R1               GET N'DEMOS IN POL ESTIMATE HDR              
         LA    RE,DEMOS                                                         
         CLI   0(RE),X'FF'                                                      
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    RE,3(RE)                                                         
         B     *-16                                                             
         STC   R1,NDEMOS           OVERRIDE N'DEMOS TO N'POL DEMOS              
         CLC   NDEMOS,SVNDEMOS     CHECK NOT DECREASING N'DEMOS                 
         BNL   BUY6                                                             
         MVC   NDEMOS,SVNDEMOS                                                  
*                                                                               
BUY6     LLC   R1,NDEMOS                                                        
         LR    RE,R1               SAVE IT                                      
         SLL   R1,3                N'DEMOS*8                                    
         LA    R1,SCDEMOS-SCHUNKD(R1) COMPUTE L'CHUNK                           
         ST    R1,LCHUNK                                                        
*                                                                               
         SLL   RE,2                N'DEMOS*4                                    
         LA    RE,SPTTABL(RE)      COMPUTE L'SPTTAB ENTRY                       
         ST    RE,LSPTTAB                                                       
         ST    RE,SBLSPTEN                                                      
         L     R1,SBLSPTTB         R1=L'TABLE                                   
         SR    R0,R0                                                            
         DR    R0,RE               MAX ENTRIES=L'TABLE/L'ENTRY                  
         ST    R1,MAXNSPT                                                       
*                                                                               
         ICM   RF,15,SBCOMDEM      A(COMSCORE DEMO OVERRIDE LIST)               
         BZ    BUY6AA3             NO - CHECK X'50' ELEM ON BUY                 
         OC    0(160,RF),0(RF)     HAVE ANY COMSCORE DEMO OVERRIDES?            
         BZ    BUY6AA3             NO - CHECK X'50' ELEM ON BUY                 
*                                                                               
         LA    R2,NTDMELEM+2       BUILD COMSCORE DEMO LIST HERE                
         LA    R0,20               UP TO 20 DEMOS                               
         LA    R1,2                ELEMENT LENGTH COUNTER                       
*                                                                               
BUY6AA1  MVC   0(8,R2),0(RF)       MOVE COMSCORE DEMO IN                        
         LA    R2,9(R2)            BUMP BY 9 (8 + FLAG BYTE)                    
         LA    RF,8(RF)            NEXT COMSCORE DEMO                           
         LA    R1,9(R1)            NEW ELEMENT LENGTH                           
         BCT   R0,BUY6AA1          CHECK FOR ANOTHER OVERRIDE                   
*                                                                               
         SHI   R1,9                ELEMENT LENGTH                               
*                                                                               
BUY6AA2  MVI   NTDMELEM,X'50'      X'50' ELEMENT                                
         STC   R1,NTDMELEM+1       ELEMENT LENGTH                               
         B     BUY6AA4             USE COMSCORE DEMO OVERRIDES                  
*                                                                               
BUY6AA3  LA    R6,BDELEM           A(FIRST BUY ELEMENT)                         
         MVI   ELCDLO,NTDELCDQ     X'50' NON-TRADITIONAL DEMO NAME ELEM         
         MVI   ELCDHI,NTDELCDQ     X'50' NON-TRADITIONAL DEMO NAME ELEM         
         BRAS  RE,NEXTEL           HAVE A X'50' ELEMENT?                        
         BNE   BUY6AA4             NO                                           
         LLC   RF,1(R6)            ELEMENT LENGTH                               
         BCTR  RF,0                -1 FOR EX                                    
         EX    RF,*+8              EXECUTE THE MVC                              
         B     *+10                FOR IDF                                      
         MVC   NTDMELEM(0),0(R6)   COPY X'50 ELEMENT TO NTDMELEM                
*                                                                               
BUY6AA4  BRAS  RE,GETAMKT          GO GET THE ALPHAMKT                          
         XC    SBCANALF,SBCANALF                                                
         XC    CDEMSTA,CDEMSTA     CLEAR OVERRIDE STA FOR CAN SOFT DEM          
         MVI   CDEMFLG,0           CLEAR FLAGS FOR CAN SOFT DEM                 
         XC    SBBYOVRD,SBBYOVRD   CLEAR DEMO OVERRIDES                         
*                                                                               
         CLI   SBEDEMTY,0          TEST FOR ACCOUNTING ONLY                     
         BE    BUY10               YES                                          
         BRAS  RE,GETPUR           EXTRACT PURCHASED DEMOS                      
*                                                                               
         OC    SBEMKT,SBEMKT       TEST FOR REQUESTED MARKET                    
         BZ    BUY8                                                             
         CLC   SBEMKT,SBBMKT       YES-TEST SAME MARKET AS KEY                  
         BE    BUY8                                                             
         LA    R6,BDELEM           NO-SPILL                                     
         MVI   ELCDLO,3            GET SPILL MARKET DEMO ELEMENT                
         MVI   ELCDHI,3                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   BUY7                                                             
         CLC   SBEMKT,NDPROG-NDELEM(R6)                                         
         BNE   *-14                                                             
         CLI   CANADA,C'Y'         CANADIAN AGENCY?                             
         BNE   BUY6A                                                            
         CLI   NDRTGSVC-NDELEM(R6),C'0'  RATING SERVICE PRESENT?                
         BL    BUY6A               NO - THEN NO OTHER OVERRIDES                 
         XC    SBBSPL,SBBSPL       CLEAR SPILL MKT RTG SRV NUMBER               
         MVI   CDEMFLG,X'02'       SET TO NSI                                   
         CLI   NDRTGSVC-NDELEM(R6),C'0'  NSI?                                   
         BE    *+8                 YES                                          
         MVI   CDEMFLG,X'01'       NO - SET FLAG TO BBM                         
         MVC   SBBYORTG,NDRTGSVC-NDELEM(R6)                                     
         MVC   SBBYOALF,NDMKTALF-NDELEM(R6)                                     
*                                                                               
         MVC   ALPHAMKT,NDMKTALF-NDELEM(R6)                                     
         MVC   SBCANALF,NDMKTALF-NDELEM(R6)                                     
         CLI   NDSTA-NDELEM(R6),C'A'                                            
         BL    BUY6B                                                            
         MVC   SBBYOSTA,NDSTA-NDELEM(R6)                                        
         MVC   CDEMSTA,NDSTA-NDELEM(R6)  USE OVERRIDE STATION                   
         MVI   CDEMSTA+4,C'T'                                                   
         B     BUY6B                                                            
*                                                                               
BUY6A    MVC   SBBSPL,NDPROG+2-NDELEM(R6)  EXTRACT SPILL RTG SVC MKT NO         
*                                                                               
BUY6B    CLI   SBQBKTYP,0          IF NO BOOK TYPE OVERRIDE,                    
         BNE   BUY7                                                             
         MVC   BKTYPE,NDPROG+4-NDELEM(R6)  SET SPECIAL BOOK TYPE                
         CLI   BKTYPE,C'O'         OLYMPIC IN BUY                               
         BNE   BUY7                 NO - ITS OK                                 
         MVI   BKTYPE,0             YES - DISABLE FOR RERATES                   
*                                                                               
BUY7     CLI   SBD0PROF,C'Y'       TEST SPILL REQUIRES ESTIMATED DEMOS          
         BNE   BUY8                                                             
         LLC   R0,NDEMOS           YES - CHECK FOR ANY ESTIMATED DEMOS          
         LA    R1,PURVALSA                                                      
         OC    1(3,R1),1(R1)                                                    
         BNZ   BUY8                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,*-14                                                          
         B     BUY22               NO ESTIMATED DEMOS, SO EXIT NOW              
*                                                                               
BUY8     CLI   SBEDEMTY,C'P'       TEST FOR PURCHASED LOOKUP                    
***      BE    *+12                YES                                          
***      CLI   SBEDEMTY,C'U'       TEST FOR UPGRADE LOOKUP                      
         BNE   BUY8A               YES                                          
*                                                                               
         CLI   CANADA,C'Y'         CANADIAN AGENCY?                             
         BNE   BUY10               NO, NO OVERRIDES                             
         B     BUY8B               YES - GET OVERRIDES IF NOT SPILL             
*                                                                               
BUY8A    CLI   SBEDEMTY,C'0'       TEST FOR SQAD                                
         BNL   BUY10               YES                                          
         CLI   SBQBKTYP,0          UNLESS BOOK TYPE OVERRIDE,                   
         BNE   BUY9                                                             
*                                                                               
BUY8B    OC    SBEMKT,SBEMKT       TEST SPILL                                   
         BZ    *+14                                                             
         CLC   SBEMKT,SBBMKT                                                    
         BNE   BUY9                                                             
*                                                                               
         LA    R6,BDELEM           NO-LOOK FOR DEMO LOOKUP OVERRIDE             
         MVI   ELCDLO,X'24'           ELEMENT                                   
         MVI   ELCDHI,X'24'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BUY9                                                             
         USING DLUELEM,R6                                                       
         CLI   CANADA,C'Y'         CANADIAN AGENCY?                             
         BNE   BUY8C               NO                                           
         CLI   DLUBAMKT,C'A'       HAVE SOFT DEMO FIELDS?                       
         BL    BUY8C               NO                                           
         MVC   ALPHAMKT,DLUBAMKT   ALPHA MARKET                                 
         MVC   SBCANALF,DLUBAMKT   ALPHA MARKET                                 
         MVC   CDEMFLG,DLUBFLGS    BBM/NSI FLAG                                 
         MVI   SBBYORTG,C'0'       INIT TO NSI                                  
         TM    DLUBFLGS,X'02'      DOING NSI?                                   
         BNZ   *+8                 YES                                          
         MVI   SBBYORTG,C'1'       NO - INDICATE BBM                            
         MVC   SBBYOALF,DLUBAMKT                                                
         CLI   DLUBSTOV,C'A'       TEST STATION OVERRIDE                        
         BL    BUY8C                                                            
         MVC   SBBYOSTA,DLUBSTOV                                                
         MVC   CDEMSTA,DLUBSTOV    STA OVERRIDE                                 
         MVI   CDEMSTA+4,C'T'                                                   
*                                                                               
BUY8C    CLI   SBEDEMTY,C'P'       PURCHASED LOOKUP?                            
         BE    BUY10               YES                                          
***      CLI   SBEDEMTY,C'U'       UPGRADE LOOKUP?                              
***      BE    BUY10               YES                                          
         MVC   BKTYPE,DLUBKTYP     EXTRACT BOOK TYPE CODE                       
         OC    DLUBAMKT,DLUBAMKT                                                
         BZ    *+10                                                             
         MVC   ALPHAMKT,DLUBAMKT   AND ALPHA MARKET                             
         DROP  R6                                                               
*                                                                               
         CLI   BKTYPE,C'O'         OLYMPIC IN BUY                               
         BNE   *+12                 NO - ITS OK                                 
         MVI   BKTYPE,0             YES - DISABLE FOR RERATES                   
         B     BUY9                                                             
*                                                                               
         CLI   BKTYPE,C'P'         PEOPLEMETER IN BUY?                          
         BNE   BUY9                 NO - ITS OK                                 
         CLC   SBAGY,=C'YN'        KILL IT IF IT'S YN                           
         BE    *+14                                                             
         CLC   SBAGY,=C'SC'        OR SC                                        
         BNE   BUY9                                                             
         MVI   BKTYPE,0             YES - DISABLE FOR RERATES                   
*                                                                               
BUY9     CLI   SBEDEMTY,C'P'       PURCHASED LOOKUP?                            
         BE    BUY10               YES                                          
         CLI   SBEDEMTY,C'U'       UPGRADE LOOKUP?                              
         BE    BUY10               YES                                          
*&&DO                                                                           
         CLI   BKTYPE,C'0'         BOOK FOR ZERO CELL DATA ?                    
         BL    *+12                NO                                           
         CLI   BKTYPE,C'9'         BOOK FOR ZERO CELL DATA ?                    
         BNH   BUY9A               YES                                          
         CLI   BKTYPE,C'A'         BOOK TYPE MUST BE A-Z                        
         BL    *+12                                                             
         CLI   BKTYPE,C'Z'                                                      
         BNH   *+8                                                              
         MVI   BKTYPE,0                                                         
*&&                                                                             
BUY9A    MVI   SBDAYS,0                                                         
         TM    SBABKLST,X'80'      TEST POST OPTION SET                         
         BZ    *+10                                                             
         MVC   SBDAYS,BDDAY        YES-SET DAYS ROTATION                        
         MVC   SBBKTYPE,BKTYPE     SET BOOK TYPE                                
         MVC   SVSBSTA,SBSTA       SAVE THE STATION                             
         XC    SBSYSCD,SBSYSCD                                                  
*                                                                               
         GOTO1 AFUSION,DMCB,SBSTA,SBSYSCD                                       
*                                                                               
         CLI   LPMWEEK,0                                                        
         BE    *+10                                                             
         MVC   SBD0PROF+10(1),LPMWEEK                                           
         CLI   LPMOV,0                                                          
         BE    *+10                                                             
         MVC   SBD0PROF+11(1),LPMOV                                             
*                                                                               
         CLI   CDEMSTA,C'A'        HAVE CANADIAN STA OVERRIDE?                  
         BL    *+10                NO                                           
         MVC   SBSTA,CDEMSTA       YES - USE OVERRIDE STATION                   
*                                                                               
         MVC   SVRTGSVC,SBCPROF+3  SAVE THE RATING SERVICE                      
         TM    CDEMFLG,X'01'       OVERRIDE TO BBM?                             
         BZ    *+8                 NO                                           
         MVI   SBCPROF+3,C'1'      YES - OVERRIDE TO BBM                        
         TM    CDEMFLG,X'02'       OVERRIDE TO NSI?                             
         BZ    *+8                 NO                                           
         MVI   SBCPROF+3,C'0'      YES - OVERRIDE TO NSI                        
*                                                                               
         TM    SBEFLAG8,SBE8CP1+SBE8CP2 COMSCORE PASS 1/2?                      
         BZ    BUY9D               NO                                           
*                                                                               
         NI    SBEFLAG9,X'FF'-SBE9CMBK  TURN OFF FLAG                           
         NI    SBEFLAG9,X'FF'-SBE9CERR  TURN OFF FLAG                           
*                                                                               
         LA    RE,DEMOS            A(DEMO LIST)                                 
*                                                                               
BUY9B    CLI   0(RE),X'FF'         END OF DEMO LIST?                            
         BE    BUY9D               YES - NO COMSCORE DEMOS FOUND                
         CLI   2(RE),0             COMSCORE DEMO?                               
         BE    BUY9C               YES - SET FLAG TO ALWAYS GET BOOK            
         AHI   RE,3                BUMP TO NEXT DEMO                            
         B     BUY9B               TEST NEXT DEMO                               
*                                                                               
BUY9C    OI    SBEFLAG9,SBE9CMBK   SPOTBOOK TO SET BOOK FOR COMSCORE            
*                                                                               
BUY9D    GOTO1 VSPBOOK,DMCB,SBLOCK                                              
         MVC   SBSTA,SVSBSTA                                                    
         MVC   SBCPROF+3(1),SVRTGSVC  SAVE THE RATING SERVICE                   
         OC    SBNBKS,SBNBKS                                                    
         BZ    BUY22               ERROR HAS OCCURRED IN SPOTBOOK               
*                                                                               
BUY10    TM    BDCANAD,X'80'       TEST CANADIAN BUY                            
         BZ    BUY12                                                            
         TM    BUYKAM,X'03'        TEST NETWORK                                 
         BNO   BUY12                                                            
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'69'                                                     
         MVI   ELCDHI,X'69'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BUY12                                                            
         ST    R6,ATAXEL                                                        
*                                                                               
* INITIALIZE FOR BUY AND BUILD THE SPOT TABLE FOR THE BUY LINE                  
*                                                                               
BUY12    CLI   BUYKPRD,X'FF'       TEST FOR POL BUY                             
         BE    BUY14               YES                                          
         BAS   RE,INBR             INITIALIZE FOR BRAND BUY                     
         BNE   BUY20               BUY REJECTED                                 
         BAS   RE,BRAND                                                         
         BZ    BUY20               NO ENTRIES IN TABLE                          
         BM    EXIT                SPTTAB FULL ERROR                            
         B     BUY15                                                            
*                                                                               
BUY14    BAS   RE,POL                                                           
         BE    BUY20               NO ENTRIES IN TABLE                          
*                                                                               
* LOOK-UP DEMOS IF REQUESTED                                                    
*                                                                               
BUY15    XC    AORBEL,AORBEL       FIND A(ORBIT ELEMENT) IF ANY                 
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'67'                                                     
         MVI   ELCDHI,X'67'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   *+8                                                              
         ST    R6,AORBEL                                                        
         CLI   SBEDEMTY,0          TEST ACCOUNTING ONLY                         
         BE    BUY19               YES                                          
*                                                                               
         CLI   SBEDEMTY,C'0'       TEST FOR SQAD LOOKUP                         
         BL    BUYSQDN                                                          
*                                                                               
         BRAS  RE,SQAD             LOOK UP SQAD CPPS                            
*                                                                               
         B     BUY18                                                            
*                                                                               
BUYSQDN  DS    0H                                                               
*                                                                               
         BAS   RE,PUR              PURCHASED DEMOS WHERE NEEDED                 
         CLI   SBEDEMTY,C'P'       TEST PURCHASED ONLY LOOKUP                   
         BE    BUY18                                                            
***                                                                             
* UPGRADE LOOKUP CODE MOVED TO AFTER WE LOOKED UP THE PURCHASED DEMOS           
* SO WE REPORT PURCHASED IF UPGRADES RETURN NOTHING!                            
***                                                                             
         CLI   SBEDEMTY,C'U'       TEST UPGRADE LOOKUP                          
         BNE   BUY17                                                            
         CLI   SPECIAL,YES         TEST FOR -S BUYLINE                          
         BNE   *+12                NO                                           
         CLI   SB1WPROF+1,C'Y'     TEST WEEKLY OPTION ACTIVE                    
         BNE   BUY18               NO-SKIP UPGRADES                             
         GOTO1 AUPG                YES-DO UPGRADES                              
         B     BUY18                                                            
*                                                                               
BUY17    BAS   RE,PBDEMGET         GET POST BUY DEMO OVERRIDES                  
         BRAS  RE,BLDBK            BUILD LIST OF RE-RATE BOOKS                  
         BAS   RE,RERATE           PERFORM RE-RATE LOOKUP                       
         BRAS  RE,UNRPSPTS         DEAL WITH UNREPORTED SPOTS                   
*                                                                               
         CLI   SBEDEMTY,C'R'       TEST FOR RERATE LOOKUP                       
         BE    BUY18                                                            
*                                                                               
         CLI   SPECIAL,YES         TEST FOR -S BUYLINE                          
         BNE   *+12                NO                                           
         CLI   SB1WPROF+1,C'Y'     TEST WEEKLY OPTION ACTIVE                    
         BNE   *+8                 NO-SKIP AFFID LOOKUP                         
         BAS   RE,AFFID            LOOKUP AFFIDAVITS                            
*                                                                               
BUY18    BAS   RE,SPLNODEM         EXCLUDE SPILL SPOTS WITHOUT DEMOS            
*                                                                               
* BUILD CHUNK ENTRIES FROM THE SPOT TABLE                                       
*                                                                               
BUY19    CLI   BUYKPRD,X'FF'       TEST FOR POL BUY                             
         BE    *+12                YES                                          
         BAS   RE,UPBR             BUILD CHUNKS FOR BRAND BUY                   
         B     *+8                                                              
         BAS   RE,UPPOL            BUILD CHUNKS FOR POL BUY                     
*                                                                               
BUY20    ICM   RE,15,ATAXEL                                                     
         BZ    *+8                                                              
         NI    0(RE),X'FF'-X'80'   FIX TAX ELEMENT                              
*                                                                               
         CLI   SBEDEMTY,0          TEST DEMO LOOKUP                             
         BE    BUY22                                                            
         CLI   BUYKPRD,X'FF'       AND POL BUY                                  
         BNE   BUY22                                                            
         CLC   SBQPRD,=C'ALL'      AND PRODUCT ALL REQUEST                      
         BNE   BUY22                                                            
         GOTO1 APRDDEMS            YES-REARRANGE DEMOS FOR EACH PRD             
*                                                                               
BUY22    MVC   SBEDEMTY,SVEDEMTY   RESTORE ORIGINAL DEMO TYPE                   
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* ROUTINE TO EXCLUDE ALL SPILL SPOTS WITH NO DEMO VALUES                        
*                                                                               
SPLNODEM NTR1  ,                                                                
         OC    SBEMKT,SBEMKT       TEST SPILL MARKET                            
         BZ    SPLNDX              NO                                           
         CLC   SBEMKT,SBBMKT                                                    
         BE    SPLNDX              NO                                           
         LM    R2,R4,ASPTTAB       YES                                          
         USING SPTTABD,R2                                                       
         LLC   R7,NDEMOS                                                        
         SLL   R7,2                                                             
         BCTR  R7,0                                                             
*                                                                               
SPLND2   TM    SPTIND,SPTDUMMY     TEST SPOT ALREADY EXCLUDED                   
         BO    SPLND4              YES                                          
         EX    R7,*+8              NO-TEST ANY DEMO VALUES                      
         B     *+10                                                             
         OC    SPTDEMOS(0),SPTDEMOS                                             
         BNZ   SPLND4              YES                                          
         OI    SPTIND,SPTDUMMY     NO-EXCLUDE THIS SPOT                         
*                                                                               
SPLND4   LA    R2,0(R4,R2)                                                      
         BCT   R3,SPLND2                                                        
*                                                                               
SPLNDX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE FOR BRAND BUY                                       
* ON EXIT, CC=EQ IF BUY LINE OK, CC=NEQ TO REJECT LINE                          
*                                                                               
INBR     NTR1                                                                   
         MVC   PRD1,BUYKPRD        SET FIRST PRODUCT                            
         MVC   SLN1,BDSEC          AND SECONDS LENGTH                           
         CLI   BDTIME,0            TEST FOR NON-POL PIGGYBACK                   
         BE    INBR2               NO                                           
*                                                                               
         MVC   SLN1,BDTIME         SET FIRST PRODUCT SHARE                      
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'04'        LOOK FOR PIGGYBACK ELEMENT                   
         MVI   ELCDHI,X'04'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PBELEM,R6                                                        
         MVC   PRD2,PBPROD         EXTRACT PASSIVE PARTNER                      
         MVC   SLN2,PBTIME         AND ITS SHARE OF TIME                        
*                                                                               
INBR2    CLI   SBEPRD,0            TEST FOR PRODUCT FILTER                      
         BE    INBR4                                                            
         CLI   SBEPRD,X'FF'                                                     
         BE    INBR4                                                            
         CLC   SBEPRD,PRD1         TEST FOR MATCH ON 1ST PROD                   
         BE    INBR4                                                            
         CLC   SBEPRD,PRD2                                                      
         BNE   INBRN                                                            
*                                                                               
INBR4    MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'08'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL           FIND A SPOT ELEMENT                          
         BNE   INBRN                                                            
         USING REGELEM,R6                                                       
*                                                                               
         XC    DUB(2),DUB                                                       
         CLI   PRD2,0              TEST FOR NON-POL PIGGYBACK                   
         BE    *+10                                                             
         MVC   DUB(2),PRD1         FIRST PRODUCT                                
*                                                                               
         GOTO1 GOGETRAT,DMCB,(DUB,SPOTS),(DUB+1,(R4)),(GRIND,(R6)),    C        
               (CURRENCY,XGROSS)                                                
         XC    TAX,TAX             ZERO TAX FIELD                               
         OC    BDNTAX,BDNTAX       TEST FOR TAX                                 
         BNZ   INBR6               YES                                          
         OC    ATAXEL,ATAXEL       TEST FOR CANADIAN NETWORK TAX                
         BZ    INBR8               NO                                           
*                                                                               
INBR6    MVC   HALF,BDNTAX                                                      
         XC    BDNTAX,BDNTAX                                                    
         ICM   RE,15,ATAXEL                                                     
         BZ    *+8                                                              
         OI    0(RE),X'80'                                                      
*                                                                               
         GOTO1 GOGETRAT,DMCB,(DUB,WORK),(DUB+1,(R4)),(GRIND,(R6)),     C        
               (CURRENCY,WORK+16)                                               
         ICM   RE,15,ATAXEL                                                     
         BZ    *+8                                                              
         NI    0(RE),X'7F'                                                      
         L     RE,GROSS                                                         
         L     RF,WORK+4                                                        
         SR    RE,RF               COMPUTE TAX                                  
         ST    RE,TAX                                                           
         MVC   BDNTAX,HALF                                                      
*                                                                               
INBR8    CLI   PRD2,0              TEST FOR PIGGYBACK                           
         BE    INBR12              NO                                           
         MVC   DUB(2),PRD2         NOW DO SECOND PRODUCT                        
*                                                                               
         GOTO1 GOGETRAT,DMCB,(DUB,SPOTS2),(DUB+1,(R4)),(GRIND,(R6)),   C        
               (CURRENCY,XGROSS2)                                               
         XC    TAX2,TAX2                                                        
         OC    BDNTAX,BDNTAX       TEST FOR TAX                                 
         BNZ   INBR10              YES                                          
         OC    ATAXEL,ATAXEL       TEST FOR CANADIAN NETWORK TAX                
         BZ    INBR12              NO                                           
*                                                                               
INBR10   MVC   HALF,BDNTAX                                                      
         XC    BDNTAX,BDNTAX                                                    
         ICM   RE,15,ATAXEL                                                     
         BZ    *+8                                                              
         OI    0(RE),X'80'                                                      
*                                                                               
         GOTO1 GOGETRAT,DMCB,(DUB,WORK),(DUB+1,(R4)),(GRIND,(R6)),     C        
               (CURRENCY,WORK+16)                                               
         ICM   RE,15,ATAXEL                                                     
         BZ    *+8                                                              
         NI    0(RE),X'7F'                                                      
         L     RE,GROSS2                                                        
         L     RF,WORK+4                                                        
         SR    RE,RF               COMPUTE TAX                                  
         ST    RE,TAX2                                                          
*                                                                               
INBR12   B     INBRY                                                            
*                                                                               
INBRN    LTR   RB,RB               SET CC=NEQ                                   
         B     INBRX                                                            
*                                                                               
INBRY    CR    RB,RB               SET CC=EQ                                    
*                                                                               
INBRX    B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO BUILD THE SPOT TABLE FOR A BRAND BUY RECORD                    
* ON EXIT, CC=EQ FOR EMPTY TABLE                                                
*                                                                               
BRAND    NTR1                                                                   
         L     R2,ASPTTAB          R2=A(SPOT TABLE)                             
         SR    R3,R3               R3=N'ENTRIES                                 
         LA    R5,SPOT                                                          
         USING SPTTABD,R5                                                       
         LA    R6,BDELEM                                                        
         XC    HALF,HALF           HALF=PREVIOUS SPOT DATE                      
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'08'                                                     
*                                                                               
         USING REGELEM,R6                                                       
BRAND2   BRAS  RE,NEXTEL                                                        
         BNE   BRAND20             EOR                                          
*                                                                               
BRAND4   XC    SPOT,SPOT                                                        
         TM    SBEFLAG2,SBE1SPT    SINGLE SPOT CHUNKS?                          
         BZ    *+12                 NO                                          
         TM    RSTATUS,X'C0'       TEST MISSED/MG                               
         BNZ   BRAND2               YES - SKIP IT                               
*                                                                               
         TM    RSTATUS,X'04'       TEST FOR HIATUS                              
         BO    BRAND2                                                           
*                                                                               
         CLC   RPAY,SBQPAYST       TEST PAID DATE FILTERS                       
         BL    BRAND2                                                           
         OC    SBQPAYEN,SBQPAYEN                                                
         BZ    *+14                                                             
         CLC   RPAY,SBQPAYEN                                                    
         BH    BRAND2                                                           
*                                                                               
         CLI   SBESPOTS,SBESPAID   TEST PAID SPOTS ONLY                         
         BNE   *+14                                                             
         OC    RPAY,RPAY                                                        
         BZ    BRAND2                                                           
         CLI   SBESPOTS,SBESUNPD   TEST UNPAID SPOTS ONLY                       
         BNE   *+14                                                             
         OC    RPAY,RPAY                                                        
         BNZ   BRAND2                                                           
*                                                                               
         BAS   RE,SETDAT                                                        
         BNE   BRAND2              DATE OUTSIDE OUT REPORT PERIOD               
         ST    R6,SBASPOT          PASS SPOT TO USER                            
         MVI   SBYORN,YES                                                       
         BAS   RE,GO                                                            
         CLI   SBYORN,NO           TEST IF SPOT REJECTED                        
         BE    BRAND2              YES                                          
*                                                                               
         MVC   HALF,RDATE          UPDATE PREVIOUS DATE                         
         CLI   SBEPAYDT,C'Y'       TEST TO USE PAY DATE                         
         BNE   *+10                                                             
         MVC   HALF,RPAY                                                        
         LLC   R1,RNUM             GET NUMBER OF SPOTS                          
         TM    RSTATUS,X'80'       TEST FOR MINUS SPOT                          
         BZ    *+6                 NO                                           
         LNR   R1,R1                                                            
         STH   R1,SPTSPOTS                                                      
         OC    RPAY,RPAY           TEST FOR PAID SPOT                           
         BZ    BRAND5                                                           
         OI    SPTIND,SPTSPAID     YES                                          
         STH   R1,SPTPAYS                                                       
         CLC   RPAY,SBBTODAY       TEST IF PAID TODAY                           
         BNE   *+8                                                              
         STH   R1,SPTPAYTS                                                      
*                                                                               
BRAND5   MVC   SPTPRD1(2),PRD1                                                  
         MVC   SPTPRD2(2),PRD2                                                  
         MVC   SPTGRS1(12),GROSS   GROSS/NET/TAX                                
         MVC   SPTGRS2(12),GROSS2                                               
         CLI   SBEGST,0            TEST GST/PST REQUIRED                        
         BE    BRAND7                                                           
*                                                                               
         GOTO1 GOGETRAT,DMCB,(PRD1,WORK),(SLN1,(R4)),(GRIND,(R6)),     C        
               (CURRENCY,XGROSS)                                                
         MVC   SPTGST1,XGSTAMT     GST                                          
* NEED PST INFO EVEN IF ONLY WANT GST INCASE HST APPLIES (NO GST!)              
*        TM    SBEGST,SBEPSTI+SBEPSTO+SBEPSTB WANT PST                          
*        BZ    BRAND6                                                           
         LA    RF,XPSTTAB                                                       
         BRAS  RE,SHIFTPST                                                      
         ICM   RE,15,XPSTAMT                                                    
         ST    RE,SPTTAX1          SAVE PST                                     
         MVC   SPTPSTCD,XPSTCODE                                                
         MVC   SPTPSTRT,XPSTRATE                                                
         MVC   SPTPSTPV,XPSTPROV+1                                              
*                                                                               
BRAND6   CLI   PRD2,0                                                           
         BE    BRAND7                                                           
*                                                                               
         GOTO1 GOGETRAT,DMCB,(PRD2,WORK),(SLN2,(R4)),(GRIND,(R6)),     C        
               (CURRENCY,XGROSS)                                                
         TM    SBEFLAG2,SBEBOOK    EXTRACTING BOOK?                             
         BNZ   BRAND7              YES - NOT COMPAT WITH GST                    
         MVC   SPTGST2,XGSTAMT2                                                 
* NEED PST INFO EVEN IF ONLY WANT GST INCASE HST APPLIES (NO GST!)              
*        TM    SBEGST,SBEPSTI+SBEPSTO+SBEPSTB WANT PST                          
*        BZ    BRAND7                                                           
         LA    RF,XPSTTAB2                                                      
         BRAS  RE,SHIFTPST                                                      
         ICM   RE,15,XPSTAMT2                                                   
         ST    RE,SPTTAX2                                                       
         MVC   SPTPSTCD,XPSTCDE2   SHOULD NOT DIFFER TO PST1 VALS...            
         MVC   SPTPSTRT,XPSTRAT2   ...BUT SET AGAIN TO ENSURE SET!              
         MVC   SPTPSTPV,XPSTPRV2+1                                              
*                                                                               
BRAND7   CLI   SBEDEMTY,0          TEST ACCOUNTING ONLY                         
         BE    BRAND7A             YES-DO NOT NEED A BOOK                       
         CLI   SBEDEMTY,C'0'                                                    
         BNL   BRAND7A             SQAD DOESN'T NEED BOOK                       
         BRAS  RE,GETBK            GET THE ACTUAL BOOK FOR SPOT DATE            
*                                                                               
BRAND7A  MVC   SPTPAYDT,RPAY                                                    
         MVC   SPTCLRSQ,RPAYSEQ                                                 
*                                                                               
         LA    R3,1(R3)            INCREMENT ENTRY COUNT                        
         C     R3,MAXNSPT                                                       
         BH    BRAND15             HIT TABLE LIMIT-STOP                         
*                                                                               
         L     R1,LSPTTAB                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)                                                    
         MVC   0(SPTTABL,R2),SPOT  SLOT IN SPOT DATA                            
         ST    R2,ALSTENT          KEEP POINTER TO ENTRY                        
         LA    R2,1(R1,R2)         NEXT ENTRY POSITION                          
*                                                                               
BRAND8   LA    R5,SPOT             RESTORE R5                                   
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'10'        CONTINUE SEARCH                              
         TM    FLAGS,SKPAFFDS                                                   
         BZ    *+8                                                              
         MVI   ELCDHI,X'08'        CONTINUE SEARCH SKIPPING AFFIDS              
         BRAS  RE,NEXTEL                                                        
         BNE   BRAND20                                                          
*                                                                               
BRAND10  CLI   0(R6),X'10'         TEST FOR AFFID ELEMENT                       
         BE    BRAND12             YES                                          
         LR    RF,R6                                                            
         MVC   HALF2,RDATE                                                      
         CLI   SBEPAYDT,C'Y'       TEST TO USE PAY DATE                         
         BNE   *+10                                                             
         MVC   HALF2,RPAY                                                       
         CLC   HALF,HALF2          TEST FOR SPOT ON SAME DATE                   
         BNE   BRAND4              NO-NEW SPOT DATE                             
         TM    RSTATUS,X'04'                                                    
         BO    BRAND8                                                           
         ST    R6,SBASPOT          PASS SPOT TO USER                            
         MVI   SBYORN,YES                                                       
         BAS   RE,GO                                                            
         CLI   SBYORN,NO           TEST IF USER REJECTED IT                     
         BE    BRAND8              YES                                          
*                                                                               
         LLC   R1,RNUM             GET NUMBER OF SPOTS                          
         TM    RSTATUS,X'80'                                                    
         BZ    *+6                                                              
         LNR   R1,R1                                                            
         L     R5,ALSTENT          POINT AT ORIGINAL TABLE ENTRY                
         LH    R0,SPTSPOTS                                                      
         AR    R0,R1                                                            
         STH   R0,SPTSPOTS         UPDATE ORDERED SPOTS                         
         OC    RPAY,RPAY           TEST IF SPOT PAID                            
         BZ    BRAND8              NO-GET NEXT ELEMENT                          
         LH    R0,SPTPAYS                                                       
         AR    R0,R1               YES-ADJUST PAID SPOT COUNT                   
         STH   R0,SPTPAYS                                                       
         CLC   RPAY,SBBTODAY       TEST IF PAID TODAY                           
         BNE   BRAND8                                                           
         LH    R0,SPTPAYTS         YES-ADJUST PAID TODAY COUNT                  
         AR    R0,R1                                                            
         STH   R0,SPTPAYTS                                                      
         B     BRAND8                                                           
*                                                                               
         PUSH  USING                                                            
         USING AFFELEM,R6                                                       
BRAND12  MVC   SPTADATE,ADATE                                                   
         MVC   SPTATIME,ATIME                                                   
         NI    SPTATIME,X'0F'       GET RID OF STATUS BITS                      
         POP   USING                                                            
         XC    SPTACTBK,SPTACTBK                                                
         CLI   SBEDEMTY,0          TEST ACCOUNTING ONLY LOOKUP                  
         BE    BRAND12A                                                         
         CLI   SBEDEMTY,C'0'       TEST SQAD LOOKUP                             
         BNL   BRAND12A                                                         
         BRAS  RE,GETBK            FETCH THE BOOK FOR AFFID DATE                
*                                                                               
BRAND12A XC    SPTCML1,SPTCML1                                                  
         XC    SPTCML2,SPTCML2                                                  
         MVC   SPTSPOTS,=H'1'                                                   
         XC    SPTPAYS,SPTPAYS                                                  
         XC    SPTPAYTS,SPTPAYTS                                                
         CLI   SBQCMLTY,SBQCTTAS   TEST WANT TRAFFIC ASSIGNED COMMLS            
         BE    BRAND14             YES-THEY DON'T EXIST FOR NON-POL             
*                                                                               
         TM    RSTATUS-REGELEM(RF),X'C0'   SPOT HAS BEEN MINUSED?               
         BNZ   BRAND14             IGNORE FILM FOR MINUSED SPOTS                
*                                                                               
         LLC   RE,1(R6)                                                         
         AR    RE,R6               LOOK AT NEXT ELEMENT                         
         CLI   0(RE),X'12'         TEST IF FILM ELEMENT                         
         BNE   BRAND14             NO                                           
*                                                                               
         USING FLMELEM,RE                                                       
         MVC   SPTCML1,FLMNUM                                                   
         CLI   SPTPRD2,0                                                        
         BE    BRAND13                                                          
         CLI   FLMLEN,7                                                         
         BL    *+10                                                             
         MVC   SPTCML2,FLMNUM+2    SECOND COMMERCIAL                            
*                                                                               
BRAND13  OC    SBECML,SBECML       TEST FOR COMMERCIAL FILTERING                
         BZ    BRAND14             NO                                           
         CLC   SBECML+1(2),SPTCML1                                              
         BE    BRAND14                                                          
         CLC   SBECML+1(2),SPTCML2                                              
         BNE   BRAND8                                                           
*                                                                               
BRAND14  L     RE,ALSTENT          GET ORIGINAL ENTRY FOR SPOT                  
         LH    R1,SPTSPOTS-SPTTABD(RE)                                          
         BCTR  R1,0                DECREMENT ITS ORDERED SPOT COUNT             
         STH   R1,SPTSPOTS-SPTTABD(RE)                                          
*                                                                               
         LA    R3,1(R3)            INCREMENT ENTRY COUNT                        
         C     R3,MAXNSPT          TEST AGAINST TABLE LIMIT                     
         BNH   BRAND16                                                          
*                                                                               
BRAND15  TM    FLAGS,DONTDIE                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R1,APARM                                                         
         MVI   0(R1),X'80'                                                      
         SR    R3,R3                                                            
         BCTR  R3,0                TO SET '-' CC ON EXIT                        
         B     BRANDX                                                           
*                                                                               
BRAND16  L     R1,LSPTTAB          ADD ENTRY TO SPOT TABLE                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)                                                    
         MVC   0(SPTTABL,R2),SPOT                                               
         LA    R2,1(R1,R2)                                                      
         B     BRAND8              GET ANOTHER ELEMENT                          
*                                                                               
BRAND20  ST    R3,NSPTTAB                                                       
         ST    R3,SBNSPTEN                                                      
*                                                                               
BRANDX   LTR   R3,R3                                                            
         B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO SET DATE IN SPOT ENTRY                                         
* ON EXIT, CC=EQ IF FITS REQUEST PERIOD,  CC=NEQ IF REJECTED                    
*                                                                               
SETDAT   ST    RE,SAVERE                                                        
         MVC   SPTRDATE,RDATE                                                   
         CLI   SBEPAYDT,C'Y'       TEST TO USE PAY DATE                         
         BNE   *+10                                                             
         MVC   SPTRDATE,RPAY                                                    
         OC    BDMGDATE,BDMGDATE   TEST FOR MAKEGOOD LINE                       
         BZ    SETDAT1             NO                                           
         OC    SBBMGST,SBBMGST     TEST FOR MG AS MISSED OPTION                 
         BZ    SETDAT1                                                          
         CLC   SPTRDATE,SBBMGST                                                 
         BL    SETDAT1                                                          
         MVC   SPTRDATE,BDMGDATE                                                
         OI    SPTIND,SPTNODEM     NO DEMO LOOKUP (ZEN AND MEL SAY)             
*                                                                               
SETDAT1  CLC   SPTRDATE,FSTDATE    TEST SPOT BEFORE REQUEST                     
         BNL   SETDAT2                                                          
         CLI   SBEBEF,YES          YES-TEST TO INCLUDE IT                       
         BNE   SETDATN                                                          
         XC    SPTRDATE,SPTRDATE   YES                                          
         OI    SPTIND,SPTNODEM     NO DEMO LOOKUP                               
         B     SETDATY                                                          
*                                                                               
SETDAT2  CLC   SPTRDATE,LSTDATE                                                 
         BNH   SETDATY                                                          
         CLI   SBEAFTER,YES        AFTER REQUEST                                
         BNE   SETDATN                                                          
         MVC   SPTRDATE,XFF                                                     
         OI    SPTIND,SPTNODEM                                                  
*                                                                               
SETDATY  CR    RB,RB               SET CC=EQ                                    
         B     SETDATX                                                          
*                                                                               
SETDATN  LTR   RB,RB               SET CC=NEQ                                   
*                                                                               
SETDATX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A SPOT TABLE FOR A POL BUY RECORD                        
*                                                                               
* ON EXIT, CC=EQ IF NO SPOTS TO LOOKUP                                          
*          CC=NEQ - SPTTAB BUILT AND NSPTTAB CONTAINS ENTRY COUNT               
*                                                                               
POL      NTR1                                                                   
         L     R2,ASPTTAB          R2=A(SPOT TABLE)                             
         SR    R3,R3               R3=N'SPOT ENTRIES                            
         LA    R5,SPOT             R5=A(POTENTIAL SPOT ENTRY)                   
         USING SPTTABD,R5                                                       
         MVI   ELCDLO,X'0B'        SEARCH FOR SPOT ELEMENTS                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
         TM    SBEFLAG2,SBESPLBY   SPLIT BUYS?                                  
         BZ    POL2                 NO                                          
         OC    SBACONT,SBACONT     IS THIS A CONTINUATION?                      
         BNZ   *+12                 YES                                         
         ST    R6,SBACONT           NO - START AT 1ST EL                        
         B     POL2                                                             
         L     R6,SBACONT          START WITH THIS EL                           
         B     *+12                                                             
*                                                                               
POL2     BRAS  RE,NEXTEL                                                        
         BNE   POL24                                                            
         USING REGELEM,R6                                                       
*                                                                               
         TM    SBEFLAG2,SBESPLBY                                                
         BZ    POL2B                                                            
         CLI   RCODE,X'0B'         STOP ON 0B OR NON-MINUS 0C ELEM ONLY         
         BE    POL2A                                                            
         CLI   RCODE,X'0C'                                                      
         BNE   POL2B                                                            
         TM    RSTATUS,X'80'       IS THIS A MINUS SPOT?                        
         BNZ   POL2B                YES - DON'T STOP HERE                       
*                                                                               
POL2A    SR    RE,RE                                                            
         L     RF,MAXNSPT                                                       
         SLL   RF,3                SPLIT AT 80% OF MAX                          
         D     RE,=F'10'                                                        
         CR    R3,RF                                                            
         BL    POL2B                                                            
         ST    R6,SBACONT                                                       
         B     POL26                                                            
*                                                                               
POL2B    TM    SBEFLAG2,SBE1SPT    SINGLE SPOT CHUNKS?                          
         BZ    *+12                 NO                                          
         TM    RSTATUS,X'C0'       TEST MISSED/MG                               
         BNZ   POL2                 YES - SKIP IT                               
*                                                                               
         TM    RSTATUS,X'04'       TEST FOR HIATUS SPOT                         
         BO    POL2                YES-SKIP OVER IT                             
*                                                                               
         TM    SBEFLAG2,SBENOMIN   SKIP 0B/0C IF SAME?                          
         BZ    POL2C                NO                                          
         LR    RE,R6                                                            
         LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         MVC   DUB(1),6(RE)        SAVE RSTATUS (OR WHATEVER)                   
         NI    6(RE),X'FF'-X'80'   TURN OFF X'80'                               
         OI    6(RE),X'40'         TURN ON X'40'                                
         CLC   1(13,R6),1(RE)      ARE THEY THE SAME?                           
         MVC   6(1,RE),DUB         RESTORE STATUS                               
         BNE   POL2C                NO - KEEP GOING                             
         BRAS  RE,NEXTEL            YES - SKIP 0B & 0C ELEMS                    
         B     POL2                                                             
*                                                                               
POL2C    CLC   RPAY,SBQPAYST       TEST PAID DATE FILTERS                       
         BL    POL2                                                             
         OC    SBQPAYEN,SBQPAYEN                                                
         BZ    *+14                                                             
         CLC   RPAY,SBQPAYEN                                                    
         BH    POL2                                                             
*                                                                               
         CLI   SBESPOTS,SBESPAID   TEST PAID SPOTS ONLY                         
         BNE   *+14                                                             
         OC    RPAY,RPAY                                                        
         BZ    POL2                                                             
         CLI   SBESPOTS,SBESUNPD   TEST UNPAID SPOTS ONLY                       
         BNE   *+14                                                             
         OC    RPAY,RPAY                                                        
         BNZ   POL2                                                             
*                                                                               
         XC    SPOT,SPOT                                                        
         MVC   SPTRDATE,RDATE                                                   
         CLI   SBEPAYDT,C'Y'       TEST TO USE PAY DATE                         
         BNE   *+10                                                             
         MVC   SPTRDATE,RPAY                                                    
         OC    BDMGDATE,BDMGDATE   TEST FOR MAKEGOOD LINE                       
         BZ    POL3                NO                                           
         OC    SBBMGST,SBBMGST     TEST FOR MG AS MISSED OPTION                 
         BZ    POL3                                                             
         CLC   SPTRDATE,SBBMGST    TEST IF SPOT AFTER ST DATE                   
         BL    POL3                                                             
         MVC   SPTRDATE,BDMGDATE   YES-USED MISSED DATE                         
         OI    SPTIND,SPTNODEM     NO DEMO LOOKUP (ZEN AND MEL SAY)             
*                                                                               
POL3     MVC   SPTSLN1,BDSEC       INITIALIZE SLN WITH BUY LINE SLN             
         CLI   RLEN,10             TEST FOR UNALLOCATED SPOT                    
         BNH   POL6                                                             
*                                                                               
         MVC   SPTPRD1,RPPRD       GET FIRST ALLOCATION                         
         TM    BDSTAT2,X'10'       TEST DIY TRADE BUY                           
         BZ    POL4                NO                                           
         CLC   SBQPRD,=C'ALL'      'ALL' REQUESTS NEVER MUSH                    
         BE    POL4                                                             
         CLI   SBQPRD+2,C'#'       TEST TRADE PRODUCT REQUEST                   
         BE    POL4                YES- DON'T MUSH                              
* DEFAULT BELOW IS TO COMBINE CASH/TRADE                                        
         TM    SBEFLAG3,SBE3NTRD   TEST SUPPRESS MUSHING CASH/TRADE             
         BO    POL4                YES                                          
         OI    SBEFLAG3,SBE3TRD    NO - THEN SAY TO MUSH                        
         TM    SBEFLAG3,SBE3TRD    TEST MUSHING TRADE/CASH TOGETHER             
         BZ    POL4                                                             
         NI    SPTPRD1,X'7F'       THEN DROP X'80' IN PRDCODE                   
*                                                                               
POL4     MVC   SPTSLN1,RPTIME                                                   
         CLI   RLEN,14             TEST SECOND ALLOCATION                       
         BNH   POL5                                                             
         MVC   SPTPRD2,RPPRD+L'RPALLOC  SECOND ALLOCATION                       
         MVC   SPTSLN2,RPTIME+L'RPALLOC                                         
*                                                                               
POL5     MVC   SPTPAYDT,RPAY                                                    
         CLI   RLEN,12                                                          
         BNH   POL6                                                             
         MVC   SPTCLRSQ,RPPAYSEQ                                                
*                                                                               
POL6     SR    R0,R0                                                            
         LR    RE,R6               RE=A(ELEMENT)                                
*                                                                               
POL7     IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             TEST FOR EOR                                 
         BE    POL10               YES                                          
         CLI   0(RE),X'0B'         TEST FOR ANOTHER SPOT ELEM                   
         BL    POL7                                                             
         CLI   0(RE),X'0D'                                                      
         BNH   POL10               YES-STOP SEARCH                              
         CLI   0(RE),X'1F'         STOP IF HIGHER THAN X'1F'                    
         BH    POL10                                                            
         CLI   0(RE),X'10'         TEST FOR AFFIDAVIT ELEM                      
         BE    POL8                                                             
         CLI   0(RE),X'12'         TEST FOR FILM ELEM                           
         BE    POL9                                                             
         CLC   0(2,RE),=X'1808'    TEST FOR TRAFFIC ASSIGNED COMMERCIAL         
         BE    POL9A                                                            
         CLI   0(RE),X'17'         TEST FOR RESPONSE COUNT ELEM                 
         BE    POL9C                                                            
         B     POL7                                                             
*                                                                               
         USING AFFELEM,RE                                                       
POL8     MVC   SPTADATE,ADATE                                                   
         MVC   SPTATIME,ATIME                                                   
         NI    SPTATIME,X'0F'       GET RID OF STATUS BITS                      
         B     POL7                                                             
*                                                                               
         USING FLMELEM,RE                                                       
POL9     CLI   SBQCMLTY,SBQCTTAS   TEST WANT TRAFFIC ASSIGNED COMMLS            
         BE    POL7                YES-IGNORE FILM ELEMENTS                     
         TM    RSTATUS,X'C0'       IGNORE FILM FOR MINUSED SPOTS                
         BNZ   POL7                                                             
         MVC   SPTCML1,FLMNUM                                                   
         CLI   FLMLEN,7            TEST FOR SECOND COMMERCIAL                   
         BL    *+10                                                             
         MVC   SPTCML2,FLMNUM+2                                                 
         CLI   SPTPRD2,0           TEST FOR SECOND PRODUCT                      
         BE    POL7                NO                                           
         B     POL9B                                                            
*                                                                               
         USING TRACID,RE                                                        
POL9A    CLI   SBQCMLTY,SBQCTTAS   TEST WANT TRAFFIC ASSIGNED COMMLS            
         BNE   POL7                                                             
         TM    RSTATUS,X'C0'       IGNORE FILM FOR MINUSED SPOTS                
         BNZ   POL7                                                             
         MVC   SPTCML1,TRACCSQ     YES-                                         
         CLI   SPTPRD2,0           TEST FOR 2ND PRODUCT                         
         BE    POL7                                                             
         MVC   SPTCML2,TRACCSQ2                                                 
*                                                                               
POL9B    CLI   SPTPRD1,0           TEST FOR FIRST PRODUCT                       
         BNE   POL7                                                             
         MVC   SPTPRD1,SPTPRD2     NO-THEN 1ST BECOMES 2ND                      
         MVC   SPTSLN1,SPTSLN2                                                  
         MVC   SPTCML1,SPTCML2                                                  
         MVI   SPTPRD2,0                                                        
         MVI   SPTSLN2,0                                                        
         MVI   SPTCML2,0                                                        
         B     POL7                                                             
*                                                                               
         USING RSVPELEM,RE                                                      
POL9C    MVC   SPTDPT,RSVPDPT      ACTUAL RESPONSE DAYPART                      
         MVC   SPTRSVPS,RSVPNUM    NUMBER OF RESPONSES                          
*                                                                               
POL10    BRAS  RE,FILTER                                                        
         BNE   POL2                REJECT SPOT                                  
         ST    R6,SBASPOT                                                       
         MVI   SBYORN,YES          PASS SPOT TO USER                            
         BAS   RE,GO                                                            
         CLI   SBYORN,NO           TEST IF USER REJECTED IT                     
         BE    POL2                YES                                          
*                                                                               
         CLI   SBEDEMTY,0          TEST ACCOUNTING ONLY                         
         BE    POL11               YES                                          
         TM    RSTATUS,X'C0'       TEST MISSED/MAKE-GOOD                        
         BNZ   POL10A              YES-SO SKIP DEMO LOOKUP                      
         CLI   SBEDEMTY,C'0'       TEST SQAD                                    
         BNL   POL11               YES                                          
         BRAS  RE,GETBK            GET THE ACTUAL BOOK                          
         B     POL11                                                            
*                                                                               
POL10A   OI    SPTIND,SPTNODEM                                                  
*                                                                               
POL11    MVC   DUB(1),BUYKPRD                                                   
         MVC   DUB+1(1),BDSEC                                                   
         CLI   SPTPRD2,0           TEST FOR PIGGYBACK                           
         BE    POL13                                                            
         CLI   SPTPRD1,0           YES-TEST FOR 1ST PRODUCT                     
         BNE   POL12                                                            
         MVC   SPTPRD1,SPTPRD2     NO-THEN 1ST BECOMES 2ND                      
         MVC   SPTSLN1,SPTSLN2                                                  
         MVC   SPTCML1,SPTCML2                                                  
         MVI   SPTPRD2,0                                                        
         MVI   SPTSLN2,0                                                        
         MVI   SPTCML2,0                                                        
*                                                                               
POL12    MVC   DUB(1),SPTPRD1      LOOKUP FIRST PRODUCT                         
         MVC   DUB+1(1),SPTSLN1                                                 
*                                                                               
POL13    GOTO1 GOGETRAT,DMCB,(DUB,SPOTS),(DUB+1,(R4)),(GRIND,(R6)),    C        
               (CURRENCY,XGROSS)                                                
         MVC   SPTGRS1(8),GROSS    GROSS/NET                                    
         MVC   SPTGST1,XGSTAMT     GST                                          
*                                                                               
* NEED PST INFO EVEN IF ONLY WANT GST INCASE HST APPLIES (NO GST!)              
*        TM    SBEGST,SBEPSTI+SBEPSTO+SBEPSTB WANT PST OR...                    
*        BZ    POL13A                                                           
         LA    RF,XPSTTAB                                                       
         BRAS  RE,SHIFTPST                                                      
         ICM   RE,15,XPSTAMT                                                    
         ST    RE,SPTTAX1          SAVE PST                                     
         MVC   SPTPSTCD,XPSTCODE                                                
         MVC   SPTPSTRT,XPSTRATE                                                
         MVC   SPTPSTPV,XPSTPROV+1                                              
*                                                                               
POL13A   MVC   SPTSPOTS,SPOTS+2                                                 
         OC    RPAY,RPAY           TEST FOR PAID SPOT                           
         BZ    POL14                                                            
         MVC   SPTPAYS,SPTSPOTS    YES                                          
         OI    SPTIND,SPTSPAID                                                  
         CLC   RPAY,SBBTODAY       TEST IF PAID TODAY                           
         BNE   *+10                                                             
         MVC   SPTPAYTS,SPTSPOTS   YES-SET PAID TODAY ALSO                      
*                                                                               
POL14    OC    BDNTAX,BDNTAX       TEST FOR TAX                                 
         BNZ   POL16               YES                                          
         OC    ATAXEL,ATAXEL       TEST FOR CANADIAN NETWORK TAX                
         BZ    POL18               NO                                           
         TM    SBEGST,SBEPSTI+SBEPSTO+SBEPSTB                                   
         BNZ   POL18               PST USES SAME SPT... FIELD (& WINS)          
*                                                                               
POL16    MVC   HALF,BDNTAX                                                      
         XC    BDNTAX,BDNTAX                                                    
         ICM   RE,15,ATAXEL                                                     
         BZ    *+8                                                              
         OI    0(RE),X'80'                                                      
*                                                                               
         GOTO1 GOGETRAT,DMCB,(DUB,SPOTS),(DUB+1,(R4)),(GRIND,(R6)),    C        
               (CURRENCY,XGROSS)                                                
         ICM   RE,15,ATAXEL                                                     
         BZ    *+8                                                              
         NI    0(RE),X'7F'                                                      
         L     RE,SPTGRS1                                                       
         L     RF,GROSS                                                         
         SR    RE,RF               COMPUTE TAX                                  
         ST    RE,SPTTAX1                                                       
         MVC   BDNTAX,HALF                                                      
*                                                                               
POL18    CLI   SPTPRD2,0           TEST FOR PIGGYBACK                           
         BE    POL22               NO                                           
         MVC   DUB(1),SPTPRD2      LOOKUP SECOND PRODUCT                        
         MVC   DUB+1(1),SPTSLN2                                                 
*                                                                               
         GOTO1 GOGETRAT,DMCB,(DUB,SPOTS),(DUB+1,(R4)),(GRIND,(R6)),    C        
               (CURRENCY,XGROSS)                                                
         MVC   SPTGRS2(8),GROSS    GROSS/NET                                    
         TM    SBEFLAG2,SBEBOOK    EXTRACTING BOOK?                             
         BNZ   POL18A              YES - NOT COMPAT WITH GST                    
         MVC   SPTGST2,XGSTAMT     GST                                          
*                                                                               
* NEED PST INFO EVEN IF ONLY WANT GST INCASE HST APPLIES (NO GST!)              
*        TM    SBEGST,SBEPSTI+SBEPSTO+SBEPSTB WANT PST OR...                    
*        BZ    POL18A                                                           
         LA    RF,XPSTTAB                                                       
         BRAS  RE,SHIFTPST                                                      
         ICM   RE,15,XPSTAMT                                                    
         ST    RE,SPTTAX2          SAVE PST                                     
         MVC   SPTPSTCD,XPSTCDE2   SHOULD NOT DIFFER TO PST1 VALS...            
         MVC   SPTPSTRT,XPSTRAT2   ...BUT SET AGAIN TO ENSURE SET!              
         MVC   SPTPSTPV,XPSTPRV2+1                                              
*                                                                               
POL18A   OC    BDNTAX,BDNTAX       TEST FOR TAX                                 
         BNZ   POL20               YES                                          
         OC    ATAXEL,ATAXEL       TEST FOR CANADIAN NETWORK TAX                
         BZ    POL22               NO                                           
         TM    SBEGST,SBEPSTI+SBEPSTO+SBEPSTB                                   
         BNZ   POL22               PST USES SAME SPT... FIELD (& WINS)          
*                                                                               
POL20    MVC   HALF,BDNTAX                                                      
         XC    BDNTAX,BDNTAX                                                    
         ICM   RE,15,ATAXEL                                                     
         BZ    *+8                                                              
         OI    0(RE),X'80'                                                      
*                                                                               
         GOTO1 GOGETRAT,DMCB,(DUB,SPOTS),(DUB+1,(R4)),(GRIND,(R6)),    C        
               (CURRENCY,XGROSS)                                                
         ICM   RE,15,ATAXEL                                                     
         BZ    *+8                                                              
         NI    0(RE),X'7F'                                                      
         L     RE,SPTGRS2                                                       
         L     RF,GROSS                                                         
         SR    RE,RF               COMPUTE TAX                                  
         ST    RE,SPTTAX2                                                       
         MVC   BDNTAX,HALF                                                      
*                                                                               
POL22    LA    R3,1(R3)            INCREMENT ENTRY COUNT                        
         C     R3,MAXNSPT          TEST AGAINST TABLE LIMIT                     
         BNH   POL23               NOT OVER LIMIT                               
         TM    SBEFLAG2,SBENOMIN   DIE OR RETURN ERROR?                         
         BNZ   *+6                 RETURN ERROR                                 
         DC    H'0'                OVER THE LIMIT                               
         NI    SBEFLAG2,X'FF'-SBENOMIN    TURN OFF AS FLAG                      
         SR    R3,R3                                                            
         B     POLX                                                             
*                                                                               
POL23    L     R1,LSPTTAB          GET ENTRY LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)       CLEAR ROOM FOR ENTRY                         
         MVC   0(SPTTABL,R2),SPOT  ADD ENTRY TO SPOT TABLE                      
         LA    R2,1(R1,R2)         POINT TO NEXT ENTRY                          
         B     POL2                                                             
*                                                                               
POL24    XC    SBACONT,SBACONT     DONE W/ELEMS - CLEAR CONT FLAG               
*                                                                               
POL26    ST    R3,NSPTTAB          SAVE ENTRY COUNT                             
         ST    R3,SBNSPTEN         SAVE ENTRY COUNT                             
*                                                                               
POLX     LTR   R3,R3               SET CC ON EXIT                               
         B     EXIT                                                             
         DROP  R5,R6,RE                                                         
         EJECT                                                                  
* SUB-ROUTINE TO FIND THE EQUIVALENCE TABLE                                     
*                                                                               
EQTAB    ST    RE,SAVERE                                                        
         LA    RF,L'SBDPTTAB/5     RF=COUNTER                                   
         LA    RE,SBDPTTAB         RE=A(DPT TABLE)                              
         CLC   BDDAYPT,0(RE)       MATCH ON DAYPART CODE                        
         BE    EQTAB2                                                           
         LA    RE,5(RE)                                                         
         BCT   RF,*-14                                                          
         B     EQTABX                                                           
*                                                                               
EQTAB2   LLC   RF,1(RE)            DAYPART INTERNAL CODE                        
         N     RF,=F'15'           ZERO HIGH ORDER NIBBLE                       
         IC    RF,SBDPEQTB(RF)     TABLE IND                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         BCTR  RF,0                                                             
         MHI   RF,60                                                            
*                                                                               
         LA    RF,SBEQTAB(RF)       EQUIV TABLE                                 
         ST    RF,AEQTAB                                                        
*                                                                               
EQTABX   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE EQUIVALENCE FACTOR FOR A SECONDS LENGTH                
*                                                                               
* AT ENTRY, BYTE CONTAINS SECONDS LENGTH, ON EXIT, EQFACT CONTAINS              
* EQUIVALENCY FACTOR                                                            
*                                                                               
GETEQU   NTR1                                                                   
         XC    EQFACT,EQFACT                                                    
         ICM   R2,15,AEQTAB        RF=A(EQUIV TABLE FOR DPT)                    
         BZ    GETEQU4                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A57'                                           
         GOTO1 VCALLOV,DMCB,0         GET SPSLENTAB                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   HALF,C'T'                                                        
         CLI   SBMED,C'T'                                                       
         BE    GETEQU1                                                          
         CLI   SBMED,C'N'                                                       
         BE    GETEQU1                                                          
         CLI   SBMED,C'C'                                                       
         BE    GETEQU1                                                          
*                                                                               
         MVI   HALF,C'R'                                                        
         CLI   SBMED,C'R'                                                       
         BE    GETEQU1                                                          
         CLI   SBMED,C'X'                                                       
         BE    GETEQU1                                                          
         DC    H'0'                                                             
*                                                                               
GETEQU1  CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    GETEQU2                                                          
         CLC   0(2,R1),SBAGY       ELSE MATCH AGY                               
         BNE   *+14                                                             
GETEQU2  CLC   HALF(1),2(R1)       AND MEDIA                                    
         BE    GETEQU3                                                          
*                                                                               
         BXLE  R1,RE,GETEQU1                                                    
         DC    H'0'                                                             
*                                                                               
GETEQU3  AHI   R1,4                POINT BEYOND HEADER                          
         LLC   R3,BYTE             GET SLN                                      
         AR    R3,R3               X 2                                          
         AR    R1,R3               POINT TO ENTRY                               
         CLI   1(R1),0             SLN VALID?                                   
         BE    GETEQU4             NO                                           
         LLC   R3,0(R1)            GET INDEX INTO EQU TABLE                     
         AR    R2,R3               INDEX INTO TABLE                             
         MVC   EQFACT+2(2),0(R2)                                                
*                                                                               
GETEQU4  OC    EQFACT,EQFACT                                                    
         BNZ   GETEQUX                                                          
         MVC   EQFACT,=F'1000'     DEFAULT TO BASE                              
*                                                                               
GETEQUX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GENERATE THE CHUNK ENTRIES FOR BRAND BUY USING                 
* THE SPOT TABLE                                                                
*                                                                               
UPBR     NTR1                                                                   
         LM    R2,R4,ASPTTAB       R2=A(SPTTAB),R3=N'NTRYS,R4=L'NTRY            
         USING SPTTABD,R2                                                       
         LA    R7,CHUNK                                                         
*                                                                               
UPBR2    TM    SPTIND,SPTDUMMY     TEST TO EXCLUDE THIS ENTRY                   
         BO    UPBR10              YES                                          
         BAS   RE,CLRCHUNK         CLEAR CHUNK AREA                             
         BAS   RE,GETDAT           SET THE CHUNK DATE                           
         BNE   UPBR10                                                           
         MVC   GROSS(12),SPTGRS1                                                
         MVC   XGSTAMT,SPTGST1                                                  
         MVC   GROSS2(12),SPTGRS2                                               
         MVC   XGSTAMT2,SPTGST2                                                 
***      CLI   SPTPSTCD,0          ANY PST JIGGERY-POKERY TO DO?                
***      BE    UPBR3A                                                           
         CLI   CANADA,C'Y'         CANADA?                                      
         BNE   UPBR3A              NO                                           
         MVC   XPSTCODE,SPTPSTCD                                                
         MVC   XPSTRATE,SPTPSTRT                                                
         MVC   XPSTPROV+1(1),SPTPSTPV                                           
         MVC   XPSTAMT,SPTTAX1                                                  
         MVC   XPSTAMT2,SPTTAX2                                                 
         XC    TAX,TAX             WOULD HAVE BEEN FILLED WITH PST!             
         XC    TAX2,TAX2                                                        
UPBR3A   BAS   RE,GETDEM           GET DEMOS FOR CHUNK                          
         BAS   RE,GETAFD           AFFIDAVIT DETAILS IF ANY                     
         CLI   SBEBYCHK,YES        BY CHECK                                     
         BNE   *+8                                                              
         BRAS  RE,GETCHK                                                        
*                                                                               
         CLI   SBEPRD,0            TEST 'ALL' PRODUCT REQUEST                   
         BE    UPBR4               YES-ALWAYS BUILD CHUNK FOR 1ST PRD           
         CLI   SBEPRD,X'FF'                                                     
         BE    UPBR4                                                            
*                                  8/15 - FLIPPING OF NON-POOL PIGGIES          
         CLI   SPTPRD2,0           TEST SECOND PRODUCT ASSIGNED                 
         BE    UPBR3               NO                                           
         CLI   SBESPLIT,NO         YES-TEST SPLITTING PIGGYBACKS                
         BE    UPBR4X              NO                                           
*                                                                               
UPBR3    CLC   SBEPRD,SPTPRD1      TEST REQUESTED BRAND=1ST PRODUCT             
         BNE   UPBR6                                                            
*                                                                               
UPBR4    CLI   SPTPRD2,0           TEST SECOND PRODUCT ASSIGNED                 
         BE    UPBR5               NO                                           
         CLI   SBESPLIT,NO         YES-TEST SPLITTING PIGGYBACKS                
         BNE   UPBR5               YES                                          
UPBR4X   BAS   RE,PIGCHUNK         NO-COMBINE PRODUCTS IN ONE CHUNK             
         B     UPBR10                                                           
*                                                                               
UPBR5    MVC   SCPRD1,SPTPRD1      FIRST PRODUCT                                
         CLI   SBEBYSLN,YES        TEST SLN BREAK OUT                           
         BNE   *+10                NO                                           
         MVC   SCSLN1,SPTSLN1      YES-PUT IT IN KEY                            
         CLI   SBEBYCML,YES        TEST BY COMMERCIAL                           
         BNE   *+10                                                             
         MVC   SCML1,SPTCML1                                                    
         GOTO1 GETDOL,1            DOLLARS FOR FIRST PRODUCT                    
         MVC   BYTE,SPTSLN1        GET EQUIVALENCY FACTOR                       
         BAS   RE,GETEQU                                                        
         BRAS  RE,CALC             CALCULATE DERIVABLE CHUNK NUMBERS            
         BAS   RE,POST             POST CHUNK FOR FIRST PRODUCT                 
*                                                                               
UPBR6    CLI   SPTPRD2,0           TEST SECOND PRODUCT ASSIGNED                 
         BE    UPBR10              NO-ALL DONE                                  
         CLI   SBEPRD,0            TEST 'ALL' PRODUCTS REQUESTED                
         BE    UPBR8               YES                                          
         CLI   SBEPRD,X'FF'                                                     
         BE    UPBR8                                                            
         CLI   SBESPLIT,NO         TEST SPLITTING PIGGYBACKS                    
         BE    UPBR10              NO-ALL DONE                                  
         CLC   SBEPRD,SPTPRD2      TEST REQUESTED BRAND=2ND PRODUCT             
         BNE   UPBR10                                                           
*                                                                               
UPBR8    MVC   SCPRD1,SPTPRD2                                                   
         CLI   SBEBYSLN,YES                                                     
         BNE   *+10                                                             
         MVC   SCSLN1,SPTSLN2                                                   
         CLI   SBEBYCML,YES                                                     
         BNE   *+10                                                             
         MVC   SCML1,SPTCML2                                                    
         GOTO1 GETDOL,2                                                         
         MVC   BYTE,SPTSLN2                                                     
         BAS   RE,GETEQU                                                        
         BRAS  RE,CALC                                                          
         BAS   RE,POST                                                          
*                                                                               
UPBR10   LA    R2,0(R4,R2)         NEXT TABLE ENTRY                             
         BCT   R3,UPBR2                                                         
*                                                                               
UPBRX    B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO GENERATE CHUNK ENTRIES FOR A POL BUY                           
*                                                                               
UPPOL    NTR1                                                                   
         LM    R2,R4,ASPTTAB                                                    
         USING SPTTABD,R2                                                       
         LA    R7,CHUNK                                                         
         CLI   SBEPRD,0            TEST 'ALL' PRODUCTS REQUESTED                
         BE    UPPOL10             YES                                          
         CLI   SBEPRD,X'FE'        TEST 'POL' REQUESTED                         
         BNL   UPPOL10             YES                                          
         TM    SBEFLAG3,SBE3TRD    TEST MUSH CASH/TRADE                         
         BO    UPPOL10                                                          
*                                                                               
* SPECIFIC PRODUCT REQUEST                                                      
*                                                                               
UPPOL2   TM    SPTIND,SPTDUMMY     TEST TO EXCLUDE ENTRY                        
         BO    UPPOL8              YES                                          
         BAS   RE,CLRCHUNK         BRAND REQUEST                                
         CLI   SPTPRD1,0           TEST FOR UNALLOCATED SPOT                    
         BE    UPPOL8              YES-SKIP IT                                  
         BAS   RE,GETDAT                                                        
         BNE   UPPOL8                                                           
         MVC   GROSS(12),SPTGRS1                                                
         MVC   XGSTAMT,SPTGST1                                                  
         MVC   GROSS2(12),SPTGRS2                                               
         MVC   XGSTAMT2,SPTGST2                                                 
***      CLI   SPTPSTCD,0          ANY PST JIGGERY-POKERY TO DO?                
***      BE    UPPOL2A                                                          
         CLI   CANADA,C'Y'         CANADA?                                      
         BNE   UPPOL2A             NO                                           
         MVC   XPSTCODE,SPTPSTCD                                                
         MVC   XPSTRATE,SPTPSTRT                                                
         MVC   XPSTPROV+1(1),SPTPSTPV                                           
         MVC   XPSTAMT,SPTTAX1                                                  
         MVC   XPSTAMT2,SPTTAX2                                                 
         XC    TAX,TAX             WOULD HAVE BEEN FILLED WITH PST!             
         XC    TAX2,TAX2                                                        
UPPOL2A  BAS   RE,GETDEM                                                        
         BAS   RE,GETAFD                                                        
         CLI   SBEBYCHK,YES        BY CHECK                                     
         BNE   *+8                                                              
         BRAS  RE,GETCHK                                                        
*                                                                               
         CLI   SBESPLIT,NO         TEST SPLITTING PIGGYBACKS                    
         BNE   UPPOL4              YES                                          
         CLI   SPTPRD2,0           NO-TEST FOR SECOND PRODUCT                   
         BE    UPPOL4              NO                                           
         BAS   RE,PIGCHUNK         YES-COMBINE PRODUCTS IN ONE CHUNK            
         B     UPPOL8                                                           
*                                                                               
UPPOL4   LA    R5,SPTPRD1          R5=A(PRODUCT/SLN/CML)                        
         LA    R1,1                R1=PRODUCT PARAMETER                         
         CLC   SBEPRD,SPTPRD1      TEST REQUESTED PRD=1ST PRD                   
         BE    *+12                YES                                          
         LA    R5,SPTPRD2          THEN ITS SECOND PRODUCT                      
         LA    R1,2                                                             
*                                                                               
         MVC   SCPRD1,0(R5)        EXTRACT PRODUCT CODE                         
         CLI   SBEBYSLN,YES        TEST SLN BREAK-OUT                           
         BNE   *+10                                                             
         MVC   SCSLN1,1(R5)                                                     
         CLI   SBEBYCML,YES        TEST REPORT BY COMMERCIAL                    
         BNE   UPPOL6                                                           
         MVC   SCML1,2(R5)         YES                                          
         MVC   SCML2,SPTCML2       SET SCML2 FOR AD-IDCD2 KEYWORD               
*                                                                               
UPPOL6   GOTO1 GETDOL,(R1)                                                      
         MVC   BYTE,1(R5)          SET SLN                                      
         BAS   RE,GETEQU           FIND ITS EQUIVALENCE FACTOR                  
         BRAS  RE,CALC             CALCULATE DERIVABLE NUMBERS IN CHUNK         
         BAS   RE,POST             POST CHUNK ENTRY                             
*                                                                               
UPPOL8   LA    R2,0(R4,R2)         NEXT TABLE ENTRY                             
         BCT   R3,UPPOL2                                                        
         B     UPPOLX                                                           
*                                                                               
* 'POL' OR 'ALL' REQUEST                                                        
*                                                                               
UPPOL10  TM    SPTIND,SPTDUMMY     TEST TO EXCLUDE THIS ENTRY                   
         BO    UPPOL25             YES                                          
         CLI   SPTPRD2,0           TEST FOR PIGGYBACK                           
         BNE   UPPOL15             YES                                          
*                                                                               
         BAS   RE,CLRCHUNK         UNALLOCATED OR SOLO SPOT                     
         BAS   RE,GETDAT           GET CHUNK DATE                               
         BNE   UPPOL25                                                          
         MVC   GROSS(12),SPTGRS1                                                
         MVC   XGSTAMT,SPTGST1                                                  
         XC    GROSS2(12),GROSS2                                                
***      CLI   SPTPSTCD,0          ANY PST JIGGERY-POKERY TO DO?                
***      BE    UPPOL11                                                          
         CLI   CANADA,C'Y'         CANADA?                                      
         BNE   UPPOL11             NO                                           
         MVC   XPSTCODE,SPTPSTCD                                                
         MVC   XPSTRATE,SPTPSTRT                                                
         MVC   XPSTPROV+1(1),SPTPSTPV                                           
         MVC   XPSTAMT,SPTTAX1                                                  
         XC    TAX,TAX             WOULD HAVE BEEN FILLED WITH PST!             
UPPOL11  BAS   RE,GETDEM           AND DEMOS                                    
         BAS   RE,GETAFD                                                        
         CLI   SBEBYCHK,YES        BY CHECK                                     
         BNE   *+8                                                              
         BRAS  RE,GETCHK                                                        
*                                                                               
         MVI   SCPRD1,X'FE'        PRODUCT=UNALLOCATED                          
         CLI   SPTPRD1,0           TEST FOR UNALLOCATED SPOT                    
         BE    *+8                                                              
         MVI   SCPRD1,X'FF'        NO-SET PRODUCT=POL                           
         CLI   SBEBYSLN,YES                                                     
         BNE   *+10                                                             
         MVC   SCSLN1,SPTSLN1                                                   
         CLI   SBEBYCML,YES                                                     
         BNE   *+16                                                             
         MVC   SCML1,SPTCML1                                                    
         MVC   SCML2,SPTCML2                                                    
*                                                                               
UPPOL14  GOTO1 GETDOL,1            BASE DOLLARS FOR 1ST PRD                     
         MVC   BYTE,SPTSLN1                                                     
         BAS   RE,GETEQU           GET EQUIVALENCE FACTOR                       
         BRAS  RE,CALC             COMPUTE DERIVABLE NUMBERS FOR CHUNK          
         BAS   RE,POST                                                          
*                                                                               
         CLI   SPTPRD1,0           TEST FOR UNALLOCATED SPOT                    
         BE    UPPOL25             YES-GO TO NEXT TABLE ENTRY                   
         CLI   SBEBYPRD,YES        TEST PRODUCT BREAK OUT                       
         BNE   UPPOL25             NO-NEXT ENTRY                                
         MVC   SCPRD1,SPTPRD1      YES-WRITE ADDITIONAL ENTRY FOR PROD          
         MVC   SPOTDATE,SPTRDATE   PASS SPOT DATE TO GETMOS                     
         BRAS  RE,CALCECST         GET EFFECTIVE COST FOR SPECIFIC PRD          
         BAS   RE,POST                                                          
         B     UPPOL25                                                          
*                                                                               
* PIGGYBACK PROCESSING                                                          
*                                                                               
UPPOL15  BAS   RE,CLRCHUNK                                                      
         BAS   RE,GETDAT           CHUNK DATE                                   
         BNE   UPPOL25                                                          
         MVC   GROSS(12),SPTGRS1   BASE NUMBERS FOR BOTH PRODUCTS               
         MVC   XGSTAMT,SPTGST1                                                  
         MVC   GROSS2(12),SPTGRS2                                               
         MVC   XGSTAMT2,SPTGST2                                                 
***      CLI   SPTPSTCD,0          ANY PST JIGGERY-POKERY TO DO?                
***      BE    UPPOL15A                                                         
         CLI   CANADA,C'Y'         CANADA?                                      
         BNE   UPPOL15A            NO                                           
         MVC   XPSTCODE,SPTPSTCD                                                
         MVC   XPSTRATE,SPTPSTRT                                                
         MVC   XPSTPROV+1(1),SPTPSTPV                                           
         MVC   XPSTAMT,SPTTAX1                                                  
         MVC   XPSTAMT2,SPTTAX2                                                 
         XC    TAX,TAX             WOULD HAVE BEEN FILLED WITH PST!             
         XC    TAX2,TAX2                                                        
UPPOL15A BAS   RE,GETDEM           CHUNK DEMOS                                  
         BAS   RE,GETAFD                                                        
         CLI   SBEBYCHK,YES        BY CHECK                                     
         BNE   *+8                                                              
         BRAS  RE,GETCHK                                                        
*                                                                               
* FIRST DEAL WITH CHUNKS FOR 'POL'                                              
*                                                                               
         MVI   SCPRD1,X'FF'                                                     
         CLI   SBEBYCML,YES                                                     
         BNE   *+10                                                             
         MVC   SCML1,SPTCML1                                                    
         CLI   SBEBYSLN,YES        TEST REPORT BY SLN                           
         BNE   *+12                NO-ONLY ONE CHUNK FOR POL                    
         CLI   SBESPLIT,NO         TEST IF SPLITTING POL P/B                    
         BNE   UPPOL18             YES-WRITE OUT TWO 'POL' CHUNKS               
*                                                                               
* ONE CHUNK FOR 'POL'                                                           
*                                                                               
UPPOL16  LLC   R5,SPTSLN1                                                       
         LLC   RE,SPTSLN2                                                       
         AR    R5,RE               R5=TOTAL SLN FOR SPOT                        
         CLI   SBEBYSLN,YES        TEST IF REPORTING BY SLN                     
         BNE   *+8                 NO                                           
         STC   R5,SCSLN1                                                        
         GOTO1 GETDOL,3            DOLLARS FOR BOTH PRODUCTS                    
         STC   R5,BYTE                                                          
         BAS   RE,GETEQU           EQUIVALENCE FACTOR FOR TOTAL SLN             
         BRAS  RE,CALC                                                          
         BAS   RE,POST                                                          
         B     UPPOL20                                                          
*                                                                               
* TWO CHUNKS FOR 'POL' - ONE FOR EACH SLN                                       
*                                                                               
UPPOL18  MVC   SCSLN1,SPTSLN1      FIRST POL/SLN                                
         GOTO1 GETDOL,1            DOLLARS FOR 1ST PRD                          
         MVC   BYTE,SPTSLN1                                                     
         BAS   RE,GETEQU                                                        
         BRAS  RE,CALC                                                          
         BAS   RE,POST                                                          
*                                                                               
         MVC   SCSLN1,SPTSLN2      SECOND POL/SLN                               
         CLI   SBEBYCML,YES        TEST REPORTING BY COMMERCIAL                 
         BNE   *+10                                                             
         MVC   SCML1,SPTCML2       YES-SECOND COMMERCIAL                        
         GOTO1 GETDOL,2            DOLLARS FOR SECOND PRODUCT                   
         MVC   BYTE,SPTSLN2                                                     
         BAS   RE,GETEQU                                                        
         BRAS  RE,CALC                                                          
         BAS   RE,POST                                                          
*                                                                               
* CHUNKS FOR THE SPECIFIC PRODUCTS                                              
*                                                                               
UPPOL20  CLI   SBEBYPRD,YES        TEST PRODUCT BREAK/OUT                       
         BNE   UPPOL25             NO-NEXT TABLE ENTRY                          
*                                                                               
         CLI   SBESPLIT,NO         TEST IF SPLITTING POL P/B                    
         BNE   *+12                YES                                          
         BAS   RE,PIGCHUNK         NO-COMBINE BOTH PRDS IN ONE CHUNK            
         B     UPPOL25                                                          
*                                                                               
         MVC   SCPRD1,SPTPRD1      FIRST PRODUCT                                
         CLI   SBEBYSLN,YES                                                     
         BNE   *+10                                                             
         MVC   SCSLN1,SPTSLN1                                                   
         CLI   SBEBYCML,YES                                                     
         BNE   *+10                                                             
         MVC   SCML1,SPTCML1                                                    
         GOTO1 GETDOL,1                                                         
         MVC   BYTE,SPTSLN1        FIRST SLN                                    
         BAS   RE,GETEQU           GET ITS EQUIVALENCE FACTOR                   
         BRAS  RE,CALC                                                          
         BAS   RE,POST                                                          
*                                                                               
UPPOL22  MVC   SCPRD1,SPTPRD2      SECOND PRODUCT                               
         CLI   SBEBYSLN,YES                                                     
         BNE   *+10                                                             
         MVC   SCSLN1,SPTSLN2                                                   
         CLI   SBEBYCML,YES                                                     
         BNE   *+10                                                             
         MVC   SCML1,SPTCML2                                                    
         GOTO1 GETDOL,2            DOLLARS FOR SECOND PRODUCT                   
         MVC   BYTE,SPTSLN2                                                     
         BAS   RE,GETEQU                                                        
         BRAS  RE,CALC                                                          
         BAS   RE,POST                                                          
*                                                                               
UPPOL25  LA    R2,0(R4,R2)         NEXT TABLE ENTRY                             
         BCT   R3,UPPOL10                                                       
*                                                                               
UPPOLX   B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO GENERATE A CHUNK ENTRY FOR A PIGGYBACK PRODUCT PAIR             
*                                                                               
PIGCHUNK NTR1                                                                   
         MVC   SCPRD1,SPTPRD1      COMBINE PRODUCTS IN ONE CHUNK                
         MVC   SCPRD2,SPTPRD2                                                   
         CLI   SBEBYSLN,YES        TEST SLN BREAK-OUT                           
         BNE   *+16                                                             
         MVC   SCSLN1,SPTSLN1                                                   
         MVC   SCSLN2,SPTSLN2                                                   
         CLI   SBEBYCML,YES        TEST REPORT BY COMMERCIAL                    
         BNE   *+16                                                             
         MVC   SCML1,SPTCML1                                                    
         MVC   SCML2,SPTCML2                                                    
         GOTO1 GETDOL,3            DOLLARS FOR BOTH PRODUCTS                    
         LLC   R1,SPTSLN1                                                       
         LLC   RE,SPTSLN2                                                       
         AR    R1,RE               COMBINE THE SPOTLENGTHS                      
         STC   R1,BYTE                                                          
         BAS   RE,GETEQU           FIND ITS EQUIVALENCE FACTOR                  
         BRAS  RE,CALC             CALCULATE DERIVABLE NUMBERS IN CHUNK         
         BAS   RE,POST             POST CHUNK ENTRY                             
         B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO CLEAR THE CHUNK AREA                                           
*                                                                               
CLRCHUNK ST    RE,SAVERE                                                        
         LA    RE,CHUNK                                                         
         LA    RF,L'CHUNK                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO FIND THE CHUNK DATE FOR A SPOT TABLE ENTRY                     
* AT ENTRY, R2=A(SPOT TABLE ENTRY)  R7=A(CHUNK)                                 
* ON EXIT,  CC EQ - DATE IS OK                                                  
*              NE - DATE IS IN HOLE IN DATES TABLE                              
*                                                                               
GETDAT   ST    RE,SAVERE                                                        
         L     RE,SBADATE          RE=A(DATE LIST)                              
         L     R0,SBNDATES         R0=COUNTER                                   
         MVC   SCDATE,SPTRDATE                                                  
         OC    SCDATE,SCDATE       TEST FOR ZERO DATE                           
         BZ    GETDATX             YES                                          
         CLC   SCDATE,XFF                                                       
         BE    GETDATX                                                          
*                                                                               
GETDAT2  CLC   SPTRDATE,0(RE)                                                   
         BL    *+14                                                             
         CLC   SPTRDATE,2(RE)                                                   
         BNH   GETDAT4                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,GETDAT2                                                       
         L     RE,SAVERE                                                        
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
GETDAT4  MVC   SCDATE,0(RE)        USE START OF DATE ENTRY                      
*                                                                               
         CLI   SBECS,YES           TEST TO EXTRACT CHILD SPOT                   
         BNE   GETDATX                                                          
         OC    SBTADATE,SBTADATE   YES-TEST 2ND SET OF TALENT FACTORS           
         BZ    GETDATX                                                          
         CLC   SPTRDATE,SBTADATE   YES-COMPARE SPOT DATE TO EFFECTIVE           
         BL    GETDATX                 DATE FOR 2ND SET                         
         CLC   SCDATE,SBTADATE     NOT EARLIER-COMPARE CHUNK DATE               
         BNL   GETDATX                                                          
         MVC   SCDATE,SBTADATE     EARLIER-SET CHUNK DATE TO EFFECTIVE          
*                                          DATE                                 
GETDATX  L     RE,SAVERE                                                        
         CR    RE,RE                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE DEMOS FOR A CHUNK ENTRY                                
*                                                                               
GETDEM   ST    RE,SAVERE                                                        
         SR    R0,R0                                                            
         ICM   R0,1,NDEMOS                                                      
         BZR   RE                                                               
         MVC   SCDBFACT+2(2),SPTFACT                                            
         LA    R1,SCDEMOS          R1=A(CHUNK DEMOS)                            
         LA    RE,SPTDEMOS         RE=A(SPTTAB DEMOS)                           
*                                                                               
GETDEM2  MVC   0(4,R1),0(RE)                                                    
         LA    R1,8(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,GETDEM2                                                       
*                                                                               
         TM    SBEFLAG2,SBEBOOK    EXTRACT BOOK?                                
         BZ    GETDEMX                                                          
         MVC   SCACTBK,SPTBOOK                                                  
         MVC   SCBKTYP,SPTBKTYP                                                 
*                                                                               
GETDEMX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUBROUTINE TO PUT AFFIDAVIT DETAILS IN CHUNK IF REQUIRED                      
*                                                                               
GETAFD   ST    RE,SAVERE                                                        
         CLI   SBEBYDT,YES                                                      
         BNE   *+10                                                             
         MVC   SCADATE,SPTADATE                                                 
         CLI   SBEBYDT,C'C'        JUST HAVE COLUMN FILTERS?                    
         BNE   *+18                NO                                           
         OC    SPTADATE,SPTADATE   YES - DO WE HAVE AN AFFID DATE?              
         BZ    *+8                 NO                                           
         MVI   SCADATE+1,1         YES - GROUP THEM ALL UNDER 1                 
         CLI   SBEBYTM,YES                                                      
         BNE   *+14                                                             
         MVC   SCATIME,SPTATIME                                                 
         NI    SCATIME,X'0F'       GET RID OF STATUS BITS                       
         CLI   SBECS,C'R'                                                       
         BNE   *+10                                                             
         MVC   SCRSVPS+2(2),SPTRSVPS                                            
         CLI   SBEBYDPT,YES                                                     
         BNE   *+10                                                             
         MVC   SCRSVPDP,SPTDPT                                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
* SUB-ROUTINE TO SLOT THE DOLLARS INTO CHUNK ENTRY                              
* AT ENTRY, R1=1 FOR FIRST PRODUCT, 2=SECOND PRODUCT, 3=SUM OF TWO              
* GROSS(12) AND GROSS2(12) HAVE BEEN SET BY CALLER                              
* FOR CANADIAN GST, XGSTAMT AND XGSTAMT2 SET BY CALLER                          
* FOR CANADIAN PST, XPSTAMT AND XPSTAMT2 SET BY CALLER                          
*                                                                               
GETDOL   ST    RE,SAVERE                                                        
         STC   R1,BYTE                                                          
         LH    RE,SPTSPOTS         SPOTS COULD BE NEGATIVE                      
         ST    RE,SCSPOTS                                                       
         CLI   BYTE,3              TEST FOR BOTH PRODUCTS                       
         BE    GETDOL2             YES                                          
         LA    RE,GROSS                                                         
         LA    RF,XGSTAMT                                                       
         CLI   BYTE,1                                                           
         BE    *+12                                                             
         LA    RE,GROSS2                                                        
         LA    RF,XGSTAMT2                                                      
         MVC   SCGROSS,0(RE)                                                    
         MVC   SCNET,4(RE)                                                      
         MVC   SCTAX,8(RE)                                                      
         MVC   SCGSTI,0(RF)                                                     
         MVC   SCPSTI,XPSTAMT                                                   
         CLI   BYTE,1                                                           
         BE    *+10                                                             
         MVC   SCPSTI,XPSTAMT2                                                  
         B     GETDOL4                                                          
*                                                                               
GETDOL2  LM    RE,R0,GROSS2        GET GROSS,NET,TAX                            
         A     RE,GROSS            GROSS1+GROSS2                                
         A     RF,NET              NET1+NET2                                    
         A     R0,TAX              TAX1+TAX2                                    
         ST    RE,SCGROSS                                                       
         ST    RF,SCNET                                                         
         ST    R0,SCTAX                                                         
         L     RE,XGSTAMT2         GST1+GST2                                    
         A     RE,XGSTAMT                                                       
         ST    RE,SCGSTI                                                        
         L     RE,XPSTAMT2         PST1+PST2                                    
         A     RE,XPSTAMT                                                       
         ST    RE,SCPSTI                                                        
*                                                                               
GETDOL4  CLI   SBEPAID,YES                                                      
         BNE   GETDOLX                                                          
         TM    SPTIND,SPTSPAID     TEST FOR ANY PAID SPOTS                      
         BZ    GETDOLX             NO                                           
         LH    RE,SPTPAYS                                                       
         ST    RE,SCPAYSP                                                       
         MVC   SCPAY,SCGROSS                                                    
         MVC   SCPAYN,SCNET                                                     
         MVC   SCPAYTX,SCTAX                                                    
         MVC   SCGSTP,SCGSTI                                                    
         MVC   SCPSTP,SCPSTI                                                    
         OC    SPTPAYTS,SPTPAYTS   TEST FOR PAID TODAY SPOTS                    
         BZ    GETDOLX                                                          
         MVC   SCPAYT,SCGROSS      TEST GROSS/NET PAID TODAY                    
         MVC   SCPAYTN,SCNET                                                    
*                                                                               
GETDOLX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO POST A CHUNK ENTRY INTO CHUNK TABLE                            
* AT ENTRY, CHUNK CONTAINS ENTRY AND SBACHUNK POINTS TO TABLE                   
*                                                                               
POST     NTR1                                                                   
         L     R7,SBACHUNK                                                      
*                                                                               
         TM    SBEFLAG,SBEWIPW     EXTRACT WESTERN PROFIT WITHIN?               
         BZ    POST1                                                            
         GOTO1 AGETPW                                                           
*                                                                               
POST1    LA    RE,CHUNK            RE=A(CHUNK ENTRY)                            
*                                                                               
POST2    OC    SCNEXT,SCNEXT       TEST FOR EOT MARKER                          
         BZ    POST14              YES                                          
         TM    SBEFLAG2,SBE1SPT    SKIP KEY TEST?                               
         BNZ   *+14                 YES                                         
         CLC   SCKEY,SCKEY-SCHUNKD(RE)  TEST FOR SAME CHUNK KEY                 
         BE    POST4                                                            
         L     R7,SCNEXT           NEXT ENTRY                                   
         B     POST2                                                            
*                                                                               
POST4    LA    R0,(SCCSX-SCDATA)/4 SET R0=N'BUCKETS                             
         CLI   SBECS,YES           TEST FOR CHILD SPOT EXTRACT                  
         BE    POST5               YES                                          
         LA    R0,(SCRSVPX-SCDATA)/4                                            
         CLI   SBECS,C'R'          TEST FOR RSVP EXTRACT                        
         BE    POST5               YES                                          
         LA    R0,(SCPAYX-SCDATA)/4                                             
         CLI   SBEPAID,YES         TEST IF EXTRACTING PAID                      
         BE    POST5                                                            
         LA    R0,(SCORDX-SCDATA)/4 DO JUST ORDERED THEN                        
*                                                                               
POST5    LA    R1,SCDATA           R1=A(BUCKETS)                                
         LA    R2,SCDATA-SCHUNKD(RE)  R2=A(CHUNK NUMBERS)                       
*                                                                               
POST6    L     RF,0(R1)                                                         
         A     RF,0(R2)                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,POST6                                                         
*                                                                               
         CLI   SBEGST,0            TEST EXTRACTING GST/PST                      
         BE    POST8                                                            
         TM    SBEGST,SBEGSTI+SBEGSTO+SBEGSTB  GST?                             
         BZ    POST7A                                                           
         LA    R0,(SCGSTX-SCGSTI)/4     YES-SUM GST AMOUNTS                     
         LA    R1,SCGSTI                                                        
         LA    R2,SCGSTI-SCHUNKD(RE)                                            
*                                                                               
POST7    L     RF,0(R1)                                                         
         A     RF,0(R2)                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,POST7                                                         
*                                                                               
POST7A   TM    SBEGST,SBEPSTI+SBEPSTO+SBEPSTB  PST?                             
         BZ    POST8                                                            
         LA    R0,(SCPSTX-SCPSTI)/4     YES-SUM PST AMOUNTS                     
         LA    R1,SCPSTI                                                        
         LA    R2,SCPSTI-SCHUNKD(RE)                                            
*                                                                               
POST7B   L     RF,0(R1)                                                         
         A     RF,0(R2)                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,POST7B                                                        
*                                                                               
POST8    DS    0H                                                               
         TM    SBEFLAG,SBEWIPW     TEST EXTRACTING WESTERN PW                   
         BZ    POST9                                                            
         LA    R0,(SCPWX-SCPWGRS)/4    YES-SUM PW AMOUNTS                       
         LA    R1,SCPWGRS                                                       
         LA    R2,SCPWGRS-SCHUNKD(RE)                                           
*                                                                               
POST8A   L     RF,0(R1)                                                         
         A     RF,0(R2)                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,POST8A                                                        
*                                                                               
POST9    CLI   SBEDEMTY,0          TEST ACCOUNTING ONLY                         
         BE    POSTX               YES-ALL DONE                                 
         L     RF,SCDBFACT                                                      
         A     RF,SCDBFACT-SCHUNKD(RE)                                          
         ST    RF,SCDBFACT         UPDATE CUMULATIVE DBFACTOR                   
*                                                                               
         LLC   R0,NDEMOS           R0=COUNTER                                   
         LA    R1,SCDEMOS          R1=BUCKETS                                   
         LA    R2,SCDEMOS-SCHUNKD(RE)                                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* FOR SQAD, IF WE ENCOUNTER A BUYLINE WITH SOME SPOTS AS MISSED OR    *         
* MAKEGOOD AND SOME REGULAR THEN WE HAVE TO CALCULATE THE MDEGOOD/    *         
* MISSED SPOT AS THE CURRENT SCDEMOS (OFF THE CHUNK) OTHERWISE, THE   *         
* CALCULATION WILL BE OFF.  CODE WAS PUT TO SUPRESS SETTING SPTDEMOS  *         
* FOR SQAD IF THE SPOT WAS MADEGOOD/MISSED.  THIS CODE IS TO CALCULATE*         
* THE SQAD NUMBER CORRECTLY IN THE CASE OF BUYLINES WITH BOTH         *         
* MADEGOOD/MISSED SPOTS AND REGULAR SPOTS.                            *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
POST10   LM    RE,RF,0(R1)                                                      
         CLI   SBEDEMTY,C'0'       SQAD?                                        
         BL    POST10A             NO                                           
         OC    0(L'SCDEMOS,R2),0(R2)  ANY DEMOS?                                
         BZ    POST12                 NO, LEAVE SCDEMOS AS IT WAS               
         OC    SCDEMOS,SCDEMOS        ANY PREVIOUS DEMO?                        
         BNZ   POST10A                YES                                       
         MVC   SCDEMOS,0(R2)          NO, CALCULATE DEMO USING CURRENT          
         LM    RE,RF,0(R1)                                                      
*                                                                               
POST10A  A     RE,0(R2)            ADD DEMO                                     
         A     RF,4(R2)            ADD EQUIVALENCED DEMO                        
         CLI   SBEDEMTY,C'0'       NO WEIGHTING BY SPOTS FOR SQAD               
         BNL   *+12                                                             
         CLI   SBENOWGT,YES        TEST OPTION TO NOT WEIGHT DEMOS              
         BNE   POST12                                                           
* SBENOWGT IS NEVER SET! AKAT 02/17/05                                          
         LA    RE,1(RE)            YES-GET AVERAGES                             
         SRL   RE,1                                                             
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
*                                                                               
POST12   STM   RE,RF,0(R1)                                                      
         LA    R1,8(R1)                                                         
         LA    R2,8(R2)                                                         
         BCT   R0,POST10                                                        
         B     POSTX                                                            
*                                                                               
POST14   DS    0H                                                               
         TM    SBEFLAG,SBEA2COS    AFTER SECOND COST CALL?                      
         BZ    POST15                                                           
         MVC   WORK(8),SCGROSS2    SAVE OFF WHAT'S THERE                        
         CLI   SBECS,C'Y'          CHILD SPOT?                                  
         BNE   *+10                                                             
         MVC   WORK(4),SCGSTI      SAVE PE/GVT VALUE (SEE WRI01)                
*                                                                               
* TEST FOR POSSIBLE OVERFLOW                                                    
POST15   TM    SBEFLAG7,SBE7APXG   AFTER PROGRAM EXCHANGE CALL?                 
         BZ    *+10                NO                                           
         MVC   WORK+8(4),SCNETPXG  SAVE NET PROGRAM EXCHANGE VALUE              
         TM    SBEFLAG9,SBE9GPC2   HAVE COST2 GST/PST?                          
         BZ    *+16                NO                                           
         MVC   WORK+8(4),SCGSTOC2  SAVE OFF COST2 GST (TRUMPS SCNETPXG)         
         MVC   WORK+12(4),SCPSTOC2 SAVE OFF COST2 PST                           
         OC    SBACHNKX,SBACHNKX                                                
         BZ    POST16                                                           
         CLC   SBACHNKX,XFF        ALREADY OVERFLOW?                            
         BE    POSTX                                                            
         LR    RE,R7               A(CHUNK)                                     
         A     RE,LCHUNK            + L'CHUNK                                   
         C     RE,SBACHNKX         NEW CHUNK AREA                               
         BNH   POST16                                                           
         MVC   SBACHNKX,XFF                                                     
         B     POSTX                                                            
*                                                                               
POST16   LR    RE,R7               RE=MOVE 'TO' ADDRESS                         
         L     RF,LCHUNK           RF=L'DATA TO MOVE                            
         LA    R0,CHUNK            R0=MOVE 'FROM' ADDRESS                       
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE IN NEW CHUNK                            
         ST    RE,SCNEXT           RE AFTER MOVE=A(NEXT CHUNK)                  
         XC    0(4,RE),0(RE)       SET EOT MARKER                               
         TM    SBEFLAG7,SBE7APXG   AFTER PROGRAM EXCHANGE CALL?                 
         BZ    *+10                NO                                           
         MVC   SCNETPXG,WORK+8     YES - RESTORE PROG XCHG VALUE                
         TM    SBEFLAG,SBEA2COS    AFTER SECOND COST CALL?                      
         BZ    POSTX                                                            
         CLI   SBECS,C'Y'          CHILD SPOT?                                  
         BE    POST17                                                           
         MVC   SCGROSS2(8),WORK    PUT BACK WHAT WAS THERE                      
         TM    SBEFLAG9,SBE9GPC2   HAVE COST2 GST/PST?                          
         BZ    *+16                NO                                           
         MVC   SCGSTOC2,WORK+8     RESTORE COST2 GST (TRUMPS SCNETPXG)          
         MVC   SCPSTOC2,WORK+12    RESTORE COST2 PST                            
         B     POSTX                                                            
POST17   MVC   SCGSTI,WORK         TRADE GROSS VALUE                            
*                                                                               
POSTX    B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO ESTABLISH DEMO LIST                                            
*                                                                               
DEML     ST    RE,SAVERE                                                        
         OC    SBEDEMOS,SBEDEMOS   TEST OVERRIDE DEMO LIST                      
         BZ    DEML1                                                            
         XC    DEMOS,DEMOS         YES                                          
         MVC   DEMOS(L'SBEDEMOS),SBEDEMOS                                       
         XC    WGTLST,WGTLST                                                    
         B     DEML8                                                            
*                                                                               
DEML1    OC    SBAESTTB,SBAESTTB                                                
         BZ    DEML2                                                            
         OC    SBAESTBF,SBAESTBF                                                
         BNZ   DEML4                                                            
*                                                                               
DEML2    MVC   DEMOS,SBPDEMOS                                                   
         MVC   WGTLST,SBPWTLST                                                  
         B     DEML8                                                            
*                                                                               
DEML4    LA    R1,BUYKPRD                                                       
         CLI   BUYKPRD,X'FF'                                                    
         BNE   DEML6                                                            
         CLI   SBQBPRD,0                                                        
         BNE   DEML5                                                            
         CLC   SBQPRD,=C'ALL'      TEST POL BUY AND PRD=ALL REQUEST             
         BNE   DEML6                                                            
         BAS   RE,DEMLPOL          YES-GET DEMOS FOR ALL PRDS IN BUY            
         B     DEML8                                                            
*                                                                               
DEML5    LA    R1,SBQBPRD                                                       
*                                                                               
DEML6    GOTO1 AGETDEMS                                                         
         BE    DEMLX                                                            
         DC    H'0'                                                             
*                                                                               
DEML8    LA    R0,MAXDEMS          INSERT EOT AFTER DEMO LIST                   
         LA    RE,DEMOS                                                         
         CLI   1(RE),0             TEST FOR EOL                                 
         BE    *+12                NO                                           
         LA    RE,3(RE)                                                         
         BCT   R0,*-12                                                          
         MVI   0(RE),X'FF'                                                      
*                                                                               
DEMLX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO ESTABLISH DEMOS FOR ALL PRODUCTS FOR A POL BUY RECORD,             
* FOR A PRODUCT ALL REQUEST                                                     
*                                                                               
DEMLPOL  NTR1                                                                   
         XC    PRDLST,PRDLST                                                    
         XC    POLDEMOS,POLDEMOS                                                
         XC    POLWGTLS,POLWGTLS                                                
         LA    R6,BDELEM           FIND BUY ELEMENTS                            
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
*                                                                               
DPOL1    BRAS  RE,NEXTEL                                                        
         BNE   DPOL6                                                            
         USING REGELEM,R6                                                       
         TM    RSTATUS,X'04'       TEST HIATUS SPOT                             
         BO    DPOL1               YES-SKIP                                     
         TM    RSTATUS,X'C0'       TEST MISSED/MAKE-GOOD                        
         BNZ   DPOL1               YES-NO DEMOS REQUIRED                        
         LA    RE,=X'FF'                                                        
         CLI   RLEN,10             TEST ALLOCATED                               
         BNH   DPOL3               NO-USE POL DEMOS                             
         LA    R0,2                                                             
         LA    RE,RPPRD            BUILD LIST OF PRODUCTS                       
*                                                                               
DPOL2    CLC   SBQPGRF,=C'   '     TEST PRODUCT GROUP FILTER                    
         BNH   DPOL3                                                            
         ICM   R1,15,SBAPRDBF      YES--                                        
         BZ    DPOL3                                                            
         LLC   RF,0(RE)            INDEX INTO PRODUCT BUFFER                    
         BCTR  RF,0                                                             
         MHI   RF,PRDBUFFL                                                      
         AR    R1,RF                                                            
         USING PRDBUFFD,R1                                                      
         OC    PBGROUP,PBGROUP     TEST PRODUCT IN PRODUCT GROUP                
         BZ    DPOL5               NO                                           
         DROP  R1                                                               
*                                                                               
DPOL3    LA    R1,PRDLST                                                        
*                                                                               
DPOL4    CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),0(RE)                                                    
         B     DPOL5                                                            
         CLC   0(1,RE),0(R1)                                                    
         BE    DPOL5                                                            
         LA    R1,1(R1)                                                         
         B     DPOL4                                                            
*                                                                               
DPOL5    CLI   RLEN,14             TEST PIGGYBACK BUY                           
         BNH   DPOL1                                                            
         CLI   SBESPLIT,NO         YES-TEST SPLITTING PIGGIES                   
         BE    DPOL1                                                            
         LA    RE,RPPRD+4          YES-ADD 2ND PRODUCT TO LIST                  
         BCT   R0,DPOL2                                                         
         B     DPOL1                                                            
*                                                                               
DPOL6    OC    PRDLST,PRDLST       TEST ANY PRODUCTS                            
         BZ    DPOL20                                                           
         LA    R1,PRDLST           YES-GET THEIR DEMOS                          
*                                                                               
DPOL8    CLI   0(R1),0                                                          
         BE    DPOL20                                                           
         GOTO1 AGETDEMS                                                         
         BNE   DPOL16                                                           
         SR    R0,R0               R0=N'REQUESTED DEMOS                         
         ICM   R0,1,SBENDEM                                                     
         BNZ   *+8                                                              
         LA    R0,4                MOVE DEMOS TO POL DEMO LIST                  
         LA    R2,DEMOS                                                         
         LA    R3,WGTLST                                                        
*                                                                               
DPOL10   CLI   0(R2),X'FF'                                                      
         BE    DPOL16                                                           
         LA    R5,POLDEMOS                                                      
         LA    RE,POLWGTLS                                                      
         LA    RF,MAXDEMS                                                       
*                                                                               
DPOL12   CLI   1(R5),0                                                          
         BNE   DPOL13                                                           
         MVC   0(3,R5),0(R2)       DEMO                                         
         MVC   0(1,RE),0(R3)       WEIGHT                                       
         B     DPOL14                                                           
*                                                                               
DPOL13   CLC   0(3,R5),0(R2)                                                    
         BE    DPOL14                                                           
         LA    R5,3(R5)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,DPOL12                                                        
         DC    H'0'                                                             
*                                                                               
DPOL14   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,1(R3)                                                         
         BCT   R0,DPOL10                                                        
*                                                                               
DPOL16   LA    R1,1(R1)            NEXT PRODUCT                                 
         B     DPOL8                                                            
*                                                                               
DPOL20   MVC   DEMOS,POLDEMOS      PASS BACK DEMOS                              
         MVC   WGTLST,POLWGTLS                                                  
*                                                                               
DPOLX    B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO PUT THE PURCHASED DEMOS INTO THE APPROPRIATE                   
* SPTTAB ENTRIES                                                                
*                                                                               
PUR      ST    RE,SAVERE                                                        
         LM    R2,R4,ASPTTAB       R2=A(SPTTAB),R3=N'ENTRIES,R4=L'ENTRY         
         USING SPTTABD,R2                                                       
         LLC   R5,NDEMOS           R5=N'DEMOS                                   
*                                                                               
PUR2     OC    SPTSPOTS,SPTSPOTS   TEST FOR ZERO SPOTS                          
         BZ    PUR6                NO DEMOS                                     
         TM    SPTIND,SPTNODEM     TEST FOR NO DEMO LOOKUP                      
         BO    PUR6                                                             
         CLI   SBEDEMTY,C'U'       UPGRADE LOOKUP?                              
         BE    *+14                YES - GET PURCHASED DEMOS FIRST!             
         OC    SPTACTBK,SPTACTBK   TEST IF ACTUAL BOOK SET                      
         BNZ   PUR6                YES-DO RE-RATE OR AFFID                      
         MVC   SPTFACT,SPTSPOTS                                                 
         LR    R1,R5               REFRESH NUMBER OF DEMOS                      
         LA    RE,SPTDEMOS         RE=A(SPOT TABLE DEMOS)                       
         LA    RF,PURVALSA         RF=A(ADJUSTED PURCHASED DEMOS)               
         CLI   SBEDEMTY,C'P'       TEST ONLY PURCHASED DEMOS REQUESTED          
         BE    PUR3                                                             
         CLI   SBEDEMTY,C'0'       SQAD                                         
         BNL   PUR3                                                             
         CLI   SBESVI,X'FF'        NO-TEST NO SVI ADJUSTMENTS                   
         BNE   PUR3                                                             
         LA    RF,PURVALSU         YES-THEN USE UNADJUSTED PURCH DENOS          
*                                                                               
PUR3     MVC   0(4,RE),0(RF)       GET PURCHASED VALUE                          
***      NI    0(RE),X'FF'-X'80'   TURN OFF MANUAL OVERRIDE BIT                 
         NI    0(RE),X'FF'-X'C0'   TURN OFF MANUAL OVERRIDE BIT & 2 DEC         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,PUR3                                                          
*                                                                               
PUR4     CLC   SPTSPOTS,=H'1'      TEST FOR ONE SPOT                            
         BE    PUR6                YES-NO NEED TO WEIGHT DEMOS                  
         CLI   SBENOWGT,YES        TEST OPTION TO NOT WEIGHT DEMOS              
         BE    PUR6                                                             
*                                                                               
         LR    R1,R5               R1=N'DEMOS                                   
         LH    R0,SPTSPOTS         R0=N'SPOTS                                   
         LA    R6,SPTDEMOS         R6=A(DEMOS)                                  
*                                                                               
PUR5     L     RF,0(R6)            GET DEMO                                     
         MR    RE,R0               WEIGHT IT BY NUMBER OF SPOTS                 
         ST    RF,0(R6)                                                         
         LA    R6,4(R6)            NEXT DEMO                                    
         BCT   R1,PUR5                                                          
*                                                                               
PUR6     LA    R2,0(R4,R2)         NEXT ENTRY                                   
         BCT   R3,PUR2                                                          
*                                                                               
PURX     L     R4,SBAIO1           RESTORE A(BUY RECORD)                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET BUY RECORD POST BUY DEMO OVERRIDES                         
*                                                                               
PBDEMGET ST    RE,SAVERE                                                        
         MVI   PBDEMO,NO                                                        
         XC    PDEMVALS,PDEMVALS                                                
         CLI   SBELKUPA,C'Y'       LKUP=ALL?                                    
         BE    PDGETX              YES - DON'T HONOR POST BUY DEMOS             
         SR    R0,R0                                                            
         SR    R3,R3                                                            
         SR    R5,R5                                                            
         LA    R6,BDELEM                                                        
*                                                                               
PDGET2   CLI   0(R6),0                                                          
         BE    PDGET12                                                          
         OC    SBEMKT,SBEMKT       TEST SPILL MARKET                            
         BZ    *+14                                                             
         CLC   SBEMKT,SBBMKT                                                    
         BNE   PDGET4                                                           
         CLI   0(R6),2             NO-TEST ORIGINAL MARKET DEMO ELE             
         BE    PDGET6                                                           
         CLI   0(R6),X'22'            OR ORIG MKT POST BUY DEMO ELE             
         BE    PDGET8                                                           
         B     PDGET10                                                          
*                                                                               
PDGET4   CLI   0(R6),3             SPILL                                        
         BNE   *+18                                                             
         CLC   SBEMKT,NDPROG-NDELEM(R6)  COMPARE AGENCY MARKET NUMBER           
         BE    PDGET6                                                           
         B     PDGET10                                                          
         CLI   0(R6),X'23'                                                      
         BNE   PDGET10                                                          
         CLC   SBEMKT,SDAGYMKT-SDELEM(R6)                                       
         BE    PDGET8                                                           
         B     PDGET10                                                          
*                                                                               
PDGET6   LR    R3,R6               R3=A(DEMO ELEMENT)                           
         B     PDGET10                                                          
PDGET8   LR    R5,R6               R5=A(POST BUY DEMO ELEMENT)                  
*                                                                               
PDGET10  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PDGET2                                                           
*                                                                               
PDGET12  LTR   R5,R5               TEST POST BUY DEMO ELE FOUND                 
         BZ    PDGETX                                                           
         LTR   R3,R3               YES-CHECK DEMO ELEMENT FOUND                 
         BZ    PDGETX                                                           
         LLC   R4,1(R3)                                                         
         SHI   R4,NDEMNO-NDELEM                                                 
         SRL   R4,3                   R4=N'DEMOS IN DEMO ELEMENT                
         LTR   R4,R4                                                            
         BZ    PDGETX                                                           
         LA    R3,NDEMNO-NDELEM(R3)   R3=A(DEMOS IN DEMO ELEMENT)               
         LA    R6,3                                                             
         LLC   R7,1(R5)            R6,R7 FOR POST BUY DEMO ELE BXLE             
         AR    R7,R5                                                            
         BCTR  R7,0                                                             
         LA    R1,PDEMO-PDELEM(R5)                                              
         CLI   0(R5),X'23'                                                      
         BNE   *+8                                                              
         LA    R1,SDEMO-SDELEM(R5)                                              
         LR    R5,R1               R5=A(DEMO VALS IN POST BUY DEMO ELE)         
         STM   R3,R7,WORK                                                       
         LA    R1,DEMOS            R1=A(DEMO LIST)                              
         LA    R2,PDEMVALS         R2=A(OUTPUT AREA)                            
         LLC   R0,NDEMOS           R0=N'DEMOS                                   
*                                                                               
PDGET14  CLI   0(R1),X'FF'         TEST END OF DEMO LIST                        
         BE    PDGETX                                                           
         LM    R3,R7,WORK                                                       
*                                                                               
PDGET16  CLC   0(3,R3),0(R1)       MATCH THE DEMO                               
         BNE   PDGET18                                                          
         TM    0(R5),X'80'         TEST POST BUY DEMO OVERRIDE                  
         BZ    PDGET20                                                          
         MVC   ADJDEMO(1),0(R5)                                                 
         NI    ADJDEMO,X'C0'       ONLY LEAVE OVRD+2DEC BITS                    
         MVC   ADJDEMO+1(3),0(R5)                                               
         NI    ADJDEMO+1,X'3F'     STRIP OVRD+2DEC BITS                         
*                                                                               
         ST    R1,ADEMLIST         A(CURRENT DEMO IN DEMO LIST)                 
         XC    A50ELEM,A50ELEM     CLEAR A(X'50') ELEMENT                       
         BRAS  RE,ADJPREC                                                       
         MVC   0(4,R2),ADJDEMO                                                  
**NOP**  OI    0(R2),X'80'         YES-MOVE IN POST DEMO VALUE                  
**NOP**  MVC   1(3,R2),0(R5)                                                    
**NOP**  NI    1(R2),X'7F'                                                      
         MVI   PBDEMO,YES          INDICATE POST BUY DEMOS EXIST                
         B     PDGET20                                                          
*                                                                               
PDGET18  LA    R3,8(R3)            NEXT DEMO IN DEMO ELE                        
         BCT   R4,*+8                                                           
         B     PDGET20                                                          
         BXLE  R5,R6,PDGET16       NEXT DEMO VAL IN POST BUY DEMO ELE           
*                                                                               
PDGET20  LA    R1,3(R1)            NEXT LOOKUP DEMO                             
         LA    R2,4(R2)                                                         
         BCT   R0,PDGET14                                                       
*                                                                               
PDGETX   L     R4,SBAIO1           RESTORE A(BUY RECORD)                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PERFORM RE-RATE LOOKUP                                         
*                                                                               
RERATE   NTR1                                                                   
         LA    R5,ACTBOOKS         R5=A(ACTUAL BOOK LIST)                       
         ZICM  R6,NACTS,1          R6=N'ACTUAL BOOKS                            
         BNZ   RERATE2                                                          
         CLI   PBDEMO,YES          IF POST BUY DEMS, LOOK UP...                 
         BNE   RERATE1                                                          
         LA    R6,1                                                             
         OC    0(2,R5),0(R5)                                                    
         BNZ   *+10                                                             
         MVC   0(2,R5),=X'0100'                                                 
         B     RERATE2                                                          
*                                                                               
RERATE1  TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BZ    RERATEX             NO - DO NOT CALL SPGETDEMF                   
         LA    R6,1                SET TO 1 BOOK                                
*                                                                               
RERATE2  MVI   BYTE,0                                                           
         XR    RF,RF               CLEAR AS FLAG FOR RERATE7                    
* FIND EARLIEST SPOT DATE THIS BOOK NOT YET LOOKED UP                           
*                                                                               
         LM    R2,R4,ASPTTAB       R2=A(SPTTAB),R3=N'NTRYS,R4=L'NTRY            
         USING SPTTABD,R2                                                       
         LLC   R7,NDEMOS           R7=N'DEMOS                                   
*                                                                               
RERATE6  OC    AORBEL,AORBEL       TEST ORBIT                                   
         BZ    RERATE7                                                          
         BAS   RE,ORBLOOK          YES-LOOKUP FOR ORBIT                         
         B     RERATE8                                                          
*                                                                               
RERATE7  LTR   RF,RF               NOT FIRST TIME THROUGH                       
         BZ    *+14                                                             
         CLC   SPTRDATE,SPTRDATE-SPTTABD(RF)   SAME DATE AS LAST SPOT?          
         BE    RERATE8                          YES - NO DEMO LOOKUP            
         L     R1,SBAIO1                                                        
         LA    RE,BDDAY-BUYREC(R1)                                              
         ST    RE,DMCB+4                                                        
         LA    RE,BDTIMST-BUYREC(R1)                                            
         ST    RE,DMCB+8                                                        
         ST    R2,DMCB+12          PASS A(SPTTAB ENTRY)                         
         GOTO1 ALOOK,DMCB,(R5)                                                  
         BE    *+8                                                              
         MVI   BYTE,1              BYTE=1 TO EXCLUDE SPOTS FOR THIS BK          
*                                                                               
RERATE8  CLC   SPTACTBK,0(R5)      TEST FOR THIS ACTUAL BOOK                    
         BNE   RERATE14            NO                                           
*                                                                               
         CLI   BYTE,1              TEST TO EXCLUDE                              
         BNE   *+12                                                             
         OI    SPTIND,SPTDUMMY     YES                                          
         B     RERATE14                                                         
*                                                                               
         CLI   SPECIAL,YES         TEST FOR SPECIAL                             
         BNE   *+12                NO                                           
         BAS   RE,RESPEC                                                        
         B     RERATE10                                                         
*                                                                               
         CLI   SBEDEMTY,C'A'       TEST AFFID LOOKUP                            
         BNE   *+14                                                             
         OC    SPTADATE,SPTADATE   YES-TEST AFFID PRESENT                       
         BNZ   RERATE14            YES-NOT A RE-RATE LOOKUP                     
         MVC   SPTFACT,DEMFACT                                                  
         LR    R1,R7                                                            
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPTDEMOS(0),DEMVALS                                              
*                                                                               
RERATE10 CLC   SPTSPOTS,=H'1'      TEST FOR ONE SPOT                            
         BE    RERATE14            YES-NO NEED TO WEIGHT DEMOS                  
         CLI   SBENOWGT,YES        TEST OPTION TO NOT WEIGHT DEMOS              
         BE    RERATE14                                                         
         LR    R0,R7               R0=N'DEMOS                                   
         LA    R1,SPTDEMOS         R1=A(DEMO VALUE)                             
*                                                                               
RERATE12 L     RE,0(R1)            GET DEMO                                     
         MH    RE,SPTSPOTS         * N'SPOTS                                    
         ST    RE,0(R1)            = WEIGHTED VALUE                             
         LA    R1,4(R1)            NEXT VALUE                                   
         BCT   R0,RERATE12                                                      
*                                                                               
RERATE14 LR    RF,R2               SAVE CURRENT ENTRY ADDR FOR RERATE7          
         LA    R2,0(R4,R2)         NEXT SPTTAB ENTRY                            
         LA    R1,RERATE8                                                       
         CLI   SBD0PROF+10,C'Y'              LPMWK=Y OPTION                     
         BE    *+8                           NEED TO LOOK AT DATES              
         CLI   SBD0PROF+11,C'M'              OVERNIGHTS NEED TO LOOK            
         BE    *+8                           DATES. BK=ACT NOT REQUIRED         
         CLI   SBD0PROF+11,C'Y'              OVERNIGHTS NEED TO LOOK            
         BE    *+12                          DATES. BK=ACT NOT REQUIRED         
         TM    SBEFLAG2,SBEWTP     WEEKLY OPTION?                               
         BZ    *+8                                                              
         LA    R1,RERATE6                                                       
         BCTR  R3,R1                                                            
*                                                                               
         LA    R5,2(R5)            NEXT BOOK                                    
         BCT   R6,RERATE2                                                       
         CLC   ACTBOOKS(2),=X'0100'  IF WE FUDGED UP TOP,                       
         BNE   *+10                                                             
         XC    ACTBOOKS(2),ACTBOOKS  PUT IT BACK                                
*                                                                               
RERATEX  B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO DEAL WITH SPECIALS ON A RERATE                                 
*                                                                               
* CALLED BY RERATE, AT ENTRY R2=ASPTTAB, R7=N'DEMOS                             
*                                                                               
RESPEC   NTR1                                                                   
*                                                                               
         MVC   SPTFACT,SPTSPOTS                                                 
         LR    R0,R7               R0=N'DEMOS                                   
         LA    R1,SPTDEMOS         R1=A(SPOT TABLE DEMOS)                       
         LA    R2,PDEMVALS         R2=A(OVERRIDE RERATE DEMOS)                  
         LA    RE,PURVALSA         RE=A(PURCHASED DEMOS)                        
         CLI   SBESVI,X'FF'                                                     
         BNE   *+8                                                              
         LA    RE,PURVALSU                                                      
         LA    RF,DEMVALS          RF=A(RERATE DEMOS)                           
*                                                                               
RESPEC2  MVC   0(4,R1),0(RE)       MOVE IN PURCHASED DEMO                       
         NI    0(R1),X'FF'-X'80'   TURN OFF MANUAL OVERRIDE BIT                 
         TM    0(R2),X'80'         TEST OVERRIDE RERATE DEMO                    
         BO    *+12                YES                                          
         TM    0(RE),X'80'         NO-TEST PURCH. VAL=MANUAL OVERRIDE           
         BO    RESPEC4             YES-TAKE IT                                  
         MVC   0(4,R1),0(RF)       USE THE RERATE VALUE INSTEAD                 
*                                                                               
RESPEC4  LA    R1,4(R1)            NEXT DEMO IN SPOT TABLE                      
         LA    R2,4(R2)            NEXT OVERRIDE RERATE VALUE                   
         LA    RE,4(RE)            NEXT PURCHASED VALUE                         
         LA    RF,4(RF)            NEXT RERATE VALUE                            
         BCT   R0,RESPEC2                                                       
*                                                                               
RESPECX  B     EXIT                                                             
* ROUTINE TO LOOK UP DEMOS FOR ORBIT BUYLINE                                    
* AT ENTRY, AORBEL=A(FIRST ORBIT ELEMENT)                                       
*           R2=A(SPTTAB ENTRY)                                                  
*           R5=A(ACTUAL BOOK)                                                   
*                                                                               
ORBLOOK  NTR1                                                                   
         L     R3,AORBEL                                                        
         USING ORBELEM,R3                                                       
         LA    R1,ORBDEMS          CLEAR ORBIT DEMO HOLD AREA                   
         LA    RE,MAXDEMS                                                       
         XC    0(4,R1),0(R1)                                                    
         LA    R1,4(R1)                                                         
         BCT   RE,*-10                                                          
         SR    R4,R4               R4=ORBIT COUNT                               
         LLC   R7,NDEMOS           R7=N'DEMOS                                   
*                                                                               
ORBL1    LLC   R6,1(R3)                                                         
         SH    R6,=Y(ORBDAY-ORBELEM)                                            
         SRL   R6,4                R6=N'ORBITS IN ELEMENT                       
         BNP   ORBLX                                                            
         ST    R3,FULL             SAVE FULL=A(ORBIT ELEMENT)                   
*                                                                               
ORBL2    LA    R4,1(R4)            INCREMENT ORBIT COUNT                        
         GOTO1 ALOOK,DMCB,(R5),ORBDAY,ORBTIME,(R2)                              
         BE    *+8                                                              
         MVI   BYTE,1                                                           
         LA    R1,ORBDEMS          ADD DEMO VALUES INTO ORBIT DEMO              
         LA    RE,DEMVALS          HOLD AREA                                    
         LR    R0,R7                                                            
*                                                                               
ORBL6    L     RF,0(R1)                                                         
         A     RF,0(RE)                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,ORBL6                                                         
*                                                                               
         LA    R3,16(R3)           NEXT ORBIT IN ELEMENT                        
         BCT   R6,ORBL2                                                         
*                                                                               
         L     R3,FULL             NEXT ORBIT ELEMENT                           
         SR    R0,R0                                                            
ORBL8    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    ORBL10                                                           
         CLI   0(R3),X'67'                                                      
         BE    ORBL1                                                            
         B     ORBL8                                                            
*                                                                               
ORBL10   LR    R0,R7               AVERAGE THE DEMO VALUES                      
         LA    R1,ORBDEMS                                                       
         LA    R3,DEMVALS                                                       
*                                                                               
ORBL12   SR    RE,RE                                                            
         L     RF,0(R1)                                                         
         SLL   RF,1                                                             
         DR    RE,R4                                                            
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
         ST    RF,0(R3)                                                         
         LA    R1,4(R1)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ORBL12                                                        
*                                                                               
ORBLX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PERFORM AFFIDAVIT LOOKUP                                       
*                                                                               
AFFID    NTR1                                                                   
         GOTO1 AAFFINIT                                                         
         LA    R0,AFFIDHK          A(HOOK ROUTINE)                              
         ST    R0,SPLKHOOK                                                      
         USING BUYREC,R4                                                        
*                                                                               
         CLI   CANADA,C'Y'         CANADIAN AGENCY?                             
         BNE   *+10                NO                                           
***      TM    SBEFLAG8,SBE8NPM    USE NEW POSTING METHODOLOGY?                 
***      BZ    *+10                NO                                           
         MVC   SPLKALF,ALPHAMKT    YES - SET ALPHA MARKET                       
*                                                                               
         LA    RF,SBQMED           MEDIA                                        
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BNE   *+8                 NO                                           
         LA    RF,SBMED            YES - CURRENT MEDIA                          
         CLI   0(RF),C'R'                                                       
         BNE   AFFID10                                                          
         OC    ALPHAMKT,ALPHAMKT                                                
         BZ    *+14                                                             
         MVC   SPLKALF,ALPHAMKT                                                 
         B     *+10                                                             
         MVC   SPLKSPL,SBMKNUM                                                  
*                                                                               
         MVI   SPLKBEST,C'P'                                                    
*                                                                               
AFFID10  TM    SBEFLAG2,SBEWTP     WEEKLY OPTION?                               
         BZ    *+8                                                              
         OI    SPLKOPT,SPLKOWTP                                                 
*                                                                               
         CLI   SBD0PROF+11,C'Y'                                                 
         BE    *+8                                                              
         CLI   LPMOV,C'Y'                                                       
         BNE   *+8                                                              
         OI    SPLKOPT2,SPLKOPT2_OPOST          OVERRIDE ON                     
* CHECK OVERNIGHTS OPTION SET TO 'M'                                            
         NI    SPLKOPT2,X'FF'-SPLKOPT2_SMMM                                     
         CLI   SBD0PROF+11,C'M'                 IF OVERNIGHT OPTION             
         BE    *+8                              SET TO M                        
         CLI   LPMOV,C'M'                       SET OPTION TO POST              
         BNE   *+12                             OFF MONTHLY FOR                 
         OI    SPLKOPT2,SPLKOPT2_SMMM           SET METERED MKTS                
         OI    SPLKOPT2,SPLKOPT2_OPOST          AND SET OVN ON                  
*                                                                               
         CLI   LPMOV,C'N'                                                       
         BNE   *+8                                                              
         NI    SPLKOPT2,X'FF'-SPLKOPT2_OPOST    OVERRIDE OFF                    
*                                                                               
         OC    SBEMKT,SBEMKT       TEST IF EXTRACT MARKET SET                   
         BZ    AFFID20             NO-HOME MARKET LOOKUP                        
         CLC   SBEMKT,SBBMKT       TEST SPILL MARKET LOOKUP                     
         BE    AFFID20                                                          
         MVC   SPLKSPL,SBBSPL      YES-SET SPILL MKT RTG SRV NUMBER             
         TM    SBINDS,SBIRSOVR                                                  
         BZ    *+12                                                             
         TM    SBINDS,SBIAGYMK                                                  
         BZ    AFFID20                                                          
         MVC   SPLKUMK,SBEMKT      PASS SPILL AGENCY MARKET NUMBER              
*                                                                               
AFFID20  LA    R0,SB1WPROF                                                      
         ST    R0,SPLKA1W                                                       
         MVC   SPLKASPT,ASPTTAB    A(SPOT TABLE)                                
         MVC   SPLKNSPT,NSPTTAB+2  N'ENTRIES                                    
         MVC   SPLKLSPT,LSPTTAB+3  L'ENTRY                                      
         MVC   SPLKSVI,SBESVI      SVI LOOK-UP OPTION                           
         CLI   SPLKSVI,0           TEST AUTO-SVI REQUESTED                      
         BNE   AFFID60             NO                                           
*                                                                               
         MVC   SPLKAUTF,BDWKIND                                                 
         GOTO1 VDATCON,DMCB,(3,BDSTART),(2,SPLKAUST)                            
         GOTO1 (RF),(R1),(3,BDEND),(2,SPLKAUND)                                 
*                                                                               
AFFID60  ICM   R4,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    AFFID60A            NO                                           
         USING SPLKXTD,R4                                                       
*                                                                               
         MVC   SPXTEOWD,SBESTOWD   ESTIMATE OUT OF WEEK ROTATOR                 
*                                                                               
         TM    SBINDS,SBIWRI       CALLER SPOT WRITER?                          
         BZ    *+8                                                              
         OI    SPXTFLG2,SPXTSPWR                                                
*                                                                               
         TM    CBLFLAG,CFFUS       OVERRIDE TO FUSION?                          
         BZ    *+8                 NO                                           
         OI    SPXTFLAG,SPXTFUS                                                 
*                                                                               
         TM    CBLFLAG,CFNLS       OVERRIDE TO NIELSON?                         
         BZ    *+8                 NO                                           
         OI    SPXTFLAG,SPXTNLS                                                 
*                                                                               
         TM    CBLFLAG,CFNONE      OVERRIDE TO NO CABLE?                        
         BZ    *+8                 NO                                           
         OI    SPXTFLAG,SPXTNONE                                                
*                                                                               
         TM    CBLFLAG,CFLPM       OVERRIDE TO NO CABLE EXCEPT FOR LPM?         
         BZ    *+8                 NO                                           
         OI    SPXTFLAG,SPXTLPM                                                 
*                                                                               
         XC    SPXTHEAD,SPXTHEAD   CLEAR THIS!                                  
*                                                                               
         CLI   SPLKSTA,C'0'        CABLE?                                       
         BL    AFFID60A            NO                                           
         MVC   SPXTHEAD,SPLKSTA    SET THE HEADEND                              
         DROP  R4                                                               
         XC    SPLKSTA,SPLKSTA                                                  
         MVC   SPLKSTA(3),SBCBLNET REPLACE THE STATION WITH THE NETWORK         
*                                                                               
AFFID60A NI    SPLKOPT2,X'FF'-SPLKOPT2_LPOST   LPMWK=N                          
         CLI   SBD0PROF+10,C'Y'          SET FLAG -LPMWK OPTION                 
         BNE   *+8                                                              
         OI    SPLKOPT2,SPLKOPT2_LPOST   LPMWK=Y                                
         TM    SBEFLAG8,SBE8NPM    USE NEW POSTING METHODOLOGY?                 
         BZ    *+8                 NO                                           
         OI    SPLKOPT2,SPLKOPT2_NPM                                            
         TM    FLAGS,LPMBOOK       HAVE LPM BOOK?                               
         BZ    AFFID61             NO                                           
         MVI   SPLKMED,C'W'                                                     
* DONT SET THIS FLAG BECAUSE EVEN IF WE DIDNT ASK FOR LPMWK=Y , OVN=Y           
* COULD TRIGGER THIS SO WE DONT WANT TO CONFUSE DEMF                            
*****    OI    SPLKOPT2,SPLKOPT2_LPOST   LPMWK=Y                                
         TM    FLAGS,LPMBKOV       LPM OVERNIGHT?                               
         BZ    AFFID61             NO                                           
         MVI   SPLKMED,C'O'                                                     
*                                                                               
AFFID61  BRAS  RE,INSPDEM          CHECK DEMO LIST FOR COMSCORE/NIELSON         
*                                                                               
         GOTO1 VSPGETDM,DMCB,(X'FF',SPDEMLK)                                    
*                                                                               
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   AFFIDX              YES - DONE                                   
         CLI   SPECIAL,YES         TEST SPECIAL                                 
         BNE   AFFIDX                                                           
         CLI   SB1WPROF+1,YES      AND NEED WEEKLY DATA FOR SPECIALS            
         BNE   AFFIDX                                                           
         LM    R2,R4,ASPTTAB                                                    
         USING SPTTABD,R2                                                       
AFFID70  TM    SPTIND,SPTNODEM     NO SF DEMOS                                  
         BNZ   AFFID80                                                          
         TM    SPTIND,SPTSPWKY     YES-ARE THERE ANY SPOTS WITHOUT              
         BZ    *+16                    WEEKLY DATA?                             
AFFID80  LA    R2,0(R2,R4)                                                      
         BCT   R3,AFFID70                                                       
         B     AFFIDX                                                           
         LM    R2,R4,ASPTTAB       YES-THEN REPLACE ALL SPOTS WITH              
*                                      WEEKLY DATA WITH BUYER'S                 
AFFID90  TM    SPTIND,SPTSPWKY         OVERRIDES                                
         BZ    AFFID140                                                         
         LA    RE,PURVALSA                                                      
         CLI   SBESVI,X'FF'                                                     
         BNE   *+8                                                              
         LA    RE,PURVALSU                                                      
         LA    RF,PDEMVALS                                                      
         LA    R1,SPTDEMOS                                                      
         LLC   R0,NDEMOS                                                        
*                                                                               
AFFID100 CLI   PBDEMO,YES                                                       
         BNE   AFFID110                                                         
         TM    0(RF),X'80'                                                      
         BZ    AFFID110                                                         
         MVC   0(4,R1),0(RF)                                                    
         NI    0(R1),X'7F'                                                      
         B     AFFID120                                                         
*                                                                               
AFFID110 TM    0(RE),X'80'                                                      
         BZ    AFFID120                                                         
         MVC   0(4,R1),0(RE)                                                    
         NI    0(R1),X'7F'                                                      
*                                                                               
AFFID120 LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         LA    R5,3(R5)                                                         
         BCT   R0,AFFID100                                                      
*                                                                               
AFFID140 LA    R2,0(R2,R4)                                                      
         BCT   R3,AFFID90                                                       
*                                                                               
AFFIDX   B     EXIT                                                             
         SPACE 2                                                                
* HOOK ROUTINE TO PROCESS AFFIDAVIT DEMOS                                       
*                                                                               
AFFIDHK  ST    RE,SAVERE                                                        
*                                                                               
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   AFFIDHKX            YES - DONE                                   
*                                                                               
         L     R2,SPLKAAFD         A(SPOT TABLE ENTRY)                          
         USING SPTTABD,R2                                                       
         LLC   R7,NDEMOS           R7=N'DEMOS                                   
*                                                                               
         TM    SBEFLAG8,SBE8CP2    COMSCORE PASS 2?                             
         BZ    *+10                NO                                           
         MVC   SAVEDEMS,THISDEMS   SAVE DEMOS SINCE THEY MAY CHANGE             
*                                                                               
         TM    SPLKAIND,SPLKAIER   TEST FOR LOOKUP ERROR                        
         BZ    AFDHK12             NO                                           
*                                                                               
         OC    SBEMKT,SBEMKT                                                    
         BZ    *+14                                                             
         CLC   SBEMKT,SBBMKT                                                    
         BNE   AFDHK6                                                           
         OI    FLAGS,AFFIDFL       FLAG COMING FROM AFFID HOOK W/ERROR          
*                                  NO-PERFORM RE-RATE LOOKUP                    
         OC    AORBEL,AORBEL       TEST ORBIT                                   
         BZ    *+16                                                             
         LA    R5,SPTACTBK                                                      
         BAS   RE,ORBLOOK          YES-LOOKUP FOR ORBIT                         
         B     AFDHK1                                                           
         L     R4,SBAIO1                                                        
         USING BUYREC,R4                                                        
         GOTO1 ALOOK,DMCB,SPTACTBK,BDDAY,BDTIMST,(R2)                           
         DROP  R4                                                               
*                                                                               
AFDHK1   NI    FLAGS,X'FF'-AFFIDFL TURN OFF FLAG                                
         CLI   SPECIAL,YES         TEST FOR SPECIAL                             
         BNE   *+12                NO                                           
         BAS   RE,RESPEC                                                        
         B     AFDHK2                                                           
         MVC   SPTFACT,DEMFACT                                                  
         LLC   R1,NDEMOS           MOVE IN THE RERATE DEMOS                     
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPTDEMOS(0),DEMVALS                                              
*                                                                               
AFDHK2   CLC   SPTSPOTS,=H'1'      TEST FOR ONE SPOT                            
         BE    AFDHK30             YES-NO NEED TO WEIGHT DEMOS                  
         CLI   SBENOWGT,YES        TEST OPTION TO NOT WEIGHT DEMOS              
         BE    AFDHK30                                                          
         LR    R0,R7               R0=N'DEMOS                                   
         LA    R1,SPTDEMOS         R1=A(DEMO VALUE)                             
*                                                                               
AFDHK4   L     RE,0(R1)            GET DEMO                                     
         MH    RE,SPTSPOTS         * N'SPOTS                                    
         ST    RE,0(R1)            = WEIGHTED VALUE                             
         LA    R1,4(R1)            NEXT VALUE                                   
         BCT   R0,AFDHK4                                                        
         B     AFDHK30                                                          
*                                  SPILL-                                       
AFDHK6   CLI   SBD0PROF+1,C'N'     TEST OPTION TO RETURN PURCHASED              
         BE    AFDHK8              YES                                          
         CLI   PBDEMO,YES          TEST POST BUY DEMO OVERRIDES                 
         BE    AFDHK14             YES                                          
         OI    SPTIND,SPTDUMMY     NO-EXCLUDE UNREPORTED SPILL SPOTS            
         B     AFDHK30                                                          
*                                                                               
AFDHK8   LR    R0,R7               MOVE IN THE PURCHASED DEMOS                  
         LA    RE,SPTDEMOS                                                      
         LA    RF,PURVALSA                                                      
         CLI   SBESVI,X'FF'                                                     
         BNE   AFDHK10                                                          
         LA    RF,PURVALSU                                                      
AFDHK10  MVC   0(4,RE),0(RF)                                                    
         NI    0(RE),X'7F'                                                      
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,AFDHK10                                                       
         B     AFDHK14                                                          
*                                                                               
*                                  NORMAL RETURN FROM AFFID LOOKUP-             
AFDHK12  L     R1,SPLKDBLK         R1=A(SPGETDEMO DBLOCK)                       
         MVC   SPTFACT,DBDIVSOR-DBLOCK(R1)                                      
*                                                                               
         CLI   SPECIAL,YES         TEST FOR SPECIAL                             
         BNE   *+16                NO                                           
         CLI   SPLKWKLY,YES        TEST WEEKLY DATA FOUND                       
         BNE   AFDHK40             NO-DO NOT USE REGULAR DATA                   
         OI    SPTIND,SPTSPWKY     YES-LEAVE AN INDICATION                      
         LR    R0,R7               R0=COUNTER                                   
         LA    R1,SPTDEMOS         R1=A(OUTPUT)                                 
         LA    RF,THISDEMS         RE=A(INPUT)                                  
         LA    R3,DEMOS                                                         
         LA    R4,PURVALSA         R4 = PURCHASED DEMO VALUES                   
         CLI   SBESVI,X'FF'        HAVE SVI ADJUSTMENTS?                        
         BNE   *+8                 NO                                           
         LA    R4,PURVALSU         R4 = UNADJUSTED PUR DEMO VALUES              
*                                                                               
AFDHK12A CLI   2(R3),0             COMSCORE DEMO?                               
         BNE   AFDHK12B            NO                                           
         TM    SPLKCERR,SPLKCBAD   COMSCORE LOOKUP ERROR?                       
         BNZ   *+14                YES - REPORT PURCHASED DEMOS                 
         CLC   0(4,RF),XFF         NULL COMSCORE VALUE?                         
         BNE   AFDHK12B            NO                                           
*                                                                               
         MVC   0(4,R1),0(R4)       PURCHASED DEMO VALUE                         
         NI    0(R1),X'FF'-X'80'   TURN OFF MANUAL OVERRIDE BIT                 
         B     AFDHK12C            BUMP POINTERS                                
*                                                                               
AFDHK12B MVC   ADJDEMO,0(RF)                                                    
         ST    R3,ADEMLIST         A(CURRENT DEMO IN DEMO LIST)                 
         XC    A50ELEM,A50ELEM     CLEAR A(X'50') ELEMENT                       
         BRAS  RE,ADJPREC                                                       
         MVC   0(4,R1),ADJDEMO                                                  
*                                                                               
AFDHK12C LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         LA    R4,4(R4)            BUMP PURCHASED DEMOS                         
         LA    R3,3(R3)                                                         
         BCT   R0,AFDHK12A                                                      
*                                                                               
AFDHK14  CLI   PBDEMO,YES          TEST POST BUY DEMO OVERRIDES                 
         BNE   AFDHK20                                                          
         LR    R0,R7               YES-MOVE THEM IN                             
         LA    R1,SPTDEMOS                                                      
         LA    RF,PDEMVALS                                                      
*                                                                               
AFDHK16  TM    0(RF),X'80'                                                      
         BZ    AFDHK19                                                          
         MVC   0(4,R1),0(RF)                                                    
         NI    0(R1),X'7F'                                                      
*                                                                               
AFDHK19  LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,AFDHK16                                                       
*                                                                               
AFDHK20  OC    SBEMKT,SBEMKT       TEST SPILL LOOKUP                            
         BZ    AFDHK40              NO                                          
         CLC   SBEMKT,SBBMKT                                                    
         BE    AFDHK40                                                          
*                                                                               
         NI    SPTIND,255-SPTDUMMY                                              
         LR    R0,R7               YES-TEST ANY DEMO VALUES                     
         LA    R1,SPTDEMOS                                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   AFDHK40                                                          
         LA    R1,4(R1)                                                         
         BCT   R0,*-14                                                          
         OI    SPTIND,SPTDUMMY     NO DEMOS - EXCLUDE                           
         B     AFDHK40                                                          
*                                                                               
AFDHK30  TM    SBEFLAG8,SBE8CP2    COMSCORE PASS 2?                             
         BZ    AFDHK40             NO                                           
*                                                                               
         LR    R0,R7               R0=COUNTER                                   
         LA    R1,SPTDEMOS         R1=A(OUTPUT)                                 
         LA    RF,SAVEDEMS         RF=A(INPUT)                                  
         LA    R3,DEMOS            DEMO LIST                                    
         LA    R4,PURVALSA         R4 = PURCHASED DEMO VALUES                   
         CLI   SBESVI,X'FF'        HAVE SVI ADJUSTMENTS?                        
         BNE   *+8                 NO                                           
         LA    R4,PURVALSU         R4 = UNADJUSTED PUR DEMO VALUES              
*                                                                               
AFDHK35  CLI   2(R3),0             COMSCORE DEMO?                               
         BNE   AFDHK37             NO - DON'T MODIFY SPTDEMOS                   
         TM    SPLKCERR,SPLKCBAD   COMSCORE LOOKUP ERROR?                       
         BNZ   *+14                YES - REPORT PURCHASED DEMOS                 
         CLC   0(4,RF),XFF         NULL COMSCORE VALUE?                         
         BNE   AFDHK36             NO                                           
*                                                                               
         MVC   0(4,R1),0(R4)       MOVE PURCHASED DEMO TO SPTDEMOS              
         NI    0(R1),X'FF'-X'80'   TURN OFF MANUAL OVERRIDE BIT                 
         B     AFDHK37             BUMP POINTERS                                
*                                                                               
AFDHK36  MVC   ADJDEMO,0(RF)       COMSCORE DEMO VALUE                          
         ST    R3,ADEMLIST         A(CURRENT DEMO IN DEMO LIST)                 
         XC    A50ELEM,A50ELEM     CLEAR A(X'50') ELEMENT                       
         BRAS  RE,ADJPREC          ADJUST FOR DECIMAL PRECISION                 
         MVC   0(4,R1),ADJDEMO     MOVE TO SPTDEMOS                             
*                                                                               
AFDHK37  LA    R1,4(R1)            BUMP SPTDEMOS TO NEXT DEMO SLOT              
         LA    RF,8(RF)            BUMP SAVEDEMS TO NEXT DEMO SLOT              
         LA    R4,4(R4)            BUMP PURCHASED DEMOS                         
         LA    R3,3(R3)            BUMP TO NEXT DEMO IN DEMO LIST               
         BCT   R0,AFDHK35          PROCESS NEXT DEMO                            
*                                                                               
AFDHK40  ST    R2,SPLKAAFD         ENSURE A(SPOT) IS SET                        
         BAS   RE,AFDGO            AFFID HOOK                                   
*                                                                               
         TM    SBEFLAG2,SBEBOOK                                                 
         BZ    AFFIDHKX                                                         
         L     R1,SPLKDBLK         R1=A(SPGETDEMO DBLOCK)                       
         MVC   SPTBOOK,DBACTBK-DBLOCK(R1)                                       
         MVC   SPTBKTYP,DBBTYPE-DBLOCK(R1)                                      
*                                                                               
AFFIDHKX XC    THISDEMS(MAXDEMS*8),THISDEMS                                     
*                                                                               
         LA    RE,SPDEMLK          RESTORE WHAT WAS IN DMCB                     
         ST    RE,DMCB             SAVER1 IN SPGETDEMF POINTS TO THIS!          
         MVI   DMCB,X'FF'          THIS WAS ALSO SET ON THE CALL                
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
* HOOK TO USER                                                                  
*                                                                               
GO       OC    SBSPHOOK,SBSPHOOK   TEST IF HOOK PASSED                          
         BZR   RE                  NO                                           
         NTR1                                                                   
         L     RF,SBSPHOOK                                                      
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         J     EXIT                                                             
*                                                                               
AFDGO    OC    SBAFHOOK,SBAFHOOK   TEST IF AFFID HOOK PASSED                    
         BZR   RE                  NO                                           
         NTR1                                                                   
         L     RF,SBAFHOOK                                                      
         L     RE,USERRD                                                        
         LA    R1,SPDEMLK          PASS R1=A(SPDEMLK BLOCK)                     
         L     R0,20(RE)                                                        
         LM    R2,RC,28(RE)                                                     
         BASR  RE,RF                                                            
         J     EXIT                                                             
         EJECT                                                                  
* CHANGE BDPURP PRIOR TO GETRATE CALL IF NEEDED *                               
* ALWAYS RESTORE IT !                           *                               
*                                                                               
         USING BUYREC,R4                                                        
GOGETRAT ST    RE,SVRE                                                          
         MVC   SVBDPURP,BDPURP                                                  
         MVC   SVBDNTAX,BDNTAX                                                  
         MVC   SVBDCIND,BDCIND                                                  
*                                                                               
         TM    SBEFLAG3,SBE3M0DL   TEST WANT MARKET0 DOLLARS (CANADA)           
         BZ    *+8                                                              
         MVI   BDPURP,X'FD'                                                     
         TM    SBEFLAG3,SBE3TRD    TEST MUSH CASH TRADE                         
         BZ    *+8                                                              
         MVI   BDPURP,X'FE'                                                     
*                                                                               
         CLI   SBETAX,YES          TEST TO SUBTRACT TAX                         
         BNE   *+10                NO                                           
         XC    BDNTAX,BDNTAX                                                    
*                                                                               
         TM    SBEFLAG,SBE2COS                                                  
         BZ    GOGET20                                                          
         CLI   SBECS,C'Y'          IS THIS COS2 CALL FOR CHILD SPOT?            
         BNE   *+12                 NO                                          
         MVI   BDCIND,BDCGROSQ      YES - EXTRACT TRADE COST AT GROSS           
         B     *+14                AND NEVER MIX WITH COS2                      
         L     RF,DMCB                                                          
         MVC   0(4,RF),=C'COS2'                                                 
*                                                                               
GOGET20  GOTO1 VGETRATE                                                         
         MVC   BDNTAX,SVBDNTAX                                                  
         MVC   BDPURP,SVBDPURP                                                  
         MVC   BDCIND,SVBDCIND                                                  
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO MOVE APPLICABLE PSTTAB ENTRY TO FIRST LOCATION                 
* NB: GETRATE FILLS POSITIONAL FIELDS                                           
* ENTRY - RF=A(PSTTAB TO SCAN)                                                  
* EXIT  - MOVED TO FIRST FIELD AREA FOR EASY REFERENCE                          
SHIFTPST ST    RE,SAVERE                                                        
         LR    R1,RF                                                            
         USING SPTTABD,R5           R5 = SPOT TABLE                             
         MVC   SPOTDATE,SPTRDATE    PASS SPOT DATE TO GETMOS                    
         DROP  R5                   DROP R5                                     
         BRAS  RE,GETMOS            GET MOS                                     
         LR    RF,R1                RESTORE RF                                  
         LHI   RE,10                                                            
SHFPST1  OC    0(L'XPSTTAB,RF),0(RF)                                            
         JNZ   SHFPST2                                                          
         AHI   RF,L'XPSTTAB                                                     
         BRCT  RE,SHFPST1                                                       
         J     SHFPST3                                                          
SHFPST2  CR    R1,RF                                                            
         JE    SHFPSTX                                                          
         MVC   0(L'XPSTTAB,R1),0(RF)                                            
         J     SHFPSTX                                                          
* NO PST SET (GETRATE DOES NOT FILL FIELDS FOR X/Z SETTINGS)                    
* BUT IF WANT OUTPUT GST/PST THEN NEED TO KNOW OF ANY SETTING INCL X/Z          
SHFPST3  TM    SBEGST,SBEGSTO+SBEPSTO  TEST OUTPUT GST/PST REQUIRED             
         JZ    SHFPSTX                                                          
         LA    RE,BDELEM                                                        
         SR    RF,RF                                                            
SHFPST4  CLI   0(RE),0                                                          
         JE    SHFPSTX             NOWT                                         
         CLI   0(RE),X'6B'         LOCATE PST ELEM                              
         JE    SHFPST5                                                          
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         J     SHFPST4                                                          
*                                                                               
SHFPST5  AHI   RE,PSTVALS-PSTELEM  POINT TO VALS                                
         LA    RF,10                                                            
         CLI   0(RE),0                                                          
         JNE   SHFPST6             FOUND A PROVINCE SETTING                     
         AHI   RE,1                                                             
         BRCT  RF,*-12                                                          
         J     SHFPSTX                                                          
* PROVINCE POSITIONS 1-10 ARE BC/AL/SA/MA/ON/PQ/NB/NS/PE/NF                     
SHFPST6  CHI   RF,10               BC?                                          
         BE    *+12                YES                                          
         CHI   RF,6                ON?                                          
         BNE   SHFPST6A            NO                                           
         CLC   SPOTMOS,=X'DCE1'    BEFORE JUL01/10?                             
         JL    SHFPSTX             YES - EXIT                                   
*                                                                               
SHFPST6A MVC   XPSTCODE-XPSTTAB(1,R1),0(RE)  SET CODE                           
         MVI   XPSTPROV+1-XPSTTAB(R1),C'Q'   ASSUME QST                         
         LHI   RE,7500                       LATEST (1997) RATE! 7.5%           
         CHI   RF,5                BCT COUNTER=5 MEANS PROV#6-QUEBEC            
         JNE   SHFPST6B            NOT QUEBEC                                   
         CLC   SPTRDATE-SPTTABD(2,R5),=X'DD9B' ON OR AFTER 12/27/10?            
         JL    SHFPST7             NO - USE OLD RATE OF 7.5%                    
         LHI   RE,8500             LATEST (2011) RATE! 8.5%                     
         CLC   SPTRDATE-SPTTABD(2,R5),=X'DF9A' ON OR AFTER 12/26/11?            
         JL    SHFPST7             NO - USE 2011 RATE OF 8.5%                   
         LHI   RE,9500             LATEST (2012) RATE! 9.5%                     
         CLC   SPTRDATE-SPTTABD(2,R5),=X'E19F' ON OR AFTER 12/31/12?            
         JL    SHFPST7             NO - USE 2012 RATE OF 9.5%                   
         LHI   RE,9975             LATEST (2013) RATE! 9.975%                   
         J     SHFPST7                                                          
*                                                                               
SHFPST6B LHI   RE,13000            HST 13% ON/AFTER 12/31/07                    
         CLC   SPTRDATE-SPTTABD(2,R5),=X'D79F'                                  
         JNL   SHFPST6C                                                         
         LHI   RE,14000            HST 14% AFTER 7/1/06                         
         CLC   SPTRDATE-SPTTABD(2,R5),=X'D4E1'                                  
         JNL   *+8                                                              
         LHI   RE,15000                      ALL HST ARE 15%                    
*                                                                               
SHFPST6C MVI   XPSTPROV+1-XPSTTAB(R1),C'S' SET FOR NS                           
         CHI   RF,3                BCT COUNTER=3 MEANS PROV#8-NOVA SCOT         
         JE    SHFPST7                                                          
         MVI   XPSTPROV+1-XPSTTAB(R1),C'B' SET FOR NB (BCT=4/PROV#7)            
         JH    SHFPST7                                                          
         MVI   XPSTPROV+1-XPSTTAB(R1),C'F' SET FOR NF (BCT=1/PROV#10)           
SHFPST7  STCM  RE,7,XPSTRATE-XPSTTAB(R1)                                        
SHFPSTX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
FIRST    DC    AL1(0)                                                           
*                                                                               
LADDRS   DS    0F                                                               
LXTRA    DC    (AXTRAN)A(0)        EXTENSION ROUTINE ADDRESSES                  
*                                                                               
LSPGETDM DC    A(0)                V(SPGETDEMO)                                 
LSPBOOK  DC    A(0)                V(SPOTBOOK)                                  
LSPDEMUP DC    A(0)                V(SPDEMUP)                                   
LGETRATE DC    A(0)                                                             
LCALLOV  DC    A(0)                                                             
LDATAMGR DC    A(0)                                                             
LDATCON  DC    A(0)                                                             
LADDAY   DC    A(0)                                                             
LGETDAY  DC    A(0)                                                             
LPWCALC  DC    A(0)                                                             
LSTAPACK DC    A(0)                                                             
*                                                                               
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   0(1,R6),ELCDLO                                                   
         JL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         JH    NEXTEL                                                           
         CR    RE,RE               SET CC EQUAL AND EXIT                        
         BR    RE                                                               
*                                                                               
NEXTELX  LTR   RE,RE               SET CC NOT EQUAL                             
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO APPLY EXTRACT FILTERS TO SPOT ENTRY                            
*                                                                               
         USING SPTTABD,R5                                                       
FILTER   ST    RE,SAVERE                                                        
         CLC   SPTRDATE,FSTDATE    TEST FOR REQUESTED PERIOD                    
         JNL   FILTER2             NO                                           
         CLI   SBEBEF,C'Y'         TEST BEFORE PERIOD DATA REQUESTED            
         JNE   FILTERN                                                          
         XC    SPTRDATE,SPTRDATE   YES                                          
         OI    SPTIND,SPTNODEM     SUPPRESS DEMO LOOKUP                         
         J     FILTER4                                                          
*                                                                               
FILTER2  CLC   SPTRDATE,LSTDATE                                                 
         JNH   FILTER4                                                          
         CLI   SBEAFTER,C'Y'                                                    
         JNE   FILTERN                                                          
         MVC   SPTRDATE,XFF                                                     
         OI    SPTIND,SPTNODEM                                                  
*                                                                               
FILTER4  CLI   SPTPRD1,0           TEST FOR UNALLOCATED SPOT                    
         JNE   FILTER6             NO                                           
         CLI   SBEUNALL,YES        TEST IF INCLUDING UNALLOCATED                
         JE    FILTER8             YES                                          
         J     FILTERN                                                          
*                                                                               
FILTER6  CLI   SBEPRD,X'FE'        TEST UNALLOCATED ONLY                        
         JE    FILTERN             YES                                          
         CLI   SBEPRD,0            TEST ALL PRODUCTS                            
         JE    FILTER8             YES                                          
         CLI   SBEPRD,X'FF'        TEST PRODUCT=POL                             
         JE    FILTER8             YES                                          
         TM    SBEFLAG3,SBE3TRD    TEST MUSH CASH/TRADE                         
         JO    FILTER8             YES                                          
         CLC   SBEPRD,SPTPRD1                                                   
         JE    FILTER8                                                          
         CLC   SBEPRD,SPTPRD2                                                   
         JNE   FILTERN                                                          
         J     FILTER8                                                          
*                                                                               
FILTER8  OC    SBECML,SBECML       TEST COMMERCIAL FILTERING                    
         JZ    FILTERY             NO                                           
         CLC   SBECML+1(2),SPTCML1                                              
         JE    FILTERY                                                          
         CLC   SBECML+1(2),SPTCML2                                              
         JE    FILTERY                                                          
*                                                                               
FILTERN  LTR   RB,RB               SET CC=NEQ                                   
         J     FILTERX                                                          
*                                                                               
FILTERY  CR    RB,RB               SET CC=EQ                                    
*                                                                               
FILTERX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE ACTUAL BOOK FOR A SPOT ENTRY                           
* ON EXIT, SPTACTBK CONTAINS ACTUAL BOOK IF FOUND                               
*                                                                               
GETBK    ST    RE,SAVERE                                                        
*        CLI   SBSTA,C'Z'          TEST CABLE                                   
*        BHR   RE                  NFB (NO BOOKS)                               
         CLI   SBEDEMTY,C'P'       TEST FOR PURCHASED LOOKUP                    
         BER   RE                  YES-DO NOT NEED BOOK                         
         CLI   SBEDEMTY,C'0'       TEST FOR SQAD LOOKUP                         
         BNLR  RE                  YES-DO NOT NEED BOOK                         
         MVC   HALF,SPTRDATE       LOOK UP BASED ON SPOT DATE                   
         CLI   SBEDEMTY,C'A'       UNLESS AFFID LOOKUP                          
         JNE   GETBK2                                                           
         OC    SPTADATE,SPTADATE   AND AFFID IS PRESENT                         
         JZ    GETBK2                                                           
         MVC   HALF,SPTADATE                                                    
*                                                                               
GETBK2   L     RE,SBABKLST         RE=A(BOOK TABLE)                             
         L     RF,SBNBKS           RF=N'BOOK ENTRIES                            
*                                                                               
GETBK4   CLC   HALF,0(RE)          TEST FOR FIT WITHIN PERIOD                   
         JL    GETBK6                                                           
         CLC   HALF,2(RE)                                                       
         JH    GETBK6                                                           
         MVC   SPTACTBK,4(RE)                                                   
         OC    6(2,RE),6(RE)       HAVE LPM BOOK?                               
         JZ    GETBKX              NO                                           
*                                                                               
* OPUP: EVEN THOUGH THE WEEKLY BOOK IS PROVIDED WITH OPTIONS WTP AND            
* LPMWKLY=Y, GRAB THE MONTHLY BOOK. WE NEED TO KNOW THE MONTHLY BOOK            
* BECAUSE ONCE THE WEEKLY DATA GOES AWAY WE'LL ENFORCE MONTHLY LOOKUPS          
* IN SPGETDEMF.  THE WEEKLY BOOK BELOW IS REALLY NOT NEEDED, BECAUSE            
* SPGETDEMF CALCULATES THE WEEK FROM THE DATE.                                  
*&&DO                                                                           
         CLI   SBD0PROF+10,C'Y'    IF LPMWKLY=Y                                 
         BE    GETBK5                                                           
         TM    SBEFLAG2,SBEWTP     WTP OPTION                                   
         BO    GETBK5                                                           
*&&                                                                             
* ALWAYS DO THIS FOR OV, AND WEEKLY                                             
         J     GETBK5                                                           
*                                                                               
         MVC   SPTACTBK,6(RE)      YES - USE THIS BOOK                          
GETBK5   OI    FLAGS,LPMBOOK       INDICATE WE HAVE LPM BOOK                    
         TM    8(RE),X'80'         HAVE OVERNIGHT?                              
         JZ    GETBKX              NO                                           
         OI    FLAGS,LPMBKOV       YES - INDICATE OVERNIGHT                     
         J     GETBKX                                                           
*                                                                               
GETBK6   LA    RE,9(RE)            NEXT ENTRY                                   
         BRCT  RF,GETBK4                                                        
*                                                                               
GETBKX   L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R5                                                               
         SPACE 2                                                                
*                                                                               
* EXTENSION ROUTINES                                                            
*                                                                               
         DS    0D                                                               
EXTRA    NMOD1 0,**SPBX**,RA                                                    
         USING WORKD,R9                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING EXTRA+8192,RC                                                    
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     LOOK                                                             
         B     UPG                                                              
         B     PRDDEMOS                                                         
         B     GETDEMOS                                                         
         B     GETPW                                                            
         B     AFFINIT                                                          
         B     GETTAL                                                           
         B     FUSION                                                           
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INTERFACE TO SPGETDEMO FOR RE-RATE                                            
* AT ENTRY, P1=A(LOOKUP BOOK)                                                   
*           P2=A(DAYS)                                                          
*           P3=A(TIMES)                                                         
*           P4=A(SPTTAB ENTRY)                                                  
* ON EXIT,  CC=EQ AND DEMVALS CONTAINS DEMO VALUES                              
*           CC=NE TO EXCLUDE THE SPOT(S) FOR SPILL                              
*                                                                               
* ON 12SEP00 WHILE MOVING SOME CODE AROUND, I DISCOVERED THAT ALL               
* REFERENCES TO SPTTAB WERE OFF R2, WHICH POINTS TO A(LOOKUP BOOK).             
* NOT FUCKING GOOD.  I ADDED CODE TO PASS P4 IN 12/98 FOR WTP.  I THINK         
* THAT'S A MUCH BETTER THING TO DO, AND NOW WILL USE R7 HERE FOR                
* SPTTABD (AND DROP THE FUCKER AT THE END).                                     
*                                                                               
         PUSH USING                                                             
LOOK     DS    0H                                                               
         L     R2,0(R1)            R2=A(BOOK)                                   
         LM    R5,R7,4(R1)         R5=A(DAYS),R6=A(TIMES),R7=A(STB ENT)         
         USING SPTTABD,R7                                                       
         L     R4,SBAIO1           R4=A(BUY RECORD)                             
         USING BUYREC,R4                                                        
         XC    THISDEMS(MAXDEMS*8),THISDEMS                                     
         XC    DEMFACT,DEMFACT                                                  
         XC    DEMVALS(MAXDEMS*4),DEMVALS                                       
*                                                                               
LOOK1    XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
         ST    R4,SPLKABUY                                                      
         LA    R0,IOAREA                                                        
         ST    R0,SPLKAREC                                                      
         MVC   SPLKXTND,SBALKXTD                                                
*                                                                               
         ICM   R1,15,SPLKXTND      POINT TO EXTENSION AREA                      
         USING SPLKXTD,R1                                                       
         XC    SPXTAREA,SPXTAREA   CLEAR EXTENSION AREA                         
         CLI   LPMOPT,C'N'         SUPRESS LPM START DATE FROM MKT REC?         
         BNE   *+8                 NO, WE WANT THE LPM START DATE               
         OI    SPXTFLAG,SPXTNLPM   YES, TELL SPGETDEMF TO SUPRESS               
*                                                                               
         TM    SBINDS,SBIWRI       CALLER SPOT WRITER?                          
         BZ    *+8                                                              
         OI    SPXTFLG2,SPXTSPWR                                                
*                                                                               
         MVC   SPXTEOWD,SBESTOWD   ESTIMATE OUT OF WEEK ROTATOR                 
         DROP  R1                                                               
*                                                                               
         MVC   SPLKAFAC,SBCOMFAC   A(COMFACS)                                   
         LA    R0,DEMOS            A(DEMO LIST)                                 
         ST    R0,SPLKALST                                                      
         LA    R0,THISDEMS         A(DEMO VALUES)                               
         ST    R0,SPLKAVAL                                                      
         LA    R0,WGTLST                                                        
         ST    R0,SPLKAWGT                                                      
         LA    R0,SB1WPROF                                                      
         ST    R0,SPLKA1W                                                       
*                                                                               
         MVC   SPLKFIL(3),=C'TTN'                                               
         CLI   SBCPROF+3,C'0'      TEST FOR NSI                                 
         BE    *+8                 YES                                          
         MVI   SPLKSRC,C'A'        NO-SET IT TO ARB                             
*                                                                               
         OC    SBAXSPIL,SBAXSPIL   TEST CANADIAN SPILL LOOKUP                   
         BNZ   LOOK1A                                                           
         CLI   CANADA,C'Y'         TEST CANADIAN AGENCY                         
         BNE   LOOK2                                                            
         CLI   SBCEXTRA+0,C'U'     TEST FOR US DEMO LOOKUP                      
         BE    LOOK2                                                            
*                                                                               
LOOK1A   MVI   SPLKMED,C'C'                                                     
*                                                                               
LOOK2    DS    0H                                                               
         LA    RF,SBQMED           MEDIA                                        
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BNE   *+8                 NO                                           
         LA    RF,SBMED            YES - CURRENT MEDIA                          
         CLI   0(RF),C'R'                                                       
         BNE   LOOK2A                                                           
         MVI   SPLKMED,C'R'                                                     
         MVC   SPLKSPL,SBMKNUM                                                  
         MVI   SPLKBEST,C'P'                                                    
*                                                                               
LOOK2A   MVC   SPLKAGY,SBAGY                                                    
         MVC   SPLKUID,SBUSERID                                                 
         MVC   SPLKCLI,SBCLT                                                    
         TM    SBEFLAG4,SBE42DEC   REPORT SUPPORT 2 DEC                         
         BZ    *+8                                                              
         OI    SPLKOPT,SPLKOP2D                                                 
         TM    SBEFLAG9,SBE92DEC   REPORT SUPPORTS 2 DEC IMPRESSIONS?           
         BZ    LOOK2AA             NO                                           
         ICM   RE,15,SPLKXTND      POINT TO EXTENSION AREA                      
         USING SPLKXTD,RE          DEMO EXTENSION AREA DSECT                    
         OI    SPXTFLG2,SPXTOP2I   REPORT 2 DECIMAL IMPRESSIONS                 
         DROP  RE                  DROP DEMO EXTENSION USING                    
*                                                                               
LOOK2AA  MVC   ORIGBOOK,0(R2)      SAVE BOOK WE CAME IN WITH                    
         MVC   SPLKDBK,0(R2)       ACTUAL BOOK                                  
*                                                                               
         CLI   SBBKAVG,0           BOOK AVERAGING?                              
         BE    LOOK2B               NO                                          
         MVI   SPLKDBK,X'FF'                                                    
         MVC   SPLKDBK+1(1),SBBKAVG                                             
         MVC   SPLKLBK,0(R2)                                                    
*                                                                               
LOOK2B   MVC   SPLKSTA,SBSTA                                                    
         CLI   SPLKSTA+4,C' '                                                   
         BE    *+12                                                             
         CLI   SPLKSTA+4,C'N'                                                   
         BNE   *+8                                                              
         MVI   SPLKSTA+4,C'T'                                                   
         BRAS  RE,GETNTI                                                        
***                                                                             
* CANADIAN SOFT DEMOS SUPPORT                                                   
***                                                                             
         CLI   CANADA,C'Y'         CANADIAN AGENCY?                             
         BNE   LOOK2B1             NO                                           
*                                                                               
         CLI   CDEMSTA,C'A'        HAVE STATION OVERRIDE?                       
         BL    *+10                NO                                           
         MVC   SPLKSTA,CDEMSTA                                                  
*                                                                               
         MVI   SPLKSRC,C'N'        INIT TO NSI                                  
         TM    CDEMFLG,X'01'       HAVE BBM                                     
         BZ    *+8                 NO                                           
         MVI   SPLKSRC,C'A'        YES - SET TO BBM/ARB                         
*                                                                               
         ICM   R1,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    LOOK2B1             NO                                           
         USING SPLKXTD,R1                                                       
         OI    SPXTFLAG,SPXTCDEM   DO NOT GO THROUGH OLD SOFT DEMO CODE         
         MVI   SPXTSVFL,0          WE MUST CLEAR IT HERE!                       
         TM    CDEMFLG,X'80'       SUPPRESS IMPS?                               
         BZ    LOOK2B1             NO                                           
         MVI   SPXTSVFL,X'80'      SET TO SUPPRESS IMPS                         
         DROP  R1                                                               
*                                                                               
LOOK2B1  TM    SBINDS,SBIRSOVR     TEST RATING SERVICE OVERRIDE                 
         BZ    *+16                                                             
         OI    SPLKOPT,SPLKOARB    YES-FORCE ARB IF NEED BE                     
         TM    SBINDS,SBIAGYMK     TEST TO PASS AGY MKT NUM                     
         BZ    *+10                                                             
         MVC   SPLKUMK,SBBMKT                                                   
         MVC   SPLKBTYP,BKTYPE     SPECIAL BOOK TYPE                            
***                                                                             
* IN CASE THERE IS A RATING SERVICE OVERRIDE AND SBIAGYMK IS NOT SET            
* WE STILL NEED TO SET THE MARKET FOR MEDIA T TO GET LPM DATA                   
***                                                                             
         CLI   SBMED,C'T'          MEDIA T?                                     
         BNE   *+10                NO                                           
         MVC   SPLKUMK,SBBMKT      YES - SET THE USER MARKET                    
*                                                                               
         CLI   SPLKBTYP,C' '       BLACK/HISPANIC WINS                          
         BH    LOOK2C                                                           
         CLI   SBBKAVG,0           BOOK AVERAGING BEATS OLYM EXC TOO            
         BNE   LOOK2C                                                           
         TM    SPLKDBK+1,X'80'     TEST OLYM EXCL APPLIES                       
         BZ    LOOK2C              NO                                           
         MVI   SPLKBTYP,C'O'       SET TO USE OLYM EXCL                         
LOOK2C   NI    SPLKDBK+1,X'7F'     MAKE SURE X'80' FLAG IS OFF                  
*                                                                               
         OC    SBEMKT,SBEMKT       TEST REQUESTED MARKET SET                    
         BZ    LOOK3               NO                                           
         CLC   SBEMKT,SBBMKT       TEST FOR SPILL MKT LOOKUP                    
         BE    LOOK3               NO                                           
         MVC   SPLKSPL,SBBSPL      YES-SPILL MKT RTG SRV NUMBER                 
         TM    SBINDS,SBIRSOVR                                                  
         BZ    *+12                                                             
         TM    SBINDS,SBIAGYMK                                                  
         BZ    LOOK3                                                            
         MVC   SPLKUMK,SBEMKT      PASS SPILL AGENCY MARKET NUMBER              
*                                                                               
LOOK3    MVC   SPLKDAY,0(R5)       BUY-LINE DAY/TIME                            
         MVC   SPLKTIM,0(R6)                                                    
*                                                                               
         MVC   SPLKSVI,SBESVI      SVI LOOK-UP OPTION                           
         CLI   SPLKSVI,0           TEST AUTO-SVI REQUESTED                      
         BNE   LOOK3F              NO                                           
*                                                                               
         OC    LSTELEM,LSTELEM     TEST HAVE DATE OF LAST ELEM YET              
         BNZ   LOOK3B              YES                                          
* FIND THE DATE OF THE LAST ELEMENT OR USE BDEND                                
         L     R0,ASPTTAB+4        NUMBER OF ENTRIES IN SPTTAB                  
         L     R1,ASPTTAB+8        ENTRY LENGTH                                 
         BCTR  R0,0                BACK UP TO LAST                              
         MR    R0,R0                                                            
         A     R1,ASPTTAB          POINT TO LAST ENTRY                          
*                                                                               
LOOK3A   MVC   LSTELEM,SPTRDATE-SPTTABD(R1)  SAVE LAST ENTRY DATE               
         CLC   LSTELEM,=X'FFFF'                                                 
         BNE   LOOK3B                                                           
         S     R1,ASPTTAB+8        BACK UP TO PREVIOUS ENTRY                    
         C     R1,ASPTTAB          STILL IN TABLE                               
         BNL   LOOK3A                                                           
         GOTO1 VDATCON,DMCB,(3,BDEND),(2,LSTELEM)                               
*                                                                               
LOOK3B   MVC   SPLKAUTF,BDWKIND                                                 
         GOTO1 VDATCON,DMCB,(3,BDSTART),(2,SPLKAUST)                            
         MVC   SPLKAUND,LSTELEM                                                 
*                                                                               
         CLC   SBQBOOK,=C'ACT'     TEST ACTUAL BOOK LOOKUP                      
         BNE   LOOK3F              NO                                           
         OC    SPTRDATE,SPTRDATE                                                
         BZ    LOOK3F                                                           
         CLC   SPTRDATE,=X'FFFF'                                                
         BE    LOOK3F                                                           
* USE BROADCAST MONTH START/END DATE FOR AUTO SVI LOOKUP                        
         GOTO1 VDATCON,DMCB,(2,SPTRDATE),(0,WORK)                               
         ICM   RF,15,SBGETBRD                                                   
         BZ    LOOK3F                                                           
         GOTO1 (RF),(R1),(1,WORK),WORK+6,VGETDAY,VADDAY                         
         GOTO1 VDATCON,(R1),WORK+6,(2,SPLKAUST)                                 
         GOTO1 (RF),(R1),WORK+12,(2,SPLKAUND)                                   
*                                                                               
LOOK3F   TM    SBEFLAG2,SBEWTP     WEEKLY OPTION?                               
         BZ    *+8                                                              
         OI    SPLKOPT,SPLKOWTP                                                 
*                                                                               
         CLI   SBD0PROF+11,C'Y'              PROFILE INDICATES                  
         BE    *+8                           OVERNIGHTS POSTING                 
         CLI   LPMOV,C'Y'                                                       
         BNE   *+8                                                              
         OI    SPLKOPT2,SPLKOPT2_OPOST       OVERRIDE ON                        
*                                                                               
* CHECK OVERNIGHTS OPTION SET TO 'M'                                            
         NI    SPLKOPT2,X'FF'-SPLKOPT2_SMMM                                     
         CLI   SBD0PROF+11,C'M'                 IF OVERNIGHT OPTION             
         BE    *+8                              SET TO M                        
         CLI   LPMOV,C'M'                       SET OPTION TO POST              
         BNE   *+12                             OFF MONTHLY FOR                 
         OI    SPLKOPT2,SPLKOPT2_SMMM           SET METERED MKTS                
         OI    SPLKOPT2,SPLKOPT2_OPOST          AND SET OVN ON                  
*                                                                               
         CLI   LPMOV,C'N'                                                       
         BNE   *+8                                                              
         NI    SPLKOPT2,X'FF'-SPLKOPT2_OPOST OVERRIDE OFF                       
         TM    FLAGS,LPMBOOK                 LPMWK=Y NSIL                       
         BO    LOOK4                                 NSIL                       
*                                                                               
         TM    SBEFLAG2,SBEWTP                                                  
         BO    LOOK4                                                            
*&&DO                                                                           
         CLI   LPMOV,C'Y'                                                       
         BE    LOOK4                                                            
         CLI   LPMOV,C'M'                                                       
         BE    LOOK4                                                            
         CLI   SBD0PROF+10,C'Y'              SET SPLKAFST DATES                 
         BE    LOOK4                         IF LPMWK=Y OPTION                  
         CLI   SBD0PROF+11,C'Y'              SET SPLKAFST DATES                 
         BE    LOOK4                         IF OVN OPTION                      
         CLI   SBD0PROF+11,C'M'              SET SPLKAFST DATES                 
         BE    LOOK4                         IF OVN OPTION                      
*&&                                                                             
                                                                                
         CLI   SPLKMED,C'T'                  DIGITAL TRANSITION BOOK            
         BNE   LOOK3H                        ALWAYS PASS TO SPGETDEMF           
*&&DO                                                                           
         CLC   SPLKDBK,=AL2(JUN_09)          SO SPGETDEMF CAN SWITCH            
         BE    LOOK4                         THE BOOK LOOKUP                    
         CLC   SPLKDBK,=AL2(FEB_10)          OLYMPIC BOOKS WITH                 
         BE    LOOK3G                        OLYMPIC EXCLUSION TURNED           
         CLC   SPLKDBK,=AL2(FEB_14)          OLYMPIC BOOKS WITH                 
         BE    LOOK3G                        OLYMPIC EXCLUSION TURNED           
         CLC   SPLKDBK,=AL2(FEB_02)                                             
         BNE   LOOK3H                                                           
*&&                                                                             
LOOK3G   CLI   SBD0PROF+7,C'Y'               ON ALSO PASS SPLKAFST              
         BE    *+8                                                              
         CLI   SBD0PROF+7,C'F'                                                  
         BNE   LOOK3H                                                           
         OI    SPLKOPT,SPLKOEXO                                                 
         B     LOOK4                                                            
*                                                                               
******LOOK3H   B     LOOK4A                                                     
LOOK3H   DS    0X                                                               
LOOK4    DS    0X                                                               
***         MVC   SPLKAFST,SPTRDATE-SPTTABD(R7)                                 
         MVC   SPLKAFST,SPTRDATE                                                
         OC    SPLKAFST,SPLKAFST   SKIP IF NULLS                                
         BZ    LOOK4A                                                           
         GOTO1 VDATCON,DMCB,(2,SPLKAFST),(0,WORK)                               
         GOTO1 VADDAY,DMCB,WORK,WORK+6,6                                        
         GOTO1 VDATCON,DMCB,(0,WORK+6),(2,SPLKAFND)                             
*                                                                               
*                                  CALL SPGETDEMO                               
LOOK4A   CLC   BDSTART,BDEND       SET A DATE FOR SINGLE WEEK BUYS              
         BE    *+14                SO WE CAN LOOK AT DEMOS FOR SPECIALS         
         OC    BDEND,BDEND         THAT RUN ON THAT DATE                        
         BNZ   LOOK4B                                                           
         OI    SPLKOPT,SPLKO1WB    TELL GETDEMF                                 
*                                  ADATE=BDSTART                                
         GOTO1 VDATCON,DMCB,(3,BDSTART),(2,SPLKADAT)                            
*                                                                               
LOOK4B   CLI   CANADA,C'Y'                                                      
         BNE   LOOK4D                                                           
         MVC   SPLKALF,ALPHAMKT                                                 
         OC    SBEMKT,SBEMKT       TEST SPILL                                   
         BZ    LOOK4D               NO                                          
         CLC   SBEMKT,SBBMKT                                                    
         BE    LOOK4D              NOT A SPILL POINTER                          
         LHI   R0,MAXDEMS                                                       
         L     RE,SPLKALST                                                      
*                                                                               
LOOK4C   CLI   1(RE),C'I'                                                       
         BNE   *+8                                                              
         NI    1(RE),X'FF'-X'40'   MAKE LOWERCASE TO FOOL GETDEM                
         AHI   RE,3                                                             
         BCT   R0,LOOK4C                                                        
*                                                                               
LOOK4D   ICM   R1,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    LOOK4D1             NO                                           
         USING SPLKXTD,R1                                                       
*                                                                               
         TM    CBLFLAG,CFFUS       OVERRIDE TO FUSION?                          
         BZ    *+8                 NO                                           
         OI    SPXTFLAG,SPXTFUS                                                 
*                                                                               
         TM    CBLFLAG,CFNLS       OVERRIDE TO NIELSON?                         
         BZ    *+8                 NO                                           
         OI    SPXTFLAG,SPXTNLS                                                 
*                                                                               
         TM    CBLFLAG,CFNONE      OVERRIDE TO NO CABLE?                        
         BZ    *+8                 NO                                           
         OI    SPXTFLAG,SPXTNONE                                                
*                                                                               
         TM    CBLFLAG,CFLPM       OVERRIDE TO NO CABLE EXCEPT FOR LPM?         
         BZ    *+8                 NO                                           
         OI    SPXTFLAG,SPXTLPM                                                 
*                                                                               
         XC    SPXTHEAD,SPXTHEAD   CLEAR THIS!                                  
*                                                                               
         CLI   SPLKSTA,C'0'        CABLE?                                       
         BL    LOOK4D1             NO                                           
         MVC   SPXTHEAD,SPLKSTA    SET THE HEADEND                              
         DROP  R1                                                               
         XC    SPLKSTA,SPLKSTA                                                  
         MVC   SPLKSTA(3),SBCBLNET REPLACE THE STATION WITH THE NETWORK         
*                                                                               
LOOK4D1  NI    SPLKOPT2,X'FF'-SPLKOPT2_LPOST   LPMWK=N                          
         CLI   SBD0PROF+10,C'Y'          SET FLAG -LPMWK OPTION                 
         BNE   *+8                                                              
         OI    SPLKOPT2,SPLKOPT2_LPOST   LPMWK=Y                                
         TM    SBEFLAG8,SBE8NPM    USE NEW POSTING METHODOLOGY?                 
         BZ    LOOK4DB             NO                                           
         OI    SPLKOPT2,SPLKOPT2_NPM                                            
***                                                                             
* IF NEW POSTING METHODOLOGY IS TURNED ON FOR CANADA THEN ALWAYS PASS           
* A(SPOT TABLE),N'ENTRIES AND L'TABLE ENTRY TO SPGETDEMF                        
***                                                                             
         CLI   CANADA,C'Y'         CANADIAN AGENCY?                             
         BNE   LOOK4DB             NO                                           
         MVC   SPLKASPT,ASPTTAB    A(SPOT TABLE)                                
         MVC   SPLKNSPT,NSPTTAB+2  N'ENTRIES                                    
         MVC   SPLKLSPT,LSPTTAB+3  L'ENTRY                                      
*                                                                               
LOOK4DB  TM    FLAGS,LPMBOOK       HAVE LPM BOOK?                               
         BZ    LOOK4D2             NO                                           
* DONT SET THIS FLAG BECAUSE EVEN IF WE DIDNT ASK FOR LPMWK=Y , OVN=Y           
* COULD TRIGGER THIS SO WE DONT WANT TO CONFUSE DEMF                            
***      OI    SPLKOPT2,SPLKOPT2_LPOST   LPMWK=Y                                
         MVI   SPLKMED,C'W'                                                     
         TM    FLAGS,LPMBKOV       LPM OVERNIGHT?                               
         BZ    LOOK4D2             NO                                           
         MVI   SPLKMED,C'O'                                                     
*                                                                               
LOOK4D2  BRAS  RE,INSPDEM          CHECK DEMO LIST FOR COMSCORE/NIELSON         
*                                                                               
         TM    SBEFLAG8,SBE8CP2    COMSCORE PASS 2?                             
         BZ    LOOK4D3             NO                                           
         TM    SBEFLAG9,SBE9CERR   NIELSEN BOOK ERROR?                          
         BNZ   LOOK4D2A            YES - NO NIELSON / YES COMSCORE              
         CLC   ORIGBOOK,=X'FFFF'   FORCING ACT BOOK FOR COMSCORE?               
         BE    LOOK4D2A            YES - NO NIELSON / YES COMSCORE              
         CLI   SBEDEMTY,C'A'       TEST AFFID LOOKUP                            
         BNE   LOOK4D3             NO                                           
         OC    SPTADATE,SPTADATE   AFFID PRESENT?                               
         BZ    LOOK4D3             NO - OK TO RERATE                            
         CLI   SPECIAL,YES         -S BUYLINE?                                  
         BE    LOOK4D3             YES - WE ALWAYS RERATE THESE                 
         TM    FLAGS,AFFIDFL       COMING FROM AFFID HOOK W/ERROR?              
         BNZ   LOOK4D3             YES - IF WE HAVE NEILSEN MUST RERATE         
*                                                                               
LOOK4D2A NI    SPLKOPT2,X'FF'-SPLKOPT2_NLSN TURN OFF NIELSON                    
         TM    SPLKOPT2,SPLKOPT2_COMS  DO WE EVEN HAVE A COMSCORE DEMO?         
         BZ    LOOKY               NO - DON'T BOTHER CALLING SPGETDEMF          
*                                                                               
LOOK4D3  GOTO1 VSPGETDM,DMCB,(X'FF',SPDEMLK)                                    
*                                                                               
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   LOOKY               YES - DONE                                   
*                                                                               
* CAREFUL NOT TO DISTURB R1/DMCB HERE!   SEE LOOK4F                             
*                                                                               
         CLI   CANADA,C'Y'                                                      
         BNE   LOOK4F                                                           
         OC    SBEMKT,SBEMKT       TEST SPILL                                   
         BZ    LOOK4F               NO                                          
         CLC   SBEMKT,SBBMKT                                                    
         BE    LOOK4F              NOT A SPILL POINTER                          
         LHI   R0,MAXDEMS                                                       
         L     RE,SPLKALST                                                      
*                                                                               
LOOK4E   CLI   1(RE),X'89'         LOWERCASE I                                  
         BNE   *+8                                                              
         OI    1(RE),X'40'         MAKE IT UPPERCASE                            
         AHI   RE,3                                                             
         BCT   R0,LOOK4E                                                        
*                                                                               
LOOK4F   MVC   DEMERR,0(R1)        SAVE NIELSON ERROR CODE                      
         CLC   ORIGBOOK,=X'FFFF'   FORCING ACT BOOK FOR COMSCORE?               
         BE    *+12                YES - FAKE NIELSEN ERROR                     
         TM    SBEFLAG9,SBE9CERR   NIELSEN BOOK ERROR?                          
         BZ    *+8                 NO                                           
         MVI   DEMERR,X'45'        YES - FAKE NIELSEN ERROR                     
         CLI   DEMERR,0            ANY ERROR CODE?                              
         BE    LOOK5               NO                                           
         OC    SBEMKT,SBEMKT       SPILL LOOKUP?                                
         BZ    LOOK5               NO                                           
         CLC   SBEMKT,SBBMKT       SPILL MARKET?                                
         BE    LOOK5               NO                                           
         CLI   SBD0PROF+1,C'N'     TEST OPTION TO RETURN PURCHASED              
         BE    LOOK5               YES                                          
         CLI   PBDEMO,YES          HAVE POST BUY DEMOS?                         
         BE    LOOK8               YES                                          
         B     LOOKN               NO-EXCLUDE UNREPORTED SPILL SPOTS            
*                                                                               
LOOK5    LLC   R0,NDEMOS           R0 = NUMBER OF DEMOS                         
         LA    RE,DEMVALS          RE = A(OUTPUT DEMOS)                         
         LA    RF,PURVALSA         RF = PURCHASED DEMO VALUES                   
         CLI   SBESVI,X'FF'        HAVE SVI ADJUSTMENTS?                        
         BNE   *+8                 NO                                           
         LA    RF,PURVALSU         RF = UNADJUSTED PUR DEMO VALUES              
         LA    R5,THISDEMS         R5 = A(DEMOS FROM SPGETDEM)                  
         LA    R3,DEMOS            R3 = A(EXTRACT DEMO LIST)                    
*&&DO                                                                           
LOOK6    CLI   SBEDEMTY,C'A'       AFFID LOOKUP?                                
         BE    *+12                YES                                          
*&&                                                                             
LOOK6    TM    SPLKCERR,SPLKCBAD   COMSCORE LOOKUP ERROR?                       
         BZ    *+12                NO                                           
         CLI   2(R3),0             COMSCORE DEMO?                               
         BE    LOOK6PUR            YES - RETURN PURCHASED                       
         CLI   DEMERR,0            NIELSON LOOKUP ERROR?                        
         BE    LOOK6ACH            NO                                           
         CLI   2(R3),0             NIELSON DEMO?                                
         BE    LOOK6ACH            NO - RETURN ACHIEVED                         
*                                                                               
LOOK6PUR MVC   0(4,RE),0(RF)       PURCHASED DEMO VALUE                         
         NI    0(RE),X'FF'-X'80'   TURN OFF MANUAL OVERRIDE BIT                 
         B     LOOK6X              BUMP POINTERS                                
*                                                                               
LOOK6ACH CLC   0(4,R5),XFF         NULL COMSCORE VALUE?                         
         BE    LOOK6PUR            YES                                          
         MVC   ADJDEMO,0(R5)       SAVE DEMO LOOKUP VALUE TO ADJDEMO            
         LR    R1,RE               SAVE RE (POINTING TO DEMVALS)                
         ST    R3,ADEMLIST         A(CURRENT DEMO IN DEMO LIST)                 
         XC    A50ELEM,A50ELEM     CLEAR A(X'50') ELEMENT                       
         BRAS  RE,ADJPREC          ADJUST DEMO PRECISION IN ADJDEMO             
         LR    RE,R1               RESTORE RE (POINTING TO DEMVALS)             
         MVC   0(4,RE),ADJDEMO     ACHIEVED DEMO VALUE                          
*                                                                               
LOOK6X   LA    RE,4(RE)            BUMP DEMVALS                                 
         LA    RF,4(RF)            BUMP PURCHASED DEMOS                         
         LA    R5,8(R5)            BUMP THISDEMS                                
         LA    R3,3(R3)            BUMP DEMO LIST                               
         BCT   R0,LOOK6            PROCESS NEXT DEMO                            
*                                                                               
LOOK8    CLI   PBDEMO,YES          TEST ANY POST BUY DEMOS                      
         BNE   LOOK10                                                           
         LLC   R0,NDEMOS           YES-MOVE THEM IN                             
         LA    R1,PDEMVALS                                                      
         LA    RF,DEMVALS                                                       
*                                                                               
LOOK9    TM    0(R1),X'80'         TEST OVERRIDE                                
         BZ    *+14                NO-IGNORE THIS DEMO                          
         MVC   0(4,RF),0(R1)       YES                                          
         NI    0(RF),X'7F'         TURN OFF OVERRIDE BIT                        
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,LOOK9                                                         
*                                                                               
LOOK10   OC    SBEMKT,SBEMKT       TEST SPILL LOOKUP                            
         BZ    LOOK12               NO                                          
         CLC   SBEMKT,SBBMKT                                                    
         BE    LOOK12                                                           
*                                                                               
         LLC   R0,NDEMOS           YES-LOOK FOR ANY DEMO VALUES                 
         LA    RE,DEMVALS                                                       
         OC    0(4,RE),0(RE)                                                    
         BNZ   LOOK12                                                           
         LA    RE,4(RE)                                                         
         BCT   R0,*-14                                                          
         B     LOOKN               NO DEMOS - EXCLUDE                           
*                                                                               
LOOK12   L     RE,SPLKDBLK         RE=A(SPGETDEMO DBLOCK)                       
         MVC   DEMFACT,DBDIVSOR-DBLOCK(RE)  EXTRACT DBDIVSOR                    
         B     LOOKY                                                            
*                                                                               
LOOKY    B     EQXIT               CC=EQ                                        
*                                                                               
LOOKN    B     NEQXIT              CC=NE                                        
         DROP  R4,R7                                                            
         POP   USING                                                            
         EJECT                                                                  
* INTERFACE TO SPDEMUP FOR UPGRADE                                              
*                                                                               
         SPACE 1                                                                
UPG      DS    0H                                                               
         OC    SBQUPGRD,SBQUPGRD   TEST UPGRADE PROVIDED                        
         BZ    UPGX                NO                                           
         L     R4,SBAIO1           R4=A(BUY RECORD)                             
         USING BUYREC,R4                                                        
         LA    R5,DEMUPBLK         R5=A(SPDEMUP BLOCK)                          
         USING SPDEMUPD,R5                                                      
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         LA    RE,IOAREA                                                        
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,SBCOMFAC                                                
         MVC   SPUPAGY,SBAGY                                                    
         MVC   SPUPMED,SBQMED                                                   
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BNE   *+10                NO                                           
         MVC   SPUPMED,SBMED       YES - USE SBMED                              
         CLI   SB1WPROF+5,C'I'                                                  
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
         CLI   SB1WPROF+7,C'Y'                                                  
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
         CLI   CANADA,C'Y'                                                      
         BNE   *+16                                                             
         CLI   SBCEXTRA+0,C'U'                                                  
         BE    *+8                                                              
         MVI   SPUPMED,C'C'                                                     
         MVC   SPUPCLI,SBCLT                                                    
         TM    SBINDS,SBIRSOVR     TEST RATING SERVICE OVERRIDE                 
         BO    *+10                                                             
         MVC   SPUPMKT,SBBMKT      NO-PASS AGENCY MARKET                        
         MVC   SPUPSTA,SBSTA                                                    
         CLI   SPUPSTA+4,C' '                                                   
         BE    *+12                                                             
         CLI   SPUPSTA+4,C'N'                                                   
         BNE   *+8                                                              
         MVI   SPUPSTA+4,C'T'                                                   
         MVC   SPUPDAY,BDDAY                                                    
         MVC   SPUPTIM,BDTIMST                                                  
         MVC   SPUPFIL,SBQUPFIL                                                 
         MVI   SPUPSRC,C'N'                                                     
         CLI   SBCPROF+3,C'0'                                                   
         BE    *+8                                                              
         MVI   SPUPSRC,C'A'                                                     
         MVC   SPUPFBK,SBQUPFBK                                                 
         MVC   SPUPTYPE(L'SBQUPGRD),SBQUPGRD                                    
         MVC   SPUPBTYP,BKTYPE                                                  
         CLI   SBQUPPUT,C'1'                                                    
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   SBQUPPUT,C'2'                                                    
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   SBQUPSHR,C'1'                                                    
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   SBQUPSHR,C'2'                                                    
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
*                                                                               
         TM    BDSTAT2,X'04'       TEST FORCE ARB                               
         BZ    *+12                                                             
         MVI   SPUPSRC,C'A'                                                     
         B     UPG3                                                             
         TM    SBINDS,SBIRSOVR     TEST NO RATING SERVICE OVERRIDE              
         BO    UPG3                                                             
         CLI   SPUPSRC,C'A'        AND SRC=ARB                                  
         BNE   UPG3                                                             
         CLI   SPUPMED,C'T'        AND USTV                                     
         BNE   UPG3                                                             
         LA    R1,BDELEM           YES-LOOK FOR BUY ELEMENTS FOR ANY            
         SR    R0,R0                   DATE LATER THAN 26DEC93                  
*                                                                               
UPG1     CLI   0(R1),0                                                          
         BE    UPG3                                                             
         CLI   0(R1),6                                                          
         BL    UPG2                                                             
         CLI   0(R1),13                                                         
         BH    UPG2                                                             
         CLC   2(2,R1),=X'BB9A'                                                 
         BNH   UPG2                                                             
         MVI   SPUPSRC,C'N'        YES-FORCE RATING SERVICE TO NSI              
         B     UPG3                                                             
*                                                                               
UPG2     IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     UPG1                                                             
*                                                                               
UPG3     DS    0H                                                               
         CLI   LPMOPT,C'N'         SUPRESS LPM START DATE?                      
         BE    UPG3C               YES                                          
         OC    SBEMKT,SBEMKT       SPILL MARKET?                                
         BZ    UPG3A               NO, JUST PULL LPM DATE FROM MKT REC          
         CLC   SBEMKT,SBBMKT       SPILL MARKET                                 
         BE    UPG3A               NO, JUST PULL LPM DATE FROM MKT REC          
*                                                                               
         LA    R3,IOAREA           READ REG MKT REC IN HERE                     
         USING MKTRECD,R3                                                       
         LA    R2,SBMKTREC         SPILL MKT RECORD                             
         MVC   MKTKEY(15),0(R2)                                                 
         MVC   MKTKMKT,SBMKT                                                    
         MVC   KEYSAVE(15),MKTKEY  SAVE THE KEY                                 
         GOTO1 VDATAMGR,DMCB,DMRDHI,=C'STATION',IOAREA,IOAREA                   
         CLC   MKTKEY(15),KEYSAVE  FOUND IT?                                    
         BNE   UPG3B               NO                                           
         MVC   SPUPLPM,MKTLPMDT    LPM START DATE                               
         B     UPG3B                                                            
*                                                                               
UPG3A    LA    R3,SBMKTREC                                                      
         MVC   SPUPLPM,MKTLPMDT    LPM START DATE                               
*                                                                               
UPG3B    OI    SPUPOPTS,SPOPEXT                                                 
         MVC   SPUPMALF,MKTALST                                                 
         DROP  R3                                                               
*                                                                               
UPG3C    TM    SBEFLAG4,SBE42DEC   REPORT SUPPORT 2 DEC                         
         BZ    *+8                 NO                                           
         OI    SPUPOPTS,SPOP2DEC   RETURN 2 DECIMAL VALUES                      
         TM    SBEFLAG9,SBE92DEC   REPORT SUPPORTS 2 DEC IMPRESSIONS?           
         BZ    *+8                 NO                                           
         OI    SPUPOPT2,SPOP2IPR   RETURN 2 DECIMAL IMPRESSIONS                 
*                                                                               
         CLI   SPUPSTA,C'0'        CABLE?                                       
         BL    UPG3C1              NO                                           
         MVC   SPUPSYSE,SPUPSTA    SET THE HEADEND                              
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),SBCBLNET REPLACE THE STATION WITH THE NETWORK         
*                                                                               
UPG3C1   GOTO1 VSPDEMUP,DMCB,DEMUPBLK,DEMOS,DEMVALS                             
*                                                                               
         CLI   SPUPSRC,0           DID SPDEMUP DO ANYTHING?                     
         BE    UPGX                NO, REPORT PURCHASED AS PER MEL              
*                                                                               
         TM    SBEFLAG4,SBE42DEC   REPORT SUPPORT 2 DEC                         
         BNZ   *+12                YES                                          
         TM    SBEFLAG9,SBE92DEC   REPORT SUPPORTS 2 DEC IMPRESSIONS?           
         BZ    UPG3E               NO                                           
*                                                                               
         LLC   R0,NDEMOS           R0=COUNTER                                   
         LA    R1,DEMVALS          RE=A(OUTPUT DEMOS)                           
*                                                                               
UPG3D    NI    0(R1),X'3F'         DROP FLAGS FROM VALUE                        
         LA    R1,4(R1)                                                         
         BCT   R0,UPG3D                                                         
*                                                                               
UPG3E    LM    R2,R4,ASPTTAB       MOVE DEMOS TO SPOT TABLE                     
         USING SPTTABD,R2                                                       
         LLC   R7,NDEMOS           R7=N'DEMOS                                   
*                                                                               
UPG4     TM    SPTIND,SPTDUMMY+SPTNODEM  TEST TO AVOID THIS ENTRY               
         BNZ   UPG8                                                             
         LR    R1,R7                                                            
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    DEMVALS(0),DEMVALS  IF UPGRADE DEMOS=0, DON'T OVERWRITE          
         BZ    UPG5                PURCHASED DEMOS STORED IN SPTDEMOS!          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPTDEMOS(0),DEMVALS                                              
UPG5     CLC   SPTSPOTS,=H'1'      TEST FOR ONE SPOT                            
         BE    UPG8                YES-NO NEED TO WEIGHT DEMOS                  
         CLI   SBENOWGT,YES        TEST OPTION TO NOT WEIGHT DEMOS              
         BE    UPG8                                                             
         LR    RF,R7                                                            
         LA    R1,SPTDEMOS         R1=A(DEMO VALUE)                             
*                                                                               
UPG6     L     RE,0(R1)            GET DEMO                                     
         MH    RE,SPTSPOTS         * N'SPOTS                                    
         ST    RE,0(R1)            = WEIGHTED VALUE                             
         LA    R1,4(R1)            NEXT VALUE                                   
         BCT   RF,UPG6                                                          
*                                                                               
UPG8     LA    R2,0(R4,R2)         NEXT SPTTAB ENTRY                            
         BCT   R3,UPG4                                                          
*                                                                               
UPGX     L     R4,SBAIO1           RESTORE A(BUY RECORD)                        
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO REARRANGE THE DEMOS FOR EACH PRODUCT FOR PRD=ALL REQUEST           
*                                                                               
PRDDEMOS DS    0H                                                               
         CLI   SBQBPRD,0           DOUBLE CHECK PRD=ALL                         
         BNE   PDEMX                                                            
         OC    SBEDEMOS,SBEDEMOS   TEST OVERRIDE DEMO LIST                      
         BNZ   PDEMX               YES                                          
         OC    SBAESTTB,SBAESTTB   CHECK EST TABLE AND BUFFER EXIST             
         BZ    PDEMX               NO                                           
         OC    SBAESTBF,SBAESTBF                                                
         BZ    PDEMX               NO                                           
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU                               
         BNZ   PDEMX               YES                                          
         CLI   DEMOS,X'FF'         TEST ANY DEMOS                               
         BE    PDEMX               NO                                           
*                                                                               
         MVI   SVPRD,0                                                          
         MVC   POLDEMOS,DEMOS      SAVE POL DEMO LIST                           
         L     R7,SBACHUNK         R7=A(CHUNKS)                                 
*                                                                               
PDEM2    OC    SCNEXT,SCNEXT       TEST END OF CHUNKS                           
         BZ    PDEMX               YES                                          
         LLC   RE,NDEMOS           TEST ALL DEMO VALUES ARE ZERO                
         SLL   RE,3                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    SCDEMOS(0),SCDEMOS                                               
         BZ    PDEM14              YES-NEXT CHUNK                               
         CLI   SCPRD1,X'FF'        SKIP POL CHUNKS                              
         BE    PDEM14                                                           
         CLC   SCPRD1,SVPRD        TEST CHANGE OF PRODUCT                       
         BNE   *+16                                                             
         CLI   DEMCHG,C'Y'         NO-TEST DEMOS NEED TO BE REARRANGED          
         BE    PDEM10                 YES                                       
         B     PDEM14                 NO                                        
         MVI   DEMCHG,C'N'                                                      
         MVC   SVPRD,SCPRD1        SAVE CURRENT PRODUCT                         
         LA    R1,=X'FF'           IN CASE UNALLOCATED                          
         CLI   SCPRD1,X'FE'        UNALLOCATED?                                 
         BE    PDEM3               YES - PRD SET TO X'FF'                       
         LA    R1,SCPRD1                                                        
*                                                                               
PDEM3    BAS   RE,LGETDEMS         GET PRODUCT'S DEMOS                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LLC   RE,SVNDEMOS                                                      
         MHI   RE,3                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DEMOS(0),POLDEMOS   TEST DEMOS ARE DIFFERENT                     
         BE    PDEM14                                                           
         MVI   DEMCHG,C'Y'         YES-INDICATE DEMOS TO BE REARRANGED          
         XC    DEMDSPLS,DEMDSPLS                                                
         LLC   R0,SVNDEMOS         GET DISPLACEMENTS OF PRODUCT'S               
         LA    R2,DEMDSPLS         DEMOS INTO POL DEMO LIST                     
         LA    R3,DEMOS                                                         
*                                                                               
PDEM4    CLI   0(R3),X'FF'                                                      
         BE    PDEM9                                                            
         LA    R5,POLDEMOS                                                      
         SR    R6,R6                                                            
*                                                                               
PDEM6    CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(3,R3),0(R5)                                                    
         BE    PDEM8                                                            
         LA    R5,3(R5)                                                         
         LA    R6,1(R6)                                                         
         B     PDEM6                                                            
*                                                                               
PDEM8    STC   R6,0(R2)                                                         
         LA    R2,1(R2)                                                         
         LA    R3,3(R3)                                                         
         BCT   R0,PDEM4                                                         
*                                                                               
PDEM9    MVI   0(R2),X'FF'                                                      
*                                                                               
PDEM10   MVC   SVDEMOS,SCDEMOS     SAVE DEMO VALUES                             
         LLC   RE,NDEMOS           CLEAR THE CHUNK DEMO VALUES                  
         SLL   RE,3                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    SCDEMOS(0),SCDEMOS                                               
         LA    R2,DEMDSPLS         SET THE PRODUCT'S DEMO VALUES                
         LA    R3,SCDEMOS          FROM POL DEMO VALUE LIST                     
*                                                                               
PDEM12   CLI   0(R2),X'FF'                                                      
         BE    PDEM14                                                           
         LLC   RE,0(R2)                                                         
         SLL   RE,3                                                             
         LA    RE,SVDEMOS(RE)                                                   
         MVC   0(8,R3),0(RE)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,8(R3)                                                         
         B     PDEM12                                                           
*                                                                               
PDEM14   L     R7,SCNEXT           NEXT CHUNK                                   
         B     PDEM2                                                            
*                                                                               
PDEMX    B     XIT                                                              
         EJECT                                                                  
* ROUTINE THE GET A DEMO LIST USING THE ESTIMATE TABLE AND BUFFER               
* ON ENTRY, R1=A(1-BYTE PRODUCT CODE)                                           
* ON EXIT,  CC EQ AND DEMOS AND WGTLST ARE SET                                  
*           CC NE IF ESTIMATE IS INVALID (DEMOS AND WGTLST NOT SET)             
*                                                                               
LGETDEMS NTR1  ,                                                                
*                                                                               
GETDEMOS DS    0H                                                               
         LLC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,BUYKEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         LA    R1,0(R1,RE)                                                      
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         BZ    NEQXIT                                                           
         BCTR  RE,0                                                             
         MH    RE,=Y(ESTBUFFL)                                                  
         L     R1,SBAESTBF                                                      
         LA    RE,0(R1,RE)                                                      
         USING ESTBUFFD,RE                                                      
         MVC   DEMOS,EBDEMOS                                                    
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU                               
         BZ    *+10                                                             
         MVC   DEMOS,SBPDEMOS      YES-SET DEMOS FROM THERE                     
         MVC   WGTLST,EBWGTLST                                                  
         LA    R0,MAXDEMS          INSERT EOT AFTER DEMO LIST                   
         LA    RE,DEMOS                                                         
         CLI   1(RE),0             TEST FOR EOL                                 
         BE    *+12                NO                                           
         LA    RE,3(RE)                                                         
         BCT   R0,*-12                                                          
         MVI   0(RE),X'FF'                                                      
         B     EQXIT                                                            
         DROP  RE                                                               
         EJECT                                                                  
*===============================================================*               
* SUBROUTINE TO LOOK UP PW PERCENTAGE AND RETURN CLIENT DOLLARS *               
*                                                               *               
*===============================================================*               
         SPACE 1                                                                
GETPW    DS    0H                                                               
*                                                                               
         L     R4,SBAIO1           R4 = A(BUY RECORD)                           
         USING BUYREC,R4                                                        
*                                                                               
GETPW10  DS    0H                                                               
         ICM   R1,15,SBAESTTB      A(ESTTABLE)                                  
         BZ    GETPW11                                                          
         LA    RE,255                                                           
         CLI   BDMASPRD,0        TEST NO MASTER PRODUCT                         
         BE    *+8                YES-USE PRODUCT POL                           
         IC    RE,BDMASPRD                                                      
         TM    BDSTAT2,X'30'       TEST NON-TBS TRADE BUY                       
         BZ    GETPW10A                                                         
         CLI   SBQPRD+2,C'#'       WESTERN TRADE/TRADE PRODUCT                  
         BNE   *+8                                                              
         O     RE,=X'00000080'                                                  
GETPW10A BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,BUYKEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         LA    R1,0(R1,RE)                                                      
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                IF HERE, MAKE SURE REQ IS FOR 1 EST          
         BCTR  RE,0                                                             
         MH    RE,=Y(ESTBUFFL)                                                  
         ICM   R1,15,SBAESTBF                                                   
         BZ    GETPW11                                                          
         LA    RE,0(R1,RE)                                                      
         USING ESTBUFFD,RE                                                      
*                                                                               
         ICM   R7,7,EBUFPCT        GET DEFAULT PERCENTAGE                       
         N     R7,=X'007FFFFF'                                                  
         DROP  RE                                                               
*                                                                               
GETPW11  ZICM  R5,SBAWIPW,3                                                     
         BZ    GETPW24                                                          
         LA    R5,2(R5)            POINT TO DATE/PCT PAIRS                      
         LA    R0,14               MAX NUMBER OF ELEMS                          
*                                                                               
         TM    SBEFLAG,SBEEEST     IS THIS AN E EST?                            
         BNZ   GETPW12A             YES - USE FIRST PCT                         
*                                                                               
GETPW12  CLC   CHUNK+(SCDATE-SCHUNKD)(L'SCDATE),0(R5)     CHNK DATE             
         BNH   GETPW12A            IF NOT HIGH, DONE                            
         LA    R5,L'SBPWWK(R5)                                                  
         BCT   R0,GETPW12                                                       
         B     GETPW24                                                          
                                                                                
GETPW12A ICM   R7,15,2(R5)         PICK UP THE PERCENTAGE                       
*                                                                               
GETPW24  DS    0H                                                               
         XC    PWBLOCK,PWBLOCK                                                  
         LA    R2,PWBLOCK                                                       
         USING PWBLKD,R2                                                        
         MVI   PWACT,PWGETBUY                                                   
         MVC   PWACTBUY,CHUNK+(SCGROSS-SCHUNKD)                                 
         ST    R7,PWPCT                                                         
* IF OVERRIDE $$$, DO NOT TRY TO CALCULATE PW$                                  
         C     R7,=X'80000000'     OVERRIDE?                                    
         BE    GETPWX                                                           
*                                                                               
         TM    SBEFLAG,SBEPWNTX    USE TAX IN PW CALCULATIONS?                  
         BO    *+10                 NOPE, DON'T DO IT!                          
         MVC   PWTAX,CHUNK+(SCTAX-SCHUNKD)                                      
         GOTO1 VPWCALC,DMCB,(R2)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CHUNK+(SCPWGRS-SCHUNKD)(L'SCPWGRS),PWVAL    SV GRS $$            
* COMPUTE PW NET                                                                
         L     R1,PWVAL            GET CLTGRS+TAX                               
         S     R1,PWCLTTAX                                                      
         BAS   RE,WIMNET                                                        
         A     R1,PWCLTTAX                                                      
         ST    R1,CHUNK+(SCPWNET-SCHUNKD)  ROUNDED ANSWER                       
* CLIENT TAX IS ALWAYS TAX ON THE CLIENT NET                                    
         MVC   CHUNK+(SCPWCLTX-SCHUNKD)(L'SCPWCLTX),PWCLTTAX                    
         MVC   CHUNK+(SCPWCTXN-SCHUNKD)(L'SCPWCLTX),PWCLTTAX                    
*                                                                               
GETPWX   DS    0H                                                               
         B     EQXIT                                                            
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* WIMNET: CALCULATE NET $$$ FROM GROSS                                          
* PASS R1 = GROSS $$$                                                           
* RETURNS R1 = NET $$$                                                          
*                                                                               
WIMNET   DS    0H                                                               
         M     R0,=F'170'          GROSS * .85 * 2                              
         D     R0,=F'100'                                                       
         LTR   R1,R1               SEE IF THE ANSWER IS NEGATIVE                
         BM    *+8                 IF IT IS, WE DON'T NEED TO ADD 1             
         AHI   R1,1                (NOT A LA INSTRUCTION)                       
         SRA   R1,1                DIVIDE BY 2                                  
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
AFFINIT  DS    0H                                                               
         XC    THISDEMS(MAXDEMS*8),THISDEMS                                     
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
         LA    R0,IOAREA                                                        
         ST    R0,SPLKAREC                                                      
         MVC   SPLKXTND,SBALKXTD                                                
*                                                                               
         ICM   RE,15,SPLKXTND      POINT TO EXTENSION AREA                      
         USING SPLKXTD,RE                                                       
         CLI   LPMOPT,C'N'         SUPRESS LPM START DATE FROM MKT REC?         
         BNE   *+8                 NO, WE WANT THE LPM START DATE               
         OI    SPXTFLAG,SPXTNLPM   YES, TELL SPGETDEMF TO SUPRESS               
         DROP  RE                                                               
*                                                                               
         MVC   SPLKAFAC,SBCOMFAC   A(COMFACS)                                   
         LA    R0,DEMOS            A(DEMO LIST)                                 
         ST    R0,SPLKALST                                                      
         LA    R0,THISDEMS                                                      
         ST    R0,SPLKAVAL         A(OUTPUT DEMO VALUES)                        
         ST    R4,SPLKABUY         A(BUY RECORD)                                
         USING BUYREC,R4                                                        
         LA    R0,WGTLST                                                        
         ST    R0,SPLKAWGT                                                      
*                                                                               
**NOP         CLI   SBD0PROF+7,C'J'     USE OLY EXCLUSION (JUL)                 
**NOP         BE    *+8                                                         
         CLI   SBMED,C'R'          NO OLYMPIC EXCLUSION FOR RADIO!              
         BE    AFFIN2                                                           
         CLI   SBD0PROF+7,C'F'     USE OLY EXCLUSION (FEB)                      
         BE    *+8                                                              
         CLI   SBD0PROF+7,C'Y'     USE ANY OLY EXCLUSION                        
         BNE   *+8                                                              
         OI    SPLKOPT,SPLKOEXO                                                 
*                                                                               
AFFIN2   OI    SPLKOPT,SPLKOAFD    AFFIDAVIT LOOKUP OPTION                      
         TM    SBEFLAG4,SBE42DEC   REPORT SUPPORT 2 DEC                         
         BZ    *+8                                                              
         OI    SPLKOPT,SPLKOP2D                                                 
         TM    SBEFLAG9,SBE92DEC   REPORT SUPPORTS 2 DEC IMPRESSIONS?           
         BZ    AFFIN3              NO                                           
         ICM   RE,15,SPLKXTND      POINT TO EXTENSION AREA                      
         USING SPLKXTD,RE          DEMO EXTENSION AREA DSECT                    
         OI    SPXTFLG2,SPXTOP2I   REPORT 2 DECIMAL IMPRESSIONS                 
         DROP  RE                  DROP DEMO EXTENSION USING                    
*                                                                               
AFFIN3   CLI   SBD0PROF+4,C'Y'     LOWER QH ON BREAKS FEATURE                   
         BNE   *+8                                                              
         OI    SPLKOPT,SPLKOLOW                                                 
         MVC   SPLKFIL(3),=C'TTN'                                               
         CLI   SBCPROF+3,C'0'      TEST FOR NSI                                 
         BE    *+8                                                              
         MVI   SPLKSRC,C'A'                                                     
*                                                                               
         OC    SBAXSPIL,SBAXSPIL   TEST CANADIAN SPILL LOOKUP                   
         BNZ   AFFIN10                                                          
         CLI   CANADA,C'Y'         TEST CANADIAN AGENCY                         
         BNE   AFFIN20                                                          
         CLI   SBCEXTRA+0,C'U'     TEST US DEMO LOOKUP                          
         BE    AFFIN20                                                          
*                                                                               
AFFIN10  MVI   SPLKMED,C'C'                                                     
*                                                                               
AFFIN20  DS    0H                                                               
         LA    RF,SBQMED           MEDIA                                        
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BNE   *+8                 NO                                           
         LA    RF,SBMED            YES - CURRENT MEDIA                          
         CLI   0(RF),C'R'                                                       
         BNE   *+8                                                              
         MVI   SPLKMED,C'R'                                                     
*                                                                               
         MVC   SPLKUID,SBUSERID                                                 
         MVC   SPLKAGY,SBAGY                                                    
         MVC   SPLKCLI,SBCLT                                                    
         MVC   SPLKSTA,SBSTA                                                    
         CLI   SPLKSTA+4,C' '                                                   
         BE    *+12                                                             
         CLI   SPLKSTA+4,C'N'                                                   
         BNE   *+8                                                              
         MVI   SPLKSTA+4,C'T'                                                   
         BRAS  RE,GETNTI                                                        
***                                                                             
* CANADIAN SOFT DEMOS SUPPORT                                                   
***                                                                             
         CLI   CANADA,C'Y'         CANADIAN AGENCY?                             
         BNE   AFFIN30             NO                                           
*                                                                               
         CLI   CDEMSTA,C'A'        HAVE STATION OVERRIDE?                       
         BL    *+10                NO                                           
         MVC   SPLKSTA,CDEMSTA                                                  
*                                                                               
         MVI   SPLKSRC,C'N'        INIT TO NSI                                  
         TM    CDEMFLG,X'01'       HAVE BBM                                     
         BZ    *+8                 NO                                           
         MVI   SPLKSRC,C'A'        YES - SET TO BBM/ARB                         
*                                                                               
         ICM   R1,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    AFFIN30             NO                                           
         USING SPLKXTD,R1                                                       
         OI    SPXTFLAG,SPXTCDEM   DO NOT GO THROUGH OLD SOFT DEMO CODE         
         MVI   SPXTSVFL,0          WE MUST CLEAR IT HERE!                       
         TM    CDEMFLG,X'80'       SUPPRESS IMPS?                               
         BZ    AFFIN30             NO                                           
         MVI   SPXTSVFL,X'80'      SET TO SUPPRESS IMPS                         
         DROP  R1                                                               
*                                                                               
AFFIN30  TM    SBINDS,SBIRSOVR     TEST RATING SERVICE OVERRIDE                 
         BZ    *+16                                                             
         OI    SPLKOPT,SPLKOARB    YES-FORCE ARB IF NEED BE                     
         TM    SBINDS,SBIAGYMK     TEST TO PASS AGY MKT NUM                     
         BZ    *+10                                                             
         MVC   SPLKUMK,SBBMKT                                                   
         MVC   SPLKBTYP,BKTYPE     SPECIAL BOOK TYPE                            
***                                                                             
* IN CASE THERE IS A RATING SERVICE OVERRIDE AND SBIAGYMK IS NOT SET            
* WE STILL NEED TO SET THE MARKET FOR MEDIA T TO GET LPM DATA                   
***                                                                             
         CLI   SBMED,C'T'          MEDIA T?                                     
         BNE   *+10                NO                                           
         MVC   SPLKUMK,SBBMKT      YES - SET THE USER MARKET                    
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,2            GET DEMO ELEMENT                             
         MVI   ELCDHI,3                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   SPLKDBK,NDBOOK-NDELEM(R6)                                        
         OC    SPLKDBK,SPLKDBK     TEST LATEST BOOK REQUEST                     
         BNZ   *+10                                                             
         MVC   SPLKDBK,BDEND       YES - SET LATEST BOOK LIMIT                  
*                                                                               
         CLI   SBBKAVG,0           BOOK AVERAGING?                              
         BE    AFFINX               NO                                          
         MVI   SPLKDBK,X'FF'                                                    
         MVC   SPLKDBK+1(1),SBBKAVG                                             
*                                                                               
AFFINX   B     EQXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET THE TALENT FACTOR                                          
*                                                                               
GETTAL   DS    0H                                                               
         MVC   OPTAGY,SBAGY        SET OPTIMIZATION VALUES                      
         MVC   OPTMED,SBMED                                                     
         MVC   OPTCLT,SBBCLT                                                    
         MVC   OPTSTDT,SBBQSTP                                                  
         MVC   OPTENDDT,SBBQENDP                                                
         CLC   OPTVALS,SBTALOPT    TEST FOR CHANGE IN LOOKUP VALUES             
         BE    GETTALX             NO-EXIT                                      
*                                                                               
         MVC   SBTALOPT,OPTVALS    SET NEW OPTIMIZATION VALUES                  
         XC    SBTADATE,SBTADATE                                                
         MVC   SBTALMUL,=F'1'                                                   
         MVC   SBTALDIV,=F'1'                                                   
*                                                                               
         LA    R0,(SBTALFX-SBTALF10)/4  NUMBER OF ACCUMS                        
         LA    R1,SBTALF10                                                      
         MVC   0(4,R1),=F'1'                                                    
         LA    R1,4(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING TALKEY,R6                                                        
         MVC   TALKTYP,=X'0D27'                                                 
         MVC   TALKAGMD,SBBAGYMD                                                
         MVC   TALKCLT,SBBCLT                                                   
         MVC   TALKDATE,SBQSTART   REQUEST START DATE (EBCDIC)                  
         MVC   KEYSAVE,KEY         SAVE THE KEY BEFORE READ                     
         GOTO1 VDATAMGR,DMCB,DMRDHI,SPTDIR,KEY,KEY                              
         CLC   TALKEY(TALKDATE-TALKEY),KEYSAVE                                  
         BNE   GETTALX             DID NOT FIND ANY                             
         MVC   KEYSAVE,KEY         SAVE ACTUAL KEY                              
*                                                                               
GETTAL2  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,GETREC,SPTFILE,KEY+14,IOAREA,DMWORK                
         LA    R6,IOAREA                                                        
         CLI   TALEL05,X'05'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LLC   RE,KEY+11           GET TALENT FACTOR GROUP                      
         SLL   RE,3                X 8                                          
         LA    RF,SBTALF10                                                      
         OC    SBTADATE,SBTADATE   TEST PASS 1 OR PASS 2                        
         BZ    *+8                                                              
         LA    RF,SBTALF20                                                      
         AR    RF,RE               POINT TO SLOT                                
         MVC   0(8,RF),TAL05MUL    EXTRACT FACTOR AND DIVISOR                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRSEQ,SPTDIR,KEY,KEY                              
         CLC   KEY(TALKFGRP-TALKEY),KEYSAVE                                     
         BE    GETTAL2                                                          
         CLC   KEY(TALKDATE-TALKEY),KEYSAVE   STILL RIGHT BASE KEY              
         BNE   GETTALX                                                          
*                                                                               
         CLC   TALKDATE,SBQEND      LAST RECORD DATE BEFORE REQUEST END         
         BH    GETTALX              NO-ONLY ONE SET OF FACTORS                  
         GOTO1 VADDAY,DMCB,TALKDATE,DUB,1    SET EFFECTIVE DATE FOR 2ND         
         GOTO1 VDATCON,(R1),DUB,(2,SBTADATE) SET OF TALENT FACTORS              
         MVC   KEYSAVE,KEY         !!!                                          
         B     GETTAL2                                                          
*                                                                               
GETTALX  B     XIT                                                              
         DROP  R6                                                               
***********************************************************************         
* CONVERT THE 3 BYTE CABLE STATIONS TO 4 BYTE CABLE STATIONS FOR DEMO *         
* CALLS.  ALSO SET THE SYSCODE                                        *         
***********************************************************************         
*                                                                               
FUSION   DS    0H                                                               
*                                                                               
         LM    R2,R3,0(R1)         R2=EBCIDIC STATION, R3=SYSCODE               
*                                                                               
         CLI   0(R2),C'0'          CABLE?                                       
         BL    FUSX                NO                                           
*                                                                               
         PACK  DUB,0(4,R2)         PACK THE CABLE STATION                       
         CVB   R0,DUB                                                           
         CHI   R0,7000             IF BETWEEN 7000-7500                         
         BL    FUS05               THEN READ MASTER RECORD                      
         CHI   R0,7500                                                          
         BNL   FUS05                                                            
***                                                                             
* READ MASTER RECORD FOR DEMO OVERRIDE SYSCODE                                  
***                                                                             
         LA    R6,IOAREA           BUILD KEY OF STATION RECORD                  
         USING STARECD,R6                                                       
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVI   STAKMED,C'T'                                                     
         MVC   STAKCALL(4),0(R2)                                                
         MVI   STAKCALL+4,C'T'     NEED TO APPEND A T                           
         MVC   STAKAGY,SBAGYREC+1  AGENCY CODE                                  
         MVC   STAKCLT,=C'000'     READ AGENCY RECORD ONLY                      
*                                                                               
         MVC   KEYSAVE(12),STAKEY  SAVE THE KEY                                 
         GOTO1 VDATAMGR,DMCB,DMRDHI,=C'STATION',IOAREA,IOAREA                   
         CLC   STAKEY(12),KEYSAVE  FOUND IT?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R0,3,SCBLLKUP                                                    
         BZ    FUSX                                                             
         DROP  R6                                                               
*                                                                               
FUS05    XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'X'        TRANSLATE 3CHAR NET TO 4CHAR                 
         MVC   STAPACOM,SBCOMFAC                                                
         MVC   STAPQNET,SBCBLNET                                                
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   STAPQSTA,C' '       FOUND 4 CHAR NET?                            
         BE    FUSX                NO                                           
         MVC   0(4,R2),STAPQSTA    MOVE 4 CHAR NETWORK CODE                     
         MVI   4(R2),C'T'                                                       
         STCM  R0,3,0(R3)          SYSCODE                                      
         DROP  R1                                                               
*                                                                               
FUSX     B     XIT                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*  CONSTANTS                                                                    
DMRDHI   DC    C'DMRDHI'                                                        
DMRSEQ   DC    C'DMRSEQ'                                                        
GETREC   DC    C'GETREC'                                                        
SPTDIR   DC    C'SPTDIR'                                                        
SPTFILE  DC    C'SPTFILE'                                                       
         DROP  RA,RB,RC                                                         
         EJECT                                                                  
***********************************************************************         
* GETAMKT - GET CANADIAN ALPHA MKT                                    *         
***********************************************************************         
         SPACE                                                                  
GETAMKT  NTR1  BASE=*,LABEL=*                                                   
         XC    ALPHAMKT,ALPHAMKT                                                
         CLI   CANADA,C'Y'         TEST CANADIAN                                
         BNE   GAMKX                                                            
         TM    BUYKAM,X'03'        TEST NETWORK                                 
         BNO   GAMKX                                                            
         CLI   BUYKMSTA+4,X'B0'    POSSIBLE CANADIAN MARKET SUFFIX              
         BL    GAMKX                                                            
         LA    RE,SUFXTAB          POINT TO THE CONVERSION TABLE                
         LHI   RF,SUFXTABN                                                      
*                                                                               
GAMK10   CLC   BUYKMSTA+4(1),2(RE) MATCH THE SUFFIX                             
         BE    GAMK20                                                           
         LA    RE,L'SUFXTAB(RE)                                                 
         BCT   RF,GAMK10                                                        
         B     GAMKX               GIVE UP ON ALPHA MKT IF NOT THERE            
*                                                                               
GAMK20   MVC   ALPHAMKT,3(RE)                                                   
*                                                                               
GAMKX    J     EXIT                                                             
*                                                                               
         LTORG                                                                  
       ++INCLUDE SPCNCBLTAB                                                     
         EJECT                                                                  
*                                                                               
GETUNIV  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   RF,15,SBUNVDEM      A(UNIVERSE FOR FIRST 8 DEMOS)                
         BZ    GETUNVX             IF NOT SET, EXIT                             
         XC    0(32,RF),0(RF)      CLEAR UNIVERSE DEMOS                         
*                                                                               
         XC    DEMOS,DEMOS         CLEAR DEMOS                                  
         MVC   DEMOS(24),SBPDEMOS  DEMOS FROM DEMO OR MENU OPTION               
         OC    SBPDEMOS,SBPDEMOS   DEMO OR MENU OPTION USED?                    
         BNZ   GETU10              YES                                          
*                                                                               
         LLC   RE,SBBPRD           BINARY PRODUCT                               
         BCTR  RE,0                -1 FOR INDEXING                              
         SLL   RE,8                X 256                                        
         LLC   RF,SBBEST           BINARY ESTIMATE                              
         BCTR  RF,0                -1 FOR INDEXING                              
         AR    RE,RF               DISPLACEMENT INTO ESTIMATE TABLE             
*                                                                               
         ICM   R1,15,SBAESTTB      HAVE A(ESTIMATE TABLE)?                      
         BZ    GETUNVX             NO - DONE                                    
         AR    R1,RE               INDEX INTO ESTIMATE TABLE                    
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,1,0(R1)          IS ESTIMATE ACTIVE?                          
         BZ    GETUNVX             NO - DONE                                    
         BCTR  RE,0                -1 FOR INDEXING                              
         MH    RE,=Y(ESTBUFFL)     DISPLACEMENT INTO ESTIMATE BUFFER            
         ICM   R1,15,SBAESTBF      HAVE A(ESTIMATE BUFFER)?                     
         BZ    GETUNVX             NO - DONE                                    
         AR    RE,R1               INDEX INTO ESTIMATE BUFFER                   
         USING ESTBUFFD,RE         ESTIMATE BUFFER DSECT                        
         MVC   DEMOS(24),EBDEMOS   FIRST 8 ESTIMATE DEMOS                       
         OC    DEMOS(24),DEMOS     DO WE HAVE ANY DEMOS?                        
         BZ    GETUNVX             NO - DONE                                    
         DROP  RE                  DROP ESTIMATE BUFFER USING                   
*                                                                               
GETU10   LA    R0,8                MAX 8 DEMOS FOR UDEM1-8                      
         LA    RE,DEMOS            DEMO LIST                                    
*                                                                               
GETU15   CLI   1(RE),0             END OF DEMO LIST?                            
         BE    GETU20              YES                                          
         MVI   1(RE),C'U'          CHANGE DEMO CATEGORY TO UNIVERSE             
         CLI   2(RE),0             COMSCORE DEMO?                               
         BNE   *+10                NO                                           
         XC    0(3,RE),0(RE)       YES - CLEAR DEMOS                            
         LA    RE,3(RE)            BUMP TO NEXT DEMO                            
         BCT   R0,GETU15           PROCESS NEXT DEMO                            
*                                                                               
GETU20   MVI   0(RE),X'FF'         INSERT X'FF' AFTER DEMO LIST                 
         MVI   PASS,1              PASS 1                                       
*                                                                               
GETU25   LA    RE,THISDEMS         RE = THISDEMS                                
         XC    0(8*8,RE),0(RE)     CLEAR THISDEMS FOR 8 DOUBLE WORDS            
         LA    RE,DEMVALS          RE = DEMVALS                                 
         XC    0(8*4,RE),0(RE)     CLEAR DEMVALS FOR 8 FULL WORDS               
         XC    SPDEMLK,SPDEMLK     CLEAR SPDEMLK                                
         LA    R0,IOAREA           IOAREA FOR SPGETDEMF                         
         ST    R0,SPLKAREC         SPLKAREC WILL USE IOAREA                     
         MVC   SPLKAFAC,SBCOMFAC   A(COMFACS)                                   
         LA    R0,DEMOS            A(DEMO LIST)                                 
         ST    R0,SPLKALST         SPLKALST = DEMO LIST                         
         LA    R0,THISDEMS         A(DEMO VALUES)                               
         ST    R0,SPLKAVAL         SPLKAVAL = DEMO VALUES                       
         MVI   SPLKFIL,C'T'        FILE CODE (T=T/P,P=PAV)                      
*                                                                               
         MVC   SPLKAGY,SBAGY       AGENCY                                       
         MVC   SPLKDBK,SBQUNBK     UNIVERSE BOOK                                
         MVC   SPLKBTYP,SBQBKTYP   SPECIAL BOOK TYPE                            
         MVC   SPLKSTA,SBSTA       STATION                                      
         CLI   SPLKSTA+4,C' '      HAVE 5TH CHARACTER?                          
         BE    *+12                NO - MAKE IT A "T"                           
         CLI   SPLKSTA+4,C'N'      NETWORK?                                     
         BNE   *+8                 NO                                           
*                                                                               
         MVI   SPLKMED,C'T'        MEDIA CODE T                                 
         MVI   SPLKSRC,C'N'        INIT SOURCE CODE (N=NSI)                     
         CLI   SBCPROF+3,C'0'      CLIENT PROFILE INDICATES NSI?                
         BE    GETU32              YES                                          
         MVI   SPLKMED,C'R'        MEDIA CODE R                                 
         MVI   SPLKSRC,C'A'        NO - SET IT TO A=ARB                         
         MVI   SPLKSTA+4,C'A'      START WITH "AM"                              
         CLI   PASS,1              PASS 1?                                      
         BE    *+8                 YES                                          
         MVI   SPLKSTA+4,C'F'      CHECKING "FM"                                
*                                                                               
         LA    RF,SBMKTREC         MARKET RECORD                                
         USING MKTRECD,RF          MARKET RECORD DSECT                          
         MVC   SPLKALF,MKTALF      ALPHA MARKET                                 
         DROP  RF                  DROP MARKET RECORD USING                     
*                                                                               
GETU32   MVI   SPLKDAY,X'40'       MONDAY                                       
         MVC   SPLKSTIM,=H'1700'   5PM                                          
         MVC   SPLKETIM,=H'1715'   515P                                         
         MVC   SPLKSVI,SBESVI      SVI LOOK-UP OPTION                           
         MVC   SPLKUID,SBUSERID    USER-ID                                      
         OI    SPLKOPT2,SPLKOPT2_NLSN  NEED THIS IF REPORT IS COMSCORE          
         LA    R0,SB1WPROF         1W PROFILE                                   
         ST    R0,SPLKA1W          A(1W PROFILE)                                
*                                                                               
         MVC   SPLKXTND,SBALKXTD   SET EXTENSION AREA                           
         ICM   R4,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    GETUNVX             NO                                           
         USING SPLKXTD,R4          EXTENSION AREA DSECT                         
         OI    SPXTFLG2,SPXTUNIV   UNIVERSE LOOK-UP                             
         DROP  R4                                                               
*                                                                               
         GOTO1 VSPGETDM,DMCB,(X'FF',SPDEMLK)                                    
*                                                                               
         XR    R0,R0               CLEAR R0                                     
         ICM   R0,1,SBENDEM        NUMBER OF DEMOS = 0?                         
         BZ    GETUNVX             YES - EXIT                                   
         CHI   R0,8                NUMBER OF DEMOS <= 8?                        
         BNH   *+8                 YES                                          
         LA    R0,8                NO - SET TO 8 DEMOS                          
         ICM   RE,15,SBUNVDEM      RE = A(OUTPUT MKT UNIVERSE)                  
         LA    R5,THISDEMS         R5 = A(UNIVERSE FROM SPGETDEMF)              
*                                                                               
GETU35   MVC   0(4,RE),0(R5)       MARKET UNIVERSE                              
         LA    RE,4(RE)            BUMP SBUNVDEM                                
         LA    R5,8(R5)            BUMP THISDEMS                                
         BCT   R0,GETU35           PROCESS NEXT DEMO                            
*                                                                               
         CLI   SPLKMED,C'R'        MEDIA R?                                     
         BNE   GETUNVX             NO                                           
         ICM   RE,15,SBUNVDEM      RE = A(OUTPUT MKT UNIVERSE)                  
         OC    0(32,RE),0(RE)      HAVE ANY DEMO DATA?                          
         BNZ   GETUNVX             YES                                          
         CLI   PASS,2              ALREADY DID PASS 2?                          
         BE    GETUNVX             YES                                          
         MVI   PASS,2              TRY "FM"                                     
         B     GETU25              CALL SPGETDEMF AGAIN WITH "FM"               
*                                                                               
GETUNVX  J     EXIT                DONE                                         
*                                                                               
         LTORG                                                                  
*                                                                               
GETNTI   NTR1  BASE=*,LABEL=*                                                   
*        CLI   SBECS,YES           TEST EXTRACTING CHILD SPOT                   
*        BNE   GETNTIX              NO                                          
         ICM   R1,7,SBASTANT                                                    
         BZ    GETNTIX                                                          
         USING STABUFFD,R1                                                      
         CLI   STBTYPE,C'C'        CABLE STATION?                               
         BNE   GETNTIX             NO                                           
         MVC   SPLKNTI,STBNTI      MOVE IN THE NTI CODE                         
GETNTIX  J     EXIT                                                             
         DROP  R1                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
* ROUTINE TO DEAL WITH UNREPORTED SPOTS                                         
* POST BUY DEMO OVERRIDES ARE USED IF THEY EXIST                                
* SPILL SPOTS ARE EXCLUDED IF THE PROFILE OPTION IS SET                         
*                                                                               
UNRPSPTS NTR1  BASE=*,LABEL=*                                                   
         MVI   SPILL,NO                                                         
         OC    SBEMKT,SBEMKT       TEST SPILL MARKET                            
         BZ    URSP1                                                            
         CLC   SBEMKT,SBBMKT                                                    
         BE    URSP1                                                            
         MVI   SPILL,YES                                                        
*                                                                               
URSP1    CLI   PBDEMO,YES          TEST POST BUY DEMO OVERRIDES                 
         BE    *+12                                                             
         CLI   SPILL,YES           NO-TEST SPILL                                
         BNE   URSPX               NO-DEMO VALUES OK SO FAR                     
         LM    R2,R4,ASPTTAB                                                    
         USING SPTTABD,R2                                                       
*                                                                               
URSP2    OC    SPTSPOTS,SPTSPOTS   TEST ZERO SPOTS                              
         BZ    URSP10              YES-IGNORE                                   
         TM    SPTIND,SPTNODEM     TEST DEMO LOOKUP SUPPRESSED                  
         BO    URSP10              YES-IGNORE                                   
         OC    SPTACTBK,SPTACTBK   TEST RERATE BOOK EXISTS                      
         BNZ   URSP10              YES-DEMOS ALREADY OK                         
         CLI   PBDEMO,YES          NO-TEST POST BUY DEMO OVERRIDES              
         BE    URSP4                                                            
         CLI   SBD0PROF+1,C'Y'     NO-TEST XCLUDE UNREPORTED SPILL SPTS         
         BNE   URSP10                                                           
         OI    SPTIND,SPTDUMMY     YES-EXCLUDE THIS SPOT                        
         B     URSP10                                                           
*                                                                               
URSP4    LLC   R0,NDEMOS           MOVE IN POST BUY DEMO OVERRIDES              
         LA    R1,SPTDEMOS                                                      
         LA    RE,PDEMVALS                                                      
*                                                                               
URSP6    TM    0(RE),X'80'         TEST OVERRIDE                                
         BZ    URSP7                NO                                          
*                                                                               
         L     RF,0(RE)            GET DEMO                                     
         MH    RF,SPTSPOTS         * N'SPOTS                                    
         ST    RF,0(R1)            = WEIGHTED VALUE                             
*                                                                               
*        MVC   0(4,R1),0(RE)       YES-USE IT                                   
         NI    0(R1),X'7F'                                                      
***      MVC   ADJDEMO,0(R1)       ADJUST THE PRECISION!                        
***      LR    R6,RE               SAVE OFF RE                                  
***      BRAS  RE,ADJPREC                                                       
***      LR    RE,R6               RESTORE RE                                   
***      MVC   0(4,R1),ADJDEMO     ADJUSTED FOR 2 DECIMAL PRECISION             
         B     URSP8                                                            
*                                  NO POST BUY DEMO OVERRIDE-                   
URSP7    CLI   SPILL,YES           TEST SPILL                                   
         BNE   URSP8               NO-LEAVE THE PURCHASED VALUE                 
         CLI   SBD0PROF+1,C'Y'     YES-TEST EXCLUDE UNREPORTED SPOTS            
         BNE   URSP8               NO-LEAVE THE PURCHASED VALUE                 
         XC    0(4,R1),0(R1)       YES-CLEAR THE DEMO VALUE                     
*                                                                               
URSP8    LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,URSP6                                                         
*                                                                               
URSP10   LA    R2,0(R4,R2)                                                      
         BCT   R3,URSP2                                                         
*                                                                               
URSPX    J     EXIT                                                             
         TITLE 'SPOTBUY - SPOTPAK EXTRACT MODULE - SQAD'                        
***********************************************************************         
*                                                                     *         
*        SUB-ROUTINE TO FIND SQAD CPP FOR DEMOS IN LIST               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SQAD     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
*                                                                               
         XC    DEMVALS(4*MAXDEMS),DEMVALS     INIT DEMO VALUES AREA             
*                                                                               
         L     R3,SBSQDBFA         POINT TO SQAD BUFFER                         
         USING SQDBUFFD,R3         ESTABLISH BUFFER                             
*                                                                               
*        BUILD SQD BUFFER KEY                                                   
*                                                                               
         XC    SQDENTKY(SQDKEYL),SQDENTKY INIT SQD BUFFER KEY                   
*                                                                               
         LA    RF,SBMKTREC         POINT TO SAVED MARKET RECORD                 
*                                                                               
         MVC   SQDALF,MKTALST-MKTREC(RF)  SQAD MKT NUM IS ALPHA MKT             
*                                                                               
*        TRANSLATE BUY DAYPART CODE TO SQAD DPT CODE                            
*                                                                               
         L     R4,SBAIO1           R4 = A(BUY RECORD)                           
         USING BUYREC,R4                                                        
*                                                                               
         LA    RF,SQDDPTTB         POINT TO DAYPART TRANSLATION TABLE           
*                                  CL1(DPTMENU),CL1(DPT),CL2(SQAD DPT)          
*                                                                               
         OC    0(SQDDPTL,RF),0(RF) SKIP IF EOT REACHED                          
         BZ    SQADX                                                            
         CLC   SBDPTMEN,SQDDPMNU-SQDDPTTB(RF) MATCH CURRENT DPT MENU            
         BNE   *+10                                                             
         CLC   BDDAYPT,SQDDPTAG-SQDDPTTB(RF) MATCH BUY'S DPT                    
         BE    *+12                                                             
         LA    RF,SQDDPTL(RF)      BUMP TO NEXT ENTRY                           
         B     *-34                                                             
*                                                                               
         MVC   SQDDPTCD,SQDDPTSQ-SQDDPTTB(RF) SET SQAD DPT CODE                 
*                                                                               
*        DETERMINE SQAD OPTION TO USE                                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,SBSQDOPT       GET CURRENT SQAD OPTION NUMBER               
         BNZ   *+8                                                              
         LA    RE,1                ZERO DEFAULTS TO 1                           
*                                                                               
         BCTR  RE,0                DECREMENT FOR INDEXING                       
         MHI   RE,SBSQDOPL         CALUCULATE INDEX                             
         CHI   RE,8                SQAD 3/4 OPTION?                             
         BL    SQAD01              NO                                           
         SHI   RE,8                                                             
         LA    RE,SBSQDQT3(RE)     POINT TO CORRECT SQAD OPTION                 
         B     SQAD02                                                           
*                                                                               
SQAD01   LA    RE,SBSQDQT1(RE)     POINT TO SQAD OPTION                         
*                                                                               
SQAD02   MVC   SQDQTR,0(RE)        SET QUARTER FOR LOOK UP                      
         MVC   SQDREL,2(RE)        SET RELEASE MONTH                            
*                                                                               
*                                                                               
*        MODIFY DEMO LIST                                                       
*                                                                               
*        DETERMINE TYPE OF SQAD CPP'S WANTED                                    
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         CLI   SBEDEMTY,C'1'       AVERAGE                                      
         BNE   *+8                                                              
         LA    RF,C'A'                                                          
         CLI   SBEDEMTY,C'2'       HIGH                                         
         BNE   *+8                                                              
         LA    RF,C'H'                                                          
         CLI   SBEDEMTY,C'3'       LOW                                          
         BNE   *+8                                                              
         LA    RF,C'L'                                                          
         CLI   SBEDEMTY,C'4'       CPM TSA LOW?                                 
         BNE   *+8                 NO                                           
         LA    RF,C'N'             YES - SET TO 'N'                             
         CLI   SBEDEMTY,C'5'       CPM TSA AVERAGE?                             
         BNE   *+8                 NO                                           
         LA    RF,C'C'             YES - SET TO 'C'                             
         CLI   SBEDEMTY,C'6'       CPM TSA HIGH?                                
         BNE   *+8                 NO                                           
         LA    RF,C'J'             YES - SET TO 'J'                             
         CLI   SBEDEMTY,C'7'       CPM DMA LOW?                                 
         BNE   *+8                 NO                                           
         LA    RF,C'M'             YES - SET TO 'M'                             
         CLI   SBEDEMTY,C'8'       CPM DMA AVERAGE?                             
         BNE   *+8                 NO                                           
         LA    RF,C'B'             YES - SET TO 'B'                             
         CLI   SBEDEMTY,C'9'       CPM DMA HIGH?                                
         BNE   *+8                 NO                                           
         LA    RF,C'I'             YES - SET TO 'I'                             
*                                                                               
         STC   RF,SQDCPPTP         SET CCP TYPE                                 
*                                                                               
         MVC   SQDDEMOS,DEMOS      COPY DEMO LIST                               
*                                                                               
         LA    R1,SQDBSRP          POINT TO BINSRCH PARMS                       
         USING BSPPRMSD,R1         ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         OC    SQDBSRP,SQDBSRP     SKIP IF TABLE ALREADY INITIALIZED            
         BNZ   SQADBSRX                                                         
*                                                                               
         LA    RF,SQDTBL           SET TABLE START                              
         ST    RF,BSPATAB                                                       
*                                                                               
         LA    RF,SQDENTL          SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         LA    RF,SQDKEYL          SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
*                                                                               
         LHI   RF,SQDMAXQ          MAX ENTRIES                                  
         ST    RF,BSPMAX                                                        
*                                                                               
SQADBSRX DS    0H                                                               
*                                                                               
         GOTO1 SBABINSR,BSPPRMS,('BSPFIND',SQDENTKY) FIND SQD DEMOS             
*                                                                               
         CLI   BSPCNTL,0           OKAY IF NO  ERRORS                           
         BE    SQAD10                                                           
         CLI   BSPCNTL,BSPNF       OKAY IF NOT FOUND                            
         BE    *+6                                                              
         DC    H'0'                ELSE DIE                                     
*                                                                               
         MVC   SVSQDKEY,SQDENTKY   SAVE CURRENT KEY                             
*                                                                               
         BRAS  RE,SQDGET           GET SQAD CPP'S FOR MARKET                    
*                                                                               
*        RE-SEARCH THE TABLE                                                    
*                                                                               
         MVC   SQDENTKY(SQDKEYL),SVSQDKEY   RESTORE CURRENT KEY                 
*                                                                               
         GOTO1 SBABINSR,BSPPRMS,('BSPFIND',SQDENTKY) FIND SQD DEMOS             
         CLI   BSPCNTL,0           OKAY IF NO  ERRORS                           
         BE    SQAD10                                                           
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    *+6                                                              
         DC    H'0'                ELSE DIE                                     
*                                                                               
         XC    SQDENTKY(SQDKEYL),SQDENTKY   KILL KEY                            
         B     SQADX                                                            
*                                                                               
SQAD10   DS    0H                                                               
*                                                                               
         ICM   R5,15,BSPAREC       POINT TO FOUND BUFFER ELEMENT                
         BZ    SQAD20              NONE FOUND                                   
*                                                                               
         USING SQDENTKY,R5         ESTABLISH SQUAD TABLE ENTRY                  
*                                                                               
         MVC   DEMVALS(4*MAXDEMS),SQDCPPS     RETURN CPP'S                      
         LLC   R0,NDEMOS           R0=COUNTER                                   
         LA    R1,DEMVALS          RE=A(OUTPUT DEMOS)                           
*                                                                               
SQAD12   MVC   ADJDEMO,0(R1)                                                    
**NOP    BRAS  RE,ADJPREC          I DON'T THINK SQAD NEEDS IT                  
         MVC   0(4,R1),ADJDEMO                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,SQAD12                                                        
*                                                                               
*                                                                               
         DROP  R1,R3,R5                                                         
*                                                                               
SQAD20   DS    0H                                                               
*                                                                               
*        ADD CPP'S TO SPOT TABLE ENTRY                                          
*                                                                               
         LM    R2,R4,ASPTTAB       R2=A(SPTTAB),R3=N'NTRYS,R4=L'NTRY            
         USING SPTTABD,R2                                                       
*                                                                               
         LLC   R7,NDEMOS           R7=N'DEMOS                                   
*                                                                               
SQADSPLP DS    0H                                                               
*                                                                               
         TM    SPTIND,SPTNODEM     MISSED/MAKEGOOD SPOT?                        
         BNZ   SQADSPCN            YES-IGNORE                                   
*                                                                               
         LR    R1,R7               NUMBER OF DEMOS                              
         SLL   R1,2                * 4 FOR BUCKET WIDTH                         
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SPTDEMOS(0),DEMVALS SAVE DEMOS (ACTUALLY CPP'S)                  
*                                                                               
SQADSPCN DS    0H                                                               
*                                                                               
         LA    R2,0(R4,R2)         NEXT SPTTAB ENTRY                            
         BCT   R3,SQADSPLP                                                      
*                                                                               
SQADSPDN DS    0H                                                               
*                                                                               
SQADDEMX DS    0H                                                               
*                                                                               
SQADX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPOTBUY - SPOTPAK EXTRACT MODULE - SQDGET'                      
***********************************************************************         
*                                                                     *         
*        SUB-ROUTINE TO LOAD SQAD CPP TABLE FOR MARKET                *         
*                                                                     *         
*NTRY          USE SQDENTKY HAS ALL NEEDED DATA                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SQDGET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
*                                                                               
         ICM   R3,15,SBSQDBFA      ESTABLISH SQAD BUFFER                        
         USING SQDBUFFD,R3                                                      
*                                                                               
         XC    DBLOCKC,DBLOCKC     INIT DBLOCK                                  
         LA    R7,DBLOCKC                                                       
         USING DBLOCKD,R7          ESTABLISH DBLOCK                             
*                                                                               
         LA    R0,IOAREA                                                        
         ST    R0,DBAREC           PASS IOAREA                                  
*                                                                               
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBFILE,=C'TP '      SET DEMO FILE TO 'TP'                        
         MVI   DBTPTT,C'P'         SET FOR 4 WEEK AVG. ONLY                     
         MVC   DBCOMFCS,SBCOMFAC   A(COMFACS)                                   
         MVI   DBBTYPE,C'S'        BOOK TYPE IS 'S'                             
         CLI   SBQBKTYP,C'H'       OVERRIDE TO HISPANIC?                        
         BNE   *+8                  NO                                          
         MVI   DBBTYPE,C'R'        BOOK TYPE R IS SQAD HISPANIC                 
***      CLI   SBQMED,C'T'         MEDIA T?                                     
         LA    RF,SBQMED           MEDIA                                        
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BNE   *+8                 NO                                           
         LA    RF,SBMED            YES - CURRENT MEDIA                          
         CLI   0(RF),C'T'          MEDIA T?                                     
         BNE   SQDGET05            NO                                           
         TM    SBEFLAG7,SBE7SQDQ   SQADTYP=Q?                                   
         BZ    SQDGET05            NO                                           
         MVI   DBBTYPE,C'Q'        BOOK TYPE Q FOR SQAD                         
*                                                                               
SQDGET05 MVI   DBSELSRC,C'N'       NIELSEN CONSIDERED SOURCE                    
***      CLI   SBQMED,C'R'         DOING RADIO?                                 
         LA    RF,SBQMED           MEDIA                                        
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BNE   *+8                 NO                                           
         LA    RF,SBMED            YES - CURRENT MEDIA                          
         CLI   0(RF),C'R'          DOING RADIO?                                 
         BNE   *+8                 NOPE                                         
         MVI   DBSELSRC,C'S'       S FOR SQAD RADIO!                            
         MVC   DBSELMED,SBMED      SET MEDIA                                    
*                                                                               
         TM    FLAGS,LPMBOOK       HAVE LPM BOOK?                               
         BZ    SQDGET10            NO                                           
         MVI   DBSELMED,C'W'       YES, PASS MEDIA 'W'                          
         TM    FLAGS,LPMBKOV       LPM OVERNIGHT?                               
         BZ    SQDGET10            NO                                           
         MVI   DBSELMED,C'O'       YES, PASS MEDIA 'O'                          
*                                                                               
SQDGET10 MVC   DBSELBK,SQDREL      SET RELEASE                                  
         MVC   DBSELSQ,SQDQTR      SET QUARTER                                  
*                                                                               
         MVC   DBSELALF,SQDALF     ALPHA MARKET NUMBER                          
*                                                                               
         MVC   DBSELAGY,SBAGY      AGENCY                                       
         MVC   DBSELCLI,SBCLT      CLIENT                                       
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         ST    RE,DBEXTEND                                                      
         USING DBXUIDD,RE                                                       
         MVC   DBXUIID,=C'UID '                                                 
         MVC   DBXUUID,SBUSERID                                                 
         DROP  RE                                                               
*                                                                               
*        FILL TABLE FOR MARKET                                                  
*                                                                               
         LA    R1,SQDBSRP          POINT TO BINSRCH PARMS                       
         USING BSPPRMSD,R1         ESTABLISH BINSRCH PARAMETERES                
                                                                                
         XC    BSPNOR,BSPNOR       RESET TABLE TO EMPTY                         
*&&DO                                                                           
         LA    R4,SQDDPTTB         POINT TO DAYPART TABLE                       
                                                                                
SQGDPTLP DS    0H                  DAYPART LOOP                                 
                                                                                
         CLI   SQDDPTAG-SQDDPTTB(R4),0     DONE AT EOT                          
         BE    SQGDPTDN                                                         
                                                                                
         MVC   DBSELTIM(2),SQDDPTSQ-SQDDPTTB(R4) SET TIME TO SQAD DPT           
*&&                                                                             
         MVC   DBSELTIM(2),SQDDPTCD  ONLT GET THIS SPOT'S DPT                   
         MVI   DEMANDSW,0          INIT DEMAND SWITCH                           
*                                                                               
         GOTO1 SQDDMNDV,DMCB,DBLOCK,SQDHOOK   GET SQAD CPPS VIA DEMAND          
*&&DO                                                                           
SQGDPTCN DS    0H                                                               
         LA    R4,SQDDPTL(R4)      BUMP TO NEXT DAYPART                         
         B     SQGDPTLP                                                         
*                                                                               
SQGDPTDN DS    0H                                                               
*&&                                                                             
SQDGETX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SPOTBUY - SPOTPAK EXTRACT MODULE - SQDHOOK'                     
***********************************************************************         
*                                                                     *         
*        SUB-ROUTINE TO LOAD ACCUMULATE CPP'S FOR MARKET/DPT          *         
*                                                                     *         
*NTRY    R4==> ENTRY IN DAYPART TRANSLATION TABLE                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SQDHOOK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         USING DBLOCKD,R7          ESTABLISH DBLOCK                             
         USING SQDBUFFD,R3         R3 = A(SQDBUFFD)                             
*                                                                               
         CLI   DEMANDSW,0          CAN ONLY COME HERE ONCE                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   DEMANDSW,C'Y'       INDICATE WE HAVE BEEN HERE ONCE              
*                                                                               
         LA    R2,SQHTYPTB         POINT TO CPP TYPES TABLE                     
*                                                                               
SQHTYPLP DS    0H                  CPP TYPE LOOP                                
*                                                                               
         XC    SQDCPPS,SQDCPPS     INIT CPP'S                                   
*                                                                               
         CLI   0(R2),X'FF'         DONE AT END OF TABLE                         
         BE    SQHTYPDN                                                         
*                                                                               
         MVC   SQDDEMOS,DEMOS      COPY DEMO LIST                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,NDEMOS         NUMBER OF DEMOS IN LIST                      
         BZ    SQHTYPCN            SKIP IF NO DEMOS                             
*                                                                               
         LA    R1,SQDDEMOS         POINT TO FIRST DEMO                          
*                                                                               
         CLI   0(R1),X'FF'         SKIP IF NO DEMOS IN LIST                     
         BE    SQHTYP10                                                         
*                                                                               
         MVC   1(1,R1),0(R2)       SET CPP TYPE                                 
         LA    R1,3(R1)            NEXT DEMO                                    
         BCT   R0,*-10                                                          
*                                                                               
         GOTO1 SQDDOUTV,DMCB,(C'L',SQDDEMOS),DBLOCK,SQDCPPS                     
*                                                                               
SQHTYP10 DS    0H                                                               
*                                                                               
         MVC   SQDDEMOS,DEMOS      SET TO REGULAR DEMO CODES                    
         MVC   SQDCPPTP,0(R2)      SET CCP TYPE                                 
*******  MVC   SQDDPTCD,SQDDPTSQ-SQDDPTTB(R4)  SET SQAD DPT CODE                
         GOTO1 SBABINSR,SQDBSRP,('BSPADD',SQDENTKY)                             
*                                                                               
         OC    BSPAREC-BSPPRMS+SQDBSRP,BSPAREC-BSPPRMS+SQDBSRP                  
         BNZ   *+6                 DIE IF TABLE FULL                            
         DC    H'0'                                                             
*                                                                               
SQHTYPCN DS    0H                  CPP TYPE LOOP                                
*                                                                               
         LA    R2,1(R2)                                                         
         B     SQHTYPLP                                                         
*                                                                               
SQHTYPDN DS    0H                  CPP TYPE LOOP                                
*                                                                               
SQDHOOKX DS    0H                                                               
         XIT1                                                                   
***                                                                             
* SQAD CPP TYPES TABLE                                                          
* HIGH = H, AVG = A, LOW = L                                                    
* HIGH = N, AVG = C, LOW = J FOR CPM TSA VALUES                                 
* HIGH = M, AVG = B, LOW = I FOR CPM DMA VALUES                                 
***                                                                             
SQHTYPTB DC    C'HAL'                                                           
         DC    C'NCJ'                                                           
         DC    C'MBI'                                                           
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
         EJECT                                                                  
*                                                                               
         TITLE 'SPOTBUY - SPOTPAK EXTRACT MODULE - CALC'                        
*                                                                               
* SUB-ROUTINE TO CALCULATE ALL DERIVED NUMBERS IN CHUNK                         
*                                                                               
* AT ENTRY, CHUNK HAS BEEN INITIALIZED BY CALLER                                
*           EQFACT,SBTALMUL,SBTALDIV HAVE BEEN LOOKED UP                        
*                                                                               
CALC     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         USING SCHUNKD,R7          R7 = A(CHUNK)                                
         USING SPTTABD,R2                                                       
*                                                                               
         MVC   SPOTDATE,SPTRDATE   PASS SPOT DATE TO GETMOS                     
         CLI   SBEDEMTY,0          TEST FOR ACCOUNTING ONLY                     
         BE    CALC6               YES-SKIP EQUIVALENCED FIGURES                
         CLI   SBEDEMTY,C'0'       TEST FOR SQAD       ONLY                     
         BNL   CALC6               YES-SKIP EQUIVALENCED FIGURES                
*                                                                               
         L     RF,SCGROSS          EQUIVALENCED DOLLARS = GROSS                 
         CLI   SBETAX,YES          TEST TO SUBTRACT TAX                         
         BNE   *+8                 NO                                           
         S     RF,SCTAX            YES-GROSS-TAX                                
         M     RE,=F'1000'         * BASE FACTOR                                
         SLDA  RE,1                                                             
         D     RE,EQFACT           / EQUIVALENCE FACTOR FOR SLN                 
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,SCEGROSS                                                      
*                                                                               
CALC2    LLC   R0,NDEMOS           R0=COUNTER                                   
         LA    R1,SCDEMOS          R1=A(DEMO VALUES)                            
*                                                                               
CALC4    L     RF,0(R1)            GET DEMO                                     
         M     RE,EQFACT           * EQUIV FACTOR FOR SLN                       
         SLDA  RE,1                                                             
         D     RE,=F'1000'         / BASE FACTOR                                
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R1)            EQUIVALENCED DEMO                            
         LA    R1,L'SCDEMOS(R1)                                                 
         BCT   R0,CALC4                                                         
*                                                                               
CALC6    L     RF,SCGROSS          COMPUTE ORDERED NUMBERS                      
         S     RF,SCNET            GROSS-NET=COMMISSION                         
         ST    RF,SCCOM                                                         
*                                                                               
         CLI   BUYPRD,X'FF'        TEST FOR POL BUY                             
         BE    CALC8               YES-DOLLARS ARE ALREADY WEIGHTED             
*                                                                               
         CLI   SBEDEMTY,C'0'       NO WEIGHTING BY SPOTS FOR SQAD               
         BNL   CALC8                                                            
*                                                                               
         LA    R0,(SCORDX-SCGROSS)/4  WEIGHT ORDERED                            
         LA    R1,SCGROSS             BY N'SPOTS                                
CALC7    L     RF,0(R1)                                                         
         M     RE,SCSPOTS                                                       
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,CALC7                                                         
*                                                                               
         B     CALC8                                                            
*                                                                               
         L     RF,SCGSTI           WEIGHT GST BY N'SPOTS                        
         M     RE,SCSPOTS                                                       
         ST    RF,SCGSTI                                                        
*                                                                               
CALC8    CLI   SBEPAID,YES         TEST TO EXTRACT PAID                         
         BNE   CALC10              NO                                           
         CLI   BUYPRD,X'FF'        TEST FOR POL BUY                             
         BE    CALC9               YES-DOLLARS ARE WEIGHTED ALREADY             
*                                                                               
         CLI   SBEDEMTY,C'0'       NO WEIGHTING BY SPOTS FOR SQAD               
         BNL   CALC9                                                            
*                                                                               
         L     RF,SCPAY            GROSS PAID * N'PAID SPOTS                    
         LR    R1,RF               SAVE GROSS PAID                              
         M     RE,SCPAYSP                                                       
         ST    RF,SCPAY                                                         
         MH    R1,SPTPAYTS         * GROSS PAID                                 
         ST    R1,SCPAYT           GROSS PAID TODAY                             
*                                                                               
         L     RF,SCPAYN           NET PAID * N'PAID SPOTS                      
         LR    R1,RF               SAVE NET PAID                                
         M     RE,SCPAYSP                                                       
         ST    RF,SCPAYN                                                        
         MH    R1,SPTPAYTS         * NET PAID                                   
         ST    R1,SCPAYTN          NET PAID TODAY                               
*                                                                               
         L     RF,SCPAYTX          TAX * N'PAID SPOTS                           
         M     RE,SCPAYSP                                                       
         ST    RF,SCPAYTX                                                       
*                                                                               
         B     CALC9                                                            
*                                                                               
         L     RF,SCGSTP           GST ON PAID SPOTS                            
         M     RE,SCPAYSP                                                       
         ST    RF,SCGSTP                                                        
*                                                                               
CALC9    L     RE,SCGROSS          GROSS UNPAID =                               
         S     RE,SCPAY            GROSS-GROSS PAID                             
         ST    RE,SCUNP                                                         
*                                                                               
         L     RE,SCNET            NET UNPAID =                                 
         S     RE,SCPAYN           NET - NET PAID                               
         ST    RE,SCUNPN                                                        
*                                                                               
         L     RE,SCTAX            UNPAID TAX =                                 
         S     RE,SCPAYTX          TAX - PAID TAX                               
         ST    RE,SCUNPTX                                                       
*                                                                               
         L     RE,SCGSTI           UNPAID GST =                                 
         S     RE,SCGSTP           GST - PAID GST                               
         ST    RE,SCGSTU                                                        
*                                                                               
         L     RE,SCPSTI           UNPAID PST =                                 
         S     RE,SCPSTP           PST - PAID PST                               
         ST    RE,SCPSTU                                                        
*                                                                               
CALC10   CLI   SBECS,YES           TEST TO EXTRACT CHILD SPOT                   
         BNE   CALC28              NO                                           
         LA    R1,SCCSNTP                                                       
         L     RF,SBAIO1                                                        
*                                                                               
         TM    BDCIND2-BUYREC(RF),X'04'  TEST BDCIND IS CHARACTER?              
         BZ    CALC12                                                           
         CLI   BDCIND-BUYREC(RF),C'P'                                           
         BE    CALC16                                                           
         B     CALC14                                                           
*                                                                               
CALC12   TM    BDCIND-BUYREC(RF),X'FE'                                          
         BZ    *+8                                                              
CALC14   LA    R1,SCCSPAY                                                       
*                                                                               
CALC16   MVC   0(4,R1),SCGROSS     SLOT GROSS AND SPOTS                         
         MVC   8(4,R1),SCSPOTS                                                  
         MVC   SCCSTIM,SCGROSS                                                  
*                                                                               
*                                                                               
         LA    RF,SBTALF10         POINT TO START OF FIRST FACTORS              
         CLC   SCDATE,=X'C0BB'     PRIOR TO 05/27/96?                           
         BL    CALC24                                                           
         OC    SBTADATE,SBTADATE   TEST EFFECTIVE DATE FOR 2ND SET              
         BZ    CALC22                                                           
         CLC   SCDATE,SBTADATE     YES-COMPARE CHUNK DATE TO IT                 
         BL    CALC22                                                           
         LA    RF,SBTALF20         POINT TO SECOND FACTORS                      
*                                                                               
CALC22   DS    0H                                                               
         CLI   SPTPRD1,X'FF'       POL REQUEST GETS GMI FACTOR                  
         BE    CALC24                                                           
*                                                                               
         CLC   SBBCLT,=X'BE60'     TEST CLT PTA                                 
         BNE   CALC23                                                           
         CLI   SPTPRD1,X'40'       PRD RL                                       
         BE    CALC22A                                                          
         CLI   SPTPRD1,X'41'       AND FL                                       
         BE    CALC22A                                                          
         CLI   SPTPRD1,X'60'       FOR PTA, 00-5F ARE GMI (FCTR0)               
         BL    CALC24                                                           
*                                                                               
CALC22A  LA    RF,8(RF)            AND 40,41, AND >X'5F' GET FCTR1              
         CLI   SPTPRD1,X'A0'                                                    
         BL    CALC24                                                           
         LA    RF,8(RF)            X'A0'-X'BF' GET FCTR2                        
         B     CALC24                                                           
*                                                                               
CALC23   CLI   SPTPRD1,X'40'       PRD CODES 01-3F GET GMI FACTOR               
         BL    CALC24                                                           
         CLI   SPTPRD1,X'80'       GREATER THEN OR EQU TO 128?                  
         BNL   CALC24              YES - NTP=3 IS THE SAME AS NTP=0             
         LA    RF,8(RF)                                                         
         CLI   SPTPRD1,X'60'                                                    
         BL    CALC24                                                           
         LA    RF,8(RF)                                                         
         CLI   SPTPRD1,X'80'                                                    
         BL    CALC24                                                           
***      LA    RF,8(RF)                                                         
***      CLI   SPTPRD1,X'A0'                                                    
***      BL    CALC24                                                           
         LA    RF,SBTALF10         ANY BETTER IDEAS???                          
*                                                                               
CALC24   LM    R1,R2,0(RF)         PICK UP MULT/DIV FACTORS                     
*                                                                               
CALC26   L     RF,SCCSTIM          TIME                                         
         MR    RE,R1               * TALENT FACTOR                              
         SLDA  RE,1                                                             
         DR    RE,R2               / TALENT BASE                                
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,SCCSTPT          TIME + TALENT                                
         S     RF,SCCSTIM                                                       
         ST    RF,SCCSTAL          TALENT DOLLARS                               
*                                                                               
CALC28   CLI   SBEDEMTY,0          TEST FOR DEMO LOOKUP                         
         BE    CALC30              NO                                           
         CLI   SBETAX,YES          TEST TO SUBTRACT TAX                         
         BNE   CALC30              NO                                           
         L     RE,SCGROSS          GET GROSS                                    
         S     RE,SCTAX            - TAX                                        
         ST    RE,SCGROSS                                                       
*                                                                               
CALC30   BRAS  RE,CALCECST         GET EFFECTIVE COST IF REQUIRED               
*                                                                               
CALCX    J     EXIT                                                             
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
         TITLE 'SPOTBUY - SPOTPAK EXTRACT MODULE - CALCECST'                    
***********************************************************************         
*                                                                     *         
* SUB-ROUTINE TO CALCULATE EFFECTIVE COST BASED ON BILL FORMULA       *         
* ALSO GETS CANADIAN OUTPUT GST/PST IF NEEDED                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CALCECST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         USING SCHUNKD,R7          R7 = A(CHUNK)                                
*                                                                               
         CLI   SBEEFCST,YES        TEST EFFECTIVE COST NEEDED                   
         BE    *+12                                                             
         TM    SBEGST,SBEGSTO+SBEPSTO      OR OUTPUT GST/PST                    
         BZ    CALECX                                                           
         MVC   BYTE,SBBILBAS       DEFAULT BILL FORMULA                         
         MVC   FULL,SBBILCOM                                                    
         CLC   SBQEST,SBQESTND     TEST MULTIPLE ESTIMATES LUMPED               
         BE    *+12                TOGETHER                                     
         CLI   SBQSEPES,C'Y'                                                    
         BNE   CALEC2              YES - USE PRODUCT BILL FORMULA               
         ICM   R1,15,SBAESTTB      NO - USE ESTIMATE BILL FORMULA               
         BZ    CALEC4                                                           
         LA    RE,255                                                           
         CLI   SCPRD1,X'FE'        TEST UNALLOCATED                             
         BE    *+8                 YES-USE PRODUCT POL                          
         IC    RE,SCPRD1                                                        
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         L     R5,SBAIO1                                                        
         LLC   RF,BUYKEST-BUYRECD(R5)                                           
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         LA    R1,0(R1,RE)                                                      
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         MH    RE,=Y(ESTBUFFL)                                                  
         ICM   R1,15,SBAESTBF                                                   
         BZ    CALEC4                                                           
         LA    RE,0(R1,RE)                                                      
         USING ESTBUFFD,RE                                                      
         MVC   BYTE,EBBILBAS                                                    
         MVC   FULL,EBBILCOM                                                    
         B     CALEC4                                                           
         DROP  RE                                                               
*                                                                               
CALEC2   ICM   R1,15,SBAPRDBF      GET PRODUCT BILL FORMULA                     
         BZ    CALEC4                                                           
         LLC   RE,SCPRD1                                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         LA    RE,0(RE,R1)                                                      
         USING PRDBUFFD,RE                                                      
         MVC   BYTE,PBBILBAS                                                    
         MVC   FULL,PBBILCOM                                                    
         DROP  RE                                                               
*                                                                               
CALEC4   BRAS  RE,GETMOS           GET MOS                                      
         BRAS  RE,RDBFORM          READ SFM BFORM IF NEEDED                     
*                                                                               
         L     R2,SCGROSS          CALCULATE EFFECTIVE COST                     
         S     R2,SCTAX            USING THE BILL FORMULA                       
         L     R3,SCNET            R2=GROSS-TAX                                 
         S     R3,SCTAX            R3=NET-TAX                                   
         SR    RF,RF                                                            
         OC    FULL,FULL                                                        
         BZ    CALEC6                                                           
         CLI   FULL,C'C'           TEST COMMISSION ONLY RATE                    
         BNE   CALEC5                                                           
         SR    R2,R3               SET EFFECTIVE COST = GROSS                   
         ST    R2,SCEFFCST                                                      
         B     CALECX                                                           
*                                                                               
CALEC5   SR    RE,RE                                                            
         LR    RF,R2                                                            
         TM    BYTE,X'01'                                                       
         BZ    *+6                                                              
         LR    RF,R3                                                            
         M     RE,FULL                                                          
         SLDA  RE,1                                                             
         D     RE,=F'1000000'                                                   
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
*                                                                               
CALEC6   LR    RE,R2                                                            
         TM    BYTE,X'10'                                                       
         BZ    *+6                                                              
         LR    RE,R3                                                            
         AR    RE,RF                                                            
         A     RE,SCTAX                                                         
         ST    RE,SCEFFCST                                                      
*                                                                               
         XC    SCGSTO,SCGSTO                                                    
         XC    SCPSTO,SCPSTO                                                    
         TM    SBEGST,SBEGSTO+SBEPSTO  TEST OUTPUT GST/PST REQUIRED             
         BZ    CALECX                                                           
         CLI   CANADA,C'Y'         AND CANADIAN                                 
         BNE   CALECX                                                           
         L     RE,SCEFFCST         COST FOR GST/PST IS...                       
         S     RE,SCTAX            EFFECTIVE COST MINUS TAX                     
*                                                                               
* PROVINCE POSITIONS 1-10 ARE BC/AL/SA/MA/ON/PQ/NB/NS/PE/NF                     
*                                                                               
         LHI   R0,5                                                             
         CLI   XPSTPROV+1,C'Q'     TEST PQ=                                     
         BE    *+12                                                             
         LHI   R0,6                                                             
         CLI   XPSTPROV+1,C'B'     TEST NB=                                     
         BE    *+12                                                             
         LHI   R0,7                                                             
         CLI   XPSTPROV+1,C'S'     TEST NS=                                     
         BE    *+8                                                              
         LHI   R0,9                ASSUME NF=                                   
         STC   R0,HALF             STORE PROVINCE INDEX                         
         MVC   HALF+1(1),XPSTCODE  PST CODE                                     
*                                                                               
         MVC   CPSTVAL,SBCPST      CLIENT PST                                   
         MVI   MAINPST,C'N'        NO MAIN PST                                  
         CLC   SPOTMOS,=X'DCE1'    BEFORE JUL01/10?                             
         JL    CALEC6A             YES - DON'T LOOK AT MAIN PST                 
         OC    SBCMPST,SBCMPST     HAVE MAIN PST FOR CLIENT?                    
         JZ    CALEC6A             NO - EXIT                                    
         XC    CPSTVAL,CPSTVAL     CLEAR PST STRING                             
         LLC   R1,SBCMPST          GET INDEX (STARTS AT 1 - NOT 0!)             
         BCTR  R1,0                SUBTRACT ONE SO WE INDEX FROM 0              
         STC   R1,HALF             SAVE INDEX INTO PST CODES                    
         LA    R1,CPSTVAL(R1)      CLIENT PST FIELD PLUS INDEX                  
         MVC   0(1,R1),SBCMPST+1   PST VALUE                                    
         MVC   HALF+1(1),0(R1)     SAVE PROVINCE PST VALUE                      
         MVI   MAINPST,C'Y'        HAVE MAIN PST                                
*                                                                               
CALEC6A  XC    FULL,FULL           FULL=A(PRODUCT IN PRDBUFF)                   
         CLI   SCPRD1,X'FF'        EXCEPT FOR POL,PRODUCT MAY BE EXEMPT         
         BE    CALEC8                                                           
         ICM   R1,15,SBAPRDBF                                                   
         BZ    CALEC8                                                           
         LLC   RF,SCPRD1                                                        
         BCTR  RF,0                                                             
         MH    RF,=Y(PRDBUFFL)                                                  
         LA    RF,0(RF,R1)                                                      
         ST    RF,FULL             SAVE A(PRDBUFF ENTRY)                        
         USING PRDBUFFD,RF                                                      
         MVC   PPSTVAL,PBUFLD2     PRODUCT PST                                  
         CLC   SPOTMOS,=X'DCE1'    BEFORE JUL01/10?                             
         JL    CALEC6B             YES - DON'T LOOK AT MAIN PST                 
         OC    PBUFMPST,PBUFMPST   HAVE MAIN PST FOR PRODUCT?                   
         JZ    CALEC6B             NO - EXIT                                    
         XC    PPSTVAL,PPSTVAL     CLEAR PST STRING                             
         LLC   R1,PBUFMPST         GET INDEX (STARTS AT 1 - NOT 0!)             
         BCTR  R1,0                SUBTRACT ONE SO WE INDEX FROM 0              
         STC   R1,HALF             SAVE INDEX INTO PST CODES                    
         LA    R1,PPSTVAL(R1)      PRODUCT PST FILED PLUS INDEX                 
         MVC   0(1,R1),PBUFMPST+1  PST VALUE                                    
         MVC   HALF+1(1),0(R1)     SAVE PROVINCE PST VALUE                      
         MVI   MAINPST,C'Y'        HAVE MAIN PST                                
*                                                                               
CALEC6B  CLI   PBGSTCD,C'X'        PRODUCT GST SETTING OVERRIDES CLIENT         
         BE    CALEC10                                                          
         CLI   PBGSTCD,C'Z'                                                     
         BE    CALEC10                                                          
         CLI   PBGSTCD,C'S'        STD GST                                      
         BE    CALEC9                                                           
         DROP  RF                                                               
*                                                                               
CALEC8   CLI   SBCEXTRA+11,C'X'    CLIENT MAY BE EXEMPT GST                     
         BE    CALEC10                                                          
         CLI   SBCEXTRA+11,C'Z'                                                 
         BE    CALEC10                                                          
*                                                                               
CALEC9   CLI   HALF+1,0            GST MAY NOT APPLY DEPENDING ON PST           
         BE    CALEC9B             NO PST SET SO NOT QUEBEC OR HST PROV         
         CLI   HALF,0              BC (BRITISH COLUMBIA)?                       
         BNE   *+14                NO                                           
         CLC   SPOTMOS,=X'E281'    ON/AFTER APR01/13                            
         BNL   CALEC9BA            YES - NO HST BUT NEED GST!                   
         CLI   HALF,5              QUEBEC (PQ)?                                 
         BE    CALEC9BA            YES                                          
         LLC   R1,HALF             PROVINCE INDEX                               
         ICM   RF,15,FULL          TEST PRODUCT SUBJECT TO THIS HST             
         BZ    CALEC9A             NO PRODUCT PST TO TEST                       
         LA    RF,PPSTVAL          PRODUCT PST                                  
         OC    PPSTVAL,PPSTVAL     HAVE PRODUCT PST?                            
         BNZ   *+8                 YES                                          
CALEC9A  LA    RF,CPSTVAL          NO - USE CLIENT PST                          
         AR    RF,R1               ADD PST INDEX                                
         CLI   0(RF),C'H'          HST?                                         
         BNE   CALEC9B             NO                                           
         CLI   HALF,8              PE (PRINCE EDWARD ISLAND)?                   
         BNE   CALEC10             NO - HST APPLIES SO NO GST                   
         CLC   SPOTMOS,=X'E281'    BEFORE APR01/13                              
         BL    CALEC9BA            YES                                          
         B     CALEC10             NO - HST APPLIES SO NO GST                   
*********************************************************************           
*  NOTE: WHEN CHANGING GST FOR CANADA, THIS CODE IS IN SPWRI02      *           
*  TOO NEAR BVAL9B                                                  *           
*********************************************************************           
CALEC9B  CLI   MAINPST,C'Y'        HAVE MAIN PST?                               
         BE    CALEC10             YES - NO GST!                                
CALEC9BA LR    RF,RE               (EFF.-MINUS TAX)                             
         LR    R1,RE               SAVE VALUE FOR PST CALC                      
*                                                                               
         CLC   SCDATE,=X'D4E1'     ON/AFTER 7/1/06?                             
         BNL   *+12                                                             
         M     RE,=F'14'           *7% GST RATE GIVES OUTPUT GST AMOUNT         
         B     CALEC9C                                                          
*                                                                               
         CLC   SCDATE,=X'D79F'     ON/AFTER 12/31/07?                           
         BNL   *+12                                                             
         M     RE,=F'12'           6% FROM 7/1/06-12/30/07                      
         B     CALEC9C                                                          
         M     RE,=F'10'           5% ON/AFTER 12/31/07                         
*                                                                               
CALEC9C  D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SCGSTO                                                        
         LR    RE,R1               EFF. COST MINUS TAX (FOR PST CALC)           
         AR    RE,RF               + GST                                        
*                                                                               
CALEC10  TM    SBEGST,SBEPSTO      EXTRACT PST                                  
         BZ    CALECX                                                           
         LR    RF,RE               RF=(EFF COST-TAX)+ANY GST                    
         CLI   HALF+1,0            HAVE PST VALUE?                              
         BE    CALECX              NO                                           
         ICM   R1,15,FULL          TEST PRODUCT EXEMPT THIS PST                 
         BZ    CALEC11             NO PRODUCT PST TO TEST                       
         LA    R1,PPSTVAL          PRODUCT PST                                  
         OC    PPSTVAL,PPSTVAL     HAVE PRODUCT PST?                            
         BNZ   *+8                 YES                                          
CALEC11  LA    R1,CPSTVAL          CLIENT PST                                   
         LLC   R0,HALF             PROVINCE INDEX                               
         AR    R1,R0               ADD PST INDEX                                
         CLI   0(R1),C'S'          STD ONLY - X/Z/_ MEAN NO OUTPUT PST          
         BE    CALEC12                                                          
         CLI   0(R1),C'H'                                                       
         BNE   CALECX              MUST BE X/Z/BLANK SO NO OUTPUT PST           
*                                                                               
CALEC12  XR    RE,RE               CLEAR RE                                     
         CLI   MAINPST,C'Y'        HAVE A MAIN PST?                             
         BE    CALEC13             YES - OVERRIDE THE PST RATE                  
         ICM   RE,7,XPSTRATE       HAVE ZERO RATE PST?                          
         BZ    CALECX              YES - EXIT                                   
         B     CALEC20             NO - CALCULATE PST                           
*                                                                               
CALEC13  CLI   HALF,5              QUEBEC?                                      
         BNE   CALEC15             NO - MUST BE AN HST PROVINCE                 
         CLI   HALF+1,C'*'         DOES QST APPLY?                              
         BE    CALECX              NO - EXIT                                    
         L     RE,=F'07500'        YES - 7.5%                                   
         CLC   SPOTMOS,=X'DE21'    BEFORE JAN01/11?                             
         JL    CALEC20             YES - OLD RATE OF 7.5%                       
         L     RE,=F'08500'        NEW RATE OF 8.5% AS OF 01/01/2011            
         CLC   SPOTMOS,=X'E021'    BEFORE JAN01/12?                             
         JL    CALEC20             YES - 2011 RATE OF 8.5%                      
         L     RE,=F'09500'        NEW RATE OF 9.5% AS OF 01/01/2012            
         CLC   SPOTMOS,=X'E221'    BEFORE JAN01/13?                             
         JL    CALEC20             YES - 2012 RATE OF 9.5%                      
         L     RE,=F'09975'        NEW RATE OF 9.975% AS OF 01/01/2013          
         B     CALEC20             CALCULATE PST                                
*                                                                               
CALEC15  CLI   HALF,0              BC (BRITISH COLUMBIA)?                       
         BNE   CALEC16             NO                                           
         L     RE,=F'12000'        YES - 12% AS OF 7/1/10                       
         CLC   SPOTMOS,=X'E281'    BEFORE APR01/13?                             
         BL    CALEC20             YES - CALCULATE PST                          
         J     XIT                 NO HST FOR BC STARTING 4/1/13                
*                                                                               
CALEC16  CLI   HALF,6              NB (NEW BRUNSWICK)?                          
         BE    *+12                YES                                          
         CLI   HALF,9              NF (NEWFOUNDLAND)?                           
         BNE   CALEC16A            NO                                           
         CLC   SPOTMOS,=X'E8E1'    ON AFTER JUL01/2016?                         
         BL    CALEC17             NO - GETS 13%                                
         L     RE,=F'15000'        YES - 15% AS OF 7/1/16                       
         B     CALEC20             CALCULATE PST                                
*                                                                               
CALEC16A CLI   HALF,7              NS (NOVIA SCOTIA)?                           
         BNE   *+12                NO                                           
         L     RE,=F'15000'        YES - 15% AS OF 7/1/10                       
         B     CALEC20             CALCULATE PST                                
*                                                                               
         CLI   HALF,8              PE (PRINCE EDWARD ISLAND)?                   
         BNE   CALEC17             NO                                           
         CLC   SPOTMOS,=X'E941'    BEFORE OCT01/16?                             
         BL    *+12                NO                                           
         L     RE,=F'15000'        YES - 15% AS OF 10/1/16                      
         B     CALEC20             CALCULATE PST                                
         CLC   SPOTMOS,=X'E281'    BEFORE APR01/13?                             
         JL    XIT                 YES - NO HST BEFORE THEN                     
         L     RE,=F'14000'        YES - 14% AS OF 4/1/13                       
         B     CALEC20             CALCULATE PST                                
*                                                                               
CALEC17  L     RE,=F'13000'        EVERYTHING ELSE IS 13%                       
*                                                                               
CALEC20  AR    RE,RE                                                            
         MR    RE,RE                                                            
         D     RE,=F'100000'                                                    
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,SCPSTO                                                        
CALECX   J     XIT                                                              
*                                                                               
RDBFORM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPTTABD,R2          R2 = SPOT TABLE                              
*                                                                               
         TM    SBEFLAG5,SBE5BFRM   OPT SET TO GET BILL FORM RECORD?             
         BZ    RDBX                NO                                           
         CLI   SBB1XPRF+11,C'N'    PROFILE SET TO GET BILL FORMULA?             
         BE    RDBX                NO                                           
*                                                                               
         ICM   R4,15,AGETBFRW      HAVE A(GETBFRW)?                             
         BZ    RDBX                NO - EXIT                                    
         USING SPGBFRDD,R4                                                      
         XC    SPGBAM(SPGBRTRN-SPGBAM),SPGBAM                                   
*                                                                               
         MVC   SPGBAM,SBBAGYMD     A/M                                          
         MVC   SPGBCLT,SBBCLT      CLIENT                                       
         ICM   R1,15,SBAPRDBF      HAVE PRD BUFFER?                             
         BZ    RDBX                NO - CAN'T GET EBCIDIC PRD!                  
         LLC   RF,SPTPRD1          BINARY PRD                                   
         BCTR  RF,0                                                             
         MHI   RF,PRDBUFFL         INDEX INTO PRD BUFFER                        
         AR    R1,RF                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SPGBPRD,PBALPH      EBCIDIC PRD                                  
         MVC   SPGBEST,SBBEST      EST                                          
         MVC   SPGBMKT,SBBMKT      MKT (NOTE- MGRS NOT DONE YET)                
         DROP  R1                                                               
*                                                                               
         CLI   SBB1XPRF+12,C'Y'    SFM BILL FORMS WITH EFF DATE?                
         BNE   RDBF4               NO                                           
         OC    SPOTMOS,SPOTMOS     HAVE MOS?                                    
         BZ    RDBX                NO - DONE                                    
         GOTO1 VDATCON,DMCB,(2,SPOTMOS),(3,DUB)                                 
         MVC   SPGBMOS,DUB         YYMM MOS                                     
*                                                                               
RDBF4    OC    SVBFKEY,SVBFKEY     FIRST TIME THROUGH?                          
         BZ    *+14                YES                                          
         CLC   SVBFKEY,SPGBAM      SAME KEY AS LAST TIME?                       
         BE    RDBF6               YES - BFORMULA STILL IN SPGBFORM             
         MVC   SVBFKEY,SPGBAM      SAVE OFF THE KEY                             
*                                                                               
         MVC   SPGBACOM,SBCOMFAC   A(COMFACS)                                   
         MVC   SPGBLODR,=V(LOADER) A(LOADER)                                    
*                                                                               
         GOTO1 =V(SPGETBFR),DMCB,SPGBFRD                                        
*                                                                               
RDBF6    MVC   BYTE,SPGBFORM       SFM BFORM REC                                
         MVC   FULL,SPGBFORM+1                                                  
*                                                                               
RDBX     J     XIT                                                              
         DROP  R2,R4                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
GETMOS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SPOTMOS,SPOTMOS     RETURNED MOS                                 
         LA    R6,SPOTDATE         SPOT DATE                                    
         ICM   R1,15,MONTAB        MONTAB RESOLVED?                             
         BZ    GMOSXIT             NO - EXIT                                    
*                                                                               
GMOS05   OC    0(4,R1),0(R1)       ANY MONTH ENTRIES LEFT?                      
         BZ    GMOSXIT             NO                                           
         CLC   0(2,R6),0(R1)       BEFORE PERIOD?                               
         BL    GMOSXIT             YES - NO MOS FOUND                           
         CLC   0(2,R6),2(R1)       SPOT DATE WITHIN PERIOD?                     
         BNH   GMOS10              YES                                          
         LA    R1,4(R1)            NO - BUMP TO NEXT MONTH ENTRY                
         B     GMOS05              CHECK NEXT MONTH ENTRY                       
*                                                                               
GMOS10   MVC   SPOTMOS,0(R1)       REQUEST MONTH                                
         TM    SBEFLAG5,SBE5CLDR   USING CALENDAR MONTHS?                       
         BNZ   GMOSXIT             YES                                          
         GOTO1 =V(BRDMON),DMCB,(X'FF',SPOTMOS),SPOTMOS                          
*                                                                               
GMOSXIT  J     XIT                                                              
         DROP                                                                   
*                                                                               
         TITLE 'SPOTBUY - SPOTPAK EXTRACT MODULE - GETPUR'                      
***********************************************************************         
* SUB-ROUTINE TO GET THE PURCHASED DEMOS                              *         
*                                                                     *         
* ON EXIT, PURVALSA CONTAINS THE EXTRACTED ADJUSTED VALUES            *         
*          PURVALSU CONTAINS THE EXTRACTED UNADJUSTED VALUES          *         
***********************************************************************         
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         USING BUYREC,R4           R4 = A(BUY RECORD)                           
*                                                                               
GETPUR   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'62'        FIRST SEARCH FOR UPGRADE ELEMENT             
         MVI   ELCDHI,X'62'        FIX MEL'S BUG BY FORCING ALL                 
         BRAS  RE,NEXTEL           PURCHASED DEMOS TO BE OVERRIDES              
         BNE   *+8                 IF BUYLINE HAS UPGRADE                       
         MVI   UPGRADE,YES         NOTE THAT WE HAVE ELEMENT                    
*                                                                               
         XC    A50ELEM,A50ELEM     CLEAR A(X'50' ELEMENT)                       
***      ICM   RE,15,SBCOMDEM      HAVE A(COMSCORE DEMO OVERRIDE LIST)          
***      BZ    GETPUR00            NO                                           
***      OC    0(160,RE),0(RE)     HAVE ANY COMSCORE DEMO OVERRIDE?             
***      BZ    GETPUR00            NO                                           
         LA    R6,BDELEM           A(FIRST BUY ELEMENT)                         
         MVI   ELCDLO,X'50'        SEARCH FOR X'50' ELEMENT                     
         MVI   ELCDHI,X'50'        SEARCH FOR X'50' ELEMENT                     
         BRAS  RE,NEXTEL           PURCHASED DEMOS TO BE OVERRIDES              
         BNE   GETPUR00            IF BUYLINE HAS UPGRADE                       
         ST    R6,A50ELEM          SAVE A(X'50' ELEMENT)                        
*                                                                               
GETPUR00 LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'02'                                                     
         OC    SBEMKT,SBEMKT       TEST FOR REQUESTED MARKET                    
         BZ    GETPUR1             NONE SET                                     
         CLC   SBEMKT,SBBMKT       TEST SAME MARKET AS KEY                      
         BE    *+8                 YES                                          
         MVI   ELCDLO,X'03'        NO-LOOKUP SPILL DATA                         
*                                                                               
GETPUR1  MVC   ELCDHI,ELCDLO                                                    
         DROP  R4                  DROP BUY RECORD USING                        
*                                                                               
GETPUR2  BRAS  RE,NEXTEL                                                        
         BNE   GETPUR10                                                         
         USING NDELEM,R6                                                        
         CLI   NDCODE,X'03'        TEST FOR SPILL ELEMENT                       
         BNE   GETPUR4                                                          
         CLC   SBEMKT,4(R6)        TEST FOR CORRECT MARKET                      
         BNE   GETPUR2             NO                                           
         MVC   SBBSPL,6(R6)        EXTRACT SPILL MKT'S RTG SRV NUMBER           
*                                                                               
GETPUR4  LLC   R2,NDEMOS           R2=COUNTER OF EXTRACT DEMOS                  
         LA    R3,DEMOS            R3=A(EXTRACT DEMO LIST)                      
         LA    R5,PURVALSA         R5=A(ADJUSTED OUTPUT VALUES)                 
         LA    R7,PURVALSU         R7=A(UNADJUSTED OUTPUT VALUES)               
         LLC   RE,NDLEN                                                         
         SHI   RE,NDEMNO-NDELEM                                                 
         SRL   RE,3                FIND N'DEMOS IN ELEMENT                      
         LTR   RE,RE                                                            
         BZ    GETPUR10                                                         
*                                                                               
GETPUR5  CLI   2(R3),0             COMSCORE DEMO?                               
         BNE   GETPUR5F            NO                                           
         CLI   0(R3),X'FF'         END OF DEMO LIST?                            
         BE    GETPUR9             YES - STOP PROCESSING DEMOS                  
         OC    0(3,R3),0(R3)       PAST END OF DEMOS LIST?                      
         BZ    GETPUR9             YES - STOP PROCESSING DEMOS                  
         ICM   RF,15,A50ELEM       HAVE A(X'50' ELEM)?                          
         BZ    GETPUR5F            NO - NO PURCH FOR COMSCORE DEMOS             
         LLC   R1,1(R3)            INDEX INTO SBCOMDEM                          
         BCTR  R1,0                -1 FOR INDEX                                 
         MHI   R1,8                COMSCORE DEMO IN SBCOMDEM                    
         ICM   R4,15,SBCOMDEM      A(COMSCORE DEMO OVERRIDE LIST)               
         AR    R4,R1               R4 = FULL COMSCORE DEMO NAME                 
         LLC   R0,1(RF)            LENGTH OF X'50' ELEMENT                      
         SHI   R0,2                -2 FOR ELEMENT LENGTH OVERHEAD               
         LA    R1,1                R1 WILL KEEP TRACK OF DEMO NUM               
         LA    RF,2(RF)            FIRST COMSCORE DEMO IN X'50' ELEM            
*                                                                               
GETPUR5A CLC   0(8,R4),0(RF)       COMSCORE DEMO OVERRIDE ON THE BUY?           
         BE    GETPUR5B            YES - EXTRACT PURCHASED DEMO VALUE           
         LA    RF,9(RF)            BUMP TO NEXT DEMO IN X'50' ELEMENT           
         AHI   R1,1                BUMP COMSCORE DEMO NUMBER                    
         SHI   R0,9                -9                                           
         LTR   R0,R0               HAVE ANY DEMOS LEFT IN X'50 ELEMENT          
         BNZ   GETPUR5A            YES                                          
         B     GETPUR9             NO - PROCESS NEXT DEMO                       
*                                                                               
GETPUR5B LR    R0,RE               R0=N'DEMOS IN ELEMENT                        
         LA    R4,NDEMNO           R4=A(DEMO DATA)                              
         XR    RF,RF               RF = COMSCORE DEMO NUMBER                    
*                                                                               
GETPUR5C CLI   2(R4),0             COMSCORE DEMO?                               
         BNE   GETPUR5D            NO                                           
         IC    RF,1(R4)            GET COMSCORE INDEX FROM X'02' ELEM           
         CR    R1,RF               INDEXES MATCH?                               
         BE    GETPUR5E            YES                                          
*                                                                               
GETPUR5D LA    R4,8(R4)            NEXT DEMO IN ELEMENT                         
         BCT   R0,GETPUR5C         PROCESS NEXT DEMO                            
         B     GETPUR9             SOMETHINGS WRONG - DEFAULT NO PURCH          
*                                                                               
GETPUR5E LR    R1,R4               A(COMSCORE DEMO IN X'02' BUY ELEM)           
         B     GETPUR6             SAVE THIS PURCHASED DEMO VALUE               
*                                                                               
GETPUR5F LR    R0,RE               R0=N'DEMOS IN ELEMENT                        
         LA    R1,NDEMNO           R1=A(DEMO DATA)                              
         CLC   0(3,R3),0(R1)       MATCH ON DEMO                                
         BE    GETPUR6                                                          
         LA    R1,8(R1)            NEXT DEMO IN ELEMENT                         
         BCT   R0,*-14                                                          
         B     GETPUR9                                                          
*                                                                               
GETPUR6  MVC   ADJDEMO,4(R1)                                                    
         ST    R1,ADEMLIST         A(CURRENT DEMO IN DEMO LIST)                 
         LR    R0,RE               SAVE RE                                      
         BRAS  RE,ADJPREC                                                       
         LR    RE,R0               AND RESTORE                                  
*                                                                               
GETPUR6A MVC   0(4,R5),ADJDEMO     GET VALUE                                    
         MVC   0(4,R7),ADJDEMO                                                  
*                                                                               
GETPUR7  CLI   UPGRADE,YES         TEST FOR UPGRADE                             
         BNE   *+8                                                              
GETPUR8  OI    0(R5),X'80'         YES-NOTE AS MANUAL OVERRIDE                  
         TM    0(R5),X'80'         TEST MANUAL OVERRIDE                         
         BO    GETPUR9                                                          
         LLC   RF,3(R1)            NO-PICK UP HUT ADJUSTMENT                    
         CHI   RF,100              TEST 100%                                    
         BE    GETPUR9                                                          
         SR    R0,R0               NO-ADJUST THE DEMO VALUE                     
         L     R1,ADJDEMO                                                       
         SLL   R1,1                                                             
         MR    R0,RF                                                            
         D     R0,=F'100'                                                       
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         ST    R1,0(R5)            STORE ADJUSTED DEMO VALUE                    
*                                                                               
GETPUR9  LA    R3,3(R3)            NEXT EXTRACT DEMO                            
         LA    R5,4(R5)            NEXT VALUE SLOT                              
         LA    R7,4(R7)                                                         
         BCT   R2,GETPUR5                                                       
*                                                                               
GETPUR10 ICM   R1,15,SBAPURVL      TEST A(PURCHASED VALUES) PASSED              
         BZ    GETPURX                                                          
         LLC   R2,NDEMOS           YES-PASS BACK ADJUSTED PURCH                 
         LA    RE,PURVALSA                                                      
         MVC   0(4,R1),0(RE)                                                    
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R2,*-14                                                          
*                                                                               
GETPURX  J     EXIT                                                             
         LTORG                                                                  
         DROP                                                                   
*                                                                               
         TITLE 'SPOTBUY - SPOTPAK EXTRACT MODULE - ADJPREC'                     
***********************************************************************         
* MODIFY DECIMAL PRECISION FOR EACH DEMO PASSED TO MATCH REPORTING    *         
* PRECISION.                                                          *         
* ADJDEMO CONTAINS DEMO VALUE                                         *         
***********************************************************************         
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
*                                                                               
ADJPREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,ADEMLIST         A(CURRENT DEMO IN DEMO LIST)                 
         CLI   0(R1),X'FF'         END OF DEMO LIST?                            
         BE    ADJPX               YES - STOP PROCESSING DEMOS                  
         OC    0(3,R1),0(R1)       PAST END OF DEMOS LIST?                      
         BZ    ADJPX               YES - STOP PROCESSING DEMOS                  
         CLI   2(R1),0             NON-TRADITIONAL DEMO?                        
         BNE   ADJPREC1            NO                                           
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,1,1(R1)          GET INDEX                                    
         JZ    *+2                 IF 0, SOMETHING IS VERY WRONG                
         BCTR  RF,0                -1                                           
         MHI   RF,9                INDEX INTO DEMO LIST                         
         ICM   RE,15,A50ELEM       HAVE A(X'50' ELEM)?                          
         BNZ   ADJPREC0            YES - USE THAT                               
         LA    RE,NTDMELEM         A(NON-TRADITIONAL DEMO LIST)                 
         CLI   0(RE),0             HAVE THE ELEMENT?                            
         BNE   *+6                 YES                                          
         DC    H'0'                NO - MUST BE THERE                           
         USING NTDELEM,RE          NON-TRADITIONAL DEMO LIST DSECT              
ADJPREC0 LA    R1,NTDDMONM         A(FIRST DEMO)                                
         AR    R1,RF               INDEX TO CURRENT DEMO                        
         BCTR  R1,0                -1 SO NEXT INSTRUCTION IS SEAMLESS           
         DROP  RE                  DROP USING                                   
*                                                                               
ADJPREC1 DS    0H                                                               
***      CLI   1(R1),C'R'          RATING?                                      
***      BE    *+12                YES - ONLY RATINGS GET 2 DEC                 
***      CLI   1(R1),C'E'          EXTENDED RATING?                             
***      BNE   ADJPX               NO - DONE                                    
                                                                                
         MVC   BYTE,ADJDEMO        SAVE OVERRIDE/2DEMO FLAGS                    
         NI    ADJDEMO,X'3F'       DROP FLAGS FROM VALUE                        
*                                                                               
         TM    BYTE,X'40'          DEMO HAVE 2 DEC                              
         BO    ADJPREC2            YES                                          
*                                                                               
         CLI   1(R1),C'R'          RATING?                                      
         BE    *+8                 YES                                          
         CLI   1(R1),C'E'          EXTENDED RATING?                             
         BE    *+12                YES                                          
         TM    SBEFLAG9,SBE92DEC   REPORT SUPPORTS 2 DEC IMPRESSIONS?           
         B     *+8                 GO TEST CC                                   
         TM    SBEFLAG4,SBE42DEC   REPORT SUPPORT 2 DEC                         
         BZ    ADJPREC4             NO - LEAVE IT ALONE                         
*                                                                               
         L     R0,ADJDEMO          ADJUST 1 DECIMAL PRECISION TO 2              
         MHI   R0,10                                                            
         ST    R0,ADJDEMO                                                       
         B     ADJPREC4                                                         
*                                                                               
* DEMO HAS 2 DEC                                                                
*                                                                               
ADJPREC2 CLI   1(R1),C'R'          RATING?                                      
         BE    *+8                 YES                                          
         CLI   1(R1),C'E'          EXTENDED RATING?                             
         BE    *+12                YES                                          
         TM    SBEFLAG9,SBE92DEC   REPORT SUPPORTS 2 DEC IMPRESSIONS?           
         B     *+8                 GO TEST CC                                   
         TM    SBEFLAG4,SBE42DEC   USER WANT 2 DEC                              
         BNZ   ADJPREC4             YES - LEAVE IT ALONE                        
*                                                                               
         L     R0,ADJDEMO          ADJUST 2 DECIMAL PRECISION TO 1              
         SRDA  R0,31               R1 = 2*FULL                                  
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,ADJDEMO                                                       
*                                                                               
ADJPREC4 TM    BYTE,X'80'          TEST OVERRIDE FLAG SET                       
         BZ    *+8                                                              
         OI    ADJDEMO,X'80'       SET IT BACK ON                               
*                                                                               
ADJPX    J     EXIT                                                             
*                                                                               
INSPDEM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
*                                                                               
         LA    RE,DEMOS            A(DEMO LIST)                                 
         NI    SPLKOPT2,X'FF'-SPLKOPT2_COMS-SPLKOPT2_NLSN-SPLKOPT2_ACT          
*                                                                               
INSPD10  CLI   0(RE),X'FF'         END OF DEMO LIST?                            
         BE    INSPD20             YES - DONE                                   
         CLI   2(RE),0             COMSCORE DEMO?                               
         BNE   *+8                 NO                                           
         OI    SPLKOPT2,SPLKOPT2_COMS  YES - FLAG COMSCORE DEMO                 
         CLI   2(RE),0             COMSCORE DEMO?                               
         BNH   *+8                 YES                                          
         OI    SPLKOPT2,SPLKOPT2_NLSN  NO - FLAG NIELSON DEMO                   
         AHI   RE,3                BUMP TO NEXT DEMO                            
         B     INSPD10             TEST NEXT DEMO                               
*                                                                               
INSPD20  CLC   =C'ACT',SBQBOOK     ACTUAL BOOK LOOKUP?                          
         BNE   *+8                 NO                                           
         OI    SPLKOPT2,SPLKOPT2_ACT  YES - SET ACTUAL BOOK FLAG                
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         JZ    *+8                 NO                                           
         NI    SPLKOPT2,X'FF'-SPLKOPT2_NLSN YES - TURN OFF NIELSON              
         ICM   R4,15,SPLKXTND      HAVE EXTENSION AREA?                         
         BZ    INSPDEMX            NO                                           
         USING SPLKXTD,R4          EXTENSION AREA DSECT                         
         MVC   SPXTSTDT,SBBQSTP    REQUEST PERIOD START DATE                    
         MVC   SPXTENDT,SBBQENDP   REQUEST PERIOD END DATE                      
         MVC   SPXTCSD,SBCOMSD     COMSCORE SUREY DATES (B/S)                   
         TM    SBEFLAG8,SBE8PP     PARENT PLUS STATION?                         
         BZ    *+8                 NO                                           
         OI    SPXTFLG2,SPXTPP     YES - SET PARENT PLUS                        
         LA    RE,NTDMELEM         X'50' ELEMENT                                
         CLI   0(RE),0             DO WE HAVE ONE?                              
         BE    INSPDEMX            NO - WE'RE DONE                              
         STCM  RE,15,SPXT50EL      A(X'50' ELEMENT)                             
         DROP  R4                  DROP USING                                   
*                                                                               
INSPDEMX J     EXIT                DONE                                         
***********************************************************************         
* SUB-ROUTINE TO BUILD A LIST OF ACTUAL BOOKS FOR RE-RATE LOOKUP IN   *         
* ACTBOOKS ON EXIT ACTS SET TO N'BOOKS                                *         
***********************************************************************         
BLDBK    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         LM    R2,R4,ASPTTAB       R2=ASPTTAB R3=NSPTTAB R4=LSPTTAB             
         USING SPTTABD,R2          SPOT TABLE DSECT                             
         SR    R5,R5               R5=N'ACTUAL BOOKS                            
*                                                                               
BLDBK2   OC    SPTSPOTS,SPTSPOTS   ZERO SPOTS?                                  
         BZ    BLDBK6              YES - NOTHING TO LOOKUP                      
         TM    SPTIND,SPTNODEM     DEMO LOOKUP SUPPRESSED?                      
         BO    BLDBK6              YES - NOTHING TO LOOKUP                      
         OC    SPTACTBK,SPTACTBK   ACTUAL BOOK PRESENT?                         
         BZ    BLDBK6              NO  - NOTHING TO LOOKUP                      
*                                                                               
         CLI   SBEDEMTY,C'A'       AFFID LOOKUP?                                
         BNE   BLDBK3              NO - SET BOOK FOR RE-RATE                    
         CLI   SPECIAL,YES         -S BUYLINE?                                  
         BE    BLDBK3              YES - ALWAYS SET BOOK FOR RE-RATE            
         TM    SBEFLAG8,SBE8CP1+SBE8CP2 COMSCORE PASS 1/2?                      
         BZ    BLDBK2B             NO                                           
*                                                                               
         LA    RE,DEMOS            A(DEMO LIST)                                 
*                                                                               
BLDBK2A  CLI   0(RE),X'FF'         END OF DEMO LIST?                            
         BE    BLDBK2B             YES - NO COMSCORE DEMOS FOUND                
         CLI   2(RE),0             COMSCORE DEMO?                               
         BE    BLDBK3              YES - ALWAYS PASS THE BOOK                   
         AHI   RE,3                BUMP TO NEXT DEMO                            
         B     BLDBK2A             TEST NEXT DEMO                               
*                                                                               
BLDBK2B  OC    SPTADATE,SPTADATE   TEST AFFID PRESENT                           
         BNZ   BLDBK6              YES-WILL NOT DO A RERATE                     
*                                                                               
BLDBK3   LA    RE,ACTBOOKS         ACTUAL BOOK TABLE                            
         LTR   R0,R5               SET COUNTER IN R0                            
         BZ    BLDBK4              ADD FIRST ENTRY IN TABLE                     
         CLC   SPTACTBK,0(RE)      TEST IF ALREADY IN TABLE                     
         BE    BLDBK6              YES                                          
         LA    RE,2(RE)            NEXT TABLE ENTRY                             
         BCT   R0,*-14             PROCESS NEXT SLOT IN TABLE                   
*                                                                               
BLDBK4   MVC   0(2,RE),SPTACTBK    SAVE ACTUAL BOOK IN ACTBOOKS                 
         LA    R5,1(R5)            INCREMENT ACTUAL BOOK COUNT                  
*                                                                               
BLDBK6   LA    R2,0(R4,R2)         NEXT SPOT TABLE ENTRY                        
         BCT   R3,BLDBK2           PROCESS NEXT SPOT TABLE ENTRY                
*                                                                               
BLDBK8   STC   R5,NACTS            NUMBER OF BOOKS                              
*                                                                               
BLDBKX   J     EXIT                EXIT                                         
*                                                                               
* SUB-ROUTINE TO GET CHECK NUMBER AND DATE FOR A SPOT                           
* ON ENTRY, R2 = A(SPOT TABLE ENTRY)                                            
*           R6 = A(SPOT ELEMENT)    ***** OBSOLETE *****                        
*                                                                               
GETCHK   NTR1  BASE=*,LABEL=*                                                   
         USING SPTTABD,R2          R2 = A(SPOT TABLE ENTRY)                     
         USING BUYREC,R4           R4 = A(BUY RECORD)                           
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         USING SCHUNKD,R7          R7 = A(CHUNK)                                
*                                                                               
         OC    SPTPAYDT,SPTPAYDT   TEST SPOT IS PAID                            
         BZ    GETCHKX                                                          
         CLI   SPTCLRSQ,0                                                       
         BE    GETCHKX                                                          
         ICM   R3,15,SBACHKTB      FOUND-TEST FOR CHECK TABLE                   
         BZ    GETCHKX                                                          
*                                  YES-LOOK UP CLEARANCE IN CHECK TABLE         
         L     R4,SBAIO1           R4 = A(BUY RECORD)                           
         L     RE,4(R3)                                                         
         ST    RE,DMCB+8           N'ENTRIES IN TABLE                           
         LA    RE,CHKTABL                                                       
         ST    RE,DMCB+12          L'ENTRY                                      
         L     RE,8(R3)                                                         
         ST    RE,DMCB+20          MAX NUMBER OF ENTRIES                        
         LA    R3,12(R3)           A(TABLE)                                     
*                                                                               
         XC    DUB,DUB                                                          
         LA    R5,DUB                                                           
         USING CHKTABD,R5                                                       
         MVC   CHCLDT,SPTPAYDT     PAID DATE                                    
         MVC   CHCLSEQ,SPTCLRSQ    SEQUENCE NUMBER                              
*                                                                               
         OC    SBNETWK,SBNETWK     CANADIAN NETWORK?                            
         BZ    GETC01              NO                                           
         MVC   CHSTA,SBBCNTWK                                                   
         B     GETC10                                                           
*                                                                               
GETC01   MVC   CHSTA,BUYKSTAC      STATION                                      
         CLI   CANADA,C'Y'         TEST CANADIAN AGENCY                         
         BE    *+16                NFC (NO CABLE)                               
         CLI   BUYKSTAC,X'E8'      TEST CABLE?                                  
         BL    *+8                  NO                                          
         NI    CHSTA+2,X'80'       LOOK FOR REGULAR STATION                     
*                                                                               
         LR    R6,R4               POINT TO BUY RECORD                          
         LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
         MVI   ELCDLO,X'9D'                                                     
         MVI   ELCDHI,X'9D'                                                     
GETC05   BRAS  RE,NEXTEL           DID WE FIND A CALL LETTER CHG ELEM?          
         BNE   GETC10              NOPE                                         
         USING SFXELEM,R6                                                       
         CLC   SPTPAYDT,SFXDATE    SPOT PAID DATE BEFORE SSC RUN?               
         BH    GETC05              NO, FIND NEXT CALL LETTER CHANGE             
         MVC   CHSTA,SFXSTA                                                     
         DROP  R6                                                               
*                                                                               
GETC10   GOTO1 SBABINSR,DMCB,DUB,(R3),,,(0,6)                                   
         CLI   0(R1),1                                                          
         BE    GETCHKX                                                          
         ICM   R5,15,0(R1)                                                      
         BZ    GETCHKX                                                          
*                                                                               
         MVC   SCCHKNUM,CHCHKNUM   EXTRACT CHECK NUMBER                         
         MVC   SCCHKDT,CHCHKDT     AND CHECK DATE                               
         TM    CHSTATUS,X'80'      TEST CHECK IS RECONCILED                     
         BZ    *+8                                                              
         OI    SCCHKIND,SCCHKREC                                                
         MVC   SCPDREP,CHPDREP     PAID REP                                     
         MVC   SCBNKDT,CHBNKDT     BANK CLEARED DATE                            
         MVC   SCCLDISK,CHDISKA    SAVE CLRST D/A                               
*                                                                               
         TM    CHSTATUS,X'02'      HAVE X'03'/X'05' ELEMENTS?                   
         BZ    GETCHKX             NO, DONE                                     
         XC    SCCHKNUM,SCCHKNUM   CLEAR CHECK NUMBER                           
         MVC   SCCHKNUM(2),CHCLDT  PASS IN PAID DATE                            
         MVC   SCCHKNUM+2(1),CHCLSEQ    PASS IN SEQ NUMBER                      
         MVC   SCCHKNUM+3(3),XFF   INDICATE X'03'/X'05' PAIR                    
*                                                                               
GETCHKX  J     EXIT                                                             
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
         TITLE 'SPOTBUY - SPOTPAK EXTRACT MODULE'                               
* DSECT TO COVER SPOTBUY WORKING STORAGE                                        
*                                                                               
WORKD    DSECT                                                                  
APARM    DS    A                                                                
RELO     DS    A                                                                
USERRD   DS    A                   USER REGISTER 13                             
SAVERE   DS    A                                                                
*                                                                               
ADDRS    DS    0A                                                               
*                                                                               
AXTRA    DS    0F                  EXTENSION ROUTINE ADDRESSES                  
ALOOK    DS    A                                                                
AUPG     DS    A                                                                
APRDDEMS DS    A                                                                
AGETDEMS DS    A                                                                
AGETPW   DS    A                                                                
AAFFINIT DS    A                                                                
AGETTAL  DS    A                                                                
AFUSION  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
VSPGETDM DS    V                   V(SPGETDEMO)                                 
VSPBOOK  DS    V                   V(SPOTBOOK)                                  
VSPDEMUP DS    V                   V(SPDEMUP)                                   
VGETRATE DS    V                                                                
VCALLOV  DS    V                                                                
VDATAMGR DS    V                                                                
VDATCON  DS    V                                                                
VADDAY   DS    V                                                                
VGETDAY  DS    V                                                                
VPWCALC  DS    V                                                                
VSTAPACK DS    V                                                                
*                                                                               
ADDRSL   EQU   *-ADDRS             LENGTH OF ADDRS                              
*                                                                               
DMCB     DS    8F                                                               
WORK     DS    XL64                                                             
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
*                                                                               
THREE    DS    XL3                                                              
BYTE     DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
XFF      DS    XL16                                                             
*                                                                               
KEY      DS    CL32                                                             
KEYSAVE  DS    CL32                                                             
DMWORK   DS    CL96                                                             
*                                                                               
ASPTTAB  DS    A                   A(SPOT TABLE)                                
NSPTTAB  DS    F                   N'SPOT TABLE ENTRIES                         
LSPTTAB  DS    F                   L'SPOT TABLE ENTRIES                         
MAXNSPT  DS    F                   MAXIMUM N'SPOT TABLE ENTRIES                 
LCHUNK   DS    F                   L'CHUNK ENTRY                                
ATAXEL   DS    A                   A(CANADIAN TAX ELEMENT)                      
AORBEL   DS    A                   A(ORBIT ELEMENT)                             
ALSTENT  DS    A                                                                
*                                                                               
FSTDATE  DS    XL2                 FIRST DATE                                   
LSTDATE  DS    XL2                 LAST DATE                                    
LSTELEM  DS    XL2                 DATE OF LAST ELEM IN PERIOD                  
*                                                                               
BUYPRD   DS    X                   PRODUCT IN BUY KEY                           
SPECIAL  DS    C                   Y=SPECIAL                                    
UPGRADE  DS    C                   Y=UPGRADE ELEMENT PRESENT                    
PRD1     DS    X                                                                
SLN1     DS    X                                                                
CML1     DS    XL2                                                              
PRD2     DS    X                                                                
SLN2     DS    X                                                                
CML2     DS    XL2                                                              
*                                                                               
SPOTS    DS    F                   GETRATE BLOCK                                
GROSS    DS    F                                                                
NET      DS    F                                                                
TAX      DS    F                                                                
*                                                                               
SPOTS2   DS    F                                                                
GROSS2   DS    F                                                                
NET2     DS    F                                                                
TAX2     DS    F                                                                
*                                                                               
XGROSS   DS    F                   GETRATE EXCHANGE AREA                        
XNET     DS    F                                                                
XTAX     DS    F                                                                
XC58     DS    F                   ONLY IF CANADIAN $ REQUEST                   
XGSTCODE DS    CL1                                                              
XGSTRATE DS    XL3                                                              
XGSTAMT  DS    F                                                                
XGSTAMTX DS    F                                                                
XPSTTAB  DS    10CL16                                                           
XPSTTABX EQU   *                                                                
         ORG   XPSTTAB                                                          
XPSTPROV DS    CL2                 PROVINCE CODE                                
XPSTCODE DS    CL1                 PST CODE                                     
         DS    XL2                 N/D                                          
XPSTRATE DS    XL3                 PST RATE TO 5 DEC                            
XPSTAMT  DS    XL4                 PST AMOUNT IN PENNIES (CAN DOLLARS)          
XPSTAMTX DS    XL4                 PST AMOUNT IN PENIES (US DOLLARS)            
         ORG   XPSTTABX            10 ENTRIES NOT 1, STOP CRAPPER!!!            
*                                                                               
XGROSS2  DS    F                                                                
XNET2    DS    F                                                                
XTAX2    DS    F                                                                
XC582    DS    F                                                                
XGSTCOD2 DS    CL1                                                              
XGSTRAT2 DS    XL3                                                              
XGSTAMT2 DS    F                                                                
XGSTAMX2 DS    F                                                                
XPSTTAB2 DS    10CL16                                                           
XPSTTBX2 EQU   *                                                                
         ORG   XPSTTAB2                                                         
XPSTPRV2 DS    CL2                 PROVINCE CODE                                
XPSTCDE2 DS    CL1                 PST CODE                                     
         DS    XL2                 N/D                                          
XPSTRAT2 DS    XL3                 PST RATE TO 5 DEC                            
XPSTAMT2 DS    XL4                 PST AMOUNT IN PENNIES (CAN DOLLARS)          
XPSTAMX2 DS    XL4                 PST AMOUNT IN PENIES (US DOLLARS)            
         ORG   XPSTTBX2            10 ENTRIES NOT 1, STOP CRAPPER!!!            
*                                                                               
AEQTAB   DS    A                   A(EQUIVALENCE TABLE)                         
EQFACT   DS    F                   SLN EQUIVALENCY FACTOR                       
*                                                                               
OPTVALS  DS    0CL(L'SBTALOPT)     TALENT FACTOR OPTIMIZATION                   
OPTAGY   DS    CL(L'SBAGY)         AGENCY                                       
OPTMED   DS    CL(L'SBMED)         MEDIA                                        
OPTCLT   DS    CL(L'SBBCLT)        CLIENT (PACKED)                              
OPTSTDT  DS    CL(L'SBBQSTP)       REQUEST START DATE (COMPRESSED)              
OPTENDDT DS    CL(L'SBBQENDP)      REQUEST END DATE (COMPRESSED)                
         DS    CL(L'OPTVALS-(*-OPTVALS)) SPARE                                  
*                                                                               
NACTS    DS    X                   N'ACTUAL BOOKS                               
NDEMOS   DS    X                                                                
BKTYPE   DS    C                   SPECIAL BOOK TYPE                            
ALPHAMKT DS    CL3                 ALPHA MARKET                                 
PBDEMO   DS    C                                                                
SPILL    DS    C                                                                
GRIND    DS    C                   GETRATE INDICATOR                            
CURRENCY DS    C                   C= ANADIAN,U=USA                             
CANADA   DS    C                   Y=CANADIAN AGENCY                            
SVEDEMTY DS    C                                                                
ADJDEMO  DS    F                   PRECISION ADJUSTED DEMO VALUE                
DEMOS    DS    CL((MAXDEMS*3)+1)   DEMO LIST                                    
POLDEMOS DS    CL((MAXDEMS*3)+1)   POL DEMOS LIST                               
SVDEMOS  DS    CL(MAXDEMS*8)       SAVED DEMO VALUE LIST                        
DEMDSPLS DS    CL(MAXDEMS)                                                      
WGTLST   DS    CL(MAXDEMS)                                                      
POLWGTLS DS    CL(MAXDEMS)                                                      
DEMFACT  DS    XL2                                                              
DEMVALS  DS    (MAXDEMS)F                                                       
PDEMVALS DS    (MAXDEMS)F                                                       
PURVALSA DS    (MAXDEMS)F          ADJUSTED PURCHASED VALUES                    
PURVALSU DS    (MAXDEMS)F          UNADJUSTED PURCHASED VALUES                  
ORBDEMS  DS    (MAXDEMS)F                                                       
THISDEMS DS    (MAXDEMS)D                                                       
PRDLST   DS    XL256                                                            
DEMUPBLK DS    XL(SPDEMUP2)                                                     
*                                                                               
SVSBSTA  DS    CL5                                                              
SVPRD    DS    XL1                                                              
DEMCHG   DS    CL1                                                              
SVNDEMOS DS    XL1                                                              
SVBDPURP DS    C                                                                
SVBDCIND DS    X                                                                
SVBDNTAX DS    XL2                                                              
SVRE     DS    A                                                                
*                                                                               
SVSQDKEY DS    XL(SQDKEYL)         SQAD KEY SAVEAREA                            
DEMANDSW DS    X                   DEMAND CALL SWITCH                           
*                                  C'Y' - HOOK HAS BEEN CALLED AT ONCE          
SPOT     DS    CL(SPTTABL)         SPOT TABLE ENTRY                             
         DS    0F                                                               
CHUNK    DS    CL(MAXCHUNK)        CHUNK ENTRY                                  
ACTBOOKS DS    CL(MAXWEEKS*2)                                                   
*                                                                               
FLAGS    DS    XL1                 VARIOUS FLAGS                                
DONTDIE  EQU   X'80'               DON'T DIE ON SPTTAB FULL                     
ECPWTAX  EQU   X'40'               CALCULATING EFF CST ON PW TAX                
SKPAFFDS EQU   X'20'               SKIP AFFIDS IN SPTTAB (FOR FIS)              
LPMBOOK  EQU   X'10'               HAVE AN LPM BOOK                             
LPMBKOV  EQU   X'08'               THIS IS AN OVERNIGHT BOOK                    
AFFIDFL  EQU   X'04'               RERATING DUE TO AFFID FAILURE                
*        EQU   X'02'               UNUSED                                       
*        EQU   X'01'               UNUSED                                       
*                                                                               
CDEMSTA  DS    XL5                 STA OVERRIDE FOR CANADIAN SOFT DEMOS         
CDEMFLG  DS    XL1                 BBM/NSI/IMP FLAG FOR CAN SOFT DEMOS          
SVRTGSVC DS    XL1                 SAVED RATING SERVICE FOR SOFT DEMOS          
*                                                                               
PWBLOCK  DS    XL48                                                             
*                                                                               
PPSTVAL  DS    CL10                PRD PST/MAIN PST STRING                      
CPSTVAL  DS    CL10                CLT PST/MAIN PST STRING                      
SPOTMOS  DS    XL2                 SPOT MOS                                     
MAINPST  DS    CL1                 MAIN CLT/PRD PST INDICATOR                   
SPOTDATE DS    XL2                 SPOT DATE                                    
*                                                                               
DEMERR   DS    XL1                 DEMO ERR ON ACHIEVED NIELSON LOOKUP          
*                                                                               
ADEMLIST DS    F                   A(DEMO LIST) FOR ADJPREC                     
A50ELEM  DS    F                   A(X'50' ELEM) FOR ADJPREC                    
NTDMELEM DS    XL182               FAKE 50 ELEM IF MENU/DEMOS OPT USED          
SAVEDEMS DS    XL(MAXDEMS*8)       SAVED DEMOS FOR AFFID HOOK                   
*                                                                               
ORIGBOOK DS    XL2                 ORIGINAL BOOK ON RERATE CALL                 
PASS     DS    XL1                 FOR MULTIPLE PASSES                          
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         SPACE 2                                                                
DBLOCKC  DS    XL256               DBLOCK AREA                                  
IOAREA   DS    CL2000                                                           
*                                                                               
WORKX    EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE SPOTTABD                                                       
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXDEMS  EQU   20                                                               
MAXWEEKS EQU   60                                                               
MAXCHUNK EQU   ((SCDEMOS-SCHUNKD)+(MAXDEMS*L'SCDEMOS))                          
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         EJECT                                                                  
SBLOCKD  DSECT                                                                  
       ++INCLUDE SPOTBLOCK                                                      
         EJECT                                                                  
* SPGENBUY                                                                      
*        PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DEDBEXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENAGY                                                                      
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENMKT                                                                      
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENSTA                                                                      
         PRINT OFF                                                              
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENTAL                                                                      
         PRINT OFF                                                              
TALRECD  DSECT                                                                  
       ++INCLUDE SPGENTAL                                                       
         PRINT ON                                                               
* SPGENWIPW                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENWIPW                                                      
         PRINT ON                                                               
* SPPWBLOCK                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPPWBLOCK                                                      
         PRINT ON                                                               
* DDBSRPRMD                                                                     
         PRINT OFF                                                              
BSPPRMSD DSECT                                                                  
       ++INCLUDE DDBSRPRMD                                                      
         PRINT ON                                                               
* SPSQDBUFFD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPSQDBUFFD                                                     
         PRINT ON                                                               
* SPDEMLKXTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPDEMLKXTD                                                     
         PRINT ON                                                               
* SPSTAPACKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPSTAPACKD                                                     
         PRINT ON                                                               
* SPGETBFRD                                                                     
SPGBFRDD DSECT                                                                  
       ++INCLUDE SPGETBFRD                                                      
SVBFKEY  DS    XL(SPGBACOM-SPGBAM)                                              
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066SPOTBUY   02/24/21'                                      
         END                                                                    
