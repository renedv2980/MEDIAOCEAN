*          DATA SET SPWRI0C    AT LEVEL 167 AS OF 02/22/21                      
*PHASE T2040CA,*                                                                
         TITLE 'T2040C - SPOTPAK WRITER GENERAL ROUTINES'                       
*                                                                               
***********************************************************************         
*                                                                     *         
*          SPWRI0C (T2040C) - SPOT WRITER GENERAL ROUTINES            *         
*                                                                     *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-48438  02/22/21 SUPPORT 2-DECIMAL RADIO RATINGS FOR CANADA*         
* AKAT SPEC-42845  02/22/21 NEW MARKET LOCK OPTIONS FOR SL            *         
* AKAT SPEC-43482  10/20/20 SUPPORT NEW UBK OPTION FOR UNIVERSE BOOK  *         
* AKAT SPEC-44870  04/02/20 NEW MCSTA16 MACRO                         *         
* AKAT SPEC-30530  11/08/19 SUPPORT FOR 2 DECIMAL IMPRESSIONS         *         
* AKAT SPEC-34636  11/07/19 SUPPORT COMSCORE DEMO INDEX               *         
* AKAT SPEC-23936  06/17/19 NEW DARE MAKEGOODS KEYWORDS SUPPORT       *         
* AKAT SPEC-28127  06/17/19 SUPPORT NEW LKUP=ALL OPTION               *         
* AKAT SPEC-24962  06/17/19 WEEKLY DATA FOR POSTS OVERRIDE OPTION     *         
* AKAT SPEC-27405  06/17/19 ALLOW PGROUP REQUEST FOR MULI-CLIENT REQ  *         
* AKAT SPEC-28262  02/19/19 FIX UCOMM/UDEF BUG FOR MED * CLT SPECIFIC *         
* AKAT SPEC-25884  11/05/18 COMSCORE CUSTOM DEMOS SUPPORT             *         
* AKAT SPEC-16250  05/30/18 SUPPORT NEW COS2 KEYWORDS                 *         
* AKAT SPEC-11692  02/16/18 NEW TARGET3 & TARGET4 KEYWORD SUPPORT     *         
* AKAT SPEC-12852  02/16/18 NEW MSMINFEM, MSFCCMIN & MSFCCFEM KEYWORDS*         
* AKAT SPEC-13400  02/16/18 NEW OMRESEND KEYWORD SUPPORT              *         
* AKAT SPEC-20320  02/05/18 RE-ALLOW IMPACT DATA LOOKUP FOR RERATES   *         
* AKAT SPEC-10776  10/23/17 NEW MKTYPE KEYWORD SUPPORT                *         
* AKAT SPEC-14608  09/08/17 DISALLOW IMPACT DATA LOOKUP FOR RERATES   *         
* AKAT SPEC-12018  06/01/17 COMSCORE OVERNIGHT SUPPORT                *         
* AKAT SPEC-6939   02/24/17 SUPPORT COMSCORE DEMO LOOKUPS FOR 17.1.5  *         
* AKAT SPEC-9011   02/10/17 UPDATE TO CANADIAN NATIONAL DEMOS         *         
* AKAT SPEC-15     02/10/17 SUPPORT NEW SQAD CPM                      *         
* AKAT CUSTENH3341 10/28/16 SUPPORT NEW CANADIAN NATIONAL DEMOS       *         
* AKAT DSSTK-270   04/18/16 SUPPORT EST UCOMM 5-8                     *         
* AKAT MOSTK-20    03/17/16 NEW -CM BAND SUPPORT FOR IHEART RADIO     *         
* AKAT MOSTK-77    01/25/16 SUPPORT NEW MGDTL KEYWORD                 *         
* AKAT MOSTK-76    01/07/16 SUPPORT NEW BUYMG=Y/N OPTION              *         
* 08JUN15 146 AKT-- ADD UNSENT TO OMSTATUS TABLE                    *           
* 13APR15 145 AKT-- 2-DECIMAL DEMO SUPPORT FOR MEDIA X              *           
* 24MAR15 144 AKT-- NEW MOFLOW OPTION                               *           
* 28JAN15 143 AKT-- SUPPORT NEW BILINVMY KEYWORD                    *           
* 13MAY14 142 AKT-- SUPPORT NEW INVCMAT KEYWORD                     *           
* 04SEP13 141 AKT-- SUPPORT 2-DECIMAL DEMOS FOR NETWORK             *           
* 27AUG13 140 AKT-- SUPPORT NEW NTYPE KEYWORD                       *           
* 01MAY13 139 AKT-- SUPPORT NEW INVAGYID KEYWORD                    *           
* 15MAR13 138 AKT-- NOFLOAT OPTION TO HONOR MINUS=YES FOR DOWNLOADS *           
*                -- ALL-DV AND ALL-SM SUPPORT                       *           
* 07DEC12 137 AKT-- PASS EDAILY FLAG FOR SL REPORT                  *           
* 24SEP12 136 AKT-- SUPPORT NEW INVREP KEYWORD AND FILTER           *           
* 22AUG12 135 AKT-- SUPPORT NEW SQADLEN, SQAD3 AND SQAD4 OPTIONS    *           
* 22MAY12 134 AKT-- SRDS SUPPORT                                    *           
* 13MAR12 133 AKT-- MOVE MKTNSI AND MKTARB HIGHER ON LEVTABLE       *           
* 26JAN12 132 AKT-- SUPPORT NEW BUYBOOK AND OMEMADD KEYWORDS        *           
* 05DEC11 130 AKT-- SUPPORT NEGATIVE SREP FILTER AND SET BILL COST2 *           
*                -- FLAG FOR NEW BILLC2 AND BILLNTC2 KEYWORDS       *           
* 28NOV11 129 AKT-- SUPPORT SQADTYP=Q                               *           
* 21NOV11 128 AKT-- SUPPORT NEW RATETYPE KEYWORD AS A ROW           *           
* 28OCT11 127 AKT-- SUPPORT NEW CLINV KEYWORD AS A ROW              *           
* 24OCT11 126 AKT-- SET IGNORE OOW FLAG ON ESTIMATE VALIDATION      *           
* 04MAY11 125 AKT-- SET PROGRAM EXCHANGE READ FLAGS                 *           
* 06APR11 124 AKT-- NEW MFID# KEYWORD SUPPORT                       *           
* 22MAR11 123 AKT-- SUPPORT NEW MATCHCOM=N FILTER                   *           
* 14FEB11 122 AKT-- NEW -OOW OPTION TO IGNORE ESTIMATES OOWR        *           
* 16DEC10 121 AKT-- PREVENT ORIG OPTION FOR SL REPORT               *           
* 17AUG10 120 AKT-- NEW ESTFILT=NP OPTION                           *           
* 14JUL10 119 AKT-- DISALLOW OPTIONS SVC=NSI/OL AND SVC=NSI/OS      *           
* 09APR10 118 AKT-- SET INDICATOR FOR CML CLASS NAME KEYWORD        *           
* 25MAR10 117 AKT-- MEDIA FRAMEWORKS SUPPORT                        *           
* 11JAN10 116 AKT-- DON'T DIE IF STAPERR=X'04' (INVALID CBL NTWK)   *           
* 09SEP09 115 AKT-- USE SBQREQST & SBQREQND FOR PERVERT CALL        *           
* 12JUN09 114 AKT-- NEW OMFRTSNT KEYWORD SUPPORT                    *           
* 31MAY09 113 AKT-- NEW DEMO OPTION TYPE KEYWORD                    *           
* 06MAY09 112 AKT-- SUPPORT NEW LNBKTYPE KEYWORD AS A ROW           *           
* 15APR09 111 AKT-- NEW MTRADE=Y/N OPTION                           *           
* 12APR09 110 AKT-- DON'T LOOK UP MKT FOR ALL MKT REQUEST           *           
* 14JAN09 109 AKT-- NEW BFORM OPTION                                *           
* 28JUL08 108 AKT-- NEW HDEF AND CNTR KEYWORD SUPPORT               *           
* 08SEP08 107 AKT-- BK5 AND BK6 OPTION                              *           
* 02JUN08 106 AKT-- NEW BTIMES OPTION                               *           
* 25APR08 105 AKT-- SUPPORT NEW ACOM HEADER MODIFIER                *           
* 16APR08 104 AKT-- SUPPORT NEW BHINVM KEYWORD AS A ROW             *           
* 27FEB08 102 AKT-- NEW UCOM=POL FILTER                             *           
* 27JAN08 101 AKT-- SUPPORT NEW ESTBKTYP KEYWORD AS A ROW           *           
* 19DEC07 100 AKT-- GM BILLING RESTRUCTURE SUPPORT                  *           
* 30NOV07 99 AKT -- OM/BUY MERGE SUPPORT                            *           
* 06NOV08 98 EFJ -- OPTION FILE TO SEND SOON OUTPUT DATA TO FILE    *           
* 04OCT07 97 AKT -- SUPPORT S-E (START TO END) DATE FILTERS         *           
* 13JUL07 96 AKT -- SUPPORT NEW MSTREET KEYWORDS AS A ROW           *           
* 05JUL07 95 AKT -- NEW REPORT2M OPTION TO VALIDATE SECOND REPORT   *           
*                -- STARTING AT THE MEDIA FIELD                     *           
* 20JUN07 94 AKT -- FIX MKT LIMIT ACCESS BUG IN OFFICER CALL        *           
* 09MAY07 94 AKT -- DISABLE MOST UP-FRONT SECURITY                  *           
* 24APR07 93 BPOO-- ALLOW OVN OPTION WITHOUT WEEKLY OPTION          *           
* 27FEB07 92 AKT -- DDSOFC NOW DEFUNCT                              *           
* 01FEB07 91 AKT -- SUPPORT OMLSTSNT KEYWORD AS A ROW AND DO NOT    *           
*                -- ALLOW SVC=NSI/O AS AN OPTION                    *           
* 22JAN07 90 AKT -- COLUMN DATE FILTER LOGIC NOW BASED ON REQUEST   *           
*                -- START DATE RATHER THAN JAN01                    *           
* 13DEC06 89 AKT -- SUPPORT INVFLAGS KEYWORD AS A ROW               *           
* 14NOV06 88 EFJ -- TWO CHARACTER BOOK TYPES                        *           
* 15SEP06 87 AKT -- SUPPORT NEW GLKDATE & ORDDATE KEYWORDS AS ROWS  *           
* 05OCT06 86 AKT -- SUPPORT NEW ERATE FILTER AND KEYWORD            *           
* 06JUL06 85 AKT -- SUPPORT NEW EQLEN KEYWORD AS A ROW              *           
* 17JUL06 84 EFJ -- SUPPORT CHAR DOWNLOADS > 128 CHARS              *           
* 08JUN06 81 AKT -- NEW ESTLOCK FILTER                              *           
* 17MAY06 80 AKT -- SUPPORT ORBIT KEYWORD AS A ROW                  *           
* 06APR06 79 AKT -- GIVE NOTAUTH ERR AFTER OFFICER FOR 1 CHAR OFFICE*           
* 06FEB06 78 AKT -- FIX TERRIBLE ADJ FILTER BUG                     *           
* 30JAN06 77 AKT -- SET SBQCLT TO AAN                               *           
* 16JAN05 76 AKT -- SUPPORT NEW BYDEMSRC KEYWORD AS A ROW           *           
* 16NOV05 75 AKT -- NEW INVOICE KEYWORD SUPPORTED AS ROWS           *           
* 16NOV05 74 AKT -- NEW NOVOID OPTION TO SUPRESS VOID CHECKS        *           
* 14SEP05 73 AKT -- NEW CBL OPTION                                  *           
* 13SEP05 72 EFJ -- FIX BUG INTRODUCED WITH 2 CHAR OFFICES          *           
* 20JUL05 65 EFJ -- 2 CHARACTER OFFICE CODE SUPPORT                 *           
* 09AUG05 64 AKT -- NEW ACTDATE FILTER                              *           
* 15JUN05 63 AKT -- SUPPORT LPMWK AND LPMOV OPTIONS                 *           
* 27JUN05 62 AKT -- 2 DECIMAL DEMOS ONLY FOR MEDIA T AND *          *           
* 20APR05 61 AKT -- SUPPORT DEMROUND=1 OPTION                       *           
* 07MAR05 60 AKT -- READ 00 PROFILE FOR DEMROUND OPTION             *           
* 14DEC05 59 EFJ -- ADD INCRDATE TO LEVTABLE                        *           
* 08DEC04 58 EFJ -- JUST CHANGED PHASE CARD                         *           
* 07DEC04 57 AKT -- PUT SR35 BACK IN RIGHT PLACE (MERGE BUG)        *           
* 07APR04 56 EFJ -- SUPPORT FOR 2 DECIMAL DEMOS                     *           
*                -- SET FLAG FOR STATION DETAILS FOR BYIDMG         *           
* 06OCT04 55 AKT -- DON'T CALL PERVERT FOR RLP                      *           
* 21SEP04 54 AKT -- LPM=NO OPT TO SUPRESS LPM DATE FROM MKT REC     *           
* 23AUG04 53 AKT -- AD-ID AS ROW & GIVE ERR IF MIXED W/CMML CODE    *           
* 12AUG04 52 AKT -- CONTINUE OPTION SUPPORTS 2ND SET OF COL FILTERS *           
* 08JUL04 51 AKT -- SET SBQ2READ IF HAVE OM REC & SET OM FILTERS    *           
* 08JUN04 50 EFJ -- SUPPORT PER=FF (FISCAL FOLLIES) OPTION          *           
* 09JUN04 49 AKT -- READ SYSTEM ACCESS RECORD ONE TIME FOR MSTREET  *           
* 08JUN04 48 PWE -- EXTRA NETSIZE FILTERS (NOW THEY TELL ME!)       *           
* 03JUN04 47 PWE -- NETSIZE FILTER                                  *           
* 12MAY04 46 AKT -- ETYPE=U/-U FILTER AND BHTYPE=ORD/-ORD/CLR/-CLR  *           
* 05MAY04 45 AKT -- CHANGING INVALID DATE FILTERS WILL NOW HANDLE   *           
*                   LEADING ZEROS & 1 DAY REQ PER WITH WEEK FILTERS *           
* 16APR04 44 AKT -- ETYPE KEYWORD AS ROW                            *           
* 14APR04 43 PWE -- MASDPT= COLUMN FILTER                           *           
* 25MAR04 42 AKT -- GIVE ERROR WHEN COMBINED ROWS ARE TOO WIDE      *           
* 18MAR04 41 AKT -- FIX CLT STRING SEC BUG AT LEVEL 39              *           
* 11MAR04 40 AKT -- FIX MGROUP X'40' BUG                            *           
* 11FEB04 39 AKT -- CHANGE INVALID DATE FILTERS ON THE FLY & GET RID*           
*                   OF BLDSECRT CODE TO TEST CLIENT LIST IN VVALCLT *           
* 29JAN04 38 PWE -- SHOW K/W (SHOWCODE - CANADA NETWORK)            *           
* 12JAN04 37 EFJ -- MOVE REQUEST SIZE (REQSML) CODE TO SPWRI01      *           
* 05JAN04 36 AKT -- VALPEX NOW COMPARES DATE FILTER TO REQ PERIOD   *           
* 08DEC03 35 AKT -- 2 MORE MSTREET KEYWRDS (SET FLAG & ALLOW AS ROW)*           
* 21NOV03 34 EFJ -- SET REQSML FOR 'ALL' CLIENT REQ                 *           
* 18NOV03 33 AKT -- DO NOT FILL IN MARKET FOR ALL CLIENT REQUEST    *           
* 10NOV03 32 AKT -- NEW BCOM OPTION                                 *           
* 06NOV03 31 PWE -- FIX AFFID DATA FOR CANADA NETWORK (NWKBUYS OPT) *           
* 03NOV03 30 AKT -- 2-CHAR CLIENT GROUP (& FIX X'40' BUG)           *           
* 25OCT03 29 AKT -- READ SPOT'S SYS NUM FOR MSTREET NOT SSB NUM     *           
* 21OCT03 28 AKT -- NEW ACTUAL BOOK OPTION (ACTBK)                  *           
* 08OCT03 27 AKT -- SET FLAG IN SETREAD IF WE HAVE MSTREET KEYWORD  *           
*                   SUPPORT MSTREET KEYWORDS AS ROWS                *           
* 02OCT03 26 EFJ -- NEW DOWNFIX OPTION                              *           
* 22SEP03 25 EFJ -- CHAIN RPTS IN SAME PQ ENTRY                     *           
* 24SEP03 24 PWE -- PST                                             *           
* 18SEP03 23 AKT -- PASS ASECBLK TO DRSECBLK IF NEW SEC             *           
* 15SEP03 22 EFJ -- SUPPORT NEW KWD - ECREATE                       *           
* 15SEP03 21 ??? -- ???                                             *           
* 22AUG03 20 EFJ -- NEW DATE OPTION MMMDD/YY                        *           
* 14AUG03 19 AKT -- ACCESS AWARE - SECRET CALLED PASS DRONE ASECBLK *           
* 16JUL03 18 EFJ -- SUPPORT MORE 2 CHARACTER MGRP CODES             *           
* 29MAY03 16 EFJ -- FIX ERROR CHECKING ON RETURN FROM UPVAL         *           
* 24APR03 14 AKT -- FIX PERIOD VALIDATION                           *           
* 15APR03 13 AKT -- MUST SET ALL PREVIOUS BOOKS STARTING WITH BK1   *           
* 09APR03 13 EFJ -- NEW OPTION SQADTYP FOR HISPANIC BOOKS           *           
* 06MAR03 12 EFJ -- NEW FILTER FOR BUYS WITH POST BUY DEMOS         *           
* 05FEB03 11 EFJ -- SET GLDADDCH                                    *           
* 17JAN03 10 EFJ -- SUPPORT REPORT2 AND CONTINUED OPTIONS           *           
* 03JAN03 09 EFJ -- ALLOW GRACEFUL RETURN FROM STAPACK ERROR        *           
* 02DEC02 08 EFJ -- LEVTABLE ENTRY FOR BLANK                        *           
* 18NOV02 07 EFJ -- MAKE SURE SOMETHING IS ENTERED WITH PROG= FILT! *           
* 23OCT02 06 EFJ -- '-S' TYPE FILTER                                *           
* 10OCT02 05 EFJ -- SUPPORT 2 CHARACTER MARKET GROUP SCHEMES        *           
* 04OCT02 04 EFJ -- NEW EASI INVOICE FILTERS                        *           
* 06SEP02 03 EFJ -- CHANGE NMOD TO NTR - DOESN'T KILL RC            *           
* 23AUG02 02 EFJ -- DON'T USE R1 TO RESOLVE ADDRS!                  *           
* 21AUG02 01 EFJ -- CHANGE FROM SPWRIGEN TO SPWRI0C                 *           
*-------------------------------------------------------------------*           
* NOTE: HISTORY PRIOR TO 16MAY02 IS AT BOTTOM OF SOURCE CODE        *           
*                                                                   *           
* 15JUL02 10 EFJ -- LEVTABLE ENTRY FOR INVSTAT                      *           
* 09JUL02 09 EFJ -- CHANGE ETYPE FILTER AGAIN                       *           
* 03JUL02 08 EFJ -- CHANGE ETYPE FILTER                             *           
* 10JUN02 07 EFJ -- BONUS FILTER                                    *           
*                -- CLTACC KEYWORD                                  *           
* 03JUN02 06 EFJ -- STATION LOCKIN DOLLARS IN STACK                 *           
*                -- ADD EXTRA BYTE TO STACKTAB                      *           
*         05     -- FIX SREP CODE                                   *           
* 28MAY02 04 EFJ -- SUPPORT ETYPE FILTER                            *           
*                -- BONUS KEYWORD                                   *           
* 23MAY02 03 EFJ -- FIX BUG IN COL OPTS FROM LEVEL 2                *           
* 16MAY02 02 EFJ -- LEVEL RESET                                     *           
*                -- PAGE OPTION                                     *           
*                -- REMOVE ALL AI MACROS!                           *           
*                -- BDREP FIXES (RCPACK)                            *           
*                                                                   *           
*********************************************************************           
         EJECT                                                                  
T2040C   CSECT                                                                  
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
WWRI0C   NTR1  BASE=*,LABEL=*                                                   
         LR    RA,RB                                                            
         AHI   RA,4096                                                          
         LR    R7,RA                                                            
         AHI   R7,4096                                                          
         USING WWRI0C+4096,RA,R7                                                
         USING GEND,RC                                                          
*                                                                               
         CNOP  0,4                                                              
         B     *+8                                                              
         DC    A(*)                                                             
         LA    R8,*-4                                                           
         S     R8,*-8                                                           
         ST    R8,RELO                                                          
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    R6,R6                                                            
         L     R8,=A(EXTRA)                                                     
         A     R8,RELO                                                          
         ST    R8,AXTRA(RE)                                                     
         STC   R6,AXTRA(RE)                                                     
         LA    R6,1(R6)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         L     R9,ASYSD                                                         
         LR    R8,R9                                                            
         AHI   R8,4096                                                          
         USING SYSD,R9,R8                                                       
         USING GETTXTD,GETTXTCB                                                 
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VVALUSER                                                         
         B     VVALMED                                                          
         B     VVALCLT                                                          
         B     VVALPRD                                                          
         B     VVALEST                                                          
         B     VVALMKT                                                          
         B     VVALSTAT                                                         
         B     VVMSPACK                                                         
         B     VVMSUNPK                                                         
         DC    AL4(0)              DEAD ROUTINE                                 
         DC    AL4(0)              DEAD ROUTINE                                 
         B     VVALFILT                                                         
         B     VVALPER                                                          
         B     VVALOPTS                                                         
         B     VVALTITS                                                         
         B     VVALOTH                                                          
         B     VINTDRON                                                         
         B     VWRPDRON                                                         
         DC    AL4(0)              DEAD ROUTINE                                 
         B     VVALHEAD                                                         
         B     VVALMID                                                          
         B     VVALROWS                                                         
         B     VVALCOLS                                                         
         B     VCURSERR                                                         
         SPACE 2                                                                
RELO     DS    F                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* USER AGENCY                                                                   
*                                                                               
VVALUSER MVC   SBQAGY,AGENCY                                                    
         MVC   AGYSIGN,BLANKS                                                   
         XC    AGYALPHA,AGYALPHA                                                
         MVI   AGYNUM,X'FF'        ASSUME NOT NUMERIC                           
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         L     R1,ATWA                                                          
         MVC   CTIKID+8(2),10(R1)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R4,CTIDATA                                                       
         SR    R3,R3                                                            
*                                                                               
VUSER1   CLI   0(R4),0                                                          
         BE    VUSER3                                                           
         CLI   0(R4),X'36'                                                      
         BE    VUSER2A                                                          
         CLI   0(R4),X'02'                                                      
         BE    VUSER2B                                                          
         CLI   0(R4),X'06'                                                      
         BE    VUSER2C                                                          
*                                                                               
VUSER1A  IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     VUSER1                                                           
*                                                                               
         USING CTORGD,R4                                                        
VUSER2A  L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         B     VUSER1A                                                          
         DROP  R1                                                               
*                                                                               
         USING CTDSCD,R4                                                        
VUSER2B  MVC   AGYSIGN,CTDSC                                                    
         MVC   WORK(3),AGYSIGN+2                                                
         NC    WORK(3),=X'F0F0F0'                                               
         CLC   WORK(3),=X'F0F0F0'                                               
         BNE   VUSER1A                                                          
         MVC   AGYNUM,AGYSIGN+2                                                 
         B     VUSER1A                                                          
*                                                                               
         USING CTAGYD,R4                                                        
VUSER2C  MVC   AGYALPHA,CTAGYID                                                 
         B     VUSER1A                                                          
*                                                                               
VUSER3   XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         GOTO1 GETFACT,DMCB,(0,0)                                               
         L     R4,DMCB                                                          
         USING FACTSD,R4                                                        
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   SBAUTL,FAAUTL                                                    
*                                                                               
***      MVI   DDSOFC,C'N'                                                      
***      CLI   FAOFFICE+3,C'*'                                                  
***      BNE   VUSERX                                                           
***      MVI   DDSOFC,C'Y'                                                      
         DROP  R4                                                               
*                                                                               
VUSERX   B     XIT                                                              
         EJECT                                                                  
* VALIDATE MEDIA                                                                
*                                                                               
VVALMED  GOTO1 ANY                                                              
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R5,KEY                                                           
         USING AGYHDRD,R5                                                       
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R5,AIO1                                                          
         ST    R5,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   SBAGYREC,0(R5)      SAVE AGENCY RECORD                           
         MVC   SBQAGY,AGENCY                                                    
         SR    R0,R0                                                            
         SR    R4,R4                                                            
         BCTR  R4,0                                                             
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         B     VM2+4                                                            
*                                                                               
VM2      BAS   RE,NEXTEL                                                        
         BNE   VM8                                                              
         CLI   8(R2),C'*'          TEST MEDIA T AND R & X REQUEST               
         BNE   VM6                                                              
         CLI   2(R5),C'T'          YES-                                         
         BNE   VM4                                                              
         MVC   SBBAGYMD,3(R5)      SAVE MEDIA T VALUES                          
         MVC   SBMEDNM,4(R5)                                                    
         LA    R4,1(R4)                                                         
         B     VM2                                                              
*                                                                               
VM4      CLI   2(R5),C'R'                                                       
         BNE   VM5                                                              
         LA    R1,SYSD             SAVE MEDIA R NAME                            
         AHI   R1,SBMEDNMR-SYSD                                                 
         MVC   0(L'SBMEDNMR,R1),4(R5)                                           
         LA    R4,1(R4)                                                         
         B     VM2                                                              
*                                                                               
VM5      CLI   2(R5),C'X'                                                       
         BNE   VM2                                                              
         LA    R1,SYSD             SAVE MEDIA X NAME                            
         AHI   R1,SBMEDNMX-SYSD                                                 
         MVC   0(L'SBMEDNMX,R1),4(R5)                                           
         LA    R4,1(R4)                                                         
         B     VM2                                                              
*                                                                               
VM6      CLC   2(1,R5),8(R2)                                                    
         BNE   VM2                                                              
         MVC   SBQMED,8(R2)        SAVE INPUT MEDIA CODE                        
         MVC   SBBAGYMD,3(R5)      AGENCY/MEDIA CODE                            
         MVC   SBMEDNM,4(R5)       MEDIA NAME                                   
         B     VMX                                                              
*                                                                               
VM8      CLI   8(R2),C'*'          TEST MEDIA T AND R                           
         BNE   VM9                 NO- INVALID MEDIA                            
         MVI   SBQMED,C'*'         YES-                                         
         LTR   R4,R4               TEST BOTH T AND R FOUND                      
         BP    VMX                                                              
*                                                                               
VM9      MVI   ERROR,INVMED                                                     
         B     TRAPERR                                                          
*                                                                               
VMX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE CLIENT                                                               
*                                                                               
VVALCLT  GOTO1 ANY                                                              
         MVI   ERROR,INVCLT                                                     
         XC    SBBCLT,SBBCLT                                                    
         XC    SBQBCLT,SBQBCLT                                                  
         CLI   WORK,C'='                                                        
         BE    VC2                                                              
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(2),(1,BLOCK),C',=,='                               
         CLI   4(R1),1                                                          
         BNE   TRAPERR                                                          
         LA    R3,BLOCK                                                         
         MVI   BYTE,0              INDICATE                                     
         CLI   0(R3),0                                                          
         BE    VC2                                                              
         CLI   0(R3),6                                                          
         BH    TRAPERR                                                          
         LLC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,GRPTEST          TEST CLIENT GROUP                            
         BNE   VC2                                                              
         CLI   1(R3),0             IS THERE 2ND HALF                            
         BE    VC2                 NO, NOT A CLIENT GROUP                       
*                                                                               
VC1      MVI   ERROR,INVCGRP       YES                                          
         LA    R4,GRPKCTYQ                                                      
         MVI   BYTE,C'N'           INDICATE NOT 2-CHAR CLIENT GROUP             
         CLI   23(R3),C'0'         2-CHAR CLIENT GROUP?                         
         BNL   VC1C                NO                                           
         CLC   =C'ALL',23(R3)      ALL SCHEMES?                                 
         BE    VC1C                YES                                          
         MVI   BYTE,C'Y'           INDICATE 2-CHAR CLIENT GROUP                 
***                                                                             
* TABLE LOOKUP                                                                  
***                                                                             
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
VC1A     CLC   22(2,R3),0(R1)      FOUND A MATCH?                               
         BE    VC1B                YES, DONE                                    
         LA    R1,3(R1)            BUMP                                         
         BCT   R0,VC1A             TEST NEXT IN TABLE                           
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
VC1B     MVC   WORK(1),2(R1)       MOVE IN THE 1 BYTE HEX VALUE                 
         MVC   WORK+1(4),24(R3)    MOVE IN THE CODE (UP TO 4 BYTES)             
*                                                                               
VC1C     BAS   RE,VALGRP                                                        
         BNE   TRAPERR                                                          
         MVC   SBQCGR,WORK                                                      
         MVC   SBCGR1LN,WORK+5                                                  
         MVC   SBCGR2LN,WORK+6                                                  
         MVC   SBQCLT,=C'ALL'                                                   
         B     VCX                                                              
*                                                                               
VC2      MVC   SBQCLT,WORK         SPACE PADDED CLIENT                          
         CLI   5(R2),6             ALLOW FOR *AA-ZZ                             
         BH    TRAPERR                                                          
         CLI   5(R2),2                                                          
         BL    TRAPERR                                                          
         MVI   ERROR,0                                                          
         L     R3,ATWA                                                          
         USING T204FFD,R3                                                       
* ALL CLIENT                                                                    
VC2B     CLC   SBQCLT,=C'ALL'      TEST ALL CLIENTS                             
         BE    VCX                                                              
*                                                                               
VC2C     CLI   SBQCLT,C'='         TEST CLIENT GROUP                            
         BE    VCX                                                              
         CLI   SBQCLT,C'$'         TEST OFFICE LIST                             
         BE    VCX                                                              
*                                                                               
VC2D     CLI   SBQCLT,C'*'         TEST OFFICE TYPE REQ                         
         BNE   VC4                                                              
*                                                                               
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T204FFD+6                                                
         MVC   OFCLMT,T204FFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,SBQCLT+1                                                 
         CLI   OFCOFC2+1,C'-'      REQUESTED A RANGE OF OFFICES?                
         BNE   *+8                 NO                                           
         MVI   OFCOFC2+1,X'40'     YES - ONLY PASS 1 CHAR OFFICE CODE           
*                                                                               
         CLI   8+1(R2),C'-'        IS THIS *-AA?                                
         BNE   *+12                                                             
         LA    RE,8+1(R2)                                                       
         B     VC2E                                                             
*                                                                               
         GOTO1 OFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                            
         LA    R1,WORK                                                          
         TM    OFCINDS,OFCIOINV    INVALID OFFICE?                              
         BNZ   VC2G                 YES                                         
         MVC   SBQCLT+1(1),OFCOFC  SAVE OFF 1 BYTE INTERNAL OFFICE CODE         
         MVI   SBQCLT+2,0                                                       
*                                                                               
         LA    RE,8+2(R2)                                                       
         CLI   0(RE),C'-'          TEST OFFICE RANGE (*AA-ZZ)                   
         BE    VC2E                 YES                                         
         AHI   RE,1                                                             
         CLI   0(RE),C'-'                                                       
         BNE   VCX                  NO                                          
*                                                                               
VC2E     MVC   OFCOFC2,1(RE)       GET 1 BYTE INTERNAL CODE OF 2ND OFC          
         OC    OFCOFC2,BLANKS                                                   
         XC    OFCOFC,OFCOFC                                                    
         GOTO1 OFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                            
         LA    R1,WORK                                                          
         TM    OFCINDS,OFCIOINV    TEST INVALID OFFICE                          
         BNZ   VC2G                 YES (NOTE ONLY INVALID, NOT UNAUTH)         
         MVC   SBQCLT+2(1),OFCOFC  SAVE OFF 1 BYTE INTERNAL OFFICE CODE         
         B     VCX                                                              
         DROP  R1                                                               
*                                                                               
VC2G     MVC   GTMSGNO,=Y(INVOFFC) OFFICE NOT ON FILE                           
         B     NEWTRAP                                                          
*                                                                               
VC4      MVI   ERROR,INVCLT                                                     
         GOTO1 CLPACK,DMCB,SBQCLT,SBBCLT   SINGLE CLIENT                        
         CLI   0(R1),0                                                          
         BNE   TRAPERR                                                          
         MVC   SBQBCLT,SBBCLT                                                   
         XC    KEY,KEY             READ CLIENT HEADER                           
         LA    R4,KEY                                                           
         USING CLTHDRD,R4                                                       
         MVC   CKEYAM,SBBAGYMD                                                  
         MVC   CKEYCLT,SBBCLT                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         TM    DMCB+8,X'12'                                                     
         BNZ   TRAPERR                                                          
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 CLUNPK,DMCB,(CPROF+6,SBBCLT),SBQCLT                              
*                                                                               
         MVC   SBCOFF,COFFICE                                                   
         LA    R1,SBLOCK                                                        
         USING SBLOCK,R1                                                        
         MVC   SBUP1TYP,CPU1TYPE                                                
         MVC   SBUP1LEN,CPU1LEN                                                 
         MVC   SBUP2TYP,CPU2TYPE                                                
         MVC   SBUP2LEN,CPU2LEN                                                 
         MVC   SBUE1TYP,CEU1TYPE                                                
         MVC   SBUE1LEN,CEU1LEN                                                 
         MVC   SBUE2TYP,CEU2TYPE                                                
         MVC   SBUE2LEN,CEU2LEN                                                 
         DROP  R1                                                               
         MVI   ERROR,0                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING DDUCOMD,RF                                                       
         MVC   UCACOMF,ACOMFACS                                                 
         MVI   UCSYS,C'S'                                                       
         MVC   UCSAM,SBBAGYMD                                                   
         MVC   UCSCLT,SBBCLT                                                    
         MVI   UCOPT,UCOTTL+UCOMKT    GET TITLES ONLY (AND MKT TITLES!)         
*                                                                               
         GOTO1 VDDUCOM,ELEM                                                     
         LA    RF,ELEM                                                          
         CLI   UCERROR,0                                                        
         BE    *+6                                                              
         DCHO                                                                   
         TM    UCDATA,UCDNOCLT     NO CLT LEVEL UCOM REC                        
         BNZ   VC10                                                             
*                                                                               
         MVC   UCDP1LEN(4),UCPMXLNS  GET PRD UCOM LENGTHS                       
         MVC   UCDP1TYP(4),UCPEDITS   AND EDIT RULES                            
         MVC   UCDE1LEN(4),UCEMXLNS  GET EST UCOM LENGTHS                       
         MVC   UCDE1TYP(4),UCEEDITS   AND EDIT RULES                            
         MVC   UCDM1LEN(4),UCMMXLNS  GET MKT UCOM LENGTHS                       
         MVC   UCDM1TYP(4),UCMEDITS   AND EDIT RULES                            
         DROP  RF                                                               
*                                                                               
VC10     TM    COPT2,COP2XEST                                                   
         BZ    *+8                                                              
         OI    OPTIND4,OPTXEST                                                  
*                                                                               
VC30     LA    R1,WORK             OFFICE LIST                                  
         XC    WORK,WORK                                                        
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T204FFD+6                                                
         MVC   OFCLMT,T204FFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCSAGMD,SBBAGYMD                                                
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCACCSC,CACCESS                                                 
         MVC   OFCCLT,SBQCLT                                                    
         MVC   OFCCLT2,SBBCLT                                                   
         OI    OFCINDS,OFCI2CSC                                                 
         MVC   OFCSECD,ASECBLK                                                  
         MVI   OFCACCSM,X'FF'       IGNORE MKT LIMIT ACCESS FOR CLT             
         DROP  R1                                                               
         GOTO1 OFFICER,DMCB,(C'N',WORK),(0,ACOMFACS)                            
         CLI   0(R1),0              ANY ERRORS?                                 
         BE    VCX                  NO, THIS CLIENT HAS ACCESS                  
*                                                                               
         MVI   ERROR,SECLOCK        SECURITY LOCKOUT FOR THIS CLIENT            
         B     TRAPERR                                                          
*                                                                               
VCX      B     XIT                                                              
         DROP  R3,R4                                                            
         SPACE 2                                                                
         EJECT                                                                  
* VALIDATE PRODUCT                                                              
*                                                                               
VVALPRD  GOTO1 ANY                                                              
         MVI   ERROR,INVPROD                                                    
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),C',=,='                              
         CLI   4(R1),1                                                          
         BNE   TRAPERR                                                          
         LA    R3,BLOCK                                                         
         CLC   =C'AAA',12(R3)                                                   
         BE    TRAPERR                                                          
         CLI   0(R3),2                                                          
         BL    TRAPERR                                                          
         CLI   1(R3),0                                                          
         BE    VP20                                                             
***      CLI   OFFLINE,C'Y'                                                     
***      BNE   TRAPERR                                                          
         CLC   12(3,R3),=C'PGR'                                                 
         BNE   TRAPERR                                                          
         MVI   ERROR,INVPGCLT                                                   
***      CLI   SBQCLT,C'='         ** THIS CODE FOR BSNY/MILLER                 
***      BE    *+12                **           AND OMNY/GF                     
***      CLI   SBBCLT,0                                                         
***      BE    TRAPERR                                                          
*                                                                               
         MVC   SBQPGRD,22(R3)      SET PRODUCT GROUP HERE                       
*                                                                               
         CLI   SBBCLT,0            MULTI-CLIENT REQUEST?                        
         BNE   *+14                NO                                           
         MVC   SBQPRD,=C'ALL'      YES - SET ALL PRODUCTS                       
         B     VP07                ALLOW THROUGH W/O VALIDATION                 
*                                                                               
         MVI   ERROR,INVPGRP       PRODUCT GROUP                                
***      MVC   SBQPGRD,22(R3)                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRGRECD,R4                                                       
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,SBBAGYMD                                                
         MVC   PRGKCLT,SBBCLT                                                   
         CLI   SBQCLT,C'='         ** THIS CODE FOR SPECIAL                     
         BNE   VP04                ** CLIENT GROUPS                             
         LA    R1,CLTGRPTB                                                      
*                                                                               
VP02     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   AGENCY,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     VP02                                                             
         MVC   PRGKCLT,2(R1)                                                    
         B     VP04                                                             
*                                                                               
CLTGRPTB DC    CL2'BS',XL2'B167'   BSNY/MILLER                                  
         DC    CL2'OM',XL2'98BF'   OMNY/GF                                      
         DC    X'00'                                                            
*                                                                               
VP04     MVC   PRGKID,SBQPGRD                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         TM    DMCB+8,X'12'                                                     
         BNZ   TRAPERR                                                          
         L     R5,AIO1                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,24(R5)                                                        
         SR    RE,RE                                                            
*                                                                               
VP06     CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),1             PRDGRP BREAK DESCRIPTION                     
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     VP06                                                             
         USING PRGEL01,R5                                                       
         LLC   RE,PRGBK1LN         SET PRDGRP BREAK LENGTHS                     
         STC   RE,SBPGR1LN                                                      
         LLC   RF,PRGBK2LN                                                      
         AR    RE,RF                                                            
         STC   RE,SBPGR2LN                                                      
         DROP  R5                                                               
*                                                                               
VP07     MVC   SBQPRD,=C'ALL'                                                   
         CLI   1(R3),1                                                          
         BE    VPX                                                              
         CLC   23(3,R3),=C'ALL'                                                 
         BE    VPX                                                              
         MVI   ERROR,INVPGFLT                                                   
         MVC   SBQPGRF,23(R3)                                                   
         LA    R1,SBQPGRF                                                       
         LA    RE,L'SBQPGRF                                                     
         CLI   0(R1),C'0'                                                       
         BL    VP10                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
         B     VPX                                                              
VP10     CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   TRAPERR                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,VP10                                                          
         B     VPX                                                              
*                                                                               
VP20     MVI   SBQBPRD,0           NOT PRODUCT GROUP                            
         MVI   SBQBPRD2,0                                                       
         MVC   SBQPRD,12(R3)                                                    
         CLC   SBQPRD,BLANKS                                                    
         BNH   VPX                                                              
         CLI   0(R3),3                                                          
         BNE   VP30                                                             
         CLC   SBQPRD,=C'ALL'                                                   
         BE    VPX                                                              
         CLC   SBQPRD,=C'POL'                                                   
         BNE   VP30                                                             
         TM    SBQPIND,SBQPOLSP    POL - TEST BREAK OUT PRODUCTS                
         BO    VPX                                                              
         MVI   SBQBPRD,X'FF'             NO                                     
         B     VPX                                                              
*                                                                               
VP30     DS    0H                                                               
         MVI   ERROR,INVPRCLT                                                   
         CLI   SBBCLT,0                                                         
         BE    TRAPERR                                                          
*                                                                               
         MVI   ERROR,INVPROD                                                    
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),C',=,-'                              
         CLI   4(R1),1                                                          
         BNE   TRAPERR                                                          
         LA    R3,BLOCK                                                         
         CLI   0(R3),2                                                          
         BL    TRAPERR                                                          
         CLI   0(R3),3                                                          
         BH    TRAPERR                                                          
         MVC   SBQPRD,12(R3)                                                    
         LA    R1,SBQPRD                                                        
         BAS   RE,VPRD                                                          
         MVC   SBQBPRD,SBBPRD                                                   
         CLI   1(R3),0             TEST PIGGYBACKS                              
         BE    VPX                                                              
         CLI   1(R3),2                                                          
         BL    TRAPERR                                                          
         CLI   1(R3),3                                                          
         BH    TRAPERR                                                          
         LA    R1,22(R3)                                                        
         BAS   RE,VPRD                                                          
         MVC   SBQBPRD2,SBBPRD     SET SECOND PRODUCT CODE                      
         MVC   SBBPRD,SBQBPRD                                                   
         B     VPX                                                              
*                                                                               
VPX      B     XIT                                                              
         SPACE 2                                                                
VPRD     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRDHDRD,R4                                                       
         MVC   PKEYAM,SBBAGYMD                                                  
*                                                                               
VPRD10   MVC   PKEYCLT,SBBCLT                                                   
         MVC   PKEYPRD,0(R1)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         TM    DMCB+8,X'12'                                                     
         BNZ   TRAPERR                                                          
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   SBBPRD,PCODE+1                                                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE ESTIMATE                                                             
*                                                                               
VVALEST  CLI   5(R2),0                                                          
         BNE   VE02                                                             
         TM    AUTH,X'80'          MUST BE SINGLE EST IF X'80'                  
         BZ    *+14                                                             
         MVC   GTMSGNO,=Y(ONEEST)                                               
         B     NEWTRAP                                                          
         MVI   SBQEST,1            DEFAULT IS EST=NO                            
         MVI   SBQESTND,255                                                     
         MVI   SBQSEPES,C'N'                                                    
         B     VEX                                                              
*                                                                               
VE02     MVI   ERROR,INVEST                                                     
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,='                              
         CLI   4(R1),1                                                          
         BL    TRAPERR                                                          
         CLI   4(R1),2                                                          
         BH    TRAPERR                                                          
         LA    R3,BLOCK                                                         
         CLI   0(R3),3                                                          
         BH    TRAPERR                                                          
         CLI   1(R3),0                                                          
         BNE   TRAPERR                                                          
         MVI   SBQSEPES,C'Y'                                                    
         MVI   SBQEST,1                                                         
         MVI   SBQESTND,255                                                     
         MVC   SBQESFLT,BLANKS                                                  
         TM    2(R3),X'80'                                                      
         BO    VE30                                                             
         TM    AUTH,X'80'          MUST BE SINGLE EST IF X'80'                  
         BZ    *+14                                                             
         MVC   GTMSGNO,=Y(ONEEST)                                               
         B     NEWTRAP                                                          
*                                                                               
         CLC   12(3,R3),=C'ALL'                                                 
         BNE   *+16                                                             
         CLI   4(R1),1                                                          
         BNE   TRAPERR                                                          
         B     VEX                                                              
         CLI   0(R3),2                                                          
         BNE   TRAPERR                                                          
         CLC   12(2,R3),=C'NO'                                                  
         BNE   TRAPERR                                                          
         MVI   SBQSEPES,C'N'                                                    
         CLI   4(R1),2                                                          
         BNE   VEX                                                              
*                                                                               
         LA    R3,32(R3)                                                        
         LA    RE,3                                                             
         LA    RF,SBQESFLT                                                      
         LLC   R0,0(R3)                                                         
         LA    R4,12(R3)                                                        
*                                                                               
VE10     CLI   0(R4),C'-'                                                       
         BNE   VE20                                                             
         BCT   R0,*+8                                                           
         B     TRAPERR                                                          
         LA    R4,1(R4)                                                         
         NI    0(R4),X'FF'-X'40'                                                
*                                                                               
VE20     MVC   0(1,RF),0(R4)                                                    
         LA    R4,1(R4)                                                         
         BCT   R0,*+8                                                           
         B     VEX                                                              
         LA    RF,1(RF)                                                         
         BCT   RE,VE10                                                          
         B     TRAPERR                                                          
*                                                                               
VE30     CLC   4(4,R3),=F'255'                                                  
         BH    TRAPERR                                                          
         CLC   4(4,R3),=F'1'                                                    
         BL    TRAPERR                                                          
         MVC   SBQEST,7(R3)                                                     
         MVC   SBQESTND,SBQEST                                                  
         MVI   SBQSEPES,C'N'                                                    
         CLI   4(R1),2                                                          
         BNE   VE50                                                             
         TM    AUTH,X'80'          MUST BE SINGLE EST IF X'80'                  
         BZ    *+14                                                             
         MVC   GTMSGNO,=Y(ONEEST)                                               
         B     NEWTRAP                                                          
*                                                                               
         LA    R3,32(R3)                                                        
         CLI   1(R3),0                                                          
         BNE   TRAPERR                                                          
         TM    2(R3),X'80'                                                      
         BZ    TRAPERR                                                          
         CLC   4(4,R3),=F'255'                                                  
         BH    TRAPERR                                                          
         CLC   4(4,R3),=F'1'                                                    
         BL    TRAPERR                                                          
         MVC   SBQESTND,7(R3)                                                   
         CLC   SBQEST,SBQESTND     IF RANGE OF ESTS, MAKE SURE...               
         BE    TRAPERR             ...THEY'RE DIFFERENT                         
         B     VEX                                                              
*                                                                               
VE50     DS    0H                                                               
         TM    AUTH,X'80'          POL EST HEADER MUST HAVE REQ=Y               
         BZ    VE60                                                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,SBQEST                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         TM    DMCB+8,X'12'                                                     
         BZ    *+14                                                             
         MVC   GTMSGNO,=Y(POLREQ)                                               
         B     NEWTRAP                                                          
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         TM    EFLAG1,EF1REQ       REQ=Y?                                       
*         BZ    TRAPERR                                                         
         BNZ   *+14                                                             
         MVC   GTMSGNO,=Y(POLREQ)                                               
         B     NEWTRAP                                                          
         DROP  R4                                                               
*                                                                               
VE60     CLC   SBQPRD,=C'ALL'      SINGLE ESTIMATE                              
         BE    VEX                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,SBQPRD                                                   
         MVC   EKEYEST,SBQEST                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         TM    DMCB+8,X'12'                                                     
         BNZ   TRAPERR                                                          
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   SBESTST,ESTART      SET ESTIMATE DATES                           
         MVC   SBESTND,EEND                                                     
         TM    SBEFLAG6,SBE6NOOW   IGNORE OOWR?                                 
         BZ    *+8                 NO                                           
         MVI   EOWSDAY,0           YES - CLEAR ESTIMATE'S OOWR                  
         MVC   SBESTOWD,EOWSDAY    SET OUT-OF-WEEK ROTATOR                      
         OC    ECOST2,ECOST2       THIS A COS2 ESTIMATE?                        
         BZ    *+8                  NO                                          
         OI    FLAGS,FLCOS2                                                     
         L     RF,ATWA              ATWA                                        
         CLC   =C'SL',CONREC-T204FFD(RF)                                        
         BNE   VEX                 NO                                           
         CLC   SBQEST,SBQESTND     RANGE OF ESTSIMATES?                         
         BNE   VEX                 YES                                          
         CLI   EDAILY,C'Y'         DAILY ESTIMATE?                              
         BNE   VEX                 NO                                           
         OI    FLAGS,FLEDAILY      YES - SET DAILY FLAG                         
*                                                                               
VEX      MVI   ERROR,0                                                          
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE MARKET                                                               
*                                                                               
VVALMKT  CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   SBQMKT(3),=C'ALL'                                                
         B     VKX                                                              
         MVI   ERROR,INVMKT                                                     
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),C',=,='                              
         CLI   4(R1),1                                                          
         BNE   TRAPERR                                                          
         LA    R3,BLOCK                                                         
         CLI   0(R3),4                                                          
         BH    TRAPERR                                                          
         MVC   SBQMKT(3),=C'ALL'                                                
         CLI   1(R3),0             ANY SECOND HALF FIELD?                       
         BE    VK20                 NO                                          
         LLC   RF,0(R3)             YES - IT BETTER BE A MGR                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'MGR'                                                 
         BNE   TRAPERR                                                          
         MVI   ERROR,INVMGRP       MARKET GROUP                                 
         CLI   22(R3),C'A'                                                      
         BL    TRAPERR                                                          
*                                                                               
         MVC   SBQMGRD,22(R3)                                                   
         MVC   FULL,23(R3)         SAVE REST OF MGRP (ALL OR NNNN)              
         CLC   23(3,R3),=C'ALL'                                                 
         BE    VK05                                                             
         CLI   23(R3),C'0'         IF SECOND CHAR IS NUMERIC,                   
         BL    VK01                 DON'T NEED TO LOOKUP                        
         CLI   23(R3),C'9'                                                      
         BH    TRAPERR                                                          
         B     VK05                                                             
*                                                                               
VK01     LA    RF,SPMGRTAB                                                      
         LHI   RE,(SPMGRTBX-SPMGRTAB)/L'SPMGRTAB                                
         CLC   22(2,R3),0(RF)                                                   
         BE    VK02                                                             
         AHI   RF,L'SPMGRTAB                                                    
         BCT   RE,*-14                                                          
         B     TRAPERR                                                          
*                                                                               
VK02     MVC   SBQMGRD,2(RF)                                                    
         MVC   FULL,24(R3)         SAVE REST OF MGRP (ALL OR NNNN)              
*                                                                               
VK05     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKGRECD,R4                                                       
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,SBBAGYMD                                                
         CLI   SBQMGRD,C' '        A-F REQUIRE CLIENT                           
         BL    VK07                                                             
         CLI   SBQMGRD,C'F'        A-F REQUIRE CLIENT                           
         BH    VK07                                                             
         CLI   SBBCLT,0                                                         
         BNE   VK06                                                             
         MVI   ERROR,INVMGCLT                                                   
         B     TRAPERR                                                          
*                                                                               
VK06     MVC   MKGKCLT,SBBCLT                                                   
         OC    SBQMGCLT,SBQMGCLT                                                
         BZ    VK07                                                             
         MVC   MKGKCLT,SBQMGCLT    ALTERNATE CLIENT                             
*                                                                               
VK07     MVC   MKGKMID,SBQMGRD                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         TM    DMCB+8,X'12'                                                     
         BNZ   TRAPERR                                                          
         L     R5,AIO1                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,24(R5)                                                        
         SR    RE,RE                                                            
*                                                                               
VK08     CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),1             MKTGRP BREAK DESCRIPTION                     
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     VK08                                                             
         USING MKGEL01,R5                                                       
         LLC   RE,MKGBK1LN         SET MKTGRP BREAK LENGTHS                     
         STC   RE,SBMGR1LN                                                      
         LLC   RF,MKGBK2LN                                                      
         AR    RE,RF                                                            
         STC   RE,SBMGR2LN                                                      
         IC    RF,MKGBK3LN                                                      
         AR    RE,RF                                                            
         STC   RE,SBMGR3LN                                                      
         DROP  R5                                                               
*                                                                               
         MVC   SBQSTA(3),=C'ALL'                                                
         CLI   1(R3),1                                                          
         BE    VKX                                                              
         CLC   FULL(3),=C'ALL'                                                  
         BE    VKX                                                              
         MVI   ERROR,INVMGFLT                                                   
         MVC   SBQMGRF,FULL                                                     
         LA    R1,SBQMGRF                                                       
         LA    RE,L'SBQMGRF                                                     
         CLI   0(R1),C'0'                                                       
         BL    VK10                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
         B     VKX                                                              
VK10     CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   TRAPERR                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,VK10                                                          
         B     VKX                                                              
*                                                                               
VK20     CLI   0(R3),3                                                          
         BNE   *+14                                                             
         CLC   12(3,R3),=C'ALL'                                                 
         BE    VKX                                                              
         TM    2(R3),X'80'                                                      
         BZ    TRAPERR                                                          
         L     R1,4(R3)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SBQMKT,DUB                                                       
         B     VS50                                                             
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
* VALIDATE STATION                                                              
*                                                                               
VVALSTAT MVI   ERROR,INVSTA                                                     
         MVC   SBQSTA(3),=C'ALL'                                                
         LLC   RF,5(R2)            SEARCH FOR C'='                              
         LA    R1,8(R2)                                                         
         MVC   DMCB+8(4),=C',=,='                                               
         CLI   0(R1),C'='                                                       
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
         MVI   DMCB+11,C'-'                                                     
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK)                                      
         LA    R3,BLOCK                                                         
         CLI   DMCB+11,C'='        TEST QUALIFIER IS C'='                       
         BNE   VS2                                                              
         CLI   4(R1),1             YES-                                         
         BNE   TRAPERR                                                          
         CLI   0(R3),3                                                          
         BH    TRAPERR                                                          
         LLC   R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,GRPTEST          TEST STATION GROUP                           
         BNE   VS1                                                              
         MVI   ERROR,INVSGRP       YES                                          
         LA    R4,GRPKSTYQ                                                      
         MVI   BYTE,C'N'           SO IT DOESN'T THINK WE HAVE CGROUP           
         BAS   RE,VALGRP                                                        
         BNE   TRAPERR                                                          
         MVC   SBQSGR,WORK                                                      
         MVC   SBSGR1LN,WORK+5                                                  
         MVC   SBSGR2LN,WORK+6                                                  
         B     VSX                                                              
*                                                                               
VS1      EX    R1,NETTEST          TEST NETWORK FILTER                          
         BNE   TRAPERR                                                          
         CLI   1(R3),3                                                          
         BL    TRAPERR                                                          
         CLI   1(R3),4                                                          
         BH    TRAPERR                                                          
         MVC   SBQNET,22(R3)                                                    
         B     VSX                                                              
*                                                                               
VS2      CLI   4(R1),1                                                          
         BL    TRAPERR                                                          
         CLI   4(R1),2                                                          
         BH    TRAPERR                                                          
         CLI   0(R3),3                                                          
         BL    TRAPERR                                                          
         CLI   12(R3),C'0'         TEST CABLE STATION                           
         BNL   VS4                                                              
         CLC   12(4,R3),=C'ALL/'                                                
         BE    VS4                                                              
         CLI   0(R3),5             NO-STATION NO LONGER THAN 5                  
         BH    TRAPERR                                                          
         BL    VS4                                                              
         CLI   16(R3),C'A'         IF 5 LONG, 4TH CHAR MUST BE A OR F           
         BE    VS4                                                              
         CLI   16(R3),C'F'                                                      
         BNE   TRAPERR                                                          
*                                                                               
VS4      CLI   1(R3),0                                                          
         BNE   VS10                                                             
         CLI   0(R3),3                                                          
         BNE   VS10                                                             
         CLC   8(4,R2),=C'ALL-'    TEST ALL EXCEPT CABLE                        
         BE    VS12                YES                                          
         CLC   12(3,R3),=C'ALL'                                                 
         BNE   VS10                                                             
         CLI   4(R1),2             TEST ALL,X                                   
         BNE   VSX                                                              
         LA    R3,32(R3)           YES-                                         
         CLI   1(R3),0                                                          
         BNE   TRAPERR                                                          
         CLI   0(R3),1                                                          
         BNE   TRAPERR                                                          
         MVC   SBQSTATY,12(R3)     X=STATION TYPE FILTER                        
         B     VSX                                                              
*                                                                               
VS10     CLI   4(R1),1                                                          
         BNE   TRAPERR                                                          
         LA    RF,5                TEST ALL-SM AND ALL-DV REQUEST               
         CLI   1(R3),2             -SM OR -DV REQUEST?                          
         BE    *+14                POSSIBLY                                     
         BCTR  RF,0                TEST ALL-S AND ALL-D REQUEST                 
         CLI   1(R3),1             -S OR -D REQUEST?                            
         BNE   VS12                NO                                           
*                                                                               
         CLI   SBQMED,C'R'         MEDIA R?                                     
         BNE   VS11                NO                                           
         EX    RF,*+8              EXECUTED                                     
         B     *+10                BRANCH SO IDF DOESN'T COMPLAIN               
         CLC   8(0,R2),=C'ALL-SM'  ALL-"DIGITAL BAND" REQUEST?                  
         BE    VS11A               YES                                          
         EX    RF,*+8              EXECUTED                                     
         B     *+10                BRANCH SO IDF DOESN'T COMPLAIN               
         CLC   8(0,R2),=C'ALL-CM'  ALL-"DIGITAL BAND" REQUEST?                  
         BE    VS11A               YES                                          
*                                                                               
VS11     CLI   SBQMED,C'T'         MEDIA T?                                     
         BNE   VS12                NO                                           
         EX    RF,*+8              EXECUTED                                     
         B     *+10                BRANCH SO IDF DOESN'T COMPLAIN               
         CLC   8(0,R2),=C'ALL-DV'  ALL-"DIGITAL BAND" REQUEST?                  
         BNE   VS12                NO                                           
*                                                                               
VS11A    MVC   SBQSTA+4(1),12(R2)  MOVE D/S/C INTO STA+4                        
         B     VSX                 EXIT                                         
*                                                                               
VS12     XC    WORK,WORK           CALL STAVAL                                  
         LA    R4,WORK                                                          
         USING SSTABLKD,R4                                                      
         STCM  R2,7,SSTBADDR+1                                                  
         MVC   SSTBMED,SBQMED                                                   
         CLI   SBQMED,C'*'         FOR CROSS MEDIA REQ...                       
         BNE   VS14                                                             
         CLC   12(3,R3),=C'ALL'    ...FOR STATION ALL...                        
         BNE   VS14                                                             
         MVI   SSTBMED,C'T'        ...USE MEDIA T                               
*                                                                               
VS14     MVI   SSTBCTRY,C'U'                                                    
         LA    RE,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDRD(RE),C'C'                                       
         BNE   *+8                                                              
         MVI   SSTBCTRY,C'C'                                                    
         MVC   SSTBACOM,ACOMFACS                                                
         GOTO1 STAVAL,DMCB,SSTABLKD                                             
         CLI   SSTBERR,SSTBESTA                                                 
         BE    TRAPERR                                                          
         MVC   SBQSTA,SSTBSTA                                                   
         CLI   SBQSTA+4,C' '                                                    
         BH    *+10                                                             
         MVC   SBQSTA+4(1),SBQMED                                               
         CLI   SBQSTA,C'0'         TEST CABLE - NUMERIC HEADEND                 
         BNL   *+12                                                             
         CLI   SBQSTA+4,C'/'       OR ALL HEADENDS                              
         BNE   *+10                                                             
         MVC   SBQCNET,SSTBNET     YES-SAVE CABLE NETWORK                       
         CLC   SBQSTA(3),=C'ALL'   TEST ALL STATIONS                            
         BE    VSX                                                              
         DROP  R4                                                               
*                                                                               
         LA    R4,KEY              READ STATION RECORD                          
         USING STARECD,R4                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(16),STAKEY                                              
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,SBQMED                                                   
         MVC   STAKCALL,SBQSTA                                                  
         MVC   STAKAGY,AGENCY                                                   
         OC    SBBCLT,SBBCLT                                                    
         BZ    *+10                                                             
         MVC   STAKCLT,SBQCLT                                                   
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   TRAPERR                                                          
         OC    SBBCLT,SBBCLT       CLIENT SPECIFIC REQUEST?                     
         BZ    *+10                NO, DON'T FILL IN MARKET                     
         MVC   SBQMKT,SMKT                                                      
*                                                                               
         CLC   SBQCNET,BLANKS      TEST CABLE STATION WITH NETWORK              
         BNH   VS50                                                             
*                                                                               
* CALL STAPACK HERE (INSTEAD OF MSPACK - DIES ON STAPERR<>0)                    
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'        SET ACTION PACK                              
         MVC   STAPAGY,SBQAGY                                                   
         LA    RE,SBAGYREC                                                      
         MVC   STAPCTRY,AGYPROF+7-AGYHDRD(RE)                                   
         MVC   STAPMED,SBQMED                                                   
         MVC   STAPACOM,SBCOMFAC                                                
         MVC   STAPQMKT,=C'0000'   MARKET                                       
         MVC   STAPQSTA(4),SBQSTA  STATION                                      
         MVC   STAPQNET,SBQCNET    NETWORK                                      
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BE    VS50                                                             
         MVC   GTMSGNO,=Y(CBLERR)                                               
         B     MYCURSOR                                                         
         DROP  R1                                                               
*                                                                               
VS50     OC    SBQMKT,SBQMKT       HAVE A MARKET?                               
         BZ    VSX                 NO, THIS IS AN ALL CLIENT REQUEST            
         CLC   SBQMKT,=C'ALL'      ALL MARKET REQUEST?                          
         BE    VSX                 YES - DON'T LOOK UP MARKET RECORD!           
         LA    R4,KEY              READ MARKET RECORD                           
         USING MKTRECD,R4                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(16),MKTKEY                                              
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,SBQMED                                                   
         MVC   MKTKMKT,SBQMKT                                                   
         MVC   MKTKAGY,AGENCY                                                   
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   ERROR,INVMKT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         CLC   KEY(MKTKEYLN),0(R4)                                              
         BNE   TRAPERR                                                          
         L     RF,ATWA                                                          
         LA    RF,6(RF)                                                         
         CLI   0(RF),C'+'          TEST MKT LIMIT ACCESS                        
         BE    *+16                                                             
         LA    RF,2(RF)                                                         
         CLI   0(RF),C'+'          TEST MKT LIMIT ACCESS                        
         BNE   VSX                                                              
*                                                                               
         LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
         CLC   1(1,RF),0(R1)                                                    
         BE    VSX                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         MVI   ERROR,NOMKTACC                                                   
         B     TRAPERR                                                          
*                                                                               
VSX      B     XIT                                                              
         EJECT                                                                  
* VALIDATE CLIENT/STATION GROUP                                                 
* ON ENTRY, R4=GROUP RECORD SUB-TYPE                                            
*           R3=A(SCANNER BLOCK)                                                 
* ON EXIT,  CC EQ - WORK(5)=GROUP-ID(1)/GROUP-FILTER(4)                         
*                   WORK+5(2)=L'LEVEL 1 BREAK(1)/L'LEVEL 1+2 BREAKS(1)          
*           CC NE - INVALID                                                     
*                                                                               
VALGRP   NTR1  ,                                                                
         LR    R1,R4                                                            
         CLI   BYTE,C'Y'           ALREADY FILLED IN WORK?                      
         BE    *+10                YES, HAVE A 2-CHAR CLIENT GROUP              
         MVC   WORK(5),22(R3)                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY              READ GROUP DEFINITION RECORD                 
         USING GRPRECD,R4                                                       
         MVI   GRPKTYP,GRPKTYPQ                                                 
         STC   R1,GRPKSTYP                                                      
         CLI   GRPKSTYP,GRPKSTYQ                                                
         BNE   VALGRP1                                                          
         CLI   WORK,C'*'           SPECIAL CODE FOR EIX STATION GROUP           
         BNE   VALGRP1                                                          
         MVI   WORK+5,3                                                         
         MVI   WORK+6,3                                                         
         B     VALGRPY                                                          
*                                                                               
VALGRP1  MVC   GRPKAGMD,SBBAGYMD                                                
         MVC   GRPKID,WORK                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'GRPKEY),KEYSAVE                                            
         BNE   VALGRPN             INVALID GROUP ID                             
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC              GET THE RECORD                               
         LA    R1,GRPEL                                                         
         SR    RF,RF               GET BREAK DESCRIPTION                        
*                                                                               
VALGRP2  CLI   0(R1),0                                                          
         BE    VALGRP4                                                          
         CLI   0(R1),GRPBRKCQ                                                   
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VALGRP2                                                          
         USING GRPBRKD,R1                                                       
         LLC   RE,GRPBK1LN         SET BREAK LENGTHS                            
         STC   RE,WORK+5                                                        
         LLC   RF,GRPBK2LN                                                      
         AR    RF,RE                                                            
         STC   RF,WORK+6                                                        
         DROP  R1                                                               
*                                                                               
VALGRP4  CLI   1(R3),1                                                          
         BE    VALGRPY                                                          
         CLC   WORK+1(3),=C'ALL'                                                
         BE    VALGRPY                                                          
         LA    R1,WORK+1                                                        
         LA    RE,4                                                             
         CLI   0(R1),C'0'                                                       
         BL    VALGRP6                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
         B     VALGRPY                                                          
*                                                                               
VALGRP6  CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   VALGRPN                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,VALGRP6                                                       
*                                                                               
VALGRPY  CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
VALGRPN  LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
*==============================================================*                
* REDIRECT CALLS FOR MSPACK/MSUNPK TO STAPACK                  *                
*   IF P2 BYTE 1 HAS X'80' SET, DON'T DIE ON ERROR,            *                
*         RETURN CC MINUS AND STAPERR = ERROR CODE.            *                
*==============================================================*                
         SPACE 1                                                                
VVMSPACK DS    0H                                                               
         LM    R2,R4,0(R1)         GET A(MKT)/A(STA)/A(MKTSTA)                  
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,SBQAGY                                                   
         LA    RE,SBAGYREC                                                      
         MVC   STAPCTRY,AGYPROF+7-AGYHDRD(RE)                                   
         MVC   STAPMED,SBMED                                                    
         MVC   STAPACOM,SBCOMFAC                                                
         MVC   STAPQMKT,0(R2)      MARKET                                       
         MVC   STAPQSTA(8),0(R3)   STATION                                      
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BE    VVMSP10                                                          
         LTR   R3,R3               CALLER WILL CHECK ERRORS                     
         BM    VVMSPX                                                           
         DC    H'0'                                                             
*                                                                               
VVMSP10  MVC   0(5,R4),STAPMKST    RETURN RESULT                                
         CR    RB,RB               SET CC EQ                                    
*                                                                               
VVMSPX   B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
VVMSUNPK DS    0H                                                               
         LM    R2,R4,0(R1)         GET A(MKTSTA)/A(MKT)/A(STA)                  
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,SBQAGY                                                   
         LA    RE,SBAGYREC                                                      
         MVC   STAPCTRY,AGYPROF+7-AGYHDRD(RE)                                   
         MVC   STAPMED,SBMED                                                    
         MVC   STAPACOM,SBCOMFAC                                                
         MVC   STAPMKST,0(R2)      MKTSTA                                       
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BE    VVMSU10                                                          
         TM    STAPERR,QSTP_INVCBL INVALID NETWORK?                             
         BNZ   VVMSU10             YES - MASTER RECORD REACTIVATED              
         LTR   R3,R3               CALLER WILL CHECK ERRORS                     
         BM    VVMSUX                                                           
         DC    H'0'                                                             
*                                                                               
VVMSU10  MVC   0(4,R3),STAPQMKT    RETURN RESULT                                
         MVC   0(5,R4),STAPQSTA                                                 
         TM    STAPERR,QSTP_INVCBL INVALID NETWORK?                             
         BZ    *+14                NO                                           
         MVC   5(3,R4),=C'???'                                                  
         B     VVMSU20                                                          
         LTR   R2,R2               TEST 8 BYTE STATION REQ                      
         BNM   *+10                                                             
         MVC   0(8,R4),STAPQSTA                                                 
VVMSU20  CR    RB,RB               SET CC EQ                                    
*                                                                               
VVMSUX   B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
* VALIDATE FILTERS                                                              
*                                                                               
VVALFILT DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(30,(R2)),(9,BLOCK),0                               
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    BADFILT                                                          
         MVI   SBESPOTS,0                                                       
         LA    R4,BLOCK                                                         
*                                                                               
VF10     LA    R3,FILTTAB                                                       
         SR    R1,R1                                                            
*                                                                               
         IC    R1,0(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,VFCLC                                                         
         BE    VFGO                                                             
         LA    R3,1+3(R3,R1)    A(NEXT TABLE ENTRY) (+1 FOR BCTR)               
         CLI   0(R3),X'FF'                                                      
         BNE   *-22                                                             
         B     BADFILT                                                          
*                                                                               
VFCLC    CLC   12(0,R4),1(R3)                                                   
*                                                                               
VFGO     SR    RF,RF                                                            
         LA    R3,2(R3,R1)         R3=A(S(ROUTINE))                             
         ICM   RF,3,0(R3)          ANY ROUTINE TO BRANCH TO?                    
         BZ    FILTEND              NO                                          
         LR    RE,RF                                                            
         SLL   RF,20               STRIP OFF BASE REG                           
         SRL   RF,20                 RF = DISPLACEMENT                          
         SRL   RE,12               ISOLATE BASE REG                             
         EX    RE,*+6                RE = BASE                                  
         BR    RF                  GO TO ROUTINE                                
         AR    RF,0                COMPUTE BASE + DISP TO ROUTINE               
*                                                                               
* FILTER VALIDATION ROUTINES                                                    
*                                                                               
*                                  ESTIMATE FILTER                              
FILTEST  CLC   22(2,R4),=C'E '       CONTROL=E FILTER                           
         BNE   *+12                                                             
         OI    SBEFLAG,SBEECNTL                                                 
         B     FILTEND                                                          
*                                                                               
         CLC   22(3,R4),=C'SEP'                                                 
         BNE   BADFILT                                                          
         MVI   SBQSEPES,C'Y'                                                    
         B     FILTEND                                                          
*                                                                               
FILTCLT  MVC   XCLT,22(R4)         CLIENT PRINT FILTER                          
         CLI   XCLT,C'*'                                                        
         BNE   FLTCLT10                                                         
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         L     RE,ATWA                                                          
         MVC   OFCAUTH,6(RE)       T204FFD+6                                    
         MVC   OFCLMT,6(RE)        T204FFD+6                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,XCLT+1                                                   
         GOTO1 OFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                            
         LA    R1,WORK                                                          
         TM    OFCINDS,OFCINOLA    AGY USING 2 CHAR?                            
         BNZ   *+12                 NO                                          
         CLI   DMCB,0                                                           
         BNE   BADFILT                                                          
         MVC   XCLT+1(1),OFCOFC    SAVE OFF 1 BYTE INTERNAL OFFICE CODE         
         MVI   XCLT+2,C' '                                                      
         B     FILTEND                                                          
*                                                                               
FLTCLT10 GOTO1 CLPACK,DMCB,XCLT,FULL                                            
         CLI   0(R1),0                                                          
         BNE   BADFILT                                                          
         B     FILTEND                                                          
*                                                                               
FILTDPT  LLC   RF,1(R4)            DAYPART FILTER                               
         LA    R1,22(R4)                                                        
         CLI   0(R1),C'-'          TEST TO EXCLUDE DAYPARTS                     
         BNE   VF30A                                                            
         OI    SBQDPTLN,SBQDLEXC   YES                                          
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNP   BADFILT                                                          
         LA    R1,1(R1)                                                         
VF30A    MVC   SBQDPT,0(R1)                                                     
         XC    SBQDPLST,SBQDPLST                                                
         LA    R3,SBQDPLST-1                                                    
         LA    RE,L'SBQDPLST+1                                                  
         B     *+10                                                             
VF30B    MVC   0(1,R3),0(R1)                                                    
         LA    R1,1(R1)                                                         
         BCT   RF,*+8                                                           
         B     FILTEND                                                          
         LA    R3,1(R3)                                                         
         BCT   RE,VF30B                                                         
         B     BADFILT                                                          
*                                                                               
FILTMDPT CLI   1(R4),1             MASTER DAYPART FILTER                        
         BNE   BADFILT                                                          
         MVC   SBQMASDP,22(R4)                                                  
         B     FILTEND                                                          
*                                                                               
FILTLEN  TM    3(R4),X'80'         SPOT LENGTH FILTER                           
         BZ    BADFILT                                                          
         CLC   8(4,R4),=F'255'                                                  
         BH    BADFILT                                                          
         MVC   SBQLEN,11(R4)                                                    
         B     FILTEND                                                          
*                                                                               
FILTADJ  XC    SBQADLST,SBQADLST   PROGRAM ADJACENCY CODE FILTER                
         LA    RF,SBQADJ                                                        
         LA    R1,22(R4)                                                        
         LA    R5,10               MAX OF 10 ADJ CODES                          
         LLC   R3,1(R4)            R3=L'INPUT                                   
         LA    R3,22(R3,R4)        R3=A(END OF INPUT)                           
*                                                                               
FLTADJ10 CLI   0(R1),C'A'          ALSO ALLOWS \ AND } (I'M LAZY)               
         BL    BADFILT                                                          
         CLI   0(R1),C'Z'                                                       
         BH    FLTADJ20                                                         
         MVC   0(1,RF),0(R1)                                                    
         LA    R1,1(R1)                                                         
         B     FLTADJ30                                                         
*                                                                               
FLTADJ20 CLI   0(R1),C'0'          VALIDATE 2 CHARACTER NUMERIC                 
         BL    BADFILT                                                          
         CLI   0(R1),C'9'                                                       
         BH    BADFILT                                                          
         CLI   1(R1),C'0'                                                       
         BL    BADFILT                                                          
         CLI   2(R1),C'9'                                                       
         BH    BADFILT                                                          
         PACK  0(1,RF),0(1,R1)                                                  
         NI    0(RF),X'F0'                                                      
         MVC   BYTE,1(R1)                                                       
         NI    BYTE,X'0F'                                                       
         OC    0(1,RF),BYTE                                                     
         LA    R1,2(R1)                                                         
*                                                                               
FLTADJ30 CR    R1,R3               PAST END OF INPUT?                           
         BNL   FILTEND              YES - DONE                                  
         CLI   0(R1),C'/'                                                       
         BNE   BADFILT                                                          
         LA    R1,1(R1)                                                         
*                                                                               
         LA    RE,SBQADJ           IN LIST OR AT SINGLE FILTER?                 
         CR    RE,RF                                                            
         BNE   FLTADJ40                                                         
         LA    RF,SBQADLST         POINT AT LIST FIRST TIME ONLY                
         B     *+8                                                              
FLTADJ40 LA    RF,1(RF)            BUMP LIST                                    
         BCT   R5,FLTADJ10                                                      
         B     BADFILT                                                          
*                                                                               
FILTSREP LLC   RE,1(R4)            LENGTH OF FILTER                             
         LA    R5,22(R4)           START OF FILTER                              
         CLI   0(R5),C'-'          NEGATIVE SREP FILTER?                        
         BNE   *+14                NO                                           
         OI    SBEFLAG7,SBE7NREP   FLAG NEGATIVE SREP FILTER                    
         LA    R5,1(R5)            BUMP PAST "-"                                
         BCTR  RE,0                SUBTRACT 1 FOR "-"                           
         CHI   RE,3                SPECIAL REP FILTER 3 CHARS?                  
         BNE   BADFILT             NO - ERROR                                   
         GOTO1 RCPACK,DMCB,(C'P',0(R5)),SBQSREP                                 
         BNE   BADFILT                                                          
         B     FILTEND                                                          
*                                                                               
FILTIREP LLC   RE,1(R4)            LENGTH OF FILTER                             
         LA    R5,22(R4)           START OF FILTER                              
         CLI   0(R5),C'-'          NEGATIVE SREP FILTER?                        
         BNE   *+14                NO                                           
         OI    OPTIND6,OPT6NREP    FLAG NEGATIVE INVREP FILTER                  
         LA    R5,1(R5)            BUMP PAST "-"                                
         BCTR  RE,0                SUBTRACT 1 FOR "-"                           
         CHI   RE,3                SPECIAL REP FILTER 3 CHARS?                  
         BNE   BADFILT             NO - ERROR                                   
         MVC   INVQSREP,0(R5)      SPECIAL REP                                  
         B     FILTEND                                                          
*                                                                               
FILTAFF  CLI   1(R4),3             AFFILIATE FILTER                             
         BH    BADFILT                                                          
         MVC   SBQAFFIL,22(R4)                                                  
         B     FILTEND                                                          
*                                                                               
FILTSPT  MVI   SBESPOTS,SBESPAID   SPOTS FILTER                                 
         CLC   22(3,R4),=C'PD '                                                 
         BE    FILTEND                                                          
         MVI   SBESPOTS,SBESUNPD                                                
         CLC   22(5,R4),=C'UNPD '                                               
         BE    FILTEND                                                          
         MVI   SBESPOTS,SBESALL                                                 
         CLC   22(4,R4),=C'ALL '                                                
         BE    FILTEND                                                          
         B     BADFILT                                                          
*                                                                               
FILTCML  CLI   1(R4),1             COMMERCIAL CLASS FILTER                      
         BL    BADFILT                                                          
         CLI   1(R4),4                                                          
         BH    BADFILT                                                          
         MVC   SBQCMLCL,22(R4)                                                  
         B     FILTEND                                                          
*                                                                               
FILTSTM  LA    R3,SBESTTIM         START TIME FILTER                            
         LA    R5,STTIMTAB                                                      
         B     VF46                                                             
*                                                                               
FILTETM  LA    R3,SBEENTIM         END TIME FILTER                              
         LA    R5,ENTIMTAB                                                      
*                                                                               
VF46     SR    R1,R1                                                            
         CLI   1(R4),0             TEST '=' IN THE EXPRESSION                   
         BNE   VF47                                                             
         LA    RE,16(R4)           NO-FORMAT IS LIKE 'STM>8P'                   
         IC    R1,0(R4)                                                         
         AHI   R1,-4                                                            
         BNP   BADFILT                                                          
         B     VF48                                                             
*                                                                               
VF47     CLI   0(R4),4             YES-                                         
         BNE   BADFILT                                                          
         LA    RE,22(R4)           FORMAT IS LIKE 'STM>=8P'                     
         IC    R1,1(R4)                                                         
*                                                                               
VF48     ST    RE,DMCB                                                          
         STC   R1,DMCB                                                          
         GOTO1 TIMVAL,DMCB,,FULL                                                
         CLI   0(R1),X'FF'                                                      
         BE    BADFILT                                                          
         MVC   0(2,R3),FULL                                                     
VF49     CLI   0(R5),0                                                          
         BE    BADFILT                                                          
         CLC   15(1,R4),0(R5)                                                   
         BE    *+12                                                             
         LA    R5,3(R5)                                                         
         B     VF49                                                             
         LA    R1,1(R5)                                                         
         CLI   1(R4),0                                                          
         BE    *+8                                                              
         LA    R1,2(R5)                                                         
         OC    SBETIMIN,0(R1)                                                   
         B     FILTEND                                                          
*                                                                               
*                                  INFOMERICAL RESPONSE DATE FILTER             
FILTRSP  GOTO1 PERVAL,DMCB,(1(R4),22(R4)),WORK                                  
         TM    DMCB+4,X'03'        INVALID/NO INPUT?                            
         BNZ   BADFILT                                                          
         LA    RE,WORK                                                          
         USING PERVALD,RE                                                       
         MVC   SBERSPDT,PVALCSTA   SAVE START AND END DATES                     
         DROP  RE                                                               
         B     FILTEND                                                          
*                                                                               
FILTIFBY CLI   1(R4),1             INFOMERCIAL BUY TYPE FILTER                  
         BNE   BADFILT                                                          
         CLI   22(R4),C'Y'                                                      
         BE    *+20                                                             
         CLI   22(R4),C'N'                                                      
         BE    *+12                                                             
         CLI   22(R4),C'A'                                                      
         BNE   BADFILT                                                          
         MVC   SBEIBYCK,22(R4)                                                  
         B     FILTEND                                                          
                                                                                
*                                                                               
FILTATIM DS    0H                                                               
         CLI   1(R4),0                                                          
         BE    BADFILT                                                          
         GOTO1 TIMVAL,DMCB,(1(R4),22(R4)),ATIMRNG                               
         CLI   0(R1),X'FF'                                                      
         BE    BADFILT                                                          
         OC    ATIMRNG+2(2),ATIMRNG+2                                           
         BZ    BADFILT                                                          
         B     FILTEND                                                          
*                                                                               
FILTRNGE DS    0H                                                               
         CLI   1(R4),0                                                          
         BE    BADFILT                                                          
         GOTO1 TIMVAL,DMCB,(1(R4),22(R4)),SBERANGE                              
         CLI   0(R1),X'FF'                                                      
         BE    BADFILT                                                          
         OC    SBERANGE+2(2),SBERANGE+2                                         
         BZ    BADFILT                                                          
         B     FILTEND                                                          
*                                                                               
FILTCDAT DS    0H                                                               
         CLI   1(R4),0                                                          
         BE    BADFILT                                                          
         GOTO1 DATVAL,DMCB,(0,22(R4)),(0,WORK)                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    BADFILT                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,SBECDAT)                                 
         B     FILTEND                                                          
*                                                                               
FILTADAT DS    0H                                                               
         CLI   1(R4),0                                                          
         BE    BADFILT                                                          
         GOTO1 DATVAL,DMCB,(0,22(R4)),(0,WORK)                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    BADFILT                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,ACTIVDAT)                                
         B     FILTEND                                                          
*                                                                               
FILTSRC  DS    0H                                                               
         CLI   1(R4),0                                                          
         BNE   BADFILT                                                          
         OI    SBEFLAG2,SBESRC                                                  
         OI    DRINDS,GLPALDET                                                  
         B     FILTEND                                                          
*                                                                               
FILTAUDT DS    0H                                                               
         CLI   1(R4),0                                                          
         BNE   BADFILT                                                          
         OI    SBEINV,SBEIAUD                                                   
         B     FILTEND                                                          
*                                                                               
FILTMCTY DS    0H                                                               
         CLI   1(R4),0                                                          
         BNE   BADFILT                                                          
         OI    SBEINV,SBEIMCY                                                   
         B     FILTEND                                                          
*                                                                               
FILTMCTN DS    0H                                                               
         CLI   1(R4),0                                                          
         BNE   BADFILT                                                          
         OI    SBEINV,SBEIMCN                                                   
         B     FILTEND                                                          
*                                                                               
FILTEASY DS    0H                                                               
         CLI   1(R4),0                                                          
         BNE   BADFILT                                                          
         OI    SBEINV,SBEIEAY                                                   
         B     FILTEND                                                          
*                                                                               
FILTEASN DS    0H                                                               
         CLI   1(R4),0                                                          
         BNE   BADFILT                                                          
         OI    SBEINV,SBEIEAN                                                   
         B     FILTEND                                                          
*                                                                               
FILTPAID DS    0H                                                               
         CLC   22(3,R4),=C'PD '                                                 
         BNE   *+12                                                             
         OI    SBEINV,SBEIPAID                                                  
         B     FILTEND                                                          
         CLC   22(5,R4),=C'UNPD '                                               
         BNE   *+12                                                             
         OI    SBEINV,SBEIUNPD                                                  
         B     FILTEND                                                          
         CLC   22(4,R4),=C'ALL '                                                
         BE    FILTEND                                                          
         B     BADFILT                                                          
*                                                                               
FILTIDR  DS    0H                                                               
         CLI   1(R4),0                                                          
         BNE   BADFILT                                                          
         OI    OPTIND4,OPTIDR                                                   
         B     FILTEND                                                          
*                                                                               
FILTEXC  DS    0H                                                               
         CLI   1(R4),1                                                          
         BNE   BADFILT                                                          
         TM    3(R4),X'80'         VALID NUMERIC?                               
         BZ    BADFILT                                                          
         CLI   8(R4),3             VALUE 1 TO 3?                                
         BH    BADFILT                                                          
         MVC   SBQEXCL,22(R4)                                                   
         B     FILTEND                                                          
*                                                                               
FILTPROG DS    0H                                                               
         MVC   SBEPROG+1(L'SBEPROG-1),BLANKS                                    
         MVI   SBEPROG,L'SBEPROG-2          DEFAULT COMPARE LENGTH              
         XR    RF,RF                                                            
         ICM   RF,1,1(R4)                                                       
         BZ    BADFILT                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SBEPROG+1(0),22(R4)                                              
         LA    RE,SBEPROG+1                                                     
         AR    RE,RF                                                            
         CLI   0(RE),C'*'          WILDCARD?                                    
         BNE   FILTEND                                                          
         BCTR  RF,0                DON'T INCLUDE * IN COMPARE                   
         STC   RF,SBEPROG                                                       
         B     FILTEND                                                          
*                                                                               
FILTETYP MVC   SBQETYPE,22(R4)                                                  
         CLC   22(2,R4),=C'S '       STEWARDSHIP ESTS ONLY                      
         BE    FILTEND                                                          
         CLC   22(2,R4),=C'B '       BARTER EST ESTS ONLY                       
         BE    FILTEND                                                          
         CLC   22(2,R4),=C'* '       ALL ESTIMATES                              
         BE    FILTEND                                                          
         CLC   22(2,R4),=C'U '       UNWIRED?                                   
         BE    FILTEND                                                          
         CLC   22(2,R4),=C'-U'       WIRED?                                     
         BNE   BADFILT               NO, ERROR                                  
         MVI   SBQETYPE,C'W'         YES, MOVE A 'W' IN                         
         B     FILTEND                                                          
*                                                                               
FILTBON  CLC   22(2,R4),=C'Y '       INCLUDE BONUS SPOTS?                       
         BE    FILTEND                                                          
         CLC   22(2,R4),=C'N '       EXCLUDE BONUS SPOTS                        
         BNE   BADFILT                                                          
         OI    SBEFLAG3,SBE3XBON                                                
         B     FILTEND                                                          
*                                                                               
FILTSTYP CLI   1(R4),0                                                          
         BNE   BADFILT                                                          
         OI    SBEFLAG3,SBE3STYP                                                
         B     FILTEND                                                          
*                                                                               
FILTPBDM CLI   1(R4),0                                                          
         BNE   BADFILT                                                          
         OI    SBEFLAG3,SBE3PBDM                                                
         B     FILTEND                                                          
*                                                                               
FILTNSIZ SR    RF,RF               NETSIZE FILTER                               
         IC    RF,1(R4)                                                         
         LA    R1,22(R4)                                                        
         CLI   0(R1),C'-'          TEST TO EXCLUDE NETSIZE                      
         BNE   *+12                                                             
         SHI   RF,1                                                             
         LA    R1,1(R1)                                                         
         LTR   RF,RF                                                            
         BNP   BADFILT             NOT ANY                                      
         CHI   RF,L'SBQNSIZE+L'SBQNSIZX    BASE FILTER + EXTRA                  
         BH    BADFILT             TOO MANY                                     
*                                                                               
         MVC   SBQNSIZE,0(R1)                                                   
         CLI   22(R4),C'-'                                                      
         BNE   *+8                                                              
         NI    SBQNSIZE,255-X'40'  LOWERCASE FOR EXCLUDE                        
         OI    DATAIND9,DINSIZE    EXTRACT NETSIZE (FOR BUYS)                   
         XC    SBQNSIZX,SBQNSIZX   DEAL WITH ADDITIONAL NETSIZE FILTERS         
         CHI   RF,1                                                             
         BNH   FILTEND             NONE                                         
         SHI   RF,2                1ST VALUE + EX                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SBQNSIZX(0),1(R1)                                                
         B     FILTEND                                                          
*                                                                               
* FILTBAT ONLY FOR IAS - SBQBATST == SBQBHIST!!!                                
FILTBAT  DS    0H                                                               
         L     RF,ATWA                                                          
         CLC   =C'IAS ',CONREC-T204FFD(RF)                                      
         BNE   BADFILT                                                          
*                                                                               
         GOTO1 PERVAL,DMCB,(1(R4),22(R4)),(0,WORK)                              
         TM    DMCB+4,X'03'        INVALID/NO INPUT?                            
         BZ    *+12                NO                                           
FBAT10   MVI   ERROR,INVDATE       ELSE - SET ERROR CODE                        
         B     TRAPERR                                                          
*                                                                               
         LA    R1,WORK                                                          
         USING PERVALD,R1                                                       
*                                                                               
         CLI   PVALASSM,X'00'      BOTH FULL DATES ENTERED?                     
         BE    *+12                YES - SO CONTINUE                            
         CLI   PVALASSM,X'70'      FULL START DATE ONLY ENTERED?                
         BNE   FBAT10              NO - ERROR                                   
*                                                                               
         MVC   SBQBATST(4),PVALCSTA   MOVE S/E DATES                            
         B     FILTEND                                                          
         DROP  R1                                                               
*                                                                               
STTIMTAB DC    C'>',AL1(SBETISGT,SBETISGE)                                      
         DC    C'<',AL1(SBETISLT,SBETISLE)                                      
         DC    X'00'                                                            
ENTIMTAB DC    C'>',AL1(SBETIEGT,SBETIEGE)                                      
         DC    C'<',AL1(SBETIELT,SBETIELE)                                      
         DC    X'00'                                                            
*                                                                               
VFX      B     XIT                                                              
         SPACE 1                                                                
*                                                                               
FILTPER  MVC   OMPERSON,22(R4)                                                  
         CLC   22(2,R4),=C'S '       INCLUDE SALESPERSON ONLY?                  
         BE    FILTEND               YES                                        
         CLC   22(2,R4),=C'P '       INCLUDE POINTPERSON ONLY?                  
         BNE   BADFILT               NO, BAD OPTION                             
         B     FILTEND                                                          
*                                                                               
FILTDEST MVC   OMDEST,22(R4)       DESTINATION                                  
         CLI   22(R4),C'R'                                                      
         BE    FILTEND                                                          
         CLI   22(R4),C'S'                                                      
         BNE   BADFILT                                                          
         B     FILTEND                                                          
*                                                                               
FILTBYR  CLI   1(R4),3             MUST BE 1-3 CHARS                            
         BH    BADFILT                                                          
         MVC   OMBUYER,22(R4)                                                   
         OC    OMBUYER,BLANKS                                                   
         B     FILTEND                                                          
*                                                                               
FILTOFC  CLI   1(R4),2             OFFIVE MUST BE 2 CHAR                        
         BNE   BADFILT                                                          
         MVC   OMOFFICE,22(R4)                                                  
         B     FILTEND                                                          
*                                                                               
FILTSTAT CLI   1(R4),6             STATUS                                       
         BH    BADFILT                                                          
         LA    R1,STATTAB                                                       
*                                                                               
FS00     CLI   0(R1),X'FF'                                                      
         BE    BADFILT                                                          
         CLC   22(6,R4),0(R1)      COMPARE...NO PARTIALS                        
         BE    FS10                                                             
         LA    R1,6(R1)                                                         
         B     FS00                                                             
*                                                                               
FS10     MVC   OMSTAT,22(R4)                                                    
         B     FILTEND                                                          
*                                                                               
STATTAB  DC    CL6'*SENT'                                                       
         DC    CL6'*VRSNT'                                                      
         DC    CL6'*RVSNT'                                                      
         DC    CL6'*FXSNT'                                                      
         DC    CL6'*EMSNT'                                                      
         DC    CL6'*VESNT'                                                      
         DC    CL6'*RESNT'                                                      
         DC    CL6'VARSNT'                                                      
         DC    CL6'REVSNT'                                                      
         DC    CL6'FXSENT'                                                      
         DC    CL6'VFXSNT'                                                      
         DC    CL6'RFXSNT'                                                      
         DC    CL6'EMSENT'                                                      
         DC    CL6'VEMSNT'                                                      
         DC    CL6'REMSNT'                                                      
         DC    CL6'RJCTED'                                                      
         DC    CL6'VARREJ'                                                      
         DC    CL6'REVREJ'                                                      
         DC    CL6'OPENED'                                                      
         DC    CL6'VAROPN'                                                      
         DC    CL6'REVOPN'                                                      
         DC    CL6'CFMPND'                                                      
         DC    CL6'CNFRMD'                                                      
         DC    CL6'VARCNF'                                                      
         DC    CL6'REVCNF'                                                      
         DC    CL6'**PCFM'                                                      
         DC    CL6'**VPCF'                                                      
         DC    CL6'**RPCF'                                                      
         DC    CL6'DELFAX'                                                      
         DC    CL6'CANFAX'                                                      
         DC    CL6'ERROR'                                                       
         DC    CL6'BYRCFM'                                                      
         DC    CL6'EMPTY'                                                       
         DC    CL6'UNDARD'                                                      
         DC    CL6'NTDARE'                                                      
         DC    CL6'RECALL'                                                      
         DC    CL6'RCLAPP'                                                      
         DC    CL6'RCLCFM'                                                      
         DC    CL6'RCLDNT'                                                      
         DC    CL6'RCLREJ'                                                      
         DC    CL6'RCLTRN'                                                      
         DC    CL6'RCLWIP'                                                      
         DC    CL6'VARRCL'                                                      
         DC    CL6'REVRCL'                                                      
         DC    CL6'RCLUNK'                                                      
         DC    CL6'*PNDNG'                                                      
         DC    CL6'UNSENT'                                                      
         DC    X'FF'                                                            
*                                                                               
FILTREP  CLI   1(R4),3             REP                                          
         BNE   BADFILT                                                          
         MVC   OMREP,22(R4)                                                     
         B     FILTEND                                                          
*                                                                               
FILTELOK CLC   22(2,R4),=C'Y '       ONLY REPORT LOCKED ESTIMATES?              
         BNE   *+12                  NO                                         
         OI    SBEFLAG4,SBE4YLOK                                                
         B     FILTEND                                                          
         CLC   22(2,R4),=C'N '       ONLY REPORT UNLOCKED ESTIMATES?            
         BNE   BADFILT               NO - ERROR                                 
         OI    SBEFLAG4,SBE4NLOK                                                
         B     FILTEND                                                          
*                                                                               
FILTERAT CLI   1(R4),1               ERATE FILTER MUST BE 1 CHARACTER           
         BNE   BADFILT                                                          
         MVC   SBQERATE,22(R4)                                                  
         B     FILTEND                                                          
*                                                                               
FILTUDEF CLC   =C'POL',22(R4)        UDEF=POL?                                  
         BNE   BADFILT               NOPE, ERROR                                
         OI    SBEFLAG5,SBE5UPOL                                                
         B     FILTEND                                                          
*                                                                               
FILTUCOM CLC   =C'POL',22(R4)        UCOM=POL?                                  
         BNE   BADFILT               NOPE, ERROR                                
         OI    SBEFLAG5,SBE5CPOL                                                
         B     FILTEND                                                          
*                                                                               
FILTMCOM CLI   22(R4),C'Y'           MATCHCOM=Y?                                
         BE    FILTEND               YES                                        
         CLI   22(R4),C'N'           MATCHCOM=N?                                
         BNE   BADFILT               NO, ERROR                                  
         OI    SBEFLAG6,SBE6MATN     FILTER OUT BUYS W/MATCH=NO COMMENT         
         B     FILTEND                                                          
*                                                                               
BADFILT  DS    0H                                                               
         MVC   GTMSGNO,=Y(FILTERR)                                              
         B     MYCURSOR                                                         
         SPACE 1                                                                
FILTEND  LA    R4,52(R4)           SFI!                                         
         BCT   R0,VF10                                                          
         B     VFX                                                              
         EJECT                                                                  
*                                                                               
* DC XL1 TEXT LENGTH                                                            
* DC CL? TEXT                                                                   
* DC SL2(ROUTINE)                                                               
*                                                                               
FILTTAB  DC    X'04',C'EST ',SL2(FILTEST)                                       
         DC    X'04',C'CLT ',SL2(FILTCLT)                                       
         DC    X'04',C'DPT ',SL2(FILTDPT)                                       
         DC    X'07',C'MASDPT ',SL2(FILTMDPT)                                   
         DC    X'04',C'LEN ',SL2(FILTLEN)                                       
         DC    X'04',C'ADJ ',SL2(FILTADJ)                                       
         DC    X'05',C'SREP ',SL2(FILTSREP)                                     
         DC    X'06',C'AFFIL ',SL2(FILTAFF)                                     
         DC    X'04',C'SPT ',SL2(FILTSPT)                                       
         DC    X'09',C'CMLCLASS ',SL2(FILTCML)                                  
         DC    X'03',C'STM',SL2(FILTSTM)                                        
         DC    X'03',C'ETM',SL2(FILTETM)                                        
         DC    X'06',C'RSPDT ',SL2(FILTRSP)                                     
         DC    X'08',C'IFBYCHK ',SL2(FILTIFBY)                                  
         DC    X'06',C'ATIME ',SL2(FILTATIM)                                    
         DC    X'07',C'ATIMES ',SL2(FILTATIM)                                   
         DC    X'06',C'AUDIT ',SL2(FILTAUDT)                                    
         DC    X'04',C'MCT ',SL2(FILTMCTY)                                      
         DC    X'05',C'MCT- ',SL2(FILTMCTN)                                     
         DC    X'06',C'NOIDR ',SL2(FILTIDR)                                     
         DC    X'04',C'INV ',SL2(FILTPAID)                                      
         DC    X'06',C'RANGE ',SL2(FILTRNGE)                                    
         DC    X'06',C'CDATE ',SL2(FILTCDAT)                                    
         DC    X'04',C'SRC ',SL2(FILTSRC)                                       
         DC    X'08',C'EXCLUDE ',SL2(FILTEXC)                                   
         DC    X'06',C'BATCH ',SL2(FILTBAT)                                     
         DC    X'05',C'PROG ',SL2(FILTPROG)                                     
         DC    X'06',C'ETYPE ',SL2(FILTETYP)                                    
         DC    X'06',C'BONUS ',SL2(FILTBON)                                     
         DC    X'05',C'EASI ',SL2(FILTEASY)                                     
         DC    X'06',C'EASI- ',SL2(FILTEASN)                                    
         DC    X'06',C'STYPE ',SL2(FILTSTYP)                                    
         DC    X'07',C'PBDEMO ',SL2(FILTPBDM)                                   
         DC    X'08',C'NETSIZE ',SL2(FILTNSIZ)                                  
         DC    X'09',C'OMPERSON ',SL2(FILTPER)                                  
         DC    X'07',C'OMDEST ',SL2(FILTDEST)                                   
         DC    X'08',C'OMBUYER ',SL2(FILTBYR)                                   
         DC    X'09',C'OMOFFICE ',SL2(FILTOFC)                                  
         DC    X'09',C'OMSTATUS ',SL2(FILTSTAT)                                 
         DC    X'06',C'OMREP ',SL2(FILTREP)                                     
         DC    X'08',C'ACTDATE ',SL2(FILTADAT)                                  
         DC    X'08',C'ESTLOCK ',SL2(FILTELOK)                                  
         DC    X'06',C'ERATE ',SL2(FILTERAT)                                    
         DC    X'05',C'UDEF ',SL2(FILTUDEF)                                     
         DC    X'05',C'UCOM ',SL2(FILTUCOM)                                     
         DC    X'09',C'MATCHCOM ',SL2(FILTMCOM)                                 
         DC    X'07',C'INVREP ',SL2(FILTIREP)                                   
         DC    X'FF'                                                            
         EJECT                                                                  
* VALIDATE PERIOD                                                               
*                                                                               
VVALPER  CLI   5(R2),0             TEST FOR INPUT                               
         BNE   VAP02               YES                                          
         CLC   SBQPRD,=C'ALL'      TEST PRD=ALL                                 
         BE    VAP02               YES - PERIOD REQUIRED                        
         CLC   SBQEST,SBQESTND     TEST FOR SINGLE ESTIMATE REQUEST             
         BNE   VAP02                                                            
         MVC   SBQSTART,SBESTST                                                 
         MVC   SBQEND,SBESTND                                                   
         MVC   QSTART,SBQSTART                                                  
         MVC   QEND,SBQEND                                                      
         B     VAP04               YES                                          
*                                                                               
VAP02    GOTO1 ANY                 REQUIRED FIELD                               
*                                  OPTION 0=YMD 2=YM                            
         MVI   QPERTYPE,0          SET PERIOD TYPE TO YMD                       
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(0,WORK)                               
         TM    DMCB+4,X'03'        INVALID/NO INPUT?                            
         BNZ   VAP02H              YES - SO ERROR                               
*                                                                               
         LA    R1,WORK                                                          
         USING PERVALD,R1                                                       
*                                                                               
         CLI   PVALASSM,X'00'      BOTH FULL DATES ENTERED?                     
         BE    VAP03               YES - SO CONTINUE                            
*                                                                               
         CLI   PVALASSM,X'70'      FULL START DATE ONLY ENTERED?                
         BE    VAP03               YES - SO CONTINUE                            
*                                                                               
         CLI   PVALASSM,X'11'      BOTH YYMM DATES ENTERED?                     
         BE    VAP03               YES - SO CONTINUE                            
*                                                                               
         CLI   PVALASSM,X'71'      YYMM START DATE ONLY ENTERED?                
         BE    VAP03               YES - SO CONTINUE                            
*                                                                               
VAP02H   EQU   *                                                                
*                                                                               
         MVI   ERROR,INVDATE       ELSE - SET ERROR CODE                        
         GOTO1 ERREX               GOTO ERROR                                   
*                                                                               
VAP03    EQU   *                                                                
*                                                                               
         MVC   QSTART(12),PVALESTA MOVE S/E DATES TO QSTART/QEND                
         TM    PVALASSM,X'01'      YYMM DATES ENTERED?                          
         BNO   VAP03A              NO - SO CONTINUE                             
*                                                                               
         MVC   QSTART+4(2),=C'00'  ELSE - SET START DD=00                       
         MVC   QEND+4(2),=C'00'    SET END DD=00                                
         MVI   QPERTYPE,2          SET PERIOD TYPE FOR YM                       
         DROP  R1                                                               
*                                                                               
VAP03A   EQU   *                                                                
*                                                                               
         MVC   SBQSTART,QSTART                                                  
         MVC   SBQEND,QEND                                                      
*                                                                               
VAP04    MVC   SBQREQST,SBQSTART   SAVE ORIGINAL REQUEST DATES                  
         MVC   SBQREQND,SBQEND                                                  
         CLC   SBQREQST+4(2),=C'00' TEST START DAY SET                          
         BNE   *+8                                                              
         MVI   SBQREQST+5,C'1'      NO-MAKE IT THE 1ST                          
         CLC   SBQREQND+4(2),=C'00' TEST END DAY SET                            
         BNE   VAP06                                                            
         MVI   SBQREQND+5,C'1'      NO-MAKE IT THE BROADCAST MONTH END          
         GOTO1 GETBROAD,DMCB,(1,SBQREQND),WORK,GETDAY,ADDAY                     
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SBQREQND,WORK+6                                                  
*                                                                               
VAP06    GOTO1 PERVERT,DMCB,SBQREQST,SBQREQND  CHECK PERIOD LENGTH              
         CLC   8(2,R1),=H'735'     NO MORE THAN 2 YEARS                         
         BH    VAP98                                                            
         CLI   WHEN,X'20'          TEST SOON REQUEST                            
         BNE   VAPX                                                             
         CLI   OFFLINE,C'Y'        SKIP CHECK OFFLINE                           
         BE    VAPX                                                             
         CLI   DDS,C'Y'            AND FOR DDS TERMINALS                        
         BE    VAPX                                                             
         CLC   13(1,R1),WLPROF+1   CHECK NOT MORE WEEKS THAN ALLOWED            
         BH    VAP10                                                            
         BL    VAPX                                                             
         OC    10(2,R1),10(R1)                                                  
         BZ    VAPX                                                             
*                                                                               
VAP10    CLI   WLPROF+3,0          SECOND WEEK LIMIT?                           
         BE    VAP98                NO - PERIOD TOO LONG                        
         MVI   WLPROF+1,X'FE'      SET NO DEM/CPP PROF IN USE                   
         CLC   13(1,R1),WLPROF+3   CHECK NOT MORE WEEKS THAN ALLOWED            
         BH    VAP98                                                            
         BL    VAPX                                                             
         OC    10(2,R1),10(R1)                                                  
         BZ    VAPX                                                             
*                                                                               
VAP98    MVI   ERROR,PERLONG                                                    
         B     TRAPERR                                                          
*                                                                               
VAP99    MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
VAPX     B     XIT                                                              
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
VVALOPTS DS    0H                                                               
         MVI   BOXOPT,0                                                         
         MVI   LEFTOPT,0                                                        
         MVI   SPACOPT,1                                                        
         MVI   DOWNOPT,0                                                        
         MVI   TRACEOPT,C'N'                                                    
         MVI   SVCOPT,0                                                         
         MVI   PEROPT,0                                                         
         MVI   DATEOPT,0                                                        
         XC    MAXREC,MAXREC                                                    
*                                                                               
         MVI   SBQSPILL,C'N'       SET NO SPILL                                 
         MVI   SBEGST,0            SET NO CANADIAN GST/PST                      
         XC    SBPDEMOS,SBPDEMOS   SET NO DEMO OPTION                           
         OI    SBQDPTLN,SBQDLTOT+SBQDLCPP   DEFAULT DPTLEN OPTIONS              
*                                                                               
         BRAS  RE,VALOPT                                                        
         BNE   MYCURSOR                                                         
         B     XIT                                                              
*                                                                               
VVALOTH  BRAS  RE,VALOPT           VALIDATE OTHER OPTIONS                       
         BNE   MYCURSOR                                                         
         CLI   DEMOPT,DEMOTGT      TEST DEMO OPTION SET TO TGT/SEC              
         BE    *+12                                                             
         CLI   DEMOPT,DEMOSEC                                                   
         BNE   XIT                                                              
         OC    SBQDEMOS,SBQDEMOS   YES - TEST DEMO MENU                         
         BNZ   XIT                                                              
         OC    SBPDEMOS,SBPDEMOS         OR DEMO OPTION                         
         BNZ   XIT                                                              
*                                                                               
         MVC   GTMSGNO,=Y(MENERR)                                               
         B     MYCURSOR                                                         
         EJECT                                                                  
* VALIDATE TITLE                                                                
*                                                                               
VVALTITS MVC   TITLE,BLANKS                                                     
         MVC   TITLE(21),=C'SPOTPAK REPORT WRITER'                              
         CLI   5(R2),0                                                          
         BE    VVALTIT2                                                         
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
VVALTIT2 GOTO1 CENTER,DMCB,TITLE,63                                             
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE HEADERS                                                              
*                                                                               
VVALHEAD XC    TOTWIDTH,TOTWIDTH   NOT CHECKING REPORT WIDTH YET                
         LLC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,5                                                             
         LA    R4,4                START ON HEAD 4                              
         MVI   ANYROWSW,C'N'                                                    
         MVI   COUNTLEV,0                                                       
         CLC   8(3,R2),=C'MED'     UNLESS FIRST HEAD NOT MEDIA                  
         BE    VVH2                                                             
         LA    R4,5                                                             
         MVI   ACOMLEV,1                                                        
*                                                                               
VVH2     MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,2          (COLUMN 2)                                   
         STC   R4,LASTHEAD                                                      
         GOTO1 AVALROW,GEND                                                     
         BL    MYEND                                                            
         BH    MYCURSOR                                                         
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    VVHX                                                             
         LLC   R4,LASTHEAD                                                      
         LA    R4,1(R4)                                                         
         BCT   R3,VVH2                                                          
*                                                                               
VVHX     B     XIT                                                              
         SPACE 2                                                                
* VALIDATE MID                                                                  
*                                                                               
VVALMID  MVI   MYPOSO,C'M'                                                      
         MVI   MYPOSO+1,1                                                       
         MVI   MYPOSO+2,1                                                       
         GOTO1 AVALROW,GEND                                                     
         BL    MYEND                                                            
         BH    MYCURSOR                                                         
         B     XIT                                                              
         SPACE 2                                                                
* VALIDATE ROWS                                                                 
*                                                                               
VVALROWS MVI   TOTWIDTH+1,1        START CHECKING REPORT WIDTH NOW              
         ST    R2,ALASTCOL                                                      
         LLC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,6                                                             
         BAS   RE,DELINS           CHECK FOR DELETE OR INSERT                   
         LA    R4,ROWWIDS                                                       
         XC    ROWWIDS,ROWWIDS                                                  
         LA    R5,1                                                             
*                                                                               
VVR2     XC    MYPOSO,MYPOSO                                                    
         STC   R5,ROWWIDTH         STORE DETAIL ROW NUMBER IN ROWWIDTH          
*  NOW    GOTO1 AVALROW,GEND        FOR NOW                                     
*   IN    BL    MYEND                                                           
* VALMAC  BH    MYCURSOR                                                        
         BAS   RE,VALMAC                                                        
         SR    R0,R0                                                            
         CLI   TOTWIDTH+1,1                                                     
         BNH   VVR3                                                             
         IC    R0,TOTWIDTH+1                                                    
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         CLI   ROW1WIDE,0                                                       
         BNE   VVR3                                                             
         STC   R0,ROW1WIDE                                                      
*                                                                               
VVR3     STC   R0,0(R4)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BAS   RE,BUMP                                                          
         BCT   R3,VVR2                                                          
*                                                                               
         STC   R0,ROWWIDTH         TOTAL ROW WIDTH                              
         LA    R4,ROWWIDS          FIGURE OUT TOTAL WIDTH FROM ROW2,            
         LA    R0,L'ROWWIDS        ROW3, ETC.                                   
         SR    RF,RF                                                            
*                                                                               
VVR4     SR    RE,RE                                                            
         ICM   RE,1,0(R4)                                                       
         BZ    VVR6                                                             
         IC    RF,ROWWIDTH                                                      
         SR    RF,RE                                                            
         BP    *+12                                                             
         BZ    VVR4A                                                            
         B     BADWIDE         NO NEED TO DIE                                   
*        BZ    *+8                                                              
*        DC    H'0'                                                             
         BCTR  RF,0                                                             
VVR4A    STC   RF,0(R4)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,VVR4                                                          
*                                                                               
VVR6     L     R2,ALASTCOL                                                      
         CLI   ANYROWSW,C'N'                                                    
         BE    BADNEED1                                                         
*                                                                               
         TM    COUNTLEV,X'80'      TEST FOR COUNT LEVEL                         
         BO    *+18                                                             
         MVI   COUNTLEV,0          NO                                           
         XC    TOTLEVS,TOTLEVS                                                  
         B     VVR8                                                             
         NI    COUNTLEV,X'7F'      YES-                                         
         LLC   R1,COUNTLEV                                                      
         LA    R1,LEVELS-1(R1)                                                  
         CLI   0(R1),QPRD          TEST COUNTING PRODUCTS OR ESTIMATES          
         BE    VVR7                                                             
         CLI   0(R1),QPRDNM                                                     
         BE    VVR7                                                             
         CLI   0(R1),QEST                                                       
         BE    VVR7                                                             
         CLI   0(R1),QESTNM                                                     
         BNE   VVR8                                                             
*                                                                               
VVR7     LLC   R1,COUNTLEN         YES-ADD 2 TO COUNTLEN TO INCLUDE CLT         
         LA    R1,2(R1)                (IN CASE THERE ARE TOTALS ABOVE          
         STC   R1,COUNTLEN              CLIENT)                                 
         OI    COUNTLEN,X'80'      INDICATE WE'VE DONE THIS                     
*                                                                               
VVR8     LLC   R4,LASTHEAD                                                      
         LA    R4,3(R4)                                                         
         CHI   R4,9                                                             
         BH    *+8                                                              
         LA    R4,9                                                             
         CLM   R4,1,MYFIRSTH                                                    
         BNH   *+8                                                              
         STC   R4,MYFIRSTH                                                      
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE COLUMNS                                                              
*                                                                               
VVALCOLS LLC   R0,MAX                                                           
         LTR   R3,R0                                                            
         BZ    XIT                                                              
         BAS   RE,DELINS           CHECK FOR DELETE OR INSERT                   
         CLI   5(R2),0             NEED AT LEAST ONE COLUMN                     
         BE    BADNEED1                                                         
         MVI   MYLABEL,C'A'                                                     
         ST    R2,ALASTCOL                                                      
         BAS   RE,SETMAX           SET LAST COLUMN FOR COMPUTE                  
         TM    DRINDS2,GLEXTBOX    TEST EXTRA BOX BETWEEN ROWS AND COLS         
         BZ    *+16                                                             
         LH    RE,TOTWIDTH         YES-INCREASE WIDTH BY ONE                    
         LA    RE,1(RE)                                                         
         STH   RE,TOTWIDTH                                                      
         L     R3,=A(EDITLIST)                                                  
         A     R3,RELO                                                          
         OI    COLIND2,COLIBLBL    ALL COLS ARE BILLABLE UNTIL PROVEN           
*                                  OTHERWISE                                    
VVALCOL2 XC    MYPOSO,MYPOSO                                                    
         MVC   0(1,R3),MYLABEL     SET LABEL IN EDIT LIST                       
* SET COLUMN NUMBER                                                             
         LR    RF,R0                                                            
         LLC   RE,MAX                                                           
         SR    RF,RE                                                            
         LPR   RF,RF                                                            
         LA    RF,1(RF)            COL NUM IS 1 RELATIVE                        
         OC    RPT2ID,RPT2ID       HAVE CONTINUE REPORT?                        
         BZ    VVALCOL3            NO                                           
         L     R1,ATWA                                                          
         USING CONHEADH-64,R1                                                   
         CLC   RPT2ID,WRINAM       ARE WE ON THE SECOND REPORT?                 
         DROP  R1                                                               
         BNE   VVALCOL3            NO                                           
         AHI   RF,14               YES - ALREADY DID THE FIRST 14 COLS!         
*                                                                               
VVALCOL3 STC   RF,COLNUM           COL # (USED FOR INDEX TO XTCOL TAB)          
*                                                                               
         GOTO1 AVALCOL,GEND                                                     
         BNE   MYCURSOR                                                         
         BAS   RE,BUMP                                                          
         MVC   MYLABEL,8(R2)                                                    
         BAS   RE,BUMP                                                          
         LA    R3,4(R3)                                                         
         BCT   R0,VVALCOL2                                                      
*                                                                               
         TM    COLIND3,COLIUNIV    HAVE UDEM1-8 KEYWORD?                        
         BZ    VVALCOL4            NO                                           
         OC    SBQUNBK,SBQUNBK     HAVE A UNIVERSE BOOK?                        
         BZ    UNIVERR             NO - ERROR                                   
         CLI   MKTLEV,0            MARKET KEYWORD IN ROWS?                      
         BNE   *+12                YES                                          
         TM    DATAIND2,DISTA      STATION KEYWORD IN ROWS?                     
         BZ    UNIVERR2            NO - ERROR                                   
         LA    RE,UNIVALS          UNIVERSE VALUES FOR FIRST 8 DEMOS            
         STCM  RE,15,SBUNVDEM      A(UNIVERSE FOR FIRST 8 DEMOS)                
*                                                                               
VVALCOL4 TM    ROWIND,ROWIPER      TEST PERIOD IN ROWS                          
         BO    VVALCOL6                                                         
         TM    SBQPER,SBQPDY       NO-TEST DAYS IN COLUMNS                      
         BZ    VVALCOL5                                                         
         CLI   WEKHI,2             YES-MAX WEEK IS 2 AND NO MONTHS              
         BH    VVALCPER                OR QUARTERS                              
         TM    SBQPER,SBQPMN+SBQPQT                                             
         BNZ   VVALCPER                                                         
*                                                                               
VVALCOL5 TM    SBQPER,SBQPWK       TEST WEEKS IN COLUMNS                        
         BZ    VVALCOL6                                                         
         CLI   MONHI,3             YES-MAX MONTH IS 3 AND                       
         BH    VVALCPER                MAX QUARTER IS 1                         
         CLI   QTRHI,1                                                          
         BH    VVALCPER                                                         
*                                                                               
VVALCOL6 TM    DOWNOPT,GLDLACTV    TEST DOWNLOADING                             
         BO    XIT                 YES-SKIP WIDTH CHECKS                        
         CLC   TOTWIDTH,=H'80'     CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   WIDTHOPT,C'N'       ONLY 80 ALLOWED WITH NARROW OPT              
         BE    VVALCBIG                                                         
         CLC   TOTWIDTH,=H'132'    CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   WIDTHOPT,C'W'                                                    
         BNE   VVALCBIG                                                         
         CLC   TOTWIDTH,=H'165'                                                 
         BH    VVALCBIG                                                         
         B     XIT                                                              
*                                                                               
VVALCPER DS    0H                                                               
         MVC   GTMSGNO,=Y(PEXERR)                                               
         B     VVALCEND                                                         
*                                                                               
VVALCBIG DS    0H                                                               
         MVC   GTMSGNO,=Y(TOOWIDE)                                              
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GTASUBST       A(SUBST TEXT)                                
         MVI   ELEM,4              L'SUBST TEXT + 1                             
         EDIT  (2,TOTWIDTH),(3,ELEM+1)                                          
*                                                                               
VVALCEND L     R2,ALASTCOL                                                      
         B     MYEND                                                            
*                                                                               
BADNEED1 DS    0H                                                               
         MVC   GTMSGNO,=Y(NEED1ERR)                                             
         B     MYEND                                                            
*                                                                               
BADWIDE  DS    0H                                                               
         L     R2,ALASTCOL                                                      
         MVC   GTMSGNO,=Y(1218)                                                 
         B     MYEND                                                            
*                                                                               
UNIVERR  L     R2,ALASTCOL                                                      
         MVC   GTMSGNO,=Y(1474)                                                 
         B     MYEND                                                            
*                                                                               
UNIVERR2 L     R2,ALASTCOL                                                      
         MVC   GTMSGNO,=Y(1475)                                                 
         B     MYEND                                                            
***                                                                             
*              CHECK FOR MACRO EXPRESSIONS                                      
***                                                                             
VALMAC   NTR1                                                                   
         CLC   8(2,R2),=C'MC'      ALL MACROS START WITH MC...                  
         BE    VALMAC2                                                          
VALMACA  GOTO1 AVALROW,GEND        FOR NOW                                      
         BL    MYEND                                                            
         BH    MYCURSOR                                                         
         B     XIT                                                              
         SPACE 1                                                                
VALMAC2  L     R4,=A(MACTABLE)                                                  
         A     R4,RELO                                                          
         GOTO1 ANY                                                              
         SPACE 1                                                                
VALMAC4  CLI   0(R4),X'FF'                                                      
         BE    VALMACA             SEE IF REGULAR KEYWORD                       
         CLC   0(8,R4),WORK        CHECK FOR MATCH IN TABLE                     
         BE    VALMAC6                                                          
         LA    R4,L'MACENTRY(R4)                                                
         B     VALMAC4                                                          
         SPACE 1                                                                
VALMAC6  L     R3,12(R4)           PICK UP END ADDRESS                          
         A     R3,RELO                                                          
         L     R4,8(R4)                    AND START ADDRESS                    
         A     R4,RELO                                                          
         SPACE 1                                                                
VALMAC8  BAS   RE,GENMAC           GENERATE A COLUMN FOR EACH ENTRY             
         CR    R4,R3               UNTIL WE GET TO THE END                      
         BE    XIT                                                              
         LA    R4,8(R4)                                                         
         B     VALMAC8                                                          
         EJECT                                                                  
*              GENERATE A COLUMN DERIVED FROM MACRO                             
         SPACE 3                                                                
*              INPUT               R4=A(CL4 ENTRY NAME)                         
*                                  R2=A(FIELD HEADER)                           
         SPACE 1                                                                
GENMAC   NTR1                                                                   
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         XC    P,P                 BORROW P FOR THIS ROUTINE                    
         LA    R1,8                FIGURE L'ENTRY                               
         MVC   P+100(8),0(R4)                                                   
         LA    R3,P+107                                                         
         SPACE 1                                                                
GENMAC2  CLI   0(R3),C' '                                                       
         BH    GENMAC4                                                          
         BCTR  R3,0                                                             
         BCT   R1,GENMAC2                                                       
         DC    H'0'                                                             
         SPACE 1                                                                
GENMAC4  STC   R1,P+5              THIS BECOMES THE NEW LENGTH                  
         OI    P+4,X'20'           PREVIOUSLY VALIDATED (FOR SECURITY)          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+8(0),P+100        PUT IN DERIVED ENTRY NAME                    
         LA    R2,P                                                             
         GOTO1 AVALROW,GEND        FOR NOW                                      
         BL    MYEND                                                            
         BH    MYCURSOR                                                         
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
SETMAX   NTR1                                                                   
         MVI   BYTE,C'A'           FIND LAST INPUT COLUMN                       
SETMAX2  CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVC   DRCMPMAX,BYTE                                                    
         BAS   RE,BUMP                                                          
         MVC   BYTE,8(R2)                                                       
         BAS   RE,BUMP                                                          
         BCT   R0,SETMAX2                                                       
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO DELETE OR INSERT UNPROTECTED FIELDS                                
* INPUT  : R2=A(FIRST UNPROTECTED FIELD)                                        
*          R3=NUMBER OF INPUT FIELDS                                            
*                                                                               
DELINS   NTR1  ,                                                                
         CLI   OFFLINE,C'Y'        ONLY FOR ONLINE                              
         BE    DELINSX                                                          
         CLI   PFKEY,3             TEST PF3 OR PF4 HIT                          
         BE    *+12                                                             
         CLI   PFKEY,4                                                          
         BNE   DELINSX                                                          
         L     R5,SYSPARMS                                                      
         L     R5,0(R5)                                                         
         USING TIOBD,R5                                                         
         LH    R4,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         A     R4,ATWA             INTO TWA                                     
         CR    R2,R4                                                            
         BE    *+16                                                             
         BAS   RE,BUMPTOUN                                                      
         BCT   R3,*-10                                                          
         B     DELINSX             (NOT IN THIS PART OF THE SCREEN)             
         CLI   PFKEY,3                                                          
         BE    DELINS3                                                          
         XC    BLOCK(80),BLOCK     PF4=INSERT                                   
*                                                                               
DELINS2  MVC   BLOCK+80(80),8(R2)  SAVE THIS FIELD                              
         LLC   R1,0(R2)            GET L'FIELD-1 INTO R1                        
         AHI   R1,-9                                                            
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         AHI   R1,-8                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLOCK       MOVE IN PREVIOUS (OR CLEAR)                  
         OI    6(R2),X'80'                                                      
         MVC   BLOCK(80),BLOCK+80                                               
         BAS   RE,BUMPTOUN                                                      
         BCT   R3,DELINS2                                                       
         MVC   GTMSGNO,=Y(INSMESS)                                              
         MVI   GTMTYP,C'I'    SET INFO TYPE MESSAGE                             
         LR    R2,R4                                                            
         B     DELINS9                                                          
*                                                                               
DELINS3  LLC   R1,0(R2)            GET L'FIELD-1 INTO R1 FOR FIRST TIME         
         AHI   R1,-9                                                            
         TM    1(R4),X'02'                                                      
         BNO   *+8                                                              
         AHI   R1,-8                                                            
*                                                                               
DELINS4  LR    R4,R2                                                            
         BAS   RE,BUMPTOUN                                                      
         CLI   5(R2),0                                                          
         BE    DELINS6                                                          
         IC    R1,0(R4)            GET L'FIELD-1 INTO R1                        
         AHI   R1,-9                                                            
         TM    1(R4),X'02'                                                      
         BNO   *+8                                                              
         AHI   R1,-8                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),8(R2)       MOVE NEXT INTO THIS                          
         OI    6(R4),X'80'                                                      
         BCT   R3,DELINS4                                                       
*                                                                               
DELINS6  EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R4),8(R4)       CLEAR LAST ONE                               
         OI    6(R4),X'80'                                                      
         LR    R2,R4                                                            
         MVC   GTMSGNO,=Y(DELMESS)                                              
         MVI   GTMTYP,C'I'          SET INFO TYPE MESSAGE                       
*                                                                               
DELINS9  OI    GENSTAT2,USGETTXT                                                
         GOTO1 ERREX                                                            
*                                                                               
DELINSX  B     XIT                                                              
         DROP  R5                                                               
         SPACE  2                                                               
BUMPTOUN LLC   RF,0(R2)            GET TO NEXT UNPROTECTED FIELD                
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         EJECT                                                                  
* DRONE UTILITIES                                                               
*                                                                               
VINTDRON MVI   DRWHO,DRSPTWHO      INITIALIZATION                               
         MVI   DRACTION,DRINIT                                                  
         MVC   DRDICT,=CL8'SPWRI'                                               
         MVC   DRALTDIC,=CL8'DRIVER'                                            
         MVC   DRCOMFAC,ACOMFACS                                                
         MVC   DRMAXWID,=H'999'    FORCE BIG - I CHECK WIDTH                    
         MVI   DRCMPMAX,C'N'       MAX COL FOR COMPS                            
***                                                                             
* SET SECURITY BLOCK ONCE... FOR NOW ONLY DRCOLS AND DRROWS LOOK                
* AT THE SECURITY BLOCK                                                         
***                                                                             
         L     R1,ATWA                                                          
         USING T204FFD,R1                                                       
         OC    T204FFD+4(2),T204FFD+4    ON NEW SECURITY?                       
         BZ    *+10                      NO, DO NOT PASS SECRET BLOCK           
         MVC   DRSECBLK,ASECBLK    PASS A(SECRET) TO DRONE                      
         GOTO1 DRONE,DMCB,DRGEN                                                 
         B     XIT                                                              
         DROP  R1                                                               
*                                                                               
VWRPDRON MVI   DRACTION,DRWRAPUP   WRAP UP                                      
         GOTO1 DRONE,DMCB,DRGEN                                                 
         BAS   RE,TRACDRON         (OPTIONAL TRACE)                             
         B     XIT                                                              
*                                                                               
TRACDRON NTR1                                                                   
         L     R2,ASPOOLD                                                       
         USING SPOOLD,R2                                                        
         CLI   TRACEOPT,C'Y'       DRONE TRACING OPTION                         
         BNE   XIT                                                              
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         L     R3,ADPGPROG                                                      
*                                                                               
TRACD2   CLI   0(R3),0                                                          
         BE    XIT                                                              
         LLC   R4,1(R3)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         LA    R4,1(R4)                                                         
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         GOTO1 HEXOUT,DMCB,(R3),BLOCK,(R4),=C'SEP'                              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),BLOCK                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         LA    R5,BLOCK+1(R4)                                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         BASR  RE,RF                                                            
         MVC   P,BLANKS                                                         
         BASR  RE,RF                                                            
         LA    R3,1(R3,R4)                                                      
         B     TRACD2                                                           
         DROP  R2                                                               
         EJECT                                                                  
* POSITION CURSOR TO CORRECT FIELD IN ERRORS                                    
* INPUT  : R2 = A(SCREEN HEADER)                                                
*          FIELDERR = NUMBER OF FIELD IN ERROR                                  
*                                                                               
VCURSERR CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    VERRXIT                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VERRXIT                                                          
         L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
         LA    RE,8(R2)                                                         
         SR    R1,R1               COMPUTE FIELD DISPLACEMENT INTO R1           
         SR    R0,R0                                                            
         ICM   R0,1,5(R2)          R0 HAS FIELD LENGTH                          
         BZ    CURSERR5                                                         
         LLC   RF,FIELDERR                                                      
         BCT   RF,CURSERR2         CHECK IF ERROR IS IN FIELD 1                 
         B     CURSERR4                                                         
*                                                                               
CURSERR2 CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   CURSERR4                                                         
         BCT   RF,CURSERR4                                                      
         LA    R1,1(R1)            FOUND ENOUGH - SPACE PAST LAST               
         B     CURSERR6                                                         
*                                                                               
CURSERR4 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CURSERR2                                                      
CURSERR5 SR    R1,R1               ERROR - DIDN'T FIND ENOUGH COMMAS            
*                                                                               
CURSERR6 STC   R1,TIOBCURI         SET CURSOR DISPLACEMENT WITHIN FIELD         
         B     VERRXIT                                                          
         EJECT                                                                  
* COMMON EXIT ROUTINES                                                          
*                                                                               
BUMP     LLC   RF,0(R2)            GET TO NEXT SCREEN FIELD                     
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
MYEND    MVI   ERROR,X'FE'                                                      
         J     THEEND                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
         GOTO1 CURSERR                                                          
*                                                                               
THEEND   J     VEXIT                                                            
*                                                                               
VERRXIT  OC    GTMSGNO,GTMSGNO     USE NEW ERROR IF PRESENT                     
         JNZ   NEWTRAP                                                          
         CLI   ERROR,X'FE'                                                      
         JE    VERRX2                                                           
         J     TRAPERR             SYSTEM MESSAGE                               
*                                                                               
VEXIT    DS    0H                                                               
         OI    6(R2),X'40'         POSITION CURSOR                              
         OC    GTMSGNO,GTMSGNO     USE NEW ERROR IF PRESENT                     
         JNZ   NEWTRAP                                                          
         CLI   ERROR,X'FE'                                                      
         JNE   TRAPERR                                                          
         J     VERRX2                                                           
*                                                                               
NEWTRAP  OI    GENSTAT2,USGETTXT                                                
TRAPERR  GOTO1 ERREX                                                            
VERRX2   GOTO1 ERREX2              MY OWN ERROR MESSAGE                         
*                                                                               
XITLO    SR    R1,R1                                                            
         CR    R1,RB                                                            
         J     XIT                                                              
*                                                                               
XITHI    SR    R1,R1                                                            
         CR    RB,R1                                                            
         J     XIT                                                              
*                                                                               
XITEQU   CR    RB,RB                                                            
         J     XIT                                                              
*                                                                               
XITNEQ   LTR   RB,RB                                                            
         J     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
* CONSTANTS TABLES ETC                                                          
*                                                                               
AXTRA    DS    0F                  ** EXTENSION ROUTINE ADDRESSES **            
AVALROW  DS    A                                                                
AVALCOL  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
*                                                                               
BLANKS   DC    CL132' '                                                         
         EJECT                                                                  
* EXECUTED INSTRUCTIONS                                                         
*                                                                               
GRPTEST  CLC   12(0,R3),=C'GRP'                                                 
NETTEST  CLC   12(0,R3),=C'NET'                                                 
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
       ++INCLUDE SPMGRTAB                                                       
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
* EXTENTION ROUTINES                                                            
*                                                                               
         DS    0H                                                               
EXTRA    NMOD1 0,**GENX**,RA,R7,R6                                              
         LR    RC,R1                                                            
         USING GEND,RC                                                          
         USING GETTXTD,GETTXTCB                                                 
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALROW                                                           
         B     VALCOL                                                           
         EJECT                                                                  
* ROUTINE TO VALIDATE A ROW                                                     
*                                                                               
VALROW   CLI   5(R2),0                                                          
         BE    VROWX                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP OFFLINE                                 
         BE    VROWC                                                            
         CLI   DDS,C'Y'            DDS?                                         
         BE    VROWC                                                            
         CLI   WLPROF,C'Y'         CHECK SECURITY AGENCY                        
         BNE   VROWC                                                            
*                                                                               
VROWB    TM    AUTH,X'40'          AUTHORIZED?                                  
         BNZ   VROWC                                                            
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VROWC                                                            
         MVI   ERROR,SECLOCK                                                    
         J     TRAPERR                                                          
*                                                                               
VROWC    MVI   ANYROWSW,C'Y'                                                    
         MVI   SAVTOTYP,0                                                       
         SR    RE,RE                                                            
         TM    COUNTLEV,X'80'      UNLESS COUNT ROW ENCOUNTERED,                
         BO    *+16                                                             
         IC    RE,COUNTLEV         BUMP ROW LEVEL                               
         LA    RE,1(RE)                                                         
         STC   RE,COUNTLEV                                                      
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,BLOCK),0                               
         LLC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADROW                                                           
         STC   R0,NROWS                                                         
         MVI   FIELDERR,1                                                       
         LA    R4,BLOCK                                                         
*                                                                               
         CLC   12(4,R4),=C'CGRP'   TEST ROW = LOWEST LEVEL CLIENT GROUP         
         BNE   VROW0                                                            
         CLI   SBQCGRD,0                                                        
         BE    BADROW                                                           
         MVI   15(R4),C'1'         YES - DETERMINE ITS LEVEL                    
         CLC   SBCGR1LN,SBCGR2LN                                                
         BE    VROW4                                                            
         MVI   15(R4),C'2'                                                      
         B     VROW4                                                            
*                                                                               
VROW0    CLC   12(4,R4),=C'SGRP'   TEST ROW = LOWEST LEVEL STA GROUP            
         BNE   VROW1                                                            
         CLI   SBQSGRD,C' '                                                     
         BNH   BADROW                                                           
         MVI   15(R4),C'1'         YES - DETERMINE ITS LEVEL                    
         CLC   SBSGR1LN,SBSGR2LN                                                
         BE    VROW4                                                            
         MVI   15(R4),C'2'                                                      
         B     VROW4                                                            
*                                                                               
VROW1    CLC   12(4,R4),=C'PGRP'   TEST ROW = LOWEST LEVEL PRDGRP               
         BNE   VROW2                                                            
         CLC   SBQPGR,BLNKS                                                     
         BNH   BADROW                                                           
         MVI   15(R4),C'1'         YES - DETERMINE ITS LEVEL                    
         CLC   SBPGR1LN,SBPGR2LN                                                
         BE    VROW4                                                            
         MVI   15(R4),C'2'                                                      
         B     VROW4                                                            
*                                                                               
VROW2    CLC   12(4,R4),=C'MGRP'   TEST ROW = LOWEST LEVEL MKTGRP               
         BNE   VROW3                                                            
         CLI   SBQMGR,0                                                         
         BE    BADROW                                                           
         MVI   15(R4),C'1'         YES - DETERMINE ITS LEVEL                    
         CLC   SBMGR1LN,SBMGR2LN                                                
         BE    VROW4                                                            
         MVI   15(R4),C'2'                                                      
         CLC   SBMGR2LN,SBMGR3LN                                                
         BE    VROW4                                                            
         MVI   15(R4),C'3'                                                      
*                                                                               
VROW3    CLI   SBQMED,C'N'         TEST MEDIA = N OR C                          
         BE    *+12                                                             
         CLI   SBQMED,C'C'                                                      
         BNE   VROW4                                                            
         CLC   12(4,R4),=C'STA '   AND ROW IS STA OR NETWORK                    
         BE    *+14                                                             
         CLC   12(7,R4),=C'NETWORK'                                             
         BNE   VROW4                                                            
         MVC   12(6,R4),=C'STANET' YES-ALTER ROW TO STANET                      
         MVI   0(R4),6                                                          
         CLI   18(R4),C'K'         NETWORK                                      
         BNE   VROW4                                                            
         MVI   18(R4),C'N'                                                      
         MVI   0(R4),7                                                          
*                                                                               
VROW4    BRAS  RE,VROWDRON         VALIDATE A ROW ENTRY                         
         BNE   BADROW                                                           
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         CLC   12(5,R4),=C'TEXT '  MAYBE TEXT                                   
         BNE   *+8                                                              
         BAS   RE,GENTEXT                                                       
         CLI   DRATTRIB,C'C'       COLUMN ONLY ENTRIES NOT ALLOWED              
         BE    BADROW                                                           
         CLI   DRATTRIB,C'D'       DETIAL ONLY ENTIES IN HEAD OR MID            
         BNE   *+12                NOT ALLOWED                                  
         CLI   MYPOSO,0                                                         
         BNE   BADROW                                                           
         CLI   DRATTRIB+1,C'P'     TEST PERIOD ROW                              
         BNE   VROW5                                                            
*                                                                               
         TM    OPTIND5,OPTISO      ISO DATE OPTION?                             
         BZ    *+8                                                              
         MVI   DRLENO,10           FORCE IT'S LENGTH TO 10                      
*                                                                               
         XC    DUB,DUB             SETTING TO ZERO VOIDS CHECKS                 
         OC    ARFPBLK,ARFPBLK     RFP MODE?                                    
         BNZ   VROW4A               YES - SKIP LEN CHECK                        
*******  GOTO1 PERVERT,DMCB,SBQSTART,SBQEND   GET PERIOD LENGTH                 
         GOTO1 PERVERT,DMCB,SBQREQST,SBQREQND GET PERIOD LENGTH                 
         MVC   DUB,8(R1)                                                        
*                                                                               
VROW4A   OI    ROWIND,ROWIPER      YES-                                         
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         CLI   DRARGSI,C'F'        TEST CHILD SPOT FLIGHTS                      
         BNE   *+12                                                             
         OI    SBQPER,SBQPFL                                                    
         B     VROW6                                                            
         CLI   DRARGSI,C'Y'        TEST YEARS                                   
         BNE   *+12                                                             
         OI    SBQPER,SBQPYR                                                    
         B     VROW6                                                            
         CLI   DRARGSI,C'Q'        TEST QUARTERS                                
         BNE   *+12                                                             
         OI    SBQPER,SBQPQT                                                    
         B     VROW6                                                            
*                                                                               
         CLI   DRARGSI+2,C'A'      SPECIAL MONTH ROW? (AVGMON)                  
         BNE   *+8                  NO                                          
         OI    ROWIND2,ROWAPER      YES - SET IND FOR VALPEX                    
*                                                                               
         CLI   DRARGSI,C'M'        TEST MONTHS                                  
         BNE   *+12                                                             
         OI    SBQPER,SBQPMN                                                    
         B     VROW6                                                            
         CLI   DRARGSI,C'W'        TEST WEEKS                                   
         BNE   VROW4B                                                           
         CLC   DUB+4(2),=H'60'     CHECK NOT MORE WEEKS THAN ALLOWED            
         BH    PERERR                                                           
         BL    *+14                                                             
         OC    10(2,R1),10(R1)                                                  
         BNZ   PERERR                                                           
         OI    SBQPER,SBQPWK                                                    
         B     VROW6                                                            
VROW4B   CLI   DRARGSI,C'D'        TEST DAYS                                    
         BNE   VROW6               EDATES DOESN'T NEED PER TABLE                
         CLC   DUB(2),=H'92'                                                    
         BH    PERERR                                                           
         OI    SBQPER,SBQPDY                                                    
         B     VROW6                                                            
*                                                                               
VROW5    CLI   DRATTRIB+1,C'C'     TEST CLEARANCE STATUS ROW                    
         BNE   VROW6                                                            
         OI    SBQREAD,SBQRDCLS    YES-READ CLEARANCE STATUS RECORDS            
         NI    SBQSKIP,255-SBQSKBUY    AND READ BUYS                            
*                                                                               
VROW6    CLC   12(4,R4),=C'RANK'                                                
         BNE   VROW8                                                            
         OI    ROWIND2,ROWIRANK                                                 
         CHI   R0,1                IF ON ITS OWN, MAKE NO PRINT                 
         BE    VROW21                                                           
         LA    R4,42(R4)           RANK NEEDS A COMPUTE EXPRESSION              
         BCTR  R0,0                                                             
         XR    R1,R1                                                            
         IC    R1,FIELDERR                                                      
         AHI   R1,1                                                             
         STC   R1,FIELDERR                                                      
         MVI   DRCMPMAX,C'N'                                                    
*                                                                               
VROW7    BRAS  RE,VCMPDRON         VALIDATE A COMPUTE EXPRESSION                
         BNE   BADROW                                                           
         OC    TOTWIDTH,TOTWIDTH   IF WE ARE IN THE ROWS                        
         BZ    VROW8                                                            
         CHI   R3,1                CHECK THIS IS NOT THE LAST ROW               
         BE    BADLRANK                                                         
         LR    R3,R2                                                            
         LLC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   5(R2),0             AND THERE IS INPUT IN NEXT                   
         LR    R2,R3                                                            
         BE    BADLRANK            NOT GOOD TO RANK ON LAST ROW                 
*                                                                               
VROW8    CLC   12(5,R4),=C'CFMON'  MON OF SERVICE FOR CASHFLOW REP              
         BNE   VROW8A                                                           
         TM    OPTIND3,OPTCFNT     DON'T SET TOTAL RTN                          
         BO    *+8                                                              
         OI    DRTOTAL,X'80'       TURN ON TOTALS                               
         B     VROW9                                                            
*                                                                               
VROW8A   CLC   12(4,R4),=C'UDEF'   TEST USER DEFINITION                         
         BE    *+14                                                             
         CLC   12(4,R4),=C'UCOM'   TEST USER COMMENT                            
         BNE   VROW9                                                            
         BAS   RE,VALUDEF          YES-VALIDATE                                 
         BNE   BADROW                                                           
*                                                                               
VROW9    CLI   MYPOSO,C'H'         SPECIAL FOR HEADS                            
         BNE   VROW10                                                           
         BAS   RE,HEADROW                                                       
         B     VROWNXT                                                          
*                                                                               
VROW10   CLI   MYPOSO,C'M'         SPECIAL FOR MID                              
         BNE   VROWNXT                                                          
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH ONE SPACE                               
         B     VROWNXT                                                          
         SPACE 2                                                                
HEADROW  DS    0H                  SPECIAL FOR HEADLINE ROW                     
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         MVI   DRFSPACE,0                                                       
         BR    RE                                                               
         EJECT                                                                  
* CHECK FOR SUBSIDIARY ROW EXPRESSIONS                                          
*                                                                               
VROW12   CLI   12(R4),C'*'         TOTAL EXPRESSION                             
         BNE   VROW14                                                           
         OI    DRTOTAL,X'80'                                                    
         MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         TM    OPTIND,OPTITOSK     TEST FOR TOTSKIP OPTION                      
         BZ    VROW13                                                           
         OI    DRLAST,X'80'        YES-GENERATE LAST WITH SKIP                  
         OI    DRLOPTS,DRLSKIP                                                  
         OI    DRTOPTS,DRTSKIP         AND SKIP AFTER TOTAL                     
*                                                                               
VROW13   CLI   0(R4),2                                                          
         BH    BADROW                                                           
         BL     *+10                                                            
         MVC   SAVTOTYP,13(R4)                                                  
         TM    COUNTLEV,X'80'      TEST COUNT LEVEL FOUND YET                   
         BO    VROWNXT                                                          
         LA    RF,1                NO-TURN ON BIT FOR POSSIBLE                  
         SLL   RF,31                  COUNTING AT THIS TOTAL LEVEL              
         CLI   COUNTLEV,16                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         LLC   R1,COUNTLEV                                                      
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         SRL   RF,1                                                             
         BCT   R1,*-4                                                           
         O     RF,TOTLEVS                                                       
         STCM  RF,12,TOTLEVS                                                    
         B     VROWNXT                                                          
*                                                                               
VROW14   CLC   12(5,R4),=C'SKIP '  SKIP TO CHANNEL 1 AFTER BREAK                
         BNE   VROW16                                                           
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         B     VROWNXT                                                          
*                                                                               
VROW16   CLC   12(6,R4),=C'SPACE ' SPACE OPTION                                 
         BNE   VROW18                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH AT LEAST ONE SPACE                      
         CLI   1(R4),0             CHECK SECOND PARAMETER                       
         BE    VROWNXT                                                          
         MVC   DRLSPACE,11(R4)                                                  
         CLI   DRLSPACE,0          S/B 1-3 LINES                                
         BE    BADROW                                                           
         CLI   DRLSPACE,3                                                       
         BH    BADROW                                                           
         B     VROWNXT                                                          
*                                                                               
VROW18   CLC   12(4,R4),=C'DET '   DET=N OR D=N                                 
         BE    VROW19                                                           
         CLC   12(2,R4),=C'D '                                                  
         BNE   VROW20                                                           
*                                                                               
VROW19   OI    DRTOTAL,X'80'                                                    
         MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         MVC   DRTDET(1),11(R4)    NUMBER OF DETAILS                            
         CLI   DRTDET,0                                                         
         BE    BADROW                                                           
         TM    DRINDS2,GLPBREAK    TEST PAGE BREAK REQUIRED                     
         BZ    VROWNXT                                                          
         OI    DRFIRST,X'80'       YES                                          
         OI    DRFOPTS,DRFSKIP                                                  
         B     VROWNXT                                                          
*                                                                               
VROW20   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VROW22                                                           
*                                                                               
VROW21   NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         MVC   DRH1LIT,BLNKS                                                    
         MVC   DRH2LIT,BLNKS                                                    
         MVC   DRH3LIT,BLNKS                                                    
         MVC   DRH4LIT,BLNKS                                                    
         B     VROWNXT                                                          
*                                                                               
VROW22   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VROW24                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VROWNXT                                                          
*                                                                               
VROW24   LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VROW25                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VROW25                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VROW25                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VROW25                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VROW28                                                           
*                                                                               
VROW25   DS    0H                                                               
         CLC   =C'UCOM',BLOCK+54   DON'T ALLOW HEAD OVERRIDES FOR UCOM          
         BE    *+14                                                             
         CLC   =C'UDEF',BLOCK+54   OR UDEF                                      
         BNE   *+14                                                             
         MVC   GTMSGNO,=Y(ENOHOV)                                               
         J     XITHI                                                            
*                                                                               
         CLI   MYPOSO,0            CHECK NOT HEADLINE OR MIDLINE                
         BNE   BADROW                                                           
         XC    0(64,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),2                                                          
         BL    VROWNXT             HN=X CAUSES REMOVAL                          
         USING DRHEADD,R1                                                       
         OI    DRHEAD,X'80'        OTHERWISE TURN IT BACK ON                    
         MVC   DRHLITL,1(R4)       PASS LITERAL LENGTH TO DRONE                 
         CLC   DRHLITL,DRLENO                                                   
         BNH   VROW26              CHECK LITERAL NOT WIDER THAN COLUMN          
         MVC   GTMSGNO,=Y(HOVERR)                                               
         J     XITHI                                                            
*                                                                               
VROW26   LLC   RE,DRHLITL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE IN THE HEADER LITERAL                   
         B     VROWNXT                                                          
         MVC   DRHLIT(0),22(R4)    ** EXECUTED                                  
         DROP  R1                                                               
*                                                                               
VROW28   CLC   12(2,R4),=C'U '                                                  
         BNE   VROW30                                                           
         BRAS  RE,VUSRDRON                                                      
         JNE   XITHI                                                            
         B     VROWNXT                                                          
*                                                                               
VROW30   CLC   12(6,R4),=C'COUNT '   COUNT LEVEL                                
         BNE   VROW32                                                           
         TM    COUNTLEV,X'80'        ONLY ONE ALLOWED                           
         BO    BADROW                                                           
         OI    COUNTLEV,X'80'                                                   
         MVC   COUNTLEN,DRLENI     SAVE L'COUNTED ROW                           
         B     VROWNXT                                                          
*                                                                               
VROW32   CLC   12(4,R4),=C'SUM '   SUMMARY                                      
         BNE   VROW34                                                           
         OC    ASUMMARY,ASUMMARY   ONLY ONE SUMMARY ALLOWED                     
         BNZ   DUPSUM                                                           
         CLI   MYPOSO,C'H'         ONLY IN THE HEADLINES                        
         BNE   POSSUM                                                           
         MVC   SUMNAME,22(R4)      NO-SAVE SUMMARY NAME                         
         BAS   RE,VALSUM           AND VALIDATE                                 
         BNE   BADSUM                                                           
         OI    ASUMMARY,X'80'                                                   
         B     VROWNXT                                                          
*                                                                               
VROW34   CLC   12(6,R4),=C'TITLE ' TITLE                                        
         BNE   VROW36                                                           
         CLI   DRATTRIB+1,C'P'     MUST BE PERIOD                               
         BNE   BADROW                                                           
         CLI   MYPOSO,C'H'         IN HEADLINES                                 
         BNE   BADROW                                                           
         LA    R1,DRARGSO          INDICATE THAT PERIOD GOES INTO               
         USING GLARGSD,R1          THE TITLE                                    
         OI    GLOIND,GLOITITL                                                  
         MVI   DRNARGSO,16                                                      
         B     VROWNXT                                                          
*                                                                               
VROW36   CLC   12(5,R4),=C'PAGE '  RE-NUMBER PAGES FROM 1                       
         BNE   VROW37                                                           
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFPAGE     WITH PAGE OPTION                             
         OI    DRFOPTS,DRFSKIP     AND SKIP OPTION                              
         B     VROWNXT                                                          
*                                                                               
VROW37   CLC   =C'ACOM',12(R4)     USER WANTS ACOM PRINTED?                     
         BNE   VROW38              NO                                           
         TM    DTAIND11,DIACOM     ALREADY REQUESTED ACOM?                      
         BNZ   BADROW              YES - ERROR                                  
         CLI   MYPOSO,C'H'         HEADLINE?                                    
         BNE   BADROW              NO - ERROR                                   
         LLC   RE,MYPOSO+1         LEVEL                                        
         SHI   RE,3                WE START AT HEADLINE 4 OR 5                  
         LLC   RF,ACOMLEV          WILL BE ONE IF MED NOT USED                  
         SR    RE,RF                                                            
         STC   RE,ACOMLEV          LEVEL WILL EQUAL DROLEV                      
         OI    DTAIND11,DIACOM     READ ACOM RECORD                             
         B     VROWNXT                                                          
*                                                                               
VROW38   B     BADROW                                                           
*                                                                               
VROWNXT  LA    R4,20+22(R4)                                                     
         XR    R1,R1                                                            
         IC    R1,FIELDERR                                                      
         AHI   R1,1                                                             
         STC   R1,FIELDERR                                                      
         BCT   R0,VROW12                                                        
*                                                                               
         TM    DRTOTAL,X'80'       TEST TOTAL                                   
         BZ    VROW40                                                           
         CLC   BLOCK+12(6),=C'MKTRNK'  YES-TEST MARKET RANK                     
         BNE   *+8                                                              
         OI    ROWIND,ROWIMRTO                                                  
         TM    DRLAST,X'80'        TEST FOR LAST PARAMETER                      
         BZ    VROW40                                                           
         TM    OPTIND,OPTITOSK     YES-TEST TOTAL SKIP                          
         BZ    *+12                                                             
         MVI   DRTSPACE,0          YES-REMOVE TOTAL SPACING                     
         B     *+14                                                             
         MVC   DRTSPACE,DRLSPACE   MOVE SPACE OPTION TO TOTAL                   
         MVI   DRLAST,0            AND FORGET THE LAST                          
         MVI   DRLSPACE,0          REMOVE LAST SPACING                          
*                                                                               
VROW40   OC    TOTWIDTH,TOTWIDTH   IF WE ARE CHECKING WIDTH                     
         BZ    VROW42                                                           
         TM    DRFLAGO,X'80'                                                    
         BZ    VROW42                                                           
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         LLC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
*                                                                               
VROW42   CLI   MYPOSO,0            TEST VALIDATING A DETAIL ROW                 
         BNE   VROWGEN                                                          
         LA    R1,DRHEAD1          YES-                                         
         TM    0(R1),X'80'         TEST HEADLINE 1 IS ACTIVE                    
         BZ    VROWGEN                                                          
         CLC   1(8,R1),=CL8'HBUY'  YES-TEST FOR BUY HEADLINE                    
         BNE   VROWGEN                                                          
         MVI   24(R1),1            YES-ARG16=HEADLINE NUMBER                    
         MVI   25(R1),16               N'ARGS=16                                
         LA    RF,DRHEAD2                                                       
         MVC   0(26,RF),0(R1)                                                   
         MVI   24(RF),2            HEAD2                                        
         LA    RF,DRHEAD3                                                       
         MVC   0(26,RF),0(R1)                                                   
         MVI   24(RF),3            HEAD3                                        
*                                                                               
VROWGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   VROWX                                                            
*                                                                               
* BUILD PQINDEX TABLE HERE                                                      
         ICM   RF,15,APQINDEX                                                   
         BZ    VROWG50                                                          
         USING PQINDEX,RF                                                       
         LA    R0,MAXROWS+MAXMIDS+MAXHEADS                                      
*                                                                               
         OC    PQKEYWRD,PQKEYWRD   IN USE?                                      
         BZ    *+16                 NO                                          
         LA    RF,PQINDXEQ(RF)                                                  
         BCT   R0,*-14                                                          
         B     VROWG50                                                          
         MVC   PQKEYWRD,BLOCK+12                                                
         MVC   PQOUTLEN,DRLENO                                                  
         MVC   PQPOSO,MYPOSO                                                    
         MVC   PQHEAD1,DRH1LIT                                                  
         MVC   PQHEAD2,DRH2LIT                                                  
         MVC   PQHEAD3,DRH3LIT                                                  
         MVC   PQHEAD4,DRH4LIT                                                  
         DROP  RF                                                               
*                                                                               
VROWG50  MVC   DRPOSO,MYPOSO                                                    
*                                                                               
         TM    DRTOTAL,X'80'       TEST TOTAL REQUESTED                         
         BZ    VROWG90                                                          
         CLI   DRTDET,0            AND NOT DETAILED TOTAL                       
         BNE   VROWG90                                                          
         CLC   BLOCK+12(6),=C'MKTRNK' YES-TEST MARKET RANK                      
         BNE   VROWG60                                                          
         MVC   DRTRTN,=CL8'TMKTRNK'   YES-SPECIAL ROUTINE                       
         MVI   DRTNARGS,1                                                       
         MVC   DRTARGS(1),DRPOSO      ARGS = H OR M                             
         CLI   DRPOSO,0                                                         
         BNE   VROWG90                                                          
         MVC   DRTARGS(1),ROWWIDTH    OR WIDTH OF DETAIL LINE                   
         B     VROWG90                                                          
*                                                                               
VROWG60  CLC   BLOCK+12(5),=C'CFMON'  YES-TEST CASHFLOW MONTH                   
         BNE   VROWG70                                                          
         TM    OPTIND3,OPTCFNT        DON'T SET TOTAL RTN                       
         BO    *+10                                                             
         MVC   DRTRTN,=CL8'SUMMOS'    SET SPECIAL ROUTINE                       
         MVC   DRTARGS(1),ROWWIDTH ARG=DETAIL ROW NUMBER                        
         MVI   DRTNARGS,1                                                       
         B     VROWG90                                                          
*                                                                               
VROWG70  CLI   DRPOSO,0            TEST DETAIL ROW                              
         BNE   VROWG80                                                          
         MVI   DRTLITLN,0          YES-PASS THROUGH GENERIC TOTAL               
         MVC   DRTRTN,=CL8'TOTAL'      ROUTINE                                  
         MVC   DRTARGS(1),ROWWIDTH ARG=DETAIL ROW NUMBER                        
         MVI   DRTNARGS,1                                                       
         B     VROWG90                                                          
*                                  HEADLINE OR MIDLINE -                        
VROWG80  CLI   DRRTNO,C' '         TEST-OUT ROUTINE SPECIFIED                   
         BNH   VROWG90                                                          
         CLI   SAVTOTYP,C'-'       (*- SUPPRESSES THIS STUFF)                   
         BE    VROWG90                                                          
         MVC   DRTRTN,DRRTNO       YES-USE THIS FOR TOTAL AS WELL               
         MVC   DRTARGS,DRARGSO     AND PASS THROUGH THE ARGUMENTS               
         MVI   DRTNARGS,16                                                      
*                                                                               
VROWG90  BRAS  RE,GROWDRON         GENERATE THE ROW                             
         CLC   BLOCK+12(4),=C'RANK' TEST RANK ROW                               
         BNE   VROWG100                                                         
         CLI   NROWS,1             WITH A COMPUTE EXPRESSION                    
         BNH   VROWG100                                                         
         LA    R4,BLOCK+42                                                      
         BRAS  RE,GCMPDRON         YES-GENERATE COMPUTE                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
*                                                                               
VROWG100 TM    ASUMMARY,X'80'      TEST SUMMARY AT THIS LEVEL                   
         BZ    VROWX                                                            
         MVC   ASUMMARY,DRCURBUF   YES-SAVE A(CURRENT DRIVAL ELEMENT)           
         LA    R2,1                                                             
         BAS   RE,LGENRPTS         INSERT REPORT SEQUENCE ROW HERE              
*                                                                               
VROWX    J     XITEQU                                                           
         EJECT                                                                  
* VALIDATE UDEF/UCOM EXPRESSION                                                 
* INPUT  : R4=A(SCANNER BLOCK FOR UDEF EXPRESSION).                             
*          EXPRESSION IS OF THE FORM UDEF=AA/BB/CC/DD, WHERE AA, ETC            
*          ARE UDEF EXPRESSIONS. VALID ONES ARE P1, P2, E1 AND E2.              
*          OR UCOM=AA/BB/CC/DD WHERE AA, ETC ARE UCOM EXPRESSIONS.              
*          VALID ONES ARE E1-E4, P1-P4, AND M1-M4                               
* OUTPUT : DRIVER INPUT ROUTINE ARGS ARE 3 BYTES FOR EACH EXPRESSION:           
*          FORMAT IS +0(1) - UDEF TYPE CODE (1=P1,2=P2,3=E1,4=E2)               
*                    +1(1) - INPUT LENGTH                                       
*                    +2(1) - UDEF TYPE (D=DATE, C=CHARACTER, ETC)               
*          DRIVER OUTPUT ROUTINE ARGS ARE THE SAME AS INPUT ARGS.               
*          DRIVER HEADING ROUTINE ARGS ARE: +0(1) - N'UDEF EXPRESSIONS          
*                                           +1(1) - 1ST UDEF TYPE CODE          
*                                           +2(1) - 2ND UDEF TYPE CODE          
*                                           ETC.                                
*          SBEUDEF IS SET TO EXTRACT PRODUCT AND/OR ESTIMATE FIELDS.            
*                                                                               
         SPACE 1                                                                
VALUDEF  NTR1  ,                                                                
         MVC   FULL,12(R4)         SAVE TYPE - UDEF/UCOM                        
         CLI   1(R4),2             AT LEAST ONE UDEF EXPRESSION                 
         BL    VUDEFNE                                                          
         BE    *+12                                                             
         CLI   MYPOSO,0            NO MORE THAN ONE FOR HEADS AND MIDS          
         BNE   VUDEFNE                                                          
         CLI   1(R4),11            ELSE MAX 4 UDEF EXPRESSIONS                  
         BH    VUDEFNE                                                          
         LLC   RF,1(R4)                                                         
         LA    R1,DRARGSI          SET ARGUMENTS TO INPUT ROUTINE               
         XC    DRARGSI,DRARGSI                                                  
         SR    R2,R2               R2=LENGTH OF ARGS TO DRIVER ROUTINES         
         LA    R4,22(R4)           R4=A(UDEF EXPRESSION)                        
         SR    RE,RE               RE=WIDTH OF DRIVER INPUT FIELD               
         MVI   BYTE,0              BYTE=WIDTH OF COLUMN                         
*                                                                               
         CLC   FULL,=C'UCOM'                                                    
         BNE   VUDEF2                                                           
         OI    DATAIND9,DIUCOM     SET EXTRACT UCOM DATA                        
         CLC   0(2,R4),=C'E5'      ESTIMATE UCOMM 5?                            
         BE    *+10                YES                                          
         CLC   0(2,R4),=C'E6'      ESTIMATE UCOMM 6?                            
         BE    *+10                YES                                          
         CLC   0(2,R4),=C'E7'      ESTIMATE UCOMM 7?                            
         BE    *+10                YES                                          
         CLC   0(2,R4),=C'E8'      ESTIMATE UCOMM 8?                            
         BNE   VUDEF2              YES                                          
         OI    DTAIND11,DIUCOM8    EXTRACT ESTIMATE UCOMM 5-8                   
***                                                                             
* THE WRITER CALLS VVALCLT BEFORE VALIDATING ROWS SO WE NEED TO CALL            
* DDUCOM AGAIN WITH UCO8EST TO EXTRACT EST UCOMMS 5-8                           
***                                                                             
         OC    SBBCLT,SBBCLT       CLIENT CODE SET?                             
         BZ    VUDEF2              NO                                           
         LR    R0,RF               SAVE OFF RF                                  
         BAS   RE,EUCOM58          EXTRACT EST UCOMMS 5-8                       
         LR    RF,R0               RESTORE RF                                   
         XR    RE,RE               RE=WIDTH OF DRIVER INPUT FIELD               
*                                                                               
VUDEF2   CHI   RF,2                EACH EXPRESSION IS 2 CHARACTERS              
         BL    VUDEFNE                                                          
         LA    R3,UDEFTAB          VALIDATE THEM                                
         CLC   FULL,=C'UCOM'       IS THIS UCOM?                                
         BNE   *+8                                                              
         LA    R3,UCOMTAB                                                       
*                                                                               
VUDEF3   CLI   0(R3),0                                                          
         BE    VUDEFNE                                                          
         CLC   0(2,R4),0(R3)                                                    
         BE    *+12                                                             
         LA    R3,L'UDEFTAB(R3)                                                 
         B     VUDEF3                                                           
*                                                                               
         MVC   0(1,R1),7(R3)       UDEF TYPE CODE                               
         OC    SBEUDEF,8(R3)       SET UDEF EXTRACT FIELD                       
         SR    R5,R5                                                            
         ICM   R5,3,4(R3)                                                       
         CLC   FULL,=C'UCOM'                                                    
         BE    *+12                                                             
         LA    R5,SBLOCK(R5)                                                    
         B     *+6                                                              
         AR    R5,R9                                                            
         MVC   2(1,R1),0(R5)       MOVE UDEF DATA TYPE TO ARGS                  
*                                                                               
         CLI   0(R5),C'D'          TEST DATA TYPE = DATE                        
         BNE   *+8                                                              
         LA    RE,2(RE)            YES-ALLOW 2 MORE FOR COMPRESSED DATE         
*                                                                               
         SR    R0,R0                                                            
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BE    *+14                YES - USE MAX LENGTH                         
         OC    SBQBCLT,SBQBCLT     TEST ALL CLIENT REQUEST                      
         BNZ   *+12                                                             
         IC    R0,6(R3)            YES-USE MAX LENGTH                           
         B     VUDEF4                                                           
*                                                                               
         SR    R5,R5               NO-PICK UP MAX LENGTH FROM CLIENT            
         ICM   R5,3,2(R3)                                                       
         CLC   FULL,=C'UCOM'                                                    
         BE    *+12                                                             
         LA    R5,SBLOCK(R5)                                                    
         B     *+6                                                              
         AR    R5,R9                                                            
         IC    R0,0(R5)                                                         
*                                                                               
VUDEF4   STC   R0,1(R1)            WIDTH GOES INTO ARGS                         
         LA    R2,3(R2)            TOTAL L'ARGUMENTS SO FAR                     
         AR    RE,R0               TOTAL L'DRIVER INPUT FIELD SO FAR            
         CLM   R0,1,BYTE           COMPARE TO COLUMN WIDTH SO FAR               
         BNH   *+8                                                              
         STC   R0,BYTE             LOW-UPDATE COLUMN WIDTH                      
*                                                                               
         CLI   MYPOSO,0            IF HEAD OR MID, DONE                         
         BNE   VUDEF6                                                           
         AHI   RF,-2               TEST ANY MORE EXPRESSIONS                    
         BNP   VUDEF6                                                           
         CLI   2(R4),C'/'          YES-MUST BE SEPARATED BY /                   
         BNE   VUDEFNE                                                          
         LA    R1,3(R1)            VALIDATE NEXT                                
         LA    R4,3(R4)                                                         
         BCT   RF,VUDEF2                                                        
*                                                                               
VUDEF6   MVC   DRARGSO,DRARGSI     O/P ARGS = I/P ARGS                          
         STC   R2,DRNARGSI         LENGTH OF ARGUMENT LISTS                     
         STC   R2,DRNARGSO                                                      
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         LA    RE,1                                                             
         STC   RE,DRLENI           LENGTH FOR DRIVER INPUT ROUTINE              
         CLI   BYTE,0              TEST MAX WIDTH SET                           
         BNE   *+8                                                              
         MVI   BYTE,8              NO-DEFAULT TO 8                              
         MVC   DRLENO,BYTE         LENGTH FOR DRIVER OUTPUT ROUTINE             
         LA    R1,DRH1ARGS+1                                                    
         LA    RE,DRARGSI                                                       
         SR    RF,RF                                                            
*                                                                               
VUDEF7   CLI   0(RE),0                                                          
         BE    VUDEF8                                                           
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         LA    RE,3(RE)                                                         
         B     VUDEF7                                                           
*                                                                               
VUDEF8   STC   RF,DRH1ARGS         N'UDEF EXPRESSIONS                           
         LA    RF,1(RF)                                                         
         STC   RF,DRH1NARG         LENGTH OF HEADING ROUTINE ARGS               
         OI    DRH1OPTS,DRHALGNL   LEFT ALIGN HEADINGS                          
*                                                                               
VUDEFEQ  J     XITEQU                                                           
*                                                                               
VUDEFNE  J     XITNEQ                                                           
         SPACE 2                                                                
         DS    0H                                                               
UDEFTAB  DS    0CL9                                                             
         DC    CL2'P1',AL2(SBUP1LEN-SBLOCK),AL2(SBUP1TYP-SBLOCK)                
         DC    AL1(L'SBUP1FLD),X'01',AL1(SBEUPRD1)                              
         DC    CL2'P2',AL2(SBUP2LEN-SBLOCK),AL2(SBUP2TYP-SBLOCK)                
         DC    AL1(L'SBUP2FLD),X'02',AL1(SBEUPRD2)                              
         DC    CL2'E1',AL2(SBUE1LEN-SBLOCK),AL2(SBUE1TYP-SBLOCK)                
         DC    AL1(L'SBUE1FLD),X'03',AL1(SBEUEST1)                              
         DC    CL2'E2',AL2(SBUE2LEN-SBLOCK),AL2(SBUE2TYP-SBLOCK)                
         DC    AL1(L'SBUE2FLD),X'04',AL1(SBEUEST2)                              
         DC    X'00'                                                            
*                                                                               
         DS    0H                                                               
UCOMTAB  DS    0CL9                                                             
         DC    CL2'P1',AL2(UCDP1LEN-SYSD),AL2(UCDP1TYP-SYSD)                    
         DC    AL1(32),X'01',X'00'                                              
         DC    CL2'P2',AL2(UCDP2LEN-SYSD),AL2(UCDP2TYP-SYSD)                    
         DC    AL1(32),X'02',X'00'                                              
         DC    CL2'P3',AL2(UCDP3LEN-SYSD),AL2(UCDP3TYP-SYSD)                    
         DC    AL1(32),X'03',X'00'                                              
         DC    CL2'P4',AL2(UCDP4LEN-SYSD),AL2(UCDP4TYP-SYSD)                    
         DC    AL1(32),X'04',X'00'                                              
         DC    CL2'E1',AL2(UCDE1LEN-SYSD),AL2(UCDE1TYP-SYSD)                    
         DC    AL1(32),X'05',X'00'                                              
         DC    CL2'E2',AL2(UCDE2LEN-SYSD),AL2(UCDE2TYP-SYSD)                    
         DC    AL1(32),X'06',X'00'                                              
         DC    CL2'E3',AL2(UCDE3LEN-SYSD),AL2(UCDE3TYP-SYSD)                    
         DC    AL1(32),X'07',X'00'                                              
         DC    CL2'E4',AL2(UCDE4LEN-SYSD),AL2(UCDE4TYP-SYSD)                    
         DC    AL1(32),X'08',X'00'                                              
         DC    CL2'E5',AL2(UCDP1LEN-SYSD),AL2(UCDP1TYP-SYSD)                    
         DC    AL1(32),X'01',X'00'                                              
         DC    CL2'E6',AL2(UCDP2LEN-SYSD),AL2(UCDP2TYP-SYSD)                    
         DC    AL1(32),X'02',X'00'                                              
         DC    CL2'E7',AL2(UCDP3LEN-SYSD),AL2(UCDP3TYP-SYSD)                    
         DC    AL1(32),X'03',X'00'                                              
         DC    CL2'E8',AL2(UCDP4LEN-SYSD),AL2(UCDP4TYP-SYSD)                    
         DC    AL1(32),X'04',X'00'                                              
         DC    CL2'M1',AL2(UCDM1LEN-SYSD),AL2(UCDM1TYP-SYSD)                    
         DC    AL1(32),X'09',X'00'                                              
         DC    CL2'M2',AL2(UCDM2LEN-SYSD),AL2(UCDM2TYP-SYSD)                    
         DC    AL1(32),X'0A',X'00'                                              
         DC    CL2'M3',AL2(UCDM3LEN-SYSD),AL2(UCDM3TYP-SYSD)                    
         DC    AL1(32),X'0B',X'00'                                              
         DC    CL2'M4',AL2(UCDM4LEN-SYSD),AL2(UCDM4TYP-SYSD)                    
         DC    AL1(32),X'0C',X'00'                                              
         DC    X'00'                                                            
         EJECT                                                                  
* ERROR EXITS FROM ROW VALIDATION                                               
*                                                                               
         SPACE 1                                                                
BADROW   DS    0H                                                               
         MVC   GTMSGNO,=Y(ROWERR)                                               
         J     XITHI                                                            
*                                                                               
PERERR   DS    0H                                                               
         MVC   GTMSGNO,=Y(EPERLONG)                                             
         J     XITHI                                                            
*                                                                               
POSSUM   DS    0H                                                               
         MVC   GTMSGNO,=Y(POSERR)                                               
         J     XITHI                                                            
*                                                                               
DUPSUM   DS    0H                                                               
         MVC   GTMSGNO,=Y(DUPERR)                                               
         J     XITHI                                                            
*                                                                               
BADSUM   DS    0H                                                               
         MVC   GTMSGNO,=Y(SUMERR)                                               
         J     XITHI                                                            
*                                                                               
BADLRANK DS    0H                                                               
         MVC   GTMSGNO,=Y(LRANKERR)                                             
         J     XITLO                                                            
         EJECT                                                                  
EUCOM58  NTR1                                                                   
*                                                                               
         XC    ELEM,ELEM           CLEAR ELEM                                   
         LA    RF,ELEM             RF = ELEM                                    
         USING DDUCOMD,RF          UCOM DSECT                                   
         MVC   UCACOMF,ACOMFACS    PASS A(COMFACS)                              
         MVI   UCSYS,C'S'          SPOT SYSTEM                                  
         MVC   UCSAM,SBBAGYMD      A/M                                          
         MVC   UCSCLT,SBBCLT       CLIENT                                       
         MVI   UCOPT,UCOTTL+UCOMKT GET TITLES ONLY                              
         OI    UCOPT,UCO8EST       EXTRACT ESTIMATE UCOMM 5-8                   
*                                                                               
         GOTO1 VDDUCOM,ELEM        CALL DDUCOM                                  
         LA    RF,ELEM             A(OUTPUT)                                    
         CLI   UCERROR,0           ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DCHO                      YES - DEATH                                  
         TM    UCDATA,UCDNOCLT     HAVE CLIENT LEVEL UCOM REC?                  
         JNZ   XIT                 NO                                           
*                                                                               
         MVC   UCDP1LEN(4),UCPMXLNS  GET PRD UCOM LENGTHS                       
         MVC   UCDP1TYP(4),UCPEDITS  AND EDIT RULES                             
         J     XIT                                                              
         DROP  RF                                                               
*                                                                               
* ROUTINE TO VALIDATE A SUMMARY REPORT                                          
* LOOKS FOR PROGRAM RECORD WITH SUMMARY NAME                                    
* INPUT  : SUMNAME=SUMMARY NAME                                                 
* OUTPUT : CC EQ - REPORT EXISTS                                                
*          CC NE - REPORT NOT FOUND                                             
*                                                                               
VALSUM   NTR1  ,                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT01RECD,R4                                                      
         MVI   CT01TYPE,CT01TYPQ                                                
         MVC   CT01AGID,AGENCY                                                  
         MVC   CT01SYS(3),=X'020422'  SPWRI22=SUMMARY RECORD MAINT              
         MVC   CT01NAME,SUMNAME                                                 
         MVC   FILENAME,=CL8'CTFILE'                                            
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO3                                                         
         GOTO1 HIGH                                                             
         STC   R0,USEIO                                                         
         XC    FILENAME,FILENAME                                                
         CLC   KEY(L'CT01KEY),KEYSAVE                                           
         JNE   XITNEQ                                                           
         J     XITEQU                                                           
         EJECT                                                                  
* VALIDATE A COLUMN                                                             
*                                                                               
VALCOL   MVI   FIELDERR,1                                                       
         CLI   COLXTEND,2          TEST HANDLING EXTENSION HERE                 
         BNE   VCOLA                                                            
         CLI   5(R2),0                                                          
         BE    VCOL50                                                           
         LA    R4,BLOCK+42+42      DON'T DISTURB FIRST 2 ENTRIES FOR            
         B     VCOL1               EXTEND - COMPUTES MAY BE THERE               
*                                                                               
VCOLA    XC    PERLIST,PERLIST     CLEAR PERIOD NUMBERS                         
*                                                                               
VCOL0    CLI   5(R2),0                                                          
         BE    VCOLX                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP OFFLINE                                 
         BE    VCOL0C                                                           
         CLI   DDS,C'Y'            DDS?                                         
         BE    VCOL0C                                                           
         CLI   WLPROF,C'Y'         SECURITY AGENCY?                             
         BNE   VCOL0C                                                           
*                                                                               
VCOL0B   TM    AUTH,X'40'          AUTHORIZED?                                  
         BNZ   VCOL0C                                                           
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VCOL0C                                                           
         MVI   ERROR,SECLOCK                                                    
         J     TRAPERR                                                          
*                                                                               
VCOL0C   ST    R2,ALASTCOL                                                      
         XC    BLOCK(252),BLOCK                                                 
         LA    R4,BLOCK+42                                                      
         MVI   COLXTEND,0                                                       
         LLC   R1,5(R2)                                                         
         LA    R1,8-1(R1,R2)       (R1=A(LAST CHARACTER))                       
         CLI   0(R1),C','          IF THIS IS A COMMA                           
         BNE   VCOL1                                                            
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,5(R2)            REDUCE APPARENT LENGTH                       
         MVI   COLXTEND,1          AND NOTE THAT THERE IS AN EXTENSION          
*                                                                               
VCOL1    GOTO1 SCANNER,DMCB,(20,(R2)),(6,(R4)),0                                
         LLC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADCOL                                                           
         CLI   COLXTEND,2          TEST THIS IS AN EXTENSION                    
         BE    VCOL12                                                           
*                                                                               
         CLC   12(6,R4),=C'DEMNDX'                                              
         BE    VCOL3                                                            
         CLC   12(6,R4),=C'DOLNDX'                                              
         BE    VCOL3                                                            
         CLC   12(3,R4),=C'DEM'                                                 
         BE    VCOL2                                                            
         CLC   12(3,R4),=C'CPP'                                                 
         BE    VCOL2                                                            
         CLC   12(3,R4),=C'DOL'                                                 
         BNE   VCOL3                                                            
*                                                                               
VCOL2    BAS   RE,GENERIC        VALIDATE GENERIC COLUMN EXPRESSION             
         BNE   BADCOL                                                           
*                                                                               
VCOL3    BRAS  RE,VCOLDRON         VALIDATE A COLUMN ENTRY                      
         BNE   VCOL6                                                            
*                                                                               
         CLC   =C'BYP',12(R4)    PURCHASED KEYWORD?                             
         BE    *+10              YES                                            
         CLC   =C'BYR',12(R4)    RERATE KEYWORD?                                
         BE    *+14              YES                                            
         CLC   =C'BYA',12(R4)    AFFID KEYWORD?                                 
         BNE   *+8               NO                                             
         OI    COLIND3,COLIDEML  HAVE DEMO LOOKUPS                              
*                                                                               
         CLC   =C'UDEM',12(R4)   UDEM1-8 KEYWORDS?                              
         BNE   *+8               NO                                             
         OI    COLIND3,COLIUNIV  HAVE UNIVERSE LOOKUPS                          
*                                                                               
         CLC   =C'INVIMP',12(R4) INVOICE IMPRESSIONS KEYWORD?                   
         BNE   VCOL3AA           NO                                             
         TM    SBEFLAG9,SBE92DEC 2-DECIMAL IMPRESSIONS?                         
         BZ    VCOL3AA           NO - LEAVE DEFAULT OF 1 DECIMAL PLACE          
         MVI   DRDECO,2          YES - SET TO 2 DECIMALS                        
*                                                                               
VCOL3AA  CLC   12(5,R4),=C'TEXT '  MAYBE TEXT                                   
         BNE   *+12                                                             
         BAS   RE,GENTEXT                                                       
         B     VCOL3A                                                           
*                                                                               
         CLC   12(2,R4),=C'ST'     TEST COLUMN IS A STACK                       
         BNE   *+16                                                             
         CLI   STACKDEF,0          YES-THERE MUST BE A STACK DEFINITION         
         BE    BADCOL                                                           
         B     VCOL3A                                                           
*                                                                               
         CLC   12(4,R4),=C'UCOM'   TEST UCOM                                    
         BE    *+14                                                             
         CLC   12(4,R4),=C'UDEF'   TEST UDEF                                    
         BNE   *+16                                                             
         BAS   RE,VALUDEF          YES-VALIDATE                                 
         BNE   BADCOL                                                           
         B     VCOL3A                                                           
*                                                                               
         CLC   12(7,R4),=C'BILLST ' TEST COL IS BILLST                          
         BNE   *+12                                                             
         OI    COLIND3,COLIBLST    SET BILLST IS COL                            
         B     VCOL3A                                                           
*                                                                               
         CLC   12(7,R4),=C'CSPOTS ' TEST COL IS CSPOTS                          
         BNE   *+12                 NO                                          
         OI    COLIND3,COLICBAG                                                 
         B     VCOL3A                                                           
*                                                                               
         LA    RE,SBAGYREC         AFFID CHECKS FOR CANADIAN NETWORK            
         CLI   AGYPROF+7-AGYHDRD(RE),C'C'                                       
         BNE   VCOL3A                                                           
         CLI   SBQMED,C'N'                                                      
         BNE   VCOL3A                                                           
         CLC   12(3,R4),=C'GST'    GST/PST COLS CANNOT REPORT FROM              
         BE    *+14                NETWORK LEVEL BUYS - NEED LOCAL              
         CLC   12(3,R4),=C'PST'    MKT TO DETERMINE GST/HST/PST RATES           
         BNE   *+12                                                             
         TM    OPTIND6,OPT6NM0B    IF READING NETWORK ONLY BUYS                 
         BNZ   BADCOL4             BLOCK ALL GST/PST DATA                       
*                                                                               
         TM    OPTIND6,OPT6NM0B    FOLLOWING MUST USE NWK ONLY BUYS             
         BNZ   VCOL3A              (OPTION NOT SET SO OK)                       
         CLC   12(3,R4),=C'ADA'    ADAY/ADATW/ADATED/ADATER                     
         BE    BADCOL3                                                          
         CLC   12(3,R4),=C'ATI'    ATIME/ATIMES/ATIMEM                          
         BE    BADCOL3                                                          
         CLC   12(4,R4),=C'MCAF'   MCAFFID                                      
         BE    BADCOL3                                                          
*                                                                               
VCOL3A   CLI   DRATTRIB+1,C'D'     TEST FOR DOLLAR COLUMN                       
         BNE   VCOL4                                                            
         TM    COLIND,COLIRND      YES-TEST FOR ROUNDING OPTION                 
         BZ    *+12                                                             
         MVI   DRDECO,0            YES-NO DECIMAL PLACES (PENNIES               
         B     VCOL3B                  ROUNDED OUT ON INPUT)                    
         TM    COLIND2,COLINOCT    TEST NOCENTS OPTION                          
         BZ    VCOL3B                                                           
         MVI   DRDECO,0            YES-ROUND OUT PENNIES ON OUTPUT              
         MVI   DRDIVO,2                                                         
*                                                                               
VCOL3B   TM    OPTIND3,OPTIFLEM    TEST FLOAT=-                                 
         BZ    VCOL5                                                            
         TM    OPTIND7,OPT7NFLT    NOFLOAT OPTION?                              
         BNZ   VCOL5               YES - LEAVE MINUS=YES ALONE                  
         MVI   DRFLOATO,C'-'       YES                                          
         NI    DROPTSO,255-DRMINUSO    TURN OFF MINUS=YES                       
         B     VCOL5                                                            
*                                                                               
VCOL4    CLI   DRATTRIB+1,C'R'     TEST FOR RATINGS COLUMN                      
         BNE   VCOL5                                                            
         MVI   DRDECO,0            YES-NO DECIMALS                              
         TM    COLIND,COLINDR      UNLESS DEMROUND=N                            
         BZ    VCOL5                                                            
         MVI   DRDECO,1                                                         
         TM    SBEFLAG4,SBE42DEC   UNLESS DEMROUND=2                            
         BZ    VCOL5                                                            
         MVI   DRDECO,2                                                         
*                                                                               
VCOL5    CLI   DRATTRIB+1,C'W'     TEST FOR 'WIDE' DOWNLOAD COL                 
         BNE   *+16                 NO                                          
         TM    DOWNOPT,GLDLACTV    ARE WE DOWNLOADING?                          
         BZ    *+8                  NO                                          
         OI    COLIND3,COLIBIGD    SET WIDE DL COLUMN                           
*                                                                               
***      CLC   =C'BYSCPP',12(R4)   SKIP IF NOT SQAD KEYWORD                     
         CLC   =C'BYSC',12(R4)     SKIP IF NOT SQAD KEYWORD                     
         BNE   VCOLSQDX                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DRARGSI+3      GET SQAD OPTION NUMBER                       
         BZ    *+6                                                              
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MHI   RF,SBSQDOPL         CALCULATE INDEX FOR OPTION                   
         CHI   RF,8                SQAD 3/4 OPTION?                             
         BL    VCOLSQD1            NO                                           
         SHI   RF,8                                                             
         LA    RF,SBSQDQT3(RF)     POINT TO CORRECT SQAD OPTION                 
         B     VCOLSQD2                                                         
*                                                                               
VCOLSQD1 LA    RF,SBSQDQT1(RF)     POINT TO CORRECT SQAD OPTION                 
*                                                                               
VCOLSQD2 OC    0(SBSQDOPL,RF),0(RF)   OPTION MUST BE AVAILABLE                  
         BZ    SQDNOOPE                                                         
*                                                                               
VCOLSQDX LA    RE,L'DRARGSI                                                     
         LLC   RF,DRNARGSI                                                      
         SR    RE,RF                                                            
         BNP   VCOLNXT                                                          
         BCTR  RE,0                                                             
         LA    R1,DRARGSI(RF)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
         MVC   DRARGSI+6(1),COLNUM  INDEX MUST BE PRESENT                       
**         MVC   DRH1ARGS+6(1),COLNUM  INDEX MUST BE PRESENT                    
**         MVI   DRH1NARG,7           LENGTH OF HEADING ROUTINE ARGS            
         CLC   12(8,R4),=C'CLT$BILT'  BILL TODAY KEYWORD?                       
         BE    *+14                                                             
         CLC   12(8,R4),=C'BILLTDY '  BILL TODAY KEYWORD?                       
         BNE   *+8                                                              
         BAS   RE,BILLTDY                                                       
         B     VCOLNXT                                                          
*                                                                               
VCOL6    XC    BLOCK(42),BLOCK     MAY BE A COMPUTE EXPRESSION                  
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),BLNKS                                               
         MVC   BLOCK+12(7),=C'COMPUTE'                                          
         LA    R4,BLOCK                                                         
         BRAS  RE,VCOLDRON         VALIDATE THE COMPUTE COLUMN                  
         BNE   BADCOL                                                           
         LA    R4,BLOCK+42                                                      
         CLI   OFFLINE,C'Y'                                                     
         BE    VCOL8                                                            
         BRAS  RE,VCMPDRON         VALIDATE A COMPUTE EXPRESSION                
         BNE   BADCOL                                                           
*                                                                               
VCOL8    BAS   RE,COMPEDIT         AUTO EDIT FOR COMPUTES                       
         BE    VCOLNXT                                                          
         CLI   OFFLINE,C'Y'        ONLY FLAG ERROR IF OFFLINE                   
         BE    BADCOL              (EDITLIST CAN ONLY BE USED OFFLINE)          
         B     VCOLNXT                                                          
         EJECT                                                                  
* CHECK FOR SUBSIDIARY COLUMN EXPRESSIONS                                       
*                                                                               
VCOL12   LLC   R5,0(R4)            R5=LENGTH FOR EXECUTED COMPARE               
         BCTR  R5,0                                                             
         TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VCOL14                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VCOLNXT                                                          
*                                                                               
*                                  CHECK FOR PERIOD EXPRESSION                  
VCOL14   GOTO1 VALPEX,DMCB,12(R4)                                               
         BNE   VCOL19                                                           
         BRAS  RE,GETEXTC                                                       
         BNE   VCOL15                                                           
         USING EXTCOLSD,RF                                                      
         OC    EXTPD,EXTPD         IS THERE A PERIOD                            
         BNZ   VCOL18               YES                                         
         DROP  RF                                                               
*                                                                               
VCOL15   SR    R1,R1                                                            
         ICM   R1,1,PERLIST                                                     
         BZ    VCOL19                                                           
         CLC   BLOCK+12(7),=C'COMPUTE'   INVALID FOR COMPUTES                   
         BE    VCOL19                                                           
         CLI   PERLIST,X'FF'       TEST INVALID EXPRESSION                      
         BE    VCOL18                                                           
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         CLI   12(R4),C'Y'                                                      
         BNE   *+16                                                             
         OI    SBQPER,SBQPYR                                                    
         L     RE,AYEARS                                                        
         B     VCOL16                                                           
         CLC   12(2,R4),=C'HY'                                                  
         BNE   *+16                                                             
         OI    SBQPER,SBQPHY                                                    
         L     RE,AHYEARS                                                       
         B     VCOL16                                                           
         CLI   12(R4),C'Q'                                                      
         BNE   *+16                                                             
         OI    SBQPER,SBQPQT                                                    
         L     RE,AQTRS                                                         
         B     VCOL16                                                           
         CLI   12(R4),C'M'                                                      
         BNE   *+16                                                             
         OI    SBQPER,SBQPMN                                                    
         L     RE,AMONTHS                                                       
         B     VCOL16                                                           
         CLI   12(R4),C'W'                                                      
         BNE   *+16                                                             
         OI    SBQPER,SBQPWK                                                    
         L     RE,AWEEKS                                                        
         B     VCOL16                                                           
         CLI   12(R4),C'D'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    SBQPER,SBQPDY                                                    
         L     RE,ADAYS                                                         
*                                                                               
VCOL16   LA    RE,0(R1,RE)         A(PERIOD S/E) BECOMES INPUT ARGS             
         STCM  RE,15,DRARGSI+12                                                 
         MVC   DRARGSI+12(1),12(R4)  ALSO PASS THE PER TYPE D/W/M/Q/Y/H         
         B     VCOLNXT                                                          
*                                                                               
VCOL18   DS    0H                  INVALID PERIOD EXPRESSION                    
         MVC   GTMSGNO,=Y(PEXERR)                                               
         J     XITNEQ                                                           
*                                                                               
VCOL19   LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VCOL20                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VCOL24                                                           
*                                                                               
         USING DRHEADD,R1                                                       
VCOL20   CLC   =C'UCOM',BLOCK+54   DON'T ALLOW HEAD OVERRIDES FOR UCOM          
         BE    *+14                                                             
         CLC   =C'UDEF',BLOCK+54   OR UDEF                                      
         BNE   *+14                                                             
         MVC   GTMSGNO,=Y(ENOHOV)                                               
         J     XITNEQ                                                           
*                                                                               
         XC    0(64,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),2                                                          
         BL    VCOLNXT             HN=X CAUSES REMOVAL                          
         OI    DRHEAD,X'80'        OTHERWISE TURN IT BACK ON                    
         CLI   13(R4),C' '         TEST HEADLINE 1                              
         BE    *+12                                                             
         CLI   13(R4),C'1'                                                      
         BNE   *+10                                                             
         MVC   DRHARGS(8),=C'OVERRIDE' YES-INDICATE IT'S AN OVERRIDE            
         MVC   DRHLITL,1(R4)       PASS LITERAL LENGTH TO DRONE                 
         CLC   DRHLITL,DRLENO                                                   
         BNH   VCOL22              CHECK LITERAL NOT WIDER THAN COLUMN          
         MVC   GTMSGNO,=Y(HOVERR)                                               
         J     XITNEQ                                                           
*                                                                               
VCOL22   LLC   RE,DRHLITL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE IN THE HEADER LITERAL                   
         B     VCOLNXT                                                          
         MVC   DRHLIT(0),22(R4)    ** EXECUTED                                  
         DROP  R1                                                               
*                                                                               
VCOL24   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VCOL26                                                           
         MVI   MYPOSO,C'N'                                                      
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         MVC   DRH1LIT,BLNKS                                                    
         MVC   DRH2LIT,BLNKS                                                    
         MVC   DRH3LIT,BLNKS                                                    
         MVC   DRH4LIT,BLNKS                                                    
         B     VCOLNXT                                                          
*                                                                               
VCOL26   CLC   12(3,R4),=C'NT '    OPTION NOT TO TOTAL THIS COLUMN              
         BNE   VCOL26A                                                          
******** OI    DRARGSI+11,X'80'    12TH ARGUMENT X'80'                          
         OI    DROPTSO+1,DRNOTOT                                                
         B     VCOLNXT                                                          
*                                                                               
VCOL26A  CLC   12(3,R4),=C'ND '    OPTION FOR NO DETAILS THIS COLUMN            
         BNE   VCOL27                                                           
         OI    DROPTSO+1,DRNODET                                                
         B     VCOLNXT                                                          
*                                                                               
VCOL27   CLC   12(4,R4),=C'WGT '   WEIGHTED DEMO OPTION                         
         BNE   VCOL28                                                           
         LA    R1,DRARGSI          INDICATE IN INPUT, OUTPUT AND                
         USING GLARGSD,R1          HEADLINE ROUTINE ARGUMENTS                   
         OI    GLIIND,GLIIWGT                                                   
         LA    R1,DRARGSO                                                       
         OI    GLOIND,GLOIWGT                                                   
         MVI   DRNARGSO,16                                                      
         LA    R1,DRH1ARGS                                                      
         OI    GLHIND,GLHIWGT                                                   
         MVI   DRH1NARG,16                                                      
         B     VCOLNXT                                                          
         DROP  R1                                                               
*                                                                               
VCOL28   CLC   12(5,R4),=C'THOU '  THOUSAND OPTION                              
         BNE   VCOL29                                                           
         MVI   DRDIVO,3                                                         
         B     VCOLNXT                                                          
*                                                                               
VCOL29   CLC   12(4,R4),=C'DIV '   DIVIDE OPTION                                
         BNE   VCOL30                                                           
         B     VCOL30              ***** REMOVE THIS OPTION *****               
         TM    3(R4),X'80'                                                      
         BZ    BADCOL                                                           
         MVI   DRDIVO,1                                                         
         CLC   8(4,R4),=F'10'                                                   
         BE    VCOLNXT                                                          
         MVI   DRDIVO,2                                                         
         CLC   8(4,R4),=F'100'                                                  
         BE    VCOLNXT                                                          
         MVI   DRDIVO,3                                                         
         CLC   8(4,R4),=F'1000'                                                 
         BE    VCOLNXT                                                          
         B     BADCOL                                                           
*                                                                               
VCOL30   CLC   12(8,R4),=C'BRACKET '   (MINUS NUMBERS)                          
         BNE   VCOL31                                                           
         OI    DROPTSO,DRBKMINO                                                 
         NI    DROPTSO,255-DRMINUSO    TURN OFF MINUS=YES                       
         B     VCOLNXT                                                          
*                                                                               
VCOL31   CLC   12(8,R4),=C'NOBLANK '   ZERO=NOBLANK                             
         BNE   VCOL32                                                           
         OI    DROPTSO,DRZEROO                                                  
         B     VCOLNXT                                                          
*                                                                               
VCOL32   CLC   12(4,R4),=C'DEC '   DECIMAL PLACES                               
         BNE   VCOL34                                                           
         CLC   BLOCK+12(7),=C'COMPUTE'  ONLY VALID FOR PERCENT-TYPE             
         BNE   VCOL34                   COMPUTES                                
         CLI   DRTRAILO,C'%'                                                    
         BNE   VCOL34                                                           
         TM    3(R4),X'80'                                                      
         BZ    BADCOL                                                           
         OC    8(4,R4),8(R4)       MUST BE DEC=0                                
         BNZ   BADCOL                                                           
         MVI   DRDECO,0                                                         
         MVI   DRDIVO,2                                                         
         B     VCOLNXT                                                          
*                                                                               
VCOL34   CLC   12(2,R4),=C'U '     USER RECORD                                  
         BNE   VCOL36                                                           
         BRAS  RE,VUSRDRON                                                      
         JNE   XITNEQ                                                           
         CLI   DRH1LITL,0          TEST HEADLINE 1 LITERAL                      
         BE    VCOLNXT                                                          
         CLC   DRH1RTN,BLNKS       AND ROUTINE PICKED UP FROM ENTRY             
         BNH   VCOLNXT                                                          
         XC    DRH1RTN,DRH1RTN     YES-GET RID OF THE ROUTINE                   
         MVI   DRH1NARG,0                                                       
         XC    DRH1ARGS,DRH1ARGS                                                
         B     VCOLNXT                                                          
*                                                                               
VCOL36   CLC   12(4,R4),=C'RANK'   COLUMN RANK                                  
         BNE   VCOL38                                                           
         MVI   COLRANK,1                                                        
         CLI   16(R4),C'-'                                                      
         BE    VCOLNXT                                                          
         CLI   22(R4),C'-'                                                      
         BE    VCOLNXT                                                          
         MVI   COLRANK,2                                                        
         B     VCOLNXT                                                          
*                                                                               
VCOL38   CLC   12(5,R4),=C'CUME '  CUME                                         
         BNE   VCOL39                                                           
         OI    DROPTSO+1,DRCUME    FOR DRIVER CUME (ONLY ONE COL)               
         B     VCOLNXT                                                          
*                                                                               
VCOL39   CLC   12(4,R4),=C'CME '   SPECIAL CUME FOR CHILD SPOT                  
         BNE   VCOL40                                                           
         LA    R1,DRARGSO                                                       
         USING GLARGSD,R1                                                       
         OI    GLOIND,GLOICUME     PASS INDICATOR TO OUTPUT ARGS                
         MVI   DRNARGSO,16                                                      
         B     VCOLNXT                                                          
*                                                                               
VCOL40   MVI   BYTE,GLIIMAT                                                     
         CLC   12(4,R4),=C'MAT '   MATCHED FILTER                               
         BE    *+18                                                             
         MVI   BYTE,GLIIUNM                                                     
         CLC   12(4,R4),=C'UNM '   UNMATCHED FILTER                             
         BNE   VCOL42                                                           
         LA    R1,DRARGSI                                                       
         OC    GLIIND,BYTE         PASS INDICATOR TO INPUT ARGS                 
         MVI   DRNARGSO,16                                                      
         OI    COLIND2,COLIMAT     INDICATE MATCHED/UNMATCHED FILTER            
*                                                                               
         LA    R1,SBAGYREC         SPECIAL CHECK FOR CANADIAN NETWORK           
         CLI   AGYPROF+7-AGYHDRD(R1),C'C'                                       
         BNE   VCOLNXT                                                          
         CLI   DRATTRIB+1,C'D'     DOLLAR COLUMN                                
         BNE   VCOLNXT                                                          
         CLI   SBQMED,C'N'         NETWORK                                      
         BNE   VCOLNXT                                                          
         TM    OPTIND6,OPT6NM0B    NEEDS COST FROM NWK BUYS                     
         BNZ   VCOLNXT                                                          
         B     BADCOL3                                                          
*                                                                               
VCOL42   CLC   12(2,R4),=C'I '     INPUT AND OUTPUT GST                         
         BE    VCOL43                                                           
         CLC   12(2,R4),=C'O '                                                  
         BE    VCOL43                                                           
         CLC   12(3,R4),=C'IP '    INPUT AND OUTPUT GST&PST                     
         BE    VCOL43                                                           
         CLC   12(3,R4),=C'OP '                                                 
         BNE   VCOL44                                                           
VCOL43   CLI   DRATTRIB+1,C'D'     MUST BE DOLLAR COLUMN                        
         BNE   BADCOL                                                           
         LA    R1,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDRD(R1),C'C'    MUST BE CANADIAN AGENCY            
         BNE   BADCOL                                                           
         TM    OPTIND6,OPT6NM0B    NO GST/PST IF COST FROM NWK BUYS             
         BNZ   BADCOL4                                                          
         MVI   BYTE,GLIIGSTI                                                    
         LA    RF,SBEGSTI                                                       
         LA    RE,DRATTRIB+2       ATTRIB+2=I - INPUT GST OK                    
         CLI   13(R4),C'P'         INCLUDING PST?                               
         BNE   *+12                                                             
         MVI   BYTE,GLIIGSTI+GLIIPST                                            
         LHI   RF,SBEGSTI+SBEPSTI                                               
         CLI   12(R4),C'I'                                                      
         BE    VCOL43B                                                          
         MVI   BYTE,GLIIGSTO                                                    
         LA    RF,SBEGSTO                                                       
         LA    RE,DRATTRIB+3       ATTRIB+3=O - OUTPUT GST OK                   
         CLI   13(R4),C'P'         INCLUDING PST?                               
         BNE   *+12                                                             
         MVI   BYTE,GLIIGSTO+GLIIPST                                            
         LHI   RF,SBEGSTO+SBEPSTO                                               
VCOL43B  CLC   12(1,R4),0(RE)                                                   
         BNE   BADCOL                                                           
         LA    R1,DRARGSI                                                       
         OC    GLIIND,BYTE         INDICATE IN INPUT ARGS                       
         TM    GLIIND,GLIIGSTI+GLIIGSTO                                         
         BO    BADCOL              CAN'T HAVE BOTH INPUT & OUTPUT               
         STC   RF,BYTE                                                          
         OC    SBEGST,BYTE         SET GST EXTRACT OPTION                       
         CLI   DRARGSI,C'S'        TEST BILLED OR BILLABLE COLUMN               
         BE    *+12                                                             
         CLI   DRARGSI+1,C'S'                                                   
         BNE   VCOLNXT                                                          
         OI    SBEGST,SBEGSTB      YES-SET TO EXTRACT BILLED GST                
         CLI   13(R4),C'P'         INCLUDING PST?                               
         BNE   *+8                                                              
         OI    SBEGST,SBEPSTB      YES-SET TO EXTRACT BILLED PST                
         B     VCOLNXT                                                          
         DROP  R1                                                               
*                                                                               
VCOL44   CLC   12(2,R4),=C'R '     RIGHT ALIGN FOR DOWNLOAD                     
         BNE   VCOL46                                                           
         OI    DROPTSO,DRALGNRO                                                 
         B     VCOLNXT                                                          
*                                                                               
VCOL46   CLC   12(4,R4),=C'ABS '   ABSOLUTE VALUE FUNCTION?                     
         BNE   VCOL48                                                           
         CLC   BLOCK+12(7),=C'COMPUTE'  ONLY VALID FOR PERCENT-TYPE             
         BNE   BADCOL                   COMPUTES                                
         OI    DROPTSO+1,DROPABSO                                               
         B     VCOLNXT                                                          
*                                                                               
VCOL48   B     VCOLFILT                                                         
         SPACE 2                                                                
COLRANK  DC    X'00'                                                            
         EJECT                                                                  
* SUPPORT COLUMN FILTERS                                                        
*                                                                               
* ARGUMENTS:    8=PACKED STATION, OR PRODUCT                                    
*              11=DAYPART                                                       
*              12=INDICATOR                                                     
*                            X'80'=DAYPART IS REALLY ADJ CODE                   
*                            X'40'=WEIGHTED DEMO                                
*      *** UNUSED ***        X'20'=GLARGS+6 HAS SPOTLEN FILTER                  
*                            X'10'=MATCHED,X'08'=UNMATCHED                      
*                            X'04'=INCLUDE INPUT GST                            
*                            X'02'=INCLUDE OUTPUT GST                           
*                            X'01'=PRODUCT FILTER PRESENT                       
*              13-16=A(PERIOD S/E)                                              
*                                                                               
VCOLFILT EX    R5,VCOLFDPT                                                      
         BE    *+12                                                             
         EX    R5,VCOLFMAS                                                      
         BNE   VCOLF1                                                           
         CLI   1(R4),1                                                          
         BNE   BADCOL                                                           
         TM    DRARGSI+11,GLIIADJ  SOMETHING THERE ALREADY?                     
         BNZ   BADCOL2                                                          
         CLI   DRARGSI+10,0                                                     
         BNE   BADCOL                                                           
         MVC   DRARGSI+10(1),22(R4)                                             
         EX    R5,VCOLFMAS                                                      
         BNE   VCOLNXT                                                          
         NI    DRARGSI+10,X'FF'-X'40'    LOWER CASE TO INDICATE MASDPT          
         B     VCOLNXT                                                          
*                                                                               
VCOLF1   EX    R5,VCOLFADJ                                                      
         BNE   VCOLF2                                                           
         CLI   DRARGSI+10,0        SOMETHING THERE ALREADY?                     
         BE    *+16                 NO                                          
         TM    DRARGSI+11,GLIIADJ  ANOTHER ADJ?                                 
         BNZ   BADCOL               YES                                         
         B     BADCOL2              NO - MUST BE DPT FILT                       
*                                                                               
         MVC   DRARGSI+10(1),22(R4)                                             
         TM    3(R4),X'40'         VALID ALPHA?                                 
         BZ    *+12                 NO                                          
         CLI   1(R4),1             ONE CHARACTER?                               
         BE    VCOLF1A              YES - IT'S VALID                            
*                                                                               
         CLI   1(R4),2             2 CHARACTERS?                                
         BNE   BADCOL               NO                                          
         TM    3(R4),X'80'         VALID NUMERIC?                               
         BZ    BADCOL                                                           
         PACK  DRARGSI+10(1),22(1,R4)                                           
         NI    DRARGSI+10,X'F0'                                                 
         MVC   BYTE,23(R4)                                                      
         NI    BYTE,X'0F'                                                       
         OC    DRARGSI+10(1),BYTE                                               
*                                                                               
VCOLF1A  OI    DRARGSI+11,GLIIADJ                                               
         B     VCOLNXT                                                          
*                                                                               
VCOLF2   EX    R5,VCOLFSTA                                                      
         BNE   VCOLF6                                                           
         LLC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         CLI   1(R4),7                                                          
         BH    BADCOL                                                           
         CLI   1(R4),3                                                          
         BL    BADCOL                                                           
         MVC   DUB,BLNKS                                                        
         MVI   DUB+4,C'T'                                                       
         CLI   1(R4),4                                                          
         BNH   VCOLF4                                                           
         CLI   25(R4),C'-'                                                      
         BNE   *+16                                                             
         LA    RE,4                                                             
         MVI   25(R4),C' '                                                      
         B     VCOLF4                                                           
         CLI   1(R4),5                                                          
         BE    VCOLF4                                                           
         CLI   26(R4),C'-'                                                      
         BNE   BADCOL                                                           
         LA    RE,4                                                             
         MVC   26(1,R4),27(R4)                                                  
*                                                                               
VCOLF4   EX    RE,VCOLFMVC                                                      
         MVC   SBMED,SBQMED        SET MEDIA (FOR MSPACK - CANADA)              
         MVC   FULL,=C'0001'                                                    
         GOTO1 MSPACK,DMCB,FULL,(X'80',DUB),WORK                                
         BM    BADCOL                                                           
         MVC   DRARGSI+7(3),WORK+2                                              
         B     VCOLNXT                                                          
*                                                                               
VCOLF6   EX    R5,VCOLFPRD         PRODUCT FILTER                               
         BNE   VCOLF8                                                           
         CLI   1(R4),2                                                          
         BL    BADCOL                                                           
         CLI   1(R4),3                                                          
         BH    BADCOL                                                           
         MVC   DRARGSI+7(3),22(R4)                                              
         OI    DRARGSI+11,GLIIPRD                                               
         CLI   1(R4),2                                                          
         BNE   VCOLNXT                                                          
         MVI   DRARGSI+9,C' '                                                   
         B     VCOLNXT                                                          
*                                                                               
VCOLF8   DS    0H                                                               
         EX    R5,VCOLFLEN                                                      
         BNE   VCOLF10                                                          
         TM    3(R4),X'80'         VALID NUMERIC                                
         BZ    BADCOL                                                           
         OC    8(3,R4),8(R4)       MAKE SURE L.T. 255                           
         BNZ   BADCOL                                                           
         BRAS  RE,GETEXTC                                                       
         BNE   VCOLF9                                                           
         USING EXTCOLSD,RF                                                      
         MVC   EXTSLEN,11(R4)                                                   
         DROP  RF                                                               
VCOLF9   OI    DATAIND4,DISLN      EXTRACT SPOT LENGTH                          
         B     VCOLNXT                                                          
*                                                                               
VCOLF10  DS    0H                                                               
         EX    R5,VCOLFORG                                                      
         BNE   VCOLF12                                                          
         BRAS  RE,GETEXTC                                                       
         BNE   VCOLNXT                                                          
         USING EXTCOLSD,RF                                                      
         OI    EXTFLAG1,EXTORIG                                                 
         DROP  RF                                                               
         B     VCOLNXT                                                          
*                                                                               
VCOLF12  DS    0H                                                               
         EX    R5,VCOLFSPL                                                      
         BNE   VCOLF14                                                          
         BRAS  RE,GETEXTC                                                       
         BNE   VCOLNXT                                                          
         USING EXTCOLSD,RF                                                      
         OI    EXTFLAG1,EXTSPIL                                                 
         DROP  RF                                                               
         B     VCOLNXT                                                          
*                                                                               
VCOLF14  DS    0H                                                               
         EX    R5,VCOLFSRP         SPECIAL REP                                  
         BNE   VCOLF16                                                          
         CLI   1(R4),3                                                          
         BH    BADCOL                                                           
         TM    3(R4),X'80'                                                      
         BZ    BADCOL                                                           
         BRAS  RE,GETEXTC                                                       
         BNE   VCOLNXT                                                          
         USING EXTCOLSD,RF                                                      
         L     RE,8(R4)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EXTSREP,DUB                                                      
         DROP  RF                                                               
         B     VCOLNXT                                                          
*                                                                               
VCOLF16  DS    0H                                                               
         EX    R5,VCOLFPD          PERIOD                                       
         BNE   VCOLF22                                                          
         OC    DRARGSI+12(4),DRARGSI+12  NOT IF ALREADY A PERIOD                
         BNZ   BADCOL                                                           
         OI    SBQPER,SBQPDY                                                    
         ST    R2,FULL                                                          
         LA    R2,22(R4)                                                        
         CLI   0(R2),C'+'                                                       
         BE    *+12                                                             
         CLI   0(R2),C'-'                                                       
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         MVC   ELEM+8(20),0(R2)   FAKE TWA FLD                                  
         LA    R2,ELEM                                                          
         XC    ERROR,ERROR                                                      
         MVI   ERROPT,C'Y'         COME BACK TO ME ON ERROR                     
****     GOTO1 VALIPER,DMCB,(0,WORK)                                            
         GOTO1 PERVAL,DMCB,(1(R4),8(R2)),(0,WORK)                               
         TM    DMCB+4,X'03'        INVALID/NO INPUT?                            
         BNZ   VCOLF16H            YES - SO ERROR                               
*                                                                               
         LA    R1,WORK                                                          
         USING PERVALD,R1                                                       
*                                                                               
         CLI   PVALASSM,X'00'      BOTH FULL DATES ENTERED?                     
         BE    VCOLF17             YES - SO CONTINUE                            
*                                                                               
         CLI   PVALASSM,X'70'      FULL START DATE ONLY ENTERED?                
         BE    VCOLF17             YES - SO CONTINUE                            
*                                                                               
VCOLF16H EQU   *                                                                
*                                                                               
         MVI   ERROR,INVDATE       ELSE - SET ERROR CODE                        
*                                                                               
VCOLF17  EQU   *                                                                
*                                                                               
         L     R2,FULL                                                          
         MVI   ERROPT,0                                                         
         OC    ERROR,ERROR                                                      
         BNZ   BADCOL                                                           
****     GOTO1 DATCON,DMCB,(0,WORK),(2,ELEM)                                    
****     GOTO1 (RF),(R1),(0,WORK+6),(2,ELEM+2)                                  
         MVC   ELEM(4),PVALCSTA    MOVE S/E DATES                               
         DROP  R1                                                               
*                                                                               
         CLI   22(R4),C'+'         IF + OR - DATES, MUST BE SINGLE DATE         
         BNE   VCOLF18                                                          
         CLC   ELEM(2),ELEM+2                                                   
         BNE   BADCOL                                                           
         XC    ELEM+2(2),ELEM+2    ZERO OUT END DATE                            
         B     VCOLF20                                                          
VCOLF18  CLI   22(R4),C'-'                                                      
         BNE   VCOLF20                                                          
         CLC   ELEM(2),ELEM+2                                                   
         BNE   BADCOL                                                           
         XC    ELEM(2),ELEM        ZERO OUT START DATE                          
*                                                                               
VCOLF20  BRAS  RE,GETEXTC                                                       
         BNE   VCOLNXT                                                          
         USING EXTCOLSD,RF                                                      
         MVC   EXTPD,ELEM                                                       
         DROP  RF                                                               
         B     VCOLNXT                                                          
*                                                                               
VCOLF22  DS    0H                  EST FILT?                                    
         EX    R5,VCOLFEST                                                      
         BNE   VCOLF24                                                          
         TM    3(R4),X'80'         VALID NUMERIC                                
         BZ    BADCOL                                                           
         OC    8(3,R4),8(R4)       MAKE SURE L.T. 255                           
         BNZ   BADCOL                                                           
         BRAS  RE,GETEXTC                                                       
         BNE   VCOLNXT                                                          
         USING EXTCOLSD,RF                                                      
         MVC   EXTEST,11(R4)                                                    
         DROP  RF                                                               
         B     VCOLNXT                                                          
*                                                                               
VCOLF24  DS    0H                                                               
         EX    R5,VCOLFTRD                                                      
         BNE   VCOLF26                                                          
         BRAS  RE,GETEXTC                                                       
         BNE   VCOLNXT                                                          
         USING EXTCOLSD,RF                                                      
         OI    EXTFLAG1,EXTTRD                                                  
         DROP  RF                                                               
         B     VCOLNXT                                                          
*                                                                               
VCOLF26  DS    0H                                                               
         EX    R5,VCOLFCSH                                                      
         BNE   VCOLF27                                                          
         BRAS  RE,GETEXTC                                                       
         BNE   VCOLNXT                                                          
         USING EXTCOLSD,RF                                                      
         OI    EXTFLAG1,EXTCASH                                                 
         DROP  RF                                                               
         B     VCOLNXT                                                          
*                                                                               
VCOLF27  DS    0H                                                               
         EX    R5,VCOLFLMG         LMG COLUMN FILTER?                           
         BNE   VCOLF28             NO                                           
         BRAS  RE,GETEXTC          HAVE EXTENDED COLUMNS?                       
         BNE   VCOLF28A            NO - CHECK IF PROCESSING BUYS!               
         USING EXTCOLSD,RF                                                      
         OI    EXTFLAG1,EXTLMG                                                  
         DROP  RF                                                               
         B     VCOLF28A            CHECK IF PROCESSING BUYS!                    
*                                                                               
VCOLF28  DS    0H                                                               
         EX    R5,VCOLFREG         REG COLUMN FILTER?                           
         BNE   VCOLF50             NO                                           
         BRAS  RE,GETEXTC          HAVE EXTENDED COLUMNS?                       
         BNE   VCOLF28A            NO - CHECK IF PROCESSING BUYS!               
         USING EXTCOLSD,RF                                                      
         OI    EXTFLAG1,EXTREG                                                  
         DROP  RF                                                               
*                                                                               
VCOLF28A CLI   DRARGSI,C'B'        PROCESSING BUYS?                             
         BNE   VCOLNXT             NO                                           
         TM    ROWIND2,ROWIPRD     HAVE PRD IN ROWS                             
         BZ    BADCOL5             NO, NEED PRD TO GET BILL PCT                 
         TM    ROWIND2,ROWIEST     HAVE EST IN ROWS                             
         BZ    BADCOL5             NO, NEED EST TO GET BILL PCT                 
         TM    ROWIND,ROWICML      HAVE CML KEYWORD?                            
         BNZ   BADCOL6             YES - CANNOT MIX WITH BPCT                   
         OI    SBQREAD2,SBQRD2BP   YES - READ BPCT RECORDS                      
         B     VCOLNXT                                                          
*                                                                               
VCOLF50  BAS   RE,VCOLMXMN         MAX/MIN EXPRESSION                           
         BE    VCOLNXT                                                          
*                                                                               
VCOLFEND B     BADCOL                                                           
         SPACE 1                                                                
VCOLFDPT CLC   12(0,R4),=C'DPT '    EXECUTED INSTRUCTIONS                       
VCOLFMAS CLC   12(0,R4),=C'MASDPT '                                             
VCOLFADJ CLC   12(0,R4),=C'ADJ '                                                
VCOLFSTA CLC   12(0,R4),=C'STATION '                                            
VCOLFPRD CLC   12(0,R4),=C'PRD'                                                 
VCOLFLEN CLC   12(0,R4),=C'LEN '                                                
VCOLFORG CLC   12(0,R4),=C'ORIG '                                               
VCOLFSPL CLC   12(0,R4),=C'SPILL '                                              
VCOLFSRP CLC   12(0,R4),=C'SREP '                                               
VCOLFPD  CLC   12(0,R4),=C'PD '                                                 
VCOLFEST CLC   12(0,R4),=C'EST '                                                
VCOLFMVC MVC   DUB(0),22(R4)                                                    
VCOLFTRD CLC   12(0,R4),=C'TRADE '                                              
VCOLFCSH CLC   12(0,R4),=C'CASH '                                               
VCOLFLMG CLC   12(0,R4),=C'LMG '                                                
VCOLFREG CLC   12(0,R4),=C'REG '                                                
         EJECT                                                                  
* WRAP UP THE COLUMNS                                                           
*                                                                               
VCOLNXT  LA    R4,42(R4)                                                        
         XR    R1,R1                                                            
         IC    R1,FIELDERR                                                      
         AHI   R1,1                                                             
         STC   R1,FIELDERR                                                      
         BCT   R0,VCOL12                                                        
*                                                                               
         CLI   COLXTEND,1          TEST THERE IS AN EXTENSION PENDING           
         BNE   VCOL50                                                           
         MVI   COLXTEND,2          YES-NOT TIME TO WRAP UP YET                  
         LLC   R1,5(R2)                PUT BACK ACTUAL LENGTH                   
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)                                                         
         B     VCOLX                                                            
*                                                                               
VCOL50   MVI   COLXTEND,0                                                       
         MVI   DRNARGSI,16         ALWAYS PASS ALL ARGS                         
         LA    R1,DRHEAD1                                                       
         TM    0(R1),X'80'         TEST HEADLINE 1 IS ACTIVE                    
         BZ    VCOL54                                                           
         LA    RE,HEADRTNS         YES-TEST HEADLINE ROUTINE WITH MORE          
*                                      THAN ONE HEADLINE                        
VCOL52   CLI   0(RE),0                                                          
         BE    VCOL54                                                           
         CLC   1(8,R1),0(RE)                                                    
         BE    *+12                                                             
         LA    RE,9(RE)                                                         
         B     VCOL52                                                           
         MVI   24(R1),1            YES-ARG16=HEADLINE NUMBER                    
         MVI   25(R1),16               N'ARGS=16                                
         LA    RF,DRHEAD2                                                       
         MVC   0(26,RF),0(R1)                                                   
         MVI   24(RF),2            HEAD2                                        
         CLI   8(RE),3                                                          
         BL    VCOL54                                                           
         LA    RF,DRHEAD3                                                       
         MVC   0(26,RF),0(R1)                                                   
         MVI   24(RF),3            HEAD3                                        
         CLI   8(RE),4                                                          
         BL    VCOL54                                                           
         LA    RF,DRHEAD4                                                       
         MVC   0(26,RF),0(R1)                                                   
         MVI   24(RF),4            HEAD4                                        
         B     VCOL54                                                           
*                                                                               
HEADRTNS DC    CL8'HDEM    ',X'04'                                              
         DC    CL8'HCPP    ',X'03'                                              
         DC    CL8'HCPPS   ',X'04'                                              
         DC    CL8'HDOLNDX ',X'03'                                              
         DC    CL8'HDEMNDX ',X'03'                                              
         DC    X'00'                                                            
*                                                                               
VCOL54   CLC   BLOCK+54(4),=C'UDEF'  EXCEPT FOR UDEF,                           
         BE    VCOL56                                                           
         CLC   BLOCK+54(4),=C'UCOM'  OR UCOM,                                   
         BE    VCOL56                                                           
         BAS   RE,HEADPOP          MAY POP IN ADDITIONAL HEADINGS               
*                                                                               
VCOL56   OC    DRARGSI+12(4),DRARGSI+12  TEST PERIOD WAS SPECIFIED              
         BNZ   *+12                                                             
         MVI   SBQPERLO,1          NO-FORCE WHOLE PERIOD                        
         MVI   SBQPERHI,X'FF'                                                   
         CLI   MYPOSO,C'N'         IF THERE IS ANY PRINTING                     
         BE    VCOLGEN                                                          
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         LLC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
*                                                                               
VCOLGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   VCOLGEN4                                                         
*                                                                               
* ADD COLUMN ENTRIES TO PQINDEX                                                 
         ICM   RF,15,APQINDEX                                                   
         BZ    VCOLGEN1                                                         
         USING PQINDEX,RF                                                       
         LA    RE,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS                              
*                                                                               
         OC    PQKEYWRD,PQKEYWRD   IN USE?                                      
         BZ    *+16                 NO                                          
         LA    RF,PQINDXEQ(RF)                                                  
         BCT   RE,*-14                                                          
         B     VCOLGEN1                                                         
         MVC   PQKEYWRD,BLOCK+42+12                                             
         MVC   PQOUTLEN,DRLENO                                                  
         MVI   PQPOSO,C'C'                                                      
         MVC   PQHEAD1,DRH1LIT                                                  
         MVC   PQHEAD2,DRH2LIT                                                  
         MVC   PQHEAD3,DRH3LIT                                                  
         MVC   PQHEAD4,DRH4LIT                                                  
         DROP  RF                                                               
*                                                                               
VCOLGEN1 MVC   DRLABELI,MYLABEL                                                 
         MVC   1(1,R3),DRDECO      SAVE EDIT CHARACTERISTICS                    
         MVC   2(1,R3),DRDIVO                                                   
         MVC   DRPOSO,MYPOSO                                                    
         BRAS  RE,GCOLDRON                                                      
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         CLC   BLOCK+12(7),=C'COMPUTE'                                          
         BNE   VCOLGEN2                                                         
         LA    R4,BLOCK+42                                                      
         BRAS  RE,GCMPDRON                                                      
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
*                                                                               
VCOLGEN2 CLI   COLRANK,0           TEST COLUMN RANKING                          
         BE    VCOLGEN4                                                         
         XC    BLOCK(12),BLOCK     YES-GENERATE ADDITIONAL RANK COL             
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),BLNKS                                               
         MVC   BLOCK+12(7),=C'COLRANK'                                          
         LA    R4,BLOCK                                                         
         BRAS  RE,VCOLDRON                                                      
         MVC   DRARGSI(1),COLRANK                                               
         MVI   DRNARGSI,1                                                       
         MVI   COLRANK,0                                                        
         BRAS  RE,GCOLDRON                                                      
*                                                                               
VCOLGEN4 MVC   PERLIST(15),PERLIST+1  SLIDE PERIOD NUMBERS                      
         MVI   PERLIST+15,0                                                     
         CLI   PERLIST,0           TEST ANY MORE                                
         BNE   VCOL0               YES-GO BACK AND GENERATE ANOTHER             
*                                                                               
VCOLGENX B     VCOLX                                                            
*                                                                               
*                                                                               
VCOLX    J     XITEQU                                                           
*                                                                               
SQDNOOPE DS    0H                                                               
         MVC   GTMSGNO,=Y(SQDNOOPT)  SQAD OPTION NOT ENTERED                    
         J     XITNEQ                                                           
BADCOL   DS    0H                                                               
         MVC   GTMSGNO,=Y(COLERR)                                               
         J     XITNEQ                                                           
BADCOL2  DS    0H                                                               
         MVC   GTMSGNO,=Y(INCMPERR)                                             
         J     XITNEQ                                                           
BADCOL3  DS    0H                                                               
         MVC   GTMSGNO,=Y(NWKBYOP)   NETBUYS OPTION REQUIRED                    
         J     XITNEQ                                                           
BADCOL4  DS    0H                                                               
         MVC   GTMSGNO,=Y(NWKBYOPX)  NETBUYS OPTION NOT ALLOWED                 
         J     XITNEQ                                                           
BADCOL5  DS    0H                                                               
         MVC   GTMSGNO,=Y(1318)      NEED PRD & EST IN ROWS FOR BILLPCT         
         J     XITNEQ                                                           
BADCOL6  MVC   GTMSGNO,=Y(1315)      CAN'T MIX BPCT W/COMMERCIAL KYWD           
         J     XITNEQ                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO FILL IN DATE FILTER FOR BILLTDY KEYWORD                            
BILLTDY  NTR1                                                                   
         OI    SBQPER,SBQPDY                                                    
         OI    OPTIND4,OPTCOLBD                                                 
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,ELEM)                                  
         BRAS  RE,GETEXTC                                                       
         JNE   XIT                                                              
         USING EXTCOLSD,RF                                                      
         MVC   EXTPD(2),ELEM                                                    
         MVC   EXTPD+2(2),ELEM                                                  
         DROP  RF                                                               
         J     XIT                                                              
         EJECT                                                                  
* ROUTINE TO FIGURE OUT EDITS FOR COMPUTES                                      
* RETURN CC EQ - OK                                                             
*           NE - INVALID COMPUTE EXPRESSION                                     
*                                                                               
COMPEDIT NTR1                                                                   
         LLC   R1,0(R4)            PICK UP EXPRESSION LENGTH                    
         LA    R1,10(R1,R4)                                                     
         CLI   0(R1),C'%'          IS LAST OPERATOR PERCENT?                    
         BE    COMPPCT                                                          
         CLI   0(R1),C'I'          OR INDEX?                                    
         BE    COMPINX                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),C'V'          OR VERTICAL PERCENT                          
         BE    COMPPCT                                                          
         LA    RF,8(R2)            RF=A(START OF INPUT STRING)                  
         LLC   R0,5(R2)            R0=LENGTH OF INPUT STRING                    
         BAS   RE,CMPEDFND         ELSE LOOK FOR FIRST OPERAND                  
         BNE   COMPEDNE                                                         
         MVC   DRDECO,1(R1)        PICK UP ITS EDIT CHARACTERISTIC              
         MVC   DRDIVO,2(R1)                                                     
         CLI   1(RF),C'/'          TEST DIVIDE                                  
         BNE   COMPEDEQ                                                         
         LA    RF,2(RF)            YES-PICK UP DIVIDOR'S EDIT                   
         AHI   R0,-2                   CHARACTERISTIC                           
         BNP   COMPEDEQ                                                         
         BAS   RE,CMPEDFND                                                      
         BNE   COMPEDEQ                                                         
         CLI   1(R1),1             TEST FOR DEC=1                               
         BNE   COMPEDEQ                                                         
         SR    RE,RE                                                            
         ICM   RE,1,DRDECO         YES-ONE LESS DECIMAL PLACE FOR               
         BZ    *+14                    RESULT                                   
         BCTR  RE,0                                                             
         STC   RE,DRDECO                                                        
         B     COMPEDEQ                                                         
         ICM   RE,1,DRDIVO         OR, DIVIDE BY ONE LESS POWER OF 10           
         BZ    COMPEDEQ                                                         
         BCTR  RE,0                                                             
         STC   RE,DRDIVO                                                        
         B     COMPEDEQ                                                         
*                                                                               
COMPPCT  MVI   DRDECO,2            PERCENTS HAVE 2 DEC                          
         MVI   DRTRAILO,C'%'       AND END WITH PERCENT SIGN                    
         B     COMPEDEQ                                                         
*                                                                               
COMPINX  MVI   DRDECO,0            INDEXES HAVE 0 DEC                           
*                                                                               
COMPEDEQ J     XITEQU                                                           
*                                                                               
COMPEDNE J     XITNEQ                                                           
         SPACE 2                                                                
CMPEDFND LA    R1,EDITLIST                                                      
CMPEDFN2 CLI   0(R1),0                                                          
         BNE   CMPEDFN4                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CMPEDFND                                                      
         LTR   RE,RE               NO VARIABLES IN INPUT STRING                 
         BR    RE                                                               
CMPEDFN4 CLC   0(1,R1),0(RF)                                                    
         BER   RE                                                               
         LA    R1,4(R1)                                                         
         B     CMPEDFN2                                                         
         EJECT                                                                  
* ROUTINE TO PUT IN ADDITIONAL HEADINGS                                         
*                                                                               
HEADPOP  NTR1                                                                   
         LA    R1,DRHEAD1          TEST HEADLINE 1 IS OVERRIDDEN                
         USING DRHEADD,R1                                                       
         CLC   DRHARGS(8),=C'OVERRIDE'                                          
         BNE   *+14                                                             
         XC    DRHARGS(8),DRHARGS                                               
         B     HEADPOP5            YES-DON'T POP OUR OWN HEADLINE THERE         
         DROP  R1                                                               
         CLI   DRARGSI+10,0       TEST DAYPART FILTER                           
         BE    HEADPOP1                                                         
         BAS   RE,HPSHUFFL                                                      
         OI    DRHEAD1,X'80'                                                    
         MVI   DRH1LITL,5                                                       
         MVC   DRH1LIT+4(1),DRARGSI+10                                          
         MVC   DRH1LIT(4),=C'DPT='                                              
         TM    DRARGSI+10,X'40'                                                 
         BNZ   *+14                                                             
         OI    DRH1LIT+4,X'40'                                                  
         MVC   DRH1LIT(3),=C'MAS='                                              
*                                                                               
         TM    DRARGSI+11,GLIIADJ  IS IT REALLY AN ADJ CODE FILTER?             
         BZ    HEADPOP1             NO                                          
         MVC   DRH1LIT(4),=C'ADJ='                                              
         CLI   DRARGSI+10,C'A'     ALPHA OR PWOS BETWEEN 01 - 99                
         BNL   HEADPOP1             ALPHA                                       
         MVI   DRH1LITL,6                                                       
         SR    RE,RE                                                            
         IC    RE,DRARGSI+10                                                    
         SRDL  RE,4                                                             
         STC   RE,DRH1LIT+4                                                     
         OI    DRH1LIT+4,X'F0'                                                  
         SRL   RF,28                                                            
         STC   RF,DRH1LIT+5                                                     
         OI    DRH1LIT+5,X'F0'                                                  
*                                                                               
HEADPOP1 OC    DRARGSI+7(3),DRARGSI+7     TEST STATION FILTER                   
         BZ    HEADPOP3                                                         
         TM    DRARGSI+11,GLIIPRD                                               
         BO    HEADPOP2                                                         
         BAS   RE,HPSHUFFL                                                      
         OI    DRHEAD1,X'80'                                                    
         XC    DUB(2),DUB                                                       
         MVC   DUB+2(3),DRARGSI+7                                               
         GOTO1 MSUNPK,DMCB,DUB,FULL,DRH1LIT                                     
         MVI   DRH1LITL,5                                                       
         B     HEADPOP3                                                         
*                                                                               
HEADPOP2 BAS   RE,HPSHUFFL         PRODUCT FILTER                               
         OI    DRHEAD1,X'80'                                                    
         MVI   DRH1LITL,3                                                       
         MVC   DRH1LIT(3),DRARGSI+7                                             
         CLI   DRARGSI+9,C' '                                                   
         BH    HEADPOP3                                                         
         MVI   DRH1LITL,2                                                       
*                                                                               
HEADPOP3 LA    R1,DRARGSI                                                       
         USING GLARGSD,R1                                                       
         TM    GLIIND,GLIIMAT      TEST MATCHED FILTER                          
         BZ    HEADPOP4                                                         
         BAS   RE,HPSHUFFL                                                      
         OI    DRHEAD1,X'80'                                                    
         MVC   DRH1LIT(7),=C'MATCHED'                                           
         MVI   DRH1LITL,7                                                       
         B     HEADPOP5                                                         
*                                                                               
HEADPOP4 TM    GLIIND,GLIIUNM      TEST UNMATCHED FILTER                        
         BZ    HEADPOP5                                                         
         DROP  R1                                                               
         BAS   RE,HPSHUFFL                                                      
         OI    DRHEAD1,X'80'                                                    
         MVC   DRH1LIT(9),=C'UNMATCHED'                                         
         MVI   DRH1LITL,9                                                       
*                                                                               
HEADPOP5 OC    DRARGSI+12(4),DRARGSI+12   WAS A PERIOD SPECIFIED?               
         BZ    HEADPOP6                                                         
         BAS   RE,HPSHUFFL                                                      
         OI    DRHEAD1,X'80'                                                    
         MVC   DRH1RTN,=CL8'HPER'                                               
         MVC   DRH1ARGS(4),DRARGSI+12   PASS A(PERIOD S/E)                      
         MVI   DRH1NARG,4          N'ARGUMENTS IS 4                             
*                                                                               
HEADPOP6 SR    RF,RF                                                            
         TM    DROPTSO+1,DRCUME    TEST FOR CUME OPTION OR GST                  
         BO    HEADPOP7                                                         
         LA    R4,DRARGSO                                                       
         USING GLARGSD,R4                                                       
         TM    GLOIND,GLOICUME                                                  
         BO    HEADPOP7                                                         
         LA    R4,DRARGSI                                                       
         TM    GLIIND,GLIIGSTI+GLIIGSTO                                         
         BZ    HEADPOP8                                                         
         LA    RF,1                                                             
*                                                                               
HEADPOP7 LA    R5,DRHEAD1          YES-ADD 'CUME' OR '+GST' TO                  
         LA    R0,4                    BOTTOM HEADING                           
         TM    0(R5),X'80'                                                      
         BZ    *+16                                                             
         LA    R5,L'DRH1ALL(R5)                                                 
         BCT   R0,*-12                                                          
         B     HEADPOP8                                                         
         OI    0(R5),X'80'                                                      
         USING DRHEAD1,R5                                                       
         MVI   DRH1LITL,4                                                       
         LTR   RF,RF                                                            
         BNZ   *+14                                                             
         MVC   DRH1LIT(4),=C'CUME'                                              
         B     HEADPOP8                                                         
         MVC   DRH1LIT(4),=C'+GST'                                              
         TM    GLIIND,GLIIPST        TEST PST INCLUDED                          
         BZ    *+12                                                             
         MVI   DRH1LIT+4,C'+'                                                   
         MVI   DRH1LITL,5                                                       
         TM    GLIIND,GLIIGSTI       TEST INPUT GST/PST                         
         BZ    HEADPOP8                                                         
         CLC   DRRTNI(6),=C'IBYDOL'  AND BUY GROSS DOLLARS                      
         BE    *+14                                                             
         CLC   DRRTNI(6),=C'IBYNET'  OR BUY NET DOLLARS                         
         BNE   HEADPOP8                                                         
         MVI   DRH1LIT+4,C'I'        YES-INDICATE IT'S INPUT GST/PST            
         MVI   DRH1LITL,5                                                       
         TM    GLIIND,GLIIPST        TEST PST INCLUDED                          
         BZ    HEADPOP8                                                         
         MVI   DRH1LIT+5,C'+'                                                   
         MVI   DRH1LITL,6                                                       
         DROP  R4,R5                                                            
*                                                                               
HEADPOP8 DS    0H                                                               
         ZICM  RE,DRARGSI+6,1      TEST ANY EXTENDED COLUMN FILTERS             
         BZ    HEADPOPX                                                         
         BRAS  RE,GETEXTC                                                       
         BNE   HEADPOPX                                                         
         LR    R5,RF                                                            
         USING EXTCOLSD,R5                                                      
*                                                                               
         TM    EXTFLAG1,EXTORIG    ORIG FILTER?                                 
         BZ    HP8A                 NO                                          
         BAS   RE,HPSHUFFL                                                      
         OI    DRHEAD1,X'80'                                                    
         MVC   DRH1LIT(06),=C'*ORIG*'                                           
         MVI   DRH1LITL,6                                                       
         B     HEADPOP9                                                         
*                                                                               
HP8A     TM    EXTFLAG1,EXTSPIL    SPILL FILTER?                                
         BZ    HEADPOP9             NO                                          
         BAS   RE,HPSHUFFL                                                      
         OI    DRHEAD1,X'80'                                                    
         MVC   DRH1LIT(06),=C'*SPIL*'                                           
         MVI   DRH1LITL,6                                                       
*                                                                               
HEADPOP9 OC    EXTPD,EXTPD         PERIOD FILTER?                               
         BZ    HEDPOP20             NO                                          
         BAS   RE,HPSHUFFL                                                      
         OI    DRHEAD1,X'80'                                                    
         LA    R4,DRH1LIT                                                       
         OC    EXTPD(2),EXTPD                                                   
         BNZ   *+16                                                             
         MVI   DRH1LIT,C'-'                                                     
         LA    R4,1(R4)                                                         
         B     HEDPOP10                                                         
         OC    EXTPD+2(2),EXTPD+2                                               
         BNZ   *+16                                                             
         MVI   DRH1LIT,C'+'                                                     
         LA    R4,1(R4)                                                         
         B     HEDPOP10                                                         
         CLC   EXTPD(2),EXTPD+2                                                 
         BE    *+8                                                              
         MVI   DRH1LIT+5,C'-'                                                   
*                                                                               
HEDPOP10 OC    EXTPD(2),EXTPD                                                   
         BZ    HEDPOP12                                                         
         GOTO1 DATCON,DMCB,(2,EXTPD),(4,0(R4))                                  
         LA    R4,6(R4)                                                         
HEDPOP12 OC    EXTPD+2(2),EXTPD+2                                               
         BZ    HEDPOP14                                                         
         CLC   EXTPD(2),EXTPD+2                                                 
         BE    HEDPOP14                                                         
         GOTO1 DATCON,DMCB,(2,EXTPD+2),(4,0(R4))                                
         LA    R4,6(R4)                                                         
HEDPOP14 LA    RF,DRH1LIT          SET LENGTH                                   
         SR    R4,RF                                                            
         STC   R4,DRH1LITL                                                      
*                                                                               
HEDPOP20 CLI   EXTEST,0            EST FILTER?                                  
         BZ    HEADPOPX             NO                                          
         BAS   RE,HPSHUFFL                                                      
         OI    DRHEAD1,X'80'                                                    
         MVC   DRH1LIT(04),=C'EST='                                             
         EDIT  EXTEST,(3,DRH1LIT+4),ALIGN=LEFT                                  
         AHI   R0,4                                                             
         STC   R0,DRH1LITL                                                      
         DROP  R5                                                               
*                                                                               
HEADPOPX J     XIT                                                              
         SPACE 2                                                                
HPSHUFFL TM    DRHEAD1,X'80'       SHUFFLE THE HEADLINES DOWN                   
         BZR   RE                  IF NECESSARY                                 
         MVC   DRH4ALL,DRH3ALL                                                  
         MVC   DRH3ALL,DRH2ALL                                                  
         MVC   DRH2ALL,DRH1ALL                                                  
         XC    DRH1ALL,DRH1ALL                                                  
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO VALIDATE A GENERIC COLUMN EXPRESSION                               
*                                                                               
GENERIC  NTR1  ,                                                                
         MVC   DUB,BLNKS                                                        
         MVI   BYTE,0                                                           
         CLC   12(3,R4),=C'DOL'                                                 
         BE    VG20                                                             
******** TM    SBQDATA,SBQDRERT+SBQDAFFD  TEST DATA FOR RERATE OR AFFID         
******** BZ    VG02                                                             
******** OC    SBQBOOK,SBQBOOK            YES - TEST BOOK SET YET               
******** BNZ   VG02                                                             
******** MVC   SBQBOOK,=C'ACT '           NO - DEFAULT = ACTUAL BOOK            
*                                                                               
VG02     CLI   0(R4),3             TEST FOR EXPRESSION DEM(N)                   
         BE    VG03                                 OR CPP(N)                   
         CLI   0(R4),4                                                          
         BNE   VG10                                                             
         CLI   15(R4),C'2'                                                      
         BL    VG10                                                             
         MVC   BYTE,15(R4)                                                      
*                                                                               
VG03     CLI   SBQDATA,0           TEST DATA SET YET                            
         BNE   VG04                                                             
         MVI   SBQDATA,SBQDPUR     DEFAULT = PURCHASED                          
         OC    SBQBOOK,SBQBOOK     UNLESS BOOK IS SET                           
         BZ    VG04                                                             
         MVI   SBQDATA,SBQDRERT    WHEN DEFAULT = RERATE                        
*                                                                               
VG04     LA    R3,VGTABLE1                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
VG06     CLI   0(R3),0                                                          
         JE    XITNEQ                                                           
         IC    RE,0(R3)                                                         
         CLC   SBQDATA,1(R3)                                                    
         BE    *+12                                                             
         LA    R3,2(RE,R3)                                                      
         B     VG06                                                             
         LA    R3,2(R3)                                                         
         B     VG90                                                             
*                                                                               
VG10     CLI   0(R4),4             TEST FOR EXPRESSION DEM(A/B)(N)              
         BE    VG12                                 OR CPP(A/B)(N)              
         CLI   0(R4),5                                                          
         JNE   XITNEQ                                                           
         CLI   15(R4),C'A'                                                      
         BE    *+12                                                             
         CLI   15(R4),C'B'                                                      
         JNE   XITNEQ                                                           
         CLI   16(R4),C'2'                                                      
         JL    XITNEQ                                                           
         MVC   BYTE,16(R4)                                                      
*                                                                               
VG12     CLI   SBQDATA,0           TEST DATA SET YET                            
         BNE   VG14                                                             
         MVI   SBQDATA,SBQDGOAL+SBQDPUR   DEFAULT = GOAL V PURCHASED            
         OC    SBQBOOK,SBQBOOK            UNLESS BOOK IS SET                    
         BZ    VG14                                                             
         MVI   SBQDATA,SBQDGOAL+SBQDRERT  WHEN DEFAULT = GOAL V ACHVD           
*                                                                               
VG14     LA    R3,VGTABLE2                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
VG16     CLI   0(R3),0                                                          
         JE    XITNEQ                                                           
         IC    RE,0(R3)                                                         
         IC    RF,1(R3)                                                         
         CLC   SBQDATA,2(R3)                                                    
         BE    *+14                                                             
         AR    RE,RF                                                            
         LA    R3,3(RE,R3)                                                      
         B     VG16                                                             
         CLI   15(R4),C'A'                                                      
         BNE   *+12                                                             
         LA    R3,3(R3)                                                         
         B     VG90                                                             
         CLI   15(R4),C'B'                                                      
         JNE   XITNEQ                                                           
         LA    R3,3(RE,R3)                                                      
         LR    RE,RF                                                            
         B     VG90                                                             
*                                                                               
VG20     LA    RE,2                                                             
         CLI   0(R4),3             TEST EXPRESSION = DOL                        
         BE    VG22                                                             
         CLI   0(R4),4             TEST EXPRESSION = DOL(A/B)                   
         JNE   XITNEQ                                                           
         CLI   15(R4),C'A'                                                      
         BNE   VG24                                                             
         CLI   SBQDATA,0                                                        
         BNE   VG22                                                             
         LA    R3,=C'GL'                                                        
         B     VG90                                                             
*                                                                               
VG22     LA    R3,=C'GL'                                                        
         TM    SBQDATA,SBQDGOAL                                                 
         BO    VG90                                                             
         LA    R3,=C'OR'                                                        
         TM    SBQDATA,SBQDORD                                                  
         BO    VG90                                                             
         LA    R3,=C'BY'                                                        
         B     VG90                                                             
*                                                                               
VG24     CLI   15(R4),C'B'                                                      
         JNE   XITNEQ                                                           
         LA    R3,=C'BY'                                                        
*                                                                               
VG90     BCTR  RE,0                                                             
         EX    RE,VGEX                                                          
         LA    RF,4(RE)                                                         
         LA    RE,DUB+1(RE)                                                     
         MVC   0(3,RE),12(R4)                                                   
         CLI   BYTE,0                                                           
         BE    *+14                                                             
         MVC   3(1,RE),BYTE                                                     
         LA    RF,1(RF)                                                         
         MVC   12(10,R4),BLNKS                                                  
         MVC   12(8,R4),DUB                                                     
         STC   RF,0(R4)                                                         
         J     XITEQU                                                           
*                                                                               
VGEX     MVC   DUB(0),0(R3)        EXECUTED                                     
*                                                                               
VGTABLE1 DC    AL1(2),AL1(SBQDGOAL),C'GL'                                       
         DC    AL1(2),AL1(SBQDORD),C'OR'                                        
         DC    AL1(3),AL1(SBQDPUR),C'BYP'                                       
         DC    AL1(3),AL1(SBQDRERT),C'BYR'                                      
         DC    AL1(3),AL1(SBQDAFFD),C'BYA'                                      
         DC    X'00'                                                            
*                                                                               
VGTABLE2 DC    AL1(2),AL1(3),AL1(SBQDGOAL+SBQDPUR),C'GL',C'BYP'                 
         DC    AL1(2),AL1(3),AL1(SBQDGOAL+SBQDRERT),C'GL',C'BYR'                
         DC    AL1(2),AL1(3),AL1(SBQDGOAL+SBQDAFFD),C'GL',C'BYA'                
         DC    AL1(3),AL1(3),AL1(SBQDPUR+SBQDRERT),C'BYP',C'BYR'                
         DC    AL1(3),AL1(3),AL1(SBQDPUR+SBQDAFFD),C'BYP',C'BYA'                
         DC    AL1(2),AL1(3),AL1(SBQDORD+SBQDPUR),C'OR',C'BYP'                  
         DC    AL1(2),AL1(3),AL1(SBQDORD+SBQDRERT),C'OR',C'BYR'                 
         DC    AL1(2),AL1(3),AL1(SBQDORD+SBQDAFFD),C'OR',C'BYA'                 
         DC    X'00'                                                            
         EJECT                                                                  
* ROUTINE TO VALIDATE PERIOD EXPRESSION                                         
* INPUT  : P1=A(INPUT EXPRESSION)                                               
* OUTPUT : PERLIST=X'00' NOT A PERIOD EXPRESSION                                
*                  X'FF' INVALID PERIOD EXPRESSION                              
*                  OTHER LIST OF PERIOD NUMBERS                                 
*          CC EQ - PERIOD EXPRESSION                                            
*          CC NE - NOT A PERIOD EXPRESSION                                      
*                                                                               
VALPEX   NTR1  ,                                                                
         L     R2,0(R1)                                                         
         LR    R5,R2               SAVE A(DATE FILTER)                          
         MVI   PERFLAG,C'N'                                                     
         TM    ROWIND2,ROWAPER     SEE IF PERIOD KEYWORD IS AVGMON              
         BNZ   *+12                 YES - SKIP NEXT CHECK                       
         TM    ROWIND,ROWIPER      CHECK FOR PERIOD IN ROWS                     
         BO    VPERXNE             YES                                          
*                                                                               
VPER00   LA    R3,2                                                             
         LA    R4,YEARLO                                                        
         CLI   0(R2),C'Y'          YEARS 1-2                                    
         BE    VPER2                                                            
         LA    R3,4                                                             
         LA    R4,HYEARLO                                                       
         CLC   0(2,R2),=C'HY'      HALF YEARS 1-4                               
         BNE   *+12                                                             
         LA    R2,1(R2)                                                         
         B     VPER2                                                            
         LA    R3,8                                                             
         LA    R4,QTRLO                                                         
         CLI   0(R2),C'Q'          QUARTERS 1-8                                 
         BE    VPER2                                                            
*        LA    R3,24               WRITER ONLY ALLOWS M1-16!                    
         LA    R3,16                                                            
         LA    R4,MONLO                                                         
         CLI   0(R2),C'M'          MONTHS 1-16                                  
         BE    VPER2                                                            
         LA    R3,14                                                            
         LA    R4,WEKLO                                                         
         CLI   0(R2),C'W'          WEEKS 1-14                                   
         BE    VPER2                                                            
         LA    R3,14                                                            
         LA    R4,DAYLO                                                         
         CLI   0(R2),C'D'          DAYS 1-14                                    
         BNE   VPERXNE                                                          
*                                                                               
VPER2    CLC   =C'S-E',1(R2)       REQUEST START TO END?                        
         BNE   VPER3               NO                                           
         L     R6,ALASTCOL         A(FIELD HEADER)                              
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BNE   *+12                NO  - DON'T CHANGE S-E ONLINE!               
         OI    6(R6),X'80'         OFFLINE = TRANSMIT THE CHANGE                
         B     *+8                                                              
         MVI   PERFLAG,C'Y'        TURN ON PERIOD S-E FLAG ONLINE               
*                                                                               
         LA    R6,8(R6)            POINT TO FIRST CHARACTER ON COLUMN           
         MVC   ELEM(32),0(R6)                                                   
*                                                                               
VPER2A   CLI   0(R6),C','          DELIMITER?                                   
         BNE   VPER2AA             NO, BUMP                                     
         CLC   1(4,R6),0(R2)       FOUND DATE FILTER?                           
         BE    VPER2AB             YES - CHANGE IT                              
         CLC   2(4,R6),0(R2)       FOUND DATE FILTER FOR HYS-E?                 
         BE    VPER2AB             YES - CHANGE IT                              
*                                                                               
VPER2AA  AHI   R6,1                NO  - BUMP                                   
         B     VPER2A                                                           
*                                                                               
VPER2AB  AHI   R6,2                POINT TO 'S-E'                               
         CLI   0(R6),C'Y'          HALF YEARS?                                  
         BNE   *+8                 NO                                           
         AHI   R6,1                YES - POINT TO 'S-E'                         
         MVI   0(R6),C'1'          START PERIOD IS ALWAYS 1                     
         CHI   R3,9                HAVE 2 DIGIT MAX END PERIOD?                 
         BH    VPER2B              YES                                          
         EDIT  (R3),(1,2(R6)),ZERO=NOBLANK,FILL=0                               
         MVC   1(3,R2),0(R6)       CHANGE SCANNER BLOCK ENTRY AS WELL           
         B     VPER3                                                            
*                                                                               
VPER2B   CLI   3(R6),C' '          HAVE ANYTHING AFTER PERIOD FILTER?           
         BNH   VPER2C              NO, EDIT AWAY                                
         L     RE,ALASTCOL         A(FIELD HEADER)                              
         AHI   RE,35               MAX FIELD LENGTH +HEADER -5                  
         SR    RE,R6               LENGTH TO MOVE AFTER S-E                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),3(R6)                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R6),WORK                                                     
*                                                                               
VPER2C   EDIT  (R3),(2,2(R6)),ZERO=NOBLANK,FILL=0                               
         MVC   1(4,R2),0(R6)       CHANGE SCANNER BLOCK ENTRY AS WELL           
         LR    RE,R5               A(SCANNER BLOCK ENTRY)                       
         SHI   RE,12               POINT TO LENGTH                              
         LLC   RF,0(RE)            CURRENT LENGTH                               
         AHI   RF,1                ADD 1                                        
         STC   RF,0(RE)            PUT IT BACK                                  
*                                                                               
VPER3    LA    R2,1(R2)                                                         
         BAS   RE,VPERNUM          VALIDATE FIRST NUMBER                        
         LTR   R0,R0                                                            
         BZ    VPERXNE             INVALID                                      
         CR    R0,R3               TEST NOT TOO HIGH                            
         BH    VPERXNE                                                          
*                                                                               
         CLI   PERLIST,0           TEST PERIOD LIST ALREADY IN PROGRESS         
         BNE   VPERX               YES-DONE                                     
*                                                                               
         MVC   FULL(2),0(R4)       SAVE VALS IN CASE WE CHG ON THE FLY          
*                                                                               
         XC    PERLIST,PERLIST                                                  
         STC   R0,PERLIST          SAVE FIRST PERIOD NUMBER                     
         CLI   0(R4),0             UPDATE LOWEST/HIGHEST PERIOD NOS             
         BE    *+14                                                             
         CLC   PERLIST,0(R4)                                                    
         BNL   *+10                                                             
         MVC   0(1,R4),PERLIST                                                  
         CLC   PERLIST,1(R4)                                                    
         BNH   *+10                                                             
         MVC   1(1,R4),PERLIST                                                  
         CLI   0(R2),C'-'          TEST FOR RANGE                               
         BNE   VPER5               MAKE SURE IT FALLS IN REQUEST PERIOD         
*                                                                               
         LA    R2,1(R2)                                                         
         BAS   RE,VPERNUM          YES-VALIDATE SECOND NUMBER                   
         LTR   R0,R0                                                            
         BZ    VPER9               INVALID                                      
         CR    R0,R3               TEST NOT TOO HIGH                            
         BH    VPER9                                                            
         LLC   R1,PERLIST                                                       
         CR    R0,R1               TEST HIGHER THAN FIRST                       
         BNH   VPER9                                                            
         LR    RE,R0                                                            
         SR    RE,R1               BUT NO MORE THAN 15 MORE                     
         CHI   RE,15                                                            
         BH    VPER9                                                            
         LA    R1,1(R1)            GENERATE A LIST OF NUMBERS                   
         LA    RE,PERLIST                                                       
*                                                                               
VPER4    LA    RE,1(RE)                                                         
         STC   R1,0(RE)                                                         
         LA    R1,1(R1)                                                         
         CR    R1,R0                                                            
         BNH   VPER4                                                            
         CLC   0(1,RE),1(R4)       UPDATE HIGHEST PERIOD NUMBER                 
         BNH   *+10                                                             
         MVC   1(1,R4),0(RE)                                                    
***                                                                             
* MAKE SURE DATE FILTER IS VALID FOR REQUEST PERIOD!                            
***                                                                             
VPER5    L     RF,ATWA                                                          
         CLC   =C'REP',CONACT-T204FFD(RF)                                       
         BNE   VPERX               ONLY CHANGE ON ACTION REPORT                 
         OC    ARFPBLK,ARFPBLK     RFP MODE?                                    
         BNZ   VPERX               YES - SKIP LEN CHECK                         
         BRAS  RE,PERFIX           INVALID FILTER?                              
         BE    VPERX               NO, ITS OK                                   
         XC    PERLIST,PERLIST     RESET PERLIST                                
         MVC   0(2,R4),FULL        RESTORE LOW & HIGH DATES                     
         LR    R2,R5               POINT TO DATA FIELD OF SCANNER BLOCK         
         B     VPER00              AND GO THROUGH DATE VALIDATION AGAIN         
*                                                                               
VPER9    MVI   PERLIST,X'FF'                                                    
*                                                                               
VPERX    CLI   PERFLAG,C'Y'                                                     
         JNE   XITEQU                                                           
         L     R6,ALASTCOL         A(FIELD HEADER)                              
         MVC   8(32,R6),ELEM       DON'T CHANGE IT ONLINE!                      
         J     XITEQU                                                           
*                                                                               
VPERXNE  J     XITNEQ                                                           
         SPACE 1                                                                
VPERNUM  SR    R0,R0                                                            
VPERNUM2 CLI   0(R2),C' '                                                       
         BER   RE                                                               
         CLI   0(R2),C'-'                                                       
         BER   RE                                                               
         CLI   0(R2),C'0'                                                       
         BL    VPERNUM4                                                         
         CLI   0(R2),C'9'                                                       
         BH    VPERNUM4                                                         
         MHI   R0,10                                                            
         LLC   R1,0(R2)                                                         
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         AR    R0,R1                                                            
         LA    R2,1(R2)                                                         
         B     VPERNUM2                                                         
VPERNUM4 SR    R0,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE THE PERIOD ON THE SCREEN SO PEOPLE WHO DON'T HAVE ACCESS TO  *         
* TO CHANGE A FORMATTED REPORT DON'T CRY THAT THEY GET AN ERROR       *         
* MESSAGE THAT THE PERIOD FILTER DOES NOT MATCH THE REQUEST PERIOD    *         
*                                                                     *         
* INPUT : R4 = Y/HY/Q/M/W/D LOW END                                   *         
*         R5 = A(SCANNER BLOCK)+12                                    *         
*                                                                     *         
* OUTPUT: R2 = A(FIELD HEADER) - IF CHANGED, CC=NEQ                   *         
***********************************************************************         
PERFIX   NTR1                                                                   
*                                                                               
         L     R2,ALASTCOL         A(FIELD HEADER)                              
         LA    R2,8(R2)            POINT TO FIRST CHARACTER ON COLUMN           
         LLC   R3,1(R4)            Y/HY/Q/M/W/D HIGH END                        
         LR    R4,R5               SCANNER BLOCK+12                             
         SHI   R4,12               SCANNER BLOCK                                
*                                                                               
         GOTO1 PERVERT,DMCB,SBQREQST,SBQREQND  CHECK PERIOD LENGTH              
*                                                                               
PF02     BAS   RE,TESTDATE         DATE FILTER WITHIN REQ PERIOD?               
         BH    PFXEQU              YES, DONE                                    
*                                                                               
         LLC   R6,0(R4)            L' DATE FILTER                               
         BCTR  R6,0                -1 FOR EX                                    
*                                                                               
PF03     CLI   0(R2),C','          DELIMITER?                                   
         BNE   PF03A               NO, BUMP                                     
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R2),12(R4)      FOUND INVALID DATE FILTER?                   
         BE    PF03B               YES - CHANGE IT                              
PF03A    LA    R2,1(R2)            NO  - BUMP                                   
         B     PF03                                                             
*                                                                               
PF03B    BAS   RE,GETLENS          STORE NUM OF DIGITS IN HALF & HALF+1         
         BAS   RE,GENPER           CHANGE DATE TO MAX ALLOWED BY REQ            
         B     PFXNEQ              INDICATE THAT WE CHANGED THE DATE            
*                                                                               
PFXEQU   CR    RB,RB                                                            
         J     PFXIT                                                            
*                                                                               
PFXNEQ   LTR   RB,RB                                                            
*                                                                               
PFXIT    XIT1                                                                   
***********************************************************************         
* SEE IF THE HIGHEST DATE IN OUR DATE RANGE IS <= TO REQUEST PERIOD   *         
*                                                                     *         
* INPUT - R4     = A(SCANNER BLOCK)                                   *         
*       - R3     = MAX NUMBER OF Q/M/W/D                              *         
*       - R1     = A(DMCB) = HAS D/W/M/Y FROM PERVERT                 *         
*                                                                     *         
* OUTPUT- CC     = IF HIGH THEN DATE FILTER IS VALID                  *         
***********************************************************************         
TESTDATE NTR1                                                                   
         SR    R5,R5                                                            
         LA    R6,14(R4)           POINT TO 2ND DIGIT / C'-' / NOTHING          
         CLI   12(R4),C'H'         HY?                                          
         BNE   *+8                 NO                                           
         LA    R6,1(R6)            POINT TO 2ND DIGIT / C'-' / NOTHING          
         BCTR  R3,0                                                             
*&&DO                                                                           
         CLI   12(R4),C'D'         DAYS?                                        
         BE    TD00A               YES                                          
         CLI   12(R4),C'W'         WEEKS?                                       
         BNE   TD00                NO                                           
*                                                                               
         OC    10(2,R1),10(R1)     ANY REMAINDER?                               
         BZ    TD00                NO, HAVE EXACT NUMBER WEEKS                  
         ICM   R5,3,10(R1)         GET REMAINDER OF (DAYS/7)                    
         CHI   R5,2                HAVE AT LEAST 2 DAYS LEFT OVER?              
         BL    TD00A               NO, CANNOT GIVE EXTRA WEEK                   
*                                                                               
TD00     BCTR  R3,0                NO...ALL BETS ARE OFF                        
*&&                                                                             
TD00A    CLI   12(R4),C'Y'         YEAR?                                        
         BNE   TD00B               NO                                           
         MHI   R3,12               GET NUMBER OF MONTHS                         
         B     TD03                                                             
*                                                                               
TD00B    CLI   12(R4),C'H'         HY?                                          
         BNE   TD01                NO                                           
         MHI   R3,6                GET NUMBER OF MONTHS                         
         B     TD03                                                             
*                                                                               
TD01     CLI   12(R4),C'Q'         QUARTERS?                                    
         BNE   TD02                NO                                           
         MHI   R3,3                GET NUMBER OF MONTHS                         
         B     TD03                                                             
*                                                                               
TD02     CLI   12(R4),C'M'         MONTH?                                       
         BE    TD03                YES                                          
*                                                                               
         CLI   12(R4),C'W'         WEEK?                                        
         BNE   TD04                NO, MUST BE DAYS                             
         MHI   R3,7                GET NUMBER OF DAYS                           
         B     TD04                                                             
*                                                                               
TD03     ICM   R5,3,14(R1)         NUMBER OF MONTHS INCLUSIVE                   
         B     TD05                                                             
*                                                                               
TD04     ICM   R5,3,8(R1)          NUMBER OF DAYS IN REQ PERIOD                 
*                                                                               
TD05     CR    R5,R3               HAVE AT LEAST 1 DAY MORE?                    
         J     XIT                                                              
***********************************************************************         
* FIND THE NUMBER OF DIGITS IN THE FIRST & SECOND NUMBERS OF THE DATE *         
* FILTERS AND STORE THEM IN HALF AND HALF+1                           *         
*                                                                     *         
* INPUT - R4   = A(SCANNER BLOCK)                                     *         
* OUTPUT- HALF = STORES NUMBER OF DIGITS IN DATE RANGE                *         
***********************************************************************         
GETLENS  DS    0H                                                               
*                                                                               
         XC    HALF,HALF                                                        
         LA    R6,13(R4)                                                        
         CLI   12(R4),C'H'         HALF YEAR?                                   
         BNE   *+8                 NO                                           
         LA    R6,1(R6)                                                         
         XR    RF,RF                                                            
*                                                                               
GL00     CLI   0(R6),X'F0'         VALID NUMERIC?                               
         BL    GL01                NO                                           
         CLI   0(R6),X'F9'         VALID NUMERIC?                               
         BH    GL01                NO                                           
         IC    RF,HALF                                                          
         AHI   RF,1                                                             
         STC   RF,HALF                                                          
         LA    R6,1(R6)                                                         
         B     GL00                                                             
*                                                                               
GL01     CLI   0(R6),C'-'          REQUESTED A RANGE?                           
         BNE   GLX                 NO                                           
         LA    R6,1(R6)                                                         
*                                                                               
GL02     CLI   0(R6),X'F0'         VALID NUMERIC?                               
         BL    GLX                 NO                                           
         CLI   0(R6),X'F9'         VALID NUMERIC?                               
         BH    GLX                 NO                                           
         IC    RF,HALF+1                                                        
         AHI   RF,1                                                             
         STC   RF,HALF+1                                                        
         LA    R6,1(R6)                                                         
         B     GL02                                                             
*                                                                               
GLX      BR    RE                                                               
***********************************************************************         
* THE COLUMN DATE FILTER EDIT IS DONE HERE. THIS WILL CHANGE THE DATE *         
* FILTER TO THE HIGHEST FILTER ALLOWED BY THE REQUEST. IF A RANGE     *         
* (M5-8) IS REQUESTED AND THERE ARE ONLY 3 MONTHS IN THE REQUEST      *         
* PERIOD, THEN IT WILL BE CHANGED TO M3 AND THE RANGE WILL BE DELETED *         
* AND ANYTHING AFTER THAT SHIFTED OVER TO THE LEFT                    *         
*                                                                     *         
* INPUT - 0(R1)   = OUTPUT FROM PERVERT                               *         
*       - 0(R2)   = 1 BYTE BEFORE Y/HY/Q/M/W/D ON SCREEN              *         
*       - 0(R4)   = SCANNER BLOCK (DATE FILTER ENTRY)                 *         
*       - HALF    = NUMBER OF DIGITS IN FIRST DATE RANGE              *         
*       - HALF+1  = NUMBER OF DIGITS IN SECOND DATE RANGE             *         
*                                                                     *         
* OUTPUT- 0(R2)   = CHANGED DATE FILTER                               *         
*       - 0(R4)   = CHANGED SCANNER BLOCK (DATE FILTER ENTRY)         *         
***********************************************************************         
GENPER   NTR1                                                                   
*                                                                               
         SR    R5,R5                                                            
         LLC   R6,HALF             L'FIRST DIGIT IN DATE FILTER                 
         LA    R3,2(R2)            FIRST DIGIT ON SCREEN                        
         CLI   1(R2),C'H'          HY?                                          
         BNE   *+8                 NO                                           
         LA    R3,1(R3)            YES - THIS IS A(FIRST DIGIT)                 
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R3,DUB              FIRST NUMBER IN BINARY                       
*                                                                               
         MVC   BYTE,HALF           N'DIGITS FIRST HALF OF DATE RANGE            
         LA    R2,2(R2)            POINT TO FIRST Y/Q/M/W/D                     
*                                                                               
         CLI   12(R4),C'Y'         YEAR                                         
         BNE   GP00                NO                                           
         LA    R6,12               NUMBER OF MONTHS IN 1 YEAR                   
         MHI   R3,12               GET NUMBER OF MONTHS                         
         ICM   R5,3,14(R1)         NUMBER OF MONTHS INCLUSIVE                   
         CR    R5,R3               REQ PERIOD TOO SMALL FOR 1ST YEAR?           
         BNH   GP05                YES, CHANGE THE FIRST YEAR                   
         B     GP04B               NO, CHANGE 2ND YEAR                          
*                                                                               
GP00     CLI   12(R4),C'H'         HY?                                          
         BNE   GP01                NO                                           
         LA    R2,1(R2)            POINT TO FIRST HY                            
         LA    R6,6                NUMBER OF MONTHS IN 1 HALF YEAR              
***      BCTR  R3,0                                                             
         MHI   R3,6                GET NUMBER OF MONTHS                         
         ICM   R5,3,14(R1)         NUMBER OF MONTHS INCLUSIVE                   
         CR    R5,R3               REQ PERIOD TOO SMALL FOR 1ST HY?             
         BNH   GP05                YES, CHANGE THE FIRST HY                     
         B     GP04B               NO, CHANGE 2ND HY                            
*                                                                               
GP01     CLI   12(R4),C'Q'         QUATER?                                      
         BNE   GP02                NO                                           
         LA    R6,3                NUMBER OF MONTHS IN 1 QUARTER                
***      BCTR  R3,0                                                             
         MHI   R3,3                GET NUMBER OF MONTHS                         
         ICM   R5,3,14(R1)         NUMBER OF MONTHS INCLUSIVE                   
         CR    R5,R3               HAVE AT LEAST 1 DAY MORE?                    
         BNH   GP05                NO, CHANGE FIRST QUARTER                     
         B     GP04B               YES, CHANGE 2ND QUARTER                      
*                                                                               
GP02     CLI   12(R4),C'M'         MONTHS?                                      
         BNE   GP03                NO                                           
         LA    R6,1                NUMBER OF MONTHS IN 1 MONTH                  
         ICM   R5,3,14(R1)         NUMBER OF MONTHS INCLUSIVE                   
         CR    R5,R3               HAVE AT LEAST 1 DAY MORE?                    
         BNH   GP05                NO, CHANGE FIRST MONTH                       
         B     GP04B               YES, CHANGE 2ND MONTH                        
*                                                                               
GP03     CLI   12(R4),C'W'         WEEKS?                                       
         BNE   GP04                NO                                           
         LA    R6,7                NUMBER OF DAYS IN A WEEK                     
         MHI   R3,7                GET NUMBER OF DAYS                           
         ICM   R5,3,8(R1)          NUMBER OF DAYS IN REQ PERIOD                 
         CHI   R5,1                HAVE ONLY 1 DAY?                             
         BE    GP05                YES, CHANGE THE FIRST WEEK FILTER            
***      CHI   R3,7                TRYING TO CHANGE WEEK1?                      
***      BE    GP04B               YES, W2 ALWAYS VALID!                        
         CR    R5,R3               HAVE AT LEAST 1 DAY MORE?                    
         BNH   GP05                NO, CHANGE FIRST WEEK                        
         B     GP04B               YES, CHANGE 2ND WEEK                         
*                                                                               
GP04     LA    R6,1                NUMBER OF DAYS IN A DAY                      
         ICM   R5,3,8(R1)          NUMBER OF DAYS IN REQ PERIOD                 
         CR    R5,R3               HAVE AT LEAST 1 DAY MORE?                    
         BNH   GP05                NO, CHANGE FIRST DAY                         
*                                                                               
GP04B    MVC   BYTE,HALF+1         N'DIGITS SECOND HALF OF DATE RANGE           
         LLC   RF,HALF             POINT TO SECOND HALF OF DATE RANGE           
         LA    R2,1(RF,R2)                                                      
***                                                                             
* CHANGE NUMBER POINTED AT BY R2                                                
***                                                                             
GP05     BAS   RE,MAXPER           CHANGE THE PERIOD                            
         LLC   RF,HALF                                                          
         AR    RF,R2                                                            
         CLI   0(RF),C'-'          RANGE REQUESTED?                             
         BNE   *+8                 NO, EXIT                                     
         BAS   RE,PARSPER          PARSE THE RANGE                              
         L     R2,ALASTCOL         A(FIELD HEADER)                              
         OI    6(R2),X'80'         TRANSMIT THE CHANGE                          
*                                                                               
GPX      J     XIT                                                              
***********************************************************************         
* PARSE THE PERIOD AND MOVE ANYTHING AFTER THE PERIOD FILTER OVER     *         
*                                                                     *         
* INPUT - 0(R2) = FIRST DIGIT OF 1ST HALF OF DATE FILTER              *         
*       - R4    = A(SCANNER ENTRY)                                    *         
*       - HALF  = NUMBER OF DIGITS IN FIRST DATE RANGE                *         
*       - HALF+1= NUMBER OF DIGITS IN SECOND DATE RANGE               *         
*                                                                     *         
* OUTPUT- 0(R2) = DATE FILTER NO LONGER A RANGE                       *         
*       - 0(R4) = CHANGED SCANNER BLOCK (DATE FILTER ENTRY)           *         
***********************************************************************         
PARSPER  DS    0H                                                               
*                                                                               
         LLC   R3,HALF             N' DIGITS OF 1ST HALF OF PER FILTER          
         AR    R3,R2               POINT TO THE C'-'                            
         LLC   R6,HALF+1                                                        
         LA    R6,1(R3,R6)                                                      
         CLI   0(R6),X'40'         ANYTHING AFTER PERIOD FILTER?                
         BNH   PP01                NO                                           
*                                                                               
         L     R6,ALASTCOL         A(FIELD HEADER)                              
         LLC   R1,5(R6)            INPUT LENGTH                                 
         LA    R1,8(R1,R6)         1 BYTE AFTER LAST CHARACTER                  
         LLC   R5,HALF+1           N' DIGITS IN 2ND HALF OF PER FILTER          
         LA    R5,1(R5,R3)         BUMP TO THE COMMA                            
         SR    R1,R5               L' OF ENTRIES AFTER DATE FILTER              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R5)       MOVE EVERYTHING DOWN OVER C'-'               
*                                                                               
         LA    R3,1(R3,R1)                                                      
PP01     LLC   R1,HALF+1           CLEAR FOR NUMBER OF DIGITS + 1               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=C'          '  CLEAR GARBAGE ON SCREEN                  
*                                                                               
PP02     LLC   R1,0(R4)            L' DATE ENTRY IN SCANNER BLOCK               
         LLC   RF,HALF+1                                                        
         SR    R1,RF                                                            
         BCTR  R1,0                ACCOUNT FOR THE C'-'                         
         STC   R1,0(R4)            UPDATE LENGTH IN SCANNER BLOCK               
         LA    R1,12(R4,R1)                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),=C'          '  CLEAR GARBAGE IN SCANNER BLOCK           
*                                                                               
PPX      BR    RE                                                               
***********************************************************************         
* REPLACE THE PERIOD FILTER ON THE SCREEN BASED ON THE REQUEST PERIOD *         
*                                                                     *         
* INPUT -  R2 - A(1ST DIGIT OF DATE FILTER TO OVERWRITE)              *         
*       -  R4 - A(SCANNER BLOCK)                                      *         
*       -  R5 - MAX DATE FILTER ALLOWED BY THE REQUEST PERIOD         *         
*       -  R6 - NUMBER OF DAYS/MONTHS IN 1 UNIT OF FILTER (Q=3 MONTHS)*         
*                                                                     *         
* OUTPUT-  R2 - MAXIMUM DATE FILTER BASED ON REQUEST PERIOD           *         
*       -  R4 - CHANGED SCANNER BLOCK (DATE FILTER ENTRY)             *         
***********************************************************************         
MAXPER   NTR1                                                                   
*                                                                               
         CLI   12(R4),C'D'         DOING DAYS?                                  
         BNE   MP00                NO                                           
         LR    R1,R5               DAYS ARE AS-IS                               
         B     MP02B                                                            
*                                                                               
MP00     LA    R1,1                COUNTER                                      
         CR    R5,R6               REQ PERIOD <= 1 UNIT?                        
         BNH   MP02                YES                                          
*                                                                               
MP01     AHI   R1,1                ADD 1 UNIT TO OUTPUT ONTO SCREEN             
         SR    R5,R6               PUT OUT THE MAXIMUM DATE FILTER              
         CR    R5,R6               REACHED MAX?                                 
         BH    MP01                NO                                           
*                                                                               
MP02     CLI   12(R4),C'W'         WEEKS?                                       
         BNE   MP02A               NO                                           
         CHI   R5,2                HAVE AT LEAST 2 DAYS LEFT OVER?              
         BL    MP02B               NO                                           
*                                                                               
*P02A    AHI   R1,1                M/Q/HY/Y ALLOWED 1 MORE                      
MP02A    DS    0H                                                               
*                                                                               
MP02B    LR    R3,R2                                                            
*                                                                               
MP02C    CLI   0(R3),C'0'                                                       
         BNE   *+12                                                             
         LA    R3,1(R3)                                                         
         B     MP02C                                                            
*                                                                               
         CLI   1(R3),X'F0'     1 SIGNIFICANT CHAR?                              
         BL    MP02D           YES                                              
         CLI   1(R3),X'F9'     1 SIGNIFICANT CHAR?                              
         BNH   MP02E           NO                                               
*                                                                               
MP02D    EDIT  (R1),(1,0(R3)),ZERO=NOBLANK,FILL=0                               
         B     MP03                                                             
*                                                                               
MP02E    EDIT  (R1),(2,0(R3)),ZERO=NOBLANK,FILL=0                               
*                                                                               
MP03     BCTR  R2,0                                                             
         CLI   0(R2),C'-'          UPDATED 2ND HALF OF DT RANGE FILTER?         
         BNE   MP05                NO                                           
         LLC   R3,HALF                                                          
         SR    R2,R3               BACK UP N'(1ST HALF DIGITS)                  
         BCTR  R2,0                +1 FOR THE C'-'                              
*                                                                               
MP05     CLI   12(R4),C'H'         HY?                                          
         BNE   *+6                 NO                                           
         BCTR  R2,0                                                             
         LLC   R1,0(R4)            L' DATE ENTRY IN SCANNER BLOCK               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R4),0(R2)      MOVE NEW DT ENTRY IN SCANNER BLOCK           
*                                                                               
MPX      J     XIT                                                              
*                                                                               
* ROUTINE TO GENERATE A REPORT SEQUENCE ROW                                     
* INPUT  : R2=REPORT SEQUENCE NUMBER                                            
         SPACE 1                                                                
LGENRPTS NTR1  ,                   LOCAL ENTRY                                  
*                                                                               
GENRPTS  LA    R4,BLOCK                                                         
         MVI   0(R4),6             VALIDATE RPTSEQ                              
         MVC   12(6,R4),=C'RPTSEQ'                                              
         BRAS  RE,VROWDRON                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   OFFLINE,C'Y'        GENERATE ROW IF OFFLINE                      
         JNE   XIT                                                              
         MVI   DRLITLNI,1                                                       
         STC   R2,DRLITI           REPORT SEQUENCE NUMBER                       
         MVI   DRFLAGO,0           NO OUTPUT                                    
         BRAS  RE,GROWDRON                                                      
         J     XIT                                                              
         EJECT                                                                  
* ROUTINE TO GENERATE A TEXT ROW OR COLUMN                                      
*                                                                               
GENTEXT  SR    R1,R1                                                            
         ICM   R1,1,1(R4)                                                       
         BNZ   *+8                                                              
         LA    R1,1                                                             
         CH    R1,=Y(L'DRLITO)                                                  
         BL    *+8                                                              
         LH    R1,=Y(L'DRLITO)                                                  
         STC   R1,DRLENO                                                        
         STC   R1,DRLITLNO                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRLITO(0),22(R4)                                                 
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE DDVALMNMX                                                      
         EJECT                                                                  
* FIELDS USED ONLY OFFILE                                                       
*                                                                               
         SPACE 1                                                                
EDITLIST DC    XL64'00'                                                         
SAVTOTYP DC    X'00'                                                            
NROWS    DS    XL1                                                              
PERFLAG  DS    XL1                                                              
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
BLNKS    DC    CL132' '                                                         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
MACTABLE DS    0D                                                               
MACENTRY DS    CL20                                                             
         DC    CL8'MCBUY   ',A(MCESTLIN),A(MCPROG),X'00000000'                  
         DC    CL8'MCBUYC  ',A(MCESTLIN),A(MCBYDOLR),X'00000000'                
         DC    CL8'MCBUYS  ',A(MCLIN),A(MCDPTLEN),X'00000000'                   
         DC    CL8'MCAFFID ',A(MCADATE),A(MCATIME),X'00000000'                  
         DC    CL8'MCDPT   ',A(MCDPTCD2),A(MCLEN2),X'00000000'                  
         DC    CL8'MCSTA16 ',A(MCSTA16),A(MCSTAN16),X'00000000'                 
         DC    CL8'MCSTANM ',A(MCSTA),A(MCSTANAM),X'00000000'                   
         DC    CL8'MCMKTSTA',A(MCMKT),A(MCSTANAM),X'00000000'                   
         DC    X'FF'                                                            
         SPACE 1                                                                
MCESTLIN DC    CL8'ESTLIN  '                                                    
MCBYDTS  DC    CL8'BYDATES '                                                    
MCBYWKS  DC    CL8'BYWKS   '                                                    
MCDPTCD  DC    CL8'DPTCD   '                                                    
MCLEN    DC    CL8'LEN     '                                                    
MCROT    DC    CL8'ROT     '                                                    
MCTIMES  DC    CL8'TIMES   '                                                    
MCPROG   DC    CL8'PROG    '                                                    
MCBYDOLR DC    CL8'BYDOLR  '                                                    
*                                                                               
MCLIN    DC    CL8'LINE    '                                                    
         DC    CL8'ROT     '                                                    
         DC    CL8'PROG    '                                                    
         DC    CL8'TIMES   '                                                    
MCDPTLEN DC    CL8'DPTLEN  '                                                    
*                                                                               
MCADATE  DC    CL8'ADATE   '                                                    
MCADAY   DC    CL8'ADAY    '                                                    
MCATIME  DC    CL8'ATIME   '                                                    
*                                                                               
MCDPTCD2 DC    CL8'DPTCD   '                                                    
         DC    CL8'DPT     '                                                    
MCLEN2   DC    CL8'LEN     '                                                    
*                                                                               
MCMKT    DC    CL8'MKT     '                                                    
MCSTA    DC    CL8'STA     '                                                    
MCSTANAM DC    CL8'STANAME '                                                    
*                                                                               
MCSTA16  DC    CL8'STA16   '                                                    
MCSTAN16 DC    CL8'STANAME '                                                    
*                                                                               
         DROP  R6,R7,RA,RB                                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE SQAD OPTION                              *         
*              FORMAT - 1QYY-MMMYY                                    *         
*                       1Q/YY-MMMYY                                   *         
*                       YYQ1-MMMYY                                    *         
*                       Q1/YY-MMMYY                                   *         
*              FIRST PART IS QUARTER AND SECOND IS RELEASE            *         
*NTRY    R3==>SQAD SAVEAREA                                           *         
*              YYQQ - QUARTER                                         *         
*              YYMM - RELEASE MONTH                                   *         
*        R4==>SCANNER BLOCK                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SQDVAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    0(4,R3),0(R3)       CHECK IF OPTION ALREADY ENTERED              
         BNZ   SQDVAL1E                                                         
*                                                                               
         XC    WORK(32),WORK       INIT WORKAREA                                
*                                                                               
         LA    R1,22(R4)           POINT TO START OF SQAD OPTION                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R4)          GET LENGTH OF SQAD OPTION                    
         BZ    SQDVAL2E            NO INPUT                                     
*                                                                               
*        FIND FIRST NON-NUMERIC IN STRING                                       
*                                                                               
         LR    R5,R1               SAVE START                                   
*                                                                               
         CLI   0(R1),C'0'          CHECK IF NOT NUMERIC                         
         BL    *+16                  NOT NUMERIC                                
         AHI   R1,1                BUMP POINTER                                 
         BCT   R0,*-12                                                          
         B     SQDVAL2E              INVALID SQAD OPTION                        
*                                                                               
         CLI   0(R1),C'Q'          IF STOP CH IS 'Q' FOR QUARTER                
         BNE   SQDVAL30                                                         
*                                                                               
         CR    R5,R1               CHECK IF Q BEFORE OR AFTER QTR #             
         BE    SQDVAL10              BEFORE                                     
*                                                                               
*        FORMAT 1QYY OR 1Q/99                                                   
*                                                                               
         BAS   RE,SQDVQTR            VALIDATE AS QUARTER                        
*                                                                               
         AHI   R1,1                BUMP PAST 'Q'                                
         BCT   R0,*+8                                                           
         B     SQDVAL2E               NO MORE INPUT - YY REQUIRED               
*                                                                               
         CLI   0(R1),C'/'          BY PASS '/' IF FORMAT 1Q/99                  
         BNE   *+16                                                             
         LA    R1,1(R1)               BUMP PAST '/'                             
         BCT   R0,*+8                                                           
         B     SQDVAL2E               NO MORE INPUT - YY REQUIRED               
*                                                                               
         B     SQDVAL15            GO HANDLE YEAR                               
*                                                                               
*        FORMAT Q1/YY                                                           
*                                                                               
SQDVAL10 DS    0H                  ASSUME OF THE FORM Q1/YY                     
*                                                                               
         AHI   R1,1                BUMP PAST 'Q'                                
         BCT   R0,*+8              UPDATE LENGTH COUNTER                        
         B     SQDVAL2E               NO MORE INPUT - YY REQUIRED               
*                                                                               
         LR    R5,R1               SAVE START OF QUARTER                        
*                                                                               
         CLI   0(R1),C'/'          FIND '/' IF FORMAT Q1/99                     
         BE    *+16                                                             
         AHI   R1,1                   BUMP POINTER                              
         BCT   R0,*+8                                                           
         B     SQDVAL2E               NO MORE INPUT - YY REQUIRED               
*                                                                               
         BAS   RE,SQDVQTR          VALIDATE QUARTER                             
*                                                                               
         AHI   R1,1                BUMP PAST '/'                                
         BCT   R0,*+8                                                           
         B     SQDVAL2E               NO MORE INPUT - YY REQUIRED               
*                                                                               
*        VALIDATE YEAR IN FORMATS WHEN YEAR IS LAST                             
*        1Q99,1Q/99,Q1/99                                                       
*                                                                               
SQDVAL15 DS    0H                                                               
*                                                                               
         LR    R5,R1               SAVE YEAR STARTING PTR                       
*                                                                               
         CLI   0(R1),C'-'          FIND END OF YEAR                             
         BE    *+16                  NO                                         
         AHI   R1,1                BUMP POINTER                                 
         BCT   R0,*-12                                                          
         B     SQDVAL2E              INVALID SQAD OPTION                        
*                                                                               
         BAS   RE,SQDVYR           VALIDATE YEAR                                
*                                                                               
         B     SQDVAL50            GO VALIDATE RELEASE                          
*                                                                               
*        FORMAT YYQ1 YY/Q1                                                      
*                                                                               
SQDVAL30 DS    0H                                                               
*                                                                               
         BAS   RE,SQDVYR           VALIDATE YEAR                                
*                                                                               
         CLI   0(R1),C'/'          BYPASS '/' OF FORMAT YY/Q1                   
         BNE   *+16                  NO                                         
         AHI   R1,1                BUMP POINTER                                 
         BCT   R0,*-12                                                          
         B     SQDVAL2E              INVALID SQAD OPTION                        
*                                                                               
         CLI   0(R1),C'Q'          MUST NOW BE DOING QUARTER                    
         BNE   SQDVAL2E              NO                                         
*                                                                               
         AHI   R1,1                BYPASS SEPERATOR                             
         BCT   R0,*+8                DECREMENT INPUT COUNTER                    
         B     SQDVAL2E              NO YEAR                                    
*                                                                               
         LR    R5,R1               SAVE START OF QUARTER                        
*                                                                               
         CLI   0(R1),C'-'          FIND END OF QUARTER                          
         BE    *+16                  NO                                         
         AHI   R1,1                BUMP POINTER                                 
         BCT   R0,*-12                                                          
         B     SQDVAL2E              INVALID SQAD OPTION                        
*                                                                               
         BAS   RE,SQDVQTR          VALIDATE QUARTER                             
*                                                                               
*        VALIDATE RELEASE AS A MONTH/YEAR                                       
*                                                                               
SQDVAL50 DS    0H                                                               
*                                                                               
         AHI   R1,1                BUMP PAST '-'                                
         BCT   R0,*+8              ADJUST INPUT COUNTER                         
         B     SQDVAL2E            NO RELEASE AVAILABLE                         
*                                                                               
         LR    R5,R1               SAVE STARTING POINT                          
*                                                                               
         GOTO1 DATVAL,DMCB,(2,0(R5)),WORK  VALIDATE AS MMMYY                    
         OC    DMCB(4),DMCB        CHECK FOR ERRORS                             
         BZ    SQDVAL2E                                                         
*                                                                               
         GOTO1 DATCON,DMCB,WORK,(3,WORK+8) CONVERT TO YMD BINARY                
*                                                                               
         MVC   2(2,R3),WORK+8   SAVE RELEASE MONTH                              
*                                                                               
SQDVALX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
SQDVAL1E DS    0H                                                               
*                                                                               
         LHI   RF,SQDDUPOP         DUPLICATE SQAD OPTION                        
         B     SQDVALER                                                         
*                                                                               
SQDVAL2E DS    0H                                                               
*                                                                               
         LHI   RF,SQDINVOP         INVALID SQAD OPTION                          
*                                                                               
SQDVALER DS    0H                                                               
*                                                                               
         STCM  RF,3,GTMSGNO        PASS TO GETMSG                               
         GOTO1 CURSERR                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE QUARTER 1-4                                                   
*                                                                               
*        R1==> END   OF QUARTER INPUT                                           
*        R5==> START OF QUARTER INPUT                                           
*        R3==> SQAD QUARTER SAVEAREA                                            
*                                                                               
SQDVQTR  DS    0H                                                               
*                                                                               
         LR    RF,R1               CALCULATE INPUT LENGTH                       
         SR    RF,R5                                                            
*                                                                               
         CHI   RF,1                IF INPUT IS ONE LONG                         
         BNE   SQDVQT1E                                                         
*                                                                               
         CLI   0(R5),C'1'             MUST BE 1 TO 4                            
         BL    SQDVQT1E                                                         
         CLI   0(R5),C'4'                                                       
         BH    SQDVQT1E                                                         
*                                                                               
         PACK  DUB,0(1,R5)         PACK QUARTER                                 
         CVB   RF,DUB              CVB                                          
         STCM  RF,1,1(R3)          SAVE QUARTER                                 
*                                                                               
SQDVQTRX DS    0H                                                               
         BR    RE                  RETURN                                       
*                                                                               
SQDVQT1E DS    0H                                                               
*                                                                               
         LHI   RF,SQDINVOP         INVALID SQAD OPTION                          
         STCM  RF,3,GTMSGNO        PASS TO GETMSG                               
         GOTO1 CURSERR                                                          
*                                                                               
*        VALIDATE YEAR USING DATVAL                                             
*                                                                               
*        R1==> END   OF YEAR INPUT                                              
*        R5==> START OF YEAR INPUT                                              
*        R3==> SQAD QUARTER SAVEAREA                                            
*                                                                               
SQDVYR   NTR1  LABEL=*                                                          
*                                                                               
         LR    RF,R1               CALCULATE INPUT LENGTH                       
         SR    RF,R5                                                            
*                                                                               
         CHI   RF,2                MUST BE 2 LONG                               
         BNE   SQDVYR1E                                                         
*                                                                               
         LR    RE,R5               START OF YEAR                                
*                                                                               
*        INPUT MUST BE NUMERIC                                                  
*                                                                               
SQDVYRLP DS    0H                                                               
*                                                                               
         CLI   0(RE),C'0'          YEAR MUST BE ALL NUMERIC                     
         BL    SQDVYR1E                                                         
         CLI   0(RE),C'9'                                                       
         BH    SQDVYR1E                                                         
*                                                                               
SQDVYRCN DS    0H                                                               
         AHI   RE,1                BUMP TO NEXT DIGIT OF YEAR                   
         BCT   RF,SQDVYRLP                                                      
*                                                                               
SQDVYRDN DS    0H                                                               
*                                                                               
         MVC   WORK(4),=C'JAN/'    USE JAN AS GENERIC MONTH                     
         MVC   WORK+4(2),0(R5)     ADD YEAR                                     
*                                                                               
         GOTO1 DATVAL,DMCB,(2,WORK),WORK+8 VALIDATE YEAR                        
         OC    DMCB(4),DMCB        CHECK FOR ERRORS                             
         BE    SQDVYR1E                                                         
*                                                                               
         GOTO1 DATCON,DMCB,WORK+8,(3,WORK)  CONVERT TO BINARY                   
*                                                                               
         MVC   0(1,R3),WORK        SAVE YEAR OF QUARTER                         
*                                                                               
SQDVYRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
SQDVYR1E DS    0H                                                               
*                                                                               
         LHI   RF,SQDINVOP         INVALID SQAD OPTION                          
         STCM  RF,3,GTMSGNO        PASS TO GETMSG                               
         GOTO1 CURSERR                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
         EJECT                                                                  
         SPACE                                                                  
* OPTIONS VALIDATION ROUTINE                                                    
*                                                                               
VALOPT   NTR1  BASE=*,LABEL=*                                                   
         LR    RA,RB               GET SECOND BASE                              
         AHI   RA,4096                                                          
         USING VALOPT+4096,RA                                                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    OPTXEQ                                                           
         MVI   FIELDERR,1                                                       
*         GOTO1 SCANNER,DMCB,(70,(R2)),(10,AIO3),0                              
         GOTO1 SCANNER,DMCB,(70,(R2)),(X'94',AIO3),0                            
         LLC   R0,4(R1)                                                         
         L     R4,AIO3                                                          
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
*                                                                               
OPT2     LA    R3,OPTTABLE                                                      
         CLC   12(8,R4),0(R3)      COMPARE ON FULL LENGTH - NO PARTIALS         
         BE    VOPTGO                                                           
         LA    R3,OPTTABL(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   *-18                                                             
         B     BADOPT                                                           
*                                                                               
VOPTGO   SR    RF,RF                                                            
         ICM   RF,3,10(R3)         ANY FLAG TO SET?                             
         BZ    VOPTGO2                                                          
         LR    RE,RF                                                            
         SLL   RF,20               STRIP OFF BASE REG                           
         SRL   RF,20                 RF = DISPLACEMENT                          
         SRL   RE,12               ISOLATE BASE REG                             
         EX    RE,*+8                RE = BASE REG                              
         B     *+6                                                              
         AR    RF,0                COMPUTE BASE + DISP TO FLAG BYTE             
         OC    0(1,RF),12(R3)      SET FLAG                                     
*                                                                               
VOPTGO2  SR    RF,RF                                                            
         ICM   RF,3,8(R3)          ANY ROUTINE TO BRANCH TO?                    
         BZ    OPTEND               NO                                          
         LR    RE,RF                                                            
         SLL   RF,20               STRIP OFF BASE REG                           
         SRL   RF,20                 RF = DISPLACEMENT                          
         SRL   RE,12               ISOLATE BASE REG                             
         EX    RE,*+6                RE = BASE REG                              
         BR    RF                  GO TO ROUTINE                                
         AR    RF,0                COMPUTE BASE + DISP TO ROUTINE               
*                                                                               
* OPTION VALIDATION ROUTINES                                                    
*                                                                               
OPTBOX   TM    DOWNOPT,GLDLACTV    BOX OPTION                                   
         BO    OPTEND                                                           
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
*                                                                               
OPTLEFT  TM    DOWNOPT,GLDLACTV    LEFT OPTION                                  
         BO    OPTEND                                                           
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
OPTSPACE MVC   SPACOPT,11(R4)      SPACING OPTION                               
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,5                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
*                                  DOWNLOADING OPTION                           
OPTDOWN  TM    OPTIND3,OPTINORQ    DON'T ALLOW DOWN W/NOREQDET                  
         BZ    *+14                                                             
         MVC   GTMSGNO,=Y(NOREQDL)                                              
         J     NEWTRAP                                                          
*                                                                               
         OI    OPTIND3,OPTIFLEM    FORCE FLOAT=-                                
         OI    DOWNOPT2,GLDADDCH   DON'T CHANGE C+ TO 'NUMERIC'                 
*                                                                               
         OI    DOWNOPT,GLDLACTV                                                 
         CLC   16(4,R4),=C'TAPE'   DOWNTAPE OPTION                              
         BNE   *+12                                                             
         OI    DOWNOPT,GLDLALPH+GLDLSTRP+GLDLNOHD+GLDLNOTR                      
         B     OPTEND                                                           
         CLC   16(4,R4),=C'HEAD'   DOWNLOAD HEADLINES OPTION                    
         BNE   *+12                                                             
         OI    DOWNOPT,GLDLHEAD                                                 
         B     OPTEND                                                           
         CLC   16(3,R4),=C'TOT'    DOWNLOAD TOTALS OPTION                       
         BNE   OPTEND                                                           
         OI    DOWNOPT,GLDLTOTS                                                 
         MVI   DOWNCHAR,0                                                       
         NI    DOWNOPT2,255-GLDLTTXT                                            
         CLI   19(R4),C'T'                                                      
         BNE   *+8                                                              
         OI    DOWNOPT2,GLDLTTXT   DOWNLOAD TOTAL ROUTINE TEXT                  
         CLI   1(R4),0                                                          
         BNH   OPTEND                                                           
         MVC   DOWNCHAR,22(R4)     CHARACTER FOR DOWNLOADING TOTALS             
         B     OPTEND                                                           
*                                                                               
OPTWIDE  MVI   WIDTHOPT,C'W'       WIDE PRINTING (165)                          
         B     OPTEND                                                           
*                                                                               
OPTTRACE MVI   TRACEOPT,C'Y'       DRIVER TRACE OPTION                          
         B     OPTEND                                                           
*                                                                               
OPTIOTRC MVI   SBQTRACE,C'Y'       SPOTIO TRACE OPTION                          
         B     OPTEND                                                           
*                                                                               
OPTNAROW MVI   WIDTHOPT,C'N'       NARROW OPTION                                
         B     OPTEND                                                           
*                                                                               
OPTPOL   CLC   22(4,R4),=C'CPP '   POOL OPTION                                  
         BNE   *+12                                                             
         OI    SBQPIND,SBQPOLCP    READ CPP GUIDE FOR POL REQUEST               
         B     OPTEND                                                           
         CLC   22(4,R4),=C'UNA '                                                
         BNE   *+12                                                             
         OI    SBQPIND,SBQPUNA     ONLY READ UNALLOCATED SPOTS                  
         B     OPTEND                                                           
         OI    SBQPIND,SBQPOLSP    REPORT INDIVIDUAL PRODUCTS FOR POL           
         CLC   22(4,R4),=C'SEP '                                                
         BE    OPTEND                                                           
         CLC   22(5,R4),=C'BOTH '                                               
         BNE   BADOPT                                                           
         OI    SBQPIND,SBQPOLAL    REPORT POL AND INDIVIDUAL PRODUCTS           
         B     OPTEND                                                           
OPTSQD2  LA    R3,SBSQDQT2         SQAD2 SAVE AREA                              
         B     OPTSQDX             GO VALIDATE SQAD                             
*                                                                               
OPTSQD3  LA    R3,SBSQDQT3         SQAD3 SAVE AREA                              
         B     OPTSQDX             GO VALIDATE SQAD                             
*                                                                               
OPTSQD4  LA    R3,SBSQDQT4         SQAD4 SAVE AREA                              
         B     OPTSQDX             GO VALIDATE SQAD                             
*                                                                               
OPTSQD1  LA    R3,SBSQDQT1         SQAD1 SAVE AREA                              
OPTSQDX  BRAS  RE,SQDVAL                                                        
         B     OPTEND                                                           
*                                                                               
OPTBK    LA    R3,SBQBOOK          BOOK                                         
         B     OPTBK10                                                          
*                                                                               
OPTBK2   LA    R3,SBQBOOK+4                                                     
         B     OPTBK9                                                           
*                                                                               
OPTBK3   LA    R3,SBQBOOK+8                                                     
         B     OPTBK9                                                           
*                                                                               
OPTBK4   LA    R3,SBQBOOK+12                                                    
         B     OPTBK9                                                           
*                                                                               
OPTBK5   LA    R3,SBQBOOK+16                                                    
         B     OPTBK9                                                           
*                                                                               
OPTBK6   LA    R3,SBQBOOK+20                                                    
         B     OPTBK9                                                           
*                                                                               
OPTBK9   LR    RE,R3                                                            
         SHI   RE,4                                                             
         OC    0(4,RE),0(RE)       PREVIOUS BOOK SET?                           
         BZ    BADOPT2             NO                                           
*                                                                               
OPTBK10  MVC   0(4,R3),=C'ACT '                                                 
         CLC   22(3,R4),=C'ACT '                                                
         BNE   OPTBK30                                                          
*                                                                               
OPTBK20  CLI   1(R4),3             BK=ACT                                       
         BE    OPTEND                                                           
         CLI   1(R4),6             TEST BK=ACT/NN OR BK=ACT-BN                  
         BNE   BADOPT                                                           
         CLI   25(R4),C'/'                                                      
         BNE   OPTBK22                                                          
         CLI   26(R4),C'0'                                                      
         BL    BADOPT                                                           
         CLI   27(R4),C'0'                                                      
         BL    BADOPT                                                           
         MVC   DUB(2),26(R4)       MOVE IN YEAR                                 
         MVC   DUB+2(4),=C'0101'   DUMMY UP MMDD                                
         GOTO1 DATCON,DMCB,(0,DUB),(3,FULL) CONVERT TO BINARY                   
         MVC   3(1,R3),FULL        STORE BINARY YEAR                            
         OI    3(R3),X'80'         INDICATOR FOR PRESENCE OF YEAR               
         B     OPTEND                                                           
*                                                                               
OPTBK22  CLC   25(2,R4),=C'-B'                                                  
         BNE   BADOPT                                                           
         CLI   27(R4),C'1'                                                      
         BL    BADOPT                                                           
         CLI   27(R4),C'4'                                                      
         BH    BADOPT                                                           
         MVC   SBBKAVG,27(R4)                                                   
         B     OPTEND                                                           
*                                                                               
OPTBK30  GOTO1 DATVAL,DMCB,(2,22(R4)),DUB                                       
         ICM   RF,15,0(R1)                                                      
         BZ    BADOPT                                                           
         MVC   0(4,R3),DUB                                                      
         LA    RE,10                                                            
         SR    RE,RF               L'REMAINING FLD                              
         BCTR  RE,0                                                             
         LA    RF,22(R4,RF)        RF=A(1ST BYTE AFTER DATE)                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BLNKS2      ANYTHING THERE?                              
         BE    OPTEND                                                           
*                                                                               
         CLC   0(2,RF),=C'-B'                                                   
         BNE   BADOPT                                                           
         CLI   2(RF),C'1'                                                       
         BL    BADOPT                                                           
         CLI   2(RF),C'4'                                                       
         BH    BADOPT                                                           
         MVC   SBBKAVG,2(RF)                                                    
         SHI   RE,3                                                             
         AHI   RF,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BLNKS2      MAKE SURE REST OF FIELD IS BLANKS            
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
OPTBKUNV LA    R3,SBQUNBK          UNIVERSE BOOK                                
         B     *+8                                                              
OPTUPTBK LA    R3,SBQUPFBK         R3 = SBQUPFBK                                
         XC    WORK,WORK           GENERATE A PSEUDO TWA FLD                    
         SR    R1,R1                                                            
         ICM   R1,1,1(R4)                                                       
         BZ    BADOPT                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),22(R4)                                                 
         AHI   R1,9                                                             
         STC   R1,WORK             FIELD LENGTH                                 
         MVC   WORK+5(1),1(R4)     INPUT LENGTH                                 
         GOTO1 BOOKVAL,DMCB,(C'N',WORK),(1,FULL),SCANNER                        
         SR    RF,RF                                                            
         ICM   RF,1,4(R1)                                                       
         BZ    BADOPT                                                           
         TM    FULL,X'BF'          TEST FOR FUNNY BOOK FORMATS                  
         BNZ   BADOPT                                                           
*                                                                               
***      MVC   SBQUPFBK,FULL+1                                                  
         MVC   0(2,R3),FULL+1                                                   
         B     OPTEND                                                           
*                                                                               
OPTUPT   DS    0H                                                               
         XC    WORK,WORK           GENERATE A PSEUDO TWA FLD                    
         SR    R1,R1                                                            
         ICM   R1,1,1(R4)                                                       
         BZ    BADOPT                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),22(R4)                                                 
         AHI   R1,9                                                             
         STC   R1,WORK             FIELD LENGTH                                 
         MVC   WORK+5(1),1(R4)     INPUT LENGTH                                 
         GOTO1 UPVAL,DMCB,(3,WORK),PARAS,(C'/',ACOMFACS)                        
         CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   GTMSGNO,=Y(INVUPGR)                                              
         B     OPTXNE                                                           
*                                                                               
         MVC   SBQUPGRD,PARAS+4                                                 
         MVI   SBQUPFIL,C'T'                                                    
         MVI   SBQUPSHR,C'1'                                                    
         MVI   SBQUPPUT,C'1'                                                    
         B     OPTEND                                                           
*                                                                               
OPTDATA  LA    R1,N1TYPES          DATA                                         
         SR    R3,R3                                                            
         LA    RE,DATA1TYP                                                      
         LA    RF,L'DATA1TYP                                                    
         CLI   1(R4),1                                                          
         BE    OPT29                                                            
         CLI   1(R4),3                                                          
         BNE   BADOPT                                                           
         LA    R1,N3TYPES                                                       
         LA    R3,2                                                             
         LA    RE,DATA3TYP                                                      
         LA    RF,L'DATA3TYP                                                    
*                                                                               
OPT29    EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R4),0(RE)                                                   
         BE    *+16                                                             
         LA    RE,0(RF,RE)                                                      
         BCT   R1,OPT29                                                         
         B     BADOPT                                                           
         LA    R3,1(R3,RE)                                                      
         MVC   SBQDATA,0(R3)                                                    
         B     OPTEND                                                           
*                                                                               
OPTMENU  MVC   SBQDEMOS,22(R4)     DEMO MENU                                    
         B     OPTEND                                                           
*                                                                               
OPTSVI   CLI   22(R4),C'N'         SVI ADJUSTMENTS                              
         BNE   *+12                                                             
         OI    SBQSVI,SBQSVINO                                                  
         B     OPTEND                                                           
         CLC   22(4,R4),=C'AUTO'                                                
         BNE   *+12                                                             
         OI    SBQSVI,SBQSVIAU                                                  
         B     OPTEND                                                           
         TM    3(R4),X'80'                                                      
         BZ    BADOPT                                                           
         XR    R1,R1                                                            
         ICM   R1,7,9(R4)                                                       
         CHI   R1,1                                                             
         BL    BADOPT                                                           
         CHI   R1,12                                                            
         BH    BADOPT                                                           
         MVC   SBQSVI,11(R4)                                                    
         B     OPTEND                                                           
*                                                                               
OPTPBSPL CLI   22(R4),C'Y'         SPLIT PIGGYBACKS                             
         BNE   *+12                                                             
         OI    SBQPIND,SBQPNOPB    FORCE SPLIT PIGGYBACKS                       
         B     OPTEND                                                           
         CLI   22(R4),C'N'                                                      
         BNE   BADOPT                                                           
         OI    SBQPIND,SBQPPB      DO NOT SPLIT PIGGYBACKS                      
         B     OPTEND                                                           
*                                                                               
OPTPB    CLI   22(R4),C'Y'         SUPPRESS PIGGYBACKS                          
         BE    OPTEND                                                           
         CLI   22(R4),C'N'                                                      
         BNE   *+12                                                             
         OI    SBQPIND2,SBQPIGNO   SUPPRESS PIGGYBACKS                          
         B     OPTEND                                                           
         CLI   22(R4),C'O'                                                      
         BNE   BADOPT                                                           
         OI    SBQPIND,SBQPIGS     ONLY PIGGYBACKS                              
         B     OPTEND                                                           
*                                                                               
OPTWGT   MVC   WGTOPT,22(R4)       MARKET WEIGHTING                             
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPTDEMO  MVI   DEMOPT,DEMOTGT      DEMO OPTION                                  
         CLC   22(3,R4),=C'TGT'                                                 
         BE    OPTEND                                                           
         MVI   DEMOPT,DEMOSEC                                                   
         CLC   22(3,R4),=C'SEC'                                                 
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPTDPT   MVC   SBQDPTMN,22(R4)     DAYPART MENU                                 
         B     OPTEND                                                           
*                                                                               
OPTRPT   CLI   1(R4),1             REPORT                                       
         BL    BADOPT                                                           
         CLI   1(R4),8                                                          
         BH    BADOPT                                                           
         XC    WORK,WORK           GENERATE A PSEUDO TWA HEADER                 
         MVC   WORK+5(1),1(R4)     (PASS THROUGH THE LENGTH)                    
         MVC   WORK+8(10),22(R4)                                                
         LA    R1,WORK                                                          
         ST    R1,DRSPTFLD                                                      
         OI    DRFLAGS,DREXPDIC                                                 
         MVI   DRACTION,DRROW                                                   
         XC    DRSECBLK,DRSECBLK   DONT PASS A(SECRET)!!!                       
         GOTO1 DRONE,DMCB,DRGEN    VALIDATE THE REPORT CODE                     
         L     R1,ATWA             NEED TO RESET DRSECBLK!                      
         USING T204FFD,R1                                                       
         OC    T204FFD+4(2),T204FFD+4    ON NEW SECURITY?                       
         BZ    *+10                      NO, DO NOT PASS SECRET BLOCK           
         MVC   DRSECBLK,ASECBLK    PASS A(SECRET) TO DRONE                      
         DROP  R1                                                               
         CLI   DRERROR,0                                                        
         BNE   BADOPT                                                           
         CLI   DRATTRIB,C'P'       CHECK ATTRIBUTE FOR REPORT INDICATOR         
         BNE   BADOPT                                                           
         MVC   RPTOVLY,DRATTRIB+1  YES - SAVE THE REPORT OVERLAY NUMBER         
         CLI   RPTOVLY,0           CHECK IT'S NOT ZERO                          
         BE    BADOPT                                                           
         MVC   DPGFILE,DRATTRIB+2  SAVE DPG FILE NUMBER, IF ANY                 
         MVC   RPTSCRN,DRATTRIB+3  SAVE SCREEN NUMBER,IF ANY                    
         B     OPTEND                                                           
*                                                                               
OPTSPILL CLI   1(R4),1             SPILL                                        
         BNE   BADOPT                                                           
         MVC   SBQSPILL,22(R4)                                                  
         CLI   22(R4),C'N'         NO SPILL                                     
         BE    OPTEND                                                           
         CLI   22(R4),C'C'         COMBINED                                     
         BE    OPTEND                                                           
         CLI   22(R4),C'S'         SEPERATE                                     
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPTDMGRP MVI   DEMGRP,C'P'         DEMOPGRP                                     
         B     OPTEND                                                           
*                                                                               
OPTDMRND CLI   22(R4),C'Y'         DEMO ROUNDING                                
         BE    OPTEND                                                           
         OI    COLIND,COLINDR                                                   
         CLI   SBQMED,C'*'         MEDIA *?                                     
         BE    OPTDR10             YES, PROCESS AS MEDIA T                      
         CLI   SBQMED,C'T'         MEDIA T?                                     
         BE    OPTDR10             YES                                          
         CLI   SBQMED,C'C'         MEDIA C?                                     
         BE    OPTDR10             YES                                          
         CLI   SBQMED,C'X'         MEDIA X?                                     
         BE    OPTDR10             YES                                          
         CLI   SBQMED,C'N'         MEDIA N?                                     
         BE    OPTDR10             YES                                          
*                                                                               
         LA    RE,SBAGYREC         A(AGENCY RECORD)                             
         USING AGYHDRD,RE          AGENCY RECORD DSECT                          
         CLI   AGYPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   OPTEND              NO, 2 DECIMAL ONLY FOR TV & NTWK             
         CLI   SBQMED,C'R'         MEDIA R?                                     
         BNE   OPTEND              NO, 2 DECIMAL ONLY FOR TV & NTWK             
         DROP  RE                  DROP AGENCY RECORD USING                     
*                                                                               
OPTDR10  CLI   22(R4),C'1'         1 DECIMAL PRECISION?                         
         BE    OPTEND              YES                                          
         CLI   22(R4),C'2'         2 DECIMAL PRECISION                          
         BNE   OPTDR11                                                          
         OI    SBEFLAG4,SBE42DEC   YES                                          
*                                                                               
         LA    RE,SBAGYREC         A(AGENCY RECORD)                             
         USING AGYHDRD,RE          AGENCY RECORD DSECT                          
         CLI   AGYPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+12                NO                                           
         CLI   SBQMED,C'R'         MEDIA R?                                     
         BE    OPTEND              YES - NO 2 DECIMAL IMPRESSIONS               
         DROP  RE                  DROP AGENCY RECORD USING                     
*                                                                               
         CLI   23(R4),C'I'         2 DECIMAL PRECISION IMPRESSIONS?             
         BNE   *+8                 NO                                           
         OI    SBEFLAG9,SBE92DEC   YES - SET 2 DECIMAL IMPRESSIONS              
         B     OPTEND                                                           
OPTDR11  CLI   22(R4),C'N'                                                      
         BNE   BADOPT                                                           
***                                                                             
* READ AGENCY LEVEL 00 PROFILE FOR 2 DECIMAL DEMOS                              
***                                                                             
         XC    WORK,WORK           PREPARE FOR PROFILE READ                     
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),SBAGYREC+1  GET AGENCY LEVEL 00 PROFILE                
         GOTO1 GETPROF,DMCB,(X'90',WORK),WORK+16,DATAMGR                        
         CLI   WORK+25,C'Y'        2 DECIMAL RATINGS?                           
         BNE   OPTEND              NO                                           
         OI    SBEFLAG4,SBE42DEC   YES                                          
*                                                                               
         LA    RE,SBAGYREC         A(AGENCY RECORD)                             
         USING AGYHDRD,RE          AGENCY RECORD DSECT                          
         CLI   AGYPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+12                NO                                           
         CLI   SBQMED,C'R'         MEDIA R?                                     
         BE    OPTEND              YES - NO 2 DECIMAL IMPRESSIONS               
         DROP  RE                  DROP AGENCY RECORD USING                     
*                                                                               
         CLI   23(R4),C'I'         2 DECIMAL PRECISION IMPRESSIONS?             
         BNE   OPTEND              NO                                           
***                                                                             
* READ AGENCY LEVEL 00A PROFILE FOR 2 DECIMAL IMPRESSIONS                       
***                                                                             
         XC    WORK,WORK           PREPARE FOR PROFILE READ                     
         MVC   WORK(4),=C'S00A'    00A PROFILE                                  
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         MVC   WORK+4(2),SBAGYREC+1  GET AGENCY LEVEL 00A PROFILE               
         GOTO1 GETPROF,DMCB,(X'90',WORK),WORK+16,DATAMGR                        
         CLI   WORK+22,C'Y'        2 DECIMAL IMPRESSIONS?                       
         BNE   OPTEND              NO                                           
         OI    SBEFLAG9,SBE92DEC   YES                                          
         B     OPTEND                                                           
*                                                                               
OPTMAX   TM    3(R4),X'80'         MAX RECORD INPUT COUNT                       
         BZ    BADOPT                                                           
*         OC    8(2,R4),8(R4)                                                   
*         BNZ   BADOPT                                                          
         CLI   9(R4),0                                                          
         BNE   BADOPT                                                           
         MVC   MAXREC,10(R4)                                                    
         B     OPTEND                                                           
*                                                                               
OPTBD    LA    R3,SBQBILST         DATE OF BILLING DATES                        
         LA    R5,QBPERTYP                                                      
         OI    OPTIND3,OPTIBD                                                   
         B     OPT56                                                            
OPTPD    LA    R3,SBQPAYST         PAID DATES                                   
         LA    R5,QPPERTYP                                                      
*                                                                               
OPT56    MVI   0(R5),0                                                          
         CLC   22(6,R4),=C'TODAY '                                              
         BNE   OPT56A                                                           
         GOTO1 DATCON,DMCB,(5,0),(2,(R3))                                       
         MVC   2(2,R3),0(R3)                                                    
         B     OPTEND                                                           
*                                                                               
OPT56A   ST    R2,FULL                                                          
         XC    GTMSGNO,GTMSGNO                                                  
***         XC    ERROR,ERROR                                                   
***         MVI   ERROPT,C'Y'         COME BACK TO ME ON ERROR                  
         LA    R2,14(R4)                                                        
         GOTO1 PERVAL,DMCB,(1(R4),8(R2)),(0,WORK)                               
         TM    DMCB+4,X'03'        INVALID/NO INPUT?                            
         BNZ   OPT56C              YES - SO ERROR                               
*                                                                               
         LA    R1,WORK                                                          
         USING PERVALD,R1                                                       
*                                                                               
         CLI   PVALASSM,X'00'      BOTH FULL DATES ENTERED?                     
         BE    OPT56E              YES - SO CONTINUE                            
*                                                                               
         CLI   PVALASSM,X'70'      FULL START DATE ONLY ENTERED?                
         BE    OPT56E              YES - SO CONTINUE                            
*                                                                               
         CLI   PVALASSM,X'11'      BOTH YYMM DATES ENTERED?                     
         BE    OPT56E              YES - SO CONTINUE                            
*                                                                               
         CLI   PVALASSM,X'71'      YYMM START DATE ONLY ENTERED?                
         BE    OPT56E              YES - SO CONTINUE                            
*                                                                               
OPT56C   MVC   GTMSGNO,=Y(BADDATE) ELSE - SET ERROR CODE                        
*                                                                               
OPT56E   L     R2,FULL             RESTORE (R2)                                 
***         MVI   ERROPT,0                                                      
         OC    GTMSGNO,GTMSGNO                                                  
         BNZ   OPTXNE                                                           
*                                                                               
         TM    PVALASSM,X'01'      YYMM DATES ENTERED?                          
         BNO   OPT56H              NO - SO CONTINUE                             
*                                                                               
         MVC   PVALESTA+4(2),=C'00' ELSE - SET START DD=00                      
         MVC   PVALEEND+4(2),=C'00' SET END DD=00                               
         MVI   0(R5),2             AND SET THIS TO MIMIC THE OLD CODE           
*                                                                               
OPT56H   MVC   WORK(12),PVALESTA   MOVE S/E DATES TO WORK                       
         DROP  R1                                                               
*                                                                               
         CLI   0(R5),2          TEST PERIOD IN MONTHS                           
         BNE   *+18                                                             
         CLC   WORK(4),WORK+6   YES-TEST START MONTH=END MONTH                  
         BNE   *+8                                                              
         OI    0(R5),X'80'      YES                                             
         CLC   WORK+4(2),=C'00' TEST START DAY SET                              
         BNE   OPT57                                                            
         MVI   WORK+5,C'1'      NO-MAKE IT THE BROADCAST START                  
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+12,GETDAY,ADDAY                      
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(6),WORK+12                                                  
*                                                                               
OPT57    CLC   WORK+10(2),=C'00'   TEST END DAY SET                             
         BNE   OPT58                                                            
         MVI   WORK+11,C'1'        MAKE IT THE BROADCAST MONTH END              
         GOTO1 GETBROAD,DMCB,(1,WORK+6),WORK+12,GETDAY,ADDAY                    
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+6(6),WORK+18                                                
*                                                                               
OPT58    GOTO1 DATCON,DMCB,(0,WORK),(2,(R3))                                    
         GOTO1 (RF),(R1),(0,WORK+6),(2,2(R3))                                   
         B     OPTEND                                                           
*                                                                               
OPTNTWRK CLI   1(R4),3             INPUT LENGTH = 3?                            
         BNE   OPTNT10             NO                                           
         CLC   =C'NWK',22(R4)      NETWORK=NWK?                                 
         BNE   BADOPT              NO - ERROR                                   
         XC    WORK,WORK           PREPARE FOR PROFILE READ                     
         MVC   WORK(4),=C'S0D0'    READ D0 PROFILE                              
         MVC   WORK+4(2),SBAGYREC+1  GET AGENCY LEVEL D0 PROFILE                
         GOTO1 GETPROF,DMCB,(X'90',WORK),WORK+16,DATAMGR                        
         CLI   WORK+29,C'P'        NETWORK DEMO LOOKUP = P?                     
         BE    *+12                NO - ERROR                                   
         CLI   WORK+29,C'B'        NETWORK DEMO LOOKUP = BOTH?                  
         BNE   BADOPT4             NO - ERROR                                   
         CLI   WORK+30,C'Y'        USE NEW POSTING METHODOLOGY?                 
         BNE   *+8                 NO                                           
         OI    SBEFLAG8,SBE8NPM    YES - SET FLAG                               
         OI    SBEFLAG8,SBE8CNWK   SET NETWORK=NWK FLAG                         
         MVI   SBQNETWK,C'B'       SET SBQNETWK TO "B"                          
         OI    SBQCAN,SBQCBYM0     READ MKT 0 BUYS                              
         OI    SBEFLAG3,SBE3M0DL   WANT MARKET0 DOLLARS (CANADA)                
         B     OPTEND              DONE                                         
*                                                                               
OPTNT10  CLI   1(R4),1             NETWORK                                      
         BNE   BADOPT                                                           
         MVC   SBQNETWK,22(R4)                                                  
         CLI   SBQNETWK,C'B'       BOTH                                         
         BE    OPTEND                                                           
         CLI   SBQNETWK,C'N'       NETWORK LEVEL ONLY                           
         BE    OPTEND                                                           
         CLI   SBQNETWK,C'L'       LOCAL STATION LEVEL ONLY                     
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                 ---- BILL HEADER DATES                        
OPTBHIDT LA    R3,SBQBHIST                                                      
         B     OPTBHD                  INVOICE DATE                             
OPTBHRDT LA    R3,SBQBHRST                                                      
         B     OPTBHD                  RUN DATE                                 
OPTBHDDT LA    R3,SBQBHDST                                                      
         B     OPTBHD                  DUE DATE                                 
OPTBHEDT LA    R3,SBQBHEST                                                      
         B     OPTBHD                  EDI DATE                                 
OPTBHPDT LA    R3,SBQBHPST                                                      
*                                                                               
OPTBHD   OC    ARFPBLK,ARFPBLK     RFP MODE?                                    
         BZ    OPTBHD30             NO                                          
         BRAS  RE,VALRFP           CHECK IF VALID RFP SYMBOL                    
         BNE   OPTBHD30             NO - CHECK IF VALID DATE                    
*                                                                               
* MAKE SURE IF RFP KEYWORD, IT IS THE LAST OPTION ON THE INPUT LINE             
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RF,8(R4)            GET DISPLACEMENT OF RFP KEYWORD              
         IC    RE,1(R4)            GET LENGTH OF RFP KEYWORD                    
         AR    RF,RE                                                            
         CLM   RF,1,5(R2)          DISP+L'INPUT S.B. L'TOTAL INPUT              
         BE    OPTBHD10                                                         
         MVC   GTMSGNO,=Y(RLPLAST)                                              
         B     OPTXNE                                                           
*                                                                               
* MAKE SURE EXPANSION OF RFP KEYWORD WILL FIT!                                  
OPTBHD10 IC    RF,5(R2)            RF=INPUT LENGTH                              
         IC    RE,1(R4)            L'RFP KEYWORD INPUT                          
         SR    RF,RE               RF=L'INPUT - RFP KEYWORD                     
*                                                                               
         IC    RE,0(R2)            RE=MAX INPUT LENGTH                          
         AHI   RE,-8                                                            
         TM    1(R2),X'02'         TEST EXTENDED FLD HDR                        
         BZ    *+8                                                              
         AHI   RE,-8                                                            
*                                                                               
         SR    RE,RF                                                            
         CHI   RE,17               MAKE SURE MAX RFP EXPANSION FITS             
         BNL   OPTBHD20                                                         
         MVC   GTMSGNO,=Y(RLPROOM)                                              
         B     OPTXNE                                                           
*                                                                               
* INCREASE FIELD INPUT LENGTH TO FIT MAX EXPANSION                              
OPTBHD20 LHI   RE,17               MAX EXPANSION                                
         IC    RF,1(R4)            L'RFP KEYWORD INPUT                          
         SR    RE,RF               HOW MUCH BIGGER INPUT NEEDS TO BE            
         IC    RF,5(R2)                                                         
         AR    RF,RE                                                            
         STC   RF,5(R2)                                                         
         OI    6(R2),X'80'         AND SET TRANSMIT BIT                         
*                                                                               
         IC    RF,8(R4)            GET DISPLACEMENT OF RFP KEYWORD              
         AR    RF,R2                                                            
         XC    8(7,RF),8(RF)       CLEAR OUT INPUT (BHRDATE)                    
         MVC   8(L'QRFPESC,RF),22(R4)   MOVE ESC SEQ TO SCREEN AFTER =          
         MVI   11(RF),17           SET L'EXPANSION OF ESC SEQ                   
*                                                                               
         CLC   23(2,R4),=Y(SP#BHRDT) ONLY BHRDATE IS VALID HERE                 
         BE    OPTEND                                                           
         MVC   GTMSGNO,=Y(BADDATE)                                              
         B     OPTXNE                                                           
*                                                                               
OPTBHD30 ST    R2,FULL            SAVE (R2)                                     
***         MVI   BYTE,0             VALIDATE FOR M/D/Y                         
***         XC    ERROR,ERROR                                                   
***         MVI   ERROPT,C'Y'         COME BACK TO ME ON ERROR                  
         XC    GTMSGNO,GTMSGNO                                                  
         LA    R2,14(R4)          FAKE A SCREEN HEADER                          
         GOTO1 PERVAL,DMCB,(1(R4),8(R2)),(0,WORK)                               
         TM    DMCB+4,X'03'        INVALID/NO INPUT?                            
         BNZ   OPTBHD40            YES - SO ERROR                               
*                                                                               
         LA    R1,WORK                                                          
         USING PERVALD,R1                                                       
*                                                                               
         CLI   PVALASSM,X'00'      BOTH FULL DATES ENTERED?                     
         BE    OPTBHD50            YES - SO CONTINUE                            
*                                                                               
         CLI   PVALASSM,X'70'      FULL START DATE ONLY ENTERED?                
         BE    OPTBHD50            YES - SO CONTINUE                            
*                                                                               
OPTBHD40 MVC   GTMSGNO,=Y(BADDATE) ELSE - SET ERROR CODE                        
*                                                                               
OPTBHD50 L     R2,FULL             RESTORE (R2)                                 
***         MVI   ERROPT,0                                                      
         OC    GTMSGNO,GTMSGNO                                                  
         BNZ   OPTXNE                                                           
         MVC   0(4,R3),PVALCSTA    MOVE S/E DATES                               
         B     OPTEND                                                           
         DROP  R1                                                               
*                                                                               
OPTBHTYP MVI   SBQBHTYP,SBQBHTYA   BILL HEADER TYPE                             
         CLC   22(3,R4),=C'AOR'                                                 
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYB                                                
         CLC   22(4,R4),=C'-AOR'                                                
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYC                                                
         CLC   22(3,R4),=C'UFC'                                                 
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYD                                                
         CLC   22(4,R4),=C'-UFC'                                                
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYN                                                
         CLC   22(3,R4),=C'NET'                                                 
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYO                                                
         CLC   22(4,R4),=C'-NET'                                                
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYE                                                
         CLC   22(3,R4),=C'RCB'                                                 
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYF                                                
         CLC   22(4,R4),=C'-RCB'                                                
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYG                                                
         CLC   22(3,R4),=C'MAN'                                                 
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYH                                                
         CLC   22(4,R4),=C'-MAN'                                                
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYQ                                                
         CLC   22(3,R4),=C'ORD'                                                 
         BE    OPTEND                                                           
         CLC   22(4,R4),=C'-CLR'                                                
         BE    OPTEND                                                           
         MVI   SBQBHTYP,SBQBHTYR                                                
         CLC   22(3,R4),=C'CLR'                                                 
         BE    OPTEND                                                           
         CLC   22(4,R4),=C'-ORD'                                                
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPTNOLNT NI    SBQDPTLN,255-SBQDLTOT  OPTION TO SUPPRESS SPOT LENGTH            
         B     OPTEND                 TOTALS FOR DPTLEN                         
*                                                                               
OPTSVC   DS    0H                  RATING SERVICE OVERRIDE                      
OPTFORCE CLI   1(R4),1                                                          
         BL    BADOPT                                                           
         CLI   1(R4),6                                                          
         BH    BADOPT                                                           
         LLC   RE,1(R4)                                                         
         CLI   1(R4),3                                                          
         BNH   OPT74A                                                           
         CLI   25(R4),C'/'                                                      
         BNE   BADOPT                                                           
***      CLC   =C'NSI/O ',22(R4)   MOVED TO OPT75TAB - WAS LEVEL 119            
***      BE    BADOPT                                                           
***      CLC   =C'NSI/OL',22(R4)   MOVED TO OPT75TAB - WAS LEVEL 119            
***      BE    BADOPT                                                           
***      CLC   =C'NSI/OS',22(R4)   MOVED TO OPT75TAB - WAS LEVEL 119            
***      BE    BADOPT                                                           
***      CLC   =C'NSI/O3',22(R4)   MOVED TO OPT75TAB - WAS LEVEL 119            
***      BE    BADOPT                                                           
         CLC   =C'NSI',22(R4)      SCV=NSI?                                     
         BNE   OPTSVC05            NO - SKIP DISALLOWED BOOKTYPE TEST           
*                                                                               
         LA    R1,OPT75TAB         TABLE OF DISALLOWED BOOKTYPES                
*                                                                               
OPTSVC00 CLI   0(R1),X'00'         END OF TABLE?                                
         BE    OPTSVC05            YES - BOOKTYPE ALLOWED                       
         CLC   0(2,R1),26(R4)      DISALLOWED BOOKTYPE?                         
         BE    BADOPT              YES - ERROR                                  
         LA    R1,2(R1)            BUMP TO NEXT ENTRY                           
         B     OPTSVC00            CHECK EOT/NEXT ENTRY                         
*                                                                               
OPTSVC05 L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
OPTSVC10 CLI   0(RF),X'FF'                                                      
         BNE   *+14                                                             
         MVC   GTMSGNO,=Y(INVBKTY) INVALID BOOK TYPE                            
         J     NEWTRAP                                                          
         CLC   26(L'SPBKTYPA,R4),SPBKTYPA                                       
         BE    *+10                                                             
         AR    RF,RE                                                            
         B     OPTSVC10                                                         
         MVC   SBQBKTYP,SPBKTYPN                                                
         DROP  RF                                                               
*                                                                               
         LLC   RE,1(R4)                                                         
         AHI   RE,-2                                                            
         CLI   27(R4),C' '                                                      
         BNH   *+6                                                              
         BCTR  RE,0                                                             
*                                                                               
OPT74A   BCTR  RE,0                                                             
         LA    R1,OPT74TAB                                                      
OPT74B   EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R4),0(R1)                                                   
         BE    OPT74C                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   OPT74B                                                           
         B     BADOPT                                                           
OPT74C   MVC   SVCOPT,3(R1)        SET SERVICE OPTION 0,1                       
         CLI   12(R4),C'F'         TEST OPTION IS FORCE                         
         BNE   OPTEND                                                           
         NI    SVCOPT,X'7F'        YES-MAKE A NOTE HERE                         
         B     OPTEND                                                           
OPT74TAB DC    CL3'ARB',C'1'                                                    
         DC    CL3'BBM',C'1'                                                    
         DC    CL3'NSI',C'0'                                                    
         DC    CL3'CSI',C'0'                                                    
         DC    X'00'                                                            
***                                                                             
* TABLE OF DISALLOWED BOOKTYPES FOR OPTION SVC=NSI/                             
***                                                                             
OPT75TAB DC    C'O '                                                            
         DC    C'OL'                                                            
         DC    C'OS'                                                            
         DC    C'O3'                                                            
***      DC    C'YL'                                                            
***      DC    C'YS'                                                            
***      DC    C'Y3'                                                            
***      DC    C'Y7'                                                            
***      DC    C'YU'                                                            
***      DC    C'YD'                                                            
***      DC    C'YC'                                                            
***      DC    C'YZ'                                                            
***      DC    C'YA'                                                            
***      DC    C'YB'                                                            
***      DC    C'YW'                                                            
***      DC    C'YJ'                                                            
***      DC    C'YE'                                                            
***      DC    C'YF'                                                            
***      DC    C'YH'                                                            
         DC    X'00'                                                            
OPTPER   CLI   1(R4),1             PERIOD                                       
         BNE   OPT76A                                                           
         MVI   PEROPT,PEROBRD      USE BROADCAST MONTHS                         
         CLI   22(R4),C'B'                                                      
         BE    OPTEND                                                           
         MVI   PEROPT,PEROCAL      USE CALENDAR MONTHS                          
         OI    SBEFLAG5,SBE5CLDR   CALENDAR MONTHS                              
         CLI   22(R4),C'C'                                                      
         BE    OPTEND                                                           
*                                                                               
OPT76A   CLI   1(R4),2                                                          
         BNE   BADOPT                                                           
         MVI   PEROPT,PEROAB       USE AB PROFILE                               
         CLC   22(2,R4),=C'AB'                                                  
         BE    OPTEND                                                           
*                                                                               
         MVI   PEROPT,PEROB3       USE B3 PROFILE                               
         CLC   22(2,R4),=C'B3'                                                  
         BE    OPTEND                                                           
*                                                                               
         MVI   PEROPT,PERO00       USE 00 PROFILE                               
         CLC   22(2,R4),=C'00'                                                  
         BE    OPTEND                                                           
*                                                                               
         MVI   PEROPT,PEROFF       USE FISCAL FOLLIES                           
         CLC   22(2,R4),=C'FF'                                                  
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPTCOM   CLI   1(R4),2             COMMENT                                      
         BNE   OPT78                                                            
         MVI   COMOPT,COMOAB                                                    
         CLC   22(2,R4),=C'AB'                                                  
         BE    OPTEND                                                           
         MVI   COMOPT,COMOA2                                                    
         CLC   22(2,R4),=C'A2'                                                  
         BE    OPTEND                                                           
         MVI   COMOPT,COMOB1                                                    
         CLC   22(2,R4),=C'B1'                                                  
         BE    OPTEND                                                           
OPT78    MVI   COMOPT,COMOMC                                                    
         CLC   22(5,R4),=C'MCOM '                                               
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPTSTACK BAS   RE,VALSTACK         STACK                                        
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
*                                  TOP                                          
OPTTOP   TM    3(R4),X'80'         TEST NUMERIC                                 
         BZ    BADOPT                                                           
*         OC    8(2,R4),8(R4)       AND NO MORE THAN HALFWORD                   
*         BNZ   BADOPT                                                          
         CLI   9(R4),0             AND NO MORE THAN HALFWORD                    
         BNE   BADOPT                                                           
         MVC   RANKMAX,10(R4)                                                   
         B     OPTEND                                                           
*                                                                               
OPTDATE  MVI   DATEOPT,DOBILL      DATE                                         
         CLC   22(3,R4),=C'BD '    BD=BILLED DATES                              
         BE    OPTEND                                                           
         MVI   DATEOPT,DOPAID                                                   
         CLC   22(3,R4),=C'PD '    PD=PAID DATES                                
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPTDF    CLI   22(R4),C'Y'         DUMB FUCK (ACCORDING TO LCONN)               
         BE    OPTEND                                                           
         CLI   22(R4),C'N'                                                      
         BNE   BADOPT                                                           
         OI    OPTIND,OPTINODF                                                  
         B     OPTEND                                                           
*                                                                               
OPTDEMOS BAS   RE,VALDEM           DEMOS                                        
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
OPTCUR   MVC   SBQCURR,22(R4)      CURRENCY                                     
         CLI   SBQCURR,C'C'                                                     
         BE    OPTEND                                                           
         CLI   SBQCURR,C'U'                                                     
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPTUNALL MVC   SBEUNALL,22(R4)     UNALL                                        
         CLI   SBEUNALL,C'Y'                                                    
         BE    OPTEND                                                           
         CLI   SBEUNALL,C'N'                                                    
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
OPTWRITE CLC   22(2,R4),=C'NO'     WRITE=NO                                     
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
OPTCML   MVI   SBQCMLTY,SBQCTINV   COMMERCIAL TYPE                              
         CLI   22(R4),C'I'         INVOICED                                     
         BE    OPTEND                                                           
         MVI   SBQCMLTY,SBQCTTAS                                                
         CLI   22(R4),C'A'         TRAFFIC ASSIGNED                             
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
*                                  NO REQUEST DETAILS                           
OPTNORQ  TM    DOWNOPT,GLDLACTV    DON'T ALLOW NOREQDET W/DOWN                  
         BZ    OPTEND                                                           
         MVC   GTMSGNO,=Y(NOREQDL)                                              
         J     NEWTRAP                                                          
*                                                                               
OPTMGRCL CLI   1(R4),2             ALTERNATE CLIENT FOR MARKET GROUPS           
         BL    BADOPT                                                           
         CLI   1(R4),3                                                          
         BH    BADOPT                                                           
         GOTO1 CLPACK,DMCB,22(R4),SBQMGCLT                                      
         B     OPTEND                                                           
*                                                                               
OPTFLOAT CLI   1(R4),1             FLOAT=-                                      
         BNE   BADOPT                                                           
         CLI   22(R4),C'-'                                                      
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
OPTEQU   GOTO1 CLPACK,DMCB,22(R4),SBBEQCLT  EQU                                 
         B     OPTEND                                                           
*                                                                               
OPTDMA   CLI   1(R4),1             DMA OVERRIDE                                 
         BNE   BADOPT                                                           
         CLI   22(R4),C'I'                                                      
         BE    *+12                                                             
         CLI   22(R4),C'R'                                                      
         BNE   BADOPT                                                           
         MVC   SBEDMA,22(R4)                                                    
         B     OPTEND                                                           
*                                                                               
OPTWDP   CLI   1(R4),1             WEEKLY DATA FOR POSTS OVERRIDE               
         BNE   BADOPT                                                           
         CLI   22(R4),C'Y'                                                      
         BE    *+12                                                             
         CLI   22(R4),C'N'                                                      
         BNE   BADOPT                                                           
         MVC   SBEWDP,22(R4)                                                    
         B     OPTEND                                                           
*                                                                               
OPTLKUP  CLI   1(R4),3             3 CHARACTER INPUT?                           
         BNE   BADOPT              NO - ERROR                                   
         CLC   22(3,R4),=C'ALL'    LKUP=ALL?                                    
         BNE   BADOPT              NO - ERROR                                   
         MVI   SBELKUPA,C'Y'       INDICATE LKUP=ALL                            
         B     OPTEND              DONE                                         
*                                                                               
OPTML    L     RF,ATWA             ATWA                                         
         USING T204FFD,RF          SCREEN DSECT                                 
         CLC   =C'SL',CONREC       SL REPORT?                                   
         BNE   BADOPT              NO - ERROR                                   
         DROP  RF                  DROP SCREEN USING                            
*                                                                               
         CLI   22(R4),C'Y'         ML=Y OR MLOCK=Y?                             
         BNE   BADOPT              NO - ERROR                                   
*                                                                               
         OI    FLAGS,FLMLOCK       SET TO ADD ML REQUEST                        
         B     OPTEND              DONE                                         
*                                                                               
OPTGL    L     RF,ATWA             ATWA                                         
         USING T204FFD,RF          SCREEN DSECT                                 
         CLC   =C'SL',CONREC       SL REPORT?                                   
         BNE   BADOPT              NO - ERROR                                   
         DROP  RF                  DROP SCREEN USING                            
*                                                                               
         CLI   22(R4),C'Y'         GL=Y OR GLOCK=Y?                             
         BNE   *+12                NO                                           
         OI    FLAGS,FLGLOCK       SET TO ALSO LOCK GOALS IN ML                 
         B     OPTEND              DONE                                         
*                                                                               
         CLI   22(R4),C'O'         GL=O(NLY) OR GLOCK=O(NLY)?                   
         BNE   BADOPT              NO - ERROR                                   
*                                                                               
         OI    FLAGS,FLGONLY       SET TO ONLY LOCK GOALS IN ML                 
         B     OPTEND              DONE                                         
*                                                                               
OPTXFIL  DS    0H                  CROSS FILE REPORTING (DDB ONLY)              
         CLI   OFFLINE,C'Y'                                                     
         BE    OPTX10                                                           
         CLI   DDS,C'Y'            DDS?                                         
         BE    OPTX10                                                           
         CLI   WHEN,X'20'          TEST SOON REQUEST                            
         BE    BADOPT               YES - NO CROSS FILE SOON                    
         TM    AUTH,X'20'          AUTHORIZED?                                  
         BZ    BADOPT              NO - DENY OPTION EVEN EXISTS                 
*                                                                               
OPTX10   OI    OPTIND4,OPTXFILE                                                 
         OI    SBIOFLAG,SBXFILE                                                 
         ICM   RE,15,SBAUTL                                                     
         MVC   SBSTAFSE,4(RE)      SET ALT STAFILE FILE AND AGY                 
         MVC   SBSTAAGY,SBQAGY                                                  
         B     OPTEND                                                           
*                                                                               
OPTCOLPD OI    OPTIND4,OPTCOLBD                                                 
         CLC   22(3,R4),=C'BD '    BD=BILLED DATES                              
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
* THIS IS A BIT TRICKY - IT'S PUTTING A TOTAL ELEM IN THE DRONE                 
* BUFFER AFTER THE REC ELEM TO CREATE GRAND TOTALS.  IT MUST BE                 
* DONE AFTER THE INITDRON CALL, BUT BEFORE ANY OTHER BUFFER ELEMS               
* ARE ADDED.  ALL THAT SHOULD BE IN THE BUFFER CURRENTLY IS 1002.               
*                                                                               
OPTGRAND L     R1,DRCURBUF         REPORT GRAND TOTALS                          
         MVC   0(2,R1),=X'4802'                                                 
         LA    R1,2(R1)                                                         
         MVC   0(2,R1),=X'820A'                                                 
         MVC   2(8,R1),=C'NOREPORT'                                             
         LA    R1,10(R1)                                                        
         ST    R1,DRCURBUF                                                      
         B     OPTEND                                                           
*                                                                               
OPTWKDY  CLI   OFFLINE,C'Y'                                                     
         BE    OPTC10                                                           
         L     RF,ATWA                                                          
         CLC   =C'REP',CONACT-T204FFD(RF)                                       
         BNE   BADOPT              ONLY ALLOWED FOR REPORT                      
         OC    ARFPBLK,ARFPBLK     BUT NOT RFP                                  
         BNZ   BADOPT                                                           
         CLI   DDS,C'Y'            AND ONLY DDS                                 
         BE    OPTC10                                                           
         B     BADOPT                                                           
*                                                                               
OPTCROSS CLI   OFFLINE,C'Y'        XEST IS DDS ONLY ON-LINE                     
         BE    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BE    OPTC10                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),DUB       SBQTODAY NOT SET YET            
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLI   0(R1),5             BEFORE FRIDAY?                               
         BNL   *+14                 NO                                          
         MVC   GTMSGNO,=Y(WKNDONLY)                                             
         B     OPTXNE                                                           
*                                                                               
OPTC10   OI    OPTIND4,OPTXEST                                                  
         B     OPTEND                                                           
*                                                                               
OPTTRD   DS    0H                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)           GET LENGTH                                  
         BZ    BADOPT                                                           
         BCTR  RE,0                                                             
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R4),=C'YES'                                                 
         BE    OPTEND                                                           
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R4),=C'NO'                                                  
         BNE   BADOPT                                                           
*                                                                               
         OI    SBEFLAG3,SBE3NTRD   SET FLAG FOR NO TRADE                        
         B     OPTEND                                                           
*                                                                               
OPTCONT  DS    0H                                                               
         OI    OPTIND5,OPT5CONT    SET 'CONTINUED' INDICATOR                    
         OI    DOWNOPT,GLDLACTV    FORCE DOWNLOAD                               
*                                                                               
OPTRPT2  DS    0H                                                               
         BRAS  RE,VALRP2                                                        
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
OPTEFILT CLC   =C'NP',22(R4)      NON-POSITIONAL?                               
         BNE   BADOPT             NO - BAD OPTION                               
         OI    SBEFLAG5,SBE5EFNP  ESTIMATE FILTER NON-POSITIONAL                
         B     OPTEND                                                           
*                                                                               
OPTORIG  CLC   =C'SL',CONREC-T204FFD(RF)   SL CANNOT USE ORIG=Y!                
         BE    BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
OPTMTRD  CLI   1(R4),1             ONLY 1 CHAR ALLOWED                          
         BNE   BADOPT                                                           
         CLI   22(R4),C'Y'                                                      
         BNE   *+12                                                             
         OI    SBEFLAG6,SBE6TGMY   MTRADE=Y                                     
         B     OPTEND                                                           
         CLI   22(R4),C'N'                                                      
         BNE   BADOPT                                                           
         OI    SBEFLAG6,SBE6TGMN   MTRADE=N                                     
         B     OPTEND                                                           
*                                                                               
OPTSQTY  CLI   1(R4),1             ONLY ONE CHAR ALLOWED                        
         BNE   BADOPT                                                           
         CLI   22(R4),C'H'         HISPANIC?                                    
         BNE   *+14                NO                                           
         MVC   SBQBKTYP,22(R4)                                                  
         B     OPTEND                                                           
         CLI   22(R4),C'Q'         Q?                                           
         BNE   BADOPT              NO - ERROR                                   
         OI    SBEFLAG7,SBE7SQDQ   YES - SET THE SQADTYP=Q FLAG                 
         B     OPTEND              EXIT                                         
*                                                                               
OPTACTB  TM    3(R4),X'80'         ACTUAL BOOK OPTION MUST BE NUMERIC           
         BZ    BADOPT                                                           
         XR    R1,R1                                                            
         ICM   R1,7,9(R4)                                                       
         BZ    BADOPT              CANNOT HAVE ZERO                             
         CHI   R1,255              ACTBOOK CAN HOLD ONLY 1 BYTE (1-255)         
         BH    BADOPT                                                           
         STC   R1,SBSPPROF+3       OVER-WRITE BOOK OPT FROM PROFILE             
         STC   R1,ACTBOOK          IN CASE WE NEED AGAIN IN WRI01               
         B     OPTEND                                                           
*                                                                               
OPTMKT0  CLI   SBQMED,C'N'         TEST MEDIA = N (IMPLIED CANADIAN!)           
         BNE   BADOPT                                                           
         OI    OPTIND6,OPT6NM0B    MARKET ZERO BUYS ONLY FOR NETWORK            
         B     OPTEND                                                           
*                                                                               
OPTBCOM  CLC   =C'CLI',22(R4)      CLI IS ONLY VALID OPT FOR BCOM               
         BNE   BADOPT                                                           
         OI    SBEFLAG3,SBE3BCOM   READ BCOM RECS AT CLIENT LEVEL               
         B     OPTEND                                                           
*                                                                               
OPTNC    MVC   OMNC,22(R4)                                                      
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
OPTUND   MVC   OMUND,22(R4)                                                     
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
OPTLPM   MVC   LPMOPT,22(R4)                                                    
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
OPTWEEK  MVC   LPMWEEK,22(R4)                                                   
         CLI   22(R4),C'Y'          LPMWEEK OPT=YES?                            
         BE    OPTEND               YES                                         
         CLI   22(R4),C'N'          LPMWEEK OPT=NO?                             
         BNE   BADOPT               NO, ERROR                                   
* ALLOW OVN OPTION WITHOUT WEEKLY OPTION                                        
**       CLI   LPMOV,C'Y'           LPMOV=YES?                                  
**       BE    BADOPT3              YES, CANNOT HAVE THAT COMBINATION!          
         B     OPTEND                                                           
*                                                                               
OPTOV    MVC   LPMOV,22(R4)                                                     
         CLI   22(R4),C'N'          LPMOV=NO?                                   
         BE    OPTEND               YES                                         
         CLI   22(R4),C'M'          LPMOV=YES?                                  
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'          LPMOV=YES?                                  
         BNE   BADOPT               NO, ERROR                                   
* ALLOW OVN OPTION WITHOUT WEEKLY OPTION                                        
**       CLI   LPMWEEK,C'N'         LPMWEEK=NO?                                 
**       BE    BADOPT3              YES, CANNOT HAVE THAT COMBINATION!          
         B     OPTEND                                                           
*                                                                               
OPTBUYMG CLI   22(R4),C'Y'          BUYMG=Y?                                    
         BNE   *+12                 NO                                          
         OI    SBEFLAG7,SBE7MGY     SET FLAG                                    
         B     OPTEND                                                           
*                                                                               
         CLI   22(R4),C'N'          BUYMG=N?                                    
         BNE   BADOPT               NO - INVALID OPTION PARAMETER               
         OI    SBEFLAG7,SBE7MGN     SET FLAG                                    
         B     OPTEND                                                           
*                                                                               
OPTCABLE CLI   22(R4),C'F'          FUSION?                                     
         BNE   OPTC01               NO                                          
         OI    CBLFLAG,CFFUS        YES - OVERRIDE 00A PROF TO FUSION           
         B     OPTEND                                                           
*                                                                               
OPTC01   CLI   22(R4),C'N'          NIELSON?                                    
         BNE   OPTC02               NO                                          
         OI    CBLFLAG,CFNLS        YES - OVERRIDE 00A PROF TO NIELSON          
         B     OPTEND                                                           
*                                                                               
OPTC02   CLI   22(R4),C'0'          NONE?                                       
         BNE   OPTC03               NO                                          
         OI    CBLFLAG,CFNONE       YES - OVERRIDE 00A PROF TO NONE             
         B     OPTEND                                                           
*                                                                               
OPTC03   CLI   22(R4),C'L'          LPM?                                        
         BNE   BADOPT               NO, ERROR                                   
         OI    CBLFLAG,CFLPM        YES - ONLY LOOK UP FOR LPM                  
         B     OPTEND                                                           
*                                                                               
BADOPT   DS    0H                                                               
         MVC   GTMSGNO,=Y(OPTERR)                                               
         B     OPTXNE                                                           
*                                                                               
BADOPT2  DS    0H                                                               
         MVC   GTMSGNO,=Y(PREVBOOK)                                             
         B     OPTXNE                                                           
*                                                                               
BADOPT3  DS    0H                                                               
         MVC   GTMSGNO,=Y(1270)                                                 
         B     OPTXNE                                                           
*                                                                               
BADOPT4  DS    0H                                                               
         MVC   GTMSGNO,=Y(1414)                                                 
         B     OPTXNE                                                           
*                                                                               
OPTEND   LA    R4,70+22(R4)                                                     
         XR    R1,R1                                                            
         IC    R1,FIELDERR                                                      
         AHI   R1,1                                                             
         STC   R1,FIELDERR                                                      
         BCT   R0,OPT2                                                          
*                                  CROSS CHECK OPTION COMBINATIONS              
         TM    OPTIND6,OPT6NM0B    IF FORCING CANADIAN NWK MKT 0 BUYS           
         BZ    OPTEND10                                                         
         CLI   SBQNETWK,C'N'       MUST BE REPORTING NETWORK LEVEL ONLY         
         BE    OPTEND10                                                         
         MVC   GTMSGNO,=Y(NWKBYNWK)                                             
         B     OPTXNE                                                           
*                                                                               
OPTEND10 CLI   BOXOPT,0            SET BOX OPTION IF NOT ALREADY SET            
         BNE   OPTXEQ                                                           
         TM    DOWNOPT,GLDLACTV                                                 
         BO    OPTXEQ                                                           
         MVI   BOXOPT,C'Y'                                                      
*                                                                               
OPTXEQ   J     XITEQU                                                           
*                                                                               
OPTXNE   J     XITNEQ                                                           
         EJECT                                                                  
* ROUTINE TO VALIDATE THE STACK OPTION                                          
* INPUT  : R4=A(SCANNER BLOCK ENTRY)                                            
* OUTPUT : CC EQ - OK AND STACKDEF IS SET                                       
*          CC NE - INVALID                                                      
*                                                                               
VALSTACK NTR1  ,                                                                
         SR    R1,R1                                                            
         ICM   R1,1,1(R4)                                                       
         BZ    VSTNO                                                            
         CLI   STACKDEF,0          ONLY ONE STACK ALLOWED                       
         BNE   VSTNO                                                            
         LA    R1,22(R1,R4)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST TERM WITH /                     
         LA    R4,22(R4)                                                        
         LA    R3,STACKDEF                                                      
         CLI   0(R3),0                                                          
         BNE   VSTNO                                                            
         LA    R0,8                MAX 8 IN STACK                               
*                                                                               
VST2     LR    RE,R4                                                            
         CLI   0(RE),C'/'                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         SR    RE,R4               TEST TERM AT LEAST 1 LONG                    
         BNP   VSTNO                                                            
         LR    R2,RE               R2=L'TERM                                    
         BCTR  RE,0                RE=L'TERM-1                                  
         LA    R5,2                                                             
*                                                                               
VST3     LA    R1,STACKTAB         VALIDATE AGAINST TABLE                       
         SR    RF,RF                                                            
*                                                                               
VST4     CLI   0(R1),0                                                          
         BE    VST7                                                             
         IC    RF,7(R1)            RF=MIN L'LONG NAME                           
         CR    R2,RF                                                            
         BL    VST5                                                             
         EX    RE,*+8              COMPARE TO LONG NAME                         
         B     *+10                                                             
         CLC   0(0,R4),0(R1)                                                    
         BE    VST8                                                             
*                                                                               
VST5     ICM   RF,1,8(R1)          TEST SHORT NAME                              
         BZ    VST6                                                             
         CR    R2,RF               YES                                          
         BL    VST6                                                             
         EX    RE,*+8              COMPARE TO SHORT NAME                        
         B     *+10                                                             
         CLC   0(0,R4),5(R1)                                                    
         BE    VST8                                                             
*                                                                               
VST6     LA    R1,L'STACKTAB(R1)                                                
         B     VST4                                                             
*                                                                               
VST7     BCT   R5,*+8                                                           
         B     VSTNO                                                            
         CLI   0(R4),C'D'          PREFIX OF D=DETAIL ONLY                      
         BNE   *+12                                                             
         OI    1(R3),X'80'                                                      
         B     *+16                                                             
         CLI   0(R4),C'T'          PREFIX OF T=TOTAL ONLY                       
         BNE   VSTNO                                                            
         OI    1(R3),X'40'                                                      
         LA    R4,1(R4)            TRY AGAIN                                    
         BCTR  RE,0                                                             
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BNP   VSTNO                                                            
         B     VST3                                                             
*                                                                               
VST8     MVC   0(1,R3),9(R1)       SAVE EXPRESSION NUMBER                       
         OC    OPTIND,10(R1)       BUY/GOAL INDICATOR                           
         OC    OPTIND2,11(R1)      SECOND OPTIONS INDICATOR                     
         OC    OPTIND5,12(R1)      FIFTH OPTIONS INDICATOR                      
         LA    R3,2(R3)                                                         
         LA    R4,1(R2,R4)         NEXT ONE                                     
         CLI   0(R4),C' '          TEST REACHED END                             
         BE    VSTX                                                             
         BCT   R0,VST2                                                          
         B     VSTNO               TOO MANY TERMS                               
*                                                                               
VSTNO    J     XITNEQ                                                           
*                                                                               
VSTX     J     XITEQU                                                           
         EJECT                                                                  
* ROUTINE TO VALIDATE THE DEMOS OPTION                                          
* INPUT  : R4=A(SCANNER BLOCK ENTRY)                                            
* OUTPUT : CC EQ - LIST OF DEMO CODES SET IN SBPDEMOS                           
*          CC NE - INVALID                                                      
*                                                                               
VALDEM   NTR1  ,                                                                
*                                                                               
         BAS   RE,GETTOKEN         GET COMSCORE TOKEN FOR CUSTOM DEMOS          
*                                                                               
         L     R0,AIO1                                                          
         LA    R1,4000                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AIO2                                                          
         LA    R1,4000                                                          
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)                                                       
         BZ    VDEMNO                                                           
*                                                                               
         L     R2,AIO1                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),22(R4)                                                   
         LA    RE,9(RE)                                                         
         STC   RE,0(R2)                                                         
*                                                                               
         L     R5,AIO2                                                          
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,SBQMED                                                  
*                                                                               
         CLI   AGYPROF+7-AGYHDRD(R1),C'C'   ESKIMOS?                            
         BNE   *+8                  NO                                          
         OI    DBVOPT,X'80'        VALIDATE AGAINST US DEMO LIST                
*                                                                               
         CLI   DBSELMED,C'N'                                                    
         BE    *+12                                                             
         CLI   DBSELMED,C'T'                                                    
         BNE   *+20                                                             
         LA    R1,SBAGYREC         CANADIAN AGY?                                
         CLI   AGYPROF+7-AGYHDRD(R1),C'C'                                       
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
*                                                                               
         MVC   DBSELAGY,AGENCY                                                  
         XC    SBPDEMOS,SBPDEMOS                                                
         LA    R3,L'SBPDEMOS/3-1                                                
         LA    R4,256(R2)          BUILD COMSCORE DEMO LIST HERE                
*                                                                               
         LR    R1,R5               DBLOCK IS USING AIO2                         
         AHI   R1,1024             DEMOVAL PARAM5 USING AIO2+1024               
         USING P5XD,R1             DSECT FOR P5                                 
         MVC   P5XID,=C'P5X '      PARAM 5 EXTENDED BLOCK                       
         LA    RE,ELEM             A(32 BYTE COMSCORE LICENSE)                  
         ST    RE,P5XLICNS         A(32 BYTE COMSCORE LICENSE)                  
         DROP  R1                  DROP P5 USING                                
*                                                                               
         ST    R1,DMCB+16          EXTENDED PARAM5                              
         OI    DMCB+16,X'80'       SO DEMOVAL KNOWS EXTENDED BLOCK              
         OI    DMCB+16,X'40'       COMSCORE DEMO INDEX SUPPORT                  
*                                                                               
         GOTO1 DEMOVAL,DMCB,(R2),((R3),SBPDEMOS),(C'S',DBLOCK),        *        
               (C'/',0),,(R4)                                                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,4(R1)                                                       
         BZ    VDEMNO                                                           
         MHI   RE,3                                                             
         LA    RE,SBPDEMOS(RE)                                                  
         MVI   0(RE),0                                                          
*                                                                               
         MVC   COMDLIST,0(R4)      COMSCORE DEMO LIST FROM DEMOVAL              
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    VDEMX               YES                                          
         CLI   ACTNUM,ACTREP       ACTION = REPORT?                             
         BNE   VDEMX               NO                                           
         OC    COMDLIST,COMDLIST   HAVE A COMSCORE DEMO IN THE LIST?            
         BZ    VDEMX               NO                                           
         MVI   REQSML,C'C'         COMSCORE REQUEST                             
         B     VDEMX                                                            
*                                                                               
VDEMNO   J     XITNEQ                                                           
*                                                                               
VDEMX    J     XITEQU                                                           
         DROP  R5                                                               
         EJECT                                                                  
BLNKS2   DC    CL132' '                                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GETTOKEN NTR1                                                                   
*                                                                               
         XC    ELEM,ELEM           SET TOKEN HERE                               
         MVC   HALF,AGENCY         AGENCY CODE                                  
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R4,KEY              R4 = KEY                                     
         USING CT5KEY,R4           SYSTEM ACCESS RECORD DSECT                   
         MVI   CT5KTYP,CT5KTYPQ    X'05'                                        
         MVC   CT5KALPH,AGENCY     AGENCY ALPHA                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO1                
         L     R4,AIO1             A(SYSTEM ACCESS RECORD)                      
         LA    R5,CT5DATA          X'01' ACTIVITY ELEMENT                       
         USING CTSEAD,R5           SECURITY AGENCY APLHA ID ELEMENT             
         MVI   ELCODE,CTSEAELQ     X'B8'                                        
         BRAS  RE,FIRSTEL          HAVE X'B8' ELEMENT?                          
         BNE   *+10                NO                                           
         MVC   HALF,CTSEAAID       YES - SET SECURITY AGENCY                    
         DROP  R5                  DROP R5                                      
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R4,KEY              R4 = KEY                                     
         USING TOKRECD,R4          BUILD TOKEN RECORD KEY                       
         MVI   TOKKMIN,TOKKMINQ    C'K'                                         
         MVI   TOKKTYP,TOKKRTRK    X'01'                                        
         MVC   TOKKSAGY,HALF       SECURITY AGENCY                              
         MVC   TOKKAAGY,AGENCY     AGENCY CODE                                  
         MVI   TOKKSYS,X'02'       SPOT SYSTEM                                  
         MVC   KEYSAVE,KEY         SAVE OFF THE KEY                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                 
*                                                                               
         CLC   KEY(L'TOKKEY),KEYSAVE    FOUND A MATCH?                          
         JNE   GTOKENX             NO - DONE                                    
*                                                                               
         L     R4,AIO1             A(RECORD)                                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,(R4),WORK             
*                                                                               
         AHI   R4,TOKFIRST         A(FIRST ELEMENT)                             
         USING RTAUTHD,R4          RENTRAK AUTHORIZATION ELEMENT                
         CLI   0(R4),RTAUTELQ      X'0A' ELEMENT?                               
         JNE   GTOKENX             NO - DONE                                    
         OC    RTAUTID,RTAUTID     HAVE USER ID TOKEN?                          
         BNZ   *+14                YES                                          
         OC    RTAUTSEC,RTAUTSEC   HAVE SECRET CODE TOKEN?                      
         BZ    GTOKENX             NO - DONE                                    
         MVC   ELEM(L'RTAUTID),RTAUTID  USER TOKEN FOR DEMOVAL                  
*                                                                               
GTOKENX  J     XIT                 EXIT                                         
         DROP  R4                  DROP R4                                      
         LTORG                                                                  
*                                                                               
DATA1TYP DS    0CL2                                                             
         DC    C'P',AL1(SBQDPUR)                                                
         DC    C'A',AL1(SBQDRERT)                                               
         DC    C'I',AL1(SBQDAFFD)                                               
         DC    C'O',AL1(SBQDORD)                                                
         DC    C'G',AL1(SBQDGOAL)                                               
N1TYPES  EQU   (*-DATA1TYP)/L'DATA1TYP                                          
*                                                                               
DATA3TYP DS    0CL4                                                             
         DC    C'GVP',AL1(SBQDGOAL+SBQDPUR)                                     
         DC    C'GVA',AL1(SBQDGOAL+SBQDRERT)                                    
         DC    C'GVI',AL1(SBQDGOAL+SBQDAFFD)                                    
         DC    C'PVA',AL1(SBQDPUR+SBQDRERT)                                     
         DC    C'PVI',AL1(SBQDPUR+SBQDAFFD)                                     
         DC    C'OVP',AL1(SBQDORD+SBQDPUR)                                      
         DC    C'OVA',AL1(SBQDORD+SBQDRERT)                                     
         DC    C'OVI',AL1(SBQDORD+SBQDAFFD)                                     
N3TYPES  EQU   (*-DATA3TYP)/L'DATA3TYP                                          
         SPACE 2                                                                
STACKTAB DS    0CL13                                                            
         DC    CL5'SPACE',CL2'  ',AL1(1,0,STSPACE,0,0,0)                        
         DC    CL5'DIFF ',CL2'  ',AL1(1,0,STDIFF,0,0,0)                         
         DC    CL5'INDEX',CL2'IX',AL1(2,2,STNDX,0,0,0)                          
         DC    CL5'GOAL ',CL2'GL',AL1(1,1,STGOAL,OPTISTGL,0,0)                  
         DC    CL5'GCPP ',CL2'  ',AL1(4,0,STGCPP,OPTISTGL,0,0)                  
         DC    CL5'ORDER',CL2'  ',AL1(1,0,STORD,OPTISTGL,0,0)                   
         DC    CL5'OCPP ',CL2'  ',AL1(4,0,STOCPP,OPTISTGL,0,0)                  
         DC    CL5'PURCH',CL2'  ',AL1(1,0,STPUR,OPTISTBY,0,0)                   
         DC    CL5'AVPCH',CL2'  ',AL1(3,0,STAVPCH,OPTISTBY,0,0)                 
         DC    CL5'PCPP ',CL2'  ',AL1(4,0,STPCPP,OPTISTBY,0,0)                  
         DC    CL5'PCPPN',CL2'  ',AL1(5,0,STPCPPN,OPTISTBY,0,0)                 
         DC    CL5'ACH  ',CL2'R ',AL1(3,1,STACH,OPTISTBY+OPTIRERT,0,0)          
         DC    CL5'AVACH',CL2'  '                                               
         DC    AL1(4,1,STAVACH,OPTISTBY+OPTIRERT,0,0)                           
         DC    CL5'RCPP ',CL2'  ',AL1(4,0,STRCPP,OPTISTBY+OPTIRERT,0,0)         
         DC    CL5'RCPPN',CL2'  '                                               
         DC    AL1(5,0,STRCPPN,OPTISTBY+OPTIRERT,0,0)                           
         DC    CL5'AFF  ',CL2'I ',AL1(1,1,STAFF,OPTISTBY+OPTIRERT,0,0)          
         DC    CL5'AVAFF',CL2'  '                                               
         DC    AL1(4,1,STAVAFF,OPTISTBY+OPTIRERT,0,0)                           
         DC    CL5'ICPP ',CL2'  ',AL1(4,0,STACPP,OPTISTBY+OPTIRERT,0,0)         
         DC    CL5'ICPPN',CL2'  '                                               
         DC    AL1(5,0,STACPPN,OPTISTBY+OPTIRERT,0,0)                           
         DC    CL5'GROSS',CL2'  ',AL1(2,0,STGROSS,OPTISTBY,0,0)                 
         DC    CL5'NET  ',CL2'  ',AL1(1,0,STNET,OPTISTBY,0,0)                   
         DC    CL5'SPOTS',CL2'  ',AL1(3,0,STSPOTS,OPTISTBY,0,0)                 
         DC    CL5'GDOL ',CL2'  ',AL1(2,0,STGDOL,OPTISTGL,0,0)                  
         DC    CL5'GNDOL',CL2'  ',AL1(2,0,STGNDOL,OPTISTGL,0,0)                 
*                                                                               
         DC    CL5'RTG  ',CL2'  ',AL1(3,0,STRTG,OPTISTBY,OPTISTR,0)             
         DC    CL5'CPP  ',CL2'  ',AL1(3,0,STCPP,OPTISTBY,OPTISTR,0)             
         DC    CL5'CPPN ',CL2'  ',AL1(4,0,STCPPN,OPTISTBY,OPTISTR,0)            
         DC    CL5'IMPS ',CL2'  ',AL1(3,0,STIMP,OPTISTBY,OPTISTI,0)             
         DC    CL5'CPM  ',CL2'  ',AL1(3,0,STCPM,OPTISTBY,OPTISTI,0)             
         DC    CL5'CPMN ',CL2'  ',AL1(4,0,STCPMN,OPTISTBY,OPTISTI,0)            
*                                                                               
         DC    CL5'BUY  ',CL2'  ',AL1(1,0,STBUY,OPTISTBY+OPTIACST,0,0)          
         DC    CL5'PAID ',CL2'PD',AL1(2,2,STPAID,OPTISTBY+OPTIACST,0,0)         
         DC    CL5'UNPD ',CL2'UP',AL1(1,1,STUNPD,OPTISTBY+OPTIACST,0,0)         
         DC    CL5'BILL ',CL2'BL',AL1(2,2,STBIL,OPTISTBL+OPTIACST,0,0)          
         DC    CL5'BILBL',CL2'BB'                                               
         DC    AL1(4,2,STBILBL,OPTISTBY+OPTISTBL+OPTIACST,0,0)                  
*                                                                               
         DC    CL5'PWPCT',CL2'  ',AL1(2,0,STPWPCT,OPTISTBY,0,0)                 
         DC    CL5'LKDOL',CL2'  ',AL1(2,0,STSLDOL,0,0,OPTISTLK)                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
* CL8 OPTION NAME                                                               
* SL2 S(ROUTINE)                                                                
* SL2 S(OPTION IND)                                                             
* AL1 OPTION EQU                                                                
*                                                                               
*                                                                               
OPTTABLE DC    C'BOX     ',SL2(OPTBOX),SL2(0),AL1(0)                            
OPTTABL  EQU   *-OPTTABLE                                                       
         DC    C'LEFT    ',SL2(OPTLEFT),SL2(0),AL1(0)                           
         DC    C'S       ',SL2(OPTSPACE),SL2(0),AL1(0)                          
         DC    C'DOWN    ',SL2(OPTDOWN),SL2(0),AL1(0)                           
         DC    C'DOWNTAPE',SL2(OPTDOWN),SL2(0),AL1(0)                           
         DC    C'DOWNHEAD',SL2(OPTDOWN),SL2(0),AL1(0)                           
         DC    C'DOWNTOT ',SL2(OPTDOWN),SL2(0),AL1(0)                           
         DC    C'DOWNTOTT',SL2(OPTDOWN),SL2(0),AL1(0)                           
         DC    C'DOWNFIX ',SL2(0),SL2(DOWNOPT2),AL1(GLD2FIX)                    
         DC    C'STRIP   ',SL2(0),SL2(DOWNOPT),AL1(GLDLSTRP)                    
         DC    C'NOTRUNC ',SL2(0),SL2(DOWNOPT),AL1(GLDLNOTR)                    
         DC    C'ALLALPH ',SL2(0),SL2(DOWNOPT),AL1(GLDLALPH)                    
         DC    C'CUT     ',SL2(0),SL2(DOWNOPT),AL1(GLDLCUT)                     
         DC    C'NOHEAD  ',SL2(0),SL2(DOWNOPT),AL1(GLDLNOHD)                    
         DC    C'WIDE    ',SL2(OPTWIDE),SL2(0),AL1(0)                           
         DC    C'TRACE   ',SL2(OPTTRACE),SL2(0),AL1(0)                          
         DC    C'IOTRACE ',SL2(OPTIOTRC),SL2(0),AL1(0)                          
         DC    C'NARROW  ',SL2(OPTNAROW),SL2(0),AL1(0)                          
         DC    C'POL     ',SL2(OPTPOL),SL2(0),AL1(0)                            
         DC    C'BK      ',SL2(OPTBK),SL2(0),AL1(0)                             
         DC    C'BK1     ',SL2(OPTBK),SL2(0),AL1(0)                             
         DC    C'BK2     ',SL2(OPTBK2),SL2(0),AL1(0)                            
         DC    C'BK3     ',SL2(OPTBK3),SL2(0),AL1(0)                            
         DC    C'BK4     ',SL2(OPTBK4),SL2(0),AL1(0)                            
         DC    C'BK5     ',SL2(OPTBK5),SL2(0),AL1(0)                            
         DC    C'BK6     ',SL2(OPTBK6),SL2(0),AL1(0)                            
         DC    C'UBK     ',SL2(OPTBKUNV),SL2(0),AL1(0)                          
         DC    C'DATA    ',SL2(OPTDATA),SL2(0),AL1(0)                           
         DC    C'MENU    ',SL2(OPTMENU),SL2(0),AL1(0)                           
         DC    C'SVI     ',SL2(OPTSVI),SL2(0),AL1(0)                            
         DC    C'PBSPLIT ',SL2(OPTPBSPL),SL2(0),AL1(0)                          
         DC    C'PB      ',SL2(OPTPB),SL2(0),AL1(0)                             
         DC    C'WGT     ',SL2(OPTWGT),SL2(0),AL1(0)                            
         DC    C'DEMO    ',SL2(OPTDEMO),SL2(0),AL1(0)                           
         DC    C'DPT     ',SL2(OPTDPT),SL2(0),AL1(0)                            
         DC    C'ROUND   ',SL2(0),SL2(COLIND),AL1(COLIRND)                      
         DC    C'RPT     ',SL2(OPTRPT),SL2(0),AL1(0)                            
         DC    C'SPILL   ',SL2(OPTSPILL),SL2(0),AL1(0)                          
         DC    C'DEMOPGRP',SL2(OPTDMGRP),SL2(0),AL1(0)                          
         DC    C'DEMROUND',SL2(OPTDMRND),SL2(0),AL1(0)                          
         DC    C'MAXREC  ',SL2(OPTMAX),SL2(0),AL1(0)                            
         DC    C'BD      ',SL2(OPTBD),SL2(0),AL1(0)                             
         DC    C'PD      ',SL2(OPTPD),SL2(0),AL1(0)                             
         DC    C'NETWORK ',SL2(OPTNTWRK),SL2(0),AL1(0)                          
         DC    C'BHIDATE ',SL2(OPTBHIDT),SL2(0),AL1(0)                          
         DC    C'BHRDATE ',SL2(OPTBHRDT),SL2(0),AL1(0)                          
         DC    C'BHDDATE ',SL2(OPTBHDDT),SL2(0),AL1(0)                          
         DC    C'BHPDATE ',SL2(OPTBHPDT),SL2(0),AL1(0)                          
         DC    C'BHEDATE ',SL2(OPTBHEDT),SL2(0),AL1(0)                          
         DC    C'BHTYPE  ',SL2(OPTBHTYP),SL2(0),AL1(0)                          
         DC    C'SOLID   ',SL2(0),SL2(DRINDS2),AL1(GLPWHOLE)                    
         DC    C'ALLTOT  ',SL2(0),SL2(DRINDS),AL1(GLPALTOT)                     
         DC    C'XBOX    ',SL2(0),SL2(DRINDS2),AL1(GLEXTBOX)                    
         DC    C'NODPLTOT',SL2(0),SL2(SBQDPTLN),AL1(SBQDLSUP)                   
         DC    C'NOLENTOT',SL2(OPTNOLNT),SL2(0),AL1(0)                          
         DC    C'SVC     ',SL2(OPTSVC),SL2(0),AL1(0)                            
         DC    C'FORCE   ',SL2(OPTFORCE),SL2(0),AL1(0)                          
         DC    C'PBREAK  ',SL2(0),SL2(DRINDS2),AL1(GLPBREAK)                    
         DC    C'PER     ',SL2(OPTPER),SL2(0),AL1(0)                            
         DC    C'COM     ',SL2(OPTCOM),SL2(0),AL1(0)                            
         DC    C'POST    ',SL2(0),SL2(SBABKLST),AL1(X'80')                      
         DC    C'STACK   ',SL2(OPTSTACK),SL2(0),AL1(0)                          
         DC    C'TOP     ',SL2(OPTTOP),SL2(0),AL1(0)                            
         DC    C'TOTSKIP ',SL2(0),SL2(OPTIND),AL1(OPTITOSK)                     
         DC    C'REP     ',SL2(0),SL2(OPTIND),AL1(OPTIREP)                      
         DC    C'DATE    ',SL2(OPTDATE),SL2(0),AL1(0)                           
         DC    C'ZERO    ',SL2(0),SL2(DRINDS),AL1(GLPALDET)                     
         DC    C'DF      ',SL2(OPTDF),SL2(0),AL1(0)                             
         DC    C'DEMOS   ',SL2(OPTDEMOS),SL2(0),AL1(0)                          
         DC    C'CUR     ',SL2(OPTCUR),SL2(0),AL1(0)                            
         DC    C'NOCENTS ',SL2(0),SL2(COLIND2),AL1(COLINOCT)                    
         DC    C'NOMKT   ',SL2(0),SL2(ROWIND),AL1(ROWINOMK)                     
         DC    C'UNALL   ',SL2(OPTUNALL),SL2(0),AL1(0)                          
         DC    C'TRUNC   ',SL2(0),SL2(OPTIND2),AL1(OPTITRUN)                    
         DC    C'WRITE   ',SL2(OPTWRITE),SL2(OPTIND2),AL1(OPTINOUP)             
         DC    C'GENAFF  ',SL2(0),SL2(OPTIND2),AL1(OPTIGENA)                    
         DC    C'YYMMDD  ',SL2(0),SL2(OPTIND2),AL1(OPTIYMD)                     
         DC    C'MMMDD/YY',SL2(0),SL2(OPTIND6),AL1(OPT6MDY)                     
         DC    C'CCYYMMDD',SL2(0),SL2(OPTIND3),AL1(OPTICYMD)                    
         DC    C'WGTZERO ',SL2(0),SL2(SBQDEMOP),AL1(SBQDOMWZ)                   
         DC    C'CML     ',SL2(OPTCML),SL2(0),AL1(0)                            
         DC    C'NOREQDET',SL2(OPTNORQ),SL2(OPTIND3),AL1(OPTINORQ)              
         DC    C'MGRCLT  ',SL2(OPTMGRCL),SL2(0),AL1(0)                          
         DC    C'XSPILL  ',SL2(0),SL2(OPTIND3),AL1(OPTIXSPL)                    
         DC    C'FLOAT   ',SL2(OPTFLOAT),SL2(OPTIND3),AL1(OPTIFLEM)             
         DC    C'EQU     ',SL2(OPTEQU),SL2(0),AL1(0)                            
         DC    C'MATCHTOT',SL2(0),SL2(OPTIND3),AL1(OPTIMTOT)                    
         DC    C'INFB    ',SL2(0),SL2(OPTIND3),AL1(OPTINFB)                     
         DC    C'CMLBLANK',SL2(0),SL2(OPTIND4),AL1(OPTCMLBK)                    
         DC    C'DMA     ',SL2(OPTDMA),SL2(0),AL1(0)                            
         DC    C'PST     ',SL2(0),SL2(SBEFLAG),AL1(SBEPST)                      
         DC    C'DISCL   ',SL2(0),SL2(OPTIND4),AL1(OPTDISCL)                    
         DC    C'ESTPOL  ',SL2(0),SL2(SBQPIND),AL1(SBQPOLES)                    
         DC    C'XFILE   ',SL2(OPTXFIL),SL2(0),AL1(0)                           
         DC    C'PRDBUY  ',SL2(0),SL2(OPTIND4),AL1(OPTPRDBY)                    
         DC    C'COLPD   ',SL2(OPTCOLPD),SL2(0),AL1(0)                          
         DC    C'XEST    ',SL2(OPTCROSS),SL2(0),AL1(0)                          
         DC    C'WKDY    ',SL2(OPTWKDY),SL2(0),AL1(0)                           
         DC    C'GRAND   ',SL2(OPTGRAND),SL2(0),AL1(0)                          
         DC    C'ORIG    ',SL2(OPTORIG),SL2(SBEFLAG2),AL1(SBEORIG)              
         DC    C'RFPDATE ',SL2(0),SL2(OPTIND5),AL1(OPTRFPDT)                    
         DC    C'WTP     ',SL2(0),SL2(SBEFLAG2),AL1(SBEWTP)                     
         DC    C'ISODATE ',SL2(0),SL2(OPTIND5),AL1(OPTISO)                      
         DC    C'PQIX    ',SL2(0),SL2(OPTIND5),AL1(OPTPQIX)                     
         DC    C'TRD     ',SL2(OPTTRD),SL2(0),AL1(0)                            
         DC    C'RECAP   ',SL2(0),SL2(OPTIND5),AL1(OPTRCAP)                     
         DC    C'NOWARN  ',SL2(0),SL2(OPTIND5),AL1(OPTNOWRN)                    
         DC    C'SQAD    ',SL2(OPTSQD1),SL2(0),AL1(0)                           
         DC    C'SQAD1   ',SL2(OPTSQD1),SL2(0),AL1(0)                           
         DC    C'SQAD2   ',SL2(OPTSQD2),SL2(0),AL1(0)                           
         DC    C'NOTAX   ',SL2(0),SL2(OPTIND5),AL1(OPTNOTAX)                    
         DC    C'UPT     ',SL2(OPTUPT),SL2(0),AL1(0)                            
         DC    C'UPTBK   ',SL2(OPTUPTBK),SL2(0),AL1(0)                          
         DC    C'ORTEST  ',SL2(0),SL2(DRINDS2),AL1(GLORTEST)                    
         DC    C'XCTEST  ',SL2(0),SL2(DRINDS2),AL1(GLXCTEST)                    
         DC    C'REPORT2 ',SL2(OPTRPT2),SL2(0),AL1(0)                           
         DC    C'CONTINUE',SL2(OPTCONT),SL2(0),AL1(0)                           
         DC    C'SQADTYP ',SL2(OPTSQTY),SL2(0),AL1(0)                           
         DC    C'ACTBK   ',SL2(OPTACTB),SL2(0),AL1(0)                           
         DC    C'NWKBUYS ',SL2(OPTMKT0),SL2(0),AL1(0)        CANADA             
         DC    C'BCOM    ',SL2(OPTBCOM),SL2(0),AL1(0)                           
         DC    C'OMNC    ',SL2(OPTNC),SL2(0),AL1(0)                             
         DC    C'OMND    ',SL2(OPTUND),SL2(0),AL1(0)                            
         DC    C'OMUND   ',SL2(OPTUND),SL2(0),AL1(0)                            
         DC    C'LPM     ',SL2(OPTLPM),SL2(0),AL1(0)                            
         DC    C'LPMWK   ',SL2(OPTWEEK),SL2(0),AL1(0)                           
         DC    C'OVN     ',SL2(OPTOV),SL2(0),AL1(0)                             
         DC    C'CBL     ',SL2(OPTCABLE),SL2(0),AL1(0)                          
         DC    C'NOVOID  ',SL2(0),SL2(SBEFLAG4),AL1(SBE4VOID)                   
         DC    C'REPORT2M',SL2(OPTRPT2),SL2(OPTIND6),AL1(OPT6RP2M)              
         DC    C'FILE    ',SL2(0),SL2(OPTIND6),AL1(OPT6FILE)                    
         DC    C'BTIMES  ',SL2(0),SL2(OPTIND6),AL1(OPT6BCST)                    
         DC    C'BFORM   ',SL2(0),SL2(SBEFLAG5),AL1(SBE5BFRM)                   
         DC    C'MTRADE  ',SL2(OPTMTRD),SL2(0),AL1(0)                           
         DC    C'DEMOCAT ',SL2(0),SL2(SBQDEMOP),AL1(SBQDDEMO)                   
         DC    C'ESTFILT ',SL2(OPTEFILT),SL2(0),AL1(0)                          
         DC    C'-OOW    ',SL2(0),SL2(SBEFLAG6),AL1(SBE6NOOW)                   
         DC    C'SQAD3   ',SL2(OPTSQD3),SL2(0),AL1(0)                           
         DC    C'SQAD4   ',SL2(OPTSQD4),SL2(0),AL1(0)                           
         DC    C'SQADLEN ',SL2(0),SL2(OPTIND6),AL1(OPT6SQAD)                    
         DC    C'NOFLOAT ',SL2(0),SL2(OPTIND7),AL1(OPT7NFLT)                    
         DC    C'MOFLOW  ',SL2(0),SL2(MOFLOW),AL1(C'Y')                         
         DC    C'BUYMG   ',SL2(OPTBUYMG),SL2(0),AL1(0)                          
         DC    C'WK      ',SL2(OPTWDP),SL2(0),AL1(0)                            
         DC    C'LKUP    ',SL2(OPTLKUP),SL2(0),AL1(0)                           
         DC    C'ML      ',SL2(OPTML),SL2(0),AL1(0)                             
         DC    C'MLOCK   ',SL2(OPTML),SL2(0),AL1(0)                             
         DC    C'GL      ',SL2(OPTGL),SL2(0),AL1(0)                             
         DC    C'GLOCK   ',SL2(OPTGL),SL2(0),AL1(0)                             
         DC    X'FF'                                                            
         EJECT                                                                  
*********************************************************************           
* VALRFP: VALIDATE RFP/RLP KEYWORD                                  *           
*  PASS R4 = A(SCAN BLOCK ENTRY)                                    *           
*                                                                   *           
*********************************************************************           
         SPACE                                                                  
VALRFP   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING QRFPD,R3                                                         
         MVI   QRFPMODE,QRFPSYMB   SYMBOLIC NAME VALIDATION                     
         XR    R1,R1                                                            
         ICM   R1,1,1(R4)                                                       
         BZ    VRFPNEQ                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),22(R4)  PASS SYMBOLIC NAME                           
         OC    QRFPWORK,=C'          '                                          
         GOTO1 RFP,DMCB,(R3)                                                    
         OC    QRFPWORK,QRFPWORK   ERROR                                        
         BZ    VRFPNEQ                                                          
         MVC   22(L'QRFPESC,R4),QRFPWORK                                        
*                                                                               
VRFPEQ   J     XITEQU                                                           
VRFPNEQ  J     XITNEQ                                                           
         EJECT                                                                  
*********************************************************************           
* VALRP2: VALIDATE REPORT2/CONTINUE OPTIONS                         *           
*  PASS R4 = A(SCAN BLOCK ENTRY)                                    *           
*                                                                   *           
*********************************************************************           
         SPACE                                                                  
VALRP2   NTR1  BASE=*,LABEL=*                                                   
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)          GET L'REPORT NAME                            
         JZ    XITNEQ                                                           
         CHI   RE,8                MAKE SURE IT'S NOT TOO LONG                  
         JH    XITNEQ                                                           
*                                                                               
         XC    KEY,KEY             READ REPORT KEY TO VERIFIFY                  
         LA    R2,KEY                                                           
         USING CT01RECD,R2         ESTABLISH PROGRAM RECORD                     
         MVI   CT01TYPE,CT01TYPQ   SET RECORD TYPE                              
         MVC   CT01AGID,AGENCY     SET AGENCY ID                                
         MVI   CT01SYS,2           SET FOR SPOT SYSTEM                          
         MVI   CT01PRG,4           SET FOR SPOT WRITER                          
         MVI   CT01PHAS,1          SET FOR PHASE 1                              
         MVC   CT01NAME,22(R4)     SECOND REPORT NAME                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'CTFILE' SET FILENAME                               
         MVI   USEIO,C'Y'                                                       
         GOTO1 HIGH                READ IN PROGRAM RECORD                       
*                                                                               
         XC    FILENAME,FILENAME   RESET FILENAME                               
         MVI   USEIO,C'N'                                                       
*                                                                               
         CLC   CT01KEY,KEYSAVE     MUST FIND IT                                 
         JNE   XITNEQ                                                           
         MVC   RPT2ID,CT01NAME     SAVE SECOND REPORT ID                        
         J     XITEQU                                                           
*                                                                               
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* DRONE UTILITIES                                                   *           
*********************************************************************           
         SPACE                                                                  
VUSRDRON NTR1  BASE=*,LABEL=*      VALIDATE USER RECORDS                        
         MVI   DRACTION,DRUSER                                                  
         MVC   DRUSRKEY(2),AGYALPHA         KEY IS AGENCY                       
         MVC   DRUSRKEY+2(8),22(R4)                AND USER CODE                
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   DRERROR,0                                                        
         JE    XITEQU                                                           
         MVC   GTMSGNO,=Y(USRERR)                                               
         J     XITNEQ                                                           
         DROP  RB                                                               
         LTORG                                                                  
*                                                                               
GROWDRON LR    R0,RE                                                            
         MVI   DRACTION,DRGENROW   GENERATE A ROW                               
         GOTO1 DRONE,DMCB,DRGEN                                                 
         LR    RE,R0                                                            
         BR    RE                  USER NEEDS TO TEST DRERROR                   
*                                                                               
GCOLDRON LR    R0,RE                                                            
         MVI   DRACTION,DRGENCOL   GENERATE A COLUMN                            
         GOTO1 DRONE,DMCB,DRGEN                                                 
         LR    RE,R0                                                            
         BR    RE                  USER NEEDS TO TEST DRERROR                   
*                                                                               
VROWDRON MVI   DRACTION,DRROW      VALIDATE A ROW                               
         J     ALLVAL                                                           
*                                                                               
VCOLDRON MVI   DRACTION,DRCOL      VALIDATE A COLUMN                            
         J     ALLVAL                                                           
*                                                                               
VCMPDRON MVI   DRACTION,DRCMP      VALIDATE A COMP                              
         J     ALLVAL                                                           
*                                                                               
GCMPDRON MVI   DRACTION,DRGENCMP   GENERATE A COMP                              
         J     ALLVAL              (LOOKS LIKE A VALIDATION)                    
*                                                                               
ALLVAL   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK           GENERATE A PSEUDO TWA HEADER                 
         ZICM  RE,0(R4),1                                                       
         JZ    XITNEQ              CAN'T HAVE LENGTH 0!                         
         STC   RE,WORK+5           PASS THROUGH LENGTH                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),12(R4)                                                 
         LA    R1,WORK                                                          
         ST    R1,DRSPTFLD                                                      
         OI    DRFLAGS,DREXPDIC    TELL DRONE TO EXPLODE DICT.                  
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   DRERROR,0                                                        
         BE    AVAL10                                                           
*                                                                               
         CLI   DRERROR,ERRSECUR    SECURITY ERROR?                              
         JNE   XITNEQ              NO                                           
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         J     TRAPERR                                                          
*                                                                               
AVAL10   BRAS  RE,SETREAD                                                       
         JNE   XITNEQ                                                           
         J     XITEQU                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO SET READ SWITCHES FROM DICTIONARY                      *           
* OUTPUT : CC EQ - OK                                               *           
*             NE - INVALID                                          *           
*********************************************************************           
         SPACE                                                                  
SETREAD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    SBEFLAG3,SBE3MS       ALREADY HAD MSTREET KEYWORD?               
         BNZ   SR00E                 YES, DO NOT READ SYSTEM ACCESS REC         
         LA    R1,WORK+8                                                        
         CLC   =C'MSFORMAT',0(R1)    MSTREET KEYWORD (ROW OR COLUMN)?           
         BE    SR00                  YES                                        
         CLC   =C'MSFREQ',0(R1)      MSTREET KEYWORD (ROW OR COLUMN)?           
         BE    SR00                  YES                                        
         CLC   =C'MSOWNER',0(R1)     MSTREET KEYWORD (ROW OR COLUMN)?           
         BE    SR00                  YES                                        
         CLC   =C'MSPARENT',0(R1)    MSTREET KEYWORD (ROW OR COLUMN)?           
         BE    SR00                  YES                                        
         CLC   =C'MSSTATE',0(R1)     MSTREET KEYWORD (ROW OR COLUMN)?           
         BE    SR00                  YES                                        
         CLC   =C'MSCHAN',0(R1)      MSTREET KEYWORD (ROW OR COLUMN)?           
         BE    SR00                  YES                                        
         CLC   =C'MSM',0(R1)         MSTREET MAILING ADDRESS KEYWORD?           
         BE    SR00                  YES                                        
         CLC   =C'MSFCC',0(R1)       MSFCCMIN/MSFCCFEM KEYWORDS?                
         BE    SR00                  YES                                        
         CLC   =C'MST',0(R1)         MSTREET TAPE ADDRESS KEYWORD?              
         BE    SR00                  YES                                        
         CLC   =C'MSNET',0(R1)       MSTREET KEYWORD (ROW OR COLUMN)?           
         BNE   SR00E                 NO                                         
*                                                                               
SR00     XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CT5REC,R1             SYSTEM ACCESS RECORD                       
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYALPHA                                                
         MVC   FILENAME,=CL8'CTFILE'                                            
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO3                                                         
         GOTO1 HIGH                                                             
         STC   R0,USEIO                                                         
         XC    FILENAME,FILENAME                                                
         CLC   KEY(L'CT5KEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                  SHOULD ALWAYS FIND THIS RECORD             
*                                                                               
         L     R1,AIO                                                           
         AHI   R1,CT5DATA-CT5REC                                                
*                                                                               
SR00A    CLI   0(R1),0               END OF RECORD?                             
         BNE   *+6                   NO                                         
         DC    H'0'                                                             
         CLI   0(R1),CTSYSELQ        SYSTEM AUTHORIZATION ELEMENT?              
         BE    SR00C                 YES                                        
SR00B    LLC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     SR00A                                                            
*                                                                               
         USING CTSYSD,R1                                                        
SR00C    CLI   CTSYSNUM,X'02'        HAVE SPOT'S SYSTEM NUMBER?                 
         BNE   SR00B                 NO, KEEP LOOKING                           
         TM    CTSYSIND,CTSYSRAD     AGENCY USES MSTREET?                       
         BZ    *+12                  NO                                         
         OI    SBEFLAG6,SBE6MS       YES - FLAG MSTREET                         
         B     SR00D                                                            
         TM    CTSYSIND,CTSYSMF      AGENCY USES MEDIA FRAMEWORKS?              
         BZ    *+12                  NO                                         
         OI    SBEFLAG6,SBE6MF       YES - FLAG MEDIA FRAMEWORKS                
         B     SR00D                                                            
         TM    CTSYSIND,CTSYSOWN     AGENCY USES SRDS OWNERSHIP?                
         BZ    *+12                  NO - ERROR                                 
         OI    SBEFLAG7,SBE7SRDS     YES - FLAG SRDS OWNERSHIP                  
         B     SR00D                                                            
         DROP  R1                                                               
*                                                                               
         MVC   GTMSGNO,=Y(NOMSDATA)  ACCESS REC DOESN'T ALLOW MSTREET           
         J     MYCURSOR                                                         
*                                                                               
SR00D    OI    SBEFLAG3,SBE3MS                                                  
*                                                                               
SR00E    CLC   WORK+8(2),=C'DL'    TEST DOWNLOAD ROW OR COLUMN                  
         BNE   SR0                                                              
         OI    DOWNOPT,GLDLACTV+GLDLALPH+GLDLNOTR YES-SET DOWNLOAD              
         OI    DOWNOPT,GLDLNOHD+GLDLHEAD+GLDLCUT      OPTIONS                   
         OI    DRINDS,GLPALDET                    PRINT ALL DETAILS             
         NI    SBQSKIP,X'FF'-SBQSKBUY             READ BUYS                     
         MVI   SBEUNALL,C'N'                      NO UNALLOCATED                
*                                                                               
SR0      CLI   DRACTION,DRCOL      TEST VALIDATING COLUMN                       
         BNE   SR14                                                             
         LA    R1,WORK+8           POINT TO KEYWORD                             
         CLC   =C'EXBILCST',0(R1)  EXBILCST KEYWORD?                            
         BE    *+14                YES                                          
         CLC   =C'EXBILNET',0(R1)  EXBILNET KEYWORD?                            
         BNE   *+8                 NO                                           
         OI    SBEFLAG6,SBE6PXBL   YES - PROGRAM EXCHANGE FOR BILLS             
         CLC   =C'EXBYNET',0(R1)   EXBYNET KEYWORD?                             
         BNE   *+8                 NO                                           
         OI    SBEFLAG6,SBE6PXBY   YES - PROGRAM EXCHANGE FOR BUYS              
*                                                                               
         CLC   =C'BILLC2',0(R1)    BILLC2 KEYWORD?                              
         BE    *+10                YES                                          
         CLC   =C'BILLNTC2',0(R1)  BILLNTC2 KEYWORD?                            
         BE    *+10                YES                                          
         CLC   =C'BILBLCC2',0(R1)  BILBLCC2 KEYWORD?                            
         BE    *+10                YES                                          
         CLC   =C'BILBLC2',0(R1)   BILBLC2 KEYWORD?                             
         BE    *+10                YES                                          
         CLC   =C'BILBLNC2',0(R1)  BILBLNC2 KEYWORD?                            
         BE    *+10                YES                                          
         CLC   =C'BILCOMC2',0(R1)  BILCOMC2 KEYWORD?                            
         BNE   *+8                 NO                                           
         OI    SBEFLAG7,SBE7BCO2   YES - REPORTING BILL COST2                   
*                                                                               
         LA    R1,DRARGSI                                                       
         LA    R0,2                                                             
*                                                                               
SR1      CLI   0(R1),C'B'          TEST COL DATA FROM BUY RECORD                
         BNE   *+8                                                              
         NI    SBQSKIP,X'FF'-SBQSKBUY   YES - READ BUYS                         
         CLI   0(R1),C'G'          TEST COL DATA FROM GOAL RECORD               
         BNE   *+8                                                              
         NI    SBQSKIP,X'FF'-SBQSKGL    YES - READ GOALS                        
         CLI   0(R1),C'X'          TEST COL DATA IS DEMO INDEX                  
         BNE   *+8                                                              
         NI    SBQSKIP,X'FF'-SBQSKBUY   YES - READ BUYS                         
         CLI   0(R1),C'S'          TEST COL DATA IS STATION BILL                
         BNE   *+12                                                             
         NI    SBQSKIP,X'FF'-SBQSKBIL   YES - READ STATION BILL RECORDS         
         OI    SBQRDOPT,SBQROSTA   SKIP CUR=X'01'                               
         CLI   0(R1),C'H'          TEST COL DATA IS BILL HEADER                 
         BNE   *+8                                                              
         OI    SBQREAD,SBQRDBH     YES-READ BILL HEADER RECORDS                 
         CLI   0(R1),C'L'          TEST COL DATA IS STATION LOCKIN              
         BNE   *+8                  NO                                          
         OI    SBQREAD,SBQRDSLK    YES-READ STATION LOCKIN RECORDS              
         CLI   0(R1),C'P'          TEST COL DATA IS PG ESTIMATE                 
         BNE   *+8                                                              
         OI    SBQREAD,SBQRDPG     YES-READ PG ESTIMATE RECORDS                 
         CLI   0(R1),C'I'          TEST COL DATA IS INFOMERCIAL                 
         BNE   *+8                                                              
         OI    SBQREAD,SBQRDINF    YES                                          
         CLI   0(R1),C'V'          TEST COL DATA IS INVOICE                     
         BNE   *+8                                                              
         OI    SBQREAD,SBQRDINV    YES-READ INVOICE RECORDS                     
         CLI   0(R1),C'O'          OM KEYWORD?                                  
         BNE   SR1A                                                             
         OI    SBQREAD2,SBQRD2OM   YES-READ OM RECORDS                          
         B     SR1B                DON'T SET SBQSKBUY!!!                        
*                                                                               
SR1A     LA    R1,1(R1)                                                         
         BCT   R0,SR1                                                           
*                                                                               
SR1B     CLC   =C'DBOOK',WORK+8     THIS A DBOOK COL?                           
         BNE   *+8                                                              
         OI    SBEFLAG2,SBEBOOK                                                 
*                                                                               
         CLC   DRRTNI,=C'IBILNVDT'  IS THIS BILLING INVOICE DATA?               
         BNE   *+8                                                              
         OI    SBQREAD2,SBQRD2BH   SET TO PASS BH RECS 1ST FOR TABLE            
*                                                                               
         CLC   =C'CLCPS',WORK+8    CLT LOCK CPS?                                
         BE    *+10                                                             
         CLC   =C'CLCPP',WORK+8    CLT LOCK/WIM LOCK CPP?                       
         BE    *+14                                                             
         CLC   =C'WLCPP',WORK+8                                                 
         BNE   *+12                                                             
         OI    SBQREAD,SBQRDSLK    YES-READ STATION LOCKIN RECORDS              
         NI    SBQSKIP,X'FF'-SBQSKBUY  READ BUYS TO GET PW...                   
*                                                                               
         LA    R1,WORK+8           TEST FOR DEMO OR CPP COLUMN                  
         CLC   0(6,R1),=C'STDATA'                                               
         BE    SR8                                                              
         CLC   0(3,R1),=C'STD'                                                  
         BE    SR3                                                              
         CLC   0(5,R1),=C'BYPAR'                                                
         BE    SR3                                                              
         CLC   0(5,R1),=C'BYRAR'                                                
         BE    SR3                                                              
         CLC   0(5,R1),=C'BYAAR'                                                
         BE    SR3                                                              
         LA    R0,6                                                             
*                                                                               
SR2      CLC   0(3,R1),=C'DEM'                                                  
         BE    SR3                                                              
         CLC   0(3,R1),=C'CPP'                                                  
         BE    SR3                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,SR2                                                           
         B     SR7                                                              
*                                                                               
SR3      CLI   DRATTRIB+2,C'D'     YES - INDICATE DEMO/CPP                      
         BNE   SR4                                                              
         CLI   DRATTRIB+3,1        TEST PRIMARY DEMO                            
         BNE   *+12                                                             
         OI    COLIND,COLIDEM1     YES                                          
         B     SR5A                                                             
         CLI   DRATTRIB+3,2        TEST SECONDARY DEMO                          
         BNE   SR4                                                              
         OI    COLIND,COLIDEM2     YES                                          
         B     SR5                                                              
*                                                                               
SR4      OI    COLIND,COLIDEM3     OTHERWISE, DEMO3 OR MORE                     
*                                                                               
SR5      CLC   WORK+8(3),=C'STD'   DEMO2 OR MORE-TEST STACK                     
         BNE   SR5A                                                             
         TM    OPTIND,OPTISTGL     YES-TEST GOALS IS STACK                      
         BZ    SR5A                                                             
         OI    COLIND,COLIGDEM     YES-INDICATE DEMO MENU MUST BE SET           
*                                                                               
SR5A     CLC   WORK+8(3),=C'STD'   TEST DEMO STACK,                             
         BNE   SR5B                                                             
         TM    OPTIND,OPTIRERT     WITH RERATE DEMOS                            
         BO    SR6                                                              
         CLI   WORK+11,C'R'        OR BUY RERATE DEMO STACK                     
         BE    SR6                                                              
         CLI   WORK+11,C'A'                                                     
         BE    SR6                                                              
         B     SR7                                                              
*                                                                               
SR5B     CLI   DRARGSI,C'B'        OR BUY RERATE COLUMN                         
         BNE   SR7                                                              
         CLI   DRARGSI+2,C'R'                                                   
         BE    SR6                                                              
         CLI   DRARGSI+2,C'A'                                                   
         BNE   SR7                                                              
SR6      OI    COLIND2,COLIRERT    YES-INDICATE THERE ARE RERATE DEMOS          
*                                                                               
SR7      CLC   WORK+8(3),=C'STD'   TEST BUY DEMO STACK                          
         BNE   SR8                                                              
         CLI   DRARGSI,C'B'                                                     
         BNE   SR8                                                              
         LA    RE,256              YES-SET BUY DEMO STACK BIT MAP               
         LLC   RF,DRARGSI+1            X'80' - STACK FOR DEMO 1                 
         SRL   RE,1                    X'40' - STACK FOR DEMO 2                 
         BCT   RF,*-4                  ETC.                                     
         LLC   RF,BYDEMSTK                                                      
         OR    RF,RE                                                            
         STC   RF,BYDEMSTK                                                      
*                                                                               
SR8      CLC   WORK+8(5),=C'SPOTS' TEST SPOTS                                   
         BE    SR10                                                             
         LA    R0,6                                                             
         LA    R1,WORK+8                                                        
*                                                                               
SR9      CLC   0(3,R1),=C'SPT'                                                  
         BE    SR10                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,SR9                                                           
         B     SR11                                                             
*                                                                               
SR10     OI    COLIND,COLISPT      YES-INDICATE SPOTS                           
*                                                                               
SR11     CLC   WORK+8(5),=C'GLDEM' TEST GOAL DEMO                               
         BNE   *+16                                                             
         CLI   WORK+13,C'1'        YES-TEST DEMO NUM IS 2 OR MORE               
         BNH   *+8                                                              
         OI    COLIND,COLIGDEM     YES-INDICATE DEMO MENU MUST BE SET           
*                                                                               
         CLC   WORK+8(5),=C'BILBL' TEST BILLABLE COLUMN                         
         BE    *+18                                                             
         CLC   WORK+8(7),=C'COMPUTE' OR COMPUTE                                 
         BE    *+8                                                              
         NI    COLIND2,255-COLIBLBL  NO-COLUMN OTHER THAN BILLABLE              
*                                                                               
         CLC   WORK+8(2),=C'ST'    TEST COLUMN IS A STACK                       
         BNE   SRX                                                              
         CLC   WORK+10(4),=C'DATA'                                              
         BE    SRX                                                              
         CLI   WORK+10,C'$'        YES-                                         
         BNE   *+12                                                             
         OI    COLIND2,COLISDOL    MEDIA DOLLAR STACK                           
         B     SR12                                                             
         CLI   WORK+10,C'D'                                                     
         BNE   *+12                                                             
         OI    COLIND2,COLISDEM    MEDIA DEMO STACK                             
         B     SR12                                                             
         OI    COLIND2,COLISACC    ACCOUNTING STACK                             
*                                                                               
SR12     TM    OPTIND,OPTISTGL     TEST GOALS IN STACK DEFINITION               
         BZ    *+8                                                              
         NI    SBQSKIP,255-SBQSKGL YES-READ GOALS                               
         TM    OPTIND,OPTISTBY     TEST BUYS IN STACK DEFINITION                
         BZ    *+8                                                              
         NI    SBQSKIP,255-SBQSKBUY YES-READ BUYS                               
         TM    OPTIND,OPTISTBL     TEST BILLS IN STACK DEFINITION               
         BZ    *+12                                                             
         NI    SBQSKIP,255-SBQSKBIL YES-READ STATION BILLS                      
         OI    SBQRDOPT,SBQROSTA   SKIP CUR=X'01'                               
         TM    OPTIND5,OPTISTLK    TEST STATION LOCKIN IN STACK DEFN            
         BZ    *+8                                                              
         OI    SBQREAD,SBQRDSLK    YES-READ STATION LOCKIN RECORDS              
         B     SRX                                                              
*                                                                               
SR14     CLI   DRACTION,DRROW      TEST VALIDATING ROW                          
         BNE   SRX                 NO                                           
         CLC   DRRTNI,=C'IBILNVDT'  IS THIS BILLING INVOICE DATA?               
         BNE   *+8                                                              
         OI    SBQREAD2,SBQRD2BH   SET TO PASS BH RECS 1ST FOR TABLE            
         LA    R1,LEVELS           FIND CURRENT LEVEL                           
         LA    R0,L'LEVELS                                                      
         LA    RE,1                                                             
*                                                                               
SR16     CLI   0(R1),0                                                          
         BE    SR18                                                             
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,SR16                                                          
*        DC    H'0'                                                             
         B     SRX                                                              
*                                                                               
SR18     CLI   MYPOSO,C'H'         TEST HEADLINE                                
         BNE   *+8                                                              
         STC   RE,LSTHEDLV         YES - SAVE LAST HEADLINE LEVEL               
         CLI   MYPOSO,C'M'         TEST MIDLINE                                 
         BNE   *+8                                                              
         STC   RE,MIDLEV           YES - SAVE MIDLINE LEVEL                     
         LA    RE,LEVTABLE                                                      
         SR    RF,RF                                                            
*                                                                               
SR20     ICM   RF,1,0(RE)                                                       
         BZ    SRXNE                                                            
         BCTR  RF,0                                                             
         EX    RF,SRCLC            COMPARE ROW ENTRY NAME                       
         BE    *+12                                                             
         LA    RE,3(RF,RE)                                                      
         B     SR20                                                             
         MVC   0(1,R1),1(RE)       SET DATA TYPE FOR THIS LEVEL                 
         LA    RE,LEVELS                                                        
         LR    RF,R1                                                            
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         CLI   0(R1),QMKT          SET MARKET LEVEL                             
         BE    SR22                                                             
         CLI   0(R1),QMKTNM                                                     
         BE    SR22                                                             
         CLI   0(R1),QMKTR                                                      
         BE    SR22                                                             
         CLI   0(R1),QMKTRNM                                                    
         BNE   SR24                                                             
*                                                                               
SR22     STC   RF,MKTLEV                                                        
         B     SRX                                                              
*                                                                               
SR24     CLI   0(R1),QMKTRNK       SET MARKET RANK LEVEL                        
         BNE   SR30                                                             
         STC   RF,MKRKLEV                                                       
         B     SRX                                                              
*                                                                               
SR30     CLI   0(R1),QDPT          TEST DAYPART                                 
         BNE   SR32                                                             
         BCTR  RF,0                YES - SET LEVEL ABOVE DPT                    
         STC   RF,INDPTLEV                                                      
         OI    ROWIND,ROWIDPT      SET DAYPART IS A ROW                         
         B     SRX                                                              
*                                                                               
SR32     CLI   0(R1),QDPTLEN       TEST DAYPART/LENGTH                          
         BNE   SR34                                                             
         BCTR  RF,0                YES - SET LEVEL ABOVE DPTLEN                 
         STC   RF,INDPTLEV                                                      
         OI    ROWIND,ROWIDPLN     SET DAYPART/LENGTH IS A ROW                  
         B     SRX                                                              
*                                                                               
SR34     CLI   0(R1),QCMLCLNM      TEST COMMERCIAL CLASS NAME                   
         BNE   *+12                                                             
         OI    ROWIND2,ROWICLAS    CLASS NAME INDICATOR                         
         B     SR35                                                             
         CLI   0(R1),QCMLCLS       OR COMMERCIAL CLASS                          
         BE    SR35                                                             
         CLI   0(R1),QCMLNUM       OR COMMERCIAL CLT NUM                        
         BE    SR35                                                             
         CLI   0(R1),QCMLNM        OR COMMERCIAL NAME/2/3                       
         BE    SR35                                                             
         CLI   0(R1),QHDCML        OR HD CMML CLASS NAME                        
         BNE   *+12                                                             
         OI    ROWIND2,ROWICLAS    CLASS NAME INDICATOR                         
         B     SR35                                                             
         CLI   0(R1),QCML          OR COMMERCIAL                                
         BNE   SR35A                                                            
         TM    SBEFLAG4,SBEADID    HAVE AD-ID KEYWORD ALREADY?                  
         BNZ   SR35ERR             YES, YOU STUPID USER!                        
         TM    SBEFLAG5,SBE5HDEF+SBE5CNTR   HAVE HDEF OR CNTR?                  
         BNZ   SR35ERR             YES - ERROR!                                 
         OI    SBEFLAG4,SBECOMM                                                 
*                                                                               
SR35     OI    ROWIND,ROWICML      YES                                          
         B     SRX                                                              
*                                                                               
SR35A    CLI   0(R1),QADID         HAVE AD-ID KEYWORD?                          
         BNE   SR35B               NO                                           
         TM    SBEFLAG4,SBECOMM    HAVE COMMERCIAL KEYWORD ALREADY?             
         BNZ   SR35ERR             YES, YOU STUPID USER!                        
         TM    SBEFLAG5,SBE5HDEF+SBE5CNTR   HAVE HDEF OR CNTR?                  
         BNZ   SR35ERR             YES - ERROR!                                 
         OI    SBEFLAG4,SBEADID                                                 
         OI    ROWIND,ROWICML                                                   
         B     SRX                                                              
*                                                                               
SR35B    CLI   0(R1),QHDEF         HAVE HDEF KEYWORD?                           
         BNE   SR35C               NO                                           
         TM    SBEFLAG4,SBECOMM+SBEADID   HAVE CMML/AD-ID KYWD?                 
         BNZ   SR35ERR             YES - ERROR!                                 
         TM    SBEFLAG5,SBE5CNTR   HAVE CENTERCUT?                              
         BNZ   SR35ERR             YES - ERROR!                                 
         OI    SBEFLAG5,SBE5HDEF                                                
         OI    ROWIND,ROWICML                                                   
         B     SRX                                                              
*                                                                               
SR35C    CLI   0(R1),QCNTR         HAVE CNTR KEYWORD?                           
         BNE   SR35D               NO                                           
         TM    SBEFLAG4,SBECOMM+SBEADID   HAVE CMML/AD-ID KYWD?                 
         BNZ   SR35ERR             YES - ERROR!                                 
         TM    SBEFLAG5,SBE5HDEF   HAVE HIGHDEF?                                
         BNZ   SR35ERR             YES - ERROR!                                 
         OI    SBEFLAG5,SBE5CNTR                                                
         OI    ROWIND,ROWICML                                                   
         B     SRX                                                              
*                                                                               
SR35ERR  MVC   GTMSGNO,=Y(ADIDCML) CAN'T MIX AD-ID W/COMMERCIAL KEYWORD         
         J     MYCURSOR                                                         
*                                                                               
SR35D    CLI   0(R1),QTARGET       TEST TARGET                                  
         BE    *+12                                                             
         CLI   0(R1),QTARGET2                                                   
         BNE   SR35E                                                            
         OI    ROWIND,ROWITGT      YES                                          
         B     SRX                                                              
*                                                                               
SR35E    CLI   0(R1),QTARGET3      TARGET3?                                     
         BE    *+12                YES                                          
         CLI   0(R1),QTARGET4      TARGET4?                                     
         BNE   SR36                NO                                           
         OI    ROWIND2,ROWITGT2    SET USING TARGET3/4 FLAG                     
         B     SRX                 DONE                                         
*                                                                               
SR36     CLI   0(R1),QNETSTA       TEST NETWORK/STATION                         
         BNE   *+12                                                             
         OI    DATAIND6,DINETSTA                                                
         B     SRX                                                              
         CLI   0(R1),QSTA          TEST STATION                                 
         BNE   SR38                                                             
         OI    DATAIND2,DISTA                                                   
         B     SRX                                                              
*                                                                               
SR38     CLI   0(R1),QCLT          TEST CLIENT                                  
         BE    *+12                                                             
         CLI   0(R1),QCLTNM                                                     
         BNE   SR40                                                             
         OI    ROWIND2,ROWICLT                                                  
         B     SRX                                                              
*                                                                               
SR40     CLI   0(R1),QPRD          TEST PRODUCT                                 
         BE    *+12                                                             
         CLI   0(R1),QPRDNM                                                     
         BNE   SR42                                                             
         OI    ROWIND2,ROWIPRD                                                  
         B     SRX                                                              
*                                                                               
SR42     CLI   0(R1),QEST          TEST ESTIMATE                                
         BE    *+12                                                             
         CLI   0(R1),QESTNM                                                     
         BNE   SR44                                                             
         OI    ROWIND2,ROWIEST                                                  
         B     SRX                                                              
*                                                                               
SR44     CLI   0(R1),QINVNO        TEST INVOICE NUMBER                          
         BE    *+12                                                             
         CLI   0(R1),QINVDATE       OR INVOICE DATE/DUE DATE                    
         BNE   SR46                                                             
         OI    SBQREAD,SBQRDINV    YES-READ INVOICE RECORDS                     
         B     SRX                                                              
*                                                                               
SR46     CLC   WORK+8(4),=C'WEEK'  THIS A WEEK ROW?                             
         BNE   SR47                                                             
         STC   RF,WEEKLEV                                                       
         B     SRX                                                              
*                                                                               
SR47     DS    0H                                                               
         CLI   0(R1),QBLIDATE      IS THIS BILLING INVOICE DATE?                
         BE    SR47A                                                            
         CLI   0(R1),QBLEDATE      IS THIS BILLING EDI     DATE?                
         BNE   SR48                                                             
SR47A    DS    0H                                                               
*                                                                               
         OI    SBQREAD2,SBQRD2BH   SET TO PASS BH RECS 2ST FOR TABLE            
         B     SRX                                                              
*                                                                               
SR48     CLC   WORK+8(6),=C'BYIDMG' BYID FOR LCI?                               
         BNE   SR50                                                             
         OI    SBEFLAG4,SBE4GSTA   NEED FULL STATION DETAILS?                   
         B     SRX                                                              
*                                                                               
SR50     DS    0H                                                               
         B     SRX                                                              
*                                                                               
SRXNE    J     XITNEQ                                                           
*                                                                               
SRX      J     XITEQU                                                           
*                                                                               
SRCLC    CLC   WORK+8(0),2(RE)     EXECUTED                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE                                                                  
LEVTABLE DS    0H                                                               
         DC    AL1(3),AL1(QMED),CL3'MED'                                        
         DC    AL1(5),AL1(QMED),CL5'WLMED'                                      
         DC    AL1(6),AL1(QSUBMED),CL6'SUBMED'                                  
         DC    AL1(6),AL1(QCLTACOF),CL6'CLTACC'                                 
         DC    AL1(7),AL1(QCLTACOF),CL7'CLTACOF'                                
         DC    AL1(6),AL1(QCLTPCT),CL6'CLTPCT'                                  
         DC    AL1(6),AL1(QCLTOFF),CL6'CLTOFF'                                  
         DC    AL1(7),AL1(QCLTOFF),CL7'XCLTOFF'                                 
         DC    AL1(7),AL1(QCLTNM),CL7'CLTNAME'                                  
         DC    AL1(6),AL1(QCLTINT),CL6'CLTINT'                                  
         DC    AL1(3),AL1(QCLT),CL3'CLT'                                        
         DC    AL1(7),AL1(QPRDNM),CL7'PRDNAME'                                  
         DC    AL1(8),AL1(QPRDNM),CL8'XPRDNAME'                                 
         DC    AL1(3),AL1(QPRD),CL3'PRD'                                        
         DC    AL1(7),AL1(QESTNM),CL7'ESTNAME'                                  
         DC    AL1(7),AL1(QESTFLT),CL7'ESTFILT'                                 
         DC    AL1(6),AL1(QESTLIN),CL6'ESTLIN'                                  
         DC    AL1(6),AL1(QESTPCT),CL6'ESTPCT'                                  
         DC    AL1(3),AL1(QEST),CL3'EST'                                        
         DC    AL1(5),AL1(QEST),CL5'PGEST'                                      
         DC    AL1(7),AL1(QMKTNM),CL7'MKTNAME'                                  
         DC    AL1(8),AL1(QMKTNM),CL8'XMKTNAME'                                 
         DC    AL1(6),AL1(QMKTRNK),CL6'MKTRNK'                                  
         DC    AL1(8),AL1(QMKTRNM),CL8'MKTRNAME'                                
         DC    AL1(5),AL1(QMKTWT),CL5'MKTWT'                                    
         DC    AL1(4),AL1(QMKTR),CL4'MKTR'                                      
         DC    AL1(6),AL1(QRSM),CL6'MKTARB'                                     
         DC    AL1(6),AL1(QRSM),CL6'MKTNSI'                                     
         DC    AL1(4),AL1(QMKTA),CL4'MKTA'                                      
         DC    AL1(3),AL1(QMKT),CL3'MKT'                                        
         DC    AL1(6),AL1(QINVNUM),CL6'INVNUM'                                  
         DC    AL1(7),AL1(QSTAFFCH),CL7'STAFFCH'                                
         DC    AL1(7),AL1(QSTANM),CL7'STANAME'                                  
         DC    AL1(7),AL1(QSTACITY),CL7'STACITY'                                
         DC    AL1(8),AL1(QSTAST),CL8'STASTATE'                                 
         DC    AL1(7),AL1(QSTAADDR),CL7'STAADDR'                                
         DC    AL1(6),AL1(QSTAAD),CL6'STAADD'                                   
         DC    AL1(6),AL1(QSTAFAX),CL6'STAFAX'                                  
         DC    AL1(6),AL1(QSTATYP),CL6'STATYP'                                  
         DC    AL1(6),AL1(QSTAZIP),CL6'STAZIP'                                  
         DC    AL1(3),AL1(QSTA),CL3'STA'                                        
         DC    AL1(4),AL1(QCNET),CL4'CNET'                                      
         DC    AL1(5),AL1(QCCNET),CL5'CCNET'                                    
         DC    AL1(6),AL1(QNETSTA),CL6'NETSTA'                                  
         DC    AL1(7),AL1(QSTA),CL7'NETWORK'                                    
         DC    AL1(5),AL1(QAFFIL),CL5'AFFIL'                                    
         DC    AL1(4),AL1(QBOOK),CL4'BOOK'                                      
         DC    AL1(4),AL1(QCHAN),CL4'CHAN'                                      
         DC    AL1(4),AL1(QSIZE),CL4'SIZE'                                      
         DC    AL1(6),AL1(QFORMAT),CL6'FORMAT'                                  
         DC    AL1(3),AL1(QMSONM),CL3'MSO'                                      
         DC    AL1(2),AL1(QICNM),CL2'IT'                                        
         DC    AL1(3),AL1(QIDR),CL3'IDR'                                        
         DC    AL1(7),AL1(QBYCDAT),CL7'BYCDATE'                                 
         DC    AL1(8),AL1(QBYCDAT),CL8'BYCHDATE'                                
         DC    AL1(7),AL1(QBYDATES),CL7'BYDATES'                                
         DC    AL1(5),AL1(QBYWKS),CL5'BYWKS'                                    
         DC    AL1(4),AL1(QBUYID),CL4'BYID'                                     
         DC    AL1(7),AL1(QBUYID),CL7'PURPOSE'                                  
         DC    AL1(3),AL1(QBUY),CL3'BUY'                                        
         DC    AL1(4),AL1(QPROG),CL4'PROG'                                      
         DC    AL1(5),AL1(QTIMES),CL5'TIMES'                                    
         DC    AL1(6),AL1(QTIMES),CL6'MTIMES'                                   
         DC    AL1(3),AL1(QROT),CL3'ROT'                                        
         DC    AL1(6),AL1(QBYTYPE),CL6'BYTYPE'                                  
         DC    AL1(7),AL1(QSPCLREP),CL7'SPCLREP'                                
         DC    AL1(6),AL1(QDPTLEN),CL6'DPTLEN'                                  
         DC    AL1(3),AL1(QDPT),CL3'DPT'                                        
         DC    AL1(3),AL1(QLEN),CL3'LEN'                                        
         DC    AL1(4),AL1(QPER),CL4'YEAR'                                       
         DC    AL1(3),AL1(QPER),CL3'QTR'                                        
         DC    AL1(3),AL1(QPER),CL3'MON'                                        
         DC    AL1(6),AL1(QPER),CL6'AVGMON'                                     
         DC    AL1(4),AL1(QPER),CL4'WEEK'                                       
         DC    AL1(3),AL1(QPER),CL3'DAY'                                        
         DC    AL1(8),AL1(QPER),CL8'WKYYMMDD'                                   
         DC    AL1(6),AL1(QPER),CL6'FLIGHT'                                     
         DC    AL1(5),AL1(QPER),CL5'CFMON'                                      
         DC    AL1(7),AL1(QPER),CL7'RUNDATE'                                    
         DC    AL1(7),AL1(QPER),CL7'ECREATE'                                    
         DC    AL1(4),AL1(QCLTGR1),CL4'CGR1'                                    
         DC    AL1(4),AL1(QCLTGR2),CL4'CGR2'                                    
         DC    AL1(4),AL1(QPRDGR1),CL4'PGR1'                                    
         DC    AL1(4),AL1(QPRDGR2),CL4'PGR2'                                    
         DC    AL1(4),AL1(QMKTGR1),CL4'MGR1'                                    
         DC    AL1(4),AL1(QMKTGR2),CL4'MGR2'                                    
         DC    AL1(4),AL1(QMKTGR3),CL4'MGR3'                                    
         DC    AL1(4),AL1(QSTAGR1),CL4'SGR1'                                    
         DC    AL1(4),AL1(QSTAGR2),CL4'SGR2'                                    
         DC    AL1(7),AL1(QNAMES),CL7'BYRNAME'                                  
         DC    AL1(7),AL1(QNAMES),CL7'BLRNAME'                                  
         DC    AL1(4),AL1(QRANK),CL4'RANK'                                      
         DC    AL1(7),AL1(QPRDINT),CL7'PRODINT'                                 
         DC    AL1(7),AL1(QTARGET2),CL7'TARGET2'                                
         DC    AL1(6),AL1(QTARGET),CL6'TARGET'                                  
         DC    AL1(4),AL1(QSREP),CL4'SREP'                                      
         DC    AL1(4),AL1(QPREP),CL4'PREP'                                      
         DC    AL1(4),AL1(QTREP),CL4'TREP'                                      
         DC    AL1(8),AL1(QCMLCLS),CL8'CMLCLASS'                                
         DC    AL1(8),AL1(QCMLCLNM),CL8'CMLCLSNM'                               
         DC    AL1(8),AL1(QCMLCLNM),CL8'CMLCCDNM'                               
         DC    AL1(6),AL1(QCMLNUM),CL6'CMLNUM'                                  
         DC    AL1(5),AL1(QCMLNM),CL5'CMLNM'                                    
         DC    AL1(3),AL1(QCML),CL3'CML'                                        
         DC    AL1(3),AL1(QADJ),CL3'ADJ'                                        
         DC    AL1(4),AL1(QADAY),CL4'ADAY'                                      
         DC    AL1(6),AL1(QADATER),CL6'ADATER'                                  
         DC    AL1(6),AL1(QADATED),CL6'ADATED'                                  
         DC    AL1(5),AL1(QDATE),CL5'ADATE'                                     
         DC    AL1(6),AL1(QATIMEM),CL6'ATIMEM'                                  
         DC    AL1(5),AL1(QTIME),CL5'ATIME'                                     
         DC    AL1(5),AL1(QAPROG),CL5'APROG'                                    
         DC    AL1(6),AL1(QRPTSEQ),CL6'RPTSEQ'                                  
         DC    AL1(4),AL1(QTEXT),CL4'TEXT'                                      
         DC    AL1(8),AL1(QBHAORTP),CL8'BHAORTYP'                               
         DC    AL1(8),AL1(QBHCOMTP),CL8'BHCOMTYP'                               
         DC    AL1(5),AL1(QBHINV),CL5'BHINV'                                    
         DC    AL1(5),AL1(QBHINV),CL5'WLINV'                                    
         DC    AL1(6),AL1(QBHRETL),CL6'BHRETL'                                  
         DC    AL1(6),AL1(QBHTYPE),CL6'BHTYPE'                                  
         DC    AL1(6),AL1(QBPRDINT),CL6'PRDINT'                                 
         DC    AL1(7),AL1(QAORAGYN),CL7'AORAGYN'                                
         DC    AL1(7),AL1(QAORINCA),CL7'AORINCA'                                
         DC    AL1(6),AL1(QAORPCT),CL6'AORPCT'                                  
         DC    AL1(6),AL1(QAORRPA),CL6'AORRPA'                                  
         DC    AL1(7),AL1(QAOREFDT),CL7'AOREFDT'                                
         DC    AL1(5),AL1(QBHIMN),CL5'BHIMN'                                    
         DC    AL1(5),AL1(QBHDMN),CL5'BHDMN'                                    
         DC    AL1(5),AL1(QBHPMN),CL5'BHPMN'                                    
         DC    AL1(5),AL1(QBHRMN),CL5'BHRMN'                                    
         DC    AL1(7),AL1(QBHSDT),CL7'BHSDATE'                                  
         DC    AL1(7),AL1(QBHRDT),CL7'BHRDATE'                                  
         DC    AL1(7),AL1(QBHPDT),CL7'BHPDATE'                                  
         DC    AL1(7),AL1(QBHIDT),CL7'BHIDATE'                                  
         DC    AL1(7),AL1(QBHDDT),CL7'BHDDATE'                                  
         DC    AL1(8),AL1(QBHSING),CL8'BHSINGLE'                                
         DC    AL1(2),AL1(QDOWN),CL2'DL'                                        
         DC    AL1(6),AL1(QRTLSCH),CL6'RTLSCH'                                  
         DC    AL1(2),AL1(QCLRST),CL2'CL'                                       
         DC    AL1(3),AL1(QCOM),CL3'COM'                                        
         DC    AL1(4),AL1(QLINE),CL4'LINE'                                      
         DC    AL1(4),AL1(QUDEF),CL4'UDEF'                                      
         DC    AL1(4),AL1(QUCOM),CL4'UCOM'                                      
         DC    AL1(6),AL1(QIFSTAT),CL6'IFSTAT'                                  
         DC    AL1(6),AL1(QIFTYPE),CL6'IFTYPE'                                  
         DC    AL1(6),AL1(QIFDATE),CL6'IFDATE'                                  
         DC    AL1(6),AL1(QIFTIME),CL6'IFTIME'                                  
         DC    AL1(6),AL1(QEDATES),CL6'EDATES'                                  
         DC    AL1(8),AL1(QIFRSPDW),CL8'IFRSPDTW'                               
         DC    AL1(8),AL1(QIFRSPDM),CL8'IFRSPDTM'                               
         DC    AL1(7),AL1(QIFRSPDT),CL7'IFRSPDT'                                
         DC    AL1(7),AL1(QIFBYCHK),CL7'IFBYCHK'                                
         DC    AL1(7),AL1(QATTCODE),CL7'ATTCODE'                                
         DC    AL1(7),AL1(QGLCDAT),CL7'GLCDATE'                                 
         DC    AL1(6),AL1(QBYDOLR),CL6'BYDOLR'                                  
         DC    AL1(5),AL1(QHDCML),CL5'HDCML'                                    
         DC    AL1(5),AL1(QINVNO),CL5'INVNO'                                    
         DC    AL1(7),AL1(QINVSTAT),CL7'INVSTAT'                                
         DC    AL1(7),AL1(QINVDATE),CL7'INVDATE'                                
         DC    AL1(8),AL1(QINVDATE),CL8'INVDDATE'                               
         DC    AL1(8),AL1(QINVDATE),CL8'INCRDATE'                               
         DC    AL1(8),AL1(QINVDATE),CL8'INVADATE'                               
         DC    AL1(7),AL1(QAGYCODE),CL7'AGYCODE'                                
         DC    AL1(8),AL1(QNWSBUY),CL8'NWSBUYER'                                
         DC    AL1(7),AL1(QNWSCAMP),CL7'NWSCAMP'                                
         DC    AL1(7),AL1(QNWSDATE),CL7'NWSDATE'                                
         DC    AL1(4),AL1(QCOST),CL4'COST'                                      
         DC    AL1(7),AL1(QNETSIZE),CL7'NETSIZE'                                
         DC    AL1(4),AL1(QNAME),CL4'NAME'                                      
         DC    AL1(7),AL1(QBHEDT),CL7'BHEDATE'                                  
         DC    AL1(5),AL1(QPDREP),CL5'PDREP'                                    
         DC    AL1(6),AL1(QSQDDPT),CL6'SQDDPT'                                  
         DC    AL1(8),AL1(QBLIDATE),CL8'BILINVDT'                               
         DC    AL1(8),AL1(QBLEDATE),CL8'BILEDIDT'                               
         DC    AL1(6),AL1(QBYCOPY),CL6'BYCOPY'                                  
         DC    AL1(8),AL1(QCMLBCOD),CL8'CMLBCODE'                               
         DC    AL1(5),AL1(QBONUS),CL5'BONUS'                                    
         DC    AL1(5),AL1(QBLANK),CL5'BLANK'                                    
         DC    AL1(7),AL1(QINVSRC),CL7'INVSRCE'                                 
         DC    AL1(8),AL1(QMSFORMT),CL8'MSFORMAT'                               
         DC    AL1(6),AL1(QMSFREQ),CL6'MSFREQ'                                  
         DC    AL1(7),AL1(QMSOWNER),CL7'MSOWNER'                                
         DC    AL1(8),AL1(QMSPARNT),CL8'MSPARENT'                               
         DC    AL1(7),AL1(QMSSTATE),CL7'MSSTATE'                                
         DC    AL1(6),AL1(QMSCHAN),CL6'MSCHAN'                                  
         DC    AL1(5),AL1(QMSNET),CL5'MSNET'                                    
         DC    AL1(4),AL1(QSHOW),CL4'SHOW'                                      
         DC    AL1(5),AL1(QETYPE),CL5'ETYPE'                                    
         DC    AL1(8),AL1(QOMSTAT),CL8'OMSTATUS'                                
         DC    AL1(6),AL1(QOMSTATD),CL6'OMSTDT'                                 
         DC    AL1(8),AL1(QOMORDN),CL8'OMORDNUM'                                
         DC    AL1(7),AL1(QOMBYRIN),CL7'OMBUYER'                                
         DC    AL1(8),AL1(QOMFLT),CL8'OMFLIGHT'                                 
         DC    AL1(5),AL1(QOMSP),CL5'OMSLP'                                     
         DC    AL1(7),AL1(QOMPP),CL7'OMPOINT'                                   
         DC    AL1(6),AL1(QOMBYRCT),CL6'OMCITY'                                 
         DC    AL1(6),AL1(QOMBYRNM),CL6'OMNAME'                                 
         DC    AL1(7),AL1(QOMROUTE),CL7'OMROUTE'                                
         DC    AL1(6),AL1(QOMDEST),CL6'OMDEST'                                  
         DC    AL1(8),AL1(QOMREPCN),CL8'OMREPCON'                               
         DC    AL1(7),AL1(QOMAGYID),CL7'OMAGYID'                                
         DC    AL1(7),AL1(QOMREPID),CL7'OMREPID'                                
         DC    AL1(5),AL1(QADID),CL5'AD-ID'                                     
         DC    AL1(7),AL1(QINVDAYS),CL7'INVDAYS'                                
         DC    AL1(8),AL1(QINVFDAT),CL8'INVFDATE'                               
         DC    AL1(7),AL1(QINVFPER),CL7'INVFPER'                                
         DC    AL1(8),AL1(QINVLDAT),CL8'INVLDATE'                               
         DC    AL1(7),AL1(QINVLPER),CL7'INVLPER'                                
         DC    AL1(8),AL1(QBYDMSRC),CL8'BYDEMSRC'                               
         DC    AL1(5),AL1(QORBIT),CL5'ORBIT'                                    
         DC    AL1(5),AL1(QEQLEN),CL5'EQLEN'                                    
         DC    AL1(5),AL1(QERATE),CL5'ERATE'                                    
         DC    AL1(7),AL1(QGKBDATE),CL7'ORDDATE'                                
         DC    AL1(7),AL1(QGKGDATE),CL7'GLKDATE'                                
         DC    AL1(8),AL1(QINVFLAG),CL8'INVFLAGS'                               
         DC    AL1(8),AL1(QOMLSTST),CL8'OMLSTSNT'                               
         DC    AL1(8),AL1(QMSMOWN),CL8'MSMINOWN'                                
         DC    AL1(8),AL1(QMSMCTRY),CL8'MSMALCRY'                               
         DC    AL1(8),AL1(QMSMCTY),CL8'MSMALCTY'                                
         DC    AL1(8),AL1(QMSMSTA),CL8'MSMALSTA'                                
         DC    AL1(8),AL1(QMSMST1),CL8'MSMALST1'                                
         DC    AL1(8),AL1(QMSMST2),CL8'MSMALST2'                                
         DC    AL1(8),AL1(QMSMZIP),CL8'MSMALZIP'                                
         DC    AL1(8),AL1(QMSTCTRY),CL8'MSTRACRY'                               
         DC    AL1(8),AL1(QMSTCTY),CL8'MSTRACTY'                                
         DC    AL1(8),AL1(QMSTSTA),CL8'MSTRASTA'                                
         DC    AL1(8),AL1(QMSTST1),CL8'MSTRAST1'                                
         DC    AL1(8),AL1(QMSTST2),CL8'MSTRAST2'                                
         DC    AL1(8),AL1(QMSTZIP),CL8'MSTRAZIP'                                
         DC    AL1(8),AL1(QESTBKTP),CL8'ESTBKTYP'                               
         DC    AL1(6),AL1(QBHINVM),CL6'BHINVM'                                  
         DC    AL1(8),AL1(QBHINVMN),CL8'BHINVMND'                               
         DC    AL1(4),AL1(QHDEF),CL4'HDEF'                                      
         DC    AL1(4),AL1(QCNTR),CL4'CNTR'                                      
         DC    AL1(8),AL1(QFTYPE),CL8'FILMTYPE'                                 
         DC    AL1(8),AL1(QBUYBK),CL8'LNBKTYPE'                                 
         DC    AL1(8),AL1(QOMFSNT),CL8'OMFRTSNT'                                
         DC    AL1(5),AL1(QMFID),CL5'MFID#'                                     
         DC    AL1(5),AL1(QCLINV),CL5'CLINV'                                    
         DC    AL1(5),AL1(QCLPRD),CL5'CLPRD'                                    
         DC    AL1(5),AL1(QCLEST),CL5'CLEST'                                    
         DC    AL1(8),AL1(QRATETYP),CL8'RATETYPE'                               
         DC    AL1(7),AL1(QBUYBOOK),CL7'BUYBOOK'                                
         DC    AL1(7),AL1(QOMEMAIL),CL7'OMEMADD'                                
         DC    AL1(6),AL1(QINVSREP),CL6'INVREP'                                 
         DC    AL1(8),AL1(QINVAID),CL8'INVAGYID'                                
         DC    AL1(5),AL1(QNTYPE),CL5'NTYPE'                                    
         DC    AL1(7),AL1(QINVCMAT),CL7'INVCMAT'                                
         DC    AL1(8),AL1(QBLINVDT),CL8'BILINVMY'                               
         DC    AL1(5),AL1(QBUYMGD),CL5'MGDTL'                                   
         DC    AL1(6),AL1(QMKT),CL6'MKTYPE'                                     
         DC    AL1(8),AL1(QMSRSENT),CL8'OMRESEND'                               
         DC    AL1(8),AL1(QMSMFEM),CL8'MSMINFEM'                                
         DC    AL1(8),AL1(QMSFMIN),CL8'MSFCCMIN'                                
         DC    AL1(8),AL1(QMSFFEM),CL8'MSFCCFEM'                                
         DC    AL1(7),AL1(QTARGET3),CL7'TARGET3'                                
         DC    AL1(7),AL1(QTARGET4),CL7'TARGET4'                                
         DC    AL1(8),AL1(QOMMGCD),CL8'OMMGCODE'                                
         DC    AL1(8),AL1(QOMMGRD),CL8'OMDTRECD'                                
         DC    AL1(7),AL1(QOMMGLS),CL7'OMDTLST'                                 
         DC    AL1(7),AL1(QOMMGAM),CL7'OMTRVSN'                                 
         DC    AL1(7),AL1(QOMMGST),CL7'OMLSTST'                                 
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* GETEXT: GET ADDRESS OF EXTENDED COL FILT FOR THIS COL               *         
***********************************************************************         
         SPACE                                                                  
GETEXTC  LR    R1,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,AXTCOLS        NOT VALID IF PERIOD IN EXT COL FILT          
         JZ    GETEXTNE                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,DRARGSI+6        COLUMN NUMBER                                
         BCTR  RE,0                                                             
         MHI   RE,EXTCOLSL         INDEX TO TABLE                               
         AR    RF,RE                                                            
*                                                                               
GETEXTEQ CR    RB,RB                                                            
         J     GETEXTX                                                          
*                                                                               
GETEXTNE LTR   RB,RB                                                            
GETEXTX  LR    RE,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE SPWRIWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
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
*        INCLUDE FAGETTXTD                                                      
*        INCLUDE FASECRETD                                                      
*        INCLUDE DDUCOMD                                                        
*        INCLUDE GEGENRFPD                                                      
*        INCLUDE SPDDEQUS                                                       
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
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDUCOMD                                                        
       ++INCLUDE GEGENRFPD                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DEDEMOVALD                                                     
       ++INCLUDE GEGENTOK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'167SPWRI0C   02/22/21'                                      
         END                                                                    
*********************************************************************           
*                                                                   *           
*          SPWRIGEN (T00A52) - SPOT WRITER GENERAL ROUTINES         *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 03MAY02 98 EFJ -- FIX VALOPT STUFF                                *           
* 26APR02 96 EFJ -- ALLOW OLYMPIC EXCLUSION BOOK                    *           
* 12FEB02 95 EFJ -- SUPPORT RFP/RLP KEYWORD FOR BHXDATE             *           
*                -- CLEAN UP EXIT CODE                              *           
* 21NOV01 94 EFJ -- SFI! INCREASED SCANBLK LENGTH - CHANGE LA!      *           
* 10NOV01 93 EFJ -- FIX DEMOVAL CALLS FOR CANADA                    *           
*                -- SUPPORT CLIENT LIST AUTHORIZATIONS              *           
* 02NOV01 93 EFJ -- FIX ADJ CODE FILTERS                            *           
* 23OCT01 92 EFJ -- LEVTABLE ENTRY FOR RUNDATE KEYWORD              *           
* 09OCT01 91 EFJ -- CHANGE *+4 TO *+8 FOR IDF                       *           
* 27AUG01 90 EFJ -- SUPPORT MARKET LEVEL USER COMMENTS (UCOM)       *           
* 13JUL01 89 EFJ -- COULDN'T GET ON SF INSTRUCTION RIGHT!           *           
* 13JUL01 88 EFJ -- NEW MCMKTSTA MACRO                              *           
* 31MAY01 87 EFJ -- NEW MCSTANM MACRO                               *           
* 09MAY01 86 EFJ -- LEVTABLE ENTRY FOR PURPOSE CODE KEYWORD         *           
* 24APR01 85 EFJ -- LEVTABLE ENTRY FOR CMLBCODE KEYWORD             *           
* 16APR01 84 EFJ -- PASS MEDIA T TO STAVAL FOR MEDIA * REQ'S        *           
*                   TO SUPPORT ALL- REQ FOR MEDIA * (STARCOM)       *           
* 12APR01 83 EFJ -- LEVTABLE ENTRY FOR NEW BUY COPY DATE/TIME       *           
* 08MAR01 82 EFJ -- IF EST RANGE GIVEN, MAKE SURE DIFFERENT NUMBERS *           
* 26FEB01 81 EFJ -- RENAME RSM KEYWORDS TO MKT                      *           
* 21FEB01 80 EFJ -- LEVTABLE ENTRY FOR NEW RSM KEYWORDS             *           
* 02NOV00 79 EFJ -- FIX OPTBK FOR AVG BK - ALWAYS USES ACT!         *           
* 25OCT00 78 EFJ -- COS2 FLAG FOR SL REPORT                         *           
* 27SEP00 77 EFJ -- SUPPORT UCOM RECORDS                            *           
* 23AUG00 76 EFJ -- ORTEST AND XCTEST OPTION                        *           
*                -- NOW GENERATES SPARE ON PHASE RECORD             *           
* 15AUG00 75 EFJ -- SUPPORT BOOK AVERAGING IN OPTBK                 *           
* 07JUL00 74 EFJ -- NEW BHTYPE FILTERS                              *           
* 04APR00 73 BOB -- MAKE BILEDIDT A ROW                             *           
* 28APR00 72 EFJ -- SUPPORT 'ABS' FOR COMPUTES (ABSOLUTE VALUE)     *           
* 04APR00 71 BOB -- MAKE BILINVDT A ROW                             *           
* 27MAR00 70 EFJ -- SPECIAL WKDY OPTION FOR STEVE/WESTERN           *           
*                -- ADD LOTS MORE ADDRESSABILITY                    *           
* 23MAR00 69 EFJ -- FIX COL VALIDATION (BROKEN L63)                 *           
*                -- FIND SOME ADDRESSABILITY                        *           
* 21MAR00 68 EFJ -- EST 0 IS INVALID                                *           
* 06MAR00 67 EFJ -- DONT MESS WITH R0 IN VALCOL!                    *           
* 01FEB00 66 EFJ -- SUPPORT UPT OPTION                              *           
*                -- SUPPORT BHEDATE OPTION                          *           
*                -- FIND SOME ADDRESSABILITY                        *           
* 04JAN00 65 EFJ -- LEVTABLE ENTRY FOR SQDDPT                       *           
* 03JAN00 64 EFJ -- NOTAX OPTION                                    *           
* 22DEC99 63 BOB -- FINAL MODIFICATIONS FOR SQAD                    *           
* 08DEC99 62 EFJ -- OPTION TO SUPPRESS WARNINGS                     *           
* 17NOV99 61 EFJ -- ADD LEVTABLE ENTRY FOR PDREP                    *           
* 10NOV99 60 EFJ -- ADD LEVTABLE ENTRY FOR BHEDATE                  *           
* 22SEP99 59 EFJ -- RE-DO TRD OPTION TO BE TRD=NO                   *           
* 13AUG99 57 EFJ -- RECAP ONLY OPTION                               *           
* 09JUL99 56 EFJ -- SUPPORT FOR BILLING INVOICE DATE (TEXACO)       *           
* 29JUN99 55 EFJ -- MAKE XEST DDS & WEEKEND ONLY                    *           
* 28JUN99 54 EFJ -- SUPPORT TRD OPTION                              *           
* 07APR99 53 EFJ -- NEW LIMIT ACCESS FEATURES (MKT & CLT)           *           
* 05APR99 52 EFJ -- RE-ENABLE BK=ACT FOR MEDIA R (SEE L16)          *           
* 10MAR99 51 EFJ -- ADD HEADERS INTO PQ INDEX ENTRIES               *           
*                -- MOVE CODE TO PICK UP PQINDEX COLS               *           
* 22FEB99 50 EFJ -- ADD PQIX OPTION TO TEST OFFLINE                 *           
*                -- PQIX REVISIONS                                  *           
* 19FEB99 49 EFJ -- DON'T DIE ON PQINDEX ENTRY NOT FOUND            *           
*                -- ADD ISO DATE OPTION AND SUPPORT                 *           
* 18FEB99 48 EFJ -- ADD LEVTABLE ENTRY                              *           
* 09FEB99 47 EFJ -- BUILD PQINDEX TABLE                             *           
* 02FEB99 46 EFJ -- ALMOST HAD L44 AGAIN...                         *           
* 28JAN99 45 EFJ -- ALMOST HAD L44                                  *           
* 26JAN99 44 EFJ -- FIX $ OFFICE SECURITY                           *           
* 06JAN99 43 EFJ -- SUPPORT 4 BOOKS                                 *           
* 21DEC98 42 EFJ -- SUPPORT WTP OPTION                              *           
* 17DEC98 41 EFJ -- ADD BOOK ENTRY TO LEVTABLE (WHY MISSING???)     *           
* 04DEC98 40 EFJ -- SUPPORT DBOOK KEYWORD                           *           
* 02NOV98 39 EFJ -- SUPPORT RFPDATE OPTION                          *           
* 29OCT98 38 EFJ -- SUPPORT NAME ROW                                *           
* 27OCT98 37 EFJ -- SUPPORT ORIG OPTION                             *           
* 29SEP98 36 EFJ -- SUPPORT GRAND TOTALS                            *           
* 24SEP98 35 EFJ -- SUPPORT PROGRAM FILTER                          *           
* 15SEP98 34 EFJ -- DON'T ALLOW O BOOK OVERRIDE                     *           
* 14AUG98 33 EFJ -- SET WEEK LEVEL                                  *           
* 12AUG98 32 NRK -- REPLACE VALIPER CALLS WITH PERVAL.              *           
* 28JUL98 31 EFJ -- SET OPTION XEST IF SINGLE CLT & COP2XEST SET    *           
* 25JUN98 30 NRK -- ADD LEVTABLE ENTRY FOR ALPHA MKT CODE KEYWORD   *           
* 17JUN98 29 NRK -- Y2K COMPLIANCE                                  *           
* 15JUN98 28 EFJ -- DON'T ALLOW HEADING OVERRIDES FOR UDEFS         *           
* 01JUN98 27 EFJ -- ADD LEVTABLE ENTRY FOR STAZIP KEYWORD           *           
* 27MAY98 26 EFJ -- COULDN'T GET ONE FUCKING LINE OF CODE RIGHT!    *           
* 26MAY98 25 EFJ -- XEST OPTION TO ALLOW PW ACROSS ESTS             *           
* 14MAY98 24 EFJ -- VALIDATE STATION SHOULD BE PACK CALL            *           
* 30APR98 23 EFJ -- SKIP L20 ERROR CHECKS IF RFP                    *           
* 18APR98 22 EFJ -- CHANGES FOR NEW CABLE BITS                      *           
* 15APR98 21 EFJ -- LEVTABLE ENTRY FOR NSIZE KEYWORD                *           
* 07APR98 20 EFJ -- GIVE ERRORS FOR DAY/WEEK KEYWORDS WHEN TOO LONG *           
* 17MAR98 19 EFJ -- SET DBSELMED PROPERLY FOR CANADIAN MEDIA N      *           
* 26FEB98 18 EFJ -- COST KEYWORD                                    *           
* 04FEB98 17 EFJ -- MCDPT MACRO                                     *           
* 12DEC97 16 EFJ -- DISALLOW BK=ACT FOR MEDIA R (PER LCON)          *           
* 11DEC97 15 EFJ -- ADD BATCH DATE FILTER FOR IAS                   *           
* 04DEC97 14 EFJ -- USE TODAYS DATE FROM SYSTEM (DATE CARD)         *           
* 02DEC97 13 EFJ -- SET TO READ SLK IF CLT LOCK CPS                 *           
* 12NOV97 12 EFJ -- SUPPORT CLT$BILT (BILLED TODAY)                 *           
* 05NOV97 11 EFJ -- UPDATE SRC FILTER TO SET GLPALDET               *           
* 27OCT97 10 EFJ -- SET TO READ SLK IF WIM/CLT LOCK CPP             *           
* 16OCT97 09 EFJ -- SUPPORT BILLTDY KEYWORD                         *           
* 09OCT97 08 EFJ -- BACK OUT LEVEL 7 (NEVER LIVE)                   *           
* 08SEP97 07 EFJ -- SUPPORT ALL CLT ONE PRD FOR COKE                *           
* 15AUG97 06 EFJ -- SUPPORT CLIENT EXCLUDE FILTER                   *           
* 13MAY97 05 EFJ -- ADD COLPD= OPTION TO SUPPORT PD COL FILT        *           
* 08MAY97 04 EFJ -- ADD EST COLUMN FILTER                           *           
* 02MAY97 03 EFJ -- SKIP STABUCK CUR=X'01' RECS                     *           
* 28MAR97 02 EFJ -- ADD PD (PERIOD) COLUMN FILTER                   *           
*                -- RESERVE 26K INSTEAD OF 24K                      *           
* 27DEC96 01 EFJ -- LEVEL RESET                                     *           
*-------------------------------------------------------------------*           
* 20DEC96 71 EFJ -- NEW OFFICE CLT GROUP LIMIT                      *           
* 05DEC96 70 EFJ -- SRC COMMENT FILTER                              *           
* 19NOV96 69 EFJ -- LEVTABLE ENTRY FOR XPRDNAME                     *           
*                -- SUPPORT FOR AVGMON (SPECIAL PERIOD) KEYWORD     *           
* 12NOV96 68 EFJ -- SET ROWIRANK IF RANK IS A ROW                   *           
* 01NOV96 67 EFJ -- NEW LEVTABLE ENTRY                              *           
* 30OCT96 66 EFJ -- PRDBUY OPTION                                   *           
* 29OCT96 65 EFJ -- NEW LEVTABLE ENTRIES                            *           
* 24OCT96 64 EFJ -- CREATION DATE FILTER                            *           
* 22AUG96 63 EFJ -- SKIP AUTH CHECK OFFLINE FOR XFILE               *           
* 16AUG96 61 EFJ -- XFILE OPTION FOR CROSS FILE REPORTING           *           
*                -- LEVTABLE ENTRY FOR XCLTOFF & XMKTNAME           *           
* 07AUG96 60 EFJ -- FIX GETFACT CALL                                *           
*                -- LEVTABLE ENTRY FOR AGY ROW KEYWORD              *           
*                -- DROP USINGS (ASSEMBLER WARNINGS)                *           
* 16JUL96 59 EFJ -- SET SBAUTL                                      *           
* 27JUN96 58 EFJ -- MCBUYS MACRO                                    *           
*                -- TIME RANGE FILTER                               *           
* 25JUN96 57 EFJ -- FILTER INVOICES BY PAID/UNPD                    *           
*                -- MOVE OTHER INVOICE FILTERS TO SBEINV            *           
* 17JUN96 56 EFJ -- TEMP FILTER FOR BUYS WITH NO IDR                *           
* 22MAY96 55 EFJ -- SUPPORT PWPCT IN STACK                          *           
* 26APR96 54 EFJ -- SUPPORT NO DEM/CCP WEEK LIMIT PROFILE           *           
* 09APR96 53 EFJ -- MIDNIGHT CLEARANCE FILTERS                      *           
* 15MAR96 52 EFJ -- ALLOW ANY LENGTH REQ FOR DDS TERMS              *           
* 14MAR96 51 EFJ -- FIX BUG PF3/PF4 SCREEN BUG                      *           
* 26JAN96 50 EFJ -- SET READ SWITCH FOR INVOICE DATA ROWS           *           
* 25JAN96 49 EFJ -- INVOICE AUDIT FILTER                            *           
* 11JAN96 48 EFJ -- SUPPORT GOAL NET $ IN STACK                     *           
*                -- SUPPORT ATIMES FILTER (SAME AS ATIME, BUT       *           
*                   MANUAL IS WRONG)                                *           
* 28DEC95 47 EFJ -- SUPPORT NEW WL PROFILE                          *           
* 07DEC95 46 EFJ -- OPTION TO USE POL EST                           *           
* 28NOV95 45 EFJ -- NEW ROWIND2 FOR CABLE AGG                       *           
* 27NOV95 44 EFJ -- NEW LEVTABLE ENTRY                              *           
* 30OCT95 43 EFJ -- ALLOW 21 WEEK SOON REQS                         *           
* 23OCT95 42 EFJ -- SET SWITCH TO READ INVOICES                     *           
* 20OCT95 41 EFJ -- IGNORE '+' LIMIT ACCESS FOR ALL CLT PROCESSING  *           
*                -- IF LIMIT ACCES = CLT, CAN ONLY REQ FOR CLT      *           
* 18OCT95 40 EFJ -- FIX BUG IN EXT COL CODE                         *           
* 13OCT95 39 EFJ -- DON'T ALLOW ALL CLT REQ IF ANY LIMIT ACCESS     *           
* 09OCT95 38 EFJ -- SUPPORT BYCHDATE                                *           
* 05OCT95 37 EFJ -- CHANGE INVPRD TO INVPROD                        *           
* 20SEP95 36 EFJ -- NEW LEVTABLE ENTRY                              *           
*                -- SUPPORT FOR CABLE AGG                           *           
* 19SEP95 35 EFJ -- ALLOW 1 YEAR SOON REQ'S                         *           
* 14SEP95 34 EFJ -- COL FILTER ON SREP                              *           
* 24AUG95 33 EFJ -- EXPAND WEEKLY FLOWCHART TO 14 WEEKS (FM 13)     *           
* 14AUG95 32 EFJ -- NEW LEVTABLE ENTRY                              *           
* 12JUL95 31 EFJ -- SET PRE-VALID BIT ON IN GENMAC FOR SECURITY     *           
* 12JUL95 30 EFJ -- ADD WI & WT TO SECURITY                         *           
* 07JUL95 29 EFJ -- ORIG/SPILL COLUMN FILTERS                       *           
* 05JUL95 28 EFJ -- SUPPORT EXTENDED COLUMN FILTERS                 *           
* 28JUN95 27 EFJ -- FIX BUG IN ALLVAL (TRIES TO USE INPUT LEN OF 0) *           
* 27JUN95 26 EFJ -- NEW LEVTAB ENTRY FOR CLTPCT                     *           
* 15JUN95 25 EFJ -- HDCML KEYWORD - CMML NAME FOR CLASS SPLIT       *           
* 13JUN95 24 EFJ -- UPDATE AFFID TIME FILTER                        *           
* 13JUN95 23 EFJ -- FORCE OPTION FLOAT=- IF DOWNLOAD                *           
* 18MAY95 22 EFJ -- SET OPTIND3 IF BILLST KEYWORD USED              *           
* 16MAY95 21 EFJ -- SUPPORT COLUMN TOTAL ONLY OPTION                *           
* 03MAY95 20 EFJ -- NEW DISCLAIMER OPTION                           *           
* 28APR95 19 EFJ -- NEW LEVTAB ENTRY FOR ESTPCT                     *           
* 20APR95 18 EFJ -- NEW LEVTAB ENTRY FOR STAADDR                    *           
* 07APR95 17 EFJ -- AFFID TIME RANGE FILTER (ATIME)                 *           
* 06APR95 16 EFJ -- MCBUYC MACRO                                    *           
* 06APR95 15 EFJ -- STAPACK NEEDS MEDIA FOR CANADA FOR COL FILT     *           
* 31MAR95 14 EFJ -- USE PRE-VALID BITS FOR FIELD AUTH TEST, NOT     *           
*                   INPUT THIS TIME                                 *           
* 27MAR95 13 EFJ -- X'8000' ALREADY IN USE - TRY X'4000'            *           
* 09MAR95 12 EFJ -- DON'T ALLOW ROW OR COLUMN CHANGES FOR WI UNLESS *           
*                   AUTH X'8000' (INCL HEADS & MIDS)                *           
* 06FEB95 11 EFJ -- NEW OPTION FOR PST (TEMPORARY)                  *           
* 05JAN95 10 EFJ -- PUT 'E' BACK IN NOREQDET                        *           
* 22DEC94 09 EFJ -- SUPPORT RETAIL CONTROL BILLING OPTIONS          *           
* 11OCT94 08 EFJ -- MAKE VALOPT & VVALFILT TABEL DRIVEN             *           
* 11OCT94 07 EFJ -- SUPPPORT FOR DMA OPTION                         *           
* 10OCT94 06 EFJ -- ALLOW ,TITLE TO BE USED FOR EDATES              *           
* 27SEP94 05 EFJ -- FILTER FOR 'E' (CONTROL) TYPE ESTIMATES         *           
* 13SEP94 04 EFJ -- INTERCEPT MSPACK/UNPK CALLS - ROUTE TO STAPACK  *           
* 22AUG94 03 EFJ -- MCAFFID MACRO (ADATE/ADAY/ATIME)                *           
* 25JUL94 02 EFJ -- FUCKING UNDO BUG KEEPS SCREWING LEVEL UP        *           
*                -- SUPPORT FOR MEDIA X WHEN REPORTING MEDIA '*'    *           
*                -- STABINV CHANGED TO INVNUM                       *           
*-------------------------------------------------------------------*           
* 07JUL94 06 EFJ -- SUPPORT FOR STABINV KEYWORD                     *           
* 27JUN94 05 EFJ -- OPTION TO BLANK CML FLDS WHEN NONE              *           
* 22JUN94 04 EFJ -- MERGE W/SPWRIGEN - STACKING KEYWORDS            *           
* 06JUN94 03 TCS -- REWORK SYSD                                     *           
* 03JUN94 02 EFJ -- LEVEL RESET                                     *           
*-------------------------------------------------------------------*           
* 03JUN94 30 EFJ -- REMOVE HARD CODED ERROR/INFO MESSAGES           *           
* 31MAY94 29 EFJ -- DON'T ALLOW OPTIONS NOREQDET & DOWN TOGETHER    *           
* 23MAY94 28 EFJ -- SUPPORT FOR STAFFCH                             *           
* 23MAY94 27 EFJ -- SUPPORT FOR ADATED                              *           
* 16MAY94 26 EFJ -- CODE TO USE GETTXT FOR ERROR MESSAGES           *           
*                -- CLEAN UP ERREXIT CODE TO SQUEEZE A FEW BYTES    *           
* 11MAY94 25 EFJ -- SET DISLN IN DATAIND4 FOR SPOT LEN COL FILTER   *           
* 06MAY94 24 TCS -- SUPPORT DOWNLOADING OF TOTALS                   *           
* 05MAY94 23 EFJ -- FIX MACRO/KEYWORD CONFLICT                      *           
* 05MAY94 22 EFJ -- NEW ROW KEYWORDS - BYCDATE & GLCDATE            *           
* 04MAY94 21 EFJ -- JWT/BELL ATLANTIC FEATURE - 1 EST, MUST HAVE    *           
*                   REQ=Y IF TWAAUTH=X'800F'                        *           
* 04MAY94 20 LW  -- DONT SET CFMON TOTAL RTN IN OPTCFNT SET         *           
* 26APR94 19 EFJ -- NEW ROW KEYWORD - MKTWT                         *           
*                -- FIX LEVTABLE - ENTRIES MUST BE BEFORE SHORTER   *           
*                   ENTRIES THAT START SAME                         *           
* 19APR94 18 EFJ -- COL FILTER FOR SPOTLEN                          *           
*                -- CLEAN UP ADJ CODE FILTER                        *           
*                -- FIX PROD COL FILTER BUG                         *           
* 13APR94 17 EFJ -- FILTER ON INFO BUYER CHECKING                   *           
* 12APR94 16 EFJ -- ADD COLUMN FILTER FOR ADJACENCY CODE            *           
* 11APR94 15 EFJ -- SUPPORT ADATER & ATIMEM KEYWORDS                *           
* 30MAR94 14 EFJ -- SUPPORT ATTCODE KEYWORD                         *           
* 17MAR94 13 EFJ -- SET DBSELMED TO 'C' FOR CANADIAN AGYS           *           
*                -- FIX BUG IN CMLNUM                               *           
*                -- CHANGE LABEL FROM SBRSPDTS TO SBERSPDT          *           
* 10MAR94 12 TCS -- SUPPORT HARD CODED STATION GROUP FOR EIX        *           
* 10MAR94 11 EFJ -- ADD SUPPORT FOR NEW ROW KEYWORD - IFBYCHK       *           
* 25FEB94 10 EFJ -- ADD OPTION TO SKIP INFO DATA W/OUT BUYS         *           
* 14FEB94 09 EFJ -- ADD SUPPORT FOR CMLNUM FROM TRAFFIC CML         *           
*                -- FIX BUG - XIT1 W/OUT NTR1 -- THANKS LISA        *           
* 10FEB94 08 EFJ -- FILTER FOR INFOMERCIAL RESPONSE DATES           *           
* 09FEB94 07 TCS -- CLIENT OFFICE SECURITY LIMITS                   *           
* 03FEB94 06 EFJ -- ADD SUPPORT FOR NEW ROWS - IFRSPDT/DTW/DTM      *           
* 26JAN94 05 TCS -- INCREASE COMMERCIAL TABLE SIZE TO 2000 ENTRIES  *           
* 25JAN94 04 ??? -- FIXED GRP= BUG                                  *           
* 20JAN94 03 TCS -- SET A(DAYPART TABLES BUFFER) FOR MEDIA=*        *           
* 06JAN94 02 TCS -- ADD OPTION TO SHUFFLE EXTRA HEADLINE            *           
* 05JAN94 00 EFJ -- HISTORY LOST.  LEVEL RESET.  ADD 'A' TO PHASE   *           
*                                                                   *           
*********************************************************************           
