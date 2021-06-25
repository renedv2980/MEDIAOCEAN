*          DATA SET SPWRI02    AT LEVEL 095 AS OF 06/17/19                      
*PHASE T20402A,*                                                                
         TITLE 'T20402 - SPOTPAK WRITER EXTENSION ROUTINES TO T20401'           
*                                                                               
***********************************************************************         
*                                                                     *         
*          SPWRI02 (T20402)                                           *         
*                                                                     *         
***********************************************************************         
* USER     JIRA       DATE                 CHANGE LOG                 *         
* ---- ------------ -------- ---------------------------------------- *         
* AKAT SPEC-24962   06/17/19 WEEKLY DATA FOR POSTS OVERRIDE OPTION    *         
* AKAT SPEC-23941   02/26/19 REPLACE REFERENCE TO ARBITRON WITH NAM   *         
* AKAT SPEC-20454   02/28/18 INIT COMSCORE SURVEY DATES IF NOT SET    *         
* AKAT SPEC-11692   02/16/18 NEW TARGET3 & TARGET4 KEYWORD SUPPORT    *         
* AKAT SPEC-12852   02/16/18 REPORT NEW MEDIA FRAMEWORKS DATA         *         
* AKAT SPEC-14215   06/12/17 FIX STATION NAME BUG FOR CABLE           *         
* AKAT SPEC-13699   06/12/17 STA BUFF BUG THAT CAUSED TSAR OVERFLOW   *         
* AKAT SPEC-13149   05/24/17 SET PARENT+ STATION FOR COMSCORE         *         
* AKAT SPEC-6939    02/24/17 SUPPORT COMSCORE DEMO LOOKUPS FOR 17.1.5 *         
* AKAT SPEC-9011    02/10/17 UPDATE TO CANADIAN NATIONAL DEMOS        *         
* AKAT MOXSYS-150   10/06/16 SUPPORT COMSCORE DEMOS                   *         
* AKAT CUSTENH-3347 05/19/16 HST INCREASING TO 15% FOR NB 07/01/16    *         
* AKAT SPSUG-20     05/19/16 HST INCREASING TO 15% FOR NF 07/01/16    *         
* AKAT SPSUG-42     05/19/16 HST INCREASING TO 15% FOR PE 10/01/16    *         
* AKAT DSSTK-270    04/18/16 SUPPORT EST UCOMM 5-8                    *         
* 26AUG13 83 AKT -- SUPPORT FOR NEW NTYPE KEYWORD                   *           
* 13MAR13 82 AKT -- ELIMINATE HST FOR BC AND NEW 14% HST FOR PE     *           
*                -- STARTING APR01/2013                             *           
* 14NOV12 81 AKT -- QST RATE INCREASE FOR QUEBEC STARTING JAN01/2013*           
* 22MAY12 79 AKT -- SRDS SUPPORT                                    *           
* 15DEC11 78 AKT -- QST RATE INCREASE FOR QUEBEC STARTING JAN01/2012*           
* 05DEC11 77 AKT -- BILL COST2 SUPPORT                              *           
* 12AUG11 76 AKT -- FIX ADDRESSABILITY BUG                          *           
* 28APR11 75 AKT -- FIX PUTELEM BUG                                 *           
* 04APR11 74 AKT -- SUPPORT NEW MFID# KEYWORD                       *           
* 01DEC10 73 AKT -- QST RATE INCREASE FOR QUEBEC STARTING JAN01/2011*           
* 19OCT10 72 AKT -- USE CLT/PRD MAIN PST AFTER JUL01/10             *           
* 17AUG10 71 AKT -- SUPPORT NEW OR & NON-POSITIONAL EST FILT OPTION *           
* 25MAR10 70 AKT -- MEDIA FRAMEWORKS SUPPORT                        *           
* 01MAY09 69 AKT -- LOOK UP TV MSTREET REC W/BAND L IF T NOT FOUND  *           
* 05MAR09 68 AKT -- NEW FLAG FOR READING ESTIMATE RECORD            *           
* 14JAN09 67 AKT -- SUPPORT NEW BFORM OPTION                        *           
* 08SEP08 66 AKT -- NEW BK5 AND BK6 OPTIONS                         *           
* 21MAY08 65 EFJ -- FIX WRONG REGISTER IN GENEDICT (AT 2:37 AM)     *           
* 11DEC07 64 EFJ -- SET NEW GST RATE OF 5 PCT FOR CANADA            *           
* 13JUL07 62 AKT -- ADD NEW FIELDS TO THE STATION BUFFER AND ONLY   *           
*                -- BUFFER THINGS WE ACTUALLY FIND FOR THEIR LENGTH *           
* 27FEB07 61 AKT -- BILLGST NOW FULLWORD, NOT XL3                   *           
* 11DEC06 60 AKT -- DON'T SAVE NETWORK BITS WHEN ADDING TO STA BUFF *           
*                -- AND MOVE STA BUFF TO TSAR BUFFER                *           
* 14NOV06 59 EFJ -- SUPPORT 2 CHARACTER BOOK TYPES                  *           
* 27JUN06 58 EFJ -- CHANGE CANADIAN TAX RATES                       *           
* 27JAN06 55 AKT -- SET SBCLT TO AAN AND DELETE COKE CODE           *           
* 31OCT05 54 AKT -- ALWAYS SET DEMNAMES FOR 2 DECIMAL DEMOS (AGAIN) *           
* 31OCT05 54 AKT -- ALWAYS SET DEMNAMES FOR 2 DECIMAL DEMOS (AGAIN) *           
* 21OCT05 53 AKT -- ALWAYS SET DEMNAMES FOR 2 DECIMAL DEMOS         *           
* 06DEC04 52 AKT -- EXTRACT MSO1 NAME IF MSO NOT PRESENT            *           
* 28JUL04 51 AKT -- GET MSTREET PARENT NAME FROM PARENT CODE        *           
*                -- AND READ MSTREET RECORDS BY STATION             *           
* 16JUN04 50 AKT -- DO NOT SET NON-CLT SPECIFIC PREP FOR CABLE      *           
*                -- WITH STATION KEYWORD                            *           
* 09JUN04 49 AKT -- NEW OWNER AND FORMAT KEY FORMAT FOR MSTREET     *           
* 31MAR04 48 EFJ -- CHANGE SPGENESTN TO SPGENEST                    *           
* 17DEC03 47 AKT -- EXTRACT RS1/2CALL FOR CANADIAN SOFT DEMOS       *           
* 23JAN04 46 AKT -- GETNSIZE - SBQMED VS. SBMED...CAUGHT AGAIN!     *           
* 16DEC03 45 PWE -- MORE BILLED PST - CHECK CLT/PRD EXEMPT PST      *           
* 18NOV03 44 ??? -- ?                                               *           
* 11NOV03 43 EFJ -- NIELSEN COPYRIGHT CODE                          *           
* 23OCT03 42 EFJ -- REMOVE REFERENCES TO SPDEMLK                    *           
* 05NOV03 41 PWE -- MORE BILLED HST REFINEMENTS (FIX GSTB)          *           
* 03NOV03 40 PWE -- FIX NETSIZE K/W (GETNSIZE RTN) FOR MEDIA 'C'    *           
* 30OCT03 39 PWE -- CORRECT EXTRACT OF BILLED QST/HST (FIX PSTB)    *           
* 13OCT03 38 AKT -- EXTRACT MSTREET DATA TO STA BUFFER IN FSTA      *           
* 09OCT03 37 EFJ -- FIX EDICT CODE                                  *           
* 24SEP03 36 PWE -- PST                                             *           
* 22MAY03 35 AKT -- MOVE BACK SBCHAN THAT LVL34 LOST TO FIX CHANNEL *           
* 12MAY03 34 PWE -- XTRA LVL33 IF NETWORK=N + AFFIL K/W FOR MEDIA N *           
* 08MAY03 33 PWE -- FIX STANAME KEYWORD FOR MEDIA 'N'               *           
* 24APR03 31 EFJ -- SET DBSELMED CORRECTLY                          *           
* 05MAR03 30 EFJ -- USE SPSLNTAB                                    *           
* 27JAN03 29 EFJ -- NEW EDICT CARDS                                 *           
*                -- YET MORE GETNSIZE!                              *           
* 23JUL02 28 EFJ -- SKIP EST 0 CODE IN PGEST                        *           
* 02JUL02 27 EFJ -- FIX STATION NAME FOR CABLE                      *           
* 30MAY02 26 EFJ -- UPDATE PGDATA KYWD                              *           
* 19APR02 24 EFJ -- YET MORE GETNSIZE!                              *           
* 31JAN02 22 EFJ -- ESTIMATE HEADER CONVERSION                      *           
* 14NOV01 21 EFJ -- GETNSIZE - 3RD TIME'S A CHARM?                  *           
* 10NOV01 20 EFJ -- MORE GETNSIZE FIXES                             *           
* 25OCT01 19 EFJ -- FIX GETNSIZE FOR MKT 0 RECS                     *           
* 09OCT01 18 EFJ -- CHANGE *+4 TO *+8 FOR IDF                       *           
* 11SEP01 17 EFJ -- PASS SPOTPROF+9 TO DEMOCON                      *           
* 27AUG01 16 EFJ -- SUPORT MARKET LEVEL USER COMMENTS (UCOM)        *           
* 07NOV00 15 EFJ -- TREAT MEDIA *, SINGLE CLT REQ AS MULTI CLT      *           
* 03OCT00 14 EFJ -- SUPPORT USER COMMENTS                           *           
*                -- MOVE CLTFST FOR ADDRESSABILITY                  *           
* 11APR00 13 BOB -- FTP CARD FOR EDICT                              *           
* 10AUG99 12 BOB -- SQAD TABLE INIT                                 *           
* 18FEB99 10 EFJ -- SHOW WTP IN HEADLINES                           *           
* 06JAN99 09 EFJ -- SUPPORT 4 BOOKS                                 *           
* 18AUG98 08 EFJ -- SET BOOK TYPE IN HEADLINES                      *           
* 13AUG98 07 EFJ -- SKIP CLT FIRST FOR XFILE!                       *           
* 17JUN98 06 NRK -- Y2K COMPLIANCE                                  *           
* 01JUN98 05 NRK -- GET ZIP AND ZIP+4 FROM STATION ADDRESS RECORD   *           
* 14APR98 04 EFJ -- GET STATION SIZE FROM NETWORK RECORD            *           
* 28AUG97 03 EFJ -- READ SPECIAL MED Z EQV REC FOR XFILE            *           
* 03JUN97 02 EFJ -- DEL EDICT OPT TO STRIP QUOTES (NEVER WORKED)    *           
* 03JUN97 01 EFJ -- LEVEL RESET                                     *           
*-------------------------------------------------------------------*           
* 11DEC96 19 EFJ -- GET NEW MARKET FROM RTG SRV MKT # ON XFILE      *           
* 28AUG96 18 EFJ -- FIX XFILE BUG WITH MULTIPLE REQ'S               *           
* 22JUL96 17 EFJ -- SUPPORT REPORTING ACROSS FILES                  *           
* 22MAY96 16 EFJ -- SUPPORT PWPCT IN STACK                          *           
* 29DEC95 15 EFJ -- READ STATION RECORD INTO LOCAL IO AREA          *           
* 20APR95 14 EFJ -- NEW ENTRY IN STABUFF - STBADDR                  *           
* 22MAR95 13 EFJ -- REMOVE PWLOCK CODE                              *           
* 06MAR95 12 EFJ -- CHANGES FOR PW REPORTING                        *           
* 27FEB95 11 EFJ -- INCREASE EFFS                                   *           
* 02DEC94 10 EFJ -- USE SBQSTART/END, NOT QSTART/END FOR PWLOCK     *           
* 07NOV94 09 EFJ -- NEW ENTRY IN STABUFF TO SUPPORT RADIO DEMOS     *           
* 27OCT94 08 EFJ -- FIX SF 'B  *+' BUG                              *           
* 20OCT94 07 EFJ -- SAVE A(STABUF ENTRY) IN FSTA                    *           
*                -- NEW ENTRIES IN STABUFF                          *           
*                -- BUILD STABUF ENTRY IN ELEM - WORK TOO SMALL     *           
* 19OCT94 06 EFJ -- FIX MEDIA '*' BUG W/STATION ADDRS               *           
* 10OCT94 05 EFJ -- SUPPORT DMA OVERRIDE OPTION                     *           
* 17AUG94 04 EFJ -- FIX BUG WITH MEDIA N IN FSTA                    *           
* 21JUL94 03 EFJ -- SUPPORT WESTERN PW LOCK $                       *           
* 28JUN94 02 EFJ -- ADD NEW STACK ENTRIES                           *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
T20402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20402,RA,R8                                                   
         USING WORKD,R6                                                         
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         ST    RB,EXTBASE1                                                      
         ST    RA,EXTBASE2                                                      
         ST    R8,EXTBASE3                                                      
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE ROUTINES ONLY!                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         LA    RF,BRANCH(RF)       POINT TO ENTRY IN BRANCH TABLE               
*                                                                               
         CLI   0(RF),0             CHECK FOR ADDRESS OR BRANCH INSTR            
         BNE   0(RF)                 BRANCH TO BRANCH INSTRUCTION               
*                                                                               
         L     RF,0(RF)               ELSE LOAD ADDRESS OF ROUTINE              
         BASR  RE,RF                  GO TO ROUTINE                             
*                                                                               
         B     XIT                    EXIT ON RETURN                            
*                                                                               
BRANCH   B     GENSUM                                                           
         B     SETSTACK                                                         
         B     AUTHOR                                                           
         B     PGEST                                                            
         B     CLRST                                                            
         B     DEMOS                                                            
         B     GETDEMS                                                          
         B     DRIVIN                                                           
         B     SETDEMO                                                          
         B     GETNAME                                                          
         B     GETMGR                                                           
         B     GETEST                                                           
         B     FSTA                                                             
         B     CBLNM                                                            
         B     GETTOTWT                                                         
         B     BYBONUS                                                          
         DC    A(BILLVAL)                                                       
         B     GENDPTOT                                                         
         B     HEADHK                                                           
         B     DATES                                                            
         B     GETFLTS                                                          
         DC    A(CLTFST)                                                        
         B     ESTFST                                                           
         B     GETDF                                                            
         B     SPILELEM                                                         
         B     GENEDICT                                                         
         B     SWAPFLES                                                         
         B     GETNSIZE                                                         
         DC    A(SQADINI)          INIT SQAD DAYPART TABLE                      
*                                                                               
XITEQ    CR    RB,RB                                                            
         J     XIT                                                              
XITNEQ   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
         EJECT                                                                  
* ROUTINE TO GENERATE SUMMARY REPORT                                            
* INPUT  : R1=A(SUMMARY TITLE)                                                  
*                                                                               
*        ******** NEVER USED ***********                                        
*                                                                               
GENSUM   DS    0H                                                               
*GENSUM   SR    RF,RF                                                           
*         ICM   RF,7,ASUMMARY+1                                                 
*         BZ    GENSUMX                                                         
*         LR    R5,R1               R5=A(SUMMARY TITLE)                         
*         L     RE,DRSTBUF          COPY ROWS ABOVE SUMMARY LEVEL               
*         LA    RE,0(RE)            FROM FIRST REPORT                           
*         SR    RF,RE                                                           
*         LR    R1,RF                                                           
*         L     R0,DRCURBUF                                                     
*         MVCL  R0,RE                                                           
*         ST    R0,DRCURBUF                                                     
*         C     R0,DRENDBUF                                                     
*         BL    *+6                                                             
*         DC    H'0'                BUFFER OVERFLOW                             
*         MVI   DRCNTRL,1           RE-SET CONTROL BYTE                         
*         GOTO1 GENRPTSQ,2          GENERATE REPORT SEQUENCE ROW                
*                                                                               
*         LA    RF,CONTAGH          BUILD SUMMARY SCREEN IN TWA                 
*         L     RE,ATWA                                                         
*         LA    RE,0(RE)                                                        
*         SR    RF,RE                                                           
*         LR    R1,RF                                                           
*          L     R0,ATIA                                                        
*         MVCL  R0,RE                                                           
*         ST    R0,DMCB                                                         
*         IC    R0,OVERLAY                                                      
*         MVI   OVERLAY,X'F3'       LOAD SUMMARY SCREEN                         
*         MVC   DMCB+4(4),SYSPHASE                                              
*         GOTO1 CALLOV,DMCB,,,0                                                 
*         OC    9(3,R1),9(R1)                                                   
*         BNZ   *+6                                                             
*         DC    H'0'                                                            
*         STC   R0,OVERLAY                                                      
*                                                                               
*         XC    KEY,KEY             GET PROGRAM RECORD FOR SUMMARY NAME         
*         LA    R4,KEY                                                          
*         USING CT01RECD,R4                                                     
*         MVI   CT01TYPE,CT01TYPQ                                               
*         MVC   CT01AGID,AGENCY                                                 
*         MVC   CT01SYS(3),=X'020422'  SPWRI22=SUMMARY RECORD MAINT             
*         MVC   CT01NAME,SUMNAME                                                
*         MVC   FILENAME,=CL8'CTFILE'                                           
*         IC    R0,USEIO                                                        
*         MVI   USEIO,C'Y'                                                      
*         MVC   AIO,AIO3                                                        
*         GOTO1 HIGH                                                            
*         STC   R0,USEIO                                                        
*         XC    FILENAME,FILENAME                                               
*         CLC   KEY(L'CT01KEY),KEYSAVE                                          
*         BE    *+6                                                             
*         DC    H'0'                SHOULD ALREADY HAVE BEEN VALIDATED          
*                                                                               
*         L     R4,ATWA             CALL GEGENPRG TO DISPLAY SUMMARY            
*         L     R7,ATIA                                                         
*         ST    R7,ATWA                                                         
*         MVC   SVDISP,DATADISP                                                 
*         MVC   SVEFHTAG,EFHTAG                                                 
*         IC    R0,MODE                                                         
*         MVI   MODE,DISPREC                                                    
*         MVC   DATADISP,=H'28'                                                 
*         LA    R1,CONTAGH                                                      
*         ST    R1,EFHTAG                                                       
*         GOTO1 GENPROG,DMCB,(RC)                                               
*         STC   R0,MODE                                                         
*         MVC   DATADISP,SVDISP                                                 
*         MVC   EFHTAG,SVEFHTAG                                                 
*                                                                               
*         L     R3,ERREX2           FORCE ERROR EXIT TO THIS ROUTINE            
*         LA    R1,GENSUMX2                                                     
*         ST    R1,ERREX2                                                       
*         LA    R2,SUMHEADH         VALIDATE ROWS AND COLUMNS                   
*         MVI   MAX,5                                                           
*         GOTO1 VALHEAD                                                         
*         LA    R2,SUMMIDH                                                      
*         MVI   MAX,1                                                           
*         GOTO1 VALMID                                                          
*         LA    R2,SUMROWSH                                                     
*         MVI   MAX,6                                                           
*         GOTO1 VALROWS                                                         
*         LA    R2,SUMCOLSH                                                     
*         MVI   MAX,14                                                          
*         GOTO1 VALCOLS                                                         
*         LA    R2,SUMTITH          VALIDATE TITLE                              
*         MVC   0(L'TITLE,R5),TITLE                                             
*         CLI   5(R2),0                                                         
*         BE    GENSUM2                                                         
*         GOTO1 VALTITS                                                         
*         XC    TITLE,0(R5)                                                     
*         XC    0(L'TITLE,R5),TITLE                                             
*         XC    TITLE,0(R5)                                                     
*                                                                               
*GENSUM2  ST    R3,ERREX2           RESTORE VALUABLE ADDRESSES                  
*         ST    R4,ATWA                                                         
*                                                                               
GENSUMX  B     XIT                                                              
*                                                                               
*GENSUMX2 DC    H'0'                JUST DIE IF VALIDATION ERROR                
         EJECT                                                                  
* SET DATA INDICATORS FOR STACKS                                                
*                                                                               
SETSTACK LA    R4,STACKDEF                                                      
         LA    R0,L'STACKDEF                                                    
*                                                                               
SETS1    CLI   0(R4),0                                                          
         BE    SETSX                                                            
         TM    COLIND2,COLISDOL    MEDIA DOLLAR STACK                           
         BZ    *+16                                                             
         LA    R1,STACKDOL                                                      
         LA    RF,L'STACKDOL                                                    
         BAS   RE,SETS2                                                         
         TM    COLIND2,COLISDEM    MEDIA DEMO STACK                             
         BZ    *+16                                                             
         LA    R1,STACKDEM                                                      
         LA    RF,L'STACKDEM                                                    
         BAS   RE,SETS2                                                         
         TM    COLIND2,COLISACC    ACCOUNTING STACK                             
         BZ    *+16                                                             
         LA    R1,STACKACC                                                      
         LA    RF,L'STACKACC                                                    
         BAS   RE,SETS2                                                         
         LA    R4,2(R4)                                                         
         BCT   R0,SETS1                                                         
         B     SETSX                                                            
*                                                                               
SETS2    CLI   0(R1),0                                                          
         BER   RE                                                               
SETS4    CLC   0(1,R4),0(R1)                                                    
         BE    *+10                                                             
         AR    R1,RF                                                            
         B     SETS2                                                            
         OC    DATAIND,1(R1)                                                    
         OC    DATAIND2,2(R1)                                                   
         OC    DATAIND3,3(R1)                                                   
         OC    DATAIND4,4(R1)                                                   
         OC    DATAIND5,5(R1)                                                   
         OC    DATAIND6,6(R1)                                                   
         OC    DATAIND7,7(R1)                                                   
         OC    DATAIND8,8(R1)                                                   
         OC    DATAIND9,9(R1)                                                   
         OC    DTAIND10,10(R1)                                                  
         OC    DTAIND11,11(R1)                                                  
         OC    DTAIND12,12(R1)                                                  
         BR    RE                                                               
*                                                                               
SETSX    B     XIT                                                              
         SPACE 2                                                                
STACKDOL DS    0XL13                                                            
         DC    AL1(STORD),AL1(0,0,DILOCKIN,0),AL4(0),AL4(0)                     
         DC    X'00'                                                            
*                                                                               
STACKDEM DS    0XL13                                                            
         DC    AL1(STGOAL),AL1(0,DIGLDEM,0,DIWEIGHT),AL4(0),AL4(0)              
         DC    AL1(STGCPP),AL1(DICPP,DIGLDEM,0,DIWEIGHT),AL4(0),AL4(0)          
         DC    AL1(STORD),AL1(0,DIGLDEM,DILOCKIN,DIWEIGHT),AL4(0)               
         DC    AL4(0)                                                           
         DC    AL1(STOCPP),AL1(DICPP,DIGLDEM,DILOCKIN,DIWEIGHT),AL4(0)          
         DC    AL4(0)                                                           
         DC    AL1(STPUR),AL1(DIDEMP,0,0,DIWEIGHT),AL4(0),AL4(0)                
         DC    AL1(STAVPCH),AL1(DIDEMP,0,0,DIWEIGHT),AL4(0),AL4(0)              
         DC    AL1(STPCPP),AL1(DIDEMP+DICPP,0,0,DIWEIGHT),AL4(0),AL4(0)         
         DC    AL1(STPCPPN),AL1(DIDEMP+DICPP,0,0,DIWEIGHT),AL4(0)               
         DC    AL4(0)                                                           
         DC    AL1(STAVACH),AL1(DIDEMR,0,0,DIWEIGHT),AL4(0),AL4(0)              
         DC    AL1(STACH),AL1(DIDEMR,0,0,DIWEIGHT),AL4(0),AL4(0)                
         DC    AL1(STRCPP),AL1(DIDEMR+DICPP,0,0,DIWEIGHT),AL4(0),AL4(0)         
         DC    AL1(STRCPPN),AL1(DIDEMR+DICPP,0,0,DIWEIGHT),AL4(0)               
         DC    AL4(0)                                                           
         DC    AL1(STAFF),AL1(DIDEMA,0,0,DIWEIGHT),AL4(0),AL4(0)                
         DC    AL1(STAVAFF),AL1(DIDEMA,0,0,DIWEIGHT),AL4(0),AL4(0)              
         DC    AL1(STACPP),AL1(DIDEMA+DICPP,0,0,DIWEIGHT),AL4(0),AL4(0)         
         DC    AL1(STACPPN),AL1(DIDEMA+DICPP,0,0,DIWEIGHT),AL4(0)               
         DC    AL4(0)                                                           
         DC    AL1(STCPP),AL1(DICPP,0,0,DIWEIGHT),AL4(0),AL4(0)                 
         DC    AL1(STCPPN),AL1(DICPP,0,0,DIWEIGHT),AL4(0),AL4(0)                
         DC    AL1(STIMP),AL1(DICPP,0,0,DIWEIGHT),AL4(0),AL4(0)                 
         DC    AL1(STPWPCT),AL4(0),AL4(0),AL1(DIWIPW,0,0,0)                     
         DC    X'00'                                                            
*                                                                               
STACKACC DS    0XL13                                                            
         DC    AL1(STPAID),AL1(0,DIBYPAID,0,0),AL4(0),AL4(0)                    
         DC    AL1(STUNPD),AL1(0,DIBYPAID,0,0),AL4(0),AL4(0)                    
         DC    X'00'                                                            
         EJECT                                                                  
* CALL DRIVER FOR INPUT FOR AUTHORIZATION DOLLARS                               
*                                                                               
AUTHOR   L     R2,SBAIO1                                                        
         USING ESTHDRD,R2                                                       
         MVC   SBBMKT,=X'FFFD'     ACROSS ALL MARKETS/STATIONS                  
         XC    SBSTA,SBSTA                                                      
         MVC   SBSTA,=C'AAAAA'                                                  
         MVC   SBAFFIL,=C'ALL'                                                  
         XC    SBCHAN,SBCHAN                                                    
         MVI   SBCHAN,X'FF'                                                     
         XC    WORK,WORK                                                        
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         L     R3,SBAIO3           GET THE ESTIMATES'S MONTHS                   
         GOTO1 MOBILE,DMCB,(12,SBESTST),(DATEFORM,(R3)),WORK,SBSPPROF           
         GOTO1 DATCON,DMCB,(2,2(R3)),(3,FULL)                                   
         LLC   R5,FULL+1           R5=MONTH NUMBER                              
         L     R7,SBADATE          ACCUMULATE AUTH DOLLARS FOR EACH             
         L     RF,SBNDATES         REQUESTED DATE PERIOD                        
*                                                                               
AUTH2    ZAP   DUB,=P'0'           DUB=DOLLAR ACCUMULATOR                       
*                                                                               
AUTH4    CLC   0(2,R7),0(R3)       TEST ESTIMATE MONTH WITHIN PERIOD            
         BH    AUTH6                                                            
         CLC   2(2,R7),2(R3)                                                    
         BL    AUTH8                                                            
         LR    RE,R5               YES-GET AUTH DOLLARS FOR THIS MONTH          
         BCTR  RE,0                                                             
         MHI   RE,6                                                             
         LA    RE,EAUTH(RE)                                                     
         AP    DUB,0(6,RE)                                                      
*                                                                               
AUTH6    LA    R3,4(R3)            NEXT ESTIMATE MONTH                          
         CLI   0(R3),X'FF'                                                      
         BE    AUTH8                                                            
         LA    R5,1(R5)            AUGMENT MONTH NUMBER                         
         CHI   R5,13                                                            
         BL    AUTH4                                                            
         BH    *+12                                                             
         CLI   DATEFORM,10         13TH MONTH ALLOWED FOR 4-WEEK MONTHS         
         BE    AUTH4                                                            
         LA    R5,1                                                             
         B     AUTH4                                                            
*                                                                               
AUTH8    CP    DUB,=P'0'           TEST ANY AUTH DOLLARS                        
         BE    AUTH10                                                           
         L     RE,SBACHUNK         YES-                                         
         ZAP   4(8,RE),DUB         STORE AUTH DOLLARS                           
         MVC   0(4,RE),0(R7)       AND PERIOD START/END                         
         BAS   RE,LDRIVIN          CALL DRIVER FOR INPUT                        
*                                                                               
AUTH10   CLI   0(R3),X'FF'         TEST END OF ESTIMATE MONTHS                  
         BE    AUTHX                                                            
         LA    R7,4(R7)            NO-NEXT PERIOD                               
         BCT   RF,AUTH2                                                         
*                                                                               
AUTHX    B     XIT                                                              
         EJECT                                                                  
* CALL DRIVER FOR INPUT FOR PG ESTIMATE RECORDS                                 
*                                                                               
PGEST    LR    R2,R1               R1=A(PGDATA FIELD)                           
         USING PGDATAD,R2                                                       
         MVC   0(PGDATAL,R2),EFFS   SET TO NOT FOUND                            
         L     R3,SBAIO2                                                        
*                                                                               
PGEST1   OC    0(256,R3),0(R3)     TEST THERE'S A PG EST RECORD                 
         BZ    PGEST6              NO                                           
         MVC   0(PGDATAL,R2),BLANKS YES-                                        
         LA    R3,PGKEDQ(R3)       SEARCH FOR DATA ELEMENTS                     
         SR    R0,R0                                                            
*                                                                               
PGEST2   CLI   0(R3),0                                                          
         BE    PGEST6                                                           
         CLI   0(R3),PGSTEIDQ                                                   
         BNE   PGEST4                                                           
         USING PGSTELMD,R3         SET PG DATA FIELDS                           
         CLC   PGSTNAME,=CL8'CHRGPER'                                           
         BNE   *+10                                                             
         MVC   PGCHPER,PGSTDATA                                                 
         CLC   PGSTNAME,=CL8'ACCOUNT'                                           
         BNE   *+10                                                             
         MVC   PGACCNT,PGSTDATA                                                 
         CLC   PGSTNAME,=CL8'BRAND'                                             
         BNE   *+10                                                             
         MVC   PGBRAND,PGSTDATA                                                 
         CLC   PGSTNAME,=CL8'ESTIMATE'                                          
         BNE   *+10                                                             
         MVC   PGESTIM,PGSTDATA                                                 
         CLC   PGSTNAME,=CL8'EVENTCD'                                           
         BNE   *+10                                                             
         MVC   PGEVENT,PGSTDATA                                                 
         CLC   PGSTNAME,=CL8'MLTBRND'                                           
         BNE   *+10                                                             
         MVC   PGMULTI,PGSTDATA                                                 
         CLC   PGSTNAME,=CL8'NOBRAND'                                           
         BNE   *+10                                                             
         MVC   PGNOBRD,PGSTDATA                                                 
         CLC   PGSTNAME,=CL8'FISYREND'                                          
         BNE   *+10                                                             
         MVC   PGSRSTRT,PGSTDATA                                                
         CLC   PGSTNAME,=CL8'BRDSUFF'                                           
         BNE   *+10                                                             
         MVC   PGESUF,PGSTDATA                                                  
         CLC   PGSTNAME,=CL8'ACCEST'                                            
         BNE   PGEST4                                                           
         LLC   RE,PGSTDATA                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PGOREST,DUB                                                      
*                                                                               
PGEST4   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PGEST2                                                           
*                                                                               
PGEST6   BAS   RE,LDRIVIN                                                       
         B     PGESTX                                                           
*                                                                               
*&&DO                                                                           
         CLC   SBBPRD,SVPRD        TEST NEW PRODUCT                             
         BE    PGESTX                                                           
         MVC   SVPRD,SBBPRD                                                     
         CLI   SBBPRD,X'FF'        AND NOT POL                                  
         BE    PGESTX                                                           
         MVC   0(PGDATAL,R2),EFFS   YES-THE EST=0 RECORD IS IN AIO3             
         L     R3,SBAIO3                IF THERE IS ONE                         
         MVI   SBBEST,0                                                         
         B     PGEST1                                                           
*&&                                                                             
*                                                                               
PGESTX   B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
* PROCESS CLEARANCE STATUS RECORD                                               
*                                                                               
         SPACE 1                                                                
CLRST    L     R2,SBAIO1                                                        
         USING CLRSTATD,R2                                                      
         LA    R3,CLSELEMS                                                      
         SR    R0,R0                                                            
*                                                                               
CLRST2   CLI   0(R3),0             LOOK FOR CLEARANCE ELEMENTS                  
         BE    CLRSTX                                                           
         CLI   0(R3),1                                                          
         BNE   CLRST20                                                          
         USING CLSTEL01,R3                                                      
         OC    SBBQSTP,SBBQSTP     CHECK FOR REQUEST DATES                      
         BZ    CLRST4                                                           
         CLC   CLSTCLRD,SBBQSTP                                                 
         BL    CLRST20                                                          
         CLC   CLSTCLRD,SBBQENDP                                                
         BH    CLRST20                                                          
*                                                                               
CLRST4   CLI   SBQBPRD,0           CHECK PRODUCT FILTERS                        
         BE    CLRST6                                                           
         CLC   CLSTPRD,SBQBPRD                                                  
         BNE   CLRST20                                                          
         CLC   CLSTPRD2,SBQBPRD2                                                
         BNE   CLRST20                                                          
*                                                                               
CLRST6   DS    0H                  GET PRODUCT'S BUFFER ENTRY                   
         LLC   RE,CLSTPRD                                                       
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         CLC   SBQPGRF,BLANKS      TEST PRODUCT GROUP FILTER                    
         BNH   *+14                                                             
         OC    PBGROUP,PBGROUP     YES-CHECK PRODUCT'S IN GROUP                 
         BZ    CLRST20                                                          
*                                                                               
         CLC   SBBPRD,CLSTPRD      TEST PRODUCT CHANGE                          
         BE    CLRST8                                                           
         MVC   SBPRD,PBALPH                                                     
         MVC   SBPRDNM,PBNAME                                                   
         MVC   SBBPGR,PBGROUP                                                   
         OC    SBBPGR,SBBPGR                                                    
         BNZ   CLRST8                                                           
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
CLRST8   CLC   SBBPRD2,CLSTPRD2    TEST 2ND PRODUCT CHANGE                      
         BE    CLRST10                                                          
         MVC   SBBPRD2,CLSTPRD2                                                 
         XC    SBPRD2,SBPRD2                                                    
         CLI   SBBPRD2,0                                                        
         BE    CLRST10                                                          
         LLC   RE,CLSTPRD2                                                      
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         MVC   SBPRD2,PBALPH                                                    
         DROP  R1                                                               
*                                                                               
CLRST10  ST    R3,SBACURCH         SAVE A(CURRENT ELEMENT)                      
         BAS   RE,LDRIVIN          PASS ELEMENT TO DRIVER                       
*                                                                               
CLRST20  IC    R0,1(R3)            NEXT ELEMENT                                 
         AR    R3,R0                                                            
         B     CLRST2                                                           
*                                                                               
CLRSTX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* GET DEMO INFORMATION IF IT'S NEEDED                                           
*                                                                               
DEMOS    CLI   SBQMKTWT,C'D'       TEST WHETHER DEMOS NEEDED                    
         BNE   *+14                                                             
         OC    PRMYDEMO,PRMYDEMO                                                
         BZ    DM10                                                             
*        TM    DATAIND4,DIDEMHED   COULD BE SUPPRESSING TARGET                  
*        BZ    DMX                                                              
         OC    DEMNAMES,DEMNAMES                                                
         BNZ   DMX                                                              
*                                                                               
DM10     OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU OR DEMO OPTION SET            
         BZ    DM20                                                             
         LA    R3,SBPDEMOS                                                      
         SR    R4,R4                                                            
         B     DM40                                                             
*                                                                               
DM20     OC    SBBCLT,SBBCLT       TEST CLIENT SET                              
         BNZ   DM22                                                             
         OC    SBQBCLT,SBQBCLT     NO - TEST SINGLE CLIENT REQUEST              
         BZ    DMX                                                              
         MVC   SBBCLT,SBQBCLT                                                   
*                                                                               
DM22     OC    SBPRD,SBPRD         TEST PRODUCT SET                             
         BNZ   DM25                                                             
         CLC   SBQPRD,=C'ALL'      NO - TEST SINGLE PRODUCT                     
         BE    DMX                                                              
         CLI   SBQPRD,C'0'                                                      
         BNL   DMX                                                              
         MVC   SBPRD,SBQPRD             YES - OK                                
*                                                                               
DM25     XC    KEY,KEY             READ ESTIMATE HEADER                         
         LA    R2,KEY                                                           
         USING ESTHDRD,R2                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,SBPRD                                                    
         CLC   SBPRD,=X'FEFEFE'    TEST PRD=UNALLOCATED                         
         BNE   *+10                                                             
         MVC   EKEYPRD,=C'POL'     YES-USE POL                                  
         MVC   EKEYEST,SBQEST      ESTIMATE START                               
         CLI   SBQSEPES,C'Y'                                                    
         BE    DM30                                                             
         CLC   SBQPRD,=C'ALL'                                                   
         BE    DM30                                                             
         MVC   EKEYEST,ESTSTART                                                 
*                                                                               
DM30     GOTO1 HIGH                                                             
         CLC   EKEY(EKEYEST-EKEY),KEYSAVE                                       
         BNE   DMX                                                              
         OC    EKEY+8(5),EKEY+8    TEST ITS AN ESTIMATE RECORD                  
         BZ    *+14                                                             
         MVC   EKEY+8(5),EFFS                                                   
         B     DM30                                                             
         CLC   EKEYEST,SBQESTND                                                 
         BH    DMX                                                              
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         CLI   SBQSEPES,C'Y'       TEST SEPERATE ESTIMATES                      
         BE    DM38                                                             
         CLC   SBQPRD,=C'ALL'      NO-TEST PRD=ALL REQUEST                      
         BNE   DM38                                                             
         OC    SBQSTART,SBQSTART   YES-NEED CONTROL ESTIMATE FOR PRD            
         BZ    DM32                                                             
         CLC   EEND,SBQSTART           CHECK ESTIMATE DATES                     
         BL    DM36                                                             
         CLC   ESTART,SBQEND                                                    
         BH    DM36                                                             
*                                                                               
DM32     CLC   SBQESFLT,BLANKS     TEST FOR ESTIMATE FILTERING                  
         BNH   DM38                                                             
         TM    SBEFLAG5,SBE5EFNP   EST FILTER "OR" & NON-POSITIONAL?            
         BZ    DM32A               NO                                           
         BRAS  RE,ESTFILT          "OR" NON-POSITIONAL FILTER PASSED?           
         BE    DM38                YES                                          
         B     DM36                NO - FILTER THIS ONE OUT                     
DM32A    LA    R1,3                                                             
         LA    RE,SBQESFLT                                                      
         LA    RF,EPROF                                                         
*                                                                               
DM33     CLI   0(RE),C'*'                                                       
         BE    DM35                                                             
         CLI   0(RE),C' '                                                       
         BE    DM35                                                             
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    DM34                YES                                          
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   DM36                                                             
         B     DM35                                                             
*                                                                               
DM34     MVC   BYTE,0(RE)                                                       
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)          NEGATIVE FILTER MUST NOT MATCH               
         BE    DM36                                                             
*                                                                               
DM35     LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,DM33                                                          
         B     DM38                                                             
*                                                                               
DM36     LA    R2,KEY              SKIP TO NEXT ESTIMATE                        
         MVC   EKEY+8(5),EFFS                                                   
         B     DM30                                                             
*                                                                               
DM38     LA    R3,EDEMLST                                                       
         LA    R4,EUSRNMS                                                       
*                                                                               
DM40     GOTO1 AGETDEMS            GET THE DEMOS                                
*                                                                               
DMX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* ROUTINE TO GET DEMOS                                                          
* INPUT  : R3 = A(DEMO LIST)                                                    
*          R4 = A(USER DEMO NAME LIST)                                          
*          AIO/AIO1 = A(ESTIMATE RECORD)                                        
*                                                                               
GETDEMS  ST    R3,ADEMLST          SAVE A(DEMO LIST)                            
         CLI   SBQMKTWT,C'D'       TEST PRIMARY DEMO WEIGHTING                  
         BNE   *+14                                                             
         MVC   PRMYDEMO,0(R3)      YES - SET PRIMARY DEMO                       
         MVI   PRMYDEMO+1,C'U'                                                  
*        TM    DATAIND4,DIDEMHED   TEST FOR DEMOS IN HEADLINES                  
*        BZ    GETDX                                                            
         OC    DEMNAMES,DEMNAMES   AND DEMO NAMES REQUIRED                      
         BNZ   GETDX                                                            
         XC    DBLOCK,DBLOCK       YES - CALL DEMOCON FOR DEMO NAMES            
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBTPTT,C'T'                                                      
         MVC   DBCOMFCS,SBCOMFAC                                                
         MVC   DBFILE,=C'TP '                                                   
*                                                                               
         L     R1,AIO1             A(ESTIMATE RECORD)                           
         USING ESTHDRD,R1          ESTIMATE RECORD DSECT                        
         LA    R5,ENONTDMS         COMSCORE DEMOS LIST                          
         OC    SBPDEMOS,SBPDEMOS   DEMO MENU OR DEMO OPTION SET?                
         BZ    *+8                 NO                                           
         LA    R5,COMDLIST         COMSCORE DEMO LIST FROM MENU/DEMO            
         DROP  R1                  DROP USING                                   
*                                                                               
         MVI   DBSELMED,C'T'                                                    
         CLI   SBMED,C'R'                                                       
         BNE   *+12                                                             
         MVI   DBSELMED,C'R'                                                    
         B     GDEM10                                                           
*                                                                               
         CLI   SBCEXTRA,C'U'       TEST US DEMOS                                
         BE    GDEM10                                                           
*                                                                               
         LA    R1,SBAGYREC                                                      
         USING AGYHDR,R1                                                        
         CLI   AGYPROF+7,C'C'                                                   
         BNE   GDEM10                                                           
         MVI   DBSELMED,C'C'                                                    
         DROP  R1                                                               
*                                                                               
GDEM10   GOTO1 DEMOCON,DMCB,(8,(R3)),(2,DEMNAMES),(C'S',DBLOCK),       X        
               (SBSPPROF+9,(R4)),(R5)                                           
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
* DRIVER INPUT ROUTINE                                                          
*                                                                               
         USING GLOBALD,R4                                                       
LDRIVIN  NTR1  ,                                                                
*                                                                               
DRIVIN   MVC   SVMKTIND,MKTIND                                                  
         MVC   SVMAXTLV,MAXTOTLV                                                
         MVI   NETWKSW,C'N'                                                     
         OC    SBNETWK,SBNETWK     TEST NETWORK IS ACTIVE                       
         BZ    DRIVIN8                                                          
         TM    DATAIND2,DISTA      YES-TEST STATION OR NETSTA IN ROWS           
         BO    *+12                    (ONLY ONE OF THEM CAN BE IN              
         TM    DATAIND6,DINETSTA       SAME REPORT)                             
         BZ    DRIVIN4                                                          
         CLI   STALEV,0            YES-TEST ITS LEVEL SET YET                   
         BNE   DRIVIN6                                                          
         LA    R1,LEVELS           NO-GETS ITS LEVEL                            
         LA    RE,L'LEVELS                                                      
*                                                                               
DRIVIN2  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),QSTA                                                       
         BE    DRIVIN3                                                          
         CLI   0(R1),QNETSTA                                                    
         BE    DRIVIN3                                                          
         LA    R1,1(R1)                                                         
         BCT   RE,DRIVIN2                                                       
         DC    H'0'                                                             
*                                                                               
DRIVIN3  LA    RE,LEVELS                                                        
         SR    R1,RE                                                            
         LA    R1,1(R1)                                                         
         STC   R1,STALEV                                                        
         B     DRIVIN6                                                          
*                                                                               
DRIVIN4  CLI   MKTLEV,0            TEST MARKET IN ROWS                          
         BE    DRIVIN8             NO                                           
*                                                                               
DRIVIN6  MVI   NETWKSW,C'Y'                                                     
*                                                                               
DRIVIN8  CLI   NETWKSW,C'Y'                                                     
         BNE   DRIVIN9                                                          
         CLI   SBQNETWK,C'N'       TEST NETWORK LEVEL ONLY                      
         BE    DRIVIN10            YES-SKIP LOCAL LEVEL STATION                 
         TM    SBINDS,SBINETBL     TEST NETWORK BILLING RECORD                  
         BZ    DRIVIN9                                                          
         TM    DATAIND6,DINETSTA   YES-TEST NETSTA IN ROWS                      
         BZ    DRIVIN10            NO-ONLY PASS THE NETWORK                     
         CLI   SBQNETWK,C'L'       YES-EXIT IF LOCAL ONLY                       
         BE    DRIVINX                                                          
*                                                                               
DRIVIN9  TM    SBEFLAG8,SBE8CNWK   NETWORK=NWK FLAG?                            
         BZ    *+14                NO                                           
         OC    SBBMKT,SBBMKT       MARKET 0000?                                 
         BZ    DRIVIN11            YES - THIS REC WON'T GET TOTALLED            
         BAS   RE,GODRIVIN                                                      
         MVI   MKTIND,FF           SET COMBINED MARKET                          
         CLI   SVMKTIND,C'O'       TEST ORIGINATING MARKET                      
         BE    *+12                                                             
         CLI   SVMKTIND,C'S'       OR SPILL MARKET                              
         BNE   DRIVIN10                                                         
         CLI   MKTLEV,0            AND REPORTING MARKET                         
         BE    DRIVIN10                                                         
         CLC   MKTLEV,MAXTOTLV     YES-                                         
         BNH   *+10                                                             
         MVC   MAXTOTLV,MKTLEV     DO NOT GENERATE ANY TOTALS                   
         BAS   RE,GODRIVIN         CALL DRIVER FOR COMBINED MARKET              
*                                                                               
DRIVIN10 CLI   NETWKSW,C'Y'                                                     
         BNE   DRIVINX                                                          
         CLI   SBQNETWK,C'L'       TEST NETWORK LOCAL LEVEL STA ONLY            
         BE    DRIVINX             YES-SKIP THE NETWORK LEVEL                   
         TM    SBEFLAG8,SBE8CNWK   NETWORK=NWK FLAG?                            
         BNZ   DRIVINX             YES - SKIP NTWK LEVEL (SUM OF LOCAL)         
*                                                                               
DRIVIN11 MVC   SVSTA,SBSTA         CALL DRIVER FOR THE NETWORK                  
         MVC   SVBMKT,SBBMKT                                                    
         MVC   SVMKTNM,SBMKTNM                                                  
         MVC   SVBMGR,SBBMGR                                                    
         TM    DATAIND6,DINETSTA   TEST NETSTA                                  
         BZ    DRIVIN12                                                         
         MVC   SBSTA,BLANKS        YES-                                         
         CLI   SBQNETWK,C'N'       NETWORK ONLY                                 
         BE    DRIVIN16                                                         
         MVC   SBSTA,EFFS          NETWORK TOTAL                                
         B     DRIVIN14                                                         
*                                                                               
DRIVIN12 MVC   SBSTA(4),SBNETWK                                                 
         MVI   SBSTA+4,C' '                                                     
         MVC   SBBMKT,=X'FFFE'                                                  
         MVC   SBMKTNM,BLANKS                                                   
         MVC   SBMKTNM(8),=C'NETWORKS'                                          
         MVC   SBBMGR,=X'9998'                                                  
*                                                                               
DRIVIN14 CLI   SBQNETWK,C'N'       TEST NETWORK ONLY                            
         BE    DRIVIN16            YES-DON'T SUPPRESS TOTALS                    
         TM    SBINDS,SBINETBL     TEST NETWORK BILLING                         
         BZ    *+12                                                             
         TM    DATAIND2,DISTA      AND STANET IN THE ROWS                       
         BO    DRIVIN16            YES-DON'T SUPPRESS TOTALS                    
         MVC   BYTE,MKTLEV                                                      
         CLI   MKTLEV,0                                                         
         BNE   *+10                                                             
         MVC   BYTE,STALEV                                                      
         CLC   BYTE,MAXTOTLV                                                    
         BNH   DRIVIN16                                                         
         MVC   MAXTOTLV,BYTE                                                    
*                                                                               
DRIVIN16 BAS   RE,GODRIVIN         CALL DRIVER FOR THE NETWORK                  
         MVC   SBSTA,SVSTA                                                      
         MVC   SBBMKT,SVBMKT                                                    
         MVC   SBMKTNM,SVMKTNM                                                  
         MVC   SBBMGR,SVBMGR                                                    
*                                                                               
DRIVINX  MVC   MKTIND,SVMKTIND                                                  
         MVC   MAXTOTLV,SVMAXTLV                                                
         B     XIT                                                              
         SPACE 2                                                                
GODRIVIN LR    R0,RE               DRIVER CALLING ROUTINE                       
         TM    SBEFLAG8,SBE8CP1    COMSCORE PASS 1?                             
         BNZ   GODRIVX             YES - SKIP DRIVER INPUT CALL                 
         L     RB,PRGBASE1                                                      
         L     RA,PRGBASE2                                                      
         L     R8,PRGBASE3                                                      
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R4)    CALL DRIVER FOR INPUT                        
         L     RB,EXTBASE1                                                      
         L     RA,EXTBASE2                                                      
         L     R8,EXTBASE3                                                      
         TM    ROWIND,ROWIMRTO     TEST MARKET RANK TOTALS NEEDED               
         BZ    GODRIVX                                                          
         MVC   SVMKTRNK,SBMKTRNK   YES-EXTRA TOTAL RECORDS                      
         CLC   MKRKLEV,MAXTOTLV                                                 
         BNH   *+10                                                             
         MVC   MAXTOTLV,MKRKLEV                                                 
         LLC   R2,SBMKTRNK                                                      
         LA    R2,5(R2)                                                         
         CLI   SBMKTRNK,10                                                      
         BE    GODRIV3                                                          
*                                                                               
GODRIV2  STC   R2,SBMKTRNK                                                      
         CLI   SBMKTRNK,75                                                      
         BH    GODRIV4                                                          
         L     RB,PRGBASE1                                                      
         L     RA,PRGBASE2                                                      
         L     R8,PRGBASE3                                                      
         GOTO1 DRIVER,DMCB,(R4)                                                 
         L     RB,EXTBASE1                                                      
         L     RA,EXTBASE2                                                      
         L     R8,EXTBASE3                                                      
*                                                                               
GODRIV3  LA    R2,10(R2)                                                        
         B     GODRIV2                                                          
*                                                                               
GODRIV4  MVC   SBMKTRNK,SVMKTRNK                                                
*                                                                               
GODRIVX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO SET THE DEMO LIST (SBEDEMOS) FOR PRD GROUP DEMO GROUPING           
*                                                                               
SETDEMO  L     R3,SBAIO1                                                        
         USING BUYREC,R3                                                        
         CLI   BUYKPRD,X'FF'       ONLY FOR POOL BUYS                           
         BNE   SETDX                                                            
         SR    R0,R0               FIND A POOL BUY ELEMENT                      
         LA    R1,BDELEM                                                        
*                                                                               
SETD2    CLI   0(R1),0                                                          
         BE    SETDX                                                            
         CLI   0(R1),11                                                         
         BL    SETD4                                                            
         CLI   0(R1),13                                                         
         BH    SETD4                                                            
         CLI   1(R1),10            FOUND-TEST PRODUCT IS ALLOCATAED             
         BH    SETD6               YES                                          
*                                                                               
SETD4    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     SETD2                                                            
*                                                                               
         USING REGELEM,R1                                                       
SETD6    LLC   RE,RPPRD            EXTRACT THE PRODUCT                          
         DROP  R1                                                               
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,BUYKEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)          POINTER TO ESTIMATE BUFFER ENTRY             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         MHI   R1,ESTBUFFL                                                      
         A     R1,SBAESTBF                                                      
         USING ESTBUFFD,R1                                                      
         MVC   SBEDEMOS,EBDEMOS    EXTRACT THE DEMOS FOR THE PRODUCT            
         DROP  R1,R3                                                            
*                                                                               
SETDX    B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO GET BUYER/BILLER NAMES                                             
* INPUT :  SVNAME=CLT(2)/PRD(1)/EST(1)/MKT(2)                                   
*                                                                               
GETNAME  MVC   SBBYRNM,ASTERS      INIT TO STARS                                
         MVC   SBBLRNM,ASTERS                                                   
         XC    KEY,KEY             BUILD STATUS RECORD KEY                      
         LA    R2,KEY                                                           
         USING STATD,R2                                                         
         MVC   STKTYPE,=X'0D71'                                                 
         MVC   STKAGMD,SBBAGYMD                                                 
         MVC   STKCLT,SVNAME                                                    
         MVC   STKPRD,SVNAME+2                                                  
         MVC   STKEST,SVNAME+3                                                  
         MVC   STKMKT,SVNAME+4                                                  
         CLI   SBSTPROF+0,C'P'     TEST DATA BY PRODUCT                         
         BE    *+8                                                              
         MVI   STKPRD,0                                                         
         CLI   SBSTPROF+2,C'E'     TEST DATA BY ESTIMATE                        
         BE    *+8                                                              
         MVI   STKEST,0                                                         
******** CLC   SBQEST,SBQESTND     TEST REQUEST ACROSS ESTIMATES                
******** BE    GETNAM2                                                          
******** CLI   SBQSEPES,C'Y'                                                    
******** BE    GETNAM2                                                          
******** CLI   STKEST,0            YES-TEST DATA BY ESTIMATE                    
******** BNE   GETNAMX             YES-CANNOT GIVE THEM A NAME                  
*                                                                               
GETNAM2  DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST RECORD FOUND                            
         BE    GETNAM4             YES                                          
         MVC   KEY,KEYSAVE                                                      
*                                                                               
GETNAM3  CLI   STKPRD,0            NO-TEST NEED TO TRY PRD=POL                  
         BE    GETNAMX                                                          
         CLI   STKPRD,X'FF'                                                     
         BE    GETNAMX                                                          
         MVI   STKPRD,X'FF'        YES-TRY PRD=POL                              
         B     GETNAM2                                                          
*                                                                               
GETNAM4  LA    R2,IOAREA                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC              READ STATUS RECORD                           
         CLC   SBBYRNM,ASTERS                                                   
         BNE   GETNAM5                                                          
         CLC   BPBUYER,BLANKS                                                   
         BNH   GETNAM5                                                          
         MVC   SBBYRNM,BPBUYER     BUYER NAME                                   
*                                                                               
GETNAM5  CLC   SBBLRNM,ASTERS                                                   
         BNE   GETNAM6                                                          
         CLC   BPPAYER,BLANKS                                                   
         BNH   GETNAM6                                                          
         MVC   SBBLRNM,BPPAYER     BILLER NAME                                  
*                                                                               
GETNAM6  CLC   SBBYRNM,ASTERS      TEST BUYER OR BILLER NAME                    
         BE    *+14                STILL NOT SET                                
         CLC   SBBLRNM,ASTERS                                                   
         BNE   GETNAMX                                                          
         LA    R2,KEY              YES-TRY PRODUCT=POL                          
         B     GETNAM3                                                          
*                                                                               
GETNAMX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET MARKET GROUP FOR PRODUCT GROUP EXCEPTIONS                  
* INPUT  : SBBMKT=MARKET CODE                                                   
*          SBBPGR=PRODUCT GROUP                                                 
* OUTPUT : SBBMGR=MARKET GROUP                                                  
*                                                                               
GETMGR   SR    RE,RE                                                            
         ICM   RE,3,SBBMKT         INDEX INTO MARKET GRP ASSIGN TABLE           
         AR    RE,RE                                                            
         MVC   HALF,SBBPGR                                                      
         OC    HALF,SBPG1MSK                                                    
         LA    R0,SBEXMAX                                                       
         LA    R1,SBPGRPEX                                                      
         LA    RF,SBAMGTB2                                                      
*                                                                               
GM2      OC    0(2,R1),0(R1)                                                    
         BZ    GM4                                                              
         MVC   FULL(2),0(R1)                                                    
         OC    FULL(2),SBPG1MSK                                                 
         CLC   HALF,FULL           TEST IT'S THE EXCEPTION GROUP                
         BNE   *+12                                                             
         A     RE,0(RF)            YES-THEN USE APPROPRIATE MGRP TABLE          
         B     GM6                                                              
         LA    R1,2(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,GM2                                                           
*                                                                               
GM4      A     RE,SBAMGTAB                                                      
*                                                                               
GM6      MVC   SBBMGR,=X'9999'                                                  
         OC    0(2,RE),0(RE)                                                    
         BZ    GMX                                                              
         MVC   SBBMGR,0(RE)                                                     
*                                                                               
GMX      B     XIT                                                              
         EJECT                                                                  
* READ ESTIMATE HEADER TO GET ESTIMATE DEMOS                                    
* INPUT  : R1=A(PRODUCT)                                                        
*          SVEST=ESTIMATE                                                       
* OUTPUT : SBESTDEM=ESTIMATE DEMOS                                              
*                                                                               
GETEST   XC    SBESTDEM,SBESTDEM                                                
*                                                                               
         LR    RF,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   RF,4096                                                          
         USING SYSD+4096,RF                                                     
         XC    SBESTDM2,SBESTDM2                                                
         DROP  RF                                                               
*                                                                               
         XC    KEY,KEY             READ ESTIMATE HEADER                         
         LA    R2,KEY                                                           
         USING ESTHDRD,R2                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,0(R1)                                                    
         MVC   EKEYEST,SVEST                                                    
         GOTO1 HIGH                                                             
         CLC   EKEY(8),KEYSAVE                                                  
         BNE   GETESTX                                                          
         OC    EKEY+8(5),EKEY+8    TEST ITS AN ESTIMATE RECORD                  
         BNZ   GETESTX                                                          
         LA    R2,IOAREA                                                        
         TM    SBEFLAG5,SBE5GEST   READ EST INTO AIO1?                          
         BZ    *+8                 NO                                           
         L     R2,SBAIO1                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         TM    SBEFLAG8,SBE8CPX    ONLY GET ECSSDTE?                            
         BZ    GETEST04            NO                                           
         MVC   SBCOMSD,ECSSDTE     YES - COMSCORE SURVEY DATES (B/S)            
         CLI   SBCOMSD,X'40'       IS ECSSDTE INITIALIZED?                      
         BH    *+8                 YES                                          
         MVI   SBCOMSD,C'S'        NO - INITIALIZE TO AN "S"                    
         B     GETESTX             DONE                                         
*                                                                               
GETEST04 TM    SBEFLAG8,SBE8NTD    ONLY GET ENONTDMS?                           
         BZ    GETEST10            NO                                           
         TM    SBEFLAG8,SBE8NTDG   SET ENONTDMS FOR GOALS W/DEMO MENU?          
         BNZ   GETEST05            YES                                          
         MVC   COMDLIST,ENONTDMS   YES - NON-TRAD DEMO LIST                     
         B     GETESTX             DONE                                         
*                                                                               
GETEST05 ICM   RF,15,ACOMLSTG      A(COMSCORE DEMO LIST FOR GOALS)              
         BZ    GETESTX             IF NOT SET, EXIT                             
         MVC   0(160,RF),ENONTDMS  NON-TRAD DEMO LIST                           
         B     GETESTX             DONE                                         
*                                                                               
GETEST10 MVC   SBESTDEM,EDEMLST                                                 
*                                                                               
         LR    RF,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   RF,4096                                                          
         USING SYSD+4096,RF                                                     
         MVC   SBESTDM2,EDEMLST+6  TARGET3 AND TARGET4 DEMOS                    
         DROP  RF                                                               
*                                                                               
         TM    SBEFLAG5,SBE5GEST   GET MORE EST INFO?                           
         BZ    GETESTX             NO                                           
         TM    SBEFLAG5,SBE5UPOL   UDEF=POL?                                    
         BZ    GETESTX             NOPE                                         
         LA    R2,KEY              WE NEED POL EST REC IN AIO2                  
         MVC   EKEYPRD,=C'POL'     BECAUSE PUTESTN IN SPWRI0B                   
         GOTO1 HIGH                                                             
         MVC   AIO,SBAIO2          IS EXPECTING THE POL EST REC IN AIO2         
         GOTO1 GETREC                                                           
*                                                                               
GETESTX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* STATION FIRST ROUTINE                                                         
*                                                                               
         SPACE 1                                                                
FSTA     DS    0H                                                               
         LR    R7,R9               R7=A(SYSD)                                   
         AHI   R7,SBEFLAG8-SYSD    R7=A(SBEFLAG8)                               
         NI    0(R7),X'FF'-SBE8PP  TURN OFF PARENT+ FLAG FOR COMSCORE           
*                                                                               
         LR    R7,R9               R7=A(SBASTANT)                               
         AHI   R7,SBASTANT-SYSD                                                 
         XR    R2,R2                                                            
         ICM   R2,7,0(R7)          R2=SBASTANT                                  
*                                                                               
         LR    R7,R9               R7=A(STATION CITY FIELD)                     
         AHI   R7,SBSTACTY-SYSD                                                 
         LR    R1,R9               SET A(STATION NAME FIELD)                    
         AHI   R1,SBSTANM-SYSD                                                  
         ST    R1,ASTANAME                                                      
         MVC   THISSTA,SBBSTA      PASS THIS STATION TO BINSRCH                 
         LA    RE,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDRD(RE),C'C'    CANADA?                            
         BE    FSTA2               YES, NFC(ABLE) FOR CANADA                    
         CLI   THISSTA,X'E8'       CABLE?                                       
         BL    FSTA2               NO                                           
         NI    THISSTA+2,X'80'     STRIP NETWORK BITS                           
*                                                                               
FSTA2    SHI   R2,TSARKEYL+2                                                    
         XC    0(TSARKEYL+2,R2),0(R2)                                           
         MVI   2(R2),NPSTACDQ      REC TYPE (17)                                
         MVC   3(3,R2),THISSTA     STATION                                      
         BRAS  RE,TSARHIGH                                                      
         BNE   FSTA4                                                            
*                                                                               
         AHI   R2,TSARKEYL+2       R2=SBASTANT                                  
         USING STABUFFD,R2                                                      
         MVC   SBAFFIL,STBAFFIL    EXTRACT THE AFFILIATE                        
         MVC   SBCHAN,STBCHAN      AND CHANNEL                                  
         MVC   SBREP,STBREP        AND REP (CONTRACT)                           
         MVC   SBSTYPE,STBTYPE     AND TYPE                                     
         MVC   SBPREP,STBPREP      AND PAYING REP                               
         MVC   SBTREP,STBREP       AND TIME SHEET REP (=CONTRACT REP)           
         MVC   SBTREP,STBREP       AND TIME SHEET REP (=CONTRACT REP)           
         MVC   SBSIZE,STBSIZE      AND STATION SIZE                             
*                                                                               
         LR    R1,R7               STATION CITY                                 
         MVC   0(L'SBSTACTY,R1),BLANKS                                          
         MVI   BYTE,SCITY          ELEMENT CODE                                 
         BRAS  RE,GETELEM          GET ELEM AND MOVE DATA TO 0(R1)              
*                                                                               
         MVC   SBSTAST,STBSTATE    AND STATION STATE                            
         MVC   SBSTAFOR,STBFORM    AND STATION FORMAT                           
         MVC   SBSTAFAX,STBFAX     AND STATION FAX                              
         MVC   SBBSTMKT,STBMKT     AND STATION'S MARKET                         
*                                                                               
         L     R1,ASTANAME         STATION NAME                                 
         MVC   0(L'SBSTANM,R1),BLANKS                                           
         MVI   BYTE,SNAME          ELEMENT CODE                                 
         BRAS  RE,GETELEM          GET ELEM AND MOVE DATA TO 0(R1)              
*                                                                               
         LA    R1,HALF             MOVE PARENT PLUS FLAG HERE                   
         MVI   HALF,0              INIT PARENT PLUS FLAG                        
         MVI   BYTE,SPARENTP       ELEMENT CODE                                 
         BRAS  RE,GETELEM          GET ELEM AND MOVE DATA TO 0(R1)              
         CLI   HALF,C'Y'           PARENT PLUS STATION?                         
         BNE   FSTA3               NO                                           
         LR    RE,R9               RE=A(SYSD)                                   
         AHI   RE,SBEFLAG8-SYSD    RE=A(SBEFLAG8)                               
         OI    0(RE),SBE8PP        SET PARENT+ FLAG FOR COMSCORE                
*                                                                               
FSTA3    TM    DATAIND8,DISTANM    TEST STATION NAME NEEDED                     
         BZ    FSTAX                                                            
         L     R1,ASTANAME         STATION NAME                                 
         CLC   0(L'SBSTANM,R1),BLANKS YES-TEST READ STA ADDR RECORD YET         
         BH    FSTAX               YES                                          
*                                                                               
         LR    RE,R2                                                            
         LA    RF,SBUFMAX          CLEAR IT FOR THIS MANY BYTES                 
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     FSTA6               ENTRY BACK                                   
*                                                                               
FSTA4    AHI   R2,TSARKEYL+2       R2=SBASTANT                                  
         LA    R4,KEY              READ STATION RECORD                          
         USING STARECD,R4                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(16),STAKEY                                              
         MVI   STAKTYPE,C'S'                                                    
         CLI   SBQMED,C'*'         MEDIA '*'?                                   
         BNE   *+14                 NO                                          
         MVC   STAKMED,SBMED        YES - USE SBMED                             
         B     *+10                                                             
         MVC   STAKMED,SBQMED                                                   
*                                                                               
         MVC   STAKCALL,SBSTA                                                   
         CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         CLI   STAKMED,C'C'         TEST MEDIA IS C                             
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'C'     YES-STATION'S MEDIA IS C                     
         CLI   STAKMED,C'N'         TEST MEDIA IS N                             
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'N'     YES-STATION'S MEDIA IS N                     
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,SBCLT                                                    
*                                                                               
         CLI   STAKMED,C'*'        JUST MAKING SURE                             
         BNE   FSTA4A                                                           
         MVI   STAKMED,C'T'                                                     
         CLI   STAKCALL+4,C' '                                                  
         BE    FSTA4A                                                           
         CLI   STAKCALL+4,C'A'                                                  
         BE    *+12                                                             
         CLI   STAKCALL+4,C'F'                                                  
         BNE   *+8                                                              
         MVI   STAKMED,C'R'                                                     
*                                                                               
FSTA4A   DS    0H                                                               
         LAY   R4,STAIO                                                         
         ST    R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   STBAFFIL,SNETWRK    NETWORK AFFILIATION                          
         MVC   STBCHAN,SCHNL       STATION CHANNEL                              
         MVC   STBREP,SCONREP      CONTRACT REP                                 
         MVC   STBTYPE,STYPE       STATION TYPE                                 
         LA    RE,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDRD(RE),C'C'                                       
         BNE   *+16                CANADA HOLDS AFFIL ELSEWHERE                 
         MVC   STBAFFIL,SCANNTWK      ...AND IT CAN BE 4 BYTES SO HOLD          
         MVC   STBTYPE(1),SCANNTWK+3  ...FINAL BYTE IN (UNUSED) 'TYPE'          
         MVC   STBPREP,SPAYREP     PAYING REP                                   
         MVC   STBSIZE,SSIZE       STATION SIZE                                 
*                                                                               
         CLC   STAKLEN,=Y(STACRLNQ) REC BIG ENOUGH TO HAVE PARENT+?             
         BNH   FSTA4A0             NO                                           
         CLI   SPARPLUS,C'Y'       IS THIS STATION PARENT+?                     
         BNE   FSTA4A0             NO                                           
         MVI   HALF,SPARENTP       ELEMENT CODE                                 
         MVI   HALF+1,S10-2        FIXED ELEMENT LENGTH                         
         LA    R5,SPARPLUS         POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         LR    RE,R9               RE=A(SYSD)                                   
         AHI   RE,SBEFLAG8-SYSD    RE=A(SBEFLAG8)                               
         OI    0(RE),SBE8PP        SET PARENT+ FLAG FOR COMSCORE                
*                                                                               
FSTA4A0  CLI   SBSTA,C'0'          TEST CABLE STATION                           
         BL    FSTA4A1                                                          
         MVI   HALF,SNAME          ELEMENT CODE                                 
         MVI   HALF+1,S1-2         FIXED ELEMENT LENGTH                         
         LA    R5,SSYSNAME         POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
FSTA4A1  MVC   STBFORM,SFORMAT     FORMAT                                       
         MVC   STBFAX,SFAX         FAX                                          
         MVC   STBMALPH,SMKTALPH                                                
         LA    RE,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDRD(RE),C'C'                                       
         BNE   FSTA4A2             CANADA NEEDS GST/PST (HAS NO SGRPCD)         
         MVC   STBGST,SGSTCODE     GST                                          
         MVC   STBPST,SPST         PST                                          
         MVC   STBNTYPE,SNETTYPE   NETWORK TYPE                                 
         B     FSTA4B                                                           
*                                                                               
FSTA4A2  CLC   STAKLEN,=Y(STANCLNQ) GROUP CD & ORD DEADLINE PRESENT?            
         BL    FSTA4B                                                           
         MVC   STBGRPCD,SGRPCD     CABLE GROUP CODE                             
*                                                                               
FSTA4B   CLI   SBSTA,C'0'          TEST CABLE STATION                           
         BL    FSTA4B1              NO                                          
         TM    DATAIND8,DISTANM    TEST STATION NAME NEEDED                     
         BZ    FSTA4B1              NO                                          
         MVC   SBPREP,SPAYREP       PREP SHOULD COME FROM CLT-SPECIFIC          
         B     FSTA4C               YES - GET FROM NON-CLT REC                  
*                                                                               
FSTA4B1  TM    DATAIND2,DIAFFIL    TEST WANT NON-CLT SPECIFIC                   
         BZ    FSTA5               AFFILIATES                                   
         TM    OPTIND2,OPTIGENA                                                 
         BZ    FSTA5                                                            
*                                                                               
FSTA4C   CLC   STAKCLT,=C'000'     YES-TEST RECORD IS CLIENT SPECIFIC           
         BE    FSTA5                                                            
         XC    KEY,KEY             YES-READ NON-CLT SPECIFIC RECORD             
         MVC   KEY(L'STAKEY),0(R4)                                              
         LA    R4,KEY                                                           
         MVC   STAKCLT,=C'000'                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVC   STBAFFIL,SNETWRK    EXTRACT GENERIC AFFILIATION                  
         LA    RE,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDRD(RE),C'C'                                       
         BNE   *+16                CANADA HOLDS AFFIL ELSEWHERE                 
         MVC   STBAFFIL,SCANNTWK      ...AND IT CAN BE 4 BYTES SO HOLD          
         MVC   STBTYPE(1),SCANNTWK+3  ...FINAL BYTE IN (UNUSED) 'TYPE'          
         CLI   SBSTA,C'0'          TEST CABLE STATION                           
         BL    FSTA5                                                            
         MVI   HALF,SNAME          ELEMENT CODE                                 
         MVI   HALF+1,S1-2         FIXED ELEMENT LENGTH                         
         LA    R5,SSYSNAME         POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
FSTA5    MVC   SBAFFIL,SNETWRK                                                  
         MVC   SBCHAN,SCHNL                                                     
         MVC   SBREP,SCONREP                                                    
         MVC   SBSTYPE,STYPE                                                    
         LA    RE,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDRD(RE),C'C'                                       
         BNE   *+16                CANADA HOLDS AFFIL ELSEWHERE                 
         MVC   SBAFFIL,SCANNTWK       ...AND IT CAN BE 4 BYTES SO HOLD          
         MVC   SBSTYPE(1),SCANNTWK+3  ...FINAL BYTE IN (UNUSED) 'TYPE'          
***                                                                             
* IF CABLE AND STA NAME NEEDED WE SET SBPREP FROM CLT SPECIFIC MASTER           
* RECORD (IF THERE IS ONE) SO DO NOT GET THE NON CLT SPECIFIC PREP              
***                                                                             
         CLI   SBSTA,C'0'          CABLE STATION?                               
         BL    *+12                NO                                           
         TM    DATAIND8,DISTANM    STATION NAME NEEDED                          
         BNZ   *+10                YES, ALREADY SET THIS                        
         MVC   SBPREP,SPAYREP                                                   
         MVC   SBTREP,SCONREP                                                   
         MVC   SBSIZE,SSIZE                                                     
         MVC   SBSTAFOR,SFORMAT                                                 
         MVC   SBSTAFAX,SFAX                                                    
         PACK  DUB,SMKT            EXTRACT THE MARKET                           
         CVB   RE,DUB                                                           
         STCM  RE,3,STBMKT                                                      
         STCM  RE,3,SBBSTMKT                                                    
         MVC   STBCALL1,SRS1CALL   RTG SVC 1 CALL LETTERS - NSI(0)              
***      MVC   STBCALL2,SRS2CALL   RTG SVC 2 CALL LETTERS - BBM/ARB(1)          
         MVC   STBCALL2,SRS2CALL   RTG SVC 2 CALL LETTERS - BBM/NAM(1)          
*                                                                               
         LR    RF,R9               RF=A(SBEFLAG3)                               
         AHI   RF,SBEFLAG3-SYSD                                                 
         TM    0(RF),SBE3MS        WANT TO EXTRACT MSTREET DATA?                
         BZ    FSTA5E              NOPE                                         
***                                                                             
* EXTRACT MSTREET DATA                                                          
***                                                                             
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CT99KEY,R1                                                       
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSRA   READ RADIO FOR FREQ, STATE, & PARENT         
         MVC   CT99KSTA,STAKCALL   NOW READING THESE RECORD BY STATION          
         BAS   RE,SETSRC           SET THE SOURCE                               
*                                                                               
FSTA5AA  BRAS  RE,RDCTFILE         READ THE CONTROL FILE                        
         CLC   KEY(L'CT99KEY),KEYSAVE                                           
         BE    FSTA5AB                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'CT99KEY),KEYSAVE                                           
         CLI   CT99KSTA+4,C'T'     LOOKING FOR MEDIA T STATION?                 
         BNE   FSTA5E              NO, NO MATCH                                 
         CLI   CT99KSTA+4,C'L'     JUST LOOKED FOR BAND "L"?                    
         BE    FSTA5E              YES - NO MATCH                               
         MVI   CT99KSTA+4,C'L'     YES - LOOK FOR BAND "L"                      
         B     FSTA5AA             AND DO ANOTHER HIGH CALL                     
         DROP  R1                                                               
*                                                                               
FSTA5AB  L     R1,AIO                                                           
         LA    R1,28(R1)           DISPLACEMENT TO FIRST ELEM                   
         CLI   0(R1),X'01'         RECORD BROKEN?                               
         BNE   FSTA5E              YES, LET E'M COMPLAIN ABOUT IT               
         USING CRCLD,R1                                                         
         MVC   MSFREQ,CRCLFRQ      FREQUENCY                                    
         MVC   MSSTATE,CRCLSTE     MSTREET STATE                                
         MVC   MSPARENT,CRCLPRNT   PARENT CODE                                  
         MVC   MSUNIQID,CRCLUIDX   UNIQUE ID                                    
         DROP  R1                                                               
*                                                                               
         LLC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),X'02'         DO WE HAVE AN FM ELEMENT?                    
         BNE   FSTA5E              NOPE                                         
*                                                                               
         USING CRFMD,R1                                                         
         MVC   MSFORMAT,CRFMFMT    SAVE FORMAT (TO READ FORMAT REC)             
         MVC   MSOWNER,CRFMOWN     SAVE OWNER (TO READ OWNER RECORD)            
         MVI   MSMOWNER,0          MSTREET MINORITY OWNER                       
         MVI   MSMOWNF,0           MINORITY OWNERSHIP IS FEMALE                 
         MVI   MSFCCM,0            QUALIFIED FCC MINORITY                       
         MVI   MSFCCMF,0           FCC MINORITY OWNERSHIP IS FEMALE             
         CLI   CRFMLN,CRFMLNQ2     HAVE NEW ELEMENT LENGTH?                     
         BL    FSTA5AC             NO, NO MINORITY OWNER                        
         MVC   MSMOWNER,CRFMMIN    MSTREET MINORITY OWNER                       
         MVC   MSMOWNF,CRFMMINF    MINORITY OWNERSHIP IS FEMALE                 
         MVC   MSFCCM,CRFMQFCC     QUALIFIED FCC MINORITY                       
         MVC   MSFCCMF,CRFMQFCF    FCC MINORITY OWNERSHIP IS FEMALE             
*                                                                               
FSTA5AC  MVI   HALF,S07ELEM        ELEMENT CODE                                 
         MVI   HALF+1,S7-2         FIXED ELEMENT LENGTH                         
         LA    R5,MSFREQ           POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         DROP  R1                                                               
*                                                                               
FSTA5A   LLC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BE    FSTA5BB             YES                                          
         CLI   0(R1),X'08'         HAVE X'08' ELEMENT?                          
         BNE   FSTA5A              NO, KEEP LOOKING                             
*                                                                               
         USING CMADRMD,R1                                                       
         CLC   CMADAD1,BLANKS      HAVE ANY DATA?                               
         BNH   FSTA5A              NO, DON'T BUFFER ANY OF IT                   
         LA    R5,WORK                                                          
*                                                                               
         MVI   HALF,STADD1         ELEMENT CODE                                 
         CLI   CMADRTY,C'M'        MAILING ADDRESS?                             
         BNE   *+8                 NO                                           
         MVI   HALF,SMADD1         ELEMENT CODE                                 
         MVC   WORK(L'CMADAD1),CMADAD1                                          
         MVI   HALF+1,S8-2         MAX ELEMENT LENGTH                           
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
         CLC   CMADAD2,BLANKS      HAVE ADDRESS LINE 2?                         
         BNH   FSTA5B              NO                                           
         MVI   HALF,STADD2         ELEMENT CODE                                 
         CLI   CMADRTY,C'M'        MAILING ADDRESS?                             
         BNE   *+8                 NO                                           
         MVI   HALF,SMADD2         ELEMENT CODE                                 
         MVC   WORK(L'CMADAD2),CMADAD2                                          
         MVI   HALF+1,SA-2         MAX ELEMENT LENGTH                           
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
FSTA5B   CLC   CMADCITY,BLANKS     HAVE CITY?                                   
         BNH   FSTA5BA             NO                                           
         MVI   HALF,STCITY         ELEMENT CODE                                 
         CLI   CMADRTY,C'M'        MAILING ADDRESS?                             
         BNE   *+8                 NO                                           
         MVI   HALF,SMCITY         ELEMENT CODE                                 
         MVC   WORK(L'CMADCITY),CMADCITY                                        
         MVI   HALF+1,SC-2         MAX ELEMENT LENGTH                           
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
FSTA5BA  MVI   HALF,STADD3         ELEMENT CODE                                 
         CLI   CMADRTY,C'M'        MAILING ADDRESS?                             
         BNE   *+8                 NO                                           
         MVI   HALF,SMADD3         ELEMENT CODE                                 
         MVC   WORK(L'CMADSTAT),CMADSTAT                                        
         MVC   WORK+2(L'CMADZIP),CMADZIP                                        
         MVC   WORK+12(L'CMADCTRY),CMADCTRY                                     
         CLC   WORK(15),BLANKS     HAVE ANY DATA?                               
         BNH   FSTA5A              NO, DON'T BUFFER IT                          
         MVI   HALF+1,SE-2         MAX ELEMENT LENGTH                           
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         B     FSTA5A              GET THE NEXT ELEMENT                         
         DROP  R1                                                               
***                                                                             
* READ OWNER RECORD                                                             
**                                                                              
FSTA5BB  XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CT99KEY,R1                                                       
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSOW   READ OWNER RECORD                            
         MVC   CT99KOME,SBMED      MEDIA                                        
         MVC   CT99KOWN,MSOWNER    5 CHAR OWNER CODE                            
         BAS   RE,SETSRC           SET THE SOURCE                               
         BRAS  RE,RDCTFILE         READ THE CONTROL FILE                        
         CLC   KEY(L'CT99KEY),KEYSAVE                                           
         BNE   FSTA5C                                                           
         L     R1,AIO                                                           
         LA    R1,28(R1)           DISPLACEMENT TO FIRST ELEM                   
         USING CONAMD,R1                                                        
         CLI   0(R1),X'01'         RECORD BROKEN?                               
         BNE   FSTA5C              YES, LET E'M COMPLAIN ABOUT IT               
         MVI   HALF,SOWNR          ELEMENT CODE                                 
         MVI   HALF+1,S6-2         MAX ELEMENT LENGTH                           
         LA    R5,CONAME           POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         DROP  R1                                                               
***                                                                             
* READ FORMAT RECORD                                                            
***                                                                             
FSTA5C   XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CT99KEY,R1                                                       
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSFM   READ FORMAT RECORD                           
         MVC   CT99KFME,SBMED      MEDIA                                        
         MVC   CT99KFRM,MSFORMAT   3 CHAR FORMAT CODE                           
         BAS   RE,SETSRC           SET THE SOURCE                               
         BRAS  RE,RDCTFILE         READ THE CONTROL FILE                        
         CLC   KEY(L'CT99KEY),KEYSAVE                                           
         BNE   FSTA5D                                                           
         L     R1,AIO                                                           
         LA    R1,28(R1)           DISPLACEMENT TO FIRST ELEM                   
         CLI   0(R1),X'01'         RECORD BROKEN?                               
         BNE   FSTA5D              YES, LET E'M COMPLAIN ABOUT IT               
         USING CFNAMD,R1                                                        
         MVI   HALF,SFRMAT         ELEMENT CODE                                 
         MVI   HALF+1,S4-2         MAX ELEMENT LENGTH                           
         LA    R5,CFNAME           POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         DROP  R1                                                               
***                                                                             
* READ MULTIMEDIA RECORD                                                        
***                                                                             
FSTA5D   XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CT99KEY,R1                                                       
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSMM   READ MULTIMEDIA RECORD                       
         MVC   CT99KMMD,MSPARENT   5 CHAR PARENT CODE                           
         BAS   RE,SETSRC           SET THE SOURCE                               
         BRAS  RE,RDCTFILE         READ THE CONTROL FILE                        
         CLC   KEY(L'CT99KEY),KEYSAVE                                           
         BNE   FSTA5E                                                           
         L     R1,AIO                                                           
         LA    R1,28(R1)           DISPLACEMENT TO FIRST ELEM                   
         CLI   0(R1),X'01'         RECORD BROKEN?                               
         BNE   FSTA5E              YES, LET E'M COMPLAIN ABOUT IT               
         USING CMMNAMD,R1                                                       
         MVI   HALF,SPRENT         ELEMENT CODE                                 
         MVI   HALF+1,S5-2         MAX ELEMENT LENGTH                           
         LA    R5,CMMNAME          POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
         DROP  R1                                                               
*                                                                               
FSTA5E   TM    DATAIND8,DISTANM    TEST STATION NAME REQUIRED                   
         BZ    FSTA8                                                            
*                                                                               
FSTA6    MVI   0(R7),X'FE'         YES-INITIALISE STATION CITY,                 
         MVC   1(L'SBSTACTY-1,R7),0(R7)                   NAME,                 
         L     R1,ASTANAME                                                      
         MVC   0(L'SBSTANM,R1),0(R7)                      AND STATE             
         MVC   SBSTAST,0(R7)                                                    
         LA    R4,KEY              READ STATION ADDRESS RECORD                  
         USING ADDRRECD,R4                                                      
         MVI   ADDRKEY,C'0'                                                     
         MVC   ADDRKEY+1(L'ADDRKEY-1),ADDRKEY                                   
         MVI   ADDKTYPE,C'A'                                                    
         CLI   SBQMED,C'*'         MEDIA '*'?                                   
         BNE   *+14                 NO                                          
         MVC   ADDKMED,SBMED        YES - USE SBMED                             
         B     *+10                                                             
         MVC   ADDKMED,SBQMED                                                   
         MVC   ADDKCALL,SBSTA                                                   
         CLI   ADDKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   ADDKCALL+4,C'T'                                                  
         CLI   SBQMED,C'C'         TEST REQUEST MEDIA IS C                      
         BNE   *+12                                                             
         MVI   ADDKCALL+4,C'C'     YES-STATION'S MEDIA IS C                     
         B     FSTA6A                                                           
         CLI   ADDKMED,C'N'        TEST STATION'S MEDIA IS N                    
         BNE   FSTA6A                                                           
         MVI   ADDKCALL+4,C'N'     YES-STATION'S MEDIA IS N                     
         CLI   SBQNETWK,C'N'       IF PROCESSING NETWORK ONLY BUYS              
         BNE   FSTA6A                                                           
         OC    SBAFFIL,SBAFFIL                                                  
         BZ    FSTA6A                                                           
         MVC   ADDKCALL(3),SBAFFIL    WANT THE AFFIL NETWORK NAME               
         MVC   ADDKCALL+3(1),SBSTYPE                                            
FSTA6A   MVC   ADDKAGY,AGENCY                                                   
         LAY   R4,STAIO                                                         
         ST    R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         L     RE,ASTANAME                                                      
         CLI   8(R1),0                                                          
         BNE   FSTA7                                                            
         MVC   0(L'SBSTANM,RE),BLANKS                                           
         MVC   0(L'ANAME,RE),ANAME       EXTRACT STATION NAME                   
         OC    0(L'ANAME,RE),BLANKS      SPACE PAD THIS!                        
         MVC   0(L'SBSTACTY,R7),A2LINE                   CITY                   
         OC    0(L'SBSTACTY,R7),BLANKS   SPACE PAD THIS!                        
         MVC   SBSTAST,A3LINE                            STATE                  
*                                                                               
FSTA7    CLI   SBSTA,C'0'          IF CABLE, NAME ALREADY SET                   
         BL    FSTA7A                                                           
         LR    R1,RE               STATION NAME                                 
         MVC   0(L'SBSTANM,R1),BLANKS                                           
         MVI   BYTE,SNAME          ELEMENT CODE                                 
         BRAS  RE,GETELEM          GET ELEM AND MOVE DATA TO 0(R1)              
         B     FSTA7B                                                           
*                                                                               
FSTA7A   MVI   HALF,SNAME          ELEMENT CODE                                 
         MVI   HALF+1,S1-2         FIXED ELEMENT LENGTH                         
         LR    R5,RE               POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
FSTA7B   MVI   HALF,SADDR          ELEMENT CODE                                 
         MVI   HALF+1,S2-2         FIXED ELEMENT LENGTH                         
         LA    R5,A1LINE           POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
         MVC   STBIGZIP,ABIGZIP    ZIP+4                                        
*                                                                               
         MVI   HALF,SCITY          ELEMENT CODE                                 
         MVI   HALF+1,S3-2         FIXED ELEMENT LENGTH                         
         LR    R5,R7               POINT R5 TO DATA WE ARE BUFFERING            
         BRAS  RE,PUTELEM          ADD ELEMENT TO THE STATION BUFFER            
*                                                                               
         MVC   STBSTATE,SBSTAST                                                 
*                                                                               
FSTA8    LA    R3,STABUFFL+1+TSARKEYL+2                                         
         LA    R1,STBELEM          START OF ELEMENTS IN STA BUFFER              
         XR    RF,RF                                                            
*                                                                               
FSTA9    CLI   0(R1),0             END OF STATION BUFFER?                       
         BE    FSTA10              YES                                          
         IC    RF,1(R1)            LENGTH OF ELEMENT                            
         AR    R3,RF               ADD ELEMENT LENGTH TO BUFFER LENGTH          
         AR    R1,RF               BUMP PAST THIS ELEMENT                       
         B     FSTA9               AND CHECK FOR END OF STATION BUFFER          
         DROP  R2                                                               
*                                                                               
FSTA10   SHI   R2,TSARKEYL+2                                                    
         XC    0(TSARKEYL+2,R2),0(R2)                                           
         STCM  R3,3,0(R2)          LENGTH OF STATION BUFFER ENTRY               
         MVI   2(R2),NPSTACDQ      REC TYPE (17)                                
         MVC   3(3,R2),THISSTA     STATION                                      
         BRAS  RE,TSARADD                                                       
*                                                                               
FSTAX    B     XIT                                                              
*                                                                               
         USING CT99KEY,R1                                                       
SETSRC   MVI   CT99KSRC,C'S'       DEFAULT TO MSTREET                           
         TM    SBEFLAG6,SBE6MF     AGENCY USES MEDIA FRAMEWORKS?                
         BZ    *+8                                                              
         MVI   CT99KSRC,C'F'       MEDIA FRAMEWORKS                             
         TM    SBEFLAG7,SBE7SRDS   AGENCY USES SRDS?                            
         BZ    *+8                 NO                                           
         MVI   CT99KSRC,C'R'       SRDS                                         
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
* CABLE NAME ROUTINE                                                            
*                                                                               
         SPACE 1                                                                
CBLNM    MVC   WORK(35),BLANKS     INIT MSO/INTERCONNECT NAMES                  
         CLI   SBSTA,C'0'          TEST CABLE STATION                           
         BL    CBL6                NO-EXIT                                      
*                                                                               
         ICM   R2,15,SBACBLBF      TEST A(CABLE BUFFER) SET YET                 
         BNZ   CBL2                                                             
*                                  NO-CALL COVAIL TO GET STORAGE                
         LA    R3,1000             ***** MAX 1000 CABLE SYSTEMS *****           
         LR    R1,R3                                                            
         MHI   R1,CBLBUFFL                                                      
         LA    R1,8(R1)                                                         
         ST    R1,DMCB+4                                                        
         ST    R1,DMCB+8                                                        
         GOTO1 COVAIL,DMCB,C'GET'                                               
         ICM   R2,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R2,SBACBLBF         SET A(CABLE BUFFER)                          
         ST    R3,4(R2)            N'ENTRIES                                    
         XC    0(4,R2),0(R2)       NO RECORDS SO FAR                            
*                                                                               
CBL2     LA    R3,8(R2)                                                         
         L     R5,0(R2)                                                         
         L     R1,4(R2)                                                         
         ST    R1,DMCB+20                                                       
         LA    R1,CBLBUFFL                                                      
         ST    R1,DMCB+12                                                       
         GOTO1 BINSRCH,DMCB,(0,SBSTA),(R3),(R5),,(0,4)                          
         CLI   0(R1),1             TEST RECORD FOUND                            
         BE    CBL4                                                             
         ICM   R2,15,0(R1)         YES                                          
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         USING CBLBUFFD,R2                                                      
         MVC   WORK(15),CBMSONM    EXTRACT MSO NAME                             
         MVC   WORK+15(20),CBICNM  AND INTERCONNECT NAME                        
         B     CBL6                                                             
*                                                                               
CBL4     LA    R4,KEY              READ CABLE RECORD                            
         USING CBLRECD,R4                                                       
         MVI   CBLKEY,C'0'                                                      
         MVC   CBLKEY+1(L'CBLKEY-1),CBLKEY                                      
         MVI   CBLKTYPE,C'Y'                                                    
         MVC   CBLKMED,SBQMED                                                   
         MVC   CBLKCALL,SBSTA                                                   
         MVI   CBLKCALL+4,C'T'                                                  
         MVC   CBLKAGY,AGENCY                                                   
         LAY   R4,STAIO                                                         
         ST    R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   CBL5                                                             
         MVC   WORK(15),CSYSMSO    EXTRACT MSO NAME                             
         CLC   CSYSMSO,BLANKS      HAVE MSO NAME?                               
         BH    CBL4A               YES                                          
         XR    R1,R1                                                            
         ICM   R1,3,CBLKLEN        RECORD LENGTH                                
         CHI   R1,CBLLNQ2          HAVE MSO1 NAME?                              
         BNE   CBL4A                                                            
         CLC   CSYSMSO1,BLANKS     HAVE MSO1 NAME                               
         BNH   CBL4A               NO                                           
         MVI   WORK,C'*'           INDICATE THAT THIS COMES FROM MSO1           
         MVC   WORK+1(14),CSYSMSO1 MOVE MSO1 NAME IN                            
*                                                                               
CBL4A    MVC   WORK+15(20),CSYSICN AND INTERCONNECT NAME                        
*                                                                               
CBL5     L     R2,AIO2                                                          
         MVC   CBSYS,SBSTA         CABLE SYSTEM                                 
         MVC   CBMSONM,WORK        MSO NAME                                     
         MVC   CBICNM,WORK+15      INTERCONNECT NAME                            
*                                                                               
         L     R2,SBACBLBF         ADD RECORD TO TABLE                          
         L     R5,0(R2)                                                         
         L     R1,4(R2)                                                         
         ST    R1,DMCB+20                                                       
         LA    R1,CBLBUFFL                                                      
         ST    R1,DMCB+12                                                       
         GOTO1 BINSRCH,DMCB,(1,AIO2),(R3),(R5),,(0,4)                           
         OC    1(3,R1),1(R1)                                                    
         BZ    CBLX                TABLE FULL                                   
         CLI   0(R1),1                                                          
         BE    *+6                                                              
         DC    H'0'                RECORD MYSTERIOUSLY FOUND                    
         L     R1,8(R1)                                                         
         ST    R1,0(R2)            NUMBER OF RECORDS NOW IN TABLE               
*                                                                               
CBL6     LR    R1,R9               SET NAMES IN SPOTBLOCK                       
         AHI   R1,(SBCBLMSO-SYSD)                                               
         MVC   0(L'SBCBLMSO,R1),WORK                                            
         LR    R1,R9                                                            
         AHI   R1,(SBCBLIC-SYSD)                                                
         MVC   0(L'SBCBLIC,R1),WORK+15                                          
*                                                                               
CBLX     B     XIT                                                              
         EJECT                                                                  
* GET TOTAL MARKET WEIGHT AND NUMBER OF MARKETS FOR A TOTAL LEVEL               
* INPUT  : R1 = A(TOTAL LEVEL)                                                  
*                                                                               
GETTOTWT MVC   TOTWGT,=F'1'                                                     
         XC    TOTMKT,TOTMKT                                                    
         LA    RE,1                                                             
         SR    RF,RF                                                            
         SR    R3,R3                                                            
         ICM   R3,1,0(R1)          R3 = LEVEL                                   
         BZ    GWX                                                              
         SR    R1,R1                                                            
         CHI   R3,8                TEST LEVEL GT 8                              
         BNH   *+12                                                             
         SHI   R3,8                YES- WE'LL TEST 2ND BYTE OF SWITCHES         
         LA    R1,1                                                             
         SRDL  RE,1                SET MASK FOR EXECUTED TM                     
         BCT   R3,*-4                                                           
         SRL   RF,24                                                            
         L     R3,AMKTLST                                                       
         USING MKTLSTD,R3                                                       
         LA    R0,MLNMKTS                                                       
         SR    R4,R4               ACCUMULATE TOTAL MKT WGT                     
         SR    R5,R5               ACCUMULATE NUMBER OF MARKETS                 
*                                                                               
GW10     OC    MLMKT,MLMKT                                                      
         BZ    GW30                                                             
         LA    RE,MLLEVS(R1)                                                    
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    0(RE),0                                                          
         BZ    GW20                                                             
         ICM   RE,15,MLWGT                                                      
         AR    R4,RE                                                            
         LA    R5,1(R5)                                                         
*                                                                               
GW20     LA    R3,MKTLSTL(R3)      NEXT MARKET                                  
         BCT   R0,GW10                                                          
*                                                                               
GW30     STCM  R4,15,TOTWGT                                                     
         STCM  R5,15,TOTMKT                                                     
*                                                                               
GWX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* ALTER THE BUY RECORD FOR BONUS DEMOS PROCESSING                               
* ON EXIT, CC EQ - THERE ARE BONUS SPOTS                                        
*          CC NE - NO BONUS SPOTS                                               
*                                                                               
BYBONUS  L     R3,SBAIO1                                                        
         USING BUYREC,R3                                                        
         MVI   BYTE,0                                                           
         CLI   BUYKPRD,X'FF'       TEST POOL BUY                                
         BE    BYBON1                                                           
         OC    BDCOST,BDCOST       NO-TEST COST=0                               
         BNZ   BYBONX                 NO - NO BONUS SPOTS                       
         MVI   BYTE,1                 YES- ALL SPOTS ARE BONUS                  
         B     BYBONX                                                           
*                                                                               
BYBON1   SR    R0,R0               POOL BUY- SEARCH FOR BUY ELEMS               
         LA    R1,BDELEM                                                        
*                                                                               
BYBON2   CLI   0(R1),0                                                          
         BE    BYBONX                                                           
         CLI   0(R1),11                                                         
         BL    BYBON8                                                           
         CLI   0(R1),13                                                         
         BH    BYBON8                                                           
         USING REGELEM,R1          POOL BUY ELEMENT                             
         TM    RSTATUS,X'20'       TEST COST OVERRIDE                           
         BZ    BYBON4                                                           
         OC    RPCOST,RPCOST       YES-TEST OVERRIDE COST=0                     
         BZ    BYBON6                  YES-BONUS                                
         B     BYBON5                                                           
         DROP  R1                                                               
*                                                                               
BYBON4   OC    BDCOST,BDCOST       TEST SPOT COST=0                             
         BZ    BYBON6              YES-BONUS                                    
*                                                                               
BYBON5   OI    0(R1),X'80'         NOT BONUS- MARK ELEMENT                      
         B     BYBON8                                                           
*                                                                               
BYBON6   MVI   BYTE,1              INDICATE THERE ARE BONUS SPOTS               
*                                                                               
BYBON8   IC    R0,1(R1)            NEXT ELEMENT                                 
         AR    R1,R0                                                            
         B     BYBON2                                                           
*                                                                               
BYBONX   CLI   BYTE,1                                                           
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* GENERATE DAYPART TOTAL RECORDS FOR DRIVER                                     
*                                                                               
GENDPTOT MVC   DPTLENSV,SBDPTLEN                                                
         TM    DATAIND,DIDPT       CHECK FOR DAYPART                            
         BZ    DTX                                                              
         OI    ININD,INIEQUIV+INIDPTOT                                          
         LLC   RE,INDPTLEV         SET MAX TOTAL LEVEL                          
         LA    RE,1(RE)                                                         
         STC   RE,MAXTOTLV                                                      
         TM    DATAIND,DIDPTLEN    IF DAYPART/LENGTH,                           
         BNO   DT02                                                             
         MVI   SBLEN,FF            DAYPART TOTAL                                
         BAS   RE,LDRIVIN                                                       
*                                                                               
DT02     MVC   SBDPT,EFFS                                                       
         TM    SBQDPTLN,SBQDLSGR   TEST SUPPRESS DPT GROUP TOTALS               
         BO    DT04                                                             
         OC    SBDPTGRP+1(3),SBDPTGRP+1   TEST FOR DAYPART GROUP                
         BZ    DT04                                                             
         BAS   RE,LDRIVIN          YES - PUT GROUP TOTAL                        
*                                                                               
DT04     TM    DATAIND,DIDPTLEN    TEST DAYPART/LENGTH                          
         BNO   DTX                 NO - FINISHED WITH TOTALS                    
         TM    SBQDPTLN,SBQDLSUP   TEST CROSS DAYPARTS SUPPRESSED               
         BO    DTX                                                              
         MVC   SBDPTGRP,EFFS                                                    
         TM    SBQDPTLN,SBQDLTOT   TEST FOR SPOT LENGTH TOTALS                  
         BZ    DT10                                                             
         MVC   SBLEN,DPTLENSV+SBLEN-SBDPTLEN   YES                              
         BAS   RE,LDRIVIN                                                       
         MVI   SBLEN,FF                                                         
DT10     BAS   RE,LDRIVIN          ALL TOTAL                                    
*                                                                               
DTX      MVC   SBDPTLEN,DPTLENSV   RESTORE DAYPART/LENGTH                       
         MVI   MAXTOTLV,0                                                       
         NI    ININD,FF-INIDPTOT   TURN OFF DAYPART TOTALING IND                
         B     XIT                                                              
         EJECT                                                                  
* HEADHOOK                                                                      
*                                                                               
HEADHK   TM    DATAIND,DIMED       TEST MEDIA ALREADY IN HEADLINE               
         BO    HEADHK2                                                          
         L     R1,AH4              NO - PUT IT IN HEAD4                         
         LA    R1,1(R1)                                                         
         MVC   0(5,R1),=C'MEDIA'                                                
         LA    R1,CODEAREA-LABLAREA(R1)                                         
         MVC   0(1,R1),SBQMED                                                   
         CLI   SBQMED,C'*'         TEST MEDIA = T AND R                         
         BE    HEADHK2             YES-LEAVE NAME BLANK                         
         LA    R1,NAMEAREA-CODEAREA(R1)                                         
         MVC   0(L'SBMEDNM,R1),SBMEDNM                                          
         LA    RF,SYSD                                                          
         AHI   RF,(SBCMEDNM-SYSD)                                               
         CLC   0(L'SBCMEDNM,RF),BLANKS   TEST CLIENT HAS A MEDIA NAME           
         BNH   HEADHK2                                                          
         MVC   0(L'SBCMEDNM,R1),0(RF)    YES-USE THAT                           
*                                                                               
HEADHK2  GOTO1 GENHEAD             LINE UP HEADLINES                            
*                                                                               
         L     R2,AH4                                                           
         A     R2,PWIDTH           R2=A(HEADLINE 5)                             
         LA    R4,96               R4=DISPLACEMENT TO RHS HEADLINES             
         CLI   WIDTHOPT,C'W'                                                    
         BNE   *+8                                                              
         LA    R4,128                                                           
         TM    DATAIND,DIBYDEM     TEST BUY DEMOS                               
         BZ    HEADHK10                                                         
         LA    R3,0(R4,R2)                                                      
         TM    DATAIND,DIDEMR+DIDEMA    YES - TEST JUST PURCHASED               
         BNZ   *+14                                                             
         MVC   0(19,R3),=C'* PURCHASED DEMOS *'                                 
         B     HEADHK2C                                                         
         CLI   SBCEXTRA+12,C'Y'                                                 
         BNE   HEADHK2D                                                         
         MVC   0(4,R3),=C'DF=N'                                                 
         CLI   SBDFPROF,C'*'                                                    
         BE    HEADHK2B                                                         
         LA    R1,SBDFPROF                                                      
         LA    R0,L'SBDFPROF                                                    
*                                                                               
HEADHK2A CLI   0(R1),0                                                          
         BE    HEADHK2C                                                         
         CLC   SBDPTMEN,0(R1)                                                   
         BE    HEADHK2B                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,HEADHK2A                                                      
         B     HEADHK2C                                                         
*                                                                               
HEADHK2B MVI   3(R3),C'Y'                                                       
*                                                                               
HEADHK2C A     R2,PWIDTH                                                        
         LA    R3,0(R4,R2)                                                      
*                                                                               
HEADHK2D MVC   0(09,R3),=C'RTG SRC -'                                           
         LR    RF,R9               RF=A(SBEFLAG2)                               
         AHI   RF,SBEFLAG2-SYSD                                                 
         TM    0(RF),SBEWTP        SHOW USING WTP'S                             
         BZ    *+12                                                             
         MVI   10(R3),C'W'                                                      
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   10(22,R3),=CL22'(C) 20XX NIELSEN MEDIA'                          
         GOTO1 DATCON,DMCB,(5,0),(20,DUB)                                       
         MVC   14(4,R3),DUB                                                     
         CLI   SBCPROF+3,C'0'                                                   
         BE    *+10                                                             
***      MVC   10(22,R3),=CL22'ARB'                                             
         MVC   10(22,R3),=CL22'NAM'                                             
         TM    OPTIND3,OPTIXSPL    TEST CANADIAN SPILL LOOKUP                   
         BO    *+16                                                             
         LA    R1,SBAGYREC                                                      
         USING AGYHDR,R1                                                        
         CLI   AGYPROF+7,C'C'                                                   
         BNE   HEADHK3                                                          
         DROP  R1                                                               
         MVC   10(22,R3),=CL22'CSI'                                             
         CLI   SBCPROF+3,C'0'                                                   
         BE    HEADHK3                                                          
         MVC   10(22,R3),=CL22'BBM'                                             
*                                                                               
HEADHK3  CLI   SBQBKTYP,0                                                       
         BE    HEADHK3A                                                         
         AHI   R3,13                                                            
         CLI   1(R3),C' '          IS THIS NIELSEN?                             
         BNH   *+8                  NO                                          
         AHI   R3,19                YES - SKIP THE DISCALIMER                   
         MVI   0(R3),C'('                                                       
         AHI   R3,1                                                             
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
HEADHK3L CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SBQBKTYP,SPBKTYPN                                                
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     HEADHK3L                                                         
*                                                                               
         MVC   0(2,R3),SPBKTYPA    BOOK TYPE                                    
         DROP  RF                                                               
*                                                                               
         AHI   R3,1                                                             
         CLI   0(R3),C' '                                                       
         BE    *+8                                                              
         AHI   R3,1                                                             
         MVI   0(R3),C')'                                                       
*                                                                               
HEADHK3A A     R2,PWIDTH                                                        
         LA    R3,0(R4,R2)                                                      
*                                                                               
         TM    DATAIND,DIDEMR                                                   
         BZ    HEADHK4                                                          
         MVC   0(11,R3),=C'ACHIEVED ON'                                         
         LA    R3,12(R3)                                                        
         B     HEADHK5                                                          
*                                                                               
HEADHK4  TM    DATAIND,DIDEMA                                                   
         BZ    HEADHK10                                                         
         MVC   0(8,R3),=C'AFFID ON'                                             
         LA    R3,9(R3)                                                         
*                                                                               
HEADHK5  LR    R1,R9                                                            
         AHI   R1,SBQBOOK-SYSD                                                  
         MVC   BKS,0(R1)        UP TO 3 BOOKS                                   
         LA    R0,6                                                             
         LA    R5,BKS                                                           
*                                                                               
HEADHK6  MVC   7(4,R3),=C'BOOK'                                                 
         CLC   0(3,R5),=C'ACT'                                                  
         BNE   HEADHK7                                                          
         MVC   0(6,R3),=C'ACTUAL'                                               
         TM    3(R5),X'80'         TEST ACT/YY INDICATOR                        
         BZ    HEADHK8                                                          
         MVI   3(R3),C'/'                                                       
         LLC   RE,3(R5)                                                         
         SHI   RE,128                                                           
*Y2K         CVD   RE,DUB                                                       
*Y2K         OI    DUB+7,X'0F'                                                  
*Y2K         UNPK  4(2,R3),DUB                                                  
*                                                                               
         STC   RE,FULL             MOVE YEAR TO FULL                            
         MVC   FULL+1(2),=X'0101'  DUMMY UP MMDD                                
         GOTO1 DATCON,DMCB,(3,FULL),(X'20',DUB) CONVERT TO EBCDIC               
         MVC   4(2,R3),DUB         MOVE OUT YEAR                                
         B     HEADHK8                                                          
*                                                                               
HEADHK7  MVC   DUB(4),0(R5)                                                     
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(6,(R3))                                     
*                                                                               
HEADHK8  CLI   SBESVI,FF           TEST SVI=NO                                  
         BE    HEADHK9                                                          
         MVI   12(R3),C'('         NO-                                          
         CLI   SBESVI,0            TEST SVI=AUTO                                
         BNE   *+14                                                             
         MVC   13(5,R3),=C'AUTO)'                                               
         B     HEADHK9                                                          
         MVI   FULL,X'57'          SVI=MONTH                                    
         MVC   FULL+1(1),SBESVI                                                 
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(4,DUB)                                     
         MVC   13(3,R3),DUB                                                     
         MVC   17(4,R3),=C'ADJ)'                                                
*                                                                               
HEADHK9  A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
         LA    R5,4(R5)            TRY NEXT BOOK                                
         CLC   0(4,R5),BLANKS                                                   
         BNH   HEADHK10                                                         
         BCT   R0,HEADHK6                                                       
*                                                                               
HEADHK10 TM    DATAIND,DICPP       TEST CPP                                     
         BZ    HEADHK20                                                         
         OC    EQBASE,EQBASE       YES-TEST EQUIV BASE                          
         BZ    HEADHK20                                                         
         LA    R3,0(R4,R2)                                                      
         MVC   0(29,R3),=C'EQUIVALENCE BASE = 29SEC. (+)'                       
         MVC   19(2,R3),EQBASE                                                  
         A     R2,PWIDTH                                                        
*                                                                               
HEADHK20 LA    R3,0(R2,R4)                                                      
         CLC   SBQSTA(3),=C'ALL'   TEST ALL CABLE REQUEST                       
         BNE   HEADHK24                                                         
         CLI   SBQSTA+4,C'/'                                                    
         BNE   HEADHK22                                                         
         MVC   0(7,R3),=C'* CABLE' YES-                                         
         CLC   SBQCNET,BLANKS      TEST CABLE NETWORK FILTER                    
         BNH   *+20                                                             
         MVC   8(3,R3),=C'NET'     YES                                          
         MVC   12(3,R3),SBQCNET                                                 
         LA    R3,8(R3)                                                         
         MVC   8(6,R3),=C'ONLY *'                                               
         B     HEADHK25                                                         
*                                                                               
HEADHK22 CLI   SBQSTA+4,C'-'       TEST ALL EXCEPT CABLE                        
         BNE   HEADHK25                                                         
         MVC   0(18,R3),=C'* CABLE EXCLUDED *'                                  
         B     HEADHK25                                                         
*                                                                               
HEADHK24 CLI   SBQSTA,C'0'         TEST CABLE REQUEST                           
         BL    HEADHK26                                                         
         CLC   SBQCNET,BLANKS      YES-TEST NETWORK FILTER                      
         BH    HEADHK26                                                         
         MVC   0(15,R3),=C'* CABLE HEADEND' NO-                                 
         MVC   16(4,R3),SBQSTA                                                  
         MVC   21(6,R3),=C'ONLY *'                                              
*                                                                               
HEADHK25 DS    0H                                                               
*                                                                               
         A     R2,PWIDTH           BUMP TO NEXT PRINT LINE                      
         LA    R3,0(R4,R2)         POINT TO START OF RHS HEADLINES              
*                                                                               
HEADHK26 DS    0H                                                               
*                                                                               
*        PRINT SQAD COPYRIGHT MESSAGE                                           
*                                                                               
         LA    R4,SBLOCK           ESTABLISH SBLOCK                             
         USING SBLOCK,R4                                                        
*                                                                               
         OC    SBSQDQT1(SBSQDOPL),SBSQDQT1     SKIP IF NO SQAD DATA             
         BZ    HEADHK30                                                         
*                                                                               
         MVC   0(33,R3),=C'COPYRIGHT SQAD:                  ' BASE MSG          
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),SBSQDRL1    MOVE SQAD RELEASE TO WORKAREA                
*                                                                               
         GOTO1 DATCON,DMCB,(3,FULL),(6,16(R3))  PRINT YEAR/MONTH                
*                                                                               
         MVC   FULL(2),SBSQDRL2    MOVE SECOND SQAD RELEASE TO WORKAREA         
*                                                                               
         OC    FULL,FULL           SKIP IF SECOND OPTION NOT REQUESTED          
         BZ    HEADHK27                                                         
*                                                                               
         MVI   23(R3),C'&&'                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,FULL),(6,25(R3))  PRINT YEAR/MONTH                
*                                                                               
HEADHK27 DS    0H                                                               
*                                                                               
         A     R2,PWIDTH           BUMP TO NEXT PRINT LINE                      
         LA    R3,0(R4,R2)         POINT TO START OF RHS HEADLINES              
*                                                                               
HEADHK30 DS    0H                                                               
*                                                                               
HEADHKX  B     XIT                                                              
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
* ROUTINE TO SET A(DATE TABLE) AND NUMBER OF DATES IN TABLE                     
*                                                                               
DATES    LA    R1,SBBQSTP                                                       
         ST    R1,SBADATE                                                       
         MVC   SBNDATES,=F'1'                                                   
         TM    SBQPER,255-SBQPFL   TEST PERIOD BREAKOUT REQUIRED                
         BZ    DATX                (OTHER THAN CHILD SPOT FLIGHTS)              
         CLI   SBQPERLO,0          YES-TEST OVERALL PERIOD RANGE GIVEN          
         BNE   DAT12               YES                                          
         XC    FULL,FULL           NO-FIND LOWEST PERIOD START                  
         MVC   FULL(2),EFFS           AND HIGHEST PERIOD END                    
         LA    R3,PERTAB                                                        
*                                                                               
DAT2     SR    R5,R5                                                            
         ICM   R5,1,0(R3)                                                       
         BZ    DAT6                                                             
         EX    R5,*+8                                                           
         B     *+8                                                              
         TM    SBQPER,0                                                         
         BZ    DAT4                                                             
         LH    R1,1(R3)                                                         
         LA    R1,SYSD(R1)                                                      
         L     R1,0(R1)                                                         
         LH    R2,3(R3)                                                         
         LA    R2,SYSD(R2)                                                      
         BAS   RE,SETHILO                                                       
         BE    DAT4                                                             
         LA    RE,255                                                           
         SR    RE,R5                                                            
         EX    RE,*+8                                                           
         B     DAT4                                                             
         NI    SBQPER,0                                                         
*                                                                               
DAT4     LA    R3,5(R3)                                                         
         B     DAT2                                                             
*                                                                               
DAT6     LA    RE,1                SET A(FIRST DATE IN PERIOD)                  
         LA    R0,NDAYS            AND N'DATES IN PERIOD                        
         L     R1,ADAYS                                                         
         XC    SBADATE,SBADATE                                                  
         TM    SBQPER,SBQPDY                                                    
         BO    DAT8                                                             
         LA    R0,NWEEKS                                                        
         L     R1,AWEEKS                                                        
         TM    SBQPER,SBQPWK                                                    
         BO    DAT8                                                             
         LA    R0,NMONTHS                                                       
         L     R1,AMONTHS                                                       
         TM    SBQPER,SBQPMN                                                    
         BO    DAT8                                                             
         LA    R0,NQTRS                                                         
         L     R1,AQTRS                                                         
         TM    SBQPER,SBQPQT                                                    
         BO    DAT8                                                             
         LA    R0,NYEARS                                                        
         L     R1,AYEARS                                                        
         TM    SBQPER,SBQPYR                                                    
         BO    DAT8                                                             
         LA    R0,NHYEARS                                                       
         L     R1,AHYEARS                                                       
         TM    SBQPER,SBQPHY                                                    
         BZ    DATX                                                             
*                                                                               
DAT8     OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   FULL(2),0(R1)                                                    
         BH    DAT10                                                            
         BL    *+8                                                              
         ST    R1,SBADATE                                                       
         CLC   FULL+2(2),2(R1)                                                  
         BH    DAT10                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    RE,SBNDATES                                                      
         OC    SBADATE,SBADATE                                                  
         BNZ   DATX                                                             
         DC    H'0'                                                             
*                                                                               
DAT10    LA    R1,4(R1)                                                         
         OC    SBADATE,SBADATE                                                  
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
         BCT   R0,DAT8                                                          
         DC    H'0'                                                             
*                                                                               
DAT12    LLC   RE,SBQPERLO                                                      
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    R0,NDAYS                                                         
         L     R1,ADAYS                                                         
         TM    SBQPER,SBQPDY       TEST DAYS REQUIRED                           
         BO    DAT14                                                            
         LA    R0,NWEEKS                                                        
         L     R1,AWEEKS                                                        
         TM    SBQPER,SBQPWK       TEST WEEKS REQUIRED                          
         BO    DAT14                                                            
         LA    R0,NMONTHS                                                       
         L     R1,AMONTHS                                                       
         TM    SBQPER,SBQPMN       TEST MONTHS REQUIRED                         
         BO    DAT14                                                            
         LA    R0,NQTRS                                                         
         L     R1,AQTRS                                                         
         TM    SBQPER,SBQPQT       TEST QUARTERS REQUIRED                       
         BO    DAT14                                                            
         LA    R0,NHYEARS                                                       
         L     R1,AHYEARS                                                       
         TM    SBQPER,SBQPHY       TEST HALF YEARS REQUIRED                     
         BO    DAT14                                                            
         LA    R0,NYEARS                                                        
         L     R1,AYEARS                                                        
         TM    SBQPER,SBQPYR       TEST YEARS REQUIRED                          
         BO    DAT14                                                            
         DC    H'0'                                                             
*                                                                               
DAT14    LA    RF,0(RE,R1)         POINT TO APPROPRIATE DATE LIST               
         ST    RF,SBADATE          SET A(DATE LIST)                             
         OC    0(4,RF),0(RF)       TEST ANY DATES                               
         BNZ   *+6                                                              
         DC    H'0'                NO-CAN'T GO ANY FURTHER                      
         CLI   SBQPERHI,FF         TEST PERIOD IN THE ROWS                      
         BE    DAT16               YES                                          
         SR    R2,R2                                                            
         ICM   R2,1,SBQPERHI       NO-SET NUMBER OF DATES                       
         BNZ   *+6                                                              
         DC    H'0'                (HIGH HAD BETTER BE SET)                     
         BCTR  R2,0                                                             
         SLL   R2,2                                                             
         AR    R2,R1                                                            
         OC    0(4,R2),0(R2)       TEST HIGHEST PERIOD IS BEYOND                
         BZ    DAT16               REQUEST PERIOD END                           
*                                                                               
         LLC   RE,SBQPERLO         NO                                           
         LLC   RF,SBQPERHI                                                      
         SR    RF,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RF,1(RF)                                                         
         ST    RF,SBNDATES                                                      
         B     DATX                                                             
*                                                                               
DAT16    SR    RE,RE               NUMBER OF DATES IS THE TOTAL                 
*                                                                               
DAT18    OC    0(4,RF),0(RF)        NUMBER IN THE PERIOD                        
         BZ    DAT20                                                            
         LA    RF,4(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,DAT18                                                         
*                                                                               
DAT20    ST    RE,SBNDATES                                                      
*                                                                               
DATX     B     XIT                                                              
         SPACE 2                                                                
PERTAB   DC    AL1(SBQPDY),AL2(ADAYS-SYSD),AL2(DAYLO-SYSD)                      
         DC    AL1(SBQPWK),AL2(AWEEKS-SYSD),AL2(WEKLO-SYSD)                     
         DC    AL1(SBQPMN),AL2(AMONTHS-SYSD),AL2(MONLO-SYSD)                    
         DC    AL1(SBQPQT),AL2(AQTRS-SYSD),AL2(QTRLO-SYSD)                      
         DC    AL1(SBQPHY),AL2(AHYEARS-SYSD),AL2(HYEARLO-SYSD)                  
         DC    AL1(SBQPYR),AL2(AYEARS-SYSD),AL2(YEARLO-SYSD)                    
         DC    AL1(0)                                                           
         SPACE 2                                                                
SETHILO  LR    R0,RE               SET LOWEST PERIOD START                      
         SR    RF,RF               AND HIGHEST PERIOD END                       
         ICM   RF,1,0(R2)          TEST PERIOD LOW GIVEN                        
         BZ    SETHILO2                                                         
         BCTR  RF,0                YES-FIND THE START DATE                      
         SLL   RF,2                                                             
         LA    RF,0(R1,RF)                                                      
         OC    0(4,RF),0(RF)       TEST ANY DATES HERE                          
         BZ    SETHILO9            NO-DROP THIS PERIOD BREAKOUT                 
         CLC   0(2,RF),FULL        YES-UPDATE LOWEST PERIOD START               
         BNL   SETHILO2                                                         
         MVC   FULL(2),0(RF)                                                    
*                                                                               
SETHILO2 SR    RE,RE                                                            
         ICM   RE,1,1(R2)          TEST PERIOD HIGH GIVEN                       
         BZ    SETHILOX                                                         
         BCTR  RE,0                YES-FIND THE END DATE                        
         SLL   RE,2                                                             
         LA    RE,0(R1,RE)                                                      
         OC    0(4,RE),0(RE)       TEST ANY DATES HERE                          
         BNZ   SETHILO4                                                         
         LTR   RF,RF               NO-BEYOND PERIOD END                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SHI   RE,4                BACK UP TO LAST ONE WITHIN PERIOD            
         CR    RE,RF                                                            
         BNL   *+6                                                              
         DC    H'0'                                                             
         OC    0(4,RE),0(RE)                                                    
         BZ    *-18                                                             
*                                                                               
SETHILO4 CLC   2(2,RE),FULL+2      UPDATE HIGHEST PERIOD END                    
         BNH   SETHILOX                                                         
         MVC   FULL+2(2),2(RE)                                                  
         B     SETHILOX                                                         
*                                                                               
SETHILO9 LTR   RE,R0               CC NE - DROP THIS PERIOD BREAKOUT            
         BR    RE                                                               
*                                                                               
SETHILOX LR    RE,R0               CC EQ - OK                                   
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* GET CHILD SPOTS FLIGHT RECORDS                                                
*                                                                               
GETFLTS  LA    R4,SBQSTART         GET THE FLIGHT YEARS                         
         LA    R5,HALF             HALF=START YEAR                              
         LA    R0,2                HALF+1=END YEAR                              
*                                                                               
FLT2     GOTO1 GETBROD2,DMCB,(1,(R4)),WORK,GETDAY,ADDAY                         
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,(R1),WORK+6,(3,FULL)                                      
         LLC   RF,FULL                                                          
         CLC   FULL+1(1),SBSPPROF+6   TEST MONTH LESS THAN FISCAL YEAR          
         BNL   FLT4                   START                                     
         LTR   RF,RF               YES-SUBTRACT ONE FOR FISCAL YEAR             
         BNZ   *+12                                                             
         LA    RF,99                                                            
         B     FLT4                                                             
         BCTR  RF,0                                                             
*                                                                               
FLT4     STC   RF,0(R5)                                                         
         LA    R5,1(R5)                                                         
         LA    R4,SBQEND                                                        
         BCT   R0,FLT2                                                          
*                                                                               
         L     R4,AWEEKS           PUT FLIGHT DATES IN WEEKS AREA               
         ST    R4,SBADATE                                                       
         SR    R5,R5                                                            
         ST    R5,SBNDATES                                                      
         XC    KEY,KEY             READ FLIGHT RECORDS                          
         LA    R2,KEY                                                           
         USING FLTMENUD,R2                                                      
         MVC   FLTTYP,=X'0D0D'                                                  
         MVC   FLTAGMD,SBBAGYMD                                                 
         MVC   FLTCLT,SBBCLT                                                    
         CLI   SBQBPRD,0           TEST ALL PRODUCTS                            
         BE    FLT6                                                             
         MVC   FLTPRD,SBQPRD                                                    
*                                                                               
FLT6     GOTO1 HIGH                                                             
*                                                                               
FLT7     CLC   KEY(FLTPRD-FLTREC),KEYSAVE   TEST END OF FILE                    
         BNE   FLT18                                                            
         CLC   KEY(FLTYR-FLTREC),KEYSAVE    TEST PRODUCT CHANGE                 
         BE    FLT8                                                             
         CLI   SBQBPRD,0           YES-TEST ALL PRODUCTS                        
         BNE   FLT18               NO-DONE                                      
         LTR   R5,R5               TEST ANY DATES FOR LAST PRODUCT              
         BZ    FLT8                                                             
         ST    R5,SBNDATES         YES-PUT FLIGHT ELEMENT TO NAMES              
         GOTO1 PUTCSFLT                BUFFER                                   
         L     R4,AWEEKS           RESET DATE LIST                              
         SR    R5,R5                                                            
*                                                                               
FLT8     CLC   FLTYR,HALF          TEST YEAR BEFORE START                       
         BNL   FLT10                                                            
         MVC   FLTYR,HALF          YES-SKIP TO START YEAR                       
         XC    FLTEST(4),FLTEST                                                 
         B     FLT6                                                             
*                                                                               
FLT10    CLC   FLTYR,HALF+1        TEST YEAR AFTER END                          
         BNH   *+14                                                             
         MVC   FLTYR(5),EFFS       YES-SKIP TO NEXT PRODUCT                     
         B     FLT6                                                             
         MVC   SBPRD,FLTPRD        SAVE THE PRODUCT                             
         L     R2,SBAIO3                                                        
         ST    R2,AIO              GET THE RECORD                               
         GOTO1 GETREC                                                           
         LA    R3,24(R2)                                                        
         SR    R0,R0               GET FLIGHT DATE ELEMENTS                     
*                                                                               
FLT12    CLI   0(R3),0                                                          
         BE    FLT16                                                            
         CLI   0(R3),5                                                          
         BNE   FLT14                                                            
         USING FLTEL05,R3                                                       
         CLC   FLTST,SBBQENDP      TEST START AFTER REQUEST END                 
         BH    FLT14                                                            
         CLC   FLTEND,SBBQSTP      TEST END BEFORE REQUEST START                
         BL    FLT14                                                            
         MVC   0(2,R4),FLTST       NO-SAVE THE FLIGHT DATES                     
         MVC   2(2,R4),FLTEND                                                   
         CLC   FLTST,SBBQSTP       TRIM TO REQUEST START AND END                
         BNL   *+10                                                             
         MVC   0(2,R4),SBBQSTP                                                  
         CLC   FLTEND,SBBQENDP                                                  
         BNH   *+10                                                             
         MVC   0(2,R4),SBBQENDP                                                 
         LA    R5,1(R5)            INCREMENT N'FLIGHTS                          
         LA    R4,4(R4)                                                         
*                                                                               
FLT14    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FLT12                                                            
*                                                                               
FLT16    GOTO1 SEQ                 GET NEXT RECORD                              
         LA    R2,KEY                                                           
         B     FLT7                                                             
*                                                                               
FLT18    LTR   R5,R5               TEST ANY DATES                               
         BZ    FLTX                                                             
         ST    R5,SBNDATES         YES                                          
         CLI   SBQBPRD,0           TEST SINGLE PRODUCT REQUEST                  
         BNE   FLTX                YES-LEAVE DATES IN WEEKS AREA                
         GOTO1 PUTCSFLT            NO-PUT DATES TO NAMES BUFFER                 
*                                                                               
FLTX     B     XIT                                                              
         EJECT                                                                  
* ESTIMATE FIRST ROUTINE                                                        
* EXECUTED ON ESTIMATE FIRST FOR DRIVER OUTPUT                                  
*                                                                               
ESTFST   DS    0H                                                               
*        CLI   SBQMKTWT,C'D'       TEST PRIMARY DEMO MKT WEIGHTING              
*        BE    *+12                                                             
*        TM    DATAIND4,DIDEMHED   OR DEMO NAMES NEEDED IN HEADS                
*        BZ    FEX                                                              
         ICM   R2,15,AFIELD                                                     
         BZ    FEX                                                              
         MVC   SBBEST,1(R2)        PICK ESTIMATE UP FROM RECORD                 
         CLI   BYTE,QESTNM                                                      
         BNE   *+10                                                             
         MVC   SBBEST,9(R2)                                                     
         CLC   0(2,R2),EFFS        TEST FOR ALL ESTIMATES RECORD                
         BNE   FE2                                                              
         CLC   SBQPRD,=C'ALL'      TEST PRD NE ALL AND SINGLE ESTIMATE          
         BE    FEX                                                              
         CLC   SBQEST,SBQESTND                                                  
         BNE   FEX                                                              
         MVC   SBBEST,SBQEST       YES-CAN GET AWAY WITH USING REQUEST          
*                                      ESTIMATE                                 
FE2      CLI   SBQSEPES,C'Y'       TEST SEPARATE ESTIMATES                      
         BE    FE10                                                             
         CLI   SBQMKTWT,C'D'                                                    
         BNE   *+14                                                             
         OC    PRMYDEMO,PRMYDEMO                                                
         BZ    FE10                                                             
*        TM    DATAIND4,DIDEMHED                                                
*        BZ    FEX                                                              
         OC    DEMNAMES,DEMNAMES                                                
         BNZ   FEX                                                              
*                                                                               
FE10     OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU OR DEMO OPTION SET            
         BZ    FE12                                                             
         LA    R3,SBPDEMOS                                                      
         SR    R4,R4                                                            
         B     FE30                                                             
*                                                                               
FE12     XC    KEY,KEY             NO - READ ESTIMATE HEADER                    
         LA    R2,KEY                                                           
         USING ESTHDRD,R2                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,=C'POL'     POL FOR POL REQUESTS                         
         MVC   EKEYEST,SBBEST                                                   
         CLC   SBQPRD,=C'POL'                                                   
         BE    FE21                                                             
         OC    SBPRD,SBPRD         TEST PRODUCT SET                             
         BNZ   FE20                YES                                          
         CLC   SBQPRD,=C'ALL'      NO-TEST SINGLE PRODUCT                       
         BE    FEX                                                              
         CLI   SBQPRD,C'0'                                                      
         BNL   FEX                                                              
         MVC   SBPRD,SBQPRD        YES-OK, SET THE PRODUCT                      
*                                                                               
FE20     MVC   EKEYPRD,SBPRD                                                    
*                                                                               
FE21     GOTO1 HIGH                                                             
         CLI   SBQSEPES,C'Y'       TEST EST=NO                                  
         BE    *+14                                                             
         CLC   SBQPRD,=C'ALL'      AND PRD=ALL                                  
         BE    FE22                YES-NEED CONTROL ESTIMATE FOR PRD            
         CLC   EKEY,KEYSAVE                                                     
         BNE   FEX                                                              
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         B     FE28                                                             
*                                                                               
FE22     CLC   EKEY(EKEYEST-EKEY),KEYSAVE                                       
         BNE   FEX                                                              
         OC    EKEY+8(5),EKEY+8    TEST ITS AN ESTIMATE RECORD                  
         BZ    *+14                                                             
         MVC   EKEY+8(5),EFFS                                                   
         B     FE21                                                             
         CLC   EKEYEST,SBQESTND                                                 
         BH    FEX                                                              
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         OC    SBQSTART,SBQSTART   YES-NEED CONTROL ESTIMATE FOR PRD            
         BZ    FE23                                                             
         CLC   EEND,SBQSTART           CHECK ESTIMATE DATES                     
         BL    FE27                                                             
         CLC   ESTART,SBQEND                                                    
         BH    FE27                                                             
*                                                                               
FE23     CLC   SBQESFLT,BLANKS     TEST FOR ESTIMATE FILTERING                  
         BNH   FE28                                                             
         LA    R1,3                                                             
         LA    RE,SBQESFLT                                                      
         LA    RF,EPROF                                                         
*                                                                               
FE24     CLI   0(RE),C'*'                                                       
         BE    FE26                                                             
         CLI   0(RE),C' '                                                       
         BE    FE26                                                             
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    FE25                YES                                          
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   FE27                                                             
         B     FE26                                                             
*                                                                               
FE25     MVC   BYTE,0(RE)                                                       
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)          NEGATIVE FILTER MUST NOT MATCH               
         BE    FE27                                                             
*                                                                               
FE26     LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,FE24                                                          
         B     FE28                                                             
*                                                                               
FE27     LA    R2,KEY              SKIP TO NEXT ESTIMATE                        
         MVC   EKEY+8(5),EFFS                                                   
         B     FE21                                                             
*                                                                               
FE28     LA    R3,EDEMLST                                                       
         LA    R4,EUSRNMS                                                       
*                                                                               
FE30     XC    DEMNAMES,DEMNAMES                                                
         GOTO1 AGETDEMS            GET DEMOS                                    
*                                                                               
FEX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* GET DEMO ADJUSTMENT FACTOR                                                    
*                                                                               
         USING BUYREC,R3                                                        
GETDF    CLI   SBCEXTRA+12,C'Y'    TEST CLIENT PROFILE FOR DEMO FACTOR          
         BNE   GETDFX                                                           
         CLI   SBDFPROF,C'*'       TEST DAYPART MENU IN DF PROFILE              
         BE    GETDF4                                                           
         LA    R1,SBDFPROF                                                      
         LA    RE,L'SBDFPROF                                                    
*                                                                               
GETDF2   CLI   0(R1),0                                                          
         BE    GETDFX                                                           
         CLC   SBDPTMEN,0(R1)                                                   
         BE    GETDF4                                                           
         LA    R1,1(R1)                                                         
         BCT   RE,GETDF2                                                        
         B     GETDFX                                                           
*                                                                               
GETDF4   LA    RF,BDELEM           YES-GET BOOK FROM DEMO ELEMENT               
         SR    RE,RE                                                            
*                                                                               
GETDF6   CLI   0(RF),0                                                          
         BE    GETDFX                                                           
         CLI   0(RF),2                                                          
         BE    *+14                                                             
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     GETDF6                                                           
         USING NDELEM,RF                                                        
         LA    R1,NSIMMTAB         TEST MARKET IS METERED                       
         CLI   SBCPROF+3,C'0'      IF NOT NSI, IT'S NOT METERED                 
         BNE   GETDF10                                                          
         LA    RE,SBMKTREC         FIND RATING SERVICE MARKET                   
         USING MKTRECD,RE                                                       
         MVC   HALF,MKTRSM1                                                     
         CLI   MKTRS1,C'0'                                                      
         BE    GETDF8                                                           
         MVC   HALF,MKTRSM2                                                     
         CLI   MKTRS2,C'0'                                                      
         BNE   GETDF10                                                          
         DROP  RE                                                               
*                                                                               
GETDF8   CLI   0(R1),X'FF'                                                      
         BE    GETDF10                                                          
         CLC   HALF,0(R1)                                                       
         BNE   *+18                                                             
         CLC   NDBOOK,2(R1)                                                     
         BL    GETDF10                                                          
         B     GETDFX              MARKET IS METERED                            
         LA    R1,4(R1)                                                         
         B     GETDF8                                                           
*                                                                               
GETDF10  LA    R1,DEMFACTB                                                      
*                                                                               
GETDF12  CLI   0(R1),X'FF'                                                      
         BE    GETDFX                                                           
         CLC   SBAGY,0(R1)                                                      
         BNE   GETDF14                                                          
         CLC   BDDAYPT,2(R1)                                                    
         BNE   GETDF14                                                          
         CLC   NDBOOK,5(R1)                                                     
         BNL   GETDF16                                                          
*                                                                               
GETDF14  LA    R1,L'DEMFACTB(R1)                                                
         B     GETDF12                                                          
*                                                                               
GETDF16  MVC   SVDEMFAC,4(R1)                                                   
*                                                                               
GETDFX   B     XIT                                                              
         DROP  R3,RF                                                            
         EJECT                                                                  
DEMFACTB DS    0CL7                                                             
         DC    C'SJ',C'1',AL2(200),AL2(0)                                       
         DC    C'SJ',C'2',AL2(140),AL2(0)                                       
         DC    C'SJ',C'3',AL2(115),AL2(0)                                       
         DC    C'OM',C'1',AL2(200),AL2(0)                                       
         DC    C'OM',C'2',AL2(140),AL2(0)                                       
         DC    C'OM',C'3',AL2(115),AL2(0)                                       
         DC    C'YN',C'1',AL2(200),AL2(0)                                       
         DC    C'YN',C'2',AL2(140),AL2(0)                                       
         DC    C'YN',C'3',AL2(115),AL2(0)                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
NSIMMTAB DC    AL2(124),AL1(88,10) ATLANTA                                      
         DC    AL2(106),AL1(88,10) BOSTON                                       
         DC    AL2(202),AL1(88,10) CHICAGO                                      
         DC    AL2(115),AL1(89,11) CINCINNATI                                   
         DC    AL2(223),AL1(88,10) DALLAS/FT. WORTH                             
         DC    AL2(351),AL1(88,10) DENVER                                       
         DC    AL2(105),AL1(88,10) DETROIT                                      
         DC    AL2(133),AL1(90,01) HARTFORD/NEW HAVEN                           
         DC    AL2(218),AL1(88,10) HOUSTON                                      
         DC    AL2(127),AL1(89,10) INDIANAPOLIS                                 
         DC    AL2(403),AL1(88,10) LOS ANGELES                                  
         DC    AL2(128),AL1(88,10) MIAMII/FT. LAUDERDALE                        
         DC    AL2(217),AL1(89,08) MILWAUKEE                                    
         DC    AL2(213),AL1(89,08) MINNEAPOLIS/ST. PAUL                         
         DC    AL2(101),AL1(88,10) NEW YORK                                     
         DC    AL2(104),AL1(88,10) PHILADELPHIA                                 
         DC    AL2(353),AL1(89,09) PHOENIX                                      
         DC    AL2(420),AL1(90,05) PROTLAND, OR                                 
         DC    AL2(462),AL1(89,08) SACRAMENTO-STOCKTON                          
         DC    AL2(241),AL1(90,11) SAN ANTONIO                                  
         DC    AL2(407),AL1(88,10) SAN FRANCISCO                                
         DC    AL2(419),AL1(89,08) SEATTLE-TACOMA                               
         DC    AL2(209),AL1(89,08) ST LOUIS                                     
         DC    AL2(139),AL1(89,11) TAMPA-ST. PETERSBURG                         
         DC    AL2(111),AL1(88,10) WASHINGTON, DC                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
* ADD DUMMY SPILL ELEMENT TO BUY RECORD FOR CANADIAN SPILL LOOKUP               
* INPUT  : R1=A(RATING SERVICE MARKET)                                          
*          SBEMKT=AGENCY MARKET                                                 
*                                                                               
         SPACE 1                                                                
SPILELEM MVC   RSVCMKT,0(R1)                                                    
         USING NDELEM,R2                                                        
         USING BUYREC,R3                                                        
         ICM   R2,15,AXSELEM       TEST ELEMENT ALREADY ADDED                   
         BNZ   SPEL6                                                            
         LA    R2,BDELEM           NO-SCAN RECORD FOR DEMO ELEMENTS             
         SR    R0,R0                                                            
         SR    RF,RF                                                            
*                                                                               
SPEL2    CLI   0(R2),0                                                          
         BE    SPEL4                                                            
         CLI   0(R2),2             TEST DEMO ELEMENT                            
         BNE   *+10                                                             
         LR    RF,R2               YES-SAVE IT'S ADDRESS                        
         B     *+12                                                             
         CLI   0(R2),3             TEST SPILL ELEMENT                           
         BE    SPEL5               YES-WE'LL MODIFY THIS ONE                    
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SPEL2                                                            
*                                                                               
SPEL4    XC    WORK,WORK           NO SPILL ELEMENTS IN RECORD -                
         LA    R2,WORK             BUILD DUMMY SPILL FROM DEMO ELEMENT          
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LLC   RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RF)                                                    
         MVI   NDELEM,3                                                         
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL '),(R3),(R2)  ADD ELEMENT             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,16(R1)           R2=A(NEW SPILL ELEMENT)                      
*                                                                               
SPEL5    ST    R2,AXSELEM          SAVE A(SPILL ELEMENT) FOR NEXT TIME          
         XC    NDPROG,NDPROG       CLEAR THE PROGRAMMING                        
         LLC   RF,NDLEN            CLEAR ALL DEMO VALUES                        
         SHI   RF,(NDEMNO-NDELEM)                                               
         SRL   RF,3                                                             
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,NDEMNO                                                        
         XC    4(4,R1),4(R1)                                                    
         LA    R1,8(R1)                                                         
         BCT   RF,*-10                                                          
*                                                                               
SPEL6    MVC   NDPROG(2),SBEMKT    AGENCY MARKET NUMBER                         
         CLC   RSVCMKT,EFFS        TEST HOME MARKET FOR CANADA                  
         BE    SPELX               YES-PASS SPILL MARKET 0 TO LOOKUPS           
         MVC   NDPROG+2(2),RSVCMKT RATING SERVICE MARKET NUMBER                 
*                                                                               
SPELX    B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
* ROUTINE TO GENERATE EDICT CONTROL CARDS FOR RECORD=TRANSMIT                   
*                                                                               
         SPACE 1                                                                
GENEDICT L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         LA    R2,KEY              READ DESTINATION ID RECORD                   
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         L     R1,TWAMASTC                                                      
         MVC   CTIKNUM,MCDESTID-MASTD(R1)                                       
         MVC   FILENAME,=CL8'CTFILE'                                            
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         STC   R0,USEIO                                                         
         XC    FILENAME,FILENAME                                                
         CLC   KEY(L'CTIKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         SR    R0,R0               FIND DESCRIPTION ELEMENT                     
         LA    R1,CTIDATA                                                       
*                                                                               
GENED2   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),CTDSCELQ                                                   
         BNE   GENED4                                                           
         CLI   1(R1),12                                                         
         BNE   GENED4                                                           
         MVC   WORK(10),CTDSC-CTDSCD(R1)  EXTRACT DESTINATION ID NAME           
         B     GENED6                                                           
*                                                                               
GENED4   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GENED2                                                           
*                                                                               
GENED6   MVC   P,BLANKS            *HDR* CARD                                   
         LA    R1,P                                                             
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(6,R1),=C'EDICT='                                               
         MVC   15(10,R1),WORK      DESTINATION                                  
         MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVC   38(10,R1),WORK      FORMATTED DESTINATION NAME                   
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,BLANKS            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS SPW  TRN'                                      
         MVC   P+9(2),AGENCY                                                    
         LA    R1,P+15                                                          
         USING SPEDICTD,R1                                                      
         MVI   SPWRTYPE,SPWRDATQ                                                
         MVC   SPWRNAME,AGENCY                                                  
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         DROP  R1                                                               
*                                                                               
         CLC   WRITIT,SPACES       ANY REPORT TITLE?                            
         BNH   GENED8               NO - NO DEFAULT                             
         MVC   P,SPACES            ++DDS CARD FOR BDE                           
         MVC   P(14),=CL14'++DDS      SUB'                                      
         MVC   P+15(34),WRITIT     PROVIDE FORMAT NAME                          
         OC    P+15(34),SPACES                                                  
*                                                                               
         TRT   P+15(34),TRTTABLE   MAKE SURE ALL CHARS ARE ALLOWED              
         BZ    *+16                OKAY                                         
         MVC   P+15(34),SPACES                                                  
         MVC   P+15(8),=CL8'BADID'                                              
*                                                                               
         CLI   P+15,C'0'           FIRST CHAR CAN'T BE NUMERIC                  
         BL    *+8                                                              
         MVI   P+15,C'Z'           REPLACE WITH Z                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
GENED8   MVC   P,SPACES            ++DDS FTP CARD                               
         MVC   P(14),=CL14'++DDS      FTP'                                      
         MVC   P+15(8),WRINAM      PROVIDE FORMAT NAME                          
         CLC   WRINAM,SPACES       IF NO FORMAT NAME                            
         BH    *+16                                                             
         MVC   P+15(3),REMUSER     REPLACE WITH REQUESTOR INITIALS              
         MVC   P+18(5),SPACES      CLEAR END OF FIELD                           
         OC    P+15(8),SPACES      MAKE UPPERCASE                               
*                                                                               
         TRT   P+15(8),TRTTABLE    MAKE SURE ALL CHARS ARE ALLOWED              
         BZ    *+10                OKAY                                         
         MVC   P+15(8),=CL8'BADID'                                              
*                                                                               
         CLI   P+15,C'0'           FIRST CHAR CAN'T BE NUMERIC                  
         BL    *+8                                                              
         MVI   P+15,C'Z'           REPLACE WITH Z                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
GENEDX   B     XIT                                                              
*                                                                               
*        TRTTABLE FOR VALID DATASET NAME CHARACTERS                             
*                                                                               
TRTTABLE DC    256AL1(*-TRTTABLE)                                               
         ORG   TRTTABLE            ALPHANUMERIC ALLOWED                         
         DC    X'FF'                                                            
         ORG   TRTTABLE+C' '       ALPHANUMERIC ALLOWED                         
         DC    X'00'                                                            
         ORG   TRTTABLE+C'A'       ALPHANUMERIC ALLOWED                         
         DC    9X'00'                                                           
         ORG   TRTTABLE+C'J'       ALPHANUMERIC ALLOWED                         
         DC    9X'00'                                                           
         ORG   TRTTABLE+C'S'       ALPHANUMERIC ALLOWED                         
         DC    8X'00'                                                           
         ORG   TRTTABLE+C'0'       ALPHANUMERIC ALLOWED                         
         DC    10X'00'                                                          
         ORG                                                                    
                                                                                
         EJECT                                                                  
SWAPFLES DS    0H                                                               
         XC    SBCLT,SBCLT                                                      
         XC    SBBCLT,SBBCLT                                                    
         ICM   R1,15,SBADPTTB      CLEAR DAYPART TABLES BUFFER                  
         BZ    SWAP10                                                           
         LA    R0,36                                                            
         XC    0(180,R1),0(R1)                                                  
         LA    R1,180(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
SWAP10   DS    0H                                                               
*** NEED TO STICK AN ID # IN TWA+10 FOR VALUSER                                 
         L     R1,ATWA                                                          
*                                                                               
         CLC   AGENCY,=C'NE'                                                    
         BNE   *+20                                                             
         MVC   AGENCY,=C'TR'                                                    
         MVC   10(02,R1),=H'31'    ID # FOR TLDA                                
         B     SWAP20                                                           
*                                                                               
         CLC   AGENCY,=C'TR'                                                    
         BNE   *+20                                                             
         MVC   AGENCY,=C'GB'                                                    
         MVC   10(02,R1),=H'315'   ID # FOR GBNY                                
         B     SWAP20                                                           
*                                                                               
         CLC   AGENCY,=C'GB'                                                    
         BNE   XITNEQ                                                           
         MVC   AGENCY,=C'NE'                                                    
         MVC   10(02,R1),=H'2348'  ID # FOR DNNYS                               
*                                                                               
SWAP20   GOTO1 VALUSER                                                          
*                                                                               
* GET SPOT SE NUMBER FOR NEW AGY                                                
         MVC   FILENAME,=CL8'CTFILE'                                            
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CT5KEY,R2                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         STC   R0,USEIO                                                         
         XC    FILENAME,FILENAME                                                
         L     R2,AIO                                                           
         LA    R2,CT5DATA                                                       
         SR    R0,R0                                                            
         DROP  R2                                                               
*                                                                               
SWAP30   CLI   0(R2),0                                                          
         BE    XITNEQ                                                           
         CLI   0(R2),X'21'         SYSTEM ELEMENT?                              
         BNE   *+12                 NO                                          
         CLI   2(R2),X'02'         SPOT SYSTEM?                                 
         BE    *+14                 YES                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SWAP30                                                           
*                                                                               
         LA    RF,SBLOCK                                                        
         USING SBLOCK,RF                                                        
         ICM   RE,15,SBAUTL                                                     
         MVC   4(1,RE),3(R2)       MOVE SPOT SYS NUMBER                         
         DROP  RF                                                               
*                                                                               
         CLC   AGENCY,=C'NE'       BACK TO START?                               
         BE    XITNEQ               YES - FILES OPEN ALREADY                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLIST,AIO                       
*                                                                               
* GET MKT SVC RTG # FROM MKT REC, READ MTR REC TO GET NEW MKT #                 
         OC    SBQMGRD,SBQMGRD     BY MKT GRP?                                  
         BNZ   XITEQ                YES, IGNORE                                 
         OC    SBQMKT,SBQMKT       ALL MARKETS?                                 
         BZ    XITEQ                YES                                         
         CLC   =C'ALL',SBQMKT                                                   
         BE    XITEQ                YES                                         
*                                                                               
         LA    R2,WRIMEDH          GO GET SBBAGYMD & OTHERS...                  
         GOTO1 VALMED                                                           
*                                                                               
         L     RE,SBAMGTAB                                                      
         L     RF,=F'20000'        CLEAR MARKET GROUP TABLE                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING MTRRECD,R2                                                       
         MVC   MTRKTYPE,=X'0D46'                                                
         MVC   MTRKAM,SBBAGYMD                                                  
         MVI   MTRKRTG,C'N'                                                     
         LA    R1,SBMKTREC                                                      
         USING MKTRECD,R1                                                       
         MVC   MTRKRSMK,MKTRSM1                                                 
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   XITNEQ                                                           
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R2,MTREL05                                                       
         USING MTREL05,R2                                                       
         SR    R0,R0                                                            
         SR    RF,RF                                                            
*                                                                               
SWAP40   CLI   0(R2),0                                                          
         BE    XITEQ                                                            
         CLI   0(R2),5                                                          
         BE    *+14                                                             
SWAP50   IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     SWAP40                                                           
*                                                                               
         ICM   RF,3,MTRAGMK        GET MKT NUM                                  
         AR    RF,RF               X 2                                          
         L     RE,SBAMGTAB                                                      
         AR    RE,RF                                                            
         STCM  RF,3,0(RE)          (TRUE MGRPS, SAVE GRP #, NOT MKT)            
         B     SWAP50                                                           
*                                                                               
FLIST    DC    CL8' SPTFILE'                                                    
         DC    CL8' SPTDIR '                                                    
         DC    CL8' STAFIL '                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
         DS    0H                                                               
         EJECT                                                                  
GETNSIZE DS    0H                                                               
         USING BUYREC,R3                                                        
         LA    R2,BYTE                                                          
         OC    BUYMSTA(2),BUYMSTA  MKT 0 REC?                                   
         BZ    GS10                 YES - USE NET FROM KEY                      
*                                                                               
         L     R3,SBAIO1           A(BUY RECORD)                                
         LA    R1,SBAGYREC         TEST CANADIAN CABLE                          
         CLI   AGYPROF+7-AGYHDRD(R1),C'C'                                       
         BNE   GS2                                                              
         CLI   BUYMSTA+4,X'B0'                                                  
         BNL   GSXX                 YES, SBSIZE IS RIGHT FROM FSTA!             
         CLI   BUYMSTA+4,X'02'                                                  
         BE    GSXX                DITTO FOR TV - MED 'C' USES NETSIZE!         
*                                                                               
GS2      LLC   R1,BUYMSTA+4        GET NET BYTE                                 
         L     R2,ANETSIZE                                                      
         AR    R2,R1                                                            
         CLI   0(R2),0             ANYTHING THERE YET?                          
         BNE   GSX                  YES - USE IT                                
*                                                                               
GS8      CLI   SBMED,C'T'                                                       
         BE    GS10                                                             
*                                                                               
         LA    RE,BDELEM           ELSE, GET THE NETWORK ELEM                   
         SR    R0,R0                                                            
                                                                                
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'68'                                                      
         BE    GS10                                                             
         CLI   0(RE),0                                                          
         BNE   *-18                                                             
         MVI   0(R2),C'?'                                                       
         B     XITEQ                                                            
*                                                                               
GS10     DS    0H                                                               
         LA    R4,KEY              READ STATION RECORD                          
         USING STARECD,R4                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(16),STAKEY                                              
         MVI   STAKTYPE,C'S'                                                    
         CLI   SBQMED,C'*'         MEDIA '*'?                                   
         BNE   *+14                 NO                                          
         MVC   STAKMED,SBMED        YES - USE SBMED                             
         B     *+10                                                             
         MVC   STAKMED,SBQMED                                                   
         MVC   STAKCALL,SBSTA      USE SBSTA                                    
         CLI   SBMED,C'T'           IF THIS IS A MEDIA T REC                    
         BE    GS12                                                             
         OC    BUYMSTA(2),BUYMSTA   OR A MKT 0 REC?                             
         BZ    GS12                                                             
         MVC   STAKCALL,2(RE)                                                   
*                                                                               
GS12     CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         LA    R1,SBQMED                                                        
         CLI   SBQMED,C'*'         MEDIA *?                                     
         BNE   *+8                 NO                                           
         LA    R1,SBMED            YES - USE SBMED                              
         CLI   0(R1),C'C'          TEST REQUEST MEDIA IS C                      
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'C'     YES-STATION'S MEDIA IS C                     
         CLI   0(R1),C'N'          TEST REQUEST MEDIA IS N                      
         BNE   *+8                                                              
         MVI   STAKCALL+4,C'N'     YES-STATION'S MEDIA IS N                     
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,SBCLT                                                    
*                                                                               
         CLI   STAKMED,C'*'        JUST MAKING SURE                             
         BNE   GS20                                                             
         MVI   STAKMED,C'T'                                                     
         CLI   STAKCALL+4,C' '                                                  
         BE    GS20                                                             
         CLI   STAKCALL+4,C'A'                                                  
         BE    *+12                                                             
         CLI   STAKCALL+4,C'F'                                                  
         BNE   *+8                                                              
         MVI   STAKMED,C'R'                                                     
*                                                                               
GS20     LAY   R4,STAIO                                                         
         ST    R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R2),SSIZE                                                    
         CLI   0(R2),C' '                                                       
         BH    GSX                                                              
         MVI   0(R2),C'*'                                                       
*                                                                               
GSX      MVC   SBSIZE,0(R2)                                                     
GSXX     B     XITEQ                                                            
*                                                                               
         EJECT                                                                  
         DS    0F                                                               
ASTANAME DS    A                                                                
RSVCMKT  DS    XL2                                                              
BKS      DS    CL24                                                             
THISSTA  DS    XL3                                                              
MSFORMAT DS    CL3                 MSTREET FORMAT FOR KEY                       
MSOWNER  DS    CL5                 MSTREET OWNER FOR KEY                        
MSPARENT DS    CL5                 MSTREET PARNET FOR KEY                       
MSFREQ   DS    CL5                 MSTREET FREQUENCY                            
MSSTATE  DS    CL2                 MSTREET STATE                                
MSMOWNER DS    CL1                 MSTREET OWNER                                
MSUNIQID DS    CL6                 MSTREET UNIQUE ID                            
MSMOWNF  DS    CL1                 MINORITY OWNERSHIP IS FEMALE                 
MSFCCM   DS    CL1                 QUALIFIED FCC MINORITY                       
MSFCCMF  DS    CL1                 FCC MINORITY OWNERSHIP IS FEMALE             
*                                                                               
EFFS     DC    XL24'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'           
         DC    XL24'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'           
BLANKS   DC    CL132' '                                                         
ASTERS   DC    CL12'************'                                               
*                                                                               
FF       EQU   X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
*********************************************************************           
*                                                                   *           
* CLIENT FIRST ROUTINE                                              *           
* EXECUTED ON CLIENT FIRST FOR DRIVER OUTPUT                        *           
*                                                                   *           
*********************************************************************           
         SPACE 2                                                                
*                                                                               
         USING WORKD,R6                                                         
         USING GEND,RC                                                          
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
CLTFST   NTR1  BASE=*,LABEL=*                                                   
         XC    PRMYDEMO,PRMYDEMO   CLEAR PRIMARY DEMO                           
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU OR DEMO OPTION SET            
         BNZ   *+14                                                             
         XC    DEMNAMES,DEMNAMES   NO - CLEAR DEMO NAMES                        
         B     FC01                                                             
         GOTO1 ADEMOS              YES-GET DEMO INFO IF REQUIRED                
*                                                                               
FC01     XC    SBPRD,SBPRD         CLEAR PRODUCT                                
         MVC   SBBCLT,SBQBCLT                                                   
         CLI   SBQMED,C'*'         MEDIA '*'?                                   
         BE    *+14                 YES - RE-READ CLT REC                       
         OC    SBQBCLT,SBQBCLT     TEST SINGLE CLIENT REQUEST                   
         BNZ   FC01A               STILL NEED UCOM DATA                         
         ICM   R2,15,AFIELD        NO-                                          
         BZ    FCX                                                              
         CLC   0(3,R2),=X'FFFFFF'  TEST ALL CLIENTS                             
         BE    FCX                 YES-EXIT                                     
         MVC   SBCLT,0(R2)         NO                                           
         CLI   BYTE,QCLTNM                                                      
         BNE   *+10                                                             
         MVC   SBCLT,8(R2)                                                      
         GOTO1 CLPACK,DMCB,SBCLT,SBBCLT                                         
*                                                                               
         TM    SBIOFLAG,SBXFILE                                                 
         BNZ   FCX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY              READ CLIENT RECORD                           
         USING CLTHDRD,R3                                                       
         MVC   CKEYAM,SBBAGYMD                                                  
         MVC   CKEYCLT,SBBCLT                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(04),KEYSAVE                                                  
         BNE   FCX                                                              
         L     R3,AIO1                                                          
         ST    R3,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 CLUNPK,DMCB,(CPROF+6,SBBCLT),SBCLT                               
*                                                                               
         MVC   SBCLTNM,CNAME       SET NAME                                     
         MVC   SBCOFF,COFFICE          OFFICE                                   
         MVC   SBCIDTIT,CTITLE         ID TITLE                                 
         MVC   SBCPROF,CPROF           PROFILE                                  
         MVC   SBCEXTRA,CEXTRA         XTRA PROFILE                             
         MVC   SBBCMCLT,CMCLTCOD       TRAFFIC MASTER CLIENT CODE               
         MVC   SBUP1TYP,CPU1TYPE       USER FIELD DEFINITIONS                   
         MVC   SBUP1LEN,CPU1LEN                                                 
         MVC   SBUP2TYP,CPU2TYPE                                                
         MVC   SBUP2LEN,CPU2LEN                                                 
         MVC   SBUE1TYP,CEU1TYPE                                                
         MVC   SBUE1LEN,CEU1LEN                                                 
         MVC   SBUE2TYP,CEU2TYPE                                                
         MVC   SBUE2LEN,CEU2LEN                                                 
         LR    R1,R9                                                            
         AHI   R1,(SBUP1DES-SYSD)                                               
         MVC   0(L'SBUP1DES,R1),CPU1                                            
         LR    R1,R9                                                            
         AHI   R1,(SBUP2DES-SYSD)                                               
         MVC   0(L'SBUP2DES,R1),CPU2                                            
         LR    R1,R9                                                            
         AHI   R1,(SBUE1DES-SYSD)                                               
         MVC   0(L'SBUE1DES,R1),CEU1                                            
         LR    R1,R9                                                            
         AHI   R1,(SBUE2DES-SYSD)                                               
         MVC   0(L'SBUE2DES,R1),CEU2                                            
*                                                                               
FC01A    TM    DATAIND9,DIUCOM     EXTRACT USER COMMENT DATA?                   
         BZ    FC01B                NO                                          
         OC    AUCOMTAB,AUCOMTAB                                                
         BNZ   *+6                                                              
         DCHO                                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING DDUCOMD,RF                                                       
         MVC   UCACOMF,ACOMFACS                                                 
         MVI   UCSYS,C'S'                                                       
         MVC   UCSAM,SBBAGYMD                                                   
         MVC   UCSCLT,SBBCLT                                                    
         MVI   UCOPT,UCOTTL+UCOMKT     GET TITLES ONLY (AND MKT TTLS!)          
         TM    DTAIND11,DIUCOM8    EXTRACT ESTIMATE UCOMM 5-8                   
         BZ    *+8                 NO                                           
         OI    UCOPT,UCO8EST       YES - EST UCOMM 5-8 RETURNED IN PRD          
*                                                                               
         GOTO1 VDDUCOM,ELEM                                                     
         LA    RF,ELEM                                                          
         CLI   UCERROR,0                                                        
         BE    *+6                                                              
         DCHO                                                                   
         TM    UCDATA,UCDNOCLT     NO CLT LEVEL UCOM REC                        
         BNZ   FC01B                                                            
*                                                                               
         L     RE,AUCOMTAB                                                      
         USING UCOMD,RE                                                         
*                                                                               
         MVC   UCDP1LEN(4),UCPMXLNS  GET PRD UCOM LENGTHS                       
         L     R1,UCPTTLS                                                       
         MVC   UCDP1DES(80),0(R1)    AND PRD UCOM TITLES                        
*                                                                               
         MVC   UCDE1LEN(4),UCEMXLNS  GET EST UCOM LENGTHS                       
         L     R1,UCETTLS                                                       
         MVC   UCDE1DES(80),0(R1)    AND EST UCOM TITLES                        
*                                                                               
         MVC   UCDM1LEN(4),UCEMXLNS  GET MKT UCOM LENGTHS                       
         L     R1,UCMTTLS                                                       
         MVC   UCDM1DES(80),0(R1)    AND MKT UCOM TITLES                        
*                                                                               
         DROP  RE,RF                                                            
*                                                                               
FC01B    CLI   SBQMED,C'*'         MEDIA '*'?                                   
         BE    *+14                 YES - RE-READ CLT REC                       
         OC    SBQBCLT,SBQBCLT     TEST SINGLE CLIENT REQUEST                   
         BNZ   FC12                                                             
*                                                                               
         LR    R1,R9                                                            
         AHI   R1,(SBCMEDNM-SYSD)                                               
         MVC   0(L'SBCMEDNM,R1),CMEDNAME   SPECIAL MEDIA NAME                   
         DROP  R3                                                               
         TM    OPTIND,OPTINODF     TEST 'NO DEMO FACTORING' OPTION SET          
         BZ    *+8                                                              
         MVI   SBCEXTRA+12,C'N'    YES-REFLECT IN PROFILE                       
         CLI   SVCOPT,0            TEST RATING SERVICE OVERRIDE                 
         BE    FC02                                                             
         MVC   SBCPROF+3(1),SVCOPT YES-PUT IT IN PROFILE                        
         OI    SBCPROF+3,X'F0'                                                  
         OI    SBINDS,SBIRSOVR     INDICATE THERE'S AN OVERRIDE                 
         TM    SVCOPT,X'80'        TEST OPTION WAS FORCE                        
         BZ    FC02                                                             
         OI    SBINDS,SBIAGYMK     YES-PASS AGENCY MKT NUM TO LOOKUPS           
*                                                                               
FC02     XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S000'    SPOT CONTROL PROFILE                         
         MVC   WORK+4(2),SBAGY                                                  
         CLI   SBQMED,C'*'         MEDIA '*'?                                   
         BNE   *+14                 NO                                          
         MVC   WORK+6(1),SBMED      YES - USE SBMED                             
         B     *+10                                                             
         MVC   WORK+6(1),SBQMED                                                 
         MVC   WORK+7(3),SBCLT                                                  
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,SBSPPROF,DATAMGR                               
*                                                                               
         MVC   WORK(4),=C'S0D0'    GET D0 PROFILE                               
         GOTO1 (RF),(R1),WORK,SBD0PROF,DATAMGR                                  
         MVC   WORK(4),=C'S01W'    GET 1W PROFILE                               
         GOTO1 (RF),(R1),WORK,SB1WPROF,DATAMGR                                  
         CLI   SBEDMA,0            ANY DMA OVERRIDE?                            
         BE    *+10                 NO                                          
         MVC   SB1WPROF+5(1),SBEDMA                                             
         LR    RF,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   RF,4096                                                          
         USING SYSD+4096,RF                                                     
         CLI   SBEWDP,0            ANY WEEKLY DATA FOR POSTS OVERRIDE?          
         BE    *+10                NO                                           
         MVC   SB1WPROF+15(1),SBEWDP                                            
         DROP  RF                  DROP USING                                   
*                                                                               
         XC    DUB,DUB             GET EQUIVALENCE RECORD                       
         MVC   DUB(2),SBAGY                                                     
         MVC   DUB+2(1),SBMED                                                   
         TM    SBIOFLAG,SBXFILE                                                 
         BZ    *+8                                                              
         MVI   DUB+2,C'Z'                                                       
         MVC   DUB+3(2),SBBCLT     PACKED CLIENT CODE                           
         OC    SBBEQCLT,SBBEQCLT   TEST SPECIAL CLIENT FOR EQUIV TABLE          
         BZ    *+10                                                             
         MVC   DUB+3(2),SBBEQCLT   YES-USE THAT CLIENT                          
         GOTO1 EQVRD,DMCB,DUB,SBDPEQTB,AIO2,DATAMGR                             
         CLI   12(R1),0            TEST FOR ERROR                               
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         L     RE,AIO2                                                          
         MVC   SBEQTAB,0(RE)       EXTRACT EQUIVALENCE TABLES                   
*                                                                               
         MVC   SBQMKTWT,WGTOPT                                                  
         CLI   WGTOPT,0            TEST MKT WGT OPTION SET                      
         BE    FC03                                                             
         CLI   WGTOPT,C'Y'         YES                                          
         BE    FC04                                                             
         B     FC06                                                             
*                                                                               
FC03     MVI   SBQMKTWT,C'N'                                                    
         TM    DATAIND4,DIWEIGHT   TEST ANY WEIGHTED COLUMNS                    
         BZ    FC06                                                             
         CLI   SBSPPROF+1,C'N'     TEST CLIENT WANTS MARKET WEIGHTING           
         BE    FC06                                                             
         MVI   SBQMKTWT,C'Y'                                                    
*                                                                               
FC04     CLI   SBSPPROF+14,C'Y'    TEST CLIENT WANTS PRIMARY DEMO               
         BNE   FC06                MARKET WEIGHTING                             
         MVI   SBQMKTWT,C'D'                                                    
*                                                                               
FC06     TM    COLIND,COLIDEM+COLISPT   TEST DEMOS OR SPOTS IN COLS             
         BNZ   *+8                                                              
         MVI   SBQMKTWT,C'N'       NO-WE DON'T NEED MARKET WEIGHTS              
         ICM   R3,15,SBAPRDBF      BUILD PRODUCT BUFFER                         
         USING PRDBUFFD,R3                                                      
         LA    RF,PRDBUFFL         CLEAR                                        
         SLL   RF,8                256 PRODUCTS                                 
         XCEFL (R3),(RF)                                                        
*                                                                               
         L     R4,AIO1             FIND PRODUCTS FROM CLTHDR                    
         LA    R4,CLIST-CLTHDRD-4(R4)                                           
         SR    R5,R5               R5=SEQUENCE NUMBER IN CLIST                  
*                                                                               
FC10     LA    R4,4(R4)                                                         
         LA    R5,1(R5)                                                         
         CLI   0(R4),C' '                                                       
         BNH   FC12                                                             
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PRDHDRD,R2                                                       
         MVC   PKEYAM,SBBAGYMD                                                  
         MVC   PKEYCLT,SBBCLT                                                   
         MVC   PKEYPRD,0(R4)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST PRODUCT RECORD EXISTS                   
         BNE   FC10                NO-NEXT PRODUCT                              
         L     R2,AIO2                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         ICM   R3,15,SBAPRDBF                                                   
         LLC   RE,3(R4)                                                         
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         AR    R3,RE                                                            
         STC   R5,PBSEQ            SAVE SEQUENCE NUMBER                         
         MVC   PBBCODE,3(R4)       PRODUCT CODE                                 
         MVC   PBALPH,0(R4)        PRODUCT ALPHA                                
         MVC   PBNAME,PNAME        PRODUCT NAME                                 
         MVC   PBINT,PACCT         PRODUCT INTERFACE CODE                       
         B     FC10                                                             
         DROP  R2,R3                                                            
*                                                                               
FC12     TM    DATAIND2,DIPRD+DIEST   TEST FOR PRD OR EST ROWS                  
         BZ    FC18                NO-GET DEMO INFO NOW                         
         LA    R0,L'LEVELS         YES-TEST PRD OR EST AT ANY LEVEL             
         LA    R1,LEVELS               ABOVE DETAIL LEVEL                       
*                                      IF NOT, THEN GET DEMO INFO NOW           
FC14     CLI   0(R1),QEST                                                       
         BE    FC16                                                             
         CLI   0(R1),QESTNM                                                     
         BE    FC16                                                             
         CLI   0(R1),QPRD                                                       
         BE    FC16                                                             
         CLI   0(R1),QPRDNM                                                     
         BE    FC16                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,FC14                                                          
         B     FC18                                                             
*                                                                               
FC16     CHI   R0,1                                                             
         BE    FC18                                                             
         CLI   1(R1),0                                                          
         BNE   FC20                                                             
*                                                                               
FC18     GOTO1 ADEMOS              GET DEMO INFORMATION NOW                     
*                                                                               
FC20     XC    EQBASE,EQBASE                                                    
         LA    R5,SBEQTAB                                                       
         LA    R3,SLNTAB                                                        
         LHI   R1,SLNTABX-SLNTAB                                                
*                                                                               
FC22     CLC   0(2,R5),=H'1000'    LOOK FOR FIRST H'1000' TO GET BASE           
         BE    FC24                                                             
         LA    R5,4(R5)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,FC22                                                          
         B     FCX                                                              
*                                                                               
FC24     EDIT  (1,0(R3)),(2,EQBASE)                                             
         LA    R5,SBEQTAB                                                       
         LA    R1,15                                                            
*                                                                               
FC26     CLC   0(2,R5),=H'1000'    IF THEY'RE ALL 1000                          
         BNE   FCX                                                              
         LA    R5,4(R5)                                                         
         BCT   R1,FC26                                                          
         XC    EQBASE,EQBASE       SUPPRESS EQUIVALENCE                         
*                                                                               
FCX      XIT1                                                                   
         SPACE 1                                                                
SLNTAB   DS    0C                                                               
       ++INCLUDE SPSLNTAB                                                       
SLNTABX  EQU   *                                                                
         DS    0H                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET VARIABLE LENGTH ELEMENT FROM THE STA BUFFER AND MOVE IT TO 0(R1)*         
* PARMS  R1     = A(DATA) TO MOVE ELEMENT TO                          *         
*        R2     = A(STA BUFFER)                                       *         
*        BYTE   = ELEMENT CODE TO FIND IN THE STATION BUFFER          *         
***********************************************************************         
GETELEM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING STABUFFD,R2                                                      
         XR    RF,RF                                                            
         LA    R3,STBELEM          START OF ELEMENTS IN STA BUFFER              
*                                                                               
GELEM10  CLI   0(R3),0             END OF STATION BUFFER?                       
         BE    GELEMX              YES, DONE                                    
         CLI   1(R3),0                                                          
         BNE   *+6                                                              
         LR    R5,R5                                                            
         CLC   0(1,R3),BYTE        MATCH ON ELEMENT ID?                         
         BE    GELEM20             YES                                          
         IC    RF,1(R3)            LENGTH OF ELEMENT                            
         AR    R3,RF               BUMP PAST THIS ELEMENT                       
         B     GELEM10             AND CHECK FOR END OF STATION BUFFER          
*                                                                               
GELEM20  IC    RF,1(R3)            ELEMENT LENGTH                               
         SHI   RF,3                MINUS ELEM OVERHEAD AND 1 FOR EX             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),2(R3)       ** EXECUTED **                               
*                                                                               
GELEMX   J     XIT                                                              
         DROP  R2                                                               
***********************************************************************         
* PUT VARIABLE LENGTH ELEMENT INTO THE STATION BUFFER                 *         
* PARMS  R2     = A(STA BUFFER)                                       *         
*        R5     = A(DATA) TO BE MOVED TO ELEMENT                      *         
*        HALF   = ELEMENT CODE                                        *         
*        HALF+1 = MAX ELEMENT LENGTH                                  *         
***********************************************************************         
PUTELEM  NTR1  BASE=*,LABEL=*                                                   
         USING STABUFFD,R2                                                      
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         LLC   R1,HALF+1                                                        
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),SPACES      HAVE ANY DATA TO BUFFER?                     
         BNH   PEXIT               NO                                           
*                                                                               
         XR    RF,RF                                                            
         LA    R1,STBELEM          START OF ELEMENTS IN STA BUFFER              
*                                                                               
PELEM10  CLI   0(R1),0             END OF STATION BUFFER?                       
         BE    PELEM20             YES                                          
         IC    RF,1(R1)            LENGTH OF ELEMENT                            
         AR    R1,RF               BUMP PAST THIS ELEMENT                       
         B     PELEM10             AND CHECK FOR END OF STATION BUFFER          
*                                                                               
PELEM20  MVC   0(1,R1),HALF        ELEMENT CODE                                 
         IC    RF,HALF+1           MAX LENGTH OF ELEMENT                        
         BCTR  RF,0                -1                                           
         LA    R2,0(RF,R5)         R2 = LAST CHAR OF DATA                       
         IC    RF,HALF+1           MAX LENGTH OF ELEMENT                        
         LAY   RE,MSFREQ                                                        
         CR    RE,R5               TRYING TO ADD FIXED LENGTH ELEMENT?          
         BE    PELEM30             YES - SAVE MSFREQ,MSSTATE & MSMOWNER         
         CLI   HALF,STADD3         TRYING TO ADD FIXED LENGTH ELEMENT?          
         BE    PELEM30             YES                                          
         CLI   HALF,SMADD3         TRYING TO ADD FIXED LENGTH ELEMENT?          
         BE    PELEM30             YES                                          
*                                                                               
PELEM25  CLI   0(R2),X'40'         HAVE ANY DATA                                
         BH    PELEM30             YES                                          
         BCTR  R2,0                DECREMENT DATA POINTER                       
         BCT   RF,PELEM25          DECREMENT DATA LENGTH POINTER                
         DC    H'0'                POINTING TO STORAGE BEFORE DATA              
*                                                                               
PELEM30  AHI   RF,2                DATA + ELEM CODE + ELEM LENGTH               
         STC   RF,1(R1)            STORE ELEMENT LENGTH                         
         SHI   RF,3                - ELEM CODE - ELEM LEN - 1 FOR EX            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R1),0(R5)       ** EXECUTED **                               
*                                                                               
PEXIT    J     XIT                                                              
         DROP  R2,R3                                                            
*                                                                               
RDCTFILE NTR1  BASE=*,LABEL=*                                                   
         MVC   FILENAME,=CL8'CTFILE'                                            
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         STC   R0,USEIO                                                         
         XC    FILENAME,FILENAME                                                
         J     XIT                                                              
*                                                                               
ESTFILT  NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDRD,R2          R2 = ESTIMATE HEADER                         
         LA    R1,3                3 FILTER VALUES TO LOOP THROUGH              
         LA    RE,SBQESFLT         REQUESTED ESTIMATE FILTERS                   
         LA    RF,EPROF            ESTIMATE FILTERS ON ESTIMATE REC             
*                                                                               
ESTF10   CLI   0(RE),C'*'          FILTER = *?                                  
         JE    XITEQ               YES - ACCEPT ANYTHING SO WE PASSED           
         CLI   0(RE),C' '          SPACE?                                       
         JE    ESTF30              YES - DONE                                   
         TM    0(RE),X'40'         NEGATIVE FILTER?                             
         BZ    ESTF20              YES                                          
         CLC   0(1,RE),0(RF)       FILTER MATCHES EST FILTER?                   
         JE    XITEQ               YES - WE HAVE A MATCH                        
         CLC   0(1,RE),1(RF)       FILTER MATCHES EST FILTER+1?                 
         JE    XITEQ               YES - WE HAVE A MATCH                        
         CLC   0(1,RE),2(RF)       FILTER MATCHES EST FILTER+2?                 
         JE    XITEQ               YES - WE HAVE A MATCH                        
         B     ESTF30              NO - CHECK NEXT EST FILTER                   
*                                                                               
ESTF20   MVC   BYTE,0(RE)          MOVE NEGATIVE FILTER TO BYTE                 
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)          NEG FILTER MATCHES EST FILTER?               
         JNE   XITEQ               NO - THIS ESTIMATE PASSED                    
         CLC   BYTE,1(RF)          NEG FILTER MATCHES EST FILTER+1?             
         JNE   XITEQ               NO - THIS ESTIMATE PASSED                    
         CLC   BYTE,1(RF)          NEG FILTER MATCHES EST FILTER+2?             
         JNE   XITEQ               NO - THIS ESTIMATE PASSED                    
*                                                                               
ESTF30   LA    RE,1(RE)            BUMP TO NEXT EST FILTER                      
         BCT   R1,ESTF10           LOOP BACK AND CHECK THIS FILTER              
         J     XITNEQ              NO MATCH FOUND - FILTER EST OUT!             
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T20402 - SPWRITER EXTENSION ROUTINES TO T20401-SQADINI'         
*********************************************************************           
*                                                                   *           
*        SQADINI - ROUTINE TO SET UP DAYPART TRANSLATION TABLE      *           
*                                                                   *           
*********************************************************************           
         SPACE 2                                                                
SQADINI  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R6                                                         
         USING GEND,RC                                                          
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
         LA    RA,SBLOCK           ESTABLISH SPOTBLOCK                          
         USING SBLOCK,RA                                                        
*                                                                               
         L     R7,SBSQDBFA         ESTABLISH SQAD BUFFER                        
         USING SQDBUFFD,R7                                                      
*                                                                               
         MVC   SQDBUFID,=CL8'SQADBUFF' ID FOR DUMPS                             
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000ADF'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SQDDOUTV,0(R1)                                                   
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000ADD'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SQDDMNDV,0(R1)                                                   
*                                                                               
         MVC   SQDKEYCT,KEY        SAVE CURRENT KEY                             
         MVC   SQDAIOSV,AIO        SAVE CURRENT AIO                             
*                                                                               
         MVI   SQDRDSW,0           INDICATE NO KEYS READ                        
*                                                                               
         XC    KEY,KEY             ESTABLISH SQADREC KEY                        
         LA    R2,KEY                                                           
         USING SQDRECD,R2                                                       
*                                                                               
         MVI   SQDKTYP,SQDKTYPQ    SET RECORD TYPE                              
         MVI   SQDKSUB,SQDKSUBQ    SET RECORD SUB-TYPE                          
         MVC   SQDKAGMD,SBBAGYMD   SET AGENCY MEDIA                             
*                                                                               
         CLC   SQDKEY(SQDKDPT-SQDKEY),SQDKEYSV OKAY IF TABLE BUILT              
         BE    SQADTABX                                                         
*                                                                               
         MVI   SQDRDSW,C'Y'        INIDICATE KEY READ                           
*                                                                               
         LA    R3,SQDDPTTB         POINT TO SQAD DPT TRANSLATION TABLE          
*                                                                               
         XC    0(SQDDPTL,R3),0(R3)  INIT FIRST TABLE ENTRY                      
*                                                                               
         LA    RF,IOAREA           SET TO READ INTO IOAREA                      
         ST    RF,AIO                                                           
*                                                                               
         GOTO1 HIGH                READ IN KEY                                  
*                                                                               
SQADDPLP DS    0H                                                               
*                                                                               
         LA    R2,KEY              RE-POINT DSECT TO KEY                        
*                                                                               
         CLC   SQDKEY(SQDKDPT-SQDKEY),KEYSAVE  MUST FIND RECORD                 
         BNE   SQADDPDN                                                         
*                                                                               
         MVC   SQDKEYSV,SQDKEY     SAVE SQAD KEY                                
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         L     R2,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LA    R4,SQDEL            POINT TO FIRST ELEMENT                       
         USING SQDEL,R4            ESTABLISH AS DAYPART ELEMENT                 
*                                                                               
         SR    RF,RF                                                            
*                                                                               
SQADLOOP DS    0H                                                               
*                                                                               
         CLI   SQDEL,0             DONE AT END OF RECORD                        
         BE    SQADDONE                                                         
*                                                                               
         CLI   SQDEL,SQDELQ        SKIP IF NOT A DAYPART ELEMENT                
         BNE   SQADCONT                                                         
*                                                                               
         MVC   0(1,R3),SQDKDPT     SAVE DAYPART MENU CODE                       
         MVC   1(1,R3),SQDDPT      SAVE AGENCY DAYPART CODE                     
         MVC   2(2,R3),SQDSQAD     SAVE SQAD EQUIVALENT DAYPART CODE            
*                                                                               
         LA    R3,SQDDPTL(R3)      BUMP TO NEXT TABLE ENTRY                     
         XC    0(SQDDPTL,R3),0(R3) INIT NEXT TABLE ENTRY                        
*                                                                               
SQADCONT DS    0H                                                               
*                                                                               
         IC    RF,SQDLEN           GET ELEMENT LENGTH                           
         LA    R4,SQDEL(RF)        BUMP TO NEXT ELEMENT                         
*                                                                               
         B     SQADLOOP                                                         
*                                                                               
SQADDONE DS    0H                                                               
*                                                                               
SQADDPCN DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
         B     SQADDPLP                                                         
*                                                                               
SQADDPDN DS    0H                                                               
*                                                                               
         MVC   AIO,SQDAIOSV        RESTORE AIO                                  
*                                                                               
SQADTABX DS    0H                                                               
*                                                                               
         MVC   KEY,SQDKEYCT        RESTORE CURRENT KEY                          
*                                                                               
         CLI   SQDRDSW,C'Y'        IF KEY WAS READ                              
         BNE   SQADKEYX                                                         
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
SQADKEYX DS    0H                                                               
*                                                                               
SQADINIX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
SQDAIOSV DS    A                   CURRENT AIO SAVEAREA                         
SQDKEYCT DS    XL(L'KEY)           CURRENT KEY SAVEAREA                         
SQDRDSW  DS    XL1                 C'Y' KEY WAS READ                            
*                                                                               
         DROP                                                                   
*                                                                               
* ROUTINE TO GET BILL VALUES                                                    
* INPUT  : R1=A(STATION BILL ELEMENT)                                           
* OUTPUT : SBBILGRS=EFFECTIVE GROSS                                             
*          SBBILNET=EFFECTIVE NET                                               
*          SBBILTAX=EFFECTIVE TAX                                               
*          BILLCOST=ACTUAL BILL AMOUNT                                          
*          BILLGST=GST AMOUNT                                                   
*          BILLPST=PST AMOUNT                                                   
*                                                                               
         SPACE 2                                                                
BILLVAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING WORKD,R6                                                         
         USING GEND,RC                                                          
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
BILLVAL1 LR    R5,R1                                                            
         USING STABELEM,R5                                                      
         XC    DMCB+8(4),DMCB+8                                                 
         TM    DATAIND5,DIBILLC    TEST BILLED COST NEEDED                      
         BO    *+12                                                             
         TM    SBEGST,SBEGSTB+SBEPSTB   OR BILLED GST/PST.                      
         BZ    BVAL5                                                            
*                                  YES-FIND THE BILL FORMULA                    
         MVC   BYTE,SBBILBAS       DEFAULT BILL FORMULA                         
         MVC   FULL,SBBILCOM                                                    
         CLC   SBQEST,SBQESTND     TEST MULTIPLE ESTIMATES LUMPED               
         BE    *+12                TOGETHER                                     
         CLI   SBQSEPES,C'Y'                                                    
         BNE   BVAL2               YES - USE PRODUCT BILL FORMULA               
         ICM   R1,15,SBAESTTB      NO - USE ESTIMATE BILL FORMULA               
         BZ    BVAL4                                                            
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,SBBEST                                                        
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         LA    R1,0(R1,RE)                                                      
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         MHI   RE,ESTBUFFL                                                      
         ICM   R1,15,SBAESTBF                                                   
         BZ    BVAL4                                                            
         LA    RE,0(R1,RE)                                                      
         USING ESTBUFFD,RE                                                      
         MVC   BYTE,EBBILBAS                                                    
         MVC   FULL,EBBILCOM                                                    
         B     BVAL4                                                            
         DROP  RE                                                               
*                                                                               
BVAL2    ICM   R1,15,SBAPRDBF      GET PRODUCT BILL FORMULA                     
         BZ    BVAL4                                                            
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MHI   RE,PRDBUFFL                                                      
         LA    RE,0(RE,R1)                                                      
         USING PRDBUFFD,RE                                                      
         MVC   BYTE,PBBILBAS                                                    
         MVC   FULL,PBBILCOM                                                    
         DROP  RE                                                               
*                                                                               
BVAL4    MVC   BILLFORM(1),BYTE                                                 
         MVC   BILLFORM+1(4),FULL                                               
         LA    R1,BILLFORM                                                      
         ST    R1,DMCB+8                                                        
*                                                                               
BVAL5    LR    RF,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   RF,4096                                                          
         USING SYSD+4096,RF                                                     
         TM    SBEFLAG5,SBE5BFRM   OPT SET TO GET BILL FORM RECORD?             
         BZ    BVAL5A              NO                                           
         CLI   SBB1XPRF+11,C'N'    PROFILE SET TO GET BILL FORMULA?             
         BE    BVAL5A              NO                                           
         MVC   BILLFORM,SBBFORM    YES - USE SFM BFORM REC                      
         DROP  RF                                                               
*                                                                               
BVAL5A   XC    SPBVALD(SPBVALDL),SPBVALD                                        
         GOTO1 SPBVAL,DMCB,(C'E',(R5)),SPBVALD                                  
         MVC   SBBILGRS,SPBVEGRS   EFFECTIVE GROSS, NET, TAX                    
         MVC   SBBILNET,SPBVENET                                                
*                                                                               
         LA    R1,SBLOCK                                                        
         USING SBLOCK,R1                                                        
         TM    SBEFLAG7,SBE7COS2   COST2 BILL?                                  
         BZ    BVAL5B              NO                                           
         ZAP   SBILGRSP,=P'0'      CLEAR EFFECTIVE GROSS PACKED                 
         ZAP   SBILNETP,=P'0'      CLEAR EFFECTIVE NET PACKED                   
         XC    SBBILTAX,SBBILTAX   CLEAR BILLED TAX                             
         XC    BILLCOST,BILLCOST   CLEAR BILLED COST                            
         ZAP   BILCOSTP,=P'0'      CLEAR ACTUAL PACKED                          
         XC    BILLGST,BILLGST     CLEAR BILL GST                               
         XC    BILLPST,BILLPST     CLEAR BILL PST                               
         B     BVALX               EXIT                                         
*                                                                               
BVAL5B   ZAP   SBILGRSP,SPBVGRSP   EFFECTIVE GROSS PACKED                       
         ZAP   SBILNETP,SPBVNETP   EFFECTIVE NET PACKED                         
         DROP  R1                                                               
*                                                                               
         L     R4,SPBVETAX                                                      
         ST    R4,SBBILTAX                                                      
         L     RE,SPBVACT                                                       
         STCM  RE,15,BILLCOST                                                   
         ZAP   BILCOSTP,SPBVACTP   ACTUAL PACKED                                
         XC    BILLGST,BILLGST                                                  
         XC    BILLPST,BILLPST                                                  
         TM    SBEGST,SBEGSTB+SBEPSTB  TEST BILLED GST/PST NEEDED               
         BZ    BVALX                                                            
         LA    R1,SBAGYREC         YES-CHECK IT'S CANADA                        
         CLI   AGYPROF+7-AGYHDRD(R1),C'C'                                       
         BNE   BVALX                                                            
         SR    RE,R4              SUBTRACT TAX FROM BILL COST                   
*                                 DETERMINE IF ANY STATION PST APPLIES          
         XC    HALF,HALF          HALF+0=PST INDEX,HALF+1=STA PROV ID           
         MVI   HALF,X'FF'         INITIALISE INDEX AS NO STATION PST            
         LR    R7,R9               R7=A(SBASTANT)                               
         AHI   R7,SBASTANT-SYSD                                                 
         XR    RF,RF                                                            
         ICM   RF,7,0(R7)          R2=SBASTANT                                  
         BZ    BVAL7                                                            
         LA    RF,STBPST-STABUFFD(RF)                                           
* NOTE: THE LOCAL STATION REC HAS A SINGLE PST SETTING FOR THAT MARKET          
* IF SPECIALTY CABLE THEN STATION REC IS FOR THE 'NETWORK' BUT IT               
* WILL ONLY HAVE ONE PST SETTING REFLECTING THE 'HOME' OF THE STATION.          
         LA    R1,10                                                            
         CLI   0(RF),0                                                          
         BNE   BVAL6               FOUND A PROVINCE SETTING                     
         AHI   RF,1                                                             
         BCT   R1,*-12                                                          
         B     BVAL7                                                            
* PROVINCE POSITIONS 1-10 ARE BC/AL/SA/MA/ON/PQ/NB/NS/PE/NF                     
BVAL6    MVC   HALF+1(1),0(RF)     SAVE PROVINCE PST VALUE                      
         LNR   RF,R1                                                            
         AHI   RF,10                                                            
         STC   RF,HALF             SAVE INDEX INTO PST CODES                    
*                                                                               
BVAL7    MVI   MAINPST,C'N'        NO MAIN PST YET                              
         LR    RF,R9               GET ADDRESSABILITY TO END OF SBLOCK          
         AHI   RF,4096                                                          
         USING SYSD+4096,RF                                                     
         MVC   CPSTVAL,SBCPST      CLIENT PST                                   
         CLC   STABPER,=X'6E07'    BEFORE JUL/10?                               
         JL    BVAL7A              YES - DON'T LOOK AT MAIN PST                 
         OC    SBCMPST,SBCMPST     HAVE MAIN PST FOR CLIENT?                    
         JZ    BVAL7A              NO - EXIT                                    
         XC    CPSTVAL,CPSTVAL     CLEAR PST STRING                             
         LLC   R1,SBCMPST          GET INDEX (STARTS AT 1 - NOT 0!)             
         BCTR  R1,0                SUBTRACT ONE SO WE INDEX FROM 0              
         STC   R1,HALF             SAVE INDEX INTO PST CODES                    
         LA    R1,CPSTVAL(R1)      CLIENT PST FIELD PLUS INDEX                  
         MVC   0(1,R1),SBCMPST+1   PST VALUE                                    
         MVC   HALF+1(1),0(R1)     SAVE PROVINCE PST VALUE                      
         MVI   MAINPST,C'Y'        HAVE MAIN PST                                
         DROP  RF                                                               
*                                                                               
BVAL7A   XC    FULL,FULL           FULL=A(PRODUCT IN PRDBUFF)                   
         CLI   SBBPRD,X'FF'        EXCEPT FOR POL,PRODUCT MAY BE EXEMPT         
         BE    BVAL8                                                            
         ICM   R1,15,SBAPRDBF                                                   
         BZ    BVAL8                                                            
         LLC   RF,SBBPRD                                                        
         BCTR  RF,0                                                             
         MHI   RF,PRDBUFFL                                                      
         LA    RF,0(RF,R1)                                                      
         ST    RF,FULL             SAVE A(PRDBUFF) ENTRY                        
         USING PRDBUFFD,RF                                                      
         MVC   PPSTVAL,PBUFLD2     PRODUCT PST                                  
         CLC   STABPER,=X'6E07'    BEFORE JUL/10?                               
         JL    BVAL7B              YES - DON'T LOOK AT MAIN PST                 
         OC    PBUFMPST,PBUFMPST   HAVE MAIN PST FOR PRODUCT?                   
         JZ    BVAL7B              NO - EXIT                                    
         XC    PPSTVAL,PPSTVAL     CLEAR PST STRING                             
         LLC   R1,PBUFMPST         GET INDEX (STARTS AT 1 - NOT 0!)             
         BCTR  R1,0                SUBTRACT ONE SO WE INDEX FROM 0              
         STC   R1,HALF             SAVE INDEX INTO PST CODES                    
         LA    R1,PPSTVAL(R1)      PRODUCT PST FILED PLUS INDEX                 
         MVC   0(1,R1),PBUFMPST+1  PST VALUE                                    
         MVC   HALF+1(1),0(R1)     SAVE PROVINCE PST VALUE                      
         MVI   MAINPST,C'Y'        HAVE MAIN PST                                
*                                                                               
BVAL7B   CLI   PBGSTCD,C'X'        PRODUCT GST SETTING OVERRIDES CLIENT         
         BE    BVAL10                                                           
         CLI   PBGSTCD,C'Z'                                                     
         BE    BVAL10                                                           
         CLI   PBGSTCD,C'S'                                                     
         BE    BVAL9               STD GST                                      
         DROP  RF                                                               
*                                                                               
BVAL8    CLI   SBCEXTRA+11,C'X'    TEST CLIENT IS GST EXEMPT                    
         BE    BVAL10                                                           
         CLI   SBCEXTRA+11,C'Z'                                                 
         BE    BVAL10                                                           
*                                  GST MAY NOT APPLY DEPENDING ON PST           
BVAL9    CLI   HALF,X'FF'          STATION PST PROV INDEX                       
         BE    BVAL9B              NONE                                         
         CLI   HALF,0              BC (BRITISH COLUMBIA)?                       
         BNE   *+14                NO                                           
         CLC   STABBDT,=X'E281'    ON AFTER APR01/2013?                         
         BNL   BVAL9BA             YES - NO HST BUT NEED GST!                   
         CLI   HALF,5              INDEX OF 5 MEANS PROV#6-QUEBEC               
         BE    BVAL9BA                                                          
         SR    R1,R1               MUST BE AN HST PROVINCE                      
         IC    R1,HALF                                                          
*                                                                               
         ICM   RF,15,FULL          TEST PRODUCT SUBJECT TO THIS HST             
         BZ    BVAL9A              NO PRODUCT PST TO TEST                       
         LA    RF,PPSTVAL          PRODUCT PST                                  
         OC    PPSTVAL,PPSTVAL     HAVE PRODUCT PST?                            
         BNZ   *+8                 YES                                          
BVAL9A   LA    RF,CPSTVAL          NO - USE CLIENT PST                          
         AR    RF,R1               ADD PST INDEX                                
         CLI   0(RF),C'H'          HST?                                         
         BNE   BVAL9B              NO                                           
         CLI   HALF,8              PE (PRINCE EDWARD ISLAND)?                   
         BNE   BVAL10              NO - HST APPLIES SO NO GST                   
         CLC   STABBDT,=X'E281'    BEFORE APR01/2013?                           
         BL    BVAL9BA             YES                                          
         B     BVAL10              NO - HST APPLIES SO NO GST                   
*********************************************************************           
*  NOTE: WHEN CHANGING GST FOR CANADA, THIS CODE IS IN SPOTBUY      *           
*  TOO NEAR CALEC9B                                                 *           
*********************************************************************           
BVAL9B   CLI   MAINPST,C'Y'        HAVE MAIN PST?                               
         BE    BVAL10              YES - DON'T REPORT GST!                      
BVAL9BA  LR    R0,RE               SAVE BILL COST-TAX FOR ANY PST CALC          
         LR    RF,RE                                                            
         CLC   STABBDT,=X'D4E1'                                                 
         BNL   *+12                                                             
         M     RE,=F'14'            AND TAKE 7 PERCENT (= GST RATE)             
         B     BVAL9C                                                           
*                                                                               
         CLC   STABBDT,=X'D821'    ON/AFTER 1/1/08?                             
         BNL   *+12                                                             
         M     RE,=F'12'           6% FROM 7/1/06-12/31/07                      
         B     BVAL9C                                                           
         M     RE,=F'10'           5% ON/AFTER 1/1/08                           
*                                                                               
BVAL9C   D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,BILLGST                                                       
         LR    RE,R0               BILL COST-TAX (FOR PST CALC)                 
         AR    RE,RF               + GST                                        
*                                                                               
BVAL10   TM    SBEGST,SBEPSTB      WANT PST OR...                               
         BNZ   *+12                                                             
         TM    SBEFLAG,SBEPST      ADD IN PST?                                  
         BZ    BVALX                                                            
         CLI   HALF,X'FF'          TEST ANY STATION PST PROV INDEX              
         BE    BVALX               NOPE, SO NO PST TO APPLY                     
         SR    R1,R1                                                            
         IC    R1,HALF                                                          
         ICM   RF,15,FULL          TEST PRODUCT EXEMPT THIS PST                 
         BZ    BVAL10A             NO PRODUCT PST TO TEST                       
         LA    RF,PPSTVAL          PRODUCT PST                                  
         OC    PPSTVAL,PPSTVAL     HAVE PRODUCT PST?                            
         BNZ   *+8                 YES                                          
BVAL10A  LA    RF,CPSTVAL          CLIENT PST                                   
         AR    RF,R1               ADD PST INDEX                                
         CLI   0(RF),C'S'          STD ONLY - X/Z/_ MEAN NO OUTPUT PST          
         BE    BVAL11                                                           
         CLI   0(RF),C'H'                                                       
         BNE   BVALX               MUST BE X/Z/BLANK SO NO OUTPUT PST           
*                                                                               
* NOTE FOR MEDIA N, IF WE HAVE A LOCAL AFFILIATE STATION HERE IT SHOULD         
* ONLY BILL PST IF THE NETWORK HAS BLANK FOR CORRESPONDING PST BUT WE           
* HAVE NO MEANS OF CHECKING THIS SO JUST LET LOCAL SETTING DICTATE              
*                                  DETERMINE ANY PST FROM STATION               
BVAL11   CLI   HALF,5              INDEX OF 5 MEANS PROV#6-QUEBEC               
         BNE   BVAL12              MUST BE AN HST PROVINCE                      
         CLI   HALF+1,C'*'         TEST QST NOT APPLY TO BILLING                
         BE    BVALX               (BYTE IS STATION PROV PST SETTING)           
         LR    RF,RE               BILL COST-TAX+ANY GST                        
         CLC   STABBDT,=X'DE21'    ON OR AFTER JAN01/2011?                      
         BNL   BVAL11A             YES - RATE IS 8.5%                           
         M     RE,=F'15'           S/X/Z STATION HAS QST, WHICH IS 7.5%         
         B     BVAL15                                                           
*                                                                               
BVAL11A  CLC   STABBDT,=X'E021'    ON OR AFTER JAN01/2012?                      
         BNL   BVAL11B             YES - RATE IS 9.5%                           
         M     RE,=F'17'           S/X/Z STATION HAS QST, WHICH IS 8.5%         
         B     BVAL15                                                           
*                                                                               
BVAL11B  CLC   STABBDT,=X'E221'    ON OR AFTER JAN01/2013?                      
         BNL   BVAL11C             YES - RATE IS 9.975%                         
         M     RE,=F'19'           S/X/Z STATION HAS QST, WHICH IS 9.5%         
         B     BVAL15                                                           
*                                                                               
BVAL11C  M     RE,=F'1995'         S/X/Z STA HAS QST, WHICH IS 9.975%           
         D     RE,=F'100'          DIVIDE FOR DEC PRECISION                     
         XR    RE,RE               CLEAR REMAINDER                              
         B     BVAL15                                                           
*                                                                               
BVAL12   LR    RF,RE               BILL COST-TAX+ANY GST                        
         CLC   STABPER,=X'6E07'    ON/AFTER JUL/10?                             
         JNL   BVAL13              YES - PROCESS MAIN PST                       
         CLC   STABBDT,=X'D4E1'                                                 
         BNL   *+12                                                             
         M     RE,=F'30'           H/X/Z STATION HAS HST, WHICH IS 15%          
         B     BVAL15                                                           
*                                                                               
         CLC   STABBDT,=X'D821'                                                 
         BNL   *+12                                                             
         M     RE,=F'28'           14% 7/1/06 TO 12/31/07                       
         B     BVAL15                                                           
         M     RE,=F'26'           13% AS OF 1/1/08                             
         B     BVAL15                                                           
*                                                                               
BVAL13   CLI   MAINPST,C'Y'        HAVE MAIN PST?                               
         BNE   BVALX               NO - NO PST                                  
         CLI   HALF,0              BC (BRITISH COLUMBIA)?                       
         BNE   BVAL14              NO                                           
         CLC   STABBDT,=X'E281'    ON AFTER APR01/2013?                         
         BNL   BVALX               YES - NO HST                                 
         M     RE,=F'24'           YES - 12% AS OF 7/1/10                       
         B     BVAL15                                                           
*                                                                               
BVAL14   CLI   HALF,6              NB (NEW BRUNSWICK)?                          
         BE    *+12                YES                                          
         CLI   HALF,9              NF (NEWFOUNDLAND)?                           
         BNE   BVAL14AA            NO                                           
         CLC   STABBDT,=X'E8E1'    ON AFTER JUL01/2016?                         
         BL    BVAL14A             NO - GETS 13%                                
         M     RE,=F'30'           YES - 15% AS OF 6/1/16                       
         B     BVAL15                                                           
*                                                                               
BVAL14AA CLI   HALF,7              NS (NOVIA SCOTIA)?                           
         BNE   *+12                NO                                           
         M     RE,=F'30'           YES - 15% AS OF 7/1/10                       
         B     BVAL15                                                           
*                                                                               
         CLI   HALF,8              PE (PRINCE EDWARD ISLAND)?                   
         BNE   BVAL14A             NO                                           
         CLC   STABBDT,=X'E941'    ON AFTER OCT01/2016?                         
         BL    *+12                NO                                           
         M     RE,=F'30'           YES - 15% AS OF 10/1/16                      
         B     BVAL15                                                           
         CLC   STABBDT,=X'E281'    ON AFTER APR01/2013?                         
         BL    BVALX               NO                                           
         M     RE,=F'28'           YES - 14% AS OF 4/1/13                       
         B     BVAL15                                                           
*                                                                               
BVAL14A  M     RE,=F'26'           EVERYTHING ELSE IS 13%                       
*                                                                               
BVAL15   D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,BILLPST          SAVE PST                                     
*                                                                               
BVALX    XIT1  ,                                                                
*                                                                               
PPSTVAL  DS    CL10                PRD PST/MAIN PST STRING                      
CPSTVAL  DS    CL10                CLT PST/MAIN PST STRING                      
MAINPST  DS    CL1                 HAVE CLT/PRD MAIN PST                        
         DROP                                                                   
*                                                                               
TSARHIGH NTR1  BASE=*,LABEL=*                                                   
         USING WORKD,R6                                                         
         USING GEND,RC                                                          
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
         MVC   WORK(4),2(R2)       SAVE OFF THE KEY                             
         LA    R4,TSAREA                                                        
         USING TSARD,R4                                                         
         MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         ST    R2,TSAREC                                                        
         GOTO1 ATSAROF,(R4)                                                     
         TM    TSERRS,TSEEOF       DID WE HIT EOF?                              
         BO    TSARHNEQ            YES, SET CC NOT EQU                          
         CLC   WORK(4),2(R2)       MATCH ON KEY?                                
         BE    TSAREQU             YES                                          
*                                                                               
TSARHNEQ LA    RE,TSARKEYL+2(R2)   START CLEARING STA BUFFER HERE               
         LA    RF,SBUFMAX          CLEAR IT FOR THIS MANY BYTES                 
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LTR   RB,RB                                                            
         B     TSARXIT                                                          
*                                                                               
TSAREQU  CR    RB,RB                                                            
*                                                                               
TSARXIT  XIT1                                                                   
         DROP                                                                   
*                                                                               
TSARADD  NTR1  BASE=*,LABEL=*                                                   
         USING WORKD,R6                                                         
         USING GEND,RC                                                          
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
         LA    R4,TSAREA                                                        
         USING TSARD,R4                                                         
         MVI   TSOFFACT,TSAADD     SET TO ADD A RECORD                          
         ST    R2,TSAREC           A(VARIABLE LENGTH RECORD)                    
         GOTO1 ATSAROF,(R4)        ADD THE RECORD                               
         CLI   TSERRS,0            ANY ERRORS?                                  
         JE    TSARAX              NO                                           
         CLI   TSERRS,TSEDUP       DUPLICATE KEY ON ADD?                        
         JE    TSARAX              YES, IGNORE IT                               
         DC    H'0'                                                             
*                                                                               
TSARAX   XIT1                                                                   
         DROP                                                                   
*                                                                               
         EJECT                                                                  
         TITLE 'T20402 - SPWRITER EXTENSION ROUTINES TO T20401'                 
         DS    0D                                                               
         DC    CL8'**STAIO*'                                                    
STAIO    DS    1024C                                                            
         EJECT                                                                  
       ++INCLUDE SPWRI01D                                                       
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDMASTD                                                                        
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
*SPGENSLK                                                                       
*SPGENPGEST                                                                     
*SPGENSQAD                                                                      
*SPOTTABD                                                                       
*SPEDICT                                                                        
*SPWRIFFD                                                                       
*SPSQDBUFFD                                                                     
*DDBSRPRMD                                                                      
*MAAORLKD                                                                       
*SPPWBLOCK                                                                      
*SPGENWIPW                                                                      
*DDUCOMD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDCOMFACS                                                      
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
MTRRECD  DSECT                                                                  
       ++INCLUDE SPGENMTR                                                       
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENDMN                                                       
       ++INCLUDE SPGENSTAT                                                      
       ++INCLUDE SPGENFLT                                                       
       ++INCLUDE SPGENSLK                                                       
       ++INCLUDE SPGENPGEST                                                     
       ++INCLUDE SPGENCLRST                                                     
       ++INCLUDE SPGENINFO                                                      
       ++INCLUDE SPGENSQAD                                                      
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
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
         EJECT                                                                  
       ++INCLUDE SPWRIWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSQDBUFFD                                                     
         EJECT                                                                  
BSRPRMD  DSECT                                                                  
       ++INCLUDE DDBSRPRMD                                                      
         EJECT                                                                  
       ++INCLUDE MAAORLKD                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPPWBLOCK                                                      
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE DDUCOMD                                                        
       ++INCLUDE CTGENRAD                                                       
       ++INCLUDE DDTSARD                                                        
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095SPWRI02   06/17/19'                                      
         END                                                                    
