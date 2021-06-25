*          DATA SET SPREPGT02  AT LEVEL 085 AS OF 01/15/19                      
*PHASE SPGT02A                                                                  
*INCLUDE DDUCOM                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'SPGT02 - SPOTPAK P&&G INTERFACE'                                
***********************************************************************         
*  SPECIAL NOTES:  ONLY ONE CLIENT SHOULD BE REQUESTED                *         
*                  PER JOB STREAM (ID)                                *         
*                  AS THE TRANSMISSION FILE NAME                      *         
*                  IS ONLY SENT ONCE AND THE NAME MIGHT BE            *         
*                  DIFFERENT BY CLIENT.                               *         
*                                                                     *         
***********************************************************************         
*  QOPT1 -   N = NO TAPE                                                        
*  QOPT2 -   TAPE SPEC CODE                                                     
*  QOPT3 -   Y = PRINT TAPE RECORD TRACE                                        
*  QOPT4 -   M=START-END DATES ARE MOS                                          
*  QOPT5 -   C = DO ONLY COMMISSION ONLY BILLS, AND PRESERVE NET                
*            A = AOR ONLY, B=AOR AND AOR/CLIENT, X = NON-AOR ONLY               
*            2 = USE COS2 BILLS                                                 
*  QOPT5+1 - * = SHOW NETPAK SUB-MED INSTEAD OF CLIENT NUMBER                   
*            X = FILTER ON NETPAK SUB-MED=X                                     
*  QOPT5+2   B = SUPPRESS BDE HEADERS (NOT SET VIA REQ)                         
*                                                                               
*  NOTE THAT QOPT5+2 = QOPT7 AND ARE SET DIRECTLY IN JCL NOT VIA REQ!           
*                                                                               
*  QOPT7     N     = NO MQ NOTIFICATION (NOT SET VIA REQ)                       
*            T     = TEST MQ NOTIFICATION (NOT SET VIA REQ)                     
*            P     = PROD MQ NOTIFICATION (NOT SET VIA REQ)                     
*            BLANK = TREAT AS PRODUCTION RUN (SAME AS P)                        
***********************************************************************         
* USER    JIRA        DATE                  CHANGE LOG                *         
* ---- ----------  -------- ----------------------------------------- *         
* YKVA SPEC-31432  HOTFIX  UPDATE PMT TERMS FOR UB FROM T193 TO T012  *         
* AKAT SPEC-27182  09/19/18 RESET ION ERROR FLAG FOR EACH REQUEST     *         
* YKVA SPEC-25921  7/13/18 UPDATE PAYMENT TERMS FOR UB TO T193        *         
* YKVA SPEC-25748  7/09/18 UPDATE PAYMENT TERMS FOR OU/CAN TO T056    *         
* YKVA SPEC-25741  7/09/18 UPDATE PAYMENT TERMS FOR OO/US TO T056     *         
* AKAT SPEC-18384  11/27/17 H&S FILE FOR P&G: FIX CUT OFF EST UDEF1   *         
* YKVA SPEC-14370   07/31/17 H&S FILE FOR P&G: GL CODE, LINE ITEM CAN *         
* YKVA SPEC-14692   07/31/17 H&S FILE FOR P&G: GL CODE, LINE ITEM US  *         
* SMUR SPEC-10110  03/02/17 REMOVE ZERO DOLLAR INVOICES               *         
* AKAT CSD-477     10/20/16 DO NOT SET PPGPRO FOR CARAT/NETWORK REQ   *         
* AKAT SPSUG-86    10/19/16 UPDATE PAYMENT TERMS FOR OO/OU TO T549    *         
* AKAT SPSUG-85    10/19/16 UPDATE PAYMENT TERMS FOR OO/OU TO T549    *         
* AKAT SPSUG-86    05/27/16 NEW EDI FILE FOR OMG USA                  *         
* AKAT SPSUG-85    05/27/16 NEW EDI FILE FOR OMG CANADA               *         
* AKAT CSD-477     05/27/16 NEW EDI FILE FOR CARAT                    *         
*                                                                     *         
* BPLA 04/13 CHANGE TO LINE ITEM TEST FIELD FOR HY                              
*                                                                               
* BPLA 09/12 CHANGE FOR MEDIACOM (HY) LEGAL ENTITY NOW                          
*            FROM PRD USER 2 INSTEAD OF HARDCODED                               
*                                                                               
* BPLA 08/11 CHANGES FOR GROUP M                                                
*            HARD CODE LEGAL ENTITY TO 283                                      
*            (IT WAS IN PRODUCT USER 1)                                         
*            PRODUCT USER 1 WILL BE INTERNAL ORDER NUMBER                       
*            (IT WAS IN ESTIMATE USER 2)                                        
*            SEE STATEMENTS THAT BEGIN WITH **HDTEST                            
*            THEY ARE FOR TESTING THE NEW FORMAT FOR AGENCY HD                  
*            WHICH HAD AN ENTRY IN THE VENTAB FOR CLIENT PGT                    
*            ONE FOR CLIENT PG1 ADDED                                           
*                                                                               
*  BPLA  10/07  CHANGE FOR TAPESTRY CLTS (HPG AND HP1)                          
*               PAYMENT TERMS NOW T010 TICKET # 0148589N                        
*                                                                               
*  ABEA  07/07  ADD CLIENT CODE PG6 TO TABLE FOR H9                             
*                                                                               
*  BPLA  12/06  ANOTHER CLIENT CODE P&G FOR H9 - HP1                            
*                                                                               
*  YKVA  10/06  CHANGE OF PMT TERMS FOR HPG TO BE T008                          
*                                                                               
*  BPLA  09/06  ANOTHER CLIENT CODE PGG FOR DU AND H9                           
*                                                                               
*  BPLA  09/06  ADD CLIENT CODE PG5 TO TABLE FOR DU                             
*                                                                               
*  BPLA  05/06  IF NETPAK AND ACCOUNT CODE (EST UDEF 1)                         
*               IS 14080026 CLEAR ORDER #, TAX CODE, JURISDICTION               
*               (LIKE DONE FOR SUPPLEMENTAL (PRD=SUP)                           
*               BUT LEAVE ALLOCATION (UCOM)                                     
*                                                                               
*  BPLA  11/05  PAYMENT TERMS CHANGE FOR CLIENT PGB (BROMLEY)                   
*               WAS T229 CHG TO T054  FOR ALL MEDIA                             
*                                                                               
*  BPLA  10/05  NEW MEDIAVEST CLIENT HPG -                                      
*               PAYMENT TERMS T011 FOR ALL MEDIA                                
*                                                                               
*  BPLA  09/05  CHANGES FOR 2 CHARACTER OFFICES                                 
*                                                                               
*  BPLA  12/04  VENDOR CODE CHANGED FOR AGENCY O0                               
*                                                                               
*  BPLA  10/04  MEDIAVEST PAYMENT TERMS FOR SPOT CABLE ANDY SYNDICATION         
*               CHANGED BACK TO T148 FROM T234                                  
*                                                                               
*  6/04      IF MEDIAVEST AND ACCOUNT NUMBER IS 14080026                        
*            THE SET INTERNAL ORDER #, TAX CODE, AND TAX                        
*            JURISDICTION TO SPACES.  THIS IS FOR SOAP (SOA)                    
*            SPOT TV ADVERTISING.                                               
*                                                                               
*  6/04      CHANGES FOR PAYMENT TERMS                                          
*                                                                               
*  4/30/04   ADD CODE FOR MEDIAVEST TORONTO (AGY= O0)                           
*            PAYMENT TERMS ARE T007 (LIKE PRINT'S NON-CD)                       
*                                                                               
*  2/11/04   PAYMENT TERMS SET TO T234 FOR SPOT AND CABLE AND                   
*            SYNDICATION FOR MEDIAVEST (TERMS WERE CODE T148)                   
***********************************************************************         
         PRINT NOGEN                                                            
SPGT02   CSECT                                                                  
         NMOD1 0,SPGT02,R6,R8                                                   
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SPGTWRKD,RC                                                      
*                                                                               
         RELOC RELO                                                             
*                                                                               
         CLI   MODE,PROCBILL                                                    
         BE    PRBL                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,OFCFRST                                                     
         BE    FBILO                                                            
         CLI   MODE,CLTFRST                                                     
         BE    FBILC                                                            
         CLI   MODE,PRDFRST                                                     
         BE    FBILP                                                            
         CLI   MODE,PRDLAST                                                     
         BE    LBILP                                                            
         CLI   MODE,CLTLAST                                                     
         BE    LBILC                                                            
         CLI   MODE,OFCLAST                                                     
         BE    LBILO                                                            
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        RUN FIRST                                                              
         SPACE 2                                                                
RUNF     DS    0H                                                               
         MVI   ERRORSW,0                                                        
         XC    RUNINVS,RUNINVS     CLEAR RUN INVOICE TOTALS                     
         MVI   GTOPENSW,C'N'       SET GT TAPE NOT OPEN                         
*                                                                               
         MVI   DOWNACT,C'N'                                                     
         LA    R3,RAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
*        BUILD BDE HEADERS                                                      
*        WILL BE SENT WITH FILE TO THE PRINTQUE                                 
*                                                                               
         MVC   BDEH1,SPACES                                                     
         MVC   BDEH2,SPACES                                                     
         MVC   BDEH3,SPACES                                                     
         MVC   BDEH4,SPACES                                                     
         MVC   BDEH1+4(19),=C'*HDR*EDICT=STARPROD'                              
         MVI   BDEH1+34,C'W'      FOR WIDE                                      
         MVC   BDEH2(5),=C'++DDS'                                               
         MVC   BDEH2+6(08),=C'SPBGTTRN'                                         
         MVC   BDEH3(5),=C'++DDS'                                               
         MVC   BDEH3+11(03),=C'SUB'                                             
         MVC   BDEH3+15(17),=C'P&&G SAP INTERFACE'                              
         MVC   BDEH4(5),=C'++DDS'                                               
         MVC   BDEH4+11(03),=C'FIL'                                             
         MVC   BDEH4+15(17),=C'F_XN_AGENCY_TEST_'                               
*                                                                               
         MVI   BDEHSW,0         SET HEADERS NOT SENT                            
         B     EXIT                                                             
         SPACE 3                                                                
*        REQUEST FIRST                                                          
         SPACE 2                                                                
REQF     DS    0H                                                               
*                                                                               
         MVC   SVAGENCY,QAGY                                                    
         L     RF,=V(DLFLD)                                                     
         A     RF,RELO                                                          
         ST    RF,VDLFLD                                                        
         L     RF,=V(DDUCOM)                                                    
         A     RF,RELO                                                          
         ST    RF,VDDUCOM                                                       
         L     RF,=A(DOWNLD)                                                    
         A     RF,RELO                                                          
         ST    RF,VDOWNLD                                                       
         L     RF,=A(FTPHUB)                                                    
         A     RF,RELO                                                          
         ST    RF,VFTPHUB                                                       
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+8                                                              
         MVI   NETPAKSW,C'N'                                                    
         MVC   AMQRPT,MCVMQRPT                                                  
         DROP  RF                                                               
*                                                                               
         CLI   QOPT5+2,C'B' SEE IF SUPPRESSING BDE FILE HEADERS                 
         BNE   *+8          (IF TRANSFER NOT READY)                             
         MVI   BDEHSW,1      SO PROGRAM WILL THINK THEY'RE ALREADY SENT         
*                            AND NOT SEND THEM                                  
*                                                                               
         MVI   HYMCSW,C'N'  SET OFF MCMMP SWITCH                                
         CLC   QAGY,=C'HY'  NO BDE HEADERS FOR AGENCY HY                        
         BNE   REQF1J       (IF TRANSFER NOT READY)                             
         MVI   BDEHSW,1      SO PROGRAM WILL THINK THEY'RE ALREADY SENT         
*                            AND NOT SEND THEM                                  
         MVI   HYMCSW,C'Y'      FOR MCMMP HY CLIENTS                            
         CLC   QCLT,=C'PG1'     CHECK MCMMP CLIENTS                             
         BE    REQF1J                                                           
         CLC   QCLT,=C'P12'                                                     
         BE    REQF1J                                                           
         CLC   QCLT,=C'P13'                                                     
         BE    REQF1J                                                           
         MVI   HYMCSW,C'N'      SET OFF MCMMP HY CLIENTS                        
*                                                                               
*                                                                               
*        FINISH FIXING BDE HEADERS                                              
*                                                                               
******   CLI   NETPAKSW,C'Y'                                                    
******   BNE   *+16                                                             
******   MVC   BDEH2+6(2),=C'NE'   NETPAK BDE                                   
******   MVC   BDEH4+15(3),=C'NET'                                              
******                                                                          
******   GOTO1 DATCON,DMCB,TODAY,(5,BDEH4+19)                                   
******   L     RF,ADAGY                                                         
******   MVC   BDEH4+28(L'AGYNAME),AGYNAME-AGYHDR(RF)                           
*                                  SET OFF VARIOUS WESTERN/APL SWITCHES         
*                                                                               
REQF1J   MVI   GTSFTP,C'N'       INIT TO NO HUB                                 
         CLC   QAGY,=C'UB'       AGENCY UB?                                     
         BE    REQF1JA           YES                                            
*                                                                               
         CLC   QAGY,=C'OO'       AGENCY OO?                                     
         BE    *+14              YES                                            
         CLC   QAGY,=C'OU'       AGENCY OU?                                     
         BNE   REQF1K            NO                                             
         NI    ERRORSW,X'FF'-X'02' RESET MISSING INTERNAL ORDER NUM ERR         
         NI    ERRORSW,X'FF'-X'04' RESET MISSING LEGAL ENTITY ERROR             
*                                                                               
REQF1JA  MVI   BDEHSW,1          YES - SUPPRESS BDE HEADERS                     
         MVI   GTSFTP,C'Y'       SEND GT FILE VIA THE HUB                       
*                                                                               
REQF1K   XC    REQINVS,REQINVS   CLEAR REQUEST                                  
         XC    OFFINVS,OFFINVS   OFFICE                                         
         XC    CLTINVS,CLTINVS   AND CLIENT INVOICE TOTALS                      
         XC    CLTZINV,CLTZINV   CLIENT ZERO INVOICE TOTALS                     
*                                                                               
         XC    PPGRECNT,PPGRECNT    CLEAR BINSRCH COUNTER                       
*                                                                               
         L     RF,ADAGY                                                         
         MVC   CNTRY,AGYPROF+7-AGYHDR(RF)   SET COUNTRY                         
         MVI   RCSUBPRG,0                                                       
         CLI   CNTRY,C'C'          CANADA GETS DIFFERENT SPROG                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,50         FOR GST                                      
         L     RF,=A(HHROUT)                                                    
         ST    RF,HEADHOOK                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVQST,QSTART        SAVE QSTART AND END                          
         CLC   QEND,SPACES         ANY END DATE PROVIDED?                       
         BNE   *+10                                                             
         MVC   QEND,QSTART         NO -- USE QSTART (SINGLE DATE RQST)          
         MVC   SVQEND,QEND                                                      
*                                                                               
*Y2K*                                                                           
         MVC   QSTART,=C'700101'   SET LONG RANGE START                         
         MVI   QEND,X'FF'            AND END FOR SPONSOR                        
         MVC   QEND+1(5),=C'91231'                                              
         MVC   DUB,SVQST                                                        
         OC    DUB(6),=6C'0'                                                    
         GOTO1 DATCON,DMCB,DUB,(3,BQSTART)                                      
         MVC   DUB,SVQEND                                                       
         OC    DUB(6),=6C'0'                                                    
         GOTO1 DATCON,DMCB,DUB,(3,BQEND)                                        
*                                 SET VALUES FOR RUN-DATE FILTERING             
         GOTO1 DATCON,DMCB,TODAY,(3,TODAYB)                                     
         ZIC   R1,TODAYB                                                        
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         STC   R1,DECADE           1980,1990,2000,2010, ETC.                    
*                                  HEX 50,5A,64,6E...                           
         MVC   YEARDIG,TODAY+1     GET YEAR WITHIN DECADE                       
         NI    YEARDIG,X'FF'-X'F0' ISOLATE YEAR DIGIT                           
*                                                                               
         XC    STARTMOS,STARTMOS   SET START MOS TO LOWEST POSSIBLE             
         MVC   ENDMOS,=X'FFFF'     SET END MOS TO HIGHEST POSSIBLE              
         CLC   QMOSSTRT,SPACES                                                  
         BE    REQF1N              NO START MOS FILTER                          
         MVC   DUB(4),QMOSSTRT                                                  
         MVC   DUB+4(2),=C'01'     FOR DATCON (COMPLETE DATE REQUIRED)          
         GOTO1 DATCON,DMCB,DUB,(3,THREE)                                        
         MVC   STARTMOS,THREE                                                   
         CLC   QMOSEND,SPACES                                                   
         BE    REQF1N              NO END MOS FILTER                            
         MVC   DUB(4),QMOSEND                                                   
         MVC   DUB+4(2),=C'01'     FOR DATCON (COMPLETE DATE REQUIRED)          
         GOTO1 DATCON,DMCB,DUB,(3,THREE)                                        
         MVC   ENDMOS,THREE                                                     
*                                                                               
REQF1N   CLI   QOPT1,C'N'             SKIP TAPE?                                
         BE    REQF16                 YES                                       
         CLI   GTSFTP,C'Y'            SENDING GT FILE VIA THE HUB?              
         BNE   REQF09                 NO                                        
         CLI   GTOPENSW,C'Y'          IS GT FILE ALREADY OPEN?                  
         BE    REQF16                 YES, DSNAME SET & FILE ALLOCATED          
         MVI   TESTMQ,0               IN MQ NOTIFICATION FLAG                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,CTODAY) TODAY - YYYYMMDD                   
         TIME  DEC                    R0=HHMMSSHH                               
*                                                                               
         ST    R0,FULL                R0 NOW HAS TIME HHMMSSHH (PWOS)           
         SRL   R0,4                   GET JUST HHMMSS                           
         ST    R0,WORD                SAVE OFF HHMMSS                           
         XC    DUB,DUB                CLEAR DUB                                 
         MVC   DUB+5(3),WORD          HHMMSS                                    
         OI    DUB+7,X'0F'            PREPARE FOR CVB                           
         CVB   R5,DUB                 HHMMSS                                    
         EDIT  (R5),(5,TIMEOFD),2,FILL=0                                        
         MVI   TIMEOFD+5,C'.'         HH.MM                                     
         UNPK  WORK(3),FULL+2(2)      GET SECONDS                               
         MVC   TIMEOFD+6(2),WORK      HH.MM.SS                                  
*                                                                               
         MVC   DSNAME,SPACES          CLEAR DSNAME                              
         MVC   DSNAME(4),=C'BIL.'     SFTPDISK.PROD.BIL.                        
         MVC   DSNAME+4(3),=C'SPT'    SFTPDISK.PROD.BIL.SPT                     
         CLI   NETPAKSW,C'N'          NET SYSTEM?                               
         BE    *+10                   NO                                        
         MVC   DSNAME+4(3),=C'NET'    SFTPDISK.PROD.BIL.NET                     
         MVI   DSNAME+7,C'.'          SFTPDISK.PROD.BIL.SPT.                    
         L     RF,VMASTC              A(MASTC)                                  
         USING MASTD,RF               MASTD DSECT                               
         L     R1,MCAEXTRA            EXTRA DATA AREA                           
         USING MCEXTRA,R1             EXTRA DATA AREA DSECT                     
         MVC   DSNAME+8(4),MCAGYCOD   AGY LABEL (CTAGCCOD) ON IDI REC           
***                                                                             
* FOR CARAT THE AGENCY LABEL FROM THE ACCESS RECORD IN C/GEN (UBNY)             
* IS ALREADY TAKEN BY THE EX REPORT SO I'VE OPTED TO TAKE THE 4                 
* CHARACTER CODE FROM THE IDINFO RECORD IN C/GEN FOR CARAT WHICH IS             
* R7NY. RASHMI CONFIRMED THAT THIS IS AVAILABLE.                                
***                                                                             
         CLC   QAGY,=C'UB'            AGENCY UB?                                
         BNE   *+10                   NO                                        
         MVC   DSNAME+8(4),=C'R7NY'   UBNY IS ALREADY TAKEN                     
         DROP  R1,RF                  DROP MASTD/MCEXTRA USINGS                 
*                                                                               
         MVC   DSNAME+12(2),=C'.D'    SFTPDISK.PROD.BIL.SPT.D                   
         MVC   DSNAME+14(6),CTODAY+2  YYMMDD                                    
         MVC   DSNAME+20(2),=C'.T'    SFTPDISK.PROD.BIL.SPT.DYYMMDD.T           
         MVC   DSNAME+22(2),TIMEOFD   SFTPDISK.PROD.BIL.SPT.DYYMMDD.T..         
         MVC   DSNAME+24(2),TIMEOFD+3 SFTPDISK.PROD.BIL.SPT.DYYMMDD.T..         
         MVC   DSNAME+26(2),TIMEOFD+6 SFTPDISK.PROD.BIL.SPT.DYYMMDD.T..         
         MVC   MQMAPNM,=C'SFTPDISK.PROD.'                                       
         CLI   QOPT7,C'P'             PRODUCTION RUN?                           
         BE    REQF8                  YES                                       
         CLI   QOPT7,C'N'             SUPPRESS MQ NOTIFICATION?                 
         BNE   *+12                   NO                                        
         MVI   TESTMQ,C'N'            YES - SUPPRESS MQ NOTIFICATION            
         B     REQF7                  WRITE TO TEST MQ BROKER                   
         CLI   QOPT7,C'T'             WRITE TO TEST MQ BROKER?                  
         BNE   REQF8                  NO - TREAT AS PRODUCTION RUN              
         MVI   TESTMQ,C'T'            WRITE TO TEST MQ BROKER                   
*                                                                               
REQF7    MVC   MQMAPNM+9(4),=C'TEST'  WRITE TO SFTPDISK.TEST                    
*                                                                               
REQF8    MVI   BYTE,X'45'             BIG NAMES                                 
         MVC   DUB,=X'000005000001'                                             
         GOTO1 DYNALLOC,DMCB,(X'80',=C'SGTTAPE '),(BYTE,DUB),                   
               (X'80',MQMAPNM)                                                  
*                                                                               
         OPEN  (SGTTAPE,(OUTPUT))     OPEN THE TAPE                             
         LTR   RF,RF                  ANY ERRORS?                               
         BZ    *+6                    NO                                        
         DC    H'0'                   YES - DEATH                               
         MVI   GTOPENSW,C'Y'          SET GT FILE OPEN                          
         B     REQF16                                                           
*                                                                               
REQF09   GOTO1 VDOWNLD,DMCB,(RA)   INITIALIZE DOWNLOAD                          
         MVI   DOWNACT,C'Y'                                                     
*                                                                               
REQF16   DS    0H                                                               
         LA    R3,RQAMTS                                                        
         BAS   RE,CLRTOTS                                                       
         XC    LSTBLKY,LSTBLKY                                                  
*                                                                               
         ZAP   EATOTGRS,=P'0'                                                   
         ZAP   EATOTAC,=P'0'                                                    
         ZAP   EATOTACT,=P'0'                                                   
         ZAP   EATOTNET,=P'0'                                                   
*                                                                               
         XC    OFFICE,OFFICE       SET NO OFFICE                                
*                                  CLEAR SAVED TAPE REC AREA                    
         XC    SVBILL,SVBILL       AND SAVED BILL AREA                          
         LA    R3,SVBAMTS          AND SAVED BILL VALUES                        
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        FIRST FOR OFFICE                                                       
         SPACE 2                                                                
FBILO    DS    0H                                                               
*                                                                               
         XC    CLTINVS,CLTINVS     CLEAR CLIENT                                 
         XC    CLTZINV,CLTZINV     CLEAR CLIENT ZERO INV                        
         XC    OFFINVS,OFFINVS     AND OFFICE INVOICE TOTALS                    
*                                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'S'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,SVAGY                                                
         MVC   OFFD.OFCOFC,COFFICE-CLTHDR(RF)                                   
*                                                                               
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         L     RF,VOFFICER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(2,WORK),(0,ACOMFACS)                                  
         CLI   0(R1),0                                                          
         BNE   FBILOX                                                           
         MVC   OFFICE,OFFD.OFCOFC2                                              
*                                                                               
         DROP  OFFD                                                             
*                                                                               
FBILOX   MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 3                                                                
*        FIRST FOR CLI                                                          
         SPACE 2                                                                
FBILC    DS    0H                                                               
         BAS   RE,GETVEN                                                        
*                                                                               
*        FINISH FIXING BDE HEADERS  - FILE NAME                                 
*                                                                               
         MVC   BDEH4+32(13),SAVFILE                                             
*                                                                               
FBILC3G  DS    0H                                                               
*                                                                               
FBILC7B  DS    0H                                                               
         SR    R0,R0               SET INVOICE LIST BINSRCH PARS                
         L     R1,=A(INVTAB)                                                    
         SR    R2,R2                                                            
         LHI   R3,3                                                             
         LHI   R4,0                                                             
         L     R5,=A(INVMAX)                                                    
         STM   R0,R5,INVPARS                                                    
         XC    CLTINVS,CLTINVS                                                  
         XC    CLTZINV,CLTZINV                                                  
*                                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         XC    B1PROF,B1PROF       READ B1 PROFILE                              
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING PROFKD,RE                                                        
         MVI   PROFKSYS,C'S'                                                    
         MVC   PROFKPGM,=C'0B1'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLIENT                                                  
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
*    NOW ALWAYS PASS OFFICE DATA                                                
         MVI   PROFKOI2,C'*'                                                    
         MVC   PROFKOCD,0(RF)                                                   
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
*                                                                               
         XC    B1XPROF,B1XPROF     READ B1X PROFILE                             
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         MVI   PROFKSYS,C'S'-X'40' MAKE SYSTEM LOWER CASE                       
         MVC   PROFKPGM,=C'B1X'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLIENT                                                  
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
*    NOW ALWAYS PASS OFFICE DATA                                                
         MVI   PROFKOI2,C'*'                                                    
         MVC   PROFKOCD,0(RF)                                                   
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
         DROP  RE                                                               
*                                                                               
         CLI   PROGPROF+0,C'Y'     NEW PAGE PER CLIENT?                         
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                  SET CLIENT DATA IN SVMID                     
         MVI   NEWCLT,C'Y'                                                      
         MVC   SVMID,SPACES                                                     
         MVC   SVMID(7),=C'CLIENT='                                             
         MVC   SVMID+8(3),CLT                                                   
         MVC   SVMID+12(24),CLTNM                                               
*                                                                               
         LA    RF,SVMID+36                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         L     RE,ADCLT                                                         
         CLI   CCLTIFC-CLTHDR(RE),C' '                                          
         BNH   FBILC9                                                           
         MVI   2(RF),C'('                                                       
         MVC   3(8,RF),CCLTIFC-CLTHDR(RE)                                       
         LA    RF,11(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
*                                                                               
FBILC9   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
*        FIRST FOR PRODUCT                                                      
         SPACE 2                                                                
FBILP    DS    0H                                                               
         LA    R3,PAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         CLI   PROGPROF+1,C'Y'     NEW PAGE PER PRODUCT?                        
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        LAST FOR PRODUCT                                                       
         SPACE 2                                                                
LBILP    DS    0H                                                               
         LA    R3,PAMTS                                                         
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,PAMTS                                                         
         LA    R4,CAMTS                                                         
         BAS   RE,PBROLL                                                        
*                                                                               
         BRAS  RE,PRNT                                                          
         MVC   P+10(20),=C'** PRODUCT TOTALS **'                                
         LA    R3,PAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST  FOR CLIENT                                                       
         SPACE 2                                                                
LBILC    DS    0H                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
*                                                                               
LBILC5   LA    R3,CAMTS                                                         
         LA    R4,OAMTS            ROLL TO OFFICE TOTS                          
         CLI   QCLT,C'$'           IF IN OFFICE LIST MODE                       
         BE    *+8                                                              
         LA    R4,RQAMTS           ELSE TO REQUEST TOTS                         
         BAS   RE,PBROLL                                                        
*                                                                               
         MVC   CLTINVS,INVPARS+8   INVOICE COUNT                                
*                                                                               
         L     R0,CLTINVS                                                       
         L     RE,CLTZINV          CLIENT ZERO INVOICE COUNT                    
         SR    R0,RE               ADJUST CLIENT INVOICE COUNT                  
         ST    R0,CLTINVS                                                       
*                                                                               
         L     R0,OFFINVS                                                       
         LA    RE,OFFINVS                                                       
         CLI   QCLT,C'$'           IF IN OFFICE LIST MODE                       
         BE    *+12                                                             
         L     R0,REQINVS                                                       
         LA    RE,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,0(RE)                                                         
*                                                                               
         BRAS  RE,PRNT                                                          
         MVC   P+10(19),=C'** CLIENT TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  CLTINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         XC    CLTINVS,CLTINVS                                                  
         B     EXIT                                                             
*                                                                               
LBILO    DS    0H                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,OAMTS                                                         
         LA    R4,RQAMTS                                                        
         BAS   RE,PBROLL                                                        
*                                                                               
         L     R0,REQINVS                                                       
         A     R0,OFFINVS                                                       
         ST    R0,REQINVS                                                       
*                                                                               
         BRAS  RE,PRNT                                                          
         MVC   P+10(19),=C'** OFFICE TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  OFFINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         XC    OFFINVS,OFFINVS                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST FOR REQ                                                           
REQL     DS    0H                                                               
         L     RF,ADBILL           CLEAR BILL RECORD                            
         XC    0(L'BILLREC,RF),0(RF)                                            
*                                                                               
         LA    R3,RQAMTS                                                        
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,RQAMTS                                                        
         LA    R4,RAMTS                                                         
         BAS   RE,PBROLL                                                        
*                                                                               
         L     R0,RUNINVS                                                       
         A     R0,REQINVS                                                       
         ST    R0,RUNINVS                                                       
*                                                                               
         MVC   P+10(20),=C'** REQUEST TOTALS **'                                
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  REQINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,RQAMTS                                                        
         BAS   RE,TOTPRNT                                                       
         XC    REQINVS,REQINVS                                                  
*                                                                               
         L     R2,PPGRECNT                                                      
         LTR   R2,R2                                                            
         BZ    LREQC5                                                           
*                                                                               
*        I HAVE SOMETHING TO SEND                                               
*                                                                               
         CLI   BDEHSW,1           BDE HEADERS ALREADY SENT?                     
         BE    LREQC3                                                           
*                                                                               
         MVC   P,BDEH1                                                          
         MVC   P2,BDEH2                                                         
         MVC   P3,BDEH3                                                         
         MVC   P4,BDEH4                                                         
*                                                                               
         MVC   SVLINE,LINE                                                      
         MVC   SVFORCEH,FORCEHED                                                
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
         MVC   LINE,SVLINE             RESTORE LINE                             
         MVC   FORCEHED,SVFORCEH       AND FORCEHED                             
         MVI   RCWHATPR,1     RESET TO FIRST                                    
         MVI   BDEHSW,1           SET BDE HEADERS SENT                          
*                                                                               
LREQC3   CLC   QAGY,=C'OO'        AGENCY OO?                                    
         BE    *+10               YES                                           
         CLC   QAGY,=C'OU'        AGENCY OU?                                    
         BNE   LREQC3A            NO                                            
         TM    ERRORSW,X'02'      MISSING INTERNAL ORDER NUMBER?                
         BNZ   EXIT               YES - FILE WILL NOT BE PRODUCED               
         TM    ERRORSW,X'04'      MISSING LEGAL ENTITIES?                       
         BNZ   EXIT               YES - FILE WILL NOT BE PRODUCED               
*                                                                               
LREQC3A  L     R3,AOFPPGT         SEND ENTRIES TO DOWNLOAD                      
         LA    R3,L'PPGKEY(R3)    PAST PSUEDO KEY                               
*                                                                               
LREQC4   CLI   GTSFTP,C'Y'        SENDING GT FILE VIA THE HUB?                  
         BNE   LREQC4A            NO                                            
         GOTO1 VFTPHUB,DMCB,(RA),(R3)                                           
         B     LREQC4B            BUMP TO NEXT ENTRY                            
*                                                                               
LREQC4A  GOTO1 VDOWNLD,DMCB,(RA),(R3)                                           
*                                                                               
LREQC4B  LA    R3,PPGTLEN(R3)      NEXT ENTRY                                   
         BCT   R2,LREQC4                                                        
*                                                                               
LREQC5   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(L'SPACES),SPACES                                               
         MVC   P+10(19),=C'** REPORT TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  RUNINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,RAMTS                                                         
         BAS   RE,TOTPRNT                                                       
*                                                                               
         CLI   ERRORSW,0        ANY ERRORS?                                     
         BE    RUNL10                                                           
         MVC   P+1(36),=C'*** ERRORS HAVE BEEN ENCOUNTERED ***'                 
         BRAS  RE,PRNT                                                          
         TM    ERRORSW,X'01'     ANY MISSING ACCOUNT NUMBERS?                   
         BNO   RUNL1                                                            
         MVC   P+3(31),=C'*** MISSING ACCOUNT NUMBERS ***'                      
         BRAS  RE,PRNT                                                          
RUNL1    TM    ERRORSW,X'20'     ANY ESTIMATE USER?                             
         BNO   RUNL3                                                            
         MVC   P+3(37),=C'** WARNING ** MISSING ESTIMATE USER 1'                
         BRAS  RE,PRNT                                                          
RUNL3    TM    ERRORSW,X'02'     ANY MISSING INTERNAL ORDER NOS.                
         BNO   RUNL4                                                            
         MVC   P+3(35),=C'*** MISSING INTERNAL ORDER NOS. ***'                  
         BRAS  RE,PRNT                                                          
RUNL4    TM    ERRORSW,X'04'     ANY MISSING LEGAL ENTITIES                     
         BNO   RUNL5                                                            
         MVC   P+3(30),=C'*** MISSING LEGAL ENTITIES ***'                       
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL5    TM    ERRORSW,X'08'   MIXED LEGAL ENTITES ON AN INVOICE                
         BNO   RUNL6                                                            
         MVC   P+3(42),=C'*** INVOICES WITH MIXED LEGAL ENTITIES ***'           
         BRAS  RE,PRNT                                                          
RUNL6    TM    ERRORSW,X'80'   MISSING POL ESTIMATE                             
         BNO   RUNL8                                                            
         MVC   P+3(29),=C'*** MISSING POL ESTIMATES ***'                        
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL8    TM    ERRORSW,X'40'   UNKNOWN MEDIA TYPE?                              
         BNO   RUNL8A          NO                                               
         MVC   P+3(27),=C'*** UNKNOWN MEDIA TYPE  ***'                          
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL8A   DS    0H                                                               
*&&DO                                                                           
RUNL8A   CLC   QAGY,=C'OO'      AGENCY OO?                                      
         BE    *+10             YES                                             
         CLC   QAGY,=C'OU'      AGENCY OU?                                      
         BNE   RUNL9            NO                                              
         TM    ERRORSW,X'02'    MISSING INTERNAL ORDER NUMBER?                  
         BNZ   *+8              YES - FILE WILL NOT BE PRODUCED                 
         TM    ERRORSW,X'04'    MISSING LEGAL ENTITIES?                         
         BZ    RUNL9            NO                                              
         MVC   P+1(37),=C'**WARNING -OUTPUT FILE NOT PRODUCED**'                
         BRAS  RE,PRNT                                                          
         B     EXIT             DONE                                            
*&&                                                                             
RUNL9    MVC   P+1(35),=C'**WARNING -OUTPUT FILE HAS ERRORS**'                  
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL10   CLI   GTSFTP,C'Y'         SENDING GT FILE VIA THE HUB?                 
         BNE   RUNL11              NO                                           
         GOTO1 VFTPHUB,DMCB,(RA)   CLOSE FILE AND SEND MQ MESSAGE               
         B     EXIT                DONE                                         
*                                                                               
RUNL11   CLI   DOWNACT,C'Y'                                                     
         BNE   EXIT                                                             
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         B     EXIT                                                             
         EJECT                                                                  
*        PROCESS BILL  **NOTE- BILL RECORD NOT READ YET**                       
         SPACE 2                                                                
PRBL     DS    0H                                                               
         MVI   SKIPBILL,0                                                       
         CLI   KEY+BKEYEST-BKEY,0  SKIP EST 0 BILLS (BILLING BUG)               
         BE    EXIT                                                             
         CLC   KEY(10),LSTBLKY     IF FIRST FOR EST/MOS                         
         BE    PRB1                                                             
         MVC   SVKEY,KEY           MUST SAVE KEY AND KEYSAVE                    
         CLI   GTSFTP,C'Y'         AGENCY UB/OO/OU?                             
         BNE   PRBA                NO                                           
         BAS   RE,READUCOM         YES - READ PRD OR EST UCOMM RECORD           
         MVC   KEY(64),SVKEY       RESTORE                                      
         B     PRB0                GO PROCESS RECORD                            
*                                                                               
PRBA     BAS   RE,PUGTPOLE         MUST READ POL EST                            
         MVC   KEY(64),SVKEY       RESTORE                                      
         BE    PRB0                                                             
         OI    ERRORSW,X'80'                                                    
         MVC   P+3(32),=C'*** POL ESTIMATE NOT ON FILE ***'                     
         BRAS  RE,PRNT                                                          
*                                  TO USE ITS USER FIELDS                       
PRB0     GOTO1 HIGH                RESTORE SEQ READ                             
*                                                                               
         MVI   REVSW,C' '          SET NOT A REVISION                           
*                                                                               
         ZAP   EATOTGRS,=P'0'                                                   
         ZAP   EATOTAC,=P'0'                                                    
         ZAP   EATOTACT,=P'0'                                                   
         ZAP   EATOTNET,=P'0'                                                   
         B     PRB1D                                                            
*                                                                               
PRB1     DS    0H                                                               
         MVI   REVSW,C'R'          SET IS A REVISION                            
*                                                                               
PRB1D    DS    0H                                                               
*                                                                               
PRB1H    DS    0H                                                               
         MVC   LSTBLKY,KEY                                                      
         TM    CONTROL1,X'80'      TEST NEED 'ESTIMATE' AMOUNTS                 
         BNZ   PRB3                YES- MUST READ ALL BILLS                     
*                                                                               
         CLC   KEY+BKEYYSRV-BKEY(2),STARTMOS  MONTH-OF-SERVICE FILTERS          
         BL    EXIT                                                             
         CLC   KEY+BKEYYSRV-BKEY(2),ENDMOS                                      
         BH    EXIT                                                             
*                                                                               
         CLI   QOPT4,C'M'          TEST MOS FILTERING                           
         BNE   PRB2D                                                            
         CLC   KEY+BKEYYSRV-BKEY(2),BQSTART                                     
         BL    EXIT                                                             
         CLC   KEY+BKEYYSRV-BKEY(2),BQEND                                       
         BH    EXIT                                                             
         B     PRB3                                                             
*                                                                               
PRB2D    DS    0H                  RUN DATE FILTERING                           
         ZIC   R3,KEY+BKEYMBIL-BKEY                                             
         SRL   R3,4                YEAR DIGIT OF BILL                           
         ZIC   RE,DECADE                                                        
         CLM   R3,1,YEARDIG        COMPARE TO YEAR OF TODAY                     
         BNH   *+8                 IF NOT HIGH, OK                              
         SH    RE,=H'10'           ELSE BACK UP TO PREV DECADE                  
         AR    RE,R3                                                            
         STC   RE,FULL             CALCULATED YEAR OF BILL                      
*                                                                               
         MVC   FULL+1(1),KEY+BKEYMBIL-BKEY                                      
         NI    FULL+1,X'FF'-X'F0'  ISOLATE MONTH                                
*                                                                               
         CLC   FULL(2),BQSTART                                                  
         BL    EXIT                                                             
         CLC   FULL(2),BQEND                                                    
         BH    EXIT                                                             
*                                                                               
PRB3     DS    0H                                                               
         GOTO1 GETBILL                                                          
         L     R2,ADBILL                                                        
         USING BILLREC,R2                                                       
*                                                                               
         CLI   QOPT5+1,C'*'        TEST NETPAK SUB MED FILT                     
         BE    PRB4                NO                                           
         CLI   QOPT5+1,C' '                                                     
         BNH   PRB4                NO                                           
         MVC   BYTE,BLMED                                                       
         CLI   BYTE,C' '           IF NO SUB MEDIA                              
         BH    *+8                                                              
         MVI   BYTE,C'N'           DEFAULT TO N                                 
         CLC   BYTE,QOPT5+1        TEST RIGHT SUB-MED                           
         BNE   EXIT                                                             
*                                                                               
PRB4     DS    0H                                                               
         TM    BILSTAT,BSTCMONQ    TEST COMMISSION ONLY BILL                    
         BNZ   *+12                YES, OK                                      
         CLI   QOPT5,C'C'          NO, TEST TO SKIP OTHERS                      
         BE    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'A'          AOR BILLS ONLY?                              
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BZ    EXIT                                                             
         CLI   QOPT5,C'B'          AOR AND AOR/CLIENT BILLS                     
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ+BSTCAORQ                                        
         BZ    EXIT                                                             
         CLI   QOPT5,C'X'          NON-AOR BILLS ONLY?                          
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BNZ   EXIT                                                             
         CLC   =C'*SOON',QUESTOR   IF WAS AUTOREQUESTED BY SOON                 
         BNE   PB6                                                              
         TM    BILSTAT3,BSTSOONQ   WAS REC GENERATED BY SOON ?                  
         BZ    EXIT                                                             
         CLC   BILLUID,RCORIGID    PROCESS ONLY FOR REQUESTING USER ID          
         BNE   EXIT                                                             
*                                  NB- POST TO 'ESTIMATE' AMTS                  
*                                  WHETHER PASSES DATE FILTERS OR NOT           
PB6      DS    0H                  SET BILL AMOUNTS                             
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
*                                                                               
         LA    R3,BAMTS            CLEAR BILL AMOUNTS                           
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         USING AMOUNTSD,R3                                                      
         ZAP   AMTGRS,SPBVGRSP     EFFECTIVE GROSS                              
         ZAP   AMTACT,SPBVACTP     ACTUAL                                       
         ZAP   AMTNET,SPBVNETP     EFFECTIVE NET                                
*                                                                               
         L     R0,SPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTGST,DUB          GST                                          
         L     R0,SPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTPST,DUB          PST'S (INCL HSTS)                            
         L     R0,SPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTHST,DUB          HST'S ALONE                                  
*                                                                               
         CLI   QOPT5,C'2'          IF COS2 REQUEST                              
         BE    *+12                                                             
         CLI   QOPT5,C'P'          (PW = COS2)                                  
         BNE   PB6C4                                                            
*                                                                               
         TM    BILSTAT2,BSTC2Q     AND COS2 BILL, OK                            
         BZ    EXIT                ELSE, SKIP                                   
*                                                                               
         ZAP   AMTGRS,BGRS2P       USE COS2 GROSS                               
         ZAP   AMTNET,BNET2P       NET                                          
         ZAP   AMTACT,BACT2P       AND ACTUAL                                   
*                                                                               
PB6C4    DS    0H                                                               
         ZAP   DUB,AMTACT                                                       
         TM    BILSTAT,BSTCMONQ    IF COMM ONLY BILL                            
         BZ    *+18                                                             
         CLI   QOPT5,C'C'          TEST LEAVE NET                               
         BE    *+10                YES                                          
         ZAP   AMTNET,=P'0'        ELSE CLEAR NET (AC = RCVBL)                  
*                                                                               
         TM    BILSTAT,BSTTAORQ    FOR AOR BILLS                                
         BZ    *+10                'NET' IS ORIGINAL BILLS COMMISION            
         ZAP   AMTNET,=P'0'        SO SIT MUST BE CLEARED HERE                  
*                                                                               
         SP    DUB,AMTNET                                                       
         ZAP   AMTAC,DUB           AC                                           
*                                                                               
         AP    EATOTGRS,AMTGRS     ADD TO EATOTS                                
         AP    EATOTAC,AMTAC                                                    
         AP    EATOTACT,AMTACT                                                  
         AP    EATOTNET,AMTNET                                                  
*                                                                               
         CLI   QOPT4,C'M'          UNLESS FILTERING ON MOS                      
         BE    PB8                                                              
         CLC   BDATE,SVQST         TEST BILL WITHIN REQ PERIOD                  
         BL    EXIT                                                             
         CLC   BDATE,SVQEND                                                     
         BH    EXIT                                                             
*                                                                               
PB8      DS    0H                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   BRETAIL,0                                                        
         BE    PB30                                                             
*                                  RETAIL BILLS                                 
         CLI   BRETAIL,X'81'       SKIP ALL CORP CONTROL                        
         BE    EXIT                                                             
*                                                                               
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                SKIP ZERO BILLS                              
*                                                                               
         MVI   RETAIL,C'Y'                                                      
*                                  BUILD PRINT LINE                             
PB30     DS    0H                                                               
*        FINALLY - PROCESS THIS BILL                                            
*                                  USE BINSRCH TO ADD TO INVTAB                 
         MVC   WORK(1),KEY+BKEYMBIL-BKEY     BILL MONTH                         
         MVC   WORK+1(2),KEY+BKEYINV-BKEY    AND INVOICE NUMBER                 
         GOTO1 BINSRCH,INVPARS,(1,WORK)                                         
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   NEWCLT,C'Y'         IF NEW CLIENT                                
         BNE   PB30B                                                            
         MVC   P,SVMID             PRINT CLIENT DATA                            
         MVI   ALLOWLIN,7                                                       
         MVI   NEWCLT,C'N'                                                      
         MVI   SPACING,2                                                        
         BRAS  RE,PRNT                                                          
*                                                                               
PB30B    DS    0H                                                               
         LHI   RF,1                SET BILL COUNT                               
         ST    RF,AMTCNT                                                        
         LA    R7,P                                                             
         USING BLINED,R7                                                        
*                                                                               
         CLI   QOPT5+1,C' '        TEST TO SHOW NETPAK SUB-MED                  
         BNH   PB31                                                             
         CLI   BLMED,C' '          TEST ANY                                     
         BNH   PB31                                                             
         MVI   P,C'*'                                                           
         MVC   P+1(1),BLMED                                                     
*                                                                               
PB31     DS    0H                                                               
         MVC   BLPRD,BKEYPRD       PRD                                          
*                                                                               
         L     RF,ADPRD            PRD NUMBER                                   
         LA    RF,PACCT-PRDHDR(RF)                                              
         MVC   BLPNUM+1(4),0(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *+20                                                             
         ZAP   DUB,1(3,RF)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  BLPNUM,DUB                                                       
*                                                                               
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BLEST,DUB                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,BKEYYSRV),(6,BLPER)                               
         CLI   BKEYYSRV+1,12       SPECIAL PERIOD                               
         BNH   PB33                                                             
         ZIC   R1,BKEYYSRV+1                                                    
         EDIT  (R1),(2,BLPER+1)                                                 
         MVI   BLPER,C' '                                                       
*                                                                               
PB33     DS    0H                                                               
         GOTO1 DATCON,DMCB,BDATE,(5,BLRUND)                                     
         GOTO1 DATCON,DMCB,BQDATE,(5,BLINVD)                                    
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(5,BLDUED)                              
*                                                                               
         MVC   BLTYPE(2),BTYPE     BILLING TYPE                                 
         MVI   BLTYPE+2,C' '                                                    
         TM    BILSTAT,BSTSCOMQ    IF UPFRONT COMM                              
         BZ    *+8                                                              
         MVI   BLTYPE,C'U'         U4-U7 = UPFRONT                              
         TM    BILSTAT,BSTSNETQ    IF NET BILL (AFTER UPFRONT)                  
         BZ    *+8                                                              
         MVI   BLTYPE,C'N'         N4-N7 = UPFRONT                              
         TM    BILSTAT,BSTMANQ     MANUAL?                                      
         BZ    *+10                                                             
         MVC   BLTYPE,=C'MAN'                                                   
         TM    BILSTAT,BSTTAORQ    AOR?                                         
         BZ    *+10                                                             
         MVC   BLTYPE,=C'AOR'                                                   
*                                                                               
PB33D    DS    0H                                                               
         MVC   DINVNO,SPACES                                                    
         L     RF,ADCONLST                                                      
         L     RF,VSPFMINO-SPADCONS(,RF)   A(SPFMTINO)                          
*                                                                               
         LA    R1,B1PROF           B1 PROFILE                                   
         ST    R1,DMCB+8           PARM 3                                       
         MVC   DMCB+8(1),QMED      MEDIA IS HOB OF PARM 4                       
         CLI   NETPAKSW,C'N'       NETPAK?                                      
         BE    *+10                NO                                           
         MVC   DMCB+8(1),BLMED     YES - MEDIA IS HOB OF PARM 4                 
         LA    R1,B1XPROF          B1X PROFILE                                  
         ST    R1,DMCB+12          PARM 4                                       
         ST    R2,DMCB+16          A(BILL HEADER RECORD)                        
*                                                                               
         GOTO1 (RF),DMCB,(C'B',BDATE),BINVNO+2                                  
***      GOTO1 (RF),DMCB,BDATE,BINVNO+2,(QMED,B1PROF),B1XPROF                   
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)      FULL FORMAT INVOICE NUMBER                   
         L     RF,DMCB+4           FORMAT MN-NNNN (SHORT INVOICE NO.)           
         LA    RE,DINVNO                                                        
         LHI   R0,7                                                             
*                                                                               
PB34     DS    0H                                                               
         CLI   0(RF),C'-'                                                       
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PB34                                                          
*                                                                               
         MVC   BLINVNO(2),DINVNO                                                
         MVI   BLINVNO+2,C'-'                                                   
         MVC   BLINVNO+3(4),DINVNO+2                                            
*                                                                               
         CLI   CNTRY,C'C'          IF CANADA                                    
         BNE   PB34B                                                            
*                                                                               
         ZAP   DOUBLE,AMTACT       ACTUAL                                       
         AP    DOUBLE,AMTGST       PLUS GST                                     
         AP    DOUBLE,AMTPST       PLUS PST (INCL HST)                          
         EDIT  (P8,DOUBLE),BLACTUAL,2,COMMAS=YES,MINUS=YES                      
*                                                                               
         EDIT  AMTGRS,BLGROSS,2,COMMAS=YES,MINUS=YES                            
         EDIT  AMTNET,BLNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  AMTAC,BLAC,2,COMMAS=YES,MINUS=YES                                
         EDIT  AMTGST,BLGST,2,COMMAS=YES,MINUS=YES                              
*                                                                               
         CP    AMTPST,=P'0'        PST ((INCL HST)                              
         BE    PB34D                                                            
         EDIT  AMTPST,BLPST,2,COMMAS=YES,MINUS=YES                              
         B     PB34D                                                            
*                                                                               
PB34B    DS    0H                  NON-CANADIAN GETS WIDER COLUMNS              
         EDIT  AMTGRS,BLGRSWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTACT,BLACTWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTNET,BLNETWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTAC,BLACWIDE,2,COMMAS=YES,MINUS=YES                            
*                                                                               
PB34D    DS    0H                                                               
         CLC   QAGY,=C'OO'         OMDUSEC (USA)                                
         BE    *+14                                                             
         CLC   QAGY,=C'OU'         OMDTOA                                       
         BNE   PB34F                                                            
         CP    AMTACT,=P'0'        ZERO DOLLAR INVOICE?                         
         BNE   PB34F                                                            
*                                                                               
         L     R0,CLTZINV                                                       
         A     R0,=F'1'                                                         
         ST    R0,CLTZINV                                                       
         MVC   BLINE,SPACES                                                     
         B     EXIT                                                             
*                                                                               
PB34F    BRAS  RE,PRNT                                                          
         LA    R4,PAMTS            ROLL TO PRODUCT TOTALS                       
         BAS   RE,PBROLL                                                        
         DROP  R3                                                               
*                                                                               
PS40     DS    0H                                                               
         BAS   RE,POSTB           POST TO TABLE                                 
*                                                                               
PS900    DS    0H                                                               
         L     RF,ADBILL           SAVE THIS BILL RECORD                        
         MVC   SVBILL,0(RF)                                                     
*                                                                               
         LA    RE,SVBAMTS          AND BILL VALUES                              
         LA    RF,BAMTS                                                         
         LHI   R0,NAMTS                                                         
         ZAP   0(6,RE),0(6,RF)                                                  
         LA    RE,6(RE)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,*-14                                                          
*                                                                               
PSX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        PUGTPOLE - PRTUSER, GET POL ESTIMATE HEADER                            
*        READ INTO ADBUY                                                        
*                                                                               
PUGTPOLE NTR1                                                                   
*                                                                               
         XC    UCOMDATA,UCOMDATA   CLEAR UCOMM DATA                             
*                                                                               
         L     RF,ADEST                                                         
         MVC   KEY,0(RF)                                                        
         L     R7,ADBUY                                                         
         ST    R7,AREC                                                          
         MVC   KEY+EKEYPRD-ESTHDR(3),=C'POL'                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NEQXIT                                                           
         GOTO1 GET                                                              
         CLI   DMCB+8,0                                                         
         BNE   NEQXIT                                                           
*                                                                               
*        CALL DDUCOM TO GET PRD POL'S ESTIMATE'S FIRST UCOMM                    
*                                                                               
         MVC   USAVKEY,KEY   SAVE MY KEY                                        
         LA    R5,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R5                                                       
*                                                                               
         MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         MVC   UCSAM,BAGYMD      AGENCY/MEDIA                                   
         MVC   UCSCLT,BCLT       PACKED CLIENT                                  
         MVC   UCPRD,=C'POL'  **TEMPORARY USE OF POL**                          
*                             **UNTIL SPOT SFM ALLOWS AAA**                     
*                                DO UCOMM FOR PRD AAA                           
         OI    UCOPT,UCOEST     RETURN ESTIMATE UCOMMS                          
         L     RF,ADBUY            REALLY POL ESTIMATE                          
         MVC   UCSEST,EKEYEST-ESTHDR(RF)                                        
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK    NEW UCOM CALL SINCE GOTO MACRO                
         CLI   UCERROR,0         TRASHED WRKING STORAGE USED BY DDUCOM          
         BNE   PGETPOLX     ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BO    PGETPOLX                                                         
         XC    UCTTLS(UCALL),UCTTLS                                             
         L     R4,UCETTLS     EST TITLES                                        
         MVC   UCTTLS,0(R4)   SAVE INFO IN MY STORAGE                           
         LA    R4,UCTTLS      AS OPPOSED TO RD CHANE                            
         L     R7,UCEDATA     EST DATA                                          
         MVC   UCOMDATA,0(R7)                                                   
         DROP  R5                                                               
*                                                                               
PGETPOLX DS    0H                                                               
         B     EQXIT                                                            
*                                                                               
NEQXIT   LTR   RD,RD               EXIT WITH CC NOT =                           
         B     *+6                                                              
EQXIT    CR    RD,RD               EXIT WITH CC =                               
         XIT1                                                                   
*                                                                               
READUCOM NTR1                                                                   
*                                                                               
         XC    UCOMDATA,UCOMDATA   CLEAR UCOMM DATA                             
*                                                                               
         LA    R5,UCOMBLK          UCOMM CONTROL BLOCK                          
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM BLOCK                            
         USING DDUCOMD,R5          UCOMM DSECT                                  
*                                                                               
         MVC   UCACOMF,ACOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'S'          SPOT SYSTEM                                  
         CLI   NETPAKSW,C'Y'       NET SYSTEM?                                  
         BNE   *+8                 NO                                           
         MVI   UCSYS,C'N'          NET SYSTEM                                   
         L     RF,ADEST            RESET RF TO ESTIMATE                         
         USING ESTHDRD,RF          ESTIMATE DSECT                               
         MVC   UCSAM,EKEYAM        AGENCY/MEDIA                                 
         MVC   UCSCLT,EKEYCLT      PACKED CLIENT                                
         MVC   UCPRD,EKEYPRD       PRODUCT                                      
         CLC   QAGY,=C'UB'         AGENCY UB?                                   
         BE    *+12                YES - UB GETS ESTIMATE UCOMMS                
         OI    UCOPT,UCOPRD        RETURN PRODUCT UCOMMS FOR OO/OU              
         B     *+14                CALL DDUCOM                                  
         MVC   UCSEST,EKEYEST      SET ESTIMATE                                 
         OI    UCOPT,UCOEST        RETURN ESTIMATE UCOMMS                       
         DROP  RF                  DROP ESTIMATE DSECT USING                    
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK     CALL DDUCOM                                  
*                                                                               
         CLI   UCERROR,0           ANY ERRORS?                                  
         BNE   RDUCOMX             YES - HANDLE THEM LATER BASED ON AGY         
*                                                                               
         CLC   QAGY,=C'UB'         AGENCY UB?                                   
         BNE   RDUCOM10            NO                                           
         TM    UCDATA,UCDNOEST     YES - DID WE GET THE EST UCOMMS?             
         BO    RDUCOMX             NO - EXIT                                    
         L     R4,UCEDATA          EST DATA (FOR AGENCY UB)                     
         B     RDUCOM20            YES - SAVE TO UCOMDATA                       
*                                                                               
RDUCOM10 TM    UCDATA,UCDNOPRD     YES - DID WE GET THE PRD UCOMMS?             
         BO    RDUCOMX             NO - EXIT                                    
         L     R4,UCPDATA          PRD DATA (FOR AGENCIES OO & OU)              
*                                                                               
RDUCOM20 MVC   UCOMDATA,0(R4)      PRD OR EST UCOMM DATA                        
         DROP  R5                  DROP UCOMM DSECT USING                       
*                                                                               
RDUCOMX  J     EXIT                RETURN                                       
*                                                                               
POSTB    NTR1                                                                   
         LA     R3,BAMTS                                                        
         USING  AMOUNTSD,R3                                                     
         LA     R4,PPGREC                                                       
         XC     0(PPGRECL,R4),0(R4)    CLEAR IT                                 
         USING  PPGRECD,R4                                                      
         XC     PPGKEY,PPGKEY    BE SURE KEY IS CLEAR                           
         L      RF,ADEST                                                        
         L      R2,ADBILL                                                       
*******  L      R5,ADBUY         POL ESTIMATE                                   
         L      R5,ADEST         ESUER1 NOW FROM BRAND EST                      
*                                                                               
         USING  BILLREC,R2                                                      
*                                                                               
         CLI    GTSFTP,C'Y'       AGENCY UB/OO/OU?                              
         BE     POSTB0            YES - DO NOT FLAG BARTER OR AOR BILL          
         CLI    SAVFMT,C'N'       SEE IF NEW FORMAT                             
         BNE    POSTB0                                                          
*                                 CHECK FRIST EST FILTER                        
         CLI    EPROF-ESTHDR(RF),C'B' SEE IF A BARTER ESTIMATE                  
         BNE    *+12                                                            
         MVI    PPGKEBRT,C'B'                                                   
         MVI    PPGEBRT,C'B'                                                    
         TM     BILSTAT,BSTTAORQ     SEE IF AOR BILL                            
         BZ     *+12                                                            
         MVI    PPGKAOR,C'A'                                                    
         MVI    PPGAOR,C'A'                                                     
*                                                                               
POSTB0   DS     0H                                                              
         MVC    PPGIDATE,BQDATE       DATE PRINTED ON BILL                      
         CLI    PPGIDATE,0            IS THERE ONE?                             
         BNE    *+10                                                            
         MVC    PPGIDATE,BDATE      ELSE USE RUN DATE                           
*                                                                               
         MVC    PPGINVF,DINVFULL      FULL INVOICE NUMBER                       
         MVC    PPGMOS,BKEYYSRV       MOS YR+MN                                 
         MVC    PPGEU2,EUSER2-ESTHDR(RF)    BRAND EST                           
         MVC    PPGEU1,EUSER1-ESTHDR(R5) WAS POL - NOW ALSO FROM BRAND          
         OC     PPGEU1,SPACES                                                   
         OC     PPGEU2,SPACES                                                   
*                                                                               
         CLC    QAGY,=C'OO'     AGENCY OO?                                      
         BE     POSTC           YES - FLAG IF ESTIMATE UDEF 1 MISSING           
         CLC    QAGY,=C'OU'     AGENCY OU?                                      
         BE     POSTC           YES - FLAG IF ESTIMATE UDEF 1 MISSING           
         CLC    QAGY,=C'UB'     AGENCY UB?                                      
         BE     POST2           YES - UB DOESN'T NEED THIS FIELD                
         CLI    PPGEU1,C'X'     IF BEGINS WITH X - CONSIDER MISSING             
         BE     POST1                                                           
POSTC    CLC    PPGEU1,SPACES                                                   
         BH     POST2                                                           
         B      POST1E                                                          
*                                                                               
POST1    MVC    P2+9(7),=C'VALUE ='                                             
         MVC    P2+16(8),PPGEU1                                                 
*                                                                               
POST1E   MVC    P+1(37),=C'** WARNING ** MISSING ESTIMATE USER 1'               
         CLC    QAGY,=C'OO'     AGENCY OO?                                      
         BE     POST1F          YES - FLAG IF ESTIMATE UDEF 1 MISSING           
         CLC    QAGY,=C'OU'     AGENCY OU?                                      
         BE     POST1F          YES - FLAG IF ESTIMATE UDEF 1 MISSING           
         MVC    P+1(37),=C'**ERROR-MISSING ACCOUNT NO. EST USER1'               
         OI     ERRORSW,X'01'                                                   
         B      POST1G                                                          
*                                                                               
POST1F   OI     ERRORSW,X'20'                                                   
*                                                                               
POST1G   BRAS   RE,PRNT                                                         
*                                                                               
POST2    DS     0H                                                              
*                                                                               
         CLI    GTSFTP,C'Y'       AGENCY UB/OO/OU?                              
         BE     POST3             YES                                           
*                                                                               
         CLI    NETPAKSW,C'Y'     NETPAK?                                       
         BNE    POST2B                                                          
         CLC    PPGEU1(8),=C'14080026'  SPECIAL ACCOUNT?                        
         BE     POST2BX       GO CLEAR INTERNAL ORDER#                          
*                                                                               
         CLC    BKEYPRD,=C'SUP'   IS IT SUPPLEMENTAL?                           
         BNE    POST2D                                                          
         MVC    PPGEU1,SPACES                                                   
         MVC    PPGEU2,SPACES       CLEAR INTERNAL ORDER NO.                    
         MVC    PPGEU1(08),=C'14060013'   SPECIAL ACCOUNT CODE                  
         B      POST3                                                           
*                                                                               
POST2B   DS     0H               HERE IF SPOT                                   
         CLC    QAGY,=C'H9'      MEDIAVEST                                      
         BNE    POST2D                                                          
         CLC    QCLT,=C'PG1'     MEDIAVEST CLIENT CODE                          
         BNE    POST2D                                                          
*                      SOAP (SOA) WILL HAVE SPECIAL ACCOUNT NUMBER              
         CLC    PPGEU1(8),=C'14080026'     SPECIAL ACCOUNT CODE                 
         BNE    POST2D                                                          
POST2BX  MVC    PPGEU2,SPACES     CLEAR INTERNAL ORDER NO.                      
         B      POST3                                                           
*                                                                               
POST2D   CLC    PPGEU2,SPACES                                                   
         BH     POST3                                                           
*                                                                               
         CLI    SAVFMT,C'N'       SEE IF DOING NEW FORMAT                       
         BE     POST3                                                           
*                                                                               
         MVC    P+1(42),=C'**ERROR-MISSING INTERNAL ORD NO. EST USER2'          
         OI     ERRORSW,X'02'                                                   
         BRAS   RE,PRNT                                                         
*                                                                               
POST3    DS     0H                                                              
         MVC    PPGENAME,EDESC-ESTHDR(RF)                                       
         L      RF,ADPRD                                                        
         MVC    PPGPU1,PUSER1-PRDHDR(RF)    LEGAL ENTITY OR IO#                 
         MVC    PPGPU2,PUSER2-PRDHDR(RF)    LEGAL ENTITY MEDIACOM               
*                                                                               
         CLC    QAGY,=C'UB'       AGENCY UB?                                    
         BE     POST4             YES - UB DOESN'T CARE IF MISSING              
         CLC    QAGY,=C'OO'       AGENCY OO?                                    
         BE     *+10              YES                                           
         CLC    QAGY,=C'OU'       AGENCY OU?                                    
         BNE    POST3AA           NO                                            
         CLC    PPGPU2,SPACES     HAVE PRODUCT UDEF 2?                          
         BH     POST4             YES - OO/OU REQUIRES THIS                     
         B      POST3A            NO - FLAG ERROR                               
*                                                                               
POST3AA  CLC    PPGPU1,SPACES                                                   
         BH     POST4                                                           
*                                                                               
         CLI    SAVFMT,C'N'    SEE IF DOING NEW FORMAT                          
         BNE    POST3B                                                          
POST3A   MVC    P+1(37),=C'**ERROR-MISSING INTERNAL ORDER NUMBER'               
         OI     ERRORSW,X'02'                                                   
         B      POST3T                                                          
*                                                                               
POST3B   MVC    P+1(38),=C'**ERROR-MISSING LEGAL ENTITY PRD USER1'              
         OI     ERRORSW,X'04'                                                   
*                                                                               
POST3T   BRAS   RE,PRNT                                                         
*                                                                               
         CLC    QAGY,=C'OO'       AGENCY OO?                                    
         BE     POST4             YES                                           
         CLC    QAGY,=C'OU'       AGENCY OU?                                    
         BE     POST4             YES                                           
*                                                                               
         CLI    SAVFMT,C'N'    SEE IF DOING NEW FORMAT                          
         BNE    POST4                                                           
         CLC    PPGPU2,SPACES                                                   
         BH     POST4                                                           
         MVC    P+1(38),=C'**ERROR-MISSING LEGAL ENTITY PRD USER2'              
         OI     ERRORSW,X'04'                                                   
         BRAS   RE,PRNT                                                         
                                                                                
POST4    DS     0H                                                              
         MVC    PPGALLO,UCOMDATA      POL EST UCOM                              
         OC     PPGALLO,SPACES        - ALLOCATION                              
         CLC    QAGY,=C'UB'           AGENCY UB?                                
         BNE    POST5                 NO                                        
         MVC    PPGEU1,UCOMDATA+32    EST UCOM 2                                
         OC     PPGEU1,SPACES         SPACE PAD                                 
*&&DO                                                                           
*        L      RF,ADEST              RESET RF TO ESTIMATE                      
*        MVC    WORK(6),ESTART-ESTHDR(RF)                                       
*                                                                               
*        GOTO1  GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                      
*        GOTO1  DATCON,DMCB,(0,WORK+6),(20,WORK+12)                             
*                                                                               
*        MVC    PPGEU2(4),WORK+12     ESTIMATE YEAR (YYYY)                      
*&&                                                                             
         B      POST6                 CONTINUE                                  
*                                                                               
POST5    CLC    QAGY,=C'OO'           AGENCY OO?                                
         BE     *+10                  YES                                       
         CLC    QAGY,=C'OU'           AGENCY OU?                                
         BNE    POST6                 NO                                        
         CLC    PPGALLO,SPACES        HAVE PRODUCT UCOMM 1?                     
         BH     POST6                 YES                                       
*                                                                               
         OI     ERRORSW,X'04'         FLAG ERROR                                
         MVC    P+1(38),=C'**ERROR-MISSING LEGAL ENTITY PRD USER1'              
         BRAS   RE,PRNT               PRINT ERROR                               
*                                                                               
POST6    CLI    QOPT1,C'N'            PRODUCING OUTPUT?                         
         BE     POSTBX                JUST EXIT                                 
*                                                                               
*        NOTE THAT A TEST RUN WILL BE ABLE TO FIND                              
*        LEGAL ENTITY MISMATCHES                                                
*                                                                               
         ZAP    PPGBAMT,BAMTS+12(6)   AMOUNT DUE                                
         AP     PPGBAMT,AMTGST        INCLUDE TAXES                             
         AP     PPGBAMT,AMTPST                                                  
*                                                                               
         ZAP    PPGGST,AMTGST         BILLS GST                                 
         AP     PPGGST,AMTHST         ADD HST                                   
         ZAP    PPGPST,AMTPST         PST                                       
         SP     PPGPST,AMTHST         EXCLUDE HST                               
*                                                                               
*  AT THIS POINT MUST ADD DATA RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         MVC    PPGKMED,QMED                                                    
         MVC    PPGKSMED,QMED       SUB MEDIA                                   
         CLI    NETPAKSW,C'Y'       SEE IF DOING NETPAK                         
         BNE    POSTB3                                                          
         CLI    BLMED,C' '          SUB MEDIA PRESENT                           
         BNH    *+10                                                            
         MVC    PPGKSMED,BLMED                                                  
POSTB3   MVC    PPGKCLI,CLT                                                     
         MVC    PPGCLI,CLT          CLIENT ALSO IN RECORD                       
         MVC    PPGMED,PPGKSMED     SUB-MEDIA ALSO IN RECORD                    
         L      RF,ADEST           RESET RF TO ESTIMATE                         
**NOPRD* MVC    PPGPRO,EKEYPRD-ESTHDR(RF)                                       
*                                                                               
         CLI    GTSFTP,C'Y'       AGENCY UB/OO/OU?                              
         BNE    POSTB4A           NO                                            
         CLC    QAGY,=C'UB'       AGENCY UB?                                    
         BNE    POSTB4            NO                                            
         CLI    NETPAKSW,C'Y'     NETPAK?                                       
         BNE    POSTB4A           NO - NO NEED TO SAVE THE PRD ANYWHERE         
***                                                                             
* AGENCY UB SHOULD COLLAPSE PRD & OMIT IT FROM THE KEY BUT SAVE IT TO A         
* FIELD THAT IS CURRENTLY UNUSED SINCE WE TEST FOR PRD 'EQ ' FOR NET/UB         
***                                                                             
         MVC    PPGPU2(3),EKEYPRD-ESTHDR(RF)  SAVE OFF PRD TO PPGPU2            
         B      POSTB4A           DO NOT SAVE PRD TO KEY                        
*                                                                               
POSTB4   MVC    PPGPRO,EKEYPRD-ESTHDR(RF)                                       
*                                                                               
POSTB4A  MVC    PPGKEST+1(1),EKEYEST-ESTHDR(RF) +1 SPOT/NET 1 BYTE EST          
         MVC    PPGEST+1(1),EKEYEST-ESTHDR(RF) BOTH PLACES                      
         MVC    PPGINVMO,BKEYMBIL      BILLING MONTH                            
         NI     PPGINVMO,X'0F'        SET OFF YEAR HALF-BYTE                    
         MVC    PPGINVN,BKEYINV                                                 
*                                                                               
         DROP   R2                                                              
*                                                                               
         L      R2,AOFPPGT        ADDRESS OF PPGTAB                             
         PRINT  GEN                                                             
         GOTO1  =V(BINSRCH),BINVALS                                             
         PRINT  NOGEN                                                           
*                                                                               
         CLI    BINVALS,1          RECORD INSERTED                              
         BE     GOTOXIT                                                         
         OC     BINVALS+1(3),BINVALS+1 IF ZERO TABLE IS FULL                    
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         L      RF,BINVALS               ADDRESS OF FOUND RECORD                
         LA     RF,L'PPGKEY(RF)          PAST KEY                               
*                                                                               
         CLI    GTSFTP,C'Y'     AGENCY UB/OO/OU?                                
         BE     POSTB5          YES                                             
         CLI    SAVFMT,C'N'     SEE IF DOING NEW FORMAT                         
         BE     POSTB5                                                          
*                                                                               
         CLC    LEDIS(L'PPGPU1,RF),PPGPU1   LEGAL ENTITIES                      
         BE     POSTB5                                                          
         OI     ERRORSW,X'08'   MISMATCH ENCOUNTERED                            
         MVC    P+1(33),=C'**ERROR-LEGAL WARNING MISMATCH ON'                   
         MVC    P+35(L'PPGINVF),PPGINVF                                         
         BRAS   RE,PRNT                                                         
POSTB5   DS     0H                                                              
         AP     AMTDIS(6,RF),PPGBAMT                                            
         AP     GSTDIS(6,RF),PPGGST                                             
         AP     PSTDIS(6,RF),PPGPST                                             
*                                                                               
GOTOXIT  DS     0H                                                              
         MVC    BINVALS,=A(PPGKMED)                                             
         MVI    BINVALS,1                                                       
POSTBX   XIT1                                                                   
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
BINVALS  DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(PPGKMED)       RECORD TO BE ADDED                            
         DC    A(PPGTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
PPGRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    AL4(PPGTLEN)       LEN OF RECORD                                 
         DC    AL4(L'PPGKEY)      KEY SIZE                                      
         DC    F'7000'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFPPGT  DC    A(PPGTABLE)                                                      
PPGKEY   DS    0XL15                                                            
PPGKMED  DS    CL1                                                              
PPGKCLI  DS    CL3                                                              
PPGKSMED DS    CL1           SUB-MEDIA                                          
PPGPRO   DS    CL3           **NOPRD*  THEY WILL DO PRDS TOGETHER               
PPGINUMB DS    0XL3                                                             
PPGINVMO DS    XL1           INVOICE MONTH                                      
PPGINVN  DS    XL2           INVOICE NUMBER                                     
PPGKEST  DS    XL2           EST IN LAST BYTE FOR SPOT/NET                      
PPGKEBRT DS    CL1           BARTER INDICATOR                                   
PPGKAOR  DS    CL1           AOR BILL                                           
*            ______                                                             
*              15                                                               
PPGREC   DS    0C                                                               
         ORG   *+PPGRECL                                                        
*                                                                               
ENDPPGR  DS    0C                                                               
*                                                                               
PPGTLEN  EQU   ENDPPGR-PPGKEY                                                   
*                                                                               
***********************                                                         
         SPACE 2                                                                
NXTEL    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NXTEL+2                                                          
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
PBROLL   NTR1                                                                   
         LHI   R0,NAMTS                                                         
PBROLL2  DS    0H                                                               
         AP    0(6,R4),0(6,R3)                                                  
         LA    R3,6(R3)                                                         
         LA    R4,6(R4)                                                         
         BCT   R0,PBROLL2                                                       
         B     EXIT                                                             
         SPACE 3                                                                
CLRTOTS  NTR1                                                                   
         LHI   R0,NAMTS                                                         
CLRT2    DS    0H                                                               
         ZAP   0(6,R3),=P'0'                                                    
         LA    R3,6(R3)                                                         
         BCT   R0,CLRT2                                                         
         B     EXIT                                                             
         SPACE 3                                                                
CHKTOTS  NTR1                                                                   
         LHI   R0,NAMTS                                                         
CHKT2    DS    0H                                                               
         CP    0(6,R3),=P'0'                                                    
         BNE   NO                  EXIT WITH CC NOT =                           
         LA    R3,6(R3)                                                         
         BCT   R0,CHKT2                                                         
         B     YES                 EXIT WITH CC =                               
         ANSR                                                                   
         EJECT                                                                  
TOTPRNT  NTR1                                                                   
         SPACE 2                                                                
         ST    R3,ATOTS                                                         
         L     R4,ATOTS                                                         
         BAS   RE,TPFMT                                                         
         BRAS  RE,PRNT                                                          
         XIT1                                                                   
         SPACE 3                                                                
TPFMT    NTR1                                                                   
         SPACE 2                                                                
         LA    R7,P                                                             
         USING BLINED,R7                                                        
         USING AMOUNTSD,R4                                                      
*                                                                               
         CLI   CNTRY,C'C'          IF CANADA                                    
         BNE   TPF02                                                            
*                                                                               
         ZAP   DUB(6),AMTACT       ACTUAL                                       
         AP    DUB(6),AMTGST       PLUS GST                                     
         AP    DUB(6),AMTPST       PLUS PST                                     
         LA    R1,DUB                                                           
         LA    R5,BLACTUAL                                                      
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTNET           NET                                          
         LA    R5,BLNET                                                         
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTGRS           GROSS                                        
         LA    R5,BLGROSS                                                       
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTAC            ACTUAL COMMISSION                            
         LA    R5,BLAC                                                          
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTGST           DO GST                                       
         LA    R5,BLGST                                                         
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTPST           AND PST                                      
         ZAP   DUB(6),AMTPST                                                    
         BZ    TPF04                                                            
         LA    R1,DUB                                                           
         LA    R5,BLPST                                                         
         BAS   RE,TPEDT                                                         
         B     TPF04                                                            
*                                                                               
TPF02    DS    0H                  NON-CANADIAN GETS WIDER COLUMNS              
         EDIT  AMTGRS,BLGRSWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTACT,BLACTWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTNET,BLNETWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTAC,BLACWIDE,2,COMMAS=YES,MINUS=YES                            
         DROP  R4                                                               
*                                                                               
TPF04    DS    0H                                                               
         BRAS  RE,PRNT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
TPEDT    DS    0H                                                               
         EDIT  (P6,0(R1)),(15,WORK),2,COMMAS=YES,MINUS=YES                      
         LR    RF,R5                                                            
         SH    RF,=H'2'                                                         
         CLI   WORK,C' '                                                        
         BNE   TPEDT6                                                           
         CLI   WORK+1,C' '                                                      
         BE    *+12                                                             
         CLI   1(RF),C' '                                                       
         BNE   TPEDT6                                                           
*                                                                               
         OC    1(15,RF),WORK                                                    
         BR    RE                                                               
*                                                                               
TPEDT6   DS    0H                                                               
         MVC   133(15,RF),WORK                                                  
         BR    RE                                                               
         EJECT                                                                  
GETVEN   NTR1                                                                   
*                                                                               
         MVC   SAVVEND,=C'15096568|'                                            
         CLC   QAGY,=C'UB'       AGENCY UB?                                     
         BE    GETVXIT           YES                                            
         MVC   SAVVEND,=C'15313660|'                                            
         CLC   QAGY,=C'OO'       AGENCY OO?                                     
         BE    GETVXIT           YES                                            
         MVC   SAVVEND,=C'15313917|'                                            
         CLC   QAGY,=C'OU'       AGENCY OU?                                     
         BE    GETVXIT           YES                                            
*                                                                               
         XC    SAVVEND,SAVVEND                                                  
         MVC   WORK(2),QAGY                                                     
         MVC   WORK+2(3),CLIENT                                                 
         LA    R5,VENTAB                                                        
         CLI   NETPAKSW,C'Y'   SEE IF NETPAK                                    
         BNE   GETV5                                                            
         LA    R5,NVENTAB      USE NET TABLE                                    
*                                                                               
GETV5    CLI   0(R5),X'FF'     END OF TABLE                                     
         BNE   *+6                                                              
         DC    H'0'            INVALID AGENCY/CLIENT                            
         CLC   0(5,R5),WORK                                                     
         BE    GETV10                                                           
         LA    R5,28(R5)                                                        
         B     GETV5                                                            
*                                                                               
GETV10   MVC   SAVVEND,5(R5)   SAVE VENDOR CODE                                 
         MVC   SAVFILE,14(R5)   FILE NAME                                       
         MVC   SAVFMT,27(R5)    SAVE FORMAT TYPE                                
GETVXIT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VENDOR CODE TABLE                                                      
*        AGY CODE - CLT CODE - VENDOR CODE                                      
*                   NOTE THE NAME IS FOLLOWED BY A "|" (X'4F')                  
*                  -THE FIELD DELIMITER                                         
*                                                                               
VENTAB   DS    0H                                                               
         DC    C'H9',C'PG ',CL09'10075955|'    LEO BURNETT/STARCOM-CHIC         
         DC    CL13'STARCOM_SPOT',C'O'                                          
         DC    C'H9',C'PGG',CL09'20517561|'    MEDIA VEST - GILLETTE            
         DC    CL13'DDS_SPT_NY',C'O'                                            
         DC    C'H9',C'PG1',CL09'20517561|'    MEDIA VEST                       
         DC    CL13'DDS_SPT_NY',C'O'                                            
         DC    C'H9',C'PG6',CL09'20517561|'    MEDIA VEST                       
         DC    CL13'DDS_SPT_NY',C'O'                                            
         DC    C'H9',C'PGB',CL09'20176036|'    BROMLEY COMM                     
         DC    CL13'DDS_SPT_BR',C'O'                                            
         DC    C'H9',C'HPG',CL09'15123794|'    TAPESTRY                         
         DC    CL13'DDS_SPT_TA',C'O'                                            
*                                                                               
         DC    C'HY',C'PG1',CL09'10056957|'  MEDIACOM                           
         DC    CL13'DDS_SPT_HY',C'N'                                            
*                                                                               
         DC    C'HY',C'P12',CL09'10056957|'  MIDIACOM                           
         DC    CL13'DDS_SPT_HY',C'N'                                            
*                                                                               
         DC    C'HY',C'P13',CL09'10056957|'  MIDIACOM                           
         DC    CL13'DDS_SPT_HY',C'N'                                            
*                                                                               
*                                                                               
**OLD**  DC    C'O0',C'PGM',CL09'15041589|'  MEDIAVEST-TORONTO                  
         DC    C'O0',C'PGM',CL09'15086124|'  MEDIAVEST-TORONTO                  
         DC    CL13'STARCOMTO_SPT',C'O'                                         
*                                                                               
**** FOR TESTING ***                                                            
         DC    C'HD',C'PGT',CL09'20517561|' HDTO - TESTING                      
         DC    CL13'DDS_SPT_XX',C'N'                                            
         DC    C'HD',C'PG1',CL09'20517561|' HDTO - TESTING                      
         DC    CL13'DDS_SPT_XX',C'N'                                            
         DC    C'T1',C'PG1',CL09'20517561|' TCH1 - TESTING                      
         DC    CL13'DDS_SPT_XX',C'N'                                            
         DC    C'TC',C'PGT',CL09'20517561|' TRAINING CENTER - TESTING           
         DC    CL13'DDS_SPT_XX',C'N'                                            
         DC    C'DR',C'PG ',CL09'20517561|' SMG TESTING AGENCY                  
         DC    CL13'DDS_SPT_DR',C'O'                                            
         DC    X'FFFF'                  END OF TABLE                            
*                                                                               
NVENTAB  DS    0H                                                               
         DC    C'H9',C'HP1',CL09'15123794|'    TAPESTRY                         
         DC    CL13'DDS_NET_TA',C'O'                                            
         DC    C'H9',C'HPG',CL09'15123794|'    TAPESTRY                         
         DC    CL13'DDS_NET_TA',C'O'                                            
         DC    C'H9',C'PGB',CL09'20176036|'    BROMLEY COMM                     
         DC    CL13'DDS_NET-BR',C'O'                                            
         DC    C'DU',C'PGG',CL09'20517561|'    MEDIA VEST - GILLETTE            
         DC    CL13'DDS_NET_NY',C'O'                                            
         DC    C'DU',C'PG1',CL09'20517561|'    MEDIA VEST                       
         DC    CL13'DDS_NET_NY',C'O'                                            
         DC    C'DU',C'PG2',CL09'20517561|'    MEDIA VEST                       
         DC    CL13'DDS_NET_NY',C'O'                                            
         DC    C'DU',C'PG3',CL09'20517561|'    MEDIA VEST                       
         DC    CL13'DDS_NET_NY',C'O'                                            
         DC    C'DU',C'PG4',CL09'20517561|'    MEDIA VEST                       
         DC    CL13'DDS_NET_NY',C'O'                                            
         DC    C'DU',C'PG5',CL09'20517561|'    MEDIA VEST                       
         DC    CL13'DDS_NET_NY',C'O'                                            
*                                                                               
**** FOR TESTING ***                                                            
         DC    C'T1',C'PG1',CL09'20517561|' TCH1 - TESTING                      
         DC    CL13'DDS_NET_XX',C'N'                                            
         DC    C'TC',C'PGT',CL09'20517561|'  TRAINING CENTER - TESTING          
         DC    CL13'DDS_NET_XX',C'N'                                            
         DC    C'DR',C'PG ',CL09'20517561|'    SMG TESTING AGENCY               
         DC    CL13'DDS_NET_DR',C'O'                                            
         DC    X'FFFF'                  END OF TABLE                            
*                                                                               
SGTTAPE  DCB   DDNAME=SGTTAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=100,                                              X        
               BLKSIZE=100,                                            X        
               MACRF=PM                                                         
*                                                                               
         LTORG                                                                  
         DROP  R7                                                               
***********************************************************************         
*   PRNT - PRINTING ROUTINE                                                     
***********************************************************************         
PRNT     NMOD1 0,PRNT                                                           
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         MVC   HEAD1+55(19),=C'NETPAK P&&G BILLING LIST'                        
         MVC   HEAD2+55(19),=C'-----------------------'                         
         CLI   MEDIA,C'N'                                                       
         BE    *+14                                                             
         MVC   HEAD1+54(4),=C'SPOT'                                             
         MVI   HEAD2+54,C'-'                                                    
*                                                                               
         CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BNE   PRNT2                                                            
         MVC   HEAD2(6),=C'OFFICE'                                              
         MVC   HEAD2+7(2),OFFICE                                                
*                                                                               
PRNT2    DS    0H                                                               
         CLI   QOPT5,C'A'         TEST AOR ONLY                                 
         BNE   *+10                                                             
         MVC   HEAD6(18),=C'**AOR BILLS ONLY**'                                 
*                                                                               
         CLI   QOPT5,C'B'         AOR AND AOR/CLIENT                            
         BNE   *+10                                                             
         MVC   HEAD6(24),=C'**AOR AND CLIENT BILLS**'                           
*                                                                               
         CLI   QOPT5,C'X'         NON=AOR BILLS ONLY                            
         BNE   *+10                                                             
         MVC   HEAD6(22),=C'**NON-AOR BILLS ONLY**'                             
*                                                                               
         CLI   QOPT5,C'C'         COMMISSION ONLY BILLS                         
         BNE   *+10                                                             
         MVC   HEAD6(25),=C'**COMMISSION ONLY BILLS**'                          
*                                                                               
         CLI   QOPT4,C'M'         MOS DATES                                     
         BNE   *+10                                                             
         MVC   HEAD4+49(32),=C'**MONTH OF SERVICE DATE FILTER**'                
*                                                                               
         LA    R4,HEAD3+49                                                      
         MVC   0(23,R4),=C'PERIOD FROM MMMDD/YY TO'                             
         GOTO1 DATCON,DMCB,SVQST,(8,12(R4))                                     
         GOTO1 DATCON,DMCB,SVQEND,(8,24(R4))                                    
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
*                             HEADHOOK ROUTINE                                  
HHROUT   NTR1                                                                   
         CLC   =C'ALL',QCLT        FIX CLIENT HEADLINE                          
         BNE   HHR2                                                             
         CLI   PROGPROF+0,C'Y'     UNLESS SEPARATE PAGE                         
         BE    HHR2                                                             
         MVC   HEAD3+9(3),=C'ALL'                                               
         MVC   HEAD3+13(24),SPACES                                              
*                                                                               
HHR2     DS    0H                                                               
         CLC   =C'ALL',QPRD        FIX PRODUCT HEADLINE                         
         BNE   HHR2B                                                            
         CLI   PROGPROF+1,C'Y'     UNLESS SEPARATE PAGE                         
         BE    HHR2B                                                            
         MVC   HEAD4+9(3),=C'ALL'                                               
         MVC   HEAD4+13(24),SPACES                                              
*                                                                               
HHR2B    DS    0H                                                               
         CLC   =C'ALL',QEST        FIX ESTIMATE HEADLINE                        
         BNE   *+16                                                             
         MVC   HEAD5+9(3),=C'ALL'                                               
         MVC   HEAD5+13(24),SPACES                                              
*                                                                               
         CLI   MODE,CLTLAST        UNLESS AFTER CLIENT LAST                     
         BH    HHR4                PUT CLIENT DATA IN MIDLINE                   
         CLC   SVMID+8(3),CLT      IF STILL WITH SAME CLIENT                    
         BNE   HHR4                                                             
         CLC   P,SVMID             DON'T REPEAT THE CLIENT LINE                 
         BE    HHR4                                                             
         MVC   MID1,SVMID                                                       
*                                                                               
HHR4     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
         L     RA,0(R1)                                                         
         USING SPWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING SPGTWRKD,RC                                                      
         L     R4,4(R1)        ADDRESS OF PPGREC                                
         USING PPGRECD,R4                                                       
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         XC    DLCB(L'DLCB),DLCB   CLEAR IT                                     
*                                                                               
         MVC   DLCBFLD,SPACES    CLEAR FIELD                                    
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         CLI   MODE,RUNLAST       SEE IF END OF REPORT                          
         BE    DNP70                                                            
*                                                                               
         CLI   MODE,REQFRST       SEE IF I NEED TO INTIALIZE                    
         BE    DNP80                                                            
*****                                                                           
*****    DOWNLOAD BILLING INFO HERE                                             
*****                                                                           
*****                                                                           
*                                                                               
         MVI   DLCBFLD,C'H'      HEADER                                         
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1      SAVE R1                                           
         GOTO1 DATCON,DMCB,(0,PPGIDATE),(X'20',WORK)                            
         L     R1,SAVER1      RESTORE R1                                        
         MVC   DLCBFLD(2),WORK+4     DAY                                        
         MVC   DLCBFLD+2(2),WORK+2   MONTH                                      
         MVC   DLCBFLD+6(2),WORK        YEAR                                    
         MVC   DLCBFLD+4(2),=C'20'     CENTURY                                  
         CLC   WORK(2),=C'80'                                                   
         BL    *+10                                                             
         MVC   WORK(2),=C'19'  IF YEAR IS GREATER THAN 80                       
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
******                         ASSUME LAST CENTURY                              
******  Y3K NOTE: CODE WILL WORK UNTIL 2080- I'LL BE DEAD                       
******                                                                          
*                                  LEGAL ENTITY                                 
         MVC   DLCBFLD(3),PPGPU2   PRD USER 2                                   
         CLC   PPGPU2,SPACES                                                    
         BH    *+10                                                             
         MVC   DLCBFLD(3),=C'283'  HARD CODED - DEFAULT                         
         CLI   SAVFMT,C'N'         SEE IF DOING NEW FORMAT                      
         BE    DOWNLDT                                                          
*                                                                               
         MVC   DLCBFLD(3),PPGPU1   PRD USER FIELD 1                             
DOWNLDT  MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
                                                                                
         MVC   DLCBFLD(3),=C'CAD'    CURRENCY                                   
         CLI   CNTRY,C'C'         SEE IF CANDIAN AGENCY                         
         BE    *+10                                                             
         MVC   DLCBFLD(3),=C'USD'                                               
*                                                                               
         MVI   DLCBFLD+3,X'4F'    EXTRA FIELD DELIMITER                         
*                                 SO I WON'T HAVE TO SEND                       
*                                 AN EMPTY FIELD                                
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
                                                                                
*                                  TRANSLATION DATE - EMPTY                     
***NO    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
***NO    MVI   DLCBACT,DLCBPUT                                                  
***NO    GOTO1 VDLFLD                                                           
***NO                                                                           
*                                   VENDOR ID                                   
*                                                                               
         MVC   DLCBFLD(09),SAVVEND                                              
*                                                                               
*  NOTE: SAVVEND HAS THE "|" (X'4F') SO THAT AN EMPTY                           
*            ALTERNATIVE PAYEE FIELD NEED NOT BE SENT                           
*                                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 ALTERNATIVE PAYEE- EMPTY                      
***NO    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
***NO    MVI   DLCBACT,DLCBPUT                                                  
***NO    GOTO1 VDLFLD                                                           
*                                 INVOICE NUMBER                                
         MVC   DLCBFLD(L'PPGINVF),PPGINVF                                       
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 INVOICE HEADER TEXT                           
*                                 MOS (MMYY) AND EST                            
         MVC   WORK(2),PPGMOS                                                   
         MVI   WORK+2,X'01'   DUMMY DAY                                         
         ST    R1,SAVER1      SAVE R1                                           
         GOTO1 DATCON,DMCB,(3,WORK),(X'20',WORK+6)                              
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(3),PPGCLI    NOW START WITH CLIENT                       
         MVC   DLCBFLD+3(2),WORK+8   MM                                         
         MVC   DLCBFLD+5(2),WORK+6   YY                                         
         MVC   SVMOS,DLCBFLD+3   SAVE FOR LINE ITEM TEXT                        
         LA    R3,PPGEST                                                        
         BAS   RE,CVD                                                           
         MVC   DLCBFLD+7(3),WORK+2          EST                                 
*                                                                               
         MVC   MYSVEST,WORK+2    SAVE FOR LINE ITEM TEXT                        
*                                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  INVOICE AMOUNT                               
         ZAP   SVBAMT,PPGBAMT     AMOUNT DUE (INCLUDES GST/PST/HST)             
         NI    PPGBAMT+5,X'F0'                                                  
         OI    PPGBAMT+5,X'0C'    MAKE POSITIVE                                 
         EDIT  (P6,PPGBAMT),(11,DLCBFLD),2,ALIGN=LEFT                           
         ZAP   PPGBAMT,SVBAMT   RESTORE FOR LINE ITEMS                          
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        MVC   DLCBFLD(4),=C'T011'  TAPESTRY GETS T011 FOR EVERYTHING           
*        MVC   DLCBFLD(4),=C'T008'  TAPESTRY GETS T008 FOR EVERYTHING           
         MVC   DLCBFLD(4),=C'T010'  TAPESTRY GETS T010 FOR EVERYTHING           
*                                   TICKET# 0148589N    PLUS 45 DAYS            
         CLC   QAGY,=C'H9'                                                      
         BNE   DNP0                                                             
         CLC   CLIENT,=C'HPG'                                                   
         BE    DNP2X                                                            
         CLC   CLIENT,=C'HP1'                                                   
         BE    DNP2X                                                            
*                                 PAYMENT TERMS                                 
DNP0     MVC   DLCBFLD(4),=C'T007'    FOR SPOT MEDIAVEST TORONTO                
         CLC   QAGY,=C'O0'                                                      
         BE    DNP1                                                             
         MVC   DLCBFLD(4),=C'T148'    FOR SPOT AND CABLE/SYND                   
**OLD**  MVC   DLCBFLD(4),=C'T234'    FOR SPOT AND CABLE/SYND                   
         CLC   QAGY,=C'H9'             MEDIAVEST AND STARCOM?                   
         BE    DNP1                                                             
         CLC   QAGY,=C'DU'             MEDIAVEST (NETWORK)                      
         BE    DNP1                                                             
         MVC   DLCBFLD(4),=C'T148'    FOR SPOT AND CABLE/SYND                   
*                                     DEFAULT FOR OTHER AGENCIES                
DNP1     CLI   NETPAKSW,C'Y'                                                    
         BNE   DNP2                                                             
         CLI   PPGMED,C'C'        CABLE?                                        
         BE    DNP2                                                             
         CLI   PPGMED,C'S'        SYNDICATION                                   
         BE    DNP2                                                             
         CLI   PPGMED,C'O'        OTHER (USED FOR CUTINS?)                      
         BE    DNP2                                                             
*                                                                               
         MVC   DLCBFLD(4),=C'T011' "NORMAL" NETWORK                             
*                                                                               
DNP2     DS    0H                                                               
         CLC   QAGY,=C'H9'        STARCOM?                                      
         BNE   DNP2HY                                                           
         CLC   CLIENT,=C'PG '     STARCOM PG CLIENT?                            
         BNE   DNP2PGB            OTHER CLTS ARE MEDIAVEST                      
         MVC   DLCBFLD(4),=C'T006'                                              
         B     DNP2X                                                            
*                                                                               
DNP2PGB  CLC   CLIENT,=C'PGB'     SEE IF BROMLEY                                
         BNE   DNP2X                                                            
******   MVC   DLCBFLD(4),=C'T011' FOR ALL NETPAK                               
******   CLI   NETPAKSW,C'Y'                                                    
******   BE    DNP2X                                                            
         MVC   DLCBFLD(4),=C'T054' SPECIAL PAYMENT TERMS FOR                    
         B     DNP2X                                                            
*                                  ALL BROMLEY MEDIA AS OF NOV01/05             
*                                  WAS T229 FOR ALL SINCE OCT/04                
*                                                                               
*                                  BROMLEY SPOT ONLY                            
*                                  FOR ALL NET AS WELL AS OF OCT25/04           
*                                                                               
*                                                                               
DNP2HY   CLC   QAGY,=C'HY'                                                      
         BNE   DNP2X               IF NOT HY LEAVE AS T148                      
         MVC   DLCBFLD(4),=C'T053' SPECIAL PAYMENT TERMS FOR MCMMP (HY)         
*                                                                               
DNP2X    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 NUMBER OF LINE ITEMS                          
*                           NOTE - USUALLY 1 EXCEPT IF CANADIAN                 
         MVI   DLCBFLD,C'1'                                                     
         CP    PPGGST,=P'0'        ANY GST?                                     
         BE    *+8                                                              
         MVI   DLCBFLD,C'2'                                                     
         CP    PPGPST,=P'0'        ANY PST?  (INCLUDES HST)                     
         BE    DNP10                                                            
         CLI   DLCBFLD,C'2'        IS IT ALREADY 2?                             
         BE    DNP5                                                             
         MVI   DLCBFLD,C'2'       NO GST PRESENT BUT PST FOUND                  
         B     DNP10                                                            
*                                                                               
DNP5     MVI   DLCBFLD,C'3'                                                     
DNP10    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
*                                                                               
         MVI   DLCXDELC,C' '     ALTER FOR THIS FIELD ONLY (THE LAST)           
         GOTO1 VDLFLD                                                           
         MVI   DLCXDELC,X'4F'    RESTORE TO VERTICAL PIKE                       
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
         EJECT                                                                  
*        LINE ITEM RECORD(S)                                                    
                                                                                
*                                  TYPE                                         
*                                                                               
         MVI   SEQNUM,C'1'                                                      
         MVI   TAXSW,0                                                          
         ZAP   MYPACK,PPGBAMT   START WITH BILL AMOUNT                          
         SP    MYPACK,PPGGST    SUBTRACT GST/HST/PST                            
         SP    MYPACK,PPGPST                                                    
         ZAP   MYPPOS,PPGBAMT                                                   
         SP    MYPPOS,PPGGST                                                    
         SP    MYPPOS,PPGPST                                                    
         NI    MYPPOS+5,X'F0'    MAKE POSITIVE                                  
         OI    MYPPOS+5,X'0C'                                                   
         LA    R6,MYPACK                                                        
         LA    R7,MYPPOS                                                        
*                                                                               
DNP20    MVC   DLCBFLD(2),=C'FI'                                                
         TM    5(R6),X'0D'   SEE IF 'REAL' AMOUNT IS NEGATIVE                   
         BNO   *+10                                                             
         MVC   DLCBFLD(3),=C'CRM'                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  LINE COUNTER                                 
*                           NOTE - FIRST LINE ITEM                              
         MVC   DLCBFLD(1),SEQNUM                                                
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 ACCOUNT NUMBER                                
*                         NOTE - BRAND ESTIMATE USER FIELD 1                    
*                                (WAS FROM POL EST)                             
*                         NOTE - LENGTH OF 8 FROM EXAMPLE                       
*                               - COULD CHANGE                                  
         MVC   DLCBFLD(8),PPGEU1                                                
*                                                                               
         CLI   SAVFMT,C'N'         SEE IF DOING NEW FORMAT                      
         BNE   DNP20C                                                           
         CLI   PPGEBRT,C'B'       SEE IF BARTER                                 
         BNE   DNP20C                                                           
         CLI   PPGAOR,C'A'        AND AND AOR BILL                              
         BNE   DNP20C                                                           
         MVC   DLCBFLD(8),=C'20640019'                                          
*                                                                               
DNP20C   CLI   TAXSW,1            SEE IF DOING GST                              
         BNE   *+10                                                             
         MVC   DLCBFLD(8),=C'12041011'    GST/HST ACCOUNT CODE                  
*                                                                               
         CLI   TAXSW,2            SEE IF DOING PST? (QST?)                      
         BNE   *+10                                                             
         MVC   DLCBFLD(8),=C'12041007'                                          
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 LINE ITEM TEXT                                
         CLI   TAXSW,1            SEE IF DOING GST/HST                          
         BNE   DNP24                                                            
*******  MVC   DLCBFLD(7),=C'GST/HST'                                           
*******  MVI   DLCBFLD+7,X'4F'     SINCE NEXT FIELD IS EMPTY                    
         MVC   DLCBFLD(13),=C'HST#866334549'                                    
         CLC   QAGY,=C'HY'                                                      
         BE    *+10                                                             
         MVC   DLCBFLD(13),=C'GST#886669233'                                    
*                                                                               
         MVI   DLCBFLD+13,X'4F'     SINCE NEXT FIELD IS EMPTY                   
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP33                                                            
*                                                                               
DNP24    CLI   TAXSW,2          SEE IF DOING PST                                
         BNE   DNP28                                                            
         MVC   DLCBFLD(3),=C'PST'                                               
***                                                                             
**       MAY NEED TO CHANGE IF THEY START ENCOUNTERING QST                      
***                                                                             
         MVI   DLCBFLD+3,X'4F'     SINCE NEXT FIELD IS EMPTY                    
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP33                                                            
*                                                                               
*                                 LINE ITEM TEXT                                
DNP28    DS    0H                                                               
*****    CLC   QAGY,=C'HY'                                                      
         CLI   HYMCSW,C'Y'       ONLY FOR MCMMP CLIENTS                         
         BNE   DNP28B5                                                          
         CLC   PPGEU2,SPACES     DO I HAVE ANY EST USER 2 DATA                  
         BNH   DNP28B5           IF NOT USE OLD FORMAT                          
         MVC   DLCBFLD(L'PPGEU2),PPGEU2                                         
         LA    RE,DLCBFLD+L'PPGEU2                                              
DNP28B2  CLI   0(RE),C' '        SCAN BACKWARD FOR FIRST NON-SPACE              
         BNE   DNP28B3                                                          
         BCT   RE,DNP28B2                                                       
*                                                                               
DNP28B3  MVC   2(L'PPGENAME,RE),PPGENAME                                        
         OC    DLCBFLD(37),SPACES       JUST IN CASE   16+1+20                  
         MVC   DLCBFLD+30(7),SPACES     LIMIT TO 30 CHARS                       
         B     DNP28BX                                                          
*                                                                               
DNP28B5  MVC   DLCBFLD(3),SVEST                                                 
         MVC   DLCBFLD(3),MYSVEST                                               
         MVC   DLCBFLD+3(4),SVMOS                                               
         MVC   DLCBFLD+8(L'PPGENAME),PPGENAME                                   
*                                                                               
DNP28BX  CLC   PPGALLO(32),SPACES       ALLOCATION PRESENT?                     
         BNE   DNP30              YES - THEN NORMAL END                         
*                                                                               
         LA    R2,DLCBFLD+36                                                    
         OC    DLCBFLD(37),SPACES                                               
DNP28C   CLI   0(R2),C' '     SCAN BACKWARD FOR NON-SPACE                       
         BH    DNP28D                                                           
         BCT   R2,DNP28C                                                        
*                                                                               
DNP28D   MVI   1(R2),X'4F'   EXTRA DELIMITER + DON'T SEND EMPTY FLD             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP33                                                            
*                                 NOW SEND LINE ITEM TEXT                       
DNP30    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 ALLOCATION CODE                               
         MVC   DLCBFLD(32),PPGALLO                                              
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  LINE ITEM AMOUNT                             
*                                  ALWAYS NOW POSTIVE                           
DNP33    EDIT  (P6,0(R7)),(11,DLCBFLD),2,ALIGN=LEFT                             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 TAX CODE                                      
         CLI   TAXSW,1            GST/HST?                                      
         BE    DNP33L                                                           
         CLI   TAXSW,2            PST/QST?                                      
         BE    DNP33L                                                           
*                                                                               
         CLI   NETPAKSW,C'Y'        SEE IF NETPAK                               
         BNE   DNP33P                                                           
         CLC   PPGEU1(8),=C'14080026'   SPECIAL ACCOUNT?                        
         BE    DNP33L                                                           
         CLC   PPGEU1(8),=C'14060013'    SEE IF SUPPLEMENTAL                    
         BNE   DNP33X                                                           
DNP33L   MVC   DLCBFLD(5),=C'|||||'   REST OF FIELDS EMPTY                      
         B     DNP55L                 SEND AS LAST FIELD                        
*                                                                               
DNP33P   DS    0H               SPOT                                            
         CLC   QAGY,=C'H9'      MEDIAVEST?                                      
         BNE   DNP33X                                                           
         CLC   QCLT,=C'PG1'     MEDIAVEST?                                      
         BNE   DNP33X                                                           
         CLC   PPGEU1(8),=C'14080026'     SOAP?                                 
         BE    DNP33L              REST LIKE SUPPLEMENTAL                       
*                                                                               
DNP33X   DS    0H                                                               
         MVC   DLCBFLD(2),=C'1Z'                                                
         MVI   DLCBFLD+2,X'4F'    EXTRA FIELD DELIMITER                         
*                               SO I WON'T HAVE TO SEND EMPTY FIELD             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 WITHHOLDING TAX CODE                          
*                                 EMPTY FOR NA                                  
***NO    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
***NO    MVI   DLCBACT,DLCBPUT                                                  
***NO    GOTO1 VDLFLD                                                           
*                                 TAX JURISDICTION                              
         MVC   DLCBFLD(4),=C'CA00'                                              
         LA    R2,DLCBFLD+4                                                     
*                                                                               
         CLI   CNTRY,C'C'                                                       
         BE    DNP55                                                            
*                                                                               
***      L     RF,ADAGY                                                         
***      USING AGYHDR,RF                                                        
***      CLI   AGYPCNDA,C'C'        CANADIAN?                                   
***      BE    DNP55                                                            
***      DROP  RF                                                               
*                                                                               
         MVC   DLCBFLD(10),=C'9999999999'                                       
         LA    R2,DLCBFLD+10                                                    
DNP55    DS    0H                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 INTERNAL ORDER NUMBER                         
         CLI   SAVFMT,C'N'        SEE IF NEW FORMAT                             
         BE    DNP55A0                                                          
         BNE   DNP55B                                                           
*                                                                               
DNP55A0  CLI   PPGEBRT,C'B'       SEE IF A BARTER BILL                          
         BNE   DNP55A5                                                          
         CLI   PPGAOR,C'A'       AND AOR                                        
         BNE   DNP55A5                                                          
*                               ALSO MUST BE A CREDIT?                          
         ZAP   MYPACK,PPGBAMT   START WITH BILL AMOUNT                          
         SP    MYPACK,PPGGST    SUBTRACT GST/HST/PST                            
         SP    MYPACK,PPGPST                                                    
         TM    MYPACK+5,X'0D'                                                   
         BNO   DNP55A5                                                          
*                                                                               
*        SKIP INTERNAL ORDER NUMBER FOR THESE                                   
*                                                                               
         LA    R2,DLCBFLD-1       SINCE DNP55C USES 1(R2)                       
         B     DNP55C                                                           
*                                                                               
DNP55A5  MVC   DLCBFLD(L'PPGPU1),PPGPU1                                         
         LA    R3,L'PPGPU1                                                      
         LA    R2,DLCBFLD+L'PPGPU1-1                                            
DNP55AT  CLI   0(R2),C' '   SCAN BACKWARD FOR NON-SPACE                         
         BH    DNP55C       REST SAME AS OLD CODE                               
         SH    R2,=H'1'                                                         
         BCT   R3,DNP55AT                                                       
*                                                                               
         LA    R2,DLCBFLD-1 SEND EMPTY FIELD                                    
         B     DNP55C       SHOULD NOT GET HERE                                 
*                                 INTERNAL ORDER NUMBER                         
*                     BRAND ESTIMATE USER FIELD 2                               
DNP55B   MVC   DLCBFLD(10),PPGEU2                                               
         LA    R3,10          IN CASE FIELD MISSING                             
         LA    R2,DLCBFLD+9                                                     
DNP55B5  CLI   0(R2),C' '    SCAN BACKWORK FOR NON-SPACE                        
         BH    DNP55C                                                           
         SH    R2,=H'1'                                                         
         BCT   R3,DNP55B5                                                       
DNP55C   MVI   1(R2),X'4F'    EXTRA FIELD DELIMITER (PO NUMBER)                 
         MVI   2(R2),X'4F'    EXTRA FIELD DELIMITER (PO LINE ITEM)              
*                               SO I WON'T HAVE TO SEND EMPTY FIELDS            
*                                                                               
*     WATCH OUT FOR OTHER REFERENCES TO DNP55L                                  
*     IT IS USED WHEN SENDING THE LAST FIELD                                    
*                                                                               
DNP55L   MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
*                                                                               
         MVI   DLCXDELC,C' '     ALTER FOR THE LAST FIELD ONLY                  
         GOTO1 VDLFLD                                                           
         MVI   DLCXDELC,X'4F'  RESTORE TO VERTICAL PIKE                         
*                                 PURCHASE ORDER NUMBER                         
*                                 EMPTY?                                        
***NO    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
***NO    MVI   DLCBACT,DLCBPUT                                                  
***NO    GOTO1 VDLFLD                                                           
*                                 PURCHASE ORDER LINE ITEM                      
*                                 EMPTY?                                        
***NO    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
***NO    MVI   DLCBACT,DLCBPUT                                                  
***NO    GOTO1 VDLFLD                                                           
*                                                                               
DNP60    DS    0H                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
         CLI   TAXSW,2            DID I JUST DO PST?                            
         BE    DNPX               YES THEN I'M DONE                             
         CLI   TAXSW,1            DID I JUST DO GST?                            
         BE    DNP65                                                            
         ZAP   MYPACK,PPGGST      GST (INCLUDES HST)                            
         CP    MYPACK,=P'0'  ANY GST/HST                                        
         BE    DNP65                                                            
         ZAP   MYPPOS,MYPACK                                                    
         NI    MYPPOS+5,X'F0'                                                   
         OI    MYPPOS+5,X'0C'     MAKE POSITIVE                                 
         LA    R6,MYPACK          'REAL' AMOUNT                                 
         LA    R7,MYPPOS          MADE POSITIVE                                 
         MVI   SEQNUM,C'2'                                                      
         MVI   TAXSW,1            SET DOING GST                                 
         B     DNP20                                                            
*                                                                               
DNP65    DS    0H                                                               
         CP    PPGPST,=P'0'      (HAD HST TAKEN OUT)                            
         BE    DNPX                                                             
         ZAP   MYPACK,PPGPST                                                    
         ZAP   MYPPOS,MYPACK                                                    
         NI    MYPPOS+5,X'F0'                                                   
         OI    MYPPOS+5,X'0C'     MAKE POSITIVE                                 
         LA    R6,MYPACK          'REAL' AMOUNT                                 
         LA    R7,MYPPOS          MADE POSITIVE                                 
         CLI   SEQNUM,C'2'        WAS SEQNUM 2                                  
         BE    DNP65C             YES - SET IT TO 3                             
         MVI   SEQNUM,C'2'        ELSE SET IT TO 2                              
         B     *+8                                                              
DNP65C   MVI   SEQNUM,C'3'                                                      
         MVI   TAXSW,2            SET DOING PST                                 
         B     DNP20                                                            
*                                                                               
DNP70    DS    0H                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'R'        SET END OF REPORT                            
         GOTO1 VDLFLD                                                           
         B     DNPX                                                             
*                                                                               
DNP80    DS    0H                                                               
*                                 AND RETURN IN SAVVEND                         
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         GOTO1 VDLFLD                                                           
DNPX     DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
CVD      DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),0(R3)                                                  
*                                                                               
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
DNPRINT  NTR1                                                                   
         MVC   SVLINE,LINE                                                      
         MVC   SVFORCEH,FORCEHED                                                
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
         MVC   LINE,SVLINE             RESTORE LINE                             
         MVC   FORCEHED,SVFORCEH       AND FORCEHED                             
         MVI   RCWHATPR,1     RESET TO FIRST                                    
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
DMTHSW   DS    CL1         M IF PROCESSING MOS LINE                             
SAVER1   DS    F                                                                
MYWORK   DS    CL12                                                             
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    X'4F'       FIELD DELIMITER - VERTICAL BAR                       
EOTCHR   DC    X'00'       END OF TEXT FIELD DELIMITER                          
EOTALT   DC    X'00'       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'00'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    X'00'       END OF REPORT CHR                                    
*                                                                               
*        NORMAL DOWNLOAD FIELDS ARE:                                            
*        C' ',C'"',C'''',X'5E',C':'     5E IS SEMI-COLON                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
*                                                                               
FTPHUB   CSECT                                                                  
         NMOD1 0,FTPHUB                                                         
         L     RA,0(R1)                A(WORKD)                                 
         USING SPWORKD,RA              SPWROKD DSECT                            
         LA    RC,SPACEND              START OF REMAINING WORK SPACE            
         USING SPGTWRKD,RC             SPGTWRKD DSECT                           
         L     R4,4(R1)                A(PPGREC)                                
         USING PPGRECD,R4              PPGRECD DSECT                            
         LR    R5,R4                   PPGREC                                   
         SHI   R5,L'PPGKEY             NEED KEY TO GET TO PPGPRO                
         USING PPGKEY,R5               PPGKEY DSECT                             
*                                                                               
         CLI   MODE,RUNLAST            END OF REPORT?                           
         BE    FTP70                   YES - CLOSE GTTAPE & SEND MQ MSG         
*                                                                               
         MVC   P,SPACES                INIT P                                   
         LA    R2,P                    R2 = P                                   
         MVI   0(R2),C'H'              HEADER                                   
         MVI   1(R2),C'|'              DELIMITER                                
         LA    R2,2(R2)                BUMP R2                                  
*                                                                               
         GOTO1 DATCON,DMCB,(0,PPGIDATE),(X'20',WORK)                            
         MVC   0(2,R2),WORK+4          DAY                                      
         MVC   2(2,R2),WORK+2          MONTH                                    
         MVC   4(2,R2),=C'20'          CENTURY                                  
         MVC   6(2,R2),WORK            YEAR                                     
         MVI   8(R2),C'|'              DELIMITER                                
         LA    R2,9(R2)                BUMP R2                                  
*                                                                               
         LA    R1,PPGPU1               PRODUCT UDEF 1                           
         CLC   QAGY,=C'UB'             AGENCY UB?                               
         BE    *+8                     YES - PRD UDEF 1 IS LEGAL ENTITY         
         LA    R1,PPGALLO              PRD UCOM1 LEGAL ENTITY FOR OO/OU         
*                                                                               
         MVC   0(4,R2),0(R1)           LEGAL ENTITY                             
         LA    R2,4(R2)                MAXED OUT DATA + 1                       
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
         MVC   0(3,R2),=C'CAD'         CURRENCY                                 
         CLI   CNTRY,C'C'              CANDIAN AGENCY?                          
         BE    *+10                    YES                                      
         MVC   0(3,R2),=C'USD'         NO - CURRENCY IN USD                     
         MVI   3(R2),C'|'              DELIMITER                                
         LA    R2,4(R2)                BUMP R2                                  
*                                                                               
         MVI   0(R2),C'|'              DELIMITER (TRANSLATN DATE BLANK)         
         LA    R2,1(R2)                BUMP R2                                  
*                                                                               
         MVC   0(9,R2),SAVVEND         VENDOR ID + DELIMITER                    
         LA    R2,9(R2)                BUMP R2                                  
*                                                                               
         MVI   0(R2),C'|'              DELIMITER (ALT PAYEE NULL)               
         LA    R2,1(R2)                BUMP R2                                  
*                                                                               
         MVC   0(10,R2),PPGINVF        INVOICE NUMBER                           
         LA    R2,10(R2)               MAXED OUT DATA + 1                       
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
         MVC   0(3,R2),PPGCLI          CLIENT CODE                              
         CLC   QAGY,=C'UB'             AGENCY UB?                               
         BE    *+10                    YES - USE CLIENT CODE                    
         MVC   0(3,R2),PPGPRO          PRODUCT CODE                             
         LA    R2,2(R2)                LAST CHARACTER OF CLT/PRD CODE           
         CLI   0(R2),X'40'             CLIENT CODE HAS 3RD CHARACTER?           
         BNH   *+8                     NO - DON'T BUMP R2                       
         LA    R2,1(R2)                YES - BUMP R2                            
         MVI   0(R2),C'-'              DASH                                     
         LA    R2,1(R2)                BUMP R2                                  
*                                                                               
         EDIT  (B2,PPGEST),(3,0(R2)),FILL=0                                     
         MVI   3(R2),C'-'              DASH                                     
         LA    R2,4(R2)                BUMP R2                                  
*                                                                               
         MVC   WORK(2),PPGMOS          MOS                                      
         MVI   WORK+2,X'01'            DUMMY DAY                                
         GOTO1 DATCON,DMCB,(3,WORK),(X'20',WORK+6)                              
         MVC   0(2,R2),WORK+8          MM                                       
         MVC   2(2,R2),WORK+6          YY                                       
         LA    R2,4(R2)                BUMP R2                                  
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
         CLC   QAGY,=C'OO'             AGENCY OO?                               
         BE    *+14                                                             
         CLC   QAGY,=C'OU'             AGENCY OU?                               
         BNE   *+14                                                             
         CP    PPGBAMT,=P'0'           ZERO DOLLAR INVOICE                      
         BE    FTPX                                                             
*                                                                               
         ZAP   SVBAMT,PPGBAMT          INV AMT (INCLUDES GST/PST/HST)           
         NI    PPGBAMT+5,X'F0'         STRIP OFF SIGN BITS                      
         OI    PPGBAMT+5,X'0C'         MAKE POSITIVE                            
         EDIT  (P6,PPGBAMT),(11,0(R2)),2,ALIGN=LEFT                             
         ZAP   PPGBAMT,SVBAMT          RESTORE FOR LINE ITEMS                   
         LA    R2,12(R2)               MAXED OUT DATA + 1                       
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
         MVC   0(4,R2),=C'T549'        PAYMENT TERMS                            
*                                                                               
         CLC   QAGY,=C'OU'             AGENCY OU?                               
         BE    *+14                                                             
         CLC   QAGY,=C'OO'             AGENCY OO?                               
         BNE   *+10                                                             
         MVC   0(4,R2),=C'T056'        PAYMENT TERMS FOR OO/OU                  
*                                                                               
         CLC   QAGY,=C'UB'             AGENCY UB?                               
         BNE   *+10                                                             
         MVC   0(4,R2),=C'T012'        PAYMENT TERMS FOR UB                     
*                                                                               
         MVI   4(R2),C'|'              DELIMITER                                
         LA    R2,5(R2)                BUMP R2                                  
*                                                                               
         LA    R1,C'1'                 LINE ITEM COUNT FOR US                   
         CLC   QAGY,=C'OU'             AGENCY OU?                               
         BNE   *+8                     NO - LINE ITEM COUNT IS 1                
         AHI   R1,1                    YES - BUMP LINE ITEM COUNT               
         STC   R1,0(R2)                LINE ITEM COUNT                          
*                                                                               
         L     R1,=A(SGTTAPE)          ADD RECORD TO SGTTAPE                    
         LA    R0,P                    P HAS LINE DATA                          
         PUT   (1),(0)                 PUT HEADER RECORD TO TAPE                
*                                                                               
*        LINE ITEM RECORD(S)                                                    
*                                                                               
         MVI   SEQNUM,C'1'             INIT SEQ NUM                             
*                                                                               
         ZAP   MYPACK,PPGBAMT          START WITH BILL AMOUNT                   
         SP    MYPACK,PPGGST           SUBTRACT GST                             
         SP    MYPACK,PPGPST           SUBTRACT PST                             
*                                                                               
FTP15    ZAP   MYPPOS,MYPACK           COPY VALUE FROM MYPACK                   
         NI    MYPPOS+5,X'F0'          STRIP SIGN BITS                          
         OI    MYPPOS+5,X'0C'          MAKE POSITIVE                            
         LA    R6,MYPACK               R6 = BILL AMOUNT-TAXES                   
         LA    R7,MYPPOS               R7 = +(BILL AMOUNT-TAXES)                
*                                                                               
         MVC   P,SPACES                INIT P                                   
         LA    R2,P                    R2 = P                                   
*                                                                               
FTP20    MVC   0(3,R2),=C'FI|'         INIT TO POSITIVE AMOUNT                  
         LA    R3,3                    BUMP BY 3                                
         TM    5(R6),X'0D'             IS 'REAL' AMOUNT IS NEGATIVE?            
         BNO   *+14                    NO                                       
         MVC   0(4,R2),=C'CRM|'        YES - CRM = NEGATIVE                     
         LA    R3,4                    BUMP BY 4                                
         AR    R2,R3                   BUMP R2                                  
*                                                                               
         MVC   0(1,R2),SEQNUM          LINE COUNTER                             
         MVI   1(R2),C'|'              DELIMITER                                
         LA    R2,2(R2)                BUMP R2                                  
*                                                                               
         CLC   QAGY,=C'UB'             AGENCY UB?                               
         BNE   FTP25                   NO                                       
         LA    R1,PPGALLO              GL ACCOUNT NUMBER IN EST UCOMM 1         
         CLI   NETPAKSW,C'Y'           NET SYSTEM?                              
         BNE   FTP35                   NO                                       
         CLC   PPGPU2(3),=C'EQ '       YES - PRODUCT EQ?                        
         BNE   FTP35                   NO                                       
         LA    R1,=C'14060013'         GL ACCOUNT NUMBER FOR N/EQ               
         B     FTP35                   DONE WITH GL ACCOUNT NUMBER              
*                                                                               
FTP25    CLC   QAGY,=C'OO'             AGENCY OO?                               
         BNE   FTP30                   NO                                       
         LA    R1,=C'33110001'         GL ACCOUNT NUMBER FOR OO MED T           
         CLI   PPGMED,C'T'             MEDIA T?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'33140002'         GL ACCOUNT NUMBER FOR OO R/X             
         CLI   PPGMED,C'R'             MEDIA R?                                 
         BE    FTP35                   YES                                      
         CLI   PPGMED,C'X'             MEDIA X?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'33110002'         GL ACCOUNT NUMBER FOR OO N/C/S           
         CLI   PPGMED,C'N'             MEDIA N?                                 
         BE    FTP35                   YES                                      
         CLI   PPGMED,C'C'             MEDIA C?                                 
         BE    FTP35                   YES                                      
         CLI   PPGMED,C'S'             MEDIA S?                                 
         BE    FTP35                   YES                                      
         B     FTP34                   DONE WITH GL ACCOUNT NUMBER              
*                                                                               
FTP30    LA    R1,=C'12041011'         FOR TAX LINE                             
         CLI   SEQNUM,C'1'             TAX LINE?                                
         BH    FTP35                   YES                                      
         LA    R1,=C'33110002'         GL ACCOUNT NUMBER FOR OU T/N             
         CLI   PPGMED,C'T'             MEDIA T?                                 
         BE    FTP35                   YES                                      
         CLI   PPGMED,C'N'             MEDIA N?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'33140002'         GL ACCOUNT NUMBER FOR OU R               
         CLI   PPGMED,C'R'             MEDIA R?                                 
         BE    FTP35                   YES                                      
         OI    ERRORSW,X'40'           UNKNOWN MEDIA TYPE WARNING OU            
*                                                                               
FTP34    LA    R1,=C'        '         BLANK GL ACCOUNT NUMBER                  
*                                                                               
FTP35    MVC   0(8,R2),0(R1)           GL ACCOUNT NUMBER                        
         MVI   8(R2),C'|'              DELIMITER                                
         LA    R2,9(R2)                BUMP R2                                  
*                                                                               
         CLC   QAGY,=C'UB'             AGENCY UB?                               
         BNE   FTP40                   NO                                       
*                                                                               
         MVC   WORK(2),PPGMOS          MOS (MMYY)                               
         MVI   WORK+2,X'01'            DUMMY DAY                                
         GOTO1 DATCON,DMCB,(3,WORK),(X'20',WORK+20)                             
*                                                                               
         MVC   0(2,R2),=C'20'          CENTURY                                  
         MVC   2(2,R2),WORK+20         YEAR                                     
***      MVC   0(4,R2),PPGEU2          ESTIMATE YEAR (YYYY)                     
         MVI   4(R2),C'-'              DASH                                     
*                                                                               
         EDIT  (B2,PPGEST),(3,5(R2)),FILL=0                                     
         MVI   8(R2),C'-'              DASH                                     
*                                                                               
         MVC   9(2,R2),WORK+22         MM                                       
         MVC   11(2,R2),WORK+20        YY                                       
         MVI   13(R2),C'-'             DASH                                     
*                                                                               
         MVC   14(15,R2),PPGENAME      ESTIMATE DESCRIPTION                     
*                                                                               
         LA    R2,30(R2)               MAXED OUT DATA + 1                       
         B     FTP44                   DONE                                     
*                                                                               
FTP40    CLC   QAGY,=C'OU'             AGENCY OU?                               
         BNE   FTP42                   NO                                       
         CLI   SEQNUM,C'1'             TAX LINE?                                
         BE    FTP41                   NO                                       
         MVC   0(15,R2),=C'786587329RT0001'                                     
         LA    R2,15(R2)               BUMP R2                                  
         B     FTP44                   DONE                                     
*                                                                               
FTP41    CLI   PPGMED,C'T'             MEDIA T?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'N'             MEDIA N?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'R'             MEDIA R?                                 
         BNE   FTP43                   NO - UNKNOWN MED - LEAVE BLANK           
*                                                                               
FTP42    MVI   0(R2),C'S'              SYSTEM                                   
         MVC   1(1,R2),PPGMED          MEDIA                                    
*                                                                               
FTP43    MVI   2(R2),C'-'              DASH                                     
*                                                                               
         LA    R3,17                   MAX LENGTH FOR ESTIMATE UDEF 1           
         MVC   3(3,R2),PPGCLI          CLIENT CODE                              
         LA    R2,5(R2)                LAST CHARACTER OF CLIENT CODE            
         CLI   0(R2),X'40'             CLIENT CODE HAS 3RD CHARACTER?           
         BNH   *+10                    NO - DON'T BUMP R2                       
         LA    R2,1(R2)                YES - BUMP R2                            
         BCTR  R3,0                    DECREMENT R3                             
         MVI   0(R2),C'-'              DASH                                     
*                                                                               
         MVC   1(3,R2),PPGPRO          PRODUCT CODE                             
         LA    R2,3(R2)                LAST CHARACTER OF PRODUCT CODE           
         CLI   0(R2),X'40'             CLIENT CODE HAS 3RD CHARACTER?           
         BNH   *+10                    NO - DON'T BUMP R2                       
         LA    R2,1(R2)                YES - BUMP R2                            
         BCTR  R3,0                    DECREMENT R3                             
         MVI   0(R2),C'-'              DASH                                     
*                                                                               
         EDIT  (B2,PPGEST),(3,1(R2)),FILL=0                                     
         MVI   4(R2),C'-'              DASH                                     
         LA    R2,4(R2)                BUMP R2                                  
*                                                                               
         CLC   QAGY,=C'OU'             AGENCY OU?                               
         BE    FTP43U                  YES, DON'T REPEAT PRODUCT CODE           
         CLC   QAGY,=C'OO'             AGENCY OO?                               
         BE    FTP43U                  YES, DON'T REPEAT PRODUCT CODE           
         SHI   R3,3                    DECREMENT FOR 2-CHAR PRD CODE            
         MVC   0(3,R2),PPGPRO          PRODUCT CODE                             
         LA    R2,3(R2)                LAST CHARACTER OF PRODUCT CODE           
         CLI   0(R2),X'40'             CLIENT CODE HAS 3RD CHARACTER?           
         BNH   *+10                    NO - DON'T BUMP R2                       
         LA    R2,1(R2)                YES - BUMP R2                            
         BCTR  R3,0                    DECREMENT R3                             
         MVI   0(R2),C'-'              DASH                                     
*                                                                               
FTP43U   BCTR  R3,0                    DECREMENT FOR EX                         
         EX    R3,*+8                  EXECUTE MVC                              
         B     *+10                    FOR IDF                                  
         MVC   1(0,R2),PPGEU1          ESTIMATE UDEF 1                          
         LA    R2,1(R3,R2)             BUMP R2                                  
*                                                                               
FTP44    BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
FTP45    MVI   0(R2),C'|'              DELIMITER (ALLOCATION CODE)              
         LA    R2,1(R2)                BUMP R2                                  
*                                                                               
         EDIT  (P6,0(R7)),(11,0(R2)),2,ALIGN=LEFT  POSITIVE AMOUNT              
         LA    R2,11(R2)               MAXED OUT DATA + 1                       
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
         CLC   QAGY,=C'UB'             AGENCY UB?                               
         BNE   FTP50                   NO                                       
         CLI   NETPAKSW,C'Y'           NET SYSTEM?                              
         BNE   FTP50                   NO                                       
         CLC   PPGPU2(3),=C'EQ '       YES - PRODUCT EQ?                        
         BNE   FTP50                   NO                                       
         MVI   0(R2),C'|'              DELIMITER (ALLOCATION CODE)              
         LA    R2,1(R2)                BUMP R2                                  
         B     FTP55                                                            
                                                                                
FTP50    CLC   QAGY,=C'OU'             AGENCY OU?                               
         BNE   *+12                    NO                                       
         CLI   SEQNUM,C'2'             TAX LINE 2?                              
         BE    *+10                    YES                                      
         MVC   0(2,R2),=C'1Z'          TAX CODE                                 
         LA    R2,2(R2)                BUMP R2                                  
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
FTP55    MVI   0(R2),C'|'              DELIMITER (WITHOLDING TAX CODE)          
         LA    R2,1(R2)                BUMP R2                                  
*                                                                               
         CLC   QAGY,=C'UB'             AGENCY UB?                               
         BNE   FTP60                   NO                                       
         CLI   NETPAKSW,C'Y'           NET SYSTEM?                              
         BNE   FTP61                   NO                                       
         CLC   PPGPU2(3),=C'EQ '       YES - PRODUCT EQ?                        
         BNE   FTP61                   NO                                       
         MVI   0(R2),C'|'              DELIMITER (ALLOCATION CODE)              
         LA    R2,1(R2)                BUMP R2                                  
         B     FTP65                                                            
*                                                                               
FTP60    CLC   QAGY,=C'OU'             AGENCY OU?                               
         BNE   FTP61                   NO                                       
         CLI   SEQNUM,C'2'             TAX LINE 2?                              
         BE    *+10                    YES                                      
         MVC   0(4,R2),=C'CA00'        TAX JURSIDICTION                         
         LA    R2,4(R2)                BUMP R2                                  
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
         B     FTP65                                                            
*                                                                               
FTP61    MVC   0(10,R2),=C'9999999999' TAX JURSIDICTION                         
         MVI   10(R2),C'|'             DELIMITER                                
         LA    R2,11(R2)               BUMP R2                                  
*                                                                               
FTP65    LA    R1,PPGEU1               EST UCOMM2 FOR AGENCY UB                 
         CLC   QAGY,=C'UB'             AGENCY UB?                               
         BE    *+8                     YES                                      
         LA    R1,PPGPU2               NO - USE PRD UDEF 2 FOR OO/OU            
         CLC   QAGY,=C'OU'             AGENCY OU?                               
         BNE   FTP66                   NO                                       
         CLI   SEQNUM,C'1'             TAX LINE?                                
         BNE   FTP67                   YES - NO INTERNAL ORDER NUMBER           
FTP66    MVC   0(10,R2),0(R1)          INTERNAL ORDER NUM                       
         LA    R2,11(R2)               MAXED OUT DATA + 1                       
FTP67    BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
         MVI   0(R2),C'|'              ONE MORE DELIMITER FOR THE END           
*                                                                               
         L     R1,=A(SGTTAPE)          ADD RECORD TO SGTTAPE                    
         LA    R0,P                    P HAS LINE DATA                          
         PUT   (1),(0)                 PUT LINE RECORD TO TAPE                  
         MVI   P,X'40'                 INIT P TO SPACES                         
         MVC   P+1(L'P-1),P            INIT P TO SPACES                         
         CLC   QAGY,=C'OU'             AGENCY OU?                               
         BNE   FTPX                    NO                                       
         CLI   SEQNUM,C'1'             NEED TAX LINES?                          
         BNE   FTPX                    NO - ALREADY DID THEM                    
*                                                                               
         ZAP   MYPACK,PPGGST           GST (INCLUDES HST)                       
         AP    MYPACK,PPGPST           PST (HAD HST TAKEN OUT)                  
         MVI   SEQNUM,C'2'             ADDITIONAL LINE FOR TAXES                
         B     FTP15                   GO AGAIN                                 
*                                                                               
         DROP  R4,R5                   DROP USINGS                              
*                                                                               
FTP70    CLOSE (SGTTAPE)               CLOSE SGTTAPE                            
*                                                                               
         LA    R1,MQMAPNM              MQ MAP NAME                              
         MVC   P+1(49),0(R1)           MOVE FILE NAME TO PRINT LINE             
         GOTO1 REPORT                  REPORT THE FILE NAME                     
         MVI   P,X'40'                 INIT P TO SPACES                         
         MVC   P+1(L'P-1),P            INIT P TO SPACES                         
*                                                                               
         CLI   TESTMQ,C'N'             SUPRESSING MQ NOTIFICATION?              
         BE    FTPX                    YES - EXIT                               
*                                                                               
         LA    R5,ELEM                 SEND MQ MESSAGE WITH FILE NAME           
         USING MQMSGD,R5               MQ MESSAGE DSECT                         
         MVI   ELEM,C' '               INIT ELEM TO SPACES                      
         MVC   ELEM+1(L'ELEM-1),ELEM   SPACE PAD                                
         LA    R1,MQMAPNM              MQ MAP NAME                              
         MVC   MQFILE(34),14(R1)       BIL.SYS.AGID.DYYMMDD.THHMMSS             
         MVC   MQDATE(6),28(R1)        YYMMDD OF FILE NAME                      
         MVC   MQTIME(6),36(R1)        HHMMSS OF FILE NAME                      
*                                                                               
         MVI   DMCB+8,X'A0'            SUPPRESS LENGTH FOR MSG & HDR            
*                                                                               
***                                                                             
* DO NOT USE THIS CODE. IT DOES NOT WORK. DMMQRPT SETS QMGRNAME TO              
* MQ1T. THIS IS OUT OF DATE. ACCORDING TO DDCPUINFO, IT SHOULD BE               
* SET TO MQ7T. IF YOU LOOK IN EJES, YOU'LL SEE THAT EDICT MQ MESSAGES           
* ARE WRITTEN TO EDISKE7T, EDISKE7Q AND EDISKE7C                                
***                                                                             
*                                                                               
***      CLI   TESTMQ,C'T'             TEST MQ RUN?                             
***      BNE   *+8                     NO                                       
***      OI    DMCB+8,X'01'            YES -PUT TO TEST MQ BROKER               
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'MEDIACOMSFTP****'),,0             
         CLI   DMCB+8,0                ANY ERRORS?                              
         BE    *+6                     NO                                       
         DCHO                          YES - DEATH                              
*                                                                               
         MVC   MQHID,=CL6'DANOT1'      HUB RECORD ID                            
         MVC   MQSYS,MQFILE+4          SYSTEM                                   
*                                                                               
         MVC   MQQUAL(7),=C'BILLING'   QUALIFIER                                
         L     RF,VMASTC               A(MASTC)                                 
         USING MASTD,RF                MASTD DSECT                              
         L     R1,MCAEXTRA             EXTRA DATA AREA                          
         USING MCEXTRA,R1              EXTRA DATA AREA DSECT                    
         MVC   MQAGYID,MCAGYCOD        AGY LABEL (CTAGCCOD) ON IDI REC          
         CLC   SVAGENCY,=C'UB'         AGENCY UB?                               
         BNE   *+10                    NO                                       
         MVC   MQAGYID,=C'R7NY'        UBNY IS ALREADY TAKEN                    
         DROP  R1,RF                   DROP MASTD/MCEXTRA USINGS                
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                          
         CLI   DMCB+8,0                ANY ERRORS?                              
         BE    *+6                     NO                                       
         DCHO                          YES - DEATH                              
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                ANY ERRORS?                              
         BE    *+6                     NO                                       
         DCHO                          YES - DEATH                              
*                                                                               
         MVC   P+1(49),MQMAPNM         FILE NAME                                
         GOTO1 REPORT                  PRINT FILE NAME                          
*                                                                               
FTPX     XIT1                          EXIT                                     
         DROP  R5                      DROP USINGS                              
         LTORG                                                                  
*                                                                               
FINDLAST CLI   0(R2),X'40'             HAVE DATA HERE?                          
         BH    *+8                     YES - DELIMITER GOES 1 AFTER             
         BCT   R2,*-8                  NO - GO BACK 1 CHAR & TRY AGAIN          
         MVI   1(R2),C'|'              DELIMITER                                
         LA    R2,2(R2)                BUMP R2                                  
         BR    RE                                                               
*                                                                               
SPGT02   CSECT                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
         MACRO                                                                  
&NAME    AGSPC &AGENCY=,&MEDIA=ALL,&TAPE=,&BLKSIZE=,&LRECL=,           +        
               &SPECS=NONE,&ESTIMATE_AMOUNTS=NO,&RECFM=FB                       
         LCLC  &TC,&MED,&CTRL                                                   
         LCLA  &CTRL1,&CTRL2,&CTRL3,&CTRL4                                      
.*                                                                              
&CTRL1   SETA  0                                                                
&CTRL2   SETA  0                                                                
&CTRL3   SETA  0                                                                
&CTRL4   SETA  0                                                                
.*                                                                              
&TC      SETC  ' '                                                              
         AIF   (T'&TAPE EQ 'O').A10                                             
&TC      SETC  '&TAPE'                                                          
.*                                                                              
.A10     ANOP                                                                   
&MED     SETC  'Z'                                                              
         AIF   ('&MEDIA' EQ 'ALL').A20                                          
&MED     SETC  '&MEDIA'                                                         
.*                                                                              
.A20     AIF   ('&ESTIMATE_AMOUNTS' NE 'YES').A30                               
&CTRL1   SETA  &CTRL1+128                                                       
.*                                                                              
.A30     ANOP                                                                   
&RFM     SETC  'F'                                                              
         AIF   ('&RECFM' NE 'VB').A40                                           
&RFM     SETC  'V'                                                              
.*                                                                              
.A40     ANOP                                                                   
&NAME    DC    C'&AGENCY&MED&TC',AL2(&BLKSIZE,&LRECL)                           
         DC    C'&RFM'                                                          
         AIF   ('&SPECS' NE 'NONE').A50                                         
         DC    AL3(0)                                                           
         AGO   .A60                                                             
.A50     DC    AL3(&SPECS)                                                      
.A60     DC    AL1(&CTRL1,&CTRL2,&CTRL3,&CTRL4)                                 
         SPACE 1                                                                
         MEXIT                                                                  
         MEND                                                                   
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
INVTAB   DS    0D                                                               
         ORG   *+(INVMAX*3)                                                     
         DC    X'00'                                                            
*                                                                               
PPGTABLE CSECT                                                                  
         ORG   *+(7000*PPGTLEN)                                                 
         DC    X'00'                                                            
PPGRECD  DSECT                                                                  
PPGMED   DS    CL1           MEDIA (ALSO IN KEY)                                
PPGCLI   DS    CL3           CLIENT (ALSO IN KEY)                               
PPGEST   DS    XL2           ESTIMATE (ALSO IN KEY)                             
PPGIDATE DS    XL6           INVOICE DATE                                       
PPGINVF  DS    CL10          FULL INVOICE NUMBER                                
PPGMOS   DS    XL2           FROM BKEYYSRV AND BKEYMSRV                         
PPGEU1   DS    CL32          EST USER 1                                         
PPGEU2   DS    CL16          EST USER 2                                         
PPGENAME DS    CL20          ESTIMATE NAME                                      
PPGEBRT  DS    CL1           BARTER INDICATOR                                   
PPGAOR   DS    CL1           AOR BILL INDICATOR                                 
PPGPU1   DS    CL32          PRD USER 1 (LEGAL ENTITY)                          
PPGPU2   DS    CL32          PRD USER 1 (LEGAL ENTITY-MEDIACOM)                 
PPGALLO  DS    CL32          ALLOCATION - POL EST UCOM                          
*        THE FIELDS WILL BE THE VALUES FOR THE FIRST BILL                       
*        FOR THE BINSRCH KEY                                                    
*                                                                               
*        THE FIELDS BELOW ARE THE TOTALS FOR THE INVOICE                        
*        (BINSRCH KEY)                                                          
PPGBAMT  DS    PL6                                                              
PPGGST   DS    PL6                                                              
PPGPST   DS    PL6                                                              
PPGRECL  EQU   *-PPGMED                                                         
*                                                                               
LEDIS    EQU   PPGPU1-PPGMED     DISPLACEMENTS                                  
AMTDIS   EQU   PPGBAMT-PPGMED                                                   
GSTDIS   EQU   PPGGST-PPGMED                                                    
PSTDIS   EQU   PPGPST-PPGMED                                                    
*                                                                               
         EJECT                                                                  
BLINED   DSECT                                                                  
BLINE    DS    0CL132                                                           
         DS    CL3                                                              
BLPRD    DS    CL3                                                              
         DS    CL1                                                              
BLPNUM   DS    CL5                                                              
         DS    CL1                                                              
BLEST    DS    CL3                                                              
         DS    CL1                                                              
BLPER    DS    CL6                                                              
         DS    CL1                                                              
BLINVNO  DS    CL7                                                              
         DS    CL1                                                              
BLRUND   DS    CL8                                                              
         DS    CL1                                                              
BLINVD   DS    CL8                                                              
         DS    CL1                                                              
BLDUED   DS    CL8                                                              
         DS    CL1                                                              
BLTYPE   DS    CL3                                                              
BLGROSS  DS    CL14                                                             
BLACTUAL DS    CL14                                                             
BLNET    DS    CL14                                                             
BLAC     DS    CL14                                                             
BLGST    DS    CL14                                                             
         ORG   BLGST+132           PST ON 2ND LINE                              
BLPST    DS    CL14                                                             
*                                  FOR NON-CANADIAN AGENCIES, GST/PST           
*                                  ISN'T NEEDED, SO WIDEN $ COLUMNS             
         ORG   BLGROSS                                                          
         DS    C                                                                
BLGRSWID DS    CL15                GROSS                                        
         DS    C                                                                
BLACTWID DS    CL15                ACTUAL                                       
         DS    C                                                                
BLNETWID DS    CL15                NET                                          
         DS    C                                                                
BLACWIDE DS    CL15                AGENCY COMMISSION                            
         DS    CL6                 SPARE                                        
         ORG                                                                    
*                                                                               
         EJECT                                                                  
SPGTWRKD DSECT                                                                  
*                                                                               
VDLFLD   DS    A                                                                
VDOWNLD  DS    A                                                                
VFTPHUB  DS    A                                                                
AMQRPT   DS    A                                                                
VDDUCOM  DS    A                                                                
ATOTS    DS    A                                                                
RELO     DS    A                                                                
SVBAMT   DS    PL6                                                              
TAXSW    DS    XL1                                                              
ERRORSW  DS    XL1                                                              
*        X'01' MISSING ACCOUNT NUMBERS                                          
*        X'02' MISSING INTERNAL ORDER NUMBERS                                   
*        X'04' MISSING LEGAL ENTITIES                                           
*        X'08' MIXED LEGAL ENTITES ON AN INVOICE                                
*        X'80' MISSING POL ESTIMATE                                             
*        X'40' UNKNOWN MEDIA TYPE                                               
*                                                                               
SAVVEND  DS    CL09                                                             
SAVFILE  DS    CL13                                                             
SAVFMT   DS    CL1       FROM VENTAB - O=OLD, N=NEW  FORMAT                     
SVMOS    DS    CL4                                                              
MYSVEST  DS    CL3                                                              
SEQNUM   DS    CL1                                                              
SVLINE   DS    X                                                                
SVFORCEH DS    CL1                                                              
MYPPOS   DS    PL6                                                              
MYPACK   DS    PL6                                                              
*                                                                               
MYDUB    DS    PL8                                                              
*                                                                               
EATOTGRS DS    PL6                                                              
EATOTAC  DS    PL6                                                              
EATOTACT DS    PL6                                                              
EATOTNET DS    PL6                                                              
*                                                                               
SVKEY    DS    CL64         KEY AND KEYSAVE                                     
*                                                                               
RECLEN   DS    H                                                                
ELCODE   DS    X                                                                
DOWNACT  DS    C                                                                
RETAIL   DS    C                                                                
REVSW    DS    C                   REVISION SWITCH                              
BYTE2    DS    X                                                                
LSTBLKY  DS    XL13                LAST BILL KEY                                
B1XPROF  DS    CL16                                                             
B1PROF   DS    CL16                                                             
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                FULL FORMAT INVOICE NUMBER                   
CONTROLS DS    0XL4                                                             
CONTROL1 DS    X                                                                
CONTROL2 DS    X                                                                
CONTROL3 DS    X                                                                
CONTROL4 DS    X                                                                
OFFICE   DS    CL2                 WAS CL1                                      
SVQST    DS    CL6                                                              
SVQEND   DS    CL6                                                              
STARTMOS DS    XL2                 START MOS FILTER (BINARY YM)                 
ENDMOS   DS    XL2                 END MOS FILTER (BINARY YM)                   
CNTRY    DS    C                                                                
SVMID    DS    CL132                                                            
NEWCLT   DS    C                                                                
NETPAKSW DS    C                                                                
SKIPBILL DS    X                                                                
JWOASW   DS    C                                                                
HYMCSW   DS    C                   MCMMP                                        
YEARDIG  DS    XL1                 YEAR DIGIT                                   
DECADE   DS    XL1                                                              
WIAPLSW  DS    C                   WESTERN/APL SWITCH                           
WIQASW   DS    C                   WESTERN/APL QA SWITCH                        
WAPLBTSW DS    C                   YET ANOTHER MODE                             
*                                                                               
CLFILT   DS    CL3                                                              
MEFILT   DS    CL1                                                              
CTODAY   DS    CL8                 YYYYMMDD                                     
TIMEOFD  DS    CL8                 HH.MM.SS                                     
*                                                                               
MQMAPNM  DS    CL14                SFTPDISK.PROD.                               
*                                                                               
DSNAME   DS    CL35                DSN -  BIL.SYS.AGID.DYYYMMDD.THHMMSS         
*                                                                               
SVAGENCY DS    CL2                 SAVE OFF AGENCY AT REQF                      
TESTMQ   DS    CL1                 TEST MQ BROKER FLAG                          
GTSFTP   DS    CL1                 FTP VIA THE HUB FLAG                         
GTOPENSW DS    CL1                 GT TAPE OPEN SWITCH                          
ELEM     DS    XL(MQMSGLNQ)        BUILD MQ MESSAGE HERE                        
*                                                                               
INVPARS  DS    6F                  INVOICE BINSRCH PARS                         
*                                                                               
INVMAX   EQU   50000               MAX INVOICES PER CLT                         
*                                                                               
CLTZINV  DS    F                   CLIENT ZERO INVOICES                         
*                                                                               
CLTINVS  DS    F                   CLIENT INVOICES                              
OFFINVS  DS    F                   OFFICE INVOICES                              
REQINVS  DS    F                   REQUEST INVOICES                             
RUNINVS  DS    F                   RUN INVOICES                                 
*                                                                               
BAMTS    DS    (NAMTS)PL6          BILL TOTALS                                  
SVBAMTS  DS    (NAMTS)PL6          SAVED TOTALS                                 
PAMTS    DS    (NAMTS)PL6          PRODUCT TOTS                                 
CAMTS    DS    (NAMTS)PL6          CLIENT                                       
OAMTS    DS    (NAMTS)PL6          OFFICE                                       
RQAMTS   DS    (NAMTS)PL6          REQUEST                                      
RAMTS    DS    (NAMTS)PL6          RUN                                          
*                                                                               
BDEHSW   DS    XL1                                                              
BDEH1    DS    CL132                                                            
BDEH2    DS    CL132                                                            
BDEH3    DS    CL132                                                            
BDEH4    DS    CL132                                                            
         EJECT                                                                  
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
SVBILL   DS    XL256                                                            
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
UCALL    EQU   *-UCTTLS                                                         
USAVKEY  DS    XL13             TO SAVE CURRENT READ SEQUENCE                   
UCOMQ    EQU   *-UCOMBLK                                                        
         SPACE 3                                                                
AMOUNTSD DSECT                                                                  
AMTGRS   DS    PL6                 GROSS                                        
AMTAC    DS    PL6                 ACTUAL COMMISSION                            
AMTACT   DS    PL6                 ACTUAL                                       
AMTNET   DS    PL6                 NET                                          
AMTCNT   DS    PL6                 COUNT                                        
AMTGST   DS    PL6                 GST                                          
AMTPST   DS    PL6                 PST                                          
AMTHST   DS    PL6                 HST                                          
NAMTS    EQU   (*-AMOUNTSD)/6                                                   
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM                                       
MQAGYID  DS    CL4                 AGENCY 1D 4-CHAR                             
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 YYMMDD OF DSN                                
MQTIME   DS    CL6                 HHMMSS OF DSN                                
MQDATA1  DS    CL32                NOT USED                                     
MQDATA2  DS    CL32                NOT USED                                     
MQFILE   DS    CL64                DSN  (MINUS SFTPDISK.PROD.)                  
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         ORG   QAREA+49                                                         
QMOS     DS    0CL8                REQUESTED MONTH-OF-SERVICE RANGE             
QMOSSTRT DS    CL4                 START MOS (YYMM)                             
QMOSEND  DS    CL4                 END MOS (YYMM)                               
         ORG                                                                    
QOPT6    EQU   QGRP                                                             
QOPT7    EQU   QGRP+1                                                           
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAOR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDGETPROFD                                                     
         EJECT                                                                  
       ++INCLUDE DDUCOMD                                                        
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NECOMBLOK                                                      
         PRINT ON                                                               
       ++INCLUDE DDOFFICED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085SPREPGT02 01/15/19'                                      
         END                                                                    
