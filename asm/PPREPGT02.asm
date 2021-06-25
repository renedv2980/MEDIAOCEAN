*          DATA SET PPREPGT02  AT LEVEL 066 AS OF 11/21/19                      
*PHASE PPGT02A                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE OFFOUT                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE DDUCOM                                                                 
*INCLUDE GETUSER                                                                
*                                                                               
         TITLE 'PPGT02 - - PRINTAK P&&G INTERFACE'                              
***********************************************************************         
* USER    JIRA        DATE                  CHANGE LOG                *         
* ---- ------------ -------- -----------------------------------------*         
* AKAT SPEC-39693   11/21/19 CHANGES FOR GROUPM                       *         
* YKVA SPEC-31432  HOTFIX  UPDATE PMT TERMS FOR UB FROM T193 TO T012  *         
* AKAT SPEC-27182   09/19/18 RESET ION ERROR FLAG FOR EACH REQUEST    *         
* YKVA SPEC-25921  7/13/18 UPDATE PAYMENT TERMS FOR UB TO T193        *         
* YKVA SPEC-25748  7/09/16 UPDATE PAYMENT TERMS FOR OU/CAN TO T056    *         
* YKVA SPEC-25741  7/09/16 UPDATE PAYMENT TERMS FOR OO/US TO T056     *         
* YKVA SPEC-16673   DS 18.1  CHANGE TO EDI BILLING FOR P&G CARAT      *         
* AKAT SPEC-14370   11/27/17 H&S FILE FOR P&G: FIX CUT OFF EST UDEF1  *         
* YKVA SPEC-14370   07/31/17 H&S FILE FOR P&G: GL CODE, LINE ITEM CAN *         
* YKVA SPEC-14692   07/31/17 H&S FILE FOR P&G: GL CODE, LINE ITEM US  *         
* AKAT SPEC-10457   06/13/17 NEW GL CODES FOR P&G                     *         
* SMUR SPEC-10110  03/02/17 REMOVE ZERO DOLLAR INVOICES               *         
* AKAT SPSUG-86     10/19/16 UPDATE PAYMENT TERMS FOR OO/OU TO T549   *         
* AKAT SPSUG-85     10/19/16 UPDATE PAYMENT TERMS FOR OO/OU TO T549   *         
* AKAT CUSTENH-3279 06/14/16 NEW EDI FILE FOR OMG USA                 *         
* AKAT CUSTENH-3280 06/14/16 NEW EDI FILE FOR OMG CANADA              *         
* AKAT CSD-477      06/14/16 NEW EDI FILE FOR CARAT                   *         
***********************************************************************         
* BPLA 11/2014   CHANGE VENDOR CODE FOR HY 10056957 TO                          
*                15282520-ONLY FOR CLTS PGC AND PRG                             
*                                                                               
* BPLA 10/2014   NEW HY CLIENT - PRG                                            
*                                                                               
* BPLA 09/2014   CHANGE TO ALLOW 4 CHARACTER LEGAL ENTITY CODES                 
*                                                                               
* BPLA 06/2014   CHANGE TO ACCOUNT # FOR H7                                     
*                EFFECTIVE JUL01/1024                                           
*                                                                               
* BPLA 10/13 ALTERATION FOR SMGTO  (O0)                                         
*            GET LEGAL ENTITY FROM PRD USER 1                                   
*            NOT FROM PRD USER 2 (3) (NORMAL FOR SAVFMT=N)                      
*            INTERNAL ORDER FROM 2ND EST USER NOT 1ST PRD USER                  
*            SMGTO IS A MIX OF SAVFMT=N AND O FOR CLIENT PSM                    
*                                                                               
* BPLA 10/13 ADD MOS (MMM/YY) TO THE CPE FIELD FOR CATALYST                     
*                                                                               
* BPLA 07/13 NEW CLIENT PSM FOR SMGTO (O0)                                      
*            FORMAT LIKE H7                                                     
*                                                                               
* BPLA 07/13 LIMIT LINE ITEM TEXT TO 30 CHARS                                   
*                                                                               
* BPLA 07/13 NEW CLIENT P13 FOR HY                                              
*                                                                               
* BPLA 04/13 CHANGE TO LINE ITEM TEST FIELD FOR HY                              
*                                                                               
* BPLA 09/12 NEW AGENCY AND CLIENT H7 + PG0 (FORMAT=C)                          
*            CHANGE FOR MEDIACOM HY LEGAL ENTITY TO                             
*            BE FROM PRD USER2 - INSTEAD OF HARDCODED                           
*                                                                               
* BPLA 04/12 TAX JURISDICTION CHANGE FOR CLT PPM                                
*                                                                               
* BPLA 08/11 CHANGES FOR GROUP M                                                
*            HARD CODE LEGAL ENTITY TO 283                                      
*            (IT WAS IN PRODUCT USER 1)                                         
*            PRODUCT USER 1 WILL BE INTERNAL ORDER NUMBER                       
*            (IT WAS IN ESTIMATE USER 2)                                        
*            SEE STATEMENTS THAT BEGIN WITH **T1TEST                            
*            THEY ARE FOR TESTING THE NEW FORMAT FOR AGENCY T1                  
*            WHICH NOW HAS AN ENTRY IN NTHE VENTAB FOR CLIENT PG1               
*                                                                               
* BPLA 11/10 NEW AGENCY (HY)                                                    
*                                                                               
* BPLA 10/07 PAYMENT TERMS CHANGE FOR MEDIA I CLIENT HPG                        
*            TICKET# 0148589N                                                   
*                                                                               
* BPLA 10/07 PAYMENT TERMS CHANGE FOR MEDIA I CLIENTS                           
*            PG1 AND PGG                                                        
*                                                                               
* YKVA 10/06 PAYMENT TERMS CHANGE FOR MEDIA I, CLIENTS:                         
*            PG1, PGG, AND HPG FROM T007 TO T008                                
*                                                                               
* BPLA 02/06 PAYMENT TERMS CHANGE FOR STARCOM CLIENTS                           
*            PG,PGN, AND PGO FROM T055 TO T007                                  
*                                                                               
* BPLA 11/05 IF MEDIAVEST AND ACCOUNT NUMBER IS 14080026                        
*            THE SET INTERNAL ORDER #, TAX CODE, AND TAX                        
*            JURISDICTION TO SPACES.                                            
*                                                                               
*    BPLA   10/05  PAYMENT TERMS CHG FOR CLIENT PGB (BROMLEY)                   
*                  WAS T229 CHG TO T054                                         
*                                                                               
*    BPLA  10/05   NEW CLIENT HPG - PAYMENT TERMS T011                          
*                                                                               
*    BPLA  10/04   PAYMENT TERMS FOR MEDIAVEST CHANGED TO T148                  
*                                                                               
*    BPLA  12/04   VENDOR CODE CHANGED FOR AGENCY O0                            
*                                                                               
         TITLE 'PPGT02 - PRINTPAK P&&G INTERFACE'                               
*                                                                               
*                                                                               
* QOPT1        'F' = PRINT FORMULAS - NOT ACTIVE                                
* QOPT2        'R' = PRINT REVERSAL INFO                                        
* QOPT3        'N' = NO PRINTING                                                
* QOPT4        ' ' = NO OUTPUT FILE, Y= PRODUCE OUTPUT FILE                     
* QOPT5        'A' = ACCOUNT CODE FROM AAA PRD EST USER 1 -OLD WAY              
*                    DEFAULT IS FROM BRAND NOW                                  
*                    WILL GET AUTOMATICALLY SET FOR AGENCY O0                   
*                    (CANADIAN)                                                 
* QOPT6         AOR OPTION                                                      
*               A=AOR ONLY                                                      
*               B=AOR ONLY AND AOR/CLIENT                                       
*               X=EXCLUDE AOR                                                   
*               BLANK=AOR AND NON-AOR                                           
*              'C' = COMMISION ONLY BILLS                                       
* QOPT7         B - EXCULDE BDE TRANSFER LINES                                  
*                                                                               
*  NOTE THAT QOPT7 IS SET DIRECTLY IN JCL NOT VIA REQ!                          
*                                                                               
* QOPT7     N     = NO MQ NOTIFICATION (NOT SET VIA REQ)                        
*           T     = TEST MQ NOTIFICATION (NOT SET VIA REQ)                      
*           P     = PROD MQ NOTIFICATION (NOT SET VIA REQ)                      
*           BLANK = TREAT AS PRODUCTION RUN (SAME AS P)                         
***********************************************************************         
PPGT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PPGT02                                                       
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,PPGT02+4095                                                   
         LA    R9,1(R9)                                                         
         USING PPGT02+4096,R9           ** NOTE USE OF R9 AS                    
*                                          SECOND BASE REGISTER                 
         LA    R8,SPACEND                                                       
         USING PPGTWRKD,R8                                                      
         LA    R7,P                                                             
         USING BILLINED,R7                                                      
*                                                                               
         RELOC RELO                                                             
         CLI   MODE,PROCBIL                                                     
         BE    PRBIL                                                            
         CLI   MODE,FBUYREQ                                                     
         BE    FBLR                                                             
         CLI   MODE,LBUYREQ                                                     
         BE    LBLR                                                             
***OFF                                                                          
         CLI   MODE,OFCFRST                                                     
         BE    FBOFF                                                            
         CLI   MODE,OFCLAST                                                     
         BE    LBOFF                                                            
***OFF                                                                          
         CLI   MODE,FBUYCLI        FRIST FOR CLIENT                             
         BE    FBC                                                              
         CLI   MODE,LBUYCLI       LAST FOR CLIENT                               
         BE    LBCLI                                                            
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
RUNF     DS    0H                  RUN FIRST                                    
         MVI   ERRORSW,0                                                        
         MVI   DOWNACT,0           CLEAR DOWNLOAD ACTIVITY                      
         MVI   GTOPENSW,C'N'       SET GT TAPE NOT OPEN                         
         L     R4,PPWORK2C                                                      
         USING PPWORK2D,R4                                                      
         MVC   ADMASTC,VMASTC                                                   
         DROP  R4                                                               
*                                                                               
*        BUILD BDE HEADERS                                                      
*        WILL BE SENT WITH FILE TO THE PRINTQUE                                 
*                                                                               
         MVC   BDEH1,SPACES                                                     
         MVC   BDEH2,SPACES                                                     
         MVC   BDEH3,SPACES                                                     
         MVC   BDEH4,SPACES                                                     
         MVC   BDEH1+4(19),=C'*HDR*EDICT=STARPROD'                              
         MVI   BDEH1+34,C'W'       FOR WIDE                                     
         MVC   BDEH2(5),=C'++DDS'                                               
         MVC   BDEH2+6(08),=C'PRBGTTRN'                                         
         MVC   BDEH3(5),=C'++DDS'                                               
         MVC   BDEH3+11(03),=C'SUB'                                             
         MVC   BDEH3+15(17),=C'P&&G SAP INTERFACE'                              
         MVC   BDEH4(5),=C'++DDS'                                               
         MVC   BDEH4+11(03),=C'FIL'                                             
         MVC   BDEH4+15(17),=C'F_XN_AGENCY_TEST_'                               
*                                                                               
         MVI   BDEHSW,0         SET HEADERS NOT SENT                            
         B     EXIT                                                             
*                                                                               
FBLR     DS    0H                  FIRST FOR REQUEST                            
*                                                                               
         MVC   SVAGENCY,QAGENCY SAVE OFF AGENCY                                 
         MVI   HYOSW,C'N'       SET OFF OUTRIDER SWITCH                         
         MVI   HYMSW,C'N'       SET OFF MEDIACOM SWITCH                         
         MVI   HYMCSW,C'N'      SET OFF MCMMP SWITCH                            
         CLC   QAGENCY,=C'HY'   OUTRIDER - NO BDE HEADERS                       
         BNE   FBLR0                                                            
         MVI   BDEHSW,1         SAY THEY WERE ALREADY SENT                      
         MVI   HYOSW,C'Y'       SET OUTRIDER SWITCH                             
         CLC   QCLIENT,=C'PGC'  CHECK OUTRIDER CLIENTS                          
         BE    FBLR0X                                                           
         CLC   QCLIENT,=C'PRG'  CHECK OUTRIDER CLIENTS                          
         BE    FBLR0X                                                           
         CLC   QCLIENT,=C'PGI'                                                  
         BE    FBLR0X                                                           
         CLC   QCLIENT,=C'PGO'                                                  
         BE    FBLR0X                                                           
         MVI   HYOSW,C'N'                                                       
         MVI   HYMSW,C'Y'       FOR OTHER HY CLIENTS                            
         MVI   HYMCSW,C'Y'      FOR MCMMP HY CLIENTS                            
         CLC   QCLIENT,=C'PG1'  CHECK MCMMP CLIENTS                             
         BE    FBLR0X                                                           
         CLC   QCLIENT,=C'P12'                                                  
         BE    FBLR0X                                                           
         CLC   QCLIENT,=C'P13'                                                  
         BE    FBLR0X                                                           
         MVI   HYMCSW,C'N'      SET OFF MCMMP HY CLIENTS                        
         B     FBLR0X                                                           
*                               SET MEDIACOM SW                                 
FBLR0    CLC   QAGENCY,=C'H7'   MINDSHARE - CATALYST                            
         BNE   FBLR0B                                                           
         MVI   BDEHSW,1         SAY THEY WERE ALREADY SENT                      
         B     FBLR0X                                                           
*                                                                               
FBLR0B   CLC   QAGENCY,=C'O0'   MEDIAVEST - TORONTO                             
         BNE   FBLR0X                                                           
         MVI   QOPT5,C'A'       USE PRD EST USER FOR ACCOUNT#                   
*                               (THE OLD WAY)                                   
         CLC   QCLIENT,=C'PSM'                                                  
         BNE   FBLR0X                                                           
         MVI   HYOSW,C'N'                                                       
         MVI   HYMCSW,C'Y'     LIKE SOME HY CLIENTS                             
*                                                                               
FBLR0X   LA    RF,PESTREC                                                       
         ST    RF,ADESM                                                         
         LA    RF,PBUYREC   PRD AAA ESTIMATE READ INTO PBUYREC                  
         ST    RF,AAAEST                                                        
         LA    RF,PPRDREC                                                       
         ST    RF,ADPRO                                                         
         LA    RF,PCLTREC                                                       
         ST    RF,ADCLI                                                         
*                                                                               
         L     RF,=V(GETUSER)                                                   
         A     RF,RELO                                                          
         ST    RF,VGETUSER                                                      
*                                                                               
         L     RF,=A(DOWNLD)                                                    
         A     RF,RELO                                                          
         ST    RF,VDOWNLD                                                       
*                                                                               
         L     RF,=V(DDUCOM)                                                    
         A     RF,RELO                                                          
         ST    RF,VDDUCOM                                                       
*                                                                               
         L     RF,=V(DLFLD)                                                     
         A     RF,RELO                                                          
         ST    RF,VDLFLD                                                        
*                                                                               
         L     RF,=A(FTPHUB)                                                    
         A     RF,RELO                                                          
         ST    RF,VFTPHUB                                                       
*                                                                               
         L     RF,=A(GETVEN)                                                    
         A     RF,RELO                                                          
         ST    RF,VGETVEN                                                       
*                                                                               
         L     RF,=V(PPBVAL)                                                    
         A     RF,RELO                                                          
         ST    RF,VPPBVAL                                                       
*                                                                               
         L     RF,=V(PPFMTINO)                                                  
         A     RF,RELO                                                          
         ST    RF,VPFMTINO                                                      
*                                                                               
         L     RF,=V(OFFOUT)                                                    
         A     RF,RELO                                                          
         ST    RF,VOFFOUT                                                       
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   AMQRPT,MCVMQRPT                                                  
         DROP  RF                                                               
*                                                                               
         CLI   QOPT7,C'B'   SEE IF SUPPRESSING BDE FILE HEADERS                 
         BNE   *+8          (IF TRANSFER NOT READY)                             
         MVI   BDEHSW,1      SO PROGRAM WILL THINK THEY'RE ALREADY SENT         
*                            AND NOT SEND THEM                                  
*                                                                               
         SR    R0,R0            SET INVOICE LIST BINSRCH PARS                   
         L     R1,=A(INVTAB)                                                    
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,4                                                             
         LA    R4,4                                                             
         L     R5,=A(INVMAX)    INVMAX IS 10,000 INVS PER CLT                   
         STM   R0,R5,INVPARS                                                    
*                                                                               
         CLI   FIRSTSW,0           FIRST TIME TEST                              
         BNE   FBLR1                                                            
         MVI   FIRSTSW,1                                                        
*                                                                               
         LA    R3,RUNTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         LA    R3,TRUNTOTS         TOTAL FOR TAPE                               
         BAS   RE,CLRTOTS                                                       
*                                                                               
         XC    RUNINVS,RUNINVS      RUN INVOICE TOTALS                          
         XC    TRUNINVS,TRUNINVS    TAPE INVOICE TOTALS                         
*                                                                               
         XC    START(12),START                                                  
         XC    LSTBLKY,LSTBLKY                                                  
         XC    EATOTS,EATOTS       CLEAR ESTIMATE AMTS                          
         MVC   SAVMAX,MAXLINES                                                  
*                                                                               
FBLR1    DS    0H                                                               
*                                                                               
         MVI   RACTSW,0              ZERO REQUEST ACTIVITY                      
         MVI   OACTSW,0              ZERO REQUEST ACTIVITY                      
*                                                                               
FBLR2    CLC   QAGENCY,=C'UB'         AGENCY UB?                                
         BE    FBLR2AA                YES                                       
*                                                                               
         CLC   QAGENCY,=C'OO'         AGENCY OO?                                
         BE    *+14                   YES                                       
         CLC   QAGENCY,=C'OU'         AGENCY OU?                                
         BNE   FBLR2A                 NO                                        
         NI    ERRORSW,X'FF'-X'02' RESET MISSING INTERNAL ORDER NUM ERR         
         NI    ERRORSW,X'FF'-X'04' RESET MISSING LEGAL ENTITY ERROR             
*                                                                               
FBLR2AA  MVI   BDEHSW,1               YES - SUPPRESS BDE HEADERS                
         MVI   GTSFTP,C'Y'            SEND GT FILE VIA THE HUB                  
                                                                                
FBLR2A   CLI   QOPT4,C'N'             SUPPRESS TAPE?                            
         BE    FBLR3                  YES                                       
         CLI   GTSFTP,C'Y'            SENDING GT FILE VIA THE HUB?              
         BNE   FBLR2X                 NO                                        
         CLI   GTOPENSW,C'Y'          IS GT FILE ALREADY OPEN?                  
         BE    FBLR3                  YES, DSNAME SET & FILE ALLOCATED          
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
         MVC   DSNAME+4(3),=C'PRT'    SFTPDISK.PROD.BIL.PRT                     
         MVI   DSNAME+7,C'.'          SFTPDISK.PROD.BIL.PRT.                    
         L     RF,ADMASTC             A(MASTC)                                  
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
         CLC   QAGENCY,=C'UB'         AGENCY UB?                                
         BNE   *+10                   NO                                        
         MVC   DSNAME+8(4),=C'R7NY'   UBNY IS ALREADY TAKEN                     
         DROP  R1,RF                  DROP MASTD/MCEXTRA USINGS                 
*                                                                               
         MVC   DSNAME+12(2),=C'.D'    SFTPDISK.PROD.BIL.PRT.D                   
         MVC   DSNAME+14(6),CTODAY+2  YYMMDD                                    
         MVC   DSNAME+20(2),=C'.T'    SFTPDISK.PROD.BIL.PRT.DYYMMDD.T           
         MVC   DSNAME+22(2),TIMEOFD   SFTPDISK.PROD.BIL.PRT.DYYMMDD.T..         
         MVC   DSNAME+24(2),TIMEOFD+3 SFTPDISK.PROD.BIL.PRT.DYYMMDD.T..         
         MVC   DSNAME+26(2),TIMEOFD+6 SFTPDISK.PROD.BIL.PRT.DYYMMDD.T..         
         MVC   MQMAPNM,=C'SFTPDISK.PROD.'                                       
         CLI   QOPT7,C'P'             PRODUCTION RUN?                           
         BE    FBLR2C                 YES                                       
         CLI   QOPT7,C'N'             SUPPRESS MQ NOTIFICATION?                 
         BNE   *+12                   NO                                        
         MVI   TESTMQ,C'N'            YES - SUPPRESS MQ NOTIFICATION            
         B     FBLR2B                 WRITE TO TEST MQ BROKER                   
         CLI   QOPT7,C'T'             WRITE TO TEST MQ BROKER?                  
         BNE   FBLR2C                 NO - TREAT AS PRODUCTION RUN              
         MVI   TESTMQ,C'T'            WRITE TO TEST MQ BROKER                   
*                                                                               
FBLR2B   MVC   MQMAPNM+9(4),=C'TEST'  WRITE TO SFTPDISK.TEST                    
*                                                                               
FBLR2C   MVI   BYTE,X'45'             BIG NAMES                                 
         MVC   DUB,=X'000005000001'                                             
         GOTO1 DYNALLOC,DMCB,(X'80',=C'PGTTAPE '),(BYTE,DUB),                   
               (X'80',MQMAPNM)                                                  
*                                                                               
         OPEN  (PGTTAPE,(OUTPUT))     OPEN THE TAPE                             
         LTR   RF,RF                  ANY ERRORS?                               
         BZ    *+6                    NO                                        
         DC    H'0'                   YES - DEATH                               
         MVI   GTOPENSW,C'Y'          SET GT FILE OPEN                          
         B     FBLR3                  DO NOT INIT DOWNLOAD                      
*                                                                               
FBLR2X   GOTO1 VDOWNLD,DMCB,(RA)      INITIALIZE DOWNLOAD                       
*                                                                               
FBLR3    DS    0H                                                               
         LA    R3,PRDTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         LA    R3,CLTTOTS                                                       
         BAS   RE,CLRTOTS                                                       
*                                                                               
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
*                                                                               
         LA    R3,REQTOTS                                                       
         BAS   RE,CLRTOTS                                                       
*                                                                               
         XC    CLTINVS,CLTINVS   CLEAR CLIENT AND REQ INVOICE TOTALS            
         XC    CLTZINV,CLTZINV   CLIENT ZERO INVOICE                            
         XC    REQINVS,REQINVS                                                  
*                                                                               
         XC    LASTKEY,LASTKEY                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   TOTSW,0                                                          
         OC    START(12),START                                                  
         BNZ   *+10                                                             
         MVC   START(12),QSTART    SET START-END FROM FRST REQ                  
         CLC   START(12),SPACES                                                 
         BNE   FBLR4                                                            
         MVC   START+00(2),RCDATE+06   YY                                       
         MVC   START+02(2),RCDATE+00   MM                                       
         MVC   START+04(2),RCDATE+03   DD       ***   CONVERT START             
         GOTO1 DATCON,DMCB,(0,START),(0,START)  *** TO NEW DATE FORMAT          
         MVC   END,START                                                        
FBLR4    DS    0H                                                               
         CLC   QSTART(12),SPACES                                                
         BNE   *+10                                                             
         MVC   QSTART(12),START                                                 
         B     EXIT                                                             
         SPACE 3                                                                
         EJECT                                                                  
FBC      DS    0H                  FIRST FOR CLIENT                             
*                                                                               
         GOTO1 VGETVEN,DMCB,(RA)      GET VENDOR CODE & BDE FILE NAME           
*                                                                               
         MVC   BDEH4+32(13),SAVFILE   REST OF FILE NAME                         
*                                                                               
         XC    LASTCPE,LASTCPE                                                  
         XC    PPGRECNT,PPGRECNT     MUST CLEAR FOR BINSRCH                     
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   FBC1                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
*                                                                               
         CLI   QCLIENT,C'*'        ONE OFFICE REQUEST ?                         
         BE    FBC1B               YES                                          
         CLI   QCLIENT,C'$'        ALL OFFICE REQUEST ?                         
         BE    FBC1B               YES                                          
         CLC   QCLIENT,=C'ALL'     ALL CLIENTS REQUEST ?                        
         BNE   FBC1D               NO - ONE CLIENT ONLY REQUEST                 
*                                                                               
FBC1B    MVI   PCLTREC,0           TO CLEAR MEDIA NAME OVERRIDE                 
*                                                                               
FBC1D    MVI   NEWCLT,C'Y'                                                      
         MVC   SVMID,SPACES                                                     
         MVC   SVMID2,SPACES                                                    
         MVC   SVMID(7),=C'CLIENT='                                             
         MVC   SVMID+8(3),PCLTKCLT                                              
         MVC   SVMID+12(L'PCLTNAME),PCLTNAME                                    
         MVC   WORK(8),SPACES                                                   
         CLI   PCLTNUM,X'FF'                                                    
         BNE   FBC2                                                             
         MVI   WORK,C'('                                                        
         UNPK  WORK+1(5),PCLTNUM+1(3)                                           
         MVI   WORK+5,C')'                                                      
         B     FBC4                                                             
*                                                                               
FBC2     MVC   BYTE,PCLTNUM                                                     
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'80'                                                       
         BNE   FBC3                                                             
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PCLTNUM                                                
         NI    FULL+1,X'7F'                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   WORK,C'('                                                        
         UNPK  WORK+1(5),DUB                                                    
         MVI   WORK+6,C')'                                                      
         B     FBC4                                                             
*                                                                               
FBC3     DS    0H                                                               
         MVC   WORK+1(3),PCLTNUM                                                
         OC    WORK+1(3),SPACES                                                 
         CLC   WORK+1(3),SPACES                                                 
         BE    FBC4X                                                            
         MVI   WORK,C'('                                                        
         LA    R1,WORK+3                                                        
FBC3C    CLI   0(R1),C' '                                                       
         BH    FBC3D                                                            
         BCT   R1,FBC3C                                                         
*                                                                               
FBC3D    MVI   1(R1),C')'                                                       
*                                                                               
FBC4     DS    0H                                                               
         LA    R1,SVMID+12+L'PCLTNAME                                           
FBC4C    CLI   0(R1),C' '                                                       
         BH    FBC4E                                                            
         BCT   R1,FBC4C                                                         
FBC4E    MVC   2(8,R1),WORK                                                     
*                                                                               
FBC4X    DS    0H                                                               
         LA    R1,SVMID+12+L'PCLTNAME+10                                        
FBC5     CLI   0(R1),C' '                                                       
         BH    FBC10                                                            
         BCT   R1,FBC5                                                          
*                                                                               
FBC10    LA    R2,SVMID2                                                        
         LA    R1,132(R1)           SAME DISPLACEMENT INTO SVMID2               
         SR    R1,R2                                                            
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'               SOMETING VERY WRONG                           
*                                                                               
         EX    R1,*+8                                                           
         B     FBCX                                                             
         MVC   SVMID2(0),=40C'-'                                                
*                                                                               
FBCX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
LBCLI    DS    0H                  LAST FOR CLIENT                              
*                                  SEND ENTRIES TO DOWNLOAD                     
         L     R2,PPGRECNT         FOR BCT    RECORD COUNT                      
         CH    R2,=H'0'            NO RECORDS                                   
         BE    ZEROCNT                                                          
*                                                                               
*        I HAVE SOMETHING TO SEND                                               
*                                                                               
         CLI   BDEHSW,1           BDE HEADERS ALREADY SENT?                     
         BE    LBCLI3                                                           
*                                                                               
         MVC   P,BDEH1                                                          
         MVC   PSECOND,BDEH2                                                    
*                                                                               
         MVC   SVLINE,LINE                                                      
         MVC   SVFORCEH,FORCEHED                                                
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P,BDEH3                                                          
         MVC   PSECOND,BDEH4                                                    
         MVI   RCWHATPR,2     SET TO SECOND SYSPRINT                            
         GOTO1 REPORT                                                           
*                                                                               
         MVC   LINE,SVLINE             RESTORE LINE                             
         MVC   FORCEHED,SVFORCEH       AND FORCEHED                             
         MVI   RCWHATPR,1     RESET TO FIRST                                    
         MVI   BDEHSW,1           SET BDE HEADERS SENT                          
*                                                                               
LBCLI3   CLC   QAGENCY,=C'OO'     AGENCY OO?                                    
         BE    *+10               YES                                           
         CLC   QAGENCY,=C'OU'     AGENCY OU?                                    
         BNE   LBCLI3A            NO                                            
         TM    ERRORSW,X'02'      MISSING INTERNAL ORDER NUMBER?                
         BNZ   EXIT               YES - FILE WILL NOT BE PRODUCED               
         TM    ERRORSW,X'04'      MISSING LEGAL ENTITIES?                       
         BNZ   EXIT               YES - FILE WILL NOT BE PRODUCED               
*                                                                               
LBCLI3A  L     R3,AOFPPGT                                                       
         LA    R3,L'PPGKEY(R3)     BUMP PAST PSUEDO KEY                         
LBCLI4   CLI   GTSFTP,C'Y'         SENDING GT FILE VIA THE HUB?                 
         BNE   LBCLI5              NO                                           
         GOTO1 VFTPHUB,DMCB,(RA),(R3)                                           
         B     LBCLI6              BUMP TO NEXT ENTRY                           
*                                                                               
LBCLI5   GOTO1 VDOWNLD,DMCB,(RA),(R3)                                           
*                                                                               
LBCLI6   LA    R3,PPGTLEN(R3) TO NEXT RECORD IN TABLE                           
         BCT   R2,LBCLI4                                                        
*                                                                               
ZEROCNT  B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
LBLR     DS    0H                  LAST FOR REQUEST                             
         CLI   QCLIENT,C'$'                                                     
         BE    LBLR2                                                            
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         L     R0,CLTINVS                                                       
         L     RE,CLTZINV          CLIENT ZERO INVOICE COUNT                    
         SR    R0,RE               ADJUST CLIENT INVOICE COUNT                  
         ST    R0,CLTINVS                                                       
*                                                                               
         CLI   RACTSW,0            CHK FOR ACTIVITY                             
         BNE   PRB2B               IF YES - GO FINISH LAST PRD/CLT              
LBLR2    DS    0H                                                               
         CLI   RACTSW,0                                                         
         BE    EXIT                                                             
         LA    R3,REQTOTS                                                       
         LA    R4,RUNTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         L     R0,RUNINVS                                                       
         A     R0,REQINVS                                                       
         ST    R0,RUNINVS                                                       
         CLI   QOPT4,C'N'                                                       
         BE    LBLR4                                                            
         LA    R3,REQTOTS                                                       
         LA    R4,TRUNTOTS         ALSO POST TO TAPE TOTALS                     
         BAS   RE,ROLTOTS                                                       
         L     R0,TRUNINVS                                                      
         A     R0,REQINVS                                                       
         ST    R0,TRUNINVS                                                      
*                                                                               
LBLR4    BRAS  RE,PRNT                                                          
         MVC   BLINE(18),=C'**REQUEST TOTALS**'                                 
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,REQINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,REQTOTS                                                       
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         BRAS  RE,PRNT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
LAST     DS    0H                  RUN LAST                                     
         BRAS  RE,PRNT                                                          
         MVC   BLINE(14),=C'**RUN TOTALS**'                                     
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,RUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
         LA    R3,RUNTOTS                                                       
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         BRAS  RE,PRNT                                                          
         MVC   BLINE(15),=C'**FILE TOTALS**'                                    
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,TRUNINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT               
         LA    R3,TRUNTOTS                                                      
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         CLI   ERRORSW,0        ANY ERRORS?                                     
         BE    RUNL10                                                           
         MVC   P+1(36),=C'*** ERRORS HAVE BEEN ENCOUNTERED ***'                 
         BRAS  RE,PRNT                                                          
         TM    ERRORSW,X'01'     ANY MISSING ACCOUNT NUMBERS?                   
         BNO   RUNL1                                                            
         MVC   P+3(31),=C'*** MISSING ACCOUNT NUMBERS ***'                      
         BRAS  RE,PRNT                                                          
RUNL1    TM    ERRORSW,X'20'     ANY MISSING ESTIMATE USER?                     
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
*                                                                               
RUNL6    TM    ERRORSW,X'80'   MISSING AAA ESTIMATE                             
         BNO   RUNL8                                                            
         MVC   P+3(29),=C'*** MISSING AAA ESTIMATES ***'                        
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL8    TM    ERRORSW,X'40'   UNKNOWN MEDIA TYPE?                              
         BNO   RUNL8A          NO                                               
         MVC   P+3(27),=C'*** UNKNOWN MEDIA TYPE  ***'                          
         BRAS  RE,PRNT                                                          
*                                                                               
RUNL8A   DS    0H                                                               
*&&DO                                                                           
RUNL8A   CLC   QAGENCY,=C'OO'   AGENCY OO?                                      
         BE    *+10             YES                                             
         CLC   QAGENCY,=C'OU'   AGENCY OU?                                      
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
RUNL11   CLI   DOWNACT,C'Y'       DOWNLOADING ACTIVITY?                         
         BNE   EXIT                                                             
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         B     EXIT                                                             
*                                  PROCESS BILL                                 
PRBIL    DS    0H                                                               
*                                                                               
         CLC   PBILLDAT,QSTART     DATES FILTER                                 
         BL    EXIT                                                             
         CLI   QEND,C' '                                                        
         BE    PRB01                                                            
         CLC   PBILLDAT,QEND                                                    
         BH    EXIT                                                             
*                                                                               
PRB01    DS    0H                                                               
         CLI   PBRETAIL,X'81'      CORP RETAIL?                                 
         BE    EXIT                                                             
*                                  GET USER FIELDS                              
         CLC   LASTCPE(6),PBILLREC+4  NEW PRODUCT                               
         BE    PRB02                                                            
*                                                                               
         XC    PPUSER1,PPUSER1     CLEAR                                        
         XC    PPUSER2,PPUSER2                                                  
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',ADCLI),(C'P',ADPRO),(0,PPUSER1),    X        
               (0,PPUSER2)                                                      
*                                                                               
PRB02    CLC   LASTCPE,PBILLREC+4  NEW ESTIMATE                                 
         BE    PRB05                                                            
*                                                                               
         MVC   LASTCPE,PBILLREC+4                                               
*                                                                               
         MVI   ESTBRT,C' '         BARTER INDICATOR                             
         CLI   PESTGRPS,C'B'       SEE IF B IN FIRST POSITION                   
         BNE   *+8                                                              
         MVI   ESTBRT,C'B'                                                      
*                                                                               
         XC    EUSER1,EUSER1       CLEAR                                        
         XC    EUSER2,EUSER2                                                    
         XC    UCOMDATA,UCOMDATA                                                
*                                                                               
*        GET INTERNAL ORDER NUMBER FROM PRD/EST - USER 2                        
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'P',ADCLI),(C'E',ADESM),0,(0,EUSER2)             
*                                                                               
         CLI   GTSFTP,C'Y'         AGENCY UB/OO/OU?                             
         BE    *+12                YES - GET EST UDEF1                          
         CLI   QOPT5,C'A'      OLD WAY?                                         
         BE    PRB02C                                                           
*                   NOW DEFAULT IS GET EUSER1 FROM BRAND                        
         GOTO1 VGETUSER,DMCB,(C'P',ADCLI),(C'E',ADESM),(0,EUSER1),0             
*                                                                               
         MVC   MYPPGK,KEY          SAVE PPG'S KEY AND KEYSAVE                   
         CLI   GTSFTP,C'Y'         AGENCY UB/OO/OU?                             
         BNE   *+12                NO                                           
         BAS   RE,READUCOM         YES - READ PRD OR EST UCOMM RECORD           
         B     PRB04               DONE - RESTORE KEY                           
*                                                                               
PRB02C   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(15),PESTKEY                                                  
         MVC   KEY+7(3),=C'AAA'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    PRB03                                                            
*                                                                               
         CLI   QOPT5,C'A'       AAA EST ONLY REQUIRED IF OLD WAY                
         BNE   PRB03U           ALLOCATION NOW FROM PRODUCT'S UCOMM             
*                                                                               
         OI    ERRORSW,X'80'                                                    
         MVC   P+3(37),=C'*** AAA ESTIMATE      NOT ON FILE ***'                
         LA    R3,PBILKEST                                                      
         BAS   RE,CVD                                                           
         MVC   P+20(3),WORK+2                                                   
         BRAS  RE,PRNT                                                          
         B     PRB04                                                            
*                                  TO USE ITS USER FIELDS                       
PRB03    MVC   AREC,AAAEST                                                      
         GOTO1 GETPRT                                                           
         CLI   QOPT5,C'A'     OLD WAY?                                          
         BNE   PRB03U         STILL GET UCOMDATA                                
*                                                                               
*        GET ACCOUNT NUMBER FOR AAA/EST USER 1                                  
*                                                                               
         XC    DMCB(16),DMCB   CLEAR DMCB                                       
         GOTO1 VGETUSER,DMCB,(C'P',ADCLI),(C'E',AAAEST),(0,EUSER1),0            
*                                                                               
*        CALL DDUCOM TO GET PRD AAA'S ESTIMATE'S FIRST UCOMM                    
*                                                                               
PRB03U   LA    R5,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R5                                                       
*                                                                               
         MVC   UCACOMF,VCOMFACS     COMFACS                                     
         MVI   UCSYS,C'P'        SYSTEM TO PRINT                                
         MVC   UCAGY,QAGENCY     AGENCY                                         
         MVC   UCMED,QMEDIA      MEDIA                                          
         MVC   UCCLT,QCLIENT     CLIENT                                         
         MVC   UCPRD,PBILKPRD                                                   
*                                DO UCOMM FOR PRD AAA                           
         OI    UCOPT,UCOEST     RETURN ESTIMATE UCOMMS                          
         MVC   UCEST,PESTKEST                                                   
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK    NEW UCOM CALL SINCE GOTO MACRO                
         CLI   UCERROR,0         TRASHED WRKING STORAGE USED BY DDUCOM          
         BNE   PRB03X       ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BO    PRB03X                                                           
         XC    UCTTLS(UCALL),UCTTLS                                             
         L     R4,UCETTLS     EST TITLES                                        
         MVC   UCTTLS,0(R4)   SAVE INFO IN MY STORAGE                           
         LA    R4,UCTTLS      AS OPPOSED TO RD CHANE                            
         L     RE,UCEDATA     EST DATA                                          
         MVC   UCOMDATA,0(RE)                                                   
         DROP  R5                                                               
*                                                                               
PRB03X DS      0H                                                               
*                                                                               
PRB04    MVC   KEY(64),MYPPGK     RESTORE PPG'S KEY AND KEYSAVE                 
         GOTO1 HIGH                                                             
*                                                                               
PRB05    DS    0H                                                               
         TM    PBILCMSW,X'02'      IS IT A COMMISION ONLY BILL                  
         BNZ   PRB09               YES, OK                                      
         CLI   QOPT6,C'C'          NO, ARE WE SKIPPING OTHERS                   
         BE    EXIT                                                             
*                                                                               
PRB09    DS    0H                                                               
         CLI   QOPT6,C' '            CHECK AOR OPTION                           
         BE    PRB09D                                                           
         CLI   QOPT6,C'A'            ONLY AOR                                   
         BNE   PRB09B                                                           
         TM    PBILCMSW,X'20'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09B   CLI   QOPT6,C'B'            AOR AND AOR/CLIENT                         
         BNE   PRB09C                                                           
         TM    PBILCMSW,X'30'                                                   
         BNO   EXIT                                                             
         B     PRB09D                                                           
*                                                                               
PRB09C   CLI   QOPT6,C'X'            SEE IF EXCLUDING AOR                       
         BNE   PRB09D                                                           
         TM    PBILCMSW,X'20'                                                   
         BO    EXIT                                                             
*                                                                               
PRB09D   DS    0H                                                               
         GOTO1 VPPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
*****                                                                           
*****    SET EFFECTIVE VALUES INTO PBILLREC                                     
*****                                                                           
         MVC   PBILLGRS,PPBVEBG                                                 
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
*                                 SET MYGST AND MYPST AND MYHST                 
         L     R0,PPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYGST,DUB                                                        
         L     R0,PPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
         L     R0,PPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYHST,DUB                                                        
*                                                                               
************                                                                    
******   NOTE USE PPBVEBC FOR CASH DISC                                         
******   DO NOT CALCULATE FROM PBILLGRS AND PBILLBIL (G-CD)                     
************                                                                    
*                                                                               
         CLC   PBILLKEY(14),LSTBLKY  IF FIRST FOR EST/MOS                       
         BE    PRB1                                                             
         MVI   REVSW,C' '          SET NOT A REVISION                           
         MVC   LSTBLKY,PBILLKEY                                                 
         XC    EATOTS,EATOTS                                                    
         B     PRB1D                                                            
*                                                                               
PRB1     DS    0H                                                               
         MVI   REVSW,C'R'          SET IS A REVISION                            
*                                                                               
PRB1D    DS    0H                  ADD TO ESTIMATE AMTS                         
         LA    R2,PBILLGRS                                                      
         LA    R3,EATOTS                                                        
         LA    R4,4                4 FIELDS STARTING AT GROSS                   
*                                                                               
PRB1F    DS    0H                                                               
         ZAP   DUB,0(BPLEQ,R2)                                                  
         CVB   R0,DUB                                                           
         A     R0,0(R3)                                                         
         ST    R0,0(R3)                                                         
*                                                                               
         LA    R2,BPLEQ(R2)                                                     
         LA    R3,4(R3)                                                         
         BCT   R4,PRB1F                                                         
*                                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   PBRETAIL,0                                                       
         BE    PRB2C                                                            
*                                  RETAIL BILL                                  
         CLI   PBRETAIL,X'81'      IGNORE CORP BILLS                            
         BE    EXIT                                                             
         MVI   RETAIL,C'Y'                                                      
*                                                                               
PRB2C    CLI   PBILLMOD,C'P'       BYPASS PRD MODE BILLS                        
         BE    EXIT                                                             
         CLI   PBILLMOD,C'S'       AND SERIES MODE                              
         BE    EXIT                                                             
*                                                                               
         CLC   PBILKCLT,LASTKCLT    SEE IF SAME CLIENT                          
         BE    PRB2C2                                                           
*                                                                               
         MVC   CLTINVS,INVPARS+8    SAVE LAST CLIENT'S TOTAL                    
*                                                                               
         L     R0,CLTINVS                                                       
         L     RE,CLTZINV          CLIENT ZERO INVOICE COUNT                    
         SR    R0,RE               ADJUST CLIENT INVOICE COUNT                  
         ST    R0,CLTINVS                                                       
*                                                                               
         SR    R0,R0            RESET INVOICE LIST BINSRCH PARS                 
         L     R1,=A(INVTAB)    FOR NEW CLIENT I REALLY PROCESS                 
         A     R1,RELO                                                          
         SR    R2,R2                                                            
         LA    R3,4                                                             
         LA    R4,4                                                             
         L     R5,=A(INVMAX)     INVMAX IS 10,000 INVOICE PER CLT               
         STM   R0,R5,INVPARS                                                    
*                                                                               
*                                  USE BINSRCH TO ADD TO INVTAB                 
PRB2C2   DS    0H                                                               
         MVC   WORK(2),PBILKBMN                                                 
         MVC   WORK+2(2),PBILKBNO                                               
         GOTO1 BINSRCH,INVPARS,(1,WORK)                                         
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   QOPT3,C'N'          NO PRINTING                                  
         BE    PRB26                                                            
*                                                                               
PRB2C5   CLC   PBILKCLT(6),LASTKCLT CHK FOR SAME      CLT + PRD                 
         BE    PRB3                YES                                          
         CLC   PBILKCLT,LASTKCLT   CHK FOR CHANGE IN CLT                        
         BNE   PRB2A               FORCE CHG IN PRD ALSO                        
         CLC   PBILKPRD,LASTKPRD                                                
         BE    PRB2D                                                            
*                                                                               
PRB2A    CLI   LASTKPRD,0                                                       
         BE    PRB2D                                                            
*                                                                               
PRB2B    DS    0H                                                               
         MVC   BLPRD,LASTKPRD                                                   
         MVC   BLINE+5(18),=C'**PRODUCT TOTALS**'                               
         LA    R3,PRDTOTS                                                       
         MVI   TOTSW,1                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
         LA    R3,PRDTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         CLI   MODE,LBUYREQ        REQUEST LAST                                 
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
         CLI   MODE,OFCLAST        OR LAST FOR OFFICE                           
         BE    PRB2F               MUST ALSO DO CLIENT TOTALS                   
*                                                                               
         CLC   PBILKCLT,LASTKCLT   SEE IF NEW CLIENT ALSO                       
         BNE   PRB2D               IF YES THEN DON'T CHK PROFILE                
*                                                                               
         CLI   PROGPROF+1,C'Y'     CHK PAGE FOR PRODUCT                         
         BNE   PRB2D                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   NEWCLT,C'Y'         SO CLIENT WILL REPRINT                       
*                                  FOR EACH PRODUCT                             
*                                                                               
PRB2D    CLC   PBILKCLT,LASTKCLT                                                
         BE    PRB3                                                             
*                                                                               
         CLI   LASTKCLT,0          SEE IF FIRST TIME                            
         BNE   PRB2F                                                            
         MVC   LASTPROF,PROGPROF   MUST SAVE PROFILE                            
         B     PRB3                                                             
*                                                                               
PRB2F    MVC   BLPRD,LASTKCLT                                                   
         XC    LASTKPRD(25),LASTKPRD                                            
         MVC   BLINE+5(17),=C'**CLIENT TOTALS**'                                
*                                                                               
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,CLTINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,CLTTOTS                                                       
         MVI   TOTSW,1                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   SPACING,2                                                        
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
*                                  FIRST CHECK PROFILE OF LAST CLIENT           
         CLI   LASTPROF+0,C'Y'     CHK NEW PAGE FOR CLIENT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   LASTPROF+1,C'Y'     OR NEW PAGE FOR PRODUCT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROGPROF+0,C'Y'     CHK NEW PAGE FOR CLIENT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROGPROF+1,C'Y'     OR NEW PAGE FOR PRODUCT                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   LASTPROF,PROGPROF                                                
*                                                                               
***OFF                                                                          
         CLI   QCLIENT,C'$'          SEE IF DOING OFFILCE LIST                  
         BNE   PRB2M                 ROLL CLTTOTS TO OFFTOTS                    
         LA    R3,CLTTOTS                                                       
         LA    R4,OFFTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         L     R0,OFFINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,OFFINVS                                                       
         B     PRB2P                                                            
***OFF                                                                          
PRB2M    LA    R3,CLTTOTS                                                       
         LA    R4,REQTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         L     R0,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,REQINVS                                                       
PRB2P    LA    R3,CLTTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         XC    CLTINVS,CLTINVS                                                  
         XC    CLTZINV,CLTZINV   CLIENT ZERO INVOICE                            
         CLI   MODE,LBUYREQ            RETURN TO REQTOTALS                      
         BE    LBLR2                                                            
         CLI   MODE,OFCLAST            RETURN TO OFFICE TOTALS                  
         BE    LBOFF5                                                           
*                                                                               
PRB3     DS    0H                                                               
         CLI   NEWCLT,C'Y'         SEE IF NEW CLIENT                            
         BNE   PRB4                                                             
         MVC   P,SVMID                                                          
         MVC   PSECOND,SVMID2                                                   
         MVI   ALLOWLIN,5                                                       
         MVI   NEWCLT,C'N'                                                      
         MVI   SPACING,2                                                        
         BRAS  RE,PRNT                                                          
*                                                                               
PRB4     DS    0H                                                               
         MVC   LASTKEY,PBILLKEY                                                 
*                                  CREATE PRINT LINE(S)                         
         MVC   BLPRD,PBILKPRD           PRD                                     
         LA    R3,PBILKEST                                                      
         BAS   RE,CVD                                                           
         MVC   BLEST(3),WORK+2          EST                                     
*                                                                               
PRB8     DS    0H                                                               
         CLI   PBILLPER,C' '                                                    
         BNH   PRB9                                                             
         CLI   PBILLPER,C'M'      MONTHLY?                                      
         BNE   PRB10                                                            
PRB9     GOTO1 DATCON,DMCB,(3,PBILKMOS),(9,BLPER)                               
         B     PRB12                                                            
*                                                                               
PRB10    DS    0H                  SPECIAL BILL PERIOD                          
         MVC   FULL(2),PBILKMOS                                                 
         MVC   FULL+2(1),PBILLSTA                                               
         GOTO1 DATCON,DMCB,(3,FULL),(7,BLPER)                                   
*                                                                               
         MVI   PSECOND+BLPERD-1,C'-'                                            
         MVC   FULL+2(1),PBILLEND                                               
*        GOTO1 (RF),(R1),,(3,WORK)                                              
         GOTO1 DATCON,(R1),,(5,WORK)                                            
         MVC   PSECOND+BLPERD(5),WORK+3                                         
*                                                                               
PRB12    DS    0H                                                               
*                                                                               
         GOTO1 VPFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),                    X        
               (PBILKMED,B1PROF),B1XPROF                                        
*                                                                               
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)   SAVE FULL INVOICE NUMBERR                       
*                                                                               
         L     RF,DMCB+4        ADDRESS OF "SHORT" FORMAT                       
         MVC   DSINVNO(7),0(RF)     MAY INCLUDE DASH                            
         MVC   BLINO+2(4),3(RF)     DON'T MOVE '-'                              
         L     RF,DMCB+8        ADDRESS OF Y/M                                  
         MVC   BLINO(2),0(RF)                                                   
         MVC   DINVNO(6),BLINO                                                  
*                                                                               
PRB13X   DS    0H                                                               
*            SEE IF RETAIL CORP OR DISTRIBUTOR BILL                             
         CLI   PBRETAIL,X'02'                                                   
         BE    PRB13X2                                                          
         CLI   PBRETAIL,X'01'                                                   
         BNE   PRB13X5                                                          
*                                                                               
PRB13X2  MVC   PSECOND+BLRDATD(12),=C'RETAIL ACCT='                             
         MVC   PSECOND+BLRDATD+12(12),PBRACCT                                   
*                                                                               
PRB13X5  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(5,BLRDAT)         RUN DATE             
*                                                                               
*        GOTO1 (RF),(R1),(1,PBILINVD),(4,BLBDAT)       BILL DATE                
         GOTO1 DATCON,(R1),(3,PBILINVD),(7,BLBDAT)      BILL DATE               
*                                                      TYPE OF BILL             
         MVC   BLTYP(2),PBILLTYP                                                
         LA    R3,BLTYP+2                                                       
         CLI   PBILLTYP,C'0'       SEE IF NEW BILL                              
         BNH   PRB14               YES                                          
*                                                                               
         MVC   BLTYP(3),=C'ORI'                                                 
         LA    R3,BLTYP+3                                                       
         CLI   PBILLTYP,C'3'                                                    
         BE    PRB14                                                            
         MVC   BLTYP(3),=C'DET'                                                 
         CLI   PBILLTYP,C'4'                                                    
         BE    PRB14                                                            
         MVC   BLTYP(3),=C'MAN'                                                 
PRB14    DS    0H                                                               
         MVI   0(R3),C'-'                                                       
*                                                                               
         TM    PBILCMSW,X'02'          FOR COMMISSION ONLY BILL                 
         BZ    PRB15                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    PBILCMSW,X'20'        SEE IF ALSO AOR                            
         BO    PRB14D                                                           
         MVC   1(3,R3),=C'COM'                                                  
*                                                                               
PRB14D   CLI   QOPT6,C'C'          TEST TO LEAVE NET                            
         BE    *+10                                                             
         ZAP   PBILLNET,=P'0'      NO, SET NET = 0 (AC = RCVBL)                 
         B     PRB16                                                            
*                                                                               
PRB15    DS    0H                                                               
         MVC   1(3,R3),=C'ADJ'                                                  
         CLI   PBILSEP,C'A'                                                     
         BE    PRB16                                                            
         MVC   1(3,R3),=C'CD '                                                  
         CLI   PBILSEP,C'C'                                                     
         BE    PRB16                                                            
         MVC   1(3,R3),=C'AOR'                                                  
         TM    PBILCMSW,X'20'        AOR BILL                                   
         BO    PRB16                                                            
         MVC   1(3,R3),=C'UFC'                                                  
         TM    PBILCMSW,X'01'         UP FRONT COMMISSION                       
         BO    PRB16                                                            
         MVC   1(3,R3),=C'NET'                                                  
         TM    PBILCMSW,X'08'         UP FRONT COMMISSION - NET                 
         BO    PRB16                                                            
*                                                                               
         MVC   1(3,R3),=C'REG'                                                  
*                                                                               
PRB16    DS    0H                                                               
*                                                                               
         ZAP   BILTOTS+30(6),MYGST                                              
         ZAP   BILTOTS+36(6),MYPST                                              
*                                                                               
         CLI   QOPT6,C'A'          SEE IF AOR ONLY                              
         BE    PRB16A              YES - THEN INCLUDE IN TOTALS                 
         TM    PBILCMSW,X'20'      SEE IF AOR BILL                              
         BZ    PRB16B                                                           
PRB16A   ZAP   BILTOTS(6),=P'0'                                                 
         ZAP   BILTOTS+6(6),=P'0'                                               
         ZAP   BILTOTS+12(6),=P'0'                                              
         ZAP   BILTOTS+18(6),PBILLRCV    SET AC TO RCVBL                        
         ZAP   BILTOTS+24(6),PBILLRCV    ACTUAL                                 
         AP    BILTOTS+24(6),MYGST                                              
         AP    BILTOTS+24(6),MYPST                                              
         ZAP   BILTOTS+30(6),MYGST                                              
         ZAP   BILTOTS+36(6),MYPST                                              
         B     PRB16E                                                           
*                                                                               
*        SET MATS IN 5 FIELDS FOR ACCUMES                                       
*        (MATCH 5 PRINT COLUMNS)                                                
*                                                                               
PRB16B   ZAP   BILTOTS+0*BPLEQ(BPLEQ),PBILLGRS     GROSS                        
         ZAP   BILTOTS+1*BPLEQ(BPLEQ),PBILLNET     NET                          
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'                                     
         SP    BILTOTS+2*BPLEQ(BPLEQ),PBILLRCV     CD IS -(RCVBL)               
*                                                                               
         CLI   PBILSEP,C'C'        FOR SEP CD BILL                              
         BE    PRB16C                                                           
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),=P'0'        CD IS 0                      
*                                                                               
         CLI   PBILCDSW,C'S'       IF SEP (ON OTHER BILL) BILL                  
         BE    PRB16C                                                           
         ZAP   BILTOTS+2*BPLEQ(BPLEQ),PPBVEBC      EFFECTIVE CD                 
*                                                                               
PRB16C   ZAP   BILTOTS+4*BPLEQ(BPLEQ),PBILLRCV     RCVBL                        
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYGST        ADD GST TO BILL AMT          
         AP    BILTOTS+4*BPLEQ(BPLEQ),MYPST        ADD PST TO BILL AMT          
         ZAP   BILTOTS+5*BPLEQ(BPLEQ),MYGST        AND GST TOTALS               
         ZAP   BILTOTS+6*BPLEQ(BPLEQ),MYPST        AND PST TOTALS               
*                                                                               
         ZAP   DUB,PBILLRCV                                                     
         TM    PBILCMSW,X'02'      UNLESS COMMISSION ONLY BILL                  
         BNZ   PRB16D                                                           
         TM    PBILBASA,X'04'                      IS CD ALREADY OUT?           
         BNZ   PRB16D                              YES                          
         SP    DUB,BILTOTS+2*BPLEQ(BPLEQ)          NO, TAKE IT OUT NOW          
*                                                                               
PRB16D   DS    0H                                                               
         ZAP   BILTOTS+3*BPLEQ(BPLEQ),DUB          FOR AC CALC                  
         SP    BILTOTS+3*BPLEQ(BPLEQ),PBILLNET     -NET = TRUE AC               
*                                                                               
PRB16E   CLC   QAGENCY,=C'OO'      OMDUSEC (USA)                                
         BE    *+14                                                             
         CLC   QAGENCY,=C'OU'      OMDTOA                                       
         BNE   PRB16G                                                           
         CP    BILTOTS+4*BPLEQ(BPLEQ),=P'0'                                     
         BNE   PRB16G                                                           
*                                                                               
         L     R0,CLTZINV                                                       
         A     R0,=F'1'            ADD ONE TO ZERO INVOICE TOTALS               
         ST    R0,CLTZINV                                                       
         MVC   BLINE(132),SPACES                                                
         B     EXIT                                                             
*                                                                               
PRB16G   LA    R3,BILTOTS                                                       
         BAS   RE,FMTAMTS               $ AMOUNTS                               
*                                                                               
         BRAS  RE,PRNT                                                          
*                                                                               
*****    CLI   QOPT1,C'F'               FORMULA OPTION CODE WAS HERE            
*****    BNE   PRB22                    NO                                      
*                                                                               
PRB22    DS    0H                                                               
         CLI   QOPT2,C'R'               REVERSALS OPTION                        
         BNE   PRB26                    NO                                      
         CLI   PBILLCDT,C'0'                                                    
         BE    PRB26                    NOT REVERSED                            
         CLI   PBILLCDT,0               ANY DATE?                               
         BE    PRB26                    NO - NOT REVERSED                       
         LA    R3,BLINO                                                         
         MVC   0(11,R3),=C'REVERSED BY'                                         
         MVC   12(4,R3),PBILLCAN+2                                              
         MVC   17(2,R3),=C'ON'                                                  
         GOTO1 DATCON,DMCB,(0,PBILLCDT),(5,20(R3))                              
*                                                                               
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
PRB26    DS    0H                                                               
         LA    R3,BILTOTS                                                       
         LA    R4,PRDTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         LA    R3,BILTOTS                                                       
         LA    R4,CLTTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         MVI   RACTSW,1            SET REQUEST ACTIVITY                         
         MVI   OACTSW,1            SET REQUEST ACTIVITY                         
*                                                                               
PRB28    DS    0H                                                               
         BRAS  RE,POSTB             POST BILL TO TABLE                          
         B     EXIT                                                             
*                                                                               
LBOFF    DS    0H                                                               
*                                                                               
         MVC   CLTINVS,INVPARS+8   LAST CLIENT'S INVOICE TOTAL                  
*                                                                               
         L     R0,CLTINVS                                                       
         L     RE,CLTZINV          CLIENT ZERO INVOICE COUNT                    
         SR    R0,RE               ADJUST CLIENT INVOICE COUNT                  
         ST    R0,CLTINVS                                                       
*                                                                               
         CLI   OACTSW,0                                                         
         BNE   PRB2B      MUST DO PRD/CLT TOTALS FIRST                          
*                         WILL RETURN TO LBOFF5                                 
LBOFF5   MVI   BLPRD,C'*'                                                       
         GOTO1 VOFFOUT,DMCB,RCSVOFC,HEXOUT,BLPRD+1                              
         MVC   BLINE+5(17),=C'**OFFICE TOTALS**'                                
*                                                                               
         MVI   OACTSW,0                                                         
*                                                                               
         CLC   OFFTOTS(24),=4PL6'0'                                             
         BNE   LBOFF8                                                           
         MVC   BLINE+25(11),=C'NO ACTIVITY'                                     
         BRAS  RE,PRNT                                                          
         B     EXIT                                                             
*                                                                               
LBOFF8   DS    0H                                                               
         MVC   BLINE+25(9),=C'INVOICES='                                        
         EDIT  (B4,OFFINVS),(7,BLINE+34),0,COMMAS=YES,ALIGN=LEFT                
*                                                                               
         LA    R3,OFFTOTS                                                       
         MVI   TOTSW,2                                                          
         BAS   RE,FMTAMTS                                                       
         MVI   MAXLINES,99                                                      
         BRAS  RE,PRNT                                                          
         LA    R3,OFFTOTS                                                       
         LA    R4,REQTOTS                                                       
         BAS   RE,ROLTOTS                                                       
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
*                                                                               
         L     R0,REQINVS        ROLL OFFICE INVOICE TOTAL                      
         A     R0,OFFINVS        TO REQUEST                                     
         ST    R0,REQINVS                                                       
         XC    OFFINVS,OFFINVS                                                  
*                                                                               
         B     EXIT                                                             
*                                                                               
FBOFF    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTKEY,LASTKEY                                                  
         MVI   OACTSW,0                                                         
         LA    R3,OFFTOTS                                                       
         BAS   RE,CLRTOTS                                                       
         XC    OFFINVS,OFFINVS                                                  
         B     EXIT                                                             
*                                                                               
READUCOM NTR1                                                                   
*                                                                               
         LA    R5,UCOMBLK          UCOMM CONTROL BLOCK                          
         XC    UCOMBLK,UCOMBLK     CLEAR UCOMM BLOCK                            
         USING DDUCOMD,R5          UCOMM DSECT                                  
*                                                                               
         MVC   UCACOMF,VCOMFACS    A(COMFACS)                                   
         MVI   UCSYS,C'P'          SPOT SYSTEM                                  
         MVC   UCAGY,QAGENCY       AGENCY/MEDIA                                 
         MVC   UCMED,PBILKMED      MEDIA                                        
         MVC   UCCLT,PBILKCLT      CLIENT                                       
         MVC   UCPRD,PBILKPRD      PRODUCT                                      
         MVC   UCEST,PBILKEST      SET ESTIMATE                                 
         OI    UCOPT,UCOEST        RETURN ESTIMATE UCOMMS                       
         CLC   QAGENCY,=C'UB'      AGENCY UB?                                   
         BE    *+8                 YES - UB GETS JUST ESTIMATE UCOMMS           
         OI    UCOPT,UCOPRD        RETURN PRODUCT UCOMMS FOR OO/OU              
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK     CALL DDUCOM                                  
*                                                                               
         CLI   UCERROR,0           ANY ERRORS?                                  
         BNE   RDUCOMX             YES - HANDLE THEM LATER BASED ON AGY         
*                                                                               
         CLC   QAGENCY,=C'UB'      AGENCY UB?                                   
         BNE   RDUCOM10            NO                                           
         TM    UCDATA,UCDNOEST     YES - DID WE GET THE EST UCOMMS?             
         BO    RDUCOMX             NO - EXIT                                    
         L     R4,UCEDATA          EST DATA (FOR AGENCY UB)                     
         B     RDUCOM20            YES - SAVE TO UCOMDATA                       
*                                                                               
RDUCOM10 TM    UCDATA,UCDNOPRD     YES - DID WE GET THE PRD UCOMMS?             
         BO    RDUCOM30            NO                                           
         L     R4,UCPDATA          PRD DATA (FOR AGENCIES OO & OU)              
*                                                                               
RDUCOM20 MVC   UCOMDATA,0(R4)      PRD OR EST UCOMM DATA                        
*                                                                               
RDUCOM30 CLC   QAGENCY,=C'OO'      AGENCY OO?                                   
         BE    *+14                YES                                          
         CLC   QAGENCY,=C'OU'      AGENCY OU?                                   
         BNE   RDUCOMX             NO                                           
         XC    UCOMDATA+32(8),UCOMDATA+32 USING THIS FOR EST UCOMM 1            
         TM    UCDATA,UCDNOEST     YES - DID WE GET THE EST UCOMMS?             
         BO    RDUCOMX             NO - EXIT                                    
         L     R4,UCEDATA          EST DATA FOR AGENCIES OO & OU                
         MVC   UCOMDATA+32(8),0(R4)   EST UCOMM1 DATA                           
         DROP  R5                  DROP UCOMM DSECT USING                       
*                                                                               
RDUCOMX  J     EXIT                RETURN                                       
*                                                                               
FMTAMTS  NTR1                                                                   
*                                                                               
         MVI   STAGSW,0                                                         
         CP    0(6,R3),=P'9999999999'   COMPARE GROSS TO 100 MILLION            
         BL    *+8                                                              
         MVI   STAGSW,1            SET TO STAGGER TOTALS                        
*                                                                               
         LA    R5,0(R3)            GROSS                                        
         LA    R4,BLGRS                                                         
         LH    R6,=H'-1'          USED WHEN STAGGERING                          
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,6(R3)           NET                                           
         LA    R4,BLNET                                                         
         LA    R6,131              NEXT LINE -1                                 
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,12(R3)           CD                                           
         LA    R4,BLCD                                                          
         LH    R6,=H'-1'                                                        
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,18(R3)           AC                                           
         LA    R4,BLAC                                                          
         LA    R6,131              NEXT LINE-1                                  
         BAS   RE,FMTEDT                                                        
*                                                                               
         LA    R5,24(R3)           ACTUAL                                       
         LA    R4,BLBIL                                                         
         LH    R6,=H'-1'                                                        
         BAS   RE,FMTEDT                                                        
*                                                                               
         CLI   PAGYNAT,C'C'        SEE IF CANADIAN                              
         BNE   FMTAX                                                            
*                                                                               
         LA    R5,30(R3)           GST (PAST IS AT 36(R3))                      
         LA    R4,BLGST                                                         
         BAS   RE,FMTEDT8       PRINT PST UNDER GST                             
*                                                                               
FMTAX    MVI   TOTSW,0                                                          
         B     EXIT                                                             
*                                                                               
FMTEDT   DS    0H                                                               
         CLI   STAGSW,1       SEE IF STAGGERING                                 
         BNE   FMTEDT5                                                          
         AR    R4,R6          ADJUST R4 FOR STAGGERING                          
         EDIT  (P6,0(R5)),(16,0(R4)),2,COMMAS=YES,CR=YES                        
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   14(R4),C'C'                                                      
         BER   RE                                                               
         MVI   14(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   15(R4),C'*'                                                      
         BR    RE                                                               
*                                                                               
FMTEDT5  EDIT  (P6,0(R5)),(15,0(R4)),2,COMMAS=YES,CR=YES                        
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   13(R4),C'C'                                                      
         BER   RE                                                               
         MVI   13(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   14(R4),C'*'                                                      
         BR    RE                                                               
*                                                                               
FMTEDT8  DS    0H               SPECIAL EDT FOR GST AND PST                     
         EDIT  (P6,0(R5)),(14,0(R4)),2,COMMAS=YES,CR=YES                        
         EDIT  (P6,6(R5)),(14,132(R4)),2,COMMAS=YES,CR=YES                      
*                                                                               
         CLI   TOTSW,0                                                          
         BER   RE                                                               
         CLI   12(R4),C'C'                                                      
         BE    FMTEDT8C                                                         
         MVI   12(R4),C'*'                                                      
         CLI   TOTSW,1                                                          
         BE    FMTEDT8C                                                         
         MVI   13(R4),C'*'                                                      
*                                                                               
FMTEDT8C DS    0H                                                               
         CLI   12+132(R4),C'C'                                                  
         BER   RE                                                               
         MVI   12+132(R4),C'*'                                                  
         CLI   TOTSW,1                                                          
         BER   RE                                                               
         MVI   13+132(R4),C'*'                                                  
*                                                                               
         BR    RE                                                               
*                                                                               
ROLTOTS  DS    0H                                                               
         LA    R0,7           FOR BCT                                           
ROLTOTS2 DS    0H                                                               
         AP    0(6,R4),0(6,R3)                                                  
         LA    R4,6(R4)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,ROLTOTS2                                                      
         BR    RE                                                               
*                                                                               
CLRTOTS  DS    0H                                                               
         LA    R0,7                                                             
         ZAP   0(6,R3),=PL6'0'                                                  
         LA    R3,6(R3)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
CVD      DS    0H                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),0(R3)                                                  
*                                                                               
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         BR    RE                                                               
*                                                                               
POSTB    NTR1                                                                   
*                                                                               
         LA     R3,PPGREC                                                       
         USING  PPGRECD,R3                                                      
         XC     PPGKEY,PPGKEY          BE SURE KEY IS CLEAR                     
         MVC    PPGIDATE,PBILINVD                                               
         MVC    PPGINVF,DINVFULL      FULL INVOICE NUMBER                       
         MVC    PPGMOS,PBILKMOS                                                 
*                                                                               
         MVC    PPGKEBRT,ESTBRT       BARTER INDICATOR                          
         MVC    PPGEBRT,ESTBRT       BARTER INDICATOR                           
*                                                                               
         MVC    PPGEU1,EUSER1+21      JUST THE VALUE                            
         MVC    PPGEU2,EUSER2+21      JUST THE VALUE                            
         OC     PPGEU1,SPACES                                                   
         OC     PPGEU2,SPACES                                                   
*                                                                               
         CLC    QAGENCY,=C'OO'    AGENCY OO?                                    
         BE     POSTC             YES - FLAG IF EST UDEF 1 MISSING              
         CLC    QAGENCY,=C'OU'    AGENCY OU?                                    
         BE     POSTC             YES - FLAG IF EST UDEF 1 MISSING              
         CLC    QAGENCY,=C'UB'    AGENCY UB?                                    
         BE     POST2             YES - UB DOESN'T NEED THIS FIELD              
         CLC    QAGENCY,=C'H7'    SEE IF GROUPM-CATALYST                        
         BE     POST2             SKIP THIS CHECK-ACCOUNT# HARDCODED            
*                                                                               
         CLI    PPGEU1,C'X'       IF BEGINS WITH X- CONSIDER MISSING            
         BE     POST1             BUT DISPLAY VALUE                             
POSTC    CLC    PPGEU1,SPACES                                                   
         BH     POST2                                                           
         B      POST1E                                                          
*                                                                               
POST1    MVC    PSECOND+9(7),=C'VALUE ='                                        
         MVC    PSECOND+16(L'PPGEU1),PPGEU1                                     
*                                                                               
POST1E   MVC    P+1(37),=C'** WARNING ** MISSING ESTIMATE USER 1'               
         CLC    QAGENCY,=C'OO'    AGENCY OO?                                    
         BE     POST1F            YES - FLAG IF EST UDEF 1 MISSING              
         CLC    QAGENCY,=C'OU'    AGENCY OU?                                    
         BE     POST1F            YES - FLAG IF EST UDEF 1 MISSING              
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
         CLI    GTSFTP,C'Y'      AGENCY UB/OO/OU?                               
         BE     POST3            YES                                            
*                                                                               
         CLC    QAGENCY,=C'H9'   MEDIAVEST                                      
         BNE    POST2D           ALL CLIENTS                                    
*                                                                               
         CLC    PPGEU1(8),=C'14080026'     SPECIAL ACCOUNT CODE                 
         BNE    POST2D                                                          
         MVC    PPGEU2,SPACES     CLEAR INTERNAL ORDER NO.                      
         B      POST3                                                           
*                                                                               
POST2D   CLC    PPGEU2,SPACES                                                   
         BH     POST3                                                           
         CLI    SAVFMT,C'N'       SEE IF NEW FORMAT                             
         BE     POST3   BELOW FOR FORMATS O (OLD) AND C (GRPM-CATALYST)         
         MVC    P+1(42),=C'**ERROR-MISSING INTERNAL ORD NO. EST USER2'          
         OI     ERRORSW,X'02'                                                   
         BRAS   RE,PRNT                                                         
*                                                                               
POST3    DS     0H                                                              
         MVC    PPGENAME,PESTNAME                                               
*                                                                               
         MVC    PPGCDTYP,PBRACTUL+5   SAVE CD ITEMS INDICATOR                   
*                       NOTE -   PBRACTUL+5 WILL BE PBCDITYP                    
*                                CHANGE AFTER NEW BILLING VERSION               
*                                HAS BEEN INSTALLED                             
*                                                                               
         MVC    PPGPU1,PPUSER1+21     JUST THE FIELD                            
         MVC    PPGPU2,PPUSER2+21     JUST THE FIELD                            
*                                                                               
         CLC    QAGENCY,=C'UB'        AGENCY UB?                                
         BE     POST4                 YES - UB DOESN'T CARE IF MISSING          
         CLC    QAGENCY,=C'OO'        AGENCY OO?                                
         BE     *+10                  YES                                       
         CLC    QAGENCY,=C'OU'        AGENCY OU?                                
         BNE    POST3AA               NO                                        
         CLC    PPGPU2,SPACES         HAVE PRODUCT UDEF 2?                      
         BH     POST4                 YES - OO/OU REQUIRES THIS                 
         B      POST3B                NO - FLAG ERROR                           
*                                                                               
POST3AA  CLC    PPGPU1,SPACES                                                   
         BH     POST3X                                                          
*                                                                               
         CLC    QAGENCY,=C'O0'   SMGTO?                                         
         BE     POST3A                                                          
         CLI    SAVFMT,C'N'     NEW FORMAT?                                     
         BE     POST3B          IF NOT MUST BE O OR C                           
*                                                                               
POST3A   MVC    P+1(38),=C'**ERROR-MISSING LEGAL ENTITY PRD USER1'              
         OI     ERRORSW,X'04'                                                   
         B      POST3T                                                          
*                                                                               
POST3B   MVC    P+1(37),=C'**ERROR-MISSING INTERNAL ORDER NUMBER'               
         OI     ERRORSW,X'02'      MISSING ION                                  
POST3T   BRAS   RE,PRNT                                                         
*                                                                               
POST3X   CLI    SAVFMT,C'N'       NEW FORMAT?                                   
         BNE    POST4                                                           
         CLC    PPGPU2,SPACES                                                   
         B      POST4                                                           
******   CODE BELOW WIIL SEND ERROR IF MISSING                                  
         BH     POST4                                                           
         MVC    P+1(38),=C'**ERROR-MISSING LEGAL ENTITY PRD USER2'              
         OI     ERRORSW,X'04'                                                   
         BRAS   RE,PRNT                                                         
*                                                                               
POST4    DS     0H                                                              
         MVC    PPGALLO,UCOMDATA      POL EST UCOM                              
         OC     PPGALLO,SPACES        - ALLOCATION                              
*                                                                               
         CLC    QAGENCY,=C'UB'        AGENCY UB?                                
         BNE    POST5                 NO                                        
         MVC    PPGEU1,UCOMDATA+32    EST UCOM 2                                
         OC     PPGEU1,SPACES         SPACE PAD                                 
*                                                                               
POST5    CLC    QAGENCY,=C'OO'        AGENCY OO?                                
         BE     *+10                  YES                                       
         CLC    QAGENCY,=C'OU'        AGENCY OU?                                
         BNE    POST6                 NO                                        
         MVC    PPGEUCOM,UCOMDATA+32  EST UCOM 1                                
         CLC    PPGEUCOM,SPACES       HAVE ESTIMATE UCOMM 1?                    
         BNH    POST5A                NO - DON'T PRINT MESSAGE                  
*                                                                               
         MVC    P+1(18),=C'** DEFAULT GL CODE'                                  
*                                                                               
         BRAS   RE,GLCODE             POINT R1 TO DEFAULT GL CODE               
*                                                                               
         MVC    P+20(8),0(R1)         HARDCODED GL ACCOUNT NUMBER               
         MVC    P+29(28),=C'OVERRIDDEN BY ESTIMATE UCOMM'                       
         MVC    P+58(8),PPGEUCOM      ESTIMATE UCOMM 1                          
         MVC    P+67(2),=C'**'        END OF MESSAGE                            
         BRAS   RE,PRNT               PRINT MESSAGE                             
*                                                                               
POST5A   CLC    PPGALLO,SPACES        HAVE PRODUCT UCOMM 1?                     
         BH     POST6                 YES                                       
*                                                                               
         OI     ERRORSW,X'04'         FLAG ERROR                                
         MVC    P+1(38),=C'**ERROR-MISSING LEGAL ENTITY PRD USER1'              
         BRAS   RE,PRNT               PRINT ERROR                               
*                                                                               
POST6    CLI    QOPT4,C'Y'               SEE IF CREATING FILE                   
         BNE    POSTBX                                                          
*                                                                               
         ZAP    PPGBAMT,BILTOTS+24(6) AMOUNT DUE (INCLUDES TAXES)               
         ZAP    PPGGST,MYGST          BILLS GST                                 
         AP     PPGGST,MYHST          ADD HST                                   
         ZAP    PPGPST,MYPST          PST                                       
         SP     PPGPST,MYHST          EXCLUDE HST                               
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         MVC    PPGMED,PBILKMED                                                 
         MVC    PPGKMED,PBILKMED                                                
         MVC    PPGCLI,PBILKCLT                                                 
*                                                                               
         CLI    GTSFTP,C'Y'       AGENCY UB/OO/OU?                              
         BE     *+12              YES                                           
         CLI    SAVFMT,C'C'       GROUPM - CATALYST                             
         BNE    *+16                                                            
*                                  NOT FOR OTHER AGENCIES                       
         MVC    PPGKPRO,PBILKPRD   SINCE PRDS MIGHT BE TOGETHER                 
         MVC    PPGPRO,PBILKPRD                                                 
*                                                                               
         MVC    PPGKEST,PBILKEST                                                
         MVC    PPGEST,PBILKEST                                                 
         MVC    PPGINVMO,PBILKBMN+1    BILLING MONTH                            
         MVC    PPGINVN,PBILKBNO                                                
         MVI    PPGKAOR,C' '                                                    
         MVI    PPGAOR,C' '                                                     
*                                                                               
         TM     PBILCMSW,X'20'                                                  
         BNO    *+12                                                            
         MVI    PPGKAOR,C'A'      AOR BILL                                      
         MVI    PPGAOR,C'A'      AOR BILL                                       
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
         DC     H'0'             TABLE FULL                                     
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         L      RF,BINVALS               ADDRESS OF FOUND RECORD                
         LA     RF,L'PPGKEY(RF)          PAST KEY                               
*                                                                               
         CLI    GTSFTP,C'Y'        AGENCY UB/OO/OU?                             
         BE     POSTB5             YES                                          
         CLC    QAGENCY,=C'O0'     SMGTO?                                       
         BE     POSTB4                                                          
         CLI    SAVFMT,C'N'        SEE IF NEW FORMAT                            
         BE     POSTB5                                                          
*                                                                               
POSTB4   CLC    LEDIS(L'PPGPU1,RF),PPGPU1   LEGAL ENTITIES                      
         BE     POSTB5                                                          
         OI     ERRORSW,X'08'   MISMATCH ENCOUNTERED                            
         LAY    RE,=C'**ERROR-LEGAL WARNING MISMATCH ON'                        
         MVC    P+1(33),0(RE)                                                   
         MVC    P+35(L'PPGINVF),PPGINVF                                         
         BRAS   RE,PRNT                                                         
*                                                                               
POSTB5   DS     0H                                                              
         AP     AMTDIS(6,RF),PPGBAMT                                            
         AP     GSTDIS(6,RF),PPGGST                                             
         AP     PSTDIS(6,RF),PPGPST                                             
*                                                                               
GOTOXIT  DS     0H                                                              
         MVC    BINVALS,=A(PPGKMED)                                             
         MVI    BINVALS,1                                                       
POSTBX   B      EXIT                                                            
*                                                                               
BINVALS  DS    0F                                                               
         DC    X'01'              ADD RECORD                                    
         DC    AL3(PPGKMED)      RECORD TO BE ADDED                             
         DC    A(PPGTABLE)        ADDRESS OF TABLE WHERE REC IS TO BE           
PPGRECNT DC    F'0'               NUMBER OF RECORDS ADDED                       
         DC    AL4(PPGTLEN)       LEN OF RECORD                                 
         DC    AL4(L'PPGKEY)      KEY SIZE                                      
         DC    F'4000'            MAX NUMBER OF RECORDS                         
*                                                                               
AOFPPGT  DC    A(PPGTABLE)                                                      
PPGKEY   DS    0XL14                                                            
PPGKMED  DS    CL1                                                              
PPGCLI   DS    CL3                                                              
PPGKPRO  DS    CL3      NOT SET IF NOT GROUPM-CATALYST                          
*                       TO ALLOW FOR PRODUCTS TOGETHER                          
PPGINUMB DS    0XL3                                                             
PPGINVMO DS    XL1           INVOICE MONTH                                      
PPGINVN  DS    XL2           INVOICE NUMBER                                     
PPGKEST  DS    XL2                                                              
PPGKEBRT DS    CL1           B= BARTER                                          
PPGKAOR  DS    CL1           A= AOR BILL                                        
*            ______                                                             
*              14                                                               
PPGREC   DS    0C                                                               
         ORG   *+PPGRECL                                                        
*                                                                               
ENDPPGR  DS    0C                                                               
*                                                                               
PPGTLEN  EQU   ENDPPGR-PPGKEY                                                   
*                                                                               
PGTTAPE  DCB   DDNAME=PGTTAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=100,                                              X        
               BLKSIZE=100,                                            X        
               MACRF=PM                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
PRNT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   PAGYNAT,C'C'           SEE IF CANADIAN                           
         BNE   *+8                                                              
         MVI   RCSUBPRG,50                                                      
*                                                                               
         CLI   FORCEHED,C'Y'                                                    
         BE    PRNT2                                                            
         ZIC   R1,LINE                                                          
         ZIC   R0,ALLOWLIN                                                      
         AR    R1,R0                                                            
         STC   R1,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    PRNT7                                                            
PRNT2    DS    0H                                                               
         CLI   MODE,LBUYREQ          REQUEST TOTALS                             
         BNL   PRNT7                                                            
         CLI   QCLIENT,C'$'          SEE IF DOING OFFICE LIST REQ               
         BNE   PRNT3                                                            
         MVC   HEAD5(8),=C'OFFICE X'                                            
*****    MVC   HEAD5+7(1),RCSVOFC                                               
         GOTO1 VOFFOUT,DMCB,RCSVOFC,HEXOUT,HEAD5+7                              
         B     PRNT4                                                            
PRNT3    CLI   QCLIENT,C'*'                                                     
         BNE   PRNT4                                                            
         MVC   HEAD5(8),=C'OFFICE X'                                            
*****    MVC   HEAD5+7(1),QCLIENT+1                                             
         GOTO1 VOFFOUT,DMCB,QCLIENT+1,HEXOUT,HEAD5+7                            
         B     PRNT4                                                            
*                                                                               
PRNT4    DS    0H                                                               
         CLI   QOPT6,C'A'         TEST AOR ONLY                                 
         BNE   PRNT5                                                            
         MVC   HEAD6(18),=C'**AOR BILLS ONLY**'                                 
*                                                                               
PRNT5    DS    0H                                                               
         CLI   QOPT6,C'B'         AOR AND AOR/CLIENT                            
         BNE   PRNT5B                                                           
         MVC   HEAD6(24),=C'**AOR AND CLIENT BILLS**'                           
*                                                                               
PRNT5B   DS    0H                                                               
         CLI   QOPT6,C'X'         NON=AOR BILLS ONLY                            
         BNE   PRNT6                                                            
         MVC   HEAD6(22),=C'**NON-AOR BILLS ONLY**'                             
*                                                                               
PRNT6    DS    0H                                                               
         CLI   QOPT6,C'C'         COMMISSION ONLY BILLS                         
         BNE   PRNT7                                                            
         MVC   HEAD6(25),=C'**COMMISSION ONLY BILLS**'                          
*                                                                               
PRNT7    DS    0H                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   MAXLINES,SAVMAX                                                  
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
GLCODE   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         LA     R1,=C'33120001'       GL NUM FOR MED M/T                        
*                                                                               
         CLC    QAGENCY,=C'OU'        AGENCY OU?                                
         BNE    *+12                                                            
         CLI    PBILKMED,C'N'         MEDIA N SAME AS M                         
         BE     GLEXIT                YES                                       
*                                                                               
         CLC    QAGENCY,=C'OO'        AGENCY OO?                                
         BNE    *+12                                                            
         CLI    PBILKMED,C'N'         MEDIA N SAME AS M                         
         BE     GLEXIT                YES                                       
*                                                                               
         CLI    PBILKMED,C'M'         MEDIA M?                                  
         BE     GLEXIT                YES                                       
         CLC    QAGENCY,=C'OU'        AGENCY OU?                                
         BE     *+12                  YES - MEDIA T NOT VALID FOR OU            
         CLI    PBILKMED,C'T'         MEDIA T?                                  
         BE     GLEXIT                YES                                       
         LA     R1,=C'33140001'       GL NUM FOR OO MED O                       
         CLI    PBILKMED,C'O'         MEDIA O?                                  
         BE     GLEXIT                YES                                       
         LA     R1,=C'33130001'       GL NUM FOR OO MED I/L                     
         CLI    PBILKMED,C'I'         MEDIA I?                                  
         BE     GLEXIT                YES                                       
         CLI    PBILKMED,C'L'         MEDIA L?                                  
         BE     GLEXIT                YES                                       
         LA     R1,=C'33120002'       GL NUM FOR OO MED N                       
         CLI    PBILKMED,C'N'         MEDIA N?                                  
         BE     GLEXIT                YES                                       
         LA     R1,=C'33130002'       GL NUM FOR OO MED S                       
         CLI    PBILKMED,C'S'         MEDIA S?                                  
         BE     GLEXIT                YES                                       
         LA     R1,=C'33130003'       GL NUM FOR OO MED V                       
         CLI    PBILKMED,C'V'         MEDIA V?                                  
         BE     GLEXIT                YES                                       
         LA     R1,=C'33130005'       GL NUM FOR OO MED B                       
         CLI    PBILKMED,C'B'         MEDIA B?                                  
         BE     GLEXIT                YES                                       
         LA     R1,=C'        '       BLANK GL ACCOUNT NUMBER                   
*                                                                               
GLEXIT   XIT1  REGS=(R1)              RETURN BUT LEAVE R1 INTACT                
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,RA,RC                                                         
*                                                                               
GETVEN   CSECT                                                                  
         NMOD1 0,GETVEN                                                         
         L     RA,0(R1)                A(WORKD)                                 
         USING PPWORKD,RA              PPWORKD DSECT                            
         L     RC,PPFILEC              PPFILEC                                  
         USING PPFILED,RC              START OF REMAINING WORK SPACE            
         LA    R8,SPACEND              SPACEND                                  
         USING PPGTWRKD,R8             PPGTWRKD DSECT                           
*                                                                               
         CLC   QAGENCY,=C'H7'    AGENCY H7?                                     
         BNE   GETV2             NO                                             
         MVC   SAVVEND,=C'15340587|'                                            
         MVC   SAVTERM,=C'T004'  PAYMENT TERMS                                  
         MVI   SAVFMT,C'C'       FORMAT                                         
         B     GETVXIT           DONE                                           
*                                                                               
GETV2    MVC   SAVVEND,=C'15096568|'                                            
         CLC   QAGENCY,=C'UB'    AGENCY UB?                                     
         BNE   GETV3             NO                                             
         CLC   =C'PAB',QCLIENT                                                  
         BNE   GETVXIT           YES                                            
         MVC   SAVVEND,=C'20104288|'                                            
         B     GETVXIT           YES                                            
*                                                                               
GETV3    MVC   SAVVEND,=C'15313660|'                                            
         CLC   QAGENCY,=C'OO'    AGENCY OO?                                     
         BE    GETVXIT           YES                                            
         MVC   SAVVEND,=C'15313917|'                                            
         CLC   QAGENCY,=C'OU'    AGENCY OU?                                     
         BE    GETVXIT           YES                                            
*                                                                               
         XC    SAVVEND,SAVVEND                                                  
         MVC   WORK(2),QAGENCY                                                  
         MVC   WORK+2(3),QCLIENT                                                
         LA    R5,VENTAB                                                        
GETV5    CLI   0(R5),X'FF'     END OF TABLE                                     
         BNE   *+6                                                              
         DC    H'0'            INVALID AGENCY                                   
         CLC   0(5,R5),WORK                                                     
         BE    GETV10                                                           
         LA    R5,32(R5)                                                        
         B     GETV5                                                            
*                                                                               
GETV10   MVC   SAVVEND,5(R5)   SAVE VENDOR CODE                                 
         MVC   SAVFILE,14(R5)  SAVE BDE FILE NAME                               
         MVC   SAVTERM,27(R5)  SAVE PAYMENT TERMS                               
         MVC   SAVFMT,31(R5) SAVE FORMAT (N=NEW,O=OLD)                          
GETVXIT  XIT1                                                                   
         LTORG                                                                  
*                                                                               
*        VENDOR CODE TABLE                                                      
*        AGY CODE - CLT CODE  - VENDOR CODE                                     
*                   NOTE THE NAME IS FOLLOWED BY A "|" (X'4F')                  
*                  -THE FIELD DELIMITER                                         
*                  -PAYMENT TERMS CODE                                          
*                  -FORMAT  O=OLD,N=NEW,C=CATALYST                              
*                                                                               
*                                                                               
VENTAB   DS    0H                                                               
* GROUPM-CATALYST                                                               
***      DC    C'H7',C'PG0',CL09'15249945|'  GROUPM-CATALYST                    
***      DC    CL13'DDS_PRT_H7'                                                 
***      DC    CL4'T003',C'C'                                                   
*                                                                               
* STARCOM                                                                       
         DC    C'H9',C'PG ',CL09'10075955|'  LEO BURNETT/STARCOM-CHIC           
         DC    CL13'STARCOM_PRINT'                                              
         DC    CL4'T007',C'O'                                                   
         DC    C'H9',C'PGN',CL09'10075955|'  LEO BURNETT/STARCOM-CHIC           
         DC    CL13'STARCOM_PRINT'                                              
         DC    CL4'T007',C'O'                                                   
         DC    C'H9',C'PGO',CL09'10075955|'  LEO BURNETT/STARCOM-CHIC           
         DC    CL13'STARCOM_PRINT'                                              
         DC    CL4'T007',C'O'                                                   
*                                                                               
         DC    C'H9',C'PG1',CL09'20517561|'  MEDIAVEST                          
         DC    CL13'DDS_PRT_NY'                                                 
         DC    CL4'T148',C'O'                                                   
**OLD**  DC    CL4'T234'    (BEFORE 10/04)                                      
*                                                                               
         DC    C'H9',C'PGG',CL09'20517561|'  MEDIAVEST - GILLETTE               
         DC    CL13'DDS_PRT_NY'                                                 
         DC    CL4'T148',C'O'                                                   
*                                                                               
         DC    C'H9',C'HPG',CL09'15123794|'  MEDIAVEST                          
         DC    CL13'DDS_PRT_TA'              TAPESTRY                           
         DC    CL4'T011',C'O'                                                   
*                                                                               
         DC    C'H9',C'PGB',CL09'20176036|'  BROMLEY COMM.                      
         DC    CL13'DDS_PRT_BR'                                                 
         DC    CL4'T054',C'O'                                                   
*                                                                               
*                                                                               
* OUTRIDER - MEDIACOM                                                           
         DC    C'HY',C'PGC',CL09'15282520|'  OUTRIDER                           
         DC    CL13'DDS_PRT_HY'                                                 
         DC    CL4'T053',C'O'                                                   
*                                                                               
         DC    C'HY',C'PGO',CL09'10056957|'  OUTRIDER                           
         DC    CL13'DDS_PRT_HY'                                                 
         DC    CL4'T053',C'O'                                                   
*                                                                               
         DC    C'HY',C'PGI',CL09'10056957|'  OUTRIDER                           
         DC    CL13'DDS_PRT_HY'                                                 
         DC    CL4'T053',C'O'                                                   
*                                                                               
         DC    C'HY',C'PRG',CL09'15282520|'  OUTRIDER                           
         DC    CL13'DDS_PRT_HY'                                                 
         DC    CL4'T053',C'O'                                                   
*                                                                               
* MEDIACOM                                                                      
         DC    C'HY',C'PG1',CL09'10056957|'  MEDIACOM                           
         DC    CL13'DDS_PRT_HY'                                                 
         DC    CL4'T053',C'N'                                                   
*                                                                               
         DC    C'HY',C'P12',CL09'10056957|'  MEDIACOM                           
         DC    CL13'DDS_PRT_HY'                                                 
         DC    CL4'T053',C'N'                                                   
*                                                                               
         DC    C'HY',C'P13',CL09'10056957|'  MEDIACOM                           
         DC    CL13'DDS_PRT_HY'                                                 
         DC    CL4'T053',C'N'                                                   
* MEDIAVEST-TORONTO                                                             
**OLD**  DC    C'O0',C'PGM',CL09'15041589|'  MEDIAVEST-TORONTO                  
         DC    C'O0',C'PGM',CL09'15086124|'  MEDIAVEST-TORONTO                  
         DC    CL13'STARCOMTOPGM'                                               
         DC    CL4'T054',C'O' TEMPORARY - CD INVOICES                           
*                                                                               
         DC    C'O0',C'PPM',CL09'15086124|'  MEDIAVEST-TORONTO                  
         DC    CL13'STARCOMTOPPM'                                               
         DC    CL4'T054',C'O' TEMPORARY - CD INVOICES                           
*                                                                               
         DC    C'O0',C'PSM',CL09'15086124|'  MEDIAVEST-TORONTO                  
         DC    CL13'STARCOMTOPSM'                                               
         DC    CL4'T054',C'N' TEMPORARY - CD INVOICES                           
*                                                                               
**T1TEST                                                                        
         DC    C'SJ',C'P&&G',CL09'15086124|'  MEDIAVEST-TORONTO                 
         DC    CL13'STARCOMTOPSM'                                               
         DC    CL4'T054',C'N' TEMPORARY - CD INVOICES                           
*                                                                               
         DC    C'T1',C'PG1',CL09'20517561|'  TCH1                               
         DC    CL13'DDS_PRT_XX'                                                 
         DC    CL4'T148',C'N'                                                   
         DC    C'HD',C'PG1',CL09'20517561|'  HDTO                               
         DC    CL13'DDS_PRT_XX'                                                 
         DC    CL4'T148',C'N'                                                   
**T1TEST                                                                        
         DC    C'TC',C'PGT',CL09'20517561|'  TRAINING CENTER                    
         DC    CL13'DDS_PRT_XX'                                                 
         DC    CL4'T148',C'O'                                                   
         DC    C'DR',C'PG ',CL09'20517561|'    SMG TESTING                      
         DC    CL13'DDS_PRT_DR'                                                 
         DC    CL4'T148',C'O'                                                   
         DC    C'FG',C'P&&G',CL09'20517561|'    LETO - CANADIAN TEST            
         DC    CL13'DDS_PRT_LE'                                                 
         DC    CL4'T148',C'O'                                                   
         DC    X'FFFF'                  END OF TABLE                            
*                                                                               
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R8,SPACEND                                                       
         USING PPGTWRKD,R8                                                      
         L     R4,4(R1)        ADDRESS OF PPGREC                                
         USING PPGRECD,R4                                                       
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVC   DLCBFLD,SPACES    CLEAR FIELD                                    
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
*                                                                               
         CLI   MODE,RUNLAST       SEE IF END OF REPORT                          
         BE    DNP70                                                            
*                                                                               
         CLI   MODE,FBUYREQ       SEE IF I NEED TO INTIALIZE                    
         BE    DNP80                                                            
*****                                                                           
*****    DOWNLOAD BILLING INFO HERE                                             
*****                                                                           
         MVI   DOWNACT,C'Y'      SET DOWN ACTIVITY                              
*                                                                               
         MVI   DLCBFLD,C'H'      HEADER                                         
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         ST    R1,SAVER1      SAVE R1                                           
         GOTO1 DATCON,DMCB,(3,PPGIDATE),(X'20',WORK)                            
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
*                                                                               
**T1TEST                                                                        
         CLC   QAGENCY,=C'O0'         SMGTO?                                    
         BE    DOWNLD2                                                          
*                                                                               
         MVC   DLCBFLD(3),PPGPU2     WAS HARDCODED                              
         CLC   PPGPU2,SPACES                                                    
         BH    *+10                                                             
         MVC   DLCBFLD(3),=C'283'    IF MISSING SET TO DEFAULT                  
*                                                                               
         CLI   SAVFMT,C'N'           SEE IF NEW FORMAT                          
         BE    DOWNLDT                                                          
*                                                                               
DOWNLD2  MVC   DLCBFLD(3),PPGPU1     PRD USER FIELD 1                           
         CLI   SAVFMT,C'C'           HY'S FORMAT                                
         BNE   DOWNLDT                                                          
         MVC   DLCBFLD(4),PPGPU1     4 POSSIBLE                                 
         OC    DLCBFLD(4),SPACES                                                
*                                                                               
DOWNLDT  MVI   DLCBTYP,C'T'          TEXT FIELD                                 
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
                                                                                
         MVC   DLCBFLD(3),=C'CAD'    CURRENCY                                   
         CLI   PAGYNAT,C'C'          CANADIAN AGENCY?                           
         BE    *+10                                                             
         MVC   DLCBFLD(3),=C'USD'    CURRENCY - ELSE USA                        
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
         MVC   DLCBFLD(9),SAVVEND                                               
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
         MVC   DLCBFLD(3),PPGCLI     START WITH CLIENT CODE                     
         CLI   SAVFMT,C'C'           GROUPM-CATALYST?                           
         BNE   DNP1A                 NO                                         
         MVI   DLCBFLD+3,C'-'        DASH                                       
         LA    R3,PPGEST             R3 = ESTIMATE                              
         BAS   RE,CVD                ESTIMATE IS NOW IN WORK(5)                 
         MVC   DLCBFLD+4(3),WORK+2   3 CHARACTERS OF ESTIMATE                   
         MVI   DLCBFLD+7,C'-'        DASH                                       
         MVC   SVEST,WORK+2          SAVE ESTIMATE FOR LINE ITEM TEXT           
         MVC   WORK(2),PPGMOS        MOS                                        
         MVI   WORK+2,X'01'          DUMMY DAY                                  
         ST    R1,SAVER1             SAVE R1                                    
         GOTO1 DATCON,DMCB,(3,WORK),(10,WORK+6)                                 
         L     R1,SAVER1             RESTORE R1                                 
         MVC   DLCBFLD+8(2),WORK+6   MM                                         
         MVC   DLCBFLD+10(2),WORK+12 YY                                         
         B     DNP1AX                                                           
*                                                                               
DNP1A    MVC   DLCBFLD+3(2),WORK+8   MM                                         
         MVC   DLCBFLD+5(2),WORK+6   YY                                         
         MVC   SVMOS,DLCBFLD+3     SAVE FOR LINE ITEM TEXT                      
         LA    R3,PPGEST                                                        
         BAS   RE,CVD                                                           
         MVC   DLCBFLD+7(3),WORK+2          EST                                 
*                                                                               
         MVC   SVEST,WORK+2       SAVE FOR LINE ITEM TEXT                       
*                                                                               
DNP1AX   MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  INVOICE AMOUNT                               
         ZAP   SVBILL,PPGBAMT     AMOUNT DUE (INCLUDES GST/PST/HST)             
         NI    PPGBAMT+5,X'F0'                                                  
         OI    PPGBAMT+5,X'0C'    MAKE POSITIVE                                 
         EDIT  (P6,PPGBAMT),(11,DLCBFLD),2,ALIGN=LEFT                           
         ZAP   PPGBAMT,SVBILL   RESTORE FOR LINE ITEMS                          
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*****                             PAYMENT TERMS                                 
*****    TEMPORARY CODE FOR CANADIAN CD INVOICES                                
*****                                                                           
*****    MVC   DLCBFLD(4),=C'T054'   FOR CD INVOICES                            
*****    CLC   QAGENCY,=C'O0'        MEDIAVEST CANADA                           
*****    BE    DNP2                                                             
*****                                                                           
*****    MVC   DLCBFLD(4),=C'T148'                                              
*****    CLC   QAGENCY,=C'H9'        UNLESS STARCOM                             
*****    BNE   *+10                                                             
*****    MVC   DLCBFLD(4),=C'T006'   T006 FOR STARCOM                           
*****                                                                           
         MVC   DLCBFLD(4),SAVTERM   FROM VENTAB                                 
*                                                                               
         CLC   QAGENCY,=C'H9'                                                   
         BNE   DNP1X                                                            
         CLI   QMEDIA,C'I'                                                      
         BNE   DNP1X                ONLY FOR MEDIA I                            
         CLC   =C'HPG',QCLIENT      CLIENT HPG GETS T008                        
         BNE   DNP1B                                                            
****     MVC   DLCBFLD(4),=C'T008'                                              
         MVC   DLCBFLD(4),=C'T010'   PLUS 45 DAYS  TICKET# 0148589N             
         B     DNP2                                                             
*                                                                               
DNP1B    CLC   =C'PG1',QCLIENT       CLIENTS PG1 AND PGG GET T102               
         BE    DNP1C                                                            
         CLC   =C'PGG',QCLIENT                                                  
         BNE   DNP1X                                                            
DNP1C    MVC   DLCBFLD(4),=C'T102'   TICKET# 0147735N                           
         B     DNP2                                                             
*                                                                               
*        SPECIAL CODE TO HANDLE CD VS. NON-CD INVOICES                          
*                                                                               
DNP1X    CLC   QAGENCY,=C'O0'       SEE IF I NEED TO ALTER                      
         BNE   DNP2                 FOR MEDIAVEST - CANADIAN                    
         CLC   PPGIDATE,=X'67070B'   SEE IF BEFORE JUL11/03                     
         BL    DNP2                  DON'T ALTER - THEY WERE CD                 
*                                    ITEMS ONLY BILLS                           
         CLI   PPGCDTYP,C'C'         WAS SET FROM BILL RECORD                   
         BE    DNP2                                                             
*                                     IF NOT C - ASSUME NON-CD                  
*                   MEANS FIELD WAS EITHER N OR NOT SET                         
         MVC   DLCBFLD(4),=C'T007'     NON-CD INVOICE PAYMENT TERMS             
*                                                                               
DNP2     MVI   DLCBTYP,C'T'       TEXT FIELD                                    
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
         MVI   DLCXDELC,C' '   ALTER FOR THIS (THE LAST) FIELD ONLY             
         GOTO1 VDLFLD                                                           
         MVI   DLCXDELC,X'4F'  RESTORE TO VERTICAL PIKE                         
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
*                                                                               
**BRT    CLI   PPGEBRT,C'B'       SEE IF A BARTER ESTIMATE                      
**BRT    BNE   *+10                                                             
**BRT    MVC   DLCBFLD(3),=C'BRT'                                               
*                                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                  LINE COUNTER                                 
*                           NOTE - FIRST LINE ITEM                              
         MVC   DLCBFLD(1),SEQNUM                                                
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*        ACCOUNT NUMBER                                                         
         CLC   QAGENCY,=C'H7'    SEE IF GROUPM-CATALYST                         
         BNE   DNP20B            HARD-CODED ACCOUNT#                            
         MVC   DLCBFLD(L'PPGEU1),PPGEU1                                         
         B     DNP22                                                            
*                                                                               
*                                 ACCOUNT NUMBER                                
*                         NOTE - AAA ESTIMATE USER FIELD 1                      
*                         NOTE - LENGTH OF 8 FROM EXAMPLE                       
*                                                                               
DNP20B   MVC   DLCBFLD(L'PPGEU1),PPGEU1                                         
         CLI   SAVFMT,C'N'         SEE IF DOING NEW FORMAT                      
         BNE   DNP20C                                                           
         CLI   PPGEBRT,C'B'       SEE IF BARTER                                 
         BNE   DNP20C                                                           
         CLI   PPGAOR,C'A'        AND AND AOR BILL                              
         BNE   DNP20C                                                           
         MVC   DLCBFLD(8),=C'20640019'                                          
*                                                                               
DNP20C   CLI   TAXSW,1            SEE IF DOING GST                              
         BNE   DNP21                                                            
         MVC   DLCBFLD(L'PPGEU1),SPACES                                         
         MVC   DLCBFLD(8),=C'12041011'    GST/HST ACCOUNT CODE                  
*                                                                               
DNP21    CLI   TAXSW,2            SEE IF DOING PST?                             
         BNE   DNP22                                                            
         MVC   DLCBFLD(L'PPGEU1),SPACES                                         
         MVC   DLCBFLD(8),=C'12041007'                                          
DNP22    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 LINE ITEM TEXT                                
         CLI   TAXSW,1            SEE IF DOING GST/HST                          
         BNE   DNP24                                                            
*******  MVC   DLCBFLD(7),=C'GST/HST'                                           
*******  MVI   DLCBFLD+7,X'4F'     SINCE NEXT FIELD IS EMPTY                    
*                                                                               
*        THEY NOW NEED THE TAX REGISTRATION NUMBER                              
*                                                                               
         MVC   DLCBFLD(13),=C'GST#886669233'                                    
         MVI   DLCBFLD+13,X'4F'    SINCE NEXT FIELD IS EMPTY                    
         CLI   HYOSW,C'Y'         SEE IF OUTRIDER                               
         BNE   DNP22C                                                           
         MVC   DLCBFLD(13),=C'HST#831734355'                                    
         MVI   DLCBFLD+13,X'4F'    SINCE NEXT FIELD IS EMPTY                    
         B     DNP23                                                            
*                                                                               
DNP22C   CLI   HYMSW,C'Y'         SEE IF MEDIACOM                               
         BNE   DNP23                                                            
         MVC   DLCBFLD(13),=C'HST#866334549'                                    
         MVI   DLCBFLD+13,X'4F'    SINCE NEXT FIELD IS EMPTY                    
*                                                                               
DNP23    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP33                                                            
*                                                                               
DNP24    CLI   TAXSW,2          SEE IF DOING PST                                
         BNE   DNP28                                                            
         MVC   DLCBFLD(3),=C'PST'                                               
*                                                                               
*NOTE*   IN THE FUTURE MAY NEED MORE TAX ID NUMBERS HERE                        
*                                                                               
         MVI   DLCBFLD+3,X'4F'     SINCE NEXT FIELD IS EMPTY                    
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP33                                                            
*                                                                               
*                                 LINE ITEM TEXT                                
DNP28    DS    0H                                                               
         CLC   QAGENCY,=C'H7'     GROUPM-CATALYST                               
         BNE   DNP28B             NO                                            
         LA    R2,DLCBFLD+4       LAST CHARACTER OF MEDIA (LEN 5)               
         MVC   DLCBFLD(5),=C'TRADE'                                             
         CLI   PPGMED,C'T'        MEDIA T?                                      
         BE    DNP28D             YES                                           
         AHI   R2,1               LAST CHARACTER OF MEDIA (LEN 6)               
         MVC   DLCBFLD(6),=C'SEARCH'                                            
         CLI   PPGMED,C'S'        MEDIA S?                                      
         BE    DNP28D             YES                                           
         MVC   DLCBFLD(6),=C'SOCIAL'                                            
         CLI   PPGMED,C'L'        MEDIA L?                                      
         BE    DNP28D             YES                                           
         MVC   DLCBFLD(6),=C'MOBILE'                                            
         CLI   PPGMED,C'B'        MEDIA B?                                      
         BE    DNP28D             YES                                           
         AHI   R2,1               LAST CHARACTER OF MEDIA (LEN 7)               
         MVC   DLCBFLD(7),=C'OUTDOOR'                                           
         CLI   PPGMED,C'O'        MEDIA O?                                      
         BE    DNP28D             YES                                           
         AHI   R2,1               LAST CHARACTER OF MEDIA (LEN 8)               
         MVC   DLCBFLD(8),=C'MAGAZINE'                                          
         CLI   PPGMED,C'M'        MEDIA M?                                      
         BE    DNP28D             YES                                           
         AHI   R2,1               LAST CHARACTER OF MEDIA (LEN 9)               
         MVC   DLCBFLD(9),=C'NEWSPAPER'                                         
         CLI   PPGMED,C'N'        MEDIA N?                                      
         BE    DNP28D             YES                                           
         AHI   R2,2               LAST CHARACTER OF MEDIA (LEN 11)              
         MVC   DLCBFLD(11),=C'LOCAL VIDEO'                                      
         CLI   PPGMED,C'W'        MEDIA W?                                      
         BE    DNP28D             YES                                           
         MVC   DLCBFLD(11),=C'INTERACTIVE'                                      
         CLI   PPGMED,C'I'        MEDIA I?                                      
         BE    DNP28D             YES                                           
         AHI   R2,2               LAST CHARACTER OF MEDIA (LEN 13)              
         MVC   DLCBFLD(13),=C'DIGITAL VIDEO'                                    
         CLI   PPGMED,C'D'        MEDIA D?                                      
         BE    DNP28D             YES                                           
         AHI   R2,1               LAST CHARACTER OF MEDIA (LEN 14)              
         MVC   DLCBFLD(14),=C'NATIONAL VIDEO'                                   
         CLI   PPGMED,C'V'        MEDIA V?                                      
         BE    DNP28D             YES                                           
         DC    H'0'               UNKNOWN MEDIA                                 
*                                                                               
DNP28B   DS    0H                                                               
***YKVA  CLC   QAGENCY,=C'HY'                                                   
         CLI   HYMCSW,C'Y'       ONLY FOR MCMMP CLIENTS                         
         BNE   DNP28B5                                                          
         LA    R5,PPGEU2         USUALLY EST USER 2                             
         CLC   QAGENCY,=C'O0'                                                   
         BNE   *+8                                                              
         LA    R5,PPGPU2         USE PRD USER 2 FOR THIS AGY                    
*                                AND SAVFMT NOT EQUAL N                         
*                                                                               
DNP28B1  CLC   0(L'PPGEU2,R5),SPACES    DO I HAVE ANY DATA                      
         BNH   DNP28B5           IF NOT USE OLD FORMAT                          
         MVC   DLCBFLD(L'PPGEU2),0(R5)                                          
         LA    RE,DLCBFLD+L'PPGEU2                                              
DNP28B2  CLI   0(RE),C' '        SCAN BACKWARD FOR FIRST NON-SPACE              
         BNE   DNP28B3                                                          
         BCT   RE,DNP28B2                                                       
*                                                                               
DNP28B3  MVC   2(L'PPGENAME,RE),PPGENAME                                        
         OC    DLCBFLD(37),SPACES       JUST IN CASE    16+1+20                 
         MVC   DLCBFLD+30(7),SPACES     CAN'T EXCEED 30 CHARS                   
         B     DNP28BX                                                          
*                                                                               
DNP28B5  MVC   DLCBFLD(3),SVEST                                                 
         MVC   DLCBFLD+3(4),SVMOS                                               
         MVC   DLCBFLD+8(L'PPGENAME),PPGENAME                                   
         OC    DLCBFLD(37),SPACES                                               
*                                                                               
DNP28BX  CLC   PPGALLO(32),SPACES       ALLOCATION PRESENT?                     
         BNE   DNP30                                                            
*                                                                               
         LA    R2,DLCBFLD+36                                                    
DNP28C   CLI   0(R2),C' '     SACN BACKWARD FOR NON-SPACE                       
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
         CLI   TAXSW,1                                                          
         BE    DNP33A                                                           
         CLI   TAXSW,2                                                          
         BE    DNP33A                                                           
         B     DNP33AP                                                          
*                                                                               
DNP33A   MVC   DLCBFLD(5),=X'4F4F4F4F4F'   GST/HST/PST LINE                     
*                                          SEND 5 EMPTY FIELDS                  
*      TAX CODE/WITHHOLDING TAX CODE/TAX JUSISDICTION                           
*      INTERNAL ORDER NUMBER/PURCHASE ORDER NUMBER/P.O. LINE ITEM               
*                                                                               
         B     DNP55L      SEND AS LAST FIELD                                   
*                                                                               
DNP33AP  DS    0H                                                               
*                                                                               
         CLC   QAGENCY,=C'H9'   MEDIAVEST?                                      
         BNE   DNP33AX          ALL CLIENTS                                     
         CLC   PPGEU1(8),=C'14080026'    SPECIAL ACCOUNT CODE                   
         BE    DNP33A     SEND EMPTY FIELDS                                     
*                                                                               
DNP33AX  DS    0H                                                               
*                                 TAX CODE                                      
*                                                                               
*        CODE BELOW WAS TEMPORAY  - P&G SCREW-UP                                
*                                                                               
*******  MVC   DLCBFLD(2),=C'1A'                                                
*******  CLI   TAXSW,1            SPECIAL FOR GST/HST                           
*******  BE    DNP33C                                                           
*******  CLI   TAXSW,2            OR PST                                        
*******  BE    DNP33C                                                           
         MVC   DLCBFLD(2),=C'1Z'                                                
*                                                                               
DNP33C   MVI   DLCBFLD+2,X'4F'    EXTRA FIELD DELIMITER                         
*                               SO I WON'T HAVE TO SEND EMPTY FIELD             
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                 WITHHOLDING TAX CODE                          
*                                 EMPTY FOR NORTH AMERICA                       
***NO    MVI   DLCBTYP,C'T'       TEXT FIELD                                    
***NO    MVI   DLCBACT,DLCBPUT                                                  
***NO    GOTO1 VDLFLD                                                           
*                                 TAX JURISDICTION                              
         CLC   QAGENCY,=C'UB'      CARAT                                        
         BNE   DNP35                                                            
         CLC   QCLIENT,=C'PAB'                                                  
         BNE   DNP35                                                            
         MVC   DLCBFLD(4),=C'PRGB'                                              
         LA    R2,DLCBFLD+4                                                     
         B     DNP55                                                            
*                                                                               
DNP35    MVC   DLCBFLD(4),=C'CA00'                                              
         LA    R2,DLCBFLD+4                                                     
*                                                                               
***      CLC   QAGENCY,=C'H7'       GROUPM - CATALYST                           
***      BNE   DNP40                                                            
***      CLC   QCLIENT,=C'PG0'      SEND 9999999999 FOR THIS CLIENT             
***      BE    DNP44                                                            
***      DC    H'0'                 SHOULD NEVER HAPPEN-UNKNOWN CLIENT          
*                                                                               
DNP40    CLC   QCLIENT,=C'PPM'      SEND 9999999999 FOR THIS CLT                
         BE    DNP44                                                            
*                                                                               
         CLI   PAGYNAT,C'C'         CANADIAN?                                   
         BE    DNP55                                                            
DNP44    MVC   DLCBFLD(10),=C'9999999999'                                       
         LA    R2,DLCBFLD+10                                                    
DNP55    DS    0H                                                               
         MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
**T1TEST                                                                        
*                                 INTERNAL ORDER NUMBER                         
*                     PRODUCT USER 1                                            
*                                                                               
         CLC   QAGENCY,=C'O0'    SMGTO?                                         
         BE    DNP55B                                                           
*                                                                               
         CLI   SAVFMT,C'N'       SEE IF NEW FORMAT                              
         BE    DNP55A0                                                          
         BNE   DNP55B                                                           
*                                                                               
**T1TEST CLC   QAGENCY,=C'HD'     SEE IF HDTO TESTING                           
**T1TEST BE    DNP55A0                                                          
**T1TEST CLC   QAGENCY,=C'T1'     SEE IF TCH1 TESTING                           
**T1TEST BNE   DNP55B                                                           
*                                                                               
DNP55A0  DS    0H                                                               
         CLI   PPGEBRT,C'B'       SEE IF A BARTER AOR BILL                      
         BNE   DNP55A5                                                          
         CLI   PPGAOR,C'A'                                                      
         BNE   DNP55A5                                                          
*                               ALSO MUST BE A CREDIT?                          
         ZAP   MYPACK,PPGBAMT   START WITH BILL AMOUNT                          
         SP    MYPACK,PPGGST    SUBTRACT GST/HST/PST                            
         SP    MYPACK,PPGPST                                                    
         TM    MYPACK+5,X'0D'                                                   
         BNO   DNP55A5                                                          
*                                                                               
*        SKIP  INTERNAL ORDER NUMBER FOR THESE                                  
*                                                                               
         LA    R2,DLCBFLD-1    SINCE DNP55C USES 1(R2)                          
         B     DNP55C                                                           
*                                                                               
DNP55A5  MVC   DLCBFLD(L'PPGPU1),PPGPU1   LOCATION OF DATA                      
         LA    R3,L'PPGPU1    IN CASE FIELD MISSING                             
         LA    R2,DLCBFLD+L'PPGPU1-1                                            
DNP55AT  CLI   0(R2),C' '    SCAN BACKWORK FOR NON-SPACE                        
         BH    DNP55C        REST SAME AS OLD                                   
         SH    R2,=H'1'                                                         
         BCT   R3,DNP55AT                                                       
*        SHOULD NEVER GET HERE                                                  
         LA    R2,DLCBFLD                                                       
         B     DNP55C                                                           
*                                                                               
DNP55B   DS    0H                                                               
**T1TEST                                                                        
*                                 INTERNAL ORDER NUMBER                         
*                     BRAND ESTIMATE USER FIELD 1                               
*                                                                               
         MVC   DLCBFLD(L'PPGEU2),PPGEU2   LOCATION OF DATA                      
         LA    R3,L'PPGEU2    IN CASE FIELD MISSING                             
         LA    R2,DLCBFLD+L'PPGEU2-1                                            
DNP55A   CLI   0(R2),C' '    SCAN BACKWORK FOR NON-SPACE                        
         BH    DNP55C                                                           
         SH    R2,=H'1'                                                         
         BCT   R3,DNP55A                                                        
DNP55C   MVI   1(R2),X'4F'    EXTRA FIELD DELIMITER (PO NUMBER)                 
         MVI   2(R2),X'4F'    EXTRA FIELD DELIMITER (PO LINE ITEM)              
*                               SO I WON'T HAVE TO SEND EMPTY FIELDS            
DNP55L   MVI   DLCBTYP,C'T'       TEXT FIELD                                    
         MVI   DLCBACT,DLCBPUT                                                  
*                                                                               
         MVI   DLCXDELC,C' '    ALTER FOR THE LAST FIELD                        
         GOTO1 VDLFLD                                                           
         MVI   DLCXDELC,X'4F'   RESTORE TO VERTICAL PIKE                        
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
         ST    R1,SAVER1           SAVE R1                                      
         GOTO1 VGETVEN,DMCB,(RA)   GET VENDOR CODE & BDE FILE NAME              
         L     R1,SAVER1           RESTORE R1                                   
*                                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         GOTO1 VDLFLD                                                           
DNPX     DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
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
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
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
         USING PPWORKD,RA              PPWORKD DSECT                            
         L     RC,PPFILEC              PPFILEC                                  
         USING PPFILED,RC              START OF REMAINING WORK SPACE            
         LA    R8,SPACEND              SPACEND                                  
         USING PPGTWRKD,R8             PPGTWRKD DSECT                           
         L     R4,4(R1)                A(PPGREC)                                
         USING PPGRECD,R4              PPGRECD DSECT                            
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
         GOTO1 DATCON,DMCB,(3,PPGIDATE),(X'20',WORK)                            
         MVC   0(2,R2),WORK+4          DAY                                      
         MVC   2(2,R2),WORK+2          MONTH                                    
         MVC   4(2,R2),=C'20'          CENTURY                                  
         MVC   6(2,R2),WORK            YEAR                                     
         MVI   8(R2),C'|'              DELIMITER                                
         LA    R2,9(R2)                BUMP R2                                  
*                                                                               
         LA    R1,PPGPU1               PRODUCT UDEF 1                           
         CLC   QAGENCY,=C'UB'          AGENCY UB?                               
         BE    *+8                     YES - PRD UDEF 1 IS LEGAL ENTITY         
         LA    R1,PPGALLO              PRD UCOM1 LEGAL ENTITY FOR OO/OU         
*                                                                               
         MVC   0(4,R2),0(R1)           LEGAL ENTITY                             
         LA    R2,4(R2)                MAXED OUT DATA + 1                       
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
         MVC   0(3,R2),=C'CAD'         CURRENCY                                 
         CLI   PAGYNAT,C'C'            CANADIAN AGENCY?                         
         BE    *+10                    YES                                      
         MVC   0(3,R2),=C'USD'         NO - CURRENCY IN USD                     
         MVI   3(R2),C'|'              DELIMITER                                
         LA    R2,4(R2)                BUMP R2                                  
*                                                                               
         MVI   0(R2),C'|'              DELIMITER (TRANSLATION DAT NULL)         
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
         CLC   QAGENCY,=C'UB'          AGENCY UB?                               
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
         CLC   QAGENCY,=C'OO'          AGENCY OO?                               
         BE    *+14                                                             
         CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BNE   *+14                                                             
         CP    PPGBAMT,=P'0'           ZERO DOLLAR INVOICE                      
         BE    FTPX                                                             
*                                                                               
         ZAP   SVBILL,PPGBAMT          INV AMT (INCLUDES GST/PST/HST            
         NI    PPGBAMT+5,X'F0'         STRIP OFF SIGN BITS                      
         OI    PPGBAMT+5,X'0C'         MAKE POSITIVE                            
         EDIT  (P6,PPGBAMT),(11,0(R2)),2,ALIGN=LEFT                             
         ZAP   PPGBAMT,SVBILL          RESTORE FOR LINE ITEMS                   
         LA    R2,12(R2)               MAXED OUT DATA + 1                       
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
         MVC   0(4,R2),=C'T549'        PAYMENT TERMS                            
*                                                                               
         CLC   QAGENCY,=C'OU'          AGENCY OO/OU?                            
         BE    *+14                                                             
         CLC   QAGENCY,=C'OO'          AGENCY UB?                               
         BNE   *+10                                                             
         MVC   0(4,R2),=C'T056'        PAYMENT TERMS FOR OO/OU                  
*                                                                               
         CLC   QAGENCY,=C'UB'          AGENCY UB?                               
         BNE   *+10                                                             
         MVC   0(4,R2),=C'T012'        PAYMENT TERMS FOR UB                     
*                                                                               
         MVI   4(R2),C'|'              DELIMITER                                
         LA    R2,5(R2)                BUMP R2                                  
*                                                                               
         LA    R1,C'1'                 LINE ITEM COUNT FOR US                   
         CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BNE   *+8                     NO - LINE ITEM COUNT IS 1                
         AHI   R1,1                    YES - BUMP LINE ITEM COUNT               
         STC   R1,0(R2)                LINE ITEM COUNT                          
*                                                                               
         L     R1,=A(PGTTAPE)          ADD RECORD TO PGTTAPE                    
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
         LA    R1,PPGALLO              GL ACCOUNT NUMBER IN EST UCOMM 1         
         CLC   QAGENCY,=C'UB'          AGENCY UB?                               
         BE    FTP35                   YES - DONE WITH GL ACCOUNT NUM           
*                                                                               
         LA    R1,=C'12041011'         FOR TAX LINE                             
         CLI   SEQNUM,C'1'             TAX LINE?                                
         BH    FTP35                   YES                                      
*                                                                               
         LA    R1,PPGEUCOM             ESTIMATE UCOMM 1                         
         CLC   PPGEUCOM,SPACES         HAVE ESTIMATE UCOMM 1?                   
         BH    FTP35                   YES - THIS IS THE GL CODE                
*                                                                               
         LA    R1,=C'33120001'         GL NUM FOR MED M/T                       
*                                                                               
         CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BNE   *+12                                                             
         CLI   PPGMED,C'N'             MEDIA N SAME AS M                        
         BE    FTP35                   YES                                      
*                                                                               
         CLC   QAGENCY,=C'OO'          AGENCY OO?                               
         BNE   *+12                                                             
         CLI   PPGMED,C'N'             MEDIA N SAME AS M                        
         BE    FTP35                   YES                                      
*                                                                               
         CLI   PPGMED,C'M'             MEDIA M?                                 
         BE    FTP35                   YES                                      
         CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BE    *+12                    YES - MEDIA T NOT VALID FOR OU           
         CLI   PPGMED,C'T'             MEDIA T?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'33140001'         GL NUM FOR OO MED O                      
         CLI   PPGMED,C'O'             MEDIA O?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'33130001'         GL NUM FOR OO MED I/L                    
         CLI   PPGMED,C'I'             MEDIA I?                                 
         BE    FTP35                   YES                                      
         CLI   PPGMED,C'L'             MEDIA L?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'33120002'         GL NUM FOR OO MED N                      
         CLI   PPGMED,C'N'             MEDIA N?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'33130002'         GL NUM FOR OO MED S                      
         CLI   PPGMED,C'S'             MEDIA S?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'33130003'         GL NUM FOR OO MED V                      
         CLI   PPGMED,C'V'             MEDIA V?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'33130005'         GL NUM FOR OO MED B                      
         CLI   PPGMED,C'B'             MEDIA B?                                 
         BE    FTP35                   YES                                      
         LA    R1,=C'        '         BLANK GL ACCOUNT NUMBER                  
         CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BNE   *+8                     NO                                       
         OI    ERRORSW,X'40'           UNKNOWN MEDIA TYPE WARNING OU            
*                                                                               
FTP35    MVC   0(8,R2),0(R1)           GL ACCOUNT NUMBER                        
         MVI   8(R2),C'|'              DELIMITER                                
         LA    R2,9(R2)                BUMP R2                                  
*                                                                               
         CLC   QAGENCY,=C'UB'          AGENCY UB?                               
         BNE   FTP40                   NO                                       
*                                                                               
         MVC   WORK(2),PPGMOS          MOS (MMYY)                               
         MVI   WORK+2,X'01'            DUMMY DAY                                
         GOTO1 DATCON,DMCB,(3,WORK),(X'20',WORK+20)                             
*                                                                               
         MVC   0(2,R2),=C'20'          CENTURY                                  
         MVC   2(2,R2),WORK+20         YEAR                                     
         MVI   4(R2),C'-'              DASH                                     
*                                                                               
         EDIT  (B2,PPGEST),(3,5(R2)),FILL=0                                     
         MVI   8(R2),C'-'              DASH                                     
*                                                                               
         MVC   9(2,R2),WORK+22         MM                                       
         MVC   11(2,R2),WORK+20        YY                                       
         MVI   13(R2),C'-'             DASH                                     
*                                                                               
         MVC   14(16,R2),PPGENAME      ESTIMATE DESCRIPTION                     
*                                                                               
         LA    R2,30(R2)               MAXED OUT DATA + 1                       
         B     FTP44                   DONE                                     
*                                                                               
FTP40    CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BNE   FTP42                   NO                                       
         CLI   SEQNUM,C'1'             TAX LINE?                                
         BE    FTP41                   NO                                       
         MVC   0(15,R2),=C'786587329RT0001'                                     
         MVI   15(R2),C'|'             DELIMITER                                
         LA    R2,16(R2)               BUMP R2                                  
         B     FTP45                   DONE                                     
*                                                                               
FTP41    CLI   PPGMED,C'M'             MEDIA M?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'N'             MEDIA N?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'O'             MEDIA O?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'T'             MEDIA T?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'S'             MEDIA S?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'I'             MEDIA I?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'B'             MEDIA B?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'L'             MEDIA L?                                 
         BE    FTP42                   YES                                      
         CLI   PPGMED,C'V'             MEDIA V?                                 
         BNE   FTP43                   NO - UNKNOWN MED - LEAVE BLANK           
*                                                                               
FTP42    MVI   0(R2),C'P'              SYSTEM                                   
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
         CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BE    FTP43U                  YES, DON'T REPEAT PRODUCT CODE           
         CLC   QAGENCY,=C'OO'          AGENCY OU?                               
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
         CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BNE   *+12                    NO                                       
         CLI   SEQNUM,C'2'             TAX LINE 2?                              
         BE    *+10                    YES                                      
         MVC   0(2,R2),=C'1Z'          TAX CODE                                 
         LA    R2,2(R2)                BUMP R2                                  
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
*                                                                               
         MVI   0(R2),C'|'              DELIMITER (WITHOLDING TAX CODE)          
         LA    R2,1(R2)                BUMP R2                                  
*                                                                               
FTP60    CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BNE   FTP61                   NO                                       
         CLI   SEQNUM,C'2'             TAX LINE 2?                              
         BE    *+10                    YES                                      
         MVC   0(4,R2),=C'CA00'        TAX JURSIDICTION                         
         LA    R2,4(R2)                BUMP R2                                  
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
         B     FTP65                                                            
*                                                                               
FTP61    CLC   QAGENCY,=C'UB'          AGENCY UB?                               
         BNE   FTP62                   NO                                       
         CLC   QCLIENT,=C'PAB'                                                  
         BNE   FTP62                                                            
         MVC   0(4,R2),=C'PRGB'        TAX JURSIDICTION                         
         LA    R2,4(R2)                BUMP R2                                  
         BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
         B     FTP65                                                            
*                                                                               
FTP62    MVC   0(10,R2),=C'9999999999' TAX JURSIDICTION                         
         MVI   10(R2),C'|'             DELIMITER                                
         LA    R2,11(R2)               BUMP R2                                  
*                                                                               
FTP65    LA    R1,PPGEU1               EST UCOMM2 FOR AGENCY UB                 
         CLC   QAGENCY,=C'UB'          AGENCY UB?                               
         BE    *+8                     YES                                      
         LA    R1,PPGPU2               NO - USE PRD UDEF 2 FOR OO/OU            
         CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BNE   FTP66                   NO                                       
         CLI   SEQNUM,C'1'             TAX LINE?                                
         BNE   FTP67                   YES - NO INTERNAL ORDER NUMBER           
FTP66    MVC   0(10,R2),0(R1)          INTERNAL ORDER NUM                       
         LA    R2,11(R2)               MAXED OUT DATA + 1                       
FTP67    BAS   RE,FINDLAST             FIND END, ADD DELIMITER & BUMP           
         MVI   0(R2),C'|'              ONE MORE DELIMITER FOR THE END           
*                                                                               
         L     R1,=A(PGTTAPE)          ADD RECORD TO PGTTAPE                    
         LA    R0,P                    P HAS LINE DATA                          
         PUT   (1),(0)                 PUT LINE RECORD TO TAPE                  
         MVI   P,X'40'                 INIT P TO SPACES                         
         MVC   P+1(L'P-1),P            INIT P TO SPACES                         
         CLC   QAGENCY,=C'OU'          AGENCY OU?                               
         BNE   FTPX                    NO                                       
         CLI   SEQNUM,C'1'             NEED TAX LINES?                          
         BNE   FTPX                    NO - ALREADY DID THEM                    
*                                                                               
         ZAP   MYPACK,PPGGST           GST (INCLUDES HST)                       
         AP    MYPACK,PPGPST           PST (HAD HST TAKEN OUT)                  
         MVI   SEQNUM,C'2'             ADDITIONAL LINE FOR TAXES                
         B     FTP15                   GO AGAIN                                 
*                                                                               
         DROP  R4                      DROP USINGS                              
*                                                                               
FTP70    CLOSE (PGTTAPE)               CLOSE PGTTAPE                            
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
         L     RF,ADMASTC              A(MASTC)                                 
         USING MASTD,RF                MASTD DSECT                              
         L     R1,MCAEXTRA             EXTRA DATA AREA                          
         USING MCEXTRA,R1              EXTRA DATA AREA DSECT                    
         MVC   MQAGYID,MCAGYCOD        AGY LABEL (CTAGCCOD) ON IDI RE           
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
         TITLE 'DSECTS AND WORK AREAS'                                          
PPGTWRKD DSECT                                                                  
PPGTWRK  DS    0C                                                               
VDOWNLD  DS    A                                                                
VFTPHUB  DS    A                                                                
VGETVEN  DS    A                                                                
AMQRPT   DS    A                                                                
VGETUSER DS    A                                                                
VDLFLD   DS    A                                                                
VPPBVAL  DS    A                                                                
VPFMTINO DS    A                                                                
VOFFOUT  DS    A                                                                
VDDUCOM  DS    A                                                                
RELO     DS    A                                                                
ADCLI    DS    A                                                                
ADESM    DS    A                                                                
AAAEST   DS    A           AAA ESTIMATE                                         
ADPRO    DS    A                                                                
ADMASTC  DS    A                                                                
ERRORSW  DS    XL1                                                              
*        X'01' MISSING ACCOUNT NUMBERS                                          
*        X'02' MISSING INTERNAL ORDER NUMBERS                                   
*        X'04' MISSING LEGAL ENTITIES                                           
*        X'08' MIXED LEGAL ENTITES ON AN INVOICE                                
*        X'80' MISSING POL ESTIMATE                                             
*        X'40' UNKNOWN MEDIA TYPE                                               
*                                                                               
DOWNACT  DS    CL1                                                              
SVLINE   DS    XL1                                                              
SVFORCEH DS    CL1                                                              
MYPPGK   DS    CL64        FOR SAVING PPG'S KEY AND KEYSAVE                     
LASTCPE  DS    CL8         LAST CLT/PRD/EST                                     
SVMOS    DS    CL4         SAVED MOS - THEIR FORMAT (MMYY)                      
SVEST    DS    CL3         SAVED ESTIMATE   (NNN)                               
ESTBRT   DS    CL1         BARTER INDICATOR FOR ESTIMATE                        
EUSER1   DS    CL53        OUTPUT OF GETUSER - ESTIMATE USER 1                  
EUSER2   DS    CL37                          - ESTIMATE USER 2                  
PPUSER1  DS    CL53        OUTPUT OF GETUSER - PRODUCT USER 1                   
PPUSER2  DS    CL37                          - PRODUCT USER 2                   
START    DS    CL6                                                              
END      DS    CL6                                                              
TAXSW    DS    XL1                                                              
SEQNUM   DS    CL1                                                              
SAVVEND  DS    CL09         AGY VENDOR CODE                                     
SAVFILE  DS    CL13         BDE FILE NAME                                       
SAVTERM  DS    CL4          PAYMENT TERMS                                       
SAVFMT   DS    CL1          FORMAT O=OLD, N=NEW                                 
MYPACK   DS    PL6          USED WHEN REPORTING GST/HST/PST                     
MYPPOS   DS    PL6          POSITIVE OF MYPACK                                  
         DS    0D                                                               
SVBILL   DS    PL6          SAVED BILTOTS+24                                    
*                                                                               
MYGST    DS    PL8                                                              
MYPST    DS    PL8                                                              
MYHST    DS    PL8                                                              
MYDUB    DS    PL8                                                              
*                                                                               
FIRSTSW  DS    X                                                                
REVSW    DS    CL1                 REVISION STATUS                              
RETAIL   DS    CL1                 'Y' IF RETAIL BILL                           
OACTSW   DS    CL1         SET TO 1 IF BILL FOR OFFICE PROCESSED                
RACTSW   DS    CL1         SET TO 1 IF BILL FOR REQUEST PROCESSED               
LSTBLKY  DS    XL25                                                             
         DS    0F                                                               
EATOTS   DS    XL16                                                             
TAPTYP   DS    C                                                                
LASTKEY  DS    0CL32                                                            
         DS    CL4                                                              
LASTKCLT DS    CL3                                                              
LASTKPRD DS    CL3                                                              
         DS    CL22                                                             
*                                                                               
LASTPROF DS    XL16                                                             
*                                                                               
B1PROF   DS    XL16                                                             
B1XPROF  DS    XL16                                                             
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                 FULL INVOICE NUMBER                         
*                                   ME-MN-NNNN                                  
*                               OR  MN-ME-NNNN                                  
DSINVNO  DS    CL7                  MN-NNNN (OR YMMNNNN)                        
*                                                                               
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
*                                                                               
BILTOTS  DS    7PL6                                                             
PRDTOTS  DS    7PL6                                                             
CLTTOTS  DS    7PL6                                                             
OFFTOTS  DS    7PL6                                                             
REQTOTS  DS    7PL6                                                             
RUNTOTS  DS    7PL6                                                             
TRUNTOTS DS    7PL6                FOR TAPE REQS                                
*                                                                               
CTODAY   DS    CL8                 YYYYMMDD                                     
TIMEOFD  DS    CL8                 HH.MM.SS                                     
*                                                                               
MQMAPNM  DS    CL14                SFTPDISK.PROD.                               
*                                                                               
DSNAME   DS    CL35                DSN -  BIL.SYS.AGID.DYYYMMDD.THHMM           
*                                                                               
TESTMQ   DS    CL1                 TEST MQ BROKER FLAG                          
GTSFTP   DS    CL1                 FTP VIA THE HUB FLAG                         
GTOPENSW DS    CL1                 GT TAPE OPEN SWITCH                          
ELEM     DS    XL(MQMSGLNQ)        BUILD MQ MESSAGE HERE                        
*                                                                               
INVPARS  DS    6F               FOR INVTAB BINSRCH                              
*                                                                               
INVMAX   EQU   10000            ALLOW 10000 INVOICES PER CLIENT                 
*                                                                               
CLTZINV  DS    F                ZERO INVOICE TOTALS                             
CLTINVS  DS    F                INVOICE TOTALS                                  
OFFINVS  DS    F                                                                
REQINVS  DS    F                                                                
RUNINVS  DS    F                                                                
TRUNINVS DS    F                                                                
*                                                                               
STAGSW   DS    X                                                                
TOTSW    DS    X                                                                
SAVMAX   DS    X                                                                
SVCD     DS    PL8                                                              
*                                                                               
HYOSW    DS    CL1          Y IF OUTRIDER (HY) SOME CLTS                        
HYMSW    DS    CL1          Y IF MEDIACOM (HY) SOME CLTS                        
HYMCSW   DS    CL1          Y IF MCMMP (HY) SOME CLTS                           
NEWCLT   DS    CL1                                                              
SVMID    DS    CL132                                                            
SVMID2   DS    CL132                                                            
*                                                                               
SVAGENCY DS    CL2                                                              
*                                                                               
BDEHSW   DS    XL1                                                              
BDEH1    DS    CL132                                                            
BDEH2    DS    CL132                                                            
BDEH3    DS    CL132                                                            
BDEH4    DS    CL132                                                            
*                                                                               
       ++INCLUDE PPBVALD           NEW PPBVAL DSECT                             
*                                                                               
PBIREC   DS    CL256                                                            
         DS    CL34                TO ALLOW FOR 300 BYTE RECORDS                
         DS    CL114                                                            
*                                                                               
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
UCALL    EQU   *-UCTTLS                                                         
UCOMQ    EQU   *-UCOMBLK                                                        
*                                                                               
BILLINED DSECT                                                                  
BLINE    DS    0C                                                               
BLPRD    DS    CL3                                                              
         DS    CL1                                                              
BLEST    DS    CL3                                                              
         DS    CL1                                                              
BLPER    DS    CL6                                                              
         DS    CL1                                                              
BLINO    DS    CL6                                                              
         DS    CL1                                                              
BLRDAT   DS    CL8                                                              
         DS    CL1                                                              
BLBDAT   DS    CL5                                                              
         DS    CL1                                                              
BLTYP    DS    CL6                                                              
BLGRS    DS    CL15                                                             
BLNET    DS    CL15                                                             
BLCD     DS    CL15                                                             
BLAC     DS    CL15                                                             
BLBIL    DS    CL15                                                             
BLGST    DS    CL13                                                             
         SPACE 3                                                                
BLESTD   EQU   BLEST-BLINE                                                      
BLPERD   EQU   BLPER-BLINE                                                      
BLRDATD  EQU   BLRDAT-BLINE                                                     
         PRINT OFF                                                              
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
       ++INCLUDE PPREPWORK                                                      
*                                                                               
       ++INCLUDE PPREPWORK2                                                     
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
BPLEQ    EQU   6                   LENGTH OF PBILLREC PACKS (PL6)               
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PAORREC                                                        
*                                                                               
       ++INCLUDE DDUCOMD                                                        
*                                                                               
       ++INCLUDE DDMASTD                                                        
*                                                                               
PPGRECD  DSECT                                                                  
PPGMED   DS    CL1           MEDIA (ALSO IN KEY)                                
PPGPRO   DS    CL3           ONLY PRESENT FOR SOME AGYS                         
PPGEST   DS    XL2           ESTIMATE (ALSO IN KEY)                             
PPGIDATE DS    XL3           INVOICE DATE                                       
PPGINVF  DS    CL10          FULL INVOICE NUMBER                                
PPGMOS   DS    XL2           FROM PBILKMOS                                      
PPGCDTYP DS    CL1           CD ITEMS INDICATOR                                 
PPGEU1   DS    CL32          EST USER 1                                         
PPGEU2   DS    CL16          EST USER 2                                         
PPGENAME DS    CL20          ESTIMATE NAME                                      
PPGEBRT  DS    CL1           BARTER INDICATOR                                   
PPGAOR   DS    CL1           AOR BILL INDICATOR                                 
PPGPU1   DS    CL32          PRD USER 1                                         
PPGPU2   DS    CL16          PRD USER 2                                         
PPGALLO  DS    CL32          ALLOCATION - PRD EST UCOM                          
PPGEUCOM DS    CL8           ESTIMATE UCOMM 1 FOR OO & OU                       
*        THE FIELDS WILL BE THE VALUES FOR THE FIRST BILL                       
*        FOR THE BINSRCH KEY                                                    
*                                                                               
*        THE FIELDS BELOW ARE THE TOTALS FOR THE INVOICE                        
*        (BINSRCH KEY)                                                          
PPGBAMT  DS    PL6                                                              
PPGGST   DS    PL6                                                              
PPGPST   DS    PL6                                                              
*                                                                               
PPGRECL  EQU   *-PPGMED                                                         
*                                                                               
LEDIS    EQU   PPGPU1-PPGMED                                                    
AMTDIS   EQU   PPGBAMT-PPGMED                                                   
GSTDIS   EQU   PPGGST-PPGMED                                                    
PSTDIS   EQU   PPGPST-PPGMED                                                    
*                                                                               
INVTAB   CSECT                                                                  
         ORG   *+(INVMAX*4)                                                     
         DC    X'00'                                                            
*                                                                               
PPGTABLE CSECT                                                                  
         ORG   *+(4000*PPGTLEN)                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066PPREPGT02 11/21/19'                                      
         END                                                                    
