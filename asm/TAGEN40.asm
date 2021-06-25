*          DATA SET TAGEN40    AT LEVEL 041 AS OF 04/21/15                      
*PHASE T70240B,*                                                                
         TITLE 'T70240 - ESTIMATE REPORT SCREEN VALIDATION'                     
T70240   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70240                                                         
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=LOCAL WORKING STORAGE                     
         USING ESTD,R7                                                          
         SPACE 3                                                                
***********************************************************************         
*                                                                     *         
*        THIS PROGRAM VALIDATES ESTIMATE REPORT REQUEST SCREEN.       *         
*        EACH TIME IN IT CLEARS ALL LOCAL STORAGE AREAS AND SAVES     *         
*        VARIOUS DATA IN THESE AREAS FOR THE REPORT GENERATION        *         
*        PROGRAM.  THE VERY LAST THING IT DOES IS STAMP THE           *         
*        ESTIMATE RECORD WITH THE CURRENT DATE AND TIME.              *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
         CLI   MODE,VALKEY         RECOGNIZE ONLY MODE VALKEY                   
         BNE   XIT                                                              
         GOTO1 INITIAL,DMCB,(X'80',PFTAB)  INITIALIZE - CLEAR W/S               
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'           IF WE'RE ONLINE                           
         BE    VK3                                                              
         CLC   ERPLAST+1(2),=X'0101'  AND SCREEN JUST LOADED                    
         BNE   VK3                                                              
         TM    TRNSTAT2,CTFLSTER      IF FULL SESSION STEREO                    
         BZ    *+8                                                              
         BAS   RE,RESETSCR            RESET SCREEN FIELD LABELS                 
*                                                                               
VK3      BAS   RE,SETPFK           SET DEFAULT PFKEY LINE                       
         CLI   PFAID,13            IF TOGGLE PFKEY                              
         BNE   *+12                                                             
         BAS   RE,SETPRT           RESET PRINT FIELD                            
         BE    PRTOGGLE            TELL USER PRINT FIELD CHANGED                
         SPACE 1                                                                
         BAS   RE,SETCNV           SET DEFAULT CANADIAN CONVERSION              
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',ERPAGYH),ERPAGYNH  AGENCY             
         MVC   AGYNAME,TGNAME                      SAVE NAME                    
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         BAS   RE,SETVALS                          SET AGENCY VALUES            
         SPACE 1                                                                
         MVC   AIO,AIO2                     USING AIO2, VAL. ESTIMATE           
         GOTO1 RECVAL,DMCB,TLESCDQ,(X'18',ERPESTH),ERPESTNH                     
         MVC   ESTKEY,KEY                   SAVE ESTIMATE REC. KEY              
         MVC   ESTDA,DMDSKADD               SAVE D/A                            
         MVC   ESTNAME,TGNAME               SAVE NAME                           
         SPACE 1                                                                
         XC    TGCLI,TGCLI         PRE-CLEAR CLIENT                             
         L     R3,AIO                                                           
         MVI   ELCODE,TAESELQ      GET FIRST ESTIMATE ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   VK5                                                              
         USING TAESD,R3                                                         
         CLI   TAESTYPE,TAESTCLI   SHOULD BE CLIENT TYPE                        
         BNE   VK5                                                              
         MVC   TGCLI,TAESCLI       SAVE CLIENT FOR CONTROL RECORD READ          
         SPACE 1                                                                
VK5      MVC   NARR,SPACES         GET NARRATIVE                                
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPG))                                     
         BNE   VK6                                                              
         L     R3,TGELEM                                                        
         USING TACMD,R3                                                         
         ZIC   RE,TACMLEN                                                       
         SH    RE,=Y(TACMLNQ+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NARR(0),TACMCOMM                                                 
         SPACE 1                                                                
**NO-OP**MVI   NARRH,L'NARR+8                                                   
**NO-OP**GOTO1 CHAROUT,DMCB,TACMELQ,NARRH,TACMTYPG  GET NARRATIVE               
**NO-OP**OC    NARR,SPACES                                                      
         SPACE 1                                                                
VK6      GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTREV      GET REVISION NUMBER         
         MVC   REVISION,TGNAME                                                  
         SPACE 1                                                                
         BAS   RE,FNDTAAC          DISPLAY LAST REQUESTED INFO                  
         SPACE 1                                                                
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         BAS   RE,SETPROF          SET PROFILE DEFAULTS                         
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF WE'RE ONLINE                              
         BE    *+14                                                             
         CLC   ERPLAST+1(2),=X'0101'  AND SCREEN JUST LOADED                    
         BE    USERHLP                GIVE USER A SHOT                          
         SPACE 1                                                                
         XC    ERPES2N,ERPES2N     VALIDATE 2ND ESTIMATE                        
         OI    ERPES2NH+6,X'80'                                                 
         CLI   ERPES2H+5,0                                                      
         BE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLESCDQ,(X'08',ERPES2H),ERPES2NH                     
         MVC   ESTKEY2,KEY         SAVE ITS KEY                                 
         SPACE 1                                                                
VK10     XC    ERPES3N,ERPES3N     VALIDATE 3RD ESTIMATE                        
         OI    ERPES3NH+6,X'80'                                                 
         CLI   ERPES3H+5,0                                                      
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLESCDQ,(X'08',ERPES3H),ERPES3NH                     
         MVC   ESTKEY3,KEY         SAVE ITS KEY                                 
         SPACE 1                                                                
VK20     XC    ERPES4N,ERPES4N     VALIDATE 4TH ESTIMATE                        
         OI    ERPES4NH+6,X'80'                                                 
         CLI   ERPES4H+5,0                                                      
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLESCDQ,(X'08',ERPES4H),ERPES4NH                     
         MVC   ESTKEY4,KEY         SAVE ITS KEY                                 
         SPACE 1                                                                
VK30     BAS   RE,VALBOT           VALIDATE BOTTOM OF SCREEN                    
         SPACE 1                                                                
         CLI   ERPSACC,TAEPRWC     IF WORK-CODE RECAP REQUESTED                 
         BNE   *+8                                                              
         BAS   RE,GETIF            GET INTERFACE RECORD FOR AGENCY              
         SPACE 1                                                                
         BAS   RE,STAMP            STAMP EST. RECORD WITH CURRENT TIME          
         SPACE 1                                                                
         MVI   REQPRI2,C'7'        SET PRIORITY IN CASE SOON JOB                
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    *+8                                                              
         MVI   PQSW,2              DON'T LET GENCON OPEN PQ                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET DEFAULT PFKEY LINE                                
         SPACE                                                                  
SETPFK   NTR1                                                                   
         TM    WHEN,X'20'          IF PROCESSING SOON                           
         BZ    *+12                                                             
         BAS   RE,SETPFKN          SET PFKEY LINE TO NOW                        
         B     *+8                                                              
         BAS   RE,SETPFKS          ELSE SET PFKEY LINE TO SOON                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TOGGLE PRINT FIELD (NOW/SOON)                         
         SPACE                                                                  
SETPRT   NTR1                                                                   
         TM    WHEN,X'20'          IF PROCESSING SOON                           
         BZ    SETPRT10                                                         
         XC    CONWHEN,CONWHEN                                                  
         MVC   CONWHEN(4),=C'NOW,'   RESET TO NOW                               
         MVC   CONWHEN+4(3),REMUSER                                             
         MVI   CONWHENH+5,7                                                     
         BAS   RE,SETPFKS          RESET PFKEY TO SOON                          
         B     SETPRT20                                                         
*                                                                               
SETPRT10 XC    CONWHEN,CONWHEN                                                  
         MVC   CONWHEN(5),=C'SOON,'  RESET TO PROCESS SOON                      
         MVC   CONWHEN+5(3),REMUSER                                             
         MVI   CONWHENH+5,8                                                     
         BAS   RE,SETPFKN          RESET PFKEY TO NOW                           
*                                                                               
SETPRT20 OI    CONWHENH+6,X'80'    TRANSMIT NEW PRINT FIELD                     
         B     YES                                                              
         SPACE                                                                  
*              ROUTINE TO SAVE CURRENT CANADIAN CONVERSION RATE                 
         SPACE 1                                                                
SETCNV   NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)                                    
         BE    *+6                 RATE                                         
         DC    H'0'                                                             
         SPACE 1                                                                
         USING TASYD,R3                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TASYELQ      GET SYSTEM ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DUB,TASYCCVT        SAVE CANADIAN CONVERSION RATE                
         CVB   R1,DUB                                                           
         ST    R1,DEFEXCH                                                       
         NI    OTHOPT,X'FF'-OTHEXCH                                             
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 1                                                                
SETPFKN  DS    0H                    PF13 WILL SAY 'SWITCH TO NOW'              
         MVC   ERPALTP(19),=CL19'PF13=Switch to NOW'                            
         OI    ERPALTPH+6,X'80'      TRANSMIT NEW PF13                          
         OI    ERPALTPH+1,X'08'      AND SET TO HIGH INTENSITY                  
         BR    RE                                                               
         SPACE                                                                  
SETPFKS  DS    0H                    PF13 WILL SAY 'SWITCH TO SOON'             
         MVC   ERPALTP(19),=CL19'PF13=Switch to SOON'                           
         OI    ERPALTPH+6,X'80'      TRANSMIT NEW PF13                          
         OI    ERPALTPH+1,X'08'      AND SET TO HIGH INTENSITY                  
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO SET AGENCY DEFAULT VALUES                             
         SPACE 1                                                                
SETVALS  NTR1                                                                   
         MVI   CSFSTAT,0                                                        
*                                                                               
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS ELEMENT                   
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   SETV20                                                           
         USING TAAYD,R3                                                         
         MVC   AGYSTAT,TAAYSTAT    SAVE STATUS BYTE                             
         MVC   AYSRULE,TAAYSGNS    SIGNATORY CALC RULE                          
         TM    TAAYSTA6,TAAYJPCY   IF AGENCY TAKES CSF,                         
         BNO   *+8                                                              
         OI    CSFSTAT,CSFAGYY     SET AGENCY CSF YES STATUS                    
*                                                                               
SETV20   MVC   TGOFF,TAAYTPOF      AND OFFICE                                   
*                                                                               
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         B     SETV25                                                           
SETV23   BAS   RE,NEXTEL                                                        
SETV25   BNE   XIT                                                              
         USING TABRD,R3                                                         
         TM    TABRSTAT,TABRSACP   REGULAR RATES ONLY                           
         BO    SETV23                                                           
*                                                                               
         MVC   TGEMP,TGTPEMP       SET DEFAULT EMPLOYER                         
         OC    TABROEOR,TABROEOR   IF EMPLOYER OVERRIDE DEFINED,                
         BZ    *+10                                                             
         MVC   TGEMP,TABROEOR      MOVE TO GLOBAL STORAGE                       
*                                                                               
         MVC   AYBRSTAT,TABRSTAT      SAVE STATUS BYTE                          
         MVC   AYHRULE,TABRHRLS                                                 
         GOTO1 BTYPVAL,DMCB,TABRTYPE  LOOK UP STANDARD RATE CARD                
*                                                                               
         OC    TABRRATE,TABRRATE      IF RATE OVERRIDES DEFINED                 
         BZ    *+10                                                             
         MVC   TGBSRC,TABRRATE        MOVE RATES TO GLOBAL STORAGE              
*                                                                               
         CLI   TABRTYPE,TABRTY1    IF THIS IS TYPE 1                            
         BE    SETV40                                                           
         CLI   TABRTYPE,TABRTY2            OR TYPE 2                            
         BE    SETV40                                                           
         CLI   TABRTYPE,TABRTY7            OR TYPE 7                            
         BE    SETV40                                                           
         CLI   TABRTYPE,TABRTY8            OR TYPE 8                            
         BE    SETV40                                                           
         CLI   TABRTYPE,TABRTY18           OR TYPE 18                           
         BE    SETV40                                                           
         CLI   TABRTYPE,TABRTY20           OR TYPE 20                           
         BE    SETV40                                                           
         CLI   TABRTYPE,TABRTY11           OR TYPE 11                           
         BNE   *+14                                                             
         XC    AYTNHRT,AYTNHRT       (TYPE 11 HAS NO TAX AMOUNT)                
         B     SETV50                                                           
         CLI   TABRTYPE,TABRTY21           OR TYPE 21                           
         BE    SETV30                 (USES SUTA FOR SESS. HANDLING)            
         CLI   TABRTYPE,TABRTY23           OR TYPE 23                           
         BE    SETV30                                                           
         CLI   TABRTYPE,TABRTY6            OR TYPE 6                            
         BNE   SETV60                                                           
SETV30   MVC   AYTNHRT2+2(2),TGBSUTA (TYPE 6 USES SUTA TOO)                     
SETV40   MVC   AYTNHRT+2(2),TGBFUTA   SET T&H RATE                              
SETV50   MVC   AYCORPRT+2(2),TGBHAND  AND CORP/HANDLING RATE                    
         B     SETV80                                                           
*                                                                               
SETV60   CLI   TABRTYPE,TABRTY12      IF BILLING TYPE 12                        
         BE    SETV61                                                           
         CLI   TABRTYPE,TABRTY22      OR 22                                     
         BE    SETV61                                                           
         CLI   TABRTYPE,TABRTY24      OR 24                                     
         BNE   SETV69                                                           
SETV61   MVC   AYTNHRT+2(2),TGBFUTA   SET FUTA & HANDLING                       
         MVC   AYTNHRT2+2(2),TGBHAND  SEPERATELY                                
         B     SETV70                                                           
SETV69   LH    R1,TGBFUTA             ELSE TAKE HIGHEST T&H RATE                
         AH    R1,TGBHAND             + HANDLING RATE                           
         STH   R1,AYTNHRT+2           SAVE NEW T&H RATE                         
SETV70   MVC   AYCORPRT+2(2),TGBCORP  SAVE CORP RATE                            
*                                                                               
SETV80   BAS   RE,ADDLBR                                                        
*                                                                               
SETV90   MVC   AYCANTAX+2(2),TGBCAN   CANADIAN TAX RATE                         
*                                                                               
         XC    AYSGNINF,AYSGNINF                                                
*                                                                               
         USING TANUD,R4                                                         
         MVI   ELCODE,TANUELQ      GET SIGNATORY RATE                           
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSGN))                                     
         BNE   SETVX                                                            
         L     R4,TGELEM                                                        
         ZIC   RF,TANULEN                                                       
         AHI   RF,-3                                                            
         GOTO1 CASHVAL,DMCB,TANUMBER,(RF)                                       
         CLI   0(R1),0                                                          
         BNE   SETVX                                                            
         MVC   AYSGNRTE,6(R1)                                                   
         SPACE                                                                  
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSCP))                                     
         BNE   SETVX              GET SIGNATORY FEE CAP                         
         L     R4,TGELEM                                                        
         ZIC   RF,TANULEN                                                       
         AHI   RF,-3                                                            
         GOTO1 CASHVAL,DMCB,TANUMBER,(RF)                                       
         CLI   0(R1),0                                                          
         BNE   SETVX                                                            
         MVC   AYSGNCAP,4(R1)                                                   
         DROP  R4                                                               
SETVX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY LAST CHANGED INFORMATION                      
         SPACE 1                                                                
FNDTAAC  NTR1                                                                   
         B     FNDTAAC9            LOOK FOR ACTIVITY ELE. ON THIS REC           
*                                                                               
FNDTAAC5 GOTO1 SEQ                                                              
         CLC   KEY(TLESSEQ-TLESD),ESTKEY                                        
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
*                                                                               
FNDTAAC9 GOTO1 ACTVOUT,DMCB,(X'80',ERPLREQH)  DISPLAY LAST REQUEST INFO         
         OC    ERPLREQ,ERPLREQ     IF NOTHING FOUND                             
         BZ    FNDTAAC5            CHECK FOR ANOTHER RECORD                     
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO RESET THE SCREEN FIELD LABELS FOR STEREO              
         SPACE 1                                                                
         USING STERD,R1                                                         
RESETSCR NTR1                                                                   
         LA    R1,STERTAB          R1=A(ENTRY IN STEREO LABEL TABLE)            
RESSC5   CLI   0(R1),X'FF'         TEST NOT END OF TABLE                        
         BE    RESSCX                                                           
         LH    R2,STERDSP                                                       
         AR    R2,RA               R2=A(FIELD HEADER)                           
         ZIC   RF,0(R2)            COMPUTE R1=LENGTH OF FIELD                   
         SH    RF,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR ORIG LABLE                             
         MVC   8(4,R2),STERLAB     MOVE IN NEW LABLE                            
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         LA    R1,STERNEXT         BUMP TO NEXT FIELD IN TABLE                  
         B     RESSC5              KEEP LOOPING TILL END                        
RESSCX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET DEFAULT ESTIMATE PROFILE OPTIONS                  
         SPACE 1                                                                
SETPROF  NTR1                                                                   
         XC    ELEMENT,ELEMENT     DUMMY AREA IN CASE NOT ON FILE               
         L     R3,AIO              INITIALIZE I/O AREA AS WELL                  
         MVI   0(R3),0                                                          
         GOTO1 RECVAL,DMCB,TLCTCDQ,(X'24',0)  GET CONTROL RECORD                
         BNE   SETP1                                                            
         MVI   ELCODE,TAEPELQ      GET ESTIMATE PROFILE ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    SETP2                                                            
         SPACE 1                                                                
SETP1    LA    R3,ELEMENT          NOT FOUND - POINT TO DUMMY AREA              
         USING TAEPD,R3            SET NON-SCREEN DEFAULTS                      
         MVI   TAEPFILT,C'C'       ACTUALS FILTER BASIS                         
         MVI   TAEPCOMM,TAEPCYES   COMMISSION                                   
         MVC   TAEPRATE,=H'1765'   COMMISSION RATE                              
         MVI   TAEPMTH,1           START FISCAL MONTH                           
         MVI   TAEPEXP,C'N'        EXPIRY EXTEND                                
         MVI   TAEPPAGE,C'N'       NEW PAGE FOR EACH COMMERCIAL                 
         MVI   TAEPSUPP,C'N'       SUPPRESS INACTIVE COMMERCIALS                
         MVI   TAEPREL,C'Y'        ESTIMATE ON RELEASED COMMERCIALS             
         MVI   TAEPTNH,C'N'        USE TNH RATE FOR CORPS                       
         SPACE 1                                                                
SETP2    MVC   BYTE,AYHRULE                                                     
         SPACE 1                                                                
         LA    RF,AGYPROFS         SET AGENCY LEVEL NON-SCREEN VALUES           
         USING PROFFRST,RF                                                      
         MVC   PROFBTYP,TGBTYPE    BILLING TYPE                                 
         MVC   PROFHRUL,BYTE       HANDLING RULE                                
         MVC   PROFFILT,TAEPFILT   ACTUALS FILTER BASIS                         
         MVC   PROFCOMM,TAEPCOMM   COMMISSION                                   
         MVC   PROFMTH,TAEPMTH     START FISCAL MONTH                           
         MVC   PROFEXP,TAEPEXP     EXPIRY EXTEND                                
         MVC   PROFMUS,TAEPMUS     SEPARATE MUSIC                               
         MVC   PROFPAGE,TAEPPAGE   NEW PAGE FOR EACH COMMERCIAL                 
         MVC   PROFSUPP,TAEPSUPP   SUPPRESS INACTIVE COMMERCIALS                
         MVC   PROFREL,TAEPREL     ESTIMATE ON RELEASED COMMLS                  
         MVC   PROFTNH,TAEPTNH     USE TNH RATE FOR CORPS                       
         MVC   PROFHNW,TAEPHNW     INCLUDE HNW IN NET                           
         MVC   PROFTNHC,TAEPTNHC   COMBINE TNH                                  
         DROP  RF                                                               
         MVC   AYCOMM+2(2),TAEPRATE  COMMISSION RATE                            
         SPACE 1                                                                
         LA    R4,PROFTAB          R4=A(PROFILE SETTING TABLE)                  
         USING PROFD,R4                                                         
SETP4    CLI   0(R4),X'FF'                                                      
         BE    XIT                                                              
         LH    R2,PROFFDSP                                                      
         AR    R2,RA               R2=A(FIELD HEADER)                           
         CLI   5(R2),0                                                          
         BNE   SETP6               THERE'S INPUT - SKIP                         
         SPACE 1                                                                
         ZIC   R1,PROFEDSP                                                      
         AR    R1,R3               R1=A(FIELD IN ELEMENT)                       
         SPACE 1                                                                
         CLI   0(R1),0             IF PROFILE VALUE NOT DEFINED                 
         BNE   *+10                                                             
         MVC   0(1,R1),PROFDFLT    SET TO DEFAULT                               
         SPACE 1                                                                
         MVC   8(1,R2),0(R1)       MOVE PROFILE VALUE TO FIELD                  
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
SETP6    LA    R4,PROFNEXT         BUMP TO NEXT ENTRY IN TABLE                  
         B     SETP4               AND CONTINUE                                 
         EJECT                                                                  
*              ROUTINES TO VALIDATE BOTTOM OF SCREEN                            
         SPACE 1                                                                
VALBOT   NTR1                                                                   
         BAS   RE,VALFMT           VALIDATE FORMAT                              
         SPACE 1                                                                
         CLI   FORMAT,TAEPFDWN     IF DOWNLOAD FORMAT                           
         BE    *+8                                                              
         CLI   FORMAT,TAEPFRCD     OR RECAP DOWNLOAD FORMAT                     
         BNE   *+10                                                             
         MVC   TWAOUT(4),=C'DOWN'  FORCE SEPERATE REQUESTS                      
         SPACE 1                                                                
         BAS   RE,VALPD            VALIDATE PERIOD                              
         SPACE 1                                                                
         LA    R2,ERPCASTH         VALIDATE INCLUDE CAST OPTION                 
         BAS   RE,YORNVAL                                                       
         MVC   CASTOPT,8(R2)                                                    
         SPACE 1                                                                
         BAS   RE,VALAUTO          VALIDATE AUTO PAYMENT OPTION                 
         SPACE 1                                                                
         LA    R2,ERPPBSSH         VALIDATE PRINT SESSION OPTION                
         BAS   RE,YORNVAL                                                       
         BNE   *+8                                                              
         OI    REPOPT,PRNTSESS                                                  
         SPACE 1                                                                
         LA    R2,ERPHORZH         VALIDATE HORIZONTAL TOTALS OPTION            
         CLI   8(R2),C'S'          IF HORIZ TOTALS WITH NET COLUMN              
         BNE   VB12                                                             
         CLI   FORMAT,C'C'         AND FORMAT IS NOT C                          
         BE    VB15                                                             
         MVI   8(R2),C'C'          FORCE THE PLAIN VANILLA HORIZ TOTALS         
         OI    6(R2),X'80'                                                      
         B     VB15                                                             
VB12     CLI   8(R2),C'C'                                                       
         BE    VB15                                                             
         BAS   RE,YORNVAL                                                       
VB15     MVC   HORZOPT,8(R2)                                                    
         SPACE 1                                                                
         LA    R2,ERPCOMLH         VAL. COMMLS INCLUDED SUMMARY OPTION          
         BAS   RE,YORNVAL                                                       
         MVC   COMLOPT,8(R2)                                                    
         SPACE 1                                                                
         LA    R2,ERPSACCH         R2=A(RECAP FIELD)                            
         LA    R4,RCPTAB           LOOK IT UP IN TABLE                          
VB20     CLI   0(R4),X'FF'         TEST LOGICAL END OF TABLE                    
         BE    FLDINV                                                           
         CLC   8(1,R2),0(R4)       TEST MATCH ON CODE                           
         BE    *+12                                                             
         LA    R4,L'RCPTAB(R4)     BUMP TO NEXT TABLE ENTRY                     
         B     VB20                LOOP                                         
         MVC   RECAPOPT,0(R4)      SAVE RECAP OPTION                            
         SPACE 1                                                                
         LA    R2,ERPACTH          VALIDATE INCLUDE ACTUALS OPTION              
         CLI   8(R2),C'P'                                                       
         BE    VB24                                                             
         CLI   8(R2),C'X'                                                       
         BE    VB24                                                             
         CLI   8(R2),C'O'                                                       
         BE    VB24                                                             
         CLI   8(R2),C'U'                                                       
         BE    VB24                                                             
         CLI   8(R2),C'Z'                                                       
         BE    VB24                                                             
         BAS   RE,YORNVAL                                                       
VB24     MVC   ACTLOPT,8(R2)                                                    
         SPACE 1                                                                
         LA    R2,ERPFILTH         FILTER ACTUALS BY ESTIMATE NUMBER            
         CLI   8(R2),C'P'                                                       
         BE    *+8                                                              
         BAS   RE,YORNVAL                                                       
         MVC   FILTOPT,8(R2)                                                    
         SPACE 1                                                                
         LA    R2,ERPONLYH         RECAP ONLY  (OPTIONAL)                       
         CLI   5(R2),0                                                          
         BE    VB30                                                             
         BAS   RE,YORNVAL                                                       
         BNE   VB30                                                             
         LA    R2,ERPSACCH                                                      
         CLI   RECAPOPT,C'N'       REQUIRE RECAP                                
         BE    FLDINV                                                           
         MVI   ONLYOPT,C'Y'        SET USER WANTS RECAP ONLY                    
         MVI   CASTOPT,C'N'        TURN OFF CAST LIST                           
         MVI   COMLOPT,C'N'                 COMML SUMMARY                       
         SPACE 1                                                                
VB30     LA    R2,ERPLEFTH         ALIGN-LEFT                                   
         BAS   RE,YORNVAL                                                       
         MVC   LEFTOPT,8(R2)                                                    
         SPACE 1                                                                
         MVI   BOXOPT,C'Y'                                                      
         LA    R2,ERPNOBXH         SUPPRESS BOXES                               
         BAS   RE,YORNVAL                                                       
         BNE   *+8                                                              
         MVI   BOXOPT,C'N'                                                      
         SPACE 1                                                                
         LA    R2,ERPDLRH          PAY HLDS IN DLR CYCLE                        
         BAS   RE,YORNVAL                                                       
         MVC   DLROPT,8(R2)                                                     
         SPACE 1                                                                
         LA    R2,ERPFREEH         FIRST MUS FREE                               
         BAS   RE,YORNVAL                                                       
         MVC   FREEOPT,8(R2)                                                    
         SPACE 1                                                                
         LA    R2,ERPSIGH          SIGNATURE LINES                              
         BAS   RE,YORNVAL                                                       
         MVC   SIGOPT,8(R2)                                                     
         SPACE 1                                                                
         LA    R2,ERPCLAH          MERGE CLASS A PAYMENTS                       
*                                                                               
         MVI   CLAOPT,C'N'         DON'T MERGE CLA PAYMENTS                     
         TM    WHEN,X'40'          IF RUNNING NOW                               
         BO    VB45                                                             
         CLI   FORMAT,TAEPFQTR     OR IF FORMAT IS Q OR T                       
         BE    VB45                                                             
         CLI   FORMAT,TAEPFCAT                                                  
         BE    VB45                                                             
         MVI   CLAOPT,C'Y'         MUST MERGE CLA PAYMENTS                      
         CLI   FORMAT,TAEPFW       IF FORMAT IS W                               
         BNE   VB48                                                             
VB45     MVC   8(1,R2),CLAOPT                                                   
         OI    6(R2),X'80'                                                      
         B     VB50                                                             
VB48     BAS   RE,YORNVAL          ELSE VALIDATE Y OR NO                        
         MVC   CLAOPT,8(R2)                                                     
         SPACE 1                                                                
VB50     MVI   PRTDET,C'N'         DEFAULT                                      
         LA    R2,ERPDETH          PRINT DETAILS                                
         BAS   RE,YORNVAL                                                       
         MVC   PRTDET,8(R2)                                                     
         SPACE 1                                                                
         MVI   TRACEOPT,C'N'       DEFAULT                                      
         LA    R2,ERPTRCH          TRACE (INPUT OPTIONAL)                       
         CLI   5(R2),0                                                          
         BE    VBX                                                              
         CLI   TGCTSTTY,TASTTYPP   IF PROGRAMMER                                
         BNE   FLDINV                                                           
         CLI   8(R2),C'L'          LOCAL TRACE ONLY                             
         BE    *+16                                                             
         CLI   8(R2),C'G'          GLOBAL TRACE ONLY                            
         BE    *+8                                                              
         BAS   RE,YORNVAL          ELSE MUST BE Y/N                             
         MVC   TRACEOPT,8(R2)                                                   
VBX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE REPORT FORMAT                                
         SPACE 1                                                                
VALFMT   NTR1                                                                   
         XC    ERPFMTN,ERPFMTN     CLEAR FORMAT NAME                            
         OI    ERPFMTNH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,ERPFMTH          R2=A(FIELD)                                  
         SPACE 1                                                                
         LA    R4,FORMTAB          LOOK IT UP IN TABLE                          
VF6      CLI   0(R4),X'FF'                                                      
         BE    FLDINV                                                           
         CLC   8(1,R2),0(R4)       MATCH ON CODE                                
         BE    *+12                                                             
         LA    R4,L'FORMTAB(R4)                                                 
         B     VF6                                                              
         SPACE 1                                                                
         TM    WHEN,X'40'          IF PROCESSING NOW                            
         BZ    *+16                                                             
         CLI   0(R4),TAEPFNOW      ACCEPT ONLY FORMAT N                         
         BNE   FLDINV                                                           
         B     *+12                                                             
         CLI   0(R4),TAEPFNOW      ELSE ACCEPT ALL BUT FORMAT N                 
         BE    FLDINV                                                           
         MVC   FORMAT,0(R4)        SAVE FORMAT CODE                             
         MVC   FORMDPG,1(R4)       DPG PHASE                                    
         MVC   ERPFMTN,2(R4)       DISPLAY FORMAT NAME                          
         MVC   FORMNAME,2(R4)      AND SAVE IT FOR REPORT                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE REPORT PERIOD                                
         SPACE 1                                                                
VALPD    NTR1                                                                   
         MVC   PEND,=3X'FF'        INITIALIZE DEFAULT END DATE                  
         SPACE 1                                                                
         LA    R2,ERPPERH          R2=A(PERIOD FIELD)                           
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VPX                                                              
         LA    R4,BLOCK            R4=A(PERVAL BLOCK)                           
         USING PERVALD,R4                                                       
         MVC   BYTE,5(R2)          SET LENGTH                                   
         OI    BYTE,X'40'          SET TO VALIDATE AS MM/DD                     
         GOTO1 PERVAL,DMCB,(BYTE,8(R2)),('PVINSGLS+PVIN1DYL',(R4))              
         SPACE 1                                                                
         TM    4(R1),PVRCINV1      TEST START DATE INVALID                      
         BO    STRTINV                                                          
         TM    4(R1),PVRCINV2      TEST END DATE INVALID                        
         BO    ENDINV                                                           
         TM    4(R1),PVRCONE       ALLOW ONE DATE INPUT                         
         BO    *+12                                                             
         CLI   4(R1),PVRCOK        TEST GOOD RETURN CODE                        
         BNE   FLDINV                                                           
         SPACE 1                                                                
         CLI   8(R2),C'-'          TEST ONLY END DATE INPUT                     
         BNE   VP2                                                              
         MVC   PVALEEND,PVALESTA   ROUTINE RETURNS DATES IN START,              
         MVC   PVALPEND,PVALPSTA   SO MOVE TO END                               
         MVC   PVALCEND,PVALCSTA                                                
         MVC   DUB,PVALCPER        INSURE DISPLAYED END DATE IS                 
         MVI   PVALCPER,C'-'       PRECEDED BY HYPHEN                           
         MVC   PVALCPER+1(8),DUB                                                
         B     VP4                                                              
         SPACE 1                                                                
VP2      MVC   USERQSTR,PVALESTA   SAVE EBCDIC START DATE                       
         MVC   PSTART,PVALPSTA          PWOS                                    
         SPACE 1                                                                
VP4      CLI   PVALPEND,0          IF WE HAVE END DATE                          
         BE    VP6                                                              
         MVC   USERQEND,PVALEEND   SAVE EBCDIC END DATE                         
         MVC   PEND,PVALPEND            PWOS                                    
         SPACE 1                                                                
VP6      MVC   DPERIOD,PVALCPER    SAVE DISPLAYABLE                             
         SPACE 1                                                                
         TM    PVALASSM,X'77'      IF ANY ASSUMPTIONS MADE                      
         BZ    VPX                                                              
         MVC   8(17,R2),DPERIOD    RE-DISPLAY DATES ON SCREEN                   
         MVI   5(R2),17                                                         
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
VPX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES A YES OR NO OPTION FIELD                       
         SPACE 1                                                                
YORNVAL  DS    0H                                                               
         CLI   8(R2),C'Y'          IF INPUT IS 'Y'                              
         BER   RE                  RETURN CC EQ                                 
         CLI   8(R2),C'N'                                                       
         BNE   FLDINV                                                           
         LTR   RE,RE               RETURN CC NOT EQUAL IF 'N' INPUT             
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE TO VALIDATE AUTO PAYMENT OPTION                          
         SPACE 1                                                                
VALAUTO  NTR1                                                                   
         LA    R2,ERPAUTOH         R2=A(FIELD)                                  
         SPACE 1                                                                
         LA    R4,AUTOTAB          LOOK IT UP IN TABLE                          
VAUT2    CLI   0(R4),X'FF'                                                      
         BE    FLDINV                                                           
         CLC   8(1,R2),0(R4)       MATCH ON CODE                                
         BE    *+12                                                             
         LA    R4,L'AUTOTAB(R4)                                                 
         B     VAUT2                                                            
         SPACE 1                                                                
         MVC   AUTOOPT,1(R4)       SAVE BIT EQUIVALENTS                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS INTERFACE RECORD FOR WORK-CODE RECAP                
         SPACE 1                                                                
GETIF    NTR1                                                                   
         XC    TGPCLI,TGPCLI                                                    
         GOTO1 RECVAL,DMCB,TLIFCDQ,(X'20',0)  GET AGENCY INTERFACE REC.         
         BNE   IFERR                                                            
         SPACE 1                                                                
         MVI   ELCODE,TAIFELQ      GET INTERFACE DETAILS ELEMENT                
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   IFERR                                                            
         B     XIT                                                              
         EJECT                                                                  
*              SET ADDITIONAL BILLING RULES FOR OFFICE O                        
*                                                                               
ADDLBR   NTR1                                                                   
         L     R3,AIO              GET BILLING RULES ELEMENT                    
         USING TABRD,R3                                                         
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         B     ADDLBR7                                                          
ADDLBR5  BRAS  RE,NEXTEL                                                        
ADDLBR7  BNE   ADDLBRX                                                          
         TM    TABRSTAT,TABRSACP   SPECIAL ACOPY BILLING RULES                  
         BZ    ADDLBR5                                                          
*                                                                               
         MVC   ADRBTYPE,TABRTYPE                                                
         MVC   ADRFUTA(L'TABRRATE),TABRFUTA   BILLING RATES                     
ADDLBRX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE STAMPS ESTIMATE RECORD WITH CURRENT TIME                 
         SPACE 1                                                                
STAMP    NTR1                                                                   
         MVC   KEY,ESTKEY          READ HIGH FOR PRIMARY KEY                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLESSEQ-TLESD),ESTKEY                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,15,ESTDA                                                      
*                                                                               
STMP2    GOTO1 SEQ                 FIND LAST PHYSICAL RECORD                    
         CLC   KEY(TLESSEQ-TLESD),ESTKEY                                        
         BNE   *+12                                                             
         ICM   R2,15,KEY+TLDRDA-TLDRD                                           
         B     STMP2                                                            
*                                                                               
         STCM  R2,15,KEY+TLDRDA-TLDRD                                           
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         GOTO1 ACTVIN,DMCB,(X'80',0)  AND ADD ACTIVITY ELEMENT THERE            
         GOTO1 PUTREC                                                           
         GOTO1 ACTVOUT,DMCB,(X'80',ERPLREQH)  RE-DISPLAY LAST REQ INFO          
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 1                                                                
USERHLP  MVI   MYMSGNO1,220        GIVE USER A CHANCE                           
         LA    R2,ERPPERH          CURSOR TO PERIOD FIELD                       
         B     INFEND                                                           
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
STRTINV  MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     THEEND                                                           
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     THEEND                                                           
IFERR    MVI   ERROR,ERINTER       MISSING INTERFACE RECORD                     
         LA    R2,ERPSACCH                                                      
         B     THEEND                                                           
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
PRTOGGLE MVI   MYMSGNO1,242        TELL USER PRINT FIELD CHANGED                
         LH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         AR    R2,RA                                                            
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
PROFTAB  DS    0H                  PROFILE DEFAULT SETTING TABLE                
         DC    AL2(ERPFMTH-T702FFD),AL1(TAEPFMT-TAEPD,TAEPFCOM)                 
         DC    AL2(ERPCASTH-T702FFD),AL1(TAEPCAST-TAEPD),C'Y'                   
         DC    AL2(ERPAUTOH-T702FFD),AL1(TAEPAUTO-TAEPD),C'Y'                   
         DC    AL2(ERPPBSSH-T702FFD),AL1(TAEPPBSS-TAEPD),C'Y'                   
         DC    AL2(ERPACTH-T702FFD),AL1(TAEPACTL-TAEPD),C'N'                    
         DC    AL2(ERPHORZH-T702FFD),AL1(TAEPHORZ-TAEPD),C'N'                   
         DC    AL2(ERPCOMLH-T702FFD),AL1(TAEPCOML-TAEPD),C'Y'                   
         DC    AL2(ERPSACCH-T702FFD),AL1(TAEPRCAP-TAEPD),C'N'                   
         DC    AL2(ERPLEFTH-T702FFD),AL1(TAEPLEFT-TAEPD),C'Y'                   
         DC    AL2(ERPNOBXH-T702FFD),AL1(TAEPNOBX-TAEPD),C'N'                   
         DC    AL2(ERPFILTH-T702FFD),AL1(TAEPEST-TAEPD),C'N'                    
         DC    AL2(ERPDLRH-T702FFD),AL1(TAEPDLR-TAEPD),C'N'                     
         DC    AL2(ERPFREEH-T702FFD),AL1(TAEPFREE-TAEPD),C'Y'                   
         DC    AL2(ERPSIGH-T702FFD),AL1(TAEPSIG-TAEPD),C'N'                     
         DC    AL2(ERPCLAH-T702FFD),AL1(TAEPCLA-TAEPD),C'Y'                     
         DC    AL2(ERPDETH-T702FFD),AL1(TAEPDET-TAEPD),C'N'                     
         DC    X'FF'                                                            
         SPACE 2                                                                
FORMTAB  DS    0CL17               VALID INPUT FOR REPORT FORMAT                
         DC    AL1(TAEPFCOM),X'90',CL15'BY COML/CYC/USE'                        
         DC    AL1(TAEPFUSE),X'91',CL15'BY USE/CYC/COML'                        
         DC    AL1(TAEPFCAT),X'92',CL15'BY COML/USE/CAT'                        
         DC    AL1(TAEPFQTR),X'93',CL15'BY COML/USE/QTR'                        
         DC    AL1(TAEPFLB),X'95',CL15'BY COML/USE/CYC'                         
         DC    AL1(TAEPFV1),X'96',CL15'VARIANCE BY QTR'                         
         DC    AL1(TAEPFA),X'9A',CL15'BY COML/USE/DET'                          
         DC    AL1(TAEPFB),X'9B',CL15'BY USE/COML/DET'                          
         DC    AL1(TAEPFW),X'9D',CL15'BY USE/COML/DET'                          
         DC    AL1(TAEPFDWN),X'99',CL15'DOWNLOAD'                               
         DC    AL1(TAEPFNOW),X'9C',CL15'NOW REPORT'                             
         DC    AL1(TAEPFRCD),X'F0',CL15'DOWNLOAD'    COML/USE/CAT               
         DC    X'FF'                                                            
         DC    AL1(TAEPFV2),X'98',CL15'VAR BY QTR/USE'                          
         DC    X'FF'                                                            
         SPACE 2                                                                
AUTOTAB  DS    0CL2                VALID INPUT FOR AUTO PAYMENT OPTION          
         DC    C'N',AL1(0)                                                      
         DC    C'1',AL1(AUTOKBSS+AUTOKBSR+AUTOKHLD+AUTOKMUS)                    
         DC    C'2',AL1(AUTOKBSS+AUTOKBSR+AUTOKHLD+AUTOKMUS+AUTOKBSM)           
         DC    C'3',AL1(AUTOKBSS+AUTOKBSR+AUTOKHLD)                             
         DC    C'4',AL1(AUTOKMUS)                                               
         DC    C'5',AL1(AUTOKMUS+AUTOKBSM)                                      
         DC    C'6',AL1(AUTOKHLD+AUTOKMUS)                                      
*                                  THESE THREE ARE SAME AS 1,2,3                
         DC    C'Y',AL1(AUTOKBSS+AUTOKBSR+AUTOKHLD+AUTOKMUS)                    
         DC    C'M',AL1(AUTOKBSS+AUTOKBSR+AUTOKHLD+AUTOKMUS+AUTOKBSM)           
         DC    C'X',AL1(AUTOKBSS+AUTOKBSR+AUTOKHLD)                             
         DC    X'FF'                                                            
         EJECT                                                                  
RCPTAB   DS    0CL1                VALID INPUT FOR RECAP OPTIONS                
         DC    C'N'                NO RECAP                                     
         DC    AL1(TAEPRMTH)       MONTHLY TOTALS                               
         DC    AL1(TAEPRQTR)       QUARTERLY TOTALS                             
         DC    AL1(TAEPRVQT)       VERTICAL QUARTERLY TOTALS                    
         DC    AL1(TAEPRUSE)       USE BY QUARTER                               
         DC    AL1(TAEPRWC)        WORK-CODE                                    
         DC    AL1(TAEPRXCM)       CROSS-COMMERCIAL                             
         DC    AL1(TAEPRTTL)       MEDIA/USE/TITLE                              
         DC    AL1(TAEPR1)         USE/COMMERCIAL                               
         DC    AL1(TAEPRCO)        BY COMMERCIAL                                
         DC    AL1(TAEPRCOG)       BY COMMERCIAL GROSS ONLY                     
         DC    AL1(TAEPRPRJ)       PROJECTED TOTAL BY USE                       
         DC    AL1(TAEPR2)         BY COML/USE/NET TOTAL                        
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
STERTAB  DS    0H                  TABLE FOR FIELD LABELS FOR STEREO            
         DC    AL2(ERPLDESH-T702FFD),CL4'DEST'                                  
         DC    AL2(ERPLAGYH-T702FFD),CL4'AGY '                                  
         DC    AL2(ERPLESTH-T702FFD),CL4'EST '                                  
         DC    AL2(ERPLES2H-T702FFD),CL4'ES2 '                                  
         DC    AL2(ERPLES3H-T702FFD),CL4'ES3 '                                  
         DC    AL2(ERPLES4H-T702FFD),CL4'ES4 '                                  
         DC    AL2(ERPLFMTH-T702FFD),CL4'FMT '                                  
         DC    AL2(ERPLPERH-T702FFD),CL4'PER '                                  
         DC    AL2(ERPLCASH-T702FFD),CL4'CAST'                                  
         DC    AL2(ERPLAUTH-T702FFD),CL4'AUTO'                                  
         DC    AL2(ERPLPBSH-T702FFD),CL4'PBSS'                                  
         DC    AL2(ERPLHORH-T702FFD),CL4'HORZ'                                  
         DC    AL2(ERPLCOMH-T702FFD),CL4'COML'                                  
         DC    AL2(ERPLSACH-T702FFD),CL4'SACC'                                  
         DC    AL2(ERPLACTH-T702FFD),CL4'ACT '                                  
         DC    AL2(ERPLFILH-T702FFD),CL4'FILT'                                  
         DC    AL2(ERPLONLH-T702FFD),CL4'ONLY'                                  
         DC    AL2(ERPLLEFH-T702FFD),CL4'LEFT'                                  
         DC    AL2(ERPLNOBH-T702FFD),CL4'NOBX'                                  
         DC    AL2(ERPLDLRH-T702FFD),CL4'DLR '                                  
         DC    AL2(ERPLFREH-T702FFD),CL4'FREE'                                  
         DC    AL2(ERPLSIGH-T702FFD),CL4'SIG '                                  
         DC    AL2(ERPLCLAH-T702FFD),CL4'CLA '                                  
         DC    AL2(ERPLDETH-T702FFD),CL4'DET '                                  
         DC    AL2(ERPLTRCH-T702FFD),CL4'TRC '                                  
         DC    AL2(ERPLLREH-T702FFD),CL4'LREQ'                                  
         DC    X'FF'               END OF TABLE                                 
         SPACE 3                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,PFTRETRN)                                     
         DC    CL3'   ',CL8'        ',CL8'        '                             
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER PROFILE TABLE                                     
         SPACE 1                                                                
PROFD    DSECT                                                                  
PROFFDSP DS    H                   DISP. TO SCREEN FIELD HEADER                 
PROFEDSP DS    XL1                 DISP. TO FIELD IN ELEMENT                    
PROFDFLT DS    CL1                 DEFAULT VALUE FOR PROFILE                    
PROFNEXT EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER STERTAB TABLE                                     
         SPACE 1                                                                
STERD    DSECT                                                                  
STERDSP  DS    H                   DISP. TO SCREEN FIELD HEADER                 
STERLAB  DS    CL4                 NEW LABLE                                    
STERNEXT EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAESTDSECT                                                     
         EJECT                                                                  
       ++INCLUDE TAGENESTD                                                      
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR40D                                                       
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041TAGEN40   04/21/15'                                      
         END                                                                    
