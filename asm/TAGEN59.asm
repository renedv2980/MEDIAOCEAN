*          DATA SET TAGEN59    AT LEVEL 004 AS OF 11/30/12                      
*PHASE T70259E,*                                                                
         TITLE 'T70259 - TPROFILE MAINTENANCE/LIST'                             
T70259   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T70259,R7,R6,CLEAR=YES                                    
         LR    R5,RC                                                            
         USING TMPD,R5                                                          
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
                                                                                
         STCM  RB,15,MYBASERB                                                   
         BRAS  RE,INIT             INITIALIZE PROGRAM                           
                                                                                
                                                                                
         CLI   ACTNUM,ACTREST                                                   
         BE    INVRAC                                                           
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         JNE   *+8                                                              
         BRAS  RE,DK                                                            
                                                                                
         CLI   MODE,VALREC         VALIDATE REC                                 
         JNE   *+8                                                              
         BRAS  RE,VR                                                            
                                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,LISTRECS       LIST REC                                     
         JNE   *+8                                                              
         BRAS  RE,LR                                                            
                                                                                
         CLI   MODE,XRECADD        RECORD ADDEED                                
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,XRECPUT        RECORD CHANGED                               
         JNE   *+8                                                              
         BRAS  RE,DR                                                            
                                                                                
         CLI   MODE,XRECDEL        RECORD DELETED                               
         JNE   *+8                                                              
         BRAS  RE,DELALL                                                        
                                                                                
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         JNE   XIT                                                              
                                                                                
         LA    RF,HOOK             SET A(HEADLINE HOOK)                         
         ST    RF,HEADHOOK                                                      
         LA    RF,MYSPECS          SET A(SPECS)                                 
         ST    RF,SPECS                                                         
         XC    COUNTER,COUNTER     CLEAR LINE COUNTER                           
         SPACE 1                                                                
         BRAS  RE,LR               GO LIST THE RECORDS                          
                                                                                
         XIT1                                                                   
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'           MOVE KEY FIELDS TO HEADS                     
         GOTO1 PRTSCRN,DMCB,CONTAGH,TKLLSTH,H4-1                                
*****    GOTO1 (RF),(R1),SMTHEADH,SMTSELH,H7-5                                  
****     MVC   H7-5(5),SPACES      CLEAR SELECT FIELD                           
         MVI   BYTE,C'P'           RESET                                        
         J     XIT                                                              
         SPACE 2                                                                
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H8,1,C'------ -----------------------------'                     
         SSPEC H8,38,C'------ ------------------------------------'             
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
                                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
INVRAC   MVI   ERROR,INVRCACT      INVALID RECORD ACTION                        
         B     END                                                              
                                                                                
ERINV    MVI   ERROR,INVALID       INVALID INPUT                                
         J     END                                                              
                                                                                
ERMIS    MVI   ERROR,MISSING       MISSING INPUT                                
         J     END                                                              
                                                                                
ERNFD    MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         J     END                                                              
                                                                                
ERROE    LHI   RE,ERROEREQ         RATE OR EQUIVALENT REQUIRED                  
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERCMB    LHI   RE,ERRTPCMB         COMBINATION ALREADY EXISTS                   
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
ERRMORE  ZICM  RB,MYBASERB,(15)      MORE TO DISPLAY                            
         MVC   MYMSGNO,=H'77'                                                   
         OI    GENSTAT2,USGETTXT                                                
         OI    RESUMEF,RESUME_EOS                                               
         J     INFEND                                                           
                                                                                
EREROL   ZICM  RB,MYBASERB,(15)      END OF DISPLAY                             
         MVC   MYMSGNO,=H'262'                                                  
         OI    GENSTAT2,USGETTXT                                                
         J     INFEND                                                           
                                                                                
INFEND   MVI   MYMTYP,GTMINF       INFORMATION MESSAGE EXIT                     
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
ERRENFLD MVI   MYMSGNO1,2          ENTER FIELDS AS REQUIRED                     
         MVI   MYMTYP,GTMINF       INFORMATION MESSAGE EXIT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
*                                                                               
END      GOTO1 EXIT,DMCB,0                                                      
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO INITIALIZE PROGRAM                                *         
***********************************************************************         
                                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 INITIAL,DMCB,0                                                   
         LA    RE,UPDTSKS                                                       
         ST    RE,AUPDTSKS                                                      
         LA    RE,UPDPTYS                                                       
         ST    RE,AUPDPTYS                                                      
                                                                                
         CLI   ACTNUM,ACTCHA                                                    
         JNE   XIT                                                              
         CLI   TWALACT,ACTSEL                                                   
         JNE   XIT                                                              
         MVI   TWALACT,ACTDIS                                                   
         MVI   THISLSEL,0                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS FOR INIT ROUTINE                      *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,VKMAINT                                                       
         BAS   RE,VKLIST                                                        
         J     XIT                                                              
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VKMAINT  NTR1                                                                   
         TM    TKRAGYH+4,X'20'                                                  
         BNO   VK01                                                             
         TM    TKRCLIH+4,X'20'                                                  
         BNO   VK01                                                             
         B     VK10                                                             
* - FIRST TIME INITIALIZATION IF AGY OR CLIENT CHANGED                          
VK01     MVI   RESUMEF,0                                                        
         LA    RE,SVSCREEN         CLEAR                                        
         LA    RF,SVSCRNX-SVSCREEN                                              
         XCEF                                                                   
         MVI   SVACTNM,0                                                        
         XC    TGCLI,TGCLI                                                      
                                                                                
*                                                                               
VK10     CLI   ACTNUM,ACTLIST                                                   
         JE    XIT                                                              
         LA    R3,KEY                                                           
         L     R4,AIO                                                           
                                                                                
         BAS   RE,VKAGY                                                         
         BAS   RE,VKCLI                                                         
         XC    TGTASK,TGTASK                                                    
         XC    TGPTYP,TGPTYP                                                    
         GOTO1 RECVAL,DMCB,TLTRCDQ,(X'40',0),('TLTRSCDQ',0)                     
*                                                                               
         J     XIT                                                              
***********************************************************************         
*        ROUTINE TO VALIDATE KEY FOR LIST SCREEN                      *         
***********************************************************************         
                                                                                
VKLIST   NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         JNE   XIT                                                              
         MVI   SVACTNM,ACTLIST                                                  
                                                                                
         OI    GLSTSTAT,NOSELFLD                                                
         LHI   RF,TIEND-TASYSIOD                                                
         XCEFL TASYSIOD,(RF)                                                    
                                                                                
         XC    TKLAGYN,TKLAGYN                                                  
         OI    TKLAGYNH+6,X'80'                                                 
*                                                                               
         OC    TKLAGY,TKLAGY                                                    
         BNZ   *+14                                                             
         MVC   TKLAGY,TGAGY                                                     
         OI    TKLAGYH+6,X'80'                                                  
                                                                                
         LA    R2,TKLAGYH                                                       
**       GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'28',TKLAGYH),TKLAGYNH              
         MVC   TIFAGY,TGAGY                                                     
*                                                                               
VKL10    MVI   TIREAD,TLTRCDQ                                                   
         MVI   TIRDSUBT,TLTRSCDQ                                                
         MVC   TIUSERID,TWAORIG                                                 
         MVC   TIQSTAFF,TGCTSTAF                                                
                                                                                
         J     XIT                                                              
***********************************************************************         
*        ROUTINE TO VALIDATE AGENCY                                   *         
*        ON ENTRY ... R4=A(I/O AREA)                                            
***********************************************************************         
VKAGY    NTR1                                                                   
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'28',TKRAGYH),TKRAGYNH              
                                                                                
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ      GET AGENCY ELEMENT                           
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,TKRAGYH          LENGTH OF AGY ELEMENT MUST BE                
         CLI   TAAYLEN,TAAYLNQ     ATLEAST TAAYLNQ                              
         JL    ERINV                                                            
         TM    TAAYSTA7,TAAYSPPL   PRODUCTIONS PLUS AGY COMMISSION              
         JNO   ERINV                                                            
         OI    TKRAGYH+4,X'20'                                                  
         J     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
*        ROUTINE TO VALIDATE CLIENT FIELD                             *         
*        ON ENTRY ... R3 = A(KEY)                                     *         
*                     R4 = A(PRIMARY COMMERCIAL RECORD)               *         
*                     R5 = A(IN-PROGRESS COMMERCIAL DETAILS ELEMENT)  *         
***********************************************************************         
                                                                                
VKCLI    NTR1                                                                   
***      GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'28',TKRCLIH),TKRCLINH                     
                                                                                
         OI    TKRCLINH+4,X'20'                                                 
         J     XIT                                                              
                                                                                
***********************************************************************         
* VALIDATE TPROFILE TASK/PAYTYPE KEY FIELDS                                     
* R2 = DISPLAY MAINTENANCE SCREEN LINE                                          
***********************************************************************         
VALTASK  NTR1                                                                   
         L     R2,ATKR                                                          
         USING TKRD,R2                                                          
         GOTO1 RECVAL,DMCB,TLTKCDQ,TKRTSKH,('TLTKSCDQ',0)                       
                                                                                
         OI    TKRTSKH+4,X'20'                                                  
         CLI   CHKMODE,MODE_PROCIO                                              
         JNE   XIT                                                              
         GOTO1 ADDTPS,DMCB,AUPDTSKS                                             
         J     XIT                                                              
         DROP  R2                                                               
***********************************************************************         
* VALIDATE TPROFILE TASK/PAYTYPE KEY FIELDS                                     
* R2 = DISPLAY MAINTENANCE SCREEN LINE                                          
***********************************************************************         
VALPTYPE NTR1                                                                   
         L     R2,ATKR                                                          
         USING TKRD,R2                                                          
         GOTO1 RECVAL,DMCB,TLPMCDQ,TKRPAYTH,('TLPMSCDQ',0)                      
         LA    RE,TGPTYP                                                        
                                                                                
         OI    TKRPAYTH+4,X'20'                                                 
         CLI   CHKMODE,MODE_PROCIO                                              
         JNE   XIT                                                              
         GOTO1 ADDTPS,DMCB,AUPDPTYS                                             
         DROP  R2                                                               
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        LITERALS FOR VALIDATE KEY ROUTINES                           *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE KEY                                   *         
***********************************************************************         
                                                                                
         USING TLTRD,R4                                                         
DK       NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         MVC   TKRAGY,TLTRAGY                                                   
         MVC   TKRCLI,TLTRCLI                                                   
         MVI   TKRAGYH+5,L'TLTRAGY                                              
         OI    TKRAGYH+6,X'80'                                                  
         MVI   TKRCLIH+5,L'TLTRCLI                                              
         OI    TKRCLIH+6,X'80'                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'28',TKRAGYH),TKRAGYNH              
         OI    TKRAGYNH+6,X'80'                                                 
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'28',TKRCLIH),TKRCLINH                     
         OI    TKRCLINH+6,X'80'                                                 
         MVC   AIO,AIO1                                                         
         DROP  R4                                                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO VALIDATE THE RECORD                               *         
*        ON ENTRY ... AIO = A(PRIMARY PAY TYPE RECORD)                          
***********************************************************************         
VR       NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR01                                                             
         LA    RE,SVSCREEN         CLEAR                                        
         LA    RF,SVSCRNX-SVSCREEN                                              
         XCEF                                                                   
                                                                                
                                                                                
VR01     L     RE,AUPDTSKS         CLEAR                                        
         LA    RF,UPDTSKSQ                                                      
         XCEF                                                                   
         MVI   0(RE),X'FF'                                                      
         L     RE,AUPDPTYS         CLEAR                                        
         LA    RF,UPDPTYSQ                                                      
         XCEF                                                                   
         MVI   0(RE),X'FF'                                                      
                                                                                
         MVI   RDUPDATE,C'Y'         SET TO READ FOR UPDATE                     
         NI    GENSTAT1,ALL-RDUPAPPL                                            
         MVI   IOOPT,C'Y'                                                       
         MVI   TPSTATUS,0                                                       
         MVI   NEWSCRNF,C'N'                                                    
         CLI   PFAID,14                                                         
         BNE   VR10                                                             
         MVI   NEWSCRNF,C'Y'                                                    
         GOTO1 NEWSCRN                                                          
         MVI   NEWSCRNF,C'N'                                                    
         LA    R2,TKRFRSTH                                                      
         AHI   R2,TKRQ                                                          
         J     ERRENFLD                                                         
                                                                                
*-----------------------------------------------------------------              
* LOOP THROUGH ALL LINES OF CURRENT PAGE TO VALIDATE EACH LINE                  
* BY CALLING CHKSCRN ROUTINE                                                    
* WE WANT TO VALIDATE ALL LINES FIRST BEFORE DOING ANY ADDS,DELETES,            
* OR PUTS ON THE FILE TO GIVE A CLEANER EXPERIENCE OTHERWISE ANY                
* INVALID LINES ENTERS ON THE SCREEN WOULD HAVE RESULTS IN PARTIAL              
* ADDS ON PREVIOUS LINES WHICH WOULD RESULT IN DUPLICATE ENTRY ERRORS           
* AFTER CORRECT THE INVALID LINE                                                
                                                                                
VR10     LA    R3,TKRFRSTH                                                      
         USING TKRD,R3                                                          
         MVI   LINENUM,0                                                        
VR20     ST    R3,ATKR                                                          
         MVI   CHKMODE,MODE_VALIDATE                                            
         GOTO1 CHKSCRN            CHECK TO SEE WHAT CHANGED                     
         GOTO1 CHKDUPS            NO DUPLICATES ALLOW ON CURRENT SCREEN         
         AHI   R3,TKRQ            PROCESS NEXT SCREEN LINE                      
         LA    RE,TKRLSTH                                                       
         ZIC   R0,LINENUM                                                       
         AHI   R0,1                                                             
         STC   R0,LINENUM                                                       
         MVI   TPSTATUS,0                                                       
         CR    R3,RE                                                            
         JL    VR20                                                             
*-----------------------------------------------------------------              
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'         SET TO READ FOR UPDATE                     
         NI    GENSTAT1,ALL-RDUPAPPL                                            
         MVI   IOOPT,C'Y'                                                       
         MVI   TPSTATUS,0                                                       
                                                                                
*-----------------------------------------------------------------              
*--- AFTER ALL LINES OF CURRENT PAGE HAS PASSED VALIDATION    ----              
*--- LOOP THROUGH ALL LINES OF CURRENT PAGE AGAIN TO PROCESS  ----              
*--- EACH LINE TO PERFORM  ADDS, PUTS, DELETES IF NECESSARY   ----              
                                                                                
         LA    R3,TKRFRSTH                                                      
         USING TKRD,R3                                                          
         MVI   LINENUM,0                                                        
VR80     ST    R3,ATKR                                                          
         MVI   CHKMODE,MODE_PROCIO                                              
         GOTO1 CHKSCRN            CHECK TO SEE WHAT CHANGED                     
         GOTO1 PROC_IO            PROCESS IO                                    
         AHI   R3,TKRQ            PROCESS NEXT SCREEN LINE                      
         LA    RE,TKRLSTH                                                       
         ZIC   R0,LINENUM                                                       
         AHI   R0,1                                                             
         STC   R0,LINENUM                                                       
         MVI   TPSTATUS,0                                                       
         CR    R3,RE                                                            
         JL    VR80                                                             
*-----------------------------------------------------------------              
                                                                                
         DROP  R3                                                               
                                                                                
* WHEN ADDING NEW RECORD - ALSO NEED TO ADD DEFAULT AGY, CLIENT                 
* RECORD WITHOUT TASK AND PAY TYPE - TO PASS GENCON KEY READ                    
* AND FOR LISTING PURPOSE                                                       
         CLI   ACTNUM,ACTADD                                                    
         JNE   VRX                                                              
         XC    TGTASK,TGTASK                                                    
         XC    TGPTYP,TGPTYP                                                    
         GOTO1 RECVAL,DMCB,TLTRCDQ,(X'40',0),('TLTRSCDQ',0)                     
*****                                                                           
         OI    DMINBTS,X'08'               GET DELETE RECORD                    
         GOTO1 HIGH                                                             
         LA    RE,KEY                                                           
         USING TLDRD,RE                                                         
         CLC   KEY(TLTRPTYP+L'TLTRPTYP-TLTRKEY),KEYSAVE                         
****     JNE   CHKSCRX                                                          
         JNE   VR100                                                            
                                                                                
         TM    TLDRSTAT,X'80'                                                   
         JNO   ERCMB                                                            
                                                                                
*  --- TURN OFF THE DELTE BITS IN OLD KEY AND OLD RECORD -----                  
         NI    TLDRSTAT,X'FF'-X'80'        TURN OFF DELETE BIT                  
         DROP  RE                                                               
                                                                                
         GOTO1 WRITE                                                            
         OI    DMINBTS,X'08'               GET DELETE RECORD                    
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         USING TLTRD,RE                                                         
         NI    TLTRSTAT,X'FF'-X'80'        TURN OFF DELETE BIT                  
         GOTO1 PUTREC                                                           
         J     VRX                                                              
*****                                                                           
VR100    MVC   KEY,KEYSAVE                                                      
         L     RE,AIO                                                           
         USING TLTRD,RE                                                         
         XC    TLTRTASK,TLTRTASK                                                
         XC    TLTRPTYP,TLTRPTYP                                                
         DROP  RE                                                               
         GOTO1 ADDREC                                                           
                                                                                
                                                                                
VRX      NI    DMINBTS,X'FF'-X'08'                                              
         GOTOR UPDTPS,DMCB,('TLTKSCDQ',AUPDTSKS)                                
         GOTOR UPDTPS,DMCB,('TLPMSCDQ',AUPDPTYS)                                
         J     XIT                                                              
***********************************************************************         
*  CHECK SCREEN TO SEE IF LINES CHANGED                                         
*  IF TASK/PAYTYPE CHANGED - DELETE OLD RECORD AND ADD NEW ONE                  
*  IF ONLY DEFAULT RATE OR ACCOUNT EQUIVALENT CHANGED THEN PUTREC               
*  IF LINE BLANKED OUT THE DELETE REC                                           
***********************************************************************         
*                                                                               
CHKSCRN  NTR1                                                                   
*                                                                               
         L     R3,ATKR                                                          
         USING TKRD,R3                                                          
         OC    TKRTSK,SPACES                                                    
         OC    TKRPAYT,SPACES                                                   
         OC    TKRDEFR,SPACES                                                   
         OC    TKRACCT,SPACES                                                   
         LA    RE,TKRQ                                                          
         ZIC   RF,LINENUM                                                       
         MR    RE,RE                                                            
         LA    RE,SVSCREEN         THE SCREEN AS CAPTURED BY DISPLAY            
         AR    RE,RF                                                            
         ST    RE,AOLDLINE                                                      
OLDLINE  USING TKRD,RE                                                          
         OC    OLDLINE.TKRTSK,SPACES                                            
         OC    OLDLINE.TKRPAYT,SPACES                                           
         OC    OLDLINE.TKRDEFR,SPACES                                           
         OC    OLDLINE.TKRACCT,SPACES                                           
                                                                                
                                                                                
* ---------- DID KEY FIELDS TASK/PAYTYPE CHANGED? -------------                 
                                                                                
         CLC   OLDLINE.TKRTSK,TKRTSK                                            
         BNE   CHKSCR20                                                         
         CLC   OLDLINE.TKRPAYT,TKRPAYT                                          
         BNE   CHKSCR20                                                         
                                                                                
* ---------KEY FIELDS TASK/PAYTYPE DID NOT CHANGE. -----------------            
* ----CHECK TO SEE IF DATA FIELDS DEF RATE/ACCOUNT EQUIV  CHANGED --            
                                                                                
         MVI   TPSTATUS,0                                                       
         CLC   OLDLINE.TKRDEFR,TKRDEFR                                          
         BNE   *+14                                                             
         CLC   OLDLINE.TKRACCT,TKRACCT                                          
         BE    CHKSCRX                                                          
         DROP  OLDLINE                                                          
         CLI   TKRDEFRH+5,0                IGNORE BLANK LINE ENTRIES            
         BNE   *+12                        IF DISPLAYED LINES ALSO WAS          
         CLI   TKRACCTH+5,0                BLANK LINE ENTRIES                   
         BE    CHKSCR60                                                         
         XC    TGTASK,TGTASK                                                    
         XC    TGPTYP,TGPTYP                                                    
         BRAS  RE,VALTASK                                                       
         BRAS  RE,VALPTYPE                                                      
         GOTO1 RECVAL,DMCB,TLTRCDQ,(X'24',0),('TLTRSCDQ',0)                     
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TPSTATUS,TPPUT                                                   
         J     CHKSCR60                                                         
                                                                                
* --------- KEY FIELDS TASK/PAYTYPE CHANGED.----------------------              
* --------- BLANK TASK & PAYTYPE = DELETE RECORD   ---------------              
                                                                                
CHKSCR20 DS    0H                                                               
         MVI   TPSTATUS,TPADD                                                   
         CLI   TKRTSKH+5,0                                                      
         BNE   CHKSCR40                                                         
         CLI   TKRPAYTH+5,0                                                     
         BNE   CHKSCR40                                                         
         MVI   TPSTATUS,TPDEL                                                   
         J     CHKSCRX                                                          
                                                                                
* KEY FIELDS TAKS/PAYTYPE CHANGED - NOT DELETE ACTION - NOT BLANKED OUT         
* IF ORIGINAL LINE ARE BLANKS THEN THIS IS A BRAND NEW ADD                      
* IF ORIGINAL LINE TASK/PAYTYPE ARE NOT BLANK - THEN THIS IS                    
* ADD NEW LINE AND DELETE OLD LINE ACTION                                       
CHKSCR40 L     RE,AOLDLINE                                                      
OLDLINE  USING TKRD,RE                                                          
         CLI   OLDLINE.TKRTSKH+5,0                                              
         BE    CHKSCR60                                                         
         CLI   OLDLINE.TKRPAYTH+5,0                                             
         BE    CHKSCR60                                                         
         MVI   TPSTATUS,TPADD+TPDEL                                             
         DROP  OLDLINE                                                          
* --------  VALIDATE DEFAULT RATE /ACCOUNT EQUIVALENT-------------              
CHKSCR60 DS    0C                                                               
                                                                                
* NEWEST CODE - ALWAYS VALIDATE TASK AND PATYPE TOO IF TASK OR                  
* PAYTYPE CHANGED ON SCREEN                                                     
         XC    TGTASK,TGTASK                                                    
         XC    TGPTYP,TGPTYP                                                    
         BRAS  RE,VALTASK                                                       
         BRAS  RE,VALPTYPE                                                      
*                                                                               
*&&DO                                                                           
         LA    R2,TKRDEFRH                                                      
****     CLI   TKRDEFRH+5,0                                                     
****     JE    ERMIS                                                            
         CLI   TKRDEFRH+5,0       EITHER DEFAULT RATE OR ACCOUNT                
         JNE   *+16               EQUIVALENT NEED TO BE FILLED IN               
         CLI   TKRACCTH+5,0                                                     
         JNE   *+8                                                              
         J     ERROE                                                            
* --------------------------- CHECK THIS LAST                                   
         CLI   TKRDEFRH+5,0       EITHER DEFAULT RATE OR ACCOUNT                
         BE    CHKSCR68                                                         
         LLC   R0,TKRDEFRH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TKRDEFR),(R0)                                    
         OC    DMCB+4(4),DMCB+4                                                 
         JZ    ERINV                                                            
         CLC   DMCB+4(4),=F'99999999'                                           
         JH    ERINV                                                            
         CLI   DMCB,X'FF'                                                       
         JE    ERINV                                                            
*&&                                                                             
* ------ FOR TPADD MODE CHECK IF RECORD ALREADY EXISTS ON FILE -----            
* ------ IF RECORD EXISTS TURN OFF DELETE BIT IF DELETE BIT ON -----            
* ------ IF RECORD EXISTS AND DELETE BIT OFF - THEN ERROR      -----            
                                                                                
***CHKSCR68 CLI   TPSTATUS,TPADD                                                
CHKSCR68 TM    TPSTATUS,TPADD                                                   
         JNO   CHKSCRX                                                          
                                                                                
CHKSCR70 BRAS  RE,VALTASK                                                       
         BRAS  RE,VALPTYPE                                                      
                                                                                
         LR    R2,R3                                                            
         GOTO1 RECVAL,DMCB,TLTRCDQ,(X'40',0),('TLTRSCDQ',0)                     
         OI    DMINBTS,X'08'               GET DELETE RECORD                    
         GOTO1 HIGH                                                             
         LA    RE,KEY                                                           
         USING TLDRD,RE                                                         
         CLC   KEY(TLTRPTYP+L'TLTRPTYP-TLTRKEY),KEYSAVE                         
         JNE   CHKSCRX                                                          
                                                                                
* --- ONLY TURN OFF DELETE BIT FOR MODE_VALIDATE ----------------               
* --- MODE_PROCIO DOEST NEED TO PROCESS THIS AGAIN LATER --------               
                                                                                
         CLI   CHKMODE,MODE_VALIDATE                                            
         JNE   CHKSCR90                                                         
                                                                                
         TM    TLDRSTAT,X'80'                                                   
         JNO   ERCMB                                                            
                                                                                
*  --- TURN OFF THE DELTE BITS IN OLD KEY AND OLD RECORD -----                  
         NI    TLDRSTAT,X'FF'-X'80'        TURN OFF DELETE BIT                  
         DROP  RE                                                               
                                                                                
         GOTO1 WRITE                                                            
         OI    DMINBTS,X'08'               GET DELETE RECORD                    
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         USING TLTRD,RE                                                         
         NI    TLTRSTAT,X'FF'-X'80'        TURN OFF DELETE BIT                  
         GOTO1 PUTREC                                                           
*                                                                               
CHKSCR90 CLI   TPSTATUS,TPADD                                                   
         BNE   *+8                                                              
         MVI   TPSTATUS,TPPUT                                                   
         CLI   TPSTATUS,TPADD+TPDEL                                             
         BNE   *+8                                                              
         MVI   TPSTATUS,TPPUT+TPDEL                                             
                                                                                
CHKSCRX  NI    DMINBTS,X'FF'-X'08'                                              
* VALIDATE DEFAULT RATE AND ACCOUNT EQUIV                                       
         TM    TPSTATUS,TPADD                                                   
         JO    *+12                                                             
         TM    TPSTATUS,TPPUT                                                   
         JNO   CHKSCRXX                                                         
         LA    R2,TKRDEFRH                                                      
****     CLI   TKRDEFRH+5,0                                                     
****     JE    ERMIS                                                            
         CLI   TKRDEFRH+5,0       EITHER DEFAULT RATE OR ACCOUNT                
         JNE   *+16               EQUIVALENT NEED TO BE FILLED IN               
         CLI   TKRACCTH+5,0                                                     
         JNE   *+8                                                              
         J     ERROE                                                            
         CLI   TKRDEFRH+5,0       EITHER DEFAULT RATE OR ACCOUNT                
         BE    CHKSCRXX                                                         
         LLC   R0,TKRDEFRH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TKRDEFR),(R0)                                    
         OC    DMCB+4(4),DMCB+4                                                 
         JZ    ERINV                                                            
         CLC   DMCB+4(4),=F'99999999'                                           
         JH    ERINV                                                            
         CLI   DMCB,X'FF'                                                       
         JE    ERINV                                                            
                                                                                
CHKSCRXX XIT1                                                                   
         DROP  R3                                                               
***********************************************************************         
* CHECK IF ALL PAY TYPE/TASK COMBINATIONS HAVE ARE UNIQUE                       
* R3 = CURRENT SCREEN LINE                                                      
***********************************************************************         
CHKDUPS  NTR1                                                                   
         LA    R2,TKRFRSTH                                                      
NEWLINE  USING TKRD,R3                                                          
CURRLINE USING TKRD,R2                                                          
CHKDUP10 LA    RE,TKRLSTH                                                       
         CR    R2,RE                                                            
         JNL   CHKDUPY                                                          
         CR    R2,R3              IF UP TO CURRENT SCREEN LINE - SKIP           
         BE    CHKDUP20                                                         
         CLI   NEWLINE.TKRPAYTH+5,0                                             
         BNE   CHKDUP16                                                         
         CLI   NEWLINE.TKRTSKH+5,0                                              
         BE    CHKDUP20                                                         
CHKDUP16 CLC   NEWLINE.TKRPAYT,CURRLINE.TKRPAYT                                 
         BNE   CHKDUP20                                                         
         CLC   NEWLINE.TKRTSK,CURRLINE.TKRTSK                                   
         BE    CHKDUPN                                                          
CHKDUP20 AHI   R2,TKRQ                                                          
         B     CHKDUP10                                                         
*                                                                               
CHKDUPY  CR    RB,RB                                                            
         J     XIT                                                              
CHKDUPN  CR    RB,RE                                                            
         J     ERCMB                                                            
         DROP  NEWLINE                                                          
         DROP  CURRLINE                                                         
*        LITERALS FOR VALIDATE RECORD ROUTINES                        *         
***********************************************************************         
*  PROCESS AIO                                                                  
***********************************************************************         
PROC_IO  NTR1                                                                   
         L     R3,ATKR                                                          
         USING TKRD,R3                                                          
         TM    TPSTATUS,TPPUT                                                   
         JNO   PROC_IO1                                                         
******   BRAS  RE,BLDKEY                                                        
         GOTO1 RECVAL,DMCB,TLTRCDQ,(X'40',0),('TLTRSCDQ',0)                     
         GOTO1 ADD_ELEM                                                         
         GOTO1 PUTREC                                                           
*                                                                               
****     CLI   TPSTATUS,TPADD                                                   
PROC_IO1 TM    TPSTATUS,TPADD                                                   
         JNO   PROC_IO2                                                         
         GOTO1 RECVAL,DMCB,TLTRCDQ,(X'40',0),('TLTRSCDQ',0)                     
                                                                                
         L     RE,AIO                                                           
         USING TLTRD,RE                                                         
         MVC   TLTRTASK,TGTASK                                                  
         MVC   TLTRPTYP,TGPTYP                                                  
         DROP  RE                                                               
         GOTO1 ADD_ELEM                                                         
         GOTO1 ADDREC                                                           
****     MVI   TPSTATUS,TPDEL                                                   
*****    J     PROC_IOX                                                         
                                                                                
                                                                                
                                                                                
****PROC_IO2 CLI   TPSTATUS,TPDEL                                               
PROC_IO2 TM    TPSTATUS,TPDEL                                                   
         BNO   PROC_IO4                                                         
                                                                                
* CALL BLDKEY TO REBUILD THE ORIGINAL RECORD FOR THE CURRENT LINE               
                                                                                
         BRAS  RE,BLDKEY                                                        
         L     RE,AIO                                                           
         USING TLTRD,RE                                                         
                                                                                
         OI    TLTRSTAT,X'80'                                                   
         DROP  RE                                                               
         GOTO1 PUTREC                                                           
                                                                                
         LA    RE,KEY                                                           
         USING TLDRD,RE                                                         
         OI    TLDRSTAT,X'80'      DELETE DIRECTORY RECORD                      
         DROP  RE                                                               
         GOTO1 WRITE                                                            
                                                                                
                                                                                
PROC_IO4 J     PROC_IOX                                                         
                                                                                
                                                                                
PROC_IOX XIT1                                                                   
         DROP  R3                                                               
***********************************************************************         
* BUILD THE TPROFILE KEY BASED ON THE ORGINAL DISPLAYED LINE NUMBER             
***********************************************************************         
BLDKEY   NTR1                                                                   
         LA    R2,TKRQ             THE SCREEN AS CAPTURED BY DISPLAY            
         ZIC   R3,LINENUM                                                       
         MR    R2,R2                                                            
         LA    R2,SVSCREEN                                                      
         AR    R2,R3                                                            
         USING TKRD,R2                                                          
* TGAGY,TGCLI SHOULD BE SET ALREADY..GET TGTASK,TGPTYP                          
         L     R0,ATKR                                                          
         ST    R2,ATKR                                                          
         BRAS  RE,VALTASK                                                       
         BRAS  RE,VALPTYPE                                                      
         ST    R0,ATKR                                                          
* CALL RECVAL TO BUILD THE TPROFILE KEY                                         
                                                                                
         GOTO1 RECVAL,DMCB,TLTRCDQ,(X'24',0),('TLTRSCDQ',0)                     
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         DROP  R2                                                               
***********************************************************************         
* ADD ELEMENTS                                                                  
***********************************************************************         
ADD_ELEM NTR1                                                                   
         L     R3,ATKR                                                          
         USING TKRD,R3                                                          
*****    LA    R2,TKRDEFRH                                                      
*****    CLI   5(R2),0                                                          
*****    JE    ERROE                                                            
                                                                                
ADD_EL50 MVC   AIO,AIO1                                                         
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 REMELEM                                                          
**                                                                              
         LA    R2,TKRDEFRH                                                      
         CLI   5(R2),0           IF DEFAULT RATE FIELD BLANKED OUT              
         JE    ADD_EL60          THEN JUST REMOVE ELEMENT                       
**                                                                              
         L     R4,AIO                                                           
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING TANUD,R4                                                         
         MVI   TANUEL,TANUELQ                                                   
         MVI   TANULEN,TANULNQ2                                                 
                                                                                
         LA    R2,TKRDEFRH                                                      
         CLI   TKRDEFRH+5,0                                                     
         JE    ERMIS                                                            
         LLC   R0,TKRDEFRH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TKRDEFR),(R0)                                    
         OC    DMCB+4(4),DMCB+4                                                 
         JZ    ERINV                                                            
         CLC   DMCB+4(4),=F'99999999'                                           
         JH    ERINV                                                            
         CLI   DMCB,X'FF'                                                       
         JE    ERINV                                                            
         MVI   TANUTYPE,TANUTDRT                                                
         MVC   TANUOVAM,DMCB+4                                                  
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
ADD_EL60 MVI   ELCODE,TAFNELQ                                                   
         L     R4,AIO                                                           
         GOTO1 REMELEM                                                          
         CLI   TKRACCTH+5,0                                                     
         BE    ADD_EL70                                                         
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',TKRACCTH),TAFNTACE                     
                                                                                
                                                                                
ADD_EL70 GOTO1 ACTVIN,DMCB,TKRLCHGH     UPDATE LAST CHANGED ELEMENT             
         DROP  R3                                                               
ADD_ELX  XIT1                                                                   
***********************************************************************         
* NEW SCREEN                                                                    
***********************************************************************         
NEWSCRN  NTR1                                                                   
         L     R3,ATKR                                                          
         USING TKRD,R3                                                          
*                                                                               
         LA    RE,SVSCREEN         CLEAR                                        
         LA    RF,SVSCRNX-SVSCREEN                                              
         XCEF                                                                   
         GOTO1 FLDVAL,DMCB,(1,TKRFRSTH),(X'80',TKRLSTH)                         
         BRAS  RE,DR                                                            
NEWSCRNX XIT1                                                                   
*        LITERALS FOR VALIDATE RECORD ROUTINES                        *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE RECORD                                *         
*        ON ENTRY ... AIO = A(PRIMARY COMMERCIAL RECORD)              *         
*                                                                               
* DEPENDING ON PAGE WE ARE UP TO DISPLAY FOR A CERTAIN READ HIGH                
*                                                                               
***********************************************************************         
DR       NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         BE    DR00                                                             
         GOTO1 FLDVAL,DMCB,(1,TKRFRSTH),(X'80',TKRLSTH)                         
DR00     CLI   RESUMEF,RESUME_EOS                                               
         BNE   DR01                                                             
         MVC   KEY,MYSVKEY                                                      
         B     DR02                                                             
*                                                                               
DR01     LA    R4,KEY                                                           
         USING TLTRD,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLTRCD,TLTRCDQ                                                   
         MVI   TLTRSCD,TLTRSCDQ                                                 
         MVC   TLTRAGY,TGAGY                                                    
         MVC   TLTRCLI,TGCLI                                                    
*  SET THE TASK KEY FIELD TO 1 TO FORCE A READ OF                               
*  TASK/PTYPE LEVEL TPROFILE RECORDS                                            
         MVI   TLTRTASK+L'TLTRTASK-1,1                                          
*                                                                               
DR02     LA    R2,TKRFRSTH                                                      
DISPLAY  USING TKRD,R2                                                          
         LA    R3,SVSCREEN                                                      
         LA    RE,SVSCREEN         CLEAR                                        
         LA    RF,SVSCRNX-SVSCREEN                                              
         XCEF                                                                   
SVSCRN   USING TKRD,R3                                                          
                                                                                
         GOTO1 HIGH                                                             
         J     DR20                                                             
DR10     GOTO1 SEQ                                                              
                                                                                
DR20     LA    R4,KEY                                                           
         CLC   TGAGY,TLTRAGY                                                    
         BNE   DRX                                                              
         CLC   TGCLI,TLTRCLI                                                    
         BNE   DRX                                                              
         TM    TLTRSTAT,X'80'       IGNORE DELETED KEYS                         
         BO    DR10                                                             
*                                                                               
         MVC   MYSVKEY,KEY                                                      
         LA    RE,SVSCRNX                                                       
         CR    R3,RE                                                            
         JNL   ERRMORE                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   DISPLAY.TKRPAYT,TLTRPTYP                                         
         MVC   DISPLAY.TKRTSK,TLTRTASK                                          
         MVI   DISPLAY.TKRPAYTH+5,L'TKRPAYT                                     
         MVI   DISPLAY.TKRTSKH+5,L'TKRTSK                                       
         MVC   SVSCRN.TKRPAYT,TLTRPTYP                                          
         MVC   SVSCRN.TKRTSK,TLTRTASK                                           
         MVC   SVSCRN.TKRPAYTH,DISPLAY.TKRPAYTH                                 
         MVC   SVSCRN.TKRTSKH,DISPLAY.TKRTSKH                                   
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,DISPLAY.TKRACCTH,TAFNTACE                   
         GOTO1 CHAROUT,DMCB,TANUELQ,DISPLAY.TKRDEFRH,TANUTDRT                   
         OC    DISPLAY.TKRDEFR,DISPLAY.TKRDEFR                                  
         BZ    DR40                                                             
         MVC   DISPLAY.TKRDEFRH+5(1),DISPLAY.TKRDEFRH+0                         
         EDIT  (B4,DISPLAY.TKRDEFR),DISPLAY.TKRDEFR,2                           
DR40     MVC   SVSCRN.TKRACCTH,DISPLAY.TKRACCTH                                 
         MVC   SVSCRN.TKRACCT,DISPLAY.TKRACCT                                   
         MVC   SVSCRN.TKRDEFRH,DISPLAY.TKRDEFRH                                 
         MVC   SVSCRN.TKRDEFR,DISPLAY.TKRDEFR                                   
         CLI   NEWSCRNF,C'Y'                                                    
         BE    *+12                                                             
         AHI   R2,TKRQ                                                          
         AHI   R3,TKRQ                                                          
*                                                                               
         J     DR10                                                             
*                                                                               
                                                                                
*                                                                               
DRX      GOTO1 ACTVOUT,DMCB,TKRLCHGH     LAST CHANGED ACTIVITY                  
         MVI   RESUMEF,0                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO LIST RECORDS                                      *         
***********************************************************************         
                                                                                
LR       NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TKLLSTH                                                       
         ST    R2,ATHISLST                                                      
         LA    R2,LISTAR                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         LA    R2,P                R2=A(DISPLAY LINE)                           
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
                                                                                
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,16                                                        
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         XC    TGCLI,TGCLI                                                      
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         OC    COUNTER,COUNTER     IF ANYTHING REPORTED                         
         BZ    LRX                                                              
         BRAS  RE,PRNTIT           SKIP A LINE                                  
         XC    COUNTER,COUNTER                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         GOTO1 FLDVAL,DMCB,(X'01',TKLLSTH),(X'80',TKLLAST)                      
         B     LRX                                                              
LRX      J     XIT                                                              
***********************************************************************         
*        ROUTINE TO PROCESS RECORDS FROM SYSIO                        *         
***********************************************************************         
                                                                                
LRHOOK   NTR1                                                                   
         USING LISTD,R2                                                         
         CLI   TIMODE,PROCREC                                                   
         JNE   XIT                                                              
*                                                                               
         MVC   AIO,TIAREC                                                       
         L     R4,AIO                                                           
         USING TLTRD,R4                                                         
         CLI   TLTRCD,TLTRCDQ                                                   
         JNE   YES                                                              
         CLI   TLTRSCD,TLTRSCDQ                                                 
         JNE   YES                                                              
**       MVC   LAGYC,TLTRAGY                                                    
**       MVC   LCLIC,TLTRCLI                                                    
         OC    TLTRTASK,TLTRTASK                                                
         JNZ   YES                                                              
         OC    TLTRPTYP,TLTRPTYP                                                
         JNZ   YES                                                              
         TM    TLTRSTAT,X'80'                                                   
         JO    YES                                                              
                                                                                
         MVC   LAGYC,TLTRAGY                                                    
         MVC   LCLIC,TLTRCLI                                                    
         MVI   DUMFLDH,L'DUMFLDH                                                
         MVC   DUMFLDH+8(L'TLTRAGY),SPACES                                      
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'AE',LAGYC),DUMFLDH                        
         MVC   LAGYN,DUMFLDH+8                                                  
*                                                                               
         MVI   DUMFLDH,L'DUMFLDH                                                
         MVC   DUMFLDH+8(L'TLTRCLI),SPACES                                      
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'AE',LCLIC),DUMFLDH                        
         MVC   LCLIN,DUMFLDH+8                                                  
LRH20    MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
                                                                                
DISP40   CLI   MODE,PRINTREP       IF DISPLAYING TO SCREEN                      
         BE    DISP50                                                           
LRH30    MVC   DMDSKADD,TIDSKADD                                                
         GOTO1 LISTMON                                                          
         B     LRHX                                                             
DISP50   GOTO1 SPOOL,DMCB,(R8)                                                  
         LH    RE,COUNTER          DISPLAY LINE TO REPORT                       
         AHI   RE,1                                                             
         STH   RE,COUNTER                                                       
LRHX     J     YES                                                              
         DROP  R2                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DELETE ALL THE UNDERLYING TPROFILE                *         
*        RECORDS FOR A GIVEN AGENCY, CLIENT                           *         
***********************************************************************         
DELALL   NTR1  BASE=*,LABEL=*                                                   
                                                                                
*                                                                               
         LA    R4,KEY                                                           
         USING TLTRD,R4                                                         
         MVI   RDUPDATE,C'Y'         SET TO READ FOR UPDATE                     
         NI    GENSTAT1,ALL-RDUPAPPL                                            
         GOTO1 HIGH                                                             
         J     DELALL20                                                         
DELALL10 GOTO1 SEQ                                                              
                                                                                
DELALL20 LA    R4,KEY                                                           
         CLC   TGAGY,TLTRAGY                                                    
         BNE   DELALLX                                                          
         CLC   TGCLI,TLTRCLI                                                    
         BNE   DELALLX                                                          
         USING TLDRD,R4                                                         
***      TM    TLTRSTAT,X'80'       IGNORE DELETED KEYS                         
         TM    TLDRSTAT,X'80'       IGNORE DELETED KEYS                         
         BO    DELALL10                                                         
         DROP  R4                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         USING TLTRD,RE                                                         
                                                                                
         OI    TLTRSTAT,X'80'                                                   
         DROP  RE                                                               
         GOTO1 PUTREC                                                           
                                                                                
         LA    RE,KEY                                                           
         USING TLDRD,RE                                                         
         OI    TLDRSTAT,X'80'      DELETE DIRECTORY RECORD                      
         DROP  RE                                                               
         GOTO1 WRITE                                                            
*                                                                               
         J     DELALL10                                                         
*                                                                               
                                                                                
*                                                                               
***DELALLX  J     DR                                                            
DELALLX  J     XIT                                                              
                                                                                
***********************************************************************         
*        LITERALS FOR DISPLAY RECORD ROUTINES                         *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        LOCAL STORAGE                                               *          
**********************************************************************          
         DS    D                                                                
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE TAUSEPT                                                        
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFFD                                                       
       ++INCLUDE DDGENTWA                                                       
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
                                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR6FD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR70D                                                       
         ORG   TKLWORK                                                          
SVSCREEN DS    (TKRLINES)CL(TKRQ)                                               
SVSCRNX  DS    0X                                                               
RESUMEF  DS    X                                                                
RESUME_EOS EQU X'80'                                                            
MYSVKEY  DS    XL(L'KEY)                                                        
SVACTNM  DS    X                                                                
                                                                                
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**********************************************************************          
*  TEMPORARY STORAGE                                                            
**********************************************************************          
TMPD     DSECT                                                                  
*                                                                               
SVKEY    DS    XL48                BACK UP OF KEY                               
DUMFLDH  DS    XL50                                                             
DUMFLD2H DS    XL50                                                             
ATKR     DS    A                                                                
TPSTATUS DS    X                                                                
TPADD    EQU   X'80'                                                            
TPDEL    EQU   X'40'                                                            
TPPUT    EQU   X'20'                                                            
TPPUTOLD EQU   X'10'                                                            
CHKMODE  DS    X                                                                
MODE_VALIDATE EQU X'80'                                                         
MODE_PROCIO   EQU X'40'                                                         
LINENUM  DS    X                                                                
MYBASERB DS    A                                                                
AUPDTSKS DS    A                                                                
AUPDPTYS DS    A                                                                
AOLDLINE DS    A                                                                
NEWSCRNF DS    C                                                                
COUNTER  DS    H                   RECORD COUNTER                               
                                                                                
UPDTSKS  DS    XL(16*L'TLTKTASK+1)                                              
UPDTSKSQ EQU   *-UPDTSKS                                                        
UPDPTYS  DS    XL(16*L'TLPMPTYP+1)                                              
UPDPTYSQ EQU   *-UPDPTYS                                                        
TMPLNQ   EQU   *-TMPD                                                           
                                                                                
**********************************************************************          
*        DSECT TO COVER EACH TPROFILE ENTRY                          *          
**********************************************************************          
                                                                                
TKRD     DSECT                                                                  
TKRTSKH  DS     XL8                                                             
TKRTSK   DS    CL6                TASK                                          
TKRPAYTH DS    XL8                                                              
TKRPAYT  DS    CL6                PAY TYPE                                      
TKRKEYQ  EQU   *-TKRD                                                           
TKRDEFRH DS    XL8                                                              
TKRDEFR  DS    CL10               DEFAULT RATE  MAX=999999.99                   
TKRACCTH DS    XL8                                                              
TKRACCT  DS    CL20               ACCOUNT                                       
TKRQ     EQU   *-TKRD                                                           
TKRLINES EQU   ((TKRLST+20)-TKRFRSTH)/TKRQ                                      
**********************************************************************          
*        DSECT TO COVER LIST LINE                                    *          
**********************************************************************          
                                                                                
LISTD    DSECT                                                                  
LAGYC    DS    CL6                 AGENCY CODE                                  
         DS    CL1                                                              
LAGYN    DS    CL29                AGENCY NAME                                  
         DS    CL1                                                              
LCLIC    DS    CL6                 CLIENT CODE                                  
         DS    CL1                                                              
LCLIN    DS    CL36                CLIENT NAME                                  
         DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TAGEN59   11/30/12'                                      
         END                                                                    
