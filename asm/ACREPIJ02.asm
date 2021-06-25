*          DATA SET ACREPIJ02  AT LEVEL 032 AS OF 10/14/15                      
*PHASE ACIJ02A                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE GETLOGO                                                                
*INCLUDE PERVERT                                                                
*INCLUDE BMONVAL                                                                
         TITLE 'REQUESTABLE DAILY JOURNAL CONTROLLER'                           
ACIJ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**INJO**,R9,R8,R7,R6,R5                                        
         USING ACWORKD,RA          GLOBAL STORAGE                               
         LA    RC,SPACEND                                                       
         USING WORKD,RC            LOCAL STORAGE                                
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SETINIT                                                          
         CLI   MODE,PROCOPTS                                                    
         BE    SREQ                                                             
         CLI   MODE,REQFRST                                                     
         BE    BLDSORT                                                          
         CLI   MODE,RUNLAST                                                     
         BE    EOREPORT                                                         
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE VALUES                                                   *         
***********************************************************************         
         SPACE 1                                                                
SETINIT  MVI   JPROG,JPIJ                                                       
         XC    PRMODE,PRMODE                                                    
         L     RF,ADMASTC                                                       
         CLI   MCTSTRUN-MASTD(RF),X'FF'                                         
         BNE   *+8                                                              
         OI    PRMODE,PRMTEST      SET TEST=YES                                 
         MVC   DESTNUM,MCDESTID-MASTD(RF)                                       
         MVI   FCRQOPT,FCRQOPTQ    INDICATE MODE OF QOPTS REQUIRED              
*                                                                               
         OI    PRMODE,PRMACT       PRINT START LOGOS NOW : ACTIVE MODE          
         MVC   HALF,ORIGINUM       (IJ ONLY REPORTS ON SINGLE ID)               
         OC    DESTNUM,DESTNUM                                                  
         BZ    *+10                                                             
         MVC   HALF,DESTNUM                                                     
         MVI   FORCEHED,YES        START NEW PAGE                               
         TM    PRMODE,PRMNOLOG     DON'T PRINT LOGOS IF NOLOGOS                 
         BO    *+12                                                             
         BAS   RE,LOGOUP           PRINT START LOGOS                            
         OI    PRMODE,PRMLOGOS                                                  
*                                                                               
         LA    RF,IOEXEC                                                        
         ST    RF,AIO                                                           
         LA    R0,HOOK                                                          
         ST    R0,HEADHOOK                                                      
*                                                                               
         L     RF,ADCOMFAC                                                      
         MVC   VBLDCUR,CBLDCUR-COMFACSD(RF)                                     
*                                                                               
         L     RF,=A(DDIN1)                                                     
         GOTO1 ADDICTAT,DMCB,C'LU  ',(RF),DDOUT1                                
         L     RF,=A(DDIN2)                                                     
         GOTO1 ADDICTAT,DMCB,C'LL  ',(RF),DDOUT2                                
         MVI   RCFLAG1,RCFREPLC    LOWER CASE SPECS                             
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAYC)                                
         GOTO1 (RF),(R1),(4,RCDATE),(0,TODAYE)                                  
         MVC   TODAYCC,EFFALL                                                   
         XC    TODAYCC,TODAYC      COMPLEMENT COMPRESSED DATE                   
*                                                                               
         L     RE,ANBUFF           CLEAR NAME TABLE                             
         L     RF,=A(NSIZE)                                                     
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD SUPPLEMENTARY REQUEST REPORT                                  *         
***********************************************************************         
         SPACE 1                                                                
SREQ     LA    R4,SRQTAB           R4=A(REQUEST REPORT TABLE)                   
         USING SRQTABD,R4                                                       
         XR    R3,R3                                                            
*                                                                               
SREQ2    MVC   P,SPACES            CLEAR PRINT LINE                             
         XR    R2,R2                                                            
         ICM   R2,3,SRQDISP        R2=DISPLACEMENT TO REQUEST FIELD             
         BZ    SREQX                                                            
         LA    R2,ACWORKD(R2)                                                   
         IC    R3,SRQLEN           R3=L'REQUEST FIELD                           
         EX    R3,*+8              TEST FIELD SET                               
         BE    SREQ4                                                            
         CLC   0(0,R2),SPACES                                                   
*                                                                               
         XC    WORK,WORK           CALL DICTATE TO SET LOWER CASE               
         MVC   WORK,SRQDD                                                       
         GOTO1 ADDICTAT,DMCB,C'SL  ',WORK                                       
         MVC   P+24(L'SRQDD),WORK  PRINT DESCRIPTION OF THE FIELD               
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+45(0),0(R2)       FOLLOWED BY CONTENTS OF THE FIELD            
*                                                                               
         GOTO1 PRINT,DMCB,P,=C'BL02'                                            
*                                                                               
SREQ4    LA    R4,SRQQ(R4)                                                      
         B     SREQ2                                                            
*                                                                               
SREQX    MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD SORT RECORDS FOR ANALYSIS                                     *         
***********************************************************************         
         SPACE 1                                                                
BLDSORT  DS    0H                                                               
         XC    BLODAT,BLODAT                                                    
         MVC   BHIDAT,=X'FFFF'                                                  
         CLC   QSTART,SPACES                                                    
         BNH   BLDS00A                                                          
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BLODAT)                                
BLDS00A  CLC   QEND,SPACES                                                      
         BNH   BLDS00B                                                          
         GOTO1 DATCON,DMCB,(0,QEND),(2,BHIDAT)                                  
BLDS00B  XC    ACIJSMOS,ACIJSMOS                                                
         MVC   ACIJEMOS,=X'FFFF'                                                
         CLC   QMOSSTRT,SPACES                                                  
         BNH   BLDS01A                                                          
         MVC   WORK(4),QMOSSTRT                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ACIJSMOS,WORK+6                                                  
*                                                                               
BLDS01A  CLC   QMOSEND,SPACES                                                   
         BNH   BLDS01B                                                          
         MVC   WORK(4),QMOSEND                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ACIJEMOS,WORK+6                                                  
*                                                                               
BLDS01B  XC    WORD,WORD                                                        
         MVC   SECALPHA,ALPHAID    DEFAULT SECURITY AGENCY ALPHA ID             
         BAS   RE,SECAID           SET SECURITY AGENCY ALPHA ID                 
*                                                                               
         XC    FILOPT,FILOPT       SET FILTER OPTION                            
         CLI   QOPT8,YES           TREAT SAVED/CLOSED,UNAPRD AS CLOSED          
         BNE   *+12                                                             
         OI    FILOPT,FOCLSD                                                    
         B     BLDS02                                                           
         CLI   QOPT8,ONLY          ONLY THESE BATCHES                           
         BNE   BLDS02                                                           
         OI    FILOPT,FOEXCL+FOCLSD                                             
*                                                                               
BLDS02   CLC   QAPPL(8),SPACES     TEST FILTERING BY PERSON                     
         BE    BLDS06                                                           
         XC    IOKEY,IOKEY         BUILD KEY FOR PERSON RECORD READ             
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECALPHA    USE SECURITY AGENCY ALPHA ID                 
         MVC   SAPEPID,QAPPL                                                    
         GOTO1 AIO,IOHI+IOCLFILE+IO4                                            
         BNE   XIT                 GET OUT NOW IF PERSON INVALID                
         L     R2,AIOBUFF                                                       
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),IOKEY                                   
         BNE   XIT                                                              
*                                                                               
         LA    R1,SAPEDATA                                                      
         XR    R0,R0                                                            
BLDS04   CLI   0(R1),0                                                          
         BE    XIT                                                              
         CLI   0(R1),SAPWDELQ      PERSON PASSWORD ELEMENT                      
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BLDS04                                                           
         MVC   WORD(2),SAPWDNUM-SAPWDD(R1)                                      
         DROP  R2                                                               
*                                                                               
BLDS06   CLC   QCOMMENT+1(2),SPACES  TEST FILTERING ON BATCH TYPE               
         BE    BLDS08                                                           
         CLI   QCOMMENT+2,C' '                                                  
         BNE   *+14                                                             
         MVC   QCOMMENT+2(1),QCOMMENT+1                                         
         MVI   QCOMMENT+1,C' '                                                  
         PACK  DUB,QCOMMENT+1(2)                                                
         CVB   R1,DUB                                                           
         STC   R1,WORD+2                                                        
*                                                                               
BLDS08   GOTO1 ADSORTER,DMCB,SORTCARD,SORTRECD  INITALISE SORT                  
*                                                                               
         XC    BATCPY,BATCPY                                                    
         XC    BATUID,BATUID                                                    
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING TBARECD,R4                                                       
         MVI   TBAPTYP,TBAPTYPQ    READ FOR BATCH HEADER PASSIVE RECORD         
         MVC   TBAPCPY,QCOMPANY                                                 
         MVC   TBAPUSER,ORIGINUM   READ ONLY FOR ORIGIN-ID                      
BLDS10   LA    R1,IOHID+IOACCDIR+IO1                                            
         B     *+8                                                              
BLDS12   LA    R1,IOSEQD+IOACCDIR+IO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(TBAPEFDT-TBAPTYP),IOKEYSAV                                 
         BNE   SELECT              EXIT IF KEY CHANGES                          
         CLC   TBAPBMOS,ACIJSMOS   CHECK MOS WITHIN RANGE                       
         BL    BLDS12                                                           
         CLC   TBAPBMOS,ACIJEMOS                                                
         BH    BLDS12                                                           
*                                                                               
BLDS12H  OC    IODA,IODA           SKIP IF DISK ADDRESS MISSING                 
         BZ    BLDS12                                                           
         CLC   QCOMMENT(1),SPACES  TEST BATCH GROUP REQUIRED                    
         BE    *+14                                                             
         CLC   TBAPGRUP,QCOMMENT                                                
         BNE   BLDS12                                                           
         OC    WORD+2(1),WORD+2    TEST BATCH TYPE REQUIRED                     
         BZ    *+14                                                             
         CLC   TBAPBTYP,WORD+2                                                  
         BNE   BLDS12                                                           
         CLC   QAPPL+8(4),SPACES   TEST BATCH REFERENCE REQUIRED                
         BE    *+14                                                             
         CLC   TBAPBREF,QAPPL+8                                                 
         BNE   BLDS12                                                           
         OC    WORD(2),WORD        TEST PERSON REQUIRED                         
         BZ    *+14                                                             
         CLC   TBAPBCHR,WORD                                                    
         BNE   BLDS12                                                           
         GOTO1 AIO,IOGETD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BATPKEY,IOKEY       SAVE BATCH HEADER PASSIVE KEY                
         LA    R4,BATAKEY          BUILD ACTIVE KEY FROM DATA RECORD            
         L     RF,AIOBUFF                                                       
         MVC   TBAKEY,0(RF)                                                     
         MVC   TBAKSTA,TBARSTA-TBARECD(RF)                                      
         MVC   TBAKDA,IODA                                                      
*                                                                               
         CLC   BATCPY,TBAKCPY      TEST CHANGE OF COMPANY                       
         BE    BLDS14                                                           
         BAS   RE,SPECPY           SET SPECIFIC COMPANY REQUIREMENTS            
         XC    WORK,WORK           SET JX PROFILES FOR COMPANY                  
         MVC   WORK(4),=C'A0JX'                                                 
         MVC   WORK+12(2),ALPHAID                                               
         GOTO1 GETPROF,DMCB,WORK,AGYPROFS,DATAMGR                               
         L     RE,APBUFF           CLEAR REPORT PROFILES TABLE                  
         ST    RE,TABLE            SET START OF TABLE                           
         L     RF,=A(PSIZE)                                                     
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ST    RE,TABLEX           SET END OF TABLE                             
         MVC   BATCPY,TBAKCPY      SAVE COMPANY CODE                            
*                                                                               
BLDS14   CLC   BATUID,TBAKUSER     TEST CHANGE OF USER-ID                       
         BE    BLDS16                                                           
         MVC   BATUID,TBAKUSER                                                  
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         XC    MCORIGID,MCORIGID   STOP STANDARD PROFILES DEFAULT               
         MVC   DEFLTID,ALPHAID     SET AGENCY PROFILES FOR DEFAULT              
         BAS   RE,USERADD          ADD USER-ID NAME & PROFS TO BUFFER           
         L     RF,ADMASTC                                                       
         MVC   MCORIGID,ORIGINUM   RESTORE STANDARD PROFILES DEFAULT            
         DROP  RF                                                               
         XC    USRPROFS,USRPROFS   SET JX PROFILES FOR USER-ID                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'A0JX'                                                 
         MVC   WORK+12(2),TBAKUSER                                              
         GOTO1 GETPROF,DMCB,WORK,USRPROFS,DATAMGR                               
         OC    USRPROFS,USRPROFS                                                
         BNZ   *+10                                                             
         MVC   USRPROFS,AGYPROFS   USE AGENCY PROFILES IF NONE FOR USER         
*                                                                               
BLDS16   BAS   RE,FILBAT           FILTER BATCH HEADER RECORD                   
         BNE   BLDS22              NEXT RECORD                                  
*                                                                               
         XC    SREC1,SREC1         BUILD SORT RECORD                            
         MVC   SKAGY,ALPHAID       AGENCY ALPHA-ID                              
         MVC   SKSUID,BATUID       SOURCE USER-ID                               
         MVI   SKGRP,SKGPRO        BATCH GROUP                                  
         CLI   TBAKGRUP,TBAGPRDQ                                                
         BE    *+8                                                              
         MVI   SKGRP,SKGGEN                                                     
         MVC   SKTYP,TBAKBTYP      BATCH TYPE                                   
         MVC   SKMOS,TBAKBMOS      BATCH MONTH                                  
         MVC   SKDAT,TBAHKADT      INPUT DATE                                   
         MVC   SKREF,TBAKBREF      REFERENCE                                    
         MVC   SDSEQN,TBAKSEQN     SEQUENCE NUMBER                              
         MVI   SKSRC,DJSRCACC      DEFAULT SOURCE - ACCOUNT FILE                
*                                                                               
         LA    RF,DJSRCTAB         CHECK ALTERNATIVE SOURCES                    
BLDS18   CLI   0(RF),0                                                          
         BE    BLDS20                                                           
         CLC   0(1,RF),TBAKBTYP                                                 
         BE    *+12                                                             
         LA    RF,DJSRCLEN(RF)                                                  
         B     BLDS18                                                           
         MVC   SKSRC,1(RF)                                                      
*                                                                               
BLDS20   BAS   RE,SUBSORT                                                       
*                                                                               
BLDS22   LA    R4,IOKEY                                                         
         MVC   IOKEY,BATPKEY       RESTORE BATCH HEADER PASSIVE KEY             
*        MVC   TBAPTSEQ,EFFALL     FORCE NEXT BATCH HEADER RECORD               
         NI    FILOPT,X'FF'-FOUPDT CLEAR ANY BATCH SPECIFIC OPTIONS             
         LA    R1,IOHID+IOACCDIR+IO1                                            
         GOTO1 AIO                 RE-READ RECORD AND BRANCH TO SEQ             
         BE    *+6                 OLD METHOD DOESN'T WORK NOW THAT             
         DC    H'0'                DATE IS IN TBAPTSEQ                          
         B     BLDS12                                                           
*        B     BLDS10                                                           
         EJECT                                                                  
***********************************************************************         
* FILTER BATCH HEADER RECORDS                                         *         
*                                                                     *         
* EXIT - CC INDICATES PROCEEDING ACTION                               *         
***********************************************************************         
         SPACE 1                                                                
FILBAT   ST    RE,RETURN1                                                       
*                                                                               
         MVI   FILVAL,0            RESET FILTER VALUES                          
         MVI   FILIND,0                                                         
         MVI   FBATDAYS,0                                                       
         SR    RF,RF               TEST WITHIN DATE RANGE                       
         ICM   RF,3,TBAHKUDT       USE UPDATED DATE IF BEFORE TODAY             
         MVI   FLTDATE,FDUPD                                                    
         LTR   RF,RF                                                            
         BZ    *+14                USE EFFECTIVE DATE IF NOT UPDATED            
         CLC   TBAHKEDT,TODAYC                                                  
         BL    *+12                                                             
         ICM   RF,3,TBAHKEDT       USE EFFECTIVE DATE IF TODAY OR LATER         
         MVI   FLTDATE,FDEFF                                                    
         CLM   RF,3,BLODAT         NO UPD/EFF DATES BEFORE RANGE                
         BL    FILBATNQ                                                         
         CLM   RF,3,BHIDAT                                                      
         BNH   FLTB01                                                           
         TM    FLTDATE,FDUPD       ONLY EFF DATES AFTER RANGE                   
         BO    FILBATNQ                                                         
         GOTO1 DATCON,DMCB,(2,TBAHKEDT),(0,TEMP)                                
         B     FLTB20                                                           
*                                                                               
FLTB01   GOTO1 DATCON,DMCB,(2,TBAHKADT),(0,TEMP)                                
         GOTO1 VPERVERT,DMCB,TEMP,TODAYE   CALC TODAY-BATCH ADDED DATE          
         MVC   HALF,8(R1)          SAVE RESULT                                  
*                                                                               
         TM    TBAKHSTA,TBAHSIAD                                                
         BZ    FLTB02                                                           
         MVI   FILVAL,FVINSU       *INSTANT UPDATE*                             
         B     FILBATEQ                                                         
*                                                                               
FLTB02   TM    TBAKHSTA,TBAHSDEL                                                
         BZ    FLTB04                                                           
         MVI   FILVAL,FVDELD       *DELETED IN POSTMAN* OR                      
         B     FILBATEQ            *PURGED/ORDERS ON BATCH*                     
*                                                                               
FLTB04   XR    R1,R1                                                            
         TM    TBAKHSTA,TBAHSIIP   OPEN                                         
         BZ    FLTB06                                                           
         MVI   FILVAL,FVOPND       *BATCH FOR DELETION*                         
         ICM   R1,1,PRFBOPND       OPEN DAYS PROFILE SET?                       
         BZ    FILBATEQ            NO                                           
         LA    R1,1(R1)                                                         
         SH    R1,HALF             PROFILE DAYS EXCEEDED?                       
         BNP   FILBATEQ            YES                                          
         MVI   FILVAL,FVOPNW       *BATCH FOR WARNING*                          
         STC   R1,FBATDAYS                                                      
         B     FILBATEQ                                                         
*                                                                               
FLTB06   TM    TBAKHSTA,TBAHSSAV   SAVED                                        
         BZ    FLTB08                                                           
         MVI   FILVAL,FVSAVD       *BATCH FOR DELETION*                         
         ICM   R1,1,PRFBSAVD       SAVED DAYS PROFILE SET?                      
         BZ    FILBATEQ            NO                                           
         LA    R1,1(R1)                                                         
         SH    R1,HALF             SAVED DAYS EXCEEDED?                         
         BNP   FILBATEQ            YES                                          
         MVI   FILVAL,FVSAVW       *BATCH FOR WARNING*                          
         STC   R1,FBATDAYS         SET DAYS REMAINING                           
         B     FILBATEQ                                                         
*                                                                               
FLTB08   TM    TBAKHSTA,TBAHSUPD   TEST UPDATED TO FILE                         
         BZ    FLTB10                                                           
         MVI   FILVAL,FVOVNU       SET *OVERNIGHT UPDATE*                       
         TM    TBAHKIND,TBAHIUOF   TEST BATCH UPDATED OFFLINE                   
         BO    FILBATEQ                                                         
         MVI   FILVAL,FVONLU       SET *ONLINE UPDATE*                          
         B     FILBATEQ                                                         
*                                                                               
FLTB10   OC    TBAHKEDT,TBAHKEDT   TEST EFFECTIVE DATE SET                      
         BZ    FLTB12                                                           
         CLC   TBAHKADT,TBAHKEDT   TEST EFFECTIVE DATE=ADDED DATE               
         BE    FLTB12                                                           
         LA    R1,TBAKBMOS                                                      
         BAS   RE,TSTBMO           TEST MOA LOCKED                              
         BE    *+8                                                              
         OI    FILIND,FILIMOAL                                                  
         TM    CMPAPRV,APREFF      TEST EFFDT BATCH APPROVAL REQ'D              
         BZ    FLTB18                                                           
         TM    TBAKHSTA,TBAHSAPR   TEST BATCH IS APPROVED                       
         BNZ   *+8                                                              
         MVI   FILIND,FILIUNAP                                                  
         B     FLTB18                                                           
*                                                                               
FLTB12   TM    CMPAPRV,APRREG      TEST REGULAR BATCH APPROVAL REQ'D            
         BZ    FLTB14                                                           
         TM    TBAKHSTA,TBAHSAPR   TEST BATCH IS APPROVED                       
         BNZ   *+8                                                              
         OI    FILIND,FILIUNAP                                                  
FLTB14   CLI   FILIND,0            TEST ANY ERRORS                              
         BNE   FLTB16                                                           
         MVI   FILVAL,FVOVNU       *OVERNIGHT UPDATE*                           
         B     FILBATEQ                                                         
*                                                                               
FLTB16   MVI   FILVAL,FVDFTD       *REGULAR BATCH FOR DELETION*                 
         XR    R1,R1                                                            
         ICM   R1,1,PRFBCLSD       CLOSED,UNAPPROVED PROFILE SET?               
         BZ    FILBATEQ            NO                                           
         LA    R1,1(R1)                                                         
         SH    R1,HALF             PROFILE DAYS EXCEEDED?                       
         BNP   FILBATEQ            YES                                          
         MVI   FILVAL,FVDFTW       *REGULAR BATCH FOR WARNING*                  
         STC   R1,FBATDAYS         SET DAYS REMAINING                           
         B     FILBATEQ                                                         
*                                                                               
FLTB18   GOTO1 DATCON,DMCB,(2,TBAHKEDT),(0,TEMP)                                
         CLC   TBAHKEDT,TODAYC                                                  
         BH    FLTB20                                                           
         MVI   FILVAL,FVDFTU       *EFFECTIVE BATCH GOING LIVE*                 
         CLI   FILIND,0            TEST ANY ERRORS                              
         BE    FILBATEQ                                                         
         MVI   FILVAL,FVDFTD       *EFFECTIVE BATCH FOR DELETION*               
         CLI   PRFBXCPD,0          EXCEPTION DAYS PROFILE SET?                  
         BE    FILBATEQ            NO                                           
         GOTO1 VPERVERT,DMCB,TEMP,TODAYE   CALC TODAY-EFFECTIVE DATE            
         MVC   HALF,8(R1)                                                       
         XR    R1,R1                                                            
         IC    R1,PRFBXCPD                                                      
         LA    R1,1(R1)                                                         
         SH    R1,HALF             PROFILE DAYS EXCEEDED?                       
         BNP   FILBATEQ            YES                                          
         MVI   FILVAL,FVDFTW       *EFFECTIVE BATCH FOR WARNING*                
         STC   R1,FBATDAYS         SET DAYS REMAINING                           
         B     FILBATEQ                                                         
*                                                                               
FLTB20   CLC   QEND,SPACES                                                      
         BNH   FILBATEQ                                                         
         GOTO1 VPERVERT,DMCB,QEND,TEMP  CALC EFFECTIVE DATE-END DATE            
         MVC   HALF,8(R1)                                                       
         LA    R1,1                                                             
         CH    R1,HALF             IS EFFECTIVE DATE WITHIN DATE RANGE?         
         BL    *+8                                                              
         OI    FILOPT,FOUPDT       YES - TREAT AS UPDATED                       
         IC    R1,PRFBDUED         TEST PROFILE 'BATCH DUE IN N DAYS'           
         CH    R1,HALF                                                          
         BL    FILBATNQ            DATE OUT OF RANGE                            
         TM    FILIND,FILIMOAL     WARN IF MOA LOCKED                           
         BO    FLTB22                                                           
         MVI   FILVAL,FVDUEN       *DUE LIVE SOON - NO APPROVAL REQD*           
         TM    CMPAPRV,APREFF                                                   
         BZ    FILBATEQ                                                         
         MVI   FILVAL,FVDUEA       *DUE LIVE SOON - APPROVED*                   
         TM    TBAKHSTA,TBAHSAPR                                                
         BO    FILBATEQ                                                         
FLTB22   MVI   FILVAL,FVDUEU       *DUE LIVE SOON - UNAPPROVED (WARN)*          
         MVC   FBATDAYS,PRFBXCPD   SET DAYS REMAINING (EXCEPTION LIST)          
         B     FILBATEQ                                                         
*                                                                               
FILBATEQ CR    RE,RE               CC EQUAL TO PROCESS                          
         B     *+6                                                              
FILBATNQ LTR   RE,RE               CC NOT EQUAL FOR NEXT RECORD                 
         L     RE,RETURN1                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL: TEST BATCH MOA IS OPEN FOR POSTING                         *         
* NTRY: R1=A(PWOS MOA)                                                *         
* EXIT: FULL(2)= PWOS YYMM, FULL+2(2)=CHARACTER YM                    *         
***********************************************************************         
         SPACE 1                                                                
TSTBMO   ST    RE,RETURN2                                                       
         XC    FULL,FULL                                                        
         MVC   FULL(L'TBAKBMOS),0(R1)                                           
         MVI   FULL+L'TBAKBMOS,1                                                
         MVC   DUB,SPACES                                                       
         GOTO1 DATCON,DMCB,(1,FULL),(9,DUB)                                     
         LA    R3,WORK                                                          
         USING BMONVALD,R3                                                      
         GOTO1 VBMONVAL,DMCB,(8,DUB),(TBAKBTYP,ADCOMFAC),              X        
               (RCLANG,BMONVALD),(TBAKCPY,0)                                    
         CLI   BMOERR,BMOEOKQ      TEST ANY ERROR                               
         BE    TSTB2                                                            
         CLI   BMOERR,BMOELOKQ     TEST LOCKED MONTH IS THE ONLY ERROR          
         BNE   TSTBNQ                                                           
         CLI   TBAKBTYP,X'11'      TEST BATCH TYPE IGNORES MOS LOCK             
         BZ    TSTBNQ              NO                                           
         CLI   TBAKBTYP,X'1B'                                                   
         BE    TSTBNQ                                                           
TSTB2    MVC   FULL(L'BMOMOSP),BMOMOSP                                          
         MVC   FULL+L'BMOMOSP(L'BMOMOSC),BMOMOSC                                
*                                                                               
TSTBEQ   CR    RE,RE                                                            
         B     *+6                                                              
TSTBNQ   LTR   RE,RE                                                            
         L     RE,RETURN2                                                       
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SUBMIT SORT RECORDS FOR REPORTS                                     *         
***********************************************************************         
         SPACE 1                                                                
SUBSORT  ST    RE,RETURN1                                                       
         L     R2,APBUFF           R2=A(PROFILE TABLE)                          
*                                                                               
         USING PTABD,R2                                                         
SUBS02   LH    RF,PTLEN                                                         
         LTR   RF,RF                                                            
         BZ    SUBSX                                                            
*                                                                               
         XC    SKCONS,SKCONS                                                    
*                                                                               
SUBS04   L     R3,=A(FILREPS)      R3=A(FILTER CLASS REPORTS TABLE)             
         LA    R0,FILREPSN         NUMBER OF FILTER VALUES                      
SUBS06   CLC   FILVAL,0(R3)        MATCH ON FILTER VALUE                        
         BNE   SUBS12                                                           
         CLI   1(R3),0             TEST PROGRAM TYPE ACTIVE                     
         BE    *+14                                                             
         CLC   JPROG,1(R3)         MATCH ON PROGRAM TYPE                        
         BNE   SUBS12                                                           
         OC    FILOPT,FILOPT       ANY FILTER OPTION?                           
         BNZ   SUBS08                                                           
         CLI   2(R3),0             NO  - ONLY USE DEFAULT REPORT TYPE           
         BE    SUBS14                    LIST                                   
         B     SUBS12                                                           
SUBS08   TM    FILOPT,FOEXCL       YES - TEST OPTION IS EXCLUSIVE               
         BZ    SUBS10                                                           
         CLI   2(R3),0             EXCLUSIVE - ALLOW NO OTHER REPORT            
         BE    SUBS12                          TYPES OR DEFAULT                 
         MVC   BYTE,2(R3)                                                       
         OI    BYTE,FOEXCL                                                      
         CLC   BYTE,FILOPT                                                      
         BNE   SUBS12                                                           
         B     SUBS14                                                           
SUBS10   MVC   BYTE,2(R3)          NON-EXCLUSIVE - ALLOW THIS REPORT            
         NC    BYTE,FILOPT                         TYPE                         
         CLC   BYTE,2(R3)                                                       
         BE    SUBS14                                                           
SUBS12   LA    R3,FILREPSQ(R3)                                                  
         BCT   R0,SUBS06                                                        
         B     SUBSX               NOT REQUIRED                                 
SUBS14   LA    R1,FILREPSQ(R3)     END OF ASSOCIATED REPORT TYPES LIST          
         ST    R1,FULL                                                          
         LA    R3,2(R3)                                                         
*                                                                               
SUBS16   XC    SKSSA1,SKSSA1       CLEAR SPECIAL SORT AREAS (NOT USED)          
         XC    SKSSA2,SKSSA2                                                    
         LA    R3,1(R3)            R3=A(ASSOICATED REPORT TYPE)                 
         C     R3,FULL                                                          
         BE    SUBSX                                                            
*        BE    SUBS18                                                           
         XR    RF,RF                                                            
         IC    RF,0(R3)                                                         
         LTR   RF,RF                                                            
         BZ    SUBSX                                                            
*        BZ    SUBS18                                                           
         STC   RF,SDREPS           ACTUAL REPORT SECTION                        
         STC   RF,SKREPS           REPORT SECTION FOR SORT                      
         CLI   SKREPS,SKROFS       TEST FOR OFFICE SUMMARY                      
         BNE   *+8                                                              
         MVI   SKREPS,SKRLGS       PROCESS WITH LEDGER SUMMARY                  
         BCTR  RF,0                                                             
         LA    RE,QOPTIONS         TEST SECTION REQUIRED                        
         AR    RE,RF               (REQUEST OVERRIDES PROFILE RECORD)           
         CLI   0(RE),C'Y'                                                       
         BNE   SUBS16                                                           
         MH    RF,=Y(PTPLEN)                                                    
         LA    RE,PTPROF                                                        
         AR    RE,RF                                                            
         MVC   SDPROF,0(RE)        PROFILES FOR THIS SECTION                    
         MVC   SDFVAL,FILVAL       FILTER VALUE                                 
         MVC   SDFDAYS,FBATDAYS    DAYS BEFORE DELETION                         
         MVC   SKOUID,PTID         OUTPUT USER-ID                               
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         GOTO1 AIO,IORDD+IOACCDIR+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SDBRDA,TBAKDA       DISK ADDRESS                                 
*                                                                               
         GOTO1 ADSORTER,DMCB,SORTPUT,SREC1                                      
         B     SUBS16                                                           
*                                                                               
SUBSX    L     RE,RETURN1                                                       
         BR    RE                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SELECT REPORT SUB-ROUTINE                                           *         
***********************************************************************         
         SPACE 1                                                                
SELECT   XC    RIND1,RIND1         CLEAR REPORT FLAG 1                          
         XC    BATCPY,BATCPY                                                    
*                                                                               
         GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   RF,15,4(R1)                                                      
         BZ    XIT                                                              
         MVC   SREC1,0(RF)         ACTIVE SORT RECORD                           
SELS02   GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   RF,15,4(R1)                                                      
         BZ    SELS04                                                           
         CLC   SREC1,0(RF)         DUPLICATE                                    
         BE    SELS02                                                           
         MVC   SREC2,0(RF)         NEXT SORT RECORD (UNLESS EOF)                
         B     *+10                                                             
SELS04   MVC   SREC2,SREC1         SET FOR EOF IF ONLY ONE SUBMISSION           
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IODA,SDBRDA                                                      
         GOTO1 AIO,IOGETD+IOACCMST+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIOBUFF          R4=A(BATCH HEADER RECORD)                    
         ST    R4,ABHDREC                                                       
         USING TBARECD,R4                                                       
         MVC   BATAKEY,TBAKEY                                                   
         MVC   BATTYPE,TBAKBTYP                                                 
         MVC   BATIBNO,TBAKBCHR                                                 
*                                                                               
         CLC   BATCPY,TBAKCPY                                                   
         BE    SELS06                                                           
         BAS   RE,SPECPY           RESET SPECIFIC COMPANY REQUIREMENTS          
         MVC   BATCPY,TBAKCPY                                                   
*                                                                               
SELS06   XC    RIND2,RIND2         CLEAR REPORT FLAGS 2 + 3                     
         XC    RIND3,RIND3                                                      
*                                                                               
         MVI   RCSUBPRG,0          GENERAL UPDATES                              
         CLI   SKREPS,SKRDJ1                                                    
         BNE   SELS08                                                           
         CLI   SDPROF+3,YES        DO WE SHOW ANALYSIS A/CS?                    
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         BAS   RE,DJREPS                                                        
         B     SELS20                                                           
*                                                                               
SELS08   MVI   RCSUBPRG,2          SPECAIL UPDATES                              
         CLI   SKREPS,SKRDJ2                                                    
         BNE   SELS10                                                           
         BAS   RE,DJREPS                                                        
         B     SELS20                                                           
*                                                                               
SELS10   CLI   SKREPS,SKRLGS       LEDGER AND OFFICE SUMMARIES                  
         BE    SELS12                                                           
         CLI   SKREPS,SKROFS                                                    
         BNE   SELS14                                                           
SELS12   BAS   RE,LOTSUM                                                        
         B     SELS20                                                           
*                                                                               
SELS14   MVI   RCSUBPRG,5          BATCH SUMMARY                                
         CLI   SKREPS,SKRBTS                                                    
         BNE   SELS16                                                           
         L     R1,=A(REPFRM07)                                                  
         ST    R1,FORMAT                                                        
         BAS   RE,BATSUM                                                        
         B     SELS20                                                           
*                                                                               
SELS16   MVI   RCSUBPRG,6          EXCEPTION LIST                               
         CLI   SKREPS,SKRXCP                                                    
         BNE   SELS18                                                           
         L     R1,=A(REPFRM08)                                                  
         ST    R1,FORMAT                                                        
         BAS   RE,EXLIST                                                        
         B     SELS20                                                           
*                                                                               
SELS18   MVI   RCSUBPRG,7          DUE LIST                                     
         CLI   SKREPS,SKRDUE                                                    
         BNE   SELS20                                                           
         L     R1,=A(REPFRM09)                                                  
         ST    R1,FORMAT                                                        
         BAS   RE,DUELIST                                                       
         B     SELS20                                                           
*                                                                               
SELS20   CLC   SREC1,SREC2         TEST FOR END OF SORT RECORDS                 
         BE    XIT                                                              
SELS22   MVC   SREC3,SREC1                                                      
         MVC   SREC1,SREC2                                                      
         B     SELS02                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* END OF REPORT                                                       *         
***********************************************************************         
         SPACE 1                                                                
EOREPORT TM    PRMODE,PRMLOGOS                                                  
         BZ    XIT                                                              
         MVI   FORCEHED,YES                                                     
         MVC   HALF,ORIGINUM       RESET ID FOR LOGOS                           
         OC    DESTNUM,DESTNUM                                                  
         BZ    *+10                                                             
         MVC   HALF,DESTNUM                                                     
         BAS   RE,LOGODOWN                                                      
         B     XIT                                                              
         EJECT                                                                  
       ++INCLUDE ACREPDJCO                                                      
         SPACE 1                                                                
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
ACIJSMOS DS    PL2                 START MOS                                    
ACIJEMOS DS    PL2                 END MOS                                      
         EJECT                                                                  
***********************************************************************         
* SUPPLEMENATARY REQUEST DETAILS                                      *         
***********************************************************************         
         SPACE 1                                                                
SRQTAB   DS    0C                                                               
         DC    AL2(QAPPL-ACWORKD),AL1(L'QAPPL-5)                                
         DCDD  AC#PRSN,20                                                       
         DC    AL2(QCOMMENT+1-ACWORKD),AL1(L'QCOMMENT-5)                        
         DCDD  AC#BATTY,20                                                      
         DC    AL2(QCOMMENT-ACWORKD),AL1(L'QCOMMENT-6)                          
         DCDD  AC#BATGP,20                                                      
         DC    AL2(QAPPL+8-ACWORKD),AL1(L'QAPPL-9)                              
         DCDD  AC#BATRF,20                                                      
         DC    AL2(QOPT8-ACWORKD),AL1(L'QOPT8-1)                                
         DCDD  AC#OCLOS,20                                                      
         DC    AL2(QOPT1-ACWORKD),AL1(L'QOPT1-1)                                
         DCDD  AC#DTLJL,20                                                      
         DC    AL2(QOPT3-ACWORKD),AL1(L'QOPT3-1)                                
         DCDD  AC#LGBTS,20                                                      
         DC    AL2(QOPT4-ACWORKD),AL1(L'QOPT4-1)                                
         DCDD  AC#OFFSU,20                                                      
         DC    AL2(QOPT5-ACWORKD),AL1(L'QOPT5-1)                                
         DCDD  AC#BATSU,20                                                      
         DC    AL2(QOPT6-ACWORKD),AL1(L'QOPT6-1)                                
         DCDD  AC#XCNLS,20                                                      
         DC    AL2(QOPT7-ACWORKD),AL1(L'QOPT7-1)                                
         DCDD  AC#DULLS,20                                                      
SRQTABX  DC    AL2(0)                                                           
         EJECT                                                                  
       ++INCLUDE ACREPDJDE                                                      
       ++INCLUDE ACREPDJWK                                                      
         SPACE 1                                                                
***********************************************************************         
* SUPPLEMENTARY DSECTS                                                *         
***********************************************************************         
         SPACE 1                                                                
SRQTABD  DSECT                     ** REQUEST REPORT TABLE **                   
SRQDISP  DS    CL2                 DISP TO FIELD IN REQUEST CARD                
SRQLEN   DS    CL1                 EXECUTABLE LENGTH OF FIELD                   
SRQDD    DS    CL20                DATA DICTIONARY REFERENCE                    
SRQQ     EQU   *-SRQDISP           ENTRY LENGTH                                 
         EJECT                                                                  
***********************************************************************         
* IO BUFFERS                                                          *         
***********************************************************************         
         SPACE 1                                                                
ACIJ02   CSECT                                                                  
IOAREALN EQU   2000+L'IODA+L'IOWORK                                             
IOAREA1  DS    (IOAREALN)X         I/O BUFFERS                                  
IOAREA2  DS    (IOAREALN)X                                                      
IOAREA3  DS    (IOAREALN)X                                                      
IOAREA4  DS    (IOAREALN)X                                                      
IOAREAS  EQU   (*-IOAREA1)/IOAREALN                                             
         EJECT                                                                  
***********************************************************************         
* GENERAL BUFFER AREA                                                 *         
***********************************************************************         
         SPACE 1                                                                
NAMEBUFF DS    (NSIZE)X                                                         
NSIZE    EQU   8000                                                             
PROFBUFF DS    (PSIZE)X                                                         
PSIZE    EQU   32000                                                            
DET1BUFF DS    (DSIZE)X                                                         
DET2BUFF DS    (DSIZE)X                                                         
DSIZE    EQU   800                                                              
SUBBUFF  DS    (SSIZE)X                                                         
SSIZE    EQU   400                                                              
LDGBUFF  DS    (LSIZE)X                                                         
LSIZE    EQU   32000                                                            
OFFBUFF  DS    (OSIZE)X                                                         
OSIZE    EQU   32000                                                            
UNITBUFF DS    (USIZE)X                                                         
USIZE    EQU   10000                                                            
WORKBUFF DS    (WSIZE)X                                                         
WSIZE    EQU   5000                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACREPIJ02 10/14/15'                                      
         END                                                                    
