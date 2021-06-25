*          DATA SET ACREPDJ02S AT LEVEL 015 AS OF 12/16/99                      
         TITLE 'DAILY JOURNAL CONTROLLER'                                       
*PHASE ACDJ02A                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE GETLOGO                                                                
ACDJ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NWDJ**,R9,R8,R7,R6,R5                                        
         USING ACWORKD,RA          GLOBAL STORAGE                               
         LA    RC,SPACEND                                                       
         USING WORKD,RC            LOCAL STORAGE                                
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SETINIT                                                          
         CLI   MODE,RUNLAST                                                     
         BE    BLDSORT                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* INITIALISE VALUES                                                   *         
***********************************************************************         
         SPACE 1                                                                
SETINIT  MVI   JPROG,JPDJ                                                       
         XC    PRMODE,PRMODE                                                    
         L     RF,ADMASTC                                                       
         CLI   MCTSTRUN-MASTD(RF),X'FF'                                         
         BNE   *+8                                                              
         OI    PRMODE,PRMTEST      SET TEST=YES                                 
         CLI   MCWRITE-MASTD(RF),YES                                            
         BE    *+8                                                              
         OI    PRMODE,PRMWRITX     SET WRITE=NO                                 
         TM    MCPRTIND-MASTD(RF),MCPRTINL                                      
         BZ    *+8                                                              
         OI    PRMODE,PRMNOLOG     SET NOLOGO                                   
*                                                                               
         LA    RF,IOEXEC                                                        
         ST    RF,AIO                                                           
         LA    R0,HOOK                                                          
         ST    R0,HEADHOOK                                                      
*                                                                               
         L     RF,ADCOMFAC                                                      
         MVC   VBLDCUR,CBLDCUR-COMFACSD(RF)                                     
*                                                                               
         MVI   RCFLAG1,RCFREPLC    LOWER CASE SPECS                             
*                                                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,SORTRECD                                  
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAYC)                                
         GOTO1 (RF),(R1),(4,RCDATE),(0,TODAYE)                                  
         GOTO1 (RF),(R1),(4,RCDATE),(1,TODAYP)                                  
         MVC   TODAYCC,EFFALL                                                   
         XC    TODAYCC,TODAYC      COMPLEMENT COMPRESSED DATE                   
*                                  SET DATE RANGE FOR BATCH FILTERING           
         LA    R0,31               HIGH: TODAY+31                               
         GOTO1 ADDAY,DMCB,TODAYE,WORK,(R0)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(2,BHIDAT)                                  
         LNR   R0,R0               LOW:  TODAY-31                               
         GOTO1 ADDAY,DMCB,TODAYE,WORK,(R0)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(2,BLODAT)                                  
*                                                                               
         L     RE,ANBUFF           CLEAR NAME TABLE                             
         L     RF,=A(NSIZE)                                                     
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    WKID,WKID         SET WORKER INDEX VALUES (ODDS FILE)            
         MVC   WKIUSER,ORIGINUM                                                 
         MVI   WKISYS,WKISACC                                                   
         MVC   WKIPRG,=C'DJ'                                                    
         MVC   WKIDAY,TODAYP+2                                                  
         MVI   WKITYPE,WKITODDS                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD SORT RECORDS FOR ANALYSIS                                     *         
***********************************************************************         
         SPACE 1                                                                
BLDSORT  TM    PRMODE,PRMTEST      TEST MODE ONLY                               
         BZ    BLDS01                                                           
         XC    WORD,WORD           ALLOW FILTERING BY BATCH TYPE                
         CLC   QCOMMENT+1(2),SPACES                                             
         BNH   BLDS01                                                           
         CLI   QCOMMENT+2,C' '                                                  
         BNE   *+14                                                             
         MVC   QCOMMENT+2(1),QCOMMENT+1                                         
         MVI   QCOMMENT+1,C' '                                                  
         PACK  DUB,QCOMMENT+1(2)                                                
         CVB   R1,DUB                                                           
         STC   R1,WORD+2                                                        
         OI    PRMODE,PRMSHRT      SET SHORT RUN ACTIVE                         
*                                                                               
BLDS01   XC    BATCPY,BATCPY                                                    
         XC    BATUID,BATUID                                                    
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING TBARECD,R4                                                       
         MVI   TBAPTYP,TBAPTYPQ    READ FOR BATCH HEADER PASSIVE RECORD         
BLDS02   LA    R1,IOHID+IOACCDIR+IO1                                            
         B     *+8                                                              
BLDS04   LA    R1,IOSEQD+IOACCDIR+IO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TBAPTYP,TBAPTYPQ    TEST END OF BATCH RECORDS                    
         BNE   SELECT                                                           
**       CLI   TBAKBMOS,X'99'      AVOID DUFF MOS-NO GOOD FOR Y2K               
**       BH    BLDS04                                                           
         OC    IODA,IODA                                                        
         BZ    BLDS04                                                           
         TM    PRMODE,PRMSHRT      SHORT RUN ONLY                               
         BZ    BLDS05                                                           
         OC    WORD+2(1),WORD+2    ALLOW FILTERING BY BATCH TYPE                
         BZ    BLDS05                                                           
         CLC   TBAPBTYP,WORD+2                                                  
         BNE   BLDS04                                                           
BLDS05   CLC   TBAPEFDT,BLODAT     TEST WITHIN DATE RANGE                       
         BNL   BLDS06                                                           
         MVC   TBAPEFDT,BLODAT                                                  
         XC    TBAPGRUP(TBAPLAST-TBAPGRUP),TBAPGRUP                             
         B     BLDS02                                                           
BLDS06   CLC   TBAPEFDT,BHIDAT                                                  
         BNH   *+14                                                             
         MVC   TBAPEFDT,EFFALL     FORCE READ FOR NEXT USER-ID                  
         B     BLDS02                                                           
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
         BE    BLDS08                                                           
         XC    WORK,WORK           SET JX PROFILES FOR COMPANY                  
         MVC   WORK(4),=C'A0JX'                                                 
         MVC   WORK+12(2),ALPHAID                                               
         GOTO1 GETPROF,DMCB,WORK,AGYPROFS,DATAMGR                               
         BAS   RE,SPECPY           SET SPECIFIC COMPANY REQUIREMENTS            
BLDS08   BAS   RE,FILBAT           FILTER BATCH HEADER RECORD                   
         BNE   BLDS20              NEXT RECORD                                  
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         XC    MCORIGID,MCORIGID   STOP STANDARD PROFILES DEFAULT               
         MVC   DEFLTID,ALPHAID     SET AGENCY PROFILES FOR DEFAULT              
         CLC   BATCPY,TBAKCPY      TEST CHANGE OF COMPANY                       
         BE    BLDS10                                                           
         MVC   BATCPY,TBAKCPY      SAVE COMPANY CODE                            
         BAS   RE,CMPNYADD         ADD COMPANY NAME & PROFS TO BUFFER           
         BAS   RE,GROUPADD         ADD ID-GRP NAMES & PROFS/USER-IDS            
BLDS10   CLC   BATUID,TBAKUSER     TEST CHANGE OF USER-ID                       
         BE    BLDS12                                                           
         MVC   BATUID,TBAKUSER                                                  
         BAS   RE,USERADD          ADD USER-ID NAME & PROFS TO BUFFER           
         L     RF,ADMASTC                                                       
         MVC   MCORIGID,ORIGINUM   RESTORE STANDARD PROFILES DEFAULT            
         XC    WORK,WORK           SET JX PROFILES FOR USER                     
         MVC   WORK(4),=C'A0JX'                                                 
         MVC   WORK+12(2),TBAKUSER                                              
         GOTO1 GETPROF,DMCB,WORK,USRPROFS,DATAMGR                               
         OC    USRPROFS,USRPROFS                                                
         BNZ   BLDS14                                                           
         MVC   USRPROFS,AGYPROFS   USE AGENCY PROFILES IF NONE FOR USER         
         B     BLDS14                                                           
*                                                                               
BLDS12   L     RF,ADMASTC                                                       
         MVC   MCORIGID,ORIGINUM   RESTORE STANDARD PROFILES DEFAULT            
         DROP  RF                                                               
*                                                                               
BLDS14   XC    SREC1,SREC1         BUILD SORT RECORD                            
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
BLDS16   CLI   0(RF),0                                                          
         BE    BLDS18                                                           
         CLC   0(RF),TBAKBTYP                                                   
         BE    *+12                                                             
         LA    RF,DJSRCLEN(RF)                                                  
         B     BLDS16                                                           
         MVC   SKSRC,1(RF)                                                      
*                                                                               
BLDS18   BAS   RE,SUBSORT                                                       
*                                                                               
BLDS20   LA    R4,IOKEY                                                         
         MVC   IOKEY,BATPKEY       RESTORE BATCH HEADER PASSIVE KEY             
         MVC   TBAPTSEQ,EFFALL     FORCE NEXT BATCH HEADER RECORD               
         B     BLDS02                                                           
         EJECT                                                                  
***********************************************************************         
* FILTER BATCH HEADER RECORDS                                         *         
*                                                                     *         
* EXIT - CC INDICATES PROCEEDING ACTION                               *         
***********************************************************************         
         SPACE 1                                                                
FILBAT   ST    RE,RETURN1                                                       
         MVI   FBATDAYS,0                                                       
         CLC   TBAHKUDT,TODAYC                                                  
         BNE   FILNQ               NO CHANGES TO REGISTER                       
*                                                                               
         MVI   FILVAL,FVINSU       *INSTANT UPDATE*                             
         TM    TBAKHSTA,TBAHSIAD   INSTANT UPDATE                               
         BO    FIL16                                                            
*                                                                               
         MVI   FILVAL,FVDFTU       *DRAFT GOING LIVE*                           
         TM    TBAHKIND,TBAHIUOF   UPDATED OFFLINE                              
         BZ    FIL02                                                            
         CLC   TBAHKADT,TODAYC     WAS BATCH ADDED TODAY?                       
         BNE   FIL16                                                            
         MVI   FILVAL,FVOVNU       *OVERNIGHT UPDATE*                           
*        MVI   FILVAL,FVOVNS       *OVERNIGHT-SPECIAL*                          
         B     FIL16                                                            
*                                                                               
FIL02    MVI   FILVAL,FVONLU       *ONLINE UPDATE*                              
         TM    TBAKHSTA,TBAHSUPD   UPDATED                                      
         BO    FIL16                                                            
*                                                                               
         CLC   TBAHKEDT,TODAYC     IS EFFECTIVE DATE AFTER TODAY?               
         BH    FIL06               YES - MUST BE DUE LIVE SOON BATCH            
*                                                                               
         TM    TBAHKIND,TBAHIAAP+TBAHIMAL  ANY APPROVAL/MOA ERRORS?             
         BNZ   FIL04                       YES - MUST BE DRAFT BATCH            
         TM    TBAKHSTA,TBAHSIIP+TBAHSSAV  IS BATCH HELD OPEN/SAVED?            
         BNZ   FIL12                       YES - MUST BE DRAFT BATCH            
         B     FIL08               BATCH OK SO UPDATE IS PENDING                
*                                                                               
FIL04    MVI   FILVAL,FVDFTD       *OLD DRAFT ERROR (DELETE)*                   
         TM    TBAHKIND,TBAHIDEL                                                
         BO    FILEQ                                                            
         MVI   FILVAL,FVDFTW       *NEW DRAFT ERROR (WARN)*                     
         B     FIL16                                                            
*                                                                               
FIL06    TM    TBAKHSTA,TBAHSIIP+TBAHSSAV  WARN IF OPEN/SAVED                   
*        BNZ   FIL10                                                            
         BNZ   FIL12                                                            
         TM    TBAHKIND,TBAHIMAL                                                
         BO    FIL10                                                            
FIL08    MVI   FILVAL,FVDUEN       *DUE LIVE SOON - NO APPROVAL REQD*           
         TM    CMPAPRV,APREFF                                                   
         BZ    FIL16                                                            
         MVI   FILVAL,FVDUEA       *DUE LIVE SOON - APPROVED*                   
         TM    TBAKHSTA,TBAHSAPR                                                
         BO    FIL16                                                            
FIL10    MVI   FILVAL,FVDUEU       *DUE LIVE SOON - UNAPPROVED (WARN)*          
         MVC   FBATDAYS,PRFBXCPD   SET DAYS REMAINING (EXCEPTION LIST)          
         B     FIL16                                                            
*                                                                               
FIL12    TM    TBAKHSTA,TBAHSIIP   BATCH OPEN                                   
         BZ    FIL14                                                            
         MVI   FILVAL,FVOPNW       *NEW OPEN BATCH (WARN)*                      
         TM    TBAHKIND,TBAHIWRN                                                
         BO    FIL16                                                            
         MVI   FILVAL,FVOPND       *OLD OPEN BATCH (DELETE)*                    
         B     FILEQ                                                            
*                                                                               
FIL14    TM    TBAKHSTA,TBAHSSAV   BATCH SAVED                                  
         BZ    FIL16                                                            
         MVI   FILVAL,FVSAVW       *NEW SAVED BATCH (WARN)*                     
         TM    TBAHKIND,TBAHIWRN                                                
         BO    FIL16                                                            
         MVI   FILVAL,FVSAVD       *OLD SAVED BATCH (DELETE)*                   
         B     FILEQ                                                            
*                                                                               
FIL16    TM    TBAKHSTA,TBAHSDEL   BATCH DELETED (BY NEW BATCH)                 
         BZ    FILEQ                                                            
         MVI   FILVAL,FVDELD       *DELETED IN NEW BATCH*                       
*                                                                               
FILEQ    CR    RE,RE               CC EQUAL TO PROCESS                          
         B     *+6                                                              
FILNQ    LTR   RE,RE               CC NOT EQUAL FOR NEXT RECORD                 
         L     RE,RETURN1                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUBMIT SORT RECORDS FOR REPORTS AT ALL LEVELS OF CONSOLIDATION      *         
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
         CLI   PTTYP,PTTYPC        COMPANY LEVEL                                
         BNE   SUBS04                                                           
         CLC   PTID,CMPUID         MATCH ON PRINCIPAL ID                        
         BNE   SUBS22                                                           
         MVI   SKCONS,SKCONCPY     COMPANY CONSOLIDATION                        
         B     SUBS10                                                           
*                                                                               
SUBS04   CLI   PTTYP,PTTYPG        ID-GROUP LEVEL ENTRY                         
         BNE   SUBS08                                                           
         LA    RE,PTIDL            RE=A(USER-IDS BLOCK)                         
         AR    RF,R2               RF=END OF BLOCK/ENTRY                        
SUBS06   CLC   0(L'BATUID,RE),BATUID  CHECK USER-ID IN GROUP                    
         BNE   *+12                                                             
         MVI   SKCONS,SKCONGRP       ID-GROUP CONSOLIDATION                     
         B     SUBS10                                                           
         LA    RE,L'PTIDL(RE)                                                   
         CR    RE,RF                                                            
         BL    SUBS06                                                           
         B     SUBS22              USER-ID NOT IN THIS GROUP                    
*                                                                               
SUBS08   CLI   PTTYP,PTTYPU        USER-ID LEVEL ENTRY                          
         BNE   SUBS22                                                           
         CLC   PTID,BATUID                                                      
         BNE   SUBS22                                                           
         XC    SKCONS,SKCONS                                                    
*                                                                               
SUBS10   L     R3,=A(FILREPS)      R3=A(FILTER CLASS REPORTS TABLE)             
         LA    R0,FILREPSN         NUMBER OF FILTER VALUES                      
SUBS12   CLC   FILVAL,0(R3)        MATCH ON FILTER VALUE                        
         BNE   SUBS14                                                           
         CLI   1(R3),0             TEST PROGRAM TYPE ACTIVE                     
         BE    SUBS16                                                           
         CLC   JPROG,1(R3)         MATCH ON PROGRAM TYPE                        
         BE    SUBS16                                                           
SUBS14   LA    R3,FILREPSQ(R3)                                                  
         BCT   R0,SUBS12                                                        
         DC    H'0'                                                             
SUBS16   LA    R1,FILREPSQ(R3)     END OF ASSOCIATED REPORT TYPES LIST          
         ST    R1,FULL                                                          
         LA    R3,2(R3)                                                         
*                                                                               
SUBS18   XC    SKSSA1,SKSSA1       CLEAR SPECIAL SORT AREAS                     
         XC    SKSSA2,SKSSA2                                                    
         LA    R3,1(R3)            R3=A(ASSOICATED REPORT TYPE)                 
         C     R3,FULL                                                          
         BE    SUBS22                                                           
         XR    RF,RF                                                            
         IC    RF,0(R3)                                                         
         LTR   RF,RF                                                            
         BZ    SUBS22                                                           
         STC   RF,SDREPS           ACTUAL REPORT SECTION                        
         STC   RF,SKREPS           REPORT SECTION FOR SORT                      
         CLI   SKREPS,SKROFS       TEST FOR OFFICE SUMMARY                      
         BNE   *+8                                                              
         MVI   SKREPS,SKRLGS       PROCESS WITH LEDGER SUMMARY                  
         BCTR  RF,0                                                             
         MH    RF,=Y(PTPLEN)                                                    
         LA    RE,PTPROF                                                        
         AR    RE,RF               RE=A(REPORT SECTION PROFILES)                
         CLI   0(RE),C'Y'          ALL BATCHES REQUIRED                         
         BNE   SUBS18                                                           
         MVC   SDPROF,0(RE)        PROFILES FOR THIS SECTION                    
         MVC   SDFVAL,FILVAL       FILTER VALUE                                 
         MVC   SDFDAYS,FBATDAYS    DAYS BEFORE DELETION                         
         MVC   SKOUID,PTID         OUTPUT USER-ID                               
         CLI   SKREPS,SKRDJ2       TEST FOR OTHER REPS EXCEPT DJ                
         BNH   SUBS20                                                           
         OC    SKCONS,SKCONS       IS THIS A CONSOLIDATED REPORT?               
         BZ    SUBS20                                                           
         MVC   SKSSA2(L'SKTYP),SKTYP  YES - INCREASE B/TYPE PRIORITY            
*                                                                               
SUBS20   XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'TBAKEY),TBAKEY                                           
         GOTO1 AIO,IORDD+IOACCDIR+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SDBRDA,TBAKDA       DISK ADDRESS                                 
*                                                                               
         GOTO1 ADSORTER,DMCB,SORTPUT,SREC1                                      
         B     SUBS18                                                           
*                                                                               
SUBS22   LH    RF,PTLEN                                                         
         AR    R2,RF                                                            
         C     R2,TABLEX                                                        
         BL    SUBS02                                                           
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
         BZ    SELSX                                                            
         MVC   SREC1,0(RF)         ACTIVE SORT RECORD                           
SELS02   GOTO1 ADSORTER,DMCB,SORTGET                                            
         ICM   RF,15,4(R1)                                                      
         BZ    SELS04                                                           
         CLC   SREC1,0(RF)         DUPLICATE                                    
         BE    SELS02                                                           
         MVC   SREC2,0(RF)         NEXT SORT RECORD (UNLESS EOF)                
         B     *+10                                                             
SELS04   MVC   SREC2,SREC1         SET EOF IF ONLY ONE SUBMISSION               
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
         BE    *+14                                                             
         BAS   RE,SPECPY           RESET SPECIFIC COMPANY REQUIREMENTS          
         MVC   BATCPY,TBAKCPY                                                   
*                                                                               
         TM    PRMODE,PRMACT       ARE WE IN ACTIVE PRINT MODE?                 
         BO    SELS08              YES - OUPUT ID HASN'T CHANGED                
         OI    PRMODE,PRMACT       NO  - GET SET FOR NEW OUTPUT ID              
         ZAP   ODNUMB,=P'0'        CLEAR ODDS FILE TOTALS                       
         ZAP   ODITEM,=P'0'                                                     
         ZAP   ODACP,=P'0'                                                      
         ZAP   ODANP,=P'0'                                                      
         MVC   HALF,SKOUID         SET ID FOR THIS RUN                          
         MVI   FORCEHED,YES        NEW PAGE FOR NEW ID                          
         BAS   RE,REMCHECK         TEST FOR REMOTE ID                           
         TM    PRMODE,PRMREMO      SKIP LOGOS IF REMOTE                         
         BZ    SELS06                                                           
         TM    COMP8,CPYSRLOG      BUT NOT ON REMOTE TURNAROUNDS                
         BO    SELS06                                                           
         MVC   PAGE,=H'1'          SET PAGE NUMBER NOW IF NO LOGOS              
         B     SELS08                                                           
SELS06   TM    PRMODE,PRMNOLOG     DON'T PRINT LOGOS IF NOLOGOS                 
         BO    SELS08                                                           
         BAS   RE,LOGOUP           PRINT START LOGOS                            
         OI    PRMODE,PRMLOGOS                                                  
*                                                                               
SELS08   XC    RIND2,RIND2         CLEAR REPORT FLAGS 2 + 3                     
         XC    RIND3,RIND3                                                      
*                                                                               
         MVI   RCSUBPRG,0          GENERAL UPDATES                              
         CLI   SKREPS,SKRDJ1                                                    
         BNE   SELS10                                                           
         CLI   SDPROF+3,YES        DO WE SHOW ANALYSIS A/CS?                    
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         BAS   RE,DJREPS                                                        
         AP    ODNUMB,=P'1'        ADD BATCH TOTALS TO ODDS FILE TOTALS         
         AP    ODITEM,DTBITA                                                    
         AP    ODACP,DTBACD                                                     
         AP    ODANP,DTBAND                                                     
         B     SELS22                                                           
*                                                                               
SELS10   MVI   RCSUBPRG,2          SPECAIL UPDATES                              
         CLI   SKREPS,SKRDJ2                                                    
         BNE   SELS12                                                           
         BAS   RE,DJREPS                                                        
         B     SELS22                                                           
*                                                                               
SELS12   CLI   SKREPS,SKRLGS       LEDGER AND OFFICE SUMMARIES                  
         BE    SELS14                                                           
         CLI   SKREPS,SKROFS                                                    
         BNE   SELS16                                                           
SELS14   BAS   RE,LOTSUM                                                        
         B     SELS22                                                           
*                                                                               
SELS16   MVI   RCSUBPRG,5          BATCH SUMMARY                                
         CLI   SKREPS,SKRBTS                                                    
         BNE   SELS18                                                           
         LA    R1,REPFRM07                                                      
         ST    R1,FORMAT                                                        
         BAS   RE,BATSUM                                                        
         B     SELS22                                                           
*                                                                               
SELS18   MVI   RCSUBPRG,6          EXCEPTION LIST                               
         CLI   SKREPS,SKRXCP                                                    
         BNE   SELS20                                                           
         LA    R1,REPFRM08                                                      
         ST    R1,FORMAT                                                        
         BAS   RE,EXLIST                                                        
         B     SELS22                                                           
*                                                                               
SELS20   MVI   RCSUBPRG,7          DUE LIST                                     
         CLI   SKREPS,SKRDUE                                                    
         BNE   SELS22                                                           
         LA    R1,REPFRM09                                                      
         ST    R1,FORMAT                                                        
         BAS   RE,DUELIST                                                       
         B     SELS22                                                           
*                                                                               
SELS22   CLC   SREC1,SREC2         TEST FOR END OF SORT RECORDS                 
         BE    SELS30                                                           
         CLC   SREC1(SKREPS-SKEY),SREC2  IS OUTPUT ID ABOUT TO CHANGE?          
         BE    SELS28                                                           
         BAS   RE,ADDODDS          YES - ADD AN ODDS FILE ENTRY                 
         TM    PRMODE,PRMREMO          - PRINT END LOGOS IF ACTIVE              
         BZ    SELS24                                                           
         TM    COMP8,CPYSRLOG                                                   
         BO    SELS24                                                           
         TM    PRMODE,PRMTEST                                                   
         BO    SELS24                                                           
         GOTO1 PRINT,DMCB,(L'PRTCLOSE,PRTCLOSE)                                 
         B     SELS26                                                           
SELS24   TM    PRMODE,PRMNOLOG                                                  
         BO    SELS26                                                           
         MVI   FORCEHED,YES                                                     
         BAS   RE,LOGODOWN                                                      
SELS26   NI    PRMODE,X'FF'-(PRMACT+PRMREMO+PRMLOGOS)                           
SELS28   MVC   SREC3,SREC1                                                      
         MVC   SREC1,SREC2                                                      
         B     SELS02                                                           
*                                                                               
SELS30   DS    0H                                                               
         BAS   RE,ADDODDS          ADD THE LAST ODDS ENTRY                      
         TM    PRMODE,PRMREMO      PRINT END LOGOS IF ACTIVE                    
         BZ    SELS32                                                           
         TM    COMP8,CPYSRLOG                                                   
         BO    SELS32                                                           
         TM    PRMODE,PRMTEST                                                   
         BO    SELS32                                                           
         GOTO1 PRINT,DMCB,(L'PRTCLOSE,PRTCLOSE)                                 
         B     SELSX                                                            
SELS32   TM    PRMODE,PRMNOLOG                                                  
         BO    SELSX                                                            
         MVI   FORCEHED,YES                                                     
         BAS   RE,LOGODOWN                                                      
SELSX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SET REMOTE DETAILS AND USER-ID DEFAULTS                             *         
***********************************************************************         
         SPACE 1                                                                
REMCHECK NTR1                                                                   
         NI    PRMODE,X'FF'-PRMREMO                                             
         L     R3,REMOTEC                                                       
         USING REMOTED,R3                                                       
         XC    REMOTKEY,REMOTKEY                                                
*                                                                               
         XC    WORK,WORK           READ ACCPAK MASTER PROFILES                  
         MVC   WORK(4),=C'A000'                                                 
         MVC   WORK+12(2),HALF                                                  
         GOTO1 GETPROF,DMCB,WORK,MSTPROFS,DATAMGR                               
*                                                                               
         XC    IOKEY,IOKEY         READ USER-ID RECORD                          
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,HALF                                                     
         GOTO1 AIO,IORD+IOCLFILE+IO4                                            
         BNE   REM12                                                            
*                                                                               
         L     R2,AIOBUFF                                                       
         LA    R1,CTIDATA                                                       
         XR    R0,R0                                                            
REM02    CLI   0(R1),0                                                          
         BE    REM10                                                            
*                                                                               
         CLI   0(R1),CTAGYELQ      AGENCY ALPHA ID ELEMENT                      
         BNE   *+14                                                             
         MVC   BYTE,CTAGYLNG-CTAGYD(R1)  LANGUAGE CODE                          
         B     REM08                                                            
*                                                                               
         CLI   0(R1),CTOCOELQ      OUTPUT CODE ELEMENT                          
         BNE   REM03                                                            
         TM    PRMODE,PRMTEST                                                   
         BO    REM08                                                            
         USING CTOCOD,R1                                                        
         CLI   CTOCODE,C'#'        TEST REMOTE                                  
         BNE   REM08                                                            
         CLC   CTOCODE+1(L'CTOCODE-1),SPACES  TEST ALL REMOTE                   
         BE    REM02A                                                           
         LA    R0,L'CTOCODE-2                                                   
         LA    RE,CTOCODE+1                                                     
         CLI   0(RE),C'A'          TEST ACCOUNTING REMOTE                       
         BE    REM02A                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
         B     REM08                                                            
REM02A   BAS   RE,REMSET                                                        
         B     REM08                                                            
*                                                                               
REM03    CLI   0(R1),CTDSTELQ      DESTINATIONS DETAIL ELEMENT                  
         BNE   REM08                                                            
         CLI   MSTPROFS+2,YES      TEST ORIGIN NAME OVERRIDE                    
         BNE   REM08                                                            
         L     RE,ANBUFF           REPLACE COMPANY NAME IN NAME BUFFER          
         USING NTABD,RE                                                         
         LR    RF,RE                                                            
         AH    RF,=Y(NSIZE-NTLNQ)                                               
REM04    OC    NTNTRY(NTLNQ),NTNTRY                                             
         BZ    REM08               CAN'T FIND COMPANY                           
         CLI   NTTYP,NTTYPC                                                     
         BNE   REM06                                                            
         CLC   NTAGY,ALPHAID                                                    
         BNE   REM06                                                            
         USING CTDSTD,R1                                                        
         MVC   NTNAME(L'CTDSTNAM),CTDSTNAM                                      
         B     REM08                                                            
REM06    LA    RE,NTLNQ(RE)                                                     
         CR    RF,RE                                                            
         BH    REM04                                                            
         B     REM08               END OF TABLE                                 
         DROP  R1,RE                                                            
*                                                                               
REM08    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     REM02                                                            
         DROP  R2                                                               
*                                                                               
REM10    CLC   RCLANG,BYTE         TEST SAME LANGUGE AS LAST COMPANY            
         BE    REM12                                                            
         L     RF,=A(DDIN1)                                                     
         GOTO1 ADDICTAT,DMCB,C'LU  ',(RF),DDOUT1                                
         ORG   *-2                                                              
         MVC   3(1,R1),BYTE        SET LANGUAGE FOR DICTATE                     
         BASR  RE,RF                                                            
         L     RF,=A(DDIN2)                                                     
         GOTO1 ADDICTAT,DMCB,C'LL  ',(RF),DDOUT2                                
         ORG   *-2                                                              
         MVC   3(1,R1),BYTE        SET LANGUAGE FOR DICTATE                     
         BASR  RE,RF                                                            
         MVC   RCLANG,BYTE                                                      
         L     R1,ADMASTC                                                       
         USING MASTD,R1                                                         
         MVC   MCLANG,BYTE                                                      
         DROP  R1                                                               
*                                                                               
REM12    DS    0H                                                               
         USING CT5REC,R2                                                        
         XC    IOKEY,IOKEY         READ ACCESS RECORD TO ESTABLISH              
         LA    R2,IOKEY                                                         
         MVI   CT5KTYP,CT5KTYPQ    AGENCY COUNTRY CODE                          
         MVC   CT5KALPH,ALPHAID                                                 
         GOTO1 AIO,IORD+IOCLFILE+IO4                                            
         BNE   REM13                                                            
         L     R2,AIOBUFF                                                       
         LA    R1,CT5DATA                                                       
         USING CTAGDD,R1                                                        
         SR    R0,R0                                                            
REM12A   CLI   CTAGDEL,0                                                        
         BE    REM13                                                            
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    *+14                                                             
         IC    R0,CTAGDLEN                                                      
         AR    R1,R0                                                            
         B     REM12A                                                           
         CLI   CTAGDLEN,CTAGDCTY-CTAGDD                                         
         BL    REM13                                                            
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   MCCTRY,CTAGDCTY     SET AGENCY COUNTRY CODE                      
*                                                                               
REM13    TM    PRMODE,PRMREMO                                                   
         BO    REMX                                                             
         TM    PRMODE,PRMTEST                                                   
         BO    REMX                                                             
*                                                                               
         XC    IOKEY,IOKEY         READ USER PROFILE RECORD                     
         LA    R2,IOKEY                                                         
         USING CTPREC,R2                                                        
         MVI   CTPKTYP,CTPKTYPQ                                                 
         MVC   CTPKSYS(3),=C'ADJ'                                               
         MVC   CTPKORIG,HALF                                                    
         GOTO1 AIO,IORD+IOCLFILE+IO4                                            
         BNE   REMX                                                             
*                                                                               
         L     R2,AIOBUFF                                                       
         LA    R1,CTPDATA                                                       
         XR    R0,R0                                                            
         USING CTOCOD,R1                                                        
REM14    CLI   CTOCOEL,0                                                        
         BE    REMX                                                             
         CLI   CTOCOEL,CTOCOELQ    OUTPUT CODE ELEMENT                          
         BNE   REM16                                                            
         CLC   =C'REMOTE',CTOCODE                                               
         BNE   REM18                                                            
         BAS   RE,REMSET                                                        
*                                                                               
REM16    CLI   CTOCOEL,X'43'                                                    
         BNE   REM18                                                            
         CLC   =C'S1',CTOCODE      SERIES 1                                     
         BNE   *+8                                                              
         NI    PRMODE,X'FF'-PRMREMO                                             
*                                                                               
REM18    IC    R0,CTOCOLEN                                                      
         AR    R1,R0                                                            
         B     REM14                                                            
         DROP  R1,R2                                                            
*                                                                               
REMX     B     XIT                                                              
*                                                                               
REMSET   MVC   REMOTDST(2),HALF                                                 
         MVC   REMOTKEY(8),=C'JOURNALS'                                         
         MVC   REMOTJID(3),=C'ADJ'                                              
         MVI   REMOTCLS,C'Q'                                                    
         OI    PRMODE,PRMREMO                                                   
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ODDS RECORD TO WORKER FILE                                      *         
***********************************************************************         
         SPACE 1                                                                
ADDODDS  NTR1                                                                   
         TM    PRMODE,PRMWRITX                                                  
         BO    XIT                                                              
*                                                                               
         L     R2,AIO4                                                          
         LA    R2,L'IODA+L'IOWORK(R2)                                           
         USING ODDENTD,R2                                                       
         L     RF,LOGOC                                                         
         USING LOGOD,RF                                                         
         XC    ODDLEN(ODDLENQ),ODDLEN                                           
         MVC   ODDLEN,=Y(ODDLENQ)                                               
         MVC   ODDCPY,BATCPY                                                    
         MVC   ODDID,SKOUID                                                     
         MVC   ODDCONS,=C'C'                                                    
         CLI   SKCONS,SKCONCPY                                                  
         BE    AODD02                                                           
         MVC   ODDCONS,=C'G'                                                    
         CLI   SKCONS,SKCONGRP                                                  
         BE    AODD02                                                           
         MVC   ODDCONS,=C'U'                                                    
AODD02   MVC   ODDLOG1,LOGO1                                                    
         MVC   ODDLOG2,LOGO2                                                    
         MVC   ODDBATS,ODNUMB                                                   
         MVC   ODDITEM,ODITEM                                                   
         MVC   ODDANPS,ODANP                                                    
         MVC   ODDACPS,ODACP                                                    
         GOTO1 AIO,IOWKADD+IOWORKER+IO4                                         
         CLC   SREC1,SREC2                                                      
         BNE   XIT                                                              
         GOTO1 AIO,IOWKCLS+IOWORKER+IO4                                         
         B     XIT                                                              
         DROP  R2,RF                                                            
         EJECT                                                                  
       ++INCLUDE ACREPDJCO                                                      
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACREPDJDE                                                      
         EJECT                                                                  
       ++INCLUDE ACREPDJWK                                                      
         EJECT                                                                  
***********************************************************************         
* IO BUFFERS                                                          *         
***********************************************************************         
         SPACE 1                                                                
ACDJ02   CSECT                                                                  
IOAREALN EQU   2000+L'IODA+L'IOWORK                                             
IOAREA1  DS    (IOAREALN)X         I/O BUFFERS                                  
IOAREA2  DS    (IOAREALN)X                                                      
IOAREA3  DS    (IOAREALN)X                                                      
IOAREA4  DS    (IOAREALN)X                                                      
IOAREAS  EQU   (*-IOAREA1)/IOAREALN                                             
         SPACE 1                                                                
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
**PAN#1  DC    CL21'015ACREPDJ02S12/16/99'                                      
         END                                                                    
