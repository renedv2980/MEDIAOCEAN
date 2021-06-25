*          DATA SET ACBRA10    AT LEVEL 007 AS OF 08/05/20                      
*PHASE T62410A                                                                  
                                                                                
         TITLE 'MQ offline update server'                                       
                                                                                
***********************************************************************         
* Level change comments                                               *         
* ---------------------                                               *         
* TKLU 001 14Feb18 DSRD-17967 New server for new Estimate Electronic  *         
*                             Signature upload                        *         
* NSHE 002 07Jun19 DSRD-19284 Fix bad length in element for audit     *         
* ABID 003 12NOV18 DSPCA-2854 ADD MQ ENTRY FOR NEW RUNNER TO UPDATE   *         
*                             WORKER FILE POSTINGS FOR ACCOUNTING     *         
* MPEN 004 01May19 DSRD-22426 Add server for order electronic sig     *         
* YNGX 005 25Oct19 DSRD-24306 Relink to include new ACURLTAB          *         
* VGUP 006 18May20 DSRD-26302 Changed NY office address in email                
* NSHE 007 05Aug20 DSRD-27173 Clean up URL logic                                
***********************************************************************         
* Coments                                                             *         
* -------                                                             *         
* Based on ACBRA14 and MELNK17 code                                   *         
* This runs under MQ but can be run from =BRA in CMV for testing, too *         
* This is supposed to run single request at a time (so no two or more *         
* requests combined as a single run)                                  *         
* TKLU.DDS.JCL(RUNBRAC) to IDF run it                                 *         
* TKLU.DDS.JCL(MQPUTBIG) to run as MQ                                 *         
* Note RLEN=4096 for maximum of data overall                          *         
* Note Workerkey=ACSI needs the AC start for Dictate offline MCSYSTEM *         
*                                                                     *         
***********************************************************************         
* Issues                                                              *         
* ------                                                              *         
* - EXITY on error under MQ not EXITN                                 *         
* - No data passing between validate and run under MQ                 *         
* - Response with K=? issue - online only                             *         
*                                                                     *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=UO,CODE=ENTRY,RLEN=4096,REQUEST=*,WORKERKEY=ACSI,  x        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ,         x        
               FILES=FILES,SERVERTYPE=TSTBOTU,SYSPHASE=SYSPHASE,IDF=Y, x        
               PROGRAM=RCVPBRAQ,ABENDLIST=FAILS,LOADFACSOFF=Y                   
                                                                                
ENTRY    DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO10**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JNZ   ENTRY02                                                          
         L     R9,LP_ABLK1                                                      
         ICM   R8,15,RSVRSAVE      R8=A(Save area)                              
         J     ENTRY04                                                          
                                                                                
ENTRY02  L     R9,RSVRSAVE                                                      
         ST    R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(Global w/s)                             
         XR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
         USING SAVED,R8            R8=A(Save w/s)                               
         MVC   MASTC,RMASTC        Set A(MASTC)                                 
         MVC   APRINTER,RPRINTER                                                
         ST    R6,ARUNFACS                                                      
         MVC   VSMTP,RSMTP         Save A(SMTP)                                 
                                                                                
ENTRY04  ST    R8,LP_ABLK2                                                      
         MVC   WRKBLKR,RWRKBLKR    Set A(FACWRK WRKIO block)                    
         ST    RE,SRVRRELO         Save program relocation factor               
         MVC   RUNMODE,RUNPMODE    Set calling mode                             
         DROP  R6,R7                                                            
                                                                                
         LARL  RF,MMI              Set A(master map index)                      
         ST    RF,LP_ANDX                                                       
         J     ENTRY06                                                          
                                                                                
MMI      LKMMI H,ACCSYSQ                                                        
         LKMMI UO,A#WRKUP,O#SRVR2B,(*,WRKUPLIT)                                 
         LKMMI UO,A#TLUPL,O#SRVR1E,(*,TLUPLLIT)                                 
         LKMMI UO,A#ESTES,O#SRVR10,(*,ESTESLIT)                                 
         LKMMI UO,A#OESI,O#SRVR2F,(*,ORDESLIT)                                  
         LKMMI E                                                                
                                                                                
WRKUPLIT DC    C'Worker File Upload'                                            
TLUPLLIT DC    C'Time Line Upload'                                              
ESTESLIT DC    C'Estimate Electronic Signature Upload'                          
ORDESLIT DC    C'Order Electronic Signature Upload'                             
                                                                                
ENTRY06  DS    0H                                                               
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
                                                                                
***********************************************************************         
* Handle DDLINK/RUNNER modes                                          *         
***********************************************************************         
                                                                                
         CLI   RUNMODE,RRUNSTRQ    Test 'First for run' mode                    
         JE    SRUN                                                             
         CLI   RUNMODE,RINIREQQ    Test 'Initialise' mode                       
         JE    SREQ                                                             
         CLI   RUNMODE,RVALREQQ    Test 'Validate record' mode                  
         JE    UPLD                                                             
         CLI   RUNMODE,RRUNREQQ    Test 'Run request' mode                      
         JE    UPLD                                                             
         CLI   RUNMODE,RRUNENDQ    Test 'Last for request' mode                 
         JE    EREQ                                                             
         J     EXITY               Ignore any other modes                       
                                                                                
***********************************************************************         
* Handle 'First for run' (once only off-line) mode                    *         
***********************************************************************         
                                                                                
SRUN     LA    R0,SAVED            Clear sacred storage                         
         LHI   R1,SAVEVAR-SAVED                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,LP_ACOM                                                       
         MVC   PROTOFF,CPROTOFF-COMFACSD(RE)                                    
         MVC   PROTON,CPROTON-COMFACSD(RE)                                      
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY                                                            
         L     RF,LP_ACOM          Load subsidiary phases & initialise          
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),('O#ROU1',0),0,0                                       
         MVC   AROUT1,0(R1)        Set A(Routine overlay 1)                     
         MVC   LP_AUIR1,AROUT1                                                  
         GOTOR (RF),(R1),('O#ROU2',0),0,0                                       
         MVC   AROUT2,0(R1)        Set A(Routine overlay 2)                     
         MVC   LP_AUIR2,AROUT2                                                  
         LA    R0,LP_D                                                          
         ST    R0,ALP              Set A(LP_D) in global w/s                    
         MVC   ACOMFACS,LP_ACOM    Set A(COMFACS)                               
         MVC   ATWA,LP_ATWA        Set A(TWA)                                   
         GOTOR (#WRKINI,AWRKINI)   Initialise working storage                   
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Handle 'First for request' (before first upload record) mode        *         
***********************************************************************         
                                                                                
SREQ     LA    R0,SAVEVAR                                                       
         LHI   R1,SAVEVARL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   TWAMODE,0           Set no errors encountered                    
         GOTO1 VDICTATE,DMCB,C'LL  ',DCDICTL,DSDICTL                            
                                                                                
* any other init needs to go here                                               
                                                                                
         L     RF,LP_ACOM          Extract A(LINKIO) from COMFACS               
         MVC   AALINKIO,CLINKIO-COMFACSD(RF)                                    
         MVC   AALIOB,LP_ALIOB     Set A(LIOB)                                  
                                                                                
         TM    LP_FLAG2,LP_FMQIM   If MQ then set agency                        
         JZ    SREQ02                                                           
         L     RE,MASTC            Set I/O trace option                         
         MVC   MCIDAGYB-MASTD(L'MCIDAGYB,RE),LP_AGYB                            
                                                                                
* one-time initialisation triggers for utilities - IMPORTANT                    
SREQ02   MVI   RUNINDS,0                                                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    SREQ04                                                           
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0   open acc files              
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
                                                                                
         L     RF,MASTC            Set I/O trace option                         
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         TM    LP_INDS,LP_IGLOB    Update - test all systems go                 
         JNO   *+2                 Primary system not open for update           
                                                                                
         OI    LP_INDS,LP_IRECV    Call recovery if abend                       
                                                                                
SREQ04   GOTOR PROTOFF             Turn off storage protection                  
         LA    R0,LP_D                                                          
         ST    R0,LLP              Save A(LP_D) locally                         
         GOTOR PROTON              Turn on storage protection                   
                                                                                
         MVC   ACOMFACS,LP_ACOM    Point to real copy of COMFACS                
                                                                                
         MVI   GIND2,GI2EEST       Set want estimate values returned            
         GOTOR (#CPYINI,ACPYINI)   Initialise company values                    
         USING CPYELD,SCPYEL                                                    
                                                                                
         J     EXITY                                                            
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
         DS    0H                                                               
                                                                                
***********************************************************************         
* Handle 'Run request' (upload a record) mode                         *         
***********************************************************************         
                                                                                
UPLD     LA    RF,RECTAB                                                        
         USING RECTABD,RF          RF=A(Record map table)                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   Look up record map code                      
         JE    UPLD02                                                           
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         J     *+2                 (No RECTAB entry)                            
                                                                                
UPLD02   MVC   RECTYPE,RECTTYPE    Set internal record type                     
         DROP  RF                                                               
                                                                                
         CLI   RECTYPE,RECESTQ     Test estimate status record                  
         JNE   UPLD04                                                           
         GOTOR UEST                                                             
         J     EXIT                                                             
                                                                                
UPLD04   DS    0H                                                               
         J     *+2                 (Invalid record)                             
         DROP  RB                                                               
                                                                                
***********************************************************************         
* Process Estimate upload record                                      *         
***********************************************************************         
                                                                                
UEST     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   CURRPIN,CCTPID      PIN retrieval                                
         TM    LP_FLAG2,LP_FMQIM                                                
         JZ    UESTRUN                                                          
         DS    0H                  Field 19: LP_SECNO (if set)                  
         L     RF,LP_ASECD         Field 29: SECOPASS from Secret block         
         MVC   CURRPIN,SECOPASS-SECD(RF)                                        
         MVC   CCTPID,CURRPIN                                                   
                                                                                
UESTRUN  CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    VALEST                                                           
         CLI   RUNMODE,RRUNREQQ    Test 'Run request' mode                      
         JNE   *+2                                                              
         TM    LP_FLAG2,LP_FMQIM   If MQ then go to update straight             
         JNZ   UPDEST                                                           
***      J     VALEST                                                           
                                                                                
*** Validate estimate request data ***                                          
                                                                                
VALEST   DS    0H                                                               
                                                                                
         NI    TWAMODE,FFQ-(TWAMERP+TWAMEDP)                                    
         XC    DV_VALS(DV_VALSL),DV_VALS                                        
                                                                                
         GOTOR VDATCON,DMCB,(15,0),(1,DV_TODAY)                                 
                                                                                
         MVC   TEMP2(L'CURRPIN),CURRPIN                                         
         GOTOR (#GETPID,AGETPID)                                                
         JE    VEST02                                                           
         LHI   RF,AE$INPIN                                                      
         J     VESTERR                                                          
                                                                                
VEST02   GOTOR (#GETPIN,AGETPIN)                                                
         JE    VEST04                                                           
         LHI   RF,AE$INPID                                                      
         J     VESTERR                                                          
                                                                                
VEST04   MVC   DV_MAILA,APPEMAIL                                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   VEST06                                                           
         GOTO1 VGETFACT,DMCB,(X'80',DV_SYSTM),F#SSYSNA                          
         J     VEST08                                                           
                                                                                
         USING RUNPARMD,R1                                                      
VEST06   L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         XR    RE,RE                                                            
         ICM   RE,B'0111',RUNPARUN                                              
         USING RUNFACSD,RE         R4=A(RUNFACS)                                
         ICM   RF,B'1111',RMASTC                                                
         ICM   RE,B'1111',MCSSB-MASTD(RF)                                       
         MVC   DV_SYSTM,SSODSPAC-SSOOFF(RE)                                     
         DROP  R1,RE                                                            
                                                                                
VEST08   CLI   DV_SYSTM,LCSCQ      Set fix for LCSC for testing                 
         JNE   VEST10                                                           
                                                                                
         MVC   DV_MAILA,SPACES                                                  
         MVC   DV_MAILA(30),=CL30'TKLUTH@MEDIAOCEAN.COM'                        
                                                                                
         USING EGNPASD,R2                                                       
VEST10   LA    R2,IOKEY            read for estimate via passive                
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,CUXCPY                                                   
         MVC   EGNPNUM,ES_GLNUM                                                 
                                                                                
         MVC   CSVKEY1,EGNPAS                                                   
                                                                                
         MVC   DV_RETID,ES_IDNUM                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JNE   *+2                                                              
                                                                                
         CLC   CSVKEY1(EGNPCLI-EGNPASD),EGNPAS                                  
         JE    VEST12                                                           
                                                                                
         LHI   RF,AE$ESTNF                                                      
         J     VESTERR                                                          
                                                                                
VEST12   MVC   CSVKEY1,EGNPAS      save estimate passive                        
                                                                                
         CLC   EGNPSOFF,SPACES     validate office if present                   
         JNH   VEST14                                                           
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,EGNPSOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         DROP  R1                                                               
         JE    VEST14                                                           
                                                                                
         LHI   RF,AE$SECLK                                                      
         J     VESTERR                                                          
                                                                                
VEST14   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
                                                                                
         USING ESTRECD,R2                                                       
         L     R2,AIO2                                                          
         MVC   DV_PEKEY,ESTKEY     save estimate prime key                      
                                                                                
         USING EMDELD,R3                                                        
         LA    R3,ESTRFST-ESTRECD(R2)                                           
         CLI   EMDEL,EMDELQ                                                     
         JNE   *+2                                                              
                                                                                
         XOUT  EMDIDN,FULL1,2      validate ID number                           
         MVC   DV_NEWID,EFFS                                                    
         CLC   ES_IDNUM,FULL1                                                   
         JE    VEST16                                                           
                                                                                
         LHI   RF,AE$EABOU                                                      
         J     VESTERR                                                          
                                                                                
VEST16   XR    RE,RE                                                            
         ICM   RE,B'0011',EMDIDN                                                
         AHI   RE,1                                                             
         STCM  RE,B'0011',DV_NEWID                                              
                                                                                
         CLI   ES_NSTAT,SPACEQ     validate status                              
         JH    VEST18                                                           
         LHI   RF,AE$MISTA                                                      
         J     VESTERR                                                          
                                                                                
VEST18   MVC   DV_OGAPS,EMDGSTAT                                                
         MVI   DV_NKSTA,ESTKCAPP                                                
         MVI   DV_NGAPS,EMDGAPP                                                 
         CLI   ES_NSTAT,ES_APPRQ                                                
         JE    VEST20                                                           
         MVI   DV_NKSTA,ESTKREJE                                                
         MVI   DV_NGAPS,EMDGREJ                                                 
         CLI   ES_NSTAT,ES_RJCTQ                                                
         JE    VEST20                                                           
         LHI   RF,AE$INTST                                                      
         J     VESTERR                                                          
                                                                                
VEST20   LARL  R1,SCVTAB           test valid status change                     
         TM    G#OFSTA2,OFFSIAEQ                                                
         JZ    VEST22                                                           
         LARL  R1,SC2TAB                                                        
                                                                                
VEST22   CLI   0(R1),FF            EOT                                          
         JE    *+2                                                              
         CLC   DV_NKSTA,0(R1)      found new status entry?                      
         JE    VEST24                                                           
         AHI   R1,SCVTABQ                                                       
         J     VEST22                                                           
                                                                                
VEST24   CLC   ESTRSTA1,1(R1)      ensure old status valid for new              
         JE    VEST26                                                           
         CLC   ESTRSTA1,2(R1)                                                   
         JE    VEST26                                                           
         CLC   ESTRSTA1,3(R1)                                                   
         JE    VEST26                                                           
         CLC   ESTRSTA1,4(R1)                                                   
         JE    VEST26                                                           
                                                                                
         LHI   RF,AE$INFST                                                      
         J     VESTERR                                                          
                                                                                
VEST26   XR    RF,RF               comments required on reject                  
         ICM   RF,B'0111',ES_COMMM                                              
         JZ    VEST28                                                           
         XR    R0,R0                                                            
         ICM   R0,B'0011',LW_NUMN-LW_D(RF)                                      
         JNZ   VEST30                                                           
                                                                                
VEST28   CLI   ES_NSTAT,ES_RJCTQ                                                
         JNE   VEST30                                                           
                                                                                
         LHI   RF,AE$MISDE         (not ideal but nothing else found)           
         J     VESTERR                                                          
                                                                                
VEST30   CLC   ES_AUTHN,SPACES     validate authorizer                          
         JH    VEST32                                                           
         LHI   RF,AE$MIPER                                                      
         J     VESTERR                                                          
                                                                                
VEST32   XOUT  DV_NEWID,DV_RETID,2 all validation done so return data           
         MVI   DV_INDIC,YESQ                                                    
                                                                                
         CLI   RUNMODE,RRUNREQQ                                                 
         JE    UPDEST              now do updates                               
                                                                                
                                                                                
         LARL  RF,OMRET            Return details                               
         ST    RF,LP_AOMAP                                                      
         GOTOR LP_APUTO,LP_D                                                    
                                                                                
         LA    R0,ES_VALS          Clear all request values (required           
         LHI   R1,ES_DLNQ          for offline)                                 
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         J     UEXITY                                                           
                                                                                
VESTERR  CLI   RUNMODE,RRUNREQQ                                                 
         JE    *+2                 die if in run mode                           
                                                                                
         STCM  RF,B'0011',ROUERRV                                               
         OI    TWAMODE,TWAMERP+TWAMEDP+TWAMPER                                  
         MVC   DV_ERROR,ROUERRV                                                 
         MVI   DV_INDIC,NOQ                                                     
                                                                                
                                                                                
         USING GETTXTD,R1          Build error text string in ELEMENT           
         LA    R1,DMCB                                                          
         XC    GTBLOCK,GTBLOCK     Resolve error text                           
         MVI   GTMAXL,L'DV_ERMSG                                                
         MVC   GTMSGNO,ROUERRV                                                  
         LA    RE,DV_ERMSG                                                      
         MVC   DV_ERMSG,SPACES                                                  
         STCM  RE,7,GTAOUT                                                      
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         DROP  R1                                                               
         GOTOR VGETTXT,(R1)                                                     
                                                                                
         LARL  RF,OMRET            Return details                               
         ST    RF,LP_AOMAP                                                      
         GOTOR LP_APUTO,LP_D                                                    
                                                                                
         LA    R0,ES_VALS          Clear all request values (required           
         LHI   R1,ES_DLNQ          for offline)                                 
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    LP_FLAG2,LP_FMQIM   If MQ then exit Y                            
         JNZ   UEXITY                                                           
                                                                                
         J     UEXITN                                                           
                                                                                
*** Update estimate with request data ***                                       
                                                                                
UPDEST   DS    0H                                                               
                                                                                
         GOTOR VDATAMGR,DMCB,DMCOMMIT,0                                         
                                                                                
*        XC    DV_VALS(DV_VALSL),DV_VALS                                        
*        ICM   RE,15,ES_AHEAD      Test data passed and copy it                 
*        JZ    *+2                                                              
*        LA    R0,DV_VALS                                                       
*        LHI   R1,DV_VALSL                                                      
*        AHI   RE,LW_LN1Q                                                       
*        LR    RF,R1                                                            
*        MVCL  R0,RE                                                            
                                                                                
         GOTOR SETTIME                                                          
                                                                                
         GOTOR TRACEIT             Trace request                                
                                                                                
         OC    DV_NEWID,DV_NEWID                                                
         JZ    *+2                 (MQ SAVED not intact)                        
                                                                                
         OC    DV_ERROR,DV_ERROR                                                
***      JNZ   *+2                 (should never get here)                      
         JZ    UEST02                                                           
         TM    LP_FLAG2,LP_FMQIM   If MQ then ok else die                       
         JNZ   UEXITY                                                           
         J     *+2                                                              
                                                                                
UEST02   LA    R2,IOKEY            get estimate for update                      
         XC    IOKEY,IOKEY                                                      
         MVC   ESTKEY,DV_PEKEY                                                  
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R2,AIO2                                                          
         USING EMDELD,R3                                                        
         LA    R3,ESTRFST                                                       
         CLI   EMDEL,EMDELQ                                                     
         JNE   *+2                                                              
                                                                                
         XOUT  EMDIDN,FULL1,2      check ID number                              
         CLC   ES_IDNUM,FULL1                                                   
         JNE   *+2                 (updated elsewhere in between)               
                                                                                
         GOTOR DELPAS,ESTRECD      delete passives                              
                                                                                
         MVC   ESTRSTA1,DV_NKSTA   set new main status                          
         MVC   EMDGSTAT,DV_NGAPS   set new GAP status                           
         MVC   EMDIDN,DV_NEWID     set new ID number                            
                                                                                
         MVI   BYTE1,NOQ                                                        
                                                                                
         USING STCELD,R3                                                        
UEST10   LLC   R1,STCLN            remove applicable STC elements               
         AR    R3,R1                                                            
         CLI   STCEL,0                                                          
         JE    UEST14                                                           
         CLI   STCEL,STCELQ                                                     
         JNE   UEST10                                                           
         CLI   STCIND,STCIEST2                                                  
         JNE   UEST10                                                           
         CLI   STCETYP,STCEESG+STCECHG                                          
         JNE   UEST10                                                           
         CLI   STCETY2,STCSTATE                                                 
         JE    UEST12                                                           
         CLI   STCETY2,STCECCN                                                  
         JNE   UEST10                                                           
                                                                                
UEST12   MVI   STCEL,FF                                                         
         MVI   BYTE1,YESQ                                                       
         J     UEST10                                                           
                                                                                
UEST14   CLI   BYTE1,YESQ          if any found remove them                     
         JNE   UEST20                                                           
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',ACCMST),(X'FF',ESTRECD),0                      
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
UEST20   L     R4,AELEAREA         create new STC elements                      
         MVI   0(R4),0                                                          
         MVI   BYTE1,0             comments element sequence                    
         MVI   BYTE2,NOQ           record full indicator                        
                                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIEST2                                                  
         MVI   STCETYP,STCEESG+STCECHG                                          
         MVC   STCUSER,CUUSER                                                   
         MVC   STCPERS,CURRPIN                                                  
         MVC   STCDATE,DV_TODAY                                                 
         MVC   STCTIME,CURTIME                                                  
         MVI   STCETY2,STCECCN                                                  
         LA    R1,STCELN1Q                                                      
         LA    RE,ES_AUTHN+L'ES_AUTHN-1                                         
         LA    RF,L'ES_AUTHN                                                    
                                                                                
UEST22   CLI   0(RE),C' '                                                       
         JH    UEST24                                                           
         SHI   RE,1                                                             
         JCT   RF,UEST22                                                        
         J     *+2                                                              
                                                                                
UEST24   AR    R1,RF                                                            
         STC   R1,STCLN                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   STCECNAM(0),ES_AUTHN                                             
         EX    RF,0(RE)                                                         
         STC   RF,DV_AUTHL                                                      
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ESTRECD,STCELD,ADDEND,0                
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         LLC   R1,STCLN            save it to AELEAREA                          
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),STCELD                                                   
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         AR    R4,R1                                                            
         MVI   0(R4),0                                                          
                                                                                
UEST26   XR    R0,R0               Point to comments pool                       
         XR    R6,R6                                                            
         ICM   R6,B'0111',ES_COMMM                                              
         JZ    UEST28                                                           
         XR    R0,R0                                                            
         ICM   R0,B'0011',LW_NUMN-LW_D(R6)                                      
         JZ    UEST28                                                           
         LA    R6,LW_DATA2-LW_D(R6)  start of list                              
                                                                                
UEST28   LA    R3,ELEMENT          put status and comments                      
         XC    ELEMENT,ELEMENT                                                  
         MVI   STCEL,STCELQ                                                     
         MVI   STCIND,STCIEST2                                                  
         MVI   STCETYP,STCEESG+STCECHG                                          
         MVC   STCUSER,CUUSER                                                   
         MVC   STCPERS,CURRPIN                                                  
         MVC   STCDATE,DV_TODAY                                                 
         MVC   STCTIME,CURTIME                                                  
         MVI   STCETY2,STCSTATE                                                 
         MVC   STCELSTP,DV_OGAPS                                                
         MVC   STCELSTC,DV_NGAPS                                                
         LA    R1,STCELNEQ                                                      
         MVI   STCLN,STCELNEQ                                                   
         CHI   R0,0                any comments (left)?                         
         JE    UEST34                                                           
         LA    RE,L'STCECOMT-1(R6)                                              
         LA    RF,L'STCECOMT                                                    
                                                                                
UEST30   CLI   0(RE),C' '                                                       
         JH    UEST32                                                           
         SHI   RE,1                                                             
         JCT   RF,UEST30                                                        
         J     *+2                                                              
                                                                                
UEST32   AR    R1,RF                                                            
         AHI   R1,L'STCECIDN+L'STCECSEQ                                         
         STC   R1,STCLN                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   STCECOMT(0),0(R6)                                                
         EX    RF,0(RE)                                                         
         MVC   STCECIDN,DV_NEWID                                                
         MVC   STCECSEQ,BYTE1                                                   
         LLC   R1,BYTE1                                                         
         AHI   R1,1                                                             
         STC   R1,BYTE1                                                         
                                                                                
UEST34   CLI   BYTE2,YESQ          skip if record already full                  
         JE    UEST38                                                           
                                                                                
         XR    RE,RE               space on record?                             
         ICM   RE,B'0011',ESTRLEN                                               
         LLC   RF,STCLN                                                         
         AHI   RF,STCSPARE                                                      
         AR    RE,RF                                                            
         CHI   RE,MAXRECLN                                                      
         JL    UEST36              need to add new record                       
                                                                                
         MVI   BYTE2,YESQ          indicate record full                         
         J     UEST38                                                           
                                                                                
UEST36   GOTO1 VHELLO,DMCB,(C'P',ACCMST),ESTRECD,STCELD,ADDEND,0                
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
UEST38   LLC   R1,STCLN            save it to AELEAREA                          
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),STCELD                                                   
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         AR    R4,R1                                                            
         MVI   0(R4),0                                                          
                                                                                
         CHI   R0,0                any comments (left)?                         
         JE    UEST40                                                           
         AHI   R6,L'STCECOMT       next wmp entry                               
         JCT   R0,UEST28           if anything left                             
                                                                                
UEST40   DS    0H                  put main estimate record back now            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
KEY      USING ESTRECD,R1                                                       
         LA    R1,IOKEY                                                         
         MVC   KEY.ESTKSTA,ESTRSTA                                              
         MVC   MYIOADR,KEY.ESTKDA                                               
         DROP  KEY                                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JNE   *+2                                                              
                                                                                
         GOTOR ADDPAS,ESTRECD                                                   
         DROP  R3                                                               
                                                                                
* find latest audit record and check for sufficient space                       
                                                                                
UEST50   MVI   BYTE1,FFQ           (for AUDKSEQ)                                
                                                                                
         USING AUDRECD,R4                                                       
         LA    R4,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,ESTKCPY                                                  
         MVI   AUDKAUDT,AUDKEST                                                 
         LA    R6,AUDKECPJ                                                      
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R6),ESTKCLI                                                  
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         AR    R6,RF                                                            
         LLC   RE,PPROLEN                                                       
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R6),ESTKPRO                                                  
         EX    RE,0(R1)                                                         
         AHI   RE,1                                                             
         AR    R6,RE                                                            
         LHI   RF,L'ESTKJOB-1                                                   
         BASR  R1,0                                                             
         MVC   0(0,R6),ESTKJOB                                                  
         EX    RF,0(R1)                                                         
         OC    AUDKECPJ,SPACES                                                  
         MVC   AUDKELNO,ESTKLNO                                                 
         MVC   CSVKEY2,AUDKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     UEST54                                                           
                                                                                
UEST52   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
                                                                                
UEST54   JNE   *+2                                                              
         CLC   AUDKEY(AUDKSEQ-AUDRECD),CSVKEY2                                  
         JNE   UEST56                                                           
         MVC   BYTE1,AUDKSEQ       save current sequence number                 
         J     UEST52                                                           
                                                                                
UEST56   CLI   BYTE1,FFQ                                                        
         JE    *+2                 (no AUDRECD at all)                          
                                                                                
         USING STCELD,R6                                                        
         L     R6,AELEAREA         point to elements                            
                                                                                
         MVC   AUDKEY,CSVKEY2      get last AUDREC and see if space             
         MVC   AUDKSEQ,BYTE1                                                    
         L     R1,=AL4(IORDUP+IODIR+IO3)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO3)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R4,AIO3                                                          
         MVI   BYTE2,NOQ           no element added yet                         
         MVI   BYTE3,CHANGEQ                                                    
                                                                                
         L     R0,AIO4             copy AIO3 into AIO4 for DELPAS               
         LA    R1,IOLENQ                                                        
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
UEST58   XR    RE,RE                                                            
         ICM   RE,B'0011',AUDRLEN                                               
         LLC   RF,STCLN                                                         
         AR    RE,RF                                                            
         CHI   RE,MAXRECLN                                                      
         JNL   UEST60              need to add new record                       
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,STCELD,ADDEND,0                
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
         MVI   BYTE2,YESQ          added element to record                      
         LLC   RF,STCLN                                                         
         AR    R6,RF                                                            
         CLI   STCEL,0                                                          
         JNE   UEST58              next element                                 
                                                                                
UEST60   CLI   BYTE2,YESQ          resolve records                              
         JE    UEST62                                                           
         CLI   STCEL,0                                                          
         JE    UEST70                                                           
                                                                                
UEST62   CLI   BYTE3,CHANGEQ       need to put record back?                     
         JNE   UEST64                                                           
                                                                                
         L     R1,AIO4                                                          
         GOTOR DELPAS                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO3'                           
         JNE   *+2                                                              
                                                                                
         MVC   MYIOADR,IOKEY+AUDKDA-AUDRECD                                     
                                                                                
         GOTOR ADDPAS,AUDRECD                                                   
         J     UEST66                                                           
                                                                                
UEST64   CLI   BYTE3,ADDQ                                                       
         JNE   *+2                 (internal check)                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO3'                           
         JNE   *+2                                                              
                                                                                
         MVC   MYIOADR,IODA                                                     
                                                                                
         GOTOR ADDPAS,AUDRECD                                                   
                                                                                
UEST66   CLI   STCEL,0             anything left?                               
         JE    UEST70                                                           
         MVI   BYTE3,ADDQ          adding records                               
         MVI   BYTE2,NOQ           no element added yet                         
         L     R4,AIO3                                                          
         LR    RE,R4                                                            
         LA    RF,IOLENQ                                                        
         XCEF                                                                   
         MVC   AUDKEY,CSVKEY2                                                   
         LLC   R1,BYTE1                                                         
         AHI   R1,1                                                             
         STC   R1,BYTE1                                                         
         MVC   AUDKSEQ,BYTE1                                                    
***      MVC   AUDRSTAT,ESTRSTA    NSHE: not required anymore                   
***      MVC   AUDRSTA2,ESTRSTA2                                                
         LHI   R1,AUDRFST-AUDRECD+1                                             
         STCM  R1,B'0011',AUDRLEN                                               
         J     UEST58                                                           
                                                                                
UEST70   DS    0H                                                               
***      GOTOR VDATAMGR,DMCB,DMCOMMIT,0                                         
         DROP  R2,R4,R6                                                         
                                                                                
         CLC   DV_MAILA,SPACES     Any mail address set then send mail          
         JNH   UEST98              (copied from ACBRA20)                        
                                                                                
         GOTOR GETURL                                                           
                                                                                
         GOTOR SETEST                                                           
                                                                                
         XR    R2,R2               Number of mail comment lines                 
                                                                                
         XR    R6,R6               Point to comments pool                       
         ICM   R6,B'0111',ES_COMMM                                              
         JZ    UEST74                                                           
         XR    R0,R0                                                            
         ICM   R0,B'0011',LW_NUMN-LW_D(R6)                                      
         JZ    UEST74                                                           
         LA    R6,LW_DATA2-LW_D(R6)  start of list                              
         L     R3,AGENAXTN           build data block here                      
         XR    R2,R2                                                            
                                                                                
UEST72   MVC   0(L'STCECOMT,R3),0(R6)                                           
         AHI   R2,L'STCECOMT                                                    
         AHI   R3,L'STCECOMT                                                    
                                                                                
         AHI   R6,L'STCECOMT         next wmp entry                             
         JCT   R0,UEST72             if anything left                           
                                                                                
         ICM   RF,B'1111',LENPARM                                               
         LHI   R3,MAXLINQ                                                       
         GOTO1 VCHOPPER,DMCB,AGENAXTN,('MLLENQ',AGENAREA),('MLLENQ',R3)+        
               ,(RF),(R2)                                                       
         L     R2,8(R1)              # of lines from Chopper                    
                                                                                
UEST74   ST    R2,SAVECT                                                        
                                                                                
***      GOTO1 VSMTP,DMCB,('SMTPAINI',0)     initialise JESMAIL                 
         GOTO1 VSMTP,DMCB,('SMTPASLL',0)     set long lines                     
         GOTO1 VSMTP,DMCB,('SMTPHTMH',0)     set HTML header                    
         GOTO1 VSMTP,DMCB,('SMTPAFR2',0),(L'AUFROM,AUFROM)                      
                                                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'DV_MAILA),DV_MAILA                                        
         MVC   TEMP,SPACES                                                      
                                                                                
         MVC   TEXT1,TEXTSPCS                                                   
         LA    R3,TEXT1                                                         
                                                                                
         LLC   RF,DV_AUTHL                                                      
         BASR  RE,0                                                             
         MVC   0(0,R3),ES_AUTHN                                                 
         EX    RF,0(RE)                                                         
         AHI   RF,2                                                             
         AR    R3,RF                                                            
                                                                                
         LA    R1,HASAPP                                                        
         CLI   ES_NSTAT,ES_APPRQ                                                
         JE    UEST76                                                           
         LA    R1,HASREJ                                                        
         CLI   ES_NSTAT,ES_RJCTQ                                                
         JNE   *+2                                                              
                                                                                
UEST76   MVC   0(L'HASAPP,R3),0(R1)                                             
         AHI   R3,L'HASAPP+1                                                    
                                                                                
         MVC   0(L'ES_GLNUM,R3),ES_GLNUM                                        
         AHI   R3,L'ES_GLNUM+1                                                  
                                                                                
         LHI   R0,L'TEXT1                    pass mail to and title             
         GOTO1 VSMTP,DMCB,('SMTPAPRS',WORK),((R0),TEXT1)                        
                                                                                
         GOTOR VDATCON,DMCB,(5,0),(20,SVTODL)                                   
         L     RF,AEMS620E         Put year in copyright                        
         MVC   0(L'EMLS620E,RF),SVTODL                                          
                                                                                
         L     RF,AEMS280E         Move in message text                         
         MVC   0(80,RF),TEXTSPCS                                                
                                                                                
         LLC   R1,DV_AUTHL                                                      
         BASR  RE,0                                                             
         MVC   0(0,RF),ES_AUTHN                                                 
         EX    R1,0(RE)                                                         
         AHI   R1,2                                                             
         AR    RF,R1                                                            
                                                                                
         LA    RE,HASAPP                                                        
         CLI   ES_NSTAT,ES_APPRQ                                                
         JE    UEST77                                                           
         LA    RE,HASREJ                                                        
         CLI   ES_NSTAT,ES_RJCTQ                                                
         JNE   *+2                                                              
                                                                                
UEST77   MVC   0(L'HASAPP,RF),0(RE)                                             
                                                                                
         L     RF,AEMS283E                                                      
         MVC   0(L'LINKOP,RF),LINKOP                                            
                                                                                
         L     RF,AEMS284E         Move in URL                                  
         LA    RE,CURURL                                                        
         MVC   0(L'TEXT1,RF),0(RE)                                              
         L     RF,AEMS285E                                                      
         AHI   RE,L'TEXT1                                                       
         MVC   0(L'TEXT1,RF),0(RE)                                              
         L     RF,AEMS286E                                                      
         AHI   RE,L'TEXT1                                                       
         MVC   0(L'TEXT1,RF),0(RE)                                              
                                                                                
         L     RF,AEMS610E                                                      
*&&UK                                                                           
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOUK,RF),AC@MOUK                                          
         CLI   CUCTRY,CTRYGBR            UK address                             
         JE    UEST78                                                           
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MODE,RF),AC@MODE                                          
         CLI   CUCTRY,CTRYGER            German address                         
         JE    UEST78                                                           
*&&                                                                             
*&&US                                                                           
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AC@MOCA,RF),AC@MOCA                                          
         CLI   CUCTRY,CTRYCAN            Canadian address                       
         JE    UEST78                                                           
*&&                                                                             
         MVC   0(L'EMLS610E,RF),TEXTSPCS                                        
         MVC   0(L'AUADDR,RF),AUADDR Default is US address                      
                                                                                
UEST78   NI    AC@EMNL2,X'FF'-X'40'  Undo capitilisation                        
         NI    AC@EMNL5,X'FF'-X'40'                                             
         NI    AC@EMNL7,X'FF'-X'40'                                             
         CLI   CUCTRY,CTRYGER        EMNL8/EMNL9 not start of line              
         JE    UEST80                                                           
         NI    AC@EMNL8,X'FF'-X'40'                                             
         NI    AC@EMNL9,X'FF'-X'40'                                             
                                                                                
UEST80   L     RF,AEMALS1                                                       
         MVC   0(L'AC@EMNL1,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL1,RF),AC@EMNL1                                        
                                                                                
         L     RF,AEMALS2                                                       
         MVC   0(L'AC@EMNL2,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL2,RF),AC@EMNL2                                        
                                                                                
         L     RF,AEMALS3                                                       
         MVC   0(L'AC@EMNL3,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL3,RF),AC@EMNL3                                        
                                                                                
         L     RF,AEMALS4                                                       
         MVC   0(L'AC@EMNL4,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL4,RF),AC@EMNL4                                        
                                                                                
         L     RF,AEMALS5                                                       
         MVC   0(L'AC@EMNL5,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL5,RF),AC@EMNL5                                        
                                                                                
         L     RF,AEMALS6                                                       
         MVC   0(L'AC@EMNL6,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL6,RF),AC@EMNL6                                        
                                                                                
         L     RF,AEMALS7                                                       
         MVC   0(L'AC@EMNL7,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL7,RF),AC@EMNL7                                        
                                                                                
         L     RF,AEMALS8                                                       
         MVC   0(L'AC@EMNL8,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL8,RF),AC@EMNL8                                        
                                                                                
         L     RF,AEMALS9                                                       
         MVC   0(L'AC@EMNL9,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNL9,RF),AC@EMNL9                                        
                                                                                
         L     RF,AEMALSA                                                       
         MVC   0(L'AC@EMNLA,RF),TEXTSPCS                                        
         MVC   0(L'AC@EMNLA,RF),AC@EMNLA                                        
                                                                                
         L     R4,AEMS625E                 Find us here...                      
         MVC   0(L'EMLS625E,R4),TEXTSPCS                                        
         MVC   0(L'AC@FUSAT,R4),AC@FUSAT                                        
                                                                                
UEST82   LA    R2,EMLSUM                                                        
         MVI   TXTMODE,NOQ                                                      
                                                                                
UEST84   CLI   0(R2),FFQ           Loop through table outputting HTML           
         JE    UEST92                                                           
         LLC   RF,0(R2)                                                         
         SHI   RF,2                                                             
         CHI   RF,L'TEXT1                                                       
         JH    *+2                                                              
                                                                                
         C     R2,AEMS289          If we get to here loop to pass out           
         JNE   UEST88              from comment block                           
                                                                                
         MVI   TXTMODE,NOQ                                                      
         L     R4,SAVECT           Get number of entries                        
         CHI   R4,0                No entries?                                  
         JNH   UEST90                                                           
                                                                                
         L     R3,AGENAREA         Point to data and move out in a loop         
         MVI   TXTMODE,YESQ                                                     
                                                                                
UEST86   MVC   TEXT1,TEXTSPCS                                                   
         MVC   TEXT1,0(R3)                                                      
                                                                                
         GOTO1 VSQUASH,DMCB,TEXT1,L'TEXT1                                       
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
                                                                                
         AHI   R3,L'TEXT1                                                       
         JCT   R4,UEST86                                                        
                                                                                
         MVI   TXTMODE,NOQ                                                      
         J     UEST90                                                           
                                                                                
UEST88   DS    0H                                                               
         MVI   TEXT1,C' '                                                       
         MVC   TEXT1,TEXTSPCS                                                   
         BASR  RE,0                                                             
         MVC   TEXT1(0),1(R2)                                                   
         EX    RF,0(RE)                                                         
         CLC   TEXT1,TEXTSPCS            Line all spaces?                       
         JNH   UEST90                                                           
                                                                                
         GOTO1 VSQUASH,DMCB,TEXT1,L'TEXT1                                       
         GOTO1 VSMTP,DMCB,('SMTPAPTL',TEXT1)                                    
                                                                                
UEST90   LLC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     UEST84                                                           
                                                                                
UEST92   DS    0H                                                               
                                                                                
UEST94   GOTO1 VSMTP,DMCB,('SMTPASND',0)     send email                         
         GOTO1 VSMTP,DMCB,('SMTPAEND',0)     detach from JESMAIL                
         GOTO1 VSMTP,DMCB,('SMTPAINI',0)     initialise JESMAIL                 
                                                                                
UEST98   DS    0H                                                               
         J     UEXITY                                                           
                                                                                
MLLENQ   EQU   L'TEMP+L'TEMP2      =160                                         
MAXLINQ  EQU   25                                                               
                                                                                
DMCOMMIT DC    C'COMMIT  '         Recovery checkpoint command                  
AUFROM   DC    C'AuraNotifications<AuraNotifications@mediaocean.com>'           
HASAPP   DC    C'has approved estimate number'                                  
HASREJ   DC    C'has rejected estimate number'                                  
AUTHOR   DC    C'Authorized by:'                                                
LENPARM  DC    C'LEN='                                                          
AUADDR   DC    C'MEDIAOCEAN | 120 BROADWAY | NEW YORK, NY 10271'                
                                                                                
AEMS280E DC    A(EMLS280E)                                                      
AEMS283E DC    A(EMLS283E)                                                      
AEMS284E DC    A(EMLS284E)                                                      
AEMS285E DC    A(EMLS285E)                                                      
AEMS286E DC    A(EMLS286E)                                                      
AEMS289  DC    A(EMLS289)                                                       
AEMS610E DC    A(EMLS610E)                                                      
AEMS620E DC    A(EMLS620E)                                                      
AEMS625E DC    A(EMLS625E)                                                      
AEMALS1  DC    A(EMAILS1)                                                       
AEMALS2  DC    A(EMAILS2)                                                       
AEMALS3  DC    A(EMAILS3)                                                       
AEMALS4  DC    A(EMAILS4)                                                       
AEMALS5  DC    A(EMAILS5)                                                       
AEMALS6  DC    A(EMAILS6)                                                       
AEMALS7  DC    A(EMAILS7)                                                       
AEMALS8  DC    A(EMAILS8)                                                       
AEMALS9  DC    A(EMAILS9)                                                       
AEMALSA  DC    A(EMAILSA)                                                       
                                                                                
TEXTSPCS DC    CL160' '                                                         
                                                                                
         DS    0H                                                               
                                                                                
OMRET    LKOUT H                                                                
         LKOUT R,R#ESTES                                                        
Indic    LKOUT C,ES#IND,(D,B#SAVED,DV_INDIC),CHAR,ND=Y                          
IDNo     LKOUT C,ES#IDNO,(D,B#SAVED,DV_RETID),CHAR,ND=Y                         
Error    LKOUT C,ES#ERROR,(D,B#SAVED,DV_ERMSG),CHAR,ND=Y                        
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
         DS    0H                                                               
                                                                                
***********************************************************************         
* Exit handling for all upload records                                *         
***********************************************************************         
                                                                                
UEXITY   DS    0H                                                               
         J     EXITY               Exit back to DDLINK                          
                                                                                
UEXITN   DS    0H                                                               
         J     EXITN               Exit back to DDLINK                          
                                                                                
***********************************************************************         
* Handle 'Last for request' mode (only passed when running live)      *         
***********************************************************************         
                                                                                
EREQ     DS    0H                                                               
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Estimate electronic signature approve/reject upload                 *         
***********************************************************************         
                                                                                
EESREC   LKREQ H,A#ESTES,NEWREC=Y                                               
Est#Glo  LKREQ F,1,(D,B#SAVED,ES_GLNUM),CHAR,TEXT=(*,ESTNLIT)                   
EstIDNo  LKREQ F,ES#IDNO,(D,B#SAVED,ES_IDNUM),CHAR,TEXT=(*,EIDNLIT)             
EStatus  LKREQ F,3,(D,B#SAVED,ES_NSTAT),CHAR,TEXT=(*,ESTALIT)                   
EAuthor  LKREQ F,4,(D,B#SAVED,ES_AUTHN),CHAR,TEXT=(*,EAUTLIT),         +        
               LOWERCASE=Y                                                      
EComms   LKREQ F,5,(I,B#SAVED,ES_COMMI),CHAR,TEXT=(*,ECOMLIT),         +        
               LIST=F,OLEN=L'STCECOMT,SORT=NO,LOWERCASE=Y                       
* does not work under MQ                                                        
*  extension to pass header vals to offine run                                  
*ataVals LKREQ F,ES#DV,(I,B#SAVED,ES_AHEAD),CHAR,TEXT=(*,DATALIT),              
*              OLEN=1                                                           
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* End of request map tables                                          *          
***********************************************************************         
                                                                                
         LKMAP X                                                                
                                                                                
***********************************************************************         
* Trace request                                                       *         
***********************************************************************         
                                                                                
SETTIME  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         OILL  GR0,X'000C'         'OR' in packed sign value                    
         STCM  R0,B'1111',CURTIME                                               
***US*&& AP    CURTIME,=P'60000'   Adjust to real (EST) time in US              
***US*&& ICM   R0,B'1111',CURTIME  and update R0                                
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Trace request                                                       *         
***********************************************************************         
                                                                                
TRACEIT  NTR1  BASE=*,LABEL=*      Trace in SYSPRINT (offline)                  
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JZ    TRACEITX                                                         
                                                                                
         L     RF,ARUNFACS         Get A(RUNFACS)                               
         L     R3,RCPRINT-RUNFACSD(RF)                                          
         AHI   R3,P_P-P_DPRINT                                                  
         USING PTRACED,R3                                                       
                                                                                
         XR    R2,R2                                                            
         ICM   R2,3,LP_QMAPN                                                    
                                                                                
         LAY   RE,TAESTES                                                       
         MVC   PTTWHO,0(RE)                                                     
         CHI   R2,A#ESTES                                                       
         JNE   *+2                                                              
                                                                                
         TM    LP_FLAG2,LP_FMQIM                                                
         JZ    TRACEIT2                                                         
         LAY   RE,TMQ                                                           
         MVC   PTMQ,0(RE)                                                       
                                                                                
TRACEIT2 LAY   RE,TTIMED                                                        
         MVC   PTTTIME,0(RE)                                                    
         THMS                                                                   
         ST    R1,FULL1                                                         
         UNPK  WORK(6),FULL1                                                    
         MVC   PTHH,WORK           Set HH:MM:SS                                 
         MVI   PTCO1,C':'                                                       
         MVC   PTMM,WORK+2                                                      
         MVI   PTCO2,C':'                                                       
         OI    WORK+5,X'F0'                                                     
         MVC   PTSS,WORK+4                                                      
                                                                                
         LAY   RE,TALPHAID                                                      
         MVC   PTTALPH,0(RE)                                                    
         MVC   PTALPH,CUAALF                                                    
         LAY   RE,TUSERPID                                                      
         MVC   PTTUSER,0(RE)                                                    
         XOUT  CURRPIN,PTUSER,2                                                 
                                                                                
         LAY   RE,TDETS                                                         
         MVC   PTFIELD(L'TDETS),0(RE)                                           
         LA    R4,PFIELD                                                        
                                                                                
         MVC   0(L'ES_GLNUM,R4),ES_GLNUM                                        
         AHI   R4,L'ES_GLNUM+1                                                  
         MVC   0(L'ES_IDNUM,R4),ES_IDNUM                                        
         AHI   R4,L'ES_IDNUM+1                                                  
         MVC   0(L'ES_NSTAT,R4),ES_NSTAT                                        
         AHI   R4,L'ES_NSTAT+1                                                  
         MVC   0(L'ES_AUTHN,R4),ES_AUTHN                                        
         AHI   R4,L'ES_AUTHN+1                                                  
         XR    R6,R6               Point to comments pool                       
         XR    RF,RF                                                            
         ICM   RF,B'0111',ES_COMMM                                              
         JZ    TRACEIT4                                                         
         XR    R6,R6                                                            
         ICM   R6,B'0011',LW_NUMN-LW_D(RF)                                      
                                                                                
TRACEIT4 EDITR (R6),(3,0(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                         
         AR    R4,R0                                                            
         AHI   R4,1                                                             
         MVC   0(L'DV_RETID,R4),DV_RETID                                        
         AHI   R4,L'DV_RETID+1                                                  
         CLC   DV_MAILA,SPACES                                                  
         JNH   TRACEIT6                                                         
         LAY   RE,TMAIL                                                         
         MVC   0(L'TMAIL,R4),0(RE)                                              
         AHI   R4,L'TMAIL+1                                                     
                                                                                
TRACEIT6 GOTO1 APRINTER                                                         
                                                                                
         OC    DV_ERROR,DV_ERROR                                                
         JZ    TRACEITX                                                         
         LAY   RE,TERROR                                                        
         MVC   PTTTIME(L'TERROR),0(RE)                                          
         EDITR (B2,DV_ERROR),(4,PTHH),0                                         
                                                                                
         GOTO1 APRINTER                                                         
                                                                                
TRACEITX J     EXITY                                                            
         DROP  R3                                                               
                                                                                
TTIMED   DC    C'Time:'                                                         
TMQ      DC    C'MQ'                                                            
TMAIL    DC    C'Mail'                                                          
TERROR   DC    C'Error:'                                                        
TALPHAID DC    C'AlphaID:'                                                      
TUSERPID DC    C'PID:'                                                          
TAESTES  DC    C'A#ESTES'                                                       
TDETS    DC    C'Details:'                                                      
                                                                                
***********************************************************************         
* Delete passives for 'old record'                                    *         
***********************************************************************         
                                                                                
         USING CPTRBLK,R2                                                       
DELPAS   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         ST    R1,AMYIO                                                         
                                                                                
         LA    R2,ELEMENT                                                       
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
                                                                                
         MVC   LDGLVALN(4),ONERL1L                                              
         GOTO1 VPADDLE,DMCB,(C'D',AMYIO),(C'K',CPTRBLK),0,0,ACOMFACS            
                                                                                
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Add passives for 'new record'                                       *         
***********************************************************************         
                                                                                
         USING CPTRBLK,R2                                                       
ADDPAS   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         ST    R1,AMYIO                                                         
                                                                                
         LA    R2,ELEMENT                                                       
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
                                                                                
         MVC   LDGLVALN(4),ONERL1L                                              
         GOTO1 VPADDLE,DMCB,(C'A',AMYIO),CPTRBLK,MYIOADR,0,ACOMFACS             
                                                                                
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Get URL settings                                                    *         
***********************************************************************         
                                                                                
GETURL   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R2,FEDURL                                                        
         LR    R0,R2                                                            
         LHI   R1,L'FEDURL                                                      
         XR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    SCPXEL+CPXSTAT4-CPXELD,CPXFEDAT                                  
         JZ    GURL06                                                           
                                                                                
         USING CPYRECD,R2                                                       
         LA    R2,IOKEY            Read for federated URL                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CUXCPY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   *+2                                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING CPYRECD,R2                                                       
         L     R2,AIO1                                                          
         LA    R3,CPYRFST                                                       
                                                                                
         USING FFTELD,R3                                                        
GURL02   CLI   FFTEL,0                                                          
         JE    GURL06                                                           
         CLI   FFTEL,FFTELQ                                                     
         JNE   GURL04                                                           
         CLI   FFTTYPE,FFTTFURL                                                 
         JNE   GURL04                                                           
         LLC   RF,FFTLN                                                         
         SHI   RF,1+FFTDATA-FFTELD                                              
         MVC   FEDURL(0),FFTDATA                                                
         EX    RF,*-6                                                           
         J     GURL06                                                           
                                                                                
GURL04   LLC   R0,FFTLN                                                         
         AR    R3,R0                                                            
         J     GURL02                                                           
         DROP  R2,R3                                                            
                                                                                
GURL06   LA    R2,CURURL           Clear out url                                
         LR    R0,R2                                                            
         LHI   R1,L'CURURL                                                      
         XR    RE,RE                                                            
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING AGYURLD,R3                                                       
         LARL  R3,AGYURL           PULL URL FROM TABLE                          
                                                                                
GURL08   CLI   AGUSE,0             LIVE OR TEST                                 
         JE    GURL10              LIVE                                         
         CLC   AGUSE,LP_SENO       TEST - CHECK SE NUMBER                       
         JE    GURL14                                                           
         J     GURL12                                                           
                                                                                
GURL10   CLI   AGUAA,0             END OF TABLE, USE DEFAULT                    
         JE    GURL14                                                           
         CLC   AGUAA,CUAALF        MATCH ON AGENCY ALPHA                        
         JE    GURL14                                                           
                                                                                
GURL12   AHI   R3,AGYURLQ          NO MATCH, NEXT ENTRY                         
         J     GURL08                                                           
                                                                                
GURL14   LA    RF,AGUHTT                                                        
         LHI   RE,L'AGUHTT-1                                                    
         CLI   AGUFED,YESQ         Is this a system with a url prefix?          
         JNE   GURL16                                                           
         TM    SCPXEL+CPXSTATA-CPXELD,CPXFEDAT                                  
         JZ    GURL16              Is federated auth in use?                    
         LA    RF,HTTP             Always http for federated urls               
         LHI   RE,L'HTTP-1                                                      
                                                                                
GURL16   BASR  R1,0                                                             
         MVC   0(0,R2),0(RF)                                                    
         EX    RE,0(R1)                                                         
         AHI   R2,L'AGUHTT                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         TM    SCPXEL+CPXSTATA-CPXELD,CPXFEDAT                                  
         JNZ   GURL18              Is federated auth in use?                    
         CLC   AGUENV,SPACES       Any environment?                             
         JNH   GURL18                                                           
         MVC   0(L'AGUENV,R2),AGUENV                                            
         AHI   R2,L'AGUENV                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
                                                                                
GURL18   CLI   AGUFED,YESQ         Is this a system with a url prefix?          
         JNE   GURL20                                                           
         TM    SCPXEL+CPXSTATA-CPXELD,CPXFEDAT                                  
         JZ    GURL20              Is federated auth in use?                    
         MVC   0(L'AGUFEDP,R2),AGUFEDP                                          
         AHI   R2,L'AGUFEDP                                                     
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         MVC   0(L'FEDURL,R2),FEDURL                                            
         AHI   R2,L'FEDURL                                                      
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
         MVC   0(L'AGUFEDS,R2),AGUFEDS                                          
         AHI   R2,L'AGUFEDS                                                     
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         AHI   R2,1                                                             
                                                                                
GURL20   MVC   0(L'AGUURL2,R2),AGUURL2  Aura                                    
                                                                                
GETURLX  DS    0H                                                               
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Set estimate display URL                                            *         
***********************************************************************         
                                                                                
SETEST   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R2,CURURL+L'CURURL-1                                             
         LHI   R3,L'CURURL-1       Find end of URL                              
                                                                                
SEST02   CLI   0(R2),SPACEQ                                                     
         JH    SEST04                                                           
         SHI   R2,1                                                             
         JCT   R3,SEST02                                                        
         J     *+2                 empty?                                       
                                                                                
SEST04   AHI   R2,1                                                             
         AHI   R3,1                                                             
                                                                                
         LA    R4,ESTDIS           move in Estimate/Display                     
         MVC   0(ESTDLNQ,R2),0(R4)                                              
         AHI   R3,ESTDLNQ                                                       
         AHI   R2,ESTNUMO-ESTDIS                                                
         MVC   0(L'ES_GLNUM,R2),ES_GLNUM                                        
         AHI   R2,L'ES_GLNUM                                                    
                                                                                
         MVC   0(L'LINKCB,R2),LINKCB                                            
         AHI   R2,L'LINKCB                                                      
         AHI   R3,L'LINKCB                                                      
                                                                                
         MVC   0(L'ES_GLNUM,R2),ES_GLNUM                                        
         AHI   R2,L'ES_GLNUM                                                    
         AHI   R3,L'ES_GLNUM                                                    
                                                                                
         MVC   0(L'LINKCL,R2),LINKCL                                            
         AHI   R2,L'LINKCL                                                      
         AHI   R3,L'LINKCL                                                      
                                                                                
         STCM  R3,B'0011',CURURLN                                               
                                                                                
SETESTX  DS    0H                                                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* General exits, literals and LTORG                                   *         
***********************************************************************         
                                                                                
EXITH    LHI   RE,2                                                             
         J     EXITCC                                                           
EXITL    DS    0H                                                               
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
MAXRECLN EQU   IOLENQ-(L'IODA+L'IOWORK)                                         
                                                                                
                                                                                
SPACEQ   EQU   C' '                                                             
FFQ      EQU   X'FF'                                                            
LCSCQ    EQU   C'C'                                                             
                                                                                
ES#IDNO  EQU   2                   ID Number map number                         
ES#IND   EQU   62                  OK indicator                                 
ES#ERROR EQU   63                  Error code                                   
*ES#DV    EQU   64                  DataVals map number                         
                                                                                
CHANGEQ  EQU   C'C'                                                             
ADDQ     EQU   C'A'                                                             
                                                                                
HTTP     DC    C'HTTP://'                                                       
                                                                                
*        DC    C'https://csc1-aura.mediaocean.com/viewport-'                    
*        DC    C'home/#osAppId=rod-ests&osPspId=rod-ests&ro'                    
*        DC    C'ute=estimates/display/estimateDisplay&esti'                    
*        DC    C'mate=000507'                                                   
ESTDIS   DS    0H                                                               
         DC    C'/viewport-home/#osAppId=rod-ests&&osPspId=rod-ests&&'          
         DC    C'route=estimates/display/estimateDisplay&&estimate='            
ESTNUMO  DC    C'000507'                                                        
ESTDLNQ  EQU   *-ESTDIS                                                         
                                                                                
SCVTAB   DS    0H                  (ACBRA18)                                    
         DC    AL1(ESTKREJE),AL1(ESTKSUBM,ESTKCAPP,0,0)                         
SCVTABQ  EQU   *-SCVTAB                                                         
         DC    AL1(ESTKCAPP),AL1(ESTKCREA,ESTKSUBM,ESTKREJE,0)                  
*        DC    AL1(ESTKLOGD),AL1(ESTKREJE,ESTKSUBM,ESTKCREA,0)                  
*        DC    AL1(ESTKDELT),AL1(ESTKCREA,0,0,0)                                
*        DC    AL1(ESTKSUBM),AL1(ESTKCREA,ESTKREJE,ESTKSUBM,0)                  
         DC    X'FF'                                                            
                                                                                
SC2TAB   DS    0H                                                               
         DC    AL1(ESTKREJE),AL1(ESTKSUBM,ESTKCAPP,ESTKINTA,0)                  
         DC    AL1(ESTKCAPP),AL1(ESTKINTA,ESTKSUBM,0,0)                         
*        DC    AL1(ESTKLOGD),AL1(ESTKREJE,ESTKSUBM,ESTKCREA,ESTKINTA)           
*        DC    AL1(ESTKDELT),AL1(ESTKCREA,0,0,0)                                
*        DC    AL1(ESTKINTA),AL1(ESTKSUBM,ESTKREJE,0,0)                         
*        DC    AL1(ESTKSUBM),AL1(ESTKCREA,ESTKREJE,ESTKINTA,ESTKSUBM)           
         DC    X'FF'                                                            
                                                                                
ESTNLIT  DC    C'Estimate global number'                                        
EIDNLIT  DC    C'Estimate ID number'                                            
ESTALIT  DC    C'Estimate new status'                                           
EAUTLIT  DC    C'Authorizer name'                                               
ECOMLIT  DC    C'Aprove/Reject comments array'                                  
*DATALIT  DC    C'Data from validate to update'                                 
                                                                                
FAILS    DC    C'tkluth@mediaocean.com:'                                        
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
                                                                                
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(WORKL)                                                       
                                                                                
PZERO    DC    P'0'                                                             
PNEGONE  DC    P'-1'                                                            
PONE     DC    P'1'                                                             
ADDEND   DC    C'ADD=END'                                                       
EFFS     DC    X'FFFFFFFF'                                                      
LINKCB   DC    C'">'                                                            
LINKOP   DC    C'<a style="text-decoration:none;" href="'                       
LINKCL   DC    C'</a>'                                                          
                                                                                
FILES    DS    0C                  ** System/File list **                       
         DC    C'ACCOUNT'                                                       
         DC    C'U'                                                             
ACCDIR   DC    C'ACCDIR '                                                       
         DC    C'U'                                                             
ACCMST   DC    C'ACCMST '                                                       
         DC    C'U'                                                             
ACCARC   DC    C'ACCARC '                                                       
         DC    C'U'                                                             
ACCRCV   DC    C'ACCRCV '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'X'                                                             
                                                                                
RECTAB   DS    0XL(RECTABL)        ** RECORD TABLE **                           
         DC    AL2(A#ESTES),AL1(RECESTQ)                                        
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
DCDICTL  DS    0X                                                               
         DCDDL AC#EMNL1,80         STANDARD EMAIL LINE 1                        
         DCDDL AC#EMNL2,80         STANDARD EMAIL LINE 2                        
         DCDDL AC#EMNL3,80         STANDARD EMAIL LINE 3                        
         DCDDL AC#EMNL4,80         STANDARD EMAIL LINE 4                        
         DCDDL AC#EMNL5,80         STANDARD EMAIL LINE 5                        
         DCDDL AC#EMNL6,80         STANDARD EMAIL LINE 6                        
         DCDDL AC#EMNL7,80         STANDARD EMAIL LINE 7                        
         DCDDL AC#EMNL8,80         STANDARD EMAIL LINE 8                        
         DCDDL AC#EMNL9,80         STANDARD EMAIL LINE 9                        
         DCDDL AC#EMNLA,80         STANDARD EMAIL LINE 9                        
         DCDDL AC#MODE,80          MEDIAOCEAN DE                                
*&&UK*&& DCDDL AC#MOUK,80          MEDIAOCEAN UK                                
*&&US*&& DCDDL AC#MOCA,80          MEDIAOCEAN CA                                
         DCDDL AC#FUSAT,18         FIND US AT                                   
DCDICTLX DC    X'FF'                                                            
                                                                                
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTABL  EQU   *-RECTABD                                                        
                                                                                
RECESTQ  EQU   1                   Estimate approve/reject upload               
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
LLP      DC    A(0)                A(LP_D)                                      
                                                                                
*** General upload response data map numbers                                    
                                                                                
D#UPLERR EQU   255                                                              
D#UPLNOD EQU   254                                                              
                                                                                
***********************************************************************         
* Saved working storage                                               *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
                                                                                
***********************************************************************         
* Sacred values                                                       *         
***********************************************************************         
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER mode                           
                                                                                
RUNINDS  DS    X                   ** Run indicators **                         
                                                                                
PROTOFF  DS    A                   A(PROTOFF)                                   
PROTON   DS    A                   A(PROTON)                                    
MASTC    DS    A                   A(MASTC)                                     
WRKBLKR  DS    A                   A(FACWRK WRKIO block)                        
APRINTER DS    A                                                                
ARUNFACS DS    A                                                                
SAVECT   DS    A                                                                
                                                                                
VSMTP    DS    A                   A(SMTP)                                      
                                                                                
DSDICTL  DS    0C                                                               
         DSDDL PRINT=YES                                                        
DSDICTLX DS    0C                                                               
                                                                                
SAVEVAR  DS    0F                  ** Variables follow **                       
                                                                                
AMYIO    DS    A                                                                
MYIOADR  DS    F                                                                
CURRPIN  DS    H                   Current PID                                  
CURTIME  DS    XL4                 Current time                                 
SVTODL   DS    CL8                                                              
STCSPARE EQU   150                 Spare 150 bytes on main ESTRECD              
                                                                                
SAVEVARL EQU   *-SAVEVAR                                                        
                                                                                
***********************************************************************         
* Request values                                                      *         
***********************************************************************         
                                                                                
ES_VALS  DS    0F                                                               
                                                                                
ES_GLNUM DS    CL6                 Estimate global number                       
ES_IDNUM DS    CL4                 Estimate ID number                           
ES_NSTAT DS    CL1                 Estimate new status                          
ES_RJCTQ EQU   C'1'                - reject                                     
ES_APPRQ EQU   C'2'                - approve                                    
ES_AUTHN DS    CL36                                                             
ES_COMMI DS    X                                                                
ES_COMMM DS    AL3                                                              
ES_AHEAD DS    AL4                                                              
                                                                                
ES_DLNQ  EQU   *-ES_VALS                                                        
                                                                                
**********************************************************************          
* Derived values                                                                
**********************************************************************          
                                                                                
DV_VALS  DS    0F                                                               
                                                                                
DV_INDIC DS    CL1                 Y/N for ok                                   
DV_RETID DS    CL4                 Return ID number                             
DV_ERROR DS    XL2                 Return Error or blank                        
DV_NEWID DS    XL2                 New ID number                                
DV_PEKEY DS    XL(L'ESTKEY)        Prime estimate key                           
DV_NKSTA DS    XL1                 New key status                               
DV_NGAPS DS    XL1                 New GAP status                               
DV_OGAPS DS    XL1                 Old GAP status                               
DV_TODAY DS    XL3                                                              
DV_MAILA DS    CL(L'APPEMAIL)      Mail address                                 
DV_AUTHL DS    XL1                                                              
DV_SYSTM DS    CL3                 FACPAK SYSTEM                                
DV_ERMSG DS    CL80                                                             
                                                                                
DV_VALSL EQU   *-DV_VALS                                                        
                                                                                
***********************************************************************         
* General working storage values                                      *         
***********************************************************************         
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      RECORD TYPE                                  
                                                                                
TXTMODE  DS    CL1                                                              
TEXT1    DS    CL160                                                            
CURURLN  DS    XL2                                                              
CURURL   DS    CL(3*L'TEXT1)                 (480)                              
                                                                                
SAVEL    EQU   *-SAVED                                                          
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
                                                                                
**********************************************************************          
* TRACEIT print line                                                            
**********************************************************************          
                                                                                
PTRACED  DSECT                                                                  
PTTWHO   DS    CL7                                                              
         DS    CL1                                                              
PTMQ     DS    CL2                                                              
         DS    CL1                                                              
PTTTIME  DS    CL(L'TTIMED)                                                     
         DS    CL1                                                              
PTHH     DS    CL2                                                              
PTCO1    DS    CL1                                                              
PTMM     DS    CL2                                                              
PTCO2    DS    CL1                                                              
PTSS     DS    CL2                                                              
         DS    CL2                                                              
PTTALPH  DS    CL(L'TALPHAID)                                                   
         DS    CL1                                                              
PTALPH   DS    CL(L'CUAALF)                                                     
         DS    CL2                                                              
PTTUSER  DS    CL(L'TUSERPID)                                                   
         DS    CL1                                                              
PTUSER   DS    CL(2*L'CUPASS)                                                   
         DS    CL2                                                              
PTFIELD  DS    CL10                                                             
         DS    CL1                                                              
PFIELD   DS    CL95                                                             
         DS    CL1                                                              
                                                                                
*  Other included books follow                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
       ++INCLUDE ACRCVRECD                                                      
*PREFIX=P_                                                                      
       ++INCLUDE DDDPRINTL                                                      
*PREFIX=                                                                        
       ++INCLUDE DDSMTPD                                                        
       ++INCLUDE FAJESMAILD                                                     
       ++INCLUDE FASSBOFF                                                       
                                                                                
***********************************************************************         
* HTML FOR EMAIL                                                      *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
EMLSUM   DS    0H                   SUMMARY EMAIL                               
                                                                                
EMLS05   DC    AL1(EMLS05L)                                                     
         DC    C'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 '                 
         DC    C'Transitional//EN" '                                            
         DC    C'"http://www.w3.org/TR/xhtml11/DTD/xhtml1-transitional'         
         DC    C'.dtd">'                                                        
EMLS05L  EQU   *-EMLS05                                                         
                                                                                
EMLS10   DC    AL1(EMLS10L)                                                     
         DC    C'<html xmlns="http://www.w3.org/1999/xhtml">'                   
EMLS10L  EQU   *-EMLS10                                                         
                                                                                
EMLS15   DC    AL1(EMLS15L)                                                     
         DC    C'<head>'                                                        
EMLS15L  EQU   *-EMLS15                                                         
                                                                                
EMLS20   DC    AL1(EMLS20L)                                                     
         DC    C'<title>'                                                       
EMLS20L  EQU   *-EMLS20                                                         
                                                                                
EMLS25   DC    AL1(EMLS25L)                                                     
         DC    C'</title>'                                                      
EMLS25L  EQU   *-EMLS25                                                         
                                                                                
EMLS30   DC    AL1(EMLS30L)                                                     
         DC    C'<meta http-equiv="Content-Type" content="text/html;'           
         DC    C' charset=UTF-8" />'                                            
EMLS30L  EQU   *-EMLS30                                                         
                                                                                
EMLS35   DC    AL1(EMLS35L)                                                     
         DC    C'<meta name="viewport" content="width=device-width; '           
         DC    C'initial-scale=1; maximum-scale=1.0"/>'                         
EMLS35L  EQU   *-EMLS35                                                         
                                                                                
EMLS40   DC    AL1(EMLS40L)                                                     
         DC    C'</head>'                                                       
EMLS40L  EQU   *-EMLS40                                                         
                                                                                
EMLS45   DC    AL1(EMLS45L)                                                     
         DC    C'<body bgcolor="#ffffff">'                                      
EMLS45L  EQU   *-EMLS45                                                         
                                                                                
EMLS50   DC    AL1(EMLS50L)                                                     
         DC    C'<!-- Outer Wrap -->'                                           
EMLS50L  EQU   *-EMLS50                                                         
                                                                                
EMLS55   DC    AL1(EMLS55L)                                                     
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0" bgcolor="#ffffff" '                            
         DC    C'style="table-layout:fixed;">'                                  
EMLS55L  EQU   *-EMLS55                                                         
                                                                                
EMLS60   DC    AL1(EMLS60L)                                                     
         DC    C'<tr>'                                                          
EMLS60L  EQU   *-EMLS60                                                         
                                                                                
EMLS65   DC    AL1(EMLS65L)                                                     
         DC    C'<td>'                                                          
EMLS65L  EQU   *-EMLS65                                                         
                                                                                
EMLS70   DC    AL1(EMLS70L)                                                     
         DC    C'<!-- top shadow -->'                                           
EMLS70L  EQU   *-EMLS70                                                         
                                                                                
EMLS75   DC    AL1(EMLS75L)                                                     
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0">'                                              
EMLS75L  EQU   *-EMLS75                                                         
                                                                                
EMLS80   DC    AL1(EMLS80L)                                                     
         DC    C'<tr>'                                                          
EMLS80L  EQU   *-EMLS80                                                         
                                                                                
EMLS85   DC    AL1(EMLS85L)                                                     
         DC    C'<td height="10">'                                              
EMLS85L  EQU   *-EMLS85                                                         
                                                                                
EMLS90   DC    AL1(EMLS90L)                                                     
         DC    C'</tr>'                                                         
EMLS90L  EQU   *-EMLS90                                                         
                                                                                
EMLS95   DC    AL1(EMLS95L)                                                     
         DC    C'</table>'                                                      
EMLS95L  EQU   *-EMLS95                                                         
                                                                                
EMLS100  DC    AL1(EMLS100L)                                                    
         DC    C'<!-- header -->'                                               
EMLS100L EQU   *-EMLS100                                                        
                                                                                
EMLS105  DC    AL1(EMLS105L)                                                    
         DC    C'<table width="100%" bgcolor="#ffffff" border="0" '             
         DC    C'cellspacing="0" cellpadding="0">'                              
EMLS105L EQU   *-EMLS105                                                        
                                                                                
EMLS110  DC    AL1(EMLS110L)                                                    
         DC    C'<tr>'                                                          
EMLS110L EQU   *-EMLS110                                                        
                                                                                
EMLS115  DC    AL1(EMLS115L)                                                    
         DC    C'<td>'                                                          
EMLS115L EQU   *-EMLS115                                                        
                                                                                
EMLS120  DC    AL1(EMLS120L)                                                    
         DC    C'<a href="http://mediaocean.com">'                              
EMLS120L EQU   *-EMLS120                                                        
                                                                                
EMLS125  DC    AL1(EMLS125L)                                                    
         DC    C'<img alt="Aura" src="http://info.mediaocean.com/rs/'           
         DC    C'331-XPM-231/images/Aura-header.png"/>'                         
EMLS125L EQU   *-EMLS125                                                        
                                                                                
EMLS130  DC    AL1(EMLS130L)                                                    
         DC    C'</a>'                                                          
EMLS130L EQU   *-EMLS130                                                        
                                                                                
EMLS135  DC    AL1(EMLS135L)                                                    
         DC    C'</td>'                                                         
EMLS135L EQU   *-EMLS135                                                        
                                                                                
EMLS140  DC    AL1(EMLS140L)                                                    
         DC    C'</tr>'                                                         
EMLS140L EQU   *-EMLS140                                                        
                                                                                
EMLS145  DC    AL1(EMLS145L)                                                    
         DC    C'</table>'                                                      
EMLS145L EQU   *-EMLS145                                                        
                                                                                
EMLS150  DC    AL1(EMLS150L)                                                    
         DC    C'<!-- end: header -->'                                          
EMLS150L EQU   *-EMLS150                                                        
                                                                                
EMLS155  DC    AL1(EMLS155L)                                                    
         DC    C'<!-- body -->'                                                 
EMLS155L EQU   *-EMLS155                                                        
                                                                                
EMLS160  DC    AL1(EMLS160L)                                                    
         DC    C'<table width="100%" border="0" cellspacing="0" '               
         DC    C'cellpadding="0">'                                              
EMLS160L EQU   *-EMLS160                                                        
                                                                                
EMLS165  DC    AL1(EMLS165L)                                                    
         DC    C'<tr>'                                                          
EMLS165L EQU   *-EMLS165                                                        
                                                                                
EMLS170  DC    AL1(EMLS170L)                                                    
         DC    C'<td height="10">'                                              
EMLS170L EQU   *-EMLS170                                                        
                                                                                
EMLS175  DC    AL1(EMLS175L)                                                    
         DC    C'</td>'                                                         
EMLS175L EQU   *-EMLS175                                                        
                                                                                
EMLS180  DC    AL1(EMLS180L)                                                    
         DC    C'</tr>'                                                         
EMLS180L EQU   *-EMLS180                                                        
                                                                                
EMLS185  DC    AL1(EMLS185L)                                                    
         DC    C'</table>'                                                      
EMLS185L EQU   *-EMLS185                                                        
                                                                                
EMLS190  DC    AL1(EMLS190L)                                                    
         DC    C'<!-- tape check module -->'                                    
EMLS190L EQU   *-EMLS190                                                        
                                                                                
EMLS195  DC    AL1(EMLS195L)                                                    
         DC    C'<table width="800" bgcolor="#ffffff" border="0" '              
         DC    C'cellspacing="0" cellpadding="0" '                              
         DC    C'style="font-family: Calibri, sans-serif; '                     
         DC    C'font-size:13px; color:#4E4D4C; '                               
EMLS195L EQU   *-EMLS195                                                        
                                                                                
EMLS200  DC    AL1(EMLS200L)                                                    
         DC    C'background-color:#D9D9CD; '                                    
         DC    C'-webkit-border-radius: 3px; '                                  
         DC    C'-moz-border-radius: 3px; '                                     
         DC    C'border-radius: 3px;">'                                         
EMLS200L EQU   *-EMLS200                                                        
                                                                                
EMLS205  DC    AL1(EMLS205L)                                                    
         DC    C'<tr>'                                                          
EMLS205L EQU   *-EMLS205                                                        
                                                                                
EMLS210  DC    AL1(EMLS210L)                                                    
         DC    C'<td height="20" colspan="3" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS210L EQU   *-EMLS210                                                        
                                                                                
EMLS215  DC    AL1(EMLS215L)                                                    
         DC    C'<img height="20" alt="spacer" '                                
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS215L EQU   *-EMLS215                                                        
                                                                                
EMLS220  DC    AL1(EMLS220L)                                                    
         DC    C'</td>'                                                         
EMLS220L EQU   *-EMLS220                                                        
                                                                                
EMLS225  DC    AL1(EMLS225L)                                                    
         DC    C'</tr>'                                                         
EMLS225L EQU   *-EMLS225                                                        
                                                                                
EMLS230  DC    AL1(EMLS230L)                                                    
         DC    C'<tr>'                                                          
EMLS230L EQU   *-EMLS230                                                        
                                                                                
EMLS235  DC    AL1(EMLS235L)                                                    
         DC    C'<td width="20">'                                               
EMLS235L EQU   *-EMLS235                                                        
                                                                                
EMLS240  DC    AL1(EMLS240L)                                                    
         DC    C'<img height="1" width="20" alt="spacer" '                      
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS240L EQU   *-EMLS240                                                        
                                                                                
EMLS245  DC    AL1(EMLS245L)                                                    
         DC    C'</td>'                                                         
EMLS245L EQU   *-EMLS245                                                        
                                                                                
EMLS250  DC    AL1(EMLS250L)                                                    
         DC    C'<td valign="top" width="360" '                                 
         DC    C'style="font-family: Calibri, sans-serif; '                     
         DC    C'font-size:30px; color:#4E4D4C; line-height:36px;">'            
EMLS250L EQU   *-EMLS250                                                        
                                                                                
EMLS255  DC    AL1(EMLS255L)                                                    
         DC    C'<div>'                                                         
EMLS255E DS    CL80                                                             
         DC    C'</div>'                                                        
EMLS255L EQU   *-EMLS255                                                        
                                                                                
EMLS260  DC    AL1(EMLS260L)                                                    
EMLS260E DS    CL160                                                            
EMLS260L EQU   *-EMLS260                                                        
                                                                                
EMLS261  DC    AL1(EMLS261L)                                                    
EMLS261E DS    CL160                                                            
EMLS261L EQU   *-EMLS261                                                        
                                                                                
EMLS262  DC    AL1(EMLS262L)                                                    
EMLS262E DS    CL160                                                            
EMLS262L EQU   *-EMLS262                                                        
                                                                                
EMLS270  DC    AL1(EMLS270L)                                                    
         DC    C'<p style="color:#4E4D4C; font-size:13px; '                     
         DC    C'text-decoration:none; line-height:16px;">'                     
EMLS270L EQU   *-EMLS270                                                        
                                                                                
EMLS280  DC    AL1(EMLS280L)                                                    
EMLS280E DS    CL80                                                             
EMLS280L EQU   *-EMLS280                                                        
                                                                                
EMLS283  DC    AL1(EMLS283L)                                                    
EMLS283E DS    CL80                                                             
EMLS283L EQU   *-EMLS283                                                        
                                                                                
EMLS284  DC    AL1(EMLS284L)                                                    
EMLS284E DS    CL160                                                            
EMLS284L EQU   *-EMLS284                                                        
                                                                                
EMLS285  DC    AL1(EMLS285L)                                                    
EMLS285E DS    CL160                                                            
EMLS285L EQU   *-EMLS285                                                        
                                                                                
EMLS286  DC    AL1(EMLS286L)                                                    
EMLS286E DS    CL160                                                            
EMLS286L EQU   *-EMLS286                                                        
                                                                                
EMLS288  DC    AL1(EMLS288L)                                                    
         DC    C'</br>'                                                         
EMLS288L EQU   *-EMLS288                                                        
                                                                                
EMLS289  DC    AL1(EMLS289L)                                                    
EMLS289E DS    CL160                                                            
EMLS289L EQU   *-EMLS289                                                        
                                                                                
EMLS290  DC    AL1(EMLS290L)                                                    
         DC    C'</a>'                                                          
EMLS290L EQU   *-EMLS290                                                        
                                                                                
EMLS295  DC    AL1(EMLS295L)                                                    
         DC    C'</p>'                                                          
EMLS295L EQU   *-EMLS295                                                        
                                                                                
EMLS300  DC    AL1(EMLS300L)                                                    
         DC    C'</td>'                                                         
EMLS300L EQU   *-EMLS300                                                        
                                                                                
EMLS305  DC    AL1(EMLS305L)                                                    
         DC    C'<td width="20">'                                               
EMLS305L EQU   *-EMLS305                                                        
                                                                                
EMLS310  DC    AL1(EMLS310L)                                                    
         DC    C'<img height="1" width="20" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
EMLS310L EQU   *-EMLS310                                                        
                                                                                
EMLS315  DC    AL1(EMLS315L)                                                    
         DC    C'</td>'                                                         
EMLS315L EQU   *-EMLS315                                                        
                                                                                
EMLS320  DC    AL1(EMLS320L)                                                    
         DC    C'</tr>'                                                         
EMLS320L EQU   *-EMLS320                                                        
                                                                                
EMLS325  DC    AL1(EMLS325L)                                                    
         DC    C'<tr>'                                                          
EMLS325L EQU   *-EMLS325                                                        
EMLS330  DC    AL1(EMLS330L)                                                    
         DC    C'<td height="30" colspan="3" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS330L EQU   *-EMLS330                                                        
                                                                                
EMLS335  DC    AL1(EMLS335L)                                                    
         DC    C'<img height="30" width="20" border="0" alt="spacer" '          
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/images'         
         DC    C'/spacer_icon.png"/>'                                           
EMLS335L EQU   *-EMLS335                                                        
                                                                                
EMLS340  DC    AL1(EMLS340L)                                                    
         DC    C'</td>'                                                         
EMLS340L EQU   *-EMLS340                                                        
                                                                                
EMLS345  DC    AL1(EMLS345L)                                                    
         DC    C'</tr>'                                                         
EMLS345L EQU   *-EMLS345                                                        
                                                                                
EMLS350  DC    AL1(EMLS350L)                                                    
         DC    C'</table>'                                                      
EMLS350L EQU   *-EMLS350                                                        
                                                                                
EMLS355  DC    AL1(EMLS355L)                                                    
         DC    C'<!-- no background module -->'                                 
EMLS355L EQU   *-EMLS355                                                        
                                                                                
EMLS360  DC    AL1(EMLS360L)                                                    
         DC    C'<table width="800" bgcolor="#ffffff" border="0" '              
         DC    C'cellspacing="0" cellpadding="0" style="font-family: '          
EMLS360L EQU   *-EMLS360                                                        
                                                                                
EMLS365  DC    AL1(EMLS365L)                                                    
         DC    C'Calibri, sans-serif; font-size:10px; color:#4E4D4C; '          
         DC    C'background-color:#ffffff; -webkit-border-radius: 3px;'         
         DC    C' -moz-border-radius: 3px; border-radius: 3px;">'               
EMLS365L EQU   *-EMLS365                                                        
                                                                                
EMLS370  DC    AL1(EMLS370L)                                                    
         DC    C'<tr>'                                                          
EMLS370L EQU   *-EMLS370                                                        
                                                                                
EMLS375  DC    AL1(EMLS375L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS375L EQU   *-EMLS375                                                        
                                                                                
EMLS380  DC    AL1(EMLS380L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS380L EQU   *-EMLS380                                                        
                                                                                
EMLS385  DC    AL1(EMLS385L)                                                    
         DC    C'</td>'                                                         
EMLS385L EQU   *-EMLS385                                                        
                                                                                
EMLS390  DC    AL1(EMLS390L)                                                    
         DC    C'<td>'                                                          
EMLS390L EQU   *-EMLS390                                                        
                                                                                
EMLS395  DC    AL1(EMLS395L)                                                    
         DC    X'50'                                                            
         DC    C'nbsp;'                                                         
EMLS395L EQU   *-EMLS395                                                        
                                                                                
EMLS400  DC    AL1(EMLS400L)                                                    
         DC    C'</td>'                                                         
EMLS400L EQU   *-EMLS400                                                        
                                                                                
EMLS405  DC    AL1(EMLS405L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS405L EQU   *-EMLS405                                                        
                                                                                
EMLS410  DC    AL1(EMLS410L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS410L EQU   *-EMLS410                                                        
                                                                                
EMLS415  DC    AL1(EMLS415L)                                                    
         DC    C'</td>'                                                         
EMLS415L EQU   *-EMLS415                                                        
                                                                                
EMLS420  DC    AL1(EMLS420L)                                                    
         DC    C'</tr>'                                                         
EMLS420L EQU   *-EMLS420                                                        
                                                                                
EMLS425  DC    AL1(EMLS425L)                                                    
         DC    C'<tr>'                                                          
EMLS425L EQU   *-EMLS425                                                        
                                                                                
EMLS430  DC    AL1(EMLS430L)                                                    
         DC    C'<td style="vertical-align:top;">'                              
EMLS430L EQU   *-EMLS430                                                        
                                                                                
EMLS435  DC    AL1(EMLS435L)                                                    
         DC    C'<p>'                                                           
EMLS435L EQU   *-EMLS435                                                        
                                                                                
EMLS440  DC    AL1(EMLS440L)                                                    
EMAILS1  DS    CL80                                                             
EMAILS2  DS    CL80                                                             
EMLS440L EQU   *-EMLS440                                                        
                                                                                
EMLS445  DC    AL1(EMLS445L)                                                    
EMAILS3  DS    CL80                                                             
EMAILS4  DS    CL80                                                             
EMLS445L EQU   *-EMLS445                                                        
                                                                                
EMLS450  DC    AL1(EMLS450L)                                                    
EMAILS5  DS    CL80                                                             
         DC    C'</p>'                                                          
EMLS450L EQU   *-EMLS450                                                        
                                                                                
EMLS455  DC    AL1(EMLS455L)                                                    
         DC    C'<p>'                                                           
EMLS455L EQU   *-EMLS455                                                        
                                                                                
EMLS460  DC    AL1(EMLS460L)                                                    
EMAILS6  DS    CL80                                                             
EMAILS7  DS    CL80                                                             
EMLS460L EQU   *-EMLS460                                                        
                                                                                
EMLS465  DC    AL1(EMLS465L)                                                    
EMAILS8  DS    CL80                                                             
EMAILS9  DS    CL80                                                             
EMLS465L EQU   *-EMLS465                                                        
                                                                                
EMLS468  DC    AL1(EMLS468L)                                                    
EMAILSA  DS    CL80                                                             
EMLS468L EQU   *-EMLS468                                                        
                                                                                
EMLS470  DC    AL1(EMLS470L)                                                    
         DC    C'</p>'                                                          
EMLS470L EQU   *-EMLS470                                                        
                                                                                
EMLS475  DC    AL1(EMLS475L)                                                    
         DC    C'</td>'                                                         
EMLS475L EQU   *-EMLS475                                                        
                                                                                
EMLS480  DC    AL1(EMLS480L)                                                    
         DC    C'</tr>'                                                         
EMLS480L EQU   *-EMLS480                                                        
                                                                                
EMLS485  DC    AL1(EMLS485L)                                                    
         DC    C'<tr>'                                                          
EMLS485L EQU   *-EMLS485                                                        
                                                                                
EMLS490  DC    AL1(EMLS490L)                                                    
         DC    C'<td height="20" style="font-size:1px; '                        
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS490L EQU   *-EMLS490                                                        
                                                                                
EMLS495  DC    AL1(EMLS495L)                                                    
         DC    C'<img height="20" border="0" alt=spacer" '                      
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS495L EQU   *-EMLS495                                                        
                                                                                
EMLS500  DC    AL1(EMLS500L)                                                    
         DC    C'</td>'                                                         
EMLS500L EQU   *-EMLS500                                                        
                                                                                
EMLS505  DC    AL1(EMLS505L)                                                    
         DC    C'</tr>'                                                         
EMLS505L EQU   *-EMLS505                                                        
                                                                                
EMLS510  DC    AL1(EMLS510L)                                                    
         DC    C'</table>'                                                      
EMLS510L EQU   *-EMLS510                                                        
                                                                                
EMLS515  DC    AL1(EMLS515L)                                                    
         DC    C'<!-- end: body -->'                                            
EMLS515L EQU   *-EMLS515                                                        
                                                                                
EMLS520  DC    AL1(EMLS520L)                                                    
         DC    C'<!-- Footer -->'                                               
EMLS520L EQU   *-EMLS520                                                        
                                                                                
EMLS525  DC    AL1(EMLS525L)                                                    
         DC    C'<table width="800" cellspacing="0" cellpadding="0" '           
         DC    C'style="height:78px; font-family: Calibri, '                    
EMLS525L EQU   *-EMLS525                                                        
                                                                                
EMLS530  DC    AL1(EMLS530L)                                                    
         DC    C'sans-serif; font-size:10px; color:#ffffff; '                   
         DC    C'background-color:#4E4D4C; text-align:center; '                 
EMLS530L EQU   *-EMLS530                                                        
                                                                                
EMLS535  DC    AL1(EMLS535L)                                                    
         DC    C'border-bottom:1px solid #4E4D4C; '                             
         DC    C'-webkit-border-radius: 3px; -moz-border-radius: 3px; '         
         DC    C'border-radius: 3px;">'                                         
EMLS535L EQU   *-EMLS535                                                        
                                                                                
EMLS540  DC    AL1(EMLS540L)                                                    
         DC    C'<tr>'                                                          
EMLS540L EQU   *-EMLS540                                                        
                                                                                
EMLS545  DC    AL1(EMLS545L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS545L EQU   *-EMLS545                                                        
                                                                                
EMLS550  DC    AL1(EMLS550L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS550L EQU   *-EMLS550                                                        
                                                                                
EMLS555  DC    AL1(EMLS555L)                                                    
         DC    C'</td>'                                                         
EMLS555L EQU   *-EMLS555                                                        
                                                                                
EMLS560  DC    AL1(EMLS560L)                                                    
         DC    C'<td colspan="2" height="15" style="font-size:1px; '            
         DC    C'border-collapse:collapse; margin:0; padding:0;">'              
EMLS560L EQU   *-EMLS560                                                        
                                                                                
EMLS565  DC    AL1(EMLS565L)                                                    
         DC    C'<img height="15" border="0" alt="spacer" '                     
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS565L EQU   *-EMLS565                                                        
                                                                                
EMLS570  DC    AL1(EMLS570L)                                                    
         DC    C'</td>'                                                         
EMLS570L EQU   *-EMLS570                                                        
                                                                                
EMLS575  DC    AL1(EMLS575L)                                                    
         DC    C'<td rowspan="3" width="30">'                                   
EMLS575L EQU   *-EMLS575                                                        
                                                                                
EMLS580  DC    AL1(EMLS580L)                                                    
         DC    C'<img height="1" width="30" border="0" alt="spacer" '           
         DC    C'src="http://info.mediaocean.com/rs/331-XPM-231/'               
         DC    C'images/spacer_icon.png"/>'                                     
EMLS580L EQU   *-EMLS580                                                        
                                                                                
EMLS585  DC    AL1(EMLS585L)                                                    
         DC    C'</td>'                                                         
EMLS585L EQU   *-EMLS585                                                        
                                                                                
EMLS590  DC    AL1(EMLS590L)                                                    
         DC    C'</tr>'                                                         
EMLS590L EQU   *-EMLS590                                                        
                                                                                
EMLS595  DC    AL1(EMLS595L)                                                    
         DC    C'<tr style="vertical-align:bottom;">'                           
EMLS595L EQU   *-EMLS595                                                        
                                                                                
EMLS600  DC    AL1(EMLS600L)                                                    
         DC    C'<td style="text-align:left;">'                                 
EMLS600L EQU   *-EMLS600                                                        
                                                                                
EMLS605  DC    AL1(EMLS605L)                                                    
         DC    C'<div>'                                                         
EMLS605L EQU   *-EMLS605                                                        
                                                                                
EMLS610  DC    AL1(EMLS610L)                                                    
EMLS610E DS    CL80                                                             
EMLS610L EQU   *-EMLS610                                                        
                                                                                
EMLS615  DC    AL1(EMLS615L)                                                    
         DC    C'<br />'                                                        
EMLS615L EQU   *-EMLS615                                                        
                                                                                
EMLS620  DC    AL1(EMLS620L)                                                    
         DC    X'50'                                                            
         DC    C'copy'                                                          
EMLS620E DS    CL4                                                              
         DC    C' MEDIAOCEAN'                                                   
EMLS620L EQU   *-EMLS620                                                        
                                                                                
EMLS625  DC    AL1(EMLS625L)                                                    
         DC    C'| '                                                            
EMLS625E DS    CL18                                                             
         DC    C':'                                                             
EMLS625L EQU   *-EMLS625                                                        
                                                                                
EMLS630  DC    AL1(EMLS630L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://www.facebook.com/team.mediaocean">'              
         DC    C'FACEBOOK'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
EMLS630L EQU   *-EMLS630                                                        
                                                                                
EMLS635  DC    AL1(EMLS635L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="https://twitter.com/teammediaocean">'                    
         DC    C'TWITTER'                                                       
         DC    C'</a>'                                                          
         DC    C' |'                                                            
EMLS635L EQU   *-EMLS635                                                        
                                                                                
EMLS640  DC    AL1(EMLS640L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://www.linkedin.com/company/mediaocean">'            
         DC    C'LINKEDIN'                                                      
         DC    C'</a>'                                                          
         DC    C' |'                                                            
EMLS640L EQU   *-EMLS640                                                        
                                                                                
EMLS642  DC    AL1(EMLS642L)                                                    
         DC    C'<a style="text-decoration:none; color:#ffffff;" '              
         DC    C'href="http://instagram.com/teammediaocean">'                   
         DC    C'INSTAGRAM'                                                     
         DC    C'</a>'                                                          
EMLS642L EQU   *-EMLS642                                                        
                                                                                
EMLS645  DC    AL1(EMLS645L)                                                    
         DC    C'</div>'                                                        
EMLS645L EQU   *-EMLS645                                                        
                                                                                
EMLS650  DC    AL1(EMLS650L)                                                    
         DC    C'</td>'                                                         
EMLS650L EQU   *-EMLS650                                                        
                                                                                
EMLS655  DC    AL1(EMLS655L)                                                    
         DC    C'<td style="text-align:right;">'                                
EMLS655L EQU   *-EMLS655                                                        
                                                                                
EMLS660  DC    AL1(EMLS660L)                                                    
         DC    C'<a href="http://www.mediaocean.com">'                          
EMLS660L EQU   *-EMLS660                                                        
                                                                                
EMLS665  DC    AL1(EMLS665L)                                                    
         DC    C'<img src="http://info.mediaocean.com/rs/331-XPM-231/'          
         DC    C'images/MO-aura-footer-logo.png" alt="MediaOcean" />'           
EMLS665L EQU   *-EMLS665                                                        
                                                                                
EMLS670  DC    AL1(EMLS670L)                                                    
         DC    C'</a>'                                                          
EMLS670L EQU   *-EMLS670                                                        
                                                                                
EMLS675  DC    AL1(EMLS675L)                                                    
         DC    C'</td>'                                                         
EMLS675L EQU   *-EMLS675                                                        
                                                                                
EMLS680  DC    AL1(EMLS680L)                                                    
         DC    C'</tr>'                                                         
EMLS680L EQU   *-EMLS680                                                        
                                                                                
EMLS685  DC    AL1(EMLS685L)                                                    
         DC    C'</table>'                                                      
EMLS685L EQU   *-EMLS685                                                        
                                                                                
EMLS690  DC    AL1(EMLS690L)                                                    
         DC    C'<!-- end: Footer -->'                                          
EMLS690L EQU   *-EMLS690                                                        
                                                                                
EMLS695  DC    AL1(EMLS695L)                                                    
         DC    C'</td>'                                                         
EMLS695L EQU   *-EMLS695                                                        
                                                                                
EMLS700  DC    AL1(EMLS700L)                                                    
         DC    C'</tr>'                                                         
EMLS700L EQU   *-EMLS700                                                        
                                                                                
EMLS705  DC    AL1(EMLS705L)                                                    
         DC    C'</table>'                                                      
EMLS705L EQU   *-EMLS705                                                        
                                                                                
EMLS710  DC    AL1(EMLS710L)                                                    
         DC    C'<!-- End Inner Wrap -->'                                       
EMLS710L EQU   *-EMLS710                                                        
                                                                                
EMLS715  DC    AL1(EMLS715L)                                                    
         DC    C'</td>'                                                         
EMLS715L EQU   *-EMLS715                                                        
                                                                                
EMLS720  DC    AL1(EMLS720L)                                                    
         DC    C'</tr>'                                                         
EMLS720L EQU   *-EMLS720                                                        
                                                                                
EMLS725  DC    AL1(EMLS725L)                                                    
         DC    C'</table>'                                                      
EMLS725L EQU   *-EMLS725                                                        
                                                                                
EMLS730  DC    AL1(EMLS730L)                                                    
         DC    C'<!-- End Outer Wrap -->'                                       
EMLS730L EQU   *-EMLS730                                                        
                                                                                
EMLS735  DC    AL1(EMLS735L)                                                    
         DC    C'</body>'                                                       
EMLS735L EQU   *-EMLS735                                                        
                                                                                
EMLS740  DC    AL1(EMLS740L)                                                    
         DC    C'</html>'                                                       
EMLS740L EQU   *-EMLS740                                                        
                                                                                
EMLSTMLL EQU   *-EMLSUM                                                         
         DC    X'FF'                                                            
                                                                                
         DS    0H                                                               
       ++INCLUDE ACURLTAB                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACBRA10   08/05/20'                                      
         END                                                                    
