*          DATA SET ACBRA1E    AT LEVEL 020 AS OF 11/12/20                      
*PHASE T6241EA                                                                  
                                                                                
         TITLE 'MQ + Regular Time Line Upload Server'                           
                                                                                
***********************************************************************         
* Level change comments                                               *         
* ---------------------                                               *         
* TKLU 001 16Aug18 DSRD-17832 New server for new Time Line Upload     *         
* Note: includes RMOR FALOCKETD and Locket under MQ changes if        *         
*       TH4RMOQ user set                                              *         
* MPEN 006 23Oct18 DSRD-20447 Relink for DPAPASD changes              *         
* MPEN 007 29Mar19 DSRD-20623 Validation and code improvements        *         
* MPEN     05Apr19 DSRD-22176 Fix for default time type               *         
* MPEN     08Apr19 DSRD-22238 Remove code to set QA dates to 2014     *         
* MPEN     25Apr19 DSRD-22383 Fix for JBT/billable checks             *         
* MPEN 008 02May19 DSRD-22459 Fix for security PID being passed       *         
* MPEN 009 30May19 DSRD-22727 Fix for audit corruption                *         
* MPEN 010 06Jun19 DSRD-22633 Mark time as being from time widget     *         
* MPEN     19Jun19 DSRD-22162 Change error message                    *         
* NSHE     12Jul19 DSRD-22996 Approver unable to approve widget time  *         
* NSHE 011 18Sep19 DSRD-23871 Reject timelines that don't have access *         
* MPEN 012 23Oct19 DSRD-24286 Tidy up error messages                  *         
* MPEN     29Oct19 DSRD-24181 Mark audit as from widget               *         
* MPEN     06Nov19 DSRD-24427 Don't perform LIMACC check when under MQ*         
* MPEN     20Nov19 DSRD-24559 Fix for audit incorrectly marking as chg*         
* MPEN 013 12Dec19 DSRD-24427 Fix for dump when add time via API      *         
* MPEN     16Jan20 DSRD025211 Fix for comments if t/s does not exist  *         
* MPEN 014 10Feb20 DSRD-25377 Relink book                             *         
* MPEN 015 25Feb20 DSRD-25576 Fix for more than 24 hours              *         
* NRAK 016 09Jun20 SPEC-46957 Don't call srupd60 without sorter recs  *         
* YNGX 017 22Jun20 DSRD-26737 Add comments for non-client timeline    *         
* NSHE 018 13Aug20 DSRD-27273 Hack for 1 character product code       *         
* NSHE 020 12Nov20 DSRD-27931 Ensure we build passives                *         
*                                                                     *         
***********************************************************************         
* Coments & issues                                                    *         
* ----------------                                                    *         
* Based on ACBRA10 (based on ACBRA14)                                 *         
* This runs under MQ but can be run via FacPak (=BRA), too            *         
* This is supposed to run single request at a time (so no two or more *         
* requests combined as a single run)                                  *         
* TKLU.DDS.JCL(ACRNBTC) to run own server                             *         
* TKLU.DDS.JCL(RUNBRAC) to IDF run it                                 *         
* TKLU.DDS.JCL(MQPUTBIG) to run as MQ                                 *         
* Note RLEN=4096 for maximum of data overall                          *         
* Note Workerkey=ACSU needs the AC start for Dictate offline MCSYSTEM *         
* EXITY on error under MQ not EXITN                                   *         
* No data passing between validate and run under MQ                   *         
* Response with K=? issue - online only                               *         
* AWMPOST data passing not working under MQ, at UPD it points to 0s   *         
*                                                                     *         
* => query THOMASQ/TH4RMOQ functionality                              *         
* => TRACEIT from LOCKER                                              *         
*                                                                     *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=UO,CODE=ENTRY,RLEN=4096,REQUEST=*,WORKERKEY=ACTL,  x        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ,         x        
               FILES=FILES,SERVERTYPE=TSTBOTU,SYSPHASE=SYSPHASE,IDF=Y, x        
               PROGRAM=RCVPBRAQ,LOADFACSOFF=Y                                   
                                                                                
ENTRY    DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO1E**,RR=RE                                                 
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
         ST    RE,SRVRRELO         Save program relocation factor               
         MVC   WRKBLKR,RWRKBLKR    Set A(FACWRK WRKIO block)                    
         MVC   ASORTER,RSORTER     Save A(SORTER)                               
         MVC   RUNMODE,RUNPMODE    Set calling mode                             
         DROP  R6,R7                                                            
                                                                                
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
         MVC   DATAMGR,CDATAMGR-COMFACSD(RE)                                    
         MVC   PROTOFF,CPROTOFF-COMFACSD(RE)                                    
         MVC   PROTON,CPROTON-COMFACSD(RE)                                      
         MVC   LOCKET,CLOCKET-COMFACSD(RE)                                      
                                                                                
         MVC   XDATAMGR,VDATAMGR   Point to real DATAMGR entry point            
         MVC   IDATAMGR,VDATAMGR                                                
                                                                                
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
         GOTOR (RF),(R1),0,(C'E',SRUPD60)                                       
         MVC   ASRUPD60,0(R1)      Set A(SRUPD60)                               
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
                                                                                
         NI    RUNINDS,FF-RUNIATRF Set first time for GOATRN                    
                                                                                
         TM    LP_FLAG2,LP_FMQIM   If MQ then set agency                        
         JZ    SREQ02                                                           
         L     RE,MASTC            Set I/O trace option                         
         MVC   MCIDAGYB-MASTD(L'MCIDAGYB,RE),LP_AGYB                            
                                                                                
* one-time initialisation triggers for utilities - IMPORTANT                    
SREQ02   MVI   RUNINDS,0                                                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    SREQ04                                                           
                                                                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0   open acc files               
         GOTOR DATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                                
                                                                                
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
                                                                                
         LA    R0,COMFACS          Take local copy of COMFACS                   
         LHI   R1,COMFACSL                                                      
         L     RE,LP_ACOM                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    RF,COMFACS                                                       
         ST    RF,ACOMFACS         Set A(COMFACS) as local copy                 
         LARL  R0,DMGRITRN                                                      
         ST    R0,IDATAMGR         Set internal DATAMGR entry point             
         LARL  R0,DMGRXTRN                                                      
         ST    R0,XDATAMGR         Set external DATAMGR entry point             
         MVC   VDATAMGR,IDATAMGR   Set addresses for callers                    
         MVC   CDATAMGR-COMFACSD(,RF),XDATAMGR                                  
                                                                                
         MVI   GIND2,GI2ETIM       Set want time values returned                
         GOTOR (#CPYINI,ACPYINI)   Initialise company values                    
         USING CPYELD,SCPYEL                                                    
         USING CPXELD,SCPXEL                                                    
                                                                                
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
                                                                                
         CLI   RECTYPE,RECTILQ     Test time line record                        
         JNE   UPLD04                                                           
         GOTOR UTIM                                                             
         J     EXIT                                                             
                                                                                
UPLD04   DS    0H                                                               
         J     *+2                 (Invalid record)                             
         DROP  RB                                                               
                                                                                
***********************************************************************         
* Process Time Line upload record                                     *         
***********************************************************************         
                                                                                
UTIM     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LAY   RE,ERRTAB                                                        
         MVI   0(RE),ET_EOTQ       Initialise error table                       
                                                                                
         MVC   CONNPIN,CCTPID      MQ checks and PIN retrieval                  
         TM    LP_FLAG2,LP_FMQIM                                                
         JZ    UTIMRUN                                                          
                                                                                
*** SPEC-21439: Turn off LP_FDRFT setting as a safety measure ***               
         NI    LP_FLAG,FFQ-LP_FDRFT                                             
*** (see ACBRA10 for more general approach)                                     
                                                                                
         TM    TWAMODE,TWAMEMQ     This avoids multiple upload records          
         JNZ   *+2                 within a request if any errors               
                                                                                
         DS    0H                  Field 19: LP_SECNO (if set)                  
         L     RF,LP_ASECD         Field 29: SECOPASS from Secret block         
         MVC   CONNPIN,SECOPASS-SECD(RF)                                        
         MVC   CCTPID,CONNPIN                                                   
                                                                                
UTIMRUN  CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UTIMR02                                                          
         TM    LP_FLAG2,LP_FMQIM   If MQ then skip validate request             
         JNZ   EXITY                                                            
         J     VALTIM                                                           
*                                                                               
UTIMR02  CLI   RUNMODE,RRUNREQQ    Test 'Run request' mode                      
         JNE   *+2                                                              
         TM    LP_FLAG2,LP_FMQIM   If not MQ then go to update straight         
         JZ    UPDTIM                                                           
                                                                                
*** Validate time line request data ***                                         
                                                                                
VALTIM   DS    0H                                                               
                                                                                
         NI    TWAMODE,FFQ-(TWAMERP+TWAMEDP)                                    
                                                                                
         GOTOR SETTIME                                                          
                                                                                
         GOTOR VDATCON,DMCB,(15,0),(1,DV_TODAY)                                 
         GOTOR VDATCON,DMCB,(15,0),(0,DV_TODF)                                  
         GOTOR VDATCON,DMCB,(15,0),(2,DV_TODC)                                  
                                                                                
         MVC   DV_DATE,TL_DATE     any date passed?                             
         OC    TL_DATE,TL_DATE                                                  
         JNZ   VTIM002                                                          
         MVC   DV_DATE,DV_TODAY    if not use today                             
                                                                                
VTIM002  GOTOR TRACEIT,VALTIMQ     Trace request                                
                                                                                
         MVC   TEMP2(L'CONNPIN),CONNPIN                                         
         GOTOR (#GETPID,AGETPID)                                                
         JE    VTIM004                                                          
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INPIN                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#GLOB                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM004  GOTOR (#GETPIN,AGETPIN)                                                
         JE    VTIM006                                                          
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INPID                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#GLOB                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM006  DS    0H                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   VTIM008                                                          
         GOTO1 VGETFACT,DMCB,(X'80',DV_SYSTM),F#SSYSNA                          
         J     VTIM010                                                          
                                                                                
         USING RUNPARMD,R1                                                      
VTIM008  L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         XR    RE,RE                                                            
         ICM   RE,B'0111',RUNPARUN                                              
         USING RUNFACSD,RE         R4=A(RUNFACS)                                
         ICM   RF,B'1111',RMASTC                                                
         ICM   RE,B'1111',MCSSB-MASTD(RF)                                       
         MVC   DV_SYSTM,SSODSPAC-SSOOFF(RE)                                     
         DROP  R1,RE                                                            
                                                                                
VTIM010  DS    0H                                                               
                                                                                
VTIM012  OC    TL_PIN,TL_PIN       Get (security) PIN first, if not set         
         JNZ   VTIM014                                                          
         CLC   TL_PID,SPACES       check for security PID, else must            
         JH    VTIM016                                                          
         CLC   TL_PCC,SPACES       have cost person code                        
         JH    VTIM026                                                          
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$MIPER                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PIN                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM014  MVC   TEMP2(L'TL_PIN),TL_PIN                                           
         GOTOR (#GETPID,AGETPID)                                                
         JE    VTIM020                                                          
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INPID                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PIN                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM016  MVC   TEMP2(L'TL_PID),TL_PID                                           
         GOTOR (#GETPIN,AGETPIN)                                                
         JE    VTIM018                                                          
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INPID                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PID                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM018  MVC   DV_PIN,TEMP2+50     set PIN from PID                             
         MVC   DV_SPIDC,TL_PID                                                  
         J     VTIM022                                                          
                                                                                
VTIM020  MVC   DV_SPIDC,TEMP2      set security person code                     
         MVC   DV_PIN,TL_PIN                                                    
                                                                                
         USING PIDRECD,R2                                                       
VTIM022  LA    R2,IOKEY            get person code via PID                      
         XC    PIDKEY,PIDKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,CUXCPY                                                   
         MVC   PIDKPID,DV_PIN                                                   
         MVI   PIDKSTYP,PIDKPERQ                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JNE   *+2                                                              
                                                                                
         CLC   PIDKEY(PIDKPER-PIDKEY),IOKEYSAV                                  
         JE    VTIM024                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$PIDPR                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PIN                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM024  MVC   DV_PIDC,PIDKPER                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
                                                                                
         L     R2,AIO2                                                          
         J     VTIM040                                                          
                                                                                
         USING PERRECD,R2                                                       
VTIM026  LA    R2,IOKEY                                                         
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TL_PCC                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    VTIM028                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$PIDPR                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PCC                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM028  MVC   DV_PIDC,TL_PCC                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
                                                                                
         USING PIDELD,R3                                                        
         L     R2,AIO2                                                          
         LA    R3,PERRFST                                                       
                                                                                
VTIM030  CLI   PIDEL,PIDELQ                                                     
         JE    VTIM034                                                          
         CLI   PIDEL,0                                                          
         JNE   VTIM032                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$RECNF                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PCC                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM032  LLC   R1,PIDLN                                                         
         AR    R3,R1                                                            
         J     VTIM030                                                          
                                                                                
VTIM034  MVC   DV_PIN,PIDNO                                                     
         DROP  R3                                                               
                                                                                
         MVC   TEMP2(L'DV_PIN),DV_PIN                                           
         GOTOR (#GETPID,AGETPID)                                                
         JE    VTIM036                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INPID                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PCC                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM036  MVC   DV_SPIDC,TEMP2      set security person code                     
                                                                                
VTIM040  DS    0H                                                               
                                                                                
         USING EMPELD,R3                                                        
VTIM044  CLI   PERKTYP,PERKTYPQ    common code (R2 points to PERRECD,           
         JNE   *+2                 ensure it is so)                             
         LA    R3,PERRFST                                                       
         XC    SEMPEL,SEMPEL       Clear saved EMPEL                            
         XC    SLOCEL,SLOCEL       Clear saved LOCEL                            
                                                                                
VTIM046  CLI   EMPEL,0                                                          
         JE    VTIM058                                                          
         CLI   EMPEL,EMPELQ                                                     
         JE    VTIM050                                                          
         CLI   EMPEL,LOCELQ                                                     
         JE    VTIM054                                                          
                                                                                
VTIM048  LLC   R1,EMPLN                                                         
         AR    R3,R1                                                            
         J     VTIM046                                                          
                                                                                
VTIM050  MVC   SEMPEL,EMPEL        Save elelemnt and validate date              
         CLC   DV_DATE,EMPHIR                                                   
         JL    VTIM052                                                          
         OC    EMPTRM,EMPTRM                                                    
         JZ    VTIM048                                                          
         CLC   DV_DATE,EMPTRM                                                   
         JNH   VTIM048                                                          
                                                                                
VTIM052  MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$OHIRE                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#DATE                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP     Error but carry on                           
         J     VTIM048                                                          
                                                                                
         USING LOCELD,R3                                                        
VTIM054  CLC   DV_DATE,LOCSTART    Find location for current date               
         JL    VTIM048                                                          
         OC    LOCEND,LOCEND                                                    
         JZ    VTIM056                                                          
         CLC   DV_DATE,LOCEND                                                   
         JH    VTIM048                                                          
                                                                                
VTIM056  MVC   SLOCEL,LOCEL                                                     
         MVC   DV_1ROFF,LOCOFF                                                  
         MVC   DV_1RDEP,LOCDEPT                                                 
         MVC   DV_1RSUB,LOCSUB                                                  
         MVC   DV_1RPER,PERKCODE                                                
         USING ACTKULA,R4                                                       
         LA    R4,DV_1RULA                                                      
         MVC   ACTKULA,SPACES                                                   
         MVC   ACTKUNT(L'LEDGER1R),LEDGER1R                                     
         LLC   RF,ONERL1L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),DV_1ROFF                                              
         EX    RF,0(RE)            Move office code to key                      
         LA    R1,ACTKACT+1(RF)                                                 
         LLC   RE,ONERL1L                                                       
         LLC   RF,ONERL2L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),DV_1RDEP    Move department to key                       
         EX    RF,0(RE)                                                         
         LA    R1,1(RF,R1)                                                      
         LLC   RE,ONERL2L                                                       
         IC    RF,ONERL3L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),DV_1RSUB    Move sub-department code to key              
         EX    RF,0(RE)                                                         
         LA    R1,1(RF,R1)                                                      
         LLC   RE,ONERL3L                                                       
         IC    RF,ONERL4L                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),DV_1RPER    Move person code to key                      
         EX    RF,0(RE)                                                         
         OC    ACTKACT,SPACES                                                   
         J     VTIM048                                                          
         DROP  R2,R3,R4                                                         
                                                                                
VTIM058  CLI   SEMPEL,EMPELQ       Test EMPEL found                             
         JNE   *+2                                                              
         CLI   SLOCEL,LOCELQ       Test location element found                  
         JE    VTIM060                                                          
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$IVLDT                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#DATE                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP     Error but carry on                           
                                                                                
VTIM060  CLI   CUACCS,0            Skip if no limit access                      
         JE    VTIM062                                                          
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,DV_1ROFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         DROP  R1                                                               
         JE    VTIM062                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$SECLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PCC                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM062  GOTOR SET1RD              Set 1R details                               
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
         GOTOR GETPED              Get period end date etc.                     
         JNE   VTIMERR                                                          
                                                                                
         GOTOR DOLOCK              Lock the PID as DV_LEDAT establ. by          
         JE    VTIM064             GETPED now                                   
                                                                                
         MVC   XERRTXT,SPACES                                                   
         ICM   RF,B'0011',ROUERRV                                               
         LHI   R0,TL#GLOB                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM064  GOTOR GETCAP                                                           
         JNE   VTIMERR                                                          
                                                                                
         GOTOR GETMAP                                                           
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
         GOTOR CHKMOA              (may set error but can continue)             
         JNE   VTIMERR                                                          
                                                                                
         GOTOR VALNCA              Validate 1N account                          
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
         GOTOR VALCLI                                                           
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
         GOTOR VALPRO                                                           
         JH    VTIMERR                                                          
                                                                                
         GOTOR VALJOB                                                           
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
         TM    LP_FLAG2,LP_FMQIM   Skip access checking for MQ                  
         JNZ   VTIM065                                                          
         GOTOR VALLIM                                                           
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
VTIM065  GOTOR VALSUB              Validate cli/pr/job subsidiary data          
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
         GOTOR VALWCD                                                           
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
         GOTOR SETDFO              Set data from Options                        
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
         GOTOR VALTTY              Validate time type                           
         JNE   VTIMERR                                                          
                                                                                
         MVC   DV_ULA,DV_1NULA     set 1N/SJ account and 1N/1C account          
         MVC   DV_ULACA,DV_1NULA                                                
         TM    DV_ICPJ,DV_INON                                                  
         JNZ   VTIM065A                                                         
         MVC   DV_ULA,DV_SJULA                                                  
         MVC   DV_ULACA,DV_1CULA                                                
                                                                                
VTIM065A MVI   DV_NARRL,0                                                       
         CLC   TL_NARR(100),SPACES                                              
         JH    VTIM070                                                          
         CLC   TL_NARR+100(L'TL_NARR-100),SPACES                                
         JH    VTIM070                                                          
         TM    DV_ICPJ,DV_ICLI+DV_INON  Test production ledger                  
         JZ    VTIM076                                                          
                                                                                
         LA    R1,DV_TFNAR         Test narrative required for time             
         LHI   R0,L'DV_TFNAR                                                    
                                                                                
VTIM066  CLC   TL_TTYP,0(R1)                                                    
         JE    VTIM068                                                          
         AHI   R1,1                                                             
         JCT   R0,VTIM066                                                       
                                                                                
         CLI   DV_COFNR,C'Y'       Test forcing narrative                       
         JNE   VTIM076                                                          
                                                                                
VTIM068  MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$NARRQ                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#NARR                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP     Error but carry on                           
         J     VTIM076                                                          
                                                                                
VTIM070  LA    RE,TL_NARR+L'TL_NARR-1                                           
         LHI   RF,L'TL_NARR                                                     
                                                                                
VTIM072  CLI   0(RE),SPACEQ                                                     
         JH    VTIM074                                                          
         SHI   RE,1                                                             
         JCT   RF,VTIM072                                                       
         J     *+2                 (bug in VTIM066 TL_NARR check)               
                                                                                
VTIM074  STC   RF,DV_NARRL                                                      
                                                                                
VTIM076  DS    0H                                                               
         CLI   TL_TTYP,TL_TBILQ    Set type of time                             
         JNE   VTIM078                                                          
         MVI   DV_TTYP,TIMTCB                                                   
                                                                                
VTIM078  CLI   TL_TTYP,TL_TRELQ                                                 
         JNE   VTIM080                                                          
         MVI   DV_TTYP,TIMTCR                                                   
                                                                                
VTIM080  CLI   TL_TTYP,TL_TNONQ                                                 
         JNE   VTIM082                                                          
         MVI   DV_TTYP,TIMTCN                                                   
         CLC   PRODUL,DV_ULA                                                    
         JE    VTIM082                                                          
         MVI   DV_TTYP,TIMTNC                                                   
                                                                                
VTIM082  DS    0H                                                               
         MVC   DV_OFF,DV_SJOFF                                                  
         MVC   DV_MED,TL_JOB                                                    
         OI    DV_MED,SPACEQ                                                    
         CLC   DV_SJOFF,SPACES                                                  
         JH    VTIM084                                                          
         TM    CPYSTAT1,CPYSOROE                                                
         JZ    VTIM084                                                          
         CLI   TL_TTYP,TL_TNONQ    Was time type N                              
         JNE   VTIM084                                                          
         MVC   DV_OFF,DV_1ROFF                                                  
                                                                                
VTIM084  DS    0H                                                               
         MVI   DV_STAT,TIMSMCS                                                  
         MVC   DV_CAULA,DV_ULA                                                  
         MVC   DV_CANAM,DV_1NNAM                                                
         CLC   PRODUL,DV_ULA       It's either SJ or 1N                         
         JNE   VTIM088                                                          
         MVC   DV_CAULA,DV_1CULA   SJ so contra is 1C                           
         MVC   DV_CANAM,DV_1CNAM                                                
                                                                                
VTIM088  DS    0H                                                               
         MVC   DV_COFF,SPACES      Set contra-office                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    VTIM090                                                          
         MVC   DV_COFF,DV_SJOFF                                                 
         CLC   DV_SJOFF,SPACES                                                  
         JH    VTIM090                                                          
         MVC   DV_COFF,DV_1ROFF                                                 
         OC    DV_COFF,SPACES      Make sure it's valid for key                 
                                                                                
VTIM090  DS    0H                                                               
         TM    DV_ICPJ,DV_IJOB     Test job level                               
         JZ    VTIM092                                                          
         L     RF,ACOBLOCK         Check estimate number compulsory             
         CLI   COEST-COBLOCKD(RF),C'C'                                          
         JNE   VTIM092                                                          
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$ESTNC                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#JOB                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP     Error but carry on                           
                                                                                
VTIM092  OC    TL_HOURS,TL_HOURS                                                
         JZ    VTIM093                                                          
         CP    TL_HOURS,PZERO      (no zero hours?)                             
         JNE   VTIM094                                                          
                                                                                
VTIM093  MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$IVHRS                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#HRS                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM094  ZAP   DUB2,TL_HOURS       ensure format ok (quarter hours)             
         MP    DUB2,PFOUR                                                       
         ZAP   DUB1,DUB2                                                        
         SRP   DUB1,64-2,0                                                      
         MP    DUB1,P100                                                        
         CP    DUB1,DUB2                                                        
         JE    VTIM096                                                          
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$FMTNA                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#HRS                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP     Error but carry on                           
                                                                                
VTIM096  CP    TL_HOURS,MAXHOURS                                                
         JNH   VTIM098                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$TMHRS                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#HRS                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM098  DS    0H                                                               
         ZAP   DV_HRS,TL_HOURS                                                  
*&&UK*&& ZAP   DV_EDHRS,PZERO                                                   
*&&US*&& ZAP   DV_EDHRS,TL_HOURS                                                
                                                                                
         GOTOR RATLUP              Rates look up                                
         JH    VTIMERR             (JL for error/continue)                      
                                                                                
         GOTOR BLDARY              Build day/hours array                        
                                                                                
         TM    TWAMODE,TWAMERP     any error then skip here                     
         JNZ   VTIM200                                                          
                                                                                
         GOTOR GETNUM              Get DV_TIME#                                 
                                                                                
         L     R1,AIO3             Build cluster and set DV_DLEN                
         GOTOR BLDCLS                                                           
                                                                                
         MVI   DV_TSSTO,0                                                       
         MVI   DV_TSSTN,0                                                       
         ZAP   X_THRS,TL_HOURS                                                  
                                                                                
         USING TSWRECD,R2          Read time sheet weekly passives to           
         LA    R2,IOKEY            check t/s and contra existing                
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUXCPY                                                   
         MVC   TSWKPER,DV_1RPER                                                 
         XR    RF,RF                                                            
         ICM   RF,B'0111',DV_LEDAT                                              
         LNR   RF,RF                                                            
         STCM  RF,B'0111',TSWKEND                                               
         LLC   RF,ONERL3L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   TSWKODS(0),DV_1RULA+2                                            
         EX    RF,0(RE)                                                         
         OC    TSWKODS,SPACES                                                   
         XC    CSVKEY3,CSVKEY3                                                  
         XC    CSVKEY4,CSVKEY4                                                  
         MVI   BYTE1,NOQ                                                        
                                                                                
         L     R1,=AL4(IOHI+IODIR+IO2)                                          
         TM    LP_FLAG2,LP_FMQIM   If MQ read for update                        
         JZ    *+8                                                              
         L     R1,=AL4(IOHIUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         J     VTIM104                                                          
                                                                                
VTIM102  LA    R2,IOKEY                                                         
         MVI   BYTE1,NOQ                                                        
         L     R1,=AL4(IOSQ+IODIR+IO2)                                          
         TM    LP_FLAG2,LP_FMQIM   If MQ read for update                        
         JZ    *+8                                                              
         L     R1,=AL4(IOSQUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
                                                                                
VTIM104  JNE   *+2                                                              
         CLC   TSWKEY(TSWKULC-TSWKEY),IOKEYSAV                                  
         JNE   VTIM150             No data for this Person/Week/ODS             
                                                                                
         OI    DV_TSSTA,DV_TSSWQ   Indicate we found Person/Week/ODS            
         MVC   DV_TSSTO,TSWKSTAT                                                
         MVC   DV_TSSTN,TSWKSTAT                                                
                                                                                
         CLI   TSWKSTAT,0          Ensure t/s is in progress                    
         JE    VTIM106                                                          
                                                                                
         MVC   XERRTXT,SPACES                                                   
*&&UK*&& LHI   RF,AE$INSTA                                                      
*&&US*&& LHI   RF,AE$ITSTA                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#DATE                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     VTIMERR                                                          
                                                                                
VTIM106  CLC   TSWKULC,DV_ULACA    Match on contra account?                     
         JNE   VTIM108                                                          
         OI    DV_TSSTA,DV_TSCAQ                                                
         MVI   BYTE1,YESQ                                                       
         DROP  R2                                                               
                                                                                
VTIM108  L     R1,=AL4(IOGET+IOMST+IO2)                                         
         TM    LP_FLAG2,LP_FMQIM   If MQ read for update                        
         JZ    *+8                                                              
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         USING TIMRECD,R2                                                       
         L     R2,AIO2             Get record to check clusters                 
         USING TIMELD,R3                                                        
         LA    R3,TIMRFST                                                       
         CLI   BYTE1,YESQ          DV_TSCAQ then save TIMKEY                    
         JNE   VTIM112                                                          
         MVC   CSVKEY4,TIMKEY                                                   
                                                                                
VTIM110  TM    DV_TSSTA,DV_TSSPQ+DV_TSCLQ  do we already have a                 
         JNZ   VTIM112                      matching cluster or space           
         XR    RE,RE                         for a new cluster                  
         ICM   RE,B'0011',TIMRLEN                                               
         LH    RF,DV_DLEN                                                       
         AR    RE,RF                                                            
         CHI   RE,TIMMAXLN                                                      
         JH    VTIM112                                                          
                                                                                
         MVC   CSVKEY3,TIMKEY                                                   
         OI    DV_TSSTA,DV_TSSPQ                                                
                                                                                
VTIM112  CLI   TIMEL,0             end of current record                        
         JE    VTIM102             then get next                                
         CLI   TIMEL,TIMELQ                                                     
         JE    VTIM116                                                          
                                                                                
VTIM114  LLC   R1,TIMLN                                                         
         AR    R3,R1                                                            
         J     VTIM112                                                          
                                                                                
VTIM116  CLI   TIMETYP,TIMETIME    Total up hours for day                       
         JNE   VTIM117                                                          
         XR    RE,RE                                                            
         LLC   RF,DV_DOTSP         Displacement of day in period                
         BCTR  RF,0                                                             
         LA    R4,TIMEDAY          R3=A(first day of period)                    
DATIM    USING TIMEDAY,R4                                                       
         LA    R1,L'TIMEDAY        R1=length of each date and hrs               
         MR    RE,R1               RF=displacement into element                 
         AR    R4,RF               R3=A(date of time in element)                
         AP    X_THRS,DATIM.TIMEHRS                                             
         J     VTIM114                                                          
         DROP  DATIM                                                            
*                                                                               
VTIM117  CLI   TIMETYP,TIMEINP     main element?                                
         JNE   VTIM114                                                          
         TM    DV_TSSTA,DV_TSCLQ   If we have good cluster skip                 
         JNZ   VTIM114              checking again                              
                                                                                
VTIM118  CLC   TIMACC,DV_ULA       match on 1N/SJ?                              
         JNE   VTIM114                                                          
         CLC   TIMTSK,TL_WCD       match work code?                             
         JNE   VTIM114                                                          
         LA    RE,DV_1ROFF                                                      
         CLC   PRODUL,TIMACC                                                    
         JNE   VTIM120                                                          
         LA    RE,DV_SJOFF                                                      
                                                                                
VTIM120  CLC   TIMOFF,0(RE)        match on 1R/SJ office?                       
         JNE   VTIM114                                                          
         CLC   TIMTTYP,DV_TTYP     match on time type?                          
         JNE   VTIM114                                                          
         CLC   TIMMOA,DV_MOA       match on MOA?                                
         JNE   VTIM114                                                          
                                                                                
         LA    RF,TIMELD                                                        
         XR    R0,R0                                                            
NAR      USING TIMELD,RF                                                        
VTIM130  LLC   R1,NAR.TIMLN                                                     
         AR    RF,R1                                                            
         CLI   NAR.TIMEL,0                                                      
         JE    VTIM132                                                          
         CLI   NAR.TIMEL,TIMELQ                                                 
         JNE   VTIM130                                                          
         CLC   NAR.TIMSEQ,TIMSEQ                                                
         JNE   VTIM132                                                          
         CLI   NAR.TIMETYP,TIMENAR                                              
         JNE   VTIM130                                                          
         CLI   NAR.TIMLN,TIMNLNQ                                                
         JNH   VTIM130                                                          
         LLC   R0,NAR.TIMLN                                                     
         SHI   R0,TIMNARR-TIMELD                                                
                                                                                
VTIM132  LLC   RE,DV_NARRL         check for 'same' narrative                   
         CR    RE,R0                                                            
         JNE   VTIM114             skip cluster if not same length              
         CHI   RE,0                                                             
         JE    VTIM140                                                          
         SHI   RE,1                                                             
         CLC   TL_NARR(0),NAR.TIMNARR                                           
         EX    RE,*-6                                                           
         JNE   VTIM114             skip cluster if not same contents            
                                                                                
VTIM140  OI    DV_TSSTA,DV_TSCLQ   good cluster to use found                    
         MVC   DV_TKEY,TIMKEY      save key and exit here                       
         MVC   DV_TCSEQ,TIMSEQ     save sequence                                
         NI    DV_TSSTA,FFQ-DV_TSSPQ                                            
         J     VTIM114             Still have to read other time recs           
*                                   to get total hours for day                  
*                                                                               
* TSWRECD/TIMRECD processing done                                               
*                                                                               
VTIM150  TM    LP_FLAG2,LP_FMQIM   If MQ then skip check                        
         JNZ   VTIM151                                                          
         CP    X_THRS,MAXHOURS     Already more than 24 hours on t/s?           
         JNH   VTIM151                                                          
         MVC   XERRTXT,SPACES      Then fatal error exit                        
         LHI   RF,AE$TM24H                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#HRS                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMEDP                                                  
         J     VTIMERR                                                          
*                                                                               
VTIM151  TM    DV_TSSTA,DV_TSCLQ   Did we find a matching cluster?              
         JNZ   VTIM200                                                          
         LH    RE,DV_TIME#         In any other case increment line#            
         AHI   RE,1                                                             
         STH   RE,DV_TIME#                                                      
         TM    DV_TSSTA,DV_TSSPQ   Did we find a record with spare?             
         JZ    VTIM152                                                          
         MVC   DV_TKEY,CSVKEY3     Set key and increment row number             
         J     VTIM200                                                          
                                                                                
VTIM152  TM    DV_TSSTA,DV_TSCAQ   Did we find a match on contra a/c?           
         JZ    VTIM154                                                          
         MVC   DV_TKEY,CSVKEY4     Save key, increment number                   
         LLC   RE,DV_TKEY+TIMKSBR-TIMRECD                                       
         CHI   RE,FFQ                                                           
         JE    *+2                                                              
         AHI   RE,1                                                             
         STC   RE,DV_TKEY+TIMKSBR-TIMRECD                                       
         OI    DV_TSSTA,DV_TSADQ   and indicate add                             
         LA    R2,IOKEY                                                         
         MVC   TIMKEY,DV_TKEY                                                   
         L     R1,=AL4(IORDD+IODIR+IO2)                                         
         TM    LP_FLAG2,LP_FMQIM   If MQ read for update                        
         JZ    *+8                                                              
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,IOERNF        OK if not found                              
         JE    VTIM200                                                          
         CLI   IOERR,IOEDEL                                                     
         JNE   *+2                 Is there a bug in the TSW IO logic?          
         OI    DV_TSSTA,DV_TSRAQ   indicate deleted                             
         NI    DV_TSSTA,FFQ-DV_TSADQ                                            
         J     VTIM200                                                          
                                                                                
VTIM154  LA    R2,TIMKEY           build new TIMKEY                             
         MVC   TIMKEY,SPACES                                                    
         MVC   TIMKCPY,CUXCPY                                                   
         MVC   TIMKULA,DV_1RULA                                                 
         MVC   TIMKOFF,DV_COFF                                                  
         MVC   TIMKCCPY,CUXCPY                                                  
         MVC   TIMKULC,DV_CAULA                                                 
         MVC   TIMKPEDT,DV_LEDAT                                                
         MVC   TIMKREF,TIMEREF                                                  
         MVI   TIMKSBR,0                                                        
         MVC   DV_TKEY,TIMKEY                                                   
         OI    DV_TSSTA,DV_TSADQ   and indicate add                             
         L     R1,=AL4(IORDD+IODIR+IO2)                                         
         TM    LP_FLAG2,LP_FMQIM   If MQ read for update                        
         JZ    *+8                                                              
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,IOERNF        OK if not found                              
         JE    VTIM200                                                          
         CLI   IOERR,IOEDEL                                                     
         JNE   *+2                 Is there a bug in the TSW IO logic?          
         OI    DV_TSSTA,DV_TSRAQ   indicate deleted                             
         NI    DV_TSSTA,FFQ-DV_TSADQ                                            
         DROP  R2,R3                                                            
                                                                                
VTIMTH2  DS    0H                                                               
                                                                                
VTIM200  TM    TWAMODE,TWAMERP     any error then error now                     
         JNZ   VTIMERR                                                          
                                                                                
VTIM300  MVI   DV_INDIC,YESQ                                                    
                                                                                
         TM    LP_FLAG2,LP_FMQIM   If MQ then handle via WMP                    
         JNZ   VTIM302                                                          
                                                                                
VTIMTH4  DS    0H                                                               
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTLRQ',TL#DV1),  +        
               ('LQ_TSINQ',DV_VALS1),DV_VAL1L                                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTLRQ',TL#DV2),  +        
               ('LQ_TSINQ',DV_VALS2),DV_VAL2L                                   
         LARL  R6,RETDATEL                                                      
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),(R6)                
                                                                                
         J     VTIM304                                                          
                                                                                
VTIM302  DS    0H                                                               
                                                                                
VTIM304  DS    0H                                                               
                                                                                
         TM    LP_FLAG2,LP_FMQIM   If MQ then handle via WMP                    
         JNZ   VTIM306                                                          
                                                                                
VTIMTH6  DS    0H                                                               
                                                                                
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Build download map element                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',TL#IND),      +        
               ('LD_CHARQ',DV_INDIC),(L'DV_INDIC,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',TL#PERNO),    +        
               ('LD_LBINQ',DV_PERNO),(L'DV_PERNO,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',TL#PERST),    +        
               ('LD_PDATQ',DV_PSDAT),(L'DV_PSDAT,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',TL#PEREN),    +        
               ('LD_PDATQ',DV_PEDAT),(L'DV_PEDAT,0)                             
         J     VTIM308                                                          
                                                                                
VTIM306  LARL  RF,OMRET            Return details                               
         ST    RF,LP_AOMAP                                                      
         GOTOR LP_APUTO,LP_D                                                    
                                                                                
VTIM308  TM    LP_FLAG2,LP_FMQIM   If MQ go to update                           
         JNZ   UPDTIM                                                           
         J     UEXITY              else exit                                    
                                                                                
VTIMERR  DS    0H                                                               
                                                                                
         OI    TWAMODE,TWAMEDP+TWAMPER                                          
         MVC   DV_ERROR,ROUERRV                                                 
         MVI   DV_INDIC,NOQ                                                     
                                                                                
         GOTOR UNLOCK              Yes - unlock all we have                     
                                                                                
         TM    LP_FLAG2,LP_FMQIM   If MQ then handle via WMP                    
         JNZ   VTIMERR8                                                         
                                                                                
VTIMTH8  DS    0H                                                               
                                                                                
         XR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Build download map element                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',TL#IND),      +        
               ('LD_CHARQ',DV_INDIC),(L'DV_INDIC,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',TL#PERNO),    +        
               ('LD_LBINQ',DV_PERNO),(L'DV_PERNO,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',TL#PERST),    +        
               ('LD_PDATQ',DV_PSDAT),(L'DV_PSDAT,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',TL#PEREN),    +        
               ('LD_PDATQ',DV_PEDAT),(L'DV_PEDAT,0)                             
                                                                                
         USING LIOBD,R4                                                         
         L     R4,AALIOB           after updates pass return data               
                                                                                
         USING ET_D,R2                                                          
         LAY   R2,ERRTAB           Send errors                                  
                                                                                
VTIMERR2 CLI   ET_D,ET_EOTQ        Test end of error table                      
         JE    VTIMERRA                                                         
                                                                                
         LLC   RF,ERRCNT           Check for maximum number of error            
         CHI   RF,ERRMAX           messages                                     
         JNL   VTIMERRA                                                         
                                                                                
         XR    R0,R0               Build download map element                   
         ICM   R0,B'0011',LP_QMAPN                                              
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
                                                                                
         USING GETTXTD,R1                                                       
         LA    R1,DMCB             Build error text string in ELEMENT           
         XC    GTBLOCK,GTBLOCK     Resolve error text                           
         MVI   GTMAXL,80                                                        
         MVC   GTMSGNO,ET_ERRNO                                                 
         LA    R0,ELEMENT                                                       
         STCM  R0,B'0111',GTAOUT                                                
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LLC   R0,ET_LN                                                         
         SHI   R0,ET_LN1Q                                                       
         LTR   R0,R0                                                            
         JZ    VTIMERR4                                                         
         STC   R0,GTLTXT           Set length of extra text                     
         LA    R0,ET_EXTRA                                                      
         STCM  R0,B'0111',GTATXT   Set A(Extra text)                            
                                                                                
VTIMERR4 GOTOR VGETTXT,(R1)                                                     
         LLC   R0,4(R1)            Length of text just added                    
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',TL#ERROR),   +        
               ('LD_CHARQ',ELEMENT),((R0),0)                                    
         OC    ET_FLDNM,ET_FLDNM                                                
         JZ    VTIMERR6                                                         
         GOTOR AALINKIO,(R1),('LIOAPUT',LIOBD),('LIOTRAW',TL#ERRRN),   +        
               ('LD_LBINQ',ET_FLDNM),(L'ET_FLDNM,0)                             
                                                                                
VTIMERR6 LLC   R0,ET_LN            Bump to next error table entry               
         AR    R2,R0                                                            
                                                                                
         LLC   RF,ERRCNT           Error counter                                
         AHI   RF,1                                                             
         STC   RF,ERRCNT                                                        
                                                                                
         J     VTIMERR2                                                         
         DROP  R1,R2,R4                                                         
                                                                                
VTIMERR8 DS    0H                                                               
                                                                                
         LARL  RF,OMRET            Return details                               
         ST    RF,LP_AOMAP                                                      
         GOTOR LP_APUTO,LP_D                                                    
                                                                                
VTIMERRA TM    TWAMODE,TWAMUWD     unwind?                                      
         JZ    VTIMERRZ                                                         
                                                                                
         NI    TWAMODE,FF-TWAMUWD                                               
         OI    GIND1,GIUNWIND      DDLINK will unwind/abend                     
         MVI   LP_RMODE,LP_RERRR                                                
         MVI   LP_EMSYS,6                                                       
         J     EXITN                                                            
                                                                                
VTIMERRZ TM    LP_FLAG2,LP_FMQIM   If MQ then exit Y  (???)                     
         JNZ   UEXITY                                                           
                                                                                
         J     UEXITN                                                           
                                                                                
*** Update time with request data ***                                           
                                                                                
UPDTIM   DS    0H                                                               
                                                                                
         CLC   TL_PCC,THOMASQ      *** special THOMAS mode ***                  
         JE    UTIMTH2             *** special THOMAS mode ***                  
                                                                                
         TM    LP_FLAG2,LP_FMQIM   Not if under MQ                              
         JNZ   UTIM00                                                           
                                                                                
UTIMTH2  DS    0H                  *** special THOMAS mode ***                  
                                                                                
                                                                                
         ICM   RE,15,TL_AHED1      Test data passed and copy it                 
         JZ    *+2                                                              
         LA    R0,DV_VALS1                                                      
         LHI   R1,DV_VAL1L                                                      
         AHI   RE,LW_LN1Q                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         ICM   RE,15,TL_AHED2      and block 2                                  
         JZ    *+2                                                              
         LA    R0,DV_VALS2                                                      
         LHI   R1,DV_VAL2L                                                      
         AHI   RE,LW_LN1Q                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
UTIM00   GOTOR SETTIME                                                          
                                                                                
         GOTOR TRACEIT,UPDTIMQ     Trace request                                
                                                                                
         TM    LP_FLAG2,LP_FMQIM   If MQ then ok else check                     
         JNZ   *+14                                                             
         OC    DV_SPIDC,DV_SPIDC                                                
         JZ    *+2                 (MQ SAVED not intact)                        
                                                                                
         OC    DV_ERROR,DV_ERROR                                                
         JZ    UTIM02                                                           
         TM    LP_FLAG2,LP_FMQIM   If MQ then ok else die                       
         JZ    *+2                                                              
         OI    TWAMODE,TWAMEMQ     save for EREQ                                
         J     UEXITY                                                           
                                                                                
UTIM02   DS    0H                                                               
                                                                                
         CLC   TL_PCC,THOMASQ      *** special THOMAS mode ***                  
         JNE   UTIMTH3             *** special THOMAS mode ***                  
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            get record for update                        
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,DV_PIDC                                                 
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R2,AIO2                                                          
         USING SCIELD,R3                                                        
         LA    R3,ELEMENT                                                       
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITNULL                                                 
         ZAP   SCIAMNT,TL_HOURS                                                 
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),PERRECD,SCIELD,ADDEND,0                
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
                                                                                
         DS    0H                  put back record                              
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
         J     UTIMTH4                                                          
                                                                                
UTIMTH3  DS    0H                                                               
                                                                                
         USING TIMRECD,R2                                                       
UTIM10   LA    R2,IOKEY            Time and audit updates (see SRUPD60)         
                                                                                
         TM    DV_TSSTA,DV_TSADQ   Adding new TIMREC?                           
         JZ    UTIM20                                                           
         XC    IOKEY,IOKEY                                                      
                                                                                
UTIM12   L     R2,AIO2             (entry point for re-add)                     
         LR    RE,R2                                                            
         LHI   RF,IOLENQ                                                        
         XCEFL                                                                  
         MVC   TIMKEY,DV_TKEY                                                   
         MVC   TIMRSTAT,DV_TSSTN                                                
         MVI   TIMRRI2,TIMKRI2Q                                                 
         MVI   TIMRRI3,TIMKRI3Q                                                 
         MVI   TIMRRI4,TIMKRI4Q                                                 
                                                                                
EL       USING PIDELD,TIMRFST                                                   
         MVI   EL.PIDEL,PIDELQ     Build PIDEL at front of time record          
         MVI   EL.PIDLN,PIDLNQ                                                  
         MVC   EL.PIDNO,DV_PIN                                                  
         DROP  EL                                                               
                                                                                
         LHI   R1,TIMRFST-TIMRECD+1+PIDLNQ                                      
         STCM  R1,B'0011',TIMRLEN                                               
                                                                                
                                                                                
         TM    CPYSTAT9,CPYSEDHO                                                
         JZ    UTIM14                                                           
                                                                                
         USING SCIELD,R3                                                        
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITEDTH                                                 
         ZAP   SCIAMNT,DV_EDHRS                                                 
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),TIMRECD,SCIELD,ADDEND,0                
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
         DROP  R3                                                               
                                                                                
         USING TIMELD,R3                                                        
UTIM14   LA    R3,TIMRFST          Find end of TIMREC                           
                                                                                
UTIM16   LLC   R1,TIMLN                                                         
         AR    R3,R1                                                            
         CLI   TIMEL,0                                                          
         JNE   UTIM16                                                           
                                                                                
         GOTOR BLDCLS,TIMELD       Build cluster and set DV_DLEN                
         XR    RE,RE                                                            
         ICM   RE,B'0011',DV_DLEN                                               
         XR    RF,RF                                                            
         ICM   RF,B'0011',TIMRLEN                                               
         AR    RF,RE                                                            
         CHI   RF,TIMMAXLN         (integrity check)                            
         JH    *+2                                                              
         STCM  RF,B'0011',TIMRLEN  update record length                         
                                                                                
         GOTOR SETSEQ,TIMRFST                                                   
                                                                                
         TM    DV_TSSTA,DV_TSRAQ                                                
         JNZ   UTIM18                                                           
                                                                                
         DS    0H                  Add TIMREC now                               
         GOTOR SETMOS                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
         USING CPTRBLK,R3                                                       
                                                                                
         LA    R3,ELEMENT                                                       
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         MVC   LDGLVALN(4),ONERL1L                                              
                                                                                
         GOTO1 VPADDLE,DMCB,(C'A',AIO2),CPTRBLK,IODA,0,ACOMFACS                 
         DROP  R3                                                               
         J     UTIM60                                                           
                                                                                
UTIM18   DS    0H                  Re-add TIMREC now                            
         GOTOR SETMOS                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
KY       USING TIMRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   KY.TIMKEY,TIMKEY                                                 
         MVC   KY.TIMKSTA,TIMRSTA                                               
         CLC   MYIOADR,KY.TIMKDA                                                
         JNE   *+2                 (disk address integrity check)               
         DROP  KY                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JNE   *+2                                                              
                                                                                
         J     UTIM60                                                           
                                                                                
UTIM20   TM    DV_TSSTA,DV_TSRAQ+DV_TSCLQ+DV_TSSPQ                              
         JZ    *+2                 (unknown scenario)                           
                                                                                
         MVC   TIMKEY,DV_TKEY                                                   
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         TM    DV_TSSTA,DV_TSRAQ                                                
         JZ    UTIM22                                                           
         L     R1,=AL4(IORDUPD+IODIR+IO2)                                       
                                                                                
UTIM22   GOTOR (#IOEXEC,AIOEXEC)                                                
         TM    DV_TSSTA,DV_TSRAQ                                                
         JNZ   UTIM24                                                           
         CLI   IOERR,0                                                          
         JNE   *+2                                                              
         J     UTIM26                                                           
                                                                                
UTIM24   CLI   IOERR,IOEDEL                                                     
         JNE   *+2                                                              
                                                                                
UTIM26   MVC   MYIOADR,IODA                                                     
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         TM    DV_TSSTA,DV_TSRAQ   if readd clear record and build new          
         JNZ   UTIM12                                                           
                                                                                
         L     R2,AIO2                                                          
         TM    DV_TSSTA,DV_TSCLQ   add time to existing cluster?                
         JZ    UTIM40                                                           
                                                                                
         USING TIMELD,R3                                                        
         LA    R3,TIMRFST                                                       
         J     UTIM30                                                           
                                                                                
UTIM28   LLC   R1,TIMLN                                                         
         AR    R3,R1                                                            
                                                                                
UTIM30   CLI   TIMEL,0                                                          
         JE    *+2                                                              
         CLI   TIMEL,TIMELQ                                                     
         JNE   UTIM28                                                           
         CLI   TIMETYP,TIMEINP                                                  
         JNE   UTIM28                                                           
         CLC   TIMSEQ,DV_TCSEQ                                                  
         JNE   UTIM28                                                           
                                                                                
         AP    TIMHRS,TL_HOURS     add to total row hours                       
         MVC   TIMADAT,DV_TODAY                                                 
                                                                                
UTIM32   LLC   R1,TIMLN            now lºook for TIMETIME element               
         AR    R3,R1                                                            
         CLI   TIMEL,0                                                          
         JE    *+2                                                              
         CLI   TIMEL,TIMELQ                                                     
         JNE   UTIM32                                                           
         CLI   TIMETYP,TIMETIME                                                 
         JNE   UTIM32                                                           
         CLC   TIMSEQ,DV_TCSEQ                                                  
         JNE   UTIM32                                                           
                                                                                
         LA    RE,DV_DHVAL                                                      
         LA    R1,TIMEDAY                                                       
                                                                                
UTIM34   OC    0(L'TIMETDTE,RE),0(RE)  For each date add hours                  
         JZ    UTIM50                                                           
         CLC   0(L'TIMETDTE,R1),0(RE)  Datew check                              
         JNE   *+2                     (data integrity check)                   
         AP    L'TIMETDTE(L'TIMEHRS,R1),L'TIMETDTE(L'TIMEHRS,RE)                
         AHI   R1,L'TIMEDAY            Bump along timel element                 
         AHI   RE,L'TIMEDAY            Bump along buffer record                 
         J     UTIM34                                                           
                                                                                
UTIM40   TM    DV_TSSTA,DV_TSSPQ   spare cluster                                
         JZ    *+2                 (what else?                                  
                                                                                
         USING TIMELD,R3                                                        
         LA    R3,TIMRFST                                                       
                                                                                
UTIM42   CLI   TIMEL,0             find end of record                           
         JE    UTIM44                                                           
         LLC   R1,TIMLN                                                         
         AR    R3,R1                                                            
         J     UTIM42                                                           
                                                                                
UTIM44   GOTOR BLDCLS,TIMELD       Build cluster and set DV_DLEN                
         XR    RE,RE                                                            
         ICM   RE,B'0011',DV_DLEN                                               
         XR    RF,RF                                                            
         ICM   RF,B'0011',TIMRLEN                                               
         AR    RF,RE                                                            
         CHI   RF,TIMMAXLN         (integrity check)                            
         JH    *+2                                                              
         STCM  RF,B'0011',TIMRLEN  update record length                         
                                                                                
         GOTOR SETSEQ,TIMRFST                                                   
         DROP  R3                                                               
                                                                                
UTIM50   DS    0H                  Update TIMREC now                            
         GOTOR SETMOS                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
KY       USING TIMRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   KY.TIMKEY,TIMKEY                                                 
         MVC   KY.TIMKSTA,TIMRSTA                                               
         CLC   MYIOADR,KY.TIMKDA                                                
         JNE   *+2                 (disk address integrity check)               
         DROP  KY                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JNE   *+2                                                              
                                                                                
         USING AUDRECD,R2                                                       
UTIM60   GOTOR BLDTRN                                                           
         LA    R2,IOKEY                                                         
         TM    DV_AUSTA,DV_AUSAQ           adding a record?                     
         JZ    UTIM70                                                           
                                                                                
         XC    IOKEY,IOKEY                                                      
         L     R2,AIO2                                                          
         LR    RE,R2                                                            
         LHI   RF,IOLENQ                                                        
         XCEFL                                                                  
         MVC   AUDKEY,DV_AKEY                                                   
         MVC   AUDRSTAT,DV_TSSTN                                                
         MVI   AUDRSTA2,0                                                       
         MVC   AUDRSTDT,DV_TODC                                                 
         MVC   AUDRENDT,DV_TODC                                                 
         MVC   AUDRINDX,DV_INDEX                                                
                                                                                
         GOTOR BLDSTC,AUDRFST                                                   
                                                                                
         LHI   RE,AUDRFST-AUDRECD                                               
         AH    RE,HALF1            Length from BLDSTC                           
         CHI   RE,AUDMAXLN                                                      
         JH    *+2                 (integrity check)                            
         STCM  RE,B'0011',AUDRLEN                                               
                                                                                
         DS    0H                  Add AUDREC now                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JNE   *+2                                                              
         J     UTIM80                                                           
                                                                                
UTIM70   MVC   AUDKEY,DV_AKEY      Add to existing audit record                 
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                 (DV_AKEY logic?)                             
                                                                                
         CLC   AUDKINDX,DV_INDEX                                                
         JNL   *+2                 (data integrity check)                       
                                                                                
         MVC   MYIOADR,IODA                                                     
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R2,AIO2                                                          
         USING STCELD,R3                                                        
         LA    R3,AUDRFST                                                       
                                                                                
UTIM72   CLI   STCEL,0             find end of record                           
         JE    UTIM74                                                           
         LLC   R1,STCLN                                                         
         AR    R3,R1                                                            
         J     UTIM72                                                           
                                                                                
UTIM74   GOTOR BLDSTC,STCELD       add to the end                               
                                                                                
         LH    RE,HALF1                                                         
         XR    RF,RF                                                            
         ICM   RF,B'0011',AUDRLEN                                               
         AR    RF,RE                                                            
         CHI   RF,AUDMAXLN         (integrity check)                            
         JH    *+2                                                              
         STCM  RF,B'0011',AUDRLEN  update record length                         
         MVC   AUDRENDT,DV_TODC                                                 
         MVC   AUDRINDX,DV_INDEX                                                
                                                                                
         DS    0H                  Update AUDREC now                            
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
KY       USING AUDRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   KY.AUDKEY,AUDKEY                                                 
         MVC   KY.AUDKSTA,AUDRSTA                                               
         CLC   MYIOADR,KY.AUDKDA                                                
         JNE   *+2                 (disk address integrity check)               
         DROP  KY                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JNE   *+2                                                              
                                                                                
UTIM80   LA    R2,IOKEY            Update all audit records with                
         MVC   AUDKEY,DV_AKEY      AUDRINDX and AUDRENDT                        
         MVI   BYTE1,0                                                          
                                                                                
UTIM82   LA    R2,IOKEY                                                         
         MVC   AUDKSEQ,BYTE1                                                    
         CLC   BYTE1,DV_AKEY+AUDKSEQ-AUDRECD                                    
         JNE   UTIM84                                                           
         LLC   R1,BYTE1                                                         
         CHI   R1,FFQ                                                           
         JE    UTIM90                                                           
         AHI   R1,1                                                             
         STC   R1,BYTE1                                                         
         MVC   AUDKSEQ,BYTE1                                                    
                                                                                
UTIM84   L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UTIM86                                                           
         CLC   AUDKEY(AUDKSEQ-AUDRECD),DV_AKEY                                  
         JNE   UTIM90                                                           
         J     UTIM88                                                           
                                                                                
UTIM86   MVC   MYIOADR,IODA                                                     
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R2,AIO2                                                          
         MVC   AUDRENDT,DV_TODC                                                 
         MVC   AUDRINDX,DV_INDEX                                                
                                                                                
         DS    0H                  Update other AUDRECs now                     
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
         LA    R2,IOKEY                                                         
         MVC   AUDKENDT,DV_TODC                                                 
         MVC   AUDKINDX,DV_INDEX                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO2'                            
         JNE   *+2                                                              
                                                                                
UTIM88   LLC   R1,BYTE1                                                         
         CHI   R1,FFQ                                                           
         JE    UTIM90                                                           
         AHI   R1,1                                                             
         STC   R1,BYTE1                                                         
         J     UTIM82                                                           
         DROP  R2                                                               
                                                                                
UTIMTH4  DS    0H                                                               
                                                                                
UTIM90   DS    0H                                                               
         GOTOR UPDTSN              Unlock locket record offline                 
         JNE   *+2                                                              
                                                                                
         J     UEXITY              >>> All done now                             
                                                                                
OMRET    LKOUT H                                                                
         LKOUT R,R#TLUPL                                                        
Indic    LKOUT C,TL#IND,(D,B#SAVED,DV_INDIC),CHAR,ND=Y                          
ErrList  LKOUT C,TL#ERROR,(A,ARYERR),FILTROUT=TSTERR                            
Perno    LKOUT C,TL#PERNO,(D,B#SAVED,DV_PERNO),LBIN,ND=Y                        
Perst    LKOUT C,TL#PERST,(D,B#SAVED,DV_PSDAT),PDAT,ND=Y                        
Peren    LKOUT C,TL#PEREN,(D,B#SAVED,DV_PEDAT),PDAT,ND=Y                        
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
ARYERR   LKOUT A,(R,NXTERR),MULTIROW=Y,ROWNAME=SAVED                            
Error    LKOUT C,TL#ERROR,DV_ERMSG,CHAR,ND=Y                                    
Row      LKOUT C,TL#ERRRN,DV_ERROW,UBIN,ND=Y                                    
         LKOUT E                                                                
                                                                                
         DS    0H                                                               
TSTERR   CLI   DV_INDIC,NOQ                                                     
         J     EXIT                                                             
                                                                                
         USING ET_D,R2                                                          
NXTERR   ST    R8,LP_ADATA                                                      
                                                                                
         L     R2,AERRNOW                                                       
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTERR2                                                          
         LA    R2,ERRTAB                                                        
         J     NXTERR4             PATCH BY TKLU                                
                                                                                
NXTERR2  CLI   ET_LN,ET_EOTQ                                                    
         JNE   NXTERR4                                                          
         MVI   LP_RMODE,LP_RLAST                                                
         J     EXITN                                                            
                                                                                
         USING GETTXTD,R1                                                       
NXTERR4  LA    R1,DMCB             Build error text string                      
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'DV_ERMSG                                                
         MVC   GTMSGNO,ET_ERRNO                                                 
         MVC   DV_ERMSG,SPACES                                                  
         LA    R0,DV_ERMSG                                                      
         STCM  R0,B'0111',GTAOUT                                                
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LLC   R0,ET_LN                                                         
         SHI   R0,ET_LN1Q                                                       
         LTR   R0,R0                                                            
         JZ    NXTERR6                                                          
         STC   R0,GTLTXT           Set length of extra text                     
         LA    R0,ET_EXTRA                                                      
         STCM  R0,B'0111',GTATXT   Set A(Extra text)                            
                                                                                
NXTERR6  GOTOR VGETTXT,(R1)                                                     
         DROP  R1                                                               
                                                                                
         MVC   DV_ERROW,ET_FLDNM                                                
                                                                                
         LLC   R1,ET_LN                                                         
         AR    R2,R1                                                            
         ST    R2,AERRNOW                                                       
         MVI   LP_RMODE,LP_RNEXT                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
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
                                                                                
         GOTOR GOATRN,0            Call GOATRN for last time                    
                                                                                
         TM    LP_FLAG,LP_FOFFL    Ensure off-line                              
         JNZ   EREQ2                                                            
                                                                                
         L     R1,WRKBLKR          Copy WRKR parameters                         
         MVC   PARM(WRKINDX-WRKIPARM),WRKIPARM-WRKIOD(R1)                       
         GOTOR DATAMGR,PARM,CLOSE  Close the FACWRK file                        
                                                                                
EREQ2    OI    LP_FLAG,LP_FFWRK    Set FACWRK file built                        
         L     RF,ACOMFACS                                                      
         MVC   VDATAMGR,DATAMGR    Reset real datamgr                           
         MVC   CDATAMGR-COMFACSD(,RF),DATAMGR                                   
                                                                                
         TM    LP_INDS,LP_IGLOB    Updative, test global updative file          
         JZ    EREQX               Cannot update                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY                                                            
                                                                                
         GOTOR DATAMGR,DMCB,DMCOMMIT,0                                          
                                                                                
         TM    LP_FLAG2,LP_FMQIM                                                
         JZ    EREQ4                                                            
         TM    TWAMODE,TWAMEMQ                                                  
         JZ    EREQ4                                                            
         NI    TWAMODE,FF-TWAMEMQ                                               
         J     EREQX                                                            
                                                                                
EREQ4    DS    0H                                                               
         TM    RUNINDS,RUNIPUTF                                                 
         JZ    EREQX               Nothing to do if Sorter empty                
         LA    RE,TSARPASS         Pass A(TSARPASS) in LP_ABLK3                 
         LA    RF,TSARRECS         Pass A(TSARRECS) in LP_ABLK4                 
         LA    R0,TSARRECS         Dummy not used for Media                     
         LA    R1,TSARRECS         Dummy not used for Media                     
         LA    R2,TSAROBUF         Pass A(TSAROBUF) in LP_ABLK7                 
         LA    R3,ASORTER          Pass A(SORTER) in LP_ABLK8                   
         STM   RE,R3,LP_ABLK3                                                   
         GOTOR ASRUPD60,DMCB,('FF',LP_D),('00',PARM),AIO1                       
                                                                                
         GOTOR DATAMGR,DMCB,DMCOMMIT,0                                          
                                                                                
EREQX    DS    0H                                                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* CALL LINKIO TO BUILD ERROR RETURN ELEMENT                           *         
***********************************************************************         
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTERR*'                                                      
                                                                                
         STCM  R1,3,WORK                                                        
                                                                                
         CLC   XERRTXT,SPACES                                                   
         JNH   PUTERR2                                                          
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,(L'XERRTXT,XERRTXT)                                         
         J     PUTERR4                                                          
                                                                                
PUTERR2  GOTOR AALINKIO,DMCB,('LIOAPUT',AALIOB),('LIOTERR',D#UPLERR),  *        
               WORK,0                                                           
                                                                                
PUTERR4  MVC   XERRTXT,SPACES                                                   
                                                                                
PUTERRX  J     EXITY                                                            
                                                                                
***********************************************************************         
* Timeline upload                                                     *         
***********************************************************************         
                                                                                
TLUREC   LKREQ H,A#TLUPL,NEWREC=Y                                               
PIN      LKREQ F,TL#PIN,(D,B#SAVED,TL_PIN),UBIN,TEXT=(*,PINLIT)                 
CostPers LKREQ F,TL#PCC,(D,B#SAVED,TL_PCC),CHAR,TEXT=(*,PCCLIT)                 
SecPID   LKREQ F,TL#PID,(D,B#SAVED,TL_PID),CHAR,TEXT=(*,PIDLIT)                 
Hours    LKREQ F,TL#HRS,(D,B#SAVED,TL_HOURS),SPAK,TEXT=(*,HRSLIT)               
Date     LKREQ F,TL#DATE,(D,B#SAVED,TL_DATE),PDAT,TEXT=(*,DATLIT)               
Client   LKREQ F,TL#CLI,(D,B#SAVED,TL_CLI),CHAR,TEXT=(*,CLILIT)                 
Product  LKREQ F,TL#PRO,(D,B#SAVED,TL_PRO),CHAR,TEXT=(*,PROLIT)                 
Job      LKREQ F,TL#JOB,(D,B#SAVED,TL_JOB),CHAR,TEXT=(*,JOBLIT)                 
WorkCode LKREQ F,TL#WCD,(D,B#SAVED,TL_WCD),CHAR,TEXT=(*,WCDLIT)                 
TimeType LKREQ F,TL#TTYP,(D,B#SAVED,TL_TTYP),CHAR,TEXT=(*,TTYPLIT)              
1NAcc    LKREQ F,TL#1NA,(D,B#SAVED,TL_1NA),CHAR,TEXT=(*,ONALIT)                 
Narr     LKREQ F,TL#NARR,(D,B#SAVED,TL_NARR),CHAR,TEXT=(*,NARRLIT),    +        
               LOWERCASE=Y                                                      
*  extension to pass header vals to offine run (not under MQ)                   
*ataVals LKREQ F,TL#DV,(I,B#SAVED,TL_AHEAD),CHAR,TEXT=(*,DATALIT),     +        
               OLEN=1                                                           
DataVal1 LKREQ F,TL#DV1,(I,B#SAVED,TL_AHED1),CHAR,TEXT=(*,DATALIT),    +        
               OLEN=1                                                           
DataVal2 LKREQ F,TL#DV2,(I,B#SAVED,TL_AHED2),CHAR,TEXT=(*,DATALIT),    +        
               OLEN=1                                                           
***DataVal3 LKREQ F,TL#DV3,(I,B#SAVED,TL_AHED3),CHAR,TEXT=(*,DATALIT), +        
               OLEN=1                                                           
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* End of request map tables                                          *          
***********************************************************************         
                                                                                
         LKMAP X                                                                
                                                                                
***********************************************************************         
* Build transactions                                                  *         
***********************************************************************         
                                                                                
BLDTRN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDTRN*'                                                      
*&&UK                                                                           
         MVI   ATPROF1,ATPR1SAL    Default to use sales rate                    
         CLI   DV_COBRT,COOPTYQ    Use cost rate not sales (B-Time)             
         JE    *+8                                                              
         MVI   ATPROF1,ATPR1CST    Use cost rate                                
         MVI   ATPROF2,ATPR1SAL    Default to use sales rate                    
         CLI   DV_CORRT,COOPTYQ    Use cost rate not sales (R-Time)             
         JE    *+8                                                              
         MVI   ATPROF2,ATPR1CST    Use cost rate                                
*&&                                                                             
                                                                                
*&&UK                                                                           
         CLI   DV_COTUP,COSAVED    For anything other than saved                
         JE    BLDTRN00             create contra-headers only                  
         ZAP   DV_HRS,PZERO        Clear hours                                  
         ZAP   DV_RATE,PZERO       Clear sales rate                             
         ZAP   DV_CRATE,PZERO      Clear cost rate                              
         J     BLDTRN02                                                         
                                                                                
BLDTRN00 CLI   DV_TTYP,TIMTNC      Test non-client time                         
         JE    BLDTRN02                                                         
                                                                                
         GOTOR BLDMTH              Build 14 contra and method table             
*&&                                                                             
                                                                                
BLDTRN02 CLI   DV_TTYP,TIMTNC      Test non-client time                         
         JE    *+12                                                             
         CLI   DV_TTYP,TIMTCN      Test nonbillable-client time                 
         JNE   BLDTRN06                                                         
         ZAP   ATAMOUNT,PZERO      Yes - posting amount is zero                 
         J     BLDTRN08                                                         
                                                                                
BLDTRN06 ZAP   PACK16,DV_RATE      Calculate time posting amount                
*&&UK                                                                           
         LA    RE,ATPROF1          Profile 1 for B-Time                         
         CLI   DV_TTYP,TIMTCB      Are we doing B-Time?                         
         JE    *+8                                                              
         LA    RE,ATPROF2          Profile 2 for N-Time and R-Time              
         TM    0(RE),ATPR1CST      Test cost rate instead of sales rate         
         JNO   *+10                                                             
         ZAP   PACK16,DV_CRATE     Yes - set cost rate                          
*&&                                                                             
         MP    PACK16,DV_HRS       Calculate amount based on rate               
         SRP   PACK16,64-2,5                                                    
         ZAP   ATAMOUNT,PACK16                                                  
                                                                                
BLDTRN08 CLC   DV_ULA,SPACES                                                    
         JNH   BLDTRNX                                                          
         MVC   ATREF,SPACES        Create batch reference #                     
         MVI   ATREF,C'T'          Time                                         
                                                                                
BLDTRN10 LLC   R1,DV_PERNO         Period number into chars 2-4                 
         CVD   R1,DUB              of batch reference                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  ATREF+1(3),DUB                                                   
                                                                                
*        *** DR 1R/1C - only update buckets (Client time) ***                   
*        *** DR 1R/1N - only update buckets (Non-client time) ***               
                                                                                
         OI    POSTSTAT,POSTS1R    1R posting time record buckets               
                                                                                
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,DV_1RULA    1R account                                   
         MVC   TRNKOFF,DV_OFF      Client office                                
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,DV_CAULA    Costing account                              
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKDATE,DV_LEDAT                                                
         MVC   TRNKREF,ATREF                                                    
         MVI   TRNRSTYP,49         Assume type 49                               
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_SCI1                                                          
         GOTOR EL_TRS                                                           
                                                                                
         OI    STPOSTNG,STBUCKET   Only update buckets                          
         GOTOR GOATRN,DV_CANAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTS1R                                              
*&&UK                                                                           
         CLI   DV_COTUP,COSAVED    For anything other than saved                
         JNE   BLDTRN32             create contra-headers only                  
         CLI   DV_TTYP,TIMTNC      Test non-client time                         
         JE    BLDTRN32            Yes - don't do method buckets                
                                                                                
         LA    R5,METHTAB          R5=A(Method table)                           
         USING MT_D,R5                                                          
BLDTRN12 CLI   MT_METHD,MT_EOTQ    Method number is anything present            
         JE    BLDTRN18            No - finished 1R/1C and 1C/14                
                                                                                
         LHI   R0,MT_BCNT          There 3 possible buckets                     
         LA    R3,MT_PRATE         Start at type 3                              
BLDTRN14 CP    0(MT_RLNQ,R3),PZERO Is this accumulator zero?                    
         JE    BLDTRN16            Yes - ignore                                 
         ZAP   PACK16,0(MT_RLNQ,R3)                                             
         MP    PACK16,DV_HRS                                                    
         MP    PACK16,MT_PCT                                                    
         SRP   PACK16,64-8,5       Shift 8                                      
         ZAP   PACK8,PACK16                                                     
         ZAP   DRAMOUNT,PACK8                                                   
         ZAP   CRAMOUNT,PZERO                                                   
         MVC   BUCKMTHD,MT_METHD   Set method                                   
         STC   R0,BUCKSLRY                                                      
         OI    BUCKSLRY,X'F0'      Set salary                                   
         OI    POSTSTAT,POSTSCA    CA type buckets                              
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,DV_1RULA    1R account                                   
         MVC   TRNKOFF,DV_OFF      Client office                                
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,DV_CAULA    1C account                                   
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKDATE,DV_LEDAT                                                
         MVC   TRNKREF,SPACES                                                   
         MVC   TRNKREF(L'MT_METHD),MT_METHD                                     
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_SCI1                                                          
                                                                                
         GOTOR GOATRN,DV_CANAM     Put transaction record                       
                                                                                
         ZAP   CRAMOUNT,PACK8                                                   
         ZAP   DRAMOUNT,PZERO                                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,DV_CAULA    1C account                                   
         MVC   TRNKOFF,DV_OFF      Client office                                
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,AT142ULA    14 account 2 level                           
         CLI   MT_STR,MT_S3LVL     Is it 3 level structure?                     
         JNE   *+10                                                             
         MVC   TRNKULC,AT143ULA    14 account 3 level                           
         TM    METHSTAT,METSNANL   Are we using profile setting?                
         JZ    *+10                No                                           
         MVC   TRNKCACT(L'MT_ANAL),MT_ANAL Set analysis from profile            
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKDATE,DV_LEDAT                                                
         MVC   TRNKREF,SPACES                                                   
         MVC   TRNKREF(L'MT_METHD),MT_METHD                                     
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_SCI1                                                          
                                                                                
         LA    R1,DV_142NM                                                      
         CLI   MT_STR,MT_S3LVL                                                  
         JNE   *+8                                                              
         LA    R1,DV_143NM                                                      
         GOTOR GOATRN              Put transaction record                       
                                                                                
BLDTRN16 AHI   R3,MT_RLNQ          Yes - go to next accumulator                 
         JCT   R0,BLDTRN14         Subtract type and repeat                     
         AHI   R5,MT_LNQ           End of accumulators get next method          
         J     BLDTRN12            in table and read that                       
                                                                                
BLDTRN18 LA    R5,METHTAB          R5=A(Method table)                           
BLDTRN20 CLI   MT_METHD,MT_EOTQ    Method number is anything present            
         JE    BLDTRN22            No - finished 1R/1C and 1C/14                
         GOTOR UPDCPR              Update payroll history record                
         AHI   R5,MT_LNQ           Bump to next method                          
         J     BLDTRN20                                                         
                                                                                
BLDTRN22 LA    R5,METHTAB          R5=A(Method table)                           
BLDTRN24 CLI   MT_METHD,MT_EOTQ    Method number is anything present            
         JE    BLDTRN30            No - finished 1R/1C and 1C/14                
                                                                                
         LHI   R0,MT_BCNT          There 3 possible buckets                     
         LA    R3,MT_PRATE         Start at type 3                              
BLDTRN26 CP    0(MT_RLNQ,R3),PZERO Is this accumulator zero?                    
         JE    BLDTRN28            Yes - ignore                                 
         ZAP   PACK16,0(MT_RLNQ,R3)                                             
         MP    PACK16,DV_HRS                                                    
         MP    PACK16,MT_PCT                                                    
         SRP   PACK16,64-8,5       Shift 8                                      
         ZAP   PACK8,PACK16                                                     
         STC   R0,BUCKSLRY                                                      
         OI    BUCKSLRY,X'F0'      Convert into display number                  
                                                                                
K        USING PLDRECD,IOKEY                                                    
         XC    K.PLDKEY(ACCKLEN),K.PLDKEY                                       
         MVI   K.PLDKTYP,PLDKTYPQ                                               
         MVI   K.PLDKSUB,PLDKSUBQ                                               
         MVC   K.PLDKCPY,CUXCPY                                                 
         MVC   K.PLDKMTHD,MT_METHD                                              
         MVC   K.PLDKCACT,DV_CAULA+L'TRNKLDG+L'TRNKUNT                          
         MVC   K.PLDKRACT,DV_1RACT                                              
         MVC   K.PLDKANAL,AT142ACT                                              
         TM    METHSTAT,METSNANL   Did analysis exist in 1R                     
         JZ    *+10                Yes                                          
         MVC   K.PLDKANAL,MT_ANAL  No - use profile value for analysis          
         MVC   K.PLDKYYMM,DV_MOA   Year/month                                   
         MVC   K.PLDKPTYP,BUCKSLRY Payroll type                                 
         ZAP   K.PLDKAMT,PACK8                                                  
         GOTOR DMGRITRN,DMCB,$PLDREC,K.PLDRECD                                  
                                                                                
BLDTRN28 AHI   R3,MT_RLNQ          Bump to next accumulator                     
         JCT   R0,BLDTRN26         Subtract type and repeat                     
         AHI   R5,MT_LNQ           End of accumulators get next method          
         J     BLDTRN24            in table and read that                       
         DROP  R5                                                               
                                                                                
BLDTRN30 NI    POSTSTAT,FF-POSTSCA                                              
*&&                                                                             
*              *** DR SJ/1R ***                                                 
                                                                                
BLDTRN32 NI    STPOSTNG,FF-STBUCKET                                             
         CLI   DV_TTYP,TIMTCN      Continue if client time                      
         JH    BLDTRNX                                                          
         CLI   DV_COTUP,COSAVED    For anything other than saved                
         JNE   BLDTRNX             Yes - don't update postings                  
         LLC   RE,PPROLEN          Test if a job specified                      
         LA    RE,DV_ACT(RE)                                                    
         CLC   0(5,RE),SPACES                                                   
         JNH   BLDTRNX                                                          
                                                                                
BLDTRN34 OI    POSTSTAT,POSTSSJ    SJ posting                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,DV_ULA      SJ account                                   
         MVC   TRNKWORK,TL_WCD     Task code                                    
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,DV_1RULA                                                 
         MVC   TRNKDATE,DV_LEDAT                                                
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_SPA                                                           
         GOTOR EL_PRT                                                           
         GOTOR EL_SPD                                                           
*&&UK*&& GOTOR EL_SCI1                                                          
*&&UK*&& GOTOR EL_SCI2                                                          
*&&UK*&& GOTOR EL_SCI4                                                          
*&&UK*&& GOTOR EL_SCI5                                                          
         GOTOR EL_TRS                                                           
         GOTOR EL_ANO                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM4                                                          
                                                                                
         GOTOR GOATRN,DV_1RNAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTSSJ                                              
                                                                                
*              *** CR SI/SJ ***                                                 
                                                                                
BLDTRN36 CLI   DV_TTYP,TIMTCB      Exit if not B-Time                           
         JNE   BLDTRNX                                                          
                                                                                
         OI    POSTSTAT,POSTSSI    SI posting                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,DV_INULA    Income account                               
         MVC   TRNKOFF,DV_OFF      Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,DV_ULA      SJ account                                   
         MVC   TRNKDATE,DV_LEDAT                                                
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
*&&US*&& GOTOR EL_MDT              Media interface element                      
*&&UK*&& GOTOR EL_SCI3                                                          
         GOTOR EL_TRS                                                           
*&&UK*&& GOTOR EL_SOR                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_RFL                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM4                                                          
                                                                                
         GOTOR GOATRN,DV_SJNAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTSSI                                              
         CLC   LEDGERSI,DV_INULA                                                
         JNE   BLDTRNX             Posting to SI - skip for u/l=SK              
                                                                                
*              *** DR 1C/12 ***                                                 
                                                                                
BLDTRN38 OI    POSTSTAT,POSTS1C                                                 
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,DV_CAULA    Costing account                              
         MVC   TRNKOFF,DV_OFF      Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,DV_12ULA    Analysis account                             
         MVC   TRNKDATE,DV_LEDAT                                                
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_TRS                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM4                                                          
                                                                                
         GOTOR GOATRN,DV_12NAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTS1C                                              
                                                                                
*              *** CR 12/1C ***                                                 
                                                                                
BLDTRN40 OI    POSTSTAT,POSTS12    12 posting                                   
                                                                                
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,DV_12ULA    Analysis account                             
         MVC   TRNKOFF,DV_OFF      Client office                                
         TM    CPYSTAT4,CPYSOFF2                                                
         JO    *+10                                                             
         MVC   TRNKOFF,SPACES      Old offices = no office in key               
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,DV_CAULA    Costing account                              
         MVC   TRNKDATE,DV_LEDAT                                                
         MVC   TRNKREF,ATREF                                                    
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_TRS                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_TIM4                                                          
                                                                                
         GOTOR GOATRN,DV_CANAM     Put transaction record                       
         NI    POSTSTAT,FF-POSTS12                                              
                                                                                
BLDTRNX  J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* X'1A' Media transfer element                                        *         
***********************************************************************         
*&&US                                                                           
EL_MDT   NTR1  LABEL=NO                                                         
         CLC   LEDGERSI,DV_INULA                                                
         JNE   EXITY               Don't add to SK posting                      
                                                                                
N        USING MDTELD,ELEMENT                                                   
         XC    N.MDTELD(MDTLNQ),N.MDTELD                                        
         MVI   N.MDTEL,MDTELQ                                                   
         MVI   N.MDTLN,MDTLNQ                                                   
         MVI   N.MDTSYS,MDTSPROD   C'J' - production                            
         LLC   R1,PPROLEN                                                       
         LA    R1,DV_ACT(R1)                                                    
         MVC   N.MDTMED,0(R1)                                                   
         MVC   N.MDTCLI(L'DV_ACT),DV_ACT                                        
         MVC   N.MDTMOS,DV_MOA     MOA                                          
         MVC   N.MDTDSCP,DV_SJNAM  SJ account name                              
         ZAP   DUB,ATAMOUNT                                                     
         CVB   R0,DUB                                                           
         STCM  R0,15,N.MDTCOM      Income (commission)                          
         STCM  R0,15,N.MDTINTL     Income (internal)                            
         J     ADDELE                                                           
*&&                                                                             
***********************************************************************         
* X'23' Others element                                                *         
***********************************************************************         
                                                                                
EL_OTH   NTR1  LABEL=NO                                                         
N        USING OTHELD,ELEMENT                                                   
         XC    N.OTHELD(OTHLN2Q),N.OTHELD                                       
         MVI   N.OTHEL,OTHELQ                                                   
         MVI   N.OTHLN,OTHLN1Q                                                  
         MVC   N.OTHNUM(L'OTHNUM+L'OTHPROF),SPACES                              
         MVC   N.OTHNUM(L'DV_ACT),DV_ACT                                        
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'2C' Special posting element                                       *         
***********************************************************************         
                                                                                
EL_SPA   NTR1  LABEL=NO                                                         
N        USING SPAELD,ELEMENT                                                   
         XC    N.SPAELD(SPALNQ),N.SPAELD                                        
         MVI   N.SPAEL,SPAELQ                                                   
         MVI   N.SPALN,SPALNQ                                                   
         MVI   N.SPATYPE,SPATCCST  Costing account                              
         MVC   N.SPAAULA,DV_CAULA                                               
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'40' Personnel rate element                                        *         
***********************************************************************         
                                                                                
EL_PRT   NTR1  LABEL=NO                                                         
N        USING PRTELD,ELEMENT                                                   
         XC    N.PRTELD(PRTLNQ),N.PRTELD                                        
         MVI   N.PRTEL,PRTELQ                                                   
         MVI   N.PRTLN,PRTLNQ                                                   
         MVC   N.PRTSTRT,DV_REFF   Effective date for rate                      
         ZAP   N.PRTRATE,DV_RATE   Rate                                         
         ZAP   N.PRTHOUR,DV_HRS    Hours                                        
*&&US*&& MVC   N.PRTLINE#,DV_TIME# TMS line #                                   
                                                                                
EL_PRT02 DS    0H                                                               
*&&US                                                                           
         MVI   N.PRTSTAT,PRTSNOTQ  N-Time                                       
         CLI   DV_TTYP,TIMTCR      R-Time                                       
         JNE   *+8                                                              
         MVI   N.PRTSTAT,PRTSRTEQ                                               
         CLI   DV_TTYP,TIMTCB      B-Time                                       
         JNE   *+8                                                              
         MVI   N.PRTSTAT,PRTSBILQ                                               
         TM    DV_RBSTA,TIMRBADJ                                                
         JNO   *+8                                                              
         OI    N.PRTSTAT,PRTSADJ   Rate was adjusted                            
*&&                                                                             
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'44' Transaction element                                           *         
***********************************************************************         
                                                                                
EL_TRN   NTR1  LABEL=NO                                                         
N        USING TRNELD,ELEMENT                                                   
         XC    N.TRNELD(TRNLN1Q),N.TRNELD                                       
         MVI   N.TRNEL,TRNELQ                                                   
         MVC   N.TRNDATE,DV_LEDAT  Set transaction date to today                
*&&UK                                                                           
         TM    POSTSTAT,POSTSCA    Are we posting CA type buckets?              
         JZ    EL_TRN02            No                                           
         MVC   N.TRNREF,SPACES     Yes                                          
         MVC   N.TRNREF(L'BUCKMTHD),BUCKMTHD                                    
         J     EL_TRN04                                                         
*&&                                                                             
EL_TRN02 MVC   N.TRNREF,ATREF                                                   
         MVI   N.TRNTYPE,49        Type 49                                      
                                                                                
EL_TRN04 MVC   N.TRNMOS,DV_MOA     Convert YYMM -> YM ebcdic                    
         OI    N.TRNMOS,X'F0'                                                   
         LLC   R1,N.TRNMOS+1                                                    
         LHI   RF,X'F0'                                                         
         TM    N.TRNMOS+1,X'10'                                                 
         JNO   *+8                                                              
         LHI   RF,X'B1'                                                         
         AR    R1,RF                                                            
         STC   R1,N.TRNMOS+1                                                    
*&&UK                                                                           
         TM    POSTSTAT,POSTSCA    Are we posting CA type buckets?              
         JZ    EL_TRN06            No                                           
         ZAP   N.TRNAMNT,PZERO     Zero amount                                  
         SR    R1,R1                                                            
         J     EL_TRN10                                                         
*&&                                                                             
EL_TRN06 LLC   R1,DV_PERNO         Period number into last 4 chars              
         CVD   R1,DUB              of batch reference                           
         OI    DUB+7,X'0F'                                                      
         UNPK  N.TRNBREF(4),DUB+5(3)                                            
                                                                                
         TM    POSTSTAT,POSTSSJ    SJ posting                                   
         JZ    *+8                                                              
         OI    N.TRNSTAT,TRNSAUTH  Only authorised users                        
         ZAP   N.TRNAMNT,PZERO                                                  
         OI    N.TRNSTAT,TRNSNOCM                                               
         TM    POSTSTAT,POSTS1R+POSTSSJ+POSTS1C                                 
         JZ    *+8                                                              
         OI    N.TRNSTAT,TRNSDR    Debit posting for 1R/SJ/1C                   
         TM    POSTSTAT,POSTS1R                                                 
         JO    EL_TRN10                                                         
         CLI   DV_TTYP,TIMTCB      Only put amount in billable postings         
         JNE   EL_TRN10                                                         
EL_TRN08 ZAP   N.TRNAMNT,ATAMOUNT  Amount                                       
                                                                                
EL_TRN10 MVC   N.TRNOFFC,DV_OFF    Client office                                
         TM    POSTSTAT,POSTSSJ                                                 
         JZ    *+10                                                             
         MVC   N.TRNOFFC,TL_WCD    SJ posting uses work code                    
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    EL_TRN12                                                         
         CLC   N.TRNOFFC,SPACES                                                 
         JH    EL_TRN12                                                         
         DC    H'0'                                                             
*                                                                               
EL_TRN12 SR    R1,R1                                                            
         ICM   R1,1,DV_NARRL                                                    
         JZ    EL_TRN14                                                         
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   N.TRNNARR(0),TL_NARR                                             
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
                                                                                
EL_TRN14 LA    RF,TRNLN1Q(R1)                                                   
         STC   RF,N.TRNLN                                                       
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'4C' Subsidiary posting element                                    *         
***********************************************************************         
                                                                                
EL_SPD   NTR1  LABEL=NO                                                         
*&&US                                                                           
         CLI   DV_TTYP,TIMTCB      Only add for Billable time                   
         JNE   EXITY               All others-skip                              
*&&                                                                             
         CLC   DV_INULA,SPACES     Don't add memo if no income                  
         JNH   EXITY                                                            
N        USING SPDELD,ELEMENT                                                   
         XC    N.SPDELD(SPDLN1Q),N.SPDELD                                       
         MVI   N.SPDEL,SPDELQ                                                   
         MVC   N.SPDACCS,DV_INULA  Income account                               
         MVI   N.SPDLN,SPDLN1Q+L'DV_INULA                                       
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'50' Subsidiary cash element (hours)                               *         
***********************************************************************         
                                                                                
EL_SCI1  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITHOUR  Hours                                        
*&&UK                                                                           
         TM    POSTSTAT,POSTSCA    Are we creating CA type buckets?             
         JZ    EL_SCI12            No                                           
         MVI   N.SCILN,SCILN3Q     Yes                                          
         MVI   N.SCITYPE,SCITNULL                                               
         ZAP   N.SCIAMNT,DRAMOUNT  Debit amount                                 
         ZAP   N.SCIADMN,CRAMOUNT  Credit amount                                
         MVC   N.SCISUBTY,SPACES                                                
         MVC   N.SCISUBTY(L'BUCKTYPE),BUCKTYPE                                  
         J     EL_SCI18                                                         
                                                                                
EL_SCI12 TM    POSTSTAT,POSTSSJ                                                 
         JZ    EL_SCI14                                                         
         MVI   N.SCITYPE,SCITSJHR                                               
         ZAP   N.SCIAMNT,DV_HRS    Hours                                        
         J     EL_SCI18                                                         
*&&                                                                             
EL_SCI14 ZAP   N.SCIAMNT,DV_HRS                                                 
                                                                                
EL_SCI18 J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'50' Subsidiary cash element (sales or cost rate memo)             *         
***********************************************************************         
*&&UK                                                                           
EL_SCI2  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITCRAT  Always use type C even if its not            
                                                                                
         ZAP   DUB,DV_RATE                                                      
         CLI   DV_TTYP,TIMTCB      Are we doing B - time?                       
         JE    *+12                Check ATPROF1 and take opposite              
         TM    ATPROF2,ATPR1CST    ATPROF2 for N and R time                     
         J     *+8                 Check ATPR1SAL and post as stated            
         TM    ATPROF1,ATPR1SAL    (this is OK this way round)                  
         JNO   *+10                                                             
         ZAP   DUB,DV_CRATE                                                     
         MP    DUB,DV_HRS                                                       
         SRP   DUB,64-2,5                                                       
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
EL_SCI22 J     ADDELE                                                           
*&&                                                                             
***********************************************************************         
* X'50' Subsidiary cash element (gross dollars bucket)                *         
***********************************************************************         
                                                                                
EL_SCI3  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITGRSS  Gross amount                                 
         ZAP   N.SCIAMNT,PZERO                                                  
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'50' Subsidiary cash element (memo sales amount)                   *         
***********************************************************************         
*&&UK                                                                           
EL_SCI4  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITMSRT  Memo sales rates                             
                                                                                
         ZAP   DUB,DV_RATE                                                      
         MP    DUB,DV_HRS                                                       
         SRP   DUB,64-2,5                                                       
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
EL_SCI42 J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'50' Subsidiary cash element (memo cost amount)                    *         
***********************************************************************         
                                                                                
EL_SCI5  NTR1  LABEL=NO                                                         
N        USING SCIELD,ELEMENT                                                   
         XC    N.SCIELD(SCILN1Q),N.SCIELD                                       
         MVI   N.SCIEL,SCIELQ                                                   
         MVI   N.SCILN,SCILN1Q                                                  
         MVI   N.SCITYPE,SCITMCRT  Memo cost rates                              
                                                                                
         ZAP   DUB,DV_CRATE                                                     
         MP    DUB,DV_HRS                                                       
         SRP   DUB,64-2,5                                                       
         ZAP   N.SCIAMNT,DUB                                                    
                                                                                
EL_SCI52 J     ADDELE                                                           
*&&                                                                             
***********************************************************************         
* X'60' Transaction status element                                    *         
***********************************************************************         
                                                                                
EL_TRS   NTR1  LABEL=NO                                                         
N        USING TRSELD,ELEMENT                                                   
         XC    N.TRSEL(TRSLNQ),N.TRSELD                                         
         MVI   N.TRSEL,TRSELQ                                                   
         MVI   N.TRSLN,TRSLNQ                                                   
         MVC   N.TRSDATE,DV_TODC                                                
         MVC   N.TRSEFDT,N.TRSDATE Effective date                               
         MVC   N.TRSPMOS,DV_MOA                                                 
         MVC   N.TRSUSER,CUUSER    User id                                      
         TM    DV_IND,TIMIADJ      Time sheet adjusted                          
         JNO   *+8                                                              
         OI    N.TRSSTAT2,TRSSTADJ                                              
         TM    N.TRSSTAT2,TRSSTMSS+TRSSTADJ                                     
         JNZ   *+8                                                              
         OI    N.TRSSTAT2,TRSSTIME Regular                                      
         MVI   N.TRSSTAT3,TRSSMCS  Source is for MCS posting                    
         TM    POSTSTAT,POSTSSJ+POSTSSI+POSTS12+POSTS1C                         
         JZ    EL_TRS16                                                         
                                                                                
         MVI   N.TRSSTAT4,TRSSSAVT None must be saved                           
                                                                                
EL_TRS16 J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'65' Analyzed office element                                       *         
***********************************************************************         
                                                                                
EL_ANO   NTR1  LABEL=NO                                                         
*&&UK*&& CLI   DV_OFF+1,C' '       One byte or two byte office code             
*&&UK*&& JE    EXITY               Only adding elem for two byte off            
N        USING ANOELD,ELEMENT                                                   
         MVI   N.ANOEL,ANOELQ      Office element                               
         MVI   N.ANOLN,ANOLNQ                                                   
         MVI   N.ANOTYPE,ANOTCLI   Client office                                
         MVC   N.ANOOFFC,DV_OFF    Client office code                           
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'7B' Source element                                                *         
***********************************************************************         
*&&UK                                                                           
EL_SOR   NTR1  LABEL=NO                                                         
         CLC   LEDGERSI,DV_INULA                                                
         JNE   EXITY               Don't add to SK posting                      
N        USING SORELD,ELEMENT                                                   
         XC    N.SORELD(SORALNQ),N.SORELD                                       
         MVI   N.SOREL,SORELQ                                                   
         MVI   N.SORLN,SORALNQ                                                  
         MVI   N.SORSYS,SORSACC    C'A' - account system                        
         MVC   N.SORAULA,DV_ULA    SJ U/L/CLI/PRD/JOB                           
         J     ADDELE                                                           
*&&                                                                             
                                                                                
                                                                                
***********************************************************************         
* X'8B' Time element - time and material row number                   *         
***********************************************************************         
                                                                                
EL_TIM4  NTR1  LABEL=NO                                                         
N        USING TIMELD,ELEMENT                                                   
         XC    N.TIMELD(TIMJLNQ),N.TIMELD                                       
         MVI   N.TIMEL,TIMELQ                                                   
         MVI   N.TIMLN,TIM#LNQ                                                  
         MVI   N.TIMETYP,TIMEROW#                                               
         MVC   N.TIM#IDNO,DV_TIME#                                              
         MVC   N.TIM#MID#,DV_ITEM#                                              
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'C0' Analysis pointer element                                      *         
***********************************************************************         
                                                                                
EL_APE   NTR1  LABEL=NO,WORK=(RC,APEACTL)                                       
         USING APEACTD,RC                                                       
         LR    R3,R2                                                            
         USING TRNRECD,R3          R3=A(Transaction record)                     
                                                                                
         XC    APEACTD(APEACTSL),APEACTD                                        
         MVC   APE1RAA,DV_1RULA                                                 
         MVI   APE1RAS,APENSDR                                                  
         MVC   APEULAA,DV_CAULA                                                 
         MVI   APEULAS,APENSDR                                                  
         MVC   APEINAA,DV_INULA                                                 
         CLC   LEDGERSI,DV_INULA   Test posting to SI                           
         JNE   EL_APE02                                                         
         MVC   APE12AA,DV_12ULA                                                 
         MVC   APECAAA,DV_CAULA                                                 
         MVI   APECAAS,APENSDR                                                  
                                                                                
EL_APE02 LA    R2,ELEMENT                                                       
         USING APEELD,R2           R2=A(Element)                                
         MVI   APEEL,APEELQ                                                     
                                                                                
         LA    R4,APENTRYS                                                      
A        USING APENTRYS,R4         R4=A(Analysis account list)                  
         LHI   R0,APEACTSN         R0=Number of accounts in list                
                                                                                
         MVI   APENUM,0            Set no analysis accounts                     
         LA    R5,APENTRY                                                       
         USING APENTRY,R5          R5=A(Analysis account entry)                 
                                                                                
EL_APE04 CLC   A.APEACTA,SPACES    Test any account here                        
         JNH   EL_APE06                                                         
         CLC   A.APEACTA,TRNKULA   Is it the account?                           
         JE    EL_APE06                                                         
         CLC   A.APEACTA,TRNKULC   Is it the contra-account?                    
         JE    EL_APE06                                                         
         CLC   LEDGERSI,A.APEACTA  Is it SI posting?                            
         JNE   *+12                No - OK                                      
         CLI   DV_TTYP,TIMTCB                                                   
         JNE   EL_APE06            Don't add SI posting if not B-Time           
                                                                                
         MVC   APENSTAT,A.APEACTS  Set status                                   
         MVC   APENACT,A.APEACTA   Set account                                  
         LA    R1,APENACT+L'APENACT-1                                           
         BASR  RE,0                                                             
         CLI   0(R1),C' '          Locate end of account code                   
         JH    *+6                                                              
         BCTR  R1,RE                                                            
         AHI   R1,1                                                             
         SR    R1,R5                                                            
         STC   R1,APENLEN          Set length of entry                          
         AR    R5,R1               Point to next entry                          
         IC    R1,APENUM           Bump number of entries                       
         AHI   R1,1                                                             
         STC   R1,APENUM                                                        
                                                                                
EL_APE06 AHI   R4,APENTRYL         Bump to next account                         
         JCT   R0,EL_APE04                                                      
                                                                                
         CLI   APENUM,0            Test any entries added                       
         JE    EXITY                                                            
         SR    R5,R2                                                            
         STC   R5,APELN            Set length of element                        
         J     ADDELE                                                           
         DROP  R2,R3,R5,RC,A                                                    
                                                                                
APEACTD  DSECT                     ** Analysis account list **                  
APENTRYS DS    0X                                                               
APEACTA  DS    0XL(L'APENACT)                                                   
APE1RAA  DS    CL(L'DV_1RULA)                                                   
APEACTS  DS    0XL(L'APENSTAT)                                                  
APE1RAS  DS    X                                                                
APENTRYL EQU   *-APENTRYS                                                       
APEULAA  DS    CL(L'DV_ULA)                                                     
APEULAS  DS    X                                                                
APE12AA  DS    CL(L'DV_12ULA)                                                   
APE12AS  DS    X                                                                
APEINAA  DS    CL(L'DV_INULA)                                                   
APEINAS  DS    X                                                                
APECAAA  DS    CL(L'DV_CAULA)                                                   
APECAAS  DS    X                                                                
APEACTSL EQU   *-APEACTD                                                        
APEACTSN EQU   APEACTSL/APENTRYL                                                
APEACTL  EQU   *-APEACTD                                                        
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* X'C5' Freeform data element (to carry work code)                    *         
***********************************************************************         
                                                                                
EL_RFL   NTR1  LABEL=NO                                                         
N        USING RFLELD,ELEMENT                                                   
         XC    N.RFLELD(RFLLNQ+L'TL_WCD),N.RFLELD                               
         MVI   N.RFLEL,RFLELQ                                                   
         MVI   N.RFLLN,RFLLNQ+L'TL_WCD                                          
         MVI   N.RFLTYPE,RFLWC                                                  
         MVC   N.RFLDATA(L'TL_WCD),TL_WCD                                       
         J     ADDELE                                                           
                                                                                
***********************************************************************         
* X'D8' Person ID element                                             *         
***********************************************************************         
                                                                                
EL_PID   NTR1  LABEL=NO                                                         
         OC    DV_PIN,DV_PIN                                                    
         JZ    EXITY                                                            
N        USING PIDELD,ELEMENT                                                   
         XC    N.PIDEL(PIDLNQ),N.PIDEL                                          
         MVI   N.PIDEL,PIDELQ                                                   
         MVI   N.PIDLN,PIDLNQ                                                   
         MVC   N.PIDNO,DV_PIN      User's PID #                                 
         J     ADDELE                                                           
         EJECT                                                                  
***********************************************************************         
* Add an element to end of record pointed to by ATRNREC               *         
***********************************************************************         
                                                                                
ADDELE   L     RE,ATRNREC          RE=A(Record)                                 
         USING TRNRECD,RE                                                       
         SR    R0,R0                                                            
         ICM   R0,3,TRNRLEN        Get current record length                    
         JNZ   *+8                                                              
         LHI   R0,TRNRFST+1-TRNRECD                                             
         LLC   RF,ELEMENT+1        RF=L'element to be added                     
         AR    R0,RF               Update record length                         
         STCM  R0,3,TRNRLEN-TRNRECD(RE)                                         
         CHI   R0,MAXRECLN                                                      
         JNH   *+6                                                              
         DC    H'0'                Record became too big                        
         SR    R0,RF                                                            
         BCTR  R0,0                Decrement for EOR                            
         AR    R0,RE               R0=A(Element insertion point)                
         LR    R1,R0                                                            
         AR    R1,RF                                                            
         MVI   0(R1),0             Set new EOR                                  
         LR    R1,RF               Set 'to' length                              
         LA    RE,ELEMENT          Set 'from' address                           
         MVCL  R0,RE               Move element to record                       
         J     EXITY                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* Build 14 contra account code and hourly rate method table           *         
***********************************************************************         
*&&UK                                                                           
BLDMTH   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDMTH*'                                                      
                                                                                
         MVC   SVMOA,DV_MOA        Save current MOA value                       
         MVI   METHSTAT,0          Initialise status flags                      
                                                                                
         CLC   DV_1RCST,SPACES     Test 1R analysis set                         
         JH    *+8                                                              
         OI    METHSTAT,METSNANL   Set no analysis found on 1R                  
                                                                                
         MVC   AT142ULA,SPACES                                                  
         MVC   AT142ULA(L'LEDGER14),LEDGER14                                    
         MVC   AT142ACT(L'DV_1RCST),DV_1RCST                                    
         LLC   RF,ONERL2L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   AT142ACT+L'DV_1RCST(0),DV_1RACT                                  
         EX    RF,0(RE)                                                         
         MVC   AT143ULA,SPACES                                                  
         MVC   AT143ULA(L'LEDGER14),LEDGER14                                    
         MVC   AT143ACT(L'DV_1RCST),DV_1RCST                                    
         LLC   RF,ONERL3L                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   AT143ACT+L'DV_1RCST(0),DV_1RACT                                  
         EX    RF,0(RE)                                                         
                                                                                
         LA    R5,METHTAB          R5=A(Method table)                           
         USING MT_D,R5                                                          
         MVI   MT_METHD,MT_EOTQ                                                 
                                                                                
K        USING CAPRECD,IOKEY       Read cost allocation profile record          
         MVC   K.CAPKEY,SPACES                                                  
         MVI   K.CAPKTYP,CAPKTYPQ                                               
         MVI   K.CAPKSUB,CAPKSUBQ                                               
         MVC   K.CAPKCPY,CUXCPY    Company code                                 
         MVI   K.CAPKMTHD,C'1'     Start with method 1                          
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    BLDMTH06                                                         
         DC    H'0'                                                             
                                                                                
BLDMTH04 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JE    BLDMTH06                                                         
         DC    H'0'                                                             
                                                                                
BLDMTH06 CLC   K.CAPKEY(CAPKMTHD-CAPKEY),IOKEYSAV                               
         JNE   BLDMTH30            No more records                              
         CLI   MT_METHD,MT_EOTQ    Test first time                              
         JE    BLDMTH08                                                         
         CLC   MT_METHD,K.CAPKMTHD Is it the same method as before?             
         JE    BLDMTH08            Yes                                          
         AHI   R5,MT_LNQ           No - bump to next entry                      
         MVI   MT_METHD,MT_EOTQ                                                 
                                                                                
BLDMTH08 CLC   K.CAPKOFC,SPACES                                                 
         JH    *+12                                                             
         OI    METHSTAT,METSHLVL   High level method record                     
         J     BLDMTH12                                                         
         CLC   K.CAPKOFC,DV_1ROFF                                               
         JNE   BLDMTH04                                                         
         CLC   K.CAPKDPT,SPACES                                                 
         JNH   BLDMTH10                                                         
         CLC   K.CAPKDPT,DV_1RDEP                                               
         JNE   BLDMTH04                                                         
         CLC   K.CAPKSDT,SPACES                                                 
         JNH   BLDMTH10                                                         
         CLC   K.CAPKDPT,DV_1RSUB                                               
         JNE   BLDMTH04                                                         
         CLC   K.CAPKPER,SPACES                                                 
         JNH   BLDMTH10                                                         
         CLC   K.CAPKPER,DV_1RPER                                               
         JNE   BLDMTH04                                                         
BLDMTH10 NI    METHSTAT,FF-METSHLVL                                             
                                                                                
BLDMTH12 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R3,IOADDR                                                        
         AHI   R3,CAPRFST-CAPRECD  Point R3 to first element of record          
         USING OPDELD,R3                                                        
         SR    R0,R0                                                            
         TM    METHSTAT,METSHLVL   Only find contra at high level rec           
         JZ    BLDMTH14                                                         
         MVI   MT_STR,MT_S2LVL     Structure of 14 as 2 level default           
         ZAP   MT_PCT,P1MILL       100% is the default allocation               
         MVI   MT_ANAL,C'Z'        'Z' is the standard default                  
                                                                                
BLDMTH14 CLI   OPDEL,0             Test end of record                           
         JE    BLDMTH04            Yes - get next record                        
         CLI   OPDEL,OPDELQ        Test option data element                     
         JNE   BLDMTH24                                                         
                                                                                
         CLI   OPDNUM,X'1C'        On-line update for allocation                
         JNE   BLDMTH16                                                         
         CLI   OPDDATA,C'Y'        Test on-line update                          
         JNE   BLDMTH26            No                                           
         MVC   MT_METHD,K.CAPKMTHD Save method number in table                  
         J     BLDMTH24                                                         
                                                                                
BLDMTH16 CLI   OPDNUM,X'0C'        Percentage of salary to allocate             
         JNE   BLDMTH18            No                                           
         ZAP   MT_PCT,OPDDATA(4)   Save % of salary to allocate                 
         CP    MT_PCT,PZERO        If zero not interested in this               
         JNE   BLDMTH24            method otherwise read other profiles         
         J     BLDMTH26            Get next sequential record                   
                                                                                
BLDMTH18 CLI   OPDNUM,X'0A'        Include executive in allocation opt          
         JNE   BLDMTH20            No - see if other type                       
         TM    SEMPEL+(EMPSTAT-EMPELD),EMPSEXEC person an executive?            
         JZ    BLDMTH24            No - no need for test                        
         CLI   OPDDATA,C'N'        Do we include                                
         JNE   BLDMTH24            Yes - get next element                       
         J     BLDMTH26            No - clear method number                     
                                                                                
BLDMTH20 CLI   OPDNUM,X'05'        Structure of 14 account option ele           
         JNE   BLDMTH22            No - get next element                        
         CLI   OPDDATA,C'3'        If it 3 level or 2                           
         JNE   BLDMTH24            No - it's 2 as default                       
         MVI   MT_STR,MT_S3LVL     Set struture of 14 as 3 level                
         J     BLDMTH24            Branch back to read rest of elements         
                                                                                
BLDMTH22 CLI   OPDNUM,X'18'        Get default analysis code                    
         JNE   BLDMTH24            No - get next element                        
         MVC   MT_ANAL,OPDDATA     Save this default                            
                                                                                
BLDMTH24 IC    R0,OPDLN            Bump to next element                         
         AR    R3,R0                                                            
         J     BLDMTH14                                                         
                                                                                
BLDMTH26 MVI   MT_METHD,MT_EOTQ    This method is invalid                       
         J     BLDMTH04            Get next record                              
                                                                                
BLDMTH30 LA    R5,METHTAB          R5=A(Method table)                           
         CLI   MT_METHD,MT_EOTQ    Have we got method                           
         JE    BLDMTHX             No                                           
                                                                                
***********************************************************************         
* Read payroll history and payroll detail records and set method      *         
* costing rates                                                       *         
***********************************************************************         
                                                                                
K        USING PHIRECD,IOKEY       Read payroll history record                  
         MVC   K.PHIKEY,SPACES                                                  
         MVI   K.PHIKTYP,PHIKTYPQ                                               
         MVI   K.PHIKSUB,PHIKSUBQ                                               
         MVC   K.PHIKCPY,CUXCPY                                                 
         MVC   K.PHIKOFC,DV_1ROFF                                               
         MVC   K.PHIKDPT,DV_1RDEP                                               
         MVC   K.PHIKSBD,DV_1RSUB                                               
         MVC   K.PHIKPER,DV_1RPER                                               
         SR    R1,R1                                                            
         ICM   R1,3,DV_MOA                                                      
         LNR   R1,R1                                                            
         STCM  R1,3,K.PHIKMOA      MOA 2's complement                           
         XC    K.PHIKSEQ,K.PHIKSEQ Clear sequence number                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                No history must error                        
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    BLDMTH32                                                         
         DC    H'0'                                                             
                                                                                
BLDMTH32 ZAP   MT_SRATE,PZERO      Zero accumulators                            
         ZAP   MT_BRATE,PZERO                                                   
         ZAP   MT_PRATE,PZERO                                                   
         ZAP   MT_TRATE,PZERO                                                   
                                                                                
         L     R3,IOADDR                                                        
         AHI   R3,PHIRFST-PHIRECD  R3=A(Payroll detail element)                 
         USING PDEELD,R3                                                        
BLDMTH34 CLI   PDEEL,0             Test end of record                           
         JE    BLDMTH54            Yes - next method                            
         CLI   PDEEL,PDEELQ        Payroll detail element                       
         JNE   BLDMTH52            No                                           
         CLC   DV_MOA,PDEDTE       Is pay date in same month?                   
         JNE   BLDMTH52            No - ignore                                  
         TM    PDESTAT2,PDESHRTE   Test hourly rate type                        
         JZ    BLDMTH52                                                         
                                                                                
K        USING CAHRECD,IOKEY       Read cost allocation history record          
         MVC   K.CAHKEY,SPACES                                                  
         MVI   K.CAHKTYP,CAHKTYPQ                                               
         MVI   K.CAHKSUB,CAHKSUBQ                                               
         MVC   K.CAHKCPY,CUXCPY                                                 
         MVC   K.CAHKMTHD,MT_METHD Set method                                   
         XC    K.CAHKOFC,K.CAHKOFC                                              
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,IOADDR                                                        
         AHI   R2,CAHRFST-CAHRECD  R2=A(First element on record)                
         USING PATELD,R2                                                        
BLDMTH44 CLI   PATEL,0             Test end of record                           
         JE    BLDMTH52            Yes - get next payroll detail                
         CLI   PATEL,PATELQ        Payroll type element                         
         JNE   BLDMTH50            No                                           
         CLC   PDENUM,PATNUM       Is payroll number equal?                     
         JNE   BLDMTH50            Yes                                          
                                                                                
         CLI   PATTYPE,PATTSAL     Is it salary type?                           
         JNE   BLDMTH46            No check for other types                     
         AP    MT_SRATE,PDEAMT     Add up salary types for method               
         AP    MT_SRATE,PDEADJ                                                  
         AP    MT_TRATE,MT_SRATE                                                
         J     BLDMTH52            Get next payroll detail                      
                                                                                
BLDMTH46 CLI   PATTYPE,PATTBEN     Is it benefit type?                          
         JNE   BLDMTH48            No check for other types                     
         AP    MT_BRATE,PDEAMT     Add up benefit types for method              
         AP    MT_BRATE,PDEADJ                                                  
         AP    MT_TRATE,MT_BRATE                                                
         J     BLDMTH52            Get next payoll detail                       
                                                                                
BLDMTH48 CLI   PATTYPE,PATTPEN     Is it pension salary type?                   
         JNE   BLDMTH52            No                                           
         AP    MT_PRATE,PDEAMT     Add up pension types for method              
         AP    MT_PRATE,PDEADJ                                                  
         AP    MT_TRATE,MT_PRATE                                                
         J     BLDMTH52            Get next payroll detail                      
                                                                                
BLDMTH50 IC    R0,PATLN            Bump to next payroll type element            
         AR    R2,R0                                                            
         J     BLDMTH44                                                         
                                                                                
BLDMTH52 IC    R0,PDELN            Bump to next payroll detail element          
         AR    R3,R0                                                            
         J     BLDMTH34                                                         
                                                                                
BLDMTH54 AHI   R5,MT_LNQ           Bump to next method                          
         CLI   MT_METHD,MT_EOTQ    Test end of table                            
         JNE   BLDMTH32            No                                           
                                                                                
BLDMTHX  J     EXITY                                                            
         DROP  K,R2,R3,R5                                                       
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* Update costing personal rates record                                *         
***********************************************************************         
*&&UK                                                                           
         USING MT_D,R5             R5=A(Method table entry)                     
UPDCPR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDCPR*'                                                      
                                                                                
         XC    BYTE1,BYTE1                                                      
K        USING CPRRECD,IOKEY                                                    
         MVC   K.CPRKEY,SPACES     Build key of costing rates record            
         MVI   K.CPRKTYP,CPRKTYPQ                                               
         MVI   K.CPRKSUB,CPRKSUBQ                                               
         MVC   K.CPRKCPY,CUXCPY                                                 
         MVC   K.CPRKUNT(L'DV_1RULA),DV_1RULA                                   
                                                                                
         L     R1,=AL4(IORDD+IODIR+IO8)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLI   IOERR,0                                                          
         JE    *+16                                                             
         CLI   IOERR,IOEDEL                                                     
         JNE   UPDCPR00                                                         
         MVI   BYTE1,2             Set record was deleted                       
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO8'                              
         JNE   *+2                                                              
         CLI   BYTE1,2                                                          
         JNE   UPDCPR02                                                         
         J     *+8                                                              
UPDCPR00 MVI   BYTE1,1             Set add record                               
         L     RF,ACPRREC          Build virgin record                          
         USING CPRRECD,RF                                                       
         XC    CPRRECD(256),CPRRECD                                             
         MVC   CPRKEY,IOKEYSAV                                                  
         MVC   IOKEY,IOKEYSAV                                                   
         LHI   R0,CPRRFST-CPRRECD+1                                             
         STCM  R0,3,CPRRLEN                                                     
         DROP  RF                                                               
                                                                                
N        USING PHRELD,ELEMENT      Initialise new element                       
UPDCPR02 XC    N.PHRELD(256),N.PHRELD                                           
         MVI   N.PHREL,PHRELQ                                                   
         MVI   N.PHRLN,PHRLN1Q                                                  
         MVC   N.PHRPER,DV_MOA                                                  
         MVC   N.PHRMTH,MT_METHD                                                
                                                                                
         L     RF,ACPRREC                                                       
         NI    CPRRSTA-CPRRECD(RF),X'FF'-X'80'  remove delete                   
         AHI   RF,CPRRFST-CPRRECD                                               
         USING PHRELD,RF                                                        
         SR    R0,R0                                                            
         SR    R2,R2                                                            
UPDCPR04 CLI   PHREL,0             Test end of record                           
         JE    UPDCPR10                                                         
         CLI   PHREL,PHRELQ                                                     
         JNE   UPDCPR06                                                         
         LTR   R2,R2                                                            
         JNZ   *+8                                                              
         LA    R2,PHRELD           R2=A(First PHREL on record)                  
         CLC   PHRPER(L'PHRPER+L'PHRMTH),N.PHRPER                               
         JE    UPDCPR08                                                         
UPDCPR06 IC    R0,PHRLN                                                         
         AR    RF,R0                                                            
         J     UPDCPR04                                                         
                                                                                
UPDCPR08 MVI   PHREL,FF            Delete PHDEL if found on record              
         GOTOR VHELLO,DMCB,(C'D',ACCMST),('FF',ACPRREC),0                       
                                                                                
UPDCPR10 ST    R2,FULL1            Save A(First PHREL)                          
                                                                                
         LHI   R3,MT_EBCNT         Number of hourly rate buckets                
         LA    R2,MT_TRATE         Total rate bucket                            
         LA    RE,N.PHRNTRY        Point to first sub-element                   
         USING PHRNTRY,RE                                                       
                                                                                
UPDCPR12 CHI   R3,MT_EBCNT         Is this the total hourly rate?               
         JNE   *+12                Yes - put out even if zero                   
         MVI   N.PHRTYPE,PHRTTOT                                                
         J     UPDCPR14                                                         
                                                                                
         CP    0(MT_RLNQ,R2),PZERO                                              
         JE    UPDCPR18            Ignore other zero counters                   
         STC   R3,PHRTYPE                                                       
         OI    PHRTYPE,X'F0'       Convert to EBCDIC                            
                                                                                
UPDCPR14 ZAP   DUB,0(MT_RLNQ,R2)   Hourly rate                                  
         MP    DUB,P100            Decimal places                               
         CP    MT_PCT,P1MILL       If percentage 100% no amending of            
         JE    UPDCPR16            rates is necessary                           
         ZAP   PACK16,DUB                                                       
         MP    PACK16,MT_PCT                                                    
         SRP   PACK16,64-6,5                                                    
         ZAP   DUB,PACK16                                                       
UPDCPR16 ZAP   PHRRATE,DUB                                                      
         LLC   R0,N.PHRLN          Increment element length                     
         AHI   R0,PHRLN2Q                                                       
         STC   R0,N.PHRLN                                                       
         LLC   R0,N.PHRNUM         Increment number of sub-elements             
         AHI   R0,1                                                             
         STC   R0,N.PHRNUM                                                      
         AHI   RE,PHRLN2Q                                                       
UPDCPR18 AHI   R2,MT_RLNQ          Bump to next rate                            
         JCT   R3,UPDCPR12                                                      
         DROP  RE                                                               
                                                                                
UPDCPR20 L     RF,ACPRREC                                                       
         SR    RE,RE                                                            
         ICM   RE,3,CPRRLEN-CPRRECD(RF)                                         
         LLC   R0,N.PHRLN          RE=L'New element to add                      
         AR    RE,R0               Add to current record length                 
         CHI   RE,MAXRECLN         Test new element fits into record            
         JNH   UPDCPR24            Yes                                          
         ICM   RF,15,FULL1         Point to oldest (first) element              
         JNZ   UPDCPR22                                                         
         DC    H'0'                There isn't one to delete                    
                                                                                
UPDCPR22 MVI   PHREL,FF            Delete this element                          
         IC    R0,PHRLN                                                         
         SR    RE,R0               Decrement length of this one                 
         CHI   RE,MAXRECLN         Does new one fit now?                        
         JNH   *+10                                                             
         AR    RF,R0               No - delete another one                      
         J     UPDCPR22                                                         
         GOTOR VHELLO,DMCB,(C'D',ACCMST),('FF',ACPRREC),0                       
                                                                                
UPDCPR24 GOTOR VHELLO,DMCB,(C'P',ACCMST),ACPRREC,N.PHRELD,0                     
         CLI   12(R1),0                                                         
         JE    *+6                                                              
         DC    H'0'                Can't add new element                        
         CLI   BYTE1,1                                                          
         JE    UPDCPR26                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO8'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
                                                                                
UPDCPR26 GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO8'                           
         JNE   *+2                                                              
         J     EXITY                                                            
         DROP  R5,RF                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* Data Manager routines                                               *         
*                                                                     *         
* DMGRITRN will be entered for any call to VDATAMGR (internal calls)  *         
* DMGRXTRN will be entered for any call to CDATAMGR (external calls)  *         
***********************************************************************         
                                                                                
DMGRITRN SR    RF,RF               Internal entry point - clear RF              
                                                                                
DMGRXTRN NTR1  LABEL=NO,WORK=(RC,DMWORKL)                                       
         J     *+12                                                             
         DC    C'*1EDMGR*'                                                      
                                                                                
         USING DMWORKD,RC          RC=A(Local w/s)                              
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
         L     R5,LLP                                                           
         USING LP_D,R5             R5=A(LP_D)                                   
         LM    R2,RB,LP_R2RB                                                    
         USING WORKD,R9            R9=A(WORKD)                                  
         USING SAVED,R8            R8=A(SAVED)                                  
         USING CPYELD,SCPYEL                                                    
         USING CPXELD,SCPXEL                                                    
         ST    R1,DMGRSVR1         Save A(Caller's parameter list)              
         MVC   DMGRDMCB,0(R1)      Save caller's DMCB                           
         MVI   DMGRERRS,0          Set no errors                                
                                                                                
         MVI   DMGRFLAG,DMGRFEXT   Set external I/O call                        
         LTR   RF,RF               Test called internally                       
         JNZ   *+8                                                              
         MVI   DMGRFLAG,DMGRFINT   Set internal I/O call                        
                                                                                
         LM    R3,R4,DMGRP1        R3=A(Command), R4=A(File)                    
                                                                                
         CLC   $ADDTRN,0(R3)       Test ADDTRN handler record                   
         JE    ADDTRN                                                           
*&&UK*&& CLC   $PLDREC,0(R3)       Test PLDREC handler record                   
*&&UK*&& JE    PLDREC                                                           
         CLC   $LOKREC,0(R3)       Test LOKREC handler record                   
         JE    LOKREC                                                           
                                                                                
         CLC   ACCOUNT,0(R4)       Only interested in Account files             
         JNE   DMGRREAL                                                         
                                                                                
         CLC   DMPUTF,0(R3)        Test updative I/O                            
         JE    DMGR0010                                                         
         CLC   DMWRTD,0(R3)                                                     
         JE    DMGR0010                                                         
         CLC   DMADDD,0(R3)                                                     
         JE    DMGR0010                                                         
         CLC   DMADDF,0(R3)                                                     
         JNE   DMGRREAL            No - issue non-updative I/O                  
                                                                                
DMGR0010 LHI   R0,FW_FDIR                                                       
         CLC   ACCDIR,0(R4)        Test ACCDIR I/O                              
         JE    DMGR0020                                                         
         LHI   R0,FW_FMST                                                       
         CLC   ACCMST,0(R4)        Test ACCMST I/O                              
         JE    DMGR0030                                                         
         LHI   R0,FW_FARC                                                       
         CLC   ACCARC,0(R4)        Test ACCARC I/O                              
         JE    DMGR0030                                                         
         DC    H'0'                Disallow other files                         
                                                                                
DMGR0020 L     R2,DMGRP4           ACCDIR updative I/O                          
         MVC   DMGRKEY,0(R2)                                                    
         SHI   R2,FW_DIROL         Back up by overhead length                   
         USING FW_D,R2             Build directory record                       
         MVC   DMGRSAVE(FW_DIRHL),0(R2)                                         
         XC    FW_D(FW_DIRHL),FW_D                                              
         STC   R0,FW_FILE          Set file number                              
         LHI   RF,FW_DIRRL                                                      
         STCM  RF,3,FW_RLEN        Set length of record                         
         MVC   FW_RKEY,DMGRKEY     Set record key                               
         MVI   FW_ACT,FW_AADDD     Set action to DMADD                          
         CLC   DMADDD,0(R3)        Test DMADD                                   
         JE    DMGR0025                                                         
         MVI   FW_ACT,FW_AWRTD     No - set action to DMWRITE                   
DMGR0025 GOTOR PUTOUT,FW_D         Put output record                            
         MVC   FW_D(FW_DIRHL),DMGRSAVE                                          
         J     DMGREXIT                                                         
                                                                                
DMGR0030 L     R2,DMGRP3           ACCMST/ACCARC updative I/O                   
         MVC   DMGRDA,0(R2)        Set disk address                             
         L     R2,DMGRP4           Point to I/O area                            
         MVC   DMGRKEY,0(R2)       Save record key                              
         XR    RF,RF                                                            
         ICM   RF,3,ACCRLEN-ACCRECD(R2)                                         
         AHI   RF,FW_RECOL         Add on length of overhead                    
         SHI   R2,FW_RECOL         Back up by overhead length                   
         USING FW_D,R2             Build file record                            
         MVC   DMGRSAVE(FW_RECHL),0(R2)                                         
         XC    FW_D(FW_RECHL),FW_D                                              
         STC   R0,FW_FILE          Set file number                              
         STCM  RF,3,FW_RLEN        Set record length                            
         MVC   FW_RKEY,DMGRKEY     Set record key                               
         MVI   FW_ACT,FW_APUTR     Set action to PUTREC                         
         MVC   FW_RDA,DMGRDA       Set disk address                             
         GOTOR SETRTYP                                                          
         CLC   DMPUTF,0(R3)        Test PUTREC                                  
         JE    DMGR0035                                                         
         MVI   FW_ACT,FW_AADDR     No - set action to ADDREC                    
         XC    FW_RDA,FW_RDA       and clear disk address                       
DMGR0035 GOTOR PUTOUT,FW_D         Put output record                            
         MVC   FW_D(FW_RECHL),DMGRSAVE                                          
         J     DMGREXIT                                                         
                                                                                
DMGRREAL GOTOR DATAMGR,DMGRDMCB    Call real DATAMGR                            
                                                                                
DMGREXIT L     R1,DMGRSVR1         Restore caller's parameter list              
         MVC   0(DMGRPL,R1),DMGRDMCB                                            
         CLI   DMGRERRS,0          Set condition code for caller                
         J     EXIT                                                             
                                                                                
SETRTYP  DS    0H                  Set record type (SRUPD60 passives)           
         MVI   FW_RTYP,0                                                        
         CLI   FW_RKEY+AUDKTYP-AUDRECD,AUDKTYPQ                                 
         JNE   SETRTYP1                                                         
         CLI   FW_RKEY+AUDKSUB-AUDRECD,AUDKSUBQ                                 
         JNE   SETRTYP1                                                         
         MVI   FW_RTYP,ACRTAUDT                                                 
         J     SETRTYPX                                                         
                                                                                
SETRTYP1 CLI   FW_RKEY+PERKTYP-PERRECD,PERKTYPQ                                 
         JNE   SETRTYP2                                                         
         MVI   FW_RTYP,ACRTPER                                                  
         J     SETRTYPX                                                         
                                                                                
SETRTYP2 CLI   FW_RKEY+ACTKCPY-ACTRECD,SPACEQ                                   
         JNH   SETRTYP5                                                         
         CLI   FW_RKEY+ACTKUNT-ACTRECD,UNIT1Q                                   
         JNE   SETRTYP5                                                         
         CLI   FW_RKEY+ACTKLDG-ACTRECD,LEDGRQ                                   
         JNE   SETRTYP5                                                         
         CLC   TIMEREF,FW_RKEY+TIMKREF-TIMRECD                                  
         JNE   SETRTYP3                                                         
         MVI   FW_RTYP,ACRTTIM                                                  
         J     SETRTYPX                                                         
                                                                                
SETRTYP3 LA    RF,FW_RKEY+CHDKNULL-CHDRECD                                      
         OC    0(L'CHDKNULL,RF),0(RF)                                           
         JH    SETRTYP4                                                         
         MVI   FW_RTYP,ACRTCHDH                                                 
         J     SETRTYPX                                                         
                                                                                
SETRTYP4 MVI   FW_RTYP,ACRTCAC                                                  
         J     SETRTYPX                                                         
                                                                                
SETRTYP5 CLI   FW_RKEY+CPRKTYP-CPRRECD,CPRKTYPQ                                 
         JNE   SETRTYP6                                                         
         MVI   FW_RTYP,ACRTCPR                                                  
         J     SETRTYPX                                                         
                                                                                
SETRTYP6 J     *+2                 unknown - not supported here yet             
                                                                                
SETRTYPX BR    RE                                                               
                                                                                
***********************************************************************         
* Handle special $ADDTRN actions                                      *         
***********************************************************************         
                                                                                
ADDTRN   CLI   DMGRACTN,FW_AAHDR   Test ADDTRN header                           
         JNE   ADDTRN02                                                         
         LA    R2,DMGRSAVE                                                      
         XC    FW_D(FW_HLNQ),FW_D                                               
         MVI   FW_FILE,FW_FTRN$    Set file to 'ADDTRN'                         
         MVI   FW_ACT,FW_AAHDR     Set action                                   
         LHI   RF,FW_HLNQ                                                       
         STCM  RF,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put ADDTRN header record                     
         MVC   FW_D(FW_HLNQ),DMGRSAVE                                           
         J     DMGREXIT                                                         
                                                                                
ADDTRN02 CLI   DMGRACTN,FW_AATRN   Test ADDTRN transaction record               
         JNE   ADDTRN04                                                         
         L     R2,ATRNREC                                                       
         MVC   DMGRKEY,0(R2)       Save transaction record key                  
         XR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(R2)                                         
         JZ    *+2                 Record length can't be zero                  
         CHI   RF,MAXRECLN                                                      
         JH    *+2                 Nor greater than the maximum                 
         AHI   RF,FW_ATOLQ         Add on overhead length                       
         SHI   R2,FW_ATOLQ         Back-up by overhead length                   
         MVC   DMGRSAVE(FW_ATHLQ),0(R2)                                         
         XC    FW_D(FW_ATHLQ),FW_D                                              
         STCM  RF,3,FW_RLEN        Set record length                            
         MVI   FW_FILE,FW_FTRN$    Set file to 'ADDTRN'                         
         MVI   FW_ACT,FW_AATRN     Set action                                   
         MVC   FW_RKEY,DMGRKEY     Set transaction key                          
         TM    STPOSTNG,STBUCKET   Test posting buckets only                    
         JZ    *+8                                                              
         MVI   FW_ATTI2,TRNIUBKO+TRNIUOFC                                       
                                                                                
         L     R1,DMGRP2                                                        
         MVC   FW_ATCAN,0(R1)      Set contra-account name                      
         GOTOR PUTOUT,FW_D         Put ADDTRN transaction record                
         MVC   FW_D(FW_ATHLQ),DMGRSAVE                                          
         J     DMGREXIT                                                         
                                                                                
ADDTRN04 CLI   DMGRACTN,FW_AAEND   Test ADDTRN end record                       
         JNE   *+2                                                              
         LA    R2,DMGRSAVE                                                      
         XC    FW_D(FW_HLNQ),FW_D                                               
         MVI   FW_FILE,FW_FTRN$    Set file to 'ADDTRN'                         
         MVI   FW_ACT,FW_AAEND     Set action                                   
         MVI   FW_RKEY,FF          Set key to high values                       
         MVC   FW_RKEY+1(L'FW_RKEY-1),FW_RKEY                                   
         LHI   RF,FW_HLNQ                                                       
         STCM  RF,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put ADDTRN end record                        
         J     DMGREXIT                                                         
                                                                                
***********************************************************************         
* Handle special $PLDREC actions                                      *         
***********************************************************************         
                                                                                
*&&UK                                                                           
PLDREC   L     R2,DMGRP2           Point to block/record                        
         MVC   DMGRKEY,0(R2)                                                    
         SHI   R2,FW_DIROL         Back up by header length                     
         MVC   DMGRSAVE(FW_DIROL),0(R2)                                         
         USING FW_D,R2             Build directory record                       
         XC    FW_D(FW_DIROL),FW_D                                              
         MVI   FW_FILE,FW_FPLD$    Set use PLDREC handler                       
         LHI   RF,FW_DIRRL                                                      
         STCM  RF,3,FW_RLEN        Set length of record                         
         MVC   FW_RKEY,DMGRKEY     Set record key                               
         GOTOR PUTOUT,FW_D         Put output record                            
         MVC   FW_D(FW_DIROL),DMGRSAVE                                          
         J     DMGREXIT                                                         
*&&                                                                             
                                                                                
***********************************************************************         
* Handle special $LOKREC actions                                      *         
***********************************************************************         
                                                                                
LOKREC   LA    R2,DMGRSAVE         Point to build area                          
         XC    FW_D(FW_DIRRL),FW_D                                              
         MVI   FW_FILE,FW_FLOK$    Set file                                     
         L     R1,DMGRP2           Point to lock key                            
         MVC   FW_LKKEY,0(R1)      Set LOCKET key to be unlocked                
         LHI   R0,FW_LKRL                                                       
         STCM  R0,3,FW_RLEN                                                     
         GOTOR PUTOUT,FW_D         Put LOCKET record to output file             
         J     DMGREXIT                                                         
         DROP  R2,RC                                                            
                                                                                
DMWORKD  DSECT                     ** DMGRXTRN s/r local w/s **                 
DMGRSVR1 DS    A                   A(Caller's parameter list)                   
                                                                                
DMGRDMCB DS    0XL(DMGRPL)         ** Caller's parameter list **                
DMGRACTN DS    0X                  Action code (special records)                
DMGRP1   DS    A                   A(Action)                                    
DMGRP2   DS    A                   A(File)                                      
DMGRERRS DS    0X                  Error return byte                            
DMGRP3   DS    A                   A(Disk address)                              
DMGRP4   DS    A                   A(I/O area)                                  
DMGRP5   DS    A                   A(DMWORK area)                               
DMGRP6   DS    A                   N/D                                          
DMGRPL   EQU   *-DMGRP1                                                         
                                                                                
DMGRFLAG DS    X                   ** DMGRXTRN flag byte **                     
DMGRFINT EQU   X'80'               Internal (VDATAMGR) call                     
DMGRFEXT EQU   X'40'               External (CDATAMGR) call                     
                                                                                
DMGRDA   DS    XL(L'IODA)          Disk address                                 
DMGRKEY  DS    XL(L'ACCKEY)        Key                                          
DMGRSAVE DS    XL128               Save area                                    
DMWORKL  EQU   *-DMWORKD                                                        
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* Put record to FACWRK recovery                                       *         
***********************************************************************         
                                                                                
PUTOUT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTOUT*'                                                      
                                                                                
         LR    R2,R1                                                            
         USING FW_D,R2             R2=A(Record to put)                          
                                                                                
         TM    LP_FLAG,LP_FDRFT    Test 'draft' (validation) mode               
         JNZ   *+2                 Updative I/O in draft mode                   
                                                                                
         TM    RUNINDS,RUNIPUTF    Test first time call                         
         JNZ   PUTOUT06                                                         
         OI    RUNINDS,RUNIPUTF    Set not first time call                      
         XC    DMGRSEQ#,DMGRSEQ#   Initialise sequence number                   
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    PUTOUT02            No - use FACWRK                              
         GOTO1 ASORTER,DMCB,SORTCARD,SORTTYPE                                   
                                                                                
PUTOUT02 L     R3,WRKBLKR                                                       
         USING WRKIOD,R3           R3=A(FACWRK WRKIO block)                     
         L     R4,WRKIAREC                                                      
H        USING FW_D,R4             Build FACWRK header record                   
         LA    R0,H.FW_D                                                        
         LHI   R1,FW_HDRL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LHI   R0,FW_HDRL                                                       
         STCM  R0,3,H.FW_RLEN                                                   
         MVC   H.FW_HSTMP,FWHSTAMP Set SRUPDxx file stamp                       
         MVI   H.FW_HOLAY,FW_OBOTU Set to call SRUPD60 in FACPAK                
         MVC   H.FW_HSENA,LP_SENO  Set SE number                                
         TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    *+8                                                              
         OI    H.FW_FLAG,FW_FOFFL  Set running off-line                         
         TM    LP_OFLG1,LP_OFKFW   Test don't purge =FWK file                   
         JZ    *+8                                                              
         OI    H.FW_FLAG,FW_FKFWK  Set don't purge =FWK file                    
         MVC   H.FW_AGY,LP_AGY     Set agency alpha ID                          
         MVC   H.FW_USER,LP_USRID  Set user-ID                                  
         MVC   H.FW_CPY,CUXCPY     Set company code                             
         MVC   H.FW_CPYS1,CPYSTAT1 Set company status bytes                     
         MVC   H.FW_CPYS2,CPYSTAT2                                              
         MVC   H.FW_CPYS3,CPYSTAT3                                              
         MVC   H.FW_CPYS4,CPYSTAT4                                              
         MVC   H.FW_CPYS5,CPYSTAT5                                              
         MVC   H.FW_CPYS6,CPYSTAT6                                              
         MVC   H.FW_CPYS7,CPYSTAT7                                              
         MVC   H.FW_CPYS8,CPYSTAT8                                              
         MVC   H.FW_CPYS9,CPYSTAT9                                              
         MVC   H.FW_CPYSA,CPYSTATA                                              
*&&UK*&& MVC   H.FW_CPYSB,CPYSTATB                                              
*&&UK*&& MVC   H.FW_CPYSC,CPYSTATC                                              
         MVC   H.FW_CPYGL,CPYGLMOA GL MOA                                       
         MVC   H.FW_CPYSF,CPYSFST  Fiscal Start Date                            
         MVC   H.FW_PERLV,ONERL1L  Set 1R ledger levels                         
         MVC   H.FW_SJALV,PCLILEN  Set SJ ledger levels                         
         MVI   H.FW_BOVLY,FW_BTIME Set time module calling                      
         TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    PUTOUT04            No - use FACWRK                              
         GOTOR TRACEPUT,H.FW_RLEN                                               
         GOTO1 ASORTER,DMCB,SORTPUT,H.FW_RLEN                                   
         J     PUTOUT06                                                         
                                                                                
PUTOUT04 GOTOR DATAMGR,WRKIPARM,ADD                                             
         JNE   *+2                                                              
         DROP  R3,H                                                             
                                                                                
PUTOUT06 TM    FW_FILE,FW_DMGR     Test normal DATAMGR I/O                      
         JNO   PUTOUT08            No                                           
         ICM   R0,15,DMGRSEQ#      Bump record sequence number                  
         AHI   R0,1                                                             
         STCM  R0,15,DMGRSEQ#                                                   
         STCM  R0,15,FW_SEQ#       Set sequence number in record                
                                                                                
PUTOUT08 TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    PUTOUT10            No - use FACWRK                              
         GOTOR TRACEPUT,FW_D                                                    
         GOTO1 ASORTER,DMCB,SORTPUT,FW_D                                        
         J     EXITY                                                            
                                                                                
PUTOUT10 L     R1,WRKBLKR          Put record to FACWRK recovery file           
         MVC   PARM(WRKINDX-WRKIPARM),WRKIPARM-WRKIOD(R1)                       
         GOTOR DATAMGR,PARM,ADD,,,FW_D                                          
         JE    EXITY                                                            
         J     *+2                                                              
         DROP  R2                                                               
                                                                                
TRACEPUT NTR1  BASE=*,LABEL=*      Trace in SYSPRINT (offline)                  
                                                                                
         TM    LP_FLAG,LP_FOFFL    Ensure offline                               
         JZ    *+2                                                              
                                                                                
         LR    R2,R1                                                            
         L     RF,ARUNFACS         Get A(RUNFACS)                               
         L     R3,RCPRINT-RUNFACSD(RF)                                          
         AHI   R3,P_P-P_DPRINT                                                  
         USING PTRACED,R3                                                       
                                                                                
         LLC   R4,FW_RTYP-FW_D(R2)                                              
         MVC   PTRACE(17),=CL17'Sorter Put Trace:'                              
         EDITR (R4),(3,PTRACE+18),0                                             
         GOTOR VHEXOUT,DMCB,0(R2),PTRACE+25,50,0                                
                                                                                
         GOTO1 APRINTER                                                         
                                                                                
TRACEPX  J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Call DMGRITRN to put ADDTRN transaction record to output file       *         
***********************************************************************         
                                                                                
GOATRN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GOATRN*'                                                      
                                                                                
         LTR   R2,R1               Test last time call                          
         JZ    GOATRN04                                                         
         TM    RUNINDS,RUNIATRF    Test first time call                         
         JNZ   GOATRN02                                                         
         OI    RUNINDS,RUNIATRF    Set not first time/Put header                
         GOTOR DMGRITRN,DMCB,('FW_AAHDR',$ADDTRN)                               
                                                                                
GOATRN02 GOTOR DMGRITRN,DMCB,('FW_AATRN',$ADDTRN),(R2)                          
         J     GOATRNX                                                          
                                                                                
GOATRN04 TM    RUNINDS,RUNIATRF    Test any transactions put                    
         JZ    GOATRNX                                                          
         GOTOR DMGRITRN,DMCB,('FW_AAEND',$ADDTRN)                               
                                                                                
GOATRNX  DS    0H                                                               
         J     EXITY                                                            
                                                                                
         USING CPYELD,SCPYEL                                                    
         USING CPXELD,SCPXEL                                                    
                                                                                
***********************************************************************         
* Re-sequence all time elements in a time record                      *         
***********************************************************************         
                                                                                
         USING TIMELD,R1                                                        
SETSEQ   DS    0H                  Point to first element on record             
                                                                                
         XR    RF,RF               RF=Sequence number                           
                                                                                
SSEQ02   CLI   TIMEL,0             Test end of record                           
         BER   RE                                                               
         CLI   TIMEL,TIMELQ        Is this a time element?                      
         JNE   SSEQ06                                                           
         CLI   TIMETYP,TIMEINP     Test start of time cluster                   
         JNE   SSEQ04                                                           
         CHI   RF,FFQ                                                           
         JE    *+2                                                              
         AHI   RF,1                Yes - bump sequence number                   
                                                                                
SSEQ04   STC   RF,TIMSEQ           Set sequence number                          
                                                                                
SSEQ06   LLC   R0,TIMLN            Bump to next element                         
         AR    R1,R0                                                            
         J     SSEQ02                                                           
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Set time                                                            *         
***********************************************************************         
                                                                                
SETTIME  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETTIM*'                                                      
                                                                                
         TIME  DEC                                                              
         SRL   R0,8                                                             
         SLL   R0,4                                                             
         OILL  GR0,X'000C'         'OR' in packed sign value                    
         STCM  R0,B'1111',CURTIME                                               
***US*&& AP    CURTIME,=P'60000'   Adjust to real (EST) time in US              
***US*&& ICM   R0,B'1111',CURTIME  and update R0                                
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Check MOA for open time                                             *         
***********************************************************************         
                                                                                
CHKMOA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CHKMOA*'                                                      
                                                                                
         MVC   DV_DMDAT,DV_DATE                                                 
                                                                                
         MVC   SVMOA,EFFS          = No future limit                            
         CLC   DV_MOA,DV_TODAY     Is time in the future?                       
         JNH   CMOA04                                                           
         MVC   SVMOA,DV_MOA        Future limit=period month                    
                                                                                
CMOA04   MVC   DV_CDAT(2),DV_MOA   Period month                                 
         LA    R2,SVMOA            Future month limit                           
                                                                                
CMOA06   MVI   DV_CDAT+2,X'01'     Set first of month                           
         GOTOR VDATCON,DMCB,(X'81',DV_CDAT),(6,DUB)                             
         LLC   R0,4(R1)                                                         
*&&US                                                                           
         GOTOR BMONVAL,DMCB,((R0),DUB),(49,ACOMFACS),(CULANG,WORK),    +        
               (CUXCPY,0)                                                       
*&&                                                                             
*&&UK                                                                           
         GOTOR BMONVAL,DMCB,((R0),DUB),(49,ACOMFACS),(CULANG,WORK),    +        
               (CUXCPY,CUUSER)                                                  
*&&                                                                             
         CLI   WORK+(BMOERR-BMONVALD),BMOEOKQ                                   
         JE    CMOA08                                                           
                                                                                
         GOTOR VDATCON,DMCB,(1,DV_CDAT),(0,WORK)                                
         GOTOR VADDAY,DMCB,(C'M',WORK),WORK+6,1                                 
         GOTOR VDATCON,DMCB,(0,WORK+6),(1,DV_CDAT)                              
                                                                                
         CLC   DV_CDAT(2),0(R2)    Test if future limit exceeded                
         JL    CMOA06                                                           
         J     CHKMOAN                                                          
                                                                                
CMOA08   MVC   DV_DMMOA,DV_CDAT                                                 
         J     CHKMOAY                                                          
                                                                                
CHKMOAY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
CHKMOAN  MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$IMOAR                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#DATE                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Get cost allocation profiles                                        *         
***********************************************************************         
                                                                                
GETCAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETCAP*'                                                      
                                                                                
         USING COBLOCKD,R2                                                      
         L     R2,ACOBLOCK                                                      
         XC    COBLOCKD(COPTIONS-COBLOCKD),COBLOCKD                             
         MVC   COADM,XDATAMGR                                                   
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CUXCPY                                                    
         MVC   COKOFC,DV_1ROFF                                                  
         MVC   COKDPT,DV_1RDEP                                                  
         MVC   COKSDT,DV_1RSUB                                                  
         MVC   COKPER,DV_1RPER                                                  
         GOTOR VGETCAP,DMCB,COBLOCK                                             
                                                                                
         CLI   COSTATUS,0                                                       
         JNE   *+2                                                              
                                                                                
         MVC   DV_COATM,COATM      Extract option values we need                
         MVC   DV_CODTM,CODTM      Default type of time                         
         MVC   DV_COBRT,COBRTE     Post sales rate not cost for B time          
         MVC   DV_CORRT,CORRTE     Post sales rate not cost for R time          
         MVC   DV_COTUP,COTUP      When is time posted                          
         MVC   DV_COACS,COACS      Approval concurrently or sequential          
         MVC   DV_COFNR,COFNR      Force narrative                              
         MVC   DV_COMAT,COMAT      Materials allowed                            
         MVC   DV_COIOS,COTBTL     Income or suspense for B time                
         MVC   DV_CONJB,CONJB      No job input allowed                         
         MVC   DV_CONDA,CONDA      Number of days allowed in future             
         MVC   DV_COFAP,COFAP      Use account setting for future time          
         MVC   DV_COFTA,COFTA      Type of time future time allowed             
         ZAP   DUB1,CONDO                                                       
         CVB   R4,DUB1                                                          
         LNR   R4,R4                                                            
         MVC   WORK(L'DV_TODF),DV_TODF  Work out Overdue date                   
         GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,(R4)                              
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,DV_OVRDU)                             
*                                                                               
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Get line manager approver                                           *         
***********************************************************************         
                                                                                
GETMAP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETMAP*'                                                      
                                                                                
IN       USING ACTKULA,DV_1RULA                                                 
                                                                                
         USING DPAPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVI   DPAPAPPL,DPAPATIM                                                
         MVC   DPAPCPY,CUXCPY                                                   
         ZAP   DPAPXVAL,PZERO                                                   
                                                                                
         LA    R3,ONERL4L          R3=A(Person length)                          
         LHI   R0,4                R0=Number of levels to search                
                                                                                
GMAP02   MVC   DPAP1RAC,SPACES                                                  
         LLC   RF,0(R3)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   DPAP1RAC(0),IN.ACTKACT                                           
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JNE   *+2                                                              
         J     GMAP06                                                           
                                                                                
GMAP04   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   *+2                                                              
                                                                                
GMAP06   CLC   DPAPAS(DPAPPIDB-DPAPASD),IOKEYSAV                                
         JNE   GMAP08                                                           
         MVC   DV_MANAP,DPAPPIDB                                                
         J     GMAP10                                                           
                                                                                
GMAP08   MVC   DPAPAS,IOKEYSAV     Reset key                                    
         SHI   R3,L'ONERL4L        Point to previous key length                 
         JCT   R0,GMAP02           Do for number of 1R levels                   
         J     GETMAPL                                                          
                                                                                
GMAP10   CLI   DPAPSEQ,0           MAIN APPROVER RECORD?                        
         JNE   GMAP12                                                           
         XC    CSVKEY1,CSVKEY1                                                  
         MVC   CSVKEY1(L'DPAPAS),DPAPAS                                         
         J     GMAP18                                                           
         DROP  IN                                                               
                                                                                
         USING APPRECD,R2          NO, READ MAIN APPROVER RECORD                
GMAP12   LA    R2,IOKEY                                                         
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,DV_MANAP                                                
         XC    CSVKEY1,CSVKEY1                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   *+2                 MAIN APPROVER RECORD MISSING                 
         J     GMAP18                                                           
                                                                                
GMAP14   OC    CSVKEY1,CSVKEY1                                                  
         JZ    GMAP16                                                           
         MVC   IOKEY,CSVKEY1       (RE-)ESTABLISH SEQUENCE                      
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   *+2                                                              
                                                                                
GMAP16   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   *+2                 MAIN APPROVER RECORD MISSING                 
                                                                                
         CLC   APPKEY(APPKSEQ-APPKEY),IOKEYSAV                                  
         JNE   GETMAPY                                                          
         DROP  R2                                                               
                                                                                
GMAP18   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
                                                                                
         L     R1,IOADDR                                                        
         OC    CSVKEY1,CSVKEY1                                                  
         JZ    GMAP19                                                           
         MVC   CSVKEY1(L'APPKEY),0(R1)  SAVE APPKEY                             
                                                                                
GMAP19   AHI   R1,ACCRFST-ACCRECD                                               
                                                                                
         USING LIDELD,R1                                                        
GMAP20   CLI   LIDEL,0             Test end of record                           
         JE    GMAP14              Yes, any more?                               
         CLI   LIDEL,LIDELQ                                                     
         JE    GMAP24                                                           
                                                                                
GMAP22   LLC   R0,LIDLN                                                         
         AR    R1,R0                                                            
         J     GMAP20                                                           
                                                                                
GMAP24   CLI   LIDTYPE,LIDTBACK                                                 
         JNE   GMAP22                                                           
         LA    R4,LIDDATA                                                       
         USING LIDDATA,R4                                                       
         LLC   R3,LIDLN                                                         
         AR    R3,R1               R3=End of element                            
         LA    RF,DV_MANBA                                                      
         XR    RE,RE                                                            
                                                                                
GMAP26   TM    LIDLAPPL,LIDLTIME   Is this entry for timesheets                 
         JZ    GMAP28              No                                           
         MVC   0(L'PIDNO,RF),LIDLPID  Yes - Get back up approver PID            
         LA    RF,L'PIDNO(RF)                                                   
         AHI   RE,1                                                             
                                                                                
GMAP28   CHI   RE,DV_BAMAX         Did we fill up the whole table?              
         JE    GETMAPY             Hope it's enough...                          
         LLC   R0,LIDITLN          Increment R4 to look at next entry           
         AR    R4,R0                                                            
         CR    R3,R4               Check we haven't reached end of el           
         JH    GMAP26              No - check next entry                        
         DROP  R4                                                               
                                                                                
GETMAPY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
GETMAPL  MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INAPP                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PCC                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         J     EXITL                                                            
                                                                                
GETMAPH  MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   R0,TL#PCC                                                        
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
                                                                                
***********************************************************************         
* Set details from 1R accounts all levels                             *         
***********************************************************************         
                                                                                
SET1RD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SET1RD*'                                                      
                                                                                
         MVI   DV_1RFJT,NOQ        Set force job is no                          
         MVI   DV_1RFPT,NOQ        Set force product is no                      
         MVI   LOCALERR,NOQ                                                     
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES       Read person level 1R account                 
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,DV_1RULA                                                 
         MVC   TEMP(14),ACTKULA                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    S1RD02                                                           
         LHI   RF,AE$INACC                                                      
         J     SET1RDH                                                          
                                                                                
S1RD02   TM    ACTKSTAT,ACTSLOCK   Test account is locked                       
         JZ    S1RD04                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$ACTLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PCC                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
S1RD04   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO2                                                          
         LA    R3,ACTRFST                                                       
         MVI   BYTE1,0                                                          
                                                                                
S1RD06   CLI   RSTEL,0                                                          
         JE    S1RD30                                                           
         CLI   RSTEL,RSTELQ                                                     
         JE    S1RD10                                                           
         CLI   RSTEL,SPAELQ                                                     
         JE    S1RD22                                                           
         CLI   RSTEL,NAMELQ                                                     
         JE    S1RD24                                                           
                                                                                
S1RD08   LLC   R1,RSTLN                                                         
         AR    R3,R1                                                            
         J     S1RD06                                                           
                                                                                
S1RD10   MVI   BYTE1,RSTELQ                                                     
         CLI   RSTLN,RSTLN2Q       Must not be a short element                  
         JL    *+2                                                              
         TM    RSTSTAT1,RSTSACIL   Test account is locked                       
         JZ    S1RD12                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$ACTLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PCC                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
S1RD12   CLI   RSTLN,RSTLN3Q       Must not be a short element                  
         JL    S1RD20                                                           
         TM    RSTSTAT7,RSTLCKTS   Test if account has locks                    
         JZ    S1RD14                                                           
         OI    DV_HIND,FW_SLOCK    Submitter locked                             
S1RD14   TM    RSTSTAT7,RSTLCKAP                                                
         JZ    S1RD16                                                           
         OI    DV_HIND,FW_ALOCK    Approver locked                              
S1RD16   TM    RSTSTAT5,RSTSPROD                                                
         JZ    S1RD18                                                           
         MVI   DV_1RFPT,YESQ                                                    
S1RD18   TM    RSTSTAT5,RSTSPRJB                                                
         JZ    S1RD20                                                           
         MVI   DV_1RFJT,YESQ                                                    
                                                                                
S1RD20   MVC   DV_1RCST,RSTCOSTG   Set costing group                            
         J     S1RD08                                                           
                                                                                
         USING SPAELD,R3                                                        
S1RD22   CLI   SPATYPE,SPATINCO    Test income account                          
         JNE   S1RD08                                                           
         MVC   DV_INULA,SPAAULA    Set override income account                  
         J     S1RD08                                                           
                                                                                
         USING NAMELD,R3                                                        
S1RD24   LLC   R1,NAMLN                                                         
         SHI   R1,1+NAMLN1Q                                                     
         MVC   DV_1RNAM(0),NAMEREC                                              
         EXRL  R1,*-6                                                           
         J     S1RD08                                                           
         DROP  R3                                                               
                                                                                
S1RD30   CLI   BYTE1,RSTELQ                                                     
         JNE   *+2                                                              
                                                                                
         LHI   R4,3                R4=Number of levels to do                    
         LA    R3,ONERL3L          R3=A(level lengths)                          
                                                                                
         USING ACTRECD,R2                                                       
S1RD32   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),LEDGER1R                            
         LLC   RF,0(R3)            RF=Length of account at this level           
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),DV_1RULA+2                                            
         EX    RF,0(RE)                                                         
         MVC   TEMP(14),ACTKULA                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    S1RD34                                                           
         LHI   RF,AE$INACC                                                      
         J     SET1RDH                                                          
                                                                                
S1RD34   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING RSTELD,R1                                                        
         L     R2,AIO1                                                          
         LA    R1,ACTRFST                                                       
                                                                                
S1RD36   CLI   RSTEL,0                                                          
         JE    S1RD70                                                           
         CLI   RSTEL,RSTELQ                                                     
         JE    S1RD40                                                           
         CLI   RSTEL,SPAELQ                                                     
         JE    S1RD46                                                           
         CLI   RSTEL,NAMELQ                                                     
         JE    S1RD48                                                           
                                                                                
S1RD38   LLC   R0,RSTLN                                                         
         AR    R1,R0                                                            
         J     S1RD36                                                           
                                                                                
S1RD40   CLC   DV_1RCST,SPACES     Test costing group resolved                  
         JH    S1RD42                                                           
         MVC   DV_1RCST,RSTCOSTG                                                
                                                                                
S1RD42   CLI   RSTLN,RSTLN3Q                                                    
         JL    S1RD38                                                           
         TM    RSTSTAT5,RSTSPROD                                                
         JZ    S1RD44                                                           
         MVI   DV_1RFPT,YESQ                                                    
                                                                                
S1RD44   TM    RSTSTAT5,RSTSPRJB                                                
         JZ    S1RD38                                                           
         MVI   DV_1RFJT,YESQ                                                    
         J     S1RD38                                                           
                                                                                
         USING SPAELD,R1                                                        
S1RD46   DS    0H                                                               
*&&US                                                                           
         CLI   SPATYPE,SPATINCO    Test income account                          
         JNE   S1RD38                                                           
         CLC   DV_INULA,SPACES     Test have income account already             
         JH    S1RD38                                                           
         MVC   DV_INULA,SPAAULA    Set override income account                  
*&&                                                                             
         J     S1RD38                                                           
                                                                                
         USING NAMELD,R1                                                        
S1RD48   LLC   R2,NAMLN                                                         
         SHI   R2,1+NAMLN1Q                                                     
         LA    RE,DV_142NM                                                      
         CHI   R4,2                Test department level                        
         JE    S1RD50                                                           
         LA    RE,DV_143NM                                                      
         CHI   R4,3                Test sub-department level                    
         JNE   S1RD38                                                           
S1RD50   MVC   0(0,RE),NAMEREC                                                  
         EXRL  R2,*-6                                                           
         J     S1RD38                                                           
         DROP  R1                                                               
                                                                                
S1RD70   SHI   R3,1                Back-up to previous length                   
         JCT   R4,S1RD32           Do for number of levels                      
                                                                                
         CLI   LOCALERR,YESQ       Any buffered error?                          
         JE    SET1RDL                                                          
                                                                                
SET1RDY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
SET1RDL  DS    0H                                                               
         J     EXITL                                                            
                                                                                
SET1RDH  MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   R0,TL#PCC                                                        
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Get period end date (in: DV_DATE - out: DV_PEDAT/DV_LEDAT)          *         
***********************************************************************         
                                                                                
GETPED   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETPED*'                                                      
                                                                                
         XR    RE,RE                                                            
         ICM   RE,B'0001',CPYSFST                                               
         JNZ   GPED02              Check to see if Fiscal Start set             
         AHI   RE,X'F1'               If not, set it to Jan                     
                                                                                
GPED02   CHI   RE,X'F0'                                                         
         JH    GPED04                                                           
         AHI   RE,X'F0'-X'C0'+X'09'                                             
                                                                                
GPED04   SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
         MVC   DV_CSTR,DV_DATE                                                  
         MVC   DV_CSTR+1(1),BYTE1                                               
         CLC   DV_DATE+1(1),BYTE1  check with NSHE (was DV_PEDAT)               
         JNL   GPED06                                                           
                                                                                
         GOTOR VDATCON,DMCB,(1,DV_CSTR),(0,WORK)                                
         GOTOR VADDAY,DMCB,(C'Y',WORK),WORK+6,-1                                
         GOTOR VDATCON,DMCB,(0,WORK+6),(1,DV_CSTR)                              
                                                                                
GPED06   GOTOR VDATCON,DMCB,(1,DV_CSTR),(0,WORK)                                
         GOTOR VADDAY,DMCB,(C'M',WORK),WORK+6,11                                
         GOTOR VDATCON,DMCB,(0,WORK+6),(1,DV_CEND)                              
                                                                                
K        USING CASKEY,IOKEY                                                     
         XC    K.CASKEY,K.CASKEY   Read calendar for office                     
         MVI   K.CASKTYP,CASKTYPQ                                               
         MVI   K.CASKSUB,CASKSUBQ                                               
         MVC   K.CASKCPY,CUXCPY                                                 
         MVC   K.CASKEMOA,DV_CEND                                               
         MVC   K.CASKSMOA,DV_CSTR                                               
         MVC   K.CASKOFC,DV_1ROFF  Set person office code                       
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    GPED08                                                           
                                                                                
         MVC   K.CASKEY,IOKEYSAV   Read all office calendar                     
         MVC   K.CASKOFC,SPACES                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JNE   GPED10                                                           
         DROP  K                                                                
                                                                                
GPED08   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    GPED12                                                           
                                                                                
GPED10   MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$NOCAL                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#DATE                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     GETPEDN                                                          
                                                                                
         USING CASRECD,R2                                                       
GPED12   L     R2,AIO2                                                          
         LA    R3,CASRFST                                                       
         USING TMPELD,R3                                                        
                                                                                
GPED14   CLI   TMPEL,0             Locate period element in calendar            
         JE    *+2                                                              
         CLI   TMPEL,TMPELQ                                                     
         JNE   GPED16                                                           
         CLC   DV_DATE,TMPSTART    Match date to period                         
         JL    GPED16                                                           
         CLC   DV_DATE,TMPEND                                                   
         JNH   GPED18                                                           
                                                                                
GPED16   LLC   R0,TMPLN                                                         
         AR    R3,R0                                                            
         J     GPED14                                                           
                                                                                
GPED18   MVC   DV_PERNO,TMPNUMB    Set period number                            
         MVC   DV_MOA,TMPMTH       Set month                                    
         MVC   DV_PSDAT,TMPSTART   Set start and end dates                      
         MVC   DV_PEDAT,TMPEND                                                  
         DROP  R2,R3                                                            
                                                                                
         MVC   DV_LSDAT,DV_PSDAT   Default loc. start date from period          
         MVC   DV_LEDAT,DV_PEDAT   Default loc. end date from period            
         USING LOCELD,R1                                                        
         LA    R1,SLOCEL                                                        
         CLC   LOCSTART,DV_PSDAT   Did location start after                     
         JNH   *+10                 period start date                           
         MVC   DV_LSDAT,LOCSTART   If so use location start date                
         OC    LOCEND,LOCEND       Do we have a location end date               
         JZ    GETPEDY                                                          
         CLC   LOCEND,DV_PEDAT     If location end date is prior to             
         JNL   GETPEDY              period end date, use this date              
         MVC   DV_LEDAT,LOCEND                                                  
                                                                                
GETPEDY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
GETPEDN  DS    0H                                                               
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Validate TL_TTY time type input                                     *         
***********************************************************************         
                                                                                
VALTTY   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALTTY*'                                                      
                                                                                
         MVI   LOCALERR,NOQ                                                     
                                                                                
         CLI   TL_TTYP,SPACEQ                                                   
         JH    VTTY02                                                           
         MVI   TL_TTYP,SPACEQ                                                   
         J     VTTY10                                                           
                                                                                
VTTY02   CLI   TL_TTYP,TL_TBILQ                                                 
         JE    VTTY04                                                           
         CLI   TL_TTYP,TL_TNONQ                                                 
         JE    VTTY04                                                           
         CLI   TL_TTYP,TL_TRELQ                                                 
         JE    VTTY04                                                           
         CLI   TL_TTYP,TL_TDEFQ                                                 
         JE    VTTY04                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$IVTYP                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#TTYP                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         J     VALTTYN                                                          
                                                                                
VTTY04   LA    RF,DV_COATM         Point to valid time types                    
         LHI   R0,L'DV_COATM       R0=Number of valid time types                
                                                                                
VTTY06   CLI   0(RF),C','          Ignore embedded commas                       
         JE    VTTY08                                                           
         CLC   0(1,RF),TL_TTYP     Match type to input                          
         JE    VTTY10              Exit with CC equal on match                  
                                                                                
VTTY08   AHI   RF,1                Else bump and try again                      
         JCT   R0,VTTY06                                                        
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$ITTML          Invalid time type for location              
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#TTYP                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         J     VALTTYN                                                          
                                                                                
VTTY10   DS    0H                  Repeat validation that uses TL_TTYP          
*&&US                                                                           
         TM    DV_JSTA1,JOBSXJOB   Is this an expense job?                      
         JNO   VTTY12                                                           
         CLI   TL_TTYP,TL_TBILQ    Billable time cant post to x-job             
         JNE   VTTY12                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$CUEXJ                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#JOB                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
*&&                                                                             
VTTY12   CLI   OFFIND,FULLYQ       Test office checking                         
         JNE   VTTY14                                                           
         CLC   DV_SJOFF,SPACES                                                  
         JH    VTTY14                                                           
         CLI   TL_TTYP,TL_TNONQ                                                 
         JE    VTTY14                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INVPO                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VTTY14   CLC   DV_1CULA,SPACES                                                  
         JH    VTTY16                                                           
         CLI   TL_TTYP,TL_TNONQ                                                 
         JE    VTTY16                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$COSTG                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VTTY16   CLI   LOCALERR,YESQ       VALWCD VWCD02 TL_TTYP check not              
         JNE   VALTTYY             possible here                                
                                                                                
VALTTYN  DS    0H                                                               
         J     EXITN                                                            
                                                                                
VALTTYY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate TL_CLI client input and extract data                       *         
***********************************************************************         
                                                                                
VALCLI   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALCLI*'                                                      
                                                                                
         MVC   DV_SJOFF,SPACES                                                  
         MVC   DV_1CULA,SPACES                                                  
         MVI   LOCALERR,NOQ                                                     
                                                                                
         CLC   TL_CLI,SPACES       Client passed?                               
         JNH   VALCLIY                                                          
                                                                                
         USING ACTRECD,R2          Build key of client                          
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),TL_CLI                                                
         EX    RF,0(RE)                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VCLI02                                                           
         LHI   RF,AE$INCLI                                                      
         J     VALCLIH                                                          
                                                                                
VCLI02   TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VCLI04                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$CLILK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VCLI04   TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VCLI06                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_CLI),TL_CLI                                         
         LHI   RF,AE$ACTCL                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VCLI06   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,ACTRFST                                                       
         MVI   BYTE1,0                                                          
                                                                                
VCLI08   CLI   RSTEL,0                                                          
         JE    VCLI20                                                           
         CLI   RSTEL,RSTELQ                                                     
         JE    VCLI12                                                           
         CLI   RSTEL,PPRELQ                                                     
         JE    VCLI16                                                           
                                                                                
VCLI10   LLC   R1,RSTLN                                                         
         AR    R3,R1                                                            
         J     VCLI08                                                           
                                                                                
VCLI12   CLI   CUAUTH+1,0          Allow if zero                                
         JE    VCLI14                                                           
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VCLI14              or if it fits                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_CLI),TL_CLI                                         
         LHI   RF,AE$SECLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VCLI14   CLI   RSTLN,RSTLN3Q                                                    
         JL    VCLI10                                                           
         TM    RSTSTAT5,RSTSNOTS   No to timesheets                             
         JZ    VCLI10                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_CLI),TL_CLI                                         
         LHI   RF,AE$INACC                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VCLI10                                                           
                                                                                
         USING PPRELD,R3                                                        
VCLI16   MVI   BYTE1,PPRELQ                                                     
         MVC   DV_SJOFF,PPRGAOFF                                                
         OC    DV_SJOFF,SPACES                                                  
         CLC   PPRCOST,SPACES                                                   
         JNH   VCLI10                                                           
         MVC   DV_1CULA,PPRCOSTU                                                
         OC    DV_1CULA,SPACES                                                  
         J     VCLI10                                                           
         DROP  R3                                                               
                                                                                
VCLI20   MVC   DV_SJCLI,SPACES     Save client code                             
         LLC   R1,PCLILEN                                                       
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   DV_SJCLI(0),ACTKACT                                              
         EX    R1,0(RE)                                                         
         OI    DV_ICPJ,DV_ICLI                                                  
         MVC   DV_SJULA,ACTKULA                                                 
                                                                                
         CLI   BYTE1,PPRELQ                                                     
         JNE   *+2                 (bad data - no PPRELQ)                       
                                                                                
         CLI   LOCALERR,YESQ       Any buffered error?                          
         JE    VALCLIL                                                          
                                                                                
VALCLIY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
VALCLIL  OI    DV_ICPJ,DV_IERR                                                  
         J     EXITL                                                            
                                                                                
VALCLIH  MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#CLI                                                        
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Validate TL_PRO product input and extract data                      *         
***********************************************************************         
                                                                                
VALPRO   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALPRO*'                                                      
                                                                                
         MVI   LOCALERR,NOQ                                                     
                                                                                
         CLC   TL_PRO,SPACES       Product passed?                              
         JNH   VALPROY                                                          
*&&UK                                                                           
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         SR    RF,RE                                                            
         LR    RE,RF               RF=length of product code                    
         LR    R1,RF               R1=length of product code                    
         SHI   RE,1                                                             
         LA    RE,TL_PRO(RE)                                                    
VPRO0A   CLI   0(RE),C' '          Is last character spaces                     
         JH    VPRO0B              No                                           
         SHI   RE,1                Check previous bytes                         
         JCT   RF,VPRO0A                                                        
         DC    H'0'                Die if we get to beginning                   
*                                  RF=number of characters completed            
VPRO0B   CR    R1,RF               Is the product code complete                 
         JE    VPRO0D              Yes                                          
         MVC   TEMP(L'TL_PRO),TL_PRO                                            
         MVC   TL_PRO,SPACES                                                    
         SR    R1,RF               R1=Number of characters to pad               
         LA    RE,TL_PRO(R1)                                                    
         BCTR  RF,0                Reduce RF by 1 for ex                        
         BASR  R1,0                                                             
         MVC   0(0,RE),TEMP                                                     
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         LA    R1,TL_PRO                                                        
VPRO0C   CLI   0(R1),C' '                                                       
         JH    VPRO0D                                                           
         MVI   0(R1),C'0'          Pad with zero                                
         LA    R1,1(R1)                                                         
         JCT   RF,VPRO0C                                                        
*&&                                                                             
                                                                                
VPRO0D   CLC   TL_CLI,SPACES       Ensure we got client                         
         JH    VPRO00                                                           
         LHI   RF,AE$MSCLI                                                      
         J     VALPROH                                                          
                                                                                
         USING ACTRECD,R2          Build key of product                         
VPRO00   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),TL_CLI                                                
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         LA    R1,ACTKACT(RF)                                                   
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),TL_PRO                                                   
         EX    RF,0(RE)                                                         
         BASR  RE,0                                                             
         MVC   DV_SJPRO(0),TL_PRO  Save product code                            
         EX    RF,0(RE)                                                         
         OC    DV_SJPRO,SPACES                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VPRO02                                                           
         LHI   RF,AE$INPRO                                                      
         J     VALPROH                                                          
                                                                                
VPRO02   TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VPRO04                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$PROLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PRO                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VPRO04   TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VPRO06                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_PRO),TL_PRO                                         
         LHI   RF,AE$ACTCL                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PRO                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VPRO06   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,ACTRFST                                                       
         MVI   BYTE1,0                                                          
                                                                                
VPRO08   CLI   RSTEL,0                                                          
         JE    VPRO20                                                           
         CLI   RSTEL,RSTELQ                                                     
         JE    VPRO12                                                           
         CLI   RSTEL,PPRELQ                                                     
         JE    VPRO16                                                           
                                                                                
VPRO10   LLC   R1,RSTLN                                                         
         AR    R3,R1                                                            
         J     VPRO08                                                           
                                                                                
VPRO12   CLI   CUAUTH+1,0          Allow if zero                                
         JE    VPRO14                                                           
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VPRO14              or if it fits                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_PRO),TL_PRO                                         
         LHI   RF,AE$SECLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PRO                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VPRO14   CLI   RSTLN,RSTLN3Q                                                    
         JL    VPRO10                                                           
         TM    RSTSTAT5,RSTSNOTS   No to timesheets                             
         JZ    VPRO10                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_PRO),TL_PRO                                         
         LHI   RF,AE$INACC                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#PRO                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VPRO10                                                           
                                                                                
         USING PPRELD,R3                                                        
VPRO16   MVI   BYTE1,PPRELQ                                                     
         CLC   PPRGAOFF,SPACES                                                  
         JNH   VPRO18                                                           
         MVC   DV_SJOFF,PPRGAOFF                                                
         OC    DV_SJOFF,SPACES                                                  
                                                                                
VPRO18   CLC   PPRCOST,SPACES                                                   
         JNH   VPRO10                                                           
         MVC   DV_1CULA,PPRCOSTU                                                
         OC    DV_1CULA,SPACES                                                  
         J     VPRO10                                                           
         DROP  R3                                                               
                                                                                
VPRO20   OI    DV_ICPJ,DV_IPRO                                                  
         MVC   DV_SJULA,ACTKULA                                                 
                                                                                
         CLI   BYTE1,PPRELQ                                                     
         JNE   *+2                 (bad data - no PPRELQ)                       
                                                                                
         CLI   LOCALERR,YESQ       Any buffered error?                          
         JE    VALPROL                                                          
                                                                                
VALPROY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
VALPROL  OI    DV_ICPJ,DV_IERR                                                  
         J     EXITL                                                            
                                                                                
VALPROH  MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#PRO                                                        
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Validate TL_JOB job input and extract data                          *         
***********************************************************************         
                                                                                
VALJOB   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALJOB*'                                                      
                                                                                
         MVI   LOCALERR,NOQ                                                     
         MVI   DV_JSTA1,0                                                       
                                                                                
         CLC   TL_JOB,SPACES       Job passed?                                  
         JNH   VALJOBY                                                          
                                                                                
         CLC   TL_CLI,SPACES       Ensure we got client                         
         JH    VJOB02                                                           
         LHI   RF,AE$MSCLI                                                      
         J     VALJOBH                                                          
                                                                                
VJOB02   CLC   TL_PRO,SPACES       Ensure we got product                        
         JH    VJOB04                                                           
         LHI   RF,AE$MSPRD                                                      
         J     VALJOBH                                                          
                                                                                
         USING ACTRECD,R2          Build key of job                             
VJOB04   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         LLC   RF,PCLILEN                                                       
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),TL_CLI                                                
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         LA    R1,ACTKACT(RF)                                                   
         LLC   RF,PPROLEN                                                       
         LLC   RE,PCLILEN                                                       
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),TL_PRO                                                   
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
         AR    R1,RF                                                            
         LLC   RF,PJOBLEN                                                       
         LLC   RE,PPROLEN                                                       
         SR    RF,RE                                                            
         CHI   RF,L'TL_JOB                                                      
         JL    VJOB06                                                           
         LHI   RF,L'TL_JOB                                                      
                                                                                
VJOB06   SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R1),TL_JOB                                                   
         EX    RF,0(RE)                                                         
         BASR  RE,0                                                             
         MVC   DV_SJJOB(0),TL_JOB  Save job code                                
         EX    RF,0(RE)                                                         
         OC    DV_SJJOB,SPACES                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VJOB08                                                           
         LHI   RF,AE$INJOB                                                      
         J     VALJOBH                                                          
                                                                                
VJOB08   TM    ACTKSTAT,ACTSDRFT                                                
         JZ    VJOB10                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'IS_JOB),IS_JOB                                         
         MVC   XERRTXT+L'IS_JOB+1(L'TL_JOB),TL_JOB                              
         LHI   RF,AE$ACCNA                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#JOB                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VJOB10   TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VJOB12                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$JOBLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#JOB                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VJOB12   TM    ACTKSTAT,ACTSCLOS                                                
         JZ    VJOB14                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$JOBCL                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#JOB                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VJOB14   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,ACTRFST                                                       
                                                                                
VJOB16   CLI   RSTEL,0                                                          
         JE    VJOB40                                                           
         CLI   RSTEL,NAMELQ                                                     
         JE    VJOB34                                                           
         CLI   RSTEL,RSTELQ                                                     
         JE    VJOB22                                                           
         CLI   RSTEL,PPRELQ                                                     
         JE    VJOB26                                                           
         CLI   RSTEL,JOBELQ                                                     
         JE    VJOB30                                                           
                                                                                
VJOB20   LLC   R1,RSTLN                                                         
         AR    R3,R1                                                            
         J     VJOB16                                                           
                                                                                
VJOB22   CLI   CUAUTH+1,0          Allow if zero                                
         JE    VJOB24                                                           
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VJOB24              or if it fits                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_JOB),TL_JOB                                         
         LHI   RF,AE$SECLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#JOB                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VJOB24   CLI   RSTLN,RSTLN3Q                                                    
         JL    VJOB20                                                           
         TM    RSTSTAT5,RSTSNOTS   No to timesheets                             
         JZ    VJOB25                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_JOB),TL_JOB                                         
         LHI   RF,AE$INACC                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#JOB                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VJOB25   TM    RSTLSTAT,RSTLSTIQ   No to timesheets                             
         JZ    VJOB20                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_JOB),TL_JOB                                         
         LHI   RF,AE$INACC                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#JOB                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VJOB20                                                           
                                                                                
         USING PPRELD,R3                                                        
VJOB26   CLC   PPRGAOFF,SPACES                                                  
         JNH   VJOB28                                                           
         MVC   DV_SJOFF,PPRGAOFF                                                
         OC    DV_SJOFF,SPACES                                                  
                                                                                
VJOB28   CLC   PPRCOST,SPACES                                                   
         JNH   VJOB20                                                           
         MVC   DV_1CULA,PPRCOSTU                                                
         OC    DV_1CULA,SPACES                                                  
         J     VJOB20                                                           
                                                                                
         USING JOBELD,R3                                                        
VJOB30   CLI   JOBLN,JOBLN3Q                                                    
         JL    VJOB20                                                           
*&&US                                                                           
         MVC   DV_JSTA1,JOBSTA1                                                 
         TM    JOBSTA1,JOBSART                                                  
         JZ    VJOB32                                                           
         OI    DV_ICPJ,DV_EADJ     Job Eliglible for rate Adjustment            
                                                                                
VJOB32   TM    JOBSTA1,JOBSXJOB    Is this an expense job?                      
         JNO   VJOB20                                                           
         CLI   TL_TTYP,TL_TBILQ    Billable time cant post to x-job             
         JNE   VJOB20                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$CUEXJ                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#JOB                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
*&&                                                                             
         J     VJOB20                                                           
                                                                                
         USING NAMELD,R3                                                        
VJOB34   LLC   R1,NAMLN                                                         
         SHI   R1,1+NAMLN1Q                                                     
         MVC   DV_SJNAM(0),NAMEREC                                              
         EXRL  R1,*-6                                                           
         J     VJOB20                                                           
         DROP  R3                                                               
                                                                                
VJOB40   OI    DV_ICPJ,DV_IJOB                                                  
         MVC   DV_SJULA,ACTKULA                                                 
         DS    0H                  OK to have no PPREL on job                   
                                                                                
         CLI   LOCALERR,YESQ       Any buffered error?                          
         JE    VALJOBL                                                          
                                                                                
VALJOBY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
VALJOBL  OI    DV_ICPJ,DV_IERR                                                  
         J     EXITL                                                            
                                                                                
VALJOBH  MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#JOB                                                        
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Validate security and limit list access to account entered          *         
***********************************************************************         
                                                                                
VALLIM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALLIM*'                                                      
*                                                                               
* check whether user has security to override limit list                        
*                                                                               
                                                                                
         GOTOR VSECRET,DMCB,('SECPRACT+SECPOSP',ASECBLK),              +        
               ('RECLIM',=AL1(ACTOVRD)),SECPROG                                 
         JE    VALLIMY             User has override set as Y                   
*                                                                               
* Get the limit list access rights for the connected user                       
*                                                                               
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSAINI',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         MVI   GAPLPARM,GAPLACLS   Set account list mode                        
*                                                                               
* Get SJ limit access rights                                                    
*                                                                               
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT2Q',TSARABUF),            +        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),('QTIME',0)            
         JNE   VALLIMH                                                          
         MVI   LIMINDS1,LIMSJLQ     Have access to a limited list               
*                                     of clients and products                   
         MVI   GAPLPARM,GAPLPADD+GAPLACLS Concatenate media list                
         XC    GAPAREA,GAPAREA                                                  
         MVI   GAPAREA+GAPTDAT1-GAPTABD,GAPTT2Q                                 
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         CLC   GAPAREA+GAPTCODE-GAPTABD(L'GAPTCODE),SPACES                      
         JH    VLIM010                                                          
         MVI   LIMINDS1,LIMSJAQ     Have access to all client prod              
*                                                                               
* Get Media limit access rights                                                 
*                                                                               
VLIM010  TM    DV_ICPJ,DV_IJOB      Do we have a job                            
         JZ    VLIM020              No - don't bother validating media          
*                                                                               
         GOTOR (#GAPLST,AGAPLST),DMCB,('GAPTT5Q',TSARABUF),            +        
               ('GAPLLIML',SPACES),('GAPLTACC',GAPLPARM),('QTIME',0)            
         JNE   VALLIMH              No media list found                         
                                                                                
         XC    GAPAREA,GAPAREA     Is it all media codes?                       
         MVI   GAPAREA+GAPTDAT1-GAPTABD,GAPTT5Q                                 
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         CLC   GAPAREA+GAPTCODE-GAPTABD(L'GAPTCODE),SPACES                      
         JNH   VLIM020             Access to all media                          
                                                                                
GAP      USING GAPTABD,GAPAREA                                                  
         XC    GAPAREA,GAPAREA     Check we have access to media                
         MVI   GAP.GAPTDAT1,GAPTT5Q                         entered             
         MVC   GAP.GAPTCODE(L'ACTKACT),SPACES                                   
         MVC   GAP.GAPTCODE(1),TL_JOB                                           
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   VALLIMH                                                          
         CLI   GAP.GAPTDAT1,GAPTT5Q                                             
         JNE   VALLIMH                                                          
         CLC   GAP.GAPTCODE(1),TL_JOB                                           
         JNE   VALLIMH                                                          
                                                                                
VLIM020  CLI   LIMINDS1,LIMSJAQ                                                 
         JE    VALLIMY                                                          
         TM    DV_ICPJ,DV_ICLI     Do we have a client                          
         JZ    *+2                 Should always have a client                  
         LA    R3,PCLILEN                                                       
         LA    R2,1                R2=Number of levels of SJ                    
         TM    DV_ICPJ,DV_IPRO     Do we have a product                         
         JZ    VLIM030                                                          
         LA    R3,PPROLEN                                                       
         LA    R2,2                                                             
         TM    DV_ICPJ,DV_IJOB     Do we have a job                             
         JZ    VLIM030                                                          
         LA    R3,PJOBLEN                                                       
         LA    R2,3                                                             
*                                                                               
* Check our access to the client product                                        
*                                                                               
GAP      USING GAPTABD,GAPAREA                                                  
VLIM030  XC    GAPAREA,GAPAREA                                                  
         MVI   GAP.GAPTDAT1,GAPTT2Q                                             
         MVC   GAP.GAPTCOFF,DV_SJOFF                                            
         LLC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   GAP.GAPTACC(0),DV_SJULA+L'ACTKUNT+L'ACTKLDG                      
         EX    RF,0(RE)                                                         
         GOTOR (#GOATSR,AGOATSR),DMCB,('TSARDH',ATSRERRS),0,GAPAREA,   +        
               TSARABUF                                                         
         TM    ATSRERRS,TSEEOF                                                  
         JNZ   VLIM040                                                          
         CLI   GAP.GAPTDAT1,GAPTT2Q                                             
         JNE   VLIM040                                                          
         CLC   GAP.GAPTCOFF,DV_SJOFF                                            
         JNE   VLIM040                                                          
         LLC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         CLC   GAP.GAPTACC(0),DV_SJULA+L'ACTKUNT+L'ACTKLDG                      
         EX    RF,0(RE)                                                         
         JE    VALLIMY                                                          
                                                                                
VLIM040  BCTR  R3,0                Move R3 to preceeding SJ level               
         JCT   R2,VLIM030                                                       
         J     VALLIMH                                                          
                                                                                
VALLIMY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
VALLIML  DS    0H                                                               
         J     EXITL                                                            
                                                                                
VALLIMH  MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#CLI                                                        
         LHI   RF,AE$ACRTS                                                      
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,=AL1(TL#CLI)                                  
         J     EXITH                                                            
***********************************************************************         
* Validate Cli/Pro/Job subsidiary data                                *         
***********************************************************************         
                                                                                
VALSUB   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALSUB*'                                                      
                                                                                
         MVI   LOCALERR,NOQ                                                     
                                                                                
         MVC   DV_SJCLO,DV_SJOFF                                                
         TM    CPYSTAT1,CPYSOROE                                                
         JNZ   VSUB00                                                           
         MVC   DV_SJOFF,SPACES                                                  
                                                                                
VSUB00   CLI   OFFIND,FULLYQ       Test office checking                         
         JNE   VSUB04                                                           
         CLC   DV_SJOFF,SPACES                                                  
         JH    VSUB02                                                           
         CLI   TL_TTYP,TL_TNONQ                                                 
         JE    VSUB04                                                           
         CLI   TL_TTYP,TL_TDEFQ    (see VALTTY)                                 
         JE    VSUB04                                                           
         LHI   RF,AE$INVPO                                                      
         J     VALSUBH                                                          
                                                                                
VSUB02   CLI   CUACCS,0            Skip if no limit access                      
         JE    VSUB04                                                           
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA         Validate production office                   
         MVC   OFFAOFFC,DV_SJOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTOR VOFFAL                                                           
         DROP  R1                                                               
         JE    VSUB04                                                           
         LHI   RF,AE$SECLK                                                      
         J     VALSUBH                                                          
                                                                                
VSUB04   XC    DV_1CNAM,DV_1CNAM                                                
         CLC   DV_1CULA,SPACES                                                  
         JH    VSUB06                                                           
         CLI   TL_TTYP,TL_TNONQ                                                 
         JE    VSUB30                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$COSTG                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
         USING ACTRECD,R2                                                       
VSUB06   LA    R2,IOKEY            Read for costing account                     
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,DV_1CULA                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VSUB08                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_1CULA),DV_1CULA                                     
         LHI   RF,AE$ILCAC                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VSUB30                                                           
                                                                                
VSUB08   TM    ACTKSTAT,ACTSDRFT                                                
         JZ    VSUB10                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_1CULA),DV_1CULA                                     
         LHI   RF,AE$ACCNA                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VSUB10   TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VSUB12                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_1CULA),DV_1CULA                                     
         LHI   RF,AE$ILCAC                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VSUB12   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,ACTRFST                                                       
                                                                                
VSUB14   CLI   RSTEL,0                                                          
         JE    VSUB30                                                           
         CLI   RSTEL,RSTELQ                                                     
         JE    VSUB18                                                           
         CLI   RSTEL,NAMELQ                                                     
         JE    VSUB20                                                           
                                                                                
VSUB16   LLC   R1,RSTLN                                                         
         AR    R3,R1                                                            
         J     VSUB14                                                           
                                                                                
VSUB18   CLI   CUAUTH+1,0          Allow if zero                                
         JE    VSUB16                                                           
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VSUB16              or if it fits                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_1CULA),DV_1CULA                                     
         LHI   RF,AE$SECLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#CLI                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VSUB16                                                           
         DROP  R3                                                               
                                                                                
         USING NAMELD,R3                                                        
VSUB20   LLC   R1,NAMLN                                                         
         SHI   R1,1+NAMLN1Q                                                     
         MVC   DV_1CNAM(0),NAMEREC                                              
         EXRL  R1,*-6                                                           
         J     VSUB16                                                           
         DROP  R3                                                               
                                                                                
VSUB30   DS    0H                                                               
                                                                                
         CLI   LOCALERR,YESQ       Any buffered error?                          
         JE    VALSUBL                                                          
                                                                                
VALSUBY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
VALSUBL  DS    0H                                                               
         J     EXITL                                                            
                                                                                
VALSUBH  MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#CLI                                                        
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Validate TL_WCD work code input and extract data                    *         
***********************************************************************         
                                                                                
VALWCD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALWCD*'                                                      
                                                                                
         MVI   LOCALERR,NOQ                                                     
                                                                                
         CLC   TL_WCD,SPACES       Work code/task code passed?                  
         JH    VWCD04                                                           
         TM    DV_ICPJ,DV_IJOB                                                  
         JZ    VALWCDY                                                          
         LHI   RF,AE$WCREQ                                                      
         CLI   TL_TTYP,TL_TBILQ                                                 
         JNE   VWCD02                                                           
         LHI   RF,AE$WCNIB                                                      
                                                                                
VWCD02   MVC   XERRTXT,SPACES                                                   
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#WCD                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VALWCDL                                                          
                                                                                
VWCD04   CLC   TL_WCD,WC_99        Can't be special types                       
         JE    VWCD06                                                           
         CLC   TL_WCD,WC_ASTS      (**)                                         
         JNE   VWCD08                                                           
                                                                                
VWCD06   MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INWRK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#WCD                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VWCD40                                                           
                                                                                
VWCD08   DS    0H                                                               
*&&UK                                                                           
         CLI   CUCTRY,CTRYGER      Type of work code OK?                        
         JNE   VWCD10                                                           
         CLI   TL_WCD,C'0'         Disallow external work codes                 
         JL    VWCD10                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INWRK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#WCD                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VWCD40                                                           
*&&                                                                             
         USING WCORECD,R2                                                       
VWCD10   LA    R2,IOKEY             Build key of work code record               
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUXCPY                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,TL_WCD                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VWCD12                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$WRKNF                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#WCD                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VWCD40                                                           
                                                                                
VWCD12   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
                                                                                
         USING WCOELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,WCORFST                                                       
                                                                                
VWCD14   CLI   WCOEL,0                                                          
         JNE   VWCD16                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INWRK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#WCD                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VWCD40                                                           
                                                                                
VWCD16   CLI   WCOEL,WCOELQ                                                     
         JE    VWCD18                                                           
         LLC   R1,WCOLN                                                         
         AR    R3,R1                                                            
         J     VWCD14                                                           
                                                                                
VWCD18   TM    WCOSTAT2,WCOSLPST   work code locked for postings?               
         JZ    VWCD20                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$WCLCK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#WCD                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VWCD20   DS    0H                                                               
*&&US                                                                           
         TM    WCOSTAT2,WCOSART    Rate Eligible for adjustment                 
         JZ    VWCD22                                                           
         OI    DV_ICPJ,DV_EADJ                                                  
*&&                                                                             
                                                                                
VWCD22   TM    WCOSTAT,WCOSHCOE    Hours carried in original est?               
*&&UK*&& JNZ   VWCD24                                                           
*&&US*&& J     VWCD24                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   RF,AE$INWRK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#WCD                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VWCD24   DS    0H                                                               
         DROP  R2,R3                                                            
                                                                                
VWCD40   DS    0H                                                               
                                                                                
         CLI   LOCALERR,YESQ       Any buffered error?                          
         JE    VALWCDL                                                          
                                                                                
VALWCDY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
VALWCDL  DS    0H                                                               
         J     EXITL                                                            
                                                                                
VALWCDH  MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#WCD                                                        
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
                                                                                
***********************************************************************         
* Validate TL_JOB job input and extract data                          *         
***********************************************************************         
                                                                                
VALNCA   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALNCA*'                                                      
                                                                                
         XC    DV_1NNAM,DV_1NNAM                                                
                                                                                
         CLC   TL_1NA,SPACES                                                    
         JNH   VALNCAY                                                          
                                                                                
         MVI   LOCALERR,NOQ                                                     
                                                                                
         CLI   TL_TTYP,TL_TDEFQ                                                 
         JNE   VNCA00                                                           
         MVI   TL_TTYP,TL_TNONQ                                                 
                                                                                
         USING LDGRECD,R2                                                       
VNCA00   LA    R2,IOKEY                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUXCPY                                                   
         MVC   LDGKUNT(L'LEDGER1N),LEDGER1N                                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   *+2                                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
                                                                                
         USING LDGELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,LDGRFST                                                       
         MVI   BYTE1,0                                                          
                                                                                
VNCA02   CLI   LDGEL,0                                                          
         JE    VNCA10                                                           
         CLI   LDGEL,LDGELQ                                                     
         JE    VNCA06                                                           
         CLI   LDGEL,RSTELQ                                                     
         JE    VNCA08                                                           
                                                                                
VNCA04   LLC   R1,LDGLN                                                         
         AR    R3,R1                                                            
         J     VNCA02                                                           
                                                                                
VNCA06   MVC   BYTE1,LDGOPOS                                                    
         J     VNCA04                                                           
                                                                                
         USING RSTELD,R3                                                        
VNCA08   CLI   CUAUTH+1,0          Allow if zero                                
         JE    VNCA04                                                           
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VNCA04              or if it fits                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_1NA),TL_1NA                                         
         LHI   RF,AE$SECLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#1NA                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VNCA04                                                           
         DROP  R2,R3                                                            
                                                                                
VNCA10   CLI   OFFIND,NONEQ        No office - no checking                      
         JE    VNCA20                                                           
         CLI   BYTE1,0             OFFPOS=T scenarios are OK                    
         JE    VNCA20                                                           
         CLI   BYTE1,LDGOTRAN                                                   
         JE    VNCA20                                                           
                                                                                
         NI    BYTE1,FF-LDGOKEY2                                                
         LLC   RF,BYTE1                                                         
         LA    RF,TL_1NA-1(RF)                                                  
                                                                                
         CLI   CUACCS,0            Skip if no limit access                      
         JE    VNCA20                                                           
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC+0(1),0(RF)                                              
         MVI   OFFAOFFC+1,C' '                                                  
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    VNCA12                                                           
         MVC   OFFAOFFC,0(RF)                                                   
                                                                                
VNCA12   MVI   OFFAACT,OFFAVAL                                                  
         GOTOR VOFFAL              Validate office                              
         DROP  R1                                                               
         JE    VNCA20                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'TL_1NA),TL_1NA                                         
         LHI   RF,AE$SECLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#1NA                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VNCA40                                                           
                                                                                
         USING ACTRECD,R2                                                       
VNCA20   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'LEDGER1N),LEDGER1N                                     
         MVC   ACTKACT,TL_1NA                                                   
         MVC   TEMP(L'ACTKULA),ACTKULA                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VNCA22                                                           
                                                                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$INACC                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#1NA                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
         J     VNCA40                                                           
                                                                                
VNCA22   TM    ACTKSTAT,ACTSLOCK                                                
         JZ    VNCA24                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$ACTLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#1NA                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VNCA24   TM    ACTKSTAT,ACTSABLP                                                
         JNZ   VNCA26                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$INACP                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#1NA                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VNCA26   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   *+2                                                              
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,ACTRFST                                                       
                                                                                
VNCA28   CLI   RSTEL,0                                                          
         JE    VNCA38                                                           
         CLI   RSTEL,RSTELQ                                                     
         JE    VNCA32                                                           
         CLI   RSTEL,NAMELQ                                                     
         JE    VNCA36                                                           
                                                                                
VNCA30   LLC   R1,RSTLN                                                         
         AR    R3,R1                                                            
         J     VNCA28                                                           
                                                                                
VNCA32   CLI   CUAUTH+1,0          Allow if zero                                
         JE    VNCA34                                                           
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   VNCA34              or if it fits                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(14),TEMP                                                 
         LHI   RF,AE$SECLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#1NA                                                        
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
VNCA34   CLI   RSTLN,RSTLN3Q                                                    
         JL    VNCA30                                                           
         MVC   DV_RST7,RSTSTAT7    Save future time byte                        
         J     VNCA30                                                           
         DROP  R3                                                               
                                                                                
         USING NAMELD,R3                                                        
VNCA36   LLC   R1,NAMLN                                                         
         SHI   R1,1+NAMLN1Q                                                     
         MVC   DV_1NNAM(0),NAMEREC                                              
         EXRL  R1,*-6                                                           
         J     VNCA30                                                           
         DROP  R3                                                               
                                                                                
VNCA38   OI    DV_ICPJ,DV_INON     Non-client found                             
         CLI   ACTRSAF2,C'T'                                                    
         JNE   VNCA40                                                           
         OI    DV_ICPJ,DV_1NHOL    is holiday                                   
                                                                                
VNCA40   MVC   DV_1NULA,ACTKULA                                                 
         DROP  R2                                                               
                                                                                
         CLI   LOCALERR,YESQ       Any buffered error?                          
         JE    VALWCDL                                                          
                                                                                
VALNCAY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
VALNCAL  DS    0H                                                               
         J     EXITL                                                            
                                                                                
VALNCAH  MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#1NA                                                        
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
                                                                                
***********************************************************************         
* Set MOS from TIMEL into key status                                  *         
***********************************************************************         
                                                                                
SETMOS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETMOS*'                                                      
                                                                                
         USING TIMRECD,R2                                                       
         L     R2,AIO2                                                          
         LA    RF,TIMRFST                                                       
                                                                                
         USING TIMELD,RF                                                        
         XC    HALF2,HALF2                                                      
         MVC   HALF1,EFFS                                                       
                                                                                
SETMOS02 CLI   TIMEL,0             Test end of record                           
         JE    SETMOSX                                                          
         CLI   TIMEL,TIMELQ        Test time element                            
         JNE   SETMOS06                                                         
                                                                                
         CLI   TIMETYP,TIMEITMS    Is it materials?                             
         JNE   SETMOS04                                                         
         CLC   HALF2,TIMIMOA                                                    
         JNL   *+10                                                             
         MVC   HALF2,TIMIMOA                                                    
         CLC   HALF1,TIMIMOA                                                    
         JNH   *+10                                                             
         MVC   HALF1,TIMIMOA                                                    
         J     SETMOS06                                                         
                                                                                
SETMOS04 CLI   TIMETYP,TIMEINP     Is it time?                                  
         JNE   SETMOS06                                                         
         CLC   HALF2,TIMMOA                                                     
         JNL   *+10                                                             
         MVC   HALF2,TIMMOA                                                     
         CLC   HALF1,TIMMOA                                                     
         JNH   SETMOS06                                                         
         MVC   HALF1,TIMMOA                                                     
                                                                                
SETMOS06 LLC   R0,TIMLN            Bump to next element                         
         AR    RF,R0                                                            
         J     SETMOS02                                                         
                                                                                
SETMOSX  OC    HALF2,HALF2                                                      
         JZ    *+2                 No time elements found/bad MOS               
                                                                                
         MVC   TIMRLMOS,HALF1                                                   
         MVC   TIMRHMOS,HALF2                                                   
         J     EXIT                                                             
         DROP  R2,RF                                                            
                                                                                
***********************************************************************         
* Set data from options                                               *         
***********************************************************************         
                                                                                
SETDFO   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETDFO*'                                                      
                                                                                
         MVI   LOCALERR,NOQ                                                     
                                                                                
         TM    DV_ICPJ,DV_ICLI     Test production ledger                       
         JZ    SETDFOY                                                          
         TM    DV_ICPJ,DV_IERR     Any error in Cli/pro/Job checks?             
         JNZ   SDFO90              Yes - don't call get opt                     
                                                                                
         L     R0,AGOBLOCB                                                      
         LHI   R1,GOBLOCKX-GOBLOCKD                                             
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R0,AGOXBLCK                                                      
         LHI   R1,GOXBLKX-GOXBLOCK                                              
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R0,AGOBBLCK                                                      
         LHI   R1,GOBBLKXX-GOBBLOCK                                             
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R3,AGOBLOCB                                                      
         USING GOBLOCKD,R3                                                      
         MVC   GOADM,XDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US uses 1st extension block                  
*&&UK*&& MVC   GOABEXT,AGOBBLCK    UK uses 2nd extension block                  
                                                                                
         MVC   GOSELCUL(L'CUXCPY),CUXCPY                                        
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL                   
         MVC   GOSELCLI,DV_SJCLI                                                
         MVC   GOSELPRO,DV_SJPRO                                                
         MVC   GOSELJOB,DV_SJJOB                                                
         MVC   GOSELWC,TL_WCD                                                   
                                                                                
         GOTOR VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
*&&UK*&& MVC   DV_TTALL,GOTTALLW                                                
*&&UK*&& MVC   DV_TFNAR,GOTFNARR                                                
*&&UK*&& MVC   DV_TNOJB,GOTNOJOB                                                
                                                                                
         L     R3,AGOXBLCK                                                      
         USING GOXBLKD,R3                                                       
         MVC   DV_TOT,GOTOT                                                     
         MVC   DV_FPT,GOFPT                                                     
         MVC   DV_FJT,GOFJT                                                     
*&&US*&& MVC   DV_TFNAR,GOTFNARR                                                
*&&US*&& MVC   DV_INACT,GOICA                                                   
*&&US*&& MVC   DV_TTALL,GOTTALLW                                                
*&&US*&& MVC   DV_TNOJB,GOTNOJOB                                                
                                                                                
         L     R3,AGOBBLCK                                                      
         USING GOBBLKD,R3                                                       
*&&UK*&& MVC   DV_INACT,GOINCAC                                                 
         DROP  R3                                                               
                                                                                
         CLI   TL_TTYP,TL_TDEFQ                                                 
         JNE   SDFO06                                                           
         MVC   TL_TTYP,DV_TOT                                                   
         CLI   DV_TOT,SPACEQ       Do we have a default type of time            
         JH    SDFO06                    from client prod job                   
         CLI   DV_CODTM,SPACEQ     No - then look at cost profile value         
         JNH   SDFO05                    for default type of time               
         CLI   DV_CODTM,CONODEFL   No default set then error                    
         JE    SDFO05                                                           
         CLI   DV_CODTM,COSETB                                                  
         JNE   SDFO02                                                           
         MVI   TL_TTYP,TL_TBILQ                                                 
                                                                                
SDFO02   CLI   DV_CODTM,COSETR                                                  
         JNE   SDFO04                                                           
         MVI   TL_TTYP,TL_TRELQ                                                 
                                                                                
SDFO04   CLI   DV_CODTM,COSETN                                                  
         JNE   *+12                                                             
         MVI   TL_TTYP,TL_TNONQ                                                 
         J     SDFO06                                                           
*                                                                               
SDFO05   MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#TTYP                                                       
         LHI   RF,AE$DTTMO         Default time type does not exist             
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     SETDFOL                                                          
                                                                                
SDFO06   DS    0H                                                               
*                                                                               
         LA    R1,DV_TTALL         Test type of time is valid                   
         LHI   R0,L'DV_TTALL                                                    
                                                                                
SDFO08   CLC   TL_TTYP,0(R1)                                                    
         JE    SDFO10                                                           
         AHI   R1,1                                                             
         JCT   R0,SDFO08                                                        
         CLI   DV_TTALL+L'TL_TTYP,C' '                                          
         JH    SDFO08A             Only 1 type of time allowed?                 
         CLI   DV_TTALL,C' '                                                    
         JH    SDFO09              Then make that the default                   
                                                                                
SDFO08A  MVC   XERRTXT,SPACES      Wrong type of time specified                 
         LHI   R0,TL#TTYP                                                       
         LHI   RF,AE$INTTM                                                      
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     SETDFOL                                                          
*                                                                               
SDFO09   MVC   TL_TTYP,DV_TTALL                                                 
*                                                                               
SDFO10   CLI   DV_1RFPT,YESQ       Is the user forced to enter product          
         JE    SDFO12              Yes                                          
         CLI   TL_TTYP,TL_TBILQ    If billable should have product              
         JE    SDFO12                                                           
*&&UK                                                                           
         CLI   TL_TTYP,TL_TRELQ    If relisation time should have prod          
         JE    SDFO12                                                           
*&&                                                                             
         CLI   DV_FPT,C' '         Any profile entered                          
         JNH   SDFO16                                                           
         CLI   DV_FPT,C'X'         Prod to be excluded                          
         JE    SDFO14                                                           
         CLI   DV_FPT,C'A'         Prod required for all types of time          
         JE    SDFO12                                                           
         CLC   DV_FPT,TL_TTYP      Required for this type?                      
         JNE   SDFO16                                                           
                                                                                
SDFO12   OC    DV_SJPRO,DV_SJPRO   Test product given                           
         JNZ   SDFO16                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#TTYP                                                       
         LHI   RF,AE$MSPRD                                                      
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     SETDFOL                                                          
                                                                                
SDFO14   OC    DV_SJPRO,DV_SJPRO   Test product given                           
         JZ    SDFO16                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#PRO                                                        
         LHI   RF,AE$PRNAL                                                      
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     SETDFOL                                                          
                                                                                
SDFO16   CLI   TL_TTYP,TL_TBILQ    If billable should have job                  
         JE    SDFO18                                                           
         CLI   DV_CONJB,YESQ       Is the user not allowed to enter job         
         JE    SDFO20              Yes                                          
         CLI   DV_1RFJT,YESQ       Is the user forced to enter job              
         JE    SDFO18              Yes                                          
*&&UK                                                                           
         CLI   TL_TTYP,TL_TRELQ    If relisation time should have job           
         JE    SDFO18                                                           
*&&                                                                             
         CLI   DV_FJT,C' '         Any profile entered                          
         JNH   SDFO28                                                           
         CLI   DV_FJT,C'X'         Job not required for all types               
         JE    SDFO20                                                           
         CLI   DV_FJT,C'A'         Job required for all types of time           
         JE    SDFO18                                                           
         CLC   DV_FJT,TL_TTYP      Required for this type?                      
         JNE   SDFO28                                                           
                                                                                
SDFO18   OC    DV_SJJOB,DV_SJJOB   Test job given                               
         JNZ   SDFO19                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#TTYP                                                       
         LHI   RF,AE$MSJOB                                                      
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     SETDFOL                                                          
*                                                                               
SDFO19   CLI   DV_CONJB,YESQ       Is the user not allowed to enter job         
         JNE   SDFO24                                                           
SDFO20   OC    DV_SJJOB,DV_SJJOB   Test job given                               
         JZ    SDFO28                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#JOB                                                        
         LHI   RF,AE$JBNAL                                                      
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     SETDFOL                                                          
                                                                                
SDFO24   OC    TL_WCD,TL_WCD       Any workcode                                 
         JNZ   SDFO28                                                           
         LHI   RF,AE$INWRK         Should have one present if                   
         CLI   TL_TTYP,TL_TBILQ              job is quoted                      
         JNE   SDFO26                                                           
         LHI   RF,AE$WCNIB                                                      
                                                                                
SDFO26   MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#TTYP                                                       
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     SETDFOL                                                          
                                                                                
SDFO28   DS    0H                                                               
*&&UK                                                                           
         CLI   DV_COIOS,COSUSP     Suspense account override?                   
         JNE   SDFO30                                                           
         MVC   DV_INULA(L'ACTKUNT+L'ACTKLDG),LEDGERSK                           
                                                                                
SDFO30   CLI   DV_COIOS,COINCOME   Income account override?                     
         JNE   SDFO32                                                           
         MVC   DV_INULA(L'ACTKUNT+L'ACTKLDG),LEDGERSI                           
*&&                                                                             
SDFO32   CLI   DV_1RFJT,YESQ       Force job input                              
*&&US*&& JE    SDFO34              Yes - then ignore opt maint                  
*&&UK*&& JE    SDFO36                                                           
         CLI   DV_TNOJB,NOQ        Is job input allowed                         
*&&US*&& JNE   SDFO34              Yes                                          
*&&UK*&& JNE   SDFO36                                                           
         OC    DV_SJJOB,DV_SJJOB   No - test job given                          
*&&US*&& JZ    SDFO34                                                           
*&&UK*&& JZ    SDFO36                                                           
         MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#TTYP                                                       
         LHI   RF,AE$JBNAL                                                      
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     SETDFOL                                                          
*&&US                                                                           
SDFO34   CLC   DV_INULA,SPACES     Return income account or SKIPS               
         JH    SDFO36                                                           
         CLI   TL_TTYP,TL_TBILQ    Is it billable or materials?                 
         JNE   SDFO36                                                           
         MVC   DV_INULA,SPACES                                                  
         MVC   DV_INULA(L'LEDGERSK),LEDGERSK                                    
         MVC   DV_INULA+L'LEDGERSK(L'AC_IPS),AC_IPS                             
*&&                                                                             
SDFO36   CLI   TL_TTYP,TL_TBILQ    Is it billable or materials?                 
         JNE   SDFO90              No - we won't be posting to SI/SK            
                                                                                
SDFO38   MVI   BYTE1,0                                                          
*&&UK                                                                           
         USING LDGRECD,R2                                                       
         LA    R2,IOKEY            Read the ledger record                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUXCPY                                                   
         MVC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),DV_INULA                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    SDFO50                                                           
         TM    CPXSTATA,CPXLACAC                                                
         JZ    SDFO50                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    SDFO40                                                           
         LHI   RF,AE$INACC                                                      
         J     SETDFOH                                                          
                                                                                
SDFO40   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING LDGELD,R3                                                        
         L     R2,AIO3                                                          
         LA    R3,LDGRFST                                                       
         MVI   BYTE1,0                                                          
                                                                                
SDFO42   CLI   LDGEL,0                                                          
         JE    SDFO50                                                           
         CLI   LDGEL,LDGELQ                                                     
         JE    SDFO46                                                           
                                                                                
SDFO44   LLC   R1,LDGLN                                                         
         AR    R3,R1                                                            
         J     SDFO42                                                           
                                                                                
SDFO46   MVC   BYTE1,LDGOPOS                                                    
         DROP  R3                                                               
                                                                                
*&&                                                                             
         USING ACTRECD,R2                                                       
SDFO50   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,DV_INULA                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    SDFO52                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_INULA),DV_INULA                                     
         LHI   RF,AE$INACC                                                      
         J     SETDFOH2                                                         
                                                                                
SDFO52   TM    ACTKSTAT,ACTSLOCK                                                
         JZ    SDFO54                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_INULA),DV_INULA                                     
         LHI   RF,AE$ACTLK                                                      
         J     SETDFOH2                                                         
                                                                                
SDFO54   TM    ACTKSTAT,ACTSABLP                                                
         JNZ   SDFO56                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_INULA),DV_INULA                                     
         LHI   RF,AE$INACP                                                      
         J     SETDFOH2                                                         
                                                                                
SDFO56   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING RSTELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
         MVI   DV_INCST,0                                                       
                                                                                
SDFO58   CLI   RSTEL,0                                                          
         JE    SDFO70                                                           
         CLI   RSTEL,RSTELQ                                                     
         JE    SDFO62                                                           
         CLI   RSTEL,SPAELQ                                                     
         JE    SDFO66                                                           
                                                                                
SDFO60   LLC   R1,RSTLN                                                         
         AR    R3,R1                                                            
         J     SDFO58                                                           
                                                                                
SDFO62   CLI   CUAUTH+1,0          Allow if zero                                
         JE    SDFO64                                                           
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   SDFO64              or if it fits                                
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_INULA),DV_INULA                                     
         LHI   RF,AE$SECLK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         LHI   R0,TL#TTYP                                                       
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         OI    TWAMODE,TWAMERP                                                  
         MVI   LOCALERR,YESQ                                                    
                                                                                
SDFO64   MVC   DV_INCST,RSTCOSTG                                                
         J     SDFO60                                                           
                                                                                
         USING SPAELD,R3                                                        
SDFO66   CLI   SPATYPE,SPATANAL    Test analysis account                        
         JNE   SDFO60                                                           
         MVC   DV_INANA,SPAAANAL   Set analysis code                            
         J     SDFO60                                                           
         DROP  R3                                                               
                                                                                
SDFO70   DS    0H                                                               
*&&UK                                                                           
         CLI   CUACCS,0            Test any user limit access                   
         JE    SDFO72                                                           
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         TM    OFFACST4,CPYSOFF2                                                
         JZ    SDFO72                                                           
         TM    OFFAXSTA,CPXLACAC   Test using Limit Account access              
         JZ    SDFO72                                                           
         MVC   OFFAOPOS,BYTE1                                                   
         L     RF,IOADDR                                                        
         ST    RF,OFFAREC          A(low-lvl a/c)                               
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         DROP  R1                                                               
         JE    SDFO72                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_INULA),DV_INULA                                     
         LHI   RF,AE$SECLK                                                      
         J     SETDFOH2                                                         
*&&                                                                             
SDFO72   CLC   LEDGERSI,DV_INULA   Test income ledger                           
         JNE   SDFO90                                                           
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY            Build key of 12 account record               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'LEDGER12),LEDGER12                                     
         MVC   ACTKACT(L'DV_INCST),DV_INCST                                     
         CLC   DV_INANA,SPACES     Test analysis code present                   
         JNH   SDFO74                                                           
         MVC   ACTKACT(L'DV_INANA),DV_INANA                                     
                                                                                
SDFO74   CLI   ACTKACT,SPACEQ      Test valid costing/analysis                  
         JNH   *+2                                                              
         MVC   DV_12ULA,ACTKULA                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    SDFO76                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_12ULA),DV_12ULA                                     
         LHI   RF,AE$INACC                                                      
         J     SETDFOH2                                                         
                                                                                
SDFO76   TM    ACTKSTAT,ACTSLOCK                                                
         JZ    SDFO78                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_12ULA),DV_12ULA                                     
         LHI   RF,AE$ACTLK                                                      
         J     SETDFOH2                                                         
                                                                                
SDFO78   TM    ACTKSTAT,ACTSABLP                                                
         JNZ   SDFO80                                                           
         MVC   XERRTXT,SPACES                                                   
         MVC   XERRTXT(L'DV_12ULA),DV_12ULA                                     
         LHI   RF,AE$INACP                                                      
         J     SETDFOH2                                                         
                                                                                
SDFO80   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING NAMELD,R3                                                        
         L     R2,AIO1                                                          
         LA    R3,ACTRFST                                                       
                                                                                
SDFO82   CLI   NAMEL,0                                                          
         JE    SDFO90                                                           
         CLI   NAMEL,NAMELQ                                                     
         JE    SDFO86                                                           
                                                                                
SDFO84   LLC   R1,NAMLN                                                         
         AR    R3,R1                                                            
         J     SDFO82                                                           
                                                                                
SDFO86   LLC   R1,NAMLN                                                         
         SHI   R1,1+NAMLN1Q                                                     
         MVC   DV_12NAM(0),NAMEREC                                              
         EXRL  R1,*-6                                                           
         J     SDFO84                                                           
         DROP  R2                                                               
                                                                                
SDFO90   CLI   LOCALERR,YESQ       Any buffered error?                          
         JE    SETDFOL                                                          
                                                                                
SETDFOY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
SETDFOL  DS    0H                                                               
         J     EXITL                                                            
                                                                                
SETDFOH  MVC   XERRTXT,SPACES                                                   
SETDFOH2 LHI   R0,TL#CLI                                                        
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
                                                                                
***********************************************************************         
* Get DV_TIME# via AUDRECD (ACBRA27.GETAUD)                           *         
***********************************************************************         
                                                                                
GETNUM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GETNUM*'                                                      
                                                                                
         USING AUDRECD,IOKEY                                                    
         LA    R2,IOKEY            read all audits for the time sheet           
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVI   AUDKAUDT,AUDKTIME                                                
         MVC   AUDKCPY,CUXCPY                                                   
         XR    RE,RE                                                            
         ICM   RE,B'0111',DV_LEDAT                                              
         LNR   RE,RE                                                            
         STCM  RE,B'0111',AUDKPEDT                                              
         MVC   AUDKPIDB,DV_PIN                                                  
         MVC   CSVKEY2,AUDKEY                                                   
         LHI   RE,1                                                             
         STCM  RE,B'0011',DV_INDEX                                              
         MVI   BYTE1,NOQ                                                        
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         J     GETN04                                                           
                                                                                
GETN02   LA    R2,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
                                                                                
GETN04   CLC   CSVKEY2(AUDKSEQ-AUDKEY),AUDKEY                                   
         JNE   GETN10              end of Audits                                
                                                                                
         MVI   BYTE1,YESQ                                                       
         XR    RE,RE                                                            
         ICM   RE,B'0011',AUDKINDX                                              
         AHI   RE,1                                                             
         STCM  RE,B'0011',DV_INDEX                                              
         MVC   CSVKEY2,AUDKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   *+2                                                              
                                                                                
         USING AUDRECD,R2                                                       
         L     R2,AIO1                                                          
         USING STCELD,R3                                                        
         LA    R3,AUDRFST          loop through STCELs                          
                                                                                
GETN06   CLI   STCEL,0                                                          
         JE    GETN02                                                           
         CLI   STCEL,STCELQ                                                     
         JNE   GETN08                                                           
         CLI   STCIND,STCITIME                                                  
         JNE   GETN08                                                           
         XR    RF,RF                                                            
         ICM   RF,B'0011',STCTROW                                               
         CH    RF,DV_TIME#                                                      
         JNH   GETN08                                                           
         STH   RF,DV_TIME#         save highest row number                      
                                                                                
GETN08   LLC   R1,STCLN                                                         
         AR    R3,R1                                                            
         J     GETN06                                                           
                                                                                
GETN10   CLI   BYTE1,NOQ                                                        
         JNE   GETN12                                                           
         OI    DV_AUSTA,DV_AUSAQ   if no Audit found need to add                
                                                                                
GETN12   L     R2,AIO1             check latest for enough space                
         MVC   DV_AKEY,CSVKEY2                                                  
                                                                                
         GOTOR BLDSTC,ELEMNT2      Length in HALF1                              
                                                                                
         XR    RE,RE                                                            
         ICM   RE,B'0011',AUDRLEN                                               
         AH    RE,HALF1                                                         
         CHI   RE,AUDMAXLN                                                      
         JNH   GETN14                                                           
         LLC   R1,DV_AKEY+AUDKSEQ-AUDRECD                                       
         CHI   R1,FFQ                                                           
         JE    *+2                 die if too many|                             
         AHI   R1,1                                                             
         STC   R1,DV_AKEY+AUDKSEQ-AUDRECD                                       
         OI    DV_AUSTA,DV_AUSAQ                                                
                                                                                
GETN14   DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* Build STC element and retunr length in HALF1                        *         
***********************************************************************         
                                                                                
BLDSTC   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDSTC*'                                                      
                                                                                
         XC    HALF1,HALF1                                                      
                                                                                
         USING STCELD,R3                                                        
         LR    R3,R1                                                            
         XC    STCELD(FFQ),STCELD                                               
         MVI   STCEL,STCELQ                                                     
         MVI   STCLN,STCLN3Q                                                    
         MVI   STCTTYP,STCTRWAM                                                 
         TM    DV_TSSTA,DV_TSCLQ                                                
         JNZ   BLDSTC2                                                          
         MVI   STCTTYP,STCTRWAD                                                 
                                                                                
BLDSTC2  MVI   STCIND,STCITIME                                                  
         MVC   STCTDTE,DV_TODAY                                                 
         MVC   STCTUSR,LP_USRID                                                 
         MVC   STCTTIM,CURTIME                                                  
         MVC   STCTPID,DV_PIN                                                   
         MVI   STCTSTA3,STCTWIDG   Set this comes from the widget               
         TM    LP_FLAG2,LP_FMQIM   If MQ then just check don't lock             
         JZ    BLDSTC4                                                          
         MVI   STCTSTA3,STCTAAPI   Set this comes from the API                  
                                                                                
BLDSTC4  MVC   STCTMOA,DV_MOA                                                   
         MVC   STCTROW,DV_TIME#                                                 
         MVC   STCTCULA,DV_ULA                                                  
         MVC   STCTCTSK,TL_WCD                                                  
         MVC   STCTCORD,SPACES                                                  
         MVC   STCTEST#,SPACES                                                  
         MVC   STCTCINT,SPACES                                                  
         MVC   STCTCTTY,DV_TTYP                                                 
         ZAP   STCTCHRS,TL_HOURS                                                
         XR    RF,RF                                                            
         ICM   RF,B'0001',DV_NARRL                                              
         JZ    BLDSTC8                                                          
         TM    DV_TSSTA,DV_TSCLQ   For change set text changed                  
         JZ    *+8                                                              
         OI    STCTSTAT,STCTTEXT                                                
         SHI   RF,1                                                             
         CHI   RF,L'STCTCNAR-1     For audit we just capture first              
         JNH   BLDSTC6             60 chars                                     
         LHI   RF,L'STCTCNAR-1                                                  
                                                                                
BLDSTC6  BASR  RE,0                                                             
         MVC   STCTCNAR(0),TL_NARR                                              
         EX    RF,0(RE)                                                         
         AHI   RF,1                                                             
                                                                                
BLDSTC8  LA    R2,STCTCNAR                                                      
         AR    RF,R2                                                            
         SR    RF,R3                                                            
         STC   RF,STCLN                                                         
                                                                                
         LH    R1,HALF1                                                         
         AR    R1,RF                                                            
         AHI   R1,TIMALNQ                                                       
         TM    DV_TSSTA,DV_TSSWQ   Did we find Person/Week/ODS                  
         JNZ   BLDSTC10                                                         
         AHI   R1,TIMALN1Q                                                      
                                                                                
BLDSTC10 STH   R1,HALF1                                                         
         AR    R3,RF                                                            
         USING TIMELD,R3             element                                    
         XC    TIMEL(TIMALN1Q+1),TIMEL                                          
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMALNQ                                                    
         MVI   TIMETYP,TIMEARIN                                                 
         MVC   TIMAIDNO,DV_TIME#                                                
         MVC   TIMAPULA,DV_ULA      SJ account                                  
         MVC   TIMAPIDB,DV_CLIAP    Client manager approver PIN                 
                                                                                
         TM    DV_TSSTA,DV_TSSWQ   Did we find Person/Week/ODS                  
         JNZ   BLDSTC12                                                         
         LA    R3,TIMALNQ(R3)      No - add line manager timel                  
         XC    TIMEL(TIMALN1Q+1),TIMEL                                          
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMLN,TIMALN1Q                                                   
         MVI   TIMETYP,TIMEARIN                                                 
         MVC   TIMAPULA,DV_1RULA    1R account                                  
         MVC   TIMAPIDB,DV_MANAP    Line manager approver PIN                   
         MVC   TIMAPSDT,DV_LSDAT    Period/location start date                  
         MVC   TIMAPER#,DV_PERNO    Period number                               
                                                                                
BLDSTC12 J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Build day/hours array                                               *         
***********************************************************************         
                                                                                
BLDARY   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDARY*'                                                      
                                                                                
         MVC   TEMP,SPACES         determine number of days                     
*&&UK*&& GOTO1 VDATCON,DMCB,(1,DV_PSDAT),(13,DUB)                               
*&&US*&& GOTO1 VDATCON,DMCB,(1,DV_PSDAT),(10,DUB)                               
         MVC   TEMP+0(2),DUB+0                                                  
         MVC   TEMP+2(2),DUB+3                                                  
         MVC   TEMP+4(2),DUB+6                                                  
         MVI   TEMP+6,C'-'                                                      
*&&UK*&& GOTO1 VDATCON,DMCB,(1,DV_DATE),(13,DUB)                                
*&&US*&& GOTO1 VDATCON,DMCB,(1,DV_DATE),(10,DUB)                                
         MVC   TEMP+7(2),DUB+0                                                  
         MVC   TEMP+9(2),DUB+3                                                  
         MVC   TEMP+11(2),DUB+6                                                 
                                                                                
         USING PERVALD,R2                                                       
         LA    R2,TEMP2                                                         
         GOTO1 VPERVAL,DMCB,(13,TEMP),(0,0(R2))                                 
         MVC   DV_DOTSP,PVALNDYS+1                                              
         DROP  R2                                                               
                                                                                
*&&UK*&& GOTO1 VDATCON,DMCB,(1,DV_LEDAT),(13,DUB)                               
*&&US*&& GOTO1 VDATCON,DMCB,(1,DV_LEDAT),(10,DUB)                               
         MVC   TEMP+7(2),DUB+0                                                  
         MVC   TEMP+9(2),DUB+3                                                  
         MVC   TEMP+11(2),DUB+6                                                 
                                                                                
         USING PERVALD,R2                                                       
         LA    R2,TEMP2                                                         
         GOTO1 VPERVAL,DMCB,(13,TEMP),(0,0(R2))                                 
         XR    R4,R4                                                            
         ICM   R4,B'0011',PVALNDYS                                              
         DROP  R2                                                               
                                                                                
         CHI   R4,1                integrity checks                             
         JL    *+2                                                              
         CHI   R4,TT_DAYS                                                       
         JH    *+2                                                              
                                                                                
         LA    R2,DV_DHVAL         loop to build & seed date & hours            
         XR    R3,R3                                                            
         GOTO1 VDATCON,DMCB,(1,DV_PSDAT),(0,TEMP+0)                             
         MVC   TEMP+12(3),DV_PSDAT                                              
         J     BLDARY4                                                          
                                                                                
BLDARY2  GOTOR VADDAY,DMCB,(C'D',TEMP+0),TEMP+6,(R3)                            
         GOTO1 VDATCON,DMCB,(0,TEMP+6),(1,TEMP+12)                              
                                                                                
BLDARY4  MVC   0(L'TIMETDT1,R2),TEMP+12                                         
         ZAP   L'TIMETDT1(L'TIMEHRS1,R2),PZERO                                  
         CLC   DV_DATE,TEMP+12                                                  
         JNE   BLDARY6                                                          
         ZAP   L'TIMETDT1(L'TIMEHRS1,R2),TL_HOURS                               
                                                                                
BLDARY6  AHI   R3,1                                                             
         AHI   R2,L'TIMETDT1+L'TIMEHRS1                                         
         JCT   R4,BLDARY2                                                       
                                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Build time cluster at Param 1 and set DV_DLEN to its length         *         
* This doesn't set TIMSEQ                                             *         
***********************************************************************         
                                                                                
BLDCLS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDCLS*'                                                      
                                                                                
         LR    R4,R1                                                            
                                                                                
         USING TIMELD,R2           R2=A(time cluster in buffer record)          
         LR    R2,R4                                                            
                                                                                
         XC    TIMEL(TIMILN1Q),TIMEL                                            
         MVI   TIMEL,TIMELQ        Build TIMEINP element                        
         MVI   TIMETYP,TIMEINP                                                  
         MVC   TIMACC,DV_ULA                                                    
         MVC   TIMTSK,TL_WCD                                                    
         MVC   TIMOFF,DV_OFF                                                    
         MVC   TIMMED,DV_MED                                                    
         MVC   TIMTTYP,DV_TTYP                                                  
         MVC   TIMIND,DV_IND                                                    
         MVC   TIMMOA,DV_MOA                                                    
         MVC   TIMSTAT,DV_STAT                                                  
         MVC   TIMLINE#,DV_TIME#                                                
         MVC   TIMADAT,DV_TODAY                                                 
         ZAP   TIMHRS,TL_HOURS                                                  
         LHI   R0,TIMILN1Q                                                      
         CLI   DV_TTYP,TIMTNC                                                   
         JE    BLDCLS02                                                         
         XC    TIMBID(TIMILN2Q-TIMILN1Q),TIMBID                                 
         MVC   TIMRATE,DV_RATE                                                  
         MVC   TIMRBSTA,DV_RBSTA                                                
         MVC   TIMREFF,DV_REFF                                                  
         MVC   TIMINC,DV_1NULA                                                  
*&&UK*&& MVC   TIMCRATE,DV_CRATE                                                
*&&UK*&& MVC   TIMCREFF,DV_CREFF                                                
         MVC   TIMAMNT,DV_AMNT                                                  
         LHI   R0,TIMILN2Q                                                      
                                                                                
BLDCLS02 STC   R0,TIMLN                                                         
         AR    R2,R0                                                            
                                                                                
         XR    R1,R1               Build TIMENAR element                        
         ICM   R1,1,DV_NARRL                                                    
         JZ    BLDCLS04                                                         
         BCTR  R1,0                                                             
         XC    TIMEL(TIMNARR-TIMEL),TIMEL                                       
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMENAR                                                  
         BASR  RE,0                                                             
         MVC   TIMNARR(0),TL_NARR                                               
         EX    R1,0(RE)                                                         
         LA    R0,TIMNARR-TIMELD+1(R1)                                          
         STC   R0,TIMLN                                                         
         AR    R2,R0                                                            
                                                                                
BLDCLS04 XC    TIMEL(TIMEDLNQ),TIMEL                                            
         MVI   TIMEL,TIMELQ        Build TIMETIME element                       
         MVI   TIMETYP,TIMETIME                                                 
         MVC   TIMEIDNO,DV_TIME#                                                
         MVI   TIMEPST1,TIMEWIDG                                                
         MVC   TIMEPIDC,DV_CLIAP                                                
         MVC   TIMETPDT,DV_LEDAT                                                
         LA    RE,DV_DHVAL                                                      
         LA    R1,TIMEDAY                                                       
                                                                                
BLDCLS12 OC    0(L'TIMETDTE,RE),0(RE)  Is there a date in the buffer            
         JZ    BLDCLS14                                                         
         MVC   0(L'TIMEDAY,R1),0(RE)   Copy date and hours to element           
         AHI   R1,L'TIMEDAY            Bump along timel element                 
         AHI   RE,L'TIMEDAY            Bump along buffer record                 
         J     BLDCLS12                                                         
                                                                                
BLDCLS14 SR    R1,R2                                                            
         STC   R1,TIMLN                                                         
         AR    R2,R1               Add length of last element added             
         MVI   TIMEL,0             Ensure EoR                                   
         LR    R0,R4                                                            
         SR    R2,R0                                                            
         STH   R2,DV_DLEN          Set length of time cluster                   
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Do rates look up                                                    *         
***********************************************************************         
                                                                                
RATLUP   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*RATLUP*'                                                      
                                                                                
         MVI   LOCALERR,NOQ                                                     
         ZAP   DV_RATE,PZERO                                                    
*&&UK*&& ZAP   DV_CRATE,PZERO                                                   
         ZAP   DV_AMNT,PZERO                                                    
                                                                                
* no ACBRA1B.GETCAP call for approver look up yet                               
                                                                                
*&&UK                                                                           
         CLI   DV_TTYP,TIMTCN                                                   
         JE    RATL02                                                           
*&&                                                                             
         CLI   DV_TTYP,TIMTCR                                                   
         JE    RATL02                                                           
         CLI   DV_TTYP,TIMTCB                                                   
         JNE   RATLUPY                                                          
                                                                                
         USING X_RATED,R3                                                       
RATL02   L     R3,ABLOCK                                                        
         XC    0(X_RATEDQ,R3),0(R3)                                             
         MVC   X_RATCTRY,CUCTRY                                                 
         MVC   X_RAT1RACT,DV_1RULA+2                                            
         MVC   X_RAT1ROFF,DV_1ROFF Person office                                
         MVC   X_RAT1RDPT,DV_1RDEP                                              
         MVC   X_RAT1RSUB,DV_1RSUB                                              
         MVC   X_RAT1RPER,DV_1RPER                                              
         MVC   X_RATSJCPJ,DV_ULA+2                                              
         MVC   X_RATSJOFF,DV_SJCLO Client client office                         
         MVC   X_RATSJTSK,TL_WCD                                                
         MVC   X_RATCRDTE,DV_PEDAT                                              
         MVC   X_RATCRDTE,DV_LEDAT                                              
         MVC   X_RDATAMGR,XDATAMGR                                              
         MVC   X_RCOMFACS,ACOMFACS                                              
         MVC   X_RCASHVAL,VCASHVAL                                              
         MVC   X_RAT1CMPY,CUXCPY                                                
*&&US                                                                           
         TM    DV_ICPJ,DV_EADJ     Job Eliglible for rate Adjustment            
         JZ    RATL04                                                           
         OI    X_RATSTAT,X_RATEADJ Get adjustment rates for US                  
*&&                                                                             
RATL04   DS    0H                                                               
*&&UK                                                                           
         CLI   DV_TTYP,TIMTCN                                                   
         JNE   RATL06                                                           
         OI    X_RATSTAT,X_RATSCOST      Get cost rate only                     
                                                                                
RATL06   MVC   X_RATCSTAT,CPYSTAT7                                              
         MVC   X_RATCSTA4,CPYSTAT4                                              
         MVI   X_RATCFLG,0               Currency flag                          
         MVC   X_RTOBACCO,VTOBACCO                                              
*&&                                                                             
         GOTOR VGETRTE,X_RATED                                                  
                                                                                
         CLI   DV_TTYP,TIMTCN                                                   
         JE    RATL08                                                           
         TM    X_RATSTAT2,X_RATEFND                                             
         JNZ   RATL08                                                           
         LHI   RF,AE$MRATE                                                      
         J     RATLUPH                                                          
                                                                                
RATL08   DS    0H                                                               
*&&UK                                                                           
         TM    X_RATSTAT2,X_RATEFNDC   was cost rate found                      
         JNZ   RATL12                                                           
         CLI   DV_TTYP,TIMTCN                                                   
         JNE   RATL10                                                           
         TM    X_RATSTAT2,X_RATEHIST   was history record found                 
         JNZ   RATL12                                                           
                                                                                
RATL10   LHI   RF,AE$00558                                                      
         J     RATLUPH                                                          
                                                                                
RATL12   CLI   DV_TTYP,TIMTCN                                                   
         JE    RATL20                                                           
*&&                                                                             
*&&US                                                                           
         TM    X_RATSTAT2,X_RATEWADJ   Was rate adjusted?                       
         JNO   RATL14                                                           
         OI    DV_RBSTA,TIMRBADJ                                                
*&&                                                                             
RATL14   ZAP   DV_RATE,X_RATEAMNT  Set billing rate                             
         MVC   DV_REFF,X_RATEEFFD  Set rate effective date                      
*&&UK*&& ZAP   DV_CRATE,X_RATEAMTC Set cost rate                                
*&&UK*&& MVC   DV_CREFF,X_RATEEFFC Set cost rate effective date                 
         ZAP   FULL2,TL_HOURS                                                   
         ZAP   DUB,X_RATEAMNT                                                   
         MP    DUB,FULL2                                                        
         SRP   DUB,64-2,5                                                       
         ZAP   DV_AMNT,DUB         Set billing amount                           
         DROP  R3                                                               
                                                                                
RATL20   CLI   LOCALERR,YESQ       Any buffered error?                          
         JE    RATLUPL                                                          
                                                                                
RATLUPY  DS    0H                                                               
         J     EXITY                                                            
                                                                                
RATLUPL  DS    0H                                                               
         J     EXITL                                                            
                                                                                
RATLUPH  MVC   XERRTXT,SPACES                                                   
         LHI   R0,TL#GLOB                                                       
         STCM  RF,B'0011',ROUERRV                                               
         GOTOR SAVERR,DMCB,(RF),0,(R0)                                          
         J     EXITH                                                            
                                                                                
***********************************************************************         
* Lock the PID for the duration of the upload.  Exit with CC=equal if *         
* locked this time or previously by me.  Exit with CC=not equal if    *         
* there is a lock failure                                             *         
***********************************************************************         
                                                                                
DOLOCK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DOLOCK*'                                                      
                                                                                
         USING LLKKEYD,WORK                                                     
                                                                                
         LA    R2,PIDLOCK          Point to PID lock table                      
         XR    R1,R1                                                            
                                                                                
DOLOCK02 OC    0(L'PIDLOCK,R2),0(R2)                                            
         JZ    DOLOCK04            Add new entry if not already locked          
         CLC   DV_PIN(DV_PDLQ),0(R2)                                            
         JE    DOLOCKY                                                          
         AHI   R2,L'PIDLOCK        Bump to next entry                           
         AHI   R1,1                                                             
         J     DOLOCK02                                                         
                                                                                
DOLOCK04 MVC   0(L'PIDLOCK,R2),DV_PIN                                           
         CHI   R1,PIDMAXQ          Check we haven't exceeded table              
         JNH   DOLOCK06            No - fine                                    
         LHI   RF,AE$TMIOU         Error too many items for update              
         STCM  RF,B'0011',ROUERRV                                               
         J     EXITN                                                            
                                                                                
DOLOCK06 GOTOR BLDLOK              Build LOCKET key                             
                                                                                
         GOTOR LOCKET,DMCB,('LLKTESTQ',LLKKEYD),ACOMFACS                        
         JE    DOLOCK08                                                         
         XC    0(L'PIDLOCK,R2),0(R2)                                            
         LHI   RF,AE$P#LOK                                                      
         STCM  RF,B'0011',ROUERRV                                               
         J     EXITN                                                            
                                                                                
DOLOCK08 TM    LP_FLAG2,LP_FMQIM   If MQ then just check don't lock             
         JNZ   DOLOCK12                                                         
                                                                                
DOLOCK10 GOTOR LOCKET,DMCB,('LLKLOCKQ',LLKKEYD),ACOMFACS                        
         JNE   *+2                                                              
         J     DOLOCKY                                                          
                                                                                
DOLOCK12 DS    0H                                                               
                                                                                
DOLOCKY  DS    0H                                                               
         GOTOR TRACEIT,LOCKQ                                                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Lock the PID for the duration of the upload.  Exit with CC=equal if *         
* locked this time or previously by me.  Exit with CC=not equal if    *         
* there is a lock failure                                             *         
***********************************************************************         
                                                                                
UNLOCK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UNLOCK*'                                                      
                                                                                
         CLC   TL_PCC,TH4RMOQ      *** special TH4RMOQ mode ***                 
         JE    ULOCK02             *** special TH4RMOQ mode ***                 
         TM    LP_FLAG2,LP_FMQIM   If MQ then skip as no locking done           
         JNZ   EXITY                                                            
                                                                                
ULOCK02  LA    R2,PIDLOCK          Unlock everything I locked so far            
         GOTOR TRACEIT,ULOKQ                                                    
                                                                                
ULOCK04  OC    0(L'PIDLOCK,R2),PIDLOCK                                          
         JZ    EXITY                                                            
         GOTOR BLDLOK,(R2)         Build LOCKET key                             
         LHI   R0,LLKUNLKQ         On-line unlock action                        
         TM    LP_FLAG,LP_FOFFL                                                 
         JZ    ULOCK06                                                          
         LHI   R0,LLKUNGLQ         Off-line - running as global                 
                                                                                
ULOCK06  GOTOR LOCKET,DMCB,((R0),LLKKEYD),ACOMFACS                              
         JNE   *+2                 Failure to unlock                            
                                                                                
         XC    0(L'PIDLOCK,R2),0(R2)                                            
         AHI   R2,L'PIDLOCK                                                     
         J     ULOCK04                                                          
                                                                                
***********************************************************************         
* unlock record to recovery file                                      *         
***********************************************************************         
                                                                                
UPDTSN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDTSN*'                                                      
                                                                                
         TM    LP_FLAG2,LP_FMQIM   If MQ then just skip                         
         JNZ   EXITY                                                            
                                                                                
UPDTSN2  GOTOR BLDLOK              Build LOCKET key in WORK                     
         GOTOR DMGRITRN,DMCB,$LOKREC,WORK                                       
         GOTOR TRACEIT,UPDTQ                                                    
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Build a LOCKET key                                                  *         
***********************************************************************         
                                                                                
BLDLOK   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BLDLOK*'                                                      
                                                                                
         L     R5,LLP                                                           
         MVC   LLOCKSE,LP_SENO                                                  
         MVC   LLOCKAGY,LP_AGY                                                  
         MVC   LLOCKRTY,=C'P#'                                                  
         MVC   LLOCKKEY,SPACES                                                  
         GOTOR VDATCON,DMCB,(1,DV_LEDAT),(0,LLOCKKEY+2*L'DV_PIN)                
         GOTOR VHEXOUT,DMCB,DV_PIN,LLOCKKEY,L'DV_PIN,0                          
         OC    LLOCKKEY,SPACES                                                  
                                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Save error messages in an area so we can send all errors together   *         
***********************************************************************         
                                                                                
SAVERR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SAVERR*'                                                      
                                                                                
         L     RF,ANXTERR          Address of next error in table               
         LTR   RF,RF                                                            
         JZ    SAVERR02                                                         
         LAY   RE,ERRTAB                                                        
         CLI   0(RE),ET_EOTQ       If table is empty                            
         JNE   SAVERR04                                                         
                                                                                
SAVERR02 LAY   RF,ERRTAB           Point to start of table                      
         XC    ALSTERR,ALSTERR                                                  
                                                                                
SAVERR04 LA    R3,XERRTXT                                                       
         XR    R0,R0                                                            
         CLC   XERRTXT,SPACES                                                   
         JNH   SAVERR10                                                         
         LA    RE,XERRTXT+L'XERRTXT-1                                           
         LHI   R0,L'XERRTXT                                                     
                                                                                
SAVERR06 CLI   0(RE),SPACEQ                                                     
         JH    SAVERR10                                                         
         SHI   RE,1                                                             
         JCT   R0,SAVERR06                                                      
         J     *+2                 (my bad SAVERR04 check ...)                  
                                                                                
         USING ET_D,RF                                                          
SAVERR10 AHI   R0,ET_LN1Q          R0=Length of new entry                       
         AR    R0,RF                                                            
         LAY   RE,ERRTAB+L'ERRTAB                                               
         CR    R0,RE                                                            
         JH    SAVERR30            No room left                                 
         SR    R0,RF                                                            
         STC   R0,0(RF)            Set length of new entry                      
         OI    TWAMODE,TWAMERP     Set we have validation error                 
                                                                                
         LM    R2,R4,0(R1)                                                      
         STCM  R2,B'0011',ET_ERRNO                                              
         STCM  R4,B'0011',ET_FLDNM                                              
         LA    R3,XERRTXT                                                       
         SHI   R0,ET_LN1Q                                                       
         LTR   R1,R0               Test any extra text to be added              
         JZ    SAVERR20            No                                           
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   ET_EXTRA(0),0(R3)   Move extra text into entry                   
         EX    R1,0(RE)                                                         
                                                                                
SAVERR20 ST    RF,ALSTERR          REMEMBER PREVIOUS ERROR SLOT                 
         LA    RF,ET_D+ET_LN1Q     Point to next error slot                     
         AR    RF,R0               Add length of extra text                     
         ST    RF,ANXTERR          Set A(Next entry to be added)                
         MVI   ET_D,ET_EOTQ        Set new end of table                         
         J     EXITY                                                            
                                                                                
SAVERR30 DS    0H                  Too many errors for DDLINKIO                 
         L     RF,ALSTERR          Truncate it                                  
         LA    R0,ET_LN1Q                                                       
         AR    R0,RF                                                            
         LAY   RE,ERRTAB+L'ERRTAB                                               
         CR    R0,RE                                                            
         JH    *+2                 Room left to warn user                       
         SR    R0,RF               table still too long                         
         STC   R0,0(RF)            Set length of new entry                      
         OI    TWAMODE,TWAMEDP     Give up now                                  
                                                                                
         LHI   R1,AE$TMERR                                                      
         STCM  R1,B'0011',ET_ERRNO                                              
         MVI   ET_LN1Q(RF),ET_EOTQ                                              
         J     EXITY                                                            
         DROP  RF                                                               
                                                                                
***********************************************************************         
* Trace request                                                       *         
***********************************************************************         
                                                                                
VALTIMQ  EQU   C'V'                                                             
UPDTIMQ  EQU   C'U'                                                             
SPECQ    EQU   C'S'                *** special THOMAS mode ***                  
LOCKQ    EQU   C'1'                                                             
UPDTQ    EQU   C'2'                                                             
ULOKQ    EQU   C'3'                                                             
                                                                                
TRACEIT  NTR1  BASE=*,LABEL=*      Trace in SYSPRINT (offline)                  
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JZ    TRACEITX                                                         
                                                                                
         STC   R1,BYTE1                                                         
                                                                                
         L     RF,ARUNFACS         Get A(RUNFACS)                               
         L     R3,RCPRINT-RUNFACSD(RF)                                          
         AHI   R3,P_P-P_DPRINT                                                  
         USING PTRACED,R3                                                       
                                                                                
         XR    R2,R2                                                            
         ICM   R2,3,LP_QMAPN                                                    
                                                                                
         LAY   RE,TATLUPL                                                       
         MVC   PTTWHO,0(RE)                                                     
         CHI   R2,A#TLUPL                                                       
         JNE   *+2                                                              
                                                                                
         MVC   PTMODE,TRUNREQ                                                   
         CLI   RUNMODE,RRUNREQQ                                                 
         JE    TRACEIT2                                                         
         MVC   PTMODE,TVALREQ                                                   
                                                                                
TRACEIT2 MVI   PTSLASH,SLASHQ                                                   
         MVC   PTFROM,BYTE1                                                     
                                                                                
         TM    LP_FLAG2,LP_FMQIM                                                
         JZ    TRACEIT4                                                         
         LAY   RE,TMQ                                                           
         MVC   PTMQ,0(RE)                                                       
                                                                                
TRACEIT4 CLI   BYTE1,LOCKQ                                                      
         JE    TRACEIT6                                                         
         CLI   BYTE1,UPDTQ                                                      
         JE    TRACEIT6                                                         
         CLI   BYTE1,ULOKQ                                                      
         JE    TRACEIT6                                                         
                                                                                
         LAY   RE,TTIMED                                                        
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
         XOUT  CONNPIN,PTUSER,2                                                 
                                                                                
         LAY   RE,TDETS                                                         
         MVC   PTFIELD(L'TDETS),0(RE)                                           
         LA    R4,PFIELD                                                        
                                                                                
         GOTOR VHEXOUT,DMCB,TL_PIN,0(R4),L'TL_PIN                               
         AHI   R4,2*L'TL_PIN+1                                                  
         MVC   0(L'TL_PCC,R4),TL_PCC                                            
         AHI   R4,L'TL_PCC+1                                                    
                                                                                
         GOTOR VDATCON,DMCB,(1,TL_DATE),(10,0(R4))                              
         AHI   R4,8+1                                                           
                                                                                
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
TRUNREQ  DC    C'RUNREQ'                                                        
TVALREQ  DC    C'VALREQ'                                                        
TMAIL    DC    C'Mail'                                                          
TERROR   DC    C'Error:'                                                        
TALPHAID DC    C'AlphaID:'                                                      
TUSERPID DC    C'PID:'                                                          
TATLUPL  DC    C'A#TLUPL'                                                       
TDETS    DC    C'Details:'                                                      
                                                                                
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
                                                                                
AAUDREC  EQU   AIO1,,C'A'          A(Time audit record)                         
ATRNREC  EQU   AIO2,,C'A'          A(Transaction record)                        
AAPPTAB  EQU   AIO4,,C'A'          A(APPTAB) - uses IO5 I06 IO7 also            
ACPRREC  EQU   AIO8,,C'A'          A(Costing personal rates record)             
ATIMREC  EQU   AIO8,,C'A'          A(Time interface record)                     
                                                                                
RETDATEL DC    AL1(LQ_RDATQ),AL2(LQ_LN1Q)                                       
                                                                                
SPACEQ   EQU   C' '                                                             
FFQ      EQU   X'FF'                                                            
LCSCQ    EQU   C'C'                                                             
SLASHQ   EQU   C'/'                                                             
UNIT1Q   EQU   C'1'                                                             
LEDGRQ   EQU   C'R'                                                             
HOURSQ   EQU   C'H'                                                             
RECLIM   EQU   03                                                               
ACTOVRD  EQU   09                                                               
                                                                                
TL#PIN   EQU   01                                                               
TL#PCC   EQU   02                                                               
TL#HRS   EQU   03                                                               
TL#DATE  EQU   04                                                               
TL#CLI   EQU   05                                                               
TL#PRO   EQU   06                                                               
TL#JOB   EQU   07                                                               
TL#WCD   EQU   08                                                               
TL#TTYP  EQU   09                                                               
TL#1NA   EQU   10                                                               
TL#NARR  EQU   11                                                               
TL#PID   EQU   12                                                               
TL#DV1   EQU   59                  DataVals map number 1                        
TL#DV2   EQU   60                  DataVals map number 2                        
TL#IND   EQU   61                  OK indicator                                 
TL#ERROR EQU   62                  Error code                                   
TL#ERRRN EQU   63                  Error row number                             
TL#GLOB  EQU   64                  Global error source                          
TL#PERNO EQU   64                  Period number                                
TL#PERST EQU   65                  Period start date                            
TL#PEREN EQU   66                  Period end date                              
                                                                                
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
                                                                                
PINLIT   DC    C'Person number (2X)'                                            
PCCLIT   DC    C'Person code Cost (8C)'                                         
PIDLIT   DC    C'Security PID (8C)'                                             
CLILIT   DC    C'Client code'                                                   
PROLIT   DC    C'Product code'                                                  
JOBLIT   DC    C'Job code'                                                      
WCDLIT   DC    C'Work code'                                                     
TTYPLIT  DC    C'Time type'                                                     
ONALIT   DC    C'1N account'                                                    
NARRLIT  DC    C'Narrative (200 max)'                                           
HRSLIT   DC    C'Hours (PL8 amount)'                                            
DATLIT   DC    C'Date for time add'                                             
DATALIT  DC    C'Data passed'                                                   
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
                                                                                
         LTORG                                                                  
                                                                                
SRUPD60  DC    C'T10D60  '         Update program phase name                    
SORTCARD DC    C'SORT FIELDS=(5,43,A),FORMAT=BI,WORK=1 '                        
SORTTYPE DC    C'RECORD TYPE=V,LENGTH=(2048,,,100,150) '                        
SORTGET  DC    C'GET'                                                           
SORTPUT  DC    C'PUT'                                                           
                                                                                
DMCOMMIT DC    C'COMMIT  '         Recovery checkpoint command                  
                                                                                
ACCOUNT  DC    C'ACC'              Account file prefix                          
                                                                                
DMWRTD   DC    C'DMW'              DMWRITE (directory)                          
DMADDD   DC    C'DMA'              DMADD   (directory)                          
DMPUTF   DC    C'PUT'              PUTREC  (file)                               
DMADDF   DC    C'ADD'              ADDREC  (file)                               
                                                                                
DMREAD   DC    C'DMREAD  '         DMREAD  (directory)                          
DMGETR   DC    C'GETREC  '         GETREC  (file)                               
                                                                                
$ADDTRN  DC    C'$ATR'             ADDTRN file prefix                           
$PLDREC  DC    C'$PLD'             PLDREC file prefix                           
$LOKREC  DC    C'$LOK'             LOKREC file prefix                           
                                                                                
ADD      DC    C'ADD     '         FACWRK add                                   
CLOSE    DC    C'CLOSE   '         FACWRK close                                 
INDEX    DC    C'INDEX   '         FACWRK index                                 
READ     DC    C'READ    '         FACWRK read                                  
                                                                                
WORKLEN  DC    AL2(WORKL)                                                       
                                                                                
MAXHOURS DC    P'2400'                                                          
EFFS     DC    X'FFFFFFFF'                                                      
PZERO    DC    P'0'                                                             
PNEGONE  DC    P'-1'                                                            
PONE     DC    P'1'                                                             
PFOUR    DC    P'4'                                                             
P100     DC    P'100'                                                           
P1MILL   DC    P'1000000'                                                       
ADDEND   DC    C'ADD=END'                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
WC_99    DC    CL2'99'                                                          
WC_ASTS  DC    CL2'**'                                                          
TIMEREF  DC    C'*TIME*'           Reference for time records                   
                                                                                
THOMASQ  DC    CL8'THOMAS'                                                      
TH4RMOQ  DC    CL8'TH4RMO'                                                      
                                                                                
SECPROG  DC    X'06FA'                                                          
                                                                                
LEDGER1R DC    C'1R'                                                            
LEDGER1N DC    C'1N'                                                            
LEDGERSK DC    C'SK'                                                            
LEDGERSI DC    C'SI'                                                            
LEDGER12 DC    C'12'                                                            
LEDGER14 DC    C'14'                                                            
AC_IPS   DC    C'IPS'                                                           
IS_JOB   DC    C'Job:'                                                          
                                                                                
FWHSTAMP DC    X'00',C'UO'          FACWRK header record stamp                  
                                                                                
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
         DC    AL2(A#TLUPL),AL1(RECTILQ)                                        
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
                                                                                
RECTILQ  EQU   1                   Time Line                                    
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
LLP      DC    A(0)                A(LP_D)                                      
                                                                                
*** General upload response data map numbers                                    
                                                                                
D#UPLERR EQU   255                                                              
D#UPLNOD EQU   254                                                              
                                                                                
***********************************************************************         
* Saved working storage                                               *         
***********************************************************************         
                                                                                
TIMMAXLN EQU   1950                Maximum length of time record                
AUDMAXLN EQU   1999                Maximum length of audit record               
CACMAXLN EQU   1999                Maximum length of c/a record                 
                                                                                
SAVED    DSECT                                                                  
                                                                                
***********************************************************************         
* Sacred values                                                       *         
***********************************************************************         
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER mode                           
                                                                                
RUNINDS  DS    X                   ** Run indicators **                         
RUNIPUTF EQU   X'80'               PUTOUT initialised                           
RUNIATRF EQU   X'40'               GOATRN first time flag                       
                                                                                
ANXTERR  DS    A                   A(Next error entry)                          
ALSTERR  DS    A                   A(Previous error entry in errtab)            
ASRUPD60 DS    A                   A(SRUPD60)                                   
ASORTER  DS    A                   A(SORTER)                                    
PROTOFF  DS    A                   A(PROTOFF)                                   
PROTON   DS    A                   A(PROTON)                                    
LOCKET   DS    A                   A(LOCKET)                                    
MASTC    DS    A                   A(MASTC)                                     
WRKBLKR  DS    A                   A(FACWRK WRKIO block)                        
APRINTER DS    A                                                                
ARUNFACS DS    A                                                                
DATAMGR  DS    A                   A(DATAMGR)                                   
IDATAMGR DS    A                   A(DMGRITRN)                                  
XDATAMGR DS    A                   A(DMGRXTRN)                                  
VSMTP    DS    A                   A(SMTP)                                      
                                                                                
AERRNOW  DS    A                                                                
                                                                                
SEMPEL   DS    XL(EMPLNQ)                                                       
SLOCEL   DS    XL(LOCLNQ)                                                       
                                                                                
IWMPOST  DS    XL1                                                              
AWMPOST  DS    AL3                                                              
WMPOSTCQ EQU   2                                                                
WMPOSTMQ EQU   10                                                               
                                                                                
TSARABUF DS    XL(TSPXTNL)         TSAR block for gaplist calls                 
TSARPASS DS    XL(TSPXTNL)         TSAR block for passive pointers              
TSARRECS DS    XL(TSPXTNL)         TSAR block for record buffer                 
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
TSAROLDT DS    XL(TSPXTNL)         TSAR block for old time buffer               
TSARNEWT DS    XL(TSPXTNL)         TSAR block for new time buffer               
TSARGENL DS    XL(TSPXTNL)         TSAR block for timeld buffer                 
TSAERR   DS    XL(L'TSERRS)        Transaction buffer error return              
GAPAREA  DS    XL(GAPTLNQ)         Area to use for buffer rec                   
ATSRERRS DS    XL1                 Error area for buffer                        
                                                                                
LOCALERR DS    CL1                 Local error Y/N                              
ERRCNT   DS    XL1                                                              
ERRMAX   EQU   25                  Maximum no. of error messages                
                                                                                
DSDICTL  DS    0C                                                               
         DSDDL PRINT=YES                                                        
DSDICTLX DS    0C                                                               
                                                                                
COMFACS  DS    XL(COMFACSL)        Local copy of COMFACS                        
                                                                                
SAVEVAR  DS    0F                  ** Variables follow **                       
                                                                                
AMYIO    DS    A                                                                
MYIOADR  DS    F                                                                
CONNPIN  DS    H                   Current connect PIN                          
CURTIME  DS    XL4                 Current time                                 
SVTODL   DS    CL8                                                              
                                                                                
PIDLOCK  DS    (PIDMAXQ)XL(DV_PDLQ)      PID lock table                         
PIDMAXQ  EQU   10                                                               
PIDLOCKL EQU   *-PIDLOCK                                                        
                                                                                
ELEMNT2  DS    XL300                                                            
                                                                                
***********************************************************************         
* Request values                                                      *         
***********************************************************************         
                                                                                
TL_VALS  DS    0F                                                               
                                                                                
TL_PIN   DS    CL2                 Security PIN                                 
TL_PCC   DS    CL8                 Person code from Cost                        
TL_DATE  DS    PL3                 Date of time added                           
TL_HOURS DS    PL8                 Hours                                        
TL_TTYP  DS    CL1                 Time type                                    
TL_CLI   DS    CL6                 Client                                       
TL_PRO   DS    CL6                 Product                                      
TL_JOB   DS    CL6                 Job                                          
TL_WCD   DS    CL2                 Work code                                    
TL_1NA   DS    CL12                1N account                                   
TL_PID   DS    CL8                 Security PID                                 
TL_NARR  DS    CL200               Narrative                                    
TL_AHED1 DS    AL4                                                              
TL_AHED2 DS    AL4                                                              
                                                                                
TL_DLNQ  EQU   *-TL_VALS                                                        
                                                                                
         DS    0H                  TL_TTYP:                                     
TL_TBILQ EQU   C'B'                - Billable time                              
TL_TNONQ EQU   C'N'                - Non-billable time                          
TL_TRELQ EQU   C'R'                - Realisation time                           
TL_TDEFQ EQU   C'D'                - Default time (resource management)         
                                                                                
**********************************************************************          
* Derived values                                                                
**********************************************************************          
                                                                                
DV_VALS  DS    0F                                                               
                                                                                
DV_VALS1 DS    0H                                                               
                                                                                
DV_DLEN  DS    H                                                                
DV_TIME# DS    H                                                                
DV_ITEM# DS    H                                                                
                                                                                
DV_INDIC DS    CL1                 Y/N for ok                                   
DV_SPIDC DS    CL8                                                              
DV_PIDC  DS    CL8                                                              
                                                                                
DV_PIN   DS    XL2                 Person ID number                             
DV_LEDAT DS    XL3                 Location end date                            
DV_LSDAT DS    XL3                 Location start date                          
DV_PDLQ  EQU   *-DV_PIN                                                         
DV_PSDAT DS    XL3                 Period start date                            
DV_PEDAT DS    XL3                 Period end date                              
                                                                                
DV_ERROR DS    XL2                 Return Error or blank                        
DV_TODAY DS    XL3                                                              
DV_TODF  DS    CL6                                                              
DV_TODC  DS    XL2                                                              
DV_SYSTM DS    CL3                 FACPAK SYSTEM                                
                                                                                
DV_DATE  DS    XL3                 (TL_DATE)                                    
                                                                                
DV_NARRL DS    XL1                                                              
DV_HIND  DS    X                   Header status (see FW_HDIND)                 
                                                                                
DV_1RULA DS    0CL(L'ACTKULA)                                                   
DV_1RUNT DS    CL(L'ACTKUNT)                                                    
DV_1RLDG DS    CL(L'ACTKLDG)                                                    
DV_1RACT DS    CL(L'ACTKACT)                                                    
                                                                                
* NEED TO GET THESE VALUS                                                       
DV_1RNAM DS    CL(L'NAMEREC)       Person 1R account name                       
DV_142NM DS    CL(L'NAMEREC)       Department name                              
DV_143NM DS    CL(L'NAMEREC)       Sub-Department name                          
                                                                                
DV_1ROFF DS    CL(L'LOCOFF)        Person office code                           
DV_1RDEP DS    CL(L'LOCDEPT)       Department code                              
DV_1RSUB DS    CL(L'LOCSUB)        Sub-department code                          
DV_1RPER DS    CL(L'PERKCODE)      Person code                                  
DV_1RCST DS    CL(L'RSTCOSTG)      Costing group                                
DV_1RFPT DS    CL1                 Force product time - Y/N                     
DV_1RFJT DS    CL1                 Force job time - Y/N                         
DV_CSTR  DS    XL3                 Calendar start date MOA                      
DV_CEND  DS    XL3                 Calendar end MOA                             
DV_PERNO DS    XL(L'TMPNUMB)       Period number                                
DV_MOA   DS    XL(L'TMPMTH)        MOA of time                                  
DV_TSSTO DS    XL(L'TIMKSTAT)      Previous time status                         
DV_TSSTN DS    XL(L'TIMKSTAT)      Current time status                          
DV_SUBLM DS    XL3                 Submitted date for line manager              
DV_SUBCL DS    XL3                 Submitted date for client approvers          
DV_1NULA DS    CL14                1N account                                   
DV_1CULA DS    CL14                Costing account                              
DV_SJOFF DS    CL2                 SJ office                                    
DV_SJCLO DS    CL2                 SJ original office                           
DV_SJCLI DS    CL(L'GOSELCLI)      Client code                                  
DV_SJPRO DS    CL(L'GOSELPRO)      Product code                                 
DV_SJJOB DS    CL(L'GOSELJOB)      Job code                                     
DV_SJULA DS    CL(L'ACTKULA)       SJ account in use                            
DV_ULA   DS    0CL(L'ACTKULA)      1N/SJ account in use                         
DV_UNT   DS    CL(L'ACTKUNT)       1N/SJ unit                                   
DV_LDG   DS    CL(L'ACTKLDG)       1N/SJ ledger                                 
DV_ACT   DS    CL(L'ACTKACT)       1N/SJ account                                
DV_ULACA DS    CL(L'ACTKULA)       1N/1C account in use                         
DV_COFF  DS    CL2                 Office code for contra account               
DV_OFF   DS    CL2                 Office code                                  
DV_MED   DS    CL1                 Media code                                   
DV_CAULA DS    CL(L'ACTKULA)       Contra account code                          
DV_CANAM DS    CL(L'NAMEREC)       Contra account                               
DV_SJNAM DS    CL(L'NAMEREC)       SJ account name                              
DV_1CNAM DS    CL(L'NAMEREC)       1C account name                              
DV_1NNAM DS    CL(L'NAMEREC)       1N account name                              
DV_12NAM DS    CL(L'NAMEREC)       12 account name                              
                                                                                
DV_RATE  DS    PL(L'TIMRATE)       Billing rate                                 
DV_RBSTA DS    PL(L'TIMRBSTA)      Billable time status                         
DV_REFF  DS    PL(L'TIMREFF)       Billing rate effective date                  
DV_IND   DS    XL(L'TIMIND)        Time indicator                               
DV_STAT  DS    XL(L'TIMSTAT)       Time status                                  
DV_TTYP  DS    CL(L'TIMTTYP)       Type of time                                 
DV_CLIAP DS    XL(L'TIMEPIDC)      Time approver                                
                                                                                
DV_RST7  DS    XL(L'RSTSTAT7)      Record status byte 7                         
DV_OVRDU DS    XL3                 Overdue date packed                          
DV_MANAP DS    XL(L'DPAPPIDB)      Manager approver PID                         
DV_MANBA DS    10XL(L'PIDNO)       Back-up manager approver PID                 
DV_BAMAX EQU   10                  10 Max Backup Approvers                      
                                                                                
DV_AMNT  DS    PL(L'TIMAMNT)       Billing amount                               
DV_DAYS  EQU   31                  Maximum number of time sheet days            
DV_DHVAL DS    XL((L'TIMETDT1+L'TIMEHRS1)*DV_DAYS)                              
DV_VAL1L EQU   *-DV_VALS1                                                       
                                                                                
DV_VALS2 DS    0H                                                               
                                                                                
DV_TOT   DS    CL(L'GOTOT)         Default type of time                         
DV_FPT   DS    CL(L'GOFPT)         Force product on time sheet                  
DV_FJT   DS    CL(L'GOFJT)         Force job on time sheet                      
DV_INACT DS    0CL(L'GOICA)        Income account                               
DV_INCPY DS    XL1                                                              
DV_INULA DS    CL(L'ACTKULA)                                                    
DV_TFNAR DS    CL(L'GOTFNARR)      Force narrative for time types               
DV_TNOJB DS    CL(L'GOTNOJOB)      Job allowed                                  
DV_TTALL DS    CL(L'GOTTALLW)      Type of time allowed                         
                                                                                
DV_COATM DS    XL(L'COATM)         Allowable time types                         
DV_CODTM DS    XL(L'CODTM)         Default time types                           
DV_COBRT DS    XL(L'COBRTE)        Post sales rate not cost B-Time              
DV_CORRT DS    XL(L'CORRTE)        Post sales rate not cost R-Time              
DV_COTUP DS    XL(L'COTUP)         When to update postings                      
DV_COACS DS    XL(L'COACS)         Time sheet approval option                   
DV_COFNR DS    XL(L'COFNR)         Force narrative on time sheet                
DV_COMAT DS    CL(L'COMAT)         Materials in use                             
DV_COIOS DS    CL(L'COTBTL)        Income or suspense for B-Time                
DV_CONJB DS    CL(L'CONJB)         No job input allowed - Y/N                   
DV_CONDA DS    PL(L'CONDA)         Number of days allowed in future             
DV_COFAP DS    CL(L'COFAP)         Use account setting for future time          
DV_COFTA DS    CL(L'COFTA)         Type of time allowed for future time         
                                                                                
DV_ICPJ  DS    X                   SJ/1N indicators                             
DV_ICLI  EQU   X'80'               - Client level found                         
DV_IPRO  EQU   X'40'               - Product level found                        
DV_IJOB  EQU   X'20'               - Job level found                            
DV_INON  EQU   X'10'               - Non-client level found                     
DV_IERR  EQU   X'08'               - Client/prod/job error                      
DV_EADJ  EQU   X'04'               - Job is eligible for adjustment             
DV_1NHOL EQU   X'02'               - 1N account is holiday (F2=T)               
                                                                                
DV_12ULA DS    CL(L'ACTKULA)       Costing revenue account                      
DV_INCST DS    CL(L'RSTCOSTG)      Costing group                                
DV_INANA DS    CL(L'SPAAANAL)      Income analysis account                      
                                                                                
DV_CDAT  DS    XL3                 MOA date                                     
DV_JSTA1 DS    XL1                 saved JOBSTA1                                
                                                                                
DV_DMOA  DS    0X                  Date/MOA entry                               
DV_DMDAT DS    XL3                 Date                                         
DV_DMMOA DS    XL2                 Month of activity                            
DV_HRS   DS    PL(L'TIMHRS)        Hours                                        
DV_EDHRS DS    PL(L'TIMEHRS1)      Daily edit hours                             
*&&UK                                                                           
DV_CRATE DS    PL(L'TIMCRATE)      Costing rate                                 
DV_CREFF DS    PL(L'TIMCREFF)      Costing rate effective date                  
*&&                                                                             
                                                                                
DV_ERROW DS    XL2                                                              
DV_ERMSG DS    CL80                                                             
                                                                                
DV_TSSTA DS    XL1                 Timesheet status                             
DV_TSSWQ EQU   X'80'               - TSWREC found for date/person               
DV_TSCAQ EQU   X'40'               - TSWREC found for contra account            
DV_TSCLQ EQU   X'20'               - Existing cluster found                     
DV_TSSPQ EQU   X'10'               - Spare for new cluster                      
DV_TSADQ EQU   X'08'               - Add record indicator                       
DV_TSRAQ EQU   X'04'               - Re-add deleted record                      
DV_TKEY  DS    XL(L'TIMKEY)        Saved TIMKEY for update                      
DV_TCSEQ DS    XL(L'TIMSEQ)        Saved cluster sequence number                
                                                                                
DV_AUSTA DS    XL1                 Audit status                                 
DV_AUSAQ EQU   X'80'               - Add audit record                           
DV_INDEX DS    XL2                 Timesheet index                              
DV_AKEY  DS    XL(L'AUDKEY)        Saved AUDKEY for update                      
DV_DOTSP DS    XL1                 Number of day in timesheet period            
                                                                                
DV_VAL2L EQU   *-DV_VALS2                                                       
                                                                                
DV_VALSL EQU   *-DV_VALS                                                        
                                                                                
***********************************************************************         
* General working storage values                                      *         
***********************************************************************         
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      RECORD TYPE                                  
                                                                                
DMGRSEQ# DS    XL(L'FW_SEQ#)       DATAMGR record sequence number               
                                                                                
SVMOA    DS    XL2                 Saved MOA value                              
                                                                                
GAPLPARM DS    XL1                 GAPLST Parameter                             
LIMINDS1 DS    XL1                 Limit list indicator                         
LIMSJAQ  EQU   X'80'               SJ access all                                
LIMSJLQ  EQU   X'40'               SJ limited access                            
                                                                                
STPOSTNG DS    X                   ** TIMTRN s/r posting status **              
STBUCKET EQU   X'10'               Update buckets only                          
                                                                                
POSTSTAT DS    X                   ** BLDTRN s/r indicators **                  
POSTS1R  EQU   X'80'               Making the 1R posting - buckets only         
POSTSSJ  EQU   X'40'               Making the SJ posting                        
POSTSSI  EQU   X'20'               Making the SI posting                        
POSTS1C  EQU   X'10'               Making the 1C posting                        
POSTS12  EQU   X'08'               Making the 12 posting                        
*&&UK                                                                           
POSTSCA  EQU   X'04'               Making the 1C/1R posting buckets             
*&&                                                                             
X_THRS   DS    PL8                 Total number of hours                        
                                                                                
ATAMOUNT DS    PL(L'TRNAMNT)       Posting amount                               
                                                                                
ATTRSST4 DS    XL(L'TRSSTAT4)      TRSSTAT4 value                               
ATREF    DS    CL(L'TRNKREF)       Transaction reference #                      
                                                                                
PACK8    DS    PL8                 For packed arithmetic                        
PACK16   DS    PL16                For packed arithmetic                        
                                                                                
*&&UK                                                                           
METHSTAT DS    X                   ** Method indicators **                      
METSHLVL EQU   X'80'               High level method profile record             
METSNANL EQU   X'40'               No analysis found on 1R use default          
                                                                                
BUCKTYPE DS    0CL2                ** Bucket type **                            
BUCKMTHD DS    C                   Method                                       
BUCKSLRY DS    C                   Salary type                                  
                                                                                
DRAMOUNT DS    PL(L'TRNAMNT)       Debit amount                                 
CRAMOUNT DS    PL(L'TRNAMNT)       Credit amount                                
                                                                                
ATPROF1  DS    X                   Cost/Sales profile 1 (B time)                
ATPROF2  DS    X                   Cost/Sales profile 2 (N and R time)          
ATPR1CST EQU   X'01'               Use cost rate                                
ATPR1SAL EQU   X'02'               Use sales rate                               
                                                                                
AT142ULA DS    0CL(L'ACTKULA)      2 level structure 14 account                 
AT142UNT DS    CL(L'ACTKUNT)                                                    
AT142LDG DS    CL(L'ACTKLDG)                                                    
AT142ACT DS    CL(L'ACTKACT)                                                    
AT143ULA DS    0CL(L'ACTKULA)      3 level structure 14 account                 
AT143UNT DS    CL(L'ACTKUNT)                                                    
AT143LDG DS    CL(L'ACTKLDG)                                                    
AT143ACT DS    CL(L'ACTKACT)                                                    
METHTAB  DS    XL(MT_MAX#*MT_LNQ)  Method table (see MT_D)                      
INCOPOS  DS    CL1                 Income ledger offpos                         
*&&                                                                             
                                                                                
         DS    0H                  Saved error messages and data                
ERRTAB   DS    XL(ET_LN1Q*ET_MAXQ)                                              
                                                                                
SAVEVARL EQU   *-SAVEVAR                                                        
SAVEL    EQU   *-SAVED                                                          
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
                                                                                
***********************************************************************         
* Error table                                                         *         
***********************************************************************         
                                                                                
ET_D     DSECT                                                                  
ET_EOTQ  EQU   FF                  End of table indiciator                      
ET_LN    DS    X                   Length of this entry                         
ET_ERRNO DS    XL(L'ROUERRV)       Error number                                 
ET_FLDNM DS    XL2                 Field number error applies                   
ET_LN1Q  EQU   *-ET_D                                                           
ET_EXTRA DS    0C                  Extra error text                             
ET_MAXQ  EQU   20                                                               
                                                                                
***********************************************************************         
* Method table                                                        *         
***********************************************************************         
*&&UK                                                                           
MT_D     DSECT                                                                  
MT_EOTQ  EQU   0                   End of table indicator                       
                                                                                
MT_METHD DS    CL(L'CAPKMTHD)      Method number                                
MT_PCT   DS    PL6                 Method % of allocation                       
                                                                                
MT_STR   DS    X                   ** Method 14 account structure **            
MT_S2LVL EQU   2                   2 level 14 account                           
MT_S3LVL EQU   3                   3 level 14 account                           
                                                                                
MT_ANAL  DS    C                   Default analysis code                        
MT_RLNQ  EQU   6                   Length of rates                              
MT_TRATE DS    PL(MT_RLNQ)         Total hourly rate                            
MT_PRATE DS    PL(MT_RLNQ)         Pension hourly rate                          
MT_BRATE DS    PL(MT_RLNQ)         Benefit hourly rate                          
MT_SRATE DS    PL(MT_RLNQ)         Salary hourly rate                           
MT_BCNT  EQU   (*-MT_PRATE)/MT_RLNQ                                             
MT_EBCNT EQU   (*-MT_TRATE)/MT_RLNQ                                             
MT_LNQ   EQU   *-MT_D                                                           
MT_MAX#  EQU   9                   Maximum number of table entries              
*&&                                                                             
**********************************************************************          
* TRACEIT print line                                                            
**********************************************************************          
                                                                                
PTRACED  DSECT                                                                  
PTRACE   DS    0CL120                                                           
PTTWHO   DS    CL7                                                              
         DS    CL1                                                              
PTMQ     DS    CL2                                                              
         DS    CL1                                                              
PTMODE   DS    CL6                                                              
PTSLASH  DS    CL1                                                              
PTFROM   DS    CL1                                                              
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
       ++INCLUDE FASSBOFF                                                       
                                                                                
ADDTRND  DSECT                                                                  
       ++INCLUDE ACADDTRND                                                      
                                                                                
*PREFIX=L                                                                       
       ++INCLUDE FALOCKETD                                                      
*PREFIX=                                                                        
LOCKETD  DSECT                                                                  
LOCKETL  EQU   *-LOCKETD                                                        
                                                                                
       ++INCLUDE SRUPD60D                                                       
       ++INCLUDE ACTRAND                                                        
       ++INCLUDE ACTIMED                                                        
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020ACBRA1E   11/12/20'                                      
         END                                                                    
