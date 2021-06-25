*          DATA SET ACBRA2F    AT LEVEL 001 AS OF 06/19/19                      
*PHASE T6242FA                                                                  
                                                                                
         TITLE 'MQ order electronic signature upload server'                    
                                                                                
***********************************************************************         
* Level change comments                                               *         
* ---------------------                                               *         
* MPEN 001 01May19 DSRD-22427 New server for Order Electronic         *         
*                             Signature upload                        *         
***********************************************************************         
* Coments                                                             *         
* -------                                                             *         
* Based on ACBRA10 (based on ACBRA14 and MELNK17 code)                *         
* This runs under MQ but can be run from =BRA in CMV for testing, too *         
* This is supposed to run single request at a time (so no two or more *         
* requests combined as a single run)                                  *         
* MPEN.TKLU.JCL(RUNBRAC) to IDF run it                                *         
* MPEN.TKLU.JCL(MQPUTBIG) to run as MQ                                *         
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
         LKSVR TYPE=UO,CODE=ENTRY,RLEN=4096,REQUEST=*,WORKERKEY=ACOI,  x        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ,         x        
               FILES=FILES,SERVERTYPE=TSTBOTU,SYSPHASE=SYSPHASE,IDF=Y, x        
               PROGRAM=RCVPBRAQ,LOADFACSOFF=Y                                   
                                                                                
ENTRY    DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO2F**,RR=RE                                                 
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
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         J     ENTRY06                                                          
         DROP  R6,R7                                                            
                                                                                
ORDESLIT DC    C'Order Electronic Signature Upload'                             
                                                                                
***********************************************************************         
* Handle DDLINK/RUNNER modes                                          *         
***********************************************************************         
                                                                                
ENTRY06  CLI   RUNMODE,RRUNSTRQ    Test 'First for run' mode                    
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
                                                                                
         CLI   RECTYPE,RECORDQ     Test order el status record                  
         JNE   UPLD04                                                           
         GOTOR UORD                                                             
         J     EXIT                                                             
                                                                                
UPLD04   DS    0H                                                               
         J     *+2                 (Invalid record)                             
         DROP  RB                                                               
                                                                                
***********************************************************************         
* Process Order upload record                                         *         
***********************************************************************         
                                                                                
UORD     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   CURRPIN,CCTPID      PIN retrieval                                
         TM    LP_FLAG2,LP_FMQIM                                                
         JZ    UORDRUN                                                          
         DS    0H                  Field 19: LP_SECNO (if set)                  
         L     RF,LP_ASECD         Field 29: SECOPASS from Secret block         
         MVC   CURRPIN,SECOPASS-SECD(RF)                                        
         MVC   CCTPID,CURRPIN                                                   
                                                                                
UORDRUN  CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    VALORD                                                           
         CLI   RUNMODE,RRUNREQQ    Test 'Run request' mode                      
         JNE   *+2                                                              
         TM    LP_FLAG2,LP_FMQIM   If MQ then go to update straight             
         JNZ   UPDORD                                                           
                                                                                
*** Validate order request data ***                                             
                                                                                
VALORD   DS    0H                                                               
                                                                                
         NI    TWAMODE,FFQ-(TWAMERP+TWAMEDP)                                    
         XC    DV_VALS(DV_VALSL),DV_VALS                                        
                                                                                
         GOTOR VDATCON,DMCB,(15,0),(1,DV_TODAY)                                 
                                                                                
         USING ORDRECD,R2                                                       
         LA    R2,IOKEY            read for order via passive                   
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUXCPY                                                   
         MVC   ORDKORD,OS_ORDNO                                                 
                                                                                
         MVC   CSVKEY1,ORDKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         JNE   *+2                                                              
                                                                                
         CLC   CSVKEY1(ORDKSEQ-ORDRECD),ORDKEY                                  
         JE    VORD02                                                           
                                                                                
         MVC   ROUERRV,=AL2(AE$ORDNF)                                           
         J     VORDERR                                                          
                                                                                
VORD02   MVC   CSVKEY1,ORDKEY      save order passive                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JNE   *+2                                                              
         GOTOR VALIDN              Check id number is valid                     
         JNE   VORDERR                                                          
*                                                                               
         L     R2,AIO2                                                          
         CLC   ORDROFF,SPACES      validate office if present                   
         JNH   VORD04                                                           
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,ORDROFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JE    VORD04                                                           
         DROP  R1                                                               
                                                                                
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         J     VORDERR                                                          
                                                                                
VORD04   MVC   DV_PEKEY,ORDKEY     save order prime key                         
         OC    OS_NSTAT,OS_NSTAT   validate status has been passed              
         JNZ   VORD06                                                           
         MVC   ROUERRV,=AL2(AE$MISTA)                                           
         J     VORDERR                                                          
                                                                                
VORD06   LA    R3,ORDRFST                                                       
*                                                                               
         USING ORDELD,R3                                                        
VORD08   CLI   ORDEL,0                                                          
         JE    *+2                 ORDELD must always be present                
         CLI   ORDEL,ORDELQ                                                     
         JE    VORD10                                                           
         LLC   R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     VORD08                                                           
*                                                                               
VORD10   MVC   DV_OGAPS,ORDGSTAT                                                
         MVI   DV_INDIC,YESQ                                                    
         MVC   DV_NGAPS,OS_NSTAT                                                
         GOTOR VHEXOUT,DMCB,DV_NEWID,DV_RETID,L'DV_NEWID                        
                                                                                
         CLI   RUNMODE,RRUNREQQ                                                 
         JE    UPDORD              now do updates                               
                                                                                
         LARL  RF,OMRET            Return details                               
         ST    RF,LP_AOMAP                                                      
         GOTOR LP_APUTO,LP_D                                                    
                                                                                
         LA    R0,ES_VALS          Clear all request values (required           
         LHI   R1,ES_DLNQ          for offline)                                 
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         J     UEXITY                                                           
                                                                                
VORDERR  CLI   RUNMODE,RRUNREQQ                                                 
         JE    *+2                 die if in run mode                           
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
                                                                                
         XR    RE,RE                                                            
         ICM   RE,1,DV_EREXL       Do we have any extra text                    
         JZ    VORDERR2                                                         
         STC   RE,GTLTXT           Set length of extra text                     
         LA    RE,TEMP                                                          
         STCM  RE,7,GTATXT         Set A(Extra text)                            
         DROP  R1                                                               
                                                                                
VORDERR2 GOTOR VGETTXT,(R1)                                                     
                                                                                
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
         DROP  R2,R3                                                            
                                                                                
*** Update order with request data ***                                          
                                                                                
UPDORD   DS    0H                                                               
                                                                                
         GOTOR VDATAMGR,DMCB,DMCOMMIT,0                                         
                                                                                
         GOTOR SETTIME                                                          
                                                                                
         GOTOR TRACEIT             Trace request                                
                                                                                
         MVC   DV_NGAPS,OS_NSTAT                                                
         OC    DV_ERROR,DV_ERROR                                                
         JZ    UORD02                                                           
         TM    LP_FLAG2,LP_FMQIM   If MQ then ok else die                       
         JNZ   UEXITY                                                           
         J     *+2                                                              
                                                                                
UORD02   OC    DV_NEWID,DV_NEWID                                                
         JZ    *+2                 (MQ SAVED not intact)                        
                                                                                
         USING ORDRECD,R2                                                       
         LA    R2,IOKEY            get order for update                         
         XC    IOKEY,IOKEY                                                      
         MVC   ORDKEY,DV_PEKEY                                                  
                                                                                
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   *+2                                                              
                                                                                
         GOTOR DELPAS,ORDRECD      delete passives                              
                                                                                
         L     R2,AIO2                                                          
         USING ORDELD,R3                                                        
         LA    R3,ORDRFST                                                       
*                                                                               
UORD04   CLI   ORDEL,0                                                          
         JE    *+2                                                              
         CLI   ORDEL,ORDELQ                                                     
         JE    UORD06                                                           
         LLC   R0,ORDLN                                                         
         AR    R3,R0                                                            
         J     UORD04                                                           
                                                                                
                                                                                
UORD06   MVC   DV_OGAPS,ORDGSTAT                                                
         MVC   ORDGSTAT,DV_NGAPS   set new GAP status                           
         MVC   ORDIDNO,DV_NEWID    set new ID number                            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JNE   *+2                                                              
                                                                                
         GOTOR ADDPAS,ORDRECD      Build passives                               
*                                                                               
EL       USING STCELD,ELEMENT                                                   
         XC    ELEMENT,ELEMENT     Build GAP status change el                   
         MVI   EL.STCEL,STCELQ                                                  
         MVI   EL.STCIND,STCIORD2                                               
         MVC   EL.STCOPID,CURRPIN                                               
         MVC   EL.STCOUSR,CUUSER                                                
         MVC   EL.STCODTE,DV_TODAY                                              
         MVC   EL.STCOTIM,CURTIME                                               
         MVI   EL.STCOTYP,STCOGAPQ                                              
         OI    EL.STCOTYP,STCOESGQ    Set electronic signature                  
         MVC   EL.STCGAPFR,DV_OGAPS                                             
         MVC   EL.STCGAPTO,DV_NGAPS                                             
         MVI   EL.STCLN,STCGAPLQ                                                
*                                                                               
         USING AUDRECD,R4                                                       
         LA    R4,IOKEY            build key of audit record                    
         MVI   BYTE1,0                                                          
         MVI   BYTE2,NOQ                                                        
         XC    IOKEY,IOKEY                                                      
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,OS_ORDNO                                                
         L     R1,=AL4(IORDD+IODIR+IO2)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    UORD10                                                           
*                                                                               
UORD08   L     R4,AIO2             clear aio area and add new audit             
         LR    R0,R4                                                            
         LHI   R1,IOLENQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,CUXCPY                                                   
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,OS_ORDNO                                                
         MVC   AUDRSTAT,ORDRSTAT                                                
         MVC   AUDRSTA2,ORDRSTA2                                                
         MVC   AUDKSEQ,BYTE1                                                    
         LHI   R0,AUDRFST-AUDRECD                                               
         STCM  R0,3,AUDRLEN                                                     
         MVI   BYTE2,YESQ                                                       
         J     UORD12                                                           
*                                                                               
UORD10   MVC   BYTE1,AUDKSEQ       Save current sequence number                 
         L     R1,=AL4(IORDUP+IODIR+IO2)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(IOGETRUP+IOMST+IO2)                                      
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO2                                                          
*                                                                               
UORD12   GOTOR TSTFIT                will it fit?                               
         JH    UORD16                                                           
         LA    RF,=CL8'ADD=END'                                                 
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),AUDRECD,EL.STCELD,(RF)                 
         CLI   12(R1),0                                                         
         JNE   *+2                                                              
         GOTOR SETDTE              Set low and high date on audit re            
         CLI   BYTE2,YESQ          Are we adding a new record?                  
         JNE   UORD14                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO2'                           
         JE    UORD20                                                           
         DC    H'0'                                                             
*                                                                               
UORD14   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOMST+IO2'                           
         JE    UORD20                                                           
         DC    H'0'                                                             
                                                                                
UORD16   LLC   RF,BYTE1            Read next audit record                       
         AHI   RF,1                                                             
         STC   RF,BYTE1                                                         
         MVI   BYTE2,NOQ                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO2'                               
         LA    R4,IOKEY                                                         
         CLC   AUDKEY(AUDKSEQ-AUDRECD),IOKEYSAV                                 
         JE    UORD12              Process next record                          
         J     UORD08              No more records left, add one                
                                                                                
UORD20   DS    0H                                                               
         J     UEXITY                                                           
         DROP  R2,R3,R4,EL                                                      
                                                                                
DMCOMMIT DC    C'COMMIT  '         Recovery checkpoint command                  
                                                                                
OMRET    LKOUT H                                                                
         LKOUT R,R#OESI                                                         
IDNo     LKOUT C,OS#IDNO,(D,B#SAVED,DV_RETID),CHAR,ND=Y                         
Error    LKOUT C,OS#ERROR,(D,B#SAVED,DV_ERMSG),CHAR,ND=Y                        
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
* Order electronic signature accessed upload                          *         
***********************************************************************         
                                                                                
OESREC   LKREQ H,A#OESI,NEWREC=Y                                                
OrdNo    LKREQ F,1,(D,B#SAVED,OS_ORDNO),CHAR,TEXT=(*,ORDNLIT)                   
OrdIDNo  LKREQ F,2,(D,B#SAVED,OS_IDNUM),HEXD,TEXT=(*,OIDNLIT)                   
Ostatus  LKREQ F,3,(D,B#SAVED,OS_NSTAT),LBIN,TEXT=(*,OSTALIT)                   
                                                                                
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
*&&US*&& AP    CURTIME,=P'60000'   Adjust to real (EST) time in US              
                                                                                
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
                                                                                
         LAY   RE,TAORDES                                                       
         MVC   PTTWHO,0(RE)                                                     
         CHI   R2,A#OESI                                                        
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
                                                                                
         LAY   RE,TDETS                                                         
         MVC   PTFIELD(L'TDETS),0(RE)                                           
         LA    R4,PFIELD                                                        
                                                                                
         MVC   0(L'OS_ORDNO,R4),OS_ORDNO                                        
         AHI   R4,L'OS_ORDNO+1                                                  
                                                                                
         GOTO1 APRINTER                                                         
                                                                                
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
TAORDES  DC    C'A#OESI'                                                        
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
         GOTO1 VPADDLE,DMCB,(C'A',AMYIO),CPTRBLK,IODA,0,ACOMFACS                
                                                                                
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Validate ID number                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALIDN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALIDN*'                                                      
                                                                                
         MVC   TEMP(L'XERRTXT),SPACES                                           
         XC    DV_NEWID,DV_NEWID                                                
                                                                                
         USING ORDELD,R2                                                        
         L     R2,AIO2             get current ID number                        
         AHI   R2,ORDRFST-ORDRECD                                               
         XR    R0,R0                                                            
                                                                                
VALIDN1  CLI   ORDEL,ORDELQ                                                     
         JE    VALIDN2                                                          
         CLI   ORDEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         IC    R0,ORDLN                                                         
         AR    R2,R0                                                            
         J     VALIDN1                                                          
                                                                                
VALIDN2  CLC   OS_IDNUM,ORDIDNO                                                 
         JE    VALIDN6                                                          
         MVC   OS_IDNUM,ORDIDNO                                                 
         J     VALIDN6                                                          
                                                                                
         MVC   ROUERRV,=AL2(AE$CFUPD)                                           
         GOTOR VHEXOUT,DMCB,ORDIDNO,TEMP,L'ORDIDNO                              
         MVI   TEMP+4,C'/'                                                      
         GOTOR VHEXOUT,DMCB,OS_IDNUM,TEMP+5,L'OS_IDNUM                          
         MVI   DV_EREXL,X'09'                                                   
         J     EXITN                                                            
                                                                                
VALIDN6  XR    RE,RE                                                            
         ICM   RE,3,ORDIDNO                                                     
         AHI   RE,1                                                             
         STCM  RE,3,DV_NEWID                                                    
         J     EXITY                                                            
                                                                                
         DROP  R2                                                               
***********************************************************************         
* Test element will fit on audit record                               *         
***********************************************************************         
         SPACE 1                                                                
         USING AUDRECD,R2                                                       
TSTFIT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*TSTFIT*'                                                      
EL       USING STCELD,ELEMENT                                                   
         XR    R0,R0                                                            
         ICM   R0,3,AUDRLEN                                                     
         LLC   RF,EL.STCLN                                                      
         AR    R0,RF                                                            
*                                                                               
TSTFITX  CHI   R0,MAXRECLN         Set condition code                           
         J     EXIT                                                             
         DROP  EL,R2                                                            
         EJECT                                                                  
***********************************************************************         
* Set date on audit record                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING AUDRECD,R2                                                       
SETDTE   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*SETDTE*'                                                      
         XC    FULL2,FULL2                                                      
         MVC   FULL1,EFFS                                                       
         LA    RF,AUDRFST                                                       
                                                                                
         USING STCELD,RF                                                        
SETDTE02 CLI   STCEL,0                                                          
         JE    SETDTE08                                                         
         CLI   STCEL,STCELQ                                                     
         JE    SETDTE06                                                         
SETDTE04 LLC   R0,STCLN                                                         
         AR    RF,R0                                                            
         J     SETDTE02                                                         
                                                                                
SETDTE06 CLC   FULL2,STCDATE                                                    
         JNL   *+10                                                             
         MVC   FULL2,STCDATE                                                    
         CLC   FULL1,STCDATE                                                    
         JNH   *+10                                                             
         MVC   FULL1,STCDATE                                                    
         J     SETDTE04                                                         
                                                                                
SETDTE08 GOTOR VDATCON,DMCB,(1,FULL1),(2,AUDRSTDT)                              
         GOTOR VDATCON,DMCB,(1,FULL2),(2,AUDRENDT)                              
         J     EXITY                                                            
         DROP  RF,R2                                                            
         EJECT                                                                  
                                                                                
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
                                                                                
OS#IDNO  EQU   1                   ID Number map number                         
OS#ERROR EQU   255                 Error code                                   
                                                                                
ORDNLIT  DC    C'Order number'                                                  
OIDNLIT  DC    C'Order ID number'                                               
OSTALIT  DC    C'Order new status'                                              
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
                                                                                
         LTORG                                                                  
                                                                                
WORKLEN  DC    AL2(WORKL)                                                       
                                                                                
PZERO    DC    P'0'                                                             
PNEGONE  DC    P'-1'                                                            
PONE     DC    P'1'                                                             
ADDEND   DC    C'ADD=END'                                                       
EFFS     DC    X'FFFFFFFF'                                                      
                                                                                
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
                                                                                
DCDICTL  DS    0X                                                               
DCDICTLX DC    X'FF'                                                            
                                                                                
RECTAB   DS    0XL(RECTABL)        ** RECORD TABLE **                           
         DC    AL2(A#OESI),AL1(RECORDQ)                                         
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTABL  EQU   *-RECTABD                                                        
                                                                                
                                                                                
RECORDQ  EQU   1                   Estimate approve/reject upload               
                                                                                
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
                                                                                
OS_ORDNO DS    CL6                 Order number                                 
OS_IDNUM DS    H                   Order ID number                              
OS_NSTAT DS    XL1                 Order new status                             
                                                                                
ES_DLNQ  EQU   *-ES_VALS                                                        
                                                                                
**********************************************************************          
* Derived values                                                                
**********************************************************************          
                                                                                
DV_VALS  DS    0F                                                               
                                                                                
DV_INDIC DS    CL1                 Y/N for ok                                   
DV_RETID DS    CL4                 Return ID number                             
DV_ERROR DS    XL2                 Return Error or blank                        
DV_EREXL DS    XL1                 Return Error extra text length               
DV_NEWID DS    XL2                 New ID number                                
DV_PEKEY DS    XL(L'ORDKEY)        Prime estimate key                           
DV_NGAPS DS    XL1                 New GAP status                               
DV_OGAPS DS    XL1                 Old GAP status                               
DV_TODAY DS    XL3                                                              
DV_ERMSG DS    CL80                                                             
                                                                                
DV_VALSL EQU   *-DV_VALS                                                        
                                                                                
***********************************************************************         
* General working storage values                                      *         
***********************************************************************         
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      RECORD TYPE                                  
                                                                                
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACBRA2F   06/19/19'                                      
         END                                                                    
