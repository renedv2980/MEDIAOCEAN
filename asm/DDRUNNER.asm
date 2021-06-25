*          DATA SET DDRUNNER   AT LEVEL 073 AS OF 11/21/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE RUNNERA                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE BINS31                                                                 
*INCLUDE BLDCUR                                                                 
*INCLUDE BUFFERIN                                                               
*INCLUDE CARDS                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE CUREDIT                                                                
*INCLUDE DEJAVU                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DMPRTQB                                                                
*INCLUDE EXPAND                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE FAEMUMSG                                                               
*INCLUDE FAGETTXT                                                               
*INCLUDE FASECRET                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE GETPROF                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOCKET                                                                 
*INCLUDE LOGOC                                                                  
*   BOTH MDUMPER & PDUMPER DEFINE PDMPSOON AS AN ENTRY. 2ND IS IGNORED!         
*   RUNNER USES PDMPSOON ONLY IF USING PDUMPER SO THAT MUST COME FIRST.         
*INCLUDE PDUMPER                                                                
*CHANGE PDMPSOON(XDMPSOON)                                                      
*INCLUDE MDUMPER                                                                
*INCLUDE PERVERT                                                                
*INCLUDE PHSCNT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE RECUP                                                                  
*INCLUDE RUNIT                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE SMTP                                                                   
*INCLUDE SMFOUT                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE SPOOFACT                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE DDSTATE                                                                
*&&US                                                                           
*INCLUDE LOGON                                                                  
*INCLUDE DEMVER                                                                 
*&&                                                                             
*&&UK                                                                           
*INCLUDE FAXPTAB                                                                
*INCLUDE LIMACC                                                                 
*INCLUDE LOGO                                                                   
*INCLUDE COMPOSE                                                                
*INCLUDE EUREKA                                                                 
*INCLUDE GETAUD                                                                 
*INCLUDE GETCPT                                                                 
*INCLUDE GETDPT                                                                 
*INCLUDE GETPROD                                                                
*INCLUDE GETUNV                                                                 
*INCLUDE MENXTSER                                                               
*INCLUDE SUGGEST                                                                
*INCLUDE TOBACCO                                                                
*INCLUDE SRCHEXEC                                                               
*INCLUDE SRCHPARS                                                               
*INCLUDE SRCHDIR                                                                
*INCLUDE SRCHDACC                                                               
*INCLUDE SRCHDUKM                                                               
*INCLUDE SRCHDATT                                                               
****LUDE PROMOTE                   removed as now in DMDMGRL/DDSIO              
*&&                                                                             
         IEABRCX DEFINE                                                         
         IEABRCX DISABLE                                                        
RUNNER   TITLE '- DDLINK batch host'                                            
RUNNER   CSECT ,                                                                
         PRINT NOGEN                                                            
                                                                                
         ENTRY ADWAIT                                                           
         ENTRY COMFACS                                                          
         ENTRY SYSFACS                                                          
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         ENTRY MASTC                                                            
                                                                                
         NBASE RUNWORKL,*RUNNER*,ARUNWORK,CLEAR=YES                             
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(RUNNER local w/s)                       
         USING UIDRECD,UIDREC                                                   
                                                                                
         ST    RB,ARUNNER          Save A(RUNNER)                               
         ST    RD,SAVERD           Save current RD value                        
                                                                                
         LAY   R0,IO                                                            
         ST    R0,AIO              Set A(I/O) area                              
         LAY   R0,QMBUFF                                                        
         ST    R0,AQMBUFF          Set A(QMAINT report buffer)                  
                                                                                
         LARL  R6,GLOBALS                                                       
         LARL  R7,GLOBALS+(4*ONEK)                                              
         USING GLOBALS,R6,R7       R6/R7=A(global literals)                     
         USING LP_D,LP_DB                                                       
         USING RUNFACSD,RUNFACS                                                 
         L     R8,AMASTC                                                        
         USING MASTC,R8            R8=A(MASTC)                                  
         MVC   DMGR,MCVDMGR        Save real datamanager address                
         MVI   WORKING,NOQ         Set not working (on a request)               
         L     RA,VCPRINT2                                                      
         USING DPRINT,RA           RA=A(CPRINT)                                 
         STM   R6,RA,RUNREG6A      Save global registers                        
         STM   RE,RC,12(RD)        Save registers for cancel                    
         LA    RE,UIDREC                                                        
         ST    RE,RUIDREC          Set A(UIDREC) in RUNFACS                     
                                                                                
         LARL  RF,SSB                                                           
         OI    SSOFLAG2-SSOOFF(RF),SSO2HIGH                                     
         OI    SSOFLAG3-SSOOFF(RF),SSO3RUNR                                     
                                                                                
         L     R1,X'10'(,0)        Get CVT address                              
         TM    X'74'(R1),X'80'     Test XA                                      
         JZ    *+10                                                             
         SR    R1,R1                                                            
         J     *+16                                                             
         L     R1,X'220'(,0)       Get ASCB address from PSAAOLD                
         L     R1,X'30'(R1)        Get LDA address                              
         L     R1,X'5C0'(R1)       Get current top of region address            
         ST    R1,MCDMPEND         Store end of region                          
                                                                                
         EXTRACT ATIOT,'S',FIELDS=(TIOT)                                        
         L     RF,ATIOT                                                         
         ST    RF,WTIOT                                                         
         MVC   MCJOB,0(RF)         Get my jobname                               
         MVC   MCSTEP,8(RF)        Get my stepname                              
                                                                                
         EXTRACT ASIDFLD,'S',FIELDS=(ASID)                                      
         L     R4,ASIDFLD          MVS job accounting information               
         LOCASCB ASID=(4)          Puts A(ASCB) into R1                         
         ST    R1,WASCB            Save A(ASCB)                                 
*                                                                               
         L     R1,ASCBASSB-ASCB(,R1) R1=A(ASSB)                                 
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         L     R1,ASSBJSAB-ASSB(,R1) R1=A(JSAB)                                 
         USING JSAB,R1                                                          
         MVC   MVSJOB,JSABJBID     GET JOBID (E.G. JOB12345)                    
         DROP  R1                                                               
         SAM24 ,                                                                
         PACK  DUB,MVSJOB+3(5)                                                  
         CVB   R1,DUB                                                           
         STCM  R1,3,MVSJNUM        CONVERT TO JOBNO                             
*                                                                               
         TIME  BIN                                                              
         ST    R0,MCRUNLPS                                                      
                                                                                
         MVC   ASID,ASIDFLD+2      Set my ASID                                  
                                                                                
         XC    MCREQNO,MCREQNO     Clear request number                         
         XC    MCNDUMPS,MCNDUMPS   Clear number of dumps                        
         LARL  R0,LOADEM                                                        
         ST    R0,MCVLOADM         Set phase loader address                     
         LARL  RE,ERRREQ           Set request abend exit                       
         STM   R0,RF,MCREQLST                                                   
         LARL  RE,ERRRUN           Set end of run routine                       
         STM   R0,RF,MCRUNLST                                                   
         GOTOR MCVDTCON,DMCB,(5,0),(10,MCDATE)                                  
                                                                                
         CSVQUERY INEPNAME=ASMIDF  Test running under IDF                       
         LTR   RF,RF                                                            
         JNZ   *+8                                                              
         OI    RUNINDS2,RUNIRIDF   Yes - set flag                               
                                                                                
         EXTRACT ACOMM,FIELDS=(COMM)                                            
         L     RF,ACOMM            Set up operator communications               
         ST    RF,WCOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB,COMECBPT                                                
         L     R4,COMCIBPT         GET A(CIB)                                   
         LA    R5,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
                                                                                
         CLI   CIBVERB-CIBNEXT(R4),CIBSTART                                     
         JNE   INIRUN02                                                         
         QEDIT ORIGIN=(5),BLOCK=(4)                                             
                                                                                
INIRUN02 QEDIT ORIGIN=(5),CIBCTR=1 Allow operator modify commands               
         LAM   AR0,ARF,ARZERO      Ensure access reqisters are clean            
                                                                                
         GOTOR VALPAR,0            Validate run parameters                      
         JNE   ENDRUN                                                           
                                                                                
         LHI   R0,10               R0=default primary cylinders                 
         OC    ROUTPRI,ROUTPRI     Set default primary cylinders                
         JNZ   *+8                                                              
         STCM  R0,7,ROUTPRI                                                     
                                                                                
         LHI   R0,5                R0=default secondary cylinders               
         OC    ROUTSEC,ROUTSEC                                                  
         JNZ   *+8                                                              
         STCM  R0,7,ROUTSEC        Set default secondary cylinders              
                                                                                
         SR    R1,R1                                                            
         ICM   R1,1,NSERVERS       Acquire server save storage                  
         JNZ   *+8                                                              
         LA    R1,1                Set to one if no server loaded               
         LAY   R0,SAVSIZEQ                                                      
         MR    R0,R0                                                            
         LA    R0,L'SVRSVEYE(R1)   Add length of eyecatcher                     
         GETMAIN R,LV=(0)          Get storage for server work areas            
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'SVRSVEYE,R1),SVRSVEYE                                        
         AHI   R1,L'SVRSVEYE                                                    
         ST    R1,ASVRSAVE         Set A(server save area)                      
                                                                                
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   *+14                                                             
         OC    DSPACE,DSPACE       Test data space known                        
         JZ    ENDRUN              No - can't do without it                     
                                                                                
         MVC   PMESSAGE(L'INFLIT4),INFLIT4                                      
         GOTOR PRTLOG                                                           
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
         OI    RUNINDS1,RUNISMTP   Set SMTP initialized                         
                                                                                
         MVC   PMESSAGE(L'INFLIT2),INFLIT2                                      
         GOTOR PRTLOG                                                           
                                                                                
         LHI   R0,QTSAR            Load TSAR and set address in LP_D            
         ICM   R0,B'1110',T00A                                                  
         GOTOR RGPHS,DMCB,0,(R0)                                                
         MVC   LP_ATSAR,0(R1)                                                   
                                                                                
         GOTOR GETUID,0            Initialize user-id buffer                    
                                                                                
         LARL  R4,COMTAB           Initialize COMFACS addresses                 
         USING COMTABD,R4                                                       
INIRUN04 ICM   R0,B'0001',COMNUM   Set/test phase number                        
         JZ    INIRUN08            Zero is end of table                         
         GOTOR RGPHS,DMCB,0,(R0)                                                
         ICM   RE,15,0(R1)                                                      
         JZ    INIRUN06                                                         
         SR    RF,RF                                                            
         ICM   RF,3,COMCDSP        Set address in COMFACS if necessary          
         JZ    INIRUN06                                                         
         A     RF,ACOMFACS                                                      
         STCM  RE,15,0(RF)                                                      
INIRUN06 AHI   R4,COMTABL          Bump to next table entry                     
         J     INIRUN04                                                         
         DROP  R4                                                               
                                                                                
INIRUN08 L     RF,ACOMFACS         Set A(LINKIO) in RUNFACSD                    
         MVC   RLINKIO,CLINKIO-COMFACSD(RF)                                     
         MVC   RWRKIO,CWRKIO-COMFACSD(RF)                                       
                                                                                
         CLI   MCESTAE,YESQ        Test ESTAE=Y                                 
         JNE   INIRUN10                                                         
         ESTAE PRGCHK,CT,ASYNCH=YES,TERM=YES                                    
                                                                                
INIRUN10 TM    RUNINDS1,RUNIBTCH   Test batch mode processing                   
         JZ    *+12                                                             
         GOTOR GETREQ              Yes - get requests and initialize            
         JNE   ENDRUN                                                           
                                                                                
         GOTOR RGOLINK,RRUNSTRQ    Pass run first mode to DDLINK/server         
         JE    INIRUN12                                                         
         MVC   PMESSAGE(L'LINIMESS),LINIMESS                                    
         GOTOR PRTLOG              Print server error message                   
         J     ENDRUN                                                           
                                                                                
INIRUN12 TM    RUNINDS1,RUNIBTCH   Test batch mode processing                   
         JNZ   INIRUNX                                                          
                                                                                
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,OFFSET                                                     
         SAC   512                                                              
         ICM   R2,15,TABSRUN-FATABSD(R2)                                        
         JZ    INIRUN30                                                         
                                                                                
         LLC   R0,TABSSRWD-TABSRUND(,R2)                                        
         LLC   R1,TABSJBWD-TABSRUND(,R2)                                        
         CLI   TABSIND1-TABSRUND(R2),0                                          
         JE    *+12                                                             
         LHI   R0,TABSSVRL                                                      
         LHI   R1,TABSQUEL                                                      
         STH   R0,SVRWIDTH         Set width of server entry                    
         STH   R1,QUEWIDTH         Set width of queue entry                     
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,TABSNSVR-TABSRUND(R2)                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R2,15,TABSASVR-TABSRUND(R2)                                      
         JNZ   INIRUN14                                                         
         DC    H'0'                                                             
                                                                                
         USING TABSSVRD,R2                                                      
INIRUN14 CLC   TSTYPE,LP_SVRTY     Test entry with same server type             
         JNE   *+14                                                             
         CLC   TSJOB,MCSTEP        Test duplicate step name                     
         JE    INIRUN16                                                         
         AH    R2,SVRWIDTH         Bump to next server                          
         JCT   R0,INIRUN14                                                      
         J     INIRUN26                                                         
                                                                                
***********************************************************************         
* A server with a duplicate step name has been found - this must be   *         
* because the server abended without removing itself from the table   *         
***********************************************************************         
                                                                                
INIRUN16 STCM  R2,15,ASVR          Save A(server entry)                         
         CLI   TSFLAG,TSFACTIV     Test canceled server was active              
         JNE   INIRUN24            No - just re-use the entry                   
                                                                                
         LAM   AR3,AR3,ALET        Yes - locate queue entry in process          
         ICM   R3,15,OFFSET                                                     
         SR    R0,R0                                                            
         ICM   R0,3,TABSNQUE-TABSRUND(R3)                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,TABSAQUE-TABSRUND(R3)                                      
         JNZ   INIRUN18                                                         
         DC    H'0'                                                             
                                                                                
         USING TABSQUED,R3         R3=A(server queue)                           
INIRUN18 CLC   TQTYPE,TSTYPE       Match server type to queue                   
         JNE   INIRUN22                                                         
         CLI   TQSTATUS,TQSRUN     Test status is 'running'                     
         JNE   INIRUN22                                                         
         CLC   TQASVR,ASVR         Test located server was running this         
         JNE   INIRUN22                                                         
         STCM  R3,15,AQUE          Save A(queue entry)                          
         MVC   WKEY,TQWKEY         Extract worker file key                      
         OC    WKEYUID,WKEYUID     Test MQ work                                 
         JZ    INIRUN24                                                         
                                                                                
         MVI   TQSTATUS,TQSDONE    Set work completed                           
         MVI   TQERROR,TQEPCHK     Set program check                            
         XC    TQASVR,TQASVR       Clear server address                         
                                                                                
         SAC   0                                                                
         LAM   AR2,AR3,ARZERO                                                   
         L     R4,RWRKBLK1                                                      
         USING WRKIOD,R4                                                        
         XC    WRKINDX,WRKINDX                                                  
         MVC   WRKIFTYP,SVRFTYP    Set file type                                
         MVI   WRKIACTN,WRKIAAPP   Set file open type                           
         MVI   WRKISTAT,0          Set file status                              
         MVC   WRKWKEY,WKEY        Set file key                                 
         GOTOR RWRKIO,WRKIOD       Open the file                                
         JNE   INIRUN24                                                         
         MVI   WRKIACTN,WRKIACLE                                                
         GOTOR RWRKIO,WRKIOD       Close file & set 'in error'                  
         J     INIRUN24                                                         
         DROP  R4                                                               
                                                                                
INIRUN22 AH    R3,QUEWIDTH                                                      
         JCT   R0,INIRUN18         Do for number of queue entries               
                                                                                
INIRUN24 LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,ASVR          Point back to server entry                   
         SAC   512                                                              
         MVC   TSLFACID,FACPAKID   Set FACPAK ID (if any)                       
                                                                                
         TM    RUNINDMQ,RUNIMQNR   Test MQRUNQ=NO                               
         JZ    *+8                                                              
         MVI   TSLFACID,X'FF'      Yes, server unavailable to FACPAKs           
                                                                                
         LA    R0,ECBWRK                                                        
         STCM  R0,7,TSLPOST        Set A(my post ECB)                           
         XC    AQUE,AQUE                                                        
         J     INIRUN32                                                         
         DROP  R3                                                               
                                                                                
INIRUN26 ICM   R2,15,OFFSET        Locate a free server entry                   
         ICM   R2,15,TABSRUN-FATABSD(R2)                                        
         SR    R0,R0                                                            
         ICM   R0,3,TABSNSVR-TABSRUND(R2)                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R2,15,TABSASVR-TABSRUND(R2)                                      
         JNZ   INIRUN28                                                         
         DC    H'0'                                                             
                                                                                
INIRUN28 SR    RE,RE                                                            
         LA    RF,ECBWRK                                                        
         ICM   RF,8,FACPAKID                                                    
         CS    RE,RF,TSLOCKID                                                   
         JE    INIRUN32            Found an empty slot?                         
         AH    R2,SVRWIDTH                                                      
         JCT   R0,INIRUN28         Do for number of servers                     
                                                                                
INIRUN30 SAC   0                   No available server slots to use             
         LAM   AR2,AR2,ARZERO                                                   
         MVC   PMESSAGE(L'FULLIT),FULLIT                                        
         GOTOR PRTLOG                                                           
         J     ENDRUN                                                           
                                                                                
INIRUN32 STCM  R2,15,ASVR          Save A(my server entry)                      
         XC    TSAQUE,TSAQUE       Set no queue entry to run                    
         MVC   TSJOB,MCSTEP        Set my step name                             
         MVC   TSASID,ASID         Set my ASID                                  
         MVC   TSTYPE,LP_SVRTY     Set my server type                           
         MVI   TSFLAG,TSFACTIV     Set me as active for now                     
         CLC   SVRWIDTH,=AL2(TSTFAGYL)                                          
         JL    INIRUNX                                                          
         TM    RUNINDMQ,RUNIMQEI   Test EDISCHED MQ input queue                 
         JZ    *+8                                                              
         OI    TSTIND1,TSTIMQ      Yes - turn on MQ enabled flag                
*                                                                               
         TM    RUNINDS1,RUNIUPDT                                                
         JZ    *+8                                                              
         OI    TSTIND1,TSTIUPDT    Yes - turn on update flag                    
*                                                                               
         MVC   TSTFAGYS(L'AGYFILT),AGYFILT                                      
                                                                                
INIRUNX  SAC   0                                                                
         LAM   AR0,ARF,ARZERO      Ensure access registers are zero             
                                                                                
         GOTOR MQINIT              Initialize MQ interface                      
         JNE   ENDRUN                                                           
         EJECT                                                                  
***********************************************************************         
* Initialize for running work                                         *         
***********************************************************************         
                                                                                
INIWRK   CLI   SWAPABLE,YESQ       Test SWAPABLE=Y                              
         JE    INIWRK02                                                         
         LHI   R0,-4                                                            
         SVC   247                 Set non-swappable                            
                                                                                
INIWRK02 TM    RUNINDS1,RUNIBTCH   Test batch mode processing                   
         JNZ   RUNBAT                                                           
                                                                                
         LARL  R1,SEOLST           Open files in all systems                    
         USING SEOLSTD,R1          R1=A(system entry)                           
INIWRK04 CLI   SEOLNUM,SEOLEOTQ    Test end of system list                      
         JE    GETWRK                                                           
         TM    SEOLFLAG,SEOLFNOP   Test system is no-op                         
         JNZ   INIWRK06                                                         
         MVI   OPNACT,OPNAOPN      Set action to open files                     
         GOTOR ROPNCLO,(R1)        Open system files                            
INIWRK06 AHI   R1,SEOLSTL          Bump to next entry                           
         J     INIWRK04                                                         
         DROP  R1,RB                                                            
                                                                                
SETREG   LARL  R6,RUNREG6A         Restore RUNNER registers                     
         LM    R6,RA,0(R6)                                                      
         BR    RE                                                               
                                                                                
ARUNWORK DC    A(RUNWORK)                                                       
RUNREG6A DC    5A(0)               RUNNER global registers (R6 thru RA)         
ACOMM    DC    A(0)                                                             
ATIOT    DC    A(0)                                                             
ASIDFLD  DC    F'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Find earliest arrived work and run it                               *         
***********************************************************************         
                                                                                
GETWRK   L     RF,MCUTL            Reset to control system files                
         MVI   4(RF),CONSYSQ                                                    
         NI    LP_FLAG2,FF-(LP_FFALT)                                           
         MVI   MCBLOW,NOQ          Preset MCBLOW                                
                                                                                
         GOTOR TSTOPS              Test any operator commands pending           
                                                                                
         CLI   ENDRUNFL,C'Y'       Exit now if terminate pending                
         JE    ENDRUN                                                           
*&&UK                                                                           
         TM    RUNINDS2,RUNIMEDZ   If MEDZ down wait for open command           
         JNO   GETWRK01                                                         
         MVC   PMESSAGE(L'MZWAITM),MZWAITM                                      
         GOTOR PRTLOG                                                           
         NI    ECBWRK,FF-(ECBPOSTQ)                                             
         NI    ECBMQG,FF-(ECBPOSTQ)                                             
         J     WAIT                                                             
*&&                                                                             
GETWRK01 TM    RUNINDS1,RUNIGOMQ   Test have uploaded FALINK string             
         JZ    GETWRK02                                                         
         NI    RUNINDS1,FF-(RUNIGOMQ)                                           
         NI    LP_FLAG2,FF-(LP_FMQEQ)                                           
         OI    LP_FLAG2,LP_FFALT   Set FALINK test string input                 
         XC    WKEYZERO,WKEYZERO                                                
         J     RUNWRK              and process as an MQ message                 
                                                                                
GETWRK02 NI    RUNINDMQ,FF-(RUNIMQUP)                                           
                                                                                
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO      Clear access registers                       
         XC    AQUE,AQUE           Clear A(work queue entry)                    
                                                                                
         CLI   DDLYNUM,0           Test retrying MQ deadly embrace              
         JE    GETWRK03                                                         
         MVC   PMESSAGE(L'DDLRMSG),DDLRMSG yes, log the fact                    
         LLC   R2,DDLYNUM                                                       
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMESSAGE+17(3),DUB+6(2)                                          
         GOTOR PRTLOG                                                           
         BCTR  R2,0                                                             
         L     R1,DDLYWT1          Wait time for first retry                    
         SLL   R1,0(R2)            double for each subsequent retry             
         C     R1,DDLYWTMX         up to a maximum                              
         JNH   *+8                                                              
         L     R1,DDLYWTMX                                                      
         ST    R1,FULL                                                          
         STIMER WAIT,BINTVL=FULL                                                
         J     GETWRK26            Go to MQ handling                            
                                                                                
GETWRK03 MVC   PMESSAGE(L'WORKMSG),WORKMSG                                      
         GOTOR PRTLOG                                                           
                                                                                
         NI    ECBWRK,FF-(ECBPOSTQ)                                             
                                                                                
         LAM   AR2,AR2,ALET        Look for work to do in the job queue         
         ICM   R2,15,ASVR                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SAC   512                                                              
         ICM   R3,15,TSAQUE        R3=A(command queue entry)                    
         LAM   AR2,AR2,ARZERO                                                   
         MVI   RUNTYPE,RUNTSRVR    Set processing a server file                 
         LTR   R3,R3                                                            
         JZ    GETWRK06                                                         
         MVI   RUNTYPE,RUNTCMND    Set processing a command file                
         LAM   AR3,AR3,ALET                                                     
         USING TABSQUED,R3                                                      
         CLI   TQSTATUS,TQSWAIT    Test work waiting to run                     
         JNE   GETWRK04                                                         
         SR    RE,RE                                                            
         L     RF,ASVR             Try to lock this work entry                  
         CS    RE,RF,TQASVR                                                     
         JNE   GETWRK04                                                         
         XC    RVALUES(RVALUESL),RVALUES                                        
         XC    CPUREQ,CPUREQ                                                    
         J     GETWRK24            Successful lock - process the job            
                                                                                
GETWRK04 SAC   0                   Else error message and tidy-up               
         LAM   AR3,AR3,ARZERO                                                   
         MVC   PMESSAGE(L'CMDERR2),CMDERR2                                      
         GOTOR PRTLOG                                                           
         J     ENDREQ                                                           
                                                                                
GETWRK06 TM    RUNINDMQ,RUNIMQNR   Test MQRUNQ=NO                               
         JZ    GETWRK08            No, skip                                     
         XC    AQUE,AQUE           Clear A(work queue entry)                    
         XC    RAQUE,RAQUE         clear address for DDLINK/server              
         XC    MCFACPAK,MCFACPAK   Clear client id in MASTC                     
         XC    WKEY,WKEY           Clear work file key                          
         J     GETWRK26            Go get next MQ item                          
                                                                                
GETWRK08 GOTOR BLDSVR              Build list of specific servers               
                                                                                
         LAM   AR3,AR3,ALET        Locate work queue entry to run               
         ICM   R3,15,OFFSET                                                     
         ICM   R3,15,TABSRUN-FATABSD(R3)                                        
         JZ    ENDRUN                                                           
         SR    R0,R0                                                            
         ICM   R0,3,TABSNQUE-TABSRUND(R3)                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,TABSAQUE-TABSRUND(R3)                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LAM   AR5,AR5,ALET                                                     
         SR    R5,R5               R5=A(oldest entry)                           
                                                                                
         USING TABSQUED,R3                                                      
GETWRK10 CLI   TQSTATUS,TQSWAIT    Test work waiting to run                     
         JNE   GETWRK22                                                         
         CLC   TQTYPE,LP_SVRTY     Yes - test one for my server                 
         JNE   GETWRK22                                                         
         CLC   TQASVR,ARZERO       Yes - test locked                            
         JNE   GETWRK22                                                         
                                                                                
         CLC   SVRWIDTH,=AL2(TSTFAGYL)                                          
         JL    GETWRK14                                                         
         CLC   TQAGY,ARZERO        Test have agency id                          
         JE    GETWRK14                                                         
         OC    AGYFILT(L'TSTFAGYS),AGYFILT                                      
         JZ    GETWRK12                                                         
         LA    RF,AGYFILT          Point to agency filter list                  
         LHI   R0,TSTFAGY#                                                      
         BASR  RE,0                                                             
         CLC   0(L'TSTFAGYS,RF),SPACES                                          
         JE    GETWRK22                                                         
         CLC   TQAGY,0(RF)         Is in my agency list - run                   
         JE    GETWRK16                                                         
         AHI   RF,L'TSTFAGYS                                                    
         BCTR  R0,RE                                                            
         J     GETWRK22                                                         
                                                                                
GETWRK12 GOTOR LOCSVR,TQAGY        Test any other server runs this work         
         JE    GETWRK22                                                         
                                                                                
GETWRK14 CLC   FACPAKID,TQLFACID   Work for me to do?                           
         JE    GETWRK16                                                         
         CLI   TQLFACID,C'O'       Test work submitted from a server            
         JE    GETWRK16            Yes - I can run this                         
         CLI   FACPAKID,0          Test I am a generic server                   
         JNE   GETWRK22            No - can't do this work                      
         SR    RE,RE                                                            
         ICM   RE,1,TQLFACID       Is there a specific server to                
         LA    RE,WORK(RE)         Run this work?                               
         CLI   0(RE),0             Yes - not for me to do then                  
         JNE   GETWRK22                                                         
                                                                                
GETWRK16 SR    RF,RF                                                            
         ICM   RF,3,NOTLSTN        Test any jobs in 'not list'                  
         JZ    GETWRK20                                                         
         LHI   RE,NOTLST-WORKD                                                  
         LA    RE,WORKD(RE)                                                     
                                                                                
GETWRK18 CLC   TQWKEY,0(RE)        Yes - is this one of them?                   
         JE    GETWRK22            Yes - go on to next entry                    
         AHI   RE,L'TQWKEY                                                      
         JCT   RF,GETWRK18         Do for N'entries in NOTLST                   
                                                                                
GETWRK20 LTR   R5,R5               Test any found yet to run                    
         JZ    *+14                                                             
         CLC   TQATIME,TQATIME-TABSQUED(R5)                                     
         JH    GETWRK22                                                         
         LR    R5,R3               Point to first or earliest found             
                                                                                
GETWRK22 AH    R3,QUEWIDTH                                                      
         JCT   R0,GETWRK10         Do for all entries                           
                                                                                
         LAM   AR5,AR5,ARZERO                                                   
         LTR   R3,R5               Test any work found to run                   
         JZ    WAIT                No - go wait for work to arrive              
         SR    RE,RE                                                            
         L     RF,ASVR             Try to lock this work entry                  
         CS    RE,RF,TQASVR                                                     
         JNE   GETWRK              Can't lock - try from top again              
                                                                                
GETWRK24 STCM  R3,15,AQUE          Save A(work queue entry)                     
         STCM  R3,15,RAQUE         Set address for DDLINK/server                
         MVC   MCFACPAK,TQLFACID   Set client id in MASTC                       
         MVC   WKEY,TQWKEY         Set work file key                            
                                                                                
         OC    WKEYZERO,WKEYZERO   Test have message id                         
         JNZ   GETWRK40            No, not MQ                                   
*                                                                               
*        MQ request handling                                                    
*                                                                               
GETWRK26 SAC   0                   Turn off access addressing                   
         LARL  R4,MQGLOB                                                        
         USING MQGLOB,R4           Point to MQ globals                          
         LAM   AR2,AR3,ARZERO                                                   
                                                                                
         CLI   DDLYNUM,0           Test retrying MQ deadly embrace              
         JE    GETWRK27                                                         
         OI    LP_FLAG2,LP_FMQDR   Yes, tell server                             
         TM    RUNINDMQ,RUNIMQNR   Test MQRUNQ=NO                               
         JNZ   GETWRK30            Yes, build new workq entry first             
         J     GETWRK38            else just go start request                   
                                                                                
GETWRK27 NI    LP_FLAG2,FF-(LP_FMQDR) Not retrying MQ deadly embrace            
         L     R0,MQIBUFF          CLear input buffer                           
         L     R1,MQIBUFFL                                                      
         SR    RF,RF                                                            
         SAM31 ,                                                                
         MVCL  R0,RE                                                            
         SAM24 ,                                                                
                                                                                
         XC    MQMDESC_MSGID,MQMDESC_MSGID                                      
         XC    MQMDESC_CORRELID,MQMDESC_CORRELID                                
         XC    MQEDIOPT_MATCHOPTIONS,MQEDIOPT_MATCHOPTIONS                      
                                                                                
         TM    RUNINDMQ,RUNIMQNR   Test MQRUNQ=NO                               
         JNZ   GETWRK28            YES, SKIP                                    
*                                                                               
*        MQ requests via Run Queue (MQRUNQ not NO)                              
*                                                                               
         MVC   MQMDESC_MSGID(L'WKEY),WKEY                                       
         MVC   MQEDIOPT_MATCHOPTIONS,MQMATOPT                                   
                                                                                
         GOTOR RCALLMQ,MQGETEDI    Issue MQGET for message id                   
         JE    GETWRK38            We got the message                           
         CLC   MQREASN,=A(MQRC_CONNECTION_BROKEN)                               
         JNE   *+2                                                              
         BRAS  RE,MQRECON                                                       
         J     GETWRK27                                                         
*                                                                               
*        MQ requests direct (MQRUNQ=NO)                                         
*                                                                               
GETWRK28 XC    ECBMQG,ECBMQG       Not using RunQ. Clear MQ ECB                 
                                                                                
         LHI   RF,MQGMO_SET_SIGNAL                                              
         ST    RF,MQEDIOPT_OPTIONS                                              
         LA    RF,ECBMQG           Tell MQ to signal and set ECB                
         ST    RF,MQEDIOPT_SIGNAL1                                              
         LHI   RF,MQWI_UNLIMITED                                                
         ST    RF,MQEDIOPT_WAITINTERVAL                                         
                                                                                
         GOTOR RCALLMQ,MQGETEDI    MQGET to return message or set ECB           
         JE    GETWRK29            DIE if unexpected MQ condition               
         CLC   MQREASN,=A(MQRC_CONNECTION_BROKEN)                               
         JNE   *+2                                                              
         BRAS  RE,MQRECON                                                       
         J     GETWRK28                                                         
                                                                                
GETWRK29 CLC   MQCOMPC,MQCCOK      Message returned if OK completion            
         JNE   WAIT                else signal accepted or already set          
                                                                                
GETWRK30 LAM   AR3,AR3,ALET        Add work queue entry for this MQ msg         
         SAC   512                                                              
         ICM   R3,15,OFFSET                                                     
         ICM   R3,15,TABSRUN-FATABSD(R3)                                        
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,TABSNQUE-TABSRUND(R3)                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,TABSAQUE-TABSRUND(R3)                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING TABSQUED,R3                                                      
GETWRK32 SR    RE,RE                                                            
         ICM   RF,12,=C'O1'        Replacement value is lock TOKEN              
         ICM   RF,3,ASID                                                        
         CS    RE,RF,TQLOCKID                                                   
         JE    GETWRK34            Grabbed a free queue entry                   
         AH    R3,QUEWIDTH         Bump to next queue entry                     
         BRCT  R0,GETWRK32                                                      
         SAC   0                                                                
         LARL  R2,ONESECND         No free entries.                             
         STIMER WAIT,BINTVL=(2)    Wait one second                              
         J     GETWRK30            and try again                                
                                                                                
GETWRK34 STCM  R3,15,AQUE          Save A(work queue entry)                     
         STCM  R3,15,RAQUE         Set address for DDLINK/server                
         MVC   WORK(TABSQUEL),TABSQUED Build queue entry in WORK                
         SAC   0                                                                
         LA    R3,WORK                                                          
         XC    TQECB,TQECB         No completion ECB                            
         XC    TQASVR,ASVR         Set we are running this                      
         TIME  TU                                                               
         STCM  R0,15,TQATIME       Set arrival time                             
         MVC   TQTYPE,LP_SVRTY     Set server type                              
         XC    TQMQZERO,TQMQZERO   Set MQ                                       
         MVC   TQMQASID,ASID       Set MQ file key                              
         GOTOR MCVDTCON,DMCB,(4,MCDATE),(2,TQMQDATE)                            
         MVC   TQMQTIME,TQATIME                                                 
         MVC   WKEY,TQWKEY                                                      
         MVI   TQSTATUS,TQSWAIT    Set waiting to run                           
         MVI   TQERROR,TQEGOOD     Set no error                                 
         MVI   TQACTION,0          Set no action                                
         MVI   TQQFLAG,TQQFNONE    Set clear flag                               
         XC    TQAGY,TQAGY         Don't know agency yet                        
         MVC   MCFACPAK,TQLFACID   Set client id in MASTC                       
         ICM   R3,15,AQUE          Restore A(work queue entry)                  
         SAC   512                                                              
         MVC   TABSQUED(TABSQUEL),WORK Move queue entry to queue                
*                                                                               
*        COMMON MQ request handling                                             
*                                                                               
GETWRK38 LAM   AR0,ARF,ARZERO      Clear access registers                       
         SAC   0                                                                
         CLI   DDLYNUM,0           Test retrying MQ deadly embrace              
         JNE   GETWRK39                                                         
         GOTOR RCALLMQ,MQCMT       No, Commit the GET                           
GETWRK39 OI    RUNINDMQ,RUNIMQCI   Set message id given                         
         OI    LP_FLAG2,LP_FMQEQ                                                
         XC    WKEYZERO,WKEYZERO                                                
         J     RUNWRK                                                           
         DROP  R4                                                               
*                                                                               
*        Non-MQ request handling (always via Run Queue)                         
*                                                                               
GETWRK40 LH    R1,TQLFASID                                                      
         SAC   0                                                                
         LAM   AR3,AR3,ARZERO                                                   
         LOCASCB ASID=(1)          Locate client's ASCB                         
         LTR   RF,RF                                                            
         JZ    GETWRK42                                                         
                                                                                
         LAM   AR3,AR3,ALET        Client is not around anymore                 
         ICM   R3,15,AQUE          Clear his queue entry                        
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SAC   512                                                              
         XC    TABSQUED(TABSQUEL),TABSQUED                                      
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,ASVR                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TSAQUE,AQUE         Test this was a command file                 
         JNE   *+10                                                             
         XC    TSAQUE,TSAQUE       Yes - clear its address                      
         J     GETWRK                                                           
                                                                                
GETWRK42 LAM   AR3,AR3,ALET                                                     
         ICM   R3,15,AQUE                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SAC   512                                                              
         GOTOR UPDQUE,WAITPROC     Set this work is being run                   
         MVI   TQQFLAG,0           Reset QMAINT flag (for SYSNOP)               
         XC    RVALUES(RVALUESL),RVALUES                                        
         XC    CPUREQ,CPUREQ                                                    
         MVC   RARRTIM,TQATIME     Set arrival time                             
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,ASVR                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO      Clear access registers                       
                                                                                
         TIME  TU                                                               
         ST    R0,RSTRTIM          Set start time                               
                                                                                
         S     R0,RARRTIM          Subtract arrival time                        
         JM    GETWRKX             (must cross midnight if negative)            
         ST    R0,FULL                                                          
         OC    MAXQTIME,MAXQTIME   Test maximum queue time set                  
         JZ    GETWRK50                                                         
         C     R0,MAXQTIME         Compare against maximum allowed              
         JNH   GETWRK50                                                         
                                                                                
         OC    SLOW,SLOW           Test slow notifications supplied             
         JZ    *+12                                                             
         LA    R0,SLOW             Yes - use it                                 
         J     *+12                                                             
         ICM   R0,15,SVRSLOW       Test anyone to notify                        
         JZ    GETWRK46                                                         
         OC    QTIMTMEM,QTIMTMEM   Have we sent a QTIMLIT email                 
         JZ    GETWRK44                                                         
         L     RF,RSTRTIM          How long ago                                 
         S     RF,QTIMTMEM                                                      
         JM    GETWRK44            crossed midnight                             
         C     RF,TUS01M                                                        
         JL    GETWRK46            don't send if <1 minute ago                  
                                                                                
GETWRK44 MVC   QTIMTMEM,RSTRTIM    save time this email sent                    
         MVC   P,SPACES                                                         
         MVC   P(L'MCSTEP),MCSTEP                                               
         MVC   P+L'MCSTEP+1(L'QTIMLIT),QTIMLIT                                  
         GOTOR VSMTP,DMCB,('SMTPCNCL',0)                                        
         GOTOR (RF),(R1),('SMTPAPRS',(R0)),(70,P)                               
         GOTOR (RF),(R1),('SMTPASND',0)                                         
         MVC   P,SPACES                                                         
                                                                                
GETWRK46 MVC   PMESSAGE(L'QTIMLIT),QTIMLIT                                      
         GOTOR PRTLOG                                                           
                                                                                
         OC    QTIMTMWT,QTIMTMWT   Have we sent a QTIMLIT WTO                   
         JZ    GETWRK48                                                         
         L     RF,RSTRTIM          How long ago                                 
         S     RF,QTIMTMWT                                                      
         JM    GETWRK48            crossed midnight                             
         C     RF,TUS10S                                                        
         JL    GETWRK50            don't send if <10 secs ago                   
                                                                                
GETWRK48 MVC   QTIMTMWT,RSTRTIM    save time this email sent                    
         GOTOR DMGR,DMCB,DMOPMSG,(L'QTIMLIT,QTIMLIT)                            
                                                                                
GETWRK50 OC    AVGQTIME,AVGQTIME   Test average queue time set                  
         JZ    GETWRKX                                                          
         L     R0,TOTQVAL          Accumulate total queue time across           
         A     R0,FULL             the sample size                              
         ST    R0,TOTQVAL                                                       
         L     R0,SAMPCNT          Bump current sample count                    
         AHI   R0,1                                                             
         ST    R0,SAMPCNT                                                       
         CLC   SAMPCNT,SAMPLE      Test sample size reached                     
         JL    GETWRKX             No                                           
         L     R1,AVGQTIME         Test average queue time exceeded             
         M     R0,SAMPLE                                                        
         C     R1,TOTQVAL                                                       
         JH    GETWRK58            No - clear counts                            
         L     R0,TOTQVAL                                                       
         SR    R0,R1               This is how much was exceeded in TUs         
         LR    R1,R0                                                            
         SR    R0,R0                                                            
         D     R0,SAMPLE                                                        
         LR    R0,R1               R0=TUs per second over average               
         GOTOR EDTTUS,ATIMTIM                                                   
                                                                                
         OC    SLOW,SLOW           Test slow notifications supplied             
         JZ    *+12                                                             
         LA    RF,SLOW             Yes - use it                                 
         J     *+12                                                             
         ICM   RF,15,SVRSLOW       Test anyone to notify                        
         JZ    GETWRK54                                                         
         OC    ATIMTMEM,ATIMTMEM   Have we sent a ATIMLIT email                 
         JZ    GETWRK52                                                         
         L     R0,RSTRTIM          How long ago                                 
         S     R0,ATIMTMEM                                                      
         JM    GETWRK52            crossed midnight                             
         C     R0,TUS01M                                                        
         JL    GETWRK54            don't send if <1 minute ago                  
                                                                                
GETWRK52 MVC   ATIMTMEM,RSTRTIM    save time this email sent                    
         MVC   P,SPACES                                                         
         MVC   P(L'MCSTEP),MCSTEP                                               
         MVC   P+L'MCSTEP+1(L'ATIMLIT+L'ATIMTIM),ATIMLIT                        
         GOTOR VSMTP,DMCB,('SMTPCNCL',0)                                        
         GOTOR (RF),(R1),('SMTPAPRS',(RF)),(70,P)                               
         GOTOR (RF),(R1),('SMTPASND',0)                                         
         MVC   P,SPACES                                                         
                                                                                
GETWRK54 MVC   PMESSAGE(L'ATIMLIT+L'ATIMTIM),ATIMLIT                            
         GOTOR PRTLOG                                                           
                                                                                
         OC    ATIMTMWT,ATIMTMWT   Have we sent a ATIMLIT WTO                   
         JZ    GETWRK56                                                         
         L     RF,RSTRTIM          How long ago                                 
         S     RF,ATIMTMWT                                                      
         JM    GETWRK56            crossed midnight                             
         C     RF,TUS10S                                                        
         JL    GETWRK58            don't send if <10 secs ago                   
                                                                                
GETWRK56 MVC   ATIMTMWT,RSTRTIM    save time this WTO                           
         GOTOR DMGR,DMCB,DMOPMSG,(L'ATIMLIT,ATIMLIT)                            
                                                                                
GETWRK58 XC    SAMPCNT,SAMPCNT                                                  
         XC    TOTQVAL,TOTQVAL                                                  
                                                                                
GETWRKX  J     RUNWRK              Go run the work                              
         EJECT                                                                  
***********************************************************************         
* No work found to run, wait for a wake-up call                       *         
***********************************************************************         
                                                                                
WAIT     GOTOR QMAINT              Maintain run queue                           
                                                                                
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO      Clear all access registers                   
                                                                                
         MVC   PMESSAGE(L'WAITMSG),WAITMSG                                      
         GOTOR PRTLOG                                                           
                                                                                
         LARL  R4,MQGLOB                                                        
         USING MQGLOB,R4           Point to MQ globals                          
                                                                                
***********************************************************************         
* Build ECB list and wait for something to get posted                 *         
***********************************************************************         
                                                                                
         MVC   ECBLOPR,AOPERECB    Set operator ECB                             
         MVI   ECBLOPR,0                                                        
         LA    R0,ECBWRK                                                        
         ST    R0,ECBLWRK          Set post ECB                                 
         LA    R0,ECBMQG                                                        
         ST    R0,ECBLMQG          Set MQ Get ECB                               
         MVC   ECBLDSP,ADSPCECB    Set global operator ECB                      
         MVI   ECBLDSP,ECBLASTQ                                                 
                                                                                
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,ASVR                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SAC   512                                                              
         MVI   TSFLAG,TSFAVAIL     Set this server is available                 
         MVI   WORKING,NOQ         and not working (shd be NOQ already)         
         XC    TSAQUE,TSAQUE       Clear command job entry                      
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
                                                                                
         LA    R1,ECBLIST                                                       
         WAIT  ECBLIST=(1)         Wait until something gets posted             
                                                                                
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,ASVR                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SAC   512                                                              
         MVI   TSFLAG,TSFACTIV     Set this server active running work          
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
                                                                                
         J     GETWRK              Look for work to do                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Run handling initialization                                         *         
***********************************************************************         
                                                                                
RUNWRK   LARL  RE,LP_XDB           Clear all values in LP_XD                    
         ST    RE,LP_ALPXD         (point to RUNNER version)                    
         LHI   RF,LP_AVMAP-LP_XD                                                
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         NI    LP_FLAG1,FF-LP_FDRFT  Clear incase last request died             
         MVI   TRAPNEST,0          Initialize trap nest level                   
         MVC   SVTRACE,MCTRACE     Save current trace value                     
         MVC   SVVPRINT,MCVPRINT   and A(PRINT)                                 
         L     RF,MCVLOGOC         Set infinite segment size                    
         MVC   LOGOSGMX-LOGOD(,RF),HOBOFF                                       
         MVC   RWRKBLK,RWRKBLK1    Set using work block 1                       
                                                                                
         TIME  BIN                                                              
         ST    R0,MCJOBSTM         Save start time                              
         L     RF,MCVLOGOC                                                      
         MVC   MCRUNPAG,LOGORUNP-LOGOD(RF)                                      
         MVC   MCRUNLIN,LOGORUNL-LOGOD(RF)                                      
         MVC   MCNXT,MCRTP         Reset next values                            
                                                                                
         MVI   ERROR,TQEGOOD                                                    
         L     RF,MCUTL            Set control system files                     
         MVI   4(RF),CONSYSQ                                                    
         MVI   MCOVSYS,0           Reset MCOVSYS if already set                 
                                                                                
         OC    WKEYZERO,WKEYZERO   Test for MQ message input                    
         JZ    RUNMQ               Yes                                          
                                                                                
***********************************************************************         
* Run handling for worker files                                       *         
***********************************************************************         
                                                                                
         NI    LP_FLAG2,FF-(LP_FMQIM+LP_FMQEQ+LP_FMQDR)                         
                                                                                
         L     R4,RWRKBLK          R4=A(worker control block)                   
         USING WRKIOD,R4                                                        
         XC    WRKINDX,WRKINDX                                                  
         MVC   WRKIFTYP,SVRFTYP    Set file type                                
         MVC   WRKIACTN,SVRFOPA    Set file open type                           
         CLI   WRKIACTN,0                                                       
         JNE   *+8                                                              
         MVI   WRKIACTN,WRKIAOPN   Default is to open for read                  
         MVI   WRKISTAT,0          Reset file status                            
         MVC   WRKWKEY,WKEY        Set file key                                 
         MVI   WORKING,YESQ        Set working mode on (treat abends as         
*                                    due to request, not RUNNER)                
         SR    RF,RF                                                            
         GOTOR GETUID,WRKWUSID     Resolve user-id values                       
         JE    *+12                                                             
         MVI   ERROR,TQEIUID       Set invalid user-id                          
         J     ENDREQ                                                           
                                                                                
         GOTOR PRTLOG                                                           
         GOTOR PRTKEY,WRKIOD       Format file key in work for printing         
         MVC   PMESSAGE(L'RUNLIT1),RUNLIT1                                      
         MVC   PMESSAGE+L'RUNLIT1(PRTKEYLQ),WORK                                
         GOTOR PRTLOG                                                           
                                                                                
         GOTOR SETMCV              Set master control values                    
                                                                                
         CLI   RUNTYPE,RUNTCMND    Test processing a command file               
         JE    RUNWRK04                                                         
                                                                                
         LARL  R0,SEOLST           Point to start of SEOLST                     
         ST    R0,RSEOLST                                                       
         CLC   WRKWSPS,MULTIGEN    Test general multi-file                      
         JE    RUNWRK04                                                         
         CLC   WRKWSPS,MULTICFM    Test CFM multi-file                          
         JE    RUNWRK04                                                         
                                                                                
         SR    R1,R1                                                            
         ICM   R1,1,SVRSYSN1                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MHI   R1,UIDSYSL                                                       
         LA    R1,UIDSYS-UIDSYSL(R1)                                            
UID      USING UIDSYS,R1                                                        
         CLI   UID.UIDSYSSE,0      Test SE number resolved                      
         JNE   RUNWRK02                                                         
         MVI   ERROR,TQEISYS       No - set invalid system                      
         J     ENDREQ                                                           
                                                                                
RUNWRK02 MVC   MCOVSYS,UID.UIDSYSN Set system values                            
         MVC   MCIDSENO,UID.UIDSYSSE                                            
         MVC   MCIDAGYB,UID.UIDSYSAB                                            
         MVC   MCIDACCS,UID.UIDSYSAC                                            
         DROP  UID                                                              
                                                                                
         CLI   SVRSYSN1,CONSYSQ    Test control system server                   
         JE    RUNWRK04                                                         
                                                                                
         GOTOR SETSYS              Set system values                            
         GOTOR RESSYS              Resolve system values                        
         JL    SYSNOP                                                           
         JE    RUNWRK04                                                         
         DC    H'0'                                                             
                                                                                
RUNWRK04 ICM   R1,7,REQNO          Bump request number                          
         AHI   R1,1                                                             
         STCM  R1,7,REQNO                                                       
                                                                                
         CLI   RUNTYPE,RUNTCMND    Test processing a command file               
         JNE   RUNWRK16                                                         
         MVI   WRKIFTYP,WRKIFTWF   Commands are always worker files             
         MVI   WRKIACTN,WRKIAOPN   Open work file and process                   
         GOTOR RWRKIO,WRKIOD                                                    
         JE    RUNWRK06                                                         
         MVC   PMESSAGE(L'CMDERR1),CMDERR1                                      
         GOTOR PRTLOG                                                           
         J     ENDREQ                                                           
                                                                                
RUNWRK06 MVI   WRKIACTN,WRKIAGET   Get next input command                       
         GOTOR RWRKIO,WRKIOD                                                    
         JNE   RUNWRK08                                                         
         L     RF,WRKIAREC                                                      
         AHI   RF,4                                                             
         USING LQ_D,RF                                                          
         CLI   LQ_EL,LQ_CMNDQ      Test this is a command card                  
         JNE   RUNWRK06            No - ignore                                  
         LLH   RE,LQ_LN                                                         
         SHI   RE,LQ_LN1Q+1                                                     
         MVC   CARD,SPACES                                                      
         EX    RE,RWRKMVCQ                                                      
         GOTOR VALPAR,CARD         Process command card                         
         JE    RUNWRK06                                                         
                                                                                
RUNWRK08 TM    RUNINDS1,RUNIGOMQ   Test have uploaded FALINK string             
         JNZ   GETWRK              Yes - process input stream                   
         MVI   DDLYNUM,0           ensure this is cleared                       
         J     ENDREQ04                                                         
                                                                                
RUNWRK16 GOTOR SETPOP              Set timer                                    
         GOTOR TSTCAN              Test cancel work                             
         MVI   WRKIACTN,WRKIAOPN                                                
         GOTOR RWRKIO,WRKIOD       Open the input file                          
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (RF),(R1)           Read the request record                      
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   WRKWKEY,WKEY                                                     
                                                                                
         L     RE,AREQSAVE         Last request                                 
         ICM   RF,3,0(RE)                                                       
         JZ    RUNWRK18            Can't copy if nothing there                  
         LARL  R0,LRQSAVE          Last request save record                     
         LHI   R1,REQSAVEL                                                      
         MVCL  R0,RE                                                            
                                                                                
RUNWRK18 L     R0,AREQSAVE         Clear the request save record                
         LHI   R1,REQSAVEL                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,WRKIAREC         Save the request record                      
         CLC   =Y(REQSAVEL),0(RE)  Trap corrupt worker record                   
         JL    *+2                 DIE - record too long                        
         LLH   RF,0(RE)                                                         
         L     R0,AREQSAVE                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   REQTRACE,0                                                       
         JE    RUNWRK20                                                         
         GOTOR LOGREQ,1            PRINT REQUEST DETAILS OF ALL request         
                                                                                
RUNWRK20 DS    0H                                                               
*&&US*&& GOTOR VDEMVER,DEMVERP,LP_D VSAM VERSION CONTROL                        
         GOTOR RGOLINK,RPRCWRKQ    Call DDLINK to process request               
         MVC   ERROR,LP_QERR       Save error return byte                       
         GOTOR TSTCAN              Test cancel work                             
         J     ENDREQ                                                           
                                                                                
RUNBAT   L     RF,MCVLOGOC         Set infinite segment size                    
         MVC   LOGOSGMX-LOGOD(,RF),HOBOFF                                       
         MVC   RWRKBLK,RWRKBLK1    Set using work block 1                       
         TIME  BIN                                                              
         ST    R0,MCJOBSTM         Save start time                              
         TIME  TU                  Get time now                                 
         ST    R0,RARRTIM          Set as arrival and start time                
         ST    R0,RSTRTIM                                                       
         GOTOR SETMCV              Set master control values                    
         GOTOR SETPOP              Set timer                                    
         MVI   REQNO+L'REQNO-1,1                                                
*&&US*&& GOTOR VDEMVER,DEMVERP,LP_D VSAM VERSION CONTROL                        
         GOTOR RGOLINK,RPRCREQQ    Pass process request mode to server          
         J     ENDREQ                                                           
         EJECT                                                                  
***********************************************************************         
* Run handling for MQ messages                                        *         
***********************************************************************         
                                                                                
RUNMQ    LARL  R4,MQGLOB           Point to MQ globals                          
         USING MQGLOB,R4                                                        
         OI    RUNINDMQ,RUNIMQUP   Set we are running MQ upload                 
         OI    LP_FLAG2,LP_FMQIM   Set MQ message in for DDLINK                 
                                                                                
         MVC   PMESSAGE(L'INFLITB),INFLITB                                      
         L     R1,MQIBUFF                                                       
         SAM31 ,                                                                
         MVC   PMESSAGE+L'INFLITB(L'PMESSAGE-L'INFLITB),0(R1)                   
         SAM24 ,                                                                
         GOTOR PRTLOG              Print log entry                              
         L     R1,MQIBUFF                                                       
         SAM31 ,                                                                
         MVC   PMESSAGE+L'INFLITB(L'PMESSAGE-L'INFLITB),L'PMESSAGE-L'IN*        
               FLITB(R1)                                                        
         SAM24 ,                                                                
         GOTOR PRTLOG              PRINT LOG ENTRY LINE 2                       
         GOTOR MQTRC,0             Trace MQ input message                       
                                                                                
         GOTOR RGOLINK,RPRCMQMQ    Extract MQ control information               
         CLI   DDLYNUM,0           Test retrying MQ deadly embrace              
         JNE   RUNMQ02             no ACK-02 if so.                             
         GOTOR RMQI,DMCB,('MQACK02Q',MQACK02),L'MQACK02 - Send ACK-02           
RUNMQ02  BRAS  RE,MQPUTRET         Put return address                           
                                                                                
         MVI   WORKING,YESQ        Set working mode on (treat abends as         
*                                    due to request, not RUNNER)                
         L     RF,MCUTL            Set control system files                     
         MVI   4(RF),CONSYSQ                                                    
         SR    RF,RF                                                            
         GOTOR GETUID,LP_USRID     Resolve user-id values                       
         JE    *+6                                                              
         DC    H'0'                Credential check failed                      
                                                                                
         GOTOR SETSYS              Set system values                            
         GOTOR RESSYS              Resolve system values                        
         JE    *+6                                                              
         DC    H'0'                System invalid or not open                   
                                                                                
         MVI   MQFLAG,MQFFRST      Set first time for MQ                        
         XC    RVALUES(RVALUESL),RVALUES                                        
         XC    CPUREQ,CPUREQ                                                    
         TIME  TU                                                               
         ST    R0,RSTRTIM          Set start time                               
         ST    R0,RARRTIM                                                       
                                                                                
         TM    RUNINDMQ,RUNIMQCI   Test message id given                        
         JZ    RUNMQ06             No                                           
                                                                                
         LAM   AR3,AR3,ALET                                                     
         ICM   R3,15,AQUE                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SAC   512                                                              
         GOTOR UPDQUE,WAITPROC     Set this work is being run                   
         MVI   TQQFLAG,0           Reset QMAINT flag (for SYSNOP)               
         MVC   RARRTIM,TQATIME     Set arrival time                             
         LAM   AR3,AR3,ARZERO                                                   
         SAC   0                                                                
                                                                                
RUNMQ06  ICM   R1,7,REQNO          Bump request number                          
         AHI   R1,1                                                             
         STCM  R1,7,REQNO                                                       
                                                                                
         MVC   PMESSAGE(L'INFLITC),INFLITC                                      
         TM    RUNINDMQ,RUNIMQCI   Test message id given                        
         JNZ   *+14                No                                           
         MVC   PMESSAGE+L'INFLITC(3),=C'n/a'                                    
         J     RUNMQ08                                                          
         GOTOR MCVHEXOU,DMCB,MQMDESC_MSGID,                            +        
               PMESSAGE+L'INFLITC,L'TQMQCTRL,0                                  
                                                                                
RUNMQ08  GOTOR PRTLOG              Print log entry                              
         GOTOR SETMCV              Set master control values                    
*        LARL  R2,FIVSECND                                                      
*        STIMER WAIT,BINTVL=(2)    Wait for testing                             
         GOTOR SETPOP              Set timer                                    
*&&US*&& GOTOR VDEMVER,DEMVERP,LP_D VSAM VERSION CONTROL                        
         GOTOR RGOLINK,RPRCMQDQ    Process the MQ input stream                  
         MVC   ERROR,LP_QERR       Save error return byte                       
         GOTOR TSTCAN              Test cancel work                             
         J     ENDREQ              Go do end of request stuff                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* MQ PUT RETURN ADDRESS OF REQUESTOR INTO DMGR DATASPACE              *         
* USED TO RETURN ERROR CONDITION IF RUNNER HAS FAILED                 *         
***********************************************************************         
MQPUTRET NTR1                                                                   
*                                                                               
         LARL  RF,SSB                                                           
         LAM   AR2,AR2,SSOALET-SSOOFF(RF)                                       
         SR    R2,R2                                                            
         USING DMDHDR,R2                                                        
         SAC   512                                                              
         L     R2,DHAMQRTN         SET R2 TO MQ BLOCK                           
         LHI   RE,DSMQRTMX         UP TO 512                                    
*                                                                               
         L     RF,=F'-1'           SET TO FFS TO GRAB ENTRY                     
         SR    R1,R1                                                            
MQPUT010 CS    R1,RF,0(R2)         SWAP IN FFFFFFFF                             
         JE    MQPUT020                                                         
         LA    R2,L'DSMQRETK(,R2)                                               
         JCT   RE,MQPUT010         DIE IF ALL ENTRIES FULL                      
         DC    H'0'                                                             
*                                                                               
MQPUT020 L     R1,MQOBUFF                                                       
         SAM31                                                                  
         MVC   0(L'DSMQRETK,R2),0(R1)  MOVE IN MQ RETURN ADDR                   
         SAM24                                                                  
         ST    R2,FULL                                                          
*                                                                               
         ICM   R2,15,ADSJOB        GOT ENTRY ADDR ALREADY                       
         JNZ   MQPUT050                                                         
*                                                                               
         SR    R2,R2                                                            
         L     R2,DHAJOBS          SET R2 TO JOBS BLOCK                         
         LA    RE,127                                                           
         USING DSJOBD,R2                                                        
MQPUT030 CLC   DSJASID,ASID        FIND THIS RUNNER                             
         JE    MQPUT050                                                         
         LA    R2,32(,R2)                                                       
         JCT   RE,MQPUT030                                                      
         DC    H'0'                                                             
*                                                                               
MQPUT050 MVC   DSJMQRET,FULL       SAVE A(MQ RETURN)                            
         ST    R2,ADSJOB                                                        
         SAC   0                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* System is not open - add entry to NOTLST and look for more work if  *         
* client is running disconnected, else return system not open error   *         
***********************************************************************         
                                                                                
SYSNOP   LAM   AR3,AR3,ALET                                                     
         ICM   R3,15,AQUE                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SAC   512                                                              
                                                                                
         GOTOR UPDQUE,PROCWAIT     Set work waiting to run                      
         MVI   TQQFLAG,TQQSNOOP    Set SYSNOP for QMAINT routine                
         XC    TQASVR,TQASVR       Clear server lockword in queue entry         
         LLH   R1,NOTLSTN                                                       
         CHI   R1,NOTLSTM          Test maximum N'entries reached               
         JL    *+6                                                              
         SR    R1,R1               Yes - rebuild the not list                   
         LA    R0,1(R1)                                                         
         STCM  R0,3,NOTLSTN        Set N'entries in the not list                
         MHI   R1,L'TQWKEY                                                      
         AHI   R1,NOTLST-WORKD                                                  
         LA    R1,WORKD(R1)        Point to next entry in NOTLST                
         MVC   0(L'TQWKEY,R1),TQWKEY                                            
         ST    R1,FULL2            Save A(worker key)                           
                                                                                
         SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
                                                                                
         MVC   WORK(80),SPACES     Build notification list                      
         MVC   WORK(L'QMLIST),QMLIST                                            
         OC    ABEND,ABEND         Test abend parameter supplied                
         JZ    *+12                                                             
         LA    RE,ABEND            Yes - this overrides server list             
         J     *+12                                                             
         ICM   RE,15,SVRABND       Test server abend list resolved              
         JZ    SYSNOP04                                                         
         LA    R1,WORK+L'QMLIST-1                                               
         MVI   0(R1),C','                                                       
SYSNOP02 AHI   R1,1                                                             
         MVC   0(1,R1),0(RE)                                                    
         CLI   0(RE),C' '          Test blank terminator                        
         JNE   *+8                                                              
         MVI   0(RE),C':'                                                       
         CLI   0(RE),C':'          Test end of abend notification list          
         JE    SYSNOP04                                                         
         AHI   RE,1                                                             
         J     SYSNOP02                                                         
                                                                                
SYSNOP04 MVC   P,SPACES                                                         
         MVC   P(L'MCSTEP),MCSTEP                                               
         MVC   P+L'MCSTEP+1(L'SNOPLIT),SNOPLIT                                  
         GOTOR VSMTP,DMCB,('SMTPCNCL',0)                                        
         GOTOR (RF),(R1),('SMTPAPRS',WORK),(70,P)                               
         MVC   P,SPACES                                                         
                                                                                
         L     R2,RWRKBLK1                                                      
         USING WRKIOD,R2           Format worker file key                       
         L     R1,FULL2                                                         
         MVC   WRKWKEY,0(R1)                                                    
         MVI   WRKIFTYP,WRKIFTWF                                                
         SR    RF,RF                                                            
         GOTOR GETUID,WRKWUSID     Resolve user-id for PRTKEY routine           
         GOTOR PRTKEY,WRKIOD                                                    
         DROP  R2                                                               
                                                                                
         MVC   P,SPACES            Build 'System=x,Worker key=y'                
         MVC   P(L'SYSTLIT),SYSTLIT                                             
         L     R1,RSEOLST                                                       
         MVC   P+L'SYSTLIT(L'SEOLNAM),SEOLNAM-SEOLSTD(R1)                       
         LA    RE,P+L'SYSTLIT+L'SEOLNAM                                         
         CLI   0(RE),C' '                                                       
         JNE   *+8                                                              
         JCT   RE,*-8                                                           
         MVC   1(L'WKEYLIT,RE),WKEYLIT                                          
         AHI   RE,1+L'WKEYLIT                                                   
         MVC   0(PRTKEYLQ,RE),WORK                                              
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
         MVC   P,SPACES                                                         
         GOTOR (RF),(R1),('SMTPASND',0)                                         
         J     NXTWRK                                                           
         EJECT                                                                  
***********************************************************************         
* Work complete - post completion                                     *         
***********************************************************************         
                                                                                
ERRRUN   MVI   MCBLOW,C'X'         Set to exit after clean-up                   
         J     ERRREQ02                                                         
                                                                                
ERRREQ   MVI   MCBLOW,C'R'         Set MCBLOW value                             
         XC    MCLSTPSW,MCLSTPSW   Clear saved psw value                        
                                                                                
ERRREQ02 TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   ENDRUN04                                                         
         TM    RUNINDMQ,RUNIMQUP   Test running MQ upload                       
         JNZ   ERRMQ                                                            
         MVI   DDLYNUM,0           No deadly retry if not MQ                    
         L     R4,RWRKBLK          Point to WRKIO block                         
         USING WRKIOD,R4                                                        
         TM    WRKISTAT,WRKISOPN+WRKISUPD                                       
         JNZ   ERRREQ04                                                         
         MVI   WRKIACTN,WRKIAAPP   Re-open worker file                          
         GOTOR RWRKIO,WRKIOD                                                    
                                                                                
ERRREQ04 MVI   WRKIACTN,WRKIACLE   Close file and set in error                  
         GOTOR RWRKIO,WRKIOD                                                    
         J     ENDREQ                                                           
         DROP  R4                                                               
                                                                                
ERRMQ    CLI   ERROR,TQEDDLY                                                    
         JNE   ERRMQ02                                                          
         CLI   DDLYNUM,0           Retry request if deadly                      
         JNE   RTRYREQ                                                          
ERRMQ02  MVC   WORK(L'MQERRM),MQERRM                                            
         GOTOR GETTQE,ERROR                                                     
         MVC   WORK+L'MQERRM(L'TQETAB-1),1(RF)                                  
         GOTOR RMQI,DMCB,('MQERRQ',WORK),L'MQERRM+L'TQETAB-1                    
         EJECT                                                                  
ENDREQ   MVI   DDLYNUM,0           Clear deadly embrace retry                   
RTRYREQ  GOTOR TSUM                Do trace summary                             
         LHI   RF,C'K'                                                          
         GOTOR VISGENQ,DMCB,(RF),ALL,0                                          
         GOTOR GETCPU                                                           
         LG    GR1,CPUSAVE                                                      
         DSGF  GR0,F10000                                                       
         LR    R3,R1               GET ELAPSED JOB STEP TIMING (SEC)            
         S     R1,MCRUNTCB                                                      
         ST    R1,MCREQTCB                                                      
         TIME  BIN                 R0=current time (in secs*100)                
         LR    R2,R0               Save current time                            
         S     R0,MCRUNLPS                                                      
         ST    R0,MCREQLPS                                                      
         L     RF,MCVLOGOC                                                      
         USING LOGOD,RF                                                         
         L     R0,LOGORUNL                                                      
         S     R0,MCRUNLIN                                                      
         ST    R0,MCREQLIN                                                      
         ST    R0,RTOTLIN                                                       
         MVC   MCRUNLIN,LOGORUNL                                                
         L     R0,LOGORUNP                                                      
         S     R0,MCRUNPAG                                                      
         ST    R0,MCREQPAG                                                      
         ST    R0,RTOTPAG                                                       
         MVC   MCRUNPAG,LOGORUNP                                                
         L     R0,MCACTIOS                                                      
         S     R0,MCRUNSIO                                                      
         ST    R0,MCREQSIO                                                      
         MVC   MCRUNSIO,MCACTIOS                                                
         ST    R2,MCRUNLPS                                                      
         ST    R3,MCRUNTCB                                                      
         DROP  RF                                                               
                                                                                
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   ENDREQ02                                                         
         MVC   MCTRACE,SVTRACE     Restore trace and V(PRINT)                   
         MVC   MCVPRINT,SVVPRINT                                                
         J     ENDREQ04                                                         
                                                                                
ENDREQ02 GOTOR PRTLOG              Print 'ending work' log entry                
         L     R1,RWRKBLK                                                       
         GOTOR PRTKEY                                                           
         MVC   PMESSAGE(L'RUNLITB),RUNLITB                                      
         MVC   PMESSAGE+L'RUNLITB(PRTKEYLQ),WORK                                
         GOTOR PRTLOG                                                           
                                                                                
ENDREQ04 TTIMER CANCEL,MIC,DUB     Reset timer                                  
         LG    GR1,DUB             Get remaining time                           
         SRAG  GR1,GR1,12          Bit 51 =sec/1000000, shift to bit 63         
         DSGF  GR0,F10000          convert to sec/100                           
         ICM   R0,15,CPURTP        Accumulate RTP time                          
         AR    R0,R1                                                            
         STCM  R0,15,CPURTP                                                     
                                                                                
         TIME  TU                                                               
         ST    R0,RENDTIM          Set end time                                 
                                                                                
         TM    RUNINDS1,RUNIUPDT   TEST UPDATIVE (GLOBAL) MODE                  
         JZ    ENDREQ05                                                         
*        MVC   SAVESIN,SSOSIN                                                   
         GOTOR DMGR,DMCB,(0,DMCOMMIT),0 Yes, ensure committed                   
*        XC    SSOSIN,SSOSIN                                                    
                                                                                
         CLI   MCBLOW,NOQ          Did we abend?                                
         JE    ENDREQ05                                                         
                                                                                
         XC    DMCB,DMCB           Yes, see if we have a system lock            
         MVI   DMCB,X'20'          Enquire on system                            
         MVC   DMCB+3(1),ABENDSE                                                
         GOTO1 VLOCKSPC,DMCB                                                    
                                                                                
         USING DMSPACED,RF         (returns local copy of dataspace)            
         L     RF,4(,R1)                                                        
         OC    DSPLOCK,DSPLOCK     Test any lock on it                          
         JZ    ENDREQ05                                                         
                                                                                
         CLC   DSPMVS,MCJOB        Yes, is it ours?                             
         JNE   ENDREQ05                                                         
         CLC   DSPJOB,MVSJNUM                                                   
         JNE   ENDREQ05                                                         
                                                                                
         MVC   PMESSAGE(L'INFLITD),INFLITD Yes, so we have to free it.          
         MVC   PMESSAGE+L'INFLITD+1(L'DSPNAME),DSPNAME                          
         DROP  RF                                                               
         GOTOR PRTLOG                                                           
         XC    DMCB,DMCB                                                        
         MVI   DMCB,X'11'          Force free                                   
         MVC   DMCB+3(1),ABENDSE                                                
         GOTO1 VLOCKSPC,DMCB                                                    
                                                                                
ENDREQ05 CLI   REQTRACE,0          PRINT REQUEST DETAILS OF BIG ONES            
         JNE   ENDREQ06            IF NOT ALREADY PRINTED                       
         GOTOR LOGREQ,0                                                         
ENDREQ06 LARL  RF,SSB                                                           
         MVC   SAVESIN,SSOSIN-SSOOFF(RF)                                        
         XC    SSOSIN-SSOOFF(,RF),SSOSIN-SSOOFF(RF)                             
                                                                                
         LA    R2,MCWORK                                                        
         USING RUNSMFD,R2          Build RUNNER SMF record                      
         XC    RUNSMFD(RUNSMAXL),RUNSMFD                                        
         LHI   RF,RUNSMFL                                                       
         STCM  RF,3,RUNSRLEN       Set SFM record length                        
         MVI   RUNSSMFT,RUNSSRUN   Set 'RUNNER' SMF record                      
         MVC   RUNSREQ#,REQNO                                                   
         MVC   RUNSSENO,MCIDSENO                                                
         MVI   RUNSTYPE,RUNSTDLD   Set 'download'                               
         TM    LP_INDS,LP_IOUPD                                                 
         JZ    *+8                                                              
         MVI   RUNSTYPE,RUNSTULD   Set 'upload'                                 
         MVI   RUNSDEST,RUNSDWK    Set destination to 'WKFILE'                  
         OC    RMQBADD,RMQBADD                                                  
         JZ    *+8                                                              
         MVI   RUNSDEST,RUNSDMQ    Set destination to 'MQ'                      
         MVC   RUNSATIM,RARRTIM                                                 
         MVC   RUNSSTIM,RSTRTIM                                                 
         MVC   RUNSETIM,RENDTIM                                                 
         MVC   RUNSAGYA,MCIDAGYA                                                
         MVC   RUNSUSID,MCUSERID                                                
         LARL  RF,SEC                                                           
         MVC   RUNSPIDC,SECPID-SECD(RF)                                         
         MVC   RUNSFPID,MCFACPAK                                                
         L     RF,LP_ALPXD                                                      
         USING LP_XD,RF                                                         
         MVC   RUNSAPID,LP_XPINF+(TXPNUM-TXPINFO)                               
         MVC   RUNSVRSN,LP_XPINF+(TXPVER-TXPINFO)                               
         MVC   RUNSOSIN,LP_OSIN                                                 
         DROP  RF                                                               
         L     RF,RWRKBLK                                                       
         MVC   RUNSRQID,WRKWSPS-WRKIOD(RF)                                      
         GOTOR MCVHEXOU,DMCB,LP_QMAPN,RUNSREQM,L'LP_QMAPN                       
         MVC   RUNSSIOS,RTOTSIO                                                 
*                                                                               
         LG    GR1,CPUREQ          Get request CPU time                         
         DSGF  GR0,F10000          Convert to old value                         
         STCM  R1,15,RUNSCPUX                                                   
*                                                                               
         MVC   RUNSCPUT,CPUREQ     Set DDLINK/server total CPU units            
         LG    GR1,CPUSAVE         Get CPU on entry to ENDREQ                   
         SG    GR1,CPUENDRQ        Less CPU at same point on last req           
         SG    GR1,CPUREQ          Less DDLINK/server CPU                       
         STG   GR1,DUB             Gives overhead CPU                           
         MVC   RUNSCPUJ,DUB                                                     
         MVC   CPUENDRQ,CPUSAVE                                                 
         MVC   RUNSCOMP,ERROR                                                   
                                                                                
         OC    RMQBADD,RMQBADD     Test output to MQ                            
         JZ    ENDREQ07            No                                           
         MVC   RUNSOUTR,RMQRADD                                                 
         MVC   RUNSOUTS,RMQBSIZ                                                 
         MVC   RUNSSEGS,RMQBADD+2                                               
         J     ENDREQ08                                                         
                                                                                
ENDREQ07 MVC   RUNSOUTR,RWRKADD                                                 
         MVC   RUNSOUTS,RWRKSIZ                                                 
         MVC   RUNSSEGS,RFILSEG+2                                               
                                                                                
ENDREQ08 TM    RUNINDS2,RUNIRIDF   No SMF record if running under IDF           
         JNZ   ENDREQ10                                                         
         GOTOR RSMFOUT,PARM,10,RUNSMFD                                          
         LARL  RF,SSB              CLEAR PERF DATA SO NOT CUMULATIVE            
         XC    SSOPERF1-SSOOFF(,RF),SSOPERF1-SSOOFF(RF)                         
         XC    SSOPERF2-SSOOFF(,RF),SSOPERF2-SSOOFF(RF)                         
                                                                                
ENDREQ10 TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   ENDREQ36                                                         
         TM    RUNINDMQ,RUNIMQUP   Test MQ upload                               
         JNZ   ENDREQ32                                                         
                                                                                
         MVC   PMESSAGE(RUNLITCL),RUNLITC                                       
         L     RF,LP_ALPXD                                                      
         USING LP_XD,RF                                                         
         OC    LP_LUID,LP_LUID     Set terminal VTAM LUID if known              
         JZ    *+10                                                             
         MVC   PMESSAGE+(RLCLUID-RUNLITC)(L'RLCLUID),LP_LUID                    
         DROP  RF                                                               
                                                                                
         LARL  RE,FACIDTAB                                                      
ENDREQ16 CLC   MCFACPAK,7(RE)      Look up FACPAK id                            
         JE    ENDREQ18                                                         
         CLI   0(RE),FF            Test end of FACPAK table                     
         JE    *+12                                                             
         AHI   RE,L'FACIDTAB       No - bump to next                            
         J     ENDREQ16                                                         
         LARL  RE,FACIDTAB         Yes - point to unknown entry                 
ENDREQ18 MVC   PMESSAGE+(RLCFACID-RUNLITC)(L'RLCFACID),0(RE)                    
         LARL  RF,SEC                                                           
         MVC   PMESSAGE+(RLCAGY-RUNLITC)(L'RLCAGY),SECOAGY-SECD(RF)             
         LA    R1,PMESSAGE+RUNLITCL-1                                           
         CLI   0(R1),C' '          Attach PID to message                        
         JNE   *+8                                                              
         JCT   R1,*-8                                                           
         MVC   1(5,R1),=C',PID='                                                
         MVC   6(L'SECPID,R1),SECPID-SECD(RF)                                   
                                                                                
         GOTOR PRTLOG                                                           
                                                                                
         L     RF,LP_ALPXD                                                      
         USING LP_XD,RF                                                         
         USING TXPINFO,LP_XPINF                                                 
                                                                                
         OC    TXPNUM,TXPNUM       Test external program resolved               
         JZ    ENDREQ32                                                         
         MVC   PMESSAGE(L'RUNLITD),RUNLITD                                      
*&&US                                                                           
         LARL  R1,APPTAB           Look-up application name in table            
         LHI   R0,APPTABN                                                       
ENDREQ20 CLC   TXPNUM,0(R1)                                                     
         JE    ENDREQ22                                                         
         LA    R1,APPTABL(,R1)                                                  
         JCT   R0,ENDREQ20                                                      
         LA    R1,PMESSAGE+L'RUNLITD-1                                          
         J     ENDREQ24                                                         
                                                                                
ENDREQ22 MVC   PMESSAGE+L'RUNLITD(APPNAML),L'TXPNUM(R1)                         
         LA    R1,PMESSAGE+L'RUNLITD+APPNAML-1                                  
*&&                                                                             
*&&UK                                                                           
         L     R1,=V(FAXPTPC)      Look-up application name in FAXPTAB          
         USING FAXPTABD,R1                                                      
ENDREQ20 CLI   FAXPTYPE,0                                                       
         JE    ENDREQ24                                                         
         CLC   TXPNUM,FAXPNUM                                                   
         JE    ENDREQ22                                                         
         LA    R1,L'FAXPNTRY(,R1)                                               
         J     ENDREQ20                                                         
                                                                                
ENDREQ22 MVC   PMESSAGE+L'RUNLITD(L'FAXPNEXE),FAXPNEXE                          
         DROP  R1                                                               
         LA    R1,PMESSAGE+L'RUNLITD+L'FAXPNEXE-1                               
*&&                                                                             
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,*-8                                                           
*&&US*&& J     ENDREQ26            UK shows number too                          
                                                                                
ENDREQ24 LLH   R0,TXPNUM           Not defined - show number                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVI   1(R1),C'#'                                                       
         UNPK  2(4,R1),DUB                                                      
         LA    R1,4+1(,R1)                                                      
                                                                                
ENDREQ26 MVC   1(L'RUNLITE,R1),RUNLITE                                          
         AHI   R1,L'RUNLITE+1                                                   
         ICM   RF,B'1110',TXPVER                                                
         LHI   R0,2                                                             
         DROP  RF                                                               
                                                                                
ENDREQ28 SR    RE,RE                                                            
         SLDL  RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(2,R1),DUB                                                      
         CLI   0(R1),C'0'                                                       
         JNE   *+10                                                             
         MVC   0(2,R1),1(R1)                                                    
         AHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JE    *+8                                                              
         AHI   R1,1                                                             
         MVI   0(R1),C'.'                                                       
         AHI   R1,1                                                             
         JCT   R0,ENDREQ28                                                      
                                                                                
         LHI   R0,2                                                             
ENDREQ30 SR    RE,RE                                                            
         SLDL  RE,8                                                             
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  0(3,R1),DUB                                                      
         MVI   3(R1),C'.'                                                       
         CLI   0(R1),C'0'                                                       
         JNE   *+10                                                             
         MVC   0(4,R1),1(R1)                                                    
         CLI   0(R1),C'0'                                                       
         JNE   *+10                                                             
         MVC   0(3,R1),1(R1)                                                    
         CLI   0(R1),C' '                                                       
         JE    *+12                                                             
         AHI   R1,1                                                             
         J     *-12                                                             
         JCT   R0,ENDREQ30                                                      
         BCTR  R1,0                                                             
         MVI   0(R1),C' '                                                       
         GOTOR PRTLOG                                                           
                                                                                
ENDREQ32 CLI   RUNTYPE,RUNTSRVR    Don't accumulate command files               
         JNE   ENDREQ34                                                         
         CLI   ERROR,TQEGOOD       And requests in error                        
         JNE   ENDREQ34                                                         
                                                                                
         L     R1,TNUMREQ                                                       
         AHI   R1,1                                                             
         ST    R1,TNUMREQ          Increment total requests                     
                                                                                
         L     R0,RSTRTIM                                                       
         SL    R0,RARRTIM                                                       
         A     R0,TQUETIM                                                       
         ST    R0,TQUETIM          Accumulate total queue time                  
                                                                                
         L     R0,RENDTIM                                                       
         SL    R0,RARRTIM                                                       
         A     R0,TTOTTIM                                                       
         ST    R0,TTOTTIM          Accumulate total elapsed time                
                                                                                
         L     R0,RTOTSIO                                                       
         A     R0,TTOTSIO                                                       
         ST    R0,TTOTSIO          Accumulate total I/Os                        
                                                                                
         LG    GR0,TTOTCPU                                                      
         AG    GR0,CPUREQ                                                       
         STG   GR0,TTOTCPU         Accumulate total CPU time                    
                                                                                
         L     R0,RWRKADD                                                       
         A     R0,TWRKADD                                                       
         ST    R0,TWRKADD          Accumulate total output records              
                                                                                
         L     R0,RMQRADD                                                       
         A     R0,TWRKADD                                                       
         ST    R0,TWRKADD                                                       
                                                                                
ENDREQ34 SR    R0,R0                                                            
         ICM   R0,7,REQNO                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RL2REQS,DUB                                                      
         GOTOR MCVHEXOU,DMCB,LP_QMAPN,RL2MAPNO,L'LP_QMAPN                       
         MVC   PMESSAGE(RUNLIT2L),RUNLIT2                                       
         LA    R3,PMESSAGE+RUNLIT2L                                             
         OC    SAVESIN,SAVESIN                                                  
         JZ    ENDREQ35                                                         
         MVC   0(5,R3),=C',SIN='                                                
         ICM   RE,15,SAVESIN        SSOSIN                                      
         NILH  GRE,X'00FF'                                                      
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  5(8,R3),DUB                                                      
         XC    SAVESIN,SAVESIN      Clear it now                                
         LA    R3,13(,R3)                                                       
ENDREQ35 MVC   0(7,R3),=C',Error='                                              
         LLC   R0,ERROR                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  7(3,R3),DUB                                                      
         MVI   10(R3),HYPHEN                                                    
         GOTOR GETTQE,ERROR                                                     
         MVC   11(L'TQETAB-1,R3),1(RF)                                          
         GOTOR PRTLOG              Print completion message                     
                                                                                
ENDREQ36 LG    GR1,CPUREQ          Print request statistics                     
         DSGF  GR0,F100                                                         
         MVC   RL3CPUT,MAXCPUTL    Display '*High*'                             
         CGF   GR1,MAXCPUT         Test it fits                                 
         JH    ENDREQ37            No                                           
         CVD   R1,DUB              CPU time                                     
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RL3CPUT,DUB                                                      
         MVC   RL3CPUT(L'RL3CPUT-5),RL3CPUT+1                                   
         MVI   RL3CPUT+L'RL3CPUT-5,C'.'                                         
                                                                                
ENDREQ37 ICM   R0,15,RTOTSIO       Start IOs                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RL3SIOS,DUB                                                      
         ICM   R0,15,RTOTPOP                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RL3POPS,DUB                                                      
         MVC   PMESSAGE(RUNLIT3L),RUNLIT3                                       
         GOTOR PRTLOG                                                           
                                                                                
         TM    RUNINDMQ,RUNIMQUP   Test MQ upload                               
         JNZ   ENDREQ46                                                         
                                                                                
         ICM   R0,15,RLOGADD       Worker file output                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RL4LOGR,DUB                                                      
         ICM   R0,15,RWRKADD                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RL4RECS,DUB                                                      
         MVC   RL4SIZE,SPACES                                                   
         MVI   WORK,BYTESIZE                                                    
         ICM   R1,15,RWRKSIZ                                                    
         LHI   RF,100                                                           
         MR    R0,RF                                                            
         LHI   RF,146              Actual file size to download factor          
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JZ    *+8                 Yes - adjust download factor                 
         LHI   RF,136                                                           
         DR    R0,RF                                                            
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         CHI   R0,1024                                                          
         JNH   ENDREQ38                                                         
         SRDL  R0,10                                                            
         SRL   R1,32-10                                                         
         MVI   WORK,KILOSIZE                                                    
         CHI   R0,1024                                                          
         JNH   ENDREQ38                                                         
         SRDL  R0,10                                                            
         SRL   R1,32-10                                                         
         MVI   WORK,MEGASIZE                                                    
                                                                                
ENDREQ38 LTR   R0,R0                                                            
         JNZ   *+14                                                             
         MVC   RL4SIZE(L'ZERO),ZERO                                             
         J     ENDREQ42                                                         
                                                                                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RL4SIZE(4),DUB                                                   
         LA    RE,4                                                             
         CLI   RL4SIZE,C'0'                                                     
         JNE   *+14                                                             
         MVC   RL4SIZE(4),RL4SIZE+1                                             
         JCT   RE,*-14                                                          
         LA    RE,RL4SIZE(RE)                                                   
         LTR   R1,R1                                                            
         JZ    ENDREQ40                                                         
         SR    R0,R0                                                            
         MHI   R1,1000                                                          
         LHI   RF,1024                                                          
         DR    R0,RF                                                            
         AHI   R1,5                                                             
         SR    R0,R0                                                            
         LHI   RF,10                                                            
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         JZ    ENDREQ40                                                         
         CHI   R1,99                                                            
         JNH   *+8                                                              
         LHI   R1,99                                                            
         MVI   0(RE),C'.'                                                       
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  1(2,RE),DUB                                                      
         AHI   RE,3                                                             
ENDREQ40 MVC   0(1,RE),WORK                                                     
                                                                                
ENDREQ42 MVC   PMESSAGE(RUNLIT4L),RUNLIT4                                       
         GOTOR PRTLOG                                                           
                                                                                
         ICM   R0,15,RFILSEG                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RL5SEGS,DUB                                                      
         OC    RFILSEG,RFILSEG                                                  
         JNZ   *+10                                                             
         MVC   RL5SEGS,NONE                                                     
         MVC   RL5ASNC,NO                                                       
         TM    SVRIND1,RSVRSEGS                                                 
         JZ    ENDREQ44                                                         
         CLI   LP_FAVER,VERSION5                                                
         JL    ENDREQ44                                                         
         MVC   RL5ASNC,YES                                                      
ENDREQ44 MVC   PMESSAGE(RUNLIT5L),RUNLIT5                                       
         GOTOR PRTLOG                                                           
         J     ENDREQ60                                                         
                                                                                
ENDREQ46 L     R0,RMQRADD                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RLJRECS,DUB                                                      
         L     R0,RMQBADD                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RLJBUFS,DUB                                                      
                                                                                
         MVC   RLJSIZE,SPACES                                                   
         MVI   WORK,BYTESIZE                                                    
         L     R0,RMQBSIZ                                                       
         XR    R1,R1                                                            
         CHI   R0,1024                                                          
         JNH   ENDREQ56                                                         
         SRDL  R0,10                                                            
         SRL   R1,32-10                                                         
         MVI   WORK,KILOSIZE                                                    
         CHI   R0,1024                                                          
         JNH   ENDREQ56                                                         
         SRDL  R0,10                                                            
         SRL   R1,32-10                                                         
         MVI   WORK,MEGASIZE                                                    
                                                                                
ENDREQ56 CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RLJSIZE(4),DUB                                                   
         LHI   RE,4                                                             
         CLI   RLJSIZE,C'0'                                                     
         JNE   *+14                                                             
         MVC   RLJSIZE(4),RLJSIZE+1                                             
         JCT   RE,*-14                                                          
         LA    RE,RLJSIZE(RE)                                                   
         LTR   R1,R1                                                            
         JZ    ENDREQ58                                                         
         SR    R0,R0                                                            
         MHI   R1,1000                                                          
         LHI   RF,1024                                                          
         DR    R0,RF                                                            
         AHI   R1,5                                                             
         SR    R0,R0                                                            
         LHI   RF,10                                                            
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         JZ    ENDREQ58                                                         
         CHI   R1,99                                                            
         JNH   *+8                                                              
         LHI   R1,99                                                            
         MVI   0(RE),C'.'                                                       
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  1(2,RE),DUB                                                      
         AHI   RE,3                                                             
ENDREQ58 MVC   0(1,RE),WORK                                                     
         MVC   PMESSAGE(RUNLITJL),RUNLITJ                                       
         GOTOR PRTLOG                                                           
                                                                                
ENDREQ60 ICM   R0,15,RARRTIM                                                    
         GOTOR EDTTUS,RL7ATIM                                                   
         ICM   R0,15,RSTRTIM                                                    
         GOTOR EDTTUS,RL7STIM                                                   
         ICM   R0,15,RENDTIM                                                    
         GOTOR EDTTUS,RL7ETIM                                                   
         MVC   PMESSAGE(RUNLIT7L),RUNLIT7                                       
         GOTOR PRTLOG                                                           
                                                                                
         ICM   R0,15,RSTRTIM                                                    
         SL    R0,RARRTIM                                                       
         GOTOR EDTTUS,RL8QTIM                                                   
         ICM   R0,15,RENDTIM                                                    
         SL    R0,RARRTIM                                                       
         GOTOR EDTTUS,RL8ETIM                                                   
         MVC   PMESSAGE(RUNLIT8L),RUNLIT8                                       
         GOTOR PRTLOG                                                           
                                                                                
         TM    RUNINDS1,RUNIBTCH   Suppress averages for batch mode             
         JNZ   ENDREQ62                                                         
         CLI   RUNTYPE,RUNTSRVR    Suppress averages for command file           
         JNE   ENDREQ62                                                         
         OC    TNUMREQ,TNUMREQ     and if no requests processed                 
         JZ    ENDREQ62                                                         
                                                                                
         ICM   R0,15,TQUETIM                                                    
         SRDL  R0,32                                                            
         D     R0,TNUMREQ                                                       
         LR    R0,R1                                                            
         GOTOR EDTTUS,RL9QTIM                                                   
                                                                                
         ICM   R0,15,TTOTTIM                                                    
         SRDL  R0,32                                                            
         D     R0,TNUMREQ                                                       
         LR    R0,R1                                                            
         GOTOR EDTTUS,RL9ETIM                                                   
         MVC   PMESSAGE(RUNLIT9L),RUNLIT9                                       
         GOTOR PRTLOG                                                           
                                                                                
         L     R0,TTOTSIO                                                       
         SRDL  R0,32                                                            
         D     R0,TNUMREQ                                                       
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RLASIOS,DUB                                                      
                                                                                
         LG    GR1,TTOTCPU                                                      
         DSGF  GR0,F100                                                         
         XR    R0,R0                                                            
         D     R0,TNUMREQ                                                       
         MVC   RLACPUT,MAXCPUTL                                                 
         CGF   GR1,MAXCPUT                                                      
         JH    ENDREQ61                                                         
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RLACPUT,DUB                                                      
         MVC   RLACPUT(L'RLACPUT-5),RLACPUT+1                                   
         MVI   RLACPUT+L'RLACPUT-5,C'.'                                         
                                                                                
ENDREQ61 L     R0,TWRKADD                                                       
         SRDL  R0,32                                                            
         D     R0,TNUMREQ                                                       
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  RLARECS,DUB                                                      
         MVC   PMESSAGE(RUNLITAL),RUNLITA                                       
         GOTOR PRTLOG                                                           
                                                                                
ENDREQ62 GOTOR PRTLOG                                                           
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   ENDRUN04            Yes - we are all done                        
         TM    LP_FLAG2,LP_FFALT   Test FALINK test string input                
         JNZ   NXTWRK                                                           
         TM    RUNINDMQ,RUNIMQUP   Test MQ upload                               
         JNZ   CLRWRK                                                           
                                                                                
         ICM   R2,15,ASVR                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LAM   AR2,AR2,ALET                                                     
         SAC   512                                                              
         USING TABSSVRD,R2                                                      
         CLC   TSAQUE,AQUE         Test just processed a command job            
         JNE   *+10                                                             
         XC    TSAQUE,TSAQUE       Yes - clear address in server entry          
         LAM   AR2,AR2,ARZERO                                                   
                                                                                
         LAM   AR3,AR3,ALET                                                     
         ICM   R3,15,AQUE                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SAC   512                                                              
                                                                                
         LA    R1,NULLDONE         Default to post work complete                
         OC    RMQBADD,RMQBADD     Test output to MQ                            
         JZ    *+8                                                              
         LA    R1,NULLFREE         Yes - set to clear work queue entry          
         TM    LP_INDS,LP_IOUPD    Test updative request                        
         JZ    *+8                                                              
         LA    R1,NULLFREE         Yes - set to clear work queue entry          
         GOTOR UPDQUE,(R1)         Update the work queue entry                  
         MVC   TQERROR,ERROR       Set work completion code                     
         XC    TQASVR,TQASVR       Clear server pointer                         
         CLI   RUNTYPE,RUNTCMND    Clear work queue if command file             
         JE    *+12                                                             
         CLI   TQERROR,TQECANC     Or if work canceled                          
         JNE   *+10                                                             
         XC    TABSQUED(TABSQUEL),TABSQUED                                      
         LH    R1,TQLFASID         R1=client ASID                               
         J     NXTWRK                                                           
                                                                                
CLRWRK   LAM   AR3,AR3,ALET        Can't post or MQ - clear queue entry         
         L     R3,AQUE                                                          
         SAC   512                                                              
         XC    TABSQUED(TABSQUEL),TABSQUED                                      
                                                                                
NXTWRK   LARL  RF,SSB              GET DMGR ALET                                
         LAM   AR2,AR2,SSOALET-SSOOFF(RF)                                       
         SAC   512                                                              
         ICM   R2,15,ADSJOB        GOT JOB ENTRY ADDR                           
         JZ    NXTWRK1                                                          
         USING DSJOBD,R2                                                        
         ICM   R1,15,DSJMQRET      ANY MQ RETURN                                
         JZ    NXTWRK1                                                          
         XC    DSJMQRET,DSJMQRET   CLEAR IT                                     
         LR    R2,R1                                                            
         XC    0(L'DSMQRETK,R2),0(R2)    AND CLEAR MQ AREA                      
         DROP  R2                                                               
                                                                                
NXTWRK1  SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         OC    MAXDUMPS,MAXDUMPS                                                
         JZ    NXTWRK2                                                          
         CLC   MCNDUMPS,MAXDUMPS                                                
         JNH   NXTWRK2                                                          
         MVC   PMESSAGE(L'MAXDLIT),MAXDLIT                                      
         GOTOR PRTLOG                                                           
         J     ENDRUN              Exit back to MVS                             
NXTWRK2  CLI   MCBLOW,C'X'         Test exit at RUNLAST                         
         JE    ENDRUN              Yes - exit back to MVS                       
         MVI   MCBLOW,NOQ          Reset MCBLOW value                           
         MVI   WORKING,NOQ         Set working mode off (treat abends           
*                                    as due to RUNNER, not request)             
         XC    MCREQNO,MCREQNO     Normally GOLINK clears but make sure         
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,ASVR                                                       
         SAC   512                                                              
         USING TABSSVRD,R2                                                      
         XC    TSAQUE,TSAQUE       Clear queue entry                            
         LAM   AR2,AR2,ARZERO                                                   
         SAC   0                                                                
         J     GETWRK              Go look for more work                        
         EJECT                                                                  
***********************************************************************         
* Tidy up and exit back to MVS                                        *         
***********************************************************************         
                                                                                
ENDRUN   LAM   AR3,AR3,ARZERO                                                   
         ICM   R2,15,ASVR          Test server entry created                    
         JZ    ENDRUN04                                                         
         LAM   AR2,AR2,ALET        Remove myself from dataspace                 
         SAC   512                                                              
         CLC   TSASID,ASID         Only clear entry if mine                     
         JNE   *+10                                                             
         XC    TABSSVRD(TABSSVRL),TABSSVRD                                      
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
                                                                                
ENDRUN04 GOTOR VDMENQDQ,DMCB,(C'D',ALL)                                         
         LHI   RF,C'K'                                                          
         GOTOR VISGENQ,DMCB,(RF),ALL,0                                          
                                                                                
ENDRUN06 LARL  R3,SEOLST           Close all open systems                       
         USING SEOLSTD,R3                                                       
ENDRUN08 CLI   SEOLNUM,SEOLEOTQ    Test end of list                             
         JE    ENDRUN11                                                         
         TM    SEOLFLAG,SEOLFOPN   Test system is open                          
         JZ    ENDRUN10                                                         
         MVI   OPNACT,OPNACLO      Set action to close files                    
         GOTOR ROPNCLO,SEOLSTD                                                  
ENDRUN10 AHI   R3,SEOLSTL          Bump to next system list entry               
         J     ENDRUN08                                                         
         DROP  R3                                                               
                                                                                
ENDRUN11 TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   ENDRUN12            Do not call DDSTATE in batch mode            
         GOTO1 =V(DDSTATE),DMCB,=C'DELETE',0                                    
                                                                                
ENDRUN12 LARL  R4,MQGLOB           Point to MQ globals                          
         USING MQGLOB,R4                                                        
                                                                                
         TM    RUNINDMQ,RUNIMQEI   Test EDI input queue open                    
         JZ    ENDRUN14                                                         
         LA    R0,MQEDIHOB                                                      
         ST    R0,MQCLOHOB                                                      
         GOTOR RCALLMQ,MQCLO                                                    
                                                                                
ENDRUN14 TM    RUNINDMQ,RUNIMQEO   Test EDI output queue open                   
         JZ    ENDRUN16                                                         
         LA    R0,MQEDOHOB                                                      
         ST    R0,MQCLOHOB                                                      
         GOTOR RCALLMQ,MQCLO                                                    
                                                                                
ENDRUN16 TM    RUNINDMQ,RUNIMQQS                                                
         JZ    ENDRUN17                                                         
         GOTOR RCALLMQ,MQDSC       Disconnect from MQ                           
                                                                                
ENDRUN17 GOTOR GETCPU                                                           
         LG    GR1,CPUSAVE         CPU NOW                                      
         SLG   GR1,CPUENDRQ                                                     
         STG   GR1,DUB             CPU TIME USED SINCE END LAST REQ             
*                                                                               
         LA    R2,MCWORK                                                        
         USING RUNSMFD,R2          Build RUNNER SMF record                      
         XC    RUNSMFD(RUNSMAXL),RUNSMFD                                        
         LHI   RF,RUNSMFL                                                       
         STCM  RF,3,RUNSRLEN       Set SFM record length                        
         MVI   RUNSSMFT,RUNSSRUN   Set 'RUNNER' SMF record                      
*                                                                               
         MVC   RUNSCPUJ,DUB        SET DDLINK/SERVER TOTAL CPU UNITS            
*                                                                               
         TIME  TU                                                               
         STCM  R0,15,RUNSATIM      All times to now                             
         STCM  R0,15,RUNSSTIM                                                   
         STCM  R0,15,RUNSETIM                                                   
*                                                                               
         TM    RUNINDS2,RUNIRIDF   No SMF record if running under IDF           
         JNZ   ENDRUN18                                                         
         GOTOR RSMFOUT,PARM,10,RUNSMFD                                          
                                                                                
ENDRUN18 DS    0H                 Stop RUNNER in dataspace                      
         GOTOR VLOCKSPC,DMCB,X'000D0000'                                        
         LAM   AR0,ARF,ARZERO                                                   
                                                                                
         MVC   PMESSAGE(L'ENDLIT),ENDLIT                                        
         GOTOR PRTLOG                                                           
                                                                                
RUNEXIT  TM    RUNINDS1,RUNISMTP   TEST SMTP INITIALIZED                        
         JZ    RUNEXIT1                                                         
         GOTOR VSMTP,DMCB,('SMTPAEND',0)                                        
RUNEXIT1 LAM   AR0,ARF,ARZERO                                                   
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JZ    RUNEXITX                                                         
         OC    MCNDUMPS,MCNDUMPS   Test any dumps                               
         JZ    RUNEXITX                                                         
         ABEND 664                                                              
                                                                                
RUNEXITX XBASE ,                   Return to operating system                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Handle requests in batch processing mode                            *         
***********************************************************************         
                                                                                
GETREQ   NTR1  LABEL=*                                                          
         OC    MCORIGID,MCORIGID   Must know origin id                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   MCOVSYS,0           Must know application system                 
         JNE   *+6                                                              
         DC    H'0'                                                             
         GOTOR SETDSP              Set DMGR dspace (if not already set)         
         L     RF,MCUTL            Open control system files                    
         MVI   4(RF),CONSYSQ                                                    
         GOTOR DMGR,DMCB,(0,DMOPEN),CONSYS,CONLST,WORK,0                        
         SR    RF,RF                                                            
         GOTOR GETUID,MCORIGID     Set user-id values                           
         GOTOR MCVCARDS,DMCB,MCREQREC,=C'R'                                     
         MVC   PMESSAGE(L'REQLIT1),REQLIT1                                      
         MVC   PMESSAGE+L'REQLIT1(L'MCREQREC),MCREQREC                          
         GOTOR PRTLOG                                                           
         CLC   RFHDRLIT,MCREQREC   Test for 'RFHDR=' card                       
         JNE   GETREQ10                                                         
         CLI   MCREQREC+L'RFHDRLIT+20,C' '                                      
         JNE   GETREQ02                                                         
         MVC   MCRFHDR(20),MCREQREC+L'RFHDRLIT                                  
         J     GETREQ04                                                         
                                                                                
GETREQ02 GOTOR MCVHEXIN,DMCB,MCREQREC+L'RFHDRLIT,MCRFHDR,L'MCRFHDR*2            
                                                                                
GETREQ04 TM    UIDOFL1,CTIDOFPW    Test password protected user-id              
         JZ    GETREQ08                                                         
                                                                                
         L     R3,AIO              Read password record                         
         USING CT0REC,R3                                                        
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,UIDSECID                                                 
         MVC   CT0KNUM,MCRFHDR+(RQHPSWD-RQHITRM)                                
         OC    CT0KNUM,CT0KNUM     Test password set                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR DMGR,DMCB,DMREAD,CTFILE,CT0REC,CT0REC                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,CT0DATA          Locate system element on record              
         USING CTSYSD,R3                                                        
         SR    R0,R0                                                            
GETREQ06 CLI   CTSYSEL,0           Test end of record                           
         JE    GETREQ08                                                         
         CLI   CTSYSEL,CTSYSELQ    Test system element                          
         JNE   *+14                                                             
         CLC   CTSYSNUM,MCOVSYS    Test correct system element                  
         JE    *+14                                                             
         IC    R0,CTSYSLEN                                                      
         AR    R3,R0                                                            
         J     GETREQ06            Look again if not                            
                                                                                
         MVC   MCP1ACCS,CTSYSLMT   Set password limit access                    
         OC    MCP1ACCS,MCP1ACCS                                                
         JZ    GETREQ08                                                         
         MVC   MCC1ACCS,MCP1ACCS   Set composite limit access                   
                                                                                
GETREQ08 GOTOR MCVCARDS,DMCB,MCREQREC,=C'R'                                     
         MVC   PMESSAGE(L'REQLIT1),REQLIT1                                      
         MVC   PMESSAGE+L'REQLIT1(L'MCREQREC),MCREQREC                          
         GOTOR PRTLOG                                                           
                                                                                
GETREQ10 L     R3,RREQUEST         R3=A(card request pool)                      
GETREQ12 MVC   0(L'MCREQREC,R3),MCREQREC                                        
         AHI   R3,L'MCREQREC       Bump to next request card                    
         GOTOR MCVCARDS,DMCB,MCREQREC,=C'R'                                     
         MVC   PMESSAGE(L'REQLIT1),REQLIT1                                      
         MVC   PMESSAGE+L'REQLIT1(L'MCREQREC),MCREQREC                          
         GOTOR PRTLOG                                                           
         CLI   MCREQREC,SLASH      Test end of request data                     
         JNE   GETREQ12                                                         
         MVC   0(L'MCREQREC,R3),SPACES                                          
                                                                                
         L     R3,RREQUEST                                                      
         USING RUNQD,R3            Process request cards                        
         CLC   RUNQPROG,MCPROG     Program code must match request              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   DUB,SPACES          Build server phase name                      
         MVC   DUB(L'RUNQSRVR),RUNQSRVR                                         
         GOTOR SETSVR,DUB          Load server and initialize                   
         JNE   EXIT                                                             
                                                                                
         ICM   RF,15,MCUTL         Open application files                       
         MVC   4(1,RF),MCIDSENO                                                 
         ICM   RF,7,SVRFILE1                                                    
         GOTOR DMGR,DMCB,(0,DMOPEN),0(RF),7(RF),WORK,0                          
                                                                                
GETREQX  J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to load a phase (emulates LOADEM in DDMASTER)               *         
*                                                                     *         
* Ntry:- R1=A(Parameter list), MCDUB=phase name                       *         
***********************************************************************         
                                                                                
LOADEM   NTR1  LABEL=*                                                          
         LR    R2,R1               Point to caller's parameter list             
         GOTOR SETREG              Set RUNNER registers                         
*                                                                               
         ICM   RF,15,=V(PHSCNT)    Count phase about to be loaded               
         JZ    LOADEM1                                                          
         GOTOR (RF),DMCB,(C'A',MCDUB),0                                         
*                                                                               
LOADEM1  GOTOR RGPHS,DMCB,0,('LOADQ',MCDUB)                                     
         MVC   4(4,R2),0(R1)       Return load address                          
         J     EXIT                Exit to caller with CC set                   
         EJECT                                                                  
***********************************************************************         
* Routine to load phases                                              *         
*                                                                     *         
* Ntry:- R1=A(Parameter list) as follows:-                            *         
*                                                                     *         
*        P1 B0    - Phase number                                      *         
*           B1-3  - A(Load address)                                   *         
*        P2 B0    - See equates below                                 *         
*           B1-3  - X'0sppoo' if CALLOV, AL3(phase name) if ebcdic    *         
*                                                                     *         
* To set an application trap:-                                        *         
*                                                                     *         
*        P1 B0    - X'80' bit on indicates P4 present                 *         
*           B1-3  - A(caller's trap routine)                          *         
*        P2 B0    - C'*' set detail and summary trace                 *         
*                 - C'+' set summary trace                            *         
*           B1-3  - A(module/routine name)                            *         
*        P3 B0-3  - A(A(module/routine address))                      *         
*        P4 B0-3  - A(caller's trap routine) or zero                  *         
***********************************************************************         
                                                                                
CALLOVQ  EQU   C'R'                CALLOV style load                            
LOADQ    EQU   C'E'                Ebcdic module name                           
TRAPQ    EQU   C'T'                Set module call trap                         
VERIFYQ  EQU   C'V'                Verify memory                                
PATCHQ   EQU   C'P'                Patch memory                                 
                                                                                
GPHS     NTR1  LABEL=*,WORK=(RC,GPWORKL)                                        
         USING GPWORKD,RC          RC=A(LOCAL W/S)                              
         XC    GPWORKD(GPWORKL),GPWORKD                                         
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         LR    R4,R1               R4=A(calling parameter list)                 
         ST    R4,GPPARA           Save calling parameter list                  
         MVC   GPMODN,SPACES       Set unknown module name                      
         CLI   4(R4),TRPPALLQ      Test setting application trap                
         JE    GPHS0020                                                         
         CLI   4(R4),TRPPSUMQ                                                   
         JE    GPHS0020                                                         
         OC    1(3,R4),1(R4)       Test A(phase given)                          
         JZ    GPHS0010                                                         
         CLC   1(3,R4),ATWA+1      Test within TWA area                         
         JL    *+14                                                             
         CLC   1(3,R4),ATWAX+1                                                  
         JNH   GPHS0010                                                         
         XC    1(3,R4),1(R4)       No - do regular load                         
                                                                                
GPHS0010 OC    1(11,R4),1(R4)      Test caller wants overlay load               
         JNZ   GPHS0022                                                         
         ICM   R0,B'1000',T00A                                                  
         L     RF,LP_ASVR                                                       
         ICM   R0,B'0110',RSVRSYSP-RSVRDEFD(RF)                                 
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R0,B'0001',0(R4)                                                 
         STCM  R0,B'1111',4(R4)    Set TSPO                                     
         J     GPHS0022                                                         
                                                                                
GPHS0020 TM    0(R4),X'80'         Test passing caller's trap address           
         JZ    *+10                                                             
         MVC   GPTRAP,12(R4)       Yes - set caller's trap address              
         L     RF,8(R4)                                                         
         MVC   GPREAL,0(RF)        Set address of module to trap                
         MVC   GPPHASE,SPACES      Build input string for LPHS                  
         MVI   GPPHASE,EMBEDDED    Set embedded parameter                       
         MVC   GPPHASE+1(L'TRPPARM),4(R4)                                       
         L     RF,4(R4)                                                         
         MVC   GPPHASE+1+L'TRPPARM(L'MODTMODN),0(RF)                            
         GOTOR RLPHS,GPPHASE       Build trap                                   
         JE    EXITY               Exit if traps are not enabled                
         L     RF,8(R4)            Point to caller's adcon                      
         MVC   0(4,RF),GPADDR      Set trap address there                       
         J     EXITN                                                            
                                                                                
GPHS0022 CLI   4(R4),TRAPQ         Test setting a trap                          
         JNE   GPHS0024                                                         
         TM    0(R4),X'80'         Test passing trap address                    
         JZ    GPHS0024                                                         
         MVC   GPTRAP,12(R4)       Set caller's trap address                    
         MVI   0(R4),0                                                          
                                                                                
GPHS0024 MVC   GPOPT,0(R4)         Save option byte                             
         MVI   0(R4),0                                                          
         MVC   GPTOADDR,0(R4)      Set 'move to' address                        
         XC    0(4,R4),0(R4)       and clear it                                 
         CLI   4(R4),0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         SR    R5,R5                                                            
         MVC   GPPHASE,SPACES                                                   
         MVI   GPTEST,SPACE                                                     
         ICM   R1,7,5(R4)                                                       
         MVC   GPTYPE,4(R4)                                                     
         CLI   GPTYPE,LOADQ        Test loading                                 
         JE    GPHS0030                                                         
         CLI   GPTYPE,TRAPQ        or loading and trapping                      
         JE    GPHS0030                                                         
         CLI   GPTYPE,PATCHQ       or processing patch card                     
         JE    GPHS0030                                                         
         CLI   GPTYPE,VERIFYQ      or processing verify card                    
         JE    GPHS0030                                                         
         CLI   GPTYPE,CALLOVQ      Test regular call                            
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR MCVHEXOU,GPMCPARA,5(R4),GPPHASE,3,0                              
         MVI   GPPHASE,C'T'                                                     
         J     GPHS0060                                                         
                                                                                
GPHS0030 LA    RF,0(R1)            RF=A(phase name(/test level)+patch)          
         LA    RE,GPPHASE                                                       
         LHI   R0,GPPHASEM+1                                                    
         J     GPHS0042                                                         
                                                                                
GPHS0040 CLI   0(RF),PLUS                                                       
         JNE   GPHS0042                                                         
         LR    R5,RF                                                            
         J     GPHS0060                                                         
                                                                                
GPHS0042 CLI   0(RF),SPACE         Look for space                               
         JE    GPHS0060            Found - no test version                      
         CLI   0(RF),SLASH         Look for delimiters                          
         JE    GPHS0050                                                         
         CLI   0(RF),COMMA                                                      
         JE    GPHS0050                                                         
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         JCT   R0,GPHS0040                                                      
         J     GPHS0060                                                         
                                                                                
GPHS0050 MVC   GPTEST,1(RF)        Move in test version                         
         MVC   0(1,RE),GPTEST      Set it in phase name                         
         CLI   2(RF),PLUS          Test followed by patch                       
         JNE   GPHS0060                                                         
         LA    R5,2(RF)                                                         
                                                                                
GPHS0060 LR    R0,RE               Save A(test level) in name                   
         LA    R1,GPPHASE                                                       
         CLI   GPTYPE,PATCHQ       Test processing a patch card                 
         JE    *+12                                                             
         CLI   GPTYPE,VERIFYQ      Test processing a verify card                
         JNE   *+8                                                              
         ICM   R1,8,=AL1(GPMXLOAD) Yes - phase must be loaded already           
         GOTOR RLPHS,(R1)          Try to load phase                            
         JNZ   GPHS0080            Found it                                     
         CLI   GPTYPE,PATCHQ       Test processing a patch card                 
         JE    GPHSN               Yes - error                                  
         CLI   GPTYPE,VERIFYQ      Test processing a verify card                
         JE    GPHSN               Yes - error                                  
         MVC   PMESSAGE(L'LODLIT1),LODLIT1                                      
         MVC   PMESSAGE+L'LODLIT1(GPPHASEM),GPPHASE                             
         MVC   PMESSAGE+L'LODLIT1+GPPHASEM+1(L'GPMODN),GPMODN                   
         CLI   GPTEST,SPACE                                                     
         JE    GPHS0070                                                         
         MVC   PMESSAGE(L'LODLIT3),LODLIT3                                      
         MVC   PMESSAGE+L'LODLIT3(GPPHASEM),GPPHASE                             
         MVC   PMESSAGE+L'LODLIT3+GPPHASEM+1(L'GPMODN),GPMODN                   
                                                                                
GPHS0070 GOTOR PRTLOG                                                           
         CLI   GPTEST,SPACE        Was it a test version?                       
         JE    GPHS0080                                                         
         LR    RE,R0                                                            
         MVI   0(RE),SPACE         Yes - go for production version              
         GOTOR RLPHS,GPPHASE                                                    
         JNZ   GPHS0080                                                         
         MVC   PMESSAGE(L'LODLIT1),LODLIT1                                      
         MVC   PMESSAGE+L'LODLIT1(GPPHASEM),GPPHASE                             
         MVC   PMESSAGE+L'LODLIT1+GPPHASEM+1(L'GPMODN),GPMODN                   
         GOTOR PRTLOG                                                           
                                                                                
GPHS0080 MVC   0(L'GPADDR,R4),GPADDR                                            
         OC    GPTOADDR,GPTOADDR                                                
         JZ    *+10                                                             
         MVC   0(L'GPTOADDR,R4),GPTOADDR                                        
         MVC   8(L'GPPHLN,R4),GPPHLN                                            
         MVC   12(L'GPREAL,R4),GPREAL                                           
         OC    GPADDR,GPADDR                                                    
         JZ    EXITN                                                            
         LTR   R5,R5                                                            
         JZ    GPHSY                                                            
                                                                                
         AHI   R5,1                                                             
         LA    RF,1(R5)                                                         
         LA    R1,L'GPDUB-1                                                     
GPHS0092 CLI   0(RF),EQUAL                                                      
         JE    GPHS0094                                                         
         AHI   RF,1                                                             
         JCT   R1,GPHS0092                                                      
         J     GPHSN                                                            
                                                                                
GPHS0094 MVI   GPDUB,C'0'                                                       
         MVC   GPDUB+1(L'GPDUB-1),GPDUB                                         
         LA    RE,GPDUB(R1)                                                     
         LHI   RF,L'GPDUB                                                       
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,GPHSMVE5                                                      
         LA    R5,2(R5,RF)                                                      
         GOTOR MCVHEXIN,GPMCPARA,GPDUB,GPWORK,L'GPDUB                           
         OC    12(4,R1),12(R1)                                                  
         JZ    GPHSN                                                            
         CLC   GPPHLN,GPWORK                                                    
         JL    GPHSN                                                            
         MVC   GPDUB(4),GPWORK                                                  
                                                                                
         LR    RF,R5                                                            
         LHI   R1,48                                                            
         SR    R0,R0                                                            
         BASR  RE,0                                                             
         CLI   0(RF),SPACE                                                      
         JE    GPHS0090                                                         
         AHI   RF,1                                                             
         AHI   R0,1                                                             
         BCTR  R1,RE                                                            
         J     GPHSN                                                            
                                                                                
GPHS0090 LTR   R0,R0                                                            
         JZ    GPHSN                                                            
         GOTOR MCVHEXIN,GPDMCB,(R5),GPWORK,(R0)                                 
         ICM   RF,15,12(R1)                                                     
         JZ    GPHSN                                                            
         L     RE,GPDUB                                                         
         A     RE,GPADDR                                                        
         BCTR  RF,0                                                             
         STM   RE,RF,GPSVRERF                                                   
         CLI   GPTYPE,LOADQ        Test processing a load card                  
         JE    *+12                                                             
         CLI   GPTYPE,PATCHQ       Test processing a patch card                 
         JNE   GPHS0100                                                         
         LR    R0,RE                                                            
         MVC   PMESSAGE(L'PCHLIT),PCHLIT                                        
         GOTOR MCVHEXOU,GPDMCB,(R0),PMESSAGE+L'PCHLIT,1(RF),0                   
         GOTOR PRTLOG                                                           
         LM    RE,RF,GPSVRERF                                                   
         EX    RF,GPHSMVEW                                                      
         J     GPHSY                                                            
                                                                                
GPHS0100 CLI   GPTYPE,VERIFYQ      Test processing a verify card                
         JE    *+6                                                              
         DC    H'0'                                                             
         EX    RF,GPHSCLEW         Compare actual value with string             
         JE    EXIT                                                             
         LR    R0,RE                                                            
         MVC   PMESSAGE(L'VERLIT),VERLIT                                        
         GOTOR MCVHEXOU,GPDMCB,(R0),PMESSAGE+L'VERLIT,1(RF),0                   
         GOTOR PRTLOG                                                           
         J     EXITN                                                            
                                                                                
GPHSN    MVC   PMESSAGE(L'LODLIT4),LODLIT4                                      
         GOTOR PRTLOG                                                           
         XC    GPADDR,GPADDR                                                    
         J     EXITN                                                            
*                                                                               
GPHSY    ICM   RF,15,=V(PHSCNT)    Count phase just loaded                      
         JZ    GPHSY1                                                           
         GOTOR (RF),GPDMCB,(C'A',GPPHASE),0                                     
*                                                                               
GPHSY1   ICM   R0,15,GPTOADDR      Test 'move to' address set                   
         JZ    EXITY                                                            
         L     R1,GPPHLN           Move phase to specified location             
         L     RE,GPADDR                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Load phases, apply patches and set module traps                     *         
***********************************************************************         
                                                                                
LPHS     NTR1  LABEL=*                                                          
         STCM  R1,8,GPMODE         Save calling mode                            
         LA    R4,0(R1)            R4=A(phase name)                             
         CLI   0(R4),EMBEDDED      Test embedded parameters                     
         JE    LPHS0080            (application traps)                          
         XC    GPVALS(GPVALSL),GPVALS                                           
         CLI   0(R4),TRPPALLQ      Test trap using map name                     
         JE    *+12                                                             
         CLI   0(R4),TRPPSUMQ      or summary only map name                     
         JNE   LPHS0002                                                         
         CLC   CRPHASE,1(R4)       Test loading T00A phase                      
         JNE   LPHS0070            No                                           
         MVC   GPPARM,0(R4)        Set parameter value                          
         LA    R4,1(R4)            and bump over it                             
                                                                                
LPHS0002 LARL  R5,MODTAB           Test phase aready loaded                     
         USING MODTABD,R5                                                       
         LHI   R0,MODTABN          R0=N'entries in phase table                  
         LHI   RE,L'MODTPHSN-1     Set up compare length                        
         LA    RF,L'MODTPHSN-1(R4)                                              
LPHS0004 CLI   0(RF),SPACE                                                      
         JNE   LPHS0010                                                         
         BCTR  RF,0                                                             
         JCT   RE,LPHS0004                                                      
                                                                                
LPHS0010 CLI   MODTABD,MODTEOTQ    Test end of phase table                      
         JE    LPHS0020                                                         
         EX    RE,LPHSCLN4         Match input name to phase name               
         JE    LPHS0050            Found - return address only                  
         EX    RE,LPHSCLM4         Match input name to module name              
         JE    LPHS0050            Found - return address only                  
         AHI   R5,MODTABL          Bump to next phase table entry               
         JCT   R0,LPHS0010                                                      
         DC    H'0'                Need more space in MODTAB                    
                                                                                
LPHS0020 TM    GPMODE,GPMXLOAD     Test don't issue load                        
         JNZ   LPHS0140                                                         
         GOTOR RLOADIT,GPMCPARA,(R4),0,0                                        
         OC    4(4,R1),4(R1)                                                    
         JZ    LPHS0140                                                         
         MVC   MODTPHSN,0(R4)      Set phase name                               
         MVC   MODTMODN,GPMODN     Set module name                              
         MVC   MODTTEST,GPTEST     Set test phase                               
         MVC   MODTADDR,4(R1)      Set phase address                            
         MVC   MODTREAL,4(R1)      Set real phase address                       
         MVC   MODTLEN,0(R1)       Set phase length                             
                                                                                
         ICM   RE,15,MODTADDR                                                   
         ICM   RF,15,MODTLEN                                                    
         AR    RE,RF               RE=A(end of phase just loaded)               
         C     RE,MCDMPEND         Set dump end if necessary                    
         JNH   LPHS0030                                                         
         ST    RE,MCDMPEND                                                      
                                                                                
LPHS0030 MVC   LL2PHSN,GPPHASE                                                  
         MVC   LL2PHAS,MODTMODN                                                 
         MVC   PMESSAGE(LODLIT2L),LODLIT2                                       
         LA    R0,PMESSAGE+LODLIT2L                                             
         GOTOR MCVHEXOU,GPMCPARA,MODTADDR,(R0),L'MODTADDR,0                     
         CLI   MODTADDR,0                                                       
         JNE   LPHS0040                                                         
                                                                                
         ICM   RF,15,MODTADDR                                                   
         ICM   R0,15,MODTLEN       Locate first book, level & date info         
         SHI   R0,MODINFLQ         Subtract length of BOOK= etc. data           
         AR    RF,R0                                                            
         SR    R1,R1                                                            
         BASR  RE,0                                                             
         CLC   BOOKPFX,00(RF)      BOOK=  at +00                                
         JNE   *+10                                                             
         CLC   LEVLPFX,16(RF)      LEVEL= at +16                                
         JNE   *+10                                                             
         CLC   DATEPFX,26(RF)      DATE=  at +26                                
         JNE   *+6                                                              
         LR    R1,RF                                                            
         BCTR  RF,0                                                             
         BCTR  R0,RE                                                            
         LTR   R1,R1               Test book info found                         
         JZ    LPHS0040                                                         
*                                                                               
         MVC   PMESSAGE+LODLIT2L+(L'MODTADDR*2)+1(MODINFLQ),0(R1)               
*                                                                               
         ICM   R0,15,MODTLEN       PHASE LEN                                    
         ICM   RE,15,MODTADDR      PHASE ADDR                                   
         LR    RF,R1               RF=Actual location of BOOK=                  
         SR    RF,RE               RF=Location within phase                     
         SR    R0,RF               Sub from len                                 
         SHI   R0,MODINF2Q         Sub length of long stamp                     
         JM    LPHS0040            If minus phase was not long enough           
         CLC   COMPPFX,54(R1)      Look for COMPILED                            
         JNE   LPHS0040                                                         
         MVC   PMESSAGE+LODLIT2L+(L'MODTADDR*2)+1(MODINF1Q),0(R1)               
         CLC   COMPPFXX,96(R1)     Look for AS                                  
         JNE   LPHS0040                                                         
         MVC   PMESSAGE+LODLIT2L+(L'MODTADDR*2)+1(MODINF2Q),0(R1)               
*                                                                               
                                                                                
LPHS0040 GOTOR PRTLOG                                                           
                                                                                
         OC    REQNO,REQNO         Test started running requests                
         JZ    LPHS0060            No, still initialising                       
         ICM   RE,15,RTOTLOD       Increment total loads                        
         AHI   RE,1                                                             
         STCM  RE,15,RTOTLOD                                                    
         J     LPHS0060                                                         
                                                                                
LPHS0050 CLI   INFOMESS,NOQ        Test don't want information messages         
         JE    LPHS0060                                                         
         MVC   PMESSAGE(L'LODLIT5),LODLIT5                                      
         MVC   PMESSAGE+L'LODLIT5(L'MODTPHSN),MODTPHSN                          
         MVC   PMESSAGE+L'LODLIT5+L'MODTPHSN+1(L'MODTMODN),MODTMODN             
         GOTOR PRTLOG                                                           
                                                                                
LPHS0060 MVC   GPADDR,MODTADDR     Set output phase address                     
         MVC   GPPHLN,MODTLEN      Set output phase length                      
         MVC   GPREAL,MODTREAL     Set real module address                      
         SR    RF,RF                                                            
         J     LPHS0090                                                         
                                                                                
LPHS0070 MVC   GPADDR,ARUNNER      Set address for runner patch                 
         MVC   GPPHLN,=A(RUNNERX-RUNNER)                                        
         CLI   1(R4),SPACE         No map name means patch runner               
         JE    LPHS0140                                                         
                                                                                
         LARL  R1,MAPTAB           Look up name in map table                    
         USING MAPTABD,R1                                                       
LPHS0072 CLI   MAPTABD,MAPTEOTQ                                                 
         JE    LPHSN                                                            
         CLC   MAPTNAME,1(R4)                                                   
         JE    *+12                                                             
         AHI   R1,MAPTABL                                                       
         J     LPHS0072                                                         
         ST    R1,GPAMAP                                                        
         LLH   RE,MAPTDISP                                                      
         SR    RF,RF                                                            
         ICM   RF,1,MAPTFNDX                                                    
         JNZ   *+12                                                             
         LA    RF,WORKD            Zero=WORKD values                            
         J     *+8                                                              
         L     RF,AFACLST-L'AFACLST(RF)                                         
         AR    RF,RE                                                            
         MVC   GPREAL,0(RF)        Set real module address                      
         MVC   GPADDR,0(RF)        Set the address                              
         MVC   GPPHLN,EFFS         and maximum length                           
         J     LPHS0090                                                         
                                                                                
LPHS0080 TM    LP_FLAG2,LP_FTRAP   Test application traps are enabled           
         JZ    EXITY                                                            
         LA    R4,1(R4)            Bump over embedded action flag               
         LARL  R5,MODTAB           Create phase table entry                     
         USING MODTABD,R5                                                       
LPHS0082 CLI   MODTABD,MODTEOTQ    Test end of phase table                      
         JE    *+12                                                             
         AHI   R5,MODTABL                                                       
         J     LPHS0082                                                         
         MVC   GPPARM,0(R4)        Set parameter value & name                   
         MVC   MODTPHSN,L'GPPARM(R4)                                            
         MVC   MODTREAL,GPREAL     Set real module address                      
         SR    RF,RF                                                            
         J     LPHS0110                                                         
                                                                                
LPHS0090 CLI   GPTYPE,TRAPQ        Test trapping calls                          
         JNE   LPHS0140                                                         
         LTR   RF,RF               Test facilities list                         
         JZ    LPHS0110                                                         
                                                                                
LPHS0100 LARL  R5,MODTAB           Yes - create phase table entry               
         USING MODTABD,R5                                                       
LPHS0102 CLI   MODTABD,MODTEOTQ    Test end of phase table                      
         JE    *+12                                                             
         AHI   R5,MODTABL                                                       
         J     LPHS0102                                                         
         MVC   MODTPHSN,1(R4)      Set module name                              
         MVC   MODTREAL,0(RF)      Set real module address                      
                                                                                
***********************************************************************         
* Build a trap table entry (TRPTAB)                                   *         
***********************************************************************         
                                                                                
LPHS0110 TM    MODTINDS,MODTITRP   Test trap set already                        
         JNZ   LPHSN               Yes - error exit                             
         OI    MODTINDS,MODTITRP   Set module has a trap                        
         LARL  R2,TRPTAB                                                        
         USING TRPTABD,R2          Get a trap table entry to use                
         USING TRPVALD,TRPVALS                                                  
         LHI   R0,1                Initialize count                             
         SR    R1,R1                                                            
         ICM   R1,3,TRPNUM         No entries so far - use first one            
         JZ    LPHS0120                                                         
         LA    R0,1(R1)                                                         
         CHI   R0,TNDXMAX                                                       
         JNH   *+6                                                              
         DC    H'0'                                                             
         MHI   R1,TRPTABL                                                       
         AR    R2,R1               else point to next available                 
LPHS0120 STH   R0,TRPNUM           Set number of entries in trap table          
         BCTR  R0,0                                                             
         SLL   R0,2                Multiply by width of TNDX                    
         LARL  R1,TNDX                                                          
         AR    R0,R1               Point to trap index jump instruction         
         STCM  R0,15,MODTADDR      Set as phase address                         
         STCM  R0,15,GPADDR        Set this for GPHS too                        
         LTR   RF,RF               Test pointing to facilities list             
         JZ    *+8                                                              
         STCM  R0,15,0(RF)         Yes - set address in facilities list         
         MVC   MODTTRAP,GPTRAP     Set caller's trap routine                    
         LA    R0,MODTABD                                                       
         ST    R0,TRPAMOD          Point to phase entry in TRPTAB entry         
         MVI   TRPPARM,TRPPALLQ    Default is everything                        
         CLI   GPPARM,0            Test given                                   
         JE    *+10                                                             
         MVC   TRPPARM,GPPARM      Yes - use this one                           
         LTR   RF,RF               Test pointing to facilities list             
         JZ    *+10                                                             
         MVC   TRPPARM,0(R4)       Yes - set trap parameter too                 
                                                                                
***********************************************************************         
* Build runner log entries for trap                                   *         
***********************************************************************         
                                                                                
         MVC   RLHMODN,MODTPHSN    Module name                                  
         GOTOR MCVHEXOU,GPMCPARA,MODTADDR,RLHADDR,L'MODTADDR,0                  
         GOTOR MCVHEXOU,GPMCPARA,MODTREAL,RLHREAL,L'MODTREAL,0                  
         GOTOR MCVHEXOU,GPMCPARA,MODTTRAP,RLHTRAP,L'MODTTRAP,0                  
         MVC   RLHFACL(L'RLHFACL+L'RLHFILL),SPACES                              
         MVC   RLHFACL(2),=C'No'                                                
         ICM   R1,15,GPAMAP        Point to map table entry                     
         JZ    LPHS0130                                                         
         LLH   RF,MAPTDISP                                                      
         STH   RF,HALF1                                                         
         LLC   RF,MAPTFNDX         Get facilities list index                    
         SRL   RF,2                divide by 4 to get number                    
         MHI   RF,L'FLITAB         Index into FLITAB for name                   
         LARL  RE,FLITAB                                                        
         AR    RE,RF                                                            
         MVC   RLHFACL,0(RE)       Control block name                           
         LA    RF,RLHFACL+L'RLHFACL-1                                           
         CLI   0(RF),C' '                                                       
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         MVC   1(6,RF),=C',Disp='                                               
         GOTOR MCVHEXOU,GPMCPARA,HALF1,7(RF),L'HALF1,0                          
                                                                                
LPHS0130 MVC   PMESSAGE(RUNLITHL),RUNLITH                                       
         GOTOR PRTLOG                                                           
         J     LPHS0140                                                         
                                                                                
***********************************************************************         
* If have just loaded DDLINK call it to initialize LP_D addresses     *         
***********************************************************************         
                                                                                
LPHS0140 OC    GPADDR,GPADDR       Test successful load                         
         JZ    LPHSX                                                            
                                                                                
         CLC   DDLINK,GPPHASE      Test just loaded DDLINK                      
         JNE   LPHS0150                                                         
         TM    RUNINDS1,RUNILINK   Test already initialized DDLINK              
         JNZ   LPHSX                                                            
         OI    RUNINDS1,RUNILINK   Set DDLINK initialization performed          
         MVI   SVRIND1,RSVRILNK                                                 
         MVC   VDDLINK,GPADDR      Set address of DDLINK                        
         GOTOR RGOLINK,FF          Call DDLINK to initialize addresses          
                                                                                
***********************************************************************         
* If have just loaded a T00A phase find its name in T00ATAB           *         
***********************************************************************         
                                                                                
LPHS0150 CLC   CRPHASE,GPPHASE     Test just loaded a T00A phase                
         JNE   LPHSX                                                            
         GOTOR MCVHEXIN,GPDMCB,GPPHASE+4,BYTE1,2,0                              
         OC    12(4,R1),12(R1)                                                  
         JZ    LPHSX                                                            
         LARL  RF,T00ATAB          Look up T00A module number in table          
         LHI   R0,T00ATABN                                                      
LPHS0160 CLC   0(1,RF),BYTE1                                                    
         JE    LPHS0162                                                         
         AHI   RF,L'T00ATAB                                                     
         JCT   R0,LPHS0160                                                      
         J     LPHS0170                                                         
LPHS0162 MVC   MODTMODN,1(RF)      Set T00A phase module name                   
                                                                                
LPHS0170 TM    GPOPT,X'80'         Test passing facilities disps.               
         JZ    LPHS0172                                                         
         L     RF,GPPARA           Point to GPHS parameter list                 
         L     RF,12(RF)           Point to facilities displacements            
         MVC   MODTDSPS(MODTDSPL),0(RF)                                         
                                                                                
LPHS0172 OC    MODTDSPS(MODTDSPL),MODTDSPS                                      
         JZ    LPHSX                                                            
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,MODTDCOM       Set address in COMFACS if necessary          
         JZ    *+14                                                             
         A     RF,ACOMFACS                                                      
         MVC   0(4,RF),MODTADDR                                                 
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,MODTDSYS       Set address in SYSFACS if necessary          
         JNZ   *+14                                                             
         OC    MODTDCOM,MODTDCOM   Allow zero disp if not COMFACS               
         JNZ   LPHSX                                                            
         A     RF,ASYSFACS                                                      
         MVC   0(L'MODTADDR,RF),MODTADDR                                        
         J     LPHSX                                                            
                                                                                
LPHSN    XC    GPADDR,GPADDR       Clear address on error                       
                                                                                
LPHSX    OC    GPADDR,GPADDR       Set CC=not zero if loaded/found              
         J     EXIT                                                             
         DROP  R1,R2,R5,RC                                                      
                                                                                
GPWORKD  DSECT ,                   ** GPHS local w/s **                         
GPDUB    DS    D                                                                
GPPARA   DS    A                                                                
GPDMCB   DS    6F                                                               
GPMCPARA DS    6F                                                               
GPWORK   DS    XL80                                                             
GPSVRERF DS    2F                                                               
GPAMAP   DS    A                   A(MAPTAB entry)                              
GPREAL   DS    A                   Real module address                          
GPTRAP   DS    A                   Caller trap address                          
GPOPT    DS    X                   Saved HOB of P1                              
GPTYPE   DS    X                   Type of call                                 
GPPARM   DS    CL(L'TRPPARM)       Trace parameter value                        
                                                                                
GPTOADDR DS    A                   A(move where)                                
                                                                                
GPVALS   DS    0X                  ** Phase values **                           
GPADDR   DS    A                   A(loaded phase)                              
GPPHLN   DS    F                   L'loaded phase                               
GPVALSL  EQU   *-GPVALS                                                         
                                                                                
GPMODN   DS    CL8                 Module name                                  
GPPHASEM EQU   8                   Maximum length of phase name                 
GPPHASE  DS    CL12                Phase name (space padded)                    
GPTEST   DS    C                   Test level                                   
                                                                                
GPMODE   DS    X                   ** Call mode for LPHS **                     
GPMXLOAD EQU   X'80'               Don't process load (PATCH=support)           
                                                                                
GPWORKL  EQU  *-GPWORKD                                                         
RUNNER   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Load or remove a phase                                              *         
*                                                                     *         
* Ntry:- R1=A(Parameter list) as follows:-                            *         
*                                                                     *         
*        P1       - AL4(Phase name)                                   *         
*        P2       - AL4(0) Load phase and return address or           *         
*                   X'FFFFFFFF' to delete named phase                 *         
*        P3/B0    - C'M' to load and move to actual address           *         
*                   C'A' to load amd move to dwd aligned address      *         
*                                                                     *         
* Exit:- Parameter list updated as follows if phase load (not delete) *         
*                                                                     *         
*        P1       - AL4(Phase length)                                 *         
*        P2       - AL4(Phase load address)                           *         
***********************************************************************         
                                                                                
LOADIT   NTR1  BASE=*,LABEL=*                                                   
         LR    R3,R1               R3=A(parameter list)                         
         SR    R2,R2                                                            
         ICM   R2,7,1(R3)          R2=A(phase name)                             
         CLC   DELPHASE,4(R3)      Test for delete mode                         
         JE    LOADIT04                                                         
         SR    R4,R4                                                            
         ICM   R4,7,5(R3)          R4=A(move to address)                        
         JZ    LOADIT02                                                         
         CLI   8(R3),C'M'          Test for move mode                           
         JE    LOADIT02                                                         
         CLI   8(R3),C'A'          Test for address mode                        
         JE    *+10                                                             
         SR    R4,R4               No - clear move to address                   
         J     LOADIT02                                                         
         AHI   R4,7                Round address to doubleword boundary         
         SRL   R4,3                                                             
         SLL   R4,3                                                             
                                                                                
LOADIT02 XC    0(8,R3),0(R3)       Clear values for error exit                  
         LOAD  EPLOC=(2),ERRET=LOADITX                                          
         SLL   R1,3                R1=length of module in bytes                 
         STCM  R1,7,1(R3)          Set length for user                          
         STCM  R0,15,4(R3)         Set entry address for user                   
         LTR   RE,R4               Test move to address set                     
         JZ    LOADITX             Exit if not address mode                     
         LR    RF,R1               Move to specified location                   
         MVCL  RE,R0                                                            
                                                                                
LOADIT04 DELETE EPLOC=(2)          Delete phase                                 
                                                                                
LOADITX  J     EXIT                                                             
         DROP  RB                                                               
                                                                                
DELPHASE DC    X'FFFFFFFF'                                                      
         EJECT                                                                  
***********************************************************************         
* Routine to call DDLINK                                              *         
*                                                                     *         
* Ntry:- R1=mode value                                                *         
*        R0=A(Command card) if mode of RPRCCMDQ is passed             *         
***********************************************************************         
                                                                                
GOLINK   NTR1  LABEL=*                                                          
         LA    R2,RUNPARA                                                       
         USING RUNPARMD,R2                                                      
         XC    RUNPARMD(RUNPARML),RUNPARMD                                      
         STC   R1,RUNPMODE         Set mode for applications                    
         LA    R0,RUNFACS                                                       
         STCM  R0,7,RUNPARUN                                                    
         LA    RF,PMESSAGE                                                      
         STCM  RF,15,RUNPALOG                                                   
         CLI   RUNPMODE,RPRCCMDQ   If 'process command card' mode               
         JNE   *+8                                                              
         STCM  R0,15,RUNPACMD      R0 points to card image                      
         MVC   RSVRSAVE,ASVRSAVE   Set A(server save area)                      
                                                                                
         XC    CPURTP,CPURTP       Init RTPCPU accum for request                
         MVC   CPURTPNX,MCRTPCPU   Set initial RTPCPU limit value               
         OC    REQNO,REQNO         Test started running requests                
         JZ    GOLINK04            No, still initialising                       
         GOTOR GETCPU                                                           
         MVC   CPUREQ,CPUSAVE      CPU START                                    
                                                                                
GOLINK04 STCM  R2,15,LP_ARUNP      Set A(RUNPARMS)                              
         MVC   MCREQNO,REQNO       Set request number                           
                                                                                
         GOTOR VDDLINK,LP_D        Call DDLINK                                  
                                                                                
         OC    REQNO,REQNO         Test started running requests                
         JZ    GOLINK06            No, still initialising                       
         GOTOR GETCPU              CPU STOP                                     
         LG    GR1,CPUSAVE                                                      
         SLG   GR1,CPUREQ          SET CPU UNITS USED BY GOLINK/SERVER          
         STG   GR1,CPUREQ                                                       
                                                                                
GOLINK06 XC    MCREQNO,MCREQNO     Set not processing request                   
         LARL  RE,LP_XDB           Point to local LP_XDB                        
         ST    RE,LP_ALPXD         (DDLINK changes this)                        
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to resolve user-id values in WORKD                          *         
*                                                                     *         
* Ntry:- R1=0 to initialize else A(user-id number)                    *         
***********************************************************************         
                                                                                
GETUID   NTR1  LABEL=*,WORK=(RC,GUWORKL)                                        
         USING GUWORKD,RC          RC=A(local W/S)                              
         STCM  RF,8,GUMODE         Set calling mode                             
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         TM    GUMODE,GUMSWCH      Test switch mode (external call)             
         JZ    GETUID01                                                         
         LR    R2,R1               Yes, point to parameter list                 
         L     R1,0(R2)            Point to user id                             
         J     GETUID02                                                         
                                                                                
GETUID01 LTR   R1,R1               Test initialization call                     
         JNZ   GETUID02                                                         
         GOTOR TSAR,DMCB,('UIDBUFQ',TSAINI),('UIDKEYL',UIDRECL)                 
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   PMESSAGE(L'UIDLIT1),UIDLIT1                                      
         GOTOR PRTLOG                                                           
         J     EXITY                                                            
                                                                                
GETUID02 MVC   GUUID,0(R1)         Set user id                                  
         MVC   UIDUID,GUUID        Set user-id key                              
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   GETUID04                                                         
                                                                                
         GOTOR TSAR,DMCB,('UIDBUFQ',TSARDH)                                     
         JE    GETUID42                                                         
                                                                                
         MVC   PMESSAGE(L'UIDLIT3),UIDLIT3                                      
         LLH   R0,GUUID                                                         
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  PMESSAGE+L'UIDLIT3(5),DUB                                        
         GOTOR PRTLOG                                                           
                                                                                
GETUID04 MVC   UIDUID,GUUID        Build user-id buffer record                  
         XC    UIDDATA(UIDDATAL),UIDDATA                                        
                                                                                
         L     R3,AIO                                                           
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,UIDUID                                                   
         GOTOR DMGR,DMCB,DMREAD,CTFILE,CTIREC,CTIREC                            
         JNE   EXITN                                                            
                                                                                
         LA    R3,CTIDATA                                                       
         USING CTDSCD,R3                                                        
         SR    R0,R0                                                            
GETUID06 CLI   CTDSCEL,0                                                        
         JE    GETUID20                                                         
         CLI   CTDSCEL,CTDSCELQ                                                 
         JE    GETUID10                                                         
         CLI   CTDSCEL,CTAGYELQ                                                 
         JE    GETUID12                                                         
         CLI   CTDSCEL,CTIDOELQ                                                 
         JE    GETUID14                                                         
         CLI   CTDSCEL,CTSYSELQ                                                 
         JE    GETUID16                                                         
         CLI   CTDSCEL,CTDSTELQ                                                 
         JE    GETUID18                                                         
GETUID08 IC    R0,CTDSCLEN                                                      
         AR    R3,R0                                                            
         J     GETUID06                                                         
                                                                                
GETUID10 MVC   UIDCODE,CTDSC                                                    
         J     GETUID08                                                         
                                                                                
         USING CTAGYD,R3                                                        
GETUID12 MVC   UIDAGYID,CTAGYID                                                 
         MVC   UIDALANG,CTAGYLNG                                                
         J     GETUID08                                                         
                                                                                
         USING CTIDOD,R3                                                        
GETUID14 MVC   UIDOFL1,CTIDOFL1                                                 
         MVC   UIDOFL2,CTIDOFL2                                                 
         J     GETUID08                                                         
                                                                                
         USING CTSYSD,R3                                                        
GETUID16 LLC   R1,CTSYSNUM                                                      
         CHI   R1,UIDSYSNT                                                      
         JH    GETUID08                                                         
         MHI   R1,UIDSYSL                                                       
         LA    R1,UIDSYS-UIDSYSL(R1)                                            
UID      USING UIDSYS,R1                                                        
         MVC   UID.UIDSYSN,CTSYSNUM SET SYSTEM VALUES                           
         MVC   UID.UIDSYSSE,CTSYSSE                                             
         MVC   UID.UIDSYSAB,CTSYSAGB                                            
         MVC   UID.UIDSYSAC,CTSYSLMT                                            
         DROP  UID                                                              
                                                                                
         CLC   CTSYSNUM,MCOVSYS    Test batch mode system                       
         JNE   GETUID08                                                         
         MVC   MCIDSENO,CTSYSSE    Yes - set system values now                  
         MVC   MCIDACCS,CTSYSLMT                                                
         MVC   MCIDAGYB,CTSYSAGB                                                
         MVC   MCC1ACCS,CTSYSLMT                                                
         J     GETUID08                                                         
                                                                                
         USING CTDSTD,R3                                                        
GETUID18 MVC   UIDDNAM,CTDSTNAM                                                 
         MVC   UIDDADD,CTDSTADD                                                 
         J     GETUID08                                                         
                                                                                
GETUID20 L     R3,AIO                                                           
         USING CT5REC,R3           Now read access record                       
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         OC    CT5KALPH,UIDAGYID                                                
         JZ    EXITN                                                            
         MVC   UIDSECID,UIDAGYID   Preset security agency=actual                
         GOTOR DMGR,DMCB,DMREAD,CTFILE,CT5REC,CT5REC                            
         JNE   EXITN                                                            
                                                                                
         LA    R3,CT5DATA                                                       
         SR    R0,R0                                                            
         USING CTDSCD,R3                                                        
GETUID22 CLI   CTDSCEL,0           Test end of record                           
         JE    GETUID40                                                         
         CLI   CTDSCEL,CTDSCELQ                                                 
         JE    GETUID26                                                         
         CLI   CTDSCEL,CTSYSELQ                                                 
         JE    GETUID28                                                         
         CLI   CTDSCEL,CTAGDELQ                                                 
         JE    GETUID30                                                         
         CLI   CTDSCEL,CTSEAELQ                                                 
         JE    GETUID32                                                         
         CLI   CTDSCEL,CTAGCELQ                                                 
         JE    GETUID34                                                         
GETUID24 IC    R0,CTDSCLEN                                                      
         AR    R3,R0                                                            
         J     GETUID22                                                         
                                                                                
GETUID26 MVC   UIDAPRIN,CTDSC                                                   
         J     GETUID24                                                         
                                                                                
         USING CTSYSD,R3                                                        
GETUID28 LLC   R1,CTSYSNUM                                                      
         CHI   R1,UIDSYSNT                                                      
         JH    GETUID24                                                         
         MHI   R1,UIDSYSL                                                       
         LA    R1,UIDSYS-UIDSYSL(R1)                                            
UID      USING UIDSYS,R1                                                        
         CLI   UID.UIDSYSN,0                                                    
         JNE   GETUID24                                                         
         MVC   UID.UIDSYSN,CTSYSNUM                                             
         MVC   UID.UIDSYSSE,CTSYSSE                                             
         MVC   UID.UIDSYSAB,CTSYSAGB                                            
         J     GETUID24                                                         
         DROP  UID                                                              
                                                                                
         USING CTAGDD,R3                                                        
GETUID30 CLI   CTAGDLEN,CTAGDL2Q                                                
         JNE   GETUID24                                                         
         MVC   UIDACURR,CTAGDCUR                                                
         MVC   UIDACTRY,CTAGDCTY                                                
         MVC   MCAGCOPT,CTAGOPTS                                                
         J     GETUID24                                                         
                                                                                
         USING CTSEAD,R3                                                        
GETUID32 MVC   UIDSECID,CTSEAAID                                                
         J     GETUID24                                                         
                                                                                
         USING CTAGCD,R3                                                        
GETUID34 MVC   UIDAGYLB,CTAGCCOD                                                
         J     GETUID24                                                         
                                                                                
GETUID40 TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   EXITY                                                            
         GOTOR TSAR,DMCB,('UIDBUFQ',TSAADD)                                     
         JE    GETUID44                                                         
         DC    H'0'                                                             
                                                                                
GETUID42 MVC   PMESSAGE(L'UIDLIT2),UIDLIT2                                      
         GOTOR PRTLOG                                                           
                                                                                
GETUID44 MVC   LP_USRID,UIDUID                                                  
         MVC   LP_AGY,UIDAGYID     Set agency, user id & person in LP_D         
         MVC   LP_SECNO,GUPID#                                                  
         TM    GUMODE,GUMSWCH      Test switch mode (external call)             
         JZ    EXITY               No, all done                                 
                                                                                
         GOTOR SETSYS              Set system values                            
         MVC   LP_AGY,UIDAGYID     Set agency, user id & person in LP_D         
         MVC   LP_USRID,UIDUID                                                  
         MVC   LP_SECNO,GUPID#                                                  
         GOTOR RESSYS              Resolve user id/system values                
         JNE   EXIT                Exit if system is not available              
                                                                                
         ICM   R1,15,4(R2)         Do security if required                      
         JZ    EXITY                                                            
         MVC   GUSECV(GUSECVL),0(R1)                                            
                                                                                
         LARL  R3,SEC                                                           
         USING SECD,R3             Point to secret control block                
         MVI   SECINDS,0           Initialize secret control block              
         MVC   SECOAGY,UIDAGYID                                                 
         MVC   SECOAGYS,UIDSECID                                                
         MVC   SECOUSER,UIDUID                                                  
         MVC   SECOPASS,GUPID#                                                  
         MVC   SECOSAGN,GUSAGN                                                  
         MVC   SECOSYS,GUOSYS                                                   
         MVC   SECOPRG,GUOPRG                                                   
         MVC   SECOAGPE,GUAGPE                                                  
         OI    SECOFLAG,X'08'                                                   
         L     RF,LP_ACOM                                                       
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('SECPINIT+SECPOVAL',SECD),0                           
         GOTOR SETMCV              Set master control values                    
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
GUWORKD  DSECT ,                   ** GETUID local w/s **                       
                                                                                
GUMODE   DS    X                   ** Calling mode **                           
GUMSWCH  EQU   X'80'               Switch to SE (external call)                 
                                                                                
GUUID    DS    XL(L'CTIKNUM)       User-id number                               
                                                                                
GUSECV   DS    0X                  ** Security values **                        
GUPID#   DS    XL(L'SECOPASS)      Password number                              
GUSAGN   DS    XL(L'SECOSAGN)      Access group number                          
GUAGPE   DS    CL(L'SECOAGPE)      Person security agency                       
GUOSYS   DS    XL(L'SECOSYS)       System number                                
GUOPRG   DS    XL(L'SECOPRG)       Program number                               
GUSECVL  EQU   *-GUSECV                                                         
                                                                                
GUWORKL  EQU   *-GUWORKD                                                        
RUNNER   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Cpu time routines                                                   *         
***********************************************************************         
GETCPU   NTR1  LABEL=*             Get elapsed CPU time                         
         IEABRCX ENABLE                                                         
         TIMEUSED STORADR=CPUSAVE,LINKAGE=SYSTEM,CPU=MIC                        
         IEABRCX DISABLE                                                        
         XIT1                      EXIT WITH CPU IN DUB                         
         EJECT                                                                  
***********************************************************************         
* Set system SE values                                                *         
***********************************************************************         
                                                                                
SETSYS   LLC   RF,SVRSYSN1         Point to SE entry                            
         MHI   RF,UIDSYSL                                                       
         LA    RF,UIDSYS-UIDSYSL(RF)                                            
                                                                                
UID      USING UIDSYS,RF           RF=A(SE entry for system)                    
         CLI   UID.UIDSYSSE,0      Test SE number resolved                      
         JE    SETSYSN             No - bad exit                                
                                                                                
         MVC   MCIDSENO,UID.UIDSYSSE  Set system to switch to                   
         MVC   LP_SENO,UID.UIDSYSSE   Set SE# and agency binary in LP_D         
         MVC   LP_AGYB,UID.UIDSYSAB   Set agency binary value                   
                                                                                
         MVI   LP_SENO2,0          Clear 2nd system values                      
         MVI   LP_AGYB2,0                                                       
         SR    RF,RF                                                            
         ICM   RF,1,SVRSYSN2       Get/test second system OV#                   
         JZ    SETSYSY                                                          
         MHI   RF,UIDSYSL                                                       
         LA    RF,UIDSYS-UIDSYSL(RF)                                            
         CLI   UID.UIDSYSSE,0      Test SE number resolved                      
         JE    SETSYSY             No                                           
         MVC   LP_SENO2,UID.UIDSYSSE  Set SE# for 2nd system                    
         MVC   LP_AGYB2,UID.UIDSYSAB  Set agency binary for 2nd system          
                                                                                
SETSYSY  CR    RE,RE               System(s) set                                
         BR    RE                                                               
                                                                                
SETSYSN  LTR   RE,RE               Primary system not available                 
         BR    RE                                                               
         DROP  UID                                                              
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR                                                   *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows:-                   *         
*                                                                     *         
*        P1/0   - Buffer number                                       *         
*        P1/1-3 - TSAR action code                                    *         
*        P2/0   - Key length (initialization call)                    *         
*        P2/2-3 - Record length (initialization call)                 *         
***********************************************************************         
                                                                                
TSAR     NTR1  LABEL=*                                                          
         LR    R2,R1               R2=A(Parameter list)                         
                                                                                
         LARL  R3,TSRTAB                                                        
         USING TSRTABD,R3          Locate buffer definition                     
         LHI   R0,TSRTABN                                                       
TSAR0010 CLC   TSRTBUF,0(R2)       Match on buffer number                       
         JE    TSAR0020                                                         
         AHI   R3,TSRTABL                                                       
         JCT   R0,TSAR0010                                                      
         DC    H'0'                                                             
                                                                                
TSAR0020 LLH   RE,TSRTREC          Get displacement to record                   
         LA    RE,WORKD(RE)        RE=A(record)                                 
         LLH   R4,TSRTBLK          Get displacement to TSAR block               
         LA    R4,WORKD(R4)        R4=A(TSAR control block)                     
                                                                                
         USING TSARD,R4            R3=A(TSAR control block)                     
         MVC   TSACTN,3(R2)        Set action code                              
         ST    RE,TSAREC           Set a(record)                                
         CLI   TSACTN,TSAINI       Test initialization call                     
         JNE   TSAR0030                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,LP_ACOM                                                   
         MVC   TSKEYL,4(R2)        Set key length                               
         MVC   TSRECL,6(R2)        Set record size                              
         MVI   TSINDS,TSINODSK                                                  
         MVI   TSIND2,TSI2BIGN+TSI2MANY                                         
         MVC   TSRECI,TSRTIND      Set TSAR buffer flag                         
         MVC   TSBUFFL,TSRTSIZ     Set buffer size (nK)                         
                                                                                
TSAR0030 GOTOR LP_ATSAR,TSARD      Call TSAR to process action                  
         CLI   TSERRS,0            Set condition code                           
         J     EXIT                                                             
         DROP  R3,R4                                                            
                                                                                
TSRTABD  DSECT ,                   ** Buffer table layout **                    
TSRTBUF  DS    X                   Buffer number                                
TSRTBLK  DS    AL2                 Displacement to TSAR block                   
TSRTREC  DS    AL2                 Displacement to record                       
TSRTIND  DS    X                   TSAR indicator byte                          
TSRTSIZ  DS    AL2                 Buffer size                                  
TSRTABL  EQU   *-TSRTABD           Length of table entry                        
RUNNER   CSECT ,                                                                
                                                                                
         DS    0H                                                               
TSRTAB   DS    0XL(TSRTABL)        ** Buffer table **                           
UIDBUFQ  EQU   1                   UID buffer                                   
         DC    AL1(UIDBUFQ),AL2(TSARUIDS-WORKD,UIDREC-WORKD)                    
         DC    AL1(TSRTSARB+TSRXTN),AL2(96)                                     
TSRTABN  EQU    (*-TSRTAB)/L'TSRTAB                                             
                                                                                
***********************************************************************         
* Initialize MQ interface                                             *         
***********************************************************************         
MQINIT   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         TM    RUNINDMQ,RUNIMQQS   Test need MQ interface                       
         JZ    EXITY                                                            
                                                                                
         LARL  R4,MQGLOB           Point to MQ globals                          
         USING MQGLOB,R4                                                        
                                                                                
         TM    RUNINDS1,RUNIMQQM   Test queue manager given                     
         JNZ   *+6                                                              
         DC    H'0'                No - can't connect to MQ without it          
                                                                                
         CLI   MQINIFLG,C'Y'       MUST BE A RE-CONNECT                         
         JE    MQINIT01                                                         
                                                                                
         MVC   PMESSAGE(L'INFLIT7),INFLIT7                                      
         GOTOR PRTLOG                                                           
                                                                                
         SAM31 ,                   Acquire storage for MQ input buffer          
         L     R2,MQIBUFFL         R2=MQ input buffer size                      
         STORAGE OBTAIN,LENGTH=(2),LOC=ANY,BNDRY=PAGE,COND=YES                  
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                Can't acquire MQ buffer storage              
         ST    R1,MQIBUFF          Set for MQ gets                              
         ST    R1,RMQBUFF          Set for DDLINK's use                         
         SAM24 ,                                                                
                                                                                
         MVC   PMESSAGE(L'INFLIT8),INFLIT8                                      
         LA    R0,MQIBUFF                                                       
         GOTOR MCVHEXOU,DMCB,(R0),PMESSAGE+L'INFLIT8,L'MQIBUFF,0                
         LA    R1,PMESSAGE+L'INFLIT8+(L'MQIBUFF*2)                              
         MVC   0(L'INFLITA,R1),INFLITA                                          
         L     R0,MQIBUFFL                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  L'INFLITA(8,R1),DUB                                              
         GOTOR PRTLOG              Print log trace line                         
                                                                                
         SAM31 ,                   Acquire storage for MQ output buffer         
         L     R2,MQOBUFFL         R2=MQ output buffer size                     
         STORAGE OBTAIN,LENGTH=(2),LOC=ANY,BNDRY=PAGE,COND=YES                  
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                Can't acquire MQ buffer storage              
         ST    R1,MQOBUFF          Set for MQ puts                              
         A     R1,MQOBUFFL                                                      
         ST    R1,MQOBUFFX         Set A(end of buffer)                         
         SAM24 ,                                                                
                                                                                
         MVC   PMESSAGE(L'INFLIT9),INFLIT9                                      
         LA    R0,MQOBUFF                                                       
         GOTOR MCVHEXOU,DMCB,(R0),PMESSAGE+L'INFLIT9,L'MQIBUFF,0                
         LA    R1,PMESSAGE+L'INFLIT9+(L'MQIBUFF*2)                              
         MVC   0(L'INFLITA,R1),INFLITA                                          
         L     R0,MQOBUFFL                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  L'INFLITA(8,R1),DUB                                              
         GOTOR PRTLOG              Print log trace line                         
                                                                                
         GOTOR RGPHS,PARM,0,('LOADQ',CSQBCONN)                                  
         MVC   ACSQBCON,0(R1)      Set A(MQ connect)                            
         GOTOR RGPHS,(R1),0,('LOADQ',CSQBOPEN)                                  
         MVC   ACSQBOPN,0(R1)      Set A(MQ open)                               
         GOTOR RGPHS,(R1),0,('LOADQ',CSQBPUT)                                   
         MVC   ACSQBPUT,0(R1)      Set A(MQ put)                                
         GOTOR RGPHS,(R1),0,('LOADQ',CSQBDISC)                                  
         MVC   ACSQBDSC,0(R1)      Set A(MQ disconnect)                         
         GOTOR RGPHS,(R1),0,('LOADQ',CSQBGET)                                   
         MVC   ACSQBGET,0(R1)      Set A(MQ get)                                
         GOTOR RGPHS,(R1),0,('LOADQ',CSQBCOMM)                                  
         MVC   ACSQBCOM,0(R1)      Set A(MQ commit)                             
         GOTOR RGPHS,(R1),0,('LOADQ',CSQBCLOS)                                  
         MVC   ACSQBCLO,0(R1)      Set A(MQ close)                              
                                                                                
MQINIT01 MVI   MQINIFLG,C'Y'       Set INIT FLAG to Y - Reconnect here          
                                                                                
         GOTOR RCALLMQ,MQCON       Connect to MQ manager                        
         JNE   EXITL                                                            
                                                                                
         TM    RUNINDMQ,RUNIMQEI   Test EDIIQ= parameter processed              
         JZ    MQINIT02                                                         
         LA    R0,MQEDIDSC                                                      
         ST    R0,MQOPNQNM                                                      
         LA    R0,MQEDIHOB                                                      
         ST    R0,MQOPNHOB                                                      
         LHI   R0,MQOO_INPUT_AS_Q_DEF+MQOO_SAVE_ALL_CONTEXT                     
         ST    R0,MQOPENO                                                       
         LHI   R0,MQOT_Q           Object is a queue                            
         ST    R0,MQEDIDSC_OBJECTTYPE                                           
         MVC   MQEDIDSC_OBJECTNAME,MQEDINAM                                     
         GOTOR RCALLMQ,MQOPN       Open EDI input queue                         
         JE    MQINIT02                                                         
         DC    H'0'                                                             
                                                                                
MQINIT02 TM    RUNINDMQ,RUNIMQEO   Test EDIOQ= parameter processed              
         JZ    EXIT                                                             
         LA    R0,MQEDODSC                                                      
         ST    R0,MQOPNQNM                                                      
         LA    R0,MQEDOHOB                                                      
         ST    R0,MQOPNHOB                                                      
         LHI   R0,MQOO_OUTPUT                                                   
         ST    R0,MQOPENO                                                       
         LHI   R0,MQOT_Q           Object is a queue                            
         STCM  R0,15,MQEDODSC_OBJECTTYPE                                        
         LHI   R0,MQPMO_SYNCPOINT+MQPMO_FAIL_IF_QUIESCING                       
         STCM  R0,15,MQEDOOPT_OPTIONS                                           
         MVC   MQEDOOPT_CONTEXT,MQEDOHOB                                        
         MVC   MQEDODSC_OBJECTNAME,MQEDONAM                                     
         GOTOR RCALLMQ,MQOPN       Open EDI output queue                        
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R4,RB                                                            
         EJECT                                                                  
***********************************************************************         
* Set master control values                                           *         
***********************************************************************         
                                                                                
SETMCV   MVC   MCUSERID,UIDCODE                                                 
         MVC   MCORIGID,UIDUID                                                  
         MVC   MCDESTID,UIDUID                                                  
         MVC   MCUSER,UIDAGYID                                                  
         MVC   MCIDAGYA,UIDAGYID                                                
         MVC   MCLANG,UIDALANG                                                  
         MVC   MCIDOPTS,UIDOFL1                                                 
         MVC   MCIDOPT2,UIDOFL2                                                 
         MVC   MCORIGIN,UIDDNAM                                                 
         MVC   MCORIGAD,UIDDADD                                                 
         MVC   MCAGCURR,UIDACURR                                                
         MVC   MCCTRY,UIDACTRY                                                  
         MVC   MCAGCTRY,UIDACTRY                                                
SETMCVX  BR    RE                                                               
                                                                                
***********************************************************************         
* Set timer if required                                               *         
***********************************************************************         
                                                                                
SETPOP   NTR1  LABEL=*                                                          
         MVI   MCPOPSW,0           Resets if suspended by abend                 
         MVI   ADWAITNT,0          ADWAIT not yet entered this time             
         LARL  R2,LOOPTIMR                                                      
         LARL  R3,TIMPOP                                                        
         STIMER TASK,(3),BINTVL=(2)                                             
SETPOPX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Read and validate input parameter cards                             *         
***********************************************************************         
                                                                                
VALPAR   NTR1  LABEL=*,WORK=(RC,VPWORKL)                                        
         USING VPWORKD,RC          RC=A(local w/s)                              
         XC    VPWORKD(VPWORKL),VPWORKD                                         
         ST    R1,VPPARM                                                        
         LARL  R4,MQGLOB           Point to MQ globals                          
         USING MQGLOB,R4                                                        
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         MVC   P,SPACES                                                         
         OC    VPPARM,VPPARM                                                    
         JZ    VALPAR02                                                         
         MVC   VPCARD,0(R1)                                                     
         MVC   PMESSAGE(L'CMDLIT1),CMDLIT1                                      
         MVC   PMESSAGE+L'CMDLIT1(L'VPCARD),VPCARD                              
         GOTOR PRTLOG                                                           
         J     VALPAR06                                                         
                                                                                
VALPAR02 MVC   TITLE(L'RUNTITLE),RUNTITLE                                       
         GOTOR PRTLOG                                                           
         MVC   PMESSAGE(L'VALLIT1),VALLIT1                                      
         GOTOR PRTLOG                                                           
         MVC   PMESSAGE(L'VALLIT2),VALLIT2                                      
         GOTOR PRTLOG                                                           
*&&UK*&& OI    RUNINDS2,RUNISYSM   set SYSMDUMP=Y default                       
                                                                                
VALPAR04 OC    VPPARM,VPPARM       Test single card passed                      
         JNZ   VALPARE                                                          
         GOTOR MCVCARDS,VPPARA,VPCARD,=C'R'                                     
         MVC   PMESSAGE(L'VPCARD),VPCARD                                        
         GOTOR PRTLOG                                                           
                                                                                
VALPAR06 TM    RUNINDS1,RUNIFALK   Test FALINK data                             
         JNZ   VALFALK                                                          
         CLI   VPCARD,SLASH                                                     
         JE    VALPAR24                                                         
         CLI   VPCARD,SPACE        Ignore comments                              
         JE    VALPAR04                                                         
         CLI   VPCARD,ASTERISK                                                  
         JE    VALPAR04                                                         
                                                                                
         CLI   VPCARD,PLUS         Test server application control card         
         JNE   VALPAR12                                                         
         OC    SVRADDR,SVRADDR     Test server is loaded                        
         JNZ   VALPAR08                                                         
         MVI   VPERROR,TQENOSVR    Set 'no server' error                        
         GOTOR GETTQE,VPERROR                                                   
         MVC   PMESSAGE(L'TQETAB-1),1(RF)                                       
         GOTOR PRTLOG              Print completion message                     
         J     VALPARH                                                          
                                                                                
VALPAR08 MVC   VPCARD(L'VPCARD-1),VPCARD+1                                      
         MVI   VPCARD+L'VPCARD-1,SPACE                                          
         LA    R0,VPCARD           Point to card for DDLINK call                
         GOTOR RGOLINK,RPRCCMDQ                                                 
         JE    VALPAR04                                                         
         J     VALPARX                                                          
                                                                                
VALPAR12 LA    RE,VPCARD                                                        
         LHI   R0,L'VALTKWRD+1                                                  
         SR    RF,RF                                                            
VALPAR14 CLI   0(RE),EQUAL                                                      
         JE    VALPAR16                                                         
         CLI   0(RE),SPACE                                                      
         JE    VALPAR16                                                         
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         JCT   R0,VALPAR14                                                      
         MVC   PMESSAGE(L'VALLIT3),VALLIT3                                      
         GOTOR PRTLOG                                                           
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   VALPAR04            Yes - ignore bad parameter cards             
         J     VALPARH                                                          
                                                                                
VALPAR16 LTR   RF,RF                                                            
         JZ    VALPARH                                                          
         BCTR  RF,0                                                             
         LA    R1,1(RE)            R1=A(keyword data)                           
         LARL  RE,VALTAB                                                        
         USING VALTABD,RE                                                       
VALPAR18 CLI   VALTABD,VALTEOTQ                                                 
         JNE   VALPAR20                                                         
         MVC   PMESSAGE(L'VALLIT4),VALLIT4                                      
         GOTOR PRTLOG                                                           
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   VALPAR04            Yes - ignore bad parameter cards             
         J     VALPARH                                                          
                                                                                
VALPAR20 EX    RF,VPARCLKC                                                      
         JE    VALPAR22                                                         
         CLI   VALTKWRD+2,C'*'     Test generic prefix (system)                 
         JNE   *+14                                                             
         CLC   VALTKWRD(2),VPCARD  Yes - match on 2 characters only             
         JE    VALPAR22                                                         
         AHI   RE,VALTABL                                                       
         J     VALPAR18                                                         
                                                                                
VALPAR22 ICM   RF,15,VALTROUT                                                   
         CR    RF,RF                                                            
         BASR  RE,RF                                                            
         JE    VALPAR04                                                         
         MVC   PMESSAGE(L'VALLIT5),VALLIT5                                      
         GOTOR PRTLOG                                                           
         J     VALPARH                                                          
                                                                                
VALPAR24 OC    VPPARM,VPPARM       Return low if single card eof                
         JNZ   VALPARL                                                          
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   VALPAR26                                                         
         OC    SVRADDR,SVRADDR     Ensure we know what the server is            
         JNZ   VALPAR26                                                         
         MVC   PMESSAGE(L'VALLIT6),VALLIT6                                      
         GOTOR PRTLOG                                                           
         J     VALPARH                                                          
                                                                                
VALPAR26 L     R1,MCRTPCPU         RTPCPU must be exact multiple                
         XR    R0,R0               of LOOPTIMR                                  
         D     R0,LOOPTIMR                                                      
         LTR   R0,R0                                                            
         JZ    VALPAR28                                                         
         MVC   PMESSAGE(L'VALLIT7),VALLIT7                                      
         GOTOR PRTLOG                                                           
         J     VALPARH                                                          
                                                                                
VALPAR28 ZAP   LINE,MAXLINE                                                     
         J     VALPARE                                                          
                                                                                
VALPARL  LHI   RF,0                                                             
         J     VALPARCC                                                         
VALPARH  LHI   RF,2                Set CC=high on error                         
         J     VALPARCC                                                         
VALPARE  LHI   RF,1                Set CC=equal if no errors                    
VALPARCC CHI   RF,1                                                             
VALPARX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Validate FALINK start/string/end                                    *         
***********************************************************************         
                                                                                
VALFALS  OI    RUNINDS1,RUNIFALK                                                
         XC    MQLIMSG,MQLIMSG                                                  
         J     VALEXIT                                                          
                                                                                
VALFALK  LARL  RF,FALINKXL         Test for <FALINKX> statement                 
         CLC   VPCARD(L'VALTKWRD),0(RF)                                         
         JE    VALFALE                                                          
         L     R1,MQIBUFF          Here for stacking                            
         ICM   R0,15,MQLIMSG                                                    
         AR    R1,R0                                                            
         SAM31 ,                                                                
         TM    RUNINDS1,RUNIFAFS   Test first card                              
         JNZ   VALFALK2                                                         
         OI    RUNINDS1,RUNIFAFS                                                
         CLI   VPCARD+1,C'='       Test if pointing to map code                 
         JE    VALFALK2                                                         
         MVC   0(75,R1),VPCARD+4   Discard first 4 characters                   
         AHI   R1,79-4                                                          
         AHI   R0,79-4                                                          
         J     VALFALK4                                                         
VALFALK2 MVC   0(79,R1),VPCARD     Here for all others                          
         AHI   R1,79                                                            
         AHI   R0,79                                                            
VALFALK4 SAM24 ,                                                                
         ST    R0,MQLIMSG                                                       
         J     EXITY                                                            
                                                                                
VALFALE  OC    MQLIMSG,MQLIMSG     Test we have some data stacked               
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   RMQLDATA,MQLIMSG    Set length of incoming message               
         NI    RUNINDS1,FF-(RUNIFALK+RUNIFAFS)                                  
         OI    RUNINDS1,RUNIGOMQ   Process as if from queue                     
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate batch mode                                                 *         
***********************************************************************         
                                                                                
VALBTCH  OI    RUNINDS1,RUNIBTCH   Set batch mode processing                    
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate updative mode                                              *         
***********************************************************************         
                                                                                
VALUPDT  OI    RUNINDS1,RUNIUPDT   Set updative (global) mode                   
         LARL  RF,SSB              Turn on global flags in SSB                  
         OI    SSOSTAT2-SSOOFF(RF),SSOSLOCK+SSOSGALO+SSOSROLC                   
         OI    SSOFLAG1-SSOOFF(RF),SSOFRCVR                                     
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate FLASH=valid flash character                                *         
***********************************************************************         
                                                                                
VALFLSH  LARL  RF,SSB              Turn on global flags in SSB                  
         CLI   0(R1),C'A'          test if valid flash character                
         BLR   RE                                                               
         CLI   0(R1),NOQ           FLASH=N means no flash copy                  
         BER   RE                                                               
         OI    SSOFLAG2-SSOOFF(RF),SSO2FLSH                                     
         MVC   SSOFLSHI-SSOOFF(,RF),0(R1)                                       
         CLI   0(R1),YESQ          FLASH=Y is default so set to FLASH=S         
         JNE   *+8                 to cause DSN=FLS.xxx...                      
         MVI   SSOFLSHI-SSOOFF(RF),C'S'                                         
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate STATE=Y/N                                                  *         
***********************************************************************         
                                                                                
VALSTAT  EQU   *                                                                
*                                                                               
         OI    RUNINDS2,RUNISTAT                                                
         CLI   0(R1),C'Y'                                                       
         BER   RE                                                               
         NI    RUNINDS2,X'FF'-RUNISTAT                                          
         CLI   0(R1),C'N'                                                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate SYSMDUMP=Y/N                                               *         
***********************************************************************         
                                                                                
VALSYSM  EQU   *                                                                
*                                                                               
         OI    RUNINDS2,RUNISYSM                                                
         CLI   0(R1),C'Y'                                                       
         BER   RE                                                               
         NI    RUNINDS2,X'FF'-RUNISYSM                                          
         CLI   0(R1),C'N'                                                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate SMTP=Y/N                                                   *         
***********************************************************************         
                                                                                
VALSMTP  EQU   *                                                                
*                                                                               
         L     RF,VSMTP                                                         
         MVC   0(2,RF),=X'90EC'                                                 
         CLI   0(R1),C'Y'                                                       
         BER   RE                                                               
         MVC   0(2,RF),=X'07FE'                                                 
         CLI   0(R1),C'N'                                                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate LOAD=phasename(/x)(+ddd=xxxx)                              *         
***********************************************************************         
                                                                                
VALLOAD  TM    RUNISVRS,RUNISVR1   Test have processed SERVER card              
         JZ    *+8                                                              
         LTR   RE,RE               Yes - can't issue load                       
         BR    RE                                                               
         LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTOR RGPHS,VPPARA,0,('LOADQ',(RF))                                    
         LR    RE,R0                                                            
         BR    RE                  (CC set by GPHS routine)                     
                                                                                
***********************************************************************         
* Validate VERIFY=phasename(/x)(+ddd=xxxx), VERIFY=*(mapname)+ddd=xxx *         
***********************************************************************         
                                                                                
VALVRFY  LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTOR RGPHS,VPPARA,0,('VERIFYQ',(RF))                                  
         LR    RE,R0                                                            
         BR    RE                  (CC set by GPHS routine)                     
                                                                                
***********************************************************************         
* Validate PATCH=phasename(/x)(+ddd=xxxx), PATCH=*(mapname)+ddd=xxxx  *         
***********************************************************************         
                                                                                
VALPTCH  LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTOR RGPHS,VPPARA,0,('PATCHQ',(RF))                                   
         LR    RE,R0                                                            
         BR    RE                  (CC set by GPHS routine)                     
                                                                                
***********************************************************************         
* Validate DDSIO=phasename                                            *         
***********************************************************************         
                                                                                
VALDDIO  L     RF,MCVDDSIO                                                      
         MVC   0(8,RF),0(R1)                                                    
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate DSPACE=dataspacename                                       *         
* Note: US any call that goes through DDSIO connects to the TABS      *         
*       data space so, SSODSPAC has to be set ASAP.                   *         
***********************************************************************         
                                                                                
VALDSPC  NTR1  LABEL=NO                                                         
         TM    RUNINDS1,RUNIBTCH   Test batch mode                              
         JNZ   VALDSPCB                                                         
                                                                                
         MVC   DSPACE,0(R1)        Set DSPACE value provided                    
                                                                                
         CLI   DSPACE+1,SPACE      Test one character id given                  
         JNE   VALDSPC2                                                         
*                                                                               
         LARL  RF,SSB              Yes - set and call LOCKSPC                   
         MVC   SSODSPAC-SSOOFF(,RF),DSPACE                                      
         GOTOR VLOCKSPC,DMCB,X'00030000',0,ADSPCECB,AOPERECB                    
         LAM   AR0,ARF,ARZERO                                                   
*                                                                               
         GOTOR VLOCKSPC,DMCB,(X'20',DTCTBL),0                                   
         LAM   AR0,ARF,ARZERO                                                   
*                                                                               
         LARL  RF,SSB                                                           
         MVC   ALET,SSOTBLET-SSOOFF(RF)                                         
         J     VALDSPC4                                                         
                                                                                
VALDSPC2 XC    WORK,WORK           Full tabs dataspace name provided            
         MVC   WORK(4),GETA                                                     
         MVC   WORK+4(L'DSPACE),DSPACE                                          
         LA    R1,WORK                                                          
         LHI   R0,-21                                                           
         SVC   247                                                              
         LTR   RF,RF                                                            
         JNZ   VALDSPE1                                                         
                                                                                
         MVC   ALET,WORK+24                                                     
         LARL  RF,SSB              Set ALET in SSB                              
         MVC   SSOTBLET-SSOOFF(,RF),ALET                                        
         MVC   SSODSPAC-SSOOFF(,RF),DSPACE+3                                    
*&&US*&& CLI   SSODSPAC-SSOOFF(RF),C'P'                                         
*&&US*&& JNE   *+8                                                              
*&&US*&& MVI   SSODSPAC-SSOOFF(RF),C'A'                                         
*                                                                               
         GOTOR VLOCKSPC,DMCB,X'00030000',0,ADSPCECB,AOPERECB                    
         LAM   AR0,ARF,ARZERO                                                   
                                                                                
VALDSPC4 OC    ALET,ALET           Test ALET is resoved                         
         JZ    VALDSPE1                                                         
         MVC   RALET,ALET                                                       
                                                                                
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,OFFSET                                                     
         SAC   512                                                              
         ICM   R2,15,TABSRUN-FATABSD(R2)                                        
         USING TABSRUND,R2                                                      
         MVC   WORK(L'TABSLRUN),TABSLRUN                                        
         DROP  R2                                                               
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
                                                                                
         LTR   R2,R2               Test run table resolved                      
         JZ    VALDSPE2                                                         
         STC   R2,DUB                                                           
         TM    DUB,3               Test run table on fullword boundary          
         JNZ   VALDSPE3                                                         
         CLC   RUNEYE,WORK         Test eye-catcher matches                     
         JNE   VALDSPE4                                                         
                                                                                
         L     RF,MCUTL            Set control system files                     
         MVI   4(RF),CONSYSQ                                                    
         GOTOR DMGR,DMCB,(0,DMOPEN),CONSYS,CONLST,WORK,0                        
         SR    RF,RF                                                            
         J     VALPARE                                                          
                                                                                
VALDSPCB LARL  RF,SSB              Batch mode - just set DMGR tabs id           
         MVC   SSODSPAC-SSOOFF(,RF),0(R1)                                       
         J     VALPARE                                                          
                                                                                
VALDSPE1 MVC   PMESSAGE(L'SPCLIT1),SPCLIT1                                      
         J     VALDSPER                                                         
                                                                                
VALDSPE2 MVC   PMESSAGE(L'SPCLIT2),SPCLIT2                                      
         J     VALDSPER                                                         
                                                                                
VALDSPE3 MVC   PMESSAGE(L'SPCLIT3),SPCLIT3                                      
         J     VALDSPER                                                         
                                                                                
VALDSPE4 DC    H'0'                                                             
                                                                                
VALDSPER GOTOR PRTLOG                                                           
         J     VALPARH                                                          
                                                                                
***********************************************************************         
* Validate UPDID=update id                                            *         
***********************************************************************         
                                                                                
VALUPID  NTR1  LABEL=NO                                                         
         MVC   MCUPDID,0(R1)                                                    
         GOTOR DMGR,DMCB,DMUPDID                                                
         L     R1,12(R1)                                                        
         MVC   0(2,R1),MCUPDID                                                  
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate RTPTIME=value                                              *         
***********************************************************************         
                                                                                
VALRTPT  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         MHI   R0,100                                                           
         ST    R0,MCRTPTCB                                                      
         ST    R0,MCNXTTCB                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate RTPPAGES=value                                             *         
***********************************************************************         
                                                                                
VALRTPP  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,MCRTPPGS                                                      
         ST    R0,MCNXTPGS                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate RTPEXCP=value                                              *         
***********************************************************************         
                                                                                
VALRTPE  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,MCRTPIOS                                                      
         ST    R0,MCNXTIOS                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate RTPCPU=value                                               *         
***********************************************************************         
                                                                                
VALRTPC  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         MHI   R0,100                                                           
         ST    R0,MCRTPCPU                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate LOOPSECS=value (default=1 second)                          *         
***********************************************************************         
                                                                                
VALLOOP  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         LTR   R0,R0               Mustn't be zero                              
         JZ    VALPARH                                                          
         MHI   R0,100                                                           
         ST    R0,LOOPTIMR                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate MAXQTIME=value (assume seconds unless followed by ',T')    *         
***********************************************************************         
                                                                                
VALQTIM  NTR1  LABEL=NO                                                         
         MVI   NUMTYPE,C'T'        Set defaults to timer units                  
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,MAXQTIME         Set maximum queue time in TUs                
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate AVGQTIME=value (assume seconds unless followed by ',T')    *         
***********************************************************************         
                                                                                
VALATIM  NTR1  LABEL=NO                                                         
         MVI   NUMTYPE,C'T'        Set defaults to timer units                  
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,AVGQTIME         Set maximum queue time in TUs                
         OC    SAMPLE,SAMPLE                                                    
         JNZ   VALPARE                                                          
         LHI   R0,1                Set default sample if not known              
         ST    R0,SAMPLE                                                        
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate SAMPLE=value (AVGQTIME sample size)                        *         
***********************************************************************         
                                                                                
VALSAMP  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,SAMPLE                                                        
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate MAXPOPS=value                                              *         
***********************************************************************         
                                                                                
VALPOPS  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,MAXPOPS                                                       
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate MAXSEGS=value                                              *         
***********************************************************************         
                                                                                
VALSEGS  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         STH   R0,RMAXSEGS                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate MAXDUMPS=value                                             *         
***********************************************************************         
                                                                                
VALMDMP  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         STCM  R0,7,MAXDUMPS                                                    
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate DDLYRTRY=value                                             *         
***********************************************************************         
                                                                                
VALDDLR  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         CHI   R0,10                                                            
         JH    VALPARH                                                          
         STC   R0,MAXDDLY                                                       
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate DDLYWT1=value (100ths second, max 10 seconds)              *         
***********************************************************************         
                                                                                
VALDDL1  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         CHI   R0,1000                                                          
         JH    VALPARH                                                          
         ST    R0,DDLYWT1                                                       
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate DDLYWTMX=value (100ths second, max 120 seconds)            *         
***********************************************************************         
                                                                                
VALDDLM  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         CHI   R0,12000                                                         
         JH    VALPARH                                                          
         ST    R0,DDLYWTMX                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate OPMESS=x                                                   *         
***********************************************************************         
                                                                                
VALOPMS  MVC   OPMESS,0(R1)                                                     
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate OPREPLY=x                                                  *         
***********************************************************************         
                                                                                
VALOPRP  MVC   OPREPLY,0(R1)                                                    
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate INFOMESS=x                                                 *         
***********************************************************************         
                                                                                
VALIFMS  MVC   INFOMESS,0(R1)                                                   
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate RUN=TEST/NO                                                *         
***********************************************************************         
                                                                                
VALRUNT  NTR1  LABEL=NO                                                         
         MVI   MCTSTRUN,0                                                       
         CLI   0(R1),NOQ                                                        
         JE    VALPARE                                                          
         CLI   0(R1),C'T'                                                       
         JNE   VALPARH                                                          
         MVI   MCTSTRUN,FF                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate TRAPDIR=dirname,keylen                                     *         
***********************************************************************         
                                                                                
VALTDIR  NTR1  LABEL=*                                                          
         MVI   BYTE1,DMTTDIRQ      Set directory                                
         GOTOR ADDDMT                                                           
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate TRAPFIL=filename                                           *         
***********************************************************************         
                                                                                
VALTFIL  NTR1  LABEL=*                                                          
         MVI   BYTE1,DMTTFILQ      Set D/A file                                 
         GOTOR ADDDMT                                                           
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate TRAPVIS=dirname,keylen                                     *         
***********************************************************************         
                                                                                
VALTVIS  NTR1  LABEL=*                                                          
         MVI   BYTE1,DMTTVISQ      Set V/L I/S file                             
         GOTOR ADDDMT                                                           
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Create a datamanager trap table entry                               *         
***********************************************************************         
                                                                                
ADDDMT   NTR1  LABEL=*                                                          
         MVC   BYTE2,0(R1)         Save parameter value                         
         AHI   R1,L'TRPPARM                                                     
         LA    RF,1(R1)            Point to file name                           
         LHI   R0,7                                                             
ADDDMT02 AHI   RF,1                                                             
         CLI   0(RF),COMMA                                                      
         JE    ADDDMT04                                                         
         CLI   0(RF),SPACE                                                      
         JE    ADDDMT04                                                         
         JCT   R0,ADDDMT02                                                      
         J     VALPARH                                                          
                                                                                
ADDDMT04 LARL  R3,DMTTAB           Point to next available entry                
         USING DMTTABD,R3                                                       
         USING TRPVALD,DMTVALS                                                  
         LHI   R0,1                                                             
         OC    DMTNUM,DMTNUM       Test first                                   
         JZ    ADDDMT06                                                         
         LLH   RE,DMTNUM           No - bump last                               
         LA    R0,1(RE)                                                         
         MHI   RE,DMTTABL                                                       
         AR    R3,RE               Point to it                                  
                                                                                
ADDDMT06 STH   R0,DMTNUM           Set number of table entries                  
         MVC   DMTTYPE,BYTE1       Set file type                                
         MVC   TRPPARM,BYTE2       Set parameter value                          
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         STC   RF,DMTCLCL          Set name compare length                      
         EX    RF,ADMTMVF1                                                      
         LA    R1,1(RF,R1)         Point to length attribute                    
         CLI   DMTTYPE,DMTTOIOQ    Test IOOTHER entry                           
         JE    ADDDMT10                                                         
         CLI   0(R1),COMMA         Test we have key length                      
         JNE   ADDDMT08                                                         
         AHI   R1,1                                                             
         GOTOR VNUM                Yes - validate it                            
         JNE   VALPARH                                                          
         CHI   R0,MAXKEYLN         Test input versus maximum                    
         JH    VALPARH                                                          
         STC   R0,DMTKLEN          Set key length                               
                                                                                
ADDDMT08 L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         LARL  R1,DMTR             Point to RUNNER trace routine                
         ST    R1,CDATAMGR         Set as DATAMGR in COMFACSD                   
         ST    R1,MCVDMGR          Set as DATAMGR in MASTD                      
         J     VALPARE                                                          
                                                                                
ADDDMT10 ST    R3,AIOOTHER         Save address of IOOTHER for DMTR             
         MVC   DMTNAME,SPACES      Copy name for display                        
         MVC   DMTNAME(L'DMTFILE),DMTFILE                                       
         J     VALPARE                                                          
         DROP  R3,RF                                                            
                                                                                
***********************************************************************         
* Validate TRAPAPP=Y                                                  *         
***********************************************************************         
                                                                                
VALTAPP  CLI   0(R1),YESQ                                                       
         BNER  RE                                                               
         OI    LP_FLAG2,LP_FTRAP                                                
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate TRAPCALL=module name/phase name                            *         
***********************************************************************         
                                                                                
VALTCAL  NTR1  ,                                                                
         LR    RF,R1                                                            
         GOTOR RGPHS,VPPARA,0,('TRAPQ',(RF)),0,0                                
         JNE   VALPARH                                                          
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate DOWNASUP=N/Y                                               *         
***********************************************************************         
                                                                                
VALDASU  NI    LP_FLAG2,FF-(LP_FDAUQ)                                           
         CLI   0(R1),NOQ                                                        
         JE    VALEXIT                                                          
         CLI   0(R1),YESQ                                                       
         JNE   VALPARH                                                          
         OI    LP_FLAG2,LP_FDAUQ   Set DOWNASUP option for DDLINK               
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate TEST=01x/02x/03x/04x (where x=test level)                  *         
***********************************************************************         
                                                                                
VALTEST  NTR1  LABEL=NO                                                         
         LA    RE,MCTEST1                                                       
         CLI   1(R1),C'1'                                                       
         JE    VALTEST1                                                         
         LA    RE,MCTEST2                                                       
         CLI   1(R1),C'2'                                                       
         JE    VALTEST1                                                         
         LA    RE,MCTEST3                                                       
         CLI   1(R1),C'3'                                                       
         JE    VALTEST1                                                         
         LA    RE,MCTEST4                                                       
         CLI   1(R1),C'4'                                                       
         JNE   VALPARH                                                          
                                                                                
VALTEST1 MVC   0(L'MCTEST1,RE),2(R1)                                            
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate ESTAE=Y/N                                                  *         
***********************************************************************         
                                                                                
VALESTA  MVC   MCESTAE,0(R1)                                                    
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate SWAPABLE=Y/N                                               *         
***********************************************************************         
                                                                                
VALSWAP  MVC   SWAPABLE,0(R1)                                                   
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate TRACE=B(oth)/L(og)/P(rint)                                 *         
***********************************************************************         
                                                                                
VALTRCE  NTR1  LABEL=NO                                                         
         MVC   MCTRACE,0(R1)                                                    
         MVC   MCVPRINT,VPRINT     Reset MCVPRINT                               
         CLI   0(R1),C'P'          Logging to print only                        
         JE    *+12                                                             
         CLI   0(R1),C'B'          Logging to print and file                    
         JE    *+12                                                             
         CLI   0(R1),C'L'          Test logging trace                           
         JNE   VALPARE                                                          
         LARL  R0,TRCLOG                                                        
         ST    R0,MCVPRINT                                                      
         J     VALPARE                                                          
                                                                                
                                                                                
***********************************************************************         
* Validate REQTRACE=1-9 or REQTRACE=F                                 *         
***********************************************************************         
                                                                                
VALRTRC  NTR1  LABEL=NO                                                         
         MVI   REQTRACE,0                                                       
         CLI   0(R1),C'N'          No request trace (ie reset)                  
         JE    VALPARX                                                          
         CLI   0(R1),C'F'          Full request trace                           
         JE    VALRTRC2                                                         
         CLI   0(R1),C'1'          One line request trace                       
         JL    VALPARH                                                          
         CLI   0(R1),C'9'          UP TO NINE LINES                             
         JH    VALPARH                                                          
VALRTRC2 MVC   REQTRACE,0(R1)      Set only if valid (may be Op cmd)            
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate DUMP=Y/O/P                                                 *         
***********************************************************************         
                                                                                
VALDUMP  MVC   MCDUMP,0(R1)                                                     
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate WRITE=Y/N                                                  *         
***********************************************************************         
                                                                                
VALWRTE  MVC   MCWRITE,0(R1)                                                    
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate JESMAIL=phasename                                          *         
***********************************************************************         
                                                                                
VALJESM  MVC   JESMAIL,0(R1)                                                    
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate QMAINTR=Y/N                                                *         
***********************************************************************         
                                                                                
VALQMNT  MVC   QMAINTR,0(R1)                                                    
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate QMLIST=e-mail address list                                 *         
***********************************************************************         
                                                                                
VALQLST  MVC   QMLIST,0(R1)                                                     
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate GLOBAL=* (all systems) or list of systems                  *         
***********************************************************************         
                                                                                
VALGLOB  NTR1  LABEL=NO                                                         
         LARL  R3,SEOLST                                                        
         USING SEOLSTD,R3                                                       
         CLI   0(R3),0             Test anything in SEOLST                      
         JE    VALPARH             (must have processed SERVER= card)           
         TM    RUNINDS1,RUNIUPDT   Must be in updative mode                     
         JZ    VALPARH             (must have processed updative card)          
         CLI   0(R1),C'*'          Test flag all systems as global              
         JE    VALGLOBA                                                         
                                                                                
VALGLOB1 LR    RF,R1               Get system name in work                      
         MVC   WORK(L'SEOLNAM),SPACES                                           
         LA    RE,WORK                                                          
         LHI   R0,L'SEOLNAM+1                                                   
VALGLOB2 CLI   0(RF),SPACE         Test end of input string                     
         JE    VALGLOB3                                                         
         CLI   0(RF),COMMA         or list delimiter                            
         JE    VALGLOB3                                                         
         MVC   0(1,RE),0(RF)       Copy this character                          
         AHI   RF,1                Bump input                                   
         AHI   RE,1                Bump output                                  
         JCT   R0,VALGLOB2                                                      
         J     VALPARH                                                          
                                                                                
VALGLOB3 CLC   WORK(L'SEOLNAM),SPACES                                           
         JE    VALPARH                                                          
         LARL  R3,SEOLST           Locate system entry                          
VALGLOB4 CLI   SEOLNUM,SEOLEOTQ    Test end of system list                      
         JE    VALPARH                                                          
         CLC   SEOLNAM,WORK        Match system name to list                    
         JE    VALGLOB5                                                         
         AHI   R3,SEOLSTL          No match - bump to next entry                
         J     VALGLOB4                                                         
VALGLOB5 TM    SEOLFLAG,SEOLFOPN                                                
         JNZ   VALPARH                                                          
         TM    RUNINDS2,RUNISTAT   If STATE=Y                                   
         JO    *+12                                                             
         TM    SEOLFLAG,SEOLFGLB   OK if already global                         
         JNZ   VALPARH                                                          
         OI    SEOLFLAG,SEOLFGLB   Set global system flag                       
         CLI   0(RF),SPACE         Set end of input string                      
         JE    VALPARE                                                          
         LA    R1,1(RF)            Point to next character in string            
         J     VALGLOB1                                                         
                                                                                
VALGLOBA LARL  R3,SEOLST           Set all systems are global                   
VALGLOBB CLI   SEOLNUM,SEOLEOTQ    Test end of system list                      
         JE    VALPARE                                                          
         TM    SEOLFLAG,SEOLFOPN   Test system is open                          
         JNZ   *+8                 Yes - can't change status                    
         OI    SEOLFLAG,SEOLFGLB   Set global system flag                       
         AHI   R3,SEOLSTL          Bump to next system entry                    
         J     VALGLOBB                                                         
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Validate LOCAL=* (all systems) or list of systems                   *         
***********************************************************************         
                                                                                
VALLOCL  NTR1  LABEL=NO                                                         
         LARL  R3,SEOLST                                                        
         CLI   0(R3),0             Test anything in SEOLST                      
         JE    VALPARH             (must have processed SERVER= card)           
         TM    RUNINDS1,RUNIUPDT   Must be in updative mode                     
         JZ    VALPARH             (must have processed updative card)          
         CLI   0(R1),C'*'          Test flag all systems as global              
         JE    VALLOCLA                                                         
                                                                                
VALLOCL1 LR    RF,R1               Get system name in work                      
         MVC   WORK(L'SEOLNAM),SPACES                                           
         LA    RE,WORK                                                          
         LHI   R0,L'SEOLNAM+1                                                   
VALLOCL2 CLI   0(RF),SPACE         Test end of input string                     
         JE    VALLOCL3                                                         
         CLI   0(RF),COMMA         or list delimiter                            
         JE    VALLOCL3                                                         
         MVC   0(1,RE),0(RF)       Copy this character                          
         AHI   RF,1                Bump input                                   
         AHI   RE,1                Bump output                                  
         JCT   R0,VALLOCL2                                                      
         J     VALPARH                                                          
                                                                                
VALLOCL3 CLC   WORK(L'SEOLNAM),SPACES                                           
         JE    VALPARH                                                          
         LARL  R3,SEOLST           Locate system entry                          
         USING SEOLSTD,R3                                                       
VALLOCL4 CLI   SEOLNUM,SEOLEOTQ    Test end of system list                      
         JE    VALPARH                                                          
         CLC   SEOLNAM,WORK        Match system name to list                    
         JE    VALLOCL5                                                         
         AHI   R3,SEOLSTL          No match - bump to next entry                
         J     VALLOCL4                                                         
VALLOCL5 TM    SEOLFLAG,SEOLFOPN   System must be closed                        
         JNZ   VALPARH                                                          
         TM    SEOLFLAG,SEOLFGLB   and must be global                           
         JZ    VALPARH                                                          
         NI    SEOLFLAG,FF-SEOLFGLB   Set global system flag                    
         CLI   0(RF),SPACE         Set end of input string                      
         JE    VALPARE                                                          
         LA    R1,1(RF)            Point to next character in string            
         J     VALLOCL1                                                         
                                                                                
VALLOCLA LARL  R3,SEOLST           Set all systems are local                    
VALLOCLB CLI   SEOLNUM,SEOLEOTQ    Test end of system list                      
         JE    VALPARE                                                          
         TM    SEOLFLAG,SEOLFOPN   Test system is open                          
         JNZ   *+8                 Yes - can't change status                    
         NI    SEOLFLAG,FF-SEOLFGLB   Set system is not global                  
         AHI   R3,SEOLSTL          Bump to next system entry                    
         J     VALLOCLB                                                         
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Validate QM=MQ queue manager name                                   *         
***********************************************************************         
                                                                                
VALMQQM  OI    RUNINDS1,RUNIMQQM   Set queue manager name supplied              
         MVC   MQQMNAME,0(R1)      Set queue manager name                       
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate MAXIO=maximum value                                        *         
***********************************************************************         
                                                                                
VALMIOS  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,MAXIOS                                                        
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate BIGIO=maximum value                                        *         
***********************************************************************         
                                                                                
VALBIOS  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,BIGIOS                                                        
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate BIGSEG=maximum value                                       *         
***********************************************************************         
                                                                                
VALBSEG  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,BIGSEG                                                        
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate BIGCPU=maximum value                                       *         
***********************************************************************         
                                                                                
VALBCPU  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,BIGCPU                                                        
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate OFILHLQ=value                                              *         
***********************************************************************         
                                                                                
VALOHLQ  MVC   ROFILHLQ,0(R1)                                                   
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate OFILPRI=primary output file cylinders                      *         
***********************************************************************         
                                                                                
VALOPRI  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         STCM  R0,7,ROUTPRI        Set primary cylinders                        
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate OFILSEC=secondary output file cylinders                    *         
***********************************************************************         
                                                                                
VALOSEC  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         STCM  R0,7,ROUTSEC        Set secondary cylinders                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate xxxADJUST=trap CPU time adjustment factor                  *         
***********************************************************************         
                                                                                
VALACPU  LA    R2,CPUADJ           CPU time adjustment factor                   
         J     VALADJ                                                           
                                                                                
VALASRB  LA    R2,SRBADJ           SRB time adjustment factor                   
         J     VALADJ                                                           
                                                                                
VALATOT  LA    R2,TOTADJ           Total time adjustment factor                 
         J     VALADJ                                                           
                                                                                
VALADJ   NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,0(R2)                                                         
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate EDIIQ=input queue name                                     *         
***********************************************************************         
                                                                                
VALMQEI  OI    RUNINDMQ,RUNIMQEI                                                
         MVC   MQEDINAM,0(R1)      Set EDI input queue name                     
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate CPUOPTION=value                                            *         
***********************************************************************         
                                                                                
VALOCPU  MVC   CPUOPT,0(R1)                                                     
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate SRBOPTION=value                                            *         
***********************************************************************         
                                                                                
VALOSRB  MVC   SRBOPT,0(R1)                                                     
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate TOTOPTION=value                                            *         
***********************************************************************         
                                                                                
VALOTOT  MVC   TOTOPT,0(R1)                                                     
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate EDIOQ=output queue name                                    *         
***********************************************************************         
                                                                                
VALMQEO  OI    RUNINDMQ,RUNIMQEO                                                
         MVC   MQEDONAM,0(R1)      Set EDI output queue name                    
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate MQTRACE=B/I/O/other value to turn off                      *         
***********************************************************************         
                                                                                
VALMQTR  NI    RUNINDMQ,FF-(RUNIMQIT+RUNIMQOT)                                  
         CLI   0(R1),C'B'          B=both input and output                      
         JE    *+12                                                             
         CLI   0(R1),C'I'          I=input only                                 
         JNE   *+8                                                              
         OI    RUNINDMQ,RUNIMQIT                                                
         CLI   0(R1),C'B'                                                       
         JE    *+12                                                             
         CLI   0(R1),C'O'          O=output only                                
         JNE   *+8                                                              
         OI    RUNINDMQ,RUNIMQOT                                                
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate ABEND=notification list                                    *         
***********************************************************************         
                                                                                
VALABND  MVC   ABEND,0(R1)                                                      
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate SLOW=notification list                                     *         
***********************************************************************         
                                                                                
VALSLOW  MVC   SLOW,0(R1)                                                       
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate END                                                        *         
***********************************************************************         
                                                                                
VALEND   J     VALPARH                                                          
                                                                                
***********************************************************************         
* Validate FACPAK=ID (number, code or name)                           *         
***********************************************************************         
                                                                                
VALFPAK  NTR1  LABEL=NO                                                         
         TM    RUNINDS2,RUNIAGYC   Test AGENCY= control card input              
         JNZ   VALPARH             Yes - we have an error                       
         GOTOR VNUM                                                             
         JNE   *+12                                                             
         CHI   R0,255                                                           
         JH    VALPARH                                                          
         LARL  RF,FACIDTAB                                                      
VALFPAK2 CLI   0(RF),FF            Test end of table                            
         JE    VALPARH                                                          
         LTR   R0,R0               Test numeric value                           
         JNZ   VALFPAK4                                                         
         CLI   1(R1),SPACE         Test one character input                     
         JNE   VALFPAK3                                                         
         CLC   7(1,RF),0(R1)       Match on id                                  
         JE    VALFPAK6                                                         
         J     VALFPAK5                                                         
VALFPAK3 CLC   0(4,RF),0(R1)       Match on full name                           
         JE    VALFPAK6                                                         
         J     VALFPAK5                                                         
VALFPAK4 CLM   R0,1,4(RF)          Match on number                              
         JE    VALFPAK6                                                         
VALFPAK5 AHI   RF,L'FACIDTAB                                                    
         JCT   RE,VALFPAK2                                                      
         J     VALPARH                                                          
VALFPAK6 MVC   FACPAKNM,0(RF)      Set FACPAK name                              
         MVC   FACPAKID,7(RF)      Set FACPAK id                                
         OI    RUNINDS2,RUNIFPKC                                                
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate AGENCY=agency alpha id (AGENCY=aabbccddeeff)               *         
***********************************************************************         
                                                                                
VALAGID  TM    RUNINDS2,RUNIFPKC   Test FACPAK= control card input              
         BNZR  RE                  Yes - we have an error                       
         MVC   AGYFILT,0(R1)       Set agency filters                           
         OI    RUNINDS2,RUNIAGYC                                                
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate SEGSIZE=value                                              *         
***********************************************************************         
                                                                                
VALSEGC  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,RSEGSIZE         Set maximum file segement size               
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate SEGRECS=value                                              *         
***********************************************************************         
                                                                                
VALSEGR  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,RSEGRECS         Set maximum file segement records            
         J     VALPARE                                                          
*&&UK                                                                           
***********************************************************************         
* Validate MEDDSPC=ID                                                 *         
***********************************************************************         
                                                                                
VALMEDS  NTR1  LABEL=NO                                                         
         MVC   MCMEDDSP,0(R1)                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),GETA                                                     
         MVC   WORK+4(L'MCMEDDSP),MCMEDDSP                                      
         MVC   WORK+8(7),VPCARD                                                 
         MVI   WORK+15,SPACE                                                    
         LA    R1,WORK                                                          
         LHI   R0,-21                                                           
         SVC   247                                                              
         LTR   RF,RF                                                            
         JNZ   VALMEDE1                                                         
                                                                                
         OC    WORK+24(4),WORK+24  Test ALET found                              
         JZ    VALMEDE1                                                         
                                                                                
         LARL  RF,SSB                                                           
         MVC   SSOMEDTB-SSOOFF(,RF),MCWORK+20  Set origin/ALET                  
         J     VALPARE                                                          
                                                                                
VALMEDE1 MVC   PMESSAGE(L'SPCLIT1),SPCLIT1                                      
         GOTOR PRTLOG                                                           
         J     VALPARH                                                          
*&&                                                                             
***********************************************************************         
* Validate DATE=value (batch processing)                              *         
***********************************************************************         
                                                                                
VALDATE  MVC   MCDATE,0(R1)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate LOGO=value (batch processing)                              *         
***********************************************************************         
                                                                                
VALLOGO  CLC   22(5,R1),SPACES                                                  
         JE    VALLOGO2                                                         
         PACK  DUB,22(5,R1)                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,MCDESTID                                                    
                                                                                
VALLOGO2 CLC   ORIGLIT,28(R1)                                                   
         JNE   VALLOGO4                                                         
         PACK  DUB,35(5,R1)                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,MCORIGID                                                    
                                                                                
VALLOGO4 J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Validate AC0PPAA (batch processing)                                 *         
***********************************************************************         
                                                                                
VALSACC  MVI   MCOVSYS,6           Set Acc system                               
         J     VALSYST                                                          
                                                                                
VALSYST  SHI   R1,L'MCUSER+1                                                    
         MVC   MCUSER,0(R1)                                                     
         SHI   R1,L'MCPROG                                                      
         MVC   MCPROG,0(R1)                                                     
         J     VALEXIT                                                          
                                                                                
***********************************************************************         
* Program termination                                                 *         
***********************************************************************         
                                                                                
VALENDR  ICM   R3,15,AQUE          Test work in process                         
         JZ    VALENDR2                                                         
         LAM   AR3,AR3,ALET                                                     
         SAC   512                                                              
         USING TABSQUED,R3                                                      
         GOTOR UPDQUE,NULLDONE     Set work completed                           
         MVI   TQERROR,TQENOSVR    Set no server error                          
         XC    TQASVR,TQASVR       Clear server lockword                        
                                                                                
VALENDR2 SAC   0                                                                
         LM    R0,RE,MCRUNLST                                                   
         J     ENDRUN                                                           
                                                                                
***********************************************************************         
* Validate PRINT=S                                                    *         
***********************************************************************         
                                                                                
VALPRNT  NTR1  LABEL=NO                                                         
         CLI   0(R1),C'S'          Test want system table printed               
         JNE   VALPARH                                                          
         LARL  RE,SEOLST                                                        
         CLI   0(RE),0             Test system table initialized                
         JE    VALPARH                                                          
         GOTOR PRTSYS              Yes - print the table contents               
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate NOOP=system                                                *         
***********************************************************************         
                                                                                
VALSYNO  NTR1  LABEL=NO                                                         
         LARL  R3,SEOLST           Look up system name in system list           
         USING SEOLSTD,R3                                                       
         LHI   R0,SEOLSTM                                                       
VALSYNO1 CLI   SEOLNUM,SEOLEOTQ    Test end of list                             
         JE    VALPARH                                                          
         CLC   SEOLNAM,0(R1)       Match name to list                           
         JE    VALSYNO2                                                         
         AHI   R3,SEOLSTL                                                       
         JCT   R0,VALSYNO1                                                      
         J     VALPARH                                                          
                                                                                
VALSYNO2 OI    SEOLFLAG,SEOLFNOP   Set this system is NOOP                      
         J     VALPARE                                                          
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Validate OP=system                                                  *         
***********************************************************************         
                                                                                
VALSYOP  NTR1  LABEL=NO                                                         
         LARL  R3,SEOLST           Look up system name in system list           
         USING SEOLSTD,R3                                                       
         LHI   R0,SEOLSTM                                                       
VALSYOP1 CLI   SEOLNUM,SEOLEOTQ    Test end of list                             
         JE    VALPARH                                                          
         CLC   SEOLNAM,0(R1)       Match name to list                           
         JE    VALSYOP2                                                         
         AHI   R3,SEOLSTL                                                       
         JCT   R0,VALSYOP1                                                      
         J     VALPARH                                                          
                                                                                
VALSYOP2 NI    SEOLFLAG,FF-SEOLFNOP                                             
         J     VALPARE                                                          
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Validate CLOSE=system                                               *         
***********************************************************************         
                                                                                
VALCLSE  NTR1  LABEL=NO                                                         
         LARL  R3,SEOLST           Look up system name in system list           
         USING SEOLSTD,R3                                                       
         LHI   R0,SEOLSTM                                                       
VALCLSE1 CLI   SEOLNUM,SEOLEOTQ    Test end of list                             
         JE    VALPARH                                                          
         CLC   SEOLNAM,0(R1)       Match name to list                           
         JE    VALCLSE2                                                         
         AHI   R3,SEOLSTL                                                       
         JCT   R0,VALCLSE1                                                      
         J     VALPARH                                                          
                                                                                
VALCLSE2 TM    SEOLFLAG,SEOLFOPN   Test this system is open                     
         JZ    VALPARH                                                          
         MVI   OPNACT,OPNACLO      Set to close system                          
         GOTOR ROPNCLO,SEOLSTD                                                  
         GOTOR PRTSYS              Print system table                           
         J     VALPARE                                                          
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Validate OPEN=system                                                *         
***********************************************************************         
                                                                                
VALOPEN  NTR1  LABEL=NO                                                         
         LARL  R3,SEOLST           Look up system name in system list           
         USING SEOLSTD,R3                                                       
         LHI   R0,SEOLSTM                                                       
VALOPEN1 CLI   SEOLNUM,SEOLEOTQ    Test end of list                             
         JE    VALPARH                                                          
         CLC   SEOLNAM,0(R1)       Match name to list                           
         JE    VALOPEN2                                                         
         AHI   R3,SEOLSTL                                                       
         JCT   R0,VALOPEN1                                                      
         J     VALPARH                                                          
                                                                                
VALOPEN2 TM    SEOLFLAG,SEOLFCLS   Test this system is closed                   
         JZ    VALPARH                                                          
         MVI   OPNACT,OPNAOPN      Set action to open files                     
         GOTOR ROPNCLO,SEOLSTD     Open system files                            
         XC    NOTLSTN,NOTLSTN     Clear the not list to process jobs           
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Validate MQIBUFFL=value                                             *         
***********************************************************************         
                                                                                
VALMQIL  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,MQIBUFFL                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate MQOBUFFL=value                                             *         
***********************************************************************         
                                                                                
VALMQOL  NTR1  LABEL=NO                                                         
         GOTOR VNUM                                                             
         JNE   VALPARH                                                          
         ST    R0,MQOBUFFL                                                      
         J     VALPARE                                                          
                                                                                
***********************************************************************         
* Validate MQACK-02=Y/N                                               *         
***********************************************************************         
                                                                                
VALMQA   NI    RUNINDMQ,FF-(RUNIACK2)                                           
         CLI   0(R1),NOQ                                                        
         BER   RE                                                               
         OI    RUNINDMQ,RUNIACK2                                                
         CLI   0(R1),YESQ                                                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate MQRUNQ=Y/N                                                 *         
***********************************************************************         
                                                                                
VALMQRQ  NI    RUNINDMQ,FF-(RUNIMQNR)                                           
         CLI   0(R1),YESQ                                                       
         BER   RE                                                               
         OI    RUNINDMQ,RUNIMQNR                                                
         CLI   0(R1),NOQ                                                        
         BR    RE                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
* Validate SERVER(1)=phasename(/x)(+ddd=xxxx)                         *         
***********************************************************************         
                                                                                
VALSVR1  NTR1  LABEL=NO                                                         
         OC    DSPACE,DSPACE       Test data space known                        
         JZ    VALPARH                                                          
         TM    RUNISVRS,RUNISVR1   Test no previous SERVER1= card               
         JNZ   EXITN                                                            
         OI    RUNISVRS,RUNISVR1   Set SERVER1=card processed                   
         GOTOR SETSVR              Load server and DDLINK                       
         MVI   NSERVERS,1                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER2=phasename(/x)(+ddd=xxxx)                           *         
***********************************************************************         
                                                                                
VALSVR2  NTR1  LABEL=NO                                                         
         TM    RUNISVRS,RUNISVR1   Test SERVER1= card processed                 
         JZ    EXITN                                                            
         TM    RUNISVRS,RUNISVR2   Test no previous SERVER2= card               
         JNZ   EXITN                                                            
         OI    RUNISVRS,RUNISVR2   Set SERVER2= card processed                  
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVR2-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,2                                                       
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate SERVER3=phasename(/x)(+ddd=xxxx)                           *         
***********************************************************************         
                                                                                
VALSVR3  NTR1  LABEL=NO                                                         
         TM    RUNISVRS,RUNISVR2   Test SERVER2= card processed                 
         JZ    EXITN                                                            
         TM    RUNISVRS,RUNISVR3   Test no previous SERVER3= card               
         JNZ   EXITN                                                            
         OI    RUNISVRS,RUNISVR3   Set SERVER3= card processed                  
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVR3-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,3                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER4=phasename(/x)(+ddd=xxxx)                           *         
***********************************************************************         
                                                                                
VALSVR4  NTR1  LABEL=NO                                                         
         TM    RUNISVRS,RUNISVR3   Test SERVER3= card processed                 
         JZ    EXITN                                                            
         TM    RUNISVRS,RUNISVR4   Test no previous SERVER4= card               
         JNZ   EXITN                                                            
         OI    RUNISVRS,RUNISVR4   Set SERVER4= card processed                  
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVR4-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,4                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER5=phasename(/x)(+ddd=xxxx)                           *         
***********************************************************************         
                                                                                
VALSVR5  NTR1  LABEL=NO                                                         
         TM    RUNISVRS,RUNISVR4   Test SERVER4= card processed                 
         JZ    EXITN                                                            
         TM    RUNISVRS,RUNISVR5   Test no previous SERVER5= card               
         JNZ   EXITN                                                            
         OI    RUNISVRS,RUNISVR5   Set SERVER5= card processed                  
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVR5-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,5                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER6=phasename(/x)(+ddd=xxxx)                           *         
***********************************************************************         
                                                                                
VALSVR6  NTR1  LABEL=NO                                                         
         TM    RUNISVRS,RUNISVR5   Test SERVER5= card processed                 
         JZ    EXITN                                                            
         TM    RUNISVRS,RUNISVR6   Test no previous SERVER6= card               
         JNZ   EXITN                                                            
         OI    RUNISVRS,RUNISVR6   Set SERVER6= card processed                  
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVR6-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,6                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER7=phasename(/x)(+ddd=xxxx)                           *         
***********************************************************************         
                                                                                
VALSVR7  NTR1  LABEL=NO                                                         
         TM    RUNISVRS,RUNISVR6   Test SERVER6= card processed                 
         JZ    EXITN                                                            
         TM    RUNISVRS,RUNISVR7   Test no previous SERVER7= card               
         JNZ   EXITN                                                            
         OI    RUNISVRS,RUNISVR7   Set SERVER7= card processed                  
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVR7-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,7                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER8=phasename(/x)(+ddd=xxxx)                           *         
***********************************************************************         
                                                                                
VALSVR8  NTR1  LABEL=NO                                                         
         TM    RUNISVRS,RUNISVR7   Test SERVER7= card processed                 
         JZ    EXITN                                                            
         TM    RUNISVRS,RUNISVR8   Test no previous SERVER8= card               
         JNZ   EXITN                                                            
         OI    RUNISVRS,RUNISVR8   Set SERVER8= card processed                  
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVR8-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,8                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER9=phasename(/x)(+ddd=xxxx)                           *         
***********************************************************************         
                                                                                
VALSVR9  NTR1  LABEL=NO                                                         
         TM    RUNISVRS,RUNISVR8   Test SERVER8= card processed                 
         JZ    EXITN                                                            
         TM    RUNISVRT,RUNISVR9   Test no previous SERVER9= card               
         JNZ   EXITN                                                            
         OI    RUNISVRT,RUNISVR9   Set SERVER9= card processed                  
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVR9-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,9                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER10=phasename(/x)(+ddd=xxxx)                          *         
***********************************************************************         
                                                                                
VALSVRA  NTR1  LABEL=NO                                                         
         TM    RUNISVRT,RUNISVR9   Test SERVER9= card processed                 
         JZ    EXITN                                                            
         TM    RUNISVRT,RUNISVRA   Test no previous SERVER10= card              
         JNZ   EXITN                                                            
         OI    RUNISVRT,RUNISVRA   Set SERVER10= card processed                 
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVRA-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,10                                                      
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER11=phasename(/x)(+ddd=xxxx)                          *         
***********************************************************************         
                                                                                
VALSVRB  NTR1  LABEL=NO                                                         
         TM    RUNISVRT,RUNISVRA   Test SERVER10= card processed                
         JZ    EXITN                                                            
         TM    RUNISVRT,RUNISVRB   Test no previous SERVER11= card              
         JNZ   EXITN                                                            
         OI    RUNISVRT,RUNISVRB   Set SERVER11= card processed                 
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVRB-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,11                                                      
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER12=phasename(/x)(+ddd=xxxx)                          *         
***********************************************************************         
                                                                                
VALSVRC  NTR1  LABEL=NO                                                         
         TM    RUNISVRT,RUNISVRB   Test SERVER11= card processed                
         JZ    EXITN                                                            
         TM    RUNISVRT,RUNISVRC   Test no previous SERVER12= card              
         JNZ   EXITN                                                            
         OI    RUNISVRT,RUNISVRC   Set SERVER12= card processed                 
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVRC-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,12                                                      
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER13=phasename(/x)(+ddd=xxxx)                          *         
***********************************************************************         
                                                                                
VALSVRD  NTR1  LABEL=NO                                                         
         TM    RUNISVRT,RUNISVRC   Test SERVER12= card processed                
         JZ    EXITN                                                            
         TM    RUNISVRT,RUNISVRD   Test no previous SERVER13= card              
         JNZ   EXITN                                                            
         OI    RUNISVRT,RUNISVRD   Set SERVER13= card processed                 
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVRD-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,13                                                      
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER14=phasename(/x)(+ddd=xxxx)                          *         
***********************************************************************         
                                                                                
VALSVRE  NTR1  LABEL=NO                                                         
         TM    RUNISVRT,RUNISVRD   Test SERVER13= card processed                
         JZ    EXITN                                                            
         TM    RUNISVRT,RUNISVRE   Test no previous SERVER14= card              
         JNZ   EXITN                                                            
         OI    RUNISVRT,RUNISVRE   Set SERVER14= card processed                 
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVRE-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,14                                                      
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER15=phasename(/x)(+ddd=xxxx)                          *         
***********************************************************************         
                                                                                
VALSVRF  NTR1  LABEL=NO                                                         
         TM    RUNISVRT,RUNISVRE   Test SERVER14= card processed                
         JZ    EXITN                                                            
         TM    RUNISVRT,RUNISVRF   Test no previous SERVER15= card              
         JNZ   EXITN                                                            
         OI    RUNISVRT,RUNISVRF   Set SERVER15= card processed                 
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVRF-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,15                                                      
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate SERVER16=phasename(/x)(+ddd=xxxx)                          *         
***********************************************************************         
                                                                                
VALSVRG  NTR1  LABEL=NO                                                         
         TM    RUNISVRT,RUNISVRF   Test SERVER15= card processed                
         JZ    EXITN                                                            
         TM    RUNISVRT,RUNISVRG   Test no previous SERVER16= card              
         JNZ   EXITN                                                            
         OI    RUNISVRT,RUNISVRG   Set SERVER16= card processed                 
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
         L     RF,LP_ALPXD                                                      
         MVC   LP_ASVRG-LP_XD(,RF),0(R1)                                        
         MVI   NSERVERS,16                                                      
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Exit via RE with CC equal                                           *         
***********************************************************************         
                                                                                
VALEXIT  CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
VPWORKD  DSECT ,                   ** VALPAR local W/S **                       
VPPARM   DS    A                                                                
VPPARA   DS    6F                                                               
VPPHASE  DS    CL11                                                             
VPCARD   DS    CL80                                                             
VPJOB    DS    CL(L'TSJOB)                                                      
VPTYPE   DS    XL(L'TSTYPE)                                                     
VPERROR  DS    XL(L'TQERROR)                                                    
VPWORKL  EQU   *-VPWORKD                                                        
RUNNER   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Validate numbers                                                    *         
*                                                                     *         
* Ntry:- R1=A(Start of numeric string)                                *         
***********************************************************************         
                                                                                
VNUM     ST    RE,12(RD)                                                        
         LR    RF,R1                                                            
         LHI   R0,20                                                            
VNUM0010 CLI   0(RF),SPACE                                                      
         JE    VNUM0020                                                         
         CLI   0(RF),COMMA                                                      
         JE    VNUM0020                                                         
         CLI   0(RF),C'0'                                                       
         JL    VNUMN                                                            
         CLI   0(RF),C'9'                                                       
         JH    VNUMN                                                            
         AHI   RF,1                                                             
         JCT   R0,VNUM0010                                                      
         J     VNUMN                                                            
                                                                                
VNUM0020 SR    RF,R1                                                            
         JNP   VNUMN                                                            
         BCTR  RF,0                                                             
         EX    RF,VNUMPKQ1                                                      
         CVBG  GR0,QUAD                                                         
         LA    R1,1(RF,R1)         Point to end of string                       
         CLI   0(R1),COMMA         Test for scale suffixes                      
         JNE   VNUM0040                                                         
                                                                                
         CLI   1(R1),KILOSIZE      Test 'K'                                     
         JNE   VNUM0030                                                         
         SLLG  GR0,GR0,10          Shift left 10 - *1024                        
         J     VNUM0050                                                         
                                                                                
VNUM0030 CLI   1(R1),MEGASIZE      Test 'M'                                     
         JNE   VNUM0040                                                         
         SLLG  GR0,GR0,20          Shift left 20 - *1048576                     
         J     VNUM0050                                                         
                                                                                
VNUM0040 CLI   NUMTYPE,C'T'        Test want timer units                        
         JNE   VNUMY                                                            
         MVI   NUMTYPE,0           Reset NUMTYPE when used                      
         CLI   1(R1),C'T'          Test timer units given                       
         JE    VNUMY                                                            
         MSGF  GR0,TUSSEC          No - multiply by 38400                       
                                                                                
VNUM0050 AHI   R1,2                Adjust pointer for ',x'                      
                                                                                
VNUMY    SR    RE,RE               Set for good exit                            
         J     VNUMX                                                            
                                                                                
VNUMN    SGR   GR0,GR0             Set for bad exit                             
         LHI   RE,1                                                             
                                                                                
VNUMX    LTR   RE,RE               Set condition code for caller                
         L     RE,12(RD)                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* MQ interface (called from RECPUT routine in DDLINK or RUNNER        *         
* itself)                                                             *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows:-                   *         
*                                                                     *         
*        P1 B0    - Calling mode (see MQxxxQ equates below)           *         
*        P1 B1-3  - A(FALINK string)                                  *         
*        P2 B0-3  - Length of FALINK string                           *         
***********************************************************************         
                                                                                
MQSENDQ  EQU   0                   Put MQ message                               
MQPUTQ   EQU   1                   Put FALINK string                            
MQENDQ   EQU   2                   Close and send                               
MQERRQ   EQU   3                   Put error                                    
MQACK02Q EQU   4                   Send acknowledgement (ACK-02)                
                                                                                
MQI      NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         LR    R2,R1                                                            
         MVC   MQCALL,0(R2)        Set calling mode                             
         SR    RE,RE                                                            
         ICM   RE,7,1(R2)                                                       
         ST    RE,MQDATAA          Set A(input string)                          
         MVC   MQDATAL,6(R2)       Set L'input string                           
                                                                                
         L     R3,LP_ALPXD                                                      
         USING LP_XD,R3            R3=A(LP_D extension area)                    
                                                                                
         CLI   MQCALL,MQSENDQ      Test sending MQ message                      
         JE    MQI0050                                                          
         CLI   MQCALL,MQACK02Q     Test sending acknowledgement                 
         JE    MQI0010                                                          
         CLI   MQCALL,MQERRQ       Test sending error                           
         JE    MQI0020                                                          
                                                                                
         CLI   MQFLAG,MQFFRST      Test first time for new buffer               
         JE    MQI0020                                                          
         CLI   MQCALL,MQENDQ       Test DDLINK end call                         
         JE    MQI0012                                                          
         LLH   R2,MQDATAL          Get length of string                         
         AL    R2,MQOBUFFP         Point to next available space                
         AHI   R2,1                Add one for end of buffer                    
         C     R2,MQOBUFFX         Test new data string will fit                
         JL    MQI0030                                                          
         DC    H'0'                Buffer is full                               
                                                                                
MQI0010  TM    RUNINDMQ,RUNIACK2   Test sending acknowledgements                
         JZ    EXITY               No - exit                                    
         J     MQI0020             Send acknowledgment message                  
                                                                                
MQI0012  GOTOR RPUTMQD             Send MQ message                              
         MVI   MQFLAG,MQFFRST      Set first for new buffer                     
         J     EXITY                                                            
                                                                                
MQI0020  CLI   MQCALL,MQENDQ       Test DDLINK end call                         
         JE    EXITY               Yes - exit - no message to be sent           
                                                                                
         SAM31 ,                   Initialize new MQ buffer                     
         L     R0,MQOBUFF                                                       
         ST    R0,MQOBUFFP         Point to start of buffer                     
         L     R1,MQOBUFFL                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE               Clear buffer                                 
                                                                                
***********************************************************************         
* Build MQ message header                                             *         
***********************************************************************         
                                                                                
         L     R2,MQOBUFF          Point to start of buffer                     
         USING LM_D,R2             Build header                                 
         MVC   LM_AGY,LP_AGY       Set agency alpha id                          
         LHI   RF,L'LM_AGY                                                      
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  LM_LHDR,DUB         Set header length in EBCDIC                  
         AHI   RF,LM_AGY-LM_D                                                   
         AL    RF,MQOBUFF                                                       
         ST    RF,MQOBUFFP         Set buffer pointer                           
                                                                                
         CLI   MQCALL,MQERRQ       Test sending error                           
         JE    MQI0022                                                          
         CLI   MQCALL,MQACK02Q     or acknowledgement                           
         JE    MQI0022                                                          
                                                                                
         MVI   MQFLAG,MQFNEXT      Set next time for this buffer                
         OC    LM_LAB,LP_MQSVR     Set/test service call routing code           
         JZ    MQI0022                                                          
                                                                                
         OC    LM_RLAB,LP_MQRKV    Set/test routing label                       
         JNZ   MQI0030                                                          
                                                                                
***********************************************************************         
* Routing has not been provided - set to route message back to a      *         
* RUNNER - assume same RUNNER class if none has been provided         *         
***********************************************************************         
                                                                                
         MVI   LM_RLAB,ASTERISK    Pre-fill with asterisks                      
         MVC   LM_RLAB+1(L'LM_RLAB-1),LM_RLAB                                   
         MVC   LM_RLAB(L'RUNCLASS),RUNCLASS                                     
         MVC   LM_RLAB+L'RUNCLASS(L'LP_MQMST),LP_MQMST                          
         CLI   LP_MQMST,0          Echo back if no other server                 
         JNE   *+10                                                             
         MVC   LM_RLAB+L'RUNCLASS(L'LP_SVRTY),LP_SVRTY                          
         J     MQI0030                                                          
                                                                                
MQI0022  TM    LP_FLAG2,LP_FFALT   Test FALINK test string input                
         JNZ   MQI0030                                                          
         OC    LM_LAB,LP_MQQUE     Set/test output routing                      
         JNZ   MQI0024                                                          
         CLI   MQCALL,MQERRQ       Test sending error                           
         JE    EXITY               Yes, may not have routing yet                
         DC    H'0'                Where are we sending this then?              
MQI0024  SR    RF,RF                                                            
         OC    LP_MQKEY,LP_MQKEY   Test user key provided                       
         JZ    *+14                                                             
         MVC   LM_UKEY,LP_MQKEY    Move user key to buffer                      
         LHI   RF,L'LP_MQKEY       and account for it                           
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  LM_LHDR,DUB         Set header length and account                
         AHI   RF,LM_UKEY-LM_D     for other header data                        
         AL    RF,MQOBUFF                                                       
         ST    RF,MQOBUFFP         Set buffer pointer to start of data          
         DROP  R2                                                               
                                                                                
MQI0030  SAM31 ,                   Add data string to current buffer            
         L     R0,MQOBUFFP         Point to next buffer space                   
         LLH   R1,MQDATAL          Get length of data string                    
         L     RE,MQDATAA          Get address of data string                   
         LR    RF,R1                                                            
         MVCL  R0,RE               Move data string to buffer                   
         ST    R0,MQOBUFFP         Set address of next buffer space             
         SAM24 ,                                                                
         J     MQI0060                                                          
                                                                                
MQI0050  SAM31 ,                   Move string to top of buffer                 
         L     R0,MQOBUFF          Point to start of buffer                     
         LLH   R1,MQDATAL          Get length of data string                    
         L     RE,MQDATAA          Get address of data string                   
         LR    RF,R1                                                            
         MVCL  R0,RE               Move data string to buffer                   
         ST    R0,MQOBUFFP         Set address of next buffer space             
         SAM24 ,                                                                
                                                                                
MQI0060  L     RE,ARUNFACS                                                      
RUNF     USING RUNFACSD,RE                                                      
         L     R0,RUNF.RMQRADD     Bump total MQ records output                 
         AHI   R0,1                                                             
         ST    R0,RUNF.RMQRADD                                                  
         DROP  RUNF                                                             
                                                                                
         CLI   MQCALL,MQSENDQ      Test sending MQ message                      
         JE    MQI0070                                                          
         CLI   MQCALL,MQERRQ       Test sending error                           
         JE    MQI0070                                                          
         CLI   MQCALL,MQACK02Q     or acknowledgement                           
         JNE   EXITY                                                            
                                                                                
MQI0070  GOTOR RPUTMQD             Send MQ message                              
         MVI   MQFLAG,MQFFRST      Set first for new buffer                     
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Put current buffer to MQ queue                                      *         
***********************************************************************         
                                                                                
PUTMQD   NTR1  LABEL=*                                                          
                                                                                
         LARL  R4,MQGLOB           Point to MQ globals                          
         USING MQGLOB,R4                                                        
         L     R3,LP_ALPXD                                                      
         USING LP_XD,R3            R3=A(LP_D extension area)                    
                                                                                
         L     R1,MQOBUFFP                                                      
         S     R1,MQOBUFF                                                       
         ST    R1,MQLOMSG          Set length of MQ data                        
                                                                                
         GOTOR MQTRC,1             Trace MQ output                              
                                                                                
         TM    LP_FLAG2,LP_FFALT   No I/O if test FALINK string input           
         JNZ   PUTMQD02                                                         
                                                                                
         GOTOR RCALLMQ,MQPUTEDI    Put current output buffer                    
         JH    PUTMQD02            High means can't send                        
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR RCALLMQ,MQCMT       Commit the put                               
         MVC   PMESSAGE(L'MQSNDLIT),MQSNDLIT                                    
         GOTOR PRTLOG                                                           
                                                                                
PUTMQD02 L     RE,ARUNFACS                                                      
RUNF     USING RUNFACSD,RE         Update RUNNER accumulators                   
         L     R0,RUNF.RMQBADD     MQ adds                                      
         AHI   R0,1                                                             
         ST    R0,RUNF.RMQBADD                                                  
         L     R0,MQLOMSG          Total size                                   
         A     R0,RUNF.RMQBSIZ                                                  
         ST    R0,RUNF.RMQBSIZ                                                  
         J     EXIT                                                             
         DROP  RUNF                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Call MQ - R1=A(MQ parameter list)                                   *         
***********************************************************************         
                                                                                
CALLMQ   NTR1  LABEL=*                                                          
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         LR    R2,R1               Point to MQ control block                    
X        USING MQ_D,R2                                                          
                                                                                
         LARL  R4,MQGLOB           Point to MQ globals                          
         USING MQGLOB,R4                                                        
                                                                                
         SR    RE,RE               Set input buffer address                     
         ICM   RE,1,X.MQ_IBUFF                                                  
         JZ    *+14                                                             
         LA    RE,X.MQ_D(RE)       RE=A(input buffer address)                   
         MVC   0(L'MQIBUFF,RE),MQIBUFF                                          
                                                                                
         SR    RE,RE               Set output buffer address                    
         ICM   RE,1,X.MQ_OBUFF                                                  
         JZ    CALLMQ02                                                         
         LA    RE,X.MQ_D(RE)       RE=A(output buffer address)                  
         MVC   0(L'MQOBUFF,RE),MQOBUFF                                          
                                                                                
CALLMQ02 CLI   X.MQ_TYPE,MQ_TPUTM  Test issuing a put                           
         JNE   CALLMQ04                                                         
         TM    LP_FLAG2,LP_FDAUQ   Can't put download as upload                 
         JNZ   EXITH                                                            
                                                                                
CALLMQ04 SAM31 ,                                                                
         L     RF,X.MQ_AROUT       RF=A(MQ routine handle)                      
         L     RF,0(RF)            RF=A(MQ routine)                             
         LA    R1,X.MQ_PARMS       R1=A(MQ routine parameters)                  
         CALL  (15),MF=(E,(1))                                                  
         SAM24 ,                                                                
         LAM   AR0,ARF,ARZERO      Clear access registers                       
                                                                                
         CLC   MQCOMPC,MQCCOK      Test good completion code                    
         JNE   CALLMQ06                                                         
         CLI   X.MQ_TYPE,MQ_TGETM  Test issuing a get                           
         JNE   EXITY                                                            
         MVC   RMQLDATA,MQLIMSG    Set length of incoming message               
         J     EXITY                                                            
                                                                                
CALLMQ06 CLI   X.MQ_TYPE,MQ_TGETM  Test issuing a get                           
         JNE   CALLMQ10                                                         
         LHI   RF,MQGMO_SET_SIGNAL                                              
         N     RF,MQEDIOPT_OPTIONS Test setting signal                          
         JZ    CALLMQ10                                                         
         CLC   MQCOMPC,MQCCWARN    Yes, can get following warning               
         JNE   CALLMQ08                                                         
         CLC   MQREASN,=A(MQRC_SIGNAL_REQUEST_ACCEPTED)                         
         JE    EXITY                                                            
         J     CALLMQ10                                                         
CALLMQ08 CLC   MQCOMPC,MQCCFAIL    or following error                           
         JNE   CALLMQ10                                                         
         CLC   MQREASN,=A(MQRC_SIGNAL_OUTSTANDING)                              
         JE    EXITY                                                            
CALLMQ10 MVC   PMESSAGE(L'MQERLIT1),MQERLIT1                                    
         MVC   PMESSAGE+L'MQERLIT1(L'MQERLIT2),MQERLIT2                         
         CLC   MQCOMPC,MQCCWARN                                                 
         JE    CALLMQ12                                                         
         MVC   PMESSAGE+L'MQERLIT1(L'MQERLIT3),MQERLIT3                         
         CLC   MQCOMPC,MQCCFAIL                                                 
         JE    CALLMQ12                                                         
         MVC   PMESSAGE+L'MQERLIT1(L'MQERLIT4),MQERLIT4                         
         ICM   R0,15,MQCOMPC                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PMESSAGE+L'MQERLIT1+L'MQERLIT4(5),DUB                            
CALLMQ12 GOTOR PRTLOG              Print condition code log entry               
         MVC   PMESSAGE(L'MQERLIT5),MQERLIT5                                    
         ICM   R0,15,MQREASN                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PMESSAGE+L'MQERLIT5(5),DUB                                       
                                                                                
         LARL  RF,MQ_REASON_CODE_TABLE                                          
CALLMQ14 CLI   0(RF),X'FF'                                                      
         JE    CALLMQ16                                                         
         CLC   MQREASN,0(RF)                                                    
         JE    *+12                                                             
         AHI   RF,28                                                            
         J     CALLMQ14                                                         
         MVC   PMESSAGE+L'MQERLIT5+6(24),4(RF)                                  
                                                                                
CALLMQ16 GOTOR PRTLOG              Print reason code log entry                  
                                                                                
CALLMQ17 CLC   MQREASN,=A(MQRC_Q_MGR_NOT_AVAILABLE)                             
         JE    EXITN                                                            
                                                                                
CALLMQ18 J     EXITN               DIE ON MQ ERROR                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Print a log event line                                              *         
***********************************************************************         
                                                                                
PRTLOG   NTR1  LABEL=*                                                          
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         TIME  DEC                 Edit time into print line                    
         ST    R0,DUB                                                           
         MVI   DUB+4,DIGIT                                                      
         UNPK  WORK(9),DUB(5)                                                   
         MVC   PHH,WORK                                                         
         MVI   PHHDEL,PHHDELQ                                                   
         MVC   PMM,WORK+2                                                       
         MVI   PMMDEL,PMMDELQ                                                   
         MVC   PSS,WORK+4                                                       
         MVI   PSSDEL,PSSDELQ                                                   
         MVC   PDS,WORK+6                                                       
         MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
         J     EXIT                                                             
         DROP  RC                                                               
         EJECT                                                                  
***********************************************************************         
* Put a server trace line to the output file                          *         
***********************************************************************         
                                                                                
TRCLOG   NTR1  LABEL=*                                                          
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         LR    R2,R1               Save calling parameter list                  
                                                                                
         CLI   MCTRACE,C'L'        Test TRACE=Log                               
         JE    *+12                                                             
         CLI   MCTRACE,C'B'        or TRACE=Both                                
         JNE   TRCLOG02                                                         
         L     R1,0(R2)            Don't log space lines                        
         CLC   1(L'P,R1),SPACES                                                 
         JE    TRCLOG02                                                         
         L     R5,RWRKBLK                                                       
         USING WRKIOD,R5                                                        
         TM    WRKISTAT,WRKISOPN+WRKISUPD                                       
         JNO   TRCLOG02                                                         
         L     R3,AIO                                                           
         ICM   R0,15,WRKIAREC                                                   
         STCM  R3,15,WRKIAREC                                                   
         LA    R4,4(R3)                                                         
         USING LQ_D,R4                                                          
         MVI   LQ_EL,LQ_RLOGQ      Build a log element                          
         MVC   LQ_CMND(L'P),1(R1)                                               
         LA    R1,LQ_CMND+L'P-1                                                 
         CLI   0(R1),SPACE                                                      
         JNE   *+8                                                              
         JCT   R1,*-8                                                           
         MVI   1(R1),0                                                          
         AHI   R1,1                                                             
         SR    R1,R4                                                            
         STCM  R1,3,LQ_LN                                                       
         AHI   R1,5                                                             
         SLL   R1,16                                                            
         STCM  R1,15,0(R3)                                                      
         MVI   WRKIACTN,WRKIAADD                                                
         GOTOR RWRKIO,WRKIOD                                                    
         JE    *+6                                                              
         DC    H'0'                                                             
         STCM  R0,15,WRKIAREC                                                   
         ICM   R0,15,RLOGADD       Bump N'log records added                     
         AHI   R0,1                                                             
         STCM  R0,15,RLOGADD                                                    
                                                                                
TRCLOG02 CLI   MCTRACE,C'P'        Test TRACE=P(rint)                           
         JE    *+12                                                             
         CLI   MCTRACE,C'B'        Test TRACE=B(oth)                            
         JNE   TRCLOGX                                                          
         GOTOR VPRINT,(R2)         Print trace line                             
                                                                                
TRCLOGX  J     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* Print system open list table                                        *         
***********************************************************************         
                                                                                
PRTSYS   NTR1  LABEL=*                                                          
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         MVC   PMESSAGE,SPACES                                                  
         GOTOR PRTLOG                                                           
         MVC   PMESSAGE(L'SYSLIT1),SYSLIT1                                      
         GOTOR PRTLOG                                                           
         MVC   PMESSAGE(L'SYSLIT2),SYSLIT2                                      
         GOTOR PRTLOG                                                           
         GOTOR PRTLOG                                                           
         MVC   PMESSAGE(L'SYSLIT3),SYSLIT3                                      
         GOTOR PRTLOG                                                           
                                                                                
         LARL  R2,SEOLST                                                        
         USING SEOLSTD,R2                                                       
         LHI   R0,SEOLSTM                                                       
PRTSYS02 CLI   SEOLNUM,SEOLEOTQ    Test end of list                             
         JE    PRTSYSX                                                          
         MVC   PMESSAGE(L'SEOLNAM),SEOLNAM                                      
         GOTOR MCVHEXOU,DMCB,SEOLNUM,PMESSAGE+8,L'SEOLNUM,0                     
         MVC   PMESSAGE+13(L'SYSOPN),SYSOPN                                     
         TM    SEOLFLAG,SEOLFOPN                                                
         JZ    PRTSYS04                                                         
         TM    SEOLFLAG,SEOLFGLB   Test global file                             
         JZ    PRTSYS06                                                         
         MVC   PMESSAGE+13+L'SYSOPN+1(L'SYSGLB),SYSGLB                          
         J     PRTSYS06                                                         
PRTSYS04 TM    SEOLFLAG,SEOLFNOP                                                
         JZ    *+14                                                             
         MVC   PMESSAGE+13(L'SYSNOOP),SYSNOOP                                   
         J     PRTSYS06                                                         
         MVC   PMESSAGE+13(L'SYSCLS),SYSCLS                                     
PRTSYS06 GOTOR PRTLOG                                                           
         AHI   R2,SEOLSTL                                                       
         JCT   R0,PRTSYS02                                                      
                                                                                
PRTSYSX  GOTOR PRTLOG                                                           
         J     EXIT                                                             
         DROP  R2                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Reconnect to MQ if connection broken                                *         
***********************************************************************         
                                                                                
MQRECON  NTR1  LABEL=*                                                          
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
*                                                                               
         LARL  R4,MQRECON                                                       
         USING MQRECON,R4                                                       
*                                                                               
         MVC   PMESSAGE(L'MQCONBRO),MQCONBRO                                    
         BRAS  RE,PRTLOG                                                        
         LA    R3,1                                                             
*                                                                               
         LARL  R1,MQDSC                                                         
         GOTOR RCALLMQ             Disconnect from MQ                           
         WTO   'DISCONNECTED ',MCSFLAG=HRDCPY                                   
*                                                                               
RECON1   LR    R1,R3                                                            
         MHI   R1,1000             Wait for TRY*10 SECONDS                      
         ST    R1,FULL                                                          
         STIMER WAIT,BINTVL=FULL                                                
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         LA    R3,1(R3)            Try 20 times up to 200 seconds               
         CHI   R3,21                                                            
         JNL   *+2                 TOO MANY RETRIES - 30 mins total             
*                                                                               
         MVC   PMESSAGE(L'MQRECO),MQRECO                                        
         EDIT  (R3),(5,PMESSAGE+10),ALIGN=LEFT                                  
         BRAS  RE,PRTLOG                                                        
*                                                                               
         BRAS  RE,MQINIT           RE-INIT MQ                                   
         JNE   RECON1                                                           
*                                                                               
RECONX   XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to handle timer pops                                        *         
***********************************************************************         
                                                                                
TIMPOP   NBASE 0,**TPOP**,ATIMWORK                                              
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         ICM   RF,15,RTOTPOP       Increment total pops                         
         AHI   RF,1                                                             
         STCM  RF,15,RTOTPOP                                                    
                                                                                
         CLI   MCPOPSW,0           Are timer interrupts enabled?                
         JNE   TIMPOPX             no, bypass (so we shouldnt be here?)         
         MVI   MCPOPSW,TIMERPOP    Set timer exit active                        
                                                                                
         GOTOR TSTOPS              Test any operator commands pending           
                                                                                
         OC    MCRTPCPU,MCRTPCPU   See if cpu time limit                        
         JZ    TIMPOP02                                                         
         ICM   R0,15,CPURTP        Accumulate RTP time                          
         A     R0,LOOPTIMR                                                      
         STCM  R0,15,CPURTP                                                     
                                                                                
TIMPOP02 CLI   ADWAITNT,0          Did we enter ADWAIT since timer set?         
         JNE   TIMPOP10            yes, so probably not looping                 
                                                                                
         XC    WORK,WORK           Tell operator may be looping                 
         LA    R3,WORK                                                          
         USING WTOD,R3                                                          
                                                                                
         LA    R0,REPLY            Set A(reply)                                 
         ST    R0,WTOARPLY         Set A(operator response)                     
         MVI   WTOLRPLY,1          Set L'operator response                      
         XC    ECBCOMS,ECBCOMS                                                  
         LA    R0,ECBCOMS                                                       
         ST    R0,WTOAECB          Set A(ECB)                                   
         LA    R2,WTOMESS          R2=A(message text)                           
                                                                                
         MVC   0(L'POPMSG1,R2),POPMSG1 'May be looping'                         
         MVC   11(L'MCSTEP,R2),MCSTEP                                           
         AHI   R2,L'POPMSG1                                                     
                                                                                
         LA    R1,WTOLRPLY         Point to length of reply                     
         CLI   MCTSTRUN,FF         If RUN=TEST                                  
         JE    TIMPOP04            no replies                                   
         MVC   0(L'POPMSG2,R2),POPMSG2 'enter for details'                      
         AHI   R2,L'POPMSG2                                                     
         J     TIMPOP06                                                         
                                                                                
TIMPOP04 LA    R1,WTOLMESS         Point to message length                      
                                                                                
TIMPOP06 LA    R0,WTOMESS-4                                                     
         SR    R2,R0               Calculate & set message length               
         STCM  R2,3,WTOLMESS                                                    
         SVC   35                  Issue WTOR                                   
                                                                                
         CLI   MCTSTRUN,FF         If RUN=TEST                                  
         JE    TIMPOP08            no reply to wait for                         
         L     R1,WTOAECB          Wait for operator response                   
         WAIT  ECB=(1)                                                          
TIMPOP08 GOTOR PUTOPS,TQELOOP      Write details to console                     
         CLI   MCTSTRUN,FF         IF RUN=TEST                                  
         JE    TIMPOP30            always stop                                  
         J     TIMPOP18                                                         
         DROP  R3                                                               
                                                                                
TIMPOP10 OC    MAXPOPS,MAXPOPS     Test maximum pops set                        
         JZ    *+14                                                             
         CLC   RTOTPOP,MAXPOPS     Test reached maximum                         
         JH    TIMPOPX                                                          
                                                                                
         OC    MCRTPCPU,MCRTPCPU   See if cpu time limit                        
         JZ    TIMPOP20                                                         
         CLC   CPURTPNX,CPURTP     Have we hit next RTP time                    
         JH    TIMPOP20                                                         
                                                                                
         L     R1,CPURTPNX                                                      
         A     R1,MCRTPCPU                                                      
         ST    R1,CPURTPNX         Set next RTPCPU limit                        
                                                                                
         GOTOR PUTOPS,TQECPUE      Write details to console                     
         CLI   MCTSTRUN,FF         IF RUN=TEST                                  
         JE    TIMPOP30            always stop                                  
         CLI   OPREPLY,C'Y'        Test operator reply wanted                   
         JNE   TIMPOP20            no, carry on                                 
                                                                                
TIMPOP18 CLC   OPNOW,REPLY         Test oper resp = now                         
         JE    TIMPOP30                                                         
         CLC   OPDUMP,REPLY        Test oper resp = dump                        
         JE    TIMPOP30                                                         
                                                                                
***********************************************************************         
* Reset the timer for new interval                                    *         
***********************************************************************         
                                                                                
TIMPOP20 GOTOR SETPOP              Set timer exit enabled                       
         J     TIMPOPX                                                          
                                                                                
***********************************************************************         
* KILL THE APPLICATION. ERROR ALREADY SET                             *         
***********************************************************************         
                                                                                
TIMPOP30 MVI   ERROR,TQECPUE       Set CPU exceeded error                       
         L     RF,X'21C'(,0)       Get PSA TCB old                              
         L     RF,0(RF)            Point to current RB                          
TIMPOP32 L     R1,28(RF)           Point to previous RB                         
         CLC   SORT,96(R1)         If sort kill here                            
         JE    TIMPOP34                                                         
         CLC   29(3,R1),X'21D'(0)  Does it point to TCB                         
         JE    TIMPOP34                                                         
         LR    RF,R1                                                            
         J     TIMPOP32                                                         
TIMPOP34 L     RE,20(R1)           Get PSW instruction address                  
         ST    RE,MCCHKPSW         Save inst address                            
         SAM31 ,                                                                
         MVC   LOOPINST,0(RE)      save instruction code                        
         MVI   0(RE),0             force abend                                  
         SAM24 ,                                                                
                                                                                
TIMPOPX  J     EXIT                                                             
                                                                                
ATIMWORK DC    A(ERRWORK)                                                       
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to handle program checks                                    *         
***********************************************************************         
                                                                                
         USING PRGCHK,RF                                                        
PRGCHK   CHI   R0,12               Test SDWA acquired                           
         JNE   *+8                 No, just let MVS percolate                   
         SR    RF,RF                                                            
         BR    RE                                                               
                                                                                
         SAC   0                   Turn off access mode                         
         SAM24 ,                   Turn off 31 bit mode                         
         STM   RE,RC,ABENDGRS      Save abend general registers                 
         STAM  AR0,ARF,ABENDARS    Save access registers                        
                                                                                
         LAM   AR0,ARF,ARZERO      CLEAR ACCESS REGISTERS                       
                                                                                
         LR    RB,RF                                                            
         DROP  RF                                                               
         USING PRGCHK,RB                                                        
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         LAM   AR0,ARF,ARZERO      Must have done SETREG                        
                                                                                
         L     RD,AERRWORK         Point to abend work area                     
                                                                                
         ST    RD,ESTAERD          Save abend RD                                
         ST    R1,ESTAER1          Save abend R1                                
                                                                                
         MVI   MCPOPSW,TIMERABN    Inhibit timer interrupts for abend           
         TTIMER CANCEL,MIC,DUB     Reset timer                                  
         LG    GR1,DUB             Get remaining time                           
         SRAG  GR1,GR1,12          Bit 51 =sec/1000000, shift to bit 63         
         DSGF  GR0,F10000          convert to sec/100                           
         ICM   R0,15,CPURTP        Accumulate RTP time                          
         AR    R0,R1                                                            
         STCM  R0,15,CPURTP                                                     
                                                                                
         OC    REQNO,REQNO         Test started running requests                
         JZ    PRGCHK00            No, still initialising                       
         GOTOR GETCPU                                                           
         LG    GR1,CPUSAVE         CPU STOP                                     
         SLG   GR1,CPUREQ                                                       
         STG   GR1,CPUREQ          Set CPU time used by DDLINK & server         
                                                                                
PRGCHK00 CLI   ERROR,TQEGOOD       Test error condition set                     
         JNE   *+8                 Yes - leave it alone                         
         MVI   ERROR,TQEPCHK       Set program check error                      
                                                                                
         LARL  RE,LP_XDB           Point to local LP_XDB                        
         ST    RE,LP_ALPXD         (DDLINK changes this)                        
                                                                                
         L     R1,ESTAER1          Restore SDWA pointer                         
         USING SDWA,R1                                                          
         MVC   ABENDPSW,SDWAEC1                                                 
         MVC   MCUSRPSW,SDWAEC1    PSW at abend                                 
         MVC   MCINTCD,SDWAICD1    Save interrupt code in PSW                   
         MVC   MCUSRRGS,SDWAGRSV   R0-RF at abend                               
         DROP  R1                                                               
                                                                                
         L     RF,MCUTL            Save UTL+4 (SE) at abend                     
         MVC   ABENDSE,4(RF)                                                    
                                                                                
         GOTOR RSORTER,DMCB,SORTEND                                             
                                                                                
         TM    RUNINDS1,RUNIUPDT   Test updative (global) mode                  
         JZ    PRGCHK02                                                         
         TM    LP_INDS,LP_IRECV    Test system recovery required                
         JZ    PRGCHK02                                                         
         MVC   PMESSAGE(L'INFLIT5),INFLIT5                                      
         GOTOR PRTLOG                                                           
         GOTOR DMGR,DMCB,(0,DMRCVR),0                                           
PRGCHK02 NI    LP_INDS,FF-(LP_IOUPD+LP_IRECV+LP_IGLOB)                          
                                                                                
PRGCHK04 MVI   MCREASON,C'P'       Assume program check                         
         OC    MCCHKPSW,MCCHKPSW   Test loop address saved at TIMPOP30          
         JZ    PRGCHKX             No, exit                                     
                                                                                
         SAM31 ,                                                                
         L     RE,MCCHKPSW                                                      
         MVC   0(L'LOOPINST,RE),LOOPINST Restore loop code                      
         XC    MCCHKPSW,MCCHKPSW                                                
         SAM24 ,                                                                
         LA    R3,666              Change S0C1 to U666(Now)/U667(Dump)          
         CLC   OPNOW,REPLY                                                      
         JE    *+8                                                              
         LA    R3,667                                                           
         L     R1,ESTAER1                                                       
         PUSH  ACONTROL                                                         
         ACONTROL TYPECHECK(NOREGISTER)                                         
         SETRP COMPCOD=((R3),USER)                                              
         POP   ACONTROL                                                         
         MVI   MCREASON,C'L'       indicate operator canceled loop              
         MVI   MCINTCD,FF          Set loop indicator in PSW                    
                                                                                
PRGCHKX  J     CHKALL                                                           
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Handle exceptional conditions (program checks etc.)                 *         
***********************************************************************         
                                                                                
CHKALL   GOTOR SETREG              Set RUNNER registers                         
                                                                                
         L     RD,AERRWORK         Point to abend work area                     
                                                                                
         TM    AOPERECB,PROCMC     Were we processing operator input            
         JZ    CHKALL02            No                                           
         L     R3,WCOMM            Well we aren't now!                          
         LA    R3,COMCIBPT-COMLIST(R3)                                          
         L     R2,0(R3)                                                         
         QEDIT ORIGIN=(R3),BLOCK=(R2) Free up comm block                        
         NI    AOPERECB,FF-(PROCMC) and reset processing op input               
                                                                                
CHKALL02 CLI   MCBLOW,C'X'         Test died in runlast after error             
         JE    CHKALL90                                                         
         CLI   MCBLOW,C'R'         Test died in reqlast after error             
         JE    CHKALL90                                                         
         CLI   MCBLOW,C'P'         Abend loop                                   
         JE    CHKALL04                                                         
         CLI   MCBLOW,C'Q'         Stop loop of reqlast/runlast dumps           
         JNE   CHKALL06                                                         
         MVI   MCBLOW,C'P'                                                      
                                                                                
CHKALL04 L     R1,ESTAER1          Issue abend 665 and terminate                
         PUSH  ACONTROL                                                         
         ACONTROL TYPECHECK(NOREGISTER)                                         
         SETRP DUMP=NO,RC=0,COMPCOD=(665,USER)                                  
         POP   ACONTROL                                                         
         J     CHKALLX                                                          
                                                                                
CHKALL06 CLC   MCUSRPSA,MCLSTPSW+1                                              
         JE    CHKALL20            Avoid multiple dumps                         
         MVC   MCLSTPSW,MCUSRPSF                                                
         SR    R0,R0                                                            
         ICM   R0,7,MCNDUMPS                                                    
         AHI   R0,1                Increment dump count                         
         STCM  R0,7,MCNDUMPS                                                    
                                                                                
         L     R4,ESTAER1                                                       
         USING SDWA,R4             R4=A(SDWA)                                   
                                                                                
         CLC   ABEND103,SDWACMPC   Test datamgr open error                      
         JNE   CHKALL08                                                         
                                                                                
         LA    R3,WORK                                                          
         USING WTOD,R3                                                          
         XC    WTOD(WTOPARML),WTOD                                              
                                                                                
         MVC   PCMESS1,LPCMESS1                                                 
         LA    R2,WTOMESS          Build message                                
                                                                                
         MVC   PCM1STEP,MCSTEP                                                  
         LLC   RE,MCUSRRR3                                                      
         SRDL  RE,4                                                             
         SRL   RF,32-4                                                          
         IC    RE,HEXTAB(RE)                                                    
         IC    RF,HEXTAB(RF)                                                    
         STC   RE,PCM1SEN+0                                                     
         STC   RF,PCM1SEN+1                                                     
         MVC   PCM1FILE,MCUSRRR3+1                                              
         MVC   0(L'PCMESS1,R2),PCMESS1                                          
         AHI   R2,L'PCMESS1                                                     
                                                                                
         LA    R1,WTOLMESS         Point to message length                      
         LA    R0,WTOMESS-4                                                     
         SR    R2,R0               Calculate & set message length               
         STCM  R2,3,WTOLMESS                                                    
         SVC   35                  Issue WTOR                                   
         J     CHKALL90                                                         
                                                                                
CHKALL08 CLC   ABEND990,SDWACMPC   Test allocation error                        
         JE    CHKALL90                                                         
         CLC   ABEND991,SDWACMPC   Test file open error                         
         JE    CHKALL90                                                         
         CLC   ABEND666,SDWACMPC   Test stop now without dump                   
         JE    CHKALL20                                                         
         CLC   ABEND669,SDWACMPC   Test bad patch card                          
         JE    CHKALL90                                                         
         CLC   ABEND670,SDWACMPC   Test bad cpuid or concurrent update          
         JE    CHKALL90                                                         
         CLC   ABEND899,SDWACMPC   Test for special                             
         JE    CHKALL90                                                         
         CLC   OPERCANC,SDWACMPC   Test operator cancel                         
         JE    CHKALL90                                                         
         CLC   ABEND268,SDWACMPC   Test locktab full                            
         JNE   *+8                                                              
         MVI   ERROR,TQELKTFL      set locktab full error                       
         CLC   ABEND269,SDWACMPC   Test deadly embrace                          
         JNE   CHKALL10                                                         
         MVI   ERROR,TQEDDLY       set deadly embrace error                     
         CLI   WORKING,YESQ        Test working mode on                         
         JNE   CHKALL10            No, so don't attempt retry                   
         TM    RUNINDMQ,RUNIMQUP   Test running MQ upload                       
         JZ    CHKALL10            No, so never retry it                        
         LLC   RF,DDLYNUM          increment retry count                        
         LA    RF,1(,RF)                                                        
         STC   RF,DDLYNUM                                                       
         CLC   DDLYNUM,MAXDDLY     Exceeded retry limit?                        
         JH    CHKALL10            Yes, no more retries                         
         SR    R0,R0                                                            
         ICM   R0,7,MCNDUMPS                                                    
         BCTR  R0,1                Undo increment dump count                    
         STCM  R0,7,MCNDUMPS                                                    
         J     CHKALL80            go retry deadly                              
                                                                                
CHKALL10 MVI   DDLYNUM,0           Clear deadly retry                           
                                                                                
         TM    RUNINDS2,RUNISYSM   DUMP=SYSM IN USE                             
         JZ    CHKALL12                                                         
         GOTOR VMDUMPER,DMCB,=C'DUMP',0  YES, USE MDUMPER                       
                                                                                
         CLI   MCDUMP,C'O'         TEST FULL OS DUMP REQUIRED                   
         JE    CHKALL90                                                         
         J     CHKALL20                                                         
                                                                                
CHKALL12 XC    MCNXTTCB,MCNXTTCB   Let's not have this again                    
         CLI   MCDUMP,C'O'         TEST FULL OS DUMP REQUIRED                   
         JE    CHKALL90                                                         
                                                                                
         OC    MCREMPQK,MCREMPQK   ELSE USE PDUMPER                             
         JZ    CHKALL14            skip if not SOON                             
         ICM   RE,15,VPDSOON       Test PDUMPER SOON area availabe              
         JZ    CHKALL14                                                         
         MVC   0(8,RE),MCUSERID    First 8 characters of userid                 
         MVC   8(5,RE),MCREMPQK+2  Report id and report number                  
         MVC   13(2,RE),MCPROG     Program id                                   
         MVC   15(3,RE),SDWACMPC   Completion code (x'sssuuu')                  
         MVC   21(1,RE),MCOVSYS    System id                                    
                                                                                
CHKALL14 ICM   RE,15,MCBXAREA                                                   
         JZ    *+8                                                              
         MVI   BOXOFF-BOXD(RE),YESQ                                             
         MVC   WORK(L'MCREMPQK),MCREMPQK                                        
         XC    MCREMPQK,MCREMPQK   and clear during PDUMP                       
                                                                                
         L     R0,ARUNNER          Establish start of dump                      
         L     RF,MCDMPEND         Get end of region address                    
         CLI   MCDUMP,YESQ                                                      
         JE    CHKALL16                                                         
         ICM   R1,15,MCAWORK       Point to start of dump                       
         JZ    CHKALL16                                                         
         LR    R0,R1                                                            
CHKALL16 GOTOR MCVPDUMP,DMCB,MCUSREGS,(R0),(RF)                                 
                                                                                
         ICM   R0,15,MCUSRDMP+00                                                
         JZ    CHKALL18                                                         
         ICM   RE,15,MCUSRDMP+04                                                
         GOTOR (RF),(R1),,(R0),(RE)                                             
                                                                                
         ICM   R0,15,MCUSRDMP+08                                                
         JZ    CHKALL18                                                         
         ICM   RE,15,MCUSRDMP+12                                                
         GOTOR (RF),(R1),,(R0),(RE)                                             
                                                                                
CHKALL18 MVC   MCREMPQK,WORK       Restore 'soon' id                            
         L     RF,MCVLOGOC                                                      
         ICM   RE,15,LOGOSGTD-LOGOD(RF)                                         
         AHI   RE,1                                                             
         STCM  RE,15,LOGOSGMX-LOGOD(RF)                                         
         GOTOR VPRINT,DMCB,P,BC01  Force new print segment                      
         L     RF,MCVLOGOC                                                      
         MVC   LOGOSGMX-LOGOD(,RF),HOBOFF                                       
         C     R3,LOGORUNP-LOGOD(RF)                                            
         JNL   *+8                                                              
         A     R3,LOGORUNP-LOGOD(RF)                                            
         STCM  R3,15,MCNXTPGS      Restore page limit                           
         ICM   R1,15,MCBXAREA                                                   
         JZ    *+8                                                              
         MVI   BOXOFF-BOXD(R1),0                                                
                                                                                
CHKALL20 CLC   ABEND666,SDWACMPC   Test stop now without dump                   
         JE    CHKALL80            Yes, go on to next request                   
         CLC   ABEND667,SDWACMPC   Test stop now with dump                      
         JE    CHKALL80            Yes, go on to next request                   
         CLC   OPERCANC,SDWACMPC   Test operator cancel                         
         JE    CHKALL90            Yes, go directly to termination              
         ICM   RF,7,SDWACMPC       Get syst and user                            
         SLL   RF,20               Shift out all but user 12 bits               
         LTR   RF,RF               See if user code non-zero                    
         JZ    CHKALL30            no, so system abend                          
         MVI   MCINTCD,FE          Set user abend                               
         CLI   WORKING,YESQ        Test working mode on                         
         JE    CHKALL30            Yes, continue                                
         J     CHKALL90            else, end runner                             
         EJECT                                                                  
*        Build and send email                                                   
*                                                                               
CHKALL30 OC    ABEND,ABEND         Test abend parameter supplied                
         JZ    *+12                                                             
         LA    R2,ABEND            Yes - this overrides server list             
         J     *+12                                                             
         ICM   R2,15,SVRABND       Test server abend list resolved              
         JZ    CHKALL60                                                         
         MVC   P,SPACES                                                         
         MVC   P(L'FAILLIT),FAILLIT                                             
         LA    RF,P+L'FAILLIT+1                                                 
         MVC   0(L'MCJOB,RF),MCJOB                                              
         LA    RF,P+L'FAILLIT+L'MCJOB                                           
         CLI   0(RF),C' '                                                       
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         AHI   RF,1                                                             
         MVI   0(RF),C'/'                                                       
         MVC   1(L'MCSTEP,RF),MCSTEP                                            
         AHI   RF,L'MCSTEP                                                      
         CLI   0(RF),SPACE                                                      
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         AHI   RF,1                                                             
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   CHKALL36                                                         
         CLI   WORKING,YESQ        Test working mode on                         
         JNE   CHKALL36                                                         
         TM    LP_FLAG2,LP_FMQIM   Test pocessing an MQ message                 
         JNZ   CHKALL34                                                         
         MVC   0(L'WKEYLIT,RF),WKEYLIT No, show worker key                      
         AHI   RF,L'WKEYLIT                                                     
         L     R1,RWRKBLK                                                       
         GOTOR PRTKEY,(R1)                                                      
         MVC   0(PRTKEYLQ,RF),WORK                                              
         AHI   RF,PRTKEYLQ                                                      
         CLI   0(RF),SPACE                                                      
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         AHI   RF,1                                                             
         J     CHKALL36                                                         
                                                                                
CHKALL34 MVC   0(L'MQIDLIT,RF),MQIDLIT MQ message shows MQID                    
         AHI   RF,L'MQIDLIT                                                     
         LR    R0,RF                                                            
         GOTOR MCVHEXOU,DMCB,WKEY,(RF),L'WKEY,0                                 
         LR    RF,R0                                                            
         AHI   RF,2*L'WKEY                                                      
                                                                                
CHKALL36 LR    R0,RF               Add error description                        
         GOTOR GETTQE,ERROR                                                     
         LR    RE,R0                                                            
         MVI   0(RE),C','                                                       
         MVC   1(L'TQETAB-1,RE),1(RF)                                           
                                                                                
         GOTOR VSMTP,DMCB,('SMTPCNCL',0)                                        
         GOTOR (RF),(R1),('SMTPAPRS',(R2)),(100,P)                              
                                                                                
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   *+12                                                             
         CLI   WORKING,YESQ                                                     
         JNE   CHKALL49                                                         
                                                                                
         MVC   P,SPACES            Request details follow                       
         MVC   P(L'REQLIT2),REQLIT2                                             
         GOTOR (RF),(R1),('SMTPAPTL',P)                                         
         MVC   P,SPACES                                                         
         GOTOR (RF),(R1)                                                        
                                                                                
         L     R2,AREQSAVE         R2=A(request record)                         
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   CHKALL42                                                         
         TM    LP_FLAG2,LP_FMQIM   Test processing an MQ message                
         JNZ   CHKALL46                                                         
         LLH   R3,0(R2)                                                         
         CHI   R3,4095             Can send first 4K of request only            
         JNH   CHKALL38                                                         
         LHI   R3,4095                                                          
                                                                                
CHKALL38 CHI   R3,40                                                            
         JL    CHKALL40                                                         
         GOTOR MCVHEXOU,DMCB,(R2),P,40,0                                        
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
         AHI   R2,40                                                            
         SHI   R3,40                                                            
         J     CHKALL38                                                         
                                                                                
CHKALL40 LTR   R3,R3                                                            
         JZ    CHKALL49                                                         
         MVC   P,SPACES                                                         
         GOTOR MCVHEXOU,DMCB,(R2),P,(R3),0                                      
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
         J     CHKALL49                                                         
                                                                                
CHKALL42 LHI   R3,10                                                            
CHKALL44 CLC   0(L'MCREQREC,R2),SPACES                                          
         JE    CHKALL49                                                         
         MVC   P(L'MCREQREC),0(R2)                                              
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
         AHI   R2,L'MCREQREC                                                    
         JCT   R3,CHKALL42                                                      
         J     CHKALL49                                                         
                                                                                
CHKALL46 L     R2,MQIBUFF          Send MQ message in e-mail body               
         L     R0,MQLIMSG                                                       
         CHI   R0,4095             Only send first 4K of message                
         JNH   CHKALL48                                                         
         LHI   R0,4095                                                          
CHKALL48 LR    R3,R0                                                            
         CHI   R3,80               Test less than 80 characters left            
         JL    *+8                                                              
         LHI   R3,80               No - set to move 80                          
         BCTR  R3,0                Subtract 1 for execute                       
         SAM31 ,                                                                
         EX    R3,CALLMVP2         Move message data to print line              
         SAM24 ,                                                                
         AHI   R3,1                Add back the one subtracted above            
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
         SR    R0,R3               Decrement remaining length                   
         JZ    CHKALL49            Zero when all sent                           
         AR    R2,R3               Bump to next part of message data            
         MVC   P,SPACES                                                         
         J     CHKALL48                                                         
                                                                                
CHKALL49 GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         MVC   P,SPACES                                                         
         EJECT                                                                  
*        Inform Operator. Ask if next request or end run                        
*                                                                               
CHKALL60 CLI   WORKING,YESQ        Test application death                       
         JNE   CHKALL82            No, just do runlast                          
                                                                                
         MVC   PCMESSA(PCMESSL),LPCMESSA                                        
         MVC   PCMASTEP,MCSTEP                                                  
                                                                                
         SR    R0,R0                                                            
         ICM   R0,7,REQNO                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  PCMAREQN,DUB                                                     
                                                                                
         L     R2,MCUSRRRB         Get user RB                                  
         LA    R2,0(R2)                                                         
         ICM   R1,15,MCUSRPSF                                                   
         N     R1,HOBOFF           R1=31 bit PSW address                        
         C     R1,MCDMPEND         Compare to current top of region             
         JNH   *+14                                                             
         MVC   PCMAMODN(L'UNKPSW),UNKPSW                                        
         J     CHKALL62                                                         
                                                                                
         L     RF,X'21C'(,0)       Get PSATOLD (current TCB address)            
         C     R1,X'70'(RF)        Get TCBFSA (first save area)                 
         JNL   *+14                                                             
         MVC   PCMAMODN(L'UNKPSW),UNKPSW                                        
         J     CHKALL62                                                         
                                                                                
         MVC   PCMAMODN,22(R2)     Show nmod name and displacement              
         CR    R1,R2               Test PSW greater than RB                     
         JNH   CHKALL62                                                         
         SR    R1,R2               Yes - calculate displacement                 
         STCM  R1,3,DUB                                                         
         GOTOR MCVHEXOU,DMCB,DUB,PCMBDISP,2,0                                   
                                                                                
CHKALL62 SR    R0,R0                                                            
         ICM   R0,7,REQNO                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  PCMAREQN,DUB                                                     
                                                                                
         SR    R0,R0                                                            
         ICM   R0,7,MCNDUMPS                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  PCMNFAIL,DUB                                                     
                                                                                
         MVC   PCMRSNC(L'CANCAPL),CANCAPL                                       
         MVI   MCCLOERR,C'L'       Set loop                                     
         CLI   MCREASON,C'L'       Test loop being cancelled                    
         JE    CHKALL66            Yes, skip                                    
         MVI   MCCLOERR,C'Q'       Set printq error                             
         MVC   PCMRSNC(L'CANCPQE),CANCPQE                                       
                                                                                
         SR    R1,R1                                                            
         ICM   R1,7,MCUSRPSA                                                    
         TM    MCUSRPSF,X'7F'      Test 31 bit PSW                              
         JNZ   *+14                                                             
         CLC   PQFULLM,0(R1)       No - test for print queue full               
         JE    CHKALL66                                                         
                                                                                
         LARL  RF,CANTAB           Look-up program check type                   
CHKALL64 CLI   0(RF),0                                                          
         JE    *+10                                                             
         CLC   0(1,RF),MCINTCD                                                  
         JE    *+12                                                             
         AHI   RF,L'CANTAB                                                      
         J     CHKALL64                                                         
         MVC   PCMRSNC(L'CANTAB-1),1(RF)                                        
         MVI   MCCLOERR,C'A'       Set abend                                    
                                                                                
CHKALL66 SR    RF,RF               Set no bad response                          
                                                                                
CHKALL68 CLI   MCREASON,C'L'       Test terminating loop or RTPCPU.             
         JE    CHKALL80            Yes - operator has already chosen            
                                                                                
         LA    R3,WORK                                                          
         USING WTOD,R3                                                          
         XC    WTOD(WTOPARML),WTOD                                              
                                                                                
         LA    R0,REPLY            Set A(reply)                                 
         ST    R0,WTOARPLY         Set A(operator response)                     
         MVI   WTOLRPLY,4          Set L'operator response                      
         XC    ECBCOMS,ECBCOMS                                                  
         LA    R0,ECBCOMS                                                       
         ST    R0,WTOAECB          Set A(ECB)                                   
                                                                                
         LA    R2,WTOMESS          Build message                                
         LTR   RF,RF                                                            
         JZ    *+20                                                             
         XC    WTOMESS(WTOMESSL),WTOMESS                                        
         MVC   0(L'BADREPLY,R2),BADREPLY                                        
         AHI   R2,L'BADREPLY                                                    
                                                                                
         MVC   0(L'PCMESSA,R2),PCMESSA                                          
         AHI   R2,L'PCMESSA                                                     
         MVC   0(L'PCMESSB,R2),PCMESSB                                          
         AHI   R2,L'PCMESSB                                                     
                                                                                
         LA    R1,WTOLRPLY         Point to length of reply                     
         CLI   MCTSTRUN,FF         Test RUN=TEST                                
         JE    CHKALL70                                                         
         MVC   0(L'PCMESSC,R2),PCMESSC                                          
         AHI   R2,L'PCMESSC                                                     
         XC    REPLY,REPLY                                                      
         J     CHKALL72                                                         
                                                                                
CHKALL70 LA    R1,WTOLMESS         Point to message length                      
                                                                                
CHKALL72 CLI   OPMESS,C'N'         Test OPMESS=N set                            
         JE    CHKALL80                                                         
                                                                                
         LA    R0,WTOMESS-4                                                     
         SR    R2,R0               Calculate & set message length               
         STCM  R2,3,WTOLMESS                                                    
         SVC   35                  Issue WTOR                                   
                                                                                
         CLI   MCTSTRUN,FF         If RUN=TEST                                  
         JE    CHKALL82            always stop                                  
         L     R1,WTOAECB          Wait for operator response                   
         WAIT  ECB=(1)                                                          
                                                                                
         CLC   OPSTOP,REPLY                                                     
         JE    CHKALL82                                                         
         CLC   OPNEXT,REPLY                                                     
         JE    CHKALL80                                                         
         LHI   RF,1                Put out 'bad reply' etc.                     
         J     CHKALL68                                                         
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
*  Continue running via RETRY routine. Either REQLAST or RUNLAST                
**********************************************************************          
CHKALL80 CLI   WORKING,YESQ        Test application death                       
         JNE   CHKALL82            No, just do runlast                          
         MVC   MCUSRRGS,MCREQLST   Yes - set request last registers             
         J     CHKALL84                                                         
                                                                                
CHKALL82 MVC   MCUSRRGS,MCRUNLST   Set run last registers                       
                                                                                
CHKALL84 MVI   MCBLOW,C'Q'                                                      
         OC    MCUSRRGS,MCUSRRGS   Test if regs initialized                     
         JZ    CHKALL90            No, no retry                                 
         TM    SDWAERRD,SDWACLUP                                                
         JNZ   CHKALL90            Retry not allowed                            
                                                                                
         CLI   WORKING,YESQ        Test application death                       
         JNE   CHKALL86                                                         
         GOTOR SETPOP              Yes, re-enable timer                         
                                                                                
CHKALL86 LARL  R3,RETRY            Set retry routine address                    
         L     R1,ESTAER1          Restore SDWA pointer                         
         PUSH  ACONTROL                                                         
         ACONTROL TYPECHECK(NOREGISTER)                                         
         CLI   DDLYNUM,0           No dump if retrying deadly                   
         JNE   CHKALL88                                                         
         TM    RUNINDS2,RUNISYSM   DUMP=SYSM IN USE                             
         JZ    CHKALL88                                                         
         CLC   ABEND666,SDWACMPC   Test stop now without dump                   
         JE    CHKALL88            Yes, no dump needed                          
         SETRP DUMP=YES,RC=4,RETADDR=(3),FRESDWA=YES     MDUMPER                
         J     CHKALLX                                                          
CHKALL88 SETRP DUMP=NO,RC=4,RETADDR=(3),FRESDWA=YES      PDUMPER                
         POP   ACONTROL                                                         
         J     CHKALLX                                                          
*                                                                               
*        Terminate program now with MVS dump                                    
*                                                                               
CHKALL90 L     R1,ESTAER1                                                       
         PUSH  ACONTROL                                                         
         ACONTROL TYPECHECK(NOREGISTER)                                         
         SETRP DUMP=YES,RC=0                                                    
         POP   ACONTROL                                                         
         J     CHKALLX                                                          
                                                                                
CHKALLX  L     RD,ESTAERD          Return to MVS abend handler                  
         LARL  RE,ABENDGRS                                                      
         LM    RE,RC,0(RE)                                                      
         BR    RE                                                               
         DROP  R4                                                               
                                                                                
         USING *,RF                                                             
RETRY    GOTOR SETREG              Set RUNNER registers                         
         LM    R0,RF,MCUSRRGS                                                   
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
         DS    0D                                                               
         DC    C'*Abend PSW=====>'                                              
ABENDPSW DC    F'0'                                                             
                                                                                
         DS    0D                                                               
         DC    C'*Abend general registers RE-RC=>'                              
ABENDGRS DC    15F'0'                                                           
                                                                                
         DS    0D                                                               
         DC    C'*Abend access  registers R0-RF=>'                              
ABENDARS DC    16F'0'                                                           
         EJECT                                                                  
***********************************************************************         
* Handle I/O waits                                                    *         
***********************************************************************         
                                                                                
ADWAIT   NTR1  LABEL=*                                                          
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         MVC   DMCBSAV,DMCB        Save caller's DMCB as we may use it          
                                                                                
         ST    R1,MCSVECB          Save ECB pointer                             
                                                                                
         MVI   ADWAITNT,C'Y'       ADWAIT entered                               
                                                                                
         TM    MCSVECB,PRINTECB    Test called from PRINT                       
         JNZ   ADWAIT16                                                         
                                                                                
         CLI   MCPOPSW,0           Test timer suspended                         
         JNE   ADWAIT08            yes, don't do I/O counts/checks              
                                                                                
         ICM   RF,15,RTOTSIO       Increment I/O count for request              
         AHI   RF,1                                                             
         STCM  RF,15,RTOTSIO                                                    
                                                                                
         OC    MAXIOS,MAXIOS       Test maximum I/O count set                   
         JZ    ADWAIT04                                                         
         CLC   RTOTSIO,MAXIOS      Test maximum I/O exceeded                    
         JNH   ADWAIT04                                                         
         MVI   ERROR,TQEIOCP       Set I/O count exceeded                       
         DC    H'0'                and die                                      
                                                                                
ADWAIT04 L     RE,MCACTIOS         Get actual excp count                        
         AHI   RE,1                                                             
         ST    RE,MCACTIOS                                                      
         S     RE,MCRUNSIO         RE=I/O count for this request                
         ICM   RF,15,MCNXTIOS      RF=maximum I/O count per request             
         JZ    ADWAIT06                                                         
         CR    RE,RF                                                            
         JL    ADWAIT06                                                         
         CLI   WORKING,YESQ        Test application running yet                 
         JNE   ADWAIT06                                                         
         GOTOR PUTOPS,TQEIOCP                                                   
         L     R0,MCRTPIOS                                                      
         A     R0,MCNXTIOS                                                      
         ST    R0,MCNXTIOS                                                      
                                                                                
ADWAIT06 OC    MCNXTTCB,MCNXTTCB   Test there was a time limit                  
         JZ    ADWAIT08                                                         
         CLI   WORKING,YESQ        Test application running yet                 
         JNE   ADWAIT08                                                         
         TIME  BIN                 Get elapsed job step timing                  
         S     R0,MCJOBSTM                                                      
         C     R0,MCNXTTCB         Compare to threshold                         
         JL    ADWAIT08                                                         
         GOTOR PUTOPS,TQETIME                                                   
         L     R0,MCRTPTCB                                                      
         A     R0,MCNXTTCB                                                      
         ST    R0,MCNXTTCB                                                      
                                                                                
ADWAIT08 L     R1,MCSVECB                                                       
         WAIT  ECB=(1)                                                          
*&&DO                                                                           
         TM    RUNINDS1,RUNIUPDT   Yes - turn on update flag                    
         JZ    ADWAIT11            Only set if update occured                   
         LARL  RF,SSB                                                           
         OC    SAVESIN,SAVESIN                                                  
         JNZ   *+10                Only keep 1st sin for request                
         MVC   SAVESIN,SSOSIN-SSOOFF(RF)                                        
*&&                                                                             
ADWAIT11 CLI   MCPOPSW,0           Test timer suspended                         
         JNE   ADWAIT18            yes, don't do limit checks                   
         CLI   WORKING,YESQ        Test application running yet                 
         JNE   ADWAIT18                                                         
         L     R2,LP_ALPXD                                                      
         USING LP_XD,R2                                                         
         OC    LP_QCPUV,LP_QCPUV   Test CPU(+SRB) limit set                     
         JZ    ADWAIT12                                                         
*                                                                               
         GOTOR GETCPU                                                           
         LG    GR1,CPUSAVE                                                      
         SLG   GR1,CPUREQ                                                       
         DSGF  GR0,F10000                                                       
         C     R1,LP_QCPUV         Test CPU value exceeded                      
         JNH   ADWAIT12                                                         
         MVI   ERROR,LP_QECPU      Yes - set CPU exceeded error                 
         J     ADWAIT14                                                         
                                                                                
ADWAIT12 OC    LP_QEXCP,LP_QEXCP   Test I/O limit set                           
         JZ    ADWAIT18                                                         
         CLC   RTOTSIO,LP_QEXCP    Test I/O limit exceeded                      
         JNH   ADWAIT18                                                         
         MVI   ERROR,LP_QEIOS      Yes - set I/Os exceeded error                
                                                                                
ADWAIT14 L     RD,SAVERD           Restore RUNNER RD value                      
         LM    RE,RC,12(RD)        Restore RUNNER registers                     
         J     ENDREQ              Go to end of request handling                
                                                                                
ADWAIT16 ICM   RF,15,RTOTPIO       Increment print I/O count                    
         AHI   RF,1                                                             
         STCM  RF,15,RTOTPIO                                                    
         CLI   MCPOPSW,0           Test timer suspended                         
         JNE   ADWAIT18            yes, don't do page limit checks              
         OC    MCNXTPGS,MCNXTPGS   Test there was a page limit                  
         JZ    ADWAIT18                                                         
         OC    MCREQNO,MCREQNO                                                  
         JZ    ADWAIT18                                                         
         L     RF,MCVLOGOC                                                      
         L     R0,LOGORUNP-LOGOD(RF)                                            
         S     R0,MCRUNPAG                                                      
         C     R0,MCNXTPGS         Compare to threshold                         
         JL    ADWAIT18                                                         
         GOTOR PUTOPS,TQEPAGE                                                   
         L     R0,MCRTPPGS                                                      
         A     R0,MCNXTPGS                                                      
         ST    R0,MCNXTPGS                                                      
                                                                                
ADWAIT18 GOTOR TSTCANAW            Test cancel work (ADWAIT entry)              
         GOTOR TSTOPS              Test any operator commands pending           
                                                                                
         CLI   MCPOPSW,0           Test timer suspended                         
         JNE   ADWAIT20            yes, don't mess with it                      
         TTIMER  ,MIC,DUB          test timer running                           
         OC    DUB,DUB                 (e.g. SORTER stops it)                   
         JNZ   ADWAIT20            yes, skip                                    
         GOTOR SETPOP              no, restart it                               
                                                                                
ADWAIT20 MVC   DMCB(L'DMCBSAV),DMCBSAV Restore caller's DMCB                    
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Interface with operator to handle exceptional conditions            *         
***********************************************************************         
                                                                                
PUTOPS   NTR1  LABEL=*                                                          
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         STC   R1,PUTOPTYP         Set error number                             
                                                                                
         CLI   MCPOPSW,0           Is timer enabled                             
         JNE   PUTOPS02            No                                           
         TTIMER CANCEL,MIC,DUB     Reset timer                                  
         LG    GR1,DUB             Get remaining time                           
         SRAG  GR1,GR1,12          Bit 51 =sec/1000000, shift to bit 63         
         DSGF  GR0,F10000          convert to sec/100                           
         ICM   R0,15,CPURTP        Accumulate RTP time                          
         AR    R0,R1                                                            
         STCM  R0,15,CPURTP                                                     
         MVI   MCPOPSW,TIMEROPS    Inhibit timer for Ops messages               
                                                                                
PUTOPS02 MVC   OPMESSA(OPMESSL),LOPMESSA                                        
         MVC   OPMASTEP,MCSTEP                                                  
                                                                                
         CLI   PUTOPTYP,TQETIME    Flag exceeded counter                        
         JNE   *+8                                                              
         MVI   OPMAET-1,C'*'                                                    
         CLI   PUTOPTYP,TQELOOP                                                 
         JE    *+12                                                             
         CLI   PUTOPTYP,TQECPUE                                                 
         JNE   *+8                                                              
         MVI   OPMBTCB-1,C'*'                                                   
         CLI   PUTOPTYP,TQEIOCP                                                 
         JNE   *+8                                                              
         MVI   OPMBSIOS-1,C'*'                                                  
         CLI   PUTOPTYP,TQEPAGE                                                 
         JNE   *+8                                                              
         MVI   OPMBPGS-1,C'*'                                                   
                                                                                
         SR    R0,R0                                                            
         ICM   R0,7,MCREQNO        Zero if not running a request                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  OPMAREQ,DUB                                                      
                                                                                
         TIME  BIN                 R0=current time in secs*100                  
         S     R0,MCJOBSTM         Get elapsed from start of req                
         JNM   *+8                 If plus, ok                                  
         A     R0,HSPERDAY         Add a day's worth                            
         GOTOR EDTTIM1,OPMAET                                                   
                                                                                
         GOTOR GETCPU                                                           
         LG    GR1,CPUSAVE         Get cpu time                                 
         SLG   GR1,CPUREQ                                                       
         DSGF  GR0,F100                                                         
         MVC   OPMBTCB,MAXCPUTL    Display '*High*'                             
         CGF   GR1,MAXCPUT         Test it fits                                 
         JH    PUTOPS03            No                                           
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  OPMBTCB,DUB                                                      
         MVC   OPMBTCB(L'OPMBTCB-5),OPMBTCB+1                                   
         MVI   OPMBTCB+L'OPMBTCB-5,C'.'                                         
                                                                                
PUTOPS03 L     RF,MCVLOGOC                                                      
         L     R0,LOGORUNP-LOGOD(RF)                                            
         S     R0,MCRUNPAG                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  OPMBPGS,DUB                                                      
         L     R0,LOGORUNL-LOGOD(RF)                                            
         S     R0,MCRUNLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  OPMBLNS,DUB                                                      
         L     R0,MCACTIOS                                                      
         S     R0,MCRUNSIO                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  OPMBSIOS,DUB                                                     
         SR    RF,RF                                                            
                                                                                
PUTOPS04 LA    R3,WORK                                                          
         USING WTOD,R3                                                          
         XC    WTOD(WTOPARML),WTOD                                              
                                                                                
         LA    R0,REPLY            Set A(reply)                                 
         ST    R0,WTOARPLY         Set A(operator response)                     
         MVI   WTOLRPLY,4          Set L'operator response                      
         XC    ECBCOMS,ECBCOMS                                                  
         LA    R0,ECBCOMS                                                       
         ST    R0,WTOAECB          Set A(ECB)                                   
                                                                                
         LA    R2,WTOMESS          Build message                                
         LTR   RF,RF               Test prefix 'bad reply'                      
         JZ    *+20                                                             
         XC    WTOMESS(WTOMESSL),WTOMESS                                        
         MVC   0(L'BADREPLY,R2),BADREPLY                                        
         AHI   R2,L'BADREPLY                                                    
                                                                                
         MVC   0(L'OPMESSA+L'OPMESSB,R2),OPMESSA                                
         AHI   R2,L'OPMESSA+L'OPMESSB                                           
                                                                                
         LA    R1,WTOLRPLY                                                      
         CLI   PUTOPTYP,TQEOPER    If from status command                       
         JE    PUTOPS05                                                         
         CLI   PUTOPTYP,TQELOOP    or loop trap                                 
         JE    PUTOPS05            allow reply                                  
         CLI   MCTSTRUN,FF         If RUN=TEST                                  
         JE    PUTOPS06            no reply to wait for                         
         CLI   OPREPLY,C'Y'        Test operator reply wanted                   
         JNE   PUTOPS06                                                         
PUTOPS05 MVC   0(L'OPMESSC,R2),OPMESSC                                          
         AHI   R2,L'OPMESSC                                                     
         XC    REPLY,REPLY                                                      
         J     PUTOPS08                                                         
                                                                                
PUTOPS06 LA    R1,WTOLMESS                                                      
                                                                                
PUTOPS08 LA    R0,WTOMESS-4                                                     
         SR    R2,R0                                                            
         STCM  R2,3,WTOLMESS                                                    
         SVC   35                                                               
                                                                                
         CLI   PUTOPTYP,TQEOPER    If from status command                       
         JE    PUTOPS10                                                         
         CLI   PUTOPTYP,TQELOOP    or loop trap                                 
         JE    PUTOPS10            allow reply                                  
         CLI   MCTSTRUN,FF                                                      
         JE    PUTOPS16                                                         
         CLI   OPREPLY,C'Y'        Test operator reply wanted                   
         JNE   PUTOPSX                                                          
PUTOPS10 L     R1,WTOAECB                                                       
         WAIT  ECB=(1)                                                          
                                                                                
         CLC   OPGO,REPLY                                                       
         JE    PUTOPSX                                                          
         CLC   OPEOR,REPLY                                                      
         JE    PUTOPS12                                                         
         CLC   OPNOW,REPLY                                                      
         JE    PUTOPS14                                                         
         CLC   OPDUMP,REPLY                                                     
         JE    PUTOPS16                                                         
*        CLC   OPIGN,REPLY                                                      
*        JE    PUTOPS18                                                         
         CLC   OPRTP,REPLY                                                      
         JE    PUTOPS20                                                         
         LHI   RF,1                                                             
         J     PUTOPS04                                                         
                                                                                
PUTOPS12 XC    MCNXT,MCNXT                                                      
         J     PUTOPSX                                                          
                                                                                
PUTOPS14 LA    R1,666              NOW gives abend 666                          
         J     *+8                                                              
PUTOPS16 LA    R1,667              DUMP gives abend 667                         
         MVC   ERROR,PUTOPTYP      Set error number                             
         CLI   PUTOPTYP,TQECPUE    If from RTPCPU trap                          
         JE    PUTOPSX                                                          
         CLI   PUTOPTYP,TQELOOP    or loop trap                                 
         JE    PUTOPSX             return reply to TIMPOP                       
         ABEND (1)                 else abend here                              
                                                                                
*UTOPS18 XC    MCNXT,MCNXT         Clear run time parameters                    
*        XC    MCRTPCPU,MCRTPCPU   Why???? Otherwise as PUTOPS12 (EOR)          
*        J     PUTOPSX                                                          
                                                                                
PUTOPS20 MVC   INMESSA(INMESSL),LINMESSA                                        
                                                                                
         ICM   R0,15,MCRTPPGS                                                   
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  INMPAGES,DUB                                                     
         ICM   R0,15,MCRTPIOS                                                   
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  INMEXCP,DUB                                                      
         ICM   R0,15,MCRTPTCB                                                   
         CVD   R0,DUB                                                           
         SRP   DUB,62,5                                                         
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  INMTIME,DUB                                                      
         ICM   R0,15,MCRTPCPU                                                   
         CVD   R0,DUB                                                           
         SRP   DUB,62,5                                                         
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  INMCPU,DUB                                                       
         XR    RF,RF                                                            
                                                                                
PUTOPS22 LA    R3,WORK                                                          
         USING WTOD,R3                                                          
         XC    WTOD(WTOPARML),WTOD                                              
                                                                                
         LA    R0,REPLY            Set A(reply)                                 
         ST    R0,WTOARPLY         Set A(operator response)                     
         MVI   WTOLRPLY,L'REPLY    Set L'operator response                      
         XC    ECBCOMS,ECBCOMS                                                  
         LA    R0,ECBCOMS                                                       
         ST    R0,WTOAECB          Set A(ECB)                                   
                                                                                
         LA    R2,WTOMESS          Build message                                
         LTR   RF,RF               Test prefix 'bad reply'                      
         JZ    *+20                                                             
         XC    WTOMESS(WTOMESSL),WTOMESS                                        
         MVC   0(L'BADREPLY,R2),BADREPLY                                        
         AHI   R2,L'BADREPLY                                                    
                                                                                
         MVC   0(INMESSL,R2),INMESSA                                            
         AHI   R2,INMESSL                                                       
                                                                                
         LA    R0,WTOMESS-4                                                     
         SR    R2,R0                                                            
         STCM  R2,3,WTOLMESS                                                    
         LA    R1,WTOLRPLY                                                      
         XC    REPLY,REPLY                                                      
         SVC   35                                                               
         L     R1,WTOAECB                                                       
         WAIT  ECB=(1)                                                          
                                                                                
         CLC   OPGO,REPLY                                                       
         JE    PUTOPSX                                                          
         CLI   REPLY+1,EQUAL                                                    
         JNE   PUTOPS30                                                         
         LA    R1,REPLY+L'REPLY-1                                               
         CLI   0(R1),SPACE                                                      
         JH    *+8                                                              
         JCT   R1,*-8                                                           
         LR    RF,R1                                                            
PUTOPS24 CLI   0(RF),EQUAL                                                      
         JE    PUTOPS26                                                         
         CLI   0(RF),C'0'                                                       
         JL    PUTOPS30            Less than 0 is error                         
         CLI   0(RF),C'9'                                                       
         JH    PUTOPS30                                                         
         JCT   RF,PUTOPS24                                                      
PUTOPS26 SR    R1,RF               Gives data length                            
         JNP   PUTOPS30                                                         
         BCTR  R1,0                                                             
         EX    R1,POPSPKDF                                                      
         CVB   R0,DUB                                                           
                                                                                
         CLC   REPLY(1),INMPAGEL                                                
         JNE   *+16                                                             
         LA    RE,MCRTPPGS                                                      
         LA    RF,MCNXTPGS                                                      
         J     PUTOPS28                                                         
                                                                                
         CLC   REPLY(1),INMEXCPL                                                
         JNE   *+16                                                             
         LA    RE,MCRTPIOS                                                      
         LA    RF,MCNXTIOS                                                      
         J     PUTOPS28                                                         
                                                                                
         CLC   REPLY(1),INMCPUL                                                 
         JNE   *+16                                                             
         MHI   R0,100              Convert to sec*100                           
         ST    R0,MCRTPCPU                                                      
         J     PUTOPS20                                                         
                                                                                
         CLC   REPLY(1),INMTIMEL                                                
         JNE   PUTOPS30                                                         
         LA    RE,MCRTPTCB                                                      
         LA    RF,MCNXTTCB                                                      
                                                                                
PUTOPS28 ST    R0,0(RE)                                                         
         LTR   R0,R0               Was entry zero                               
         JNZ   PUTOPS20                                                         
         ST    R0,0(RF)            Zero MCNXTxxx                                
         J     PUTOPS20                                                         
                                                                                
PUTOPS30 LA    RF,1                Set 'bad reply' and go again                 
         J     PUTOPS22                                                         
                                                                                
PUTOPSX  CLI   MCPOPSW,TIMEROPS    Did we suspend timer                         
         JNE   EXIT                no, leave flag                               
         GOTOR SETPOP              yes, restart it                              
         J     EXIT                                                             
                                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Format time for operator display                                    *         
***********************************************************************         
                                                                                
EDTTIM1  LR    RF,R1                                                            
         SRDL  R0,32               Convert sec*100 to sec                       
         D     R0,F100                                                          
         SR    R0,R0               Leaves seconds in R1                         
         J     EDTTIM3                                                          
                                                                                
EDTTIM2  LR    RF,R1                                                            
         SRDL  R0,32                                                            
                                                                                
EDTTIM3  D     R0,SECPERHR                                                      
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  0(2,RF),DUB                                                      
         MVI   2(RF),PERIOD                                                     
         SRDL  R0,32                                                            
         D     R0,SECPERMN         Gives minutes in R1/secs in R0               
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  3(2,RF),DUB                                                      
         MVI   5(RF),PERIOD                                                     
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  6(2,RF),DUB                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* TEST FOR OPERATOR CONSOLE COMMANDS AND MESSAGES                     *         
***********************************************************************         
                                                                                
TSTOPS   NTR1  LABEL=*,WORK=(RC,OPWORKL)                                        
         USING OPWORKD,RC                                                       
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         TM    AOPERECB,PROCMC     Exit now if processing console               
         JNZ   EXIT                                                             
         TM    ADSPCECB,PROCMC     or global command                            
         JNZ   EXIT                                                             
         TM    PAUSEFLG,PAUSEAQ    or PAUSE in progress                         
         JNZ   EXIT                                                             
                                                                                
*        Deal with PAUSE - operator dialogue between requests                   
                                                                                
TSTOPS10 TM    PAUSEFLG,PAUSEPQ    Is a PAUSE pending?                          
         JZ    TSTOPS20            no, skip                                     
         CLI   WORKING,YESQ        Can we pause?                                
         JE    TSTOPS20            Not if working on a request                  
         CLI   ENDRUNFL,C'Y'       Not if terminating                           
         JE    TSTOPS20                                                         
         OI    PAUSEFLG,PAUSEAQ    Set processing PAUSE                         
                                                                                
TSTOPS12 XC    OPWORK,OPWORK                                                    
         LA    R3,OPWORK                                                        
         USING WTOD,R3                                                          
         MVC   CARD,SPACES                                                      
         LA    R0,CARD             Set A(Reply)                                 
         ST    R0,WTOARPLY                                                      
         MVI   WTOLRPLY,L'CARD     Set L'operator response                      
         XC    ECBCOMS,ECBCOMS                                                  
         LA    R0,ECBCOMS                                                       
         ST    R0,WTOAECB          Set A(ECB)                                   
         MVC   WTOMESS(L'MCSTEP),MCSTEP                                         
         MVC   WTOMESS+L'MCSTEP(L'PAUSEMSG),PAUSEMSG                            
         LA    R1,WTOLRPLY         Point to length of reply                     
         LA    R0,L'MCSTEP+L'PAUSEMSG+4                                         
         STCM  R0,3,WTOLMESS                                                    
         SVC   35                  Issue WTOR                                   
         L     R1,WTOAECB          Wait for operator response                   
         WAIT  ECB=(1)                                                          
         CLC   CARD,SPACES                                                      
         JE    *+14                Spaces = EOB = GO                            
         CLC   OPGO,CARD                                                        
         JNE   TSTOPS40            not GO, so validate command                  
         MVI   PAUSEFLG,0          end PAUSE                                    
         DROP  R3                                                               
                                                                                
*        Deal with operator command                                             
                                                                                
TSTOPS20 L     RF,AOPERECB         Get A(operator ECB)                          
         TM    0(RF),ECBPOSTQ      Test message/command pending                 
         JZ    TSTOPS60                                                         
                                                                                
         L     R3,WCOMM            Point to operator comms block                
         LA    R3,COMCIBPT-COMLIST(R3)                                          
         L     R2,0(R3)                                                         
         USING CIBNEXT,R2                                                       
                                                                                
         CLI   CIBVERB,CIBSTOP     Test operator entered stop command           
         JE    TSTOPS46                                                         
                                                                                
TSTOPS22 CLI   CIBVERB,CIBMODFY    Did operator enter modify command            
         JE    *+6                                                              
         DC    H'0'                What the hell did he do then?                
                                                                                
         CLC   CIBDATLN,=H'1'      One character short forms                    
         JL    TSTOPS42            Null command is invalid                      
         JH    TSTOPS24                                                         
         CLI   CIBDATA,C'?'        ?=STATUS                                     
         JE    TSTOPS30                                                         
         CLI   CIBDATA,C'P'        P=PAUSE                                      
         JE    TSTOPS34                                                         
         J     TSTOPS36                                                         
TSTOPS24 CLC   CIBDATLN,=H'3'                                                   
         JNE   TSTOPS26                                                         
         CLC   CIBDATA(3),=C'EOJ'                                               
         JE    TSTOPS46                                                         
         J     TSTOPS36                                                         
TSTOPS26 CLC   CIBDATLN,=H'5'                                                   
         JNE   TSTOPS28                                                         
         CLC   CIBDATA(5),=C'PAUSE'                                             
         JE    TSTOPS34                                                         
         J     TSTOPS36                                                         
TSTOPS28 CLC   CIBDATLN,=H'6'                                                   
         JNE   TSTOPS36                                                         
         CLC   CIBDATA(6),=C'ENDRUN'                                            
         JE    TSTOPS46                                                         
         CLC   CIBDATA(6),=C'STATUS'                                            
         JE    TSTOPS30                                                         
         CLC   CIBDATA(6),=C'WAKEUP' LOOK FOR DSPACE COMMAND                    
         JNE   TSTOPS36                                                         
         OI    ECBDSP,ECBPOSTQ       POST to make search happen                 
         J     TSTOPS50                                                         
                                                                                
TSTOPS30 OI    AOPERECB,PROCMC     STATUS: Set processing command               
         CLI   WORKING,YESQ        Can't say much when waiting for work         
         JE    TSTOPS32                                                         
         MVC   OPWORK+L'MCSTEP(15),=C' RUNNER is idle'                          
         LA    R0,L'MCSTEP+15                                                   
         J     TSTOPS48                                                         
TSTOPS32 GOTOR PUTOPS,TQEOPER      Issue status, allow reply                    
         J     TSTOPS50                                                         
                                                                                
TSTOPS34 OI    AOPERECB,PROCMC     PAUSE: Set processing command                
         OI    PAUSEFLG,PAUSEPQ    Set PAUSE pending                            
         MVC   OPWORK+L'MCSTEP(15),=C' PAUSE accepted'                          
         LA    R0,L'MCSTEP+15                                                   
         J     TSTOPS48                                                         
                                                                                
TSTOPS36 CLI   WORKING,YESQ        Can't process during work request            
         JNE   TSTOPS38                                                         
         MVC   OPWORK+L'MCSTEP(L'BUSYMSG),BUSYMSG                               
         LA    R0,L'MCSTEP+L'BUSYMSG                                            
         J     TSTOPS48                                                         
                                                                                
TSTOPS38 OI    AOPERECB,PROCMC     Set processing message/command               
         SR    R1,R1                                                            
         ICM   R1,3,CIBDATLN       Modify with no data is invalid               
         JZ    TSTOPS42                                                         
         BCTR  R1,0                                                             
         MVC   CARD,SPACES                                                      
         EX    R1,TOPTMVCD                                                      
         CLC   CARD,SPACES         (spaces invalid too)                         
         JE    TSTOPS42                                                         
                                                                                
TSTOPS40 GOTOR VALPAR,CARD         Call VALPAR to process message               
         JE    TSTOPS44                                                         
TSTOPS42 MVC   OPWORK+L'MCSTEP(16),=C' Invalid command'                         
         LA    R0,L'MCSTEP+16                                                   
         J     TSTOPS48                                                         
TSTOPS44 CLI   ENDRUNFL,C'Y'                                                    
         JE    TSTOPS46                                                         
         MVC   OPWORK+L'MCSTEP(17),=C' Command accepted'                        
         LA    R0,L'MCSTEP+17                                                   
         J     TSTOPS48                                                         
                                                                                
TSTOPS46 MVI   ENDRUNFL,C'Y'                                                    
         MVC   OPWORK+L'MCSTEP(19),=C' Runner terminating'                      
         LA    R0,L'MCSTEP+19                                                   
         CLI   WORKING,YESQ                                                     
         JNE   TSTOPS48                                                         
         MVC   OPWORK+L'MCSTEP+19(18),=C' at end of request'                    
         AHI   R0,18                                                            
                                                                                
TSTOPS48 MVC   OPWORK(L'MCSTEP),MCSTEP                                          
         GOTOR DMGR,DMCB,DMOPMSG,((R0),OPWORK)                                  
                                                                                
TSTOPS50 QEDIT ORIGIN=(R3),BLOCK=(R2)                                           
         LAM   AR0,ARF,ARZERO                                                   
         NI    AOPERECB,FF-(PROCMC)                                             
                                                                                
         TM    PAUSEFLG,PAUSEAQ    Test processing a PAUSE                      
         JNZ   TSTOPS12            yes, get next command                        
         CLI   PAUSEFLG,0          is a pause pending?                          
         JNE   TSTOPS10            see if we can pause now                      
                                                                                
         DROP  R2                                                               
                                                                                
*        Process GLOBAL commands                                                
                                                                                
TSTOPS60 TM    ECBDSP,ECBPOSTQ     Test global command ECB posted               
         JZ    EXIT                                                             
         CLI   MCPOPSW,TIMERPOP    Test called by timer exit                    
         JE    EXIT                Can't use RUNNER's ALETs in exit             
         OI    ADSPCECB,PROCMC     Set processing global command                
         MVI   ECBDSP,0            Set ECB not posted                           
                                                                                
         MVI   OPNACT,0            Default to no action                         
                                                                                
         LARL  RF,SSB                                                           
         LAM   AR2,AR2,SSOALET-SSOOFF(RF)                                       
         SR    R2,R2                                                            
         USING DMDHDR,R2                                                        
         SAC   512                                                              
         L     R2,DHACOMM          Set R2 to comms block                        
         AHI   R2,4096             Skip headers                                 
         LHI   R0,512              Up to 512 commands                           
                                                                                
         USING DSCOMM,R2                                                        
TSTOPS62 TM    DSCFLAG,DSCDONEQ    Ignore processed                             
         JO    TSTOPS86                                                         
         TM    DSCFLAG,DSCJNUMQ    Ignore if not for a job                      
         JNO   TSTOPS86                                                         
         CLC   ASID,DSCDEST        Ignore if not for me                         
         JNE   TSTOPS86                                                         
                                                                                
         ST    R2,THISCOMM         A(Command)                                   
         MVC   THISTIME,0(R2)      Time of command                              
         MVC   THISJOB,DSCJNUM     Job number that submitted it                 
                                                                                
         LARL  R3,SEOLST           Scan system list for this system             
         USING SEOLSTD,R3                                                       
         OC    DSCDATA(2),DSCDATA  No system required                           
         JZ    TSTOPS66                                                         
TSTOPS64 CLI   SEOLNUM,SEOLEOTQ    Test end of list                             
         JE    TSTOPS84                                                         
         CLC   SEOLNUM,DSCDATA+1   Test this system is defined here             
         JNE   TSTOPS82                                                         
                                                                                
TSTOPS66 CLI   WORKING,YESQ        Real work to do so must be idle              
         JNE   TSTOPS70                                                         
         OI    ECBDSP,ECBPOSTQ     Not idle so re-post ECB                      
         J     TSTOPS86                                                         
                                                                                
TSTOPS70 CLC   =X'0004',DSCCOMM    Action 4 start                               
         JNE   *+16                                                             
         MVI   OPNACT,OPNAOPN      Set action to open files                     
         TM    SEOLFLAG,SEOLFOPN   Test system is open                          
         JNZ   TSTOPS84                                                         
                                                                                
         CLC   =X'000A',DSCCOMM    Action A Read                                
         JNE   *+16                                                             
         MVI   OPNACT,OPNAOPN      Set action to open files                     
         TM    SEOLFLAG,SEOLFOPN   Test system is open                          
         JNZ   TSTOPS84                                                         
                                                                                
         CLC   =X'0005',DSCCOMM    Action 5 stop                                
         JNE   *+16                                                             
         MVI   OPNACT,OPNACLO      Set action to close files                    
         TM    SEOLFLAG,SEOLFOPN   Test system is open                          
         JZ    TSTOPS84                                                         
                                                                                
TSTOPS72 CLC   =X'001D',DSCCOMM    Action 29 RQ Stop                            
         JNE   TSTOPS74                                                         
         SAC   0                                                                
         LA    R0,128+2            STOP ACTION IN P1                            
         LLC   RF,DSCDATA+1        SE# TO START IN P2                           
         GOTO1 =V(DMRCVUSS),DMCB,(R0),(RF)                                      
         SAC   512                                                              
         J     TSTOPS84                                                         
                                                                                
TSTOPS74 CLC   =X'001E',DSCCOMM    Action 30 RQ Go                              
         JNE   TSTOPS76                                                         
         SAC   0                                                                
         LA    R0,128+1            START ACTION IN P1                           
         LLC   RF,DSCDATA+1        SE# TO START IN P2                           
         GOTO1 =V(DMRCVUSS),DMCB,(R0),(RF)                                      
         SAC   512                                                              
         J     TSTOPS84                                                         
                                                                                
TSTOPS76 CLC   =X'001F',DSCCOMM    Action 31 RQ Stat                            
         JNE   TSTOPS78                                                         
         SAC   0                                                                
         LA    R0,128+3            STAT ACTION IN P1                            
         LLC   RF,DSCDATA+1        SE# TO START IN P2                           
         GOTO1 =V(DMRCVUSS),DMCB,(R0),(RF)                                      
         SAC   512                                                              
         J     TSTOPS84                                                         
                                                                                
TSTOPS78 CLC   =X'0020',DSCCOMM    Action 32 endrun                             
         JNE   TSTOPS80                                                         
         MVI   ENDRUNFL,C'Y'                                                    
         J     TSTOPS84                                                         
                                                                                
TSTOPS80 CLI   OPNACT,0            No action set so forget it                   
         JE    TSTOPS84                                                         
         SAC   0                                                                
         GOTOR ROPNCLO,SEOLSTD     Open/close this system                       
         SAC   512                                                              
         J     TSTOPS84            Set done                                     
                                                                                
TSTOPS82 AHI   R3,SEOLSTL          Bump to next system list entry               
         J     TSTOPS64                                                         
         DROP  R3                                                               
                                                                                
TSTOPS84 LARL  RF,SSB                                                           
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,SSOALET-SSOOFF(RF)                                       
         SAC   512                                                              
         OI    DSCFLAG,DSCDONEQ    Flag it as done                              
         XC    THISBLK,THISBLK     Clear saved command info                     
         CLC   =X'FFFE',DSCSORC    Was it DDOPER                                
         JE    TSTOPS86                                                         
         XC    0(L'DSCOMMS,R2),0(R2)                                            
                                                                                
TSTOPS86 AHI   R2,L'DSCOMMS        Point to next comms entry                    
         JCT   R0,TSTOPS62         Next command                                 
                                                                                
         SAC   0                                                                
                                                                                
         LAM   AR0,ARF,ARZERO                                                   
         NI    ADSPCECB,FF-(PROCMC)                                             
         J     EXIT                                                             
         DROP  R2,RC                                                            
                                                                                
OPWORKD  DSECT ,                   ** TSTOPS local w/s **                       
OPWORK   DS    CL(L'WORK)                                                       
OPWORKL  EQU   *-OPWORKD                                                        
RUNNER   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Test for work to be canceled and if so clear the current work       *         
* queue entry, purge the worker file(s) then look for more work to do *         
***********************************************************************         
                                                                                
TSTCANAW NTR1  LABEL=*             Entry from ADWAIT                            
         GOTOR SETREG              Set RUNNER registers                         
         MVI   ADWCAN,C'Y'         Called from ADWAIT                           
         J     TSTCANC                                                          
                                                                                
TSTCAN   NTR1  LABEL=*                                                          
         GOTOR SETREG              Set RUNNER registers                         
         MVI   ADWCAN,C'N'         Not called from ADWAIT                       
                                                                                
TSTCANC  TM    RUNINDS1,RUNIBTCH   Exit if running in batch mode                
         JNZ   EXIT                                                             
         OC    AQUE,AQUE           Are we running work now?                     
         JZ    EXIT                                                             
         CLI   ERROR,TQEGOOD       Test processing error                        
         JNE   TSTCANN                                                          
         CLI   CANCEL,0            Test processing cancel                       
         JNE   TSTCANN                                                          
                                                                                
         LAM   AR3,AR3,ALET                                                     
         ICM   R3,15,AQUE                                                       
         SAC   512                                                              
         USING TABSQUED,R3         R3=A(work queue entry)                       
         CLI   TQACTION,TQACANC    Test cancel current work                     
         JNE   TSTCANN                                                          
         SAC   0                                                                
         LAM   AR3,AR3,ARZERO                                                   
                                                                                
***********************************************************************         
* Purge the worker file(s).  For segmented files the worker files are *         
* purged in reverse order.  Record 1 of each segmented file contains  *         
* a pointer element (LQ_FSEGQ) - the key of the previous file is set  *         
* and when it is read the forward pointer is checked to ensure it     *         
* points to one last purged - if not no further files are read (this  *         
* is not treated as an error as the previous file may have been sent  *         
* and purged (or reused) already)                                     *         
***********************************************************************         
                                                                                
         L     R4,RWRKBLK          Point to current WRKIO block                 
         USING WRKIOD,R4                                                        
         TM    WRKISTAT,WRKISOPN   Test worker file is open                     
         JZ    TSTCANN                                                          
*&&UK                                                                           
         GOTOR VDMENQDQ,PARM,(C'T',WRKIFILE)                                    
         TM    4(R1),X'01'         Test WKFILE enqueued                         
         JNZ   TSTCANN             Yes - wait until dequeued                    
*&&                                                                             
         LARL  RF,SSB                                                           
         TM    SSOFLAG3-SSOOFF(RF),SSO3NINT  Test WRKF being executed           
         JO    TSTCANN                       Yes - wait until completed         
*                                                                               
         MVI   CANCEL,1            Set processing cancel                        
         MVI   WRKIACTN,WRKIACLO   Close the current file                       
         GOTOR RWRKIO,WRKIOD                                                    
         JNE   TSTCANY                                                          
         XC    FSEGS,FSEGS         Clear segment pointers                       
                                                                                
TSTCAN02 MVI   WRKIACTN,WRKIAOPN   Open first or previous segment               
         MVC   WORK(L'WRKWKEY),WRKWKEY                                          
         GOTOR RWRKIO,WRKIOD                                                    
         JNE   TSTCANY                                                          
         CLC   WRKWKEY,WORK        Ensure we have correct file                  
         JNE   TSTCANY                                                          
         L     RE,WRKIAREC                                                      
         XC    0(256,RE),0(RE)                                                  
         GOTOR (RF),(R1)           Read first record                            
         JNE   TSTCANY                                                          
                                                                                
         L     R1,WRKIAREC         Look for segment pointer element             
         AHI   R1,4                                                             
         USING LQ_D,R1                                                          
         SR    R0,R0                                                            
TSTCAN04 CLI   LQ_EL,0             Not there means not asynchronous             
         JE    TSTCAN06                                                         
         CLI   LQ_EL,LQ_FSEGQ                                                   
         JE    *+14                                                             
         ICM   R0,3,LQ_LN                                                       
         AR    R1,R0                                                            
         J     TSTCAN04                                                         
                                                                                
         OC    FPREV,FPREV         Test previous key set                        
         JZ    *+14                                                             
         CLC   LQ_FNEXT,FPREV      Yes - does current point to it               
         JNE   TSTCANY                                                          
         MVC   FNEXT,LQ_FPREV      Set key of next segment                      
                                                                                
TSTCAN06 GOTOR PRTKEY,WRKIOD       Format & print purge log message             
         MVC   PMESSAGE(L'CANLIT),CANLIT                                        
         MVC   PMESSAGE+L'CANLIT(PRTKEYLQ),WORK                                 
         GOTOR PRTLOG                                                           
                                                                                
         GOTOR DMGR,WRKIPARM,WKPURGE                                            
         OC    FNEXT,FNEXT         Test next segment key set                    
         JZ    TSTCANY                                                          
         MVC   FPREV,WRKWKEY       Yes - save current as previous               
         MVC   WRKWKEY,FNEXT       Set next to be inspected                     
         XC    FNEXT,FNEXT         Clear next segment pointer                   
         J     TSTCAN02                                                         
                                                                                
TSTCANY  LAM   AR3,AR3,ALET                                                     
         ICM   R3,15,AQUE                                                       
         SAC   512                                                              
         MVI   TQACTION,0          Reset cancel action in queue                 
         SAC   0                                                                
         LAM   AR3,AR3,ARZERO                                                   
         MVI   ERROR,TQECANC       Set error for cancel                         
         MVI   CANCEL,0            Reset cancel flag                            
                                                                                
         GOTOR RSORTER,DMCB,SORTEND                                             
         CLI   ADWCAN,C'Y'         Test called from ADWAIT                      
         JNE   TSTCANYX            No, CPUREQ already set                       
         GOTOR GETCPU                                                           
         LG    GR1,CPUSAVE                                                      
         SLG   GR1,CPUREQ                                                       
         STG   GR1,CPUREQ          Set CPU time used by DDLINK & server         
                                                                                
TSTCANYX L     RD,SAVERD           Restore RUNNER RD value                      
         LM    RE,RC,12(RD)        Restore RUNNER registers                     
         J     ENDREQ              Print summary and find new work              
                                                                                
TSTCANN  SAC   0                   No cancel exit                               
         LAM   AR3,AR3,ARZERO                                                   
         J     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* Look up completion message in TQETAB                                *         
***********************************************************************         
                                                                                
GETTQE   LARL  RF,TQETAB           Point to message table                       
         CLI   0(R1),TQEGOOD       Good return is at front of table             
         BER   RE                                                               
GETTQE02 AHI   RF,L'TQETAB         Bump to next table entry                     
         CLI   0(RF),0             Look up error text in table                  
         BER   RE                                                               
         CLC   0(1,RF),0(R1)       Match error number                           
         BER   RE                                                               
         J     GETTQE02                                                         
         EJECT                                                                  
***********************************************************************         
* Format work file key for printing                                   *         
***********************************************************************         
                                                                                
         USING WRKIOD,R1                                                        
PRTKEY   STM   RE,R0,12(RD)                                                     
         MVC   WORK(PRTKEYLQ),SPACES                                            
         CLI   WRKIFTYP,WRKIFTWF   Test worker file                             
         JNE   PRTKEY02                                                         
         LA    RF,WORK                                                          
         MVC   0(L'UIDCODE,RF),UIDCODE                                          
         CLI   0(RF),SPACE                                                      
         JE    *+12                                                             
         AHI   RF,1                                                             
         J     *-12                                                             
         MVI   0(RF),COMMA                                                      
         UNPK  DUB(3),WRKWDAY(2)                                                
         MVC   1(L'WRKWSPS,RF),WRKWSPS                                          
         MVC   1+L'WRKWSPS(2,RF),DUB                                            
         MVC   1+L'WRKWSPS+2(L'WRKWCLS,RF),WRKWCLS                              
         AHI   RF,1+L'WRKWSPS+2+L'WRKWCLS                                       
         SR    R0,R0                                                            
         ICM   R0,3,WRKWSEQN                                                    
         JZ    PRTKEYX                                                          
         MVI   0(RF),COMMA                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  1(5,RF),DUB                                                      
         CLI   1(RF),C'0'                                                       
         JNE   PRTKEYX                                                          
         MVC   1(5,RF),2(RF)                                                    
         J     *-14                                                             
                                                                                
PRTKEY02 DC    H'0'                                                             
                                                                                
PRTKEYX  OC    WORK(PRTKEYLQ),SPACES                                            
         LM    RE,R0,12(RD)                                                     
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Edit timer units into printable format                              *         
***********************************************************************         
                                                                                
EDTTUS   STM   RE,R1,12(RD)                                                     
         LR    RF,R1               RF=A(output)                                 
         SRDL  R0,32               R0/F1=tus                                    
         D     R0,TUSSEC           R0=seconds,F1=tus                            
         ST    R1,DUB              Save seconds                                 
         MHI   R0,1000                                                          
         SRDL  R0,32                                                            
         D     R0,TUSSEC           R1=milliseconds                              
         L     R0,DUB                                                           
         CVD   R1,DUB              Edit milliseconds                            
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  9(3,RF),DUB                                                      
         SRDL  R0,32               Restore seconds                              
         LHI   RE,SECSMIN                                                       
         DR    R0,RE                                                            
         CVD   R0,DUB              Edit seconds                                 
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  6(2,RF),DUB                                                      
         SR    R0,R0                                                            
         LHI   RE,MINHOUR                                                       
         DR    R0,RE                                                            
         CVD   R0,DUB              Edit minutes                                 
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  3(2,RF),DUB                                                      
         CVD   R1,DUB              Edit hours                                   
         OI    DUB+L'DUB-1,DIGIT                                                
         UNPK  0(2,RF),DUB                                                      
EDTTUSX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Load server phase and initialize                                    *         
***********************************************************************         
                                                                                
SETSVR   NTR1  LABEL=*                                                          
         LR    R0,R1               Save A(phase name)                           
         GOTOR RGPHS,PARM,0,('LOADQ',(R0))                                      
         JNE   EXITN                                                            
                                                                                
         ICM   RF,15,0(R1)         Initialize server values                     
         USING RSVRDEFD,RF                                                      
         CLC   RSVREYE,SVREYE      Match server eye-catcher                     
         JE    SETSVR02                                                         
         TM    RSVRIND1,RSVRIDFC   Test IDF compatible                          
         JZ    EXITN                                                            
         CLC   RSVREYE+8(L'RSVREYE-8),SVREYE                                    
         JNE   EXITN                                                            
                                                                                
SETSVR02 ST    RF,RASVR            Save A(server) for application               
         LLH   R1,RSVRADDR                                                      
         AR    R1,RF                                                            
         ST    R1,SVRADDR          Set A(entry point)                           
         SR    R1,R1                                                            
         ICM   R1,3,RSVRFIL1                                                    
         JZ    *+10                                                             
         AR    R1,RF                                                            
         STCM  R1,7,SVRFILE1       Set A(system/file 1 info)                    
         SR    R1,R1                                                            
         ICM   R1,3,RSVRFIL2                                                    
         JZ    *+10                                                             
         AR    R1,RF                                                            
         STCM  R1,7,SVRFILE2       Set A(system/file 2 info)                    
         SR    R1,R1                                                            
         ICM   R1,3,RSVRFACS                                                    
         JZ    *+10                                                             
         AR    R1,RF                                                            
         ST    R1,SVRFACS          Set A(facilities list)                       
         SR    R1,R1                                                            
         ICM   R1,3,RSVRNOTE                                                    
         JZ    *+10                                                             
         AR    R1,RF                                                            
         ST    R1,SVRABND          Set A(abend notification list)               
         SR    R1,R1                                                            
         ICM   R1,3,RSVRSLOW                                                    
         JZ    *+10                                                             
         AR    R1,RF                                                            
         ST    R1,SVRSLOW          Set A(slow notification list)                
                                                                                
         MVC   SVRSYSN1,RSVRSYS1   Extract native system 1 number               
         OC    SVRFILE2,SVRFILE2   Test system/files 2 set                      
         JZ    *+10                                                             
         MVC   SVRSYSN2,RSVRSYS2   Extract native system 2 number               
         MVC   LP_SVRTY,RSVRTYPE   Set server type                              
         MVC   LP_MQMST,RSVRTYPE   Set server type                              
         MVC   SVRSYSP,RSVRSYSP    Set server sysphase                          
         MVC   SVRFTYP,RSVRFTYP    Set server file type                         
         MVC   SVRFOPA,RSVRFOPA    Set server file open type                    
         MVC   SVRINDS,RSVRIND1    Set server indicators                        
         MVC   MCSYSTEM,RSVRSYSC   Set MCSYSTEM value                           
         MVC   MCPROG,RSVRPRGC     Set MCPROG value                             
         DROP  RF                                                               
                                                                                
         ICM   R3,15,SVRFACS       Build COMFACS addresses                      
         JZ    SETSVR08                                                         
         MVC   PMESSAGE(L'INFLIT3),INFLIT3                                      
         GOTOR PRTLOG                                                           
         USING RFACTABD,R3                                                      
         SR    R0,R0                                                            
         ICM   R0,B'1110',T00A                                                  
SETSVR04 ICM   R0,1,RFACPHSN       Set/test phase number                        
         JZ    SETSVR08            Zero is end of table                         
         GOTOR RGPHS,DMCB,(X'80',0),(R0),0,RFACCOMD                             
SETSVR06 AHI   R3,RFACTABL         Bump to next table entry                     
         J     SETSVR04                                                         
         DROP  R3                                                               
                                                                                
SETSVR08 TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   SETSVR90                                                         
         CLI   SVRSYSN1,CONSYSQ    Test control system server                   
         JE    SETSVR90                                                         
                                                                                
         LARL  R2,SEOLST           Build a list of open systems                 
         USING SEOLSTD,R2                                                       
         LA    R4,SVRSYS           Point to list of systems                     
         LHI   R0,1                                                             
         CLI   SVRSYSN2,0          Test second system defined                   
         JE    SETSVR09                                                         
         LHI   R0,2                                                             
                                                                                
SETSVR09 TM    RUNINDS2,RUNISTAT   USE DDSTATE SETTINGS                         
         JO    SETSVR20                                                         
                                                                                
SETSVR10 L     R5,AIO              Read system list record                      
         USING CTWREC,R5           Build key of system list record              
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         MVC   CTWKSYSN,0(R4)      Set native system number                     
         L     RF,MCUTL            Set control system files                     
         MVI   4(RF),CONSYSQ                                                    
         GOTOR DMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                            
         JE    SETSVR12                                                         
         MVC   PMESSAGE(L'BADLIT1),BADLIT1                                      
         GOTOR PRTLOG                                                           
         J     EXITN                                                            
                                                                                
SETSVR12 L     R5,AIO                                                           
         LA    R5,CTWDATA          Open server files in all systems             
         USING CTLSTD,R5                                                        
SETSVR14 CLI   CTLSTD,0            Test end of list record                      
         JE    SETSVR18                                                         
         CLI   CTLSTEL,CTLSTELQ    Test list element                            
         JNE   SETSVR16                                                         
         CLC   CTLSTSYS,0(R4)      For the server system                        
         JNE   SETSVR16                                                         
                                                                                
         MVC   SEOLNUM,CTLSTSEN    Build system open list entry                 
         MVC   SEOLNAM,CTLSTNAM                                                 
         CLC   SVRFILE1,L'SVRSYSN1(R4)   Match on FILE1                         
         JE    SETSVR15                  ok                                     
         CLC   SVRFILE2,L'SVRSYSN1(R4)   Match on FILE2                         
         JNE   *+12                                                             
         OI    SEOLFLAG,SEOLFIL2   Set FILE2 set in SE list                     
         J     SETSVR15                                                         
         OI    SEOLFLAG,SEOLFILX   Else set SEOLFILX flag                       
                                                                                
SETSVR15 AHI   R2,SEOLSTL                                                       
                                                                                
SETSVR16 LLC   RF,CTLSTLEN         Bump to next list element                    
         AR    R5,RF                                                            
         J     SETSVR14                                                         
         DROP  R5                                                               
                                                                                
SETSVR18 AHI   R4,L'SVRSYS         Bump to next system                          
         JCT   R0,SETSVR10         Do for number of systems                     
         J     SETSVR30                                                         
                                                                                
SETSVR20 EQU   *                                                                
         GOTOR SETDSP              Set DMGR dspace (if not already set)         
                                                                                
         GOTO1 =V(DDSTATE),DMCB,=C'INIT',0                                      
         LARL  RF,SSB                                                           
         LAM   AR2,AR2,SSOALET-SSOOFF(RF)                                       
         LAM   AR3,AR3,SSOALET-SSOOFF(RF)                                       
         L     R3,DMCB+4           RETURN A(LOCAL STATE)                        
         L     R5,DMCB+8           RETURN A(DEFAULT OPEN STATE)                 
                                                                                
         SAC   512                                                              
         USING DSLSTATE,R3                                                      
         LA    R3,DSLSYS           READ LOCAL TABLE                             
         LA    R0,256                                                           
         LA    R1,IO               SAVE CURRENT SYS STATES INTO IO              
SETSVR21 MVC   0(1,R1),0(R3)                                                    
         MVC   1(1,R1),0(R5)                                                    
         LA    R1,2(R1)                                                         
         LA    R3,2(R3)                                                         
         LA    R5,2(R5)                                                         
         JCT   R0,SETSVR21                                                      
         SAC   0                                                                
         DROP  R2                                                               
*                                                                               
SETSVR22 LAM   AR3,AR3,ARZERO                                                   
         LARL  R5,SEOLST           BUILD A LIST OF OPEN SYSTEMS                 
         USING SEOLSTD,R5                                                       
         LA    R0,256              LOOP THROUGH IO                              
         LA    R3,IO                                                            
SETSVR23 CLI   1(R3),0             IGNORE ZERO                                  
         JE    SETSVR24                                                         
         CLI   1(R3),C'C'          IGNORE CLOSED                                
         JE    SETSVR24                                                         
         LR    RE,R3               CALCULATE OFFSET TO DSPACE                   
         LA    RF,IO                                                            
         SR    RE,RF                                                            
         SLL   RE,5                                                             
         LR    R2,RE                                                            
         SAC   512                                                              
*                                                                               
         ICM   R2,7,61(R2)         POINT TO SYSTEM HEADER                       
         JZ    SETSVR24                                                         
*                                                                               
         USING DMSYSHDR,R2                                                      
         MVC   SEOLNUM,DSYSENUM+1  BUILD SYSTEM OPEN LIST ENTRY                 
         MVC   SEOLNAM,DSYSID+5                                                 
*                                                                               
         CLI   1(R3),C'W'          IF WRITE                                     
         JNE   *+8                                                              
         OI    SEOLFLAG,SEOLFGLB   OPEN GLOBAL                                  
*                                                                               
         CLI   0(R3),C'C'          IS SYSTEM ACTUALLY CLOSED                    
         JNE   *+8                                                              
         OI    SEOLFLAG,SEOLFNOP   SET THIS SYSTEM IS NOOP                      
*                                                                               
         CLC   DSYOVSYS,0(R4)      SYSN1                                        
         JE    SETSV23A                                                         
         CLC   DSYOVSYS,SVRSYSN2   SYSN2                                        
         JNE   *+12                                                             
         OI    SEOLFLAG,SEOLFIL2   FILE SET 2                                   
         J     SETSV23A                                                         
         OI    SEOLFLAG,SEOLFILX   SET SEOLFILX IF UNKNOWN                      
*                                                                               
SETSV23A AHI   R5,SEOLSTL                                                       
         SAC   0                                                                
*                                                                               
SETSVR24 LA    R3,2(R3)                                                         
         JCT   R0,SETSVR23                                                      
*&&DO                                                                           
         L     R3,=V(SELIST)       SCAN THE SE LIST                             
         USING SELISTD,R3                                                       
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
SETSVR25 SR    R1,R1                                                            
         IC    R1,SESYS                                                         
         SLL   R1,1                                                             
         LA    R1,IO(R1)                                                        
         CLI   1(R1),0             IGNORE IF NO ENTRY                           
         JE    SETSVR29                                                         
         CLI   1(R1),C'C'          IGNORE IF CLOSED                             
         JE    SETSVR29                                                         
         MVC   SEOLNUM,SESYS       BUILD SYSTEM OPEN LIST ENTRY                 
         MVC   SEOLNAM,SENAME                                                   
         CLI   1(R1),C'W'          IF WRITE                                     
         JNE   *+8                                                              
         OI    SEOLFLAG,SEOLFGLB   OPEN GLOBAL                                  
*                                                                               
         CLI   0(R1),C'C'          IS SYSTEM ACTUALLY CLOSED                    
         JNE   *+8                                                              
         OI    SEOLFLAG,SEOLFNOP   SET THIS SYSTEM IS NOOP                      
*                                                                               
         CLC   SEOVSYS,0(R4)                                                    
         JE    *+8                                                              
         OI    SEOLFLAG,SEOLFIL2   FILE SET 2                                   
         AHI   R2,SEOLSTL                                                       
                                                                                
SETSVR29 JXLE  R3,RE,SETSVR25                                                   
*&&                                                                             
         DROP  R5                                                               
                                                                                
SETSVR30 LARL  RE,SEOLST                                                        
         CLI   0(RE),0             Test any systems were found                  
         JNE   SETSVR90                                                         
         MVC   PMESSAGE(L'BADLIT2),BADLIT2                                      
         GOTOR PRTLOG                                                           
         J     EXITN               No - not much we can do                      
                                                                                
SETSVR90 TM    SVRIND1,RSVRILNK    Test runs under DDLINK control               
         JZ    SETSVRX                                                          
         OC    VDDLINK,VDDLINK     Yes - test DDLINK already resolved           
         JNZ   SETSVRX                                                          
         LHI   RF,QDDLINK          No - load it now                             
         ICM   RF,B'1110',T00A                                                  
         GOTOR RGPHS,DMCB,0,(RF)                                                
         OC    VDDLINK,0(R1)                                                    
         JNZ   SETSVRX                                                          
         DC    H'0'                                                             
                                                                                
SETSVRX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Initialize DMGR dspace value in SSB if not already set              *         
***********************************************************************         
                                                                                
SETDSP   NTR1  LABEL=*                                                          
                                                                                
         LARL  R2,SSB                                                           
         CLI   SSODSPAC-SSOOFF(R2),C' '                                         
         JH    SETDSPX                                                          
         GOTOR VLOCKSPC,DMCB,(X'20',DTCTBL),0                                   
         LAM   AR0,ARF,ARZERO                                                   
         CLI   SSODSPAC-SSOOFF(R2),C' '                                         
         JH    SETDSPX                                                          
         DC    H'0'                                                             
                                                                                
SETDSPX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Update TQSTATUS                                                     *         
*                                                                     *         
* Ntry:- R3=A(Work queue entry)                                       *         
*        R1=A(Current/new status values)                              *         
***********************************************************************         
                                                                                
         USING TABSQUED,R3                                                      
UPDQUE   NTR1  LABEL=*                                                          
                                                                                
         ST    R1,FULL1            Save parameter address                       
         STAM  AR3,AR3,FULL2       Save queue pointer                           
         SAC   0                                                                
         LAM   AR3,AR3,ARZERO                                                   
         TIME  TU                                                               
         ST    R0,FULL3            Save current time                            
                                                                                
         GOTOR VLOCKSPC,DMCB,DTRUNITL,0                                         
         LAM   AR0,ARF,ARZERO                                                   
                                                                                
         L     R1,FULL1            Update the queue entry                       
         LAM   AR3,AR3,FULL2                                                    
         SAC   512                                                              
                                                                                
         CLI   0(R1),0             Test if matching current status              
         JE    *+14                                                             
         CLC   TQSTATUS,0(R1)      Yes - match status                           
         JNE   UPDQUE02                                                         
                                                                                
         CLI   1(R1),TQSFREE       Test free queue entry (MQ output)            
         JNE   *+14                                                             
         XC    TABSQUED(TABSQUEL),TABSQUED                                      
         J     UPDQUE02                                                         
                                                                                
         MVC   TQSTATUS,1(R1)      Set new work queue status                    
                                                                                
         CLI   1(R1),TQSDONE       Test work completed                          
         JNE   UPDQUE02                                                         
         MVC   TQATIME,FULL3       Yes - set completion time as now             
                                                                                
UPDQUE02 SAC   0                                                                
         LAM   AR3,AR3,ARZERO                                                   
                                                                                
         GOTOR VLOCKSPC,DMCB,(X'10',DTRUNITL),0                                 
         LAM   AR0,ARF,ARZERO                                                   
                                                                                
         LAM   AR3,AR3,FULL2                                                    
         SAC   512                                                              
                                                                                
UPDQUEX  J     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Check work queue for entries which need to be reported/purged -     *         
* send email if necessary                                             *         
***********************************************************************         
                                                                                
QMAINT   NTR1  LABEL=*                                                          
                                                                                
         LAM   AR0,ARF,ARZERO      Lock the dataspace                           
         SAC   0                                                                
*                                                                               
         TIME  TU                                                               
         CL    R0,TUS120           Are we in fist 2 hours of day                
         JL    EXITY               Then ignore this to avoid anomalies          
*                                                                               
         GOTOR VLOCKSPC,DMCB,DTRUNITL,0                                         
         LAM   AR0,ARF,ARZERO                                                   
                                                                                
         TIME  TU                                                               
         ST    R0,FULL1            Current time                                 
         SL    R0,TUS20M                                                        
         ST    R0,FULL2            Purge time for completed reports             
         SL    R0,TUS40M                                                        
         ST    R0,FULL3            Reporting time (over 60 mins)                
         SL    R0,TUS30M                                                        
         ST    R0,FULL4            Auto purge time (over 90 mins)               
                                                                                
         LAM   AR3,AR3,ALET                                                     
         ICM   R3,15,OFFSET                                                     
         SAC   512                                                              
         ICM   R3,15,TABSRUN-FATABSD(R3)                                        
         SR    R0,R0                                                            
         ICM   R0,3,TABSNQUE-TABSRUND(R3)                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,TABSAQUE-TABSRUND(R3)                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING TABSQUED,R3         R3=A(work queue)                             
         L     R1,AQMBUFF          R1=A(list of entries to report upon)         
                                                                                
QMAINT02 CLI   TQSTATUS,TQSFREE    Test free entry                              
         JE    QMAINT10            Yes - ignore                                 
         CLI   TQQFLAG,TQQSNOOP    Test system not open                         
         JE    QMAINT10            Yes - ignore                                 
                                                                                
         CLC   TQATIME,FULL1       Test arrival time greater than now           
         JNH   *+12                                                             
         MVI   BYTE1,FF            Yes - remove it immediately                  
         J     QMAINT08                                                         
                                                                                
         CLI   TQSTATUS,TQSDONE    Test work completed                          
         JNE   QMAINT04                                                         
         CLC   TQATIME,FULL2       Test in completed purge period               
         JH    QMAINT10            No - ignore                                  
         MVI   BYTE1,FE            Set to free this entry                       
         J     QMAINT08                                                         
                                                                                
QMAINT04 CLC   TQATIME,FULL3       Test in report period                        
         JH    QMAINT10            No - ignore                                  
         CLI   TQQFLAG,TQQFNONE    Test not reported yet                        
         JNE   QMAINT06                                                         
         MVI   BYTE1,TQQFREPT      Yes - report it now                          
         J     QMAINT08                                                         
                                                                                
QMAINT06 CLI   TQQFLAG,TQQFREPT    Test reported previously                     
         JNE   QMAINT10                                                         
         MVI   BYTE1,TQQFREPT      No - set status to reported                  
         CLC   TQATIME,FULL4       Test in purge period                         
         JH    QMAINT10            No - ignore                                  
         MVI   BYTE1,FD            Set to free this entry                       
                                                                                
QMAINT08 MVC   TQQFLAG,BYTE1       Set QMAINT action                            
         MVC   0(TABSQUEL,R1),TABSQUED                                          
         AHI   R1,TABSQUEL                                                      
         CLI   BYTE1,FD            Test purgeable entry                         
         JL    QMAINT10                                                         
         XC    TABSQUED(TABSQUEL),TABSQUED                                      
                                                                                
QMAINT10 AH    R3,QUEWIDTH         Bump to next queue entry                     
         JCT   R0,QMAINT02                                                      
         MVI   0(R1),FF            Terminate the report table                   
                                                                                
         LAM   AR0,ARF,ARZERO      Free the dataspace                           
         SAC   0                                                                
         GOTOR VLOCKSPC,DMCB,(X'10',DTRUNITL),0                                 
         LAM   AR0,ARF,ARZERO                                                   
                                                                                
         L     R3,AQMBUFF                                                       
         CLI   TABSQUED,FF         Test anything to report                      
         JE    EXITY                                                            
         CLI   QMAINTR,NOQ         Test qmaint report required                  
         JE    EXITY                                                            
                                                                                
         MVC   QMSUBJN,MCSTEP      Set step name                                
         GOTOR VSMTP,DMCB,('SMTPCNCL',0)                                        
         GOTOR (RF),(R1),('SMTPAPRS',QMLIST),('QMSUBJL',QMSUBJ)                 
         MVC   P,SPACES                                                         
         MVC   P(L'QMHEAD),QMHEAD                                               
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
                                                                                
QMAINT12 MVC   P,SPACES                                                         
         L     R2,RWRKBLK1                                                      
         USING WRKIOD,R2           Format worker file key                       
         MVC   WRKWKEY,TQWKEY                                                   
         CLC   TQMQZERO,ARZERO                                                  
         JNE   QMAINT14                                                         
         MVC   WORK(2),=C'M='                                                   
         GOTOR MCVHEXOU,DMCB,WRKWKEY,WORK+2,L'TQMQCTRL,0                        
         J     QMAINT16                                                         
                                                                                
QMAINT14 MVI   WRKIFTYP,WRKIFTWF                                                
         SR    RF,RF                                                            
         GOTOR GETUID,WRKWUSID     Resolve user-id for PRTKEY routine           
         GOTOR PRTKEY,WRKIOD                                                    
         DROP  R2                                                               
                                                                                
QMAINT16 MVC   P,SPACES            Format report line for sending               
         MVC   PQMFACID,TQLFACID   FACPAK                                       
         MVC   PQMTSKID,TQLTSKID   Task                                         
         MVC   PQMTYPE,TQTYPE      Server type                                  
         MVC   PQMSTAT,TQSTATUS    Work status                                  
         MVC   PQMFILE,WORK        Worker file key                              
                                                                                
         ICM   R0,15,TQATIME                                                    
         GOTOR EDTTUS,RL7ATIM      Edit arrival time                            
         MVC   PQMTIME,RL7ATIM                                                  
                                                                                
         MVC   PQMACTN(L'QMACTN1),QMACTN1                                       
         CLI   TQQFLAG,FF                                                       
         JE    QMAINT18                                                         
         MVC   PQMACTN(L'QMACTN2),QMACTN2                                       
         CLI   TQQFLAG,FE                                                       
         JE    QMAINT18                                                         
         MVC   PQMACTN(L'QMACTN3),QMACTN3                                       
         CLI   TQQFLAG,FD                                                       
         JE    QMAINT18                                                         
         MVC   PQMACTN(L'QMACTN4),QMACTN4                                       
                                                                                
QMAINT18 GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
         AHI   R3,TABSQUEL         Bump to next report entry                    
         CLI   TABSQUED,FF         Test end of report table                     
         JNE   QMAINT12                                                         
                                                                                
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         MVC   P,SPACES                                                         
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Print big request details to the RUNNER trace log                   *         
* Requests in error always print else test for trigger value exceeded *         
***********************************************************************         
                                                                                
LOGREQ   NTR1  LABEL=*                                                          
         CHI   R1,1                                                             
         JE    LOGREQ02                                                         
         CLI   DDLYNUM,0           Don't log if retrying deadly                 
         JNE   EXITY                                                            
         CLI   ERROR,TQEGOOD       TEST REQUEST IN ERROR                        
         JNE   LOGREQ02                                                         
         OC    BIGIOS,BIGIOS       Test for BIGIO parameter                     
         JZ    *+14                                                             
         CLC   RTOTSIO,BIGIOS      Test I/O count exceeded                      
         JH    LOGREQ02                                                         
         L     R0,RFILSEG                                                       
         AHI   R0,1                R0=number of segments used                   
         OC    BIGSEG,BIGSEG       Test for BIGSEG parameter                    
         JZ    *+12                                                             
         C     R0,BIGSEG           Test number of segments exceeded             
         JH    LOGREQ02                                                         
         OC    BIGCPU,BIGCPU       Test for BIGCPU parameter                    
         JZ    EXITY                                                            
         LG    GR1,CPUREQ                                                       
         DSGF  GR0,F10000                                                       
         ST    R1,FULL                                                          
         CLC   FULL,BIGCPU         Test CPU units exceeded                      
         JNH   EXITY                                                            
                                                                                
LOGREQ02 MVC   PMESSAGE(L'SORLIT),SORLIT                                        
         GOTOR PRTLOG              Print start of trace line                    
         TM    LP_FLAG2,LP_FMQIM   Test processing an MQ message                
         JNZ   LOGREQ06                                                         
                                                                                
         L     R2,AREQSAVE         Point to request record                      
         LLH   R3,0(R2)                                                         
         XR    R4,R4               preset unlimited lines                       
         CLI   REQTRACE,C'1'       Option to print first 1-9 lines?             
         JL    LOGREQ04            No, it's empty or F                          
         IC    R4,REQTRACE         pick up zoned number                         
         AHI   R4,-C'0'            Clear zone                                   
LOGREQ04 LHI   R0,40               Width of HEXOUT line                         
         CR    R3,R0               Test remainder exceeds line width            
         JH    *+6                                                              
         LR    R0,R3               No - print remainder                         
         GOTOR MCVHEXOU,DMCB,(R2),PMESSAGE,(R0),0                               
         GOTOR PRTLOG                                                           
         JCT   R4,*+8              REQTRACE limit reached?                      
         J     LOGREQX             Yes, exit                                    
         SR    R3,R0               Decrement output length                      
         JZ    LOGREQX                                                          
         AR    R2,R0               Point to next                                
         J     LOGREQ04                                                         
                                                                                
LOGREQ06 L     R2,MQIBUFF          Format MQ message for printing               
         L     R0,MQLIMSG                                                       
         XR    R4,R4               preset unlimited lines                       
         CLI   REQTRACE,C'1'       Option to print first 1-9 lines?             
         JL    LOGREQ08            No, it's empty or F                          
         IC    R4,REQTRACE         pick up zoned number                         
         AHI   R4,-C'0'            Clear zone                                   
LOGREQ08 LR    R3,R0                                                            
         CHI   R3,80               Test less than 80 characters left            
         JL    *+8                                                              
         LHI   R3,80               No - set to move 80                          
         BCTR  R3,0                Subtract 1 for execute                       
         SAM31 ,                                                                
         EX    R3,BLOGMVM2         Move message data to print line              
         SAM24 ,                                                                
         AHI   R3,1                Add back the one subtracted above            
         GOTOR PRTLOG                                                           
         JCT   R4,*+8              REQTRACE limit reached?                      
         J     LOGREQX             Yes, exit                                    
         SR    R0,R3               Decrement output length                      
         JZ    LOGREQX                                                          
         AR    R2,R3               Point to next                                
         J     LOGREQ08                                                         
                                                                                
LOGREQX  MVC   PMESSAGE(L'EORLIT),EORLIT                                        
         GOTOR PRTLOG              Print end of trace line                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Open files for a system - R1=A(SEOLST entry)                        *         
***********************************************************************         
                                                                                
OPNCLO   NTR1  LABEL=*,WORK=(RC,OCWORKL)                                        
         USING OCWORKD,RC                                                       
         LR    R3,R1                                                            
         USING SEOLSTD,R3          R3=A(system entry)                           
                                                                                
         MVC   PMESSAGE(L'INFLIT1),INFLIT1                                      
         CLI   OPNACT,OPNAOPN      Test open or close                           
         JE    *+10                                                             
         MVC   PMESSAGE(L'INFLIT6),INFLIT6                                      
         MVC   PMESSAGE+L'INFLIT1(L'SEOLNAM),SEOLNAM                            
         TM    SEOLFLAG,SEOLFGLB   Test global system                           
         JZ    *+10                                                             
         MVC   PMESSAGE+L'INFLIT1+L'SEOLNAM+1(L'SYSGLB),SYSGLB                  
                                                                                
         OC    THISBLK,THISBLK     Any command info                             
         JZ    OPNCLOZ                                                          
         LA    R1,PMESSAGE+L'INFLIT1+L'SEOLNAM+L'SYSGLB+2                       
         MVC   0(2,R1),=C'(J'                                                   
         EDIT  (B2,THISJOB),(6,2(R1)),FILL=0                                    
         MVI   8(R1),C')'                                                       
         LA    R1,10(R1)                                                        
         ST    R1,PARM+4                                                        
         GOTOR MCVHEXOU,PARM,THISBLK,,8,0                                       
                                                                                
OPNCLOZ  GOTOR PRTLOG                                                           
                                                                                
         L     RE,MCUTL                                                         
         MVC   OCSENUM,4(RE)                                                    
         MVC   4(1,RE),SEOLNUM     Set SE number in UTL for open                
         NI    SEOLFLAG,FF-(SEOLFCLS+SEOLFOPN)                                  
         ICM   RF,7,SVRFILE1       Assume FILE1                                 
         TM    SEOLFLAG,SEOLFIL2+SEOLFILX                                       
         JZ    OPNCLO0             OK if not FILE2 or UNDEFINED                 
         ICM   RF,7,SVRFILE2       Assume FILE2 for now                         
                                                                                
OPNCLO0  LARL  R1,OVERFILE         Scan for override in table                   
OPNCLO1  CLC   SEOLNUM,1(R1)       Find SE in table                             
         JE    OPNCLO2                                                          
         ICM   R1,15,2(R1)         Next                                         
         JZ    OPNCLO01            Not found so continue                        
         J     OPNCLO1                                                          
                                                                                
OPNCLO2  LA    RF,6(R1)            Load override file list                      
                                                                                
OPNCLO01 MVC   OCSYSTEM,0(RF)      Set system name                              
         AHI   RF,L'OCSYSTEM       RF=A(input file list)                        
         LHI   R0,OCFILESM         R0=maximum number of files                   
         LA    R1,OCFILES          R1=A(output file list)                       
                                                                                
OPNCLO02 MVC   0(1,R1),0(RF)       Copy control character                       
         CLI   0(R1),C'X'          Test end of input file list                  
         JE    OPNCLO06                                                         
         MVC   1(L'OCFILES-1,R1),1(RF)                                          
         CLI   0(R1),C'N'          Test read-only file                          
         JE    OPNCLO04                                                         
         CLI   0(R1),C'U'          Test updative file                           
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    SEOLFLAG,SEOLFGLB   Test updative system                         
         JNZ   OPNCLO04                                                         
         MVI   0(R1),C'N'          No - set read-only file                      
                                                                                
OPNCLO04 AHI   RF,L'OCFILES        Bump to next input file                      
         AHI   R1,L'OCFILES        Bump to next output file                     
         JCT   R0,OPNCLO02                                                      
         DC    H'0'                Too many files in open list                  
                                                                                
OPNCLO06 CHI   R0,OCFILESM                                                      
         JNE   *+6                                                              
         DC    H'0'                No files in open list                        
         LA    R0,DMOPEN           Point to appropriate action                  
         CLI   OPNACT,OPNAOPN                                                   
         JE    *+8                                                              
         LA    R0,DMCLSE                                                        
         GOTOR DMGR,DMCB,(R0),OCSYSTEM,OCFILES,AIO,0                            
         L     RE,MCUTL                                                         
         MVC   4(L'OCSENUM,RE),OCSENUM                                          
                                                                                
         TM    RUNINDS1,RUNIBTCH   Test batch processing mode                   
         JNZ   OPNCLO08            Do not call DDSTATE in batch mode            
                                                                                
         XC    DMCB(24),DMCB                                                    
         LA    RF,=C'LOPEN '       DO OPEN CLOSE STATE CALL FOR SYSTEM          
         CLI   OPNACT,OPNAOPN                                                   
         JE    *+8                                                              
         LA    RF,=C'LCLOSE'                                                    
         MVC   DMCB+7(1),SEOLNUM                                                
         GOTO1 =V(DDSTATE),DMCB,(RF)                                            
*&&UK                                                                           
         CLI   SEOLNUM,X'14'       MEDZ STATE CHANGE                            
         JNE   OPNCLO08                                                         
         NI    RUNINDS2,255-RUNIMEDZ                                            
         CLI   OPNACT,OPNACLO      If close                                     
         JNE   OPNCLO08                                                         
         OI    RUNINDS2,RUNIMEDZ   Flag MEDZ closed                             
*&&                                                                             
                                                                                
OPNCLO08 OC    SEOLFLAG,OPNACT     Set open/close status                        
         J     VALPARE             Which may be waiting to run                  
         DROP  R3,RC                                                            
                                                                                
OCWORKD  DSECT ,                   ** OPNCLO local w/s **                       
OCSENUM  DS    X                   Current SE number from UTL                   
OCSYSTEM DS    CL7                 System name                                  
OCFILESM EQU   32                  Maximum number of files supported            
OCFILES  DS    (OCFILESM)XL8,X     File list                                    
OCWORKL  EQU   *-OCWORKD                                                        
RUNNER   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Print input/output MQ trace                                         *         
***********************************************************************         
                                                                                
MQTRC    NTR1  LABEL=*                                                          
         LARL  R4,MQGLOB           Point to MQ globals                          
         USING MQGLOB,R4                                                        
         LTR   R2,R1               Test input/output message                    
         JNZ   MQTRC02                                                          
         TM    RUNINDMQ,RUNIMQIT   Test input trace required                    
         JZ    EXIT                                                             
         MVC   PMESSAGE(L'MQTRLIT1),MQTRLIT1                                    
         L     R1,MQIBUFF                                                       
         L     R0,MQLIMSG                                                       
         J     MQTRC04                                                          
                                                                                
MQTRC02  TM    LP_FLAG2,LP_FFALT   Test FALINK test string input                
         JNZ   *+12                                                             
         TM    RUNINDMQ,RUNIMQOT   Test output trace required                   
         JZ    EXIT                                                             
         MVC   PMESSAGE(L'MQTRLIT2),MQTRLIT2                                    
         L     R1,MQOBUFF                                                       
         L     R0,MQLOMSG                                                       
                                                                                
MQTRC04  GOTOR PRTLOG                                                           
                                                                                
MQTRC06  CHI   R0,79                                                            
         JNH   MQTRC08                                                          
         SAM31 ,                                                                
         MVC   PMESSAGE(79),0(R1)                                               
         SAM24 ,                                                                
         GOTOR PRTLOG                                                           
         AHI   R1,79                                                            
         SHI   R0,79                                                            
         J     MQTRC06                                                          
                                                                                
MQTRC08  LTR   RF,R0                                                            
         JZ    MQTRC10                                                          
         BCTR  RF,0                                                             
         SAM31 ,                                                                
         EX    RF,MTRCMVM1                                                      
         SAM24 ,                                                                
         GOTOR PRTLOG                                                           
                                                                                
MQTRC10  MVC   PMESSAGE(L'MQTRLIT3),MQTRLIT3                                    
         LTR   R2,R2                                                            
         JZ    *+10                                                             
         MVC   PMESSAGE(L'MQTRLIT4),MQTRLIT4                                    
         GOTOR PRTLOG                                                           
         J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Build an array indexable by FACPAK id number indicating whether a   *         
* FACPAK specific server is running for each one                      *         
***********************************************************************         
                                                                                
BLDSVR   NTR1  LABEL=*                                                          
         LAM   AR2,AR2,ALET        Build list of specific servers               
         ICM   R2,15,OFFSET                                                     
         SAC   512                                                              
         ICM   R2,15,TABSRUN-FATABSD(R2)                                        
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,TABSNSVR-TABSRUND(R2)                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R2,15,TABSASVR-TABSRUND(R2)                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING TABSSVRD,R2                                                      
         XC    WORK,WORK           Clear server array                           
                                                                                
BLDSVR02 CLC   TSLOCKID,ARZERO     Test server entry active                     
         JE    BLDSVR04                                                         
         CLC   TSTYPE,LP_SVRTY     Yes - test same as my server                 
         JNE   BLDSVR04                                                         
         C     R2,ASVR             Test myself                                  
         JE    BLDSVR04                                                         
         SR    RE,RE                                                            
         ICM   RE,1,TSLFACID       Test server for specific FACPAK              
         JZ    *+12                                                             
         LA    RE,WORK(RE)                                                      
         OI    0(RE),1             Yes - set flag in server array               
BLDSVR04 AH    R2,SVRWIDTH         Bump to next server                          
         JCT   R0,BLDSVR02         Do for all servers                           
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Test any other server runs this work                                *         
* R1 points to agy alpha in tabs dataspace.                           *         
***********************************************************************         
                                                                                
LOCSVR   NTR1  LABEL=*                                                          
         LAM   ARE,ARE,ALET                                                     
         ICM   RE,15,OFFSET                                                     
         SAC   512                                                              
         ICM   RE,15,TABSRUN-FATABSD(RE)                                        
         LLH   R0,TABSNSVR-TABSRUND(,RE)                                        
         ICM   RE,15,TABSASVR-TABSRUND(RE)                                      
Q        USING TABSSVRD,RE                                                      
LOCSVR02 CLC   Q.TSTYPE,LP_SVRTY                                                
         JNE   LOCSVR06                                                         
         CLC   Q.TSTFAGYS,ARZERO                                                
         JE    LOCSVR06                                                         
         CPYA  ARF,ARE                                                          
         CPYA  AR1,ARE                                                          
                                                                                
         LA    RF,Q.TSTFAGYS                                                    
         LHI   R2,TSTFAGY#                                                      
X        USING TSTFAGYS,RF                                                      
LOCSVR04 CLC   X.TSTFAGYS,SPACES   Not in agency list - ignore                  
         JE    LOCSVR06                                                         
         CLC   X.TSTFAGYS,0(R1)    Other server runs this work                  
         JE    LOCSVRY                                                          
         AHI   RF,L'TSTFAGYS                                                    
         JCT   R2,LOCSVR04                                                      
                                                                                
LOCSVR06 AH    RE,SVRWIDTH         Test all servers                             
         JCT   R0,LOCSVR02                                                      
         LAM   ARE,AR1,ARZERO                                                   
         J     EXITN               I should run this work                       
                                                                                
LOCSVRY  LAM   ARE,AR1,ARZERO                                                   
         J     EXITY               Other server should run this work            
         DROP  Q,X                                                              
         EJECT                                                                  
***********************************************************************         
* Resolve user id and system (SE) values                              *         
***********************************************************************         
                                                                                
RESSYS   NTR1  LABEL=*                                                          
                                                                                
         LARL  RF,SEOLST           Look up system in system list                
         USING SEOLSTD,RF                                                       
         LA    R0,SEOLSTM          R0=maximum N'systems                         
RESSYS02 CLI   SEOLNUM,SEOLEOTQ    Test end of list                             
         JE    RESSYSN                                                          
         CLC   SEOLNUM,LP_SENO     Match system to table                        
         JE    RESSYS04                                                         
         AHI   RF,SEOLSTL          No - bump to next entry                      
         JCT   R0,RESSYS02                                                      
         J     RESSYSN                                                          
                                                                                
RESSYS04 ST    RF,RSEOLST          Set A(SELOST entry) for app.                 
         TM    SEOLFLAG,SEOLFOPN   Test system is open here                     
         JZ    EXITL               No - exit with CC low                        
         L     RE,MCUTL            Set SE number in UTL                         
         MVC   4(1,RE),SEOLNUM                                                  
                                                                                
         NI    LP_INDS,FF-LP_IGLOB                                              
         TM    SEOLFLAG,SEOLFGLB   Test primary system is global                
         JZ    *+8                                                              
         OI    LP_INDS,LP_IGLOB                                                 
                                                                                
         XC    RSEOLST2,RSEOLST2                                                
         NI    LP_INDS,FF-(LP_IGLO2+LP_IOPN2)                                   
         CLI   LP_SENO2,0          Test secondary system resolved               
         JE    EXITY               No                                           
         LARL  RF,SEOLST           Look up 2nd system in system list            
         LA    R0,SEOLSTM          R0=maximum N'systems                         
RESSYS06 CLI   SEOLNUM,SEOLEOTQ    Test end of list                             
         JE    RESSYS08                                                         
         CLC   SEOLNUM,LP_SENO2    Match system to table                        
         JE    RESSYS08                                                         
         AHI   RF,SEOLSTL          No - bump to next entry                      
         JCT   R0,RESSYS06                                                      
         J     EXITY                                                            
                                                                                
RESSYS08 ST    RF,RSEOLST2         Set A(SELOST2 entry) for app.                
         TM    SEOLFLAG,SEOLFOPN   Test 2nd system is open here                 
         JZ    *+8                                                              
         OI    LP_INDS,LP_IOPN2                                                 
         TM    SEOLFLAG,SEOLFGLB   Test 2nd system is global                    
         JZ    *+8                                                              
         OI    LP_INDS,LP_IGLO2    Set 2nd system is global                     
         J     EXITY                                                            
                                                                                
RESSYSN  MVI   ERROR,TQEISYS       Set invalid system                           
         J     EXITH               Exit with CC high                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* Put datamanager and trap summaries to the log                       *         
***********************************************************************         
                                                                                
TSUM     NTR1  LABEL=*                                                          
         SR    R0,R0                                                            
         ICM   R0,3,DMTNUM         Do I/O trace summary first                   
         JZ    TSUM04                                                           
         MVC   P(19),=C'<<< I/O summary >>>'                                    
         MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
         LARL  R2,DMTTAB                                                        
         USING DMTTABD,R2                                                       
         USING TRPVALD,DMTVALS                                                  
TSUM02   MVC   RLGLABEL,SPACES                                                  
         MVC   RLGLABEL(L'DMTNAME),DMTNAME                                      
         GOTOR PUTSUM,TRPVALD                                                   
         XC    TRPQVAL(TRPQVALL),TRPQVAL                                        
         MVI   TRPNEST,0                                                        
         AHI   R2,DMTTABL                                                       
         JCT   R0,TSUM02                                                        
         MVC   P(26),=C'<<< End of I/O summary >>>'                             
         MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
TSUM04   SR    R0,R0                                                            
         ICM   R0,3,TRPNUM         Do trap summary next                         
         JZ    TSUMX                                                            
         MVC   P(20),=C'<<< Trap summary >>>'                                   
         MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
         LARL  R2,TRPTAB                                                        
         USING TRPTABD,R2                                                       
         USING TRPVALD,TRPVALS                                                  
TSUM06   L     R1,TRPAMOD                                                       
         MVC   RLGLABEL,SPACES                                                  
         MVC   RLGLABEL(L'MODTMODN),MODTMODN-MODTABD(R1)                        
         CLC   MODTMODN-MODTABD(,R1),SPACES                                     
         JH    *+10                                                             
         MVC   RLGLABEL(L'MODTPHSN),MODTPHSN-MODTABD(R1)                        
         GOTOR PUTSUM,TRPVALD                                                   
         XC    TRPQVAL(TRPQVALL),TRPQVAL                                        
         MVI   TRPNEST,0                                                        
         AHI   R2,TRPTABL                                                       
         JCT   R0,TSUM06                                                        
         MVC   P(27),=C'<<< End of trap summary >>>'                            
         MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
TSUMX    J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Print trap summary line(s)                                          *         
***********************************************************************         
                                                                                
PUTSUM   NTR1  LABEL=*                                                          
         LR    R2,R1                                                            
         USING TRPVALD,R2                                                       
                                                                                
         ICM   R0,15,TRPQCALL      Print request summary line                   
         JZ    EXITY               Exit if no calls for this trap               
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGCALL,DUB                                                      
                                                                                
         LG    GR0,TRPQCPU                                                      
         GOTOR ADJCPU              Adjust CPU time if necessary                 
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGCPUT,DUB                                                      
                                                                                
         LG    GR0,TRPQSRB                                                      
         GOTOR ADJSRB              Adjust SRB time if necessary                 
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGSRBT,DUB                                                      
                                                                                
         LG    GR0,TRPQCPU                                                      
         ALG   GR0,TRPQSRB                                                      
         GOTOR ADJTOT              Adjust total time if necessary               
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGTOTT,DUB                                                      
                                                                                
         ICM   R0,15,TRPQIOS                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGIOT,DUB                                                       
                                                                                
         MVC   P(RUNLITGL),RUNLITG                                              
         MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
                                                                                
         CLC   TRPQCALL,TRPRCALL   Test run total same as request               
         JE    EXIT                                                             
                                                                                
         MVC   RLGLABEL,SPACES     Pint run summary line                        
                                                                                
         L     R0,TRPRCALL                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGCALL,DUB                                                      
                                                                                
         LG    GR0,TRPRCPU                                                      
         GOTOR ADJCPU                                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGCPUT,DUB                                                      
                                                                                
         LG    GR0,TRPRSRB                                                      
         GOTOR ADJSRB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGSRBT,DUB                                                      
                                                                                
         LG    GR0,TRPRCPU                                                      
         ALG   GR0,TRPRSRB                                                      
         GOTOR ADJTOT                                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGTOTT,DUB                                                      
                                                                                
         ICM   R0,15,TRPRIOS                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLGIOT,DUB                                                       
                                                                                
         MVC   P(RUNLITGL),RUNLITG                                              
         MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* RUNNER datamanager trace                                            *         
***********************************************************************         
                                                                                
DMTR     NTR1  LABEL=*,WORK=(RC,DTWORKL)                                        
         USING DTWORKD,RC          RC=A(local w/s)                              
         LR    R4,R1               R4=A(caller's DMCB)                          
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         SR    R1,R1                                                            
         ICM   R1,7,5(R4)          Point to file name                           
         LLH   R0,DMTNUM           Get number of entries in DMTTAB              
         LARL  R3,DMTTAB                                                        
         USING DMTTABD,R3                                                       
         USING TRPVALD,DMTVALS                                                  
DMTR0010 LLC   RF,DMTCLCL          Get length-1 of file name into RF            
         EX    RF,DTRCCLF1         Match to trap table entry                    
         JE    DMTR0020                                                         
         AHI   R3,DMTTABL          No match - bump to next entry                
         JCT   R0,DMTR0010         Do for number of entries                     
                                                                                
DMTR0012 ICM   R3,15,AIOOTHER      Get address of IOOTHER entry                 
         JNZ   DMTR0020                                                         
         MVI   BYTE1,DMTTOIOQ                                                   
         GOTOR ADDDMT,IOOTHER      Add trap for IOOTHER                         
         L     R3,AIOOTHER         Set address                                  
                                                                                
DMTR0020 OC    DMTNAME,DMTNAME     Save file name first time through            
         JNZ   *+10                                                             
         MVC   DMTNAME,0(R1)                                                    
                                                                                
         MVC   DTFILE,0(R1)        Set file name                                
         ICM   R1,7,1(R4)          Point to action name                         
         MVC   DTACTN,0(R1)        Set action name                              
                                                                                
         CLI   TRPPARM,TRPPSUMQ    Test summary only                            
         JE    DMTR0030                                                         
*                                                                               
         LLC   RF,TRAPNEST                                                      
         LA    RF,P(RF)                                                         
         MVC   0(4,RF),=C'DMTR'                                                 
         MVC   5(L'DTACTN,RF),DTACTN                                            
         MVC   5+L'DTACTN+1(L'DTFILE,RF),DTFILE                                 
         CLI   DMTKLEN,0           Test there is a key length                   
         JE    DMTR0022                                                         
         CLC   =C'DMR',DTACTN                                                   
         JE    *+14                                                             
         CLC   =C'DMW',DTACTN                                                   
         JNE   DMTR0028                                                         
         MVC   P+L'RLFLABEL(4),=C'Key='                                         
         L     R1,8(R4)            Point to key and extract into WORK           
         MVC   WORK(MAXKEYLN),0(R1)                                             
         LLC   R0,DMTKLEN          Length to print                              
         GOTOR MCVHEXOU,DTDMCB,WORK,P+L'RLFLABEL+4,(R0),0                       
         J     DMTR0028                                                         
                                                                                
DMTR0022 CLC   =C'GET',DTACTN                                                   
         JE    *+14                                                             
         CLC   =C'PUT',DTACTN                                                   
         JNE   DMTR0028                                                         
         MVC   P+L'RLFLABEL(4),=C'D/A='                                         
         L     R0,8(R4)            Point to disk address                        
         GOTOR MCVHEXOU,DTDMCB,(R0),P+L'RLFLABEL+4,4,0                          
                                                                                
DMTR0028 MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
                                                                                
DMTR0030 GOTOR GETTIM                                                           
         STG   GR1,TRPCCPU                                                      
         STG   GR0,TRPCSRB                                                      
         MVC   TRPCIOS,RTOTSIO                                                  
                                                                                
         GOTOR DMGR,(R4)           Call real DATAMGR                            
                                                                                
         IPM   R0                  Save condition code                          
                                                                                
         CLI   TRPPARM,TRPPSUMQ    Test summary only                            
         JE    DMTR0040                                                         
         LLC   RF,TRAPNEST                                                      
         LA    RF,P(RF)                                                         
         MVC   0(4,RF),=C'DMTR'                                                 
         MVC   5(5,RF),=C'After'                                                
         MVC   12(L'DTACTN,RF),DTACTN                                           
         CLI   DMTKLEN,0           Test there is a key length                   
         JE    DMTR0040                                                         
         CLC   =C'DMR',DTACTN                                                   
         JE    *+14                                                             
         CLC   =C'DMW',DTACTN                                                   
         JNE   DMTR0040                                                         
         MVC   P+L'RLFLABEL(4),=C'Key='                                         
         L     R1,12(R4)           Point to key and extract into WORK           
         MVC   WORK(MAXKEYLN),0(R1)                                             
         LLC   R0,DMTKLEN          Length to print                              
         GOTOR MCVHEXOU,DTDMCB,WORK,P+L'RLFLABEL+4,(R0),0                       
         MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
                                                                                
                                                                                
DMTR0040 MVC   RLFLABEL,SPACES                                                  
         LLC   RF,TRAPNEST                                                      
         LA    RF,RLFLABEL(RF)                                                  
         MVC   0(L'DTFILE,RF),DTFILE                                            
         MVC   TRPNEST,TRAPNEST    Set I am the owner of this                   
         GOTOR PUTDET,DMTVALS      Print detail trace line                      
                                                                                
         SPM   R0                  Restore condition code                       
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
DTWORKD  DSECT ,                   ** TRACE routine local w/s **                
DTDMCB   DS    6F                  For calls                                    
DTFILE   DS    CL(L'DMTFILE)       File name                                    
DTACTN   DS    CL6                 Action                                       
DTWORKL  EQU   *-DTWORKD                                                        
RUNNER   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Trap entry points - each TRPTAB entry corresponds to a jump         *         
* instruction below, TRAP calculates the trap index number based on   *         
* the displacement of RF into this list                               *         
***********************************************************************         
                                                                                
         DS    0H                                                               
TNDX     DS    0XL4                                                             
TNDX001  J     TRAP                                                             
TNDX002  J     TRAP                                                             
TNDX003  J     TRAP                                                             
TNDX004  J     TRAP                                                             
TNDX005  J     TRAP                                                             
TNDX006  J     TRAP                                                             
TNDX007  J     TRAP                                                             
TNDX008  J     TRAP                                                             
TNDX009  J     TRAP                                                             
TNDX010  J     TRAP                                                             
TNDX011  J     TRAP                                                             
TNDX012  J     TRAP                                                             
TNDX013  J     TRAP                                                             
TNDX014  J     TRAP                                                             
TNDX015  J     TRAP                                                             
TNDX016  J     TRAP                                                             
TNDX017  J     TRAP                                                             
TNDX018  J     TRAP                                                             
TNDX019  J     TRAP                                                             
TNDX020  J     TRAP                                                             
TNDX021  J     TRAP                                                             
TNDX022  J     TRAP                                                             
TNDX023  J     TRAP                                                             
TNDX024  J     TRAP                                                             
TNDX025  J     TRAP                                                             
TNDX026  J     TRAP                                                             
TNDX027  J     TRAP                                                             
TNDX028  J     TRAP                                                             
TNDX029  J     TRAP                                                             
TNDX030  J     TRAP                                                             
TNDX031  J     TRAP                                                             
TNDX032  J     TRAP                                                             
TNDX033  J     TRAP                                                             
TNDX034  J     TRAP                                                             
TNDX035  J     TRAP                                                             
TNDX036  J     TRAP                                                             
TNDX037  J     TRAP                                                             
TNDX038  J     TRAP                                                             
TNDX039  J     TRAP                                                             
TNDX040  J     TRAP                                                             
TNDX041  J     TRAP                                                             
TNDX042  J     TRAP                                                             
TNDX043  J     TRAP                                                             
TNDX044  J     TRAP                                                             
TNDX045  J     TRAP                                                             
TNDX046  J     TRAP                                                             
TNDX047  J     TRAP                                                             
TNDX048  J     TRAP                                                             
TNDX049  J     TRAP                                                             
TNDX050  J     TRAP                                                             
TNDX051  J     TRAP                                                             
TNDX052  J     TRAP                                                             
TNDX053  J     TRAP                                                             
TNDX054  J     TRAP                                                             
TNDX055  J     TRAP                                                             
TNDX056  J     TRAP                                                             
TNDX057  J     TRAP                                                             
TNDX058  J     TRAP                                                             
TNDX059  J     TRAP                                                             
TNDX060  J     TRAP                                                             
TNDX061  J     TRAP                                                             
TNDX062  J     TRAP                                                             
TNDX063  J     TRAP                                                             
TNDX064  J     TRAP                                                             
TNDX065  J     TRAP                                                             
TNDX066  J     TRAP                                                             
TNDX067  J     TRAP                                                             
TNDX068  J     TRAP                                                             
TNDX069  J     TRAP                                                             
TNDX070  J     TRAP                                                             
TNDX071  J     TRAP                                                             
TNDX072  J     TRAP                                                             
TNDX073  J     TRAP                                                             
TNDX074  J     TRAP                                                             
TNDX075  J     TRAP                                                             
TNDX076  J     TRAP                                                             
TNDX077  J     TRAP                                                             
TNDX078  J     TRAP                                                             
TNDX079  J     TRAP                                                             
TNDX080  J     TRAP                                                             
TNDX081  J     TRAP                                                             
TNDX082  J     TRAP                                                             
TNDX083  J     TRAP                                                             
TNDX084  J     TRAP                                                             
TNDX085  J     TRAP                                                             
TNDX086  J     TRAP                                                             
TNDX087  J     TRAP                                                             
TNDX088  J     TRAP                                                             
TNDX089  J     TRAP                                                             
TNDX090  J     TRAP                                                             
TNDX091  J     TRAP                                                             
TNDX092  J     TRAP                                                             
TNDX093  J     TRAP                                                             
TNDX094  J     TRAP                                                             
TNDX095  J     TRAP                                                             
TNDX096  J     TRAP                                                             
TNDX097  J     TRAP                                                             
TNDX098  J     TRAP                                                             
TNDX099  J     TRAP                                                             
TNDX100  J     TRAP                                                             
TNDX101  J     TRAP                                                             
TNDX102  J     TRAP                                                             
TNDX103  J     TRAP                                                             
TNDX104  J     TRAP                                                             
TNDX105  J     TRAP                                                             
TNDX106  J     TRAP                                                             
TNDX107  J     TRAP                                                             
TNDX108  J     TRAP                                                             
TNDX109  J     TRAP                                                             
TNDX110  J     TRAP                                                             
TNDX111  J     TRAP                                                             
TNDX112  J     TRAP                                                             
TNDX113  J     TRAP                                                             
TNDX114  J     TRAP                                                             
TNDX115  J     TRAP                                                             
TNDX116  J     TRAP                                                             
TNDX117  J     TRAP                                                             
TNDX118  J     TRAP                                                             
TNDX119  J     TRAP                                                             
TNDX120  J     TRAP                                                             
TNDX121  J     TRAP                                                             
TNDX122  J     TRAP                                                             
TNDX123  J     TRAP                                                             
TNDX124  J     TRAP                                                             
TNDX125  J     TRAP                                                             
TNDX126  J     TRAP                                                             
TNDX127  J     TRAP                                                             
TNDX128  J     TRAP                                                             
TNDX129  J     TRAP                                                             
TNDX130  J     TRAP                                                             
TNDX131  J     TRAP                                                             
TNDX132  J     TRAP                                                             
TNDX133  J     TRAP                                                             
TNDX134  J     TRAP                                                             
TNDX135  J     TRAP                                                             
TNDX136  J     TRAP                                                             
TNDX137  J     TRAP                                                             
TNDX138  J     TRAP                                                             
TNDX139  J     TRAP                                                             
TNDX140  J     TRAP                                                             
TNDX141  J     TRAP                                                             
TNDX142  J     TRAP                                                             
TNDX143  J     TRAP                                                             
TNDX144  J     TRAP                                                             
TNDX145  J     TRAP                                                             
TNDX146  J     TRAP                                                             
TNDX147  J     TRAP                                                             
TNDX148  J     TRAP                                                             
TNDX149  J     TRAP                                                             
TNDX150  J     TRAP                                                             
TNDX161  J     TRAP                                                             
TNDX162  J     TRAP                                                             
TNDX163  J     TRAP                                                             
TNDX164  J     TRAP                                                             
TNDX165  J     TRAP                                                             
TNDX166  J     TRAP                                                             
TNDX167  J     TRAP                                                             
TNDX168  J     TRAP                                                             
TNDX169  J     TRAP                                                             
TNDX170  J     TRAP                                                             
TNDXMAX  EQU   (*-TNDX)/L'TNDX                                                  
                                                                                
***********************************************************************         
* RUNNER module trap                                                  *         
***********************************************************************         
                                                                                
TRAP     NTR1  LABEL=*,WORK=(RC,TRWORKL)                                        
         USING TRWORKD,RC          RC=A(local w/s)                              
                                                                                
         LR    R4,R1               R4=A(caller's parameter list)                
         STCM  RF,8,TRHOB          Save HOB of RF                               
         ST    R1,TRR1                                                          
         ST    R0,TRR0             Save calling R0                              
         STAM  AR0,ARF,TRARS1      Save access registers coming in              
                                                                                
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         LAM   AR0,ARF,ARZERO      Clear access registers                       
         SAC   0                   and set addressing modes                     
         SAM24 ,                                                                
                                                                                
         LLC   R0,TRAPNEST         Increment trap nesting level                 
         AHI   R0,1                                                             
         STC   R0,TRAPNEST                                                      
                                                                                
         CLI   TRAPNEST,TRAPMAXN   Test maximum nest level reached              
         JNH   *+6                                                              
         DC    H'0'                Yes - need to increase TRAPMAXN              
                                                                                
         LARL  RE,TNDX             Point to trap index                          
         LA    RF,0(RF)            Clear HOB of RF                              
         SR    RF,RE               RF=displacement into trap index              
         JNM   *+6                                                              
         DC    H'0'                Need more trap jumps above                   
         SRL   RF,2                RF=trap index number (/4)                    
         CHI   RF,TNDXMAX          Test good index number                       
         JNH   *+6                                                              
         DC    H'0'                                                             
         MHI   RF,TRPTABL          RF=displacement to entry in TRPTAB           
         LARL  R3,TRPTAB                                                        
         AR    R3,RF                                                            
         USING TRPTABD,R3          R3=A(TRPTAB entry)                           
         USING TRPVALD,TRPVALS                                                  
         CLI   TRPNEST,0           Test trap nested                             
         JNE   *+10                Yes - higher nest owns it                    
         MVC   TRPNEST,TRAPNEST    Else I'll account for the time               
         ST    R3,TRATRP           Save pointer to TRPTAB entry                 
         L     RE,4(RD)            Save caller's R2 through RC                  
         MVC   TRR2RC(TRR2RCL),28(RE)                                           
                                                                                
         L     R2,TRPAMOD                                                       
         USING MODTABD,R2          R2=A(module table entry)                     
                                                                                
         CLI   TRPPARM,TRPPSUMQ    Test summary only                            
         JE    TRAP0022                                                         
         LLC   RF,TRAPNEST                                                      
         LA    RF,P(RF)                                                         
         BCTR  RF,0                                                             
         MVC   0(7,RF),=C'Calling'                                              
         MVC   8(L'MODTPHSN,RF),MODTPHSN                                        
         MVC   P+L'RLFLABEL(3),=C'R1='                                          
         ST    R4,FULL1                                                         
         GOTOR MCVHEXOU,TRDMCB,FULL1,P+L'RLFLABEL+3,L'FULL1,0                   
         C     R4,=A(RUNNERX-RUNNER)                                            
         JL    TRAP0020            Low is not a valid address                   
         MVC   P+48(6),=C'Parms='                                               
         GOTOR MCVHEXOU,TRDMCB,(R4),P+54,10*4,0                                 
                                                                                
TRAP0020 MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
                                                                                
TRAP0022 GOTOR GETTIM                                                           
         STG   GR1,TRCCPU          Save current CPU units                       
         STG   GR0,TRCSRB          Save current SRB units                       
         MVC   TRCIOS,RTOTSIO      and current I/O count                        
                                                                                
         LA    RF,TRWORKD          Point to my work area                        
                                                                                
         GOTOR GOTOIT              Call the module or server trap               
                                                                                
         STM   R2,RC,TRR2RC2-TRWORKD(RF)                                        
         ST    R0,TRR0-TRWORKD(RF)                                              
                                                                                
         IPM   R0                  Save condition code in R0                    
                                                                                
         LR    RC,RF               Point to my work area                        
         L     R3,TRATRP           Reset R3 and R2                              
         ICM   R2,15,TRPAMOD                                                    
         L     R4,TRR1             Point to caller's parameter list             
         GOTOR SETREG              Set RUNNER registers                         
                                                                                
         STAM  AR0,ARF,TRARS2      Save and clear access registers              
         LAM   AR0,ARF,ARZERO      (LOCKSPC (for one) sets them)                
         SAC   0                                                                
         SAM24 ,                                                                
                                                                                
         MVC   RLFLABEL,SPACES     Build label                                  
         LLC   R1,TRAPNEST                                                      
         LA    R1,RLFLABEL-1(R1)                                                
         MVC   0(L'MODTPHSN,R1),MODTPHSN                                        
                                                                                
         CLC   MODTMODN,SPACES     Build module name                            
         JNH   TRAP0030                                                         
         LA    R1,L'MODTPHSN+1(R1)                                              
         MVI   0(R1),C'('                                                       
         MVC   1(L'MODTMODN,R1),MODTMODN                                        
         LA    R1,L'MODTMODN(R1)                                                
         CLI   0(R1),C' '                                                       
         JH    *+8                                                              
         JCT   R1,*-8                                                           
         MVI   1(R1),C')'                                                       
                                                                                
TRAP0030 MVC   TRPCCPU,TRCCPU      Set my start time                            
         MVC   TRPCSRB,TRCSRB                                                   
         MVC   TRPCIOS,TRCIOS      and I/O count                                
         GOTOR PUTDET,TRPVALD      Print detail trace line                      
         LAM   AR0,ARF,TRARS2      Restore access registers                     
         CLC   TRPNEST,TRAPNEST    Test I am the owner of this trap             
         JNE   *+8                                                              
         MVI   TRPNEST,0           Yes - now set unowned                        
         LLC   RF,TRAPNEST         Decrement trap nesting level                 
         BCTR  RF,0                                                             
         STC   RF,TRAPNEST                                                      
                                                                                
         SPM   R0                  Restore condition code                       
                                                                                
         L     R0,TRR0             Restore returned R0 value                    
         L     R1,TRR1             Restore original R1 value                    
         LM    R2,RC,TRR2RC2       Restore R2 through RC                        
         L     RD,4(RD)            Restore RD                                   
         LM    RE,RF,12(RD)        Restore RE and RF                            
         BR    RE                  Return to caller                             
                                                                                
GOTOIT   NTR1  LABEL=*             Call trapped module                          
         ICM   RF,15,MODTREAL      Point to real routine                        
         OC    MODTTRAP,MODTTRAP                                                
         JZ    *+8                                                              
         ICM   RF,15,MODTTRAP      or trap routine if specified                 
         ICM   RF,8,TRHOB          Set calling HOB in RF                        
         LR    R1,R4               Point to calling parameter list              
         LAM   AR0,ARF,TRARS1      Restore access registers                     
         L     R0,TRR0             Restore R0                                   
         LM    R2,RC,TRR2RC        Restore general registers R2 thru RC         
         GOTOR (RF),(R1)           Go to module passing caller's R1             
         L     RD,4(RD)                                                         
         LM    RE,RF,12(RD)        Return with RF intact                        
         BR    RE                                                               
         DROP  R2,R3,RC                                                         
                                                                                
TRWORKD  DSECT ,                   ** TRAP local w/s **                         
TRR2RC   DS    11F                 Calling registers R2 through RC              
TRR2RCL  EQU   *-TRR2RC                                                         
TRR2RC2  DS    11F                 Caller's R2 through RC                       
TRDMCB   DS    6F                  For calls                                    
TRARS1   DS    16F                 Save area for access registers in            
TRARSL   EQU   *-TRARS1                                                         
TRARS2   DS    16F                 Save area for access registers out           
TRATRP   DS    A                   A(TRPTAB entry)                              
TRR1     DS    F                   Register 1 value                             
TRR0     DS    F                   Returned register 0 value                    
TRHOB    DS    X                   HOB of RF on entry to TRAP                   
                                                                                
TRCCPU   DS    D                   Start CPU time                               
TRCSRB   DS    D                   Start SRB time                               
TRCIOS   DS    F                   Start I/O count                              
                                                                                
TRWORKL  EQU   *-TRWORKD                                                        
RUNNER   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* Put detail trace line to RUNNER log - update request/run totals     *         
***********************************************************************         
                                                                                
PUTDET   NTR1  LABEL=*                                                          
                                                                                
         LR    R2,R1                                                            
         USING TRPVALD,R2                                                       
         OC    MCREQNO,MCREQNO     Don't log RUNNER requests                    
         JZ    PUTDET02                                                         
         CLC   TRPQREQ,MCREQNO     Test change of request number                
         JE    PUTDET02                                                         
         MVC   TRPQREQ,MCREQNO     Set new request number                       
         L     R0,TRPNREQ          Increment request count                      
         AHI   R0,1                                                             
         ST    R0,TRPNREQ                                                       
                                                                                
PUTDET02 L     RF,TRPQCALL         Increment request call count                 
         AHI   RF,1                                                             
         ST    RF,TRPQCALL                                                      
         L     RF,TRPRCALL         Increment run call count                     
         AHI   RF,1                                                             
         ST    RF,TRPRCALL                                                      
         GOTOR GETTIM              Get current time values                      
         SG    GR1,TRPCCPU                                                      
         STG   GR1,TRPCCPU         R1=Activity CPU                              
         SG    GR0,TRPCSRB                                                      
         STG   GR0,TRPCSRB         R0=Activity SRB                              
                                                                                
         CLC   TRPNEST,TRAPNEST    Test this is the owner                       
         JNE   PUTDET04            No                                           
                                                                                
         LG    GRF,TRPQCPU         Update request CPU                           
         AGR   GRF,GR1                                                          
         STG   GRF,TRPQCPU                                                      
         LG    GRF,TRPRCPU         Update run CPU                               
         AGR   GRF,GR1                                                          
         STG   GRF,TRPRCPU                                                      
         LG    GRF,TRPQSRB         Update request SRB                           
         AGR   GRF,GR0                                                          
         STG   GRF,TRPQSRB                                                      
         LG    GRF,TRPRSRB         Update run SRB                               
         AGR   GRF,GR0                                                          
         STG   GRF,TRPRSRB                                                      
         L     R1,RTOTSIO          Update I/O counts                            
         L     R0,TRPCIOS                                                       
         SR    R1,R0               R1=number for this request                   
         ST    R1,TRPCIOS          Set current I/O count                        
         L     R0,TRPQIOS                                                       
         AR    R0,R1                                                            
         ST    R0,TRPQIOS          Set total I/O count                          
         L     R0,TRPRIOS                                                       
         AR    R0,R1                                                            
         ST    R0,TRPRIOS                                                       
                                                                                
PUTDET04 CLI   TRPPARM,TRPPSUMQ    Test summary only                            
         JE    EXIT                                                             
                                                                                
         LG    GR0,TRPCCPU                                                      
         GOTOR ADJCPU              Adjust CPU time if necessary                 
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLFCPU,DUB                                                       
                                                                                
         LG    GR0,TRPCSRB                                                      
         GOTOR ADJSRB              Adjust SRB time if necessary                 
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLFSRB,DUB                                                       
                                                                                
         LG    GR0,TRPCCPU                                                      
         ALG   GR0,TRPCSRB                                                      
         GOTOR ADJTOT              Adjust total time if necessary               
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLFTOT,DUB                                                       
                                                                                
         L     R0,TRPCIOS                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLFIO,DUB                                                        
                                                                                
         L     R0,TRPQCALL                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLFCALL,DUB                                                      
                                                                                
         LG    GR0,TRPQCPU                                                      
         GOTOR ADJCPU                                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLFCPUT,DUB                                                      
                                                                                
         LG    GR0,TRPQSRB                                                      
         GOTOR ADJSRB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLFSRBT,DUB                                                      
                                                                                
         LG    GR0,TRPQCPU                                                      
         ALG   GR0,TRPQSRB                                                      
         GOTOR ADJTOT                                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLFTOTT,DUB                                                      
                                                                                
         L     R0,TRPQIOS                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RLFIOT,DUB                                                       
                                                                                
         MVC   P(RUNLITFL),RUNLITF                                              
         MVI   VPRINTER,FF                                                      
         GOTOR VPRINTER                                                         
         J     EXIT                                                             
         DROP  R2                                                               
                                                                                
GETTIM   NTR1  LABEL=*             Get CPU (R1) and SRB (R0) times              
         CALLDISP ,                                                             
         L     RF,WASCB                                                         
         LG    GR1,ASCBEJST-ASCB(RF)                                            
         SRLG  GR1,GR1,12                                                       
         LG    GR0,ASCBSRBT-ASCB(RF)                                            
         SRLG  GR0,GR0,12                                                       
         XIT1  REGS=(R0,R1)                                                     
                                                                                
ADJCPU   ICM   RF,15,CPUADJ        Adjust CPU time                              
         BZR   RE                                                               
         J     ADJUST                                                           
                                                                                
ADJSRB   ICM   RF,15,SRBADJ        Adjust SRB time                              
         BZR   RE                                                               
         J     ADJUST                                                           
                                                                                
ADJTOT   ICM   RF,15,TOTADJ        Adjust total time                            
         BZR   RE                                                               
                                                                                
ADJUST   LGR   GR1,GR0             Adjustment value is in RF                    
         MGHI  GR1,2               Input value is in GR0                        
         DSGFR GR0,RF                                                           
         LTGR  GR1,GR1                                                          
         JNP   *+8                                                              
         AGHI  GR1,1                                                            
         SRAG  GR1,GR1,1                                                        
         LGR   GR0,GR1             Output value is in GR0                       
         BR    RE                                                               
         EJECT                                                                  
EXITH    LHI   RF,2                                                             
         J     EXITCC                                                           
EXITY    LHI   RF,1                                                             
         J     EXITCC                                                           
EXITL    EQU   *                                                                
EXITN    LHI   RF,0                                                             
EXITCC   CHI   RF,1                                                             
EXIT     XIT1  ,                   General routine exit point                   
                                                                                
RETURN   BR    RE                  Pointed to by PROTOFF/PROTON                 
         EJECT                                                                  
MAPTABD  DSECT ,                   ** Map table **                              
MAPTEOTQ EQU   0                   End of table                                 
MAPTNAME DS    CL8                 Name                                         
MAPTDISP DS    AL2                 Displacement into control block              
MAPTFNDX DS    AL1                 Control block index                          
MAPTABL  EQU   *-MAPTABD           Length of table entry                        
RUNNER   CSECT ,                                                                
                                                                                
         DS    0H                                                               
MAPTAB   DS    0XL(MAPTABL)        ** Map table **                              
         DC    C'SORTER  ',AL2(RSORTER-RUNFACSD),AL1(RUNFACQ)                   
         DC    C'VALPAR  ',AL2(RVALPAR-RUNFACSD),AL1(RUNFACQ)                   
         DC    C'OPNCLO  ',AL2(ROPNCLO-RUNFACSD),AL1(RUNFACQ)                   
         DC    C'PUTMQD  ',AL2(RPUTMQD-RUNFACSD),AL1(RUNFACQ)                   
         DC    C'GOLINK  ',AL2(RGOLINK-RUNFACSD),AL1(RUNFACQ)                   
         DC    C'LOADIT  ',AL2(RLOADIT-RUNFACSD),AL1(RUNFACQ)                   
         DC    C'GPHS    ',AL2(RGPHS-RUNFACSD),AL1(RUNFACQ)                     
         DC    C'LPHS    ',AL2(RLPHS-RUNFACSD),AL1(RUNFACQ)                     
         DC    C'CALLMQ  ',AL2(RCALLMQ-RUNFACSD),AL1(RUNFACQ)                   
         DC    C'MQI     ',AL2(RMQI-RUNFACSD),AL1(RUNFACQ)                      
         DC    C'GETUID  ',AL2(RGETUID-RUNFACSD),AL1(RUNFACQ)                   
         DC    C'SETKEY  ',AL2(LP_ASETK-LP_D),AL1(LP_DQ)                        
         DC    C'ADDWMP  ',AL2(LP_AAWMP-LP_D),AL1(LP_DQ)                        
         DC    C'OUTPUT  ',AL2(LP_APUTO-LP_D),AL1(LP_DQ)                        
         DC    C'RECPUT  ',AL2(LP_ARECP-LP_D),AL1(LP_DQ)                        
         DC    C'VMAP    ',AL2(LP_AVMAP-LP_XD),AL1(LP_XDQ)                      
         DC    C'VSTR    ',AL2(LP_AVSTR-LP_XD),AL1(LP_XDQ)                      
         DC    C'VINP    ',AL2(LP_AVINP-LP_XD),AL1(LP_XDQ)                      
         DC    C'EOUT    ',AL2(LP_AEOUT-LP_XD),AL1(LP_XDQ)                      
         DC    C'PUTARY  ',AL2(LP_APARY-LP_XD),AL1(LP_XDQ)                      
         DC    C'ARYPUT  ',AL2(LP_AARYP-LP_XD),AL1(LP_XDQ)                      
         DC    C'RUNR    ',AL2(LP_ARUNR-LP_XD),AL1(LP_XDQ)                      
         DC    C'MQGET   ',AL2(LP_AMQGT-LP_XD),AL1(LP_XDQ)                      
         DC    C'RUN     ',AL2(LP_ARUN-LP_XD),AL1(LP_XDQ)                       
         DC    C'GREQ    ',AL2(LP_AGREQ-LP_XD),AL1(LP_XDQ)                      
         DC    C'GMQR    ',AL2(LP_AGMQR-LP_XD),AL1(LP_XDQ)                      
         DC    C'BLDMQD  ',AL2(LP_ABMQD-LP_XD),AL1(LP_XDQ)                      
         DC    C'FLDPUT  ',AL2(LP_AFPUT-LP_XD),AL1(LP_XDQ)                      
         DC    C'GOSVR   ',AL2(LP_AGSVR-LP_XD),AL1(LP_XDQ)                      
         DC    C'BMMI    ',AL2(LP_ABMMI-LP_XD),AL1(LP_XDQ)                      
         DC    C'LMAP    ',AL2(LP_ALMAP-LP_XD),AL1(LP_XDQ)                      
         DC    C'BREQ    ',AL2(LP_ABREQ-LP_XD),AL1(LP_XDQ)                      
         DC    C'SETSEC  ',AL2(LP_ASSEC-LP_XD),AL1(LP_XDQ)                      
         DC    C'BSRCH   ',AL2(LP_ABSRC-LP_XD),AL1(LP_XDQ)                      
         DC    C'RUNREQ  ',AL2(LP_ARREQ-LP_XD),AL1(LP_XDQ)                      
         DC    C'SMFOUT  ',AL2(RSMFOUT-RUNFACSD),AL1(RUNFACQ)                   
         DC    C'SMTP    ',AL2(RSMTP-RUNFACSD),AL1(RUNFACQ)                     
         DC    C'BUFFRIN ',AL2(RBUFFRIN-RUNFACSD),AL1(RUNFACQ)                  
         DC    C'CUREDIT ',AL2(CCUREDIT-COMFACSD),AL1(COMFACQ)                  
         DC    C'RUNIT   ',AL2(RRUNIT-RUNFACSD),AL1(RUNFACQ)                    
         DC    C'SECRET  ',AL2(CSECRET-COMFACSD),AL1(COMFACQ)                   
         DC    C'DDSIO   ',AL2(MCVDDSIO-MASTC),AL1(MASTCQ)                      
         DC    C'DADDS   ',AL2(MCVDADDS-MASTC),AL1(MASTCQ)                      
         DC    C'ISDDS   ',AL2(MCVISDDS-MASTC),AL1(MASTCQ)                      
         DC    C'DMOD000 ',AL2(MCVDMOD0-MASTC),AL1(MASTCQ)                      
         DC    C'ENQDEQ  ',AL2(MCVNQDQ-MASTC),AL1(MASTCQ)                       
         DC    C'LOCKET  ',AL2(CLOCKET-COMFACSD),AL1(COMFACQ)                   
         DC    C'RECUP   ',AL2(CRECUP-COMFACSD),AL1(COMFACQ)                    
         DC    C'DEJAVU  ',AL2(CDEJAVU-COMFACSD),AL1(COMFACQ)                   
         DC    C'GETDAY  ',AL2(CGETDAY-COMFACSD),AL1(COMFACQ)                   
         DC    C'DATCON  ',AL2(CDATCON-COMFACSD),AL1(COMFACQ)                   
         DC    C'DATCONX ',AL2(CDATCONX-COMFACSD),AL1(COMFACQ)                  
         DC    C'GETPROF ',AL2(CGETPROF-COMFACSD),AL1(COMFACQ)                  
         DC    C'PERVERT ',AL2(CPERVERT-COMFACSD),AL1(COMFACQ)                  
         DC    C'DICTATE ',AL2(CDICTATE-COMFACSD),AL1(COMFACQ)                  
         DC    C'HELLO   ',AL2(CHELLO-COMFACSD),AL1(COMFACQ)                    
         DC    C'CALLOV  ',AL2(CCALLOV-COMFACSD),AL1(COMFACQ)                   
         DC    C'GETTXT  ',AL2(CGETTXT-COMFACSD),AL1(COMFACQ)                   
         DC    C'SCANNER ',AL2(CSCANNER-COMFACSD),AL1(COMFACQ)                  
         DC    C'CASHVAL ',AL2(CCASHVAL-COMFACSD),AL1(COMFACQ)                  
         DC    C'ADDAY   ',AL2(CADDAY-COMFACSD),AL1(COMFACQ)                    
         DC    C'DATVAL  ',AL2(CDATVAL-COMFACSD),AL1(COMFACQ)                   
         DC    C'HEXIN   ',AL2(CHEXIN-COMFACSD),AL1(COMFACQ)                    
         DC    C'HEXOUT  ',AL2(CHEXOUT-COMFACSD),AL1(COMFACQ)                   
         DC    C'BLDCUR  ',AL2(CBLDCUR-COMFACSD),AL1(COMFACQ)                   
         DC    C'MQRPT   ',AL2(CMQRPT-COMFACSD),AL1(COMFACQ)                    
*&&UK*&& DC    C'EUREKA  ',AL2(CEUREKA-COMFACSD),AL1(COMFACQ)                   
*&&UK*&& DC    C'LIMACC  ',AL2(CLIMACC-COMFACSD),AL1(COMFACQ)                   
*&&UK*&& DC    C'GETUNV  ',AL2(AGETUNV-MEDFACSD),AL1(SYSFACQ)                   
*&&UK*&& DC    C'GETAUD  ',AL2(AGETAUD-MEDFACSD),AL1(SYSFACQ)                   
*&&UK*&& DC    C'GETDPT  ',AL2(AGETDPT-MEDFACSD),AL1(SYSFACQ)                   
*&&UK*&& DC    C'SUGGEST ',AL2(ASUGGEST-MEDFACSD),AL1(SYSFACQ)                  
*&&UK*&& DC    C'GETCPT  ',AL2(AGETCPT-MEDFACSD),AL1(SYSFACQ)                   
*&&UK*&& DC    C'GETDAY  ',AL2(AGETDAY-MEDFACSD),AL1(SYSFACQ)                   
*&&UK*&& DC    C'GETPROD ',AL2(AGETPROD-MEDFACSD),AL1(SYSFACQ)                  
*&&UK*&& DC    C'MENXTSER',AL2(ANEXTSER-MEDFACSD),AL1(SYSFACQ)                  
*&&UK*&& DC    C'MDMGR   ',AL2(ADATAMGR-MEDFACSD),AL1(SYSFACQ)                  
*&&UK*&& DC    C'BINSRCH ',AL2(ABINSRCH-MEDFACSD),AL1(SYSFACQ)                  
MAPTABX  DS    AL1(MAPTEOTQ)                                                    
MAPTABN  EQU   (*-MAPTAB)/MAPTABL                                               
                                                                                
         DS    0H                                                               
       ++INCLUDE FACIDTAB                                                       
                                                                                
         DS    0H                                                               
T00ATAB  DS    0CL9                ** Core resident phase names **              
         DC    AL1(QDDLINK),CL8'DDLINK'                                         
*&&US*&& DC    AL1(QBOOKVAL),CL8'BOOKVAL'                                       
*&&US*&& DC    AL1(QCENTER),CL8'CENTER'                                         
*&&US*&& DC    AL1(QCHOPPER),CL8'CHOPPER'                                       
*&&US*&& DC    AL1(QDAYVAL),CL8'DAYVAL'                                         
*&&US*&& DC    AL1(QDEMCON),CL8'DEMCON'                                         
*&&US*&& DC    AL1(QDEMEX),CL8'DEMEX'                                           
*&&US*&& DC    AL1(QDEMOTAB),CL8'DEMOTAB'                                       
*&&US*&& DC    AL1(QDEMVAL),CL8'DEMVAL'                                         
*&&US*&& DC    AL1(QREDEMUP),CL8'REDEMUP'                                       
*&&US*&& DC    AL1(QINVEDIT),CL8'INVEDIT'                                       
*&&US*&& DC    AL1(QCFMIO),CL8'CFMIO'                                           
*&&US*&& DC    AL1(QSPOOL),CL8'SPOOL'                                           
*&&US*&& DC    AL1(QSQUASH),CL8'SQUASH'                                         
*&&US*&& DC    AL1(QTIMVAL),CL8'TIMVAL'                                         
*&&US*&& DC    AL1(QUNDAY),CL8'UNDAY'                                           
*&&US*&& DC    AL1(QDAYUNPK),CL8'DAYUNPK'                                       
*&&US*&& DC    AL1(QUNDRLIN),CL8'UNDERLIN'                                      
*&&US*&& DC    AL1(QUNTIME),CL8'UNTIME'                                         
*&&US*&& DC    AL1(QXSORT),CL8'XSORT'                                           
*&&US*&& DC    AL1(QUPVAL),CL8'UPVAL'                                           
*&&US*&& DC    AL1(QCLPACK),CL8'CLPACK'                                         
*&&US*&& DC    AL1(QCLUNPK),CL8'CLUNPK'                                         
*&&US*&& DC    AL1(QNETUNIV),CL8'NETUNIV'                                       
*&&US*&& DC    AL1(QNETWEEK),CL8'NETWEEK'                                       
*&&US*&& DC    AL1(QNETGTSP),CL8'NETGTSP'                                       
*&&US*&& DC    AL1(QNEUTIL),CL8'NEUTIL'                                         
*&&US*&& DC    AL1(QPARSNIP),CL8'PARSNIP'                                       
*&&US*&& DC    AL1(QMSPACK),CL8'MSPACK'                                         
*&&US*&& DC    AL1(QMSUNPK),CL8'MSUNPK'                                         
*&&US*&& DC    AL1(QGETBROD),CL8'GETBROD'                                       
*&&US*&& DC    AL1(QGETBRD),CL8'GETBRD'                                         
*&&US*&& DC    AL1(QNETUNBK),CL8'NETUNBK'                                       
*&&US*&& DC    AL1(QDARREPS),CL8'DARREPS'                                       
*&&US*&& DC    AL1(QGETDEM1),CL8'GETDEM1'                                       
*&&US*&& DC    AL1(QGETDEM2),CL8'GETDEM2'                                       
*&&US*&& DC    AL1(QSPDEMUP),CL8'SPDEMUP'                                       
*&&US*&& DC    AL1(QNETSPVL),CL8'NETSPVL'                                       
*&&US*&& DC    AL1(QSPGTIUN),CL8'SPGTIUN'                                       
*&&US*&& DC    AL1(QSPOON),CL8'SPOON'                                           
*&&US*&& DC    AL1(QDEFINE),CL8'DEFINE'                                         
*&&US*&& DC    AL1(QNETIO),CL8'NETIO'                                           
*&&US*&& DC    AL1(QNETVALU),CL8'NETVALU'                                       
*&&US*&& DC    AL1(QREGTIUN),CL8'REGTIUN'                                       
*&&US*&& DC    AL1(QSPGETIUN),CL8'SPGETIUN'                                     
*&&US*&& DC    AL1(QGETBUY),CL8'GETBUY'                                         
*&&US*&& DC    AL1(QWRKIO),CL8'WRKIO'                                           
*&&US*&& DC    AL1(QGETBTAB),CL8'GETBTAB'                                       
*&&US*&& DC    AL1(QGENCON),CL8'GENCON'                                         
*&&US*&& DC    AL1(QDICTCON),CL8'DICTCON'                                       
*&&US*&& DC    AL1(QGETNUN),CL8'GETNUN'                                         
*&&US*&& DC    AL1(QGETHUT),CL8'GETHUT'                                         
*&&US*&& DC    AL1(QNETGOAL),CL8'NETGOAL'                                       
*&&US*&& DC    AL1(QDRIVAL),CL8'DRIVAL'                                         
*&&US*&& DC    AL1(QGENPRG),CL8'GENPRG'                                         
*&&US*&& DC    AL1(QOFFICER),CL8'OFFICER'                                       
*&&US*&& DC    AL1(QDRONE),CL8'DRONE'                                           
*&&US*&& DC    AL1(QDRIVER),CL8'DRIVER'                                         
*&&US*&& DC    AL1(QBUFFOON),CL8'BUFFOON'                                       
*&&US*&& DC    AL1(QPRVAL),CL8'PRVAL'                                           
*&&US*&& DC    AL1(QMEDGEN),CL8'MEDGEN'                                         
*&&US*&& DC    AL1(QPRVAL2),CL8'PRVAL2'                                         
*&&US*&& DC    AL1(QSPACNVL),CL8'SPACNVL'                                       
*&&US*&& DC    AL1(QSPOTIO),CL8'SPOTIO'                                         
*&&US*&& DC    AL1(QSPOTDRV),CL8'SPOTDRV'                                       
*&&US*&& DC    AL1(QRANSID),CL8'RANSID'                                         
*&&US*&& DC    AL1(QSPOTBUY),CL8'SPOTBUY'                                       
*&&US*&& DC    AL1(QSPOTBK),CL8'SPOTBK'                                         
*&&US*&& DC    AL1(QNEWRIGN),CL8'NEWRIGN'                                       
*&&US*&& DC    AL1(QWBSIO),CL8'WBSIO'                                           
*&&US*&& DC    AL1(QPRNTIO),CL8'PRNTIO'                                         
*&&US*&& DC    AL1(QSPOTMKR),CL8'SPOTMKR'                                       
*&&US*&& DC    AL1(QPRWRIGN),CL8'PRWRIGN'                                       
*&&US*&& DC    AL1(QQSORT),CL8'QSORT'                                           
*&&US*&& DC    AL1(QSPOTGL),CL8'SPOTGL'                                         
*&&US*&& DC    AL1(QSPWRIGN),CL8'SPWRIGN'                                       
*&&US*&& DC    AL1(QPODDRIV),CL8'PODDRIV'                                       
*&&US*&& DC    AL1(QPDWRIGN),CL8'PDWRIGN'                                       
*&&US*&& DC    AL1(QSPOTSLK),CL8'SPOTSLK'                                       
*&&US*&& DC    AL1(QSPSLNTB),CL8'SPSLNTB'                                       
*&&US*&& DC    AL1(QTSAR),CL8'TSAR'                                             
*&&US*&& DC    AL1(QSPADINT),CL8'SPADINT'                                       
*&&US*&& DC    AL1(QGETRATE),CL8'GETRATE'                                       
*&&US*&& DC    AL1(QOFFAL),CL8'OFFAL'                                           
*&&US*&& DC    AL1(QADDTRN),CL8'ADDTRN'                                         
*&&US*&& DC    AL1(QGETBANK),CL8'GETBANK'                                       
*&&US*&& DC    AL1(QSETLOCK),CL8'SETLOCK'                                       
*&&US*&& DC    AL1(QRFPIO),CL8'RFPIO'                                           
*&&US*&& DC    AL1(QSTAVAL),CL8'STAVAL'                                         
*&&US*&& DC    AL1(QPRORATA),CL8'PRORATA'                                       
*&&US*&& DC    AL1(QWLEVEL),CL8'WLEVEL'                                         
*&&US*&& DC    AL1(QPSTVAL),CL8'PSTVAL'                                         
*&&US*&& DC    AL1(QBATFAC4),CL8'BATFAC4'                                       
*&&US*&& DC    AL1(QBATFAC3),CL8'BATFAC3'                                       
*&&US*&& DC    AL1(QBATFAC2),CL8'BATFAC2'                                       
*&&US*&& DC    AL1(QBATFAC1),CL8'BATFAC1'                                       
*&&US*&& DC    AL1(QNODIO),CL8'NODIO'                                           
*&&US*&& DC    AL1(QEDITOR),CL8'EDITOR'                                         
*&&US*&& DC    AL1(QMOBILE),CL8'MOBILE'                                         
*&&US*&& DC    AL1(QREQTWA),CL8'REQTWA'                                         
*&&US*&& DC    AL1(QSPGETBU),CL8'SPGETBU'                                       
*&&US*&& DC    AL1(QDBLBOOK),CL8'DBLBOOK'                                       
*&&US*&& DC    AL1(QPWCALC),CL8'PWCALC'                                         
*&&US*&& DC    AL1(QSTAPACK),CL8'STAPACK'                                       
*&&US*&& DC    AL1(QBLDMGA),CL8'BLDMGA'                                         
*&&US*&& DC    AL1(QGETCTA),CL8'GETCTA'                                         
*&&US*&& DC    AL1(QTSAROFF),CL8'TSAROFF'                                       
*&&US*&& DC    AL1(QLOCKET),CL8'LOCKET'                                         
*&&US*&& DC    AL1(QBLDMGE),CL8'BLDMGE'                                         
*&&US*&& DC    AL1(QACCGEN),CL8'ACCGEN'                                         
*&&US*&& DC    AL1(QPROGEN),CL8'PROGEN'                                         
*&&US*&& DC    AL1(QGETOPT),CL8'GETOPT'                                         
*&&US*&& DC    AL1(QJOBBER),CL8'JOBBER'                                         
*&&US*&& DC    AL1(QTASYSIO),CL8'TASYSIO'                                       
*&&US*&& DC    AL1(QTASYSVL),CL8'TASYSVL'                                       
*&&US*&& DC    AL1(QTASYSTB),CL8'TASYSTB'                                       
*&&US*&& DC    AL1(QTAREPGN),CL8'TAREPGN'                                       
*&&US*&& DC    AL1(QTASYSDR),CL8'TASYSDR'                                       
*&&US*&& DC    AL1(QDROOL),CL8'DROOL'                                           
*&&US*&& DC    AL1(QTASYSES),CL8'TASYSES'                                       
*&&US*&& DC    AL1(QTASYSCA),CL8'TASYSCA'                                       
*&&US*&& DC    AL1(QTACONCU),CL8'TACONCU'                                       
*&&US*&& DC    AL1(QTACONPR),CL8'TACONPR'                                       
*&&US*&& DC    AL1(QCABLETB),CL8'CABLETB'                                       
*&&US*&& DC    AL1(QPADDLE),CL8'PADDLE'                                         
*&&US*&& DC    AL1(QSCROLL),CL8'SCROLL'                                         
*&&US*&& DC    AL1(QPRWROFF),CL8'PRWROFF'                                       
*&&US*&& DC    AL1(QPRHELP),CL8'PRHELP'                                         
*&&US*&& DC    AL1(QREFETCH),CL8'REFETCH'                                       
*&&US*&& DC    AL1(QCASHIER),CL8'CASHIER'                                       
*&&US*&& DC    AL1(QREVAL),CL8'REVAL'                                           
*&&US*&& DC    AL1(QREVAL2),CL8'REVAL2'                                         
*&&US*&& DC    AL1(QRENWIO),CL8'RENWIO'                                         
*&&US*&& DC    AL1(QRENWOFF),CL8'RENWOFF'                                       
*&&US*&& DC    AL1(QRENWTER),CL8'RENWTER'                                       
*&&US*&& DC    AL1(QGETINS),CL8'GETINS'                                         
*&&US*&& DC    AL1(QREPFACS),CL8'REPFACS'                                       
*&&US*&& DC    AL1(QRENWGEN),CL8'RENWGEN'                                       
*&&US*&& DC    AL1(QSOFDAT),CL8'SOFDAT'                                         
*&&US*&& DC    AL1(QEDIMAP),CL8'EDIMAP'                                         
*&&US*&& DC    AL1(QSPAUTH),CL8'SPAUTH'                                         
*&&US*&& DC    AL1(QPUBVAL),CL8'PUBVAL'                                         
*&&US*&& DC    AL1(QPUBEDIT),CL8'PUBEDIT'                                       
*&&US*&& DC    AL1(QPPGETCG),CL8'PPGETCG'                                       
*&&US*&& DC    AL1(QPGETADR),CL8'PGETADR'                                       
*&&US*&& DC    AL1(QRCPACK),CL8'RCPACK'                                         
*&&US*&& DC    AL1(QPOSTWRK),CL8'POSTWRK'                                       
*&&US*&& DC    AL1(QREKFACS),CL8'REKFACS'                                       
*&&US*&& DC    AL1(QSUPERD),CL8'SUPERD'                                         
*&&US*&& DC    AL1(QGETCAP),CL8'GETCAP'                                         
*&&US*&& DC    AL1(QBLDMGN),CL8'BLDMGN'                                         
*&&US*&& DC    AL1(QGENBAT),CL8'GENBAT'                                         
*&&US*&& DC    AL1(QDDISP),CL8'DDISP'                                           
*&&US*&& DC    AL1(QDEMTABS),CL8'DEMTABS'                                       
*&&US*&& DC    AL1(QDEMTBOF),CL8'DEMTBOF'                                       
*&&US*&& DC    AL1(QDMAST),CL8'DMAST'                                           
*&&US*&& DC    AL1(QDFORM),CL8'DFORM'                                           
*&&US*&& DC    AL1(QDNAME),CL8'DNAME'                                           
*&&US*&& DC    AL1(QDCODE),CL8'DCODE'                                           
*&&US*&& DC    AL1(QDCONT),CL8'DCONT'                                           
*&&US*&& DC    AL1(QDADJS),CL8'DADJS'                                           
*&&US*&& DC    AL1(QDEMOVAL),CL8'DEMOVAL'                                       
*&&US*&& DC    AL1(QDEMOMTH),CL8'DEMOMTH'                                       
*&&US*&& DC    AL1(QDEMEL),CL8'DEMEL'                                           
*&&US*&& DC    AL1(QDEMAINT),CL8'DEMAINT'                                       
*&&US*&& DC    AL1(QDEMAND),CL8'DEMAND'                                         
*&&US*&& DC    AL1(QDEMADDR),CL8'DEMADDR'                                       
*&&US*&& DC    AL1(QDEMOUT),CL8'DEMOUT'                                         
*&&US*&& DC    AL1(QDEMOCON),CL8'DEMOCON'                                       
*&&US*&& DC    AL1(QDEMAND1),CL8'DEMAND1'                                       
*&&US*&& DC    AL1(QFALINK),CL8'FALINK'                                         
*&&US*&& DC    AL1(QREPORT),CL8'REPORT'                                         
*&&US*&& DC    AL1(QGETIDS),CL8'GETIDS'                                         
*&&US*&& DC    AL1(QPERVAL),CL8'PERVAL'                                         
*&&US*&& DC    AL1(QGENIDS),CL8'GENIDS'                                         
*&&US*&& DC    AL1(QCORENDX),CL8'CORENDX'                                       
*&&UK*&& DC    AL1(QCENTER),CL8'CENTER'                                         
*&&UK*&& DC    AL1(QCHOPPER),CL8'CHOPPER'                                       
*&&UK*&& DC    AL1(QDAYVAL),CL8'DAYVAL'                                         
*&&UK*&& DC    AL1(QSPOOL),CL8'SPOOL'                                           
*&&UK*&& DC    AL1(QSQUASH),CL8'SQUASH'                                         
*&&UK*&& DC    AL1(QTIMVAL),CL8'TIMVAL'                                         
*&&UK*&& DC    AL1(QUNDAY),CL8'UNDAY'                                           
*&&UK*&& DC    AL1(QUNDRLIN),CL8'UNDRLIN'                                       
*&&UK*&& DC    AL1(QUNTIME),CL8'UNTIME'                                         
*&&UK*&& DC    AL1(QXSORT),CL8'XSORT'                                           
*&&UK*&& DC    AL1(QPARSNIP),CL8'PARSNIP'                                       
*&&UK*&& DC    AL1(QCRATABS),CL8'CRATABS'                                       
*&&UK*&& DC    AL1(QSPOON),CL8'SPOON'                                           
*&&UK*&& DC    AL1(QTABOFF),CL8'TABOFF'                                         
*&&UK*&& DC    AL1(QWRKIO),CL8'WRKIO'                                           
*&&UK*&& DC    AL1(QGENCON),CL8'GENCON'                                         
*&&UK*&& DC    AL1(QDICTCON),CL8'DICTCON'                                       
*&&UK*&& DC    AL1(QDRIVAL),CL8'DRIVAL'                                         
*&&UK*&& DC    AL1(QGENPRG),CL8'GENPRG'                                         
*&&UK*&& DC    AL1(QOFFICER),CL8'OFFICER'                                       
*&&UK*&& DC    AL1(QDRONE),CL8'DRONE'                                           
*&&UK*&& DC    AL1(QDRIVER),CL8'DRIVER'                                         
*&&UK*&& DC    AL1(QGETEST),CL8'GETEST'                                         
*&&UK*&& DC    AL1(QGETUNV),CL8'GETUNV'                                         
*&&UK*&& DC    AL1(QGETAUD),CL8'GETAUD'                                         
*&&UK*&& DC    AL1(QGETUNIV),CL8'GETUNIV'                                       
*&&UK*&& DC    AL1(QGETTARG),CL8'GETTARG'                                       
*&&UK*&& DC    AL1(QGETPGM),CL8'GETPGM'                                         
*&&UK*&& DC    AL1(QBUDGETT),CL8'BUDGETT'                                       
*&&UK*&& DC    AL1(QBUDGETP),CL8'BUDGETP'                                       
*&&UK*&& DC    AL1(QDEMAND),CL8'DEMAND'                                         
*&&UK*&& DC    AL1(QGETPCNT),CL8'GETPCNT'                                       
*&&UK*&& DC    AL1(QDEMVALS),CL8'DEMVALS'                                       
*&&UK*&& DC    AL1(QGETWKS),CL8'GETWKS'                                         
*&&UK*&& DC    AL1(QTVDIR),CL8'TVDIR'                                           
*&&UK*&& DC    AL1(QTVLIST),CL8'TVLIST'                                         
*&&UK*&& DC    AL1(QBUYHIST),CL8'BUYHIST'                                       
*&&UK*&& DC    AL1(QMDAYVAL),CL8'MDAYVAL'                                       
*&&UK*&& DC    AL1(QSTABLE),CL8'STABLE'                                         
*&&UK*&& DC    AL1(QNETWORK),CL8'NETWORK'                                       
*&&UK*&& DC    AL1(QBUYTER),CL8'BUYTER'                                         
*&&UK*&& DC    AL1(QGETDPT),CL8'GETDPT'                                         
*&&UK*&& DC    AL1(QGETEQIV),CL8'GETEQIV'                                       
*&&UK*&& DC    AL1(QQSORT),CL8'QSORT'                                           
*&&UK*&& DC    AL1(QGTVDIR),CL8'GTVDIR'                                         
*&&UK*&& DC    AL1(QGTVLIST),CL8'GTVLIST'                                       
*&&UK*&& DC    AL1(QBLDMED),CL8'BLDMED'                                         
*&&UK*&& DC    AL1(QSUGGEST),CL8'SUGGEST'                                       
*&&UK*&& DC    AL1(QTMPACK),CL8'TMPACK'                                         
*&&UK*&& DC    AL1(QTMUNPK),CL8'TMUNPK'                                         
*&&UK*&& DC    AL1(QCOMPOSE),CL8'COMPOSE'                                       
*&&UK*&& DC    AL1(QDEFORM),CL8'DEFORM'                                         
*&&UK*&& DC    AL1(QDEPOSIT),CL8'DEPOSIT'                                       
*&&UK*&& DC    AL1(QBUSMAN),CL8'BUSMAN'                                         
*&&UK*&& DC    AL1(QSPROUT),CL8'SPROUT'                                         
*&&UK*&& DC    AL1(QDEMPARS),CL8'DEMPARS'                                       
*&&UK*&& DC    AL1(QGETCPT),CL8'GETCPT'                                         
*&&UK*&& DC    AL1(QTSAR),CL8'TSAR'                                             
*&&UK*&& DC    AL1(QMINIBUY),CL8'MINIBUY'                                       
*&&UK*&& DC    AL1(QTRAVAIL),CL8'TRAVAIL'                                       
*&&UK*&& DC    AL1(QSPCVAL),CL8'SPCVAL'                                         
*&&UK*&& DC    AL1(QOFFAL),CL8'OFFAL'                                           
*&&UK*&& DC    AL1(QADDTRN),CL8'ADDTRN'                                         
*&&UK*&& DC    AL1(QDECADE),CL8'DECADE'                                         
*&&UK*&& DC    AL1(QJOBUFF),CL8'JOBUFF'                                         
*&&UK*&& DC    AL1(QSETLOCK),CL8'SETLOCK'                                       
*&&UK*&& DC    AL1(QRFPIO),CL8'RFPIO'                                           
*&&UK*&& DC    AL1(QPRORATA),CL8'PRORATA'                                       
*&&UK*&& DC    AL1(QMBGENLK),CL8'MBGENLK'                                       
*&&UK*&& DC    AL1(QPSTVAL),CL8'PSTVAL'                                         
*&&UK*&& DC    AL1(QBATFAC4),CL8'BATFAC4'                                       
*&&UK*&& DC    AL1(QBATFAC3),CL8'BATFAC3'                                       
*&&UK*&& DC    AL1(QBATFAC2),CL8'BATFAC2'                                       
*&&UK*&& DC    AL1(QBATFAC1),CL8'BATFAC1'                                       
*&&UK*&& DC    AL1(QNODIO),CL8'NODIO'                                           
*&&UK*&& DC    AL1(QEDITOR),CL8'EDITOR'                                         
*&&UK*&& DC    AL1(QCRFTABS),CL8'CRFTABS'                                       
*&&UK*&& DC    AL1(QMINIO),CL8'MINIO'                                           
*&&UK*&& DC    AL1(QLINUP),CL8'LINUP'                                           
*&&UK*&& DC    AL1(QCODEX),CL8'CODEX'                                           
*&&UK*&& DC    AL1(QAFTABS),CL8'AFTABS'                                         
*&&UK*&& DC    AL1(QTSAROFF),CL8'TSAROFF'                                       
*&&UK*&& DC    AL1(QLOCKET),CL8'LOCKET'                                         
*&&UK*&& DC    AL1(QACCIO),CL8'ACCIO'                                           
*&&UK*&& DC    AL1(QACCGEN),CL8'ACCGEN'                                         
*&&UK*&& DC    AL1(QGETOPT),CL8'GETOPT'                                         
*&&UK*&& DC    AL1(QJOBBER),CL8'JOBBER'                                         
*&&UK*&& DC    AL1(QGETQRP),CL8'GETQRP'                                         
*&&UK*&& DC    AL1(QTVDIR2),CL8'TVDIR2'                                         
*&&UK*&& DC    AL1(QFTVLIST),CL8'FTVLIST'                                       
*&&UK*&& DC    AL1(QGETDEAL),CL8'GETDEAL'                                       
*&&UK*&& DC    AL1(QDROOL),CL8'DROOL'                                           
*&&UK*&& DC    AL1(QDERUNV),CL8'DERUNV'                                         
*&&UK*&& DC    AL1(QTYPIST),CL8'TYPIST'                                         
*&&UK*&& DC    AL1(QGETWGTS),CL8'GETWGTS'                                       
*&&UK*&& DC    AL1(QMBVAL),CL8'MBVAL'                                           
*&&UK*&& DC    AL1(QMBVAL1),CL8'MBVAL1'                                         
*&&UK*&& DC    AL1(QMBVAL2),CL8'MBVAL2'                                         
*&&UK*&& DC    AL1(QMBSPVAL),CL8'MBSPVAL'                                       
*&&UK*&& DC    AL1(QMBLVAL),CL8'MBLVAL'                                         
*&&UK*&& DC    AL1(QMBQED),CL8'MBQED'                                           
*&&UK*&& DC    AL1(QMBNMVAL),CL8'MBNMVAL'                                       
*&&UK*&& DC    AL1(QMBQCOMP),CL8'MBQCOMP'                                       
*&&UK*&& DC    AL1(QMBDATER),CL8'MBDATER'                                       
*&&UK*&& DC    AL1(QMBDCALC),CL8'MBDCALC'                                       
*&&UK*&& DC    AL1(QMBLOOK),CL8'MBLOOK'                                         
*&&UK*&& DC    AL1(QMBMECNV),CL8'MBMECNV'                                       
*&&UK*&& DC    AL1(QMBVAL3),CL8'MBVAL3'                                         
*&&UK*&& DC    AL1(QMBVAL4),CL8'MBVAL4'                                         
*&&UK*&& DC    AL1(QPADDLE),CL8'PADDLE'                                         
*&&UK*&& DC    AL1(QSCROLL),CL8'SCROLL'                                         
*&&UK*&& DC    AL1(QSOFDAT),CL8'SOFDAT'                                         
*&&UK*&& DC    AL1(QEUREKA),CL8'EUREKA'                                         
*&&UK*&& DC    AL1(QGETRAT2),CL8'GETRAT2'                                       
*&&UK*&& DC    AL1(QCCRTABS),CL8'CCRTABS'                                       
*&&UK*&& DC    AL1(QGETCAP),CL8'GETCAP'                                         
*&&UK*&& DC    AL1(QGENBAT),CL8'GENBAT'                                         
*&&UK*&& DC    AL1(QCONVERT),CL8'CONVERT'                                       
*&&UK*&& DC    AL1(QLINKIO),CL8'LINKIO'                                         
*&&UK*&& DC    AL1(QGENNEW),CL8'GENNEW'                                         
*&&UK*&& DC    AL1(QFALINK),CL8'FALINK'                                         
*&&UK*&& DC    AL1(QGENERAL),CL8'GENERAL'                                       
*&&UK*&& DC    AL1(QSEARCH),CL8'SEARCH'                                         
*&&UK*&& DC    AL1(QGETIDS),CL8'GETIDS'                                         
*&&UK*&& DC    AL1(QREPORT),CL8'REPORT'                                         
*&&UK*&& DC    AL1(QPERVAL),CL8'PERVAL'                                         
*&&UK*&& DC    AL1(QGENIDS),CL8'GENIDS'                                         
*&&UK*&& DC    AL1(QREQTWA),CL8'REQTWA'                                         
*&&UK*&& DC    AL1(QSYSCON),CL8'SYSCON'                                         
*&&UK*&& DC    AL1(QCRINDEX),CL8'CRINDEX'                                       
T00ATABN EQU   (*-T00ATAB)/L'T00ATAB                                            
                                                                                
         DS    0H                                                               
CANTAB   DS    0XL21               ** Table of cancel codes **                  
         DC    AL1(1),CL20'Operation exception'                                 
         DC    AL1(2),CL20'Prvl. oper exception'                                
         DC    AL1(3),CL20'Execution exception'                                 
         DC    AL1(4),CL20'Protection exception'                                
         DC    AL1(5),CL20'Addressing exception'                                
         DC    AL1(6),CL20'Specification excptn'                                
         DC    AL1(7),CL20'Data exception'                                      
         DC    AL1(FE),CL20'User ABEND'                                         
CANTABX  DC    AL1(0),CL20'Unknown interruption'                                
                                                                                
         DS    0H                                                               
TQETAB   DS    0XL31               ** Table of error codes **                   
         DC    AL1(TQEGOOD),CL30'None'                                          
         DC    AL1(TQENOSVR),CL30'Server not available'                         
         DC    AL1(TQENOQUE),CL30'No free queue entry'                          
         DC    AL1(TQEIUID),CL30'Invalid user-id'                               
         DC    AL1(TQEISYS),CL30'Invalid system'                                
         DC    AL1(TQEIWRK),CL30'Invalid worker key'                            
         DC    AL1(TQEIOCP),CL30'I/O count exceeded'                            
         DC    AL1(TQECPUE),CL30'CPU time exceeded'                             
         DC    AL1(TQEPAGE),CL30'Page count exceeded'                           
         DC    AL1(TQETIME),CL30'TCB time exceeded'                             
         DC    AL1(TQEPCHK),CL30'Program check'                                 
         DC    AL1(TQECANC),CL30'Request canceled by user'                      
         DC    AL1(TQEDDLY),CL30'Deadly embrace'                                
         DC    AL1(TQELOOP),CL30'May be looping'                                
         DC    AL1(TQEOPER),CL30'Operator cancelled'                            
         DC    AL1(TQELKTFL),CL30'Lock table full'                              
         DC    AL1(LP_QEIOS),CL30'Exceeded I/O count'                           
         DC    AL1(LP_QEREC),CL30'Exceeded record count'                        
         DC    AL1(LP_QESEG),CL30'Exceeded segment count'                       
         DC    AL1(LP_QESIZ),CL30'Exceeded output file size'                    
         DC    AL1(LP_QECPU),CL30'Exceeded CPU value'                           
TQETABX  DC    AL1(0),CL30'Unknown'                                             
                                                                                
         DS    0H                                                               
VALTAB   DS    0XL(VALTABL)        ** Table of parameter words **               
         DC    C'UPDATIVE    ',AL4(VALUPDT)                                     
         DC    C'SERVER      ',AL4(VALSVR1)                                     
         DC    C'SERVER1     ',AL4(VALSVR1)                                     
         DC    C'SERVER2     ',AL4(VALSVR2)                                     
         DC    C'SERVER3     ',AL4(VALSVR3)                                     
         DC    C'SERVER4     ',AL4(VALSVR4)                                     
         DC    C'SERVER5     ',AL4(VALSVR5)                                     
         DC    C'SERVER6     ',AL4(VALSVR6)                                     
         DC    C'SERVER7     ',AL4(VALSVR7)                                     
         DC    C'SERVER8     ',AL4(VALSVR8)                                     
         DC    C'SERVER9     ',AL4(VALSVR9)                                     
         DC    C'SERVER10    ',AL4(VALSVRA)                                     
         DC    C'SERVER11    ',AL4(VALSVRB)                                     
         DC    C'SERVER12    ',AL4(VALSVRC)                                     
         DC    C'SERVER13    ',AL4(VALSVRD)                                     
         DC    C'SERVER14    ',AL4(VALSVRE)                                     
         DC    C'SERVER15    ',AL4(VALSVRF)                                     
         DC    C'SERVER16    ',AL4(VALSVRG)                                     
         DC    C'DSPACE      ',AL4(VALDSPC)                                     
         DC    C'RTPTIME     ',AL4(VALRTPT)                                     
         DC    C'RTPPAGES    ',AL4(VALRTPP)                                     
         DC    C'RTPEXCP     ',AL4(VALRTPE)                                     
         DC    C'RTPCPU      ',AL4(VALRTPC)                                     
         DC    C'LOOPSECS    ',AL4(VALLOOP)                                     
         DC    C'SWAPABLE    ',AL4(VALSWAP)                                     
         DC    C'WRITE       ',AL4(VALWRTE)                                     
         DC    C'STATE       ',AL4(VALSTAT)                                     
         DC    C'MAXPOPS     ',AL4(VALPOPS)                                     
         DC    C'MAXSEGS     ',AL4(VALSEGS)                                     
         DC    C'MAXQTIME    ',AL4(VALQTIM)                                     
         DC    C'MAXDUMPS    ',AL4(VALMDMP)                                     
         DC    C'DDLYRTRY    ',AL4(VALDDLR)                                     
         DC    C'DDLYWT1     ',AL4(VALDDL1)                                     
         DC    C'DDLYWTMX    ',AL4(VALDDLM)                                     
         DC    C'AVGQTIME    ',AL4(VALATIM)                                     
         DC    C'SAMPLE      ',AL4(VALSAMP)                                     
         DC    C'OPMESS      ',AL4(VALOPMS)                                     
         DC    C'OPREPLY     ',AL4(VALOPRP)                                     
         DC    C'INFOMESS    ',AL4(VALIFMS)                                     
         DC    C'NOOP        ',AL4(VALSYNO)                                     
         DC    C'OP          ',AL4(VALSYOP)                                     
         DC    C'CLOSE       ',AL4(VALCLSE)                                     
         DC    C'OPEN        ',AL4(VALOPEN)                                     
         DC    C'ABEND       ',AL4(VALABND)                                     
         DC    C'SLOW        ',AL4(VALSLOW)                                     
         DC    C'FACPAK      ',AL4(VALFPAK)                                     
         DC    C'AGENCY      ',AL4(VALAGID)                                     
         DC    C'SEGSIZE     ',AL4(VALSEGC)                                     
         DC    C'SEGRECS     ',AL4(VALSEGR)                                     
         DC    C'JESMAIL     ',AL4(VALJESM)                                     
*&&UK*&& DC    C'MEDDSPC     ',AL4(VALMEDS)                                     
         DC    C'QM          ',AL4(VALMQQM)                                     
         DC    C'QMAINTR     ',AL4(VALQMNT)                                     
         DC    C'QMLIST      ',AL4(VALQLST)                                     
         DC    C'GLOBAL      ',AL4(VALGLOB)                                     
         DC    C'LOCAL       ',AL4(VALLOCL)                                     
         DC    C'EDIIQ       ',AL4(VALMQEI)                                     
         DC    C'EDIOQ       ',AL4(VALMQEO)                                     
         DC    C'MQTRACE     ',AL4(VALMQTR)                                     
         DC    C'MQIBUFFL    ',AL4(VALMQIL)                                     
         DC    C'MQOBUFFL    ',AL4(VALMQOL)                                     
         DC    C'MQACK-02    ',AL4(VALMQA)                                      
         DC    C'MQRUNQ      ',AL4(VALMQRQ)                                     
         DC    C'MAXIO       ',AL4(VALMIOS)                                     
         DC    C'BIGIO       ',AL4(VALBIOS)                                     
         DC    C'BIGSEG      ',AL4(VALBSEG)                                     
         DC    C'BIGCPU      ',AL4(VALBCPU)                                     
         DC    C'OFILHLQ     ',AL4(VALOHLQ)                                     
         DC    C'OFILPRI     ',AL4(VALOPRI)                                     
         DC    C'OFILSEC     ',AL4(VALOSEC)                                     
*                                  ** Test options **                           
         DC    C'TRAPDIR     ',AL4(VALTDIR)                                     
         DC    C'TRAPFIL     ',AL4(VALTFIL)                                     
         DC    C'TRAPVIS     ',AL4(VALTVIS)                                     
         DC    C'TRAPAPP     ',AL4(VALTAPP)                                     
         DC    C'TRAPCALL    ',AL4(VALTCAL)                                     
         DC    C'CPUADJUST   ',AL4(VALACPU)                                     
         DC    C'SRBADJUST   ',AL4(VALASRB)                                     
         DC    C'TOTADJUST   ',AL4(VALATOT)                                     
         DC    C'CPUOPTION   ',AL4(VALOCPU)                                     
         DC    C'SRBOPTION   ',AL4(VALOSRB)                                     
         DC    C'TOTOPTION   ',AL4(VALOTOT)                                     
         DC    C'DOWNASUP    ',AL4(VALDASU)                                     
         DC    C'RUN         ',AL4(VALRUNT)                                     
         DC    C'TRACE       ',AL4(VALTRCE)                                     
         DC    C'REQTRACE    ',AL4(VALRTRC)                                     
         DC    C'DUMP        ',AL4(VALDUMP)                                     
         DC    C'ESTAE       ',AL4(VALESTA)                                     
         DC    C'DDSIO       ',AL4(VALDDIO)                                     
         DC    C'UPDID       ',AL4(VALUPID)                                     
         DC    C'LOAD        ',AL4(VALLOAD)                                     
         DC    C'VERIFY      ',AL4(VALVRFY)                                     
         DC    C'PATCH       ',AL4(VALPTCH)                                     
         DC    C'END         ',AL4(VALEND)                                      
         DC    C'PRINT       ',AL4(VALPRNT)                                     
         DC    C'ENDRUN      ',AL4(VALENDR)                                     
         DC    C'EOJ         ',AL4(VALENDR)                                     
         DC    C'TEST        ',AL4(VALTEST)                                     
         DC    C'SYSMDUMP    ',AL4(VALSYSM)                                     
         DC    C'SMTP        ',AL4(VALSMTP)                                     
         DC    C'<FALINK>    ',AL4(VALFALS)                                     
FALINKXL DC    C'<FALINKX>   ',AL4(VALFALE)                                     
*                                  ** Batch mode commands **                    
         DC    C'BATCH       ',AL4(VALBTCH)                                     
         DC    C'AC*         ',AL4(VALSACC)                                     
         DC    C'DATE        ',AL4(VALDATE)                                     
         DC    C'LOGO        ',AL4(VALLOGO)                                     
         DC    C'SHIP        ',AL4(VALEXIT)                                     
         DC    C'DEST        ',AL4(VALEXIT)                                     
         DC    C'DESTINATION ',AL4(VALEXIT)                                     
         DC    C'DEST2       ',AL4(VALEXIT)                                     
         DC    C'INPUT       ',AL4(VALEXIT)                                     
         DC    C'DIRECT      ',AL4(VALEXIT)                                     
         DC    C'TAPE        ',AL4(VALEXIT)                                     
         DC    C'FLASH       ',AL4(VALFLSH)                                     
VALTABX  DC    AL1(VALTEOTQ)                                                    
                                                                                
VALTABD  DSECT ,                   ** Layout of VALTAB above **                 
VALTEOTQ EQU   0                   End of table indicator                       
VALTKWRD DS    CL12                Keyword                                      
VALTROUT DS    AL4                 Validation routine                           
VALTABL  EQU   *-VALTABD           Length of table entry                        
RUNNER   CSECT ,                                                                
                                                                                
WTOD     DSECT ,                   ** WTOR control block **                     
WTOLRPLY DS    0AL1                Length of reply                              
WTOARPLY DS    A                   Address of reply                             
WTOAECB  DS    A                   A(ECB)                                       
WTOLMESS DS    AL2                 Length of message                            
WTOND    DS    AL2                 n/d                                          
WTOPARML EQU   *-WTOD              Length of WTOR parameters                    
                                                                                
WTOMESS  DS    0C                  Message                                      
WTOMESSL EQU   L'WORK-WTOPARML     (WTOD always based on WORK)                  
                                                                                
RUNNER   CSECT ,                                                                
         EJECT                                                                  
CONSYSQ  EQU   10                  Control system SE number                     
ONEK     EQU   1024                                                             
FF       EQU   X'FF'                                                            
FE       EQU   X'FE'                                                            
FD       EQU   X'FD'                                                            
TIMERABN EQU   X'FF'               MCPOPSW Timer suspended due to abend         
TIMEROPS EQU   X'FE'               MCPOPSW Timer suspended for PUTOPS           
TIMERPOP EQU   C'P'                MCPOPSW Timer pop active                     
PRINTECB EQU   X'80'                                                            
MODINFLQ EQU   39                  L'phase info (book, level & date)            
MODINF1Q EQU   95                  + COMPILED BY                                
MODINF2Q EQU   107                 + AS RMBOOK                                  
PRTKEYLQ EQU   25                  Maximum L'printed file key                   
                                                                                
SPACE    EQU   C' '                                                             
COMMA    EQU   C','                                                             
EMBEDDED EQU   C'@'                                                             
EQUAL    EQU   C'='                                                             
PLUS     EQU   C'+'                                                             
PERIOD   EQU   C'.'                                                             
HYPHEN   EQU   C'-'                                                             
ASTERISK EQU   C'*'                                                             
COLON    EQU   C':'                                                             
SLASH    EQU   C'/'                                                             
PROCMC   EQU   X'80'                                                            
DIGIT    EQU   X'0F'                                                            
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
BYTESIZE EQU   C'B'                                                             
KILOSIZE EQU   C'K'                                                             
MEGASIZE EQU   C'M'                                                             
VERSION5 EQU   5                   Comms. asynchronous download version         
MAXKEYLN EQU   50                                                               
                                                                                
HOURDAY  EQU   24                                                               
MINHOUR  EQU   60                                                               
SECSMIN  EQU   60                                                               
                                                                                
DUB      EQU   MCDUB                                                            
WORK     EQU   MCWORK                                                           
DMCB     EQU   MCPARA                                                           
                                                                                
COMTABD  DSECT ,                   ** COMFACS table **                          
COMTEOTQ EQU   0                   End of table indicator                       
COMNUM   DS    X                   Phase number (T00Axx)                        
COMCDSP  DS    AL2                 Displacement to adcon in COMFACS             
COMTABL  EQU   *-COMTABD           Length of table entry                        
RUNNER   CSECT ,                                                                
                                                                                
         DS    0H                                                               
COMTAB   DS    0XL(COMTABL)        ** COMFACS phases **                         
         DC    AL1(QXSORT),AL2(CXSORT-COMFACSD)                                 
         DC    AL1(QQSORT),AL2(CQSORT-COMFACSD)                                 
         DC    AL1(QMINIO),AL2(CMINIO-COMFACSD)                                 
         DC    AL1(QWRKIO),AL2(CWRKIO-COMFACSD)                                 
         DC    AL1(QLINKIO),AL2(CLINKIO-COMFACSD)                               
         DC    AL1(QSOFDAT),AL2(CSOFDAT-COMFACSD)                               
         DC    AL1(QPERVAL),AL2(CPERVAL-COMFACSD)                               
         DC    AL1(QPARSNIP),AL2(CPARSNIP-COMFACSD)                             
*&&UK*&& DC    AL1(QEDITOR),AL2(CEDITOR-COMFACSD)                               
*&&UK*&& DC    AL1(QREQTWA),AL2(CREQTWA-COMFACSD)                               
*&&UK*&& DC    AL1(QCONVERT),AL2(CCONVERT-COMFACSD)                             
COMTABX  DC    AL1(COMTEOTQ)                                                    
                                                                                
         DS    0H                                                               
FLITAB   DS    0CL8                Facilites list names                         
         DC    CL8'WORKD   '                                                    
         DC    CL8'RUNFACSD'                                                    
         DC    CL8'COMFACSD'                                                    
         DC    CL8'SYSFACSD'                                                    
         DC    CL8'MASTD   '                                                    
         DC    CL8'LP_D    '                                                    
         DC    CL8'LP_XD   '                                                    
         EJECT                                                                  
***********************************************************************         
* Global literals                                                     *         
***********************************************************************         
                                                                                
GLOBALS  DS    0D                                                               
                                                                                
         USING LQ_D,RF                                                          
RWRKMVCQ MVC   CARD(0),LQ_CMND                                                  
         DROP  RF                                                               
GPHSMVE5 MVC   0(0,RE),0(R5)                                                    
         USING GPWORKD,RC                                                       
GPHSMVEW MVC   0(0,RE),GPWORK                                                   
GPHSCLEW CLC   0(0,RE),GPWORK                                                   
         DROP  RC                                                               
         USING MODTABD,R5                                                       
LPHSCLN4 CLC   MODTPHSN(0),0(R4)                                                
LPHSCLM4 CLC   MODTMODN(0),0(R4)                                                
         DROP  R5                                                               
         USING VPWORKD,RC                                                       
         USING VALTABD,RE                                                       
VPARCLKC CLC   VALTKWRD(0),VPCARD                                               
         DROP  RC,RE                                                            
         USING DMTTABD,R3                                                       
ADMTMVF1 MVC   DMTFILE(0),0(R1)                                                 
         DROP  R3                                                               
VNUMPKQ1 PACK  QUAD,0(0,R1)                                                     
CALLMVP2 MVC   P(0),0(R2)                                                       
         USING CIBNEXT,R2                                                       
TOPTMVCD MVC   CARD(0),CIBDATA                                                  
         DROP  R2                                                               
BLOGMVM2 MVC   PMESSAGE(0),0(R2)                                                
MTRCMVM1 MVC   PMESSAGE(0),0(R1)                                                
REQMSG   MVC   PMESSAGE(0),0(RE)                                                
         USING DMTTABD,R3                                                       
DTRCCLF1 CLC   DMTFILE(0),0(R1)                                                 
         DROP  R3                                                               
POPSPKDF PACK  DUB,1(0,RF)                                                      
                                                                                
         LTORG ,                                                                
                                                                                
ARZERO   DC    16F'0'                                                           
                                                                                
ASVRSAVE DC    A(0)                                                             
AREQSAVE DC    A(REQSAVE)                                                       
AERRWORK DC    A(ERRWORK)                                                       
ADSPCECB DC    A(ECBDSP)           A(ECB of global operator interrupt)          
                                                                                
AFACLST  DS    0A                  ** Addresses of facilities blocks **         
WORKFACQ EQU   *-AFACLST                                                        
ARUNFACS DC    A(RUNFACS)                                                       
RUNFACQ  EQU   *-AFACLST                                                        
ACOMFACS DC    A(COMFACS)                                                       
COMFACQ  EQU   *-AFACLST                                                        
ASYSFACS DC    A(SYSFACS)                                                       
SYSFACQ  EQU   *-AFACLST                                                        
AMASTC   DC    A(MASTC)                                                         
MASTCQ   EQU   *-AFACLST                                                        
ALP_D    DC    A(LP_DB)                                                         
LP_DQ    EQU   *-AFACLST                                                        
ALP_XD   DC    A(LP_XDB)                                                        
LP_XDQ   EQU   *-AFACLST                                                        
                                                                                
VDMENQDQ DC    V(DMENQDEQ)                                                      
VISGENQ  DC    V(DMISGENQ)                                                      
VLOCKSPC DC    V(LOCKSPC)                                                       
VSMTP    DC    V(SMTP)                                                          
VCPRINT2 DC    V(CPRINT2)                                                       
VPRINTER DC    V(PRINTER)                                                       
VPRINT   DC    V(PRINT)                                                         
VPDSOON  DC    V(PDMPSOON)                                                      
VMDUMPER DC    V(MDUMPER)                                                       
*&&US                                                                           
VDEMVER  DC    V(DEMVER)                                                        
*&&                                                                             
ATWA     DC    A(TWA)                                                           
ATWAX    DC    A(TWAX)                                                          
                                                                                
HSPERDAY DC    A(HOURDAY*MINHOUR*SECSMIN*100)                                   
SECPERHR DC    A(MINHOUR*SECSMIN)                                               
SECPERMN DC    A(SECSMIN)                                                       
                                                                                
DTRUNITL DC    A(DTRUNIT)                                                       
DTCTBL   DC    A(DTCTB)                                                         
                                                                                
F100     DC    F'100'                                                           
F10000   DC    F'10000'                                                         
TUSSEC   DC    A(38400)                                                         
TUS10S   DC    A(38400*10)                                                      
TUS01M   DC    A(38400*60)                                                      
TUS20M   DC    A(38400*60*20)                                                   
TUS30M   DC    A(38400*60*30)                                                   
TUS40M   DC    A(38400*60*40)                                                   
TUS120   DC    A(38400*60*120)                                                  
HOBOFF   DC    X'7FFFFFFF'                                                      
                                                                                
MAXCPUT  DC    F'99999999'                                                      
MAXCPUTL DC    C'**High***'                                                     
                                                                                
BC01     DC    C'BC01'                                                          
                                                                                
HEXTAB   DC    C'0123456789ABCDEF'                                              
ICMTAB   DC    AL1(01,03,07,15)                                                 
LENTAB   DC    AL1(03,05,08,10)                                                 
                                                                                
WAITPROC DC    AL1(TQSWAIT,TQSRUN)                                              
PROCWAIT DC    AL1(TQSRUN,TQSWAIT)                                              
NULLDONE DC    AL1(0,TQSDONE)                                                   
NULLFREE DC    AL1(0,TQSFREE)                                                   
                                                                                
DDSUSID  DS    0AL2                DDS user-id for command files                
*&&UK*&& DC    AL2(1)                                                           
*&&US*&& DC    AL2(90)                                                          
                                                                                
OPNACT   DS    X                   OPNCLO s/r action code                       
OPNAOPN  EQU   SEOLFOPN                                                         
OPNACLO  EQU   SEOLFCLS                                                         
                                                                                
DDLINK   DC    C'T00AE5'           DDLINK's phase name                          
CRPHASE  DC    C'T00A'             Core resident phase prefix                   
                                                                                
CONSYS   DC    C'CONTROL'                                                       
RUNCLASS DC    C'RUNNERCLASS'                                                   
                                                                                
MULTIGEN DC    C'XFIL'             General multi-file worker key                
MULTICFM DC    C'CFMC'             CFM multi-file worker key                    
                                                                                
IOOTHER  DC    C'+Other   '        For counting other I/Os                      
                                                                                
MQIBUFF  DC    A(0)                A(input buffer)                              
MQIBUFFL DC    A(ONEK*ONEK*2)      MQ input buffer length                       
MQOBUFF  DC    A(0)                A(output buffer)                             
MQOBUFFL DC    A(ONEK*ONEK*8)      MQ output buffer length                      
MQOBUFFP DC    A(0)                Current output message pointer               
MQOBUFFX DC    A(0)                A(end of MQ output buffer)                   
                                                                                
MQCCOK   DC    A(MQCC_OK)          Good MQ return code                          
MQCCWARN DC    A(MQCC_WARNING)     Warning return code                          
MQCCFAIL DC    A(MQCC_FAILED)      Failure return code                          
MQMATOPT DC    A(MQMO_MATCH_MSG_ID+MQMO_MATCH_CORREL_ID)                        
                                                                                
MQCOMPC  DC    A(0)                Completion code                              
MQREASN  DC    A(0)                Reason code                                  
                                                                                
MQLOMSG  DC    A(0)                Length of output message                     
MQLIMSG  DC    A(0)                Length of input message                      
                                                                                
LOOPTIMR DC    A(100)              Loop timer 1 second (hundredths)             
ONESECND DC    A(100)              1 second (hundredths) for wait               
FIVSECND DC    A(500)              5 seconds (hundredths) for wait              
                                                                                
DDLYWT1  DC    A(25)               INITIAL DEADLY EMBRACE RETRY WAIT            
DDLYWTMX DC    A(1000)             MAXIMUM DEADLY EMBRACE RETRY WAIT            
         EJECT                                                                  
CONLST   DC    C'N'                ** Control file list **                      
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
CONLSTX  DC    C'X'                                                             
                                                                                
DMUPDID  DC    C'UPDID  '                                                       
DMOPEN   DC    C'DMOPEN '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
DMRCVR   DC    C'RECOVR '                                                       
DMCOMMIT DC    C'COMMIT '                                                       
DMOPMSG  DC    C'OPMSG  '                                                       
WKPURGE  DC    C'PURGE  '                                                       
SORTEND  DC    C'END'                                                           
                                                                                
JESMAIL  DC    CL8'JESMAIL'                                                     
ASMIDF   DC    CL8'ASMIDF'                                                      
T00A     DC    AL1(CALLOVQ),X'000A'                                             
                                                                                
EFFS     DC    X'FFFFFFFFFFFFFFFF'                                              
                                                                                
OPSTOP   DC    C'STOP'                                                          
OPNEXT   DC    C'NEXT'                                                          
OPGO     DC    C'GO'                                                            
OPEOR    DC    C'EOR'                                                           
OPNOW    DC    C'NOW'                                                           
OPDUMP   DC    C'DUMP'                                                          
OPIGN    DC    C'IGN'                                                           
OPRTP    DC    C'RTP'                                                           
                                                                                
NO       DC    C'NO '                                                           
YES      DC    C'YES'                                                           
ALL      DC    C'ALL'                                                           
GETA     DC    C'GETA'                                                          
SORT     DC    C'SORT    '                                                      
BOOKPFX  DC    C'BOOK='                                                         
LEVLPFX  DC    C'LEVEL='                                                        
DATEPFX  DC    C'DATE='                                                         
COMPPFX  DC    C'COMPILED'                                                      
COMPPFXX DC    C'AS'                                                            
NONE     DC    C'NONE'                                                          
ZERO     DC    C'ZERO'                                                          
                                                                                
SVREYE   DC    C'*SERVER**SERVER**SERVER**SERVER*'                              
RUNEYE   DC    C'*RUN*RUN'                                                      
SVRSVEYE DC    C'*SVRSAV**SVRSAV**SVRSAV**SVRSAV*'                              
                                                                                
MQERRM   DC    C'ERROR-Server abended-'                                         
MQACK02  DC    C'ACK-02'                                                        
                                                                                
RFHDRLIT DC    C'RFHDR='                                                        
ORIGLIT  DC    C'ORIGIN='                                                       
REQLIT1  DC    C'Request data='                                                 
REQLIT2  DC    C'Request record:-'                                              
TSTPQLIT DC    C'TSTPQDD0'                                                      
TSTCTLIT DC    C'TSTCTDD0'                                                      
REPPQLIT DC    C'REPPQDD0'                                                      
                                                                                
SYSLIT1  DC    C'RUNNER System table contents'                                  
SYSLIT2  DC    C'----------------------------'                                  
SYSLIT3  DC    C'System  SE#  Status'                                           
                                                                                
SYSNOOP  DC    C'No-op'                                                         
SYSOPN   DC    C'Open'                                                          
SYSCLS   DC    C'Closed'                                                        
SYSGLB   DC    C'for update'                                                    
                                                                                
WORKMSG  DC    C'RUNNER looking for work'                                       
DDLRMSG  DC    C'RUNNER retry no. nnn after deadly embrace'                     
WAITMSG  DC    C'RUNNER going into wait state'                                  
COMSMSG  DC    C'RUNNER Processing commands'                                    
                                                                                
RUNTITLE DC    C'RUNNER Trace'                                                  
                                                                                
LINIMESS DC    C'Server intialization error'                                    
                                                                                
POPMSG1  DC    C'+RUNNER01+ stepname may be looping'                            
POPMSG2  DC    C' - hit ENTER for details'                                      
                                                                                
LOPMESSA DC    C'+RUNNER02+ stepname Req nnnnnnn ET=hh.mm.ss '                  
LOPMESSB DC    C'CPU=nnnn.nnnn SIOs=nnnnnnn Lns=nnnnnn Pgs=nnnnn '              
LOPMESSC DC    C'Reply:GO/EOR/NOW/DUMP/RTP '                                    
                                                                                
LPCMESS1 DC    C'+RUNNER03+ stepname Abend103 opening:- SE#=XX '                
LPCMESS2 DC    C'File allocation error'                                         
                                                                                
LPCMESSA DC    C'+RUNNER04+ stepname Req nnnnnnn died in xxxxxxxx '             
LPCMESSB DC    C'+???? xxxxxx fails,Reason=xxxxxxxxxxxxxxxxxxxx '               
LPCMESSC DC    C'Reply:NEXT/STOP'                                               
                                                                                
BUSYMSG  DC    C' busy. Command ignored. Use PAUSE or STATUS command?'          
PAUSEMSG DC    C' paused. Enter command or GO to continue.'                     
                                                                                
UNKPSW   DC    C'Unknown-See PSW'                                               
                                                                                
LINMESSA DC    C'CPU=nnnnnnn '                                                  
LINMESSB DC    C'TIME=nnnnnnn '                                                 
LINMESSC DC    C'EXCP=nnnnnnn '                                                 
LINMESSD DC    C'PAGES=nnnnnnn '                                                
LINMESSE DC    C'Reply:C=n/T=n/E=n/P=n/GO'                                      
                                                                                
BADREPLY DC    C'Bad reply-'                                                    
                                                                                
PQFULLM  DC    C'PRINTQ FUL'                                                    
MZWAITM  DC    C'Waiting for MEDZ'                                              
                                                                                
CANCPQE  DC    CL20'*Print queue error*'                                        
CANCAPL  DC    CL20'Program looping'                                            
*&&US                                                                           
QMLIST   DC    CL64'AHYD,TCLE,RMOR:'                                            
*&&                                                                             
*&&UK                                                                           
QMLIST   DC    CL64'TCLEEVE,RMORSE:'                                            
*&&                                                                             
QMSUBJ   DC    C'RUNNER Queue Maintenance Report from '                         
QMSUBJN  DC    CL(L'MCSTEP)' '     Step name                                    
QMSUBJL  EQU   *-QMSUBJ                                                         
                                                                                
QMHEAD   DC    C'FTTS Arr.Time     Worker key                Action'            
QMACTN1  DC    C'Purged - future arrival time'                                  
QMACTN2  DC    C'Purged - complete not sent  '                                  
QMACTN3  DC    C'Report warning              '                                  
QMACTN4  DC    C'Purged - time exceeded      '                                  
                                                                                
CMDERR1  DC    C'Can''t open external command file - ignoring'                  
CMDERR2  DC    C'Invalid command job queue entry'                               
BADLIT1  DC    C'Can''t read system list record - run terminating'              
BADLIT2  DC    C'No systems were found to open - run terminating'               
INFLIT1  DC    C'Opening system files:- '                                       
INFLIT2  DC    C'Loading COMFACS phases'                                        
INFLIT3  DC    C'Loading SYSFACS phases'                                        
INFLIT4  DC    C'Initializing SMTP'                                             
INFLIT5  DC    C'Invoking system recovery'                                      
INFLIT6  DC    C'Closing system files:- '                                       
INFLIT7  DC    C'Initializing MQ interface'                                     
INFLIT8  DC    C'MQ input buffer acquired, address='                            
INFLIT9  DC    C'MQ output buffer acquired, address='                           
INFLITA  DC    C',Length='                                                      
INFLITB  DC    C'MQ message received:- '                                        
INFLITC  DC    C'Running work - message id='                                    
INFLITD  DC    C'Releasing system lock for'                                     
ENDLIT   DC    C'RUNNER exiting back to MVS'                                    
FULLIT   DC    C'Run table full - run terminating'                              
SORLIT   DC    C'<<< Start of request details >>>'                              
EORLIT   DC    C'<<< End of request details >>>'                                
MAXDLIT  DC    C'MAXDUMPS exceeded'                                             
                                                                                
PCHLIT   DC    C'Patching storage - current value='                             
VERLIT   DC    C'Verify failed - actual value='                                 
                                                                                
RUNLIT1  DC    C'Starting work, worker key:- '                                  
                                                                                
RUNLIT2  DC    C'Work complete. Request#='                                      
RL2REQS  DC    C'nnnnnn'                                                        
         DC    C',Map#=X'''                                                     
RL2MAPNO DC    C'nnnn'                                                          
         DC    C''''                                                            
RUNLIT2L EQU   *-RUNLIT2                                                        
                                                                                
RUNLIT3  DC    C'CPU time='                                                     
RL3CPUT  DC    C'nnnn.nnnn'                                                     
         DC    C',Start I/Os='                                                  
RL3SIOS  DC    C'nnnnnnn'                                                       
         DC    C',Timer pops='                                                  
RL3POPS  DC    C'nnnnn'                                                         
RUNLIT3L EQU   *-RUNLIT3                                                        
                                                                                
RUNLIT4  DC    C'Log records='                                                  
RL4LOGR  DC    C'nnnnnn'                                                        
         DC    C',Data records='                                                
RL4RECS  DC    C'nnnnnn'                                                        
         DC    C',Est. download size='                                          
RL4SIZE  DC    C'nnnn.nnx'                                                      
RUNLIT4L EQU   *-RUNLIT4                                                        
                                                                                
RUNLIT5  DC    C'Segments added='                                               
RL5SEGS  DC    C'nnnn'                                                          
         DC    C',Asynch='                                                      
RL5ASNC  DC    C'   '                                                           
RUNLIT5L EQU   *-RUNLIT5                                                        
                                                                                
RUNLIT7  DC    C'Arrived@='                                                     
RL7ATIM  DC    C'hh:mm:ss.mmm'                                                  
         DC    C',Started@='                                                    
RL7STIM  DC    C'hh:mm:ss.mmm'                                                  
         DC    C',Ended@='                                                      
RL7ETIM  DC    C'hh:mm:ss.mmm'                                                  
RUNLIT7L EQU   *-RUNLIT7                                                        
                                                                                
RUNLIT8  DC    C'Queue time='                                                   
RL8QTIM  DC    C'hh:mm:ss.mmm'                                                  
         DC    C',Elapsed time='                                                
RL8ETIM  DC    C'hh:mm:ss.mmm'                                                  
RUNLIT8L EQU   *-RUNLIT8                                                        
                                                                                
RUNLIT9  DC    C'Avg. queue time='                                              
RL9QTIM  DC    C'hh:mm:ss.mmm'                                                  
         DC    C',Avg. elapsed time='                                           
RL9ETIM  DC    C'hh:mm:ss.mmm'                                                  
RUNLIT9L EQU   *-RUNLIT9                                                        
                                                                                
RUNLITA  DC    C'Avg. start I/Os='                                              
RLASIOS  DC    C'nnnnnn'                                                        
         DC    C',Avg. CPU time='                                               
RLACPUT  DC    C'nnnn.nnnn'                                                     
         DC    C',Avg. output records='                                         
RLARECS  DC    C'nnnnnn'                                                        
RUNLITAL EQU   *-RUNLITA                                                        
                                                                                
RUNLITB  DC    C'Ending work, worker key:- '                                    
                                                                                
RUNLITC  DC    C'Requested from LUID='                                          
RLCLUID  DC    C'????????'                                                      
         DC    C',AGENCY='                                                      
RLCAGY   DC    C'xx'                                                            
         DC    C',FACPAK='                                                      
RLCFACID DC    C'xxxx'                                                          
RUNLITCL EQU   *-RUNLITC                                                        
                                                                                
RUNLITD  DC    C'PC Application='                                               
RUNLITE  DC    C',Version='                                                     
                                                                                
RUNLITJ  DC    C'MQ data records='                                              
RLJRECS  DC    C'nnnnnn'                                                        
         DC    C',Message(s)='                                                  
RLJBUFS  DC    C'nnn'                                                           
         DC    C',Total message size='                                          
RLJSIZE  DC    C'nnnn.nnx'                                                      
RUNLITJL EQU   *-RUNLITJ                                                        
                                                                                
RUNLITF  DS    0C                                                               
RLFLABEL DC    CL36'Trace caller'                                               
         DC    C'CPU='                                                          
RLFCPU   DC    C'nnnnnnnnn'                                                     
         DC    C',SRB='                                                         
RLFSRB   DC    C'nnnnnnnnn'                                                     
         DC    C',TOT='                                                         
RLFTOT   DC    C'nnnnnnnnn'                                                     
         DC    C',SIO='                                                         
RLFIO    DC    C'nnnnnn'                                                        
         DC    C',Calls='                                                       
RLFCALL  DC    C'nnnnnn'                                                        
         DC    C',TCPU='                                                        
RLFCPUT  DC    C'nnnnnnnnn'                                                     
         DC    C',TSRB='                                                        
RLFSRBT  DC    C'nnnnnnnnn'                                                     
         DC    C',TTOT='                                                        
RLFTOTT  DC    C'nnnnnnnnn'                                                     
         DC    C',TSIO='                                                        
RLFIOT   DC    C'nnnnnn'                                                        
RUNLITFL EQU   *-RUNLITF                                                        
                                                                                
RUNLITG  DS    0C                                                               
RLGLABEL DC    CL10'Trace caller'                                               
         DC    C'Calls='                                                        
RLGCALL  DC    C'nnnnnnnnn'                                                     
         DC    C',TCPU='                                                        
RLGCPUT  DC    C'nnnnnnnnn'                                                     
         DC    C',TSRB='                                                        
RLGSRBT  DC    C'nnnnnnnnn'                                                     
         DC    C',TTOT='                                                        
RLGTOTT  DC    C'nnnnnnnnn'                                                     
         DC    C',TSIO='                                                        
RLGIOT   DC    C'nnnnnnnnn'                                                     
RUNLITGL EQU   *-RUNLITG                                                        
                                                                                
RUNLITH  DS    0C                                                               
         DC    C'Trap created for '                                             
RLHMODN  DC    C'MODNAME'                                                       
         DC    C' RunTrap='                                                     
RLHADDR  DC    C'MODTADDR'                                                      
         DC    C',SvrTrap='                                                     
RLHTRAP  DC    C'MODTTRAP'                                                      
         DC    C',RealAddr='                                                    
RLHREAL  DC    C'MODTREAL'                                                      
         DC    C',FacList='                                                     
RLHFACL  DC    C'FLINAME'                                                       
RLHFILL  DC    CL16' '             Cleared area                                 
RUNLITHL EQU   *-RUNLITH                                                        
                                                                                
LODLIT1  DC    C'Request for load failed:- '                                    
                                                                                
LODLIT2  DC    C'Phase '                                                        
LL2PHSN  DC    C'xxxxxxxx'                                                      
         DC    C' '                                                             
LL2PHAS  DC    C'yyyyyyyy'                                                      
         DC    C' loaded at '                                                   
LODLIT2L EQU   *-LODLIT2                                                        
                                                                                
LODLIT3  DC    C'Request for test phase load failed:- '                         
LODLIT4  DC    C'Invalid patch format s/b +disp(1-8)=hex value'                 
LODLIT5  DC    C'Load of phase avoided:- '                                      
                                                                                
UIDLIT1  DC    C'User-ID buffer initialized'                                    
UIDLIT2  DC    C'ID found in buffer'                                            
UIDLIT3  DC    C'Reading User-ID record, ID#='                                  
                                                                                
CMDLIT1  DC    C'Running external command:- '                                   
                                                                                
VALLIT1  DC    C'Input parameter cards'                                         
VALLIT2  DC    C'---------------------'                                         
VALLIT3  DC    C'Invalid parameter card format - s/b KEYWORD=DATA'              
VALLIT4  DC    C'Control card not recognised'                                   
VALLIT5  DC    C'Invalid parameter value'                                       
VALLIT6  DC    C'No SERVER= parameter specified'                                
VALLIT7  DC    C'RTPCPU=value must be multiple of LOOPSECS=value'               
                                                                                
SPCLIT1  DC    C'Invalid DataSpace - run terminating'                           
SPCLIT2  DC    C'TABSRUN value in DataSpace is zero - re-initialize'            
SPCLIT3  DC    C'TABSRUN value in DataSpace is invalid - re-initialize'         
                                                                                
FAILLIT  DC    C'Failure in job'                                                
WKEYLIT  DC    C',Worker key='                                                  
MQIDLIT  DC    C',Message id='                                                  
QTIMLIT  DC    C'Maximum queue time exceeded - need more servers'               
ATIMLIT  DC    C'Average queue time exceeded by '                               
ATIMTIM  DC    C'hh:ss:mm.mmm'                                                  
SNOPLIT  DC    C'Can''t run work - system not open'                             
SYSTLIT  DC    C'System='                                                       
                                                                                
CANLIT   DC    C'About to purge worker file:- '                                 
                                                                                
MQCONBRO DC    C'MQ Connection broken '                                         
MQRECO   DC    C'MQ Reconnected ok '                                            
                                                                                
ABEND103 DC    AL3(103)                                                         
ABEND268 DC    AL3(268)                                                         
ABEND269 DC    AL3(269)                                                         
ABEND666 DC    AL3(666)                                                         
ABEND667 DC    AL3(667)                                                         
ABEND669 DC    AL3(669)                                                         
ABEND670 DC    AL3(670)                                                         
ABEND899 DC    AL3(899)                                                         
ABEND990 DC    AL3(990)                                                         
ABEND991 DC    AL3(991)                                                         
OPERCANC DC    X'222000'                                                        
OPERCAND DC    X'122000'                                                        
                                                                                
         DS    0D                                                               
         DC    C'**ECBS****ECBS****ECBS****ECBS**'                              
ECBWRK   DC    A(0)                Wake-up ECB                                  
ECBDSP   DC    A(0)                Global operator ECB                          
ECBMQG   DC    A(0)                MQ Get ECB                                   
                                                                                
         DS    0D                                                               
         DC    C'*RUNFACS*RUNFACS*RUNFACS*RUNFACS'                              
RUNFACS  DS    0A                                                               
         DC    (RUNFACSL)X'00'                                                  
         ORG   RUNFACS+(RWRKBLK1-RUNFACSD)                                      
         DC    A(WRKBLK1)                                                       
         ORG   RUNFACS+(RWRKBLK2-RUNFACSD)                                      
         DC    A(WRKBLK2)                                                       
         ORG   RUNFACS+(RDDLINKP-RUNFACSD)                                      
         DC    A(LP_DB)                                                         
         ORG   RUNFACS+(RMASTC-RUNFACSD)                                        
         DC    A(MASTC)                                                         
         ORG   RUNFACS+(RCOMFACS-RUNFACSD)                                      
         DC    A(COMFACS)                                                       
         ORG   RUNFACS+(RSYSFACS-RUNFACSD)                                      
         DC    A(SYSFACS)                                                       
         ORG   RUNFACS+(RREQUEST-RUNFACSD)                                      
         DC    A(REQSAVE)                                                       
         ORG   RUNFACS+(RWRKBLKR-RUNFACSD)                                      
         DC    A(WRKBLKR)                                                       
         ORG   RUNFACS+(RFIDTAB-RUNFACSD)                                       
         DC    A(FACIDTAB)                                                      
         ORG   RUNFACS+(RCPRINT-RUNFACSD)                                       
         DC    V(CPRINT)                                                        
         ORG   RUNFACS+(RPRINTER-RUNFACSD)                                      
         DC    V(PRINTER)                                                       
         ORG   RUNFACS+(RSORTER-RUNFACSD)                                       
         DC    V(SORTER)                                                        
         ORG   RUNFACS+(RBINSRCH-RUNFACSD)                                      
         DC    V(BINSRCH)                                                       
         ORG   RUNFACS+(RBUFFRIN-RUNFACSD)                                      
         DC    V(BUFFERIN)                                                      
         ORG   RUNFACS+(RRUNIT-RUNFACSD)                                        
         DC    V(RUNIT)                                                         
         ORG   RUNFACS+(RVALPAR-RUNFACSD)                                       
         DC    A(VALPAR)                                                        
         ORG   RUNFACS+(RGPHS-RUNFACSD)                                         
         DC    A(GPHS)                                                          
         ORG   RUNFACS+(RPRTLOG-RUNFACSD)                                       
         DC    A(PRTLOG)                                                        
         ORG   RUNFACS+(RMQI-RUNFACSD)                                          
         DC    A(MQI)                                                           
         ORG   RUNFACS+(RGETUID-RUNFACSD)                                       
         DC    AL1(GUMSWCH),AL3(GETUID)                                         
         ORG   RUNFACS+(RCPRINT2-RUNFACSD)                                      
         DC    V(CPRINT2)                                                       
*&&UK*&& ORG   RUNFACS+(RCOMPOSE-RUNFACSD)                                      
*&&UK*&& DC    V(COMPOSE)                                                       
         ORG   RUNFACS+(RSMFOUT-RUNFACSD)                                       
         DC    V(SMFOUT)                                                        
         ORG   RUNFACS+(RSMTP-RUNFACSD)                                         
         DC    V(SMTP)                                                          
         ORG   RUNFACS+(RSAVSIZE-RUNFACSD)                                      
         DC    A(SAVSIZEQ)                                                      
         ORG   RUNFACS+(RCALLMQ-RUNFACSD)                                       
         DC    A(CALLMQ)                                                        
         ORG   RUNFACS+(RLPHS-RUNFACSD)                                         
         DC    A(LPHS)                                                          
         ORG   RUNFACS+(RLOADIT-RUNFACSD)                                       
         DC    A(LOADIT)                                                        
         ORG   RUNFACS+(RGOLINK-RUNFACSD)                                       
         DC    A(GOLINK)                                                        
         ORG   RUNFACS+(RPUTMQD-RUNFACSD)                                       
         DC    A(PUTMQD)                                                        
         ORG   RUNFACS+(ROPNCLO-RUNFACSD)                                       
         DC    A(OPNCLO)                                                        
         ORG   RUNFACS+(RDYNALOC-RUNFACSD)                                      
         DC    V(DYNALLOC)                                                      
         ORG   ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'**LP_D***LP_D****LP_D****LP_D***'                              
LP_DB    DC    (LP_LNQ)X'00'       DDLINK parameter block                       
         ORG   LP_DB+(LP_FLAG-LP_D)                                             
         DC    AL1(LP_FOFFL)       We are running offline                       
         ORG   LP_DB+(LP_ACOM-LP_D)                                             
         DC    A(COMFACS)          Common facilities                            
         ORG   LP_DB+(LP_AWORK-LP_D)                                            
         DC    A(DDLINKW)          DDLINK work area                             
         ORG   LP_DB+(LP_ASAVE-LP_D)                                            
         DC    A(DDLINKS)          DDLINK save area                             
         ORG   LP_DB+(LP_ATWA-LP_D)                                             
         DC    A(TWA)              TWA                                          
         ORG   LP_DB+(LP_AUWMP-LP_D)                                            
         DC    A(WMP)              Work map pool                                
         ORG   LP_DB+(LP_WMPL-LP_D)                                             
         DC    AL2(WMPL)           Length of work map pool                      
         ORG   LP_DB+(LP_ASECD-LP_D)                                            
         DC    A(SEC)              SECRET control block                         
         ORG   LP_DB+(LP_ALIOB-LP_D)                                            
         DC    A(LIOBB)            LINKIO control block                         
         ORG   LP_DB+(LP_ABFIN-LP_D)                                            
         DC    V(BUFFERIN)         A(BUFFERIN)                                  
         ORG   LP_DB+(LP_ALPXD-LP_D)                                            
         DC    A(LP_XDB)           A(LP_XDB)                                    
         ORG   ,                                                                
                                                                                
***********************************************************************         
* Storage after here should not be directly addressed via USING GLOBALS         
***********************************************************************         
                                                                                
         DS    0D                                                               
         DC    C'**LP_XD**LP_XD***LP_XD***LP_XD**'                              
LP_XDB   DC    (LP_XLNQ)X'00'                                                   
         EJECT                                                                  
APPNAML  EQU   24                  Length of application name                   
APPTABL  EQU   L'TXPNUM+APPNAML    Width of application table                   
         DS    0H                                                               
*&&US                                                                           
APPTAB   DS    0XL(APPTABL)        ** Application program table **              
*                                                                               
*        UK. USES FAXPTAB INSTEAD                                               
*                                                                               
         DC    AL2(0016),CL(APPNAML)'Steward'                                   
         DC    AL2(0026),CL(APPNAML)'AdBuyer'                                   
         DC    AL2(0028),CL(APPNAML)'MediaVantage'                              
         DC    AL2(0030),CL(APPNAML)'MatchMaker'                                
         DC    AL2(0033),CL(APPNAML)'ComparaGraph'                              
         DC    AL2(0034),CL(APPNAML)'FrontRunner'                               
         DC    AL2(0035),CL(APPNAML)'Spot Steward'                              
         DC    AL2(0036),CL(APPNAML)'Accent on Cashflow'                        
         DC    AL2(0045),CL(APPNAML)'Accent'                                    
         DC    AL2(0046),CL(APPNAML)'Spot Desktop'                              
         DC    AL2(0049),CL(APPNAML)'MFM'                                       
         DC    AL2(0053),CL(APPNAML)'BrandOcean Time'                           
         DC    AL2(0054),CL(APPNAML)'DDS Desktop'                               
         DC    AL2(0055),CL(APPNAML)'iDesk'                                     
         DC    AL2(0058),CL(APPNAML)'Organizer'                                 
         DC    AL2(0061),CL(APPNAML)'MediaVantage'                              
         DC    AL2(0070),CL(APPNAML)'Mobile'                                    
         DC    AL2(0071),CL(APPNAML)'Aura'                                      
         DC    AL2(0073),CL(APPNAML)'Optica'                                    
         DC    AL2(0081),CL(APPNAML)'Prisma'                                    
         DC    AL2(0082),CL(APPNAML)'Station Tool Kit'                          
         DC    AL2(0083),CL(APPNAML)'Spot Auth Tracker'                         
         DC    AL2(1001),CL(APPNAML)'Optimizer'                                 
         DC    AL2(1002),CL(APPNAML)'Pinergy'                                   
APPTABN  EQU   (*-APPTAB)/L'APPTAB                                              
*&&                                                                             
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*COMFACS*COMFACS*COMFACS*COMFACS'                              
COMFACS  DS    0A                  ** Common facilities **                      
         DC    (COMFACSN)A(0)                                                   
         ORG   COMFACS+(CBINSRCH-COMFACSD)                                      
         DC    V(BINSRCH)                                                       
         ORG   COMFACS+(CDATAMGR-COMFACSD)                                      
         DC    V(DATAMGR)                                                       
         ORG   COMFACS+(CCALLOV-COMFACSD)                                       
         DC    A(GPHS)                                                          
         ORG   COMFACS+(CGETFACT-COMFACSD)                                      
         DC    V(GETFACT)                                                       
         ORG   COMFACS+(CGETMSG-COMFACSD)                                       
         DC    V(GETMSG)                                                        
         ORG   COMFACS+(CGETTXT-COMFACSD)                                       
         DC    V(GETTXT)                                                        
         ORG   COMFACS+(CHELLO-COMFACSD)                                        
         DC    V(HELLO)                                                         
         ORG   COMFACS+(CSCANNER-COMFACSD)                                      
         DC    V(SCANNER)                                                       
         ORG   COMFACS+(CHEXIN-COMFACSD)                                        
         DC    V(HEXIN)                                                         
         ORG   COMFACS+(CHEXOUT-COMFACSD)                                       
         DC    V(HEXOUT)                                                        
         ORG   COMFACS+(CCASHVAL-COMFACSD)                                      
         DC    V(CASHVAL)                                                       
         ORG   COMFACS+(CDATVAL-COMFACSD)                                       
         DC    V(DATVAL)                                                        
         ORG   COMFACS+(CDATCON-COMFACSD)                                       
         DC    V(DATCON)                                                        
         ORG   COMFACS+(CDATCONX-COMFACSD)                                      
         DC    V(DATCONX)                                                       
         ORG   COMFACS+(CADDAY-COMFACSD)                                        
         DC    V(ADDAY)                                                         
         ORG   COMFACS+(CGETDAY-COMFACSD)                                       
         DC    V(GETDAY)                                                        
         ORG   COMFACS+(CGETPROF-COMFACSD)                                      
         DC    V(GETPROF)                                                       
         ORG   COMFACS+(CPERVERT-COMFACSD)                                      
         DC    V(PERVERT)                                                       
         ORG   COMFACS+(CDICTATE-COMFACSD)                                      
         DC    V(DICTATE)                                                       
         ORG   COMFACS+(CLOCKSPC-COMFACSD)                                      
         DC    V(LOCKSPC)                                                       
         ORG   COMFACS+(CMASTC-COMFACSD)                                        
         DC    A(MASTC)                                                         
         ORG   COMFACS+(CCUREDIT-COMFACSD)                                      
         DC    V(CUREDIT)                                                       
         ORG   COMFACS+(CBLDCUR-COMFACSD)                                       
         DC    V(BLDCUR)                                                        
         ORG   COMFACS+(CSECRET-COMFACSD)                                       
         DC    V(SECRET)                                                        
         ORG   COMFACS+(CPROTOFF-COMFACSD)                                      
         DC    A(RETURN)                                                        
         ORG   COMFACS+(CPROTON-COMFACSD)                                       
         DC    A(RETURN)                                                        
         ORG   COMFACS+(CLOCKET-COMFACSD)                                       
         DC    V(LOCKET)                                                        
         ORG   COMFACS+(CRECUP-COMFACSD)                                        
         DC    V(RECUP)                                                         
         ORG   COMFACS+(CDEJAVU-COMFACSD)                                       
         DC    V(DEJAVU)                                                        
         ORG   COMFACS+(CMQRPT-COMFACSD)                                        
         DC    V(MQRPT)                                                         
         ORG   COMFACS+(CSEARCH-COMFACSD)                                       
         DC    V(SRCHEXEC)                                                      
*&&UK*&& ORG   COMFACS+(CLIMACC-COMFACSD)                                       
*&&UK*&& DC    V(LIMACC)                                                        
*&&UK*&& ORG   COMFACS+(CEUREKA-COMFACSD)                                       
*&&UK*&& DC    V(EUREKA)                                                        
*&&UK*&& ORG   COMFACS+(CTOBACCO-COMFACSD)                                      
*&&UK*&& DC    V(TOBACCO)                                                       
*&&UK*&& ORG   COMFACS+(CPROMOTE-COMFACSD)                                      
*&&UK*&& DC    V(PROMOTE)                                                       
         ORG   COMFACS+(CGETRET-COMFACSD)                                       
         DC    V(GETRET)                                                        
         ORG   ,                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*SYSFACS*SYSFACS*SYSFACS*SYSFACS'                              
SYSFACS  DS    0A                  ** System facilities **                      
         DC    128A(0)                                                          
*&&UK*&& ORG   SYSFACS+(AGETUNV-MEDFACSD)                                       
*&&UK*&& DC    V(GETUNV)                                                        
*&&UK*&& ORG   SYSFACS+(AGETAUD-MEDFACSD)                                       
*&&UK*&& DC    V(GETAUD)                                                        
*&&UK*&& ORG   SYSFACS+(AGETDPT-MEDFACSD)                                       
*&&UK*&& DC    V(GETDPT)                                                        
*&&UK*&& ORG   SYSFACS+(ASUGGEST-MEDFACSD)                                      
*&&UK*&& DC    V(SUGGEST)                                                       
*&&UK*&& ORG   SYSFACS+(AGETCPT-MEDFACSD)                                       
*&&UK*&& DC    V(GETCPT)                                                        
*&&UK*&& ORG   SYSFACS+(AGETDAY-MEDFACSD)                                       
*&&UK*&& DC    V(GETDAY)                                                        
*&&UK*&& ORG   SYSFACS+(AGETPROD-MEDFACSD)                                      
*&&UK*&& DC    V(GETPROD)                                                       
*&&UK*&& ORG   SYSFACS+(ANEXTSER-MEDFACSD)                                      
*&&UK*&& DC    V(NXTSER)                                                        
*&&UK*&& ORG   SYSFACS+(ADATAMGR-MEDFACSD)                                      
*&&UK*&& DC    V(DATAMGR)                                                       
*&&UK*&& ORG   SYSFACS+(ABINSRCH-MEDFACSD)                                      
*&&UK*&& DC    V(BINSRCH)                                                       
*&&UK*&& ORG   SYSFACS+(AMEDCOMF-MEDFACSD)                                      
*&&UK*&& DC    A(COMFACS)                                                       
         ORG   ,                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*SSB*SSB*SSB*SSB*SSB*SSB*SSB*SSB'                              
SSB      DS    0F                                                               
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSB                                                              
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    AL1(FF)             Set extended off-line SSB                    
         ORG   SSOFLAG3                                                         
         DC    AL1(SSO3XUTL)       Set extended off-line UTL                    
         ORG   SSOMASTC                                                         
         DC    A(MASTC)            Set A(MASTC)                                 
         ORG   ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'*UTL*UTL*UTL*UTL*UTL*UTL*UTL*UTL'                              
UTL      DC    XL256'00'                                                        
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    AL1(CONSYSQ)        Control system                               
         ORG   ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'*MODTAB**MODTAB**MODTAB**MODTAB*'                              
MODTAB   DC    (MODTABN)XL(MODTABL)'00'                                         
                                                                                
MODTABD  DSECT ,                   ** Layout of module table above **           
MODTEOTQ EQU   0                   End of table indicator                       
MODTPHSN DS    CL8                 Phase name                                   
MODTMODN DS    CL8                 Module name                                  
MODTTEST DS    C                   Test level loaded                            
                                                                                
MODTINDS DS    X                   ** Module indicators **                      
MODTITRP EQU   X'80'               Trap set                                     
                                                                                
MODTADDR DS    AL4                 A(phase)                                     
MODTLEN  DS    AL4                 L'phase                                      
MODTREAL DS    AL4                 Real phase address                           
MODTTRAP DS    AL4                 Caller supplied trap routine                 
                                                                                
MODTDSPS DS    0X                                                               
MODTDCOM DS    XL(L'RFACCOMD)                                                   
MODTDSYS DS    XL(L'RFACSYSD)                                                   
MODTDSPL EQU   *-MODTDSPS                                                       
                                                                                
MODTABL  EQU   *-MODTABD           Length of table entry                        
MODTABN  EQU   256                 Maximum number of table entries              
RUNNER   CSECT ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'**MASTC***MASTC***MASTC***MASTC*'                              
MASTC    DS    0X                                                               
         PRINT OFF                                                              
       ++INCLUDE DDMASTC                                                        
         PRINT ON                                                               
                                                                                
RUNNERX  EQU   *                   End of RUNNER patchable area                 
         EJECT                                                                  
***********************************************************************         
* OVERRIDE FILE LISTS                                                 *         
***********************************************************************         
         DS    0F                                                               
*&&US                                                                           
OVERFILE DC    XL1'00'                                                          
         DC    XL1'00'                                                          
         DC    AL4(00000000)                                                    
*&&                                                                             
*&&UK                                                                           
OVERFILE DC    XL1'00'                                                          
         DC    XL1'14'             OVERRIDE FOR MEDZ                            
         DC    AL4(OVRFILE2)       NEXT                                         
*                                                                               
         DC    C'MEDIA  '          SYSTEM NAME FOR OPEN                         
         DC    CL8'NMEDDIR'                                                     
         DC    CL8'NMEDFIL'                                                     
         DC    CL8'NDMNDIR'                                                     
         DC    CL8'NDMNNEW'                                                     
         DC    CL8'NDMNOLD'                                                     
         DC    CL8'NDMODIR'                                                     
         DC    CL8'NDMO1FL'                                                     
         DC    CL8'NDMO2FL'                                                     
         DC    CL8'NDMO3FL'                                                     
         DC    CL8'NDMO4FL'                                                     
         DC    CL8'NCTFILE '                                                    
         DC    C'X'                                                             
*                                                                               
OVRFILE2 DC    XL1'00'                                                          
         DC    XL1'09'             OVERRIDE FOR MBA1                            
         DC    AL4(OVRFILE3)       NEXT                                         
*                                                                               
         DC    C'MBA    '          SYSTEM NAME FOR OPEN                         
         DC    CL8'NMBADIR'                                                     
         DC    CL8'NMBAFILE'                                                    
         DC    CL8'NMBUDIR'                                                     
         DC    CL8'NMBUFILE'                                                    
         DC    C'X'                                                             
                                                                                
OVRFILE3 DC    XL1'00'                                                          
         DC    XL1'1A'             OVERRIDE FOR MBA2                            
         DC    AL4(0)              EOT                                          
*                                                                               
         DC    C'MBA    '          SYSTEM NAME FOR OPEN                         
         DC    CL8'NMBADIR'                                                     
         DC    CL8'NMBAFILE'                                                    
         DC    CL8'NMBUDIR'                                                     
         DC    CL8'NMBUFILE'                                                    
         DC    C'X'                                                             
*&&                                                                             
         EJECT                                                                  
* DDMQREASON included here                                                      
         PRINT OFF                                                              
       ++INCLUDE DDMQREASON                                                     
         PRINT ON                                                               
                                                                                
         DS    0D                                                               
         DC    C'*SEOLST**SEOLST**SEOLST**SEOLST*'                              
SEOLST   DC    (SEOLSTM)XL(SEOLSTL)'00',X'00'                                   
                                                                                
         DS    0D                                                               
         DC    C'*REQSAVE*REQSAVE*REQSAVE*REQSAVE'                              
REQSAVE  DC    (14*ONEK)X'00'      Request save area                            
REQSAVEL EQU   *-REQSAVE                                                        
                                                                                
         DS    0D                                                               
         DC    C'*LRQSAVE*LRQSAVE*LRQSAVE*LRQSAVE'                              
LRQSAVE  DC    (14*ONEK)X'00'      Last Request save area                       
                                                                                
DMTTABD  DSECT ,                   ** DMTTAB below **                           
                                                                                
DMTFILE  DS    CL6                 Datamanager file name                        
DMTNAME  DS    CL7                 File name from first caller                  
                                                                                
DMTTYPE  DS    X                   ** File type **                              
DMTTDIRQ EQU   1                   Directory                                    
DMTTFILQ EQU   2                   D/A file                                     
DMTTVISQ EQU   3                   V/L I/S file                                 
DMTTOIOQ EQU   255                 IOOTHER entry                                
                                                                                
DMTCLCL  DS    X                   Compare length (-1 for EX)                   
DMTKLEN  DS    X                   Key length                                   
                                                                                
         DS    0D                  ** Doublword alignment **                    
DMTVALS  DS    XL(TRPVALL)         Trap data                                    
                                                                                
DMTTABL  EQU   *-DMTTABD                                                        
RUNNER   CSECT ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'*DMTTAB**DMTTAB**DMTTAB**DMTTAB*'                              
DMTTAB   DC    64XL(DMTTABL)'00'   File traps                                   
                                                                                
TRPTABD  DSECT ,                   ** TRPTAB below **                           
                                                                                
TRPAMOD  DS    A                   Pointer to module table entry                
                                                                                
         DS    0D                  ** Doublword alignment **                    
TRPVALS  DS    XL(TRPVALL)         Trap data                                    
                                                                                
TRPTABL  EQU   *-TRPTABD                                                        
                                                                                
***********************************************************************         
* Trap values - used for both module and datamanager traps above      *         
***********************************************************************         
                                                                                
TRPVALD  DSECT ,                   Must be doubleword aligned                   
                                                                                
TRPNREQ  DS    F                   Number of requests                           
TRPQREQ  DS    XL3                 Current request value                        
TRPNEST  DS    X                   Owner nest level (module trap only)          
                                                                                
TRPPARM  DS    C                   ** Parameter value **                        
TRPPALLQ EQU   C'*'                Print all trace entries                      
TRPPSUMQ EQU   C'+'                Print summary trace only                     
                                                                                
TRPQVAL  DS    0X                  ** Current request values **                 
TRPQCPU  DS    D                   CPU units                                    
TRPQSRB  DS    D                   SRB units                                    
TRPQIOS  DS    F                   I/O count                                    
TRPQCALL DS    F                   Calls                                        
TRPQVALL EQU   *-TRPQVAL                                                        
                                                                                
TRPRVAL  DS    0X                  ** All request values **                     
TRPRCPU  DS    D                   CPU units                                    
TRPRSRB  DS    D                   SRB units                                    
TRPRIOS  DS    F                   I/O count                                    
TRPRCALL DS    F                   Calls                                        
TRPRVALL EQU   *-TRPRVAL                                                        
                                                                                
TRPCVAL  DS    0D                  ** Current call values **                    
TRPCCPU  DS    D                   CPU units                                    
TRPCSRB  DS    D                   SRB units                                    
TRPCIOS  DS    F                   I/O count                                    
         DS    F                   n/d                                          
TRPCVALL EQU   *-TRPCVAL                                                        
                                                                                
TRPVALL  EQU   *-TRPVALD                                                        
RUNNER   CSECT ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'*TRPTAB**TRPTAB**TRPTAB**TRPTAB*'                              
TRPTAB   DC    ((TNDXMAX)*(TRPTABL))X'00'                                       
                                                                                
SAVSIZEQ EQU   128*ONEK            Size of each server save area                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*WRKBLK1*WRKBLK1*WRKBLK1*WRKBLK1'                              
WRKBLK1  DC    (WRKIOBL)X'00'      WRKIO control block 1                        
         ORG   WRKBLK1+(WRKIAREC-WRKIOB)                                        
         DC    A(WRKIOA1)                                                       
         ORG   WRKBLK1+(WRKIABUF-WRKIOB)                                        
         DC    A(WRKBUFF1)                                                      
         ORG   WRKBLK1+(WRKIACOM-WRKIOB)                                        
         DC    A(COMFACS)                                                       
         ORG   ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'*WRKBUF1*WRKBUF1*WRKBUF1*WKKBUF1'                              
WRKBUFF1 DC    (14*ONEK)X'00'      Worker buffer area 1                         
WRKBUFFL EQU   *-WRKBUFF1                                                       
                                                                                
         DS    0D                                                               
         DC    C'*WRKIOA1*WRKIOA1*WRKIOA1*WKKIOA1'                              
WRKIOA1  DC    (14*ONEK)X'00'      Worker I/O area 1                            
                                                                                
         DS    0D                                                               
         DC    C'*WRKBLK2*WRKBLK2*WRKBLK2*WRKBLK2'                              
WRKBLK2  DC    (WRKIOBL)X'00'      WRKIO control block 2                        
         ORG   WRKBLK2+(WRKIAREC-WRKIOB)                                        
         DC    A(WRKIOA2)                                                       
         ORG   WRKBLK2+(WRKIABUF-WRKIOB)                                        
         DC    A(WRKBUFF2)                                                      
         ORG   WRKBLK2+(WRKIACOM-WRKIOB)                                        
         DC    A(COMFACS)                                                       
         ORG   ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'*WRKBUF2*WRKBUF2*WRKBUF2*WKKBUF2'                              
WRKBUFF2 DC    (14*ONEK)X'00'      Worker buffer area 2                         
                                                                                
         DS    0D                                                               
         DC    C'*WRKIOA2*WRKIOA2*WRKIOA2*WKKIOA2'                              
WRKIOA2  DC    (14*ONEK)X'00'      Worker I/O area 2                            
                                                                                
         DS    0D                                                               
         DC    C'*WRKBLKR*WRKBLKR*WRKBLKR*WRKBLKR'                              
WRKBLKR  DC    (WRKIOBL)X'00'      FACWRK WRKIO control block                   
         ORG   WRKBLKR+(WRKIAREC-WRKIOB)                                        
         DC    A(WRKIOAR)                                                       
         ORG   WRKBLKR+(WRKIABUF-WRKIOB)                                        
         DC    A(WRKBUFFR)                                                      
         ORG   WRKBLKR+(WRKIACOM-WRKIOB)                                        
         DC    A(COMFACS)                                                       
         ORG   ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'*WRKBUFR*WRKBUFR*WRKBUFR*WKKBUFR'                              
WRKBUFFR DC    (14*ONEK)X'00'      Worker buffer area                           
                                                                                
         DS    0D                                                               
         DC    C'*WRKIOAR*WRKIOAR*WRKIOAR*WRKIOAR'                              
WRKIOAR  DC    (14*ONEK)X'00'      Worker I/O area                              
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*DDLINKD*DDLINKD*DDLINKD*DDLINKD'                              
DDLINKW  DC    (10*ONEK)X'00'      DDLINK work area                             
                                                                                
         DS    0D                                                               
         DC    C'*LS_D****LS_D****LS_D****LS_D***'                              
DDLINKS  DC    (LS_LNQ)X'00'       DDLINK save area                             
                                                                                
         DS    0D                                                               
         DC    C'**LIOB****LIOB****LIOB****LIOB**'                              
LIOBB    DC    (L'LIOB)X'00'       LINKIO control block                         
         ORG   LIOBB+(LIOBALPD-LIOBD)                                           
         DC    A(LP_DB)            DDLINK control block                         
         ORG   ,                                                                
                                                                                
         DS    0D                                                               
         DC    C'*TWAAREA*TWAAREA*TWAAREA*TWAAREA'                              
TWA      DC    (18*ONEK)X'00'      TWA area                                     
TWAX     DS    0X                                                               
                                                                                
         DS    0D                                                               
         DC    C'***WMP*****WMP*****WMP*****WMP**'                              
WMPL     EQU   X'FFFF'             Maximum that will fit into 2 bytes           
WMP      DC    (WMPL+1)X'00'       Work map pool                                
                                                                                
         DS    0D                                                               
         DC    C'*FASECD**FASECD**FASECD**FASECD*'                              
SEC      DC    (SECLENQ)X'00'      SECRET control block                         
                                                                                
         DS    0D                                                               
         DC    C'*ERRWORK*ERRWORK*ERRWORK*ERRWORK'                              
ERRWORK  DC    (20*ONEK)X'00'      Abend working storage chain                  
         EJECT                                                                  
***********************************************************************         
* MQ global values                                                    *         
***********************************************************************         
                                                                                
         DS    0D                                                               
         DC    C'*MQGLOB**MQGLOB**MQGLOB**MQGLOB*'                              
MQGLOB   DS    0D                                                               
                                                                                
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
ACSQBOPN DC    A(0)                                                             
CSQBCONN DC    CL8'CSQBCONN'                                                    
ACSQBCON DC    A(0)                                                             
CSQBDISC DC    CL8'CSQBDISC'                                                    
ACSQBDSC DC    A(0)                                                             
CSQBCOMM DC    CL8'CSQBCOMM'                                                    
ACSQBCOM DC    A(0)                                                             
CSQBCLOS DC    CL8'CSQBCLOS'                                                    
ACSQBCLO DC    A(0)                                                             
CSQBGET  DC    CL8'CSQBGET'                                                     
ACSQBGET DC    A(0)                                                             
CSQBPUT  DC    CL8'CSQBPUT'                                                     
ACSQBPUT DC    A(0)                                                             
                                                                                
         PUSH ACONTROL                                                          
         ACONTROL COMPAT(NOCASE),FLAG(NOPAGE0)                                  
         CMQA  LIST=YES,EQUONLY=NO                                              
         POP ACONTROL                                                           
                                                                                
MQMDESC  CMQMDA DSECT=NO,LIST=YES                                               
         ORG   MQMDESC_FORMAT                                                   
         DC    CL8'MQSTR   '                                                    
         ORG   ,                                                                
                                                                                
MQCON    DS    0F                  ** MQ connect parameters **                  
         DC    CL(L'MQ_BLKID)'MQ connect'                                       
         DC    AL1(MQ_TCONT,0,0,0)                                              
         DC    A(ACSQBCON)         A(CSQBCONN)                                  
         DC    A(MQQMNAME)                                                      
         DC    A(MQCONHDL)                                                      
         DC    A(MQCOMPC)                                                       
         DC    X'80',AL3(MQREASN)                                               
                                                                                
MQCONHDL DC    A(0)                Connection handle                            
                                                                                
MQQMNAME DC    CL(L'MQEDIDSC_OBJECTQMGRNAME)' '                                 
                                                                                
MQOPN    DS    0F                  ** MQ open control blcok **                  
         DC    CL(L'MQ_BLKID)'MQ open'                                          
         DC    AL1(MQ_TOPEN,0,0,0)                                              
         DC    A(ACSQBOPN)         A(CSQBOPEN)                                  
         DC    A(MQCONHDL)                                                      
MQOPNQNM DC    A(0)                CMQODA                                       
         DC    A(MQOPENO)                                                       
MQOPNHOB DC    A(0)                Handle to object                             
         DC    A(MQCOMPC)                                                       
         DC    X'80',AL3(MQREASN)                                               
MQOPENO  DC    A(0)                Open options                                 
                                                                                
MQCLO    DS    0F                  ** MQ close control block **                 
         DC    CL(L'MQ_BLKID)'MQ close'                                         
         DC    AL1(MQ_TCLOS,0,0,0)                                              
         DC    A(ACSQBCLO)         A(CSQBCLO)                                   
         DC    A(MQCONHDL)                                                      
MQCLOHOB DC    A(0)                                                             
         DC    A(MQCLOSO)                                                       
         DC    A(MQCOMPC)                                                       
         DC    X'80',AL3(MQREASN)                                               
MQCLOSO  DC    A(0)                Close options                                
                                                                                
MQDSC    DS    0F                  ** MQ disconnect control block **            
         DC    CL(L'MQ_BLKID)'MQ disconnect'                                    
         DC    AL1(MQ_TDISC,0,0,0)                                              
         DC    A(ACSQBDSC)                                                      
         DC    A(MQCONHDL)                                                      
         DC    A(MQCOMPC)                                                       
         DC    X'80',AL3(MQREASN)                                               
                                                                                
MQCMT    DS    0F                  ** MQ commit control block **                
         DC    CL(L'MQ_BLKID)'MQ commit'                                        
         DC    AL1(MQ_TCOMT,0,0,0)                                              
         DC    A(ACSQBCOM)         A(CSQBCOMM)                                  
         DC    A(MQCONHDL)                                                      
         DC    A(MQCOMPC)                                                       
         DC    X'80',AL3(MQREASN)                                               
                                                                                
MQGETEDI DS    0F                  ** MQ get control block **                   
         DC    CL(L'MQ_BLKID)'MQ get EDI Q'                                     
         DC    AL1(MQ_TGETM,0,MQEDIBUF-MQGETEDI,0)                              
         DC    A(ACSQBGET)         A(CSQBGET)                                   
         DC    A(MQCONHDL)                                                      
         DC    A(MQEDIHOB)         Queue HOB                                    
         DC    A(MQMDESC)                                                       
         DC    A(MQEDIOPT)                                                      
         DC    A(MQIBUFFL)         Max buffer length                            
MQEDIBUF DC    A(0)                A(MQ input buffer)                           
         DC    A(MQLIMSG)                                                       
         DC    A(MQCOMPC)                                                       
         DC    X'80',AL3(MQREASN)                                               
                                                                                
MQEDIHOB DC    A(0)                Input queue handle                           
                                                                                
MQEDIOPT CMQGMOA DSECT=NO,LIST=YES Get options for EDI input queue              
                                                                                
MQEDIDSC CMQODA  DSECT=NO,LIST=YES EDI input queue                              
                                                                                
MQEDINAM DC    CL(L'MQEDIDSC_OBJECTNAME)' '                                     
                                                                                
MQPUTEDI DS    0F                  ** MQ put output parameters **               
         DC    CL(L'MQ_BLKID)'MQ put EDI Q'                                     
         DC    AL1(MQ_TPUTM,0,0,MQEDOBUF-MQPUTEDI)                              
         DC    A(ACSQBPUT)                                                      
         DC    A(MQCONHDL)                                                      
         DC    A(MQEDOHOB)                                                      
         DC    A(MQMDESC)                                                       
         DC    A(MQEDOOPT)                                                      
         DC    A(MQLOMSG)                                                       
MQEDOBUF DC    A(0)                A(MQ output buffer)                          
         DC    A(MQCOMPC)                                                       
         DC    X'80',AL3(MQREASN)                                               
                                                                                
MQEDOHOB DC    A(0)                EDI output queue handle                      
                                                                                
MQEDOOPT CMQPMOA DSECT=NO,LIST=YES Put options for EDI output queue             
                                                                                
MQEDODSC CMQODA  DSECT=NO,LIST=YES EDI output queue                             
                                                                                
MQEDONAM DC    CL(L'MQEDODSC_OBJECTNAME)' '                                     
                                                                                
MQTRLIT1 DC    C'<<< Start of MQ input trace >>>'                               
MQTRLIT2 DC    C'<<< Start of MQ output trace >>>'                              
MQTRLIT3 DC    C'<<< End of MQ input trace >>>'                                 
MQTRLIT4 DC    C'<<< End of MQ output trace >>>'                                
MQERLIT1 DC    C'MQ condition code:- '                                          
MQERLIT2 DC    C'Warning'                                                       
MQERLIT3 DC    C'Failure'                                                       
MQERLIT4 DC    C'Unknown:- '                                                    
MQERLIT5 DC    C'MQ reason code:- '                                             
MQSNDLIT DC    C'MQ message sent'                                               
                                                                                
         DS    0D                                                               
         DC    C'*RUNWORK*RUNWORK*RUNWORK*RUNWORK'                              
RUNWORK  DC    (300*ONEK)X'00'     Regular working storage chain                
RUNWORKX DS    0X                                                               
         EJECT                                                                  
WORKD    DSECT ,                   ** RUNNER local w/s **                       
                                                                                
ARUNNER  DS    A                   A(RUNNER)                                    
SAVERD   DS    A                   Saved RD value (for cancel)                  
DMGR     DS    V                   A(DataManager)                               
VDDLINK  DS    V                   A(DDLINK)                                    
AIO      DS    A                   A(I/O area)                                  
AQMBUFF  DS    A                   A(QMAINT report buffer)                      
DEMVERP  DS    A                   PARMS FOR DEMVER                             
                                                                                
PARM     DS    6F                  General parameter list                       
RUNPARA  DS    6F                  Server RUNPARMS                              
                                                                                
DMCBSAV  DS    XL(6*4)             Save area for DMCB (See ADWAIT)              
                                                                                
* FOLLOWING ARE CPU TIMES IN MICROSECONDS                                       
* CPUSAVE IS THE VALUE FROM THE LATEST TIMEUSED CALL (SEE GETCPU)               
* CPUREQ IS INITIALLY TIMEUSED BEFORE CALLING DDLINK, THEN ON RETURN            
* IT BECOMES THE DIFFERENCE BETWEEN THAT AND TIMEUSED ON RETURN, I.E.           
* CPU TIME USED BY THE REQUEST DURING DDLINK AND THE SERVER.                    
* CPUENDRQ IS TIMEUSED AS AT THE END OF THE LAST REQUEST. IT'S USED TO          
* CALCULATE THE CPU USED FROM END OF LAST REQUEST TO END OF THE CURRENT         
* REQUEST, I.E. THE TIME FOR THE WHOLE REQUEST. IT WOULD BE EQUAL TO            
* CPUREQ PLUS ANY RUNNER OVERHEAD SETTING UP THE TASK.                          
                                                                                
CPUSAVE  DS    D                   VALUE FROM TIMEUSED                          
CPUREQ   DS    D                   Total CPU unit for DDLINK & server           
CPUENDRQ DS    D                   CPUSAVE AS AT END OF LAST REQUEST            
WASCB    DS    A                   A(ASCB) from extract                         
WTIOT    DS    A                   A(TIOT) from extract                         
WCOMM    DS    A                   A(COMM) from extract                         
                                                                                
AOPERECB DS    A                   A(ECB of operator interrupt)                 
ECBCOMS  DS    A                   Operator communications ECB                  
                                                                                
ECBLIST  DS    0A                  ** ECB list for waiting **                   
ECBLOPR  DS    A                   Operator ECB                                 
ECBLWRK  DS    A                   Post ECB (for wake-up)                       
ECBLMQG  DS    A                   Mq get ecb if MQRUNQ=NO                      
ECBLDSP  DS    A                   Global operator ECB                          
                                                                                
ECBPOSTQ EQU   X'40'               ECB posted                                   
ECBLASTQ EQU   X'80'               End of list flag                             
                                                                                
MAXIOS   DS    F                   Maximum N'I/Os for request                   
BIGIOS   DS    F                   Maximum N'I/Os for LOGREQ                    
BIGSEG   DS    F                   Maximum N'Segs for LOGREQ                    
BIGCPU   DS    F                   Maximum CPU units for LOGREQ                 
                                                                                
MAXQTIME DS    F                   Maximum allowable queue time                 
AVGQTIME DS    F                   Cummulative before e-mail sent               
SAMPLE   DS    F                   Sample size for averages                     
                                                                                
TOTQVAL  DS    F                   Accumulated queue time                       
SAMPCNT  DS    F                   Current sample count                         
                                                                                
CPURTP   DS    F                   Accumulated CPU for RTPCPU limit             
CPURTPNX DS    F                   CPU value for next RTPCPU limit              
                                                                                
TNUMREQ  DS    F                   Total number of server requests              
TQUETIM  DS    F                   Total queue time                             
TTOTTIM  DS    F                   Total elapsed time                           
TTOTSIO  DS    F                   Total I/O count                              
TWRKADD  DS    F                   Total output records                         
TTOTCPU  DS    D                   Total CPU unit                               
                                                                                
CPUADJ   DS    F                   CPU time adjustment factor                   
SRBADJ   DS    F                   SRB time adjustment factor                   
TOTADJ   DS    F                   Total time adjustment factor                 
                                                                                
QTIMTMEM DS    F                   Time of last QTIMLIT email                   
QTIMTMWT DS    F                   Time of last QTIMLIT WTO                     
ATIMTMEM DS    F                   Time of last ATIMLIT email                   
ATIMTMWT DS    F                   Time of last ATIMLIT WTO l                   
                                                                                
CPUOPT   DS    C                   CPU run option                               
SRBOPT   DS    C                   SRB run option                               
TOTOPT   DS    C                   Total run option                             
                                                                                
SVRADDR  DS    V                   A(server application code)                   
SVRFACS  DS    A                   A(server system facilities list)             
SVRABND  DS    A                   A(server abend notification list)            
SVRSLOW  DS    A                   A(server slow notification list)             
                                                                                
AIOOTHER DS    A                   A(IOOTHER entry)                             
                                                                                
QUAD     DS    L                   For number validation                        
                                                                                
SVRSYSP  DS    CL(L'RSVRSYSP)      Server sysphase                              
SVRFTYP  DS    CL(L'RSVRFTYP)      Server file type                             
SVRFOPA  DS    CL(L'RSVRFOPA)      Server file open type                        
                                                                                
SVRINDS  DS    0XL(L'RSVRIND1+L'RSVRIND2)                                       
SVRIND1  DS    XL(L'RSVRIND1)      Server indicator byte 1                      
SVRIND2  DS    XL(L'RSVRIND2)      Server indicator byte 2                      
                                                                                
SVRSYS   DS    0XL4                ** Systems which can be run **               
SVRSYSN1 DS    XL(L'RSVRSYS1)      Native system 1 number                       
SVRFILE1 DS    AL3                 A(server system/files 1 info)                
SVRSYSN2 DS    XL(L'RSVRSYS2)      Native system 2 number                       
SVRFILE2 DS    AL3                 A(server system/files 2 info)                
SVRSYSN  EQU   (*-SVRSYS)/L'SVRSYS                                              
                                                                                
QUEWIDTH DS    H                   Width of server entry                        
SVRWIDTH DS    H                   Width of queue entry                         
DMTNUM   DS    H                   Number of entries in DMTTAB                  
TRPNUM   DS    H                   Number of entries in TRPTAB                  
TRAPNEST DS    X                   Trap routine nesting level                   
TRAPMAXN EQU   32                  Maximum number of nested calls               
                                                                                
NSERVERS DS    X                   Number of server phases loaded               
                                                                                
RUNISVRS DS    X                   ** Server indicators 1 **                    
RUNISVR1 EQU   X'80'               SERVER1= card processed                      
RUNISVR2 EQU   X'40'               SERVER2= card processed                      
RUNISVR3 EQU   X'20'               SERVER3= card processed                      
RUNISVR4 EQU   X'10'               SERVER4= card processed                      
RUNISVR5 EQU   X'08'               SERVER5= card processed                      
RUNISVR6 EQU   X'04'               SERVER6= card processed                      
RUNISVR7 EQU   X'02'               SERVER7= card processed                      
RUNISVR8 EQU   X'01'               SERVER8= card processed                      
                                                                                
RUNISVRT DS    X                   ** Server indicators 2 **                    
RUNISVR9 EQU   X'80'               SERVER9= card processed                      
RUNISVRA EQU   X'40'               SERVER10= card processed                     
RUNISVRB EQU   X'20'               SERVER11= card processed                     
RUNISVRC EQU   X'10'               SERVER12= card processed                     
RUNISVRD EQU   X'08'               SERVER13= card processed                     
RUNISVRE EQU   X'04'               SERVER14= card processed                     
RUNISVRF EQU   X'02'               SERVER15= card processed                     
RUNISVRG EQU   X'01'               SERVER16= card processed                     
                                                                                
RUNINDS1 DS    X                   ** Run indicators 1 **                       
RUNIBTCH EQU   X'80'               Running in batch mode                        
RUNIUPDT EQU   X'40'               Running in updative (global) mode            
RUNISMTP EQU   X'20'               SMTP initialized                             
RUNIMQQM EQU   X'10'               MQ queue manager name given                  
RUNIFALK EQU   X'08'               FALINK input being processed                 
RUNIFAFS EQU   X'04'               First FALINK card processed                  
RUNIGOMQ EQU   X'02'               Go to RUNMQ                                  
RUNILINK EQU   X'01'               DDLINK initialized                           
                                                                                
RUNINDS2 DS    X                   ** Run indicators 2 **                       
RUNIRIDF EQU   X'80'               Running under IDF                            
RUNIAGYC EQU   X'40'               AGENCY= control card input                   
RUNIFPKC EQU   X'20'               FACPAK= control card input                   
RUNISYSM EQU   X'10'               DUMP=SYSM USED                               
RUNISTAT EQU   X'08'               STATE=Y USED                                 
RUNIMEDZ EQU   X'04'               MEDZ is down                                 
                                                                                
RUNINDMQ DS    X                   ** Run indicators for MQ **                  
RUNIMQIT EQU   X'80'               MQ input trace                               
RUNIMQOT EQU   X'40'               MQ output trace                              
RUNIMQCI EQU   X'20'               MQ message id present                        
RUNIMQUP EQU   X'10'               MQ upload in process                         
RUNIACK2 EQU   X'08'               MQ acknowledgements to be sent               
RUNIMQNR EQU   X'04'               Don't use run q for MQ (MQRUNQ=N)            
RUNIMQEI EQU   X'02'               EDIIQ specified                              
RUNIMQEO EQU   X'01'               EDIOQ specified                              
RUNIMQQS EQU   RUNIMQEI+RUNIMQEO                                                
                                                                                
MQVARS   DS    0F                  ** MQ variables **                           
MQDATAA  DS    A                   A(data string)                               
MQDATAL  DS    H                   L'data string                                
MQCALL   DS    X                   MQI calling mode                             
MQFLAG   DS    X                   ** Flag byte **                              
MQFFRST  EQU   0                   First time for new buffer                    
MQFNEXT  EQU   1                   Next time for current buffer                 
MQVARL   EQU   *-MQVARS                                                         
MQINIFLG DS    C                   SET to Y on MQINIT                           
                                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
FULL3    DS    F                                                                
FULL4    DS    F                                                                
HALF1    DS    H                                                                
HALF2    DS    H                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
                                                                                
SAVESIN DS     F                                                                
                                                                                
CARD     DS    CL80                SYSIN reading area                           
                                                                                
NUMTYPE  DS    C                   Number type for VNUM call                    
WORKING  DS    C                   YESQ if running work                         
OPMESS   DS    C                   Ask operator if task abends                  
OPREPLY  DS    C                   Ask for reply to runaway task msgs           
QMAINTR  DS    C                   Send QMAINT report (Y/N)                     
INFOMESS DS    C                   Output information messages                  
ENDRUNFL DS    C                   Set to Y if endrun pending                   
ADWAITNT DS    C                   Set to Y IF ADWAIT entered                   
PUTOPTYP DS    C                   Error type for PUTOPS routine                
LOOPINST DS    C                   Saved instruction from loop abend            
PAUSEFLG DS    X                   Operator PAUSE between requests              
PAUSEPQ  EQU   X'40'               - pause pending                              
PAUSEAQ  EQU   X'80'               - pause active                               
ADWCAN   DS    C                   TSTCAN entry flag                            
                                                                                
ABEND    DS    CL64                Abend notification list                      
SLOW     DS    CL64                Slows notification list                      
                                                                                
FSEGS    DS    0XL(L'FPREV+L'FNEXT)                                             
FPREV    DS    XL(L'LQ_FPREV)      Previous segment                             
FNEXT    DS    XL(L'LQ_FNEXT)      Next segment                                 
                                                                                
SVTRACE  DS    CL(L'MCTRACE)       Saved trace value                            
SVVPRINT DS    AL(L'MCVPRINT)      Saved print address                          
REQTRACE DS    X'00'               REQUEST=# or FULL                            
                                                                                
WKEY     DS    0XL(L'TQWKEY)       ** Worker key **                             
WKEYUID  DS    XL2                 User-id number                               
WKEYSPS  DS    0CL(L'WKEYSYS+L'WKEYPRG+L'WKEYSPG)                               
WKEYSYS  DS    C                   System                                       
WKEYPRG  DS    CL2                 Program                                      
WKEYSPG  DS    C                   Sub-program                                  
WKEYDAY  DS    P                   Day (pwos)                                   
WKEYCLS  DS    C                   Class                                        
WKEYSEQ  DS    XL2                 Workfile sequence number                     
                                                                                
         ORG   WKEY                                                             
WKEYZERO DS    XL(L'TQMQZERO)      MQ message indicator                         
WKEYASID DS    XL(L'TQMQASID)      ASID of scheduler                            
WKEYDATE DS    XL(L'TQMQDATE)      Date scheduled (compressed)                  
WKEYTIME DS    XL(L'TQMQTIME)      Time scheduled (TU's)                        
                                                                                
REQNO    DS    XL(L'MCREQNO)       Current request number                       
MAXPOPS  DS    F                   Maximum N'timer pops allowed                 
                                                                                
MAXDUMPS DS    AL3                 Maximum N'dumps allowed                      
MAXDDLY  DS    AL1                 Maximum N'deadly embrace retries             
                                                                                
LOGRECS  DS    AL2                 N'log records                                
LOGDATA  DS    CL(L'P-1)           Log data                                     
                                                                                
SWAPABLE DS    C                   C'Y'=swapable                                
                                                                                
ERROR    DS    X                   Error return byte                            
CANCEL   DS    X                   Non-zero if canceling request                
                                                                                
MVSJOB   DS    CL8                 Job number as C'JOBnnnnn'                    
MVSJNUM  DS    XL2                 Job number as XL2'nnnnn'                     
                                                                                
         DS    0F                                                               
THISBLK  DS    0XL10                                                            
THISCOMM DS    F                   A(OPS COMMAND IN DATASPACE)                  
THISTIME DS    F                   TIME OF OPS COMMAND                          
THISJOB  DS    H                   JOB NUMBER OF COMMAND                        
                                                                                
ABENDSE  DS    XL1                 UTL+4 at ABEND                               
                                                                                
DDLYNUM  DS    X                   Mumber of deadly embrace retries             
                                                                                
RUNTYPE  DS    X                   ** Type of work **                           
RUNTCMND EQU   1                   RUNNER command file                          
RUNTSRVR EQU   2                   Server work file                             
                                                                                
ALET     DS    A                   Tabs ALET                                    
OFFSET   DS    A                   Offset                                       
                                                                                
ASVR     DS    A                   A(dataspace server entry)                    
AQUE     DS    A                   A(dataspace queue entry)                     
                                                                                
DSPACE   DS    CL12                Data space name                              
ASID     DS    H                   My ASID                                      
ADSJOB   DS    A                   MY DSJOBTAB ENTRY                            
                                                                                
FACPAKNM DS    CL4                 FACPAK name                                  
FACPAKID DS    X                   FACPAK id                                    
                                                                                
AGYFILT  DS    CL(TSTFAGY#*L'TSTFAGYS)                                          
                                                                                
REPLY    DS    CL10                Operator reply area                          
                                                                                
ESTAERD  DS    A                   Abend exit saved RD                          
ESTAER1  DS    A                   Abend exit saved R1                          
                                                                                
NOTLSTN  DS    AL2                 Actual N'entries in NOTLST                   
                                                                                
TSARUIDS DS    XL(TSPXTNL)         TSAR block for UID records                   
                                                                                
UIDREC   DS    XL(UIDRECL)         User-id buffer record (see UIDRECD)          
                                                                                
OPMESSA  DS    0CL(L'LOPMESSA)                                                  
         DS    CL11                                                             
OPMASTEP DS    CL8                                                              
         DS    CL5                                                              
OPMAREQ  DS    CL7                                                              
         DS    CL4                                                              
OPMAET   DS    CL8                                                              
         ORG   OPMESSA+L'OPMESSA                                                
                                                                                
OPMESSB  DS    0CL(L'LOPMESSB)                                                  
         DS    CL4                                                              
OPMBTCB  DS    CL9                                                              
         DS    CL6                                                              
OPMBSIOS DS    CL7                                                              
         DS    CL5                                                              
OPMBLNS  DS    CL6                                                              
         DS    CL5                                                              
OPMBPGS  DS    CL5                                                              
         ORG   OPMESSB+L'OPMESSB                                                
                                                                                
OPMESSC  DS    CL(L'LOPMESSC)                                                   
                                                                                
OPMESSL  EQU   *-OPMESSA                                                        
                                                                                
PCMESS1  DS    0CL(L'LPCMESS1)                                                  
         DS    CL11                                                             
PCM1STEP DS    CL8                                                              
         DS    CL24                                                             
PCM1SEN  DS    CL2                                                              
         DS    CL6                                                              
PCM1FILE DS    CL7                                                              
         ORG   PCMESS1+L'PCMESS1                                                
                                                                                
PCMESS2  DS    CL(L'LPCMESS2)                                                   
                                                                                
PCMESSA  DS    0CL(L'LPCMESSA)                                                  
         DS    CL11                                                             
PCMASTEP DS    CL8                                                              
         DS    CL5                                                              
PCMAREQN DS    CL7                                                              
         DS    CL9                                                              
PCMAMODN DS    CL8                                                              
         ORG   PCMESSA+L'PCMESSA                                                
                                                                                
PCMESSB  DS    0CL(L'LPCMESSB)                                                  
         DS    CL1                                                              
PCMBDISP DS    CL4                                                              
         DS    CL1                                                              
PCMNFAIL DS    CL6                                                              
         DS    CL14                                                             
PCMRSNC  DS    CL20                                                             
         ORG   PCMESSB+L'PCMESSB                                                
                                                                                
PCMESSC  DS    CL(L'LPCMESSC)                                                   
                                                                                
PCMESSL  EQU   *-PCMESSA                                                        
                                                                                
INMESSA  DS    0CL(L'LINMESSA)                                                  
INMCPUL DS     CL4                                                              
INMCPU   DS    CL7                                                              
         ORG   INMESSA+L'INMESSA                                                
                                                                                
INMESSB  DS    0CL(L'LINMESSB)                                                  
INMTIMEL DS    CL5                                                              
INMTIME  DS    CL7                                                              
         ORG   INMESSB+L'INMESSB                                                
                                                                                
INMESSC  DS    0CL(L'LINMESSC)                                                  
INMEXCPL DS    CL5                                                              
INMEXCP  DS    CL7                                                              
         ORG   INMESSC+L'INMESSC                                                
                                                                                
INMESSD  DS    0CL(L'LINMESSD)                                                  
INMPAGEL DS    CL6                                                              
INMPAGES DS    CL7                                                              
         ORG   INMESSD+L'INMESSD                                                
                                                                                
INMESSE  DS    CL(L'LINMESSE)                                                   
                                                                                
INMESSL  EQU   *-INMESSA                                                        
                                                                                
NOTLSTM  EQU   200                 Maximum N'entries in NOTLST                  
NOTLST   DS    (NOTLSTM)XL(L'TQWKEY)                                            
                                                                                
IO       DS    XL(4*ONEK)          General I/O area                             
QMBUFF   DS    XL(TABQUELN+1)      QMAINT report buffer                         
                                                                                
RUNWORKL EQU   *-WORKD                                                          
                                                                                
MQ_D     DSECT ,                   ** MQ control block **                       
                                                                                
MQ_BLKID DS    CL16                Block description                            
                                                                                
MQ_TYPE  DS    AL1                 ** Function type **                          
MQ_TCONT EQU   1                   MQ connect                                   
MQ_TOPEN EQU   2                   MQ open                                      
MQ_TGETM EQU   3                   MQ get message                               
MQ_TPUTM EQU   4                   MQ put message                               
MQ_TCOMT EQU   5                   MQ commit                                    
MQ_TCLOS EQU   6                   MQ close                                     
MQ_TDISC EQU   7                   MQ disconnect                                
                                                                                
MQ_INDS1 DS    X                   ** indicator flags 1 **                      
                                                                                
MQ_IBUFF DS    AL1                 Disp. to input buffer address                
MQ_OBUFF DS    AL1                 Disp. to output buffer address               
                                                                                
MQ_AROUT DS    A                   A(A(MQ routine))                             
MQ_PARMS DS    0F                  Parameter list                               
         EJECT                                                                  
       ++INCLUDE FATABSRUN                                                      
         EJECT                                                                  
       ++INCLUDE DDRUNNERD                                                      
         EJECT                                                                  
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         ORG   P                                                                
         DS    C                                                                
PHH      DS    CL2                                                              
PHHDEL   DS    C                                                                
PHHDELQ  EQU   COLON                                                            
PMM      DS    CL2                                                              
PMMDEL   DS    C                                                                
PMMDELQ  EQU   COLON                                                            
PSS      DS    CL2                                                              
PSSDEL   DS    C                                                                
PSSDELQ  EQU   C'.'                                                             
PDS      DS    CL2                                                              
         DS    C                                                                
PMESSAGE DS    0CL(L'P-(*-P))                                                   
                                                                                
         ORG   P                                                                
PQMFACID DS    CL(L'TQLFACID)                                                   
PQMTSKID DS    CL(L'TQLTSKID)                                                   
PQMTYPE  DS    CL(L'TQTYPE)                                                     
PQMSTAT  DS    CL(L'TQSTATUS)                                                   
         DS    C                                                                
PQMTIME  DS    CL12                                                             
         DS    C                                                                
PQMFILE  DS    CL(PRTKEYLQ)                                                     
         DS    C                                                                
PQMACTN  DS    0C                                                               
         ORG   ,                                                                
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDWRKIOD                                                                      
         PRINT OFF                                                              
WRKIOD   DSECT ,                                                                
       ++INCLUDE DDWRKIOD                                                       
         PRINT ON                                                               
* DDLINKD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLINKD                                                        
         PRINT ON                                                               
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
* DDLINKIOD                                                                     
         PRINT OFF                                                              
LIOBD    DSECT ,                                                                
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
* DDRUNSMFD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDRUNSMFD                                                      
         PRINT ON                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
COMFACSN EQU   (*-COMFACSD)/4                                                   
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
                                                                                
CTLSTD   DSECT ,                   ** Define SE list entry **                   
         ORG   CTLSTDTA                                                         
CTLSTNAM DS    CL7                 SE name                                      
CTLSTSYS DS    X                   CALLOV system number                         
CTLSTSEN DS    X                   SE number                                    
         PRINT ON                                                               
*&&UK                                                                           
* DDMEDFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDMEDFACS                                                      
         PRINT ON                                                               
* FAXPTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAXPTABD                                                       
         PRINT ON                                                               
*&&                                                                             
* DMREQHDRA                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMREQHDRA                                                      
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMDSYSHDR                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMDSYSHDR                                                      
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* IEZCIB/IHAASCB/IHASDWA/IHAASSB/IAZJSAB/IEZCOM/DCBD                            
         PRINT OFF                                                              
         DSECT ,                                                                
         IEZCIB                                                                 
         IHAASCB                                                                
         IHASDWA                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         IEZCOM                                                                 
         DCBD  DSORG=PS,DEVD=DA                                                 
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073DDRUNNER  11/21/20'                                      
         END                                                                    
