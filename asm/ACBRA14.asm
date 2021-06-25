*          DATA SET ACBRA14    AT LEVEL 007 AS OF 05/28/13                      
*PHASE T62414A                                                                  
*INCLUDE SORTER                                                                 
ACBRA14  TITLE '- BRA History/Salary upload server'                             
*                                                                               
**********************************************************************          
* BIGASS NOTE: If you change the mapping table for this upload you              
*     must replicate those changes in the 'CSV uploads available' call          
*     This is currently the 41 call to ACBRA1C.                                 
**********************************************************************          
*                                                                               
* Level change comments                                                         
* ---------------------                                                         
*NRAK 001 18MAR11 <PR000031> this book used to hold old eTime upload            
*NRAK     30SEP11 <br18643d> sal date can be beyond end date                    
*NSHE     02DEC11 Use SORTER instead of FACWRK to process records               
*NSHE 002 02SEP12 Change to IO routine for auto switching system                
*                                                                               
SVRDEF   CSECT                                                                  
         LKSVR TYPE=UO,CODE=ENTRY,RLEN=2000,REQUEST=*,WORKERKEY=BOSU,  x        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=ACCSYSQ,         x        
               FILES=FILES,SERVERTYPE=TSTBOTU,SYSPHASE=SYSPHASE,IDF=Y, x        
               PROGRAM=RCVPBRAQ                                                 
         EJECT                                                                  
*                                                                               
ENTRY    NMOD1 0,**BO14**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         BNZ   ENTRY02                                                          
         L     R9,LP_ABLK1                                                      
         ICM   R8,15,RSVRSAVE      R8=A(Save area)                              
         B     ENTRY04                                                          
                                                                                
ENTRY02  L     R9,RSVRSAVE                                                      
         ST    R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(Global w/s)                             
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
         USING SAVED,R8            R8=A(Save w/s)                               
         MVC   MASTC,RMASTC        Set A(MASTC)                                 
                                                                                
ENTRY04  ST    R8,LP_ABLK2                                                      
         MVC   WRKBLKR,RWRKBLKR    Set A(FACWRK WRKIO block)                    
         ST    RE,SRVRRELO         Save program relocation factor               
         MVC   RUNMODE,RUNPMODE    Set calling mode                             
         DROP  R6,R7                                                            
                                                                                
         LA    R7,HS_CUR                                                        
*        USING HS_D,R7             R7=A(Current time buffer record)             
OLD      USING HS_D,HS_OLD         Old time record                              
NEW      USING HS_D,HS_NEW         New time record                              
                                                                                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
                                                                                
***********************************************************************         
* Handle DDLINK/RUNNER modes                                          *         
***********************************************************************         
                                                                                
         CLI   RUNMODE,RRUNSTRQ    Test 'First for run' mode                    
         BE    SRUN                                                             
         CLI   RUNMODE,RINIREQQ    Test 'Initialise' mode                       
         BE    SREQ                                                             
         CLI   RUNMODE,RVALREQQ    Test 'Validate record' mode                  
         BE    UPLD                                                             
         CLI   RUNMODE,RRUNREQQ    Test 'Run request' mode                      
         BE    UPLD                                                             
         CLI   RUNMODE,RRUNENDQ    Test 'Last for request' mode                 
         JE    EREQ                                                             
         J     EXITY               Ignore any other modes                       
         EJECT                                                                  
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
         EJECT                                                                  
***********************************************************************         
* Handle 'First for request' (before first upload record) mode        *         
***********************************************************************         
                                                                                
SREQ     LA    R0,SAVEVAR                                                       
         LHI   R1,SAVEVARL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   TWAMODE,0           Set no errors encountered                    
         GOTO1 VDICTATE,DMCB,C'LL  ',DCDICTL,DSDICTL                            
*                                                                               
* ANY OTHER INIT HERE                                                           
*                                                                               
         L     RF,LP_ACOM          Extract A(LINKIO) from COMFACS               
         MVC   AALINKIO,CLINKIO-COMFACSD(RF)                                    
         MVC   AALIOB,LP_ALIOB     Set A(LIOB)                                  
*                                                                               
* one-time initialisation triggers for utilities - IMPORTANT!                   
         MVI   RUNINDS,0           runibufr,runilmax,RUNIPUTF                   
*                                                                               
         XC    RUN#LOCK,RUN#LOCK                                                
*                                                                               
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    SREQ02                                                           
*                                                                               
         GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0   open acc files               
         GOTOR DATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                                
*                                                                               
         L     RF,MASTC            Set I/O trace option                         
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
*                                                                               
SREQ02   GOTOR PROTOFF             Turn off storage protection                  
         LA    R0,LP_D                                                          
         ST    R0,LLP              Save A(LP_D) locally                         
         GOTOR PROTON              Turn on storage protection                   
*                                                                               
         MVC   ACOMFACS,LP_ACOM    Point to real copy of COMFACS                
*                                                                               
         MVI   GIND2,GI2ETIM       Set want 1R values returned                  
         GOTOR (#CPYINI,ACPYINI)   Initialise company values                    
         USING CPYELD,SCPYEL                                                    
*                                                                               
         GOTOR INIBUF              Initialise record buffer                     
*                                                                               
         TM    LP_FLAG,LP_FOFFL    Test on-line                                 
         JNZ   SREQ03                                                           
         GOTOR GETPAYTB            BUILD TABLE OF PAYCODES                      
SREQ03   DS    0H                                                               
*                                                                               
         TM    LP_FLAG,LP_FDRFT    Test validation mode                         
         JZ    SREQ04                                                           
         GOTOR (#SETFAC,ASETFAC),'BRO#HISL' Usage Stats for GCOR                
         J     EXITY                                                            
*                                                                               
* Now force all dmgr (including BRA01 etc) through my Handler routine           
* so we can redirect updative calls to FACWRK rather than the file              
SREQ04   LA    R0,COMFACS          Take local copy of COMFACS                   
         LHI   R1,COMFACSL                                                      
         L     RE,LP_ACOM                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    RF,COMFACS                                                       
         ST    RF,ACOMFACS         Set A(COMFACS) to local copy                 
         LARL  R0,DMGRITRN                                                      
         ST    R0,VDATAMGR         Set internal DATAMGR entry point             
         LARL  R0,DMGRXTRN                                                      
         ST    R0,CDATAMGR-COMFACSD(,RF) Set external DMGR entry point          
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Handle 'Run request' (upload a record) mode                         *         
***********************************************************************         
*                                                                               
UPLD     LA    RF,RECTAB                                                        
         USING RECTABD,RF          RF=A(Record map table)                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   Look up record map code                      
         BE    *+12                                                             
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                                                             
         MVC   RECTYPE,RECTTYPE    Set internal record type                     
         DROP  RF                                                               
                                                                                
         MVI   ERRTAB,ET_EOTQ      Clear error table                            
                                                                                
         CLI   RECTYPE,RECHHDRQ    Test header record                           
         JNE   UPLD020                                                          
         GOTOR UHDR                                                             
         J     EXIT                                                             
*                                                                               
UPLD020  CLI   RECTYPE,RECHELEQ    Test Time Line                               
         JNE   UPLD025                                                          
         GOTOR UELE                                                             
         J     EXIT                                                             
*                                                                               
UPLD025  DS    0H                                                               
         CLI   RECTYPE,RECHTRLQ    Test trailer record                          
         JNE   UPLD030                                                          
         GOTOR UTRL                                                             
         J     EXIT                                                             
*                                                                               
UPLD030  DS    0H                                                               
         DC    H'0'                Invalid record                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Process Salary upload header record                                 *         
***********************************************************************         
*                                                                               
UHDR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR GOTSAR,DMCB,('TSAINI',NEWBUF),0                                  
*                                                                               
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    UHDR0100                                                         
*                                                                               
         XC    HD_1RACC,HD_1RACC                                                
         ICM   RE,15,QH_AHEAD      Test header values passed                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R0,HD_VALS          and copy                                     
         LHI   R1,HD_VALSL                                                      
         AHI   RE,LW_LN1Q                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
* NOW DO ANY OFFLINE HEADER INIT HERE                                           
* buffer 'old' records to identify/process deleted elements if required         
         J     UEXIT                                                            
*                                                                               
* VALIDATE HEADER DATA, LOCK ALL PHIRECS FOR PERSON/MOA (PHIKSEQ)               
UHDR0100 DS    0H                  validate mode                                
         LA    R0,HD_VALS          Initialise header values                     
         LHI   R1,HD_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,TR_VALS          Initialise trailer values                    
         LHI   R1,TR_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   HD_TOKEN,QH_TOKEN                                                
*                                                                               
         LH    RF,RUN#LOCK                                                      
         LA    RF,1(RF)                                                         
         STH   RF,RUN#LOCK                                                      
         CLC   RUN#LOCK,RUN#LMAX                                                
         JNH   UHDR0105                                                         
         TM    RUNINDS,RUNILMAX                                                 
         JNZ   UFATAL              ONLY ONE OF THESE ERRORS PER FILE            
         MVC   ROUERRV,=AL2(AE$FTLPE)                                           
         XC    TEMP2,TEMP2               BUILD SUBSTITUTION PARM TABLE          
         MVI   TEMP2,C'&&'             FOR SAVERR TO PROC AS SUBSTIT            
         MVI   TEMP2+1,5               TABLE ENTRY LENGTH                       
         EDIT  RUN#LMAX,(4,TEMP2+2),0  TABLE ENTRY DATA                         
         MVI   TEMP2+6,0               terminator                               
         GOTOR SAVERR,DMCB,ROUERRV,(7,TEMP2),0                                  
         OI    RUNINDS,RUNILMAX     ª--L'EDITED RUN#LMAX+'&'+LEN+TERM           
         J     UFATAL                                                           
*                                                                               
UHDR0105 DS    0H                                                               
         MVC   HD_1RACC,QH_1RACC   1R EXISTS?                                   
         MVC   IOKEY,SPACES                                                     
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(2),=C'1R'                                                
         MVC   ACTKACT,QH_1RACC                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    UHDR0110                                                         
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RACC,HD_1RACC),0                      
         J     UFATAL                                                           
*                                                                               
UHDR0110 DS    0H                                                               
* SET LEDGER FOR LENGTHS, CHECK THIS IS LOW LEVEL                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#SECCHK,ASECCHK)   RSTSECY CHECK                                
         BE    UHDR0115                                                         
         MVC   ROUERRV,=AL2(AE$SCLOK)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RACC,HD_1RACC),0                      
         J     UFATAL                                                           
*                                                                               
UHDR0115 DS    0H                                                               
         GOTOR SPLIT1R                                                          
         JNE   UFATAL                                                           
         GOTOR VALOFF                                                           
         JNE   UFATAL                                                           
* VALIDATE MOA                                                                  
         GOTOR VALMOA                                                           
         JNE   UFATAL                                                           
UHDR0120 DS    0H                                                               
* Lock a high level record to avoid concurrent updates                          
*        GOTOR DOLOCK                 (not required currently)                  
*        JE    UHDR0130                                                         
*        GOTOR SAVERR,DMCB,ROUERRV,0                                            
*        J     UFATAL                                                           
*                                                                               
UHDR0130 DS    0H                                                               
* buffer 'old' records to identify/process deleted elements if required         
*                                                                               
* HEADER REPLY VALUES                                                           
UHDR0220 GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTLRQ',64),('LQ_TSINQ',HD_VALS),HD_VALSL                     
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
         J     UEXIT                                                            
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* Process Salary upload element                                       *         
***********************************************************************         
*                                                                               
UELE     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    TWAMODE,TWAMEDP     Exit on previous fatal error                 
         JNZ   UEXIT               (No further processing)                      
*                                                                               
         LA    R0,EL_VALS          Clear row values                             
         LHI   R1,HL_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   EL_TOKEN,QX_TOKEN                                                
*                                                                               
* Update run should be passed TSAR records built in validate call,              
* so we can just copy them to NEWBUF here                                       
         GOTOR DOTSAR,QX_TSAR                                                   
         JE    UEXIT              Tsar found (and added to NEWBUF)              
*                                                                               
* no tsar, so must be RVALREQQ run                                              
* Validate detail, build Tsar rec, attach to request. add to NEWBUF if          
* req by UTRL validation/duplicate checking                                     
*                                                                               
* VALIDATE PAYCODE                                                              
         GOTOR VALPAYC                                                          
         JNE   UEXIT                                                            
* DATE VALIDATION AGAINST LOCELS                                                
         GOTOR VDATCON,DMCB,(2,QX_SDATE),(1,EL_DATE)                            
         GOTOR VALDAT                                                           
         JNE   UEXIT                                                            
*                                                                               
         ZAP   EL_AMNT,QX_AMNT                                                  
         CP    EL_AMNT,=P'999999999'                                            
         JNH   UELE030                                                          
         MVC   ROUERRV,=AL2(AE$AMTHI)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UEXIT                                                            
*                                                                               
UELE030  DS    0H                                                               
         GOTOR NEWSAL                                                           
         JE    UEXIT                                                            
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Process trailer record                                              *         
***********************************************************************         
                                                                                
UTRL     TM    TWAMODE,TWAMEDP     Exit on previous fatal error                 
         JNZ   UEXIT               (no further processing)                      
*                                                                               
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UTRL0100                                                         
*                                                                               
         LA    R0,TR_VALS          Clear trailer values                         
         LHI   R1,TR_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TR_TOKEN,QT_TOKEN                                                
*                                                                               
*  whole-request type validation goes here                                      
*                                                                               
*  set status - any history info already on file?                               
         USING PHIRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVI   PHIKTYP,PHIKTYPQ                                                 
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CUXCPY                                                   
         MVC   PHIKOFC,HD_OFC                                                   
         MVC   PHIKDPT,HD_DPT                                                   
         MVC   PHIKSBD,HD_SDPT                                                  
         MVC   PHIKPER,HD_PERS                                                  
         MVC   PHIKMOA,HD_MOAC                                                  
         MVI   PHIKSEQ,0                                                        
         MVC   IOKEYSAV,IOKEY                                                   
         GOTOR VDATAMGR,DMCB,DMRDHI,ACCDIR,IOKEY,IOKEY                          
         JE    *+6                                                              
         DC    H'0'                READ ERROR                                   
         LA    R2,IOKEY                                                         
         MVI   TR_STAT,C'2'        ASSUME AMEND                                 
         CLC   PHIKEY(PHIKSEQ-PHIKEY),IOKEYSAV                                  
         JE    *+8                 NO PHIREC FOR LOCATION/MOA                   
         MVI   TR_STAT,C'1'        WE'LL BE ADDING ONE                          
*                                                                               
* set trailer response (validate)                                               
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTLRQ',64),('LQ_TSINQ',TR_VALS),TR_VALSL                     
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
         J     UEXIT                                                            
*                                                                               
* 'Run' mode - build FACWRK file                                                
UTRL0100 DS    0H                                                               
*                                                                               
* retrieve trailer data from validate run                                       
*                                                                               
         ICM   RE,15,QT_ATRLR      Test trailer values passed                   
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R0,TR_VALS                                                       
         LHI   R1,TR_VALSL                                                      
         AHI   RE,LW_LN1Q                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
*        GOTOR BLDLOK,HD_???       Build LOCKET key for DOLOCK rec              
*        GOTOR VDATAMGR,DMCB,$LOKREC,WORK  add to FACWRK (see hdr val)          
*                                                                               
* build new File rec(s), add to FACWRK through #IOEXEC call(s) for each         
* dir/fil update required.                                                      
         GOTOR UPDSAL                                                           
*                                                                               
* set trailer response (run)                                                    
         J     UEXIT                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* Exit handling for all upload records                                *         
***********************************************************************         
                                                                                
UFATAL   OI    TWAMODE,TWAMEDP     no more processing until trailer             
                                                                                
UEXIT    TM    TWAMODE,TWAMERP+TWAMEDP                                          
         JNZ   UEXIT04             error on this req                            
         TM    TWAMODE,TWAMPER     Error on previous 'set'?                     
         JZ    UEXIT02             No                                           
         GOTOR UNLOCK              Yes - unlock record locked this time         
*                                       (won't be going offline)                
UEXIT02  TM    LP_FLAG,LP_FOFFL                                                 
         JNZ   UEXIT24             nothing more to do offline                   
         CLI   RECTYPE,RECHTRLQ    Test just processed trailer record           
         JNE   UEXIT24                                                          
         TM    LP_FLAG,LP_FDRFT    Only send reply in 'draft' mode              
         JZ    UEXIT24                                                          
*                                                                               
* build online trailer reply data                                               
         SR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Build download map element                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),                         +        
               ('LIOTRAW',D#TOKEN),('LD_CHARQ',TR_TOKEN),(L'TR_TOKEN,0)         
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTRAW',D#STA),('LD_CHARQ',TR_STAT),(L'TR_STAT,0)             
         J     UEXIT24                                                          
*                                                                               
* error on this 'set'                                                           
UEXIT04  GOTOR UNLOCK              Unlock everything I locked so far            
         CLI   RECTYPE,RECHTRLQ    Test just processed trailer record           
         JNE   UEXIT06                                                          
         NI    TWAMODE,FF-(TWAMERP+TWAMEDP)  clear error for next 'set'         
         OI    TWAMODE,TWAMPER     set previous error                           
*                                                                               
UEXIT06  TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    *+6                                                              
         DC    H'0'                Die to unwind any updates                    
*                                                                               
* validate run, return all errors found for this 'set'                          
UEXIT08  LAY   R2,ERRTAB           Send errors                                  
         USING ET_D,R2             R2=A(Error table)                            
UEXIT10  SR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Build download map element                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
* return appropriate token                                                      
         CLI   RECTYPE,RECHTRLQ                                                 
         JE    UEXIT14                                                          
         CLI   RECTYPE,RECHHDRQ                                                 
         JE    UEXIT12                                                          
*   ELEMENT TOKEN                                                               
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),                         +        
               ('LIOTRAW',D#TOKEN),('LD_CHARQ',EL_TOKEN),(L'QX_TOKEN,0)         
         J     UEXIT16                                                          
*   HEADER TOKEN                                                                
UEXIT12  GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),                         +        
               ('LIOTRAW',D#TOKEN),('LD_CHARQ',HD_TOKEN),(L'QH_TOKEN,0)         
         J     UEXIT16                                                          
*   TRAILER TOKEN                                                               
UEXIT14  GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),                         +        
               ('LIOTRAW',D#TOKEN),('LD_CHARQ',TR_TOKEN),(L'QT_TOKEN,0)         
*                                                                               
UEXIT16  LA    R1,DMCB                                                          
         USING GETTXTD,R1          Build error text string in ELEMENT           
         CLI   ET_D,ET_EOTQ        Test end of error table                      
         JE    UEXIT22                                                          
         XC    GTBLOCK,GTBLOCK     Resolve error text                           
         MVI   GTMAXL,80                                                        
         MVC   GTMSGNO,ET_ERRNO                                                 
         LA    RE,ELEMENT                                                       
         STCM  RE,7,GTAOUT                                                      
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LLC   RE,ET_LN                                                         
         SHI   RE,ET_LN1Q                                                       
         LTR   RE,RE                                                            
         JZ    UEXIT18                                                          
         STC   RE,GTLTXT           Set length of extra text                     
         LA    RE,ET_EXTRA                                                      
         STCM  RE,7,GTATXT         Set A(Extra text)                            
         CLI   ET_EXTRA,C'&&'         SUBSTITUTION TEXT?                        
         JNE   UEXIT18                                                          
         LA    RE,1(RE)                                                         
         STCM  RE,7,GTASUBST         Set A(SUBSTITUTION TABLE)                  
         XC    GTLTXT,GTLTXT                                                    
         XC    GTATXT,GTATXT                                                    
UEXIT18  GOTOR VGETTXT,(R1)                                                     
         LLC   R0,4(R1)            Length of text just added                    
         GOTOR AALINKIO,(R1),('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTRAW',D#ERR),('LD_CHARQ',ELEMENT),((R0),0)                  
         OC    ET_ROWNM,ET_ROWNM                                                
         JZ    UEXIT20                                                          
         GOTOR AALINKIO,(R1),('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTRAW',D#ERROW),('LD_LBINQ',ET_ROWNM),(L'ET_ROWNM,0)         
UEXIT20  LLC   R0,ET_LN            Bump to next error table entry               
         AR    R2,R0                                                            
         J     UEXIT10                                                          
         DROP  R1,R2                                                            
* end of error table                                                            
UEXIT22  CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    EXITN                                                            
                                                                                
UEXIT24  J     EXITY               Exit back to DDLINK                          
         EJECT                                                                  
***********************************************************************         
* Handle 'Last for request' mode (only passed when running live)      *         
***********************************************************************         
                                                                                
EREQ     DS    0H                                                               
*                                                                               
         GOTOR BUFREC,0            FLUSH PHIREC BUFFER TO FACWRK                
*                                                                               
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JNZ   EREQ02              Yes - using SORTER not FACWRK                
         L     R1,WRKBLKR          Copy WRKR parameters                         
         MVC   PARM(WRKINDX-WRKIPARM),WRKIPARM-WRKIOD(R1)                       
         GOTOR DATAMGR,PARM,CLOSE  Close the FACWRK file                        
*                                                                               
EREQ02   OI    LP_FLAG,LP_FFWRK    Set FACWRK file built                        
         MVC   VDATAMGR,DATAMGR    Point to real DATAMGR                        
         MVC   ACOMFACS,LP_ACOM    Point to real COMFACS                        
*                                                                               
***********************************************************************         
* If we are running off-line with updative (global) files re-open the *         
* FACWRK recovery file and read the header record,  call  DATAMGR to  *         
* checkpoint recovery (COMMIT), call SRUPD60 to update the files and  *         
* then checkpoint recovery again to clear the global lock table.      *         
***********************************************************************         
*                                                                               
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY                                                            
         TM    LP_OFLG1,LP_OFDFT   Test 'draft' upload                          
         JNZ   EREQ04                                                           
         TM    LP_INDS,LP_IGLOB    Test global (updative) file                  
         JZ    EXITY                                                            
*                                                                               
EREQ04   GOTOR DATAMGR,DMCB,DMCOMMIT,0                                          
*                                                                               
         LA    RE,TSARPASS         Pass A(TSARPASS) in LP_ABLK3                 
         LA    RF,TSARRECS         Pass A(TSARRECS) in LP_ABLK4                 
         LA    R0,TSARNEWH         Pass A(TSARNEWH) in LP_ABLK5                 
         LA    R1,TSARNEWH         Pass A(TSARNEWH) in LP_ABLK6                 
         LA    R2,TSAROBUF         Pass A(TSAROBUF) in LP_ABLK7                 
         LA    R3,VSORTER          Pass A(SORTER) in LP_ABLK8                   
         STM   RE,R3,LP_ABLK3                                                   
         GOTOR ASRUPD60,DMCB,('FF',LP_D),('00',PARM),AIO1                       
*                                                                               
         GOTOR DATAMGR,DMCB,DMCOMMIT,0                                          
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET A/C LEVEL LENGTHS, SPLIT 1R ACC INTO COMPONENTS                           
*   CC NEQ IF ACCOUNT ISN'T PERSON LEVEL (saverr ALREADY CALLED)                
***********************************************************************         
*                                                                               
SPLIT1R  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,=C'1R'                                                    
         GOTOR (#SETLDG,ASETLDG)                                                
         CLI   CUAUTH+1,0          Allow if zero                                
         JE    SPLIT20                                                          
         CLC   LDGASC,CUAUTH+1                                                  
         JNH   SPLIT20                                                          
         MVC   ROUERRV,=AL2(AE$SCLOK)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RACC,HD_1RACC),0                      
         J     EXITN                                                            
*                                                                               
SPLIT20  DS    0H                                                               
         MVC   HD_OFC,SPACES                                                    
         MVC   HD_DPT,SPACES                                                    
         MVC   HD_SDPT,SPACES                                                   
         MVC   HD_PERS,SPACES                                                   
*                                                                               
         LLC   RF,LDGAL1                                                        
         SHI   RF,1                                                             
         MVC   HD_OFC(0),QH_1RACC                                               
         EX    RF,*-6                                                           
*                                                                               
         LLC   R0,LDGAL1                                                        
         LA    RE,QH_1RACC                                                      
         AR    RE,R0               A(DEPT)                                      
         LLC   RF,LDGAL2                                                        
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   HD_DPT(0),0(RE)                                                  
         EX    RF,*-6                                                           
*                                                                               
         LLC   R0,LDGAL2                                                        
         LA    RE,QH_1RACC                                                      
         AR    RE,R0               A(SUB DEPT                                   
         LLC   RF,LDGAL3                                                        
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   HD_SDPT(0),0(RE)                                                 
         EX    RF,*-6                                                           
*                                                                               
         LLC   R0,LDGAL3                                                        
         LA    RE,QH_1RACC                                                      
         AR    RE,R0               A(PERSON                                     
         LLC   RF,LDGAL4                                                        
         SR    RF,R0                                                            
         SHI   RF,1                                                             
         MVC   HD_PERS(0),0(RE)                                                 
         EX    RF,*-6                                                           
*                                                                               
         CLI   LDGAL4,0                                                         
         JNE   *+6                                                              
         DC    H'0'                WHAT KIND OF STRUCTURE IS THIS?              
         CLC   HD_PERS,SPACES                                                   
         JH    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$INACC)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_1RACC,HD_1RACC),0                      
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE OFFICE/ACCOUNT - OFFAL                                               
***********************************************************************         
*                                                                               
VALOFF   NTR1  LABEL=*,BASE=*                                                   
         L     R1,AOFFAREA                                                      
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,HD_OFC     Move in office and validate                  
         MVI   OFFAACT,OFFAVAL                                                  
         GOTOR VOFFAL                                                           
         JE    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'HD_OFC,HD_OFC)                            
         J     EXITN                                                            
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERSON CODE  - LOCATION MUST EXIST FOR DATE                          
*   if header, location must exist for >0 days in MOA                           
***********************************************************************         
*                                                                               
VALDAT   NTR1  LABEL=*,BASE=*                                                   
         LA    R2,IOKEY                                                         
         USING PERRECD,R2                                                       
* READ PERREC                                                                   
         MVC   PERKEY,SPACES                                                    
         MVC   PERKCPY,CUXCPY                                                   
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCODE,HD_PERS                                                 
         L     R2,AIO2                                                          
         CLC   PERKEY,IOKEY                                                     
         JE    VDAT025             ALREADY HAVE IT                              
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    VDAT020                                                          
         MVC   ROUERRV,=AL2(AE$IVPER)    1R EXISTS, BUT PERSON DOESN'T?         
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     EXITN                                                            
*                                                                               
VDAT020  DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
*                                                                               
* IF HEADER FLESH OUT PERIOD                                                    
VDAT025  DS    0H                                                               
         LA    R3,PERRFST                                                       
         CLI   RECTYPE,RECHHDRQ    Test header record                           
         JNE   VDAT027                                                          
         MVC   FULL1(L'HD_MOA),HD_MOA                                           
         MVC   FULL1+4(2),=C'01'                                                
         GOTOR VDATCON,DMCB,(0,FULL1),(1,DUB1)                                  
         MVC   FULL1(L'HD_MOA),HD_MOA                                           
         MVC   FULL1+4(2),=C'31'                                                
         GOTOR VDATCON,DMCB,(0,FULL1),(1,DUB2)                                  
         J     VDAT030                                                          
*                                                                               
* IF DETAIL, MATCH DATE AGAINST MOA                                             
VDAT027  DS    0H                                                               
         CLC   HD_MOAP,EL_DATE                                                  
         JNE   VDATNOMO                                                         
*                                                                               
* FIND LOCEL FOR 1R SENT                                                        
         USING LOCELD,R3                                                        
VDAT030  DS    0H                                                               
         CLI   LOCEL,0                                                          
         JE    VDATNO                                                           
         CLI   LOCEL,LOCELQ                                                     
         JNE   VDAT090                                                          
*                                                                               
         LLC   RF,LDGAL1           O/D/S LENGTHS INCONSISTENT IN DSECTS         
         SHI   RF,1                                                             
         BASR  R4,0                                                             
         CLC   LOCOFF(0),HD_OFC                                                 
         EX    RF,0(R4)                                                         
         JNE   VDAT090                                                          
*                                                                               
         LLC   RF,LDGAL2           O/D/S LENGTHS INCONSISTENT IN DSECTS         
         LLC   RE,LDGAL1                                                        
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R4,0                                                             
         CLC   LOCDEPT(0),HD_DPT                                                
         EX    RF,0(R4)                                                         
         JNE   VDAT090                                                          
*                                                                               
         LLC   RF,LDGAL3           O/D/S LENGTHS INCONSISTENT IN DSECTS         
         LLC   RE,LDGAL2                                                        
         SR    RF,RE                                                            
         SHI   RF,1                                                             
         BASR  R4,0                                                             
         CLC   LOCSUB(0),HD_SDPT                                                
         EX    RF,0(R4)                                                         
         JNE   VDAT090                                                          
*                                                                               
VDAT040  DS    0H                                                               
         CLI   RECTYPE,RECHHDRQ    Test header record                           
         JE    VDAT060                                                          
* ELEMENT, CHECK DATE SENT WITHIN LOCATION                                      
         CLC   EL_DATE,LOCSTART                                                 
         JL    VDAT090                                                          
         OC    LOCSALKD,LOCSALKD   USE SAL DATE FOR END, IF EXISTS              
         JZ    VDAT045                                                          
         CLC   EL_DATE,LOCSALKD                                                 
         JNH   VDATYES                                                          
         MVC   BYTE1,LOCSTAT                                                    
         MVC   FULL1,LOCSALKD                                                   
         J     VDAT090                                                          
*                                                                               
VDAT045  DS    0H                  ELSE END DATE                                
         OC    LOCEND,LOCEND                                                    
         JZ    VDATYES             ACTIVE INDEFINTELY                           
         CLC   EL_DATE,LOCEND                                                   
         JNH   VDATYES                                                          
         MVC   BYTE1,LOCSTAT                                                    
         MVC   FULL1,LOCEND                                                     
         J     VDAT090                                                          
*                                                                               
VDAT060  DS    0H                                                               
* HEADER REC - CHECK MOA SENT OVERLAPS AT LEAST ONE LOCEL                       
                                                                                
         CLC   LOCSTART,DUB2                                                    
         JH    VDAT090                                                          
         OC    LOCSALKD,LOCSALKD                                                
         JZ    VDAT065                                                          
         CLC   LOCSALKD,DUB1                                                    
         JNL   VDATYES             FOUND A LOCEL COVERING PART OF MOA           
         MVC   BYTE1,LOCSTAT                                                    
         MVC   FULL1,LOCSALKD                                                   
         J     VDAT090                                                          
*                                                                               
VDAT065  DS    0H                                                               
         OC    LOCEND,LOCEND                                                    
         JZ    VDATYES             NO END DATE - OK                             
         CLC   LOCEND,DUB1                                                      
         JNL   VDATYES             FOUND A LOCEL COVERING PART OF MOA           
         MVC   BYTE1,LOCSTAT                                                    
         MVC   FULL1,LOCEND                                                     
         J     VDAT090                                                          
*                                                                               
* NEXT LOCEL                                                                    
VDAT090  DS    0H                                                               
         LLC   RF,LOCLN                                                         
         AR    R3,RF                                                            
         J     VDAT030                                                          
*                                                                               
VDATYES  DS    0H                                                               
         J     EXITY                                                            
*                                                                               
VDATNOMO DS    0H                                                               
         MVC   ROUERRV,=AL2(AE$PDNIM)    PAYDATE OUTSIDE MOA                    
         XC    TEMP2,TEMP2               BUILD SUBSTITUTION PARM table          
         MVI   TEMP2,C'&&'               FLAG CHAR                              
         MVI   TEMP2+1,L'HD_1RACC+1      TABLE ENTRY LENGTH                     
         MVC   TEMP2+2(L'HD_1RACC),HD_1RACC   TABLE ENTRY DATA   - 1R           
         MVI   TEMP2+2+L'HD_1RACC,11                                            
         GOTOR VDATCON,DMCB,(1,EL_DATE),(21,TEMP2+L'HD_1RACC+3)  -PDATE         
         MVI   TEMP2+13+L'HD_1RACC,5                                            
         MVC   TEMP2+14+L'HD_1RACC(L'HD_MOA),HD_MOA               -MOA          
         LA    RE,L'HD_1RACC+L'HD_MOA+14                                        
         J     VDATNO30                                                         
*                                                                               
VDATNO   DS    0H                                                               
         CLI   RECTYPE,RECHHDRQ          header or detail?                      
         JNE   VDATNO10                                                         
         MVC   ROUERRV,=AL2(AE$MOANV)    PERSON NOT ACTIVE                      
         XC    TEMP2,TEMP2               BUILD SUBSTITUTION PARM table          
         MVI   TEMP2,C'&&'               FLAG CHAR                              
         MVI   TEMP2+1,L'HD_1RACC+1      TABLE ENTRY LENGTH                     
         MVC   TEMP2+2(L'HD_1RACC),HD_1RACC   TABLE ENTRY DATA -1R              
         MVI   TEMP2+2+L'HD_1RACC,5                                             
         MVC   TEMP2+3+L'HD_1RACC(L'HD_MOA),HD_MOA             -MOA             
         MVI   TEMP2+3+L'HD_1RACC+L'HD_MOA,0    terminator                      
         LA    RE,L'HD_1RACC+L'HD_MOA+4                                         
         J     VDATNO30                                                         
*                                                                               
VDATNO10 DS    0H               TRAILER-BYTE HAS LAST LOCSTAT                   
         MVC   ROUERRV,=AL2(AE$AFSAL)    PAYDATE NOT VALID                      
         XC    TEMP2,TEMP2               BUILD SUBSTITUTION PARM table          
         MVI   TEMP2,C'&&'                                                      
         MVI   TEMP2+1,L'HD_1RACC+1      TABLE ENTRY LENGTH                     
         MVC   TEMP2+2(L'HD_1RACC),HD_1RACC   TABLE ENTRY DATA  -1R             
         MVI   TEMP2+2+L'HD_1RACC,10                                            
         MVC   TEMP2+3+L'HD_1RACC(9),AC@LOA                     -STATUS         
         CLI   BYTE1,LOCSLOA                                                    
         JE    VDATNO20                                                         
         MVC   TEMP2+3+L'HD_1RACC(9),AC@TRM                                     
         CLI   BYTE1,LOCSTRM                                                    
         JE    VDATNO20                                                         
         MVC   TEMP2+3+L'HD_1RACC(9),AC@OTHER                                   
         CLI   BYTE1,LOCSOTH                                                    
         JE    VDATNO20                                                         
         MVC   TEMP2+3+L'HD_1RACC(9),AC@ACTV                                    
         CLI   BYTE1,LOCSACT                                                    
         JE    VDATNO20                                                         
         MVC   TEMP2+3+L'HD_1RACC(9),AC@XFR                                     
         CLI   BYTE1,LOCSTRAN                                                   
         JE    VDATNO20                                                         
         MVC   TEMP2+3+L'HD_1RACC(9),SPACES                                     
         MVC   TEMP2+3+L'HD_1RACC(3),=C'???'                                    
VDATNO20 DS    0H                                                               
         MVI   TEMP2+12+L'HD_1RACC,11                          -PAYDATE         
         GOTOR VDATCON,DMCB,(1,EL_DATE),(21,TEMP2+13+L'HD_1RACC)                
         MVI   TEMP2+23+L'HD_1RACC,0      TERMINATOR                            
         LA    RE,24+L'HD_1RACC                                                 
*                                                                               
VDATNO30 DS    0H                                                               
*        LLC   RE,TEMP2+1                                                       
*        AHI   RE,1                                                             
         GOTOR SAVERR,DMCB,ROUERRV,((RE),TEMP2),0                               
         J     EXITN                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE MOA, CC = NEQ IF ERROR (SAVERR ALREADY CALLED)                       
***********************************************************************         
*                                                                               
VALMOA   NTR1  LABEL=*,BASE=*                                                   
         MVC   HD_MOA,QH_MOA       SAVE UNCOMPLEMENTED                          
         MVC   DUB1(L'QH_MOA),QH_MOA                                            
         MVC   DUB1+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,(0,DUB1),(1,HD_MOAP)    PACKED                      
         GOTO1 VDATCON,DMCB,(0,DUB1),(10,WORK)    DD/MM/YY FOR BMONVAL          
         SR    RF,RF                                                            
         SH    RF,HD_MOAP                                                       
         STH   RF,HD_MOAC                          COMPLEMENTED MOA             
*&&US*&& GOTO1 BMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,TEMP2),(CUXCPY,0)         
*&&UK                                                                           
         GOTO1 BMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,TEMP2),          X        
               (CUXCPY,CUACCS)                                                  
*&&                                                                             
         LA    R1,TEMP2                                                         
         USING BMONVALD,R1                                                      
         CLI   BMOERR,BMOEOKQ                                                   
         JE    VALMOA10            MOA AVAILABLE                                
*                                                                               
         MVC   ROUERRV,=AL2(AE$MOSLK)                                           
         TM    BMOERR,BMOELOKQ                                                  
         JNZ   VALMOAN                                                          
         MVC   ROUERRV,=AL2(AE$DOPSP)                                           
         TM    BMOERR,BMOERNGQ                                                  
         JNZ   VALMOAN                                                          
         MVC   ROUERRV,=AL2(AE$INVIF)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'AC@MOA,AC@MOA)                            
         J     EXITN                                                            
*                                                                               
VALMOA10 DS    0H                  CALENDAR FOR MOA?                            
         GOTOR CALCHK                                                           
         JNE   EXITN                                                            
         GOTOR VALDAT              1R ACTIVE IN MOA?                            
         JNE   EXITN                                                            
         J     EXITY                                                            
*                                                                               
VALMOAN  DS    0H                                                               
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     EXITN                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR CALENDAR REC FOR HD_MOAP                                            
***********************************************************************         
*                                                                               
CALCHK   NTR1  LABEL=*,BASE=*                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CASRECD,R2                                                       
         MVI   CASKTYP,CASKTYPQ                                                 
         MVC   CASKCPY,CUXCPY                                                   
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKEMOA,HD_MOAP                                                 
         L     RF,AIO4             HAVE A CASREC ALREADY?                       
         CLC   CASKEY(CASKEMOA-CASKEY),0(RF)                                    
         JNE   CALCHK30                                                         
*                                                                               
         LR    R2,RF                                                            
         CLC   CASKEMOA,HD_MOAP                                                 
         JL    CALCHK30                                                         
         CLC   CASKSMOA,HD_MOAP                                                 
         JNH   EXITY               MATCHES                                      
*                                                                               
CALCHK30 DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'0'                MUST FIND SOMETHING...                       
*                                                                               
         LA    R2,IOKEY                                                         
         CLC   CASKEY(CASKEMOA-CASKEY),IOKEYSAV                                 
         JNE   CALCHKN             NO MORE CALRECS                              
*                                                                               
         CLC   CASKSMOA,HD_MOAP                                                 
         JNH   EXITY               COVERS MOA                                   
*                                                                               
CALCHKN  DS    0H                                                               
*&&UK*&& MVC   ROUERRV,=AL2(AE$CALM)   MISSING CALENDAR                         
*&&US*&& MVC   ROUERRV,=AL2(AE$NOCAL)  MISSING CALENDAR                         
         GOTOR SAVERR,DMCB,ROUERRV,0                                            
         J     EXITN                                                            
         J     EXITY                                                            
***********************************************************************         
* VALIDATE PAYCODE, CC = NEQ IF ERROR (SAVERR ALREADY CALLED)                   
***********************************************************************         
*                                                                               
VALPAYC  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     R4,AELEAREA                                                      
         USING PAYCDTAB,R4                                                      
VALPAY40 CLC   PAYCDNME,QX_SPYCD                                                
         JE    VALPAY50                                                         
         LA    R4,PAYCDLEN(R4)                                                  
         CLI   PAYCDNUM,0                                                       
         JNE   VALPAY40                                                         
VALPAY45 DS    0H                                                               
         MVC   ROUERRV,=AL2(AE$INVCD)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'QX_SPYCD,QX_SPYCD),0                      
         J     EXITN                                                            
*                                                                               
VALPAY50 DS    0H                                                               
* DEFENSIVE CODE HERE FOR PERCENTAGES ETC - HOPING NOT TO SUPPORT               
         CLC   PAYCDREV,SPACES       REVERSE TYPES                              
         JH    VALPAY45                                                         
         CLC   PAYCDPCS,SPACES       PERCENTAGE PAYCODES                        
         JH    VALPAY45                                                         
*                                                                               
*COPY ENTRY                                                                     
         MVC   EL_PAYCD,PAYCDNME                                                
         MVC   EL_PAYNM,PAYCDNUM                                                
         TM    PAYCDST,PAYADJRT                                                 
         BNO   *+8                                                              
         OI    EL_STAT2,PDESADJ                                                 
         TM    PAYCDST,PAYSHRTE                                                 
         BNO   *+8                                                              
         OI    EL_STAT2,PDESHRTE                                                
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE DETAIL REQ, BUILD TSAR REQ                                           
***********************************************************************         
*                                                                               
NEWSAL   NTR1  LABEL=*,BASE=*                                                   
         USING HS_D,R7                                                          
         LA    R7,HS_NEW                                                        
* BUILD TSAR REC                                                                
         XC    HS_D(HS_LNQ),HS_D                                                
*                                                                               
         MVC   HS_DATE,EL_DATE                                                  
         MVC   HS_NUM,EL_PAYNM                                                  
         MVC   HS_STAT2,EL_STAT2                                                
         MVC   HS_AMT,EL_AMNT                                                   
         ZAP   HS_ADJ,=P'0'                                                     
         MVC   HS_OLD,HS_NEW       SAVE AWAY IN CASE DUPE                       
*                                                                               
         GOTOR GOTSAR,DMCB,('TSAADD',NEWBUF),0                                  
         JE    NEWSALX                                                          
         TM    DMCB,TSEEOF         Test end of buffer                           
         JNZ   NSAL050                                                          
         TM    DMCB,TSEDUP         Test end of buffer                           
         JNZ   NSAL055                                                          
         DC    H'0'                Duplicate key                                
NSAL050  DS    0H                                                               
         MVC   ROUERRV,=AL2(AE$MAX#)                                            
         J     EXITN                                                            
*  NON-STANDARD CODE HERE! SUPPORT DUPLICATE ENTRIES (LAST ONE WINS)            
NSAL055  DS    0H                                                               
         MVC   HS_NEW,HS_OLD       REPLACE OLD VERSION WITH NEW                 
         GOTOR GOTSAR,DMCB,('TSAPUT',NEWBUF),0                                  
         JE    NEWSALX                                                          
         DC    H'0'                WHAT'S WRONG NOW?                            
*                                                                               
NEWSALX  DS    0H                                                               
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   EXITN                    (SHOULD ALWAYS BE TRUE...)              
*                                                                               
* attach derived-element-data to request for update (see DOTSAR)                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     X        
               ('LIOTLRQ',64),('LQ_TSINQ',HS_D),HS_LNQ                          
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
         J     EXITY                                                            
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE DETAIL REQ, BUILD TSAR REQ                                           
* NOTE: SINCE WE'RE USING FACWRK FOR THE UPDATIVE STUFF (VIA DMGRITRN)          
*       YOU SHOULD NEVER GET RECORD FOR UPDATE. JUST DO A PLAIN GET             
*       FOLLOWED BY A PUTREC.                                                   
* NOTE: HS_AMT=P'0' MEANS DELETE ANY MATCHING ELEMENT ONLY                      
***********************************************************************         
*                                                                               
UPDSAL   NTR1  LABEL=*,BASE=*                                                   
         USING HS_D,R7                                                          
         LA    R7,HS_NEW                                                        
         CLI   RUNMODE,RVALREQQ    Test 'Run request' mode                      
         JE    EXITN                    (SHOULD ALWAYS BE TRUE...)              
         XC    HS_D(HS_LNQ),HS_D                                                
* GET EXISTING PHIRECS INTO BUFFER                                              
         GOTOR BUFSAL                                                           
* READ ELEMENT BUFFER                                                           
         XC    HS_D(HS_LNQ),HS_D                                                
         GOTOR GOTSAR,DMCB,('TSARDH',NEWBUF),0                                  
         J     USAL015                                                          
*                                                                               
USAL010  DS    0H                                                               
         GOTOR GOTSAR,DMCB,('TSANXT',NEWBUF),0                                  
*                                                                               
USAL015  DS    0H                                                               
         JE    USAL020                                                          
         TM    DMCB,TSEEOF         Test end of buffer                           
         JNZ   UPDSALX                                                          
         TM    DMCB,TSERNF         REC NOT FOUND NOT SIGNIFICANT                
         JNZ   USAL020                                                          
         DC    H'0'                BAD TSAR ERROR                               
*                                                                               
*    BUILD NEW ELEM FOR THIS DATE                                               
USAL020  DS    0H                                                               
         XC    ELEMENT,ELEMENT                                                  
         USING PDEELD,R3                                                        
         LA    R3,ELEMENT                                                       
         MVI   PDEEL,PDEELQ                                                     
         MVI   PDELN,PDELNQ                                                     
         MVC   PDEDTE,HS_DATE                                                   
         MVC   PDENUM,HS_NUM                                                    
         MVC   PDEAMT,HS_AMT                                                    
         MVC   PDEADJ,HS_ADJ                                                    
         MVC   PDESTAT,HS_STAT                                                  
         MVC   PDESTAT2,HS_STAT2                                                
*                                                                               
* AND LOOK FOR REC TO ADD IT TO                                                 
         USING PHIRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVI   PHIKTYP,PHIKTYPQ                                                 
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CUXCPY                                                   
         MVC   PHIKOFC,HD_OFC                                                   
         MVC   PHIKDPT,HD_DPT                                                   
         MVC   PHIKSBD,HD_SDPT                                                  
         MVC   PHIKPER,HD_PERS                                                  
         MVC   PHIKMOA,HD_MOAC                                                  
         MVI   PHIKSEQ,0                                                        
*                                                                               
USAL025  DS    0H                                                               
         GOTOR BUFREC,DMCB,('BUFGDR',0),(0,AIO3)                                
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         LA    R3,PHIRFST                                                       
         TM    PHIRSTAT,TRNSDELT                                                
         JZ    USAL040                                                          
* if deleted, restore but purge all old elems                                   
USAL030  DS    0H                                                               
         NI    PHIRSTAT,FF-TRNSDELT                                             
         GOTOR VHELLO,DMCB,(C'D',ACCMST),('PDEELQ',AIO3),0                      
         J     USAL070                                                          
*                                                                               
*   LOOK FOR OLD VERSION ON PHIREC                                              
USAL040  DS    0H                                                               
         CLI   PDEEL,0                                                          
         JE    USAL052                                                          
         CLI   PDEEL,PDEELQ                                                     
         JNE   USAL045                                                          
         CLC   PDEDTE,HS_DATE                                                   
         JNE   USAL045                                                          
         CLC   PDENUM,HS_NUM                                                    
         JE    USAL050                                                          
USAL045  DS    0H                                                               
         LLC   RF,PDELN                                                         
         AR    R3,RF                                                            
         J     USAL040                                                          
*  MATCHING ELEM FOUND                                                          
USAL050  DS    0H                                                               
         MVI   PDEEL,X'FF'                                                      
         GOTOR VHELLO,DMCB,(C'D',ACCMST),(X'FF',AIO3),0                         
         J     USAL070             NEXT ELEM TO UPDATE                          
*                                                                               
*  NO MATCHING ELEM FOUND, ANY MORE IN BUFFER?                                  
USAL052  DS    0H                                                               
         LLC   RF,TR_#SALR                                                      
         SHI   RF,1                                                             
         CLM   RF,1,PHIKSEQ                                                     
         JE    USAL070                                                          
         MVC   IOKEY,PHIKEY     YES, GO READ                                    
         LA    R2,IOKEY                                                         
         LLC   RF,PHIKSEQ                                                       
         AHI   RF,1                                                             
         STC   RF,PHIKSEQ                                                       
         J     USAL025                                                          
*                                                                               
*   TRY ADDING THE ELEM TO THIS ONE                                             
USAL070  DS    0H                                                               
         CP    HS_AMT,=P'0'       ZERO AMOUNT MEANS DELETE LINE                 
         JE    USAL075                                                          
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO3,ELEMENT                           
         CLI   DMCB+12,0                                                        
         JNE   USAL080                                                          
USAL075  DS    0H                                                               
         GOTOR BUFREC,DMCB,('BUFPUT',0),(0,AIO3)                                
         JE    USAL010            GOOD PUT, UPDATE BUFFER ENTRY                 
         DC    H'0'                                                             
*                                                                               
* NO ROOM ON RECORD                                                             
USAL080  DS    0H                                                               
         CLI   DMCB+12,5                                                        
         JE    *+6                                                              
         DC    H'0'                BAD PUT                                      
         GOTOR SALADD,1           REC FULL, ADD A NEW ONE                       
         J     USAL010                                                          
*                                                                               
UPDSALX  DS    0H                                                               
         J     EXITY                                                            
         DROP  R7,R2                                                            
         EJECT                                                                  
***********************************************************************         
*              ADD NEW PAYROLL HISTORY RECORD                         *         
* ENTRY: R1 = 0 : NO PHIRECS ON FILE, ADD FIRST                                 
*        R1 = 1 : ADD NEXT SEQUENTIAL PHIREC (USES TR_#SALR) AND                
*                        COPY IN ELEMENT                                        
*        R7     : TSAR SOURCE                                                   
***********************************************************************         
*                                                                               
         USING HS_D,R7                                                          
SALADD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R3,R1                                                            
         USING PHIRECD,R2                                                       
         L     R2,AIO3                                                          
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ                                                 
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CUXCPY                                                   
         MVC   PHIKOFC,HD_OFC                                                   
         MVC   PHIKDPT,HD_DPT                                                   
         MVC   PHIKSBD,HD_SDPT                                                  
         MVC   PHIKPER,HD_PERS                                                  
         MVC   PHIKMOA,HD_MOAC                                                  
         MVC   PHIRLEN,=AL2(L'PHIKEY+L'PHIRSTA+L'PHIRLNK+3)                     
         XC    PHIRSTA,PHIRSTA                                                  
         XC    PHIRLNK,PHIRLNK                                                  
         MVI   PHIRLNK+L'PHIRLNK,0 TERMINATOR                                   
*                                                                               
         MVI   PHIKSEQ,0                                                        
         LTR   R3,R3                                                            
         JZ    SALADD10                                                         
         MVC   PHIKSEQ,TR_#SALR                                                 
         LLC   RF,TR_#SALR                                                      
         AHI   RF,1                                                             
         STC   RF,TR_#SALR                                                      
         GOTOR VHELLO,DMCB,(C'P',ACCMST),AIO3,ELEMENT                           
         CLI   DMCB+12,0                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SALADD10 DS    0H                                                               
         MVC   IOKEY,PHIKEY                                                     
         XC    IODA,IODA                                                        
         GOTOR BUFREC,DMCB,('BUFADD',0),(0,AIO3)                                
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2,R7                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* Save error messages in an area so we can send all errors together   *         
***********************************************************************         
*                                                                               
SAVERR   NTR1  LABEL=NO,BASE=*                                                  
         J     *+12                                                             
         DC    C'*SAVERR*'                                                      
                                                                                
         CLI   RUNMODE,RVALREQQ    Can't handle errors if live update           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RF,ANXTERR          Address of next error in table               
         LAY   RE,ERRTAB                                                        
         CLI   0(RE),ET_EOTQ      If table is empty                             
         JNE   SAVERR10                                                         
         LAY   RF,ERRTAB           Point to start of table                      
         XC    ALSTERR,ALSTERR                                                  
         USING ET_D,RF                                                          
SAVERR10 LLC   R0,4(R1)                                                         
         AHI   R0,ET_LN1Q          R0=Length of new entry                       
         AR    R0,RF                                                            
         LAY   RE,ERRTAB+L'ERRTAB                                               
         CR    R0,RE                                                            
         JH    SAVERR30            No room left                                 
         SR    R0,RF                                                            
         STC   R0,0(RF)            Set length of new entry                      
         OI    TWAMODE,TWAMERP     Set we have validation error                 
         LM    R2,R4,0(R1)                                                      
         MVC   ET_ERRNO,0(R2)                                                   
         MVC   ET_ROWNM,0(R4)                                                   
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
         JNH   *+6                 Room left to warn user                       
         DC    H'0'                table still too long                         
         SR    R0,RF                                                            
         STC   R0,0(RF)            Set length of new entry                      
         OI    TWAMODE,TWAMEDP     Give up now                                  
         MVC   ET_ERRNO,=AL2(AE$TMERR)                                          
         MVI   ET_LN1Q(RF),ET_EOTQ                                              
         J     EXITY                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* Clear TSAR record                                                   *         
***********************************************************************         
*                                                                               
         USING HS_D,R7                                                          
CLRSAL   STM   RE,R1,12(RD)                                                     
         LA    R0,HS_D                                                          
         LHI   R1,HS_LNQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  R7                                                               
*                                                                               
***********************************************************************         
* Clear work area (RC=A(Work area), R1=L'Work area)                   *         
***********************************************************************         
*                                                                               
CLRWRK   STM   RE,R1,12(RD)                                                     
         LR    R0,RC                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Locate an element in record pointed to by (R2)                      *         
*                                                                     *         
* Ntry:- R1=Element code                                              *         
* Exit:- R1=A(Element) and CC=Equal if element found                  *         
*        R1=0 and CC=Not equal if element not found                   *         
***********************************************************************         
*                                                                               
GETELA   LR    RF,R1               RF=Element code to search for                
*        L     R1,IOADDR                                                        
         LA    R1,ACCRFST-ACCRECD(R2)                                           
         SR    R0,R0                                                            
GETELA02 CLI   0(R1),0             Test end of record                           
         JNE   *+10                                                             
         SR    R1,R1               Yes - clear pointer address                  
         CR    RE,R1               Set CC to not equal                          
         BR    RE                                                               
         CLM   RF,1,0(R1)          Test correct element                         
         BER   RE                  Yes - exit with CC equal                     
         IC    R0,1(R1)            No - bump to next element on record          
         AR    R1,R0                                                            
         J     GETELA02                                                         
         EJECT                                                                  
***********************************************************************         
* copy derived-element-data tsar from req to buffer (offline)                   
* ENTRY: R7 POINTS TO TSAR REC IO AREA                                          
***********************************************************************         
*                                                                               
         USING HS_D,R7                                                          
DOTSAR   NTR1  LABEL=NO,BASE=*                                                  
         J     *+12                                                             
         DC    C'*DOTSAR*'                                                      
                                                                                
         LA    R7,HS_NEW                                                        
         ICM   RE,15,0(R1)         RE=Index to work pool element                
         JZ    EXITN               CC=Low if element not found                  
         SR    RF,RF                                                            
         ICM   RF,3,LW_LN-LW_D(RE)                                              
         SHI   RF,LW_LN1Q          RF=L'TSAR record                             
         AHI   RE,LW_LN1Q          RE=A(TSAR record)                            
         LA    R0,HS_D             To address                                   
         LR    R1,RF               To length                                    
         MVCL  R0,RE               Copy record to I/O area 1                    
         MVC   HS_OLD,HS_NEW                                                    
         GOTOR GOTSAR,DMCB,('TSAADD',NEWBUF),0                                  
         JE    EXITY                                                            
*        DC    H'0'                                                             
* NON-STANDARD CODE HERE! NORMALLY DO NOT ALLOW DUPES                           
* BUT GERMANY WANT TO FOR HISTORY UPLOAD (LAST VERSION WINS)                    
         TM    DMCB,TSEDUP                                                      
         JNZ   DOTSAR50                                                         
         DC    H'0'                                                             
*                                                                               
DOTSAR50 DS    0H                                                               
         MVC   HS_NEW,HS_OLD       UPDATE TSAR WITH LATEST VERSION              
*        GOTOR GOTSAR,DMCB,('TSARDH',NEWBUF),0                                  
*        CLC   HS_NEW(HS_KEYL),HS_OLD                                           
*        JE    *+6                                                              
*        DC    H'0'                                                             
         GOTOR GOTSAR,DMCB,('TSAPUT',NEWBUF),0                                  
         JE    EXITY                                                            
         DC    H'0'                WHAT NOW?                                    
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* Interface to TSAR for derived-element-data buffers                  *         
*                                                                     *         
* Note that when running off-line the TSAR buffers are acquired only  *         
* once - the XC(TSPNEWL) below is intentional as the first time       *         
* through the code TSAR will issue the GETMAIN for the buffer         *         
* ENTRY : R1 P0=TSAR ACTION                                                     
*         R1 P3=OLDBUF/NEWBUF                                                   
* ENTRY: R7 POINTS TO TSAR REC IO AREA                                          
* EXIT :  CCODE NEQ IF ERROR, ERROR BITS IN DMCB P1 B1 (TSERRS)                 
***********************************************************************         
                                                                                
         USING HS_D,R7                                                          
GOTSAR   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GOTSAR*'                                                      
                                                                                
         LR    R2,R1               R2=A(Caller's parameter list)                
         LA    R3,TSARNEWH         Point to correct TSAR block                  
                                                                                
         USING TSARD,R3            R3=A(TSAR block)                             
         LA    R0,HS_D                                                          
         ST    R0,TSAREC           Set A(Record)                                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   GOTSAR02                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVC   TSACTN,0(R2)                                                     
         MVC   TSACOM,ACOMFACS                                                  
         LHI   R0,ONEK                                                          
         STCM  R0,3,TSBUFFL        Set require 1MB off-line                     
         MVI   TSRECI,TSRTSAB1     NEWBUF                                       
         OI    TSRECI,TSRXTN                                                    
         MVI   TSKEYL,HS_KEYL      Set key length                               
         LHI   R0,HS_LNQ                                                        
         STCM  R0,3,TSRECL         Set record length                            
         MVI   TSINDS,TSINODSK     Set no disk writes (save/restore)            
         GOTOR VTSAR,TSARD                                                      
         TM    TSINDS,TSIINIOK                                                  
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
GOTSAR02 TM    TSINDS,TSIINIOK     Test initialised                             
         JZ    GOTSAR04                                                         
                                                                                
         MVC   TSACTN,0(R2)        Set action                                   
         OC    4(L'TSAREC,R2),4(R2)                                             
         JZ    *+10                                                             
         MVC   TSAREC,4(R2)        Set A(record) if passed                      
         GOTOR VTSAR,TSARD         Call TSAR                                    
         J     GOTSARX                                                          
                                                                                
GOTSAR04 MVI   TSERRS,TSEEOF       Set EOF if not initialised                   
*                                                                               
GOTSARX  MVC   DMCB(L'TSERRS),TSERRS    PASS BACK ERRORS                        
         CLI   TSERRS,0            Set condition code for caller                
         J     EXIT                                                             
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* Initialise optimisation buffer (uses WSSVR buffer)                  *         
***********************************************************************         
                                                                                
INIBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*INIBUF*'                                                      
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAINI     Set action to 'Initialise'                   
         MVI   T.TSRECI,TSRXTN+TSRWSSVR                                         
         MVI   T.TSKEYL,L'OB_KEY                                                
         LHI   R0,ONEK                                                          
         STCM  R0,3,T.TSBUFFL                                                   
         LHI   R0,OB_LNQ                                                        
         STCM  R0,3,T.TSRECL                                                    
         MVC   T.TSACOM,ACOMFACS                                                
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* Add a record to optimisation buffer                                 *         
*                                                                     *         
* Ntry:- R1 points to caller's OB_D                                   *         
***********************************************************************         
                                                                                
ADDBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDBUF*'                                                      
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSAADD     Set action to 'Add'                          
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* Get a record from optimisation buffer                               *         
*                                                                     *         
* Ntry:- R1 points to caller's OB_D                                   *         
* Exit:- CC=Low if record not found in buffer                         *         
*        CC=Equal if record found and is not posted with an error     *         
*           - record is returned in caller's OB_D                     *         
*        CC=High if record found and is posted with an error (set in  *         
*           ROUERRV)                                                  *         
***********************************************************************         
                                                                                
GETBUF   NTR1  LABEL=NO,WORK=(RC,OB_LNQ)                                        
         J     *+12                                                             
         DC    C'*GETBUF*'                                                      
                                                                                
W        USING OB_D,RC                                                          
         LR    R2,R1                                                            
P        USING OB_D,R2             R2=A(caller's OB_D)                          
         MVC   W.OB_KEY,P.OB_KEY                                                
                                                                                
T        USING TSARD,TSAROBUF                                                   
         MVI   T.TSACTN,TSARDH     Set action to 'Read High'                    
         LA    R0,W.OB_D                                                        
         ST    R0,T.TSAREC         Read into acquired storage                   
         GOTOR VTSAR,T.TSARD                                                    
         JNE   EXITL               Not found/EOF                                
         DROP  T                                                                
                                                                                
         MVC   P.OB_D(OB_LNQ),W.OB_D                                            
                                                                                
         MVC   ROUERRV,P.OB_ERROR  Set/test record in error                     
         OC    ROUERRV,ROUERRV                                                  
         JNZ   EXITH                                                            
         J     EXITY                                                            
         DROP  W,P                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF PAYCODES                                                       
***********************************************************************         
GETPAYTB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,AELEAREA         CLEAR BLOCK FOR TABLE                        
         LA    R1,PCTABLN                                                       
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AELEAREA                                                      
         LA    R1,PCTABLN                                                       
         AR    R1,R4                                                            
         ST    R1,APAYEND          SAVE END OF TABLE                            
*                                                                               
         USING PAYCDTAB,R4                                                      
         USING PAYRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   PAYKSUB,PAYKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   PAYKCPY,CUXCPY       COMPANY                                     
         MVI   PAYKSEQ,0                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    GPAYT012                                                         
         DC    H'0'                                                             
*                                                                               
GPAYT010 DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JE    GPAYT012                                                         
         DC    H'0'                                                             
*                                                                               
GPAYT012 DS    0H                                                               
         LA    R2,IOKEY                                                         
         CLC   PAYKEY(PAYKSEQ-PAYKEY),IOKEYSAV                                  
         BNE   GPAYTX              ALL DONE                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
*                                                                               
         L     R2,AIO1                                                          
         LA    R3,PAYRFST                                                       
         SR    RF,RF                                                            
         USING PAYELD,R3                                                        
GPAYT020 DS    0H                                                               
         CLI   PAYEL,0                                                          
         JE    GPAYT010            LOOK FOR MORE RECORDS                        
         CLI   PAYEL,PAYELQ                                                     
         JE    GPAYT025                                                         
GPAYT021 IC    RF,PAYLN                                                         
         AR    R3,RF                                                            
         J     GPAYT020                                                         
*    ADD ENTRY                                                                  
GPAYT025 DS   0H                                                                
         MVC   PAYCDNUM,PAYNUM                                                  
         MVC   PAYCDNME,PAYCODE                                                 
         MVC   PAYCDREV,PAYREV                                                  
         MVC   PAYCDPCS,PAYPCS                                                  
         MVC   PAYCDST,PAYSTAT                                                  
*                                                                               
         LA    R4,PAYCDLEN(R4)                                                  
         L     R1,APAYEND                                                       
         CR    R4,R1                                                            
         BL    GPAYT021                                                         
         DC    H'0'                TOO MANY PAYCODES-INCREASE TABLE             
*                                                                               
GPAYTX   XIT1                                                                   
         DROP  R3,R4                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* BUFFER EXISTING SALARY RECORDS                                                
*   SETS HD_STAT                                                                
***********************************************************************         
BUFSAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   TR_#SALR,1                                                       
*                                                                               
         MVC   IOKEY,SPACES                                                     
         USING PHIRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVI   PHIKTYP,PHIKTYPQ                                                 
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CUXCPY                                                   
         MVC   PHIKOFC,HD_OFC                                                   
         MVC   PHIKDPT,HD_DPT                                                   
         MVC   PHIKSBD,HD_SDPT                                                  
         MVC   PHIKPER,HD_PERS                                                  
         MVC   PHIKMOA,HD_MOAC                                                  
         MVI   PHIKSEQ,0                                                        
         MVC   IOKEYSAV,IOKEY                                                   
* CHECK IF ALREADY BUFFERED (PREVIOUS REQUEST BLOCK)                            
         GOTOR BUFREC,DMCB,('BUFRDH',0),(0,AIO3)                                
         JNE   BSAL040             NOT FOUND                                    
         L     R2,AIO3                                                          
         CLC   PHIKEY,IOKEYSAV                                                  
         JNE   BSAL040             SOME OTHER RECORD                            
*                                                                               
BSAL020  DS    0H                                                               
         LLC   RE,PHIKSEQ          BUFFERED, READ ALL TO SET                    
         LA    RE,1(RE)                          TR_#SALR                       
         STC   RE,TR_#SALR                                                      
         MVC   IOKEY(L'PHIKEY),PHIKEY                                           
         STC   RE,IOKEY+PHIKSEQ-PHIKEY                                          
         GOTOR BUFREC,DMCB,('BUFRDH',0),(0,AIO3)                                
         JNE   BUFSALX                                                          
         CLC   PHIKEY,IOKEY                                                     
         JNE   BUFSALX             SOME OTHER RECORD                            
         J     BSAL020                                                          
                                                                                
*   Not yet buffered, exists on file?                                           
BSAL040  DS    0H                                                               
         MVC   IOKEY,IOKEYSAV                                                   
         L     R1,=AL4(IOHID+IODIR+IO3)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    BSAL045                                                          
         TM    IOERR,IOEDEL                                                     
         JNZ   BSAL045                                                          
         DC    H'0'                READ ERROR                                   
BSAL045  DS    0H                                                               
         LA    R2,IOKEY                                                         
         CLC   PHIKEY(PHIKSEQ-PHIKEY),IOKEYSAV                                  
         JNE   BUFSALN             NO PHIREC FOR LOCATION/MOA                   
         J     BSAL050                                                          
* buffer record that we've found                                                
BSAL050  DS    0H                                                               
         GOTOR BUFREC,DMCB,('BUFGDR',0),(0,AIO3)                                
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,IOKEYSAV                                                   
         LA    R2,IOKEY                                                         
         MVC   PHIKSEQ,TR_#SALR    build next sequential key                    
*  ANY SEQUENTIALS?                                                             
BSAL060  DS    0H                                                               
         L     R1,=AL4(IOHID+IODIR+IO3)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    BSAL065                                                          
         TM    IOERR,IOEDEL                                                     
         JNZ   BSAL065                                                          
         DC    H'0'                READ ERROR                                   
BSAL065  DS    0H                                                               
         LA    R2,IOKEY                                                         
         CLC   PHIKEY(PHIKSEQ-PHIKEY),IOKEYSAV                                  
         JNE   BUFSALX                                                          
         LLC   RF,TR_#SALR                                                      
         AHI   RF,1                                                             
         STC   RF,TR_#SALR                                                      
         J     BSAL050             got one, buffer it                           
*                                                                               
*                                                                               
BUFSALN  DS    0H                  BUFFER NEW, FIRST PHIREC                     
         GOTOR SALADD,0                                                         
*                                                                               
BUFSALX  XIT1                                                                   
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* Manage a TSAR buffer of ACCMST records that are to be updated       *         
*                                                                     *         
* This routine will add an entry to the buffer for the first read     *         
* of the record, provided that the record is found and not deleted.   *         
* The caller may add new records to the buffer (with a disk address   *         
* of zero) and records which are to replace deleted records (which    *         
* are not added automatically).  When the routine is called for the   *         
* last time (R1=0) the buffer records are read back and the internal  *         
* DATAMGR routine is called to add the records to the output FACWRK   *         
* recovery file - any records with a zero disk address will cause     *         
* an ADDREC in SRUPD60 else a PUTREC will be done instead.            *         
*                                                                     *         
* Ntry:- R1 points to a parameter list as follows or zero if last     *         
*        time call (to flush buffer and write recovery records)       *         
*                                                                     *         
*        P1/0   - Action/read flags                                   *         
*        P2/1-3 - A(Record)                                           *         
*        P3     - A(Caller hook routine for buffer add)               *         
***********************************************************************         
                                                                                
BUFGET   EQU   1                   Get/create a buffer entry                    
BUFGDR   EQU   BUFGET+X'08'        Get/create - read for deletes                
BUFPUT   EQU   2                   Update a buffer entry                        
BUFADD   EQU   3                   Add a new buffer entry                       
BUFRDH   EQU   4                   Read high for a record key                   
BUFNXT   EQU   5                   Get next record (after read high)            
                                                                                
BUFREC   NTR1  LABEL=NO,BASE=*,WORK=(RC,BRWORKL)                                
         J     *+12                                                             
         DC    C'*BUFREC*'                                                      
                                                                                
         USING BRWORKD,RC          RC=A(local working storage)                  
TB       USING TSARD,TSARRECS      BUFREC TSAR block                            
         LTR   R2,R1               Test last time call                          
         JZ    BUFREC65                                                         
         MVC   BRACTN,0(R2)                                                     
         MVC   BRINDS,0(R2)                                                     
         NI    BRACTN,B'00000111'  Isolate action number                        
         NI    BRINDS,B'11111000'  Isolate read flags                           
*                                                                               
         TM    RUNINDS,RUNIBUFR    Test first time                              
         JNZ   BUFREC10                                                         
         MVI   TB.TSACTN,TSAINI    Yes - initialise TSAR buffer                 
         MVI   TB.TSRECI,TSRXTN+TSRMINB2+TSRVAR                                 
         MVI   TB.TSKEYL,L'BR_KEY                                               
         LHI   R0,BR_LNQ                                                        
         STCM  R0,3,TB.TSRECL      Set maximum record length                    
         LHI   R0,4*ONEK           Default to 4MB off-line                      
         OC    TB.TSBUFFL,TB.TSBUFFL                                            
         JNZ   *+8                                                              
         STCM  R0,3,TB.TSBUFFL     Set buffer length if not set                 
         MVC   TB.TSACOM,ACOMFACS                                               
         GOTOR VTSAR,TB.TSARD      Initialise TSAR buffer                       
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    RUNINDS,RUNIBUFR    Set not first time                           
*                                                                               
BUFREC10 L     R4,4(R2)            R4=A(Record)                                 
         SHI   R4,BR_HL            Back up by header length                     
         ST    R4,TB.TSAREC        Set A(TSAR record)                           
R        USING BR_D,R4                                                          
         MVC   BRSAVE(BR_HL),0(R4) Save what's there                            
         XC    R.BR_D(BR_HL),R.BR_D                                             
         MVI   BRTSARER,0          Clear TSAR return flags                      
*                                                                               
         CLI   BRACTN,BUFGET       Test get record from buffer                  
         JNE   BUFREC30                                                         
         MVC   R.BR_KEY,IOKEY                                                   
         MVI   TB.TSACTN,TSARDH                                                 
         GOTOR VTSAR,TB.TSARD      See if we have record already                
         JE    BUFREC55                                                         
         MVI   TB.TSACTN,TSAADD    Set action to add record                     
         MVC   R.BR_KEY,IOKEY      Set record key                               
*                                                                               
         MVC   IOKEYSAV,IOKEY      Save record key (emulate IOEXEC)             
         GOTOR VDATAMGR,PARM,(BRINDS,DMREAD),ACCDIR,IOKEY,IOKEY                 
         MVC   IODA,IOKEY+(ACCKDA-ACCRECD)                                      
         MVC   IOERR,8(R1)         Save directory return                        
         JE    BUFREC20            Exit on any errors                           
         TM    BRINDS,X'08'        allow deletes if we want them                
         JZ    EXITN                                                            
         CLI   IOERR,IOEDEL                                                     
         JNE   EXITN                                                            
*                                                                               
BUFREC20 DS    0H                                                               
         MVC   R.BR_DA,IODA        Set disk address                             
         GOTOR VDATAMGR,(R1),(BRINDS,DMGETR),ACCMST,R.BR_DA,R.BR_REC,  +        
               R.BR_WORK                                                        
         JE    BUFREC21                                                         
         TM    BRINDS,X'08'        allow deletes if we want them                
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   8(R1),IOEDEL                                                     
         JNE   BUFREC21                                                         
         DC    H'0'                                                             
*                                                                               
BUFREC21 DS    0H                                                               
         SR    R0,R0               Set buffer record length                     
         ICM   R0,3,R.BR_REC+(ACCRLEN-ACCRECD)                                  
         AHI   R0,BR_REC-BR_D                                                   
         STCM  R0,3,R.BR_LEN                                                    
*                                                                               
         GOTOR VTSAR,TB.TSARD      Add record to TSAR buffer                    
         JE    BUFREC55                                                         
         DC    H'0'                                                             
*                                                                               
BUFREC30 CLI   BRACTN,BUFPUT       Test put record                              
         JE    *+12                                                             
         CLI   BRACTN,BUFADD       Test add record                              
         JNE   BUFREC40                                                         
         SR    R0,R0               Set buffer record length                     
         ICM   R0,3,R.BR_REC+(ACCRLEN-ACCRECD)                                  
         AHI   R0,BR_REC-BR_D                                                   
         STCM  R0,3,R.BR_LEN                                                    
         MVC   R.BR_KEY,IOKEY      Set record key                               
         MVC   R.BR_ERRS,IOERR     Set error flags                              
         MVC   R.BR_DA,IODA        Set disk address                             
         MVC   R.BR_WORK,IOWORK    Set PUTREC work area                         
         MVI   TB.TSACTN,TSAADD    Set action to add                            
         CLI   BRACTN,BUFPUT                                                    
         JNE   *+8                                                              
         MVI   TB.TSACTN,TSAPUT    Set action to put                            
         GOTOR VTSAR,TB.TSARD                                                   
         JE    BUFREC60                                                         
         DC    H'0'                                                             
                                                                                
BUFREC40 CLI   BRACTN,BUFRDH       Test read high for a record key              
         JNE   BUFREC45                                                         
         MVC   IOKEYSAV,IOKEY      Save caller's key                            
         MVC   R.BR_KEY,IOKEY      Set read high key                            
         MVI   TB.TSACTN,TSARDH    Set action to 'read high'                    
         J     BUFREC50                                                         
                                                                                
BUFREC45 CLI   BRACTN,BUFNXT       Get next record for a record key             
         JE    *+6                                                              
         DC    H'0'                                                             
         MVI   TB.TSACTN,TSANXT    Set action to 'get next'                     
                                                                                
BUFREC50 GOTOR VTSAR,TB.TSARD      Read high/get next                           
         MVC   BRTSARER,TB.TSERRS                                               
         TM    BRTSARER,TSEEOF     Test end of file                             
         JNZ   BUFREC60                                                         
         MVI   BRTSARER,0          Clear error for read high                    
                                                                                
BUFREC55 MVC   IODA,R.BR_DA        Set disk address                             
         MVC   IOWORK,R.BR_WORK    Set PUTREC work area                         
K        USING ACCRECD,IOKEY       Build directory record                       
         MVC   K.ACCKEY,R.BR_REC                                                
         MVC   K.ACCKSTA,R.BR_REC+(ACCRSTA-ACCRECD)                             
         MVC   K.ACCKDA,IODA                                                    
                                                                                
BUFREC60 MVC   R.BR_D(BR_HL),BRSAVE                                             
         CLI   BRTSARER,0          Set condition code for caller                
         J     EXIT                                                             
         DROP  R                                                                
*                                                                               
* 'LAST TIME' CALL - FLUSH BUFFERED RECORDS TO FILE (USES DMGRITRN)             
BUFREC65 TM    RUNINDS,RUNIBUFR    Test first time flag set                     
         JZ    EXITY               No - buffer must be empty                    
                                                                                
         MVI   TB.TSACTN,TSARDH                                                 
         L     R2,AIO1             Use IO1 for these calls                      
         ST    R2,TB.TSAREC                                                     
         USING BR_D,R2                                                          
         XC    BR_KEY,BR_KEY                                                    
                                                                                
BUFREC70 GOTOR VTSAR,TB.TSARD      Get first/next buffer record                 
         MVI   TB.TSACTN,TSANXT                                                 
         TM    TB.TSERRS,TSEEOF    Test end of buffer                           
         JNZ   EXITY                                                            
                                                                                
         OC    BR_DA,BR_DA         Test new record                              
         JZ    BUFREC80                                                         
BUFREC75 GOTOR VDATAMGR,DMCB,DMPUTF,ACCMST,BR_DA,BR_REC,BR_WORK                 
*                                                                               
         GOTOR VDATAMGR,DMCB,(X'08',DMREAD),ACCDIR,BR_KEY,IOKEYSAV              
         TM    8(R1),FF-(IOEDEL)                                                
         JZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   K.ACCKEY,BR_REC     Build directory record                       
         MVC   K.ACCKSTA,BR_REC+(ACCRSTA-ACCRECD)                               
         MVC   K.ACCKDA,BR_DA                                                   
         CLC   K.ACCRECD(ACCKLEN),IOKEYSAV                                      
         JE    BUFREC70            Done if no change to it                      
         GOTOR VDATAMGR,DMCB,DMWRTD,ACCDIR,K.ACCKEY,K.ACCKEY                    
         J     BUFREC70                                                         
                                                                                
BUFREC80 GOTOR VDATAMGR,DMCB,DMADDF,ACCMST,BR_DA,BR_REC,BR_WORK                 
         J     BUFREC70                                                         
         DROP  RC,K,R2,TB                                                       
                                                                                
BRWORKD  DSECT                     ** BUFREC local working storage **           
BRACTN   DS    X                   Action                                       
BRINDS   DS    X                   Read flags                                   
BRTSARER DS    XL(L'TSERRS)        TSAR error                                   
BRSAVE   DS    XL(BR_HL)           Save area                                    
BRWORKL  EQU   *-BRWORKD                                                        
                                                                                
BR_D     DSECT                     ** Record buffer **                          
BR_LEN   DS    AL2                 Buffer record length                         
                                                                                
BR_KEY   DS    XL(L'ACCKEY)        Record key                                   
                                                                                
BR_ERRS  DS    XL(L'IOERR)         Error flags                                  
BR_DA    DS    XL(L'IODA)          Disk address                                 
BR_WORK  DS    XL(L'IOWORK)        PUTREC work area                             
BR_HL    EQU   *-BR_D              Length of header                             
                                                                                
BR_REC   DS    XL(IOLENQ)          ACCMST record (including key)                
BR_LNQ   EQU   *-BR_D              Maximum record size                          
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Salary upload header record                                         *         
***********************************************************************         
                                                                                
HDRREC   LKREQ H,R#HSLHDR,NEWREC=Y                                              
PCToken  LKREQ F,1,(D,B#SAVED,QH_TOKEN),CHAR,TEXT=(*,TOKNLIT)                   
1RAcc    LKREQ F,2,(D,B#SAVED,QH_1RACC),CHAR,TEXT=AC#1RACC                      
MOA      LKREQ F,3,(D,B#SAVED,QH_MOA),CHAR,TEXT=AC#MOA                          
*  extension to pass header vals to offine run                                  
HDRVals  LKREQ F,64,(I,B#SAVED,QH_AHEAD),CHAR,TEXT=(*,HEADLIT),OLEN=1           
*                                                                               
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* Salary upload item record                                           *         
***********************************************************************         
                                                                                
ITMREC   LKREQ H,R#HSLELE,NEWREC=Y                                              
PCToken  LKREQ F,01,(D,B#SAVED,QX_TOKEN),CHAR,TEXT=(*,TOKNLIT)                  
SalDate  LKREQ F,02,(D,B#SAVED,QX_SDATE),CDAT,TEXT=AC#DATE                      
*ethod   LKREQ F,03,(D,B#SAVED,QX_METHD),CHAR,TEXT=AC#METH                      
PayCode  LKREQ F,03,(D,B#SAVED,QX_SPYCD),CHAR,TEXT=AC#RSPYC                     
Amount   LKREQ F,04,(D,B#SAVED,QX_AMNT),SPAK,TEXT=AC#AMT                        
* extension to pass valid tsar rec to offline run                               
TSARRec  LKREQ F,64,(I,B#SAVED,QX_TSAR),CHAR,TEXT=(*,TSARLIT),OLEN=1            
*                                                                               
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* Salary upload trailer record                                        *         
***********************************************************************         
                                                                                
TRLREC   LKREQ H,R#HSLTRL,NEWREC=Y                                              
PCToken  LKREQ F,1,(D,B#SAVED,QT_TOKEN),CHAR,TEXT=(*,TOKNLIT)                   
* Test run - header only sent - skips locking etc                               
TRLVals  LKREQ F,64,(I,B#SAVED,QT_ATRLR),CHAR,TEXT=(*,TRLRLIT),OLEN=1           
*                                                                               
         LKREQ E                                                                
***********************************************************************         
* End of request map tables                                          *          
***********************************************************************         
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
***********************************************************************         
* Data Manager HANDLER                                                *         
*                                                                     *         
* DMGRITRN will be entered for any call to VDATAMGR (internal calls)  *         
* DMGRXTRN will be entered for any call to CDATAMGR (external calls)  *         
*                                                                     *         
* All DMGR updates will be redirected to FACWRK (including eg BRA01)  *         
* Special code for ADDTRN, and adding entries to lock records (LOKREC)*         
***********************************************************************         
                                                                                
DMGRITRN SR    RF,RF               Internal entry point - clear RF              
                                                                                
DMGRXTRN NTR1  LABEL=NO,WORK=(RC,DMWORKL)                                       
         J     *+12                                                             
         DC    C'*BODMGR*'                                                      
                                                                                
         USING DMWORKD,RC          RC=A(Local w/s)                              
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
         L     R5,LLP                                                           
         USING LP_D,R5             R5=A(LP_D)                                   
         LM    R2,RB,LP_R2RB                                                    
         USING WORKD,R9            R9=A(WORKD)                                  
         USING SAVED,R8            R8=A(SAVED)                                  
         USING CPYELD,SCPYEL                                                    
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
         JE    *+8                                                              
         MVI   FW_ACT,FW_AWRTD     No - set action to DMWRITE                   
         GOTOR PUTOUT,FW_D         Put output record                            
         MVC   FW_D(FW_DIRHL),DMGRSAVE                                          
         J     DMGREXIT                                                         
                                                                                
DMGR0030 L     R2,DMGRP3           ACCMST/ACCARC updative I/O                   
         MVC   DMGRDA,0(R2)        Set disk address                             
         L     R2,DMGRP4           Point to I/O area                            
         MVC   DMGRKEY,0(R2)       Save record key                              
         SR    RF,RF                                                            
         ICM   RF,3,ACCRLEN-ACCRECD(R2)                                         
         AHI   RF,FW_RECOL         Add on length of overhead                    
         SHI   R2,FW_RECOL         Back up by overhead length                   
         USING FW_D,R2             Build file record                            
         MVC   DMGRSAVE(FW_RECHL),0(R2)                                         
         XC    FW_D(FW_RECHL),FW_D                                              
         STC   R0,FW_FILE          Set file number                              
         STCM  RF,3,FW_RLEN        Set record length                            
         MVC   FW_RKEY,DMGRKEY     Set record key                               
         CLI   FW_RKEY+TBAKTYP-TBARECD,TBAKTYPQ                                 
         JNE   DMGR0040                                                         
         MVI   FW_RTYP,ACRTNBT                                                  
DMGR0040 CLI   FW_RKEY+EXCKTYP-EXCRECD,EXCKTYPQ                                 
         JNE   DMGR0050                                                         
         CLI   FW_RKEY+EXCKSUB-EXCRECD,EXCKSUBQ                                 
         JNE   DMGR0050                                                         
         MVI   FW_RTYP,ACRTEXPC                                                 
DMGR0050 MVI   FW_ACT,FW_APUTR     Set action to PUTREC                         
         MVC   FW_RDA,DMGRDA       Set disk address                             
         CLC   DMPUTF,0(R3)        Test PUTREC                                  
         JE    *+14                                                             
         MVI   FW_ACT,FW_AADDR     No - set action to ADDREC                    
         XC    FW_RDA,FW_RDA       and clear disk address                       
         GOTOR PUTOUT,FW_D         Put output record                            
         MVC   FW_D(FW_RECHL),DMGRSAVE                                          
         J     DMGREXIT                                                         
                                                                                
DMGRREAL GOTOR DATAMGR,DMGRDMCB    Call real DATAMGR                            
                                                                                
DMGREXIT L     R1,DMGRSVR1         Restore caller's parameter list              
         MVC   0(DMGRPL,R1),DMGRDMCB                                            
         CLI   DMGRERRS,0          Set condition code for caller                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Handle special $ADDTRN actions                                      *         
***********************************************************************         
                                                                                
         USING OB_D,VKEYAREA                                                    
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
         GOTOR INIBUF                                                           
         J     DMGREXIT                                                         
                                                                                
ADDTRN02 CLI   DMGRACTN,FW_AATRN   Test ADDTRN transaction record               
         JNE   ADDTRN10                                                         
         L     R2,ATRNREC                                                       
ADDTRN04 MVC   OB_KEY(L'ACTKEY),0(R2)                                           
         GOTOR GETBUF,OB_D         See if key of trans already exists           
         JL    ADDTRN06            No - add to buffer                           
         LLC   RF,TRNKSBR-TRNRECD(R2)                                           
         AHI   RF,1                                                             
         STC   RF,TRNKSBR-TRNRECD(R2)                                           
         J     ADDTRN04                                                         
                                                                                
ADDTRN06 GOTOR ADDBUF,OB_D                                                      
         XC    OB_D(OB_LNQ),OB_D                                                
         MVC   DMGRKEY,0(R2)       Save transaction record key                  
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(R2)                                         
         JNZ   *+6                                                              
         DC    H'0'                Record length can't be zero                  
         CHI   RF,MAXRECLN                                                      
         JNH   *+6                                                              
         DC    H'0'                Nor greater than the maximum                 
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
         MVC   FW_ATMOA,HD_MOAC    Set month of activity                        
         L     R1,DMGRP2                                                        
         MVC   FW_ATCAN,0(R1)      Set contra-account name                      
         GOTOR PUTOUT,FW_D         Put ADDTRN transaction record                
         MVC   FW_D(FW_ATHLQ),DMGRSAVE                                          
         J     DMGREXIT                                                         
                                                                                
ADDTRN10 CLI   DMGRACTN,FW_AAEND   Test ADDTRN end record                       
         JE    *+6                                                              
         DC    H'0'                                                             
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
         EJECT                                                                  
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
VKEYAREA DS    XL(OB_LNQ)                                                       
         ORG   VKEYAREA+(OB_DATA-OB_D)                                          
OB_KEY2  DS    CL(L'ACTKEY)        Real key                                     
         ORG                                                                    
DMWORKL  EQU   *-DMWORKD                                                        
*                                                                               
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Put record to FACWRK recovery                                       *         
***********************************************************************         
                                                                                
PUTOUT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PUTOUT*'                                                      
                                                                                
         LR    R2,R1                                                            
         USING FW_D,R2             R2=A(Record to put)                          
                                                                                
         TM    LP_FLAG,LP_FDRFT    Test 'draft' (validation) mode               
         JZ    *+6                 No                                           
         DC    H'0'                Updative I/O in draft mode                   
                                                                                
         TM    RUNINDS,RUNIPUTF    Test first time call                         
         JNZ   PUTOUT06                                                         
         OI    RUNINDS,RUNIPUTF    Set not first time call                      
         XC    DMGRSEQ#,DMGRSEQ#   Initialise sequence number                   
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    PUTOUT02            No - use FACWRK                              
         GOTO1 VSORTER,DMCB,SORTCARD,SORTTYPE                                   
                                                                                
PUTOUT02 L     R3,WRKBLKR                                                       
         USING WRKIOD,R3           R3=A(FACWRK WRKIO block)                     
         L     R4,WRKIAREC                                                      
H        USING FW_D,R4             Build FACWRK header record                   
         LA    R0,H.FW_D                                                        
         LHI   R1,FW_HDRL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LHI   R0,FW_HDRL                                                       
         STCM  R0,3,H.FW_RLEN                                                   
         MVC   H.FW_HSTMP,FWHSTAMP Set SRUPDxx file stamp                       
         MVI   H.FW_HOLAY,FW_OBOTU Set to call SRUPD60 in FACPAK                
         MVC   H.FW_HSENA,LP_SENO   Set SE number                               
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
         MVC   H.FW_PERLV,ONERL1L  Set 1R ledger levels                         
         MVI   H.FW_BOVLY,FW_BEXPN Set expense module calling                   
         TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    PUTOUT04            No - use FACWRK                              
         GOTO1 VSORTER,DMCB,SORTPUT,H.FW_RLEN                                   
         J     PUTOUT06                                                         
                                                                                
PUTOUT04 GOTOR DATAMGR,WRKIPARM,ADD                                             
         JE    PUTOUT06                                                         
         DC    H'0'                                                             
         DROP  R3,H                                                             
                                                                                
PUTOUT06 TM    FW_FILE,FW_DMGR     Test normal DATAMGR I/O                      
         JNO   PUTOUT08            No                                           
         ICM   R0,15,DMGRSEQ#      Bump record sequence number                  
         AHI   R0,1                                                             
         STCM  R0,15,DMGRSEQ#                                                   
         STCM  R0,15,FW_SEQ#       Set sequence number in record                
                                                                                
PUTOUT08 TM    LP_FLAG,LP_FOFFL    Test running off-line                        
         JZ    PUTOUT10            No - use FACWRK                              
         GOTO1 VSORTER,DMCB,SORTPUT,FW_D                                        
         J     EXITY                                                            
                                                                                
PUTOUT10 L     R1,WRKBLKR          Put record to FACWRK recovery file           
         MVC   PARM(WRKINDX-WRKIPARM),WRKIPARM-WRKIOD(R1)                       
         GOTOR DATAMGR,PARM,ADD,,,FW_D                                          
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Lock a History/Salary rec for the duration of the upload.           *         
* Locks all sequential records, so they can be rebuilt if req                   
* ENTRY: IOKEY holds 1st sequential rec key                                     
* Exit with CC=equal if locked this time or previously by me.         *         
* Exit with CC=not equal if there is a lock failure                   *         
***********************************************************************         
*                                                                               
*                                                                               
*     not tested!                                                               
*                                                                               
*                                                                               
DOLOCK   NTR1  LABEL=NO                                                         
LOCK     USING LLKKEYD,WORK                                                     
* LOCK A SEQUENTIAL RECORD, SAVE TO HISLOCK TABLE                               
*OLOCK10 DS    0H                                                               
*        LA    R2,HISLOCK          Point to lock table                          
*        USING HISLOCKD,R2                                                      
*        LA    RF,HISLOCK                                                       
*        LA    RF,HISLOCKL(RF)                                                  
*OLOCK15 OC    0(L'HISLOCK,R2),0(R2)                                            
*        JZ    DOLOCK25            Add new entry if not already locked          
*        CLC   HD_1RACC,HISL1RAC    Test already locked this record             
*        JNE   DOLOCK20                                                         
*        CLC   HD_MOAC,HISLMOA                                                  
*        JNE   DOLOCK20                                                         
*        CLC   HISLSEQ,IOKEY+PHIKSEQ-PHIRECD                                    
*        JNE   DOLOCK20                                                         
*        DC    H'0'                 yes, die on double lock                     
*OLOCK20 AHI   R2,L'HISLOCK      Bump to next entry                             
*        CR    R2,RF                                                            
*        JL    DOLOCK15                                                         
*        GOTOR UNLOCK              HISLOCK TABLE TOO SMALL                      
*        DC    H'0'                    RELEASE LOCKS AND DIE HERE               
* BUILD LOCKTAB ENTRY                                                           
*OLOCK25 MVC   HISL1RAC,HD_1RACC                                                
*        MVC   HISLMOA,HD_MOAC                                                  
*        MVC   HISLSEQ,IOKEY+PHIKSEQ-PHIRECD                                    
* LOCK RECORD THROUGH LOCKET                                                    
*        GOTOR BLDLOK,(R2)                                                      
*        GOTOR LOCKET,DMCB,('LLKTESTQ',LOCK.LLKKEYD),ACOMFACS                   
*        JE    DOLOCK30                                                         
*        XC    0(L'HISLOCK,R2),0(R2)                                            
*        MVC   ROUERRV,=AL2(AE$X#LOK)                                           
*        J     EXITN                                                            
*                                                                               
*OLOCK30 DS    0H                                                               
*        TM    LP_OFLG1,LP_OFDFT   skip locking on draft run                    
*        JNZ   DOLOCK40                                                         
*        GOTOR LOCKET,DMCB,('LLKLOCKQ',LOCK.LLKKEYD),ACOMFACS                   
*        JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ANY MORE TO LOCK?                                                             
*OLOCK40 DS    0H                                                               
*        GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
*        JNE   EXITY               NO                                           
*        CLC   IOKEY(PHIKSEQ-PHIKEY),IOKEYSAV                                   
*        JNE   EXITY               NOT SEQUENTIAL REC                           
*        J     DOLOCK10                                                         
*        DROP  R2                                                               
***********************************************************************         
* Build a LOCKET key - R1=A(PID#)   DOES NOT LOCK!                    *         
***********************************************************************         
                                                                                
BLDLOK   NTR1  LABEL=NO            Build LOCKET key                             
*                                                                               
*                                                                               
*  not tested!                                                                  
*                                                                               
*                                                                               
*        LR    R0,R1               Point to PID                                 
*        MVC   LOCK.LLOCKSE,LP_SENO                                             
*        MVC   LOCK.LLOCKAGY,LP_AGY                                             
*        MVC   LOCK.LLOCKRTY,=C'P#'                                             
*        MVC   LOCK.LLOCKKEY,SPACES                                             
*        MVC   LOCK.LLOCKKEY(L'HD_CLM#),0(R1)                                   
*        GOTOR VHEXOUT,DMCB,(R0),LOCK.LLOCKKEY,L'HD_PPID#,0                     
*        OC    LOCK.LLOCKKEY,SPACES                                             
*        MVI   LOCK.LLOCKKEY+4,C'X'                                             
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Lock the PID for the duration of the upload.  Exit with CC=equal if *         
* locked this time or previously by me.  Exit with CC=not equal if    *         
* there is a lock failure                                             *         
***********************************************************************         
                                                                                
UNLOCK   NTR1  LABEL=NO                                                         
*                                                                               
*                                                                               
ULOCK02  LA    R2,HISLOCK          Unlock everything I locked so far            
ULOCK04  OC    0(L'HISLOCK,R2),HISLOCK                                          
         JZ    EXITY                                                            
         GOTOR BLDLOK,(R2)         Build LOCKET key                             
         LHI   R0,LLKUNLKQ         On-line unlock action                        
         TM    LP_FLAG,LP_FOFFL                                                 
         JZ    *+8                                                              
         LHI   R0,LLKUNGLQ         Off-line - running as global                 
         TM    LP_OFLG1,LP_OFDFT   TEST RUN, NO ACTUAL LOCKING DONE             
         JNZ   ULOCK10                                                          
         GOTOR LOCKET,DMCB,((R0),LOCK.LLKKEYD),ACOMFACS                         
         JE    ULOCK10                                                          
         DC    H'0'                Failure to unlock                            
ULOCK10  XC    0(L'HISLOCK,R2),0(R2)                                            
         AHI   R2,L'HISLOCK                                                     
         J     ULOCK04                                                          
         DROP  LOCK                                                             
         EJECT                                                                  
***********************************************************************         
* GENERAL EXITS HERE                                                  *         
***********************************************************************         
                                                                                
EXITH    LHI   RE,2                                                             
         J     EXITCC                                                           
EXITL    DS    0H                                                               
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
ATRNREC  EQU   AIO2,,C'A'          A(Transaction record)                        
                                                                                
MAXRECLN EQU   IOLENQ-(L'IODA+L'IOWORK)                                         
                                                                                
*OLDBUF   EQU   1                   Old record buffer                           
NEWBUF   EQU   2                   New record buffer                            
                                                                                
D#TOKEN  EQU   1                   PC token map number                          
D#ERR    EQU   2                   Error message number                         
D#STA    EQU   3                   New status if changed                        
D#ERROW  EQU   10                  Error row number                             
                                                                                
TOKNLIT  DC    C'PC Token'                                                      
TSARLIT  DC    C'*** TSAR Record ***'                                           
HEADLIT  DC    C'*** Header Values **'                                          
TRLRLIT  DC    C'*** Trailer Values **'                                         
         EJECT                                                                  
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
VSORTER  DC    V(SORTER)                                                        
WORKLEN  DC    AL2(WORKL)                                                       
                                                                                
SRUPD60  DC    C'T10D60  '         Update program phase name                    
SORTCARD DC    C'SORT FIELDS=(5,43,A),FORMAT=BI,WORK=1 '                        
SORTTYPE DC    C'RECORD TYPE=V,LENGTH=(2048,,,100,150) '                        
SORTGET  DC    C'GET'                                                           
SORTPUT  DC    C'PUT'                                                           
                                                                                
PZERO    DC    P'0'                                                             
PNEGONE  DC    P'-1'                                                            
P1HUND   DC    P'100'                                                           
P1THOU   DC    P'1000'                                                          
P1MILL   DC    P'1000000'                                                       
MAXHOURS DC    P'2400'                                                          
EFFS     DC    X'FFFFFFFF'                                                      
PONE     DC    P'1'                                                             
P50MAX   DC    P'50'                                                            
ADDEND   DC    C'ADD=END'                                                       
*UN#LMAX DC    XL2'0014'           LOCK TABLE CAPACITY   20                     
RUN#LMAX DC    XL2'09C4'           LOCK TABLE CAPACITY 2500                     
                                                                                
ACT999   DC    CL12'999999999999'                                               
ACTDISC  DC    CL12'MD          '                                               
                                                                                
FWHSTAMP DC    X'00',C'UO'          FACWRK header record stamp                  
                                                                                
RETDATEL DC    AL1(LQ_RDATQ),AL2(LQ_LN1Q)                                       
                                                                                
ACCOUNT  DC    C'ACC'              Account file prefix                          
                                                                                
DMWRTD   DC    C'DMW'              DMWRITE (directory)                          
DMADDD   DC    C'DMA'              DMADD   (directory)                          
DMPUTF   DC    C'PUT'              PUTREC  (file)                               
DMADDF   DC    C'ADD'              ADDREC  (file)                               
                                                                                
DMREAD   DC    C'DMREAD  '         DMREAD  (directory)                          
DMRDHI   DC    C'DMRDHI  '         DMREAD  (directory)                          
DMRSEQ   DC    C'DMRSEQ  '         DMREAD  (directory)                          
DMGETR   DC    C'GETREC  '         GETREC  (file)                               
                                                                                
DMCOMMIT DC    C'COMMIT  '         Recovery checkpoint command                  
                                                                                
$ADDTRN  DC    C'$ATR'             ADDTRN file prefix                           
$PLDREC  DC    C'$PLD'             PLDREC file prefix                           
$TIMREC  DC    C'$TIM'             TIMREC file prefix                           
$LOKREC  DC    C'$LOK'             LOKREC file prefix                           
                                                                                
ADD      DC    C'ADD     '         FACWRK add                                   
CLOSE    DC    C'CLOSE   '         FACWRK close                                 
INDEX    DC    C'INDEX   '         FACWRK index                                 
READ     DC    C'READ    '         FACWRK read                                  
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
                                                                                
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
         DC    AL2(R#HSLHDR),AL1(RECHHDRQ)                                      
         DC    AL2(R#HSLELE),AL1(RECHELEQ)                                      
         DC    AL2(R#HSLTRL),AL1(RECHTRLQ)                                      
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
*                                                                               
DCDICTL  DS    0X                                                               
         DCDDL AC#ALLCS,L'AC@ALLCS                                              
         DCDDL AC#NOORD,L'AC@NOORD                                              
         DCDDL AC#MOA,L'AC@MOA                                                  
         DCDDL AC#LOA,L'AC@LOA                                                  
         DCDDL AC#OTHER,L'AC@OTHER                                              
         DCDDL AC#TRM,L'AC@TRM                                                  
         DCDDL AC#ACTV,L'AC@ACTV                                                
         DCDDL AC#XFR,L'AC@XFR                                                  
DCDICTLX DC    X'FF'                                                            
*                                                                               
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTABL  EQU   *-RECTABD                                                        
*                                                                               
RECHHDRQ EQU   1                   History header                               
RECHELEQ EQU   2                   History item                                 
RECHTRLQ EQU   3                   History trailer                              
*                                                                               
***********************************************************************         
*        PAYROLL CODES AND NUMBERS DSECT                                        
***********************************************************************         
*                                                                               
PAYCDTAB DSECT                     TABLE OF '85' ELEMS                          
PAYCDNUM DS    XL1                 PAY CODE NUMBER                              
PAYCDNME DS    XL5                 PAYROLL CODE                                 
PAYCDREV DS    XL5                 PAYROLL REVERSAL CODE                        
PAYCDPCS DS    0XL2                PCTS  ******* CHANGE WHEN ADDING PCS         
PAYCDPC1 DS    XL1                 PAYROLL PC1 NUMBER                           
PAYCDPC2 DS    XL1                 PAYROLL PC2 NUMBER                           
PAYCDST  DS    XL1                 STATUS (EQUS AS IN ELEM)                     
PAYCDLEN EQU   *-PAYCDTAB                                                       
PCTABLN  EQU   255*PAYCDLEN                                                     
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
SVRDEF   CSECT                                                                  
                                                                                
LLP      DC    A(0)                A(LP_D)                                      
         EJECT                                                                  
                                                                                
*** GENERAL UPLOAD RESPONSE DATA MAP NUMBERS                                    
                                                                                
D#UPLERR EQU   255                                                              
D#UPLNOD EQU   254                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
* Saved working storage                                               *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
                                                                                
***********************************************************************         
* Sacred values                                                       *         
***********************************************************************         
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER mode                           
                                                                                
RUNINDS  DS    X                   ** Run indicators **                         
RUNIPUTF EQU   X'40'               PUTOUT initialised                           
RUNIBUFR EQU   X'20'               BUFREC initialised                           
RUNILMAX EQU   X'10'               Suppress further lock table msgs             
*                                                                               
ASRUPD60 DS    A                   A(SRUPD60)                                   
PROTOFF  DS    A                   A(PROTOFF)                                   
PROTON   DS    A                   A(PROTON)                                    
LOCKET   DS    A                   A(LOCKET)                                    
MASTC    DS    A                   A(MASTC)                                     
DATAMGR  DS    A                   A(DATAMGR) USE VDATAMGR FOR FILE IO!         
WRKBLKR  DS    A                   A(FACWRK WRKIO block)                        
RUN#LOCK DS    H                   HEADER COUNT                                 
*                                                                               
TSARPASS DS    XL(TSPXTNL)         TSAR block for passive pointers              
TSARRECS DS    XL(TSPXTNL)         TSAR block for record buffer                 
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
TSAROLDH DS    XL(TSPXTNL)         TSAR block for old time buffer               
TSARNEWH DS    XL(TSPXTNL)         TSAR block for new time buffer               
*                                                                               
COMFACS  DS    XL(COMFACSL)        Local copy of COMFACS                        
*                                                                               
HISLOCK  DS    20XL(L'HD_1RACC+L'HD_MOAC) TABLE OF LOCKED HISTORY RECS          
HISLOCKL EQU    *-HISLOCK                                                       
*                                                                               
DSDICTL  DS    0C                                                               
AC@ALLCS DS    CL36                                                             
AC@NOORD DS    CL6                                                              
AC@MOA   DS    CL3                                                              
AC@LOA   DS    CL9                                                              
AC@OTHER DS    CL9                                                              
AC@TRM   DS    CL9                                                              
AC@ACTV  DS    CL9                                                              
AC@XFR   DS    CL9                                                              
                                                                                
SAVEVAR  DS    0F                  ** Variables follow **                       
*                                                                               
APAYEND  DS    A                                                                
*                                                                               
ERRTAB   DS    XL500               Saved error messages and data                
* ERRTAB is currently limited by the record size you've asked DDLINKIO          
* to use (LIOBRECL or 1K default).                                              
SAVEVARL EQU   *-SAVEVAR                                                        
                                                                                
***********************************************************************         
* Request values                                                      *         
***********************************************************************         
                                                                                
*              *** Header ***                                                   
                                                                                
QH_VALS  DS    0X                                                               
*                                                                               
QH_TOKEN DS    CL8                 PC Token (echoed in error response)          
QH_1RACC DS    CL12                1R account for person                        
QH_MOA   DS    CL4                 MOA                                          
QH_AHEAD DS    AL4                 A(Passed header values) (run mode)           
QH_DLNQ  EQU   *-QH_VALS                                                        
                                                                                
*              ** Salary element **                                             
                                                                                
QX_TSAR  DS    AL4                 A(Passed TSAR record)                        
QX_TOKEN DS    CL8                 PC Token (echoed in error response)          
QX_SDATE DS    XL2                 Salary Date                                  
QX_SPYCD DS    CL5                 Payroll code                                 
QX_AMNT  DS    PL(L'PDEAMT)        Salary amount - ZERO TO DELETE               
*                                                                               
*              ** Trailer record **                                             
                                                                                
QT_TOKEN DS    CL8                 PC Token (echoed in error response)          
QT_ATRLR DS    AL4                 A(Passed trailer values) (run mode)          
*                                                                               
**********************************************************************          
* Derived header values (passed to update phase)                                
**********************************************************************          
                                                                                
HD_VALS  DS    0F                                                               
HD_1RACC DS    CL12                1R account for person                        
HD_OFC   DS    CL(L'PHIKOFC)                                                    
HD_DPT   DS    CL(L'PHIKDPT)                                                    
HD_SDPT  DS    CL(L'PHIKSBD)                                                    
HD_PERS  DS    CL(L'PHIKPER)                                                    
HD_MOA   DS    XL(L'QH_MOA)        MOA                                          
HD_MOAP  DS    XL2                 MOA (PACKED)                                 
HD_MOAC  DS    XL2                 MOA (PACKED, COMPLEMENT)                     
*HD_PIND  DS    XL1                                                             
HD_TOKEN DS    CL(L'QT_TOKEN)                                                   
*                                                                               
HD_VALSL EQU   *-HD_VALS                                                        
                                                                                
***********************************************************************         
* Derived row values                                                            
***********************************************************************         
                                                                                
EL_VALS  DS    0F                                                               
EL_PAYCD DS    CL(L'PAYCODE)       SALARY PAYCODE                               
EL_PAYNM DS    CL(L'PAYNUM)        SALARY PAYCODE NUMBER                        
EL_DATE  DS    CL(L'PDEDTE)        SALARY DATE                                  
EL_AMNT  DS    CL(L'PDEAMT)        SALARY AMOUNT                                
EL_STAT2 DS    XL1                 PDESTAT2                                     
EL_TOKEN DS    CL(L'QT_TOKEN)                                                   
*                                                                               
HL_VALSL EQU   *-EL_VALS                                                        
                                                                                
***********************************************************************         
* Derived trailer values (passed to update phase)                     *         
***********************************************************************         
                                                                                
TR_VALS  DS    0F                                                               
TR_STAT  DS    CL1                 RECORD UPDATE STATUS                         
TR_STADD EQU   C'1'                ADDED                                        
TR_STCHA EQU   C'2'                CHANGED                                      
TR_#SALR DS    XL1                 COUNT OF SAL RECS IN BUFFER                  
TR_TOKEN DS    CL(L'QT_TOKEN)                                                   
*                                                                               
TR_VALSL EQU   *-TR_VALS                                                        
                                                                                
***********************************************************************         
* General working storage values                                      *         
***********************************************************************         
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      RECORD TYPE                                  
*                                                                               
STPOSTNG DS    X                   ** TIMTRN s/r posting status **              
STBCKOUT EQU   X'80'               Back out old posting                         
STADDNEW EQU   X'40'               Add new posting                              
STNEGATE EQU   X'20'               Post negative amount                         
STBUCKET EQU   X'10'               Update buckets only                          
STA1CBUC EQU   X'08'               Don't post 1C/14 and 1R/1C buckets           
*                                                                               
ANXTERR  DS    A                   A(Next error entry)                          
ALSTERR  DS    A                   A(Previous error entry in errtab)            
*                                                                               
DMGRSEQ# DS    XL(L'FW_SEQ#)       DATAMGR record sequence number               
*                                                                               
HS_CUR   DS    0X                  Current buffer record                        
HS_OLD   DS    XL(HS_LNQ)         Old item buffer record (NXTBUF)               
HS_NEW   DS    XL(HS_LNQ)         New item buffer record (NXTBUF)               
                                                                                
         EJECT                                                                  
                                                                                
SAVEL    EQU   *-SAVED                                                          
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
         EJECT                                                                  
***********************************************************************         
* History/Salary data buffer (passed in FW_?????)                               
*  used to store any derived data needed for update, data not in reqs           
***********************************************************************         
*                                                                               
HS_D     DSECT                                                                  
*                                                                               
HS_KEY   DS    0H                                                               
HS_DATE  DS    XL3                 SALARY DATE                                  
HS_NUM   DS    XL(L'PDENUM)        PAYROLL CODE NUMBER(SEE PAYELD)              
*                                                                               
HS_KEYL  EQU   *-HS_KEY                                                         
*                                                                               
HS_LOC   DS    0XL(HS_LOCL)        ** Data location **                          
HS_DA    DS    XL(L'ACCKDA)        Disk address of History record               
*                                                                               
HS_LOCL  EQU   *-HS_DA                                                          
                                                                                
*HS_BSTAT DS    X                   ** Buffer status **                         
*HS_BSROB EQU   X'40'               Item exists in other buffer                 
*HS_BSRIA EQU   X'20'               Item approved/rejected client mgr.          
*HS_BSRNR EQU   X'10'               Row added                                   
*HS_BSRCR EQU   X'08'               Row amended                                 
*HS_BSRCO EQU   X'04'               New row copied from old buffer              
*                                                                               
HS_BDATA DS    0X                  ** Record data follows **                    
HS_AMT   DS    PL(L'PDEAMT)        AMOUNT (FROM INTERFACE SYSTEM)               
HS_ADJ   DS    PL(L'PDEADJ)        ADJUSTMENT                                   
HS_STAT  DS    XL1                 PDESTAT                                      
*PDERVRSL EQU   X'01'               AUTO REVERSAL                               
*PDEPCS   EQU   X'06'               PDEPC1+PDEPC2 (ALL PERCENTAGES)             
*PDEPC1   EQU   X'02'               GENERATED BY PC1                            
*PDEPC2   EQU   X'04'               GENERATED BY PC2                            
HS_STAT2 DS    XL1                 PDESTAT2                                     
*PDESLAST EQU   X'80'               USE LAST DAY OF MONTH W/MTH OPTION          
*PDESHRTE EQU   X'40'               AMOUNTS ARE HOURLY RATES                    
*PDESADJ  EQU   X'20'               AMOUNT IS AN ADJUSTMENT RATE                
         DS    XL3                 N/D                                          
*                                                                               
HS_LNQ   EQU   *-HS_D                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* Error table                                                         *         
* NOTE: if first char of ET_EXTRA is &, the content is assumed to be            
*     substitution text (space-separated).                                      
***********************************************************************         
                                                                                
ET_D     DSECT                                                                  
ET_EOTQ  EQU   FF                  End of table indiciator                      
ET_LN    DS    X                   Length of this entry                         
ET_ERRNO DS    XL(L'ROUERRV)       Error number                                 
ET_ROWNM DS    XL1                 Row number error applies                     
ET_LN1Q  EQU   *-ET_D                                                           
ET_EXTRA DS    0C                  Extra error text                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* HISTORY REC LOCK TABLE                                              *         
***********************************************************************         
                                                                                
HISLOCKD DSECT                                                                  
HISL1RAC DS    CL12                1R ACCOUNT                                   
HISLMOA  DS    XL2                 MOA                                          
HISLSEQ  DS    XL1                 SEQUENCE NUMBER (PHIKSEQ                     
HISLLNQ  EQU   *-HISLOCKD                                                       
         EJECT                                                                  
*                                                                               
***********************************************************************         
* Optimisation buffer record layout                                   *         
***********************************************************************         
                                                                                
OB_D     DSECT                                                                  
                                                                                
OB_KEY   DS    XL42                Record key                                   
                                                                                
OB_ERROR DS    XL2                 Error number (Zero=OK)                       
                                                                                
OB_DATA  DS    0X                  Data starts here                             
OB_NAME  DS    CL(L'NAMEREC)       Record name                                  
OB_OTHER DS    0X                  Other data                                   
         ORG   OB_D+256                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
                                                                                
                                                                                
       ++INCLUDE SRUPD60D                                                       
       ++INCLUDE ACTRAND                                                        
                                                                                
*  Other included books follow                                                  
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
*&&US                                                                           
       ++INCLUDE ACCATCALLD                                                     
*&&                                                                             
ADDTRND  DSECT                                                                  
       ++INCLUDE ACADDTRND                                                      
       ++INCLUDE ACRCVRECD                                                      
*PREFIX=L                                                                       
       ++INCLUDE FALOCKETD                                                      
*PREFIX=                                                                        
LOCKETD  DSECT                                                                  
LOCKETL  EQU   *-LOCKETD                                                        
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACBRA14   05/28/13'                                      
         END                                                                    
