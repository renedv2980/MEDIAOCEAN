*          DATA SET ACBRA1A    AT LEVEL 017 AS OF 05/28/13                      
*PHASE T6241AA                                                                  
*INCLUDE SORTER                                                                 
ACBRA1A  TITLE '- SALARY DATA UPLOAD (POSTINGS)'                                
*                                                                               
***********************************************************************         
* IF CHANGES ARE MADE TO THIS BOOK,                                             
* PLEASE UPDATE ACBRA1C 'BRA File Upload download server'                       
***********************************************************************         
*                                                                               
* Level change comments                                                         
* ---------------------                                                         
* SMAN 001 23SEP11 <PR000031> First version based on ACREPCQ02                  
* SMAN 002 21OCT11 <BR18739D> Fix building of TRNMOS                            
* SMAN 003 15NOV11 <BR18745D> Validation improvements and small fixes           
* JFOS 004 23NOV11 <BR18862D> Handle no WC amount/no accounts                   
* NSHE 005 02DEC11 Use SORTER instead of FACWRK to buffer records               
* NSHE 006 28AUG12 <BR20226D> Deal with duplicate entries correctly             
* NSHE 007 03SEP12 Change IO routine for auto switching system                  
* NRAK 008 27SEP12 <BR20399D> Stop zero-filling 2d accounts                     
*                                                                               
***********************************************************************         
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=UO,                                                +        
               CODE=ENTRY,                                             +        
               REQUEST=*,                                              +        
               WORKERKEY=BOPU,                                         +        
               RLEN=2000,                                              +        
               LINKIO=Y,                                               +        
               SYSTEM=ACCSYSQ,                                         +        
               FILES=FILES,                                            +        
               ABENDLIST=FAILS,                                        +        
               SLOWLIST=SLOWS,                                         +        
               SYSPHASE=SYSPHASE,                                      +        
               IDF=Y,                                                  +        
               PROGRAM=RCVPBRAQ,                                       +        
               BLOCKS=(B#SAVED,SAVED),                                 +        
               SERVERTYPE=TSTBOTU                                               
                                                                                
ENTRY    NMOD1 0,**BO1A**,RR=RE                                                 
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          R5=A(GLOBAL LITERALS)                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         BNZ   ENTRY02             yes                                          
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
         ST    R6,ARUNFACS         Set A(RUNFACS)                               
         MVC   APRINTER,RPRINTER   and A(PRINTER) for trace routine             
                                                                                
ENTRY04  ST    R8,LP_ABLK2                                                      
         MVC   WRKBLKR,RWRKBLKR    Set A(FACWRK WRKIO block)                    
         ST    RE,SRVRRELO         Save program relocation factor               
         MVC   RUNMODE,RUNPMODE    Set calling mode                             
         DROP  R6,R7                                                            
                                                                                
         LAY   R7,I_CUR                                                         
         USING I_CUR,R7                                                         
         USING IP_RECD,I_CURPOS                                                 
         USING IW_RECD,I_CURWCN                                                 
                                                                                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
                                                                                
***********************************************************************         
* Handle DDLINK/RUNNER modes                                          *         
***********************************************************************         
                                                                                
         CLI   RUNMODE,RRUNSTRQ    Test 'First for run' mode    x'00'           
         JE    SRUN                                                             
         CLI   RUNMODE,RINIREQQ    Test 'Initialise' mode       x'03'           
         JE    SREQ                                                             
         CLI   RUNMODE,RVALREQQ    Test 'Validate record' mode  x'04'           
         JE    UPLD                                                             
         CLI   RUNMODE,RRUNREQQ    Test 'Run request' mode      x'05'           
         JE    UPLD                                                             
         CLI   RUNMODE,RRUNENDQ    Test 'Last for request' mode x'FF'           
         JE    EREQ                                                             
         J     EXITY               Ignore any other modes                       
         EJECT                                                                  
***********************************************************************         
* Handle 'First for run' (once only off-line) mode              x'00' *         
***********************************************************************         
                                                                                
SRUN     LA    R0,SAVED            Clear sacred storage                         
         LHI   R1,SAVEVAR-SAVED                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,LP_ACOM                                                       
         MVC   DATAMGR,CDATAMGR-COMFACSD(RE)                                    
         MVC   PROTOFF,CPROTOFF-COMFACSD(RE)  FAPROT                            
         MVC   PROTON,CPROTON-COMFACSD(RE)                                      
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
         EJECT                                                                  
***********************************************************************         
* Handle 'First for request' (before first upload record) mode  x'03' *         
***********************************************************************         
                                                                                
SREQ     LA    R0,SAVEVAR                                                       
         LHI   R1,SAVEVARL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   TWAMODE,0           Set no errors encountered                    
                                                                                
         L     RF,LP_ACOM          Extract A(LINKIO) from COMFACS               
         MVC   AALINKIO,CLINKIO-COMFACSD(RF)                                    
         MVC   AALIOB,LP_ALIOB     Set A(LIOB)                                  
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    SREQ02                                                           
                                                                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                                
         GOTOR DATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                                
                                                                                
         L     RF,MASTC            Set I/O trace option                         
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
SREQ02   GOTOR PROTOFF             Turn off storage protection                  
         LA    R0,LP_D                                                          
         ST    R0,LLP              Save A(LP_D) locally                         
         GOTOR PROTON              Turn on storage protection                   
                                                                                
         MVC   ACOMFACS,LP_ACOM    Point to real copy of COMFACS                
                                                                                
         MVI   GIND2,GI2MAIN                                                    
         GOTOR (#CPYINI,ACPYINI)   Initialise company values                    
         USING CPYELD,SCPYEL                                                    
         USING CPXELD,SCPXEL                                                    
                                                                                
         MVI   OFFSW,NOQ                                                        
         CLI   OFFIND,FULLYQ                                                    
         BNE   *+8                                                              
         MVI   OFFSW,YESQ          Office based                                 
         TM    SCPYEL+(CPYSTAT4-CPYELD),CPYSOFF2                                
         BZ    *+8                                                              
         MVI   OFFSW,OFFTWO        On 2 char offices                            
                                                                                
         GOTOR OFFPOS              Get office position of ledgers               
                                                                                
SREQ04   L     R0,SRVRRELO                                                      
         L     RF,=A(DCDICTL)                                                   
         AR    RF,R0                                                            
         GOTO1 VDICTATE,DMCB,C'LL  ',(RF),DSDICTL                               
                                                                                
         GOTOR INIBUF              Initialise record buffer                     
                                                                                
         TM    LP_FLAG,LP_FDRFT    Test validation mode                         
         JZ    SREQ06                                                           
         GOTOR (#SETFAC,ASETFAC),'BRO#SALP'                                     
         J     EXITY                                                            
                                                                                
SREQ06   LA    R0,COMFACS          Take local copy of COMFACS                   
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
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Handle 'Run request' (upload a record) mode             x'04'/x'05' *         
***********************************************************************         
                                                                                
UPLD     LA    RF,RECTAB                                                        
         USING RECTABD,RF          RF=A(Record map table)                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   Look up record map code                      
         JE    *+12                                                             
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                                                             
         MVC   RECTYPE,RECTTYPE    Set internal record type                     
         DROP  RF                                                               
         LAY   RE,ERRTAB                                                        
         MVI   0(RE),ET_EOTQ       Clear error table                            
                                                                                
UPLD05   DS    0H                                                               
         GOTOR (#SYSCHK,ASYSCHK)                                                
         JE    UPLD10                                                           
         MVC   ROUERRV,=AL2(AE$FLRD)                                            
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
         J     EXITN                                                            
                                                                                
UPLD10   DS    0H                                                               
         CLI   RECTYPE,RECTHDRQ    Test header record       75                  
         JE    UHDR                                                             
         CLI   RECTYPE,RECTPITQ    Test posting item        76                  
         JE    UPIT                                                             
         CLI   RECTYPE,RECTPWCQ    Test posting work code   77                  
         JE    UPWC                                                             
         CLI   RECTYPE,RECTTRLQ    Test Trailer record      78                  
         JE    UTRL                                                             
         DC    H'0'                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Process posting upload header record  (75 record)                   *         
***********************************************************************         
                                                                                
UHDR     DS    0H                                                               
         MVI   TWAMODE,0           Set to no error                              
                                                                                
         GOTOR BUFSAL,DMCB,('TSAINI',NEWBUF),0                                  
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    UHDR0020                                                         
                                                                                
** Running offline                                                              
         XR    RE,RE                                                            
         ICM   RE,B'0111',QH_AHEAD Test header values passed                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R0,HD_VALS          Save header values to local storage          
         LHI   R1,HD_VALSL                                                      
         LA    RE,LW_LN1Q(,RE)                                                  
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         J     UHDR0160                                                         
                                                                                
** Running online                                                               
UHDR0020 LA    R0,HD_VALS          Initialise header values                     
         LHI   R1,HD_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,TR_VALS          Initialise trailer values                    
         LHI   R1,TR_VALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         OC    CCTPID,CCTPID       Any connected PID ?                          
         JNZ   UHDR0030            Yes                                          
         OI    TWAMODE,TWAMEDP     No, error.                                   
         MVC   ROUERRV,=AL2(AE$NCPID)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
         J     EXITN                                                            
                                                                                
UHDR0030 CLI   CUCTRY,CTRYGER                                                   
         JE    UHDR0040                                                         
         MVC   ROUERRV,=AL2(AE$GERON) For German agencies only                  
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
         J     EXITN                                                            
                                                                                
UHDR0040 DS    0H                                                               
                                                                                
* Set up various dates                                                          
         CLI   CUTSYS,X'73'        For QA                                       
         JNE   UHDR0060                                                         
         MVC   HD_TODP,=X'A60701'                    packed                     
         GOTOR VDATCON,DMCB,(1,HD_TODP),(2,HD_TODC)  compressed                 
         GOTOR (RF),(R1),(1,HD_TODP),(0,HD_TODF)     character                  
         J     UHDR0080                                                         
                                                                                
UHDR0060 GOTOR VDATCON,DMCB,(5,0),(1,HD_TODP)        packed                     
         GOTOR (RF),(R1),(5,0),(2,HD_TODC)           compressed                 
         GOTOR (RF),(R1),(5,0),(0,HD_TODF)           character                  
                                                                                
UHDR0080 CLI   QH_BTYP,TRNTSPOS    BT69?                                        
         JE    UHDR0082                                                         
         MVC   ROUERRV,=AL2(AE$IAFBT)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
         J     UHDR0084                                                         
                                                                                
UHDR0082 MVI   BYTE1,B69UPLD                                                    
         GOTOR VSECRET,DMCB,('SECPRACT+SECPOSP',ASECBLK),              +        
               ('RECSUPD',BYTE1),SECPROG                                        
         JE    UHDR0084                                                         
         MVC   ROUERRV,=AL2(AE$ISBTY)  Invalid security for this BT             
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
                                                                                
UHDR0084 CLC   QH_BREF,SPACES                                                   
         JH    UHDR0090                                                         
         MVC   ROUERRV,=AL2(AE$BRMIS)        Missing batch ref                  
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
                                                                                
UHDR0090 XC    WORK(6),WORK                                                     
         OC    QH_MOA,QH_MOA                                                    
         JNZ   UHDR0092                                                         
         MVC   ROUERRV,=AL2(AE$MSMOA)        Missing MOA                        
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
                                                                                
UHDR0092 MVC   HD_MOA(2),QH_MOA    Period month                                 
         MVI   HD_MOA+2,1          Set first of month                           
         GOTOR VDATCON,DMCB,(1,HD_MOA),(0,WORK)                                 
         GOTOR VDATCON,DMCB,(0,WORK),(1,HD_MOA)                                 
         GOTOR VDATCON,DMCB,(X'81',HD_MOA),(6,DUB2)                             
         MVC   BYTE1,4(R1)                                                      
         MVC   BYTE2,QH_BTYP                                                    
         GOTO1 BMONVAL,PARM,(BYTE1,DUB2),(BYTE2,ACOMFACS),             +        
               (CULANG,TEMP2),(CUXCPY,0)     Validate batch MOS                 
         CLI   TEMP2+(BMOERR-BMONVALD),BMOEOKQ                                  
         JE    UHDR0094                                                         
         MVC   ROUERRV,=AL2(AE$MOSLK)        MOS locked                         
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
                                                                                
UHDR0094 CLC   QH_BNAM,SPACES                                                   
         JH    UHDR0100                                                         
         MVC   ROUERRV,=AL2(AE$BNMIS)        Missing batch name                 
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
                                                                                
UHDR0100 GOTOR VALBAT              Check Batch number not used today            
                                                                                
UHDR0120 DS    0H                                                               
         OI    IND,HEADREC                                                      
         TM    TWAMODE,TWAMERP+TWAMEDP   Any errors ?                           
         JNZ   UEXIT                     Yes                                    
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UEXIT                                                            
                                                                                
UHDR0140 DS    0H                                                               
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTLRQ',H#SHDR),('LQ_TSINQ',HD_VALS),HD_VALSL                 
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
                                                                                
UHDR0160 DS    0H                                                               
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Process posting item record (76 record)                             *         
***********************************************************************         
                                                                                
UPIT     TM    TWAMODE,TWAMEDP     Exit on previous fatal error                 
         JNZ   UEXIT               (No further processing)                      
                                                                                
         LA    R0,IP_REC           Clear IP_ buffer                             
         LHI   R1,IP_LENQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,PS_VALS          Clear posting saved values                   
         LHI   R1,PS_VALSL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,QW_VALS          Clear input values for Work code             
         LHI   R1,QW_LNQ                                                        
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,IS_VALS          Clear saved values                           
         LHI   R1,IS_VALSL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
UPIT0020 CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    UPIT0040                                                         
                                                                                
** Running offline                                                              
         XR    RE,RE                                                            
         ICM   RE,B'0111',QP_AHEAD Test posting item values passed              
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R0,IP_REC           Save item values to local storage            
         LHI   R1,IP_LENQ                                                       
         LA    RE,LW_LN1Q(,RE)                                                  
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     UPIT0070            Move to TSAR buffer                          
                                                                                
** Running online                                                               
                                                                                
UPIT0040 GOTOR POSBLD              Yes, validate and build IP_REC               
         LH    RF,MYHALF1          Only allow 600 items in a batch              
         AHI   RF,1                                                             
         CHI   RF,600                                                           
         JNH   UPIT0050                                                         
         OI    TWAMODE,TWAMEDP                                                  
         MVC   ROUERRV,=AL2(AE$TMNYF)  Too many postings in file                
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
UPIT0050 STH   RF,MYHALF1                                                       
                                                                                
UPIT0060 TM    IND,HEADREC                                                      
         JNZ   UPIT0070                                                         
         MVC   ROUERRV,=AL2(AE$HDMIS)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
         J     UPIT0075                                                         
                                                                                
UPIT0070 GOTOR BUFSAL,DMCB,('TSAADD',NEWBUF),IP_REC                             
         JE    UPIT0075                                                         
         OI    TWAMODE,TWAMEDP                                                  
         MVC   ROUERRV,=AL2(AE$BATMX)                                           
         TM    SALERR,TSEEOF       EOF if full                                  
         JNZ   UPIT0072                                                         
         TM    SALERR,TSEDUP                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ROUERRV,=AL2(AE$DUPIT)                                           
UPIT0072 GOTOR SAVERR,DMCB,ROUERRV,0,QP_SREF,0                                  
                                                                                
UPIT0075 TM    IND,POSTITEM                                                     
         JZ    *+8                                                              
         OI    IND,NEWPOST                                                      
                                                                                
         TM    IND,NEWPOST         Previously posted an item?                   
         JZ    UPIT0100                                                         
         TM    IND,WCITEM          Were there any WC items?                     
         JZ    UPIT0080                                                         
         NI    IND,FF-WCITEM                                                    
         J     UPIT0100                                                         
                                                                                
UPIT0080 MVC   ROUERRV,=AL2(AE$PPRWC)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
                                                                                
UPIT0100 OI    IND,POSTITEM                                                     
         TM    TWAMODE,TWAMERP+TWAMEDP   Any errors ?                           
         JNZ   UEXIT                                                            
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UEXIT                                                            
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTLRQ',I#SPIT),('LQ_TSINQ',IP_REC),IP_LENQ                   
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
                                                                                
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Process work code (77 record)                                       *         
***********************************************************************         
                                                                                
UPWC     TM    TWAMODE,TWAMEDP     Exit on previous fatal error                 
         JNZ   UEXIT               (No further processing)                      
                                                                                
         LA    R0,IW_REC           Clear WC values                              
         LHI   R1,IW_LENQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
UPWC0020 CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    UPWC0040                                                         
                                                                                
** Running offline                                                              
         XR    RE,RE                                                            
         ICM   RE,B'0111',QW_AHEAD Test workcode item values passed             
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R0,IW_REC           Save item values to local storage            
         LHI   R1,IW_WCLQ                                                       
         LA    RE,LW_LN1Q(,RE)     Bump over header                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         J     UPWC0060            Write to TSAR                                
                                                                                
** Running online                                                               
UPWC0040 GOTOR WCNBLD              Validate WC and build IW_REC                 
         TM    IND,HEADREC                                                      
         JNZ   UPWC0050                                                         
         MVC   ROUERRV,=AL2(AE$HDMIS)                                           
         GOTOR SAVERR,DMCB,ROUERRV,0,0,0                                        
                                                                                
UPWC0050 TM    IND,POSTITEM        Posted item already?                         
         JNZ   UPWC0060                                                         
         MVC   ROUERRV,=AL2(AE$WCIPS)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'QW_SREF,QW_SREF),IW_WCSQ,0                
                                                                                
UPWC0060 GOTOR BUFSAL,DMCB,('TSAADD',NEWBUF),IW_REC                             
         JE    UPWC0070                                                         
         MVC   ROUERRV,=AL2(AE$1WCPR)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'QW_SREF,QW_SREF),IW_WCSQ,0                
                                                                                
UPWC0070 OI    IND,WCITEM                                                       
         TM    TWAMODE,TWAMERP+TWAMEDP   Any errors ?                           
         JNZ   UEXIT                     Yes                                    
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JNE   UEXIT                                                            
                                                                                
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTLRQ',P#SPWC),('LQ_TSINQ',IW_REC),IW_WCLQ                   
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTGEL',0),RETDATEL            
                                                                                
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Process trailer record  (78 record)                                 *         
***********************************************************************         
                                                                                
UTRL     TM    IND,HEADREC         Was there ever a HEADER record?              
         JZ    UTRL0010                                                         
         TM    IND,POSTITEM        Posted an item already?                      
         JZ    UTRL0010                                                         
         TM    IND,WCITEM          Any WC items? (must be at least one)         
         JNZ   UTRL0010                                                         
         MVC   ROUERRV,=AL2(AE$PSWCI)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'IP_SREF,IP_SREF),IP_SREF,0                
                                                                                
UTRL0010 TM    TWAMODE,TWAMERP+TWAMEDP   Any errors ?                           
         JNZ   UEXIT               (no further processing)                      
         CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    UTRL0020                                                         
                                                                                
         GOTOR TRNBLD              Build transactions and batches               
         J     UEXIT                                                            
                                                                                
** Running online                                                               
                                                                                
UTRL0020 DS    0H                                                               
         J     UEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* Exit handling for all upload records                                *         
***********************************************************************         
                                                                                
UFATAL   OI    TWAMODE,TWAMEDP     Set fatal error on                           
                                                                                
UEXIT    NI    IND,FF-NEWPOST                                                   
         TM    TWAMODE,TWAMERP+TWAMEDP   Any errors?                            
         JZ    UEXIT220            No                                           
                                                                                
* Build errors                                                                  
UEXIT040 DS    0H                                                               
         CLI   RECTYPE,RECTTRLQ    Test just processed trailer record           
         JNE   UEXIT060                                                         
         NI    TWAMODE,FF-(TWAMERP+TWAMEDP)  Remove error this time             
         OI    TWAMODE,TWAMPER     Set previous error                           
UEXIT060 TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    *+6                                                              
         DC    H'0'                Die to unwind any updates                    
                                                                                
UEXIT080 LAY   R2,ERRTAB           Send errors                                  
         USING ET_D,R2             R2=A(Error table)                            
UEXIT100 CLI   ET_D,ET_EOTQ        Test end of error table                      
         JE    UEXIT200                                                         
         SR    R0,R0                                                            
         ICM   R0,3,LP_QMAPN       Build download map element                   
         GOTOR AALINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',(R0))              
         MVC   DUB1,QH_TOKEN                                                    
         CLI   RECTYPE,RECTHDRQ    Test header record                           
         JE    UEXIT120                                                         
         MVC   DUB1,QP_TOKEN                                                    
         CLI   RECTYPE,RECTPITQ    Test posting item record                     
         JE    UEXIT120                                                         
         MVC   DUB1,QW_TOKEN                                                    
         CLI   RECTYPE,RECTPWCQ    Test posting work code record                
         JE    UEXIT120                                                         
         MVC   DUB1,QT_TOKEN                                                    
         CLI   RECTYPE,RECTTRLQ    Test trailer record                          
         JE    UEXIT120                                                         
         DC    H'0'                                                             
                                                                                
UEXIT120 DS    0H                                                               
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),                         +        
               ('LIOTRAW',D#TOKEN),('LD_CHARQ',DUB1),(L'QH_TOKEN,0)             
                                                                                
UEXIT140 LA    R1,DMCB                                                          
         USING GETTXTD,R1          Build error text string in ELEMENT           
         XC    GTBLOCK,GTBLOCK     Resolve error text                           
         MVI   GTMAXL,80                                                        
         MVC   GTMSGNO,ET_ERRNO                                                 
         LA    R0,ELEMENT                                                       
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMERR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LLC   R0,ET_LN                                                         
         SHI   R0,ET_LN1Q                                                       
         LTR   R0,R0                                                            
         JZ    UEXIT160                                                         
         STC   R0,GTLTXT           Set length of extra text                     
         LA    R0,ET_EXTRA                                                      
         STCM  R0,7,GTATXT         Set A(Extra text)                            
UEXIT160 GOTOR VGETTXT,(R1)                                                     
         LLC   R0,4(R1)            Length of text just added                    
         GOTOR AALINKIO,(R1),('LIOAPUT',LP_ALIOB),                     +        
               ('LIOTRAW',D#ERR),('LD_CHARQ',ELEMENT),((R0),0)                  
UEXIT180 LLC   R0,ET_LN            Bump to next error table entry               
         AR    R2,R0                                                            
         J     UEXIT100                                                         
         DROP  R1,R2                                                            
                                                                                
UEXIT200 CLI   RUNMODE,RVALREQQ    Test 'Validate request' mode                 
         JE    EXITN                                                            
                                                                                
UEXIT220 J     EXITY               Exit back to DDLINK                          
         EJECT                                                                  
***********************************************************************         
* Handle 'Last for request' mode (only passed when running live) x'ff'*         
***********************************************************************         
                                                                                
EREQ     GOTOR GOATRN,0            Call GOATRN for last time                    
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JNZ   EREQ02              Yes - using SORTER not FACWRK                
         L     R1,WRKBLKR          Copy WRKR parameters                         
         MVC   PARM(WRKINDX-WRKIPARM),WRKIPARM-WRKIOD(R1)                       
         GOTOR DATAMGR,PARM,CLOSE  Close the FACWRK file                        
                                                                                
EREQ02   OI    LP_FLAG,LP_FFWRK    Set FACWRK file built                        
         MVC   VDATAMGR,DATAMGR    Point to real DATAMGR                        
         MVC   ACOMFACS,LP_ACOM    Point to real COMFACS                        
                                                                                
***********************************************************************         
* If we are running off-line with updative (global) files re-open the *         
* FACWRK recovery file and read the header record,  call  DATAMGR to  *         
* checkpoint recovery (COMMIT), call SRUPD60 to update the files and  *         
* then checkpoint recovery again to clear the global lock table.      *         
***********************************************************************         
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    EXITY                                                            
         TM    LP_OFLG1,LP_OFDFT   Test 'draft' upload                          
         JNZ   EREQ04                                                           
         TM    LP_INDS,LP_IGLOB    Test global (updative) file                  
         JZ    EXITY                                                            
                                                                                
EREQ04   GOTOR DATAMGR,DMCB,DMCOMMIT,0                                          
                                                                                
*        MVI   LP_OFTRC,X'FF'      Trace                                        
         LA    RE,TSARPASS         Pass A(TSARPASS) in LP_ABLK3                 
         LA    RF,TSARRECS         Pass A(TSARRECS) in LP_ABLK4                 
         LA    R0,TSAROLDI         Pass A(TSAROLDI) in LP_ABLK5                 
         LA    R1,TSARNEWI         Pass A(TSARNEWI) in LP_ABLK6                 
         LA    R2,TSAROBUF         Pass A(TSAROBUF) in LP_ABLK7                 
         LA    R3,VSORTER          Pass A(SORTER) in LP_ABLK8                   
         STM   RE,R3,LP_ABLK3                                                   
         GOTOR ASRUPD60,DMCB,('FF',LP_D),('00',PARM),AIO1                       
                                                                                
         GOTOR DATAMGR,DMCB,DMCOMMIT,0                                          
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate posting item and build IP_REC line                         *         
***********************************************************************         
                                                                                
POSBLD   NTR1  LABEL=*                                                          
         LA    R0,IP_REC           Clear work code input values                 
         LHI   R1,IP_LENQ                                                       
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   IP_RLEN,=AL2(IP_LENQ)   Set length of record                     
         MVC   IP_BTYP,QH_BTYP                                                  
         MVC   IP_TREF,SPACES                                                   
         MVC   WORK(2),QH_MOA                                                   
         MVI   WORK+2,1                                                         
         GOTOR VDATCON,DMCB,(1,WORK),(0,WORK+3)                                 
         MVC   IP_TREF(4),WORK+3                                                
                                                                                
* Deal with sub-reference                                                       
         MVC   IP_SREF,QP_SREF                                                  
                                                                                
* Validate office                                                               
         GOTOR OFFVAL,DMCB,QP_OFF                                               
         JE    POSB0008                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'QP_OFF,QP_OFF),IP_SREF,0                  
                                                                                
* Validate the Debit/Credit U/L pairing                                         
POSB0008 GOTOR PAIRVAL,DMCB,QP_DEBAC,QP_CRDAC                                   
         JE    POSB0010                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'QP_DEBAC+L'QP_CRDAC,QP_DEBAC),   +        
               IP_SREF,0                                                        
                                                                                
* Validate the Debit account                                                    
POSB0010 MVC   IP_DEB,QP_DEBAC                                                  
         GOTOR ACCVAL,DMCB,('DEBITQ',IP_DEB),IP_DEBN                            
         JE    POSB0012                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'IP_DEB,IP_DEB),IP_SREF,0                  
         J     POSB0020                                                         
                                                                                
POSB0012 CLC   IP_DEB(L'LEDGERSE),LEDGERSE                                      
         JNE   POSB0020                                                         
         TM    IP_DRST1,RSTSEADD   Test expense account demands dept            
         JZ    POSB0020                                                         
         GOTOR SEVAL,DMCB,IP_DEB                                                
                                                                                
* Validate the Credit account                                                   
POSB0020 MVC   IP_CRD,QP_CRDAC                                                  
         GOTOR ACCVAL,DMCB,('CREDITQ',IP_CRD),IP_CRDN                           
         JE    POSB0022                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'IP_CRD,IP_CRD),IP_SREF,0                  
         J     POSB0024                                                         
                                                                                
POSB0022 CLC   IP_CRD(L'LEDGERSE),LEDGERSE                                      
         JNE   POSB0024                                                         
         TM    IP_CRST1,RSTSEADD   Test expense account demands dept            
         JZ    POSB0024                                                         
         GOTOR SEVAL,DMCB,IP_CRD                                                
                                                                                
POSB0024 CLC   LEDGERSE,IP_CRD                                                  
         JE    POSB0026                                                         
         CLC   LEDGERSX,IP_DEB                                                  
         JE    POSB0026                                                         
         CLC   LEDGERSV,IP_DEB                                                  
         JE    POSB0026                                                         
         CLC   LEDGERSI,IP_DEB                                                  
         JE    POSB0026                                                         
         CLC   LEDGERSG,IP_CRD     Credit for output VAT                        
         JNE   *+12                                                             
         TM    IP_CRST1,RSTSIVAT                                                
         JNZ   POSB0026                                                         
         CLC   LEDGERSG,IP_DEB     Debit for input VAT                          
         JNE   POSB0030                                                         
         TM    IP_DRST1,RSTSIVAT                                                
         JNZ   POSB0030                                                         
                                                                                
POSB0026 OI    IP_SWAP,IP_SYES                                                  
                                                                                
         XC    IP_DEB,IP_CRD       Swap debit & credit acc CODES                
         XC    IP_CRD,IP_DEB                                                    
         XC    IP_DEB,IP_CRD                                                    
                                                                                
         XC    IP_DEBN,IP_CRDN     Swap debit & credit acc NAMES                
         XC    IP_CRDN,IP_DEBN                                                  
         XC    IP_DEBN,IP_CRDN                                                  
                                                                                
* Validate date                                                                 
POSB0030 OC    QP_DATE,QP_DATE                                                  
         JNZ   POSB0040                                                         
         MVC   ROUERRV,=AL2(AE$MISVA)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'AC8DATE,AC8DATE),IP_SREF,0                
                                                                                
POSB0040 DS    0H                                                               
         GOTOR VDATCON,DMCB,(2,QP_DATE),(1,IP_DATE)                             
                                                                                
         CLC   IS_DATE,IP_DATE                                                  
         JE    POSB0050                                                         
         USING EURKBLKD,RE                                                      
         LA    RE,TEMP                                                          
         XC    0(EURKBLKL,RE),0(RE)                                             
         MVC   EURKCUFR,SCPYEL+(CPYCURR-CPYELD)  Agency currency                
         MVC   EURKMOS,IP_DATE                                                  
         MVC   EURKAFAC,ACOMFACS                                                
         GOTO1 VEUREKA,DMCB,('EZMOSQ',EURKBLKD),DUB,DUB                         
         CLI   0(R1),NOTVALQ                                                    
         JNE   POSB0042                                                         
         MVC   ROUERRV,=AL2(AE$ICMOS)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'IP_DATE,IP_DATE),IP_SREF,0                
         DROP  RE                                                               
                                                                                
POSB0042 MVC   IS_DATE,IP_DATE                                                  
                                                                                
* Validate reference                                                            
POSB0050 CLC   QP_REF,SPACES                                                    
         JH    POSB0060                                                         
         MVC   ROUERRV,=AL2(AE$MISVA)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'AC8REF,AC8REF),IP_SREF,0                  
                                                                                
POSB0060 MVC   IP_REFL,QP_REFL                                                  
         MVC   IP_REF,QP_REF       Reference                                    
         MVC   IP_NARRL(L'QP_NARRL+L'QP_NARR),QP_NARRL Narrative                
                                                                                
POSB0100 DS    0H                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate Workcode and build IW_D line                               *         
***********************************************************************         
                                                                                
WCNBLD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*WCNBLD*'                                                    
                                                                                
         LA    R0,IW_REC           Clear work code input values                 
         LHI   R1,IW_LENQ                                                       
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   IW_RLEN,=AL2(IW_LENQ)   Set length of record                     
                                                                                
         MVC   IW_DEB,QP_DEBAC                                                  
         MVC   IW_CRD,QP_CRDAC                                                  
                                                                                
         TM    IP_SWAP,IP_SYES                                                  
         JZ    WCBLD010                                                         
         MVC   IW_DEB,QP_CRDAC                                                  
         MVC   IW_CRD,QP_DEBAC                                                  
                                                                                
WCBLD010 GOTOR VDATCON,DMCB,(2,QP_DATE),(1,IW_DATE)                             
         MVC   IW_REF,QP_REF                                                    
         MVC   IW_SREF,QW_SREF                                                  
         MVC   IW_WCSQ,QW_WCSQ                                                  
                                                                                
* Check Item number is same as for current Posting Item                         
         CLC   QW_SREF,IP_SREF                                                  
         JE    WCBLD020                                                         
         MVC   ROUERRV,=AL2(AE$WCITN)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'QW_SREF,QW_SREF),IW_WCSQ,0                
                                                                                
* Validate work code                                                            
WCBLD020 GOTOR WCDVAL,QW_WC        Validate the work code                       
         JE    WCBLD100                                                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'QW_WC,QW_WC),IW_WCSQ,0                    
                                                                                
WCBLD100 TP    QW_WAMT             Work code amount                             
         JE    WCBLD102                                                         
         MVC   ROUERRV,=AL2(AE$MISVA)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'QW_WC,QW_WC),IW_WCSQ,0                    
         ZAP   IW_WAMT,PZERO                                                    
         J     *+10                                                             
WCBLD102 ZAP   IW_WAMT,QW_WAMT                                                  
         ZAP   IW_WFAMT,PZERO                                                   
         OC    QW_WFAMT,QW_WFAMT                                                
         JZ    *+10                                                             
         ZAP   IW_WFAMT,QW_WFAMT   Work code foreign amount                     
                                                                                
* Deal with sign of work code amount                                            
         TM    IP_SWAP,IP_SYES     Had IP_DEB and IP_CRD been swapped           
         JZ    WCBLD110            around earlier in POSBLD?                    
         MP    IW_WAMT,=P'-1'      Yes, so reverse sign of amount               
                                                                                
* Deal with narrative                                                           
WCBLD110 XR    RE,RE                                                            
         ICM   RE,B'0111',QW_ANARR Narrative list                               
         JZ    WCBLD160            No list                                      
         XR    R0,R0               # of list entries                            
         ICM   R0,B'0011',LW_NUMN-LW_D(RE)                                      
         LA    RE,LW_LN2Q(,RE)     Bump over header                             
         XR    R1,R1                                                            
         LA    RF,IW_NARRL                                                      
         USING WC_NRARY,RE                                                      
WCBLD150 DS    0H                                                               
         IC    R1,WC_NARRL                                                      
         BASR  R2,0                                                             
         MVC   0(*-*,RF),WC_NARRL  Move length and field                        
         EX    R1,0(R2)                                                         
         LA    RE,L'WC_NARRL+L'WC_NARR(,RE)                                     
         LA    RF,L'IW_NARRL+L'IW_NARR(,RF)                                     
         BRCT  R0,WCBLD150                                                      
         DROP  RE                                                               
                                                                                
WCBLD160 DS    0H                                                               
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate office code                                                *         
***********************************************************************         
                                                                                
OFFVAL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*OFFVAL*'                                                    
                                                                                
         L     R2,0(R1)                                                         
         CLI   OFFIND,NONEQ                                                     
         JE    EXITY                                                            
         CLC   0(L'QP_OFF,R2),SPACES  Test user has entered an office           
         JNH   EXITY                                                            
                                                                                
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,0(R2)                                                   
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              Validate office                              
         JE    OFFVAL02                                                         
                                                                                
         MVC   ROUERRV,=AL2(AE$SECLK)  Office is invalid for logon              
         J     EXITN                                                            
                                                                                
OFFVAL02 MVC   IP_OFF,0(R2)        Save validate office code                    
         J     EXITY                                                            
         DROP  R1                                                               
         EJECT                                                                  
                                                                                
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Validate DEBIT/CREDIT U/L pairing                                   *         
***********************************************************************         
                                                                                
PAIRVAL  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PAIRVA*'                                                      
                                                                                
         LM    R2,R3,0(R1)             R2,R3=A(QP_DEBAC,QP_CRDAC)               
                                                                                
         USING PAITABD,R6                                                       
         LA    R6,PAIRTAB                                                       
         LA    RF,PAITABN                                                       
PAIRV02  CLC   PAITFRM,0(R2)           R2=QP_DEBAC                              
         JNE   PAIRV04                 R3=QP_CRDAC                              
         CLC   PAITTO,0(R3)                                                     
         JNE   PAIRV04                                                          
         MVC   ROUERRV,=AL2(AE$IULCB) Invalid unit/ledger combination           
         J     EXITN                                                            
                                                                                
PAIRV04  LA    R6,PAITABL(R6)                                                   
         JCT   RF,PAIRV02                                                       
         DROP  R6                                                               
                                                                                
PAIRV08  DS    0H            CHECK FOR UNIT S TO OTHER UNIT PAIRINGS            
         CLI   0(R2),UNIT#S                                                     
         JE    PAIRV10                                                          
         CLI   0(R3),UNIT#S                                                     
         JNE   PAIRVY                                                           
         MVC   ROUERRV,=AL2(AE$IULCB) Invalid unit/ledger combination           
         J     EXITN                                                            
                                                                                
PAIRV10  CLI   0(R3),UNIT#S                                                     
         JE    PAIRVY                                                           
         MVC   ROUERRV,=AL2(AE$IULCB) Invalid unit/ledger combination           
         J     EXITN                                                            
                                                                                
PAIRVY   J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate DEBIT/CREDIT account                                       *         
***********************************************************************         
                                                                                
ACCVAL   NTR1  LABEL=NO,WORK=(RC,VBWORKL)                                       
         J     *+12                                                             
         DC    C'*ACCVAL*'                                                      
                                                                                
         USING VBWORKD,RC              R2,R3=A(IP_DEB,IP_DEBN)                  
         LM    R2,R3,0(R1)         Or  R2,R3=A(IP_CRD,IP_CRDN)                  
         MVC   MYBYTE1,0(R1)                                                    
         GOTOR CLRWRK,VBWORKL                                                   
         USING OB_D,VBRBAREA                                                    
         XC    ROUERRV,ROUERRV                                                  
                                                                                
         CLI   OFFSW,NOQ           Test agency on offices                       
         JE    ACCVA04                                                          
                                                                                
         CLC   0(L'ACTKUNT+L'ACTKLDG,R2),LEDGERSA                               
         JNE   *+12                                                             
         LA    RF,OFFPOSA                                                       
         J     ACCVA02                                                          
                                                                                
         CLC   0(L'ACTKUNT+L'ACTKLDG,R2),LEDGERSE                               
         JNE   *+12                                                             
         LA    RF,OFFPOSE                                                       
         J     ACCVA02                                                          
                                                                                
         CLC   0(L'ACTKUNT+L'ACTKLDG,R2),LEDGERSI                               
         JNE   *+12                                                             
         LA    RF,OFFPOSI                                                       
         J     ACCVA02                                                          
                                                                                
         CLC   0(L'ACTKUNT+L'ACTKLDG,R2),LEDGERSQ                               
         JNE   *+12                                                             
         LA    RF,OFFPOSQ                                                       
         J     ACCVA02                                                          
                                                                                
         CLC   0(L'ACTKUNT+L'ACTKLDG,R2),LEDGERSR                               
         JNE   *+12                                                             
         LA    RF,OFFPOSR                                                       
         J     ACCVA02                                                          
                                                                                
         CLC   0(L'ACTKUNT+L'ACTKLDG,R2),LEDGERSV                               
         JNE   *+12                                                             
         LA    RF,OFFPOSV                                                       
         J     ACCVA02                                                          
                                                                                
         CLC   0(L'ACTKUNT+L'ACTKLDG,R2),LEDGERSX                               
         JNE   *+12                                                             
         LA    RF,OFFPOSX                                                       
         J     ACCVA02                                                          
                                                                                
         CLC   0(L'ACTKUNT+L'ACTKLDG,R2),LEDGERSG                               
         JNE   *+12                                                             
         LA    RF,OFFPOSG                                                       
         J     ACCVA02                                                          
                                                                                
         MVC   ROUERRV,=AL2(AE$MOIUL) Missing/invalid unit+ledger               
         J     ACCVAX                                                           
                                                                                
ACCVA02  CLI   0(RF),LDGONONE+1    Test if OFFPOS is within the account         
         JL    ACCVA04             No                                           
         CLI   0(RF),LDGOKEY                                                    
         JH    ACCVA04             No                                           
         SR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         BCTR  R0,0                R0=Displacement to office in account         
         LA    R4,L'ACTKUNT+L'ACTKLDG(R2)                                       
         AR    R4,R0               R4=A(Office position in output area)         
                                                                                
         XR    R1,R1                                                            
         CLI   OFFSW,OFFTWO                                                     
         JNE   *+8                                                              
         LA    R1,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,R4),QP_OFF      Move office to output area                   
         EX    R1,0(RE)                                                         
                                                                                
K        USING ACTRECD,IOKEY                                                    
ACCVA04  MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKULA,0(R2)                                                  
         OC    K.ACTKULA,SPACES                                                 
         MVC   OB_KEY(L'ACTKEY),K.ACTKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JL    ACCVA06                                                          
         JH    EXITN                                                            
         MVC   0(L'NAMEREC,R3),OB_NAME   R3=A(IP_DEBN) or A(IP_CRDN)            
         MVC   IP_CRST1,OB_EXST1                                                
         CLI   MYBYTE1,DEBITQ                                                   
         JNE   EXITY                                                            
         MVC   IP_DRST1,OB_EXST1                                                
         J     EXITY                                                            
                                                                                
ACCVA06 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                                
         JE    ACCVA08                                                          
         MVC   ROUERRV,=AL2(AE$MIACC)    Missing account                        
         J     ACCVAX                                                           
                                                                                
ACCVA08  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GETNAM,OB_NAME                                                   
         MVC   0(L'NAMEREC,R3),OB_NAME   R3=A(IP_DEBN) OR A(IP_CRDN)            
         GOTOR GETELA,ABLELQ             Low level account?                     
         JE    ACCVA10                                                          
         MVC   ROUERRV,=AL2(AE$NLOWA)    Not a low level account                
         J     ACCVAX                                                           
                                                                                
ACCVA10  GOTOR GETELA,RSTELQ                                                    
         JNE   ACCVA12                                                          
         USING RSTELD,R1                                                        
         MVC   OB_EXST1,RSTSTAT1                                                
         MVC   IP_CRST1,OB_EXST1                                                
         CLI   MYBYTE1,DEBITQ                                                   
         JNE   ACCVA12                                                          
         MVC   IP_DRST1,OB_EXST1                                                
         DROP  R1                                                               
                                                                                
ACCVA12  GOTOR ADDBUF,OB_D                                                      
         J     EXITY                                                            
                                                                                
ACCVAX   MVC   OB_ERROR,ROUERRV    Add record in error to buffer                
         GOTOR ADDBUF,OB_D                                                      
         J     EXITN                                                            
                                                                                
VBWORKD  DSECT                     ** ACCVAL local w/s **                       
VBRBAREA DS    XL(OB_LNQ)                                                       
         ORG   VBRBAREA+(OB_OTHER-OB_D)                                         
OB_EXST1 DS    XL(L'RSTSTAT1)                                                   
         ORG                                                                    
VBWORKL  EQU   *-VBWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Deal with 28 and 2D accounts                                                  
***********************************************************************         
                                                                                
SEVAL    NTR1  LABEL=NO,WORK=(RC,V2WORKL)                                       
         J     *+12                                                             
         DC    C'*SEVAL**'                                                      
                                                                                
         USING V2WORKD,RC                                                       
         L     R2,0(R1)            R2=A(IP_DEB) or A(IP_CRD)                    
         GOTOR CLRWRK,V2WORKL                                                   
         USING OB_D,V2RBAREA                                                    
         XC    ROUERRV,ROUERRV                                                  
                                                                                
* Check for 28 ledger                                                           
K        USING LDGRECD,IOKEY                                                    
         MVC   K.LDGKEY,SPACES                                                  
         MVC   K.LDGKCPY,CUXCPY                                                 
         MVC   K.LDGKUNT(L'LDGKUNT+L'LDGKLDG),LEDGER28                          
         MVC   OB_KEY(L'LDGKEY),K.LDGKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JE    SEVAL02                                                          
         JL    SEVAL00                                                          
         GOTOR SAVERR,DMCB,ROUERRV,(L'XERRTXT,XERRTXT),IP_SREF,0                
         J     SEVALAX                                                          
                                                                                
SEVAL00  GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    SEVAL01                                                          
         MVC   ROUERRV,=AL2(AE$MISLE)    Missing ledger                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'LEDGER28,LEDGER28),IP_SREF,0              
         J     SEVAL06                                                          
                                                                                
SEVAL01  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR TSTSEC,0            Test security                                
         MVC   OB_ERROR,ROUERRV    Set error if any                             
         GOTOR ADDBUF,OB_D                                                      
                                                                                
*Set up 28 account                                                              
SEVAL02  MVC   IP_28,0(R2)                                                      
         MVC   IP_28(L'ACTKUNT+L'ACTKLDG),LEDGER28                              
         USING ACTRECD,R5                                                       
         LA    R5,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,IP_28                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SEVAL04                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SEVAL04                                                          
         GOTOR GETNAM,WORK                                                      
         MVC   IP_28N,WORK                                                      
         J     SEVAL06                                                          
                                                                                
SEVAL04  MVC   ROUERRV,=AL2(AE$MIACC)    Missing account                        
         GOTOR SAVERR,DMCB,ROUERRV,(L'IP_28,IP_28),IP_SREF,0                    
                                                                                
* Check for 2D ledger                                                           
SEVAL06  GOTOR CLRWRK,V2WORKL                                                   
         USING OB_D,V2RBAREA                                                    
         XC    ROUERRV,ROUERRV                                                  
                                                                                
K        USING LDGRECD,IOKEY                                                    
         MVC   K.LDGKEY,SPACES                                                  
         MVC   K.LDGKCPY,CUXCPY                                                 
         MVC   K.LDGKUNT(L'LDGKUNT+L'LDGKLDG),LEDGER2D                          
         MVC   OB_KEY(L'LDGKEY),K.LDGKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JE    SEVAL10                                                          
         JL    SEVAL08                                                          
         GOTOR SAVERR,DMCB,ROUERRV,(L'XERRTXT,XERRTXT),IP_SREF,0                
         J     SEVALAX                                                          
                                                                                
SEVAL08  GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    SEVAL09                                                          
         MVC   ROUERRV,=AL2(AE$MISLE)    Missing ledger                         
         GOTOR SAVERR,DMCB,ROUERRV,(L'LEDGER2D,LEDGER2D),IP_SREF,0              
         J     SEVALAX                                                          
                                                                                
SEVAL09  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR TSTSEC,0            Test security                                
         MVC   OB_ERROR,ROUERRV    Set error if any                             
         GOTOR ADDBUF,OB_D                                                      
                                                                                
* Set up 2D account                                                             
SEVAL10  MVC   IP_2D,SPACES                                                     
         MVC   IP_2D(L'ACTKUNT+L'ACTKLDG),LEDGER2D                              
         LA    R4,IP_2D+L'ACTKUNT+L'ACTKLDG                                     
         XR    RE,RE                                                            
         CLI   OFFSW,NOQ           Do we have offices                           
         JE    SEVAL11             No - then skip compare                       
         CLI   OFFSW,OFFTWO                                                     
         JNE   *+8                                                              
         LA    RE,1                                                             
         BASR  R1,0                                                             
         CLC   QP_DEPT(0),QP_OFF                                                
         EX    RE,0(R1)                                                         
         JE    SEVAL11                                                          
         MVC   ROUERRV,=AL2(AE$AONMO) Acc off does not match suppl off          
         GOTOR SAVERR,DMCB,ROUERRV,(L'QP_DEPT,QP_DEPT),IP_SREF,0                
                                                                                
SEVAL11  LA    RE,QP_DEPT+L'QP_DEPT-1                                           
         LA    R1,L'QP_DEPT                                                     
SEVAL12  CLI   0(RE),C' '          Work out length of account                   
         JH    SEVAL14                                                          
         SHI   RE,1                                                             
         JCT   R1,SEVAL12                                                       
         J     SEVAL20                                                          
                                                                                
SEVAL14  BCTR  R1,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R4),QP_DEPT     Take department                              
         EX    R1,0(RF)                                                         
         OC    IP_2D,SPACES                                                     
         LA    R5,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKULA,IP_2D                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SEVAL16                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    SEVAL18                                                          
                                                                                
SEVAL16  MVC   ROUERRV,=AL2(AE$MIACC)    Missing account                        
         GOTOR SAVERR,DMCB,ROUERRV,(L'IP_2D,IP_2D),IP_SREF,0                    
         J     SEVALAX                                                          
                                                                                
SEVAL18  GOTOR GETNAM,WORK                                                      
         MVC   IP_2DN,WORK                                                      
         GOTOR GETELA,ABLELQ       Test low level account                       
         JE    SEVALAX                                                          
SEVAL20  MVC   ROUERRV,=AL2(AE$INVDP)    Invalid department                     
         GOTOR SAVERR,DMCB,ROUERRV,(L'QP_DEPT,QP_DEPT),IP_SREF,0                
                                                                                
SEVALAX  J     EXIT                                                             
         DROP  R5,RC,K                                                          
                                                                                
V2WORKD  DSECT                     ** SEVAL local w/s **                        
V2RBAREA DS    XL(OB_LNQ)                                                       
V2WORKL  EQU   *-V2WORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
**********************************************************************          
* Validate Work codes                                                *          
**********************************************************************          
                                                                                
WCDVAL   NTR1  LABEL=NO,WORK=(RC,VWWORKL)                                       
         J     *+12                                                             
         DC    C'*WCDVAL*'                                                      
                                                                                
         USING VWWORKD,RC                                                       
         MVC   IW_WC,0(R1)         Work code                                    
         CLC   IW_WC,SPACES        Any work code?                               
         JNH   EXITY               No, no checking required                     
         GOTOR CLRWRK,VWWORKL      Clear work area                              
         USING OB_D,VWRBAREA                                                    
                                                                                
         CLC   IW_WC,=C'99'        Can't be special types                       
         JE    *+14                                                             
         CLC   IW_WC,=C'**'                                                     
         JNE   *+14                                                             
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     EXITN                                                            
                                                                                
         CLI   CUCTRY,CTRYGER      Type of work code OK?                        
         JNE   WCDVAL00                                                         
         CLI   IW_WC,C'0'          Disallow external work codes                 
         JNL   WCDVAL00                                                         
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     EXITN                                                            
                                                                                
K        USING WCORECD,IOKEY                                                    
WCDVAL00 MVC   K.WCOKEY,SPACES      Build key of work code record               
         MVI   K.WCOKTYP,WCOKTYPQ                                               
         MVC   K.WCOKCPY,CUXCPY                                                 
         MVC   K.WCOKUNT(L'PRODUL),PRODUL                                       
         MVC   K.WCOKWRK,IW_WC                                                  
         MVC   OB_KEY(L'WCOKEY),K.WCOKEY                                        
         DROP  K                                                                
                                                                                
         GOTOR GETBUF,OB_D         Read buffer record                           
         JH    EXITN               Exit if found or bad                         
         JE    EXITY                                                            
                                                                                
* Not found, read record and then add to buffer                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$WRKNF)                                           
         J     WCDVALN                                                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR GETELA,WCOELQ       Locate work code element                     
         JE    *+14                                                             
         MVC   ROUERRV,=AL2(AE$INWRK)                                           
         J     WCDVALN                                                          
                                                                                
         LR    R2,R1               A(WCOEL)                                     
         USING WCOELD,R2                                                        
         TM    WCOSTAT2,WCOSLPST   work code locked for postings?               
         JZ    *+14                                                             
         MVC   ROUERRV,=AL2(AE$WCLCK)                                           
         J     WCDVALN                                                          
                                                                                
         GOTOR ADDBUF,OB_D         Add valid entry to buffer                    
         J     EXITY                                                            
                                                                                
WCDVALN  MVC   OB_ERROR,ROUERRV    Add invalid entry buffer                     
         GOTOR ADDBUF,OB_D                                                      
         J     EXITN                                                            
         DROP  R2                                                               
VWWORKD  DSECT                     ** WCDVAL local w/s **                       
VWRBAREA DS    XL(OB_LNQ)                                                       
         ORG   VWRBAREA+(OB_OTHER-OB_D)                                         
VWWORKL  EQU   *-VWWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Clear work area (RC=A(Work area), R1=L'Work area)                   *         
***********************************************************************         
                                                                                
CLRWRK   STM   RE,R1,12(RD)                                                     
         LR    R0,RC                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* Save error messages in an area so we can send all errors together   *         
* Ntry: P1/0                                                                    
*         /1-3                                                                  
*       P2/0                                                                    
***********************************************************************         
                                                                                
SAVERR   NTR1  LABEL=*                                                          
                                                                                
         CLI   RUNMODE,RVALREQQ    Can't handle errors if live update           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RF,ANXTERR          Address of next error in table               
         LAY   RE,ERRTAB                                                        
         CLI   0(RE),ET_EOTQ       If table is empty                            
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
         LTR   R4,R4               Any row # ?                                  
         JZ    *+10                No                                           
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
* Extract name of record pointed to by IOADDR                         *         
*                                                                     *         
* Ntry:- R1=A(Output area)                                            *         
***********************************************************************         
                                                                                
GETNAM   STM   RE,R2,12(RD)                                                     
         LR    R2,R1               R2=A(Output area)                            
         MVC   0(L'NAMEREC,R2),SPACES                                           
         GOTOR GETELA,NAMELQ       Locate name element on record                
         JNE   GETNAMX                                                          
         USING NAMELD,R1                                                        
         CLI   NAMLN,NAMLN1Q       Test good length                             
         JNH   GETNAMX                                                          
         LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         CHI   RF,L'NAMEREC-1                                                   
         JNH   *+8                                                              
         LHI   RF,L'NAMEREC-1                                                   
         BASR  RE,0                                                             
         MVC   0(0,R2),NAMEREC     Move name to output area                     
         EX    RF,0(RE)                                                         
GETNAMX  LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Locate an element in record pointed to by IOADDR                    *         
*                                                                     *         
* Ntry:- R1=Element code                                              *         
* Exit:- R1=A(Element) and CC=Equal if element found                  *         
*        R1=0 and CC=Not equal if element not found                   *         
***********************************************************************         
                                                                                
GETELA   LR    RF,R1               RF=Element code to search for                
         L     R1,IOADDR                                                        
         AHI   R1,ACCRFST-ACCRECD                                               
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
* Test security for an account record                                 *         
*                                                                     *         
* Ntry:- R1=A(Account status element) or 0 to locate status element   *         
*        on current record                                            *         
* Exit:- CC=Equal if okay, not equal if security lockout with ROUERRV *         
*        set to message number                                        *         
***********************************************************************         
                                                                                
TSTSEC   LTR   R1,R1               Test pointing to status element              
         JNZ   TSTSEC02                                                         
         ST    RE,12(RD)                                                        
         GOTOR GETELA,RSTELQ       Locate status element on record              
         L     RE,12(RD)                                                        
         JE    TSTSEC02                                                         
         CR    RE,RE               No status element - set OK                   
         BR    RE                  (Element pointer will be zero)               
                                                                                
         USING RSTELD,R1                                                        
TSTSEC02 CLI   CUAUTH+1,0          Allow if zero                                
         BER   RE                                                               
         CLC   RSTSECY+1(1),CUAUTH+1                                            
         JNH   *+12                                                             
         MVC   ROUERRV,=AL2(AE$SECLK)                                           
         BR    RE                                                               
         CR    RE,RE               Set CC=Equal                                 
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* Get office position                                                 *         
***********************************************************************         
                                                                                
OFFPOS   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*OFFPOS*'                                                      
                                                                                
         LA    R2,OFFPTAB          Get office position of ledgers               
         LA    R3,OFFPOSA                                                       
         XC    0(OFFDLN,R3),0(R3)                                               
OFFP02   CLI   0(R2),X'00'                                                      
         JE    OFFPX                                                            
                                                                                
         USING LDGRECD,R4                                                       
         LA    R4,IOKEY                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUXCPY                                                   
         MVC   LDGKUNT,0(R2)                                                    
         MVC   LDGKLDG,L'LDGKUNT(R2)                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   OFFP08                                                           
                                                                                
OFFP04   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R4,AIO1                                                          
         LA    R4,LDGRFST                                                       
         SR    RF,RF                                                            
         USING LDGELD,R4                                                        
OFFP06   CLI   LDGEL,0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   LDGEL,LDGELQ        Look for ledger element                      
         JE    *+14                                                             
         IC    RF,LDGLN                                                         
         AR    R4,RF                                                            
         J     OFFP06                                                           
                                                                                
         MVC   0(L'LDGOPOS,R3),LDGOPOS   Save office position                   
                                                                                
OFFP08   AHI   R2,2                                                             
         LA    R3,1(R3)                                                         
         J     OFFP02                                                           
                                                                                
OFFPX    J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Interface to TSAR for salary buffers                                *         
*                                                                     *         
* Note that when running off-line the TSAR buffers are acquired only  *         
* once - the XC(TSPNEWL) below is intentional as the first time       *         
* through the code TSAR will issue the GETMAIN for the buffer         *         
***********************************************************************         
                                                                                
BUFSAL   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*BUFSAL*'                                                      
                                                                                
         LR    R2,R1               R2=A(Caller's parameter list)                
         LA    R3,TSAROLDI         Point to correct TSAR block                  
                                                                                
         USING TSARD,R3            R3=A(TSAR block)                             
         LAY   R0,GENAEXTN                                                      
         ST    R0,TSAREC           Set A(Record)                                
         CLI   0(R2),TSAINI        Test initialisation call                     
         JNE   BUFSAL02                                                         
         XC    TSARD(TSPNEWL),TSARD                                             
         MVC   TSACTN,0(R2)        Action                                       
         MVC   TSACOM,ACOMFACS     A(COMFACS)                                   
         LHI   R0,(3*ONEK)                                                      
         STCM  R0,3,TSBUFFL        Set require 1MB off-line                     
         MVI   TSRECI,TSRMINB1+TSRMINB2+TSRXTN+TSRVAR                           
         MVI   TSKEYL,IP_KEYL      Set key length                               
         LHI   R0,IW_LENQ                                                       
         STCM  R0,3,TSRECL         Set max record length                        
         MVI   TSINDS,TSINODSK     Set no disk writes (save/restore)            
         GOTOR VTSAR,TSARD                                                      
         TM    TSINDS,TSIINIOK                                                  
         JNZ   EXITY                                                            
         DC    H'0'                Initialisation failure                       
                                                                                
BUFSAL02 TM    TSINDS,TSIINIOK     Test initialised                             
         JZ    BUFSAL04                                                         
                                                                                
         MVC   TSACTN,0(R2)        Set action                                   
         OC    4(L'TSAREC,R2),4(R2)                                             
         JZ    *+10                                                             
         MVC   TSAREC,4(R2)        Set A(record) if passed                      
         GOTOR VTSAR,TSARD         Call TSAR                                    
         MVC   SALERR,TSERRS       Return TSERRS in SALERR                      
         J     BUFSALX                                                          
                                                                                
BUFSAL04 MVI   SALERR,TSEEOF       Set EOF if not initialised                   
                                                                                
BUFSALX  CLI   SALERR,0            Set condition code for caller                
         J     EXIT                                                             
         DROP  R3                                                               
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
         TM    T.TSERRS,TSEDUP     test duplicate                               
         JZ    *+14                                                             
         OC    OB_ERROR,OB_ERROR   test upload rec in error                     
         JNZ   *+6                 missing accs may result in dup tskey         
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
* Posting upload header record                                        *         
***********************************************************************         
                                                                                
HDRREC   LKREQ H,A#POHD,NEWREC=Y                                                
PCToken  LKREQ F,01,(D,B#SAVED,QH_TOKEN),CHAR,TEXT=(*,TOKNLIT)                  
BType    LKREQ F,02,(D,B#SAVED,QH_BTYP),LBIN,TEXT=AC#BATTY                      
BatRf    LKREQ F,03,(D,B#SAVED,QH_BREF),CHAR,TEXT=AC#BATRF                      
BatMOA   LKREQ F,04,(D,B#SAVED,QH_MOA),HEXD,TEXT=AC#MOA                         
BatNm    LKREQ F,05,(D,B#SAVED,QH_BNAM),CHAR,TEXT=AC#BATN                       
BatCm    LKREQ F,06,(D,B#SAVED,QH_BCOML),(R,VALTXT),TEXT=AC#CMTS,      +        
               LOWERCASE=Y,OLEN=L'QH_BCOM+L'QH_BCOML                            
                                                                                
H#SHDR   EQU   100                                                              
HDRVals  LKREQ F,H#SHDR,(I,B#SAVED,QH_IHEAD),CHAR,                     +        
               TEXT=(*,HEADLIT),OLEN=1                                          
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Posting upload item record                                          *         
***********************************************************************         
                                                                                
PITREC   LKREQ H,A#PITM,NEWREC=Y                                                
PCToken  LKREQ F,01,(D,B#SAVED,QP_TOKEN),CHAR,TEXT=(*,TOKNLIT)                  
ItemNum  LKREQ F,02,(D,B#SAVED,QP_SREF),LBIN,TEXT=AC#ITMS                       
DebAcc   LKREQ F,03,(D,B#SAVED,QP_DEBAC),CHAR,TEXT=AC#DRA                       
CrdAcc   LKREQ F,04,(D,B#SAVED,QP_CRDAC),CHAR,TEXT=AC#CRA1                      
Ref      LKREQ F,05,(D,B#SAVED,QP_REFL),(R,VALTXT),TEXT=AC#REFN,       +        
               LOWERCASE=Y,OLEN=L'QP_REF+L'QP_REFL                              
PDate    LKREQ F,06,(D,B#SAVED,QP_DATE),CDAT,TEXT=AC#DATE                       
Office   LKREQ F,07,(D,B#SAVED,QP_OFF),CHAR,TEXT=AC#OFFC                        
VATcode  LKREQ F,08,(D,B#SAVED,QP_VAT),CHAR,TEXT=AC#VATC1                       
VATacc   LKREQ F,09,(D,B#SAVED,QP_VATA),CHAR,TEXT=AC#VATA                       
Ord#     LKREQ F,10,(D,B#SAVED,QP_ORD#),CHAR,TEXT=AC#ORNUM                      
OrdDte   LKREQ F,11,(D,B#SAVED,QP_ORDTE),PDAT,TEXT=AC#ORDDT                     
FCurc    LKREQ F,12,(D,B#SAVED,QP_FCURC),CHAR,TEXT=AC#CURRC                     
ExchRat  LKREQ F,13,(D,B#SAVED,QP_XRAT),HEXD,TEXT=AC#EXCHR                      
Client   LKREQ F,14,(D,B#SAVED,QP_CLI),CHAR,TEXT=AC#CLIC                        
Product  LKREQ F,15,(D,B#SAVED,QP_PRO),CHAR,TEXT=AC#PROC                        
Job      LKREQ F,16,(D,B#SAVED,QP_JOB),CHAR,TEXT=AC#JOBC                        
Dept     LKREQ F,17,(D,B#SAVED,QP_DEPT),CHAR,TEXT=AC#DPT                        
Staff    LKREQ F,18,(D,B#SAVED,QP_STAFF),CHAR,TEXT=AC#STAFF                     
Miles    LKREQ F,19,(D,B#SAVED,QP_MILE),SPAK,TEXT=AC#MILES                      
Est#     LKREQ F,20,(D,B#SAVED,QP_EST#),CHAR,TEXT=AC#EST                        
DueDate  LKREQ F,21,(D,B#SAVED,QP_DUED),PDAT,TEXT=AC#DUEDT                      
IntRef   LKREQ F,22,(D,B#SAVED,QP_INRF),CHAR,TEXT=AC#INTRF                      
Chq#     LKREQ F,23,(D,B#SAVED,QP_CHQ#),CHAR,TEXT=AC#CHKC                       
ChqDate  LKREQ F,24,(D,B#SAVED,QP_CHQD),CDAT,TEXT=AC#CHKDT                      
BankAcc  LKREQ F,25,(D,B#SAVED,QP_BNKA),CHAR,TEXT=AC#BNKA                       
DiscAcc  LKREQ F,26,(D,B#SAVED,QP_DSCA),CHAR,TEXT=AC#DISAC                      
DiscRate LKREQ F,27,(D,B#SAVED,QP_DSCR),LBIN,TEXT=AC#DSCRT                      
KSVRATE  LKREQ F,28,(D,B#SAVED,QP_KSVRT),LBIN,TEXT=AC#XS210                     
KSV      LKREQ F,29,(D,B#SAVED,QP_KSV),CHAR,TEXT=(*,KSVBLIT)                    
NetGross LKREQ F,30,(D,B#SAVED,QP_NOG),CHAR,TEXT=AC#NETGR                       
                                                                                
Narr     LKREQ F,31,(D,B#SAVED,QP_NARRL),(R,VALTXT),TEXT=AC#NRTV,      +        
               LOWERCASE=Y,OLEN=L'QP_NARR+L'QP_NARRL                            
                                                                                
I#SPIT   EQU   100                                                              
TSARRec  LKREQ F,I#SPIT,(I,B#SAVED,QP_IHEAD),CHAR,                     +        
               TEXT=(*,TSARLIT),OLEN=1                                          
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Posting upload work code item                                       *         
***********************************************************************         
                                                                                
PWCREC   LKREQ H,A#PWKC,NEWREC=Y                                                
PCToken  LKREQ F,01,(D,B#SAVED,QW_TOKEN),CHAR,TEXT=(*,TOKNLIT)                  
ItemNum  LKREQ F,02,(D,B#SAVED,QW_SREF),LBIN,TEXT=AC#ITMS                       
WcSeq    LKREQ F,03,(D,B#SAVED,QW_WCSQ),LBIN,TEXT=AC#WCNO                       
WorkCode LKREQ F,04,(D,B#SAVED,QW_WC),CHAR,TEXT=AC#WC                           
Amount   LKREQ F,05,(D,B#SAVED,QW_WAMT),SPAK,TEXT=AC#AMT                        
FCurAmt  LKREQ F,06,(D,B#SAVED,QW_WFAMT),SPAK,TEXT=AC#FCAMT                     
                                                                                
Narr     LKREQ F,10,(I,B#SAVED,QW_NARTI),(R,VALTXT),TEXT=AC#NRTV,      +        
               LIST=(F,NOSORT),OLEN=L'WC_NARRL+L'WC_NARR,LOWERCASE=Y            
                                                                                
WC_NRARY DSECT ,                   ** Narrative list **                         
WC_NARRL DS    XL1                                                              
WC_NARR  DS    CL200                                                            
SVRDEF   CSECT ,                                                                
                                                                                
                                                                                
P#SPWC   EQU   100                                                              
TSARRec  LKREQ F,P#SPWC,(I,B#SAVED,QW_IHEAD),CHAR,                     +        
               TEXT=(*,TSARLIT),OLEN=1                                          
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Posting upload trailer record                                       *         
***********************************************************************         
                                                                                
TRLREC   LKREQ H,A#POTL,NEWREC=Y                                                
PCToken  LKREQ F,1,(D,B#SAVED,QT_TOKEN),CHAR,TEXT=(*,TOKNLIT)                   
                                                                                
P#STR    EQU   100                                                              
TSARRec  LKREQ F,P#STR,(I,B#SAVED,QT_ITRLR),CHAR,                      +        
               TEXT=(*,TRLRLIT),OLEN=1                                          
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* End of request map tables                                          *          
***********************************************************************         
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
***********************************************************************         
* Validate rejection comment/time or materials narrative              *         
***********************************************************************         
                                                                                
VALTXT   LM    R2,R4,LP_AINP-LP_D(R5)                                           
         STC   R3,0(R4)            Returns L'Text,Text string                   
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         MVC   1(0,R4),0(R2)                                                    
         EX    R3,0(RE)                                                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Build transactions and batches                                      *         
***********************************************************************         
                                                                                
TRNBLD   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*TRNBLD*'                                                      
                                                                                
         LAY   R0,GENAEXTN         Clear buffer                                 
         LHI   R1,I_BUFL                                                        
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR OPNBAT              Initialise batch header record               
                                                                                
         GOTOR BUFSAL,DMCB,('TSARDH',NEWBUF),0                                  
         TM    SALERR,TSEEOF       Is it the end of the buffer?                 
         JZ    TRBLD040            No                                           
         DC    H'0'                Die as we should always have a rec           
                                                                                
TRBLD020 GOTOR BUFSAL,DMCB,('TSANXT',NEWBUF),0                                  
         TM    SALERR,TSEEOF       Is it the end of the buffer?                 
         JNZ   TRBLD360            Yes - close batch item and batch             
                                                                                
* Move record to correct location                                               
TRBLD040 LAY   RE,GENAEXTN                                                      
BUF      USING IP_RECD,RE                                                       
         OC    BUF.IP_WCSQ,BUF.IP_WCSQ Work code item?                          
         JNZ   TRBLD200            Yes                                          
*                                  Else must be Posting item                    
                                                                                
* Posting item level record                                                     
TRBLD160 TM    HD_PIND,HD_PPITI    Has posting item been initialised            
         JZ    TRBLD180            No                                           
         GOTOR CLOITM              Close batch item record                      
         NI    HD_PIND,FF-HD_PBITI                                              
                                                                                
TRBLD180 LA    R0,IP_REC                                                        
         LAY   RE,GENAEXTN                                                      
         LH    R1,0(RE)            L' record                                    
         LA    R1,2(,R1)           + L' of length field                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         ZAP   PS_WAMT,PZERO                                                    
         ZAP   PS_WFAMT,PZERO                                                   
         OI    HD_PIND,HD_PPITI                                                 
                                                                                
         ZAP   POSITOT,PZERO                                                    
         GOTOR OPNITM                                                           
         J     TRBLD020                                                         
                                                                                
* Work code level record                                                        
TRBLD200 LA    R0,IW_REC                                                        
         LAY   RE,GENAEXTN                                                      
         LH    R1,0(RE)            L' record                                    
         LA    R1,2(,R1)           + L' of length field                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         ZAP   AMTWAMT,IW_WAMT     Set work code amount                         
         ZAP   AMTWFAMT,IW_WFAMT   Set work code foreign amount                 
         AP    POSITOT,IW_WAMT     For batch item total                         
         AP    POSBTOT,IW_WAMT     For batch total                              
                                                                                
         AP    PS_WAMT,IW_WAMT                                                  
         AP    PS_WFAMT,IW_WFAMT                                                
                                                                                
         GOTOR DRPOST                                                           
         GOTOR CRPOST                                                           
                                                                                
TRBLD250 TM    IP_DRST1,RSTSEADD   Have we got department postings to           
         JNZ   TRBLD260                                         make            
         TM    IP_CRST1,RSTSEADD                                                
         JZ    TRBLD020                                                         
TRBLD260 GOTOR PSTDEPT                                                          
         J     TRBLD020                                                         
*                                                                               
TRBLD360 TM    HD_PIND,HD_PBITI    Have we initialised a batch item             
         JZ    TRBLD440            No - nothing to process                      
                                                                                
         GOTOR CLOITM              Close batch item record                      
                                                                                
TRBLD440 GOTOR CLOBAT                                                           
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Validate batch. Check that batch reference has not been used today  *         
***********************************************************************         
                                                                                
VALBAT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*VALBAT*'                                                      
                                                                                
K        USING TBARECD,IOKEY                                                    
         XC    K.TBAKEY,K.TBAKEY                                                
         MVI   K.TBAKTYP,TBAKTYPQ                                               
         MVC   K.TBAKCPY,CUXCPY                                                 
         MVC   K.TBAKUSER,CUUSER                                                
         MVC   K.TBAKADDT,HD_TODC                                               
         XC    K.TBAKADDT,EFFS                                                  
         MVI   K.TBAKGRUP,TBAGGENQ                                              
         MVC   K.TBAKBTYP,QH_BTYP                                               
         MVC   K.TBAKBMOS,HD_MOA                                                
         MVC   K.TBAKBREF,QH_BREF                                               
         L     R1,=AL4(IOHID+IODIR+IO1)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   K.TBAKEY(TBAKBCHR-TBAKEY),IOKEYSAV                               
         JNE   VALBAT10                                                         
         MVC   ROUERRV,=AL2(AE$BATAE)                                           
         GOTOR SAVERR,DMCB,ROUERRV,(L'QH_BREF,QH_BREF),0,0                      
                                                                                
VALBAT10 DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Initialise batch item record                                        *         
***********************************************************************         
                                                                                
OPNBAT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*OPNBAT*'                                                      
                                                                                
         TM    HD_PIND,HD_PBINI    Have we already opened batch                 
         JNZ   EXITY               Yes nothing to do                            
         ZAP   POSBTOT,PZERO       Zero total for batch                         
         XC    POSITMS,POSITMS                                                  
         OI    HD_PIND,HD_PBINI    Set we opened batch                          
         L     R2,ATBHREC          Initialise batch header record               
         USING TBARECD,R2                                                       
OPNBAT04 XC    TBARECD(TBARFST+1-TBARECD),TBARECD                               
         LHI   RF,TBARFST+1-TBARECD                                             
         STH   RF,TBARLEN                                                       
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKCPY,CUXCPY                                                   
         MVC   TBAKUSER,CUUSER                                                  
         MVC   TBAKADDT,HD_TODC                                                 
         XC    TBAKADDT,EFFS                                                    
         MVI   TBAKGRUP,TBAGGENQ                                                
         MVC   TBAKBTYP,QH_BTYP                                                 
         MVC   TBAKBMOS,HD_MOA                                                  
         MVC   TBAKBREF,QH_BREF                                                 
         MVC   TBAKBCHR,CCTPID                                                  
         CLI   CUACCS,C'*'         Are we limit access                          
         JNE   OPNBAT05                                                         
         MVC   TBAKOFFC(1),CUACCS+1                                             
         TM    CPYSTAT4,CPYSOFF2   Is it 2 char offices                         
         JZ    OPNBAT05                                                         
         MVC   TBAKOFFC,CUACCS+2   Yes                                          
                                                                                
OPNBAT05 MVI   TBARHSTA,TBAHSUPD                                                
         MVC   TBAHRADT,HD_TODC    Date batch added                             
         MVC   TBAHREDT,HD_TODC    Effective date                               
         MVC   TBAHRUDT,HD_TODC    Date batch updated                           
                                                                                
K        USING TBARECD,IOKEY                                                    
OPNBAT06 MVC   K.TBAKEY,TBAKEY                                                  
         XC    K.TBAKTSEQ,K.TBAKTSEQ                                            
         L     R1,=AL4(IOHID+IODIR+IO1)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   K.TBAKEY(TBAKOFFC-TBAKEY),TBAKEY                                 
         JNE   OPNBAT08                                                         
         CLI   TBAKSEQN,FF         Error if all sequence's used                 
         JNE   *+6                                                              
         DC    H'0'                                                             
         IC    RE,TBAKSEQN         Increment sequence number                    
         AHI   RE,1                                                             
         STC   RE,TBAKSEQN                                                      
         J     OPNBAT06                                                         
                                                                                
N        USING BHDELD,ELEMENT                                                   
OPNBAT08 XC    N.BHDELD(BHDLNQ),N.BHDELD                                        
         MVI   N.BHDEL,BHDELQ                                                   
         MVI   N.BHDLN,BHDLN2Q                                                  
         MVC   N.BHDNAME,QH_BNAM                                                
         ZAP   N.BHDCASHC,PZERO                                                 
         ZAP   N.BHDCASHA,PZERO                                                 
         MVC   N.BHDLUID,CUTSYM                                                 
         MVC   N.BHDIBNO,CCTPID                                                 
         MVC   N.BHDAPRVR,CCTPID                                                
         MVI   N.BHDPRGNO,RCVPBRAQ =BrandOcean                                  
         GOTOR ADDELE,TBARECD                                                   
                                                                                
N        USING FFTELD,ELEMENT                                                   
         XC    ELEMENT,ELEMENT                                                  
         MVI   N.FFTEL,FFTELQ      Add comments                                 
         MVI   N.FFTTYPE,FFTTFREE                                               
         MVI   N.FFTSEQ,1                                                       
         MVI   N.FFTLN,FFTLN1Q                                                  
         GOTOR BATCOM,QH_BCOML                                                  
         CLI   N.FFTLN,FFTLN1Q                                                  
         JNH   OPNBAT12                                                         
         GOTOR ADDELE,TBARECD                                                   
                                                                                
OPNBAT12 J     EXITY                                                            
         DROP  N,K                                                              
         EJECT                                                                  
**********************************************************************          
* Build comments for batch                                           *          
**********************************************************************          
                                                                                
N        USING FFTELD,ELEMENT                                                   
BATCOM   NTR1  ,                                                                
         XR    RE,RE                                                            
         ICM   RE,1,0(R1)          RE = L(comment)                              
         JZ    EXIT                                                             
         LA    RF,1(R1)            RF = A(comment)                              
         LA    R3,0(RE,RF)                                                      
         SHI   R3,1                R3 = A(Last character)                       
BCOM02   CLI   0(R3),C' '          Remove trailing blanks                       
         JH    BCOM04                                                           
         SHI   R3,1                                                             
         JCT   RE,BCOM02                                                        
         J     EXIT                Effectively zero length                      
                                                                                
BCOM04   XR    R2,R2                                                            
         IC    R2,N.FFTLN                                                       
         LA    R3,N.FFTELD(R2)                                                  
         USING FFTDLEN,R3                                                       
         STC   RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   FFTDATA(0),0(RF)                                                 
         EX    RE,0(R1)                                                         
         LA    R2,2(RE,R2)                                                      
         CHI   R2,X'FF'                                                         
         JNH   *+6                                                              
         DC    H'0'                                                             
         STC   R2,N.FFTLN                                                       
         J     EXIT                                                             
         DROP  N,R3                                                             
         EJECT                                                                  
***********************************************************************         
* Initialise batch item record                                        *         
***********************************************************************         
                                                                                
OPNITM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*OPNITM*'                                                      
                                                                                
         TM    HD_PIND,HD_PBITI    Have we already opened batch item            
         JNZ   EXITY               Yes nothing to do                            
         ZAP   POSTOTDR,PZERO                                                   
         ZAP   POSTOTCR,PZERO                                                   
                                                                                
OPNITM02 LH    RE,POSITMS          Increment item count                         
         AHI   RE,1                                                             
         STH   RE,POSITMS                                                       
         L     R2,ATBAREC                                                       
         OI    HD_PIND,HD_PBITI    Set we opened batch item                     
                                                                                
         USING TBARECD,R2                                                       
OPNITM04 XC    TBARECD(TBARFST+1-TBARECD),TBARECD                               
         L     RF,ATBHREC                                                       
         MVC   TBAKEY,0(RF)                                                     
         MVC   TBAKTSEQ,POSITMS                                                 
         LHI   RF,TBARFST+1-TBARECD                                             
         STH   RF,TBARLEN                                                       
                                                                                
N        USING BIAELD,ELEMENT                                                   
         XC    N.BIAELD(BIALNQ),N.BIAELD                                        
         MVI   N.BIAEL,BIAELQ                                                   
         MVI   N.BIALN,BIALNQ                                                   
         MVC   N.BIAREF,IP_REF                                                  
         OC    N.BIAREF,SPACES                                                  
         GOTOR ADDELE,TBARECD                                                   
         J     EXITY                                                            
         DROP  N,R2                                                             
         EJECT                                                                  
***********************************************************************         
* Update item after posting has been added                            *         
***********************************************************************         
                                                                                
UPDITM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*UPDITM*'                                                      
                                                                                
         L     R2,ATBAREC                                                       
         USING TBARECD,R2                                                       
UPDITM02 L     R4,ATRNREC                                                       
         USING TRNRECD,R4                                                       
N        USING TRNELD,TRNRFST                                                   
                                                                                
         TM    N.TRNSTAT,TRNSDR      Update debit/credit item total             
         JZ    UPDITM04                                                         
         AP    POSTOTDR,N.TRNAMNT                                               
         J     UPDITM06                                                         
UPDITM04 AP    POSTOTCR,N.TRNAMNT                                               
                                                                                
N        USING ASKELD,ELEMENT                                                   
UPDITM06 MVI   N.ASKEL,ASKELQ                                                   
         MVI   N.ASKLN,ASKLNQ                                                   
         MVI   N.ASKSEQN,0                                                      
         MVC   N.ASKKEY,TRNKEY                                                  
                                                                                
         GOTOR ADDELE,TBARECD                                                   
                                                                                
         J     EXITY                                                            
         DROP  N,R2,R4                                                          
         EJECT                                                                  
***********************************************************************         
* Close item after last posting has been added                        *         
***********************************************************************         
                                                                                
CLOITM   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CLOITM*'                                                      
         CP    POSTOTDR,POSTOTCR   Item must balance                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ATBAREC                                                       
         USING TBARECD,R2                                                       
                                                                                
N        USING BIAELD,TBARFST                                                   
         ZAP   N.BIAAMT,POSITOT                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO1'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  N                                                                
         EJECT                                                                  
***********************************************************************         
* Close batch                                                         *         
***********************************************************************         
         SPACE 1                                                                
                                                                                
CLOBAT   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CLOBAT*'                                                      
         L     R2,ATBHREC                                                       
         USING TBARECD,R2                                                       
N        USING BHDELD,TBARFST                                                   
                                                                                
         OI    TBARHSTA,TBAHSUPD                                                
         MVC   N.BHDITEMC,POSITMS    Items control total                        
         MVC   N.BHDITEMA,POSITMS    Items input                                
         ZAP   N.BHDCASHC,POSBTOT    Cash control total                         
         ZAP   N.BHDCASHA,POSBTOT    Cash total                                 
         ZAP   N.BHDTOTDR,POSTOTDR   Debit total                                
         ZAP   N.BHDTOTCR,POSTOTCR   Credit total                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOMST+IO3'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  N                                                                
         EJECT                                                                  
***********************************************************************         
* Post Debit posting                                                  *         
***********************************************************************         
                                                                                
DRPOST   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*DRPOST*'                                                      
                                                                                
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,IP_DEB                                                   
         MVC   TRNKWORK,SPACES                                                  
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,IP_CRD                                                   
         CLC   IP_DEB(2),LEDGERSR  Test posting to SR                           
         JNE   DRPOST02                                                         
         MVC   TRNKCULC,SPACES                                                  
         MVC   TRNKCACT,IP_CRD                                                  
                                                                                
DRPOST02 MVC   TRNKDATE,IP_DATE                                                 
         MVC   TRNKREF,IP_REF                                                   
         OC    TRNKREF,SPACES                                                   
         MVI   TRNKSBR,0                                                        
                                                                                
         MVC   POSTULA,TRNKULA     Posting account                              
         MVI   POSTYPE,POSTDR      Debit                                        
         ZAP   POSAMNT,AMTWAMT                                                  
         MVC   POSBT,IP_BTYP       Posting batch type                           
                                                                                
* Build attribute table here                                                    
         USING APXLISD,RE                                                       
         LA    RE,APELIST                                                       
         MVI   APXSTAT,0                                                        
         CLI   POSTYPE,POSTDR                                                   
         JNE   *+8                                                              
         OI    APXSTAT,APENSDR                                                  
         MVC   APXACT,IP_DEB                                                    
         AHI   RE,APXLNQ                                                        
         MVI   APXSTAT,0                                                        
         MVC   APXACT,IP_CRD                                                    
         AHI   RE,APXLNQ                                                        
         CLC   LEDGERSE,IP_DEB     Test posting to SE                           
         JNE   DRPOST10                                                         
         TM    IP_DRST1,RSTSEADD   Test 28/2D postings required                 
         JNZ   *+12                                                             
         TM    IP_CRST1,RSTSEADD                                                
         JZ    DRPOST10                                                         
         MVI   APXSTAT,0                                                        
         MVC   APXACT,IP_28                                                     
         AHI   RE,APXLNQ                                                        
         MVI   APXSTAT,APENSDR                                                  
         MVC   APXACT,IP_2D                                                     
         AHI   RE,APXLNQ                                                        
DRPOST10 MVI   0(RE),X'FF'                                                      
         DROP  RE                                                               
                                                                                
* Build posting elements                                                        
         GOTOR EL_TRN              TRNELD                                       
         GOTOR EL_APE              APEELD                                       
         GOTOR EL_PID              PIDELD                                       
         GOTOR EL_NARR                                                          
                                                                                
         GOTOR GOATRN,IP_CRDN      Put transaction record                       
         GOTOR UPDITM                                                           
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Post Credit posting                                                 *         
***********************************************************************         
                                                                                
CRPOST   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*CRPOST*'                                                      
                                                                                
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,IP_CRD                                                   
         MVC   TRNKWORK,SPACES                                                  
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,IP_DEB                                                   
         CLC   IP_DEB(2),LEDGERSR  Test posting to SR                           
         JNE   CRPOST02                                                         
         MVC   TRNKCULC,SPACES                                                  
         MVC   TRNKCACT,IP_DEB                                                  
                                                                                
CRPOST02 MVC   TRNKDATE,IP_DATE                                                 
         MVC   TRNKREF,IP_REF                                                   
         OC    TRNKREF,SPACES                                                   
         MVI   TRNKSBR,0                                                        
                                                                                
         MVC   POSTULA,TRNKULA     Posting account                              
         MVI   POSTYPE,POSTCR      Credit                                       
         ZAP   POSAMNT,AMTWAMT                                                  
         MVC   POSBT,IP_BTYP       Posting batch type                           
                                                                                
         GOTOR EL_TRN              TRNELD                                       
         GOTOR EL_APE              APEELD                                       
         GOTOR EL_PID              PIDELD                                       
         GOTOR EL_NARR                                                          
                                                                                
         GOTOR GOATRN,IP_DEBN      Put transaction record                       
         GOTOR UPDITM                                                           
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Post department postings (2D and 28 postings)                       *         
***********************************************************************         
                                                                                
PSTDEPT  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*PSTDEPT'                                                      
                                                                                
         USING TRNRECD,R2                                                       
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,IP_2D                                                    
         MVC   TRNKWORK,SPACES                                                  
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,IP_28                                                    
         MVC   TRNKDATE,IP_DATE                                                 
         MVC   TRNKREF,IP_REF                                                   
         OC    TRNKREF,SPACES                                                   
         MVI   TRNKSBR,0                                                        
                                                                                
         ZAP   POSAMNT,AMTWAMT                                                  
         MVI   POSTYPE,POSTDR      Debit                                        
         MVC   POSBT,IP_BTYP       Posting batch type                           
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_NARR                                                          
                                                                                
         GOTOR GOATRN,IP_28N       Put transaction record                       
         GOTOR UPDITM                                                           
                                                                                
         USING TRNRECD,R2                                                       
         L     R2,ATRNREC                                                       
         XC    TRNRECD(TRNRFST-TRNRECD),TRNRECD                                 
         MVC   TRNKCPY,CUXCPY                                                   
         MVC   TRNKULA,IP_28                                                    
         MVC   TRNKWORK,SPACES                                                  
         MVC   TRNKCCPY,CUXCPY                                                  
         MVC   TRNKULC,IP_2D                                                    
         MVC   TRNKDATE,IP_DATE                                                 
         MVC   TRNKREF,IP_REF                                                   
         OC    TRNKREF,SPACES                                                   
         MVI   TRNKSBR,0                                                        
                                                                                
         ZAP   POSAMNT,AMTWAMT                                                  
         MVI   POSTYPE,POSTCR      Credit                                       
         MVC   POSBT,IP_BTYP       Posting batch type                           
                                                                                
         GOTOR EL_TRN                                                           
         GOTOR EL_APE                                                           
         GOTOR EL_PID                                                           
         GOTOR EL_NARR                                                          
                                                                                
         GOTOR GOATRN,IP_2DN       Put transaction record                       
         GOTOR UPDITM                                                           
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* X'44' Transaction element                                           *         
***********************************************************************         
                                                                                
EL_TRN   NTR1  LABEL=NO                                                         
                                                                                
N        USING TRNELD,ELEMENT                                                   
         XC    N.TRNELD(TRNLN1Q),N.TRNELD                                       
         MVI   N.TRNEL,TRNELQ                                                   
         MVC   N.TRNDATE,IP_DATE                                                
         MVC   N.TRNREF,IP_REF                                                  
         OC    N.TRNREF,SPACES                                                  
         MVI   N.TRNSUB,0                                                       
         MVC   N.TRNTYPE,POSBT     Batch type                                   
         TM    POSTYPE,POSTDR                                                   
         JZ    *+8                                                              
         OI    N.TRNSTAT,TRNSDR    Debit posting for SE/SJ/SG/2D/2P/1P          
         MVC   N.TRNBTCH,SPACES                                                 
         MVC   N.TRNMOS(1),IP_TREF+1   Second digit of the YEAR                 
         MVC   N.TRNMOS+1(1),IP_TREF+3 Second digit of the MONTH                
         CLI   IP_TREF+2,C'1'          TEST MONTH = 10-12                       
         JNE   EL_TRN10                                                         
         NI    N.TRNMOS+1,X'C3'                                                 
         XR    R1,R1                                                            
         IC    R1,N.TRNMOS+1                                                    
         AHI   R1,1                (10=A, 11=B, 12=C)                           
         STC   R1,N.TRNMOS+1                                                    
                                                                                
EL_TRN10 MVC   N.TRNBREF,QH_BREF                                                
         ZAP   N.TRNAMNT,POSAMNT   Amount                                       
         MVC   N.TRNOFFC,IP_OFF    Office                                       
         OC    N.TRNOFFC,SPACES                                                 
                                                                                
         CLC   PRODUL,TRNKULA                                                   
         JNE   *+10                                                             
         MVC   N.TRNOFFC,IW_WC     SJ posting uses work code                    
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    EL_TRN12                                                         
         CLC   N.TRNOFFC,SPACES                                                 
         JH    EL_TRN12                                                         
         DC    H'0'                                                             
                                                                                
EL_TRN12 SR    R1,R1               See if we have finance narrative             
         CLI   IW_NARRL,0          Any narrative at workcode level              
         JE    EL_TRN14            No - use posting level narrative             
         J     EL_TRN16                                                         
EL_TRN14 ICM   R1,1,IP_NARRL                                                    
         JZ    EL_TRN16            No - see if we have original narr            
         BCTR  R1,0                Yes - add to trans element                   
         BASR  RE,0                                                             
         MVC   N.TRNNARR(0),IP_NARR                                             
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
                                                                                
EL_TRN16 LA    RF,TRNLN1Q(R1)                                                   
         STC   RF,N.TRNLN                                                       
                                                                                
         GOTOR ADDELE,TRNRECD                                                   
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* X'C0' Subsidiary cash element net amount                            *         
***********************************************************************         
                                                                                
EL_APE   NTR1  LABEL=NO                                                         
                                                                                
N        USING APEELD,ELEMENT                                                   
                                                                                
         USING APENTRY,R1                                                       
         LA    R1,N.APENTRY                                                     
         USING APXLISD,RE                                                       
         LA    RE,APELIST                                                       
         XR    R0,R0                                                            
                                                                                
         MVI   N.APEEL,APEELQ                                                   
EL_AP00  CLI   0(RE),X'FF'                                                      
         JE    EL_APEX                                                          
         CLC   APXACT,POSTULA    Exclude posting acc                            
         JE    EL_AP06                                                          
         MVC   APENSTAT,APXSTAT                                                 
         LA    RF,APXACT+L'APXACT-1                                             
         LA    R5,L'APXACT                                                      
EL_AP02  CLI   0(RF),C' '                                                       
         JH    EL_AP04                                                          
         SHI   RF,1                                                             
         JCT   R5,EL_AP02                                                       
         DC    H'0'                                                             
EL_AP04  AHI   R5,APELN2Q                                                       
         STC   R5,APENLEN                                                       
         AHI   R0,1                                                             
         MVC   APENACT,APXACT                                                   
         AR    R1,R5                                                            
EL_AP06  AHI   RE,APXLNQ                                                        
         J     EL_AP00                                                          
                                                                                
EL_APEX  LTR   R0,R0                                                            
         JNZ   *+12                                                             
         MVI   N.APEEL,APEELQ                                                   
         J     EXITY                                                            
         STC   R0,N.APENUM                                                      
         LA    RE,ELEMENT                                                       
         SR    R1,RE                                                            
         STC   R1,N.APELN                                                       
         GOTOR ADDELE,TRNRECD                                                   
         J     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
* X'DB' Add Extra long narrative                                   *            
**********************************************************************          
                                                                                
EL_NARR  NTR1  LABEL=NO                                                         
         LA    R6,IW_NARRL                                                      
         LHI   R3,1                                                             
NAR      USING IW_NARRL,R6                                                      
EL_NARR2 CLI   NAR.IW_NARRL,0      Any narrative at workcode level              
         JE    EXITY                                                            
N        USING FFTELD,ELEMENT                                                   
         XC    N.FFTELD(L'ELEMENT),N.FFTELD                                     
         MVI   N.FFTEL,FFTELQ                                                   
         MVI   N.FFTTYPE,FFTTNARR                                               
         MVC   N.FFTITSEQ,IW_SREF                                               
         MVC   N.FFTWCSEQ,IW_WCSQ                                               
         MVC   N.FFTWCODE,IW_WC                                                 
         STC   R3,N.FFTWCTSQ                                                    
         LLC   RF,NAR.IW_NARRL                                                  
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   N.FFTWCNAR(0),NAR.IW_NARR                                        
         EX    RF,0(RE)                                                         
         AHI   RF,1+FFTWCLN                                                     
         STC   RF,N.FFTLN                                                       
         GOTOR ADDELE,TRNRECD                                                   
         LA    R6,L'IW_NARRL+L'IW_NARR(R6)                                      
         AHI   R3,1                                                             
         J     EL_NARR2                                                         
         DROP  NAR,N                                                            
***********************************************************************         
* X'D8' Person ID element                                             *         
***********************************************************************         
                                                                                
EL_PID   NTR1  LABEL=NO                                                         
         OC    CCTPID,CCTPID                                                    
         JZ    EXITY                                                            
N        USING PIDELD,ELEMENT                                                   
         XC    N.PIDEL(PIDLNQ),N.PIDEL                                          
         MVI   N.PIDEL,PIDELQ                                                   
         MVI   N.PIDLN,PIDLNQ                                                   
         MVC   N.PIDNO,CCTPID       User's PID #                                
         GOTOR ADDELE,TRNRECD                                                   
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Add an element to end of record pointed to by ATRNREC               *         
***********************************************************************         
                                                                                
ADDELE   NTR1  LABEL=NO                                                         
         LR    RE,R1               RE=A(Record)                                 
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
* Call DMGRITRN to put ADDTRN transaction record to output file       *         
***********************************************************************         
                                                                                
GOATRN   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*GOATRN*'                                                      
                                                                                
         LTR   R2,R1               Test last time call                          
         JZ    GOATRN04                                                         
         TM    SR_PIND,SR_PATRF    Test first time call                         
         JNZ   GOATRN02                                                         
         OI    SR_PIND,SR_PATRF    Set not first time/Put header                
         GOTOR DMGRITRN,DMCB,('FW_AAHDR',$ADDTRN)                               
                                                                                
GOATRN02 GOTOR DMGRITRN,DMCB,('FW_AATRN',$ADDTRN),(R2)                          
         J     EXITY                                                            
                                                                                
GOATRN04 TM    SR_PIND,SR_PATRF    Test any transactions put                    
         JZ    EXITY                                                            
         GOTOR DMGRITRN,DMCB,('FW_AAEND',$ADDTRN)                               
         J     EXITY                                                            
         EJECT                                                                  
         DROP  RA                  Drop GLOBALS                                 
***********************************************************************         
* Data Manager routines                                               *         
*                                                                     *         
* DMGRITRN will be entered for any call to VDATAMGR (internal calls)  *         
* DMGRXTRN will be entered for any call to CDATAMGR (external calls)  *         
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
         JNE   *+8                                                              
         MVI   FW_RTYP,ACRTNBT                                                  
         MVI   FW_ACT,FW_APUTR     Set action to PUTREC                         
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
         USING TRNRECD,R2                                                       
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         XC    SUBREF,SUBREF                                                    
         XC    SAVKREF,SAVKREF                                                  
                                                                                
ADDTRN04 MVC   OB_KEY(L'ACTKEY),0(R2)                                           
         GOTOR GETBUF,OB_D         See if key of trans already exists           
         JL    ADDTRN06            No - add to buffer                           
         LH    RF,SUBREF                                                        
         AHI   RF,1                                                             
         STH   RF,SUBREF                                                        
         STC   RF,TRNKSBR                                                       
         STC   RF,TRNSUB                                                        
         SRA   RF,8                Test overflow                                
         JZ    ADDTRN04                                                         
         CHI   RF,100              Allow 100*256 (in twos)                      
         JNH   *+6                                                              
         DC    H'0'                                                             
         OC    SAVKREF,SAVKREF     Test first key change                        
         JNZ   *+10                                                             
         MVC   SAVKREF,TRNKREF     yes - save original key value                
         BCTR  RF,0                amend reference number                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TRNKREF+4(2),DUB                                                 
         MVI   TRNKREF+3,C'*'                                                   
         MVC   TRNREF,TRNKREF                                                   
         J     ADDTRN04                                                         
         DROP  R3                                                               
                                                                                
ADDTRN06 GOTOR ADDBUF,OB_D                                                      
         XC    OB_D(OB_LNQ),OB_D                                                
         MVC   DMGRKEY,0(R2)       Save transaction record key                  
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN                                                     
         JNZ   *+6                                                              
         DC    H'0'                Record length can't be zero                  
         CHI   RF,MAXRECLN                                                      
         JNH   *+6                                                              
         DC    H'0'                Nor greater than the maximum                 
         USING FW_D,R2                                                          
         AHI   RF,FW_ATOLQ         Add on overhead length                       
         SHI   R2,FW_ATOLQ         Back-up by overhead length                   
         MVC   DMGRSAVE(FW_ATHLQ),0(R2)                                         
         XC    FW_D(FW_ATHLQ),FW_D                                              
         STCM  RF,3,FW_RLEN        Set record length                            
         MVI   FW_FILE,FW_FTRN$    Set file to 'ADDTRN'                         
         MVI   FW_ACT,FW_AATRN     Set action                                   
         MVC   FW_RKEY,DMGRKEY     Set transaction key                          
         MVC   FW_ATMOA,HD_MOA     Set month of activity                        
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
         DROP  R2,RC                                                            
                                                                                
DMWORKD  DSECT                     ** DMGRXTRN s/r local w/s **                 
DMGRSVR1 DS    A                   A(Caller's parameter list)                   
                                                                                
DMGRDMCB DS    0XL(DMGRPL)         ** Caller's parameter list **                
DMGRACTN DS    0X                  Action code (special records)                
DMGRP1   DS    A                   A(Action)                                    
DMGRP2   DS    A                   A(File)                                      
DMGRERRS DS    0X                  Error return BYTE1                           
DMGRP3   DS    A                   A(Disk address)                              
DMGRP4   DS    A                   A(I/O area)                                  
DMGRP5   DS    A                   A(DMWORK area)                               
DMGRP6   DS    A                   N/D                                          
DMGRPL   EQU   *-DMGRP1                                                         
                                                                                
DMGRFLAG DS    X                   ** DMGRXTRN flag BYTE1 **                    
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
                                                                                
         TM    SR_PIND,SR_PPUTF    Test first time call                         
         JNZ   PUTOUT06                                                         
         OI    SR_PIND,SR_PPUTF    Set not first time call                      
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
         MVC   H.FW_CPYSB,CPYSTATB                                              
         MVC   H.FW_CPYSC,CPYSTATC                                              
         MVC   H.FW_CPYGL,CPYGLMOA GL MOA                                       
         MVC   H.FW_PERLV,ONERL1L  Set 1R ledger levels                         
         MVI   H.FW_BOVLY,FW_BSALP Set Brand Ocean salary upload call           
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
ATBAREC  EQU   AIO1,,C'A'          A(Batch item record)                         
ATBHREC  EQU   AIO3,,C'A'          A(Batch header record)                       
                                                                                
MAXRECLN EQU   IOLENQ-(L'IODA+L'IOWORK)                                         
                                                                                
NEWBUF   EQU   1                   New time buffer                              
                                                                                
D#TOKEN  EQU   1                   PC token map number                          
D#ERR    EQU   2                   Error msg                                    
                                                                                
TOKNLIT  DC    C'PC Token'                                                      
KSVBLIT  DC    C'KSV billable/non billable'                                     
TSARLIT  DC    C'*** TSAR Record ***'                                           
HEADLIT  DC    C'*** Header Values **'                                          
TRLRLIT  DC    C'*** Trailer Values **'                                         
                                                                                
FAILS    DC    C'SMANDDLO@dds.co.uk:'                                           
SLOWS    DC    C'SMANDDLO@dds.co.uk:'                                           
                                                                                
DEBITQ   EQU   X'01'                                                            
CREDITQ  EQU   X'02'                                                            
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
                                                                                
SECPROG  DC    X'06F1'             Acc/Upload                                   
                                                                                
PZERO    DC    P'0'                                                             
EFFS     DC    X'FFFFFFFF'                                                      
                                                                                
UNIT#S   EQU   C'S'                                                             
LEDGERSA DC    C'SA'                                                            
LEDGERSE DC    C'SE'                                                            
LEDGERSG DC    C'SG'                                                            
LEDGERSI DC    C'SI'                                                            
LEDGERSQ DC    C'SQ'                                                            
LEDGERSR DC    C'SR'                                                            
LEDGERSV DC    C'SV'                                                            
LEDGERSX DC    C'SX'                                                            
LEDGER2D DC    C'2D'                                                            
LEDGER2P DC    C'2P'                                                            
LEDGER28 DC    C'28'                                                            
                                                                                
FWHSTAMP DC    X'00',C'UO'         FACWRK header record stamp                   
                                                                                
RETDATEL DC    AL1(LQ_RDATQ),AL2(LQ_LN1Q)                                       
                                                                                
ACCOUNT  DC    C'ACC'              Account file prefix                          
                                                                                
DMWRTD   DC    C'DMW'              DMWRITE (directory)                          
DMADDD   DC    C'DMA'              DMADD   (directory)                          
DMPUTF   DC    C'PUT'              PUTREC  (file)                               
DMADDF   DC    C'ADD'              ADDREC  (file)                               
                                                                                
DMCOMMIT DC    C'COMMIT  '         Recovery checkpoint command                  
                                                                                
$ADDTRN  DC    C'$ATR'             ADDTRN file prefix                           
                                                                                
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
                                                                                
RECTAB   DS    0XL(RECTABL)        ** Record table **                           
         DC    AL2(A#POHD),AL1(RECTHDRQ)   75 Header                            
         DC    AL2(A#PITM),AL1(RECTPITQ)   76 Posting Item                      
         DC    AL2(A#PWKC),AL1(RECTPWCQ)   77 Posting Work Code                 
         DC    AL2(A#POTL),AL1(RECTTRLQ)   78 Trailer                           
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
PAIRTAB  DS    0XL(PAITABL)                                                     
         DC    C'SG',C'SG'                                                      
         DC    C'SE',C'SE'                                                      
         DC    C'SI',C'SI'                                                      
         DC    C'SX',C'SX'                                                      
         DC    C'SV',C'SV'                                                      
         DC    C'SX',C'SV'                                                      
         DC    C'SV',C'SX'                                                      
PAITABN  EQU   (*-PAIRTAB)/L'PAIRTAB                                            
                                                                                
OFFPTAB  DS    0X                                                               
         DC    C'SA',C'SE',C'SG',C'SI',C'SQ',C'SR',C'SV',C'SX'                  
         DC    X'00'                                                            
                                                                                
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTABL  EQU   *-RECTABD                                                        
                                                                                
RECTHDRQ EQU   1                   Posting header                               
RECTPITQ EQU   2                   Posting item                                 
RECTPWCQ EQU   3                   Posting work code                            
RECTTRLQ EQU   4                   Posting trailer                              
                                                                                
PAITABD  DSECT                                                                  
PAITFRM  DS    CL2                                                              
PAITTO   DS    CL2                                                              
PAITABL  EQU   *-PAITABD                                                        
                                                                                
         EJECT                                                                  
SVRDEF   CSECT                                                                  
                                                                                
LLP      DC    A(0)                A(LP_D)                                      
         EJECT                                                                  
                                                                                
DCDICTL  DS    0X                                                               
         DCDDL AC#DATE,L'AC8DATE                                                
         DCDDL AC#REF,L'AC8REF                                                  
DCDICTLX DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* Saved working storage                                               *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
                                                                                
***********************************************************************         
* Sacred values                                                       *         
***********************************************************************         
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER mode                           
                                                                                
ASRUPD60 DS    A                   A(SRUPD60)                                   
PROTOFF  DS    A                   A(PROTOFF)                                   
PROTON   DS    A                   A(PROTON)                                    
IDATAMGR DS    A                   A(DMGRITRN)                                  
XDATAMGR DS    A                   A(DMGRXTRN)                                  
MASTC    DS    A                   A(MASTC)                                     
DATAMGR  DS    A                   A(DATAMGR)                                   
WRKBLKR  DS    A                   A(FACWRK WRKIO block)                        
ARUNFACS DS    A                   A(RUNFACS) - offline, for trace rtn          
APRINTER DS    A                   A(PRINTER) - offline, for trace rtn          
                                                                                
PL16     DS    PL16                                                             
                                                                                
OFFD     DS    0C                                                               
OFFPOSA  DS    C                   OFFICE POSITION LEDGER SA                    
OFFPOSE  DS    C                   OFFICE POSITION LEDGER SE                    
OFFPOSG  DS    C                   OFFICE POSITION LEDGER SG                    
OFFPOSI  DS    C                   OFFICE POSITION LEDGER SI                    
OFFPOSQ  DS    C                   OFFICE POSITION LEDGER SQ                    
OFFPOSR  DS    C                   OFFICE POSITION LEDGER SR                    
OFFPOSV  DS    C                   OFFICE POSITION LEDGER SV                    
OFFPOSX  DS    C                   OFFICE POSITION LEDGER SX                    
OFFDLN   EQU   *-OFFD                                                           
                                                                                
OFFSW    DS    C                   COMPANY IS ON OFFICES YES/NO                 
OFFTWO   EQU   C'T'                TWO CHAR OFFICES                             
                                                                                
SUBREF   DS    H                   Sub reference counter                        
SAVKREF  DS    CL(L'TRNREF)        Transaction reference                        
                                                                                
IND      DS    XL1                                                              
HEADREC  EQU   X'80'                                                            
POSTITEM EQU   X'40'                                                            
WCITEM   EQU   X'20'                                                            
NEWPOST  EQU   X'10'                                                            
                                                                                
TSARPASS DS    XL(TSPXTNL)         TSAR block for passive pointers              
TSARRECS DS    XL(TSPXTNL)         TSAR block for record buffer                 
TSAROBUF DS    XL(TSPXTNL)         TSAR block for optimisation buffer           
TSAROLDI DS    XL(TSPXTNL)         TSAR block for old salary buffer             
TSARNEWI DS    XL(TSPXTNL)         TSAR block for new salary buffer             
TSARGENL DS    XL(TSPXTNL)         TSAR block for posting/audit buffer          
                                                                                
COMFACS  DS    XL(COMFACSL)        Local copy of COMFACS                        
                                                                                
MYBYTE1  DS    XL1                                                              
                                                                                
DSDICTL  DS    0X                                                               
AC8DATE  DS    CL8                                                              
AC8REF   DS    CL8                                                              
                                                                                
SAVEVAR  DS    0F                  ** Variables follow **                       
MYHALF1  DS    H                                                                
                                                                                
***********************************************************************         
* Saved SRUPD60 values                                                *         
***********************************************************************         
                                                                                
SR_PIND  DS    XL1                                                              
SR_PPUTF EQU   X'40'               PUTOUT initialised                           
SR_PATRF EQU   X'20'               GOATRN first time flag                       
                                                                                
***********************************************************************         
* Request values                                                      *         
***********************************************************************         
                                                                                
* # 0075  Header.                                                               
                                                                                
QH_VALS  DS    0X                                                               
QH_BTYP  DS    XL1                 Batch type                                   
QH_BREF  DS    CL4                 Batch reference                              
QH_MOA   DS    XL2                 Month of activity (YYMM)                     
QH_BNAM  DS    CL15                Batch name                                   
QH_BCOML DS    XL1                 Batch comments length                        
QH_BCOM  DS    CL50                Batch comments                               
QH_DLNQ  EQU   *-QH_VALS                                                        
QH_IHEAD DS    XL1                                                              
QH_AHEAD DS    AL3                 A(Passed header values)                      
QH_TOKEN DS    CL8                 PC Token (echoed in error response)          
                                                                                
                                                                                
* # 0076  Posting Item.                                                         
                                                                                
QP_VALS  DS    0X                                                               
QP_IHEAD DS    XL1                                                              
QP_AHEAD DS    AL3                 A(Passed item posting values)                
QP_SREF  DS    XL2                 Sub-reference                                
QP_DEBAC DS    CL14                Debit account                                
QP_CRDAC DS    CL14                Credit account                               
QP_REFL  DS    XL1                 Length of reference                          
QP_REF   DS    CL20                Reference                                    
QP_DATE  DS    XL2                 Posting date                                 
QP_OFF   DS    CL2                 Office                                       
QP_VAT   DS    CL1                 VAT code                                     
QP_VATA  DS    CL12                VAT account code                             
QP_ORD#  DS    CL6                 Order number                                 
QP_ORDTE DS    PL3                 Order date                                   
QP_FCURC DS    CL3                 Foreign currency code                        
QP_XRAT  DS    XL7                 Exchange rate                                
QP_CLI   DS    CL6                 Client code                                  
QP_PRO   DS    CL6                 Product code                                 
QP_JOB   DS    CL6                 Job code                                     
QP_DEPT  DS    CL12                Department account 2D                        
QP_STAFF DS    CL12                Personnel account  2P                        
QP_MILE  DS    PL6                 Miles                                        
QP_EST#  DS    CL6                 Estimate number                              
QP_DUED  DS    PL3                 Due date                                     
QP_INRF  DS    CL7                 Internal reference                           
QP_CHQ#  DS    CL6                 Cheque number                                
QP_CHQD  DS    XL2                 Cheque date                                  
QP_BNKA  DS    CL14                Bank account                                 
QP_DSCA  DS    CL14                Discount account                             
QP_DSCR  DS    XL2                 Discount rate                                
QP_KSVRT DS    XL2                 KSV rate                                     
QP_KSV   DS    CL1                 KSV billable or non-billable                 
QP_NOG   DS    CL1                 Net or gross (N/G)                           
*                                                                               
QP_NARRL DS    XL1                                                              
QP_NARR  DS    CL200                                                            
*                                                                               
QP_TOKEN DS    CL8                 PC Token (echoed in error response)          
QP_DLNQ  EQU   *-QP_VALS                                                        
                                                                                
                                                                                
* # 0077  Posting work code.                                                    
                                                                                
QW_VALS  DS    0X                                                               
QW_IHEAD DS    XL1                                                              
QW_AHEAD DS    AL3                 A(Passed work code posting values)           
QW_SREF  DS    XL2                 Sub-reference                                
QW_WCSQ  DS    XL2                 Work code sequence number                    
QW_WC    DS    CL2                 Work code                                    
QW_WAMT  DS    PL6                 Work code amount                             
QW_WFAMT DS    PL6                 Work code foreign amount                     
*                                                                               
QW_NARTI DS    XL1                 Narrative array indicator                    
QW_ANARR DS    AL3                 A(Narrative array)                           
*                                                                               
QW_TOKEN DS    CL8                 PC Token (echoed in error response)          
QW_LNQ   EQU   *-QW_VALS                                                        
                                                                                
* # 0078 Trailer record                                                         
                                                                                
QT_VALS  DS    0X                                                               
QT_TOKEN DS    CL8                 PC Token (echoed in error response)          
QT_ITRLR DS    XL1                 A(Passed trailer values)                     
QT_ATRLR DS    AL3                                                              
                                                                                
**********************************************************************          
* Derived header values (passed to update phase)                                
**********************************************************************          
                                                                                
HD_VALS  DS    0F                                                               
HD_PIND  DS    XL1                 Header indicator                             
HD_PBINI EQU   X'80'               Batch initialisation                         
HD_PBITI EQU   X'08'               Batch item initialisation                    
HD_PPITI EQU   X'04'               Posting item initialisation                  
HD_MOA   DS    XL3                 Month of activity                            
HD_TODP  DS    XL3                 Today's date packed                          
HD_TODC  DS    XL2                 Today's date compressed                      
HD_TODF  DS    CL6                 Today's date character                       
                                                                                
HD_VALSL EQU   *-HD_VALS                                                        
                                                                                
***********************************************************************         
* Derived trailer values (passed to update phase)                     *         
***********************************************************************         
                                                                                
TR_VALS  DS    0F                                                               
TR_VALSL EQU   *-TR_VALS                                                        
                                                                                
***********************************************************************         
* Saved item values                                                   *         
***********************************************************************         
                                                                                
IS_VALS  DS    0F                                                               
IS_DATE  DS    PL3                 Date                                         
IS_VALSL EQU   *-IS_VALS                                                        
                                                                                
***********************************************************************         
* Derived posting values                                              *         
***********************************************************************         
                                                                                
PS_VALS  DS    0F                                                               
*                                                                               
PS_WAMT  DS    PL6                 Work code amount                             
PS_WFAMT DS    PL6                 Foreign work code amount                     
PS_VALSL EQU   *-PS_VALS                                                        
                                                                                
***********************************************************************         
* General working storage values                                      *         
***********************************************************************         
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      RECORD TYPE                                  
                                                                                
SALERR   DS    XL(L'TSERRS)        Salary buffer error return                   
                                                                                
ANXTERR  DS    A                   A(Next error entry)                          
ALSTERR  DS    A                   A(Previous error entry in errtab)            
                                                                                
DMGRSEQ# DS    XL(L'FW_SEQ#)       DATAMGR record sequence number               
                                                                                
POSTULA  DS    0CL14               Posting account                              
POSTUL   DS    CL2                 Unit and ledger                              
POSTACT  DS    CL12                Account                                      
POSAMNT  DS    XL(L'TRNAMNT)       Transaction amount                           
POSTYPE  DS    XL1                 Posting type                                 
POSTDR   EQU   X'80'               Debit                                        
POSTCR   EQU   X'40'               Credit                                       
         DS    0H                                                               
POSITMS  DS    XL(L'TBAKTSEQ)      Number items in batch                        
POSITOT  DS    PL6                 Batch item total                             
POSBTOT  DS    PL6                 Batch total                                  
POSTOTCR DS    PL6                 Batch credits                                
POSTOTDR DS    PL6                 Batch debits                                 
POSBT    DS    XL1                 Batch type                                   
                                                                                
AMTWAMT  DS    PL6                 Work code amount                             
AMTWFAMT DS    PL6                 Work code foreign amount                     
                                                                                
APELIST  DS    XL90                Attribute list                               
                                                                                
SAVEVARL EQU   *-SAVEVAR                                                        
                                                                                
ERRTAB   DS    XL500               Saved error messages and data                
* ERRTAB is currently limited by the record size you've asked DDLINKIO          
* to use (LIOBRECL or 1K default).                                              
                                                                                
* The following is storage for validated records. These are based               
* on R7.                                                                        
                                                                                
I_CUR    DS    0H                                                               
I_CURPOS DS    (IP_LENQ)X                                                       
I_CURWCN DS    (IW_LENQ)X                                                       
                                                                                
SAVEL    EQU   *-SAVED                                                          
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
         EJECT                                                                  
***********************************************************************         
* Derived/validated Posting item values (passed to update phase)      *         
***********************************************************************         
                                                                                
IP_RECD  DSECT ,                                                                
IP_REC   DS    0X                                                               
IP_RLEN  DS    XL2                 Record length                                
                                                                                
IP_KEY   DS    0X                                                               
IP_DEB   DS    CL14                Debit account                                
IP_CRD   DS    CL14                Credit account                               
IP_REF   DS    CL20                Reference                                    
IP_DATE  DS    PL3                 Date                                         
IP_SREF  DS    XL2                 Sub-reference                                
IP_WCSQ  DS    XL2                 Work code sequence number (zero)             
IP_KEYL  EQU   *-IP_KEY                                                         
                                                                                
IP_DEBN  DS    CL36                Debit account name                           
IP_DRST1 DS    XL(L'RSTSTAT1)      RSTSTAT1 from debit account                  
IP_CRDN  DS    CL36                Credit account name                          
IP_CRST1 DS    XL(L'RSTSTAT1)      RSTSTAT1 from credit account                 
IP_REFL  DS    XL1                 Actual length of IP_REF                      
                                                                                
IP_BTYP  DS    XL1                 Batch type                                   
IP_OFF   DS    CL2                 Office                                       
                                                                                
IP_28    DS    CL14                                                             
IP_2D    DS    CL14                                                             
IP_28N   DS    CL36                                                             
IP_2DN   DS    CL36                                                             
IP_TREF  DS    CL(L'TRNKREF)                                                    
                                                                                
IP_SWAP  DS    XL1                                                              
IP_SYES  EQU   X'80'                                                            
                                                                                
IP_NARRL DS    XL1                                                              
IP_NARR  DS    CL200                                                            
IP_RCLQ  EQU   *-IP_KEY                                                         
IP_LENQ  EQU   *-IP_REC                                                         
                                                                                
***********************************************************************         
* Derived/validated Work Code values (passed to update phase)         *         
***********************************************************************         
                                                                                
IW_RECD  DSECT ,                                                                
IW_REC   DS    0X                                                               
IW_RLEN  DS    XL2                                                              
                                                                                
IW_KEY   DS    0X                                                               
IW_DEB   DS    CL14                Debit account                                
IW_CRD   DS    CL14                Credit account                               
IW_REF   DS    CL20                Reference                                    
IW_DATE  DS    PL3                 Date                                         
IW_SREF  DS    XL2                 Sub-reference                                
IW_WCSQ  DS    XL2                 Work code sequence number                    
IW_KEYL  EQU   *-IW_KEY                                                         
                                                                                
IW_WC    DS    CL2                 Work code                                    
IW_WAMT  DS    PL6                 Work code amount                             
IW_WFAMT DS    PL6                 Work code foreign amount                     
                                                                                
IW_NARR# EQU   3                   Max number of narratives                     
IW_NARRL DS    XL1                                                              
IW_NARR  DS    CL200                                                            
         DS    (IW_NARR#-1)XL(L'IW_NARRL+L'IW_NARR)                             
IW_WCLQ  EQU   *-IW_KEY                                                         
IW_LENQ  EQU   *-IW_REC                                                         
                                                                                
***********************************************************************         
* The records are read from the TSAR buffer into I_BUF. From here     *         
* they are moved into I_CUR. The following reserves storage for       *         
* the largest record.                                                 *         
***********************************************************************         
                                                                                
I_BUF    DSECT ,                                                                
         DS    (IW_LENQ)X                                                       
         ORG   I_BUF                                                            
         DS    (IW_LENQ)X                                                       
         ORG   ,                                                                
I_BUFL   EQU   *-I_BUF                                                          
                                                                                
***********************************************************************         
* Error table                                                         *         
***********************************************************************         
                                                                                
ET_D     DSECT                                                                  
ET_EOTQ  EQU   FF                  End of table indiciator                      
ET_LN    DS    X                   Length of this entry                         
ET_ERRNO DS    XL(L'ROUERRV)       Error number                                 
ET_ROWNM DS    XL2                 Row number error applies                     
ET_LN1Q  EQU   *-ET_D                                                           
ET_EXTRA DS    0C                  Extra error text                             
                                                                                
***********************************************************************         
* APELIST DSECT                                                                 
***********************************************************************         
APXLISD  DSECT                                                                  
APXSTAT  DS    XL1                                                              
APXACT   DS    XL14                                                             
APXLNQ   EQU   *-APXLISD                                                        
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
                                                                                
ADDTRND  DSECT                                                                  
       ++INCLUDE ACADDTRND                                                      
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
*PREFIX=P_                                                                      
       ++INCLUDE DDDPRINTL         For request details logging/trace            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACBRA1A   05/28/13'                                      
         END                                                                    
                                                                                
