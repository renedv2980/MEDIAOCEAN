*          DATA SET SPLNK12    AT LEVEL 116 AS OF 02/23/21                      
*PHASE T21E12C                                                                  
SPLNK12  TITLE '- Spot Desktop server'                                          
                                                                                
         GBLB  &FAST_ASSEMBLY                                                   
&FAST_ASSEMBLY SETB 1              ** NOOP THIS FOR INTEGRITY CHECK **          
                                                                                
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,                                                 +        
               SERVERTYPE=TSTSPOT,                                     +        
               WORKERKEY=SPBD,                                         +        
               FACS=FACS,                                              +        
               LOADFACSOFF=Y,                                          +        
               SEGMENT=Y,                                              +        
               APPEND=Y,                                               +        
               REQUEST=*,                                              +        
               CODE=CODE,                                              +        
               SYSPHASE=SYSPHASE,                                      +        
               SYSTEM=SPTSYSQ,                                         +        
               FILES=FILES,                                            +        
               IDF=Y,                                                  +        
               BIG=Y,                                                  +        
               SLOWLIST=SLOWS,                                         +        
               ABENDLIST=FAILS,                                        +        
               AUTOCLEAR=Y,                                            +        
               BLOCKS=(B#WORKD,WORKD,                                  +        
               B#SAVED,SAVED,                                          +        
               B#TWAD,TWAD,                                            +        
               B#AGYREC,AGYHDR,                                        +        
               B#SDRREC,SDRRECD,                                       +        
               B#SVRDEF,SVRDEF),                                       +        
               BLOCKS2=(B#BUYREC,BUYREC,B#MKTREC,MKTREC,               +        
               B#ESTREC,ESTHDR,B#FLTREC,DFLRECD,B#SOFREC,DFLRECD,      +        
               B#CLRREC,CLRSTATD,B#GOLREC,GOALRECD,B#SLKREC,SLKRECD,   +        
               B#REVREC,DRVRECD,B#DEVREC,DDVRECD,                      +        
               B#MSTREC,CT99RECD,B#ERPREC,CTEPRECD,                    +        
               B#STSREC,SRSRECD)                                                
                                                                                
CODE     NMOD1 0,**SL12**,RR=RE                                                 
         LAY   RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK PARAMETER BLOCK)                 
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   INIT02                                                           
         L     R9,LP_ABLK1         ONLINE - ROOT PROVIDES WORKD/SAVED           
         L     R8,LP_ABLK2                                                      
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD+2048(R8)                                                
         USING SAVED,R8            R8=A(SAVE W/S)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ABUFFRIN,RBUFFRIN                                                
         DROP  R6,R7                                                            
                                                                                
         LA    R0,LP_D                                                          
         ST    R0,ALP              SAVE A(LP_D)                                 
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         USING STABLKD,WORK                                                     
         USING STAPACKD,WORK                                                    
         USING FAWSSVRD,WORK                                                    
         USING TSARD,TSARBLK                                                    
         USING FLMRECD,TSARREC                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR02                                                         
                                                                                
         LHI   R0,18*ONEK                                                       
         GETMAIN R,LV=(0)          ACQUIRE STORAGE FOR SPOT TABLE               
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ASDSTAB          SET A(SPOT TABLE)                            
                                                                                
         L     RF,ACOMFACS         LOAD FACILITIES OVERLAYS                     
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
*&&DO                                                                           
         GOTOR (RF),DMCB,('P#MFMSVR',0),0,0                                     
         MVC   LP_ASVR2,0(R1)      SET A(MFM/MEDIAVANTAGE SERVER)               
         GOTOR (RF),DMCB,('P#STWSVR',0),0,0                                     
         MVC   LP_ASVR3,0(R1)      SET A(CANADIAN STEWARD SERVER)               
         GOTOR (RF),DMCB,('P#CANSVR',0),0,0                                     
         MVC   LP_ASVR4,0(R1)      SET A(CANADIAN DEKSTOP SERVER)               
         GOTOR (RF),DMCB,('P#REVDLD',0),0,0                                     
         MVC   LP_ASVR5,0(R1)      SET A(US WORKSHEET/REVISION SERVER)          
         GOTOR (RF),DMCB,('P#FUASVR',0),0,0                                     
         MVC   LP_ASVR6,0(R1)      SET A(US FUA SERVER)                         
         GOTOR (RF),DMCB,('P#OMSVR',0),0,0                                      
         MVC   LP_ASVR7,0(R1)      SET A(OM SERVER)                             
*&&                                                                             
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
RUNSTR02 MVC   LP_BLKS+((B#AGYREC-1)*L'LP_BLKS)(AIOLAST-AIO1),AIO1              
         MVC   LP_BLKS+((B#TWAD-1)*L'LP_BLKS)(L'LP_BLKS),LP_ATWA                
         MVC   LP_BLKS+((B#SVRDEF-1)*L'LP_BLKS)(L'LP_BLKS),LP_ASVR              
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONSN                                                       
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         A     R2,SRVRRELO                                                      
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
                                                                                
         XC    REQVALS(REQVALL),REQVALS                                         
         XC    PRVVALS(PRVVALL),PRVVALS                                         
         LA    R0,QVALS                                                         
         LHI   R1,L'QVALS                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R0,OUTVALS                                                       
         LHI   R1,OUTVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         OI    LP_FLAG2,LP_FSTIA   SET SAVE/RESTORE TIA                         
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RUN DOWNLOAD REQUEST                                                *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYINIT                                                   
         MVC   GBYAGY,LP_AGY                                                    
         MVC   GBYCOMF,ACOMFACS                                                 
         GOTOR VGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
         MVC   SAVE1OR2,GBY1OR2                                                 
         MVC   SV1OR2,GBY1OR2      FOR IOEXEC                                   
                                                                                
         L     RE,LP_AWMP          BUILD DEFAULT RANGE ELEMENTS IN WMP          
         USING LW_D,RE                                                          
         MVI   LW_TYPE,LW_TALLQ    ALL VALUES                                   
         STCM  RE,7,AALL                                                        
         AHI   RE,LW_LN1Q                                                       
         MVI   LW_TYPE,LW_TNZRQ    NON-ZERO VALUES                              
         STCM  RE,7,ANZR                                                        
         AHI   RE,LW_LN1Q                                                       
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
         XC    MAPI,MAPI           INITIALIZE MAP INDICATORS                    
         LA    RF,MAPTAB                                                        
         LHI   R0,MAPTABN                                                       
         BASR  RE,0                                                             
         CLC   LP_QMAPN,0(RF)                                                   
         JNE   *+14                                                             
         MVC   MAPI,L'LP_QMAPN(RF)                                              
         J     *+10                                                             
         AHI   RF,L'MAPTAB                                                      
         BCTR  R0,RE                                                            
                                                                                
         OC    ENDDATEB,ENDDATEB   SET END DATES TO HIGH VALUES                 
         BNZ   *+10                IF NOT INPUT                                 
         MVC   ENDDATEB,EFFS                                                    
         OC    ENDDATEC,ENDDATEC                                                
         BNZ   *+10                                                             
         MVC   ENDDATEC,EFFS                                                    
         OC    QENDDATE,QENDDATE                                                
         BNZ   *+10                                                             
         MVC   QENDDATE,EFFS                                                    
                                                                                
         MVC   AGENCY,LP_AGY       SET AGENCY ALPHA ID                          
         ICM   RF,15,AMASTC        SET TRACE OPTION IF OFFLINE                  
         BZ    *+10                                                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNREQ02                                                         
         GOTOR VDATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,TRFDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,SPTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,XSPFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,STAFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENFIL,(4,0),0                               
                                                                                
         GOTOR (#GETAGY,AGETAGY),LP_AGY                                         
                                                                                
RUNREQ02 MVI   RUNI1,0                                                          
         L     RF,AAGYREC          EXIT IF CANADIAN AGENCY                      
         CLI   AGYPCNDA-AGYHDR(RF),CANAGYQ                                      
         JE    EXITN                                                            
****     OI    RUNI1,RUNICANA                                                   
                                                                                
         TM    MAPI2,MAPIAMCE      TEST WANT AGY/MED/CLT/EST RESOLVED           
         BZ    RUNREQ08                                                         
                                                                                
         ICM   RF,7,AMED                                                        
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         JNE   *+2                                                              
                                                                                
         ICM   RF,7,ACLT           RF=A(CLIENT REQUEST VALUE)                   
         BZ    RUNREQ04                                                         
         MVC   QCLTX,LW_DATA1-LW_D(RF)                                          
         GOTOL RESCLT              RESOLVE CLIENT VALUES                        
                                                                                
RUNREQ04 CLI   ESTIND,LW_TALLQ     TEST ANY ESTIMATES GIVEN                     
         BNE   RUNREQ06                                                         
         L     RE,LP_AWMP          NO - BUILD WORK MAP POOL ENTRY               
         USING LW_D,RE                                                          
         STCM  RE,7,AEST2          AND POINT TO IT                              
         MVI   ESTIND2,LW_TLSTQ                                                 
         LHI   R0,LW_LN2Q                                                       
         STCM  R0,3,LW_LN                                                       
         XC    LW_NUMN,LW_NUMN     SET NO ENTRIES IN LIST                       
         MVI   LW_TYPE,LW_TLSTQ                                                 
         AHI   RE,LW_LN2Q+256                                                   
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
RUNREQ06 GOTOL INIDAT              INITIALIZE REQUEST DATES                     
                                                                                
         ICM   RE,7,ADEM           TEST ANY DEMOS REQUESTED                     
         BNZ   *+8                                       FEATURE WAS            
         MVI   XSPILL,YESQ         NO - TURN OFF SPILL | DEPRACATED             
                                                                                
         CLC   LP_QMAPN,SDBUYD#    TEST BUY/GOAL DOWNLOAD                       
         JE    *+14                                                             
         CLC   LP_QMAPN,SDNBUYD#   OR NEW BUY/GOAL DOWNLOAD                     
         JNE   RUNREQ08                                                         
         CLI   DLDBYSTA,0          DOWNLOAD ONLY BY STATION?                    
         JNE   RUNREQ08            MARKET AND STATION PROVIDED                  
         GOTOL BLDMKS              BUILD MARKET/STATION DRIVER VALUES           
         J     RUNREQ08                                                         
                                                                                
RUNREQ08 TM    MAPI1,MAPIBUYD      TEST BUY DOWNLOAD                            
         BZ    RUNREQ10                                                         
         GOTOL BUFFER,DMCB,('TSAINI',0),('FLMRKEYL',FLMRRECL)                   
         JNE   *+2                                                              
                                                                                
RUNREQ10 XC    SVERROR,SVERROR     INITIALIZE ERROR NUMBER                      
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         OC    SVERROR,SVERROR     TEST ERROR NUMBER IS SET                     
         BZ    EXITY                                                            
         MVC   LP_ERROR,SVERROR    YES - SET DDLINK ERROR VALUE                 
         B     EXITN                                                            
         DROP  RB                                                               
         EJECT                                                                  
FILES    DS    0C                  ** SYSTEM/FILE LIST **                       
                                                                                
         DC    C'SPOT   '                                                       
                                                                                
         DC    C'N'                                                             
         DC    C'SPTDIR '                                                       
         DC    C'N'                                                             
         DC    C'SPTFILE'                                                       
         DC    C'N'                                                             
         DC    C'XSPDIR '                                                       
         DC    C'N'                                                             
         DC    C'XSPFILE'                                                       
         DC    C'N'                                                             
         DC    C'STAFILE'                                                       
         DC    C'N'                                                             
         DC    C'CTFILE '                                                       
         DC    C'N'                                                             
         DC    C'STRFFL '                                                       
         DC    C'N'                                                             
         DC    C'STRFDR '                                                       
         DC    C'N'                                                             
         DC    C'GENDIR '                                                       
         DC    C'N'                                                             
         DC    C'GENFIL '                                                       
         DC    C'N'                                                             
         DC    C'DEMDIRA'                                                       
         DC    C'N'                                                             
         DC    C'DEMDIRN'                                                       
         DC    C'N'                                                             
         DC    C'DEMDIRR'                                                       
         DC    C'N'                                                             
         DC    C'L=DEMFA'                                                       
         DC    C'N'                                                             
         DC    C'L=DEMFN'                                                       
         DC    C'N'                                                             
         DC    C'L=DEMFR'                                                       
                                                                                
FILESX   DC    C'X'                                                             
                                                                                
FAILS    DC    C'JNEW,HWON,WHOA:'                                               
SLOWS    DC    C'JNEW,HWON,WHOA:'                                               
                                                                                
FACS     DS    0XL(RFACTABL)       ** SYSTEM FACILITIES **                      
         DC    AL1(QDEMTABS),AL2(CDEMTABS-COMFACSD,0)                           
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD,0)                             
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD,0)                           
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD,0)                             
         DC    AL1(QDEMEL),AL2(CDEMEL-COMFACSD,0)                               
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD,0)                           
         DC    AL1(QDEMOMTH),AL2(CDEMOMTH-COMFACSD,0)                           
         DC    AL1(QDDISP),AL2(CT00AD0-COMFACSD,0)                              
         DC    AL1(RFACEOTQ)                                                    
                                                                                
* SPDARERROR                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPDARERROR                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* INITIAL DOWNLOAD                                                    *         
***********************************************************************         
                                                                                
REQINI   LKREQ H,I#SDINID,OUTSDI,NEXTREQ=REQMKT                                 
Enviro   LKREQ F,1,(D,B#SAVED,ENVIRO),LBIN,TEXT=SP#NUM,COL=*                    
CnvrDl   LKREQ F,10,(D,B#SAVED,CNVRGEDL),HDRO,TEXT=SP#CNVRG,COL=*               
         LKREQ E                                                                
                                                                                
OUTSDI   LKOUT H                   ** INITIAL DOWNLOAD **                       
                                                                                
SDAGYC   LKOUT R,X'0012'                                                        
Array    LKOUT C,X'0012',(A,ARYSDA)                                             
         LKOUT E                                                                
*&&DO                                                                           
SDTEST   LKOUT R,X'0048'                                                        
CNVRDL   LKOUT C,1,(D,B#SAVED,CNVRGEDL),CHAR,ND=Y                               
CNVRDL   LKOUT C,10,(D,B#SAVED,CNVRGEDL),CHAR,ND=Y                              
         LKOUT E                                                                
*&&                                                                             
SDDEMC   LKOUT R,X'0014'                                                        
Array    LKOUT C,X'0014',(A,ARYSDD)                                             
         LKOUT E                                                                
                                                                                
SDMEDC   LKOUT R,X'0016'                                                        
Array    LKOUT C,X'0016',(A,ARYSDMD),FILTROUT=TSTCNVRG                          
         LKOUT E                                                                
                                                                                
SDREPS   LKOUT R,X'00D0',PCVERSION=2.0.0.1                                      
Array    LKOUT C,X'00D0',(A,ARYREPS),FILTROUT=TSTCNVRG                          
         LKOUT E                                                                
                                                                                
SDERRS   LKOUT R,X'0013',PCVERSION=2.0.0.1                                      
Array    LKOUT C,X'0013',(A,ARYERRS),FILTROUT=TSTCNVRG                          
         LKOUT E                                                                
                                                                                
SDSRPS   LKOUT R,X'0015',PCVERSION=3.0.0.0                                      
Array    LKOUT C,X'0015',(A,ARYSRPS),FILTROUT=TSTCNVRG                          
         LKOUT E                                                                
                                                                                
SDSRC    LKOUT R,X'0017',PCVERSION=4.5.0.19                                     
Array    LKOUT C,X'0017',(A,ARYSTSR),FILTROUT=TSTCNVRG                          
         LKOUT E                                                                
                                                                                
SDPURP   LKOUT R,X'0019',PCVERSION=2.8.0.0                                      
ARRAY    LKOUT C,X'0019',(A,ARYPURP),FILTROUT=TSTCNVRG                          
         LKOUT E                                                                
                                                                                
SDREAS   LKOUT R,X'0021',PCVERSION=3.0.0.0                                      
Array    LKOUT C,X'0021',(A,ARYREAS),FILTROUT=TSTCNVRG                          
         LKOUT E                                                                
                                                                                
SDBTY2   LKOUT R,X'0023',PCVERSION=3.1.0.111                                    
Array    LKOUT C,X'0023',(A,ARYBKT2),FILTROUT=TSTCNVRG                          
         LKOUT E                                                                
                                                                                
SDMFMT   LKOUT R,X'0058',PCVERSION=3.2.0.16    MSTREET FORMAT RECS              
ARRAY    LKOUT C,255,(A,ARYFMT),FILTROUT=TSTOWCNV                               
         LKOUT E                                                                
                                                                                
SDDPTL   LKOUT R,X'0026',PCVERSION=3.2.0.16    DPT LIST ACROSS MENUS            
Array    LKOUT C,X'0026',(A,ARYDPTL),FILTROUT=TSTCNVRG                          
         LKOUT E                                                                
                                                                                
SDMOWN   LKOUT R,X'005A',PCVERSION=4.0.0.32    MSTEET FORMAT RECS               
ARRAY    LKOUT C,255,(A,ARYOWN),FILTROUT=TSTOWCNV                               
         LKOUT E                                                                
                                                                                
SDSWEEP  LKOUT R,X'0018',PCVERSION=4.6.0.50      SWEEP TABLE DOWNLOAD           
Array    LKOUT C,X'0018',(A,ARYSWP),FILTROUT=TSTCNVRG                           
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
TSTUODTA CLI   USESODTA,C'Y'       TEST USES OWNERSHIP DATA                     
         BR    RE                                                               
                                                                                
TSTCNVRG CLI   CNVRGEDL,0          TEST CONVERGENCE INIITAL DOWNLOAD?           
         BR    RE                                                               
                                                                                
* TEST BOTH OWNERSHIP AND CONVERGENCE                                           
TSTOWCNV CLI   USESODTA,C'Y'                                                    
         BNE   RTRNNO                                                           
         CLI   CNVRGEDL,0                                                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR AGENCY DOWNLOAD                                *         
***********************************************************************         
                                                                                
ARYSDA   LKOUT A,(D,B#AGYREC,AGYHDR),NEWEL=Y,NROWS=1,ROWWIDTH=0                 
PRout    LKOUT P,,SETMAX                                                        
PRout    LKOUT P,,GTDATTIM                                                      
PRout    LKOUT P,,GETUID                                                        
AgyNm    LKOUT C,1,AGYNAME,CHAR                                                 
AgyAd    LKOUT C,2,AGYADDR,CHAR                                                 
UsrNm    LKOUT C,3,(D,B#SAVED,USIDNAME),CHAR,ND=Y                               
PRout    LKOUT P,AGYPCNDA,SETCAN                                                
CanAg    LKOUT C,4,(D,B#WORKD,BYTE),CHAR,ND=Y                                   
BPOTO    LKOUT C,5,AGYPBOTO,CHAR,ND=Y                                           
BuyID    LKOUT C,6,AGYPBREQ,CHAR,ND=Y                                           
MgeMM    LKOUT C,7,AGYPMGMM,CHAR,ND=Y                                           
-SReq    LKOUT C,8,AGYPSAUT,CHAR,ND=Y                                           
OfReq    LKOUT C,9,AGYPOREQ,CHAR,ND=Y                                           
By/Br    LKOUT C,10,AGYPBYBR,CHAR,ND=Y                                          
SOTOC    LKOUT C,11,AGYPSOCD,CHAR,ND=Y                                          
AgyOf    LKOUT C,12,(D,B#SAVED,DAREROUT),CHAR,ND=Y,PCVERSION=2.0.0.1            
MxBLn    LKOUT C,13,(D,B#SAVED,MAXBLNS),UBIN,ND=Y,PCVERSION=3.0.0.120           
MxBSz    LKOUT C,14,(D,B#SAVED,MAXBSIZ),UBIN,ND=Y,PCVERSION=3.0.0.120           
UAddr    LKOUT C,16,(D,B#SAVED,USIDADDR),ND=Y,CHAR,PCVERSION=3.0.0.120          
AgyAl    LKOUT C,18,(D,B#SAVED,AGENCY),CHAR,PCVERSION=3.0.0.120                 
PRout    LKOUT P,,SETXTRAI                                                      
DDSTm    LKOUT C,100,(D,B#WORKD,WORK+0),CHAR,ND=Y,LEN=1,               +        
               PCVERSION=3.0.0.131                                              
ROnly    LKOUT C,101,(D,B#WORKD,WORK+1),CHAR,ND=Y,LEN=1,               +        
               PCVERSION=3.0.0.131                                              
PRout    LKOUT P,,GETTOKN                                                       
COMACCS  LKOUT C,102,(D,B#WORKD,TEMP1),CHAR,ND=Y                                
PRout    LKOUT P,,GETSDR                                                        
Array    LKOUT C,255,(A,ARYSDR),PCVERSION=3.0.0.120                             
*                                                                               
UODta    LKOUT C,37,(D,B#SAVED,USESODTA),CHAR,ND=Y,PCVERSION=3.2.0.16           
CurrDt   LKOUT C,40,(D,B#SAVED,CURRDATE),EDAT,ND=Y,PCVERSION=4.0.0.5            
CurrTm   LKOUT C,41,(D,B#SAVED,CURRTIME),HEXD,ND=Y,PCVERSION=4.0.0.5            
OwnrSrce LKOUT C,44,(D,B#SAVED,OWNRSRCE),CHAR,ND=Y,PCVERSION=4.0.0.32           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* SET MAXIMUM SIZE OF BUY RECORD AND HIGHEST BUY LINE NUMBER          *         
***********************************************************************         
                                                                                
SETMAX   MVC   MAXBSIZ,=AL2(5972)  CURRENTLY THE MAX BUY SIZE                   
         MVC   MAXBLNS,=AL2(255)   255 IS HIGHEST BUYLINE #                     
*                                                                               
         CLI   GBY1OR2,2           TEST 1 OR 2 BYTE LINE NUMBERS                
         JNE   *+10                1-BYTE, LEAVE MAXBLNS ALONE                  
         MVC   MAXBLNS,=AL2(499)   2-BYTE LINE NUMBER, SET 499 MAX              
*                                                                               
         B     EXITY                                                            
                                                                                
***********************************************************************         
* GET CURRENT DATE/TIME (EST)                                                   
***********************************************************************         
GTDATTIM DS    0H                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,FULL2         FULL & FULL2 USED AS PACKED #'S HERE         
         ST    R1,FULL                                                          
         AP    FULL2,FULL                                                       
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(0,CURRDATE)   THE TIME                       
         CP    FULL2,=P'240000'    PAST MIDNIGHT?                               
         JL    GTDTM010                                                         
         SP    FULL2,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST             
         GOTOR VADDAY,DMCB,CURRDATE,CURRDATE,F'1'                               
*                                                                               
GTDTM010 ICM   R1,15,FULL2                                                      
         SRL   R1,12                                                            
         STCM  R1,3,CURRTIME                                                    
*                                                                               
         B     EXITY                                                            
                                                                                
***********************************************************************         
* READ USER-ID RECORD AND EXTRACT VALUES                              *         
***********************************************************************         
                                                                                
GETUID   XC    USIDNAME,USIDNAME                                                
         XC    USIDADDR,USIDADDR                                                
         XC    DAREROUT,DAREROUT                                                
                                                                                
         BRAS  RE,GETACC           GET THE ACCESS RECORD                        
                                                                                
GETUID10 LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,LP_USRID                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO3'                            
         BNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         LA    R2,CTIDATA          R2=A(FIRST ELEMENT ON ID RECORD)             
         USING CTORGD,R2                                                        
         SR    R0,R0                                                            
GETUID15 CLI   CTORGEL,EOR         TEST END OF RECORD                           
         BE    EXITY                                                            
         CLI   CTORGEL,CTORGELQ    TEST ORIGIN DETAIL ELEMENT                   
         JNE   GETUID20                                                         
         MVC   USIDNAME,CTORGNAM                                                
         MVC   USIDADDR,CTORGADD                                                
         J     GETUID30                                                         
                                                                                
         USING CTUSAD,R2                                                        
GETUID20 CLI   CTUSAEL,CTUSAELQ    TEST US AGENCY EXTRA INFO ELEMENT            
         JNE   GETUID30                                                         
         MVC   DAREROUT,CTUSADRC   SET DARE ROUTING CODE                        
         L     RF,ATWA                                                          
         MVC   SVDARERC-TWAD(,RF),DAREROUT                                      
                                                                                
GETUID30 IC    R0,CTUSALEN         BUMP TO NEXT ELEMENT ON RECORD               
         AR    R2,R0                                                            
         J     GETUID15                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* SET MAXIMUM SIZE OF DEMO REQUEST PARAMETERS                                   
***********************************************************************         
*&&DO                                                                           
SETDMSIZ MVC   MXDMSIZE,=AL2(SPDMAXSZ)                                          
         MVI   STDTSIZE,SPDSTADT                                                
         MVI   BOOKSIZE,SPDBKFIL                                                
         MVI   DEMOSIZE,SPDDEMOS                                                
         MVI   PROJSIZE,SPDPROJ                                                 
         MVI   MAXPROJ,SPDNPROJ                                                 
         B     EXITY                                                            
*                                                                               
SPDMAXSZ EQU   7500                MAX BUFFER SIZE                              
SPDSTADT EQU   121                 STATION DAYTIME REQUEST SIZE                 
SPDBKFIL EQU   23                  BOOK REQUEST SIZE                            
SPDDEMOS EQU   24                  DEMO REQUEST SIZE                            
SPDPROJ  EQU   35                  PROJECTION REQUEST SIZE                      
SPDNPROJ EQU   10                  MAX PROJECTIONS ALLOWED                      
*&&                                                                             
                                                                                
SETCAN   L     R1,LP_AINP          SET BYTE=C'Y' IF CANADIAN AGENCY             
         MVI   BYTE,0                                                           
         CLI   0(R1),C'C'                                                       
         BNER  RE                                                               
         MVI   BYTE,C'Y'                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* SET WORK+0 TO C'Y' IF USER IS ON A DDS TERMINAL                     *         
* SET WORK+1 TO C'Y' IF USER IS READ ONLY ACCESS                      *         
***********************************************************************         
                                                                                
SETXTRAI XC    WORK(2),WORK                                                     
         L     RF,LP_ALPXD         GET ADDRESS OF EXTENSION BLOCK               
         USING LP_XD,RF                                                         
         USING XTRAINFD,LP_XTRAV                                                
         TM    XIFLAG1,XIDDSPER+XIDDSTRM                                        
         JZ    *+8                                                              
         MVI   WORK+0,C'Y'         DDS PERSON OR TERMINAL                       
         TM    XIFLAG1,XIROSYS+XIROMODE                                         
         JZ    *+8                                                              
         MVI   WORK+1,C'Y'         READONLY SYSTEM OR MODE                      
         B     EXIT                                                             
         DROP  RF                                                               
***********************************************************************         
* GET THE SECURITY TOKEN RECORDS                                                
*                                                                               
* NOTE:  AIO3 WILL BE CLOBBERED TO READ THE TOKEN RECORD                        
*        TEMP1 WILL BE CLOBBERED WITH THE RETURN STRING                         
***********************************************************************         
GETTOKN  DS    0H                                                               
         XC    TEMP1,TEMP1           THIS IS OUR OUTPUT AREA                    
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING TOKKEY,R4                                                        
         XC    IOKEY,IOKEY           X'00'                                      
         MVI   TOKKMIN,TOKKMINQ      C'K'                                       
         MVI   TOKKTYP,TOKKRTRK      X'01' - RENTRAK RECORD                     
         L     R1,LP_ASECD                                                      
         MVC   TOKKSAGY,SECAGY-SECD(R1)  SECURITY AGENCY CODE                   
         MVC   TOKKAAGY,SECOAGY-SECD(R1) AGENCY ALPHA CODE                      
         MVI   TOKKSYS,X'02'         SPOT SYSTEM                                
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOGENDIR+IO3'                            
         JNE   GETTOKNX                                                         
         CLC   IOKEY(TOKKUSR-TOKKEY),IOKEYSAV                                   
         JNE   GETTOKNX                                                         
         CLI   TOKKSYS,X'02'       SPOT?                                        
         JNE   GETTOKNX                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOGENFIL+IO3'                           
         JNE   GETTOKNX                                                         
*                                                                               
         L     R4,AIO3                                                          
         LA    R4,TOKFIRST(R4)         R4=A(1ST ELEMENT)                        
GTTKN10  CLI   0(R4),0                 ANY ELEMENTS?                            
         JE    GETTOKNX                                                         
         CLI   0(R4),RTAUTELQ          X'0A' - RENTRAK AUTHOR ELEM?             
         JE    GTTKN30                                                          
         LLC   R0,1(R4)                CHECK THE NEXT ELEMENT                   
         AR    R4,R0                                                            
         J     GTTKN10                                                          
*                                                                               
         USING RTAUTHD,R4                                                       
GTTKN30  CLC   RTAUTID,SPACES          LICENSE ID BETTER BE > SPACES            
         JNH   GETTOKNX                                                         
         LA    RF,TEMP1                                                         
         USING COMACCSD,RF                                                      
         MVC   RNTKRTGS,=C'COM'        comScore                                 
         MVI   RNTKPIP1,C'|'                                                    
         MVC   RNTKLICI,RTAUTID        LICENSE ID                               
         MVI   RNTKPIP2,C'|'                                                    
         MVC   RNTKSECI,RTAUTSEC       SECURITY ID                              
         DROP  R4,RF                                                            
*                                                                               
GETTOKNX B     EXIT                                                             
*                                                                               
COMACCSD DSECT                                                                  
RNTKRTGS DS    CL3                     RATING SOURCE                            
RNTKPIP1 DS    C                       C'|'                                     
RNTKLICI DS    CL(L'RTAUTID)           LICENSE ID                               
RNTKPIP2 DS    C                       C'|'                                     
RNTKSECI DS    CL(L'RTAUTSEC)          SECURITY ID                              
*                                                                               
SVRDEF   CSECT                                                                  
***********************************************************************         
* CALL DDLINK TO GET SELF-DEFINING RECORD                             *         
* CUSTOM OVERRIDE KEY:                                                          
*  + 0 - AGENCY ALPHA CODE                                                      
*  + 2 - ENVIRONMENT (X'01'-DEV, X'02'-STAGING, X'FF'-UAT)                      
*                                                                               
* NOTE:  AIO3 WILL BE CLOBBERED TO READ THE GENERIC SDR                         
*        AIO5 WILL BE CLOBBERED TO READ THE AGENCY SPECIFIC SDR                 
***********************************************************************         
                                                                                
         USING GETSDRD,RC                                                       
GETSDR   GOTOR LP_AGSDR,DMCB,LP_D,ASDRREC,0                                     
*                                                                               
         L     RF,ACOMFACS         LOAD FACILITIES OVERLAYS                     
         L     RF,CRECUP-COMFACSD(RF)                                           
         ST    RF,VRECUP                                                        
*                                                                               
*****                                                                           
* SDR SEQUENTIAL RECORD                                                         
*****                                                                           
GTSDR05  L     R2,AIO5                                                          
         USING SDRRECD,R2                                                       
         XC    0(L'SDRKEY,R2),0(R2)  CLEAR OUT KEY IN IOAREA                    
*                                                                               
         LHI   RF,1                                                             
GTSDR07  STC   RF,BYTE                                                          
         GOTOR LP_AGSDR,DMCB,LP_D,(BYTE,AIO5),0                                 
*                                                                               
         L     RE,ASDRREC          NEED THIS CHECK AS GETSDR DOES A             
         L     RF,AIO5               DEFAULT READ BY CLEARING SDRKFFL           
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDR09                                                          
         CLC   31(1,RF),BYTE       MATCH ON SEQ?                                
         JNE   GTSDR09                                                          
*                                                                               
         BRAS  RE,SDRMOVE                                                       
*                                                                               
         LLC   RF,BYTE                                                          
         AHI   RF,1                                                             
         CHI   RF,3                MORE THEN 2 SEQ RECORDS?                     
         JNH   GTSDR07              NO, GET NEXT SEQ                            
         DC    H'0'                 YES, TRASHES NEXT AIO6                      
*****                                                                           
* AGENCY ALPHA SDR OVERRIDE                                                     
*****                                                                           
GTSDR09  L     R2,AIO5                                                          
         USING SDRRECD,R2                                                       
         XC    0(L'SDRKEY,R2),0(R2)  CLEAR OUT KEY IN IOAREA                    
*                                                                               
         XC    WORK,WORK             READ INTO AIO5                             
         MVC   WORK(L'LP_AGY),LP_AGY                                            
         GOTOR LP_AGSDR,DMCB,LP_D,AIO5,WORK                                     
*                                                                               
         L     RE,ASDRREC            NEED THIS CHECK AS GETSDR DOES A           
         L     RF,AIO5                 DEFAULT READ BY CLEARING SDRKFFL         
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDR10                                                          
*                                                                               
         BRAS  RE,SDROVRW                                                       
*****                                                                           
* ENVIRONMENT SDR OVERRIDE, ENVIRONMENT PASSED FROM SBTK                        
*****                                                                           
GTSDR10  L     R2,AIO5                                                          
         USING SDRRECD,R2                                                       
         XC    0(L'SDRKEY,R2),0(R2)  CLEAR OUT KEY IN IOAREA                    
*                                                                               
         CLI   ENVIRO,0              DO WE HAVE AN ENVIROMENT OVERRIDE?         
         JE    GTSDR20               NO                                         
         XC    WORK,WORK             READ INTO AIO5                             
         MVC   WORK+2(L'ENVIRO),ENVIRO                                          
         GOTOR LP_AGSDR,DMCB,LP_D,AIO5,WORK                                     
*                                                                               
         L     RE,ASDRREC            NEED THIS CHECK AS GETSDR DOES A           
         L     RF,AIO5                 DEFAULT READ BY CLEARING SDRKFFL         
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDR20                                                          
*                                                                               
         BRAS  RE,SDROVRW                                                       
*****                                                                           
* UAT SDR OVERRIDE - ENVIRONMENT 255 (NOT PASSED FROM SBTK)                     
*****                                                                           
GTSDR20  L     R2,AIO5                                                          
         USING SDRRECD,R2                                                       
         XC    0(L'SDRKEY,R2),0(R2)  CLEAR OUT KEY IN IOAREA                    
*                                                                               
         L     RE,ACOMFACS                                                      
         L     RE,CXTRAINF-COMFACSD(RE)                                         
         USING XTRAINFD,RE                                                      
         TM    XIAGCOPT,XIAGUAT      ARE WE UAT?                                
         JZ    GTSDR30               NO, NOTHING TO DO HERE THEN                
         DROP  RE                                                               
*                                                                               
         XC    WORK,WORK             READ INTO AIO5                             
         MVI   WORK+2,X'FF'          SPECIFICALLY LOOK FOR UAT                  
         GOTOR LP_AGSDR,DMCB,LP_D,AIO5,WORK                                     
*                                                                               
         L     RE,ASDRREC            NEED THIS CHECK AS GETSDR DOES A           
         L     RF,AIO5                 DEFAULT READ BY CLEARING SDRKFFL         
         CLC   0(SDRKKEY-SDRKEY,RE),0(RF)                                       
         JNE   GTSDR30                                                          
*                                                                               
         BRAS  RE,SDROVRW                                                       
*****                                                                           
GTSDR30  DS    0H                                                               
*                                                                               
GTSDRX   B     EXIT                                                             
**********************                                                          
* MOVE VALUES IN SEQ SDR INTO OUR SPECIFIC SDR                                  
**********************                                                          
SDRMOVE  NTR1  LABEL=*                                                          
         L     R2,AIO5             LOOK AT THE AGENCY SPECIFIC SDR              
         OC    0(2,R2),0(R2)       MAKE SURE WE HAVE A RECORD                   
         JZ    SDRMVX              OTHERWISE JUST USE GENERIC SDR               
         LA    R2,SDRRFRST                                                      
SDRMV010 CLI   0(R2),0             ANY MORE ELEMENT IN SEQ RECORD?              
         JE    SDRMVX                                                           
*                                                                               
         L     R6,ASDRREC                                                       
         LLH   RF,SDRRLEN-SDRKEY(R6)  GET LENGTH OF REC                         
         AR    R6,RF                                                            
*                                                                               
         GOTOR VRECUP,DMCB,(X'FE',ASDRREC),0(R2),0(R6),=X'002A00201770'         
*                                                                               
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     SDRMV010                                                         
SDRMVX   B     EXIT                                                             
**********************                                                          
* OVERWRITE VALUES IN GENERIC SDR THAT WE FOUND IN OUR SPECIFIC SDR             
**********************                                                          
SDROVRW  NTR1  LABEL=*                                                          
         MVI   LASTSDR,0                                                        
*                                                                               
         L     R2,AIO5             LOOK AT THE AGENCY SPECIFIC SDR              
         OC    0(2,R2),0(R2)       MAKE SURE WE HAVE A RECORD                   
         JZ    SDROVX              OTHERWISE JUST USE GENERIC SDR               
         LA    R2,SDRRFRST                                                      
SDROV010 CLI   0(R2),0             ANY MORE ELEMENT IN AGY SPECIFIC?            
         JE    SDROVX                                                           
*                                                                               
         CLC   LASTSDR,0(R2)       SAME AS LAST SDR MAP?                        
         JE    SDROV035             YES, GO ADD IT                              
         MVC   LASTSDR,0(R2)       SAVE THE SDR MAP FOR LATER                   
         L     R6,ASDRREC                                                       
         LA    R6,SDRRFRST-SDRKEY(R6)  R6 = 1ST ELEMENT IN GENERIC              
SDROV020 CLI   0(R6),0             CAN'T FIND A MATCHING ELEM?                  
         JE    SDROV040            NO, JUST ADD IT TO GENERIC                   
         CLC   0(1,R6),0(R2)       FOUND A MATCH?                               
         JE    SDROV030             YES, DELETE IT                              
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         J     SDROV020                                                         
*                                                                               
* DELETE THE ELEM IN GENERIC                                                    
SDROV030 GOTOR VRECUP,DMCB,(X'FE',ASDRREC),0(R6),0(R6),=X'002A00201770'         
         J     SDROV020                                                         
*                                                                               
* BUMP BEYOND CURRENT ELEMENT AND                                               
SDROV035 LLC   R1,1(R6)            ADD THE NEW SDR AFTER (NOT BEFORE)           
         AR    R6,R1                CURRENT ELEMENT                             
*                                                                               
* ADD ELEM IN GENERIC                                                           
SDROV040 GOTOR VRECUP,DMCB,(X'FE',ASDRREC),0(R2),0(R6),=X'002A00201770'         
*                                                                               
         LLC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         J     SDROV010                                                         
SDROVX   B     EXIT                                                             
                                                                                
GETSDRD  DSECT                                                                  
VRECUP   DS    V                   RECUP FROM COMFACS                           
LASTSDR  DS    X                                                                
SVRDEF   CSECT                                                                  
***********************************************************************         
* SEND SELF-DEFINING ELEMENTS                                         *         
***********************************************************************         
                                                                                
ARYSDR   LKOUT A,(D,B#SDRREC,SDRRFRST),EOT=EOR,ROWWIDTH=(V,SDELEN),    +        
               ROWID=(SDELD,0)                                                  
                                                                                
***********************************************************************         
* FIELDS SENT FROM SELF-DEFINING RECORD ARE AS FOLLOWS:-              *         
*                                                                     *         
*        17    MAXIMUM ORDERS FOR BATCH ORDER EXPORT                  *         
*        19    MAXIMUM E-MAIL ADDRESSES PER ORDER                     *         
*        20    MESSAGE QUEUE STRING FOR OEXPRESS                      *         
*        21    MAXIMUM CHARACTERS PER E-MAIL ADDRESS                  *         
*        22    MAINFRAME HEADER                                       *         
*        23    DOC STORAGE SOAP SERVER CONFIGURATION INFO             *         
*        24    OEXPRESS GAP SERVER CONFIGURATION INFO                 *         
*        25    HTTP AUTHENTICATION USER ID/PASSWORD                   *         
*        26    ACCEPTABLE DDS TERMINAL EMAIL ADDRESSES                *         
***********************************************************************         
                                                                                
Defvl    LKOUT C,255,SDELD,SDEL                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMO TABLE DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYSDD   LKOUT A,(R,GETDCN),EOT=EOR,NEWEL=R,ROWNAME=DEMTABD,           +        
               ROWWIDTH=DEMTABL                                                 
DemCd    LKOUT C,1,DEMTCOD,CHAR                                                 
DemNm    LKOUT C,2,DEMTNAM,CHAR                                                 
         LKOUT E                                                                
                                                                                
***********************************************************************         
* BUILD DEMO CODE/NAME TABLE IN I/O AREA 6 FOR DOWNLOADING            *         
***********************************************************************         
                                                                                
GETDCN   DS    0H                                                               
         OC    LP_VRSN,LP_VRSN     CHECK PC VERSION                             
         JZ    GETDCN01                                                         
*                                                                               
         CLC   LP_VRSN,V330200                                                  
***NOP   BNL   NOMORE                                                           
*                                                                               
         USING GDWORKD,RC                                                       
         USING DBLOCKD,GDDBLOCK                                                 
GETDCN01 XC    DBLOCK,DBLOCK       BUILD TABLE OF DEMO CODES & NAMES            
         MVC   DBFILE,DEMOFILE                                                  
         MVI   DBSELMED,USAMEDQ    SET USA MEDIA                                
         L     RF,AAGYREC                                                       
         LA    R3,MODLUSA          R3=A(LIST OF MODIFIERS)                      
         CLI   AGYPCNDA-AGYHDR(RF),CANAGYQ                                      
         JNE   *+12                                                             
         LA    R3,MODLCAN                                                       
         MVI   DBSELMED,CANMEDQ    SET CANADIAN MEDIA                           
         MVC   DBCOMFCS,ACOMFACS                                                
                                                                                
         L     R2,AIO6             R2=A(DEMO TABLE BUILD AREA)                  
         USING DEMTABD,R2                                                       
         ST    R2,LP_ADATA                                                      
GETDCN02 CLI   0(R3),0             TEST END OF MODIFIER LIST                    
         JE    GETDCN06                                                         
         SR    R0,R0                                                            
GETDCN04 AHI   R0,1                BUMP DEMO NUMBER                             
         CHI   R0,255              TEST ALL CATEGORIES PROCESSED                
         JNH   *+12                                                             
         AHI   R3,1                YES - BUMP TO NEXT MODIFIER                  
         J     GETDCN02                                                         
                                                                                
         MVI   WORK+0,0            BUILD DEMO LOOK-UP EXPRESSION                
         MVC   WORK+1(1),0(R3)                                                  
         STC   R0,WORK+2                                                        
         GOTOR VDEMOCON,DMCB,WORK,(2,DEMTNAM),DBLOCK                            
         CLI   DEMTNNAM,C'*'       TEST BAD DEMO                                
         JE    GETDCN04                                                         
         CLI   DEMTNMOD,C' '       TEST MODIFIER IS A SPACE (IMPS)              
         JNE   *+14                                                             
         MVC   DEMTMOD(L'DEMTNNAM),DEMTNNAM                                     
         MVI   DEMTNNAM+L'DEMTNNAM-1,C' '                                       
         GOTOR (#EDTDCD,AEDTDCD),DMCB,WORK,0,DEMTCOD                            
                                                                                
         AHI   R2,DEMTABL          BUMP TO NEXT OUTPUT TABLE ENTRY              
         J     GETDCN04                                                         
                                                                                
GETDCN06 MVI   DEMTCOD,0           SET END OF DEMO TABLE                        
         B     EXITY                                                            
         DROP  R2,RC                                                            
                                                                                
GDWORKD  DSECT                     ** GETDCN LOCAL WORKING STORAGE **           
GDDBLOCK DS    XL(L'DBLOCK)        GETDCN DBLOCK                                
                                                                                
DEMTABD  DSECT                     ** DEMO TABLE **                             
DEMTCOD  DS    0CL(L'DEMTMOD+L'DEMTNUM)                                         
DEMTMOD  DS    CL1                 DEMO MODIFIER                                
DEMTNUM  DS    CL3                 DEMO NUMBER                                  
DEMTNAM  DS    0CL(L'DEMTNMOD+L'DEMTNNAM)                                       
DEMTNMOD DS    CL1                 DEMO MODIFIER                                
DEMTNNAM DS    CL6                 DEMO NAME                                    
DEMTABL  EQU   *-DEMTABD                                                        
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MEDIA DOWNLOAD                                 *         
***********************************************************************         
                                                                                
ARYSDMD  LKOUT A,(D,B#AGYREC,AGYEL),EOT=EOR,                           +        
               ROWID=(AGYMEDEL,AGYMEDEQ),ROWWIDTH=(V,AGYMEDLN)                  
                                                                                
MedCd    LKOUT C,1,AGYMEDCD,CHAR                                                
MedNm    LKOUT C,2,AGYMEDEX,CHAR                                                
PRout    LKOUT P,AGYMEDCD,SETCMED                                               
Array    LKOUT C,255,(A,ARYSLN)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
SETCMED  L     R1,LP_AINP          SET CURRENT MEDIA FROM INPUT VALUE           
         MVC   CURRMED,0(R1)                                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR VALID SPOT LENGTHS                             *         
***********************************************************************         
                                                                                
ARYSLN   LKOUT A,(R,NXTSLN),ROWNAME=SLNTABD                                     
SptLn    LKOUT C,3,(D,B#SAVED,CURRSLN),UBIN,LEN=1                               
EqvLn    LKOUT C,4,STABEQV,UBIN,LEN=1                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* DOWNLOAD VALID SPOT LENGTHS                                         *         
***********************************************************************         
                                                                                
NXTSLN   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSLN08                                                         
         MVI   CURRSLN,0           START WITH SPOT LENGTH 0                     
         L     R2,VSLNTAB                                                       
         LH    RE,0(R2)            RE=L(EACH AGY/MEDIA ENTRY)                   
         L     RF,2(R2)            RF=A(EOT)                                    
         AR    RF,R2                                                            
         AHI   R2,6                R2=A(1ST ENTRY)                              
                                                                                
         MVI   BYTE1,C'T'                                                       
         CLI   CURRMED,C'T'                                                     
         JE    NXTSLN02                                                         
         CLI   CURRMED,C'N'                                                     
         JE    NXTSLN02                                                         
         CLI   CURRMED,C'C'                                                     
         JE    NXTSLN02                                                         
                                                                                
         MVI   BYTE1,C'R'                                                       
         CLI   CURRMED,C'R'                                                     
         JE    NXTSLN02                                                         
         CLI   CURRMED,C'X'                                                     
         JNE   *+2                                                              
                                                                                
NXTSLN02 CLC   =C'00',0(R2)        FOUND DEFAULT ENTRY?                         
         JE    NXTSLN04                                                         
         CLC   LP_AGY,0(R2)        NO, MATCH ON AGENCY                          
         JNE   *+14                                                             
NXTSLN04 CLC   BYTE1,2(R2)         MATCH ON MEDIA                               
         JE    NXTSLN06                                                         
         JXLE  R2,RE,NXTSLN02                                                   
         DC    H'0'                                                             
                                                                                
NXTSLN06 AHI   R2,4                                                             
         J     NXTSLN10                                                         
                                                                                
NXTSLN08 XR    R2,R2               NOT FIRST TIME - POINT TO NEXT               
         ICM   R2,7,ANXTTAB                                                     
         AHI   R2,SLNTABQ                                                       
         XR    RF,RF                                                            
         IC    RF,CURRSLN                                                       
         LA    RF,1(RF)                                                         
         CHI   RF,255                                                           
         BH    NOMORE                                                           
         STC   RF,CURRSLN                                                       
                                                                                
NXTSLN10 STCM  R2,7,ANXTTAB        SAVE A(TABLE ENTRY)                          
         ST    R2,LP_ADATA                                                      
         CLI   1(R2),0             EQUIVALENCE OF 0 SHOULD BE SKIPPED           
         JE    NXTSLN08                                                         
         MVI   LP_RMODE,LP_RMORE                                                
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR REP NAMES DOWNLOAD                             *         
***********************************************************************         
                                                                                
ARYREPS  LKOUT A,(R,NXTREP),MULTIROW=Y,ROWNAME=CTEPRECD                         
RepCd    LKOUT C,1,CTEPKREP,CHAR                                                
         LKOUT C,2,(A,ARYREP1)                                                  
         LKOUT E                                                                
*                                                                               
ARYREP1  LKOUT A,(D,B#ERPREC,CTEPDAT),ROWID=(CTEPRD,CTEPRELQ),EOT=EOR, +        
               ROWWIDTH=(V,CTEPRLEN)                                            
RepNm    LKOUT C,2,CTEPPNAM,CHAR                                                
RepPf    LKOUT C,3,CTEPPREF,CHAR                                                
PRout    LKOUT P,,SETMBITS                                                      
Indic1   LKOUT C,4,CTEPFLG1,HEXD,ND=Y                                           
Indic2   LKOUT C,5,CTEPFLG2,HEXD,ND=Y                                           
         LKOUT E                                                                
*                                                                               
SETMBITS L     R1,LP_AINP          REP PARTNER PRIMARY INFO ELEM                
         USING CTEPRD,R1                                                        
*                                                                               
         L     RF,AERPREC          REP PARTNER RECORD                           
         CLC   =C'EOC',CTEPKREP-CTEPKEY(RF)  EOC (TV)?                          
         JE    *+10                                                             
         CLC   =C'ENC',CTEPKREP-CTEPKEY(RF)  EOC (RADIO)?                       
         JNE   *+8                                                              
         OI    CTEPFLG2,CTE2EOC    X'80' -SET END OF CONTRACT FLAG              
*                                                                               
         CLI   CTEPRMED,C'T'       MEDIA T?                                     
         JNE   *+8                                                              
         OI    CTEPFLG1,CTE1TV     X'04' TO MATCH WHAT IS IN DDDARETAB          
*                                                                               
         CLI   CTEPRMED,C'R'       MEDIA R?                                     
         JNE   *+8                                                              
         OI    CTEPFLG1,CTE1RAD    X'02' TO MATCH WHAT IS IN DDDARETAB          
*                                                                               
         B     EXITY                                                            
         DROP  R1                                                               
***********************************************************************         
* DOWNLOAD REP CODES                                                            
***********************************************************************         
                                                                                
NXTREP   GOTOR (#NXTREC,ANXTREC),DMCB,ERPKEYT,('B#ERPREC',0),          +        
               ('$NXTRGEN',SAVED),0,0                                           
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DARE ERROR DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYERRS  LKOUT A,(D,B#SVRDEF,ERRORLST),ROWID=(ERRVALS,0),EOT=0,        +        
               ROWWIDTH=(V,ERRLEN),NEWEL=B                                      
ErrCd    LKOUT C,1,ERRCODE,CHAR                                                 
ErrTx    LKOUT C,2,ERRTEXT,CHAR,LEN=V                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SPECIAL REP DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYSRPS  LKOUT A,(R,NXTSRP),MULTIROW=Y,ROWNAME=REPRECD                          
MedCd    LKOUT C,1,REPKMED,CHAR                                                 
RepCd    LKOUT C,2,REPKREP,CHAR                                                 
RepNm    LKOUT C,3,RNAME,CHAR                                                   
         LKOUT E                                                                
                                                                                
***********************************************************************         
* DOWNLOAD SPECIAL REPS                                               *         
***********************************************************************         
                                                                                
NXTSRP   GOTOR (#NXTREC,ANXTREC),DMCB,REPKEYT,('B#COMREC',0),          +        
               ('$NXTRSTA',SAVED),0,0                                           
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMO BOOK TYPES DOWNLOAD 2                     *         
***********************************************************************         
                                                                                
ARYBKT2  LKOUT A,(R,NXTBT2),MULTIROW=Y,ROWNAME=SPBKTYPD                         
BkTyp    LKOUT C,1,(D,B#SAVED,BKFILE),CHAR,ND=Y                                 
         LKOUT C,2,SPBKTYPA,CHAR,LEN=2                                          
         LKOUT C,3,SPBKTDES,CHAR,ND=Y                                           
         LKOUT C,4,(D,B#SAVED,BTFLAG),CHAR,ND=Y                                 
         LKOUT E                                                                
                                                                                
***********************************************************************         
* DOWNLOAD BOOK TYPES 2                                               *         
***********************************************************************         
                                                                                
NXTBT2   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTBT210                                                         
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,SPBOOKTB                                               
         ICM   R2,15,0(R1)                                                      
         JZ    *+2                                                              
         STCM  R2,7,ANXTTAB2       SAVE OFF BEGINNING OF TABLE FOR LOOP         
         L     R0,4(R1)            LENGTH OF EACH ENTRY                         
         STC   R0,NTRYLEN          HAS TO FIT IN 1 BYTE                         
         BRAS  RE,SETBTADR         POINT R1 TO BKTYTAB                          
         STCM  R1,7,ANXTTAB3                                                    
         J     NXTBT220                                                         
                                                                                
NXTBT210 XR    R2,R2               NOT FIRST TIME - POINT TO NEXT               
         ICM   R2,7,ANXTTAB                                                     
         LLC   R0,NTRYLEN                                                       
         AR    R2,R0                                                            
                                                                                
NXTBT220 STCM  R2,7,ANXTTAB        SAVE A(TABLE ENTRY)                          
         ICM   RF,7,ANXTTAB3                                                    
         ST    R2,LP_ADATA                                                      
         USING SPBKTYPD,R2                                                      
         USING BKTYD,RF                                                         
                                                                                
         CLI   0(R2),FF            TEST END OF TABLE                            
         JE    NXTBTBMP                                                         
***  REPLACED DUE TO FUTURE STORAGE PROTECTION VIOLATION                        
***      OC    SPFILIND,SPFILIND                                                
***      JZ    NXTBT210                                                         
***                                                                             
         CLC   SPFILIND,=XL4'00'                                                
         JE    NXTBT210                                                         
***  REPLACED DUE TO FUTURE STORAGE PROTECTION VIOLATION                        
         MVC   WORK(4),SPFILIND                                                 
         NC    WORK(4),BKTYBIT                                                  
         JZ    NXTBT210                                                         
                                                                                
         BRAS  R1,SETBTAD2         POINT RE TO BKTYTAB                          
         USING SPCBTD,RE                                                        
         MVI   BTFLAG,0            CLEAR BOOKTYPE FLAG                          
*                                                                               
NXTBT230 CLI   0(RE),X'FF'         EOT                                          
         JE    NXTBT250                                                         
         CLC   SPCBDES,SPBKTDES    MATCH ON BOOKTYPE DESCRPTION?                
         JE    NXTBT240                                                         
         AHI   RE,SPBTLQ           BUMP                                         
         J     NXTBT230                                                         
*                                                                               
NXTBT240 TM    SPCBTFLG,SPBTREM    REMOVED?                                     
         JNZ   NXTBT210             -YES, SKIP THIS BOOKTYPE                    
*                                                                               
         MVI   BTFLAG,0                                                         
         TM    SPCBTFLG,SPBTRES    RESEARCH?                                    
         JZ    NXTBT250                                                         
         CLC   LP_VRSN,V320015     REMOVED UNTIL V3.2.0.15                      
         JL    NXTBT210                                                         
         MVI   BTFLAG,BTRES        FLAG AS RESEARCH                             
                                                                                
NXTBT250 MVC   BKFILE,BKTY3C                                                    
         B     EXITY                                                            
         DROP  R2,RF,RE                                                         
                                                                                
NXTBTBMP DS    0H                                                               
         XR    R0,R0                                                            
         LA    R0,BKTYL            L'BKTYTAB = 7                                
         AR    RF,R0                                                            
         STCM  RF,7,ANXTTAB3                                                    
         CLI   0(RF),FF            TEST END OF BKTYTAB                          
         BE    NOMORE                                                           
         MVC   ANXTTAB,ANXTTAB2    RESET THE TABLE ENTRY                        
         J     NXTBT210                                                         
                                                                                
SETBTADR BASR  R1,RE               POINT R1 TO BKTYTAB AND RETURN               
                                                                                
BKTYTAB  DS    0CL7                                                             
         DC    CL3'TT',AL4(SP_NSI_TP_TV)                                        
         DC    CL3'TF',AL4(SP_NSI_TP_TV)                                        
         DC    CL3'WTP',AL4(SP_NSI_WTP_TV)                                      
         DC    CL3'OTP',AL4(SP_NSI_OTP_TV)                                      
         DC    CL3'RTP',AL4(SP_ARB_RTP_RADIO)                                   
         DC    CL3'RDP',AL4(SP_ARB_RTP_RADIO)                                   
         DC    CL3'TDP',AL4(SP_NSI_TDP_TV)                                      
         DC    CL3'RUA',AL4(SP_ARB_CTY_RADIO)                                   
         DC    CL3'TRT',AL4(SP_TRI_RTP_RADIO)  Triton Time Period               
         DC    CL3'TRD',AL4(SP_TRI_RTP_RADIO)         Daypart                   
         DC    CL3'TVD',AL4(SP_VID)            Videology Live only              
         DC    CL3'RDH',AL4(SP_IHT)            IHeart Time Period               
         DC    CL3'RTH',AL4(SP_IHT)            IHeart Daypart                   
         DC    CL3'XT ',AL4(SP_XT)  FAKE- SBTK NEEDED IT DUE TO BUG             
         DC    CL3'COM',AL4(SP_COM)            comScore                         
         DC    CL3'NCM',AL4(SP_NCM)            NCM                              
         DC    X'FF'                                                            
*                                                                               
SETBTAD2 BASR  RE,R1               POINT RE TO SPCBKTP AND RETURN               
                                                                                
SPCBKTP  DS    0X              SPECIAL BOOKTYPE RULES FOR SPOT DESKTOP          
*                                                                               
*                                  RESEARCH BOOKTYPES                           
         DC    CL25'DMA Prv/Spcl',AL1(SPBTRES)                                  
***      DC    CL25'Total DMA',AL1(SPBTRES)                                     
***********************************************************************         
* THE FOLLOWING 3 BOOKTYPES ARE NO LONGER RESEARCH       -HWON 04/26/17         
***********************************************************************         
***      DC    CL25'CABLE',AL1(SPBTRES)                                         
***      DC    CL25'LIVE CABLE',AL1(SPBTRES)                                    
***      DC    CL25'LIVE+3 CABLE',AL1(SPBTRES)                                  
*                                                                               
*                                  REMOVED BOOKTYPES                            
         DC    CL25'Standard 0 Cell',AL1(SPBTREM)                               
         DC    CL25'Hispanic 0 Cell',AL1(SPBTREM)                               
         DC    CL25'Cable 0 Cell',AL1(SPBTREM)                                  
         DC    CL25'Hardwired 0 Cell',AL1(SPBTREM)                              
         DC    CL25'Extended',AL1(SPBTREM)                                      
         DC    CL25'Special FoxNet',AL1(SPBTREM)                                
         DC    CL25'Live+3 Parent Only',AL1(SPBTREM)                            
         DC    CL25'Live+3 Wired Parent Only',AL1(SPBTREM)                      
*                                                                               
         DC    X'FF'                                                            
                                                                                
BKTYD    DSECT                                                                  
BKTY3C   DS    CL3                                                              
BKTYBIT  DS    AL4                                                              
BKTYL    EQU   *-BKTYD                                                          
                                                                                
SPCBTD   DSECT                                                                  
SPCBDES  DS    CL25                                                             
SPCBTFLG DS    X                                                                
SPBTREM  EQU   X'80'               REMOVED BOOKTYPES                            
SPBTRES  EQU   X'40'               RESEARCH BOOKTYPES                           
SPBTLQ   EQU   *-SPCBTD                                                         
                                                                                
SVRDEF   CSECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR SWEEP DOWNLOAD                                 *         
***********************************************************************         
                                                                                
ARYSWP   LKOUT A,(R,NXTSWP),MULTIROW=Y,ROWNAME=SWPVALS                          
Sweep    LKOUT C,1,SWPFILE,CHAR                                                 
         LKOUT C,2,SWPMED,CHAR                                                  
         LKOUT C,3,SWPSRCE,CHAR                                                 
         LKOUT C,4,SWPYEAR,CHAR                                                 
         LKOUT C,5,SWPMON,UBIN                                                  
         LKOUT C,6,SWPSDTE,EDAT                                                 
         LKOUT C,7,SWPEDTE,EDAT                                                 
         LKOUT E                                                                
                                                                                
***********************************************************************         
* DOWNLOAD SWEEP DATES FOR LAST 3 YEARS                               *         
***********************************************************************         
                                                                                
NXTSWP   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSWP10                                                         
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,SWEEPTBL                                               
         ICM   R2,15,0(R1)                                                      
         JZ    *+2                                                              
         STCM  R2,7,ANXTTAB        SAVE OFF BEGINNING OF TABLE FOR LOOP         
         L     R0,4(R1)            LENGTH OF EACH ENTRY                         
         STC   R0,NTRYLEN          HAS TO FIT IN 1 BYTE                         
                                                                                
         GOTOR VDATCON,DMCB,(5,0),(3,FULL)                                      
         ZIC   RF,FULL             SAVE OFF YEAR                                
         SHI   RF,4                DATA FOR 2012 AND FORWARD                    
         STC   RF,BYTE                                                          
         J     NXTSWP20                                                         
                                                                                
NXTSWP10 XR    R2,R2               NOT FIRST TIME - POINT TO NEXT               
         ICM   R2,7,ANXTTAB                                                     
         LLC   R0,NTRYLEN                                                       
         AR    R2,R0                                                            
                                                                                
         USING SWPTABD,R2                                                       
NXTSWP20 STCM  R2,7,ANXTTAB        SAVE A(TABLE ENTRY)                          
                                                                                
         LA    RF,SWPVALS                                                       
         ST    RF,LP_ADATA                                                      
         XC    SWPVALS(SWPVALL),SWPVALS                                         
                                                                                
         CLC   SWPTTYPE,=C'TTN'    TEST END OF TABLE                            
         JNE   NOMORE                                                           
         CLC   BYTE,SWPTYR         WITHIN REQUESTED PERIOD?                     
         JH    NXTSWP10                                                         
                                                                                
         MVC   SWPFILE,SWPTTYPE                                                 
         MVC   SWPMED,SWPTTYPE+1                                                
         MVC   SWPSRCE,SWPTTYPE+2                                               
         MVC   SWPMON,SWPTMO       MONTH                                        
         GOTOR VDATCON,DMCB,(10,SWPTSDTE),(0,SWPSDTE)  START DATE               
                                                                                
         CLI   SWPTNUM,X'01'       1 WEEK?                                      
         JNE   NXTSWP22                                                         
         GOTOR VADDAY,DMCB,SWPSDTE,SWPEDTE,F'6'                                 
NXTSWP22 CLI   SWPTNUM,X'02'       2 WEEK?                                      
         JNE   NXTSWP24                                                         
         GOTOR VADDAY,DMCB,SWPSDTE,SWPEDTE,F'13'                                
NXTSWP24 CLI   SWPTNUM,X'03'       3 WEEK?                                      
         JNE   NXTSWP26                                                         
         GOTOR VADDAY,DMCB,SWPSDTE,SWPEDTE,F'20'                                
NXTSWP26 CLI   SWPTNUM,X'04'       4 WEEK?                                      
         JNE   NXTSWP28                                                         
         GOTOR VADDAY,DMCB,SWPSDTE,SWPEDTE,F'27'                                
                                                                                
NXTSWP28 MVC   DUB(L'SWPEDTE),SWPEDTE      DEFAULT END DATE                     
         CLC   =C'12',SWPSDTE+2            DECEMBER?                            
         JNE   NXTSWP30                                                         
         GOTOR VADDAY,DMCB,SWPSDTE,DUB,F'15'                                    
                                                                                
NXTSWP30 GOTOR VDATCON,DMCB,(0,DUB),(23,DUB1)                                   
         MVC   SWPYEAR,DUB1        YEAR                                         
                                                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
SWPTABD  DSECT                                                                  
SWPTTYPE DS    CL3                 TTN                                          
SWPTYR   DS    XL1                 BINARY YEAR                                  
SWPTMO   DS    XL1                 BINARY MONTH                                 
SWPTNUM  DS    XL1                 # WEEK SWEEP                                 
SWPTSDTE DS    0CL10               START DATE                                   
SWPTYEAR DS    CL4                 YEAR                                         
         DS    CL1                                                              
SWPTMON  DS    CL2                 MONTH                                        
         DS    CL1                                                              
SWPTDAY  DS    CL2                 DAY                                          
         DS    CL1                                                              
                                                                                
SVRDEF   CSECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR DAYPART LIST ACROSS ALL DAYPART MENUS                    
***********************************************************************         
                                                                                
ARYDPTL  LKOUT A,(R,GETDPTL),MULTIROW=Y,ROWNAME=DPTLD,                 +        
               ROWWIDTH=DPTLDPTL-DPTLD                                          
                                                                                
MedCod   LKOUT C,1,DPTLMEDA,CHAR,ND=Y                                           
Array    LKOUT C,255,(A,ARYDPTS)                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYDPTS  LKOUT A,(*,DPTLDPTL),EOT=0,ROWNAME=DPTLD,ROWWIDTH=1                    
DptCode  LKOUT C,2,DPTLDPTL,CHAR,ND=Y                                           
         LKOUT E                                                                
***********************************************************************         
* GET ALL DAYPART CODES FOR EACH MEDIA                                          
*                                                                               
* FORMAT OF DPTTAB IS:                                                          
*      BYTE 0         MEDIA CODE                                                
*      BYTE 1 TO n    n DPT CODES                                               
*      BYTE n+1       X'00'                                                     
***********************************************************************         
                                                                                
GETDPTL  CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   GTDPTL50                                                         
*                                                                               
         LA    RE,DPTTAB           WE SHOULD HAVE MORE THAN ENUF ROOM           
         LA    RF,STAVALS-DPTTAB                                                
         XCEFL                                                                  
                                                                                
         LA    R3,DPTTAB           FOR INITIAL D/L, DPTTAB WILL CONTAIN         
         ST    R3,LP_ADATA             ALL THE DAYPARTS                         
         ST    R3,ADPTMEDA         A(DPT MEDIA) FOR DUPLICATE CHECKING          
         MVI   0(R3),X'FF'         IN CASE WE DO NOT HAVE ANY                   
                                                                                
         LA    R2,IOKEY                                                         
         USING DPTHDR,R2           BUILD KEY OF DAYPART MENU RECORD             
         XC    DPTKEY,DPTKEY                                                    
         MVI   DPTKTYPE,DPTKTYPQ                                                
         MVC   DPTKAGY,LP_AGY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         BNE   EXITY                                                            
GTDPTL00 CLC   DPTKEY(DPTKMED-DPTKEY),IOKEYSAV   HAS TO BE THE SAME AGY         
         JE    GTDPTL03                          YES                            
         MVI   0(R3),0                                                          
         MVI   1(R3),X'FF'                       NO MORE MEDIAS                 
         LA    R3,DPTTAB           WE'RE DONE GETTING THE DATA FROM MF          
         ST    R3,ADPTMEDA         NOW TO START SENDING IT TO SD                
         B     EXITY                                                            
                                                                                
GTDPTL03 CLC   DPTKEY(DPTKMENU-DPTKEY),IOKEYSAV  SAME MEDIA???                  
         JE    GTDPTL10                          YES                            
                                                                                
         CLI   IOKEYSAV+DPTKMED-DPTKEY,0    WAS MEDIA SET???                    
         JE    GTDPTL06                     NO, OUR FIRST MEDIA                 
         MVI   0(R3),0                      OTHERWISE PUT AN E-O-T              
         LR    RE,R3                                                            
         L     R1,ADPTMEDA                                                      
         LA    RF,2(R1)                                                         
         SR    RE,RF                                                            
         STC   RE,1(R1)            # OF DPTS SO FAR FOR THIS MEDIA              
         LA    R3,1(R3)                                                         
                                                                                
GTDPTL06 MVC   0(1,R3),DPTKMED                                                  
         ST    R3,ADPTMEDA         A(DPT MEDIA) FOR DUPLICATE CHECKING          
         LA    R3,2(R3)                                                         
                                                                                
GTDPTL10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         JNE   *+2                                                              
                                                                                
         L     R2,IOADDR           R2=A(DAYPART MENU RECORD)                    
         LA    R4,DPTCODES         R4=A(DAYPART LIST)                           
GTDPTL20 CLI   0(R4),0             TEST END OF DAYPART LIST                     
         JE    GTDPTL40                                                         
         MVC   BYTE1,0(R4)         COPY THIS AND SEE IF WE ALREADY HAVE         
                                                                                
         L     R1,ADPTMEDA         CHECK AGAINST ALL THE DPTS FOR MEDIA         
         LA    R1,2(R1)             TO MAKE SURE NOT A DUPLICATE                
GTDPTL25 CLI   0(R1),0                                                          
         JE    GTDPTL24            NOT A DUPLICATE                              
         CLC   0(1,R1),0(R4)                                                    
         JE    GTDPTL30            NEXT DPT AS WE ALREADY HAVE THIS ONE         
         LA    R1,1(R1)                                                         
         J     GTDPTL25                                                         
                                                                                
GTDPTL24 MVC   0(1,R3),0(R4)                                                    
         LA    R3,1(R3)                                                         
                                                                                
GTDPTL30 AHI   R4,5                BUMP TO NEXT DAYPART LIST ENTRY              
         J     GTDPTL20                                                         
                                                                                
GTDPTL40 LR    RE,R3                                                            
         L     R1,ADPTMEDA                                                      
         LA    RF,2(R1)                                                         
         SR    RE,RF                                                            
         STC   RE,1(R1)            # OF DPTS SO FAR FOR THIS MEDIA              
                                                                                
         LA    R2,IOKEY                                                         
         MVC   IOKEYSAV,IOKEY      SO THAT IOKEYSAV WILL HAVE A MEDIA           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO5'                               
         BNE   EXITY                                                            
         J     GTDPTL00                                                         
         DROP  R2                                                               
*                                                                               
GTDPTL50 L     R3,ADPTMEDA                                                      
         SR    RE,RE                                                            
         IC    RE,1(R3)                                                         
         LA    RF,3(RE,R3)         MEDIA + LEN + DPTS + X'00'                   
         CLI   0(RF),X'FF'         NO MORE MEDIAS WITH DAYPARTS                 
         BE    NOMORE                                                           
         ST    RF,LP_ADATA                                                      
         ST    RF,ADPTMEDA                                                      
         B     EXITY                                                            
                                                                                
DPTLD    DSECT                                                                  
DPTLMEDA DS    C                   MEDIA OF THIS DAYPART LIST                   
DPTLNDPT DS    XL1                 NUMBER OF DAYPARTS FOR THIS MEDIA            
DPTLDPTL DS    0C                  LIST OF DAYPARTS                             
         EJECT                                                                  
SVRDEF   CSECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR DEMO ALIAS TABLE DISPLAY                       *         
***********************************************************************         
                                                                                
ARYDMTD  LKOUT A,(R,GETDMTD),EOT=EOR,ROWNAME=DMEDTABD,                 +        
               ROWWIDTH=(V,DMEDLN),ROWID=(DMEDEL,DMEDELQ)                       
DemMe    LKOUT C,4,DMEDMED,CHAR                                                 
Array    LKOUT C,255,(A,ARYDMAL)                                                
         LKOUT E                                                                
                                                                                
ARYDMAL  LKOUT A,(*,DMEDALI),NROWS=*,ROWNAME=DMEDTABD,ROWWIDTH=DMEDALIL         
DemAli   LKOUT C,5,DMEDALI,CHAR,ND=Y                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* DISPLAY THE DEMO TABLE                                              *         
***********************************************************************         
                                                                                
GETDMTD  DS    0H                                                               
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   EXITY                                                            
                                                                                
***  IS IT ALREADY CLEARED?                                                     
         L     R0,AIO6                                                          
         LA    R1,4000                                                          
         SR    RF,RF                                                            
         LR    RE,RF                                                            
         MVCL  R0,RE                                                            
***  IS IT ALREADY CLEARED?                                                     
                                                                                
         USING DMEDTABD,R2                                                      
         L     R2,AIO6             THE TABLE IS IN AIO6                         
         ST    R2,LP_ADATA                                                      
****     ST    R2,ANXTTAB2         STORE THE BEGINNING OF AIO6                  
                                                                                
         BRAS  RE,SETMDADR         POINT R1 TO DMMEDTAB                         
GETDMT05 DS    0H                                                               
         MVI   DMEDEL,DMEDELQ                                                   
         MVC   DMEDMED,0(R1)       MOVE MEDIA FROM THE TABLE                    
                                                                                
         STCM  R2,7,ANXTTAB        SAVES OFF THE BEGINNING OF THE TABLE         
                                                                                
         BRAS  RE,GETDMCN                                                       
                                                                                
         LA    R1,1(R1)            BUMP THE MEDIA TABLE                         
         CLI   0(R1),0             DID WE HIT THE END YET                       
         JE    GETDMT10             - YES, GET OUT OF LOOP                      
         LLC   RE,DMEDLN                                                        
         AR    R2,RE                                                            
         J     GETDMT05                                                         
                                                                                
GETDMT10 DS    0H                                                               
         L     RE,AIO6                                                          
         LR    R2,RE                                                            
         LLC   R3,DMEDLN                                                        
         AR    R2,R3                                                            
         LR    R1,R2               SAVE OFF R2                                  
                                                                                
         LA    RE,DMEDMEDL(RE)     BUMP THESE 2 BYTES                           
         LA    R2,DMEDMEDL(R2)     WE'RE GONNA COMPARE THE ENTRIES              
                                                                                
         CLI   0(R2),C' '          SPACE OR LESS?                               
         JH    GETDMT15                                                         
         MVI   0(R1),0              - NULLIFY THE ENTRY                         
                                                                                
GETDMT15 AHI   R3,-(DMEDMEDL)                                                   
         LR    RF,R3                                                            
                                                                                
***  NOTE: R2 AND RE POINTS TO THE END AFTER CLCL!!  NEED TO RESET              
         CLCL  R2,RE               COMPARE THE DYNAMIC LENGTHS                  
         BNE   EXITY                                                            
***  NOTE: R2 AND RE POINTS TO THE END AFTER CLCL!!  NEED TO RESET              
                                                                                
         L     RE,AIO6                                                          
         LR    R2,RE                                                            
         LLC   R3,DMEDLN                                                        
         AR    RE,R3                                                            
                                                                                
         MVI   DMEDMED,C'*'        GOOD FOR ALL MEDIA                           
         MVI   0(RE),0             NULLIFY THE ENTRY                            
         B     EXITY                                                            
                                                                                
****   USE DEMOCON TO GET DEMO ALIASES                                          
                                                                                
GETDMCN  NTR1  LABEL=*                                                          
         MVI   CURROUT,2           CURRENT OUTPUT NUMBER                        
         ICM   R2,7,ANXTTAB        POINT TO THE FIRST ENTRY                     
                                                                                
GETDMC05 DS    0H                                                               
                                                                                
         USING DMWORKD,RC                                                       
         USING DBLOCKD,DMDBLOCK                                                 
GETDMC10 LA    R0,DMWORKD                                                       
         LHI   R1,DMWORKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   DBFILE,DEMOFILE     BUILD TABLE OF DEMO CODES & NAMES            
         MVC   DBSELMED,DMEDMED    MOVE IN THE MEDIA                            
         MVI   DBSELSRC,C'N'       IF TV                                        
         CLI   DMEDMED,C'R'                                                     
         JNE   *+8                                                              
         MVI   DBSELSRC,C'A'       IF RADIO                                     
*&&DO                                                                           
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,USAMEDQ    SET USA MEDIA                                
         L     RF,AAGYREC                                                       
****     LA    R3,MODLUSA          R3=A(LIST OF MODIFIERS)                      
         CLI   AGYPCNDA-AGYHDR(RF),CANAGYQ                                      
         JNE   *+8                                                              
****     JNE   *+12                                                             
****     LA    R3,MODLCAN                                                       
         MVI   DBSELMED,CANMEDQ    SET CANADIAN MEDIA                           
*&&                                                                             
         MVC   DBCOMFCS,ACOMFACS                                                
                                                                                
         LA    R2,DMEDMEDL(R2)     POINTS TO FIRST DEMO ALIAS AREA (2)          
GETDMC15 DS    0H                                                               
         XC    DMFLD,DMFLD         CLEAR OUT                                    
         XC    WORK,WORK           BUILD DEMO LOOK-UP EXPRESSION                
         MVI   WORK+1,C'I'         USE IMPRESSION                               
         MVC   WORK+2(1),CURRDEM                                                
         GOTOR VDEMOCON,DMCB,WORK,(CURROUT,DMFLD),(C'D',DBLOCK)                 
         CLI   WORK+1,C'*'         TEST BAD DEMO                                
         JE    *+2                                                              
         OC    DMFLD,SPACES        SPACE PAD FOR 20 BYTES                       
                                                                                
         XR    RF,RF                                                            
         ICM   RF,7,ANXTTAB        POINTS TO FIRST ALIAS TO COMPARE             
         LA    RF,DMEDMEDL(RF)                                                  
GETDMC17 CR    RF,R2                                                            
         JNL   GETDMC20            WE'RE DONE COMPARING                         
         CLC   DMFLD,0(RF)         IS IT THE SAME? (20 BYTES)                   
         JE    GETDMC30             - YUP, DON'T NEED IT                        
         LA    RF,DMEDALIL(RF)     COMPARE WITH THE NEXT ALIAS                  
         J     GETDMC17                                                         
                                                                                
GETDMC20 DS    0H                                                               
         MVI   DMFLDH,L'DMFLDH+L'DMFLD                                          
         MVI   DMFLDH+5,L'DMFLD    20 BYTES                                     
         GOTOR VDEMOVAL,DMCB,DMFLDH,(1,WORK2),DBLOCK                            
         CLI   DMCB+4,0            ANY ERRORS?                                  
         JE    GETDMC30             - YUP, DON'T NEED THIS ONE                  
                                                                                
         MVC   0(DMEDALIL,R2),DMFLD      MOVE IN THE ALIAS                      
         LA    R2,DMEDALIL(R2)     NEXT ALIAS AREA (20 BYTES)                   
                                                                                
GETDMC30 CLI   CURROUT,11                                                       
         JNL   GETDMC50                                                         
         XR    R1,R1                                                            
         IC    R1,CURROUT                                                       
GETDMC40 LA    R1,1(R1)                                                         
         CHI   R1,8                WE DON'T NEED 8                              
         JE    GETDMC40                                                         
         CHI   R1,10               OR 10                                        
         JE    GETDMC40                                                         
         STC   R1,CURROUT          BUMP THIS                                    
         J     GETDMC15            LET'S GET THE NEXT OUTPUT TYPE               
         DROP  RC                                                               
                                                                                
GETDMC50 DS    0H                                                               
                                                                                
         XR    RE,RE                                                            
         ICM   RE,7,ANXTTAB                                                     
         LR    R1,R2                                                            
         SR    R1,RE                                                            
         STC   R1,1(RE)                                                         
                                                                                
         B     EXITY                                                            
                                                                                
SETMDADR BASR  R1,RE               POINT R1 TO DMMEDTAB AND RETURN              
                                                                                
DMMEDTAB DS    0C                                                               
         DC    C'T',C'R'                                                        
         DC    X'00'                                                            
                                                                                
DMWORKD  DSECT                     ** GETDMCN LOCAL WORKING STORAGE **          
DMFLDH   DS    XL8                 DUMMY FIELD HEADER                           
DMFLD    DS    CL20                DUMMY INPUT FIELD                            
DMDBLOCK DS    XL(L'DBLOCK)        GETDMCN DBLOCK                               
DMWORKL  EQU   *-DMWORKD                                                        
                                                                                
DMEDTABD DSECT                                                                  
DMEDEL   DS    X                                                                
DMEDELQ  EQU   X'01'                                                            
DMEDLN   DS    X                                                                
DMEDMED  DS    C                                                                
DMEDMEDL EQU   *-DMEDTABD                                                       
DMEDALI  DS    CL20                                                             
DMEDALIL EQU   *-DMEDALI                                                        
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PURPOSE DOWNLOAD                               *         
***********************************************************************         
                                                                                
ARYPURP  LKOUT A,(R,NXTPRP),MULTIROW=Y,ROWNAME=PRPRECD                          
Media    LKOUT C,1,PRPKMED,CHAR                                                 
PCode    LKOUT C,2,PRPCODE,CHAR                                                 
PText    LKOUT C,3,PRPELTXT,CHAR                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* DOWNLOAD PURPOSE CODES                                              *         
***********************************************************************         
                                                                                
NXTPRP   GOTOR (#NXTREC,ANXTREC),DMCB,PRPKEYT,('B#COMREC',0),SAVED,0,0          
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION SOURCE DOWNLOAD                        *         
***********************************************************************         
                                                                                
ARYSTSR  LKOUT A,(R,NXTSTSR),MULTIROW=Y,ROWNAME=SRSRECD                         
Media    LKOUT C,1,SRSKMEDA,CHAR                                                
Prirty   LKOUT C,2,SRSPRTY,CHAR                                                 
SBand    LKOUT C,3,SRSKBAND,CHAR                                                
SCALL    LKOUT C,4,SRSKCALL,CHAR                                                
Array    LKOUT C,5,(A,ARYSRCE)                                                  
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY SOURCE LIST DOWNLOAD                                                    
***********************************************************************         
*&&DO                                                                           
ARYWNM   LKOUT A,(D,B#REVREC,DRVEL),ROWID=(RSNELD,RSNELQ),EOT=EOR,     +        
               ROWWIDTH=(V,RSNLEN)                                              
         LKOUT C,2,RSNAME,CHAR,LEN=V                                            
         LKOUT E                                                                
*&&                                                                             
ARYSRCE  LKOUT A,(D,B#STSREC,SRSFSTEL),ROWID=(RATSRCD,RATSRCQ),EOT=EOR,+        
               ROWWIDTH=(V,RATSRCLN)                                            
                                                                                
SrcList  LKOUT C,5,RATSOURC,CHAR,LEN=V                                          
                                                                                
         LKOUT E                                                                
***********************************************************************         
* DOWNLOAD STATION RATING SOURCE                                      *         
***********************************************************************         
                                                                                
NXTSTSR  DS    0H                                                               
         MVC   LP_ADATA,AIO3                                                    
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSS05                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         USING SRSRECD,R2          BUILD KEY OF DAYPART MENU RECORD             
         XC    SRSKEY,SRSKEY                                                    
         MVI   SRSKSYS,SRSKSYSQ                                                 
         MVI   SRSKTYP,SRSKTYPQ                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOBHI+IOGENDIR+B#STSREC'                      
         JE    NXTSS10                                                          
         DC    H'0'                                                             
*                                                                               
NXTSS05  GOTOR (#IOEXEC,AIOEXEC),'IOBSQ+IOGENDIR+B#STSREC'                      
         JNE   *+2                                                              
*                                                                               
NXTSS10  CLC   IOKEY(2),IOKEYSAV                                                
         JNE   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOGENFIL+B#STSREC'                     
                                                                                
         J     MORE                                                             
***********************************************************************         
* ARRAY DEFINITION FOR REASON CODES DOWNLOAD                          *         
***********************************************************************         
                                                                                
ARYREAS  LKOUT A,(R,NXTRSN),MULTIROW=Y,ROWNAME=RSNRECD                          
Media    LKOUT C,1,RSNKMED,CHAR                                                 
PCode    LKOUT C,2,RSNKCODE,CHAR                                                
OCode    LKOUT C,3,RSNKOFF,CHAR,ND=Y                                            
PText    LKOUT C,4,RSNTEXT,CHAR                                                 
         LKOUT E                                                                
                                                                                
***********************************************************************         
* DOWNLOAD REASON CODES                                               *         
***********************************************************************         
                                                                                
NXTRSN   GOTOR (#NXTREC,ANXTREC),DMCB,RSNKEYT,('B#COMREC',0),SAVED,0,0          
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* MARKET DOWNLOAD                                                     *         
***********************************************************************         
                                                                                
REQMKT   LKREQ H,I#SDMKTD,OUTSDM,NEXTREQ=REQSDB                                 
Media    LKREQ F,1,(D,B#SAVED,MEDIA),CHAR,TEXT=SP#MED,COL=*                     
         LKREQ E                                                                
                                                                                
OUTSDM   LKOUT H                                                                
SDMKTC   LKOUT R,X'0018'                                                        
Array    LKOUT C,X'0018',(A,ARYSDMK)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MARKET DOWNLOAD                                *         
***********************************************************************         
                                                                                
ARYSDMK  LKOUT A,(R,NXTMKT),MULTIROW=Y,ROWNAME=MKTREC                           
MktNo    LKOUT C,1,MKTKMKT,CHAR                                                 
MktNm    LKOUT C,2,MKTNAME,CHAR                                                 
Array    LKOUT C,3,(A,ARYMKTA)                                                  
MkAAMkt  LKOUT C,4,(D,B#SAVED,STAAAMK),LBIN,ND=Y,PCVERSION=4.5.0.63             
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET MARKET RECORDS                                                  *         
***********************************************************************         
                                                                                
NXTMKT   GOTOR (#NXTREC,ANXTREC),DMCB,MKTKEYT,('B#MKTREC',0),          +        
               ('$NXTRSTA',SAVED),0,0                                           
         L     R1,AIO3                                                          
         USING MKTRECD,R1                                                       
         XC    STAAAMK,STAAAMK     SET MEDIAOCEAN AUTOMATED AVAIL MKT #         
         LA    RE,C'0'                                                          
         CLI   MKTKMED,C'T'        MEDIA T, WE NEED NIELSEN                     
         JE    NXTMKT20                                                         
         LA    RE,C'1'                                                          
         CLI   MKTKMED,C'R'        MEDIA R OR X, WE NEED ARBITRON               
         JE    NXTMKT20                                                         
         CLI   MKTKMED,C'X'                                                     
         JNE   NXTMKTX             WHAT ELSE TO USE OTHERWISE?                  
*                                                                               
NXTMKT20 DS    0H                                                               
         LA    RF,MKTRSM1                                                       
         CLM   RE,1,MKTRS1         MATCH ON THIS RATING SERVICE?                
         JE    NXTMKT25                                                         
         LA    RF,MKTRSM2                                                       
         CLM   RE,1,MKTRS2         OR THIS ONE?                                 
         JNE   NXTMKTX                                                          
NXTMKT25 MVC   STAAAMK,0(RF)       YES, USE THAT FOR THE AA MARKET              
*                                                                               
NXTMKTX  B     EXITY                                                            
         DROP  R1                                                               
***********************************************************************         
* ARRAY DEFINITION FOR MARKET ALPHA CODES                             *         
***********************************************************************         
                                                                                
ARYMKTA  LKOUT A,(D,B#MKTREC,MKTALST),NROWS=L'MKTALST/3,ROWWIDTH=3              
MktCd    LKOUT C,3,MKTALST,CHAR,LEN=3,ND=Y                                      
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR FLIGHT DOWNLOAD                              *           
***********************************************************************         
                                                                                
ARYFLT   LKOUT A,(R,NXTFLT),MULTIROW=Y,ROWNAME=DFLRECD                          
                                                                                
EstNo    LKOUT C,5,DFLKEST,LBIN                                                 
Array    LKOUT C,255,(A,ARYFINV)                                                
Array    LKOUT C,255,(A,ARYFLTV)                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET FLIGHT RECORDS FOR BUY/GOAL DOWNLOAD                            *         
***********************************************************************         
                                                                                
NXTFLT   GOTOR (#NXTREC,ANXTREC),DMCB,FLTKEYT,('B#FLTREC',0),SAVED,0,0          
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR FLIGHT DETAILS DOWNLOAD                        *         
***********************************************************************         
                                                                                
ARYFLTV  LKOUT A,(D,B#FLTREC,DFLEL),EOT=EOR,                           +        
               ROWID=(DFFLTEL,DFFLTELQ),ROWWIDTH=(V,DFFLTLEN)                   
                                                                                
FltNo    LKOUT C,2,DFFLTNUM,UBIN                                                
StrDt    LKOUT C,3,DFFLTSTR,BDAT                                                
EndDt    LKOUT C,4,DFFLTEND,BDAT                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR FLIGHT INFO DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYFINV  LKOUT A,(D,B#FLTREC,DFLEL),EOT=EOR,                           +        
               ROWID=(DFINFEL,DFINFELQ),ROWWIDTH=(V,DFINFLEN)                   
                                                                                
FNFSD    LKOUT C,1,DFINFSDT,BDAT                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* BUY DOWNLOAD REQUEST X'0140'                                        *         
***********************************************************************         
                                                                                
REQSDB   LKREQ H,I#SDBUYD,OUTSDB,NEXTREQ=REQNSB                                 
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),             +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
PrdCd    LKREQ F,3,(I,B#SAVED,PRDIND),(R,VALPRD),LIST=NOD,             +        
               OLEN=L'BUYKPRD,MAXLEN=L'PKEYPRD,TEXT=SP#PRO,COL=*                
EstNo    LKREQ F,4,(I,B#SAVED,ESTIND),LBIN,LIST=NOD,RANGE=Y,           +        
               DEFAULT=NOT,OLEN=L'EKEYEST,TEXT=SP#EST,COL=*                     
EstFt    LKREQ F,5,(D,B#WORKD,QPOLFLT),CHAR,LOWERCASE=Y,TEXT=SP#ESTFI, +        
               COL=*                                                            
SDate    LKREQ F,6,(D,B#WORKD,QSTRDATE),EDAT,TEXT=SP#STDT,COL=*                 
EDate    LKREQ F,7,(D,B#WORKD,QENDDATE),EDAT,TEXT=SP#ENDT,COL=*                 
Demos    LKREQ F,8,(I,B#SAVED,DEMIND),(U,#VALDCD,$VALDCD),LIST=NOD,    +        
               SORT=N,OLEN=L'EDEMLIST,MAXLIST=EDEMLSTN,                +        
               MAXLEN=4,TEXT=SP#DEMO,COL=*                                      
MktNo    LKREQ F,9,(I,B#SAVED,MKTIND),(R,VALMKT),OLEN=L'BUYKMKTN,      +        
               LIST=NOD,DEFAULT=NOT,TEXT=SP#MKT,COL=*                           
StNet    LKREQ F,10,(I,B#SAVED,STAIND),(R,VALSTM),LIST=NOD,DEFAULT=Y,  +        
               OLEN=L'BUYKMSTA,MAXLEN=STASTAL,TEXT=SP#STA,COL=*                 
BOnly    LKREQ F,11,(D,B#SAVED,BRDSONLY),HDRO,TEXT=SP#BRDOY,COL=*               
DlGol    LKREQ F,12,(D,B#SAVED,DLDGOALS),HDRO,TEXT=SP#DLGOL,COL=*               
DlAfd    LKREQ F,13,(D,B#SAVED,DLDAFFID),HDRO,TEXT=SP#DLAFD,COL=*               
DlClr    LKREQ F,20,(D,B#SAVED,DLDCLEAR),HDRO,TEXT=SP#DLCLR,COL=*               
DlMLk    LKREQ F,21,(D,B#SAVED,DLDMKTLK),HDRO,TEXT=SP#DLMLI,COL=*               
DlSLk    LKREQ F,22,(D,B#SAVED,DLDSTALK),HDRO,TEXT=SP#DLSLI,COL=*               
DlByS    LKREQ F,24,(D,B#SAVED,DLDBYSTA),HDRO,TEXT=SP#DLBYS,COL=*               
XPaid    LKREQ F,14,(D,B#SAVED,XPAIDS),HDRO,TEXT=SP#XPAID,COL=*                 
XMiss    LKREQ F,15,(D,B#SAVED,XMISSD),HDRO,TEXT=SP#XMISS,COL=*                 
XMkGd    LKREQ F,16,(D,B#SAVED,XMGOOD),HDRO,TEXT=SP#XMKGD,COL=*                 
XZero    LKREQ F,17,(D,B#SAVED,XZERO$),HDRO,TEXT=SP#XZERO,COL=*                 
XPAlc    LKREQ F,18,(D,B#SAVED,XPALOC),HDRO,TEXT=SP#XPALO,COL=*                 
XSpil    LKREQ F,19,(D,B#SAVED,XSPILL),HDRO,TEXT=SP#XSPIL,COL=*                 
         LKREQ E                                                                
                                                                                
OUTSDB   LKOUT H                   ** BUY DOWNLOAD **                           
                                                                                
SDBCLT   LKOUT R,X'0042'           CLIENT (AND RELATED) RECORD                  
CltCd    LKOUT C,1,(D,B#WORKD,QCLTA),CHAR                                       
CltNm    LKOUT C,2,(D,B#SAVED,CLTNAME),CHAR                                     
PolTy    LKOUT C,4,(D,B#SAVED,CLTBPOL),HDRO,ND=Y                                
*                                                                               
         LKOUT C,5,(D,B#SAVED,PROF00V+00),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+01),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+02),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+03),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+04),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+05),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+06),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+07),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+08),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#TWAD,SVS002DP),CHAR,LEN=1                               
         LKOUT C,5,(D,B#SAVED,PROF00V+10),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+11),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+12),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+13),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+14),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,6,(D,B#SAVED,PROF1WV+00),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+01),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+02),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+03),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+04),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+05),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+06),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+07),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+08),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+09),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+10),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+11),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+12),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+13),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+14),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,7,(D,B#SAVED,PROFB0V+00),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+01),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+02),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+03),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+04),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+05),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+06),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+07),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+08),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+09),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+10),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+11),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+12),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+13),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+14),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,8,(D,B#SAVED,PROFSTV+00),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+01),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+02),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+03),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+04),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+05),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+06),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+07),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+08),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+09),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+10),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+11),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+12),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+13),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+14),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,9,(D,B#SAVED,PROFAJV+00),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+01),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+02),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+03),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+04),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+05),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+06),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+07),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+08),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+09),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+10),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+11),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+12),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+13),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+14),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,18,(D,B#SAVED,PROFDARV+00),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+01),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+02),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+03),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+04),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+05),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+06),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+07),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+08),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+09),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+10),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+11),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+12),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+13),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+14),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+15),CHAR,LEN=1                          
*                                                                               
         LKOUT C,19,(D,B#SAVED,PROF00A+00),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+01),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+02),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+03),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+04),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+05),CHAR,LEN=1                           
         LKOUT C,19,(D,B#TWAD,SVS00A2DI),CHAR,LEN=1                             
         LKOUT C,19,(D,B#SAVED,PROF00A+07),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+08),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+09),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+10),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+11),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+12),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+13),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+14),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+15),CHAR,LEN=1                           
*                                                                               
         LKOUT C,20,(D,B#SAVED,PROFBW+00),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+01),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+02),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+03),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+04),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+05),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+06),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+07),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+08),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+09),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+10),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+11),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+12),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+13),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+14),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,21,(D,B#SAVED,PROFBWA+00),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+01),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+02),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+03),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+04),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+05),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+06),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+07),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+08),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+09),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+10),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+11),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+12),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+13),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+14),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+15),CHAR,LEN=1                           
*                                                                               
         LKOUT C,22,(D,B#SAVED,PROFOM+00),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+01),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+02),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+03),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+04),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+05),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+06),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+07),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+08),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+09),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+10),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+11),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+12),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+13),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+14),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+15),CHAR,LEN=1                            
*                                                                               
Inds.    LKOUT C,10,(D,B#SAVED,CLTINDS),HEXD                                    
ACtrl    LKOUT C,11,(D,B#SAVED,CLTADJC),CHAR                                    
RCtrl    LKOUT C,12,(D,B#SAVED,CLTRCTL),CHAR                                    
IDReq    LKOUT C,13,(D,B#SAVED,CLTIDRQ),CHAR                                    
C2Fac    LKOUT C,14,(D,B#SAVED,CLTC2FAC),SPAK                                   
FrzMo    LKOUT C,15,(D,B#SAVED,CLTFRZMO),BMON                                   
Array    LKOUT C,255,(A,ARYEQU)                                                 
CAcOf    LKOUT C,22,(D,B#SAVED,CLTACCOF),CHAR,ND=Y,PCVERSION=3.0.0.120          
CAcAg    LKOUT C,23,(D,B#SAVED,CLTACCAG),CHAR,ND=Y,PCVERSION=3.0.0.120          
         LKOUT E                                                                
                                                                                
SDBPRD   LKOUT R,X'0044'           PRODUCT RECORDS                              
Array    LKOUT C,255,(A,ARYPRD)                                                 
         LKOUT E                                                                
                                                                                
SDBFLT   LKOUT R,X'0047'           FLIGHT RECORDS                               
Array    LKOUT C,255,(A,ARYFLT)                                                 
         LKOUT E                                                                
                                                                                
SDBDPT   LKOUT R,X'0049'           DAYPART RECORD                               
Array    LKOUT C,255,(A,ARYSDDP)                                                
         LKOUT E                                                                
                                                                                
SDBBUY   LKOUT R,1                 BUY RECORDS                                  
Array    LKOUT C,255,(A,ARYBUY)                                                 
         LKOUT E                                                                
                                                                                
SDBMKN   LKOUT R,X'0050'           SEND MARKET NAME IF NO BUYS                  
Array    LKOUT C,255,(A,ARYSMN),FILTROUT=TSTMARKN                               
         LKOUT E                                                                
                                                                                
SDBGOL   LKOUT R,X'0060'           GOALS AND MARKET LOCKIN RECORDS              
Array    LKOUT C,255,(A,ARYSDG)                                                 
         LKOUT E                                                                
                                                                                
SDBCLR   LKOUT R,X'0063'           CLEARANCE RECORDS                            
Array    LKOUT C,255,(A,ARYCLR),FILTROUT=TSTCLEAR                               
         LKOUT E                                                                
                                                                                
SDBSLK   LKOUT R,X'0092'           STATION LOCKIN RECORDS                       
Array    LKOUT C,255,(A,ARYSLK),FILTROUT=TSTSTALK                               
         LKOUT E                                                                
                                                                                
SDBREV   LKOUT R,X'0095'           REVSHEET LIST                                
Array    LKOUT C,255,(A,ARYREV),PCVERSION=3.0.0.0                               
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
TSTMARKN TM    RUNI1,RUNIMARK      TEST MARKET NAME SENT                        
         BR    RE                                                               
                                                                                
TSTCLEAR CLI   DLDCLEAR,0          TEST DOWNLOAD CLEARANCE DATA                 
         B     SETCCC                                                           
                                                                                
TSTSTALK CLI   DLDSTALK,0          TEST DOWNLOAD STATION LOCKIN DATA            
         B     SETCCC                                                           
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MARKET NAME DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYSMN   LKOUT A,(R,GETMKN),NROWS=1,ROWNAME=STAVALS                             
                                                                                
MktNo    LKOUT C,2,STAMKT#,LBIN,ND=Y                                            
MktNm    LKOUT C,4,STAMKTN,CHAR,ND=Y                                            
MktAC    LKOUT C,11,STAAMKT,CHAR,ND=Y,PCVERSION=2.5.0.13                        
MkLPM    LKOUT C,12,STALPMD,CDAT,ND=Y,PCVERSION=2.6.0.0                         
MkCDm    LKOUT C,13,STACDEM,CHAR,ND=Y,PCVERSION=2.6.0.0                         
MktTmZ   LKOUT C,14,STAMKTZ,CHAR,PCVERSION=3.1.0.111                            
MktBkTyp LKOUT C,15,STAMKBT,(R,TRNSBKT),ND=Y,PCVERSION=3.3.0.6                  
MktAAMkt LKOUT C,16,STAAAMK,LBIN,PCVERSION=4.5.0.63                             
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO GET MARKET NAME - THIS IS FOR THE CASE WHERE THERE ARE   *         
* NO BUY RECORDS DOWNLOADED FOR THE REQUEST                           *         
***********************************************************************         
                                                                                
GETMKN   LA    R0,STAVALS                                                       
         ST    R0,LP_ADATA                                                      
         ICM   R1,7,AMKT                                                        
         AHI   R1,LW_LN1Q                                                       
         MVC   STAMKT#,0(R1)                                                    
         GOTOR GETMKT                                                           
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* NEW BUY DOWNLOAD REQUEST X'014A'                                    *         
***********************************************************************         
                                                                                
REQNSB   LKREQ H,I#NSDBYD,OUTNSB,NEXTREQ=REQNGL                                 
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),             +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
PrdCd    LKREQ F,3,(I,B#SAVED,PRDIND),(R,VALPRD),LIST=NOD,             +        
               OLEN=L'BUYKPRD,MAXLEN=L'PKEYPRD,TEXT=SP#PRO,COL=*                
EstNo    LKREQ F,4,(I,B#SAVED,ESTIND),LBIN,LIST=NOD,RANGE=Y,           +        
               DEFAULT=NOT,OLEN=L'EKEYEST,TEXT=SP#EST,COL=*                     
EstFt    LKREQ F,5,(D,B#WORKD,QPOLFLT),CHAR,LOWERCASE=Y,TEXT=SP#ESTFI, +        
               COL=*                                                            
SDate    LKREQ F,6,(D,B#WORKD,QSTRDATE),EDAT,TEXT=SP#STDT,COL=*                 
EDate    LKREQ F,7,(D,B#WORKD,QENDDATE),EDAT,TEXT=SP#ENDT,COL=*                 
Demos    LKREQ F,8,(I,B#SAVED,DEMIND),(U,#VALDCD,$VALDCD),LIST=NOD,    +        
               SORT=N,OLEN=L'EDEMLIST,MAXLIST=EDEMLSTN,                +        
               MAXLEN=4,TEXT=SP#DEMO,COL=*                                      
MktNo    LKREQ F,9,(I,B#SAVED,MKTIND),(R,VALMKT),OLEN=L'BUYKMKTN,      +        
               LIST=NOD,DEFAULT=NOT,TEXT=SP#MKT,COL=*                           
StNet    LKREQ F,10,(I,B#SAVED,STAIND),(R,VALSTM),LIST=NOD,DEFAULT=Y,  +        
               OLEN=L'BUYKMSTA,MAXLEN=STASTAL,TEXT=SP#STA,COL=*                 
BOnly    LKREQ F,11,(D,B#SAVED,BRDSONLY),HDRO,TEXT=SP#BRDOY,COL=*               
DlGol    LKREQ F,12,(D,B#SAVED,DLDGOALS),HDRO,TEXT=SP#DLGOL,COL=*               
DlAfd    LKREQ F,13,(D,B#SAVED,DLDAFFID),HDRO,TEXT=SP#DLAFD,COL=*               
DlClr    LKREQ F,20,(D,B#SAVED,DLDCLEAR),HDRO,TEXT=SP#DLCLR,COL=*               
DlMLk    LKREQ F,21,(D,B#SAVED,DLDMKTLK),HDRO,TEXT=SP#DLMLI,COL=*               
DlSLk    LKREQ F,22,(D,B#SAVED,DLDSTALK),HDRO,TEXT=SP#DLSLI,COL=*               
DlSLs    LKREQ F,23,(D,B#SAVED,DLDSTALS),HDRO,TEXT=SP#DLSLS,COL=*               
DlByS    LKREQ F,24,(D,B#SAVED,DLDBYSTA),HDRO,TEXT=SP#DLBYS,COL=*               
DlSpl    LKREQ F,25,(D,B#SAVED,DLDSPLST),HDRO,TEXT=(*,TDLONSPL),COL=*           
XPaid    LKREQ F,14,(D,B#SAVED,XPAIDS),HDRO,TEXT=SP#XPAID,COL=*                 
XMiss    LKREQ F,15,(D,B#SAVED,XMISSD),HDRO,TEXT=SP#XMISS,COL=*                 
XMkGd    LKREQ F,16,(D,B#SAVED,XMGOOD),HDRO,TEXT=SP#XMKGD,COL=*                 
XZero    LKREQ F,17,(D,B#SAVED,XZERO$),HDRO,TEXT=SP#XZERO,COL=*                 
XPAlc    LKREQ F,18,(D,B#SAVED,XPALOC),HDRO,TEXT=SP#XPALO,COL=*                 
XSpil    LKREQ F,19,(D,B#SAVED,XSPILL),HDRO,TEXT=SP#XSPIL,COL=*                 
XBuys    LKREQ F,26,(D,B#SAVED,XBUYS),HDRO,TEXT=(*,TEXCLBUY),COL=*              
         LKREQ E                                                                
                                                                                
OUTNSB   LKOUT H                   ** NEW BUY DOWNLOAD **                       
                                                                                
NSBCLT   LKOUT R,X'0042'           CLIENT (AND RELATED) RECORD                  
CltCd    LKOUT C,1,(D,B#WORKD,QCLTA),CHAR                                       
CltNm    LKOUT C,2,(D,B#SAVED,CLTNAME),CHAR                                     
PolTy    LKOUT C,4,(D,B#SAVED,CLTBPOL),HDRO,ND=Y                                
*                                                                               
         LKOUT C,5,(D,B#SAVED,PROF00V+00),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+01),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+02),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+03),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+04),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+05),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+06),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+07),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+08),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#TWAD,SVS002DP),CHAR,LEN=1                               
         LKOUT C,5,(D,B#SAVED,PROF00V+10),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+11),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+12),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+13),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+14),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,6,(D,B#SAVED,PROF1WV+00),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+01),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+02),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+03),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+04),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+05),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+06),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+07),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+08),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+09),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+10),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+11),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+12),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+13),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+14),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,7,(D,B#SAVED,PROFB0V+00),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+01),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+02),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+03),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+04),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+05),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+06),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+07),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+08),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+09),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+10),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+11),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+12),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+13),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+14),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,8,(D,B#SAVED,PROFSTV+00),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+01),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+02),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+03),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+04),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+05),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+06),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+07),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+08),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+09),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+10),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+11),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+12),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+13),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+14),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,9,(D,B#SAVED,PROFAJV+00),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+01),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+02),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+03),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+04),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+05),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+06),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+07),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+08),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+09),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+10),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+11),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+12),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+13),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+14),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,18,(D,B#SAVED,PROFDARV+00),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+01),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+02),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+03),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+04),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+05),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+06),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+07),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+08),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+09),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+10),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+11),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+12),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+13),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+14),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+15),CHAR,LEN=1                          
*                                                                               
         LKOUT C,19,(D,B#SAVED,PROF00A+00),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+01),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+02),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+03),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+04),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+05),CHAR,LEN=1                           
         LKOUT C,19,(D,B#TWAD,SVS00A2DI),CHAR,LEN=1                             
         LKOUT C,19,(D,B#SAVED,PROF00A+07),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+08),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+09),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+10),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+11),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+12),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+13),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+14),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+15),CHAR,LEN=1                           
*                                                                               
         LKOUT C,20,(D,B#SAVED,PROFBW+00),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+01),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+02),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+03),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+04),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+05),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+06),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+07),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+08),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+09),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+10),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+11),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+12),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+13),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+14),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,21,(D,B#SAVED,PROFBWA+00),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+01),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+02),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+03),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+04),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+05),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+06),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+07),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+08),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+09),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+10),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+11),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+12),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+13),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+14),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+15),CHAR,LEN=1                           
*                                                                               
         LKOUT C,22,(D,B#SAVED,PROFOM+00),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+01),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+02),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+03),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+04),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+05),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+06),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+07),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+08),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+09),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+10),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+11),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+12),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+13),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+14),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+15),CHAR,LEN=1                            
Inds.    LKOUT C,10,(D,B#SAVED,CLTINDS),HEXD                                    
ACtrl    LKOUT C,11,(D,B#SAVED,CLTADJC),CHAR                                    
RCtrl    LKOUT C,12,(D,B#SAVED,CLTRCTL),CHAR                                    
IDReq    LKOUT C,13,(D,B#SAVED,CLTIDRQ),CHAR                                    
C2Fac    LKOUT C,14,(D,B#SAVED,CLTC2FAC),SPAK                                   
FrzMo    LKOUT C,15,(D,B#SAVED,CLTFRZMO),BMON                                   
Array    LKOUT C,255,(A,ARYEQU)                                                 
CAcOf    LKOUT C,22,(D,B#SAVED,CLTACCOF),CHAR,ND=Y,PCVERSION=3.0.0.120          
CAcAg    LKOUT C,23,(D,B#SAVED,CLTACCAG),CHAR,ND=Y,PCVERSION=3.0.0.120          
         LKOUT E                                                                
                                                                                
NSBPRD   LKOUT R,X'0044'           PRODUCT RECORDS                              
Array    LKOUT C,255,(A,ARYPRD)                                                 
         LKOUT E                                                                
                                                                                
NSBFLT   LKOUT R,X'0047'           FLIGHT RECORDS                               
Array    LKOUT C,255,(A,ARYFLT)                                                 
         LKOUT E                                                                
                                                                                
NSBDPT   LKOUT R,X'0049'           DAYPART RECORD                               
Array    LKOUT C,255,(A,ARYSDDP)                                                
         LKOUT E                                                                
                                                                                
NSBMKT   LKOUT R,X'0018'           MARKET RECORD                                
MktNo    LKOUT C,1,(D,B#SAVED,STAMKT#),LBIN,FILTROUT=TSTONEST,ND=Y              
         LKOUT E                                                                
                                                                                
NSBBUY   LKOUT R,1                 BUY RECORDS                                  
Array    LKOUT C,255,(A,ARYBUY),FILTROUT=TSTXBUYS                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* THE SETUP BELOW SEEMS CONFUSING AND REDUNDANT      -HWON 11/15/2016           
* LINE1- IF NOT D/L SPILL & D/L STATION LIST, THEN ARYMST                       
* LINE2- IF CLT HAS BUYID & NOT D/L SPILL & D/L STATN-LIST, THEN ARYSTQ         
* LINE3- IF D/L SPILL, THEN ARYMST                                              
*                                                                               
* BASED ON THE RULES ABOVE, WE EITHER, IF STATION D/L, CALL ARYMST ONCE         
* & NOT THE 2ND||OR IF SPILL DOWNLOAD, 1ST IS SKIPPED AND 2ND IS CALLED         
* SO WHY NOT CHG IT TO CALL ARYMST ONCE, FOR BOTH D/L STATION OR D/L            
***********************************************************************         
NSBMST   LKOUT R,X'0050'           STATIONS IN MARKET RECORD                    
Array    LKOUT C,255,(A,ARYMST),FILTROUT=TSTSTALS                               
**Array    LKOUT C,255,(A,ARYSTQ),FILTROUT=TSTCBYID                             
Array    LKOUT C,255,(A,ARYMST),FILTROUT=TSTSPLST                               
         LKOUT E                                                                
                                                                                
NSBRAD   LKOUT R,X'0059'           MSTREET STATION RECORDS                      
Array    LKOUT C,255,(A,ARYRAD),PCVERSION=3.2.0.16                              
         LKOUT E                                                                
                                                                                
NSBSMK   LKOUT R,X'0051'           SPILL MARKET CODE, NAME, ALPHA               
Array    LKOUT C,255,(A,ARYSPM),PCVERSION=3.0.0.131                             
         LKOUT E                                                                
                                                                                
NSBGOL   LKOUT R,X'0060'           GOALS AND MARKET LOCKIN RECORDS              
Array    LKOUT C,255,(A,ARYSDG)                                                 
         LKOUT E                                                                
                                                                                
NSBCLR   LKOUT R,X'0063'           CLEARANCE RECORDS                            
Array    LKOUT C,255,(A,ARYCLR),FILTROUT=TSTCLEAR                               
         LKOUT E                                                                
                                                                                
NSBSLK   LKOUT R,X'0092'           STATION LOCKIN RECORDS                       
Array    LKOUT C,255,(A,ARYSLK),FILTROUT=TSTSTALK                               
         LKOUT E                                                                
                                                                                
NSBREV   LKOUT R,X'0095'           REVSHEET LIST                                
Array    LKOUT C,255,(A,ARYREV),PCVERSION=3.0.0.0                               
         LKOUT E                                                                
                                                                                
SDBWRK   LKOUT R,X'0096'           WORKSHEET LIST                               
Array    LKOUT C,255,(A,ARYWRK),PCVERSION=3.0.0.0                               
         LKOUT E                                                                
                                                                                
SDBDEV   LKOUT R,X'03D0'           SCHEDULED GUIDELINES                         
Array    LKOUT C,255,(A,ARYSGL),PCVERSION=3.1.0.20                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
TSTSTALS CLI   DLDSPLST,0          TEST ONLY SPILL WANTED                       
         BNE   RTRNNO              YES, WE DON'T WANT STATION LIST              
         CLI   DLDSTALS,0          TEST DOWNLOAD STATION LIST DATA              
         B     SETCCC                                                           
                                                                                
TSTSPLST CLI   DLDSPLST,0          TEST DOWNLOAD ONLY SPILL STATIONS            
         B     SETCCC                                                           
                                                                                
TSTCBYID CLI   CLTIDRQ,C'N'        TEST CLIENT BUY ID REQUIRED                  
         BE    RTRNNO              NO, IT IS NOT REQUIRED                       
         CLI   DLDSPLST,0          TEST ONLY SPILL WANTED                       
         BNE   RTRNNO              YES, WE DON'T WANT STATION LIST              
         CLI   DLDSTALS,0          TEST DOWNLOAD STATION LIST DATA              
         B     SETCCC                                                           
                                                                                
TSTONEST CLI   DLDBYSTA,0          TEST DOWNLOAD BY STATION                     
         BNE   SETCCC                                                           
         CLI   STAIND,LW_TSINQ     TEST SINGLE STATION REQUEST                  
         BNE   SETCCC                                                           
         XR    R1,R1                                                            
         ICM   R1,7,AMKT             AMKT SET BY BLDMKS                         
         MVC   STAMKT#,LW_DATA1-LW_D(R1)                                        
         B     RTRNYES                                                          
                                                                                
TSTXBUYS CLI   XBUYS,0             TEST EXCLUDE DOWNLOAD BUY DATA               
         BR    RE                                                               
                                                                                
***********************************************************************         
* NEW GOAL WKS DOWNLOAD  X'014B'                                                
***********************************************************************         
REQNGL   LKREQ H,I#SDNGDL,OUTNGL,NEXTREQ=REQSLS                                 
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),             +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
PrdCd    LKREQ F,3,(I,B#SAVED,PRDIND),(R,VALPRD),LIST=NOD,             +        
               OLEN=L'BUYKPRD,MAXLEN=L'PKEYPRD,TEXT=SP#PRO,COL=*                
EstNo    LKREQ F,4,(I,B#SAVED,ESTIND),LBIN,LIST=NOD,RANGE=Y,           +        
               DEFAULT=NOT,OLEN=L'EKEYEST,TEXT=SP#EST,COL=*                     
MktNo    LKREQ F,5,(I,B#SAVED,MKTIND),(R,VALMKT),OLEN=L'BUYKMKTN,      +        
               LIST=NOD,DEFAULT=NOT,TEXT=SP#MKT,COL=*                           
         LKREQ E                                                                
                                                                                
OUTNGL   LKOUT H                   ** NEW GOAL DOWNLOAD **                      
NGLGOL   LKOUT R,X'0061'           GOALS AND MARKET LOCKIN RECORDS              
Array    LKOUT C,255,(A,ARYNGD)                                                 
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR NEW GOAL DOWNLOAD                                        
***********************************************************************         
                                                                                
ARYNGD   LKOUT A,(R,NXTNGD),MULTIROW=Y                                          
                                                                                
Media    LKOUT C,1,(D,B#WORKD,QMEDA),CHAR,ND=Y                                  
CltCd    LKOUT C,2,(D,B#WORKD,QCLTA),CHAR,ND=Y                                  
Pr1Cd    LKOUT C,3,(D,B#SAVED,GOLPRD1),(U,#EDTPRD,$EDTPRD),ND=Y                 
EstNo    LKOUT C,4,(D,B#SAVED,GOLEST),LBIN,ND=Y                                 
*                                                                               
MktNo    LKOUT C,5,(D,B#SAVED,GOLOMKT),LBIN,ND=Y                                
GlStDt   LKOUT C,6,(D,B#SAVED,GOLOSDT),CDAT,ND=Y                                
GlEnDt   LKOUT C,7,(D,B#SAVED,GOLONDT),CDAT,ND=Y                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET GOALS WEEKS                                                               
***********************************************************************         
                                                                                
NXTNGD   LA    RF,GOLVALS                                                       
         ST    RF,LP_ADATA         POINT TO SEND VALUES                         
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTNGD10                                                         
                                                                                
         XC    GOLVALS(GOLVALL),GOLVALS                                         
                                                                                
         ICM   RF,7,AMED           SET EBCDIC MEDIA                             
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
*                                                                               
         ICM   RF,7,ACLT           SET EBCDIC CLIENT                            
         MVC   QCLTX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTCLT,AEDTCLT),DMCB,QCLTX,,QCLTA                              
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING EKEY,R2                                                          
         MVC   EKEYAM,QMEDX                                                     
         MVC   EKEYCLT,QCLTX                                                    
         MVC   EKEYPRD,=C'POL'                                                  
         ICM   RF,7,AEST                                                        
         CLI   LW_D-LW_D(RF),LW_TLSTQ   LIST OF VALUES?                         
         JE    *+14                                                             
         MVC   EKEYEST,LW_DATA1-LW_D(RF)                                        
         J     *+10                                                             
         MVC   EKEYEST,LW_DATA2-LW_D(RF) GET FIRST IN THE LIST                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   NXTNGD80                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         JNE   NXTNGD80                                                         
         L     R2,IOADDR                                                        
         MVC   ESTSTRDT,ESTART                                                  
         MVC   ESTENDDT,EEND                                                    
         MVC   ESTOOW,EOWSDAY                                                   
         DROP  R2                                                               
         J     NXTNGDNR                                                         
********                                                                        
* NOT THE FIRST TIME ANYMORE                                                    
********                                                                        
NXTNGD10 OC    GOLMKT,GOLMKT       NO MORE MARKETS                              
         BZ    NOMORE                                                           
         MVC   GOLOMKT,GOLMKT      NOT THE FIRST TIME HERE SO WE                
NXTNGD12 MVC   GOLOSDT,GOLSTDT       OUTPUT A GOAL MARKET ONCE                  
         MVC   GOLONDT,GOLNDDT                                                  
*                                                                               
NXTNGDNR GOTOR (#NXTREC,ANXTREC),DMCB,NGLKEYT,('B#GOLREC',0),SAVED,    +        
               AFLTNGL,0                                                        
         JE    NXTNGD13                                                         
         XC    GOLMKT,GOLMKT       NO MORE MARKETS                              
         J     NXTNGD80            OUTPUT LAST MARKET'S GOAL DATES              
*                                                                               
NXTNGD13 L     R2,IOADDR                                                        
         USING GKEY,R2                                                          
         MVC   GOLPRD1,GKEYPRD     SET PRODUCT1 (PRODUCT) NUMBER                
         MVC   GOLEST,GKEYEST      SET ESTIMATE NUMBER                          
         MVC   GOLMKT,GKEYMKT                                                   
*                                                                               
         CLC   GOLMKT,GOLOMKT      CHANGE IN MARKET?                            
         JE    *+10                                                             
         XC    GOLSTDT(L'GOLSTDT*2),GOLSTDT                                     
*                                                                               
         OC    GOLOMKT,GOLOMKT     HAVE WE SET THE MARKET YET?                  
         JNZ   NXTNGD15            YES                                          
         MVC   GOLOMKT,GKEYMKT                                                  
*                                                                               
NXTNGD15 LA    R2,GDELEM                                                        
         USING GLEMENT,R2                                                       
NXTNGD20 CLI   0(R2),0             EOR?                                         
         JE    NXTNGD50                                                         
         CLI   0(R2),GLCODEQ       X'21' - GOAL WEEK ELEM                       
         JE    NXTNGD30                                                         
NXTNGD25 LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         J     NXTNGD20                                                         
*                                                                               
NXTNGD30 OC    GOLSTDT,GOLSTDT     DO WE HAVE ANY GOAL WEEKS?                   
         JNZ   NXTNGD40                                                         
         MVC   GOLSTDT,GLWEEK      NO, START AND END ARE THE SAME               
         MVC   GOLNDDT,GLWEEK                                                   
         J     NXTNGD25                                                         
*                                                                               
NXTNGD40 CLC   GLWEEK,GOLSTDT      IS THIS WEEK EARLIER?                        
         JNL   NXTNGD45                                                         
         MVC   GOLSTDT,GLWEEK      YES                                          
         J     NXTNGD25            CHECK OUT THE NEXT WEEK                      
*                                                                               
NXTNGD45 CLC   GLWEEK,GOLNDDT      IS THIS WEEK LATER?                          
         JNH   NXTNGD25                                                         
         MVC   GOLNDDT,GLWEEK      YES                                          
         J     NXTNGD25            CHECK OUT THE NEXT WEEK                      
*                                                                               
NXTNGD50 CLC   GOLMKT,GOLOMKT      STILL WORKING ON OUR OUTPUT MARKET?          
         JE    NXTNGD12            YES                                          
*                                                                               
NXTNGD80 OC    GOLOMKT,GOLOMKT     ANY MARKET TO OUTPUT?                        
         JZ    NOMORE                                                           
*                                                                               
         OC    GOLOSDT(L'GOLOSDT*2),GOLOSDT  OUTPUT MKT HAS WEEKS?              
         JZ    NXTNGD10                      NO, USE CURRENT MKT GOALS          
*                                                                               
         GOTOR VDATCON,DMCB,(2,GOLONDT),(0,WORK)                                
         CLC   ESTSTRDT,WORK       GOAL IS ONLY FOR 1 WEEK?                     
         JNE   NXTNGD90            NO, THEN IT IS NOT COMPLICATED               
         CLI   ESTOOW,1            DO WE HAVE AN OOW?                           
         JH    NXTNGD90            YES, ALSO NOT COMPLICATED                    
* ESTIMATES THAT ARE NOT OOW DON'T HAVE TO START ON MONDAY                      
         GOTOR VGETDAY,DMCB,(0,WORK),(0,WORK+6)                                 
         CLI   DMCB,1              START DATE ON MONDAY?                        
         JNH   NXTNGD90            THEN WE DO SUNDAY IS END OF WEEK             
         LA    R1,7                FIND # OF DAYS FROM SUNDAY                   
         LLC   R0,DMCB                                                          
         SR    R1,R0                                                            
         J     NXTNGD95            AND USE THAT TO CALC END DATE                
*                                                                               
NXTNGD90 LA    R1,6                                                             
NXTNGD95 ST    R1,DMCB+8                                                        
         GOTOR VADDAY,DMCB,WORK,WORK+6,,0   CALCULATE END DATE                  
         CLC   WORK+6(6),ESTENDDT         IS CALCULATED DATE > EST END?         
         JNH   *+10                                                             
         MVC   WORK+6(6),ESTENDDT         YES, THEN EST END IS OUR END          
         GOTOR VDATCON,DMCB,(0,WORK+6),(2,GOLONDT)                              
*                                                                               
NXTNGDX  J     MORE                EXIT TO SEND GOAL WEEKS                      
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* APPLY GOAL FILTERS                                                            
***********************************************************************         
         USING GKEY,IOKEY                                                       
FLTNGL   TM    GKEYAGY,GKEY2NPQ+GKEYNHRQ+GKEYTAR  X'40'+X'20'+'10'              
         JNZ   EXITN               ONLY WANT X'00' OR X'80'  FOR                
         J     EXITY                  NORMAL GOAL OR PIGGY GOAL KEY             
***********************************************************************         
* STATION LIST BY MARKET REQUEST X'0145'                                        
***********************************************************************         
REQSLS   LKREQ H,I#STALST,OUTSLS,NEXTREQ=REQSGL                                 
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
MktNo    LKREQ F,2,(I,B#SAVED,MKTIND),(R,VALMKT),OLEN=L'BUYKMKTN,      +        
               LIST=NOD,DEFAULT=NOT,TEXT=SP#MKT,COL=*                           
         LKREQ E                                                                
*                                                                               
OUTSLS   LKOUT H                                                                
         LKOUT R,X'0050'           STATIONS IN MARKET RECORD                    
Array    LKOUT C,255,(A,ARYSLS)                                                 
**Array    LKOUT C,255,(A,ARYSTQ)                                               
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION LIST BY MARKET (K PASSIVE)                       
***********************************************************************         
                                                                                
ARYSLS   LKOUT A,(R,NXTSLS),MULTIROW=Y                                          
                                                                                
Media    LKOUT C,1,(D,B#SAVED,STAMEDIA),CHAR,ND=Y                               
MktNo    LKOUT C,2,(D,B#SAVED,STAMKT#),LBIN,ND=Y                                
Statn    LKOUT C,3,(D,B#SAVED,STASTA),CHAR                                      
MktNm    LKOUT C,4,(D,B#SAVED,STAMKTN),CHAR,ND=Y                                
Chnnl    LKOUT C,5,(D,B#SAVED,STACHNL),CHAR,ND=Y                                
NwAff    LKOUT C,6,(D,B#SAVED,STANTWK),CHAR,ND=Y                                
Frmat    LKOUT C,7,(D,B#SAVED,STAFRMT),CHAR,ND=Y                                
TaxRt    LKOUT C,9,(D,B#SAVED,STATAX),CHAR,ND=Y                                 
BkTyp    LKOUT C,10,(D,B#SAVED,STABTYP),(R,TRNSBKT),ND=Y                        
MktBkTyp LKOUT C,10,(D,B#SAVED,STAMKBT2),(R,TRNSBKT),                  +        
               ND=Y,FILTROUT=TSTSTABT                                           
MktAC    LKOUT C,11,(D,B#SAVED,STAAMKT),CHAR,ND=Y                               
MkLPM    LKOUT C,12,(D,B#SAVED,STALPMD),CDAT,ND=Y                               
MkCDm    LKOUT C,13,(D,B#SAVED,STACDEM),CHAR,ND=Y                               
**MktTmZ   LKOUT C,14,(D,B#SAVED,STAMKTZ),CHAR,PCVERSION=3.1.0.111              
MktBkTyp LKOUT C,15,(D,B#SAVED,STAMKBT),(R,TRNSBKT),                   +        
               ND=Y,PCVERSION=3.3.0.6                                           
MkAAMkt  LKOUT C,16,(D,B#SAVED,STAAAMK),LBIN,ND=Y,PCVERSION=4.5.0.63            
Array    LKOUT C,255,(A,ARYNWK),FILTROUT=TSTCABLE                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* SEND STATION VALUES                                                 *         
***********************************************************************         
                                                                                
NXTSLS   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSLS10                                                         
                                                                                
         ICM   RF,7,AMED           SET EBCDIC MEDIA                             
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         MVC   BUYMED,QMEDA                                                     
                                                                                
         ICM   RE,7,AMKT           SET EBCDIC STATION MARKET FILTER             
         SR    R0,R0                                                            
         ICM   R0,3,LW_DATA1-LW_D(RE)                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  STAMKTNO,DUB                                                     
                                                                                
         L     R2,LP_AWMP          INITIALIZE STATIONS THAT HAVE BUYS           
         USING LW_D,R2                                                          
         XC    LW_NUMN,LW_NUMN                                                  
         ST    R2,ASTALST          NO STATION FROM BUY RECORDS                  
         LA    R2,LW_LN2Q(R2)                                                   
         ST    R2,AKSTALST         SET A(STATION PASSIVE LIST)                  
         USING LW_D,R2             BUT WE NEED TO FIND WHERE IN WMP             
         LA    R3,LW_DATA2         R3=A(STATION LIST)                           
         SR    R0,R0               R0=NUMBER OF STATIONS IN LIST                
         XC    WORK(L'STKKSTA),WORK                                             
                                                                                
NXTSLS02 GOTOR (#NXTREC,ANXTREC),DMCB,MSTKEYT,('B#STAREC',0),          +        
               (X'80',SAVED),0,0                                                
         JNE   NXTSLS04                                                         
         L     R1,IOADDR                                                        
         USING STARECD,R1                                                       
         OC    STKKCLT,STKKCLT     ONLY WANT NON-CLIENT SPECIFIC                
         JNZ   NXTSLS02                                                         
                                                                                
         CLC   STKKSTA,WORK        TEST ALREADY HAVE THIS STATION               
         JE    NXTSLS02                                                         
         MVC   WORK(L'STKKSTA),STKKSTA                                          
         LR    RE,R3               MAKE SURE WE HAVE ENOUGH ROOM                
         AHI   RE,L'STKKSTA        BUMP TO NEXT STATION LIST ENTRY              
         L     RF,LP_AWMPX         MAKE SURE WE'RE NOT CLOBBERING WHAT          
         CR    RE,RF                 IS BEYOND  AWMP                            
         JH    *+2                                                              
                                                                                
         MVC   0(L'STKKSTA,R3),STKKSTA                                          
         LR    R3,RE                                                            
                                                                                
         AHI   R0,1                BUMP NUMBER OF ENTRIES                       
         J     NXTSLS02                                                         
                                                                                
NXTSLS04 MVI   LP_RMODE,LP_RNEXT   RESET MODE                                   
         STCM  R0,3,LW_NUMN        SET NUMBER OF STATIONS FOUND                 
         XC    STANUMN,STANUMN     SET NO ENTRIES PROCESSED                     
         XC    PMKT#,PMKT#         INITIALIZE MARKET OPTIMIZER                  
         MVI   STALEFTS,0          SET NOT PROCESSING LEFTOVERS                 
                                                                                
         L     R2,AKSTALST         R2=A(STATION LIST WMP ENTRY)                 
         XR    RE,RE                                                            
         ICM   RE,3,LW_NUMN                                                     
         MHI   RE,L'STKKSTA                                                     
         LA    RE,LW_DATA2(RE)                                                  
                                                                                
         ST    RE,ASPMKLST         SPILL MARKET LIST TO GO HERE                 
         XC    0(LW_LN2Q,RE),0(RE)                                              
         MVI   LW_NUMN+1-LW_D(RE),0                                             
         MVC   LW_DATA2-LW_D(2,RE),=X'FFFF'  EOT                                
         MVC   LW_DATA2+(MXSPLMKS*2)-LW_D(2,RE),=X'FFFF'                        
         LA    RE,LW_DATA2+(MXSPLMKS*2)+2-LW_D(RE)                              
                                                                                
         ST    RE,ASTEQLST         CLT-SPECIFIC MGREQ STATIONS GO HERE          
         XC    0(LW_DATA2-LW_D,RE),0(RE)                                        
         MVC   LW_DATA2-LW_D(L'STEKSTA,RE),EFFS   EOT                           
                                                                                
***********************************************************************         
* PROCESS FIRST/NEXT STATION STATION PASSIVE LIST                     *         
***********************************************************************         
                                                                                
NXTSLS10 L     R2,AKSTALST         R2=A(STATION LIST WMP ENTRY)                 
         XC    STAVALS(STAVALL),STAVALS                                         
         LA    RF,STAVALS                                                       
         ST    RF,LP_ADATA         POINT TO SEND VALUES                         
                                                                                
         SR    R1,R1               BUMP STATION NUMBER                          
         ICM   R1,3,STANUMN                                                     
         LA    R0,1(R1)                                                         
         CLM   R0,3,LW_NUMN        TEST ALL ENTRIES PROCESSED                   
         JH    NOMORE                                                           
*                                                                               
         STCM  R0,3,STANUMN                                                     
         MHI   R1,L'STKKSTA                                                     
         LA    R1,LW_DATA2(R1)     POINT TO NEXT STATION IN LIST                
         MVC   STASTA(L'STKKSTA),0(R1)                                          
         GOTOR GETSCL                                                           
         L     R1,AIO3                                                          
         CLC   SMKT,STAMKTNO       TEST STATION IN CORRECT MARKET               
         JNE   NXTSLS10                                                         
                                                                                
***********************************************************************         
* SET STATION/MARKET VALUES                                           *         
***********************************************************************         
                                                                                
         CLI   STASTA,C'0'         IS THIS A CABLE SYSCODE?                     
         JL    NXTSLS26            NO, DON'T HAVE TO KEEP RECORD AROUND         
         CLC   STAKCLT,EZEROS      WE HAVE CLIENT SPECIFIC?                     
         JE    NXTSLS24            NO, PLAIN VANILLA HAS THE NETWORKS           
         L     RE,AIO3                                                          
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'STAKEY),0(RE)       GET THE STATION 'S' KEY              
         LA    R1,IOKEY                                                         
         MVC   STAKCLT,EZEROS      NEED PLAIN VANILLA FOR THE NETWORKS          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO7'                            
         J     NXTSLS26                                                         
                                                                                
NXTSLS24 L     RE,AIO3             COPY STATION RECORD TO IO7                   
         L     R0,AIO7                                                          
         XR    RF,RF                                                            
         ICM   RF,3,STAKLEN                                                     
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
NXTSLS26 L     R1,AIO3                                                          
         CLC   PMKT#,SMKT          TEST CHANGE OF MARKET?                       
         JE    NXTSLS28            NO, NO NEED TO READ MKT RECORD THEN          
         MVC   PMKT#,SMKT                                                       
                                                                                
         PACK  DUB,SMKT                                                         
         CVB   RE,DUB                                                           
         STCM  RE,3,STAMKT#                                                     
         GOTOR GETMKT,STAMKT#      READ MARKET RECORD                           
                                                                                
NXTSLS28 B     EXITY               EXIT TO SEND STATION VALUES                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SCHEDULED GUIDELINES DOWNLOAD REQUEST                               *         
***********************************************************************         
                                                                                
REQSGL   LKREQ H,I#SDDEVD,OUTSGL,NEXTREQ=REQRRD                                 
RecDA    LKREQ F,D#DA,(I,B#SAVED,RDAIND),HEXD,OLEN=L'BUYKDA,           +        
               TEXT=SP#SDBDA,COL=*                                              
PCKey    LKREQ F,D#PCKEY,(D,B#SAVED,PCKEY),CHAR,TEXT=SP#KEY,COL=*               
TxtResp  LKREQ F,D#UPLERR,(D,B#SAVED,SGLERTXT),VSTR,TEXT=SP#ERR,COL=*           
Numresp  LKREQ F,D#UPLERN,(D,B#SAVED,SGLERNUM),LBIN,TEXT=SP#ERR,COL=*           
         LKREQ E                                                                
                                                                                
OUTSGL   LKOUT H                   SCHEDULE GUIDELINES DOWNLOAD                 
         LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYSGL)                                                   
         LKOUT X                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SCHEDULED GUIDELINES DOWNLOAD                  *         
***********************************************************************         
ARYSGL   LKOUT A,(R,NXTSGL),MULTIROW=Y                                          
Array    LKOUT C,X'03D0',(A,ARYDVD)    SCHD G'LINES DATA                        
         LKOUT C,255,(A,ARYDEV)                                                 
         LKOUT E                                                                
                                                                                
ARYDVD   LKOUT A,(D,B#DEVREC,DDVRECD),NROWS=1,NEWEL=Y,ROWWIDTH=1000             
Cksum    LKOUT C,1,(D,B#SAVED,SGLCKSM),HEXD,ND=Y                                
RecDA    LKOUT C,2,(D,B#SAVED,SGLDA),HEXD,ND=Y                                  
ErrTxt   LKOUT C,3,(D,B#SAVED,SGLERTXT),CHAR,ND=Y                               
ErrNum   LKOUT C,4,(D,B#SAVED,SGLERNUM),LBIN,ND=Y                               
         LKOUT E                                                                
                                                                                
******   Scheduled GuideLines Download   ******                                 
ARYDEV   LKOUT A,(D,B#DEVREC,DDVRECD),NROWS=1,ROWWIDTH=1000                     
Array    LKOUT C,X'03D1',(A,ARYDDT)    SCHD G'LINES WEEK DATA                   
Array    LKOUT C,X'03D2',(A,ARYDAC)    SCHD G'LINES ACTIVITY                    
         LKOUT E                                                                
                                                                                
******   Scheduled GuideLines Weeks Data                                        
ARYDDT   LKOUT A,(D,B#DEVREC,DDVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(DVDATAD,DVDELQ),ROWWIDTH=(V,DVDLEN)                       
SDate    LKOUT C,1,DVDSTDAT,CDAT,ND=Y                                           
Inc/Exc  LKOUT C,2,DVDINEX,CHAR,ND=Y                                            
Array    LKOUT C,3,(A,ARYDEV2)                                                  
         LKOUT E                                                                
                                                                                
ARYDEV2  LKOUT A,(*,DVDATA2),ROWNAME=DVDEL,NROWS=*,ROWWIDTH=DVDLEN2Q            
Days     LKOUT C,3,DVDAYS,LBIN,ND=Y                                             
STime    LKOUT C,4,DVDSTIME,HEXD,ND=Y                                           
ETime    LKOUT C,5,DVDETIME,HEXD,ND=Y                                           
         LKOUT E                                                                
                                                                                
******   Scheduled Guidelines Activity Data                                     
ARYDAC   LKOUT A,(D,B#DEVREC,DDVEL),EOT=EOR,NEWEL=Y,                   *        
               ROWID=(DVAELD,DVAELQ),ROWWIDTH=(V,DVALEN)                        
                                                                                
AddDt    LKOUT C,1,DVAADD,CDAT,ND=Y                                             
AddTime  LKOUT C,2,DVAATIME,HEXD,ND=Y                                           
AddPid   LKOUT C,3,DVAAPID,(R,EDTPID),LEN=2,ND=Y                                
AddNM    LKOUT C,4,(D,B#SAVED,PERNAME),CHAR,ND=Y                                
ChgDt    LKOUT C,5,DVACHG,CDAT,ND=Y                                             
ChgTime  LKOUT C,6,DVACTIME,HEXD,ND=Y                                           
ChgPid   LKOUT C,7,DVACPID,(R,EDTPID),LEN=2,ND=Y                                
ChgNM    LKOUT C,8,(D,B#SAVED,PERNAME),CHAR,ND=Y                                
         LKOUT E                                                                
***********************************************************************         
* GET SCHEDULED GUIDELINES / DEVIATED WEEKS RECORDS                             
***********************************************************************         
NXTSGL   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSGL20                                                         
         ST    R0,LP_ADATA         SET A(DUMMY OUTPUT) IN CASE OF EXIT          
                                                                                
         CLC   LP_QMAPN,SDSGL#     TEST SCHEDULED GUIDELINES RESPONSE           
         JNE   NXTSGL10                                                         
         ICM   RE,7,ARDA           TEST DISK ADDRESS PASSED                     
         JZ    *+2                                                              
         CLI   LW_TYPE-LW_D(RE),LW_TSINQ                                        
         JNE   *+2                                                              
         MVC   IODAOVER,LW_DATA1-LW_D(RE)                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOXSPFIL+IO4'                           
         JE    NXTSGL30                                                         
         DC    H'0'                                                             
*                                                                               
NXTSGL10 MVC   PRD,POLPRD          USE POL IF NOT SENT                          
         CLI   NOPRDREQ,C'Y'       NO PRODUCT IN REQUEST                        
         JE    NXTSGL20                                                         
         ICM   RF,7,APRD                                                        
         JZ    NXTSGL20                                                         
         GOTOR (#EDTPRD,AEDTPRD),DMCB,LW_DATA1-LW_D(RF),0,PRD                   
                                                                                
NXTSGL20 GOTOR (#NXTREC,ANXTREC),DMCB,DEVKEYT,('B#DEVREC',0),          *        
               ('$NXTRXSP',SAVED),0,0                                           
NXTSGL30 BRAS  RE,GETCSUM                                                       
         MVC   SGLDA,IODA          SET DISK ADDRESS                             
         MVI   SGLFIND,C'X'        XSPFIL INDICATOR                             
         J     EXITY                                                            
                                                                                
GETCSUM  NTR1  LABEL=NO                                                         
         L     RE,IOADDR           RE=A(A(RECORD))                              
         SR    RF,RF                                                            
         ICM   RF,3,DRVRLEN-DRVKEY(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,SGLCKSM                                                    
         XIT1                                                                   
***********************************************************************         
* ARRAY DEFINITION FOR CLEARANCE DATA DOWNLOAD                        *         
***********************************************************************         
                                                                                
ARYCLR   LKOUT A,(R,NXTCLR),MULTIROW=Y,ROWNAME=CLRSTATD                         
                                                                                
Array    LKOUT C,X'0063',(A,ARYCLRV)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET CLEARANCE STATUS RECORDS                                        *         
***********************************************************************         
                                                                                
NXTCLR   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCLR02                                                         
                                                                                
         OC    BUYCLRHI,BUYCLRHI   TEST ANY CLEARANCE DATES SET                 
         BZ    NOMORE                                                           
         XC    CLRVALS(CLRVALL),CLRVALS                                         
         MVC   APRD1,APRD                                                       
         ICM   RF,7,APRD1                                                       
         CLI   LW_TYPE-LW_D(RF),LW_TSINQ                                        
         JNE   NXTCLR02                                                         
         CLI   LW_DATA1-LW_D(RF),POLPRDQ                                        
         JNE   NXTCLR02                                                         
         MVC   APRD1,AALL          DEFAULT PRODUCT ENTRY                        
                                                                                
NXTCLR02 GOTOR (#NXTREC,ANXTREC),DMCB,CLRKEYT,('B#CLRREC',0),SAVED,0,0          
         BNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING CLRSTATD,R2         R2=A(CLEARANCE RECORD)                       
         MVC   CLRSTAX,CLSKSTA                                                  
                                                                                
         LA    R2,CLSELEMS                                                      
         USING CLSTEL01,R2         R2=A(FIRST CLEARANCE ELEMENT)                
         SR    R0,R0                                                            
         SR    R3,R3                                                            
                                                                                
NXTCLR04 CLI   CLSTEL01,EOR        TEST END OF RECORD                           
         JNE   NXTCLR06                                                         
         LTR   R3,R3               YES - TEST ANYTHING TO DOWNLOAD              
         JZ    NXTCLR02                                                         
         B     EXITY                                                            
                                                                                
NXTCLR06 CLI   CLSTEL01,CLS01ELQ   TEST CLEARANCE ELEMENT                       
         JNE   NXTCLR12                                                         
         LA    RE,CLSTCLRD                                                      
         CLI   CLS01LEN,CLSTOCLR-CLSTEL01                                       
         JL    NXTCLR08                                                         
         OC    CLSTOCLR,CLSTOCLR                                                
         JZ    NXTCLR08                                                         
         LA    RE,CLSTOCLR                                                      
                                                                                
NXTCLR08 CLC   0(L'CLSTCLRD,RE),BUYCLRLO                                        
         JL    NXTCLR10                                                         
         CLC   0(L'CLSTCLRD,RE),BUYCLRHI                                        
         JH    NXTCLR10                                                         
         MVC   CLRPRD,CLSTPRD                                                   
         GOTOR LP_ASETK,DMCB,(1,PRDFLT),CLRPRD,SAVED,('FF',LP_D)                
         JNE   NXTCLR10                                                         
         AHI   R3,1                                                             
         J     NXTCLR12                                                         
                                                                                
NXTCLR10 MVI   CLSTEL01,FF         DISABLE CLEARANCE ELEMENT                    
                                                                                
NXTCLR12 LLC   R0,CLS01LEN         BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         J     NXTCLR04                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CLEARANCE DETAIL DOWNLOAD                      *         
***********************************************************************         
                                                                                
ARYCLRV  LKOUT A,(D,B#CLRREC,CLSELEMS),EOT=EOR,NEWEL=B,                +        
               ROWID=(CLSTEL01,CLS01ELQ),ROWWIDTH=(V,CLS01LEN)                  
                                                                                
Statn    LKOUT C,1,(D,B#SAVED,CLRSTAX),(R,EDTCLS),ND=Y                          
ClrDt    LKOUT C,2,CLSTCLRD,CDAT                                                
ClrSq    LKOUT C,3,CLSTCLSQ,LBIN                                                
RepTy    LKOUT C,4,CLSTREPT,CHAR                                                
Payee    LKOUT C,5,CLSTPYEE,CHAR,ND=Y                                           
PrdCd    LKOUT C,6,CLSTPRD,(U,#EDTPRD,$EDTPRD)                                  
PigCd    LKOUT C,7,CLSTPRD2,(U,#EDTPRD,$EDTPRD),ND=Y                            
StrDt    LKOUT C,8,CLSTSTDT,CDAT                                                
EndDt    LKOUT C,9,CLSTNDDT,CDAT                                                
Gross    LKOUT C,10,CLSTGRS,CBIN,ND=Y                                           
NetCs    LKOUT C,11,CLSTNET,CBIN,ND=Y                                           
Check    LKOUT C,12,CLSTCHK,CHAR,ND=Y                                           
CDate    LKOUT C,13,CLSTCHDT,CDAT,ND=Y                                          
CStat    LKOUT C,14,CLSTSTAT,LBIN,ND=Y                                          
BCDat    LKOUT C,15,CLSTBKDT,CDAT,ND=Y                                          
OCDat    LKOUT C,16,CLSTOCLR,CDAT,ND=Y                                          
EstNo    LKOUT C,17,CLSTEST,LBIN,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR WORKSHEET LIST                                 *         
***********************************************************************         
                                                                                
ARYWRK   LKOUT A,(R,NXTWRK),MULTIROW=Y,ROWNAME=DRVRECD                          
         LKOUT P,DRVKREVS,REVCOMP                                               
         LKOUT C,1,(D,B#WORKD,WORK),LBIN,LEN=L'DRVKREVS                         
         LKOUT C,2,(A,ARYWNM)                                                   
         LKOUT C,3,(D,B#SAVED,SHTDA),HEXD,ND=Y,PCVERSION=3.0.0.30               
         LKOUT C,4,(D,B#SAVED,SHTFIND),CHAR,ND=Y,PCVERSION=3.0.0.30             
         LKOUT E                                                                
                                                                                
ARYWNM   LKOUT A,(D,B#REVREC,DRVEL),ROWID=(RSNELD,RSNELQ),EOT=EOR,     +        
               ROWWIDTH=(V,RSNLEN)                                              
         LKOUT C,2,RSNAME,CHAR,LEN=V                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET WORKSHEET RECORDS                                               *         
***********************************************************************         
                                                                                
NXTWRK   XC    SHTVALS(SHTVALL),SHTVALS                                         
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTWRK02                                                         
*** NOP'D - CANADIAN AGENCY WILL NEVER BE SUPPORTED    -HWON 05/26/2020         
***      TM    RUNI1,RUNICANA      IGNORE FOR CANADA                            
***      BNZ   NOMORE                                                           
         MVC   PRD,POLPRD          USE POL IF NOT SENT                          
         CLI   NOPRDREQ,C'Y'       NO PRODUCT IN REQUEST                        
         JE    NXTWRK02                                                         
         ICM   RF,7,APRD                                                        
         JZ    NXTWRK02                                                         
         GOTOR (#EDTPRD,AEDTPRD),DMCB,LW_DATA1-LW_D(RF),0,PRD                   
                                                                                
NXTWRK02 GOTOR (#NXTREC,ANXTREC),DMCB,WRKKEYT,('B#REVREC',0),          +        
               ('$NXTRXSP',SAVED),0,0                                           
         MVI   SHTFIND,C'X'        XSPFIL INDICATOR                             
         MVC   SHTDA,IODA          SET DISK ADDRESS                             
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR REVISION SHEET LIST                            *         
***********************************************************************         
                                                                                
ARYREV   LKOUT A,(R,NXTREV),MULTIROW=Y,ROWNAME=DRVRECD                          
         LKOUT P,DRVKREVS,REVCOMP                                               
         LKOUT C,1,(D,B#WORKD,WORK),LBIN,LEN=L'DRVKREVS                         
         LKOUT C,2,(A,ARYRNM)                                                   
         LKOUT C,3,(D,B#SAVED,SHTDA),HEXD,ND=Y,PCVERSION=3.0.0.30               
         LKOUT C,4,(D,B#SAVED,SHTFIND),CHAR,ND=Y,PCVERSION=3.0.0.30             
         LKOUT E                                                                
                                                                                
ARYRNM   LKOUT A,(D,B#REVREC,DRVEL),ROWID=(RSNELD,RSNELQ),EOT=EOR,     +        
               ROWWIDTH=(V,RSNLEN)                                              
         LKOUT C,2,RSNAME,CHAR,LEN=V                                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET REVSHEET RECORDS                                                *         
***********************************************************************         
                                                                                
NXTREV   XC    SHTVALS(SHTVALL),SHTVALS                                         
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTREV02                                                         
*** NOP'D - CANADIAN AGENCY WILL NEVER BE SUPPORTED    -HWON 05/26/2020         
***      TM    RUNI1,RUNICANA      IGNORE FOR CANADA                            
***      BNZ   NOMORE                                                           
         MVC   PRD,POLPRD          USE POL IF NOT SENT                          
         CLI   NOPRDREQ,C'Y'       NO PRODUCT IN REQUEST                        
         JE    NXTREV02                                                         
         ICM   RF,7,APRD                                                        
         JZ    NXTREV02                                                         
         GOTOR (#EDTPRD,AEDTPRD),DMCB,LW_DATA1-LW_D(RF),0,PRD                   
                                                                                
NXTREV02 GOTOR (#NXTREC,ANXTREC),DMCB,REVKEYT,('B#REVREC',0),          +        
               ('$NXTRXSP',SAVED),0,0                                           
         MVI   SHTFIND,C'X'        XSPFIL INDICATOR                             
         MVC   SHTDA,IODA          SET DISK ADDRESS                             
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DAYPART MENU DOWNLOAD                          *         
***********************************************************************         
                                                                                
ARYSDDP  LKOUT A,(R,GETDPT),NEWEL=R,EOT=EOR,ROWWIDTH=DPTTABL,          +        
               ROWNAME=DPTTABD                                                  
                                                                                
DCode    LKOUT C,1,DPTTALPH,CHAR                                                
DName    LKOUT C,2,DPTTNAME,CHAR                                                
DMast    LKOUT C,3,DPTTMAST,CHAR,ND=Y                                           
MDptN    LKOUT C,4,DPTTMSCD,HEXD                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION PASSIVE BY MARKET                      *         
***********************************************************************         
                                                                                
ARYMST   LKOUT A,(R,NXTMST),MULTIROW=Y                                          
                                                                                
Media    LKOUT C,1,(D,B#SAVED,STAMEDIA),CHAR,ND=Y                               
MktNo    LKOUT C,2,(D,B#SAVED,STAMKT#),LBIN,ND=Y                                
Statn    LKOUT C,3,(D,B#SAVED,STASTA),CHAR                                      
MktNm    LKOUT C,4,(D,B#SAVED,STAMKTN),CHAR,ND=Y                                
Chnnl    LKOUT C,5,(D,B#SAVED,STACHNL),CHAR,ND=Y                                
NwAff    LKOUT C,6,(D,B#SAVED,STANTWK),CHAR,ND=Y                                
Frmat    LKOUT C,7,(D,B#SAVED,STAFRMT),CHAR,ND=Y                                
TaxRt    LKOUT C,9,(D,B#SAVED,STATAX),CHAR,ND=Y                                 
BkTyp    LKOUT C,10,(D,B#SAVED,STABTYP),(R,TRNSBKT),ND=Y                        
MktBkTyp LKOUT C,10,(D,B#SAVED,STAMKBT2),(R,TRNSBKT),                  +        
               ND=Y,FILTROUT=TSTSTABT                                           
MktAC    LKOUT C,11,(D,B#SAVED,STAAMKT),CHAR,ND=Y                               
MkLPM    LKOUT C,12,(D,B#SAVED,STALPMD),CDAT,ND=Y                               
MkCDm    LKOUT C,13,(D,B#SAVED,STACDEM),CHAR,ND=Y,PCVERSION=2.6.0.0             
MktTmZ   LKOUT C,14,(D,B#SAVED,STAMKTZ),CHAR,PCVERSION=3.1.0.111                
MktBkTyp LKOUT C,15,(D,B#SAVED,STAMKBT),(R,TRNSBKT),                   +        
               ND=Y,PCVERSION=3.3.0.6                                           
MkAAMkt  LKOUT C,16,(D,B#SAVED,STAAAMK),LBIN,ND=Y,PCVERSION=4.5.0.63            
MkCBkTyp LKOUT C,17,(D,B#SAVED,MKTCBKTY),CHAR,ND=Y,PCVERSION=4.6.0.50           
StCBkTyp LKOUT C,18,(D,B#SAVED,STACBKTY),CHAR,ND=Y,PCVERSION=4.6.0.50           
StcomMkt LKOUT C,19,(D,B#SAVED,STACMKT),LBIN,ND=Y,PCVERSION=4.6.0.50            
Array    LKOUT C,255,(A,ARYPC2),FILTROUT=TSTNLEFT                               
Array    LKOUT C,255,(A,ARYSDF),FILTROUT=TSTNLEFT                               
Array    LKOUT C,255,(A,ARYNWK),FILTROUT=TSTCABLE                               
CSpcf    LKOUT C,33,(D,B#SAVED,STACLT),CHAR,ND=Y,PCVERSION=3.0.0.120            
Midas    LKOUT C,34,(D,B#SAVED,STAMIDS),CHAR,ND=Y,PCVERSION=3.0.0.120           
PrntPls  LKOUT C,35,(D,B#SAVED,STAPPLUS),CHAR,ND=Y                              
                                                                                
         LKOUT E                                                                
                                                                                
TSTNLEFT CLI   DLDSPLST,0          TEST ONLY DOWNLOADING SPILL                  
         BNER  RE                  YES, SET FALSE AND DON'T D/L                 
         CLI   STALEFTS,0          SET TRUE IF NOT PROCESSING LEFTOVERS         
         BR    RE                                                               
                                                                                
TSTCABLE CLI   STASTA,C'0'         SET CC=EQUAL IF CABLE STATION                
         BL    RTRNNO                                                           
         B     RTRNYES                                                          
                                                                                
TSTSTABT CLI   STABTYP,0                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* SEND STATION VALUES                                                 *         
***********************************************************************         
                                                                                
NXTMST   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTMST06                                                         
                                                                                
         ICM   RF,7,AMED           SET EBCDIC MEDIA                             
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         MVC   BUYMED,QMEDA                                                     
                                                                                
         ICM   RF,7,ACLT           SET EBCDIC CLIENT CODE                       
         MVC   QCLTX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTCLT,AEDTCLT),DMCB,QCLTX,,QCLTA                              
                                                                                
         ICM   RE,7,AMKT           SET EBCDIC STATION MARKET FILTER             
         SR    R0,R0                                                            
         ICM   R0,3,LW_DATA1-LW_D(RE)                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  STAMKTNO,DUB                                                     
                                                                                
         ICM   R2,15,ASTALSTX      SAVE ALL 'K' CALL LETTERS IN WMP             
         JNZ   NXTMST01                                                         
         L     R2,ASTALST                                                       
         LA    R2,LW_LN2Q(R2)      DON'T STEP ON ASTALST                        
NXTMST01 ST    R2,AKSTALST         SET A(STATION PASSIVE LIST)                  
         USING LW_D,R2             BUT WE NEED TO FIND WHERE IN WMP             
         LA    R3,LW_DATA2         R3=A(STATION LIST)                           
         SR    R0,R0               R0=NUMBER OF STATIONS IN LIST                
         XC    WORK(L'STKKSTA),WORK                                             
                                                                                
NXTMST02 GOTOR (#NXTREC,ANXTREC),DMCB,MSTKEYT,('B#STAREC',0),          +        
               (X'80',SAVED),0,0                                                
         JNE   NXTMST04                                                         
         L     R1,IOADDR                                                        
         USING STARECD,R1                                                       
         OC    STKKCLT,STKKCLT     TEST DEFAULT (ALL AGENCY) STATION            
         JZ    *+14                                                             
         CLC   STKKCLT,QCLTA       TEST REQUESTED CLIENT IF NOT                 
         JNE   NXTMST02                                                         
                                                                                
         CLC   STKKSTA,WORK        TEST ALREADY HAVE THIS STATION               
         JE    NXTMST02                                                         
         MVC   WORK(L'STKKSTA),STKKSTA                                          
         LR    RE,R3               MAKE SURE WE HAVE ENOUGH ROOM                
         AHI   RE,L'STKKSTA        BUMP TO NEXT STATION LIST ENTRY              
         L     RF,LP_AWMPX         MAKE SURE WE'RE NOT CLOBBERING WHAT          
         CR    RE,RF                 IS BEYOND  AWMP                            
         JH    *+2                                                              
*                                                                               
         MVC   0(L'STKKSTA,R3),STKKSTA                                          
         LR    R3,RE                                                            
*                                                                               
         AHI   R0,1                BUMP NUMBER OF ENTRIES                       
         J     NXTMST02                                                         
                                                                                
NXTMST04 MVI   LP_RMODE,LP_RNEXT   RESET MODE                                   
         STCM  R0,3,LW_NUMN        SET NUMBER OF STATIONS FOUND                 
         XC    STANUMN,STANUMN     SET NO ENTRIES PROCESSED                     
         XC    PMKT#,PMKT#         INITIALIZE MARKET OPTIMIZER                  
         MVI   STALEFTS,0          SET NOT PROCESSING LEFTOVERS                 
*                                                                               
         L     R2,AKSTALST         R2=A(STATION LIST WMP ENTRY)                 
         XR    RE,RE                                                            
         ICM   RE,3,LW_NUMN                                                     
         MHI   RE,L'STKKSTA                                                     
         LA    RE,LW_DATA2(RE)                                                  
*                                                                               
         ST    RE,ASPMKLST         SPILL MARKET LIST TO GO HERE                 
         XC    0(LW_LN2Q,RE),0(RE)                                              
         MVI   LW_NUMN+1-LW_D(RE),0                                             
         MVC   LW_DATA2-LW_D(2,RE),=X'FFFF'  EOT                                
         MVC   LW_DATA2+(MXSPLMKS*2)-LW_D(2,RE),=X'FFFF'                        
         LA    RE,LW_DATA2+(MXSPLMKS*2)+2-LW_D(RE)                              
                                                                                
         ST    RE,ASTEQLST         CLT-SPECIFIC MGREQ STATIONS GO HERE          
         XC    0(LW_DATA2-LW_D,RE),0(RE)                                        
         MVC   LW_DATA2-LW_D(L'STEKSTA,RE),EFFS   EOT                           
                                                                                
***********************************************************************         
* PROCESS FIRST/NEXT STATION STATION PASSIVE LIST                     *         
***********************************************************************         
                                                                                
NXTMST06 L     R2,AKSTALST         R2=A(STATION LIST WMP ENTRY)                 
         XC    STAVALS(STAVALL),STAVALS                                         
         LA    RF,STAVALS                                                       
         ST    RF,LP_ADATA         POINT TO SEND VALUES                         
         CLI   STALEFTS,0          TEST PROCESSING LEFTOVERS                    
         JNE   NXTMST16                                                         
                                                                                
NXTMST08 SR    R1,R1               BUMP STATION NUMBER                          
         ICM   R1,3,STANUMN                                                     
         LA    R0,1(R1)                                                         
         CLM   R0,3,LW_NUMN        TEST ALL ENTRIES PROCESSED                   
         JH    NXTMST14                                                         
         STCM  R0,3,STANUMN                                                     
         MHI   R1,L'STKKSTA                                                     
         LA    R1,LW_DATA2(R1)     POINT TO NEXT STATION IN LIST                
         MVC   STASTA(L'STKKSTA),0(R1)                                          
         GOTOR GETSCL                                                           
         L     R1,AIO3                                                          
         CLC   SMKT,STAMKTNO       TEST STATION IN CORRECT MARKET               
         JNE   NXTMST08                                                         
                                                                                
         ICM   RE,15,ASTALST       ANY STATIONS THAT HAVE BUYS?                 
         JZ    NXTMST12            NO                                           
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(RE)   # OF STATION THAT HAVE BUYS              
         JZ    NXTMST12                                                         
         LA    RF,LW_DATA2-LW_D(RE)    RF=A(1ST STATION IN LIST)                
NXTMST10 CLC   STAORIG,0(RF)                                                    
         JNE   *+12                                                             
         OI    L'STAORIG(RF),X'80' CHECK MARK FOR THIS STATION                  
         J     NXTMST12                                                         
         AHI   RF,L'STAORIG+1                                                   
         JCT   R0,NXTMST10                                                      
                                                                                
NXTMST12 CLI   DLDSPLST,0          TEST DOWNLOADING SPILL ONLY                  
         JNE   NXTMST08            YES - DON'T SEND STATIONS                    
         J     NXTMST20                                                         
                                                                                
***********************************************************************         
* PROCESS LEFTOVER STATIONS                                           *         
***********************************************************************         
                                                                                
NXTMST14 MVI   STALEFTS,1          SET PROCESSING LEFTOVERS                     
         XC    STANUMN,STANUMN     INITIALIZE STATION COUNTER                   
                                                                                
NXTMST16 ICM   R2,15,ASTALST       POINT TO STATIONS FOR BUYS                   
         BZ    NOMORE                                                           
                                                                                
NXTMST18 SR    R1,R1               POINT TO NEXT STATION TO INSPECT             
         ICM   R1,3,STANUMN                                                     
         LA    R0,1(R1)                                                         
         CLM   R0,3,LW_NUMN        TEST ALL STATIONS PROCESSED                  
         BH    NOMORE                                                           
         STCM  R0,3,STANUMN                                                     
         MHI   R1,L'STAPQSTA+1                                                  
         LA    R1,LW_DATA2(R1)                                                  
         TM    L'STAPQSTA(R1),X'80' TEST ALREADY SENT THIS STATION              
         JNZ   NXTMST18                                                         
         OI    L'STAPQSTA(R1),X'80'                                             
         MVC   STASTA(L'STAPQSTA),0(R1)                                         
         GOTOR GETSCL                                                           
         L     R1,AIO3                                                          
                                                                                
***********************************************************************         
* SET STATION/MARKET VALUES                                           *         
***********************************************************************         
                                                                                
NXTMST20 CLI   STALEFTS,0          TEST PROCESSING LEFTOVERS                    
         JNE   NXTMST24            YES, COPY STATION INTO IO7                   
         CLI   STASTA,C'0'         IS THIS A CABLE SYSCODE?                     
         JL    NXTMST26            NO, DON'T HAVE TO KEEP RECORD AROUND         
         CLC   STAKCLT,EZEROS      WE HAVE CLIENT SPECIFIC?                     
         JE    NXTMST24            NO, PLAIN VANILLA HAS THE NETWORKS           
         L     RE,AIO3                                                          
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'STAKEY),0(RE)       GET THE STATION 'S' KEY              
         LA    R1,IOKEY                                                         
         MVC   STAKCLT,EZEROS      NEED PLAIN VANILLA FOR THE NETWORKS          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO7'                            
         J     NXTMST26                                                         
                                                                                
NXTMST24 L     RE,AIO3             COPY STATION RECORD TO IO7                   
         L     R0,AIO7                                                          
         XR    RF,RF                                                            
         ICM   RF,3,STAKLEN                                                     
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
NXTMST26 L     R1,AIO3                                                          
         CLC   PMKT#,SMKT          TEST CHANGE OF MARKET?                       
         JE    NXTMST28            NO, NO NEED TO READ MKT RECORD THEN          
         MVC   PMKT#,SMKT                                                       
                                                                                
         PACK  DUB,SMKT                                                         
         CVB   RE,DUB                                                           
         STCM  RE,3,STAMKT#                                                     
         GOTOR GETMKT,STAMKT#      READ MARKET RECORD                           
                                                                                
         CLI   QMEDA,C'T'          MEDIA T?                                     
         JNE   NXTMST28                                                         
         OC    STAAMKT,STAAMKT     HAVE ALPHA MARKET?                           
         JZ    NXTMST28                                                         
         GOTOR GTCOMMKT            GET COMSCORE MARKET                          
                                                                                
NXTMST28 B     EXITY               EXIT TO SEND STATION VALUES                  
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION EQUIVALENCE FOR A MARKET               *         
***********************************************************************         
*&&DO                                                                           
ARYSTQ   LKOUT A,(R,NXTSTQ),MULTIROW=Y,ROWNAME=STAVALS                          
                                                                                
Media    LKOUT C,1,STAMEDIA,CHAR,ND=Y                                           
MktNo    LKOUT C,2,STAMKT#,LBIN,ND=Y                                            
Statn    LKOUT C,3,STASTA,CHAR                                                  
MktNm    LKOUT C,4,STAMKTN,CHAR,ND=Y                                            
Chanl    LKOUT C,5,STACHNL,CHAR,ND=Y                                            
NwAff    LKOUT C,6,STANTWK,CHAR,ND=Y                                            
Frmat    LKOUT C,7,STAFRMT,CHAR,ND=Y                                            
TaxRt    LKOUT C,9,STATAX,CHAR,ND=Y                                             
BkTyp    LKOUT C,10,STABTYP,(R,TRNSBKT),ND=Y                                    
MktAC    LKOUT C,11,STAAMKT,CHAR,ND=Y                                           
MkLPM    LKOUT C,12,STALPMD,CDAT,ND=Y                                           
Array    LKOUT C,255,(A,ARYPC2)                                                 
Array    LKOUT C,255,(A,ARYSDF)                                                 
MkGID    LKOUT C,21,STAEQMKT,CHAR,ND=Y                                          
MkGrp    LKOUT C,22,STAEQMK#,(R,EDTMGRP),LEN=L'STEMGRP*2,ND=Y                   
Array    LKOUT C,255,(A,ARYNWK),FILTROUT=TSTCABLE                               
                                                                                
         LKOUT E                                                                
*&&                                                                             
***********************************************************************         
* READ STATION EQUIVALENCE RECORDS                                    *         
***********************************************************************         
***********************************************************************         
* ONLY THE X'014A'(NEW BUY D/L) AND X'0145'(STATION LIST) CALL NXTSTQ           
*                                                                               
* @NXSTQ080, THE ONLY STATION-EQUIV (AKA MGREQ) RECORDS D/L'D ARE ONES          
* THAT MATCH THE MARKET REQUESTED, AND MATCH CLTIDRQ (CLT REQUEST)              
*                                                                               
* THE PROBLEM IS, IF COMING FROM THE X'0145', CLIENT IS NOT PART OF THE         
* REQUEST, SO CLTIDRQ IS NEVER SET, SO A MATCH WILL NEVER OCCURS,               
* WHICH RESULTS IN X'0145' NEVER D/L STATION-EQUIV(AKA MGREQ)                   
*                                                                               
* I SPOKE WITH JULIE CHEA, AND BASED ON CHANGE-CONTROL DOC SHE FOUND,           
* SHE BELIEVES THIS PROJECT WAS NEVER COMPLETED.                                
* JULIA BROYTMAN CONFIRMED, THIS IS NOT SUPPORTED BY SBTK                       
                                                                                
*                    ** CODE COMMENTED OUT **          -HWON 11/17/16           
***********************************************************************         
*&&DO                                                                           
NXTSTQ   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXSTQ010                                                         
         XC    PMKT##,PMKT##       INITIALIZE MARKET OPTIMIZER                  
         MVI   STEQMODE,0                                                       
                                                                                
NXSTQ010 DS    0H                                                               
         CLI   STEQMODE,FF         NOW LOOK THRU 'ALL' CLT MGREQ RECS           
         JE    NXSTQ020                                                         
         GOTOR (#NXTREC,ANXTREC),DMCB,STQCLTT,('B#STEREC',0),SAVED,0,0          
         JE    NXSTQ030                                                         
         MVI   STEQMODE,FF         NOW LOOK THRU 'ALL' CLT MGREQ RECS           
*                                  FORCE NXTREC AT NXSTQ020 TO START            
         MVI   LP_RMODE,LP_RFRST      AS IF FIRST TIME                          
                                                                                
NXSTQ020 GOTOR (#NXTREC,ANXTREC),DMCB,STQALLT,('B#STEREC',0),SAVED,    +        
               AFLTSTQ,0                                                        
         JNE   NXTSTQY                                                          
                                                                                
NXSTQ030 LA    R1,STAVALS                                                       
         ST    R1,LP_ADATA                                                      
                                                                                
         L     R2,IOADDR                                                        
         USING STEKEY,R2                                                        
         MVC   STASTA,STEKSTA      SET STATION CODE                             
                                                                                
         CLI   STEQMODE,FF         LOOKING THRU 'ALL' CLT MGREQ RECS?           
         JE    NXSTQ050            YES, AT THIS MOMENT                          
         ICM   RE,15,ASTEQLST      PUT CLT-SPECIFIC STATION IN OUR LIST         
         JZ    *+2                 SHOULDN'T EVER BE                            
         USING LW_D,RE                                                          
         LA    RF,LW_DATA2                                                      
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        # OF CLT-SPECIFIC MGREQ SO FAR               
         LR    R1,R0                                                            
         MHI   R0,L'STEKSTA                                                     
         AR    RF,R0                                                            
         MVC   0(L'STEKSTA,RF),STEKSTA                                          
         AHI   R1,1                                                             
         STCM  R1,3,LW_NUMN                                                     
         J     NXSTQ050                                                         
*&&DO                                                                           
NXSTQ040 ICM   RE,15,ASTEQLST      WE'VE SEEN CLT-SPECIFIC ALREADY?             
         USING LW_D,RE                                                          
         LA    RF,LW_DATA2                                                      
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        # OF CLT-SPECIFIC MGREQ SO FAR               
         JZ    NXSTQ050                                                         
NXSTQ045 CLC   0(L'STEKSTA,RF),STEKSTA                                          
         JE    NXSTQ010                                                         
         LA    RF,L'STEKSTA(RF)                                                 
         JCT   R0,NXSTQ045                                                      
         DROP  RE                                                               
*&&                                                                             
NXSTQ050 LA    R2,STEFEL           ONLY WANT REC IF ELEM HAS OUR MKT            
         USING STEMGEL,R2                                                       
NXSTQ055 CLI   STEMGEL,0                                                        
         JE    NXSTQ010            CAN'T USE THIS RECORD                        
         CLI   STEMGEL,STEMGELQ                                                 
         JE    NXSTQ080                                                         
NXSTQ060 LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         J     NXSTQ055                                                         
                                                                                
NXSTQ080 ICM   RF,7,AMKT                                                        
         CLC   STEMGMKT,LW_DATA1-LW_D(RF)  TEST MARKET IN THIS ELEM             
         JNE   NXSTQ060                                                         
***********************************************************************         
* AS A TEST, I ADDED CODE TO BYPASS CLTIDRQ CHECK        -HWON 11/17/16         
***********************************************************************         
         CLC   LP_QMAPN,SDNBUYD#   NEW BUY/GOAL DOWNLOAD                        
         JNE   NXSTQ090                                                         
                                                                                
         CLC   CLTIDRQ,STEMGID     & MARKET ID TO MATCH CLT'S BUY ID            
         JNE   NXSTQ060                                                         
                                                                                
NXSTQ090 GOTOR GETSCL              RESOLVE STATION VALUES                       
                                                                                
         MVC   STAEQMKT,STEMGID    STATION EQ MARKET ID                         
         MVC   STAEQMK#,STEMGRP    STATION EQ MARKET NUMBER                     
                                                                                
         CLC   PMKT##,STEMGMKT     TEST ALREADY SENT MARKET VALUES              
         JE    NXTSTQY                                                          
         MVC   PMKT##,STEMGMKT                                                  
                                                                                
         MVC   STAMKT#,STEMGMKT                                                 
         GOTOR GETMKT,STAMKT#      READ MARKET RECORD                           
                                                                                
NXTSTQY  B     EXITY                                                            
         DROP  R2                                                               
*&&                                                                             
***********************************************************************         
* ARRAY DEFINITION FOR SPILL MARKETS FOR A STATION                    *         
***********************************************************************         
                                                                                
ARYSDF   LKOUT A,(R,NXTSDF),ROWNAME=SDEFRECD                                    
                                                                                
ARRAY    LKOUT C,255,(A,ARYSPL)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* READ SPILL DEFINITION RECORD                                        *         
***********************************************************************         
                                                                                
NXTSDF   MVC   LP_ADATA,AIO7                                                    
         LA    R2,IOKEY                                                         
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSDF04                                                         
         CLC   LP_QMAPN,SDNBUYD#   X'014A' -  NEW BUY/GOAL DOWNLOAD?            
         JNE   NXTSDF01                                                         
         L     RF,ACLTREC                                                       
         USING CLTRECD,RF                                                       
         CLI   CEXTRA+5,YESQ       CLIENT SUPPORTS US-SPILL?                    
         JNE   NOMORE              NO, DON'T SEND IT                            
         DROP  RF                                                               
*                                                                               
NXTSDF01 MVC   SVIOVALS,IOVALS     SAVE CURRENT I/O VALUES                      
         USING SDEFKEY,R2                                                       
         XC    SDEFKEY,SDEFKEY                                                  
         MVI   SDEFKTY,SDEFKTYQ                                                 
         MVI   SDEFKSB,SDEFKSBQ                                                 
         MVC   SDEFKAGY,AGENCY                                                  
         MVI   SDEFKRSV,C'0'       START WITH NSI FIRST                         
         MVC   SDEFKSTA,STASTA                                                  
                                                                                
         CLI   SDEFKSTA+4,C' '                                                  
         JE    *+12                                                             
         CLI   SDEFKSTA+4,C'T'                                                  
         JNE   *+8                                                              
         MVI   SDEFKSTA+4,0                                                     
                                                                                
         MVC   SDEFKCLT,QCLTX                                                   
                                                                                
NXTSDF02 GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO7'                               
         CLC   IOKEY(L'SDEFKTYP),IOKEYSAV                                       
         JNE   NXTSDF06                                                         
                                                                                
         CLC   IOKEY(L'SDEFKEY),IOKEYSAV                                        
         JE    NXTSDF10                                                         
                                                                                
         MVC   IOKEY,IOKEYSAV                                                   
         CLC   SDEFKCLT,BZEROS     WE CHECKED ALL CLT YET?                      
         JE    NXTSDF04            YES                                          
         MVC   SDEFKCLT,BZEROS     CHECK ALL CLIENT THEN                        
         J     NXTSDF02                                                         
                                                                                
NXTSDF04 CLI   SDEFKRSV,C'1'       WENT THROUGH BOTH RATING SVC?                
         JNE   NXTSDF08                                                         
                                                                                
NXTSDF06 MVC   IOVALS(IOVALL),SVIOVALS                                          
         B     NOMORE                                                           
                                                                                
NXTSDF08 MVI   SDEFKRSV,C'1'                                                    
         MVC   SDEFKCLT,QCLTX                                                   
         J     NXTSDF02                                                         
                                                                                
NXTSDF10 MVC   STARSV,SDEFKRSV                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO7'                              
         B     MORE                                                             
         DROP  R2                                                               
                                                                                
ARYSPL   LKOUT A,(D,B#SOFREC,DFLEL),EOT=EOR,                           +        
               ROWID=(SDEFEL05,SDEFELQ),ROWWIDTH=(V,SDEF5LEN)                   
                                                                                
RtgSv    LKOUT C,23,(D,B#SAVED,STARSV),CHAR,ND=Y                                
RtgMk    LKOUT C,24,SDEFRMKT,UBIN,ND=Y                                          
AgyMk    LKOUT C,25,SDEFAMKT,UBIN,ND=Y,FILTROUT=TSTASPMK                        
AlfMk    LKOUT C,26,SDEFALPH,CHAR,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
TSTASPMK L     R1,LP_AINP          TEST ADD SPILL AGENCY MARKET #               
         USING SDEFEL05,R1                                                      
         ICM   RE,15,ASPMKLST      RE=A(SPILL AGENCY MKT LIST)                  
         BZ    EXITY                                                            
         USING LW_D,RE                                                          
         LA    RF,LW_DATA2         POINT TO SPL MKT LIST                        
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET COUNT OF SPL MKT SO FAR                  
         JZ    TSTASM04                                                         
TSTASM02 CLC   0(L'SDEFAMKT,RF),SDEFAMKT  WE ALREADY HAVE THIS MKT?             
         BE    EXITY                      YES, DON'T WANT DUPS                  
         AHI   RF,L'SDEFAMKT                                                    
         JCT   R0,TSTASM02                                                      
                                                                                
         ICM   R0,3,LW_NUMN        ADD NEW ENTRY TO END OF LIST                 
TSTASM04 AHI   R0,1                                                             
         CHI   R0,MXSPLMKS         MAX # SPL MKTS A MARKET CAN SPILL TO         
         JH    *+2                                                              
MXSPLMKS EQU   80                                                               
*                                                                               
TSTASM06 STCM  R0,3,LW_NUMN                                                     
         MVC   0(L'SDEFAMKT,RF),SDEFAMKT                                        
         LA    RF,L'SDEFAMKT(RF)                                                
         MVC   0(L'SDEFAMKT,RF),=X'FFFF'                                        
         B     EXITY                                                            
         DROP  R1,RE                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MSTREET FORMAT RECORDS                                   
***********************************************************************         
                                                                                
ARYFMT   LKOUT A,(R,NXTFMT),MULTIROW=Y,ROWNAME=CT99KEY                          
                                                                                
MedCod   LKOUT C,1,CT99KFME,CHAR,ND=Y                                           
FmtCod   LKOUT C,2,(D,B#SAVED,HASHFCOD),CHAR,ND=Y                               
Array    LKOUT C,255,(A,ARYFMN)                                                 
                                                                                
         LKOUT E                                                                
                                                                                
ARYFMN   LKOUT A,(D,B#MSTREC,CT99DATA),EOT=EOR,ROWID=(CFNAMD,CFNMELQ), *        
               ROWWIDTH=(V,CFNMLN)                                              
FmtNam   LKOUT C,3,CFNAME,CHAR,ND=Y                                             
         LKOUT E                                                                
***********************************************************************         
* READ MSTREET FORMAT RECORD                                                    
***********************************************************************         
                                                                                
NXTFMT   MVC   LP_ADATA,AIO7                                                    
         XC    HASHFCOD,HASHFCOD                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTFMT10            NO, NEXT FORMAT RECORD                       
*                                                                               
         BRAS  RE,GETACC           THIS BREAKS READ SEQ                         
         CLI   USESODTA,C'Y'                                                    
         BNE   NOMORE                                                           
         MVI   DIGSRCE,0                                                        
         MVC   USNGSRCE,OWNRSRCE   Default ownership source                     
*                                                                               
NXTFMT10 GOTOR (#NXTREC,ANXTREC),DMCB,MSTFKEYT,('B#MSTREC',0),         +        
               ('$NXTRCTF+$NXTRXAD',SAVED),0,0                                  
         JNE   NXTFMT20                                                         
*                                                                               
         L     RE,AIO7                                                          
         USING CT99KEY,RE                                                       
         LA    RF,HASHFCOD                                                      
         CLC   LP_VRSN,V450019                                                  
         JL    NXTFMT15                                                         
         MVC   0(L'CT99KSRC,RF),CT99KSRC                                        
         LA    RF,L'CT99KSRC(RF)                                                
NXTFMT15 MVC   0(L'CT99KFRM,RF),CT99KFRM                                        
         J     MORE                                                             
         DROP  RE                                                               
*                                                                               
NXTFMT20 CLI   USNGSRCE,CT99KODS   Did we do the digital source yet?            
         JE    NOMORE              yes, we're done                              
         MVI   DIGSRCE,CT99KODS    no, do them now                              
         MVC   USNGSRCE,DIGSRCE                                                 
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         J     NXTFMT10                                                         
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MSTREET OWNER RECORDS                                    
***********************************************************************         
                                                                                
ARYOWN   LKOUT A,(R,NXTOWN),MULTIROW=Y,ROWNAME=CT99KEY                          
                                                                                
MedCod   LKOUT C,1,CT99KOME,CHAR,ND=Y                                           
OwnCod   LKOUT C,2,(D,B#SAVED,HASHOCOD),CHAR,ND=Y                               
Array    LKOUT C,255,(A,ARYOWNN)                                                
                                                                                
         LKOUT E                                                                
                                                                                
ARYOWNN  LKOUT A,(D,B#MSTREC,CT99DATA),EOT=EOR,ROWID=(CONAMD,CONMELQ), *        
               ROWWIDTH=(V,CONMLN)                                              
OwnNam   LKOUT C,3,CONAME,CHAR,ND=Y                                             
         LKOUT E                                                                
***********************************************************************         
* READ MSTREET OWNER RECORD                                                     
***********************************************************************         
                                                                                
NXTOWN   MVC   LP_ADATA,AIO7                                                    
         XC    HASHOCOD,HASHOCOD                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTOWN10            NO, NEXT FORMAT RECORD                       
*                                                                               
         BRAS  RE,GETACC           THIS BREAKS READ SEQ                         
         CLI   USESODTA,C'Y'                                                    
         BNE   NOMORE                                                           
         MVC   USNGSRCE,OWNRSRCE                                                
         MVI   DIGSRCE,0                                                        
*                                                                               
***  MSTOKEYT was changed to only get radio so that initial download            
***    doesn't overflow the worker file                                         
*                                                                               
NXTOWN10 GOTOR (#NXTREC,ANXTREC),DMCB,MSTOKEYT,('B#MSTREC',0),         +        
               ('$NXTRCTF+$NXTRXAD',SAVED),0,0                                  
         JNE   NXTOWN20                                                         
         L     RE,AIO7                                                          
         USING CT99KEY,RE                                                       
         LA    RF,HASHOCOD                                                      
         CLC   LP_VRSN,V450019                                                  
         JL    NXTOWN15                                                         
         MVC   0(L'CT99KSRC,RF),CT99KSRC                                        
         LA    RF,L'CT99KSRC(RF)                                                
NXTOWN15 MVC   0(L'CT99KOWN,RF),CT99KOWN                                        
         J     MORE                                                             
         DROP  RE                                                               
*                                                                               
NXTOWN20 CLI   USNGSRCE,CT99KODS   DID WE DO THE DIGITAL SOURCE YET?            
         JE    NOMORE              Yes, we're done                              
         MVI   DIGSRCE,CT99KODS                                                 
         MVC   USNGSRCE,DIGSRCE    No, do them now                              
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         J     NXTOWN10                                                         
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MSTREET STATION RECORDS                                  
***********************************************************************         
                                                                                
ARYRAD   LKOUT A,(R,NXTRAD),MULTIROW=Y,ROWNAME=CT99KEY                          
Statn    LKOUT C,1,(D,B#SAVED,STASTA),CHAR                                      
Array    LKOUT C,255,(A,ARYRFQ)                                                 
Array    LKOUT C,255,(A,ARYRFM)                                                 
         LKOUT E                                                                
*                                                                               
ARYRFQ   LKOUT A,(D,B#MSTREC,CT99DATA),EOT=EOR,ROWID=(CRCLD,CRCLELQ),  +        
               ROWWIDTH=(V,CRCLLN)                                              
MSTFrq   LKOUT C,2,CRCLFRQ,CHAR,ND=Y                                            
         LKOUT E                                                                
*                                                                               
ARYRFM   LKOUT A,(D,B#MSTREC,CT99DATA),EOT=EOR,ROWID=(CRFMD,CRFMELQ),  +        
               ROWWIDTH=(V,CRFMLN)                                              
PRout    LKOUT P,,HASHOWNR                                                      
FmtCod   LKOUT C,3,(D,B#SAVED,HASHFCOD),CHAR,ND=Y                               
OwnrCod  LKOUT C,4,(D,B#SAVED,HASHOCOD),CHAR,ND=Y                               
         LKOUT E                                                                
***********************************************************************         
* READ MSTREET STATION RECORDS                                                  
***********************************************************************         
                                                                                
NXTRAD   MVC   LP_ADATA,AIO7                                                    
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXRAD10                                                          
         XC    STANUMN,STANUMN     INITIALIZE STATION COUNTER                   
*                                                                               
         BRAS  RE,GETACC                                                        
                                                                                
         CLI   USESODTA,C'Y'                                                    
         BNE   NOMORE                                                           
*                                                                               
NXRAD10  MVI   DIGSRCE,0           DON'T USE DDS SOURCE FOR OWNERSHIP           
         ICM   R2,15,AKSTALST      STATION LIST                                 
         BZ    NOMORE                                                           
         USING LW_D,R2                                                          
         SR    R1,R1               POINT TO NEXT STATION TO INSPECT             
         ICM   R1,3,STANUMN                                                     
         LA    R0,1(R1)                                                         
         CLM   R0,3,LW_NUMN        TEST ALL STATIONS PROCESSED                  
         BH    NOMORE                                                           
         STCM  R0,3,STANUMN                                                     
         MHI   R1,L'STAPQSTA                                                    
         LA    R1,LW_DATA2(R1)                                                  
         MVC   STASTA(L'STAPQSTA),0(R1)                                         
         DROP  R2                                                               
*                                                                               
         LA    R4,IOKEY                                                         
         USING CT99KEY,R4                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   CT99KTYP,CT99KTYQ   MSTREET RECORD                               
         MVI   CT99KSUB,CT99KSRA   X'00' - SUBTYPE FOR STATION REC              
         MVC   CT99KSRC,OWNRSRCE   OWNER SOURCE                                 
         MVC   CT99KUID(L'STAPQSTA),STASTA                                      
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE+IO7'                            
         JNE   *+2                                                              
         CLC   IOKEY(L'CT99KEY),IOKEYSAV                                        
         JE    MORE                                                             
*************************************                                           
* WE MIGHT HAVE STREAMING STATIONS OF SOMETHING OF THE SORT                     
*************************************                                           
         LA    R4,IOKEY                                                         
         USING CT99KEY,R4                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   CT99KTYP,CT99KTYQ   MSTREET RECORD                               
         MVI   CT99KSUB,CT99KSRA   X'00' - SUBTYPE FOR STATION REC              
         MVI   CT99KSRC,CT99KODS   C'D' - POSSIBLE STREAMING STATIONS           
         MVC   CT99KUID(L'STAPQSTA),STASTA                                      
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE+IO7'                            
         JNE   *+2                                                              
         CLC   IOKEY(L'CT99KEY),IOKEYSAV                                        
         JNE   NXRAD20                                                          
         MVI   DIGSRCE,CT99KODS   USING DS SOURCE FOR OWNERSHIP                 
         J     MORE                                                             
*************************************                                           
*                                                                               
NXRAD20  L     R1,AIO7               DUMMY UP A RECORD FOR SPOT DESKTOP         
         MVC   0(L'CT99KEY,R1),IOKEYSAV                                         
         MVC   CT99LEN-CT99KEY(2,R1),=AL2(CT99DATA+1-CT99KEY)                   
         MVI   CT99DATA-CT99KEY(R1),0                                           
         B     MORE                                                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
* HASH the owner source with the code                                           
***********************************************************************         
                                                                                
HASHOWNR DS    0H                                                               
         XC    HASHFCOD,HASHFCOD    CLEAR OUT THE HASHED FORMAT AND             
         XC    HASHOCOD,HASHOCOD                         OWNER CODES            
         L     R2,AIO7                                                          
         USING CT99KEY,R2                                                       
*                                                                               
         L     R6,LP_AINP           R6 = A(X'02' ELEM)                          
         USING CRFMD,R6                                                         
         OC    CRFMFMT,CRFMFMT                                                  
         JZ    HOWNR10                                                          
         LA    RF,HASHFCOD                                                      
         CLC   LP_VRSN,V450019                                                  
         JL    HOWNR05                                                          
         MVC   0(L'CT99KSRC,RF),CT99KSRC                                        
         LA    RF,L'CT99KSRC(RF)                                                
HOWNR05  MVC   0(L'CRFMFMT,RF),CRFMFMT                                          
*                                                                               
HOWNR10  OC    CRFMOWN,CRFMOWN                                                  
         JZ    MORE                                                             
         LA    RF,HASHOCOD                                                      
         CLC   LP_VRSN,V450019                                                  
         JL    HOWNR15                                                          
         MVC   0(L'CT99KSRC,RF),CT99KSRC                                        
         LA    RF,L'CT99KSRC(RF)                                                
HOWNR15  MVC   0(L'CRFMOWN,RF),CRFMOWN                                          
         B     MORE                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR CLIENT EQUIVALENCY VALUES DOWNLOAD             *         
***********************************************************************         
                                                                                
ARYEQU   LKOUT A,(D,B#SAVED,CLTEQU),EOT=EOR                                     
                                                                                
SptLn    LKOUT C,16,CLTEQU,LBIN,LEN=1                                           
EqFac    LKOUT C,17,CLTEQU+1,LBIN,LEN=L'CLTEQU-1                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CABLE NETWORK LIST                             *         
***********************************************************************         
                                                                                
ARYNWK   LKOUT A,(R,NXTNWK)                                                     
                                                                                
DDSNwk   LKOUT C,30,(D,B#SAVED,STADNWK),CHAR,ND=Y                               
NSINwk   LKOUT C,31,(D,B#SAVED,STANNWK),CHAR,ND=Y                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* PUT OUT THE CABLE NETWORK CODES (DDS & NIELSEN)                     *         
***********************************************************************         
                                                                                
NXTNWK   LA    RF,STAVALS          BECAUSE NXTREC IS NOT CALLED                 
         ST    RF,LP_ADATA         THIS HAS TO BE MANUALLY SET                  
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTNWK02                                                         
         MVI   STANWCTR,0          STATION'S NETWORK COUNTER                    
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING STAPACKD,R1                                                      
         MVI   STAPACT-STAPACKD(R1),C'P'                                        
         MVC   STAPACOM-STAPACKD(L'STAPACOM,R1),ACOMFACS                        
         MVC   STAPAGY,LP_AGY                                                   
         MVI   STAPMED,C'T'                                                     
         MVC   STAPQMKT,EZEROS                                                  
         MVC   STAPQSTA,STASTA                                                  
         MVI   STAPQSTA+4,C'T'                                                  
         GOTOR VSTAPACK,WORK2                                                   
         MVC   STAPCODE,STAPSTA    SAVE BINARY CABLE W/O NETWORK                
         DROP  R1                                                               
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    *+12                 NO, ONLINE SHOULD ALWAYS GET ADDR           
         ICM   RF,15,ACBLLIST                                                   
         JNZ   NXNWK00                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         ICM   R0,15,=X'D9000A9E'  GET SPCBLLIST                                
         GOTOR (RF),DMCB,0,(R0),0                                               
         L     RF,0(R1)            RF = A(SPCBLLIST)                            
         ST    RF,ACBLLIST                                                      
*                                                                               
NXNWK00  MVC   BYTE,5(RF)          5(RF) IS L'CABLE NETWORK ENTRY               
         NI    BYTE,X'FF'-X'80'    BUT IT WAS MODIFIED IN STAPACK:MS030         
         LLC   R1,BYTE             BYTE NOW HAS THE CORRECTED LENGTH            
*                                                                               
         LHI   R3,X'02C0'          WE CAN SKIP X'2C0' NETWORKS                  
         XR    R2,R2               ENTRIES START AT X'0001'                     
         MR    R2,R1                 SO AFTER THE SKIP WE SHOULD BE AT          
         AR    R3,RF                 X'02C1'                                    
*                                                                               
         CLC   =X'02C1',3(R3)      MAKE SURE WE SKIPPED CORRECTLY               
         JNE   *+2                   SO WE HAVE A GOOD STARTING POINT           
NXNWK01A LA    RF,0(R1,R3)         RF = A(NEXT ENTRY)                           
         CLC   =X'FFFFFF',0(RF)    EOT?                                         
         JE    NXNWK01G            THEN WE'RE DONE, NO OLD NETWORKS             
         CLC   3(2,RF),3(R3)       IS THE BINARY LESS THAN PREVIOUS?            
         JL    NXNWK01G            YES, WE FOUND START OF OLD NTWKS             
         AR    R3,R1               NO, WE NEED TO FIND THE START                
         J     NXNWK01A                                                         
*                                                                               
NXNWK01G ST    RF,ADUPLIST                                                      
                                                                                
NXTNWK02 L     R2,AIO7                                                          
         USING STARECD,R2                                                       
*                                                                               
         LLC   RE,STANWCTR                                                      
         LA    RE,1(RE)                                                         
         STC   RE,STANWCTR                                                      
                                                                                
**** What if SYSCODE has the full 127 networks?                                 
         CHI   RE,256              this is a boundary check                     
         BNL   NOMORE              we're done                                   
                                                                                
         TM    STANWCTR,X'80'      ARE WE IN DUPLICATES MODE?                   
         JNZ   NXTNWK50            YES, SEND OLD NETWORKS TOO                   
                                                                                
         CHI   RE,24               LOOKING FOR A TOP 24 NETWORK?                
         JH    NXTNWK08            NO, WANT A POSITIONAL NETWORK                
                                                                                
         LA    R3,SCBL24           LET'S SEE IF THIS TOP 24 BIT IS ON           
         LA    R1,X'100'                                                        
NXTNWK04 CHI   RE,8                                                             
         JNH   NXTNWK06                                                         
         SHI   RE,8                                                             
         LA    R3,1(R3)                                                         
         J     NXTNWK04                                                         
                                                                                
NXTNWK06 CLI   0(R3),0             NO BITS ON IN THIS 8-BIT RANGE               
         JE    NXTNWK02                                                         
         SRL   R1,0(RE)                                                         
         STC   R1,BYTE                                                          
         NC    BYTE,0(R3)                                                       
         CLI   BYTE,0              BIT WAS NOT ON?                              
         JE    NXTNWK02            NO, IT WASN'T, CHECK NEXT TOP 24             
         J     NXTNWK10                                                         
                                                                                
NXTNWK08 SHI   RE,25               BECAUSE 1ST POSITIONAL IS 25TH               
         AR    RE,RE               DOUBLE IT                                    
         LA    R3,SCBLSEQ(RE)      POINT TO THE POSITION                        
         OC    0(2,R3),0(R3)       ANYTHING HERE?                               
         JZ    NXTNWK20            WE MIGHT NEED TO PROCESS DUPLICATES          
         CLC   EFFS(2),0(R3)       PLACEHOLDER?                                 
         JE    NXTNWK02            YES, GET NEXT ONE                            
                                                                                
NXTNWK10 XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING STAPACKD,R1                                                      
         MVI   STAPACT-STAPACKD(R1),C'U'                                        
         MVC   STAPACOM-STAPACKD(L'STAPACOM,R1),ACOMFACS                        
         MVC   STAPAGY,LP_AGY                                                   
         MVI   STAPMED,C'T'                                                     
         MVC   STAPSTA,STAPCODE                                                 
         OC    STAPSTA+2(1),STANWCTR                                            
         GOTOR VSTAPACK,WORK2                                                   
         MVC   STADNWK,STAPQNET    DDS 3 CHAR NETWORK                           
         MVI   STAPACT,C'X'        STAPQNET ALREADY SET                         
         GOTOR VSTAPACK,WORK2                                                   
         MVC   STANNWK,STAPQSTA                                                 
         B     MORE                                                             
         DROP  R1                                                               
                                                                                
NXTNWK20 LA    RE,25+128           SET TO DO THE OLD NETWORKS                   
         STC   RE,STANWCTR                                                      
                                                                                
NXTNWK50 SHI   RE,25+128           BECAUSE 1ST POSITIONAL IS 25TH               
         SLL   RE,1                DOUBLE IT                                    
         LA    R3,SCBLSEQ(RE)      POINT TO THE POSITION                        
         OC    0(2,R3),0(R3)       ANYTHING HERE?                               
         BZ    NOMORE              DONE WITH NEW AND OLD CODES                  
         CLC   EFFS(2),0(R3)       PLACEHOLDER?                                 
         JE    NXTNWK02            YES, GET NEXT ONE                            
                                                                                
         L     RF,ACBLLIST         RF = A(SPCBLLIST)                            
         MVC   BYTE,5(RF)          5(RF) IS L'CABLE NETWORK ENTRY               
         NI    BYTE,X'FF'-X'80'    BUT IT WAS MODIFIED IN STAPACK:MS030         
         LLC   R0,BYTE                                                          
*                                                                               
         L     RF,ADUPLIST                                                      
NXTNWK55 CLC   EFFS(3),0(RF)       NO MORE DUPLICATE NETWORKS?                  
         JE    NXTNWK02            NEXT NETWORK IN STATION RECORD               
         CLC   0(2,R3),3(RF)       MATCH ON THE BINARY?                         
         JE    NXTNWK60            YES,                                         
         AR    RF,R0               NO, CHECK NEXT ENTRY                         
         J     NXTNWK55                                                         
*                                                                               
* SEND THE OLD CODE AS WELL                                                     
NXTNWK60 MVC   STADNWK,0(RF)       DDS 3 CHAR NETWORK code                      
         MVC   STANNWK,7(RF)       NCC 4 char network code                      
         B     MORE                                                             
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR SPILL MARKET NAME LIST                         *         
***********************************************************************         
                                                                                
ARYSPM   LKOUT A,(R,NXTSPM)                                                     
                                                                                
SpMk#    LKOUT C,1,(D,B#SAVED,STAMKT#),LBIN,ND=Y                                
MktNm    LKOUT C,2,(D,B#SAVED,STAMKTN),CHAR,ND=Y                                
AlphM    LKOUT C,3,(D,B#SAVED,STAAMKT),CHAR,ND=Y                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* SET UP THE MARKET # SO THAT THE MARKET NAME CAN BE GOTTEN           *         
***********************************************************************         
                                                                                
NXTSPM   LA    RF,STAVALS          BECAUSE NXTREC IS NOT CALLED                 
         ST    RF,LP_ADATA         THIS HAS TO BE MANUALLY SET                  
         ICM   R2,15,ASPMKLST      SPILL MARKET LIST                            
         JZ    NOMORE                                                           
         USING LW_D,R2                                                          
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSPM02                                                         
         OC    LW_NUMN,LW_NUMN                                                  
         JZ    NOMORE                                                           
         MVI   STANWCTR,0                                                       
         LLC   RE,STANWCTR                                                      
         J     NXTSPM04                                                         
                                                                                
NXTSPM02 LLC   RE,STANWCTR                                                      
         LA    RE,1(RE)                                                         
         STC   RE,STANWCTR                                                      
                                                                                
         CLM   RE,3,LW_NUMN                                                     
         JNL   NOMORE                                                           
                                                                                
NXTSPM04 MHI   RE,L'SDEFAMKT                                                    
         LA    R1,LW_DATA2(RE)                                                  
         CLC   =X'FFFF',0(R1)      EOT?                                         
         JE    NOMORE                                                           
         MVC   STAMKT#,0(R1)                                                    
         GOTOR GETMKT                                                           
         J     MORE                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST MSTREET STATION DOWNLOAD    X'00EB'                                   
***********************************************************************         
                                                                                
REQRRD   LKREQ H,I#SDMSTR,OUTRRAD,NEXTREQ=REQPID                                
Statn    LKREQ F,1,(I,B#SAVED,RRADIND),CHAR,LIST=F,                    +        
               OLEN=L'STASTA,MAXLEN=L'STASTA,TEXT=SP#STA,COL=*                  
         LKREQ E                                                                
                                                                                
OUTRRAD  LKOUT H                   ** REQUESTED MSTREET RECORD **               
R4RRAD   LKOUT R,X'0059'                                                        
Array    LKOUT C,255,(A,ARYRRD),PCVERSION=3.2.0.16                              
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR REQUESTED MSTREET STATION DOWNLOAD                       
***********************************************************************         
ARYRRD   LKOUT A,(R,NXTRRD),MULTIROW=Y,ROWNAME=CT99KEY                          
Statn    LKOUT C,1,(D,B#SAVED,STASTA),CHAR                                      
Array    LKOUT C,255,(A,ARYRFQ)                                                 
Array    LKOUT C,255,(A,ARYRFM)                                                 
         LKOUT E                                                                
                                                                                
***********************************************************************         
* READ REQUESTED MSTREET STATION RECORDS                                        
***********************************************************************         
                                                                                
NXTRRD   MVC   LP_ADATA,AIO7                                                    
         SR    R2,R2                                                            
         L     R2,RRADIND                                                       
         USING LW_D,R2                                                          
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXRRD10                                                          
         BRAS  RE,GETACC                                                        
                                                                                
         CLI   USESODTA,C'Y'                                                    
         BNE   NOMORE                                                           
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN                                                     
         CHI   R0,255              HAVE MORE THEN 255?                          
         JH    *+2                  YES, THATS NOT GOOD                         
                                                                                
         XR    R1,R1               START FROM THE BEGINNING                     
         J     NXRRD15                                                          
                                                                                
NXRRD10  MVI   DIGSRCE,0           DON'T USE DDS SOURCE FOR OWNERSHIP           
         XR    R1,R1                                                            
         IC    R1,RRADIND                                                       
NXRRD15  LA    R0,1(R1)                                                         
         CLM   R0,3,LW_NUMN        TEST ALL STATIONS PROCESSED                  
         BH    NOMORE                                                           
         STC   R0,RRADIND                                                       
         MHI   R1,L'STASTA                                                      
         LA    R1,LW_DATA2(R1)                                                  
         MVC   STASTA(L'STASTA),0(R1)                                           
         DROP  R2                                                               
*                                                                               
         LA    R4,IOKEY                                                         
         USING CT99KEY,R4                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   CT99KTYP,CT99KTYQ   MSTREET RECORD                               
         MVI   CT99KSUB,CT99KSRA   X'00' - SUBTYPE FOR STATION REC              
         MVC   CT99KSRC,OWNRSRCE   OWNER SOURCE                                 
         MVC   CT99KUID(L'STAPQSTA),STASTA                                      
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE+IO7'                            
         JNE   *+2                                                              
         CLC   IOKEY(L'CT99KEY),IOKEYSAV                                        
         JE    MORE                                                             
*************************************                                           
* WE MIGHT HAVE STREAMING STATIONS OF SOMETHING OF THE SORT                     
*************************************                                           
         LA    R4,IOKEY                                                         
         USING CT99KEY,R4                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   CT99KTYP,CT99KTYQ   MSTREET RECORD                               
         MVI   CT99KSUB,CT99KSRA   X'00' - SUBTYPE FOR STATION REC              
         MVI   CT99KSRC,CT99KODS   C'D' - POSSIBLE STREAMING STATIONS           
         MVC   CT99KUID(L'STAPQSTA),STASTA                                      
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE+IO7'                            
         JNE   *+2                                                              
         CLC   IOKEY(L'CT99KEY),IOKEYSAV                                        
         JNE   NXRRD20                                                          
         MVI   DIGSRCE,CT99KODS   USING DS SOURCE FOR OWNERSHIP                 
         J     MORE                                                             
*************************************                                           
*                                                                               
NXRRD20  L     R1,AIO7               DUMMY UP A RECORD FOR SPOT DESKTOP         
         MVC   0(L'CT99KEY,R1),IOKEYSAV                                         
         MVC   CT99LEN-CT99KEY(2,R1),=AL2(CT99DATA+1-CT99KEY)                   
         MVI   CT99DATA-CT99KEY(R1),0                                           
         B     MORE                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SUPERDESK PID DATA DOWNLOAD                                         *         
***********************************************************************         
                                                                                
REQPID   LKREQ H,I#SDPIDS,OUTPID,NEXTREQ=REQMLK                                 
PID      LKREQ F,1,(I,B#SAVED,PIDSIND),CHAR,OLEN=L'SAPEPID,            +        
               MAXLEN=L'SAPEPID,TEXT=SP#MED,COL=*,LIST=F                        
         LKREQ E                                                                
                                                                                
OUTPID   LKOUT H                   ** PID DATA DOWNLOAD **                      
                                                                                
PIDREC   LKOUT R,X'0157'                                                        
Array    LKOUT C,255,(A,ARYPID)                                                 
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PID DOWNLOAD                                             
***********************************************************************         
                                                                                
ARYPID   LKOUT A,(R,NXTPID),MULTIROW=Y,ROWNAME=PIDVALS                          
                                                                                
PIdPId   LKOUT C,1,PIDPID,CHAR                                                  
PIdFNm   LKOUT C,2,PIDFNAM,CHAR,ND=Y                                            
PIdMNm   LKOUT C,3,PIDMNAM,CHAR,ND=Y                                            
PIdLNm   LKOUT C,4,PIDLNAM,CHAR,ND=Y                                            
PIdOfc   LKOUT C,5,PIDOFCD,CHAR,ND=Y                                            
PIdEml   LKOUT C,6,PIDEMAD,CHAR,ND=Y                                            
PIdPhn   LKOUT C,7,PIDPHON,CHAR,ND=Y                                            
PIdExt   LKOUT C,8,PIDEXT,CHAR,ND=Y                                             
                                                                                
         LKOUT E                                                                
**********************************************************************          
* GET PERSON INFO FROM PID RECORD                                               
*   FIRST NAME                                                                  
*   MIDDLE NAME                                                                 
*   LAST NAME                                                                   
*   OFFICE CODE                                                                 
*   EMAIL ADDRESS                                                               
*   PHONE NUMBER                                                                
**********************************************************************          
NXTPID   DS    0H                                                               
         LA    R2,PIDVALS                                                       
         ST    R2,LP_ADATA                                                      
         XC    PIDVALS(PIDVALL),PIDVALS                                         
*                                                                               
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTPID05                                                         
         MVI   PIDSIND,0                                                        
         XR    R1,R1                                                            
         J     NXTPID10                                                         
*                                                                               
NXTPID05 LLC   R1,PIDSIND                                                       
         LA    R1,1(R1)                                                         
         STC   R1,PIDSIND                                                       
*                                                                               
NXTPID10 XR    RE,RE                                                            
         ICM   RE,7,APIDS                                                       
         CLM   R1,3,LW_NUMN-LW_D(RE)   COMPARE AGAINST # OF PIDS SENT           
         BNL   NOMORE                                                           
         MHI   R1,L'PIDPID                                                      
         LA    R1,LW_DATA2-LW_D(RE,R1)                                          
         MVC   PIDPID,0(R1)                                                     
*                                                                               
         LA    R4,IOKEY                                                         
         XC    IOKEY,IOKEY         READ NEW SECURITY PERSON RECORD              
         USING SAPEREC,R4                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         L     R1,LP_ASECD                                                      
         MVC   SAPEAGY,SECAGY-SECD(R1)                                          
         MVC   SAPEPID,PIDPID                                                   
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE+IO7'                            
         JNE   *+2                                                              
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),IOKEYSAV                                
         JNE   NXTPIDY                                                          
*                                                                               
         L     R4,AIO7                                                          
         LA    R2,SAPEDATA         LOCATE PERSONNEL DETAILS ELEMENT             
NXTPID20 CLI   0(R2),EOR           END-OF-RECORD?                               
         JE    NXTPIDY             THEN WE'RE DONE                              
*                                                                               
         CLI   0(R2),SANAMELQ      X'C5' - NAME ELEMENT                         
         JE    NXTPID30                                                         
         CLI   0(R2),SAPERELQ      X'C6' - PERSONNEL ELEMENT FOR OFFICE         
         JE    NXTPID40                                                         
         CLI   0(R2),SAADRELQ      X'C7' - ADDRESS ELEMENT FOR PHONE            
         JE    NXTPID50                                                         
         CLI   0(R2),SAPEEELQ      X'E5' - EMAIL ELEMENT                        
         JE    NXTPID60                                                         
NXTPID25 LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         J     NXTPID20                                                         
***************                                                                 
* X'C5' - NAME ELEMENT                                                          
***************                                                                 
         USING SANAMD,R2                                                        
NXTPID30 LA    R1,SANAMES          R3 = A(NAMES)                                
         USING SANAMES,R1                                                       
         LA    R3,PIDFNAM                                                       
         TM    SANAMIND,SANAMIFN   X'80' - FIRST NAME PRESENT?                  
         JZ    NXTPID33                                                         
         LLC   RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R3),SANAME                                                   
         EX    RF,0(RE)                                                         
         LA    R1,2(RF,R1)                                                      
*                                                                               
NXTPID33 TM    SANAMIND,SANAMIMN   X'40' - MIDDLE NAME PRESENT?                 
         JZ    NXTPID36                                                         
         LA    R3,PIDMNAM                                                       
         LLC   RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R3),SANAME                                                   
         EX    RF,0(RE)                                                         
         LA    R1,2(RF,R1)                                                      
*                                                                               
NXTPID36 TM    SANAMIND,SANAMILN   X'20' - LAST NAME PRESENT?                   
         JZ    NXTPID25                                                         
         LA    R3,PIDLNAM                                                       
         LLC   RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R3),SANAME                                                   
         EX    RF,0(RE)                                                         
         J     NXTPID25                                                         
         DROP  R1                                                               
***************                                                                 
* X'C6' - PERSONNEL ELEMENT FOR OFFICE                                          
***************                                                                 
         USING SAPERD,R2                                                        
NXTPID40 MVC   PIDOFCD,SAPEROFF                                                 
         MVC   PIDEXT,SAPEREXT                                                  
         J     NXTPID25                                                         
***************                                                                 
* X'C7' - ADDRESS ELEMENT FOR PHONE                                             
***************                                                                 
         USING SAADRD,R2                                                        
NXTPID50 CLI   SAADRTYP,SAADPHOQ   X'1N'=TELEPHONE N (1 OR 2)                   
         JL    NXTPID25               SO CHECK AGAINST                          
         CLI   SAADRTYP,X'12'         X'11' AND X'12'                           
         JH    NXTPID25                                                         
         LLC   RF,SAADRDLN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   PIDPHON(0),SAADRDAT                                              
         EX    RF,0(RE)                                                         
         J     NXTPID25                                                         
***************                                                                 
* X'E5' - EMAIL ADDRESS ELEMENT                                                 
***************                                                                 
         USING SAPEED,R2                                                        
NXTPID60 LLC   RF,SAPEELN                                                       
         SHI   RF,SAPEELNQ                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   PIDEMAD(0),SAPEEID                                               
         EX    RF,0(RE)                                                         
         J     NXTPID25                                                         
*                                                                               
NXTPIDY  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* MARKET LOCKIN DATA DOWNLOAD                                         *         
***********************************************************************         
                                                                                
REQMLK   LKREQ H,I#SDMLID,OUTMLK,NEXTREQ=REQSLK                                 
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),DEFAULT=Y,   +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
PrdCd    LKREQ F,3,(I,B#SAVED,PRDIND),(R,VALPRD),LIST=NOD,DEFAULT=Y,   +        
               OLEN=L'BUYKPRD,MAXLEN=L'PKEYPRD,TEXT=SP#PRO,COL=*                
EstNo    LKREQ F,4,(I,B#SAVED,ESTIND),LBIN,LIST=NOD,RANGE=Y,           +        
               DEFAULT=NOT,OLEN=L'EKEYEST,TEXT=SP#EST,COL=*                     
MktNo    LKREQ F,5,(I,B#SAVED,MKTIND),(R,VALMKT),OLEN=L'BUYKMKTN,      +        
               LIST=NOD,DEFAULT=NOT,TEXT=SP#MKT,COL=*                           
StNet    LKREQ F,6,(I,B#SAVED,MKTIND),(R,VALSTM),LIST=NOD,DEFAULT=Y,   +        
               OLEN=L'BUYKMKTN,MAXLEN=STASTAL,TEXT=SP#STA,COL=*                 
         LKREQ E                                                                
                                                                                
OUTMLK   LKOUT H                   ** MARKET LOCKIN DOWNLOAD **                 
                                                                                
MLKREC   LKOUT R,X'0060'                                                        
Array    LKOUT C,255,(A,ARYSDG)                                                 
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR GOAL DOWNLOAD                                  *         
***********************************************************************         
                                                                                
ARYSDG   LKOUT A,(R,NXTGOL),MULTIROW=Y,ROWNAME=GOLVALS                          
                                                                                
P1Cod    LKOUT C,1,GOLPRD1,(U,#EDTPRD,$EDTPRD),ND=Y                             
P1Sec    LKOUT C,2,GOLSEC1,LBIN,ND=Y                                            
P2Cod    LKOUT C,3,GOLPRD2,(U,#EDTPRD,$EDTPRD),ND=Y                             
P2Sec    LKOUT C,4,GOLSEC2,LBIN,ND=Y                                            
EstNo    LKOUT C,5,GOLEST,LBIN,ND=Y                                             
MktNo    LKOUT C,6,GOLMKT,LBIN,ND=Y                                             
DayPt    LKOUT C,7,GOLDPT,CHAR,ND=Y                                             
GLkin    LKOUT C,8,GOLLKIN,CDAT,ND=Y                                            
GPurp    LKOUT C,9,GOLPURP,CHAR,ND=Y,PCVERSION=2.8.0.0                          
Array    LKOUT C,X'0062',(A,ARYSDGW),FILTROUT=TSTGOALS                          
Array    LKOUT C,X'0091',(A,ARYMLIW),FILTROUT=TSTMKTLK                          
Array    LKOUT C,X'0091',(A,ARYMLIM),FILTROUT=TSTMKTLK                          
                                                                                
         LKOUT E                                                                
                                                                                
TSTGOALS CLI   DLDGOALS,0          TEST GOAL DATA REQUESTED                     
         B     SETCCC                                                           
                                                                                
TSTMKTLK CLI   DLDMKTLK,0          TEST MARKET LOCKIN DATA REQUESTED            
         B     SETCCC                                                           
                                                                                
***********************************************************************         
* GET GOAL RECORDS FOR (BUY/)GOAL AND MARKET LOCKIN DOWNLOADS         *         
***********************************************************************         
                                                                                
NXTGOL   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTGOL08                                                         
                                                                                
         MVI   NOPRDREQ,C'N'       RESET BEFORE CREAMING APRD                   
         XC    PRVVALS(PRVVALL),PRVVALS                                         
                                                                                
         CLC   LP_QMAPN,SDMLID#    TEST MARKET LOCKIN DOWNLOAD                  
         JNE   NXTGOL02                                                         
         MVI   DLDMKTLK,1          SET INDICATOR AND AGENCY/MEDIA               
         ICM   RF,7,AMED                                                        
         MVC   GOALAGM,LW_DATA1-LW_D(RF)                                        
         ICM   RF,7,ACLT                                                        
         MVC   QCLTX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#GETCLT,AGETCLT)                                                
         JE    NXTGOL06                                                         
         DC    H'0'                                                             
                                                                                
NXTGOL02 CLI   DLDGOALS,0          TEST DOWNLOADING GOALS                       
         JNE   *+8                                                              
         CLI   DLDMKTLK,0          TEST DOWNLOADING MARKET LOCKINS              
         BE    NOMORE                                                           
                                                                                
         MVC   GOALMED,QMEDA                                                    
                                                                                
         LLC   R0,QMEDA            SAVE QMEDA                                   
         GOTOR (#VALMED,AVALMED),DMCB,GOALMED,0,GOALAGM                         
         STC   R0,QMEDA            RESTORE QMEDA                                
         BNE   NOMORE                                                           
                                                                                
NXTGOL06 OC    APRD,APRD           TEST ANY PRODUCTS SPECIFIED                  
         JNZ   NXTGOL08                                                         
         MVI   NOPRDREQ,C'Y'       SET NO PRD IN REQ BEFORE CREAMING            
         L     RE,LP_AWMP          NO - BUILD DEFAULT ENTRY                     
         USING LW_D,RE                                                          
         STCM  RE,7,APRD           CREAMS APRD                                  
         MVI   LW_TYPE,LW_TRNGQ                                                 
         MVI   LW_DATA1+0,1                                                     
         MVI   LW_DATA1+1,POLPRDQ-1                                             
         AHI   RE,LQ_LN1Q+(L'GKEYPRD*2)                                         
         ST    RE,LP_AWMP                                                       
         DROP  RE                                                               
                                                                                
NXTGOL08 GOTOR (#NXTREC,ANXTREC),DMCB,GOLKEYT,('B#GOLREC',0),SAVED,0,0          
         BNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         AHI   R2,GDELEM-GOALREC                                                
         USING GLEMENT,R2          R2=A(GOAL RECORD ELEMENT)                    
NXTGOL10 CLI   GLCODE,EOR          TEST END OF RECORD                           
         JE    NXTGOL08                                                         
         CLI   GLCODE,GLCODEQ      TEST GOAL WEEKLY ELEMENT                     
         JNE   NXTGOL12                                                         
                                                                                
         CLC   GLWEEK,STRDMONC     TEST START BEFORE REQUEST START              
         JL    NXTGOL12                                                         
         CLC   GLWEEK,ENDDATEC     TEST WEEK START AFTER REQUEST END            
         JNH   NXTGOL14                                                         
NXTGOL12 LLC   R0,GLEN             BUMP TO NEXT RECORD ELEMENT                  
         AR    R2,R0                                                            
         J     NXTGOL10                                                         
                                                                                
NXTGOL14 XC    GOLVALS(GOLVALL),GOLVALS                                         
         LA    R0,GOLVALS                                                       
         ST    R0,LP_ADATA                                                      
         L     R2,IOADDR           R2=A(GOAL RECORD)                            
         USING GOALREC,R2                                                       
         MVC   GOLPRD1,GKEYPRD     SET PRODUCT1 (PRODUCT) NUMBER                
         MVC   GOLEST,GKEYEST      SET ESTIMATE NUMBER                          
         MVC   GOLMKT,GKEYMKT      SET MARKET NUMBER                            
         MVC   GOLDPT,GKEYDPT      SET DAYPART CODE                             
         MVC   GOLSEC1,GKEYSEC     SET PRODUCT1 SECONDS LENGTH                  
                                                                                
         CLI   GKEYPRD2,0          TEST PIGGYBACK GOAL                          
         JE    NXTGOL16                                                         
         TM    GKEYAGY,GKEY2NPQ                                                 
         JNZ   NXTGOL16                                                         
         MVC   GOLPRD2,GKEYPRD2    SET PRODUCT2 (PRODUCT) NUMBER                
         LLC   RE,GKEYSLN                                                       
         SLL   RE,1                                                             
         CLM   RE,1,GKEYSEC        TEST 50/50 SPLIT                             
         JE    NXTGOL16                                                         
         LLC   RE,GKEYSEC          RE=TOTAL SECONDS LENGTH                      
         LLC   RF,GKEYSLN          RF=LENGTH ASSIGNED TO PRODUCT1               
         SR    RE,RF                                                            
         STC   RE,GOLSEC2          SET PRODUCT2 SECONDS LENGTH                  
                                                                                
NXTGOL16 DS    0H                                                               
         BRAS  RE,GTESTPDM         GET ESTIMATE RECORD'S PRIMARY DEMO           
*                                                                               
         OC    GDLKGOAL,GDLKGOAL   IS THERE A GOAL LOCKIN DATE?                 
         JZ    NXTGOL18                                                         
         MVC   GOLLKIN,GDLKGOAL    YES, SEND IT OUT ON THE DOWNLOAD             
                                                                                
NXTGOL18 OC    GDIDR,GDIDR         IS THERE A PURPOSE CODE?                     
         JZ    NXTGOL20                                                         
         MVC   GOLPURP,GDIDR       YES, SEND IT OUT ON THE DOWNLOAD             
                                                                                
NXTGOL20 CLI   DLDGOALS,0          TEST DOWNLOADING GOALS                       
         JE    NXTGOL22                                                         
                                                                                
         CLC   PPRDNUM,GOLPRDS     DO GOAL RECORD OPTIMIZATION                  
         MVC   PPRDNUM,GOLPRDS                                                  
         JNE   *+10                                                             
         XC    GOLPRDS,GOLPRDS                                                  
                                                                                
         CLC   PSECLEN,GOLSECS                                                  
         MVC   PSECLEN,GOLSECS                                                  
         JNE   *+10                                                             
         XC    GOLSECS,GOLSECS                                                  
                                                                                
         CLC   PESTNUM,GOLEST                                                   
         MVC   PESTNUM,GOLEST                                                   
         JNE   *+10                                                             
         XC    GOLEST,GOLEST                                                    
                                                                                
         CLC   PMKTNUM,GOLMKT                                                   
         MVC   PMKTNUM,GOLMKT                                                   
         JNE   *+10                                                             
         XC    GOLMKT,GOLMKT                                                    
                                                                                
         CLC   PDPTCOD,GOLDPT                                                   
         MVC   PDPTCOD,GOLDPT                                                   
         JNE   *+10                                                             
         XC    GOLDPT,GOLDPT                                                    
         B     EXITY                                                            
                                                                                
NXTGOL22 LA    R2,GDELEM           TEST MARKET LOCKIN DATA PRESENT              
         USING GLKELEM,R2                                                       
NXTGOL24 CLI   GLKELEM,EOR         TEST END OF RECORD                           
         JE    NXTGOL08                                                         
         CLI   GLKELEM,GLKCOWKQ    TEST WEEK LOCKIN ELEMENT                     
         BE    EXITY                                                            
         CLI   GLKELEM,GLKCOMNQ    TEST MONTH LOCKIN ELEMENT                    
         BE    EXITY                                                            
         LLC   R0,GLKLEN                                                        
         AR    R2,R0                                                            
         J     NXTGOL24                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR GOAL WEEKLY DOWNLOAD                           *         
***********************************************************************         
                                                                                
ARYSDGW  LKOUT A,(R,GETGWK),NEWEL=B,NROWS=(B#SAVED,GWKTAB#),           +        
               ROWWIDTH=GWKTABL,ROWNAME=GWKTABD                                 
                                                                                
MapCd    LKOUT C,0,(D,B#WORKD,HALF),EMAP,FILTROUT=SETGOLM                       
RFact    LKOUT C,1,GWKREP#,LBIN,ND=Y                                            
WkStD    LKOUT C,2,GWKSTR,EDAT,ND=Y                                             
Goal$    LKOUT C,3,GWKDOL,SPAK,ND=Y                                             
GoalP    LKOUT C,4,GWKGRP,SPAK,ND=Y                                             
PRout    LKOUT P,GWKTYPE,SET2DCMG                                               
Goal2Dec LKOUT C,5,(D,B#WORKD,BYTE),HDRO,ND=Y,PCVERSION=4.6.0.170               
         LKOUT E                                                                
                                                                                
SETGOLM  L     R1,LP_AINP          SET MAP CODE FOR GOAL DOWNLOAD REC           
         LLC   R0,GWKTYPE-GWKTABD(R1)                                           
         AHI   R0,X'0062'          X'0069' - GOAL LOCKIN VALUES                 
         STH   R0,HALF                                                          
         B     RTRNYES                                                          
*                                                                               
SET2DCMG L     R1,LP_AINP                                                       
         USING GWKTABD,R1                                                       
         MVI   BYTE,0                                                           
         OC    GWKGRP,GWKGRP       ANY POINTS?                                  
         JZ    EXIT                NONE, REPEATING FROM PREVIOUS                
*                                                                               
         TM    GWKINDS,GWKI2DEC    X'10', IS THE INDICATOR ON?                  
         JZ    EXIT                                                             
         MVI   BYTE,YESQ                                                        
         J     EXIT                                                             
         DROP  R1                                                               
*                                                                               
***********************************************************************         
* ROUTINE TO BUILD GOAL WEEKLY RECORDS                                *         
***********************************************************************         
                                                                                
GETGWK   L     R2,IOADDR                                                        
         USING GOALREC,R2          R2=A(GOAL RECORD)                            
         XC    GWKTAB#,GWKTAB#     INITIALIZE N'ENTRIES IN GWKTAB               
         XC    GOALDATE,GOALDATE   CLEAR LAST WEEK START DATE                   
                                                                                
         LA    R2,GDELEM                                                        
         USING GLEMENT,R2          R2=A(FIRST GOAL RECORD ELEMENT)              
         L     R3,AIO8                                                          
         ST    R3,LP_ADATA         SET A(ARRAY FOR DDLINK)                      
TAB      USING GWKTABD,R3          R3=A(CURRENT TABLE ENTRY)                    
         SR    R4,R4                                                            
PRV      USING GWKTABD,R4          R4=A(PREVIOUS TABLE ENTRY)                   
                                                                                
GETGWK10 CLI   GLCODE,EOR          TEST END OF RECORD                           
         JE    GETGWK80                                                         
                                                                                
         L     RF,LP_ADATA                                                      
         CLI   GLCODE,GLCODEQ      TEST GOAL WEEKLY ELEMENT                     
         JE    GETGWK20                                                         
         CLI   GLCODE,GLLKELQ      TEST GOAL LOCKIN ELEMENT                     
         JNE   GETGWK70                                                         
                                                                                
GETGWK20 CLC   GLWEEK,STRDMONC     TEST START BEFORE REQUEST START              
         JL    GETGWK70                                                         
         CLC   GLWEEK,ENDDATEC     TEST WEEK START AFTER REQUEST END            
         JH    GETGWK70                                                         
                                                                                
         XC    TAB.GWKTABD(GWKTABL),TAB.GWKTABD   INITIALIZE ENTRY              
                                                                                
         CLI   GLCODE,GLLKELQ      ARE WE PROCESSING LOCKIN DATA?               
         JNE   *+8                 NO, TYPE ALREADY 0                           
         MVI   TAB.GWKTYPE,7       YES, SET THE APPROPRIATE TYPE                
                                                                                
         GOTOR VDATCON,DMCB,(2,GLWEEK),TAB.GWKSTR                               
                                                                                
         OC    GLGRP,GLGRP         TEST GRP...                                  
         JZ    *+14                                                             
         OC    GLBUDGET,GLBUDGET   ...OR DOLLAR VALUE MISSING                   
         JNZ   *+8                                                              
         GOTOR GETCPP              YES - ATTEMPT LOOK-UP OF CPP GUIDE           
*                                                                               
         XR    R0,R0               CLEAR R0 IN CASE WE HAVE GARBAGE             
         ICM   R1,15,GLGRP                                                      
         N     R1,=X'3FFFFFFF'                                                  
         CVD   R1,DUB                                                           
         TM    GLGRP,X'40'         TEST 2-DECIMAL GOAL POINTS?                  
         JZ    GETGWK50                                                         
*                                                                               
         CLC   LP_VRSN,V4600AA     SBTK V4.6.0.169 OR LOWER?                    
         JL    GETGWK40            ROUND TO 1 DEC                               
*  IF RATINGS AND G0 PROFILE SAYS NOT 2 DEC                                     
*     THEN  ROUND THE 2 DECIMAL GOAL TO 1 DECIMAL                               
         CLI   ESTPDTYP,C'E'       IF EXTENDED OR                               
         JE    *+12                                                             
         CLI   ESTPDTYP,C'R'       RATING?                                      
         JNE   GETGWK30                                                         
*                                  -YES, HAVE RAT/EXT                           
         CLI   PROFG0V+7,C'Y'      2 DECIMAL GOAL RATINGS?                      
         JE    GETGWK35            YES, GOOD WITH 2-DEC                         
         J     GETGWK40            NO, ROUND TO 1 DEC                           
*                                                                               
*                                  -NO, HAVE IMPS                               
GETGWK30 CLI   PROFG0V+8,C'Y'      2 DECIMAL GOAL IMPS?                         
         JNE   GETGWK40            NO, ROUND TO 1 DEC                           
*                                                                               
GETGWK35 OI    TAB.GWKINDS,GWKI2DEC  WE'RE GOOD WITH 2-DEC                      
         J     GETGWK50                                                         
*                                                                               
GETGWK40 SRP   DUB,64-1,5          ROUND TO 1 DEC                               
*                                                                               
GETGWK50 ZAP   TAB.GWKGRP,DUB      SET GRPS                                     
         ICM   R0,15,GLBUDGET                                                   
         CVD   R0,DUB                                                           
         ZAP   TAB.GWKDOL,DUB      SET DOLLARS                                  
         MVI   TAB.GWKREP#,1                                                    
                                                                                
         CLC   TAB.GWKTYPE,PRV.GWKTYPE   GOAL DATA TYPE THE SAME?               
         JNE   GETGWK60                  NO, DON'T GROUP TOGETHER               
                                                                                
         LTR   R4,R4               TEST FIRST TIME                              
         JZ    GETGWK60            IT IS, DONE WITH THIS ENTRY                  
*&&DO                                                                           
**  THIS SECTION HAS BEEN COMMENTED OUT BECAUSE SBTK NEVER IMPLEMENTED          
**    IT ALSO EXPLAINS THE  **NOOP**  LINE                                      
         LLC   R0,PRV.GWKREP#                                                   
         MHI   R0,7                                                             
         GOTOR VADDAY,DMCB,PRV.GWKSTR,WORK,(R0)                                 
         CLC   TAB.GWKSTR,WORK                                                  
**NOOP** JNE   *+8                                                              
         J     *+8                                                              
         OI    TAB.GWKINDS,GWKISEQ SET WEEK IN SEQUENCE                         
**  THIS SECTION has been commented out because SBTK never implemented          
*&&                                                                             
                                                                                
         CP    TAB.GWKGRP,PRV.GWKGRP                                            
         JNE   *+8                                                              
         OI    TAB.GWKINDS,GWKIGRP SET GRPS SAME AS PREVIOUS                    
                                                                                
         CP    TAB.GWKDOL,PRV.GWKDOL                                            
         JNE   *+8                                                              
         OI    TAB.GWKINDS,GWKIDOL SET DOLLARS SAME AS PREVIOUS                 
                                                                                
         TM    TAB.GWKINDS,GWKISEQ+GWKIGRP+GWKIDOL                              
         JNO   GETGWK60                                                         
                                                                                
         LLC   R0,PRV.GWKREP#      BUMP REPLICATION FACTOR IF WEEK IN           
         AHI   R0,1                SEQUENCE AND ALL DATA THE SAME               
         STC   R0,PRV.GWKREP#                                                   
         J     GETGWK70                                                         
                                                                                
GETGWK60 LA    R4,TAB.GWKTABD      POINT TO CURRENT (PREVIOUS)                  
         AHI   R3,GWKTABL          POINT TO NEXT (CURRENT)                      
         LH    R0,GWKTAB#          BUMP GOAL WEEK ENTRY COUNT                   
         AHI   R0,1                                                             
         STH   R0,GWKTAB#                                                       
                                                                                
GETGWK70 LLC   R0,GLEN             BUMP TO NEXT RECORD ELEMENT                  
         AR    R2,R0                                                            
         J     GETGWK10                                                         
                                                                                
GETGWK80 SR    R0,R0               DO GOAL WEEK DOWNLOAD OPTIMIZATION           
         ICM   R0,3,GWKTAB#                                                     
         JZ    *+2                                                              
         L     R3,AIO8             POINT TO START OF TABLE                      
                                                                                
GETGWK90 CLI   TAB.GWKREP#,1       TEST REPLICATION FACTOR IS ONE               
         JNE   *+8                                                              
         MVI   TAB.GWKREP#,0       YES - CLEAR REPLICATION FACTOR               
                                                                                
         TM    TAB.GWKINDS,GWKISEQ TEST DATE IN SEQUENCE                        
         JZ    *+10                                                             
         XC    TAB.GWKSTR,TAB.GWKSTR                                            
                                                                                
         TM    TAB.GWKINDS,GWKIGRP TEST GRP SAME AS PREVIOUS                    
         JZ    *+10                                                             
         XC    TAB.GWKGRP,TAB.GWKGRP                                            
                                                                                
         TM    TAB.GWKINDS,GWKIDOL TEST DOLLARS SAME AS PREVIOUS                
         JZ    *+10                                                             
         XC    TAB.GWKDOL,TAB.GWKDOL                                            
                                                                                
         AHI   R3,GWKTABL          BUMP TO NEXT TABLE ENTRY                     
         JCT   R0,GETGWK90         DO FOR NUMBER OF ENTRIES                     
         B     EXITY                                                            
         DROP  TAB,PRV,R2                                                       
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MARKET LOCKIN WEEKLY DATA                      *         
***********************************************************************         
                                                                                
ARYMLIW  LKOUT A,(D,B#GOLREC,GDELEM),EOT=EOR,NEWEL=B,                  +        
               ROWID=(GLKELEM,GLKCOWKQ),ROWWIDTH=(V,GLKLEN)                     
                                                                                
Array    LKOUT C,255,(A,ARYMLIV),FILTROUT=SETELEM                               
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MARKET LOCKIN MONTHLY DATA                     *         
***********************************************************************         
                                                                                
ARYMLIM  LKOUT A,(D,B#GOLREC,GDELEM),EOT=EOR,NEWEL=B,                  +        
               ROWID=(GLKELEM,GLKCOMNQ),ROWWIDTH=(V,GLKLEN)                     
                                                                                
Array    LKOUT C,255,(A,ARYMLIV),FILTROUT=SETELEM                               
                                                                                
         LKOUT E                                                                
                                                                                
SETELEM  L     R1,LP_AINP          COPY ELEMENT INTO ELEM                       
         MVC   ELEM,0(R1)                                                       
         CLI   0(R1),GLKCOWKQ      WEEKLY DATA?                                 
         JE    STELM10                                                          
         CLI   0(R1),GLKCOSWQ      SPILL LOCKIN WEEKLY?                         
         BNE   EXITY               NO                                           
* YES, GLKDLR IS TO NEAREST DIME IF WEEKLY.  SO LET'S MAKE IT DOLLARS           
STELM10  XR    R0,R0                                                            
         ICM   R0,15,GLKDLR-GLKELEM+ELEM                                        
         MHI   R0,10                                                            
         STCM  R0,15,GLKDLR-GLKELEM+ELEM                                        
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR MARKET LOCKIN VALUES                           *         
***********************************************************************         
                                                                                
ARYMLIV  LKOUT A,(D,B#WORKD,ELEM),NROWS=1,ROWWIDTH=GLKLENQ,            +        
               ROWNAME=GLKELEM                                                  
                                                                                
LType    LKOUT C,1,GLKELEM,(R,EDTLTY)                                           
LWeek    LKOUT C,2,GLKDAT,CDAT                                                  
PrdCd    LKOUT C,3,GLKPRD,(U,#EDTPRD,$EDTPRD)                                   
SptLn    LKOUT C,4,GLKTSC,LBIN                                                  
Spots    LKOUT C,5,GLKSPT,LBIN                                                  
Dolrs    LKOUT C,6,GLKDLR,CBIN                                                  
PRout    LKOUT P,,GTMLEST                                                       
                                                                                
PRout    LKOUT P,,INITDEMS                                                      
DemCd    LKOUT C,7,GLKDEMNO,(R,EDTGDCD),ND=Y                                    
DemVl    LKOUT C,8,GLKDEMVL,(R,EDTDVL),ND=Y                                     
PRout    LKOUT P,GLKDEMVL,GTML2DEC                                              
Dem2Dc   LKOUT C,9,(D,B#WORKD,BYTE),HDRO,ND=Y                                   
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
INITDEMS XC    SVDEMTYL,SVDEMTYL                                                
         XC    ACURDMTP,ACURDMTP                                                
         J     EXITY                                                            
*                                                                               
         USING GTMLESTD,RC                                                      
GTMLEST  MVC   GMEIOVAL,IOVALS     SAVE IOVALS                                  
         L     R2,AGOLREC                                                       
         USING GOALREC,R2                                                       
         LA    R3,IOKEY                                                         
         USING ESTHDR,R3           READ ESTIMATE TO ESTABLISH PERIOD            
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,GKEYAM                                                    
         MVC   EKEYCLT,GKEYCLT                                                  
         MVC   EKEYPRD,POLPRD                                                   
         MVC   EKEYEST,GKEYEST                                                  
         DROP  R2                                                               
         L     R3,AESTREC                                                       
         CLC   EKEY,IOKEY                                                       
         JE    GTMLESTX                                                         
         DROP  R3                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#ESTREC'                         
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#ESTREC'                        
         JNE   *+2                                                              
GTMLESTX MVC   IOVALS(IOVALL),GMEIOVAL                                          
         J     EXITY                                                            
*                                                                               
GTML2DEC L     R1,LP_AINP                                                       
         MVI   BYTE,0                                                           
*                                                                               
         TM    0(R1),X'40'         2 DECIMAL POINTS?                            
         JZ    EXIT                NO                                           
*                                                                               
         CLI   PROFG0V+7,C'Y'      AGENCY WANTS 2 DECIMAL GOALS?                
         JNE   EXIT                                                             
         MVI   BYTE,YESQ           YES!                                         
         J     EXIT                                                             
*                                                                               
GTMLESTD DSECT                                                                  
GMEIOVAL DS    XL(IOVALL)                                                       
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* STATION LOCKIN DATA DOWNLOAD                                        *         
***********************************************************************         
                                                                                
REQSLK   LKREQ H,I#SDSLID,OUTSLK,NEXTREQ=REQSDS                                 
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),DEFAULT=Y,   +        
               OLEN=L'CKEYCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                  
PrdCd    LKREQ F,3,(I,B#SAVED,PRDIND),(R,VALPRD),LIST=NOD,DEFAULT=Y,   +        
               OLEN=L'BUYKPRD,MAXLEN=L'PKEYPRD,TEXT=SP#PRO,COL=*                
EstNo    LKREQ F,4,(I,B#SAVED,ESTIND),LBIN,LIST=NOD,RANGE=Y,           +        
               DEFAULT=NOT,OLEN=L'SLKKEST,TEXT=SP#EST,COL=*                     
MktNo    LKREQ F,5,(I,B#SAVED,MKTIND),(R,VALMKT),OLEN=L'BUYKMKTN,      +        
               LIST=NOD,DEFAULT=NOT,TEXT=SP#MKT,COL=*                           
StNet    LKREQ F,6,(I,B#SAVED,MKTIND),(R,VALSTM),LIST=NOD,DEFAULT=Y,   +        
               OLEN=L'BUYKMSTA,MAXLEN=STASTAL,TEXT=SP#STA,COL=*                 
         LKREQ E                                                                
                                                                                
OUTSLK   LKOUT H                   ** STATION LOCKIN DOWNLOAD **                
                                                                                
SLKREC   LKOUT R,X'0092'                                                        
Array    LKOUT C,255,(A,ARYSLK)                                                 
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION LOCKIN DOWNLOAD                        *         
***********************************************************************         
                                                                                
ARYSLK   LKOUT A,(R,NXTSLK),MULTIROW=Y,ROWNAME=SLKRECD                          
                                                                                
Statn    LKOUT C,1,SLKKSTA,(R,EDTCLS),ND=Y                                      
PrdCd    LKOUT C,2,SLKKPRD,(U,#EDTPRD,$EDTPRD)                                  
PigCd    LKOUT C,3,SLKKPRD2,(U,#EDTPRD,$EDTPRD),ND=Y                            
EstNo    LKOUT C,4,SLKKEST,LBIN                                                 
DayPt    LKOUT C,5,SLKKDPT,CHAR                                                 
SLen1    LKOUT C,6,SLKKLEN,LBIN                                                 
SLen2    LKOUT C,7,SLKKLEN2,LBIN,ND=Y                                           
PRout    LKOUT P,,GTSLEST                                                       
*                                                                               
PRout    LKOUT P,,INITDEMS                                                      
Demo1    LKOUT C,8,SLKDEM1,(R,EDTEDCD)                                          
Demo2    LKOUT C,9,SLKDEM2,(R,EDTEDCD),ND=Y                                     
Demo3    LKOUT C,10,SLKDEM3,(R,EDTEDCD),ND=Y                                    
Demo4    LKOUT C,11,SLKDEM4,(R,EDTEDCD),ND=Y                                    
CrDat    LKOUT C,12,SLKCRDT,BDAT                                                
LCDat    LKOUT C,13,SLKUPDT,BDAT,ND=Y                                           
Array    LKOUT C,X'0093',(A,ARYSLKV)                                            
                                                                                
         LKOUT E                                                                
                                                                                
         USING GTSLESTD,RC                                                      
GTSLEST  MVC   GSEIOVAL,IOVALS     SAVE IOVALS                                  
         L     R2,ASLKREC                                                       
         USING SLKRECD,R2                                                       
         LA    R3,IOKEY                                                         
         USING ESTHDR,R3           READ ESTIMATE TO ESTABLISH PERIOD            
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,SLKKAGMD                                                  
         MVC   EKEYCLT,SLKKCLT                                                  
         MVC   EKEYPRD,POLPRD                                                   
         MVC   EKEYEST,SLKKEST                                                  
         DROP  R2                                                               
*                                                                               
         L     R3,AESTREC                                                       
         CLC   EKEY,IOKEY                                                       
         JE    GTSLESTX                                                         
         DROP  R3                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#ESTREC'                         
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#ESTREC'                        
         JNE   *+2                                                              
GTSLESTX MVC   IOVALS(IOVALL),GSEIOVAL                                          
         J     EXITY                                                            
GTSLESTD DSECT                                                                  
GSEIOVAL DS    XL(IOVALL)                                                       
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET STATION LOCKIN RECORDS                                          *         
***********************************************************************         
                                                                                
NXTSLK   CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTSLK02                                                         
         XC    CLRSTAL,CLRSTAL                                                  
         CLC   LP_QMAPN,SDSLID#                                                 
         JNE   NXTSLK02                                                         
         MVC   APRD1,APRD                                                       
         ICM   RF,7,AMED                                                        
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,BUYMED                             
         ICM   RF,7,ACLT                                                        
         MVC   QCLTX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#GETCLT,AGETCLT)                                                
         JNE   *+2                                                              
                                                                                
NXTSLK02 GOTOR (#NXTREC,ANXTREC),DMCB,SLKKEYT,('B#SLKREC',0),          +        
               ('$NXTRXSP',SAVED),0,0                                           
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION LOCKIN WEEK DOWNLOAD                   *         
***********************************************************************         
                                                                                
ARYSLKV  LKOUT A,(D,B#SLKREC,SLKFSTEL),EOT=EOR,NEWEL=B,                +        
               ROWID=(LOKEL,LOKELCDQ),ROWWIDTH=(V,LOKELLN)                      
                                                                                
WkSDt    LKOUT C,1,LOKWEEK,CDAT                                                 
Spots    LKOUT C,2,LOKSPOTS,LBIN                                                
Grs$s    LKOUT C,3,LOKDOLS,CBIN                                                 
Net$s    LKOUT C,4,LOKNET,CBIN,ND=Y                                             
Cst2$    LKOUT C,5,LOKDOL2,CBIN,ND=Y                                            
Net2$    LKOUT C,6,LOKNET2,CBIN,ND=Y                                            
Demo1    LKOUT C,7,LOKDEM,(R,EDTDVL),ND=Y                                       
Demo2    LKOUT C,8,LOKDEM2,(R,EDTDVL),ND=Y                                      
Demo3    LKOUT C,9,LOKDEM3,(R,EDTDVL),ND=Y                                      
Demo4    LKOUT C,10,LOKDEM4,(R,EDTDVL),ND=Y                                     
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* SINGLE BUY DOWNLOAD REQUEST                                         *         
***********************************************************************         
                                                                                
REQSDS   LKREQ H,I#SDBUYR,OUTBUY,NEXTREQ=REQSDC                                 
BuyDA    LKREQ F,D#DA,(I,B#SAVED,RDAIND),HEXD,OLEN=L'BUYKDA,           +        
               TEXT=SP#SDBDA,COL=*                                              
Token    LKREQ F,D#TOKEN,(I,B#SAVED,TOKIND),HEXD,OLEN=L'FAWSTOKN,      +        
               TEXT=SP#TOKEN,COL=*                                              
Error    LKREQ F,D#UPLERR,(I,B#SAVED,ERRIND),CHAR,OLEN=80,             +        
               TEXT=SP#ERR,COL=*                                                
PCKey    LKREQ F,D#PCKEY,(D,B#SAVED,PCKEY),VSTR,TEXT=SP#KEY,COL=*               
OLock    LKREQ F,D#OLOCK,(D,B#SAVED,OLOCK),CHAR,TEXT=SP#LOCK,COL=*              
         LKREQ E                                                                
                                                                                
OUTBUY   LKOUT H                   ** SINGLE BUY DOWNLOAD **                    
         LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYBUY)                                                   
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION BUY DOWNLOAD                                       *         
***********************************************************************         
                                                                                
ARYBUY   LKOUT A,(R,NXTBUY),MULTIROW=Y,ROWNAME=BUYREC                           
                                                                                
Array    LKOUT C,X'0050',(A,ARYSDV),FILTROUT=TSTSDB                             
Array    LKOUT C,X'0052',(A,ARYSDL)                                             
Array    LKOUT C,X'0054',(A,ARYSDO)                                             
Array    LKOUT C,X'0094',(A,ARYSORB)                                            
Array    LKOUT C,X'0064',(A,ARYPBDO),FILTROUT=TSTSPLN                           
Array    LKOUT C,X'0055',(A,ARYSDSI)                                            
Array    LKOUT C,X'0065',(A,ARYPBDS),FILTROUT=TSTSPLY                           
Array    LKOUT C,X'0066',(A,ARYSPO)                                             
Array    LKOUT C,X'0056',(A,ARYSPT)                                             
Array    LKOUT C,X'0071',(A,ARYBWS)                                             
Array    LKOUT C,X'0072',(A,ARYBTR)                                             
Array    LKOUT C,X'0073',(A,ARYACT)                                             
Array    LKOUT C,X'0074',(A,ARYBDA)                                             
Array    LKOUT C,X'0075',(A,ARYMFX)                                             
Array    LKOUT C,X'0076',(A,ARYSFX)                                             
                                                                                
         LKOUT E                                                                
                                                                                
TSTSDB   CLC   LP_QMAPN,SDNBUYD#   SET CC=NE IF NEW BUY/GOAL DOWNLOAD           
         B     SETCCC                                                           
                                                                                
TSTSPLN  CLI   SCDSPILL,NOQ        SET CC EQUAL IF ORIGINAL BUY                 
         BR    RE                                                               
                                                                                
TSTSPLY  CLI   SCDSPILL,YESQ       SET CC EQUAL IF SPILL BUY                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* GET NON-NETWORK BUY RECORDS AND FORMAT BUY LINE FOR DOWNLOADING     *         
***********************************************************************         
                                                                                
NXTBUY   CLC   LP_QMAPN,SDBUYDD#   TEST RETURN FROM BUY PROGRAM                 
         JNE   NXTBUY40                                                         
         XC    LBUYVALS(LBUYVALL),LBUYVALS                                      
                                                                                
***********************************************************************         
* FOLLOWING CODE PROCESSES RECORDS RETURNED FROM BUY UPLOADS.  THE    *         
* ELEMENT SEQUENCE IS IMPORTANT.  UPLOAD ERROR TEXT ELEMENT(S) MUST   *         
* PRECEDE THE BUY RECORD THAT THEY REFER TO - MORE THAN ONE ERROR     *         
* TEXT ELEMENT MAY BE PRESENT.  RECORDS ARE DEFINED BY EITHER A WSSVR *         
* TOKEN (FOR SIMULATED ADDS) WHERE THE BUY RECORD ITSELF IS PASSED IN *         
* THE BUFFER AND A UNIQUE TOKEN PASSED IN THE DOWNLOAD REQUEST OR     *         
* ELSE THE DISK ADDRESS OF THE BUY RECORD IS PASSED.  TOKENS AND DISK *         
* ADDRESSES MAY BE INTERSPERSED ON THE DOWNLOAD REQUEST - THE RECORDS *         
* WILL BE DOWNLOADED IN THE SEQUENCE IN WHICH THEY ARE PRESENTED      *         
***********************************************************************         
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTBUY05                                                         
         MVI   DLDAFFID,1          SET WANT AFFIDAVIT DATA DOWNLOADED           
         MVI   DLDCLEAR,1          SET WANT CLEARANCE DATA DOWNLOADED           
         SR    RE,RE               FIND FIRST ERROR/TOKEN/DISK ADDRESS          
         ICM   RE,7,AERR           ELEMENT IN THE WMP                           
         SR    RF,RF                                                            
         ICM   RF,7,ATOK                                                        
         SR    R1,R1                                                            
         ICM   R1,7,ARDA                                                        
         ICM   R0,15,EFFS                                                       
         LTR   RE,RE                                                            
         JZ    *+6                                                              
         LR    R0,RE                                                            
         LTR   RF,RF                                                            
         JZ    *+12                                                             
         CLR   RF,R0                                                            
         JH    *+6                                                              
         LR    R0,RF                                                            
         LTR   R1,R1                                                            
         JZ    *+12                                                             
         CLR   R1,R0                                                            
         JH    *+6                                                              
         LR    R0,R1                                                            
         CLM   R0,15,EFFS                                                       
         BE    NOMORE                                                           
         ST    R0,AWMPEL                                                        
                                                                                
NXTBUY05 XC    BUYIND(BUYINDL),BUYIND                                           
                                                                                
         L     RE,AWMPEL           POINT TO FIRST/NEXT ELEMENT                  
         USING LW_D,RE                                                          
         SR    R0,R0                                                            
NXTBUY10 CLC   LW_CODE,BZEROS      TEST END OF WMP                              
         BE    NOMORE                                                           
                                                                                
         CLC   LW_CODE,=AL2(D#UPLERR)                                           
         JNE   NXTBUY15                                                         
         OC    AERR,AERR           TEST FIRST ERROR TEXT ELEMENT                
         JNZ   *+12                                                             
         STCM  RE,7,AERR           YES - SET IT                                 
         MVI   ERRIND,LW_TSINQ                                                  
         ICM   R0,3,LW_LN          BUMP TO NEXT ELEMENT                         
         AR    RE,R0                                                            
         J     NXTBUY10                                                         
                                                                                
NXTBUY15 CLC   LW_CODE,=AL2(D#TOKEN)                                            
         JNE   NXTBUY20                                                         
         STCM  RE,7,ATOK                                                        
         MVI   TOKIND,LW_TSINQ     SET A(TOKEN) ELEMENT                         
         J     NXTBUY25                                                         
                                                                                
NXTBUY20 CLC   LW_CODE,=AL2(D#DA)                                               
         JNE   *+2                 UNKNOWN DATA TYPE IN WMP                     
         STCM  RE,7,ARDA                                                        
         MVI   RDAIND,LW_TSINQ                                                  
                                                                                
NXTBUY25 ICM   R0,3,LW_LN                                                       
         AR    RE,R0                                                            
         ST    RE,AWMPEL           SET A(NEXT WMP ELEMENT TO PROCESS)           
                                                                                
         ICM   RE,7,ATOK           TEST TOKEN SET                               
         JZ    NXTBUY30                                                         
         USING LW_D,RE                                                          
         XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         MVC   FAWSTOKN,LW_DATA1                                                
         MVC   FAWSADR,AIO5                                                     
         MVI   FAWSACTN,FAWSURST                                                
         L     RF,ACOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTOR (RF),FAWSSVRD       CALL WSSVR TO GET THE BUY RECORD             
         CLI   FAWSRTN,0                                                        
         JNE   *+2                 DIE IF CAN'T SAVE RECORD BUFFER              
         MVI   FAWSACTN,FAWSUDEL   DELETE THE BUY FROM THE BUFFER               
         GOTOR (RF),(R1)                                                        
         MVC   IOADDR,AIO5         SET A(BUY RECORD)                            
         J     NXTBUY32                                                         
                                                                                
NXTBUY30 ICM   RE,7,ARDA           TEST RECORD DISK ADDRESS SET                 
         JZ    *+2                                                              
         MVC   IODAOVER,LW_DATA1   READ THE BUY RECORD                          
         OC    IODAOVER,IODAOVER   ENSURE GOOD DISK ADDRESS                     
         JZ    *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         JNE   *+2                                                              
         DROP  RE                                                               
                                                                                
NXTBUY32 L     R2,IOADDR                                                        
         USING BUYREC,R2           R2=A(BUY RECORD)                             
         ST    R2,LP_ADATA         SET A(RECORD) FOR DDLINK                     
         MVC   QMEDX,BUYKAM                                                     
         MVC   QCLTX,BUYKCLT                                                    
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         JNE   *+2                                                              
         MVC   BUYMED,QMEDA                                                     
         GOTOR (#GETCLT,AGETCLT)   READ CLIENT RECORD                           
         JNE   *+2                 CAN'T READ CLIENT RECORD                     
         MVC   MKTSTA,BUYKMKTN     SET MARKET/STATION                           
         MVI   SPILL,NOQ           SET NOT A SPILL BUY                          
         ST    R2,IOADDR           (IOADDR DESTROYED BY GETCLT)                 
*                                                                               
         ICM   RE,7,ATOK           TEST TOKEN SET - WSSVR PASSED BUY?           
         JNZ   NXTBUY36             YES, THEN BUY ALREADY CONVERTED!!           
*                                   NO, BUY NEEDS TO BE CONVERTED               
         MVC   GBY1OR2,SAVE1OR2                                                 
***************                                                                 
* DON'T REMOVE UNTIL FQA/SJR IS CONVERTED              -HWON 05/26/2020         
         CLC   LP_AGY,=C'SJ'                                                    
         JNE   NXTBUY34                                                         
         CLC   BUYKCLT,=X'CC2B'      CLIENT TBL?                                
         JE    *+10                                                             
         CLC   BUYKCLT,=X'BCC9'      CLIENT PG0?                                
         JE    *+10                                                             
         CLC   BUYKCLT,=X'BCDA'      CLIENT PG1?                                
         JNE   NXTBUY34                                                         
         MVI   GBY1OR2,2                                                        
* DON'T REMOVE UNTIL FQA/SJR IS CONVERTED              -HWON 05/26/2020         
***************                                                                 
NXTBUY34 DS    0H                                                               
         MVC   GBYIOA,AIO5                                                      
         MVI   GBYACT,GBYCONV                                                   
         GOTOR VGETBUY,GETBLK                                                   
         MVI   GBYACT,0                                                         
*                                                                               
* FOR SINGLE BUY DOWNLOAD, MUST READ PROFILES                                   
*                                                                               
NXTBUY36 DS    0H                                                               
         L     RF,ACLTREC                                                       
         USING CLTRECD,RF                                                       
         XC    WORK,WORK           BUILD PROFILE KEY & GET PROFILES             
         MVC   WORK+04(L'AGENCY),AGENCY                                         
         MVC   WORK+06(L'QMEDA),QMEDA                                           
         MVC   WORK+07(L'QCLTA),QCLTA                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(L'COFFICE),COFFICE                                       
         DROP  RF                                                               
*                                                                               
         L     RF,VGETPROF                                                      
         LA    R4,PROFKSDB         R4=A(PROGRAM KEY LIST)                       
         LA    R3,PROFV            R3=A(PROFILES VALUES)                        
         LHI   R0,PROFNSDB         R0=NUMBER OF PROFILES TO LOOK UP             
NXTBUY38 MVC   WORK(L'PROFKSDB),0(R4)                                           
         GOTOR (RF),DMCB,WORK,(R3),VDATAMGR                                     
         AHI   R4,L'PROFKSDB                                                    
         AHI   R3,L'PROFV                                                       
         JCT   R0,NXTBUY38                                                      
*                                                                               
         XC    WORK,WORK           G0 PROFILE IS SPECIAL                        
         XC    PROFG0V,PROFG0V                                                  
         MVC   WORK(4),=C'S0G0'                                                 
         MVC   WORK+4(L'AGENCY),AGENCY                                          
         MVC   WORK+6(4),=X'FFFFFFFF'                                           
         GOTOR (RF),DMCB,WORK,PROFG0V,VDATAMGR                                  
         J     NXTBUY50                                                         
*                                                                               
* NEW BUY/GOAL DOWNLOAD (SDBUYD#)  OR  BUY/GOAL DOWNLOAD (SDNBUYD#)             
*                                                                               
NXTBUY40 CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTBUY45                                                         
                                                                                
         XC    LBUYVALS(LBUYVALL),LBUYVALS                                      
                                                                                
         MVC   BUYCLRLO,EFFS       SET INITIAL CLEARANCE DATES                  
         XC    BUYCLRHI,BUYCLRHI                                                
                                                                                
         L     RF,LP_AWMP          INITIALIZE STATIONS THAT HAVE BUYS           
         USING LW_D,RF                                                          
         XC    LW_NUMN,LW_NUMN                                                  
         ST    RF,ASTALST                                                       
         DROP  RF                                                               
                                                                                
         L     R0,AIO7             CLEAR MARKET REFERENCE TABLE                 
         LHI   R1,IO7LQ                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   PESTNUM,0                                                        
         XC    PMKT#,PMKT#         CLEAR LAST MARKET NUMBER                     
         XC    C2PMKT#,C2PMKT#     CLEAR LAST MARKET NUMBER                     
         MVC   BUYMED,QMEDA                                                     
                                                                                
         LLC   R0,QMEDA            SAVE QMEDA                                   
         GOTOR (#VALMED,AVALMED),DMCB,BUYMED,0,BUYAGM                           
         STC   R0,QMEDA            RESTORE QMEDA                                
         BNE   NOMORE                                                           
*                                                                               
         CLC   QCLTX,BZEROS                                                     
         JE    *+2                 DIE IF WE DON'T HAVE A CLIENT                
***************                                                                 
* DON'T REMOVE UNTIL FQA/SJR IS CONVERTED              -HWON 05/26/2020         
         CLC   LP_AGY,=C'SJ'                                                    
         JNE   NXTBUY45                                                         
         CLC   QCLTX,=X'CC2B'      CLIENT TBL?                                  
         JE    *+10                                                             
         CLC   QCLTX,=X'BCC9'      CLIENT PG0?                                  
         JE    *+10                                                             
         CLC   QCLTX,=X'BCDA'      CLIENT PG1?                                  
         JNE   NXTBUY45                                                         
         MVI   SV1OR2,2                                                         
* DON'T REMOVE UNTIL FQA/SJR IS CONVERTED              -HWON 05/26/2020         
***************                                                                 
                                                                                
NXTBUY45 LA    R0,BUYKEYT1                                                      
         CLI   DLDBYSTA,0          ARE WE LOOKING BY STATION?                   
         JE    *+8                 YES                                          
         LA    R0,BUYKEYT2                                                      
         GOTOR (#NXTREC,ANXTREC),DMCB,(R0),('B#BUYREC',0),SAVED,       +        
               AFLTBKY,AFLTPRD                                                  
         BNE   EXITY                                                            
                                                                                
         L     R2,IOADDR           POINT TO BUY RECORD                          
         L     R1,ACLTREC          TEST WE HAVE CLIENT RECORD AROUND            
         CLC   BUYKCLT,CKEYCLT-CLTRECD(R1)                                      
         JE    NXTBUY50                                                         
         MVC   QCLTX,BUYKCLT       NO - READ IT                                 
         GOTOR RESCLT                                                           
         ST    R2,IOADDR                                                        
         JE    NXTBUY50                                                         
         ICM   R1,B'0111',BUYKAM   ON ERROR SKIP TO NEXT CLIENT                 
         AHI   R1,1                                                             
         XC    BUYKEY,BUYKEY                                                    
         STCM  R1,B'0111',BUYKEY                                                
         J     NXTBUY45            GO BACK AND PROCESS NEXT CLIENT              
                                                                                
NXTBUY50 CLC   PMKTSTA,MKTSTA      TEST CHANGE OF MARKET/STATION                
         MVC   PMKTSTA,MKTSTA      SET MARKET/STATION                           
         JE    NXTBUY70                                                         
*                                                                               
         XC    STAVALS(STAVALL),STAVALS                                         
         MVI   STASEND,YESQ        SET TO SEND STATION RECORD                   
         OI    RUNI1,RUNIMARK      SET MARKET NAME SENT                         
*                                                                               
*                                  GETSTA always returns radio call             
         GOTOR GETSTA,MKTSTA        with a '-' separating the band              
*                                                                               
         MVC   STASTA,STAPQSTA     SET STATION                                  
                                                                                
         CLI   STAPQSTA+4,C'-'     If 4 char call and band, then                
         JNE   NXTBUY55                                                         
         MVC   STAPQSTA+4(1),STAPQSTA+5   REPLACE '-' WITH BAND                 
         J     NXTBUY65                                                         
                                                                                
NXTBUY55 CLI   STAPQSTA+4,C' '     It can be a space for TV or cable            
         JE    *+12                                                             
         CLI   STAPQSTA+4,C'/'                                                  
         JNE   NXTBUY60                                                         
         MVI   STAPQSTA+4,C'T'                                                  
         J     NXTBUY65                                                         
                                                                                
NXTBUY60 CLI   STAPQSTA+3,C'-'     Something like  "WBZ-F"?                     
         JNE   NXTBUY65                                                         
         MVI   STAPQSTA+3,C' '     Make it "WBZ F", this is for ASTALST         
                                                                                
NXTBUY65 GOTOR ADBSTA,STAPQSTA                                                  
                                                                                
         CLC   LP_QMAPN,SDNBUYD#   NEW BUY DOWNLOAD?                            
         JE    NXTBUY70             YES, SKIP THESE TESTS                       
                                                                                
         GOTOR GETSCL                                                           
                                                                                
         CLI   STAPMED,TELMEDQ                                                  
         JNE   NXTBUY66                                                         
         CLI   STAPQSTA,C'0'       CABLE STATION?                               
         JL    NXTBUY67                                                         
         XC    WORK2,WORK2         TRANSLATE DDS 3CHAR NET TO 4CHAR             
         LA    R1,WORK2                                                         
         MVI   STAPACT-STAPACKD(R1),C'X'                                        
         MVC   STAPACOM-STAPACKD(L'STAPACOM,R1),ACOMFACS                        
         MVC   STAPQNET-STAPACKD(L'STAPQNET,R1),STAPQNET                        
         GOTOR VSTAPACK,WORK2                                                   
         CLI   STAPERR-STAPACKD(R1),0                                           
         JNE   *+2                                                              
         CLI   STAPQSTA-STAPACKD(R1),C' '                                       
         JE    NXTBUY67                                                         
         MVC   STADNWK,STAPQNET    DDS 3 CHAR NETWORK                           
         MVC   STANNWK,STAPQSTA-STAPACKD(R1)                                    
         J     NXTBUY67                                                         
*                                                                               
NXTBUY66 CLI   STAPMED,RADMEDQ     DO WE HAVE TO MODIFY CHANNEL?                
         JNE   NXTBUY67                                                         
         CLC   WORK,SPACES         NOT IF <= SPACES                             
         JNH   NXTBUY67                                                         
         CLI   STACHNL+2,C'.'      92.3 FORMAT?  WE HOPE                        
         JE    NXTBUY67                                                         
         MVI   STACHNL+3,C'.'      NO 1067, BUT SHOULD BE 106.7                 
         MVC   STACHNL+4(1),WORK+3 BETTER NOT BE  1.23  (SFM ALLOWS)            
                                                                                
NXTBUY67 DS    0H                                                               
*** NOP'D - CANADIAN AGENCY WILL NEVER BE SUPPORTED    -HWON 05/26/2020         
***BUY67 TM    RUNI1,RUNICANA      TEST CANADIAN AGENCY                         
***      JZ    NXTBUY68                                                         
***      MVC   STAMEDIA,BUYMED     SET MEDIA                                    
***      MVC   STAMKT#,MKTSTA      SET MARKET                                   
                                                                                
*** NOP'D - THESE ARE THE ONLY 3 VALID REQUESTS        -HWON 05/26/2020         
***BUY68 CLC   LP_QMAPN,SDBUYD#    TEST BUY DOWNLOAD                            
***      JE    NXTBUY69                                                         
***      CLC   LP_QMAPN,SDNBUYD#   OR NEW BUY DOWNLOAD                          
***      JE    NXTBUY69                                                         
***      CLC   LP_QMAPN,SDBUYDD#   OR DOWNLOAD ON EXIT FROM $BUY                
***      JNE   NXTBUY70                                                         
                                                                                
NXTBUY69 CLC   PMKT#,STAPQMKT      TEST/SET CHANGE OF MARKET                    
         MVC   PMKT#,STAPQMKT                                                   
         JE    NXTBUY70                                                         
         MVC   STAMKT#,MKTNUM                                                   
         GOTOR GETMKT,STAMKT#      YES - READ MARKET RECORD                     
*                                                                               
* IF HERE, FORMAT OUT BUY DATA (LINVALS) FOR DOWNLOAD                           
*                                                                               
NXTBUY70 GOTOR FMTLIN,BUYREC       FORMAT BUY LINE VALUES FOR DOWNLOAD          
                                                                                
         LA    R0,PKGVALS          CLEAR OUT PACKAGE VALUES                     
         LHI   R1,PKGVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GETPKG,BUYREC       EXTRACT PACKAGE VALUES                       
                                                                                
         LA    R0,ORBVALS          CLEAR OUT ORBIT VALUES                       
         LHI   R1,ORBVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR GETORB,BUYREC       EXTRACT ORBIT VALUES                         
                                                                                
         LA    R0,SCDVALS                                                       
         LHI   R1,SCDVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   PBDVALS,FF                                                       
         MVC   SCDSPILL,SPILL                                                   
         CLI   SCDSPILL,YESQ       TEST SPILL BUY                               
         JE    NXTBUY80                                                         
         GOTOR GETCST,BUYREC       NO - SET BUY COSTS                           
                                                                                
NXTBUY80 GOTOR GETDEM,BUYREC       EXTRACT DEMO VALUES                          
         CLI   SCDSPILL,YESQ                                                    
         BNE   EXITY                                                            
         MVC   SCDMKT#,BUYKMKTN    SET SPILL MARKET                             
                                                                                
         CLC   LP_QMAPN,SDBUYD#    TEST BUY DOWNLOAD                            
         JE    *+14                                                             
         CLC   LP_QMAPN,SDNBUYD#   OR NEW BUY DOWNLOAD                          
         JNE   NXTBUY85                                                         
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,SCDMKT#                                                     
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         IC    RF,BITLIST(RF)                                                   
         LR    R1,RE                                                            
         A     R1,AIO7                                                          
         BASR  RE,0                                                             
         TM    0(R1),0                                                          
         EX    RF,0(RE)            TEST ALREADY SENT MARKET NAME                
         JNZ   NXTBUY85                                                         
         BASR  RE,0                                                             
         OI    0(R1),0                                                          
         EX    RF,0(RE)                                                         
***                                                                             
         MVC   WORK(STACLT-STAMKTN),STAMKTN  SAVE SO WE CAN RESTORE IT          
         GOTOR GETMKT,SCDMKT#                                                   
         MVC   SCDMKTN,STAMKTN     NO - SEND MARKET NAME                        
         MVC   SCDMALF,STAAMKT            AND ALPHA                             
         MVC   STAMKTN(STACLT-STAMKTN),WORK  RESTORE IT NOW                     
                                                                                
NXTBUY85 CLC   STACOD,BUYKSTAC     TEST SAME STATION CODE                       
         BE    EXITY                                                            
         GOTOR GETSTA,BUYKMSTA                                                  
         MVC   SCDSTA,STAPQSTA     SET STATION                                  
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SPILL-OUT BUY STATION/COST/DEMOS               *         
***********************************************************************         
                                                                                
ARYSPO   LKOUT A,(R,NXTSPO),MULTIROW=Y,ROWNAME=SCDVALS                          
                                                                                
MktNo    LKOUT C,2,SCDMKT#,LBIN,ND=Y                                            
Array    LKOUT C,255,(A,ARYSDSD)                                                
MktNm    LKOUT C,9,SCDMKTN,CHAR,ND=Y                                            
MktAlpha LKOUT C,11,SCDMALF,CHAR,ND=Y   WAS MAP 10, CHANGED FOR SGIN            
Array    LKOUT C,X'0065',(A,ARYPBDS)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO BUILD SPILL-OUT DEMO VALUES LINE                         *         
***********************************************************************         
                                                                                
NXTSPO   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTSPO02                                                         
         CLI   SPILL,YESQ          TEST SPILL BUY                               
         BE    NOMORE                                                           
         L     R1,AIO5                                                          
         AHI   R1,BDELEM-BUYREC                                                 
         ST    R1,ANDELEM                                                       
                                                                                
NXTSPO02 LA    R0,SCDVALS                                                       
         LHI   R1,SCDVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   PBDVALS,FF                                                       
         LA    R0,SCDVALS                                                       
         ST    R0,LP_ADATA         SET A(ARRAY FOR DDLINK)                      
                                                                                
         L     R3,ANDELEM          POINT TO LAST ELEMENT AND BUMP               
         USING NDELEM,R3                                                        
NXTSPO04 LLC   R0,NDLEN            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         CLI   NDCODE,0            TEST END OF RECORD                           
         BE    NOMORE                                                           
         CLI   NDCODE,NDCSPLQ      TEST SPILL DEMO ELEMENT                      
         JNE   NXTSPO04                                                         
         ST    R3,ANDELEM                                                       
                                                                                
         MVC   SCDMKT#,NDPROG      SET AGENCY SPILL MARKET NUMBER               
         SR    RE,RE                                                            
         ICM   RE,3,SCDMKT#                                                     
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         IC    RF,BITLIST(RF)                                                   
         LR    R1,RE                                                            
         A     R1,AIO7                                                          
         BASR  RE,0                                                             
         TM    0(R1),0                                                          
         EX    RF,0(RE)            TEST ALREADY SENT MARKET NAME                
         JNZ   NXTSPO06                                                         
         BASR  RE,0                                                             
         OI    0(R1),0                                                          
         EX    RF,0(RE)                                                         
***                                                                             
         MVC   WORK(STACLT-STAMKTN),STAMKTN  SAVE SO WE CAN RESTORE IT          
         GOTOR GETMKT,SCDMKT#                                                   
         MVC   SCDMKTN,STAMKTN     NO - SEND MARKET NAME                        
         MVC   SCDMALF,STAAMKT            AND ALPHA                             
         MVC   STAMKTN(STACLT-STAMKTN),WORK  RESTORE IT NOW                     
                                                                                
NXTSPO06 LA    R1,NDELEM                                                        
         ICM   R1,8,HEXC0                                                       
         GOTOR GETDEM                                                           
         B     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SPOT DOWNLOAD                                  *         
***********************************************************************         
                                                                                
ARYSPT   LKOUT A,(R,GETSPT),NEWEL=B,NROWS=(B#SAVED,SPTTAB#),           +        
               ROWNAME=SPTTABD,ROWWIDTH=SPTTABL                                 
                                                                                
MapCd    LKOUT C,255,SPTMAPN,EMAP                                               
SStat    LKOUT C,1,SPTSTAT,HEXD                                                 
SDate    LKOUT C,2,SPTDATE,EDAT,ND=Y                                            
SRef#    LKOUT C,3,SPTREF#,LBIN,ND=Y                                            
RFact    LKOUT C,4,SPTREP#,LBIN,ND=Y                                            
P1Cod    LKOUT C,5,SPTPRD1,(U,#EDTPRD,$EDTPRD),ND=Y                             
P2Cod    LKOUT C,6,SPTPRD2,(U,#EDTPRD,$EDTPRD),ND=Y                             
SLen1    LKOUT C,11,SPT1LEN,LBIN,ND=Y                                           
AfDat    LKOUT C,12,SPTAFDT,CDAT,ND=Y                                           
AfTim    LKOUT C,13,SPTAFTM,LBIN,ND=Y                                           
COver    LKOUT C,14,SPTCOST,SPAK,ND=Y                                           
MgdCd    LKOUT C,15,SPTMGC,CHAR,ND=Y                                            
Film1    LKOUT C,16,SPTFILM1,(R,EDTFLM),ND=Y                                    
Film2    LKOUT C,16,SPTFILM2,(R,EDTFLM),ND=Y                                    
ClrDt    LKOUT C,17,SPTCLRDT,CDAT,ND=Y                                          
PaySq    LKOUT C,18,SPTCLRSQ,LBIN,ND=Y                                          
FlmF1    LKOUT C,20,SPTFILM1,HEXD,ND=Y,PCVERSION=3.0.0.101                      
FlmF2    LKOUT C,20,SPTFILM2,HEXD,ND=Y,PCVERSION=3.0.0.101                      
FlmTy    LKOUT C,21,SPTFLMTY,HEXD,ND=Y,PCVERSION=3.0.0.101                      
FlmDy    LKOUT C,22,SPTFLMDY,HEXD,ND=Y,PCVERSION=3.0.0.101                      
FlmPt    LKOUT C,23,SPTFLMPT,HEXD,ND=Y,PCVERSION=3.0.0.101                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO BUILD SPOT RECORDS                                       *         
***********************************************************************         
                                                                                
GETSPT   L     R2,IOADDR                                                        
         USING BUYREC,R2           R2=A(BUY RECORD)                             
         XC    SPTTAB#,SPTTAB#     INITIALIZE N'ENTRIES IN TABLE                
         XC    SPOTDATE,SPOTDATE   INITIALIZE SPOT DATE                         
         MVC   SPOTTLEN,BDSEC      SET TOTAL SECONDS LENGTH                     
                                                                                
         LA    R2,BDELEM                                                        
         USING REGELEM,R2          R2=A(BUY ELEMENT)                            
                                                                                
         L     R3,ATIA             POINT TO TIA ONLINE                          
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    *+8                                                              
         L     R3,ASDSTAB          POINT TO GETMAIN AREA OFFLINE                
         ST    R3,LP_ADATA         SET A(ARRAY FOR DDLINK)                      
TAB      USING SPTTABD,R3          R3=A(CURRENT TABLE ENTRY)                    
         SR    R4,R4                                                            
PRV      USING SPTTABD,R4          R4=A(PREVIOUS TABLE ENTRY)                   
                                                                                
GETSPT02 LLC   R0,RLEN             BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         CLI   RCODE,0             TEST END OF RECORD                           
         JE    GETSPT32                                                         
         CLI   RCODE,RCORGQ        TEST FOR SPOT ELEMENTS                       
         JL    GETSPT02                                                         
         CLI   RCODE,X'0D'                                                      
         JH    GETSPT02                                                         
                                                                                
         CLC   RDATE,SPOTDATE      NEW NEW DATE                                 
         JE    *+14                                                             
         MVC   SPOTDATE,RDATE      YES - SET NEW DATE                           
         MVI   SPOTREFN,0          AND INITIALIZE SPOT REFERENCE NUMBER         
                                                                                
         GOTOR FLTSPT,REGELEM      APPLY SPOT FILTERS                           
         JNE   GETSPT02                                                         
                                                                                
         XC    TAB.SPTTABD(SPTTABL),TAB.SPTTABD                                 
                                                                                
         MVC   TAB.SPTMAPN,SPTPTMAP                                             
         MVC   TAB.SPTREF#,SPOTREFN                                             
         MVI   TAB.SPTREP#,1                                                    
         GOTOR VDATCON,DMCB,(2,RDATE),(0,TAB.SPTDATE)                           
                                                                                
         CLI   DLDCLEAR,0          TEST CLEARANCE DATA REQUESTED                
         JE    GETSPT04                                                         
         MVC   TAB.SPTCLRDT,RPAY   YES - SET CLEARANCE DATE                     
         OC    TAB.SPTCLRDT,TAB.SPTCLRDT                                        
         JZ    GETSPT04                                                         
         CLC   TAB.SPTCLRDT,BUYCLRLO                                            
         JH    *+10                                                             
         MVC   BUYCLRLO,TAB.SPTCLRDT                                            
         CLC   TAB.SPTCLRDT,BUYCLRHI                                            
         JL    GETSPT04                                                         
         MVC   BUYCLRHI,TAB.SPTCLRDT                                            
                                                                                
GETSPT04 OC    RPAY,RPAY           SET SPOT STATUS                              
         JZ    *+8                                                              
         OI    TAB.SPT#PAID,SPTSPAID                                            
         TM    RSTATUS,RSMINSDQ                                                 
         JZ    *+8                                                              
         OI    TAB.SPT#MISD,SPTSMISD                                            
         TM    RSTATUS,RSMKGDPQ                                                 
         JZ    *+8                                                              
         OI    TAB.SPT#MGPD,SPTSMGPD                                            
         TM    RSTATUS,RSMGONLQ                                                 
         JZ    *+8                                                              
         OI    TAB.SPT#MKGD,SPTSMKGD                                            
         TM    RSTATUS,RSNOALLQ                                                 
         JZ    *+8                                                              
         OI    TAB.SPT#PRAL,SPTSPRAL                                            
         TM    RSTATUS,RSHIATSQ                                                 
         JZ    *+8                                                              
         OI    TAB.SPT#HIAT,SPTSHIAT                                            
                                                                                
         CLI   RCODE,RCPOTOQ           X'0C' ELEM?                              
         JNE   GETSPT08                                                         
         TM    RSTATUS,RSMINUSQ        TEST MINUS SPOT                          
         JZ    GETSPT06                NO                                       
         OI    TAB.SPT#MOTO,SPTSMOTO   YES, -OTO                                
         OI    TAB.SPT#MINS,SPTSMINS   & MINUS SPOT                             
         J     GETSPT08                                                         
                                                                                
GETSPT06 OI    TAB.SPT#POTO,SPTSPOTO   +OTO                                     
                                                                                
GETSPT08 CLI   RLEN,RLPOL1LQ                                                    
         JL    *+8                                                              
         OI    TAB.SPT#ALOC,SPTSALOC                                            
         TM    RSTATUS,RSB1PALQ                                                 
         JZ    *+8                                                              
         OI    TAB.SPT#B1PA,SPTSB1PA                                            
                                                                                
         TM    RSTATUS,RSRATOVQ    TEST COST OVERRIDE                           
         JZ    GETSPT10                                                         
         SR    RF,RF                                                            
         ICM   RF,7,RPCOST                                                      
         TM    LINSTAT,BDSPRECQ    TEST POL NPW                                 
         JZ    *+8                                                              
         N     RF,=X'0003FFFF'                                                  
         TM    LINCIND,BDCMINSQ    TEST MINUS RATE                              
         JZ    *+6                                                              
         LNR   RF,RF                                                            
         CVD   RF,DUB                                                           
         ZAP   TAB.SPTCOST,DUB     SET COST OVERRIDE                            
         OI    TAB.SPT#COVR,SPTSCOVR    AND ITS STATUS BIT                      
                                                                                
GETSPT10 CLI   RLEN,RLPOL1LQ       NO - TEST ALLOCATED                          
         JL    *+10                                                             
         MVC   TAB.SPTPRD1,RPALLOC+(RPPRD-RPALLOC)                              
         CLI   RLEN,RLPOL2LQ       TEST PIGGYBACK ALLOCATION                    
         JL    GETSPT14                                                         
         MVC   TAB.SPTPRD2,RPALLOC2+(RPPRD-RPALLOC)                             
         LLC   RE,SPOTTLEN                                                      
         LLC   RF,RPALLOC2+(RPTIME-RPALLOC)                                     
         SR    RE,RF                                                            
         STC   RE,TAB.SPT1LEN      SET SPOT LENGTH FOR PRIMARY PRODUCT          
                                                                                
GETSPT14 CLI   DLDCLEAR,0          TEST CLEARANCE DATA REQUESTED                
         JE    *+10                                                             
         MVC   TAB.SPTCLRSQ,RPAYSEQ                                             
         CLI   RLEN,RLPOL1LQ       TEST ALLOCATED                               
         JL    GETSPT16                                                         
         CLI   DLDCLEAR,0          TEST CLEARANCE DATA REQUESTED                
         JE    *+10                                                             
         MVC   TAB.SPTCLRSQ,RPPAYSEQ                                            
         TM    RPSTAT2,X'FF'       TEST MAKEGOOD CODE PRESENT                   
         JZ    GETSPT16                                                         
                                                                                
         LA    R6,ELEM                                                          
         USING MGABLKD,R6                                                       
                                                                                
         XC    0(MGALNQ,R6),0(R6)                                               
                                                                                
         MVI   MGAACT,MGAQTRNS                                                  
         MVC   MGAACOM,ACOMFACS    SET A(COMFACS)                               
         L     RF,IOADDR           A(BUYREC)                                    
         ST    RF,MGAIO                                                         
         ST    R2,MGAELEM                                                       
                                                                                
         GOTOR VBLDMGN,MGABLKD                                                  
         CLI   MGAERR,0                                                         
         JNE   *+2                                                              
                                                                                
         MVC   TAB.SPTMGC,MGQCODE                                               
         DROP  R6                                                               
                                                                                
GETSPT16 LA    R1,REGELEM          LOOK FOR OTHER SPOT RELATED ELEMENTS         
                                                                                
GETSPT18 LLC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),EOR           TEST END OF RECORD                           
         JE    GETSPT28                                                         
         CLI   0(R1),FLMCODEQ      X'12' - TEST FILM ELEMENT                    
         JE    GETSPT22                                                         
         CLI   0(R1),ACCODEQ       X'10' - TEST AFFIDAVIT ELEMENT               
         JE    GETSPT24                                                         
         CLI   0(R1),TRACIDQ       X'18' - TEST TRF CML/ASSIGN ELEM             
         JE    GETSPT26                                                         
                                                                                
         CLI   0(R1),RCORGQ                                                     
         JL    GETSPT18                                                         
         CLI   0(R1),X'0D'                                                      
         JH    GETSPT20                                                         
         TM    RSTATUS-REGELEM(R1),RSMINUSQ                                     
         JZ    GETSPT20                                                         
         OC    RPAY-REGELEM(,R1),RPAY-REGELEM(R1)                               
         JZ    GETSPT20                                                         
         OI    TAB.SPT#MSPD,SPTSMSPD                                            
                                                                                
GETSPT20 CLI   0(R1),X'08'                                                      
         JNH   GETSPT28                                                         
         CLI   0(R1),RCPOLOQ                                                    
         JL    GETSPT18                                                         
         CLI   0(R1),X'0D'                                                      
         JNH   GETSPT28                                                         
         J     GETSPT18                                                         
                                                                                
         USING FLMELEM,R1          PROCESS FILM ELEMENT                         
GETSPT22 OI    TAB.SPT#FILM,SPTSFILM                                            
         MVC   TAB.SPTFLMTY,FLMCODE                                             
         MVC   TAB.SPTFLMDY,FLMDAY                                              
         MVC   TAB.SPTFILM1,FLMNUM                                              
         CLI   FLMLEN,FLMNUM+L'FLMNUM-FLMELEM                                   
         JNH   GETSPT18                                                         
         MVC   TAB.SPTFILM2,FLMNUM+L'FLMNUM                                     
         J     GETSPT18                                                         
                                                                                
         USING AFFELEM,R1          PROCESS AFFIDAVIT ELEMENT                    
GETSPT24 OI    TAB.SPT#AFID,SPTSAFID                                            
         CLI   DLDAFFID,0          TEST WANT AFFID DATA                         
         JE    GETSPT18                                                         
         MVC   TAB.SPTAFDT,ADATE   SET AFFIDAVIT VALUES                         
         MVC   TAB.SPTAFTM,ATIME                                                
         NI    TAB.SPTAFTM,X'0F'   TAKE OFF AFFID STATUS BIT(0-3) FLAGS         
         J     GETSPT18                                                         
                                                                                
         USING TRACID,R1           PROCESS TRAFFIC CML ASSIGN ELEM              
GETSPT26 TM    TAB.SPT#FILM,SPTSFILM   ALREADY HAVE X'12' ELEM?                 
         JNZ   GETSPT18                 YES, X'12' SUPERCEDES X'18'             
         OI    TAB.SPT#FILM,SPTSFILM                                            
         MVC   TAB.SPTFLMTY,TRACID                                              
         MVC   TAB.SPTFILM1,TRACCSQ                                             
         MVC   TAB.SPTFILM2,TRACCSQ2                                            
         MVC   TAB.SPTFLMPT,TRACREF                                             
         J     GETSPT18                                                         
         DROP  R1                                                               
                                                                                
GETSPT28 LTR   R4,R4               TEST FIRST TIME                              
         JZ    GETSPT30                                                         
         LLC   R1,PRV.SPTREF#                                                   
         LLC   R0,PRV.SPTREP#                                                   
         AR    R1,R0                                                            
         CLM   R1,1,TAB.SPTREF#    TEST SPOT REF# IN SEQUENCE                   
         JNE   GETSPT30                                                         
         CLC   TAB.SPTCOMP(SPTCOMPL),PRV.SPTCOMP                                
         JNE   GETSPT30                                                         
         LLC   R1,PRV.SPTREP#      SPOT DATA IS THE SAME AS PREVIOUS...         
         AHI   R1,1                ...BUMP THE DUPLICATION FACTOR               
         STC   R1,PRV.SPTREP#                                                   
         J     GETSPT02                                                         
                                                                                
GETSPT30 LA    R4,TAB.SPTTABD      POINT TO CURRENT (PREVIOUS)                  
         AHI   R3,SPTTABL          POINT TO NEXT (CURRENT)                      
         LH    R0,SPTTAB#          BUMP SPOT TABLE ENTRY COUNT                  
         AHI   R0,1                                                             
         STH   R0,SPTTAB#                                                       
         J     GETSPT02            GET NEXT ELEMENT                             
                                                                                
GETSPT32 SR    R0,R0               DO SPOT DOWNLOAD OPTIMIZATION                
         ICM   R0,3,SPTTAB#                                                     
         BZ    EXITY                                                            
         L     R3,LP_ADATA         POINT TO START OF TABLE                      
PRV      USING SPTTABD,WORK                                                     
         XC    PRV.SPTTABD(SPTTABL),PRV.SPTTABD                                 
                                                                                
GETSPT34 CLC   TAB.SPTDATE,PRV.SPTDATE                                          
         JE    *+12                                                             
         MVI   PRV.SPTREF#,0       RESET REFERENCE# AND REPLICATION#            
         MVI   PRV.SPTREP#,1       ON CHANGE OF SPOT DATE                       
                                                                                
         LLC   RE,PRV.SPTREF#                                                   
         MVC   PRV.SPTREF#,TAB.SPTREF#                                          
         LLC   RF,PRV.SPTREP#                                                   
         MVC   PRV.SPTREP#,TAB.SPTREP#                                          
         AR    RE,RF                                                            
         CLM   RE,1,TAB.SPTREF#    TEST CURRENT REF# IN SEQUENCE                
         JNE   *+8                                                              
         MVI   TAB.SPTREF#,0       YES - CLEAR REFERENCE#                       
         CLI   TAB.SPTREP#,1       TEST REPLICATION FACTOR IS ONE               
         JNE   *+8                                                              
         MVI   TAB.SPTREP#,0       YES - CLEAR REPLICATION FACTOR               
                                                                                
         CLC   PRV.SPTDATE,TAB.SPTDATE                                          
         JNE   *+14                                                             
         XC    TAB.SPTDATE,TAB.SPTDATE                                          
         J     GETSPT36                                                         
         OC    PRV.SPTDATE,PRV.SPTDATE                                          
         JNZ   *+14                                                             
         MVC   PRV.SPTDATE,TAB.SPTDATE                                          
         J     GETSPT36                                                         
         GOTOR VADDAY,DMCB,PRV.SPTDATE,WORK2,7                                  
         CLC   TAB.SPTDATE,WORK2   TEST/SET SPOT WEEK IN SEQUENCE               
         MVC   PRV.SPTDATE,TAB.SPTDATE                                          
         JNE   GETSPT36                                                         
         OI    TAB.SPT#ISEQ,SPTSISEQ                                            
         XC    TAB.SPTDATE,TAB.SPTDATE                                          
                                                                                
GETSPT36 CLC   PRV.SPTPRDS,TAB.SPTPRDS                                          
         MVC   PRV.SPTPRDS,TAB.SPTPRDS                                          
         JNE   *+10                                                             
         XC    TAB.SPTPRDS,TAB.SPTPRDS                                          
                                                                                
         SR    RE,RE               TEST 50/50 SPOT SPLIT                        
         ICM   RE,1,TAB.SPT1LEN                                                 
         SLL   RE,1                                                             
         CLM   RE,1,SPOTTLEN                                                    
         JNE   GETSPT38                                                         
         MVI   TAB.SPT1LEN,0                                                    
                                                                                
GETSPT38 CLC   PRV.SPTAFDT,TAB.SPTAFDT                                          
         MVC   PRV.SPTAFDT,TAB.SPTAFDT                                          
         JNE   *+10                                                             
         XC    TAB.SPTAFDT,TAB.SPTAFDT                                          
                                                                                
         CLC   PRV.SPTFILMV(SPTFILML),TAB.SPTFILMV                              
         MVC   PRV.SPTFILMV(SPTFILML),TAB.SPTFILMV                              
         JNE   *+10                                                             
         XC    TAB.SPTFILMV(SPTFILML),TAB.SPTFILMV                              
                                                                                
         CLC   PRV.SPTMGC,TAB.SPTMGC                                            
         MVC   PRV.SPTMGC,TAB.SPTMGC                                            
         JNE   *+10                                                             
         XC    TAB.SPTMGC,TAB.SPTMGC                                            
                                                                                
         AHI   R3,SPTTABL          BUMP TO NEXT TABLE ENTRY                     
         JCT   R0,GETSPT34         DO FOR NUMBER OF SPOTS                       
         B     EXITY                                                            
         DROP  TAB,PRV,R2                                                       
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR POST BUY SPILL DEMO VALUES                     *         
***********************************************************************         
                                                                                
ARYPBDS  LKOUT A,(D,B#SAVED,PBDVALS),NEWEL=Y,EOT=FF,ROWWIDTH=L'PBDDEMV          
                                                                                
PRout    LKOUT P,,INITDEMS                                                      
DemCd    LKOUT C,1,PBDDEMO,(R,EDTBDCD)                                          
DemVl    LKOUT C,2,PBDDEM#,(R,EDTDVL)                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SPILL-OUT DEMO VALUES                          *         
***********************************************************************         
                                                                                
ARYSDSD  LKOUT A,(D,B#SAVED,SCDDEMV),NROWS=(B#SAVED,SCDDEMN),          +        
               ROWWIDTH=L'SCDDEMV                                               
                                                                                
PRout    LKOUT P,,INITDEMS                                                      
DemCd    LKOUT C,12,SCDDEMO,(R,EDTBDCD),FILTROUT=TSTSDBDD,             +        
               PCVERSION=3.3.0.8                                                
DemVl    LKOUT C,6,SCDDEM#,(R,EDTDVL)                                           
DemOv    LKOUT C,8,SCDDEMF,HDRO,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION DOWNLOAD                               *         
***********************************************************************         
                                                                                
ARYSDV   LKOUT A,(D,B#SAVED,STAVALS),NROWS=1,SENDONCE=Y,NEWEL=Y,       +        
               ROWNAME=STAVALS,ROWID=(STASEND,YESQ),ROWWIDTH=STAVALL            
                                                                                
Media    LKOUT C,1,STAMEDIA,CHAR,ND=Y                                           
MktNo    LKOUT C,2,STAMKT#,LBIN,ND=Y                                            
Statn    LKOUT C,3,STASTA,CHAR                                                  
MktNm    LKOUT C,4,STAMKTN,CHAR,ND=Y                                            
Chnnl    LKOUT C,5,STACHNL,CHAR,ND=Y                                            
NwAff    LKOUT C,6,STANTWK,CHAR,ND=Y                                            
Frmat    LKOUT C,7,STAFRMT,CHAR,ND=Y                                            
TaxRt    LKOUT C,9,STATAX,CHAR,ND=Y                                             
BkTyp    LKOUT C,10,STABTYP,(R,TRNSBKT),ND=Y                                    
MktAC    LKOUT C,11,STAAMKT,CHAR,ND=Y,PCVERSION=2.5.0.13                        
MkLPM    LKOUT C,12,STALPMD,CDAT,ND=Y,PCVERSION=2.6.0.0                         
MkCDm    LKOUT C,13,STACDEM,CHAR,ND=Y,PCVERSION=2.6.0.0                         
**MktTmZ   LKOUT C,14,STAMKTZ,CHAR,PCVERSION=3.1.0.111                          
**MktBkTyp LKOUT C,15,(D,B#SAVED,STAMKBT),(R,TRNSBKT),                          
*               ND=Y,PCVERSION=3.3.0.6                                          
MkAAMkt  LKOUT C,16,STAAAMK,LBIN,ND=Y,PCVERSION=4.5.0.63                        
DDSNwk   LKOUT C,30,STADNWK,CHAR,ND=Y                                           
NSINwk   LKOUT C,31,STANNWK,CHAR,ND=Y                                           
*NNtwk    LKOUT C,15,STANNWK,CHAR,ND=Y,PCVERSION=2.6.0.0                        
Array    LKOUT C,255,(A,ARYPC2)                                                 
CSpcf    LKOUT C,33,STACLT,CHAR,ND=Y                                            
PrntPls  LKOUT C,35,STAPPLUS,CHAR,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUYLINE DOWNLOAD                               *         
***********************************************************************         
                                                                                
ARYSDL   LKOUT A,(D,B#SAVED,LINVALS),NROWS=1,ROWWIDTH=LINVALL,NEWEL=Y           
                                                                                
EstNo    LKOUT C,1,LINEST,LBIN,ND=Y                                             
Line#    LKOUT C,2,LINLIN,LBIN,ND=Y                                             
Rottn    LKOUT C,3,LINROT,LBIN,ND=Y                                             
SDayN    LKOUT C,4,LINDAY,LBIN,ND=Y                                             
STime    LKOUT C,5,LINSTRT,LBIN                                                 
Etime    LKOUT C,6,LINENDT,LBIN,ND=Y                                            
SptLn    LKOUT C,7,LINSEC,LBIN,ND=Y                                             
DayPt    LKOUT C,8,LINDPT,CHAR,ND=Y                                             
ProgN    LKOUT C,9,LINPGN,CHAR,ND=Y                                             
ProgC    LKOUT C,10,LINPGC,CHAR,ND=Y                                            
ConNo    LKOUT C,11,LINCON,CHAR,ND=Y                                            
AdjCd    LKOUT C,12,LINADJ,CHAR,ND=Y                                            
P1Cod    LKOUT C,13,LINPRD,CHAR,ND=Y                                            
P2Cod    LKOUT C,14,LINPIG,CHAR,ND=Y                                            
BType    LKOUT C,15,LINBTYPE,CHAR,ND=Y                                          
BPerd    LKOUT C,16,LINPER,CHAR,ND=Y                                            
Buyer    LKOUT C,18,LINBYR,CHAR,ND=Y                                            
Comm1    LKOUT C,19,LINCOM1,CHAR,ND=Y                                           
Comm2    LKOUT C,20,LINCOM2,CHAR,ND=Y                                           
Comm3    LKOUT C,21,LINCOM3,CHAR,ND=Y                                           
Comm4    LKOUT C,22,LINCOM4,CHAR,ND=Y                                           
Comm5    LKOUT C,23,LINCOM5,CHAR,ND=Y                                           
MgdCd    LKOUT C,24,LINMGC,CHAR,ND=Y                                            
SpPWk    LKOUT C,25,LIN#WK,LBIN,ND=Y                                            
Purps    LKOUT C,26,LINPRP,CHAR,ND=Y                                            
RType    LKOUT C,27,LINRTY,CHAR,ND=Y                                            
Reasn    LKOUT C,28,LINRSN,CHAR,ND=Y                                            
RepCd    LKOUT C,29,LINREP,CHAR,ND=Y                                            
TaxRt    LKOUT C,30,LINTAX,LBIN,ND=Y                                            
Upgrd    LKOUT C,31,LINUPG,CHAR,ND=Y                                            
RBook    LKOUT C,32,LINRBOOK,BMON,ND=Y                                          
Cksum    LKOUT C,33,LINCKSM,HEXD,ND=Y                                           
SDate    LKOUT C,34,LINSTRD,BDAT                                                
EDate    LKOUT C,35,LINENDD,BDAT                                                
Array    LKOUT C,36,(A,ARYERR)                                                  
ChDat    LKOUT C,37,LINCHGD,BDAT,ND=Y                                           
ChWhy    LKOUT C,38,LINWHY,HEXD,ND=Y,LEN=LINWHYL                                
PCKey    LKOUT C,39,LINPCKEY,CHAR,ND=Y                                          
OLock    LKOUT C,40,(D,B#SAVED,OLOCK),HDRO,ND=Y                                 
BStat    LKOUT C,41,LINSTAT,HEXD,ND=Y,LEN=LINSTATL                              
PkgTy    LKOUT C,42,(D,B#SAVED,PKGTYPE),UBIN,FILTROUT=TSTNEPKG,        +        
               SKIPCOLS=1                                                       
Array    LKOUT C,43,(A,ARYPKG)                                                  
Array    LKOUT C,44,(A,ARYAVG)                                                  
Array    LKOUT C,45,(A,ARYAAU)                                                  
Statn    LKOUT C,51,(D,B#SAVED,STASTA),CHAR,PCVERSION=3.0.0.6                   
RecDA    LKOUT C,60,(D,B#SAVED,LINDA),HEXD,ND=Y,PCVERSION=3.0.0.30              
RecSz    LKOUT C,62,(D,B#SAVED,LINRSIZ),UBIN,ND=Y,PCVERSION=3.0.0.120           
                                                                                
         LKOUT E                                                                
                                                                                
TSTNEPKG OC    PKGNLIN,PKGNLIN     MUST HAVE ONE TO BE A PACKAGE                
         B     SETCCC                                                           
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ORIGINAL BUY STATION/COST/DEMOS                *         
***********************************************************************         
                                                                                
ARYSDO   LKOUT A,(D,B#SAVED,SCDVALS),NEWEL=Y,NROWS=1,ROWNAME=SCDVALS,  +        
               ROWID=(SCDSPILL,NOQ),ROWWIDTH=SCDVALL                            
                                                                                
Array    LKOUT C,X'0054',(A,ARYSDOV)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ORBIT LINE DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYSORB  LKOUT A,(R,NXTORB),MULTIROW=Y                                          
                                                                                
Array    LKOUT C,X'0094',(A,ARYORBT)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO BUILD ORBIT LINE                                         *         
***********************************************************************         
                                                                                
NXTORB   XR    RF,RF                                                            
         ICM   RF,3,ORBVALN                                                     
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JE    NXTORB02                                                         
         MVC   ORBVDAY(8*ORBVLNQ),ORBVDAY+ORBVLNQ                               
         SHI   RF,1                                                             
         STCM  RF,3,ORBVALN                                                     
                                                                                
NXTORB02 CHI   RF,1                ANY ORBITS?                                  
         BL    NOMORE              NO MORE                                      
         LA    R0,ORBVDAY                                                       
         ST    R0,LP_ADATA         SET A(ARRAY FOR DDLINK)                      
         B     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ORBIT                                          *         
***********************************************************************         
                                                                                
ARYORBT  LKOUT A,(D,B#SAVED,ORBVALS),NEWEL=Y,NROWS=1,ROWWIDTH=ORBVLNQ           
                                                                                
ODayN    LKOUT C,1,ORBVDAY,LBIN,ND=Y                                            
OSTim    LKOUT C,2,ORBVSTM,LBIN                                                 
OETim    LKOUT C,3,ORBVETM,LBIN,ND=Y                                            
OPrgN    LKOUT C,4,ORBVPRG,CHAR,ND=Y                                            
ODemV    LKOUT C,5,ORBVDMV,(R,EDTDVLO)                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR POST BUY ORIGINAL DEMO VALUES                  *         
***********************************************************************         
                                                                                
ARYPBDO  LKOUT A,(D,B#SAVED,PBDVALS),NEWEL=Y,EOT=FF,ROWWIDTH=L'PBDDEMV          
                                                                                
PRout    LKOUT P,,INITDEMS                                                      
DemCd    LKOUT C,1,PBDDEMO,(R,EDTBDCD)                                          
DemVl    LKOUT C,2,PBDDEM#,(R,EDTDVL)                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SPILL-IN BUY STATION/COST/DEMOS                *         
***********************************************************************         
                                                                                
ARYSDSI  LKOUT A,(D,B#SAVED,SCDVALS),NEWEL=Y,NROWS=1,                  +        
               ROWNAME=SCDVALS,ROWID=(SCDSPILL,YESQ),ROWWIDTH=SCDVALL           
                                                                                
Array    LKOUT C,X'0055',(A,ARYSDOV)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR STATION/COST/DEMOS LINE DOWNLOAD               *         
***********************************************************************         
                                                                                
ARYSDOV  LKOUT A,(D,B#SAVED,SCDVALS),NEWEL=Y,NROWS=1,ROWWIDTH=SCDVALL           
                                                                                
Statn    LKOUT C,1,SCDSTA,CHAR,ND=Y                                             
MktNo    LKOUT C,2,SCDMKT#,LBIN,ND=Y                                            
MSRef    LKOUT C,3,SCDMKST#,LBIN,ND=Y                                           
Cost1    LKOUT C,4,SCDCOST1,SPAK,ND=Y                                           
Cost2    LKOUT C,5,SCDCOST2,SPAK,ND=Y                                           
PRout    LKOUT P,,INITDEMS                                                      
Array    LKOUT C,255,(A,ARYSDDV)                                                
MktNm    LKOUT C,9,SCDMKTN,CHAR,ND=Y                                            
C2Fac    LKOUT C,10,SCDC2FAC,CBIN,ND=Y                                          
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMO DOWNLOAD                                  *         
***********************************************************************         
                                                                                
ARYSDDV  LKOUT A,(D,B#SAVED,SCDDEMV),NROWS=(B#SAVED,SCDDEMN),          +        
               ROWWIDTH=L'SCDDEMV                                               
                                                                                
DemCd    LKOUT C,6,SCDDEMO,(R,EDTBDCD),FILTROUT=TSTSDBDD                        
DemVl    LKOUT C,7,SCDDEM#,(R,EDTDVL)                                           
DemOv    LKOUT C,8,SCDDEMF,HDRO,ND=Y                                            
*NtDemoNm LKOUT C,11,(D,B#SAVED,SCDNTDEM),CHAR,ND=Y                             
NtDemRLK LKOUT C,11,(D,B#SAVED,SCDNTDMF),MB80,FILTROUT=TSTNTRD                  
NtDemSLK LKOUT C,12,(D,B#SAVED,SCDNTDMF),MB40,FILTROUT=TSTNTRD                  
NtDemNA  LKOUT C,13,(D,B#SAVED,SCDNTDMF),MB20,FILTROUT=TSTNTRD                  
                                                                                
         LKOUT E                                                                
                                                                                
TSTSDBDD MVI   SCDNTDMF,0          RESET NON-TRAD FLAG                          
         L     R2,LP_AINP                                                       
         LLC   RF,0(R2)                                                         
         ICM   RF,B'0010',2(R2)    BYTE1&3 ZERO, NON-TRADITIONAL?               
         LTR   RF,RF                                                            
****     JNZ   TSTSDBD9            NO                                           
**** SBTK NEEDS DEMO CODE TO ASSIGN VALUES CORRECTLY                            
**** SO CHANGING THIS TO ALWAYS DOWNLOAD TRADITIONAL DEMO CODE                  
         JNZ   RTRNYES                                                          
         OC    LP_VRSN,LP_VRSN     YES, IF NO VERSION                           
         JZ    RTRNYES                                                          
         CLC   LP_VRSN,V460050     OR VERSION 4.6.0.50 AND HIGHER               
         JNL   RTRNYES             THEN RETURN CATEGORY                         
TSTSDBD9 DS    0H                                                               
         CLC   LP_QMAPN,SDBUYDD#   SET CC=EQUAL FOR BUY RETURN                  
         BR    RE                                                               
                                                                                
TSTNTRD  OC    LP_VRSN,LP_VRSN     IF NO VERSION                                
         JZ    TSTNTRD5             THEN GO TEST IF NON-TRAD DEMO               
         CLC   LP_VRSN,V460050     IF VERSION LT 4.6.0.50                       
         JL    RTRNNO               THEN DON'T RETURN                           
TSTNTRD5 L     R1,LP_AINP          LETS TEST IF NON-TRAD DEMO                   
         LLC   RF,0(R1)                                                         
         ICM   RF,B'0010',2(R1)    BYTE1&3 ZERO, NON-TRADITIONAL?               
         LTR   RF,RF               HAVE NON-TRADITIONAL DEMO CAT?               
         BR    RE                  CC = 0 MEANS YES                             
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PACKAGE                                        *         
***********************************************************************         
                                                                                
ARYPKG   LKOUT A,(D,B#SAVED,PKGLINS),ROWWIDTH=L'PKGLINS,               +        
               NROWS=(B#SAVED,PKGNLIN)                                          
                                                                                
PkgPL    LKOUT C,43,PKGLINS,UBIN                                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR AVERAGE BOOKS FORMULA                          *         
***********************************************************************         
                                                                                
ARYAVG   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,                          +        
               ROWID=(AVGBKLEM,AVGBKELQ),ROWWIDTH=(V,AVGBKLN)                   
                                                                                
AvgForm  LKOUT C,44,AVGBOOK,CHAR,LEN=V,ND=Y,FILTROUT=TSTAVG                     
UpgForm  LKOUT C,31,AVGBOOK,CHAR,LEN=V,ND=Y,FILTROUT=TSTUPG                     
                                                                                
         LKOUT E                                                                
                                                                                
TSTAVG   L     R1,LP_AINP                                                       
         CLC   AVGBOOK-AVGBKLEM(3,R1),=C'AVG'                                   
         BR    RE                                                               
                                                                                
TSTUPG   L     R1,LP_AINP                                                       
         CLC   AVGBOOK-AVGBKLEM(3,R1),=C'AVG'                                   
         JE    EXITN                                                            
         CLC   LINUPG,SPACES                                                    
         JNZ   EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR AUTOMATED AVAIL'S UUID                         *         
***********************************************************************         
                                                                                
ARYAAU   LKOUT A,(D,B#BUYREC,BDELEM),EOT=EOR,                          +        
               ROWID=(AAVELEM,AAVCODQ),ROWWIDTH=(V,AAVLEN)                      
                                                                                
AAUUID   LKOUT C,45,AAVUUID,CHAR,LEN=V,ND=Y                                     
                                                                                
         LKOUT E                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUY ERROR MESSAGE DOWNLOAD                     *         
***********************************************************************         
                                                                                
ARYERR   LKOUT A,(R,NXTERR),NROWS=1                                             
                                                                                
Error    LKOUT C,36,(D,B#WORKD,WORK),CHAR,LEN=80                                
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO SEND BUY ERROR MESSAGES                                  *         
***********************************************************************         
                                                                                
NXTERR   CLC   LP_QMAPN,SDBUYDD#                                                
         BNE   NOMORE                                                           
         CLI   ERRIND,0                                                         
         BE    NOMORE                                                           
         ICM   R1,7,AERR           POINT TO ERROR ELEMENT                       
         JZ    *+2                                                              
         USING LW_D,R1                                                          
         CLC   LW_CODE,=AL2(D#UPLERR)                                           
         BNE   NOMORE                                                           
         LA    R0,WORK                                                          
         ST    R0,LP_ADATA         A(ROW)=WORK                                  
         MVC   WORK(80),SPACES                                                  
         SR    RF,RF                                                            
         ICM   RF,3,LW_LN                                                       
         SHI   RF,LW_DATA1+1-LW_D                                               
         BASR  RE,0                                                             
         MVC   WORK(0),LW_DATA1                                                 
         EX    RF,0(RE)                                                         
         ICM   RF,3,LW_LN                                                       
         AR    R1,RF                                                            
         STCM  R1,7,AERR                                                        
         B     MORE                                                             
         DROP  R1                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COST2 DOWNLOAD                                 *         
***********************************************************************         
                                                                                
ARYPC2   LKOUT A,(R,NXTCS2),NROWS=1,ROWNAME=PWRECD                              
                                                                                
PrdCd    LKOUT C,8,PWKPRD,(U,#EDTPRD,$EDTPRD),ND=Y                              
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* READ COST2 RECORDS                                                  *         
***********************************************************************         
                                                                                
NXTCS2   MVC   LP_ADATA,AIO8                                                    
                                                                                
         CLC   LP_QMAPN,SDBUYDD#   ON EXIT FROM $BUY?                           
         JE    NXTCS206            YES, WE DON'T HAVE NORMAL FIELDS             
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NXTCS202                                                         
                                                                                
         CLC   C2PMKT#,PMKT#                                                    
         BE    NOMORE                                                           
         MVC   C2PMKT#,PMKT#                                                    
                                                                                
         MVC   SVIOVALS,IOVALS                                                  
         MVC   C2PRDRNG,BR#RNG                                                  
         XR    RF,RF                                                            
         ICM   RF,7,APRD                                                        
         JZ    NXTCS202                                                         
         CLI   8(RF),FF            POL REQUESTED?                               
         JE    NXTCS202                                                         
         MVC   C2PRDRNG(1),8(RF)   NO, RANGE IS THE PRODUCT ITSELF              
         MVC   C2PRDRNG+1(1),8(RF)                                              
                                                                                
NXTCS202 GOTOR (#NXTREC,ANXTREC),DMCB,CS2KEYT,('B#CS2REC',0),SAVED,0,0          
         JE    NXTCS204                                                         
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         B     EXITY                                                            
                                                                                
NXTCS204 L     R1,IOADDR                                                        
         USING PWRECD,R1                                                        
         TM    PWGNFLG,PWGNBUYQ+PWGNBILQ   BUYS LOCKED?                         
         JZ    NXTCS202                    NO, LOOP BACK, WANT LOCKED           
         B     MORE                                                             
         DROP  R1                                                               
                                                                                
***********************************************************************         
* ON EXIT FROM $BUY PROGRAM                                           *         
***********************************************************************         
                                                                                
NXTCS206 MVC   QPRDA,LINPRD                                                     
         GOTOR (#VALPRD,AVALPRD),DMCB,QPRDA,L'QPRDA,QPRDX                       
                                                                                
         MVC   SVIOVALS,IOVALS                                                  
         LA    R2,IOKEY                                                         
         USING PWRECD,R2           R2=A(COST2 KEY)                              
         XC    PWFKEY,PWFKEY                                                    
         MVI   PWKTYPE,PWKTYPEQ                                                 
         MVI   PWKSTYPE,PWKSTYPQ                                                
         MVC   PWKAGMD,QMEDX                                                    
         MVC   PWKCLT,QCLTX                                                     
         MVC   PWKPRD,QPRDX                                                     
         MVC   PWKEST,PESTNUM                                                   
         MVC   PWKMKT,PMKTNUM                                                   
         DROP  R2                                                               
                                                                                
         L     R1,=A(IORD+IODIR+IO8)   NORMAL WAY IS NO GOOD FOR IO8            
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JE    NXTCS210                                                         
NXTCS208 MVC   IOVALS(IOVALL),SVIOVALS                                          
         B     NOMORE                                                           
                                                                                
NXTCS210 L     R1,=A(IOGET+IOFIL+IO8)                                           
         GOTOR (#IOEXEC,AIOEXEC)                                                
         JNE   NXTCS208                                                         
                                                                                
         L     R1,IOADDR                                                        
         USING PWRECD,R1                                                        
         TM    PWGNFLG,PWGNBUYQ+PWGNBILQ   BUYS LOCKED?                         
         JZ    NXTCS208                    NO, LOOP BACK, WANT LOCKED           
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         B     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BWS UPLOAD ACTIVITY                            *         
***********************************************************************         
                                                                                
ARYBWS   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(BWSELEM,BWSCODEQ),ROWWIDTH=(V,BWSLEN)                     
                                                                                
BWByr    LKOUT C,1,BWSBYR,CHAR,ND=Y                                             
BWCam    LKOUT C,2,BWSCAM,LBIN,ND=Y                                             
BWDat    LKOUT C,3,BWSDATE,BDAT,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR AMFM/DARE TRACE ELEMENT                        *         
***********************************************************************         
                                                                                
ARYBTR   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(BTRCELEM,BTRCCODQ),ROWWIDTH=(V,BTRCLEN)                   
                                                                                
BTSeq    LKOUT C,1,BTRCSEQN,LBIN,FILTROUT=TSTSEQN,ND=Y                          
BTSeq    LKOUT C,1,BTRCSEQN,HEXD,FILTROUT=TSTSEQH,ND=Y                          
BTDat    LKOUT C,2,BTRCDATE,BDAT,ND=Y                                           
BTTim    LKOUT C,3,BTRCTIME,HEXD,ND=Y                                           
BTTrm    LKOUT C,4,BTRCLUID,CHAR,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
TSTSEQN  L     R1,LP_AINP          SET CC=EQUAL IF SEQUENCE NUMBER              
         OC    BTRCSEQN-BTRCELEM(2,R1),BTRCSEQN-BTRCELEM(R1)                    
         BR    RE                                                               
                                                                                
TSTSEQH  L     R1,LP_AINP          SET CC=EQUAL IF HEX SEQUENCE CODE            
         OC    BTRCSEQN-BTRCELEM(2,R1),BTRCSEQN-BTRCELEM(R1)                    
         BZ    RTRNNO                                                           
         B     RTRNYES                                                          
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUY ACTIVITY                                   *         
***********************************************************************         
                                                                                
ARYACT   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(ACTVELEM,ACTVELQ),ROWWIDTH=(V,ACTVELEM+1)                 
                                                                                
AdWho    LKOUT C,1,ACTVADD,(R,EDTWHO),ND=Y                                      
AdDat    LKOUT C,2,ACTVADD+L'ACTVADD,BDAT,LEN=3,ND=Y                            
ChWho    LKOUT C,3,ACTVCHG,(R,EDTWHO),ND=Y                                      
ChDat    LKOUT C,4,ACTVCHG+L'ACTVCHG,BDAT,LEN=3,ND=Y                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DARE TRACE ELEMENT                             *         
***********************************************************************         
                                                                                
ARYBDA   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(BDARELEM,BDARCODQ),ROWWIDTH=(V,BDARLEN)                   
                                                                                
CDate    LKOUT C,1,BDARDATE,BDAT,ND=Y                                           
CTime    LKOUT C,2,BDARTIME,HEXD,ND=Y                                           
Order    LKOUT C,3,BDARORD,CHAR,ND=Y                                            
Buyer    LKOUT C,4,BDARBYR,CHAR,ND=Y                                            
MGGrp    LKOUT C,5,BDARMKGP,CHAR,ND=Y                                           
FltNo    LKOUT C,6,BDARFLT,LBIN,ND=Y                                            
SeqNo    LKOUT C,7,BDARSEQN,HEXD,ND=Y                                           
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUY MARKET FIX ACTIVITY                        *         
***********************************************************************         
                                                                                
ARYMFX   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(MFXELEM,MFXCODEQ),ROWWIDTH=(V,MFXLEN)                     
                                                                                
MktNo    LKOUT C,1,MFXMKT,LBIN,ND=Y                                             
ChDat    LKOUT C,2,MFXDATE,CDAT,ND=Y                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUY STATION CALL LETTER CHANGE ACTIVTY         *         
***********************************************************************         
                                                                                
ARYSFX   LKOUT A,(D,B#BUYREC,BDELEM),NEWEL=Y,EOT=EOR,                  +        
               ROWID=(SFXELEM,SFXCODEQ),ROWWIDTH=(V,SFXLEN)                     
                                                                                
SCall    LKOUT C,1,SFXSTA,(R,EDTSTA),ND=Y                                       
ChDat    LKOUT C,2,SFXDATE,CDAT,ND=Y                                            
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* CLIENT DOWNLOAD REQUEST - X'0082'                                             
***********************************************************************         
                                                                                
REQSDC   LKREQ H,I#SDCLTD,OUTSDC,NEXTREQ=REQSDBE                                
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),             +        
               OLEN=L'DCKCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                   
XCltLs   LKREQ F,3,(D,B#SAVED,XCLTLS),HDRO,TEXT=SP#XCLTL,COL=*                  
GetPrf   LKREQ F,4,(D,B#SAVED,GETPRF),HDRO,TEXT=SP#GTPRF,COL=*                  
         LKREQ E                                                                
                                                                                
OUTSDC   LKOUT H                   ** CLIENT DOWNLOAD **                        
         LKOUT R,X'0020'                                                        
Array    LKOUT C,X'0020',(A,ARYSDC)                                             
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CLIENT DOWNLOAD                                *         
***********************************************************************         
                                                                                
ARYSDC   LKOUT A,(R,NXTCLT),MULTIROW=Y,ROWNAME=CLTRECD                          
                                                                                
CltCd    LKOUT C,1,(D,B#WORKD,QCLTA),CHAR                                       
CltNm    LKOUT C,2,CNAME,CHAR                                                   
Daily    LKOUT C,3,CDAILY,CHAR,ND=Y                                             
*                                                                               
         LKOUT C,5,(D,B#SAVED,PROF00V+00),CHAR,LEN=1,FILTROUT=TSTACLT, +        
               SKIPCOLS=15                                                      
         LKOUT C,5,(D,B#SAVED,PROF00V+01),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+02),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+03),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+04),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+05),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+06),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+07),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#SAVED,PROF00V+08),UBIN,LEN=1,SENDCHAR=Y                 
         LKOUT C,5,(D,B#TWAD,SVS002DP),CHAR,LEN=1                               
         LKOUT C,5,(D,B#SAVED,PROF00V+10),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+11),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+12),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+13),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+14),CHAR,LEN=1                            
         LKOUT C,5,(D,B#SAVED,PROF00V+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,6,(D,B#SAVED,PROF1WV+00),CHAR,LEN=1,FILTROUT=TSTACLT, +        
               SKIPCOLS=15                                                      
         LKOUT C,6,(D,B#SAVED,PROF1WV+01),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+02),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+03),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+04),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+05),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+06),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+07),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+08),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+09),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+10),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+11),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+12),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+13),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+14),CHAR,LEN=1                            
         LKOUT C,6,(D,B#SAVED,PROF1WV+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,7,(D,B#SAVED,PROFB0V+00),CHAR,LEN=1,FILTROUT=TSTACLT, +        
               SKIPCOLS=15                                                      
         LKOUT C,7,(D,B#SAVED,PROFB0V+01),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+02),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+03),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+04),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+05),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+06),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+07),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+08),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+09),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+10),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+11),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+12),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+13),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+14),CHAR,LEN=1                            
         LKOUT C,7,(D,B#SAVED,PROFB0V+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,8,(D,B#SAVED,PROFSTV+00),CHAR,LEN=1,FILTROUT=TSTACLT, +        
               SKIPCOLS=15                                                      
         LKOUT C,8,(D,B#SAVED,PROFSTV+01),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+02),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+03),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+04),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+05),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+06),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+07),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+08),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+09),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+10),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+11),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+12),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+13),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+14),CHAR,LEN=1                            
         LKOUT C,8,(D,B#SAVED,PROFSTV+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,9,(D,B#SAVED,PROFAJV+00),CHAR,LEN=1,FILTROUT=TSTACLT, +        
               SKIPCOLS=15                                                      
         LKOUT C,9,(D,B#SAVED,PROFAJV+01),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+02),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+03),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+04),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+05),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+06),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+07),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+08),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+09),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+10),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+11),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+12),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+13),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+14),CHAR,LEN=1                            
         LKOUT C,9,(D,B#SAVED,PROFAJV+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,18,(D,B#SAVED,PROFDARV+00),CHAR,LEN=1,                +        
               FILTROUT=TSTACLT,SKIPCOLS=15                                     
         LKOUT C,18,(D,B#SAVED,PROFDARV+01),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+02),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+03),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+04),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+05),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+06),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+07),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+08),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+09),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+10),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+11),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+12),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+13),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+14),CHAR,LEN=1                          
         LKOUT C,18,(D,B#SAVED,PROFDARV+15),CHAR,LEN=1                          
*                                                                               
         LKOUT C,19,(D,B#SAVED,PROF00A+00),CHAR,LEN=1,                 +        
               FILTROUT=TSTACLT,SKIPCOLS=15                                     
         LKOUT C,19,(D,B#SAVED,PROF00A+01),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+02),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+03),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+04),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+05),CHAR,LEN=1                           
         LKOUT C,19,(D,B#TWAD,SVS00A2DI),CHAR,LEN=1                             
         LKOUT C,19,(D,B#SAVED,PROF00A+07),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+08),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+09),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+10),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+11),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+12),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+13),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+14),CHAR,LEN=1                           
         LKOUT C,19,(D,B#SAVED,PROF00A+15),CHAR,LEN=1                           
*                                                                               
         LKOUT C,20,(D,B#SAVED,PROFBW+00),CHAR,LEN=1,                  +        
               FILTROUT=TSTACLT,SKIPCOLS=15                                     
         LKOUT C,20,(D,B#SAVED,PROFBW+01),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+02),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+03),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+04),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+05),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+06),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+07),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+08),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+09),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+10),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+11),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+12),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+13),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+14),CHAR,LEN=1                            
         LKOUT C,20,(D,B#SAVED,PROFBW+15),CHAR,LEN=1                            
*                                                                               
         LKOUT C,21,(D,B#SAVED,PROFBWA+00),CHAR,LEN=1,                 +        
               FILTROUT=TSTACLT,SKIPCOLS=15                                     
         LKOUT C,21,(D,B#SAVED,PROFBWA+01),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+02),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+03),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+04),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+05),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+06),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+07),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+08),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+09),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+10),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+11),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+12),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+13),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+14),CHAR,LEN=1                           
         LKOUT C,21,(D,B#SAVED,PROFBWA+15),CHAR,LEN=1                           
*                                                                               
         LKOUT C,22,(D,B#SAVED,PROFOM+00),CHAR,LEN=1,                  +        
               FILTROUT=TSTACLT,SKIPCOLS=15                                     
         LKOUT C,22,(D,B#SAVED,PROFOM+01),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+02),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+03),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+04),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+05),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+06),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+07),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+08),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+09),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+10),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+11),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+12),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+13),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+14),CHAR,LEN=1                            
         LKOUT C,22,(D,B#SAVED,PROFOM+15),CHAR,LEN=1                            
                                                                                
         LKOUT E                                                                
                                                                                
TSTACLT  OC    ACLT,ACLT           DO WE HAVE A CLIENT INPUT?                   
         JNZ   SETCCC              YES, WE CAN GET PROFILES                     
         CLI   GETPRF,C'Y'         NO, STILL WANT TO GET PROFILES?              
         JE    EXITY                   yes                                      
         J     EXITN                   NO, NEED A CLIENT TO GET PROFs           
***********************************************************************         
* GET CLIENT RECORDS FOR CLIENT DOWNLOAD                              *         
***********************************************************************         
                                                                                
NXTCLT   DS    0H                                                               
         L     R5,ALP                                                           
         L     RF,LP_ALPXD         LET'S SEE IF OPTICA IS CALLING               
         USING LP_XD,RF                                                         
         LA    RE,LP_XPINF                                                      
         DROP  RF                                                               
*                                                                               
         USING TXPINFO,RE             FROM FAUTL                                
         CLC   TXPNUM,=AL2(XPOPTICQ)  OPTICA CALLING FOR A CLIENT LIST?         
         JNE   NXTCLTNX               NO                                        
         DROP  RE                                                               
*                                  ANYTHING IN TWAACCS? (TWA+6)                 
         L     RF,LP_ATWA                                                       
         OC    TWAACCS-TWAD(L'TWAACCS,RF),TWAACCS-TWAD(RF)                      
         JNZ   NXTCLTNX                                                         
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NOMORE              NO, ONLY SEND ALL/ALL FIRST TIME             
*                                                                               
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         XC    0(255,R2),0(R2)                                                  
         MVC   QCLTA,=C'ALL'          DO WE NEED THE BINARY AGY/MED?            
         MVC   CKEYCLT,=X'816B'       CLPACK for C'ALL'                         
         MVC   CNAME(3),=C'ALL'                                                 
         ST    R2,LP_ADATA                                                      
         J     EXITY                                                            
*                                                                               
NXTCLTNX DS    0H                                                               
         XR    RE,RE                  DID WE GET A CLIENT INPUT                 
         ICM   RE,7,ACLT                                                        
         JNZ   NXTCLT10               YES, WE HAVE A SPECIFIC CLIENT            
*                                                                               
         CLI   XCLTLS,C'Y'            NO, EXCLUDE CLIENT LIST?                  
         JNE   NXCLT020                   NO, USER WANT IT                      
         CLI   GETPRF,C'Y'                YES, WANT PROFILES?                   
         JNE   NOMORE                          NO, NO MORE DATA THEN            
*                                                                               
*****    GET SOME PROFILES NOW                                                  
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME?                             
         JNE   NOMORE              NO, NOTHING LEFT TO DO                       
*                                                                               
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         XC    0(255,R2),0(R2)                                                  
         MVC   QCLTA,=C'ALL'          don't need BINARY AGY/MED?                
         MVC   CKEYCLT,=X'816B'       CLPACK for C'ALL'                         
         MVC   CNAME(3),=C'ALL'                                                 
         ST    R2,LP_ADATA                                                      
         DROP  R2                                                               
*                                                                               
         XC    WORK,WORK           BUILD PROFILE KEY & GET PROFILES             
         MVC   WORK+04(L'AGENCY),AGENCY                                         
         ICM   RF,7,AMED           SET EBCDIC MEDIA                             
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
         MVC   WORK+06(L'QMEDA),QMEDA                                           
*                                                                               
         L     RF,VGETPROF                                                      
         LA    R2,PROFKSDB         R2=A(PROGRAM KEY LIST)                       
         LA    R3,PROFV            R3=A(PROFILES VALUES)                        
         LHI   R0,PROFNSDB         R0=NUMBER OF PROFILES TO LOOK UP             
NXCLT010 MVC   WORK(L'PROFKSDB),0(R2)                                           
         GOTOR (RF),DMCB,WORK,(R3),VDATAMGR                                     
         AHI   R2,L'PROFKSDB                                                    
         AHI   R3,L'PROFV                                                       
         JCT   R0,NXCLT010                                                      
*                                                                               
         XC    WORK,WORK           G0 PROFILE IS SPECIAL                        
         XC    PROFG0V,PROFG0V                                                  
         MVC   WORK(4),=C'S0G0'                                                 
         MVC   WORK+4(L'AGENCY),AGENCY                                          
         MVC   WORK+6(4),=X'FFFFFFFF'                                           
         GOTOR (RF),DMCB,WORK,PROFG0V,VDATAMGR                                  
         J     EXITY                                                            
*                                                                               
*****    GET SOME PROFILES NOW                                                  
*                                                                               
NXCLT020 GOTOR (#NXTREC,ANXTREC),DMCB,CLTKEYT,('B#CLTREC',0),SAVED,0,  +        
               ('#LIMCLT',ALIMCLT)                                              
         BNE   EXITY                                                            
*                                                                               
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         OC    CPLDATA(CPLDATAL),CPLDATA                                        
         JZ    NXTCLTNX                                                         
         CLC   LP_QMAPN,SDCLTD#    TEST CLIENT DOWNLOAD                         
         BE    EXITY                                                            
*                                                                               
         MVC   SVIOVALS,IOVALS                                                  
         USING EKEY,IOKEY          TEST ANY POL ESTIMATES EXIST                 
         XC    EKEY,EKEY                                                        
         MVC   EKEY,CKEY                                                        
         MVC   EKEYPRD,POLPRD                                                   
         MVI   EKEYEST,1                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR'                                   
         JNE   *+2                                                              
         CLC   EKEY(EKEYEST-EKEY),IOKEYSAV                                      
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         JNE   NXTCLTNX                                                         
         GOTOR (#EDTMED,AEDTMED),DMCB,CKEYAM,,MEDCOD                            
         JNE   *+2                                                              
         CLC   PMEDCOD,MEDCOD                                                   
         MVC   PMEDCOD,MEDCOD                                                   
         JNE   *+8                                                              
         MVI   MEDCOD,0            ONLY SEND MEDIA CODE ONCE                    
         B     EXITY                                                            
*                                                                               
NXTCLT10 GOTOR (#NXTREC,ANXTREC),DMCB,CLTKYTB,('B#CLTREC',0),SAVED,0,  +        
               ('#LIMCLT',ALIMCLT)                                              
         BNE   EXITY                                                            
         ICM   RF,7,AMED           SET EBCDIC MEDIA                             
         MVC   QMEDX,LW_DATA1-LW_D(RF)                                          
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,,QMEDA                              
*                                                                               
         L     R2,ACLTREC                                                       
         USING CLTRECD,R2                                                       
         XC    WORK,WORK           BUILD PROFILE KEY & GET PROFILES             
         MVC   WORK+04(L'AGENCY),AGENCY                                         
         MVC   WORK+06(L'QMEDA),QMEDA                                           
         MVC   WORK+07(L'QCLTA),QCLTA                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(L'COFFICE),COFFICE                                       
*                                                                               
         L     RF,VGETPROF                                                      
         LA    R2,PROFKSDB         R2=A(PROGRAM KEY LIST)                       
         LA    R3,PROFV            R3=A(PROFILES VALUES)                        
         LHI   R0,PROFNSDB         R0=NUMBER OF PROFILES TO LOOK UP             
NXTCLT20 MVC   WORK(L'PROFKSDB),0(R2)                                           
         GOTOR (RF),DMCB,WORK,(R3),VDATAMGR                                     
         AHI   R2,L'PROFKSDB                                                    
         AHI   R3,L'PROFV                                                       
         JCT   R0,NXTCLT20                                                      
*                                                                               
         XC    WORK,WORK           G0 PROFILE IS SPECIAL                        
         XC    PROFG0V,PROFG0V                                                  
         MVC   WORK(4),=C'S0G0'                                                 
         MVC   WORK+4(L'AGENCY),AGENCY                                          
         MVC   WORK+6(4),=X'FFFFFFFF'                                           
         GOTOR (RF),DMCB,WORK,PROFG0V,VDATAMGR                                  
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRODUCT/ESTIMATE DOWNLOAD REQUEST                                   *         
***********************************************************************         
                                                                                
REQSDBE  LKREQ H,I#SDBRED,OUTSDE,NEXTREQ=REQUESTX                               
Media    LKREQ F,1,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),             +        
               OLEN=L'CKEYAM,MAXLEN=L'QMEDA,TEXT=SP#MED,COL=*                   
CltCd    LKREQ F,2,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),             +        
               OLEN=L'DCKCLT,MAXLEN=L'QCLTA,TEXT=SP#CLI,COL=*                   
PrdCd    LKREQ F,3,(I,B#SAVED,PRDIND2),(U,#VALPRD,$VALPRD),DEFAULT=NOT,+        
               OLEN=L'PKEYPRD,MAXLEN=L'PKEYPRD,VPARM=C'C',TEXT=SP#PRO, +        
               COL=*                                                            
EstNo    LKREQ F,4,(I,B#SAVED,ESTIND),LBIN,LIST=NOD,RANGE=Y,           +        
               DEFAULT=NOT,OLEN=L'DCKEST,TEXT=SP#EST,COL=*                      
AllPrds  LKREQ F,10,(D,B#SAVED,ALLPRDS),CHAR,TEXT=(*,DOPTLIT)                   
         LKREQ E                                                                
                                                                                
DOPTLIT  DC    C'Download Option'                                               
                                                                                
OUTSDE   LKOUT H                   ** PRODUCT/EST DOWNLOAD **                   
         LKOUT R,X'0022'                                                        
Array    LKOUT C,X'0022',(A,ARYPRD)                                             
         LKOUT X                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PRODUCT DOWNLOAD                               *         
***********************************************************************         
                                                                                
ARYPRD   LKOUT A,(R,NXTPRD),MULTIROW=Y,ROWNAME=PRDHDR                           
                                                                                
PrdCd    LKOUT C,1,PKEYPRD,CHAR                                                 
PrdNm    LKOUT C,2,PNAME,CHAR                                                   
Array    LKOUT C,X'0046',(A,ARYEST)                                             
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* GET NEXT PRODUCT RECORD FOR PRODUCT/ESTIMATE DOWNLOAD               *         
***********************************************************************         
                                                                                
NXTPRD   CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   *+12                                                             
         MVI   ESTFOUND,0          SET NO ESTIMATES FOUND                       
         MVI   POLECTRL,0                                                       
                                                                                
         CLC   LP_QMAPN,SDBUYD#    X'0140' -  BUY/GOAL DOWNLOAD?                
         JE    NXTPRD04                                                         
         CLC   LP_QMAPN,SDNBUYD#   X'014A' -  NEW BUY/GOAL DOWNLOAD?            
         JE    NXTPRD04                                                         
                                                                                
NXTPRD02 GOTOR (#NXTREC,ANXTREC),DMCB,PRDKEYT,('B#PRDREC',0),SAVED,    +        
               AFLTSDB,0                                                        
         B     EXITY                                                            
                                                                                
NXTPRD04 OC    LP_VRSN,LP_VRSN     CHECK PC VERSION                             
         JZ    *+14                                                             
         CLC   LP_VRSN,V250013                                                  
         JL    NXTPRD02                                                         
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXTPRD06                                                         
                                                                                
         ICM   RE,7,APRD2                                                       
W        USING PKEY,IOKEY                                                       
         XC    W.PKEY,W.PKEY                                                    
         ICM   RE,7,AMED                                                        
         MVC   W.PKEYAM,LW_DATA1-LW_D(RE)                                       
         ICM   RE,7,ACLT                                                        
         MVC   W.PKEYCLT,LW_DATA1-LW_D(RE)                                      
         MVC   W.PKEYPRD,POLPRD                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO3'                            
         JNE   NXTPRD08                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
         JNE   NXTPRD08                                                         
         MVC   LP_ADATA,AESTREC    SEND POL PRODUCT RECORD                      
         B     EXITY                                                            
                                                                                
NXTPRD06 CLC   W.PKEYPRD,POLPRD    TEST LAST PRODUCT WAS POL                    
         JNE   NXTPRD08                                                         
         ICM   RE,7,APRD2                                                       
         CLC   POLPRD,LW_DATA1-LW_D(RE)                                         
         JE    NXTPRD12            NO MORE TO DO IF POL REQUEST                 
         XC    IOKEY,IOKEY         ELSE CLEAR KEY FOR NXTREC                    
         CLC   =X'414040',LW_DATA1-LW_D(RE)   NO PRODUCT                        
         JE    NXTPRD08                                                         
         MVI   ESTFOUND,0          SET BRAND ESTIMATE NOT FOUND                 
                                                                                
NXTPRD08 GOTOR (#NXTREC,ANXTREC),DMCB,PRDKY2T,('B#ESTREC',0),SAVED,    +        
               0,0                                                              
         JNE   NXTPRD10                                                         
         CLC   W.PKEYPRD,POLPRD    TEST POL PRODUCT                             
         JE    NXTPRD08            YES - DROP IT                                
*                                                                               
         MVC   SVIOVALS,IOVALS     SAVE SO WE DON'T LOSE OUR PLACE              
         XC    W.PKEY,W.PKEY                                                    
         ICM   RE,7,AMED                                                        
         MVC   W.PKEYAM,LW_DATA1-LW_D(RE)                                       
         ICM   RE,7,ACLT                                                        
         MVC   W.PKEYCLT,LW_DATA1-LW_D(RE)                                      
         L     RE,IOADDR                                                        
         USING ESTHDR,RE           R2=A(ESTIMATE RECORD)                        
         MVC   W.PKEYPRD,EKEYPRD                                                
         DROP  RE                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO3'                            
         JNE   NXTPRD08                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
         JNE   NXTPRD08                                                         
         MVC   LP_ADATA,AESTREC    SEND PRODUCT RECORD                          
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         B     EXITY                                                            
                                                                                
NXTPRD10 CLI   ESTFOUND,0          TEST ANY ESTIMATES FOUND                     
         BNE   EXITY                                                            
         MVC   SVERROR,=AL2(502)   RETURN ERROR                                 
         B     EXITN                                                            
                                                                                
NXTPRD12 CLI   ESTFOUND,0          TEST ANY ESTIMATES FOUND                     
         BNE   NOMORE                                                           
         MVC   SVERROR,=AL2(502)   RETURN ERROR                                 
         B     EXITN                                                            
         DROP  W                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PRODUCT/ESTIMATE ESTIMATE DOWNLOAD             *         
***********************************************************************         
                                                                                
ARYEST   LKOUT A,(R,NXTEST),MULTIROW=Y,ROWNAME=ESTHDR                           
                                                                                
EstNo    LKOUT C,1,EKEYEST,UBIN                                                 
EDesc    LKOUT C,2,EDESC,CHAR                                                   
SDate    LKOUT C,3,ESTART,EDAT                                                  
Edate    LKOUT C,4,EEND,EDAT                                                    
Array    LKOUT C,5,(A,ARYEDC)                                                   
OOWSD    LKOUT C,6,EOWSDAY,UBIN,ND=Y                                            
Daily    LKOUT C,7,EDAILY,HDRO,ND=Y                                             
DayPt    LKOUT C,9,EDAYMENU,CHAR,ND=Y                                           
RCtrl    LKOUT C,10,ERATE,CHAR,ND=Y                                             
EstIn    LKOUT C,11,(D,B#SAVED,ESTINDS),HEXD,ND=Y                               
FrzMo    LKOUT C,12,(D,B#SAVED,ESTFRZMO),BMON,ND=Y                              
RBook    LKOUT C,13,EBOOK,BMON,ND=Y                                             
RepCd    LKOUT C,14,(D,B#SAVED,ESTREP),CHAR,ND=Y                                
BType    LKOUT C,15,EBKTYPE,(R,TRNSBKT),ND=Y                                    
C2Fac    LKOUT C,16,(D,B#SAVED,ESTC2FAC),SPAK,ND=Y                              
ECntl    LKOUT C,17,ECNTRL,HEXD,ND=Y                                            
PRout    LKOUT P,EFLAG1,SETTEST                                                 
Trade    LKOUT C,18,(D,B#WORKD,BYTE),HDRO,ND=Y,PCVERSION=3.0.0.120              
comBT    LKOUT C,19,ECSBKTYP,CHAR,ND=Y,PCVERSION=4.6.0.50                       
comSD    LKOUT C,20,ECSSDTE,CHAR,ND=Y,PCVERSION=4.6.0.50                        
                                                                                
         LKOUT E                                                                
                                                                                
SETTEST  L     R1,LP_AINP          SET BYTE TO 'Y' IF TRADE ESTIMATE            
         MVI   BYTE,0                                                           
         TM    0(R1),EF1TRADE                                                   
         BZ    EXIT                                                             
         MVI   BYTE,YESQ                                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* GET ESTIMATE RECORDS FOR PRODUCT FOR PRODUCT/ESTIMATE DOWNLOAD      *         
***********************************************************************         
                                                                                
NXTEST   GOTOR (#NXTREC,ANXTREC),DMCB,SDEKEYT,('B#ESTREC',SVPOLKEY),   +        
               SAVED,0,0                                                        
         BNE   EXITY                                                            
         L     R2,IOADDR                                                        
         USING ESTHDR,R2           R2=A(ESTIMATE RECORD)                        
         CLC   EKEYPRD,=C'POL'     ARE WE ON THE POL ESTIMATE?                  
         JNE   *+14                                                             
         MVC   POLECTRL,ECNTRL     YES, SAVE OFF CONTROL STATUS BITS            
         J     *+10                                                             
         OC    ECNTRL,POLECTRL     NO, USE WHAT WE GOT FROM POL EST             
*                                                                               
         XC    ESTV(ESTVL),ESTV                                                 
         MVC   ESTFRZMO,ELOCKYM                                                 
         NI    ESTFRZMO+1,FF-X'C0'                                              
         TM    ELOCKMON,X'80'                                                   
         JZ    *+8                                                              
         OI    EST#FPRI,ESTIFPRI                                                
         TM    ELOCKMON,X'40'                                                   
         JZ    *+8                                                              
         OI    EST#FSUB,ESTIFSUB                                                
         CLI   EDAILY,C'Y'                                                      
         JE    *+8                                                              
         MVI   EDAILY,0                                                         
         CLI   DPTCODE,0                                                        
         JNE   *+10                                                             
         MVC   DPTCODE,EDAYMENU                                                 
         OC    EREP,EREP           SET REP CODE IF PRESENT                      
         JZ    NXTEST02                                                         
         GOTOR VRCPACK,DMCB,(C'U',EREP),ESTREP                                  
                                                                                
NXTEST02 OC    ECOST2,ECOST2       TEST COST2 DEFINED                           
         JZ    NXTEST04                                                         
         ZAP   ESTC2FAC,PZERO                                                   
         TM    ECOST2,X'80'        TEST INPUT BUT ZERO                          
         JNZ   NXTEST04                                                         
         ICM   R0,15,ECOST2                                                     
         CVD   R0,DUB                                                           
         ZAP   ESTC2FAC,DUB                                                     
                                                                                
NXTEST04 OC    ADEM,ADEM           WE ALREADY HAVE ADEM SET FROM REQ??          
         JNZ   NXTEST10            YES, LEAVE IT ALONE THEN                     
                                                                                
         CLC   EKEYPRD,=C'POL'     ARE WE ON THE POL ESTIMATE?                  
         JNE   NXTEST05            NO                                           
         ICM   RE,7,APRD2          DID USER REQUEST POL PRODUCT?                
         CLC   POLPRD,LW_DATA1-LW_D(RE)                                         
         JE    NXTEST05                                                         
         CLC   =X'414040',LW_DATA1-LW_D(RE)   OR NO PRODUCT?                    
         JNE   NXTEST10              NO, SKIP DEMO LIST COPY                    
                                                                                
NXTEST05 L     RF,LP_AWMP          USE WORK MAP POOL TO STORE THE DEMO          
         USING LW_D,RF             CATEGORY LIST                                
         OI    LW_TYPE,LW_TLSTQ    LIST OF VALUES                               
         MVC   LW_DATA2(L'EDEMLST),EDEMLST  COPY THE LIST                       
                                                                                
         LA    RE,LW_DATA2                                                      
         LA    R0,LW_DATA2+L'EDEMLST                                            
         SR    R1,R1               R1=NUMBER OF DEMO CATEGORIES                 
NXTEST06 OC    0(3,RE),0(RE)       ANY DEMO CAT?                                
         JZ    NXTEST08                                                         
         AHI   R1,1                BUMP # OF CATEGORIES                         
         AHI   RE,L'EDEMLIST                                                    
         CR    RE,R0                                                            
         JL    NXTEST06                                                         
                                                                                
NXTEST08 STCM  R1,3,LW_NUMN        CALCULATE THE # OF DEMOS                     
         STCM  RF,7,ADEM                                                        
         ST    RE,LP_AWMP                                                       
         SR    RE,RF                                                            
         STCM  RE,3,LW_LN          SET LENGTH OF THIS LIST                      
         DROP  RF                                                               
                                                                                
NXTEST10 OI    ESTFOUND,1          SET ESTIMATE FOUND                           
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ESTIMATE DEMO CODES DOWNLOAD                   *         
***********************************************************************         
                                                                                
ARYEDC   LKOUT A,(D,B#ESTREC,EDEMLST),NROWS=EDEMLSTN,                  +        
               ROWWIDTH=L'EDEMLIST                                              
                                                                                
PRout    LKOUT P,,INITDEMS                                                      
DemCd    LKOUT C,5,EDEMLST,(R,EDTEDCD),LEN=L'EDEMLIST,ND=Y                      
UDemTxt  LKOUT C,8,(D,B#SAVED,ESTNTDEM),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
REQUESTX LKREQ X                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* EDIT OUT DEMO CODE                                                            
***********************************************************************         
EDTGDCD  MVI   SVDEMORG,SVDMOGD    GOAL DEMO, GET NTDEM FROM EST REC            
         J     EDTDCD                                                           
EDTEDCD  MVI   SVDEMORG,SVDMOED    EST DEMO,  GET NTDEM FROM EST REC            
         J     EDTDCD                                                           
EDTBDCD  MVI   SVDEMORG,SVDMOBD    BUY DEMO,  GET NTDEM FROM BUY REC            
         J     EDTDCD                                                           
*                                                                               
EDTDCD   CLI   SVDEMORG,0          MUST HAVE DEMO ORIGIN                        
         JE    *+2                                                              
*                                                                               
         XC    ESTNTDEM,ESTNTDEM   CLEAR THE TEXT                               
         MVI   SVDEMTYP,0                                                       
*                                                                               
         L     R2,LP_AINP                                                       
         USING EDEMLIS1,R2                                                      
         CLC   EDEMLIS1,HZEROS     ANY MORE?                                    
         JE    EXITN                NO                                          
*                                                                               
         LLC   RF,EDEMLNU1                                                      
         ICM   RF,B'0010',EDEMLIS1 BYTE1&3 ZERO, NON-TRADITIONAL?               
         LTR   RF,RF               HAVE NON-TRADITIONAL DEMO CAT?               
         JZ    GTNTDEM              YES                                         
*                                                                               
         MVC   SVDEMTYP,EDEMLTY1   SAVE DEMO TYPE                               
*                                                                               
         CLI   SVDEMORG,SVDMOED    PROCESSING EST REC?                          
         JNE   EDTDCD10             YES                                         
         CLI   EDEMLTY1,X'21'      USER DEFINED DEMO?                           
         JNE   EDTDCD10             NO                                          
         L     RE,IOADDR           GET ESTIMATE USER-DEFINED NAME               
         LA    RE,EUSRNMS-ESTHDR(RE)   RE=A(START OF USER-DEF LIST)             
         LLC   RF,EDEMLNU1         GET USER-DEF INDEX                           
         SHI   RF,1                SUB 1                                        
         MHI   RF,L'EUSRNML        MULTIPLE * LENGTH OF ENTRY                   
         AR    RF,RE               ADD OFFSET TO START OF LIST                  
         MVC   ESTUDEMO,0(RF)      AND OUTPUT                                   
*                                                                               
EDTDCD10 GOTOR (#EDTDCD,AEDTDCD),DMCB,LP_AINP,0,LP_AOUT                         
         MVC   LP_OLEN,DMCB+4      SET OUTPUT LENGTH                            
*                                                                               
* SAVE DEMO TYPE (SVDEMTYP) TO A DEMO LIST (SVDEMTYL)                           
*                                                                               
EDTDCD20 LA    RE,SVDEMTYL                                                      
         LA    RF,SVDEMTYL+L'SVDEMTYL-1                                         
EDTDCD30 CR    RE,RF               TOO MANY FOR LIST                            
         JH    *+2                  YES, DIE                                    
         CLI   0(RE),0             FOUND EMPTY SLOT?                            
         JE    EDTDCD40             YES                                         
         LA    RE,1(RE)             NO, BUMPT TO NEXT SLOT                      
         J     EDTDCD30                                                         
EDTDCD40 MVC   0(1,RE),SVDEMTYP                                                 
         J     EXITY                                                            
***                                                                             
* GET NON-TRADITIONAL DEMO NAME FROM A)EST-REC OR B)BUY NTDEMO ELEM             
* IF < V4.60.50, TRANSFORM NON-TRAD TO USER-DEF DEMO                            
***                                                                             
GTNTDEM  LLC   RF,EDEMLTY1         RF = NON-TRAD DEMO INDEX                     
         OC    LP_VRSN,LP_VRSN     IF NO VERSION                                
         JZ    GTENTDM                                                          
         CLC   LP_VRSN,V460050     OR VERSION 4.6.0.50 AND HIGHER               
         JNL   GTENTDM              GET NON-TRAD NAME                           
         MVI   EDEMLTY1,X'21'      TRANSFORM TO USER DEF                        
         LA    RE,10(RF)           ADD 10 THE INDEX AND                         
         STC   RE,EDEMLNU1         START AT +10 (U11,U12,U13,...,U30)           
*                                                                               
* GET NON-TRADITIONAL DEMO NAME FROM ESTIMATE RECORD                            
*                                                                               
GTENTDM  CLI   SVDEMORG,SVDMOED    IF PROCESSING EST REC OR                     
         JE    GTENTDM2                                                         
         CLI   SVDEMORG,SVDMOGD    GOAL REC?                                    
         JNE   GTBNTDM                                                          
*                                  THEN GET NT DEMO CODE FROM EST REC           
GTENTDM2 L     RE,AESTREC                                                       
         LA    RE,ENONTDMS-ESTHDR(RE)  RE=A(START OF NONTRAD DEMO LIST)         
         BCTR  RF,0                SUB 1 FROM INDEX                             
         MHI   RF,L'ENONTDMS       MULTIPLE * LENGTH OF ENTRY                   
         AR    RF,RE               ADD OFFSET TO START OF LIST                  
         LA    RE,20*L'ENONTDMS(RE)                                             
         CR    RF,RE               BEYOND LIST?                                 
         JNL   *+14                 YES, RETURN ????????                        
         CLC   0(L'NTDDMONM,RF),SPACES  HAVE SHORT NAME?                        
         JH    *+8                       YES                                    
         LA    RF,=C'????????'     NO, RETURN ???????? FOR NAME                 
*                                                                               
         MVC   SVDEMTYP,0(RF)      SAVE DEMO TYPE                               
*                                                                               
         CLI   EDEMLTY1,X'21'      DID WE TRANSFORM TO USER DEF?                
         JNE   GTENTDM5             NO                                          
         MVC   ESTNTDEM,0(RF)       YES, OUTPUT AS USER-DEF                     
         J     EDTDCD10             AND CALL #EDTDCD                            
*                                                                               
GTENTDM5 L     R4,LP_AOUT                                                       
         MVC   0(L'ENONTDMS,R4),0(RF)  SET NON-TRAD DEMO CAT AS OUTPUT          
         LA    RE,8                                                             
         ST    RE,LP_OLEN          AND SET LENGTH                               
         J     EDTDCD20                                                         
*                                                                               
* GET NON-TRADITIONAL DEMO NAME FROM BUY NTDELEM                                
*                                                                               
GTBNTDM  CLI   SVDEMORG,SVDMOBD    GET NT DEMO CODE FROM BUY NTDELEM            
         JNE   *+2                  NO, SOMETHING WRONG                         
*                                                                               
         L     R3,ABUYREC          GET THE SHORT NAME AND FLAG                  
         LA    R3,BDELEM-BUYREC(R3)                                             
GTBNTDM2 CLI   0(R3),EOR           EOR?                                         
         JE    GTBNTDM6             YES, RETURN ????????                        
         CLI   0(R3),NTDELCDQ      X'50' NON-TRAD DEMO NAME ELEM?               
         JE    GTBNTDM4             YES                                         
         LLC   RE,1(R3)             NO, BUMP TO NEXT ELEM                       
         LA    R3,0(RE,R3)                                                      
         J     GTBNTDM2                                                         
*                                                                               
GTBNTDM4 BCTR  RF,0                     DEC BY 1                                
         MHI   RF,L'NTDDMONM+L'NTDDMOFL MULT BY L'ENTRY                         
         LA    RF,NTDOVHDQ(RF)                                                  
         MVI   SCDNTDMF,0                                                       
         CLM   RF,1,1(R3)               BEYOND LIST?                            
         JNL   GTBNTDM6                  YES, RETURN ????????                   
         LA    RF,0(R3,RF)              POINT R3 TO OUR ENTRY                   
         MVC   SCDNTDMF,L'NTDDMONM(RF)  COPY THE FLAGS BITS TO OUTPUT           
         LA    RE,8                     SET RETURN LENGTH                       
         CLC   0(L'NTDDMONM,RF),SPACES  HAVE SHORT NAME?                        
         JH    GTBNTDM8                  YES                                    
GTBNTDM6 LA    RF,=C'????????'           NO, RETURN ???????? FOR NAME           
         LA    RE,4                      AND SET RETURN LENGTH                  
*                                                                               
GTBNTDM8 MVC   SVDEMTYP,0(RF)           SAVE DEMO TYPE                          
*                                                                               
         CLI   EDEMLTY1,X'21'           DID WE TRANSFORM TO USER DEF?           
         JE    EDTDCD10                  YES, CALL #EDTDCD                      
*                                                                               
         L     R4,LP_AOUT                                                       
         MVC   0(L'NTDDMONM,R4),0(RF)   SET THE SHORT FORM NAME                 
         ST    RE,LP_OLEN                                                       
         J     EDTDCD20                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* EDIT DEMO VALUE                                                               
***********************************************************************         
                                                                                
EDTDVLO  MVC   SVDEMTYP,SVDEMTYL                                                
         J     EDTDVL10                                                         
*                                                                               
EDTDVL   ICM   RE,15,ACURDMTP                                                   
         JNZ   EDTDVL05                                                         
         LA    RE,SVDEMTYL                                                      
*                                                                               
EDTDVL05 LA    RF,SVDEMTYL+L'SVDEMTYL-1                                         
         CR    RE,RF               BEYOND DEMO LIST?                            
         JH    *+2                                                              
         MVC   SVDEMTYP,0(RE)                                                   
         LA    RE,1(RE)                                                         
         ST    RE,ACURDMTP                                                      
*                                                                               
EDTDVL10 L     R1,LP_AINP                                                       
         MVC   FULL(4),0(R1)                                                    
         TM    FULL,MODMOVRD                                                    
         JZ    *+8                                                              
         NI    FULL,FF-(MODMOVRD)                                               
         ST    RE,SAVERE                                                        
         GOTOR ADJPRC                                                           
         L     RE,SAVERE                                                        
         ICM   R0,15,FULL                                                       
         L     RF,LP_AOUT                                                       
         CURED (R0),(10,(RF)),0,ALIGN=LEFT,ZERO=NOBLANK                         
         ST    R0,LP_OLEN                                                       
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET THE BRAND ESTIMATE'S PRIMARY DEMO CATEGORY                                
* THIS IS IMPORTANT FOR GOALS BECAUSE THE PRIMARY DEMO ON THE BRAND             
*   ESTIMATE CAN ENTIRELY DIFFERENT WHAT IS THE PRIMARY DEMO ON THE             
*   POL ESTIMATE AND ALL OTHER BRAND ESTIMATES                                  
*                                                                               
* NTRY:- R2=A(GOAL WEEK ELEMENT)                                                
*        IOADDR=A(GOAL RECORD)                                                  
***********************************************************************         
                                                                                
GTESTPDM NTR1  LABEL=NO                                                         
         MVC   SVIOVALS,IOVALS     SAVE CURRENT I/O VALUES                      
*                                                                               
         L     R2,IOADDR                                                        
         USING GOALREC,R2                                                       
         LA    R3,IOKEY                                                         
         USING ESTHDR,R3           READ ESTIMATE TO ESTABLISH PERIOD            
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,GKEYAM                                                    
         MVC   EKEYCLT,GKEYCLT                                                  
         GOTOR (#EDTPRD,AEDTPRD),DMCB,GKEYPRD,0,EKEYPRD                         
         MVC   EKEYEST,GKEYEST                                                  
*                                                                               
         L   R3,AESTREC             DO WE ALREADY HAVE THIS ESTIMATE?           
         IF  (CLC,EKEY,NE,IOKEY)                                                
           GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#ESTREC'  GO GET IT            
           JNE   *+2                                                            
           GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#ESTREC'                      
           JNE   *+2                                                            
         ENDIF                                                                  
*                                                                               
         LA  R1,EDEMLIST            POINT TO THE DEMO LIST                      
         IF  (CLI,EDEMLNUM,NE,0)    IF WE'RE A TRADITIONAL DEMO                 
           MVC  ESTPDTYP,EDEMLTYP   THEN SAVE OFF THE DEMO TYPE                 
         ELSE ,                                                                 
           LLC  RE,EDEMLTYP         FIND THE NON-TRAD DEMO NAME                 
           BCTR RE,0                IN ENONTDMS                                 
           MHI  RE,L'ENONTDMS                                                   
           LA   RE,ENONTDMS(RE)                                                 
**  IF NON-TRAD DEMO NAME IS A RATINGS TYPE                                     
           IF  (CLI,0(RE),EQ,C'R'),OR,(CLI,0(RE),EQ,C'E')                       
             MVC  ESTPDTYP,0(RE)        THEN REMEMBER IT AS SUCH                
           ELSE ,                                                               
             MVI  ESTPDTYP,C'I'         OTHERWISE ASSUME IMPRESSIONS            
           ENDIF                                                                
         ENDIF                                                                  
*                                                                               
GTESTPDX MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* USE CPP GUIDE (IF ANY) TO CALCULATE MISSING GOAL WEEK VALUE         *         
*                                                                     *         
* NTRY:- R2=A(GOAL WEEK ELEMENT)                                      *         
*        IOADDR=A(GOAL RECORD)                                        *         
***********************************************************************         
                                                                                
GETCPP   NTR1  LABEL=NO                                                         
         ST    R2,FULL1            SAVE A(GOAL ELEMENT)                         
         L     R2,IOADDR                                                        
         USING GOALREC,R2          R2=A(GOAL RECORD)                            
         MVC   SVIOVALS,IOVALS     SAVE CURRENT I/O VALUES                      
                                                                                
         L     RF,ACLTREC                                                       
         CLI   CPROF+8-CLTRECD(RF),C'0'                                         
         JNE   GETCPPX                                                          
                                                                                
         MVC   WORK(L'GDCPPES),GDCPPES                                          
         CLI   GDCPPES,0                                                        
         JNE   *+10                                                             
         MVC   WORK(L'GKEYEST),GKEYEST                                          
         CLC   PCPPEST,WORK        TEST ALREADY ESTABLISHED CPP GUIDE           
         JE    GETCPP34                                                         
         MVC   PCPPEST,GKEYEST     NO - BUILD CPP GUIDE                         
                                                                                
         LA    R3,IOKEY                                                         
         USING ESTHDR,R3           READ ESTIMATE TO ESTABLISH PERIOD            
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,GKEYAM                                                    
         MVC   EKEYCLT,GKEYCLT                                                  
         MVC   EKEYPRD,POLPRD                                                   
         MVC   EKEYEST,GKEYEST                                                  
         L     R3,AESTREC                                                       
         CLC   EKEY,IOKEY                                                       
         JE    GETCPP01                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IODIR+B#ESTREC'                         
         JNE   *+2                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOFIL+B#ESTREC'                        
         JNE   *+2                                                              
GETCPP01 MVC   QSTRDATE(L'ESTART+L'EEND),ESTART                                 
         DROP  R3                                                               
                                                                                
         MVC   WORK+00(4),VGETBRD                                               
         MVC   WORK+04(4),VADDAY                                                
         MVC   WORK+08(4),VGETDAY                                               
         MVC   WORK+12(4),VDATCON                                               
         LLC   R0,PROF00V+2        GET PROFILE DATE CONTROL                     
         CHI   R0,6                ONLY ALLOW 6 THROUGH 8                       
         JL    *+12                                                             
         CHI   R0,8                                                             
         JNH   *+6                                                              
         SR    R0,R0                                                            
         GOTOR VMOBILE,DMCB,(12,QSTRDATE),((R0),ESTMOS),WORK,PROF00V            
                                                                                
         L     R4,AIO5                                                          
         USING CPPTABD,R4          R4=A(CPP GUIDE)                              
         MVI   CPPTABD,CPPTEOTQ    SET END OF CPP GUIDE TABLE                   
                                                                                
         LA    R3,IOKEY                                                         
CPP      USING GOALREC,R3                                                       
         MVC   CPP.GKEY,GKEY                                                    
         MVI   CPP.GKEYPRD,POLPRDQ                                              
         MVI   PCPPEST2,0                                                       
         CLI   GDCPPES,0           TEST CPP ESTIMATE OVERRIDE PRESENT           
         JE    GETCPP12                                                         
         MVC   CPP.GKEYCLT,GDCPPCL                                              
         MVC   CPP.GKEYEST,GDCPPES                                              
         MVC   PCPPEST2,GDCPPES2                                                
                                                                                
GETCPP12 XC    CPP.GKEYDPT(GKCNTRLS-GKEYDPT),CPP.GKEYDPT                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO6'                               
         JE    GETCPP16                                                         
         DC    H'0'                                                             
                                                                                
GETCPP14 LA    R3,IOKEY            NEED TO POINT R3 BACK TO IOKEY               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO6'                               
         JNE   *+2                                                              
                                                                                
GETCPP16 CLC   CPP.GKEY(GKEYDPT-GKEY),IOKEYSAV                                  
         JNE   GETCPP32                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO6'                              
         JNE   *+2                                                              
*                                                                               
         L     R3,IOADDR           R3=A(GOAL RECORD)                            
         L     R4,AIO5             R4=A(CPP GUIDE TABLE)                        
         LHI   R0,CPPTMAXD                                                      
GETCPP18 CLI   CPPTABD,CPPTEOTQ    TEST END OF TABLE                            
         JE    GETCPP20                                                         
         CLC   CPPTDPT,CPP.GKEYDPT MATCH TABLE KEY                              
         JE    GETCPP22                                                         
         AHI   R4,CPPTABL                                                       
         JCT   R0,GETCPP18                                                      
         DC    H'0'                TOO MANY DAYPARTS                            
                                                                                
GETCPP20 XC    CPPTABD(CPPTABL),CPPTABD                                         
         MVI   CPPTABD+CPPTABL,CPPTEOTQ                                         
         MVC   CPPTDPT,CPP.GKEYDPT                                              
                                                                                
GETCPP22 LA    R3,CPP.GDELEM                                                    
CPPEL    USING GLEMENT,R3                                                       
         LA    RE,ESTMOS                                                        
         LA    RF,CPPTVALS                                                      
                                                                                
GETCPP24 CLI   CPPEL.GLCODE,EOR    TEST END OF RECORD                           
         JE    GETCPP14                                                         
         CLI   CPPEL.GLCODE,GLCODEQ                                             
         JNE   GETCPP30                                                         
         CLC   CPPEL.GLWEEK,0(RE)  TEST BEFORE REQUEST START DATE               
         JL    GETCPP30                                                         
                                                                                
GETCPP26 CLC   CPPEL.GLWEEK,2(RE)  TEST IN THIS MONTH                           
         JNH   GETCPP28                                                         
         AHI   RE,L'ESTMOS         NO - BUMP TO NEXT MONTH                      
         AHI   RF,L'CPPTVALS                                                    
         CLI   0(RE),FF            TEST END OF REQUEST PERIOD                   
         JNE   GETCPP26                                                         
         J     GETCPP14            YES - GET NEXT RECORD                        
                                                                                
GETCPP28 MVC   0(L'GLBUDGET,RF),CPPEL.GLBUDGET                                  
                                                                                
GETCPP30 LLC   R0,CPPEL.GLEN       BUMP TO NEXT ELEMENT ON RECORD               
         AR    R3,R0                                                            
         J     GETCPP24                                                         
                                                                                
GETCPP32 CLI   PCPPEST2,0          TEST SECOND ESTIMATE PRESENT                 
         JE    GETCPP34                                                         
         LA    R3,IOKEY                                                         
         MVC   CPP.GKEYEST,PCPPEST2                                             
         MVI   PCPPEST2,0                                                       
         J     GETCPP12                                                         
                                                                                
GETCPP34 MVC   DUB(2),CLTEQU30     USE 30 SECOND EQUIVANCE IF NOT FOUND         
         LA    R1,CLTEQU                                                        
         BASR  RE,0                                                             
         CLI   0(R1),0             TEST END OF EQUIVALENCE TABLE                
         JE    GETCPP36                                                         
         CLC   GKEYSEC,0(R1)       MATCH SECOND LENGTH TO TABLE                 
         JE    *+10                                                             
         AHI   R1,L'CLTEQU                                                      
         BR    RE                                                               
         MVC   DUB(2),1(R1)        SAVE SPOT LENGTH EQUIVALENCE FACTOR          
                                                                                
GETCPP36 L     R4,AIO5             R4=A(CPP GUIDE)                              
GETCPP38 CLI   CPPTABD,CPPTEOTQ    TEST END OF TABLE                            
         JE    GETCPPX                                                          
         CLC   CPPTDPT,GKEYDPT     MATCH GOAL DAYPART TO TABLE                  
         JE    *+12                                                             
         AHI   R4,CPPTABL                                                       
         J     GETCPP38                                                         
                                                                                
         L     R2,FULL1            R2=A(GOAL WEEK ELEMENT)                      
         USING GLEMENT,R2                                                       
         LA    RE,ESTMOS           LOCATE CORRECT MONTH FOR GOAL WEEK           
         LA    RF,CPPTVALS                                                      
GETCPP40 CLC   GLWEEK,0(RE)                                                     
         JL    *+14                                                             
         CLC   GLWEEK,2(RE)                                                     
         JNH   GETCPP42                                                         
         AHI   RE,L'ESTMOS                                                      
         AHI   RF,L'CPPTVALS                                                    
         CLI   0(RE),FF                                                         
         JNE   GETCPP40                                                         
         J     GETCPPX                                                          
                                                                                
GETCPP42 OC    GLBUDGET,GLBUDGET   TEST ZERO DOLLARS                            
         JNZ   GETCPP44            NO, WE NEED THE POINTS                       
*                                                                               
*                                  DOLLARS = POINTS * CPP GUIDE                 
         ICM   R1,15,GLGRP         R1 = POINTS                                  
         CVD   R1,PACKOF8B                                                      
         IF  (TM,GLGRP,X'40',O)    POINTS ARE TO 2 DECIMAL?                     
           N  R1,=X'3FFFFFFF'      YES, REMOVE THAT INDICATOR BIT               
           CVD  R1,PACKOF8B                                                     
         ELSE                                                                   
           SRP  PACKOF8B,1,0       X 10 SO POINTS ARE TO 2 DECIMAL              
         ENDIF ,                                                                
         ZAP   PACK16B1,PACKOF8B                                                
*                                                                               
         ICM   R1,15,0(RF)         X CPP GUIDE                                  
         CVD   R1,PACKOF8B                                                      
         MP    PACK16B1,PACKOF8B                                                
*                                                                               
         XR    R1,R1               X SPOT LEN EQUIV                             
         ICM   R1,3,DUB                                                         
         CVD   R1,PACKOF8B                                                      
         MP    PACK16B1,PACKOF8B                                                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CLTEQU30                                                    
         CVD   RF,PACKOF8B                                                      
         SRP   PACKOF8B,4,0        X 10000                                      
*                                                                               
         LA    RF,PACKOF8B         DIVISOR IS IN PACKOF8B                       
         LA    RE,8                AND CAN BE A LENGTH OF 8                     
         DO  WHILE=(CLI,0(RF),EQ,0)                                             
           LA   RF,1(RF)             IF NULL IN HOB OF THE DIVISOR              
           BCTR RE,0                 SHORTEN DIVISOR BY 1 BYTE                  
         ENDDO ,                                                                
*                                                                               
         BCTR  RE,0                SETUP DIVISOR LENGTH FOR EX INSTR            
         LAY   R1,GETCPPD1         / 30 SEC FACTOR                              
         EX    RE,0(R1)                                                         
*                                                                               
         LA    RE,1(RE)            LOCATE THE QUOTIENT                          
         LNR   RE,RE                                                            
         LA    RF,L'PACK16B1                                                    
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         LAY   R1,GETCPPZ2                                                      
         EX    RE,0(R1)            WE HAVE THE QUOTIENT IN PACK16B2             
*                                                                               
         ZAP   PACKOF8B,PACK16B2                                                
         CVB   R1,PACKOF8B                                                      
         STCM  R1,15,GLBUDGET                                                   
         J     GETCPPX                                                          
*                                                                               
GETCPPD1 DP    PACK16B1,0(0,RF)                                                 
GETCPPD2 DP    PACK16B2,0(0,RF)                                                 
GETCPPZ1 ZAP   PACK16B1,PACK16B2(0)                                             
GETCPPZ2 ZAP   PACK16B2,PACK16B1(0)                                             
***********************************                                             
* GRP = DOLLARS / CPP GUIDE                                                     
***********************************                                             
GETCPP44 ICM   R1,15,GLBUDGET      GRP =  DOLLARS / CPP GUIDE                   
         CVD   R1,PACKOF8B         CONVERT TO DECIMAL FOR MATHEMATICAL          
         ZAP   PACK16B1,PACKOF8B      CALCULATIONS                              
*                                                                               
         MP    PACK16B1,=P'20000'  NOW WE CAN SUPPORT 2 DECIMAL PLACES          
*                                   SO MULTIPLY BY 20K INSTEAD OF 2K            
         ICM   RF,15,0(RF)                                                      
         JZ    GETCPPX             DIVIDE BY 0??? LEAVE GRP AS 0                
*                                                                               
         CVD   RF,PACKOF8B         WE'RE GOING TO DIVIDE PACK                   
         LA    RF,PACKOF8B         DIVISOR IS IN PACKOF8B                       
         LA    RE,8                AND CAN BE A LENGTH OF 8                     
         DO  WHILE=(CLI,0(RF),EQ,0)                                             
           LA   RF,1(RF)             IF NULL IN HOB OF THE DIVISOR              
           BCTR RE,0                 SHORTEN DIVISOR BY 1 BYTE                  
         ENDDO ,                                                                
* RE SHOULD NOW HAVE NUMBER OF PACK BYTES TO DIVIDE FOR                         
* RF IS A(SHORTEN PACKED NUMBER) AFTER GETTING RID OF LEADING ZEROS             
         BCTR  RE,0                                                             
         LAY   R1,GETCPPD1                                                      
         EX    RE,0(R1)            (DOLLARS * 20000) / CPP GUIDE                
*                                                                               
         LA    RE,1(RE)            LOCATE THE QUOTIENT                          
         LNR   RE,RE                                                            
         LA    RF,L'PACK16B1                                                    
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         LAY   R1,GETCPPZ2                                                      
         EX    RE,0(R1)            WE HAVE THE QUOTIENT IN PACK16B2             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CLTEQU30       X 30 SEC FACTOR                              
         CVD   RF,PACKOF8B                                                      
         MP    PACK16B2,PACKOF8B                                                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,DUB            NEED TO DIVIDE W/ SPOT LEN EQUIV             
         CVD   RF,PACKOF8B                                                      
*                                                                               
         LA    RF,PACKOF8B         DIVISOR IS IN PACKOF8B                       
         LA    RE,8                AND CAN BE A LENGTH OF 8                     
         DO  WHILE=(CLI,0(RF),EQ,0)                                             
           LA   RF,1(RF)             IF NULL IN HOB OF THE DIVISOR              
           BCTR RE,0                 SHORTEN DIVISOR BY 1 BYTE                  
         ENDDO ,                                                                
* RE SHOULD NOW HAVE NUMBER OF PACK BYTES TO DIVIDE FOR                         
* RF IS A(SHORTEN PACKED NUMBER) AFTER GETTING RID OF LEADING ZEROS             
         BCTR  RE,0                                                             
         LAY   R1,GETCPPD2                                                      
         EX    RE,0(R1)            / SPOT LEN EQUIV                             
*                                                                               
         LA    RE,1(RE)            LOCATE THE QUOTIENT                          
         LNR   RE,RE                                                            
         LA    RF,L'PACK16B2                                                    
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         LAY   R1,GETCPPZ1                                                      
         EX    RE,0(R1)            WE HAVE THE QUOTIENT IN PACK16B1             
*                                                                               
         AP    PACK16B1,=P'1'                                                   
         DP    PACK16B1,=P'2'      ROUNDED                                      
         ZAP   PACKOF8B,PACK16B1(15)                                            
         CVB   R1,PACKOF8B                                                      
         STCM  R1,15,GLGRP                                                      
         OI    GLGRP,X'40'        2 DECIMAL PLACES FOR GOAL POINTS              
                                                                                
GETCPPX  MVC   IOVALS(IOVALL),SVIOVALS                                          
         J     EXIT                                                             
         DROP  CPP,CPPEL,R2,R4                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PRODUCT CODE                                    *         
***********************************************************************         
                                                                                
VALPRD   L     R1,ACLTREC                                                       
         CLI   CPROF-CLTRECD(R1),C'0'                                           
         JNE   VALPRD02                                                         
         L     R1,LP_AINP          FOR TRUE POOL ENSURE POL PRODUCT             
         CLC   POLPRD,0(R1)                                                     
         JE    VALPRD02                                                         
         LHI   R0,1220             RETURN ERROR IF NOT                          
         STCM  R0,3,LP_ERROR                                                    
         XC    0(3,R1),0(R1)       CLEAR THIS OUT FOR THE ERROR OFFLINE         
         B     EXITN                                                            
                                                                                
VALPRD02 GOTOR (#VALPRD,AVALPRD),LP_AINP                                        
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE MARKET FOR BUY DOWNLOAD                         *         
***********************************************************************         
                                                                                
VALMKT   L     R1,LP_AINP                                                       
         L     RF,LP_ILEN                                                       
         SHI   RF,1                                                             
         MVC   DUB,EZEROS                                                       
         BASR  RE,0                                                             
         MVZ   DUB(0),0(R1)                                                     
         EX    RF,0(RE)                                                         
         CLC   DUB,EZEROS                                                       
         BNE   EXITN                                                            
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         OI    DUB+L'DUB-1,X'0F'                                                
         LA    R1,IOKEY                                                         
         USING MKTRECD,R1                                                       
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMEDA                                                    
         UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,LP_AGY                                                   
         MVC   MKTKFILL,EZEROS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO5'                            
         BNE   EXITN                                                            
         CVB   R0,DUB                                                           
         L     R1,LP_AOUT                                                       
         STCM  R0,3,0(R1)                                                       
         B     EXITY                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A STATION CODE AND SET MARKET                              *         
***********************************************************************         
                                                                                
VALSTM   LM    RF,R0,LP_AINP                                                    
         MVI   LP_VPARM,$VALMKAS   SET MARKET LOOKUP REQUIRED                   
         GOTOR (#VALSTA,AVALSTA),DMCB,(RF),(R0),STAPACKD                        
         BNE   EXIT                                                             
         L     RF,LP_AOUT                                                       
         MVC   0(L'STAPMKST,RF),STAPMKST                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EDIT LOCKIN TYPE                                                    *         
***********************************************************************         
                                                                                
EDTLTY   L     R1,LP_AINP                                                       
         L     RF,LP_AOUT                                                       
         MVI   0(RF),C'W'          SET WEEKLY                                   
         CLI   0(R1),GLKCOWKQ                                                   
         JE    EDTLTY02                                                         
         MVI   0(RF),C'M'          SET MONTHLY                                  
         CLI   0(R1),GLKCOMNQ                                                   
         JE    EDTLTY02                                                         
         MVI   0(RF),C'?'                                                       
                                                                                
EDTLTY02 LHI   R0,1                                                             
         STCM  R0,15,LP_OLEN                                                    
         B     EXITY                                                            
                                                                                
***********************************************************************         
* EDIT CLEARANCE STATION CODE                                         *         
***********************************************************************         
                                                                                
EDTCLS   L     R1,LP_AINP                                                       
         CLC   CLRSTAL,0(R1)                                                    
         BE    XCOLEN                                                           
         MVC   CLRSTAL,0(R1)                                                    
         J     EDTSTA02                                                         
                                                                                
***********************************************************************         
* EDIT STATION CODE                                                   *         
***********************************************************************         
                                                                                
EDTSTA   L     R1,LP_AINP                                                       
         OC    0(L'STAPSTA,R1),0(R1)                                            
         BZ    XCOLEN                                                           
         GOTOR ADDSTA              ADD TO STATION LIST IF NECESSARY             
                                                                                
EDTSTA02 XC    WORK2(L'STAPMKT),WORK2                                           
         MVC   WORK2+L'STAPMKT(L'STAPSTA),0(R1)                                 
         GOTOR GETSTA,WORK2                                                     
         L     R1,LP_AOUT                                                       
         MVC   0(L'STAPQSTA+L'STAPQNET,R1),STAPQSTA                             
         LHI   R0,L'STAPQSTA+L'STAPQNET                                         
         STCM  R0,15,LP_OLEN                                                    
         B     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
* EDIT SECURITY 'WHO DID IT'/'AUTHORIZED IT' AND OFFICE LOCATION     *          
**********************************************************************          
                                                                                
         USING EWWORKD,RC                                                       
         USING SA0REC,EWIO                                                      
EDTWHO   L     R1,LP_AINP          R1=A(PASSWORD NUMBER)                        
         OC    0(L'SA0KNUM,R1),0(R1)                                            
         BZ    XCOLEN                                                           
                                                                                
         MVC   EWIOSAVE,IOVALS     SAVE CURRENT I/O VALUES                      
         MVC   EWPASSWD,0(R1)      SET PASSWORD NUMBER                          
         L     R1,LP_ASECD                                                      
         MVC   EWSECAGY,SECAGY-SECD(R1)                                         
         LA    R1,EWIO                                                          
         ST    R1,IOADDR                                                        
         XC    EWPERV(EWPERVL),EWPERV                                           
                                                                                
         XC    SA0KEY,SA0KEY       READ PERSON PASSWORD RECORD                  
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,EWSECAGY                                                 
         MVC   SA0KNUM,EWPASSWD                                                 
         MVC   IOKEY,SA0KEY                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE'                                
         JNE   EDTWHO10                                                         
         LA    RE,SA0DATA                                                       
         USING SAPALD,RE                                                        
                                                                                
EDTWHO02 CLI   SAPALEL,EOR         TEST END OF RECORD                           
         JE    EDTWHO10                                                         
         CLI   SAPALEL,SAPALELQ    TEST NEW SECURITY PERSON ELEMENT             
         JE    EDTWHO04                                                         
         LLC   R0,SAPALLN          BUMP TO NEXT ELEMENT                         
         AR    RE,R0                                                            
         J     EDTWHO02                                                         
                                                                                
EDTWHO04 MVC   EWPID,SAPALPID      PERSON ID                                    
                                                                                
         USING SAPEREC,EWIO                                                     
         XC    SAPEKEY,SAPEKEY     READ NEW SECURITY PRESON RECORD              
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,EWSECAGY                                                 
         MVC   SAPEPID,EWPID                                                    
         MVC   IOKEY,SA0KEY                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE'                                
         JNE   *+2                                                              
         CLC   SAPEKEY(SAPEAGY-SAPEKEY),IOKEYSAV                                
         JNE   EDTWHO12                                                         
                                                                                
         LA    RE,SAPEDATA         LOCATE PERSONNEL DETAILS ELEMENT             
         USING SAPERD,RE                                                        
EDTWHO06 CLI   SAPEREL,EOR                                                      
         JE    EDTWHO12                                                         
         CLI   SAPEREL,SAPERELQ                                                 
         JE    EDTWHO08                                                         
         LLC   R0,SAPERLN                                                       
         AR    RE,R0                                                            
         J     EDTWHO06                                                         
EDTWHO08 MVC   EWOFFICE,SAPEROFF   EXTRACT OFFICE CODE                          
         J     EDTWHO12                                                         
                                                                                
EDTWHO10 MVC   EWPID,UNKNOWN       UNKNOWN PERSON                               
                                                                                
EDTWHO12 L     R1,LP_AOUT          POINT TO OUTPUT AREA                         
         LR    R0,R1                                                            
         OC    EWOFFICE,EWOFFICE   SEND OFFICE IF KNOWN                         
         JZ    EDTWHO14                                                         
         MVC   0(L'EWOFFICE,R1),EWOFFICE                                        
         MVI   L'EWOFFICE(R1),C' '                                              
         AHI   R1,L'EWOFFICE+1                                                  
                                                                                
EDTWHO14 MVC   0(L'EWPID,R1),EWPID SEND PID                                     
         AHI   R1,L'EWPID                                                       
         SR    R1,R0                                                            
         ST    R1,LP_OLEN          SET OUTPUT LENGTH                            
         MVC   IOVALS(IOVALL),EWIOSAVE                                          
         B     EXITY                                                            
         DROP  RC                                                               
                                                                                
EWWORKD  DSECT                     ** EDTWHO LOCAL WORKING STORAGE **           
EWPASSWD DS    XL(L'SA0KNUM)       PASSWORD NUMBER                              
EWSECAGY DS    XL(L'SA0KAGY)       SECURITY AGENCY                              
EWIOSAVE DS    XL(IOVALL)          SAVED I/O VALUES                             
                                                                                
EWPERV   DS    0X                  ** PERSON VALUES **                          
EWOFFICE DS    CL(L'SAPEROFF)      OFFICE CODE                                  
EWPID    DS    XL(L'SAPALPID)      PERSON ID                                    
EWPERVL  EQU   *-EWPERV                                                         
                                                                                
EWIO     DS    XL1000              I/O AREA                                     
SVRDEF   CSECT                                                                  
         EJECT                                                                  
**********************************************************************          
* EDIT PERSON ID AND EXTRACT PERSON NAME FROM PID RECORD             *          
**********************************************************************          
                                                                                
         USING EPWORKD,RC                                                       
         USING SA0REC,EPIO                                                      
EDTPID   MVC   EPIOSAVE,IOVALS     SAVE CURRENT I/O VALUES                      
         XC    PERNAME,PERNAME     CLEAR PERSON NAME                            
         LHI   R0,L'SAPALPID                                                    
         ST    R0,LP_OLEN                                                       
         L     R1,LP_AINP          R1=A(PASSWORD NUMBER)                        
         OC    0(L'SA0KNUM,R1),0(R1)                                            
         JZ    EDTPIDN                                                          
                                                                                
         XC    SA0KEY,SA0KEY       READ PERSON PASSWORD RECORD                  
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KNUM,0(R1)                                                    
         L     R1,LP_ASECD                                                      
         MVC   SA0KAGY,SECAGY-SECD(R1)                                          
         MVC   IOKEY,SA0KEY                                                     
         LA    R1,EPIO                                                          
         ST    R1,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE'                                
         JNE   EDTPIDN                                                          
                                                                                
         LA    R1,SA0DATA                                                       
         USING SAPALD,R1                                                        
EDTPID02 CLI   SAPALEL,EOR         TEST END OF RECORD                           
         JE    EDTPIDN                                                          
         CLI   SAPALEL,SAPALELQ    TEST NEW SECURITY PERSON ELEMENT             
         JE    EDTPID04                                                         
         LLC   R0,SAPALLN          BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         J     EDTPID02                                                         
                                                                                
EDTPID04 L     RF,LP_AOUT                                                       
         MVC   0(L'SAPALPID,RF),SAPALPID                                        
                                                                                
         USING SAPEREC,EPIO                                                     
         XC    SAPEKEY,SAPEKEY     READ NEW SECURITY PERSON RECORD              
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         L     R1,LP_ASECD                                                      
         MVC   SAPEAGY,SECAGY-SECD(R1)                                          
         MVC   SAPEPID,0(RF)                                                    
         MVC   IOKEY,SA0KEY                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE'                                
         JNE   *+2                                                              
         CLC   SAPEKEY(SAPEAGY-SAPEKEY),IOKEYSAV                                
         JNE   EDTPIDY                                                          
                                                                                
         LA    R1,SAPEDATA         LOCATE PERSONNEL DETAILS ELEMENT             
         USING SANAMD,R1                                                        
EDTPID06 CLI   SANAMEL,EOR                                                      
         JE    EDTPIDY                                                          
         CLI   SANAMEL,SANAMELQ                                                 
         JE    EDTPID08                                                         
         LLC   R0,SANAMLN                                                       
         AR    R1,R0                                                            
         J     EDTPID06                                                         
                                                                                
EDTPID08 MVC   EPNAMIND,SANAMIND                                                
         LA    R1,SANAMES                                                       
         LA    R2,PERNAME                                                       
         USING SANAMES,R1                                                       
                                                                                
         TM    EPNAMIND,SANAMIFN   TEST FIRST NAME PRESENT                      
         JZ    EDTPID10                                                         
         LLC   RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),SANAME                                                   
         EX    RF,0(RE)                                                         
         LA    R2,1(R2,RF)                                                      
         MVI   0(R2),C' '                                                       
         AHI   R2,1                                                             
         LA    R1,2(RF,R1)                                                      
                                                                                
EDTPID10 TM    EPNAMIND,SANAMIMN   TEST MIDDLE NAME PRESENT                     
         JZ    EDTPID12                                                         
         LLC   RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),SANAME                                                   
         EX    RF,0(RE)                                                         
         LA    R2,1(R2,RF)                                                      
         MVI   0(R2),C' '                                                       
         AHI   R2,1                                                             
         LA    R1,2(RF,R1)                                                      
                                                                                
EDTPID12 TM    EPNAMIND,SANAMILN   TEST LAST NAME PRESENT                       
         JZ    EDTPIDY                                                          
         LLC   RF,SANAMELN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R2),SANAME                                                   
         EX    RF,0(RE)                                                         
         J     EDTPIDY                                                          
         DROP  R1                                                               
                                                                                
EDTPIDN  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         B     XCOLEN                                                           
                                                                                
EDTPIDY  MVC   IOVALS(IOVALL),EPIOSAVE                                          
         B     EXITY                                                            
         DROP  RC                                                               
                                                                                
EPWORKD  DSECT                     ** EDTPID LOCAL WORKING STORAGE **           
EPIOSAVE DS    XL(IOVALL)          SAVED I/O VALUES                             
EPNAMIND DS    XL(L'SANAMIND)      NAME INDICATORS                              
EPIO     DS    XL1000              I/O AREA                                     
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EDIT FILM CODE                                                      *         
***********************************************************************         
                                                                                
EDTFLM   L     R1,LP_AINP                                                       
         OC    0(L'SPTFILM1,R1),0(R1)                                           
         BZ    XCOLEN                                                           
         ST    RC,AFLMREC    SET DDLINK PASSED STORAGE AS LP_BLK/IOAREA         
                                                                                
         MVC   WORK(FLMRKEYL),0(R1)                                             
         MVC   FLMRKEY(FLMRKEYL),WORK                                           
         GOTOR BUFFER,DMCB,('TSARDH',0)                                         
         JE    EDTFLM10                                                         
                                                                                
         MVC   SVIOVALS,IOVALS                                                  
         LA    R2,IOKEY                                                         
         USING CMLRECD,R2          READ COMMERCIAL PASSIVE                      
         XC    CMLKEY,CMLKEY                                                    
         MVI   CMLPID+0,X'0A'                                                   
         MVI   CMLPID+1,X'A1'                                                   
         MVC   CMLPAM,QMEDX                                                     
         MVC   CMLPCLT,QCLTX                                                    
         MVC   CMLPSEQ+1(2),WORK                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOBRD+IOTRFDIR+B#FLMREC'                      
         JNE   EDTFLM06                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOBGET+IOTRFFIL+B#FLMREC'                     
         JNE   EDTFLM06                                                         
                                                                                
         L     R2,AFLMREC                                                       
         MVC   FLMRCODE,SPACES     SET FILM CODE FROM KEY                       
         MVC   FLMRCODE(L'CMLKCML),CMLKCML                                      
                                                                                
         LA    R2,CMLDTAEL                                                      
         USING CMLADIEL,R2         LOOK FOR AD-ID ELEMENT                       
EDTFLM02 LLC   R0,CMLADILN                                                      
         AR    R2,R0                                                            
         CLI   CMLADIEL,EOR                                                     
         JE    EDTFLM08                                                         
         CLI   CMLADIEL,X'A0'                                                   
         JNE   EDTFLM02                                                         
         MVC   FLMRCODE,CMLADID    FOUND IT - USE AD-ID INSTEAD                 
         J     EDTFLM08                                                         
         DROP  R2                                                               
                                                                                
EDTFLM06 MVI   FLMRCODE,C'?'                                                    
         MVC   FLMRCODE+1(L'FLMRCODE-1),FLMRCODE                                
                                                                                
EDTFLM08 MVC   IOVALS(IOVALL),SVIOVALS                                          
         MVC   FLMRKEY(FLMRKEYL),WORK     ADD BUFFER RECORD                     
         GOTOR BUFFER,DMCB,('TSAADD',0)                                         
         JNE   *+2                                                              
                                                                                
EDTFLM10 LHI   R0,L'FLMRCODE                                                    
         STCM  R0,15,LP_OLEN                                                    
         L     R1,LP_AOUT                                                       
         MVC   0(L'FLMRCODE,R1),FLMRCODE                                        
*                                                                               
         XC    AFLMREC,AFLMREC     CLEAR LP_BLK ADDRESS                         
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ADD NEW STATION TO LIST OF STATIONS FOR READ FOR ROUTES DOWNLOAD    *         
***********************************************************************         
                                                                                
ADDSTA   STM   RE,R1,12(RD)                                                     
                                                                                
         CLC   LP_QMAPN,SDNBUYD#   (X'014A') NEW BUY DOWNLOAD                   
         JE    ADDSTAX             YES, ADDSTA IS NOT NECESSARY                 
                                                                                
         ICM   RE,15,ASTALST       RE=A(STATION LIST WMP ENTRY)                 
         JZ    ADDSTAX                                                          
                                                                                
         CLI   0(R1),X'E8'         ARE WE CABLE?                                
         JL    *+8                                                              
         NI    2(R1),X'80'         YES, ONLY WANT SYSCODE LEVEL                 
                                                                                
         USING LW_D,RE                                                          
         LA    RF,LW_DATA2         POINT TO STATION LIST                        
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET ENTRY COUNT SO FAR                       
         JZ    ADDSTA04                                                         
ADDSTA02 CLC   0(L'DOKSTA,RF),0(R1)                                             
         JE    ADDSTAX             EXIT IF ALREADY HAVE THIS STATION            
         AHI   RF,L'DOKSTA                                                      
         JCT   R0,ADDSTA02                                                      
         ICM   R0,3,LW_NUMN        ADD NEW ENTRY TO END OF LIST                 
                                                                                
ADDSTA04 AHI   R0,1                BUMP ENTRY COUNT                             
         STCM  R0,3,LW_NUMN                                                     
         MVC   0(L'DOKSTA,RF),0(R1)                                             
         XC    L'DOKSTA(L'DOKSTA,RF),L'DOKSTA(RF)                               
                                                                                
ADDSTAX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* ADD NEW STATION TO LIST OF BUY STATIONS FOR NXTMST ROUTINE          *         
*                                                                     *         
* NTRY:- R1=A(STATION CALL LETTERS)                                   *         
***********************************************************************         
                                                                                
ADBSTA   STM   RE,R1,12(RD)                                                     
                                                                                
         ICM   RE,15,ASTALST       RE=A(STATION LIST WMP ENTRY)                 
         JZ    ADBSTAX                                                          
                                                                                
         USING LW_D,RE                                                          
         LA    RF,LW_DATA2         POINT TO STATION LIST                        
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        GET ENTRY COUNT SO FAR                       
         JZ    ADBSTA04                                                         
ADBSTA02 CLC   0(L'STAPQSTA,RF),0(R1)                                           
         JE    ADBSTAX             EXIT IF ALREADY HAVE THIS STATION            
         AHI   RF,L'STAPQSTA+1                                                  
         JCT   R0,ADBSTA02                                                      
         ICM   R0,3,LW_NUMN        ADD NEW ENTRY TO END OF LIST                 
                                                                                
ADBSTA04 AHI   R0,1                BUMP ENTRY COUNT                             
         STCM  R0,3,LW_NUMN                                                     
         MVC   0(L'STAPQSTA,RF),0(R1)                                           
         MVI   L'STAPQSTA(RF),0                                                 
         AHI   RF,L'STAPQSTA+1                                                  
         ST    RF,ASTALSTX                                                      
         XC    L'STAPQSTA+1(L'STAPQSTA+1,RF),L'STAPQSTA+1(RF)                   
                                                                                
ADBSTAX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
**********************************************************************          
* EDIT MARKET GROUP                                                             
**********************************************************************          
EDTMGRP  L     R2,LP_AINP                                                       
         L     R3,LP_AOUT                                                       
         UNPK  0(5,R3),0(3,R2)                                                  
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* EDIT/TRANSLATE BOOK TYPE                                                      
**********************************************************************          
TRNSBKT  LM    R2,R4,LP_AINP                                                    
         CLI   0(R2),0                                                          
         JE    EXITN                                                            
                                                                                
         GOTOR (#TRNSBT,ATRNSBT),DMCB,(R2),1,(R4),12                            
         MVI   LP_OLEN+3,2                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT BUY LINE VALUES                                              *         
***********************************************************************         
                                                                                
         USING BUYREC,R2                                                        
FMTLIN   NTR1  LABEL=NO                                                         
         LA    R0,LINVALS                                                       
         LHI   R1,LINVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ST    R2,IOADDR                                                        
         MVC   LINDA,IODA                                                       
         MVC   LINRSIZ,BUYRLEN                                                  
         GOTOR GETSUM,LINCKSM      SET RECORD CHECK SUM                         
         CLC   PESTNUM,BUYKEST     TEST CHANGE OF ESTIMATE NUMBER               
         MVC   PESTNUM,BUYKEST                                                  
         JE    *+10                NO - DON'T BOTHER SENDING IT                 
         MVC   LINEST,BUYKEST                                                   
                                                                                
         MVC   LINSTA,BUYKSTA                                                   
         MVC   LINLIN,BUYKBUY                                                   
         CLI   BDDAY,B'01111100'   TEST MONDAY-FRIDAY ROTATION                  
         JE    *+10                YES - DON'T BOTHER SENDING IT                
         MVC   LINROT,BDDAY                                                     
         LLC   R1,BDSEDAY                                                       
         SRL   R1,4                                                             
         CHI   R1,1                TEST MONDAY START DAY                        
         JE    *+8                 YES - DON'T BOTHER SENDING IT                
         STC   R1,LINDAY                                                        
         MVC   LIN#WK,BDNOWK                                                    
         MVC   LINSTRD,BDSTART                                                  
         MVC   LINENDD,BDEND                                                    
         MVC   LINCHGD,BDCHG                                                    
         MVC   LINSTRT,BDTIMST                                                  
         MVC   LINENDT,BDTIMEND                                                 
         MVC   LINPRP,BDPURP                                                    
         MVC   LINTAX,BDNTAX                                                    
         CLI   BDSEC,SEC30Q        TEST DEFAULT SECOND SPOT LENGTH              
         JE    *+10                YES - DON'T BOTHER SENDING IT                
         MVC   LINSEC,BDSEC                                                     
         MVC   LINDPT,BDDAYPT                                                   
         MVC   LINMGC,BDMGDATE                                                  
         MVC   LINWHY1,BDWHY                                                    
         MVC   LINWHY2,BDWHY2                                                   
         MVC   LINWHY3,BDWHY3                                                   
         MVC   LINPCKEY,PCKEY                                                   
         MVC   LINSTAT,BDSTAT                                                   
         MVC   LINSTAT2,BDSTAT2                                                 
         MVC   LINSTAT3,BDSTAT3                                                 
         MVC   LINCIND,BDCIND                                                   
         XC    PCKEY,PCKEY         SEND PC KEY ONCE ONLY                        
                                                                                
         OC    BDREP,BDREP         SET REP CODE IF PRESENT                      
         JZ    FMTLIN02                                                         
         GOTOR VRCPACK,DMCB,(C'U',BDREP),LINREP                                 
                                                                                
FMTLIN02 L     RF,ACLTREC          FORMAT PROGRAM ADJACENCY CODE                
         CLI   CPROF+9-CLTRECD(RF),C'0'                                         
         JE    FMTLIN04                                                         
         MVC   LINADJ(L'BDPROGT),BDPROGT                                        
         CLI   CPROF+9-CLTRECD(RF),C'1'                                         
         JE    FMTLIN04                                                         
         ICM   R1,8,BDPROGT                                                     
         SLDL  R0,4                                                             
         SLL   R0,4                                                             
         SLDL  R0,4                                                             
         STCM  R0,3,LINADJ                                                      
         OI    LINADJ+0,C'0'                                                    
         OI    LINADJ+1,C'0'                                                    
                                                                                
FMTLIN04 MVC   LINPGN,BDPROGRM     SET PROGRAM NAME (NO CODE)                   
                                                                                
FMTLIN10 LA    R3,BDELEM           LOCATE CONTRACT NUMBER ELEMENT               
         USING IDELEM,R3                                                        
         SR    R0,R0                                                            
FMTLIN12 CLI   IDELEM,EOR          TEST END OF RECORD                           
         JE    FMTLIN16                                                         
         CLI   IDELEM,IDELEMQ      TEST CONTRACT NUMBER ELEMENT                 
         JE    FMTLIN13                                                         
         IC    R0,IDELLEN                                                       
         AR    R3,R0                                                            
         J     FMTLIN12                                                         
FMTLIN13 DS    0H                                                               
         OC    LP_VRSN,LP_VRSN     HAVE SBTK VERSION?                           
         JZ    FMTLIN14             -NO, LEAVE ALONE                            
         CLC   LP_VRSN,V470037     SBTK <= V4.7.0.37?                           
         JL    FMTLIN14             -YES, LEAVE ALONE                           
         CLC   LP_VRSN,V4700149    SBTK >= V4.7.0.149?                          
         JNL   FMTLIN14             -YES, LEAVE ALONE                           
         CLI   PROFB0V+09,C'Y'     PURPOSE CODE REQUIRED?                       
         JE    *+12                                                             
         CLI   PROFB0V+09,C'O'     PURPOSE CODE OPTIONAL?                       
         JNE   FMTLIN14                                                         
         BRAS  RE,VALPURP          HAVE PURPOSE CODE?                           
         JNE   FMTLIN16             NO                                          
FMTLIN14 MVC   LINCON,IDCONNO      SET CONTRACT NUMBER                          
         DROP  R3                                                               
                                                                                
FMTLIN16 CLC   LP_QMAPN,SDBUYDD#   TEST DOWNLOAD ON EXIT FROM $BUY              
         JE    FMTLIN18                                                         
         CLC   LP_QMAPN,SDBUYD#    TEST BUY DOWNLOAD                            
         JE    FMTLIN18                                                         
         CLC   LP_QMAPN,SDNBUYD#   OR NEW BUY DOWNLOAD                          
         JNE   FMTLINX                                                          
                                                                                
FMTLIN18 MVC   LINPER,SPACES       FORMAT BUY PERIOD                            
         GOTOR VDATCON,DMCB,(3,BDSTART),(7,LINPER)                              
         MVI   LINPER+5,C'-'                                                    
         MVI   LINPER+6,C'E'                                                    
         CLI   BDINPUT,2           TEST INPUT METHOD                            
         JH    FMTLIN22            -E                                           
         JL    FMTLIN20            WEEKS                                        
         GOTOR (RF),(R1),(3,BDEND),(7,LINPER+6)                                 
         J     FMTLIN22                                                         
                                                                                
FMTLIN20 LLC   R0,BDWKS                                                         
         CURED (R0),(2,LINPER+6),0,ALIGN=LEFT                                   
         LA    RE,LINPER+6                                                      
         AR    RE,R0                                                            
         MVI   0(RE),C'W'                                                       
         CLI   BDWKIND,C'O'                                                     
         JE    FMTLIN22                                                         
         MVC   1(1,RE),BDWKIND                                                  
                                                                                
FMTLIN22 GOTOR (#EDTPRD,AEDTPRD),DMCB,BDMASPRD+0,,LINPRD                        
         GOTOR (#EDTPRD,AEDTPRD),DMCB,BDMASPRD+1,,LINPIG                        
                                                                                
         USING GETBUBLD,WORK                                                    
         XC    GETBUBLD,GETBUBLD   SET BUYER CODE                               
         MVC   GBCOMFAC,ACOMFACS                                                
         MVC   GBIOA,AIO6                                                       
         XC    WORK2,WORK2         BUILD DUMMY INPUT FIELD                      
         MVI   WORK2,L'LNKINPH+20                                               
         LA    R0,WORK2                                                         
         ST    R0,GBNAMFLD                                                      
         MVC   GBAGY,AGENCY                                                     
         MVC   GBMEDEBC,BUYMED                                                  
         MVC   GBCLTEBC,QCLTA                                                   
                                                                                
         OC    CLTNAME,CLTNAME     CLIENT NAME RESOLVED?                        
         JNZ   FMTLIN24                                                         
         L     RF,ACLTREC                                                       
         USING CLTRECD,RF                                                       
         MVC   CLTNAME,CNAME                                                    
         MVC   CLTOFF,COFFICE                                                   
         DROP  RF                                                               
                                                                                
FMTLIN24 MVC   GBOFFICE,CLTOFF                                                  
         MVC   GBAGYMD,BUYKAM                                                   
         MVC   GBCLT,QCLTX                                                      
         MVC   GBPRD,BUYKPRD                                                    
         MVC   GBEST,BUYKEST                                                    
         MVC   GBMKT(L'GBMKT+L'GBSTA),BUYKMSTA                                  
         MVI   GBTYPE,C'B'         SET FOR BUYER CODE                           
                                                                                
         CLC   LBUYGBKY,GBAGYMD    TEST GETBUBL KEY CHANGED                     
         JNE   *+14                                                             
         MVC   LINBYR,LBUYBUYR     NO - WE ALREADY HAVE BUYER                   
         J     FMTLIN26                                                         
                                                                                
         MVC   LBUYGBKY,GBAGYMD    SET GETBUBL KEY - CALL GETBUBL               
         GOTOR VGETBUBL,DMCB,GETBUBLD                                           
         CLI   GBERR,0                                                          
         JNE   FMTLIN26                                                         
         MVC   LINBYR,WORK2+L'LNKINPH                                           
         MVC   LBUYBUYR,LINBYR     SET LAST BUYER CODE                          
                                                                                
FMTLIN26 MVI   LINRTY,C'C'         SET RATE TYPE                                
         TM    BDCIND2,X'80'                                                    
         JO    FMTLIN28                                                         
         MVI   LINRTY,C' '                                                      
         TM    BDCIND,X'20'                                                     
         JO    FMTLIN28                                                         
         MVI   LINRTY,C'F'                                                      
         TM    BDCIND,X'80'                                                     
         JO    FMTLIN28                                                         
         MVI   LINRTY,C'Q'                                                      
         TM    BDCIND,X'40'                                                     
         JO    FMTLIN28                                                         
         MVI   LINRTY,C'N'                                                      
         TM    BDCIND,X'10'                                                     
         JO    FMTLIN28                                                         
         MVI   LINRTY,C'V'                                                      
         TM    BDCIND,X'08'                                                     
         JO    FMTLIN28                                                         
         MVI   LINRTY,C'S'                                                      
         TM    BDCIND,X'04'                                                     
         JO    FMTLIN28                                                         
         MVI   LINRTY,C'X'                                                      
         TM    BDCIND,X'02'                                                     
         JO    FMTLIN28                                                         
         MVI   LINRTY,C'P'                                                      
                                                                                
FMTLIN28 LA    R3,BDELEM           PROCESS OTHER BUY LINE ELEMENTS              
FMTLIN30 CLI   0(R3),EOR           TEST END OF RECORD                           
         JE    FMTLINX                                                          
         CLI   0(R3),NDCORGQ       (X'02') TEST DEMO ELEMENT                    
         JE    FMTLIN34                                                         
         CLI   0(R3),CMELEMQ       (X'66') TEST COMMENT ELEMENT                 
         JE    FMTLIN36                                                         
         CLI   0(R3),RCELCODQ      (X'90') TEST REASON CODE ELEMENT             
         JE    FMTLIN38                                                         
         CLI   0(R3),UPELEMQ       (X'62') TEST UPGRADE ELEMENT                 
         JE    FMTLIN40                                                         
         CLI   0(R3),DLOVELQ       (X'24') DEMO OVERRIDE ELEMENT                
         JE    FMTLIN42                                                         
                                                                                
FMTLIN32 LLC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         J     FMTLIN30                                                         
                                                                                
         USING NDELEM,R3                                                        
FMTLIN34 MVC   LINRBOOK,NDBOOK                                                  
         J     FMTLIN32                                                         
                                                                                
         USING COMELEM,R3                                                       
FMTLIN36 CLI   CMNUM,LINCOMN                                                    
         JH    FMTLIN32                                                         
         LLC   RF,CMNUM                                                         
         BCTR  RF,0                                                             
         MHI   RF,L'LINCOMS                                                     
         LA    RF,LINCOMS(RF)                                                   
         LLC   R1,CMLEN                                                         
         SHI   R1,CMDATA+1-COMELEM                                              
         BASR  RE,0                                                             
         MVC   0(0,RF),CMDATA                                                   
         EX    R1,0(RE)                                                         
         J     FMTLIN32                                                         
                                                                                
         USING RCELEM,R3                                                        
FMTLIN38 MVC   LINRSN,RCELRC                                                    
         J     FMTLIN32                                                         
                                                                                
         USING UPELEM,R3                                                        
         USING SPEDUPD,WORK                                                     
FMTLIN40 XC    SPEDUPD(SPEDUPL),SPEDUPD                                         
         MVI   SPEDCTRY,C'U'                                                    
*** NOP'D - CANADIAN AGENCY WILL NEVER BE SUPPORTED    -HWON 05/26/2020         
***      TM    RUNI1,RUNICANA      TEST CANADIAN AGENCY                         
***      JZ    *+8                                                              
***      MVI   SPEDCTRY,C'C'                                                    
         MVC   SPEDMEDC,QMEDA                                                   
         L     RF,ACLTREC                                                       
         MVC   SPEDUDEM,CEXTRA-CLTRECD(RF)                                      
         LA    RF,UPELEM                                                        
         ST    RF,SPEDAUPE                                                      
                                                                                
         CLC   UPTYPE+4-UPELEM(2,RF),=X'E800'                                   
         JL    *+8                                                              
         MVI   UPTYPE+4-UPELEM(RF),0                                            
                                                                                
         LA    RF,LINUPG                                                        
         ST    RF,SPEDAOUT                                                      
         MVC   SPEDACOM,ACOMFACS                                                
         MVC   SPEDADEM,VDEMOCON                                                
         MVC   SPEDADAY,VUNDAY                                                  
         MVC   SPEDATIM,VUNTIME                                                 
         GOTOR ASPEDUP,SPEDUPD                                                  
                                                                                
         CLC   =C'UPT=',LINUPG                                                  
         JNE   FMTLIN32                                                         
         CLI   LINUPG+13,C' '                                                   
         JNE   FMTLIN32                                                         
         MVI   LINUPG+13,C'/'                                                   
         J     FMTLIN32                                                         
                                                                                
         USING DLUELEM,R3                                                       
FMTLIN42 CLI   DLUBKTYP,0          ANY BOOK TYPE?                               
         JE    FMTLIN32                                                         
         CLI   DLUBKTYP,C' '       OR BOOK TYPE OF SPACE?                       
         JE    FMTLIN32            THEN NONE                                    
                                                                                
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTOR (RF),DMCB,SPBOOKTB    PARAM1 = A(BOOK TYPE TABLE)                
         ICM   RE,15,0(R1)                                                      
         JZ    *+2                 THIS SHOULD NEVER HAPPEN                     
         USING SPBKTYPD,RE                                                      
         ICM   RF,15,4(R1)         GET ENTRY LENGTH RETURNED TO US              
                                                                                
FMTLIN44 CLI   0(RE),FF            TEST END OF TABLE?                           
         JNE   FMTLIN46                                                         
         MVC   LINBTYPE,=C'??'     DON'T KNOW WHAT IT IS AND WE DO NOT          
         J     FMTLIN32            WANT TO DIE FOR THIS                         
                                                                                
FMTLIN46 CLC   DLUBKTYP,SPBKTYPN                                                
         JE    FMTLIN48                                                         
         AR    RE,RF                                                            
         J     FMTLIN44                                                         
                                                                                
FMTLIN48 MVC   LINBTYPE,SPBKTYPA                                                
         J     FMTLIN32                                                         
                                                                                
FMTLINX  B     EXITY                                                            
         DROP  R2,R3,RE                                                         
         EJECT                                                                  
***********************************************************************         
* READ MARKET RECORD                                                  *         
*                                                                     *         
* NTRY:- R1=A(BINARY MARKET NUMBER)                                   *         
* EXIT:- WORK=MARKET NAME + ALPHA MARKET + LPM START DATE             *         
*                         + CABLE DEMO FLAG                           *         
***********************************************************************         
                                                                                
GETMKT   NTR1  LABEL=NO,WORK=(RC,GMWORKL)                                       
         USING GMWORKD,RC                                                       
         MVC   GMIOVALS,IOVALS                                                  
                                                                                
         SR    R0,R0               CONVERT BINARY MARKET TO EBCDIC              
         ICM   R0,3,0(R1)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
                                                                                
         LA    R1,IOKEY                                                         
         USING MKTREC,R1           READ MARKET RECORD                           
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMEDA                                                    
         UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,AGENCY                                                   
         MVC   MKTKFILL,EZEROS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO3'                            
         L     R1,IOADDR                                                        
         JE    GETMKT10                                                         
         MVI   MKTNAME,C'?'                                                     
         MVC   MKTNAME+1(L'MKTNAME-1),MKTNAME                                   
         XC    MKTALF,MKTALF                                                    
         MVC   MKTLPMDT,MKTLPMDT                                                
         XC    MKTCDEM,MKTCDEM                                                  
                                                                                
GETMKT10 MVC   STAMKTN,MKTNAME     SET MARKET NAME                              
         MVC   STAAMKT,MKTALF      SET ALPHA MARKET                             
         MVC   STALPMD,MKTLPMDT    SET LPM START DATE                           
         MVC   STACDEM,MKTCDEM     SET CABLE DEMO FLAG                          
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE VERSION #?                              
         JZ    *+14                 NO, OVERRIDE LPMDATE                        
         CLC   LP_VRSN,V460072     SBTK V4.6.0.72?                              
         JNE   GETMKT15                                                         
         MVC   STALPMD,=X'C821'    FORCE 1/1/2000 LPM START DATE                
*                                                                               
GETMKT15 XC    STAAAMK,STAAAMK     SET MEDIAOCEAN AUTOMATED AVAIL MKT #         
         LA    RE,C'0'                                                          
         CLI   QMEDA,C'T'          MEDIA T, WE NEED NIELSEN                     
         JE    GETMKT20                                                         
         LA    RE,C'1'                                                          
         CLI   QMEDA,C'R'          MEDIA R OR X, WE NEED ARBITRON               
         JE    GETMKT20                                                         
         CLI   QMEDA,C'X'                                                       
         JNE   GETMKT30            WHAT ELSE TO USE OTHERWISE?                  
*                                                                               
GETMKT20 LA    RF,MKTRSM1                                                       
         CLM   RE,1,MKTRS1         MATCH ON THIS RATING SERVICE?                
         JE    GETMKT25                                                         
         LA    RF,MKTRSM2                                                       
         CLM   RE,1,MKTRS2         OR THIS ONE?                                 
         JNE   GETMKT30                                                         
GETMKT25 MVC   STAAAMK,0(RF)       YES, USE THAT FOR THE AA MARKET              
*                                                                               
GETMKT30 MVC   STAMKTZ,MKTZONE     SET TIME ZONE CODE                           
         MVC   STAMKBT,MKTBKTYP    SET BOOKTYPE                                 
         MVC   STAMKBT2,MKTBKTYP    SET BOOKTYPE                                
         MVC   MKTCBKTY,MKTCSBTY   SET comScore BOOKTYPE                        
                                                                                
         MVC   IOVALS(IOVALL),GMIOVALS                                          
         B     EXITY                                                            
         DROP  R1,RC                                                            
                                                                                
GMWORKD  DSECT                     ** GETMKT LOCAL WORKING STORAGE **           
GMIOVALS DS    XL(IOVALL)                                                       
GMWORKL  EQU   *-GMWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* READ MARKET RECORD                                                  *         
*                                                                     *         
* NTRY:- R1=A(BINARY MARKET NUMBER)                                   *         
* EXIT:- WORK=MARKET NAME + ALPHA MARKET + LPM START DATE             *         
*                         + CABLE DEMO FLAG                           *         
***********************************************************************         
                                                                                
GTCOMMKT NTR1  LABEL=NO,WORK=(RC,GCWORKL)                                       
         USING GCWORKD,RC                                                       
         MVC   GCIOVALS,IOVALS                                                  
                                                                                
         LA    R1,IOKEY                                                         
         USING CTDMKEY,R1          READ ALPHA MARKET RECORD                     
         XC    CTDMKEY,CTDMKEY                                                  
         MVI   CTDMKTYP,CTDMKTEQ   C'D'                                         
         MVI   CTDMKTY2,CTDMKT2E   C'M'                                         
         MVC   CTDMKMED,QMEDA                                                   
         MVI   CTDMKSRC,C'C'       C'C' FOR COMSCORE                            
         MVC   CTDMKMKT,STAAMKT    ALPHA MARKET                                 
         MVI   CTDMKBKT,X'FF'      STANDARD BOOKTYPE                            
         LA    R1,GCIO                                                          
         ST    R1,IOADDR                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCTFILE'                                
         JNE   *+2                                                              
         L     R1,IOADDR                                                        
         CLC   0(CTDMKNUM-CTDMKEY,R1),IOKEY                                     
         JNE   EXITY                                                            
         MVC   STACMKT,CTDMKNUM                                                 
         J     EXITY                                                            
         DROP  RC,R1                                                            
GCWORKD  DSECT                     ** GTCOMMKT LOCAL WORKING STORAGE **         
GCIOVALS DS    XL(IOVALL)                                                       
GCIO     DS    XL1000                                                           
GCWORKL  EQU   *-GCWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* READ STATION RECORD                                                 *         
*                                                                     *         
* NTRY:- STASTA=A(STATION CALL LETTERS)                               *         
* EXIT:- STAVALS CONTAIN STATION VALUES                               *         
***********************************************************************         
                                                                                
GETSCL   NTR1  LABEL=NO,WORK=(RC,GSWORKL)                                       
         USING GSWORKD,RC                                                       
                                                                                
         MVC   WORK(L'STASTA),STASTA                                            
         MVC   WORK+L'STASTA(L'STASEND),STASEND                                 
         XC    STAVALS(STAVALL),STAVALS                                         
         MVC   STAORIG,WORK                                                     
         MVC   STASTA,WORK                                                      
         MVC   STASEND,WORK+L'STASTA                                            
         MVC   GSIOVALS,IOVALS                                                  
                                                                                
         LA    R1,IOKEY                                                         
         USING STAREC,R1           READ STATION RECORD                          
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,QMEDA                                                    
                                                                                
         MVC   STAKCALL,STASTA                                                  
         CLI   STAKCALL+4,C' '                                                  
         JNE   *+14                                                             
         MVC   STAKCALL+4(1),QMEDA                                              
         J     GETSCL02                                                         
                                                                                
         CLI   STAKCALL+4,C'-'                                                  
         JNE   *+14                                                             
         MVC   STAKCALL+4(1),5(RF)                                              
         J     GETSCL02                                                         
                                                                                
         CLI   STAKCALL+4,C'/'                                                  
         JNE   GETSCL02                                                         
         MVI   STAKCALL+4,C'T'                                                  
                                                                                
GETSCL02 MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLTA       LOOKING FOR CLIENT SPECIFIC FIRST            
         MVC   STAKFILL,EZEROS                                                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO3'                            
         L     R1,IOADDR                                                        
         JE    GETSCL04                                                         
*                                                                               
         MVC   IOKEY,IOKEYSAV      RESTORE OUR KEY SO WE CAN                    
         MVC   STAKCLT,EZEROS      GET RID OF THE CLT SPECIFIC                  
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO3'                            
         L     R1,IOADDR                                                        
         JE    GETSCL04                                                         
                                                                                
         XC    SCHNL,SCHNL                                                      
         XC    SNETWRK,SNETWRK                                                  
         XC    SFORMAT,SFORMAT                                                  
         XC    SNEWTAX,SNEWTAX                                                  
         XC    SBKTYPE,SBKTYPE                                                  
         XC    STACBKTY,STACBKTY                                                
                                                                                
GETSCL04 XR    R0,R0                                                            
         ICM   R0,3,STAKLEN                                                     
         LHI   RF,SCBLSQNQ         SO FAR, THIS IS MAX LEN OF STA REC           
         SR    RF,R0               # OF BYTES LEFT TO CLEAR                     
         LR    RE,R1                                                            
         AR    RE,R0                                                            
         XCEFL                                                                  
*                                                                               
         L     R1,IOADDR                                                        
         MVC   WORK(L'SCHNL),SCHNL RETURN CHANNEL IN WORK TOO                   
         CLI   STASTA+4,C'T'       PC DOES NOT WANT A BAND OF T                 
         JNE   *+8                                                              
         MVI   STASTA+4,C' '                                                    
                                                                                
         MVC   STACLT,STAKCLT                                                   
         CLC   STACLT,EZEROS       NOT A CLIENT SPECIFIC STATION?               
         JNE   *+10                                                             
         XC    STACLT,STACLT       CLEAR IT SO IT WON'T BE SENT                 
                                                                                
         MVC   STACHNL,SCHNL                                                    
         MVC   STANTWK,SNETWRK                                                  
         MVC   STAFRMT,SFORMAT                                                  
         MVC   STABTYP,SBKTYPE                                                  
         MVC   STAMIDS,STMIDAS                                                  
         MVC   STACBKTY,SCBKTYPE                                                
         MVC   STAPPLUS,SPARPLUS                                                
*                                                                               
* TO ADDRESS A SBTK ISSUE, WHEN THIS VALUE IS NULL OR SPACE, SBTK               
* WOULD COPY THE VALUE FROM THE PREVIOUS D/L STATION TO THE CURRENT             
* TO FIX, WE ARE FORCING A C'N' FOR ANY NULL OR SPACE                           
*                                                                               
         CLI   STAPPLUS,C' '                                                    
         JH    *+8                                                              
         MVI   STAPPLUS,C'N'                                                    
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,SNEWTAX                                                     
         JZ    GETSCL06                                                         
         CVD   RE,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  STATAX,DUB                                                       
         MVC   STATAX(2),STATAX+1                                               
         MVI   STATAX+2,C'.'                                                    
                                                                                
GETSCL06 MVC   IOVALS(IOVALL),GSIOVALS                                          
         B     EXITY                                                            
         DROP  R1                                                               
                                                                                
GSWORKD  DSECT                     ** GETSCL LOCAL WORKING STORAGE **           
GSIOVALS DS    XL(IOVALL)                                                       
GSWORKL  EQU   *-GSWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE HAVE PURPOSE CODE                                                    
* ENTRY  R3       A(BUY IDELEM)                                                 
*                                                                               
* EXIT:  CC       EQ  = VALID PURPOSE CODE                                      
*                 NEQ = NOT A VALID PURPOSE CODE                                
*                                                                               
***********************************************************************         
                                                                                
         USING IDELEM,R3                                                        
VALPURP  NTR1  LABEL=NO,WORK=(RC,VPWORKL)                                       
         USING VPWORKD,RC                                                       
         MVC   VPIOVALS,IOVALS                                                  
                                                                                
         LA    R1,IOKEY                                                         
         USING PRPKEY,R1           READ PURPOSE CODE KEY                        
         XC    IOKEY,IOKEY                                                      
         MVI   PRPKTYP,PRPKTYPQ    X'0D'                                        
         MVI   PRPKSUB,PRPKSUBQ    X'19'                                        
         MVC   PRPKAGY,AGENCY                                                   
         MVC   PRPKMED,QMEDA                                                    
         MVC   PRPCODE,IDCONNO                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO3'                            
         MVC   IOVALS(IOVALL),VPIOVALS                                          
         J     EXIT                EXIT WITH CC                                 
         DROP  RC,R3,R1                                                         
VPWORKD  DSECT                     ** VALPURP LOCAL WORKING STORAGE **          
VPIOVALS DS    XL(IOVALL)                                                       
VPWORKL  EQU   *-VPWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT BUY ORBIT VALUES INTO ORBVALS                    *         
***********************************************************************         
                                                                                
GETORB   NTR1  LABEL=NO                                                         
                                                                                
         LA    R2,0(R1)                                                         
         USING BUYREC,R2                                                        
         STCM  R1,8,CALLMODE                                                    
                                                                                
         TM    CALLMODE,X'80'      TEST ELEMENT ADDRESS PASSED                  
         JNZ   GETORB04                                                         
                                                                                
         LA    R2,BDELEM           LOCATE ORBIT ELEMENT (X'67')                 
         USING ORBELEM,R2                                                       
GETORB02 LLC   R3,ORBLEN           BUMP TO NEXT ELEMENT                         
         AR    R2,R3                                                            
         CLI   ORBELEM,EOR         TEST END OF RECORD                           
         JE    GETORBX                                                          
         CLI   ORBELEM,X'67'       TEST ORBIT ELEMENT                           
         JNE   GETORB02                                                         
                                                                                
GETORB04 ST    R2,AORBTEL          SAVE A(ORBIT ELEMENT FOR LATER)              
         LA    R3,ORBVDAY          EXTRACT ORBIT VALUES                         
         USING ORBVDAY,R3                                                       
                                                                                
         LLC   R4,ORBLEN                                                        
         SHI   R4,ORBDAY-ORBELEM                                                
         SRL   R4,4                DIVIDE BY 16                                 
         LTR   R4,R4               EXIT IF NONE THERE                           
         JZ    GETORBX                                                          
         CHI   R4,ORBMAXN                                                       
         JH    *+2                                                              
         STCM  R4,3,ORBVALN        SET NUMBER OF ORBITS TO WHAT'S THERE         
         LA    R6,ORBDAY           AND POINT TO FIRST ONE                       
         DROP  R2                                                               
                                                                                
         USING ORBDAY,R6                                                        
GETORB06 MVC   ORBVDAY,ORBDAY      SET DEMO CODE                                
         MVC   ORBVSTM(L'ORBTIME),ORBTIME   COPY START AND END TIME             
         MVC   ORBVPRG,ORBDESC                                                  
                                                                                
         XC    ORBVDMV,ORBVDMV                                                  
         MVC   ORBVDMV+2(2),ORBDEM 3 BYTE DEMO VALUE                            
         NI    ORBVDMV+2,FF-NDEMMANQ-NDEM2DEC TAKE OFF ALL FLAG BITS            
                                                                                
         TM    ORBDEM,NDEM2DEC    2 DEC BIT IN 3-BYTE DEMO VALUE?               
         JZ    *+8                                                              
         OI    ORBVDMV,NDEM2DEC                                                 
*&&DO                                                                           
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),ORBDEM                                                 
         TM    ORBDEM,NDEM2DEC                                                  
         JZ    GETORB08                                                         
         NI    FULL+2,FF-NDEM2DEC                                               
         OI    FULL,NDEM2DEC                                                    
GETORB08 GOTOR ADJPRC                                                           
         MVC   ORBVDMV,FULL+2                                                   
*&&                                                                             
         AHI   R6,16               BUMP TO NEXT BUY ORBIT IN ELEM               
         AHI   R3,ORBVLNQ          BUMP TO NEXT OUTPUT ORBIT ENTRY              
         JCT   R4,GETORB06         DO FOR NUMBER OF REQUESTED ORBITS            
                                                                                
GETORBX  B     EXITY                                                            
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT BUY DEMO VALUES INTO SCDDEMV                     *         
***********************************************************************         
                                                                                
GETDEM   NTR1  LABEL=NO                                                         
                                                                                
         LA    R2,0(R1)                                                         
         USING BUYREC,R2                                                        
         STCM  R1,8,CALLMODE       SAVE CALLING MODE                            
                                                                                
         MVI   WORK,NDCORGQ        SET ELEMENT CODE FOR SEARCH                  
         CLC   LP_QMAPN,SDBUYDD#   TEST DOWNLOAD ON EXIT FROM BUY               
         JE    GETDEM02                                                         
                                                                                
         CLI   SPILL,YESQ                                                       
         JNE   GETDEM02                                                         
         MVI   WORK,NDCSPLQ                                                     
                                                                                
GETDEM02 TM    CALLMODE,X'80'      TEST ELEMENT ADDRESS PASSED                  
         JNZ   GETDEM06                                                         
                                                                                
         LA    R2,BDELEM           LOCATE DEMO ELEMENT                          
         USING NDELEM,R2                                                        
GETDEM04 LLC   R3,NDLEN            BUMP TO NEXT ELEMENT                         
         AR    R2,R3                                                            
         CLI   NDCODE,EOR          TEST END OF RECORD                           
         JE    GETDEMX                                                          
         CLC   NDCODE,WORK         TEST CORRECT DEMO ELEMENT                    
         JNE   GETDEM04                                                         
         CLI   SPILL,YESQ          TEST SPILL BUY                               
         JNE   GETDEM06                                                         
         CLC   MKTNUM,NDPROG       YES - TEST CORRECT MARKET                    
         JNE   GETDEM04                                                         
                                                                                
GETDEM06 ST    R2,ADEMOEL          SAVE A(DEMO ELEMENT FOR LATER)               
         LA    R3,SCDDEMV          EXTRACT DEMO VALUES                          
         USING SCDDEMV,R3                                                       
         LLC   RE,NDLEN                                                         
         SHI   RE,NDEMNO-NDELEM                                                 
         SRL   RE,3                                                             
         LTR   RE,RE               EXIT IF NONE THERE                           
         JZ    GETDEMX                                                          
         ST    RE,DUB              SAVE #DEMOS IN ELEMENT                       
                                                                                
*&&DO                                                                           
         TM    CALLMODE,X'40'      TEST SPILL-OUT DEMOS                         
         JNZ   *+14                                                             
*&&                                WE WANT SPILL TO SHOW SAME DEMO CAT          
                                                                                
         CLC   LP_QMAPN,SDBUYDD#   TEST DOWNLOAD ON EXIT FROM BUY               
         JNE   GETDEM08            NO                                           
         STCM  RE,3,SCDDEMN        SET NUMBER OF DEMOS TO WHAT'S THERE          
         LR    R0,RE               USED FOR LATER JCT IN GETDEM16               
         LA    RF,NDEMNO           AND POINT TO FIRST ONE                       
         J     GETDEM10                                                         
                                                                                
GETDEM08 ICM   RE,7,ADEM                                                        
         JZ    GETDEM18                                                         
         LHI   R0,1                                                             
         LA    R1,LW_DATA1-LW_D(RE)                                             
         CLI   LW_TYPE-LW_D(RE),LW_TSINQ                                        
         JE    *+12                                                             
         ICM   R0,3,LW_NUMN-LW_D(RE)                                            
         LA    R1,LW_DATA2-LW_D(RE)                                             
         STCM  R0,3,SCDDEMN        SET N'OUTPUT DEMO VALUES                     
         LTR   R0,R0               TEST ANY DEMOS                               
         JZ    GETDEMX             NO - EXIT                                    
                                                                                
*&&DO                                                                           
GETDEM10 TM    CALLMODE,X'40'      TEST SPILL-OUT DEMOS                         
         JNZ   GETDEM14            YES - DON'T DO DEMO SEARCH                   
*&&                                                                             
GETDEM10 DS    0H                  WE WANT SPILL TO SHOW SAME DEMO CAT          
         CLC   LP_QMAPN,SDBUYDD#   TEST DOWNLOAD ON EXIT FROM BUY               
         JE    GETDEM14            YES - DON'T DO DEMO SEARCH                   
                                                                                
         L     RE,DUB                                                           
         LA    RF,NDEMNO                                                        
         USING NDEMNO,RF                                                        
GETDEM12 CLC   NDEMNO,0(R1)        MATCH REQUEST DEMO TO BUY DEMO               
         JE    GETDEM14                                                         
         AHI   RF,NDEMLNQ                                                       
         JCT   RE,GETDEM12                                                      
         MVC   SCDDEMO,0(R1)       DEMO NOT IN BUY, SET DEMO CODE STILL         
         J     GETDEM16                                                         
                                                                                
GETDEM14 MVC   SCDDEMO,NDEMNO      SET DEMO CODE                                
*&&DO                                                                           
         MVC   FULL,NDEMRAW        SET DEMO VALUE                               
         NI    FULL,FF-NDEMMANQ                                                 
         GOTOR ADJPRC              ADJUST DEMO PRECISION                        
         MVC   SCDDEM#,FULL                                                     
*&&                                                                             
         MVC   SCDDEM#,NDEMRAW                                                  
         TM    NDEMRAW,NDEMMANQ    TEST MANUAL OVERRIDE                         
         JZ    GETDEM16                                                         
         MVI   SCDDEMF,YESQ        SET DEMO OVERRIDE FLAG                       
                                                                                
GETDEM16 AHI   R1,L'NDEMNO         BUMP TO NEXT REQUEST DEMO                    
         AHI   RF,NDEMLNQ          BUMP TO NEXT BUY DEMO                        
         AHI   R3,L'SCDDEMV        BUMP TO NEXT OUTPUT DEMO VALUE               
         JCT   R0,GETDEM10         DO FOR NUMBER OF REQUESTED DEMOS             
         DROP  R3,RF                                                            
                                                                                
GETDEM18 L     R2,ADEMOEL                                                       
         LLC   R1,NDLEN                                                         
         AR    R1,R2                                                            
         USING PDELEM,R1           R1=A(NEXT ELEMENT ON RECORD)                 
         CLI   NDCODE,NDCORGQ      TEST ORIGINAL DEMO ELEMENT                   
         JNE   GETDEM20                                                         
         CLI   PDELEM,PDELEMQ      YES - TEST POST BUY OVERRIDE                 
         JNE   GETDEMX                                                          
         LA    R1,PDEMO            YES - POINT TO OVERRIDE LIST                 
         J     GETDEM22                                                         
                                                                                
         USING SDELEM,R1                                                        
GETDEM20 CLI   NDCODE,NDCSPLQ      TEST SPILL DEMO ELEMENT                      
         JNE   GETDEMX                                                          
         CLI   SDELEM,SDELEMQ      YES - TEST POST BUY SPILL OVERRIDE           
         JNE   GETDEMX                                                          
         LA    R1,SDEMO                                                         
                                                                                
GETDEM22 LA    RF,NDEMNO                                                        
         USING NDEMNO,RF           RF=A(DEMO LIST)                              
         LA    RE,PBDVALS                                                       
         USING PBDVALS,RE          RE=A(OUTPUT OVERRIDE LIST)                   
         ICM   R0,15,DUB           R0=NUMBER OF DEMOS IN LIST                   
         JZ    *+2                                                              
                                                                                
GETDEM24 TM    0(R1),NDEMMANQ      TEST OVERRIDE DEMO                           
         JZ    GETDEM26                                                         
         MVC   PBDDEMO,NDEMNO      YES - BUILD OVERRIDE ENTRY                   
         XC    PBDDEM#,PBDDEM#                                                  
         MVC   PBDDEM#+1(3),0(R1)  3 BYTE DEMO VALUE                            
         NI    PBDDEM#+1,FF-NDEMMANQ-NDEM2DEC   TAKE OFF ALL FLAG BITS          
                                                                                
         TM    0(R1),NDEM2DEC     2 DEC BIT IN 3-BYTE DEMO VALUE?               
         JZ    *+8                                                              
         OI    PBDDEM#,NDEM2DEC                                                 
*&&DO                                                                           
         ST    RE,SAVERE           SAVE RE                                      
         GOTOR ADJPRC                                                           
         L     RE,SAVERE                                                        
         MVC   PBDDEM#,FULL+1                                                   
*&&                                                                             
         AHI   RE,L'PBDDEMV        BUMP TO NEXT OUTPUT ENTRY                    
                                                                                
GETDEM26 AHI   RF,NDEMLNQ          BUMP TO NEXT DEMO                            
         AHI   R1,L'PDEMO          BUMP TO NEXT DEMO VALUE                      
         JCT   R0,GETDEM24         DO FOR NUMBER OF DEMOS                       
         MVI   PBDVALS,FF                                                       
                                                                                
GETDEMX  B     EXITY                                                            
         DROP  R1,R2,RE,RF                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT BUY PACKAGE VALUES INTO PKGVALS                  *         
***********************************************************************         
                                                                                
GETPKG   NTR1  LABEL=NO                                                         
                                                                                
         LA    R2,0(R1)                                                         
         USING BUYREC,R2                                                        
                                                                                
         LA    R2,BDELEM           LOCATE PACKAGE ELEM (X'05')                  
         USING PKGELEM,R2                                                       
GETPKG02 LLC   R3,PKGLEN           BUMP TO NEXT ELEMENT                         
         AR    R2,R3                                                            
         CLI   PKGCODE,EOR         TEST END OF RECORD                           
         JE    GETPKGX                                                          
         CLI   PKGCODE,PKGCODEQ    X'05'                                        
         JNE   GETPKG02                                                         
                                                                                
         MVC   PKGTYPE,PKGIND      EXTRACT PACKAGE VALUES                       
                                                                                
         LLC   R4,PKGLEN                                                        
         SHI   R4,PKGLINES-PKGELEM+1  EX LENGTH                                 
         JM    *+2                                                              
                                                                                
         CHI   R4,MXPKGLNS         MORE THAN MXPKGLNS?                          
         JNL   *+2                  YES                                         
                                                                                
         BASR  RE,0                                                             
         MVC   PKGLINS(0),PKGLINES                                              
         EX    R4,0(RE)                                                         
         AHI   R4,1                                                             
         STCM  R4,3,PKGNLIN        STORE # OF LINES REFERENCED                  
         DROP  R2                                                               
                                                                                
GETPKGX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADJUST DEMO PRECISION TO 1 OR TWO DECIMAL PLACES BASED   *         
* ON VALUE ITSELF AND PROFILE OPTION - VALUE PASSED IN FULL(4)        *         
***********************************************************************         
                                                                                
ADJPRC   STM   RE,R2,12(RD)                                                     
         L     R2,LP_ATWA                                                       
         USING TWAD,R2                                                          
                                                                                
         TM    FULL,NDEM2DEC       TEST DEMO AT 2 DECIMAL PLACES                
         JNZ   ADJPRC02            YES                                          
*                                  HAVE 1 DECIMAL PREC DEMO                     
         BRAS  RE,CHKPRC           DOES AGENCY USE 2 DECIMAL PREC               
         JNE   ADJPRCX              NO - LEAVE IT ALONE                         
         L     R0,FULL             ADJUST 1 DECIMAL PRECISION TO 2              
         MHI   R0,10                                                            
         ST    R0,FULL                                                          
         J     ADJPRCX                                                          
*                                  HAVE 2 DECIMAL PREC DEMO                     
ADJPRC02 NI    FULL,FF-NDEM2DEC    TURN OFF 2 DECIMAL PLACE FLAG                
         BRAS  RE,CHKPRC           TEST AGENCY USES 2 DECIMAL PREC              
         JNE   ADJPRC04             NO, ADJUST 2 DECMAL PRECISION TO 1          
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE SBTK VERSION?                           
         JZ    ADJPRCX              -NO, LEAVE ALONE                            
         CLC   LP_VRSN,V470037     SBTK >= V4.7.0.37?                           
         JNL   ADJPRCX              -YES, LEAVE ALONE                           
*                                                                               
         CLI   QMEDA,C'R'          MEDIA R?                                     
         JNE   ADJPRCX              NO, LEAVE IT ALONE                          
         LA    RE,10               RE = ADJUST VALUE BACK TO 2-DEC              
         J     *+8                                                              
*                                                                               
ADJPRC04 LA    RE,1                RE = ADJUST VALUE NONE                       
         L     R0,FULL             ADJUST 2 DECIMAL PRECISION TO 1              
         SRDA  R0,31               R1=2*FULL                                    
         LHI   RF,10                                                            
         DR    R0,RF                                                            
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
         MR    R0,RE               ADJUST VALUE IF NECESSARY                    
*                                                                               
         ST    R1,FULL                                                          
*                                                                               
ADJPRCX  LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
*                                                                               
CHKPRC   SR    R1,R1                                                            
*                                                                               
         OC    LP_VRSN,LP_VRSN     HAVE SBTK VERSION?                           
         JZ    *+14                 -NO                                         
         CLC   LP_VRSN,V470037     SBTK < V4.7.0.37?                            
         JL    CHKPRCR              -YES, ALWAYS CHECK 00 RATING PROF           
*                                                                               
         CLI   QMEDA,C'R'          IF MEDIA R                                   
         JNE   CHKPRC05                                                         
         LA    R1,=C'N'            DOES NOT SUPPORT 2-DEC RATS/IMPS             
         J     CHKPRCX                                                          
*                                                                               
CHKPRC05 CLI   QMEDA,C'X'          FOR MEDIA X                                  
         JNE   CHKPRC10                                                         
         LA    R1,=C'Y'                                                         
         CLI   SVDEMTYP,C'R'       IF RATING                                    
         JE    CHKPRCX              SUPPORT 2-DEC RATS                          
         CLI   SVDEMTYP,C'E'       OR EXTENDED                                  
         JE    CHKPRCX              SUPPORT 2-DEC RATS                          
         LA    R1,=C'N'            ELSE, SUPPORT 1-DEC IMPS                     
         J     CHKPRCX                                                          
*                                                                               
CHKPRC10 CLI   QMEDA,C'T'          IF MEDIA T                                   
         JNE   *+2                                                              
         CLI   SVDEMTYP,C'R'       RATING                                       
         JE    CHKPRCR                                                          
         CLI   SVDEMTYP,C'E'       EXTENDED                                     
         JNE   CHKPRCI                                                          
*                                                                               
CHKPRCR  DS    0H                  HAVE RATING/EXTENDED DEMO                    
         CLI   SVDEMORG,SVDMOGD    GOAL DEMO?                                   
         JNE   *+8                  NO                                          
         LA    R1,PROFG0V+7         YES, R1=A(2 DECIMAL GOAL RTG)               
         CLI   SVDEMORG,SVDMOBD    BUY DEMO?                                    
         JNE   *+8                  NO                                          
CHKPRCRZ LA    R1,SVS002DP          YES, R1=A(2 DECIMAL BUY RTG)                
         J     CHKPRCX                                                          
*                                                                               
CHKPRCI  DS    0H                  HAVE IMPRESSION DEMO                         
         CLI   SVDEMORG,SVDMOGD    GOAL DEMO?                                   
         JNE   *+8                  NO                                          
         LA    R1,PROFG0V+8         YES, R1=A(2 DECIMAL GOAL IMP)               
         CLI   SVDEMORG,SVDMOBD    BUY DEMO?                                    
         JNE   *+8                  NO                                          
         LA    R1,SVS00A2DI         YES, R1=A(2 DECIMAL BUY IMP)                
CHKPRCX  CLI   0(R1),C'Y'          TEST 2-DEC PRECISION?                        
         BR    RE                   AND RETURN CC                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ESTABLISH BUY COSTS (SCDCOST1/SCDCOST2/SCDC2FAC)         *         
*                                                                     *         
* NTRY:- R1=A(BUY RECORD)                                             *         
***********************************************************************         
                                                                                
         USING BUYREC,R1                                                        
GETCST   SR    R0,R0                                                            
         ICM   R0,7,BDCOST         R1=COST (DOLLARS OR PENNIES)                 
         TM    BDCIND,BDCMINSQ     TEST MINUS RATE                              
         JZ    *+6                                                              
         LNR   R0,R0                                                            
         TM    BDCIND2,BDCNBRDQ    DEFAULT IS PENNIES                           
         JZ    *+8                 UNLESS THE INDICATOR SAYS NOT                
         MHI   R0,100              CONVERT TO PENNIES                           
         CVD   R0,DUB                                                           
         ZAP   SCDCOST1,DUB        SET COST VALUE                               
                                                                                
*** THIS LINE IS BAD ** WHY DID WE DO THIS!! **                                 
***NOP   ZAP   SCDCOST2,DUB        SET COST2 VALUE = COST                       
*** THIS LINE IS BAD ** WHY DID WE DO THIS!! **                                 
                                                                                
         LA    RF,BDELEM           LOCATE COST2 ELEMENTS ON BUY                 
         USING COS2ELEM,RF                                                      
         SR    R0,R0                                                            
GETCST02 CLI   COS2ELEM,EOR                                                     
         JE    GETCSTX                                                          
         CLI   COS2ELEM,COS2ELQ                                                 
         JE    GETCST06                                                         
         CLI   COS2ELEM,C2FCELQ                                                 
         JE    GETCST08                                                         
                                                                                
GETCST04 LLC   R0,COS2ELEM+1                                                    
         AR    RF,R0                                                            
         J     GETCST02                                                         
                                                                                
GETCST06 ICM   R0,15,2(RF)         SET COST 2 VALUE                             
         TM    BDCIND,BDCMINSQ     TEST MINUS RATE                              
         JZ    *+6                                                              
         LNR   R0,R0                                                            
         TM    BDCIND2,BDCNBRDQ    DEFAULT IS PENNIES                           
         JZ    *+8                 UNLESS THE INDICATOR SAYS IN $               
         MHI   R0,100              CONVERT TO PENNIES                           
         CVD   R0,DUB                                                           
         ZAP   SCDCOST2,DUB                                                     
         J     GETCST04                                                         
                                                                                
GETCST08 MVC   SCDC2FAC,2(RF)      SET COST 2 FACTOR                            
         J     GETCST04                                                         
                                                                                
GETCSTX  BR    RE                                                               
         DROP  R1,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET STATION CALL LETTERS                                 *         
*                                                                     *         
* NTRY:- R1=A(COMPRESSED MARKET/STATION)                              *         
***********************************************************************         
                                                                                
GETSTA   NTR1  LABEL=NO                                                         
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         L     RF,AAGYREC                                                       
         MVC   STAPCTRY,AGYPCNDA-AGYHDR(RF)                                     
         MVC   STAPMED,BUYMED                                                   
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,0(R1)                                                   
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPMED,RADMEDQ     TEST RADIO                                   
         JE    GETSTA02            LEAVE IT AS IS                               
         CLC   STAPQNET,SPACES     TEST CABLE CHANNEL SET                       
         JE    *+8                                                              
         MVI   STAPQSTA+L'STAPQSTA-1,C'/'                                       
         J     GETSTAX                                                          
*                                                                               
GETSTA02 LLC   R0,STAPQSTA+L'STAPQSTA-1                                         
         LA    RE,STAPQSTA+L'STAPQSTA-2                                         
         CLI   0(RE),C' '                                                       
         JH    *+8                                                              
         JCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
         STC   R0,2(RE)                                                         
         MVI   3(RE),C' '                                                       
*                                                                               
GETSTAX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET CHECK SUM FOR A SPTFIL RECORD                                   *         
***********************************************************************         
                                                                                
GETSUM   STM   RE,R2,12(RD)                                                     
         LR    R2,R1               R2=A(CHECK SUM DESTINATION FIELD)            
         L     RE,IOADDR                                                        
         SR    RF,RF                                                            
         ICM   RF,3,BUYRLEN-BUYREC(RE)                                          
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         STCM  R0,15,0(R2)                                                      
         LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* READ DAYPART RECORD AND BUILD DAYPART TABLE                         *         
***********************************************************************         
                                                                                
GETDPT   CLI   DPTCODE,0           TEST DAYPART CODE ESTABLISHED                
         BE    NOMORE                                                           
                                                                                
         LA    R3,DPTTAB                                                        
         ST    R3,LP_ADATA         SET A(DAYPART TABLE)                         
         USING DPTTABD,R3          R3=A(OUTPUT TABLE)                           
                                                                                
         LA    R2,IOKEY                                                         
         USING DPTHDR,R2           BUILD KEY OF DAYPART MENU RECORD             
         XC    DPTKEY,DPTKEY                                                    
         MVI   DPTKTYPE,DPTKTYPQ                                                
         MVC   DPTKAGY,LP_AGY                                                   
         MVC   DPTKMED,QMEDA                                                    
         MVC   DPTKMENU,DPTCODE                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         JE    GETDPT02                                                         
         MVC   DPTKEY,IOKEYSAV     NO FOUND - READ ALL AGENCY DEFAULT           
         MVC   DPTKAGY,DEFAGY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         BNE   NOMORE                                                           
                                                                                
GETDPT02 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         JNE   *+2                                                              
                                                                                
         L     R2,IOADDR           R2=A(DAYPART MENU RECORD)                    
         LA    R4,DPTCODES         R4=A(DAYPART LIST)                           
GETDPT04 CLI   0(R4),0             TEST END OF DAYPART LIST                     
         BE    EXITY                                                            
         MVC   DPTTALPH,0(R4)                                                   
         MVC   DPTTMSCD,1(R4)                                                   
         MVC   DPTTNAME,2(R4)                                                   
         TM    DPTTMSCD,X'F0'                                                   
         JZ    GETDPT08                                                         
         MVC   BYTE1,DPTTMSCD                                                   
         NI    BYTE1,X'F0'                                                      
                                                                                
         LA    R1,DPTCODES         LOOK-UP MASTER DAYPART CODE IN LIST          
GETDPT06 CLI   0(R1),0                                                          
         JE    GETDPT08                                                         
         MVC   BYTE2,1(R1)                                                      
         NI    BYTE2,X'F0'                                                      
         CLC   BYTE1,BYTE2         MATCH ON MASTER DAYPART NUMBER               
         JE    *+12                                                             
         AHI   R1,5                                                             
         J     GETDPT06                                                         
         CR    R1,R4               TEST POINTING TO ITSELF                      
         JE    GETDPT08                                                         
         MVC   DPTTMAST,2(R1)      NO - SET MASTER DAYPART                      
                                                                                
GETDPT08 AHI   R4,5                BUMP TO NEXT DAYPART LIST ENTRY              
         AHI   R3,DPTTABL          BUMP TO NEXT DAYPART TABLE ENTRY             
         J     GETDPT04                                                         
         DROP  R2,R3                                                            
         EJECT                                                                  
         LKARY T                                                                
                                                                                
GETTIME  LR    R2,RE               GET CURRENT TIME IN TU'S IN FULL             
         TIME  TU                                                               
         ST    R0,FULL                                                          
         LR    RE,R2                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
REVCOMP  L     R1,LP_AINP          COMPLEMENT REVISION NUMBER                   
         MVC   WORK(L'DRVKREVS),0(R1)                                           
         XC    WORK(L'DRVKREVS),EFFS                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE REQUEST DATES                                            *         
***********************************************************************         
                                                                                
INIDAT   NTR1  LABEL=NO                                                         
         OC    QSTRDATE,QSTRDATE   SET START DATES                              
         JZ    INIDAT02                                                         
         GOTOR VDATCON,DMCB,QSTRDATE,(3,STRDATEB)                               
         GOTOR (RF),(R1),QSTRDATE,(2,STRDATEC)                                  
         GOTOR VGETDAY,DMCB,QSTRDATE,WORK                                       
         MVC   STRDMONC,STRDATEC                                                
         CLI   0(R1),1             TEST START DATE IS A MONDAY                  
         JE    INIDAT02                                                         
         SR    R0,R0               NO - GET DATE OF PREVIOUS MONDAY             
         ICM   R0,1,0(R1)                                                       
         JZ    *+2                                                              
         SHI   R0,1                                                             
         LNR   R0,R0                                                            
         GOTOR VADDAY,DMCB,QSTRDATE,WORK,(R0)                                   
         GOTOR VDATCON,DMCB,WORK,(2,STRDMONC)                                   
                                                                                
INIDAT02 CLC   QENDDATE,EFFS       SET END DATES                                
         JE    INIDATX                                                          
         GOTOR VDATCON,DMCB,QENDDATE,(3,ENDDATEB)                               
         GOTOR (RF),(R1),QENDDATE,(2,ENDDATEC)                                  
                                                                                
INIDATX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD MARKET/STATION DRIVER ENTRIES                                 *         
***********************************************************************         
                                                                                
BLDMKS   CLI   STAIND,LW_TSINQ     TEST SINGLE STATION REQUEST                  
         JNE   BLDMKS02                                                         
         L     RF,LP_AWMP                                                       
         USING LW_D,RF                                                          
         STCM  RF,7,AMKT           YES - CONVERT TO SINGLE MARKET               
         MVI   MKTIND,LW_TSINQ                                                  
         MVI   LW_TYPE,LW_TSINQ                                                 
         ICM   R1,7,ASTA                                                        
         MVC   LW_DATA1(L'BUYKMKTN),LW_DATA1-LW_D(R1)                           
         AHI   RF,LW_LN1Q+L'BUYKMKTN                                            
         ST    RF,LP_AWMP                                                       
                                                                                
BLDMKS02 MVC   ASTA,ANZR           DEFAULT STATION VALUE                        
                                                                                
BLDMKSX  BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO TSAR (ONLINE) OR BUFFERIN (OFFLINE)                    *         
***********************************************************************         
                                                                                
         USING BUFFD,FLMBUF                                                     
BUFFER   NTR1  LABEL=NO                                                         
         LR    R2,R1                                                            
         MVC   TSACTN,0(R2)                                                     
                                                                                
         CLI   TSACTN,TSAINI       TEST INITIALIZATION CALL                     
         JNE   BUFFER02                                                         
         XC    TSARBLK,TSARBLK     CLEAR TSAR BLOCK                             
         MVI   TSACTN,TSAINI                                                    
         LA    R0,TSARREC                                                       
         ST    R0,TSAREC                                                        
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGN,8                                                         
         OI    TSINDS,TSINODSK+TSIXTTWA                                         
         MVC   TSKEYL,4(R2)                                                     
         MVC   TSRECL,6(R2)                                                     
                                                                                
BUFFER02 TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   BUFFER04            YES                                          
         GOTOR VTSAR,TSARD         ELSE USE TSAR                                
         BE    EXIT                                                             
         CLI   TSERRS,TSEDUP       ALLOW DUPLICATE KEY ERROR                    
         B     EXIT                                                             
                                                                                
BUFFER04 LHI   R0,BUFFAINI         CONVERT TSAR ACTION CODE TO BUFFERIN         
         CLI   TSACTN,TSAINI                                                    
         JNE   BUFFER06                                                         
         LLC   RE,TSKEYL                                                        
         SR    RF,RF                                                            
         ICM   RF,3,TSRECL                                                      
         SR    RF,RE                                                            
         STCM  RE,3,BUFFLKEY       SET KEY LENGTH                               
         STCM  RF,3,BUFFLCOM       SET COMMENT LENGTH                           
         J     BUFFER08                                                         
                                                                                
BUFFER06 LHI   R0,BUFFAPUT                                                      
         CLI   TSACTN,TSAADD                                                    
         JE    BUFFER08                                                         
         CLI   TSACTN,TSAWRT                                                    
         JE    BUFFER08                                                         
         LHI   R0,BUFFASEQ                                                      
         CLI   TSACTN,TSANXT                                                    
         JE    BUFFER08                                                         
         MVC   TSARSAV,TSARREC                                                  
         LHI   R0,BUFFARDH                                                      
         CLI   TSACTN,TSARDH                                                    
         JNE   *+2                                                              
                                                                                
BUFFER08 GOTOR ABUFFRIN,DMCB,((R0),BUFFD),TSARREC,ACOMFACS                      
         CLI   TSACTN,TSARDH                                                    
         JNE   BUFFER10                                                         
         LLC   RF,TSKEYL                                                        
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         CLC   TSARREC(0),TSARSAV                                               
         EX    RF,0(RE)                                                         
         JE    BUFFER10                                                         
         OI    BUFFERRS-BUFFPARM(R1),BUFFERNF                                   
                                                                                
BUFFER10 MVC   TSERRS,BUFFERRS-BUFFPARM(R1)                                     
         CLI   TSERRS,0                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESOLVE CLIENT DETAILS FOR BUY DOWNLOADS                            *         
***********************************************************************         
                                                                                
RESCLT   NTR1  LABEL=NO                                                         
         GOTOR (#GETCLT,AGETCLT)                                                
         BNE   EXIT                                                             
         GOTOR (#EDTCLT,AEDTCLT),DMCB,QCLTX,,QCLTA                              
         L     RF,ACLTREC                                                       
         USING CLTRECD,RF                                                       
         MVC   CLTNAME,CNAME                                                    
         MVC   CLTOFF,COFFICE                                                   
         MVI   CLTBPOL,0                                                        
         OC    CCOST2,CCOST2       TEST COST2 PRESENT                           
         JZ    RESCLT02                                                         
         ZAP   CLTC2FAC,PZERO                                                   
         TM    CCOST2,X'80'        TEST INPUT BUT ZERO                          
         JNZ   RESCLT02                                                         
         ICM   R0,15,CCOST2                                                     
         CVD   R0,DUB                                                           
         ZAP   CLTC2FAC,DUB                                                     
                                                                                
RESCLT02 CLI   CPROF,C'0'                                                       
         JE    *+8                                                              
         MVI   CLTBPOL,YESQ        SET CLIENT IS BRAND/POOL                     
         TM    COPT1,COP1COSQ      SET COST2 REQUIRED INDICATOR                 
         JZ    *+8                                                              
         OI    CLT#C2RQ,CLTIC2RQ                                                
         TM    COPT2,COP2FRZ       SET FROZEN CLIENT INDICATOR                  
         JZ    *+8                                                              
         OI    CLT#FRZN,CLTIFRZN                                                
         TM    CLOCKMON,X'80'      SET FROZEN MONTH PRIOR                       
         JZ    *+8                                                              
         OI    CLT#FPRI,CLTIFPRI                                                
         TM    CLOCKMON,X'40'      SET FROZEN MONTH SUBSEQUENT                  
         JZ    *+8                                                              
         OI    CLT#FSUB,CLTIFSUB                                                
         TM    COPT3,COP3COSQ      SET COST2 OPTIONAL INDICATOR                 
         JZ    *+8                                                              
         OI    CLT#C2OP,CLTIC2OP                                                
         TM    COPT4,COP4TRD       SET COST2 TRADE INDICATOR                    
         JZ    *+8                                                              
         OI    CLT#C2TR,CLTIC2TR                                                
         TM    COPT4,COP4MIDS      SET MIDAS BARTER INDICATOR                   
         JZ    *+8                                                              
         OI    CLT#MIDS,CLTIMIDS                                                
         CLI   CEXTRA+5,YESQ       SET US SPILL INDICATOR                       
         JNE   *+8                                                              
         OI    CLT#USSP,CLTIUSSP                                                
         CLI   CEXTRA+8,YESQ       SET GOAL REQUIRED INDICATOR                  
         JNE   *+8                                                              
         OI    CLT#GLRQ,CLTIGLRQ                                                
         CLI   CEXTRA+10,YESQ      SET OUT OF WEEK INDICATOR                    
         JNE   *+8                                                              
         OI    CLT#OOWC,CLTIOOWC                                                
         MVC   CLTADJC,CPROF+9     SET PROGRAM ADJACENCY CONTROL                
         MVC   CLTRCTL,CPROF+14    SET RATE CONTROL                             
         MVC   CLTIDRQ,CEXTRA+2    SET BUY ID REQUIRED                          
         MVC   CLTFRZMO,CLOCKYM    SET FROZEN MONTH                             
         NI    CLTFRZMO+1,FF-X'C0'                                              
         MVC   CLTACCOF,CACCOFC    SET CLIENT ACCOUNTING OFFICE                 
         MVC   CLTACCAG,CACCAGY    SET CLIENT AGENCY OVERRIDE                   
                                                                                
         CLC   LP_QMAPN,SDBUYD#    TEST BUY/GOAL DOWNLOAD                       
         JE    *+14                  OR                                         
         CLC   LP_QMAPN,SDNBUYD#        NEW BUY/GOAL DOWNLOAD                   
         JNE   RESCLT06                                                         
                                                                                
         XC    WORK,WORK           BUILD PROFILE KEY & GET PROFILES             
         MVC   WORK+04(L'AGENCY),AGENCY                                         
         MVC   WORK+06(L'QMEDA),QMEDA                                           
         MVC   WORK+07(L'QCLTA),QCLTA                                           
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(L'CLTOFF),CLTOFF                                         
         L     RF,VGETPROF                                                      
         LA    R2,PROFKSDB         R2=A(PROGRAM KEY LIST)                       
         LA    R3,PROFV            R3=A(PROFILES VALUES)                        
         LHI   R0,PROFNSDB         R0=NUMBER OF PROFILES TO LOOK UP             
RESCLT04 MVC   WORK(L'PROFKSDB),0(R2)                                           
         GOTOR (RF),DMCB,WORK,(R3),VDATAMGR                                     
         AHI   R2,L'PROFKSDB                                                    
         AHI   R3,L'PROFV                                                       
         JCT   R0,RESCLT04                                                      
                                                                                
         XC    WORK,WORK           G0 PROFILE IS SPECIAL                        
         XC    PROFG0V,PROFG0V                                                  
         MVC   WORK(4),=C'S0G0'                                                 
         MVC   WORK+4(L'AGENCY),AGENCY                                          
         MVC   WORK+6(4),=X'FFFFFFFF'                                           
         GOTOR (RF),DMCB,WORK,PROFG0V,VDATAMGR                                  
                                                                                
RESCLT06 LA    RF,IOKEY                                                         
         USING EQUHDR,RF           READ CLIENT EQUIVALENCY RECORD               
         XC    EQUKEY,EQUKEY                                                    
         MVI   EQUKTYPE,EQUKTYPQ                                                
         MVC   EQUKAGY,AGENCY                                                   
         MVC   EQUKMED,QMEDA                                                    
         MVC   EQUKCLT(L'QCLTX),QCLTX                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO7'                               
         JE    RESCLT08                                                         
         LA    RF,IOKEY            READ ALL CLIENT DEFAULT RECORD               
         MVC   EQUKEY,IOKEYSAV                                                  
         XC    EQUKCLT,EQUKCLT                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO7'                               
         JE    RESCLT08                                                         
         LA    RF,IOKEY            READ ALL AGENCY DEFAULT RECORD               
         MVC   EQUKEY,IOKEYSAV                                                  
         MVC   EQUKAGY,EZEROS                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO7'                               
         JNE   RESCLT38                                                         
                                                                                
RESCLT08 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO7'                              
         JNE   *+2                                                              
                                                                                
         L     R1,VSLNTAB          RESOLVED IN SPLNK01                          
         LH    RE,0(R1)            RE = L(EACH AGY/MEDIA ENTRY)                 
         L     RF,2(R1)            RF = A(EOT)                                  
         AR    RF,R1                                                            
         AHI   R1,6                R1 = A(1ST ENTRY)                            
                                                                                
         MVI   BYTE1,C'T'                                                       
         CLI   QMEDA,C'T'                                                       
         JE    RESCLT10                                                         
         CLI   QMEDA,C'N'                                                       
         JE    RESCLT10                                                         
         CLI   QMEDA,C'C'                                                       
         JE    RESCLT10                                                         
                                                                                
         MVI   BYTE1,C'R'                                                       
         CLI   QMEDA,C'R'                                                       
         JE    RESCLT10                                                         
         CLI   QMEDA,C'X'                                                       
         JNE   *+2                                                              
                                                                                
RESCLT10 CLC   =C'00',0(R1)        FOUND DEFAULT ENTRY?                         
         JE    RESCLT12                                                         
         CLC   LP_AGY,0(R1)        NO, MATCH ON AGENCY                          
         JNE   *+14                                                             
RESCLT12 CLC   BYTE1,2(R1)         MATCH ON MEDIA                               
         JE    RESCLT14                                                         
         JXLE  R1,RE,RESCLT10                                                   
         DC    H'0'                                                             
                                                                                
RESCLT14 AHI   R1,4                                                             
         ST    R1,ASLNTAB          AND SET AS SLNTAB ADDRESS                    
                                                                                
         L     RF,IOADDR           BUILD EQUIVALENCY TABLE                      
         LA    RF,EQUSECT1                                                      
         LA    R1,CLTEQU           MAX OF 30 ENTRIES                            
         L     RE,ASLNTAB                                                       
         LA    R2,0                START WITH SPOT LENGTH 2                     
RESCLT16 CHI   R2,256              TEST END OF SPOT LENGTH TABLE                
         JH    RESCLT20            LET'S SEE IF WE NEED TO REARRANGE            
                                                                                
         CLI   1(RE),0             NO EQUIVALENCE LENGTH?                       
         JE    RESCLT18            NONE, SKIP THIS LENGTH                       
         CLM   R2,1,1(RE)          MATCH ON EQUIVALENCE LENGTH?                 
         JNE   RESCLT18                                                         
                                                                                
         MVC   0(1,R1),1(RE)                                                    
         LLC   R6,0(RE)            GET D(EQUIVALENCE REC)                       
         AR    R6,RF               POINT TO THE FACTOR                          
         MVC   1(2,R1),0(R6)       SAVE EQUIVALENCE FACTOR                      
                                                                                
         CLI   1(RE),SEC30Q        TEST 30 SECOND ENTRY                         
         JNE   *+10                                                             
         MVC   CLTEQU30,0(R6)      YES - SAVE IT                                
                                                                                
         LA    R1,L'CLTEQU(R1)                                                  
RESCLT18 LA    RE,2(RE)                                                         
         LA    R2,1(R2)                                                         
         J     RESCLT16                                                         
                                                                                
RESCLT20 LA    R1,CLTEQU                                                        
         LA    R2,30               MAX # OF ENTRIES IN CLTEQU                   
         XR    RE,RE                                                            
         XR    RF,RF                                                            
                                                                                
RESCLT22 CLI   0(R1),30            30 SECOND AND FACTOR?                        
         JNE   RESCLT24                                                         
         LR    RE,R1                                                            
         J     RESCLT26                                                         
                                                                                
RESCLT24 CLI   0(R1),60                                                         
         JNE   RESCLT26                                                         
         LR    RF,R1                                                            
         J     RESCLT28                                                         
                                                                                
RESCLT26 LA    R1,L'CLTEQU(R1)                                                  
         JCT   R2,RESCLT22                                                      
                                                                                
RESCLT28 CLC   1(2,RE),1(RF)       30 SEC FACTOR = 60 SEC FACTOR?               
         JNE   RESCLT32            NO                                           
         LA    R1,CLTEQU           YES, CHANGE ALL FACTORS TO 1000              
         LA    R2,30                                                            
RESCLT30 CLI   0(R1),0                                                          
         JE    RESCLT38                                                         
         MVC   1(2,R1),=H'1000'                                                 
         LA    R1,L'CLTEQU(R1)                                                  
         JCT   R2,RESCLT30                                                      
         J     RESCLT38                                                         
                                                                                
RESCLT32 CLC   1(2,RE),=H'1000'                                                 
         JNE   RESCLT34                                                         
         LR    R2,RE                                                            
         J     RESCLT36                                                         
                                                                                
RESCLT34 CLC   1(2,RF),=H'1000'                                                 
         JNE   RESCLT38                                                         
         LR    R2,RF                                                            
                                                                                
RESCLT36 LA    R1,CLTEQU           1ST ENTRY IN CLTEQU                          
         XC    0(3,R1),0(R2)       SWAP ENTRIES                                 
         XC    0(3,R2),0(R1)                                                    
         XC    0(3,R1),0(R2)                                                    
                                                                                
RESCLT38 TM    MAPI1,MAPIORDD      TEST ORDER DOWNLOAD                          
         JNZ   RESCLTX                                                          
         OC    APRD,APRD           TEST PRODUCT DEFINED                         
         JNZ   RESCLT42                                                         
                                                                                
         ICM   RE,7,APRD1                                                       
         JNZ   *+8                                                              
         L     RE,LP_AWMP          NO - BUILD DRIVER ENTRY                      
         USING LW_D,RE                                                          
         STCM  RE,7,APRD1                                                       
         MVI   PRDIND1,LW_TSINQ                                                 
         MVI   LW_TYPE,LW_TSINQ                                                 
         MVI   LW_DATA1,POLPRDQ                                                 
                                                                                
         CLI   CLTBPOL,YESQ        TEST BRAND POOL CLIENT                       
         JNE   RESCLT40                                                         
         MVI   LW_TYPE,LW_TRNGQ                                                 
         MVI   LW_DATA1,1                                                       
         MVI   LW_DATA1+1,POLPRDQ-1                                             
                                                                                
RESCLT40 AHI   RE,LW_LN1Q+2                                                     
         STCM  RE,7,APRD2                                                       
         MVI   LW_TYPE,LW_TRNGQ                                                 
         MVC   LW_DATA1(L'BRCRNG),BRCRNG                                        
         AHI   RE,LW_LN1Q+L'BRCRNG                                              
         CLM   RE,15,LP_AWMP                                                    
         JL    *+8                                                              
         ST    RE,LP_AWMP                                                       
         J     RESCLTX                                                          
                                                                                
RESCLT42 CLC   LP_QMAPN,SDBUYD#    TEST BUY/GOAL DOWNLOAD                       
         JE    *+14                                                             
         CLC   LP_QMAPN,SDNBUYD#   OR NEW BUY/GOAL DOWNLOAD                     
         JNE   RESCLTX                                                          
                                                                                
         MVC   PRDIND1,PRDIND                                                   
         MVC   APRD1,APRD                                                       
         ICM   RF,7,APRD1          YES - BUILD ALPHA PRODUCT ENTRY              
         GOTOR (#EDTPRD,AEDTPRD),DMCB,LW_DATA1-LW_D(RF),,WORK                   
         L     RE,LP_AWMP                                                       
         STCM  RE,7,APRD2                                                       
         MVI   PRDIND2,LW_TSINQ                                                 
         MVI   LW_TYPE,LW_TSINQ                                                 
         MVC   LW_DATA1(L'PKEYPRD),WORK                                         
         AHI   RE,LW_LN1Q+L'PKEYPRD                                             
         ST    RE,LP_AWMP                                                       
                                                                                
RESCLTX  B     EXITY                                                            
         DROP  RE,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY FILTERS TO SPOT ELEMENT                            *         
*                                                                     *         
* NTRY:- R1=A(ELEMENT)                                                *         
***********************************************************************         
                                                                                
         USING REGELEM,R1                                                       
FLTSPT   CLI   RCODE,RCORGQ        TEST FOR SPOT ELEMENTS                       
         BLR   RE                                                               
         CLI   RCODE,X'0D'                                                      
         BHR   RE                                                               
         CLC   RDATE,STRDATEC      TEST SPOT WITHIN REQUEST PERIOD              
         BLR   RE                                                               
         CLC   RDATE,ENDDATEC                                                   
         BHR   RE                                                               
         OC    LP_VRSN,LP_VRSN                                                  
         JZ    *+14                                                             
         CLC   LP_VRSN,V300101                                                  
         JNL   FLTSPT02                                                         
         TM    RSTATUS,RSMINUSQ    DON'T SEND MINUSED SPOTS IF VERSION          
         BNZR  RE                  < 3.0.0.101                                  
                                                                                
FLTSPT02 LLC   R0,SPOTREFN         BUMP SPOT REFERENCE NUMBER                   
         AHI   R0,1                                                             
         STC   R0,SPOTREFN                                                      
                                                                                
         CLI   XPAIDS,0            TEST EXCLUDE PAID FILTER SET                 
         JE    *+12                                                             
         OC    RPAY,RPAY                                                        
         BNZR  RE                                                               
         CLI   XMISSD,0            TEST EXCLUDE MISSED SPOTS                    
         JE    *+10                                                             
         TM    RSTATUS,RSMINSDQ                                                 
         BNZR  RE                                                               
         CLI   XMGOOD,0            TEST EXCLUDE MAKEGOODS                       
         JE    *+10                                                             
         TM    RSTATUS,RSMGONLQ                                                 
         BNZR  RE                                                               
         CLI   XPALOC,0            TEST EXCLUDE PRE-ALLOCATED SPOTS             
         JE    FLTSPTY                                                          
         CLI   RLEN,RLPOL1LQ                                                    
         BNLR  RE                                                               
                                                                                
FLTSPTY  CR    RE,RE                                                            
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY FILTERS TO PRODUCT RECORDS FOR DOWNLOAD. THIS      *         
* FILTERS OUT ANY PRODUCT THAT HAS NO ESTIMATES                       *         
***********************************************************************         
                                                                                
FLTSDB   NTR1  LABEL=NO            We want products that have no est?           
         CLI   ALLPRDS,C'Y'        Yes, that is correct                         
         BE    EXITY                                                            
         MVC   SVIOVALS,IOVALS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         CLC   IOKEY(PKEYPRD+L'PKEYPRD-PRDHDR),IOKEYSAV                         
         MVC   IOVALS(IOVALL),SVIOVALS                                          
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO APPLY FILTERS TO BUY KEY (DROP PASSIVE KEYS/SET SPILL)   *         
***********************************************************************         
                                                                                
FLTBKY   MVC   MKTSTA,IOKEY+(BUYKMKTN-BUYKEY)                                   
         MVI   SPILL,NOQ                                                        
         CLI   IOKEY+(BUYKBUY-BUYKEY),FF                                        
         BER   RE                                                               
         TM    IOKEY+(BUYKBUY-BUYKEY),X'80'                                     
         JZ    FLTBKY02                                                         
         MVI   SPILL,YESQ          SET THIS ONE IS SPILL                        
         CLI   XSPILL,0            TEST EXCLUDING SPILL                         
         BR    RE                                                               
                                                                                
FLTBKY02 CLI   IOKEY+(BUYKBUY-BUYKEY),0                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY FILTERS TO MGREQ KEY                                         
***********************************************************************         
FLTSTQ   ICM   R1,15,ASTEQLST      WE'VE SEEN CLT-SPECIFIC ALREADY?             
         USING LW_D,R1                                                          
         LA    RF,LW_DATA2                                                      
         XR    R0,R0                                                            
         ICM   R0,3,LW_NUMN        # OF CLT-SPECIFIC MGREQ SO FAR               
         BZR   RE                  NONE, PASS FILTER                            
FLTSTQ10 CLC   0(L'STEKSTA,RF),STEKSTA-STEKEY+IOKEY                             
         BE    EXITN               WE'VE ALREADY SEEN THIS STATION              
         LA    RF,L'STEKSTA(RF)                                                 
         JCT   R0,FLTSTQ10                                                      
         DROP  R1                                                               
***********************************************************************         
* ROUTINE TO APPLY FILTERS TO BUY RECORD - TEST ANY SPOT QUALIFIES    *         
***********************************************************************         
                                                                                
         USING BUYREC,R1                                                        
FLTPRD   NTR1  LABEL=NO                                                         
         CLC   BDSTART,ENDDATEB    TEST BUY OVERLAPS PERIOD                     
         BH    EXITN                                                            
         CLC   BDEND,STRDATEB                                                   
         BL    EXITN                                                            
         CLI   XZERO$,0            TEST EXCLUDING ZERO DOLLAR BUYS              
         JE    *+14                                                             
         OC    BDCOST,BDCOST       YES - APPLY COST TEST                        
         BZ    EXITN                                                            
         B     EXITY               NO - DON'T APPLY SPOT FILTERS HERE           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* GET THE ACCESS RECORD                                                         
***********************************************************************         
GETACC   NTR1  LABEL=*                                                          
         MVI   USESODTA,0                                                       
         MVI   OWNRSRCE,0                                                       
                                                                                
         LA    R2,IOKEY                                                         
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
         MVC   CT5KALPH,LP_AGY                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO3'                            
         JNE   GETACCX                                                          
                                                                                
         L     R2,IOADDR                                                        
         LA    R2,CT5DATA          R2=A(FIRST ELEMENT ON ID RECORD)             
         USING CTSYSD,R2                                                        
         SR    R0,R0                                                            
                                                                                
GETACC02 CLI   CTSYSEL,EOR         TEST END OF RECORD                           
         JE    GETACCX                                                          
         CLI   CTSYSEL,CTSYSELQ    X'21' SYSTEM AUTH ELEMENT?                   
         JE    GETACC06                                                         
GETACC04 IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         J     GETACC02                                                         
                                                                                
GETACC06 CLI   CTSYSNUM,2          FOR SPOT?                                    
         JNE   GETACC04            NO                                           
                                                                                
         TM    CTSYSIND,CTSYSRAD   YES, ARE WE AN MSTREET USER?                 
         JZ    *+8                                                              
         MVI   OWNRSRCE,C'S'            YES                                     
*                                                                               
         TM    CTSYSIND,CTSYSMF         ARE WE AN MFW USER?                     
         JZ    *+8                                                              
         MVI   OWNRSRCE,C'F'            YES                                     
*                                                                               
         TM    CTSYSIND,CTSYSOWN        ARE WE AN SRDS USER?                    
         JZ    *+8                                                              
         MVI   OWNRSRCE,C'R'            YES                                     
*                                                                               
         CLI   OWNRSRCE,0          USING ONE OF THE SOURCES?                    
         JE    GETACCX                                                          
         MVI   USESODTA,C'Y'       YES                                          
*                                                                               
GETACCX  B     EXITY                                                            
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
*                                                                               
         DS    0H                                                               
TDLONSPL DC    C'D/L only Spill Stations?'                                      
TEXCLBUY DC    C'Exclude Buys?'                                                 
                                                                                
SETCCC   BE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               CC=EQUAL IF INDICATOR ON                     
         BR    RE                                                               
         LTR   RE,RE               CC=NOT EQUAL IF INDICATOR OFF                
         BR    RE                                                               
                                                                                
MORE     MVI   LP_RMODE,LP_RMORE   TELL DDLINK TO CALL ME AGAIN                 
         B     EXITY                                                            
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS & EXIT                   
         B     EXITY                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     CLEAR OUTPUT FIELD LENGTH & EXIT             
         B     EXITY                                                            
                                                                                
RTRNYES  CR    RE,RE               RETURN WITH CC=EQUAL                         
         BR    RE                                                               
                                                                                
RTRNNO   LTR   RE,RE               RETURN WITH CC=NOT EQUAL                     
         BR    RE                                                               
                                                                                
EXITN    LHI   RE,0                                                             
         B     EXITSET                                                          
EXITY    LHI   RE,1                                                             
EXITSET  CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
SPTPTMAP DC    AL2(86)             SPOT RECORD MAP NUMBER                       
                                                                                
SDSLID#  DC    AL2(I#SDSLID)       STATION LOCKIN DATA DOWNLOAD                 
SDMLID#  DC    AL2(I#SDMLID)       MARKET LOCKIN DATA DOWNLOAD                  
SDCLTD#  DC    AL2(I#SDCLTD)       CLIENT DOWNLOAD                              
SDBRED#  DC    AL2(I#SDBRED)       PRODUCT/ESTIMATE DOWNLOAD                    
SDINID#  DC    AL2(I#SDINID)       INITIAL DOWNLOAD                             
SDMKTD#  DC    AL2(I#SDMKTD)       MARKET DOWNLOAD                              
SDOCOM#  DC    AL2(I#SDOCOD)       OCOM DOWNLOAD                                
SDSCOM#  DC    AL2(I#SDSCOD)       STATION COME DOWNLOAD                        
SDMCOM#  DC    AL2(I#SDMCOD)       MARKET COME DOWNLOAD                         
SDPDV#   DC    AL2(I#SDADBV)       PERSON DIRECTORY VALIDATION                  
SDBOX#   DC    AL2(I#SDBOXD)       BATCH ORDER EXPORT                           
SDSGL#   DC    AL2(I#SDDEVD)       SCHEDULE GUIDELINES DOWNLOAD                 
                                                                                
MAPTAB   DS    0XL4                ** MAP/INDICATORS TABLE **                   
                                                                                
SDBUYD#  DC    AL2(I#SDBUYD)       BUY/GOAL DOWNLOAD                            
         DC    AL1(MAPIBUYD)                                                    
         DC    AL1(MAPIAMCE)                                                    
                                                                                
SDNBUYD# DC    AL2(I#NSDBYD)       NEW BUY/GOAL DOWNLOAD                        
         DC    AL1(MAPIBUYD)                                                    
         DC    AL1(MAPIAMCE)                                                    
                                                                                
SDBUYDD# DC    AL2(I#SDBUYR)       BUY DOWNLOAD BY D/A                          
         DC    AL1(MAPIBUYD)                                                    
         DC    AL1(0)                                                           
                                                                                
MAPTABN  EQU   (*-MAPTAB)/L'MAPTAB                                              
                                                                                
MODLUSA  DC    C'RID',AL1(0)                                                    
MODLCAN  DC    C'ERI',AL1(0)                                                    
                                                                                
BITLIST  DC    X'8040201008040201'                                              
PZERO    DC    P'0'                                                             
BZEROS   DC    X'0000'                                                          
EZEROS   DC    C'00000000'                                                      
HZEROS   DC    X'000000000000'                                                  
DEFAGY   EQU   EZEROS              DEFAULT AGENCY CODE FOR DAYPARTS             
UNKNOWN  DC    C'*Unknown'                                                      
HEX80    DC    X'80'                                                            
HEXC0    DC    X'C0'                                                            
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
STAFIL   DC    C'STATION'                                                       
SPTDIR   DC    C'SPTDIR '                                                       
SPTFIL   DC    C'SPTFILE'                                                       
XSPDIR   DC    C'XSPDIR '                                                       
XSPFIL   DC    C'XSPFILE'                                                       
GENDIR   DC    C'GENDIR '                                                       
GENFIL   DC    C'GENFIL '                                                       
TRFDIR   DC    C'TRFDIR '                                                       
                                                                                
PROFKSDB DS    0CL4                ** BUY PROFILE KEYS **                       
         DC    C'sDAR'                                                          
         DC    C'S000'                                                          
         DC    C'S01W'                                                          
         DC    C'S0B0'                                                          
         DC    C'S0ST'                                                          
         DC    C'S0AJ'                                                          
         DC    C's00A'                                                          
         DC    C'S0BW'                                                          
         DC    C'sBWA'                                                          
         DC    C'S0OM'                                                          
PROFNSDB EQU   (*-PROFKSDB)/L'PROFKSDB                                          
         EJECT                                                                  
PRDFLT   LKKEY H,CLRPRD,SAVED      ** PRODUCT FILTER **                         
         LKKEY WMP,CLRPRD,APRD1                                                 
         LKKEY E                                                                
                                                                                
ERPKEYT  LKKEY H,CTEPKEY           ** EDI REP PARNTER DRIVER **                 
         LKKEY LIT,CTEPKTYP,CTEPKTYQ    X'00'                                   
         LKKEY LIT,CTEPKSTY,CTEPKSTQ    X'39'                                   
         LKKEY LIT,CTEPKSPR,0                                                   
         LKKEY ALL,CTEPKREP                                                     
         LKKEY E                                                                
                                                                                
REPKEYT  LKKEY H,REPKEY            ** SPECIAL REP DRIVER **                     
         LKKEY LIT,REPKTYPE,C'R'                                                
         LKKEY ALL,REPKMED                                                      
         LKKEY ALL,REPKREP                                                      
         LKKEY SIN,REPKAGY,AGENCY                                               
         LKKEY LIT,REPKFILL,C'0'                                                
         LKKEY E                                                                
                                                                                
PRPKEYT  LKKEY H,PRPKEY            ** PURPOSE CODE DRIVER **                    
         LKKEY LIT,PRPKTYP,PRPKTYPQ                                             
         LKKEY LIT,PRPKSUB,PRPKSUBQ                                             
         LKKEY SIN,PRPKAGY,AGENCY                                               
         LKKEY ALL,PRPKMED                                                      
         LKKEY LIT,PRPKSPR,0                                                    
         LKKEY ALL,PRPCODE                                                      
         LKKEY E                                                                
                                                                                
RSNKEYT  LKKEY H,RSNKEY            ** REASON CODE DRIVER **                     
         LKKEY LIT,RSNKTYP,RSNKTYPQ                                             
         LKKEY LIT,RSNKSUB,RSNKSUBQ                                             
         LKKEY SIN,RSNKAGY,AGENCY                                               
         LKKEY ALL,RSNKMED                                                      
         LKKEY ALL,RSNKCODE                                                     
         LKKEY ALL,RSNKOFF                                                      
         LKKEY E                                                                
                                                                                
MKTKEYT  LKKEY H,MKTKEY,SAVED      ** MARKET DRIVER **                          
         LKKEY LIT,MKTKTYPE,MKTKTYPQ                                            
         LKKEY SIN,MKTKMED,MEDIA                                                
         LKKEY RNG,MKTKMKT,MKTRNGE                                              
         LKKEY SIN,MKTKAGY,AGENCY                                               
         LKKEY LIT,MKTKFILL,C'0'                                                
         LKKEY E                                                                
                                                                                
MSTFKEYT LKKEY H,CT99KEY           ** MSTREET FORMAT DRIVER TABLE               
         LKKEY LIT,CT99KTYP,CT99KTYQ    x'99'                                   
         LKKEY LIT,CT99KSUB,CT99KSFM    x'04'                                   
         LKKEY SIN,CT99KSRC,USNGSRCE    OWNERSHIP DATA SOURCE                   
         LKKEY LIT,CT99KSPR,0                                                   
         LKKEY ALL,CT99KFME                                                     
         LKKEY ALL,CT99KFRM                                                     
         LKKEY E                                                                
                                                                                
MSTOKEYT LKKEY H,CT99KEY           ** MSTREET OWNER DRIVER TABLE                
         LKKEY LIT,CT99KTYP,CT99KTYQ    x'99'                                   
         LKKEY LIT,CT99KSUB,CT99KSOW    x'02'                                   
         LKKEY SIN,CT99KSRC,USNGSRCE    OWNERSHIP DATA SOURCE                   
         LKKEY LIT,CT99KSPR,0                                                   
         LKKEY ALL,CT99KOME                                                     
         LKKEY ALL,CT99KOWN                                                     
         LKKEY E                                                                
                                                                                
PRDKEYT  LKKEY H,PKEY              ** PRODUCT DRIVER TABLE **                   
         LKKEY LIT,PKEYTYPE,PKEYTYPQ                                            
         LKKEY WMP,PKEYAM,AMED                                                  
         LKKEY WMP,PKEYCLT,ACLT                                                 
         LKKEY WMP,PKEYPRD,APRD2                                                
         LKKEY LIT,PKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
PRDKY2T  LKKEY H,EKEY              ** PRODUCT DRIVER TABLE **                   
         LKKEY LIT,EKEYTYPE,EKEYTYPQ                                            
         LKKEY WMP,EKEYAM,AMED                                                  
         LKKEY WMP,EKEYCLT,ACLT                                                 
         LKKEY WMP,EKEYPRD,APRD2                                                
         LKKEY WMP,EKEYEST,AEST                                                 
         LKKEY LIT,EKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
SDEKEYT  LKKEY H,EKEY              ** ESTIMATE DRIVER TABLE **                  
         LKKEY SIN,EKEY,SVPOLKEY,EKEYPRD+L'EKEYPRD-EKEY                         
         LKKEY WMP,EKEYEST,AEST                                                 
         LKKEY LIT,EKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
BUYKEYT1 LKKEY H,BUYKEY            ** BUY DRIVER TABLE 1 **                     
         LKKEY SIN,BUYKAM,BUYAGM                                                
         LKKEY WMP,BUYKCLT,ACLT                                                 
         LKKEY WMP,BUYKPRD,APRD1                                                
         LKKEY WMP,BUYKMKTN,AMKT                                                
         LKKEY WMP,BUYKSTAC,ASTA                                                
         LKKEY WMP,BUYKEST,AEST                                                 
         LKKEY ALL,BUYKBUY                                                      
         LKKEY E                                                                
                                                                                
BUYKEYT2 LKKEY H,BUYKEY            ** BUY DRIVER TABLE 2 **                     
         LKKEY SIN,BUYKAM,BUYAGM                                                
         LKKEY WMP,BUYKCLT,ACLT                                                 
         LKKEY WMP,BUYKPRD,APRD1                                                
         LKKEY WMP,BUYKMKTN,ASTA,L'BUYKMKTN+L'BUYKSTAC                          
         LKKEY WMP,BUYKEST,AEST                                                 
         LKKEY ALL,BUYKBUY                                                      
         LKKEY E                                                                
                                                                                
GOLKEYT  LKKEY H,GKEY              ** GOAL DRIVER TABLE **                      
         LKKEY LIT,GKEYTYPE,GKEYTYPQ                                            
         LKKEY SIN,GKEYAM,GOALAGM                                               
         LKKEY WMP,GKEYCLT,ACLT                                                 
         LKKEY WMP,GKEYPRD,APRD                                                 
         LKKEY WMP,GKEYMKT,AMKT                                                 
         LKKEY WMP,GKEYEST,AEST                                                 
         LKKEY ALL,GKEYDPT,,L'GKEYDPT+L'GKEYSLN+L'GKEYSEC                       
         LKKEY ALL,GKEYAGY,,L'GKEYAGY                                           
         LKKEY RNG,GKEYPRD2,BR#RNG                                              
         LKKEY E                                                                
                                                                                
NGLKEYT  LKKEY H,GKEY              ** NEW GOAL DRIVER TABLE **                  
         LKKEY LIT,GKEYTYPE,GKEYTYPQ                                            
         LKKEY WMP,GKEYAM,AMED                                                  
         LKKEY WMP,GKEYCLT,ACLT                                                 
         LKKEY WMP,GKEYPRD,APRD                                                 
         LKKEY ALL,GKEYMKT,,L'GKEYMKT                                           
         LKKEY WMP,GKEYEST,AEST                                                 
         LKKEY ALL,GKEYDPT,,L'GKEYDPT+L'GKEYSLN+L'GKEYSEC                       
         LKKEY ALL,GKEYAGY,,L'GKEYAGY                                           
         LKKEY ALL,GKEYPRD2,,L'GKEYPRD2                                         
         LKKEY E                                                                
                                                                                
CLRKEYT  LKKEY H,CLSKEY            ** CLEARANCE DRIVER **                       
         LKKEY LIT,CLSKTYPE+0,X'0D',1                                           
         LKKEY LIT,CLSKTYPE+1,X'76',1                                           
         LKKEY WMP,CLSKAGMD,AMED                                                
         LKKEY WMP,CLSKCLT,ACLT                                                 
         LKKEY WMP,CLSKMKT,AMKT                                                 
         LKKEY ALL,CLSKSTA,,L'CLSKEY-(CLSKSTA-CLSKEY)                           
         LKKEY E                                                                
                                                                                
CLTKEYT  LKKEY H,CKEY              ** CLIENT DRIVER TABLE **                    
         LKKEY LIT,CKEYTYPE,CKEYTYPQ                                            
         LKKEY WMP,CKEYAM,AMED                                                  
         LKKEY NZR,CKEYCLT                                                      
         LKKEY LIT,CKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
CLTKYTB  LKKEY H,CKEY              ** SPECIFIC CLIENT DRIVER **                 
         LKKEY LIT,CKEYTYPE,CKEYTYPQ                                            
         LKKEY WMP,CKEYAM,AMED                                                  
         LKKEY WMP,CKEYCLT,ACLT                                                 
         LKKEY LIT,CKEYREST,0                                                   
         LKKEY E                                                                
                                                                                
FLTKEYT  LKKEY H,DFLKEY            ** FLIGHT DRIVER TABLE **                    
         LKKEY LIT,DFLKTYP,DFLKTYPQ                                             
         LKKEY LIT,DFLKSUB,DFLKSUBQ                                             
         LKKEY WMP,DFLKAGMD,AMED                                                
         LKKEY WMP,DFLKCLT,ACLT                                                 
         LKKEY SIN,DFLKPRD,POLPRD                                               
         LKKEY WMP,DFLKEST,AEST                                                 
         LKKEY LIT,DFLKREST,0                                                   
         LKKEY E                                                                
                                                                                
MSTKEYT  LKKEY H,STAKEY            ** MARKET/STATION DRIVER TABLE **            
         LKKEY LIT,STKKTYPE,STKKTYPQ                                            
         LKKEY SIN,STKKAGY,AGENCY                                               
         LKKEY SIN,STKKMED,BUYMED                                               
         LKKEY WMP,STKKMKT,AMKT                                                 
         LKKEY ALL,STKKSTA                                                      
         LKKEY ALL,STKKCLT                                                      
         LKKEY E                                                                
                                                                                
STQCLTT  LKKEY H,STEKEY            ** STATION EQUIV DRIVER TABLE **             
         LKKEY LIT,STEKTYPE,STEKTYPQ    CLIENT SPECIFIC MGREQ RECS              
         LKKEY LIT,STEKSTYP,STEKSTYQ                                            
         LKKEY WMP,STEKAGMD,AMED                                                
         LKKEY WMP,STEKCLT,ACLT                                                 
         LKKEY ALL,STEKSTA                                                      
         LKKEY LIT,STEKNUSD,0                                                   
         LKKEY E                                                                
                                                                                
STQALLT  LKKEY H,STEKEY            ** STATION EQUIV DRIVER TABLE **             
         LKKEY LIT,STEKTYPE,STEKTYPQ    'ALL' CLIENT MGREQ RECS                 
         LKKEY LIT,STEKSTYP,STEKSTYQ                                            
         LKKEY WMP,STEKAGMD,AMED                                                
         LKKEY LIT,STEKCLT,FF                                                   
         LKKEY ALL,STEKSTA                                                      
         LKKEY LIT,STEKNUSD,0                                                   
         LKKEY E                                                                
                                                                                
SLKKEYT  LKKEY H,SLKKEY            ** STATION LOCKIN DRIVER TABLE **            
         LKKEY LIT,SLKKTYP,SLKKTYPQ                                             
         LKKEY LIT,SLKKSUB,SLKKSUBQ                                             
         LKKEY LIT,SLKKSUB+L'SLKKSUB,0,SLKKAGMD-(SLKKSUB+L'SLKKSUB)             
         LKKEY WMP,SLKKAGMD,AMED                                                
         LKKEY WMP,SLKKCLT,ACLT                                                 
         LKKEY WMP,SLKKMKT,AMKT                                                 
         LKKEY ALL,SLKKSTA                                                      
         LKKEY WMP,SLKKPRD,APRD1                                                
         LKKEY ALL,SLKKPRD2                                                     
         LKKEY WMP,SLKKEST,AEST                                                 
         LKKEY ALL,SLKKDPT,,SLKKIND-SLKKDPT                                     
         LKKEY LIT,SLKKIND,0                                                    
         LKKEY E                                                                
                                                                                
CS2KEYT  LKKEY H,PWFKEY            ** COS2 DRIVER TABLE **                      
         LKKEY LIT,PWKTYPE,PWKTYPEQ                                             
         LKKEY LIT,PWKSTYPE,PWKSTYPQ                                            
         LKKEY WMP,PWKAGMD,AMED                                                 
         LKKEY WMP,PWKCLT,ACLT                                                  
         LKKEY RNG,PWKPRD,C2PRDRNG                                              
         LKKEY WMP,PWKEST,AEST                                                  
         LKKEY WMP,PWKMKT,AMKT                                                  
         LKKEY LIT,PWKSTA,0,PWLEN-PWKSTA                                        
         LKKEY E                                                                
                                                                                
REVKEYT  LKKEY H,DRVKEY            ** REVSHEET DRIVER TABLE **                  
         LKKEY LIT,DRVKTYP,DRVKTYPQ                                             
         LKKEY LIT,DRVKSUB,DRVKSUBQ                                             
         LKKEY LIT,DRVKSPR1,0                                                   
         LKKEY WMP,DRVKAM,AMED                                                  
         LKKEY WMP,DRVKCLT,ACLT                                                 
         LKKEY SIN,DRVKPRD,PRD                                                  
         LKKEY WMP,DRVKEST,AEST                                                 
         LKKEY WMP,DRVKMKT,AMKT                                                 
         LKKEY ALL,DRVKREVS                                                     
         LKKEY LIT,DRVKREVL,0                                                   
         LKKEY LIT,DRVKFLG,0                                                    
         LKKEY LIT,DRVKSPR3,0                                                   
         LKKEY E                                                                
                                                                                
DEVKEYT  LKKEY H,DDVKEY            ** DEVIATED WEEKS KEY DRIVER **              
         LKKEY LIT,DDVKTYP,DDVKTYPQ                                             
         LKKEY LIT,DDVKSUB,DDVKSUBQ                                             
         LKKEY LIT,DDVKSPR1,0                                                   
         LKKEY WMP,DDVKAM,AMED                                                  
         LKKEY WMP,DDVKCLT,ACLT                                                 
         LKKEY SIN,DDVKPRD,PRD                                                  
         LKKEY WMP,DDVKEST,AEST                                                 
         LKKEY WMP,DDVKMKT,AMKT                                                 
         LKKEY LIT,DDVKSPR2,0                                                   
         LKKEY E                                                                
                                                                                
WRKKEYT  LKKEY H,DRVKEY            ** WORKSHEET DRIVER TABLE **                 
         LKKEY LIT,DRVKTYP,DRVKTYPQ                                             
         LKKEY LIT,DRVKSUB,DWKKSUBQ                                             
         LKKEY LIT,DRVKSPR1,0                                                   
         LKKEY WMP,DRVKAM,AMED                                                  
         LKKEY WMP,DRVKCLT,ACLT                                                 
         LKKEY SIN,DRVKPRD,PRD                                                  
         LKKEY WMP,DRVKEST,AEST                                                 
         LKKEY WMP,DRVKMKT,AMKT                                                 
         LKKEY ALL,DRVKREVS                                                     
         LKKEY LIT,DRVKREVL,0                                                   
         LKKEY LIT,DRVKFLG,0                                                    
         LKKEY LIT,DRVKSPR3,0                                                   
         LKKEY E                                                                
                                                                                
         EJECT                                                                  
FLMBUF   BUFFD TYPE=D,KEYLEN=0,COMLEN=0,BUFFERS=10                              
                                                                                
LVALUES  DS    0F                  ** LITERALS MOVED TO SAVED **                
         DC    A(FLTSDB)                                                        
         DC    A(FLTBKY)                                                        
         DC    A(FLTNGL)                                                        
         DC    A(FLTPRD)                                                        
         DC    A(FLTSTQ)                                                        
                                                                                
         DC    AL1(2,5,0,13)       ASSOCIATED WITH LABEL V250013                
         DC    AL1(3,0,0,101)      ASSOCIATED WITH LABEL V300101                
         DC    AL1(3,1,0,111)      ASSOCIATED WITH LABEL V310111                
         DC    AL1(3,1,0,150)      ASSOCIATED WITH LABEL V310150                
         DC    AL1(3,2,0,015)      ASSOCIATED WITH LABEL V320015                
         DC    AL1(3,3,0,200)      ASSOCIATED WITH LABEL V330200                
         DC    AL1(4,5,0,019)      ASSOCIATED WITH LABEL V450019                
         DC    AL1(4,6,0,50)       ASSOCIATED WITH LABEL V460050                
         DC    AL1(4,6,0,72)       ASSOCIATED WITH LABEL V460072                
         DC    AL1(4,6,0,170)      ASSOCIATED WITH LABEL V4600AA                
         DC    AL1(4,7,0,37)       ASSOCIATED WITH LABEL V470037                
         DC    AL1(4,7,0,149)      ASSOCIATED WITH LABEL V4700149               
         DC    C'D7'                                                            
         DC    C'POL'                                                           
         DC    X'414040',C'999'                                                 
         DC    X'00',X'FE'                                                      
         DC    C'00009999'         VALID MARKET NUMBER RANGE                    
         DC    C'TP '              DEMO FILE NAME                               
         DC    C'ALL'                                                           
         DC    X'0000FF'                                                        
         DC    X'0D0C'                                                          
         EJECT                                                                  
                                                                                
                                                                                
B#B3IO1  EQU   3                   IO1 -                                        
B#AGYREC EQU   3                       - AGENCY RECORD                          
B#B4IO2  EQU   4                   IO2 -                                        
B#STAREC EQU   4                       - STATION RECORD                         
B#CLTREC EQU   4                       - CLIENT RECORD                          
B#B5IO3  EQU   5                   IO3 -                                        
B#ESTREC EQU   5                       - ESTIMATE RECORD                        
AESTREC  EQU   LP_BLKS+((B#ESTREC-1)*L'LP_BLKS),,C'A'                           
B#FLTREC EQU   5                       - FLIGHT RECORD                          
B#MKTREC EQU   5                       - MARKET RECORD                          
B#COMREC EQU   5                       - STANDARD COMMENT RECORD                
B#SDRREC EQU   5                       - SELF-DEFINING RECORD                   
B#STSREC EQU   5                       - STATION SOURCE RECORD                  
B#ALMREC EQU   5                       - ALPHA MARKET RECORD                    
ASDRREC  EQU   LP_BLKS+((B#SDRREC-1)*L'LP_BLKS),,C'A'                           
B#B6IO4  EQU   6                   IO4 -                                        
B#PRDREC EQU   6                       - PRODUCT RECORD                         
B#GOLREC EQU   6                       - GOAL RECORD                            
AGOLREC  EQU   LP_BLKS+((B#GOLREC-1)*L'LP_BLKS),,C'A'                           
B#SLKREC EQU   6                       - STATION LOCKIN RECORD                  
ASLKREC  EQU   LP_BLKS+((B#SLKREC-1)*L'LP_BLKS),,C'A'                           
B#REVREC EQU   6                       - REVSHEET RECORD                        
B#DEVREC EQU   6                       - DEVIATED WEEKS RECORD                  
B#CLRREC EQU   6                       - CLEARANCE STATUS RECORD                
B#B7IO5  EQU   7                   IO5 -                                        
B#BUYREC EQU   7                   IO5 - BUY RECORD                             
ABUYREC  EQU   LP_BLKS+((B#BUYREC-1)*L'LP_BLKS),,C'A'                           
B#B8IO6  EQU   8                   IO6 -                                        
B#STEREC EQU   8                       - STATION EQUIV RECORD                   
B#B9IO7  EQU   9                   IO7 -                                        
B#SOFREC EQU   9                       - FLIGHT RECORD (SUBSCRIPTION)           
B#MSTREC EQU   9                       - MSTREET RECORDS                        
B#B10IO8 EQU   10                  IO8 -                                        
B#ERPREC EQU   10                  IO8 - EDI REP PARTNER RECORD                 
AERPREC  EQU   LP_BLKS+((B#ERPREC-1)*L'LP_BLKS),,C'A'                           
B#CS2REC EQU   10                        COS2 RECORD                            
B#B11IO9 EQU   11                  IO9 -                                        
B#FLMREC EQU   11                  FILM RECORD (USES PASSED STORAGE)            
AFLMREC  EQU   LP_BLKS+((B#FLMREC-1)*L'LP_BLKS),,C'A'                           
B#SVRDEF EQU   12                  SERVER                                       
                                                                                
CANAGYQ  EQU   C'C'                CANADIAN AGENCY IDENTIFIER                   
RADMEDQ  EQU   C'R'                RADIO MEDIA                                  
TELMEDQ  EQU   C'T'                TELEVISION MEDIA                             
NTRDMEDQ EQU   C'X'                NETWORK RADIO MEDIA                          
CANMEDQ  EQU   C'C'                CANADIAN MEDIA LETTER                        
USAMEDQ  EQU   C'T'                USA MEDIA LETTER                             
QUNSENT  EQU   C'-'                DUMMY EQUATE FOR UNSENT ORDER                
                                                                                
EOR      EQU   0                   END OF RECORD ELEMENT                        
CBLSUFQ  EQU   X'B0'               SUFFIX FOR CABLE STATIONS                    
DLTGELQ  EQU   X'18'               DEALER CML/TAG ASSIGN ELEMENT                
PDELEMQ  EQU   X'22'               POST BUY DEMO ORIGINAL ELEMENT               
SDELEMQ  EQU   X'23'               POST BUY DEMO SPILL ELEMENT                  
DLOVELQ  EQU   X'24'               DEMO LOOK-UP OVERRIDE ELEMENT                
UPELEMQ  EQU   X'62'               UPGRADE ELEMENT                              
IDELEMQ  EQU   X'70'               ID ELEMENT                                   
CMELEMQ  EQU   X'66'               COMMENT ELEMENT                              
MGRCELQ  EQU   X'65'               MAKEGOOD REASON CODE ELEMENT                 
COS2ELQ  EQU   X'71'               COST2 ELEMENT                                
C2FCELQ  EQU   X'73'               COST2 FACTOR ELEMENT                         
ACTVELQ  EQU   X'99'               BUY ACTIVITY ELEMENT                         
DPTKTYPQ EQU   X'08'               DAYPART RECORD CODE                          
EQUKTYPQ EQU   X'09'               EQUIVALENCY HEADER RECORD TYPE               
POLPRDQ  EQU   FF                  POOL PRODUCT NUMBER                          
SEC30Q   EQU   30                  DEFAULT SECONDS LENGTH                       
         EJECT                                                                  
         DS    0H                                                               
ORDLOCKS DS    0CL(L'DOXMTSTA)     ** UNLOCKED ORDER STATUSES **                
         DC    AL1(QRJCT)          REJECTED                                     
         DC    AL1(QCFMD)          CONFIRMED                                    
         DC    AL1(QUNDARE)        UNDARED                                      
         DC    AL1(QNODARE)        NOT DARE ANY MORE                            
         DC    AL1(QERRORED)       SF ERROR                                     
         DC    AL1(QRECALL)        RECALLING IS OKAY FOR AUTO-RECALL            
         DC    AL1(QRCLAPPR)       RECALL OF APPROVED ORDER                     
         DC    AL1(QRCLDELN)       RECALL OF DELIVERED ORDER                    
         DC    AL1(QRCLTRNS)       RECALL AND TRANSMITTED TO STATION            
         DC    AL1(QRCLWIP)        RECALL AND WORK IN PROGRESS                  
         DC    AL1(QEMPTY)         NO BUYS WHEN THIS ORDER WAS SENT             
         DC    AL1(QBYRCNFM)       BUYER CONFIRMED                              
         DC    AL1(QSNTXCNF)       SENT CANCELLED, RECALL CONF W/COM            
         DC    AL1(QSNTXREJ)       SENT CANCELLED, RECALLED REJECTED            
ORDLOCKN EQU   (*-ORDLOCKS)/L'ORDLOCKS                                          
                                                                                
         EJECT                                                                  
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
                                                                                
WVALUES  DS    0X                  ** LITERAL VALUES **                         
                                                                                
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONSTANTS **            
AFLTSDB  DS    A                   A(PRODUCT FILTER ROUTINE)                    
AFLTBKY  DS    A                   A(BUY KEY FILTER ROUTINE)                    
AFLTNGL  DS    A                   A(NEW GOAL KEY FILTER ROUTINE)               
AFLTPRD  DS    A                   A(BUY RECORD FILTER ROUTINE)                 
AFLTSTQ  DS    A                   A('ALL' CLT MGREQ FILTER ROUTINE)            
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
                                                                                
V250013  DS    XL4                 PC VERSION 2.5.0.13                          
V300101  DS    XL4                 PC VERSION 3.0.0.101                         
V310111  DS    XL4                 PC VERSION 3.1.0.111                         
V310150  DS    XL4                 PC VERSION 3.1.0.150                         
V320015  DS    XL4                 PC VERSION 3.2.0.015                         
V330200  DS    XL4                 PC VERSION 3.3.0.200                         
V450019  DS    XL4                 PC VERSION 4.5.0.019                         
V460050  DS    XL4                 PC VERSION 4.6.0.50                          
V460072  DS    XL4                 PC VERSION 4.6.0.72                          
V4600AA  DS    XL4                 PC VERSION 4.6.0.170                         
V470037  DS    XL4                 PC VERSION 4.7.0.37                          
V4700149 DS    XL4                 PC VERSION 4.7.0.149                         
DARADM   DS    CL2                 DARE AGENCY CODE                             
POLPRD   DS    CL3                 POL PRODUCT CODE                             
BRCRNG   DS    CL6                 PRODUCT CODE KEY RANGE                       
BR#RNG   DS    XL2                 PRODUCT NUMBER KEY RANGE                     
MKTRNGE  DS    CL8                 VALID MARKET NUMBER RANGE                    
DEMOFILE DS    CL3                 DEMO FILE NAME                               
ALLPRD   DS    CL3                 C'ALL'                                       
POLPRDX  DS    XL3                 X'0000FF'                                    
COMTYPEQ DS    XL2                 COMMENT RECORD TYPE                          
                                                                                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
C2PRDRNG DS    XL2                 PRODUCT NUMBER KEY RANGE FOR COS2            
AMASTC   DS    A                   A(MASTC)                                     
ABUFFRIN DS    A                   A(BUFFERIN)                                  
ACBLLIST DS    A                   A(SPCBLLIST - T00A9E)                        
ADUPLIST DS    A                   A(DUPLICATES IN SPCBLLIST)                   
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
                                                                                
SVPOLKEY DS    XL(L'IOKEY)         SAVED POOL ESTIMATE KEY                      
                                                                                
MAPI     DS    0XL(L'MAPI1+L'MAPI2)                                             
                                                                                
MAPI1    DS    X                   ** MAP INDICATOR BYTE 1 **                   
MAPIORDD EQU   X'80'               ORDER DOWNLOAD                               
MAPINOTD EQU   X'40'               DOWNLOAD NOTICE DETAILS                      
MAPIBUYD EQU   X'20'               BUY DOWNLOAD                                 
MAPIOBCR EQU   X'10'               DOWNLOAD ORDER BUYER COMMENTS                
*        EQU   X'08'               N/D                                          
MAPILORD EQU   X'04'               LOCKED ORDER DOWNLOAD                        
MAPIMGDD EQU   X'02'               MAKEGOOD DOWNLOAD                            
MAPIR4RD EQU   X'01'               ROUTES DOWNLOAD                              
                                                                                
MAPI2    DS    X                   ** MAP INDICATOR BYTE 2 **                   
MAPIAMCE EQU   X'80'               RESOLVE AGENCY/MEDIA/CLIENT/EST              
MAPIMISS EQU   X'40'               DOWNLOAD MISSED BUYLINE DETAILS              
MAPIOFFD EQU   X'20'               DOWNLOAD OFFER DETAILS                       
MAPIOBUY EQU   X'10'               DOWNLOAD OFFER BUY DETAILS                   
MAPIMNHD EQU   X'08'               DOWNLOAD MAKEGOOD NOTICE HEADER              
MAPIORDR EQU   X'04'               DOWNLOAD ORDER DETAILS                       
MAPIFSTA EQU   X'01'               APPLY STATION FILTER IN GETORD S/R           
                                                                                
RUNI1    DS    X                   ** RUN INDICATOR BYTE 1 **                   
RUNICANA EQU   X'80'               CANADIAN AGENCY                              
RUNIENIK EQU   X'40'               ESTIMATE NUMBER IN ORDER KEY                 
RUNIMARK EQU   X'20'               MARKET NAME SENT                             
                                                                                
SVERROR  DS    XL(L'LP_ERROR)      ERROR NUMBER                                 
                                                                                
BUYAGM   DS    X                   AGENCY/MEDIA CODE FOR BUY RECORDS            
BUYMED   DS    C                   MEDIA CODE FOR BUY RECORDS                   
                                                                                
BUYCLRLO DS    XL(L'SPTCLRDT)      LOWEST CLEARANCE DATE                        
BUYCLRHI DS    XL(L'SPTCLRDT)      HIGHEST CLEARANCE DATE                       
                                                                                
SAVE1OR2 DS    X                                                                
                                                                                
SPILL    DS    C                   YESQ IF SPILL BUY                            
SPOTDATE DS    XL(L'RDATE)         SPOT DATE                                    
SPOTTLEN DS    XL(L'BDSEC)         SPOT LENGTH TOTAL                            
SPOTREFN DS    X                   SPOT REFERENCE NUMBER                        
GOALDATE DS    XL(L'GLWEEK)        GOAL WEEK START DATE                         
GOALAGM  DS    X                   AGENCY/MEDIA CODE FOR GOAL RECORDS           
GOALMED  DS    C                   MEDIA CODE FOR GOAL RECORDS                  
CALLMODE DS    X                   SUB-ROUTINE CALLING MODE                     
ESTFOUND DS    X                   ESTMATE FOUND (NXTEST)                       
PRD      DS    CL(L'DRVKPRD)       PRODUCT CODE                                 
                                                                                
ANDELEM  DS    A                   A(CURRENT DEMO SPILL ELEMENT)                
ADEMOEL  DS    A                   A(CURRENT DEMO ELEMENT)                      
AORBTEL  DS    A                   A(CURRENT ORBIT ELEMENT)                     
AWMPEL   DS    A                   A(NEXT WMP ELEMENT)                          
ASDSTAB  DS    A                   A(SPOT TABLE)                                
                                                                                
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
TSARREC  DS    XL16                TSAR RECORD                                  
TSARSAV  DS    XL16                TSAR RECORD SAVE AREA                        
                                                                                
REQVALS  DS    0F                  ** REQUEST VALUES **                         
                                                                                
MEDIND   DS    X                   MEDIA                                        
AMED     DS    AL3                                                              
                                                                                
CLTIND   DS    X                   CLIENT                                       
ACLT     DS    AL3                                                              
                                                                                
MKTIND   DS    X                   MARKET                                       
AMKT     DS    AL3                                                              
                                                                                
STAIND   DS    X                   STATION                                      
ASTA     DS    AL3                                                              
                                                                                
PRDIND   DS    X                   PRODUCT (NUMBER)                             
APRD     DS    AL3                                                              
                                                                                
PIGIND   DS    X                   PIGGYBACK PRODUCT (NUMBER)                   
APIG     DS    AL3                                                              
                                                                                
PRDIND1  DS    X                   PRODUCT (CODE)                               
APRD1    DS    AL3                                                              
                                                                                
PRDIND2  DS    X                   PRODUCT (CODE)                               
APRD2    DS    AL3                                                              
                                                                                
ESTIND   DS    X                   ESTIMATE                                     
AEST     DS    AL3                                                              
                                                                                
ESTIND2  DS    X                   ESTIMATE (BUILT FROM POOL ESTIMATES)         
AEST2    DS    AL3                                                              
                                                                                
PDVIND   DS    X                                                                
APDV     DS    AL3                                                              
                                                                                
STTNIND  DS    0X                                                               
RRADIND  DS    0X                                                               
PIDSIND  DS    0X                                                               
COMIND   DS    X                                                                
ASTATN   DS    0AL3                                                             
APIDS    DS    0AL3                                                             
ACOM     DS    AL3                                                              
                                                                                
DEMIND   DS    X                   DEMOS                                        
ADEM     DS    AL3                                                              
                                                                                
BUYIND   DS    0X                  ** BUY PROGRAM RETURN VALUES **              
RDAIND   DS    X                                                                
ATIM     DS    0AL3                TIME                                         
ARDA     DS    AL3                 RECORD DISK ADDRESS                          
                                                                                
TOKIND   DS    X                                                                
ATOK     DS    AL3                 RECORD TOKEN                                 
                                                                                
ERRIND   DS    X                   BUY ERRORS                                   
AERR     DS    AL3                                                              
BUYINDL  EQU   *-BUYIND                                                         
                                                                                
PCKEY    DS    CL(#$PCKEY)         PC KEY                                       
PCKEYORD DS    CL12                PC KEY FOR ORDERS                            
OLOCK    DS    C                   ORDER LOCKED                                 
                                                                                
AGENCY   DS    CL(L'AGYKAGY)       AGENCY CODE                                  
CNVRGEDL DS    X                   CONVERGENCE INITIAL DOWNLOAD?                
                                                                                
BRDSONLY DS    X                   PRODUCTS ONLY (ALLOCATION)?                  
ENVIRO   DS    X                   ENVIROMENT TO USE                            
                                                                                
DLDAFFID DS    X                   DOWNLOAD AFFIDAVIT DATA?                     
DLDGOALS DS    X                   DOWNLOAD GOALS?                              
DLDCLEAR DS    X                   DOWNLOAD CLEARANCE DATA?                     
DLDMKTLK DS    X                   DOWNLOAD MARKET LOCKIN DATA?                 
DLDSTALK DS    X                   DOWNLOAD STATION LOCKIN DATA?                
DLDSTALS DS    X                   DOWNLOAD STATION LIST?                       
DLDBYSTA DS    X                   DOWNLOAD BY STATION?                         
DLDSPLST DS    X                   DOWNLOAD ONLY SPILL STATIONS                 
                                                                                
XPAIDS   DS    X                   EXCLUDE PAID SPOTS?                          
XMISSD   DS    X                   EXCLUDE MISSED SPOTS?                        
XMGOOD   DS    X                   EXCLUDE MAKEGOOD SPOTS?                      
XZERO$   DS    X                   EXCLUDE ZERO DOLLAR SPOTS?                   
XPALOC   DS    X                   EXCLUDE PRE-ALLOCATED SPOTS?                 
XSPILL   DS    X                   EXCLUDE SPILL?                               
XBUYS    DS    X                   EXCLUDE BUY DATA? (BUYTRACKER D/L)           
XCLTLS   DS    X                   EXCLUDE CLIENT LIST                          
GETPRF   DS    X                   GET PROFILES                                 
                                                                                
STRDATEB DS    XL3                 REQUEST START DATE (BINARY)                  
ENDDATEB DS    XL3                 REQUEST END DATE (BINARY)                    
                                                                                
STRDATEC DS    XL2                 REQUEST START DATE (COMPRESSED)              
ENDDATEC DS    XL2                 REQUEST END DATE (COMPRESSED)                
STRDMONC DS    XL2                 REQUEST START MONDAY (COMPRESSED)            
                                                                                
STAPCODE DS    XL(L'DSRKSTA)       STATION CODE                                 
                                                                                
ASTALST  DS    A                   A(STATION LIST FOR ROUTES DOWNLOAD)          
ASTALSTX DS    A                   A(END OF STATION LIST)                       
AKSTALST DS    A                   A(STATION LIST THAT HAVE K PASSIVES)         
ASPMKLST DS    A                   A(SPILL MARKET LIST)                         
ASTEQLST DS    A                   A(CLT-SPECIFIC MGREQ STATIONS)               
                                                                                
ADPTMEDA DS    A                   A(CURRENT DPT MEDIA IN DPTTAB)               
                                                                                
ASLNTAB  DS    A                   A(SPOT LENGTH TABLE FOR AGY/MED)             
                                                                                
ALLPRDS  DS    C                   User wants all products ?                    
NOPRDREQ DS    C                   NO PRODUCT IN REQUEST                        
                                                                                
REQVALL  EQU   *-REQVALS                                                        
                                                                                
AALL     DS    AL3                 ALL VALUE WMP ENTRY                          
ANZR     DS    AL3                 NON-ZERO VALUE WMP ENTRY                     
                                                                                
TODAYCMP DS    0XL2                TODAY'S DATE (COMPRESSED)                    
STRDATER DS    XL(L'STRDATEC*2)    ESTIMATE START DATE RANGE                    
ENDDATER DS    XL(L'ENDDATEC*2)    ESTIMATE END DATE RANGE                      
                                                                                
MEDIA    DS    CL(L'MKTKMED)       MEDIA CODE                                   
CURRMED  DS    C                   CURRENT MEDIA                                
CURRSLN  DS    X                   CURRENT SPOT LENGTH                          
                                                                                
NTRYLEN  DS    X                   LENGTH TO NEXT TABLE ENTRY                   
ANXTTAB  DS    AL3                 A(NEXT TABLE ENTRY) 1                        
ANXTTAB2 DS    AL3                 A(NEXT TABLE ENTRY) 2                        
ANXTTAB3 DS    AL3                 A(NEXT TABLE ENTRY) 3                        
                                                                                
BKFILE   DS    CL3                 BOOK FILE                                    
BTFLAG   DS    C                   BOOKTYPE FLAG                                
BTRES    EQU   C'R'                - RESEARCH BOOK                              
                                                                                
CURROUT  DS    X                   CURRENT OUTPUT TYPE FOR DEMOCON              
CURRDEM  DS    X                   CURRENT DEMO NUMBER                          
                                                                                
USESODTA DS    CL1                 USES OWNERSHIP DATA                          
OWNRSRCE DS    CL1                 OWNERSHIP SOURCE (X'00',S,F)                 
DIGSRCE  DS    CL1                 C'D' - digital SOURCE FOR OWNERSHIP          
USNGSRCE DS    CL1                 SOURCE SET TO USE IN THE KEY TABLES          
HASHFCOD DS    CL8                 HASHED film code with source leading         
HASHOCOD DS    CL8                 HASHED ownr code with source leading         
                                                                                
CURRDATE DS    CL6                 CURRENT DATE                                 
CURRTIME DS    XL2                 CURRENT TIME                                 
                                                                                
DAREAGYV DS    0X                  ** DARE AGENCY VALUES **                     
USIDNAME DS    CL(L'CTORGNAM)      USER-ID NAME                                 
USIDADDR DS    CL(L'CTORGADD)      USER-ID ADDRESS                              
DAREROUT DS    0CL(L'CTUSADRC)     DARE ROUTING CODE                            
DAREAGYC DS    CL3                 DARE AGENCY CODE                             
DAREOFFC DS    CL2                 OFFICE CODE                                  
DAREAGYL EQU  *-DAREAGYV                                                        
                                                                                
MAXBLNS  DS    XL2                 MAXIMUM NUMBER OF BUYLINES                   
MAXBSIZ  DS    XL2                 MAX SIZE, IN BYTES, OF A BUY RECORD          
                                                                                
MXDMSIZE DS    XL2                                                              
STDTSIZE DS    X                                                                
BOOKSIZE DS    X                                                                
DEMOSIZE DS    X                                                                
PROJSIZE DS    X                                                                
MAXPROJ  DS    X                                                                
                                                                                
SVDEMORG DS    C                   DEMO ORIGIN                                  
SVDMOED  EQU   C'E'                - ESTIMATE DEMO                              
SVDMOGD  EQU   C'G'                - GOAL DEMO                                  
SVDMOBD  EQU   C'B'                - BUY DEMO                                   
SVDEMTYP DS    C                   SAVED DEMO TYPE                              
ACURDMTP DS    A                   A(CURRENT DEMO TYPE)                         
SVDEMTYL DS    CL20                LIST OF SAVED DEMO TYPES                     
         ORG   SVDEMORG                                                         
PERNAME  DS    CL62                PERSON NAME                                  
*                                                                               
OUTVALS  DS    0X                  ** OUTPUT VALUES **                          
                                                                                
MEDCOD   DS    CL(L'AGYMEDCD)      MEDIA CODE                                   
                                                                                
CLTNAME  DS    CL(L'CNAME)         CLIENT NAME                                  
CLTOFF   DS    CL(L'COFFICE)       CLIENT OFFICE CODE                           
CLTACCOF DS    CL(L'CACCOFC)       CLIENT ACCOUNTING OFFICE                     
CLTACCAG DS    CL(L'CACCAGY)       CLIENT AGENCY OVERRIDE                       
CLTBPOL  DS    X                   CLIENT IS BRAND/POOL                         
CLTADJC  DS    C                   PROGRAM ADJACENCY CONTROL                    
CLTRCTL  DS    C                   RATE CONTROL                                 
CLTIDRQ  DS    C                   BUY ID REQUIRED                              
CLTC2FAC DS    PL4                 COST2 FACTOR                                 
CLTFRZMO DS    XL(L'CLOCKYM)       FROZEN YEAR/MONTH                            
CLTEQU30 DS    XL2                 30 SECOND EQUIVALENCE                        
CLTEQU   DS    30XL3               CLT LENGTH EQUIV TBL-NO MORE THAN 30         
CLTINDS  DS    0XL4                ** CLIENT INDICATORS **                      
CLTINDS1 DS    X                   ** CLIENT INDICATORS BYTE 1 **               
CLTRUNI1 DS    X                   ** CLIENT INDICATORS BYTE 2 **               
CLTINDS3 DS    X                   ** CLIENT INDICATORS BYTE 3 **               
CLTINDS4 DS    X                   ** CLIENT INDICATORS BYTE 4 **               
                                                                                
CLT#MIDS EQU   CLTINDS3                                                         
CLTIMIDS EQU   X'02'               MIDAS CLIENT                                 
CLT#C2TR EQU   CLTINDS3                                                         
CLTIC2TR EQU   X'01'               COST2 TRADE INDICATOR                        
CLT#FSUB EQU   CLTINDS4                                                         
CLTIFSUB EQU   X'80'               FROZEN MONTH SUBSEQUENT                      
CLT#FPRI EQU   CLTINDS4                                                         
CLTIFPRI EQU   X'40'               FROZEN MONTH PRIOR                           
CLT#FRZN EQU   CLTINDS4                                                         
CLTIFRZN EQU   X'20'               FROZEN CLIENT                                
CLT#C2OP EQU   CLTINDS4                                                         
CLTIC2OP EQU   X'10'               COST2 OPTIONAL                               
CLT#OOWC EQU   CLTINDS4                                                         
CLTIOOWC EQU   X'08'               OUT OF WEEK CLIENT                           
CLT#GLRQ EQU   CLTINDS4                                                         
CLTIGLRQ EQU   X'04'               GOAL REQUIRED FOR BUYING                     
CLT#USSP EQU   CLTINDS4                                                         
CLTIUSSP EQU   X'02'               US SPILL                                     
CLT#C2RQ EQU   CLTINDS4                                                         
CLTIC2RQ EQU   X'01'               COST2 REQUIRED                               
                                                                                
ESTV     DS    0X                  ** ESTIMATE VALUES **                        
ESTSTRDT DS    CL(L'ESTART)        ESTIMATE START DATE                          
ESTENDDT DS    CL(L'EEND)          ESTIMATE END DATE                            
ESTOOW   DS    CL(L'EOWSDAY)       ESTIMATE OOW START DAY                       
ESTPDTYP DS    CL(L'EDEMLTY1)      ESTIMATE PRIMARY DEMO TYPE                   
*                                                                               
ESTREP   DS    CL3                 SPECIAL REP CODE                             
ESTFRZMO DS    XL(L'ELOCKYM)       FROZEN MONTH                                 
ESTC2FAC DS    PL4                 COST2 FACTOR                                 
                                                                                
ESTINDS  DS    0XL4                ** ESTIMATE INDICATORS **                    
ESTINDS1 DS    X                   ** ESTIMATE INDICATORS BYTE 1 **             
ESTINDS2 DS    X                   ** ESTIMATE INDICATORS BYTE 2 **             
ESTINDS3 DS    X                   ** ESTIMATE INDICATORS BYTE 3 **             
ESTINDS4 DS    X                   ** ESTIMATE INDICATORS BYTE 4 **             
                                                                                
ESTUDEMO DS    0CL(L'EUSRNML)      HOLDS TEXT OF USER DEMO                      
ESTNTDEM DS    CL(L'ENONTDMS)      HOLD TEXT OF NON-TRADITIONAL DEMO            
                                                                                
EST#FSUB EQU   ESTINDS4                                                         
ESTIFSUB EQU   X'02'               FROZEN MONTH SUBSEQUENT                      
EST#FPRI EQU   ESTINDS4                                                         
ESTIFPRI EQU   X'01'               FROZEN MONTH PRIOR                           
ESTMOS   DS    (CPPTMAXM)XL4,X     ESTIMATE MONTHS                              
ESTVL    EQU   *-ESTV                                                           
POLECTRL DS    XL(L'ECNTRL)        POL ESTIMATE'S ECNTRL BYTE                   
                                                                                
PROFV    DS    0XL16               ** PROFILE VALUES **                         
PROFDARV DS    XL(L'PROFV)         SPOT/DAR PROFILE VALUES                      
PROF00V  DS    XL(L'PROFV)         SPOT/00 PROFILE VALUES                       
PROF1WV  DS    XL(L'PROFV)         SPOT/1W PROFILE VALUES                       
PROFB0V  DS    XL(L'PROFV)         SPOT/B0 PROFILE VALUES                       
PROFSTV  DS    XL(L'PROFV)         SPOT/ST PROFILE VALUES                       
PROFAJV  DS    XL(L'PROFV)         SPOT/AJ PROFILE VALUES                       
PROF00A  DS    XL(L'PROFV)         SPOT/00A PROFILE VALUES                      
PROFBW   DS    XL(L'PROFV)         SPOT/BW  PROFILE VALUES                      
PROFBWA  DS    XL(L'PROFV)         SPOT/BWA PROFILE VALUES                      
PROFOM   DS    XL(L'PROFV)         SPOT/OM  PROFILE VALUES                      
PROFL    EQU   *-PROFV                                                          
         ORG   PROFV+(PROFNSDB*L'PROFV)                                         
PROFG0V  DS    XL(L'PROFV)         SPOT/G0 PROFILE VALUES  **SPECIAL**          
                                                                                
CORPDEMO DS    XL(L'EDEMLIST)      CORPORATE DEMO FROM POOL ESTIMATE            
                                                                                
DPTCODE  DS    CL(L'EDAYMENU)      MASTER DAYPART CODE                          
                                                                                
DPTTAB   DS    (DPTTMAXN)CL(DPTTABL),X                                          
                                                                                
STAVALS  DS    0X                  ** STATION VALUES **                         
STASEND  DS    C                   YESQ TO SEND STATION VALUES                  
STAORIG  DS    CL(L'STAPQSTA)      ORIGINAL VALUE OF STASTA                     
STAMEDIA DS    CL(L'QMEDA)         MEDIA CODE                                   
STAMKT#  DS    XL(L'BUYKMKTN)      MARKET NUMBER                                
STAMKTN  DS    CL(L'MKTNAME)       MARKET NAME                                  
STAMKTZ  DS    CL(L'MKTZONE)       TIME ZONE CODE                               
STAAMKT  DS    CL(L'MKTALF)        MARKET ALPHA                                 
STALPMD  DS    XL(L'MKTLPMDT)      MARKET LPM DATE                              
STACDEM  DS    CL(L'MKTCDEM)       MARKET CABLE DEMO FLAG                       
STAAAMK  DS    XL2                 MEDIAOCEAN AUTOMATED AVAIL MKT #             
STACLT   DS    CL(L'STAKCLT)       CLT FOR THE CLT SPECIFIC STATION             
STACHNL  DS    CL(L'SCHNL+1)       CHANNEL (MIGHT HAVE TO PUT A .)              
STANTWK  DS    CL(L'SNETWRK)       NETWORK AFFLIATE                             
STAFRMT  DS    CL(L'SFORMAT)       FORMAT FOR RADIO                             
STABTYP  DS    CL(L'SBKTYPE)       BOOKTYPE                                     
STAMKBT  DS    CL(L'MKTBKTYP)      MARKET BOOKTYPE                              
STASTAL  EQU   L'STAPQSTA+L'STAPQNET                                            
STABSTA  DS    XL(L'STAPSTA)       BINARY STATION                               
STASTA   DS    CL(STASTAL)         STATION(/CABLE NETWORK)                      
STATAX   DS    CL6                 TAX RATE (SEE SPSFM14-SSTTAX)                
STAEQMKT DS    CL(L'STEMGID)       EQUIVALENCE MARKET ID                        
STAEQMK# DS    XL(L'STEMGRP)       EQUIVALENCE MARKET NUMBER                    
STARSV   DS    CL(L'SDEFKRSV)      SPILL DEF RATING SERVICE CODE                
STANWCTR DS    XL1                 STATION'S NETWORK CODE COUNTER               
STADNWK  DS    CL3                 DDS NETWORK CODE                             
STANNWK  DS    CL4                 NIELSEN NETWORK CODE                         
STAPPLUS DS    CL(L'SPARPLUS)      Parent+Satellite Y/N, default is N           
STAMIDS  DS    CL(L'STMIDAS)       MIDAS                                        
STACBKTY DS    CL(L'SCBKTYPE)      Station comScore Booktype                    
MKTCBKTY DS    CL(L'MKTCSBTY)      Market comScore Booktype                     
STACMKT  DS    XL(L'CTDMKNUM)      comScore market                              
STAVALL  EQU   *-STAVALS                                                        
STAMKBT2 DS    CL(L'MKTBKTYP)      MARKET BOOKTYPE                              
                                                                                
STANUMN  DS    AL2                 STATION ENTRY BEING PROCESSED                
STAMKTNO DS    CL(L'SMKT)          STATION MARKET TO MATCH TO                   
STALEFTS DS    X                   1=PROCESSING LEFTOVER STATIONS               
                                                                                
STEQMODE DS    X                   00 = CLT SPECIFIC MGREQ RECORDS              
*                                  FF = 'ALL' CLIENT MGREQ RECORDS              
                                                                                
LINVALS  DS    0X                  ** BUYLINE VALUES **                         
LINEST   DS    XL(L'BUYKEST)       ESTIMATE NUMBER                              
LINSTA   DS    XL(L'BUYKSTA)       STATION                                      
LINLIN   DS    XL2                 BUY LINE NUMBER                              
LINROT   DS    XL(L'BDDAY)         ROTATION                                     
LIN#WK   DS    XL(L'BDNOWK)        NUMBER OF SPOTS PER WEEK                     
LINDAY   DS    X                   START DAY NUMBER                             
LINPER   DS    CL12                BUY PERIOD                                   
LINSTRD  DS    XL(L'BDSTART)       START DATE                                   
LINENDD  DS    XL(L'BDEND)         END DATE                                     
LINSTRT  DS    XL(L'BDTIMST)       START TIME                                   
LINENDT  DS    XL(L'BDTIMEND)      END TIME                                     
LINSEC   DS    XL(L'BDSEC)         SECONDS LENGTH                               
LINDPT   DS    CL(L'BDDAYPT)       DAYPART CODE                                 
LINPGN   DS    CL(L'BDPROGRM)      PROGRAM NAME                                 
LINPRP   DS    CL(L'BDPURP)        PURPOSE CODE                                 
LINREP   DS    CL3                 REP CODE                                     
LINTAX   DS    CL(L'BDNTAX)        TAX RATE                                     
LINPGC   DS    CL5                 SHOW CODE (XXXX, =XXX OR =XXXX)              
LINCON   DS    CL12                CONTRACT NUMBER                              
LINADJ   DS    CL2                 PROGRAM ADJACENCY CODE                       
LINRTY   DS    C                   RATE TYPE                                    
LINPRD   DS    CL(L'PKEYPRD)       PRODUCT CODE                                 
LINPIG   DS    CL(L'PKEYPRD)       PIGGYBACK PRODUCT CODE                       
LINBYR   DS    CL12                BUYER CODE                                   
LINDA    DS    XL(L'IODA)          BUYLINE DISK ADDRESS                         
LINRSIZ  DS    XL(L'BUYRLEN)       BUYLINE RECORD SIZE                          
LINCOMS  DS    0CL80               ** COMMENTS **                               
LINCOM1  DS    CL(L'LINCOMS)       COMMENT LINE 1                               
LINCOM2  DS    CL(L'LINCOMS)       COMMENT LINE 2                               
LINCOM3  DS    CL(L'LINCOMS)       COMMENT LINE 3                               
LINCOM4  DS    CL(L'LINCOMS)       COMMENT LINE 4                               
LINCOM5  DS    CL(L'LINCOMS)       COMMENT LINE 5                               
LINCOMN  EQU   (*-LINCOMS)/L'LINCOMS                                            
LINMGC   DS    CL(L'BDMGDATE)      MAKEGOOD CODE                                
LINRSN   DS    CL(L'RCELRC)        REASON CODE                                  
LINUPG   DS    CL80                UPGRADE EXPRESSION                           
LINBTYPE DS    CL2                 BOOK TYPE (2 CHAR AT MOST)                   
LINRBOOK DS    XL(L'NDBOOK)        RATING BOOK                                  
LINCKSM  DS    XL4                 RECORD CHECK SUM                             
LINCHGD  DS    XL(L'BDCHG)         LAST CHANGE DATE                             
LINWHY   DS    0X                  ** REASONS FOR LAST CHANGE **                
LINWHY3  DS    XL(L'BDWHY3)                                                     
LINWHY2  DS    XL(L'BDWHY2)                                                     
LINWHY1  DS    XL(L'BDWHY)                                                      
LINWHYL  EQU   *-LINWHY                                                         
LINPCKEY DS    CL(L'PCKEY)         PC KEY                                       
LINSTAT  DS    XL(L'BDSTAT)        STATUS                                       
LINSTAT2 DS    XL(L'BDSTAT2)       STATUS BYTE 2                                
LINSTAT3 DS    XL(L'BDSTAT3)       STATUS BYTE 3                                
LINSTATL EQU   *-LINSTAT                                                        
LINCIND  DS    XL(L'BDCIND)        COST INDICATORS                              
LINVALL  EQU   *-LINVALS                                                        
                                                                                
SCDVALS  DS    0X                  ** STATION/COST/DEMO VALUES **               
SCDSPILL DS    C                   YESQ IF A SPILL BUY                          
SCDSTA   DS    CL(STASTAL)         STATION/NETWORK                              
SCDMKT#  DS    XL(L'BUYKMKTN)      MARKET NUMBER                                
SCDMKTN  DS    XL(L'MKTNAME)       MARKET NAME                                  
SCDMALF  DS    XL(L'MKTALF)        MARKET ALPHA                                 
SCDMKST# DS    XL2                 MARKET/STATION REFERENCE#                    
SCDCOST1 DS    PL6                 COST 1 (PENNIES)                             
SCDCOST2 DS    PL6                 COST 2 (PENNIES)                             
SCDC2FAC DS    XL4                 COST 2 FACTOR                                
SCDDEMN  DS    AL2                 N'DEMOS TO SEND                              
SCDDEMV  DS    0XL(L'SCDDEMO+L'SCDDEM#+L'SCDDEMF)                               
SCDDEMO  DS    XL3                 DEMO CODE                                    
SCDDEM#  DS    XL4                 RAW DEMO VALUE                               
SCDDEMF  DS    X                   DEMO FLAG (Y=OVERRIDE)                       
         ORG   SCDDEMV                                                          
         DS    128XL(L'SCDDEMV)                                                 
SCDNTDMF DS    X                                                                
SCDVALL  EQU   *-SCDVALS                                                        
                                                                                
PBDVALS  DS    0X                  ** BUY POST DEMOS **                         
PBDDEMV  DS    0XL(L'PBDDEMO+L'PBDDEM#)                                         
PBDDEMO  DS    XL3                 DEMO CODE                                    
PBDDEM#  DS    XL4                 DEMO VALUE                                   
         DS    (EDEMLSTN)XL(L'PBDDEMV)                                          
                                                                                
ORBVALS  DS    0X                  ** ORBIT VALUES **                           
ORBVALN  DS    AL2                 N'DEMOS TO SEND                              
                                                                                
ORBVNTRY DS    0X                  ** ORBIT ENTRY **                            
ORBVDAY  DS    X                   DAY BITS (SAME LAYOUT AS ORBELEM)            
ORBVSTM  DS    XL2                 START TIME                                   
ORBVETM  DS    XL2                 END TIME                                     
ORBVPRG  DS    CL7                 PROGRAM DESC                                 
ORBVDMV  DS    XL4                 DEMO VALUE                                   
         DS    XL2                 SPARE                                        
ORBVLNQ  EQU   *-ORBVNTRY          L'ORBIT ENTRY                                
         ORG   ORBVNTRY                                                         
ORBMAXN  EQU   9                                                                
         DS    (ORBMAXN)XL(ORBVLNQ)                                             
ORBVALL  EQU   *-ORBVALS           MAXIMUM NUMBER OF ORBITS                     
                                                                                
PKGVALS  DS    0X                  ** PACKAGE VALUES **                         
PKGNLIN  DS    AL2                 N'LINES TO SEND                              
PKGTYPE  DS    X                   PACKAGE TYPE                                 
PKGLINS  DS    (MXPKGLNS)XL1       CHILD LINES OR SINGLE PARENT LINE            
PKGVALL  EQU   *-PKGVALS                                                        
MXPKGLNS EQU   20                                                               
                                                                                
CLRVALS  DS    0X                  ** CLEARANCE RECORD VALUES **                
CLRSTAX  DS    XL(L'CLSKSTA)       CLEARANCE STATION                            
CLRSTAL  DS    XL(L'CLSKSTA)       LAST CLEARANCE STATION                       
CLRPRD   DS    XL(L'CLSTPRD)       PRODUCT CODE                                 
CLRVALL  EQU   *-CLRVALS                                                        
         ORG   CLRVALS                                                          
                                                                                
GOLVALS  DS    0X                  ** GOAL HEADER VALUES **                     
GOLPRDS  DS    0XL(L'GOLPRD1+L'GOLPRD2)                                         
GOLPRD1  DS    CL(L'GKEYPRD)       PRODUCT1 CODE                                
GOLPRD2  DS    CL(L'GKEYPRD2)      PRODUCT2 CODE                                
GOLSECS  DS    0XL(L'GOLSEC1+L'GOLSEC2)                                         
GOLSEC1  DS    XL(L'GKEYSEC)       PRODUCT1 SECONDS LENGTH                      
GOLSEC2  DS    XL(L'GKEYSEC)       PRODUCT2 SECONDS LENGTH                      
GOLEST   DS    XL(L'GKEYEST)       ESTIMATE NUMBER                              
GOLMKT   DS    XL(L'GKEYMKT)       MARKET NUMBER                                
GOLDPT   DS    CL(L'GKEYDPT)       DAYPART CODE                                 
GOLLKIN  DS    XL(L'GDLKGOAL)      GOAL LOCKIN DATE                             
GOLPURP  DS    XL(L'GDIDR)         GOAL PURPOSE CODE                            
GOLSTDT  DS    XL(L'GLWEEK)        GOAL START DATE FOR THE MARKET               
GOLNDDT  DS    XL(L'GLWEEK)        GOAL END   DATE FOR THE MARKET               
GOLOMKT  DS    XL(L'GKEYMKT)       OUTPUT MARKET NUMBER                         
GOLOSDT  DS    XL(L'GLWEEK)        OUTPUT GOAL MARKET START DATE                
GOLONDT  DS    XL(L'GLWEEK)        OUTPUT GOAL MARKET END DATE                  
GOLVALL  EQU   *-GOLVALS                                                        
                                                                                
SHTVALS  DS    0X                  ** REVSHEET/WRKSHEET VALUES **               
SHTFIND  DS    C                   FILE INDICATOR                               
SHTDA    DS    XL(L'IODA)          DISK ADDRESS                                 
SHTVALL  EQU   *-SHTVALS                                                        
                                                                                
SGLVALS  DS    0X                  ** DEVIATED WEEKS VALUES **                  
SGLFIND  DS    C                   FILE INDICATOR                               
SGLDA    DS    XL(L'IODA)          DISK ADDRESS                                 
SGLCKSM  DS    XL4                 RECORD CHECK SUM                             
SGLERTXT DS    CL(L'WORK)                                                       
SGLERNUM DS    XL2                                                              
SGLVALL  EQU   *-SGLVALS                                                        
                                                                                
SWPVALS  DS    0X                  ** SWEEP TABLE VALUES **                     
SWPFILE  DS    CL1                 C'T'                                         
SWPMED   DS    CL1                 C'T'                                         
SWPSRCE  DS    CL1                 C'N'                                         
SWPYEAR  DS    CL4                 YEAR                                         
SWPMON   DS    XL1                 MONTH                                        
SWPSDTE  DS    CL6                 START DATE                                   
SWPEDTE  DS    CL6                 END DATE                                     
SWPVALL  EQU   *-SWPVALS                                                        
                                                                                
OUTVALL  EQU   *-OUTVALS                                                        
                                                                                
PRVVALS  DS    0X                  ** PREVIOUS VALUES **                        
PMEDCOD  DS    CL(L'AGYMEDCD)                                                   
PCLTCOD  DS    CL(L'QCLTA)                                                      
PPRDNUM  DS    XL(L'GKEYPRD+L'GKEYPRD2)                                         
PSECLEN  DS    XL(L'GKEYSEC*2)                                                  
PDPTCOD  DS    CL(L'GKEYDPT)                                                    
PESTNUM  DS    XL(L'BUYKEST)                                                    
PMKTSTA  DS    0XL(L'PMKTNUM+L'PNETSTA)                                         
PMKTNUM  DS    XL(L'BUYKMKTN)                                                   
PNETSTA  DS    XL(L'BUYKSTAC)                                                   
PMKT##   DS    0XL(L'STEMGMKT)                                                  
PMKT#    DS    CL(L'MKTKMKT)                                                    
PCPPEST  DS    XL(L'GDCPPES)                                                    
PCPPEST2 DS    XL(L'GDCPPES2)                                                   
PRVVALL  EQU   *-PRVVALS                                                        
                                                                                
MKTSTA   DS    0XL(L'MKTNUM+L'STACOD)                                           
MKTNUM   DS    XL(L'BUYKMKTN)                                                   
STACOD   DS    XL(L'BUYKSTAC)                                                   
                                                                                
         ORG   OUTVALS                                                          
PIDVALS  DS    0X                  ** PID DOWNLOAD VALUES **                    
PIDPID   DS    CL(L'SAPEPID)       ACTUAL PID CHARACTERS                        
PIDFNAM  DS    CL(L'SANAME)        FIRST NAME                                   
PIDMNAM  DS    CL(L'SANAME)        MIDDLE NAME                                  
PIDLNAM  DS    CL(L'SANAME)        LAST NAME                                    
PIDOFCD  DS    CL(L'SAPEROFF)      OFFICE CODE                                  
PIDEMAD  DS    CL63                EMAIL ADDRESS                                
PIDPHON  DS    CL12                PHONE NUMBER                                 
PIDEXT   DS    CL(L'SAPEREXT)      telephone ext number                         
PIDVALL  EQU   *-PIDVALS                                                        
         ORG                                                                    
                                                                                
SPTTAB#  DS    H                   N'ENTRIES IN SPTTAB                          
GWKTAB#  DS    H                   N'ENTRIES IN GWKTAB                          
                                                                                
ERRVALS  DSECT                     ** DARE ERRORS **                            
ERRLEN   DS    X                   LENGTH OF TABLE ENTRY                        
ERRCODE  DS    CL3                 ERROR CODE                                   
ERRTEXT  DS    0C                  ERROR TEXT                                   
                                                                                
SLNTABD  DSECT                     ** SPOT LENGTH TABLE **                      
STABLEN  DS    X                   SPOT LENGTH                                  
STABEQV  DS    X                   SPOT LENGTH EQUIVALENT                       
SLNTABQ  EQU   *-SLNTABD                                                        
                                                                                
DPTTABD  DSECT                     ** DAYPART TABLE **                          
DPTTALPH DS    C                   DAYPART ALPHA CODE                           
DPTTNAME DS    CL3                 DAYPART NAME                                 
DPTTMAST DS    CL3                 MASTER DAYPART NAME                          
DPTTMSCD DS    X                   MASTER/SLAVE CODE                            
DPTTABL  EQU   *-DPTTABD                                                        
DPTTMAXN EQU   36                  MAXIMUM N'TABLE ENTRIES                      
                                                                                
FLMRECD  DSECT                     ** FILM BUFFER RECORD **                     
FLMRKEY  DS    0X                                                               
FLMRSEQN DS    XL(L'FLMNUM)        FILM SEQUENCE NUMBER                         
FLMRKEYL EQU   *-FLMRKEY                                                        
FLMRCODE DS    CL(L'CMLADID)       FILM CODE                                    
FLMRRECL EQU   *-FLMRECD                                                        
                                                                                
SPTTABD  DSECT                     ** SPOT VALUES **                            
SPTMAPN  DS    XL(L'LD_CODE)       MAP NUMBER                                   
SPTREF#  DS    XL(L'SPOTREFN)      SPOT REFERENCE NUMBER                        
SPTREP#  DS    X                   SPOT REPLICATION FACTOR                      
                                                                                
SPTCOMP  DS    0X                  ** DATA FOR DUPLICATION COMPARE **           
SPTDATE  DS    CL6                 SPOT DATE                                    
SPTPRDS  DS    0XL2                ** PRODUCT ALLOCATIONS **                    
SPTPRD1  DS    XL(L'RPPRD)         ALLOCATED PRIMARY PRODUCT                    
SPTPRD2  DS    XL(L'RPPRD)         ALLOCATED PIGGYBACK PRODUCT                  
SPT1LEN  DS    XL(L'RPTIME)        LENGTH FOR PRIMARY PRODUCT                   
SPT2LEN  DS    XL(L'RPTIME)        LENGTH FOR PIGGYBACK PRODUCT                 
SPTAFDT  DS    XL(L'ADATE)         AFFIDAVIT DATE                               
SPTAFTM  DS    XL(L'ATIME)         AFFIDAVIT TIME                               
SPTCLRDT DS    XL(L'RPAY)          CLEARANCE DATE                               
SPTCLRSQ DS    XL(L'RPAYSEQ)       CLEARANCE SEQUENCE NUMBER                    
SPTFILMV DS    0X                  ** FILM DATA **                              
SPTFILM1 DS    XL(L'FLMNUM)        FILM CODE 1                                  
SPTFILM2 DS    XL(L'FLMNUM)        FILM CODE 2                                  
SPTFLMTY DS    XL(L'FLMCODE)       FILM ELEMENT CODE (X'12' OR X'18')           
SPTFLMDY DS    XL(L'FLMDAY)        FILM DAY BITS (FROM X'12')                   
SPTFLMPT DS    XL(L'TRACREF)       FILM PATTERN REF (FROM X'18')                
SPTFILML EQU   *-SPTFILMV                                                       
SPTMGC   DS    CL2                 MAKEGOOD CODE                                
                                                                                
SPTSTAT  DS    0XL4                ** SPOT STATUS **                            
SPTSTAT1 DS    X                   ** STATUS BYTE 1 **                          
SPTSTAT2 DS    X                   ** STATUS BYTE 2 **                          
SPT#MINS EQU   SPTSTAT2            MINUS SPOT                                   
SPTSMINS EQU   X'01'                                                            
                                                                                
SPTSTAT3 DS    X                   ** STATUS BYTE 3 **                          
SPT#HIAT EQU   SPTSTAT3            HIATUS                                       
SPTSHIAT EQU   X'80'                                                            
SPT#MSPD EQU   SPTSTAT3            MINUS SPOT PAID                              
SPTSMSPD EQU   X'40'                                                            
SPT#MGPD EQU   SPTSTAT3            MAKEGOOD PENDING                             
SPTSMGPD EQU   X'20'                                                            
SPT#MKGD EQU   SPTSTAT3            MAKEGOOD                                     
SPTSMKGD EQU   X'10'                                                            
SPT#MISD EQU   SPTSTAT3            MISSED                                       
SPTSMISD EQU   X'08'                                                            
SPT#POTO EQU   SPTSTAT3            +OTO                                         
SPTSPOTO EQU   X'04'                                                            
SPT#MOTO EQU   SPTSTAT3            -OTO                                         
SPTSMOTO EQU   X'02'                                                            
SPT#PAID EQU   SPTSTAT3            PAID                                         
SPTSPAID EQU   X'01'                                                            
                                                                                
SPTSTAT4 DS    X                   ** STATUS BYTE 4 **                          
SPT#B1PA EQU   SPTSTAT4            PRODUCT 1 PAYS ALL (PIGGYBACKS)              
SPTSB1PA EQU   X'80'                                                            
SPT#MKGC EQU   SPTSTAT4            MAKEGOOD CODE PRESENT                        
SPTSMKGC EQU   X'40'                                                            
SPT#FILM EQU   SPTSTAT4            FILM DATA PRESENT                            
SPTSFILM EQU   X'20'                                                            
SPT#AFID EQU   SPTSTAT4            AFFIDAVIT DATA AVAILABLE                     
SPTSAFID EQU   X'10'                                                            
SPT#ALOC EQU   SPTSTAT4            ALLOCATED                                    
SPTSALOC EQU   X'08'                                                            
SPT#PRAL EQU   SPTSTAT4            PRE-ALLOCATED                                
SPTSPRAL EQU   X'04'                                                            
SPT#COVR EQU   SPTSTAT4            COST OVERRIDE                                
SPTSCOVR EQU   X'02'                                                            
SPT#ISEQ EQU   SPTSTAT4            SPOT IN NEXT WEEK                            
SPTSISEQ EQU   X'01'                                                            
                                                                                
SPTCOST  DS    PL6                 COST OVERRIDE                                
SPTCOMPL EQU   *-SPTCOMP                                                        
                                                                                
SPTTABL  EQU   *-SPTTABD                                                        
                                                                                
GWKTABD  DSECT                     ** GOAL WEEKLY VALUES **                     
GWKTYPE  DS    X                   GOAL DATA TYPE (0=WEEKLY, 7=LOCKIN)          
GWKREP#  DS    X                   GOAL REPLICATION FACTOR                      
GWKSTR   DS    CL6                 GOAL WEEK START DATE                         
GWKGRP   DS    PL6                 GOAL WEEK POINTS                             
GWKDOL   DS    PL6                 GOAL WEEK DOLLARS                            
GWKINDS  DS    X                   ** INDICATORS **                             
GWKISEQ  EQU   X'80'               GOAL WEEK START DATE IN SEQUENCE             
GWKIGRP  EQU   X'40'               GOAL GRPS SAME AS PREVIOUS                   
GWKIDOL  EQU   X'20'               GOAL DOLLARS SAME AS PREVIOUS                
GWKI2DEC EQU   X'10'               GOAL POINTS ARE 2 DECIMAL PRECISION          
GWKTABL  EQU   *-GWKTABD                                                        
                                                                                
CPPTABD  DSECT                     ** DSECT FOR GOAL CPP GUIDE **               
CPPTEOTQ EQU   FF                  END OF TABLE INDICATOR                       
CPPTDPT  DS    CL(L'GKEYDPT)       DAYPART CODE                                 
CPPTMAXM EQU   12                  MAXIMUM NUMBER OF MONTHS                     
CPPTVALS DS    (CPPTMAXM)XL(L'GLBUDGET)                                         
CPPTABL  EQU   *-CPPTDPT           WIDTH OF TABLE ENTRY                         
CPPTMAXD EQU   15                  MAXIMUM NUMBER OF DAYPARTS                   
                                                                                
       ++INCLUDE DDDARETABD                                                     
         EJECT                                                                  
       ++INCLUDE SPLNKWRK                                                       
                                                                                
CLTRECD  DSECT                                                                  
         ORG   CKEYCLT+L'CKEYCLT                                                
CKEYREST DS    XL(L'CKEY-(*-CKEY))                                              
                                                                                
PRDRECD  DSECT                                                                  
         ORG   PKEYPRD+L'PKEYPRD                                                
PKEYREST DS    XL(L'PKEY-(*-PKEY))                                              
                                                                                
ESTRECD  DSECT                                                                  
         ORG   EKEYEST+L'EKEYEST                                                
EKEYREST DS    XL(L'EKEY-(*-EKEY))                                              
                                                                                
WORKD    DSECT                     ** REDEFINE OVERWORK (1K) **                 
         ORG   OVERWORK                                                         
         DS    0D                                                               
PACKOF8B DS    PL8                                                              
PACK16B1 DS    PL16                                                             
PACK16B2 DS    PL16                                                             
                                                                                
SAVERE   DS    A                   SAVE RE                                      
SAVERF   DS    A                   SAVE RF                                      
SAVER0   DS    A                   SAVE R0                                      
SAVER1   DS    A                   SAVE R1                                      
SAVER2   DS    A                   SAVE R2                                      
SAVER3   DS    A                   SAVE R3                                      
SAVER4   DS    A                   SAVE R4                                      
                                                                                
SVIOVALS DS    XL(IOVALL)          SAVED I/O VALUES                             
                                                                                
C2PMKT#  DS    CL(L'PMKT#)         SAVED C2 MARKET                              
                                                                                
LBUYVALS DS    0X                  ** LAST BUY VALUES **                        
LBUYGBKY DS    XL(GBSTA+L'GBSTA-GBAGYMD)                                        
LBUYBUYR DS    CL(L'LINBYR)        LAST BUYER CODE                              
LBUYVALL EQU   *-LBUYVALS                                                       
                                                                                
         DS    XL(ONEK-(*-OVERWORK)) PROTECTION SO WE DON'T GO OVER 1K          
         ORG                                                                    
                                                                                
* OTHER INCLUDED DSECTS                                                         
         PRINT OFF                                                              
       ++INCLUDE CTGENRAD          MSTREET RECORDS                              
       ++INCLUDE CTGENFILE                                                      
CTFXREC  DSECT                                                                  
         ORG   CTFXKTYP+L'CTFXKTYP                                              
CTFXSPR1 DS    XL(CTFXAGY-*)                                                    
         ORG   CTFXAGY+L'CTFXAGY                                                
CTFXSPR2 DS    XL(CTFXCODE-*)                                                   
         ORG   CTFXSUBC+L'CTFXSUBC                                              
CTFXSPR3 DS    XL(CTFXLEN-*)                                                    
         ORG                                                                    
*PREFIX=D                                                                       
       ++INCLUDE CTGENSTAD                                                      
STAKEYD  DSECT                                                                  
         ORG   STAKTYP+L'STAKTYP                                                
STAKSPR1 DS    XL(DSTAKMEDA-*)                                                  
         ORG   STAKEFDA+L'STAKEFDA                                              
STAKSPR2 DS    XL(DSTAKSTAT-*)                                                  
*PREFIX=                                                                        
       ++INCLUDE CTGENAGRD                                                      
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE GEGENSDR                                                       
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE SPEDUPD                                                        
       ++INCLUDE SPMGADN                                                        
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENCLRST                                                     
         ORG   CLSTEL01+L'CLSTEL01                                              
CLS01LEN DS    X                                                                
CLS01ELQ EQU   X'01'                                                            
         ORG                                                                    
       ++INCLUDE SPGENDAYPT                                                     
       ++INCLUDE SPGENDRBTC                                                     
       ++INCLUDE SPGENDRFLT                                                     
DFLRECD  DSECT                                                                  
         ORG   DFLKEST+L'DFLKEST                                                
DFLKREST DS    XL(L'DFLKEY-(*-DFLKEY))                                          
       ++INCLUDE SPGENDRORD                                                     
DAREORDD DSECT                                                                  
         ORG   DOKEY                                                            
DODKEY   DS    0XL32                                                            
       ++INCLUDE SPGENDRMKN                                                     
       ++INCLUDE SPGENDRMKO                                                     
       ++INCLUDE SPGENDREV                                                      
       ++INCLUDE SPGENDESTN                                                     
DESTNRCD DSECT                                                                  
         ORG   DSRKCLT+L'DSRKCLT                                                
DSRKREST DS    XL(L'DSRKEY-(DSRKCLT+L'DSRKCLT-DSRKEY))                          
       ++INCLUDE SPGENESTD                                                      
       ++INCLUDE SPGENEQU                                                       
       ++INCLUDE SPGENXLK                                                       
       ++INCLUDE SPGETBUBLD                                                     
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPTRCMML                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENSTEQ                                                      
       ++INCLUDE SPGENSDEF                                                      
       ++INCLUDE GEGENSPSAL                                                     
         ORG   GSPKPCOD+L'GSPKPCOD                                              
GSPLKSPR DS    XL(L'GSPLKEY-(*-GSPLKEY))                                        
       ++INCLUDE SPADAVCOM                                                      
         ORG   COMKCOM+L'COMKCOM                                                
COMKSPR  DS    XL(L'COMKEY-(*-COMKEY))                                          
       ++INCLUDE SPADBUYER                                                      
         ORG   BYRKBYR+L'BYRKBYR                                                
BYRKSPR  DS    XL(L'BYRKEY-(*-BYRKEY))                                          
*PREFIX=O                                                                       
COMD     DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
         ORG   COMKSTA                                                          
COMKSPR  DS    XL1                 SPARE FOR MARKET NUMBER                      
         ORG                                                                    
COMELQ   EQU   X'05'                                                            
         ORG   COMELEN+L'COMELEN                                                
COMMENT  DS    0C                                                               
*PREFIX=                                                                        
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPGENEMAIL                                                     
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPGENPURP                                                      
       ++INCLUDE GEGENSTSRC                                                     
       ++INCLUDE GEGENTOK                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE SPGENDDEV                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE GEGENEDI                                                       
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAXPEQUS                                                       
CTEPRECD DSECT                                                                  
         ORG   CTEPKSTY+L'CTEPKSTY                                              
CTEPKSPR DS    XL27                                                             
         ORG                                                                    
       ++INCLUDE DDGETDARED                                                     
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'116SPLNK12   02/23/21'                                      
         END                                                                    
